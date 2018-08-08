open Core

open Types_j
open Types_t

open CommonTypes


let read_json_and_marshal in_fname out_fname =
  let dump_str = In_channel.read_all in_fname in
  let dump = Types_j.dump_of_string dump_str in

  Out_channel.with_file out_fname ~f:(fun oc ->
    Marshal.to_channel oc dump [];
  );

  dump

let read_marshal in_fname =
  In_channel.with_file in_fname ~f:(fun ic ->
    (Marshal.from_channel ic : Types_j.dump)
  )

let fail j =
  let repr = Yojson.Safe.to_string j in
  pf "    NUOVO! |||%s|||\n" repr;
  failwith "FINE PENA MAI"

let localzone = Time.Zone.local |> Lazy.force

let different_day reference current =
  let open Date in
  let reference_day = of_time reference ~zone:localzone in
  let current_day = of_time current ~zone:localzone in
  if equal reference_day current_day
  then false
  else true

let split_messages_by_day ?(max_per_chunk=500) (messages : Message.t list) =
  match List.hd messages with
  | Some m -> begin
    let first_ts = m.date in

    let _, _, chunks =
      List.fold messages ~init:(0, first_ts, [[]]) ~f:(fun (count, first_ts, chunks) message ->
        let last_chunk, other_chunks = List.split_n chunks 1 in
        let last_chunk = List.hd_exn last_chunk in

        if count >= max_per_chunk && (different_day first_ts message.date) then begin
          (* Close current chunk and create a new one *)
          0, message.date, [message]::chunks
        end else begin (* count < max_per_chunk || not (different_day first_ts message.date) *)
          (count + 1), first_ts, (message::last_chunk)::other_chunks
        end
      ) in

    let chunks_num = List.length chunks in

    List.foldi chunks ~init:[] ~f:(fun number chunks' messages ->
      let messages = List.rev messages in
      let messages_number  = List.length messages in
      let day_begin, day_end =
        match (List.hd messages), (List.last messages) with
        | Some fst, Some last -> begin
          let day_fst = Date.of_time ~zone:localzone fst.date in
          let day_last = Date.of_time ~zone:localzone last.date in
          if Date.equal day_fst day_last
          then day_fst, day_fst
          else day_fst, day_last
        end
        | Some m, None
        | None, Some m -> let d = (m.date |> Date.of_time ~zone:localzone) in d, d
        | None, None -> assert false
      in
      let number = chunks_num - number in
      { day_begin; day_end; number; messages_number; messages; }::chunks'
    )
  end
  | None -> assert false

let print_chunk (chunk : message_chunk) =
  pf "        chunk of %d messages from %s to %s\n%!"
    chunk.number (Date.to_string chunk.day_begin) (Date.to_string chunk.day_end)

let print_chat chat =
  pf "===============================================================================\n";
  pf "%03d Chat name = \"%s\"\n%!" chat.number chat.name;
  pf "    %d messages\n%!" (List.length chat.messages);
  pf "===============================================================================\n";
  pf "    WE HAVE %d CHUNKS OF MESSAGES\n%!" (List.length chat.messages);
  List.iter chat.messages ~f:print_chunk;
  pf "===============================================================================\n\n\n"

let sanitize_string s =
  (* Segment graphemes *)
  let segments = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc s -> s::acc) [] s in

  let rec loop ?(acc=[]) d =
    match Uutf.decode d with
    | `Uchar u -> loop ~acc:(u::acc) d
    | `End -> begin
      let result = List.rev acc in
      let result =
        if List.length result > 3
        then List.split_n result 3 |> fst
        else result in
      let b = Buffer.create 256 in
      List.iter result ~f:(Uutf.Buffer.add_utf_8 b);
      Buffer.contents b
    end
    | `Malformed _ -> loop ~acc:(Uutf.u_rep::acc) d
    | `Await -> assert false in

  let segments' =
    List.fold segments ~init:[] ~f:(fun segments' s ->
      (loop (Uutf.decoder ~encoding:`UTF_8 (`String s)))::segments'
    ) in
  String.concat ~sep:"" segments'

let sanitize_path p =
  let p' = sanitize_string p in

  (* Replace the regular "/" with "∕", Unicode Character 'DIVISION SLASH' (U+2215) *)
  let pattern = String.Search_pattern.create "/" in
  String.Search_pattern.replace_all pattern ~in_:p' ~with_:"∕"

module Filters = struct
  open Jg_types

  let date ?(defaults=[("format", Tstr "%Y-%m-%d")]) date_str kwargs =
    let ts = Jg_runtime.string_of_tvalue date_str |> Time.of_string in
    let fmt = Jg_runtime.jg_get_kvalue "format" kwargs ~defaults |> Jg_runtime.string_of_tvalue in
    Tstr (Time.format ts fmt ~zone:localzone)
end

let optional v ~f =
  Option.value_map v ~default:Jg_types.Tnull ~f

let optional_string v ~f =
  optional v ~f:(fun v -> Tstr (f v |> sanitize_string))

let optional_int v ~f =
  optional v ~f:(fun v -> Tint (f v))

let prepare_message_date (m : Message.t) =
  let open Jg_types in
  let data = [
    "id", Tint m.id;
    "type_", Tstr (Message_type.to_string m.type_);
    "date", Tstr (m.date |> Time.to_string);
    "edited", Tstr (m.edited |> Time.to_string);
    "text", Tstr (MessageText.to_string m.text |> sanitize_string);
    "file", Tstr (Option.value m.file ~default:"");
    "action",optional m.action ~f:(fun a -> Tstr (Action_type.show a));
    "contact_information", (optional m.contact_information ~f:(fun ci ->
        Tobj [
          ("first_name", Tstr ci.first_name);
          ("last_name", Tstr ci.last_name);
          ("phone_number", Tstr ci.phone_number);
        ]
      )
    );
    "discard_reason", optional_string m.discard_reason ~f:Discard_reason_type.show;
    "location_information", (optional m.location_information ~f:(fun li ->
        Tobj [
          ("latitude", Tfloat li.latitude);
          ("longitude", Tfloat li.longitude);
        ]
      )
    );
    "media_type", optional_string m.media_type ~f:Media_type.show;
    "members", optional m.members ~f:(fun members -> Tlist (List.map members ~f:(fun m -> Tstr (sanitize_string m))))
  ] in
  let data = List.fold [
    ("from_", m.from);
    ("forwarded_from", m.forwarded_from);
    ("actor", m.actor);
    ("address", m.address);
    ("author", m.author);
    ("contact_vcard", m.contact_vcard);
    ("game_description", m.game_description);
    ("game_link", m.game_link);
    ("game_title", m.game_title);
    ("inviter", m.inviter);
    ("performer", m.performer);
    ("title", m.title);
    ("place_name", m.place_name);
    ("reason_domain", m.reason_domain);
    ("saved_from", m.saved_from);
    ("sticker_emoji", m.sticker_emoji);
    ("via_bot", m.via_bot);
    ("mime_type", m.mime_type);
    ("photo", m.photo);
  ] ~init:data ~f:(fun acc (name, value) -> (name, (optional_string value ~f:ident))::acc) in
  let data = List.fold [
    ("duration_seconds", m.duration_seconds);
    ("game_message_id", m.game_message_id);
    ("height", m.height);
    ("width", m.width);
    ("live_location_period_seconds", m.live_location_period_seconds);
    ("message_id", m.message_id);
    ("reply_to_message_id", m.reply_to_message_id);
    ("score", m.score);
  ] ~init:data ~f:(fun acc (name, value) -> (name, (optional_int value ~f:ident))::acc) in
  data

let prepare_chunk_data total_chunks chat_name (chunk : message_chunk) =
  let open Jg_types in
  let msgs_model = Tlist (List.map chunk.messages ~f:(fun m -> Tobj (prepare_message_date m))) in
  let day_begin = Time.of_date_ofday ~zone:localzone chunk.day_begin (Time.Ofday.of_string "00:00") in
  let day_end = Time.of_date_ofday ~zone:localzone chunk.day_end (Time.Ofday.of_string "23:59:59") in
  let models = [
    "day_begin", Tstr (Time.to_string day_begin);
    "day_end", Tstr (Time.to_string day_end);
    "number", Tint chunk.number;
    "total_chunks", Tint total_chunks;
    "messages_number", Tint chunk.messages_number;
    "msgs", msgs_model;
    "chat_name", Tstr chat_name;
  ] in
  models

let save_chat ?(output_dir="OUTPUT") chat =
  let (/) a b = a^"/"^b in
  let dir = output_dir/(spf "%03d_%s" chat.number chat.name) in
  Core_extended.Shell.mkdir ~p:() dir;

  let index_fname = dir/"index.html" in
  Out_channel.with_file index_fname ~f:(fun oc ->
    Out_channel.output_string oc (spf "TITOLO: \"%s\"" chat.name);
  );

  (* Save pages *)
  let total_chunks = List.length chat.messages in
  List.iter chat.messages ~f:(fun chunk ->
    let day_begin = chunk.day_begin |> Date.to_string in
    let day_end = chunk.day_end |> Date.to_string in
    let chunk_fname = dir/(spf "messages_%s--%s.html" day_begin day_end) in
    Out_channel.with_file chunk_fname ~f:(fun oc ->
      let models = prepare_chunk_data total_chunks chat.name chunk in
      let env = { Jg_types.std_env with
        autoescape = false;
        filters = [
          ("date", Jg_runtime.func_arg1 Filters.date);
        ];
        template_dirs = ["src/static/templates"]
      } in
      let data = Jg_template.from_file ~env ~models "messages_page.html" in
      Out_channel.output_string oc data
    )
  )

let save_chat_to_disk chat_number (chat : Types_t.chat) =
  if chat_number <> 10000 then begin
    match chat.name with
    | Some name -> begin
      let name = sanitize_path name in

      let messages = List.map chat.messages ~f:Message.of_raw_message in
      let messages_chunks = split_messages_by_day messages in
      let chat : CommonTypes.chat = {
        name;
        number = chat_number;
        messages = messages_chunks;
      } in

      print_chat chat;
      save_chat chat
    end
    | None -> begin
      pf "Skipping chat numeber %d: no name available\n\n\n%!" chat_number
    end
  end

let () =
(*   let dump = read_json_and_marshal "result.json" "result.bin" in *)
  let dump = read_marshal "result.bin" in
  ignore dump;

  List.iteri dump.chats ~f:(fun idx chat ->
    save_chat_to_disk (idx + 1) chat
  )
