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

let split_messages_by_day ?(max_per_chunk=100) (messages : Message.t list) =
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

    List.fold chunks ~init:[] ~f:(fun chunks' messages ->
      let messages = List.rev messages in
      let number = List.length messages in
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
      { day_begin; day_end; number; messages; }::chunks'
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

let sanitize_path p =
  (* Segment graphemes *)
  let segments = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc s -> s::acc) [] p in

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

  let p' = String.concat ~sep:"" segments' in

  (* Replace the regular "/" with "∕", Unicode Character 'DIVISION SLASH' (U+2215) *)
  let pattern = String.Search_pattern.create "/" in
  String.Search_pattern.replace_all pattern ~in_:p' ~with_:"∕"

let prepare_chunk_data (chunk : message_chunk) =
  let open Jg_types in
  let msgs_model = Tlist (List.map chunk.messages ~f:(fun m ->
    Tobj [
      "date", Tstr (m.date |> Time.to_string);
      "edited", Tstr (m.edited |> Time.to_string);
      "from_", Tstr (Option.value m.from ~default:"");
      "text", Tstr (MessageText.to_string m.text);
    ]
  )) in
  let models = [
    "day_begin", Tstr (chunk.day_begin |> Date.to_string);
    "day_end", Tstr (chunk.day_end |> Date.to_string);
    "number", Tint chunk.number;
    "msgs", msgs_model;
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
  List.iter chat.messages ~f:(fun chunk ->
    let day_begin = chunk.day_begin |> Date.to_string in
    let day_end = chunk.day_end |> Date.to_string in
    let chunk_fname = dir/(spf "messages_%s--%s.html" day_begin day_end) in
    Out_channel.with_file chunk_fname ~f:(fun oc ->
      let models = prepare_chunk_data chunk in
      let env = { Jg_types.std_env with
        autoescape = false;
        template_dirs = ["src/static/templates"]
      } in
      let data = Jg_template.from_file ~env ~models "messages_page.html" in
      Out_channel.output_string oc data
    )
  )

let save_chat_to_disk chat_number (chat : Types_t.chat) =
  if chat_number = 559 then begin
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

(*
  let acc =
    List.foldi dump.chats ~init:String.Map.empty ~f:(fun i1 acc chat ->
      pf "===============================================================================\n";
      pf "%03d Chat name = \"%s\"\n%!" (i1 + 1) (Option.value ~default:"N/A" chat.name);
      pf "===============================================================================\n";

      let acc =
        List.foldi chat.messages ~init:acc ~f:(fun i2 acc msg ->
        match msg with
        | `Assoc l -> begin
          let keys = List.map l ~f:fst |> List.sort ~compare:String.compare in
          List.iter l ~f:(fun (k, v) ->
            if k = "media_type" then pf "GUARDA QUI -> %s\n%!" (Yojson.Safe.to_string v)
          );
          let md5 = String.concat keys |> Md5.digest_string |> Md5.to_hex in
          if String.Map.existsi acc ~f:(fun ~key ~data -> key = md5)
          then acc
          else String.Map.add_exn acc ~key:md5 ~data:keys
        end
        | _ -> fail msg
      ) in

      pf "===============================================================================\n\n\n";
      acc
    ) in
*)

(*
  let remove_commons xs =
    let xs_set = String.Set.of_list xs in
    let common_set = String.Set.of_list [
      "id";
      "type";
      "date";
      "edited";
      "text"
    ] in
    let dif_set = String.Set.diff xs_set common_set in
    String.Set.to_list dif_set |> List.sort ~compare:String.compare in

  String.Map.iteri acc ~f:(fun ~key ~data ->
    pf "%s -> %s\n%!" key (String.concat ~sep:" " (remove_commons data))
  )
*)
