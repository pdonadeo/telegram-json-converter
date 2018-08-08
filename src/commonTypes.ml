open Core

open Types_j
open Types_t


let pf = Printf.printf
let epf = Printf.eprintf
let spf = Printf.sprintf

module MessageText = struct
  type fragment_type =
  | Email
  | Hashtag
  | Mention
  | Link
  | Bot_command
  | Phone
  | Bold
  | Cashtag
  | Italic
  | Code
  [@@deriving yojson, compare, show]

  type raw_fragment = {
    type_ : string [@key "type"];
    text : string;
  } [@@deriving yojson, compare, show]

  type fragment = {
    type_ : fragment_type;
    text : string;
  } [@@deriving yojson, compare, show]

  type mention_name = {
    type_ : string [@key "type"];
    text : string;
    user_id : int;
  } [@@deriving yojson, compare, show]

  type pre = {
    type_ : string [@key "type"];
    text : string;
    language : string;
  } [@@deriving yojson, compare, show]

  type link = {
    type_ : string [@key "type"];
    text : string;
    href : string;
  } [@@deriving yojson, compare, show]

  type t =
    | Fragment_list of t list
    | String_msg of string
    | Fragment of fragment
    | Mention_name of mention_name
    | Pre of pre
    | Link of link
    [@@deriving yojson, compare, show]

  let fail j =
    let repr = Yojson.Safe.to_string j in
    pf "    NUOVO! |||%s|||\n" repr;
    failwith "FINE PENA MAI"

  let raw_frag_to_frag (f : raw_fragment) =
    match f.type_ with
    | "email" -> { type_ = Email; text = f.text}
    | "hashtag" -> { type_ = Hashtag; text = f.text}
    | "mention" -> { type_ = Mention; text = f.text}
    | "link" -> { type_ = Link; text = f.text}
    | "bot_command" -> { type_ = Bot_command; text = f.text}
    | "phone" -> { type_ = Phone; text = f.text}
    | "bold" -> { type_ = Bold; text = f.text}
    | "cashtag" -> { type_ = Cashtag; text = f.text}
    | "italic" -> { type_ = Italic; text = f.text}
    | "code" -> { type_ = Code; text = f.text}
    | _ -> begin
      pf "TIPO FRAMMENTO SCONOSCIUTO = %s\n%!" f.type_;
      fail (raw_fragment_to_yojson f)
    end

  let rec of_raw_txt j =
    match j with
    | `String s -> String_msg s
    | `List l -> begin
      let l' = List.map l ~f:of_raw_txt in
      Fragment_list l'
    end
    | `Assoc o -> begin
      let raw_fragment_r = raw_fragment_of_yojson j in
      match raw_fragment_r with
      | Ok f -> Fragment (raw_frag_to_frag f)
      | Error _ -> begin
        let pre_r = pre_of_yojson j in
        match pre_r with
        | Ok p -> begin
          assert (p.type_ = "pre");
          Pre p
        end
        | Error _ -> begin
          let link_r = link_of_yojson j in
          match link_r with
          | Ok l -> begin
            assert (l.type_ = "text_link");
            Link l
          end
          | Error _ -> begin
            let mention_name_r = mention_name_of_yojson j in
            match mention_name_r with
            | Ok m -> begin
              assert (m.type_ = "mention_name");
              Mention_name m
            end
            | Error _ -> begin
              fail j
            end
          end
        end
      end
    end
    | _ -> fail j

  let frag_to_string (f : fragment) =
    match f.type_ with
    | Email -> spf "<a href=\"mailto:%s\">%s</a>" f.text f.text
    | Hashtag -> spf "<span class=\"hashtag\">%s</span>" f.text
    | Mention -> spf "<span class=\"mention\">%s</span>" f.text
    | Link -> spf "<a href=\"%s\">%s</a>" f.text f.text
    | Bot_command -> spf "<span class=\"bot-command\">%s</span>" f.text
    | Phone -> spf "<a href=\"tel:%s\">%s</a>" f.text f.text
    | Bold -> spf "<b>%s</b>" f.text
    | Cashtag -> spf "<span class=\"cashtag\">%s</span>" f.text
    | Italic -> spf "<i>%s</i>" f.text
    | Code -> spf "<code>%s</code>" f.text

  let rec to_string t =
    match t with
    | Fragment_list xs -> begin
      let s = List.map xs ~f:to_string |> String.concat ~sep:" " in
      String.Search_pattern.replace_all (String.Search_pattern.create "\n") ~in_:s ~with_:"<br/>"
    end
    | String_msg s -> spf "<span>%s</span>" s
    | Fragment f -> frag_to_string f
    | Mention_name m -> spf "<span>%s</span>" m.text
    | Pre p -> spf "<pre>%s</pre>" p.text
    | Link l -> spf "<a href=\"%s\">%s</a>" l.href l.text
end

module Message_type = struct
  type t = [
    | `Message
    | `Service
  ] [@@deriving yojson, compare, show]

  let to_string t =
    match t with
    | `Message -> "message"
    | `Service -> "service"
end

type filename = string [@@deriving yojson, compare, show]
type url = string [@@deriving yojson, compare, show]
type mime_type = string [@@deriving yojson, compare, show]

module Action_type = struct
  type t = [
      `Allow_sending_messages | `Clear_history | `Create_channel
    | `Create_group | `Delete_group_photo | `Edit_group_photo
    | `Edit_group_title | `Invite_members | `Join_group_by_link
    | `Migrate_from_group | `Migrate_to_supergroup | `Phone_call | `Pin_message
    | `Remove_members | `Score_in_game
  ] [@@deriving yojson, compare, show]
end

module Contact_information = struct
  type t = {
    first_name: string;
    last_name: string;
    phone_number: string
  } [@@deriving yojson, compare, show]

  let of_types_t (ci : Types_t.contact_information) : t =
  {
    first_name = ci.first_name;
    last_name = ci.last_name;
    phone_number = ci.phone_number
  }
end

module Discard_reason_type = struct
  type t = [
    | `Busy
    | `Disconnect
    | `Hangup
    | `Missed
  ] [@@deriving yojson, compare, show]
end

module Media_type = struct
  type t = [
    | `Animation | `Audio_file | `Sticker | `Video_file | `Video_message
    | `Voice_message
  ] [@@deriving yojson, compare, show]
end

module Location_information = struct
  type t = {
    latitude: float;
    longitude: float
  } [@@deriving yojson, compare, show]

  let of_types_t (li : Types_t.location_information) : t =
  {
    latitude = li.latitude;
    longitude = li.longitude;
  }
end

module Message = struct
  type t = {
    id: int;
    type_ : Message_type.t;
    date: Time.t;
    edited: Time.t;
    from: string option;
    text: MessageText.t;
    forwarded_from: string option;
    file: filename option;
    action: Action_type.t option;
    actor: string option;
    address: string option;
    author: string option;
    contact_information: Contact_information.t option;
    contact_vcard: filename option;
    discard_reason: Discard_reason_type.t option;
    duration_seconds: int option;
    game_description: string option;
    game_link: url option;
    game_message_id: int option;
    game_title: string option;
    height: int option;
    width: int option;
    inviter: string option;
    live_location_period_seconds: int option;
    location_information: Location_information.t option;
    media_type: Media_type.t option;
    members: string list option;
    message_id: int option;
    mime_type: mime_type option;
    performer: string option;
    title: string option;
    photo: filename option;
    place_name: string option;
    reason_domain: string option;
    reply_to_message_id: int option;
    saved_from: string option;
    score: int option;
    sticker_emoji: string option;
    via_bot: string option
  } [@@deriving compare, show]

  let of_raw_message (r_msg : Types_t.message) : t =
    {
      id = r_msg.id;
      type_ = r_msg.type_;
      date = Time.of_string r_msg.date;
      edited = Time.of_string r_msg.edited;
      from = r_msg.from;
      text = MessageText.of_raw_txt r_msg.text;
      forwarded_from = r_msg.forwarded_from;
      file = r_msg.file;
      action = r_msg.action;
      actor = r_msg.actor;
      address = r_msg.address;
      author = r_msg.author;
      contact_information = Option.map r_msg.contact_information ~f:Contact_information.of_types_t;
      contact_vcard = r_msg.contact_vcard;
      discard_reason = r_msg.discard_reason;
      duration_seconds = r_msg.duration_seconds;
      game_description = r_msg.game_description;
      game_link = r_msg.game_link;
      game_message_id = r_msg.game_message_id;
      game_title = r_msg.game_title;
      height = r_msg.height;
      width = r_msg.width;
      inviter = r_msg.inviter;
      live_location_period_seconds = r_msg.live_location_period_seconds;
      location_information = Option.map r_msg.location_information ~f:Location_information.of_types_t;
      media_type = r_msg.media_type;
      members = Option.map r_msg.members ~f:(fun xs -> List.filter_map xs ~f:ident);
      message_id = r_msg.message_id;
      mime_type = r_msg.mime_type;
      performer = r_msg.performer;
      title = r_msg.title;
      photo = r_msg.photo;
      place_name = r_msg.place_name;
      reason_domain = r_msg.reason_domain;
      reply_to_message_id = r_msg.reply_to_message_id;
      saved_from = r_msg.saved_from;
      score = r_msg.score;
      sticker_emoji = r_msg.sticker_emoji;
      via_bot = r_msg.via_bot;
    }

  let time_to_string t =
    Time.to_string_trimmed ~zone:(Time.Zone.of_string "Europe/Rome") t
end

type message_chunk = {
  day_begin : Date.t;
  day_end : Date.t;
  number : int;
  messages_number : int;
  messages : Message.t list;
}

type chat = {
  name: string;
  number : int;
  messages: message_chunk list;
}
