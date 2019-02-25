(* Rough-and-ready parsing of orgmode files
   2019 T. Bourke
   BSD-2 License

   Requires Str (str.cma, str.cmxa)

   See: https://orgmode.org/worg/dev/org-syntax.html

   Supported:
   - Keywords (in Headlines)
   - Priorities
   - Tags
   - Dates (from headlines and sections)
   - Planning Dates (from line following headline)
   - Comment Headlines

   Parse files into nested headlines (fully parsed) and sections (lists of
   strings)

   Not supported:
   - Greater Blocks
   - Drawers
   - Property Drawers
   - Dynamic Blocks
   - Footnotes
   - Inline tasks
   - Plain Lists and Items
   - Tables
   - Elements
   - Comments
   - Fixed Width Areas
   - Horizontal Rules
   - Keywords in Sections
   - LaTeX Environments
   - Node Properties
   - Paragraphs
   - Table Rows
   - Objects (apart from timestamps)
   - Dynamic adjustement of accepted Keywords

*)

type keyword =
  | Todo
  | Done
  | Other of string

let keywords = ref [ "TODO"; "DONE" ]

type date = {
    day   : int;
    month : int;
    year  : int;
  }

type time = {
    hour    : int;
    minutes : int;
  }

type repeater_type =
    Cumulate  (* +  *)
  | CatchUp   (* ++ *)
  | Restart   (* .+ *)

type time_inc =
    Hours
  | Days
  | Weeks
  | Months
  | Years

type repeater = repeater_type * int * time_inc

type warning_delay_type =
    AllOccurrences  (* -  *)
  | FirstOccurrence (* -- *)

type warning_delay = warning_delay_type * int * time_inc

type repeater_or_delay =
    Repeater of repeater
  | Delay of warning_delay

type single_timestamp = {
    active  : bool;
    date    : date;
    time    : time option;
    repeat  : repeater option;
    warning : warning_delay option;
  }

type timestamp =
  | Timestamp of single_timestamp
  | Range of single_timestamp * single_timestamp
  | HourRange of single_timestamp * time

type planning_type =
  | Deadline
  | Scheduled
  | Closed

type planning = planning_type * timestamp

type 'sec item = {
    level      : int;
    commented  : bool;
    keyword    : keyword option;
    priority   : char option;
    title      : string;
    tags       : string list;
    dates      : timestamp list;
    planning   : planning list;
    sections   : 'sec list;
    categories : 'sec item list;
  }

module LazyStream = struct

  type 'a cell = SNil | SCons of 'a * 'a t
  and 'a t = ('a cell) Lazy.t

  let empty = lazy (SNil)

  let fromf f =
    let rec gen () =
      lazy (match f () with
            | None -> SNil
            | Some v -> SCons (v, gen ()))
    in
    gen ()

  let map f =
    let rec go xs =
      lazy (match Lazy.force xs with
            | SNil -> SNil
            | SCons (x, xs') -> SCons (f x, go xs'))
    in
    go

end

module Parser = struct

  (* Regular expressions *)

  let rec or_list = function
    | []  -> ""
    | [x] -> x
    | x::xs -> x ^ "\\|" ^ or_list xs

  let space = "[ \t\n\r\\f\\v]"
  let spaces = "[ \t\n\r\\f\\v]+"
  let spaces' = "[ \t\n\r\\f\\v]*"
  let alpha = "[A-Za-z]"

  (* 1: asterisks
     3: keyword
     5: priority
     6: comment
     7: title (and tags)
     NB: string must end with a \n *)
  let re_category = Str.regexp
    ("^\\(\\*+\\)" ^ spaces'
     ^ "\\(\\(" ^ or_list !keywords ^ "\\)" ^ spaces ^ "\\)?"
     ^ "\\(\\[#\\(" ^ alpha ^ "\\)\\]" ^ spaces ^ "\\)?"
     ^ "\\(COMMENT" ^ spaces ^ "\\)?"
     ^ "\\(.*\\)")

  (* 1: heading
     2: tags      *)
  let re_tags = Str.regexp
      ("^\\(.*\\)"
       ^ spaces ^ "\\(\\(:[A-Za-z0-9_@#%]+\\)+:\\)"
       ^ spaces' ^ "$")

  let date =
    "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)"
    ^ "\\(" ^ spaces ^ "[^]+->0-9 \t\n\r\\f\\v]+\\)?"

  let time = "\\([0-9][0-9]?\\):\\([0-9][0-9]\\)"
  let repeater_or_delay' =
    "\\(\\(\\+\\|\\+\\+\\|.\\+\\|-\\|--\\)\\([0-9]+\\)\\([hdwmy]\\)\\)"
  let repeater_or_delay =
    "\\(" ^ spaces ^ repeater_or_delay' ^ "?"
          ^ spaces' ^ repeater_or_delay' ^ "?"
    ^ "\\)?"

  let timestamp_body =
    date ^ "\\(" ^ spaces ^ time ^ "\\)?" ^ repeater_or_delay

  let timestamp_range_body =
    date ^ spaces ^ time ^ "-" ^ time ^ repeater_or_delay

  let timestamp =
    (* "<2006-11-01 Wed 19:15 +1w--1h>--<2007-12-02 Thu 06:10 +1d++2m>" *)
    "\\(<" ^ timestamp_body ^ ">\\(--<" ^ timestamp_body ^ ">\\)?\\)"
    ^ "\\|"
    (* "[2006-11-01 Wed 19:15 +1w--1h]--[2007-12-02 Thu 06:10 +1d++2m]"
       /  +1  +2 +3  +4 +6 +7 /|\ /|\ 18+,50+...
      1                      // / \\ \
     32               +10,+11,+12  +14,+15,+16 *)
    ^ "\\(\\[" ^ timestamp_body ^ "\\]\\(--\\[" ^ timestamp_body ^ "\\]\\)?\\)"
    ^ "\\|"
    (* "<2006-11-01 Wed 19:15-20:00 +1w--1h>" *)
    ^ "\\(<" ^ timestamp_range_body ^ ">\\)"
    ^ "\\|"
    ^ "\\(\\[" ^ timestamp_range_body ^ "\\]\\)"
    (* "[2006-11-01 Wed 19:15-20:00 +1w--1h]"
       /  +1  +2 +3  +4 +5 +6 +7 +8 /|\ /|\
     66                            // / \\ \
     84                    +11,+12,+13   +15,+16,+17 *)

  let re_timestamp = Str.regexp timestamp

  let re_planning = Str.regexp
    ("\\(\\(DEADLINE\\|SCHEDULED\\|CLOSED\\):" ^ spaces' ^ "\\)?"
     ^ "\\(" ^ timestamp ^ "\\)")

  let repeat_parse re parse s =
    let rec go acc i =
      try
        ignore (Str.search_forward re s i);
        go (parse 0 s :: acc) (Str.match_end ())
      with Not_found -> List.rev acc
    in
    go [] 0

  (* debugging *)
  let groups re s =
    try
      let n = Str.search_forward re s 0 in
      Printf.printf "match at %d\n" n;
      for i = 0 to 100 do
        match Str.matched_group i s with
        | s -> Printf.printf "%d -> %s\n" i s
        | exception Not_found -> Printf.printf "%d ->\n" i
      done
    with
    | Not_found -> ()
    | Invalid_argument _ -> ()

  (* Parsing Functions *)

  type 'a line =
    | Headline of {
        level     : int;
        keyword   : keyword option;
        priority  : char option;
        commented : bool;
        title     : string;
        tags      : string list;
      }
    | SectionLine of string

  let group g s =
    match Str.matched_group g s with
    | t -> Some t
    | exception Not_found -> None

  let ochar i l =
    match group i l with
    | None -> None
    | Some s ->
        (match String.get s 1 with
         | c -> Some c
         | exception Invalid_argument _ -> None)
    | exception Not_found -> None

  let okeyword i l =
    match group i l with
    | None                -> None
    | Some "TODO"         -> Some Todo
    | Some "DONE"         -> Some Done
    | Some s              -> Some (Other s)
    | exception Not_found -> None

  let drop_first_char s =
    String.(sub s 1 (length s - 1))

  let get_tags s =
    if Str.string_match re_tags s 0
    then String.split_on_char ':' (drop_first_char (Str.matched_group 2 s)),
         String.trim (Str.matched_group 1 s)
    else [], String.trim s

  let has_group i s =
    match Str.matched_group i s with
    | s -> true
    | exception Not_found -> false

  let to_int i s =
    match Str.matched_group i s with
    | s -> int_of_string s
    | exception Not_found -> 0

  let to_date i s = {
      day   = to_int i s;
      month = to_int (i + 1) s;
      year  = to_int (i + 2) s;
    }

  let to_time i s =
    if has_group i s
    then Some { hour    = to_int i s;
                minutes = to_int (i + 1) s; }
    else None

  let to_repeater_type i s =
    match Str.matched_group i s with
    | "+"  -> Cumulate
    | "++" -> CatchUp
    | ".+" -> Restart
    | _    -> assert false

  let to_warning_delay_type i s =
    match Str.matched_group i s with
    | "-"  -> AllOccurrences
    | "--" -> FirstOccurrence
    | _    -> assert false

  let to_time_inc i s =
    match Str.matched_group i s with
    | "h" -> Hours
    | "d" -> Days
    | "w" -> Weeks
    | "m" -> Months
    | "y" -> Years
    | _   -> assert false

  let to_repeater i s =
    to_repeater_type i s, to_int (i+1) s, to_time_inc (i+2) s

  let to_repeater_or_delay i s =
    match Str.matched_group i s with
    | "+"  -> Some (Repeater
                      (Cumulate,        to_int (i+1) s, to_time_inc (i+2) s))
    | "++" -> Some (Repeater
                      (CatchUp,         to_int (i+1) s, to_time_inc (i+2) s))
    | ".+" -> Some (Repeater
                      (Restart,         to_int (i+1) s, to_time_inc (i+2) s))
    | "-"  -> Some (Delay
                      (AllOccurrences,  to_int (i+1) s, to_time_inc (i+2) s))
    | "--" -> Some (Delay
                      (FirstOccurrence, to_int (i+1) s, to_time_inc (i+2) s))
    | _    -> assert false
    | exception Not_found -> None

  let to_repeater_and_delay i s =
    let rd1 = to_repeater_or_delay i s in
    let rd2 = to_repeater_or_delay (i+4) s in
    match rd1, rd2 with
    | None,              None              -> None,   None
    | Some (Repeater r), None              -> Some r, None
    | Some (Delay d),    None              -> None,   Some d
    | None,              Some (Repeater r) -> Some r, None
    | None,              Some (Delay d)    -> None,   Some d
    | Some (Repeater r), Some (Delay d)    -> Some r, Some d
    | Some (Delay d),    Some (Repeater r) -> Some r, Some d
    | Some (Repeater _), Some (Repeater _) -> None,   None
    | Some (Delay _),    Some (Delay _)    -> None,   None

  let to_single_timestamp active i s =
    let r, d = to_repeater_and_delay (i+10) s in
    {
      active  = active;
      date    = to_date (i+1) s;
      time    = to_time (i+6) s;
      repeat  = r;
      warning = d;
    }

  let to_timestamp_range active i s =
    let r, d = to_repeater_and_delay (i+11) s in
    match to_time (i+7) s with
    | None -> assert false
    | Some end_time ->
        HourRange ({ active  = active;
                     date    = to_date (i+1) s;
                     time    = to_time (i+5) s;
                     repeat  = r;
                     warning = d }, end_time)

  let to_timestamp i s =
    let i = i + 1 in
    if has_group i s then begin
      if has_group (i+17) s then Range (to_single_timestamp true i s,
                                        to_single_timestamp true (i+17) s)
      else Timestamp (to_single_timestamp true i s)
    end else if has_group (i+34) s then begin
      if has_group (i+51) s then Range (to_single_timestamp false (i+34) s,
                                        to_single_timestamp false (i+51) s)
      else Timestamp (to_single_timestamp true (i+34) s)
    end
    else if has_group (i+68) s then to_timestamp_range true (i+68) s
    else if has_group (i+86) s then to_timestamp_range false (i+86) s
    else assert false

  let to_planning_type i s =
    match Str.matched_group i s with
    | "DEADLINE"  -> Some Deadline
    | "SCHEDULED" -> Some Scheduled
    | "CLOSED"    -> Some Closed
    | _           -> assert false
    | exception Not_found -> None

  let to_planning i s = to_planning_type (i+2) s, to_timestamp (i+3) s

(*
  let tests_planning = [|
    "<2006-11-01 Wed 19:15>";
    "[2006-11-01 Wed 19:15]";
    "<2006-11-02 Thu 20:00-22:00>";
    "[2006-11-02 Thu 20:00-22:00]";
    "<2006-11-02 Thu 20:00-22:00 -1d+2w>";
    "[2006-11-02 Thu 20:00-22:00 +2w -1d]";
    "<2007-05-16 Wed 12:30 +1w>";
    "[2007-05-16 Wed 12:30 +1w]";
    "<2004-08-23 Mon>--<2004-08-26 Thu>";
    "[2004-08-23 Mon]--[2004-08-26 Thu]";
    "<2004-08-23 Mon 12:00>--<2004-08-26 Thu 14:00>";
    "[2004-08-23 Mon 12:00]--[2004-08-26 Thu 14:00]";
    "<2006-11-01 Wed>";
    "[2006-11-01 Wed]";
    "<2006-11-01>";
    "[2006-11-01]";
    "DEADLINE: <2004-02-29 Sun>";
    "DEADLINE: [2004-02-29 Sun]";
    "SCHEDULED: <2004-12-25 Sat>";
    "SCHEDULED: [2004-12-25 Sat]";
  |]
  let test_planning s =
    if Str.string_match re_planning s 0
    then to_planning 0 s
    else assert false
*)

  let parse_lines : string LazyStream.t -> string line LazyStream.t =
    let rec f level si =
      lazy (match Lazy.force si with
            | LazyStream.SNil -> LazyStream.SNil
            | LazyStream.SCons (l, si') ->
                let (level', v) =
                  if Str.string_match re_category l 0
                  then
                    let level'   = String.length (Str.matched_group 1 l) in
                    let keyword  = okeyword 3 l in
                    let priority = ochar 5 l in
                    let commented  = has_group 6 l in
                    let tags, title = get_tags (Str.matched_group 7 l) in
                    level', Headline { level = level';
                                       keyword; priority; commented; title; tags}
                  else level, SectionLine l
                in
                LazyStream.SCons (v, f level' si'))
    in
    f 0

  let split_planning =
    let rec f dates plans = function
      | [] -> (List.rev dates, List.rev plans)
      | (None, ts)::xs   -> f (ts::dates) plans xs
      | (Some p, ts)::xs -> f dates ((p, ts)::plans) xs
    in
    f [] []

  let read_dates title ss =
    let tdates = repeat_parse re_timestamp to_timestamp title in
    match ss with
    | []    -> tdates, []
    | s::ss ->
        let pdates, planning =
          split_planning (repeat_parse re_planning to_planning s) in
        let odates =
          List.(concat (map (repeat_parse re_timestamp to_timestamp) ss)) in
        tdates @ pdates @ odates, planning

  let rec read_headlines level acc si =
    match Lazy.force si with
    | LazyStream.SNil -> List.rev acc, LazyStream.empty
    | LazyStream.SCons (v, si1) ->
        match v with
        | Headline ({ level = level' } as c) ->
            if level' <= level then List.rev acc, si
            else
              let cs, si2 = slurp_contents [] si1 in
              let vs, si3 = read_headlines level' [] si2 in
              let dates, planning = read_dates c.title cs in
              let v = {
                level      = c.level;
                commented  = c.commented;
                keyword    = c.keyword;
                priority   = c.priority;
                title      = c.title;
                tags       = c.tags;
                dates      = dates;
                planning   = planning;
                sections   = cs;
                categories = vs;
              } in
              read_headlines level (v::acc) si3
        | SectionLine s ->
            (* contents before any headings *)
            let cs, si2 = slurp_contents [] si1 in
            let cs = s::cs in
            let dates, planning = read_dates "" cs in
            let v = {
                level      = 0;
                commented  = false;
                keyword    = None;
                priority   = None;
                title      = "";
                tags       = [];
                dates      = dates;
                planning   = planning;
                sections   = cs;
                categories = [];
            } in
            read_headlines level (v::acc) si2

    and slurp_contents acc si =
      match Lazy.force si with
      | LazyStream.SNil -> List.rev acc, LazyStream.empty
      | LazyStream.SCons (v, si1) ->
          match v with
          | Headline hl -> List.rev acc, si
          | SectionLine s  -> slurp_contents (s::acc) si1

end

let parse_stream si = fst (Parser.read_headlines 0 [] (Parser.parse_lines si))

let read_lines chan : string LazyStream.t =
  let f () =
    match input_line chan with
    | l -> Some l
    | exception End_of_file -> None
  in
  LazyStream.fromf f

let parse_channel chan = parse_stream (read_lines chan)

