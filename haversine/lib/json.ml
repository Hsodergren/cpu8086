module Lexer = struct
  type t = {
      buf: string;
      mutable pos: int
    }

  type token =
    | String of string
    | Float of float
    | Colon
    | Comma
    | LeftBrace
    | LeftBracket
    | RightBrace
    | RightBracket [@@deriving show]

  let v buf = {buf;pos=0}

  let[@inline] advance t = t.pos <- t.pos +1
  let[@inline] peek t =
    if t.pos < String.length t.buf then
      Some (String.get t.buf t.pos)
    else None

  let[@inline] get t = Option.map (fun c -> advance t; c) (peek t)

  let string t =
    let buf = Buffer.create 0 in
    let rec aux t =
      match get t with
      | Some '"' -> Some (String (Buffer.contents buf))
      | Some '\\' -> begin
          match get t with
          | Some '\\' -> Buffer.add_char buf '\\'; aux t
          | Some '"' -> Buffer.add_char buf '"'; aux t
          | Some 'n' -> Buffer.add_char buf '\n'; aux t
          | Some 't' -> Buffer.add_char buf '\t'; aux t
          | Some _ -> failwith "invalid escaped string"
          | None -> None
        end
      | Some c -> Buffer.add_char buf c; aux t
      | None -> None
    in
    aux t

  let (let*) = Option.bind

  let float t =
    let buf = Buffer.create 0 in
    let rec loop t =
      match peek t with
      | Some ('0'..'9' | 'e' | '.' | '-' as c) -> advance t; Buffer.add_char buf c; loop t
      | Some _ -> Some ()
      | None -> None
    in
    let* () = loop t in
    let f = float_of_string (Buffer.contents buf) in
    Some (Float f)


  let rec lex t =
    let* c = peek t in
    match c with
    | '{' -> advance t; Some LeftBrace
    | '[' -> advance t; Some LeftBracket
    | ']' -> advance t; Some RightBracket
    | '}' -> advance t; Some RightBrace
    | ':' -> advance t; Some Colon
    | ',' -> advance t; Some Comma
    | '"' -> advance t; string t
    | ' ' | '\n' | '\t' -> advance t; lex t
    | _ -> float t

  let lex_peek t = lex {pos = t.pos; buf=t.buf}
end

type t =
  | Obj of (string * t) array
  | Arr of t array
  | Num of float
  | Str of string [@@deriving show]

let parse string =
  let lexer = Lexer.v string in
  let rec json_parse () =
    match Lexer.lex lexer with
    | Some Lexer.LeftBrace -> obj_parse []
    | Some Lexer.LeftBracket -> list_parse []
    | Some Lexer.String str -> Str str
    | Some Lexer.Float float -> Num float
    | Some _ -> failwith "syntax error"
    | None -> raise End_of_file
  and obj_parse acc =
    let obj () =
      match Lexer.lex lexer with
      | Some Lexer.Colon ->
         let o = json_parse () in
         o
      | _ -> failwith "obj syntax error 2"
    in
    let lex = Lexer.lex lexer in
    match lex with
    | Some Lexer.RightBrace -> Obj (List.rev acc |> Array.of_list)
    | Some Lexer.Comma -> obj_parse acc
    | Some (Lexer.String str) ->
       let o = obj () in
       obj_parse ((str,o)::acc)
    | Some _ -> failwith "obj syntax error"
    | None -> raise End_of_file
  and list_parse acc =
    match Lexer.lex_peek lexer with
    | Some Lexer.RightBracket -> ignore (Lexer.lex lexer); Arr (List.rev acc |> Array.of_list)
    | Some Lexer.Comma -> ignore (Lexer.lex lexer); list_parse acc
    | Some _ ->
       let o = json_parse () in
       list_parse (o::acc)
    | None -> raise End_of_file
  in
  json_parse ()


let member key t =
  match t with
  | Obj l -> Array.find_map (fun (k,v) -> if k = key then Some v else None ) l
  | _ -> failwith "needs to be an obj"

let member_exn key t =
  member key t |> Option.get

let float = function
  | Num f -> f
  | _ -> failwith "needs to be a float"
