
module ByteStream = struct
  type t = {
    buf : Bytes.t;
    mutable pos : int
  }

  let of_bytes bytes =
    {buf=bytes;
     pos=0}

  let of_file file =
    let buf = Buffer.create 0 in
    In_channel.with_open_bin file (fun inc ->
        let rec loop () =
          match In_channel.input_char inc with
          | Some c -> Buffer.add_char buf c; loop ()
          | None -> ()
        in
        loop ()
      );
    of_bytes @@  Buffer.to_bytes buf

  let take t =
    if t.pos < Bytes.length t.buf
    then begin
      let res = Bytes.get_uint8 t.buf t.pos in
      t.pos <- t.pos + 1;
      res
    end else
      raise Not_found
end


module Inst = struct
  type reg_size = X | L | H
  type register = A of reg_size | C of reg_size | D of reg_size | B of reg_size | SP | BP | SI | DI
  type location = Register of register | Address of string | Immediate of char

  let mod11_regw0_table = [|A L; C L; D L; B L; A H; C H; D H; B H|]
  let mod11_regw1_table = [|A X; C X; D X; B X; SP; BP; SI; DI|]

  type t = Mov of { dst : location; src : location }

  let parse stream =
    let b1 = ByteStream.take stream in
    let b2 = ByteStream.take stream in
    
    if (b1 land 0b11111100) lxor 0b10001000 = 0
    then
      let d = b1 land 0b00000010 > 0 in
      let w = b1 land 0b00000001 > 0 in
      let mod' = (b2 land 0b11000000) lsr 6 in
      let reg  = (b2 land 0b00111000) lsr 3 in
      let r'm  = b2 land 0b00000111 in
      let table = if w then mod11_regw1_table else mod11_regw0_table in
      let r1,r2 = if mod' = 3
        then Register table.(reg), Register table.(r'm)
        else failwith "only mod = 3 supported"
      in
      if d
      then Mov {src=r1; dst=r2}
      else Mov {src=r2; dst=r1}
    else begin
      failwith "error"
    end

  let add_size buf = function
    | L -> Buffer.add_char buf 'l'
    | H -> Buffer.add_char buf 'h'
    | X -> Buffer.add_char buf 'x'

  let add_reg buf = function
    | A s -> Buffer.add_char buf 'a'; add_size buf s
    | B s -> Buffer.add_char buf 'b'; add_size buf s
    | C s -> Buffer.add_char buf 'c'; add_size buf s
    | D s -> Buffer.add_char buf 'd'; add_size buf s
    | SP -> Buffer.add_string buf "sp"
    | BP -> Buffer.add_string buf "bp"
    | SI -> Buffer.add_string buf "si"
    | DI -> Buffer.add_string buf "di"
    
  let to_string = function
    | Mov {src=Register src;dst= Register dst} ->
      let buf = Buffer.create 0 in
      Buffer.add_string buf "mov ";
      add_reg buf src;
      Buffer.add_string buf ", ";
      add_reg buf dst;
      Buffer.to_bytes buf |> String.of_bytes
    | _ -> failwith "not supported"
end
  
  
  

  



