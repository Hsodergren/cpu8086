
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

  let take1 t =
    if t.pos < Bytes.length t.buf
    then begin
      let res = Bytes.get_uint8 t.buf t.pos in
      t.pos <- t.pos + 1;
      res
    end else
      raise Not_found

  let take t n =
    let buf = Bytes.create n in
    if t.pos + n < Bytes.length t.buf
    then begin
      Bytes.blit t.buf t.pos buf 0 n;
      t.pos <- t.pos + n;
      buf
    end else
      raise Not_found
end


module Inst = struct
  type reg_size = X | L | H
  type register = A of reg_size | C of reg_size | D of reg_size | B of reg_size | SP | BP | SI | DI

  type location = Register of register
                | Address of int
                | Immediate of int
                | Plus of location * location

  let mod11_regw0_table = [|A L; C L; D L; B L; A H; C H; D H; B H|]
  let mod11_regw1_table = [|A X; C X; D X; B X; SP; BP; SI; DI|]

  type t = Mov of { dst : location; src : location }

  let displacement stream = function
    | 0 -> 0
    | 1 ->
      ByteStream.take1 stream
    | 2 ->
      let bytes = ByteStream.take stream 2 in
      Bytes.get_uint16_ne bytes 0
    | _ -> assert false

  let f r'm displ =
    match r'm with
    | 0 -> Plus (Register (B X), Plus (Register SI, Address displ))
    | 1 -> Plus (Register (B X), Plus (Register DI, Address displ))
    | 2 -> Plus (Register BP, Plus (Register SI, Address displ))
    | 3 -> Plus (Register BP, Plus (Register DI, Address displ))
    | 4 -> Plus (Register SI, Address displ)
    | 5 -> Plus (Register DI, Address displ)
    | 6 -> Plus (Register BP, Address displ)
    | 7 -> Plus (Register (B X), Address displ)
    | _ -> assert false

  let reg_addr w r =
    let table = if w then mod11_regw1_table else mod11_regw0_table in
    Register table.(r)
    

  let parse stream =
    let b1 = ByteStream.take1 stream in
    if (b1 land 0b11111100) lxor 0b10001000 = 0 then
      let b2 = ByteStream.take1 stream in
      let d = b1 land 0b00000010 > 0 in
      let w = b1 land 0b00000001 > 0 in
      let mod' = (b2 land 0b11000000) lsr 6 in
      let reg  = (b2 land 0b00111000) lsr 3 in
      let r'm  = b2 land 0b00000111 in
      let reg = reg_addr w reg in
      let r1,r2 =
        if mod' = 3 then
          reg, reg_addr w r'm
        else
          let displ = displacement stream mod' in
          reg, f r'm displ
      in
      if d
      then Mov {src=r2; dst=r1}
      else Mov {src=r1; dst=r2}
    else if (b1 land 0b11110000) lxor 0b10110000 = 0 then
      let w = b1 land 0b00001000 > 0 in
      let reg = b1 land 0b00000111 |> reg_addr w in
      let value = displacement stream (if w then 2 else 1) in
      Mov {dst=reg; src=Immediate value}
    else begin 
      failwith "unknown opcode"
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

  let location_to_string loc =
    let buf = Buffer.create 0 in
    let rec to_string = function
      | Immediate value -> Buffer.add_string buf (string_of_int value)
      | Register reg -> add_reg buf reg
      | Address addr ->
        Buffer.add_string buf (string_of_int addr);
      | Plus (l1,Address 0) ->
        to_string l1
      | Plus (l1,l2) ->
        to_string l1;
        Buffer.add_string buf " + ";
        to_string l2
    in
    let loop loc =
      match loc with
      | Immediate _ | Register _ as v -> to_string v
      | Address _ | Plus _ as v->
        Buffer.add_string buf "[ ";
        to_string v;
        Buffer.add_string buf " ]"

    in
    loop loc;
    Buffer.to_bytes buf |> Bytes.to_string
    
  let to_string = function
    | Mov {src;dst} ->
      let buf = Buffer.create 0 in
      Buffer.add_string buf "mov ";
      Buffer.add_string buf (location_to_string dst);
      Buffer.add_string buf ", ";
      Buffer.add_string buf (location_to_string src);
      Buffer.to_bytes buf |> String.of_bytes
end
  
  
  

  



