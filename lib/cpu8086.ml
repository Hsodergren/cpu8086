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

  let take1 ?(signed=true) t =
    if t.pos < Bytes.length t.buf
    then begin
      let res =
        if signed 
        then Bytes.get_int8 t.buf t.pos
        else Bytes.get_uint8 t.buf t.pos
      in
      t.pos <- t.pos + 1;
      res
    end else
      raise Not_found

  let take t n =
    let buf = Bytes.create n in
    if t.pos + n <= Bytes.length t.buf
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
                | Immediate of (int * bool)
                | Plus of location * location

  let mod11_regw0_table = [|A L; C L; D L; B L; A H; C H; D H; B H|]
  let mod11_regw1_table = [|A X; C X; D X; B X; SP; BP; SI; DI|]

  type t = Mov of { dst : location; src : location }
         | Add of { dst : location; src : location }
         | Sub of { dst : location; src : location }
         | Cmp of { dst : location; src : location }
         | Je of int
         | Jl of int
         | Jle of int
         | Jb of int
         | Jbe of int
         | Jp of int
         | Jo of int
         | Js of int
         | Jne of int
         | Jnl of int
         | Jnle of int
         | Jnb of int
         | Jnbe of int
         | Jnp of int
         | Jno of int
         | Jns of int
         | Loop of int
         | Loopz of int
         | Loopnz of int
         | Jcxz of int

  let displacement ~signed stream = function
    | 0 -> 0
    | 1 ->
      ByteStream.take1 ~signed:false stream
    | 2 ->
      let bytes = ByteStream.take stream 2 in
      if signed
      then Bytes.get_uint16_ne bytes 0
      else Bytes.get_int16_ne bytes 0
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

  let reg_addr w reg =
    let table = if w then mod11_regw1_table else mod11_regw0_table in
    Register table.(reg)


  let get_displacement stream mod' w reg =
    if mod' = 3 then
      reg_addr w reg
    else if reg = 0b110 && mod' = 0 then
      let address = displacement ~signed:true stream 2 in
      Address address
    else
      let displ = displacement ~signed:true stream mod' in
      f reg displ

  let mem_to_from_reg b1 stream = 
      let b2 = ByteStream.take1 stream in
      let w = b1 land 0b00000001 > 0 in
      let d = b1 land 0b00000010 > 0 in
      let mod' = (b2 land 0b11000000) lsr 6 in
      let reg  = (b2 land 0b00111000) lsr 3 in
      let r'm  = b2 land 0b00000111 in
      let reg = reg_addr w reg in
      let r'm = get_displacement stream mod' w r'm in
      if d
      then reg, r'm
      else r'm, reg

  let imm_to_reg_or_mem b1 stream =
    let b2 = ByteStream.take1 stream in
    let w = b1 land 0b00000001 > 0 in
    let s = not (b1 land 0b00000010 > 0) in
    let mod' = (b2 land 0b11000000) lsr 6 in
    let r'm  = b2 land 0b00000111 in
    let dst = get_displacement stream mod' w r'm in
    let src = Immediate (displacement ~signed:false stream  (if s && w then 2 else 1), w) in
    dst, src, b2
      
  let parse stream =
    let b1 = ByteStream.take1 ~signed:false stream in
    if (b1 land 0b11111100) lxor 0b10001000 = 0 then
      let dst, src = mem_to_from_reg b1 stream in
      Mov {dst;src}
    else if (b1 land 0b11111110) lxor 0b11000110 = 0 then
      let dst,src,_ = imm_to_reg_or_mem b1 stream in
      Mov {dst;src}
    else if (b1 land 0b11110000) lxor 0b10110000 = 0 then
      let w = b1 land 0b00001000 > 0 in
      let reg = b1 land 0b00000111 |> reg_addr w in
      let value = displacement ~signed:true stream (if w then 2 else 1) in
      Mov {dst=reg; src=Immediate (value,w)}
    else if (b1 land 0b11111110) lxor 0b10100000 = 0 then
      let w = b1 land 0b00000001 > 0 in
      let data = displacement ~signed:true stream (if w then 2 else 1) in
      Mov {dst=Register (A X); src=Address data}
    else if (b1 land 0b11111110) lxor 0b10100010 = 0 then
      let w = b1 land 0b00000001 > 0 in
      let data = displacement ~signed:true stream (if w then 2 else 1) in
      Mov {dst=Address data; src=Register (A X)};
    else if (b1 land 0b11000100) lxor 0 = 0 then 
      let op = (b1 land 0b00111000) lsr 3 in
      let dst,src = mem_to_from_reg b1 stream in
      match op with
      | 0 -> Add {dst;src}
      | 5 -> Sub {dst;src}
      | 7 -> Cmp {dst;src}
      | _ -> failwith "unknown operation"
    else if (b1 land 0b11111100) lxor 0b10000000 = 0 then
      let dst,src,b2 = imm_to_reg_or_mem b1 stream in
      match (b2 land 0b00111000) lsr 3 with
      | 0 -> Add {dst;src}
      | 5 -> Sub {dst;src}
      | 7 -> Cmp {dst;src}
      | _ -> failwith "unknown operation"
    else if (b1 land 0b11000110) lxor 0b00000100 = 0 then
      let w = b1 land 0b00000001 > 0 in
      let data = displacement ~signed:true stream (if w then 2 else 1) in
      let dst,src = Register (A X), Immediate (data,w) in
      match (b1 land 0b00111000) lsr 3 with
      | 0 -> Add {dst;src}
      | 5 -> Sub {dst;src}
      | 7 -> Cmp {dst;src}
      | _ -> failwith "unknown operation"
    else if b1 lxor 0b01110100 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Je offset
    else if b1 lxor 0b01111100 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jl offset
    else if b1 lxor 0b01111110 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jle offset
    else if b1 lxor 0b01110010 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jb offset
    else if b1 lxor 0b01110110 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jbe offset
    else if b1 lxor 0b01111010 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jp offset
    else if b1 lxor 0b01110000 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jo offset
    else if b1 lxor 0b01111000 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Js offset
    else if b1 lxor 0b01110101 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jne offset
    else if b1 lxor 0b01111101 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jnl offset
    else if b1 lxor 0b01111111 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jnle offset
    else if b1 lxor 0b01110011 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jnb offset
    else if b1 lxor 0b01110111 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jnbe offset
    else if b1 lxor 0b01111011 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jnp offset
    else if b1 lxor 0b01110001 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jno offset
    else if b1 lxor 0b01111001 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jns offset
    else if b1 lxor 0b11100010 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Loop offset
    else if b1 lxor 0b11100001 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Loopz offset
    else if b1 lxor 0b11100000 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Loopnz offset
    else if b1 lxor 0b11100011 = 0 then
      let offset = ByteStream.take1 ~signed:true stream in
      Jcxz offset
    else
      failwith "unknown opcode"

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
      | Immediate (value, _) ->
        Buffer.add_string buf (string_of_int value)
      | Register reg -> add_reg buf reg
      | Address addr ->
        Buffer.add_string buf (string_of_int addr);
      | Plus (l1,Address 0) ->
        to_string l1
      | Plus (l1, Address value) ->
        to_string l1;
        (if value > 0
        then Buffer.add_string buf " + "
        else Buffer.add_string buf " - ");
        to_string (Address (abs value))
      | Plus (l1,l2) ->
        to_string l1;
        Buffer.add_string buf " + ";
        to_string l2
    in
    let loop loc =
      match loc with
      | Immediate _ | Register _ as v -> to_string v
      | Address _ | Plus _ as v ->
        Buffer.add_char buf '[';
        to_string v;
        Buffer.add_char buf ']'

    in
    loop loc;
    Buffer.to_bytes buf |> Bytes.to_string

  let add_registers buf dst src =
      Buffer.add_string buf (location_to_string dst);
      Buffer.add_string buf ", ";
      (match dst,src with
       | (Plus _ | Address _), Immediate (_,w) -> Buffer.add_string buf (if w then "word " else "byte ")
       | _ -> ());
      Buffer.add_string buf (location_to_string src)

  let add_jump buf accro offset =
    Buffer.add_string buf accro;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (string_of_int offset)
      
  let to_string inst =
    let buf = Buffer.create 0 in
    let () =
      match inst with
    | Mov {dst;src} ->
      Buffer.add_string buf "mov ";
      add_registers buf dst src
    | Add {dst;src} ->
      Buffer.add_string buf "add ";
      add_registers buf dst src
    | Sub {dst;src} ->
      Buffer.add_string buf "sub ";
      add_registers buf dst src
    | Cmp {dst;src} ->
      Buffer.add_string buf "cmp ";
      add_registers buf dst src
    | Je v -> add_jump buf "je" v
    | Jl v -> add_jump buf "jl" v
    | Jle v -> add_jump buf "jb" v

    | Jb v -> add_jump buf "jb" v
    | Jbe v -> add_jump buf "jbe" v
    | Jp v -> add_jump buf "jp" v
    | Jo v -> add_jump buf "jo" v
    | Js v -> add_jump buf "js" v
    | Jne v -> add_jump buf "jne" v
    | Jnl v -> add_jump buf "jnl" v
    | Jnle v -> add_jump buf "jnle" v
    | Jnb v -> add_jump buf "jnb" v
    | Jnbe v -> add_jump buf "jnbe" v
    | Jnp v -> add_jump buf "jnp" v
    | Jno v -> add_jump buf "jno" v
    | Jns v -> add_jump buf "jns" v
    | Loop v -> add_jump buf "loop" v
    | Loopz v -> add_jump buf "loopz" v
    | Loopnz v -> add_jump buf "loopnz" v
    | Jcxz v -> add_jump buf "jcxz" v
    in
    Buffer.to_bytes buf |> String.of_bytes
end
