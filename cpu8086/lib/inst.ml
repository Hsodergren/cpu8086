type address = Addr of int | Reg of Registerlabel.t | Plus of address * address

type location = Register of Registerlabel.t
              | Address of address
              | Immediate of (int * bool)

let mod11_regw0_table = Registerlabel.[|A `L; C `L; D `L; B `L; A `H; C `H; D `H; B `H|]
let mod11_regw1_table = Registerlabel.[|A `X; C `X; D `X; B `X; SP; BP; SI; DI|]

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
    Bytestream.take1 ~signed:false stream
  | 2 ->
    let bytes = Bytestream.take stream 2 in
    if signed
    then Bytes.get_uint16_ne bytes 0
    else Bytes.get_int16_ne bytes 0
  | _ -> assert false

let f r'm displ =
  match r'm with
  | 0 -> Plus (Reg (B `X), Plus (Reg SI, Addr displ))
  | 1 -> Plus (Reg (B `X), Plus (Reg DI, Addr displ))
  | 2 -> Plus (Reg BP, Plus (Reg SI, Addr displ))
  | 3 -> Plus (Reg BP, Plus (Reg DI, Addr displ))
  | 4 -> Plus (Reg SI, Addr displ)
  | 5 -> Plus (Reg DI, Addr displ)
  | 6 -> Plus (Reg BP, Addr displ)
  | 7 -> Plus (Reg (B `X), Addr displ)
  | _ -> assert false

let reg_addr w reg =
  let table = if w then mod11_regw1_table else mod11_regw0_table in
  Register table.(reg)


let get_displacement stream mod' w reg =
  if mod' = 3 then
    reg_addr w reg
  else if reg = 0b110 && mod' = 0 then
    let address = displacement ~signed:true stream 2 in
    Address (Addr address)
  else
    let displ = displacement ~signed:true stream mod' in
    Address (f reg displ)

let mem_to_from_reg b1 stream =
  let b2 = Bytestream.take1 stream in
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
  let b2 = Bytestream.take1 stream in
  let w = b1 land 0b00000001 > 0 in
  let s = not (b1 land 0b00000010 > 0) in
  let mod' = (b2 land 0b11000000) lsr 6 in
  let r'm  = b2 land 0b00000111 in
  let dst = get_displacement stream mod' w r'm in
  let src = Immediate (displacement ~signed:false stream  (if s && w then 2 else 1), w) in
  dst, src, b2

let parse stream =
  let b1 = Bytestream.take1 ~signed:false stream in
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
    Mov {dst=Register (A `X); src=Address (Addr data)}
  else if (b1 land 0b11111110) lxor 0b10100010 = 0 then
    let w = b1 land 0b00000001 > 0 in
    let data = displacement ~signed:true stream (if w then 2 else 1) in
    Mov {dst=Address (Addr data); src=Register (A `X)};
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
    let dst,src = Register (A `X), Immediate (data,w) in
    match (b1 land 0b00111000) lsr 3 with
    | 0 -> Add {dst;src}
    | 5 -> Sub {dst;src}
    | 7 -> Cmp {dst;src}
    | _ -> failwith "unknown operation"
  else if b1 lxor 0b01110100 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Je offset
  else if b1 lxor 0b01111100 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jl offset
  else if b1 lxor 0b01111110 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jle offset
  else if b1 lxor 0b01110010 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jb offset
  else if b1 lxor 0b01110110 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jbe offset
  else if b1 lxor 0b01111010 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jp offset
  else if b1 lxor 0b01110000 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jo offset
  else if b1 lxor 0b01111000 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Js offset
  else if b1 lxor 0b01110101 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jne offset
  else if b1 lxor 0b01111101 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jnl offset
  else if b1 lxor 0b01111111 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jnle offset
  else if b1 lxor 0b01110011 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jnb offset
  else if b1 lxor 0b01110111 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jnbe offset
  else if b1 lxor 0b01111011 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jnp offset
  else if b1 lxor 0b01110001 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jno offset
  else if b1 lxor 0b01111001 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jns offset
  else if b1 lxor 0b11100010 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Loop offset
  else if b1 lxor 0b11100001 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Loopz offset
  else if b1 lxor 0b11100000 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Loopnz offset
  else if b1 lxor 0b11100011 = 0 then
    let offset = Bytestream.take1 ~signed:true stream in
    Jcxz offset
  else
    failwith "unknown opcode"


let location_to_string loc =
  let buf = Buffer.create 0 in
  let rec to_string = function
    | Addr addr ->
      Buffer.add_string buf (string_of_int addr);
    | Reg reg -> Registerlabel.add_reg buf reg
    | Plus (l1,Addr 0) -> to_string l1
    | Plus (l1, Addr value) ->
      to_string l1;
      (if value > 0
       then Buffer.add_string buf " + "
       else Buffer.add_string buf " - ");
      to_string (Addr (abs value))
    | Plus (l1,l2) ->
      to_string l1;
      Buffer.add_string buf " + ";
      to_string l2
  in
  let loop loc =
    match loc with
    | Immediate (value,_) -> Buffer.add_string buf (string_of_int value)
    | Register reg -> Registerlabel.add_reg buf reg
    | Address a ->
      Buffer.add_char buf '[';
      to_string a;
      Buffer.add_char buf ']'

  in
  loop loc;
  Buffer.to_bytes buf |> Bytes.to_string

let add_registers buf dst src =
  Buffer.add_string buf (location_to_string dst);
  Buffer.add_string buf ", ";
  (match dst,src with
   | Address _, Immediate (_,w) -> Buffer.add_string buf (if w then "word " else "byte ")
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

let of_stream stream =
  Seq.of_dispenser (fun () -> try Some (parse stream) with Not_found -> None)
