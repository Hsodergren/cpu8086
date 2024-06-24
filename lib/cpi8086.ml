
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

  let take n t =
    let ret = Bytes.create n in
    Bytes.blit t.buf t.pos ret 0 n;
    t.pos <- t.pos + n;
    ret
end


module Inst = struct
  type register = AX | CX| DX | BX | SP | BP| SI | DI
  type location = Register of string | Address of string | Immediate of char

  type t = Mov of { dst : location; src : location }

  let parse stream =
    let bytes = ByteStream.take 2 stream in
    let b1 = Bytes.get_uint8 bytes 0 in
    let b2 = Bytes.get_uint8 bytes 1 in
    
    if (b1 land 0b11111100) lxor 0b10001000 > 0
    then
      let d = b1 land 0b00000010 > 0 in
      let w = b1 land 0b00000001 > 0 in
      let mod' = b2 land 0b11000000 lsl 6 in
      let reg  = b2 land 0b00111000 lsl 3 in
      let r'm  = b2 land 0b00000111 in
      ()
    else
      failwith "error"
    
end



