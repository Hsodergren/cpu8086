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
