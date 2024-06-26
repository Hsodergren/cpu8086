open Cpu8086

let rec run stream =
  try
    let inst = Inst.parse stream in
    print_endline (Inst.to_string inst);
    run stream
  with Not_found -> ()
     | exn -> raise exn


let () =
  let stream = ByteStream.of_file Sys.argv.(1) in
  print_endline "bits 16";
  run stream
