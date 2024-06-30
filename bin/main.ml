open Cpu8086


let rec disassemble stream =
  try
    let inst = Inst.parse stream in
    print_endline (Inst.to_string inst);
    disassemble stream
  with Not_found -> ()
     | exn -> raise exn

let exec stream =
  let seq = Inst.of_stream stream in
  let rec loop seq cpu =
    match seq () with
    | Seq.Cons (inst, tl) ->
      let next = CPU.handle inst cpu in
      let diff = CPU.diff_state cpu next in
      Printf.printf "%s ; %s\n" (Inst.to_string inst) (Registers.diff_to_string diff);
      loop tl next
    | Seq.Nil -> cpu
  in
  let final_state = loop seq (CPU.start stream) in
  print_endline "\nFINAL STATE";
  print_endline "===========";
  Registers.print final_state.CPU.registers

open Cmdliner

let stream =
  let doc = "Assembly file" in
  Arg.(required @@ pos 0 (some file) None @@ info ~doc [])
  |> Term.map Bytestream.of_file

let exec_cmd =
  Cmd.v (Cmd.info "exec") Term.(const exec $ stream)

let disassemble_cmd =
  Cmd.v (Cmd.info "dis") Term.(const disassemble $ stream)

let cmd = Cmd.group (Cmd.info "") [exec_cmd; disassemble_cmd]

let () =
  ignore @@  Cmd.eval cmd
