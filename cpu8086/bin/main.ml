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
    let pre_ip = CPU.ip cpu in
    match seq () with
    | Seq.Cons (inst, tl) ->
      let next = CPU.handle inst cpu in
      let cur_ip = CPU.ip next in
      let old_flags, new_flags = Flags.to_string cpu.CPU.flags, Flags.to_string next.CPU.flags in
      let diff = CPU.diff_state cpu next in
      Printf.printf "%s ; %s"
        (Inst.to_string inst)
        (Registers.diff_to_string diff);
      Printf.printf " ip:0x%02X->0x%02X" pre_ip cur_ip;
      if old_flags <> new_flags
      then Printf.printf " || Flags: %s->%s\n" old_flags new_flags
      else Printf.printf "\n";
      loop tl next
    | Seq.Nil -> cpu
  in
  let final_state = loop seq (CPU.start stream) in
  print_endline "\nFINAL STATE";
  print_endline "===========";
  Registers.print final_state.CPU.registers;
  Printf.printf "ip: 0x%04X\n" (CPU.ip final_state)

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
