open Cmdliner

type gen_method = | Uniform | Region
let gen_method_of_string = function
  | "uniform" -> Ok Uniform
  | "region" -> Ok Region
  | _ -> Error (`Msg "Only uniform and region are valid")

let printer ppf a =
  match a with
  | Uniform -> Format.fprintf ppf "uniform"
  | Region -> Format.fprintf ppf "region"

let gen_method =
  let doc = "point generation method" in
  let docv = "METHOD" in
  let meth_conv = Arg.conv ~docv (gen_method_of_string,printer) in
  Arg.(required @@ pos 0 (some meth_conv) None @@ info ~doc ~docv [])

let seed =
  let doc = "seed for random lines" in
  let docv = "SEED" in
  Arg.(value @@ opt (some int) None @@ info ~doc ~docv ["s"; "seed"])
  |> Term.map (function | None -> Random.State.make_self_init ()
                        | Some v -> Random.State.make [|v|])

let lines =
  let doc = "number of lines" in
  let docv = "LINES" in
  Arg.(required @@ opt (some int) None @@ info ~doc ~docv ["l"; "lines"])

let file =
  let doc = "output file" in
  let docv = "FILE" in
  Arg.(value @@ opt string "-" @@ info ~doc ~docv ["f"; "file"])

let validation =
  let doc = "validation bin file" in
  let docv = "FILE" in
  Arg.(value @@ opt (some file) None @@ info ~doc ~docv ["v"; "validation"])

let in_file =
  let doc = "input file" in
  let docv = "FILE" in
  Arg.(required @@ opt (some file) None @@ info ~doc ~docv ["i"; "file"])


let[@inline] gen_point cx cy dx dy state =
  let dx2 = dx /. 2. in
  let dy2 = dy /. 2. in
  let x1 = Random.State.float state dx -. dx2 +. cx in
  let y1 = Random.State.float state dy -. dy2 +. cy in
  (x1,y1)

let[@inline] gen_line cx cy dx dy state =
  let p1 = gen_point cx cy dx dy state in
  let p2 = gen_point cx cy dx dy state in
  Haversine.Line.v p1 p2

let avg_arr fs =
  let sum,len = Array.fold_left (fun (sum,len) f -> f +. sum, len+1) (0.,0) fs in
  sum /. float len

let avg fs =
  let sum,len = List.fold_left (fun (sum,len) f -> f +. sum, len+1) (0.,0) fs in
  sum /. float len

let gen_f seed uniform lines file  =
  let lines =
    match uniform with
    | Uniform ->
      List.init lines (fun _ -> gen_line 0. 0. 360. 180. seed )
    | Region ->
      let gen_region () =
        let cx,cy = gen_point 0. 0. 360. 180. seed in
        let dx = min 20. (180. -. (Float.abs cx)) in
        let dy = min 20. (90. -. (Float.abs cy)) in
        cx,cy,dx,dy
      in
      let regions = List.init 1000 (fun _ -> gen_region ()) in
      let per_region = lines / 1000 in
      List.concat_map (fun (cx,cy,dx,dy) ->
          List.init per_region (fun _ -> gen_line cx cy dx dy seed)
        ) regions
  in
  let json = `List (List.map Haversine.Line.to_yojson lines) in
  let haversines = List.map Haversine.Line.haversine lines in
  let avg = avg haversines in
  Printf.printf "Avg: %f\n" avg;
  if file = "-"
  then Yojson.Safe.pretty_to_channel stdout json
  else begin
    let bytes =
      let buf = Buffer.create 0 in
      List.iter (fun f ->
          let i = Int64.bits_of_float f in
          Buffer.add_int64_ne buf i
        ) haversines;
      Buffer.add_int64_ne buf (Int64.bits_of_float avg);
      Buffer.to_bytes buf
    in
    Yojson.Safe.to_file file json;
    Out_channel.with_open_bin (file ^ ".answer") (fun oc ->
        Out_channel.output_bytes oc bytes
      )
  end

let lex_json f =
  let content = In_channel.with_open_bin f In_channel.input_all in
  print_endline content;
  let lexer = Haversine.Json.Lexer.v content in
  let rec loop lex =
    match Haversine.Json.Lexer.lex lexer with
    | Some token ->
      Format.printf "%a@ %!" Haversine.Json.Lexer.pp_token token;
      loop lex
    | None -> print_endline "EOF"
  in
  loop lexer

let parse_json f =
  let content = In_channel.with_open_bin f In_channel.input_all in
  print_endline content;
  let o = Haversine.Json.parse content in
  Format.printf "%a" Haversine.Json.pp o

let read_bin_file f points =
  let buf = Bytes.create 8 in
  let read_float ic =
    In_channel.really_input ic buf 0 8
    |> Option.map (fun () -> Bytes.get_int64_ne buf 0 |> Int64.float_of_bits)
  in
  let arr = Array.make points 0. in
  let avg = ref 0. in
  let () = In_channel.with_open_bin f (fun ic ->
      for i = 0 to points - 1 do
        Array.set arr i (read_float ic |> Option.get)
      done;
      avg := read_float ic |> Option.get
    )
  in
  arr,avg

let calc_f f validation =
  let content = In_channel.with_open_bin f In_channel.input_all in
  let json = Haversine.Json.parse content in
  match json with
  | Haversine.Json.Arr l ->
    let dists = Array.map (fun json -> Haversine.Line.of_json json |> Haversine.Line.haversine) l in
    let avg = avg_arr dists in
    Printf.printf "Input size: %d\n" (String.length content);
    Printf.printf "Pair count: %d\n" (Array.length dists);
    Printf.printf "Avg: %f\n" avg;

    Option.iter (fun f ->
        let hav, _ = read_bin_file f (Array.length dists) in
        let ref_avg = avg_arr hav in
        Printf.printf "\nReference Avg: %f\n" ref_avg;
        Printf.printf "Difference: %f\n" (avg -. ref_avg)
      ) validation
  | _ -> failwith "needs to be array"


let gen_cmd = Cmd.v (Cmd.info "gen") Term.(const gen_f $ seed $ gen_method $ lines $ file)
let json_cmd = Cmd.v (Cmd.info "json_lex") Term.(const lex_json $ in_file)
let json_parse_cmd = Cmd.v (Cmd.info "json_parse") Term.(const parse_json $ in_file)
let calc_hav_cmd = Cmd.v (Cmd.info "calc") Term.(const calc_f $ in_file $ validation)

let cmd = Cmd.group (Cmd.info "haversine") [gen_cmd; json_cmd; json_parse_cmd; calc_hav_cmd]

let () = ignore @@ Cmd.eval cmd
