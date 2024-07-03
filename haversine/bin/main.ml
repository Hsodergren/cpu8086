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
  let sum,len = List.fold_left (fun (sum,len) f -> Haversine.Line.haversine f +. sum, len+1) (0.,0) lines in
  Printf.printf "Avg: %f\n" (sum /. float len);
  if file = "-"
  then Yojson.Safe.pretty_to_channel stdout json
  else Yojson.Safe.to_file file json
    
  
let gen_cmd = Cmd.v (Cmd.info "gen") Term.(const gen_f $ seed $ gen_method $ lines $ file)
let cmd = Cmd.group (Cmd.info "haversine") [gen_cmd]
let () = ignore @@ Cmd.eval cmd
