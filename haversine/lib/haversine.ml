module Json = Json

module Point = struct
  type t = float * float
end

module Timing = struct
  let getosfreq = 1000000


  external gettimeofday : unit -> (int * int) = "my_gettimeofday"
  external rdtsc : unit -> int = "rdtsc" [@@noalloc]
  external add : int -> int -> int = "add"

  let ostimerfreq = 1_000_000
  let ostimer () =
    let sec,usec = gettimeofday () in
    sec*ostimerfreq + usec

  let estimate_cycles ms =
    let start = ostimer () in
    let cpustart = rdtsc () in
    let end' = ref 0 in
    let elapsed = ref 0 in
    let c = 1. /. 1000. *. float ms in
    while (float !elapsed < float getosfreq *. c) do
      end' := ostimer ();
      elapsed := !end' - start;
    done;
    let cpuend = rdtsc () in
    int_of_float @@ float (cpuend - cpustart) /. c
    
  module Prof = struct
    let profs = ref []
        
    let print tot =
      List.iter (fun (name, c) ->
          Printf.printf "  %s: %d (%.2f%%)\n" name c (float c /. float tot *. 100.)
        ) (List.rev !profs)


    let prof name f =
      let start = rdtsc () in
      let res = f () in
      let end' = rdtsc () in
      profs := (name,(end'-start))::!profs;
      res

    let prof1 name f arg = prof name (fun () -> f arg)

    let prof2 name f arg1 arg2  = prof name (fun () -> f arg1 arg2)

    let main f =
      let start = rdtsc () in
      let res = f () in
      let end' = rdtsc () in
      let cycles_p_s = estimate_cycles 10 in
      let tot_cycles = end'-start in
      Printf.printf "main took %d (%f seconds)\n" tot_cycles (float tot_cycles /. float cycles_p_s);
      print tot_cycles;
      profs := [];
      res
  end
end

module Line = struct
  type t = {
      p1: Point.t;
      p2: Point.t;
    }

  let v p1 p2 = {p1;p2}

  let to_yojson {p1=(x0,y0);p2=(x1,y1)} =
    `Assoc ["x0",`Float x0;"y0",` Float y0;"x1",`Float x1; "y1", `Float y1]

  let of_json o =
    let x0 = Json.member_exn "x0" o |> Json.float in
    let x1 = Json.member_exn "x1" o |> Json.float in
    let y0 = Json.member_exn "y0" o |> Json.float in
    let y1 = Json.member_exn "y1" o |> Json.float in
    { p1=(x0,y0);p2=(x1,y1) }


  let square x = x *. x

  let to_radians =
    let c = Float.pi /. 180. in
    fun deg -> c *. deg

  let refHaversine x0 y0 x1 y1 r =
    let dy = to_radians (y1 -. y0) in
    let dx = to_radians (x1 -. x0) in
    let y0 = to_radians y0 in
    let y1 = to_radians y1 in
    let a = Float.(square (sin (dy /. 2.)) +. cos y0 *. cos y1 *. square (sin (dx /. 2.))) in
    let c = 2. *. Float.asin (Float.sqrt a) in
    r *. c

  let haversine {p1=(x0,y0); p2=(x1,y1)} =
    refHaversine x0 y0 x1 y1 6378.1
end
