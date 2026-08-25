let setup () =
  let open Raylib in
  let window_width = 1280 in
  let window_height = 720 in
  let window_name = "Opening Trainer" in
  init_window window_width window_height window_name;
  set_target_fps 60;
  Client.init, Game.init

let rec loop (cs, gs : Client.State.t * Game.State.t) =
  let open Raylib in
  match window_should_close () with
  | true  -> close_window ()
  | false -> 
      let cs = Client.next cs gs in
      Raylib.begin_drawing ();
      Raylib.clear_background Color.black;
      Client.draw cs gs;
      Raylib.end_drawing ();
      loop (cs, gs)


let () = setup () |> loop
