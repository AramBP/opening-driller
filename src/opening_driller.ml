let setup () =
  let open Raylib in
  let window_width = 1280 in
  let window_height = 720 in
  let window_name = "Opening Trainer" in
  init_window window_width window_height window_name;
  set_target_fps 60;
  Client.init

let rec loop (cs : Client.State.t) =
  let open Raylib in
  match window_should_close () with
  | true  -> close_window ()
  | false -> 
      let cs = Client.next cs in
      Raylib.begin_drawing ();
      Raylib.clear_background Color.black;
      Client.draw cs;
      Raylib.end_drawing ();
      loop cs


let () = setup () |> loop
