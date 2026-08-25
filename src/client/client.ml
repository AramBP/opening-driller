open Core

module Tile = Game.Tile
module Button = Helpers.Button

module State = struct
  type button_kind = Flip
  type t = {
    layout      : Tile.layout ;
    buttons     : button_kind Button.t list ;
    perspective : Game.Color.t 
  }
end

let get_tile (tile : Tile.Coord.t) (cs : State.t) : Tile.Coord.t =
  let z = match cs.perspective with White -> 0 | Black -> 7 in
  { x = abs (z - tile.x) ; y = abs (z - tile.y) }

let select_tile (mx : int) (my : int) (cs : State.t) : unit =
  let board_hover =
    (cs.layout.origin.x + cs.layout.size.x * 8) >= mx && mx >= cs.layout.origin.x &&
    (cs.layout.origin.y + cs.layout.size.y * 8) >= my && my >= cs.layout.origin.y
  in
  match board_hover with
  | false -> ()
  | true -> 
      let tile = Tile.Coord.of_pixel (Float.of_int mx) (Float.of_int my) cs.layout in
      let tile = get_tile tile cs in
      Printf.printf "Clicked : (%d, %d) \n" tile.x tile.y; Out_channel.(flush stdout)
       
let next (cs : State.t) : State.t =
  let mx, my = Raylib.(get_mouse_x (), get_mouse_y ()) in

  let buttons = List.map cs.buttons ~f:(fun button -> Button.next mx my button) in
  let perspective = 
    let res = match Raylib.(is_mouse_button_pressed MouseButton.Left) with 
    | true -> Button.on_click buttons (function Flip -> Game.Color.flip cs.perspective)
    | false -> None
    in
    match res with Some color -> color | None -> cs.perspective
  in
  
  if Raylib.(is_mouse_button_pressed MouseButton.Left) then select_tile mx my cs;

  { cs with perspective = perspective ; buttons = buttons }

let draw (cs : State.t) (gs : Game.State.t) : unit = 
  let open Raylib in

  List.iter cs.buttons ~f:Button.draw;

  (* Draw chess board *)
  let lsx, lsy = cs.layout.size.x, cs.layout.size.y in
  let tile_color = ref Game.Color.Black in
  for x = 0 to 7 do
    tile_color := Game.Color.flip !tile_color;
    for y = 0 to 7 do
      let x_px, y_px = Tile.Coord.to_pixel { x ; y } cs.layout in
      draw_rectangle x_px y_px lsx lsy (Helpers.get_color !tile_color);
      tile_color := Game.Color.flip !tile_color;
      if x = 7 then begin
        let y = abs ((match cs.perspective with White -> 7 | Black -> 0) - y) in
        draw_text_ex
          (Lazy.force Helpers.font_bold)
          String.(get (Tile.Coord.to_string { x ; y }) 1 |> of_char)
          (Vector2.create (x_px + lsx - 15 |> Float.of_int) (y_px + 7 |> Float.of_int))
          20. 2. (Helpers.get_color !tile_color);
      end;
      if y = 7 then begin
        let x = abs ((match cs.perspective with White -> 0 | Black -> 7) - x) in
        draw_text_ex
          (Lazy.force Helpers.font_bold)
          String.(get (Tile.Coord.to_string { x ; y }) 0 |> of_char)
          (Vector2.create (x_px + 5 |> Float.of_int) (y_px + lsy - 20 |> Float.of_int))
          20. 2. (Helpers.get_color !tile_color)
      end;
    done;
  done;

  (* Draw pieces *)
  List.iter gs.ents ~f:(fun (coord, ent) ->
    let coord = get_tile coord cs in
    let tex = Helpers.get_piece_tex ent in
    let x, y = Tile.Coord.to_pixel coord cs.layout in
    draw_texture tex x y Raylib.Color.white;
  ) 

let init =
  let flip_button = Button.create 800. 600. 100. 50. 0.5 "Flip" Center Helpers.gray Helpers.light_gray State.Flip in
  State.{
    layout      = Tile.{ size = { x = 80 ; y = 80 } ; origin = { x = 40 ; y = 40 } } ;
    perspective = White ;
    buttons     = [flip_button]
  }
