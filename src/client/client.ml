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

let next (cs : State.t) : State.t =
  let mx, my = Raylib.(get_mouse_x (), get_mouse_y ()) in
  let buttons = List.map cs.buttons ~f:(fun button -> Button.next mx my button) in
  let res = 
    match Raylib.(is_mouse_button_pressed MouseButton.Left) with 
    | true -> Button.on_click buttons (function Flip -> Game.Color.flip cs.perspective)
    | false -> None
  in
  let perspective = match res with Some color -> color | None -> cs.perspective in
  { cs with perspective = perspective ; buttons = buttons }

let draw (cs : State.t) : unit = 
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
          (Vector2.create (x_px + lsx - 20 |> Float.of_int) (y_px + 10 |> Float.of_int))
          24. 2. (Helpers.get_color !tile_color);
      end;
      if y = 7 then begin
        let x = abs ((match cs.perspective with White -> 0 | Black -> 7) - x) in
        draw_text_ex
          (Lazy.force Helpers.font_bold)
          String.(get (Tile.Coord.to_string { x ; y }) 0 |> of_char)
          (Vector2.create (x_px + 10 |> Float.of_int) (y_px + lsy - 30 |> Float.of_int))
          24. 2. (Helpers.get_color !tile_color)
      end;
    done;
  done

let init =
  let flip_button = Button.create 800. 600. 100. 50. 0.5 "Flip" Center Helpers.gray Helpers.light_gray State.Flip in
  State.{
    layout      = Tile.{ size = { x = 80 ; y = 80 } ; origin = { x = 40 ; y = 40 } } ;
    perspective = White ;
    buttons     = [flip_button]
  }
