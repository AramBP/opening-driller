open Core

module Tile = Game.Tile

module State = struct
  type screen = Main

  type t = {
    layout      : Tile.layout ;
    perspective : Game.Color.t ;
    screen      : screen
  }
end

  let draw (cs : State.t) : unit = 
  let open Raylib in
  
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
        let y' = abs ((match cs.perspective with White -> 7 | Black -> 0) - y) in
        draw_text_ex
          (Lazy.force Helpers.font_bold)
          String.(get (Tile.Coord.to_string { x = x ; y = y' }) 1 |> of_char)
          (Vector2.create (x_px + lsx - 20 |> Float.of_int) (y_px + 10 |> Float.of_int))
          24. 2. (Helpers.get_color !tile_color);
      end;
      if y = 7 then begin
        let x' = abs ((match cs.perspective with White -> 0 | Black -> 7) - x) in
        draw_text_ex
          (Lazy.force Helpers.font_bold)
          String.(get (Tile.Coord.to_string { x = x' ; y = y }) 0 |> of_char)
          (Vector2.create (x_px + 10 |> Float.of_int) (y_px + lsy - 30 |> Float.of_int))
          24. 2. (Helpers.get_color !tile_color)
      end;
    done;
  done

let init =
  State.{
    layout      = Tile.{ size = { x = 80 ; y = 80 } ; origin = { x = 40 ; y = 40 } } ;
    perspective = White ;
    screen      = Main
  }
