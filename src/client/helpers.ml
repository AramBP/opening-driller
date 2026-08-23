open Core 

let load_font font_data =
  let open Raylib in
  let charset = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" in
  let count = Ctypes.allocate Ctypes.int 0 in
  let codepoints_ptr = load_codepoints charset count in
  let n = Ctypes.(!@)count in
  let codepoints = CArray.from_ptr codepoints_ptr n in
  load_font_from_memory ".ttf" font_data 32 codepoints 

let font = lazy (load_font [%blob "../../assets/fonts/Roboto-Regular.ttf"])
let font_bold = lazy (load_font [%blob "../../assets/fonts/Roboto-Bold.ttf"])

let white_square = Raylib.Color.create 240 217 181 255 
let black_square = Raylib.Color.create 181 136 99 255 

let get_color (color : Game.Color.t) : Raylib.Color.t =
  match color with White -> white_square | Black -> black_square

module Button = struct
  type state_kind = Hover | Idle
  type align_kind = Left | Center | Right
  type content_kind = { text : string ; align : align_kind ; text_color : Raylib.Color.t ; font : Raylib.Font.t ; font_size : float }
  type 'a t = {
    rect      : Raylib.Rectangle.t ;
    roundness : float ;
    state     : state_kind ;
    content   : content_kind ;
    idlec     : Raylib.Color.t ;
    hoverc    : Raylib.Color.t ;
    mode      : 'a
  }

  let next (mx : int) (my : int) (but : 'a t) : 'a t =
    let hover =
      let mx, my = Float.of_int mx, Float.of_int my in
      let x, y, width, height = Raylib.Rectangle.(x but.rect, y but.rect, width but.rect, height but.rect) in
      Float.(mx >= x && x + width >= mx && y >= my && my >= y - height)
    in
    match but.state, hover with
    | Hover, false  -> { but with state = Idle }
    | Idle, true    -> { but with state = Hover }
    | Hover, true | Idle, false  -> but
    
  let draw (but : 'a t) : unit =
    let open Raylib in
    let color = match but.state with Hover -> but.hoverc | Idle -> but.idlec in
    draw_rectangle_rounded but.rect but.roundness 12 color;
    let cont = but.content in
    let text_pos = 
      let text_width = Vector2.x (measure_text_ex cont.font cont.text cont.font_size 2.) in
      let width = Rectangle.width but.rect in
      let y = Rectangle.(height but.rect /. 4. +. y but.rect) in
      let x = match cont.align with
      | Left -> 5. +. Rectangle.x but.rect 
      | Right -> 
          let dif = width -. text_width in
          let offset = if Float.(dif > 0.) then dif else 0. in
          offset +. Rectangle.x but.rect
      | Center ->
          let dif = width -. (text_width /. 2.) in
          let offset = if Float.(dif > 0.) then dif else 0. in
          offset +. Rectangle.x but.rect
      in
      Vector2.create x y
    in 
    draw_text_ex cont.font cont.text text_pos cont.font_size 2. cont.text_color

  let create x y width height roundness text align idlec hoverc mode =
    let rect = Raylib.Rectangle.create x y width height in
    let content = { text = text ; align = align ; text_color = Raylib.Color.white ; font = Lazy.force font ; font_size = 24. } in 
    { rect = rect ; roundness = roundness ; state = Idle ; content = content ; idlec = idlec ; hoverc = hoverc ; mode = mode }

  let on_click (buttons : 'a t list) (fn : 'a -> 'b) : 'b option =
    List.fold_until buttons ~init:None ~finish:(fun acc -> acc) 
      ~f:(fun acc button -> 
        match button.state with 
        | Hover -> Stop (Some (fn button.mode))
        | Idle -> Continue acc)
end
