open Core 

let load_font font_data =
  let open Raylib in
  let charset = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" in
  let count = Ctypes.allocate Ctypes.int 0 in
  let codepoints_ptr = load_codepoints charset count in
  let n = Ctypes.(!@)count in
  let codepoints = CArray.from_ptr codepoints_ptr n in
  load_font_from_memory ".ttf" font_data 32 codepoints 

let font_regular = lazy (load_font [%blob "../../assets/fonts/Roboto-Regular.ttf"])
let font_bold = lazy (load_font [%blob "../../assets/fonts/Roboto-Bold.ttf"])

let load_i s = 
  let im = Raylib.load_image_from_memory ".png" s (String.length s) in
  let im_ptr = Ctypes.allocate Raylib.Image.t im in 
  Raylib.(image_resize im_ptr 80 80);
  Ctypes.(!@) im_ptr

let load s = 
  let im = load_i s in
  let tex = Raylib.load_texture_from_image im in
  Raylib.unload_image im;
  tex

let kingb = lazy (load [%blob "../../assets/pieces/Chess_kdt60.png"])
let kingw = lazy (load [%blob "../../assets/pieces/Chess_klt60.png"])
let queenb = lazy (load [%blob "../../assets/pieces/Chess_qdt60.png"])
let queenw = lazy (load [%blob "../../assets/pieces/Chess_qlt60.png"])
let bishopb = lazy (load [%blob "../../assets/pieces/Chess_bdt60.png"])
let bishopw = lazy (load [%blob "../../assets/pieces/Chess_blt60.png"])
let knightb = lazy (load [%blob "../../assets/pieces/Chess_ndt60.png"])
let knightw = lazy (load [%blob "../../assets/pieces/Chess_nlt60.png"])
let rookb = lazy (load [%blob "../../assets/pieces/Chess_rdt60.png"])
let rookw = lazy (load [%blob "../../assets/pieces/Chess_rlt60.png"])
let pawnb = lazy (load [%blob "../../assets/pieces/Chess_pdt60.png"])
let pawnw = lazy (load [%blob "../../assets/pieces/Chess_plt60.png"]) 

let get_piece_tex (piece : Game.piece) =
  let lazy_tex = 
    match piece with
    | King, White -> kingw | King, Black -> kingb
    | Queen, White -> queenw | Queen, Black -> queenb
    | Bishop, White -> bishopw | Bishop, Black -> bishopb
    | Knight, White -> knightw | Knight, Black -> knightb
    | Rook, White -> rookw | Rook, Black -> rookb
    | Pawn, White -> pawnw | Pawn, Black -> pawnb
  in
  Lazy.force lazy_tex

let light_gray = Raylib.Color.create 51 49 46 255
let gray = Raylib.Color.create 38 36 33 255
let white_square = Raylib.Color.create 240 217 181 255 
let black_square = Raylib.Color.create 181 136 99 255 
let transparent_dark_green = Raylib.Color.(create (r darkgreen) (g darkgreen) (b darkgreen) 150)

module Button = struct
  type state_kind = Hover | Idle
  type align_kind = Left | Center | Right
  type content_kind = { text : string ; align : align_kind ; font : Raylib.Font.t Lazy.t; text_color : Raylib.Color.t ; font_size : float }
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
      Float.(mx >= x && x + width >= mx && y + height >= my && my >= y)
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
      let text_width = measure_text cont.text (Int.of_float cont.font_size) |> Float.of_int in
      let width = Rectangle.width but.rect in
      let y = Rectangle.(height but.rect /. 2. +. y but.rect -. cont.font_size /. 2.) in
      let x = match cont.align with
      | Left -> 5. +. Rectangle.x but.rect 
      | Right -> 
          let dif = width -. text_width in
          let offset = if Float.(dif > 0.) then dif else 0. in
          offset +. Rectangle.x but.rect
      | Center ->
          let dif = (width -. text_width) /. 2. in
          let offset = if Float.(dif > 0.) then dif else 0. in
          offset +. Rectangle.x but.rect
      in
      Vector2.create x y
    in 
    draw_text_ex (Lazy.force cont.font) cont.text text_pos cont.font_size 2. cont.text_color

  let create x y width height roundness text align idlec hoverc mode =
    let rect = Raylib.Rectangle.create x y width height in
    let content = { text ; align ; font = font_regular ; text_color = Raylib.Color.white ; font_size = 24. } in 
    { rect ; roundness ; state = Idle ; content ; idlec ; hoverc ; mode }

  let on_click (buttons : 'a t list) (fn : 'a -> 'b) : 'b option =
    List.fold_until buttons ~init:None ~finish:(fun acc -> acc) 
      ~f:(fun acc button -> 
        match button.state with 
        | Hover -> Stop (Some (fn button.mode))
        | Idle -> Continue acc)
end

module type ScrollItem = sig
  type content
  type t = { content : content ; width : int ; height : int}
  val draw : t -> int -> int -> unit
end

module ScrollableBox (Item : ScrollItem) = struct 
  type t = {
    pos_x : int ;
    pos_y : int ; (* Position of the top left corner of the box *)
    items : Item.t List.t ;
    render_tex : Raylib.RenderTexture.t ;
    background_color : Raylib.Color.t ;
    scroll : float ; (* Specifies the scroll position in the box : 0.0 -> top of the box and 1.0 -> bottom of the box*)
    bottom_line : int ;
    top_line : int
  }

  let get_dim (scroll_box : t) =
    let tex = Raylib.RenderTexture.texture scroll_box.render_tex in
    Raylib.Texture.(width tex, height tex)

  let add_item (scroll_box : t) (item : Item.t) : t =
    {scroll_box with items = List.append scroll_box.items [item] }
    
  let update (scroll_box : t) : t =
    let clamp x mi ma = Float.(max mi (min ma x)) in
    let mx, my = Raylib.(get_mouse_x (), get_mouse_y ()) in
    let width, height = get_dim scroll_box in
    
    let hover =
      scroll_box.pos_x + width >= mx && mx >= scroll_box.pos_x &&
      scroll_box.pos_y + height >= my && my >= scroll_box.pos_y
    in
    
    let total_height =
      List.fold_left scroll_box.items ~init:0 ~f:(fun acc item ->
          acc + item.Item.height)
    in  

    let max_scroll = max 0 (total_height - height) in
    let px_step = 30.0 in
    let scroll =
      if hover && max_scroll > 0 then
        let wheel = Raylib.get_mouse_wheel_move () in
        let current_px = scroll_box.scroll *. Float.of_int max_scroll in
        let new_px = clamp (current_px -. (wheel *. px_step)) 0.0 (Float.of_int max_scroll) in
        new_px /. Float.of_int max_scroll
      else
        scroll_box.scroll
    in
    let top_line = Float.to_int (scroll *. Float.of_int max_scroll) in
    let bottom_line = top_line + height in
    { scroll_box with scroll ; top_line ; bottom_line }


  let draw (scroll_box : t) : unit =
    let open Raylib in
    let width, height = get_dim scroll_box in
    begin_texture_mode scroll_box.render_tex;
    clear_background scroll_box.background_color;
    
    let y_offset = ref 0 in
    List.iter scroll_box.items ~f:(fun item ->
      let item_top = !y_offset in
      let item_bottom = !y_offset + item.height in
      if item_bottom >= scroll_box.top_line && item_top <= scroll_box.bottom_line then
        Item.draw item 0 (item_top - scroll_box.top_line);
      y_offset := item_bottom
    );
    end_texture_mode ();

    draw_texture_rec
      (RenderTexture.texture scroll_box.render_tex)
      (Rectangle.create 0.0 0.0 (Float.of_int width) (-1. *. Float.of_int height))
      (Vector2.create (Float.of_int scroll_box.pos_x) (Float.of_int scroll_box.pos_y))
      Color.white

  let init (pos_x : int) (pos_y : int) (width : int) (height : int) (background_color : Raylib.Color.t): t =
    let render_tex = Raylib.load_render_texture width height in
    { pos_x = pos_x ; pos_y = pos_y ; items = [] ; render_tex = render_tex ; background_color = background_color ; scroll = 0.0 ; bottom_line = 0 ; top_line = 0 }
end
