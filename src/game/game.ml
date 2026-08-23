open Core

module Tile = Tile

type kind = Pawn | Knight | Bishop | Rook | Queen | King
type move = { src : Tile.Coord.t ; dest : Tile.Coord.t }

module Color = struct
  type t = White | Black
  let flip = function White -> Black | Black -> White
  
  let equal a b =
    match (a, b) with 
    | (White, White) | (Black, Black) -> true
    | _ -> false

  let to_str = function White -> "White" | Black -> "Black"
end

module State = struct
  type over = Checkmate | Stalemate
  type state_kind = 
  | Choose_ent  
  | Choose_move of Tile.Coord.t 
  | Over of over * Color.t

  type ent = { kind : kind ; color : Color.t ; has_moved : bool }

  type t = {
    ents        : (Tile.Coord.t, ent) List.Assoc.t ;
    moves       : (Tile.Coord.t, Tile.Coord.t list) List.Assoc.t ;   
    prev_move   : (move * kind) option ;
    state       : state_kind ; 
    curr_color  : Color.t
  }
end

let is_check (gs : State.t) (color : Color.t) : bool = 
  let moves = gs.moves in
  let ents = gs.ents in
  let king_coord =     
    List.find_map ~f:(fun (coord, ent) -> 
      match ent.kind with 
      | King -> if Color.equal color ent.color then Some coord else None
      | _ -> None) 
    gs.ents |> Option.value ~default:(failwith "King not found")
  in

  List.fold_until ~init:false 
    ~f:(fun acc (coord, ent) ->
        if Color.equal ent.color color then Continue acc 
        else
          let ent_moves = List.Assoc.find_exn moves coord ~equal:Tile.Coord.equal in        
          
          if List.mem ent_moves king_coord ~equal:Tile.Coord.equal 
          then Stop true
          else Continue acc
        )
    ~finish:(fun _ -> false)
    ents

let is_mate (gs : State.t) (color : Color.t) : bool = 
  if not (is_check gs color) then false
  else 
    let moves = gs.moves in
    let ents = gs.ents in
    List.fold_until ~init:true
      ~f:(fun acc (coord, ent) -> 
        if not (Color.equal color ent.color) then Continue acc
        else 
          let ent_moves = List.Assoc.find_exn moves coord ~equal:Tile.Coord.equal in
          let replace_with_move = List.map ~f:(fun move_to -> 
            let ents = List.Assoc.remove ents coord ~equal:Tile.Coord.equal in
            { gs with ents = List.Assoc.add ents move_to ent ~equal:Tile.Coord.equal }) 
            ent_moves 
          in
          if List.for_all replace_with_move ~f:(fun gs -> is_check gs color)
          then Continue acc
          else Stop false
        )
      ~finish:(fun _ -> true)
      ents
    
let is_stalemate (gs : State.t) (color : Color.t) : bool = 
  if is_check gs color then false
  else
    let moves = gs.moves in
    let ents = gs.ents in
    List.fold_until ~init:true
      ~f:(fun acc (coord, ent) -> 
        if not (Color.equal color ent.color) then Continue acc
        else 
          let ent_moves = List.Assoc.find_exn moves coord ~equal:Tile.Coord.equal in
          let replace_with_move = List.map ~f:(fun move_to -> 
            let ents = List.Assoc.remove ents coord ~equal:Tile.Coord.equal in
            { gs with ents = List.Assoc.add ents move_to ent ~equal:Tile.Coord.equal }) 
            ent_moves 
          in
          if List.for_all replace_with_move ~f:(fun gs -> is_check gs color)
          then Continue acc
          else Stop false
        )
      ~finish:(fun _ -> true)
      ents

let get_moves (tile : Tile.Coord.t) (kind : kind) (color : Color.t) : Tile.Coord.t list list = 
  let moves = 
    match kind with
    | Pawn    -> let mult = match color with | White -> 1 | Black -> -1 in
                  [List.append 
                    [{ tile with y = tile.y + 1 * mult } ; { x = tile.x + 1 ; y = tile.y + 1 * mult } ; { x = tile.x - 1 ; y = tile.y + 1 * mult }]
                    (if (tile.y = 1 && Color.equal color White) || (tile.y = 6 && Color.equal color Black)
                    then [{ tile with y = tile.y + 2 * mult }] 
                    else [])]

    | Knight  -> [[ { x = tile.x - 2 ; y = tile.y + 1 } ; 
                  { x = tile.x - 1 ; y = tile.y + 2 } ;
                  { x = tile.x + 1 ; y = tile.y + 2 } ; 
                  { x = tile.x + 2 ; y = tile.y + 1 } ; 
                  { x = tile.x - 2 ; y = tile.y - 1 } ; 
                  { x = tile.x - 1 ; y = tile.y - 2 } ;
                  { x = tile.x + 1 ; y = tile.y - 2 } ; 
                  { x = tile.x + 2 ; y = tile.y - 1 } ]]

    | King    -> [[ { tile with x = tile.x + 1 } ; { tile with x = tile.x - 1 } ;  
                  { tile with y = tile.y + 1 } ; { tile with y = tile.y - 1 } ; 
                  { x = tile.x + 1 ; y = tile.y + 1 } ; { x = tile.x - 1 ; y = tile.x - 1 } ;
                  { x = tile.x - 1 ; y = tile.y + 1 } ; { x = tile.x + 1 ; y = tile.y - 1 } ; 
                  { tile with x = tile.x + 2 } ; { tile with x = tile.x - 2 }]]

    | Bishop  -> [(List.init 7 ~f:(fun i -> Tile.{ x = tile.x + i + 1 ; y = tile.y + i + 1 })) ;
                  (List.init 7 ~f:(fun i -> Tile.{ x = tile.x - i - 1 ; y = tile.y - i - 1 }))]

    | Rook    -> [(List.init 7 ~f:(fun i -> { tile with x = tile.x + i + 1 })) ;
                  (List.init 7 ~f:(fun i -> { tile with x = tile.x - i - 1 })) ;       
                  (List.init 7 ~f:(fun i -> { tile with y = tile.y + i + 1 })) ;
                  (List.init 7 ~f:(fun i -> { tile with y = tile.y - i - 1 }))]
    
    | Queen   -> [(List.init 7 ~f:(fun i -> { tile with x = tile.x + i + 1 })) ;
                  (List.init 7 ~f:(fun i -> { tile with x = tile.x - i - 1 })) ;       
                  (List.init 7 ~f:(fun i -> { tile with y = tile.y + i + 1 })) ;
                  (List.init 7 ~f:(fun i -> { tile with y = tile.y - i - 1 })) ;
                  (List.init 7 ~f:(fun i -> Tile.{ x = tile.x + i + 1 ; y = tile.y + i + 1 })) ;
                  (List.init 7 ~f:(fun i -> Tile.{ x = tile.x - i - 1 ; y = tile.y - i - 1 }))]
in  
moves |> List.(map ~f:(filter ~f:Tile.Coord.in_board))

(* get all possible moves including king capture and putting yourself in check *)
let pseudo_legal_moves (src : Tile.Coord.t) (ents : (Tile.Coord.t, State.ent) List.Assoc.t) : Tile.Coord.t list =  
  let eq = Tile.Coord.equal in
  let ent = List.Assoc.find_exn ents src ~equal:eq in
  let kind = ent.kind in
  let color = ent.color in

  let rec aux acc = function
  | x::xs -> 
      (match List.Assoc.find ents x ~equal:eq with
      | Some ent -> 
          if Color.equal color ent.color then acc
          else (x::acc)
      | None -> aux (x::acc) xs)
  | [] -> acc
  in
  let moves = get_moves src kind color in 
  match kind with
  | Pawn -> List.(filter 
    ~f:(fun dest -> 
        if src.x = dest.x then (match List.Assoc.find ents dest ~equal:eq with
        | Some _ -> false
        | None -> 
            if Int.abs(src.y - dest.y) = 1 then true
            else
              let dest = Tile.{ x = src.x ; y = src.y + (match color with White -> 1 | Black -> -1) } in
              Tile.Coord.in_board dest && not (List.Assoc.mem ents dest ~equal:eq) 
            )
        else 
          (match List.Assoc.find ents dest ~equal:eq with
          | Some ent -> not (Color.equal color ent.color)
          | None -> 
              (* Check for possible en-passant  *)
              let dest = Tile.{ x = dest.x ; y = src.y } in
              (match List.Assoc.find ents dest ~equal:eq with
              | Some ent -> not (Color.equal color ent.color)
              | None -> false))
          ) 
    (join moves))
  | King -> List.(filter
    ~f:(fun dest ->
    if Int.abs (src.x - dest.x) = 1 then 
      (match List.Assoc.find ents dest ~equal:eq with
      | Some ent -> not (Color.equal color ent.color)
      | None -> true)
    else 
      (* Check for possible castle *)
      let y = match color with White -> 0 | Black -> 7 in
      if dest.x = 2 && src.y = y && dest.y = y then
        List.Assoc.(not (mem ents { x = 1 ; y = y} ~equal:eq) && not (mem ents { x = 2 ; y = y } ~equal:eq) && not (mem ents { x = 3 ; y = y } ~equal:eq))
      else if dest.x = 6 && src.y = y && dest.y = y then
        List.Assoc.(not (mem ents { x = 5 ; y = y } ~equal:eq) && not (mem ents { x = 6 ; y = y } ~equal:eq))
      else false
    )
    (join moves))
  | Knight -> List.(filter 
    ~f:(fun dest -> match List.Assoc.find ents dest ~equal:eq with
        | Some ent -> not (Color.equal color ent.color)
        | None -> true)
    (join moves))
  | Bishop | Rook | Queen -> List.(join (map ~f:(fun l -> aux [] l) moves))

let make_move (move : move) (gs : State.t) : State.t = 
  let eq = Tile.Coord.equal in
  let ents, prev_move, curr_color = gs.ents, gs.prev_move, gs.curr_color in

  let move_ent (src : Tile.Coord.t) (dest : Tile.Coord.t) (ent : State.ent) (ents : (Tile.Coord.t, State.ent) List.Assoc.t) =
    let ents = List.Assoc.remove ents src ~equal:eq in
    let ents = List.Assoc.add ents dest State.{ ent with has_moved = true } ~equal:eq in
    if not (is_check { gs with ents = ents } curr_color) then Some ents else None
  in  

  (* Update ents using move returns None if the move isnt legal *)
  let update_ents (ents : (Tile.Coord.t, State.ent) List.Assoc.t) (move : move) = 
    let src, dest = move.src, move.dest in
    match List.Assoc.find ents src ~equal:eq with
      | None -> None
      | Some ent ->
        let dif_x = src.x - dest.x in
        let dif_y = Int.abs (src.y - dest.y) in

        (match ent.kind with 
        | Pawn ->
            if dif_x = 0 then 
              let ents = move_ent src dest ent ents in 
              if Option.is_some ents && (if dif_y = 2 then not ent.has_moved else true) then ents else None 
            else
              (match List.Assoc.find ents dest ~equal:eq with
              | Some _ -> move_ent src dest ent ents
              | None -> 
                  let coord_adj = Tile.{ x = dest.x ; y = src.y } in
                  (match List.Assoc.find ents coord_adj ~equal:eq with
                  | Some { kind = Pawn ; color = _ ; has_moved = true } ->
                      if Option.is_none prev_move then None
                      else
                        let { src = prev_src ; dest = prev_dest }, kind = Option.value_exn prev_move in
                        let is_pawn = match kind with Pawn -> true | _ -> false in
                        if (eq coord_adj prev_dest) && Int.abs (prev_src.y - prev_dest.y) = 2 && is_pawn then
                          let ents = List.Assoc.remove ents coord_adj ~equal:eq in
                          move_ent src dest ent ents
                        else None
                  | _ -> None
                  )
              )
        | King -> 
          let x, mult = if dif_x < 0 then (0, 1) else (7, -1) in
          let y = match curr_color with White -> 0 | Black -> 6 in
          if dif_x = 0 || dif_x = 1 then move_ent src dest ent ents
          else if ent.has_moved || not (dest.y = y) then None
          else
            (match List.Assoc.find ents Tile.{ x = x ; y = y } ~equal:eq with 
            | Some { kind = Rook ; color = _ ; has_moved = false } ->
                if List.for_all [move_ent src dest ent ents ; move_ent src Tile.{ x = dest.x + mult ; y = y } ent ents] ~f:Option.is_some
                then
                  let ents = List.Assoc.remove ents src ~equal:eq in
                  let ents = List.Assoc.add ents dest ent ~equal:eq in
                  move_ent Tile.{ x = x ; y = y } Tile.{ x = x + 3 * mult ; y = y } { kind = Rook ; color = curr_color ; has_moved = true } ents 
                else 
                  None
            | _ -> None
            ) 
        | _  -> move_ent src dest ent ents 
      ) 
  in

  match update_ents ents move with
  | None -> { gs with state = Choose_ent }
  | Some ents' ->
    let moves = 
      let pseudo_legal_moves = List.map ~f:(fun (coord, _) -> (coord, pseudo_legal_moves coord ents')) ents' in 
      List.map ~f:(fun (coord, moves) -> 
        (coord, List.filter ~f:(fun move -> Option.is_some (update_ents ents' { src = coord ; dest = move})) moves)) 
        pseudo_legal_moves
    in
   
    let ent = List.Assoc.find_exn ents move.src ~equal:eq in
    { ents = ents ; moves = moves ; prev_move = Some (move, ent.kind) ; state = Choose_ent ; curr_color = Color.flip curr_color }
  
let next (coord : Tile.Coord.t) (gs : State.t) =
  match gs.state with
  | Choose_ent -> 
      (match List.Assoc.find gs.ents coord ~equal:Tile.Coord.equal with
      | Some ent -> 
          if Color.equal ent.color gs.curr_color then { gs with state = Choose_move coord } else gs
      | None -> gs)
  | Choose_move coord' -> 
      if Tile.Coord.equal coord coord' then gs
      else
        let gs = 
          (match List.Assoc.find gs.ents coord ~equal:Tile.Coord.equal with
          | Some ent ->
              if Color.equal ent.color gs.curr_color then gs else make_move { src = coord' ; dest = coord } gs
          | None -> make_move { src = coord' ; dest = coord } gs)
        in
        if is_mate gs gs.curr_color then { gs with state = Over (Checkmate, gs.curr_color) }
        else if is_stalemate gs gs.curr_color then { gs with state = Over (Stalemate, gs.curr_color) }
        else gs
  | Over _ -> gs 

let init = 
  let ents = [
    (Tile.{ x = 0 ; y = 0 }, State.{ kind = Rook ; color = Color.White ; has_moved = false }) ; 
    (Tile.{ x = 7 ; y = 0 }, State.{ kind = Rook ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 1 ; y = 0 }, State.{ kind = Knight ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 6 ; y = 0 }, State.{ kind = Knight ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 2 ; y = 0 }, State.{ kind = Bishop ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 5 ; y = 0 }, State.{ kind = Bishop ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 3 ; y = 0 }, State.{ kind = Queen ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 4 ; y = 0 }, State.{ kind = King ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 0 ; y = 7 }, State.{ kind = Rook ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 7 ; y = 7 }, State.{ kind = Rook ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 1 ; y = 7 }, State.{ kind = Knight ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 6 ; y = 7 }, State.{ kind = Knight ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 2 ; y = 7 }, State.{ kind = Bishop ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 5 ; y = 7 }, State.{ kind = Bishop ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 3 ; y = 7 }, State.{ kind = Queen ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 4 ; y = 7 }, State.{ kind = King ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 0 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 7 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 1 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 6 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 2 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 5 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 3 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 4 ; y = 1 }, State.{ kind = Pawn ; color = Color.White ; has_moved = false }) ;
    (Tile.{ x = 0 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 7 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 1 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 6 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 2 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 5 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 3 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false }) ;
    (Tile.{ x = 4 ; y = 6 }, State.{ kind = Pawn ; color = Color.Black ; has_moved = false })      
  ]
  in

  let moves = List.map ~f:(fun (coord, _) -> (coord, pseudo_legal_moves coord ents)) ents in
  State.{ 
    ents        = ents ; 
    prev_move   = None ;
    moves       = moves ; 
    state       = Choose_ent ; 
    curr_color  = White 
  }
