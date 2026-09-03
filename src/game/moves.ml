open Core
open Board

type t = (piece, (int List.t Array.t)) List.Assoc.t Array.t 

let directions = [
  (1, 0) ; (1, 1) ; (0, 1) ; (-1, 1) ; (* lines *)
  (-1, 0) ; (-1, -1) ; (0, -1) ; (1, -1) ;
  (2, 1) ; (1, 2) ; (-1, 2) ; (-2, 1) ;  (* knights *)
  (-2, -1) ; (-1, -2) ; (1, -2) ; (2, -1)]

let round x = (x *. 100. |> Int.of_float |> Float.of_int) /. 100.
let normalize (a, b) = 
  let a, b = Float.(of_int a, of_int b) in
  let d = Float.(sqrt (a**2. + b**2.)) in
  (round (a /. d), round (b /. d))

let rays = List.map directions ~f:normalize

let is_legal y dx dy = function
  | King, _ -> abs dx <= 1 && abs dy <= 1
  | Queen, _ -> dx = 0 || dy = 0 || dx = dy
  | Rook, _ -> dx = 0 || dy = 0
  | Knight, _ -> (abs dx = 2 && abs dy = 1) || (abs dx = 1 && abs dy = 2)
  | Bishop, _ -> abs dx = abs dy 
  | Pawn, Black -> (y < 8 && abs dx <= 1 && dy = -1)
  | Pawn, White -> (y > 1 && abs dx <= 1 && dy = 1)

let moves = 
  let tuplef_eq (a, b) (c, d) = Float.equal a c && Float.equal b d in
  let pieces =  
    [ (Pawn, White) ; (Pawn, Black) ; (Knight, White) ; (Knight, Black) ;
    (Bishop, White) ; (Bishop, Black) ; (Rook, White) ; (Rook, Black) ;
    (Queen, White) ; (Queen, Black) ; (King, White) ; (King, Black) ] 
  in

  let move_arr : t = Array.init 64 ~f:(
    fun idx -> List.map pieces ~f:(fun piece -> 
      let rank = idx % 8 in
      let row = 8 - idx / 8 in
      let legal_moves = Array.init 8 ~f:(fun _ -> []) in
      List.(
        init 64 ~f:(fun x -> x) |> 
        sort ~compare:(fun x y -> -1 * Int.compare (abs (x - idx)) (abs (y - idx))) |>
        iter ~f:(fun i -> 
          let dx = (i % 8) - rank in
          let dy = (8 - i / 8) - row in
          if i = idx || not (is_legal row dx dy piece) then 
            ()
          else
            let ray = normalize (dx, dy) in 
            if List.mem rays ray ~equal:tuplef_eq then
              (* Mod by 8 to shift the ray index down by 8 from the index 
                 found in directions. Moves for other pieces will be unchanged*)
              let ray_idx = List.find_mapi_exn rays ~f:(fun i ray' -> if tuplef_eq ray ray' then Some i else None) in
              let ray_idx = ray_idx mod 8 in
              legal_moves.(ray_idx) <- i::(legal_moves.(ray_idx))
            else
              ()
      ));
      let legal_moves = Array.filter legal_moves ~f:(function [] -> false | _ -> true) in 
      
      (* Add castling for kings *)
      if idx = 4 && equal_piece (King, White) piece then begin
        legal_moves.(0) <- (6::(legal_moves.(0)));
        legal_moves.(4) <- (2::(legal_moves.(4)))
      end;
      
      if idx = 60 && equal_piece(King, Black) piece then begin
        legal_moves.(0) <- (62::(legal_moves.(0)));
        legal_moves.(4) <- (58::(legal_moves.(4)))
      end;
      (piece, legal_moves)
    ))
  in

  move_arr


