let player_x = 1
let player_o = -1
let no_player = 0
let board_size = 9

let rec board_new size fill_value =
  match size with 0 -> [] | x -> fill_value :: board_new (x - 1) fill_value

let convert_to_player i =
  match i with
  | _ when i = player_x -> "X"
  | _ when i = player_o -> "O"
  | _ -> "-"

let board_print board =
  let out =
    String.concat ""
      (List.mapi
         (fun i el ->
           if (i + 1) mod 3 != 0 then Printf.sprintf "%s | " (convert_to_player el)
           else Printf.sprintf "%s\n" (convert_to_player el))
         board)
  in
  Printf.printf "Board: \n%s\n" out

let board_move (board : int list) (move : int) (player : int) : int list =
  List.mapi (fun i el -> if i = move then player else el) board

let get_user_move player =
  let move = ref board_size in
  while !move = board_size do
    let player_str = if player == player_o then "O" else "X" in
    Printf.printf "Enter move (player %s): \n" player_str;
    let user_move = read_int_opt () in
    match user_move with
    | Some x when x >= 0 -> move := x
    | _ ->
        Printf.printf "Invalid move. Move should be an int in range [0; %d)\n%!"
          board_size
  done;
  !move

let make_user_move board player =
  let user_move = get_user_move player in
  board_move board user_move player

let next_player player = player * -1

let board_winner board =
  (* TODO: Finish this *)
  ignore board;
  Some player_x

let () =
  let board = ref [] in
  board := board_new 9 no_player;
  board_print !board;
  print_newline ();

  let player = ref player_x in
  let quit_loop = ref false in
  while not !quit_loop do
    board := make_user_move !board !player;
    player := next_player !player;
    board_print !board;

    match board_winner !board with
    | Some x ->
        Printf.printf "Player %s won!\n" (convert_to_player x);
        quit_loop := true
    | _ -> ()
  done
