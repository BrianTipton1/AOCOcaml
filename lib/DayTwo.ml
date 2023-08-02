open Re
(*
RULES:
The winner of the whole tournament is the player with the highest score.
Your total score is the sum of your scores for each round.
The score for a single round is the score for the shape you selected 
(1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round 
(0 if you lost, 3 if the round was a draw, and 6 if you won).
*)

type rock = A of string | X of string [@@deriving show]
type paper = B of string | Y of string [@@deriving show]
type scissor = C of string | Z of string [@@deriving show]

type move = Rock of rock | Paper of paper | Scissor of scissor
[@@deriving show]

let move_of_str s : move =
  let upper = s |> String.uppercase_ascii in
  match upper with
  | "A" -> Rock (A upper)
  | "X" -> Rock (X upper)
  | "B" -> Paper (B upper)
  | "Y" -> Paper (Y upper)
  | "C" -> Scissor (C upper)
  | "Z" -> Scissor (Z upper)
  | _ -> raise (Failure "Unmatched moved")

let move_score move =
  match move with Rock _move -> 1 | Paper _move -> 2 | Scissor _move -> 3

let my_round_score mine theres =
  match (mine, theres) with
  | Rock _mine, Paper _theres -> 0 + move_score (Rock _mine)
  | Rock _mine, Rock _theres -> 3 + move_score (Rock _mine)
  | Rock _mine, Scissor _theres -> 6 + move_score (Rock _mine)
  | Paper _mine, Paper _theres -> 3 + move_score (Paper _mine)
  | Paper _mine, Rock _theres -> 6 + move_score (Paper _mine)
  | Paper _mine, Scissor _theres -> 0 + move_score (Paper _mine)
  | Scissor _mine, Paper _theres -> 6 + move_score (Scissor _mine)
  | Scissor _mine, Rock _theres -> 0 + move_score (Scissor _mine)
  | Scissor _mine, Scissor _theres -> 3 + move_score (Scissor _mine)

let round_of_str str_rnd =
  let split = Str.split (Str.regexp " ") str_rnd in
  match split with
  | [ x; y ] -> (move_of_str x, move_of_str y)
  | _ -> raise (Failure "Bad input data")
