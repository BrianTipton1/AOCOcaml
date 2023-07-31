open Re

(* let sample = *)
(*   [ *)
(*     "1000"; *)
(*     "2000"; *)
(*     "3000"; *)
(*     ""; *)
(*     "4000"; *)
(*     ""; *)
(*     "5000"; *)
(*     "6000"; *)
(*     ""; *)
(*     "7000"; *)
(*     "8000"; *)
(*     "9000"; *)
(*     ""; *)
(*     "10000"; *)
(*   ] *)

let read_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Str.split (Str.regexp "\n") content

type elf = { num : int; itemCosts : int list; costSum : int } [@@deriving show]

let parseString x = try Some (int_of_string x) with _failure -> None
let mapVals = List.map parseString

let isEmpty = function
  | [] -> None
  | x :: rest when x = None -> Some rest
  | x -> Some x

let elves (xs : string list) =
  let vals = mapVals xs in
  let rec getNext result remain =
    match remain with
    | Some x :: rest -> getNext (List.append result [ x ]) rest
    | _ -> (result, isEmpty remain)
  in
  let rec getAll result remain =
    match remain with
    | Some x ->
        let newres, newremain = getNext [] x in
        getAll (List.append result [ newres ]) newremain
    | None -> (result, remain)
  in
  let toElf ns i =
    { costSum = List.fold_left ( + ) 0 ns; itemCosts = ns; num = i + 1 }
  in

  let fin, _ = getAll [] (Some vals) in
  List.mapi (fun acc x -> toElf x acc) fin

let findElf elfs =
  let rec getMax currMax elfs =
    match elfs with
    | x :: rest ->
        let m = if x.costSum > currMax.costSum then x else currMax in
        getMax m rest
    | _ -> currMax
  in
  getMax { num = 0; itemCosts = []; costSum = 0 } elfs

let () =
  let lines = read_file "input.txt" in
  elves lines |> findElf |> show_elf |> print_string
