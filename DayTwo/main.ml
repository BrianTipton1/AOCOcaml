open AOCOCaml.DayTwo
open AOCOCaml.IO

let _sample = [ "A Y"; "B X"; "C Z" ]

let () =
  List.map (fun x -> round_of_str x) (read_file "DayTwo/input.txt")
  |> List.map (fun (theres, mine) -> my_round_score mine theres)
  |> List.fold_left (fun acc x -> acc + x) 0
  |> print_int
