open AOCOCaml.DayOne
open AOCOCaml.IO

(* Testing Input from https://adventofcode.com/2022/day/1
    let sample =
    [
      "1000";
      "2000";
      "3000";
      "";
      "4000";
      "";
      "5000";
      "6000";
      "";
      "7000";
      "8000";
      "9000";
      "";
      "10000";
    ]
*)

let () =
  let elves = read_file "DayOne/input.txt" |> elfCalories_of_strings in
  let top_elf_str = elves |> findMaxCalories |> print_elfCalorie in
  let top_3_elves = elves |> List.sort compare_elf |> List.rev |> take 3 in
  let top_3_sum_str =
    top_3_elves
    |> List.map (fun x -> x.calorieSum)
    |> List.fold_left (fun acc x -> acc + x) 0
    |> string_of_int
  in
  let top_3_str =
    top_3_elves |> List.map (fun x -> print_elfCalorie x) |> String.concat "\n"
  in
  print_string ("TOP ELF IS \n" ^ top_elf_str ^ "\n");
  print_string ("TOP 3 SUM: " ^ top_3_sum_str ^ "\n");
  print_string ("TOP 3 ELVES ARE \n" ^ top_3_str)
