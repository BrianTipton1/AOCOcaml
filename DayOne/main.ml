open AOCOCaml.ElfCalorie
open AOCOCaml.IO

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


let () =
  let lines = read_file "DayOne/input.txt" in
  elfCalories_of_strings lines |> findMaxCalories |> print_elfCalorie |> print_string
