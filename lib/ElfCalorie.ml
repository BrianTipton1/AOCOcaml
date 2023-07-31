type elf = { elfNumber : int; _allCalories : int list; calorieSum : int }

let findMaxCalories elfs =
  let rec getMax currMax elfs =
    match elfs with
    | x :: rest ->
        let m = if x.calorieSum > currMax.calorieSum then x else currMax in
        getMax m rest
    | _ -> currMax
  in
  getMax (List.hd elfs) elfs

let parseStringToIntOrNone x =
  match x with
  | "" -> None
  | _ -> ( try Some (int_of_string x) with _failure -> raise _failure)

let mapStrOptions = List.map parseStringToIntOrNone

let isEmpty = function
  | [] -> None
  | x :: rest when x = None -> Some rest
  | x -> Some x

let elfCalories_of_strings (xs : string list) =
  let vals = mapStrOptions xs in
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
    | None -> result
  in
  let toElf i ns =
    {
      calorieSum = List.fold_left ( + ) 0 ns;
      _allCalories = ns;
      elfNumber = i + 1;
    }
  in

  let fin = getAll [] (Some vals) in
  List.mapi (fun acc x -> toElf acc x) fin

let print_elfCalorie e =
  Printf.sprintf "Elf Number: %d\nTotal Calories: %d\n" e.elfNumber e.calorieSum
