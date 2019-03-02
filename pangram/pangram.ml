(*Helper function to see if any single string is a panagram*)
let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'];;
let rec alphabetIteration alphabet str =
  match alphabet with
  | [] -> true
  | h::t -> if String.contains str h then alphabetIteration t str else false;;

let pangram (inFile : string) (outFile : string) : unit =

  (* Here we open an input channel for first argument, inFile,
     and bind it to a variable ic so that we can refer it
     later in loop_read function. *)
  let ic = open_in inFile in

  (* Use the second argument as file name to open an output channel,
     and bind it to variable oc for later reference. *)
  let oc = open_out outFile in

  (* Helper function: file input function. It reads file line by line
     and return the result as a list of string.  *)
  let rec loop_read acc =
      (* We use try with to catch the End_of_file exception. *)
      try
          (* Read a line from ic. Build a new list with l::acc
             and pass to next recursive call. *)
          let l = input_line ic in loop_read (l::acc)
      with
        (* At the end of file, we will reverse the string since
           the list is building by keeping add new element at the
           head of old list. *)
      | End_of_file -> List.rev acc in

  (* Helper function: file output function. It takes a bool value and
     write it to the output file. *)
  let file_write bool_val = Printf.fprintf oc "%b\n" bool_val in

  (* This variable contains the result of input file from helper
     function, loop_read. Please remember this is a list of string. *)



  (*Hi PIazza*)
  let ls_str = loop_read [] in
  let rec function2 (listString : string list) : unit =
      match listString with
      | [] -> ()
      | headString::tail -> if alphabetIteration alphabet headString then file_write true else file_write false; function2 tail
in function2 ls_str




(* Do not include this in your submission but use this line to test your code *)
(*pangram "input.txt" "output.txt"*)
