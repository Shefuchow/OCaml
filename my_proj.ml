print_endline "Hello, World!";;

(* This is a single-line comment. *)

(* This is a
 * multi-line
 * comment.
*)

(* ----- How to compile projects? ----- *)
(* After we made our program in our fav IDE,
 * run `ocamlbuild <file_name>.native`
 * run `./<file_name>.native`
 * bytecode compiler
 * run `$ ocamlc -o <file_name> <file_name>.ml
 * run `$ ./<file_name>`
 *)

(* ----- Data Types ----- *)

let x = 1+2 ;;
(* type int *)

let square x = x *. x ;;
(* type float. *)

(1 < 2) = true ;;
(* type bool *)

'a' ;;
(* type char *)

"Hello World";;
(* type string *)

(*unit types*)

(* ----- Data Structures Series ----- *)
(* ----- Youtube Link: https://tinyurl.com/yyhothy8 ----- *)

(*list are homogenous
 * immutable
 * LIFO Last-in-first-out
 * let newlist = 'e'::list;; add element
 * x @ y;; appending lists
 * List.hd list;; Head of list
 * List.tl list;; tail is list
 * polymorphic (primitive, strings, objects, even functions)
 *)
let l = ["is"; "a"; "tale"; "told"];;
(* string list = ["is"; "a"; "tale"; "told"] *)

let l2 = "Life"::l;;
(* string list = ["Life"; "is"; "a"; "tale"; "told"] *)

let list = ['a'; 'b'; 'c'];;
let list2 = ['d'; 'e'; 'f'];;
let list3 = list@list2;;
(*list3 = ['a'; 'b'; 'c'; 'd'; 'e'; 'f']*)

(* -------- functions ------- *)
let rec has_element list element =
  match list with
    | [] -> false
    | head::tail -> if element = head then
      true
    else
      has_element tail element

let rec duplicate_list_elements list5 =
  match list5 with
  | [] -> []
  | head::tail -> head :: head :: duplicate_list_elements tail

let rec triple_list_elements = function
  | [] -> []
  | head::tail -> head :: head :: head :: duplicate_list_elements tail

let rec reverse_list list6 reverseList =
  match list6 with
  | [] -> reverseList
  | head :: tail -> reverse_list tail reverseList@[head]

let rec element_at myList index =
  match myList with
  | [] -> raise (Failure "empty list")
  | h::t ->
    if index = 0 then h
    else element_at t(index-1)

let rec element_at2 myList index =
  match myList with
  |[]                   -> raise (Failure "empty list")
  | h::_ when index=0   -> h
  | _::t                -> element_at2 t (index-1)

let rec range a b result =
  if a=b then
    result@[b]
  else
    range (a+1) b (result@[a])

let range a b =
  let rec aux a b =
    if a > b then
      []
    else
      a :: aux (a+1) b
    in
    aux a b
