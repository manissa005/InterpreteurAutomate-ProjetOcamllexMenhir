open Type

(*Fonction qui convertit string en liste de carac *)
let stringToListChar  s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


(*Fonction qui teste si tous les carac d'un mot (liste de carac) sont présents dans les input symbols
renvoi false dans le cas contraire *)
 let test_input input listChar =
    let bool = true in
    let c = List.hd listChar in
    while (bool) do
        if (List.mem c input) then bool = true else bool = false 
        let c = List.hd (List.tl listChar)
    done;
    bool;;  

(* Fonction qui transforme un type transitions en une liste de transition *)
let rec list_transitions transList = 
    match transList with
    | None -> []
    | Transitions(transition,transitions) ->  [transition]@ list_transitions transitions;;

(*Fonction qui transforme un type (input_symbols, stack_symbols et state) en une liste (de type lettre) *)
let rec list_symbols symbols = 
    match symbols with
    | Element(elem) -> [elem]
    | List(elem,list) -> [elem] @ list_symbols list;;

(*Fonction qui prend un char et une stack et une liste de transitions et qui renvoi la transition qui convient
ou null s'il y'en a pas *)
let rec trouveTransition state carac stackSymbol list_trans = 
    match list_trans with
    | [] -> (' ',None,' ',' ',None)
    | [s1,c,s,s2,stack] when (s1,c,s)==(state,carac,stackSymbol) -> (s1,c, s, s2, stack)
    | x::l -> trouveTransition carac stackSymbol l;;

(*Fonction qui prend un automate et qui renvoi ses transitions*)
let recup_transitions automate = 
    match automate with
    | (declarations,transitions) -> transitions
    | _ -> failwith "erreur dans le type automate"
(*Fonction qui prend un type declarations et qui renvoi une liste contenant tt les donnes de lautomate *)
let recup_declaration automate = 
    match automate with 
    | (declarations,transitions) -> 
    (match declarations with
        | (input, stack, state,init_state, init_stack) -> [input,stack,state,init_state, init_stack]
        | _ -> failwith "erreur dans le type declarations"
    )
    | _ -> failwith "erreur dans le type automate"
(*Fonction qui prend un automate et un mot et qui teste si le mot est accepté ou pas *)

let execute_automate automate mot =  
    let decla = recup_declaration automate in
    let list_transition = list_transitions (recup_transitions automate) in 
    let list_input = list_symbols ((List.nth (recup_declaration automate) 0)) in
    let list_stack = list_symbols (List.nth (recup_declaration automate) 1) in
    let list_state = list_symbols (List.nth (recup_declaration automate) 2) in
    let init_state = List.nth (recup_declaration automate) 3 in
    let init_stack = List.nth (recup_declaration automate) 4 in
    (*if (mot =="") then failwith "le mot est vide"
    else let list_mot = stringToListChar mot in
    match list_mot with
    | [] -> failwith "liste caracteres vide"
    | x::l->  on doit faire pr le char x : chercher la transiition qui faut, tester si elle renvoi pas null mettre a jour les state et stack et rappeler sur un autre char
*)

   Printf.printf "initial state: %c\n" init_state;
   Printf.printf "initial state: %c\n" init_stack;;