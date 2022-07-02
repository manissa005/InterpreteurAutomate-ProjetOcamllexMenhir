(*Declaration des types *)
open Str
exception Error of string

(* let R = Str.regexp ( "[0-9a-zA-Z]" ) *)
type lettre = char

(*type initialStack = lettre
type initialState = lettre*)

(*type lettre ou vide*)
type lettreouvide = 
    | Lettre of lettre
    | None 


(*type de la liste des lettre utilisé pour input_symbols, stack_symbols et state *)
type listeLettre = 
    | Element of lettre
    | List of lettre * listeLettre


(*type des inputs symbols qui représente les terminaux*)
(*type input_symbols = listeLettre

(*type des stack symbols qui representent les non terminaux*)
type stack_symbols  = listeLettre

(*type des states qui representent les états de l'automate*)
type state = listeLettre*)

(*type pile non vide*)
(*type stackNonVide = 
    | Lettre1 of lettre
    | StackNonVide of lettre * stackNonVide*)

(*type stackNonVide = lettre list*)

(*type stack qui represente lapile des non terminaux*)
type stack = Stack of
     (lettre list)
    | None1

(*type d'une seule transition*)
type transition = lettre * lettreouvide * lettre * lettre * stack

(*type transitions qui represente toutes les transitions de l'automate*)
type transitions =
    | Transitions of transition * transitions
    | None2
(*type des declarations de l'automate*)
type declarations = listeLettre * listeLettre * listeLettre * lettre * lettre

(*type automate*)
type automate = declarations * transitions








(*-----------------------------------------------------------------------------------------------------------------------*)











(*Fonction qui convertit string en liste de carac *)
let stringToListChar  (s:string):lettreouvide list =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (Lettre(s.[i]) :: l) in
    exp (String.length s - 1) [];;
  
  
  (*Fonction qui teste si tous les carac d'un mot (liste de carac) sont présents dans les input symbols
  renvoi false dans le cas contraire *)
  let test_input input listChar = 
    let f a = List.mem a input in 
    List.for_all f listChar ;;
  
  (* Fonction qui transforme un type transitions en une liste de transition *)
  let rec list_transitions transList = 
      match transList with
      | None2 -> []
      | Transitions(transition,transitions) ->  [transition]@ list_transitions transitions;;
  
  (*Fonction qui transforme un type (input_symbols, stack_symbols et state) en une liste (de type lettre) *)
  let rec list_symbols symbols = 
      match symbols with
      | Element(elem) -> [elem]
      | List(elem,list) -> [elem] @ list_symbols list;;
  
  (*Fonction qui prend un char et une stack et une liste de transitions et qui renvoi la transition qui convient
  ou null s'il y'en a pas *)
  let rec trouveTransition (state:lettre) (carac) (stackSymbol:lettre) (list_trans:transition list) = 
    match list_trans with
    | [] -> false
    | (s1,c,s,s2,stack)::l -> match c with
                                |None-> trouveTransition state carac stackSymbol l
                                |Lettre(x)-> if(s1==state && x==carac && s==stackSymbol) then true else trouveTransition state carac stackSymbol l;;

    let rec trouveTransition1 state carac stackSymbol list_trans = 
    match list_trans with
     | (s1,c,s,s2,stack)::l -> match c with
                                |None-> trouveTransition1 state carac stackSymbol l
                                |Lettre(x)-> if(s1==state && x==carac && s==stackSymbol) then (s1,c,s,s2,stack) else trouveTransition1 state carac stackSymbol l;;


    let rec trouveTransition2 (state:lettre) (stackSymbol:lettre) (list_trans:transition list) = 
        match list_trans with
        | [] -> false
        | (s1,c,s,s2,stack)::l when s1==state && c==None && s==stackSymbol-> true
        | (s1,c,s,s2,stack)::l -> trouveTransition2 state stackSymbol l ;;
    
    let rec trouveTransition3 (state:lettre) (stackSymbol:lettre) (list_trans:transition list) = 
        match list_trans with
        | (s1,c,s,s2,stack)::l when s1==state && c==None &&  s== stackSymbol-> (s1,c,s,s2,stack)
        | (s1,c,s,s2,stack)::l -> trouveTransition3 state stackSymbol l ;;
    
    let rec trouveTransition4 (list_trans:transition list) = 
        match list_trans with
        |[]-> print_string "automate deterministe \n"
        | (s1,c,s,s2,stack)::l -> if(c==None) then print_string "automate non deterministe \n" else trouveTransition4 l;;


    
    

  
  (*Fonction qui prend un automate et qui renvoi ses transitions*)
  let recup_transitions automate = 
      match automate with
      | (declarations,transitions) -> transitions
      | _ -> failwith "erreur dans le type automate"

  (*Fonction qui prend un type declarations et qui renvoi un tuple contenant tt les donnes de lautomate *)
  let recup_declaration automate = 
      match automate with 
      | (declarations,transitions) -> 
      (match declarations with
          | (input, stack, state,init_state, init_stack) -> (input,stack,state,init_state,init_stack)
          | _ -> failwith "erreur dans le type declarations"
      )
      | _ -> failwith "erreur dans le type automate"
  (*Fonction qui prend un automate et un mot et qui teste si le mot est accepté ou pas *)
  
  let execute_automate automate mot =  
      let decla = recup_declaration automate in
      let list_transition = list_transitions (recup_transitions automate) in 
      if list_transition =[] then failwith "transitions vide" else
      let list_input = list_symbols (match decla with
                                        |(input, stack, state,init_state, init_stack) -> input
                                    ) in
      if list_input =[] then failwith "inputs vide" else
      let list_stack = list_symbols (match decla with
                                        |(input, stack, state,init_state, init_stack) -> stack
                                    ) in
      if list_stack =[] then failwith "symboles de pile vide" else
      let list_state = list_symbols (match decla with
                                        |(input, stack, state,init_state, init_stack) -> state
                                    ) in
      let init_state = (match decla with
                            |(input, stack, state,init_state, init_stack) -> init_state
                        ) in 
       if list_state =[] then failwith "symboles d etat vide" else
      let init_stack = (match decla with
                            |(input, stack, state,init_state, init_stack) -> init_stack
                         ) in 

      let list_mot = stringToListChar mot in
      (*let pile = ref [init_stack] in*)
      let pile = Stack.create () in
      let etat = ref init_state in
      Stack.push init_stack pile;


      let rec loop pile li = match li with |[]->() |h::t->(Stack.push h pile; loop pile t) in

      let majpile pile (e:stack) =
        match e with
        |None1-> Stack.pop pile; ()
        |Stack(l)-> Stack.pop pile; loop pile l
            in

      let rec trouv mot= 
            let i = ref 0 in 
            let taille=String.length mot in
            while !i<taille 
            do
                   if (Stack.is_empty pile) then failwith "la pile est vide sans que l entrée soit épuisée"
                   else
                   (
                    let t =trouveTransition !etat mot.[!i] (Stack.top pile) list_transition in
                   if (t) then (let trans=trouveTransition1 !etat mot.[!i] (Stack.top pile) list_transition  
                    in 
                                match trans with
                                    |(a,b,c,d,e)-> etat:=d ; majpile pile e; i:=!i+1;)
                        
                    else 
                    (
                    let t =trouveTransition2 !etat (Stack.top pile) list_transition in
                   if (t) then (let trans=trouveTransition3 !etat (Stack.top pile) list_transition  
                    in 
                                match trans with
                                    |(a,b,c,d,e)-> etat:=d ; majpile pile e;)
                    else 
                    (
                     failwith "il n y a aucune transition qui s applique " 
                    )
                    )
                   )
                     
            done ;

            
            while (Stack.is_empty pile==false )
            do
            let t =trouveTransition2 !etat (Stack.top pile) list_transition in
                   if (t) then (let trans=trouveTransition3 !etat (Stack.top pile) list_transition  
                    in 
                                match trans with
                                    |(a,b,c,d,e)-> etat:=d ; majpile pile e;)
                    else failwith "La pile est pleine et le mot est consommée -mot refuse- "
            done;
            if (Stack.is_empty pile) then failwith "MOT ACCEPTE"
             in


            trouv mot;;
            
            
            

let verif_autom (automate:automate) =
    let decla = recup_declaration automate in
      let list_transition = list_transitions (recup_transitions automate) in 
      if list_transition =[] then failwith "transitions vide" else
      let list_input = list_symbols (match decla with
                                        |(input, stack, state,init_state, init_stack) -> input
                                    ) in
      if list_input =[] then failwith "inputs vide" else
      let list_stack = list_symbols (match decla with
                                        |(input, stack, state,init_state, init_stack) -> stack
                                    ) in
      if list_stack =[] then failwith "symboles de pile vide" else
      let list_state = list_symbols (match decla with
                                        |(input, stack, state,init_state, init_stack) -> state
                                    ) in
      if list_state =[] then failwith "symboles d etat vide" else
      let init_state = (match decla with
                            |(input, stack, state,init_state, init_stack) -> init_state
                        ) in 
      let init_stack = (match decla with
                            |(input, stack, state,init_state, init_stack) -> init_stack
                         ) in 
      
     if ((test_input  list_state [init_state]) && (test_input list_stack [init_stack])) then  print_string "verification etat initial et symbole de pile initial d automate correcte \n" else 
     if (test_input list_stack [init_stack]=false) then failwith "verification symbole de pile initial d automate incorrecte" else 
     if (test_input list_state [init_state]=false) then failwith "verification etat initial d automate incorrecte " ;  
     trouveTransition4 list_transition;
     ;;

