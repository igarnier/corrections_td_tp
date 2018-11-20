(* TD 10, sur les arbres *)

(* Exercice 1

   rappel: la profondeur d'un arbre binaire est le nombre de noeuds sur le chemin le plus
   long partant de la racine.

   Donc si l'arbre a juste une racine, sa profondeur = 1.
   Si on a un arbre avec une racine et deux feuilles, profondeur = 2 et nombre de noeuds = 3.
   Continuons en rajoutant une couche de feuilles: on a profondeur = 3 et nombre de noeuds = 7.
   Il semble que la formule décrivant le nombre de noeud d'un abrbre binaire complet de profnodeur n
   soit 2^n - 1.

   1. borne superieure: 2^n - 1, borne atteinte pour un arbre équilibré. Preuve par recurrence.

   2. Tout noeud d'un arbre binaire a exactement 1 ancètre, sauf la racine qui n'en a pas.
   Donc le nombre d'aretes d'un arbre binaire contenant n noeuds est n - 1.

*)

(* Exercice 2 *)

type 'a noeud = {
  mutable v     : 'a;
  mutable left  : 'a noeud option;
  mutable right : 'a noeud option
}

(* Version directe. Correspond au code 'java' suivant:

   arbre = new noeud(3, 
                     new noeud(5, new noeud(9, null, null), null),
                     new noeud(7, null, null)) *)

let arbre =
  {
    v = 3;
    left = Some { v     = 5; 
                  left  = Some { v = 9; left = None; right = None };
                  right = None };
    right = Some { v     = 7;
                   left  = None;
                   right = None }
  }


(* Juste un intermède sans importance: on peut rendre le code OCaml plus similaire au code java: *)
let null = None

let new_node (v, left, right) = Some { v; left; right }

(* Cet arbre est égal à celui défini juste au dessus. *)
let arbre = 
  new_node (3, new_node (5, new_node (9, null, null), null),
               new_node (7, null, null))

(* Exercice 3. Fonctions d'arbres. *)

let rec taille (arbre : 'a noeud option) =
  match arbre with
  | None -> 0
  | Some noeud ->
    1 + (taille noeud.left) + (taille noeud.right)

let rec hauteur (arbre : 'a noeud option) =
  match arbre with
  | None -> 0
  | Some noeud ->
    1 + (max (hauteur noeud.left) (hauteur noeud.right))

let rec nb_feuilles (arbre : 'a noeud option) =
  match arbre with
  | None -> 0
  | Some noeud ->
    (match noeud.left, noeud.right with
     | None, None ->
       (* C'est une feuille *)
      1
     | _ ->
       nb_feuilles noeud.left + nb_feuilles noeud.right)

let rec max_val_aux (noeud : int noeud) =
  match noeud.left, noeud.right with
  | None, None ->
    noeud.v
  | Some n, None
  | None, Some n ->
    max noeud.v (max_val_aux n)
  | Some n, Some n' ->
    max noeud.v (max (max_val_aux n) (max_val_aux n'))

let max_val (arbre : int noeud option) =
  match arbre with
  | None -> failwith "arbre vide: pas de valeur maximale"
  | Some n ->
    max_val_aux n
    
let taille_test = taille arbre

let hauteur_test = hauteur arbre

let nbfeuilles_test = nb_feuilles arbre

let maxval_test = max_val arbre

let est_une_feuille (arbre : 'a noeud option) =
  match arbre with
  | None       -> false
  | Some noeud ->
    (match noeud.left, noeud.right with
     | None, None -> true
     | _          -> false)

let rec peigne_gauche (arbre : 'a noeud option) =
  match arbre with
  | None ->
    true
  | Some noeud ->
    est_une_feuille noeud.right && peigne_gauche noeud.left

(* Exercice 4. Parcourir un arbre *)

(* Parcours préfixe. *)

let rec parcours_prefixe (f : 'a -> unit) (arbre : 'a noeud option) =
  match arbre with
  | None -> ()
  | Some noeud ->
    f noeud.v;
    parcours_prefixe f noeud.left;
    parcours_prefixe f noeud.right

let rec parcours_infixe (f : 'a -> unit) (arbre : 'a noeud option) =
  match arbre with
  | None -> ()
  | Some noeud ->
    parcours_infixe f noeud.left;
    f noeud.v;    
    parcours_infixe f noeud.right

let rec parcours_postfixe (f : 'a -> unit) (arbre : 'a noeud option) =
  match arbre with
  | None -> ()
  | Some noeud ->
    parcours_postfixe f noeud.left;
    parcours_postfixe f noeud.right;
    f noeud.v

(* Parcours: 
   prefixe: 3, 5, 9, 2, 6, 8, 7, 0, 1, 4
   infixe: 2, 9, 6, 8, 5, 3, 0, 7, 4, 1
   postfixe: 2, 8, 6, 9, 5, 0, 4, 1, 7, 3
*)

(* On teste sur l'arbre donné dans le TD. *)

let arbre_exo4 =
  new_node (3,
            new_node (5,
                      new_node (9,
                                new_node (2, null, null),
                                new_node (6,
                                          null,
                                          new_node (8, null, null)
                                         )
                               ),
                      null),
            new_node (7,
                      new_node (0, null, null),
                      new_node (1,
                                new_node (4, null, null),
                                null)
                     )
           )

let test_prefixe =
  Printf.printf "parcours prefixe: ";
  parcours_prefixe (fun i -> Printf.printf "%d " i) arbre_exo4;
  print_newline ()

let test_infixe = 
  Printf.printf "parcours infixe: ";
  parcours_infixe (fun i -> Printf.printf "%d " i) arbre_exo4;
  print_newline ()

let test_postfixe =
  Printf.printf "parcours postfixe: ";
  parcours_postfixe (fun i -> Printf.printf "%d " i) arbre_exo4;
  print_newline ()

(* 4.5:
   oui, oui, oui
*)

(* 4.6: on utilise les piles OCaml, mais on pourrait également réutiliser le
   code du TD 8. *)

let arbre_vide (arbre : 'a noeud option) =
  match arbre with
  | None -> true
  | _    -> false

(* note: l'algo de l'exercice ne gère pas les nulls *)
let fonction (f : 'a -> unit) (arbre : 'a noeud option) =
  let p = Stack.create () in
  Stack.push arbre p;
  while not (Stack.is_empty p) do
    let n = Stack.pop p in
    match n with
    | None ->
      ()
    | Some node ->
      f node.v;
      Stack.push node.right p;
      Stack.push node.left p
  done


(* Produit le parcours prefixe *)
let _ = fonction (fun i -> Printf.printf "%d " i) arbre_exo4

let fonction2 (f : 'a -> unit) (arbre : 'a noeud option) =
  let p = Queue.create () in
  Queue.add arbre p;
  while not (Queue.is_empty p) do
    let n = Queue.take p in
    match n with
    | None ->
      ()
    | Some node ->
      f node.v;
      Queue.add node.right p;
      Queue.add node.left p
  done

(* Produit le parcours en largeur *)
let _ = fonction2 (fun i -> Printf.printf "%d " i) arbre_exo4


(* Exercice 5: notation polonaise inverse *)

(* 1. Parcours postfixe: d'abord on calcule les opérandes, puis on calcule l'opération *)

(* 2. *)

(* Je copie-colle le code des listes fait en td8/9 *)

type 'a cell = {
  mutable v    : 'a;
  mutable next : 'a cell option
}

(* -------------------------------------------------------------------------- *)
(* Une liste chaînée est une suite de cellule. Par dessus ces suites de 
   cellules, on peut construire des listes avec "pointeurs de tête" et 
   "pointeurs de queue". Je préfère séparer clairement les algorithmes sur
   les listes "pures", vues comme suites de cellules, des algorithmes sur
   les listes avec pointeurs de tête et etc. *)

module Liste =
struct

  type 'a t = 'a cell option

  let create () = None

  (* Retourne "vrai" si et seulement si la liste est vide. O(1) *)
  let is_empty (l : 'a t) =
    match l with
    | None   -> true
    | Some _ -> false

  (* Retourne la valeur stockée dans la première cellule de la liste. 
     Lève une erreur si la liste est vide. O(1) *)
  let get_first (l : 'a t) =
    match l with
    | Some cell -> cell.v
    | None      ->
      (* erreur *)
      failwith "get_first: liste vide, erreur"

  (* Insère un nouvel élément en tête de liste. 
     Retourne la nouvelle cellule. O(1) *)
  let insert_first (l : 'a t) (x : 'a) =
    match l with
    | None ->
      Some { v = x; next = None }
    | Some cell ->
      Some { v = x; next = Some cell }

  let rec rev_aux (l : 'a t) (acc : 'a t) =
    match l with
    | None -> acc
    | Some cell ->
      let next = cell.next in
      cell.next <- acc;
      rev_aux next l

  let rev (l : 'a t) =
    rev_aux l None
      
  let rec iter (f : 'a -> unit) (c : 'a t) =
    match c with
    | None -> ()
    | Some cell ->
      f cell.v;
      iter f cell.next

end

module ListeSimple =
struct

  (* Une liste est un pointeur sur une cellule. Le "option" veut dire que le pointeur
     peut être nul (en OCaml, Null = None). *)
  type 'a t = {
    mutable first : 'a Liste.t
  }

  (* Crée une liste vide. O(1) *)
  let create () =
    { first = None }

  (* Retourne "vrai" si et seulement si la liste est vide. O(1) *)
  let is_empty (l : 'a t) =
    Liste.is_empty l.first
      
  (* Retourne la valeur stockée dans la première cellule de la liste. 
     Lève une erreur si la liste est vide. O(1) *)
  let get_first (l : 'a t) =
    Liste.get_first l.first

  (* Insère un nouvel élément en tête de liste. O(1) *)
  let insert_first (l : 'a t) (x : 'a) =
    l.first <- Liste.insert_first l.first x

  (* Ajoute un élément en fin de liste. O(n) !!! 
     Il faut parcourir toute la liste jusqu'à arriver à la
     fin pour ajouter une nouvelle cellule.
  *)
  let insert_last (l : 'a t) (x : 'a) =
    let rec loop (c : 'a cell) =
      match c.next with
      | None ->
        let cell = Some { v = x; next = None } in
        c.next <- cell
      | Some cell ->
        loop cell
    in
    match l.first with
    | None ->
      l.first <- Some { v = x; next = None }
    | Some cell ->
      loop cell

  (* Enlève un élément en tête de liste. O(1) *)
  let take (l : 'a t) =
    match l.first with
    | None ->
      (* erreur *)
      failwith "take: liste vide, erreur"
    | Some cell ->
      let next = cell.next in
      l.first <- next;
      cell.v

  (* Déplace la tête de liste sur la cellule suivante (qui peut être None). Erreur si la liste est vide. *)
  let move_next (l : 'a t) =
    match l.first with
    | None ->
      failwith "move_next: liste vide, erreur"
    | Some cell ->
      let next = cell.next in
      l.first <- next

  (* Inverse une liste. La liste originelle est détruite. O(n) *)
  let rev (l : 'a t) =
    l.first <- Liste.rev l.first

  (* Applique une fonction [f] à chaque élément d'une liste chaînée [l]. *)
  let iter (f : 'a -> unit) (l : 'a t) =
    Liste.iter f l.first
  
end

(* Le type suivant est un peu comme un type enum en C (je ne sais pas si ça existe en Java) *)
type operation =
  | Addition
  | Soustraction
  | Multiplication
  | Division

type symbole =
  | Nombre of int
  | Operation of operation

let to_tree (liste : symbole ListeSimple.t) =
  let p  = Stack.create () in
  while not (ListeSimple.is_empty liste) do
    let elt = ListeSimple.get_first liste in
    ListeSimple.move_next liste;
    match elt with
    | Nombre _ ->
      Stack.push (new_node (elt, null, null)) p
    | Operation _ ->
      let rhs = Stack.pop p in
      let lhs = Stack.pop p in
      Stack.push (new_node (elt, lhs, rhs)) p
  done;
  Stack.pop p
    
(* 3. *)

let op_to_string (op : operation) =
  match op with
  | Addition       -> "+"
  | Soustraction   -> "-"
  | Multiplication -> "*"
  | Division       -> "/"

let rec to_string (expr : symbole noeud option) =
  match expr with
  | None -> ""
  | Some noeud ->
    (match noeud.v with
     | Nombre i -> string_of_int i
     | Operation op ->
       let left  = to_string noeud.left in
       let right = to_string noeud.right in
       "(" ^ left ^ (op_to_string op) ^ right ^ ")")

let test =
  let l = ListeSimple.create () in
  ListeSimple.insert_first l (Operation Addition);
  ListeSimple.insert_first l (Operation Soustraction);
  ListeSimple.insert_first l (Nombre 1);
  ListeSimple.insert_first l (Nombre 5);
  ListeSimple.insert_first l (Operation Multiplication);
  ListeSimple.insert_first l (Nombre 6);
  ListeSimple.insert_first l (Nombre 2);
  l

let arbre = to_tree test

let _ =
  Printf.printf "%s\n" (to_string arbre)
