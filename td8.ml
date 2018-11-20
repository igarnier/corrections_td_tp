(* -------------------------------------------------------------------------- *)
(* Implémentation de listes (important) *)

(* Type de cellule de liste chainée, utilisée par toutes les implémentations 
   qui suivent. *)

(* On définit le type des listes simplement chainées comme une suite
   de "cellules". On fait un type de liste /générique/ en le type
   d'élément que contient la liste, comme les generics en Java, si vous
   connaissez :
   https://en.wikipedia.org/wiki/Generics_in_Java
   Le type plus bas est l'équivalent du type Java suivant:
    class Cell<T> {
        T       v;
        Cell<T> next;
    }
   Le <T> est un paramètre de type qui correspond au 'a en OCaml.

   En OCaml il n'y a pas de pointeurs nulls. Donc on utilise un type qui modélise
   l'absence potentielle de quelque chose: le type "option". 

   Donc, le champ [next] ci-dessous est une valeur de type "'a cell" qui est 
   soit présente (auquel cas next = Some cell) soit absente (next = None).
*)

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

(* -------------------------------------------------------------------------- *)
(* Liste chaînées avec pointeur de tête, en OCaml. *)

module ListeSimple =
struct

  (* Une liste est un pointeur sur une cellule. Le "option" veut dire que le pointeur
     peut être nul (en OCaml, Null = None). *)
  type 'a t = {
    mutable first : 'a cell option
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

  (* Inverse une liste. La liste originelle est détruite. O(n) *)
  let rev (l : 'a t) =
    l.first <- Liste.rev l.first

  (* Applique une fonction [f] à chaque élément d'une liste chaînée [l]. *)
  let iter (f : 'a -> unit) (l : 'a t) =
    Liste.iter f l.first
  
end


(* -------------------------------------------------------------------------- *)
(* DEBUT OFFICIEL DE LA CORRECTION DU TD 8 *)

(* Une pile est une structure de donnée abstraite sur laquelle sont définies
   trois opérations: 

   empty(P) : teste si la pile est vide
   push(x, P) : ajoute un élément x au sommet de la pile P
   pop(P) : enlève la valeur au sommet de la pile P et la renvoie
   Si on applique l'opération "pop" à une pile vide, c'est une erreur!

   En Java, on écrirait une interface qui ressemblerait à ça :

   public interface Stack {
     protected Stack();

     public bool empty();
     public void push(int x);
     public Object pop();
   }

   et si on a une implémentation concrète de cette interface,
   alors on ferait pour une instance P de cette classe,
   à la place de push(x,P) -> P.push(x),
                 empty(P)  -> P.empty()
                 pop(P)    -> P.pop()
   
   
   Mais aujourd'hui on fait du OCaml. (C'est une bonne idée de convertir le 
   TD en Java ou dans un autre langage de votre choix).
   Voilà l'équivalent d'une interface en OCaml.
*)

module type Pile =
sig

  (* type générique des piles qui contiennent des éléments de type 'a *)
  type 'a t

  (* crée une pile vide (unit est un peu comme void en Java) *)
  val create : unit -> 'a t

  (* ajoute un élément x au sommet de la pile *)
  val push : 'a t -> 'a -> unit
    
  (* enlève la valeur au sommet de la pile et la renvoie *)
  (* si jamais la pile est vide, déclenche une erreur *)
  val pop : 'a t -> 'a

end

(* Exercice 1 *)

(* 
   suite = L A * S T I * N * F I R * S T * * O U * T * * * * * *

   suite des instructions:

   initialement, pile P vide
   
   push(P, L) -> P = L :: []
   push(P, A) -> P = A :: L :: []
   POP(P)     -> résultat = A, P = L :: []
   push(P, S) -> P = S :: L :: []
   push(P, T) -> P = T :: S :: L :: []
   push(P, I) -> P = I :: T :: S :: L :: []
   POP(P)     -> résultat = I, P = T :: S :: L :: []
   ... etc. 
*)

(* La solution de l'exercice 1 est paramétrée par une implémentation
   de l'interface "Pile". *)

module Exercice1(P : Pile) =
struct

  let suite = 
    [ 'L'; 'A'; '*'; 'S'; 'T'; 'I'; '*'; 'N'; '*'; 'F'; 'I'; 'R'; '*'; 'S'; 'T';
      '*'; '*'; 'O'; 'U'; '*'; 'T'; '*'; '*'; '*'; '*'; '*'; '*' ]

  let pile = P.create ()

  (* On fait une fonction récursive "loop" qui 
     examine les éléments un par un, si l'élément est un '*'
     on fait un 'pop' sinon on le push sur la pile. 
     Quand on a examiné tous les éléments (l = []) on termine. *)
  let _ =
    let rec loop l =
      match l with
      | [] -> ()
      | '*' :: reste ->
        let top = P.pop pile in
        Printf.printf "%c\n" top;
        loop reste
      | lettre :: reste ->
        P.push pile lettre;
        loop reste
    in
    loop suite

end


(* Exercice 2: Réalisations concrètes. *)

(* 2.1 *)

(* On réalise une implémentation de l'interface 'Pile' par une liste simplement chaînée.
   On exporte le fait que la liste est implémentée par une liste (par défaut, c'est caché).
 *)
module PileAsList : Pile with type 'a t = 'a ListeSimple.t =
struct

  (* Les piles sont juste des listes *)
  type 'a t = 'a ListeSimple.t

  (* Initialement, la liste d'éléments est vide. 
     "None" correspond à "NULL". *)
  let create () =
    ListeSimple.create ()
    
  let push pile x =
    ListeSimple.insert_first pile x

  let pop pile =
    match pile.ListeSimple.first with
    | None ->
      (* La pile est vide! Erreur.*)
      failwith "pop: erreur, pile vide"
    | Some cell ->
      let next = cell.next in
      pile.first <- next;
      cell.v

end

(* Pile d'entiers de longueur max fixée. Noter que PileAsArray
   ne suit pas l'interface "Pile" donnée plus haut: le type
   de create est différent. *)
module PileAsArray(Len : sig val length : int end) =
struct

  type 'a t = 
    {
              tab : 'a array;
      mutable top : int
    }

  (* Initialement on crée un tableau vide car on ne sait pas quoi
     mettre dedans (vu que le type est générique !). Si on faisait *)
  let create default =
    { 
      tab = Array.make Len.length default;
      top = -1
    }
    
  let push pile x =
    if pile.top = Len.length - 1 then
      failwith "push: taille max atteinte";
    pile.top <- pile.top + 1;
    pile.tab.(pile.top) <- x

  let pop pile =
    if pile.top = -1 then
      (* La pile est vide! Erreur.*)
      failwith "pop: erreur, pile vide";
    let res = pile.tab.(pile.top) in
    pile.top <- pile.top - 1;
    res

end

(* On crée un module de piles de caractères *)

(* Application à l'exercice 1 *)

module Exo1AvecListe = Exercice1(PileAsList)

(*
Sortie:
A
I
N
R
T
S
U
T
O
I
F
T
S
*)

(* Exercice 3 *)

(* 3.1 *)

let reverse_list (l : 'a ListeSimple.t) =
  let pile = PileAsList.create () in
  while not (ListeSimple.is_empty l) do
    let first_cell = l.first in
    match first_cell with
    | None -> failwith "impossible, la liste n'est pas vide"
    | Some cell ->
      let v = cell.v in
      PileAsList.push pile v;
      l.first <- cell.next
  done;
  pile

(* Test. *)

(* une liste qui contient les éléments 1,2,3. On la crée d'un
   seul coup. *)
let liste_test = 
  {
    ListeSimple.first =
      Some { v    = 1;
             next = Some { v    = 2;
                           next = Some { v    = 3; 
                                         next = None
                                       }
                         }
           }
  }

let resultat = reverse_list liste_test

(* val resultat : int linked_list =
  {first = Some {v = 3; next = Some {v = 2; next = Some {v = 1; next = None}}}}
*)


let rec concat_aux (l1 : 'a cell option) (l2 : 'a cell option) =
  match l1 with
  | None -> 
    (* la première liste est vide, donc le résultat est égal à l2 *)
    l2
  | Some { v; next } ->
    Some { v; next = concat_aux next l2 }

let concat (l1 : 'a ListeSimple.t) (l2 : 'a ListeSimple.t) =
  { ListeSimple.first = concat_aux l1.first l2.first }
