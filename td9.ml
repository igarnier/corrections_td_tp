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
(* Liste chaînées avec pointeur de tête ET de queue, en OCaml. *)

module ListeQueue =
struct

  (* Une liste est un pointeur sur une cellule. Le "option" veut dire que le pointeur
     peut être nul (en OCaml, Null = None). *)
  type 'a t = {
    mutable first : 'a cell option;
    mutable last  : 'a cell option
  }

  let create () =
    { first = None; last = None }

  let is_empty (l : 'a t) =
    match l.first, l.last with
    | None, None -> true
    | Some _, Some _ -> false
    | _ ->
      failwith "is_empty: liste incohérente ..."

  let get_first (l : 'a t) =
    match l.first with
    | Some cell -> cell.v
    | None ->
      (* erreur *)
      failwith "get_first: liste vide, erreur"
  
  (* Insère un nouvel élément en tête de liste. O(1) *)
  let insert_first (l : 'a t) (x : 'a) =
    match l.first with
    | None ->
      (* liste vide *)
      let cell = Some { v = x; next = None } in
      l.first <- cell;
      l.last  <- cell
    | Some cell ->
      l.first <- Some { v = x; next = Some cell }

  (* Insère un nouvel élément en fin de liste. O(1)! Comparer
     avec l'implémentation précédente. *)
  let insert_last (l : 'a t) (x : 'a) =
    match l.last with
    | None ->
      (* liste vide *)
      let cell = Some { v = x; next = None } in
      l.first <- cell;
      l.last  <- cell
    | Some c ->
      let cell = Some { v = x; next = None } in
      c.next <- cell;
      l.last <- cell

  (* Enlève un élément en tête de liste. O(1) 
     Il faut faire très attention au cas où la liste
     ne contient qu'un seul élément !
  *)
  let take (l : 'a t) =
    match l.first with
    | None ->
      (* erreur *)
      failwith "take: liste vide, erreur"
    | Some cell ->
      match cell.next with
      | None ->
        (* C'est le dernier élément ! *)
        let result = cell.v in
        l.first <- None;
        l.last  <- None;
        result
      | _ ->
        let result = cell.v in
        l.first <- cell.next;
        result

  (* Applique une fonction [f] à chaque élément d'une liste chaînée [l]. *)
  let iter (f : 'a -> unit) (l : 'a t) =
    Liste.iter f l.first

end


(* Une fonction qui crée une liste avec un seul élément. Une valeur de type
   'a option est soit égale à None soit égale à "Some x" où x est de type 'a.
*)
(* let singleton x =
 *   let cell = Some { v = x; next = None } in
 *   { first = cell; last = cell } *)


(* -------------------------------------------------------------------------- *)
(* Début du TD. Le type abstrait des files: *)

module type File =
sig

  (* type générique des files qui contiennent des éléments de type 'a *)
  type 'a t

  (* crée une file vide (unit est un peu comme void en Java) *)
  val create : unit -> 'a t

  (* ajoute un élément x à la fin de la file *)
  val put : 'a t -> 'a -> unit
    
  (* enlève la valeur au début de la file et la renvoie *)
  (* si jamais la file est vide, déclenche une erreur *)
  val get : 'a t -> 'a

end


(* La solution de l'exercice 1 est paramétrée par une implémentation
   de l'interface "File". *)
module Exercice1(F : File) =
struct

  let suite = 
    [ 'E'; 'A'; 'S'; '*'; 'Y'; '*'; 'Q'; 'U'; 'E'; '*'; '*'; '*';
      'S'; 'T'; '*'; '*'; '*'; 'I'; 'O'; '*'; 'N'; '*'; '*'; '*' ]

  let file = F.create ()

  (* On fait une fonction récursive "loop" qui 
     examine les éléments un par un, si l'élément est un '*'
     on fait un 'pop' sinon on le push sur la pile. 
     Quand on a examiné tous les éléments (l = []) on termine. *)
  let _ =
    let rec loop l =
      match l with
      | [] -> ()
      | '*' :: reste ->
        let top = F.get file in
        Printf.printf "%c" top;
        loop reste
      | lettre :: reste ->
        F.put file lettre;
        loop reste
    in
    loop suite;
    print_newline ()

end

(* Exercice 2: Réalisations concrètes. Tout le travail a été fait
   en amont, dans les modules ListeSimple et ListeQueue. *)

(* Avec liste simple *)
module FileListe : File with type 'a t = 'a ListeSimple.t =
struct
  
  type 'a t = 'a ListeSimple.t

  let create () = ListeSimple.create ()

  let put file elt =
    ListeSimple.insert_last file elt

  let get file =
    ListeSimple.take file
  
end

(* Avec liste avec pointeur de queue *)
module FileListeQueue : File with type 'a t = 'a ListeQueue.t =
struct
  
  type 'a t = 'a ListeQueue.t

  let create () = ListeQueue.create ()

  let put file elt =
    ListeQueue.insert_last file elt

  let get file =
    ListeQueue.take file
  
end


(* Avec une paire de listes simples. *)
module FilePaireListe : File =
struct
  
  type 'a t =
    {
      mutable first  : 'a Liste.t;
      mutable second : 'a Liste.t
    }

  let create () = { first  = Liste.create (); 
                    second = Liste.create () }

  let put file elt =
    file.first <- Liste.insert_first file.first elt
      
  let get file =
    match file.second with
    | None ->
      (* la seconde liste est vide! *)
      file.second <- Liste.rev file.first;
      file.first  <- None;
      (match file.second with
       | None ->
         (* la première liste était vide également. *)
         failwith "get: file vide, erreur"
       | Some cell ->
         let res = cell.v in
         file.second <- cell.next;
         res)      
    | Some cell ->
      let res = cell.v in
      file.second <- cell.next;
      res
  
end

(* On résoud l'exercice 1 avec les différentes implémentations de file qu'on
   vient de produire. *)
module Exo1AvecListe = Exercice1(FileListe)

module Exo1AvecListeQueue = Exercice1(FileListeQueue)

module Exo1AvecPaireListe = Exercice1(FilePaireListe)


(* Exercice 3: listes circulaires *)

(* L'argument [past] correspond à la liste des cellules qu'on a déjà parcouru.
   Si jamais lors du parcours de la liste, on retombe sur une cellule déjà vue,
   cela signifie que la liste contient une boucle, i.e. qu'elle est 'circulaire'.
   Noter que [past] est une liste dont les valeurs sont des pointeurs sur des
   cellules ... d'où le type "'a cell cell option".
*)

let rec deja_vu (cell : 'a cell) (past : 'a cell cell option) =
  match past with
  | None -> false
  | Some c ->
    c.v == cell || (deja_vu cell c.next)

let rec est_circulaire_aux (cell : 'a cell option) (past : 'a cell cell option) =
  match cell with
  | None -> false
  | Some cell ->
    (deja_vu cell past) ||
    est_circulaire_aux cell.next (Some { v = cell; next = past })

let est_circulaire cell =
  match cell with
  | None ->
    (* une liste vide n'est pas circulaire *)
    false
  | Some cell ->
    (* regardons si on retombe sur [cell] en partant
       de [cell.next]. *)
    est_circulaire_aux cell.next None

let test : int Liste.t =
  let a = { v = 1; next = None } in
  let b = { v = 2; next = Some a } in
  let c = { v = 3; next = Some a } in
  a.next <- Some c;
  b.v    <- 4;
  Some b

let _ = est_circulaire test  
