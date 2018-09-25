(**
   Mini intro à OCaml.

   0) Commentaires: (* blahblah *)

   1) Types de base: int, bool, string, float

   2) Variables

   Déclaration d'une variable x égale à 3 :
   let x = 3 in ...

   Attention, les variables en OCaml sont *constantes* par défaut.
   Pour pouvoir les modifier, il faut déclarer la variable comme référence.

   let x = ref 3 in ...

   Pour récupérer la valeur d'une référence, on fait !x
   Pour assigner une nouvelle valeur à une référence, on fait x := ...

   E.g. déclarer une référence et l'incrémenter. L'expression suivante s'évalue à 4.

   let x = ref 3 in
   x := !x + 1;
   !x

   3) Tableaux

   Syntaxe pour créer un tableau de longueur 4 contenant les éléments 0,1,4,2: [| 0; 1; 4; 2 |]

   Les tableaux en OCaml sont indicés à partir de 0 (comme en C/Java/etc)

   Accéder à l'élément d'indice i d'un tableau t:  t.(i)

   L'expression suivante s'évalue à la valeur 4:

   let tableau = [| 0; 1; 4; 2 |] in
   tableau.(2)

   Assigner une valeur à un tableau:
   tableau.(i) <- valeur

   4) Fonctions

   Déclaration d'une fonction "test" prenant 4 arguments arg1 ... arg4 et calculant leur somme:

   let test arg1 arg2 arg3 arg4 =
     arg1 + arg2 + arg3 + arg4

   On peut spécifier les types si on veut être précis:

   let test (arg1 : int) (arg2 : int) (arg3 : int) (arg4 : int) =
     arg1 + arg2 + arg3 + arg4

   5) Structures de contrôle

   if condition then
     blahblah
   else
     blahblah

   while condition do
     truc
   done

   for i = debut to fin do
     truc
   done

   for i = fin downto debut do
     truc
   done
*)



(** Exercice 1 du TD 2: "tri par sélection" *)

(* Renvoie l'indice de l'élément le plus petit dans l'intervalle [start; stop]
   du tableau t.
   Par exemple, soit t = [| 10; -1; 9; 2 |]. Alors,
   "trouve_minimum t 0 3" calcule l'indice du plus petit élément entre
   les indices 0 et 3 (inclus). 
   Ici, trouve_minimum t 0 3 = 1 (correspondant à l'élément -1)
   Autre exemple: "trouve_minimum [| 10; -1; 9; 2 |] 2 3 " cherche l'indice
   du plus petit élément entre les indices 2 et 3 (inclus). 
   Alors, trouve_minimum t 2 3 = 3 (correspondant à l'élément 2) *)
let trouve_minimum (t : int array) (start : int) (stop : int) =
  if stop < start then
    failwith "Erreur: stop < start";
  let min = ref start in
  for j = start + 1 to stop do
    if t.(j) < t.(!min) then
      min := j
  done;
  !min

(* Calcule le tri par sélection, en place, du tableau t. 
   Utilise la routine auxiliaire "trouve_minimum". *)
let tri_par_selection (t : int array) =
  let n = Array.length t in
  for i = 0 to n - 2 do
    let min = trouve_minimum t i (n-1) in
    let x   = t.(i) in
    t.(i) <- t.(min);
    t.(min) <- x
  done

(* ----------------------------------------------------------------- *)
(* Versions du code ci-dessus avec affichage, pour les besoins du td *)
(* Pas besoin de lire ce code *)

let affiche_tab (t : int array) =
  let elements = Array.to_list t in
  let strings  = List.map string_of_int elements in
  "[|"^(String.concat ";" strings)^"|]"  

let trouve_minimum_affichage (t : int array) (start : int) (stop : int) =
  let open Printf in
  if stop < start then
    failwith "Erreur: stop < start";  
  let min = ref start in
  printf "  Calcul du minimum entre start = %d et stop = %d.\n" start stop;
  printf "  On pose initialement min = start = %d.\n" start;
  printf "  On boucle pour j allant de %d a %d\n" (start+1) stop;
  for j = start + 1 to stop do
    printf "  boucle interieure: j = %d\n" j;
    if t.(j) < t.(!min) then
      (printf "  t[%d] = %d < t[%d] = %d, on assigne min := %d \n" j t.(j) !min t.(!min) j;
      min := j)
    else
      printf "  t[%d] = %d >= t[%d] = %d, rien a faire\n" j t.(j) !min t.(!min)
  done;
  !min

let tri_par_selection_affichage (t : int array) =
  let open Printf in
  let n = Array.length t in  
  printf "Etape initiale. t = %s, taille %d\n" (affiche_tab t) n;
  printf "On boucle pour i allant de %d a %d\n" 0 (n - 2);
  for i = 0 to n - 2 do
    printf "entree dans la boucle exterieure: i = %d\n" i;
    let min = trouve_minimum_affichage t i (n-1) in
    printf "min entre %d et %d = %d\n" i (n-1) min;
    let x   = t.(i) in
    t.(i) <- t.(min);
    t.(min) <- x;
    printf "fin de boucle pour i = %d: t = %s\n" i (affiche_tab t);
  done;
  (* on renvoie le tableau trié (meme si ici c'est superflu en principe) *)
  t

(*
Résultat:

Etape initiale. t = [|0;3;1;2|], taille 4
On boucle pour i allant de 0 a 2
entree dans la boucle exterieure: i = 0
  Calcul du minimum entre start = 0 et stop = 3.
  On pose initialement min = start = 0.
  On boucle pour j allant de 1 a 3
  boucle interieure: j = 1
  t[1] = 3 >= t[0] = 0, rien a faire
  boucle interieure: j = 2
  t[2] = 1 >= t[0] = 0, rien a faire
  boucle interieure: j = 3
  t[3] = 2 >= t[0] = 0, rien a faire
min entre 0 et 3 = 0
fin de boucle pour i = 0: t = [|0;3;1;2|]
entree dans la boucle exterieure: i = 1
  Calcul du minimum entre start = 1 et stop = 3.
  On pose initialement min = start = 1.
  On boucle pour j allant de 2 a 3
  boucle interieure: j = 2
  t[2] = 1 < t[1] = 3, on assigne min := 2 
  boucle interieure: j = 3
  t[3] = 2 >= t[2] = 1, rien a faire
min entre 1 et 3 = 2
fin de boucle pour i = 1: t = [|0;1;3;2|]
entree dans la boucle exterieure: i = 2
  Calcul du minimum entre start = 2 et stop = 3.
  On pose initialement min = start = 2.
  On boucle pour j allant de 3 a 3
  boucle interieure: j = 3
  t[3] = 2 < t[2] = 3, on assigne min := 3 
min entre 2 et 3 = 3
fin de boucle pour i = 2: t = [|0;1;2;3|]
*)

(* Compte du # de comparaisons *)
(* On utilise un petit hack en définissant un opérateur de comparaisons
   qui compte pour nous. *)

let nombre_comparaisons = ref 0

let (<<) x y =
  nombre_comparaisons := !nombre_comparaisons + 1;
  x < y

let trouve_minimum_compte (t : int array) (start : int) (stop : int) =
  let min = ref start in
  for j = start + 1 to stop do
    if t.(j) << t.(!min) then
      min := j
  done;
  !min

(* Calcule le tri par sélection, en place, du tableau t. 
   Utilise la routine auxiliaire "trouve_minimum". *)
let tri_par_selection_compte (t : int array) =
  let n = Array.length t in
  for i = 0 to n - 2 do
    let min = trouve_minimum_compte t i (n-1) in
    let x   = t.(i) in
    t.(i) <- t.(min);
    t.(min) <- x
  done;
  t

let resultat = 
  nombre_comparaisons := 0;
  let resultat = tri_par_selection_compte [| 0; 3; 1; 2 |] in
  (resultat, !nombre_comparaisons)

(* On obtient:       
   val resultat : int array * int = ([|0; 1; 2; 3|], 6) 
   
   Note: ça ne prend pas en compte les tests de fin de boucles!
*)



(** Exercice 2 du TD 2: "tri sur tri" *)

(* Une fonction pour calculer l'ordre lexicographique sur des 
   tableaux de char *)
let inf (t1 : char array) (t2 : char array) =
  let n1   = Array.length t1 in
  let n2   = Array.length t2 in
  let min, if_equal_prefix  = 
    if n1 < n2 then
      n1, -1
    else if n1 > n2 then
      n2, 1
    else  (* n1 = n2 *)
      n1, 0
  in
  let i    = ref 0 in
  while !i < min && t1.(!i) = t2.(!i) do
    i := !i + 1
  done;
  if !i = min then
    (* préfixes [0, min - 1] égaux *)
    if_equal_prefix
  else
    (* !i < min => t1.(!i) <> t2.(!i) *)
    (if t1.(!i) < t2.(!i) then
       -1
     else if t1.(!i) > t2.(!i) then
       1
     else
       failwith "Error: impossible case")

let of_string s =
  Array.init (String.length s) (fun i -> s.[i])

(* Testons notre fonction *)
let result =
  inf (of_string "musique") (of_string "musicien")


let trouve_minimum_char (t : char array) (start : int) (stop : int) =
  if stop < start then
    failwith "Erreur: stop < start";
  let min = ref start in
  for j = start + 1 to stop do
    if t.(j) < t.(!min) then
      min := j
  done;
  !min

let tri_par_selection_char (t : char array) =
  let n = Array.length t in
  for i = 0 to n - 2 do
    let min = trouve_minimum_char t i (n-1) in
    let x   = t.(i) in
    t.(i) <- t.(min);
    t.(min) <- x
  done

let trouve_minimum_char_array (t : char array array) (start : int) (stop : int) =
  if stop < start then
    failwith "Erreur: stop < start";
  let min = ref start in
  for j = start + 1 to stop do
    if inf t.(j) t.(!min) = -1 then
      min := j
  done;
  !min

let tri_par_selection_char_array (t : char array array) =
  let n = Array.length t in
  for i = 0 to n - 2 do
    let min = trouve_minimum_char_array t i (n-1) in
    let x   = t.(i) in
    t.(i) <- t.(min);
    t.(min) <- x
  done

let solution (t : char array array) =
  let n = Array.length t in
  for i = 0 to n - 1 do
    tri_par_selection_char t.(i)
  done;
  tri_par_selection_char_array t

let entree = [| of_string "car"; of_string "arbre"; of_string "ceci" |]

(*

utop[15]> solution entree;;
- : unit = ()
utop[16]> entree;;
- : char array array =
[|[|'a'; 'b'; 'e'; 'r'; 'r'|]; [|'a'; 'c'; 'r'|]; [|'c'; 'c'; 'e'; 'i'|]|]
*)  

(* Une solution plus élégante: tri par sélection générique *)

let trouve_minimum_gen (lt : 'a -> 'a -> bool) (t : 'a array) (start : int) (stop : int) =
  if stop < start then
    failwith "Erreur: stop < start";
  let min = ref start in
  for j = start + 1 to stop do
    if lt t.(j) t.(!min) then
      min := j
  done;
  !min

let tri_par_selection_gen (lt : 'a -> 'a -> bool) (t : 'a array) =
  let n = Array.length t in
  for i = 0 to n - 2 do
    let min = trouve_minimum_gen lt t i (n-1) in
    let x   = t.(i) in
    t.(i) <- t.(min);
    t.(min) <- x
  done

let lt_char (c1 : char) (c2 : char) =
  c1 < c2

let lt_char_array (t1 : char array) (t2 : char array) =
  (inf t1 t2) = -1

let solution (t : char array array) =
  let n = Array.length t in
  for i = 0 to n - 1 do
    tri_par_selection_gen lt_char t.(i)
  done;
  tri_par_selection_gen lt_char_array t



(** Exercice 3: tri bourrin *)

let xchng (t : int array) (i : int) (j : int) =
  let v = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- v

(* 
pour chaque indice i de 0 à n - 2
  pour chaque indice j entre i+1 et n-1,
   si l'élément j est plus petit que l'élément i, les échanger
*)
let tri_bourrin (t : int array) =
  let n = Array.length t in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      if t.(i) > t.(j) then
        xchng t i j
    done
  done

(* Pourquoi c'est correct:
   Un tableau est trié si pour tout i < j, t.(i) < t.(j)

   L'invariant maintenu par l'algorithme de tri_bourrin est que pour tout indice i,
   les éléments entre [0,i-1] sont 1) triés et 2) plus petits que tous les éléments dans
   [i, n-1].

   Donc en particulier, pour i = n, le tableau est trié entre [0,n-1] (et le tableau [n,n-1] est vide).
   
   A chaque étape, l'algorithme étend l'invariant de i à i+1 en scannant les éléments 
   d'indice j dans [i+1,n] et en échangeant t[i] avec t[j] si t[j] < t[i]. 
*)

let test = [| 0; -1; 9; 2; 3 |]


(** Exercice 4, drapeau Hollandais *)

type couleur =
  | Zero
  | Un
  | Deux

let xchng_clr (t : couleur array) (i : int) (j : int) =
  let v = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- v

let tri_drapeau (t : couleur array) =
  let p = ref 0 in
  let m = ref 0 in
  let g = ref (Array.length t - 1) in
  while !m <= !g do
    match t.(!m) with
    | Zero ->
      xchng_clr t !m !p;
      p := !p + 1;
      m := !m + 1
    | Un ->
      m := !m + 1
    | Deux ->
      xchng_clr t !m !g;
      g := !g - 1
  done

let test = [| Un; Un; Zero; Deux; Zero; Deux |]

let _ = tri_drapeau test

(* résultat: 
test;;
- : couleur array = [|Zero; Zero; Un; Un; Deux; Deux|]
*)

(* Comment ça marche:

   L'invariant que maintient l'algorithme est le suivant:

   [0; !p-1] contenant des "Zero"
   [!p; !m-1] contenant des "Un",
   [!m;!g] non trié
   [!g+1, n-1] contenant des "Deux"

   Initialement, p = m = 0, g = n-1 et les zones 
   "Zero", "Un", "Deux" sont vides


   1. Exécution pas à pas sur le tableau "test"

   [...]

   2. Terminaison: à chaque étape de l'algorithme, l'intervalle [!m; !g] décroit

   3. Nb d'échanges 
   pire cas: n (e.g. tableau de Zero), 
   meilleur cas: 0 (tableau de Un)

   4. Nb de comparaisons
   terminaison quand !m + 1 = !g, donc n-2 tour de boucle requis
*)


(** 5. Drapeau Polonais: deux couleurs *)

type blanc_ou_rouge =
  | Blanc
  | Rouge

let xchng_pol (t : blanc_ou_rouge array) (i : int) (j : int) =
  let v = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- v

let tri_drapeau_pol (t : blanc_ou_rouge array) =
  let m = ref 0 in
  let g = ref (Array.length t - 1) in
  while !m <= !g do
    match t.(!m) with
    | Blanc ->
      m := !m +1
    | Rouge ->
      xchng_pol t !m !g;
      g := !g - 1
  done

let test = [| Rouge; Blanc; Rouge; Blanc; Blanc; Rouge; Blanc |]

(* résultat: [|Blanc; Blanc; Blanc; Blanc; Rouge; Rouge; Rouge|] *)

(* Exercice 6: Une seule bulle *)

let bulle (t : int array) =
  let n = Array.length t in
  for i = 0 to n - 2 do
    if t.(i) > t.(i + 1) then
      xchng t i (i + 1)
  done

let t1 = [| 4; 1; 2; 3 |]

let t2 = [| 2; 1; 4; 3 |]

let t3 = [| 4; 2; 1; 3 |]

(* Condition suffisante pour que le tableau soit trié:

   1. tableau vide
   2. tableau avec une seule valeur
   3. permutation se décompose en produit de transpositions élémentaires ou identité
      de la forme
      sigma = (t_23 ou id) o (t_12 ou id) o (t_01 ou id)

   -> cond nécéssaire et suffisante
*)

