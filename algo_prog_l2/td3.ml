(* Tri par insertion *)

let tri_insertion (t : int array) =
  let n = Array.length t in
  for i = 1 to n - 1 do
    let v = t.(i) in
    let j = ref (i - 1) in
    (* invariant:
       les éléments de t entre [0,i-1] = [0,j] sont triés *)
    while !j >= 0 && v < t.(!j) do 
      t.(!j+1) <- t.(!j); (* on décale d'un cran vers la droite *)
      j := !j - 1
      (* . les éléments entre [0,j] sont inchangés (donc triés)
         . t.(j+2) = t.(j+1)
         . les éléments entre [j+2,i] sont triés et > v
           ce sont les éléments d'origine décalés d'un cran vers la droite
      *)
    done;
    (* invariant:
       . les éléments entre [0,j] sont inchangés (donc triés) et <= v
           pourquoi: on hérite de l'invariant de fin de boucle
           + on sait que t(j) <= v à cause de la condition de fin de boucle.
           dans le cas ou j < 0, alors l'intervalle [0,j] est vide donc l'invariant
           est également trivialement vrai.
       . les éléments entre [j+2,i] sont triés et > v
    *)
    t.(!j+1) <- v
    (* invariant: les éléments entre [0,i] sont triés 
                  les éléments sont une permutation des éléments d'origine:
                  si le tableau au début est [x0, ..., xn-1] alors en fin de boucle on a:
                  [ x0, ..., xj, xi=v, xj+1, xi-1, xi+1, xn-1]
    *)
  done

(* application 
   t = [8,4,10,-5,1]
*)


(* Exercice 2: tri par insertion selon la parité *)

(* On définit un nouvel ordre total sur les entiers *)

let pair (n : int) = n mod 2 = 0

let less_than (n1 : int) (n2 : int) =
  let n1_pair = pair n1 in
  let n2_pair = pair n2 in
  if n1_pair then
    if n2_pair then
      n1 < n2
    else
      true
  else if not n2_pair then
    n1 < n2
  else
    false

let tri_insertion_exo2 (t : int array) =
  let n = Array.length t in
  for i = 1 to n - 1 do
    let v = t.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && less_than v t.(!j) do 
      t.(!j+1) <- t.(!j);
      j := !j - 1
    done;
    t.(!j+1) <- v
  done
(*    
utop[4]> let t = [| 2; 1; 6; 8; 5; -3 |];;
val t : int array = [|2; 1; 6; 8; 5; -3|]
utop[5]> tri_insertion_exo2 t;;
- : unit = ()
utop[6]> t;;
- : int array = [|2; 6; 8; -3; 1; 5|]
utop[7]> 

*)

(* Exercice 3 *)

(* Une carte est une paire (couleur, rang) 
   couleur = pique, carreau, coeur, trefle
   codage des couleurs:
     1 = pique
     2 = carreau
     3 = coeur
     4 = trefle

   ordre sur les couleurs: pique < carreau < coeur < trefle
   ordre insuit sur le code: 1 < 2 < 3 < 4
   => coincide avec celui des entiers

   rang = 2, 3, 4, 5, 6, 7, 8, 9, 10, valet, dame, roi, as
   codage du rang:
     1 = as
     2 = 2
     ...
     10 = 10
     11 = valet
     12 = dame
     13 = roi

   ordre sur le rang: 2 < 3 < ... < 10 < valet < dame < roi < as
   ordre induit sur le code: 2 < 3 < ... < 10 < 11 < 12 < 13 < 1
   => Attention, ne coincide pas avec celui des entiers!
      On note que pour les rangs entre 2 et 13, il s'agit de l'ordre 
      naturel sur les entiers
*)

let less_than_couleur (couleur1 : int) (couleur2 : int) =
  couleur1 < couleur2 (* on réutilise l'ordre sur les entiers pour la couelur *)

let less_than_rang (rang1 : int) (rang2 : int) =
  if rang1 != 1 && rang2 != 1 then
    (* si les rangs sont différents de l'as, alors on peut juste utiliser
       l'ordre sur les entiers *)
    rang1 < rang2
  else 
    (* soit rang1 = as, soit rang2 = as, soit les deux *)
     if rang1 != 1 then
      (* rang1 != as donc rang2 = as, donc rang1 < rang2 *)
      true
    else
      (* rang1 = as, donc on ne peut pas avoir rang1 < rang2 *)
      false

(* une carte est un tableau d'entiers de taille 2 *)
type carte = int array

let less_than_carte (carte1 : carte) (carte2 : carte) =
  let couleur1 = carte1.(0) in
  let couleur2 = carte2.(0) in
  let rang1    = carte1.(1) in
  let rang2    = carte2.(1) in
  if couleur1 = couleur2 then
    less_than_rang rang1 rang2
  else
    less_than_couleur couleur1 couleur2

let tri_insertion_exo3 (t : int array array) =
  let n = Array.length t in
  for i = 1 to n - 1 do
    let v = t.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && less_than_carte v t.(!j) do 
      t.(!j+1) <- t.(!j);
      j := !j - 1
    done;
    t.(!j+1) <- v
  done

(* Exo 4: Tri stable *)

type etudiant = {
  nom  : string;
  note : int
}

let less_than_or_equal_etudiant (e1 : etudiant) (e2 : etudiant) =
  e1.nom <= e2.nom

(* Ceci ne définit pas un ordre total:
   Par exemple, e1 = { nom = "A"; note = 10 }
                e2 = { nom = "A"; note = 12 }
   sont équivalents pour cette relation d'ordre,
   car e1 <= e2 et e2 <= e1. 

   Un tri est stable si il préserve l'ordre relatif des éléments dans le tableau.
   C'est à dire que si e1 apparait avant e2 dans le tableau avant le tri, alors
   e1 apparait avant e2 dans le tableau /après/ le tri.
   
   Le tri par insertion est stable: soit t un tableau et t.(i1), t.(i2) deux éléments
   équivalents avec i1 < i2. Appliquons l'algo de tri par insertion jusqu'à i = i2.
   L'invariant de fin de boucle garantit que t.(i2) va être inséré au plus grand
   indice j <= i2 t.q. t.(j) < t.(k) pour k 
*)

(* Exercice 5 *)

let exchange (t : int array) (i : int) (j : int) =
  let v = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- v

let fonc (t : int array) =
  let n = Array.length t in
  let i = ref 0 in
  while !i < n-1 do
    if t.(!i) > t.(!i+1) then
      (exchange t !i (!i+1);
       i := 0)
    else
      i := !i + 1
  done

(* que fait fonc? 
   Exécution sur t = [3;1;2;0]
   
   i = 0

   0 < n-1 ? oui
     t.(0) = 3 > t.(1) = 1 ? oui
       échange t.(0) et t.(1) -> t = [1;3;2;0]
       i = 0

   0 < n-1 ? oui
     t.(0) = 1 > t.(1) = 3 ? non
       i = i+1

   1 < n-1 ? oui
     t.(1) = 3 > t.(2) = 2 ? oui
       échange t.(1) et t.(2) -> t = [1;2;3;0]
       i = 0

   0 < n-1 ? oui
     t.(0) = 1 > t.(1) = 2 ? non
       i = i+1

   1 < n-1 ? oui
     t.(1) = 2 > t.(2) = 3 ? non
       i = i+1

   2 < n-1 ? oui
     t.(2) = 3 > t.(3) = 0 ? oui
       échange t.(2) et t.(3) -> t = [1;2;0;3]
       i = 0

   0 < n-1 ? oui
     t.(0) = 1 > t.(1) = 2 ? non
       i = i+1

   1 < n-1 ? oui
     t.(1) = 2 > t.(2) = 0 ? oui
       échange t.(1) et t.(2) -> t = [1;0;2;3]
       i = 0

   0 < n-1 ? oui
     t.(0) = 1 > t.(1) = 0 ? oui
       échange t.(0) et t.(1) -> t = [0;1;2;3]
       i = 0

   0 < n-1 ? oui
     t.(0) = 0 > t.(1) = 1 ? non
       i = i+1

   1 < n-1 ? oui
     t.(1) = 1 > t.(2) = 2 ? non
       i = i+1

   2 < n-1 ? oui
     t.(2) = 2 > t.(3) = 3 ? non
       i = i+1

   3 < n-1 ? non

   stop
   résultat: t = [0;1;2;3]


   La fonction "fonc" implémente l'algorithme suivant:
     rechercher i t.q. t.(i) > t.(i+1) et transposer ces éléments
     si i t.q. t.(i) > t.(i+1) a été trouvé, recommencer

   Cet algorithme termine lorsque il n'existe pas d'éléments adjacents mal ordonnés,
   i.e. lorsque le tableau est trié.

   Pourquoi cet algorithme termine:
   Si un tableau n'est pas trié, il existe au moins une paire d'éléments adjacents
   dans le mauvais ordre. A chaque étape, on réduit le nombre de paires 
   d'éléments dans le mauvais ordre de 1.
  
*)

let fusion (t : int array) =
  (* trouver le point de séparation k entre les deux tableaux *)
  let n = Array.length t in
  let k = ref 0 in
  while !k < n-1 && t.(!k) <= t.(!k+1) do
    k := !k + 1
  done;
  (* ici:
     soit !k = n-1 et le tableau est trié, 
     soit !k < n-1 et:
       le tableau entre 0 et !k (inclus) est trié (on vient de le vérifier)
       le tableau entre !k+1 et n-1 (inclus) est trié (par hypothèse, t = deux tableaux triés collés )
  *)
  if !k < n - 1 then
    begin
      (* on applique le tri par insertion à partir de l'indice !k+1 *)
      for i = !k+1 to n - 1 do
        let v = t.(i) in
        let j = ref (i - 1) in
        while !j >= 0 && v < t.(!j) do 
          t.(!j+1) <- t.(!j); (* o ndécale d'un cran vers la droite *)
          j := !j - 1
        done;
        t.(!j+1) <- v
      done
    end

(* nombre de comparaisons dans le pire des cas: si le tableau est par exemple
   t = [| 9; 1; 2; 3; 4; 5; 6; 7; 8; |], alors le tableau initial est de longueur 1
   et on a O(n) comparaisons
   si le tableau est de la forme
   t = [| 2; 3; 4; 5; 6; 7; 8; 9; 2 |] alors on a encore O(n) comparaison
   si le tableau est de la forme
   t = [| 5; 6; 7; 8; 9; 1; 2; 3; 4 |] alors on a de l'ordre de n/2 comparaisons pour trouver k
   + n/2 comparaisons par élément dans la 2ème partie du tableau [| ...; 1; 2; 3; 4 |]
   i.e. de l'ordre de n^2 comparaisons. 
*)  

