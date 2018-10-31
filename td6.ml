(* Retour à OCaml pour le TD 6 *)

(* Exercice 1: division entière *)

(* Rappel sur la division entière de n par m, dénotée n/m dans la suite. Par définition, on a:

   n = n/m * m + n mod m

*)

(* Algo récursif de division entière *)

let rec division (n : int) (m : int) =
  if n >= m then
    1 + division (n - m) m
  else
    0

(* Preuve de terminaison. 
   On suppose n >= 0, m > 0.
   L'algorithme termine quand n < m. Or, à chaque appel récursif, le premier argument
   passe de n à n - m, i.e. il décroit strictement. Donc après un nombre fini d'appels
   récursifs, on a que n < m.


   Preuve de correction.
   Il faut prouver que pour tout n >= 0, pour tout m > 0, on a l'égalité
     
   division n m = n/m

   En utilisant la définition de division entière, on peut réécrire

   division n m = division (n/m * m + n mod m) m

   Donc il faut prouver que

   division (n/m * m + n mod m) m = n/m
   
   On fait une preuve par recurrence sur n/m. 

   . Cas de base. n/m = 0. Il faut montrer que

   division (n mod m) m = 0. Puisque n mod m < m, c'est le cas.

   . Cas de recurrence. n/m = x + 1. L'hypothèse de recurrence est:
   
   division (x * m + n mod m) m = x

   A l'aide de cette hypothèse, il faut montrer que

   division ((x+1) * m + n mod m) m = x+1.

   Par définiton de la division entière:
   n = n/m * m + n mod m
   En remplaçant n/m par x+1:
   n = (x + 1) * m + n mod m
     = x * m + m + n mod m
   donc
   n - m = x * m + n mod m
   On insère cette égalité dans l'hypothèse de recurrence. On obtient:

   division (n - m) m = x

   Or, par définition de la fonction division, puisque ((x+1) * m + n mod m) >= m, on a:

   division ((x+1) * m + n mod m) m = 1 + division (x * m + n mod m) m
                                    = 1 + division (n - m) m
                                    = 1 + x

   Ce qu'il fallait démontrer.
  
*)

(* le calcul du reste suit facilement *)
let reste (n : int) (m : int) =
  n - (division n m) * m


(* Algo itératif de pgcd. On rappelle:
   d = pgcd n m  si et seulement si
   pour tout d' t.q. n mod d' = 0 et m mod d' = 0, d' <= d.

   Observation 1: si m = k * n, alors le pgcd de m et n est égal à m.

   Propriété des diviseurs (dans l'énoncé): si d est un diviseur de n et m,
   alors d est un diviseur de "reste n m".

   Observation 2: en supposant n >= m, le pgcd de n et m est égal au pgcd de m et "reste n m".

   Observation 3: "reste n m" < m 
                  "reste m (reste n m)" < "reste n m"
                  etc
   -> la suite des restes est strictement décroissante et atteint donc 0 après un nombre
   fini d'étapes, ce qui nous permet d'utiliser l'observation 1. 

   On en déduit l'algorithme suivant.
*)

let min (n : int) (m : int) =
  if n < m then
    n
  else
    m

(* On suppose qu'initialement, n >= m. *)
let pgcd_aux (n : int) (m : int) =
  let x = ref n in
  let y = ref m in
  while not (!x mod !y = 0) do
    let tmp = !x in
    x := !y;
    y := tmp mod !y
  done;
  !y

let pgcd (n : int) (m : int) =
  if n >= m then
    pgcd_aux n m
  else
    pgcd_aux m n


(** Exercice 2: méthode de Horner *)

(* 2.1 *)

(* On considère des polynomes à coefficients entiers. *)
(* On a bien 2n multiplications.*)

(* TD: exo pour se chauffer: implémentation naïve, itérative, de pow *)

(* solution: *)
let eval_poly (t : int array) (x0 : int) =
  let n    = Array.length t in
  let acc  = ref 0 in
  let xpow = ref x0 in
  for i = 0 to n - 1 do
    acc  := !acc + t.(i) * !xpow;
    xpow := !xpow * x0
  done;
  !acc

(* 2.2 version optimisée. En fait, on peut écrire plus clairement que dans
   l'énoncé:

   P(x) = ((... (((0 * x + a(n-1)) x + a(n-2)) x + a(n-3)) x + ...) x + a(1)) x + a0

   i.e. on a un accumulateur initialement à zéro, à chaque étape i (allant de n-1 à 0)
   on multiplie l'accumulateur par x et on additionne a(i).
*)

let eval_poly_opt (t : int array) (x0 : int) =
  let n   = Array.length t in
  let acc = ref 0 in
  for i = n-1 downto 0 do
    acc := !acc * x0 + t.(i)
  done;
  !acc

  
(* 3 Tableau zigzag. *)

let rec zig (t : int array) (i : int) (j : int) =
  i = j || (t.(i) <= t.(i+1) && zag t (i+1) j)
             
and zag (t : int array) (i : int) (j : int) =
  i = j || (t.(i) >= t.(i+1) && zig t (i+1) j)

let zigzag (t : int array) =
  zig t 0 (Array.length t - 1)

(* Exécution à la main de zigzag sur [|1;2;3;4|] *)

(*
      zigzag [|1;2;3;4|]
   =  zig [|1;2;3;4|] 0 3
   =  0 = 3 || 1 <= 2 && zag [|1;2;3;4|] 1 3
   =  false || true && (1 = 3 || 2 >= 3 && zig [|1;2;3;4|] 2 3)
   =  false || true && (false || false &&  zig [|1;2;3;4|] 2 3)
   =  false

      zigzag [|1;3;2;4|]
   =  zig [|1;3;2;4|] 0 3
   =  0 = 3 || 1 <= 3 && zag [|1;3;2;4|] 1 3
   =  false || true && (1 = 3 || 3 >= 2 && zig [|1;3;2;4|] 2 3)
   =  false || true && (1 = 3 || 3 >= 2 && (2 = 3 || 2 <= 4 && (zag [|1;3;2;4|] 3 3)))
   =  false || true && (1 = 3 || 3 >= 2 && (2 = 3 || 2 <= 4 && (3 = 3 || ...)))
   =  false || true && (1 = 3 || 3 >= 2 && (false || true && true))
   =  false || true && (false || true && true)
   =  false || true && true
   =  true
*)

(* 3.2 zigzag calcule si les paires d'éléments consécutifs sont successivement
   croissantes et décroissantes. *)

(* 3.3 appels terminaux si les opérateurs booléens || et && ont une sémantique
   de "court-circuit" évaluée de gauche à droite. 
   Appels non-terminaux sinon.
 *)

(* 3.4 version itérative (sans pile!) *)

let zigzag_iter (t : int array) =
  let n = Array.length t in
  let acc = ref true in
  let i   = ref 0 in
  while !i < n - 2 do
    (if !i mod 2 = 0 then
       acc := !acc && t.(!i) <= t.(!i+1)
     else
       acc := !acc && t.(!i) >= t.(!i+1));
    i := !i +1
  done;
  !acc
