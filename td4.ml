(* 
Rappels de syntaxe OCaml préliminaires
1. fonction non récursive 'somme1' qui calcule la somme d'un tableau d'entiers
*)

let somme1 (t : int array) =
   let n = Array.length t in
   let acc = ref 0 in
   for i = 0 to n - 1 do
      acc := !acc + t.(i)
   done;
   !acc

(*
2. première variante avec récursion
   on a une fonction récursive 'boucle' qui implémente la boucle
   la récursion n'est PAS terminale !

   Noter qu'on utilise "let rec" pour définir une fonction récursive.
*)

let rec boucle (t : int array) (i : int) =
   if i = Array.length t then
      0
   else
      t.(i) + (boucle t (i+1))

let somme_rec (t : int array) =
  boucle t 0

(*
3. La même chose, cette fois çi avec une récursion terminale. Noter qu'on a
   un argument en plus, l'accumulateur.
*)

let rec boucle_term (t : int array) (i : int) (acc : int) =
   if i = Array.length t then
      acc
   else
     boucle_term t (i+1) (acc + t.(i))

(* l'accumulateur joue le role de la variable "acc" dans la version 
   itérative. *)

let somme_rec_term (t : int array) =
  boucle_term t 0 0

(* -------------------------------------------------------------------------- *)
(* Début du TD 4 *)

(* Exercice 1 

   On a une suite définie par
   a(0) = 0
   a(n) = 3 + a(n-1)        (pour n > 0)

   1. Donner une forme close pour a(n)
   
   On regarde les premiers éléments de la suite :
   a(0) = 0
   a(1) = 3 + a(0) = 3
   a(2) = 3 + a(1) = 3 + (3 + a(0)) = 6
   
   Il semble que la forme close soit a(n) = 3 * n
   Prouvons par récurrence la proposition "pour tout n, a(n) = 3 * n"

   . Cas de base, n = 0.
     Il faut prouver a(0) = 3 * 0 = 0. C'est le cas par hypothèse.

   . Cas inductif, n = m + 1. L'hypothèse de récurrence est "a(m) = 3 * m".
     Il faut prouver a(n) = 3 * n.

     Par hypothèse, n = m + 1, donc on a que a(n) = a(m+1).
     Par définition de la suite, a(m+1) = 3 + a(m)
     On a par hypothèse de récurrence que a(m) = 3 * m, donc:
     a(n) = a(m+1) = 3 + a(m) = 3 + 3 * m
     En simplifiant, on obtient
     a(n) = 3 * (m + 1)
     Or, n = m + 1, donc
     a(n) = 3 * n

   1.2. Ecrire une fonction récursive qui calcule a(n)

   a(n):
     if n > 0 then
        3 + a(n-1)
     else
        0
*)
let rec a (n : int) =
  if n > 0 then
    3 + a (n-1)
  else
    0

(* Cette récursion est elle terminale? non car le résultat de l'appel récursif
   est utilisé dans le contexte d'une addition. *)

(* 1.3. Version récursive terminale:

   a'(n, acc):
     if n > 0 then
        a'(n-1, acc + 3)
     else
        acc

   a(n):
     a'(n, 0)
*)

let rec a' (n : int) (acc : int) =
  if n > 0 then
    a' (n-1) (acc + 3)
  else
    acc

let a n = a' n 0

(* 1.4. version iterative:
   
   a(n):
    acc = 0
    for i = 1 to n do
      acc += 3
    done;
    return acc
*)

let a n =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + 3
  done;
  !acc


(* Exercice 2, récursivité et état *)

(* implémentation de H en OCaml *)

let rec h (n : int) =
  if n > 0 then
    begin
      h (n / 10);
      Printf.printf "%d" (n mod 10)
    end

(* 2.1 Appels récursifs sur n = 123 
   
   h(n = 123)
   123 > 0
      h(123 / 10 = 12)
      12 > 0
         h(12 / 10 = 1)
         1 > 0
             h(1 / 10 = 0)
         affiche(1 mod 10) -> '1'
      affiche(12 mod 10) -> '2'
   affiche(123 mod 10) -> '3'
*)

(* 2.2 Que fait cette fonction: décomposer son argument en base 10 
   Rappel: tout nombre n s'écrit en base 10 comme
   
   somme_i (n_i * 10^i) avec n_i entre 0 et 9

   e.g. 765233 s'écrit
   7 * 10^5 + 6 * 10^4 + 5 * 10^3 + 2 * 10^2 + 3 * 10^1 + 3 * 10^0

   Est-elle récursive terminale? Non: l'appel récursif n'est pas en position
   finale.
*)

(* 2.3 Version itérative 

   h_iter(n):

   taille = 0
   x = n

   while (x > 0) {
     x = x / 10
     taille = taille + 1
   }

   chiffres = alloc_tableau(int, taille)
   x = n

   for exposant = 0 to taille - 1 {
     chiffres(exposant) = x mod 10
     x = x / 10
   }
   
   for i = taille-1 downto 0 {
     affiche(char(chiffres(i)))
   }
   
*)

let h_iter (n : int) =
  let x      = ref n in
  let stack  = Stack.create () in
  while !x > 0 do
    Stack.push !x stack;
    x := !x / 10;
  done;
  while not (Stack.is_empty stack) do
    let v = Stack.pop stack in
    Printf.printf "%d" (v mod 10)
  done

(* Exercice 3: suite de Fibonacci 
   
   f(0) = 1
   f(1) = 1

   f(n) = f(n-2) + f(n-1)    (n >= 2)


   3.1 Implémentation récursive naive

   fibo(n) =
     if n = 0 then
       1
     else if n = 1 then
       1
     else
       fibo(n-1) + fibo(n-2)
     end
*)

let rec fibo0 n =
  if n = 0 then
    1
  else if n = 1 then
    1
  else
    fibo0 (n-1) + fibo0 (n-2)

(* "fibo0 40" prend quelque secondes à s'exécuter sur ma machine. *)

(*
   3.2 Appels récursifs

   f(4)
   = f(3) + f(2)
   = (f(2) + f(1)) + (f(1) + f(0))
   = ((f(1) + f(0)) + f(1)) + (f(1) + f(0))
   = 5

   nb additions: 4

   3.3 Formule de récurrence du _nombre d'additions_?

   nombre d'additions en fonction de n:

   f(n) = f(n-1) + f(n-2)

   donc le nombre d'additions pour calculer f(n) =
       nb d'addtions pour calculer f(n-1) +
       nb d'additions pour caculer f(n-2) +
       1

   donc a(n) = a(n-1) + a(n-2) + 1
   
   a(0) = 0
   a(1) = 0
   a(2) = 1 car
     f(2) = f(0) + f(1)
   a(3) = 2 car
     f(3) = f(2) + f(1)
     donc
     a(3) = a(2) + a(1) + 1
          = 1 + 0 + 1
   a(4) = ?
     f(4) = f(3) + f(2)
     donc
     a(4) = a(3) + a(2) + 1
          = 2    + 1    + 1
          = 4
   a(5) = a(4) + a(3) + 1
        = 4 + 2 + 1
        = 6
   a(6) = a(5) + a(4) + 1
        = 6 + 4 + 1
        = 11
*)

(* 3.4: algo itératif. 

fibo_iter(n):

  fn_1 = 1
  fn_2 = 1

  i = 2

  while(i <= n) {

    tmp  = fn_1
    fn_1 = fn_1 + fn_2
    fn_2 = tmp

    i = i + 1
  }

  return fn_1
*)

let fibo_iter (n : int) =

  let fn_1 = ref 1 in
  let fn_2 = ref 1 in
  let i    = ref 2 in

  while !i <= n do
    
    let tmp = !fn_1 in

    fn_1 := !fn_1 + !fn_2;

    fn_2 := tmp;

    i := !i + 1
    
  done;

  !fn_1

(* Une version récursive plus intelligente. fibo(n) retourne
   une paire (fibo(n-1), fibo(n-2)). On évite les calculs dupliqués.
*)
let rec fibo1 n =
  if n = 0 then
    (1, 0)
  else if n = 1 then
    (1, 1)
  else
    let fibo_n_1, fibo_n_2 = fibo1 (n-1) in
    (fibo_n_1 + fibo_n_2, fibo_n_1)

(* fibo1 s'exécute de manière instantanée! on économise bcp de calculs par rapport
   à fibo0. *)

(* /!\ Attention, materiel un peu avancé, pas indispensable /!\
   fibo2 ci dessous s'exécute également très rapidement. Noter que la récursion de fibo_aux
   est terminale. Cette fonction est un peu compliquée à comprendre: j'ai converti
   la version de fibo1, dont la récursion est non terminale, en une fonction
   avec récursion terminale, via une transformation "continuation passing style"
   (googlez ce terme). L'argument "k" de fibo_aux correspond à la suite du calcul
   après l'appel récursif. Appeller "fibo_aux n k" correspond à calculer
   f(n) et f(n-1) et à passer la paire (f(n), f(n-1)) en argument à la fonction k.
   
   Plus intuitivement, j'ai transformé fibo1 en fibo_aux en convertissant la pile
   d'exécution dans le 2ème argument de fibo_aux.
*)   

let rec fibo_aux n k =
  if n = 0 then
    k (1, 0)
  else if n = 1 then
    k (1, 1)
  else
    fibo_aux (n - 1) (fun (fn1, fn2) -> k (fn1 + fn2, fn1))
(* la fonction "(fun (fn1, fn2) -> k (fn1 + fn2, fn1))" correspond à la suite du calcul,
   (c'est pourquoi on appelle ça une continuation). 
   Intuitivement lorsque fibo_aux (n-1) a fini son calcul, le résultat est passé à cette
   continuation.
*)

(* pour calculer fibo, on passe comme continuation la fonction qui sélectionne
   le premier élément de la paire (x, y) *)
let fibo2 n = fibo_aux n (fun (x, y) -> x)




(* Exercice 4. Ackermann. *)

let rec ackermann m n =
  if m = 0 then
    n + 1
  else if n = 0 then
    ackermann (m-1) 1
  else
    ackermann (m-1) (ackermann m (n-1))

(* 4.1

   ackermann 0 n = n + 1

   -----------
   
   ackermann 1 n = ackermann 0 (ackermann 1 (n-1))
                 = (ackermann 1 (n-1)) + 1
   -> 
   ackermann 1 n = (ackermann 1 0) + n
                 = (ackermann 0 1) + n
                 = 2 + n

   -----------

   ackermann 2 3 = ackermann 1 (ackermann 2 2)
                 = ackermann 1 (ackermann 1 (ackermann 2 1))
                 = ackermann 1 (ackermann 1 (ackermann 1 (ackermann 2 0)))

   ackermann 2 n = ackermann 1 (ackermann 2 (n-1))
                 = ackermann 1 (... (ackermann 1 (ackermann 2 0))) (avec n fois ackermann 1 ...)

   or ackermann 2 0 = ackerman 1 1 = 3 ->
   ackermann 2 n = ackermann 1 (.. (ackermann 1 3))
   
   donc on applique n fois la fonction ackermann 1 x = 2 + x à 3
   ackermann 2 n = 2 n + 3

   -----------

   ackermann 3 n = ackermann 2 (ackermann 3 (n - 1))
                 = ackermann 2 (... (ackermann 2 (ackermann 3 0))) (avec n fois ackermann 2)

   or ackermann 3 0 = ackermann 2 1 = 2 + 3 = 5

   donc ackermann 3 n = 2 (2 (2 n + 3) + 3) + 3 [... emboités n fois]

   4.2 ackermann termine car l'appel récursif se produit sur une paire d'arguments 
       qui décroit dans l'ordre lexicogrraphique

   4.3 bonne chance: d'après wikipedia, ackermann 4 2 est un nombre avec environ 20000 chiffres.

   ackermann 10 10 doit largement dépasser le nombre d'atomes dans l'univers.
*)
