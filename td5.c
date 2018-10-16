#include <stdio.h>

/* Pour changer, on va implémenter les algorithmes en langage C.
   La syntaxe est similaire à Java mais le langage propose moins
   de facilités: la gestion de la mémoire doit être effectuée à la main.
   Cela ne devrait pas poser de pb pour ce td.
*/



/* Algo de recherche dichotomique récursif vu en cours

rechDicho(T: tableau trié de n éléments,
          x: élément comparable avec tout élément de T,
          deb: entier, fin: entier){
    if deb <= fin {
      m <- (deb + fin) / 2;

      if x = t[m] { return m }
      if x > t[m] { return rechDicho(T,x,m+1,fin) }
      if x < t[m] { return rechDicho(T,x,deb,m-1) }
    }
    return -1
}
*/


/* Implémentation récursive de l'algo de recherche dichotomique en langage C.
   On travaille sur des tableaux d'entiers mais comme d'habitude, l'algo
   fonctionne avec n'importe quel type d'élément qui dispose d'une relation
   d'ordre qui est totale. */

/* On recherche l'élément x dans l'intervalle d'indices [deb, fin] inclus.
   Pour rechercher sur tout le tableau, il faut donc
   appeller rechDicho(t, x, 0, n-1).
 */
int rechDicho(int t[],
              int x,
              int deb, int fin)
{
     /* si l'intervalle est non vide on examine la situation 
        plus finement */
     if(deb <= fin) {
          
          /* calcul du milieu de l'intervalle */
          int m = (deb + fin) / 2;
          
          /* est-ce que x est à la position m? */
          if(x == t[m]) {
               /* si oui, on retourne donc m  */
               return m;
          }
          
          /* Si on est là on sait que x != t[m].
             Si x > t[m], puisque le tableau est trié, alors  x ne peut que se
             trouver dans l'intervalle entre m+1 et fin. On poursuit
             la recherche dans cet intervalle. Si il n'y est pas
             alors l'élément x n'est pas dans le tableau. */
          if(x > t[m]) {
               return rechDicho(t, x, m+1, fin);
          }

          /* Ici, on sait que x < t[m], le test ci dessous est donc
             redondant. Puisque le tableau est trié, l'élément doit
             se trouver dans l'intervalle [deb, m-1]. */

          if(x < t[m]) {
               return rechDicho(t, x, deb, m-1);
          }
     }

     /* On arrive ici dans deux cas:
        1. soit deb > fin auquel cas l'intervalle dans lequel on recherche
           est vide;
        2. soit deb <= fin auquel cas on a effectué la recherche dichotomique
           dans l'intervalle [deb;fin] et l'élément n'a pas été trouvé.
      */
     return -1;
}


/* L'algo de recherche dichotomique itérative présenté en cours
dicho_it(T : tableau trié d’elts, x : elt comparable avec les elts):
  deb <- 0
  fin <- n-1
  while deb <= fin {
    m <- (deb+fin)/2
    if x = T[m] { return m }
    if x > T[m] { deb <- m+1}
    if x < T[m] { fin <- m-1}
  }
  return -1
*/

int dicho_it(int t[], int n, int x)
{
     // n est la taille du tableau t.
     int deb = 0;
     int fin = n-1;
     
     while(deb <= fin) {
          int m = (deb + fin) / 2;
          
          if(x == t[m]) { 
               return m; 
          }
          
          if(x > t[m]) { 
               deb = m+1; 
          }

          if(x < t[m]) {
               fin = m-1;
          }
     }

     // si on ne trouve pas l'élément, on retourne -1
     return -1;
}



/* -------------------------------------------------------------------------- */

/* Exercice 1 */

/* 1.1: 

   a) exécuter rechDicho sur t = [1,2,42,57,99]

   Appel initial: rechDicho(t, 42, 0, 4)
   x   = 42
   deb = 0,
   fin = 4

   test (deb <= fin) : true
   
   m = (0 + 4) / 2 = 2

   test (x == t[m]) : true (car x = 42 et t[m] = t[2] = 42)
   -> return 2

   Nombre de comparaisons: 2 tests


   b) exécuter rechDicho sur t = [1,2,3,4,42]

   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Appel initial: rechDicho(t, 42, 0, 4)
   x   = 42
   deb = 0,
   fin = 4

   test (deb <= fin) : true
   
   m = (0 + 4) / 2 = 2

   test (x == t[m]) : false (car x = 42 et t[m] = t[2] = 3)
   
   test (x > t[m]) : true

     return rechDicho(t, 42, 3, 4)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     Appel récursif: rechDicho(t, 42, 3, 4)

     x   = 42
     deb = 3
     fin = 4

     test (deb <= fin) : true

     m = (3+4) / 2 = 3 (division entière)
     
     test (x == t[m]) : false (car x = 42 et t[m] = t[3] = 4)

     test (x > t[m]) : true

       return rechDicho(t, 42, 4, 4)
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       Appel récursif: rechDicho(t, 42, 4, 4)

       x   = 42
       deb = 4
       fin = 4

       test (deb <= fin) : true

       m = (4 + 4) / 2 = 4

       test (x == t[m]) : true (car x = 42 et t[m] = t[4] = 42)
       -> return 4

       Fin de l'appel récursif
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       rechDicho(t, 42, 4, 4) = 4

     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     rechDicho(t, 42, 3, 4) = 4

   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   rechDicho(t, 42, 0, 4) = 4

   Nombre de comparaisons: 8 tests


   c) exécuter rechDicho sur t = [1,2,3,57,99]

   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Appel initial: rechDicho(t, 42, 0, 4)
   x   = 42
   deb = 0,
   fin = 4

   test (deb <= fin) : true
   
   m = (0 + 4) / 2 = 2

   test (x == t[m]) : false (car x = 42 et t[m] = t[2] = 3)
   
   test (x > t[m]) : true

     return rechDicho(t, 42, 3, 4)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     Appel récursif: rechDicho(t, 42, 3, 4)

     x   = 42
     deb = 3
     fin = 4

     test (deb <= fin) : true

     m = (3+4) / 2 = 3 (division entière)
     
     test (x == t[m]) : false (car x = 42 et t[m] = t[3] = 57)

     test (x > t[m]) : false

     test (x < t[m]) : true
       return rechDicho(t, 42, 3, 2)
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       Appel récursif: rechDicho(t, 42, 3, 2)

       x   = 42
       deb = 3
       fin = 2

       test (deb <= fin) : false

       -> return -1
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       rechDicho(t, 42, 3, 2) = -1

     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     rechDicho(t, 42, 3, 4) = -1

   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   rechDicho(t, 42, 0, 4) = -1

   Nombre de comparaisons: 8 tests

1.2:

   Dans tous les cas qui suivent, n = 5

   a) exécuter dicho_it sur t = [1,2,42,57,99]

   dicho_it(t, n, 42)

   deb = 0
   fin = n-1

   test(deb <= fin) : true (0 <= n-1)

   // première itération de la boucle

   m = (deb + fin) / 2 = (n-1) / 2 = 2

   test (x == t[m]) : true (car x = 42 et t[m] = t[2] = 42)
    -> return m = 2
    
   Nombre de comparaisons: 2 tests



   b) exécuter dicho_it sur t = [1,2,3,4,42]

   dicho_it(t, n, 42)

   deb = 0
   fin = n-1

   test(deb <= fin) : true (0 <= n-1)
   
   // première itération de la boucle

   m = (deb + fin) / 2 = (n-1) / 2 = 2

   test (x == t[m]) : false (car x = 42 et t[m] = 3)

   test (x > t[m]) : true

   deb = m+1 = 3

   test(deb <= fin) : true (3 <= n-1)
   
   // seconde itération de la boucle

   m = (deb + fin) / 2 = (n-1 + 3) / 2 = 3

   test (x == t[m]) : false (car x = 42 et t[m] = 4)

   test (x > t[m]) : true

   deb = m+1 = 4

   test(deb <= fin) : true (4 <= n-1)

   // troisième itération de la boucle

   m = (deb + fin) / 2 = (n-1 + 4) / 2 = 4

   test (x == t[m]) : true (car x = 42 et t[m] = 42)
   -> return m = 4

   Nombre de comparaisons: 8 tests


   c) exécuter dicho_it sur t = [1,2,3,57,99]
   dicho_it(t, n, 42)

   deb = 0
   fin = n-1

   test(deb <= fin) : true (0 <= n-1)
   
   // première itération de la boucle

   m = (deb + fin) / 2 = (n-1) / 2 = 2

   test (x == t[m]) : false (car x = 42 et t[m] = 3)

   test (x > t[m]) : true

   deb = m+1 = 3

   test(deb <= fin) : true (3 <= n-1)
   
   // seconde itération de la boucle

   m = (deb + fin) / 2 = (n-1 + 3) / 2 = 3

   test (x == t[m]) : false (car x = 42 et t[m] = 57)

   test (x > t[m]) : false

   test (x > t[m]) : true

   fin = m-1 = 2

   test(deb <= fin) : false (deb = 3, fin = 2)

   -> return - 1

   Nombre de comparaisons: 8 tests
   
*/


/* Exercice 3, recherche linéaire et dichotomie */

/* On rappelle l'algo de recherche linéaire */

int recherche_lineaire(int t[], int n, int x)
{
     for(int i = 0; i < n; i++) {
          if(t[i] == x)
               return i;
     }

     return -1;
}


/* 
   1.1: Nombre de tests d'égalités dans le pire cas ?

   Si le tableau ne contient pas l'élément recherché, n tests d'égalité. Il s'agit du pire cas.

   1.2: Nombre de tests d'égalité en moyenne lorsque l'élément recherché est trouvé ?

   Supposons que la position de l'élément recherché soit distribuée uniformément entre
   0 et n-1. Le nombre de comparaisons en moyenne est:

   1/n (1 + ... + n) = 1/n * n * (n+1)/2 = (n+1)/2


   1.3 Soit t un tableau de taille n. On veut y effectuer m recherches soit
       linéaires soit dichotomiques et compter le nombre de comparaisons/tests d'égalités
       dans le pire cas.
       
       Quelques résultats préliminaires:
       . on a vu que pour la recherche linéaire, le nombre de comparaisons et de tests est
       de l'ordre de la taille du tableau, i.e. de l'ordre de n.
       . pour la recherche dichotomique, le nombre de tests est de l'ordre de log(n),
       car à chaque étape on fait un nombre constant de tests puis on coupe la taille du tableau en 2
       avant de répéter.
       

   a) si on effectue m recherches linéaires, dans le pire cas, on a un cout C en nb de tests
      de l'ordre de m * n  (i.e. C_a(m, n) = m * n)

   b) si on effectue un tri par insertion suivi de m recherches dichotomiques, on paye le cout préalable
   du tri par insertion, de l'ordre de n^2 tests, multiplié par le cout de m recherches dichotomiques.
   Le coût total est donc C_b(m,n) = n^2 + m * log(n)

   c) si à la place d'un tri inefficace comme le tri par insertion on utilise un tri avec une 
   complexité de l'ordre de n * log(n), alors le cout total est C_c(m,n) = log(n) + m * log(n).

   Afin de se faire une idée des mérites respectifs de ces approches, il est utile de se placer
   dans le cas ou n est très grand par rapport à n. Supposons que la longueur du tableau soit
   égale à n = 2^64 - 1. C'est l'entier le plus grand représentable dans un mot sur une machine
   64 bits et en pratique on traite toujours des tableaux beaucoup, beaucoup, /beaucoup/ plus
   petits. Donc c'est une bonne approximation de l'infini du point de vue de l'ordinateur.

   Si on prend un logarithme à base 2, alors log(2^64) = 64. Les formules se simplifient ainsi:

   dans le cas a), le coût est C(m) = m * 2^64 (j'oublie le -1 volontairement pour simplifier)
   dans le cas b), le coût est C(m) = 2^128 + m * 64
   dans le cas c), le coût est C(m) = 2^64 * 64 + m * 64

   Donc clairement, quand n est très grand et m est plus grand que 64 (e.g. m = 1000), la solution c est 
   meilleure que la solution a qui est meilleure  que la solution b

   Faites le même raisonnement quand m est très grand par rapport à n chez vous.

*/

/* Exercice 2: point fixe

   Dans la suite, on considère un tableau T *trié*

   1. 
         0; 1; 2; 3; 4; 5
   T = [-1; 0; 1; 3; 4; 8]
   
   pré-points fixes: indices 0, 1, 2, 3, 4
   post-points fixes: 3,4 5
   points fixes: 3, 4

   On appel un pré/post point fixe "strict" si ce n'est pas un point fixe.

   2. propriété des pré et post-points fixes /stricts/ dans un tableau trié où tous les
      éléments sont distincts (hypothèse cruciale):
      les pré-points fixes sont avant les post-points fixes. Preuve par l'absurde:

      supposons deux indices i < j t.q. j soit un pré-point fixe strict et i un post-point fixe strict.
      Alors i < T[i] <= T[j] < j. Cela signifie que les éléments entre i et j prennent un nombre
      de valeurs strictement inférieur à j-i, ce qui contredit l'hypothèse que les éléments sont 
      tous distincts.

      On vient de prouver que les pré-points fixes sont avant les post-points fixes.
      En fait, on a une propriété plus forte: la quantité (T[i] - i) est croissante,
      i.e. le tableau S[i] = T[i] - i est trié.
      Supposons que ce ne soit pas le cas: alors il y a un indice k tel que
          T[k+1] - (k+1) < T[k] - k
      <=> T[k+1] - T[k] < 1
      Or, T est trié, donc par ailleurs T[k+1] - T[k] >= 0
      donc T[k+1] - T[k] = 0
      ce qui contredit l'hypothèse que tous les éléments sont distincts.

      Un point fixe correspond à un indice i tel que S[i] = 0. Puisque S est trié (comme nous venons
      de le prouver) il suffit d'utiliser l'algo de recherche dichotomique sur S.
      Complexité log(n) dans le pire cas!
*/

/* Exercice 4: pas de correction pour l'instant ... */





int main(int argc, char** argv)
{
     
     int tableau[] = { 1, 2, 42, 57, 99 };
     int n         = 5;

     printf("%d\n", dicho_it(tableau, n, 2));

     return 0;
}
