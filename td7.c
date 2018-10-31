#include <stdio.h>
#include <stdlib.h>

// "true" et "false" ne sont pas définis en C.

#define true (1)
#define false (0)

// Définition d'un type de liste chainée en C.
// Inutile de comprendre ça dans le détail pour la compréhension du TD.

// Le langage est plus bas-niveau que Java: pas de garbage collector.
// Mais on va ignorer ce problème.

// -----------------------------------------------------------------------------

// Une cellule contient deux champs: une valeur entière et un pointeur sur la 
// Cellule suivante.
typedef struct Cellule_s {
     int               val;
     struct Cellule_s* next;
} Cellule_t;

typedef Cellule_t* Cellule;

Cellule new_Cellule(int val, Cellule next) 
{
     Cellule c = malloc(sizeof(Cellule_t));

     c->val  = val;
     c->next = next;

     return c;
}

// Normalement il faut écrire une fonction de déallocation de la mémoire.
// Mais on va faire comme si on avait un GC.

void affiche_liste(Cellule c) {
     
     while(c != NULL) {
          printf("%d ", c->val);
          c = c->next;
     }

     printf("\n");
}

// -----------------------------------------------------------------------------
// Début de la correction du TD

// Remarque: en Java, on fait c.next, c.val; en langage C on fait c->next, c->val
// car on manipule explicitement des pointeurs.

// Exercice 1

void exercice_11()
{
     Cellule a = new_Cellule(1, NULL);
     Cellule b = new_Cellule(2, a);
     Cellule c = new_Cellule(3, NULL);

     a->next = c;
     b->val  = 4;

     affiche_liste(b);
     // 4, 1, 3
}

void exercice_12()
{
     Cellule M =
          new_Cellule(1, new_Cellule(2, new_Cellule(3, new_Cellule(4, new_Cellule(5, NULL)))));

     affiche_liste(M);
     // 1, 2, 3, 4, 5

     affiche_liste(M->next->next->next);
     // 4, 5

     M->next->next = M->next->next->next;

     affiche_liste(M);
     // 1, 2, 4, 5
}

void exercice_13()
{
     Cellule M =
          new_Cellule(1, new_Cellule(2, new_Cellule(4, new_Cellule(5, NULL))));
     
     affiche_liste(M);
     // 1, 2, 4, 5

     Cellule L = M ->next->next;

     affiche_liste(L);

     M->next->next = new_Cellule(3, L);

     affiche_liste(M);
     // 1, 2, 3, 4, 5
}


// Exercice 2
// Question subsidiaire à laquelle vous devriez réfléchir: pourquoi ces algorithmes
// terminent-ils, intuitivement ?

// 2.1
int max_liste_aux(Cellule c, int v)
{
     if(c == NULL) {
          return v;
     } else {
          if(c->val > v) {
               return max_liste_aux(c->next, c->val);
          } else {
               return max_liste_aux(c->next, v);
          }
     }
}

int max_liste(Cellule c)
{
     if(c == NULL) {
          printf("max_liste: erreur, liste vide");
     } else {
          return max_liste_aux(c->next, c->val);
     }
}

// 2.2
int liste_triee_aux(Cellule c, int v)
{
     if(c == NULL) {
          return true;
     } else {
          if(c->val >= v) {
               return liste_triee_aux(c->next, c->val);
          } else {
               return false;
          }
     }
}

int liste_triee(Cellule c)
{
     if(c == NULL) {
          return true;
     } else {
          return liste_triee_aux(c->next, c->val);
     }
}

// 2.3

// Version avec allocation: la liste d'origine n'est pas modifiée,
// on crée une nouvelle liste!
Cellule list_rev_aux(Cellule c, Cellule acc)
{
     if(c == NULL) {
          return acc;
     } else {
          return list_rev_aux(c->next, new_Cellule(c->val, acc));
     }
}

Cellule list_rev(Cellule c)
{
     return list_rev_aux(c, NULL);
}

// 2.3: version sans allocations
Cellule list_rev_aux_noalloc(Cellule c, Cellule acc)
{
     if(c == NULL) {
          return acc;
     } else {
          Cellule next = c->next;
          c->next = acc;
          return list_rev_aux_noalloc(next, c);
     }
}

Cellule list_rev_noalloc(Cellule c)
{
     return list_rev_aux_noalloc(c, NULL);
}

// Exercice 3

// Implémentation de BienInserer et Mystere en C

void BienInserer(int a, Cellule c)
{
     if(a < c->val) {
          Cellule X = new_Cellule(c->val, c->next);
          c->val  = a;
          c->next = X;
     } else if(c->next == NULL) {
          Cellule X = new_Cellule(a, NULL);
          c->next = X;
     } else {
          BienInserer(a, c->next);
     }
}

Cellule Mystere(Cellule L) 
{
     // Dans l'énoncé il manque un test pour vérifier que la liste est non-vide!
     // Je le rajoute.
     if(L == NULL) {
          return L;
     }

     Cellule X = new_Cellule(L->val, NULL);

     L = L->next;

     while(L != NULL) {
          int a = L->val;
          L = L->next;
          BienInserer(a, X);
     }

     return X;
}

// 2.2 Mystere implémente un tri. Lequel ? Sélection ou Insertion ? Je vous 
//     laisse regarder.
       
// 3.3 Mystere est un tri /stable/. Est-ce le cas du tri dont Mystere est
//     inspiré, sur les tableaux ?


// Exercice 4

// Première étape: lisez bien l'énoncé. Il s'agit de décider si une suite
// de parenthèses est bien équilibrée. Par exemple:
// bien parenthésé: (())()((()))
// pas bien parenthésé ())(()
// pas bien parenthésé: )(

// On représente une parenthèse ouvrante par +1 et une parenthèse fermante par -1.

int bien_parenthese_aux(Cellule c, int sum)
{
     if(sum < 0) {
          // plus de parenthèse fermantes que de parenthèses ouvrantes
          return false;
     } else {
          
          if(c == NULL) {
               if(sum != 0) {
                    // on a rencontré plus de parenthèses ouvrantes que de parenthèses fermantes
                    return false;
               } else {
                    return true;
               }
               // équivalent: return (sum == 0);
          } else {
               return bien_parenthese_aux(c->next, sum + c->val);
          }

     }
}

int bien_parenthese(Cellule c)
{
     return bien_parenthese_aux(c, 0);
}

// Suite de l'exercice: maintenant on veut gérer les parenthèses (), les acollades {},
// les crochets [], etc. Chaque type de parenthèse correspond à un entier, e.g.:
// ( = 1, ) = -1
// { = 2, } = -2
// [ = 3, ] = -3
// On ne peut plus utiliser un seul entier pour résoudre le pb. Par exemple, 
// la suite [}) somme à 0 avec l'idée précédente.

// On va utiliser une PILE implémentée par une liste.
// Une parenthèse est soit OUVRANTE (> 0) soit fermante (< 0).
// Si elle est ouvrante, on la pousse sur la pile.
// . Si elle est fermante, on doit retrouver en haut de la pile sa variante ouvrante; 
//   dans ce cas on la dépile, i.e. on l'enlève du haut de la pile. Et on continue.
// . Si ce n'est pas le cas l'expression est mal parenthésée, on s'arrete.

// Exemple ou ça se passe bien, la séquence {()[{}]}
// La liste associée est 2, 1, -1, 3, 2, -2, -3, -2
// Initialement, pile = empty
// On lit 2, > 0 donc on pousse sur la pile.
// pile = 2 :: empty
// On lit 1, > 0 donc on pousse sur la pile.
// pile = 1 :: 2 :: empty
// on lit -1, < 0. On vérifie sur le haut de la pile: pile.top = 1, donc ça matche, on dépile.
// pile = 2 :: empty
// on lit 3, > 0 donc on pousse sur la pile
// pile = 3 :: 2 :: empty
// on lit 2
// pile = 2 :: 3 :: 2 :: empty
// on lit -2
// pile = 3 :: 2 :: empty
// on lit -3
// pile = 2 :: empty
// on lit -2
// pile = empty
// à la fin, la pile est vide donc l'expression était mal parenthésée.

// Si l'expression est mal parenthésée, trois cas d'erreur peuvent se produire:
// 1. la pile est vide alors qu'on vient de lire un symbole fermant (< 0)
//    e.g. une expression de type ()) va produire cette erreur
// 2. le haut de la pile ne matche pas avec le symbole fermant
//    e.g. une expression de type (()} va produire cette erreur
// 3. on a fini de lire la suite de symboles mais la pile est non vide
//    une expression de la forme (() va produire cette erreur

int bien_parenthese2_aux(Cellule c, Cellule pile)
{
     if(c == NULL) {
          // si la pile n'est pas vide à la fin de la liste, alors c'est qu'il y
          // avait trop de parenthèses ouvrantes.
          if(pile == NULL) {
               return true;
          } else {
               return false;
          }
     } else {
          if(c->val > 0) {
               // on pousse sur la pile
               return bien_parenthese2_aux(c->next, new_Cellule(c->val, pile));
          } else if(c->val < 0) {
               // on veut accéder au sommet de la pile
               // si la pile est vide, c'est une erreur
               if(pile == NULL) {
                    return false;
               } else {
                    int sommet = pile->val;

                    // si les symboles ne correspondent pas c'est une erreur
                    if(sommet != -c->val) {
                         return false;
                    } else {
                         // tout va bien, on dépile et on continue
                         pile = pile->next;

                         bien_parenthese2_aux(c->next, pile);
                    }
               }
          }
     }
}

int bien_parenthese2(Cellule c)
{
     return bien_parenthese2_aux(c, NULL);
}

// -----------------------------------------------------------------------------
int main(int argc, char** argv)
{
     
     printf("1.1\n");
     exercice_11();

     printf("\n1.2\n");
     exercice_12();

     printf("\n1.3\n");
     exercice_13();

     printf("\n2.1\n");

     Cellule M = 
          new_Cellule(1, new_Cellule(2, new_Cellule(4, new_Cellule(5, NULL))));

     printf("Ex. 2.1, liste:\n");
     affiche_liste(M);
     printf("Max: %d\n", max_liste(M));

     printf("Ex. 2.2, liste:\n");
     affiche_liste(M);
     printf("Est triée: %d\n", liste_triee(M));

     Cellule M2 = 
          new_Cellule(1, new_Cellule(2, new_Cellule(3, new_Cellule(2, NULL))));

     printf("Ex. 2.2, liste:\n");
     affiche_liste(M2);
     printf("Est triée: %d\n", liste_triee(M2));

     printf("Ex. 2.3, liste:\n");
     affiche_liste(M);
     printf("liste inversée, version alloc:\n");

     Cellule MR = list_rev(M);
     affiche_liste(MR);
     
     printf("la liste d'origine n'est pas modifiée:\n");
     affiche_liste(M);

     printf("liste inversée, version noalloc:\n");

     affiche_liste(list_rev_noalloc(M));

     printf("la liste d'origine est modifiée:\n");
     affiche_liste(M);

     printf("Exercice 3\nL = ");

     Cellule L =
          new_Cellule(5, new_Cellule(4, new_Cellule(7, new_Cellule(6, NULL))));

     affiche_liste(L);

     L = Mystere(L);

     printf("Mystere(L) = ");

     affiche_liste(L);

     return 0;
}
