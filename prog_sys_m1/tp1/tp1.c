#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void exo1()
{
     const char* chaine1 = "tut ";
     const char* chaine2 = "pouet ";

     const pid_t pid = fork();

     if(pid == -1) {
          /* erreur de fork */
          exit(1);
     } else if(pid == 0) {
          /* fils */
          while(1)
               write(1, chaine2, strlen(chaine2));
     } else {
          /* parent */

          /* write the parent message */
          while(1)
               write(1, chaine1, strlen(chaine1));

          /* wait for child to die */
          int wstatus = 0;
          waitpid(pid, NULL, 0);
     }
}

void exo2()
{
     pid_t pids[13];

     for(int i = 0; i < 13; i++) {
          pids[i] = fork();

          if(pids[i] == -1) {
               /* erreur de fork */
               exit(1);
          } else if(pids[i] == 0) {
               /* processus fils */
               const pid_t pid_fils = getpid();
               srandom(pid_fils);
               const int   randtime = random() % 5;
               printf("processus fils: %d, attente de %d secondes\n", pid_fils, randtime);
               sleep(randtime);
               exit(0);
          } else {
               printf("processus père: création du processus fils %d/13 %d ok\n", i+1, pids[i]);
          }
     }

     /* Le père attend la mort des fils */
     for(int i = 0; i < 13; i++) {
          waitpid(pids[i], NULL, 0);
          printf("processus père: fils %d mort\n", pids[i]);
     }

     exit(0);
}

void processus_feuille()
{
     printf("petit-fils %d\n", getpid());
     sleep(10);
     exit(0);
}

void attendre_mort(pid_t* tab, int n)
{
     for(int j = 0; j < n; j++) {
          waitpid(tab[j], NULL, 0);
     }
}

void exo3()
{
     pid_t layer0[3];
     pid_t layer1[2];

     for(int i = 0; i < 3; i++) {
          layer0[i] = fork();

          if(layer0[i] == -1) {
               /* erreur de fork */
               exit(1);
          } else if(layer0[i] == 0) {

               /* processus fils */
               if(i == 0) {
                    /* premier fils */
                    for(int j = 0; j < 2; j++) {
                         layer1[j] = fork();

                         if(layer1[j] == -1) {
                              /* erreur de fork */
                              exit(1);
                         } else if(layer1[j] == 0) {
                              processus_feuille();
                         }
                    }

                    /* le premier fils attend la mort de ses 2 propres fils */
                    attendre_mort(layer1, 2);
                    exit(0);
               } else {
                    processus_feuille();
               }
          }
     }

     /* le processus père attend la mort de ses 3 fils */
     attendre_mort(layer0, 3);
}

void cherche_42(int* tableau, int debut, int fin)
{
     sleep(10);
     for(int i = debut; i < fin; i++) {
          if(tableau[i] == 42) {
               printf("proc %d : indice %d\n", getpid(), i);
          }
     }
}

void cherche_rec(int* tableau, int debut, int fin, int prof)
{
     if(prof == 0) {
          cherche_42(tableau, debut, fin);
     } else {
          const int milieu = debut + (fin - debut) / 2;

          const int pid = fork();

          if(pid == -1) {
               printf("erreur de fork\n");
               exit(1);
          } else if(pid == 0) {
               // fils
               cherche_rec(tableau, milieu, fin, prof - 1);
          } else {
               cherche_rec(tableau, debut, milieu, prof - 1);
               waitpid(pid, NULL, 0);
          }
     }
}

void exo4(int argc, char** argv)
{
     int* tableau = NULL;
     const int taille = 10000;

     if(argc != 2) {
          fprintf(stderr, "veuillez donner la profondeur k en argument\n");
          exit(1);
     }

     /* lecture de l'argument en ligne de commande */
     const int k = strtol(argv[1], NULL, 10);

     if(k < 0) {
          fprintf(stderr, "argument incorrect\n");
          exit(1);
     }

     tableau = malloc(sizeof(int) * taille);

     /* initialisation */
     for(int i = 0; i < taille; i++) {
          tableau[i] = random() % 1000;
     }

     cherche_rec(tableau, 0, taille, k);

     free(tableau);
}

int main(int argc, char** argv)
{
     /* exo1(); */
     /* exo2(); */
     /* exo3(); */
     exo4(argc, argv);
     return 0;
}
