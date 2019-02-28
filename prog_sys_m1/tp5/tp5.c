#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include <sys/errno.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>



/* ------------------------------------------------------------------------- */

/* Exercice 1 */

int exo1()
{
     int pere_vers_fils[2];
     int fils_vers_pere[2];

     const char* msg1 = "Salut, fiston";
     const int   len1 = strlen(msg1) + 1; /* + 1 pour le zero terminal */
     const char* msg2 = "Salut, papa!";
     const int   len2 = strlen(msg2) + 1;

     if(pipe(pere_vers_fils) == -1) {
          perror("Erreur du premier pipe");
          exit(1);
     }
     if(pipe(fils_vers_pere) == -1) {
          perror("Erreur du second pipe");
          exit(1);
     }

     const pid_t pid = fork();

     if(pid == -1) {
          perror("erreur de fork");
          exit(1);
     } else if(pid == 0) {
          char buffer[len1];
          /* si on était paranoiaque il faudrait vérifier que ces appels à close
             terminent correctement. */
          close(fils_vers_pere[0]); /* le fils ne lit jamais ce qu'il écrit lui même */
          close(pere_vers_fils[1]); /* le fils n'écrit jamais à la place du père */
          if(read(pere_vers_fils[0], buffer, len1) != len1) {
               perror("fils: erreur de read");
               exit(1);
          }
          printf("fils: message reçu = %s\n", buffer);
          const int nb = write(fils_vers_pere[1], msg2, len2);
          if(nb == -1) {
               perror("fils: erreur de write");
               exit(1);
          } else if(nb < len2) {
               /* On a pas pu écrire tout ??!!? Ça peut se produire
                  si on écrit sur disque et que le disque est plein. */
               fprintf(stderr, "fils: incapable d'écrire le message");
               exit(1);
          }
          exit(0);
     } else {
          char buffer[len2];
          close(fils_vers_pere[1]);
          close(pere_vers_fils[0]);
          const int nb = write(pere_vers_fils[1], msg1, len1);
          if(nb == -1) {
               perror("pere: erreur de write");
               exit(1);
          } else if(nb < len1) {
               fprintf(stderr, "pere: incapable d'écrire le message");
               exit(1);
          }
          waitpid(pid, NULL, 0);
          if(read(fils_vers_pere[0], buffer, len2) != len2) {
               perror("fils: erreur de read");
               exit(1);
          }
          printf("pere: message reçu = %s\n", buffer);
          exit(0);
     }
}


/* ------------------------------------------------------------------------- */


/* Exercice 2 */

void exo2()
{
     int pipe_fd[2];

     pipe(pipe_fd);

     const pid_t w_fork = fork();

     if(w_fork == -1) {
          perror("Erreur de fork (pour w)");
          exit(0);
     } else if(w_fork == 0) {
          /* redirection de stdout vers l'entrée du pipe */
          close(pipe_fd[0]); /* ce processus ne lit pas sur le pipe */
          if(dup2(pipe_fd[1], 1) == -1) {
               perror("Erreur de dup2 (pour w)");
               exit(0);
          }

          char* const args[] = { "w", NULL };

          if(execvp("w", args) == -1) {
               perror("Erreur de exec");
               exit(1);
          }
     }

     /* le processus père et le processus wc -l n'écrivent pas sur le pipe */
     close(pipe_fd[1]);

     /* un pipe pour rediriger la sortie standard de wc -l vers le père */
     int pipe_wc_fd[2];

     pipe(pipe_wc_fd);

     const pid_t wc_fork = fork();

     if(wc_fork == -1) {
          perror("Erreur de fork (pour wc)");
          exit(0);
     } else if(wc_fork == 0) {
          /* redirection de la sortie du pipe sur stdin */
          if(dup2(pipe_fd[0], 0) == -1) {
               perror("Erreur de dup2 (pour wc)");
               exit(0);
          }

          if(dup2(pipe_wc_fd[1], 1) == -1) {
               perror("Erreur de dup2 (pour wc)");
               exit(0);
          }

          close(pipe_wc_fd[0]);

          char* const args[] = { "wc", "-l", NULL };

          if(execvp("wc", args) == -1) {
               perror("Erreur de exec");
               exit(1);
          }
     }

     close(pipe_fd[0]);
     close(pipe_wc_fd[1]);

     waitpid(w_fork, NULL, 0);
     waitpid(wc_fork, NULL, 0);

     char buff[1024];

     read(pipe_wc_fd[0], buff, 1024);

     printf("père: wc retourne %s\n", buff);
}

/* ------------------------------------------------------------------------- */

/* Exercice 3 */

/* exo3(){ */
/*   int fichier = open("toto.txt", O_WRONLY | O_CREAT , S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ); */
/*   int pf[2]; */
/*   int fp[2]; */

/* } */


/* Token ring par pipe */

/* 10 processus indexés de 0 à 9
   processus 0:
   création de pipe[2]
   pipe[0]: on lit
   pipe[1]: on écrit
   entre k et k+1, il y a un pipe pipe_k où k écrit et où k+1 lit
*/

void transfer_loop(int index, int read_from_pred, int write_to_next, int initial_write)
{
     int token = 0;

     if(initial_write) {
          // bootstrap du token ring
          printf("Processus %d: j'ai le token\n", index);
          sync(); // Force printf
          sleep(1); // enlever les sleep() pour aller à pleine vitesse

          token = 1;
          write(write_to_next, &token, sizeof(int));
          token = 0;
     }

     while(1) {

          if(read(read_from_pred, &token, sizeof(int)) != sizeof(int)) {
               perror("transfer_loop: erreur de read");
               exit(1);
          }

          printf("Processus %d: j'ai le token\n", index);
          // ICI INSERER ACCES AU FICHIER
          sync(); // Force printf
          sleep(1);

          const int nb = write(write_to_next, &token, sizeof(int));

          if(nb == -1) {
               perror("transfer_loop: erreur de write");
               exit(1);
          } else if(nb < sizeof(int)) {
               fprintf(stderr, "transfer_loop: incapable d'écrire le token");
               exit(1);
          }

          token = 0;
     }
}

void create_ring(int current_index, int ring_size, int read_from_pred, int loop_fd)
{

     if(current_index == 0) {
          // premier processus de l'anneau
          int loop_pipe_fd[2];
          int pipe_fd[2];

          pipe(pipe_fd);
          pipe(loop_pipe_fd);

          pid_t pid = fork();

          if(pid == -1) {
               fprintf(stderr,"Dans le processus d'indice %d,", pid);
               perror("Erreur de fork");
               exit(1);
          } else if(pid == 0) {
               close(pipe_fd[1]);
               create_ring(current_index + 1, ring_size, pipe_fd[0], loop_pipe_fd[1]);
          } else {
               close(pipe_fd[0]);
               transfer_loop(current_index, loop_pipe_fd[0], pipe_fd[1], 1);
          }

     } else if(current_index == (ring_size - 1)) {

          // dernier processus: lit depuis read_from_pred, doit écrire dans un fd
          // prevu à cet effet
          transfer_loop(current_index, read_from_pred, loop_fd, 0);

     } else {
          int pipe_fd[2];
          pipe(pipe_fd);

          pid_t pid = fork();

          if(pid == -1) {
               fprintf(stderr,"Dans le processus d'indice %d,", pid);
               perror("Erreur de fork");
               exit(1);
          } else if(pid == 0) {
               close(pipe_fd[1]);
               create_ring(current_index + 1, ring_size, pipe_fd[0], loop_fd);
          } else {
               close(pipe_fd[0]);
               transfer_loop(current_index, read_from_pred, pipe_fd[1], 0);
          }
     }
}

/* int main(int argc, char** argv) */
/* { */
/*      create_ring(0, 10, -1, -1); */

/*      return 0; */
/* } */


/* ------------------------------------------------------------------------- */

int main(int argc, char** argv)
{
     exo2();

     return 0;
}
