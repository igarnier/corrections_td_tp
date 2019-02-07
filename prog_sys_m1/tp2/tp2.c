#include <stdlib.h> /* pour malloc/free */
#include <stdio.h>  /* pour printf/fprintf */
#include <unistd.h> /* pour execve */
#include <string.h> /* pour strerror */
#include <errno.h>  /* pour errno */
#include <sys/types.h> /* pour waitpid */
#include <sys/wait.h> /* pour waitpid */

/* Exercice 1.
. `exec ls` effectue un appel à exec sur le binaire /bin/ls depuis le processus du shell.
  ls s'exécute puis termine. Donc le terminal disparait.
*/

void exec_ls()
{
     /* Si on oublie le NULL à la fin, execve va échouer. */
     char* const argv[] = { "/bin/ls", "-l", NULL };
     extern char** environ;

     const int err = execve(argv[0], &argv[0], environ);

     fprintf(stderr, "exercice1: erreur dans execve : %s\n", strerror(errno));
}

void exercice1()
{
     const pid_t p = fork();

     if(p == -1) {
          fprintf(stderr, "exercice1: erreur dans fork\n");
     } else if(p == 0) {
          exec_ls();
     } else {
          waitpid(p, NULL, 0);
     }
}

/* ------------------------------------------------------------------------- */

void please_v0(int main_argc, char** main_argv)
{
     if(main_argc < 2) {
          fprintf(stderr, "exercice2: erreur, pas assez d'aruments\n");
          exit(1);
     }
     /* execvp prend l'environnement du processus appellant. */
     const int err = execvp(main_argv[1], main_argv+1);

     fprintf(stderr, "exercice2: erreur dans execvp : %s\n", strerror(errno));
}

/* version de please où on récupère le résultat du processus fils. */
void please_v1(int main_argc, char** main_argv)
{
     if(main_argc < 2) {
          fprintf(stderr, "exercice2: erreur, pas assez d'aruments\n");
          exit(1);
     }

     const pid_t pid = fork();

     if(pid == -1) {
          fprintf(stderr, "exercice2: erreur dans fork\n");
     } else if(pid == 0) {
          /* execvp prend l'environnement du processus appellant, pas besoin
             de le passer manunellement comme avec execve. */
          const int err = execvp(main_argv[1], main_argv+1);
          fprintf(stderr, "exercice2: erreur dans execvp : %s\n", strerror(errno));
     } else {
          int waitstatus = 0;

          waitpid(pid, &waitstatus, 0);

          if(WIFEXITED(waitstatus)) {
               printf("execution sans erreur, retour = %d\n", WEXITSTATUS(waitstatus));
          } else if(WIFSIGNALED(waitstatus)) {
               printf("processus fils terminé par un signal %d\n", WTERMSIG(waitstatus));
          } else {
               printf("terminaison anormale pour le processus fils\n");
          }
     }
}

void please5(int main_argc, char** main_argv)
{
     for(int i = 0; i < 5; i++)
          please_v1(main_argc, main_argv);
}

/* ------------------------------------------------------------------------- */

int exec_and_get_return(char* name)
{
     char* prog_name    = malloc(strlen(name));
     strcpy(prog_name, name);
     char* const argv[] = { prog_name, NULL };

     const pid_t pid = fork();

     if(pid == -1) {
          fprintf(stderr, "exercice3: erreur dans fork\n");
     } else if(pid == 0) {
          /* execvp prend l'environnement du processus appellant, pas besoin
             de le passer manunellement comme avec execve. */
          const int err = execvp(argv[0], &argv[0]);
          fprintf(stderr, "exercice3: erreur dans execvp : %s\n", strerror(errno));
     } else {
          int waitstatus = 0;

          waitpid(pid, &waitstatus, 0);

          if(WIFEXITED(waitstatus)) {
               const int status = WEXITSTATUS(waitstatus);
               return status;
          } else if(WIFSIGNALED(waitstatus)) {
               fprintf(stderr, "processus fils terminé par un signal %d\n", WTERMSIG(waitstatus));
               exit(1);
          } else {
               printf("terminaison anormale pour le processus fils\n");
               exit(1);
          }
     }
}

int exercice3(int argc, char** argv)
{
     if(argc <= 2) {
          fprintf(stderr, "si: pas assez d'arguments en ligne de commande\n");
          exit(1);
     }

     if(argc == 3) {
          char* condition = argv[1];
          char* commande  = argv[2];

          const int cond_ret = exec_and_get_return(condition);

          switch(cond_ret) {
          case 0:
               /* 0 = true en shell ... mais pas en C !!! */
               (void) exec_and_get_return(commande);
               break;
          case 1:
               /* 1 = false, on ne fait rien */
               break;
          default:
               fprintf(stderr,
                       "si: code de retour de la condition invalide: %d",
                       cond_ret);
               exit(1);
          }
     } else if(argc == 4) {
          char* condition = argv[1];
          char* commande1 = argv[2];
          char* commande2 = argv[3];

          const int cond_ret = exec_and_get_return(condition);

          switch(cond_ret) {
          case 0:
               /* 0 = true en shell ... mais pas en C !!! */
               (void) exec_and_get_return(commande1);
               break;
          case 1:
               (void) exec_and_get_return(commande2);
               break;
          default:
               fprintf(stderr,
                       "si: code de retour de la condition invalide: %d",
                       cond_ret);
               exit(1);
          }
     }

}

/* ------------------------------------------------------------------------- */

int main(int argc, char** argv)
{
     /* exercice1(); */

     please5(argc, argv);

     return 0;
}
