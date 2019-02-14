#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <string.h> /* strlen */


void my_cp(char* src, const char* dst)
{
     const pid_t pid = fork();

     /* Ouverture des fichiers */
     /* ouverture du fichier cible en écriture */
     const int dstfd = open(dst, O_WRONLY | O_TRUNC | O_APPEND);

     /* sauvegarde de stdout (= 1) avec un nouveau nom */
     const int stdoutfd = dup(1);

     /* redirection de stdout (= 1) vers dstfd */
     const int dupfd = dup2(dstfd, 1);

     if(dstfd == -1) {
          fprintf(stderr, "erreur de open");
          exit(1);
     }

     if((dupfd == -1) || (stdoutfd == -1)) {
          fprintf(stderr, "erreur de dup ou dup2");
          exit(1);
     }

     if(pid == -1) {
          fprintf(stderr, "erreur de fork");
          exit(1);
     } else if (pid == 0) {
          char* const args[] = { "cat", src, NULL };
          if(execvp("cat", args) == -1) {
               fprintf(stderr, "erreur de execve");
               exit(1);
          }
     } else {
          waitpid(pid, NULL, 0);
     }

     if(close(dstfd) != 0) {
          fprintf(stderr, "erreur de close(dstfd)");
          exit(1);
     }

     const char* str = "Copie terminee\n";

     if(write(stdoutfd, str, strlen(str)) == -1) {
          fprintf(stderr, "erreur de write");
     }

     exit(0);
}

/* ------------------------------------------------------------------------- */
/* Ecritures concurrentes. */

void exercice2_fork_avant_open(const char* filename)
{
     const pid_t pid = fork();
     const char* str = "gloups\n";

     if(pid == -1) {
          fprintf(stderr, "erreur de fork");
          exit(1);
     } else if(pid == 0) {

          const int fd = open(filename, O_CREAT | O_WRONLY | O_TRUNC);

          if(fd == -1) {
               fprintf(stderr, "erreur de open (fils)");
               exit(1);
          }

          if(write(fd, str, strlen(str)) == -1) {
               fprintf(stderr, "erreur de write (fils)");
               exit(1);
          }

          if(close(fd) != 0) {
               fprintf(stderr, "erreur de close(fd) (fils)");
               exit(1);
          }

     } else {

          waitpid(pid, NULL, 0);

          const int fd = open(filename, O_CREAT | O_WRONLY | O_TRUNC);

          if(fd == -1) {
               fprintf(stderr, "erreur de open (pere)");
               exit(1);
          }

          if(write(fd, str, strlen(str)) == -1) {
               fprintf(stderr, "erreur de write (pere)");
               exit(1);
          }

          if(close(fd) != 0) {
               fprintf(stderr, "erreur de close(fd) (pere)");
               exit(1);
          }
     }
}


void exercice2_fork_apres_open(const char* filename)
{
     const pid_t pid = fork();
     const char* str = "gloups";

     const int fd = open(filename, O_CREAT | O_WRONLY | O_TRUNC);

     if(fd == -1) {
          fprintf(stderr, "erreur de open");
               exit(1);
     }

     if(pid == -1) {
          fprintf(stderr, "erreur de fork");
          exit(1);
     } else if(pid == 0) {

          if(write(fd, str, strlen(str)) == -1) {
               fprintf(stderr, "erreur de write (fils)");
               exit(1);
          }

          if(close(fd) != 0) {
               fprintf(stderr, "erreur de close(fd) (fils)");
               exit(1);
          }

     } else {

          waitpid(pid, NULL, 0);

          if(write(fd, str, strlen(str)) == -1) {
               fprintf(stderr, "erreur de write (pere)");
               exit(1);
          }

          if(close(fd) != 0) {
               fprintf(stderr, "erreur de close(fd) (pere)");
               exit(1);
          }
     }
}

/* ------------------------------------------------------------------------- */
/* Ecritures concurrentes */

void exercice3(const char* filename)
{
     /* Ouverture du fichier de communication en lecture-ecriture.
        Partagé entre le fils et le père. */
     const int comm_fd = open(filename, O_RDWR | O_APPEND);

     if(comm_fd == -1) {
          fprintf(stderr, "erreur de open (pere)\n");
     }

     const pid_t pid = fork();

     if(pid == -1) {
          fprintf(stderr, "erreur de fork");
          exit(1);
     } else if(pid == 0) {
          /* allocation d'un buffer pour stocker les portions de messages
             depuis stdin. */
          int readbytes = 0;
          char* buffer = malloc(1024 * sizeof(char));

          if(buffer == NULL) {
               fprintf(stderr, "erreur de malloc (fils)");
               exit(1);
          }

          /* Lecture du message sur stdin (= 0) jusqu'à épuisement */
          do {
               readbytes = read(0, buffer, 1024);
               if(readbytes == -1) {
                    fprintf(stderr, "erreur de read (fils)");
                    exit(1);
               } else if(readbytes == 0) {
                    break;
               }

               if(write(comm_fd, buffer, readbytes) != readbytes) {
                    fprintf(stderr, "erreur de write (fils)");
                    exit(1);
               }
          } while(readbytes == 1024);

          /* désallocation du buffer */
          free(buffer);

          if(close(comm_fd) != 0) {
               fprintf(stderr, "erreur de close(fd) (fils)");
               exit(1);
          }

     } else {

          /* attente du fils */
          waitpid(pid, NULL, 0);

          /* remise du curseur à 0 */
          lseek(comm_fd, 0, SEEK_SET);

          /* allocation d'un buffer pour stocker les portions de messages
             depuis stdin. */
          int readbytes = 0;
          char* buffer = malloc(1024 * sizeof(char));

          if(buffer == NULL) {
               fprintf(stderr, "erreur de malloc (pere)");
               exit(1);
          }

          /* Lecture du message sur stdin (= 0) jusqu'à épuisement */
          do {
               readbytes = read(comm_fd, buffer, 1024);
               if(readbytes == -1) {
                    fprintf(stderr, "erreur de read (pere)");
                    exit(1);
               } else if(readbytes == 0) {
                    break;
               }

               if(write(1, buffer, readbytes) != readbytes) {
                    fprintf(stderr, "erreur de write (pere)");
                    exit(1);
               }
          } while(readbytes == 1024);

          free(buffer);

          if(close(comm_fd) != 0) {
               fprintf(stderr, "erreur de close(fd) (pere)");
               exit(1);
          }
     }
}


int main(int argc, char** argv)
{
     /* Exercice 1 */
     /* if(argc > 2) */
     /*      my_cp(argv[1], argv[2]); */

     /* Exercice 2 */
     /* if(argc > 1) */
     /*      exercice2_fork_apres_open(argv[1]); */

     /* Exercice 2 */
     if(argc > 1)
          exercice3(argv[1]);

     return 0;
}
