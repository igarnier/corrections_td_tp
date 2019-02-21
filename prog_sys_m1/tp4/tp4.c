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

const int   max = 5;
const char* lockfile = "/tmp/lonely.lock";

void lonely()
{
     int fd = open(lockfile, O_RDONLY | O_CREAT | O_EXCL);

     if(fd == -1 && errno == EEXIST) {
          fprintf(stderr, "%s existe\n", lockfile);
          exit(127);
     } else if(fd == -1) {
          perror("erreur de open");
          exit(1);
     }

     for(int i = 0; i < max; i++) {
          printf("je suis seul\n");
          sleep(1);
     }

     if(close(fd) == -1) {
          perror("erreur de close");
          exit(1);
     }

     remove(lockfile);
}

// version avec flock
void lonely_flock()
{
     int fd = open(lockfile, O_RDONLY | O_CREAT);

     if(fd == -1) {
          perror("erreur de open");
          exit(1);
     }

     if(flock(fd, LOCK_EX | LOCK_NB) == -1) {
          switch(errno) {
          case EWOULDBLOCK:
               fprintf(stderr, "verrou actif sur %s\n", lockfile);
               exit(127);
          default:
               perror("erreur de flock");
               exit(1);
          }
     }

     for(int i = 0; i < max; i++) {
          printf("je suis seul\n");
          sleep(1);
     }

     if(flock(fd, LOCK_UN) == -1) {
          perror("erreur de flock");
          exit(1);
     }

     if(close(fd) == -1) {
          perror("erreur de close");
          exit(1);
     }
}

/* ------------------------------------------------------------------------- */

/* Exercice 2 */

/* Puisque on compte jusqu'à 100000 la taille max à lire est de 6 */
const int taille_max = 6;

int read_int_from_file(int fd)
{
     char buffer[taille_max + 1]; /* +1 pour le zero terminal */
     int  rdnb = 0;
     int  entier = 0;

     /* On lit depuis le debut du fichier */
     lseek(fd, 0, SEEK_SET);

     if((rdnb = read(fd, buffer, taille_max)) == -1) {
          perror("read_int_from_file: Erreur de read");
          exit(1);
     }

     /* Faire en sorte que la chaîne de caractères se termine par 0. */
     buffer[rdnb] = '\0';

     entier = atoi(buffer);

     printf("%d: read_int_from_file: lecture de %d\n", getpid(), entier);
     fflush(stdout);
     return entier;
}

void write_int_in_file(int fd, int i)
{
     char buffer[taille_max + 1];

     /* On écrit depuis le debut du fichier */
     lseek(fd, 0, SEEK_SET);

     /* Conversion de l'entier en chaine */
     sprintf(buffer, "%d", i);

     if(write(fd, buffer, strlen(buffer)) == -1) {
          perror("write_int_in_file: Erreur de write");
          exit(1);
     }

     printf("%d: write_int_in_file: ecriture de %d\n", getpid(), i);
     fflush(stdout);
}

void exo2_sequentiel()
{
     const char* compteur = "/tmp/compteur";

     int fd = open(compteur, O_TRUNC | O_RDWR | O_CREAT, 0666);

     if(fd == -1) {
          perror("Erreur de open");
          exit(1);
     }

     /* On commence par écrire 0 */
     write_int_in_file(fd, 0);

     for(int i = 0; i < 100; i++) {

          const int entier = read_int_from_file(fd);

          write_int_in_file(fd, entier + 1);
     }

     close(fd);
}

/* Cette fonction illustre qu'on a besoin de locks */
void exo2_fork()
{
     const char* compteur = "/tmp/compteur";

     int fd = open(compteur, O_TRUNC | O_RDWR | O_CREAT, 0666);

     if(fd == -1) {
          perror("Erreur de open");
          exit(1);
     }

     /* On commence par écrire 0 */
     write_int_in_file(fd, 0);

     const pid_t pid = fork();

     if(pid == -1) {
          perror("Erreur de fork");
          exit(1);
     } else if(pid == 0) {
          for(int i = 0; i < 50000; i++) {
               const int entier = read_int_from_file(fd);
               write_int_in_file(fd, entier + 1);
          }
     } else {
          for(int i = 0; i < 50000; i++) {
               const int entier = read_int_from_file(fd);
               write_int_in_file(fd, entier + 1);
          }
     }

     close(fd);
}

/* Solution avec des flock:
   Remarquez qu'on close() puis reopen() /après/ le fork.
   Si on utilisait juste le premier fd alors les locks seraient
   également partagés (i.e. les processus auraient les
   mêmes permissions).
 */
void exo2_fork_flock()
{
     const char* compteur = "/tmp/compteur";

     int fd = open(compteur, O_TRUNC | O_RDWR | O_CREAT, 0666);

     if(fd == -1) {
          perror("Erreur de open");
          exit(1);
     }

     /* On commence par écrire 0 */
     write_int_in_file(fd, 0);

     close(fd);

     const pid_t pid = fork();

     if(pid == -1) {
          perror("Erreur de fork");
          exit(1);
     } else if(pid == 0) {
          int fd = open(compteur, O_RDWR, 0666);
          if(fd == -1) {
               perror("Erreur de open");
               exit(1);
          }

          for(int i = 0; i < 50000; i++) {
               /* Les section critiques sont protégées par lock/unlock */
               if(flock(fd, LOCK_EX) == -1) { perror("erreur de lock"); exit(1); }
               const int entier = read_int_from_file(fd);
               write_int_in_file(fd, entier + 1);
               if(flock(fd, LOCK_UN) == -1) { perror("erreur d'unlock"); exit(1); }
          }

          close(fd);
     } else {
          int fd = open(compteur, O_RDWR, 0666);
          if(fd == -1) {
               perror("Erreur de open");
               exit(1);
          }

          for(int i = 0; i < 50000; i++) {
               if(flock(fd, LOCK_EX) == -1) { perror("erreur de lock"); exit(1); }
               const int entier = read_int_from_file(fd);
               write_int_in_file(fd, entier + 1);
               if(flock(fd, LOCK_UN) == -1) { perror("erreur d'unlock"); exit(1); }
          }

          close(fd);
     }
}

/* Avec flock, les verrous sont spécifiques au processus et ne sont pas partagés. */
void exo2_fork_fcntl()
{
     const char* compteur = "/tmp/compteur";

     int fd = open(compteur, O_TRUNC | O_RDWR | O_CREAT, 0666);

     if(fd == -1) {
          perror("Erreur de open");
          exit(1);
     }

     /* On commence par écrire 0 */
     write_int_in_file(fd, 0);

     const pid_t pid = fork();

     if(pid == -1) {
          perror("Erreur de fork");
          exit(1);
     } else if(pid == 0) {
          // Lock en ecriture
          struct flock fd_lock = { F_WRLCK, SEEK_SET, 0, 0, 0 };
          for(int i = 0; i < 10; i++) {
               fd_lock.l_type = F_WRLCK;
               /* Les section critiques sont protégées par lock/unlock */
               if(fcntl(fd, F_SETLKW, &fd_lock) == -1) { perror("Erreur de fcntl"); exit(1); }
               const int entier = read_int_from_file(fd);
               write_int_in_file(fd, entier + 1);
               fd_lock.l_type = F_UNLCK;
               if(fcntl(fd, F_SETLKW, &fd_lock) == -1) { perror("Erreur de fcntl"); exit(1); }
          }
     } else {
          struct flock fd_lock = { F_WRLCK, SEEK_SET, 0, 0, 0 };
          for(int i = 0; i < 10; i++) {
               fd_lock.l_type = F_WRLCK;
               if(fcntl(fd, F_SETLKW, &fd_lock) == -1) { perror("Erreur de fcntl"); exit(1); }
               const int entier = read_int_from_file(fd);
               write_int_in_file(fd, entier + 1);
               fd_lock.l_type = F_UNLCK;
               if(fcntl(fd, F_SETLKW, &fd_lock) == -1) { perror("Erreur de fcntl"); exit(1); }
          }
     }

     close(fd);
}

/* ------------------------------------------------------------------------- */

/* Exercice 3 */

/* Son suppose qu'un fichier vide de 1032 octets a été créé via la commande
   donnée dans l'énoncé.
*/

int cherche_place_libre(int fd)
{
     char file[1032];

     const int rdnb = read(fd, file, 1032);

     if(rdnb == -1) {
          perror("read error");
     }

     for(int i = 0; i < 1032; i++) {
          if(file[i] == 0)
               return i;
     }

     return -1;
}

int verifie_place_libre(int fd, int i)
{
     lseek(fd, i, SEEK_SET);

     char bit = 0;

     if(read(fd, &bit, 1) == -1) {
          perror("write error");
     }

     return (int) !bit;
}

void reserve_place(int fd, int i)
{
     lseek(fd, i, SEEK_SET);

     const char bit = 1;

     if(write(fd, &bit, 1) == -1) {
          perror("write error");
     }
}

void reservation_simple(const char* db_name)
{
     int fd = open(db_name, O_RDWR);

     if(fd == -1) {
          perror("erreur de open");
          exit(1);
     }

     if(flock(fd, LOCK_EX) == -1) {
          perror("erreur de flock");
          exit(1);
     }

     const int place = cherche_place_libre(fd);

     if(place == -1) {
          printf("pas de place trouvée !\n");
     } else {
          printf("place trouvée: %d\n", place);
          reserve_place(fd, place);
     }

     if(flock(fd, LOCK_UN) == -1) {
          perror("erreur de flock");
          exit(1);
     }

     close(fd);
}

void reservation_acces_partage(const char* db_name)
{
     int fd = open(db_name, O_RDWR);

     if(fd == -1) {
          perror("erreur de open");
          exit(1);
     }

     if(flock(fd, LOCK_SH) == -1) {
          perror("erreur de flock");
          exit(1);
     }

     const int place = cherche_place_libre(fd);

     if(flock(fd, LOCK_UN) == -1) {
          perror("erreur de flock");
          exit(1);
     }

     if(place == -1) {
          printf("pas de place trouvée !\n");
          return;
     }

     printf("place trouvée: %d\n", place);
     printf("tentative de réservation ...");

     if(flock(fd, LOCK_EX) == -1) {
          perror("erreur de flock");
          close(fd);
          exit(1);
     }

     if(verifie_place_libre(fd, place)) {
          printf(" Succes !\n");
          reserve_place(fd, place);
     } else {
          printf(" Echec !\n");
     }

     if(flock(fd, LOCK_UN) == -1) {
          perror("erreur de flock");
          exit(1);
     }

     close(fd);
}


int main(int argc, char** argv)
{

     if(argc < 2) {
          return 1;
     } else {
          reservation_acces_partage(argv[1]);
     }

     return 0;
}
