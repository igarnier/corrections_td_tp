#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
/* #include <sys/errno.h> */
/* #include <sys/file.h> */
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
/* #include <string.h> */


/* ------------------------------------------------------------------------- */

/* Exercice 1 */

void my_cat(const char* source)
{
     int fd = open(source, O_RDONLY);

     if(fd == -1) {
          perror("erreur de open");
          exit(1);
     }

     /* recuperer la taille du fichier */
     struct stat statbuf;

     if(fstat(fd, &statbuf) == -1) {
          perror("erreur de fstat");
          exit(1);
     }

     const int length = statbuf.st_size;

     void* mmaped = mmap(NULL, length, PROT_READ, MAP_SHARED, fd, 0);

     if(mmaped == MAP_FAILED) {
          perror("erreur de mmap");
          exit(1);
     }

     if(write(STDOUT_FILENO, mmaped, length) != length) {
          fprintf(stderr, "erreur de write: taille ecrite != taille fichier");
          exit(1);
     }

     /* optionnel mais + propre: unmapper */
     if(munmap(mmaped, length) == -1) {
          perror("erreur de munmap");
          exit(1);
     }

     close(fd);
}

void my_cp(const char* source, const char* dest)
{
     int source_fd = open(source, O_RDONLY);

     if(source_fd == -1) {
          perror("erreur de open pour la source");
          exit(1);
     }

     int dest_fd = open(dest, O_RDWR | O_EXCL | O_CREAT, S_IRWXU);

     if(dest_fd == -1) {
          close(source_fd);
          perror("erreur de open pour la destination");
          exit(1);
     }

     /* recuperer la taille du fichier source */
     struct stat statbuf;

     if(fstat(source_fd, &statbuf) == -1) {
          perror("erreur de fstat");
          exit(1);
     }

     const int length = statbuf.st_size;

     /* augmenter la taille du fichier cible */
     ftruncate(dest_fd, length);

     void* source_mmaped = mmap(NULL, length, PROT_READ, MAP_SHARED, source_fd, 0);

     if(source_mmaped == MAP_FAILED) {
          perror("erreur de mmap");
          exit(1);
     }

     void* dest_mmaped = mmap(NULL, length, PROT_READ | PROT_WRITE, MAP_SHARED, dest_fd, 0);

     if(dest_mmaped == MAP_FAILED) {
          perror("erreur de mmap");
          exit(1);
     }

     memcpy(dest_mmaped, source_mmaped, length);

     msync(dest_mmaped, length, MS_SYNC);

     if(munmap(source_mmaped, length) == -1) {
          perror("erreur de munmap");
          exit(1);
     }


     if(munmap(dest_mmaped, length) == -1) {
          perror("erreur de munmap");
          exit(1);
     }

     close(source_fd);
     close(dest_fd);
}

/* Exercice 2 */

int exo2()
{
  int syncro_fd = open("syncro", O_CREAT | O_RDWR, 500);

  if(syncro_fd == -1) {
       perror("erreur de open");
       exit(1);
  }

  ftruncate(syncro_fd, 4);

  void* syncro_mmaped = mmap(NULL, 4, PROT_READ | PROT_WRITE, MAP_SHARED, syncro_fd, 0);

  if(syncro_mmaped == MAP_FAILED) {
       perror("erreur de mmap");
       exit(1);
  }

  const pid_t pid = fork();

  if(pid == -1)
  {
       perror("erreur de fork");
       exit(1);
  }
  else if(pid == 0)
  {
    sleep(1);
    int *p = syncro_mmaped;
    *p = getpid();
    printf("Fils : Mon pid est : %d\n",*p);
    if(msync(syncro_mmaped, 4, MS_SYNC) != 0)
    {
      perror("msync");
      exit(1);
    }

    return 0;
  }
  else
  {
    int son_pid = 0;

    ssize_t nb_byte = 0;

    while(son_pid == 0)
    {
      lseek(syncro_fd, 0, SEEK_SET);
      read(syncro_fd, &son_pid, 4);
    }

    printf("Père : Le pid de mon fils est : %d\n",son_pid);
    wait(NULL);

    close(syncro_fd);
    remove("./syncro");
    return 0;
  }
}

int exo2bis()
{
  void* syncro_mmaped = mmap(NULL, 4, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0);

  if(syncro_mmaped == MAP_FAILED) {
       perror("erreur de mmap");
       exit(1);
  }

  int *p = syncro_mmaped;

  const pid_t pid = fork();

  if(pid == -1)
  {
       perror("erreur de fork");
       exit(1);
  }
  else if(pid == 0)
  {
    sleep(1);
    *p = getpid();
    printf("Fils : Mon pid est : %d\n",*p);
    if(msync(syncro_mmaped, 4, MS_SYNC) != 0)
    {
      perror("msync");
      exit(1);
    }

    return 0;
  }
  else
  {
    while(*p == 0);

    printf("Père : Le pid de mon fils est : %d\n",*p);
    wait(NULL);
    return 0;
  }
}


/* ------------------------------------------------------------------------- */

int main(int argc, char** argv)
{
     /*if(argc == 2)
          my_cat(argv[1]);
     else if(argc == 3)
          my_cp(argv[1], argv[2]);
     else {
          fprintf(stderr, "pas assez d'arguments en ligne de commande");
     }*/

     exo2bis();

     return 0;
}
