#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <pthread.h>

/* ------------------------------------------------------------------------- */
/* barrière de synchronisation à n processus avec boucle de scrutation */

typedef struct {
     pthread_mutex_t mutex;
     int nprocs;
     int block_flag;
} barriere_v1_t;

void barriere_v1_init(barriere_v1_t* b, int nprocs)
{
     pthread_mutexattr_t attr;

     if(pthread_mutexattr_init(&attr) != 0) {
          fprintf(stderr, "pthread_mutexattr_init\n");
          exit(1);
     }

     if(pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED) != 0) {
          fprintf(stderr, "pthread_mutexattr_setpshared\n");
          exit(1);
     }

     if(pthread_mutex_init(&(b->mutex), &attr) != 0) {
          fprintf(stderr, "pthread_mutex_init\n");
          exit(1);
     }

     b->nprocs = nprocs;
     b->block_flag = 1;
}


/* barrière de synchronisation à n processus avec boucle de scrutation */
void barriere_scrutation(barriere_v1_t* b)
{
     if(pthread_mutex_lock(&(b->mutex)) != 0) {
          fprintf(stderr, "barriere: pthread_mutex_lock\n");
          exit(1);
     }

     /* message de debug pour suivre ce qui se passe */
     printf("%d: nprocs = %d, block_flag = %d\n", getpid(), b->nprocs, b->block_flag);

     if(b->nprocs > 1) {
          /* plus de processus doivent arriver */
          /* on s'enregistre */
          b->nprocs -= 1;
     } else if(b->nprocs == 1) {
          /* on est le dernier processus */
          b->nprocs -= 1;
          b->block_flag = 0;
     } else if(b->nprocs == 0) {
          /* bug */
          fprintf(stderr, "%d: barriere: b->nprocs == 0\n", getpid());
          exit(1);
     }

     msync(b, sizeof(barriere_v1_t), MS_SYNC);

     if(pthread_mutex_unlock(&(b->mutex)) != 0) {
          fprintf(stderr, "barriere: pthread_mutex_unlock\n");
          exit(1);
     }

     while(b->block_flag) ;
}

/* Processus utilisant la barrière de synchronisation */
void processus_v1()
{
     int shm = shm_open("/barriere", O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);

     if(shm == -1) {
          perror("processus: shm_open");
          exit(1);
     }

     const int total_size = sizeof(barriere_v1_t);

     if(ftruncate(shm, total_size) == -1) {
          perror("processus: ftruncate");
          exit(1);
     }

     void* mmaped = mmap(NULL, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm, 0);

     if(mmaped == MAP_FAILED) {
          perror("processus: mmap");
          exit(1);
     }

     barriere_v1_t* b = (barriere_v1_t*) mmaped;

     srand(getpid());

     sleep(rand() % 10);

     printf("processus %d avant barriere\n", getpid());
     barriere_scrutation(b);
     printf("processus %d après barriere\n", getpid());

     if(munmap(mmaped, total_size) == -1) {
          perror("processus: munmap");
          exit(1);
     }

     close(shm);
}

/* processus initialisant la barrière de synchro et forkant les
   sous-processus */

void processus_pere_v1()
{
     int shm = shm_open("/barriere", O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);

     if(shm == -1) {
          perror("processus: shm_open");
          exit(1);
     }

     const int total_size = sizeof(barriere_v1_t);

     if(ftruncate(shm, total_size) == -1) {
          perror("processus: ftruncate");
          exit(1);
     }

     void* mmaped = mmap(NULL, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm, 0);

     if(mmaped == MAP_FAILED) {
          perror("processus: mmap");
          exit(1);
     }

     barriere_v1_t* b = (barriere_v1_t*) mmaped;

     barriere_v1_init(b, 9);
     msync(b, sizeof(barriere_v1_t), MS_SYNC);

     printf("pere (%d): nprocs = %d, block_flag = %d\n", getpid(), b->nprocs, b->block_flag);

     pid_t pid = 0;

     /* fork n procs */
     for(int i = 0; i < 9; i++) {
          pid = fork();

          if(pid == -1) {
               perror("fork"); exit(1);
          } else if(pid == 0) {
               processus_v1();
               exit(0);
          } else {
               continue;
          }
     }
}

/* ------------------------------------------------------------------------- */
/* barrière de synchronisation à n processus avec variable de condition */

typedef struct {
     pthread_mutex_t mutex;
     pthread_cond_t cond;
     int nprocs;
     int block_flag;
} barriere_v2_t;

void barriere_v2_init(barriere_v2_t* b, int nprocs)
{
     pthread_mutexattr_t mattr;
     pthread_condattr_t cattr;

     if(pthread_mutexattr_init(&mattr) != 0) {
          fprintf(stderr, "pthread_mutexattr_init\n");
          exit(1);
     }

     if(pthread_mutexattr_setpshared(&mattr, PTHREAD_PROCESS_SHARED) != 0) {
          fprintf(stderr, "pthread_mutexattr_setpshared\n");
          exit(1);
     }

     if(pthread_mutex_init(&(b->mutex), &mattr) != 0) {
          fprintf(stderr, "pthread_mutex_init\n");
          exit(1);
     }


     if(pthread_condattr_init(&cattr) != 0) {
          fprintf(stderr, "pthread_condattr_init\n");
          exit(1);
     }

     if(pthread_condattr_setpshared(&cattr, PTHREAD_PROCESS_SHARED) != 0) {
          fprintf(stderr, "pthread_mutexattr_setpshared\n");
          exit(1);
     }

     if(pthread_cond_init(&(b->cond), &cattr) != 0) {
          fprintf(stderr, "pthread_cond_init\n");
          exit(1);
     }

     b->nprocs = nprocs;
     b->block_flag = 1;
}



/* barrière de synchronisation à n processus avec variable */
void barriere_condwait(barriere_v2_t* b)
{
     printf("%d: locking...\n", getpid());

     if(pthread_mutex_lock(&(b->mutex)) != 0) {
          fprintf(stderr, "barriere: pthread_mutex_lock\n");
          exit(1);
     }

     printf("%d: nprocs = %d\n", getpid(), b->nprocs);

     if(b->nprocs > 1) {
          /* plus de processus doivent arriver */
          /* on s'enregistre et on se met en attente */
          b->nprocs -= 1;
          msync(b, sizeof(barriere_v2_t), MS_SYNC);

          printf("%d: en attente\n", getpid());
          while(b->block_flag) {
               pthread_cond_wait(&(b->cond), &(b->mutex));
               printf("%d: tentative de reactivation ...\n", getpid());
          }
          printf("%d: réactivé\n", getpid());

          if(pthread_mutex_unlock(&(b->mutex)) != 0) {
               fprintf(stderr, "barriere: pthread_mutex_unlock\n");
               exit(1);
          }

     } else if(b->nprocs == 1) {
          /* on est le dernier processus: on reveille tous les autres */

          printf("%d: dernier proc\n", getpid());

          b->nprocs -= 1;
          b->block_flag = 0;

          msync(b, sizeof(barriere_v2_t), MS_SYNC);

          if(pthread_cond_broadcast(&(b->cond)) != 0) {
               fprintf(stderr, "barriere: pthread_cond_broadcast\n");
               exit(1);
          }

          if(pthread_mutex_unlock(&(b->mutex)) != 0) {
               fprintf(stderr, "barriere: pthread_mutex_unlock\n");
               exit(1);
          }

     } else if(b->nprocs == 0) {
          if(pthread_mutex_unlock(&(b->mutex)) != 0) {
               fprintf(stderr, "barriere: pthread_mutex_unlock\n");
               exit(1);
          }

          /* bug */
          fprintf(stderr, "%d: barriere: b->nprocs == 0\n", getpid());
          exit(1);
     }
}


/* Processus utilisant la barrière de synchronisation avec condwait
   (c'est exactement la même chose sauf que le processus ne consomme
   pas de ressources CPU pendant qu'il attend) */
void processus_v2()
{
     int shm = shm_open("/barriere", O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);

     if(shm == -1) {
          perror("processus: shm_open");
          exit(1);
     }

     const int total_size = sizeof(barriere_v2_t);

     if(ftruncate(shm, total_size) == -1) {
          perror("processus: ftruncate");
          exit(1);
     }

     void* mmaped = mmap(NULL, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm, 0);

     if(mmaped == MAP_FAILED) {
          perror("processus: mmap");
          exit(1);
     }

     barriere_v2_t* b = (barriere_v2_t*) mmaped;

     srand(getpid());

     sleep(rand() % 10);

     printf("processus %d avant barriere\n", getpid());
     barriere_condwait(b);
     printf("processus %d après barriere\n", getpid());

     if(munmap(mmaped, total_size) == -1) {
          perror("processus: munmap");
          exit(1);
     }

     close(shm);
}


/* processus initialisant la barrière de synchro et forkant les
   sous-processus */

void processus_pere_v2()
{
     int shm = shm_open("/barriere", O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);

     if(shm == -1) {
          perror("processus: shm_open");
          exit(1);
     }

     const int total_size = sizeof(barriere_v2_t);

     if(ftruncate(shm, total_size) == -1) {
          perror("processus: ftruncate");
          exit(1);
     }

     void* mmaped = mmap(NULL, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm, 0);

     if(mmaped == MAP_FAILED) {
          perror("processus: mmap");
          exit(1);
     }

     barriere_v2_t* b = (barriere_v2_t*) mmaped;

     barriere_v2_init(b, 9);
     msync(b, sizeof(barriere_v2_t), MS_SYNC);

     printf("pere (%d): nprocs = %d\n", getpid(), b->nprocs);

     pid_t pid = 0;

     /* fork n procs */
     for(int i = 0; i < 9; i++) {
          pid = fork();

          if(pid == -1) {
               perror("fork"); exit(1);
          } else if(pid == 0) {
               processus_v2();
               exit(0);
          } else {
               continue;
          }
     }

     printf("Processus père terminé");
}

/* ------------------------------------------------------------------------- */

int main(int argc, char** argv)
{
     processus_pere_v2();
}
