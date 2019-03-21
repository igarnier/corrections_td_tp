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
#include <semaphore.h>
#include <sys/file.h>


/* compiler avec gcc tp7.c -lrt -lpthread -o tp7*/
/* ------------------------------------------------------------------------- */

/* Exercice 1 */

struct rendezvous {
int jour,mois,annee;
};


void bob(){
  int fd = shm_open("/rendezvous", O_CREAT | O_RDONLY, S_IRUSR | S_IWUSR );
  if(fd<0)
  {
    perror("shm_open");
    exit(2);
  }
  ftruncate(fd, sizeof(struct rendezvous));

  struct rendezvous *rdv = mmap(NULL, sizeof(struct rendezvous), PROT_READ, MAP_SHARED, fd, 0);

  if(rdv == MAP_FAILED) {
       perror("erreur de mmap");
       exit(1);
  }

  sem_t *vide = sem_open("/vide", O_CREAT, S_IRUSR | S_IWUSR, 1);
  sem_t *plein = sem_open("/plein", O_CREAT, S_IRUSR | S_IWUSR, 0);
  if(vide == SEM_FAILED || plein == SEM_FAILED)
  {
    perror("sem_open");
    exit(0);
  }
  while(1)
  {
    sem_wait(plein);
    printf("Rendez vous le %d / %d / %d\n", rdv->jour, rdv->mois, rdv->annee );
    sem_post(vide);
  }
}

void alice(){
  int fd = shm_open("/rendezvous", O_CREAT | O_RDWR, S_IRUSR | S_IWUSR );
  if(fd<0)
  {
    perror("shm_open");
    exit(2);
  }
  ftruncate(fd, sizeof(struct rendezvous));

  struct rendezvous *rdv = mmap(NULL, sizeof(struct rendezvous), PROT_WRITE, MAP_SHARED, fd, 0);

  if(rdv == MAP_FAILED) {
       perror("erreur de mmap");
       exit(1);
  }

  sem_t *vide = sem_open("/vide", O_CREAT, S_IRUSR | S_IWUSR ,1 );
  sem_t *plein = sem_open("/plein", O_CREAT, S_IRUSR | S_IWUSR, 0 );

  srandom(getpid());

  while(1)
  {
    sem_wait(vide);
    rdv->jour = (random() % 28 ) + 1;
    rdv->mois = (random() % 12 ) + 1;
    rdv->annee = (random() % 10 ) + 2019;
    sem_post(plein);
    sleep(5);
  }
}


void exo1()
{
  sem_unlink("/vide");
  sem_unlink("/plein");
  int fourchette = fork();
  if(fourchette<0)
  {
    perror("fork");
    exit(2);
  }
  else if(fourchette==0)
  {
    alice();
  }
  else
  {
    bob();
  }
}

/* ------------------------------------------------------------------------- */

/* Je dévie un peu de l'énoncé. L'index peut être maintenu manuellement en
   dehors de la structure, mais un peu de modularité ne fait pas de mal. */

#define N 5

typedef struct tampon {
     int   tab[N];
     sem_t libres;
     sem_t occupees;
} tampon_t;

tampon_t* tampon_init()
{
     /* on utilise un semaphore nommé pour effectuer une barrière de
        synchronisation entre le producteur et le consommateur. Plus
        précisément, on veut éviter que le consommateur commence à
        lire dans le buffer avant que le producteur finisse d'initialiser
        le tampon. Il y a peut-être des façons + élégantes de faire. */

     sem_t* barriere = sem_open("/barriere",
                                O_CREAT,
                                S_IRUSR | S_IWUSR,
                                0);

     printf("producteur: ouverture du sem. /barriere\n");

     if(barriere == SEM_FAILED) {
          perror("Erreur de sem_open");
          exit(1);
     }

     int shm_fd = shm_open("/tampon", O_CREAT | O_RDWR, S_IRUSR | S_IWUSR );

     printf("producteur: ouverture de /tampon\n");

     if(shm_fd == -1) {
          perror("Erreur de shm_open");
          exit(1);
     }

     if(ftruncate(shm_fd, sizeof(tampon_t)) == -1) {
          perror("Erreur de ftruncate");
          exit(1);
     }

     printf("producteur: ftruncate ok\n");

     tampon_t* t = (tampon_t*) mmap(NULL,
                                    sizeof(tampon_t),
                                    PROT_READ | PROT_WRITE,
                                    MAP_SHARED,
                                    shm_fd,
                                    0);

     if(t == MAP_FAILED) {
          perror("Erreur de mmap");
          exit(1);
     }

     for(int i = 0; i < N; i++)
          t->tab[i] = 0;

     sem_init(&(t->libres), 1, N);
     sem_init(&(t->occupees), 1, 0);

     printf("producteur: init ok\n");

     if(sem_post(barriere) < 0) {
          perror("Erreur de sem_wait");
          exit(1);
     }

     printf("producteur: init ok\n");

     if(sem_close(barriere) < 0) {
          perror("Erreur de sem_close");
          exit(1);
     }

     return t;
}

void producteur_boucle(tampon_t* t)
{
     int compteur = 0;

     while(1) {
          if(sem_wait(& (t->libres)) < 0) {
               perror("Erreur de sem_wait");
               exit(1);
          }

          const int rd = random();

          printf("producteur: ecriture de %d à l'indice %d\n", rd, compteur);

          t->tab[compteur] = rd;

          compteur = (compteur + 1) % N;

          if(sem_post(& (t->occupees)) < 0) {
               perror("Erreur de sem_wait");
               exit(1);
          }

          sleep(2);
     }
}

void consommateur()
{
     sem_t* barriere = sem_open("/barriere", O_CREAT, S_IRUSR | S_IWUSR, 0);

     if(barriere == SEM_FAILED) {
          perror("Erreur de sem_open");
          exit(1);
     }

     int val;

     sem_getvalue(barriere, &val);

     printf("consommateur: open /barriere ok, value = %d\n", val);

     if(sem_wait(barriere) < 0) {
          perror("Erreur de sem_wait");
          exit(1);
     }

     printf("consommateur: barriere ok\n");

     /* ici on est sur que le producteur a fini d'initaliser le buffer. */

     if(sem_unlink("/barriere") < 0) {
          perror("Erreur de sem_unlink");
          exit(1);
     }

     printf("consommateur: unlink ok\n");

     int shm_fd = shm_open("/tampon", O_RDWR, S_IRUSR | S_IWUSR );

     if(shm_fd == -1) {
          perror("Erreur de shm_open");
          exit(1);
     }

     tampon_t* t = (tampon_t*) mmap(NULL,
                                    sizeof(tampon_t),
                                    PROT_READ | PROT_WRITE,
                                    MAP_SHARED,
                                    shm_fd,
                                    0);

     if(t == MAP_FAILED) {
          perror("Erreur de mmap");
          exit(1);
     }

     int compteur = 0;

     while(1) {

          if(sem_wait(& (t->occupees)) < 0) {
               perror("Erreur de sem_wait");
               exit(1);
          }

          printf("consommateur: lecture de %d à l'indice %d\n", t->tab[compteur], compteur);

          compteur = (compteur + 1) % N;

          if(sem_post(& (t->libres)) < 0) {
               perror("Erreur de sem_wait");
               exit(1);
          }

          sleep(2);
     }

}

void exo2()
{
     pid_t pid = fork();

     if(pid == -1) {
          perror("Erreur de fork");
          exit(1);
     } else if(pid == 0) {
          consommateur();
     } else {
          tampon_t* t = tampon_init();
          producteur_boucle(t);
     }
}


/* ------------------------------------------------------------------------- */

int main(int argc, char** argv)
{
     exo2();

     return 0;
}
