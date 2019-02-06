#include "signal.h"

int glov, gct;
extern int errno;
int errct;
#define SIGNUM 10



main()
{
  int i;

  printf("Test  1 ");

  for (i = 0; i < 15; i++) {
	test10();
	test11();
  }
  if (errct == 0)
	printf("ok\n");
  else
	printf(" %d errors\n", errct);
  exit(0);
}

test10()
{
  int i, n, pid;

  n = 4;
  for (i = 0; i < n; i++) {
	  if ( (pid=fork()) ) {
		if (pid < 0) { printf("\nTest 1 fork failed\n"); exit(1);}
		parent();
	  } else
		child(i);
  }
}

parent()
{

  int n;

  n = getpid();
  wait(&n);
}

child(i)
int i;
{
  int n;

  n = getpid();
  exit(i);
}

test11()
{
  int i, k, func();

  for (i = 0; i < 4; i++)  {
	glov = 0;
	signal(SIGNUM, func);
	if ( (k=fork())) {
		if (k < 0){printf("Test 1 fork failed\n"); exit(1);}
		parent1(k);
	} else
		child1(k);
  }
}


parent1(childpid)
int childpid;
{

  int n;

  for (n = 0; n < 5000; n++) ;
  while (kill(childpid, SIGNUM) < 0) /* null statement */ ;
  wait(&n);
}

func()
{
  glov++;
  gct++;
}
child1(k)
int k;
{
  while (glov == 0) ;
  exit(gct);
}



e(n)
int n;
{
  printf("\nError %d  errno=%d  ", n, errno);
  perror("");
 errct++;
}


