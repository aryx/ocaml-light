#include "config.h"

// when don't have APE, have to fake those for now.

#ifdef OS_PLAN9

int errno = -1;

int myopen(char*, int, int) { return -1; }

int unlink(char*)           { return -1;}
int rename(char*, char*)    { return -1; }
int getcwd(char*, int)      { return 0; }
int system(char*)           { return -1; }
char * strerror(int n)      { return "ERROR"; }
int sscanf(const char *, const char *, ...) { return -1; }
void signal(int, void (*)()) { }

#endif
