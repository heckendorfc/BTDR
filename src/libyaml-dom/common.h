#ifndef COMMON_H
#define COMMON_H

#ifdef FAIL_LOUD
#define INIT_MEM(x,y) if((x=malloc(sizeof(*x)*(y)))==NULL){fprintf(stderr,"Malloc of %d failed at %s:%d pid %d\n",(int)y,__FILE__,__LINE__,getpid());exit(22);}
#else
#define INIT_MEM(x,y) (x=malloc(sizeof(*x)*(y)))
#endif

#endif
