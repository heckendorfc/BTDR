#ifndef COMMON_H
#define COMMON_H

#define INIT_MEM(x,y) if((x=malloc(sizeof(*x)*(y)))==NULL){fprintf(stderr,"Malloc of %d failed at %s:%d pid %d\n",(int)y,__FILE__,__LINE__,getpid());exit(22);}

#endif
