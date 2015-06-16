# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include "AUC.h"

/* new function: 07/25/08 10:33:43 */
void AUC(int* length, double* x, double* y, double* res){
  int len,i;
  double w,a;
  len=*length;
  w = 0.0;
  a = 0.0;
  for(i=0;i< (len-1);i++){
    a=fabs(x[i]-x[i+1])*(fabs(y[i])+fabs(y[i+1]))/2;
    w=w+a;
  }
  *res=w;
}

