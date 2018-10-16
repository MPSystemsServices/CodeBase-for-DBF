/*ex24.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

double function( void ) /* This function returns '(double) 67.3'. */
{
   double d ;

   /* only the first five characters are used */
   d = c4atod("67.37 Garbage", 5 ) ;

   return d ;
}

void main()
{
   double retDouble ;

   retDouble = function() ;
   printf("Return from c4atod() = %f\n", retDouble) ;

   return ;
}
