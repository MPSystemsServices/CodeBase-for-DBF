/*ex78.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char today[8] ;

   date4today( today ) ;
   printf( "The current month is %s\n", date4cmonth( today )) ;
   /* displays "The current month is January" if the system clock says
         that it is. */
}
