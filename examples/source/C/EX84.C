/*ex84.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char time[9] ;

   date4timeNow( time ) ;
   time[8] = 0 ;  /* Null-terminate the string. */
   printf( "The current time is %s.\n", time ) ;
}
