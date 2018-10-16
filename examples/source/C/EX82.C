/*ex82.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   long yesterday ;
   char today[8], tomorrow[8], result[13] ;

   date4today( today ) ; /* Get the current date from the system clock*/

   yesterday = date4long( today ) - 1L ;

   date4assign( tomorrow, yesterday + 2L );
   date4format( today, result, "MMM DD, CCYY" ) ;
   printf( "Today is %s\n", result ) ;
   printf( "The Julian date for yesterday is %d\n", yesterday ) ;
   printf( "The Julian date for tomorrow is %d\n", date4long( tomorrow )) ;
}
