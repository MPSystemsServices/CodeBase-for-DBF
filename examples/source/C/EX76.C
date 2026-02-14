/*ex76.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char date[8]= "19900101" ;
   char dayBefore[8], result[11], resultBefore[11] ;

   date4assign( dayBefore, date4long(date) - 1L ) ;
   date4format( date, result, "MMM DD, 'YY") ;
   date4format( dayBefore, resultBefore, "MMM DD, 'YY") ;
    printf("%s is after %s\n", result, resultBefore);
}
