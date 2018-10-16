/*ex85.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char d[8] ;
   int daysToWeekEnd ;

   date4today( d ) ;
   printf("Today is %s\n", date4cdow( d )) ;

   daysToWeekEnd = 7 - date4dow( d ) ;
   if( daysToWeekEnd == 0 || daysToWeekEnd == 6 )
      printf("Better enjoy it!\n") ;
   else
      printf("Only %d more to go till the weekend.\n", daysToWeekEnd ) ;
}
