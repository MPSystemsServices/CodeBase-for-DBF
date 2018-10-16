/*ex83.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

static int daysInMonth[] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 } ;

void main( )
{
   int endOfMonth  ;
   char today[8] ;

   date4today( today ) ;
   endOfMonth  = daysInMonth[ date4month( today ) ] ;
   if( date4month( today ) == 2 && date4isLeap( today ) )
      endOfMonth++ ;
   printf("there are %d days left till the end of the month\n",
                           endOfMonth - date4day( today )) ;
}
