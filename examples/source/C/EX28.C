/*ex28.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char to[10] ;

   /* The result put into 'to' will be "C B A" */
   c4encode( to, "ABC", "3 2 1", "123" ) ;
   printf("Result = %s\n", to) ;

   /* The result put into 'to' will be "19901230" */
   c4encode( to, "30-12/1990", "789A4512", "123456789A" ) ;
   printf("Result = %s\n", to) ;

   /* The result put into 'to' will be "12/30/90" */
   c4encode( to, "19901230", "EF/GH/CD", "ABCDEFGH" ) ;
   printf("Result = %s\n", to) ;

   return ;
}