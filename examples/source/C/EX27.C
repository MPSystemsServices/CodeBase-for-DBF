/*ex27.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

long function( char *string, int len )
{
   /*convert the field data into an 'int' */
   return c4atol( string, len) ;
}

void main()
{
   long retLong ;
   char string[] = "35476cat5" ;

   /* 'long result' will be "35" since it only converts the 2 characters as specified by the second parameter. */
   retLong = function( string, 2 ) ;

   printf("Return from c4atol() = %i\n\n", retLong) ;
   retLong = 0 ;

   retLong = function( string, strlen(string) ) ;
   printf("Return from c4atol() = %i\n", retLong) ;

   return ;
}
