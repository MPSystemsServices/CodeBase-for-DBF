/* 'f4int()' uses 'c4atoi' because database field data is not null terminated */
#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int function( char *string, int len )
{
	/*convert the field data into an 'int' */
	return c4atoi( string, len ) ;
}


void main()
{
   int retInt ;
   char string[] = "12356cat5" ;

   retInt = function( string, 5 ) ;
   printf("Return from c4atoi() = %i\n\n", retInt) ;
   retInt = 0 ;

   retInt = function( string, strlen(string) ) ;
   printf("Return from c4atoi() = %i\n", retInt) ;

   return ;
}