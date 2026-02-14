/*ex100.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void displayTheRecord( DATA4 *d )
{
   int numFields ;
   short curField ;
   FIELD4 *genericField ;

   numFields = d4numFields( d );

   for(curField = 1 ; curField <= numFields ; curField++ )
   {
      genericField = d4fieldJ( d, curField ) ;
      printf("%s\t", f4memoStr( genericField ) ) ;
   }
   printf( "\n" ) ;
}

void main()
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   d4top( data ) ;
   displayTheRecord( data ) ;
   code4initUndo( &cb ) ;
}
