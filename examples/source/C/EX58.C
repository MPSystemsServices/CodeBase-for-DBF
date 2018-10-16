#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

long recsInFile( CODE4 *cb, DATA4 *d )
{
   long count ;

   if( cb->errorCode ) return -1L ; /* an error occurred */

   count = d4recCount( d ) ; /* save the record count */

   return count ;
}

void main( )
{
   CODE4 cb ;
   DATA4 *data ;
   long count ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   count = recsInFile( &cb, data ) ;
   printf( "the number of records in the file is %d \n", count ) ;
   code4initUndo( &cb ) ;
}
