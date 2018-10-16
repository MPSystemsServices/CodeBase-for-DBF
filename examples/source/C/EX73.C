/*ex73.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data;
   long numRecs ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ; /* open exclusively to avoid corruption*/
   data = d4open( &cb, "INFO1" ) ;
   d4top( data ) ;

   /* Make all of the records in the data file the same as the first record*/

   for( numRecs = d4recCount( data ) ; numRecs > 1 ; numRecs-- )
      if( d4write( data, numRecs ) != 0 )
         break ;

   d4close( data ) ;
   code4initUndo( &cb ) ;
}
