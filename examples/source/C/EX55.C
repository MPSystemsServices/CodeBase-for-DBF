#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ; /* open file exclusively*/

   data = d4open( &cb, "INFO" ) ;
   error4exitTest( &cb ) ;
   code4optStart( &cb ) ;

   for( d4top( data ); ! d4eof( data ); d4skip( data, 2 ) )
      d4delete( data ) ;       /* Mark every other record for deletion*/

     /* Remove the deleted records from the physical disk */
   d4pack( data ) ;

   d4close( data ) ;
   code4initUndo( &cb ) ;
}
