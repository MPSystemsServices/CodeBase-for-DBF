#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;

	code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ; /* open file exclusively to speed pack*/

   data = d4open( &cb, "DATABASE" ) ;

   error4exitTest( &cb ) ;
   code4optStart( &cb ) ;

   for( d4top( data ); ! d4eof( data ); d4skip( data, 2 ) )

   d4delete( data ) ;        /* Mark the record for deletion*/

   /*  Physically remove the deleted records from the disk*/

   d4pack( data ) ;

   d4close( data ) ;

   code4initUndo( &cb ) ;
}
