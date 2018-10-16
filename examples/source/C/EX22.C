#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data;

   code4init( &cb );
   data = d4open( &cb, "INFO") ;

   d4lockAll( data ) ;
   d4optimizeWrite( data, OPT4ALL ) ;

   code4optStart( &cb ) ;

   /* ... some other code */

   code4optSuspend( &cb ) ; /* flush & free optimization memory.*/

   d4unlock( data ) ; /* let other users make modifications.*/

   /* ... some other code */

   d4lockAll( data ) ;

   code4optStart( &cb ) ;

   /* ... other code */

   code4initUndo( &cb ) ;
}
