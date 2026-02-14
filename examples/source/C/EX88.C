#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   data = d4open( &cb, "FILE" ) ;
   error4exitTest( &cb ) ; /* the application will exit if FILE cannot be opened */

   /*... other code ... */

   code4initUndo( &cb ) ;
}
