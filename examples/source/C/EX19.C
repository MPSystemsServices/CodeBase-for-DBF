#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void exitToSystem( CODE4 *cb )
{
   printf("\nShutting down application ... \n") ;
   code4close( cb ) ;
   code4exit( cb ) ;
}

void main( )
{
   CODE4 cb ;

   code4init ( &cb ) ;
   exitToSystem( &cb ) ;
}
