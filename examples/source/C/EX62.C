#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void update( DATA4 *d )
{
   if( d4changed( d, -1 ) )
      printf( "Changes not discarded.\n") ;
   else
      d4refreshRecord( d ) ;
}

void main()
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA") ;
   d4top( data ) ;
   update( data ) ;
   code4initUndo( &cb ) ;
}
