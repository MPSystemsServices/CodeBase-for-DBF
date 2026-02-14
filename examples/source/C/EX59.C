#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   TAG4 *tag ;
   long count = 0 ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   tag = d4tagDefault( data ) ;

   d4tagSelect( data, tag  ) ;  /* select the default tag*/

   for( d4top( data ); !d4eof( data ) ; d4skip( data, 1 ) )
   {
       printf( "Tag position: %d\n", ++count ) ;
       printf( "      Record Position: %d\n",d4recNo( data ) ) ;
   }
   code4initUndo( &cb ) ;
}
