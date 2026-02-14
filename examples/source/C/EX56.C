#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   TAG4 *tag ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   tag = d4tagDefault( data ) ;

   error4exitTest( &cb ) ;
   d4tagSelect( data, tag ) ; /* select the default tag. */
   d4positionSet( data, .25 ) ; /* move one quarter through the index file.*/
   printf( "Record number: %d \n", d4recNo( data )) ;

   printf( "The current position is: %f \n", d4position( data )) ;

   code4initUndo( &cb ) ;
}
