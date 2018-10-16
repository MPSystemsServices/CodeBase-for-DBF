#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   int i ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA" ) ;
   error4exitTest( &cb );

   /* Add 20 blank records	 */
   for( i = 20 ; i ; i-- )
      d4appendBlank( data ) ;

   /* Close the data file.*/
   d4close( data ) ;

   code4initUndo( &cb ) ; /* Free up any memory used. */
}
