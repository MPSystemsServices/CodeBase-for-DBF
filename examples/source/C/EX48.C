#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 code ;
   DATA4 *df ;
   int rc ;

   code4init( &code ) ;
   df = d4open( &code, "INFO" ) ;

   rc = d4lockAll( df ) ;

   if( rc == r4success )
      printf( "Lock is successful.\n ") ;
   code4initUndo( &code ) ;
}
