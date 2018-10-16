#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *prices ;
   TAG4 *tag ;

   code4init( &cb );
   cb.readOnly = 1 ;

   /* open a file on a drive without write access*/
   prices = d4open( &cb, " W:\\INFO.DBF" ) ;

   error4exitTest( &cb ) ;

   tag = d4tagDefault( prices ) ;
   d4tagSelect( prices, tag ) ;

   if( d4seek( prices, "SMITH" ) == 0 )
      printf( "SMITH is found\n") ;
   else
      printf( "SMITH is not found\n" ) ;
   code4initUndo( &cb ) ;
}
