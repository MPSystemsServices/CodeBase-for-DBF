#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 code ;
   DATA4 *data ;
   EXPR4 *expression ;
   char badExpr[] = "NAME = 5" ;

   code4init( &code ) ;
   data = d4open( &code, "INFO" );
   expression = expr4parse( data, badExpr ) ;

   printf( "\nAn error message just displayed\n") ;

   code.errorCode = code.errExpr = 0 ;
   expression= expr4parse( data, badExpr ) ;
   printf("No error message displayed.\n") ;

   if ( expression )
      expr4free( expression ) ;

   code4initUndo( &code ) ;
}
