/*ex7.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   d4go( data, d4recCount( data ) + 1 ) ;

   printf("\nAn error message was displayed\n") ;
   cb.errorCode = cb.errGo = 0 ;

   d4go( data, d4recCount( data ) + 1 ) ;
   printf("No error message was displayed\n") ;

   code4initUndo( &cb ) ;
}
