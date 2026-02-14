/*ex92.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 cb ;
   DATA4 *data, *info ;
   EXPR4 *expr ;
   char *result ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA" ) ;
   info = d4open( &cb, "INFO" ) ;
   expr = expr4parse( data, "FNAME+' '+DTOS( INFO->BIRTH_DATE)" ) ;

   d4top( data ) ;
   d4top( info ) ;
   expr4vary( expr, &result ) ;
   printf( "First name from DATA and birth date from INFO: %s\n", result ) ;

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
