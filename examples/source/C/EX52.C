/*ex52.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;

   data = d4open( &cb, "INFO" ) ;

   if( data != NULL )
   {
      printf( "Data file \\INFO.DBF has %d records\n", d4recCount( data )) ;
      d4close( data ) ;
   }
   code4initUndo( &cb ) ;
}
