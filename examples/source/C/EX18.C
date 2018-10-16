#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void openAFile( CODE4 *cb )
{
   DATA4 *d ;

   /* 'd' falls out of scope.  Data file is still open*/
   d = d4open( cb, "INFO" ) ;
}


void main( void )
{
   CODE4 cb ;
	DATA4 *d ;

   code4init( &cb ) ;
   openAFile( &cb ) ;

   d = code4data( &cb, "INFO" ) ; /* obtain a new DATA4 structure*/

   if( d != NULL )
   {
      printf("INFO has %d records.\n", d4recCount( d )) ;
      d4top( d ) ;
      d4close( code4data( &cb, "INFO") ) ; /*an alternative way to close the file*/
   }

   code4initUndo( &cb ) ;
}
