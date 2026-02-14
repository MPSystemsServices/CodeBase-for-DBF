#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   INDEX4 *index ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;

   /* Since CODE4.autoOpen is by default true (non-zero),
      the INFO index file should have been opened.*/
   index = d4index( data, "INFO" ) ;
   if ( index != NULL )
      printf( "INDEX: INFO has been opened\n") ;

   index = d4index( data, "JUNK" ) ;
   if( index == NULL )
      printf( "INDEX: JUNK has not been opened\n") ;

   d4close( data ) ;
   code4initUndo( &cb ) ;
}
