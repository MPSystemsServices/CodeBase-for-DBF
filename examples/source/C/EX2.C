/*ex2.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info, *data;
   INDEX4 *infoIndex;

   code4init( &cb ) ;
   cb.autoOpen = 0 ;      /* Do not automatically open production index file. */
   cb.errOpen = 0 ;
   info = d4open( &cb, "INFO" ) ;

   infoIndex = i4open( info, "INFO" ) ;

   if( cb.errorCode < 0 )
      printf("Production index file is not opened.\n") ;

   /* DATA.DBF has a production index. Open it. */
   cb.autoOpen = 1 ;
   data = d4open( &cb, "DATA" ) ;

   /*  Some other code */

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
