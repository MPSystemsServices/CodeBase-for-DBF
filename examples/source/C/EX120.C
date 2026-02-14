/*ex120.c*/
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
   cb.autoOpen = 0 ; /* don't automatically open index file */

   data = d4open( &cb, "INFO" ) ;
   index = i4open( data, "INFO2" ) ; /* open a secondary index file */

   cb.lockAttempts = WAIT4EVER ; /* wait until the lock succeeds*/
   d4lockAll ( data )  ;
   if( i4reindex( index ) == 0 )
      printf( "Reindexed successfully\n" ) ;

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
