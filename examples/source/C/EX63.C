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

   printf( "Reindexing %s Please Wait\n", d4alias( data ) ) ;
   if( d4reindex( data ) != 0 )
      printf( "Reindex NOT successful.\n ") ;
   else
      printf( "Successfully reindexed.\n") ;
   code4initUndo( &cb ) ;
}
