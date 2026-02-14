#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   cb.readLock = 1 ;
   cb.lockAttempts = 3 ;

   data = d4open( &cb, "INFO" ) ;
   if( d4top( data ) == r4locked )
   {
      printf("Top record locked by another user.\n") ;
      printf("Lock attempted %d time(s)\n", cb.lockAttempts ) ;
   }
   code4initUndo( &cb ) ;
}
