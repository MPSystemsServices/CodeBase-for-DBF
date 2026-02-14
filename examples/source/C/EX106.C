/*ex106.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 testFile ;

   code4init( &cb ) ;
   cb.safety = 0 ;
   file4create( &testFile, &cb, "TEMP.FIL", 0 ) ;
   code4optStart( &cb ) ;
   file4write( &testFile, 0, "Is this information written?", 27 ) ;
    /* Written to memory, not disk*/

   file4flush( &testFile ) ; /* Physically write to disk*/

   printf( "Flushing complete.\n") ;
   printf( "Check TEMP.FIL after you power off the computer.\n") ;

   code4initUndo( &cb ) ;
}

