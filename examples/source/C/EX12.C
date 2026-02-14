#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main ()
{
   CODE4 code ;
   DATA4 *d ;
   FIELD4 *dateField ;

   char today[8];
   int oldLockAttempts, oldOpt, oldOptWrite ;
   code4init( &code ) ;
   oldLockAttempts = code.lockAttempts ;

   oldOpt = code.optimize;
   oldOptWrite = code.optimizeWrite ;

   code.lockAttempts = WAIT4EVER ;
   code.optimize = OPT4ALL ;
   code.optimizeWrite = OPT4ALL ;

   d = d4open( &code, "DATEFILE" ) ;
   if( code.errorCode < 0 )
      code4exit( &code ) ;

   d4lockAll( d ) ; /* lock the file for optimizations to take place*/

   dateField = d4field( d, "DATE" ) ;
   date4today( today );

   code4optStart( &code ) ;

   for( d4top( d ) ; ! d4eof( d ) ; d4skip( d, 1L ) )
      f4assign( dateField, today ) ;

   code4optSuspend( &code ) ;

   d4close( d ) ;

   code.lockAttempts = oldLockAttempts ;
   code.optimize = oldOpt ;
   code.optimizeWrite = oldOptWrite ;

   code4initUndo( &code );
}
