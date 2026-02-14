//ex12.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int startWithToday( Code4 &code, char *name )
{
   int oldLockAttempts = code.lockAttempts ;
   int oldOpt = code.optimize, oldOptWrite = code.optimizeWrite ;

   code.lockAttempts = WAIT4EVER ;
   code.optimize = OPT4ALL ;
   code.optimizeWrite = OPT4ALL ;

   Data4 d( code, name ) ;
   if( code.errorCode < 0 )
      return 0 ;
   d.lockAll( ) ; // lock the file for optimizations to take place

   Field4 dateField( d, "BIRTH_DATE" ) ;
   Date4 now ;
   now.today( ) ;
   code.optStart( ) ;
   for( d.top( ) ; ! d.eof( ) ; d.skip( ) )
      dateField.assign( now ) ;
   code.optSuspend( ) ;

   d.close( ) ;
   code.lockAttempts = oldLockAttempts ;
   code.optimize = oldOpt ; code.optimizeWrite = oldOptWrite ;
   return code.errorCode ;
}

void main()
{
   Code4 cb ;
   startWithToday( cb, "INFO" ) ;
   cb.initUndo() ;
}
