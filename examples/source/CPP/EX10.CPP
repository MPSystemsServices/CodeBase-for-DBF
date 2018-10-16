#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   cb.readLock = 1 ;
   cb.lockAttempts = 3 ;

   if( data.top( ) == r4locked ) 
   {
      cout << "Top record locked by another user" << endl ;
      cout << "Lock attempted "<< cb.lockAttempts << " time(s)" << endl;
   }
   cb.initUndo( ) ;
}
