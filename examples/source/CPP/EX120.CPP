//ex120.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 testFile ;

   cb.safety = 0 ;
   testFile.create( cb, "TEMP.FIL", 0 ) ;
   cb.optStart( ) ;
   testFile.write( 0, "Is this information written?", 27 ) ;
   // Written to memory, not disk

   testFile.flush( ) ; // Physically write to disk

   cout << "Flushing complete.  "
        << "Check TEMP.FIL after you power off the computer." << endl ;
   cb.initUndo( ) ;
}
