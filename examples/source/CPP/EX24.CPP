//ex24.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 code ;
   code.accessMode = OPEN4DENY_RW ;

   Data4 dataFile( code, "INFO" ) ;
   code.exitTest( ) ;

   // initialize optimization with default settings.
   code.optStart( ) ;
   int delCount = 0 ;
   for( int rc = dataFile.top( ); rc == r4success ; rc = dataFile.skip( ) )
      if( dataFile.deleted( ) )
         delCount++ ;

   cout << delCount << " records are marked for deletion." << endl ;
   code.initUndo( ) ;
}
