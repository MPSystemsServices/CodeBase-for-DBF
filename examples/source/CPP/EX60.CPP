//ex60.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 codeBase ;
   Data4 dataFile( codeBase, "INFO" ) ;

   dataFile.optimize( OPT4ALL ) ;
   codeBase.optStart( ) ;

   dataFile.top( ) ;
   cout << "Press Enter when you want to refresh your data." << endl ;
   getchar( ) ;

   dataFile.refresh( ) ;
   dataFile.top( ) ;  // re-read the record from disk.

   cout << "The latest information is:" << dataFile.record( ) << endl ;

   dataFile.close( ) ;
   codeBase.initUndo( ) ;
}
