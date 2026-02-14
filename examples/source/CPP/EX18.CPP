//ex18.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void openAFile( Code4 &cb )
{
  // 'd' falls out of scope.  Data file is still open
  Data4 d( cb, "INFO" ) ;
}

void main( )
{
   Code4 cb ;
   cb.autoOpen = 0 ;
   openAFile( cb ) ;

   Data4 d( cb, "DATAFILE" ) ; // open a second file
   cout << "Number of records in DATAFILE: " << d.recCount( ) << endl ;

   cb.closeAll( ) ; // INFO and DATAFILE are both closed
   cb.initUndo( ) ;
}
