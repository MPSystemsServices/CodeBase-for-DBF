//ex51.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 data ;  // Do not open the file with the constructor

   cb.accessMode = OPEN4DENY_RW ;
   int rc = data.open( cb, "INFO" ) ;

   if( rc == r4success )
   {
      cout << "Data file INFO.DBF has " << data.recCount( ) ;
      cout << " records. " << endl ;
      data.close( ) ;
   }
   cb.initUndo( ) ;
}
