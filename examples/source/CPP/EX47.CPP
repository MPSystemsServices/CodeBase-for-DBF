//ex47.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main( )
{
   Code4 code ;
   Data4 df( code, "INFO" ) ;

   int rc = df.lockAll( ) ;

   if( rc == r4success )
      cout << "Lock is successful." << endl ;
   else
      cout << "Lock was unsuccessful" << endl ;

  code.initUndo( ) ;
}
