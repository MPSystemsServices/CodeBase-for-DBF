//ex26.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 info( cb, "INFO" ) ;
   Data4 data( cb, "DATAFILE" ) ;

   info.lock( 1L ) ;
   data.lockAll( ) ;

   if ( cb.unlock() == r4success )
      cout << "Successfully unlocked" << endl ;

   cb.initUndo( ) ;
}
