//ex2.cpp
#include "d4all.hpp"

extern unsigned _stklen = 15000;

void main( )
{
   Code4 cb ;

   // Do not automatically open production index file
   cb.autoOpen = 0 ;
   Data4 info( cb, "INFO.DBF" ) ;
   Index4 infoIndex = info.index("INFO") ;

   if( ! infoIndex.isValid())
      cout <<"Production index file is not opened" << endl ;

   // DATA.DBF has a production index.  Open it
   cb.autoOpen = 1 ;
   Data4 data( cb, "DATA.DBF" ) ;

   //  Some other code

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
