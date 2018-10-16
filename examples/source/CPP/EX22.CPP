//ex22.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000;

void main( )
{
   Code4 cb ;
   Data4 data( cb, "FILE" ) ;
   cb.exitTest( ) ; // the application will exit if FILE cannot be opened

   // ... other code ...
   cb.initUndo( ) ;
}
