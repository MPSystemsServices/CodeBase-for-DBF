//ex145.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 info( cb, "INFO" ) ;

   Relate4set TopMaster( info ) ;
   // ... other code ...

   // This relation tree is no longer needed. Create a new one
   TopMaster.free( ) ;
   TopMaster.init( info ) ;

   // ... other code ...
   TopMaster.free( 1 ) ; // Automatically close all files in the relation
   cb.closeAll( ) ;     // close any remaining files
   cb.initUndo( ) ;
}
