//ex54.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // Borland compilers only

void main( )
{
   Code4 cb ;
   Data4 data ;

   cb.accessMode = OPEN4DENY_RW ; // open file exclusively

   data.open( cb, "INFO" ) ;
   cb.exitTest( ) ;
   data.lockAll( ) ;
   cb.optStart( ) ;

   // Mark every other record for deletion
   for( data.top( ); ! data.eof( ); data.skip( 2 ) )
      data.deleteRec( ) ;

   // Remove the deleted records from the physical disk
   data.pack( ) ;

   data.close( ) ;
   cb.initUndo( ) ;
}
