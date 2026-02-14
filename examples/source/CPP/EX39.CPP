//ex39.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // Borland compilers only

void main( )
{
   Code4 cb ;
   Data4 data ;

   cb.accessMode = OPEN4DENY_RW ; // open file exclusively to speed pack

   data.open( cb, "INFO" ) ;
   cb.exitTest( ) ;
   cb.optStart( ) ;

   for( data.top( ); ! data.eof( ); data.skip( 2 ) )
       data.deleteRec( ) ;  // Mark the record for deletion

   data.pack( ) ; // Physically remove the deleted records from the disk

   data.close( ) ;
   cb.initUndo( ) ;
}
