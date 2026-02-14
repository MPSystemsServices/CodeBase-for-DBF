//ex70.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
        // open exclusively to avoid multiuser corruption
   cb.accessMode = OPEN4DENY_RW ;

   Data4 data( cb, "INFO" ) ;
   data.go( 1L ) ;

   // Make all of the records in the data file the same as the first record

   for( long numRecs = data.recCount( ) ; numRecs-1 ; numRecs -- )
      if( data.write( numRecs ) != 0 )
         break ;

   data.close( ) ;
   cb.initUndo( ) ;
}
