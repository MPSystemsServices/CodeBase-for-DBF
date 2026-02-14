//ex138.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   cb.autoOpen = 0 ; // don't automatically open index file
   Data4 data( cb, "INFO" ) ;

   Index4 index ;
   index.open( data, "INFO2" ) ; // open a secondary index file

   cb.lockAttempts = WAIT4EVER ; // wait until the lock succeeds
   data.lockAll( ) ;
   if( index.reindex( ) == 0 )
      cout << "Reindexed successfully" << endl ;

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
