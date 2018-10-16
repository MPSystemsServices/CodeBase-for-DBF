//ex35.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   data.top() ;
   cout << "Changed status: " << data.changed( ) << endl ;  // Displays 0
   data.lockAll( ) ;
   Field4( data, 1 ).assign("TEMP DATA") ;
   cout << "Changed status: " << data.changed( ) << endl ;  // Displays 1
   data.changed( 0 ) ;

   data.close( ) ;
   // The top record is not flushed.
   cb.initUndo( ) ;
}
