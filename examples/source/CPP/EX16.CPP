//ex16.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

Code4 cb ;  // create a global Code4 object

void main( )
{
   cb.accessMode = OPEN4DENY_RW ;  // open files exclusively
   cb.autoOpen = 0 ;  // do not automatically open index files.
   Data4 data( cb, "INFO" ) ;
   data.top( ) ;

   Str4flex string( cb ) ;
   string.assign( data.record( ), data.recWidth( ) ) ;

   cout << "Copy of the current Record Buffer: " << endl ;
   cout << string.str( ) << endl ;

   cb.closeAll( ) ;
   cb.accessMode = OPEN4DENY_NONE ; // let other users use the file as well
   data.open( cb, "DATAFILE" ) ;

   cb.initUndo( ) ;
}
