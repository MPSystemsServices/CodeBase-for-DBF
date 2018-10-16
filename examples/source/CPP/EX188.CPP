//ex188.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;

   // open the datafile but not the index file
   cb.autoOpen = 0 ;
   Data4 data( cb, "INFO" ) ;

   Tag4info tags( cb ) ;
   tags.add( "retired", "age", "age>55", 0, r4descending) ;
   tags.add( "inf_name", "name", 0, r4uniqueContinue ) ;

   Index4 index ;
   cb.safety = 0 ; // overwrite an existing file
   index.create( data, "INFO2", tags.tags( ) ) ;
   if( cb.errorCode )
      cout << "An error occurred" << endl ;

   cb.initUndo( ) ; // Tag4info destructor called
}
