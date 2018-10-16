//ex184.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Data4 data( cb, "INFO" ) ;
   cb.exitTest( ) ;

   // get the reference to the "INF_NAME" tag
   Tag4 tag( data, "INF_NAME" ) ;

   data.select( tag ) ; // select the "INF_NAME" ordering
   for( data.top( ) ; !data.eof( ) ; data.skip( ) )
   {
      Field4 field( data, "NAME" ) ;
      cout << field.str( ) << endl ; // List out the NAMES
   }
   data.close( ) ;
   cb.initUndo( ) ;
}
