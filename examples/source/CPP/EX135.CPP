//ex135.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;

   // automatically open the INFO production index file
   Data4 data( cb, "INFO" ) ;

   // Copy the Index4 object for the production index file
   Index4 info = data.index( "INFO" ) ;

   cb.errOpen = 0 ;
   Index4 names( data, "NOT" ) ; // attempt a second index file
   if( !names.isValid( ))
   {
      cout << "NOT index file not opened" << endl ;
      cout << (names.index == NULL ? "NULL": "NOT NULL") << endl ;
   }

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
