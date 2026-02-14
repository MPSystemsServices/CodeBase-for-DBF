//ex44.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 data( settings, "INFO" ) ;

   // Since Code4::autoOpen is by default TRUE, the INFO index file
   // should have been opened.

   Index4 index ;

   index = data.index( "INFO" ) ;

   if( index.isValid( ) )
      cout << "INDEX: INFO has been opened" << endl ;

   index = data.index( "JUNK" ) ;

   if( !index.isValid( ) )
      cout << "INDEX: JUNK has not been opened" << endl ;

   data.close( ) ;
   settings.initUndo( ) ;
}
