//ex66.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "NAMES" ) ;

  // Skip to the last record in the file whose NAME field contains "John"

   cb.optStart( ) ;

   Field4 name( data, "F_NAME" ) ;

   for( data.bottom( ) ;! data.bof( ) ; data.skip( -1L ) )
   {
      if( name.at(Str4ptr("John") ) != -1 ) // anything but -1 indicates a find
         break ;
   }
   if( data.bof( ) )
      cout << "John not located" << endl ;
   else
      cout << "The last John is in record: " << data.recNo( ) << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
