//ex102.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Data4 data( cb, "INFO" ) ;
   Field4 field ;

   // Display all of the field names
   for( int i = 1 ; i <= data.numFields( ) ; i++ )
   {
      field.init( data, i ) ;
      cout << field.name( ) << endl ;
   }

   data.close( ) ;
   cb.initUndo( ) ;
}
