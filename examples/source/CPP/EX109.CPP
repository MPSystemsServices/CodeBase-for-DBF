//ex109.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Field4info fields( data ) ;

   if( data.numFields( ) == fields.numFields( ) )
      cout << "A copy has been made of the fields" << endl ;
   else
      cout << "An error must have occurred." << endl ;

   cb.initUndo( ) ;
}
