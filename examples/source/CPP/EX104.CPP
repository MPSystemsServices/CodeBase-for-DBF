//ex104.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void displayField( Field4 field )
{
   cout << field.name( ) << "   " << field.str( ) << endl;
}

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Data4 data( cb, "INFO" ) ;
   Field4 field( data, "NAME" ) ;
   data.top( ) ;
   displayField( field ) ;
   cb.initUndo( ) ;
}
