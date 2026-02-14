//ex101.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void displayFieldStats( Field4 f )
{
   Data4 db = f.data( ) ;

   cout << "-----------------------------------------------------" << endl
        << "Database: " << db.alias( ) << " Field: " << f.name( )  << endl
        << "Length: " << f.len( ) << "     Type: " << f.type( )    << endl
        << "Decimals: " << f.decimals( )                           << endl
        << "-----------------------------------------------------" << endl ;
   return ;
}

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Data4 data( cb, "INFO" ) ;
   Field4 field( data, "NAME" ) ;
   displayFieldStats( field ) ;
   cb.initUndo( ) ;
}
