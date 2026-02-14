//ex114.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void displayTheRecord( Data4 d )
{
   int numFields = d.numFields( ), curField = 1 ;
   Field4memo genericField ;

   for( ; curField <= numFields; curField++ )
   {
      genericField.init( d, curField ) ;
      cout << genericField.str( ) << "\t" ;
   }
   cout << endl ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "DATA2" ) ;
   data.top( ) ;
   displayTheRecord( data ) ;
   cb.initUndo( ) ;
}
