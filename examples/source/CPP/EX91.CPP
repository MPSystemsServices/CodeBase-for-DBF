//ex91.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Field4 bdate( data, "BIRTH_DATE") ;
   data.top( ) ;

   Date4 date1( bdate.ptr( ) ) ; // make a copy of the field's contents
   data.skip( ) ;
   Date4 date2( bdate.ptr( ) ) ;// make a copy of the field's contents

   if( date1.year( ) == date2.year( ) )
     cout << "The people in the first and second records were both born in "
          << date1.year( ) << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
