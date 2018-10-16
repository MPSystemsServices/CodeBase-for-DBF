//ex75.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Field4 bdate( data, "BIRTH_DATE" ) ;

   data.top( ) ;
   Date4 dt1( bdate.ptr( ) ) ;
   Date4 dt2( "JANUARY 12, 1990", "MMMMMMM DD, CCYY" ) ;

   if( dt1 > dt2 )
   {
      cout << "First record is after " << dt2.format( "MMM DD, CCYY" ) << endl;
      // displays 'First record is after JAN 12, 1990'
      if( dt1 == bdate )
         cout << "This is always true." << endl ;
   }
   cb.initUndo( ) ;
}
