//ex78.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main( )
{
   Date4 independence = "17760704" ;
   Date4 dayAfter = independence + 1L ;
   Date4 dayBefore = independence - 1L ;

   cout << "Independence Day originally was on a "
        << independence.cdow( ) << endl ;

   cout << "The day after was "
        << dayAfter.format( "MMMM DD, 'YY" ) << endl ;

   cout << "The day before was "
        << dayBefore.format( "MMMM DD, 'YY" ) << endl ;
}
