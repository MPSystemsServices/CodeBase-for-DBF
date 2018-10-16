//ex85.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 dt( "19901002" ) ;
   char  result[20] ;

   dt.format( result, "YY.MM.DD" ) ;   // 'result' will contain "90.10.02"
    cout << result << endl ;
   dt.format( result, "CCYY.MM.DD" ) ; // 'result' will contain "1990.10.02"
    cout << result << endl ;
   dt.format( result, "MM/DD/YY" ) ;   // 'result' will contain "10/02/90"
    cout << result << endl ;
   dt.format( result, "MMM DD/CCYY" ) ;// 'result' will contain "Oct 02/1990"
    cout << result << endl ;

   cout << dt.format( "MMM DD/CCYY" ) << endl ; // outputs 'Oct 02/1990'
}
