//ex81.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Date4 date ;
   Date4 dayBefore ;
   date.assign( "19900101" ) ;
   dayBefore.assign( (long)date - 1L ) ;
   char result[16] ;

   date.format(result,  "MMM DD, 'YY" );
   cout << result << " is after " ;
   dayBefore.format( result, "MMM DD, 'YY" );
   cout << result << endl ;
}
