//ex177.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4ten trueFalse ;

   trueFalse.assign( "Y" ) ;

   #ifdef S4USE_TRUE
   int result =  trueFalse.true() ;  // Result is non-zero
   #else
   int result =  trueFalse.isTrue() ;  // Result is non-zero
   #endif
   cout << result << endl ;
}
