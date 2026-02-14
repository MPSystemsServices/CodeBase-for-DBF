//ex159.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4ptr strBob1( "Bob" ) ;
   Str4ptr strBob2( "Bob" ) ;
   Str4ptr strBobby("Bobby" ) ;
   Str4ptr strabcdef( "abcdef" ) ;
   Str4ptr strabb("abb" ) ;

   // The result of the comparison is placed in r1, r2 and r3
   int r1 = strBob1 > strBobby ; // r1 = 0
   int r2 = strBob1 > strBob2 ;  // r2 = 0
   int r3 = strabcdef > strabb ; // r3 = 1
   cout << r1 << endl ;
   cout << r2 << endl ;
   cout << r3 << endl ;
}
