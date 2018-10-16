//ex164.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( void )
{
   Str4large str ;
   str.assignLong( 17L,4 ) ;
   cout << str.ptr( ) << endl ;    // displays "  17"
   // Put '0' characters in front
   str.assignLong( 38L,4,1 ) ;
   cout << str.ptr( ) << endl ;    // displays "0038"
}
