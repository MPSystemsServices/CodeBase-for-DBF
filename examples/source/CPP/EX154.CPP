//ex154.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4len str("123.5", 5 ) ;
   int i1 = (int) str ;             // i1 becomes 123
   int i2 = (int) Str4ptr( "14.7"); // i2 becomes  14
   str.assign( "21a.4") ;
   int i3 = (int) str ;             // i3 becomes  21
   cout << i1 << endl ;
   cout << i2 << endl ;
   cout << i3 << endl ;
}
