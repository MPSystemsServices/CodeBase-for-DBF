//ex176.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4large test ;
   test.assign( "A " ) ;  // test.len() is 2
   test.add( " " ) ;      // test.len() becomes 3
   cout << "length before trim " << test.len( ) << endl ;

   test.trim() ;  // test.len() becomes 1
   cout << "length after trim " << test.len( ) << endl ;
}

