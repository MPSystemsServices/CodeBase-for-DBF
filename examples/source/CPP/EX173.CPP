//ex173.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4large s ;
   s.setLen( 80 ) ;
   s.set( '*' ) ;   // Set to eighty blanks
   cout << s.str( ) << s.len( ) << endl ;
}
