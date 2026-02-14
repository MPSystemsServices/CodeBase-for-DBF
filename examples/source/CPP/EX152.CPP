//ex152.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4ptr string( "Fred") ;
   char FChar = (char) string ; // FChar now contains 'F'
   cout << FChar << string.str( ) << endl ;
}
