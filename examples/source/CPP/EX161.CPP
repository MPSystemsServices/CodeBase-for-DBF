//ex161.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4ptr string("BOB's") ;
   cout << string.str( ) << endl ; // outputs 'BOB's'
   string[4] = 'S' ;
   cout << string.str( ) << endl ; // outputs 'BOB'S'
}
