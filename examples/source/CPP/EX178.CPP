//ex178.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4ptr string( "This is some text" ) ;
   string.upper( ) ;
   cout << string.ptr( ) << endl ;

   // This displays 'THIS IS SOME TEXT'
}
