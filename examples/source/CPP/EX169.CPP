//ex169.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   Str4ptr string( "This is some text   " ) ;

   cout << string.ptr( ) << " that is " << string.len( )
        << " characters long." << endl ;
   // the length is 20
   string.trim( ) ;
   // the length is now 17
   cout << "the length is now " << string.len( ) << endl ;
}
