//ex172.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4large name ;
   name.assign( "John Smith" ) ;

   // The contents of 'name' become "John Johnson"
   name.replace( Str4ten("Johnson"), 5 ) ;
   cout << name.str( ) << endl ;
}
