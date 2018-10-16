//ex163.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( void )
{
   Str4large str ;
   str.assign( "assign info" ) ;
   str.assign( Str4ptr( "assign info" ));
   cout << str.str( ) << endl ;
}
