//ex167.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   char buf[300] ;
   Str4max str( buf, sizeof(buf) ) ;

   str.assign( "Information" ) ;
   str.insert( Str4ptr("My "), 0 ) ;
   str.insert( Str4ptr("Insert "), 3 ) ;

   // 'str' now contains "My Insert Information"
   cout << str.str( ) << endl ;
}
