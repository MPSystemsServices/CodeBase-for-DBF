//ex171.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4ten info ;
   info.assign( "12345" ) ;

   char buf[3] ;

   //'buf' will contain "12\0". i.e. buf will contain the first
   // two characters of 'info' followed by a null character.

   info.ncpy( buf, sizeof(buf) ) ;
   cout << buf << endl ;
}
