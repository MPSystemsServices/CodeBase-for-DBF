//ex151.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   Code4 cb ;

   Str4flex wideString( cb ) ;
   wideString.setLen( 2000 ) ;
   wideString.set( 'S' ) ;

   // Fill up the entire screen
   cout << wideString.ptr() << endl ;

   Str4large s1 ;
   s1.assign( "Test String" ) ;

   Str4ptr test( "Test String" ) ;
   if ( s1 == test )
      cout << "This is always True !" << endl ;

   cb.initUndo( ) ;
}
