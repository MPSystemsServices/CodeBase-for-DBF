//ex179.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   Str4char chO( 'o') ;
   Str4char charNull( ) ;
   Str4char charZero('0') ;
   Str4char charSp(' ') ;

   Str4ten anotherString( "Hell" );
   anotherString.add( chO ) ;
   anotherString.add( charSp ) ;
   anotherString.add( "Mom" ) ;
   cout << anotherString.str() << endl ;
}
