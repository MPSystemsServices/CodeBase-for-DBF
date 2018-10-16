//ex168.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   Str4ptr alphabet( "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ) ;
   Str4ten  firstTen, middleTen, lastTen ;

   // 'firstTen' will contain "ABCDEFGHIJ"
   firstTen.assign( alphabet.left( 10 ) ) ;
   cout << firstTen.str( ) << endl ;

   // 'middleTen' will contain "IJKLMNOPQR"
   middleTen.assign( alphabet.substr(8,10) ) ;
   cout << middleTen.str( ) << endl ;

   // 'lastTen' will contain "QRSTUVWXYZ"
   lastTen.assign( alphabet.right( 10 ) ) ;
   cout << lastTen.str( ) << endl ;
}
