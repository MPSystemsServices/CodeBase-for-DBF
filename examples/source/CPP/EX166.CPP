//ex166.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( void )
{
   Str4large str ;
   /* The result in str will be "C B A" */
   str.encode( "ABC", "3 2 1", "123" ) ;
   cout << str.str( ) << endl ;

   /* The result in str will be "A&B&C" */
   str.encode( "ABC", "1&2&3", "123" ) ;
   cout << str.str( ) << endl ;

   /* The result in str will be "19901230" */
   str.encode( "30-12/1990", "789A4512", "123456789A" ) ;
   cout << str.str( ) << endl ;

   /* The result in str will be "12/30/90" */
   str.encode( "19901230", "EF/GH/CD", "ABCDEFGH" ) ;
   cout << str.str( ) << endl ;
}
