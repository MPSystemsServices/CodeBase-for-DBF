#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Str4ten str1 ;
   str1.setLen( 10 ) ;
   str1.set( ' ' ) ;

   cout << "Enter Password (up to 10 characters): " ;
   cin >> str1.ptr( ) ;

   str1.upper( ) ;
   str1.trim( ) ;
   cout << endl << str1.ptr( ) << " was entered " << endl  ;
   cout << "which has a length of " << str1.len( ) << endl ;
}
