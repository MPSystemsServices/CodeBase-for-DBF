#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Str4large str1 ;

   str1.assign( "Roses are red,\nViolets are blue,\nSugar is " );
   str1.add( "sweet,\nAnd so are you!" ) ;

   cout << str1.ptr( ) << endl << endl  ;
   cout << "(This poem has " << str1.len( ) << " characters)" << endl ;
}
