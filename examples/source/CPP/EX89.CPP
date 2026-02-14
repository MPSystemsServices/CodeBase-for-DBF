#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 d ;
   d.today( ) ;

   cout << "Today is " << d.str( ) << endl ;  // displays in CCYYMMDD
}
