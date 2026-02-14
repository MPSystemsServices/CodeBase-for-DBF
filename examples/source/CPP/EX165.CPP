//ex165.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( void )
{
   Str4ten info, searchValue ;

   info.assign( "ABCDEF" ) ;
   searchValue.assign( "CDF" ) ;

   int pos =  info.at( searchValue ) ; // 'pos' becomes -1
   cout << pos << endl ;
   searchValue.assign( "CDE" ) ;
   pos = info.at( searchValue ) ;      // 'pos' becomes 2
   cout << pos << endl ;
}
