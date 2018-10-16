//ex175.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Str4flex info(cb), result(cb) ;

   info.assign( "INFORMATION" ) ;
   result.assign( info.substr( 0,4 ) ) ;  // result now contains 'INFO'
   cout << result.str( ) << endl ;
   cb.initUndo( ) ;
}
