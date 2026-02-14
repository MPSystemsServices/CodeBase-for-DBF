//ex180.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 codeBase ;
   Str4flex flex( codeBase ) ;

   flex.assign( "This is some text" ) ;
   cout << flex.str( ) << " and its length is " << flex.len( ) << endl ;

   flex.free( ) ;
   cout << "Length is now " << flex.len( ) << endl ;
   codeBase.initUndo( ) ;
}
