//ex162.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main()
{
   Code4 codeBase ;
   Str4flex outString( codeBase ) ;

   outString.add( "An " ) ;
   outString.add( "output " ) ;
   Str4ptr temp( "String" ) ;
   outString.add( temp ) ;

   // Null-terminate the string.
   Str4char strNull( 0 ) ;
   outString.add( strNull ) ;

   cout << outString.ptr( ) << endl ;
   codeBase.initUndo( ) ;
}
