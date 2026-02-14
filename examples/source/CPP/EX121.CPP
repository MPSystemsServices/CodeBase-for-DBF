//ex121.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 testFile ;

   cb.safety = 0 ;     // overwrite existing file if it exists
   testFile.create( cb, "NUMBER.FIL" ) ;
   Str4ptr outText( "0123456789" ) ;

   testFile.write( 0, outText ) ;
   cout << "Length of the file is: " << testFile.len( ) << endl ;

   testFile.close( ) ;
   cb.initUndo( ) ;
}
