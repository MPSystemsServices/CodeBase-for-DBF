#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 dataFile ;

   settings.autoOpen = 0 ;
   settings.memSizeBlock = 0x800 ; // 2048
   settings.memSizeBuffer = 0x2000 ; // 8192

   settings.errDefaultUnique = r4unique ;
   dataFile.open( settings, "INFO" ) ;

   // this is equivalent to calling Code4::exitTest( )
   if( settings.errorCode )  settings.exit( ) ;

   // ...

   settings.initUndo( ) ;
}
