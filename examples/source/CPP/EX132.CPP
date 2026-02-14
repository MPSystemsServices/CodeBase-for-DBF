//ex132.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;
void main( )
{
  Code4 cb ;
  char buffer[ 0x0400 ];
  cb.accessMode = OPEN4DENY_RW ; // use write optimizations.
  File4 file( cb, "TEXT.FIL" ) ;
  cb.optStart( ) ;

  File4seqWrite writeFile( file, 0, buffer, sizeof(buffer));

  writeFile.write( "Mary had a little lamb," ) ;
  writeFile.write( "little lamb," ) ;
  writeFile.write( "little lamb," ) ;
  writeFile.write( "Mary had a little lamb," ) ;
  writeFile.write( "Whose fleece was white as snow" ) ;

  writeFile.flush();

  file.close( ) ;
  cb.initUndo( ) ;
}
