//ex133.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( int argc, char **argv )
{
   Code4 cb ;
   char buffer[ 0x1FFF ] ;
   File4 file( cb, "TEXT.FIL" ) ;
   File4seqWrite seqFile( file, 0, buffer, sizeof( buffer ) ) ;

   // write 1-100 to file as integers
   int i ;
   Str4len strI( &i, sizeof(i) ) ;
   for( i = 1 ; i <=100 ; i++ )
      seqFile << (int)strI ;

   // write the program's name and the number of parameters to the file
   seqFile << Str4ptr( argv[0] ) << Str4len( &argc, sizeof(int) ) ;

   seqFile.flush( ) ; // flush the changes and close the file
   file.close( ) ;
   cb.initUndo( ) ;
}
