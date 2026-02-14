//ex134.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   File4 file( cb, "TEXT.FIL" ) ;

   File4seqWrite writeFile ;
   char buffer[ 0x1FFF ] ;

   cb.lockAttempts = 1 ;
   if( file.lock( 0, file.len( ) ) == 0)
   {
      writeFile.init( file, file.len( ), buffer, sizeof( buffer ) ) ;
      // begin writing at the end of the file

      writeFile.write( "This is the end of the file." ) ;
      // perform lots of other writes to justify using a 7K buffer
      // ...
      // clear out the buffering and use write optimizations
      writeFile.flush( ) ;
      file.optimizeWrite( OPT4EXCLUSIVE ) ;
      writeFile.init(file, 0, buffer, sizeof( buffer ) ) ;
      cb.optStart( ) ;
      // do other writes.
   }
   file.close( ) ; // close and flush 'buffer' to disk
   cb.initUndo( ) ;
}
