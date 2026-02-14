#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

int addToFile( Code4 &cb, File4 &file, Str4 &string )
{
   int oldLockAttempts = cb.lockAttempts ;
   cb.lockAttempts = 1 ;
   long fileSize = file.len( ) ;

   if( file.lock( fileSize, LONG_MAX ) == r4locked )
   {
      cout << "Cannot add to file, another user is writing" << endl ;
      cb.lockAttempts = oldLockAttempts ;
      return 1 ;
   }
   // lock succeeded, I may add to the file without corrupting anyone else's
   // writes

   file.write( fileSize, string ) ;

   file.unlock( fileSize, LONG_MAX ) ;
   cb.lockAttempts = oldLockAttempts ;
   return 0 ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "TEXT.FIL" ) ;
   Str4large info( "adding a string to the file" ) ;
   addToFile( cb, file, info ) ;
   cb.initUndo( ) ;
}
