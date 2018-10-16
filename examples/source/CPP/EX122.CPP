//ex122.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 test ;

   if( test.open( cb, "TEXT.FIL") < 0 )
      cb.exit( ) ;

   cb.lockAttempts = 2 ; // attempt a lock 2 times
   if( test.lock( 0, LONG_MAX ) != 0 )
   {
      cout << "Unable to lock the file" << endl ;
      test.close( ) ;
      cb.initUndo( ) ;
      cb.exit( ) ;
   }
   // double the file size for fun
   test.setLen( test.len( ) * 2 ) ;

   test.close( ) ; // File4::close automatically unlocks the file
   cb.initUndo( ) ;
}
