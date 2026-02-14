#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 test ;

   code4init( &cb ) ;
   if( file4open( &test, &cb, "TEXT.FIL", 0 ) < 0 )
      code4exit( &cb ) ;
   cb.lockAttempts = 20 ; /* attempt a lock 20 times*/
   if( file4lock( &test, 0, LONG_MAX ) != 0 )
   {
      printf( "Unable to lock the file\n" ) ;
      file4close( &test ) ;
      code4initUndo( &cb ) ;
      code4exit( &cb ) ;
   }
   /* double the file size for fun */
   file4lenSet( &test, file4len( &test ) * 2 ) ;
   printf( "The new length of the file is %ld.\n", file4len( &test ) ) ;
   file4close( &test ) ; /* file4close automatically unlocks the file. */
   code4initUndo( &cb ) ;
}
