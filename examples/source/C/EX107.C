/*ex107.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 testFile ;

   code4init( &cb ) ;
   cb.safety = 0 ;

   /* overwrite existing file if it exists */
   file4create( &testFile, &cb, "NUMBER.FIL", 0 ) ;

   file4write( &testFile, 0, "0123456789", sizeof( "0123456789" ) ) ;
   printf( "Length of the file is: %ld\n", file4len( &testFile ) ) ;

   file4close( &testFile ) ;
   code4initUndo( &cb ) ;
}
