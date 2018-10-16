/* ex109.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main()
{
   CODE4 cb ;
   FILE4 file ;

   code4init( &cb ) ;
   file4open( &file, &cb, "TEXT.FIL", 0 ) ;
   error4exitTest( &cb ) ;
   printf( "File name: %s\n", file4name( &file ) ) ;
   printf( "Length: %ld\n", file4len( &file ) ) ;
   code4initUndo( &cb ) ;
}
