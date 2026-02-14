/*ex110.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main()
{
   CODE4 cb ;
   FILE4 file ;
   char buf[11] ;
   int pos ;

   code4init( &cb ) ;
   file4open( &file, &cb, "TEXT.FIL", 0 ) ;

   memset( buf, 0, sizeof( buf ) ) ; /* ensure null termination for output */
   pos = file4read( &file, 0L, buf, sizeof( buf ) - 1 ) ;
   if( cb.errorCode < 0 ) return ;
      printf( "%s\n", buf ) ;
   code4initUndo( &cb ) ;
}
