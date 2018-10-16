#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 cb ;
   FILE4 temp ;

   code4init( &cb ) ;
   cb.errCreate = 0 ;

   if( file4create( &temp, &cb, "NEWFILE.TXT", 1 ) == r4noCreate)
      /* File exists. Try in the temp directory.*/
      file4create( &temp, &cb, "C:\\temp\\NEWFILE.TXT", 1 ) ;

   if( cb.errorCode < 0 )
      code4exit( &cb ) ;

   /* Some other code*/

   code4initUndo( &cb ) ;
}
