/*ex104.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 autoexec ;

   code4init( &cb ) ;
   file4open( &autoexec, &cb, "C:\\AUTOEXEC.BAT", 0 ) ;

   file4lock( &autoexec, 0, file4len( &autoexec ) ) ; /* lock the entire file*/

   /* ... some other code */

   file4close( &autoexec ) ; /* save changes and close the file*/
   code4initUndo( &cb ) ;
}
