#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data, *extra ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;

   data = d4open( &cb, "INFO" ) ;

   /* Open the file exclusively, default optimization is the same as if
         d4optimize( data, OPT4EXCLUSIVE ) were called.*/
   /* open a shared file. */
   cb.accessMode = OPEN4DENY_NONE ;
   extra = d4open( &cb, "DATA" ) ;

   d4optimize( extra, OPT4ALL ) ;  /* read optimize the extra "DATA" file */

   code4optStart( &cb ) ;  /* Begin the memory optimizations.*/

   /* .... Some other code .... */
   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
