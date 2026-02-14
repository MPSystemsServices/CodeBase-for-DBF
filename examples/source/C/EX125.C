/*ex125.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info ;
   RELATE4 *TopMaster ;

   code4init( &cb ) ;
   info = d4open( &cb, "INFO" ) ;

   TopMaster = relate4init( info ) ;

   /* ... other code ...*/

   /* This relation tree is no longer needed. Create a new one. */
   relate4free( TopMaster, 0 ) ;

   TopMaster = relate4init( info ) ;

   /* ... other code ... */

   relate4free( TopMaster, 1 ) ;  /* Automatically close all files in the relation. */

   code4close( &cb ) ;     /* Close any remaining files. */
   code4initUndo( &cb ) ;
}
