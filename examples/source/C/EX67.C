/*ex67.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 settings ;
   DATA4 *info;
   TAG4 *tag ;

   code4init( &settings ) ;
   info = d4open( &settings, "INFO" ) ;

   /* List the names of the tags in any production index file corresponding
      to "INFO" */

   printf( "Production index file tags for data file: %s\n", d4alias( info ) ) ;
         /*d4tagNext returns the first tag when NULL is passed to it */
   for( tag = d4tagNext(info, NULL); tag != NULL ; tag = d4tagNext( info, tag ))
      printf( "Tag name: %s\n", t4alias( tag ) ) ;

   code4initUndo( &settings ) ;
}
