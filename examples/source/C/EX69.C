/* ex69.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   TAG4 *nameTag, *defaultTag ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ; /* automatically open data & index file.*/
   nameTag = d4tag( data, "INF_NAME" ) ;
   defaultTag = d4tagDefault( data ) ;

   d4tagSelect( data, defaultTag ) ; /* Select the default tag */
   d4seekDouble( data, 32 ) ;    /* Seek using default tag 'PPL_AGE'*/

   d4tagSelect( data, nameTag ) ; /* Select the 'INF_NAME' tag*/
   d4seek( data, "Fred" ) ; /* Seek using 'INF_NAME */

   d4tagSelect( data, NULL ) ;  /* Select record ordering */
   d4seek( data, "ginger" );    /* The seek uses the default tag, which is
                                   INF_AGE, so this seek fails even though
                                   "ginger" is in the data file */

   code4initUndo( &cb ) ;
}
