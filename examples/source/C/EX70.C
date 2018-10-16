/* ex70.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   TAG4 *tag ;

   code4init( &cb ) ;
   data = d4open( &cb, "DBF" ) ;
   tag = d4tag( data, "DBF_NAME" ) ; /* A tag with '.NOT.DELETED()' filter */

   d4top( data ) ; /* Position to the first record that is not deleted.*/
   d4delete( data ) ; /* The current record no longer is located in the tag*/

   d4tagSync( data, tag );/*The change to the record is flushed, and the datafile                       is positioned on a valid record.*/
   d4skip( data, 1L) ;
   /*... some other code ...*/

   code4initUndo( &cb ) ;
}
