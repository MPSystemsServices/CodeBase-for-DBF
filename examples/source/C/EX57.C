#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

long recallAll( DATA4 *d, int lockTries )
{
   TAG4 *saveSelected ;
   long count ;

   saveSelected = d4tagSelected( d ) ;
   d4tagSelect( d, NULL ) ; /* use record ordering */

   d4lockAll( d ) ;
   count = 0 ;

   for( d4top( d ) ;  !d4eof( d ) ; d4skip( d, 1 ) )
   {
       d4recall( d ) ;
       count++ ;
   }

   d4tagSelect( d, saveSelected ) ; /* reset the selected tag.*/
   return count ;
}

void main()
{
   CODE4 cb ;
   DATA4 *data ;
   int lockTries = WAIT4EVER ;
   long count  ;

   code4init( &cb ) ;
   data = d4open( &cb , "INFO" ) ;
   count = recallAll( data, lockTries ) ;
   printf( "record count is %d \n", count ) ;
   code4initUndo( &cb ) ;
}
