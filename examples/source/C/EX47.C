#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4  cb ;
   DATA4 *data ;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA" ) ;

   cb.lockAttempts = 4 ; /* Try lock four times */

   rc = d4lock( data, 4 ) ;
   if( rc == r4success )
      printf( "Record 4 is now locked.\n") ;
   else if( rc == r4locked )
      printf( "Record 4 is locked by another user.\n" ) ;

   cb.lockAttempts = WAIT4EVER ; /* Try forever, d4lock will not return r4locked */
   rc = d4lock( data, 4 ) ;

   if ( rc == r4locked )
      printf( "This should never Happen\n") ;

   d4close( data ) ;
   code4initUndo( &cb ) ;
 }
