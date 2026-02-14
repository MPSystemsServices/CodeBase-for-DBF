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
   data = d4open( &cb, "INFO" ) ;

   cb.lockAttempts = 1 ;
   cb.readLock = 1 ;

   rc =  d4go( data, 2L ) ;
   if ( rc == r4locked )
   {
      printf( "Record 2 was locked by another user.\n") ;
      cb.readLock = 0 ; /* Turn automatic locking off.*/

      rc = d4go( data, 2L ) ;
      if ( rc == r4locked )
      {
         printf("This will never happen because ") ;
         printf("'CODE4.readLock' is false.\n");
      }
   }
   d4close( data ) ;
   code4initUndo( &cb ) ;
 }
