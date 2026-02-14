/* mutex4.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */


#include "d4all.h"


#if defined( S4SERVER ) && ( defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED ) )
   int mutex4initShare( MUTEX4 *mutex, const char *mutexName )
   {
      // used to create a shared mutex.  used between ODBC and CodeBase server to restrict access to such things as
      // the transaction file

      mutex->mutex = CreateMutex( 0, FALSE, mutexName ) ;
      if ( mutex->mutex == 0 )  // failed to create...
         return error4( 0, e4create, E96927 ) ;

      mutex->isValid = 1 ;
      return 0 ;
   }



   void mutex4initUndo( MUTEX4 *mutex )
   {
      if ( mutex->isValid == 0 )
         return ;

      int rc = CloseHandle( mutex->mutex ) ;
      if ( rc < 0 )
         error4( 0, e4create, E96927 ) ;

      mutex->isValid = 0 ;
   }
#endif /* #if defined( S4SERVER ) && ( defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED ) ) */
