/* l4mutex.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/*If E4PARM_LOW is defined, the functions in this module will ensure that
  their paramaters are not invalid.  This error will be generated even
  for those functions which otherwise say that no error is generated. */

#include "d4all.h"

#if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD )
   #ifdef S4COM_PRINT
      void S4FUNCTION debug4display( const char *str ) ;
   #endif



   void list4mutexAdd( LIST4MUTEX *listMutex, void *item )
   {
      /* ERRORS

         No errors are possible in this module
      */

      #ifdef E4PARM_LOW
         if ( ( listMutex == 0 ) || ( item == 0 ) )
         {
            error4( 0, e4parm_null, E96926 ) ;
            return ;
         }
      #endif

      list4mutexWait( listMutex ) ;
      l4add( &listMutex->list, item ) ;
      list4mutexRelease( listMutex ) ;
   }



   void list4mutexInit( LIST4MUTEX *listMutex )  /* initialize a mutex object */
   {
      /* ERRORS

         In case of error, call error4( ) with a NULL CODE4 ( since one is not
           available ), and an error code of e4result.  This severe error
           indicates an unrecoverable failure, and indicates a major failure.
      */

      #ifdef E4PARM_LOW
         if ( listMutex == 0 )
         {
            error4( 0, e4parm_null, E96927 ) ;
            return ;
         }
      #endif

      memset( listMutex, 0, sizeof( LIST4MUTEX ) ) ;
      listMutex->mutex = CreateMutex( 0, FALSE, 0 ) ;
      if ( listMutex->mutex == 0 )  // failed to create...
      {
         error4( 0, e4create, E96927 ) ;
         return ;
      }
      listMutex->isValid = 1 ;

      #ifdef S4COM_PRINT
         char buffer[128] ;
         sprintf( buffer, "List4mutexInit: %ld", ( long )( listMutex->mutex ) ) ;
         debug4display( buffer ) ;
      #endif
   }



   void list4mutexInitUndo( LIST4MUTEX *listMutex )  /* uninit a mutex object */
   {
      /* ERRORS

         ignore any errors.
      */

      #ifdef S4COM_PRINT
         char buffer[128] ;
         sprintf( buffer, "List4mutexInitUndo: %ld", ( long )( listMutex->mutex ) ) ;
         debug4display( buffer ) ;
      #endif

      if ( listMutex->isValid == 0 )
         return ;

      int rc = CloseHandle( listMutex->mutex ) ;
      #ifdef S4COM_PRINT
         if ( !rc )
         {
            sprintf( buffer, "*** list4mutexInitUndo FAILED: %ld", ( long )rc ) ;
            debug4display( buffer ) ;
         }
      #endif

      #ifdef E4ANALYZE
         // it is an error to uninitialize a list mutex which still has links on its chain
         // ( i.e. they didn't get removed, this is very bad )
         if ( listMutex->list.lastNode != 0 )
            error4( 0, e4parm, E96928 ) ;
      #endif

      memset( listMutex, 0, sizeof( LIST4MUTEX ) ) ;
      listMutex->isValid = 0 ;
   }



   /*
      S4EXPORT void list4mutexRelease( LIST4MUTEX *listMutex )
      {
         ERRORS

         In case of error, call error4( ) with a NULL CODE4 ( since one is not
           available ), and an error code of e4result.  This severe error
           indicates an unrecoverable failure, and indicates a major failure.

         used to just release the mutex associcated with the list
         this function is called if list4mutexWait( ) was called to gain control

         #ifdef E4PARM_LOW
            if ( listMutex == 0 )
            {
               error4( 0, e4parm_null, E96928 ) ;
               return ;
            }
         #endif

          ( ( !ReleaseMutex( listMutex->mutex ) ) ? error4( 0, e4result, E96928 ) : 0 )
      }
   */



   void *list4mutexRemove( LIST4MUTEX *listMutex )
   {
      /* ERRORS

         No errors are possible in this module
      */

      #ifdef E4PARM_LOW
         if ( listMutex == 0 )
         {
            error4( 0, e4result, E96929 ) ;
            return 0 ;
         }
      #endif

      list4mutexWait( listMutex ) ;

      void *item = l4remove( &listMutex->list, l4first( &listMutex->list ) ) ;

      list4mutexRelease( listMutex ) ;
      return item ;
   }



   void list4mutexRemoveLink( LIST4MUTEX *listMutex, LINK4 *link )
   {
      /* ERRORS

         No errors are possible in this module
      */

      #ifdef E4PARM_LOW
         if ( listMutex == 0 )
         {
            error4( 0, e4result, E96930 ) ;
            return ;
         }
      #endif

      list4mutexWait( listMutex ) ;
      l4remove( &listMutex->list, link ) ;
      list4mutexRelease( listMutex ) ;
   }


   /*
      S4EXPORT void list4mutexWait( LIST4MUTEX *listMutex )
      {

       ERRORS

         In case of error, call error4( ) with a NULL CODE4 ( since one is not
           available ), and an error code of e4result.  This severe error
           indicates an unrecoverable failure, and indicates a major failure.

         used to just wait on the mutex associcated with the list
         after this call, list4mutexRelease( ) must be called to release control
         of the mutex so other threads may access the list

         #ifdef E4PARM_LOW
            if ( listMutex == 0 )
            {
               error4( 0, e4result, E96931 ) ;
               return ;
            }
         #endif

         ( listMutex->isValid == 0 ) ? error4( 0, e4parm, E96931 ) :
           ( ( WaitForSingleObject( listMutex->mutex, INFINITE ) == WAIT_FAILED ) ?  error4( 0, e4result, E96931 ) : 0 ) )
      }
   */
#endif /* #if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD ) */
