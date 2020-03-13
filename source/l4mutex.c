/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

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
      #ifdef E4LINK
         if ( listMutex->isValid == 0 || ((LINK4 *)item)->n != 0 || ((LINK4 *)item)->p != 0 )
         {
            error4( 0, e4parm, E96928 ) ;
            return ;
         }
      #endif

      if ( list4mutexWait( listMutex ) != 0 )
      {
         error4( 0, e4info, E96926 ) ;
         return ;
      }
      l4add( &listMutex->list, item ) ;
      list4mutexRelease( listMutex ) ;
   }



   // AS May 15/02 - for compression/preprocessing sometimes need to put messages back on the list
   void list4mutexAddTop( LIST4MUTEX *listMutex, void *item )
   {
      // same as list4mutexAdd but insert into the top of the list instead of appending at the end
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
      l4addBefore( &listMutex->list, l4first( &listMutex->list ), item ) ;
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
      #ifdef E4PARM_LOW
         if ( listMutex->isValid == 1 )
         {
            error4( 0, e4parm, E96928 ) ;
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
      #ifdef E4LINK
         // non-null if the list is a list-mutex - ensures we have mutexes held when manipulating lists
         listMutex->list.listMutex = listMutex ;
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

      #ifdef E4LINK
         // it is an error to uninitialize a list mutex which still has links on its chain
         // ( i.e. they didn't get removed, this is very bad )
         if ( listMutex->list.lastNode != 0 )
            error4( 0, e4parm, E96928 ) ;
      #endif

      memset( listMutex, 0, sizeof( LIST4MUTEX ) ) ;
      listMutex->isValid = 0 ;
   }



   #ifdef E4LINK
      S4EXPORT void list4mutexRelease( LIST4MUTEX *listMutex )
      {
         /*
            ERRORS

            In case of error, call error4( ) with a NULL CODE4 ( since one is not
              available ), and an error code of e4result.  This severe error
              indicates an unrecoverable failure, and indicates a major failure.

            used to just release the mutex associcated with the list
            this function is called if list4mutexWait( ) was called to gain control
         */

         #ifdef E4PARM_LOW
            if ( listMutex == 0 )
            {
               error4( 0, e4parm_null, E96928 ) ;
               return ;
            }
         #endif
         #ifdef E4PARM_LOW
            if ( listMutex->isValid == 0 )
            {
               error4( 0, e4parm, E96928 ) ;
               return ;
            }
         #endif

         if ( ReleaseMutex( listMutex->mutex ) == 0 )
            error4( 0, e4result, E96928 ) ;
         else
            list4mutexHeld( listMutex, 1 ) ;
      }
   #endif /* E4LINK */



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
      #ifdef E4PARM_LOW
         if ( listMutex->isValid == 0 )
         {
            error4( 0, e4parm, E96928 ) ;
            return 0 ;
         }
      #endif

      if ( list4mutexWait( listMutex ) != 0 )
         return 0 ;

      // AS July 18/02 - if no items on the list, return null...
      void *itemToRemove = l4first( &listMutex->list ) ;
      #ifdef E4LINK
         if ( itemToRemove == 0 && listMutex->list.lastNode != 0 )  // list has gone bad
            error4( 0, e4info, E96928 ) ;
      #endif
      void *item = 0 ;
      if ( itemToRemove != 0 )
         item = l4remove( &listMutex->list, itemToRemove ) ;

      list4mutexRelease( listMutex ) ;
      #ifdef E4LINK
         if ( item == 0 && listMutex->list.lastNode != 0 )  // list has gone bad
            error4( 0, e4info, E96928 ) ;
      #endif
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
      #ifdef E4PARM_LOW
         if ( listMutex->isValid == 0 )
         {
            error4( 0, e4parm, E96928 ) ;
            return ;
         }
      #endif

      if ( list4mutexWait( listMutex ) != 0 )
      {
         error4( 0, e4info, E96926 ) ;
         return ;
      }
      l4remove( &listMutex->list, link ) ;
      list4mutexRelease( listMutex ) ;
   }


   #ifdef E4LINK
      S4EXPORT int list4mutexWait( LIST4MUTEX *listMutex )
      {
         /*
          ERRORS

            In case of error, call error4( ) with a NULL CODE4 ( since one is not
              available ), and an error code of e4result.  This severe error
              indicates an unrecoverable failure, and indicates a major failure.

            used to just wait on the mutex associcated with the list
            after this call, list4mutexRelease( ) must be called to release control
            of the mutex so other threads may access the list
         */

         #ifdef E4PARM_LOW
            if ( listMutex == 0 )
               return error4( 0, e4result, E96931 ) ;
            if ( listMutex->isValid == 0 )
               return error4( 0, e4parm, E96928 ) ;
         #endif

         if ( WaitForSingleObject( listMutex->mutex, INFINITE ) == WAIT_FAILED )
            return error4( 0, e4result, E96931 ) ;
         else
            return list4mutexHeld( listMutex, 1 ) ;
      }
   #endif /* E4LINK */
#endif /* #if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD ) */
