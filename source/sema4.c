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

/* sema4.c   (c)Copyright Sequiter Software Inc., 1988-2006.  All rights reserved. */

/*If E4PARM_LOW is defined, the functions in this module will ensure that
  their paramaters are not invalid.  This error will be generated even
  for those functions which otherwise say that no error is generated.*/

/* the SEMAPHORE4 module contains some operating system specific coding
   for the actual implementation of a semaphore */

#include "d4all.h"

#ifdef S4COM_PRINT
   void S4FUNCTION debug4display( const char *str ) ;
#endif

#if !defined( S4OFF_THREAD ) || defined( S4SEMA4LOCK )
   int S4FUNCTION semaphore4init( SEMAPHORE4 *semaphore )
   {
      /* PARAMATERS

         semaphore is the semaphore to initialize

         ERRORS

         In case of error, call error4() with a NULL CODE4 ( since one is not
           available ), and an error code of e4result.  This severe error
           indicates an unrecoverable failure, and indicates a major failure.

         NOTES

         This function does whatever intialization is required to create a valid
           semaphore.

         In S4WIN32, this function calls the WIN32 API function CreateSemaphore
           to create a valid semaphore handle, and stores that handle in the
           SEMAPHORE4 structure.

         RETURNS

         r4success
         < 0 error
      */

      #ifdef E4PARM_LOW
         if ( semaphore == 0 )
            return error4( 0, e4parmNull, E96934 ) ;
      #endif

      semaphore->handle = CreateSemaphore( 0, 0, INT_MAX, 0 ) ;

      if ( semaphore->handle == 0 )
         return error4( 0, e4semaphore, E96934 ) ;

      #ifdef S4COM_PRINT
         char buffer[128] ;
         sprintf( buffer, "Semaphore4init: %ld", (long)( semaphore->handle ) ) ;
         debug4display( buffer ) ;
      #endif

      return r4success ;
   }



   int S4FUNCTION semaphore4initShared( SEMAPHORE4 *semaphore, const char *sema4name )
   {
      /* DESCRIPTION

         Used to initialize a shared semaphore.  For example, used to share between the ODBC and CodeBase Server
           engines some critical access to certain files, etc.

         PARAMATERS

         semaphore is the semaphore to initialize
         sema4name is the name of the semaphore

         ERRORS

         In case of error, call error4() with a NULL CODE4 ( since one is not
           available ), and an error code of e4result.  This severe error
           indicates an unrecoverable failure, and indicates a major failure.

         NOTES

         This function does whatever intialization is required to create a valid
           semaphore.

         In S4WIN32, this function calls the WIN32 API function CreateSemaphore
           to create a valid semaphore handle, and stores that handle in the
           SEMAPHORE4 structure.

         RETURNS

         r4success
         < 0 error
      */

      #ifdef E4PARM_LOW
         if ( semaphore == 0 )
            return error4( 0, e4parmNull, E96934 ) ;
      #endif

      semaphore->handle = CreateSemaphore( 0, 0, INT_MAX, sema4name ) ;

      if ( semaphore->handle == 0 )
         return error4( 0, e4semaphore, E96934 ) ;

      #ifdef S4COM_PRINT
         char buffer[128] ;
         sprintf( buffer, "Semaphore4init: %ld", (long)( semaphore->handle ) ) ;
         debug4display( buffer ) ;
      #endif

      return r4success ;
   }



   int S4FUNCTION semaphore4initUndo( SEMAPHORE4 *semaphore )
   {
      /* PARAMATERS

         semaphore is the semaphore to uninitialize

         ERRORS

         ignore any errors.

         NOTES

         This function does whatever unintialization is required to create a valid
           semaphore.

         In S4WIN32, this function calls the WIN32 API function CloseHandle on
           SEMAPHORE4.handle

         RETURNS

         r4success
         < 0 error
      */

      int rc ;

      #ifdef E4PARM_LOW
         if ( semaphore == 0 )
            return ( error4( 0, e4parmNull, E96935 ) ) ;
      #endif

      #ifdef S4COM_PRINT
         char buffer[128] ;
         sprintf( buffer, "Semaphore4initUndo: %ld", (long)( semaphore->handle ) ) ;
         debug4display( buffer ) ;
      #endif
      rc = CloseHandle( semaphore->handle ) ;
      if ( rc ==FALSE )
      {
         #ifdef S4COM_PRINT
            sprintf( buffer, "*** semaphore4InitUndo FAILED: %ld", (long)rc ) ;
            debug4display( buffer ) ;
         #endif

         return e4semaphore ;
      }
      semaphore->handle = 0 ;
      return r4success ;
   }



   /*
      void S4FUNCTION semaphore4release( SEMAPHORE4 *semaphore )
      {
         ERRORS

         In case of error, call error4() with a NULL CODE4 ( since one is not
           available ), and an error code of e4result.  This severe error
           indicates an unrecoverable failure, and indicates a major failure.

         NOTES

         This function is used to release a semaphore, which indirectly causes any
           thread waiting on the SEMAPHORE4 to be activated and able to respond to
           this semaphore.

         In S4WIN32, this function calls the WIN32 API function ReleaseSemaphore
           on the semaphore handle to release the semaphore.

         #ifdef E4PARM_LOW
            if ( semaphore == 0 )
            {
               error4( 0, e4parmNull, E96936 )  ;
               return ;
            }
         #endif

         ( ReleaseSemaphore( semaphore->handle, 1, 0 ) == FALSE ? error4( 0, e4semaphore, E96936 ) : 0 ) ;
      }
   */



   /*
      int S4FUNCTION semaphore4wait ( SEMAPHORE4 *semaphore, int waitSecs )
      {
         PARAMATERS

         semaphore is the semaphore to wait on
         waitSecs is the number of seconds to wait on the semaphore.  A value of
           zero means no wait, a value of WAIT4EVER means an infinite wait.

         ERRORS

         In case of E4PARM_LOW error, call error4() with a NULL CODE4 ( since one is
           not available ), and an error code of e4result.  This severe error
           indicates an unrecoverable failure, and indicates a major failure.

         If the Wait fails due to error, return FALSE.  This is because worker
           threads expect to just fail out when the semphore is deleted ( see
           server4worker() ).

         RETURNS

         > 0 means the semaphore was released
         0 means the semaphore was not released and doWait was not infinite

         NOTES

         This function is used to suspend a thread until the given SEMAPHORE4 has
           been releaseed or the timeout has elapsed

         in S4WIN32, this function calls the WIN32 API function
           WaitForSingleObject() on SEMAPHORE4.handle

         #ifdef E4PARM_LOW
            if ( semaphore == 0 )
               return( error4( 0, e4parm, E96937 ) ) ;
         #endif

         return ( WaitForSingleObject( semaphore->handle, waitSecs < 0 ? INFINITE : waitSecs*1000 ) == WAIT_OBJECT_0 ? 1 : 0 ) ;
      }
   */
#endif /* #if !defined( S4OFF_THREAD ) || defined( S4SEMA4LOCK ) */
