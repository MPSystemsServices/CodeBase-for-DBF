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

/* mutex4.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */


#include "d4all.h"

// AS Nov 11/02 - Used now in Windows CE for shared locking

#ifdef S4MUTEX4LOCK
   int mutex4initShare( MUTEX4 *mutex, const char *mutexName )
   {
      // used to create a shared mutex.  used between ODBC and CodeBase server to restrict access to such things as
      // the transaction file

      #ifdef S4WINCE /* LY 2002/11/13 : different params and mutex functions under CE */
         unsigned short uBuff[LEN4PATH] ;

         memset( uBuff, 0, LEN4PATH*sizeof(unsigned short) ) ;
         c4atou( mutexName, uBuff, strlen(mutexName) ) ;

         mutex->mutex = CreateMutex( 0, FALSE, uBuff ) ;
         if ( mutex->mutex == 0 )  // failed to create or open existing
            return error4( 0, e4create, E96927 ) ;
      #else
         mutex->mutex = CreateMutex( 0, FALSE, mutexName ) ;
         if ( mutex->mutex == 0 )  // failed to create... try opening it instead
         {
            mutex->mutex = OpenMutex( MUTEX_ALL_ACCESS, FALSE, mutexName ) ;
            if ( mutex->mutex == 0 )
               return error4( 0, e4create, E96927 ) ;
         }
      #endif

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



   void mutex4release( MUTEX4 *mutex )
   {
      DWORD rc = ReleaseMutex( mutex->mutex ) ;
      if ( rc == 0 )
         error4( 0, e4result, E96928 ) ;

      #ifdef E4ANALYZE
         mutex->lockCount-- ;
      #endif
   }



   int mutex4wait( MUTEX4 *mutex, long waitSecs )
   {
      #ifdef E4PARM_LOW
         if ( mutex->isValid == 0 )
            return error4( 0, e4parm, E96931 ) ;
      #endif

      DWORD timeout ;
      if ( waitSecs == -1 )
         timeout = INFINITE ;
      else
         timeout = 1000 * waitSecs ;

      DWORD rc = WaitForSingleObject( mutex->mutex, timeout ) ;
      switch ( rc )
      {
         case WAIT_TIMEOUT:
            return r4locked ;
         case WAIT_FAILED:
            return error4( 0, e4parm, E96931 ) ;
         case WAIT_OBJECT_0:  // success
            #ifdef E4ANALYZE
               mutex->lockCount++ ;
            #endif
            break ;
         default:
            return error4( 0, e4result, E96931 ) ;
      }

      return 0 ;
   }
#endif
