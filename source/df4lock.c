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

/* df4lock.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#if !defined( S4OFF_MULTI ) && !defined( S4CLIENT )
   #ifndef S4INDEX_OFF
      int S4FUNCTION d4lockTestIndexExport( DATA4 *data )
      {
         // required for ODBC
         return dfile4lockTestIndex( data->dataFile, 1L ) ;
      }


      int S4FUNCTION d4lockIndexExport( DATA4 *data )
      {
         // required for ODBC
         return d4lockIndex( data ) ;
      }
   #endif
#endif

#ifndef S4SINGLE
   #ifdef S4CLIENT
      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lock( DATA4FILE *data, const long lockId, const long serverId, const long rec, Lock4type lockType )
      {
         int rc ;
         CODE4 *c4 ;
         CONNECTION4 *connection ;
         CONNECTION4LOCK_INFO_IN *info ;
         long recNum ;
         #ifdef S4FOX
            FILE4LONG pos2 ;
         #endif

         #ifdef E4PARM_LOW
            if ( data == 0 || rec < 1L || lockId == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any )
                 #ifdef S4SERVER
                    || serverId == 0
                 #endif
               )
               return error4( (data == 0 ? 0 : data->c4 ), e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;

         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         if ( data->lockTestLockId != 0 && code4unlockAuto( c4 ) != LOCK4ALL )
         {
            if ( c4->lockAttempts == WAIT4EVER )
               return error4( c4, e4lock, E81523 ) ;
            else
               return r4locked ;
         }

         connection = data->connection ;
         if ( connection == 0 )
            rc = e4connection ;
         else
         {
            connection4assign( connection, CON4LOCK, lockId, data->serverId ) ;
            connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
            info->type = htons5(LOCK4RECORD) ;
            // AS Nov 30/01 - The lock type needs to be put into byte order
            // info->lockType = lockType ;
            info->lockType = htonl5(lockType) ;
            recNum = htonl5(rec) ;
            connection4addData( connection, &recNum, sizeof( recNum ), NULL ) ;
            rc = connection4repeat( connection ) ;

            if ( rc < 0 )
               connection4error( connection, c4, rc, E91102 ) ;
         }
         return rc ;
      }
   #else
      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lock( DATA4FILE *data, const long lockId, const long serverId, const long rec )
      {
         /* does not check for existing lock (by us) (assumes this has already been tested) */
         #ifdef E4PARM_LOW
            if ( data == 0 || rec < 1L || lockId == 0
                 #ifdef S4SERVER
                    || serverId == 0
                 #endif
               )
               return error4( (data == 0 ? 0 : data->c4 ), e4parm, E91102 ) ;
         #endif

         CODE4 *c4 = data->c4 ;

         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         int rc = 0 ;

         // AS Jul 30/02 - don't physically lock if opened exclusively
         if ( data->file.lowAccessMode != OPEN4DENY_RW )
         {
            FILE4LONG position ;
            file4longAssign( position, 0, c4->largeFileOffset ) ;

            // AS Jun 19/06 - if large file offset not null, support in clipper...
            #ifdef S4CLIPPER
               if ( c4->largeFileOffset == 0 )
                  file4longAdd( &position, L4LOCK_POS ) ;
               file4longAdd( &position, rec ) ;
            #endif
            #ifdef S4MDX
               // AS Nov 4/03 - large file support
               if ( c4->largeFileOffset == 0 )
               {
                  file4longAdd( &position, L4LOCK_POS ) ;
                  file4longSubtract( &position, rec + 1U ) ;
               }
               else
                  file4longAdd( &position, rec ) ;
            #endif
            #ifdef S4FOX
               if ( c4->largeFileOffset == 0 )
               {
                  #ifdef E4ANALYZE
                     assert5( ( data->compatibility == 30 && data->version == 0x30 ) || data->compatibility != 30 ) ;
                  #endif
                  if ( ( data->hasMdxMemo & 0x01 ) || data->version == 0x30 )
                  {
                     file4longAdd( &position, L4LOCK_POS ) ;
                     file4longSubtract( &position, rec ) ;
                  }
                  else
                  {
                     FILE4LONG pos2 ;
                     file4longAdd( &position, L4LOCK_POS_OLD ) ;
                     file4longAssignLong( pos2, dfile4recordPosition( data, rec ) ) ;
                     file4longAddLong( &position, &pos2 ) ;
                  }
               }
               else
                  file4longAdd( &position, rec ) ;
            #endif

            #ifdef S4MDX
               // AS Nov 4/03 don't bother with this extra locking when large-file offset set (i.e. improve performance)
               if ( c4->largeFileOffset == 0 )
                  rc = file4lockInternal( &data->file, L4LOCK_POS - 1U, 0, 1L, 0 ) ;
               if ( rc == 0 )
               {
            #endif
                  rc = file4lockInternal( &data->file, file4longGetLo( position ), file4longGetHi( position ), 1L, 0 ) ;
            #ifdef S4MDX
                  if ( c4->largeFileOffset == 0 )
                     file4unlockInternal( &data->file, L4LOCK_POS - 1U, 0, 1L, 0 ) ;
               }
            #endif
            if ( rc )
            {
               if ( rc == r4locked )
                  dfile4registerLocked( data, rec, 0 ) ;
               return rc ;
            }
         }

         data->recordLockWriteCount++ ;
         #ifdef S4SERVER_GUI
            if ( rc == 0 )
               c4->server->info.lockCount++ ;
         #endif
         return 0 ;
      }
   #endif /* S4CLIENT */

   #if !defined( S4CLIENT ) && !defined( S4CLIPPER ) && !defined( S4OFF_MEMO )
      int dfile4lockMemo( DATA4FILE *data )
      {
         return memo4fileLock( &data->memoFile ) ;
      }
   #endif



   #ifdef S4CLIENT
      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockAppend( DATA4FILE *data, const long lockId, const long serverId )
      {
         int rc ;
         CODE4 *c4 ;
         CONNECTION4LOCK_INFO_IN *info ;
         CONNECTION4 *connection ;

         #ifdef E4PARM_LOW
            if ( data == 0 || lockId == 0 )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return -1 ;

         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         if ( data->lockTestLockId != 0 && code4unlockAuto( c4 ) != LOCK4ALL )
         {
            if ( c4->lockAttempts == WAIT4EVER )
               return error4( c4, e4lock, E81523 ) ;
            else
               return r4locked ;
         }

         connection = data->connection ;
         if ( connection == 0 )
            return e4connection ;
         connection4assign( connection, CON4LOCK, lockId, data->serverId ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
         info->type = htons5(LOCK4APPEND) ;
         // AS Nov 30/01 - The lock type needs to be put into byte order
         // info->lockType = lock4write ;
         info->lockType = htonl5(lock4write) ;
         rc = connection4repeat( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E91102 ) ;

         return rc ;
      }
   #endif /* S4CLIENT */


   #ifndef S4CLIENT
      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockAppend( DATA4FILE *data, const long lockId, const long serverId )
      {
         /* assumes that all testing for locks is complete, just does the actual lock */
         int rc ;
         CODE4 *c4 ;

         #ifdef E4PARM_LOW
            if ( data == 0 || lockId == 0
                 #ifdef S4SERVER
                    || serverId == 0
                 #endif
               )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return -1 ;

         // AS Jul 30/02 - don't physically lock if opened exclusively
         if ( data->file.lowAccessMode == OPEN4DENY_RW )
            rc = 0 ;
         else
         {
            if ( c4->largeFileOffset == 0 )
            {
               #ifdef S4FOX
                  if ( ( data->hasMdxMemo & 0x01 ) || data->compatibility == 30 )
                     rc = file4lockInternal( &data->file, L4LOCK_POS, c4->largeFileOffset, 1L, 0 ) ;
                  else
                     rc = file4lockInternal( &data->file, L4LOCK_POS_OLD, c4->largeFileOffset, 1L, 0 ) ;
               #else
                  rc = file4lockInternal( &data->file, L4LOCK_POS, c4->largeFileOffset, 1L, 0 ) ;
               #endif
            }
            else  /* use 0 byte for append record */
               rc = file4lockInternal( &data->file, 0, c4->largeFileOffset, 1L, 0 ) ;
         }

         if ( rc == 0 )
         {
            data->appendServerLock = serverId ;
            data->appendClientLock = lockId ;
         }
         if ( rc == r4locked )
            dfile4registerLocked( data, 0L, 0 ) ;

         #ifdef S4SERVER_GUI
            if ( rc == 0 )
               c4->server->info.lockCount++ ;
         #endif
         return rc ;
      }
   #endif /* !S4CLIENT */



   #ifdef S4USE_ADDITIVE_LOCK
      #error additive locks no longer supported
   #endif

   #ifndef S4CLIENT
      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockTestFileInternal( DATA4FILE *data, const long lockId, const long serverId, Lock4type lockType )
      {
         /* returns 1 if we have file locked.  returns r4locked if some other DATA4
            has it locked.   returns 0 if no DATA4 has it locked */
         /* dataIn of 0 - means just return the lock conflict, do not actually check if we may have the lock */
         // similar to dfile4lockTestFile(), but we also return r4locked instead of 0
         // AS Apr 15/03 - support for new lockId for shared clone locking - We know look through this list
         // when we may have it locked via a clone lock
         // now returns '1' in that case if we have it locked.
         #ifdef S4SINGLE
            return 1 ;
         #else
            int retVal ;

            #ifdef E4PARM_LOW
               // AS 11/24/99 lockId may be zero if doing a general inquiry from cbAdmin
               // i.e. we want to know if any lock exists, and don't disclude any clients...
               if ( data == 0 || serverId == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
                  return error4( 0, e4parm_null, E91102 ) ;
            #endif

            if ( data->file.lowAccessMode == OPEN4DENY_NONE && lockType == lock4read )  // impossible to be a conflict, readLocks not supported
               return 0 ;

            #ifdef S4SERVER
               if ( data->exclusiveOpen != 0 )
               {
                  /* verify that the server id matches for the data which has exclusive open */
                  if ( data->exclusiveOpen->serverId == serverId )
                     return 1 ;
                  else
                  {
                     data->tempServerLock = data->exclusiveOpen->serverId ;
                     data->tempClientLock = data4lockId( data->exclusiveOpen ) ;
                     dfile4registerLocked( data, -1L, 1 ) ;
                     return r4locked ;
                  }
               }
            #else
               // AS 01/04/00 - if open in DENY_WRITE mode, we want to still enforce locks.
               // this is because OLE-DB and ODBC perform locks to ensure records are committed.
               // if locks are not placed on writes, the other client may place locks on the
               // record but the value can still be changed or rolled back because no locks
               // are actually placed.  t4odbc4.c displays problem.
               // if ( data->file.lowAccessMode != OPEN4DENY_NONE )
               if ( data->file.lowAccessMode == OPEN4DENY_RW )
                  return 1 ;
            #endif

            if ( lockType == lock4write )
            {
               if ( data->fileServerWriteLock != 0 )
               {
                  // with ODBC locking, clientId's are the same for all data4's of a particular client,
                  // and we support shared locking there, so only clientId's need to match
                  if ( data->fileClientWriteLock == lockId )
                  {
                     #ifdef S4ODBC_BUILD
                        if ( data->c4->odbc == 1 )
                           return 1 ;
                        else
                     #endif
                        {
                           // AS Apr 15/03 - support for new lockId for shared clone locking
                           // do not check serverId in s/a since it == clientId, and we are interested in lockId due to clones
                           #ifndef S4STAND_ALONE
                              if ( data->fileServerWriteLock == serverId )
                           #endif
                                 return 1 ;
                        }
                  }

                  data->tempServerLock = data->fileServerWriteLock ;
                  data->tempClientLock = data->fileClientWriteLock ;
                  dfile4registerLocked( data, -1L, 1 ) ;
                  return r4locked ;
               }

               return 0 ;
            }

      //      if ( lockType == lock4read )
      //      {
               Lock4 *iterate = (Lock4 *)((Single4lock *)(data->fileReadLocks.initIterate())) ;

               if ( iterate == 0 )
                  return 0 ;

               for( ;; )
               {
                  if ( iterate == 0 )  // just register the 1st lock as the conflicter
                  {
                     iterate = (Lock4 *)((Single4lock *)(data->fileReadLocks.initIterate())) ;
                     data->tempServerLock = serverId ;
                     #ifdef S4SERVER
                        data->tempClientLock = iterate->clientId ;
                     #else
                        data->tempClientLock = data4lockId( iterate->data ) ;
                     #endif
                     dfile4registerLocked( data, -1L, 1 ) ;
                     retVal = r4locked ;
                     break ;
                  }

                  if ( data4serverId( iterate->data ) == serverId )
                  {
                     #ifdef S4SERVER
                        if ( lockId == iterate->clientId )
                     #else
                        if ( lockId == data4lockId( iterate->data ) )
                     #endif
                     {
                        retVal = 1 ;
                        break ;
                     }
                  }

                  iterate = (Lock4 *)(Single4lock *)((Single4lock *)iterate)->next() ;
               }
      //      }
            return retVal ;  // CS 2000/12/01 fix Borland warning
         #endif /* S4SINGLE */
      }
   #endif /* S4CLIENT */



   #ifdef S4CLIENT
      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockFile( DATA4FILE *data, const long lockId, const long serverId, DATA4 *d4, Lock4type lockType )
      {
         int rc = 0 ;
         CODE4 *c4 ;
         CONNECTION4LOCK_INFO_IN *info=0 ;
         CONNECTION4 *connection ;

         // AS Mar 13/06 - don't memset this to null at this point, not set yet...
         // memset( &info, 0, sizeof( CONNECTION4LOCK_INFO_IN ) ) ;

         #ifdef E4PARM_LOW
            if ( data == 0 || lockId == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any )
                 #ifndef S4CLIENT
                    || serverId == 0
                 #endif
               )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         if ( data->lockTestLockId != 0 && code4unlockAuto( c4 ) != LOCK4ALL )
         {
            if ( c4->lockAttempts == WAIT4EVER )
               return error4( c4, e4lock, E81523 ) ;
            else
               return r4locked ;
         }
         connection = data->connection ;
         if ( connection == 0 )
            return e4connection ;
         connection4assign( connection, CON4LOCK, lockId, data->serverId ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
         // AS Mar 13/06 - null out in case of new items
         memset( info, 0, sizeof( CONNECTION4LOCK_INFO_IN ) ) ;
         info->type = htons5(LOCK4FILE) ;
         // CJ July 17/01-lockType is no longer an enumerator in CONNECTION4LOCK_INFO_IN problem with mac client
         info->lockType = htonl5( lockType ) ;
         rc = connection4repeat( connection ) ;
         if ( rc < 0 )
            connection4error( connection, c4, rc, E91102 ) ;
         if ( rc == 0 )
         {
            // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
            data->fileLockServerId = data4serverId( d4 ) ;
            data->fileLockLockId = data4lockId( d4 ) ;
         }
         return rc ;
      }



      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockAll( DATA4FILE *data, const long lockId, const long serverId )
      {
         CONNECTION4LOCK_INFO_IN *info ;
         CONNECTION4 *connection ;
         int rc ;
         CODE4 *c4 ;

         #ifdef E4PARM_LOW
            if ( data == 0 )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         connection = data->connection ;
         if ( connection == 0 )
            return e4connection ;
         connection4assign( connection, CON4LOCK, lockId, data->serverId ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
         info->type = htons5(LOCK4ALL) ;
         // AS Nov 30/01 - The lock type needs to be put into byte order
         // info->lockType = lock4write ;
         info->lockType = htonl5(lock4write) ;
         rc = connection4repeat( connection ) ;
         if ( rc < 0 )
            connection4error( connection, c4, rc, E91102 ) ;
         return rc ;
      }
   #else



      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockFile( DATA4FILE *data, const long lockId, const long serverId, Lock4type lockType )
      {
         int rc = 0 ;
         CODE4 *c4 ;

         #ifdef E4PARM_LOW
            if ( data == 0 || lockId == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any )
                 #ifndef S4CLIENT
                    || serverId == 0
                 #endif
               )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         /* now check if any other lock is conflicting */
         rc = dfile4lockTestAppend( data, lockId, serverId ) ;
         if ( rc != 0 )
         {
            if ( rc == 1 )  /* means we have locked ourselves out */
            {
               dfile4registerLocked( data, 0L, 1 ) ;
               rc = r4locked ;
            }
            #ifndef S4SERVER
               if ( rc == r4locked )
                  if ( c4->lockAttempts == WAIT4EVER )
                     return error4( c4, e4lock, E81523 ) ;
            #endif
            return rc ;
         }

         if ( data->recordLockWriteCount != 0  )
         {
            #ifdef S4SERVER
               if ( c4->currentClient->isStream )
                  return r4locked ;
            #else
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
            #endif
            /* find out and register who is holding the lock - don't check if we have locked */
            if ( dfile4lockTest( data, 0, 0, 0, lock4write ) != r4locked )
               return error4( c4, e4lock, E91102 ) ;
            return r4locked ;
         }

         if ( data->recordLockReadCount != 0 && lockType == lock4write )
         {
            #ifdef S4SERVER
               if ( c4->currentClient->isStream )
                  return r4locked ;
            #else
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
            #endif
            /* find out and register who is holding the lock */
            if ( dfile4lockTest( data, 0, 0, 0, lock4read ) != r4locked )
               return error4( c4, e4lock, E91102 ) ;
            return r4locked ;
         }

         // read locks do nothing, just get registered by caller
         if ( lockType == lock4write )  // must remove all of our read locks
         {
            // AS July 16/02 - at the low level, if file is opened exclusively, don't request a lock
            if ( data->file.lowAccessMode != OPEN4DENY_RW )
            {
               // AS Nov 4/03 - large file support
               #ifdef S4CLIPPER
                  if ( c4->largeFileOffset == 0 )
                     rc = file4lockInternal( &data->file, L4LOCK_POS, 0, L4LOCK_POS, 0 ) ;
                  else
                     return error4( c4, e4notSupported, E91102 ) ;
               #endif

               #ifdef S4MDX
                  if ( c4->largeFileOffset == 0 )
                     rc = file4lockInternal( &data->file, L4LOCK_POS_OLD, 0, L4LOCK_POS - L4LOCK_POS_OLD + 1, 0 ) ;
                  else
                     rc = file4lockInternal( &data->file, 0, c4->largeFileOffset, ULONG_MAX, 0 ) ;
               #endif

               #ifdef S4FOX
                  /* codebase locks the append byte as well... */
                  if ( c4->largeFileOffset == 0 )
                     rc = file4lockInternal( &data->file, L4LOCK_POS_OLD, 0, L4LOCK_POS_OLD - 1L, 0 ) ;
                  else
                     rc = file4lockInternal( &data->file, 0, c4->largeFileOffset, ULONG_MAX, 0 ) ;
               #endif

               if ( rc != 0 )
               {
                  if ( rc == r4locked )
                     dfile4registerLocked( data, -1L, 0 ) ;
                  return rc ;
               }

               // AS Oct 7/03 - Enable file optimization
               #ifndef S4OPTIMIZE_OFF
                  file4setWriteOpt( &data->file, 1 ) ;
                  // AS Oct 27/03 - And for memo file if open
                  #ifndef S4OFF_MEMO
                     if ( data->nFieldsMemo != 0 )
                        file4setWriteOpt( &data->memoFile.file, 1 ) ;
                  #endif
               #endif
            }

            #ifdef S4SERVER_GUI
               if ( rc == 0 )
                  c4->server->info.lockCount++ ;
            #endif

            data->fileClientWriteLock = lockId ;
            data->fileServerWriteLock = serverId ;
            // LY Jan 18/05 : added #ifdef (avoid compiler errors)
            #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
               // AS Jul 9/04 - For compressed writing to tables, if the data file is locked, the compression area is also considered locked
               if ( data->file.compressInfo != 0 )
                  data->file.compressInfo->isLocked = 1 ;
            #endif
         }

         #ifndef S4OPTIMIZE_OFF
            file4refresh( &data->file ) ;   /* make sure all up to date */
         #endif
         return 0 ;
      }



      int S4FUNCTION dfile4lockIndex( DATA4FILE *data, const long serverId )
      {
         #ifndef S4INDEX_OFF
            int rc, oldAttempts, count ;
            #ifdef S4CLIPPER
               TAG4FILE *tagOn ;
            #else
               INDEX4FILE *indexOn ;
            #endif
            CODE4 *c4 ;

            #ifdef E4PARM_LOW
               if ( data == 0 || serverId == 0 )
                  return error4( 0, e4parm, E91102 ) ;
            #endif

            c4 = data->c4 ;

            if ( error4code( c4 ) < 0 )
               return e4codeBase ;

            #ifdef S4CLIPPER
               if ( data->indexLocked == 1 )
                  return 0 ;
            #endif

            count = oldAttempts = c4->lockAttempts ;  /* take care of wait here */
            c4->lockAttempts = 1 ;

            rc = 0 ;
            for(;;)
            {
               #ifdef S4CLIPPER
                  for ( tagOn = 0 ;; )
                  {
                     tagOn = dfile4tagNext( data, tagOn ) ;
                     if ( tagOn == 0 )
                        break ;
                     rc = tfile4lock( tagOn, serverId ) ;
                     if ( rc != 0 )
                        break ;
                  }
               #else
                  for ( indexOn = 0 ;; )
                  {
                     indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn ) ;
                     if ( indexOn == 0 )
                        break ;
                     rc = index4lock( indexOn, serverId ) ;
                     if ( rc != 0 )
                        break ;
                  }
               #endif

               if ( rc == 0 )
                  break ;

               #ifdef S4CLIPPER
                  for ( tagOn = 0 ;; )
                  {
                     tagOn = dfile4tagNext( data, tagOn ) ;
                     if ( tagOn == 0 )
                        break ;
                     if ( tfile4unlock( tagOn, serverId ) < 0 )
                        rc = -1 ;
                  }
               #else
                  for ( indexOn = 0 ;; )
                  {
                     indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn ) ;
                     if ( indexOn == 0 )
                        break ;
                     if ( index4unlock( indexOn, serverId ) < 0 )
                        rc = -1 ;
                  }
               #endif

               if ( rc != r4locked )
                  break ;

               if ( count == 0 || rc == -1 )
                  break ;

               if ( count > 0 )
                  count-- ;

               #ifdef S4TEMP
                  if ( d4display_quit( &display ) )
                     return error4( c4, e4result, E80604 ) ;
               #endif

               u4delayHundredth( c4->lockDelay ) ;   /* wait a second & try lock again */
            }

            #ifdef S4CLIPPER
               data->indexLocked = 1 ;
            #endif
            c4->lockAttempts = oldAttempts ;
            if ( error4code( c4 ) < 0 )
               return -1 ;

            return rc ;
         #else
            return 0 ;
         #endif
      }
   #endif /* S4CLIENT */



   #ifdef S4CLIENT
      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockTest( DATA4FILE *data, DATA4 *dataIn, const long lockId, const long serverId, const long rec, Lock4type lockType )
      {
         /* if rec == -1, then any lock is checked */
         /* for client, if lockId == 0, then any d4 with a lock will return success */
         /* for server, if lockId == 0 and serverId == 0, then any d4 with a lock will return success */
         /* for server, if lockId == 0  then any d4 with a serverId lock will return success */
         int rc ;
         LOCK4LINK *lock ;

         #ifdef E4PARM_LOW
            if ( data == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         rc = dfile4lockTestFile( data, lockId, serverId, lock4write ) ;
         if ( rc )
            return rc ;

         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         if ( data->fileLockLockId != 0 )
         {
            data->lockTestServerId = data->fileLockServerId ;
            data->lockTestLockId = data->fileLockLockId ;
            return 0 ;
         }

         for( lock = (LOCK4LINK *)(data->lockedRecords.initIterate()) ;; )
         {
            if ( lock == 0 )
               break ;
            if ( lock->recNo == rec || rec == -1L )
            {
               #ifdef E4DEBUG
                  /*
                     AS 03/24/99 -- this code no longer works for testing, removed.

                     We want to do some special checking of the server here.  Namely, the client here
                     has reported that a lock does exist.  We want to bypass the clients check of the
                     lock and request the results from the server.  Basically, this tests that the
                     server is correctly recording locks (i.e. in the 'normal' case, the server status
                     is not verified that it is correct, here we hope that a test program will fail
                     suitably if the mismatch is not noticed).
                  if ( lock->lockType != lockType )  // r4locked for sure - return failure
                     return 0 ;
                  */
               #endif
               if ( lockId == 0 )   /* if clintId == 0 then any lock is considered success */
                  return 1 ;
               // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
               // if ( lock->data->lockId == lockId )
               if ( lock->lockId == lockId )
                  return 1 ;
               else
               {
                  // data->lockTest = lock->data ;
                  data->lockTestServerId = lock->serverId ;
                  data->lockTestLockId = lock->lockId ;
                  return 0 ;
               }
            }
            lock = (LOCK4LINK *)single4next( &lock->link ) ;
         }
         return 0 ;
      }
   #else



      // AS Apr 15/03 - support for new lockId for shared clone locking
      int dfile4lockTest( DATA4FILE *d4file, DATA4 *dataIn, const long serverId, const long rec, Lock4type lockType )
      {
         /* checks record locks only, not file locks */
         /* rec of 0 means return first lock found */
         /* dataIn of 0 - means just return the lock conflict, do not actually check if we may have the lock */
         /* this function only called when we know we don't have it locked (looking through other users locks) */
         /* return r4locked or 0 or error */
         /* serverId of 0 means as appropriate check all DATA4's (usual case) */
         // AS Apr 15/03 - support for new lockId for shared clone locking - We know look through this list
         // when we may have it locked via a clone lock
         // now returns '1' in that case if we have it locked.
         Lock4 *lock = 0 ;
         DATA4 *data = 0 ;
         LINK4 *link = 0 ;

         #ifdef E4PARM_LOW
            if ( d4file == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         link = (LINK4 *)l4first( &d4file->data4list ) ;
         assert5( link != 0 ) ;   /* must be at least one member on the list */

         for( ;; )
         {
            if ( link == 0 )
               break ;
            data = (DATA4 *)(link-1) ;

            if ( serverId != 0 )  /* only check for DATA4 with given serverId */
               if ( data4serverId( data ) != serverId )
               {
                  link = (LINK4 *)l4next( &d4file->data4list, link ) ;
                  continue ;
               }

            for ( lock = (Lock4 *)(Single4lock *)data->lockedRecords.initIterate() ;; )
            {
               if ( lock == 0 )
                  break ;
               if ( lockType == lock->lockType )
               {
                  // AS Apr 15/03 - support for new lockId for shared clone locking
                  if ( dataIn != 0 && lock->recNum == rec && data4lockId( dataIn ) == data4lockId( lock->data ) )
                  {
                     #ifndef S4STAND_ALONE
                        // in client, ensure the DATA4FILE is the same base... (i.e. opened as a true clone???)
                        // AS This should always match since they share the same serverId.
                        if ( data4serverId( lock->data ) == data4serverId( dataIn ) )
                     #endif
                     return 1 ;
                  }

                  if ( rec == 0 || lock->recNum == rec )  /* found failure */
                  {
                     d4file->tempServerLock = data4serverId( lock->data ) ;
                     #ifdef S4SERVER
                        d4file->tempClientLock = lock->clientId ;
                     #else
                        d4file->tempClientLock = data4lockId( lock->data ) ;
                     #endif
                     dfile4registerLocked( d4file, rec, 1 ) ;
                     return r4locked ;
                  }
               }
               lock = (Lock4 *)(Single4lock *)(((Single4lock *)lock)->next()) ;
            }

            // AS Apr 15/03 - support for new lockId for shared clone locking
            // consider case where a clone has the whole file locked
            // if ( dataIn != 0 && data->fileServerWriteLock != 0 )

            link = (LINK4 *)l4next( &d4file->data4list, link ) ;
         }

         return 0 ;
      }



      /* AS Nov 13/02 - export for dot */
      int S4FUNCTION dfile4lockTestIndex( DATA4FILE *data, const long serverId )
      {
         #ifndef S4INDEX_OFF
            #ifdef S4CLIPPER
               TAG4FILE *tagOn ;
            #else
               INDEX4FILE *indexOn ;
            #endif

            #ifdef E4PARM_LOW
               if ( data == 0 )
                  return error4( 0, e4parm_null, E91102 ) ;
            #endif

            #ifdef S4SERVER
               if ( data->exclusiveOpen != 0 )
                  return 1 ;
            #endif

            #ifdef S4CLIPPER
               for( tagOn = 0 ; ; )
               {
                  tagOn = (TAG4FILE *)l4next( &data->tagfiles, tagOn ) ;
                  if ( !tagOn )
                     break ;
                  if ( serverId == 0 && tagOn->fileLocked != 0 )
                     return 1 ;
                  if ( tagOn->fileLocked != serverId )
                     return 0 ;
               }
            #else
               for( indexOn = 0 ; ; )
               {
                  indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn ) ;
                  if ( !indexOn )
                     break ;
                  if ( serverId == 0 && indexOn->fileLocked != 0 )
                     return 1 ;
                  if ( indexOn->fileLocked != serverId )
                     return 0 ;
               }
            #endif
         #endif
         return 1 ;
      }
   #endif  /* S4CLIENT */
#endif /* S4SINGLE */



#ifdef P4ARGS_USED
   #pragma argsused
#endif
#ifdef S4CLIENT
   // AS Apr 15/03 - support for new lockId for shared clone locking
   int S4FUNCTION dfile4lockTestAppend( DATA4FILE *data, const long lockId, const long serverId )
   {
      #ifdef S4SINGLE
         return 1 ;
      #else
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         data->lockTestServerId = 0 ;
         data->lockTestLockId = 0 ;
         if ( data->appendLockLockId != 0 )
         {
            if ( data->appendLockLockId == lockId && data->appendLockServerId == serverId )
               return 1 ;
            data->lockTestServerId = data->appendLockServerId ;
            data->lockTestLockId = data->appendLockLockId ;
            return 0 ;
         }

         if ( data->fileLockLockId )
            return ( data->fileLockLockId == lockId ) ;

         return 0 ;
      #endif /* S4SINGLE */
   }



#else
   // AS Apr 15/03 - support for new lockId for shared clone locking
   int S4FUNCTION dfile4lockTestAppend( DATA4FILE *data, const long lockId, const long serverId )
   {
      /* lockId == 0 means any client lock for the DATA4FILE
         serverId == 0 means any server lock for the DATA4FILE
      */
      #ifdef S4SINGLE
         return 1 ;
      #else
         #ifdef E4PARM_LOW
            if ( data == 0 )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         if ( data->appendServerLock == 0 )
            return 0 ;

         // AS For stand/alone, do not require the serverId check since this == clientId anyway
         if ( lockId == 0 || data->appendClientLock == lockId )
         {
            #ifndef S4STAND_ALONE
               if ( data->appendServerLock == serverId )
            #endif
                  return 1 ;
         }

         if ( serverId == 0 )  /* means as long as we have lock at DATA4FILE level */
            return 1 ;

         data->tempServerLock = data->appendServerLock ;
         data->tempClientLock = data->appendClientLock ;

         dfile4registerLocked( data, 0L, 1 ) ;
         return r4locked ;
      #endif /* S4SINGLE */
   }
#endif /* S4CLIENT */



#ifdef P4ARGS_USED
   #pragma argsused
#endif
// AS Apr 15/03 - support for new lockId for shared clone locking
int S4FUNCTION dfile4lockTestFile( DATA4FILE *data, const long lockId, const long serverId, Lock4type lockType )
{
   // returns 0 if we don't have locked, 1 if we do.
   // similar fuynction dfile4lockTestFileInternal returns r4locked if another internal
   // user has locked, but this function does not
   #ifdef S4SINGLE
      return 1 ;
   #else
      #ifdef E4PARM_LOW
         if ( data == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif

      #ifdef S4CLIENT
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         data->lockTestServerId = 0 ;
         data->lockTestLockId = 0 ;
         // AS 01/25/00 -- changed - in order to properly support isolation levels, etc.
         // we must be able to perform these locks, even if file not open in exclusive
         // if ( data->accessMode != OPEN4DENY_NONE )
         //    return 1 ;
         if ( data->accessMode == OPEN4DENY_RW )
            return 1 ;
         if ( data->fileLockLockId != 0 )
         {
            if ( data->fileLockLockId == lockId && data->fileLockServerId == serverId )
               return 1 ;
            data->lockTestLockId = data->fileLockLockId ;
            data->lockTestServerId = data->fileLockServerId ;
            return 0 ;
         }
         return 0 ;
      #else
         #ifdef S4SERVER
            if ( data->exclusiveOpen != 0 )
            {
               /* verify that the server id matches for the data which has exclusive open */
               if ( data->exclusiveOpen->serverId == serverId )
                  return 1 ;
               else
               {
                  data->tempServerLock = data->exclusiveOpen->serverId ;
                  data->tempClientLock = data4lockId( data->exclusiveOpen ) ;
                  if ( serverId == 0 )
                     return 1 ;
                  else
                     return 0 ;
               }
            }
         #else
            // AS 01/04/99 - if open in DENY_WRITE mode, we want to still enforce locks.
            // this is because OLE-DB and ODBC perform locks to ensure records are committed.
            // if locks are not placed on writes, the other client may place locks on the
            // record but the value can still be changed or rolled back because no locks
            // are actually placed.  t4odbc4.c displays problem.
            // if ( data->file.lowAccessMode != OPEN4DENY_NONE )
            if ( data->file.lowAccessMode == OPEN4DENY_RW )
               return 1 ;
         #endif

         if ( lockType == lock4write )
         {
            if ( data->fileServerWriteLock == 0 )
               return 0 ;

            if ( serverId == 0 )
            {
               data->tempServerLock = data->fileServerWriteLock ;
               data->tempClientLock = data->fileClientWriteLock ;
               return 1 ;
            }

            if ( lockId == 0 )
            {
               if ( data->fileServerWriteLock != serverId )
                  return 0 ;
               return 1 ;
            }

            if ( data->fileServerWriteLock == serverId && data->fileClientWriteLock == lockId )
               return 1 ;
            return 0 ;
         }

         // in lockRead instance, returns '1' if we hold a lock, r4locked if someone else

         Lock4 *iterate = (Lock4 *)((Single4lock *)(data->fileReadLocks.initIterate())) ;

         if ( iterate == 0 )
            return 0 ;

         if ( serverId == 0 )
         {
            data->tempServerLock = data4serverId( iterate->data ) ;
            #ifdef S4SERVER
               data->tempClientLock = iterate->clientId ;
            #else
               data->tempClientLock = data4lockId( iterate->data ) ;
            #endif
            return 1 ;
         }

         for( ;; )
         {
            if ( iterate == 0 )
               return 0 ;

            if ( data4serverId( iterate->data ) == serverId )
            {
               if ( lockId == 0 )
               {
                  data->tempServerLock = serverId ;
                  #ifdef S4SERVER
                     data->tempClientLock = iterate->clientId ;
                  #else
                     data->tempClientLock = data4lockId( iterate->data ) ;
                  #endif
                  dfile4registerLocked( data, -1L, 1 ) ;
                  return 1 ;
               }
               else
               {
                  #ifdef S4SERVER
                     if ( lockId == iterate->clientId )
                  #else
                     if ( lockId == data4lockId( iterate->data ) )
                  #endif
                     return 1 ;
               }
            }

            iterate = (Lock4 *)(Single4lock *)((Single4lock *)iterate)->next() ;
         }
      #endif /* S4CLIENT */
   #endif /* S4SINGLE */
}

