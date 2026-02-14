/* df4lock.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4SINGLE
   #ifdef S4CLIENT
      int dfile4lock( DATA4FILE *data, const long clientId, const long serverId, const long rec, Lock4type lockType )
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
            if ( data == 0 || rec < 1L || clientId == 0
                 #ifdef S4SERVER
                    || serverId == 0
                 #endif
               )
               return error4( (data == 0 ? 0 : data->c4 ), e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;

         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         if ( data->lockTest != 0 && code4unlockAuto( c4 ) != LOCK4ALL )
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
            connection4assign( connection, CON4LOCK, clientId, data->serverId ) ;
            connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
            info->type = htons5(LOCK4RECORD) ;
            info->lockType = lockType ;
            recNum = htonl5(rec) ;
            connection4addData( connection, &recNum, sizeof( recNum ), NULL ) ;
            rc = connection4repeat( connection ) ;

            if ( rc < 0 )
               connection4error( connection, c4, rc, E91102 ) ;
         }
         return rc ;
      }
   #else
      int dfile4lock( DATA4FILE *data, const long clientId, const long serverId, const long rec )
      {
         /* does not check for existing lock (by us) (assumes this has already been tested) */
         int rc ;
         CODE4 *c4 ;
         FILE4LONG position ;
         #ifdef S4FOX
            FILE4LONG pos2 ;
         #endif

         #ifdef E4PARM_LOW
            if ( data == 0 || rec < 1L || clientId == 0
                 #ifdef S4SERVER
                    || serverId == 0
                 #endif
               )
               return error4( (data == 0 ? 0 : data->c4 ), e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;

         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         file4longAssign( position, 0, c4->largeFileOffset ) ;

         #ifdef S4CLIPPER
            file4longAdd( &position, L4LOCK_POS ) ;
            file4longAdd( &position, rec ) ;
         #endif
         #ifdef S4MDX
            file4longAdd( &position, L4LOCK_POS ) ;
            file4longSubtract( &position, rec + 1U ) ;
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
                  file4longAdd( &position, L4LOCK_POS_OLD ) ;
                  file4longAssignLong( pos2, dfile4recordPosition( data, rec ) ) ;
                  file4longAddLong( &position, &pos2 ) ;
               }
            }
            else
               file4longAdd( &position, rec ) ;
         #endif

         #ifdef S4MDX
            rc = file4lockInternal( &data->file, L4LOCK_POS - 1U, 0, 1L, 0 ) ;
            if ( rc == 0 )
            {
         #endif
            rc = file4lockInternal( &data->file, file4longGetLo( position ), file4longGetHi( position ), 1L, 0 ) ;
         #ifdef S4MDX
               file4unlockInternal( &data->file, L4LOCK_POS - 1U, 0, 1L, 0 ) ;
            }
         #endif
         if ( rc )
         {
            if ( rc == r4locked )
               dfile4registerLocked( data, rec, 0 ) ;
            return rc ;
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
      int dfile4lockAppend( DATA4FILE *data, const long clientId, const long serverId )
      {
         int rc ;
         CODE4 *c4 ;
         CONNECTION4LOCK_INFO_IN *info ;
         CONNECTION4 *connection ;

         #ifdef E4PARM_LOW
            if ( data == 0 || clientId == 0 )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return -1 ;

         if ( data->lockTest != 0 && code4unlockAuto( c4 ) != LOCK4ALL )
         {
            if ( c4->lockAttempts == WAIT4EVER )
               return error4( c4, e4lock, E81523 ) ;
            else
               return r4locked ;
         }

         connection = data->connection ;
         if ( connection == 0 )
            return e4connection ;
         connection4assign( connection, CON4LOCK, clientId, data->serverId ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
         info->type = htons5(LOCK4APPEND) ;
         info->lockType = lock4write ;
         rc = connection4repeat( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E91102 ) ;

         return rc ;
      }
   #endif /* S4CLIENT */


   #ifndef S4CLIENT
      int dfile4lockAppend( DATA4FILE *data, const long clientId, const long serverId )
      {
         /* assumes that all testing for locks is complete, just does the actual lock */
         int rc ;
         CODE4 *c4 ;

         #ifdef E4PARM_LOW
            if ( data == 0 || clientId == 0
                 #ifdef S4SERVER
                    || serverId == 0
                 #endif
               )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return -1 ;

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
         if ( rc == 0 )
         {
            data->appendServerLock = serverId ;
            data->appendClientLock = clientId ;
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
      int dfile4lockTestFileInternal( DATA4FILE *data, const long clientId, const long serverId, Lock4type lockType )
      {
         /* returns 1 if we have file locked.  returns r4locked if some other DATA4
            has it locked.   returns 0 if no DATA4 has it locked */
         // similar to dfile4lockTestFile(), but we also return r4locked instead of 0
         #ifdef S4SINGLE
            return 1 ;
         #else
            int retVal ;

            #ifdef E4PARM_LOW
               // AS 11/24/99 clientId may be zero if doing a general inquiry from cbAdmin
               // i.e. we want to know if any lock exists, and don't disclude any clients...
               if ( data == 0 || serverId == 0 )
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
                     data->tempClientLock = data->exclusiveOpen->clientId ;
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
                  if ( data->fileServerWriteLock == serverId && data->fileClientWriteLock == clientId )
                     return 1 ;

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
                        data->tempClientLock = data4clientId( iterate->data ) ;
                     #endif
                     dfile4registerLocked( data, -1L, 1 ) ;
                     retVal = r4locked ;
                     break ;
                  }

                  if ( data4serverId( iterate->data ) == serverId )
                  {
                     #ifdef S4SERVER
                        if ( clientId == iterate->clientId )
                     #else
                        if ( clientId == data4clientId( iterate->data ) )
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
      int dfile4lockFile( DATA4FILE *data, const long clientId, const long serverId, DATA4 *d4, Lock4type lockType )
      {
         int rc = 0 ;
         CODE4 *c4 ;
         CONNECTION4LOCK_INFO_IN *info ;
         CONNECTION4 *connection ;

         memset( &info, 0, sizeof( CONNECTION4LOCK_INFO_IN ) ) ;

         #ifdef E4PARM_LOW
            if ( data == 0 || clientId == 0
                 #ifndef S4CLIENT
                    || serverId == 0
                 #endif
               )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         c4 = data->c4 ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         if ( data->lockTest != 0 && code4unlockAuto( c4 ) != LOCK4ALL )
         {
            if ( c4->lockAttempts == WAIT4EVER )
               return error4( c4, e4lock, E81523 ) ;
            else
               return r4locked ;
         }
         connection = data->connection ;
         if ( connection == 0 )
            return e4connection ;
         connection4assign( connection, CON4LOCK, clientId, data->serverId ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
         info->type = htons5(LOCK4FILE) ;
         info->lockType = lockType ;
         rc = connection4repeat( connection ) ;
         if ( rc < 0 )
            connection4error( connection, c4, rc, E91102 ) ;
         if ( rc == 0 )
            data->fileLock = d4 ;
         return rc ;
      }



      int dfile4lockAll( DATA4FILE *data, const long clientId, const long serverId )
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
         connection4assign( connection, CON4LOCK, clientId, data->serverId ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), (void **)&info ) ;
         info->type = htons5(LOCK4ALL) ;
         info->lockType = lock4write ;
         rc = connection4repeat( connection ) ;
         if ( rc < 0 )
            connection4error( connection, c4, rc, E91102 ) ;
         return rc ;
      }
   #else



      int dfile4lockFile( DATA4FILE *data, const long clientId, const long serverId, Lock4type lockType )
      {
         int rc = 0 ;
         CODE4 *c4 ;

         #ifdef E4PARM_LOW
            if ( data == 0 || clientId == 0
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
         rc = dfile4lockTestAppend( data, clientId, serverId ) ;
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
            /* find out and register who is holding the lock */
            if ( dfile4lockTest( data, 0, 0, lock4write ) != r4locked )
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
            if ( dfile4lockTest( data, 0, 0, lock4read ) != r4locked )
               return error4( c4, e4lock, E91102 ) ;
            return r4locked ;
         }

         // read locks do nothing, just get registered by caller
         if ( lockType == lock4write )  // must remove all of our read locks
         {
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
                  return error4( c4, e4notSupported, E91102 ) ;
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

            #ifdef S4SERVER_GUI
               if ( rc == 0 )
                  c4->server->info.lockCount++ ;
            #endif

            data->fileClientWriteLock = clientId ;
            data->fileServerWriteLock = serverId ;
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
      int dfile4lockTest( DATA4FILE *data, const long clientId, const long serverId, const long rec, Lock4type lockType )
      {
         /* if rec == -1, then any lock is checked */
         /* for client, if clientId == 0, then any d4 with a lock will return success */
         /* for server, if clientId == 0 and serverId == 0, then any d4 with a lock will return success */
         /* for server, if clientId == 0  then any d4 with a serverId lock will return success */
         int rc ;
         LOCK4LINK *lock ;

         #ifdef E4PARM_LOW
            if ( data == 0 )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         rc = dfile4lockTestFile( data, clientId, serverId, lock4write ) ;
         if ( rc )
            return rc ;

         if ( data->fileLock != 0 )
         {
            data->lockTest = data->fileLock ;
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
               if ( clientId == 0 )   /* if clintId == 0 then any lock is considered success */
                  return 1 ;
               if ( lock->data->clientId == clientId )
                  return 1 ;
               else
               {
                  data->lockTest = lock->data ;
                  return 0 ;
               }
            }
            lock = (LOCK4LINK *)single4next( &lock->link ) ;
         }
         return 0 ;
      }
   #else



      int dfile4lockTest( DATA4FILE *dataIn, const long serverId, const long rec, Lock4type lockType )
      {
         /* checks record locks only, not file locks */
         /* rec of 0 means return first lock found */
         /* this function only called when we know we don't have it locked (looking through other users locks) */
         /* return r4locked or 0 or error */
         /* serverId of 0 means as appropriate check all DATA4's (usual case) */
         Lock4 *lock = 0 ;
         DATA4 *data = 0 ;
         LINK4 *link = 0 ;

         #ifdef E4PARM_LOW
            if ( dataIn == 0 )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         link = (LINK4 *)l4first( &dataIn->data4list ) ;
         assert5( link != 0 ) ;   /* must be at least one member on the list */

         for( ;; )
         {
            if ( link == 0 )
               break ;
            data = (DATA4 *)(link-1) ;

            if ( serverId != 0 )  /* only check for DATA4 with given serverId */
               if ( data4serverId( data ) != serverId )
               {
                  link = (LINK4 *)l4next( &dataIn->data4list, link ) ;
                  continue ;
               }

            for ( lock = (Lock4 *)(Single4lock *)data->lockedRecords.initIterate() ;; )
            {
               if ( lock == 0 )
                  break ;
               if ( lockType == lock->lockType )
                  if ( rec == 0 || lock->recNum == rec )  /* found failure */
                  {
                     dataIn->tempServerLock = data4serverId( lock->data ) ;
                     #ifdef S4SERVER
                        dataIn->tempClientLock = lock->clientId ;
                     #else
                        dataIn->tempClientLock = data4clientId( lock->data ) ;
                     #endif
                     dfile4registerLocked( dataIn, rec, 1 ) ;
                     return r4locked ;
                  }
               lock = (Lock4 *)(Single4lock *)(((Single4lock *)lock)->next()) ;
            }

            link = (LINK4 *)l4next( &dataIn->data4list, link ) ;
         }

         return 0 ;
      }



      int dfile4lockTestIndex( DATA4FILE *data, const long serverId )
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
   int S4FUNCTION dfile4lockTestAppend( DATA4FILE *data, const long clientId, const long serverId )
   {
      #ifdef S4SINGLE
         return 1 ;
      #else
         data->lockTest = 0 ;
         if ( data->appendLock != 0 )
         {
            if ( data->appendLock->clientId == clientId )
               return 1 ;
            data->lockTest = data->appendLock ;
            return 0 ;
         }

         if ( data->fileLock )
            return ( data->fileLock->clientId == clientId ) ;

         return 0 ;
      #endif /* S4SINGLE */
   }



#else
   int S4FUNCTION dfile4lockTestAppend( DATA4FILE *data, const long clientId, const long serverId )
   {
      /* clientId == 0 means any client lock for the DATA4FILE
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

         if ( data->appendServerLock == serverId )
            if ( clientId == 0 || data->appendClientLock == clientId )
               return 1 ;

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
int S4FUNCTION dfile4lockTestFile( DATA4FILE *data, const long clientId, const long serverId, Lock4type lockType )
{
   // returns 0 if we don't have locked, 1 if we do.
   // similar fuynction dfile4lockTestFileInternal returns r4locked if another internal
   // user has locked, but this function does not
   #ifdef S4SINGLE
      return 1 ;
   #else
      #ifdef S4CLIENT
         #ifdef E4DEBUG
            /*
               AS 03/24/99 -- this code no longer works for testing, removed.

               We want to do some special checking of the server here.  Namely, the client here
               has reported that a lock does exist.  We want to bypass the clients check of the
               lock and request the results from the server.  Basically, this tests that the
               server is correctly recording locks (i.e. in the 'normal' case, the server status
               is not verified that it is correct, here we hope that a test program will fail
               suitably if the mismatch is not noticed).
            if ( lockType == lock4write )   // go to server for testing
               return 0 ;
            */
         #endif
         data->lockTest = 0 ;
         // AS 01/25/00 -- changed - in order to properly support isolation levels, etc.
         // we must be able to perform these locks, even if file not open in exclusive
         // if ( data->accessMode != OPEN4DENY_NONE )
         //    return 1 ;
         if ( data->accessMode == OPEN4DENY_RW )
            return 1 ;
         if ( data->fileLock != 0 )
         {
            if ( data->fileLock->clientId == clientId )
               return 1 ;
            data->lockTest = data->fileLock ;
            return 0 ;
         }
         return 0 ;
      #else
         #ifdef E4PARM_LOW
            if ( data == 0 || lockType == lock4any )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         #ifdef S4SERVER
            if ( data->exclusiveOpen != 0 )
            {
               /* verify that the server id matches for the data which has exclusive open */
               if ( data->exclusiveOpen->serverId == serverId )
                  return 1 ;
               else
               {
                  data->tempServerLock = data->exclusiveOpen->serverId ;
                  data->tempClientLock = data->exclusiveOpen->clientId ;
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

            if ( clientId == 0 )
            {
               if ( data->fileServerWriteLock != serverId )
                  return 0 ;
               return 1 ;
            }

            if ( data->fileServerWriteLock == serverId && data->fileClientWriteLock == clientId )
               return 1 ;
            return 0 ;
         }

         // in lockRead instance, returns '1' if we hold a lock, r4locked if someone else

//         if ( lockType == lock4read )
//         {
            Lock4 *iterate = (Lock4 *)((Single4lock *)(data->fileReadLocks.initIterate())) ;

            if ( iterate == 0 )
               return 0 ;

            if ( serverId == 0 )
            {
               data->tempServerLock = data4serverId( iterate->data ) ;
               #ifdef S4SERVER
                  data->tempClientLock = iterate->clientId ;
               #else
                  data->tempClientLock = data4clientId( iterate->data ) ;
               #endif
               return 1 ;
            }

            for( ;; )
            {
               if ( iterate == 0 )
                  return 0 ;

               if ( data4serverId( iterate->data ) == serverId )
               {
                  if ( clientId == 0 )
                  {
                     data->tempServerLock = serverId ;
                     #ifdef S4SERVER
                        data->tempClientLock = iterate->clientId ;
                     #else
                        data->tempClientLock = data4clientId( iterate->data ) ;
                     #endif
                     dfile4registerLocked( data, -1L, 1 ) ;
                     return 1 ;
                  }
                  else
                  {
                     #ifdef S4SERVER
                        if ( clientId == iterate->clientId )
                     #else
                        if ( clientId == data4clientId( iterate->data ) )
                     #endif
                        return 1 ;
                  }
               }

               iterate = (Lock4 *)(Single4lock *)((Single4lock *)iterate)->next() ;
            }
//         }
      #endif /* S4CLIENT */
   #endif /* S4SINGLE */
}

