/* df4unlok.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#if !defined( S4OFF_MULTI ) && !defined( S4CLIENT )

   #if !defined( S4CLIPPER ) && !defined( S4OFF_MEMO )
      int dfile4memoUnlock( DATA4FILE *data )
      {
         #ifdef E4PARM_LOW
            if ( data == 0 )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         if ( code4unlockAuto( data->c4 ) == LOCK4OFF )
            return 0 ;

         // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
         //    if ( data->memoFile.file.hand != NULL )
         // #else
            if ( data->memoFile.file.hand != INVALID4HANDLE )
         // #endif
            return memo4fileUnlock( &data->memoFile ) ;
         else
            return 0 ;
      }
   #endif



   // AS Apr 15/03 - support for new lockId for shared clone locking
   int dfile4unlockAppend( DATA4FILE *data, const long lockId, const long serverId )
   {
      int rc ;

      #ifdef E4PARM_LOW
         if ( data == 0 || serverId == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif
      // AS Apr 15/03 - This is checked by the caller
      // if ( code4unlockAuto( data->c4 ) == LOCK4OFF )
      //    return 0 ;

      // AS Apr 15/03 - support clone lock/unlock; for stand/alone, we do not need to check the serverIds
      if ( lockId == 0 || lockId == data->appendClientLock )
         #ifndef S4STAND_ALONE
            if ( data->appendServerLock == serverId )
         #endif
      {
         #ifndef S4OFF_WRITE
            // AS Feb 18/03 for ODBC, we need to update the header at this point because
            // it likely was not updated if a transaction was in progress.
            if ( data->file.lowAccessMode == OPEN4DENY_RW
                 #ifndef S4OFF_TRAN
                    || ( data->c4->odbc == 1 && data->odbcUpdateRequired == 1 )
                 #endif
            )
               dfile4updateHeader( data, 1, 1, 1 ) ;  // AS Nov 1/02 - ensure auto-increment value is updated here as well.
            if ( data->doDate == 1 )
            {
               u4yymmdd( &data->yy ) ;
               data->doDate = 0 ;
            }
         #endif

         // AS Jul 30/02 - don't physically lock if opened exclusively
         if ( data->file.lowAccessMode == OPEN4DENY_RW )
            rc = 0 ;
         else
         {
            if ( data->c4->largeFileOffset == 0 )
            {
               #ifdef S4FOX
                  if ( ( data->hasMdxMemo & 0x01 ) || data->version == 0x30 )
                     rc = file4unlockInternal( &data->file, L4LOCK_POS, 0, 1L, 0 ) ;
                  else
                     rc = file4unlockInternal( &data->file, L4LOCK_POS_OLD, 0, 1L, 0 ) ;
               #else
                  rc = file4unlockInternal( &data->file, L4LOCK_POS, 0, 1L, 0 ) ;
               #endif
            }
            else
               rc = file4unlockInternal( &data->file, 0, data->c4->largeFileOffset, 1L, 0 ) ;
         }

         if ( rc < 0 )
            return error4stack( data->c4, (short)rc, E91102 ) ;
         data->appendServerLock = 0 ;
         data->appendClientLock = 0 ;
         data->numRecs = -1 ;
         #ifdef S4SERVER_GUI
            if ( rc == 0 )
               data->c4->server->info.lockCount-- ;
         #endif
      }
      if ( error4code( data->c4 ) < 0 )
         return error4code( data->c4 ) ;
      return 0 ;
   }



   // AS Apr 15/03 - support for new lockId for shared clone locking
   int dfile4unlockFile( DATA4FILE *data, const long lockId, const long serverId )
   {
      CODE4 *c4 ;
      #ifdef E4PARM_LOW
         if ( data == 0 || serverId == 0 )
            return error4( 0, e4parm, E91102 ) ;
      #endif

      c4 = data->c4 ;

      if ( code4unlockAuto( c4 ) == LOCK4OFF )
         return 0 ;

      if ( data->fileServerWriteLock == serverId && ( lockId == 0 || lockId == data->fileClientWriteLock ) )
      {
         #ifndef S4OFF_WRITE
            if ( data->file.lowAccessMode == OPEN4DENY_RW )
               dfile4updateHeader( data, 1, 1, 0 ) ;
            if ( data->doDate == 1 )
            {
               u4yymmdd( &data->yy ) ;
               data->doDate = 0 ;
            }
         #endif

         // AS July 16/02 - at the low level, if file is opened exclusively, don't request a lock
         if ( data->file.lowAccessMode != OPEN4DENY_RW )
         {
            #ifdef S4CLIPPER
               if ( file4unlockInternal( &data->file, L4LOCK_POS, 0, L4LOCK_POS, 0 ) < 0 )
                  return -1 ;
            #else
               // AS Nov 4/03 - large file support
               if ( c4->largeFileOffset == 0 )
               {
                  #ifdef S4FOX
                     /* codebase locks the append byte as well... */
                     if ( file4unlockInternal( &data->file, L4LOCK_POS_OLD, 0, L4LOCK_POS_OLD - 1L, 0 ) < 0 )
                        return -1 ;
                  #else
                     if ( file4unlockInternal( &data->file, L4LOCK_POS_OLD, 0, L4LOCK_POS - L4LOCK_POS_OLD + 1, 0 ) < 0 )
                        return -1 ;
                  #endif
               }
               else
               {
                  if ( file4unlockInternal( &data->file, 0, c4->largeFileOffset, ULONG_MAX, 0 ) < 0 )
                     return -1 ;
               }
            #endif
         }

         #ifdef S4SERVER_GUI
            c4->server->info.lockCount-- ;
         #endif
         data->fileServerWriteLock = 0 ;
         data->fileClientWriteLock = 0 ;
         // LY Jan 18/05 : added #ifdef (avoid compiler errors)
         #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
            // AS Jul 9/04 - For compressed writing to tables, if the data file is locked, the compression area is also considered locked
            if ( data->file.compressInfo != 0 )
               data->file.compressInfo->isLocked = 0 ;
         #endif
         data->numRecs = -1 ;
      }
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;
      return 0 ;
   }



   #ifndef S4INDEX_OFF
      int S4FUNCTION d4unlockIndex( DATA4 *data )
      {
         // required for ODBC
         return dfile4unlockIndex( data->dataFile, data4serverId( data ) ) ;
      }



      int S4FUNCTION dfile4unlockIndex( DATA4FILE *data, const long serverId )
      {
         int rc ;
         #ifdef S4CLIPPER
            TAG4FILE *tagOn ;
         #else
            INDEX4FILE *indexOn ;
         #endif

         #ifdef E4PARM_LOW
            // AS Dec 31/03 - the serverId may be 0 if the data file is not valid (failed open)
            if ( data == 0 || (serverId == 0 && data->valid))
               return error4( 0, e4parm, E91102 ) ;
         #endif

         #ifdef S4CLIPPER
            if ( data->indexLocked == 0 )
               return 0 ;
         #endif

         /* if the data file is locked, then delay unlocking index if lock off */
         if ( code4unlockAuto( data->c4 ) == LOCK4OFF )
            if ( data->fileServerWriteLock == serverId )
                return 0 ;

         #ifdef S4CLIPPER
            // AS Aug 29/02 - It is not guaranteed that the index is unlocked after this call.
            // in particular, if another user is holding the index locks (so they are still held)
            // this is not true.  Therefore, changed to test for locked afterwards...
            // data->indexLocked = 0 ;

            Bool5 setUnlock = 1 ;  // by default we will be unlocked
            for ( tagOn = 0 ;; )
            {
               tagOn = dfile4tagNext( data, tagOn ) ;
               if ( tagOn == 0 )
               {
                  rc = error4code( data->c4 ) ;
                  if ( rc < 0 )
                     return rc ;
                  if ( setUnlock == 1 )
                     data->indexLocked = 0 ;
                  return 0 ;
               }
               rc = tfile4unlock( tagOn, serverId ) ;
               if ( rc < 0 )
                  return rc ;
               if ( tagOn->fileLocked != 0 )
               {
                  // means after calling unlock, still in locked state (probably another client)
                  // therefore, we cannot set the data->indexLocked switch to off...
                  setUnlock = 0 ;
               }
            }
         #else
            for ( indexOn = 0 ;; )
            {
               indexOn = (INDEX4FILE *)l4next(&data->indexes,indexOn) ;
               if ( indexOn == 0 )
               {
                  rc = error4code( data->c4 ) ;
                  if ( rc < 0 )
                     return rc ;
                  return 0 ;
               }
               index4unlock( indexOn, serverId ) ;
            }
         #endif
      }
   #endif



   int dfile4unlockRecordDo( DATA4FILE *data, long rec )
   {
      FILE4LONG position ;
      CODE4 *c4 ;
      #ifdef S4FOX
         FILE4LONG pos2 ;
       #endif

      c4 = data->c4 ;

      if ( data->file.lowAccessMode != OPEN4DENY_RW )
      {
         // AS Nov 4/03 - large file support
         file4longAssign( position, 0, c4->largeFileOffset ) ;
         if ( c4->largeFileOffset == 0 )
         {
            #ifdef S4CLIPPER
               file4longAdd( &position, L4LOCK_POS ) ;
               file4longAdd( &position, rec ) ;
            #endif
            #ifdef S4MDX
               file4longAdd( &position, L4LOCK_POS ) ;
               file4longSubtract( &position, rec + 1U ) ;
            #endif
            #ifdef S4FOX
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
            #endif
            if ( file4unlockInternal( &data->file, file4longGetLo( position ), 0, 1L, 0 ) < 0 )
               return -1 ;
         }
         else
         {
            if ( file4unlockInternal( &data->file, rec, c4->largeFileOffset, 1L, 0 ) < 0 )
               return -1 ;
         }
      }

      assert5( data->recordLockWriteCount > 0 ) ;
      data->recordLockWriteCount-- ;
      #ifdef S4SERVER_GUI
         c4->server->info.lockCount-- ;
      #endif
      return 0 ;
   }

#endif /* !defined( S4OFF_MULTI ) && !defined( S4CLIENT ) */
