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

/* d4close.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

#ifndef S4OFF_MEMO
   extern unsigned short f4memoNullChar ;
#endif  /* not S4OFF_MEMO */

#ifndef S4CLIENT
   int dfile4closeLow( DATA4FILE *data )
   {
      /* closes the given datafile if it's user count is zero */
      int finalRc ;
      // CODE4 *c4 ;
      #ifndef S4OFF_INDEX
         #ifdef S4CLIPPER
            TAG4FILE *t4 ;
         #else
            INDEX4FILE *i4 ;
         #endif
      #endif

      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif

      if ( data->userCount > 0 )
         return 0 ;

      if ( data->info != 0 )
      {
         u4free( data->info ) ;
         data->info = 0 ;
      }

      // AS Sep 2/03 - It is possible c4 is null.
      CODE4 *c4 = data->c4 ;
      if ( c4 == 0 )
         return -1 ;

      finalRc = error4set( c4, 0 ) ;

      if ( file4openTest( &data->file ) )
      {
         if ( c4getDoRemove( c4 ) == 1 )
            file4setTemporary( &data->file, 1, 1 ) ;

         #ifndef S4OFF_WRITE
            if ( data->fileChanged && file4getTemporary( &data->file ) != 1 && data->file.isReadOnly == 0 )
            {
               u4yymmdd( &data->yy ) ;
               #ifdef S4OFF_MULTI
                  dfile4updateHeader( data, 1, 1, 0 ) ;
               #else
                  if ( data->file.lowAccessMode == OPEN4DENY_RW )
                     dfile4updateHeader( data, 1, 1, 0 ) ;
                  else // only date stamp required
                     dfile4updateHeader( data, 1, 0, 0 ) ;
               #endif
            }
         #endif
      }

      #ifndef S4OFF_MEMO
         if ( file4openTest( &data->memoFile.file ) )
         {
            if ( c4getDoRemove( c4 ) == 1 )
               file4setTemporary( &data->memoFile.file, 1, 0 ) ;
            if ( file4close( &data->memoFile.file ) < 0 )
               finalRc = error4set( c4, 0 ) ;
         }
      #endif

      #ifndef S4OFF_INDEX
         for ( ;; )
         {
            #ifdef S4CLIPPER
               t4 = (TAG4FILE *)l4first( &data->tagfiles ) ;
               if ( t4 == 0 )
                  break ;
               tfile4close( t4, data ) ;
            #else
               i4 = (INDEX4FILE *)l4first( &data->indexes ) ;
               if ( i4 == 0 )
                  break ;
               index4close( i4 ) ;
            #endif
         }
      #endif

      if ( data->link.n != 0 )
         l4remove( &c4->dataFileList, data ) ;

      if ( file4openTest( &data->file ) )
      {
         // AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
         #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
            // AS Nov 7/02 - don't mark for invalid tables - ocurred with preprocess on
            if ( c4->validationTable != 0 && data->valid == 1 )
               code4validateAddClose( c4, data->file.name, file4getTemporary( &data->file ) ) ;
         #endif

         if ( file4close( &data->file ) < 0 )
            finalRc = error4set( c4, 0 ) ;
      }

      data->record = 0 ;

      // AS May 17/04 - server support for data file compression
      #if defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
         // AS Feb 18/04 - we were not always freeing all associated memory
         if ( data->compressInfo != 0 )
         {
            compress4infoFree( data->compressInfo ) ;
            data->compressInfo = 0 ;
         }
      #endif

      #ifdef S4LOCK_HASH
         delete data->lockHash ;
      #endif
      mem4free( c4->data4fileMemory, data ) ;
      data = 0 ;
      error4set( c4, (short)finalRc ) ;

      return finalRc ;
   }



   int code4dataFileCloseAll( CODE4 *c4 )  // CS 2000/12/20 remove static for server compile
   {
      /* closes all datafiles for which the user count is zero */
      DATA4FILE *data ;
      int rc ;

      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E91304 ) ;
      #endif

      for( ;; )
      {
         for ( data = (DATA4FILE *)l4first( &c4->dataFileList ) ;; )
         {
            if ( data == 0 )
               break ;
            // AS May 16/03 - To fix a server-side closing issue - after create but before open it was possible that
            // the server would close the file, and if it was temporary it was being deleted.
            if ( data->userCount != 0 )
               data = (DATA4FILE *)l4next( &c4->dataFileList, data ) ;
            else
               break ;
         }
         if ( data == 0 )
            break ;
         rc = dfile4closeLow( data ) ;
         if ( rc < 0 )  /* need to return in order to avoid endless loop */
            return error4stack( c4, (short)rc, E91304 ) ;
      }

      return 0 ;
   }
#endif /* !S4CLIENT */



int dfile4close( DATA4FILE *data )
{
   // #ifndef S4STAND_ALONE
   //    CODE4 *c4 ;
   // #endif
   #ifdef S4CLIENT
      // int finalRc ; //, saveRc ;
      INDEX4FILE *i4 ;
      CONNECTION4 *connection ;
   #endif

   #ifdef E4PARM_LOW
      if ( data == 0 )
         return error4( 0, e4parm_null, E91102 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( data->userCount <= 0 )
      {
         // AS 04/30/99 -- added 'data->valid', else failing when open fails...
         #ifndef S4CLIENT
            if ( data->valid )
         #endif
               return error4( 0, e4struct, E91102 ) ;
      }
   #endif

   #ifndef S4STAND_ALONE
      CODE4 *c4 = data->c4 ;
      assert5( c4 != 0 ) ;
   #endif

   #ifdef S4CLIENT
      int saveRc = error4set( c4, 0 ) ;
      int finalRc = 0 ;
   #endif

   if ( data->userCount > 0 )  // may be at 0 if datafile is invalid...
      data->userCount-- ;
   assert5( data->userCount >= 0 ) ;
   if ( data->userCount == 0 )
   {
      #ifdef S4CLIENT
         if ( data->info != 0 )
         {
            u4free( data->info ) ;
            data->info = 0 ;
         }

         connection = data->connection ;
         if ( connection == 0 )
            finalRc = e4connection ;
         else
         {
            connection4assign( connection, CON4CLOSE, 0, data->serverId ) ;
            connection4sendMessage( connection ) ;
            finalRc = connection4receiveMessage( connection ) ;
            if ( finalRc >= 0 )
               finalRc = connection4status( connection ) ;
         }
         #ifndef S4OFF_INDEX
            for ( ;; )
            {
               i4 = (INDEX4FILE *)l4first( &data->indexes ) ;
               if ( i4 == 0 )
                  break ;
               index4close( i4 ) ;
            }
         #endif

         l4remove( &c4->dataFileList, data ) ;
         #ifdef S4LOCK_HASH
            delete data->lockHash ;
         #endif
         mem4free( c4->data4fileMemory, data ) ;
         if ( saveRc != 0 )
         {
            error4set( c4, saveRc ) ;
            return saveRc ;
         }
         else
         {
            error4set( c4, finalRc ) ;
            return finalRc ;
         }
      #else
         #ifdef S4SERVER
            // AS Oct 24/02 - also closelow if remove is set to true, so the file is removed (otherwise was left in place)
            // AS Sep 2/03 - for testing purposes, close the testing files even if keepOpen is set to true
            #ifdef S4TESTING
               int oldKeepOpen = c4->server->keepOpen ;
               const char *name = data->file.name ;
               // AS May 28/04 - or the ANDREW.T
               if ( c4->server->keepOpen == 1 && ( name != 0 && (strnicmp( name, "h:\\codebase.t", 13 ) == 0 || strnicmp( name, "h:\\andrew.t", 11 ) == 0 ) ) )
                  c4->server->keepOpen = 0 ;
            #endif

            if ( c4getDoRemove( c4 ) == 1 || c4->server->keepOpen == 0 || data->valid != 1 )    /* not a valid datafile (failure in dfile4open) so close */
            {
               int rc = dfile4closeLow( data ) ;
               #ifdef S4TESTING
                  c4->server->keepOpen = oldKeepOpen ;
               #endif
               return rc ;
            }

            #ifdef S4TESTING
               c4->server->keepOpen = oldKeepOpen ;
            #endif

            if ( file4getTemporary( &data->file ) && c4->server->keepOpen != 2 )  /* cannot leave temp files open or they will never close */
            {
               return dfile4closeLow( data ) ;
            }

            // AS May 16/03 - To fix a server-side closing issue - after create but before open it was possible that
            // the server would close the file, and if it was temporary it was being deleted.
            if ( c4->server->keepOpen == 2 )  /* cannot leave temp files open or they will never close */
               data->userCount = -1 ;
            data->singleClient = 0 ;
            time( &data->nullTime ) ;
         #else
            return dfile4closeLow( data ) ;
         #endif
      #endif  /* S4CLIENT */
   }

   return 0 ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
#ifndef S4SERVER
   static void code4lockClearData( CODE4 *c4, DATA4 *data )
   {
      /* in server we never leave locks in place, since clientId not avail */
      /* clears all locks for a given DATA4 from the list (useful when closing a file) */
      #ifndef S4SINGLE
         LOCK4GROUP *lock ;
         SINGLE4DISTANT lockList ;
         TRAN4 *trans ;

         #ifdef S4SERVER
            trans = &c4->currentClient->trans ;
         #else
            trans = &c4->c4trans.trans ;
         #endif

         single4distantInitIterate( &lockList, &trans->locks ) ;

         for (  ;; )
         {
            lock = (LOCK4GROUP *)single4distantToItem( &lockList ) ;
            if ( lock == 0 )
               break ;
            // AS Apr 15/03 - support for new lockId for shared clone locking
            #ifdef S4SERVER
               if ( lock->lockId != data4lockId( data ) )
            #else // LY Feb 9/05 : !S4SERVER should also get next lock if locked DATA4* doesn't match given DATA4*
               if ( lock->data != data )
            #endif
               {
                  single4distantNext( &lockList ) ;
                  continue ;
               }
            // #else
            //    #ifdef E4ANALYZE
            //       if ( lock->data != data )
            //       {
            //          error4( data->codeBase, e4result, E91102 ) ;
            //          return ;
            //       }
            //    #endif
            // #endif
            single4distantPop( &lockList ) ;
            mem4free( c4->lockGroupMemory, lock ) ;
         }
      #endif /* S4SINGLE */
      return ;
   }
#endif /* !S4SERVER */



#ifdef S4SERVER
   int d4closeTemp( DATA4 *data )
   {
      /*CJ-12/08/99- Use this function to close temporary database on the server that are keeped open so that a client may
                     open them later.  This will not log the close in the database */
      #ifndef S4OFF_TRAN
         CODE4 *c4 = data->codeBase ;
         int oldTranStatus = code4tranStatus(c4) ;
         code4tranStatusSet(c4 , r4off) ;
      #endif
      int rc = d4close( data ) ;
      #ifndef S4OFF_TRAN
         code4tranStatusSet(c4 , oldTranStatus ) ;
      #endif
      return rc ;
   }
#endif /* S4SERVER */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   int d4logClose( DATA4 *data )
   {
      // log the close message to the transaction log file
      /* AS 11/-3/99 -- if d4open() failed (and was not logged in trans file), the close was still getting logged thus causing log file inconsistencies... */
      CODE4 *c4 = data->codeBase ;
      // AS Apr 29/03 - transcations are run-time in odbc now
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
            return 0 ;
      #endif

      int saveRc = 0 ;
      if ( data->dataFile->valid == 1 )
      {
         #ifdef S4SERVER
            if ( c4->currentClient != 0 )
         #endif
            {
               // AS 02/01/01 - This turns out to be a nightmare to maintain, (i.e. to disable code4trans on
               // the close).  Instead, we now track whether the open was logged and if it was then we log
               // the close...
               // if ( code4transEnabled( c4 ) )
               // if open was logged, trans should be enabled here...
               assert5( ( data->openWasLogged && code4transEnabled( c4 ) ) || (data->openWasLogged == 0 ) ) ;
               if ( data->openWasLogged && code4transEnabled( c4 ) )
               {
                  TRAN4 *trans = code4trans( c4 ) ;
                  #ifdef S4STAND_ALONE
                     long connectionId = 0L ;
                  #else
                     long connectionId = c4->currentClient->id ;
                  #endif
                  if ( trans->currentTranStatus != r4active )
                  {
                     int rc = tran4set( trans, trans->currentTranStatus, -1L, connectionId, TRAN4CLOSE, 0, data4clientId( data ), data4serverId( data ) ) ;
                     if ( rc < 0 )
                        saveRc = rc ;
                     if ( tran4lowAppend( trans, "\0", 0 ) != 0 )
                        saveRc = e4transAppend ;
                  }
               }
            }
      }
      return saveRc ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



int S4FUNCTION d4close( DATA4 *data )
{
   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E91302 ) ;
   #endif

   CODE4 *c4 = data->codeBase ;

   if ( error4code( c4 ) == e4unique )
      data->recordChanged = 0 ;

   int saveRc = 0 ;
   int saveRc2 ;
   if ( error4code( c4 ) < 0 )
      saveRc2 = error4set( c4, 0 ) ;
   else
      saveRc2 = 0 ;

   #ifdef S4CLIENT
      d4batchReadFree( data ) ;
      // Apr 19/02 - flush out any bufferred records...
      saveRc = d4writeBufferDo( data ) ;
      if ( saveRc != 0 && saveRc2 == 0 )
         saveRc2 = saveRc ;
      d4batchWriteFree( data ) ;
   #endif

   if ( data->dataFile != 0 )
   {
      /* need to remove any references to any code4lock() calls */
      #ifndef S4SERVER
         /* in server we never leave locks in place, since clientId not avail */
         code4lockClearData( c4, data ) ;
      #endif

      #ifndef S4OFF_WRITE
         saveRc = d4update( data ) ;
         if ( saveRc == e4unique )
            data->recordChanged = 0 ;
      #endif

      int rc = 0 ;

      #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
         rc = d4logClose( data ) ;
         if ( rc < 0 )
            saveRc = rc ;
      #endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */

      #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
         // if we are within a transaction and the file may have changed we just put it into a list of files to be
         // closed.  This is required because if a rollback occurs we must be able to undo the changes, so we need
         // the DATA4, with all locks in place, in order to perform the rollback
         // AS 04/10/00 if the table is open read-only, I changed to go through with the close since we should not
         // need the table for rolling back.  Plus with SQL, we would open read-only on select and then could not
         // re-open for write if we wanted to make a change within the same transaction
         TRAN4 *trans = code4trans( c4 ) ;
         #ifdef S4CLIENT
            if ( code4transEnabled( c4 ) && code4tranStatus( c4 ) == r4active )  // can't unlock in this instance
         #else
            // AS If the file is opened temporary we can also go ahead and close it.
            if ( data->dataFile->file.isReadOnly == 0 && trans != 0 && trans->currentTranStatus == r4active && data->dataFile->file.isTemporary == 0 )
         #endif
         {
            l4remove( tran4dataList( data->trans ), data ) ;
            l4add( &(trans->closedDataFiles), data ) ;
            return 0 ;
         }
      #endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) */

      #ifndef S4SINGLE
         // AS Apr 15/03 - support for new lockId for shared clone locking
         if ( d4unlockLow( data, data4lockId( data ), 0 ) < 0 )
            saveRc = error4set( c4, 0 ) ;
         // AS Feb 17/03 - Detect any left locks in debug
         // AS May 20/03 - compile fix for client
         #if !defined(S4CLIENT) && defined(__cplusplus)
            assert5( data->lockedRecords.next() == 0 ) ;
         #endif
      #endif  /* S4SINGLE */

      #ifndef S4INDEX_OFF
         for( INDEX4 *indexNext = (INDEX4 *)l4first( &data->indexes );; )
         {
            INDEX4 *indexOn = indexNext ;
            indexNext = (INDEX4 *)l4next( &data->indexes, indexOn ) ;
            if ( !indexOn )
               break ;

            #ifdef S4CLIPPER
               // AS 06/25/99 --> in clipper, was not removing index files when closing
               // removed data files.  If the data file is marked as 'isTemp', then we
               // should remove index files as well...
               // AS 07/22/99 addendum - if c4->keepOpen == 2 in server case, we do not want to
               // remove the file becuase server->keepOpen of 2 means that we want to perform a soft close
               // of a temporary file (done often as part of create process).
               // AS 11/27/00 - in server, must use c4getDoRemove because the flag is on a client by client basis
               int oldDoRemove = c4getDoRemove( c4 ) ;
               if ( file4getTemporary( &data->dataFile->file ) == 1 )
               {
                  #ifdef S4SERVER
                     if ( c4->server->keepOpen != 2 )
                  #endif
                        c4setDoRemove( c4, 1 ) ;
               }
            #endif
            int rc = i4closeLow( indexOn ) ;
            #ifdef S4CLIPPER
               c4setDoRemove(c4, oldDoRemove ) ;
            #endif
            if ( rc < 0 )
            {
               saveRc = rc ;
               error4set( c4, 0 ) ;
            }
         }
      #endif

      #ifndef S4OFF_MEMO
         if ( data->fieldsMemo != 0 )
         {
            for ( int memoFieldIndex = 0 ; memoFieldIndex < data->dataFile->nFieldsMemo ; memoFieldIndex++ )
            {
               #ifndef S4CLIENT
                  // AS Jun 2/03 - Was failing to properly update indexes on memo fields.  Need to keep a copy of the old key around.
                  if ( data->fieldsMemo[memoFieldIndex].contentsOld != 0 )
                  {
                     u4free( data->fieldsMemo[memoFieldIndex].contentsOld ) ;
                     data->fieldsMemo[memoFieldIndex].contentsOld = 0 ;
                  }
               #endif

               if ( data->fieldsMemo[memoFieldIndex].contents != (char *)(&f4memoNullChar) )
               {
                  u4free( data->fieldsMemo[memoFieldIndex].contents ) ;
                  data->fieldsMemo[memoFieldIndex].contents = (char *)(&f4memoNullChar) ;
               }
            }

            u4free( data->fieldsMemo ) ;
            data->fieldsMemo = 0 ;
         }
      #endif  /* S4OFF_MEMO */

      #ifdef S4SERVER
         /* 05/31/96 AS exclusive open not getting cleared with KEEPOPEN (t4code4.c) */
         if ( data->dataFile->exclusiveOpen == data )
            data->dataFile->exclusiveOpen = 0 ;
      #endif

      #ifndef S4CLIENT
         #ifndef S4OFF_MULTI
            /* AS 02/02/99 - If from a failed open, the link might not be added yet, so check first... */
            if ( data->dataFileLink.n != 0 )
            {
               l4remove( &data->dataFile->data4list, &data->dataFileLink ) ;
               // AS Oct 31/02 - ensure it is removed...
               assert5( data->dataFileLink.n == 0 ) ;
            }
         #endif
      #endif

      #ifdef S4CLIENT
         if ( c4getDoRemove( c4 ) == 1 )
            rc = dfile4remove( data->dataFile ) ;
         else
      #endif
         rc = dfile4close( data->dataFile ) ;
      if ( rc < 0 )
         saveRc = rc ;
      data->dataFile = 0 ;

      if ( data->groupRecordAlloc != 0 )
      {
         u4free( data->groupRecordAlloc ) ;
         data->record = 0 ;
         data->recordOld = 0 ;
         data->fields = 0 ;
         data->recordBlank = 0 ;
      }
      else
      {
         u4free( data->record ) ;
         u4free( data->recordOld ) ;
         u4free( data->fields )  ;
         u4free( data->recordBlank ) ;
         #ifdef S4CLIENT
            /* AS Apr 10/02 - New function for advance-reading client/server */
            if ( data->batchRead.recordExtra != 0 )
               u4free( data->batchRead.recordExtra ) ;
         #endif
      }

      if ( data->trans != 0 )
         l4remove( tran4dataList( data->trans ), data ) ;
      #ifdef E4ANALYZE
         else
         {
            LINK4 compareLink ;
            memset( &compareLink, 0, sizeof( LINK4 ) ) ;
            if ( c4memcmp( &data->link, &compareLink, sizeof( LINK4 ) ) != 0 )
               saveRc = error4( c4, e4struct, E81305 ) ;
         }
      #endif
   }
   else
      if ( data->trans != 0 )
         l4remove( tran4dataList( data->trans ), data ) ;

   // AS July 30/02 - At least reset the debugInt so that if we attempt to use this (now freed) memory
   // again in v.b. we will get an error.
   #ifdef E4VBASIC
      data->debugInt = 0 ;
   #endif

   if ( c4->dataMemory != 0 ) /* LY 2002/08/14 : avoid -935 error */
      mem4free( c4->dataMemory, data ) ;

   if ( saveRc != 0 )
      error4set( c4, (short)saveRc ) ;

   if ( saveRc2 != 0 )  // AS Apr 24/02 - need non-zero return for batch updating / r4unique...
   {
      error4set( c4, (short)saveRc2 ) ;
      return saveRc2 ;
   }
   return saveRc ;
}
