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

/* d4skip.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4SERVER
   long d4skipRecno( DATA4 *data, long n )
   {
      /* -2 means eof, -3 means bof */
      #ifndef S4OFF_INDEX
         TAG4 *tag ;
         TAG4FILE *tagFile ;
      #endif
      long startRec, newRec ;
      #ifndef S4OFF_INDEX
         unsigned char *keyValue ;
         int rc ;
         long recno, nSkipped ;
      #endif
      CODE4 *c4 ;

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94801 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E94801 ) ;
      #endif

      c4 = data->codeBase ;
      if ( c4 == 0 )
         return e4info ;
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;

      if ( data->recNum < 1L )
      {
         #ifndef S4SERVER
            if ( c4->errSkip )
               return error4( c4, e4info, E84801 ) ;
         #endif
         return e4info ;
      }

      #ifndef S4OFF_INDEX
         tag = data->tagSelected ;  /* avoid function call */
         if ( tag == 0 )
         {
      #endif
         if ( n != 0L )
            if ( d4recCountLessEq( data, 1L ) == 0 )  /* count == 0 */
               return -4L ;

         data->bofFlag = 0 ;
         startRec = data->recNum ;
         newRec = startRec + n ;

         if ( newRec > 0L )
         {
            if ( d4recCountLessEq( data, newRec ) == 0 )
               return -2L ;
            return newRec ;
         }
         else
            return -3L ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            tagFile = tag->tagFile ;
            if ( data->eofFlag )
            {
               if ( n >= 0L )
                  return -2L ;

               rc = d4bottom( data ) ;
               if ( rc && rc != r4eof )
                  return rc ;
               if ( rc == r4eof )
                  return -2L ;
               n++ ;
               data->recNum = (long)tfile4recNo( tagFile ) ;
            }

            data->bofFlag = 0 ;

            #ifndef S4OFF_WRITE
               if ( data->recordChanged )
               {
                  /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
                  rc = d4updateRecord( data, 0, 1 ) ;
                  if ( rc < 0 )
                     return rc ;
               }
            #endif

            t4versionCheck( tag, 1, 0 ) ;

            if ( n == 0 )
               return data->recNum ;

            if ( (long)tfile4recNo( tagFile ) != data->recNum )
            {
               if ( d4lockTest( data, data->recNum, lock4any ) != 1 )  /* ensure latest from disk */
               {
                  rc = d4go( data, data->recNum ) ;
                  if ( rc < 0 )
                     return rc ;
               }

               expr4context( tagFile->expr, data ) ;
               tfile4exprKey( tagFile, &keyValue ) ;

               rc = tfile4go( tagFile, keyValue, data->recNum, 0 ) ;
               if ( rc < 0 )
                  return rc ;

               if ( tfile4empty( tagFile ) )
                  return -4L ;

               if ( tfile4eof( tagFile ) && n >= 0L )
                  return -2L ;

               #ifdef S4HAS_DESCENDING
                  if ( tagFile->header.descending )
                  {
                     if ( (rc > 0) && (n < 0) )
                        n-- ;
                  }
                  else
                     if ( (rc > 0) && (n > 0) )
                        n-- ;
               #else
                  if ( (rc > 0) && (n > 0) )
                     n-- ;
               #endif
            }
            else
            {
               if ( tfile4eof( tagFile ) )
                  return -2L ;
            }

            nSkipped = tfile4dskip( tagFile, n ) ;

            if ( n > 0 && nSkipped != n )
               return -2L ;
            if ( n < 0 && nSkipped != n )
               return -3L ;

            if ( tfile4eof( tagFile ) )
               return -2L ;

            recno = (long)tfile4recNo( tagFile ) ;
            if ( recno < 0 )
               return recno ;
            return recno ;
         }
      #endif
   }
#endif /* S4SERVER */



/* flush the current record to disk, and update the data pointer to point
   to a valid tag location (in case the current record has been filtered out */
/* direction is -1 if line up backwards, or 1 if line up forwards */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4tagSync( DATA4 *data, TAG4 * const tag )
{
   #ifdef S4OFF_INDEX
      return 0 ;
   #else
      CODE4 *c4 ;
      #ifdef S4CLIENT
         int rc, saveRc ;
         CONNECTION4 *connection ;
         CONNECTION4TAG_SYNCH_INFO_IN *info ;
         CONNECTION4TAG_SYNCH_INFO_OUT *out ;
         #ifndef S4OFF_MEMO
            int i ;
         #endif
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94803 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E94803 ) ;
      #endif

      if ( tag == 0 )
         return 0 ;

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;

      #ifdef S4CLIENT
         // AS Oct 25/05 - support for low level tag operations
         tag->tagFile->tagDataValid = 0 ;  // reset to invalid
         if ( data->recNum <= 0 || d4eof( data ) )
         {
            #ifndef S4OFF_MEMO
               for ( i = 0; i < data->dataFile->nFieldsMemo; i++ )
                  f4memoReset( data->fieldsMemo[i].field ) ;
            #endif
            data->recordChanged = 0 ;
            return 0 ;
         }

         if ( data->recordChanged == 0 )
         {
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc )
               return rc ;
         }

         connection = data->dataFile->connection ;
         if ( connection == 0 )
            return error4stack( data->codeBase, e4connection, E94803 ) ;

         connection4assign( connection, CON4TAG_SYNCH, data4clientId( data ), data4serverId( data ) ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4TAG_SYNCH_INFO_IN ), (void **)&info ) ;
         memcpy( info->tagName, tag->tagFile->alias, LEN4TAG_ALIAS  ) ;
         info->tagName[LEN4TAG_ALIAS] = 0 ;
         // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
         u4ncpy( info->indexFileName, tag->tagFile->indexFile->accessName, strlen( tag->tagFile->indexFile->accessName ) + 1 ) ;
         info->recno = htonl5(d4recNo( data )) ;
         saveRc = connection4repeat( connection ) ;

         if ( saveRc == r4eof || saveRc == r4noRecords )
         {
            data->bofFlag = 0 ;
            rc = d4goEof( data ) ;
            if ( rc == 0 )
               rc = r4eof ;
            if ( saveRc == r4noRecords )
               data->bofFlag = 1 ;
            return rc ;
         }

         if ( saveRc == r4bof )
         {
            data->eofFlag = 0 ;
            rc = d4goBof( data ) ;
            data->bofFlag = 1 ;
            if ( rc == 0 )
               rc = r4bof ;
            return rc ;
         }

         if ( saveRc != 0 && saveRc != r4after )
         {
            if ( saveRc < 0 )
               return connection4errorDescribe( connection, c4, saveRc, E94803, tag->tagFile->alias, 0, 0 ) ;
            return saveRc ;
         }

         if ( connection4len( connection ) != sizeof( CONNECTION4TAG_SYNCH_INFO_OUT ) )
            return error4( c4, e4packetLen, E94803 ) ;
         out = (CONNECTION4TAG_SYNCH_INFO_OUT *)connection4data( connection ) ;
         out->recNo = ntohl5(out->recNo) ;
         if ( out->recNo != 0 && out->recNo != d4recNo( data ) )
         {
            rc = d4go( data, out->recNo ) ;
            if ( rc < 0 )
               return rc ;
         }

         return saveRc ;
      #else
         return d4tagSyncDo( data, tag, 1 ) ;
      #endif
   #endif /* S4OFF_INDEX */
}

#if !defined(S4OFF_INDEX) && !defined(S4CLIENT)
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int d4tagSyncDo( DATA4 *data, TAG4 * const tag, int direction )
   {
      int rc, saveRc = 0 ;
      unsigned char *keyValue ;
      #ifndef S4OFF_MULTI
         int done, verifyReccount ;
         #ifndef S4OFF_TRAN
            int nSkipped ;
            CODE4 *c4 ;
         #endif
      #endif
      TAG4 *oldSelected ;
      TAG4FILE *tagFile ;
      long recno ;

      #if !defined(S4OFF_MULTI) && !defined(S4OFF_TRAN)
         c4 = data->codeBase ;
      #endif

      // AS Sep 8/08 - update the record prior to positioning to avoid being in an invalid position
      #ifndef S4OFF_WRITE
         if ( data->recordChanged )
         {
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc )
            {
               return rc ;
            }
         }
      #endif

      tagFile = tag->tagFile ;
      expr4context( tagFile->expr, data ) ;
      tfile4exprKey( tagFile, &keyValue ) ;
      recno = (long)tfile4recNo( tagFile ) ;
      if ( recno != data->recNum )
      {
         rc = tfile4go( tagFile, keyValue, data->recNum, 0 ) ;
         if ( rc < 0 )
            return rc ;
         if ( rc == r4after )
            saveRc = r4after ;
      }

      oldSelected = data->tagSelected ;
      d4tagSelect( data, tag ) ;

      if ( data->eofFlag )
      {
         d4tagSelect( data, oldSelected ) ;

         /* if forward skip not true, not eof, here is */
         return d4goEof( data ) ;
      }
      else
      {
         t4versionCheck( tag, 1, 0 ) ;

         if ( tfile4eof( tagFile ) )
            return d4goEof( data ) ;

         recno = (long)tfile4recNo( tagFile ) ;
         if ( recno != data->recNum )
         {
            rc = d4go( data, recno ) ;
            saveRc = r4after ;
         }
         else
            rc = 0 ;
      }

      #ifndef S4OFF_MULTI
         #ifdef S4SERVER
            // AS Apr 15/03 - support for new lockId for shared clone locking
            if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4write ) != 1 )
            if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4read ) != 1 )
         #else
            if ( d4lockTestFile( data ) != 1 )
         #endif
            for( verifyReccount = 1, done = 0 ; done == 0 ; )
            {
               #ifndef S4OFF_TRAN
                  /* if transactions are enabled, we do not remove keys from an index file when
                     records are changed to avoid unique problems caused by another user.
                     The result of this is we may need to re-sync ourselves in that case - namely,
                     we do not want to position to a record indicated by a 'removed' entry in
                     a tag.
                  */
                  // AS Apr 29/03 - transcations are run-time in odbc now
                  // AS Jun 20/03 - was checking wrong flag
                  #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
                     if ( c4->server->odbcTrans == 1 )  // odbc build, no trans available
                  #endif
                        if ( code4transEnabled( c4 ) )
                        {
                           if ( t4unique( tag ) != 0 || tagFile->filter != 0 )
                              if ( code4tranStatus( c4 ) == r4active && d4lockTest( data, recno, lock4write ) == 1 )
                              {
                                 /* if within a transaction, and record is locked, it may mean
                                    that the record contents have changed.  If the current tag key
                                    does not match, it must be an old entry (if a unique tag),
                                    and thus skip over
                                 */
                                 for ( ;; )
                                 {
                                    if ( recno > dfile4getMinCount( data->dataFile ) && verifyReccount == 1 )   /* ensure correct sequencing for multi-user */
                                       break ;  /* just let the code below take care of this case */

                                    Bool5 passedFilterTest = 1 ;
                                    Bool5 passedUniqueTest = 1 ;
                                    /* AS 03/25/99 --> also must consider instance where a record now becomes
                                       filtered from the record set...*/
                                    if ( tagFile->filter != 0 )
                                    {
                                       expr4context( tagFile->filter, data ) ;
                                       if ( recno != d4recNo( data ) )  /* need to reposition data4 first */
                                           d4go( data, recno ) ;
                                       if ( expr4true( tagFile->filter ) == 0 ) // should be filtered
                                       {
                                          nSkipped = (int)tfile4skip( tagFile, direction ) ;
                                          passedFilterTest = 0 ;
                                       }
                                    }

                                    if ( t4unique( tag ) != 0 && passedFilterTest == 1 )
                                    {
                                       expr4context( tagFile->expr, data ) ;
                                       if ( recno != d4recNo( data ) )  /* need to reposition data4 first */
                                           d4go( data, recno ) ;
                                       tfile4exprKey( tagFile, &keyValue ) ;
                                       if ( memcmp( tfile4key( tagFile ), keyValue, tagFile->header.keyLen ) != 0 )  /* different, so skip */
                                       {
                                          nSkipped = (int)tfile4skip( tagFile, direction ) ;
                                          passedUniqueTest = 0 ;
                                       }
                                    }

                                    if ( passedUniqueTest == 1 && passedFilterTest == 1 )  // done
                                       break ;

                                    verifyReccount = 1 ;  /* don't re-check reccount if all is ok... */
                                    if ( direction > 0  &&  nSkipped != 1 )
                                    {
                                       rc = d4goEof( data ) ;
                                       done = 1 ;
                                       break ;
                                    }

                                    if ( tfile4eof( tagFile ) )
                                    {
                                       data->bofFlag = 1 ;
                                       rc = d4goEof( data ) ;
                                       done = 1 ;
                                       break ;
                                    }
                                    if ( (long)tfile4recNo( tagFile ) != recno ) /* ok, may continue */
                                    {
                                       recno = (long)tfile4recNo( tagFile ) ;
                                       d4go( data, recno ) ;
                                       // AS Jan 13/09 - we may not be done yet....example case is that this record also doesn't belong to the tag in which case we need to still keep going forward
                                       continue ;
                                    }
                                 }
                              }
                        }
               #endif /* S4OFF_TRAN */

               if ( done == 1 )
                  break ;

               if ( verifyReccount == 1 )
               {
                  verifyReccount = 0 ;
                  if ( recno > dfile4getMinCount( data->dataFile ) )   /* ensure correct sequencing for multi-user */
                  {
                     while ( d4recCountLessEq( data, recno ) == 0 && done == 0 && rc == 0 )
                     {
                        // AS July 16/02 - incrrectly returning -1 in this case.
                        rc = (int)tfile4skip( tagFile, direction ) ;
                        if ( rc == -direction )
                           done = 1 ;
                        else
                        {
                           if ( rc == 0 )
                           {
                              data->bofFlag = 1 ;
                              done = 1 ;
                              rc = d4goEof( data ) ;
                           }
                           else
                           {
                              recno = (long)tfile4recNo( tagFile ) ;
                              if ( recno <= 0 )
                              {
                                 done = 1 ;
                                 rc = -1 ;
                              }
                              if ( recno > dfile4getMinCount( data->dataFile ) )  // skip to next one
                                 continue ;
                              if ( (long)tfile4recNo( tagFile ) != recno ) /* ok, may continue */
                              {
                                 recno = (long)tfile4recNo( tagFile ) ;
                                 d4go( data, recno ) ;
                                 break ;
                              }
                           }
                        }
                     }
                  }
                  else
                     done = 1 ;
               }
               else
                  done = 1 ;
            }
      #endif /* S4OFF_MULTI */

      d4tagSelect( data, oldSelected ) ;

      if ( saveRc == 0 )
         return rc ;
      else
         return saveRc ;
   }
#endif  // !S4CLIENT && !S4OFF_INDEX



#ifdef S4CLIENT
   /* AS Apr 10/02 - New functionality for advance-reading client/server (reading a block) */
   // S4CLIENT
   // AS Aug 30/02 - added potential to call from d4seek...
   int d4skipFetchLow( DATA4 *data, short mode, const long nSkip, const char *seekKey, short keyLen, double dkey )
   {
      // mode indicates the # of dkips to perform, with the following special value codes:
      // BATCH4SKIP       0 means skip from current position
      // BATCH4TOP       -1 means perform a d4top first
      // BATCH4BOTTOM    -2 means perform a d4bottom first
      // BATCH4SEEKNEXT  -3 means perform a d4seekNext first
      // BATCH4SEEKMATCH -4 means perform a d4seek first, and use d4seekNext for succeeding skips (buffer all matching records)
      // BATCH4SEEK      -5 means perform a d4seek first, and use d4skip for succeeding skips (read in the next group of records whether or not the match)
      assert5port( "New function for advance-reading client/server" ) ;
      // attempts a single fetch call (with buffer option).  does not handle case where r4locked
      // returned should mean to re-try locking
      int saveRc ;

      CODE4 *c4 = data->codeBase ;
      CONNECT4 *connect = c4getClientConnect( c4 ) ;
      long recWidth = dfile4recWidth( data->dataFile ) ;

      if ( connect == 0 )
         return error4( c4, e4connect, E94802 ) ;

      connect4sendShort( connect, MSG5SKIP ) ;
      connect4sendShort( connect, mode ) ;
      connect4sendLong( connect, data4clientId( data ) ) ;
      connect4sendLong( connect, data4serverId( data ) ) ;
      connect4sendShort( connect, c4->readLock ) ;
      connect4sendShort( connect, code4unlockAuto( c4 ) ) ;
      // if ( keyLen != 0 )
      //    connect4sendLong( connect, -3L ) ;
      // else
      connect4sendLong( connect, data->recNum ) ;
      connect4sendLong( connect, nSkip ) ;
      long numToRead = data->batchRead.readRecsToBuf ;
      if ( data->batchRead.readRecsToBufNext != 0 && data->batchRead.readRecsToBufNext != data->batchRead.readRecsToBuf )
      {
         if ( data->batchRead.readRecsToBufNext != data->batchRead.readRecsToBuf )  // need to reallocate the buffers
         {
            int rc = d4readBuffer( data, data->batchRead.readRecsToBufNext, 0 ) ;
            if ( rc < 0 )
               return rc ;
         }
         numToRead = data->batchRead.readRecsToBufNext ;
      }
      long modus = 0 ;  // AS Jul 17/09 - new modus support

      if ( numToRead == 0 )   // mean's not bufferring, just read 1 record
      {
         numToRead = 1 ;
      }
      else
         if ( data->batchRead.modusStart < 0 )
            modus = data->batchRead.modusStart ;
      connect4sendLong( connect, numToRead ) ;
      connect4sendLong( connect, modus ) ;

      short doMemos = data->batchRead.doMemos ;
      if ( numToRead == 1 )  // on single fetch consider single-fetch memo setting
         doMemos = data->includeMemos ;
      connect4sendShort( connect, doMemos ) ;

      #ifdef S4OFF_INDEX
         connect4sendShort( connect, 0 ) ;  // doesn't use tag
      #else
         char *tagName = 0 ;
         TAG4 *tag = data->tagSelected ;
         if ( tag == 0 && keyLen != 0 ) // use default tag
         {
            tag = d4tagDefault( data ) ;
            if ( tag == 0 ) // no tag for seek
               return r4noTag ;
         }

         if ( tag == 0 )
            connect4sendShort( connect, 0 ) ;  // doesn't use tag
         else
         {
            // AS Oct 25/05 - support for low level tag operations
            tag->tagFile->tagDataValid = 0 ;  // reset to invalid
            connect4sendShort( connect, 1 ) ;  // does use tag
            connect4send( connect, tag->tagFile->alias, LEN4TAG_ALIAS ) ;
            short indexNameLen = strlen( tag->tagFile->indexFile->accessName ) ;
            connect4sendShort( connect, indexNameLen ) ;  // does use tag
            connect4send( connect, tag->tagFile->indexFile->accessName, indexNameLen  ) ;

            // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
            tagName = tag->tagFile->alias ;

            // may be doing a seek or seek-next...
            if ( keyLen != 0 )
            {
               if ( seekKey == 0 )  // double
                  connect4sendShort( connect, r4double ) ;
               else
                  connect4sendShort( connect, r4str ) ;

               if ( mode != BATCH4SEEKNEXT || d4eof( data ) || d4bof( data ) || data->recNum <= 0 )
                  connect4sendLong( connect, 0L ) ;  // indicate to do a seek from the bof position
               else
                  connect4sendLong( connect, d4recNo( data ) ) ;  // start position for the seek

               if ( seekKey == 0 )  // double
               {
                  dkey = ntohd( dkey ) ;
                  connect4send( connect, &dkey, sizeof( double ) ) ;
               }
               else
               {
                  connect4sendShort( connect, keyLen ) ;
                  connect4send( connect, seekKey, keyLen ) ;
               }
            }
         }
      #endif

      connect4sendFlush( connect ) ;

      for ( Bool5 isDone = 0 ; isDone == 0 ; )
      {
         short code = connect4receiveShort( connect ) ;
         switch ( code )
         {
            case MSG5SKIP:  // means done processing any unlock codes
               isDone = 1 ;
               break ;
            case STREAM4UNLOCK_ALL:
               code4unlockSet( c4 ) ;
               break ;
            case STREAM4UNLOCK_DATA:
               {
                  // AS Apr 15/03 - support for new lockId for shared clone locking
                  long unlocklockId = connect4receiveLong( connect ) ;
                  long unlockServerId = connect4receiveLong( connect ) ;
                  d4unlockClientData( tran4data( &(c4->c4trans.trans), unlockServerId, unlocklockId )) ;
               }
               break ;
            default:  // means unexpected/bad data
               // throw an exception
               // #ifdef S4TESTING
               //    MessageBox( 0, "problem for debugging", __FILE__, MB_OK ) ;
               // #endif
               return error4( c4, e4info, E94802 ) ;

         }
      }

      Bool5 includeMemos = data->includeMemos ;

      short rc = connect4receiveShort( connect ) ;
      if ( rc != 0 )
      {
         saveRc = rc ;
         switch ( rc )
         {
            case r4eof:
            case r4noRecords:
               data->bofFlag = 0 ;
               rc = d4goEof( data ) ;
               if ( rc == 0 )
                  rc = r4eof ;
               if ( saveRc == r4noRecords )
                  data->bofFlag = 1 ;
               return rc ;
            case r4bof:
               {
                  data->eofFlag = 0 ;
                  long oldRecs = data->batchRead.readRecsToBuf ;  // disable to avoid bufferring when top record read
                  data->batchRead.readRecsToBuf = 0 ;
                  rc = d4goBof( data ) ;
                  data->batchRead.readRecsToBuf = oldRecs ;
                  data->bofFlag = 1 ;
                  if ( rc == 0 )
                     rc = r4bof ;
               }
               return rc ;
            case r4after:  // means 1 row will be retrieved, so continue on
               if ( keyLen == 0 )  // should never happen...
                  return rc ;
               numToRead = 1 ;  // r4after case, only 1 record sent back
               includeMemos = data->batchRead.doMemos ;  // override normal data->readMemos for single skip
               break ;
            // AS Oct 13/05 - is posible that r4entry is returned in client/server if the index is corrupt...
            case r4entry:
               data->recNum = -1 ;  /* at an invalid position */
               d4blankLow( data, data->record ) ;

               // AS Jan 25/06 - not necessarily an error...in the batch read case, r4entry is expected and should be returned out as is
               if ( c4->errGo && mode != BATCH4SEEKNEXT )
                  return error4describe( c4, e4read, E91102, d4alias( data ), 0, 0 ) ;

               return rc ;
            default:
               if ( rc < 0 )
                  return error4( c4, rc, E94802 ) ;
               return rc ;
         }
      }

      if ( numToRead == 1 )
      {
         // normal case (no buffer)
         long recNo = connect4receiveLong( connect ) ;
         short recordLocked = connect4receiveShort( connect ) ;

         assert5( data->batchRead.recordExtra != 0 ) ;
         connect4receive( connect, data->batchRead.recordExtra, recWidth, code4timeoutVal( c4 ) ) ;
         // AS Mar 2/03 - If r4after is being returned on a seek, we must save this value for later
         int saveRc = rc ;
         if ( rc == r4after && ( mode == BATCH4SEEKNEXT || mode == BATCH4SEEK || mode == BATCH4SEEKMATCH ) )
         {
            rc = 0 ;
         }

         rc = d4goVirtual2( data, recNo, rc, recordLocked, data->batchRead.recordExtra, 0, 0 ) ;
         if ( rc < 0 )
            return rc ;

         if ( saveRc == r4after && ( mode == BATCH4SEEKNEXT || mode == BATCH4SEEK || mode == BATCH4SEEKMATCH ) )
            rc = saveRc ;

         // and do memos
         if ( includeMemos == 1 )
         {
            int numMemos = data->dataFile->nFieldsMemo ;
            for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
            {
               FIELD4 *field = data->fieldsMemo[memoLoop].field ;
               long memoLen = connect4receiveLong( connect ) ;
               int rc2 = f4memoReadSet( field, memoLen, 0 ) ;
               if ( rc2 != 0 )
                  return rc2 ;
               if ( memoLen != 0 )
               {
                  rc2 = connect4receive( connect, field->memo->contents, memoLen, code4timeoutVal( c4 ) ) ;
                  if ( rc2 != 0 )
                     return rc2 ;
               }
            }
         }
         return rc ;
      }

      assert5( data->batchRead.readAdvanceBuf != 0 ) ;
      assert5( data->batchRead.readRecsToBuf > 1 ) ;
      data->batchRead.readBufNum = 0 ;
      data->batchRead.readBufPos = -1 ;
      char *pos = data->batchRead.readAdvanceBuf ;
      data->batchRead.readBufRecNoOn = 0 ;
      if ( nSkip < 0 )
         data->batchRead.readBufDirection = -1 ;
      else
         data->batchRead.readBufDirection = 1 ;
      saveRc = 0 ;

      int numMemos = data->dataFile->nFieldsMemo ;
      short firstRecordLocked = 0 ;

      for( ;; )  // handle the bufferring if required
      {
         if ( data->batchRead.readBufNum != 0 )  // non-first record, haven't read rc yet
         {
            rc = connect4receiveShort( connect ) ;
            if ( rc != 0 )  // means done (reached eof, error, bof, etc...)  - Jul 17/09 also r4done, but this is still an acceptable return
            {
               return saveRc ;
            }
         }

         long recNo = connect4receiveLong( connect ) ;

         // AS Aug 28/02 - now receive lock info as well (readLock will leave all records locked)
         short recordLocked = connect4receiveShort( connect ) ;
         if ( data->batchRead.readBufNum == 0 )  // first record, record first lock
            firstRecordLocked = recordLocked ;
         else if ( recordLocked == 1 )  // we do the first records lock later in the code
            d4localLockSet( data, recNo ) ;

         memcpy( pos, &recNo, sizeof( long ) ) ;
         pos += sizeof( long ) ;
         connect4receive( connect, pos, recWidth, code4timeoutVal( c4 ) ) ;
         pos += recWidth ;

         // now do the memo fields...
         if ( data->batchRead.doMemos == 1 )
         {
            for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
            {
               MEMO4BATCH_ENTRY *entry = &data->batchRead.memos[data->batchRead.readBufNum].memos[memoLoop] ;
               long memoLen = connect4receiveLong( connect ) ;
               if ( memoLen > 0 )
               {
                  if ( (long)entry->contentsAllocLen < memoLen )
                  {
                     if ( entry->contents != 0 )
                     {
                        u4free( entry->contents ) ;
                        entry->contents = 0 ;
                        entry->contentsAllocLen = 0 ;
                     }
                     entry->contents = (char *)u4allocFree( data->codeBase, memoLen ) ;
                     if ( entry->contents == 0 )
                        return e4memory ;
                     entry->contentsAllocLen = memoLen ;
                  }
                  connect4receive( connect, entry->contents, memoLen, code4timeoutVal( c4 ) ) ;
               }
               entry->contentsLen = memoLen ;
            }
         }

         data->batchRead.readBufNum++ ;

         if ( data->batchRead.readBufNum == 1 )   // means successfully read first record, go to it...
         {
            saveRc = d4goVirtual2( data, recNo, 0, firstRecordLocked, pos-recWidth, 0, 0 ) ;
            if ( data->batchRead.doMemos == 1 )
            {
               for ( int i = 0 ; i < data->dataFile->nFieldsMemo ; i++ )
               {
                  MEMO4BATCH_ENTRY *entry = &data->batchRead.memos[0].memos[i] ;
                  FIELD4 *field = data->fieldsMemo[i].field ;
                  f4memoReadSet( field, entry->contentsLen, entry->contents ) ;
                  field->memo->status = 0 ;
               }
            }

            data->batchRead.readBufRecNoOn = recNo ;
            data->batchRead.readBufPos = 0 ;
         }
         if ( data->batchRead.readBufNum == data->batchRead.readRecsToBuf ) // done
            return saveRc ;
      }

      return saveRc ;
   }



   // S4CLIENT
   static int d4skipFetch( DATA4 *data, short mode, const long nSkip )
   {
      // handles r4locked retrying as well
      CODE4 *c4 = data->codeBase ;

      c4->lockedLockItem = -2L ;
      c4->lockedFileName = 0 ;
      c4->lockedUserName = 0 ;
      c4->lockedNetName = 0 ;

      int rc = d4skipFetchLow( data, mode, nSkip, 0, 0, 0 ) ;

      if ( c4->lockAttempts == 1 || rc != r4locked ) // only bother calling once
         return rc ;

      #ifndef S4LOCK_HOOK
         // AS 02/27/01 - fix #255 - track num attempts only if NOT S4LOCK_HOOK
         int loop = c4->lockAttempts ;
      #endif
      int delayHundredths = c4->lockDelay ;

      #ifdef S4LOCK_HOOK
         for ( long count = 0 ; rc == r4locked ; count++ )
      #else
         while ( rc == r4locked && loop > 0 )
      #endif
      {
         #ifdef S4LOCK_HOOK
            rc = code4lockHook( c4, c4->lockedFileName, c4->lockedUserNameBuf, c4->lockedNetNameBuf, c4->lockedLockItem, count ) ;
            if ( rc != 0 )
               break ;
         #endif
         u4delayHundredth( delayHundredths ) ;
         #ifndef S4LOCK_HOOK
            // AS 02/27/01 - fix #255 - track num attempts only if NOT S4LOCK_HOOK
            if ( loop > 0 )
               loop-- ;
         #endif
         c4->lockedLockItem = -2L ;
         c4->lockedFileName = 0 ;
         c4->lockedUserName = 0 ;
         c4->lockedNetName = 0 ;

         rc = d4skipFetchLow( data, mode, nSkip, 0, 0, 0 ) ;
      }

      return rc ;
   }



   // S4CLIENT
   // AS Aug 30/02 - support for d4seekNext and multi-fetch skip - break out this part from skip
   int d4fetchFromBuffer( DATA4 *data, int newBufPos )
   {
      long size = dfile4recWidth( data->dataFile ) ;
      // position into buffer based on record size + sizeof( long ) for the record number, which is stored second
      char *pos = data->batchRead.readAdvanceBuf + newBufPos * ( size + sizeof( long ) ) ;
      long recNo = *((long *)pos) ;
      pos += sizeof( long ) ;
      memcpy( data->record, pos, size ) ;
      data->recNum = recNo ;
      data->batchRead.readBufRecNoOn = recNo ;
      // and do the memos if required
      #ifndef S4OFF_MEMO
         if ( data->batchRead.doMemos == 1 )
         {
            for ( int i = 0 ; i < data->dataFile->nFieldsMemo ; i++ )
            {
               MEMO4BATCH_ENTRY *entry = &data->batchRead.memos[newBufPos].memos[i] ;
               FIELD4 *field = data->fieldsMemo[i].field ;
               f4memoReadSet( field, entry->contentsLen, entry->contents ) ;
               field->memo->status = 0 ;
            }
         }
      #endif

      // AS Aug 28/02 - We need to update this info as well if locked, if only because
      // of the side effect of an old recNumOld being set to this record number and
      // if we don't update it it could be invalid.
      if ( d4lockTest( data, recNo ) )
      {
         memcpy( data->recordOld, data->record, dfile4recWidth( data->dataFile ) ) ;
         data->recNumOld = recNo ;
         #if !defined( S4OFF_MULTI ) && !defined( S4OFF_MEMO )
            data->memoValidated = 1 ;
         #endif
      }

      return 0 ;
   }



   // S4CLIENT
   int d4skipFetchMultiple( DATA4 *data, short mode, const long nSkip )
   {
      assert5port( "New function for advance-reading client/server" ) ;
      // first look into the buffer to see if it is there...
      if ( data->batchRead.readBufRecNoOn != data->recNum )   // see if we are positioned in buffer to current pos
      {
         // we were not positioned at correct spot to start, so reset the buffer
         d4readBufferReset( data, 0 ) ;
      }
      else
      {
         // check if our record is available in the buffer...
         // calculate the new 'buf position'
         int newBufPos = data->batchRead.readBufPos ;
         if ( data->batchRead.readBufDirection == 1 )  // means the buffer is forwards
            newBufPos += nSkip ;
         else  // means the buffer is backwards
            newBufPos -= nSkip ;

         if ( newBufPos < 0 || newBufPos > data->batchRead.readBufNum )  // means we have gone outside the buffer
            d4readBufferReset( data, 0 ) ;
         else  // we are ok, just read from the buffer
         {
            data->batchRead.readBufPos = newBufPos ;
            if ( data->batchRead.readBufPos == data->batchRead.readBufNum )  // means we have read last record in buffer
            {
               // we save the rc code for the last to-fetch row in the buffer.  return it now.
               // if ( data->readBufRcSave != 0 )
               //    return data->readBufRcSave ;
               // if here, the rc was 0, and we reached end of buffer, just normal skip...
               d4readBufferReset( data, 0 ) ;
               return d4skipFetch( data, mode, nSkip ) ;
            }

            return d4fetchFromBuffer( data, newBufPos ) ;
         }
      }

      return d4skipFetch( data, mode, nSkip ) ;
   }
#endif /* S4CLIENT */


// AS Feb 24/09 - support for d4skipCache()
// CS Mar 3/09 Define in CS and SA
// AS Jul 17/09 - modus functionality added
// S4CLIENT
int S4FUNCTION d4skipCache( DATA4 *data, const long nCache, long modus )
{
   // this uses the cache for skipping records from the server
   // functions that reposition the current DATA4 pointer effectively reset the cache
   // a cross between d4skipFetchMultiple and tfile4skipCache
   // input is the # of rows to store in the cache
   // skips 1 record forward from the current position and retrieves the record
   // it will retrieve from the cache if it can
   // otherwise it will read from the server and cache the next nCache records
   // modus...if > 0, then nCache is the number of rows to cache and modus is the number of that numRowsin is doubled when caching continues.
   // modus<= -1 then nCache is the maximum number of rows to cache and abs(modus) is the number of characters that have to match at the beginning of the key.

   #ifdef S4CLIENT
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94802 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E94802 ) ;
         if ( nCache == 0 )
            return error4( 0, e4parm, E94802 ) ;
      #endif

      CODE4 *c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;

      if ( data->recNum < 1L )
      {
         #ifndef S4SERVER
            if ( c4->errSkip )
               error4( c4, e4info, E84801 ) ;
         #endif
         return e4info ;
      }

      if ( nCache == 1 || nCache == -1 )  // in this case there is no point in cacheing
         return d4skip( data, nCache ) ;

      short skipPos = 1 ;
      int rc ;
      if ( nCache < 1 )
         skipPos = -1 ;
      data->batchRead.modusStart = modus ;

      if ( modus > 0 )
      {
         if ( data->batchRead.modusStart != modus || nCache != data->batchRead.nCacheIn )  // reset
         {
            data->batchRead.currentDbl = modus ;
            data->batchRead.nCacheIn = nCache ;
            data->batchRead.readRecsToBufNext = nCache * skipPos ;
         }
         else if ( data->batchRead.currentDbl > 0 )
         {
            if ( (data->batchRead.readBufPos == 0 && skipPos < 0 ) || (data->batchRead.readBufPos == (data->batchRead.readBufNum-1) && skipPos > 0 ) )
            {
               data->batchRead.currentDbl-- ;
               data->batchRead.readRecsToBufNext = data->batchRead.readRecsToBufNext * 2  ;
            }
         }
      }
      if ( data->batchRead.readRecsToBuf == 0 )
      {
         rc = d4readBuffer( data, nCache * skipPos, 0 ) ;
         if ( rc < 0 )
            return rc ;
      }
      else if ( modus <= 0 )
         data->batchRead.readRecsToBufNext = nCache * skipPos ;
      return d4skipFetchMultiple( data, 0, skipPos ) ;
   #else
      return e4notSupported;
   #endif
}



int S4FUNCTION d4skip( DATA4 *data, const long nSkip )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E94802 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E94802 ) ;
   #endif

   CODE4 *c4 = data->codeBase ;
   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;

   if ( data->recNum < 1L )
   {
      #ifndef S4SERVER
         if ( c4->errSkip )
            error4( c4, e4info, E84801 ) ;
      #endif
      return e4info ;
   }

   int rc = 0 ;

   #ifdef S4CLIENT
      rc = d4updateRecord( data, 0, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc )
         return rc ;

      /* AS Apr 10/02 - New functionality for advance-reading client/server (reading a block) */
      assert5port( "New functionality for advance-reading client/server" ) ;
      // AS Dec 17/02 updated for config flags
      if ( data->batchRead.readRecsToBuf != 0  && ( data->readBatchMode & r4batchSkip ) )
         return d4skipFetchMultiple( data, BATCH4SKIP, nSkip ) ;
      else
         return d4skipFetch( data, BATCH4SKIP, nSkip ) ;
   #else
      #ifndef S4SERVER
         int saveFlag ;
      #endif
      long startRec, newRec ;
      int oldEofFlag, c1 ;
      long n = nSkip ;

      #ifndef S4OFF_INDEX
         TAG4 *tag = data->tagSelected ;
         if ( tag == 0 )
         {
      #endif
         data->bofFlag = 0 ;
         startRec = data->recNum ;
         newRec = startRec + n ;
         if ( newRec > 0L )
         {
            #ifndef S4SERVER
               saveFlag = c4->errGo ;
               c4->errGo = 0 ;
            #endif
            #ifndef S4OFF_OPTIMIZE
               data->dataFile->hiPrio = -1 ;  /* indicate d4skip - data level */
            #endif
            rc = d4go( data, newRec ) ;
            #ifndef S4OFF_OPTIMIZE
               data->dataFile->hiPrio = 0 ;
            #endif
            #ifndef S4SERVER
               c4->errGo = saveFlag ;
            #endif
            if ( rc >= 0 && rc != r4entry )
               return rc ;
         }

         c1 = d4recCountLessEq( data, 1L ) ;
         if ( c1 < 0 )
            return c1 ;
         if ( ( c1 == 0 ) || ( d4recCountLessEq( data, newRec ) == 0 ) )
         {
            if ( c1 == 0L )
            {
               rc = d4goEof( data ) ;
               if ( rc != r4eof )
                  return rc ;
               data->bofFlag = 1 ;
            }
            if ( n < 0 )
            {
               data->bofFlag = 1 ;
               return r4bof ;
            }
            else
               return d4goEof( data ) ;
         }

         if ( newRec < 1L )
         {
            oldEofFlag = data->eofFlag ;
            rc = d4go( data, 1L ) ;
            if ( rc )
               return rc ;
            data->bofFlag = 1 ;
            data->eofFlag = oldEofFlag ;
            return r4bof ;
         }

         return d4go( data, newRec ) ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            TAG4FILE *tagFile = tag->tagFile ;
            if ( data->eofFlag )
            {
               if ( n >= 0 )
                  return d4goEof( data ) ;

               rc = d4bottom( data ) ;
               if ( rc && rc != r4eof )
                  return rc ;
               if ( rc == r4eof )
               {
                  rc = d4goEof( data ) ;
                  if ( rc != r4eof )
                     return rc ;
                  return r4bof ;
               }
               n++ ;
               data->recNum = (long)tfile4recNo( tagFile ) ;
            }

            data->bofFlag = 0 ;

            #ifndef S4OFF_WRITE
               if ( data->recordChanged )
               {
                  /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
                  rc = d4updateRecord( data, 0, 1 ) ;
                  if ( rc < 0 )
                     return rc ;
               }
            #endif

            t4versionCheck( tag, 1, 0 ) ;

            if ( n == 0 )
               return 0 ;

            if ( (long)tfile4recNo( tagFile ) != data->recNum )
            {
               rc = d4go( data, data->recNum ) ;
               if ( rc )
                  return rc ;

               expr4context( tagFile->expr, data ) ;
               unsigned char *keyValue ;
               tfile4exprKey( tagFile, &keyValue ) ;

               rc = tfile4go( tagFile, keyValue, data->recNum, 0 ) ;
               if ( rc < 0 )
                  return rc ;

               #ifdef S4HAS_DESCENDING
                  if ( tagFile->header.descending )
                  {
                     if ( (rc > 0) && (n < 0) )
                        n-- ;
                  }
                  else
                     if ( (rc > 0) && (n > 0) )
                        n-- ;
               #else
                  if ( (rc > 0) && (n > 0) )
                     n-- ;
               #endif
            }

            long nSkipped = tfile4dskip( tagFile, n ) ;
            if ( n > 0  &&  nSkipped != n )
               return d4goEof( data ) ;

            if ( tfile4eof( tagFile ) )
            {
               data->bofFlag = 1 ;
               return d4goEof( data ) ;
            }

            /* AS 03/15/99 -- it is possible that the key found may not be in the data file because of update conflicts.  For example
               added to tag but not added to datafile yet.  Or added by another user in transaction so shouldn't go to recor yet.
               modified to skip forward in that case.  */
            int skipDirection ;
            if ( n < 0 )
               skipDirection = -1 ;
            else
               skipDirection = 1 ;

            long recno ;
            for( ;; )
            {
               recno = (long)tfile4recNo( tagFile ) ;
               if ( recno < 0 )
                  return (int)recno ;
               if ( d4recCountLessEq( data, (long)tfile4recNo( tagFile ) ) != 0 )  /* valid spot */
                  break ;
               if ( tfile4dskip( tagFile, skipDirection ) == 0 ) // didn't skip
               {
                  data->bofFlag = 1 ;
                  return d4goEof( data ) ;
               }
            }
            rc = d4go( data, recno ) ;
            if ( rc )
               return rc ;

            #if !defined(S4OFF_MULTI) && !defined(S4OFF_TRAN)
               /* if transactions are enabled, we do not remove keys from an index file when
                  records are changed to avoid unique problems caused by another user.
                  The result of this is we may need to re-sync ourselves in that case - namely,
                  we do not want to position to a record indicated by a 'removed' entry in
                  a tag.
               */
               // AS Apr 29/03 - transcations are run-time in odbc now
               // AS Jun 20/03 - was checking wrong flag
               #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
                  if ( c4->server->odbcTrans == 1 )  // odbc build, no trans available
               #endif
                     if ( code4transEnabled( c4 ) && t4unique( tag ) != 0 )
                     {
                        #ifdef S4SERVER
                           // AS Apr 15/03 - support for new lockId for shared clone locking
                           if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4write ) != 1 )
                           if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4read ) != 1 )
                        #else
                           if ( d4lockTestFile( data ) != 1 )
                        #endif
                        {
                           rc = d4tagSyncDo( data, tag, n > 0 ? 1 : -1 ) ;
                           if ( rc != 0 )
                              return rc ;
                        }
                     }
            #endif

            if ( n == nSkipped )
               return 0 ;

            #ifdef S4HAS_DESCENDING
               if ( ( n < 0 && !tagFile->header.descending ) ||  ( n < 0 && tagFile->header.descending ) )
            #else
               if ( n < 0 )
            #endif
            {
               data->bofFlag = 1 ;
               return r4bof ;
            }
         }
         return 0 ;
      #endif
   #endif
}
