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

/* d4bottom.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

#ifdef S4CLIENT
   int S4FUNCTION d4bottom( DATA4 *data )
   {
      #ifndef S4INDEX_OFF
         TAG4 *tag ;
      #endif
      int rc ;
      CONNECTION4 *connection ;
      CONNECTION4BOTTOM_INFO_IN *info ;
      CONNECTION4BOTTOM_INFO_OUT *out ;
      CODE4 *c4 ;

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E91201 ) ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E91201 ) )
            return -1 ;
      #endif

      c4 = data->codeBase ;

      #ifdef S4OFF_WRITE
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;
      #else
         rc = d4updateRecord( data, 0, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc )
            return rc ;
      #endif

      /* AS Apr 23/02 - New functionality for advance-reading client/server (reading a block) */
      /* basically, we just perform a skip with the start pos marked as 'top' - i.e. recno == -1 */
      // AS Dec 17/02 updated for config flags
      if ( data->batchRead.readRecsToBuf != 0 && ( data->readBatchMode & r4batchBottom ) )
      {
         data->recNum = -2 ;   // use to indicate d4bottom to server
         return d4skipFetchMultiple( data, BATCH4BOTTOM, -1 ) ;
      }

      connection = data->dataFile->connection ;
      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( c4, e4connection, E91201 ) ;
      #endif

      connection4assign( connection, CON4BOTTOM, data4clientId( data ), data4serverId( data ) ) ;
      connection4addData( connection, NULL, sizeof( CONNECTION4BOTTOM_INFO_IN ), (void **)&info ) ;
      #ifdef S4INDEX_OFF
         info->usesTag = 0 ;
      #else
         tag = data->tagSelected ;
         if ( tag == 0 )
            info->usesTag = 0 ;
         else
         {
            // AS Oct 25/05 - support for low level tag operations
            tag->tagFile->tagDataValid = 0 ;  // reset to invalid
            info->usesTag = 1 ;
            memcpy( info->tagName, tag->tagFile->alias, LEN4TAG_ALIAS  ) ;
            info->tagName[LEN4TAG_ALIAS] = 0 ;
            // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
            u4ncpy( info->indexFileName, tag->tagFile->indexFile->accessName, strlen( tag->tagFile->indexFile->accessName ) + 1 ) ;
         }
      #endif

      // AS May 21/02 - code to support auto-transfer of memo fields
      info->includeMemos = data->includeMemos ;
      rc = connection4repeat( connection ) ;
      data->recNum = -1 ;
      switch( rc )
      {
         case r4eof:
            data->bofFlag = 1 ;
            return d4goEof( data ) ;
         case r4success:
            break ;
         default:
            if ( rc < 0 )
               connection4error( connection, c4, rc, E91201 ) ;
            return rc ;
      }

      if ( connection4len( connection ) < (long)sizeof( CONNECTION4BOTTOM_INFO_OUT ) + (long)dfile4recWidth( data->dataFile ) )
         return error4( c4, e4packetLen, E91201 ) ;

      out = (CONNECTION4BOTTOM_INFO_OUT *)connection4data( connection ) ;

      /* now copy the data into the record */
      // AS May 21/02 - code to support auto-transfer of memo fields
      char *recPtr = ((char *)out) + sizeof( CONNECTION4BOTTOM_INFO_OUT ) ;
      memcpy( data->record, recPtr, dfile4recWidth( data->dataFile ) ) ;
      if ( data->includeMemos == 1 )
      {
         char *memosPtr = recPtr + dfile4recWidth( data->dataFile ) ;
         d4goVirtualDoMemos( data, memosPtr ) ;
      }

      data->bofFlag = out->bofFlag ;
      data->eofFlag = out->eofFlag ;

      data->recNum = ntohl5(out->recNo) ;
      if ( out->recordLocked )
      {
         d4localLockSet( data, data->recNum ) ;
         memcpy( data->recordOld, data->record, dfile4recWidth( data->dataFile ) ) ;
         data->recNumOld = data->recNum ;
         data->memoValidated = 1 ;
      }

      return 0 ;
   }
#else



   int S4FUNCTION d4bottom( DATA4 *data )
   {
      long rec ;
      CODE4 *c4 ;
      #ifndef S4INDEX_OFF
         TAG4 *tag ;
         long recno ;
         int rc ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E91201 ) )
            return -1;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E91201 ) ;
      #endif

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifndef S4INDEX_OFF
         tag = data->tagSelected ;

         if ( tag == 0 )
         {
      #endif
         rec = d4recCount( data ) ;  /* updates the record, returns -1 if error4code( codeBase ) < 0 */
         if ( rec > 0L )
            return d4go( data, rec ) ;
         if ( rec < 0 )
            return (int)rec ;
      #ifndef S4INDEX_OFF
         }
         else
         {
            TAG4FILE *tagFile = tag->tagFile ;
            #ifndef S4OFF_WRITE
               /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
               rc = d4updateRecord( data, 0, 1 ) ;
               if ( rc )
                  return error4stack( data->codeBase, (short)rc, E91201 ) ;
            #endif
            t4versionCheck( tag, 0, 0 ) ;
            rc = tfile4bottom( tagFile ) ;
            if ( rc )
               return error4stack( data->codeBase, (short)rc, E91201 ) ;
            if ( !tfile4eof( tagFile ) )
            {
               // coerce recno to long, current tag supports 'unsigned long' recno's, but DATA4 level
               // only supports long recnos...
               recno = (long)tfile4recNo( tagFile ) ;
               if ( recno < 0 )
                  return -1 ;
               #ifndef S4SINGLE
                  /* AS 03/15/99 - may be more than 1 record extra in file, changed from if to while and allowed to continue*/
                  /* AS 03/25/99 should  use data->count, not minCount which doesn't take into effect that we
                     may be the user performing the transaction
                     while ( recno > data->dataFile->minCount )
                  */
                  /* AS 03/31/99 --> do same as top, call recCountLessEq which handles the min values
                     properly
                  */

                  while ( d4recCountGreater( data, recno ) ) // if ( recno > data->count )   /* ensure correct sequencing for multi-user */
                  {
                     if ( recno > d4recCount( data ) )
                     {
                        rc = (int)tfile4skip( tagFile, -1L ) ;
                        // AS July 16/02 - incrrectly returning -1 in this case.
                        if ( rc == 1 )  // tfile4skip returns -numskip, in our case 1 in case of error
                           return -1 ;
                        if ( rc == 0 )
                        {
                           data->bofFlag = 1 ;
                           return d4goEof( data ) ;
                        }
                        // coerce recno to long, current tag supports 'unsigned long' recno's, but DATA4 level
                        // only supports long recnos...
                        recno = (long)tfile4recNo( tagFile ) ;
                        if ( recno <= 0 )
                           return error4stack( data->codeBase, -1, E91201 ) ;
                     }
                  }
               #endif
               rc = d4go( data, recno ) ;
               #if !defined( S4OFF_MULTI ) && !defined( S4OFF_TRAN )
                  /* if transactions are enabled, we do not remove keys from an index file when
                     records are changed to avoid unique problems caused by another user.
                     The result of this is we may need to re-sync ourselves in that case - namely,
                     we do not want to position to a record indicated by a 'removed' entry in
                     a tag.
                  */
                  if ( rc == 0 )
                  {
                     // AS Apr 29/03 - transcations are run-time in odbc now
                     // AS May 20/03 - had the #endif in wrong spot...was losing code in non-odbc
                     #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
                        if ( c4->server->odbcTrans )  // odbc build, no trans available
                     #endif
                        {
                           if ( code4transEnabled( c4 ) )
                           {
                              if ( t4unique( tag ) != 0 ) // || tagFile->filter != 0 )  /* AS 03/25/99 added also to test if filter condition exists... */
                              {
                                 #ifdef S4SERVER
                                    // AS Apr 15/03 - support for new lockId for shared clone locking
                                    if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4write ) != 1 )
                                       if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4read ) != 1 )
                                 #else
                                    if ( d4lockTestFile( data ) != 1 )
                                 #endif
                                    rc = d4tagSyncDo( data, tag, -1 ) ;
                              }
                           }
                        }
                  }
               #endif
               return rc ;
            }
         }
      #endif

      data->bofFlag = 1 ;
      return d4goEof( data ) ;
   }
#endif /* S4CLIENT else */

