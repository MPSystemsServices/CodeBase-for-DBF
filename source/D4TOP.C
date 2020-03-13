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

/* d4top.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

#ifdef S4CLIENT
   int S4FUNCTION d4top( DATA4 *data )
   {
      #ifndef S4OFF_INDEX
         TAG4 *tag ;
      #endif
      int rc ;
      CONNECTION4 *connection ;
      CONNECTION4TOP_INFO_IN *info ;
      CONNECTION4TOP_INFO_OUT *out ;
      CODE4 *c4 ;

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92301 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92301 ) ;
      #endif

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifndef S4OFF_WRITE
         rc = d4updateRecord( data, 0, 1 ) ;
         if ( rc )
            return rc ;
      #endif

      connection = data->dataFile->connection ;
      if ( connection == 0 )
         return e4connection ;

      data->recNum = -1 ;

      /* AS Apr 23/02 - New functionality for advance-reading client/server (reading a block) */
      /* basically, we just perform a skip with the start pos marked as 'top' - i.e. recno == -1 */
      assert5port( "New Functionality for batch reading client/server" ) ;
      // AS Dec 17/02 updated for config flags
      if ( data->batchRead.readRecsToBuf != 0 && ( data->readBatchMode & r4batchTop ) )
         return d4skipFetchMultiple( data, BATCH4TOP, 0 ) ;

      connection4assign( connection, CON4TOP, data4clientId( data ), data4serverId( data ) ) ;
      connection4addData( connection, NULL, sizeof( CONNECTION4TOP_INFO_IN ), (void **)&info ) ;
      #ifdef S4OFF_INDEX
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
      assert5port( "New Functionality to optionally include memos on communicatinos retrieval" ) ;
      info->includeMemos = data->includeMemos ;
      rc = connection4repeat( connection ) ;
      switch( rc )
      {
         case r4eof:
            data->bofFlag = 1 ;
            return d4goEof( data ) ;
         case r4success:
            break ;
         default:
            if ( rc < 0 )
               connection4error( connection, c4, rc, E92301 ) ;
            return rc ;
      }

      if ( connection4len( connection ) < (long)sizeof( CONNECTION4TOP_INFO_OUT ) + (long)dfile4recWidth( data->dataFile ) )
         return error4( c4, e4packetLen, E92301 ) ;

      out = (CONNECTION4TOP_INFO_OUT *)connection4data( connection ) ;

      /* now copy the data into the record */
      // AS May 21/02 - code to support auto-transfer of memo fields
      assert5port( "New Functionality to optionally include memos on communicatinos retrieval" ) ;
      char *recPtr = ((char *)out) + sizeof( CONNECTION4TOP_INFO_OUT ) ;
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
#endif


#ifndef S4CLIENT
   int S4FUNCTION d4top( DATA4 *data )
   {
      int rc ;
      CODE4 *c4 ;
      #ifndef S4OFF_INDEX
         long recno ;
         TAG4 *tag ;
         TAG4FILE *tagFile ;
      #endif
      #ifndef S4SERVER
         int saveFlag ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92301 ) )
            return -1;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92301 ) ;
      #endif

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return -1 ;

      #ifndef S4OFF_INDEX
         tag = data->tagSelected ;

         if ( tag == 0 )
         {
      #endif
         if ( d4recCount( data ) > 0L )
         {
            #ifndef S4SERVER
               saveFlag = c4->errGo ;
               c4->errGo = 0 ;
            #endif
            rc = d4go( data, 1L ) ;
            #ifndef S4SERVER
               c4->errGo = saveFlag ;
            #endif
            return rc ;
         }
      #ifndef S4OFF_INDEX
         }
         else
         {
            tagFile = tag->tagFile ;
            #ifndef S4OFF_WRITE
               /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
               rc = d4updateRecord( data, 0, 1 ) ;
               if ( rc )
                  return rc ;
            #endif
            t4versionCheck( tag, 0, 0 ) ;
            rc = tfile4top( tagFile ) ;
            if ( rc )
               return rc ;
            while ( !tfile4eof( tagFile ) )
            {
               recno = tfile4recNo( tagFile ) ;
               if ( recno < 0 )
                  return (int)recno ;
               /* AS 03/15/99 -- it is possible that the key found may not be in the data file because of update conflicts.  For example
                  added to tag but not added to datafile yet.  Or added by another user in transaction so shouldn't go to recor yet.
                  modified to skip forward in that case.  */

               if ( d4recCountLessEq( data, tfile4recNo( tagFile ) ) != 0 )  /* valid spot */
               {
                  rc = d4go( data, recno ) ;
                  #ifndef S4OFF_MULTI
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
                              if ( rc == 0 && code4transEnabled( c4 ) && t4unique( tag ) != 0 )
                              {
                                 #ifdef S4SERVER
                                    // AS Apr 15/03 - support for new lockId for shared clone locking
                                    if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4write ) != 1 )
                                    if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4read ) != 1 )
                                 #else
                                    if ( d4lockTestFile( data ) != 1 )
                                 #endif
                                    rc = d4tagSyncDo( data, tag, 1 ) ;
                              }
                     #endif
                  #endif
                  return rc ;
               }
               else
               {
                  /* try next record */
                  if ( tfile4skip( tagFile, 1L ) == 0 )  /* eof */
                     break ;
                  if ( error4code( data->codeBase ) < 0 )
                     return -1 ;
               }
            }
         }
      #endif

      data->bofFlag = 1 ;
      return d4goEof( data ) ;
   }
#endif /* S4CLIENT */



int S4FUNCTION d4goBof( DATA4 *data )
{
   int rc ;

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E93104 ) ;
   #endif

   if ( error4code( data->codeBase ) < 0 )
      return e4codeBase ;

   #ifndef S4OFF_WRITE
      /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
      rc = d4updateRecord( data, 0, 1 ) ;
      if ( rc )
         return rc ;
   #endif

   rc = d4top( data ) ;
   data->bofFlag = 1 ;
   if ( rc < 0 )
      return rc ;

   return r4bof ;
}
