/* d4write.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TURBOC__ */
#endif  /* S4UNIX */

#if !defined( S4OFF_WRITE ) && defined( S4CLIENT )
   static int d4writeBatch( DATA4 *data, long serverId, long clientId )
   {
      // add to buffer...
      DATA4FILE *d4file = data->dataFile ;
      DATA4BATCH_WRITE *batch = &data->batchWrite ;

      if ( batch->curWriteBufCount == batch->writeRecsToBuf ) // flush the buffer
      {
         int rc = d4writeBufferDo( data ) ;
         if ( rc != 0 )
            return rc ;
         assert5( batch->curWriteBufCount == 0 ) ;
      }

      long recWidth = dfile4recWidth( d4file ) ;
      char *buf = batch->writeDelayBuf + (recWidth + sizeof(long) ) * batch->curWriteBufCount ;
      *((long *)buf) = d4recNo( data ) ;  // indicate we want write/update
      buf += sizeof( long ) ;
      memcpy( buf, d4record( data ), recWidth ) ;
      for ( int loop = 0 ; loop < data->dataFile->nFieldsMemo ; loop++ )
      {
         MEMO4BATCH_ENTRY *entry = &batch->memos[batch->curWriteBufCount].memos[loop] ;
         F4MEMO *memoField = &data->fieldsMemo[loop] ;
         entry->contentsLen = memoField->len ;
         if ( memoField->isChanged == 0 )
         {
            entry->contentsLen = -1 ;  // indicate unchanged
         }
         else if ( memoField->len != 0 )
         {
            if ( entry->contentsAllocLen < memoField->len )
            {
               if ( entry->contents != 0 )
               {
                  u4free( entry->contents ) ;
                  entry->contents = 0 ;
                  entry->contentsAllocLen = 0 ;
               }
               entry->contents = (char *)u4allocFree( data->codeBase, memoField->len ) ;
               if ( entry->contents == 0 )
                  return e4memory ;
               entry->contentsAllocLen = memoField->len ;
            }
            memcpy( entry->contents, memoField->contents, memoField->len ) ;
            memoField->isChanged = 0 ;
         }
      }


      #ifdef E4DEBUG
         assert5( d4lockTest( data, data->recNum ) == 1 ) ;   // must be locked for batch processing
      #endif

      batch->curWriteBufCount++ ;

      data->recordChanged = 0 ;

      return 0 ;
   }



   int S4FUNCTION d4writeLow( DATA4 *d4, const long recIn, const int unlock, const int dolock )
   {
      int rc ;
      CONNECTION4 *connection ;
      CONNECTION4WRITE_INFO_IN *info ;
      CONNECTION4WRITE_INFO_OUT *out ;
      long rec ;
      #ifndef S4OFF_MEMO
         CONNECTION4MEMO *memo ;
         F4MEMO *mfield ;
         short i ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( d4, 2, E92601 ) )
            return 0 ;
      #endif  /* E4VBASIC */

      if ( recIn == -1 )
         rec = d4recNo( d4 ) ;
      else
         rec = recIn ;

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E92601 ) ;
         if ( rec == 0 || rec < -1 || d4->codeBase == 0 )
            return error4( d4->codeBase, e4parm, E92601 ) ;
         #ifdef E4ANALYZE
            if ( d4->dataFile == 0 )
               return error4( d4->codeBase, e4parm, E92601 ) ;
         #endif
      #endif

      CODE4 *c4 = d4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifdef E4MISC
         if ( d4->record[0] != ' ' && d4->record[0] != '*' )
            return error4( c4, e4info, E83301 ) ;
      #endif

      if ( d4->readOnly == 1 )
         return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;

      /* ensure the record is being written to a valid position */
      // AS Dec 17/02 - for efficiency avoid calling recCount if possible... use d4recCountGreater in place
      if ( d4recCountGreater( d4, rec - ( d4lockTestAppend( d4 ) == 1 ) ) )
      {
         // AS 06/08/01 - if CodeBase already in error state, don't change the error code
         if ( error4code( c4 ) < 0 )
            return error4code( c4 ) ;
         return error4describe( c4, e4write, E82601, d4alias( d4 ), 0, 0 ) ;
      }

      long clientId = data4clientId( d4 ) ;
      long serverId = data4serverId( d4 ) ;

      // AS Jul 13/02 new functionality for writeBatch
      assert5port( "new functionality for writeBatch" ) ;
      // AS Dec Only need to verify that record is locked, not entire file
      if ( d4->batchWrite.writeRecsToBuf > 0 && dfile4lockTest( d4->dataFile, clientId, serverId, rec, lock4write ) == 1 )  // buffer writes
         return d4writeBatch( d4, serverId, clientId ) ;

      #ifdef E4ANALYZE
         if ( d4->dataFile->connection == 0 )
           return error4( d4->codeBase, e4parm, E92601 ) ;
      #endif

      connection = d4->dataFile->connection ;
      connection4assign( connection, CON4WRITE, clientId, serverId ) ;
      connection4addData( connection, NULL, sizeof( CONNECTION4WRITE_INFO_IN ), (void **)&info ) ;
      #ifndef S4OFF_MEMO
         for ( i = 0 ; i < d4->dataFile->nFieldsMemo ; i++ )
         {
            mfield = d4->fieldsMemo + i ;
            if ( mfield->isChanged == 1 )
               info->numMemoFields++ ;
         }
      #endif
      info->numMemoFields = htons5(info->numMemoFields) ;
      info->recNo = htonl5(rec) ;
      info->unlock = unlock ;
      connection4addData( connection, d4->record, dfile4recWidth( d4->dataFile ), NULL ) ;
      #ifndef S4OFF_MEMO
         for ( i = 0 ; i < d4->dataFile->nFieldsMemo ; i++ )
         {
            mfield = d4->fieldsMemo + i ;
            if ( mfield->isChanged == 1 )
            {
               connection4addData( connection, NULL, sizeof( CONNECTION4MEMO ), (void **)&memo ) ;
               memo->fieldNum = htons5(i) ;
               memo->memoLen = htonl5(mfield->len) ;
               if ( mfield->len > 0 )
                  connection4addData( connection, mfield->contents, mfield->len, NULL ) ;
            }
         }
      #endif
      rc = connection4repeat( connection ) ;
      if ( rc == r4locked )
         return rc ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E92601 ) ;
      if ( connection4len( connection ) != sizeof( CONNECTION4WRITE_INFO_OUT ) )
         return error4( c4, e4packetLen, E92601 ) ;
      out = (CONNECTION4WRITE_INFO_OUT *)connection4data( connection ) ;
      if ( out->recordLocked )
         d4localLockSet( d4, d4recNo( d4 ) ) ;

      if ( rc > 0 )  /* eg. r4entry or r4locked */
         return rc ;
      #ifndef S4OFF_MEMO
         for ( i = 0 ; i < d4->dataFile->nFieldsMemo ; i++ )
         {
            mfield = d4->fieldsMemo + i ;
            mfield->isChanged = 0 ;
         }
      #endif
      d4->recordChanged = 0 ;
      return 0 ;
   }
#endif /* !defined( S4OFF_WRITE ) && defined( S4CLIENT ) */


#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   static int d4unwriteKeys( DATA4 *, const long ) ;
#endif


#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   int S4FUNCTION d4writeLow( DATA4 *d4, const long recIn, const int unlock, const int doLock )
   {
      /* AS 09/02/98 --> for OLE-DB, we handle locks outside scope, so pass 0 to doLock */
      #ifndef S4OFF_TRAN
         unsigned long len ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( d4, 2, E92601 ) )
            return 0 ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E92601 ) ;
         if ( recIn < -1 || recIn == 0 || d4->codeBase == 0 )
            return error4( d4->codeBase, e4parm, E92601 ) ;
      #endif

      #ifdef E4MISC
         if ( d4->record[0] != ' ' && d4->record[0] != '*' )
            return error4( d4->codeBase, e4info, E83301 ) ;
      #endif
      long rec ;
      if ( recIn == -1 )
         rec = d4recNo( d4 ) ;
      else
         rec = recIn ;

      CODE4 *c4 = d4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( d4->readOnly == 1 )
         return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;

      int old = d4->recordChanged ;
      d4->recordChanged = 0 ;

      /* set lock before transaction handling since cannot otherwise rollback */
      int rc = 0 ;  // AS Jan 9/03 - Ensure initialized to zero due to changes below
      #ifndef S4OFF_MULTI
         if ( doLock )
         {
            #ifdef S4SERVER
               rc = d4lockInternal( d4, rec, 0 ) ;
            #else
               rc = d4lockInternal( d4, rec, 1 ) ;
            #endif
            if ( rc )
            {
               d4->recordChanged = old ;
               return rc ;
            }
         }
      #endif  /* S4OFF_MULTI */

      /* ensure the record is being written to a valid position */
      if ( d4bof( d4 ) || d4eof( d4 ) )
      {
         if ( d4recCountGreater( d4, rec - 1 ) )
         #ifndef S4OFF_MULTI
            /* AS 09/02/98 - is more efficient to only check d4lockTestAppend if the
               rec # is indeed greater than (rec - 1) for efficiency reasons, since this
               function gets called often on updates, thus the dual if statments on d4recCountGreater */
            if ( d4lockTestAppend( d4 ) == 0 )
               if ( d4recCountGreater( d4, rec ) )
         #endif
         d4->recordChanged = old ;
         return error4( c4, e4write, E82601 ) ;
      }

      #ifndef S4OFF_TRAN
         int hasTran = d4startMiniTransactionIfRequired( d4 ) ;
         if ( hasTran < 0 )
         {
            d4->recordChanged = old ;
            return hasTran ;
         }
      #endif
      /* 0. Validate memo id's */
      /* 1. Update Keys */
      /* 2. Update Memo Information */
      /* 3. Update Data FILE4 */

      #if !defined( S4OFF_MEMO ) && !defined( S4OFF_MULTI )
         if ( d4->dataFile->nFieldsMemo > 0 )
            if ( ( rc = d4validateMemoIds( d4 ) ) != 0 )
            {
               d4->recordChanged = old ;
               #ifndef S4OFF_TRAN
                  if ( hasTran )
                     code4tranRollbackSingle( c4 ) ;
               #endif
               return rc ;
            }
      #endif

      #ifndef S4OFF_TRAN
         TRAN4 *trans = code4trans( c4 ) ;
         S4LONG connectionId = 0L ;
         if ( d4transEnabled( d4, 1 ) )
         {
            #ifndef S4STAND_ALONE
               connectionId = c4->currentClient->id ;
            #endif
            S4LONG recNo = d4recNo( d4 ) ;
            rc = tran4set( trans, trans->currentTranStatus, -1L, connectionId, TRAN4WRITE,
                 sizeof( recNo ) + 2 * dfile4recWidth( d4->dataFile ), data4clientId( d4 ), data4serverId( d4 ) ) ;
            if ( rc < 0 )
            {
               if ( hasTran )
                  code4tranRollbackSingle( c4 ) ;
               d4->recordChanged = old ;
               return 0 ;
            }
            if ( tran4putData( trans, &recNo, sizeof( recNo ) ) == e4memory )
            {
               if ( hasTran )
                  code4tranRollbackSingle( c4 ) ;
               d4->recordChanged = old ;
               return 0 ;
            }
            if ( d4readOld( d4, rec ) < 0 )
            {
               d4->recordChanged = old ;
               return -1 ;
            }
            if ( tran4putData( trans, d4->recordOld, dfile4recWidth( d4->dataFile ) ) == e4memory )
            {
               if ( hasTran )
                  code4tranRollbackSingle( c4 ) ;
               d4->recordChanged = old ;
               return 0 ;
            }
            if ( tran4putData( trans, d4record( d4 ), dfile4recWidth( d4->dataFile ) ) == e4memory )
            {
               if ( hasTran )
                  code4tranRollbackSingle( c4 ) ;
               d4->recordChanged = old ;
               return 0 ;
            }
            #ifdef S4WINCE
               memcpy( &len, trans->header.dataLen, sizeof(S4LONG) ) ;
            #else
               len = trans->header.dataLen ;
            #endif
            #ifndef S4OFF_MEMO
               /* First cycle through the fields to be flushed */
               char *ptr = 0 ;
               unsigned int ptrLen = 0 ;

               for ( int memoFieldIndex = 0 ; memoFieldIndex < d4->dataFile->nFieldsMemo ; memoFieldIndex++ )
               {
                  if ( d4->fieldsMemo[memoFieldIndex].isChanged == 1 )
                  {
                     char *tempRecord = d4->record ;
                     d4->record = d4->recordOld ;
                     S4LONG entry = f4long( d4->fieldsMemo[memoFieldIndex].field ) ;
                     d4->record = tempRecord ;

                     if ( entry == 0 )
                     {
                        unsigned S4LONG zero = 0L ;
                        if ( tran4putData( trans, &zero, sizeof( zero ) ) == e4memory )
                        {
                           rc = e4memory ;
                           break ;
                        }
                        len += sizeof( zero ) ;
                     }
                     else
                     {
                        #ifdef S4MFOX
                           long type ;
                           rc = memo4fileRead( &d4->dataFile->memoFile, entry, &ptr, &ptrLen, &type ) ;
                        #else
                           rc = memo4fileRead( &d4->dataFile->memoFile, entry, &ptr, &ptrLen ) ;
                        #endif
                        if ( rc < 0 )
                           break ;
                        unsigned S4LONG tempLong = ptrLen;
                        if ( tran4putData( trans, &tempLong, sizeof( tempLong ) ) == e4memory )
                        {
                           rc = e4memory ;
                           break ;
                        }
                        len += sizeof( tempLong ) ;
                        if ( ptrLen > 0 )
                        {
                           if ( tran4putData( trans, ptr, ptrLen ) == e4memory )
                           {
                              rc = e4memory ;
                              break ;
                           }
                           len += ptrLen ;
                        }
                     }
                     unsigned S4LONG tempLong = d4->fieldsMemo[memoFieldIndex].len;
                     if ( tran4putData( trans, &tempLong, sizeof( tempLong ) ) == e4memory )
                     {
                        rc = e4memory ;
                        break ;
                     }
                     len += sizeof( tempLong ) ;
                     if ( d4->fieldsMemo[memoFieldIndex].len )
                     {
                        if ( tran4putData( trans, d4->fieldsMemo[memoFieldIndex].contents, d4->fieldsMemo[memoFieldIndex].len ) == e4memory )
                        {
                           rc = e4memory ;
                           break ;
                        }
                        len += d4->fieldsMemo[memoFieldIndex].len ;
                     }
                  }
                  else
                  {
                     unsigned S4LONG zero = 0L ;
                     if ( tran4putData( trans, &zero, sizeof( zero ) ) == e4memory )
                     {
                        rc = e4memory ;
                        break ;
                     }
                     if ( tran4putData( trans, &zero, sizeof( zero ) ) == e4memory )
                     {
                        rc = e4memory ;
                        break ;
                     }
                     len += 2 * sizeof( zero ) ;
                  }
               }

               u4free( ptr ) ;
               ptr = 0 ;
               ptrLen = 0 ;
               if ( rc < 0 )
               {
                  if ( hasTran )
                     code4tranRollbackSingle( c4 ) ;
                  d4->recordChanged = old ;
                  return error4stack( c4, (short)rc, E92601 ) ;
               }
               #ifdef S4WINCE
                  memcpy( trans->header.dataLen, &len, sizeof(S4LONG) ) ;
               #else
                  // AS 03/01/01 - len / dataLen is not unsigned short, but is a long - required for long memos
                  // trans->header.dataLen = (unsigned short int)len ;
                  trans->header.dataLen = len ;
               #endif
            #endif  /* S4OFF_MEMO */

            #ifdef S4OFF_OPTIMIZE
               rc = tran4lowAppend( trans, 0, 1 ) ;
            #else
               rc = tran4lowAppend( trans, 0, ( ( d4->dataFile->file.bufferWrites == 1 && d4->dataFile->file.doBuffer == 1) ? 0 : 1 ) ) ;
            #endif
            if ( rc != 0 )
            {
               if ( hasTran )
                  code4tranRollbackSingle( c4 ) ;
               d4->recordChanged = old ;
               return rc ;
            }
         }
      #endif /* S4OFF_TRAN */

      #ifndef S4INDEX_OFF
         rc = d4writeKeys( d4, rec ) ;
      #endif
      d4->recordChanged = old ;

      int finalRc = 0 ;
      if ( rc == 0 )
      {
         #ifndef S4OFF_MEMO
            /* First cycle through the fields to be flushed */
            for ( int memoFieldIndex = 0; memoFieldIndex < d4->dataFile->nFieldsMemo; memoFieldIndex++ )
            {
               rc = f4memoUpdate( d4->fieldsMemo[memoFieldIndex].field) ;
               if ( rc < 0 )
                  break ;
               if ( rc > 0 )
                  finalRc = rc ;
            }
         #endif  /* S4OFF_MEMO */

         if ( rc >= 0 )
            rc = d4writeData( d4, rec, doLock ) ;

         #ifndef S4OFF_INDEX
            if ( rc < 0 )
               d4unwriteKeys( d4, rec ) ;
         #endif
      }

      if ( finalRc == 0 )
         finalRc = rc ;

      #ifndef S4OFF_TRAN
         if ( rc < 0 || rc == r4unique )  // AS Jan 15/03 also applies for r4unique
         {
            // AS Jan 9/03 - updated rollback sequence
            if ( hasTran )
               code4tranRollbackSingle( c4 ) ;

            if ( d4transEnabled( d4, 1 ) )
            {
               rc = tran4set( trans, trans->currentTranStatus, -1L,
                    connectionId, TRAN4VOID, (unsigned int)0, data4clientId( d4 ), data4serverId( d4 ) ) ;
               if ( rc < 0 )
               {
                  if ( hasTran )
                     code4tranRollbackSingle( c4 ) ;
                  return rc ;
               }
               // AS Jan 30/02 - Need to return finalRc not result of tran4lowAppend.
               #ifdef S4OFF_OPTIMIZE
                  rc = tran4lowAppend( trans, "\0", 1 ) ;
               #else
                  rc = tran4lowAppend( trans, "\0", ( ( d4->dataFile->file.bufferWrites == 1 && d4->dataFile->file.doBuffer == 1) ? 0 : 1 ) ) ;
               #endif
               if ( rc < 0 && finalRc >= 0 )  // override normal return code with error if required
                  finalRc = rc ;
               return finalRc ;
            }
         }
      #else
         if ( rc < 0 )
            return rc ;
      #endif

      #ifndef S4OFF_TRAN
         if ( hasTran )
            code4tranCommitSingle( c4 ) ;
      #endif

      if ( unlock && code4unlockAuto( c4 ) != 0 ) /* unlock records (unless entire file is locked)... */
      {
         #ifdef S4SERVER
            if ( dfile4lockTestFile( d4->dataFile, data4clientId( d4 ), data4serverId( d4 ), lock4write ) != 1 )
               return d4unlockData( d4, -1 ) ;
         #else
            #ifndef S4OFF_TRAN
               if ( d4transEnabled( d4, 1 ) )
                  return 0 ;
            #endif
            #ifndef S4OFF_MULTI
               if ( d4lockTestFile( d4 ) != 1 )
               {
                  rc = d4unlockLow( d4, data4clientId( d4 ), 0 ) ;
                  if ( rc == r4active )  /* just a transactional notification */
                     return 0 ;
                  return rc ;
               }
            #endif
         #endif
      }

      // AS 05/20/99 -- problem here - if the write succeeded, was not copying the recordOld
      // with record.  Therefore if call d4write 2 times in a row without d4go(), etc. then
      // had an incorrect old record!
      if ( finalRc == 0 )
         c4memcpy( d4->recordOld, d4->record, dfile4recWidth( d4->dataFile ) ) ;

      return finalRc ;
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   int d4writeData( DATA4 *data, const long rec, const int doLock )
   {
      #ifndef S4OFF_MULTI
         int rc ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92602 ) ;
         if ( rec < 1 || data->codeBase == 0 )
            return error4( data->codeBase, e4parm, E92602 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      #ifndef S4OFF_MULTI
         if ( doLock )
         {
            #ifdef S4SERVER
               rc = d4lockInternal( data, rec, 0 ) ;
            #else
               rc = d4lockInternal( data, rec, 1 ) ;
            #endif
            if ( rc )
               return rc ;
         }
      #endif  /* S4OFF_MULTI */

      data->recordChanged = 0 ;
      return dfile4writeData( data->dataFile, rec, data->record ) ;
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   int dfile4writeData( DATA4FILE *d4, const long rec, const char *record )
   {
      #ifdef E4PARM_LOW
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
         if ( rec < 1 || d4->c4 == 0 )
            return error4( d4->c4, e4parm, E91102 ) ;
      #endif

      if ( error4code( d4->c4 ) < 0 )
         return e4codeBase ;

      d4->fileChanged = 1 ;
      // AS Aug 15/01 - Code for 'version number' handling so users can know if the file has changed.
      /* LY 2001/08/21 : added ifdef to avoid compile error on non-WIN32 */
      #if defined(S4WIN32) || !defined(S4STAND_ALONE)
         dfile4versionIncrement( d4 ) ;
      #endif

      if ( record[0] != ' ' && record[0] != '*' )  //  AS Nov 14/02 - data record has become corrupt... add this as a final check
         return error4( d4->c4, e4info, E83301 ) ;

      return file4writeInternal( &d4->file, dfile4recordPosition(d4, rec), record, d4->recWidth ) ;
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4OFF_TRAN )
   TAG4KEY_REMOVED *t4keyFind( TAG4 *tag, unsigned long recno, char *key )
   {
      /* search the removed list for the specified keys/char combination.  if recno is 0, then the matching character entry is returned */
      TAG4KEY_REMOVED *found ;

      for ( found = 0 ; ; )
      {
         found =(TAG4KEY_REMOVED *)l4next( &tag->removedKeys, found ) ;
         if ( found == 0 )
            break ;
         if ( recno == 0 )
         {
            if ( c4memcmp( key, found->key, (unsigned int)tag->tagFile->header.keyLen ) == 0 )
               return found ;
         }
         else
            if ( found->recno == recno )
               if ( c4memcmp( key, found->key, (unsigned int)tag->tagFile->header.keyLen ) == 0 )
                  return found ;
      }

      return 0 ;
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4OFF_TRAN ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   static int d4writeKeysRemoveKeys( DATA4 *d4, TAG4 *tagOn, char *saveRecBuffer, long rec )
   {
      // called if a failure occurs in adding a key.  eg. e4unique/r4unique/r4candidate or error condition
      for(;;)
      {
         tagOn = d4tagPrev( d4, tagOn ) ;
         if ( tagOn == 0 )
            break ;
         TAG4FILE *tagFileOn = tagOn->tagFile ;

         int rc2 ;
         if ( tagOn->added )
         {
            d4->record = saveRecBuffer ;

            rc2 = expr4context( tagFileOn->expr, d4 ) ;
            if ( rc2 < 0 )
               return rc2 ;
            if ( tagFileOn->filter != 0 )
            {
               rc2 = expr4context( tagFileOn->filter, d4 ) ;
               if ( rc2 < 0 )
                  return rc2 ;
            }

            rc2 = tfile4removeCalc( tagFileOn, rec ) ;
            if ( rc2 < 0 )
               return rc2 ;
         }

         if ( tagOn->removed )
         {
            d4->record = d4->recordOld ;

            rc2 = expr4context( tagFileOn->expr, d4 ) ;
            if ( rc2 < 0 )
               return rc2 ;
            if ( tagFileOn->filter != 0 )
            {
               rc2 = expr4context( tagFileOn->filter, d4 ) ;
               if ( rc2 < 0 )
                  return rc2 ;
            }
            rc2 = t4addCalc( tagOn, rec ) ;
            if ( rc2 < 0 )
            {
               d4->record = saveRecBuffer ;
               return rc2 ;
            }
         }
      }
      return 0 ;
   }


   static int d4writeKeysOneTag( DATA4 *d4, TAG4 *tagOn, char *saveRecBuffer, int *indexLocked, long rec )
   {
      tagOn->added = tagOn->removed = 0 ;

      TAG4FILE *tagFileOn = tagOn->tagFile ;
      int rc2 = expr4context( tagFileOn->expr, d4 ) ;
      if ( rc2 < 0 )
         return rc2 ;
      if ( tagFileOn->filter != 0 )
      {
         rc2 = expr4context( tagFileOn->filter, d4 ) ;
         if ( rc2 < 0 )
            return rc2 ;
      }
      unsigned char *tempPtr ;
      int keyLen = tfile4exprKey( tagFileOn, &tempPtr ) ;
      if ( keyLen < 0 )
         return -1 ;

      CODE4 *c4 = d4->codeBase ;
      #ifdef E4ANALYZE
         if ( keyLen != tagFileOn->header.keyLen || keyLen > I4MAX_KEY_SIZE)
            return error4( c4, e4index, E92604 ) ;
      #endif

      unsigned char newKeyBuf[I4MAX_KEY_SIZE] ;
      c4memcpy( (void *)newKeyBuf, tempPtr, (unsigned int)keyLen ) ;
      #ifdef S4FOX
         int newKeyLen = keyLen ;
      #endif

      int addNewKey = 1 ;
      if ( tagFileOn->filter )
         addNewKey = expr4true( tagFileOn->filter ) ;

      d4->record = d4->recordOld ;

      rc2 = expr4context( tagFileOn->expr, d4 ) ;
      if ( rc2 < 0 )
         return rc2 ;

      if ( tagFileOn->filter != 0 )
      {
         rc2 = expr4context( tagFileOn->filter, d4 ) ;
         if ( rc2 < 0 )
            return rc2 ;
      }

      int oldKeyAdded = 1 ;
      if ( tagFileOn->filter )
         oldKeyAdded = expr4true( tagFileOn->filter ) ;
      unsigned char *oldKey ;
      keyLen = tfile4exprKey( tagOn->tagFile, &oldKey ) ;

      d4->record = saveRecBuffer ;

      if ( keyLen < 0 )
         return -1 ;

      if ( oldKeyAdded == addNewKey )
      {
         #ifdef S4FOX
            // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
            // if ( u4keycmp( (void *)newKeyBuf, oldKey, (unsigned int)keyLen, (unsigned int)newKeyLen, 0, &tagFileOn->vfpInfo ) == 0 )
            if ( u4keycmp( (void *)newKeyBuf, oldKey, (unsigned int)keyLen, (unsigned int)newKeyLen, 0, collation4get( tagFileOn->collateName ) ) == 0 )
         #else
            if ( u4memcmp( (void *)newKeyBuf, oldKey, (unsigned int)keyLen ) == 0 )
         #endif
             return 0 ;
      }

      int rc = 0 ;
      #ifndef S4OFF_MULTI
         if ( (*indexLocked) == 0 )
         {
            #ifdef S4SERVER
               rc = dfile4lockIndex( d4->dataFile, data4serverId( d4 ) ) ;
            #else
               rc = d4lockIndex( d4 ) ;
            #endif
            if ( rc )
               return rc ;
            (*indexLocked) = 1 ;
         }
      #endif  /* S4OFF_MULTI */

      if ( oldKeyAdded )
      {
         tagOn->removed = 1 ;
         #ifndef S4OFF_TRAN
            if ( code4tranStatus( c4 ) == r4active && ( t4unique( tagOn ) == r4unique || t4unique( tagOn ) == e4unique
                 #ifdef S4FOX
                    || t4unique( tagOn ) == r4candidate
                 #endif
                 ) )  /* save the entry due to transactions */
            {
               /* LY 2001/07/30 : changed sizeof(S4LONG) to sizeof(long) to match actual type of TAG4KEY_REMOVED.recno */
               TAG4KEY_REMOVED *removed = (TAG4KEY_REMOVED *)u4allocFree( c4, (long)sizeof( LINK4 ) + (long)sizeof(long) + tagFileOn->header.keyLen ) ;
               if ( removed == 0 )
                  return e4memory ;
               removed->recno = rec ;
               memcpy( removed->key, oldKey, (unsigned int)tagFileOn->header.keyLen ) ;
               l4addBefore( &tagOn->removedKeys, l4first( &tagOn->removedKeys ), removed ) ;
            }
            else
            {
         #endif
            rc2 = expr4context( tagFileOn->expr, d4 ) ;
            if ( rc2 < 0 )
               return rc2 ;
            if ( tagFileOn->filter != 0 )
            {
               rc2 = expr4context( tagFileOn->filter, d4 ) ;
               if ( rc2 < 0 )
                  return rc2 ;
            }

            rc2 = tfile4remove( tagFileOn, oldKey, (unsigned long)rec ) ;
            if ( rc2 < 0 )
               return rc2 ;
         #ifndef S4OFF_TRAN
            }
         #endif
      }

      #ifndef S4OFF_TRAN
         if ( code4tranStatus( c4 ) == r4rollback && ( t4unique( tagOn ) == r4unique || t4unique( tagOn ) == e4unique
              #ifdef S4FOX
                 || t4unique( tagOn ) == r4candidate
              #endif
              ) )  /* remove the removal due to transactions */
         {
            TAG4KEY_REMOVED *removed = t4keyFind( tagOn, rec, (char *)newKeyBuf ) ;
            if ( removed != 0 )  /* means the record really was not deleted, so just remove from the list of to-be-removed */
            {
               l4remove( &tagOn->removedKeys, removed ) ;
               u4free( removed ) ;
               addNewKey = 0 ;
            }
            /* else: removed is null when, within the same transaction,  a record alteration is made to replace the key that was to
               be removed.  At that point, the key was actually removed,  so it must now be added back */
         }
      #endif
      if ( addNewKey )
      {
         tagOn->added = 1 ;
         rc2 = expr4context( tagFileOn->expr, d4 ) ;
         if ( rc2 < 0 )
            return rc2 ;
         if ( tagFileOn->filter != 0 )
         {
            rc2 = expr4context( tagFileOn->filter, d4 ) ;
            if ( rc2 < 0 )
               return rc2 ;
         }
         #ifndef S4OFF_TRAN
            /* if a unique tag, first check if the record may have instead been pre-deleted */
            if ( t4unique( tagOn ) != 0 )
            {
               TAG4KEY_REMOVED *removed = t4keyFind( tagOn, 0L, (char *)newKeyBuf ) ;
               if ( removed != 0 )   /* re-adding a key that was removed, so really remove this entry from the tag file first */
               {
                  // AS Dec 12/02 - If the record number is the same, it means we actually
                  // are adding back the same key we had removed before (e.g. a user set
                  // name from 'Bob' to 'Doug', and is now returning to 'Bob', as opposed
                  // to the other scenario where record 1 is changed from 'Bob' to 'Doug'
                  // and record 2 is changed from 'Dave' to 'Bob'
                  // therefore, if the recno matches, don't do the remove and don't do
                  // the add, just remove from the list of removed keys.
                  if ( rec == (long)removed->recno )
                  {
                     l4remove( &tagOn->removedKeys, removed ) ;
                     u4free( removed ) ;
                     return 0 ;
                  }
                  rc2 = tfile4remove( tagFileOn, removed->key, removed->recno ) ;
                  if ( rc2 < 0 )
                     return rc2 ;
                  l4remove( &tagOn->removedKeys, removed ) ;
                  u4free( removed ) ;
               }
            }
         #endif
         rc = tfile4add( tagFileOn, newKeyBuf, rec, t4unique( tagOn ) ) ;
         if ( rc == r4unique || rc == e4unique )
         {
            int saveError = error4set( c4, 0 ) ;

            // AS Dec 12/02 - only needs removal if the old key was added...
            if ( oldKeyAdded )
            {
               #ifndef S4OFF_TRAN
                  if ( code4tranStatus( c4 ) == r4active && ( t4unique( tagOn ) == r4unique || t4unique( tagOn ) == e4unique
                       #ifdef S4FOX
                          || t4unique( tagOn ) == r4candidate
                       #endif
                       ) )  /* just remove from the removed list */
                  {
                     TAG4KEY_REMOVED *removed =(TAG4KEY_REMOVED *)l4first( &tagOn->removedKeys ) ;
                     if ( removed == 0 )
                     {
                        error4( c4, e4info, E92604 ) ;
                        error4set( c4, 0 ) ;
                        return e4info ;
                     }
                     // ensure it is correct...
                     assert5( (long)removed->recno == (long)rec ) ;
                     l4remove( &tagOn->removedKeys, removed ) ;
                     u4free( removed ) ;
                  }
                  else
               #endif
               {
                  rc2 = expr4context( tagFileOn->expr, d4 ) ;
                  if ( rc2 < 0 )
                     return rc2 ;
                  if ( tagFileOn->filter != 0 )
                  {
                     rc2 = expr4context( tagFileOn->filter, d4 ) ;
                     if ( rc2 < 0 )
                        return rc2 ;
                  }
                  rc2 = tfile4add( tagFileOn, (unsigned char *)oldKey, rec, t4unique( tagOn ) ) ;
                  if ( rc2 < 0 )
                     return -1 ;
               }
            }

            /* Remove the keys which were just added */
            rc2 = d4writeKeysRemoveKeys( d4, tagOn, saveRecBuffer, rec ) ;
            if ( rc2 < 0 )
               return rc2 ;
            d4->record = saveRecBuffer ;

            error4set( c4, (short)saveError ) ;
            if ( saveError < 0 )
               rc = saveError ;
            return rc ;
         }
         if ( rc < 0 )   /* can't generate e4unique, so just set to -1 */
            return rc ;
         rc = 0 ;
      }

      return rc ;
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( EXCEPTION4REINDEX ) && !defined( S4OFF_INDEX )
   int d4writeRecoverIndexes( DATA4 *data, TAG4 **tagOn, TAG4 **tagFailed, char *saveRecBuffer, long rec )
   {
      /* Indexes have been detected as corrupt.  Attempt here to recover them be reindexing
         This function may get called on either an e4index error or a GPF.  If a GPF or error
         generated in this code below, it indicates that we cannot recover from the problem.
         (i.e. gpf likely due to vast amounts of useful memory being overwritten, or an
         index error that cannot be fixed automatically).

         tagFailed is used to indicate which tag has failed.  We use it as an input paramater to indicate
         which tag was last recovered using this function.  That way, if the same tag fails a second time
         it indicates that we are stuck in a repetitive failure, so must instead return the failure back
         to the application.  the tagFailed paramater is set to the input tag.
      */

      // if an exception occurs here, it means memory is corrupted and we cannot recover, so give error as such
      // catch any exception that occurs during an addCalc, and then reindex in that case
      int rc = e4index ;  // be default we are in an error state already
      try
      {
         error4describe( data->codeBase, e4index, 80401L, d4alias( data ), t4alias( (*tagOn) ), 0 ) ;  // note failure so keys below are reomved
         error4set( data->codeBase, 0 ) ;
         if ( (*tagFailed) != (*tagOn) )   // this tag did not just previously fail, so attempt a reindex
         {
            (*tagFailed) = (*tagOn) ;
            int saveRecNum = data->recNum ;  // should be for append
            data->recordChanged = 0 ;
            // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with aes encryption
            char *saveRecord = (char *)u4alloc( dfile4recWidth( data->dataFile ) ) ;
            if ( saveRecord != 0 )
            {
               d4writeKeysRemoveKeys( data, (*tagOn), saveRecBuffer, rec ) ;
               // recover by reindexing the offending index
               error4set( data->codeBase, 0 ) ;

               // we need to save the record which was being appended...
               // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with aes encryption
               memcpy( saveRecord, d4record( data ), dfile4recWidth( data->dataFile ) ) ;
               rc = i4reindex( (*tagOn)->index ) ;
               memcpy( d4record( data ), saveRecord, dfile4recWidth( data->dataFile ) ) ;
               u4free( saveRecord ) ;
               data->recordChanged = 1 ;
               data->recNum = saveRecNum ;  // reset
            }
            if ( rc >= 0 )  // also if r4unique, continue on
            {
               // we were successful, so lets just continue on with the tag reset
               (*tagOn) = 0 ;
            }
            // else // more severe error it will fail below as expected
         }
      }
      catch( ... )
      {
         // cannot recover, give error and shut down
         #ifdef S4SERVER
            rc = error4( data->codeBase, e4memory, E70223 ) ;  // note failure so keys below are reomved
         #else
            rc = error4( data->codeBase, e4memory, E91103 ) ;  // note failure so keys below are reomved
         #endif
      }

      return rc ;
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( EXCEPTION4REINDEX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   int d4writeKeys( DATA4 *d4, const long rec )
   {
      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E92604 ) ;
         if ( rec < 1 || d4->codeBase == 0 )
            return error4( d4->codeBase, e4parm, E92604 ) ;
      #endif
      #ifdef S4INDEX_OFF
         return 0 ;
      #else
         CODE4 *c4 = d4->codeBase ;
         d4->bofFlag = d4->eofFlag = 0 ;

         int rc ;
         #if defined( S4CB51 ) && !defined(S4OFF_MULTI )
            #ifdef S4SERVER
               rc = d4lockInternal( d4, rec, 0 ) ;
            #else
               rc = d4lockInternal( d4, rec, 1 ) ;
            #endif
            if ( rc )
               return rc ;
         #endif

         #ifdef S4CLIPPER
            if ( d4->dataFile->tagfiles.nLink > 0 )
         #else
            if ( d4->dataFile->indexes.nLink > 0 )
         #endif
         {
            if ( d4readOld( d4, rec ) < 0 )
               return -1 ;
            if ( u4memcmp( d4->recordOld, d4->record, dfile4recWidth( d4->dataFile )) == 0 )
               return 0 ;
         }

         char *saveRecBuffer = d4->record ;
         rc = 0 ;
         // AS 06/09/00 was not compiling in S4OFF_MULTI
         int indexLocked ;
         #ifndef S4OFF_MULTI
            #ifdef S4SERVER
               indexLocked = (dfile4lockTestIndex( d4->dataFile, data4serverId( d4 ) ) == 1 ) ? 2 : 0 ;  /* 2 means was user locked */
            #else
               indexLocked = ( d4lockTestIndex( d4 ) == 1 ) ? 2 : 0 ;  /* 2 means was user locked */
            #endif
         #endif

         #ifdef EXCEPTION4REINDEX
            TAG4 *tagFailed = 0 ;   // a previously failing tag if any for auto recovery
         #endif

         for( TAG4 *tagOn = 0 ;; )
         {
            tagOn = d4tagNext( d4, tagOn ) ;
            if ( tagOn == 0 )
               break ;

            #ifdef EXCEPTION4REINDEX
               try
               {
            #endif
                  rc = d4writeKeysOneTag( d4, tagOn, saveRecBuffer, &indexLocked, rec ) ;
            #ifdef EXCEPTION4REINDEX
               }
               catch( ... )
               {
                  // try to recover in the gpf case by reindexing index
                  rc = e4index ;
               }
            #endif

            if ( rc != 0 )
            {
               #ifdef EXCEPTION4REINDEX
                  // attempt to recover if an index corruption error has occurred
                  if ( rc == e4index || error4code( c4 ) == e4index )
                     rc = d4writeRecoverIndexes( d4, &tagOn, &tagFailed, saveRecBuffer, rec ) ;
               #endif
               if ( rc < 0 || rc == r4unique )
                  break ;
            }
         }
         #ifndef S4OFF_MULTI
            if ( indexLocked == 1 )
               dfile4unlockIndex( d4->dataFile, data4serverId( d4 ) ) ;
         #endif

         d4->recNumOld = -1 ;
         return rc ;
      #endif  /* S4OFF_INDEX */
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */



#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   static int d4unwriteKeys( DATA4 *d4, const long rec )
   {
      unsigned char newKeyBuf[I4MAX_KEY_SIZE] ;
      unsigned char *oldKey, *tempPtr ;
      int rc2, rc, keyLen, oldKeyAdded, addNewKey ;
      #ifndef S4OFF_MULTI
         int indexLocked ;
      #endif
      TAG4 *tagOn ;
      TAG4FILE *tagFileOn ;
      #ifndef S4OFF_TRAN
         TAG4KEY_REMOVED *removed ;
      #endif
      #ifdef S4FOX
         int newKeyLen ;
      #endif

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E92605 ) ;
         if ( rec < 1 || d4->codeBase == 0 )
            return error4( d4->codeBase, e4parm, E92605 ) ;
      #endif

      #ifdef S4CLIPPER
         if ( d4->dataFile->tagfiles.nLink > 0 )
      #else
         if ( d4->dataFile->indexes.nLink > 0 )
      #endif
         if ( u4memcmp( d4->recordOld, d4->record, dfile4recWidth( d4->dataFile )) == 0 )
            return 0 ;

      char *saveRecBuffer = d4->record ;

      rc = 0 ;
      #ifndef S4OFF_MULTI
         #ifdef S4SERVER
            indexLocked = ( dfile4lockTestIndex( d4->dataFile, data4serverId( d4 ) ) == 1 ) ? 2 : 0 ;  /* 2 means was user locked */
         #else
            indexLocked = ( d4lockTestIndex( d4 ) == 1 ) ? 2 : 0 ;  /* 2 means was user locked */
         #endif
      #endif

      for( tagOn = 0 ;; )
      {
         tagOn = d4tagNext( d4, tagOn ) ;
         if ( tagOn == 0 )
            break ;

         tagFileOn = tagOn->tagFile ;
         rc2 = expr4context( tagFileOn->expr, d4 ) ;
         if ( rc2 < 0 )
         {
            rc = rc2 ;
            break ;
         }
         if ( tagFileOn->filter != 0 )
         {
            rc2 = expr4context( tagFileOn->filter, d4 ) ;
            if ( rc2 < 0 )
            {
               rc = rc2 ;
               break ;
            }
         }

         oldKeyAdded = addNewKey = 1 ;

         keyLen = tfile4exprKey( tagFileOn, &tempPtr ) ;
         if ( keyLen < 0 )
         {
            rc = keyLen ;
            break ;
         }
         #ifdef E4ANALYZE
            if ( keyLen != tagFileOn->header.keyLen || keyLen > I4MAX_KEY_SIZE)
               return error4( d4->codeBase, e4index, E92605 ) ;
         #endif

         c4memcpy( (void *)newKeyBuf, tempPtr, (unsigned int)keyLen ) ;
         #ifdef S4FOX
            newKeyLen = keyLen ;
         #endif

         if ( tagFileOn->filter )
            addNewKey = expr4true( tagFileOn->filter ) ;

         d4->record = d4->recordOld ;

         rc2 = expr4context( tagFileOn->expr, d4 ) ;
         if ( rc2 < 0 )
         {
            rc = rc2 ;
            break ;
         }
         if ( tagFileOn->filter != 0 )
         {
            rc2 = expr4context( tagFileOn->filter, d4 ) ;
            if ( rc2 < 0 )
            {
               rc = rc2 ;
               break ;
            }
         }

         if ( tagFileOn->filter )
            oldKeyAdded = expr4true( tagFileOn->filter ) ;
         keyLen = tfile4exprKey( tagFileOn, &oldKey ) ;

         d4->record = saveRecBuffer ;

         if ( keyLen < 0 )
         {
            rc = keyLen ;
            break ;
         }
         if ( oldKeyAdded == addNewKey )
            #ifdef S4FOX
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // if ( u4keycmp( (void *)newKeyBuf, oldKey, (unsigned int)keyLen, (unsigned int)newKeyLen, 0, &tagFileOn->vfpInfo ) == 0 )
               if ( u4keycmp( (void *)newKeyBuf, oldKey, (unsigned int)keyLen, (unsigned int)newKeyLen, 0, collation4get( tagFileOn->collateName ) ) == 0 )
            #else
               if ( u4memcmp( (void *)newKeyBuf, oldKey, (unsigned int)keyLen ) == 0 )
            #endif
                continue ;

         #ifndef S4OFF_MULTI
            if ( indexLocked == 0 )
            {
               #ifdef S4SERVER
                  rc = dfile4lockIndex( d4->dataFile, data4serverId( d4 ) ) ;
               #else
                  rc = d4lockIndex( d4 ) ;
               #endif
               if ( rc )
                  break ;
               indexLocked = 1 ;
            }
         #endif  /* S4OFF_MULTI */

         if ( oldKeyAdded )
         {
            #ifndef S4OFF_TRAN
               if ( code4tranStatus( d4->codeBase ) == r4active && ( t4unique( tagOn ) == r4unique ||
                    t4unique( tagOn ) == e4unique
                    #ifdef S4FOX
                       || t4unique( tagOn ) == r4candidate
                    #endif
                    ) )  /* save the entry due to transactions */
               {
                  for ( removed = 0 ;; )
                  {
                     removed = (TAG4KEY_REMOVED *)l4next( &tagOn->removedKeys, removed ) ;
                     /* if a reindex occurs, then the list will be empty,
                        so follow regular procedures.  Otherwise the key should
                        be on the removed list */
                     if ( removed == 0 )
                        break ;
                     if ( c4memcmp( removed->key, oldKey, (unsigned int)tagFileOn->header.keyLen ) == 0 && ( removed->recno == (unsigned long)rec ) )
                     {
                        l4remove( &tagOn->removedKeys, removed ) ;
                        u4free( removed ) ;
                        break ;
                     }
                  }
                  if ( rc < 0 )
                     break ;
               }
               else
               {
            #endif
               rc2 = expr4context( tagFileOn->expr, d4 ) ;
               if ( rc2 < 0 )
               {
                  rc = rc2 ;
                  break ;
               }
               if ( tagFileOn->filter != 0 )
               {
                  rc2 = expr4context( tagFileOn->filter, d4 ) ;
                  if ( rc2 < 0 )
                  {
                     rc = rc2 ;
                     break ;
                  }
               }
               if ( tfile4add( tagFileOn, oldKey, rec, t4unique( tagOn ) ) < 0 )
               {
                  rc = -1 ;
                  break ;
               }
            #ifndef S4OFF_TRAN
               }
            #endif
         }

         #ifndef S4OFF_TRAN
            if ( code4tranStatus( d4->codeBase ) == r4rollback && ( t4unique( tagOn ) == r4unique ||
                 t4unique( tagOn ) == e4unique
                 #ifdef S4FOX
                    || t4unique( tagOn ) == r4candidate
                 #endif
                 ) )  /* remove the removal due to transactions */
            {
               rc = -1 ;
               break ;
            }
            else
         #endif
         if ( addNewKey )
         {
            rc2 = expr4context( tagFileOn->expr, d4 ) ;
            if ( rc2 < 0 )
            {
               rc = rc2 ;
               break ;
            }
            if ( tagFileOn->filter != 0 )
            {
               rc2 = expr4context( tagFileOn->filter, d4 ) ;
               if ( rc2 < 0 )
               {
                  rc = rc2 ;
                  break ;
               }
            }
            rc = tfile4remove( tagFileOn, newKeyBuf, (unsigned long)rec ) ;
            if ( rc == r4unique || rc == e4unique )
            {
               rc = -1 ;
               break ;
            }
            if ( rc < 0 )
            {
               rc = -1 ;
               break ;
            }
            rc = 0 ;
         }
      }
      #ifndef S4OFF_MULTI
         if ( indexLocked == 1 )
            dfile4unlockIndex( d4->dataFile, data4serverId( d4 ) ) ;
      #endif

      d4->recNumOld = -1 ;
      return rc ;
   }
#endif /* !defined( S4OFF_WRITE ) && defined( S4CLIENT ) */


#if !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   // used for ODBC
   int S4FUNCTION D4writeDirect( DATA4 *d4, long recIn, char *buffer )
   {
      // currently only used by ODBC driver.  Writes the record direct to disk.  Avoids any locking.  Uses the input
      // buffer to do the write.
      #ifndef S4OFF_SECURITY
         if ( account4userAllowUpdate( &d4->codeBase->currentClient->account, d4 ) == FALSE )
            return e4authorize ;
         if ( d4->record[0] == '*' && d4->recordOld[0] == ' ' )  // attempt to delete
            if ( account4userAllowDelete( &d4->codeBase->currentClient->account, d4 ) == FALSE )
               return e4authorize ;
      #endif
      int rc = 0 ;

      assert5 ( recIn > 0 ) ;

      char *recordBuf = d4->record ;
      long oldRecNo = d4recNo( d4 ) ;

      d4->record = buffer ;
      d4recNoSet( d4, recIn ) ;

      d4changedVoid( d4, 1 ) ;
      rc = d4writeLow( d4, recIn, 0, 0 ) ;
      d4changedVoid( d4, 0 ) ; // in case of failure...

      d4->record = recordBuf ;
      d4recNoSet( d4, oldRecNo ) ;

      return rc ;
   }
#endif /* !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */


#if !defined( S4OFF_WRITE ) && defined( S4CLIENT )
   /* AS Apr 10/02 - New function for advance-reading client/server */
   int d4writeBufferDo( DATA4 *data )
   {
      assert5port( "new client/server functionality for batch writing of records" ) ;
      DATA4BATCH_WRITE *batch = &data->batchWrite ;

      // buffer-write a set of records to the server
      if ( batch->curWriteBufCount == 0 )  // buffer empty
         return 0 ;

      CODE4 *c4 = data->codeBase ;
      CONNECT4 *connect = c4getClientConnect( c4 ) ;
      long recWidth = dfile4recWidth( data->dataFile ) ;

      if ( connect == 0 )
         return error4( c4, e4connect, E94802 ) ;

      connect4sendShort( connect, MSG5WRITE_BATCH ) ;
      connect4sendLong( connect, data4clientId( data ) ) ;
      connect4sendLong( connect, data4serverId( data ) ) ;
      connect4sendShort( connect, c4->readLock ) ;
      connect4sendShort( connect, code4unlockAuto( c4 ) ) ;
      connect4sendLong( connect, batch->curWriteBufCount ) ;

      char *buf = batch->writeDelayBuf ;
      int numMemos = data->dataFile->nFieldsMemo ;
      for ( int loop = 0 ; loop < batch->curWriteBufCount ; loop++ )
      {
         connect4sendLong( connect, *((long *)buf) ) ;
         buf += sizeof( long ) ;
         connect4send( connect, buf, recWidth ) ;
         buf += recWidth ;
         for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
         {
            MEMO4BATCH_ENTRY *entry = &batch->memos[loop].memos[memoLoop] ;
            connect4sendLong( connect, entry->contentsLen ) ;
            if ( entry->contentsLen > 0 )
               connect4send( connect, entry->contents, entry->contentsLen ) ;
         }
      }

      connect4sendFlush( connect ) ;
      short rc = connect4receiveShort( connect ) ;

      batch->curWriteBufCount = 0 ;
      switch( rc )
      {
         case r4unique:
            rc = r4batchUnique ;
            break ;
         case e4unique:
            rc = error4( c4, e4batchUnique, E94802 ) ;
            break ;
         case r4uniqueContinue:
            rc = r4batchUniqueContinue ;
            break ;
      }

      if ( rc != 0 )  // if a write/append failed, indicate that record count is out of date
         data->dataFile->numRecs = -1 ;

      return rc ;
   }


   void d4batchWriteFree( DATA4 *data )
   {
      assert5port( "new client/server functionality for batch writing of records" ) ;
      assert5( data->batchWrite.curWriteBufCount == 0 ) ;   // better should be empty...

      // free memory for the batch write buffers
      DATA4BATCH_WRITE *batch = &data->batchWrite ;

      if ( batch->memos != 0 )
      {
         MEMO4BATCH *batchOn = batch->memos ;
         for ( int onEntry = batch->writeRecsToBuf - 1 ; onEntry >= 0 ; onEntry-- )
         {
            if ( batchOn->memos != 0 )
            {
               for ( int onMemo = data->dataFile->nFieldsMemo - 1 ; onMemo >= 0 ; onMemo -- )
               {
                  if ( batchOn->memos[onMemo].contents != 0 )
                  {
                     u4free( batchOn->memos[onMemo].contents ) ;
                     batchOn->memos[onMemo].contents = 0 ;
                     batchOn->memos[onMemo].contentsLen = 0 ;
                     batchOn->memos[onMemo].contentsAllocLen = 0 ;
                  }
               }
            }
            batchOn++ ;
         }

         u4free( batch->memos ) ;
         batch->memos = 0 ;
         if ( batch->memoBatchEntry != 0 )
         {
            u4free( batch->memoBatchEntry ) ;
            batch->memoBatchEntry = 0 ;
         }
      }

      if ( batch->writeDelayBuf != 0 )
      {
         u4free( batch->writeDelayBuf ) ;
         batch->writeDelayBuf = 0 ;
      }
      batch->writeRecsToBuf = 0 ;
   }



   static int d4batchWriteAlloc( DATA4 *data, long numRecs )
   {
      assert5port( "new client/server functionality for batch writing of records" ) ;
      // allocate memory for the batch write buffers
      // returns 0 or e4memory only

      // buffer holds the rec # and record buffer for each record
      DATA4BATCH_WRITE *batch = &data->batchWrite ;

      // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with aes encryption
      batch->writeDelayBuf = (char *)u4allocFree( data->codeBase, numRecs * (dfile4recWidth( data->dataFile ) + sizeof( long ) )) ;
      if ( batch->writeDelayBuf == 0 )
         return e4memory ;

      int numMemos = data->dataFile->nFieldsMemo ;
      if ( numMemos != 0 )
      {
         // allocate memory for memo fields
         batch->memos = (MEMO4BATCH *)u4allocFree( data->codeBase, numRecs * sizeof( MEMO4BATCH ) ) ;
         if ( batch->memos == 0 )
         {
            d4batchWriteFree( data ) ;
            return e4memory ;
         }

         batch->memoBatchEntry = (MEMO4BATCH_ENTRY *)u4allocFree( data->codeBase, numRecs * sizeof( MEMO4BATCH_ENTRY ) * numMemos ) ;
         if ( batch->memoBatchEntry == 0 )
         {
            d4batchWriteFree( data ) ;
            return e4memory ;
         }
         // and set up the memo entries
         for ( int memoLoop = 0 ; memoLoop < numRecs ; memoLoop++ )
         {
            batch->memos[memoLoop].memos = batch->memoBatchEntry + (numMemos * memoLoop) ;
         }
      }

      return 0 ;
   }
#endif /* !defined( S4OFF_WRITE ) && defined( S4CLIENT ) */



long S4FUNCTION d4writeBuffer( DATA4 *data, long numRecs )
{
   #ifdef S4LUPACH  /* LY 2003/07/07 */
      return 0 ;
   #else
      assert5port( "new client/server functionality for batch writing of records" ) ;
   #if !defined( S4OFF_WRITE ) && defined( S4CLIENT )
      // numRecs == # to buffer, 0 to stop, -1 to get a current count only

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93316 ) ;
         if ( numRecs < -1 )
            return error4( 0, e4parm_null, E93316 ) ;
      #endif

      DATA4BATCH_WRITE *batch = &data->batchWrite ;

      if ( numRecs < 0 )
         return batch->writeRecsToBuf ;

      int rc = 0 ;

      if ( batch->curWriteBufCount != 0 )  // flush out the existing buffer
      {
         rc = d4writeBufferDo( data ) ;
         assert5( batch->curWriteBufCount == 0 ) ;
      }

      if ( batch->writeRecsToBuf != numRecs )
      {
         if ( numRecs > batch->writeRecsToBuf || numRecs == 0 )   // existing buffer, if any, not large enough
         {
            d4batchWriteFree( data ) ;
            if ( numRecs != 0 )
               if ( d4batchWriteAlloc( data, numRecs ) != 0 )
                  return e4memory ;
         }

         batch->writeRecsToBuf = numRecs ;
      }

      if ( rc != 0 )
         return error4set( data->codeBase, rc ) ;

      return batch->writeRecsToBuf ;
   #else
      return 0 ;
   #endif
   #endif  /* S4LUPACH */
}



#ifdef S4CLIENT
   void code4writeBufferReset( CODE4 *c4 )
   {
      assert5port( "new client/server functionality for batch writing of records" ) ;
      // resets all write buffers WITHOUT flusing contents (normally done when transaction rollback performed)
      DATA4 *dataOn, *dataNext ;
      LIST4 *list = tran4dataList( (&(c4->c4trans.trans)) ) ;

      for ( dataNext = (DATA4 *)l4first( list ) ; ; )
      {
         dataOn = dataNext ;
         if ( dataOn == 0 )
            break ;
         dataNext = (DATA4 *)l4next( list, dataNext ) ;
         if ( dataOn == dataNext )   /* error -- stuck in endless loop */
            break ;
         if ( dataOn->batchWrite.writeRecsToBuf > 0 )
            dataOn->batchWrite.curWriteBufCount = 0 ;
      }
   }



   void code4writeBufferFlush( CODE4 *c4 )
   {
      assert5port( "new client/server functionality for batch writing of records" ) ;
      // flushes all write buffers (normally done when transaction commit performed)
      // resets all write buffers WITHOUT flusing contents (normally done when transaction rollback performed)
      DATA4 *dataOn, *dataNext ;
      LIST4 *list = tran4dataList( (&(c4->c4trans.trans)) ) ;

      int saveRc = error4code( c4 ) ;

      for ( dataNext = (DATA4 *)l4first( list ) ; ; )
      {
         dataOn = dataNext ;
         if ( dataOn == 0 )
            break ;
         dataNext = (DATA4 *)l4next( list, dataNext ) ;
         if ( dataOn == dataNext )   /* error -- stuck in endless loop */
            break ;
         if ( dataOn->batchWrite.writeRecsToBuf > 0 )
            if ( dataOn->batchWrite.curWriteBufCount > 0 )
            {
               int rc = d4writeBufferDo( dataOn ) ;
               if ( rc != 0 )
               {
                  if ( saveRc >= 0 )  // replace with code, or change to error code, leave if in error state already
                     saveRc = rc ;
                  error4set( c4, 0 ) ;  // reset so that buffers will continue to be written (for now)
               }
            }
      }

      if ( saveRc != 0 )
         error4set( c4, saveRc ) ;
   }
#endif /* S4CLIENT */
