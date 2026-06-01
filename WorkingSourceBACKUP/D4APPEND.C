/* d4append.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

#ifndef S4OFF_WRITE

// AS Jun 5/03 - cleaned up for clipper
#if !defined( S4CLIENT ) && !defined( S4OFF_MULTI ) && defined( S4FOX ) && !defined( S4UTILS )
   /* not S4CLIENT, not S4OFF_MULTI, not S4OFF_WRITE, S4FOX, not S4UTILS */
   static void d4incrementAutoIncrementValue( DATA4 *data )
   {
      if ( data->dataFile->autoIncrementSupported )  // update the auto-increment value...
      {
         #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
            double tempDbl ;  /* LY July 10/03 : restored tempDbl */
            memcpy( (char*)&tempDbl, data->dataFile->autoIncrementVal, sizeof(double) ) ;
            tempDbl++ ;
            memcpy( data->dataFile->autoIncrementVal, (char*)&tempDbl, sizeof(double) ) ;
         #else
            data->dataFile->autoIncrementVal += 1 ;
         #endif
      }
   }



   /* not S4CLIENT, not S4OFF_MULTI, not S4OFF_WRITE, S4FOX, not S4UTILS */
   static void d4assignAutoIncrementField( DATA4 *data )
   {
      if ( data->autoIncrementField != 0 )
      {
         #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
            double tempDbl ;  /* LY July 10/03 : restored tempDbl */
            memcpy( (char*)&tempDbl, data->dataFile->autoIncrementVal, sizeof(double) ) ;
            f4assignDouble( data->autoIncrementField, tempDbl ) ;
         #else
            f4assignDouble( data->autoIncrementField, data->dataFile->autoIncrementVal ) ;
         #endif
      }
   }



   /* not S4CLIENT, not S4OFF_MULTI, not S4OFF_WRITE, S4FOX, not S4UTILS */
   void d4assignAutoTimestampField( DATA4 *data )
   {
      #ifndef S4MACINTOSH  // LY Aug 13/04
         assert5port( "added autoTimestampField support" ) ;
         // AS Mar 11/03 - support for new feature r4autoTimestamp
         if ( data->autoTimestampField == 0 )
            return ;

         #ifndef S4OFF_TRAN
            // don't update if rolling back a transaction
            if ( code4tranStatus( data->codeBase ) == r4rollback )
               return ;
         #endif

         // get the date/time...
         #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
            assert5port( "may need porting for data alignment" ) ;
         #else
            date4timeNowFull( f4ptr( data->autoTimestampField ) ) ;
         #endif
      #endif
   }
#endif



#if !defined( S4CLIENT ) && !defined( S4OFF_MULTI )
   /* not S4CLIENT, not S4OFF_MULTI, not S4OFF_WRITE */
   int S4FUNCTION d4lockAppendRecord( DATA4 *data, int doUnlock )
   {
      #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
         double tempDbl ;
      #endif

      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E91101 ) ;
      #endif

      int rc = 0 ;

      // AS May 5/03 - Register this earlier - was using without assigning
      int oldUnlock = code4unlockAuto( data->codeBase ) ;
      if ( d4lockTestFile( data ) == 1 )
      {
         // AS Oct 29/02 - don't assign automatically if using the utilities since we need to use the
         // actual source value in that case.
         // #if defined( S4FOX ) && !defined( S4UTILS )
         //    d4assignAutoIncrementField( data ) ;
         // #endif
      }
      else
      {
         // AS Apr 11/03 - There was an issue here that if the caller explicitly requested lockAppend after
         // calling appendStart, the call to d4append() calls this function which will call code4unlock()
         // which is a problem because it will reset the memos.
         // so, basically change this so that if the appendbytes are locked just go ahead and attempt to lock
         // the +1 record (which should be available)
         // thus moved this line up.
         if ( d4lockTestAppend( data ) == 0 )
         {
            if ( doUnlock )
               /* AS 12/19/97 ole-db t5row6.cpp fails if we unlock before checking lock */
               if ( d4lockTestAppend( data ) == 0 || d4lockTest( data, d4recCount( data ) + 1, lock4write ) == 0 )
               {
                  switch( code4unlockAuto( data->codeBase ) )
                  {
                     case LOCK4ALL :
                        code4unlock( data->codeBase ) ;
                        break ;
                     case LOCK4DATA :
                        // AS Apr 15/03 - support for new lockId for shared clone locking
                        rc = d4unlockLow( data, data4lockId( data ), 0 ) ;
                        break ;
                     default:
                        break ;
                  }
               }

            #ifdef S4SERVER
               rc = d4lockAppendInternal( data, 0 ) ;
            #else
               rc = d4lockAppendInternal( data, 1 ) ;
            #endif

            if ( rc == 0 )
            {
               data->dataFile->numRecs = -1 ;
               code4unlockAutoSet( data->codeBase, 0 ) ;
               #ifndef S4OPTIMIZE_OFF
                  data->codeBase->opt.forceCurrent = 1 ;  /* force the recCount to be current */
               #endif
               #ifdef S4SERVER
                  rc = d4lockInternal( data, d4recCount( data ) + 1, 0 ) ;
               #else
                  rc = d4lockInternal( data, d4recCount( data ) + 1, 1 ) ;
               #endif

               #ifndef S4OPTIMIZE_OFF
                  data->codeBase->opt.forceCurrent = 0 ;
               #endif
            }
            // #if defined( S4FOX ) && !defined( S4UTILS )
            //    // AS Sept 27/02 - if file open exclusively to not read value because it has not been updated on disk yet.
            //    if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW )
            //       d4assignAutoIncrementField( data ) ;
            // #endif
         }
         else
         {
            code4unlockAutoSet( data->codeBase, 0 ) ;
            #ifdef S4SERVER
               rc = d4lockInternal( data, d4recCount( data ) + 1, 0 ) ;
            #else
               rc = d4lockInternal( data, d4recCount( data ) + 1, 1 ) ;
            #endif
         }
         if ( rc )
            d4unlockData( data, -1 ) ;

         code4unlockAutoSet( data->codeBase, oldUnlock ) ;

         // #if defined( S4FOX ) && !defined( S4UTILS )
         //    if ( rc == 0 )  // look to see if this is an auto-increment field...
         //       d4assignAutoIncrementField( data ) ;
         // #endif
      }

      return rc ;
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_MULTI ) */



#ifndef S4CLIENT
   /* not S4CLIENT, not S4OFF_WRITE */
   static int dfile4appendData( DATA4FILE *data, const void *record, S4LONG *recNum )
   {
      long count ;
      FILE4LONG pos ;
      int  rc ;

      #ifdef E4PARM_LOW
         if ( data == 0 || record == 0 || recNum == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif

      #ifdef S4OFF_MULTI
         count = dfile4recCount( data, 0L ) ;  /* returns -1 if error4code( codeBase ) < 0 */
      #else
         count = dfile4recCount( data, -2L ) ;  /* returns -1 if error4code( codeBase ) < 0 */
      #endif
      if ( count < 0L )
         return -1 ;
      data->fileChanged = 1 ;
      // AS Aug 15/01 - Code for 'version number' handling so users can know if the file has changed.
      /* LY 2001/08/21 : added ifdef to avoid compile error on non-WIN32 */
      #if defined(S4WIN32) || !defined(S4STAND_ALONE)
         dfile4versionIncrement( data ) ;
      #endif

      pos = dfile4recordPosition( data, count + 1L ) ;

      // AS Nov 12/04 - for encrypted files do not append the eof marker since they don't require since not compatible, and causes length problems
      // AS Apr 19/06 - unsigned short is not large enough (large file support)...
      unsigned long lenWrite = data->recWidth ;

      #ifdef S4FOX
         if ( data->compatibility != 30 )   /* no eof marker */
            lenWrite++ ;
      #else
         #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )  // LY Nov 24/04 : avoid compiler error
            if ( data->file.preprocessed == 0 )
               lenWrite++ ;
         #endif
      #endif

      rc = file4writeInternal( &data->file, pos, record, lenWrite ) ;
      if ( rc == 0 )
      {
         data->numRecs = count + 1L ;
         *recNum = data->numRecs ;
         #ifndef S4OFF_TRAN
            /* 04/24/96 AS --> 2 lines below added so that server datafile has correct
               min count when not a transaction.  t4commit.c */
            if ( code4tranStatus( data->c4 ) != r4active )
               dfile4setMinCount( data, data->numRecs ) ;
         #endif
         /* LY 99/05/12 : set FILE4.isLong when file > 4GB */
         //#ifdef S4FILE_EXTENDED
            /* AS 09/13/99 --> this method is extremely slow, modified to speed up.
               FILE4LONG len = u4filelength( data->file.hand ) ;
               if ( file4longGetHi( len ) > 0 )
                  data->file.isLong = 1 ;
            */
            // AS 10/16/00 - this coding should be done internally else doesn't work for all files
            // if ( (unsigned long)(*recNum) > data->numRecsBeforeFileLong && data->file.isLong == 0 )
            // {
            //    // not precise, may be off by 1 record, so just do a final check...
            //    FILE4LONG len = u4filelength( data->file.hand ) ;
            //    if ( file4longGetHi( len ) > 0 )
            //       data->file.isLong = 1 ;
            // }
         //#endif
      }

      return rc ;
   }
#endif /* !S4CLIENT */



/* not S4OFF_WRITE */
static int d4appendVerifyInputs( DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E91103 ) )
         return -1 ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E91103 ) ;
   #endif

   CODE4 *c4 = data->codeBase ;  // ok to be NULL since we just pass as params to error4 where NULL is ok.

   #ifdef E4ANALYZE
      if ( c4 == 0 )
         return error4( 0, e4struct, E91103 ) ;
   #endif

   #ifdef E4MISC
      if ( data->record[0] != ' ' && data->record[0] != '*' )
         return error4( c4, e4parm, E83301 ) ;
   #endif

   #ifdef S4DEMO
      if ( d4recCount( data ) >= 200L)
      {
         d4close( data ) ;
         return error4( c4, e4demo, 0 ) ;
      }
   #endif

   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( data->readOnly == 1 )
      return error4describe( c4, e4write, E80606, d4alias( data ), 0, 0 ) ;

   #ifdef E4MISC
      if ( data->recNum )
         return error4( c4, e4result, E81103 ) ;
   #endif

   return 0 ;
}



#ifdef S4CLIENT
   static int d4appendBatch( DATA4 *data, long serverId, long clientId )
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
      *((long *)buf) = -1 ;  // indicate we want append
      buf += sizeof( long ) ;
      memcpy( buf, d4record( data ), recWidth ) ;
      for ( int loop = 0 ; loop < data->dataFile->nFieldsMemo ; loop++ )
      {
         MEMO4BATCH_ENTRY *entry = &batch->memos[batch->curWriteBufCount].memos[loop] ;
         F4MEMO *memoField = &data->fieldsMemo[loop] ;
         entry->contentsLen = memoField->len ;
         if ( memoField->len != 0 )
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
         }
      }


      batch->curWriteBufCount++ ;

      #ifdef E4DEBUG
         assert5( d4lockTestAppend( data ) == 1 ) ;   // must be locked for batch processing
      #endif

      long count = d4recCount( data ) ;
      if ( count < 0 )
         return count ;
      data->recNum = count + 1 ;
      d4file->numRecs = data->recNum ;
      if ( code4tranStatus( data->codeBase ) != r4active )
         d4file->minCount = data->recNum ;
      data->recordChanged = 0 ;

      return 0 ;
   }



   static int d4appendRequestFromServer( DATA4 *data, long serverId, long clientId )
   {
      DATA4FILE *d4file = data->dataFile ;
      assert5( d4file != 0 ) ;

      CONNECTION4 *connection = d4file->connection ;
      if ( connection == 0 )
         return e4connection ;

      connection4assign( connection, CON4APPEND, clientId, serverId ) ;
      CONNECTION4APPEND_INFO_IN *appendInfoIn ;
      connection4addData( connection, NULL, sizeof( CONNECTION4APPEND_INFO_IN ), (void **)&appendInfoIn ) ;

      #ifndef S4OFF_MEMO
         for ( int memoFieldCountIndex = 0 ; memoFieldCountIndex < d4file->nFieldsMemo ; memoFieldCountIndex++ )
         {
            F4MEMO *mfield = data->fieldsMemo + memoFieldCountIndex ;
            if ( mfield->len > 0 )
               appendInfoIn->numMemoFields++ ;
         }
         appendInfoIn->numMemoFields = htons5( appendInfoIn->numMemoFields ) ;
      #endif

      connection4addData( connection, data->record, dfile4recWidth( d4file ), NULL ) ;

      #ifndef S4OFF_MEMO
         for ( int memoFieldSendIndex = 0 ; memoFieldSendIndex < d4file->nFieldsMemo ; memoFieldSendIndex++ )
         {
            F4MEMO *mfield = data->fieldsMemo + memoFieldSendIndex ;
            if ( mfield->len > 0 )
            {
               CONNECTION4MEMO *memo ;
               connection4addData( connection, NULL, sizeof( CONNECTION4MEMO ), (void **)&memo ) ;
               memo->fieldNum = htons5( memoFieldSendIndex ) ;
               memo->memoLen = htonl5( mfield->len ) ;
               if ( mfield->len > 0 )
                  connection4addData( connection, mfield->contents, mfield->len, NULL ) ;
            }
         }
      #endif

      CODE4 *c4 = data->codeBase ;

      int rc = connection4repeat( connection ) ;
      if ( rc < 0 )
      {
         // AS 10/29/99 --> was not generating an error as expected...
         // return rc ;
         connection4error( connection, c4, rc, E91103 ) ;
         return rc ;
      }

      if ( connection4len( connection ) != sizeof( CONNECTION4APPEND_INFO_OUT ) )
         return error4( c4, e4packetLen, 91103 ) ;

      CONNECTION4APPEND_INFO_OUT *appendInfoOut = (CONNECTION4APPEND_INFO_OUT *)connection4data( connection ) ;
      data->bofFlag = appendInfoOut->bofFlag ;
      data->eofFlag = appendInfoOut->eofFlag ;
      data->recordChanged = appendInfoOut->recordChanged ;
      data->recNum = ntohl5( appendInfoOut->recNum ) ;
      if ( d4lockTestAppend( data ) == 1 && rc == 0 )
      {
         // AS Jul 3/03 - with recycling of rows it is possible the count does not match the recNum
         // d4file->numRecs = data->recNum ;
         d4file->numRecs = ntohl5( appendInfoOut->recCount ) ;
         if ( code4tranStatus( c4 ) != r4active )
            d4file->minCount = data->recNum ;
      }
      else
         d4file->numRecs = - 1 ;

      // AS 03/05/01 - client code for auto-increment
      if ( data->autoIncrementField != 0 )
         memcpy( f4ptr( data->autoIncrementField ), &(appendInfoOut->autoIncrementVal), sizeof( double ) ) ;

      // AS Sep 5/03 - client/server support for autoTimestamp
      if ( data->autoTimestampField != 0 )
         memcpy( f4ptr( data->autoTimestampField ), &(appendInfoOut->autoTimestampVal), sizeof( double ) ) ;

      rc = connection4status( connection ) ;
      if ( rc != 0 )
      {
         if ( rc < 0 )
            connection4error( connection, c4, rc, E91103 ) ;
         return rc ;
      }
      if ( appendInfoOut->recordLocked  )
         d4localLockSet( data, data->recNum ) ;
      if ( appendInfoOut->appendLocked  )
      {
         // AS Jul 3/03 - with recycling of rows it is possible the count does not match the recNum
         // d4file->numRecs = data->recNum ;
         d4file->numRecs = ntohl5( appendInfoOut->recCount ) ;
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         // d4file->appendLock = data ;
         d4file->appendLockServerId = data4serverId( data ) ;
         d4file->appendLockLockId = data4lockId( data ) ;
      }
      #ifndef S4OFF_MEMO
         for ( int memoFieldResetIndex = 0 ; memoFieldResetIndex < d4file->nFieldsMemo ; memoFieldResetIndex++ )
         {
            F4MEMO *mfield = data->fieldsMemo + memoFieldResetIndex ;
            mfield->isChanged = 0 ;
            #ifdef S4CLIENT
               // AS Sep 2/03 - We want to ensure that if a memo was inserted that we have non-zero for the memo
               // entry in order that EMPTY() will work.
               if ( mfield->len > 0 )
                  f4assignLong( mfield->field, 1 ) ;
            #endif
         }
         // AS Sep 5/03 - was not marking the record as back unchanged causing problems.
         data->recordChanged = 0 ;
      #endif
      return 0 ;
   }
#endif /* S4CLIENT */



#ifndef S4CLIENT

#ifdef S4OFF_MULTI
   #define d4appendUnlock( a, b, c, d ) ( 0 )
#else
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   // AS Apr 15/03 - support for new lockId for shared clone locking
   static void d4appendUnlock( DATA4 *data, Bool5 indexLocked, long serverId, long lockId )
   {
      DATA4FILE *d4file = data->dataFile ;

      #ifndef S4OFF_INDEX
         if ( !indexLocked )
         {
            // AS 09/22/99 speed up in case where no indexes...
            #ifndef S4CLIPPER
               if ( l4numNodes( &d4file->indexes ) != 0 )
            #endif
                  dfile4unlockIndex( d4file, serverId ) ;
         }
      #endif
      // AS Apr 15/03 - d4unlockAppend() now ignores the Code4.unlockAuto, so look at here
      if ( code4unlockAuto( data->codeBase ) != LOCK4OFF )
      {
         #ifdef S4SERVER
            dfile4unlockAppend( d4file, lockId, serverId ) ;
         #else
            d4unlockAppendInternal( data ) ;
         #endif
      }
   }
#endif  /* S4OFF_MULTI else */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_MULTI )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   // AS Apr 15/03 - support for new lockId for shared clone locking
   static int d4appendLockIndexes( DATA4 *data, Bool5 *indexLocked, const long serverId, const long lockId )
   {
      #ifdef S4SERVER
         DATA4FILE *d4file = data->dataFile ;
         *indexLocked = dfile4lockTestIndex( d4file, serverId ) == 1 ;
      #else
         *indexLocked = d4lockTestIndex( data ) == 1 ;
      #endif
      if ( !(*indexLocked) )
      {
         #ifdef S4SERVER
            int rc = dfile4lockIndex( d4file, serverId ) ;
         #else
            int rc = d4lockIndex( data ) ;
         #endif
         if ( rc != 0 )  /* error or r4locked */
            return rc ;
      }

      return 0 ;
   }
#else
   #define d4appendLockIndexes( a, b, c, d ) ( 0 )
#endif /* #if !defined( S4OFF_INDEX ) && !defined( S4OFF_MULTI ) */


#ifdef S4OFF_INDEX
   #define d4appendUpdateIndexesCancel( a, b, c, d ) ( 0 )
#else
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   static void d4appendUpdateIndexesCancel( DATA4 *data, TAG4 *tagOn, long serverId, long clientId )
   {
      #ifdef I4PRINT
         char dump[255];
         sprintf( dump, "d4appendUpdateIndexesCancel called for tag: %s\r\n", tagOn->tagFile->alias ) ;
         log5( dump ) ;
      #endif
      /* Remove the keys which were just added */
      for(;;)
      {
         tagOn = d4tagPrev( data, tagOn ) ;
         if ( !tagOn )
            break ;
         tfile4removeCalc( tagOn->tagFile, data->recNum ) ;
      }

      data->recNum = 0 ;
   }
#endif /* S4OFF_INDEX */



#ifdef S4OFF_INDEX
   #define d4appendUpdateIndexes( a, b, c, d ) ( 0 )
#else
   #ifdef EXCEPTION4REINDEX
      int d4appendRecoverIndexes( DATA4 *data, TAG4 **tagOn, TAG4 **tagFailed, long serverId, long clientId )
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
               // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with block preprocessing
               char *saveRecord = (char *)u4alloc( dfile4recWidth( data->dataFile ) ) ;
               if ( saveRecord != 0 )
               {
                  d4appendUpdateIndexesCancel( data, (*tagOn), serverId, clientId ) ;
                  // recover by reindexing the offending index
                  error4set( data->codeBase, 0 ) ;

                  // we need to save the record which was being appended...
                  // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with block preprocessing
                  memcpy( saveRecord, d4record( data ), dfile4recWidth( data->dataFile ) ) ;
                  rc = i4reindex( (*tagOn)->index ) ;
                  #ifdef E4ANALYZE
                     // let's verify that it is accurate now...
                     if ( rc == 0 )
                     {
                        if ( d4check( data ) != 0 )
                           return e4index ;
                     }
                  #endif
                  // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with block preprocessing
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
   #endif



   static int d4appendUpdateIndexes( DATA4 *data, long serverId, long clientId, Bool5 *indexLocked )
   {
      DATA4FILE *d4file = data->dataFile ;

      #ifdef S4CLIPPER
         Bool5 hasIndexes = ( l4numNodes( &data->indexes ) > 0 ) ? 1 : 0 ;
      #else
         Bool5 hasIndexes = ( l4numNodes( &d4file->indexes ) > 0 ) ? 1 : 0 ;
      #endif

      if ( hasIndexes )
      {
         // AS Apr 15/03 - support for new lockId for shared clone locking
         int rc = d4appendLockIndexes( data, indexLocked, serverId, data4lockId( data ) ) ;
         /* AS 07/23/99 --> r4locked was incorrectly not being caught here... */
         /* if ( rc < 0 ) */
         if ( rc != 0 )
            return rc ;

         #ifdef EXCEPTION4REINDEX
            // keep track of which tag failed to ensure not an endless loop of attempted recovery...
            TAG4 *tagFailed = 0 ;
         #endif

         for( TAG4 *tagOn = 0 ;; )
         {
            tagOn = d4tagNext( data, tagOn ) ;
            if ( !tagOn )
               break ;
            TAG4FILE *t4file = tagOn->tagFile ;

            rc = expr4context( t4file->expr, data ) ;
            if ( rc == 0 )
               if ( t4file->filter != 0 )
                  rc = expr4context( t4file->filter, data ) ;

            #ifdef EXCEPTION4REINDEX
               Bool5 needsReindex = 0 ;
            #endif
            if ( rc == 0 )
            {
               #ifdef EXCEPTION4REINDEX
                  // catch any exception that occurs during an addCalc, and then reindex in that case
                  try
                  {
               #endif
                     rc = t4addCalc( tagOn, data->recNum ) ;
               #ifdef EXCEPTION4REINDEX
                  }
                  catch( ... )
                  {
                     rc = e4index ;
                  }
               #endif
            }

            if ( rc < 0 || rc == r4unique )
            {
               #ifdef EXCEPTION4REINDEX
                  if ( rc == e4index || error4code( data->codeBase ) == e4index )  // attempt recovery
                     rc = d4appendRecoverIndexes( data, &tagOn, &tagFailed, serverId, clientId ) ;
               #endif

               if ( rc < 0 || rc == r4unique )
               {
                  int saveError = error4set( data->codeBase, 0 ) ;
                  d4appendUpdateIndexesCancel( data, tagOn, serverId, clientId ) ;
                  error4set( data->codeBase, (short)saveError ) ;
                  return rc ;
               }
            }
         }
      }

      return 0 ;
   }
#endif /* !S4OFF_INDEX */



#ifdef S4OFF_MEMO
   #define d4appendUpdateMemos( a ) ( 0 )
#else
   static int d4appendUpdateMemos( DATA4 *data )
   {
      DATA4FILE *d4file = data->dataFile ;

      for ( int memoFieldIndex = 0 ; memoFieldIndex < d4file->nFieldsMemo ; memoFieldIndex++ )
      {
         F4MEMO *mfield = data->fieldsMemo + memoFieldIndex ;
         mfield->isChanged = 0 ;
         if ( mfield->len > 0 )
         {
            long newId = 0L ;
            // AS 05/09/01 - Was not returning errors out in this case...
            int rc = memo4fileWrite( &d4file->memoFile, &newId, mfield->contents, mfield->len ) ;
            if ( rc > 0 )  /* positive value means fail, so don't update field, but append record */
               break ;
            else if ( rc < 0 )
               return rc ;
            f4assignLong( mfield->field, newId ) ;
         }
         else
         {
            #ifdef S4FOX
               if ( d4compatibility( data ) == 30 )
               {
                  if ( !f4null( mfield->field ) )  /* if null, then don't assign since that will replace the null flag */
                     f4assignLong( mfield->field, 0 ) ;
               }
               else
                  f4assign( mfield->field, " " ) ;
            #else
               f4assign( mfield->field, " " ) ;
            #endif
         }
      }

      return 0 ;
   }
#endif /* !S4OFF_MEMO */



/* not S4CLIENT, not S4OFF_WRITE */
static int d4appendUpdateData( DATA4 *data )
{
   DATA4FILE *d4file = data->dataFile ;

   data->record[dfile4recWidth( d4file )] = 0x1A ;

   int rc = dfile4appendData( d4file, data->record, (S4LONG *)&data->recNum ) ;
   if ( rc != 0 )
      return rc ;

   data->recordChanged = 0 ;
   data->record[dfile4recWidth( d4file )] = 0 ;

   return 0 ;
}
#endif /* !S4CLIENT */



/* not S4OFF_WRITE */
#ifdef S4CLIENT
   int S4FUNCTION d4append( DATA4 *data )
   {
      int rc = 0 ;
      #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
         double tempDbl ;
      #endif

      /* this is called earlier in non-client case, so not needed here for them...*/
      rc = d4appendVerifyInputs( data ) ;
      if ( rc != 0 )
         return rc ;

      long serverId = data4serverId( data ) ;
      // AS Apr 22/03 - use lockId to allow for shared clone locks.
      long lockId = data4lockId( data ) ;

      // AS Oct 25/05 - support for low level tag operations, we need to reset all the tag pointers in this case...
      for( TAG4 *tagOn = 0 ;; )
      {
         tagOn = d4tagNext( data, tagOn ) ;
         if ( !tagOn )
            break ;
         TAG4FILE *t4file = tagOn->tagFile ;
         t4file->tagDataValid = 0 ;
      }

      // AS Apr 15/02 -support for batch writing/appending...
      // AS Dec Only need to verify that append bytes are locked, not entire file
      if ( data->batchWrite.writeRecsToBuf > 0 && dfile4lockTestAppend( data->dataFile, lockId, serverId ) == 1 )  // buffer writes
         return d4appendBatch( data, serverId, lockId ) ;
      else
         return d4appendRequestFromServer( data, serverId, lockId ) ;
   }
#endif


#ifndef S4CLIENT
   /* !S4OFF_WRITE, !S4CLIENT */
   static int d4doAppend( DATA4 *data )
   {
      int rc = 0 ;
      #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
         double tempDbl ;
      #endif

      long serverId = data4serverId( data ) ;
      long clientId = data4clientId( data ) ;
      // AS Apr 15/03 - support for new lockId for shared clone locking
      long lockId = data4lockId( data ) ;

      /* 1. Update index file
         2. Update memo File
         3. Update data file */

      Bool5 indexLocked = 0 ;
      rc = d4appendUpdateIndexes( data, serverId, clientId, &indexLocked ) ;
      if ( rc != 0 )  // either < 0 or r4unique */
      {
         // AS Apr 15/03 - support for new lockId for shared clone locking
         d4appendUnlock( data, indexLocked, serverId, lockId ) ;
         return rc ;
      }

      rc = d4appendUpdateMemos( data ) ;

      if ( rc == 0 )
         rc = d4appendUpdateData( data ) ;

      if ( rc != 0 )  /* must remove tag entries since the data append failed */
         d4appendUpdateIndexesCancel( data, 0, serverId, clientId ) ;

      if ( rc == 0 )
      {
         /* LY 2003/06/24 : added !S4OFF_MULTI */
         #if defined( S4FOX ) && !defined( S4UTILS ) && !defined( S4OFF_MULTI )
            d4incrementAutoIncrementValue( data ) ;
         #endif
         #ifndef S4OFF_MULTI
            if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW )
               rc = dfile4updateHeader( data->dataFile, 1, 1, 1 ) ;
         #endif  /* S4OFF_MULTI */
      }

      // AS Apr 15/03 - support for new lockId for shared clone locking
      d4appendUnlock( data, indexLocked, serverId, lockId ) ;

      return rc ;
   }
#endif  /* !S4CLIENT */



#if !defined( S4OFF_TRAN ) && !defined( S4CLIENT )
   int d4transEnabled( DATA4 *data, Bool5 checkActiveStatus )
   {
      /*
         Basically, trans is not enabled if phsyically disabled for the file (eg. LOG4OFF for
         internal schema tables), or if transaction processing is not enabled and a transaction
         has not been started.

         in some instances, we don't care about the activeStatus.  For example,
         a pack or zap gets logged everytime no matter what the status.  This
         is important because we don't perform mini-transactions for pack and zap.
      */
      // AS Apr 29/03 - transcations are run-time in odbc now
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( data->codeBase->server->odbcTrans == 0 )  // odbc build, no trans available
            return 0 ;
      #endif

      if ( data->logVal != LOG4OFF )
         if ( code4transEnabled( data->codeBase ) )
         {
            if ( checkActiveStatus == 0 )
               return 1 ;
            if ( code4tranStatus( data->codeBase ) == r4active )
               return 1 ;
         }
      return 0 ;
   }
#endif /*#if !defined( S4OFF_TRAN ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_TRAN ) && !defined( S4CLIENT )
   int d4startMiniTransactionIfRequired( DATA4 *data )
   {
      /*
         Basically, a mini-transaction is required in the case of:  there is no transaction in
         currently in place but the log file is enabled and the log value on the data file
         says to log the file.

         RETURNS:
         1 if a mini transaction was started
         0 if a mini transaction was NOT started
         < 0 if error
      */

      CODE4 *c4 = data->codeBase ;
      int rc ;

      // AS Apr 29/03 - transcations are run-time in odbc now
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
            return 0 ;
      #endif

      if ( data->logVal != LOG4TRANS && data->logVal != LOG4OFF )
      {
         if ( code4transEnabled( c4 ) )
         {
            if ( ( code4tranStatus( c4 ) == r4inactive ) )  /* start a mini-transaction */
            {
               rc = code4tranStartSingle( c4 ) ;
               if ( rc != 0 )
                  return rc ;
               return 1 ;
            }
         }
      }

      return 0 ;
   }
#endif /*#if !defined( S4OFF_TRAN ) && !defined( S4CLIENT ) */



#ifndef S4CLIENT
#ifdef S4OFF_TRAN
   #define d4appendRegisterTransaction( a, b ) ( 0 )
   #define d4appendRegisterTransactionFinish( a, b, c ) ( 0 )
#else
   static int d4appendRegisterTransaction( DATA4 *data, const int hasTran )
   {
      if ( d4transEnabled( data, 1 ) )
      {
         CODE4 *c4 = data->codeBase ;

         TRAN4 *trans = code4trans( c4 ) ;
         long connectionId = code4connectionId( c4 ) ;
         int rc = tran4set( trans, trans->currentTranStatus, -1L, connectionId, TRAN4APPEND,
                        sizeof( data->recNum ) + (unsigned int)dfile4recWidth( data->dataFile ), data4clientId( data ), data4serverId( data ) ) ;
         if ( rc < 0 )
         {
            if ( hasTran )
               code4tranRollbackSingle( c4 ) ;
            return rc ;
         }

         if ( tran4putData( trans, &data->recNum, sizeof( data->recNum ) ) == e4memory )
         {
            if ( hasTran )
               code4tranRollbackSingle( c4 ) ;
            return e4memory ;
         }

         if ( tran4putData( trans, d4record( data ), (unsigned int)dfile4recWidth( data->dataFile ) ) == e4memory )
         {
            if ( hasTran )
               code4tranRollbackSingle( c4 ) ;
            return e4memory ;
         }

         #ifndef S4OFF_MEMO
            #ifdef S4WINCE    /* LY 00/03/13 */
               unsigned long len ;
               memcpy( &len, trans->header.dataLen, sizeof(S4LONG) ) ;
            #else
               unsigned long len = trans->header.dataLen ;
            #endif
            for ( int memoFieldIndex = 0 ; memoFieldIndex < data->dataFile->nFieldsMemo ; memoFieldIndex++ )
            {
               len += ( data->fieldsMemo[memoFieldIndex].len + sizeof( unsigned long ) ) ;
               long tempLong = data->fieldsMemo[memoFieldIndex].len;

               if ( tran4putData( trans, &tempLong, sizeof( unsigned long ) ) == e4memory )
               {
                  if ( hasTran )
                     code4tranRollbackSingle( c4 ) ;
                  return e4memory ;
               }

               if ( data->fieldsMemo[memoFieldIndex].len != 0 )
               {
                  if ( tran4putData( trans, data->fieldsMemo[memoFieldIndex].contents, data->fieldsMemo[memoFieldIndex].len ) == e4memory )
                  {
                     if ( hasTran )
                        code4tranRollbackSingle( c4 ) ;
                     return e4memory ;
                  }
               }
            }
            #ifdef S4WINCE    /* LY 00/03/13 */
               memcpy( trans->header.dataLen, &len, sizeof(S4LONG) ) ;
            #else
               trans->header.dataLen = len ;
            #endif
         #endif

         #ifdef S4OFF_OPTIMIZE
            if ( tran4lowAppend( trans, 0, 1 ) != 0 )
         #else
            if ( tran4lowAppend( trans, 0, ( ( data->dataFile->file.bufferWrites == 1 && data->dataFile->file.doBuffer == 1) ? 0 : 1 ) ) != 0 )
         #endif
         {
            if ( hasTran )
               code4tranRollbackSingle( c4 ) ;
            return e4transAppend ;
         }
      }

      return 0 ;
   }



   // AS Jan 25/06 - Need to also preform a transaction rollback here...was not being done, being left in a transactional state
   static int d4appendRegisterTransactionFinish( DATA4 *data, int rc, int *hasTran )
   {
      if ( rc < 0 || rc == r4unique )
      {
         // AS Jan 25/06 - Need to also preform a transaction rollback here...was not being done, being left in a transactional state
         CODE4 *c4 = data->codeBase ;
         if ( *hasTran )
         {
            code4tranRollbackSingle( c4 ) ;
            // AS Jun 24/04 - need to indicate we don't need to undo the transaction again later...
            *hasTran = 0 ;
         }

         if ( d4transEnabled( data, 1 ) )
         {
            TRAN4 *trans = code4trans( c4 ) ;
            long connectionId = code4connectionId( c4 ) ;
            int saveRc = tran4set( trans, trans->currentTranStatus, -1L, connectionId, TRAN4VOID, 0, data4clientId( data ), data4serverId( data ) ) ;
            if ( saveRc < 0 )
            {
               if ( *hasTran )
                  code4tranRollbackSingle( c4 ) ;
               return saveRc ;
            }
            #ifdef S4OFF_OPTIMIZE
               if ( tran4lowAppend( trans, "\0", 1 ) != 0 )
            #else
               if ( tran4lowAppend( trans, "\0", ( ( data->dataFile->file.bufferWrites == 1 && data->dataFile->file.doBuffer == 1) ? 0 : 1 ) ) != 0 )
            #endif
            {
               if ( *hasTran )
                  code4tranRollbackSingle( c4 ) ;
               return e4transAppend ;
            }
         }
      }

      return 0 ;
   }
#endif /* S4OFF_TRAN else */
#endif /* !S4CLIENT */


#ifndef S4CLIENT
   // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
   #if !defined( S4OFF_INDEX ) && !defined( S4CLIPPER )
      // AS Jun 11/02 - added support for re-using of deleted rows.
      static Bool5 d4appendFindDeletedRow( DATA4 *data )
      {
         // returns true if a record is found
         // if found the record is locked and we are positioned to the record

         TAG4FILE *appendTag = data->dataFile->appendTag ;
         if ( appendTag == 0 )
            return 0 ;

         // don't position data file per se at this point because we don't want to have to reposition back to the
         // previous position
         int rc = tfile4versionCheckFree( appendTag ) ;
         if ( rc != 0 )
            return 0 ;
         rc = tfile4top( appendTag ) ;

         if ( rc != 0 || tfile4eof( appendTag ) == 1 )  // not found
            return 0 ;

         // we will need to copy the existing record data to be actually appended... - to reset if failure, etc.

         CODE4 *c4 = data->codeBase ;

         for ( ;; )
         {
            long recNo = tfile4recNo( appendTag ) ;
            // ensure we keep our existing 'to append' data in the record buffer, load the
            // existing record into the 'old record' buffer
            // AS Oct 18/06 - S4OFF_MEMO fix
            #ifndef S4OFF_MEMO
               // AS Dec 9/05 - was not handling the memo field...
               int loop ;
               for ( loop = 0 ; loop < data->dataFile->nFieldsMemo ; loop++ )
               {
                  F4MEMO *memoField = &data->fieldsMemo[loop] ;
                  memoField->savedMemo = 0 ;  // if by chance it is already stored, then we have the 'blank' data stored, we want to replace this for the read record...
                  // if ( memoField->savedMemo == 0 )
                  {
                     data->useOldMemo = 0 ;
                     f4memoSaveOld( memoField->field, 0 ) ;
                     data->useOldMemo = 1 ;
                  }
               }
            #endif

            char *svRecord = data->record ;
            data->record = data->recordOld ;
            // AS Dec 9/02,. Dec 31/02 - ensure we lock this record, skip it if locked...
            char oldReadLock = c4getReadLock( c4 ) ;
            short oldLockAttempts = c4->lockAttempts ;
            short oldunlockAuto = code4unlockAuto( data->codeBase ) ;
            #ifndef S4OFF_MULTI  // LY Jul 26/04
               code4unlockAutoSet( data->codeBase, LOCK4OFF ) ;    // don't unlock the append bytes...
            #endif
            c4->lockAttempts = 1 ;
            c4setReadLock( c4, 1 ) ;
            rc = d4go( data, recNo ) ;
            code4unlockAutoSet( data->codeBase, oldunlockAuto ) ;    // don't unlock the append bytes...
            c4setReadLock( c4, oldReadLock ) ;
            c4->lockAttempts = oldLockAttempts ;
            // AS Oct 18/06 - S4OFF_MEMO fix
            #ifndef S4OFF_MEMO
               // AS Dec 9/05 - was not handling the memo field...
               data->useOldMemo = 0 ;
               for ( loop = 0 ; loop < data->dataFile->nFieldsMemo ; loop++ )
               {
                  F4MEMO *memoField = &data->fieldsMemo[loop] ;
                  memoField->savedMemo = 1 ;
                  int len = f4memoLen( memoField->field ) ;  // read the old memo in
                  // now we have the old memo in the current contents field and the new memo is in the contentsOld field.
                  // we effectively need to (at least potentiall) swap this data.  Do this by copying the pointers over and
                  // freeing up if they get reallocated.
                  int lenOld = memoField->lenContentsOld ;
                  int lenAllocated = memoField->lenContentsOldAlloc ;
                  void *contents = memoField->contentsOld ;
                  memoField->contentsOld = 0 ;
                  memoField->lenContentsOld = 0 ;
                  memoField->lenContentsOldAlloc = 0 ;
                  memoField->savedMemo = 0 ;
                  f4memoAssignN( memoField->field, (char *)contents, lenOld ) ;
                  if ( memoField->savedMemo == 1 )   // this means it was reallocated, in which case just free up the old data
                  {
                     u4free( contents ) ;
                  }
                  else // need to reassign all the data
                  {
                     assert5( memoField->contentsOld == 0 ) ;
                     assert5( memoField->lenContentsOld == 0 ) ;
                     assert5( memoField->lenContentsOldAlloc == 0 ) ;
                     memoField->contentsOld = contents ;
                     memoField->lenContentsOld = lenOld ;
                     memoField->lenContentsOldAlloc = lenAllocated ;
                  }
               }
            #endif
            // AS Mar 17/03 - Use internal function
            if ( rc == 0 && d4deletedInternal( data ) )   // ensure is still deleted (maybe another user appending at same time got record first)
            {
               data->record = svRecord ;
               data->recordChanged = 1 ;
               return 1 ;
            }

            // AS Mar 31/10 was failing to set the data->record back...if we return or go back up the loop it gets permanently lost
            data->record = svRecord ;

            // problem here... if rc == r4locked, just try another record
            if ( rc != r4locked )  // not locked, assume not available...
               return 0 ;
            rc = tfile4skip( appendTag, 1L ) ;
            if ( rc != 1 || tfile4eof( appendTag ) == 1 )  // not found
               return 0 ;
         }
      }
   #endif



   // AS 11/05/99 -- this function implemented different than cfix project, due to other changes...
   // AS Jan 25/06 - need to allow input hasTran to change...
   static int d4appendLow( DATA4 *data, int *hasTran, long *count )
   {
      data->bofFlag = data->eofFlag = 0 ;
      data->recordChanged = 0 ;
      // AS 08/06/99 -- was not resetting old record # in case of failure...
      long oldRecNum = data->recNum ;

      // AS Jun 11/02 - added support for re-using of deleted rows.  The way this works is that at this
      // point we have locked the append bytes and recNo + 1.  We now check if a deleted row is available
      // for use (happens if a DEL4REUSE tag exists), and if so we also lock that record.  After the append
      // we unlock the recno+1 record which is unused (note that we originally still lock this record
      // because of other functionality in CodeBase that performs to-append locks way before the actual
      // append may actually go through)

      int rc = 0 ;
      // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
      #if !defined( S4OFF_INDEX ) && !defined( S4CLIPPER )
         // find deleted row will lock the row and position the recNum appropriately
         Bool5 recNoAvailable = d4appendFindDeletedRow( data ) ;
         if ( recNoAvailable == 1 )
         {
            // normally in this case we don't need to lock the append bytes, but we do if
            /* LY 2003/06/24 : added !S4OFF_MULTI */
            #if defined( S4FOX ) && !defined( S4UTILS ) && !defined( S4CLIENT ) && !defined( S4OFF_MULTI )
               if ( data->dataFile->autoIncrementSupported )  // lock append header update the auto-increment value...
               {
                  rc = d4lockAppendInternal( data, 0 ) ;
                  if ( rc != 0 )
                     return rc ;

                  data->dataFile->numRecs = d4recCount( data ) ;
                  d4assignAutoIncrementField( data ) ;
               }
               #ifndef S4MACINTOSH  // LY Jul 16/04
                  // AS Jun 3/03 don't require auto-increment for auto-timestamp
                  assert5port( "added autoTimestampField support" ) ;
                  if ( data->dataFile->autoTimestampSupported )  // lock append header update the auto-increment value...
                     d4assignAutoTimestampField( data ) ;
               #endif
            #endif
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc != 0 )  // AS Dec 12/05 - if this fails, we need to reset the data and record...
            {
               data->recordChanged = 0 ;
               data->recNum = 0 ;
            }
            /* LY 2003/06/24 : added !S4OFF_MULTI */
            #if defined( S4FOX ) && !defined( S4UTILS ) && !defined( S4CLIENT ) && !defined( S4OFF_MULTI )
               if ( rc == 0 && data->dataFile->autoIncrementSupported )  // update the auto-increment value...
               {
                  d4incrementAutoIncrementValue( data ) ;
                  #ifndef S4OFF_MULTI
                     if ( rc == 0 )
                        if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW )
                           rc = dfile4updateHeader( data->dataFile, 1, 1, 1 ) ;
                  #endif
               }
            #endif
            // AS Feb 4/10 - We were forgetting to unlock the 'rec+1) record in this case, resulting in only 1 person being able to lock at a time...
            // AS Mar 3/10 - Actually we need to also unlock in the case of a failure to append since we really didn't use the record.
            //if ( rc == 0 )
            //{
               // disable auto-unlock...
               int oldUnlockAuto = code4unlockAuto( data->codeBase ) ;
               code4unlockAutoSet( data->codeBase, LOCK4DATA ) ;
               d4unlockRecord( data, dfile4recCount( data->dataFile, 0L ) + 1 ) ;
               code4unlockAutoSet( data->codeBase, oldUnlockAuto ) ;
            //}
         }
         else
      #endif
      {
         data->recNum = d4recCountAssumeLocked( data ) + 1 ;

         #ifndef S4OFF_MULTI
            *count = data->recNum ;
         #endif

         // AS Apr 10/03 Need to assign the auto-inc and timestamps before registering transaction to ensure can be undone
         /* LY 2003/06/24 : added !S4OFF_MULTI */
         #if defined( S4FOX ) && !defined( S4UTILS ) && !defined( S4OFF_MULTI )
            d4assignAutoIncrementField( data ) ;
            #ifndef S4MACINTOSH  // LY Jul 16/04
               assert5port( "added autoTimestampField support" ) ;
               // AS Mar 11/03 - support for new feature r4autoTimestamp
               d4assignAutoTimestampField( data ) ;
            #endif
         #endif

         rc = d4appendRegisterTransaction( data, *hasTran ) ;

         if ( rc == 0 )
         {
            rc = d4doAppend( data ) ;
         }
         // if ( rc == 0 )
         int saveRc = d4appendRegisterTransactionFinish( data, rc, hasTran ) ;
         if ( saveRc < 0 )
            rc = saveRc ;
      }

      if ( rc != 0 && (*count) != -1 )
         data->recNum = oldRecNum ;

      return rc ;
   }



   /* !S4CLIENT */
   int S4FUNCTION d4append( DATA4 *data )
   {
      // AS note, similar to d4appendOledb()
      int rc = d4appendVerifyInputs( data ) ;
      #ifndef S4OFF_MULTI
         Bool5 doUnlock = 0 ;
      #endif
      long count = -1 ;
      if ( rc < 0 )
         return rc ;
      #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
         double tempDbl ;
      #endif

      #ifdef S4OFF_TRAN
         int hasTran = 0 ;
      #else
         int hasTran = d4startMiniTransactionIfRequired( data ) ;
         if ( hasTran < 0 )
            return hasTran ;
      #endif

      #ifdef S4OFF_MULTI
      //    #if defined( S4FOX ) && !defined( S4UTILS )
      //       d4assignAutoIncrementField( data ) ;
      //    #endif
      #else
         // AS Oct 30/02 - This is not required in the SERVER as D4append locks the record first
         #ifndef S4SERVER
            rc = d4lockAppendRecord( data, 1 ) ;
         #endif
      #endif  /* S4OFF_MULTI */

      // AS Feb 1/06 - changed the hasTran slightly.  It may get reset if the transaction is rolled back.  But, esp. in server,
      // we need to still do unlock
      int hadTran = hasTran ;

      if ( rc == 0 )
      {
         rc = d4appendLow( data, &hasTran, &count ) ;
         if ( rc != 0 )
         {
            #ifndef S4OFF_MULTI
                /* AS 03/17/99 was not unlocking in case of r4unique (t4fail.c) */
                /* 08/10/99 not unlocking if other rc's (eg. r4locked)... */
               // if ( count != -1 && ( rc < 0 || rc == r4unique ) )   /* append failed, so unlock record */
               if ( count != -1 )   /* append failed, so unlock record */
                  doUnlock = 1 ;  // have to undo transaction before can unlock...
            #endif
         }
      }

      #ifndef S4OFF_TRAN
         // AS 05/09/01 - if rc < 0, flow error out, don't commit (rollback perhaps, but not commit)
         // AS Feb 1/06 - changed the hasTran slightly.  It may get reset if the transaction is rolled back.  But, esp. in server,
         // we need to still do unlock
         if ( hadTran && rc >= 0 )
         {
            if ( hasTran )
               code4tranCommitSingle( data->codeBase ) ;
            #ifndef S4OFF_MULTI
               if ( code4unlockAuto( data->codeBase ) != LOCK4OFF )
               {
                  #ifdef S4SERVER
                     // AS Apr 15/03 - support for new lockId for shared clone locking
                     int saveRc = dfile4unlockAppend( data->dataFile, data4lockId( data ), data4serverId( data ) ) ;
                  #else
                     int saveRc = d4unlockAppendInternal( data ) ;
                  #endif
                  #ifndef S4OFF_INDEX
                     if ( saveRc == 0 )
                        saveRc = dfile4unlockIndex( data->dataFile, data4serverId( data ) ) ;
                  #endif
                  if ( saveRc < 0 )
                     return saveRc ;
               }
            #endif /* S4OFF_MULTI */
         }
      #endif

      #ifndef S4OFF_MULTI
         // can unlock now after mini-transaction has been finished...
         if ( doUnlock == 1 )
         {
            assert5( count != -1 ) ;   // if -1, should not have set doUnlock on...
            d4unlockRecord( data, count ) ;
         }
      #endif

      return rc ;
   }



   int S4FUNCTION d4appendOledb( DATA4 *data )
   {
      // AS note, similar to d4append()

      // used internally by d4append() and by OLE-DB where the locking
      // is done independent of the append operation... for a performance
      // boost when appending records, avoids double locking.
      #ifdef S4OFF_TRAN
         int hasTran = 0 ;
      #else
         int hasTran = d4startMiniTransactionIfRequired( data ) ;
         if ( hasTran < 0 )
            return hasTran ;
      #endif

      long count = -1 ; // AS 11/05/99 used as part of locking handling, also requires memory for d4appendLow internals
      int rc = d4appendLow( data, &hasTran, &count ) ;

      #ifndef S4OFF_TRAN
         // AS 02/01/00 - should be committing if rc == 0, not != 0!
         // if ( rc != 0 && hasTran )
         if ( rc == 0 && hasTran )
            code4tranCommitSingle( data->codeBase ) ;
      #endif

      return rc ;
   }
#endif /* !S4CLIENT */



/* not S4OFF_WRITE */
int S4FUNCTION d4appendBlank( DATA4 *data )
{
   int rc ;

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E91104 ) )
         return -1 ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E91104 ) ;
   #endif

   #ifdef S4DEMO
      if ( d4recCount( data ) >= 200L)
      {
         d4close( data ) ;
         return error4( data->codeBase, e4demo, 0 ) ;
      }
   #endif

   rc = d4appendStart( data, 0 ) ;  /* updates the record, returns -1 if error4code( codeBase ) < 0 */
   if ( rc )
      return rc ;

   d4blank( data ) ;   /* make sure goes through f4blank() for non-character field types */
   return d4append( data ) ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
/* not S4OFF_WRITE */
short S4FUNCTION d4appendStartLow( DATA4 *data, short useMemoEntries )
{
   // used to do appendStart for OLE-DB and other low-level internal...
   // does not do unlocking of record...

   CODE4 *c4 = data->codeBase ;
   assert5( c4 != 0 ) ;

   #ifdef S4DEMO
      if ( d4recCount( data ) >= 200L)
      {
         d4close( data ) ;
         return error4( c4, e4demo, 0 ) ;
      }
   #endif

   #ifdef S4SERVER
      /* the client performs these operations seperately through d4appendStart
         so the record should never be marked as changed at this point
         - no longer true for java */
      #ifdef E4ANALYZE
         if ( useMemoEntries != 0 )
            return error4( c4, e4info, E91107 ) ;
      #endif
      data->recNum = 0 ;
      #ifndef S4OFF_MEMO
         for ( int memoFieldIndex = 0 ; memoFieldIndex < data->dataFile->nFieldsMemo ; memoFieldIndex++ )
            f4memoReset( data->fieldsMemo[memoFieldIndex].field ) ;
      #endif
   #else
      int rc = d4updateRecord( data, 1, 1 ) ;   /* returns -1 if error4code( c4 ) < 0 */
      if ( rc )
         return rc ;

      #ifndef S4OFF_MEMO
         if ( data->dataFile->nFieldsMemo != 0 )
         {
            for ( int memoFieldIndex = 0 ; memoFieldIndex < data->dataFile->nFieldsMemo; memoFieldIndex++ )
               f4memoReset( data->fieldsMemo[memoFieldIndex].field ) ;

            if ( useMemoEntries == 1 )
            {
               if ( data->recNum > 0 && !d4eof( data ) && !d4bof( data ) )
               {
                  #ifndef S4CLIENT
                     #ifdef E4ANALYZE
                        if ( !file4openTest( &data->dataFile->memoFile.file ) )
                           return error4( c4, e4data, E81101 ) ;
                     #endif
                  #endif

                  /* Read in the current memo entries of the current record */
                  #ifndef S4OFF_MULTI
                     rc = d4lockInternal( data, data->recNum, 1 ) ;
                     if ( rc )
                        return rc ;
                  #endif  /* S4OFF_MULTI */

                  char *savePtr = data->record ;
                  data->record = data->recordOld ;

                  #ifdef S4CLIENT
                     d4go( data, data->recNum ) ;
                  #else
                     d4goData( data, data->recNum ) ;
                  #endif

                  for ( int memoFieldIndex = 0 ; memoFieldIndex < data->dataFile->nFieldsMemo ; memoFieldIndex++ )
                  {
                     #ifdef S4CLIENT
                        f4memoRead( data->fieldsMemo[memoFieldIndex].field ) ;
                     #else
                        f4memoReadLow( data->fieldsMemo[memoFieldIndex].field ) ;
                     #endif
                     data->fieldsMemo[memoFieldIndex].status = 0 ;
                  }

                  data->record = savePtr ;

                  if ( error4code( c4 ) < 0 )
                     return error4stack( c4, error4code( c4 ), E91107 ) ;
               }
            }
         }

         int oldLockEnforce = c4->lockEnforce ;
         c4->lockEnforce = 0 ;
         /* no matter what, must clear the memo entries from the data file */
         for ( int memoIndex = 0 ; memoIndex < data->dataFile->nFieldsMemo ; memoIndex++ )
            f4assignLong( data->fieldsMemo[memoIndex].field, 0 ) ;
         // AS Sep 5/03 - was not marking the record as back unchanged causing problems.
         data->recordChanged = 0 ;
         c4->lockEnforce = oldLockEnforce ;
      #endif  /* not S4OFF_MEMO */

      data->recNum = 0 ;
   #endif  /* S4SERVER */

   return 0 ;
}



short S4FUNCTION d4appendStart( DATA4 *data, short useMemoEntries )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E91107 ) )
         return -1 ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E91107 ) ;
   #endif

   int rc = d4appendStartLow( data, useMemoEntries ) ;

   #if defined( S4STAND_ALONE ) && !defined( S4OFF_MULTI )
      if ( d4lockTestFile( data ) != 1 )
      {
         // AS 01/24/00 -- if rc != 0 already, was overwriting here...
         int rc2 = d4unlockData( data ) ;
         if ( rc == 0 )
            rc = rc2 ;
      }
   #endif

   return rc ;
}



#ifndef S4CLIENT
   int S4FUNCTION D4appendDirect( DATA4 S4PTR *data, char S4PTR *buffer )
   {
      // currently only used by ODBC driver.  Appends the record direct to disk.  Avoids any locking.  Uses the input
      // buffer to do the write.

      #ifndef S4OFF_SECURITY
         if ( account4userAllowAppend( &data->codeBase->currentClient->account, data ) == FALSE )
            return e4authorize ;
      #endif

      char *recordBuf = data->record ;
      data->record = buffer ;
      data->recNum = 0 ;

      int rc = d4append( data ) ;

      data->record = recordBuf ;

      return rc ;
   }



   #ifndef S4OFF_TRAN
      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      /* not S4OFF_TRAN, not S4CLIENT, not S4OFF_WRITE */
      static int d4unappendData( DATA4 *dataIn )
      {
         long count ;
         FILE4LONG pos ;
         int  rc ;
         char endMark ;
         DATA4FILE *data = dataIn->dataFile ;

         #ifdef E4PARM_LOW
            if ( data == 0 )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         count = dfile4recCount( data, data4serverId( dataIn ) ) ;
         if ( count < 0L )
            return error4stack( data->c4, (int)count, E91102 ) ;
         data->fileChanged = 1 ;
         // AS Aug 15/01 - Code for 'version number' handling so users can know if the file has changed.
         #if defined(S4WIN32) || !defined(S4STAND_ALONE) /* LY July 10/03 : not defined on UNIX */
            dfile4versionIncrement( data ) ;
         #endif

         pos = dfile4recordPosition( data, count + 1 ) ;

         rc = file4lenSetLow( &data->file, pos ) ;
         #ifdef S4FOX
            // AS May 24/02 - 3.0 files do not write the extra eof byte
            if ( d4compatibility( dataIn ) != 30 )  /* 3.0 file */
         #endif
         {
            endMark = 0x1A;
            rc = file4writeInternal( &data->file, pos, &endMark, 1 ) ;
         }

         if ( rc == 0 )
         {
            #ifdef S4OFF_MULTI
               data->numRecs = count ;
            #else
               rc = d4lockTestAppend( dataIn ) ;
               if ( rc == 1 )
               {
                  data->numRecs = count ;
                  rc = 0 ;
               }
            #endif  /* S4OFF_MULTI */
         }

         return rc ;
      }



      /* not S4OFF_TRAN, not S4CLIENT, not S4OFF_WRITE */
      int S4FUNCTION d4unappend( DATA4 *data )
      {
         int rc, saveError ;
         CODE4 *c4 ;
         #ifndef S4OFF_INDEX
            TAG4 *tagOn ;
            #ifndef S4OFF_MULTI
               int indexLocked ;
            #endif
         #endif
         #ifndef S4OFF_MEMO
            int i ;
         #endif

         #ifdef I4PRINT
            char dump[255];
            sprintf( dump, "d4unappend called\r\n" ) ;
            log5( dump ) ;
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( data, 2, E91108 ) )
               return -1 ;
         #endif  /* E4VBASIC */

         #ifdef E4PARM_LOW
            if ( data == 0 )
               return error4( 0, e4parm_null, E91108 ) ;
         #endif

         c4 = data->codeBase ;
         rc = 0 ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         /* 0. Lock record count bytes
            1. Update index file
            2. Update memo File
            3. Update data file */

         #ifndef S4OFF_MULTI
            #ifdef S4SERVER
               #ifdef E4MISC
                  if ( d4lockTestAppend( data ) != 1 )
                     return error4( c4, e4struct, E91108 ) ;
                  if ( d4lockTest( data, data->dataFile->numRecs, lock4write ) != 1 )
                     return error4( c4, e4struct, E91108 ) ;
               #endif
            #else
               d4lockAppendInternal( data, 0 ) ;
               d4lockInternal( data, data->dataFile->numRecs, 0 ) ;
            #endif
         #endif  /* S4OFF_MULTI */

         data->bofFlag = data->eofFlag = 0 ;
         data->recordChanged = 1 ;

         data->recNum = d4recCount( data ) ;
         if ( data->recNum < 0 )
            return (int)data->recNum ;
         data->count = data->recNum - 1 ; /* reset the count approximator */
         data->dataFile->numRecs = data->count ;   /* put back in for t4comit3...double append/unappend requires */
         saveError = 0 ;

         #ifndef S4OFF_INDEX
            #ifndef S4OFF_MULTI
               #ifdef S4SERVER
                  // AS Apr 25/03 - don't use undefined tran4serverDataId
                  // indexLocked = dfile4lockTestIndex( data->dataFile, tran4serverDataId( data->trans ) ) == 1 ;
                  indexLocked = dfile4lockTestIndex( data->dataFile, data4serverId( data ) ) == 1 ;
                  if ( !indexLocked )
                  {
                     rc = dfile4lockIndex( data->dataFile, data4serverId( data ) ) ;
                     if ( rc )
                     {
                        dfile4unlockAppend( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
                        return rc ;
                     }
                  }
               #else
                  indexLocked = d4lockTestIndex( data ) == 1 ;
                  if ( !indexLocked )
                  {
                     rc = d4lockIndex( data ) ;
                     if ( rc )
                     {
                        // AS Apr 15/03 - support for new lockId for shared clone locking
                        d4unlockAppendInternal( data ) ;
                        return rc ;
                     }
                  }
               #endif /* S4SERVER */
            #endif  /* not S4OFF_MULTI */

            for( tagOn = 0 ;; )
            {
               tagOn = d4tagNext( data, tagOn ) ;
               if ( !tagOn )
                  break ;

               rc = expr4context( tagOn->tagFile->expr, data ) ;
               if ( rc == 0 )
                  if ( tagOn->tagFile->filter != 0 )
                     rc = expr4context( tagOn->tagFile->filter, data ) ;

               if ( rc == 0 )
                  rc = tfile4removeCalc( tagOn->tagFile, data->recNum ) ;
               if ( rc < 0 )
                  saveError = error4set( c4, 0 ) ;
            }

            #ifndef S4OFF_MULTI
               // AS Apr 25/03 - don't use undefined tran4serverDataId
               if ( !indexLocked )
                  // dfile4unlockIndex( data->dataFile, tran4serverDataId( data->trans ) ) ;
                  dfile4unlockIndex( data->dataFile, data4serverId( data ) ) ;
            #endif
         #endif  /* not S4OFF_INDEX */

         #ifndef S4OFF_MEMO
            if ( data->dataFile->nFieldsMemo > 0 )
            {
               #ifndef S4OFF_MULTI
                  if ( ( rc = d4validateMemoIds( data ) ) != 0 )
                  {
                     data->recordChanged = 0 ;
                     return rc ;
                  }
               #endif

               /* Cycle through the fields to be flushed */
               for ( i = 0; i < data->dataFile->nFieldsMemo; i++ )
               {
                  f4memoAssignN( data->fieldsMemo[i].field, "\0", 0 ) ;
                  rc = f4memoUpdate( data->fieldsMemo[i].field ) ;
                  if ( rc < 0 )
                     saveError = error4set( c4, 0 ) ;
               }
            }
         #endif  /* S4OFF_MEMO */

         data->record[dfile4recWidth( data->dataFile )] = 0x1A ;

         #ifndef S4OFF_MULTI
            rc = d4unappendData( data ) ;
         #endif

         #ifndef S4OFF_MULTI
            if ( rc == 0 )
               if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW )
                  rc = dfile4updateHeader( data->dataFile, 1, 1, 0 ) ;
         #endif

         // AS Apr 16/03 - We unlock after rolling back (except if in utils)
         #if defined( S4UTILS ) && !defined( S4OFF_MULTI )
            #ifdef S4SERVER
               // AS Apr 25/03 - don't use undefined tran4serverDataId
               dfile4unlockAppend( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
            #else
               d4unlockAppend( data ) ;
            #endif
         #endif

         error4set( c4, (short)saveError ) ;

         /* reset to an invalid position */
         data->recNum = data->recNumOld = -1 ;
         data->recordChanged = 0 ;
         data->bofFlag = data->eofFlag = 0 ;

         d4blankLow( data, data->record ) ;

         return rc ;
      }
   #endif  /* !S4OFF_TRAN */
#endif  /* !S4CLIENT */
#endif  /* !S4OFF_WRITE */
