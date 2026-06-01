/* d4pack.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE

// AS Jun 30/03 - support d4packWithProgress
#if defined( TIME4STATUS ) && !defined( S4OFF_WRITE ) && defined( S4WIN32 )
   #include <process.h>
#endif

#ifdef S4WIN32
   // AS Feb 9/06 - added clipper support for packwithstatus
   #if defined( TIME4STATUS )
      static void d4packStatusInit( REINDEX4STATUS *reindexStatus, DATA4 *data )
      {
         reindexStatus->c4 = data->codeBase ;
         reindexStatus->numTags = 1 ;  // count d4pack as == 1 tag for reindexing
         for ( TAG4 *tagOn = 0 ;; reindexStatus->numTags++ )
         {
            tagOn = d4tagNext( data, tagOn ) ;
            if ( tagOn == 0 )
               break ;
         }
         reindexStatus->tagOn = 0 ;
         reindexStatus->c4->percentDone = 0.01 / reindexStatus->numTags ;  // a basic starting quantity based on number tags
         reindexStatus->c4->incrementWeight = .95 / reindexStatus->numTags ;
         reindexStatus->c4->incrementVal = 0 ;
         // we go through each record approx. twice - once while reading, once while writing
         // LY Dec 9/04 : have incrementMax = 1 if rec count == 0 (avoid divide by zero error)
         reindexStatus->c4->incrementMax = ( 2 * d4recCount( data ) > 1 ? 2 * d4recCount( data ) : 1 ) ;
         reindexStatus->c4->actionCode = ACTION4REINDEX ;  // CS 2000/01/16 move this line down
         reindexStatus->c4->packStatus = reindexStatus ;
      }



      static void d4packStatusInitAfterPack( REINDEX4STATUS *reindexStatus, DATA4 *data )
      {
         // after pack but before reindex, the # of records has changed so update value
         reindexStatus->c4->incrementMax = 2 * d4recCount( data ) ;
      }



      static void d4packStatusFinalSet( REINDEX4STATUS *reindexStatus, double val )
      {
         reindexStatus->c4->packStatus = 0 ;
      }

      // AS Jun 30/03 - support d4packWithProgress
      void __cdecl d4packMonitor(void *callbackInfo)
      {
         // This function runs in a thread and polls
         // the CODE4 at certain intervals for pack status
         // and calls the callback function.
         #ifdef S4CLIENT
            int rc ;
            CODE4 c4 ;
            CODE4 *cb = &c4 ;
            CODE4 *cbReindex = ((REINDEX4CALLBACK*)callbackInfo)->data->codeBase ;
         #else
            CODE4 *cb = ((REINDEX4CALLBACK*)callbackInfo)->data->codeBase ;
         #endif
         short( __stdcall *callback )( double ) = ((REINDEX4CALLBACK*)callbackInfo)->callback ;
         long sleepInterval = ((REINDEX4CALLBACK*)callbackInfo)->sleepInterval ;
         short *reindexDone = &( ((REINDEX4CALLBACK*)callbackInfo)->reindexDone ) ;
         short *callbackStarted = &( ((REINDEX4CALLBACK*)callbackInfo)->callbackStarted ) ;

         #ifdef S4CLIENT  // in client/server, the server is polled with a separate connection
            rc = code4init(cb);
            // AS Apr 13/06 - also may need to send the application stamp...
            if ( cbReindex->applicationVerify != 0 )
               code4verifySet( cb, cbReindex->applicationVerify ) ;
            if (rc == r4success)
            {
               // AS Aug 28/06 - wrong settings for server
               rc = code4connect(cb, cbReindex->defaultServer.serverName, cbReindex->defaultServer.port, cbReindex->defaultServer.userName, cbReindex->defaultServer.password, DEF4PROTOCOL);
               if ( rc != 0 )  // couldn't connect
               {
                  *callbackStarted = 1 ;  // indicate we can continue anyway
                  return ;
               }
            }
            else
               cb = 0 ;
         #endif

         *callbackStarted = 1 ;  // tell the calling process that this thread has started

         #ifdef S4CLIENT  // if the 2nd connection could not be established, send rc to callback
            if (rc != r4success)
            {
               callback( (double)rc ) ;
               if (cb)
               {
                  code4initUndo(cb);
                  cb = 0;
               }
            }
         #endif

         #ifdef S4STAND_ALONE  // wait for reindex to start
            while ( cb->actionCode != ACTION4REINDEX && !(*reindexDone) )
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( cb ) ;
            }
         #endif

         double percent ;
         while ( ! (*reindexDone) )
         {
            if (cb)
            {
               percent = code4status( cb ) ;
               if ( percent < 1.0 )
                  callback( percent );
            }
            else  // if C/S connection could not be est, send 0% to callback at given interval
               callback( 0.0 );
            Sleep( sleepInterval );
         }
         callback( 1.0 );

         u4free( callbackInfo ) ;
         #ifdef S4CLIENT
            if (cb)
               code4initUndo( cb ) ;
         #endif
      }

      // AS Jun 30/03 - support d4packWithProgress
      void __cdecl d4packThread( void *info )
      {
         // 2001/01/15
         // This function here so that d4pack can be called as a thread.

         short *reindexDone = &( ((REINDEX4CALLBACK*)info)->reindexDone ) ;
         DATA4 *data = ((REINDEX4CALLBACK*)info)->data ;

         #ifdef S4CLIENT
            short *callbackStarted = &( ((REINDEX4CALLBACK*)info)->callbackStarted ) ;

            while ( *callbackStarted == 0 )  // wait for callback thread to connect to the server
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( data->codeBase ) ;
            }
         #endif
         d4pack( data ) ;
         *reindexDone = 1;
      }
   #endif //#if defined( TIME4STATUS )



   // AS Jun 30/03 - support d4packWithProgress
   short S4FUNCTION d4packWithProgress( DATA4 S4PTR *d4, REINDEX_CALLBACK callback, long milliseconds )
   {
      // AS Feb 9/06 - added clipper support for packwithstatus
      #if defined( TIME4STATUS )
         if ( callback == 0 )
            return d4pack( d4 ) ;

         #ifdef E4PARM_HIGH
            if ( d4 == 0 )
               return error4( 0, e4parmNull, E93004 ) ;
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( d4, 2, E93004 ) )
               return -1 ;
         #endif

         CODE4 *c4 = d4->codeBase ;

         #ifdef S4OFF_WRITE
            return error4( c4, e4notWrite, E93004 ) ;
         #else
            if ( milliseconds <= 0 )
               return error4(c4, e4parm, E93004 ) ;

            REINDEX4CALLBACK *r4info = (REINDEX4CALLBACK*)u4alloc( sizeof( REINDEX4CALLBACK ) ) ;
            if (!r4info)
               return error4( c4, e4memory, E93004 ) ;

            c4->actionCode = ACTION4INITIALIZING;

            r4info->data = d4 ;
            r4info->callback = callback ;
            r4info->sleepInterval = milliseconds ;
            r4info->callbackStarted = 0 ;
            r4info->reindexDone = 0 ;

            // begin thread to poll CodeBase status
            if ( _beginthread( d4packMonitor, 0, (void *)r4info ) == -1 )
               return error4( c4, e4result, E93004 ) ;

            // begin thread to call d4pack
            if ( _beginthread( d4packThread, 0, (void *)r4info ) == -1 )
               return error4(c4, e4result, E93004 ) ;

            while (r4info->callbackStarted == 0 || c4->actionCode == ACTION4NONE)
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( c4 ) ;
            }
         #endif  // !OFF_WRITE
      #else  // TIME4STATUS
         callback(0.0);
         int rc = d4pack( d4 ) ;
         callback(1.0);
         return rc;
      #endif

      return r4success ;
   }
#endif  // S4WIN32



int S4FUNCTION d4pack( DATA4 *d4 )
{
   #ifdef S4CLIENT
      int rc ;
      CONNECTION4PACK_INFO_OUT *out ;
      CONNECTION4 *connection ;
   #else
      int rc ;
   #endif
   CODE4 *c4 ;

   #ifdef E4VBASIC
      if ( c4parm_check( d4, 2, E94601 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( d4 == 0 )
         return error4( 0, e4parm_null, E94601 ) ;
   #endif

   c4 = d4->codeBase ;

   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( d4->readOnly == 1 )
      return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;

   #ifdef S4CLIENT
      rc = d4update( d4 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc )
         return rc ;

      // AS Nov 1/05 - keep track of additional information for the tag...low level tag functions
      d4tagInvalidateAll( d4 ) ;

      // Apr 25/02 - ensure batched writes get flushed first
      code4writeBufferFlush( c4 ) ;
      if ( error4code( c4 ) < 0 )  // check if write buffer flush returned an error
         return error4code( c4 ) ;

      connection = d4->dataFile->connection ;
      if ( connection == 0 )
         return error4stack( c4, e4connection, E94601 ) ;

      d4->count = -1 ;
      d4->dataFile->numRecs = -1 ;
      connection4assign( connection, CON4PACK, data4clientId( d4 ), data4serverId( d4 ) ) ;
      rc = connection4repeat( connection ) ;
      if ( rc == r4locked )
         return r4locked ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E94601 ) ;

      if ( connection4len( connection ) != sizeof( CONNECTION4PACK_INFO_OUT ) )
         return error4( c4, e4packetLen, E94601 ) ;
      out = (CONNECTION4PACK_INFO_OUT *)connection4data( connection ) ;
      if ( out->lockedDatafile )
      {
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         // d4->dataFile->fileLock = d4 ;
         d4->dataFile->fileLockServerId = data4serverId( d4 ) ;
         d4->dataFile->fileLockLockId = data4lockId( d4 ) ;
      }

      d4->recNum = -1 ;
      d4->recNumOld = -1 ;
      // AS Jun 28/02 - not doing a proper blank - sometimes null data is present
      // memset( d4->record, ' ', dfile4recWidth( d4->dataFile ) ) ;
      d4blankLow( d4, d4->record ) ;

      return rc ;
   #else
      #ifndef S4SINGLE
         rc = d4lockAllInternal( d4, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc )
            return rc ;
      #endif

      #ifndef S4OFF_TRAN
         if ( d4transEnabled( d4, 0 ) )   // always log if data logging is on, not just if 'active'
            if ( tran4active( d4->codeBase, d4 ) != 0 )
               return error4( c4, e4transViolation, E81518 ) ;
      #endif

      // AS Feb 9/06 - added clipper support for packwithstatus
      #if defined( TIME4STATUS ) && !defined( S4OFF_WRITE )
         REINDEX4STATUS packStatus ;
         d4packStatusInit( &packStatus, d4 ) ;
      #endif

      rc = d4packData( d4 ) ;  /* returns -1 if error4code( codeBase ) < 0 */
      #if defined( TIME4STATUS ) && !defined( S4OFF_WRITE )
         d4packStatusInit( &packStatus, d4 ) ;
         r4reindexStatusNextTag( &packStatus  ) ;
      #endif
      if ( rc == 0 )
      {
         if ( d4recCount( d4 ) == 0 )
            d4->bofFlag = d4->eofFlag = 1 ;
         else
            d4->bofFlag = d4->eofFlag = 0 ;
         #ifndef S4INDEX_OFF
            rc = d4reindex( d4 ) ;
         #endif
         if ( rc == 0 )
            dfile4updateHeader( d4->dataFile, 1, 1, 0 ) ;
      }
      #if defined( TIME4STATUS ) && !defined( S4OFF_WRITE )
         d4packStatusFinalSet( &packStatus, .955 ) ;
      #endif

      return rc ;
   #endif  /* S4CLIENT */
}



#ifndef S4CLIENT
   /* AS Nov 13/02 - export for dot */
   int S4FUNCTION d4packData( DATA4 *d4 )
   {
      int rc ;
      #ifndef S4CLIENT
         #ifndef S4OFF_TRAN
            TRAN4 *trans ;
            long connectionId ;
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E94602 ) ;
      #endif

      rc = d4update( d4 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc )
         return rc ;

      #ifndef S4SINGLE
         rc = d4lockFileInternal( d4, 1 ) ;
         if ( rc )
            return rc ;
      #endif

      #ifndef S4CLIENT
         #ifndef S4OFF_TRAN
            // AS 02/16/01 - base on whether the data file is being logged, not transaction logging
            // if ( code4transEnabled( data->codeBase ) )
            if ( d4transEnabled( d4, 0 ) )   // always log if data logging is on, not just if 'active'
            {
               trans = code4trans( d4->codeBase ) ;
               #ifdef S4STAND_ALONE
                  connectionId = 0L ;
               #else
                  connectionId = d4->codeBase->currentClient->id ;
               #endif
               if (  tran4set( trans, trans->currentTranStatus, -1L, connectionId, TRAN4PACK,
                    0, data4clientId( d4 ), data4serverId( d4 ) ) == 0 )
                  tran4lowAppend( trans, "\0", 1 ) ;
            }
         #endif
      #endif

      rc = dfile4packData( d4->dataFile ) ;

      d4->recNum = -1 ;
      d4->recNumOld = -1 ;
      // AS Jun 28/02 - not doing a proper blank - sometimes null data is present
      // c4memset( d4->record, ' ', dfile4recWidth( d4->dataFile ) ) ;
      d4blankLow( d4, d4->record ) ;

      return rc ;
   }



   int dfile4packData( DATA4FILE *d4 )
   {
      char *rdBuf, *wrBuf, *record ;
      FILE4SEQ_READ   rd ;
      FILE4SEQ_WRITE  wr ;
      long newCount, curCount, iRec ;
      int  rc ;
      unsigned bufSize ;
      CODE4 *c4 ;
      FILE4LONG len ;

      #ifdef E4PARM_LOW
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif

      c4 = d4->c4 ;

      d4->fileChanged = 1 ;
      // AS Aug 15/01 - Code for 'version number' handling so users can know if the file has changed.
      /* LY 2001/08/21 : added ifdef to avoid compile error on non-WIN32 */
      #if defined(S4WIN32) || !defined(S4STAND_ALONE)
         dfile4versionIncrement( d4 ) ;
      #endif

      rdBuf = wrBuf = 0 ;
      bufSize = c4->memSizeBuffer ;

      for ( ; bufSize > d4->recWidth; bufSize -= 0x800 )
      {
         rdBuf = (char *)u4allocFree( c4, (long)bufSize ) ;
         if ( rdBuf == 0 )
            continue ;

         wrBuf = (char *)u4allocFree( c4, (long)bufSize ) ;
         if ( wrBuf )
            break ;

         u4free( rdBuf ) ;
         rdBuf = 0 ;
      }

      newCount = 0L ;
      curCount = dfile4recCount( d4, -2L ) ;

      record = (char *)u4allocFree( c4, (long)d4->recWidth ) ;
      if ( record == 0 )
         return -1 ;

      file4seqReadInitDo( &rd, &d4->file, dfile4recordPosition( d4, 1L ), rdBuf, ( rdBuf == 0 ) ? 0 : bufSize, 1 ) ;
      file4seqWriteInitLow( &wr, &d4->file, dfile4recordPosition( d4, 1L ), wrBuf, ( wrBuf == 0 ) ? 0 : bufSize) ;

      for ( iRec= 1L; iRec <= curCount; iRec++ )
      {
         file4seqReadAll( &rd, record, d4->recWidth ) ;
         #if defined( TIME4STATUS ) && !defined( S4OFF_THREAD )
            InterlockedIncrement( &(c4->incrementVal) ) ;
         #endif
         if ( record[0] == ' ' )
         {
            file4seqWrite( &wr, record, d4->recWidth ) ;
            newCount++ ;
         }
         // AS Feb 10/06 add a delay for testing status...
         // #ifdef S4TESTING
         //    Sleep( 10 ) ;
         // #endif
         #if defined( TIME4STATUS ) && !defined( S4OFF_THREAD )
            InterlockedIncrement( &(c4->incrementVal) ) ;
         #endif
      }

      u4free( record ) ;

      // AS Mar 29/06 - for encrypted files do not append the eof marker since they don't require since not compatible, and causes length problems
      #ifdef S4FOX
         // AS May 24/02 - 3.0 files do not write the extra eof byte
         if ( d4->compatibility != 30 )  /* 3.0 file */
            file4seqWrite( &wr, "\032", 1 ) ;
      #else
         #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
            if ( d4->file.preprocessed == 0 )
               file4seqWrite( &wr, "\032", 1 ) ;
         #endif
      #endif
      rc = file4seqWriteFlush( &wr ) ;
      #ifdef S4ADVANCE_READ
         file4seqReadInitUndo( &rd ) ;
      #endif
      u4free( rdBuf ) ;
      u4free( wrBuf ) ;
      if ( rc < 0 )
         return -1 ;

      d4->numRecs = newCount ;
      dfile4setMinCount( d4, newCount ) ;

      len = dfile4recordPosition( d4, newCount + 1L ) ;
      // AS Nov 12/04 - for encrypted files do not append the eof marker since they don't require since not compatible, and causes length problems
      #ifdef S4FOX
         // AS May 24/02 - 3.0 files do not write the extra eof byte
         if ( d4->compatibility != 30 )  /* 3.0 file */
            file4longAdd( &len, 1 ) ;
      #else
         #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )  // LY Nov 24/04 : avoid compiler error
            if ( d4->file.preprocessed == 0 )
               file4longAdd( &len, 1 ) ;
         #endif
      #endif
      return file4lenSetLow( &d4->file, len ) ;
   }
#endif /* !S4CLIENT */
#endif /* S4OFF_WRITE */
