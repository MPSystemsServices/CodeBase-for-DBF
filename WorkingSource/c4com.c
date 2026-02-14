/* c4com.c   (c)Copyright Sequiter Software Inc., 1988-1998.  All rights reserved. */

/* CONNECTION4::connectionFailure indicates an irrecoverable connection error.
   In the case of only a error4code() condition, communications will still
   operate normally */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

/*
   MODULE

   CONNECTION4

   This module is used to perform high-level communications access.  It is
     completely independent of the communications protocol beings used
*/


#ifndef S4SERVER
   long S4FUNCTION code4timeout( CODE4 *c4 )
   {
      /*
         FUNCTION

         long code4timeout( CODE4 *c4 )

         DESCRIPTION

         Returns the current timeout setting for CODE4

         NOTES

         cannot be inline due to DLL CODE4 structure potential mismatches
         available whether or not communications are available
      */
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E91006 ) ;
      #endif
      return c4->timeout ;
   }



   void S4FUNCTION code4timeoutSet( CODE4 *c4, long val )
   {
      /*
         FUNCTION

         void code4timeoutSet( CODE4 *c4, long val )

         DESCRIPTION

         Sets the current timeout setting for CODE4

         NOTES

         cannot be inline due to DLL CODE4 structure potential mismatches
         available whether or not communications are available
      */
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E91007 ) ;
            return ;
         }
      #endif
      c4->timeout = val ;
   }
#endif /* !S4SERVER */



#ifndef S4OFF_COMMUNICATIONS
#ifndef S4SERVER
   #ifndef S4MACINTOSH
      #ifndef S4UNIX
         #include <sys\timeb.h>
      #else
         #ifndef S4NO_FTIME
            #include <sys/timeb.h>
         #else
            #include <sys/time.h>
         #endif
      #endif
   #endif
#endif

#ifndef E4OFF_STRING
   extern long error4seek( long ) ;
#endif

static int connection4disconnect( CONNECTION4 *connection )
{
   /*
      DESCRIPTION

      This function performs a friendly or an unfriendly disconnection.

      NOTES

      If CODE4.errorCode is < 0 perform an unfriendly disconnect (just close
        down the connection).
      If CODE4.errorCode is >= 0 perform a friendly disconnect.

      To perform a friendly disconnect, send a CON4DISCONNECT request to the
        other connection.

   */
   unsigned short int mType ;

   #ifdef E4PARM_LOW
      if ( connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif
   if (!connection->connected)
      return r4success ;

   if ( error4code( connection->cb ) >= 0 )  /* do a normal disconnect message first */
   {
      // AS Sept. 26/02 - log all disconnects as well
      #ifndef S4OFF_LOG
         if ( connection->cb->logConn > 5 )
         {
            char buf[250] ;
            #ifdef S4SERVER
               sprintf( buf, "IS0060:Server is performing a normal disconnect of client for userId: [%s] and ip adress: [%s]",
                        connection->connect->client->account.accountId, connection->connect->client->account.tcpAddress ) ;
            #else
               sprintf( buf, "IC0061:Client is performing a normal disconnect" ) ;
            #endif
            assert5( strlen( buf ) < sizeof( buf ) ) ;
            code4logInfo( connection->cb, buf ) ;
         }
      #endif

      mType = htons5( STREAM4DISCONNECT ) ;
      connection4send( connection, (char *)&mType, sizeof( mType ) ) ;
      connection4sendFlush( connection ) ;
      #ifdef S4CLIENT
         // AS Jan 24/02 - Give the server a chance to shut itself down in a friendly manner before terminating our side... - wait 1 second
         u4delaySec();
      #endif
   }

   if ( connection->connect != 0 )
      connect4disconnect( connection->connect ) ;
   connection->connected = 0 ;
   return r4success ;
}



int connection4init( CONNECTION4 *connection, CONNECT4 *connect )
{
   /*
      FUNCTION

      int connection4init( CONNECTION4 *connection, CONNECT4 *connect )
      *** note change in paramaters which is to take effect.

      PARAMATERS

      connect is a connected connection

      NOTES

      Initializes the CONNECTION4 structure by clearing out memory and setting:
        connection->cb = connect->cb ;
        connection->connect = connect ;

   */
   #ifdef E4PARM_LOW
      if ( connect == 0 || connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif

   connection->cb = connect->cb ;
   connection->connect = connect ;

   #ifdef S4SERVER
      assert5( connect->link.n == 0 && connect->link.p == 0 ) ;
   #endif

   return 0 ;
}



int connection4initUndo( CONNECTION4 *connection )
{
   /*
      FUNCTION

      int connection4initUndo( CONNECTION4 *connection )

      DESCRIPTION

      NOTES

      Uninitialize the CONNECTION4 structure:
        performing disconnection
   */
   #ifdef E4PARM_LOW
      if ( connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif

   #ifdef E4ANALYZE
      connection->initUndone = 1 ;
   #endif

   connection4disconnect( connection ) ;

   connection4clear( connection ) ;

   if (connection->buffer != NULL)
   {
      connection->buffer -= 4 ;
      u4free(connection->buffer) ; /*The sub-buffers were cleared with connection4clear */
   }
   if ( connection->connect != 0 )
      connect4initUndo( connection->connect ) ;

   memset( connection, 0, sizeof ( CONNECTION4 ) ) ;

   return 0 ;
}



int connection4assign( CONNECTION4 *connection, const short type, const long clientId, const long serverId )
{
   /*
      FUNCTION

      int connection4assign( CONNECTION4 *connection, const int type, const long clientId, const long serverId )

      DESCRIPTION

      Just fills in data into the static PACKET4 structure (CONNECTION4.packet)

   */
   #ifdef E4PARM_LOW
      if ( connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif

   if ( connection->connectionFailure < 0 )
      return connection->connectionFailure ;

   #ifndef S4SERVER
      connection->cb->lockedLockItem = -2L ;
      if ( connection->cb->lockedFileName != 0 )
         connection->cb->lockedFileName[0] = 0 ;
      if ( connection->cb->lockedUserName != 0 )
         connection->cb->lockedUserName[0] = 0 ;
      if ( connection->cb->lockedNetName != 0 )
         connection->cb->lockedNetName[0] = 0 ;
   #endif

   packet4setType( &connection->packet, type ) ;
   packet4setStatus( &connection->packet, 0 ) ;
   packet4setClientId( &connection->packet, clientId ) ;
   packet4setServerId( &connection->packet, serverId ) ;
   #ifndef S4SERVER
      packet4setReadLock( &connection->packet, connection->cb->readLock ) ;
      packet4setUnlockAuto( &connection->packet, code4unlockAuto( connection->cb ) ) ;
      // AS Aug 29/03 - Support for memSizeMemoExpr which can change at any time
      // AS Mar 14/07 - this should always be included
      packet4setSizeMemoExpr( &connection->packet, c4getMemSizeMemoExpr( connection->cb ) ) ;
   #endif

   connection4clear( connection ) ;

   return 0 ;
}

#define packet4connectionId( p ) ( (p)->connectionId )


#ifndef S4SERVER
   int code4unlockSet( CODE4 *c4 )
   {
      /*
         FUNCTION

         static int code4unlockSet( CODE4 *c4 )

         DESCRIPTION

         This function is used to mark everything unlocked for the client.
         This is performed when the server tells the client that it had to unlock
         everything in order to execute the requested function
      */
      DATA4FILE *data ;
      LOCK4LINK *lock ;
      SINGLE4DISTANT singleDistant ;

      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E90155 ) ;
      #endif

      for( data = 0 ;; )
      {
         data = (DATA4FILE *)l4next( &c4->dataFileList, data ) ;
         if ( data == 0 )
            return 0 ;
         data->numRecs = -1 ;
         #ifdef S4CLIENT
            // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
            // data->appendLock = 0 ;
            // data->fileLock = 0 ;
            data->fileLockServerId = 0 ;
            data->fileLockLockId = 0 ;
            data->appendLockServerId = 0 ;
            data->appendLockLockId = 0 ;
         #endif

         single4distantInitIterate( &singleDistant, &data->lockedRecords ) ;

         for( ;; )
         {
            lock = (LOCK4LINK *)single4distantToItem( &singleDistant ) ;
            if ( lock == 0 )
               break ;
            single4distantPop( &singleDistant ) ;
            mem4free( c4->lockLinkMemory, lock ) ;
         }
      }
   }



   void d4unlockClientData( DATA4 *data )
   {
      DATA4FILE *dfile ;
      LOCK4LINK *lock ;
      SINGLE4DISTANT singleDistant ;

      /* AS June 08/01 - It is possible that the input 'data' may be NULL, esp. if the server
         has just shut down.  In that case, just return without doing anything
      */
      if ( data == 0 )
         return ;

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return ;
      #endif

      dfile = data->dataFile ;

      // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
      // if ( dfile->fileLock == data )
      if ( dfile->fileLockServerId == data4serverId( data ) && dfile->fileLockLockId == data4lockId( data ) )
      {
         data->dataFile->numRecs = -1 ;
         dfile->fileLockServerId = 0 ;
         dfile->fileLockLockId = 0 ;
      }
      // if ( dfile->appendLock == data )
      if ( dfile->appendLockServerId == data4serverId( data ) && dfile->appendLockLockId == data4lockId( data ) )
      {
         data->dataFile->numRecs = -1 ;
         dfile->appendLockServerId = 0 ;
         dfile->appendLockLockId = 0 ;
      }
      single4distantInitIterate( &singleDistant, &dfile->lockedRecords ) ;
      for ( ;; )
      {
         lock = (LOCK4LINK *)single4distantToItem( &singleDistant ) ;
         if ( lock == 0 )
            break ;
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         // if ( lock->data == data )
         if ( lock->serverId == data4serverId( data ) && lock->lockId == data4lockId( data ) )
         {
            single4distantPop( &singleDistant ) ;
            mem4free( data->codeBase->lockLinkMemory, lock ) ;
         }
         else
            single4distantNext( &singleDistant ) ;
      }
   }
#endif /* S4SERVER */



// AS July 25/02 - Perform message verification, this will help to halt problems before they occur at a minimal overhead
static Bool5 connection4verifyMessageType( short int messageType )
{
   switch( messageType )
   {
      case CON4LOCK:
      case CON4UNLOCK:
      case CON4WRITE:
      case CON4GO:
      case CON4SKIP:
      case CON4SEEK:
      case CON4SEEK_DBL:
      case CON4SEEK_LONGLONG:
      case CON4START:
      case CON4COMMIT_PHASE_ONE:
      case CON4COMMIT_PHASE_TWO:
      case CON4COMMIT_BOTH_PHASES:
      case CON4ROLLBACK:
      case CON4OPEN:
      case CON4CLOSE:
      case CON4RECCOUNT:
      case CON4LOCK_CONFIRM:
      case CON4LOCK_GROUP:
      // case CON4ABORT:
      case CON4CONNECT:
      case CON4DISCONNECT:
      // case CON4DISCONNECTED:
      case CON4PACK:
      case CON4ZAP:
      case CON4CREATE:
      case CON4CANCEL:
      case CON4RELATE_INIT:
      case CON4RELATE_TOP:
      case CON4RELATE_BOTTOM:
      case CON4RELATE_DO:
      case CON4RELATE_DO_ONE:
      case CON4RELATE_FREE:
      // case CON4RELATE_CHANGED:
      case CON4RELATE_LOCK:
      case CON4RELATE_UNLOCK:
      case CON4RELATE_SKIP:
      case CON4INDEX_CREATE:
      case CON4INDEX_OPEN:
      case CON4INDEX_CLOSE:
      case CON4POSITION:
      case CON4POSITION_SET:
      case CON4REINDEX:
      case CON4CHECK:
      case CON4TOP:
      case CON4BOTTOM:
      case CON4APPEND:
      case CON4MEMO_COMPRESS:
      case CON4MEMO:
      case CON4INFO:
      case CON4UNIQUE_SET:
      // case CON4PASSWORD:
      case CON4TRANS_INIT:
      case CON4RELATE_OPT:
      case CON4SYSTEM:
      case CON4TAG_SYNCH:
      case CON4DATE_FORMAT:
      case CON4TRAN_EOF:
      case CON4TRAN_EOF_HALT:
      case CON4TRAN_RESTART:
      case CON4INDEX_FORMAT:
      case CON4INDEX_INFO:
      case CON4INDEX_REINDEX:
      case CON4ACK:
      case CON4INDEX_FNAME:
      case CON4DATA_FNAME:
      // AS May 17/04 - functionality to copmress the data file...
      case CON4DATA_COMPRESS:
      case CON4ADD_TAG:
      case CON4CATALOG:
      case CON4REMOVE:
      case CON4PASSWORD_SET:
      case CON4RESTART:
      case CON4CALC_CREATE:
      case CON4CALC_RESET:
      case CON4TAG_OPEN:
      case CON4CRASH:
      case CON4CREATE_AUX_CONNECTION:
      case CON4CONFIG_NAME:
      case CON4SHUTDOWN:
      case CON4CONNECT_ACCEPT_NEW:
      case CON4CONNECT_CUT_ALL:
      case CON4CONNECT_CUT:
      case CON4CLOSE_FILES:
      case CON4TAG_FNAME:
      case CON4SERVER_OS:
      case CON4DATA_CODEPAGE:
      case CON4REMOVE_TAG:
      // AS Apr 15/03 - support for unlock append by client
      case CON4UNLOCK_APPEND:
      // case CON4FIELDS_ADD:
      case CON4FLUSH: // AS Nov 12/03 - flush support
      case CON4VERSION:
      // AS Oct 25/05 - low level tag functionality
      case CON4TAG_BOTTOM:
      case CON4TAG_CACHE_SKIP:
      case CON4TAG_COUNT:
//      case CON4TAG_KEY:  // AS Feb 9/09 - replaced functionality with code to get key every time instead
      case CON4TAG_EXPR_KEY:
      case CON4TAG_SEEK:
      case CON4TAG_SKIP:
      case CON4TAG_TOP:
      case CON4TAG_GO:
      case CON4TAG_EOF:
      case CON4TAG_POSITION:
      case CON4TAG_POSITION_SET:
      // AS Jun 11/07 - new function to copy a database
      case CON4COPY:
      case CON4MODIFY:
      case STREAM4DISCONNECT:
      case STREAM4UNLOCK_ALL:
      case STREAM4LOCKED_INFO:
      case STREAM4RECONNECT:
      case STREAM4UNLOCK_DATA:
      case STREAM4PING:
      case STREAM4STATUS:
      case STREAM4TABLES:
      case STREAM4CURRENT_DIRECTORY:
      case STREAM4DIRECTORY:
      case STREAM4BUSY_STATUS:
      case STREAM4CLIENT_PIPE_RECV:
      case STREAM4CLIENT_PIPE_SEND:
      case STREAM4CLIENT_PIPE_SEND_CHECK:
      case STREAM4CLIENT_PIPE_SEND_CANCEL:
      case STREAM4CLIENT_PIPE_OPEN_RECV:
      case STREAM4CLIENT_PIPE_OPEN_SEND:
      case STREAM4CLIENT_PIPE_CLOSE_RECV:
      case STREAM4RECONNECT_NEW:
      case STREAM4BLAST_TEST_WRITE:
      case STREAM4BLAST_TEST_READ:
      case STREAM4SET_SLOW_DELAY:
      case STREAM4SWAP_LOGFILE:
      case MSG5DB_SESSION_DATA_CLOSE:
      case MSG5DB_SESSION_DATA_OPEN:
      case MSG5DB_SESSION_DATA_CREATE:
      case MSG5DB_ROW_REQUEST_ARRAY:
      case MSG5DB_REQUEST_DEFERRED_FIELD:
      case MSG5DB_ROW_REQUEST_SEQUENTIAL:
      case MSG5DB_INDEX_REQUEST_KEY:
      case MSG5DB_INDEX_ROW_REQUEST:
      case MSG5DB_INDEX_ROW_REQUEST_KEYS:
      case MSG5DB_COLUMN_INFO:
      case MSG5DB_UPDATE_FIELDSET:
      case MSG5DB_RECCOUNT:
      case MSG5DB_SESSION_INDEX_CLOSE:
      case MSG5DB_SESSION_INDEX_OPEN:
      case MSG5DB_INDEX_COLUMN_INFO:
      case MSG5DB_INDEX_UPDATE_FIELDSET:
      case MSG5DB_SESSION_SCHEMA_OPEN:
      case MSG5DB_CURRENT_DIRECTORY:
      case MSG5DB_SET_RESTRICTIONS:
      case MSG5DB_ADD_DIRECTORY:
      case MSG5DB_TAG_SELECT:
      case MSG5DB_SCHEMA_SEEK:
      case MSG5DB_SCHEMA_REQUEST_SEQ:
      case MSG5DB_SCHEMA_GET_NEXT:
      case MSG5DB_FIND_OR_ADD_ENTRY:
      case MSG5DB_ADD_TAG:
      case MSG5DB_INDEX_REMOVE:
      case MSG5DB_TBL_REMOVE_INDEXES:
      case MSG5DB_TBL_REMOVE:
      case MSG5DB_SESSION_DATA_SEEK:
      case MSG5DB_SESSION_DATA_WRITE:
      case MSG5DB_SESSION_DATA_WRITE_DONE:
      case MSG5LEN:
      case MSG5DB_SESSION_DATA_DELETE:
      case MSG5DB_SESSION_DATA_DEFAULTS:
      case MSG5DB_TBL_WRITE_RESULT_INC:
      case MSG5DB_INDEX_SET_RANGE:
      case MSG5DB_SESSION_ISO_LEVEL:
      case MSG5DB_SESSION_IN_TRANSACTION:
      case MSG5DB_ADD_COLUMN:
      case MSG5DB_REMOVE_COLUMN:
      case MSG5DB_ROWSET_DESTROY:
      case MSG5DB_SCHEMA_POPULATE_INDEXES:
      case MSG5DB_SCHEMA_POPULATE_COLUMNS:
      case MSG5DB_FETCHNEXTRECNO:
      case MSG5DB_SCHEMA_REQUEST_DIRECT:
      case MSG5DB_INTEGRATED_TAG_SELECT:
      case MSG5DB_INTEGRATED_ROW_REQUEST:
      case MSG5DB_INDEX_DESTROY_RANGE:
      // case MSG5DB_INTEGRATED_TAG_EXISTS:
      case MSG4TEST_SEND_RECEIVE:
      case MSG5DB_SESSION_UNLOCK_ALL:
      case MSG5SKIP:
      case MSG5RELATE_SKIP_MULTI:
      case MSG5WRITE_BATCH:
      case MSG5COMPRESS:
//       case MSG5PREPROCESS_P:
//       case MSG5PREPROCESS_PK:
      case MSG5PREPROCESS_B:
      case MSG5PREPROCESS_DISABLE:
      #ifdef S4JAVA
         case JAVA4START_COUNT:
         case JAVA4LOCK_GROUP:
         case JAVA4UNLOCK:
         case JAVA4WRITE:
         case JAVA4WRITE2:
         case JAVA4GO:
         case JAVA4SKIP:
         case JAVA4SEEK_N:
         case JAVA4SEEK_DBL:
         case JAVA4OPEN:
         case JAVA4CLOSE:
         case JAVA4RECCOUNT:
         case JAVA4CONNECT:
         case JAVA4DISCONNECT:
         case JAVA4PACK:
         case JAVA4CREATE:
         case JAVA4INDEX_CREATE:
         case JAVA4INDEX_OPEN:
         case JAVA4POSITION:
         case JAVA4POSITION_SET:
         case JAVA4REINDEX:
         case JAVA4TOP:
         case JAVA4BOTTOM:
         case JAVA4APPEND:
         case JAVA4APPEND2:
         case JAVA4ACCESS_MODE_SET:
         case JAVA4READ_LOCK_SET:
         case JAVA4STATUS_CODE:
         case JAVA4STATUS_FIELDS:
         case JAVA4READ_ONLY_SET:
         case JAVA4REGISTER_FIELD:
         case JAVA4REGISTER_FIELD2:
         case JAVA4SAFETY_SET:
         case JAVA4SELECT_TAG:
         case JAVA4DEFAULT_UNIQUE_SET:
         case JAVA4SELECT_DATA:
         case JAVA4BLANK:
         case JAVA4RECNO:
         case JAVA4UNLOCK_AUTO_SET:
         case JAVA4GET_FIELD_INFO:
         case JAVA4REFRESH:
      #endif
         return 1;
      default:
         return 0 ;
   }
}



int connection4receiveMessage( CONNECTION4 *connection )
{
   /*
      FUNCTION

      int connection4receiveMessage( CONNECTION4 *connection )

      DESCRIPTION

      This function is used to retrieve a message from the given connection.
      This function only returns when either the message has been received or
      if the connection time out.

      ERRORS

      Set CODE4.errorCode in case of error

      RETURNS

      < 0 means error
      any other code is the message->type received

      NOTES

      In S4CLIENT mode, the message request must be examined.  In some cases,
        messages are sent to the client from the server to request special
        actions.  These are:
          CON4DISCONNECT
          CON4UNLOCK_ALL
          CON4LOCKED

      In S4SERVER mode, this function calls server4processMessage() or the
        equivalent JAVA process function to process the received message.

      This function will wait for the entire message to arrive before proceeding
        if it is one of the old-style CodeBase communication functions
        (i.e. < 10000)

      This function calls connect4receive() to receive the first piece of
        data.
   */
   #ifdef E4PARM_LOW
      if ( connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif

   if ( connection->connectionFailure < 0 )
      return connection->connectionFailure ;

   CONNECT4 *connect = connection->connect ;

   CODE4 *c4 = connection->cb ;
   assert5( c4 != 0 ) ;

   // don't require exclusive access for this fixed constant value...
   long timeout = code4timeoutVal( c4 ) ;

   #ifdef S4SERVER
      // must save, may even become invalid if connection terminated, but still need to remove via exitExclusive
      SERVER4CLIENT *client = connect->client ;
   #endif

   for( ;; )
   {
      /* get the message type (first 2 bytes) */

      int rc = connect4receive( connect, &connection->packet.type, sizeof( connection->packet.type ), timeout ) ;

      if ( rc != r4success )
      {
         connection->connectionFailure = e4connection ;
         if ( rc == r4timeout ) /* CS 1999/09/28 report e4timeout if applicable */
            rc = e4timeOut ;

         #ifdef S4SERVER
            code4enterExclusiveVerifyFirst( c4, client, 1 ) ;
            if ( client->didConnectStatus == 1 )  // only report if we did actually get a connection through from the client
            {
               error4( c4, rc < 0 ? rc : e4net, E90160 ) ;
            }
            code4exitExclusiveVerifyLast( c4, client ) ;
         #else
            error4( c4, rc < 0 ? rc : e4net, E90160 ) ;
         #endif

         return rc ;
      }

      short int messageType = ntohs5(connection->packet.type) ;

      #if defined( S4COM_PRINT ) && defined( S4SERVER )
         char buffer[128] ;
         sprintf(buffer, "Receiving Message: %s, on socket %d", s4connectionPrint( messageType ), connect->connectBuffer.connectLowPtr->sockr ) ;
         debug4display(buffer) ;
      #endif

      #if defined( S4SERVER ) && defined( TIME4STATUS )
         // this message should be handled while another worker thread is activated and servicing requests...
         // i.e. don't exclusivize the server
         if ( messageType == STREAM4BUSY_STATUS )
         {
            double result = htond( code4status( c4 ) ) ;
            connect4send( connect, &result, sizeof( double ) ) ;
            connect4sendFlush( connect ) ;
            list4mutexWait( &c4->connectsToService ) ;
            connect->workState = CONNECT4WORKING ;
            list4mutexRelease( &c4->connectsToService ) ;
            return 0 ;
         }
      #endif

      #if defined( S4SERVER ) && !defined( S4OFF_THREAD )
         // BCR 10/20/00 -- only screws up mutex count in off_thread mode
         code4enterExclusiveVerifyFirst( c4, client, 1 ) ;
      #endif

      // AS Sep 3/03 - Improved sequencing here.  Now that we have exclusive access to the client, change the state
      // to working to allow shutdown to be possible (once we remove our exclusive access to the client)
      #ifdef S4SERVER
         list4mutexWait( &c4->connectsToService ) ;
         connect->workState = CONNECT4WORKING ;
         list4mutexRelease( &c4->connectsToService ) ;
      #endif

      if ( messageType < 10000 )
      {
         if ( messageType < 0 )   // bad message, terminate client by returning error
         {
            connection->connectionFailure = e4connection ;
            #ifdef S4SERVER
               code4exitExclusiveVerifyLast( c4, client ) ;
            #endif
            return error4( c4, e4connection, E90160 ) ;
         }

         /* old-style message, receive the whole message */
         // AS Jan 21/02 - Add error handling to report error occasionally missed...
         int rcc = connect4receive( connect, &connection->packet.status, sizeof( connection->packet ) - sizeof( connection->packet.type ), timeout ) ;
         if ( rcc < 0 )
         {
            connection->connectionFailure = e4connection ;
            #ifdef S4SERVER
               code4exitExclusiveVerifyLast( c4, client ) ;
            #endif
            return error4( c4, e4connection, E90160 ) ;
         }
         long tempLen = connection4len( connection ) ;
         // AS Be a little less generous here - anything over a certain size would have to be considered
         // as suspicious enough to be too large. - I would say anything over 10 Megs would be too large
         // another thing we can do is test the messageType for validity here...
         Bool5 messageBad = 0 ;
         if ( connection4verifyMessageType( messageType ) == 0 )
            messageBad = 1 ;
         // AS Mar 12/03 - Releax this constraint completely to avoid potentially disconnecting clients with large messages
         // {
         //    // AS Mar 3/03 - For some messages this may be ok - in particular those dealing
         //    // with memos (i.e. write and append and read)
         //    if ( messageType != CON4WRITE && messageType != CON4APPEND && messageType != CON4MEMO )
         //       messageBad = 1 ;
         // }

         if ( messageBad == 1 )
         {
            connection->connectionFailure = e4connection ;
            error4( c4, e4connection, E90160 ) ;
            #ifdef S4SERVER
               code4exitExclusiveVerifyLast( c4, client ) ;
            #endif
            return e4connection ;
         }
         if ( connection->bufferLen < tempLen )
         {
            /* allocate an extra 4 bytes and move pointers according to connection->buffer ) */
            char *tPtr = (char *)u4allocFree( c4, tempLen + 4 ) ;
            if ( tPtr == NULL )
            {
               #if defined( S4SERVER ) && !defined (S4OFF_THREAD)
                  code4exitExclusiveVerifyLast( c4, client ) ;
               #endif
               return e4memory ;
            }
            if ( connection->buffer )
            {
               connection->buffer -= 4 ; /* first 4 bytes used for pool handling */
               memcpy( tPtr, connection->buffer, connection->bufferLen + 4 ) ;
               u4free( connection->buffer ) ;
            }
            connection->buffer = tPtr + 4 ;
            connection->bufferLen = tempLen;
         }

         if ( connect4receive( connect, connection->buffer, tempLen, timeout ) != r4success )
         {
            connection->connectionFailure = e4connection ;
            error4( c4, e4connection, E90160 ) ;
            #if defined( S4SERVER ) && !defined( S4OFF_THREAD )
               code4exitExclusiveVerifyLast( c4, client ) ;
            #endif
            return e4connection ;
         }
      }

      #ifdef S4CLIENT
         /* client takes special actions on some messages */
         unsigned short len ;
         // AS Apr 15/03 - support for new lockId for shared clone locking
         S4LONG lockId, serverId ;

         switch( messageType )
         {
            case STREAM4DISCONNECT:
               if ( connection->cb->logConn > 5 )
               {
                  code4logInfo( connection->cb, "IC0062:Server has sent a disconnect request to this client" ) ;
               }
               connection->connectionFailure = e4connection ;
               return error4( c4, e4connection, E90160 ) ;
            case STREAM4UNLOCK_ALL:
               rc = code4unlockSet( c4 ) ;
               if ( rc < 0 )
                  return rc ;
               continue ;   /* this message type means that there is an additionaly regular message arriving */
            case STREAM4UNLOCK_DATA:
               /* in this case, the server sends back the lockId and serverId of who gets unlocked*/
               connect4receive( connect, &lockId, sizeof( lockId ), timeout ) ;
               connect4receive( connect, &serverId, sizeof( serverId ), timeout ) ;
               lockId = ntohl5(lockId) ;
               serverId = ntohl5(serverId) ;
              /* if ( rc != r4success )*/
              /* {*/
              /*    connection->connectionFailure = e4connection ;*/
              /*    return error4( c4, e4connection, E90160 ) ;*/
              /* }*/
               d4unlockClientData( tran4data( &(c4->c4trans.trans), serverId, lockId )) ;
              /* if ( rc < 0 )*/
              /*    return rc ;*/
               continue ;   /* this message type means that there is an additional regular message arriving */
            case STREAM4LOCKED_INFO:
               connect4receive( connect, &c4->lockedLockItem, sizeof( long ), timeout ) ;
               c4->lockedLockItem = htonl5( c4->lockedLockItem ) ;
               connect4receive( connect, &len, sizeof( short ), timeout ) ;
               len = htons5( len ) ;
               if ( c4->lockedFileNameLen < len )
               {
                  if ( u4allocAgain( c4, &c4->lockedFileNameBuf, &c4->lockedFileNameLen, len ) != 0 )
                     return error4( c4, e4memory, E90160 ) ;
               }
               connect4receive( connect, c4->lockedFileNameBuf, len, timeout ) ;
               c4->lockedFileName = c4->lockedFileNameBuf ;
               connect4receive( connect, &len, sizeof( short ), timeout ) ;
               len = htons5( len ) ;
               if ( c4->lockedUserNameLen < len )
               {
                  if ( u4allocAgain( c4, &c4->lockedUserNameBuf, &c4->lockedUserNameLen, len ) != 0 )
                     return error4( c4, e4memory, E90160 ) ;
               }
               connect4receive( connect, c4->lockedUserNameBuf, len, timeout ) ;
               c4->lockedUserName = c4->lockedUserNameBuf ;
               connect4receive( connect, &len, sizeof( short ), timeout ) ;
               len = htons5( len ) ;
               if ( c4->lockedNetNameLen < len )
               {
                  if ( u4allocAgain( c4, &c4->lockedNetNameBuf, &c4->lockedNetNameLen, len ) != 0 )
                     return error4( c4, e4memory, E90160 ) ;
               }
               rc = connect4receive( connect, c4->lockedNetNameBuf, len, timeout ) ;
               c4->lockedNetName = c4->lockedNetNameBuf ;
               if ( rc != r4success )
               {
                  connection->connectionFailure = e4connection ;
                  return error4( c4, e4connection, E90160 ) ;
               }
               continue ;   /* this message type means that there is an additionaly regular message arriving */
         }
      #else
         #ifdef S4JAVA
            if ( messageType > JAVA4START_COUNT )  /* java message */
               rc = java4processMessage( c4, client, &messageType ) ;
            else  /* must be a standard server message */
         #endif
            {  // braces required for else just above
               #ifdef TIMER5OUT_ALL_MESS
                  server4timerStart( c4->server, "processMessage being called " )  ;
               #endif
               rc = server4processMessage( c4->server, client ) ;
               #ifdef TIMER5OUT_ALL_MESS
                  server4timerStop( c4->server ) ;
               #endif
            }

         #ifndef S4OFF_THREAD
            // AS Oct 12/04 - support for java...
            assert5( rc < 0 || messageType == STREAM4DISCONNECT || messageType == JAVA4DISCONNECT || client->connect.workState == CONNECT4WORKING ) ;
            code4exitExclusiveVerifyLast( c4, client ) ;
         #endif
      #endif

      if ( rc < 0 )
         return rc ;

      return messageType ;
   }
}



int connection4addData( CONNECTION4 *connection, const void *data, long dataLen, void **atPtr )
{
   /*
      FUNCTION

      int connection4addData( CONNECTION4 *connection, const void *data, long dataLen, void **atPtr  )

      DESCRIPTION

      This function is used to add data to the connection data buffer

      PARAMATERS

      data may by NULL if we just want to reserve space, if data is NULL, atPtr
        cannot be NULL.  if data is NULL, the reserved space is blanked out withj
        NULLs
      atPtr is generally used if data is NULL, although it can be used in either
        case.  atPtr returns a pointer to the location in memory where the data
        which will be sent actually resides.  This allows programs to reserve
        memory and use the memory-to-send buffer directly without having to do
        extra allocation of their own.  This also reduces memory copying if
        data is NULL.

      NOTES

      Because the pointer input/output may be used after this function is called,
        the memory cannot be moved around.  Therefore, if the size of the
        default buffer is exceeded, a chain of memory pools is used instead.
        The first 4 bytes of the previous pool point to the next pool, until
        a NULL is reached.  The extra pools are freed up when connection4clear()
        are called.  In general, there will rarely be extra pools, and if
        there are extra pools, there will generally only be 1 or 2 large ones
        (eg. large memo fields)

      RETURNS

      r4success
      < 0

      ERRORS

      Set CODE4.errorCode in case of error
   */
   char **nextPoolPtr, *usePtr ;

   if (connection->buffer)
      if ( connection->bufferLen < CONNECTION4BUFFER_LEN )
      {
         #ifdef E4DEBUG
            /* shouldn't be possible */
            if ( connection->curBufferLen != 0 )
               return error4( 0, e4parm, E90160 ) ;
         #endif
         usePtr = connection->buffer - 4 ;
         u4free( usePtr ) ;
         connection->buffer = 0 ;
         connection->bufferLen = 0 ;
      }

   if ( connection->buffer == 0 )
   {
      connection->buffer = (char *)u4allocFree( connection->cb, CONNECTION4BUFFER_LEN + sizeof(void *) ) ;
      if ( connection->buffer == NULL )
         return e4memory ;
      connection->buffer += sizeof(void *) ;   /* use first 4 bytes for pointer to next pool */
      connection->bufferLen = CONNECTION4BUFFER_LEN ;
      connection->curBufferLen = 0 ;
   }

   nextPoolPtr = (char **)(connection->buffer - 4) ;   /* first 4 bytes of pool */

   /* if the nextPoolPtr is not NULL or if we do not have enough room
      in our current buffer, then the data must go into its own pool
      and be added to the pool */
   if ( *nextPoolPtr != NULL || connection->curBufferLen + dataLen > connection->bufferLen )
   {
      while ( *nextPoolPtr != NULL )  /* find the end of the chain */
        nextPoolPtr = (char **)*nextPoolPtr ;

      /* now create a new allocation, and place it at the current pool spot */
      *nextPoolPtr = (char *)u4allocFree( connection->cb, dataLen + 8 ) ;
      if ( *nextPoolPtr == 0 )
         return e4memory ;
      usePtr = *nextPoolPtr + 8 ;  /* save 4 bytes for ptr to next pool, 4 bytes for length */
      *((long *)(usePtr - 4)) = dataLen ;
      *((void **)*nextPoolPtr ) = NULL ;
   }
   else
      usePtr = connection->buffer + connection->curBufferLen ;

   if ( data == NULL )
      memset( usePtr, 0, dataLen ) ;
   else
      memcpy( usePtr, data, dataLen ) ;

   if ( atPtr != NULL )
      *atPtr = usePtr ;

   if ( *nextPoolPtr == NULL )   /* increase current position for basic pool */
      connection->curBufferLen += dataLen ;

   return 0 ;
}



void connection4clear( CONNECTION4 *connection )
{
   /*
      FUNCTION

      int connection4clear( CONNECTION4 *connection )

      DESCRIPTION

      This function is used to reset the current buffer position to zero.
      Also, if there are extra pool pointers, they are freed.

      ERRORS

      Only severe errors are possible
   */
   char **xPtr, **nxPtr ;

   connection->curBufferLen = 0 ;
   if ( connection->buffer == NULL )
      return ;

   xPtr = (char **)(*(char **)(connection->buffer - 4 )) ;   /* extra pools stored there */
   *(void **)(connection->buffer - 4) = NULL ;
   while ( xPtr != NULL )   /* send the additional data */
   {
      nxPtr = (char **)*xPtr ;
      u4free( xPtr ) ;
      xPtr = nxPtr ;
   }
}



int S4FUNCTION connection4receive( CONNECTION4 *connection, char *data, int len )
{
   /*
      FUNCTION

      int connection4receive( CONNECTION4 *connection, char *data, int len )

      DESCRIPTION

      This function is used to receieve part or all of a remaining message.
      No more than maxLen will be received

      PARAMATERS

      len is the quantity to receive
      data must be large enough to hold maxLen.

      RETURNS

      the amount read is returned.

      ERRORS

      Set CODE4.errorCode in case of error

      NOTES

      Wait for the next message from CONNECT, use the CODE4.timeout to determine
        when to fail out with a timing error.

      Must be exported for S4UTILS switch
   */
   int rc ;

   rc = connect4receive( connection->connect, data, len, code4timeoutVal( connection->cb ) ) ;
   if (rc == r4success )
      return len ;
   return rc ;
}



#ifndef S4SERVER
   short connection4repeat( CONNECTION4 *connection )
   {
      /*
         FUNCTION

         DESCRIPTION

         uses CODE4.numAttempts to repetitively try the operation.
         uses CODE4.timeOut to wait between attempts.

         Only re-tries if r4locked failure

         NOTES

      */
      int rc, delayHundredths ;
      PACKET4 savePacket ;
      unsigned int dataLen ;
      CODE4 *c4 ;
      #ifdef S4LOCK_HOOK
         int count ;
      #else
         // AS 02/27/01 - fix #255 - track num attempts only if NOT S4LOCK_HOOK
         int loop ;
      #endif
      char **xPtr, **nxPtr ;
      void *sv = 0 ;

      #ifdef E4PARM_LOW
         if ( connection == 0 )
            return error4( 0, e4parm, E90160 ) ;
      #endif

      c4 = connection->cb ;

      #ifndef S4LOCK_HOOK
         // AS 02/27/01 - fix #255 - track num attempts only if NOT S4LOCK_HOOK
         loop = c4->lockAttempts ;
      #endif
      delayHundredths = c4->lockDelay ;

      dataLen = connection->curBufferLen ;
      #ifndef S4LOCK_HOOK
         // AS 02/27/01 - fix #255 - track num attempts only if NOT S4LOCK_HOOK
         if ( loop != 1 )   /* if only 1 attempt, don't bother with storing */
      #endif
         {
            if ( c4->savedDataLen < dataLen )
            {
               // AS 05/03/99 -- was failing to free saved data on reallocation...
               if ( c4->savedData != 0 )
               {
                  c4->savedDataLen = 0 ;
                  u4free( c4->savedData ) ;
               }
               c4->savedData = (char *)u4allocFree( c4, dataLen ) ;
               if ( c4->savedData == 0 )
                  return e4memory ;
               c4->savedDataLen = dataLen ;
            }
            memcpy( c4->savedData, connection->buffer, dataLen ) ;
            memcpy( &savePacket, &connection->packet, sizeof( PACKET4 ) ) ;
         }

      xPtr = (char **)(*(char **)(connection->buffer - 4 )) ;   /* extra pools stored there */
      memcpy( &sv, connection->buffer - 4, 4 ) ;

      #ifdef S4LOCK_HOOK
         for ( count = 0 ;; count++ )
      #else
         for ( ;; )
      #endif
      {
         #ifndef S4LOCK_HOOK
            // AS 02/27/01 - fix #255 - track num attempts only if NOT S4LOCK_HOOK
            if ( loop > 0 )
               loop-- ;
         #endif
         c4->lockedLockItem = -2L ;
         c4->lockedFileName = 0 ;
         c4->lockedUserName = 0 ;
         c4->lockedNetName = 0 ;

         rc = connection4sendMessageLow( connection, 0 ) ;  // AS Sep 9/11 - wasn't checking for failure in send operation resulting in hang
         if ( rc < 0 )
            break ;

         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            break ;
         rc = connection4status( connection ) ;
         #ifndef S4LOCK_HOOK
            if ( loop == 0 )   /* get receive return if loop == 0 */
               break ;
         #endif
         if ( rc != r4locked )
            break ;
         #ifdef S4LOCK_HOOK
            rc = code4lockHook( c4, c4->lockedFileName, c4->lockedUserNameBuf, c4->lockedNetNameBuf, c4->lockedLockItem, count ) ;
            if ( rc != 0 )
               break ;
         #endif
         connection4assign( connection, savePacket.type, 0, 0 ) ;
         memcpy( &connection->packet, &savePacket, sizeof( PACKET4 ) ) ;
         connection4addData( connection, c4->savedData, dataLen, 0 ) ;
         memcpy( connection->buffer - 4, &sv, 4 ) ;
         u4delayHundredth( delayHundredths ) ;
      }

      while (xPtr != NULL )   /* send the additional data */
      {
         nxPtr = (char **)*xPtr ;
         u4free(xPtr) ;
         xPtr = nxPtr ;
      }
      return rc ;
   }
#endif /* !S4SERVER */



int S4FUNCTION connection4sendMessageLow( CONNECTION4 *connection, char doFreeXPtr )
{
   /* needs to be exported for S4UTILS */
   /* for connection4repeat(), we don't want to free the xPtr's */
   int rc ;
   long int connectionLen, len ;
   char **xPtr, **nxPtr ;

   #ifdef E4PARM_LOW
      if ( connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( connection->cb == 0 )
         return error4( 0, e4struct, E90160 ) ;
   #endif

   if ( connection->connectionFailure < 0 )
      return connection->connectionFailure ;

   len = connection->curBufferLen ;
   if ( connection->buffer != 0 )  // AS 04/20/99 --> is possible nothing sent, don't gpf in that case...
   {
      xPtr = (char **)( connection->buffer - 4 ) ;   /* extra pools stored there */
      while (*xPtr != NULL )   /* send the additional data */
      {
         len += *((long *)(*xPtr + 4) ) ;
         xPtr = (char **)*xPtr ;
      }
   }

   rc = connection4setLen( connection, len ) ;
   if ( rc < 0 )
   {
      connection->connectionFailure = rc ;
      return error4stack( connection->cb, (short)rc, E90160 ) ;
   }

   #ifdef S4COM_PRINT
      #ifdef S4CLIENT
         #ifdef S4UNIX
            printf( "Sending Message:  %s\n",
               s4connectionPrint( connection4type( connection ) ) ) ;
         #endif
      #endif
   #endif

   connectionLen = connection4len( connection ) ;
   if ( connectionLen < 0 )
   {
      connection->connectionFailure = (short)connectionLen ;
      return error4stack( connection->cb, (short)connectionLen, E90160 ) ;
   }

   // AS July 25/02 - Ensure the client is sending a valid message - this helps keep the server from having
   // to resolve these problems.
   short int messageType = ntohs5(connection->packet.type) ;
   if ( connection4verifyMessageType( messageType ) == 0 )
      return error4( connection->cb, e4message, E84310 ) ;

   rc = connect4send( connection->connect, &connection->packet, sizeof( connection->packet ) ) ;
   rc = connect4send( connection->connect, connection->buffer, connection->curBufferLen ) ;
   if ( connection->buffer != 0 )  // AS 04/20/99 --> is possible nothing sent, don't gpf in that case...
   {
      xPtr = (char **)(*(char **)(connection->buffer - 4 )) ;   /* extra pools stored there */
      *(char **)(connection->buffer -4) = NULL ;
      while (xPtr != NULL )   /* send the additional data */
      {
         nxPtr = (char **)*xPtr ;
         len = *((long *)((char *)xPtr + 4) ) ;
         /* xPtr has 4 bytes for next pointer, 4 bytes for length, and then data */
         rc = connect4send( connection->connect, ((char *)xPtr) + 8, len ) ;
         if ( doFreeXPtr )
            u4free(xPtr) ;
         xPtr = nxPtr ;
      }
   }
   return connect4sendFlush( connection->connect ) ;
}



int connection4setStatus( CONNECTION4 *connection, const short status )
{
   CODE4 *c4 ;

   #ifdef E4PARM_LOW
      if ( connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif

   c4 = connection->cb ;
   packet4setStatus( &connection->packet, status ) ;
   #ifdef E4OFF_STRING
      return packet4setErrCode2( &connection->packet, error4code2( c4 ) ) ;
   #else
      return packet4setErrCode2( &connection->packet, error4number2( error4code2( c4 ) ) ) ;
   #endif
}



int connection4errorDescribeExecute( const CONNECTION4 *connection, CODE4 *c4, int c1, long c2, const char *s1, const char *s2, const char *s3 )
{
   long cErr ;

   cErr = connection4errCode2( connection ) ;
   if ( c2 != 0 && ( cErr > 90000 || cErr == 0 ) )
      cErr = c2 ;
   #ifndef E4OFF_STRING
      else
         cErr = error4seek( cErr ) ;    /* convert the long value to the appropriate positional value */
   #endif
   return error4describeExecute( c4, c1, cErr, s1, s2, s3 ) ;
}



CODE4 * S4FUNCTION connect4codeBase( CONNECT4 *connect )
{
   /*
      FUNCTION

      NOTES

      This function needs to be a function because it is imported into the
        communications dll's.
   */
   return connect->cb ;
}
#endif /* S4OFF_COMMUNICATIONS */
