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

/*
   FUNCTION

   long code4timeout( CODE4 *c4 )

   DESCRIPTION

   Returns the current timeout setting for CODE4

   NOTES

   cannot be inline due to DLL CODE4 structure potential mismatches
   available whether or not communications are available
*/

#ifndef S4SERVER
   long S4FUNCTION code4timeout( CODE4 *c4 )
   {
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
#endif /* S4SERVER */



#ifndef S4OFF_COMMUNICATIONS
#ifndef S4SERVER
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

#ifndef E4OFF_STRING
   extern long error4seek( long ) ;
#endif

/*
   FUNCTION

   int connection4disconnect( CONNECTION4 *connection )

   DESCRIPTION

*** REWRITE THIS FUNCTION

   This function performs a friendly or an unfriendly disconnection.

   NOTES

   If CODE4.errorCode is < 0 perform an unfriendly disconnect (just close
     down the connection).
   If CODE4.errorCode is >= 0 perform a friendly disconnect.

   To perform a friendly disconnect, send a CON4DISCONNECT request to the
     other connection.

*/
int connection4disconnect( CONNECTION4 *connection )
{
   unsigned short int mType ;

   #ifdef E4PARM_LOW
      if ( connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif
   if (!connection->connected)
      return r4success ;

   if ( error4code( connection->cb ) >= 0 )  /* do a normal disconnect message first */
   {
      mType = htons5(STREAM4DISCONNECT) ;
      connection4send( connection, (char *)&mType, sizeof( mType ) ) ;
      connection4sendFlush( connection ) ;
   }

   if ( connection->connect != 0 )
      connect4disconnect( connection->connect ) ;
   connection->connected = 0 ;
   return r4success ;
}

/*
   FUNCTION

   int connection4init( CONNECTION4 *connection, CONNECT4 *connect )
   *** note change in paramaters which is to take effect.

   DESCRIPTION

*** REWRITE THIS FUNCTION

   PARAMATERS

   connect is a connected connection

   NOTES

   Initializes the CONNECTION4 structure by clearing out memory and setting:
     connection->cb = connect->cb ;
     connection->connect = connect ;

*/
int connection4init( CONNECTION4 *connection, CONNECT4 *connect )
{
   #ifdef E4PARM_LOW
      if ( connect == 0 || connection == 0 )
         return error4( 0, e4parm_null, E90160 ) ;
   #endif

   connection->cb = connect->cb ;
   connection->connect = connect ;

   return 0 ;
}

/*
   FUNCTION

   int connection4initUndo( CONNECTION4 *connection )

   DESCRIPTION

   NOTES

   Uninitialize the CONNECTION4 structure:
     performing disconnection
*/
int connection4initUndo( CONNECTION4 *connection )
{
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

/*
   FUNCTION

   int connection4assign( CONNECTION4 *connection, const int type, const long clientId, const long serverId )

   DESCRIPTION

   Just fills in data into the static PACKET4 structure (CONNECTION4.packet)

*** REWRITE THIS FUNCTION
*/
int connection4assign( CONNECTION4 *connection, const short type, const long clientId, const long serverId )
{
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
   #endif

   connection4clear( connection ) ;

   return 0 ;
}

#define packet4connectionId( p ) ( (p)->connectionId )


#ifdef S4CLIENT
int code4ping(CODE4 *c4, long s, long r )
{
   long send, receive ;
   char *data ;
   CONNECT4 *connect = &c4->clientConnect ;
//   int rc ;

   send =  (s<10)?10:s ;
   receive = (r<0)?0:r ;
   connect4sendShort( connect, STREAM4PING ) ;
   connect4sendLong(connect, send) ;
   connect4sendLong(connect, receive) ;
   if (send > 10 )
   {
      data = (char *)u4alloc(send - 10 ) ;
      connect4send(connect, data, send - 10 ) ;
      connect4sendFlush(connect) ;
      u4free(data) ;
   }
   if (receive)
   {
      data = (char *)u4alloc(receive) ;
      connect4receive(connect, data, receive, code4timeoutVal(c4) ) ;
      u4free(data) ;
   }
   return r4success ;
}

#endif
#ifndef S4SERVER
/*
   FUNCTION

   static int code4unlockSet( CODE4 *c4 )

   DESCRIPTION

   This function is used to mark everything unlocked for the client.
   This is performed when the server tells the client that it had to unlock
   everything in order to execute the requested function
*/
static int code4unlockSet( CODE4 *c4 )
{
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
         data->appendLock = 0 ;
         data->fileLock = 0 ;
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

static void d4unlockClientData( DATA4 *data )
{
   DATA4FILE *dfile ;
   LOCK4LINK *lock ;
   SINGLE4DISTANT singleDistant ;

   #ifdef S4SERVER
      if ( data->accessMode == OPEN4DENY_RW )
         return ;
   #endif

   dfile = data->dataFile ;

   if ( dfile->fileLock == data )
   {
      data->dataFile->numRecs = -1 ;
      dfile->fileLock = 0 ;
   }
   if ( dfile->appendLock == data )
   {
      data->dataFile->numRecs = -1 ;
      dfile->appendLock = 0 ;
   }
   single4distantInitIterate( &singleDistant, &dfile->lockedRecords ) ;
   for ( ;; )
   {
      lock = (LOCK4LINK *)single4distantToItem( &singleDistant ) ;
      if ( lock == 0 )
         break ;
      if ( lock->data == data )
      {
         single4distantPop( &singleDistant ) ;
         mem4free( data->codeBase->lockLinkMemory, lock ) ;
      }
      else
         single4distantNext( &singleDistant ) ;
   }
}
#endif /* S4SERVER */




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
         #endif
         error4( c4, rc < 0 ? rc : e4net, E90160 ) ;
         #ifdef S4SERVER
            code4exitExclusiveVerifyLast( c4, client ) ;
         #endif
         return rc ;
      }

      short int messageType = ntohs5(connection->packet.type) ;

      #if defined( S4COM_PRINT ) && defined( S4SERVER )
         char buffer[128] ;
         sprintf(buffer, "Receiving Message: %s, on socket %d", s4connectionPrint( messageType ), connect->connectBuffer.connectLow->sockr ) ;
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
            return 0 ;
         }
      #endif

      #if defined( S4SERVER ) && !defined( S4OFF_THREAD )
         // BCR 10/20/00 -- only screws up mutex count in off_thread mode
         code4enterExclusiveVerifyFirst( c4, client, 1 ) ;
      #endif

      if ( messageType < 10000 )
      {
         /* old-style message, receive the whole message */

         connect4receive( connect, &connection->packet.status, sizeof( connection->packet ) - sizeof( connection->packet.type ), timeout ) ;
         long tempLen = connection4len( connection ) ;
         if ( tempLen < 0 )  /* bad length, must be a communications fault */
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
         S4LONG clientId, serverId ;

         switch( messageType )
         {
            case STREAM4DISCONNECT:
               connection->connectionFailure = e4connection ;
               return error4( c4, e4connection, E90160 ) ;
            case STREAM4UNLOCK_ALL:
               rc = code4unlockSet( c4 ) ;
               if ( rc < 0 )
                  return rc ;
               continue ;   /* this message type means that there is an additionaly regular message arriving */
            case STREAM4UNLOCK_DATA:
               /* in this case, the server sends back the clientId and serverId of who gets unlocked*/
               connect4receive( connect, &clientId, sizeof( clientId ), timeout ) ;
               connect4receive( connect, &serverId, sizeof( serverId ), timeout ) ;
               clientId = ntohl5(clientId) ;
               serverId = ntohl5(serverId) ;
              /* if ( rc != r4success )*/
              /* {*/
              /*    connection->connectionFailure = e4connection ;*/
              /*    return error4( c4, e4connection, E90160 ) ;*/
              /* }*/
               d4unlockClientData( tran4data( &(c4->c4trans.trans), serverId, clientId )) ;
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
               rc = java4processMessage( c4, client, messageType ) ;
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
            code4exitExclusiveVerifyLast( c4, client ) ;
         #endif
      #endif

      if ( rc < 0 )
         return rc ;

      return messageType ;
   }
}

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
int connection4addData( CONNECTION4 *connection, const void *data, long dataLen, void **atPtr )
{
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

/*
   FUNCTION

   int connection4clear( CONNECTION4 *connection )

   DESCRIPTION

   This function is used to reset the current buffer position to zero.
   Also, if there are extra pool pointers, they are freed.

   ERRORS

   Only severe errors are possible
*/
void connection4clear( CONNECTION4 *connection )
{
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
int S4FUNCTION connection4receive( CONNECTION4 *connection, char *data, int len )
{
   int rc ;

   rc = connect4receive( connection->connect, data, len, code4timeoutVal( connection->cb ) ) ;
   if (rc == r4success )
      return len ;
   return rc ;
}

#ifndef S4SERVER
/*
   FUNCTION

   DESCRIPTION

   uses CODE4.numAttempts to repetitively try the operation.
   uses CODE4.timeOut to wait between attempts.

   Only re-tries if r4locked failure

   NOTES

*/
short connection4repeat( CONNECTION4 *connection )
{
/*
   int rc, numAttempts ;

   #ifdef E4PARM_LOW
      if (connection == NULL )
         return e4parmNull ;
   #endif

   if ( numRepeat == -2 )
      numAttempts = connection->cb->lockAttempts ;
   else
      numAttempts = numRepeat ;

   do
   {
      connection->packet.delayHundredthsIn = (htonl5(delayHundredthsIn)) ;
      connection4sendMessage(connection) ;
      connection4clear(connection) ;
      rc = connection4receiveMessage(connection) ;
      if (rc < 0 )
         return rc ;
      rc = ntohs5( connection->packet.status ) ;
      if ( rc != r4locked )
         return rc ;
      if ( numAttempts != -1 )
         numAttempts-- ;
   } while( numAttempts != 0 ) ;

   return r4locked ;
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

      connection4sendMessageLow( connection, 0 ) ;

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
#endif /* S4SERVER */

/* needs to be exported for S4UTILS */
/* for connection4repeat(), we don't want to free the xPtr's */
int S4FUNCTION connection4sendMessageLow( CONNECTION4 *connection, char doFreeXPtr )
{
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

/*
   FUNCTION

   NOTES

   This function needs to be a function because it is imported into the
     communications dll's.
*/
CODE4 * S4FUNCTION connect4codeBase( CONNECT4 *connect )
{
   return connect->cb ;
}

#endif /* S4OFF_COMMUNICATIONS */
