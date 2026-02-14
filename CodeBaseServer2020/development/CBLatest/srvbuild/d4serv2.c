/* d4serv2.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef S4SERVER

#include "stamp5.hpp"

extern STAMP5 CustomerStamp;

#ifndef UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_COMMUNICATIONS

#ifdef S4UNIX
   #include <dirent.h>
#endif

#if defined( S4SERVER ) && defined( S4REL_PRINT ) && defined( S4TESTING )
   // include files for outputting relation stuff
   #include "t4test.c"
#endif

static int server4clientRelatePackBatch( CONNECT4 *connect, RELATION4 *relation, short doMemos ) ;
static int server4clientSendMemos( SERVER4CLIENT *client, DATA4 *data ) ;
static int server4clientCloseData( SERVER4CLIENT *, DATA4 * ) ;
static void server4clientMessageCancelAll( SERVER4CLIENT *client ) ;
static RELATION4 *client4getRelation( SERVER4CLIENT *, const unsigned long ) ;



/* not S4OFF_COMMUNICATIONS, S4SERVER */
CONNECT4HALF * code4findFirstConnection(CODE4 *cb, TCP4ADDRESS address, short port, short direction)
{
   /* This function assumes that you have waited on the list4mutex already */
   CONNECT4HALF *connectOn = 0 ;
   do
   {
      #ifdef S4OFF_THREAD
         connectOn = (CONNECT4HALF *)l4next( &cb->connectHalfList, connectOn ) ;
      #else
         connectOn = (CONNECT4HALF *)l4next( &cb->connectHalfListMutex.list, connectOn ) ;
      #endif
      if (!connectOn)
         break ;
      else if ( (!memcmp(&address, &connectOn->address, 4)) && (port == connectOn->port) && (direction == connectOn->fromClient ) )
      {
         #ifdef S4OFF_THREAD
            l4remove(&cb->connectHalfList, connectOn ) ;
         #else
            l4remove(&cb->connectHalfListMutex.list, connectOn ) ;
         #endif

         break;
      }
   } while (connectOn);

   return connectOn ;
}



#ifdef E4DEBUG
   /* E4DEBUG, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientVerifyData4s( SERVER4CLIENT *client )
   {
      // do some verification on the DATA4's...
      LIST4 *dataList = tran4dataList( &client->trans ) ;
      if ( dataList == 0 )  // early on, not set up yet...
         return 0 ;

      for ( DATA4 *data = 0 ;; )
      {
         data = (DATA4 *)l4next( dataList, data ) ;
         if ( data == 0 )
            break ;
         if ( data->dataFile == 0 )  // data4 is invalid, because no DATA4FILE associated with it...
            return -1 ;
      }

      return 0 ;
   }
#endif /* E4DEBUG */



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientAccept( SERVER4 *server, CODE4 *c4 )
{
   SERVER4CLIENT *newClient = 0 ;
   CONNECT4LOW *newSocket ;
   CONNECT4 *newConnect ;
   int rc ;
   long id ;
   #ifdef S4HALFJAVA
      CONNECT4HALF *javaPtr ;

      TCP4ADDRESS address ;
      short port ;
   #endif

   code4enterExclusive( c4, c4->catalogClient, 1 ) ;
   #ifdef E4ANALYZE
      // AS Aug 27/01 - Ensure that only we are accessing the CODE4 connectLowMemory structure
      // attempting to access memeory when we don't have exclusive CODE4 access
      if ( c4->accessMutexCount == 0 || c4->currentClient != c4->catalogClient )
         return error4( c4, e4memory, E96917 ) ;
   #endif
   newSocket = (CONNECT4LOW *)mem4allocErrZero( c4->connectLowMemory, c4 ) ;
   code4exitExclusive( c4, c4->catalogClient ) ;

   if ( newSocket == 0 )
      return error4describe( c4, e4memory, E96978, "Failure to allocate new socket", 0, 0 ) ;

   memset( newSocket->serverConnectStatus, '0', sizeof(newSocket->serverConnectStatus) ) ;

   newSocket->serverConnectStatus[0] = '1' ;  // we are allocated

   if ( connect4lowAccept( &server->listenSocket, newSocket, c4 ) < 0 )
   {
      if ( !(c4->shutdownWorkerThreads))  /* if shutting down, listen was just closed, ok. else error */
      {
         error4( c4, e4socket, E70230 ) ;
         #ifndef S4OFF_LOG
            if ( c4->logConn > 0 )
            {
               char buf[250] ;
               sprintf( buf, "ES0001:Server is terminating the accept thread due to unexpected error in connect4lowAccept" ) ;
               assert5( strlen( buf ) < sizeof( buf ) ) ;
               code4logInfo( c4, buf ) ;
            }
         #endif
      }
      else
      {
         #ifndef S4OFF_LOG
            if ( c4->logConn > 0 )
            {
               char buf[250] ;
               sprintf( buf, "ES0002:Server is terminating the accept thread as part of normal server shutdown" ) ;
               assert5( strlen( buf ) < sizeof( buf ) ) ;
               code4logInfo( c4, buf ) ;
            }
         #endif
      }

      code4enterExclusive( c4, c4->catalogClient, 1 ) ;
      mem4free(c4->connectLowMemory, newSocket) ;
      code4exitExclusive( c4, c4->catalogClient ) ;
      return e4socket ; /* Possibly not an error, but we're shutting down */
   }

   #ifndef S4OFF_LOG
      // time the amount of time that it takes for the accept to go through
      time_t startTime, endTime ;
      if ( c4->logConn > 6 )
         time( &startTime ) ;
   #endif

   /* Always read four bytes.  If id is set to -1, it means it is the second half of a connection. */
   // AS Jan 9/02 - Added a timeout paramater to use for low-read (previously used a default)
   // AS May 13/02 - communications compression coding - don't compress the connection message
   rc = connect4lowRead( newSocket, (char *)&id, 4, WAIT4EVER, 0 ) ;
   // AS Dec 8/03 - need to network swap the id
   id = ntohl5( id ) ;
   if ( rc )
   {
      #ifndef S4OFF_LOG
         if ( c4->logConn > 0 )
         {
            char buf[250] ;
            sprintf( buf, "WS0001:Warning: Server received an non-zero code on connect4lowRead() when setting up a connection" ) ;
            assert5( strlen( buf ) < sizeof( buf ) ) ;
            code4logInfo( c4, buf ) ;
         }
      #endif
      // AS Sep 21/04 - if the connection read fails, reset the Code4.errorCode so that we can continue to accept connections
      // from other clients
      error4set( c4, 0 ) ;
      // AS 08/21/00 - Was failing in JAVA clients - if the read
      // fails on the new socket, just pretend that the connection
      // failed and then go on normally.
      code4enterExclusive( c4, c4->catalogClient, 1 ) ;
      mem4free( c4->connectLowMemory, newSocket ) ;
      code4exitExclusive( c4, c4->catalogClient ) ;
      return 0 ;
   }

   newSocket->serverConnectStatus[2] = '1' ;  // we received 4 identifying bytes

   if (id == -1) /* It's a second connection (either java or c) */
   {
      newSocket->serverConnectStatus[3] = '1' ;  // 2nd connection in accept
      address = connect4lowPeerTcpAddress( newSocket ) ;
      // AS Jan 9/02 - Added a timeout paramater to use for low-read (previously used a default)
      // AS May 13/02 - communications compression coding - don't compress the connection message
      connect4lowRead( newSocket, (char *)&port, 2, WAIT4EVER, 0 ) ;
      #ifndef S4OFF_THREAD
         list4mutexWait(&c4->connectHalfListMutex) ;
      #endif
      javaPtr = code4findFirstConnection(c4, address, port, 1 ) ;
      if (javaPtr)
      {
         newSocket->serverConnectStatus[4] = '1' ;  // found javaptr in accept
         short returnValue = r4success;
         /* First Java connection has already arrived */
         // we need to ensure that the client is still valid (it may have got disconnected already)
         // it will either be valid or not; it has to wait on the connectHalfListMutex to clean itself
         // up as part of its procedure...
         SERVER4CLIENT *client = javaPtr->client ;
         #ifdef S4WINSOCK
            SOCKET socket = newSocket->sockr ;
         #else
            int socket = newSocket->sockr ;
         #endif

         if ( client != 0 )
         {
            client->connectHalf = 0 ;
            CONNECT4BUFFER *connectBuffer = &(client->connect.connectBuffer) ;
            connect4lowEnterExclusive( connectBuffer ) ;
            #ifndef S4OFF_LOG
               if ( c4->logConn > 8 )
               {
                  strcat( client->connectInfoStatus, "existing client found in d4serv2.c server4clientAccept()" ) ;
                  assert5( strlen( client->connectInfoStatus ) <= sizeof(client->connectInfoStatus) ) ;
               }
            #endif
            if ( connect4bufferLowGet( connectBuffer) != 0 )
            {
               connect4bufferLowGet( connectBuffer )->sockw = socket ;
               // AS May 13/02 - communications compression coding - don't compress the connection message
               connect4lowWrite( connect4bufferLowGet( connectBuffer ), (char *)&returnValue, 2, 0 ) ;
               assert5( connect4bufferLowGet( connectBuffer ) != newSocket ) ;
               newSocket->serverConnectStatus[5] = '1' ;  // set up sockw in accept
            }
            connect4lowExitExclusive( connectBuffer ) ;
         }
         else
         {
            // if it is zero,  don't return an error, just let the connection info disappear
            // allow to free memory by using catalog client
            #ifndef S4OFF_LOG
               if ( c4->logConn > 1 )  // can happen if client connection timed out and disconnected before we got here (when server is very busy)
               {
                  char buf[250] ;
                  sprintf( buf, "ES0010:Server unable to find expected second half of connection in server4clientAccept() for ip address: %ld.%ld.%ld.%ld",
                                (long)address.tcpAddress[0], (long)address.tcpAddress[1], (long)address.tcpAddress[2], (long)address.tcpAddress[3] ) ;
                  assert5( strlen( buf ) < sizeof( buf ) ) ;
                  code4logInfo( c4, buf ) ;
               }
            #endif
            client = c4->catalogClient ;
         }

         // release this only after we have set up all of our stuff if required (to allow the init undo to work ok)
         #ifndef S4OFF_THREAD
            list4mutexRelease( &c4->connectHalfListMutex ) ;
         #endif

         // AS July 23/02 - Don't free the memory until after we have released the
         // halflistmutex, because that mutex may cause deadlock with a thread waiting
         // to get at the code4 exclusivity.

         code4enterExclusive( c4, client ) ;
         // copy the newSocket status over to the merged socket
         if ( client->connect.connectBuffer.connectLowPtr != 0 )
            memcpy( client->connect.connectBuffer.connectLowPtr->serverConnectStatusSaved, newSocket->serverConnectStatus, sizeof( newSocket->serverConnectStatus ) ) ;

         // we don't need the newSocket since we have transferred its sockr to our existing socket
         mem4free( c4->connectLowMemory, newSocket ) ;
         newSocket = 0 ;
         #ifdef E4LINK
            assert5( javaPtr->link.n == 0 && javaPtr->link.p == 0 ) ;
         #endif
         mem4free( c4->connectHalfMemory, javaPtr ) ;
         javaPtr = 0 ;
         code4exitExclusive( c4, client ) ;
      }
      else
      {
         newSocket->serverConnectStatus[4] = '2' ;  // didn't find javaptr in accept
         javaPtr = (CONNECT4HALF *)mem4allocErrZero( c4->connectHalfMemory, 0 ) ;
         if ( javaPtr == 0 )
         {
            code4enterExclusive( c4, c4->catalogClient, 1 ) ;
            mem4free( c4->connectLowMemory, newSocket ) ;
            code4exitExclusive( c4, c4->catalogClient ) ;
            #ifndef S4OFF_THREAD
               list4mutexRelease( &c4->connectHalfListMutex ) ;
            #endif
            return error4describe( c4, e4memory, E96978, "Failure to allocate 2nd half of connection socket", 0, 0 ) ;
         }
         newSocket->serverConnectStatus[6] = '1' ;  // created new javaptr in accept
         // address and port are sufficient to uniquely identify the connection
         javaPtr->address = address ;
         javaPtr->port = port ;
         javaPtr->fromClient = 0 ;
         javaPtr->sock = newSocket->sockr ;
         javaPtr->client = 0 ;

         #ifdef S4OFF_THREAD
            l4add( &c4->connectHalfList, javaPtr ) ;
         #else
            l4add( &c4->connectHalfListMutex.list, javaPtr ) ;
            list4mutexRelease( &c4->connectHalfListMutex ) ;
         #endif

         // AS July 23/02 - Don't free the memory until after we have released the
         // halflistmutex, because that mutex may cause deadlock with a thread waiting
         // to get at the code4 exclusivity.

         code4enterExclusive( c4, c4->catalogClient, 1 ) ;
         mem4free( c4->connectLowMemory, newSocket ) ;
         code4exitExclusive( c4, c4->catalogClient ) ;
      }
      #ifndef S4OFF_LOG
         if ( c4->logConn > 6 )
         {
            time( &endTime ) ;
            long elapsedThousandths = (long)( difftime( endTime, startTime ) * 1000 );
            char buf[250] ;
            sprintf( buf, "IS0001:Elapsed time in Server4accept for client: %ld", elapsedThousandths ) ;
            assert5( strlen( buf ) < sizeof( buf ) ) ;
            code4logInfo( c4, buf ) ;
         }
      #endif
      return r4success ;
   }

   #ifdef S4DEAD_CHECK
      if (id)
      {
         connect4lowClose(newSocket ) ;
         code4enterExclusive( c4, c4->catalogClient, 1 ) ;
         mem4free(c4->connectLowMemory, newSocket) ;
         code4exitExclusive( c4, c4->catalogClient ) ;
         newClient = server4clientId(server, id ) ;
         if (newClient)
            newClient->timeCount = server->timeStamp ;
         server4verifyClients(server) ;
         return r4success ;
      }
      else
         server4verifyClients(server) ;
   #endif

   // AS Apr 28/03 - made trans-shared a run-time switch
   if ( c4->transShared == 1 )
   {
      // AS Apr 21/03 - set the clientId to offset to the transaction id if using a shared transaction file (odbc server)
      TRAN4 *trans = &c4->catalogClient->trans ;
      server->clientIdCount += TRAN4MAX_USERS ;
   }
   else
      server->clientIdCount++ ;

   code4enterExclusive( c4, c4->catalogClient, 1 ) ;
   newClient = (SERVER4CLIENT *)mem4allocErrZero( c4->clientMemory, c4 ) ; // CS move up
   if (newClient == 0 )
   {
      connect4lowClose(newSocket) ;
      mem4free(c4->connectLowMemory, newSocket) ;
      code4exitExclusive( c4, c4->catalogClient ) ;
      return error4describe(c4, e4memory, E96978, "Failure to allocate new client", 0, 0) ;
   }
   code4exitExclusive( c4, c4->catalogClient ) ;

   #ifndef S4OFF_LOG
      if ( c4->logConn > 8 )
      {
         strcat( newClient->connectInfoStatus, "new client created by server in server4clientAccept()" ) ;
         assert5( strlen( newClient->connectInfoStatus ) <= sizeof(newClient->connectInfoStatus) ) ;
      }
   #endif

   rc = server4clientInit( newClient, server, server->clientIdCount ) ;
   if (rc < 0 )
   {
      #ifndef S4OFF_LOG
         if ( c4->logConn > 0 )
         {
            strcat( newClient->connectInfoStatus, " ERROR: FAILED TO INITIALIZE CLIENT in server4clientAccept()" ) ;
            assert5( strlen( newClient->connectInfoStatus ) <= sizeof(newClient->connectInfoStatus) ) ;
         }
      #endif
      connect4lowClose(newSocket) ;
      code4enterExclusive( c4, c4->catalogClient, 1 ) ;
      mem4free(c4->connectLowMemory, newSocket) ;
      mem4free(c4->clientMemory, newClient ) ;
      code4exitExclusive( c4, c4->catalogClient ) ;
      return 0 ;
   }
   #ifdef S4DEAD_CHECK
      newClient->timeCount = server->timeStamp ;
   #endif

   newConnect = &newClient->connect ;
   newConnect->client = newClient ;
   newConnect->cb = c4 ;

   assert5( newConnect->link.n == 0 && newConnect->link.p == 0 ) ;
   connection4init( &newClient->connection, newConnect ) ;

   rc = connect4init( newConnect, &newSocket, c4 ) ;
   if (rc < 0 )
   {
      #ifndef S4OFF_LOG
         if ( c4->logConn > 0 )
         {
            strcat( newClient->connectInfoStatus, " ERROR: FAILED TO connect4init() CLIENT in server4clientAccept()" ) ;
            assert5( strlen( newClient->connectInfoStatus ) <= sizeof(newClient->connectInfoStatus) ) ;
         }
      #endif
      code4enterExclusive( c4, newClient, 1 ) ;  // must delay or else will block out other client
      #ifndef S4OFF_LOG
         if ( c4->logConn > 6 )
         {
            char buf[250] ;
            sprintf( buf, "IS0020:Server is uninitializing client due to failure to complete the connection process for" ) ;
            assert5( strlen( buf ) < sizeof( buf ) ) ;
            code4logInfo( c4, buf ) ;
         }
      #endif
      server4clientInitUndo( newClient ) ;
      code4exitExclusive( c4, newClient ) ;

      connect4lowClose( newSocket ) ;
      code4enterExclusive( c4, c4->catalogClient, 1 ) ;
      if ( newSocket != 0 ) // AS Sept 26/02 - avoid double free
         mem4free(c4->connectLowMemory, newSocket ) ;
      // AS Oct 4/02 - the new Client would already have been freed here, so skip
      // mem4free(c4->clientMemory, newClient ) ;
      code4exitExclusive( c4, c4->catalogClient ) ;
      return 0 ;
   }

   #ifndef S4OFF_LOG
      if ( c4->logConn > 8 )
      {
         strcat( newClient->connectInfoStatus, " connection initialized in server4clientAccept()" ) ;
         assert5( strlen( newClient->connectInfoStatus ) <= sizeof(newClient->connectInfoStatus) ) ;
      }
   #endif
   // AS July 24/02 - don't register as connected until initilized...
   newClient->connection.connected = 1 ;
   server4clientListAdd( server, newClient ) ;

   return r4success ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
static SERVER4CLIENT *server4clientFromId( SERVER4 *server, long id )
{
   SERVER4CLIENT *client ;

   server4clientListReserve( server ) ;
   for ( client = 0 ;; )
   {
      client = server4clientGetNext( server, client ) ;
      if ( client == 0 )
      {
         server4clientListRelease( server ) ;
         return 0 ;
      }
      if ( client->id == id )
      {
         server4clientListRelease( server ) ;
         return client ;
      }
   }
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCancel( SERVER4CLIENT *client )
{
   CONNECTION4CLIENT_CANCEL_INFO_IN *info ;
   SERVER4CLIENT *cancelClient ;
   #ifdef E4ANALYZE
      int rc ;
   #endif

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   if ( connection4len( &client->connection ) != sizeof( CONNECTION4CLIENT_CANCEL_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4CLIENT_CANCEL_INFO_IN *)connection4data( &client->connection ) ;

   connection4clear( &client->connection ) ;

   // AS 12/06/99 -- id for sending changed to character string for ODBC large connection ids.
   // get the connectionId and make it into a LONGLONG...
   // note that for LINUX, for now, probably can assume that it will be the size of a long,
   // so can do a atol for LINUX.

   #ifdef S4ODBC_ENABLED
      LONGLONG clientID = _atoi64( info->clientId ) ;

      // if the clientID > ULONG_MAX, then we know is an ODBC client, else is CodeBase client...
      if ( clientID > ULONG_MAX )
      {
         // ODBC client
         odbc4cutOneClient( clientID ) ;
         // if non-odbc enabled server, assume input just a bad client id...
         return 0 ;
      }
   #else
      // non ODBC, assume id just not a LONGLONG
      long clientID = c4atol( info->clientId, sizeof( info->clientId ) ) ;
   #endif

   long clientLong = (long)clientID ;  // we know client id here < ULONG_MAX, so coerce to long )
   cancelClient = server4clientFromId( client->server, clientLong ) ;
   if ( cancelClient == 0 )   /* no such client--client considered canceled */
      return 0 ;

   #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
      if ( cancelClient->trans.currentTranStatus == r4partial )
         server4clientTransactionCommitPhaseTwo( cancelClient ) ;
      else
         if ( cancelClient->trans.currentTranStatus == r4active )
            server4clientTransactionRollback( cancelClient ) ;
   #endif


   #ifndef S4OFF_LOG
      if ( client->server->c4->logConn > 6 )
      {
         char buf[250] ;
         sprintf( buf, "IS0050:Server is disconnecting client due to server4clientCancel() call for userId: [%s] and ip adress: [%s] by userId: [%s] and ip adress: [%s]",
                       cancelClient->account.accountId, cancelClient->account.tcpAddress,
                       client->account.accountId, client->account.tcpAddress ) ;
         assert5( strlen( buf ) < sizeof( buf ) ) ;
         code4logInfo( client->server->c4, buf ) ;
      }
   #endif

   if ( client == cancelClient )
   {
      server4disconnect( client->server, cancelClient ) ;
      return r4exit ;
   }
   else
   {
       code4exitExclusive( client->server->c4, client ) ;
       code4enterExclusive( client->server->c4, cancelClient, 1 ) ;
       server4disconnect( client->server, cancelClient ) ;
       code4exitExclusive( client->server->c4, cancelClient ) ;
       code4enterExclusive( client->server->c4, client, 1 ) ;
   }

   return 0 ;
}

#endif /* not S4OFF_COMMUNICATIONS */



#ifdef S4DEAD_CHECK
   /* S4DEAD_CHECK, S4SERVER */
   SERVER4CLIENT *server4clientId( SERVER4 S4PTR *server, const long id )
   {
      SERVER4CLIENT *clientOn, *clientNext ;

      list4mutexWait( &server->clients ) ;
      clientNext = (SERVER4CLIENT *)l4first( &server->clients.list ) ;

      for ( clientOn = 0 ;; )
      {
         if ( clientNext == 0 )
            break ;
         clientOn = clientNext ;
         clientNext = (SERVER4CLIENT *)l4next( &server->clients.list, clientOn ) ;
         if (clientOn->id == id )
         {
            list4mutexRelease( &server->clients ) ;
            return clientOn ;
         }
      }
      list4mutexRelease( &server->clients ) ;
      return 0 ;
   }
#endif /* S4DEAD_CHECK */



/* S4SERVER */
int S4FUNCTION server4clientInit( SERVER4CLIENT *client, SERVER4 *server, const long id )
{
   int rc ;

   #ifdef E4PARM_LOW
      if ( client == 0 || server == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   memset( client, 0, sizeof( SERVER4CLIENT ) ) ;

   client->id = id ;
   time( &client->info.connectionStart ) ;
   #ifdef E4ANALYZE
      rc = server4verify( server, 1 ) ;
      if ( rc < 0 )
         return rc ;
   #endif

   rc = code4tranInitLow( &client->trans, &server->c4trans ) ;
   if ( rc < 0 )
      return rc ;

   client->server = server ;
   client->errCreate = 1 ;

   #ifdef S4JAVA
      client->errDefaultUnique = r4uniqueContinue ;
      client->accessMode = OPEN4DENY_NONE ;
      client->readLock = 0 ;
      client->safety = 1 ;
   #endif

   client->readOnly = server->c4->readOnlyDefault ;

   // AS Sep 15/03 - Initialize the memoSizeMemoExpr to a default start value (for ODBC in particular)
   // this is due to a change to allow the client to affect this value for the server
   client->memSizeMemoExpr = 0x400 ;

   #ifdef S4COMPRESS
      // AS May 13/04 - configureable compression levels
      client->fileCompressLevel = 9 ;   // default to maximum file compression
   #endif

   // AS Jul 7/06 - initialize the block sizes for the client as well...due to other applied code changes
   #ifdef S4FOX
      client->foxCreateIndexBlockSize = 512 ;
      client->foxCreateIndexMultiplier = 1 ;
   #endif


   #ifdef E4ANALYZE
      return server4clientVerify( client, 1 ) ;
   #else
      return 0 ;
   #endif
} /* server4clientInit() */



#ifndef S4OFF_COMMUNICATIONS
   /* S4SERVER, !S4OFF_COMMUNICATIONS */
   static void server4clientMessageSendInitUndo( SERVER4CLIENT *client, MESSAGE4SEND *messageSent ) ;
   static void server4clientMessageRecvInitUndo( SERVER4CLIENT *client, MESSAGE4RECEIVE *message ) ;
   static void server4pipeInitUndo( SERVER4 *server, PIPE4 *pipe )
   {
      l4remove( &server->pipes, pipe ) ;
      for ( ;; )
      {
         MESSAGE4RECEIVE *message = (MESSAGE4RECEIVE *)l4first( &pipe->messageList ) ;
         if ( message == 0 )
            break ;
         if ( message->messageSend != 0 )
            server4clientMessageSendInitUndo( message->messageSend->sender, message->messageSend ) ;
         server4clientMessageRecvInitUndo( pipe->owner, message ) ;
      }
      u4free( pipe->name ) ;
      mem4free( server->pipeMemory, pipe ) ;

   }



   /* S4SERVER, !S4OFF_COMMUNICATIONS */
   static void server4pipeRemoveClientPipes( SERVER4 *server, SERVER4CLIENT *client )
   {
      PIPE4 *pipeCur = 0 ;
      for ( ;; )
      {
         PIPE4 *pipeOn = (PIPE4 *)l4next( &server->pipes, pipeCur ) ;
         if ( pipeOn == 0 )
            break ;
         if ( pipeOn->owner == client )
            server4pipeInitUndo( server, pipeOn ) ;
         else
            pipeCur = pipeOn ;
      }
   }
#endif /* S4OFF_COMMUNICATIONS */



/* S4SERVER */
int server4clientInitUndo( SERVER4CLIENT *client, int doDelay )
{
   int rc, saveRc, oldRc ;
   RELATION4 *relation ;
   CODE4 *c4 ;
   #ifndef S4OFF_COMMUNICATIONS
      DATA4 *data ;
      LIST4 *list ;
   #endif

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef S4TESTING
      if ( client->getBuf )
         u4free( client->getBuf ) ;
   #endif
   if ( client->server == 0 )  // happens if catalogClient fails in initialization
      return -1 ;

   c4 = client->server->c4 ;

   // AS Mar 23/10 - incorrect sequencing; cannot enter exclusive on client if the halflistmutex is not held, so do this operation first
   if ( client->connectHalf != 0 )  // probably need to mark as being reset
   {
      // ensure the accept thread is not marking our structure...
      #ifndef S4OFF_THREAD
         list4mutexWait(&c4->connectHalfListMutex) ;
      #endif

      // AS Jan 21/02 - occasionally the connectHalf will be nulled out while we
      // are waiting to get at the list.
      if ( client->connectHalf != 0 )  // probably need to mark as being reset
         client->connectHalf->client = 0 ;  // accept thread will clean up the connectHalf memory...

      #ifndef S4OFF_THREAD
         list4mutexRelease( &c4->connectHalfListMutex ) ;
      #endif
   }

   code4enterExclusive( c4, client, doDelay ) ;

   #ifdef OLEDB5BUILD
      /* do this early so we get all the data4's contained therein properly closed */
      client->indexes.freeAll() ;  /* delete all indexes */
      client->tables.freeAll() ;
      client->rowsets.freeAll() ;
      client->indexRanges.freeAll() ;

      if ( client->session != 0 )
         delete client->session ;
   #endif

   #ifdef E4LINK
      if ( client->link.n != 0 ) /* still on server list, this should not happen (called by server4disconnect) */
         return error4( c4, e4result, E91530 ) ;
   #endif

   oldRc = error4set( c4, 0 ) ;

   #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
      if ( client->trans.currentTranStatus == r4partial )
         server4clientTransactionCommitPhaseTwo( client ) ;
      if ( client->trans.currentTranStatus == r4active )
         server4clientTransactionRollback( client ) ;
   #endif

   saveRc = rc = 0 ;

   code4calcReset( c4 ) ;

   for ( relation = 0 ;; )
   {
      relation = (RELATION4 *)l4first( &client->relations ) ;
      if ( relation == 0 )
         break ;
      l4remove( &client->relations, relation ) ;
      rc = relate4free( &relation->relate, 1 ) ;
      if ( rc < 0 )   /* can't recover from a failed close... */
         break ;
   }

   if ( rc != 0 )
   {
      saveRc = rc ;
      rc = 0 ;
   }

   #ifndef S4OFF_COMMUNICATIONS
      list = tran4dataList( &client->trans ) ;
      if ( list != 0 )
         for ( data = 0 ;; )
         {
            data = (DATA4 *)l4first( list ) ;
            if ( data == 0 )
               break ;

            data->clientId = 0 ;
            rc = D4close( data ) ;

            if ( rc < 0 )   /* can't recover from a failed close... */
            {
               saveRc = rc ;
               if ( data == (DATA4 *)l4first( list ) ) /* avoid repetitive failed loop */
                  l4pop( list ) ;
            }
         }
   #endif /* S4OFF_COMMUNICATIONS */

   #ifndef S4OFF_WRITE
      #ifndef S4OFF_TRAN
         #ifndef S4OFF_COMMUNICATIONS
            if ( client->connection.connected )
               code4tranInitUndoLow( &client->trans, (unsigned long)client->id ) ;
            else
         #endif /* S4OFF_COMMUNICATIONS */
         {
            if ( client == c4->catalogClient )
               code4tranInitUndoLow( &client->trans, client->id ) ;  // AS Apr 24/03 - Use client->id (for odbc it differs for each process)
            else
            {
               #ifdef S4OFF_COMMUNICATIONS
                  code4tranInitUndoLow( &client->trans, client->id ) ;
               #else
                  code4tranInitUndoLow( &client->trans, 0 ) ;
               #endif
            }
         }
      #endif
   #endif

   #ifndef S4OFF_COMMUNICATIONS
      if ( client->connection.connected )
      {
         #ifndef S4OFF_LOG
            if ( c4->logConn > 6 )
            {
               char buf[250] ;
               sprintf( buf, "IS0010:Server is disconnecting client due to server4clientInitUndo() call for userId: [%s] and ip adress: [%s]",
                             client->account.accountId, client->account.tcpAddress ) ;
               assert5( strlen( buf ) < sizeof( buf ) ) ;
               code4logInfo( c4, buf ) ;
            }
         #endif
         rc = connection4initUndo( &client->connection ) ;
      }

      // AS 10/06/00 - only if communications
      server4clientMessageCancelAll( client ) ;
   #endif /* S4OFF_COMMUNICATIONS */

   assert5( l4numNodes( &client->messageSendList ) == 0 ) ;     // ensure all messages removed / freed

   code4exitExclusive(c4, client) ;

   if ( oldRc != 0 )
      error4set( c4, oldRc ) ;
   if ( saveRc == 0 )
      saveRc = rc ;

   return saveRc ;
} /* server4clientInitUndo() */



#ifndef S4OFF_COMMUNICATIONS
   /* not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientPack( SERVER4CLIENT *client )
   {
      DATA4 *data ;
      DATA4FILE *dataFile ;
      int rc ;
      short int dataFileLocked ;

      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E91530 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
      if ( data == 0 )
         return e4name ;
      dataFile = data->dataFile ;

      dataFileLocked = (short int)dfile4lockTestFile( dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 ;
      rc = D4pack( data ) ;
      if ( rc < 0 )
      {
         if ( dataFileLocked == 0 )
            if ( dfile4lockTestFile( dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 )
               dfile4unlockFile( dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ) ) ;
         return rc ;
      }
      dataFileLocked = (short int)dfile4lockTestFile( dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 ;
      connection4clear( &client->connection ) ;
      connection4addData( &client->connection, &dataFileLocked, sizeof( short int ), 0 ) ;
      return rc ;
   }



   #ifndef S4OFF_SECURITY
      /* not S4OFF_SECURITY, not S4OFF_COMMUNICATIONS, S4SERVER */
      int server4clientPasswordSet( SERVER4CLIENT *client )
      {
         #ifdef E4PARM_LOW
            if ( client == 0 )
               return error4( 0, e4parm_null, E91530 ) ;
         #endif

         int rc ;

         #ifdef E4ANALYZE
            if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
               return rc ;
         #endif

         SERVER4 *server = client->server ;
         CODE4 *c4 = server->c4 ;
         unsigned short (*len)[3] ; /* Pointer to an array of 3 shorts */
         len = (unsigned short int (*)[3])connection4data( &client->connection ) ;

         if ( connection4len( &client->connection ) != (long)sizeof(len) + (*len)[0] + (*len)[1] + (*len)[2] )
            return e4packetLen ;

         const char *user = (const char *)connection4data( &client->connection ) + sizeof( len ) ;
         const char *oldPass = user + (*len)[0] ;
         const char *newPass = oldPass + (*len)[1] ;

         code4enterExclusive( c4, c4->catalogClient, 1 ) ;

         d4tagSelect( server->dbAuth, server->granteeTag ) ;
         rc = d4seek( server->dbAuth, user ) ;

         if ( rc != 0 )
            rc = e4seek ;
         else
         {
            const char *fld = f4ptr( server->pass ) ;

            unsigned short chLen = (unsigned short)f4len( server->pass ) ;

            for ( ; chLen > 0 ; chLen-- )
            {
               if ( fld[chLen-1] != ' ' )
                  break ;
            }

            if ( chLen != (*len)[1] )  /* password mismatch in length */
               rc = e4authorize ;
            else
            {
               if ( memcmp( oldPass, fld, chLen ) != 0 )  /* password mismatch */
                  rc = e4authorize ;
               else
               {
                  f4assign( server->pass, newPass ) ;
                  rc = d4update( server->dbAuth ) ;
                  if ( rc != 0 )   /* make sure then to not modify record */
                     server->dbAuth->recordChanged = 0 ;
               }
            }
         }

         code4exitExclusive(c4, c4->catalogClient) ;

         return rc ;
      } /* server4clientPasswordSet() */
   #endif /* S4OFF_SECURITY */
#endif /* not S4OFF_COMMUNICATIONS */



#if ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) ) && !defined( S4OFF_TRAN ) && !defined( S4OFF_ODBC_TRANS )
   int code4transServerShareInit( CODE4TRANS *c4trans )
   {
      CODE4 *c4 = c4trans->c4 ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      assert5( c4->transShared == 1 ) ;
      // initializes the shared memory fileused to co-ordinate multiple users of the transaction file
      c4trans->odbcTransShare = share4init( c4, "ODBC4TRANS_SHARE", ODBC4TRANS_SHARE_LEN, 1 ) ;
      if ( c4trans->odbcTransShare == 0 )
         return error4describe( c4, e4server, E70106, "Failed to create shared memory for odbc transactions", 0, 0 ) ;
      #ifdef S4ODBC_ENABLED
         // initialize the memory count to 0
         long zero = 0 ;
         if ( share4putData( c4trans->odbcTransShare, 0, &zero, sizeof( long ) ) != 0 )
            return error4describe( c4, e4server, E70106, "Failed to zero shared memory for odbc transactions", 0, 0 ) ;
      #endif
      return 0 ;
   }



   int code4transServerShareInitUndo( CODE4TRANS *c4trans )
   {
      // AS Apr 28/03 - made trans-shared a run-time switch
      assert5( c4trans->c4->transShared == 1 ) ;
      if ( c4trans->odbcTransShare != 0 )
      {
         share4initUndo( c4trans->odbcTransShare ) ;
         c4trans->odbcTransShare = 0 ;
      }
      return 0 ;
   }



   static int code4transServerShareLock( CODE4TRANS *c4trans, long lockByte )
   {
      // locks a byte in the shared memory file used to co-ordinate multiple users of the transaction file
      // set to -1 to lock the next available transaction byte
      // there exists a lock for every transaction in progress.  These locks start at the start of the lock
      // area and move too the end.  The unlock at then end and move to the start.  This is regardless of
      // who actually placed the lock.  If we fail to lock due to r4locked we attempt another.
      CODE4 *c4 = c4trans->c4 ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      assert5( c4->transShared == 1 ) ;
      int oldAttempts = c4->lockAttempts ;
      if ( lockByte != -1 )
      {
         c4->lockAttempts = -1 ;
         int rc = share4lock( c4trans->odbcTransShare, lockByte ) ;
         c4->lockAttempts = oldAttempts ;
         return rc ;
      }

      // start at 'next' spot, if we fail move backwards.  We will get a lock <= numLocks or else the count is wrong
      long numLocks = 0 ;
      if ( share4getData( c4trans->odbcTransShare, 0, &numLocks, sizeof( long ) ) != 0 )
         return error4describe( c4, e4server, E70106, "Failed to get transaction count for odbc transactions", 0, 0 ) ;
      c4->lockAttempts = 1 ;
      long lockPos = 0 ;
      for( ;; )
      {
         int rc = share4lock(c4trans->odbcTransShare, TRAN4LOCK_START_TRANS + lockPos ) ;
         if ( rc != r4locked )
            break ;
         lockPos++ ;
         if ( lockPos > TRAN4LOCK_NUM_USERS )  // more locks than there should be, must be corruption
            return error4describe( c4, e4server, E70106, "transaction share count corrupt for odbc transactions", 0, 0 ) ;
      }
      c4->lockAttempts = oldAttempts ;
      return 0 ;
   }



   static int code4transServerShareUnlock( CODE4TRANS *c4trans, long unlockByte )
   {
      // unlocks a byte in the shared memory file used to co-ordinate multiple users of the transaction file
      // set to -1 to unlock the next available transaction byte
      // if we fail to unlock because we don't hold the lock, we just try another.
      CODE4 *c4 = c4trans->c4 ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      assert5( c4->transShared == 1 ) ;
      if ( unlockByte != -1 )
         return share4unlock( c4trans->odbcTransShare, unlockByte ) ;

      // start at 'last' spot, if we fail move backwards.  We will get an unlock <= numLocks or else the count is wrong
      long numLocks = 0 ;
      if ( share4getData( c4trans->odbcTransShare, 0, &numLocks, sizeof( long ) ) != 0 )
         return error4describe( c4, e4server, E70106, "Failed to get transaction count for odbc transactions", 0, 0 ) ;
      long lockPos = 0 ;
      c4->errUnlock = 0 ;
      for( ;; )
      {
         #ifdef E4ANALYZE
            if ( TRAN4LOCK_START_TRANS + lockPos == TRAN4LOCK_START_COMMIT )   // should not be unlocking this byte
               return error4( c4, e4info, E70106 ) ;
         #endif
         int rc = share4unlock( c4trans->odbcTransShare, TRAN4LOCK_START_TRANS + lockPos ) ;
         if ( rc != e4unlock )
            break ;
         // error4set( c4, 0 ) ;
         lockPos++ ;
         if ( lockPos > TRAN4LOCK_NUM_USERS )  // more locks than there should be, must be corruption
         {
            c4->errUnlock = 1 ;
            return error4describe( c4, e4server, E70106, "transaction share count corrupt for odbc transactions", 0, 0 ) ;
         }
      }
      c4->errUnlock = 1 ;
      return 0 ;
   }



   static int code4transServerVerify( CODE4TRANS *c4trans, short minVal )
   {
      // verifies the lock byte in the shared memory file used to co-ordinate multiple users of the transaction file
      // It is assumed that the share lock TRAN4LOCK_START_COMMIT is locked
      // the caller of this function.  Failure to do so can cause file and transaction corruption.
      CODE4 *c4 = c4trans->c4 ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      assert5( c4->transShared == 1 ) ;
      long numLocks = 0 ;
      if ( share4getData( c4trans->odbcTransShare, 0, &numLocks, sizeof( long ) ) != 0 )
         return error4describe( c4, e4server, E70106, "Failed to get transaction count for odbc transactions", 0, 0 ) ;

      if ( numLocks >= TRAN4LOCK_NUM_USERS || numLocks < minVal )  // exceeded capacity
         return error4describe( c4, e4server, E70106, "Invalid transaction count for odbc transactions", 0, 0 ) ;

      // simply verify that # of locks in shared area == numLocks value
      int lockPos = 0 ;
      while( numLocks > 0 )
      {
         if ( lockPos > TRAN4LOCK_NUM_USERS ) // too many users
         {
            error4describe( c4, e4server, E70106, "Shutting down server due to failure of a process with a pending transaction.", 0, 0 ) ;
            server4quit( c4->server, r4shutdown ) ;  /* this function does not return */
         }
         if ( share4lock( c4trans->odbcTransShare, TRAN4LOCK_START_TRANS + lockPos ) == r4locked )
            numLocks-- ;
         else
         {
            // otherwise, we need to unlock
            share4unlock( c4trans->odbcTransShare, TRAN4LOCK_START_TRANS + lockPos ) ;
         }
         lockPos++ ;
      }

      return 0 ;
   }



   static int code4transServerChangeActive( CODE4TRANS *c4trans, int quantity )
   {
      // increments or decrements the active transaction count.  It is assumed that the share lock TRAN4LOCK_START_COMMIT is locked
      // the caller of this function.  Failure to do so can cause file and transaction corruption.
      CODE4 *c4 = c4trans->c4 ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      assert5( c4->transShared == 1 ) ;
      long numLocks = 0 ;
      if ( share4getData( c4trans->odbcTransShare, 0, &numLocks, sizeof( long ) ) != 0 )
         return error4describe( c4, e4server, E70106, "Failed to get transaction count for odbc transactions", 0, 0 ) ;
      numLocks += quantity ;
      if ( share4putData( c4trans->odbcTransShare, 0, &numLocks, sizeof( long ) ) != 0 )
         return error4describe( c4, e4server, E70106, "Failed to zero shared memory for odbc transactions", 0, 0 ) ;
      return 0 ;
   }
#endif


static Bool5 server4clientActiveTransactions( SERVER4 S4PTR *server )
{
   // returns true if a client has an active transaction in progress
   SERVER4CLIENT *clientOn, *clientNext ;
   Bool5 activeTransactions = 0 ;

   list4mutexWait( &server->clients ) ;
   clientNext = (SERVER4CLIENT *)l4first( &server->clients.list ) ;

   for ( clientOn = 0 ;; )
   {
      if ( clientNext == 0 )
         break ;
      clientOn = clientNext ;
      clientNext = (SERVER4CLIENT *)l4next( &server->clients.list, clientOn ) ;
      if (clientOn->trans.currentTranStatus != r4inactive )
      {
         activeTransactions = 1 ;
         break ;
      }
   }
   list4mutexRelease( &server->clients ) ;
   return activeTransactions ;
}



#if !defined( S4OFF_TRAN ) && !defined( S4ODBC_BUILD )
   // AS Mar 20/03 - not supported with ODBC (we only support the auto-delete of the log file on startup)
   // AS May 14/03 - do make available with ODBC if odbc-trans are disabled...
   static void server4clientLogDisableIfTemp( SERVER4CLIENT *client )
   {
      // AS Aug 13/01 - internal function to centralize code
      // used to disable log file in the instance where it was temporarily enabled
      TRAN4FILE *t4 = client->trans.c4trans->transFile ;
      SERVER4 *server = client->server ;

      // AS Aug 13/01 - to allow logging into the backup log file only, not the primary log file
      if ( server4clientActiveTransactions( server ) == 0 )
      {
         // only close/disable the log file if it was temporarily enabled and no clients have active transactions

         if ( t4->primary.isDisabled == log4tempUncompressed )
         {
            // in this instance, the LOG setting is at '5' (normally means no log file).  After
            // a transaction has begun the log file is left in place until we reach a maximum
            // size, after which it is disabled again.  On re-enabling, all of the open-table
            // information needs to be added back in.
            if ( file4longGreater( t4->primary.fileLen, server->maxLogSize ) ) // truncate it...
            {
               int oldBackupState = t4->backup.validState ;
               t4->backup.validState = 0 ;
               int rc = code4transInitUndo( &(server->c4trans) ) ;
               if ( rc == 0 )
               {
                  server->c4trans.transFile = &server->transFile ;
                  t4 = client->trans.c4trans->transFile ;
                  t4->primary.isDisabled = log4compressed ;
                  u4remove( server->logName ) ;
               }
               t4->backup.validState = oldBackupState ;
            }
            else
               return ;
         }
         else if ( t4->primary.isDisabled == log4tempEnabled )
         {
            // don't disable the backup (don't shut it down)
            int oldBackupState = t4->backup.validState ;
            t4->backup.validState = 0 ;
            int rc = tran4fileLowClose( &(t4->primary) ) ;
            if ( rc == 0 )
            {
               t4->primary.isDisabled = log4disabled ;
               u4remove( server->logName ) ;
            }
            t4->backup.validState = oldBackupState ;
         }
         else
            return ;

         // don't forget to mark the clients as not being transaction enabled...
         SERVER4CLIENT *clientOn ;
         list4mutexWait( &server->clients ) ;

         // if a rollback is coming from server4clientInitUndo() - a dead client - the current client
         // will not be on the client list, but we need to service it.  Therefore track it as well...
         Bool5 currentClientFound = 0 ;

         for ( clientOn = 0 ;; )
         {
            clientOn = (SERVER4CLIENT *)l4next( &server->clients.list, clientOn ) ;
            if ( clientOn == 0 )
               break ;
            clientOn->transEnabled = 0 ;
            if ( clientOn == client )
               currentClientFound = 1 ;
            // and mark each DATA4 as not being logged (so no TRAN4CLOSE inserted, etc.)
            for ( DATA4 *dataOn = 0 ;; )
            {
               dataOn = (DATA4 *)l4next( tran4dataList( &clientOn->trans ), dataOn ) ;
               if ( dataOn == 0 )
                  break ;
               dataOn->openWasLogged = 0 ;
            }
         }

         if ( currentClientFound == 0 && client != 0 )
         {
            clientOn = client ;
            clientOn->transEnabled = 0 ;
            // and mark each DATA4 as not being logged (so no TRAN4CLOSE inserted, etc.)
            for ( DATA4 *dataOn = 0 ;; )
            {
               dataOn = (DATA4 *)l4next( tran4dataList( &clientOn->trans ), dataOn ) ;
               if ( dataOn == 0 )
                  break ;
               dataOn->openWasLogged = 0 ;
            }
         }

         list4mutexRelease( &server->clients ) ;
      }
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4ODBC_BUILD ) */



#ifndef S4OFF_TRAN
   static int server4clientTransPhaseTwoInternal( SERVER4CLIENT *client )
   {
      // AS Aug 13/01 - internal function to centralize code
      int rc = tran4lowCommitPhaseTwo( &client->trans, client->id, 1 ) ;
      if ( rc != 0 )
         return rc ;
      // AS Mar 20/03 - not supported with ODBC (we only support the auto-delete of the log file on startup)
      // AS May 14/03 - do make available with ODBC if odbc-trans are disabled...
      #if !defined( S4OFF_TRAN ) && !defined( S4ODBC_BUILD )
         #ifdef S4ODBC_ENABLED
            if ( client->server->odbcTrans == 0 )
         #endif
               server4clientLogDisableIfTemp( client );
      #endif
      return 0 ;
   }
#endif /* S4OFF_TRAN */



/* S4SERVER */
int server4clientTransactionCommitPhaseTwo( SERVER4CLIENT *client )
{
   #ifdef S4OFF_TRAN
      return 0 ;
   #else
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         #ifdef E4ANALYZE
            int rc ;
            if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
               return rc ;
         #endif
         #ifndef S4OFF_COMMUNICATIONS
            connection4clear( &client->connection ) ;
         #endif

         return server4clientTransPhaseTwoInternal( client ) ;

      #endif /* S4OFF_WRITE */
   #endif /* S4OFF_TRAN */
}



/* S4SERVER */
int server4clientTransactionRollback( SERVER4CLIENT *client )
{
   #ifdef S4OFF_TRAN
      return e4notSupported ;
   #else
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         int rc ;

         #ifdef E4ANALYZE
            if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
               return rc ;
         #endif

         #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS )
            SERVER4 *server = client->server ;
            // AS Apr 28/03 - made trans-shared a run-time switch
            if( server->c4->transShared == 1 )
            {
               // Sequence to rollback a transaction
               //    - lock start/commit byte
               //    - verify the transaction file is valid
               //    - perform the rollback or commit
               //    - unlock our id byte
               //    - decrement numActiveTrans by 1
               //    - unlock start/commit byte
               code4transServerShareLock( &server->c4trans, TRAN4LOCK_START_COMMIT ) ;
               code4transServerVerify( &server->c4trans, 1 ) ;
            }
         #endif
         rc = tran4lowRollback( &client->trans, client->id, 1 ) ;
         #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS )
            // AS Apr 28/03 - made trans-shared a run-time switch
            if( server->c4->transShared == 1 )
            {
               code4transServerShareUnlock( &server->c4trans, -1 ) ;
               code4transServerChangeActive( &server->c4trans, -1 ) ;
               code4transServerShareUnlock( &server->c4trans, TRAN4LOCK_START_COMMIT ) ;
            }
         #endif
         if ( rc != 0 )
            return rc ;
         // AS Mar 20/03 - not supported with ODBC (we only support the auto-delete of the log file on startup)
         #if !defined( S4OFF_TRAN ) && !defined( S4ODBC_ENABLED ) && !defined( S4ODBC_BUILD )
            server4clientLogDisableIfTemp( client );
         #endif
         return 0 ;
      #endif /* S4OFF_WRITE */
   #endif /* S4OFF_TRAN */
}



#ifdef E4ANALYZE
   /* E4ANALYZE, S4SERVER */
   int server4clientVerify( SERVER4CLIENT *client, const int subs )
   {
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;

      if ( subs == 1 )
      {
         int rc ;

         if ( ( rc = tran4verify( &client->trans, 1 ) ) < 0 )
            return rc ;

         if ( ( rc = server4verify( client->server, 0 ) ) < 0 )
            return rc ;
      }

      return 0 ;
   }
#endif



#ifdef S4DEAD_CHECK
   /* S4DEAD_CHECK, S4SERVER */
   void server4verifyClients(SERVER4 *server)
   {
      long currentTime = time(0) ;

      if ( currentTime > server->timeCheck )
      {
         /* Check all of the clients */
         list4mutexWait( &server->clients ) ;

         for ( SERVER4CLIENT *clientNext = (SERVER4CLIENT *)l4first( &server->clients.list ) ; clientNext != 0 ; )
         {
            SERVER4CLIENT *clientOn = clientNext ;
            clientNext = (SERVER4CLIENT *)l4next( &server->clients.list, clientOn ) ;
            #ifdef S4JAVA
               if (!clientOn->javaClient)
            #endif
            if (clientOn->timeCount != server->timeStamp)
            {
               #ifndef S4OFF_LOG
                  if ( c4->logConn > 6 )
                  {
                     char buf[250] ;
                     sprintf( buf, "IS0080:Server is disconnecting client due to dead connection for userId: [%s] and ip adress: [%s] ",
                                   clientOn->account.accountId, clientOn->account.tcpAddress ) ;
                     assert5( strlen( buf ) < sizeof( buf ) ) ;
                     code4logInfo( c4, buf ) ;
                  }
               #endif
               server4disconnect( server, clientOn ) ;
            }
         }

         list4mutexRelease( &server->clients ) ;
         server->timeCheck = currentTime + DEAD4CHECK_SERVER_WAITTIME ;
         server->timeStamp++ ;
      }
   }
#endif /* S4DEAD_CHECK */



#ifndef S4OFF_COMMUNICATIONS
// AS Jun 11/07 - new function to copy a database
/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientDataCopy( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   CONNECTION4COPY_INFO_IN *dataIn = (CONNECTION4COPY_INFO_IN *)connection4data( &client->connection ) ;

   DATA4 *data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   short includeIndex = ntohs5( dataIn->includeIndex ) ;
   int rc = d4copyTable( data, dataIn->path, includeIndex ) ;

   connection4clear( connection ) ;

   return rc ;
}



// AS Jun 11/07 - new function to copy a database
/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientDataModify( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   CONNECTION4MODIFY_INFO_IN *dataIn = (CONNECTION4MODIFY_INFO_IN *)connection4data( &client->connection ) ;
   CODE4 *c4 = client->server->c4 ;

   DATA4 *data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   unsigned short numFields = ntohs5( dataIn->numFields ) ;
   unsigned short numTags = ntohs5( dataIn->numTags ) ;
   long dataLen = connection4len( connection ) ;
   if ( dataLen < (long)sizeof( CONNECTION4MODIFY_INFO_IN ) + (long)numFields * (long)sizeof( FIELD4INFO )  + (long)numTags * (long)sizeof( TAG4INFO ) )
      return error4( c4, e4packetLen, E91530 ) ;

   #ifndef S4OFF_SECURITY
      if ( file4getTemporary( &data->dataFile->file ) == 0 )  // don't disallow adding columns to temp files
         if ( account4userAllowCreate( &client->account, 0, TRUE ) == FALSE )
            return e4authorize ;
   #endif

   int rc = 0 ;
   connection4clear( connection ) ;

   CONNECTION4FIELD_INFO *fieldInfo = 0 ;
   CONNECTION4TAG_INFO *tagInfo = 0 ;
   if ( numFields != 0 )
   {
      fieldInfo =(CONNECTION4FIELD_INFO *)u4allocFree( c4, ( (long)numFields + 1L ) * sizeof( CONNECTION4FIELD_INFO ) ) ;
      if ( fieldInfo == 0 )
         rc = e4memory ;
   }

   if ( rc == 0 && numTags != 0 )
   {
      tagInfo = (CONNECTION4TAG_INFO *)u4allocFree( c4, ( (long)dataIn->numTags + 1L ) * sizeof( CONNECTION4TAG_INFO ) ) ;
      if ( tagInfo == 0 )
         rc = e4memory ;
   }

   if ( rc == 0 )
   {
      fieldInfo[numFields].name.ptr = 0 ;
      int offset = sizeof(CONNECTION4MODIFY_INFO_IN) ;
      int i ;
      for ( i = 0 ; i < numFields ; i++ )
      {
         memcpy( &fieldInfo[i], (char *)dataIn + offset, sizeof( CONNECTION4FIELD_INFO ) ) ;
         fieldInfo[i].name.offset = ntohs5(fieldInfo[i].name.offset) ;
         if ( (long)fieldInfo[i].name.offset > dataLen )  /* gives 10 character leeway */
            return error4( c4, e4packet, E91530 ) ;
         fieldInfo[i].type = ntohs5(fieldInfo[i].type) ;
         fieldInfo[i].len = ntohs5(fieldInfo[i].len) ;
         fieldInfo[i].dec = ntohs5(fieldInfo[i].dec) ;
         fieldInfo[i].nulls = ntohs5(fieldInfo[i].nulls) ;
         fieldInfo[i].name.ptr = (char *)dataIn + fieldInfo[i].name.offset ;
         offset += ( sizeof( CONNECTION4FIELD_INFO ) + strlen( fieldInfo[i].name.ptr ) + 1 ) ;
      }

      for ( i = 0 ; i < numTags ; i++ )
      {
         memcpy( &tagInfo[i], (char *)dataIn + offset, sizeof( CONNECTION4TAG_INFO ) ) ;
         tagInfo[i].name.ptr = (char *)dataIn + ntohs5(tagInfo[i].name.offset) ;
         tagInfo[i].expression.ptr = (char *)dataIn + ntohs5(tagInfo[i].expression.offset) ;
         offset += ( sizeof( CONNECTION4TAG_INFO ) + strlen( tagInfo[i].name.ptr ) + strlen( tagInfo[i].expression.ptr ) + 2 ) ;
         if ( tagInfo[i].filter.offset == 0 )
            tagInfo[i].filter.ptr = 0 ;
         else
         {
            tagInfo[i].filter.ptr = (char *)dataIn + ntohs5(tagInfo[i].filter.offset) ;
            offset += ( strlen( tagInfo[i].filter.ptr ) + 1 ) ;
         }
         tagInfo[i].unique = ntohs5(tagInfo[i].unique) ;
         tagInfo[i].descending = ntohs5(tagInfo[i].descending) ;
      }
      data = d4modifyStructure( data, (FIELD4INFO *)fieldInfo, (TAG4INFO *)tagInfo ) ;
      if ( data == 0 )
      {
         rc = error4code( c4 ) ;
         if ( rc == 0 )
            rc = e4create ;
         return rc ;
      }
      // otherwise we need to close the data4 as it will be opened by the caller...
      d4close( data ) ;
   }

   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCatalogSet( SERVER4CLIENT *client )
{
   #ifdef S4OFF_CATALOG
      return 0 ;
   #else
      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E91530 ) ;
      #endif

      CODE4 *c4 = client->server->c4 ;

      #ifdef E4ANALYZE
         int rc ;
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( connection4len( client->connection ) != sizeof( CONNECTION4CATALOG_SET_INFO_IN ) )
         return e4packetLen ;

      CONNECTION4CATALOG_SET_INFO_IN *info = (CONNECTION4CATALOG_SET_INFO_IN *)connection4data( client->connection ) ;

      if ( c4->catalog != 0 )
      {
         c4->catalog->catalogAdd = ntohs5(info->catalogAdd) ;
         c4->catalog->catalogStatus = ntohs5(info->catalogStatus) ;
      }

      connection4clear( client->connection ) ;

      return 0 ;
   #endif
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientDateFormatSet( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      int rc ;
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif
   if ( connection4len( &client->connection ) != sizeof( CONNECTION4DATE_FORMAT_SET_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4DATE_FORMAT_SET_INFO_IN *info = (CONNECTION4DATE_FORMAT_SET_INFO_IN *)connection4data( &client->connection ) ;
   memcpy( client->trans.dateFormat, info->dateFormat, sizeof( client->trans.dateFormat ) ) ;
   connection4clear( &client->connection ) ;
   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientConfigName( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;

   connection4clear( connection ) ;
   connection4addData( connection, client->server->configName, strlen( client->server->configName ) + 1, 0 ) ;

   return error4code( client->server->c4 ) ;
}



// AS May 17/04 - client functionality to copmress the data file...
int server4clientDataCompress( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   CONNECTION4DATA_COMPRESS_INFO_IN *dataIn = (CONNECTION4DATA_COMPRESS_INFO_IN *)connection4data( &client->connection ) ;

   DATA4 *data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   short blockSize = ntohs5( dataIn->blockSize ) ;
   int rc = D4compress( data, dataIn->name, blockSize, (char)dataIn->safety ) ;

   connection4clear( connection ) ;

   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientDataFileCodePage( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;

   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   connection4clear( connection ) ;

   return data->dataFile->codePage ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientDataFileName( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;

   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   connection4clear( connection ) ;
   connection4addData( connection, d4fileName( data ), strlen( d4fileName( data ) ) + 1, 0 ) ;

   return error4code( client->server->c4 ) ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS Feb 9/09 - added support to return key as well
int server4clientTagBottom( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_BOTTOM_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_BOTTOM_INFO_IN *info = (CONNECTION4TAG_BOTTOM_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   int rc = tfile4bottom( tag->tagFile ) ;
   if ( rc != 0 )
      return rc ;
   char *key = tfile4key( tag->tagFile ) ;

   connection4clear( &client->connection ) ;
   CONNECTION4TAG_BOTTOM_INFO_OUT *out ;
   rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_BOTTOM_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   out->recNo = htonl5(tfile4recNo( tag->tagFile )) ;

   long keyLen ;
   if ( key == 0 )
      keyLen = 0 ;
   else
      keyLen = tfile4keyLen( tag->tagFile ) ;

   out->keyLen = htonl5( keyLen ) ;
   if ( key != 0 )
   {
      rc = connection4addData( &client->connection, key, keyLen, NULL ) ;
      if ( rc != 0 )
         return rc ;
   }

   return error4code( client->server->c4 ) ;
}



// AS Feb 9/09 - added support for tag cache skipping
int server4clientTagCacheSkip( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_CACHE_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_CACHE_INFO_IN *info = (CONNECTION4TAG_CACHE_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;
   TAG4FILE *tagFile = tag->tagFile ;

   long startRecno = htonl5( info->startRecno ) ;
   if ( startRecno != 0 && tfile4recNo( tagFile ) != (unsigned long)startRecno )
   {
      int rc = d4go( data, startRecno ) ;
      if ( rc != 0 )
         return rc ;
      expr4context( tagFile->expr, data ) ;
      unsigned char *result ;
      short len = tfile4exprKey( tagFile, &result ) ;
      if ( len < 0 )
         return len ;
      t4versionCheck( tag, 0, 0 ) ;
      rc = tfile4go( tagFile, result, data->recNum, 0 ) ;
      if ( rc != 0  && rc != r4eof && rc != r4after )
         return rc ;
   }

   connection4clear( &client->connection ) ;
   CONNECTION4TAG_CACHE_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_CACHE_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;

   long keyLen = tfile4keyLen( tagFile ) ;
   long numToSkip = ntohl5( info->numRows ) ;
   long numSkipped ;
   long modus = ntohl5( info->modus ) ;
   short skipDir ;
   Bool5 doFirstSkip ;
   if ( numToSkip < 0 )
   {
      skipDir = -1 ;
      // in this case, skip backwards first and still return the rows in the forward order
      if ( startRecno != 0 && tfile4recNo( tagFile ) != (unsigned long)startRecno )
      {
         int rc = d4go( data, startRecno ) ;
         if ( rc != 0 )
            return rc ;
         expr4context( tagFile->expr, data ) ;
         unsigned char *result ;
         short len = tfile4exprKey( tagFile, &result ) ;
         if ( len < 0 )
            return len ;
         t4versionCheck( tag, 0, 0 ) ;
         rc = tfile4go( tagFile, result, data->recNum, 0 ) ;
         if ( rc != 0  && rc != r4eof && rc != r4after )
            return rc ;
      }

      numToSkip = tfile4skip( tagFile, numToSkip ) * -1 ;
      startRecno = tfile4recNo( tagFile ) ;
      Bool5 doFirstSkip = 0 ;
   }
   else
   {
      skipDir = 1 ;
      doFirstSkip = 1 ;
   }

   long keyMatch = 0 ;
   char keyBuf[I4MAX_KEY_SIZE] ;
   if ( modus < 0 )  // means only skip as long as the specified number of characters match
   {
      keyMatch = -modus ;
      memcpy( keyBuf, tfile4key( tagFile ), keyLen ) ;
   }

   // first do the recno's
   for ( numSkipped = 0 ; numSkipped < numToSkip ; numSkipped++ )
   {
      if ( doFirstSkip == 1 )  // skip first then...
      {
         rc = tfile4skip( tagFile, 1 ) ;
         if ( rc == -1 )  // means an error condition if returns opposite of input
            return error4code( client->server->c4 ) ;
         if ( rc != 1 )
            break ;
      }
      else
         doFirstSkip = 1 ;  // so we do the next skip

      long recno = htonl5( tfile4recNo( tagFile ) ) ;
      rc = connection4addData( &client->connection, &recno, sizeof( long ), NULL ) ;
      if ( rc != 0 )
         return rc ;
      char *outKey = tfile4key( tagFile ) ;
      if ( outKey == 0 )
         return -1 ;
      if ( keyMatch > 0 )
      {
         if ( memcmp( outKey, keyBuf, keyMatch ) != 0 )
         {
            numSkipped++ ;
            break ;
         }
      }
   }

   long maxToSkip = numSkipped ;

   // now do the keys
   if ( startRecno != 0 && tfile4recNo( tagFile ) != (unsigned long)startRecno )
   {
      int rc = d4go( data, startRecno ) ;
      if ( rc != 0 )
         return rc ;
      expr4context( tagFile->expr, data ) ;
      unsigned char *result ;
      short len = tfile4exprKey( tagFile, &result ) ;
      if ( len < 0 )
         return len ;
      t4versionCheck( tag, 0, 0 ) ;
      rc = tfile4go( tagFile, result, data->recNum, 0 ) ;
      if ( rc != 0  && rc != r4eof && rc != r4after )
         return rc ;
   }

   long numKeySkipped ;
   if ( skipDir == -1 )
      doFirstSkip = 0 ;

   for ( numKeySkipped = 0 ; numKeySkipped < maxToSkip ; numKeySkipped++ )
   {
      if ( doFirstSkip == 1 )  // skip first then...
      {
         rc = tfile4skip( tagFile, 1 ) ;
         if ( rc == -1 )  // means an error condition if returns opposite of input
            return error4code( client->server->c4 ) ;
         if ( rc != 1 )
            break ;
      }
      else
         doFirstSkip = 1 ;  // so we do the next skip

      char *outKey = tfile4key( tagFile ) ;
      if ( outKey == 0 )
         return -1 ;
      rc = connection4addData( &client->connection, outKey, keyLen, NULL ) ;
      if ( rc != 0 )
         return rc ;
   }

   if ( numSkipped != numKeySkipped )
      return error4( 0, e4index, E91530 ) ;

   out->numRows = htonl5( numSkipped * skipDir ) ;
   // out->recNo = htonl5( tfile4recNo( tagFile ) ) ;

   out->keyLen = htonl5( keyLen ) ;
   return error4code( client->server->c4 ) ;
}


/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS Feb 9/09 - added support to return key as well
int server4clientTagCount( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_COUNT_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_COUNT_INFO_IN *info = (CONNECTION4TAG_COUNT_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   long count = tfile4count( tag->tagFile ) ;
   if ( count < 0 )
      return count ;
   char *key = tfile4key( tag->tagFile ) ;

   connection4clear( &client->connection ) ;

   CONNECTION4TAG_COUNT_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_COUNT_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;

   out->count = htonl5( count ) ;
   out->recNo = htonl5( tfile4recNo( tag->tagFile ) ) ;

   long keyLen ;
   if ( key == 0 )
      keyLen = 0 ;
   else
      keyLen = tfile4keyLen( tag->tagFile ) ;

   out->keyLen = htonl5( keyLen ) ;
   if ( key != 0 )
   {
      rc = connection4addData( &client->connection, key, keyLen, NULL ) ;
      if ( rc != 0 )
         return rc ;
   }

   return error4code( client->server->c4 ) ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientTagExprKey( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_KEY_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_KEY_INFO_IN *info = (CONNECTION4TAG_KEY_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   long startRecno = htonl5( info->startRecno ) ;
   int rc = d4go( data, startRecno ) ;
   if ( rc != 0 )
      return rc ;
   expr4context( tag->tagFile->expr, data ) ;
   unsigned char *key ;
   short len = tfile4exprKey( tag->tagFile, &key ) ;
   if ( len < 0 )
      return len ;

   connection4clear( &client->connection ) ;

   CONNECTION4TAG_KEY_INFO_OUT *out ;
   rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_KEY_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   long keyLen = tfile4keyLen( tag->tagFile ) ;
   out->keyLen = htonl5( keyLen ) ;
   rc = connection4addData( &client->connection, key, keyLen, NULL ) ;
   if ( rc != 0 )
      return rc ;


   return error4code( client->server->c4 ) ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS Feb 9/09 - added support to return key as well
int server4clientTagGo( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_GO_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_GO_INFO_IN *info = (CONNECTION4TAG_GO_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   long recNo = ntohl5( info->recNum ) ;
   unsigned short keyLen = ntohs5( info->keyLen ) ;
   unsigned short goAdd = ntohs5( info->goAdd ) ;
   unsigned char *key = (unsigned char *)connection4data( connection ) + sizeof( CONNECTION4TAG_GO_INFO_IN ) ;
   int saveRc = tfile4go( tag->tagFile, key, recNo, goAdd ) ;
   connection4clear( &client->connection ) ;
   if ( saveRc != 0 )
      return (int) saveRc ;

   char *outKey = tfile4key( tag->tagFile ) ;
   if ( outKey == 0 )
      return -1 ;

   CONNECTION4TAG_GO_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_GO_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   out->rec = htonl5(tfile4recNo( tag->tagFile )) ;
   long outKeyLen = tfile4keyLen( tag->tagFile ) ;
   out->keyLen = htonl5( outKeyLen ) ;
   rc = connection4addData( &client->connection, outKey, outKeyLen, NULL ) ;
   if ( rc != 0 )
      return rc ;

   return error4code( client->server->c4 ) ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientTagEof( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_EOF_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_EOF_INFO_IN *info = (CONNECTION4TAG_EOF_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   int isEof = tfile4eof( tag->tagFile ) ;
   if ( isEof < 0 )  // AS Feb 10/09, isEof of 1 should still be sent back as normal, not as a return code...
      return isEof ;
   connection4clear( &client->connection ) ;
   CONNECTION4TAG_EOF_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_EOF_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   out->isEof = htonl5( isEof ) ;
   return error4code( client->server->c4 ) ;
}



// AS Feb 9/09 - replaced functionality with code to get key every time instead
/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
/*
int server4clientTagKey( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_KEY_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_KEY_INFO_IN *info = (CONNECTION4TAG_KEY_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   long startRecno = htonl5( info->startRecno ) ;
   if ( startRecno != 0 && tfile4recNo( tag->tagFile ) != (unsigned long)startRecno )
   {
      int rc = d4go( data, startRecno ) ;
      if ( rc != 0 )
         return rc ;
      expr4context( tag->tagFile->expr, data ) ;
      unsigned char *result ;
      short len = tfile4exprKey( tag->tagFile, &result ) ;
      if ( len < 0 )
         return len ;
      t4versionCheck( tag, 0, 0 ) ;
      rc = tfile4go( tag->tagFile, result, data->recNum, 0 ) ;
      if ( rc != 0  && rc != r4eof && rc != r4after )
         return rc ;
   }
   char *key = tfile4key( tag->tagFile ) ;
   if ( key == 0 )
      return -1 ;

   connection4clear( &client->connection ) ;

   CONNECTION4TAG_KEY_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_KEY_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   long keyLen = tfile4keyLen( tag->tagFile ) ;
   out->keyLen = htonl5( keyLen ) ;
   rc = connection4addData( &client->connection, key, keyLen, NULL ) ;
   if ( rc != 0 )
      return rc ;

   return error4code( client->server->c4 ) ;
}
*/


/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS Feb 9/09 - added support to return key as well
int server4clientTagSeek( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_SEEK_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_SEEK_INFO_IN *info = (CONNECTION4TAG_SEEK_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   long keyLen = ntohl5( info->keyLen ) ;
   char *key = (char *)connection4data( connection ) + sizeof( CONNECTION4TAG_SEEK_INFO_IN ) ;
   int saveRc = tfile4seek( tag->tagFile, key, keyLen ) ;
   if ( saveRc < 0 )
      return (int)saveRc ;

   if ( saveRc == 0 || saveRc == 2 )
   {
      keyLen = tfile4keyLen( tag->tagFile ) ;
      key = tfile4key( tag->tagFile ) ;
      if ( key == 0 )  // probably shouldn't happen, but don't disallow it
      {
         keyLen = 0 ;
         key = 0 ;
      }
   }
   else
   {
      keyLen = 0 ;
      key = 0 ;
   }

   connection4clear( &client->connection ) ;
   CONNECTION4TAG_SEEK_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_SEEK_INFO_OUT ), (void **)&out ) ;
   if ( rc < 0 )
      return rc ;
   out->rec = htonl5(tfile4recNo( tag->tagFile )) ;
   out->keyLen = htonl5( keyLen ) ;
   if ( key != 0 )
   {
      rc = connection4addData( &client->connection, key, keyLen, NULL ) ;
      if ( rc != 0 )
         return rc ;
   }

   return saveRc ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS Feb 9/09 - added support to return key as well
int server4clientTagSkip( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_SKIP_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_SKIP_INFO_IN *info = (CONNECTION4TAG_SKIP_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   long startRecno = htonl5( info->startRecno ) ;
   if ( startRecno != 0 && tfile4recNo( tag->tagFile ) != (unsigned long)startRecno )
   {
      int rc = d4go( data, startRecno ) ;
      if ( rc != 0 )
         return rc ;
      expr4context( tag->tagFile->expr, data ) ;
      unsigned char *result ;
      short len = tfile4exprKey( tag->tagFile, &result ) ;
      if ( len < 0 )
         return len ;
      t4versionCheck( tag, 0, 0 ) ;
      rc = tfile4go( tag->tagFile, result, data->recNum, 0 ) ;
      if ( rc != 0  && rc != r4eof && rc != r4after )
         return rc ;
   }

   long numToSkip = ntohl5( info->numSkip ) ;
   long numSkip ;
   if ( info->tfile4dskip == 1 )
      numSkip = tfile4dskip( tag->tagFile, numToSkip ) ;
   else
      numSkip = tfile4skip( tag->tagFile, numToSkip ) ;
   if ( numSkip == -numToSkip )  // means an error condition
      return error4code( client->server->c4 ) ;

   char *outKey = tfile4key( tag->tagFile ) ;
   if ( outKey == 0 )
      return -1 ;

   connection4clear( &client->connection ) ;
   CONNECTION4TAG_SKIP_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_SKIP_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   out->numSkip = htonl5( numSkip ) ;
   out->recNo = htonl5( tfile4recNo( tag->tagFile ) ) ;
   long outKeyLen = tfile4keyLen( tag->tagFile ) ;
   out->keyLen = htonl5( outKeyLen ) ;
   rc = connection4addData( &client->connection, outKey, outKeyLen, NULL ) ;
   if ( rc != 0 )
      return rc ;


   return error4code( client->server->c4 ) ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS Feb 9/09 - added support to return key as well
int server4clientTagPosition( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_POSITION_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_POSITION_INFO_IN *info = (CONNECTION4TAG_POSITION_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   long startRecno = htonl5( info->startRecno ) ;
   if ( startRecno != 0 && tfile4recNo( tag->tagFile ) != (unsigned long)startRecno )
   {
      int rc = d4go( data, startRecno ) ;
      if ( rc != 0 )
         return rc ;
      expr4context( tag->tagFile->expr, data ) ;
      unsigned char *result ;
      short len = tfile4exprKey( tag->tagFile, &result ) ;
      if ( len < 0 )
         return len ;
      t4versionCheck( tag, 0, 0 ) ;
      rc = tfile4go( tag->tagFile, result, data->recNum, 0 ) ;
      if ( rc != 0  && rc != r4eof && rc != r4after )
         return rc ;
   }

   // AS Jul 14/09 - need to use t4position since the tag may not actually be positioned properly (happens if we skip forward using the leaf blocks; the parent blocks may not match up)
   double val = t4position( tag ) ;
   if ( val < 0 )
      return (int) val ;
   char *key = tfile4key( tag->tagFile ) ;

   connection4clear( &client->connection ) ;
   CONNECTION4TAG_POSITION_INFO_OUT *out ;
   int rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_POSITION_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   out->position = htond(val) ;
   out->rec = htonl5(tfile4recNo( tag->tagFile )) ;

   long keyLen ;
   if ( key == 0 )
      keyLen = 0 ;
   else
      keyLen = tfile4keyLen( tag->tagFile ) ;

   out->keyLen = htonl5( keyLen ) ;
   if ( key != 0 )
   {
      rc = connection4addData( &client->connection, key, keyLen, NULL ) ;
      if ( rc != 0 )
         return rc ;
   }

   return error4code( client->server->c4 ) ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientTagPositionSet( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_POSITION_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_POSITION_SET_INFO_IN *info = (CONNECTION4TAG_POSITION_SET_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   double pos = ntohd( info->pos ) ;
   int rc = tfile4positionSet( tag->tagFile, pos ) ;
   if ( rc < 0 )
      return rc ;
   connection4clear( &client->connection ) ;
   CONNECTION4TAG_POSITION_SET_INFO_OUT *out ;
   rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_POSITION_SET_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   out->rec = htonl5(tfile4recNo( tag->tagFile )) ;

   return error4code( client->server->c4 ) ;
}



/* AS Oct 25/05 - support for tag functionality in client/server */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS Feb 9/09 - added support to return key as well
int server4clientTagTop( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;
   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( connection ) < sizeof( CONNECTION4TAG_TOP_INFO_IN ) )
      return e4packetLen ;

   CONNECTION4TAG_TOP_INFO_IN *info = (CONNECTION4TAG_TOP_INFO_IN *)connection4data( connection ) ;
   INDEX4 *i4 = d4index( data, info->indexName ) ;
   if ( i4 == 0 )
      return e4name ;
   TAG4 *tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;

   int rc = tfile4top( tag->tagFile ) ;
   if ( rc != 0 )
      return rc ;

   char *key = tfile4key( tag->tagFile ) ;

   connection4clear( &client->connection ) ;
   long rec = htonl5(tfile4recNo( tag->tagFile )) ;
   CONNECTION4TAG_TOP_INFO_OUT *out ;
   rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TAG_TOP_INFO_OUT ), (void **)&out ) ;
   if ( rc != 0 )
      return rc ;
   out->recNo = htonl5(tfile4recNo( tag->tagFile )) ;

   long keyLen ;
   if ( key == 0 )
      keyLen = 0 ;
   else
      keyLen = tfile4keyLen( tag->tagFile ) ;

   out->keyLen = htonl5( keyLen ) ;
   if ( key != 0 )
   {
      rc = connection4addData( &client->connection, key, keyLen, NULL ) ;
      if ( rc != 0 )
         return rc ;
   }

   return error4code( client->server->c4 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientTagFileName( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;

   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   TAG4 *t4 = d4tag( data, connection4data( connection ) ) ;
   if ( t4 == 0 )
      return e4name ;

   connection4clear( connection ) ;

   char blankName = '\0' ;
   const char *iName = t4fileName( t4 ) ;
   if ( iName == 0 )
      iName = &blankName ;
   connection4addData( connection, iName, strlen( iName ) + 1, 0 ) ;

   return error4code( client->server->c4 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientIndexFileName( SERVER4CLIENT *client )
{
   DATA4 *data ;
   INDEX4 *i4 ;
   CONNECTION4 *connection ;
   const char *iName ;
   char blankName = '\0' ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   connection = &client->connection ;

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   i4 = d4index( data, connection4data( connection ) ) ;
   if ( i4 == 0 )
      return e4name ;

   connection4clear( connection ) ;
   iName = i4fileName( i4 ) ;
   if ( iName == 0 )
      iName = &blankName ;
   connection4addData( connection, iName, strlen( iName ) + 1, 0 ) ;

   return error4code( client->server->c4 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientServerOS( SERVER4CLIENT *client )
{
   #ifdef E4ANALYZE
      int rc ;
   #endif

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif
   if ( connection4len( &client->connection ) != 0 )
      return e4packetLen ;

   connection4clear( &client->connection ) ;

   long serverOS = OS4UNKNOWN ;
   #ifdef S4UNIX
      serverOS = OS4UNIX ;
   #endif
   #ifdef S4WIN32
      serverOS = OS4WIN32 ;
   #endif
   connection4addData( &client->connection, &serverOS, sizeof( long ), 0 ) ;

   return error4code( client->server->c4 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientIndexFormat( SERVER4CLIENT *client )
{
   #ifdef E4ANALYZE
      int rc ;
   #endif

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif
   if ( connection4len( &client->connection ) != 0 )
      return e4packetLen ;

   connection4clear( &client->connection ) ;
   return code4indexFormat( client->server->c4 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientIndexInfo( SERVER4CLIENT *client )
{
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   CONNECTION4 *connection = &client->connection ;

   DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   INDEX4 *i4 = d4index( data, connection4data( connection ) ) ;
   if ( i4 == 0 )
      return e4name ;

   connection4clear( connection ) ;
   TAG4INFO *tagInfo = i4tagInfo( i4 ) ;
   if ( tagInfo == 0 )
      return e4memory ;

   short i;
   for ( i = 0 ;; i++ )
   {
      if ( tagInfo[i].name == 0 )
         break ;
   }

   CONNECTION4INDEX_INFO_OUT *out ;
   connection4addData( &client->connection, 0, sizeof( CONNECTION4INDEX_INFO_OUT ), (void **)(&out) ) ;
   out->numTags = htons5(i) ;
   unsigned int len = 0 ;
   short offset = sizeof( CONNECTION4INDEX_INFO_OUT ) ;
   for ( short j = 0 ; j != i ; j++ )
   {
      len = strlen( tagInfo[j].name ) + 1 ;
      offset += sizeof( CONNECTION4TAG_INFO_FOR_I4INFO ) ;
      CONNECTION4TAG_INFO_FOR_I4INFO *tinfo ;
      connection4addData( connection, 0, sizeof(CONNECTION4TAG_INFO_FOR_I4INFO), (void **)(&tinfo) ) ;
      tinfo->name.offset = htons5(offset) ;
      unsigned int len2 = strlen( tagInfo[j].expression ) + 1 ;
      offset += len ;
      tinfo->expression.offset = htons5(offset) ;
      TAG4 *tag = d4tag( data, tagInfo[j].name ) ;
      assert5( tag != 0 ) ;
      tinfo->keyLen = htons5( tfile4keyLen( tag->tagFile ) ) ;
      offset += len2 ;
      unsigned int len3 ;
      if ( tagInfo[j].filter == 0 )
      {
         len3 = 0 ;
         tinfo->filter.offset = 0 ;
      }
      else
      {
         len3 = strlen( tagInfo[j].filter ) + 1 ;
         if ( len3 == 1 )
         {
            len3 = 0 ;
            tinfo->filter.offset = 0 ;
         }
         else
            tinfo->filter.offset = htons5(offset) ;
      }
      offset += len3 ;
      tinfo->unique = htons5(tagInfo[j].unique) ;
      tinfo->descending = htons5(tagInfo[j].descending) ;
      connection4addData( connection, tagInfo[j].name, len, 0 ) ;
      connection4addData( connection, tagInfo[j].expression, len2, 0 ) ;
      if ( len3 != 0 )
         connection4addData( connection, tagInfo[j].filter, len3, 0 ) ;
   }

   u4free( tagInfo ) ;
   return error4code( client->server->c4 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientUniqueSet( SERVER4CLIENT *client )
{
   CONNECTION4UNIQUE_INFO_IN *info ;
   CONNECTION4UNIQUE_TAG_INFO *tagInfo ;
   DATA4 *data ;
   TAG4 *tag ;
   unsigned int i ;
   char *pos ;
   #ifdef E4ANALYZE
      int rc ;
   #endif

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( &client->connection ) < sizeof( CONNECTION4UNIQUE_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4UNIQUE_INFO_IN *)connection4data( &client->connection ) ;
   info->numTags = ntohs5(info->numTags) ;
   if ( connection4len( &client->connection ) != (long)(sizeof( CONNECTION4UNIQUE_INFO_IN ) + (long)info->numTags * sizeof( CONNECTION4UNIQUE_TAG_INFO ) ) )
      return e4packetLen ;
   for ( i = 0, pos = ((char *)connection4data( &client->connection )) + sizeof( CONNECTION4UNIQUE_INFO_IN ) ; i < info->numTags ; i++ )
   {
      tagInfo = ( CONNECTION4UNIQUE_TAG_INFO *)pos ;
      tag = d4tag( data, tagInfo->alias ) ;
      if ( tag == 0 )
         return e4name ;
      tagInfo->unique = ntohs5(tagInfo->unique) ;
      if ( t4unique( tag ) != tagInfo->unique )  /* zero disallowed if different only */
         switch( tagInfo->unique )
         {
            case r4unique:
            case r4uniqueContinue:
            case e4unique:
            #ifdef S4FOX
               case r4candidate:
               case e4candidate:
            #endif
               tag->errUnique = tagInfo->unique ;
               break ;
            default:
               return error4( client->server->c4, e4unique, E85301 ) ;
         }

      pos += sizeof( CONNECTION4UNIQUE_TAG_INFO ) ;
   }

   connection4clear( &client->connection ) ;
   return 0 ;
}



#ifdef S4TESTING
   /* S4TESTING, not S4OFF_COMMUNICATIONS, S4SERVER */
   void server4clientTestSendReceive( SERVER4CLIENT *client )
   {
      CODE4 *c4 = client->server->c4 ;
      CONNECT4 *connect = &client->connect ;

      long numBytes ;

      numBytes = connect4receiveLong( connect ) ;

      if ( numBytes > client->getBufLen )
      {
         if ( client->getBuf )
            u4free( client->getBuf ) ;
         client->getBuf = (char *)u4alloc( numBytes ) ;
         if ( client->getBuf == 0 )  // allocation failure, just cancel client
         {
            connect4sendShort( connect, -1 ) ;
            connect4sendFlush( connect ) ;
            error4( connect->cb, e4connect, E91530 ) ;
            return ;
         }
         client->getBufLen = numBytes ;
      }

      connect4receive( connect, client->getBuf, numBytes, code4timeoutVal(c4) ) ;

      connect4sendShort( connect, 1 ) ;  // send a byte to tell client we are done
      connect4sendFlush( connect ) ;
   }
#endif /* S4TESTING */



#ifndef S4OFF_MEMO
   /* not S4OFF_MEMO, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientMemoCompress( SERVER4CLIENT *client )
   {
      DATA4 *data ;
      int rc ;

      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E91530 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( client->server->doCompress == 0 )
         return e4notSupported ;

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
      if ( data == 0 )
         return e4name ;

      #ifdef E4ANALYZE
         if ( data->readOnly == 1 )
            return error4( data->codeBase, e4info, E80606 ) ;
      #endif

      rc = D4memoCompress( data ) ;
      connection4clear( &client->connection ) ;
      // AS 07/09/99 --> if rc > 0 (eg. r4entry), just ignore.  this may occur when datafile at eof on start of compress... just ignore
      // AS 08/20/99 -> but due return r4locked...
      if ( rc > 0 && rc != r4locked )
         rc = 0 ;
      return rc ;
   }



   /* not S4OFF_MEMO, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientMemoRead( SERVER4CLIENT *client )
   {
      DATA4 *data ;
      int rc ;
      CONNECTION4MEMO_INFO_IN *info ;
      CONNECTION4MEMO_INFO_OUT *out ;
      char *ptr ;
      FIELD4 *field ;
      long len ;

      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E91530 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
      if ( data == 0 )
         return e4name ;

      if ( connection4len( &client->connection ) != sizeof( CONNECTION4MEMO_INFO_IN ) )
         return e4packetLen ;

      info = (CONNECTION4MEMO_INFO_IN *)connection4data( &client->connection ) ;

      rc = D4goRead( data, ntohl5( info->recNo ) ) ;  /* don't setOldRecord since server only does reading at this point */
      if ( rc != 0 )
         return rc ;

      field = d4fieldJ( data, ntohs5(info->fieldNo )) ;
      if ( field == 0 )
         return error4( client->server->c4, e4fieldName, E91530 ) ;
      connection4clear( &client->connection ) ;
      connection4addData( &client->connection, NULL, sizeof( CONNECTION4MEMO_INFO_OUT ), (void **)&out ) ;
      len = f4memoLen( field ) ;
      if ( len == 0 )
      {
         if ( error4code( client->server->c4 ) != 0 )
            return error4code( client->server->c4 ) ;
      }
      else
      {
         out->memoLen = htonl5(len) ;
         ptr = f4memoPtr( field ) ;
         if ( ptr == 0 )
            return error4code( client->server->c4 ) ;
         connection4addData( &client->connection, ptr, len, NULL ) ;
      }

      return error4code( client->server->c4 ) ;
   }
#endif /* S4OFF_MEMO */



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientZap( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   CONNECTION4 *connection ;
   CONNECTION4ZAP_INFO_IN *info ;
   short int dataFileLocked ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   if ( client->server->doZap == 0 )
      return e4notSupported ;

   connection = &client->connection ;

   if ( connection4len( connection ) != sizeof( CONNECTION4ZAP_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4ZAP_INFO_IN *)connection4data( connection ) ;

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   #ifdef E4ANALYZE
      if ( data->readOnly == 1 )
         return error4( data->codeBase, e4info, E80606 ) ;
   #endif

   dataFileLocked = (short int)dfile4lockTestFile( data->dataFile, connection4clientId( connection ), connection4serverId( connection ), lock4write ) == 1 ;
   info->recStart = ntohl5(info->recStart) ;
   info->recStop = ntohl5(info->recStop) ;
   if ( info->recStart < 1 || info->recStop < 1 )
      return e4parm ;
   rc = D4zap( data, info->recStart, info->recStop) ;
   if ( rc < 0 )
   {
      if ( dataFileLocked == 0 )
         if ( dfile4lockTestFile( data->dataFile, connection4clientId( connection ), connection4serverId( connection ), lock4write ) == 1 )
            dfile4unlockFile( data->dataFile, connection4clientId( connection ), connection4serverId( connection ) ) ;
      return rc ;
   }
   dataFileLocked = (short int)dfile4lockTestFile( data->dataFile, connection4clientId( connection ), connection4serverId( connection ), lock4write ) == 1 ;
   connection4clear( connection ) ;
   connection4addData( connection, &dataFileLocked, sizeof( short int ), NULL ) ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientReccount( SERVER4CLIENT *client )
{
   DATA4 *data ;
   long count ;
   int rc ;
   CONNECTION4RECCOUNT_INFO_OUT *out ;
   CONNECTION4 *connection ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   connection = &client->connection ;

   #ifdef E4ANALYZE
      if ( connection == 0 )
         return error4( client->server->c4, e4parm, E91530 ) ;
   #endif

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   count = D4recCount( data ) ;
   if ( count < 0 )
      return (int)count ;
   connection4clear( connection ) ;
   rc = connection4addData( connection, NULL, sizeof( CONNECTION4RECCOUNT_INFO_OUT ), (void **)&out ) ;
   if (rc >= 0 )
   {
      out->recCount = htonl5(count) ;
      out->appendLocked = ( d4lockTestAppend( data ) == 1 ) ;
   }
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
static int server4clientCloseData( SERVER4CLIENT *client, DATA4 *data )
{
   #ifdef E4PARM_LOW
      if ( client == 0 || data == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   data->clientId = connection4clientId( &client->connection ) ;

   return D4close( data ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRemove( SERVER4CLIENT *client )
{
   int rc ;
   DATA4 *data ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   rc = D4remove( data ) ;
   if ( rc == r4open )
   {
      connection4clear( &client->connection ) ;
      rc = server4clientCloseData( client, data ) ;
   }
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientClose( SERVER4CLIENT *client )
{
   DATA4 *data ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   connection4clear( &client->connection ) ;
   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;

   if ( data == 0 )
      return 0 ;
   return server4clientCloseData( client, data ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCloseIndex( SERVER4CLIENT *client )
{
   /* note that close index is not recorded in the transaction file */

   DATA4 *data ;
   INDEX4 *i4 ;
   CODE4 *c4 = client->server->c4 ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   // AS May 23/03 - Also requires the doRemove flag if non-auto-open and we are removing
   CONNECTION4CLOSE_INDEX_INFO_IN *info = (CONNECTION4CLOSE_INDEX_INFO_IN *)connection4data( &client->connection ) ;
   i4 = d4index( data, ((char *)info) + sizeof( CONNECTION4CLOSE_INDEX_INFO_IN ) ) ;
   if ( i4 == 0 )
      return e4name ;

   int oldDoRemove = c4getDoRemove( c4 ) ;
   int tempDoRemove = 0 ;
   // AS Oct 29/04 - for Clipper this only gets called for non-production indexes, so always set the temp remove flag as appropriate
   #ifdef S4CLIPPER
      if ( info->doRemove )
         tempDoRemove = 1 ;
   #else
      // only do for non-production indexes
      if ( info->doRemove && index4isProduction( i4->indexFile ) == 0 )
         tempDoRemove = 1 ;
   #endif
   c4setDoRemove( c4, tempDoRemove ) ;
   int rc = i4closeLow( i4 ) ;
   c4setDoRemove( c4, oldDoRemove ) ;

   connection4clear( &client->connection ) ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientBottom( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   CONNECTION4 *connection ;
   CONNECTION4BOTTOM_INFO_IN *info ;
   CONNECTION4BOTTOM_INFO_OUT *out ;
   TAG4 *tag ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   connection = &client->connection ;

   #ifdef E4ANALYZE
      if ( connection == 0 )
         return error4( client->server->c4, e4parm, E91530 ) ;
   #endif
   if ( connection4len( connection ) != sizeof( CONNECTION4BOTTOM_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4BOTTOM_INFO_IN *)connection4data( connection ) ;

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( info->usesTag )
   {
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;
      d4tagSelect( data, tag ) ;
   }
   else
      d4tagSelect( data, 0 ) ;

   // AS May 21/02 - optionally also send memos...
   assert5port( "Added IncludeMemos member to this communications message" );
   short includeMemos = info->includeMemos ;

   rc = D4bottom( data ) ;
   if ( rc < 0 || rc == r4eof || rc == r4locked )
      return rc ;

   connection4clear( connection ) ;
   connection4addData( connection, NULL, sizeof( CONNECTION4BOTTOM_INFO_OUT ), (void **)&out ) ;
   out->recNo = htonl5(d4recNo( data )) ;
   out->bofFlag = data->bofFlag ;
   out->eofFlag = data->eofFlag ;
   out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
   rc = connection4addData( connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;
   // AS May 21/02 - optionally also send memos...
   if ( includeMemos )
      if ( server4clientSendMemos( client, data ) < 0 )
         return -1 ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCalcCreate( SERVER4CLIENT *client )
{
   CONNECTION4 *connection = 0 ;
   CONNECTION4CALC_CREATE_INFO_IN *info = 0 ;
   CODE4 *c4 = 0 ;
   DATA4 *data = 0 ;
   EXPR4 *calcExpr = 0 ;
   #ifdef E4ANALYZE
      int rc = 0 ;
   #endif

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   connection = &client->connection ;
   c4 = client->server->c4 ;

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   #ifdef E4ANALYZE
      if ( connection == 0 )
         return error4( c4, e4parm, E91530 ) ;
   #endif
   if ( connection4len( connection ) < sizeof( CONNECTION4CALC_CREATE_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4CALC_CREATE_INFO_IN *)connection4data( connection ) ;
   short len ;

   char *dataPtr = connection4data( connection ) ;
   dataPtr += sizeof( CONNECTION4CALC_CREATE_INFO_IN ) ;
   len = htons5( *((short *)dataPtr) ) ;
   dataPtr += sizeof( short ) ;
   char *exprSourcePtr = (char *)dataPtr ;
   dataPtr += len ;

   /* AS 09/14/98 modifications in case of data aliases... */
   short numData4 ;

   numData4 = htons5( *((short *)dataPtr) ) ;
   dataPtr += sizeof( short ) ;

   int iData, isError = 0 ;
   DATA4 **data4ptrArray = 0 ;

   if ( numData4 != 0 )
   {
      /* we are not trying for efficiency here, just basic code working */
      data4ptrArray = (DATA4 **)u4allocFree( c4, sizeof(DATA4 *) * numData4 ) ;
      if ( data4ptrArray == 0 )
         return e4memory ;

      #ifdef E4ANALYZE
         /* u4allocFree memsets the array to 0's, this is required and assumed */
         for ( iData = 0 ; iData < numData4 ; iData++ )
            assert5( data4ptrArray[iData] == 0 ) ;
      #endif

      for ( iData = 0 ; iData < numData4 ; iData++ )
      {
         short lenData = htons5( *((short *)dataPtr) ) ;
         dataPtr += sizeof( short ) ;

         if ( lenData != 0 )
         {
            char *dataName = (char *)dataPtr ;
            dataPtr += lenData ;

            DATA4 *data = d4open( c4, dataName ) ;
            if ( data == 0 )
            {
               isError = 1 ;
               break ;
            }

            short lenAlias = htons5( *((short *)dataPtr) ) ;
            dataPtr += sizeof( short ) ;

            char *aliasName = (char *)dataPtr ;
            dataPtr += lenAlias ;

            d4aliasSet( data, aliasName ) ;

            data4ptrArray[iData] = data ;
         }
      }
   }

   if ( isError )
      calcExpr = 0 ;
   else
      calcExpr = expr4parse( data, exprSourcePtr ) ;

   for ( iData = 0 ; iData < numData4 ; iData++ )
      if ( data4ptrArray[iData] != 0 )
         d4close( data4ptrArray[iData] ) ;

   if ( data4ptrArray != 0 )
      u4free( data4ptrArray ) ;

   if ( calcExpr == 0 )
      return -1 ;
   if ( code4calcCreate( c4, calcExpr, info->calcName ) == 0 )
      return -1 ;
   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCalcReset( SERVER4CLIENT *client )
{
   code4calcReset( client->server->c4 ) ;
   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientTop( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   CONNECTION4 *connection ;
   CONNECTION4TOP_INFO_IN *info ;
   CONNECTION4TOP_INFO_OUT *out ;
   TAG4 *tag ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   connection = &client->connection ;
   #ifdef E4ANALYZE
      if ( connection == 0 )
         return error4( client->server->c4, e4parm, E91530 ) ;
   #endif
   if ( connection4len( connection ) != sizeof( CONNECTION4TOP_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4TOP_INFO_IN *)connection4data( connection ) ;

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( info->usesTag )
   {
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;
      d4tagSelect( data, tag ) ;
   }
   else
      d4tagSelect( data, 0 ) ;

   // AS May 21/02 - optionally also send memos...
   assert5port( "Added IncludeMemos member to this communications message" );
   short includeMemos = info->includeMemos ;
   rc = D4top( data ) ;
   if ( rc < 0 || rc == r4eof || rc == r4locked || rc == r4entry)
      return rc ;

   connection4clear( connection ) ;
   connection4addData( connection, NULL, sizeof( CONNECTION4TOP_INFO_OUT ), (void **)&out ) ;
   out->recNo = htonl5(d4recNo( data )) ;
   out->bofFlag = data->bofFlag ;
   out->eofFlag = data->eofFlag ;
   out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
   rc = connection4addData( &client->connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;
   // AS May 21/02 - optionally also send memos...
   if ( includeMemos )
      if ( server4clientSendMemos( client, data ) < 0 )
         return -1 ;
   return rc ;
}



// AS 01/31/01 - not available for now
// /* not S4OFF_COMMUNICATIONS, S4SERVER */
// int server4clientFieldsAdd( SERVER4CLIENT *client )
// {
//    #ifdef S4OFF_WRITE
//       return e4notSupported ;
//    #else
//       CONNECTION4 *connection = &client->connection ;
//       CONNECTION4FIELDS_ADD_INFO_IN *dataIn = (CONNECTION4FIELDS_ADD_INFO_IN *)connection4data( connection ) ;
//       CODE4 *c4 = client->server->c4 ;
//
//       DATA4 *data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
//       if ( data == 0 )
//          return e4name ;
//
//       unsigned short numFields = ntohs5(dataIn->numFields) ;
//       if ( numFields == 0 )
//          return e4create ;
//
//       long dataLen = connection4len( connection ) ;
//       if ( dataLen < (long)sizeof( CONNECTION4FIELDS_ADD_INFO_IN ) + (long)numFields * (long)sizeof( FIELD4INFO ) )
//          return error4( c4, e4packetLen, E91530 ) ;
//
//       #ifndef S4OFF_SECURITY
//          if ( file4getTemporary( &data->dataFile->file ) == 0 )  // don't disallow adding columns to temp files
//             if ( account4userAllowCreate( &client->account, 0, TRUE ) == FALSE )
//                return e4authorize ;
//       #endif
//
//       int rc = 0 ;
//       connection4clear( connection ) ;
//
//       CONNECTION4FIELD_INFO *fieldInfo = (CONNECTION4FIELD_INFO *)u4allocFree( c4, ( (long)numFields + 1L ) * sizeof( CONNECTION4FIELD_INFO ) ) ;
//       if ( fieldInfo == 0 )
//       {
//          rc = e4memory ;
//       }
//       else
//       {
//          fieldInfo[numFields].name.ptr = 0 ;
//          int offset, i ;
//          for ( offset = sizeof(CONNECTION4FIELDS_ADD_INFO_IN), i = 0 ; i < numFields ; i++ )
//          {
//             memcpy( &fieldInfo[i], (char *)dataIn + offset, sizeof( CONNECTION4FIELD_INFO ) ) ;
//             fieldInfo[i].name.offset = ntohs5(fieldInfo[i].name.offset) ;
//             if ( (long)fieldInfo[i].name.offset > dataLen )  /* gives 10 character leeway */
//                return error4( c4, e4packet, E91530 ) ;
//             fieldInfo[i].type = ntohs5(fieldInfo[i].type) ;
//             fieldInfo[i].len = ntohs5(fieldInfo[i].len) ;
//             fieldInfo[i].dec = ntohs5(fieldInfo[i].dec) ;
//             fieldInfo[i].nulls = ntohs5(fieldInfo[i].nulls) ;
//             fieldInfo[i].name.ptr = (char *)dataIn + fieldInfo[i].name.offset ;
//             offset += ( sizeof( CONNECTION4FIELD_INFO ) + strlen( fieldInfo[i].name.ptr ) + 1 ) ;
//          }
//          data = d4fieldsAdd( data, numFields, (FIELD4INFO *)fieldInfo ) ;
//          if ( data == 0 )
//             return e4create ;
//       }
//
//       return rc ;
//    #endif
// }
//



/* not S4OFF_COMMUNICATIONS, S4SERVER */
static int server4clientSendMemos( SERVER4CLIENT *client, DATA4 *data )
{
   int numMemos = data->dataFile->nFieldsMemo ;
   for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
   {
      FIELD4 *field = data->fieldsMemo[memoLoop].field ;
      long memoLen = f4memoLen( field ) ;
      void *data ;

      // reserve the space first.
      connection4addData( &client->connection, 0, sizeof( long ) + memoLen, &data ) ;
      *((long *)data) = htonl5( memoLen ) ;
      if ( memoLen != 0 )
         memcpy( (char *)data + 4, f4memoPtr( field ), memoLen ) ;
   }

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientGo( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   CONNECTION4GO_INFO_IN *info ;
   CONNECTION4GO_INFO_OUT *out ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   if ( connection4len( &client->connection ) != sizeof( CONNECTION4GO_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4GO_INFO_IN *)connection4data( &client->connection ) ;

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   // AS May 21/02 - optionally also send memos...
   assert5port( "Added IncludeMemos member to this communications message" );
   short includeMemos = info->includeMemos ;
   // AS Aug 26/05 - ensure the recNo not negative (was causing an error on server)
   long recNo = ntohl5(info->recNo) ;
   if ( recNo < 0 )
      return e4entry ;
   rc = D4goRead( data, recNo ) ;  /* don't setOldRecord since server only does reading at this point */
   connection4clear( &client->connection ) ;
   if ( rc != 0 )    /* a return code of r4entry is also possible... */
      return rc ;

   connection4addData( &client->connection, NULL, sizeof( CONNECTION4GO_INFO_OUT ), (void **)&out ) ;
   out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
   out->recNo = htonl5( d4recNo( data ) ) ;
   rc = connection4addData( &client->connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;

   // AS May 21/02 - optionally also send memos...
   if ( includeMemos )
      if ( server4clientSendMemos( client, data ) < 0 )
         return -1 ;

   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientLockGroup( SERVER4CLIENT *client )
{
   CONNECTION4LOCK_GROUP_INFO_IN *info ;
   LOCK4ID *locks ;
   DATA4 *data ;
   CODE4 *c4 ;
   int i, rc ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   c4 = client->server->c4 ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return error4stack( c4, rc, E91530 ) ;
   #endif

   if ( connection4len( &client->connection ) < sizeof( CONNECTION4LOCK_GROUP_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4LOCK_GROUP_INFO_IN *)connection4data( &client->connection ) ;
   if ( info == 0 )
      return error4( c4, e4corrupt, E91530 ) ;
   info->numLocks = ntohs5(info->numLocks) ;

   if ( (long)connection4len( &client->connection ) != (long)sizeof( CONNECTION4LOCK_GROUP_INFO_IN ) + (long)info->numLocks * (long)sizeof( LOCK4ID ) )
      return e4packetLen ;

   code4lockClear( c4 ) ;

   locks = (LOCK4ID *)(connection4data( &client->connection ) + sizeof (CONNECTION4LOCK_GROUP_INFO_IN ) ) ;
   if ( ( code4unlockAuto( c4 ) == LOCK4DATA ) )  /* must perform and register data unlock */
   {
      for ( i = 0 ; i < info->numLocks ; i++ )
      {
         data = tran4data( &client->trans, ntohl5(locks[i].serverId), ntohl5(locks[i].lockId) ) ;
         if ( data == 0 )
            return e4name ;
         // AS Apr 15/03 - support for new lockId for shared clone locking
         d4unlockLow( data, data4lockId( data ), 0 ) ;
      }
   }

   rc = 0 ;
   for ( i = 0 ; i < info->numLocks ; i++ )
   {
      data = tran4data( &client->trans, ntohl5(locks[i].serverId), ntohl5(locks[i].lockId) ) ;
      if ( data == 0 )
         return e4name ;

      switch( ntohs5(locks[i].type) )
      {
         case LOCK4ALL:
            rc = d4lockAddAll( data ) ;
            break ;
         case LOCK4APPEND:
            rc = d4lockAddAppend( data ) ;
            break ;
         case LOCK4FILE:
            rc = d4lockAddFile( data ) ;
            break ;
         case LOCK4RECORD:
            locks[i].recNum = ntohl5(locks[i].recNum) ;
            if ( locks[i].recNum < 1 )
               return error4( c4, e4lock, E81505 ) ;
            rc = d4lockAdd( data, locks[i].recNum ) ;
            break ;
         default:
            return error4( c4, e4lock, E81505 ) ;
      }
   }

   if ( rc != 0 )
      return rc ;

   rc = tran4lock( &client->trans ) ;
   if ( rc != 0 )   /* never leave in a state of pending locks */
      code4lockClear( c4 ) ;

   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientLock( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   CONNECTION4LOCK_INFO_IN *info ;
   CONNECTION4 *connection ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   connection = &client->connection ;

   if ( connection4len( connection ) < sizeof( CONNECTION4LOCK_INFO_IN ) )
      return e4packetLen ;

   info = (CONNECTION4LOCK_INFO_IN *)connection4data( connection ) ;
   if ( info == 0 )
      return error4( 0, e4corrupt, E91530 ) ;

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
   if ( data == 0 )
      return e4name ;

   info->test = ntohs5(info->test) ;
   info->type = ntohs5(info->type) ;
   if ( info->test != 0 && info->test != 1 )
      return error4( client->server->c4, e4corrupt, E91530 ) ;

   rc = 0 ;

   if ( info->test != 1 )
   {
      switch ( code4unlockAuto( client->server->c4 ) )
      {
         case LOCK4OFF:
            break ;
         case LOCK4ALL:
            rc = code4unlockDo( tran4dataList( &client->trans ) ) ;
            break ;
         case LOCK4DATA:
            // AS Apr 15/03 - support for new lockId for shared clone locking
            rc = d4unlockLow( data, data4lockId( data ), 0 ) ;
            break ;
         default:
            rc = error4( client->server->c4, e4unlock, E81506 ) ;
      }
   }

   Lock4type lockType = (Lock4type)ntohl5( info->lockType ) ;
   #ifdef E4DEBUG
      if ( lockType != lock4read && lockType != lock4write && lockType != lock4any )
      {
         // invalid lock type - possibly not being put into network byte order by client
         rc = error4( client->server->c4, e4corrupt, E91530 )  ;
      }
   #endif

   if ( rc == 0 )
      switch( info->type )
      {
         case LOCK4ALL:
            if ( info->test == 1 )  /* can't verify this */
               return 0 ;
            else
               rc = d4lockAllInternal( data, 0 ) ;
            break ;
         case LOCK4APPEND:
            if ( info->test == 1 )
               rc = d4lockTestAppend( data ) == 1 ;
            else
               rc = d4lockAppendInternal( data, 0 ) ;
            break ;
         case LOCK4FILE:
            if ( info->test == 1 )
               rc = dfile4lockTestFile( data->dataFile, connection4clientId( connection ), data4serverId( data ), lock4write ) == 1 ;
            else
               rc = d4lockFileInternal( data, 0, lockType ) ;
            break ;
         case LOCK4RECORD:
            if ( info->test == 1 )
               rc = d4lockTest( data, ntohl5(*(long *)(connection4data( connection ) + sizeof( CONNECTION4LOCK_INFO_IN ) ) ), lock4write ) == 1 ;
            else
               rc = d4lockInternal( data, ntohl5(*(long *)(connection4data( connection ) + sizeof( CONNECTION4LOCK_INFO_IN ) ) ), 0, lockType ) ;
            break ;
         default:
            rc = e4corrupt ;
      }

   connection4clear( connection ) ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
static int server4clientOpenInfoOutPrepare( SERVER4CLIENT *client, DATA4 *data )
{
   TAG4 *tagOn ;
   CONNECTION4OPEN_INFO_OUT *dataInfo ;
   CONNECTION4 *connection ;
   unsigned short uniqueTemp ;

   #ifdef E4ANALYZE
      int rc ;

      rc = server4clientVerify( client, 1 ) ;
      if ( rc < 0 )
      {
         d4close( data ) ;
         return rc ;
      }
   #endif

   connection = &client->connection ;
   connection4clear( connection ) ;

   connection4addData( connection, NULL, sizeof( CONNECTION4OPEN_INFO_OUT ), (void **)&dataInfo ) ;

   dataInfo->headerLen = htons5(data->dataFile->headerLen) ;
   // AS Jun 7/06 - recWidth is not large enough...in particular for large files...  also move to a better spot for byte alignment
   // dataInfo->recWidth = htons5((short)data->dataFile->recWidth) ;
   dataInfo->recWidth = htonl5(data->dataFile->recWidth) ;
   dataInfo->infoLen = htons5(data->dataFile->infoLen) ;
   dataInfo->serverId = htonl5(data4serverId( data )) ;
   dataInfo->readOnly = htons5( data->readOnly ) ;
   // AS Jun 17/03 - Pass the CodePage as well, may be required for expression module on client
   dataInfo->codePage = htons5( data->dataFile->codePage ) ;
   dataInfo->version = d4version( data ) ;
   #ifdef S4FOX
      dataInfo->longFieldNameSupported = htons5( data->dataFile->longFieldNamesSupported ) ;
   #else
      dataInfo->longFieldNameSupported = htons5( 0 ) ;
   #endif

   for ( tagOn = 0, dataInfo->numTags = 0 ;; )
   {
      tagOn = (TAG4 *)d4tagPrev( data, tagOn ) ;
      if ( tagOn == 0 )
         break ;
      dataInfo->numTags++ ;
   }

   connection4addData( connection, data->dataFile->info, data->dataFile->infoLen, NULL ) ;

   /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
   #ifdef S4CLIPPER
      dataInfo->fullPathNameLen = htons5( 0 ) ;   // for clipper don't do this since it doesn't apply
   #else
      tagOn = (TAG4 *)d4tagPrev( data, 0 ) ;
      if ( tagOn == 0 )
         dataInfo->fullPathNameLen = htons5( 0 ) ;   // for clipper don't do this since it doesn't apply
      else
      {
         const char *fullIndexName = tagOn->index->indexFile->file.name ;
         unsigned short fullIndexNameLen = strlen( fullIndexName ) + 1 ;  // add '1' for the null
         dataInfo->fullPathNameLen = htons5( fullIndexNameLen  );   // for clipper don't do this since it doesn't apply
         connection4addData( connection, fullIndexName, fullIndexNameLen, NULL ) ;
      }
   #endif

   for ( tagOn = 0, dataInfo->numTags = 0 ;; )
   {
      tagOn = (TAG4 *)d4tagPrev( data, tagOn ) ;
      if ( tagOn == 0 )
         break ;
      dataInfo->numTags++ ;
      connection4addData( connection, tfile4alias( tagOn->tagFile ), LEN4TAG_ALIAS, NULL ) ;
      uniqueTemp = htons5(tagOn->errUnique) ;
      connection4addData( connection, &uniqueTemp, sizeof( short int ), NULL ) ;
   }

   if ( error4code( client->server->c4 ) < 0 )
   {
      d4close( data ) ;
      return error4code( client->server->c4 ) ;
   }
   dataInfo->numTags = htons5(dataInfo->numTags) ;
   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRemoveTag( SERVER4CLIENT *client )
{
   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      DATA4 *data ;
      int rc ;
      CODE4 *c4 ;
      CONNECTION4 *connection ;
      CONNECTION4TAG_REMOVE_INFO_IN *dataIn ;

      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E91530 ) ;
      #endif

      c4 = client->server->c4 ;
      connection = &client->connection ;

      data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
      if ( data == 0 )
         return e4name ;

      dataIn = (CONNECTION4TAG_REMOVE_INFO_IN *)connection4data( connection ) ;
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, dataIn->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      TAG4 *tag = i4tag( i4, dataIn->tagName ) ;
      if ( tag == 0 )
         return e4name ;

      rc = i4tagRemove( tag ) ;
      connection4clear( connection ) ;
      return rc ;
   #endif /* S4OFF_WRITE */
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientAddTag( SERVER4CLIENT *client )
{
   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      DATA4 *data ;
      int offset, rc ;
      unsigned int i ;
      short int dataFileLocked ;
      INDEX4 *i4 ;
      CODE4 *c4 ;
      CONNECTION4 *connection ;
      CONNECTION4TAG_ADD_INFO_IN *dataIn ;
      CONNECTION4TAG_INFO *tagInfo ;

      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E91530 ) ;
      #endif

      c4 = client->server->c4 ;
      connection = &client->connection ;

      data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
      if ( data == 0 )
         return e4name ;

      dataIn = (CONNECTION4TAG_ADD_INFO_IN *)connection4data( connection ) ;
      dataIn->numTags = ntohs5(dataIn->numTags) ;
      i4 = d4index( data, dataIn->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;

      tagInfo = (CONNECTION4TAG_INFO *)u4allocFree( c4, ( (long)dataIn->numTags + 1L ) * sizeof( CONNECTION4TAG_INFO ) ) ;
      if ( tagInfo == 0 )
         return e4memory ;

      offset = sizeof( CONNECTION4TAG_ADD_INFO_IN ) ;

      for ( i = 0 ; i < dataIn->numTags ; i++ )
      {
         memcpy( &tagInfo[i], (char *)dataIn + offset, sizeof( CONNECTION4TAG_INFO ) ) ;
         tagInfo[i].name.ptr = (char *)dataIn + ntohs5(tagInfo[i].name.offset) ;
         tagInfo[i].expression.ptr = (char *)dataIn + ntohs5(tagInfo[i].expression.offset) ;
         offset += ( sizeof( CONNECTION4TAG_INFO ) + strlen( tagInfo[i].name.ptr )
                   + strlen( tagInfo[i].expression.ptr ) + 2 ) ;
         if ( tagInfo[i].filter.offset == 0 )
            tagInfo[i].filter.ptr = 0 ;
         else
         {
            tagInfo[i].filter.ptr = (char *)dataIn + ntohs5(tagInfo[i].filter.offset) ;
            offset += ( strlen( tagInfo[i].filter.ptr ) + 1 ) ;
         }
         tagInfo[i].unique = ntohs5(tagInfo[i].unique) ;
         tagInfo[i].descending = ntohs5(tagInfo[i].descending) ;
      }

      #ifdef S4CLIPPER
         // safety to affect whether or not can re-create
         int oldSafety = c4->safety ;
         c4->safety = (char)(dataIn->safety) ;
      #endif
      rc = I4tagAdd( i4, (TAG4INFO *)tagInfo ) ;
      #ifdef S4CLIPPER
         c4->safety = oldSafety ;
      #endif
      u4free( tagInfo ) ;
      dataFileLocked = dfile4lockTestFile( data->dataFile, connection4clientId( connection ), connection4serverId( connection ), lock4write ) == 1 ;
      connection4clear( connection ) ;
      connection4addData( connection, &dataFileLocked, sizeof( short int ), NULL ) ;
      // AS Apr 23/04 - we need to clear the error code out for the next potential call (don't want an r4unique hanging around)
      switch( error4code( c4 ) )
      {
         case r4unique:
         case r4uniqueContinue:
         case r4candidate:
            error4set( c4, 0 ) ;
            break ;
         default:
            break ;
      }

      return rc ;
   #endif /* S4OFF_WRITE */
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
// AS 09/15/00 function for swapping backup log files for auto-recovery
int server4clientSwapLogFile( SERVER4CLIENT *client )
{
   /*
      for auto-recovery, this function is called when the backup server wants to update its
      files locally (as a periodic backup procedure).  What the server does is switch to using
      an alternate backup file.  This allows the 'current' backup file to be closed and used
      by the backup server directly to update its files, and then discard it (putting any
      extra entries in the pre-pend backup file).

      What we do here is as follows:
      1.  Check that the 'new' backup file is an empty file (if not, then the backup utility
          must not have properly processed it previously, so send an error back to the backup
          utility).
      2.  Swap the 2 backup field entries in the configuration file (so that if server restarts
          it starts with the correct backup log file).
      3.  internally switch over to the new backup file.
      4.  send the name of the old backup log file to the caller
   */

   SERVER4 *server = client->server ;
   CODE4 *c4 = server->c4 ;
   CONNECT4 *connect = &client->connect ;

   if ( server->transFile.backup.validState == 0 )  //
   {
      // AS Oct 22/01 - This is actually an unsupported error - backups not enabled.
      connect4sendShort( connect, e4notSupported ) ;
      connect4sendFlush( connect ) ;
      return e4config ;
   }

   FILE4 newBackupFile ;
   int oldAccessMode = c4->accessMode ;
   c4->accessMode = OPEN4DENY_RW ;  // ensure only we can access it...
   int oldErrOpen = c4->errOpen ;
   c4->errOpen = 0 ;
   // AS Mar 17/03 - should be using internal open...
   short rc = file4openInternal( &newBackupFile, c4, server->backupLogName2, 0, OPT4NONE ) ;
   c4->errOpen = oldErrOpen ;
   if ( rc == e4fileFind || rc == r4noExist )  // file doesn't exist (yet) - try to create it...
   {
      error4set( c4, 0 ) ;
      int oldSafety = c4->safety ;
      c4->safety = 1 ;  // ensure we do not overwrite if in use, etc.
      // TRAN4FILE_LOW tempTranFile ;
      // tran4fileLowInit( &tempTranFile, &server->c4trans, &server->transFile ) ;
      // rc = tran4fileLowCreate( &tempTranFile, server->backupLogName2 ) ;
      rc = file4createInternal( &newBackupFile, c4, server->backupLogName2, 0, OPT4NONE ) ;
      c4->safety = oldSafety ;
      // if ( rc == 0 )
      //    rc = tran4fileLowClose( &tempTranFile ) ;
      // if ( rc == 0 )
      //    rc = file4open( &newBackupFile, c4, server->backupLogName2, 0 ) ;
   }
   c4->accessMode = oldAccessMode ;

   if ( rc != 0 )
   {
      connect4sendShort( connect, rc ) ;
      connect4sendFlush( connect ) ;
      return rc ;
   }

   FILE4LONG fileLen = file4lenLow( &newBackupFile ) ;
   // the filelength should be 30 - i.e. just enough to contain the first entry which indicates it was closed with shutdown
   if ( file4longGetHi( fileLen ) != 0 || file4longGetLo( fileLen ) != 0 )
   {
      connect4sendShort( connect, e4len ) ;
      file4close( &newBackupFile ) ;
      // AS 12/19/00 - on e4len error, we send back the name of the log file so that it can be re-processed
      // usutally e4len means that the recovery procedure was not completed, so needs to be retried with the
      // file we are giving here...
      connect4sendString( connect, server->backupLogName2 ) ;
      connect4sendFlush( connect ) ;
      return e4len ;
   }

   file4close( &newBackupFile ) ;
   DATA4 *config = d4open( c4, server->configName ) ;
   if ( config == 0 )
   {
      connect4sendShort( connect, e4open ) ;
      connect4sendFlush( connect ) ;
      return e4open ;
   }

   FIELD4 *fieldBackLog1 = d4field( config, "BACKLOG1" ) ;
   FIELD4 *fieldBackLog2 = d4field( config, "BACKLOG2" ) ;
   if ( fieldBackLog1 == 0 || fieldBackLog2 == 0 )
   {
      d4close( config ) ;
      connect4sendShort( connect, e4config ) ;
      connect4sendFlush( connect ) ;
      return e4config ;
   }

   // put the 'backup swap close' marker in the log file so it is closed
   // but still noted as valid
   tran4fileLowMarkBackupClose( &(server->transFile.backup) )  ;

   rc = tran4fileLowClose( &(server->transFile.backup) ) ;
   if ( rc != 0 )
   {
      d4close( config ) ;
      connect4sendShort( connect, e4close ) ;
      connect4sendFlush( connect ) ;
      return e4close ;
   }

   // AS Aug 15/01 - ensure that logging is not diabled
   int oldDisabled = c4->logDisable ;
   c4->logDisable = 0 ;
   rc = tran4fileLowOpen( &(server->transFile.backup), server->backupLogName2 ) ;
   c4->logDisable = oldDisabled ;
   if ( rc != 0 )
   {
      d4close( config ) ;
      connect4sendShort( connect, e4open ) ;
      connect4sendFlush( connect ) ;
      // reopen the previous log file to compensate...
      error4set( c4, 0 ) ;
      tran4fileLowOpen( &(server->transFile.backup), server->backupLogName1 ) ;
      return e4open ;
   }

   // AS 01/02/00 - Need to reset the length setting.
   tran4fileLowLenSet( &server->transFile.backup, file4lenLow( &(server->transFile.backup.file )) ) ;


   // assign the 'backlog2' name to 'backlog1' and vice versa
   f4assign( fieldBackLog1, server->backupLogName2 ) ;
   f4assign( fieldBackLog2, server->backupLogName1 ) ;

   d4close( config ) ;

   char tempBackupName[LEN4PATH] ;   // to save the old name when swapping the names in the SERVER4 structure
   assert5( sizeof( tempBackupName ) == sizeof( server->backupLogName1 ) && sizeof( tempBackupName ) == sizeof( server->backupLogName2 ) ) ;

   memcpy( tempBackupName, server->backupLogName1, LEN4PATH ) ;
   memcpy( server->backupLogName1, server->backupLogName2, LEN4PATH ) ;
   memcpy( server->backupLogName2, tempBackupName, LEN4PATH ) ;

   connect4sendShort( connect, 0 ) ;
   connect4sendString( connect, tempBackupName ) ;
   connect4sendFlush( connect ) ;

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCreateIndex( SERVER4CLIENT *client )
{
   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      int rc = 0 ;

      #ifdef E4ANALYZE
         rc = server4clientVerify( client, 1 ) ;
         if ( rc < 0 )
            return rc ;
      #endif

      CONNECTION4 *connection = &client->connection ;
      CODE4 *c4 = client->server->c4 ;

      CONNECTION4INDEX_CREATE_INFO_IN *dataIn = (CONNECTION4INDEX_CREATE_INFO_IN *)connection4data( connection ) ;
      dataIn->numTags = ntohs5(dataIn->numTags) ;
      if ( dataIn->numTags == 0 )
         return e4create ;

      // AS Jan 1/03 - support for creating temporary non updating indexes
      short isTemp = dataIn->createTemp ;   // if set to 2, means we create a temporary non-updating index

      DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
      if ( data == 0 )
         return e4name ;

      CONNECTION4TAG_INFO *tagInfo = (CONNECTION4TAG_INFO *)u4allocFree( c4, ( (long)dataIn->numTags + 1L ) * sizeof( CONNECTION4TAG_INFO ) ) ;
      if ( tagInfo == 0 )
         return e4memory ;

      int offset = sizeof( CONNECTION4INDEX_CREATE_INFO_IN ) ;

      for ( unsigned int i = 0 ; i < dataIn->numTags ; i++ )
      {
         memcpy( &tagInfo[i], (char *)dataIn + offset, sizeof( CONNECTION4TAG_INFO ) ) ;
         tagInfo[i].name.ptr = (char *)dataIn + ntohs5(tagInfo[i].name.offset) ;
         tagInfo[i].expression.ptr = (char *)dataIn + ntohs5(tagInfo[i].expression.offset) ;
         offset += ( sizeof( CONNECTION4TAG_INFO ) + strlen( tagInfo[i].name.ptr )
                   + strlen( tagInfo[i].expression.ptr ) + 2 ) ;
         if ( tagInfo[i].filter.offset == 0 )
            tagInfo[i].filter.ptr = 0 ;
         else
         {
            tagInfo[i].filter.ptr = (char *)dataIn + ntohs5(tagInfo[i].filter.offset) ;
            offset += ( strlen( tagInfo[i].filter.ptr ) + 1 ) ;
         }
         tagInfo[i].unique = ntohs5(tagInfo[i].unique) ;
         tagInfo[i].descending = ntohs5(tagInfo[i].descending) ;
      }

      char *name ;
      if ( dataIn->isProduction )
         name = 0 ;
      else
      {
         name = dataIn->indexFileName ;
         #ifndef S4UNIX
            c4upper( name ) ;
         #endif
      }

      #ifdef S4FOX
         Bool5 doSetBlockSize = (dataIn->foxCreateIndexBlockSize != 0 ) ;  // 0 means don't set from defaults...
         if ( doSetBlockSize )
            rc = code4indexBlockSizeSet( c4, htons5( dataIn->foxCreateIndexBlockSize ) ) ;
      #endif
      if ( rc == 0 )
      {
         Collate4name collateName = (Collate4name)htons5( dataIn->collateName ) ;
         Collate4name collateNameUnicode = (Collate4name)htons5( dataIn->collateNameUnicode ) ;
         // AS Jan 1/03 - support for creating temporary non updating indexes
         int oldCreateTemp = c4getCreateTemp( c4 ) ;
         if ( isTemp == 2 )
            c4setCreateTemp( c4, isTemp ) ;
         else
            c4setCreateTemp( c4, 0 ) ;   // if data file is temp will mark index automatically...

         rc = I4create( data, name, (TAG4INFO *)tagInfo, OPEN4DENY_RW, (char)dataIn->safety, c4getCreateTemp( c4 ), (char)dataIn->readOnly,
                       (char)dataIn->fileFlush, ntohs5( dataIn->collatingSequence ), collateName, collateNameUnicode ) ;

         c4setCreateTemp( c4, oldCreateTemp ) ;
      }
      #ifdef S4FOX
         if ( doSetBlockSize )
            code4indexBlockSizeSet( c4, -1 ) ;   // set default back to fox defaults...
      #endif

      u4free( tagInfo ) ;

      short int dataFileLocked = htons5(dfile4lockTestFile( data->dataFile, connection4clientId( connection ), connection4serverId( connection ), lock4write ) == 1 ) ;
      connection4clear( connection ) ;
      connection4addData( connection, &dataFileLocked, sizeof( short int ), NULL ) ;

      // AS Apr 23/04 - we need to clear the error code out for the next potential call (don't want an r4unique hanging around)
      if ( rc == 0 )
      {
         switch( error4code( c4 ) )
         {
            case r4unique:
            case r4uniqueContinue:
            case r4candidate:
               error4set( c4, 0 ) ;
               break ;
            default:
               break ;
         }
      }

      return rc ;
   #endif /* S4OFF_WRITE */
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCreate( SERVER4CLIENT *client )
{
   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      CONNECTION4TAG_INFO *tagInfo = 0 ;
      short int offset ;
      int rc = 0, isTemp, len ;
      unsigned int i ;
      #ifdef S4FOX
         int oldCompatibility ;
      #endif
      #ifndef S4OFF_CATALOG
         int oldUseCatalog, doAdd ;
         char alias[LEN4DATA_ALIAS+1], pathName[LEN4PATH+1] ;
      #endif

      #ifdef E4ANALYZE
         rc = server4clientVerify( client, 1 ) ;
         if ( rc < 0 )
            return rc ;
      #endif

      CONNECTION4 *connection = &client->connection ;
      SERVER4 *server = client->server ;
      CODE4 *c4 = server->c4 ;

      #ifdef E4ANALYZE
         if ( sizeof( CONNECTION4PTR_UNION ) != sizeof( char * ) )
            return error4( c4, e4info, E81507 ) ;
      #endif

      long dataLen = connection4len( connection ) ;
      if ( dataLen < sizeof( CONNECTION4CREATE_INFO_IN ) )
         return e4packetLen ;

      CONNECTION4CREATE_INFO_IN *dataIn = (CONNECTION4CREATE_INFO_IN *)connection4data( connection ) ;
      unsigned short numFields = ntohs5(dataIn->numFields) ;
      if ( numFields == 0 )
         return e4create ;
      unsigned short numTags = ntohs5(dataIn->numTags ) ;

      if ( dataLen < (long)sizeof( CONNECTION4CREATE_INFO_IN )+ (long)numFields * (long)sizeof( FIELD4INFO ) +
          (long)numTags * (long)sizeof( TAG4INFO ) )
         return error4( c4, e4packetLen, E91530 ) ;

      CONNECTION4FIELD_INFO *fieldInfo = (CONNECTION4FIELD_INFO *)u4allocFree( c4, ( (long)numFields + 1L ) * sizeof( CONNECTION4FIELD_INFO ) ) ;
      if ( fieldInfo == 0 )
      {
         rc = e4memory ;
      }
      else
      {
         fieldInfo[numFields].name.ptr = 0 ;
         for ( offset = sizeof(CONNECTION4CREATE_INFO_IN), i = 0 ; i < numFields ; i++ )
         {
            memcpy( &fieldInfo[i], (char *)dataIn + offset, sizeof( CONNECTION4FIELD_INFO ) ) ;
            fieldInfo[i].name.offset = ntohs5(fieldInfo[i].name.offset) ;
            if ( (long)fieldInfo[i].name.offset > dataLen )  /* gives 10 character leeway */
               return error4( c4, e4packet, E91530 ) ;
            fieldInfo[i].type = ntohs5(fieldInfo[i].type) ;
            fieldInfo[i].len = ntohs5(fieldInfo[i].len) ;
            fieldInfo[i].dec = ntohs5(fieldInfo[i].dec) ;
            fieldInfo[i].nulls = ntohs5(fieldInfo[i].nulls) ;
            fieldInfo[i].name.ptr = (char *)dataIn + fieldInfo[i].name.offset ;
            offset += ( sizeof( CONNECTION4FIELD_INFO ) + strlen( fieldInfo[i].name.ptr ) + 1 ) ;
         }

         if ( numTags > 0 )
         {
            tagInfo = (CONNECTION4TAG_INFO *)u4allocFree( c4, ( (long)numTags + 1L ) * sizeof( CONNECTION4TAG_INFO ) ) ;
            if ( tagInfo == 0 )
            {
               u4free( fieldInfo ) ;
               rc = e4memory ;
            }
            else
            {
               for ( i = 0 ; i < numTags ; i++ )
               {
                  memcpy( &tagInfo[i], (char *)dataIn + offset, sizeof( CONNECTION4TAG_INFO ) ) ;
                  tagInfo[i].name.offset = ntohs5(tagInfo[i].name.offset) ;
                  tagInfo[i].expression.offset = ntohs5(tagInfo[i].expression.offset) ;
                  tagInfo[i].filter.offset = ntohs5(tagInfo[i].filter.offset) ;

                  if ( (long)tagInfo[i].name.offset > dataLen || (long)tagInfo[i].expression.offset > dataLen || (long)tagInfo[i].filter.offset > dataLen )
                     return error4( c4, e4packet, E91530 ) ;

                  tagInfo[i].unique = ntohs5(tagInfo[i].unique) ;
                  tagInfo[i].descending = ntohs5(tagInfo[i].descending) ;
                  tagInfo[i].name.ptr = (char *)dataIn + tagInfo[i].name.offset ;

                  // AS Jan 7/04 - not taking into account that DEL4REUSE has no expression...

                  if ( tagInfo[i].expression.offset == 0 )
                  {
                     tagInfo[i].expression.ptr = 0 ;
                     offset += ( sizeof( CONNECTION4TAG_INFO ) + strlen( tagInfo[i].name.ptr ) + 1 ) ;
                  }
                  else
                  {
                     tagInfo[i].expression.ptr = (char *)dataIn + tagInfo[i].expression.offset ;
                     offset += ( sizeof( CONNECTION4TAG_INFO ) + strlen( tagInfo[i].name.ptr )
                               + strlen( tagInfo[i].expression.ptr ) + 2 ) ;
                  }

                  if ( tagInfo[i].filter.offset == 0 )
                     tagInfo[i].filter.ptr = 0 ;
                  else
                  {
                     tagInfo[i].filter.ptr = (char *)dataIn + tagInfo[i].filter.offset ;
                     offset += ( strlen( tagInfo[i].filter.ptr ) + 1 ) ;
                  }
               }
            }
         }
         else
         {
            tagInfo = 0 ;
         }
      }

      isTemp = dataIn->createTemp ;
      len = strlen( dataIn->name ) ;
      if ( len == 0 && isTemp == 0 )   /* invalid */
         return e4create ;

      #ifndef S4CASE_SEN
         c4upper( dataIn->name ) ;
      #endif

      DATA4 *data = 0 ;

      #ifdef S4FOX
         assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
         oldCompatibility = c4->compatibility ;
         c4->compatibility = ntohs5(dataIn->compatibility) ;
         if ( c4->compatibility != 30 && c4->compatibility != 26 && c4->compatibility != 25 )
            rc = e4create ;
         double oldAutoIncrementStart = c4->autoIncrementStart ;
         c4->autoIncrementStart = dataIn->autoIncrementStart ;
         // AS Jun 27/02 - support for compressed memos
         assert5port( "added compressed memo entries support" ) ;
         int oldCompressedMemos = c4->compressedMemos ;
         c4->compressedMemos = (unsigned char)ntohs5(dataIn->compressedMemosSupported) ;
      #endif

      // AS Nov 6/02 - support client requesting a preprocessed file
      #ifdef S4PREPROCESS_FILE
         int oldPreprocess = code4getPreprocessFile( c4 ) ;
         code4setPreprocessFile( c4, ntohs5(dataIn->preprocessFile) ) ;
      #endif

      if ( rc >= 0 )
      {
         int oldKeepOpen = server->keepOpen ;
         server->keepOpen = 1 ;
         int oldLog = c4->log ;
         if ( c4->log == LOG4ON )   /* means to use client setting */
            c4->log = ntohs5(dataIn->log) ;
         int oldReadOnly = c4getReadOnly( c4 ) ;
         c4setReadOnly( c4, 0 ) ;
         int oldOledbSchemaCreate = c4->oledbSchemaCreate ;
         c4->oledbSchemaCreate = (char)dataIn->oledbSchemaCreate ;
         int oldTemp = c4->createTemp ;
         if ( isTemp )
            c4->createTemp = 1 ;
         else
            c4->createTemp = 0 ;

         // AS Jul 6/06 - was not setting this value...
         #ifdef S4FOX
            Bool5 doSetBlockSize = ( dataIn->foxCreateIndexBlockSize != 0 ) && ( tagInfo != 0 ) ;  // 0 means don't set from defaults...
            if ( doSetBlockSize )
               rc = code4indexBlockSizeSet( c4,  htons5( dataIn->foxCreateIndexBlockSize ) ) ;
         #endif

         /* AS 07/21/99 - added parm for win 95/98 to avoid endless lazy writes */
         Collate4name collateName = (Collate4name)htons5( dataIn->collateName ) ;
         Collate4name collateNameUnicode = (Collate4name)htons5( dataIn->collateNameUnicode ) ;
         rc = D4create( c4, ((len == 0) ? 0 : dataIn->name ), (FIELD4INFO *)fieldInfo, (TAG4INFO *)tagInfo,
                        &data, (char)OPEN4DENY_RW, (char)dataIn->safety, (int)(ntohs5( dataIn->fileFlush ) ),
                        ntohs5( dataIn->collatingSequence ), ntohs5( dataIn->codePage ), collateName, collateNameUnicode ) ;

         // AS Jul 6/06 - was not setting this value...
         #ifdef S4FOX
            if ( doSetBlockSize )
               code4indexBlockSizeSet( c4, -1 ) ;   // set default back to fox defaults...
         #endif

         c4->createTemp = oldTemp ;
         c4->oledbSchemaCreate = oldOledbSchemaCreate ;
         c4setReadOnly( c4, oldReadOnly ) ;
         c4->log = oldLog ;
         server->keepOpen = oldKeepOpen ;
      }

      #ifdef S4PREPROCESS_FILE
         code4setPreprocessFile( c4, oldPreprocess ) ;
      #endif
      #ifdef S4FOX
         // AS Jun 27/02 - support for compressed memos
         assert5port( "added compressed memo entries support" ) ;
         c4->compressedMemos = oldCompressedMemos ;
         c4->autoIncrementStart = oldAutoIncrementStart ;
         c4->compatibility = oldCompatibility ;
      #endif

      if ( isTemp )
      {
         if ( data == 0 )
         {
            if ( rc >= 0  )
            rc = e4create ;
         }
         else
         {
            data->dataFile->singleClient = client ;
            data->trans = &client->trans ;
         }
      }
      else
         if ( data != 0 && rc >= 0 )
            rc = e4create ;

      u4free( fieldInfo ) ;
      if ( tagInfo != 0 )
         u4free( tagInfo ) ;
      if ( rc < 0 )
         return rc ;

      connection4clear( connection ) ;

      if ( isTemp )
      {
         connection4addData( connection, dfile4name( data->dataFile ), strlen( dfile4name( data->dataFile ) ), NULL ) ;
         int oldKeepOpen = server->keepOpen ;
         server->keepOpen = 2 ;
         d4closeTemp( data ) ;  // CJ-26/10/99-Does not log the close in the transaction file as the open does not.
         server->keepOpen = oldKeepOpen ;
      }

      return 0 ;
   #endif /* S4OFF_WRITE */
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientStatus( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int oldKeepOpen ;
   short len ;
   CONNECT4 *connect = &client->connect ;
   CODE4 *c4 = client->server->c4 ;

   // open the data file as a system table, avoids STREAM4UNLOCK sending
   // unlock back to original client
   c4->currentClient = c4->catalogClient ;
   data = code4connectionStatus( c4 ) ;
   if ( data == NULL )
   {
      short rc = error4code( c4 ) < 0 ? error4code( c4 ) : -1 ;
      error4set( c4, 0 ) ;
      c4->currentClient = client ;  // don't reset client until we have fixed error code and retrieved it above
      connect4sendShort( connect, rc) ;
      connect4sendFlush( connect ) ;
      return 0 ;
   }
   c4->currentClient = client ;

   len = (short)strlen( dfile4name( data->dataFile ) ) ;

   connect4sendShort( connect, 0 ) ;
   connect4sendShort( connect, len ) ;
   connect4send( connect, dfile4name( data->dataFile ), len ) ;
   connect4sendFlush( connect ) ;

   oldKeepOpen = client->server->keepOpen ;
   client->server->keepOpen = 2 ;
   d4closeTemp( data ) ;  // CJ-12/08/99-Does not log the close in the transaction file as the open does not.
   client->server->keepOpen = oldKeepOpen ;
   return 0 ;
}



#ifndef S4OFF_SECURITY
   /* not S4OFF_SECURITY, not S4OFF_COMMUNICATIONS, S4SERVER */
   int d4authorize( DATA4 *data, SERVER4CLIENT *client )
   {
      #ifndef S4OFF_CATALOG
         int rc, len, len2, i, lenMatch ;
         char key[LEN4USERID+LEN4DATA_ALIAS+1], *owner ;
         SERVER4 *server ;
         CODE4 *c4 ;

         #ifdef E4PARM_LOW
            if ( data == 0 || client == 0 )
               return error4( 0, e4parm_null, E91521 ) ;
         #endif

         server = client->server ;
         c4 = server->c4 ;

         if ( client->dbaAuthorize == AUTH4ALLOW || server->tableAuth == 0 )
         {
            memset( &data->authorize, AUTH4ALLOW, sizeof( data->authorize ) ) ;
            return 0 ;
         }

         if ( cat4avail( c4->catalog ) == 0 )
            return 0 ;
         if ( c4->catalog->catalogStatus == 0 )
            return 0 ;

         code4enterExclusive( c4, c4->catalogClient, 1 ) ;

         rc = cat4find( c4->catalog, d4alias( data ), CAT4DBF ) ;
         if ( rc == 0 )
         {
            owner = cat4owner( c4->catalog ) ;
            if ( owner != 0 )
            {
               len2 = strlen( client->trans.userId ) ;
               lenMatch = 1 ;
               for( i = LEN4USERID - 1 ; i >= len2 ; i-- )
               {
                  if ( owner[i] != ' ' )
                  {
                     lenMatch = 0 ;
                     break ;
                  }
               }
               if ( lenMatch == 1 )
                  if ( memcmp( owner, client->trans.userId, len2 ) == 0 )
                  {
                     memset( &data->authorize, AUTH4ALLOW, sizeof( data->authorize ) ) ;
                     code4exitExclusive(c4, c4->catalogClient) ;
                     return 0 ;
                  }
            }
         }

         memset( key, ' ', sizeof( key ) - 1 ) ;
         len = strlen( client->trans.userId ) ;
         if ( len > LEN4USERID )
            len = LEN4USERID ;
         memcpy( key, client->trans.userId, len ) ;
         len = strlen( d4alias( data ) ) ;
         if ( len > LEN4DATA_ALIAS )
            len = LEN4DATA_ALIAS ;
         memcpy( key+LEN4USERID, d4alias( data ), (unsigned int)len ) ;
         key[sizeof(key)-1] = 0 ;

         d4tagSelect( server->authorize, d4tag( server->authorize, "AUTH" ) ) ;

         rc = d4seek( server->authorize, key ) ;
         // AS Apr 15/03 - support for new lockId for shared clone locking
         d4unlockLow( server->authorize, data4lockId( server->authorize ), 0 ) ;
         if ( rc != 0 )
         {
            /* try PUBLIC access */
            memcpy( key, "PUBLIC", 6 ) ;
            rc = d4seek( server->authorize, key ) ;
            d4unlockLow( server->authorize, data4lockId( server->authorize), 0 ) ;
            if ( rc != 0 )
            {
               code4exitExclusive(c4, c4->catalogClient) ;
               return e4authorize ;
            }
         }

         if ( f4true( server->a4open ) == 0 )
         {
            code4exitExclusive(c4, c4->catalogClient) ;
            return e4authorize ;
         }
         else
         {
            data->authorize.open = AUTH4ALLOW ;
            if ( f4true( server->a4insert ) )
               data->authorize.append = AUTH4ALLOW ;

            if ( f4true( server->a4delete ) )
               data->authorize.delet = AUTH4ALLOW ;

            if ( f4true( server->a4update ) )
               data->authorize.update = AUTH4ALLOW ;

            if ( f4true( server->a4read ) )
               data->authorize.read = AUTH4ALLOW ;

            if ( f4true( server->a4index ) )
               data->authorize.index = AUTH4ALLOW ;

            if ( f4true( server->a4update ) )
               data->authorize.alter = AUTH4ALLOW ;

   /*         if ( f4true( server->a4refer ) )*/
   /*            data->authorize.refer = AUTH4ALLOW ;*/

            if ( f4true( server->a4compress ) )
               data->authorize.compress = AUTH4ALLOW ;
         }
         code4exitExclusive(c4, c4->catalogClient) ;
      #endif
      return 0 ;
   }
#endif /* S4OFF_SECURITY*/



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientOpen( SERVER4CLIENT *client )
{
   DATA4 *data ;
   CODE4 *c4 ;
   CONNECTION4OPEN_INFO_IN *dataIn ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif
   c4 = client->server->c4 ;

   dataIn = (CONNECTION4OPEN_INFO_IN *)connection4data( &client->connection ) ;

   data = D4open( c4,
                  dataIn->name,
                  ntohs5( dataIn->accessMode ),
                  ntohs5( dataIn->errDefaultUnique ),
                  ntohs5( dataIn->log ),
                  ntohs5( dataIn->openForCreate ),
                  ntohs5( dataIn->fileFlush ), /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
                  ntohs5( dataIn->readOnly ),
                  ntohs5( dataIn->singleOpen ),
                  ntohs5( dataIn->compatibility ) ) ;

   if ( data == 0 )
      return error4code( c4 ) ;

   return server4clientOpenInfoOutPrepare( client, data ) ;
}

/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientOpenIndex( SERVER4CLIENT *client )
{
   /* note that open index is not recorded in transaction file */

   int rc ;
   short int errUnique ;
   INDEX4 *index ;
   DATA4 *data ;
   CONNECTION4OPEN_INDEX_INFO_IN *in ;
   CONNECTION4OPEN_INDEX_INFO_OUT *info ;
   CONNECTION4 *connection ;
   CODE4 *c4 ;
   const char *name ;
   TAG4 *tagOn ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   c4 = client->server->c4 ;

   connection = &client->connection ;

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( &client->connection ) < sizeof( in ) )
      return e4packetLen ;

   in = (CONNECTION4OPEN_INDEX_INFO_IN *)connection4data( connection ) ;
   short errDefaultUnique = ntohs5(in->errDefaultUnique) ;
   switch( errDefaultUnique )
   {
      case r4unique:
      case r4uniqueContinue:
      case e4unique:
         break ;
      default:
         return error4( c4, e4unique, E81718 ) ;
   }

   if ( connection4len( &client->connection ) != ((long)sizeof( CONNECTION4OPEN_INDEX_INFO_IN ) + (long)ntohs5(in->nameLen)) )
      return e4packetLen ;

   name = connection4data( connection ) + sizeof( CONNECTION4OPEN_INDEX_INFO_IN ) ;

   // AS 01/29/01 - Was not using input accessMode as required...
   short accessMode = ntohs5( in->accessMode ) ;
   short safety  = ntohs5( in->safety ) ;
   short fileFlush  = ntohs5( in->fileFlush ) ;
   short readOnly  = ntohs5( in->readOnly ) ;
   short openForCreate  = ntohs5( in->openForCreate ) ;

   /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
   /* AS 04/05/01 - if the client requests the open after a create and it is temp, mark it as such */
   // AS Jul 6/04 - default unique is an int, not a char
   index = I4open( data, name, (char)accessMode, errDefaultUnique, (char)readOnly, (char)accessMode, (char)openForCreate, (char)fileFlush, (char)safety, (char)in->createTemp ) ;

   // AS 01/07/00 --> c4->errorCode is set to r4noOpen if I4open() indicates file already open.
   // for example, the client may have re-requested an index using a different path identifier which
   // actually points to the same index file, in which case, the client cannot catch this, so catch it
   // here instead.
   rc = error4code( c4 ) ;

   connection4clear( connection ) ;

   // AS 01/07/99 -- actually, r4noOpen is ok, it just means that the index was already opened, but
   // that presumeably the client did not itself open it, so just send back info anyway.
   // if ( rc == r4noOpen )
   //    return rc ;
   if ( rc == r4noOpen )
      error4set( c4, 0 ) ;

   if ( index == 0 )
   {
      rc = error4code( c4 ) ;
      if ( rc < 0 )
         return rc ;
      return e4open ;
   }
   else
   {
      connection4addData( connection, NULL, sizeof( CONNECTION4OPEN_INDEX_INFO_OUT ), (void **)&info ) ;
      /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
      #ifdef S4CLIPPER
         info->numTags = ntohs5((unsigned short)index->tags.nLink) ;
         info->fullPathNameLen = htons5( 0 ) ;   // for clipper don't do this since it doesn't apply
      #else
         info->numTags = ntohs5((unsigned short)index->indexFile->tags.nLink) ;
         const char *fullIndexName = index->indexFile->file.name ;
         unsigned short fullIndexNameLen = strlen( fullIndexName ) + 1 ;  // add '1' for the null
         info->fullPathNameLen = htons5( fullIndexNameLen ) ;   // for clipper don't do this since it doesn't apply
         connection4addData( connection, fullIndexName, fullIndexNameLen, NULL ) ;
      #endif

      for ( tagOn = 0 ;; )
      {
         tagOn = (TAG4 *)l4prev( &index->tags, tagOn ) ;
         if ( tagOn == 0 )
            break ;
         connection4addData( connection, tfile4alias( tagOn->tagFile ), LEN4TAG_ALIAS, NULL ) ;
         errUnique = htons5(tagOn->errUnique) ;
         connection4addData( &client->connection, &errUnique, sizeof( short int ), NULL ) ;
      }
   }

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientSeek( SERVER4CLIENT *client )
{
   #ifdef S4OFF_INDEX
      return e4notSupported ;
   #else
      DATA4 *data ;
      int rc ;
      CONNECTION4SEEK_INFO_IN *info ;
      CONNECTION4GO_INFO_OUT *out ;
      TAG4 *tag ;
      const char *key ;

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( connection4len( &client->connection ) < sizeof( info ) )
         return e4packetLen ;

      info = (CONNECTION4SEEK_INFO_IN *)connection4data( &client->connection ) ;
      info->keyLen = ntohs5(info->keyLen) ;
      if ( connection4len( &client->connection ) != (long)sizeof( CONNECTION4SEEK_INFO_IN ) + info->keyLen + 1 )
         return e4packetLen ;

      key = connection4data( &client->connection ) + sizeof( CONNECTION4SEEK_INFO_IN ) ;

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;

      if ( data == 0 )
         return e4name ;

      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;
      info->startPos = ntohl5(info->startPos) ;
      assert5port( "Added IncludeMemos member to this communications message" );
      short includeMemos = info->includeMemos ;
      if ( info->fromCurrentPos && info->startPos > 0L )
      {
         rc = D4goRead( data, info->startPos ) ;  /* don't setOldRecord since server only does reading at this point */
         if ( rc == 0 )
            rc = D4seekN( data, tag, key, info->keyLen, 1 ) ;
      }
      else
      {
         // AS Oct 21/02 - added to support seek without data movement and record transfer
         if ( info->doDataPosition == 1 )
            rc = D4seekN( data, tag, key, info->keyLen, 0 ) ;
         else  // don't need data position out, so just ignore...
            rc = t4seekNdo( data, tag, key, info->keyLen, 0 ) ;
      }

      if ( rc < 0 || rc == r4eof )
         return rc ;

      connection4clear( &client->connection ) ;
      connection4addData( &client->connection, NULL, sizeof( CONNECTION4GO_INFO_OUT ), (void **)&out ) ;
      if ( info->doDataPosition )
      {
         out->recNo = htonl5(d4recNo( data )) ;
         out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
         connection4addData( &client->connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;
         if ( includeMemos )
            if ( server4clientSendMemos( client, data ) < 0 )
               return -1 ;
      }
      return rc ;
   #endif /* S4OFF_INDEX */
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientSeekDouble( SERVER4CLIENT *client )
{
   #ifdef S4OFF_INDEX
      return e4notSupported ;
   #else
      DATA4 *data ;
      #ifdef E4ANALYZE
         int rc ;
      #endif
      int saveRc ;
      CONNECTION4SEEK_INFO_IN *info ;
      CONNECTION4GO_INFO_OUT *out ;
      TAG4 *tag ;
      const char *key ;
      short keyLen ;
      S4LONG startPos ;

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( connection4len( &client->connection ) < sizeof( info ) )
         return e4packetLen ;

      info = (CONNECTION4SEEK_INFO_IN *)connection4data( &client->connection ) ;
      keyLen = ntohs5(info->keyLen ) ;
      if ( connection4len( &client->connection ) != (long)sizeof( CONNECTION4SEEK_INFO_IN ) + keyLen + 1 )
         return e4packetLen ;

      key = connection4data( &client->connection ) + sizeof( CONNECTION4SEEK_INFO_IN ) ;

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;

      if ( data == 0 )
         return e4name ;

      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;

      if ( keyLen != sizeof( double ) )
         return e4seek ;
      assert5port( "Added IncludeMemos member to this communications message" );
      short includeMemos = info->includeMemos ;
      startPos = ntohl5(info->startPos) ;
      if ( info->fromCurrentPos && startPos > 0L )
      {
         saveRc = D4goRead( data, startPos ) ;  /* don't setOldRecord since server only does reading at this point */
         if ( saveRc == 0 )
            saveRc = D4seekDouble( data, tag, ntohd(*((double *)key)), 1 ) ;
      }
      else
         saveRc = D4seekDouble( data, tag, ntohd(*((double *)key)), 0 ) ;

      if ( saveRc < 0 )
         return saveRc ;
      connection4clear( &client->connection ) ;
      connection4addData( &client->connection, NULL, sizeof( CONNECTION4GO_INFO_OUT ), (void **)&out ) ;
      out->recNo = htonl5( d4recNo( data ) ) ;
      out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
      connection4addData( &client->connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;
      if ( includeMemos )
         if ( server4clientSendMemos( client, data ) < 0 )
            return -1 ;
      return saveRc ;
   #endif /* S4OFF_INDEX */
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
/* AS Sep 7/11 c/s support for seek long long */
int server4clientSeekLongLong( SERVER4CLIENT *client )
{
   #ifdef S4OFF_INDEX
      return e4notSupported ;
   #else
      DATA4 *data ;
      #ifdef E4ANALYZE
         int rc ;
      #endif
      int saveRc ;
      CONNECTION4SEEK_INFO_IN *info ;
      CONNECTION4GO_INFO_OUT *out ;
      TAG4 *tag ;
      const char *key ;
      short keyLen ;
      S4LONG startPos ;

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( connection4len( &client->connection ) < sizeof( info ) )
         return e4packetLen ;

      info = (CONNECTION4SEEK_INFO_IN *)connection4data( &client->connection ) ;
      keyLen = ntohs5(info->keyLen ) ;
      if ( connection4len( &client->connection ) != (long)sizeof( CONNECTION4SEEK_INFO_IN ) + keyLen + 1 )
         return e4packetLen ;

      key = connection4data( &client->connection ) + sizeof( CONNECTION4SEEK_INFO_IN ) ;

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;

      if ( data == 0 )
         return e4name ;

      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;

      if ( keyLen != sizeof( double ) )
         return e4seek ;
      assert5port( "Added IncludeMemos member to this communications message" );
      short includeMemos = info->includeMemos ;
      startPos = ntohl5(info->startPos) ;
      if ( info->fromCurrentPos && startPos > 0L )
      {
         saveRc = D4goRead( data, startPos ) ;  /* don't setOldRecord since server only does reading at this point */
         if ( saveRc == 0 )
            saveRc = D4seekLongLong( data, tag, (*((LONGLONG *)key)), 1 ) ;
      }
      else
         saveRc = D4seekLongLong( data, tag, (*((LONGLONG *)key)), 0 ) ;

      if ( saveRc < 0 )
         return saveRc ;
      connection4clear( &client->connection ) ;
      connection4addData( &client->connection, NULL, sizeof( CONNECTION4GO_INFO_OUT ), (void **)&out ) ;
      out->recNo = htonl5( d4recNo( data ) ) ;
      out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
      connection4addData( &client->connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;
      if ( includeMemos )
         if ( server4clientSendMemos( client, data ) < 0 )
            return -1 ;
      return saveRc ;
   #endif /* S4OFF_INDEX */
}



/* AS Apr 12/02 - New functionality for batch-writing */
int server4clientMsgWriteBatch( SERVER4CLIENT *client )
{
   assert5port( "New functionality for batch-writing" );
   // it is required that client has file locked in this case, so no lock/unlock handling is required.
   int rc = 0 ;
   CODE4 *c4 = client->server->c4 ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   CONNECT4 *connect = &client->connect ;
   long clientId = connect4receiveLong( connect ) ;
   long serverId = connect4receiveLong( connect ) ;
   short readLock = connect4receiveShort( connect ) ;
   short unlockAuto = connect4receiveShort( connect ) ;
   long count = connect4receiveLong( connect ) ;

   DATA4 *data = tran4data( &client->trans, serverId, clientId ) ;
   if ( data == 0 )   // can't recover client from bad data here
      return error4( c4, e4connect, E91530 ) ;

   // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with aes encryption
   long recWidth = dfile4recWidth( data->dataFile ) ;

   short finalRc = 0 ;
   short saveRc = 0 ;

   while ( count > 0 )
   {
      long recNo = connect4receiveLong( connect ) ;
      if ( finalRc != 0 )  // just read the data in the buffer, but don't process it
      {
         error4set( c4, 0 ) ;
         // just use the data4->record as a buffer,
         connect4receive( connect, d4record( data ), recWidth, code4timeoutVal(c4)) ;
         count-- ;

         // we weren't retrieving the memo fields....
         for( int i = 0; i < data->dataFile->nFieldsMemo ; i++ )
         {
            F4MEMO *memoField = &data->fieldsMemo[i] ;
            memcpy( data->record + memoField->field->offset, data->recordBlank + memoField->field->offset, memoField->field->len ) ;
            long memoLen = connect4receiveLong( connect ) ;
            assert5( memoLen >= 0 ) ;
            f4memoSetLen( memoField->field, memoLen ) ;

            if ( memoLen > 0 )
               connect4receive( connect, memoField->contents, memoLen, code4timeoutVal(c4) ) ;
         }

         continue ;
      }

      if ( recNo == - 1 )  // means append a record
      {
         finalRc = d4appendStart( data, 0 ) ;

         connect4receive( connect, d4record( data ), recWidth, code4timeoutVal(c4)) ;

         if ( finalRc == 0 )
         {
            for( int i = 0; i < data->dataFile->nFieldsMemo ; i++ )
            {
               F4MEMO *memoField = &data->fieldsMemo[i] ;
               memcpy( data->record + memoField->field->offset, data->recordBlank + memoField->field->offset, memoField->field->len ) ;
               long memoLen = connect4receiveLong( connect ) ;
               assert5( memoLen >= 0 ) ;
               f4memoSetLen( memoField->field, memoLen ) ;

               if ( memoLen > 0 )
                  connect4receive( connect, memoField->contents, memoLen, code4timeoutVal(c4) ) ;
            }
         }

         if ( finalRc == 0 )
         {
            // AS Jul 17/03 - with batch writing the client already stores the record number
            // of the record locally based on the record count.  That means that we cannot
            // use an appendTag, if any, to change the record number here or we have a conflict
            // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
            #ifndef S4CLIPPER
               TAG4FILE *appendTag = data->dataFile->appendTag ;
               data->dataFile->appendTag = 0 ;
            #endif
            finalRc = D4append( data, clientId, serverId ) ;
            #ifndef S4CLIPPER
               data->dataFile->appendTag = appendTag ;
            #endif
         }
      }
      else
      {
         finalRc = D4goWrite( data, recNo ) ;  /* do set setOldRecord since we are to do a write */
         memcpy( data->recordOld, data->record, dfile4recWidth( data->dataFile ) ) ;
         connect4receive( connect, d4record( data ), recWidth, code4timeoutVal(c4)) ;

         if ( finalRc == 0 )  // reset the memo fields to point to the old record
         {
            for( int i = 0; i < data->dataFile->nFieldsMemo ; i++ )
            {
               F4MEMO *memoField = &data->fieldsMemo[i] ;
               memcpy(data->record + memoField->field->offset, data->recordOld + memoField->field->offset, memoField->field->len ) ;
               long memoLen = connect4receiveLong( connect ) ;
               if ( memoLen != -1 )  // -1 means unchanged
               {
                  f4memoSetLen( memoField->field, memoLen ) ;
                  if ( memoLen > 0 )
                     connect4receive( connect, memoField->contents, memoLen, code4timeoutVal(c4) ) ;
               }
            }
         }

         if ( finalRc == 0 )
            finalRc = D4write( data, recNo, 0, 1 ) ;
         data->recordChanged = 0 ;
      }

      if ( finalRc == r4uniqueContinue ) // means go on...
      {
         finalRc = 0 ;
         saveRc = r4uniqueContinue ;
      }


      count-- ;
   }

   if ( finalRc == 0 )  // get back r4uniqueContinue if appropriate
      finalRc = saveRc ;
   else
      error4set( c4, 0 ) ;

   connect4sendShort( connect, finalRc ) ;
   return connect4sendFlush( connect ) ;
}



// AS May 13/02 data compression and preprocess
int server4clientMsgCompress( SERVER4CLIENT *client )
{
   assert5port( "New functionality for communications compression" );
   CODE4 *c4 = client->server->c4 ;

   #ifdef E4ANALYZE
      int rc ;
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   CONNECT4 *connect = &client->connect ;
   // AS May 13/04 - support for configuring the file compression level on a client basis as well
   c4->currentClient->fileCompressLevel = connect4receiveShort( connect ) ;
   short comLevel = connect4receiveShort( connect ) ;
   short minLength = connect4receiveShort( connect ) ;

   // if not supported, just ignore the message
   #ifdef S4COMPRESS
      connect4lowCompressConfigure( connect->connectBuffer.connectLowPtr, comLevel, minLength ) ;
   #endif

   return 0 ;
}



/* AS Apr 12/02 - New functionality for advance-reading client/server (reading a batch) */
int server4clientMsgRelateSkipMulti( SERVER4CLIENT *client )
{
   assert5port( "New function for batch reading of relation records" );
   // for relate module we don't do any locking/unlocking, so just ignore...
   int rc = 0 ;
   CODE4 *c4 = client->server->c4 ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   CONNECT4 *connect = &client->connect ;

   long relationId = connect4receiveLong( connect ) ;
   short readLock = connect4receiveShort( connect ) ;
   short unlockAuto = connect4receiveShort( connect ) ;
   short startCode = connect4receiveShort( connect ) ;   // top/bottom/skip
   long numSkips = connect4receiveLong( connect ) ;
   long recordReturnCount = connect4receiveLong( connect ) ;  // max # of records to return
   short doMemos = connect4receiveShort( connect ) ;
   long startOffset = connect4receiveLong( connect ) ;  // # of skips to perform before starting to retrieve records (when not top/bottom)

   if ( error4code( c4 ) < 0 )
   {
      connect4sendShort( connect, e4codeBase ) ;
      connect4sendFlush( connect ) ;
      error4set( c4, 0 ) ;
      return 0 ;
   }

   RELATION4 *relationOn = client4getRelation( client, relationId ) ;
   if ( relationOn == 0 )
   {
      connect4sendShort( connect, e4relateRefer ) ;
      connect4sendFlush( connect ) ;
      // AS July 29/02 - Let's also log a server error for informational purposes - in theory this error
      // shouldn't really happen...
      error4( c4, e4relateRefer, E81516 ) ;
      error4set( c4, 0 ) ;
      return 0 ;
   }

   switch ( startCode )
   {
      case 0:  // means normal skipping (from current pos) but use startOffset to skip first
         if ( startOffset != 0 )
         {
            rc = relate4skip( &relationOn->relate, startOffset ) ;
            if ( rc < 0 )  // if error, stop now...
            {
               error4set( c4, 0 ) ;
               connect4sendShort( connect, rc ) ;
               connect4sendFlush( connect ) ;
               return 0 ;
            }
         }
         break ;
      case 1:  // means from relate4top
         rc = relate4top( &relationOn->relate ) ;
         connect4sendShort( connect, rc ) ;
         if ( rc >= 0 )
            server4clientRelatePackBatch( connect, relationOn, doMemos ) ;
         if ( rc != 0 )   // any time we have a non zero rc, we stop sending more records
         {
            error4set( c4, 0 ) ;
            connect4sendFlush( connect ) ;
            return 0 ;
         }
         recordReturnCount-- ;
         break ;
      case 2:  // means from d4bottom
         rc = relate4bottom( &relationOn->relate ) ;
         connect4sendShort( connect, rc ) ;
         if ( rc >= 0 )
            server4clientRelatePackBatch( connect, relationOn, doMemos ) ;
         if ( rc != 0 )   // any time we have a non zero rc, we stop sending more records
         {
            error4set( c4, 0 ) ;
            connect4sendFlush( connect ) ;
            return 0 ;
         }
         recordReturnCount-- ;
         numSkips = -1 ;
         break ;

   }

   while( recordReturnCount-- > 0 )
   {
      rc = relate4skip( &relationOn->relate, numSkips ) ;
      connect4sendShort( connect, rc ) ;
      if ( rc >= 0 )
         server4clientRelatePackBatch( connect, relationOn, doMemos ) ;
      if ( rc != 0 )   // any time we have a non zero rc, we stop sending more records
      {
         error4set( c4, 0 ) ;
         connect4sendFlush( connect ) ;
         return 0 ;
      }
      // AS Apr 30/07 - we need to adjust the numSkips after the 1st skip (if not 1/-1)
      if ( numSkips > 1 )
         numSkips = 1 ;
      else
          if ( numSkips < -1 )
            numSkips = -1 ;
      assert5( numSkips == 1 || numSkips == -1 ) ;
   }

   connect4sendFlush( connect ) ;
   return 0 ;
}



static void connect4sendMemos( CONNECT4 *connect, DATA4 *data )
{
   assert5port( "New function for batch reading of relation records" );
   int numMemos = data->dataFile->nFieldsMemo ;

   for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
   {
      FIELD4 *field = data->fieldsMemo[memoLoop].field ;
      long memoLen = f4memoLen( field ) ;
      connect4sendLong( connect, memoLen ) ;
      if ( memoLen != 0 )
         connect4send( connect, f4memoPtr( field ), memoLen ) ;
   }
}



/* AS Apr 10/02 - New functionality for advance-reading client/server (reading a batch) */
int server4clientMsgSkip( SERVER4CLIENT *client )
{
   assert5port( "New function for batch reading of relation records" );
   // we need to consider locking here.  In particular any unlocking and any locking.
   // for unlocking we only need to consider the first skip/top/etc.  If any unlock occurs it will
   // occur on that first movement.
   int rc = 0 ;
   CODE4 *c4 = client->server->c4 ;
   assert5( client->sendUnlock == 0 ) ;  // should only be set on within this function

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   CONNECT4 *connect = &client->connect ;

   short mode = connect4receiveShort( connect ) ;
   long clientId = connect4receiveLong( connect ) ;
   long serverId = connect4receiveLong( connect ) ;
   short readLock = connect4receiveShort( connect ) ;
   short unlockAuto = connect4receiveShort( connect ) ;
   c4setReadLock( c4, (char)readLock ) ;
   code4unlockAutoSet( c4, unlockAuto ) ;
   long startPos = connect4receiveLong( connect ) ;
   long numSkip = connect4receiveLong( connect ) ;
   long numRecsToRead = connect4receiveLong( connect ) ;
   long modus = connect4receiveLong( connect ) ;  // AS Jul 17/09 - new modus support
   short doMemos = connect4receiveShort( connect ) ;
   short usesTag = connect4receiveShort( connect ) ;
   char alias[LEN4TAG_ALIAS+1] ;
   char indexName[LEN4PATH+1] ;
   if ( usesTag == 1 )
   {
      connect4receive( connect, alias, LEN4TAG_ALIAS, code4timeoutVal(c4) ) ;
      alias[LEN4TAG_ALIAS] = 0 ;
      short indexNameLen = connect4receiveShort( connect ) ;

      if ( indexNameLen > LEN4PATH )
         return error4( c4, e4connect, E91530 ) ;

      connect4receive( connect, indexName, indexNameLen, code4timeoutVal(c4) ) ;
      indexName[indexNameLen] = 0 ;
   }

   char seekKey[I4MAX_KEY_SIZE] ;
   double seekDoubleKey ;
   long seekStartRecNo = 0L ;
   short seekKeyLen = 0 ;
   short seekType ;   // double or byte
   if ( mode == BATCH4SEEKNEXT || mode == BATCH4SEEKMATCH || mode == BATCH4SEEK ) // means d4seek / d4seekNext, fetch more data
   {
      seekType = connect4receiveShort( connect ) ;
      seekStartRecNo = connect4receiveLong( connect ) ;
      if ( mode != BATCH4SEEKNEXT )  // for pure seeks, we always start at the top...
         seekStartRecNo = 0 ;
      if ( seekStartRecNo < 0 )  // bad start, just cancel client
      {
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return error4( connect->cb, e4connect, E91530 ) ;
      }
      if ( seekType == r4double )
      {
         connect4receive( connect, &seekDoubleKey, sizeof( double), code4timeoutVal(c4) ) ;
         seekDoubleKey = ntohd( seekDoubleKey ) ;
         seekKeyLen = sizeof( double ) ;
      }
      else
      {
         seekKeyLen = connect4receiveShort( connect ) ;
         if ( seekKeyLen > I4MAX_KEY_SIZE || seekStartRecNo < 0 )  // bad size, just cancel client
         {
            connect4sendShort( connect, -1 ) ;
            connect4sendFlush( connect ) ;
            return error4( connect->cb, e4connect, E91530 ) ;
         }
         connect4receive( connect, seekKey, seekKeyLen, code4timeoutVal(c4) ) ;
      }
   }

   DATA4 *data = tran4data( &client->trans, serverId, clientId ) ;
   if ( data == 0 )
   {
      connect4sendShort( connect, e4name ) ;
      return connect4sendFlush( connect ) ;
   }

   TAG4 *tag = 0 ;
   short outRc = 0 ;
   if ( usesTag )
   {
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, indexName ) ;
      if ( i4 == 0 )
      {
         connect4sendShort( connect, e4name ) ;
         return connect4sendFlush( connect ) ;
      }
      tag = i4tag( i4, alias ) ;
      if ( tag == 0 )
      {
         connect4sendShort( connect, e4tagName ) ;
         return connect4sendFlush( connect ) ;
      }
   }

   // AS Aug 28/02 - Some customers do not want read-locking on for batch reading, but in general
   // it should be done
   #ifdef S4INTUIT
      if ( numRecsToRead == 1 )   // this is the normal non-batch type skip
         client->sendUnlock = 1 ;   //  we need to send unlock info to the client in this case
      else
         readLock = 0 ;  // we don't do locking for batch reading, so just set the readlock off.
   #else
      client->sendUnlock = 1 ;   //  we need to send unlock info to the client in this case
      // we also will need to turn off auto-unlock after the first fetch...
   #endif

   Bool5 doSeek = 0 ;  // true if we are to perform a seek prior to the skip

   switch( mode )
   {
      case BATCH4TOP:  // means d4top
         d4tagSelect( data, tag ) ;
         outRc = 0 ;  // no special code for the skipping...
         rc = D4top( data ) ;
         break ;
      case BATCH4BOTTOM:  // means d4bottom
         d4tagSelect( data, tag ) ;
         outRc = 0 ;  // no special code for the skipping...
         // numSkip = -1 ;  // skip backwards
         rc = D4bottom( data ) ;
         break ;
      case BATCH4SEEKNEXT:  // means d4seekNext
      case BATCH4SEEK:  // means d4seek, followed by retrieving without checking keys
      case BATCH4SEEKMATCH:  // means d4seek followed by retrieving records with matching keys
         // in this case we need to obtain the start position as well as the seek info
         doSeek = 1 ;
         if ( tag == 0 )  // must be a tag for seeking
         {
            connect4sendShort( connect, -1 ) ;
            connect4sendFlush( connect ) ;
            return error4( connect->cb, e4connect, E91530 ) ;
         }
         {
            unsigned short int fromCurrentPos = 1 ;
            if ( seekStartRecNo == 0 )  // start at bof
               fromCurrentPos = 0 ;
            else
               rc = D4goRead( data, seekStartRecNo ) ;
            if ( rc >= 0 )  // r4eof/r4bof are ok as well
            {
               // if the seek type is double and the key sent is sizeof(double),
               if ( seekType == r4double )
                  rc = D4seekDouble( data, tag, seekDoubleKey, fromCurrentPos ) ;
               else
                  rc = D4seekN( data, tag, seekKey, seekKeyLen, fromCurrentPos ) ;
            }
         }
         numSkip = 1 ;
         if ( mode == BATCH4SEEK )  // don't use the key for for matching after this point of being positioned
            seekKeyLen = 0 ;
         break ;
      default:
         rc = D4skip( data, tag, numSkip, &outRc, startPos ) ;
         break ;
   }

   // the outRc == rc from D4skip except that the rc returns the result of calling d4go
   // after the skip (means valid record, so rc's are r4locked, disk-read fail, etc.)
   // outRc includes r4bof, r4eof, etc. if rc == 0 it means the record was retrievable
   // if rc != 0 but outRc == 0 it means the skip was ok, but the d4go failed.
   if ( rc != 0 && outRc == 0 )
      outRc = rc ;

   client->sendUnlock = 0 ;   // reset the send-unlock
   connect4sendShort( connect, MSG5SKIP ) ;  // distinguish return as skip instead of 'unlock' which is sent internally
   connect4sendShort( connect, outRc ) ;

   if ( outRc != 0 )
   {
      if ( outRc != r4after || doSeek == 0 )
      {
         connect4sendFlush( connect ) ;
         return 0 ;
      }
      else // indicate not to send more rows since we have a non-zero rc
         numRecsToRead = 1 ;
   }

   connect4sendLong( connect, d4recNo( data ) ) ;
   short recordLocked = (d4lockTest( data, d4recNo( data ), lock4write ) == 1) ;
   #if !defined( S4INTUIT ) && defined( E4ANALYZE )
      // the record must be locked in this case (or return of r4locked, which would have broken above)
      assert5( recordLocked == 1 || readLock == 0 ) ;  // if readlock is on, must be locked, else optional (could have been locked elsewhere)
   #endif
   connect4sendShort( connect, recordLocked ) ;
   connect4send( connect, d4record( data ), dfile4recWidth( data->dataFile ) ) ;
   // and send the memo fields...
   if ( doMemos == 1 )
      connect4sendMemos( connect, data ) ;

   if ( numRecsToRead == 1 )   // this is the normal non-batch type skip
   {
      connect4sendFlush( connect ) ;
      return 0 ;
   }

   /* AS Apr 10/02 - New functionality for advance-reading client/server (reading a block) */
   long toSkip = 1 ;
   if ( numSkip < 0 )
      toSkip = -1 ;

   // AS Aug 28/02 - for readlock handling, must turn off auto-unlock for the duration
   int oldUnlockAuto = code4unlockAuto( c4 ) ;
   code4unlockAutoSet( c4, LOCK4OFF ) ;

   long keyMatch = 0 ;
   char keyBuf[I4MAX_KEY_SIZE] ;
   if ( modus < 0 )  // means only skip as long as the specified number of characters match
   {
      keyMatch = min( -modus, tfile4keyLen( tag->tagFile ) ) ;
      memcpy( keyBuf, tfile4key( tag->tagFile ), tfile4keyLen( tag->tagFile ) ) ;
   }

   Bool5 nextDone = 0 ;
   while ( (--numRecsToRead) > 0 )
   {
      // send additional rc/record pairs
      short rcGo, rcSkip, rcOut ;

      if ( seekKeyLen != 0 ) // means do seekNext not skip
      {
         rcGo = 0 ;
         if ( seekType == r4double )
            rcSkip = D4seekDouble( data, tag, seekDoubleKey, 1 ) ;
         else
            rcSkip = D4seekN( data, tag, seekKey, seekKeyLen, 1 ) ;
      }
      else
         rcGo = D4skip( data, tag, toSkip, &rcSkip, data->recNum ) ;

      // register rcSkip return code first, rcGo afterwards (skip gives eof/bof, go gives r4locked, etc.)
      if ( rcSkip != 0 )
         rcOut = rcSkip ;
      else
         rcOut = rcGo ;

      assert5( rcOut != r4done ) ;   // AS Jul 17/09 - we added this new return code, verify it isn't being returned under normal situations

      if ( nextDone == 1 )
         rcOut = r4done ;
      connect4sendShort( connect, rcOut ) ;
      // if doing seek, and return is r4entry, we position to 1 after, so send data down in that case
      if ( rcOut != 0 ) // if non-zero stop bufferring...
         break ;

      // now do new modus check
      if ( keyMatch > 0 )
      {
         if ( memcmp( tfile4key( tag->tagFile ), keyBuf, keyMatch ) != 0 )   // don't buffer any more
            nextDone = 1 ;
      }

      connect4sendLong( connect, d4recNo( data ) ) ;
      // AS Aug 28/02 - also send the recordLocked info now...
      short recordLocked = (d4lockTest( data, d4recNo( data ), lock4write ) == 1) ;
      #if !defined( S4INTUIT ) && defined( E4ANALYZE )
         // the record must be locked in this case (or return of r4locked, which would have broken above)
         assert5( recordLocked == 1 || readLock == 0 ) ;  // if readlock is on, must be locked, else optional (could have been locked elsewhere)
      #endif
      connect4sendShort( connect, recordLocked ) ;
      connect4send( connect, d4record( data ), dfile4recWidth( data->dataFile ) ) ;
      if ( doMemos == 1 )
         connect4sendMemos( connect, data ) ;

   }

   connect4sendFlush( connect ) ;
   code4unlockAutoSet( c4, oldUnlockAuto ) ;  // and reset the auto-unlock
   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientSkip( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   CONNECTION4SKIP_INFO_IN *info ;
   CONNECTION4GO_INFO_OUT *out ;
   TAG4 *tag ;
   short outRc ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   if ( connection4len( &client->connection ) != sizeof( CONNECTION4SKIP_INFO_IN ) )
      return e4packetLen ;

   info = ( CONNECTION4SKIP_INFO_IN *)connection4data( &client->connection ) ;

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   rc = 0 ;
   if ( info->usesTag )
   {
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;
      rc = D4skip( data, tag, ntohl5(info->numSkip), &outRc, ntohl5(info->startPos) ) ;
   }
   else
      rc = D4skip( data, 0, ntohl5(info->numSkip), &outRc, ntohl5(info->startPos) ) ;

   connection4clear( &client->connection ) ;
   connection4addData( &client->connection, NULL, sizeof( CONNECTION4GO_INFO_OUT ), (void **)&out ) ;

   out->skipRc = htons5(outRc) ;

   if ( rc == 0 )
   {
      out->recNo = htonl5(d4recNo( data )) ;
      out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
   }

   if ( out->skipRc == 0 && rc != r4locked )
      connection4addData( &client->connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;

   return rc ;
}



#ifdef S4CLIPPER
   /* S4CLIPPER, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientTagOpen( SERVER4CLIENT *client )
   {
      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E91530 ) ;
      #endif

      CONNECTION4 *connection = &client->connection ;

      DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
      if ( data == 0 )
         return e4name ;

      if ( connection4len( connection ) < sizeof( CONNECTION4OPEN_TAG_INFO_IN ) )
         return e4packetLen ;

      CONNECTION4OPEN_TAG_INFO_IN *in = (CONNECTION4OPEN_TAG_INFO_IN *)connection4data( connection ) ;

      CODE4 *c4 = client->server->c4 ;
      switch( (short)ntohs5(in->errDefaultUnique) )
      {
         case r4unique:
         case r4uniqueContinue:
         case e4unique:
            break ;
         default:
            return error4( c4, e4unique, E81718 ) ;
      }

      in->nameLen = ntohs5(in->nameLen) ;

      if ( connection4len( connection ) != ((long)sizeof( CONNECTION4OPEN_TAG_INFO_IN ) + (long)in->nameLen) )
         return e4packetLen ;

      int oldAccessMode = c4->accessMode ;
      int oldSingleClient = c4->singleClient ;
      int oldUniqueError = c4->errDefaultUnique ;

      c4->singleClient = ntohs5(in->accessMode) ;
      switch( c4->accessMode )
      {
         case OPEN4DENY_NONE:
            c4->accessMode = (char)c4->singleClient ;
            break ;
         case OPEN4DENY_WRITE:
            if ( in->accessMode == OPEN4DENY_RW )
               c4->accessMode = (char)c4->singleClient ;
            break ;
         default:
            break ;
      }
      c4->errDefaultUnique = (char)ntohs5(in->errDefaultUnique) ;
      c4->readOnlyRequest = in->readOnly ;

      INDEX4 *index = 0 ;
      if ( in->hasIndex )
         index = d4index( data, connection4data( connection ) + sizeof( CONNECTION4OPEN_TAG_INFO_IN ) ) ;

      // AS 10/06/99 -- failure if passed name includes the '.ntx' extension... remove it
      // before doing the lookup.
      char tagNameBuf[LEN4PATH] ;
      u4namePiece( tagNameBuf, LEN4PATH, in->tagName, 0, 1 ) ;
      c4upper( tagNameBuf ) ;
      u4nameRemoveGivenExtension( tagNameBuf, "NTX" ) ;
      int oldTagErr = c4->errTagName ;
      c4->errTagName = 0 ;
      TAG4 *tag = d4tag( data, tagNameBuf ) ;
      c4->errTagName = oldTagErr ;

      connection4clear( connection ) ;

      CONNECTION4OPEN_TAG_INFO_OUT *out ;
      connection4addData( connection, NULL, sizeof(CONNECTION4OPEN_TAG_INFO_OUT), (void **)&out ) ;

      if ( tag == 0 )
      {
         int oldFileFlush = c4->fileFlush ;
         c4->fileFlush = in->fileFlush ;
         tag = t4open( data, index, in->tagName ) ;
         c4->fileFlush = oldFileFlush ;
         out->autoOpened = 0 ;
      }
      else
         out->autoOpened = 1 ;

      if ( in->indexName[0] != 0 )
         u4ncpy( tag->index->accessName, in->indexName, sizeof( index->accessName ) ) ;

      c4->errDefaultUnique = oldUniqueError ;
      c4->singleClient = oldSingleClient ;
      c4->accessMode = oldAccessMode ;
      c4->readOnlyRequest = 0 ;

      if ( tag == 0 )
      {
         int rc = error4code( c4 ) ;
         if ( rc < 0 )
            return rc ;
         return e4open ;
      }
      else
      {
         connection4addData( connection, tfile4alias( tag->tagFile ), LEN4TAG_ALIAS, NULL ) ;
         short errUnique = htons5(tag->errUnique) ;
         connection4addData( connection, &errUnique, sizeof( short int ), NULL ) ;
      }

      return 0 ;
   }
#endif /* S4CLIPPER */



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientTagSync( SERVER4CLIENT *client )
{
   DATA4 *data ;
   long rec ;
   int rc ;
   CONNECTION4TAG_SYNCH_INFO_IN *info ;
   TAG4 *tag ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   if ( connection4len( &client->connection ) != sizeof( CONNECTION4TAG_SYNCH_INFO_IN ) )
      return e4packetLen ;

   info = ( CONNECTION4TAG_SYNCH_INFO_IN *)connection4data( &client->connection ) ;

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
   INDEX4 *i4 = d4index( data, info->indexFileName ) ;
   if ( i4 == 0 )
      return e4name ;
   tag = i4tag( i4, info->tagName ) ;
   if ( tag == 0 )
      return e4tagName ;
   info->recno = ntohl5(info->recno) ;
   if ( info->recno > data->dataFile->minCount )
   {
      if ( d4recCountLessEq( data, info->recno ) == 0 )
      {
         rc = d4goEof( data ) ;
         if ( rc == r4eof )
            rc = 0 ;
      }
      else
         rc = D4goRead( data, info->recno ) ;  /* don't setOldRecord since server only does reading at this point */
   }
   else
      rc = D4goRead( data, info->recno ) ;  /* don't setOldRecord since server only does reading at this point */

   if ( rc == 0 )
      rc = D4tagSync( data, tag ) ;

   rec = htonl5(d4recNo( data )) ;
   connection4clear( &client->connection ) ;
   connection4addData( &client->connection, &rec, sizeof( rec ), NULL ) ;

   return rc ;
}
#endif /* not S4OFF_COMMUNICATIONS */



/* S4SERVER */
int S4FUNCTION server4clientUnlock( SERVER4CLIENT *client )
{
   int rc = 0 ;
   #ifndef S4OFF_COMMUNICATIONS
      DATA4 *data ;
      unsigned short int mType ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   #ifndef S4OFF_COMMUNICATIONS
      if ( code4unlockAuto( client->server->c4 ) == 2 )  /* CB51 compat, unlock data file only */
      {
         data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
         if ( data == 0 )
            return e4name ;
         rc = D4unlock( data ) ;
      }
      else
   #endif /* not S4OFF_COMMUNICATIONS */
      {
         // AS Apr 15/10 - check the status of transactions here...if we are in an active transaction the client shouldn't have made this call...
         #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
            CODE4 * c4 = client->server->c4 ;
            if ( code4transEnabled( c4 ) && code4tranStatus( c4 ) == r4active )
            {
               #ifdef S4STAND_ALONE
                  // AS Feb 14/03 - Relax constraint for temporary tables
                  if ( data->dataFile->file.isReadOnly != 1 && data->dataFile->file.isTemporary == 0 )  // AS 04/10/00 allow to close if readOnly
               #endif
                  {
                     // AS Feb 5/03 - with ODBC, just continue the unlock since it is called on a close
                     if ( c4->odbc != 1 )
                        rc = error4( c4, e4transViolation, E91530 ) ;
                  }
            }
         #endif

         if ( rc == 0 )
            rc = Client4unlock( client ) ;
         #ifndef S4OFF_COMMUNICATIONS
            #ifdef S4JAVA
               if ( client->javaClient == 0 )
            #endif
            if ( client->isStream == 0 )  // stream clients (ole-db) don't expect unlock info to come back
            {  /* braces needed for above if statement */
               /* send a STREAM4UNLOCK_ALL message to the client to tell it to unlock stuff */
               mType = htons5(STREAM4UNLOCK_ALL) ;
               if ( client->connection.connect != NULL )   // probably the server catalog client
                  connection4send( &client->connection, &mType, sizeof( mType ) ) ;
            }
         #endif /* not S4OFF_COMMUNICATIONS */
      }

   return rc ;
}



#ifndef S4OFF_COMMUNICATIONS
// AS Sep 8/03 Don't include if OFF_COMMUNICATIONS is defined
/* S4SERVER */
// AS Apr 15/03 - support for unlock append by client
int server4clientUnlockAppend( SERVER4CLIENT *client )
{
   int rc ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   DATA4 *data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   // AS Apr 15/03 - support for new lockId for shared clone locking
   rc = d4unlockAppendInternal( data, connection4clientId( &client->connection ) ) ;

   connection4clear( &client->connection ) ;

   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientVersion( SERVER4CLIENT *client )
{
   CONNECTION4VERSION_INFO_OUT *out ;
   DATA4 *data ;
   CONNECTION4 *connection ;
   int rc ;

   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E91530 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   connection = &client->connection ;

   data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   long versionNumber = d4versionNumber( data ) ;
   if ( versionNumber < 0 )
      return (int)versionNumber ;
   connection4clear( connection ) ;
   rc = connection4addData( connection, NULL, sizeof( CONNECTION4VERSION_INFO_OUT ), (void **)&out ) ;
   if (rc >= 0 )
      out->versionNumber = htonl5(versionNumber) ;

   return rc ;
}



// AS Nov 7/03 - Modified d4flush behaviour in client/server to actually perform a physical file flush
/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientFlush( SERVER4CLIENT *client )
{
   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      #ifdef E4ANALYZE
         int rc ;
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      CONNECTION4 *connection = &client->connection ;
      DATA4 *data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
      if ( data == 0 )
         return e4name ;


      if ( connection4len( &client->connection ) != 0 )
         return e4packetLen ;

      connection4clear( connection ) ;

      // AS Dec 15/03 - actually this is ok, just return ok in this case
      if ( data->readOnly == 1 )
         return 0 ;   // error4( data->codeBase, e4info, E80606 ) ;
      else
         return d4flush( data ) ;  /* do set setOldRecord since we are to do a write */
   #endif
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientWrite( SERVER4CLIENT *client )
{
   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      DATA4 *data ;
      int rc, len ;
      CONNECTION4WRITE_INFO_IN *info ;
      CONNECTION4WRITE_INFO_OUT *out ;
      CONNECTION4 *connection ;
      const char *pos ;
      #ifndef S4MEMO_OFF
         int i ;
         short numMemoFields ;
         F4MEMO *memoField ;
         CONNECTION4MEMO *memo ;
      #endif

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      connection = &client->connection ;
      data = tran4data( &client->trans, connection4serverId( connection ), connection4clientId( connection ) ) ;
      if ( data == 0 )
         return e4name ;

      if ( data->record == data->recordOld )
         return error4describe( client->server->c4, e4append, E85901, d4alias( data ), "record and record_old unexpectedly equal", 0 ) ;

      #ifdef E4ANALYZE
         if ( data->readOnly == 1 )
            return error4( data->codeBase, e4info, E80606 ) ;
      #endif

      if ( (long)connection4len( connection ) < (long)sizeof( CONNECTION4WRITE_INFO_IN ) + (long)dfile4recWidth( data->dataFile ) )
         return e4packetLen ;

      pos = connection4data( connection ) ;
      info = (CONNECTION4WRITE_INFO_IN *)pos ;
      pos += sizeof(CONNECTION4WRITE_INFO_IN) ;
      info->recNo = ntohl5(info->recNo) ;
      if ( info->recNo > data->dataFile->minCount )
         if ( d4recCountLessEq( data, info->recNo ) == 0 )
            return error4( client->server->c4, e4append, E81509 ) ;

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_DATA_NAME
            if ( stricmp( d4alias(data), I4PRINT_DATA_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            sprintf( dump, "server4clientWrite for recNo %ld, oldRecord: %ld   record/old record:\r\n", (long)d4recNo(data), (long)data->recNumOld ) ;
            log5( dump );
            log5( data->record ) ;
            log5( "\r\n" );
            log5( data->recordOld ) ;
            log5( "\r\n" );
         }
      #endif

      rc = D4goWrite( data, info->recNo ) ;  /* do set setOldRecord since we are to do a write */

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_DATA_NAME
            if ( stricmp( d4alias(data), I4PRINT_DATA_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            sprintf( dump, "server4clientWrite D4goWrite done for recNo %ld, oldRecord: %ld   record/old record:\r\n", (long)d4recNo(data), (long)data->recNumOld ) ;
            log5( dump );
            log5( data->record ) ;
            log5( "\r\n" );
            log5( data->recordOld ) ;
            log5( "\r\n" );
            if ( data->record == data->recordOld )
               log5( "record and recordOld equal..." ) ;
         }
      #endif

      if ( rc == 0 )
      {
         /* AS 08/22/97 cannot just copy record accross because memo field markers may get lost...
             so instead, copy the record for a later restore of memo */
         memcpy( data->recordOld, data->record, dfile4recWidth( data->dataFile ) ) ;
         memcpy( d4record( data ), pos, dfile4recWidth( data->dataFile ) ) ;
         pos += dfile4recWidth( data->dataFile ) ;

         #ifdef I4PRINT_ADD
            #ifdef I4PRINT_DATA_NAME
               if ( stricmp( d4alias(data), I4PRINT_DATA_NAME ) == 0 )
            #endif
            {
               char dump[255] ;
               sprintf( dump, "server4clientWrite after old record copy for recNo %ld, oldRecord: %ld   record/old record:\r\n", (long)d4recNo(data), (long)data->recNumOld ) ;
               log5( dump );
               log5( data->record ) ;
               log5( "\r\n" );
               log5( data->recordOld ) ;
               log5( "\r\n" );
            }
         #endif

         #ifdef S4MEMO_OFF
            if ( info->numMemoFields )
               rc = e4notMemo ;
         #else
            numMemoFields = ntohs5(info->numMemoFields) ;
            len = sizeof( CONNECTION4WRITE_INFO_IN ) + (int)dfile4recWidth( data->dataFile ) ;
            len += numMemoFields * sizeof( CONNECTION4MEMO ) ;
            if ( connection4len( connection ) < len )
               return e4packetLen ;
            for(i=0; i< data->dataFile->nFieldsMemo; i++ )
            {
               memoField = &data->fieldsMemo[i] ;
               memcpy(data->record + memoField->field->offset, data->recordOld + memoField->field->offset, memoField->field->len ) ;
            }
            for ( i = 0 ; i < numMemoFields ; i++ )
            {
               memo = (CONNECTION4MEMO *)pos ;
               pos += sizeof( CONNECTION4MEMO ) ;
               memoField = &data->fieldsMemo[ntohs5(memo->fieldNum)] ;
               memo->memoLen = ntohl5(memo->memoLen) ;
               if ( memo->memoLen > 0 )
               {
                  len += memo->memoLen ;
                  rc = f4memoAssignN( memoField->field, pos, memo->memoLen ) ;
                  if ( connection4len( connection ) < len )
                     return e4packetLen ;
                  pos += memo->memoLen ;
               }
               else
                  rc = f4memoAssignN( memoField->field, pos, 0 ) ;
               if ( rc < 0 )
                  break ;
            }
         #endif

         #ifdef I4PRINT_ADD
            #ifdef I4PRINT_DATA_NAME
               if ( stricmp( d4alias(data), I4PRINT_DATA_NAME ) == 0 )
            #endif
            {
               char dump[255] ;
               sprintf( dump, "server4clientWrite prior to D4write for recNo %ld, oldRecord: %ld   record/old record:\r\n", (long)d4recNo(data), (long)data->recNumOld ) ;
               log5( dump );
               log5( data->record ) ;
               log5( "\r\n" );
               log5( data->recordOld ) ;
               log5( "\r\n" );
            }
         #endif

         rc = D4write( data, info->recNo, info->unlock, 1 ) ;
         #ifdef I4PRINT_ADD
            #ifdef I4PRINT_DATA_NAME
               if ( stricmp( d4alias(data), I4PRINT_DATA_NAME ) == 0 )
            #endif
            {
               char dump[255] ;
               sprintf( dump, "server4clientWrite after D4write for recNo %ld, oldRecord: %ld   record/old record:\r\n", (long)d4recNo(data), (long)data->recNumOld ) ;
               log5( dump );
               log5( data->record ) ;
               log5( "\r\n" );
               log5( data->recordOld ) ;
               log5( "\r\n" );
            }
         #endif

         /* AS 08/10/99 if d4write failed, was leaving the record
            marked as changed, which causes problems later...
         */
         data->recordChanged = 0 ;
      }

      if ( data->record == data->recordOld )
         return error4describe( client->server->c4, e4append, E85901, d4alias( data ), "record and record_old unexpectedly equal", 0 ) ;

      connection4clear( connection ) ;

      connection4addData( connection, NULL, sizeof( CONNECTION4WRITE_INFO_OUT ), (void **)&out ) ;
      out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
      return rc ;
   #endif /* S4OFF_WRITE */
}



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
   /* not S4OFF_WRITE, not S4OFF_TRAN, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientUnwrite( SERVER4CLIENT *client )
   {
      #ifdef E4ANALYZE
         int rc ;

         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      return tran4lowUnwrite( &client->trans ) ;
   }



   /* not S4OFF_WRITE, not S4OFF_TRAN, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientUnappend( SERVER4CLIENT *client )
   {
      #ifdef E4ANALYZE
         int rc ;

         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      return tran4lowUnappend( &client->trans ) ;
   }
#endif /* #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) */
#endif /* S4OFF_COMMUNICATIONS */



/* S4SERVER */
int server4clientTransactionStart( SERVER4CLIENT *client )
{
   #ifdef S4OFF_TRAN
      return e4notSupported ;
   #else
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         #ifdef E4ANALYZE
            int rca ;
            if ( ( rca = server4clientVerify( client, 1 ) ) < 0 )
               return rca ;
         #endif
         // AS Aug 13/01 - If the primary log file is disabled (due to not logging on primary), enable it now for the duration
         CODE4TRANS *c4trans = client->trans.c4trans ;
         SERVER4 *server = client->server ;

         // AS Mar 25/03 - The ODBC builds always have transactions enabled, due to otherwise sequencing problems
         #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS )
            // AS Apr 28/03 - made trans-shared a run-time switch
            if( server->c4->transShared == 1 )
            {
               assert5( c4trans->transFile->primary.isDisabled != log4disabled ) ;
               assert5( c4trans->transFile->primary.isDisabled != log4compressed ) ;

               // perform co-ordination here...
               // Sequence to start a transactiom
               //    - lock start/commit byte
               //    - verify the transaction file is valid
               //    - lock our id byte
               //    - increment numActiveTrans by 1
               //    - unlock start/commit byte
               code4transServerShareLock( &server->c4trans, TRAN4LOCK_START_COMMIT ) ;
               code4transServerVerify( &server->c4trans, 0 ) ;
               code4transServerShareLock( &server->c4trans, -1 ) ;
               #ifdef E4ANALYZE
                  CODE4 *c4 = server->c4 ;
                  long numLocks = 0 ;
                  if ( share4getData( c4trans->odbcTransShare, 0, &numLocks, sizeof( long ) ) != 0 )
                     return error4describe( c4, e4server, E70106, "Failed to get transaction count for odbc transactions", 0, 0 ) ;
               #endif
               code4transServerChangeActive( &server->c4trans, 1 ) ;
               #ifdef E4ANALYZE
                  long numLocks2 = 0 ;
                  if ( share4getData( c4trans->odbcTransShare, 0, &numLocks2, sizeof( long ) ) != 0 )
                     return error4describe( c4, e4server, E70106, "Failed to get transaction count for odbc transactions", 0, 0 ) ;
                  if ( numLocks2 != numLocks + 1 )
                     return error4describe( c4, e4server, E70106, "Failed to get transaction count for odbc transactions", 0, 0 ) ;
               #endif
               code4transServerShareUnlock( &server->c4trans, TRAN4LOCK_START_COMMIT ) ;
            }
            else  // AS May 20/03 - compile error fix
         #endif
            {
               int rc = 0 ;
               if ( c4trans->transFile->primary.isDisabled == log4disabled )
               {
                  CODE4 *c4 = server->c4 ;
                  c4trans->enabled = 0 ;  // mark so that it will get enabled
                  int oldDisable = c4->logDisable ;
                  c4->logDisable = log4enabled ;
                  rc = code4transFileEnable( c4trans, server->logName, 0 ) ;
                  if ( rc == r4noExist )
                     rc = code4transFileEnable( c4trans, server->logName, 1 ) ;
                  c4->logDisable = oldDisable ;
                  if ( rc != 0 )
                     return rc ;
                  // AS Oct 19/01 - If the backup is not enabled we actually want to do a full enabling of the
                  // primary logile at this point, it remains valid until the server shuts down.
                  if ( c4trans->transFile->backup.transFile != 0 )
                     c4trans->transFile->primary.isDisabled = log4tempEnabled ;
               }
               else if ( c4trans->transFile->primary.isDisabled == log4compressed )
               {
                  // currently in a compressed state (i.e. deleted)
                  // in this case, we need to enable it and update all of the log data...
                  CODE4 *c4 = server->c4 ;
                  c4trans->enabled = 0 ;  // mark so that it will get enabled
                  int oldDisable = c4->logDisable ;
                  c4->logDisable = log4enabled ;
                  rc = code4transFileEnable( c4trans, server->logName, 0 ) ;
                  if ( rc == r4noExist )
                     rc = code4transFileEnable( c4trans, server->logName, 1 ) ;
                  c4->logDisable = oldDisable ;
                  if ( rc != 0 )
                     return rc ;

                  // The log file must have all user-id and table information now inserted to bring
                  // it to a valid state
                  rc = code4transFilePopulate( c4trans ) ;
                  if ( rc != 0 )
                     return rc ;
                  c4trans->transFile->primary.isDisabled = log4tempUncompressed ;
               }
            }

         return tran4lowStart( &client->trans, client->id, 0 ) ;
      #endif /* S4OFF_WRITE */
   #endif /* S4OFF_TRAN */
}



/* S4SERVER */
int server4clientTransactionCommitPhaseOne( SERVER4CLIENT *client )
{
   #ifdef S4OFF_TRAN
      return 0 ;
   #else
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         #ifdef E4ANALYZE
            int rc ;

            if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
               return rc ;
         #endif
         #ifndef S4OFF_COMMUNICATIONS
            connection4clear( &client->connection ) ;
         #endif
         return tran4lowCommitPhaseOne( &client->trans, client->id, singlePhaseCommit ) ;
      #endif /* S4OFF_WRITE */
   #endif /* S4OFF_TRAN */
}



/* S4SERVER */
int server4clientTransactionCommitBothPhases( SERVER4CLIENT *client )
{
   #ifdef S4OFF_TRAN
      return 0 ;
   #else
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         int rc ;
         #ifdef E4ANALYZE
            if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
               return rc ;
         #endif
         #ifndef S4OFF_COMMUNICATIONS
            connection4clear( &client->connection ) ;
         #endif
         #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS )
            SERVER4 *server = client->server ;
            // AS Apr 28/03 - made trans-shared a run-time switch
            if( server->c4->transShared == 1 )
            {
               // Sequence to commit a transaction
               //    - lock start/commit byte
               //    - verify the transaction file is valid
               //    - perform the rollback or commit
               //    - unlock our id byte
               //    - decrement numActiveTrans by 1
               //    - unlock start/commit byte
               code4transServerShareLock( &server->c4trans, TRAN4LOCK_START_COMMIT ) ;
               code4transServerVerify( &server->c4trans, 1 ) ;
            }
         #endif
         rc = tran4lowCommitPhaseOne( &client->trans, client->id, dualPhaseCommit ) ;
         if ( rc == 0 )
            rc = server4clientTransPhaseTwoInternal( client ) ;
         #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS )
            // AS Apr 28/03 - made trans-shared a run-time switch
            if( server->c4->transShared == 1 )
            {
               code4transServerShareUnlock( &server->c4trans, -1 ) ;
               code4transServerChangeActive( &server->c4trans, -1 ) ;
               code4transServerShareUnlock( &server->c4trans, TRAN4LOCK_START_COMMIT ) ;
            }
         #endif
         return rc ;
      #endif /* S4OFF_WRITE */
   #endif /* S4OFF_TRAN */
}



#ifndef S4OFF_COMMUNICATIONS
#ifdef S4JAVA
   int Server4clientTranCommit(SERVER4CLIENT *client)
   {
      assert5port( "Java Transaction Processing added" );
      // CS 2002/04/30  For use by Java functionality
      #if defined(S4OFF_TRAN) || defined(S4OFF_WRITE)
         return 0 ;
      #else
         int rc ;
         rc = tran4lowCommitPhaseOne( &client->trans, client->id, dualPhaseCommit ) ;
         if ( rc != 0 )
            return rc ;
         return server4clientTransPhaseTwoInternal( client ) ;
      #endif // #if S4OFF_TRAN || S4OFF_WRITE
   }


   int Server4clientTranRollback(SERVER4CLIENT *client)
   {
      assert5port( "Java Transaction Processing added" );
      // CS 2002/05/01  For use by Java functionality
      #if defined(S4OFF_TRAN) || defined(S4OFF_WRITE)
         return 0 ;
      #else
         int rc ;

         rc = tran4lowRollback( &client->trans, client->id, 1 ) ;
         if ( rc != 0 )
            return rc ;
         // AS Mar 20/03 - not supported with ODBC (we only support the auto-delete of the log file on startup)
         #if !defined( S4OFF_TRAN ) && !defined( S4ODBC_ENABLED ) && !defined( S4ODBC_BUILD )
            server4clientLogDisableIfTemp( client );
         #endif
         return 0 ;
      #endif // #if S4OFF_WRITE || S4OFF_TRAN
   }


   int Server4clientTranStart(SERVER4CLIENT *client)
   {
      assert5port( "Java Transaction Processing added" );
      // CS 2002/05/01  For use by Java functionality
      #if defined(S4OFF_TRAN) || defined(S4OFF_WRITE)
         return 0 ;
      #else
         int rc ;
         // AS Aug 13/01 - If the primary log file is disabled (due to not logging on primary), enable it now for the duration
         CODE4TRANS *c4trans = client->trans.c4trans ;
         if ( c4trans->transFile->primary.isDisabled == log4disabled )
         {
            CODE4 *c4 = client->server->c4 ;
            c4trans->enabled = 0 ;  // mark so that it will get enabled
            int oldDisable = c4->logDisable ;
            c4->logDisable = log4enabled ;
            rc = code4transFileEnable( c4trans, client->server->logName, 0 ) ;
            if ( rc == r4noExist )
               rc = code4transFileEnable( c4trans, client->server->logName, 1 ) ;
            c4->logDisable = oldDisable ;
            if ( rc != 0 )
               return rc ;
            // AS Oct 19/01 - If the backup is not enabled we actually want to do a full enabling of the
            // primary logile at this point, it remains valid until the server shuts down.
            if ( c4trans->transFile->backup.transFile != 0 )
               c4trans->transFile->primary.isDisabled = log4tempEnabled ;
         }

         return tran4lowStart( &client->trans, client->id, 0 ) ;
      #endif // #if S4OFF_WRITE || S4OFF_TRAN
   }
#endif  // S4JAVA




/* not S4OFF_COMMUNICATIONS, S4SERVER */
static int server4clientRelateCreateSlaves( SERVER4CLIENT *client, CONNECTION4RELATE_INIT_INFO_IN *info, CONNECTION4RELATE *connection4relate, RELATE4 *relate, unsigned int *slaveOffset )
{
   int i, rc, len ;
   CONNECTION4RELATE *connectionRelateOn ;
   DATA4 *dataOn, *dataOld ;
   char *dataOnName, *tagOnName, *expr, *aliasName ;
   RELATE4 *relateOn ;
   TAG4 *tagOn ;
   CODE4 *c4 ;
   LIST4 *oldList ;
   #ifndef S4CLIPPER
      INDEX4FILE *i4file ;
   #endif
   short matchLen, numSlaves ;

   c4 = client->server->c4 ;

   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;
   matchLen = ntohs5(connection4relate->matchLen ) ;
   if ( matchLen > 0 )
   {
      len = expr4keyLen( relate->masterExpr ) ;
      if ( len != matchLen )
      {
         relate4matchLen( relate, matchLen ) ;
         if ( error4code( c4 ) < 0 )
            return error4code( c4 ) ;
      }
   }
   relate4type( relate, ntohs5(connection4relate->relationType) ) ;
   relate4errorAction( relate, ntohs5(connection4relate->errorAction) ) ;

   #ifdef E4ANALYZE
      if ( matchLen < 0 )
         return error4( c4, e4struct, E94413 ) ;
   #endif
   numSlaves = ntohs5(connection4relate->numSlaves) ;
   for ( i = 0 ; i < numSlaves ; i++ )
   {
      connectionRelateOn = (CONNECTION4RELATE *)( ((char *)info) + *slaveOffset) ;
      dataOnName = (char *)info + ntohs5(connectionRelateOn->dataAccessName.offset) ;
      if ( dataOnName == 0 )
         return error4( c4, e4connection, E84305 ) ;
      dataOld = tran4dataName( &client->trans, dataOnName, ntohl5(connectionRelateOn->clientId), 1 ) ;

      long clientId = ntohl5( connectionRelateOn->clientId ) ;
      DATA4 *slaveDataFromClient = tran4dataName( &client->trans, dataOnName, clientId, 1 ) ;
      if ( slaveDataFromClient == 0 )
         return error4( c4, e4connection, E84305 ) ;

      oldList = tran4dataList( code4trans( c4 ) ) ;
      tran4dataListSet( code4trans( c4 ), &relate->relation->localDataList ) ;
      if ( connectionRelateOn->dataAliasName.offset )
         aliasName = (char *)info + ntohs5(connectionRelateOn->dataAliasName.offset) ;
      else
         aliasName = 0 ;

      // we should not be logging these opens since data file contents cannot be changed
      // and we don't want to have to track these back for closing... (or with compressed
      // log file)
      #ifndef S4OFF_TRAN
         int oldStatus = code4tranStatus( c4 ) ;
         code4tranStatusSet( c4, r4off ) ;
      #endif
      dataOn = d4open( c4, dataOnName ) ;
      // AS Aug 9/02 - Need to have the capability to determine the clientId and serverId of the
      // source DATA4 for the server's relation DATA4 module.  In particular, we want to allow
      // for record counts to be accessible for clients who have a transaction in progress and
      // then use a relation to access those fields.
      dataOn->relationsSourceClientId = clientId ;
      dataOn->relationsSourceServerId = slaveDataFromClient->serverId ;
      #ifndef S4OFF_TRAN
         code4tranStatusSet( c4, oldStatus ) ;
      #endif

      if ( aliasName != NULL )
         d4aliasSet( dataOn, aliasName ) ;

      #ifndef S4CLIPPER
         /* 05/16/96 AS need to open indexes for all indexes not automatically
            opened from the d4open(), since relations can refer to them */
         if ( l4numNodes( &dataOn->indexes ) != l4numNodes( &dataOn->dataFile->indexes ) )
         {
            for( i4file = 0 ;; )
            {
               i4file = (INDEX4FILE *)l4next( &dataOn->dataFile->indexes, i4file ) ;
               if ( i4file == 0 )
                  break ;
               if ( d4index( dataOn, i4file->file.name ) == 0 )
                  i4open( dataOn, i4file->file.name ) ;
            }
         }
      #endif /* S4CLIPPER */

      tran4dataListSet( code4trans( c4 ), oldList ) ;
      if ( dataOn == 0 )
         return error4( c4, e4name, E81510 ) ;
      if ( !connectionRelateOn->dataTagName.offset )
         tagOn = 0 ;
      else
      {
         tagOnName = (char *)info + ntohs5(connectionRelateOn->dataTagName.offset) ;
         tagOn = d4tag( dataOn, tagOnName ) ;
         if ( tagOn == 0 )
            return error4( c4, e4name, E81511 ) ;
      }
      if ( !connectionRelateOn->masterExpr.offset )
         return error4( c4, e4name, E81512 ) ;
      else
         expr = (char *)info + ntohs5(connectionRelateOn->masterExpr.offset) ;
      relateOn = relate4createSlave( relate, dataOn, expr, tagOn ) ;
      if ( relateOn == 0 )
         return error4( c4, e4relate, E81513 ) ;
      relateOn->dataOld = dataOld ;
      relateOn->freeData = 1 ;
      d4tagSelect( relateOn->data, tagOn ) ;
      *slaveOffset += sizeof( CONNECTION4RELATE ) ;
      rc = server4clientRelateCreateSlaves( client, info, connectionRelateOn, relateOn, slaveOffset ) ;
      if ( rc < 0 )
         return rc ;
      #ifdef E4ANALYZE
         if ( relate->matchLen < 0 )
            return error4( c4, e4struct, E94413 ) ;
      #endif
   }

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateInit( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_INIT_INFO_IN *info ;
   CONNECTION4RELATE *master ;
   CONNECTION4RELATE_INIT_INFO_OUT out ;
   RELATE4 *masterRelate, *relate ;
   char *dataMasterName, *query, *sort, *aliasName ;
   CONNECTION4 *connection ;
   int rc ;
   unsigned int slaveOffset ;
   DATA4 *dataOn, *dataOld, *masterData ;
   char *dataOnName, *tagOnName ;
   CODE4 *c4 ;
   TAG4 *tagOn ;
   RELATION4 *relationOn ;
   short relateOffset, id ;

   c4 = client->server->c4 ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   connection = &client->connection ;
   if ( connection == 0 )
      return error4( c4, e4parm, E91530 ) ;

   if ( connection4len( &client->connection ) < sizeof( CONNECTION4RELATE_INIT_INFO_IN ) )
      return e4packetLen ;
   info = (CONNECTION4RELATE_INIT_INFO_IN *)connection4data( &client->connection ) ;

   if ( info->relationId )
   {
      relationOn = client4getRelation( client, ntohl5(info->relationId) ) ;
      if ( relationOn == 0 )
         return e4relateRefer ;
      l4remove( &client->relations, relationOn ) ;
      #ifdef S4REL_PRINT
         #ifdef S4TESTING
            d4displayStr( mem4displayPtr, "relate4free for relation: ", 1 ) ;
            long idVal = ntohl5(info->relationId) ;
            d4displayNum( mem4displayPtr, idVal, 0 ) ;
         #endif
      #endif

      rc = relate4free( &relationOn->relate, 1 ) ;
      if ( rc < 0 )
         return rc ;
   }
   relateOffset = ntohs5(info->relateOffset) ;
   master = (CONNECTION4RELATE *)((char *)connection4data( connection ) + relateOffset ) ;

   dataMasterName = (char *)info + ntohs5(master->dataAccessName.offset) ;

   masterData = tran4dataName( &client->trans, dataMasterName, ntohl5(info->masterClientId), 1 ) ;

   if ( masterData == 0 )
      return error4( c4, e4name, E81510 ) ;

   if ( master->dataAliasName.offset )
      aliasName = (char *)info + ntohs5(master->dataAliasName.offset) ;
   else
      aliasName = 0 ;

   #ifdef S4REL_PRINT
      #ifdef S4TESTING
         d4displayStr( mem4displayPtr, "relate4init for master: ", 1 ) ;
         d4displayStr( mem4displayPtr, d4alias( masterData ), 0 ) ;
      #endif
   #endif

   /* AS 09/28/99 --> only need in read-only mode, if we open in read/write but file actually
      only read-only, we get a failure... */
   int oldReadOnlyRequest = c4->readOnlyRequest ;
   c4->readOnlyRequest = 1 ;
   masterRelate = relate4init( masterData, aliasName ) ;
   c4->readOnlyRequest = oldReadOnlyRequest ;

   if ( masterRelate == 0 )
      return error4( c4, e4relate, E81515 ) ;

   c4->currentRelation = masterRelate->relation ;
   l4add( &client->relations, masterRelate->relation ) ;
   masterRelate->relation->bitmapDisable = ntohs5(info->bitmapDisable) ;
   client->relationIdCount++ ;
   masterRelate->relation->relationId = client->relationIdCount ;
   #ifdef S4REL_PRINT
      #ifdef S4TESTING
         d4displayStr( mem4displayPtr, "assigned output relationId: ", 0 ) ;
         d4displayNum( mem4displayPtr, masterRelate->relation->relationId, 0 ) ;
      #endif
   #endif
   out.relationId = htonl5(client->relationIdCount) ;

   if ( info->relation.skipBackwards  )
      relate4skipEnable( masterRelate, 1 ) ;

   // AS May 1/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
   assert5port( "New member of the communications message for this function" );
   relate4retain( masterRelate, htons5( info->retainRelation ) ) ;

   if ( info->relation.exprSource.offset )
   {
      query = (char *)info + ntohs5(info->relation.exprSource.offset) ;
      relate4querySet( masterRelate, query ) ;
   }

   if ( info->relation.sortSource.offset )
   {
      sort = (char *)info + ntohs5(info->relation.sortSource.offset) ;
      relate4sortSet( masterRelate, sort ) ;
   }

   dataOnName = (char *)info + ntohs5(master->dataAccessName.offset) ;
   if ( dataOnName == 0 )
   {
      l4remove( &client->relations, masterRelate->relation ) ;
      relate4free( masterRelate, 1 ) ;
      return error4( c4, e4connection, E84305 ) ;
   }

   dataOld = tran4dataName( &client->trans, dataOnName, ntohl5(master->clientId), 1 ) ;

   if ( dataOld == 0 )
   {
      l4remove( &client->relations, masterRelate->relation ) ;
      relate4free( masterRelate, 1 ) ;
      return error4( c4, e4name, E81510 ) ;
   }

   dataOn = masterRelate->data ;

   if ( dataOn == 0 )
   {
      l4remove( &client->relations, masterRelate->relation ) ;
      relate4free( masterRelate, 1 ) ;
      return error4( c4, e4name, E81510 ) ;
   }

   if ( !master->dataTagName.offset )
      tagOn = 0 ;
   else
   {
      tagOnName = (char *)info + ntohs5(master->dataTagName.offset) ;
      tagOn = d4tag( dataOn, tagOnName ) ;
      if ( tagOn == 0 )
      {
         l4remove( &client->relations, masterRelate->relation ) ;
         relate4free( masterRelate, 1 ) ;
         return error4( c4, e4name, E81511 ) ;
      }
   }

   if ( master->masterExpr.offset )  // this would mean the top relation had a master - impossible, client must have called with a slave
   {
      l4remove( &client->relations, masterRelate->relation ) ;
      relate4free( masterRelate, 1 ) ;
      return error4( c4, e4name, E81512 ) ;
   }

   masterRelate->dataOld = dataOld ;
   masterRelate->freeData = 1 ;
   d4tagSelect( masterRelate->data, tagOn ) ;

   slaveOffset = relateOffset + sizeof( CONNECTION4RELATE ) ;
   /* AS 09/28/99 --> only need in read-only mode, if we open in read/write but file actually
      only read-only, we get a failure... */
   oldReadOnlyRequest = c4->readOnlyRequest ;
   c4->readOnlyRequest = 1 ;
   rc = server4clientRelateCreateSlaves( client, info, master, masterRelate, &slaveOffset ) ;
   c4->readOnlyRequest = oldReadOnlyRequest ;

   if ( rc != 0 )
   {
      l4remove( &client->relations, masterRelate->relation ) ;
      relate4free( masterRelate, 1 ) ;
      return rc ;
   }

   connection4clear( &client->connection ) ;
   connection4addData( &client->connection, &out, sizeof( CONNECTION4RELATE_INIT_INFO_OUT ), NULL ) ;

   for ( relate = masterRelate ;; )
   {
      if ( relate == 0 )
         break ;

      relate->relation->relateIdCount++ ;
      relate->id = relate->relation->relateIdCount ;
      id = htons5(relate->id) ;
      connection4addData( &client->connection, &id, sizeof( id ), NULL ) ;

      if ( relate4next( &relate ) == 2 )
         break ;
   }

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
static RELATION4 *client4getRelation( SERVER4CLIENT *client, const unsigned long id )
{
   RELATION4 *relationOn ;

   for( relationOn = 0;; )
   {
      relationOn = (RELATION4 *)l4next( &client->relations, relationOn ) ;
      if ( relationOn == 0 )
         return 0 ;
      if ( id == relationOn->relationId )
         break ;
   }

   client->server->c4->currentRelation = relationOn ;
   return relationOn ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
static int server4clientRelatePack( SERVER4CLIENT *client, RELATION4 *relation, short includeMemos )
{
   /* package the output data for relate4go, seek, skip, bottom */

   CONNECTION4RELATION_DATA_OUT *out ;
   RELATE4 *relate ;
   long count, recNum ;
   CONNECTION4 *connection = &client->connection ;

   connection4addData( connection, NULL, sizeof( CONNECTION4RELATION_DATA_OUT ), (void **)&out ) ;
   out->relationId = htonl5(relation->relationId) ;

   for( relate = &relation->relate ;; )
   {
      DATA4 *data = relate->data ;
      // AS May 1/02 was not indicating bof properly...
      if ( d4bof( data ) == 1 )
         recNum = 0 ;
      else
         recNum = htonl5(data->recNum) ;
      connection4addData( connection, &recNum, sizeof( recNum ), NULL ) ;
      count = htonl5( d4recCount( data ) ) ;
      connection4addData( connection, &count, sizeof( count ), NULL ) ;
      connection4addData( connection, data->record, dfile4recWidth( data->dataFile ), NULL ) ;
      if ( includeMemos == 1 )
         server4clientSendMemos( client, data ) ;
      if ( relate4next( &relate ) == 2 )
         break ;
   }

   return 0 ;
}



/* AS Apr 12/02 - New functionality for advance-reading client/server (reading a batch) */
/* not S4OFF_COMMUNICATIONS, S4SERVER */
static int server4clientRelatePackBatch( CONNECT4 *connect, RELATION4 *relation, short doMemos )
{
   assert5port( "New function for batch reading of relation records" );
   /* package the output data for relate4go, seek, skip, bottom */

   for( RELATE4 *relate = &relation->relate ;; )
   {
      // AS Apr 25/07 - if the data is at the bof position, we were incorrectly returning that we were at the top position
      if ( relate->data->bofFlag == 1 )
         connect4sendLong( connect, 0L ) ;
      else
         connect4sendLong( connect, relate->data->recNum ) ;
      connect4sendLong( connect, d4recCount( relate->data ) ) ;   // used to determine if we are in eof state
      connect4send( connect, relate->data->record, dfile4recWidth( relate->data->dataFile ) ) ;
      if ( doMemos )
      {
         int numMemos = relate->data->dataFile->nFieldsMemo ;
         for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
         {
            FIELD4 *field = relate->data->fieldsMemo[memoLoop].field ;
            long memoLen = f4memoLen( field ) ;
            connect4sendLong( connect, memoLen ) ;
            if ( memoLen != 0 )
               connect4send( connect, f4memoPtr( field ), memoLen ) ;
         }
      }
      if ( relate4next( &relate ) == 2 )
         break ;
   }

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateTop( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_TOP_INFO_IN *info ;
   CONNECTION4 *connection ;
   RELATION4 *relationOn ;
   int rc, rc2 ;

   connection = &client->connection ;

   info = (CONNECTION4RELATE_TOP_INFO_IN *)connection4data( connection ) ;
   if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_TOP_INFO_IN ) )
      return e4packetLen ;
   short includeMemos = info->includeMemos ;

   relationOn = client4getRelation( client, ntohl5( info->relationId ) ) ;
   if ( relationOn == 0 )
      return e4relateRefer ;

   // AS 06/15/00 - added general collating relation tag support in...
   #ifdef S4FOX
      client->server->c4->useGeneralTagsInRelate = ntohs5( info->useGeneralTagsInRelate ) ;
   #endif

   connection4clear( connection ) ;
   rc = relate4top( &relationOn->relate ) ;
   if ( rc >= 0 )
   {
      rc2 = server4clientRelatePack( client, relationOn, includeMemos ) ;
      if ( rc2 != 0 && rc >= 0 )
         rc = rc2 ;
   }
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateBottom( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_BOTTOM_INFO_IN *info ;
   CONNECTION4 *connection ;
   RELATION4 *relationOn ;
   int rc, rc2 ;

   connection = &client->connection ;

   info = (CONNECTION4RELATE_BOTTOM_INFO_IN *)connection4data( connection ) ;
   if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_BOTTOM_INFO_IN ) )
      return e4packetLen ;
   short includeMemos = info->includeMemos ;

   relationOn = client4getRelation( client, ntohl5(info->relationId) ) ;
   if ( relationOn == 0 )
      return e4relateRefer ;

   connection4clear( connection ) ;
   rc = relate4bottom( &relationOn->relate ) ;
   if ( rc >= 0 )
   {
      rc2 = server4clientRelatePack( client, relationOn, includeMemos ) ;
      if ( rc2 != 0 && rc >= 0 )
         rc = rc2 ;
   }
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateOpt( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_OPT_INFO_IN *info ;
   CONNECTION4 *connection ;
   RELATION4 *relationOn ;

   connection = &client->connection ;

   info = (CONNECTION4RELATE_OPT_INFO_IN *)connection4data( connection ) ;
   if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_OPT_INFO_IN ) )
      return e4packetLen ;

   relationOn = client4getRelation( client, ntohl5(info->relationId) ) ;
   if ( relationOn == 0 )
      return e4relateRefer ;

   // AS 06/15/00 - added general collating relation tag support in...
   #ifdef S4FOX
      client->server->c4->useGeneralTagsInRelate = ntohs5( info->useGeneralTagsInRelate ) ;
   #endif

   connection4clear( connection ) ;
   int rc = relate4optimizeable( &relationOn->relate ) ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateSkip( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_SKIP_INFO_IN *info ;
   CONNECTION4 *connection ;
   RELATION4 *relationOn ;
   S4LONG numSkips ;
   int rc, rc2 ;

   connection = &client->connection ;

   info = (CONNECTION4RELATE_SKIP_INFO_IN *)connection4data( connection ) ;
   if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_SKIP_INFO_IN ) )
      return e4packetLen ;

   relationOn = client4getRelation( client, ntohl5(info->relationId) ) ;
   if ( relationOn == 0 )
      return e4relateRefer ;
   short includeMemos = info->includeMemos ;

   if ( relationOn->isInitialized == 0 )
      return e4relateRefer ;

   numSkips = ntohl5(info->numSkips) ;
   connection4clear( connection ) ;
   rc = relate4skip( &relationOn->relate, numSkips ) ;
   if ( rc >= 0 )
   {
      rc2 = server4clientRelatePack( client, relationOn, includeMemos ) ;
      if ( rc2 != 0 && rc >= 0 )
         rc = rc2 ;
   }
   return rc ;
}



#ifdef S4OLD_RELATE_LOCK
   /* S4OLD_RELATE_LOCK, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientRelateLock( SERVER4CLIENT *client )
   {
      CONNECTION4RELATE_LOCK_INFO_IN *info ;
      CONNECTION4RELATE_LOCK_INFO_OUT *dataOut ;
      CONNECTION4 *connection ;
      RELATION4 *relation ;
      int rc ;
      CONNECTION4RELATE_LOCK_SUB_DATA *subData ;
      CODE4 *c4 ;
      RELATE4 *relateOn ;

      c4 = client->server->c4 ;
      connection = &client->connection ;

      info = (CONNECTION4RELATE_LOCK_INFO_IN *)connection4data( connection ) ;
      if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_LOCK_INFO_IN ) )
         return e4packetLen ;

      relation = client4getRelation( client, ntohl5(info->relationId) ) ;
      if ( relation == 0 )
         return e4relateRefer ;

      connection4clear( connection ) ;
      rc = relate4lock( &relation->relate ) ;
      if ( rc != 0 )
         return rc ;

      /* make a list of locks performed to send back to client */
      connection4addData( &client->connection, NULL, sizeof( CONNECTION4RELATE_LOCK_INFO_OUT ), &dataOut ) ;

      dataOut->count = 0 ;
      for( relateOn = &relation->relate ;; )
      {
         if ( relateOn == 0 )
            break ;

         if ( relateOn->dataOld->dataFile->fileClientLock == relateOn->dataOld->clientId &&
              relateOn->dataOld->dataFile->fileServerLock == relateOn->dataOld->serverId )
            dataOut->count++ ;
         if ( relate4next( &relateOn ) == 2 )
            break ;
      }
      dataOut->count = htons5(dataOut->count) ;
      for( relateOn = &relation->relate ;; )
      {
         if ( relateOn == 0 )
            break ;

         if ( relateOn->dataOld->dataFile->fileClientLock == relateOn->dataOld->clientId &&
              relateOn->dataOld->dataFile->fileServerLock == relateOn->dataOld->serverId )
         {
            connection4addData( &client->connection, NULL, sizeof( subData ), &subData ) ;
            subData.clientId = htonl5(data4clientId( relateOn->dataOld )) ;
            subData.serverId = htonl5(data4serverId( relateOn->dataOld )) ;
         }
         if ( relate4next( &relateOn ) == 2 )
            break ;
      }

      return 0 ;
   }
#endif /* S4OLD_RELATE_LOCK */



/* not S4OFF_COMMUNICATIONS, S4SERVER */
static RELATE4 *client4getRelate( RELATION4 *relation, unsigned short int relateId )
{
   RELATE4 *relateOn ;
   int rc ;

   for ( relateOn = &relation->relate ;; )
   {
      if ( relateOn == 0 )
         return 0 ;
      if ( relateOn->id == relateId )
         return relateOn ;
      rc = relate4next( &relateOn ) ;
      if ( rc != 1 )
         break ;
   }

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateDo( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_DO_INFO_IN *info ;
   CONNECTION4 *connection ;
   RELATE4 *relate ;
   RELATION4 *relationOn ;
   int rc, rc2 ;

   connection = &client->connection ;

   info = (CONNECTION4RELATE_DO_INFO_IN *)connection4data( connection ) ;
   if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_DO_INFO_IN ) )
      return e4packetLen ;

   relationOn = client4getRelation( client, ntohl5(info->relationId) ) ;
   if ( relationOn == 0 )
      return e4relateRefer ;
   short includeMemos = info->includeMemos ;

   relate = client4getRelate( relationOn, ntohs5(info->relateId) ) ;
   if ( relate == 0 )
      return error4( client->server->c4, e4connection, E81516 ) ;

   #ifdef E4ANALYZE
      if ( relate->master != 0 )
         return error4( relate->codeBase, e4parm, E84402 ) ;
   #endif

   D4goRead( relate->data, ntohl5(info->masterStartPos) ) ;  /* don't setOldRecord since server only does reading at this point */

   connection4clear( connection ) ;
   rc = relate4doAll( relate ) ;
   if ( rc >= 0 )
   {
      rc2 = server4clientRelatePack( client, relationOn, includeMemos ) ;
      if ( rc2 != 0 && rc >= 0 )
         rc = rc2 ;
   }
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateDoOne( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_DO_ONE_INFO_IN *info ;
   CONNECTION4 *connection ;
   RELATE4 *relate ;
   RELATION4 *relationOn ;
   long recNo ;
   int rc ;

   connection = &client->connection ;

   info = (CONNECTION4RELATE_DO_ONE_INFO_IN *)connection4data( connection ) ;
   if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_DO_ONE_INFO_IN ) )
      return e4packetLen ;

   relationOn = client4getRelation( client, ntohl5(info->relationId) ) ;
   if ( relationOn == 0 )
      return e4relateRefer ;

   relate = client4getRelate( relationOn, ntohs5(info->relateId) ) ;
   if ( relate == 0 )
      return error4( client->server->c4, e4connection, E81516 ) ;

   #ifdef E4ANALYZE
      if ( relate->master == 0 )
         return error4( relate->codeBase, e4parm, E84405 ) ;
   #endif

   rc = D4goRead( relate->master->data, ntohl5(info->masterStartPos) ) ;  /* don't setOldRecord since server only does reading at this point */
   if ( rc < 0 )
      return rc ;

   connection4clear( connection ) ;
   rc = relate4doOne( relate ) ;
   if ( rc == r4terminate )
      return r4terminate ;
   if ( rc >= 0 )
   {
      if ( d4eof( relate->data ) )
         return r4eof ;
      recNo = htonl5(d4recNo( relate->data )) ;
      connection4addData( &client->connection, &recNo, sizeof( recNo ), NULL ) ;
   }

   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientRelateFree( SERVER4CLIENT *client )
{
   CONNECTION4RELATE_FREE_INFO_IN *info ;
   CONNECTION4 *connection ;
   RELATION4 *relationOn ;

   connection = &client->connection ;

   info = (CONNECTION4RELATE_FREE_INFO_IN *)connection4data( connection ) ;
   if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_FREE_INFO_IN ) )
      return e4packetLen ;

   #ifdef S4REL_PRINT
      #ifdef S4TESTING
         d4displayStr( mem4displayPtr, "relate4free for id: ", 1 ) ;
         long id = ntohl5(info->relationId) ;
         d4displayNum( mem4displayPtr, id, 0 ) ;
      #endif
   #endif

   relationOn = client4getRelation( client, ntohl5(info->relationId) ) ;
   if ( relationOn == 0 )
      return e4relateRefer ;

   connection4clear( connection ) ;
   l4remove( &client->relations, relationOn ) ;
   return relate4free( &relationOn->relate, 1 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCheck( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   short int dataFileLocked ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   if ( client->server->doCheck == 0 )
      return e4notSupported ;

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   rc = D4check( data ) ;
   dataFileLocked = (short int)dfile4lockTestFile( data->dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 ;
   connection4clear( &client->connection ) ;
   connection4addData( &client->connection, &dataFileLocked, sizeof( short int ), NULL ) ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
void server4clientDirectory( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int oldKeepOpen ;
   CONNECT4 *connect = &client->connect ;
   CODE4 *c4 = client->server->c4 ;
   char fileName[LEN4PATH+6] ;
   char dirName[LEN4PATH] ;
   short len ;

   char *name = connect4receiveString( connect ) ;

   if ( name == NULL )  // no entry, means current directory
   {
      dirName[0] = '.' ;
      dirName[1] = 0 ;
      len = 1 ;
   }
   else
   {
      len = strlen( name ) ;

      if ( len > LEN4PATH )
      {
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      strncpy( dirName, name, LEN4PATH ) ;

      u4free( name ) ;
      name = 0 ;
   }

   if ( dirName[len-1] != S4DIR )
   {
      char dirTemp[2] = { S4DIR, 0 } ;
      strcat( dirName, dirTemp );
      len++ ;
   }

   strcpy( fileName, dirName ) ;
   strcat( fileName, "*." ) ;
   strcat( fileName, DBF4EXT ) ;  /* CS 2000/02/14 updates for UNIX */

   static FIELD4INFO flds[] =
   {
      { "FILE_NAME", r4str, LEN4PATH, 0 },
      { 0, 0, 0, 0 },
   } ;

   static TAG4INFO tags[] =
   {
      #ifdef S4CASE_SEN
         { "FILENAME", "LEFT( FILE_NAME, 120 )", 0, 0, 0, },
      #else
         { "FILENAME", "UPPER( LEFT( FILE_NAME, 120 ) )", 0, 0, 0, },  // CS 2000/05/08 Ignore case on Windows
      #endif
      { 0, 0, 0, 0, 0 }
   } ;

   // open the data file as a system table, avoids STREAM4UNLOCK sending
   // unlock back to original client
   c4->currentClient = c4->catalogClient ;

   #ifdef S4FOX
      Collate4name oldCollate = c4->collateName ;  // CS 2000/05/08
      c4->collateName = collate4generalCp1252 ;
   #endif
   short oldCP = c4->codePage ;
   c4->codePage = cp1252 ;
   c4->oledbSchemaCreate = 1 ;
   data = d4createTemp( c4, flds, tags ) ;
   c4->oledbSchemaCreate = 0 ;
   #ifdef S4FOX
      c4->collateName = oldCollate ;
   #endif
   c4->codePage = oldCP ;

   if ( data == 0 )
   {
      error4set( c4, 0 ) ;
      c4->currentClient = client ;

      connect4sendShort( connect, error4code( c4 ) < 0 ? error4code( c4 ) : -1) ;
      connect4sendFlush( connect ) ;
      return ;
   }

   c4->currentClient = client ;

   FIELD4 *fld = d4field( data, "FILE_NAME" ) ;
   if ( fld == 0 )
   {
      connect4sendShort( connect, error4code( c4 ) < 0 ? error4code( c4 ) : -1) ;
      connect4sendFlush( connect ) ;
      return ;
   }

   Find4file files( dirName, 0, DBF4EXT ) ;
   const char *tempName ;
   char fName[LEN4PATH] ;
   for (tempName = files.getNext();tempName;tempName = files.getNext())
   {
      d4appendStart( data, 0 ) ;
      u4namePiece( fName, sizeof( fName ), tempName, 0, 1 ) ;
      f4assign( fld, fName ) ;
      d4append( data ) ;
   }

   connect4sendShort( connect, 0 ) ;
   connect4sendString( connect, dfile4name( data->dataFile ) ) ;
   connect4sendFlush( connect ) ;

   oldKeepOpen = client->server->keepOpen ;
   client->server->keepOpen = 2 ;
   d4closeTemp( data ) ; // CJ-12/08/99-Does not log the close in the transaction file as the open does not.
   client->server->keepOpen = oldKeepOpen ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientCurrentDirectory( SERVER4CLIENT *client )
{
   CONNECT4 *connect = &client->connect ;
   char name[LEN4PATH + 1] ;
   short len ;

   u4nameCurrent( name, LEN4PATH, "" ) ;
   len = strlen( name ) ;
   if (name[len-1] != S4DIR)
   {
      name[len] = S4DIR ;  /* CS 2000/02/14 append trailing slash */
      len += 1 ;
   }

   connect4sendShort( connect, len ) ;
   connect4send( connect, name, len ) ;

   return connect4sendFlush( connect ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientConnectNew( SERVER4CLIENT *client )
{
   // AS 03/08/01 - This is the new conneciton method whereby the client creates both connections.
   // this works around firewall/internet problems which often wouldn't allow the connection back to
   // the originating client

   // what happens is that the client indicates with this call that it is going to set up a second
   // connection.  So, what we do is add ourselves to a list of connections that will be looking
   // for a second half...
   CONNECT4 *connect = &client->connect ;

   short portNo ;
//   short version ;
//   char *userName, *pass = 0 ;
   TCP4ADDRESS address;
   CONNECT4HALF *javaPtr ;
   CODE4 *c4 = client->server->c4 ;

   // AS Aug 27/01 - The connection might not be completely established yet, so we need to potentially
   // give up control of our exclusive access so that the accept thread can proceed.
   int startTime = code4timeoutVal(client->connect.cb) ;
   for( ;; )
   {
      // AS 08/21/00 - was not checking valid return code here before proceding.
      int rc = connect4receive(&client->connect, (char *)&portNo, sizeof( short ), 1 ) ;
      if ( rc == 0 )
         break ;
      if ( rc < 0 )
         return rc ;
      // else assume timeout
      if ( startTime >= 0 )
      {
         startTime-- ;
         if ( startTime == 0 )  // timed out
            return r4timeout ;
      }
      code4exitExclusive( c4, client ) ;
      u4delaySec() ;
      code4enterExclusive( c4, client, 1 ) ;
   }

   /* We'll get the address from the socket */
   CONNECT4BUFFER *connectBuffer = &(client->connect.connectBuffer) ;
   connect4lowEnterExclusive( connectBuffer ) ;
   address = connect4lowPeerTcpAddress( connect4bufferLowGet( connectBuffer ) ) ;
   connect4lowExitExclusive( connectBuffer ) ;

   /* March 3, 1998. Java now connects twice to the server */
   #ifndef S4OFF_THREAD
      // AS Dec 1/06 - this is in the incorrect order...on line 738 we get the client first, then the halflist mutex
      // therefore we can just skip releasing the client mutex here...
      // code4exitExclusive( c4, client ) ;
      list4mutexWait(&c4->connectHalfListMutex) ;
      // code4enterExclusive( c4, client, 1 ) ;
   #endif
   javaPtr = code4findFirstConnection(c4, address, portNo, 0 ) ;
   if (javaPtr)
   {
      connect4lowEnterExclusive( connectBuffer ) ;
      CONNECT4LOW *conLow = connect4bufferLowGet( connectBuffer ) ;
      conLow->serverConnectStatus[7] = '1' ;  // found javaptr in connectNew
      #ifndef S4OFF_LOG
         if ( c4->logConn > 8 )
         {
            strcat( client->connectInfoStatus, "existing client found in d4serv2.c server4clientConnectNew()" ) ;
            assert5( strlen( client->connectInfoStatus ) <= sizeof(client->connectInfoStatus) ) ;
         }
      #endif
      short returnValue = r4success ;
      conLow->sockw = javaPtr->sock ;
      // AS May 13/02 - communications compression coding - don't compress the connection message
      connect4lowWrite( conLow, (char *)&returnValue, 2, 0 ) ;
      connect4lowExitExclusive( connectBuffer ) ;
      mem4free( c4->connectHalfMemory, javaPtr ) ;
   }
   else
   {
      connect4lowEnterExclusive( connectBuffer ) ;
      connect4bufferLowGet( connectBuffer )->serverConnectStatus[7] = '2' ;  // not found javaptr in connectNew
      connect4lowExitExclusive( connectBuffer ) ;
      javaPtr = (CONNECT4HALF *)mem4allocZero( c4->connectHalfMemory ) ;
      if ( javaPtr == 0 )
      {
         // mem4free( c4->connectLowMemory, newSocket ) ;
         #ifndef S4OFF_THREAD
            list4mutexRelease( &c4->connectHalfListMutex ) ;
         #endif
         return error4describe( c4, e4memory, E96978, "Failure to java half", 0, 0 ) ;
      }
      // address and port are sufficient to uniquely identify the connection
      javaPtr->address = address ;
      javaPtr->port = portNo ;
      javaPtr->fromClient = 1 ;
      // javaPtr->sock = newSocket->sockr ;
      javaPtr->client = client ;
      client->connectHalf = javaPtr ;  // for cleanup and reference purposes

      #ifdef S4OFF_THREAD
         l4add( &c4->connectHalfList, javaPtr ) ;
      #else
         l4add( &c4->connectHalfListMutex.list, javaPtr ) ;
      #endif
      #ifndef S4OFF_LOG
         if ( c4->logConn > 8 )
         {
            strcat( client->connectInfoStatus, "first half java was not found in server4clientConnectNew(), it has been added now" ) ;
            assert5( strlen( client->connectInfoStatus ) <= sizeof(client->connectInfoStatus) ) ;
         }
      #endif
   }
   #ifndef S4OFF_THREAD
      list4mutexRelease(&c4->connectHalfListMutex) ;
   #endif

   if ( client->status != 0 )
      return client->status ;

   /* note that for java clients there is no way to change the date-format, leave at default... */
   //memcpy( client->trans.dateFormat, "MM/DD/YY", 9 ) ;

   return 0 ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientConnect( SERVER4CLIENT *client )
{
   CODE4 *c4 = client->server->c4 ;
   short addrLen, conn ;
   int rc ;
   C4ADDRESS address ;
   C4NETID netID ;
   unsigned short portNo ;
   CONNECT4 *connect = &client->connect ;

   #ifndef S4OFF_LOG
      if ( c4->logConn > 8 )
      {
         strcat( client->connectInfoStatus, " serverv4clientConnect() receiving info" ) ;
         assert5( strlen( client->connectInfoStatus ) <= sizeof(client->connectInfoStatus) ) ;
      }
   #endif

   connect4receive( connect, (char *)&conn, sizeof(short), code4timeoutVal( c4 ) ) ;
   connect4receive( connect, (char *)&addrLen, sizeof(short), code4timeoutVal( c4 ) ) ;

   addrLen = ntohs5( addrLen) ;
   connect4receive( connect, (char *)&portNo, addrLen, code4timeoutVal( c4 ) ) ;
   connect4receive( connect, (char *)&addrLen, sizeof(short), code4timeoutVal( c4 ) ) ;
   addrLen = ntohs5( addrLen) ;
   connect4receive( connect, (char *)&netID, addrLen, code4timeoutVal( c4 ) ) ;
   rc = address4makeNetID( &address, &addrLen, netID, portNo ) ;
   CONNECT4BUFFER *connectBuffer = &(client->connect.connectBuffer) ;
   if (!conn)
   {
      connect4lowEnterExclusive( connectBuffer ) ;
      rc = connect4lowConnect( connect4bufferLowGet( connectBuffer ), &address, addrLen, c4 ) ;
      connect4lowExitExclusive( connectBuffer ) ;
      if ( rc )
         return error4( c4, rc, E70232 ) ;
      (connectBuffer->connected)++ ;
   }
   #ifndef S4OFF_BLAST
      else /*conn != 0 */
      {
         CONNECT4LOW *connectLow = (CONNECT4LOW *)mem4allocZero( c4->connectLowMemory ) ;
         if ( connectLow == 0 )
            return error4( c4, e4memory, E96979 ) ;
         memset( newSocket->serverConnectStatus, '0', sizeof(newSocket->serverConnectStatus) ) ;
         rc = connect4lowConnect( connectLow, &address, addrLen ) ;
         if ( rc < 0 )
         {
            mem4free( c4->connectLowMemory, connectLow ) ;
            return error4( c4, rc, E70232 ) ;
         }
         connect4lowBlast( connectLow ) ;
         if ( !c4->connectBufferMemory )
            c4->connectBufferMemory = (MEM4 *)mem4create(c4, MEMORY4START_CONNECT_BUFFER, sizeof(CONNECT4BUFFER), MEMORY4EXPAND_CONNECT_BUFFER, 0 ) ;
         CONNECT4BUFFER *connectBuffer = (CONNECT4BUFFER *)mem4allocero( c4->connectBufferMemory ) ;
         if ( connectBuffer == NULL )
         {
            connect4lowClose( connectLow ) ;
            mem4free( c4->connectLowMemory, connectLow ) ;
            return error4( c4, e4memory, E96979 ) ;
         }
         if (conn > 0 )   /* Write only connection */
            rc = connect4bufferInit( connectBuffer, connect, connectLow, 0, 0, c4->writeMessageNumOutstanding ) ;
         else             /* Read only connection */
            rc = connect4bufferInit( connectBuffer, connect, connectLow, c4->readMessageNumBuffers, c4->readMessageBufferLen, 0 ) ;
         if ( rc < 0 )
         {
            mem4free( c4->connectBufferMemory, connectBuffer ) ;
            connect4lowClose( connectLow ) ;
            mem4free( c4->connectLowMemory, connectLow ) ;
            return error4( c4, rc, E70233 ) ;
         }
         connectBuffer->id = ( conn > 0 ) ? conn : -conn ;
         l4add( &connect->blastList, (LINK4 *)connectBuffer ) ;
      }
   #endif /* !S4OFF_BLAST */

   return r4success ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientPing( SERVER4CLIENT *client )
{
   S4LONG sendSize, recSize ;
   char *data ;
   int rc ;
   rc = connect4receive( &client->connect, (char *)&recSize, sizeof(S4LONG), -1 ) ;
   if (rc)
      return -1 ;
   rc = connect4receive( &client->connect, (char *)&sendSize, sizeof(S4LONG), -1 ) ;
   if (rc)
      return -1 ;
   recSize = ntohl5( recSize ) ;
   sendSize = ntohl5( sendSize ) ;
   if ( recSize > 10 )
   {
      data = (char *)u4alloc( recSize - 10 ) ;
      rc = connect4receive( &client->connect, data, recSize-10, -1 ) ;
      u4free( data ) ;
      if ( rc )
         return -1 ;
   }
   if ( sendSize > 0 )
   {
      data = (char *)u4alloc( sendSize ) ;
      rc = connect4send( &client->connect, data, sendSize ) ;
      rc |= connect4sendFlush( &client->connect ) ;
      u4free( data ) ;
      if ( rc )
         return -1 ;
    }
    return 0 ;
}



#ifndef S4OFF_BLAST
   /* not S4OFF_COMMUNICATIONS, S4SERVER, not S4OFF_BLAST */
   int server4clientBlastTestWrite( SERVER4CLIENT *client )
   {
      CONNECT4BUFFER *connectBuffer ;
      S4LONG numBytes, bufLen, left =  0 ;
      char *data ;
      short id ;
      CODE4 *c4 = client->server->c4 ;

      connect4receive(&client->connect, &numBytes, sizeof(S4LONG), code4timeoutVal( c4 )) ;
      numBytes = ntohl5(numBytes) ;
      data = (char *)u4alloc(bufLen = c4->readMessageBufferLen) ;
      connect4receive(&client->connect, &id, sizeof(short), code4timeoutVal( c4 ) ) ;
      id = ntohs5(id) ;
      connectBuffer = connect4bufferAuxConnectionSpecific(&client->connect, id );
      if (!connectBuffer)
         return e4connection ;
      left = numBytes ;
      while (left > 0 )
      {
         if (left > bufLen )
         {
            connect4bufferReceive( connectBuffer, data, bufLen, code4timeoutVal( c4 )) ;
            left -= bufLen ;
         }
         else
         {
            connect4bufferReceive( connectBuffer, data, left, code4timeoutVal( c4 )) ;
            left = 0 ;
         }
      }
      u4free(data) ;
      connect4bufferAuxConnectionPut(connectBuffer, &client->connect ) ;
      return r4success ;
   }



   /* not S4OFF_COMMUNICATIONS, S4SERVER, not S4OFF_BLAST */
   int server4clientBlastTestRead( SERVER4CLIENT *client )
   {
      CONNECT4BUFFER *connectBuffer ;
      S4LONG numBytes, left =  0 ;
      char *data ;
      short id ;
      CODE4 *c4 ;
      int rc ;

      c4 = client->server->c4 ;
      connect4receive(&client->connect, &numBytes, sizeof(S4LONG), code4timeoutVal( c4 )) ;
      numBytes = ntohl5(numBytes) ;
      data = (char *)u4alloc(numBytes) ;
      connect4receive(&client->connect, &id, sizeof(short), code4timeoutVal( c4 ) ) ;
      id = ntohs5(id) ;
      connectBuffer = connect4bufferAuxConnectionSpecific(&client->connect, id );
      if (!connectBuffer)
         return e4connection ;
      rc = connect4bufferSend(connectBuffer, data, numBytes ) ;
      if (rc < 0)
      {
         u4free(data) ;
         connect4bufferAuxConnectionPut(connectBuffer, &client->connect ) ;
         return rc ;
      }
      rc = connect4bufferSendFlush(connectBuffer) ;
      u4free(data) ;
      connect4bufferAuxConnectionPut(connectBuffer, &client->connect ) ;
      return rc ;
   }
#endif /* S4OFF_BLAST */



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientReindex( SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;
   short int dataFileLocked ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   dataFileLocked = (short int)dfile4lockTestFile( data->dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 ;
   rc = D4reindex( data ) ;
   if ( rc < 0 )
   {
      if ( dataFileLocked == 0 )
         if ( dfile4lockTestFile( data->dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 )
            dfile4unlockFile( data->dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ) ) ;
      return rc ;
   }
   dataFileLocked = (short int)dfile4lockTestFile( data->dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 ;
   connection4clear( &client->connection ) ;
   connection4addData( &client->connection, &dataFileLocked, sizeof( short int ), NULL ) ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientReindexIndex( SERVER4CLIENT *client )
{
   #ifndef S4OFF_WRITE
      DATA4 *data ;
      INDEX4 *index ;
      int rc ;
      short int dataFileLocked ;
   #endif

   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( client->server->doReindex == 0 )
         return e4notSupported ;

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
      if ( data == 0 )
         return e4name ;

      index = d4index( data, connection4data( &client->connection ) ) ;
      if ( index == 0 )
         return e4name ;
      rc = I4reindex( index ) ;
      dataFileLocked = (short int)dfile4lockTestFile( data->dataFile, connection4clientId( &client->connection ), connection4serverId( &client->connection ), lock4write ) == 1 ;
      connection4clear( &client->connection ) ;
      connection4addData( &client->connection, &dataFileLocked, sizeof( short int ), NULL ) ;
      return rc ;
   #endif
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientPosition( SERVER4CLIENT *client )
{
   DATA4 *data ;
   CONNECTION4DATA_POS_OUT *out ;
   CONNECTION4DATA_POS_IN *info ;
   double val ;
   TAG4 *tag ;
   int rc ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( &client->connection ) != sizeof( CONNECTION4DATA_POS_IN ) )
      return e4packetLen ;
   info = (CONNECTION4DATA_POS_IN *)connection4data( &client->connection ) ;
   if ( info->usesTag )
   {
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;
      d4tagSelect( data, tag ) ;
   }
   else
      d4tagSelect( data, 0 ) ;

   D4goRead( data, ntohl5(info->startRecno) ) ;  /* don't setOldRecord since server only does reading at this point */
   val = D4position( data ) ;
   if ( val < 0 )
      return (int) val ;
   connection4clear( &client->connection ) ;
   rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4DATA_POS_OUT ), (void **)&out ) ;
   out->position = htond(val) ;
   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientPositionSet( SERVER4CLIENT *client )
{
   DATA4 *data ;
   long rc, recNo ;
   CONNECTION4DATA_POS_SET_IN *info ;
   TAG4 *tag ;
   CONNECTION4GO_INFO_OUT *out ;

   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return (int)rc ;
   #endif

   data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
   if ( data == 0 )
      return e4name ;

   if ( connection4len( &client->connection ) != sizeof( CONNECTION4DATA_POS_SET_IN ) )
      return e4packetLen ;
   info = (CONNECTION4DATA_POS_SET_IN *)connection4data( &client->connection ) ;

   if ( info->usesTag )
   {
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      INDEX4 *i4 = d4index( data, info->indexFileName ) ;
      if ( i4 == 0 )
         return e4name ;
      tag = i4tag( i4, info->tagName ) ;
      if ( tag == 0 )
         return e4tagName ;
      d4tagSelect( data, tag ) ;
   }
   else
      d4tagSelect( data, 0 ) ;

   assert5port( "New member for this communications message" );
   // AS May 21/02 - optionally also send memos...
   short includeMemos = info->includeMemos ;
   rc = D4positionSet( data, ntohd(info->position) ) ;
   if ( rc < 0 )
      return (int)rc ;

   connection4clear( &client->connection ) ;

   connection4addData( &client->connection, NULL, sizeof( CONNECTION4GO_INFO_OUT ), (void **)&out ) ;
   recNo = d4recNo( data ) ;
   out->recNo = htonl5(recNo) ;
   out->recordLocked = d4lockTest( data, recNo, lock4write ) == 1 ;

   if ( rc == 0 )
      connection4addData( &client->connection, d4record( data ), dfile4recWidth( data->dataFile ), NULL ) ;

   // AS May 21/02 - optionally also send memos...
   if ( includeMemos )
      server4clientSendMemos( client, data ) ;

   return (int)rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientAppend( SERVER4CLIENT *client )
{
   #ifdef S4OFF_WRITE
      return e4notSupported ;
   #else
      DATA4 *data ;
      int rc ;  /*, appendLocked ; */
      CONNECTION4APPEND_INFO_IN *info ;
      CONNECTION4APPEND_INFO_OUT *out ;
      const char *pos ;
      #ifndef S4OFF_MEMO
         F4MEMO *memoField ;
         CONNECTION4MEMO *memo ;
         int i ;
      #endif

      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      data = tran4data( &client->trans, connection4serverId( &client->connection ), connection4clientId( &client->connection ) ) ;
      if ( data == 0 )
         return e4name ;

      if ( data->record == data->recordOld )
         return error4describe( client->server->c4, e4append, E85901, d4alias( data ), "record and record_old unexpectedly equal", 0 ) ;

      #ifdef E4ANALYZE
         if ( data->readOnly == 1 )
            return error4( data->codeBase, e4info, E80606 ) ;
         // AS Jan 26/06 - track transaction violation code...the status should be the same before and after append.
         int tranStatus = client->trans.currentTranStatus ;
      #endif

      if ( (long)connection4len( &client->connection ) < (long)sizeof( CONNECTION4APPEND_INFO_IN ) + (long)dfile4recWidth( data->dataFile ) )
         return e4packetLen ;

      pos = connection4data( &client->connection ) ;
      info = (CONNECTION4APPEND_INFO_IN *)pos ;
      pos += sizeof( CONNECTION4APPEND_INFO_IN ) ;

      rc = d4appendStart( data, 0 ) ;
      if ( rc == 0 )
      {
         memcpy( d4record( data ), pos, dfile4recWidth( data->dataFile ) ) ;
         pos += dfile4recWidth( data->dataFile ) ;
         info->numMemoFields = ntohs5(info->numMemoFields) ;
         #ifdef S4MEMO_OFF
            if ( info->numMemoFields != 0 )
               rc = e4notMemo ;
         #else
            for ( i = 0 ; i < info->numMemoFields ; i++ )
            {
               memo = (CONNECTION4MEMO *)pos ;
               pos += sizeof( CONNECTION4MEMO ) ;
               memo->fieldNum = ntohs5(memo->fieldNum) ;
               memo->memoLen = ntohl5(memo->memoLen) ;
               memoField = &data->fieldsMemo[memo->fieldNum] ;
               rc = f4memoAssignN( memoField->field, pos, memo->memoLen ) ;
               pos += memo->memoLen ;
               if ( rc < 0 )
                  break ;
            }
         #endif

         if ( rc == 0 )
            rc = D4append( data, connection4clientId( &client->connection ), data4serverId( data ) ) ;
      }

      if ( data->record == data->recordOld )
         return error4describe( client->server->c4, e4append, E85901, d4alias( data ), "record and record_old unexpectedly equal", 0 ) ;

      connection4clear( &client->connection ) ;

      connection4addData( &client->connection, NULL, sizeof( CONNECTION4APPEND_INFO_OUT ), (void **)&out ) ;
      out->recNum = htonl5(d4recNo( data )) ;
      out->bofFlag = data->bofFlag ;
      out->eofFlag = data->eofFlag ;
      out->recordChanged = data->recordChanged ;
      // AS Jul 3/03 - with recycling of rows it is possible the count does not match the recNum
      out->recCount = htonl5(d4recCount( data )) ;
      #ifdef S4FOX
         // AS 03/05/01 - server code for auto-increment field value propogation
         if ( data->autoIncrementField != 0 )
            out->autoIncrementVal = f4double( data->autoIncrementField ) ;
         // AS Sep 5/03 - client/server support for autoTimestamp
         if ( data->autoTimestampField != 0 )
         {
            assert5( sizeof( double ) == sizeof( out->autoTimestampVal ) && sizeof( double ) == f4len( data->autoTimestampField ) ) ;
            memcpy( &out->autoTimestampVal, f4ptr( data->autoTimestampField ), sizeof( double ) ) ;
         }
      #endif
      /* if whole file is locked, no new locks, so don't mark */
      if ( dfile4lockTestFile( data->dataFile, connection4clientId( &client->connection ), data4serverId( data ), lock4write ) != 1 )
      {
         out->recordLocked = d4lockTest( data, d4recNo( data ), lock4write ) == 1 ;
         out->appendLocked = d4lockTestAppend( data ) == 1 ;
      }
      else
      {
         out->recordLocked = 0 ;
         out->appendLocked = 0 ;
      }

      data->recordChanged = 0 ;

      // AS Jan 26/06 - track transaction violation code...the status should be the same before and after append.
      #ifdef E4ANALYZE
         assert5( tranStatus == client->trans.currentTranStatus ) ;
      #endif
      return rc ;
   #endif
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientConnectCloseFiles( SERVER4CLIENT *client )
{
   connection4clear( &client->connection ) ;
   return code4dataFileCloseAll( client->server->c4 ) ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientConnectCut( SERVER4CLIENT *client )
{
   if ( connection4len( &client->connection ) != 21 )  // 21 characters is size of field for connection id (plus null)
      return e4packetLen ;

   long connId ;
   int rc = 0 ;

   #ifdef S4ODBC_ENABLED
      // AS 12/06/99 -- id for sending changed to character string for ODBC large connection ids.
      // get the connectionId and make it into a LONGLONG...
      LONGLONG connIdLongLong = _atoi64( (char *)connection4data( &client->connection ) ) ;

      connection4clear( &client->connection ) ;

      // if the clientID > ULONG_MAX, then we know is an ODBC client, else is CodeBase client...
      if ( connIdLongLong > ULONG_MAX )
      {
         odbc4cutOneClient( connIdLongLong ) ;
      }
      else
      {
         assert5( connIdLongLong <= ULONG_MAX ) ;
         connId = (long)connIdLongLong ;   // if here, < ULONG_MAX, so can coerce it...
   #else
      { // required for else
         // non ODBC, assume id just not a LONGLONG
         connId = c4atol( connection4data( &client->connection ), 20 ) ;
         connection4clear( &client->connection ) ;
   #endif
         if ( connId == client->id )  // invalid to disconnect ones own self
            return e4name ;

         SERVER4 *server = client->server ;

         server4clientListReserve( server ) ;
         Bool5 listReserved = 1 ;

         SERVER4CLIENT *clientOn, *clientFound ;
         Bool5 firstTimeInList = 1 ;
         for ( rc = 0, clientOn = 0 ;; )
         {
            clientOn = server4clientGetNext( server, clientOn ) ;
            if ( clientOn == 0 )
            {
               if ( firstTimeInList == 0 )  // need to reset the resoruces as well
               {
                  code4exitExclusive( server->c4, clientFound ) ;  // server4clientInitUndo enters exclusive for the client being disconnected
                  server4clientListRelease( server ) ;
                  listReserved = 0 ;
                  code4enterExclusive( server->c4, client, 1 ) ;
               }
               rc = r4found ;
               break ;
            }
            if ( clientOn->id == connId )
            {
               // AS Jun 5/06 0 we need to reserve the client we are disconnecting and then get the client list again to avoid
               // potential deadlock.  There is also a risk that someone else is disconnecting this client at the same time, so we
               // need to watch for that possibility (i.e. we need to go through this list again to ensure the client is still there).

               if ( firstTimeInList == 1 )
               {
                  server4clientListRelease( server ) ;
                  code4exitExclusive( server->c4, client ) ;  // server4clientInitUndo enters exclusive for the client being disconnected
                  code4enterExclusive( server->c4, clientOn, 1 ) ;
                  server4clientListReserve( server ) ;
                  firstTimeInList = 0 ;
                  clientFound = clientOn ;
                  clientOn = 0 ;
                  continue ;
               }
               else
               {
                  if ( clientOn != clientFound )  // not the correct client...
                     continue ;

                  server4disconnect( server, clientOn ) ;
                  // AS May 30/06 - there was a deadlock contention problem here...in d4open, we got the connection but couldn't get
                  // the list...actually we don't need the list here, so release it first
                  code4exitExclusive( server->c4, clientOn ) ;  // server4clientInitUndo enters exclusive for the client being disconnected
                  server4clientListRelease( server ) ;
                  listReserved = 0 ;
                  code4enterExclusive( server->c4, client, 1 ) ;

                  #ifndef S4OFF_LOG
                     if ( server->c4->logConn > 6 )
                     {
                        char buf[250] ;
                        sprintf( buf, "IS0090:Server is disconnecting client due to server4clientConnectCut() call for userId: [%s] and ip adress: [%s] by userId: [%s] and ip adress: [%s]",
                                      clientOn->account.accountId, clientOn->account.tcpAddress,
                                      client->account.accountId, client->account.tcpAddress ) ;
                        assert5( strlen( buf ) < sizeof( buf ) ) ;
                        code4logInfo( server->c4, buf ) ;
                     }
                  #endif
               }
               break ;
            }
         }
         // AS May 30/06 - due to change...
         if ( listReserved )
            server4clientListRelease( server ) ;

      } // required for else

   return rc ;
}



// AS Jun 05/06 - add some additional debugging here
#if defined( E4MUTEX_CHECK ) && !defined( S4OFF_THREAD )
   int server4clientListReserve( SERVER4 *server )
   {
      // track which client is reserving this...
      int rc = list4mutexWait( &((server)->clients) ) ;
      if ( rc == 0 )
      {
         if ( server->c4->currentClient == 0 )
            server->clientListHolder = server->c4->catalogClient ;
         else
            server->clientListHolder = server->c4->currentClient ;
      }
      return rc ;
   }



   int server4clientListRelease( SERVER4 *server )
   {
      #ifdef E4MUTEX_CHECK
         // AS Mar 25/09 possibly can add another check here...
         // I think we need to have the c4->accessMutex here.  This is because we say that we can't get the c4->accessMutex if we have the list, so we should undo in the same order
         // it appears that the cancel/cut operations may violoate this but otherwise I think it should hold true...

         SERVER4CLIENT *client = 0 ;
         if ( c4accessMutexCountZero( server->c4 ) )
         {
            client = server->c4->currentClient ;
            if ( client != 0 )
            {
               // let's try to get exclusive access anyway since otherwise in an error condition the worker threads start to shut down...
               Bool5 enterClient = 0 ;
               if ( server->c4->catalogClient != 0 )
               {
                  code4enterExclusive( server->c4, server->c4->catalogClient ) ;
                  enterClient = 1 ;
               }
               error4( server->c4, e4parm, E70223 ) ;
               if ( enterClient == 1 )
                  code4exitExclusive( server->c4, server->c4->catalogClient ) ;
            }
         }
      #endif

      server->clientListHolder = 0 ;
      return list4mutexRelease( &((server)->clients) ) ;
   }
#endif



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientConnectCutAll( SERVER4CLIENT *client )
{
   SERVER4 *server = client->server ;

   server4clientListReserve( server ) ;
   SERVER4CLIENT *clientNext = server4clientGetFirst( server ) ;

   int rc ;
   SERVER4CLIENT *clientOn ;
   for ( rc = 0, clientOn = 0 ;; )
   {
      if ( clientNext == 0 )
         break ;
      clientOn = clientNext ;
      clientNext = server4clientGetNext( server, clientOn ) ;
      if ( clientOn != client )
      {
         code4exitExclusive( server->c4, client ) ;  // server4clientInitUndo enters exclusive for the client being disconnected
         #ifndef S4OFF_LOG
            if ( server->c4->logConn > 6 )
            {
               char buf[250] ;
               sprintf( buf, "IS0200:Server is disconnecting client due to server4clientConnectCutAll() call for userId: [%s] and ip adress: [%s] by userId: [%s] and ip adress: [%s]",
                             clientOn->account.accountId, clientOn->account.tcpAddress,
                             client->account.accountId, client->account.tcpAddress ) ;
               assert5( strlen( buf ) < sizeof( buf ) ) ;
               code4logInfo( server->c4, buf ) ;
            }
         #endif
         server4disconnect( server, clientOn ) ;
         // AS May 30/06 - there was a deadlock contention problem here...in d4open, we got the connection but couldn't get
         // the list...actually we don't need the list here, so release it first
         server4clientListRelease( server ) ;
         code4enterExclusive( server->c4, client, 1 ) ;
         server4clientListReserve( server ) ;
      }
   }

   server4clientListRelease( server ) ;

   #ifdef S4ODBC_ENABLED
      // and now cut all of the ODBC clients...
      odbc4cutAllClients() ;
   #endif

   connection4clear( &client->connection ) ;

   return rc ;
}



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientConnectAcceptNew( SERVER4CLIENT *client )
{
   if ( connection4len( &client->connection ) != sizeof( unsigned char ) )
      return e4packetLen ;

   unsigned char flag = *(unsigned char *)(connection4data( &client->connection ) ) ;

   int rc = 0 ;

   #ifdef S4ODBC_ENABLED
      assert5( sizeof( flag ) == SHARE4MEM_ACCEPT_LEN ) ;
      rc = share4putData( client->server->odbcSharedMemory, SHARE4MEM_ACCEPT_OFFSET, &flag, SHARE4MEM_ACCEPT_LEN ) ;
   #else
      client->server->connectAccept = flag ;
   #endif
   connection4clear( &client->connection ) ;

   return rc ;
}



static void server4clientMessageSendInitUndo( SERVER4CLIENT *client, MESSAGE4SEND *messageSent )
{
   // removes the MESSAGE4 from the client's send list and frees the memory associated with it
   l4remove( &client->messageSendList, messageSent ) ;
   mem4free( client->server->messageSendMemory, messageSent ) ;
}



// AS 08/22/00 new functions for inter-client communications
static void server4clientMessageRecvInitUndo( SERVER4CLIENT *client, MESSAGE4RECEIVE *message )
{
   // cancels the given message from all clients which may be waiting to receive it...

   SERVER4 *server = client->server ;

   u4free( message->data ) ;
   message->data = 0 ;
   // AS Aug 23/02 - the message may already be removed
   if ( message->link.n != 0 )
      l4remove( &message->pipe->messageList, message ) ;
   mem4free( server->messageRecvMemory, message ) ;
}



static PIPE4 *server4pipeGet( SERVER4 *server, unsigned long pipeId )
{
   // retrieves a pipe based on pipeid.  returns 0 if no active pipe found
   for ( PIPE4 *pipeOn = 0 ;; )
   {
      pipeOn = (PIPE4 *)l4next( &server->pipes, pipeOn ) ;
      if ( pipeOn == 0 )
         break ;
      if ( pipeOn->id == pipeId )
         return pipeOn ;
   }
   return 0 ;
}



int server4clientReceive( SERVER4CLIENT *client )
{
   CONNECT4 *connect = &client->connect ;
   SERVER4 *server = client->server ;

   unsigned long pipeId = connect4receiveLong( connect ) ;
   PIPE4 *pipe = server4pipeGet( server, pipeId ) ;

   // client wants to receive any messages available for it...
   if ( pipe == 0 )  // pipe invalid
   {
      connect4sendShort( connect, e4open ) ;
      connect4sendFlush( connect )  ;

      return 0 ;
   }

   MESSAGE4RECEIVE *message = (MESSAGE4RECEIVE *)l4remove( &pipe->messageList, l4first( &pipe->messageList ) ) ;
   if ( message == 0 )  // no message on this pipe
   {
      connect4sendShort( connect, r4timeout ) ;
      connect4sendFlush( connect )  ;

      return 0 ;
   }

   connect4sendShort( connect, 0) ;

   assert5( message != 0 ) ;

   // a message is available, so send it to client
   connect4sendLong( connect, message->len ) ;
   connect4send( connect, message->data, message->len ) ;
   connect4sendFlush( connect ) ;

   if ( message->messageSend != 0 )
      server4clientMessageSendInitUndo( message->messageSend->sender, message->messageSend ) ;
   server4clientMessageRecvInitUndo( client, message ) ;

   return 0 ;
}



static void server4clientMessageCancelAll( SERVER4CLIENT *client )
{
   // cancels all of clients outgoing and incoming messages ...

   server4pipeRemoveClientPipes( client->server, client ) ;

   for ( MESSAGE4SEND *messageSend = 0 ;; )
   {
      // cancels any outstanding to-send messages...
      messageSend = (MESSAGE4SEND *)l4pop( &client->messageSendList ) ;
      if ( messageSend == 0 )  // done
         break ;
      server4clientMessageRecvInitUndo( client, messageSend->message ) ;
      server4clientMessageSendInitUndo( client, messageSend ) ;
   }
}



int server4clientCloseRecv( SERVER4CLIENT *client )
{
   // shut down the socket, cancel any outstanding messages
   CONNECT4 *connect = &client->connect ;
   SERVER4 *server = client->server ;
   CODE4 *c4 = server->c4 ;

   unsigned long pipeId = connect4receiveLong( connect ) ;
   PIPE4 *pipe = server4pipeGet( server, pipeId ) ;
   if ( pipe != 0 )
      server4pipeInitUndo( server, pipe ) ;
   return 0 ;
}



int server4clientOpenRecv( SERVER4CLIENT *client )
{
   CONNECT4 *connect = &client->connect ;
   SERVER4 *server = client->server ;
   CODE4 *c4 = server->c4 ;

   PIPE4 *pipe = (PIPE4 *)mem4createAlloc( c4, &server->pipeMemory, 10, sizeof( PIPE4 ), 10, 0 ) ;
   if ( pipe == 0 )
      return error4describe( c4, e4memory, E96978, "Failure to allocate pipe", 0, 0 ) ;

   pipe->name = connect4receiveString( connect ) ;

   for ( PIPE4 *pipeOn = 0 ;; )
   {
      pipeOn = (PIPE4 *)l4next( &server->pipes, pipeOn ) ;
      if ( pipeOn == 0 )
         break ;
      if ( strcmp( pipeOn->name, pipe->name ) == 0 )  // duplicate name - disallow
      {
         u4free( pipe->name ) ;
         mem4free( server->pipeMemory, pipe ) ;
         connect4sendShort( connect, e4permiss ) ;
         connect4sendFlush( connect ) ;
         return 0 ;
      }
   }

   pipe->owner = client ;
   pipe->id = server->pipeIdCount++ ;
   pipe->messageIdCount = 0 ;
   l4add( &server->pipes, pipe ) ;

   connect4sendShort( connect, 0 ) ;
   connect4sendLong( connect, pipe->id ) ;
   connect4sendFlush( connect ) ;
   return 0 ;
}



int server4clientOpenSend( SERVER4CLIENT *client )
{
   CONNECT4 *connect = &client->connect ;
   SERVER4 *server = client->server ;
   CODE4 *c4 = server->c4 ;

   char *pipeName = connect4receiveString( connect ) ;
   PIPE4 *pipe = 0 ;

   // ensure pipe is open for receiving
   for ( PIPE4 *pipeOn = 0 ;; )
   {
      pipeOn = (PIPE4 *)l4next( &server->pipes, pipeOn ) ;
      if ( pipeOn == 0 )
         break ;
      if ( strcmp( pipeOn->name, pipeName ) == 0 )  // found
      {
         pipe = pipeOn ;
         break ;
      }
   }

   u4free( pipeName ) ;

   // ensure no other user is using the pipe...'
   if ( pipe == 0 ) // not found
     connect4sendShort( connect, e4fileFind ) ;
   else
   {
      connect4sendShort( connect, 0 ) ;
      connect4sendLong( connect, pipe->id ) ;
   }

   connect4sendFlush( connect ) ;
   return 0 ;
}



int server4clientSend( SERVER4CLIENT *client )
{
   CONNECT4 *connect = &client->connect ;
   SERVER4 *server = client->server ;
   CODE4 *c4 = server->c4 ;
   MESSAGE4RECEIVE *messageReceive = (MESSAGE4RECEIVE *)mem4createAllocNoZero( c4, &server->messageRecvMemory, 10, sizeof( MESSAGE4RECEIVE ), 10, 0 ) ;
   if ( messageReceive == 0 )
      return error4describe( c4, e4memory, E96978, "Failure to allocate message", 0, 0 ) ;

   unsigned long pipeId = connect4receiveLong( connect ) ;
   short waitForReceipt = connect4receiveShort( connect ) ;  // does the sender want a receipt when the message is sent?
   messageReceive->len = connect4receiveLong( connect ) ;
   messageReceive->data = u4alloc( messageReceive->len ) ;
   if ( messageReceive->data == 0 )
   {
      mem4free( server->messageRecvMemory, messageReceive ) ;
      char messageDesc[80] ;
      sprintf( messageDesc, "Failure to allocate message data for size: %ld", messageReceive->len ) ;
      return error4describe( c4, e4memory, E96978, messageDesc, 0, 0 ) ;
   }

   connect4receive( connect, messageReceive->data, messageReceive->len, code4timeoutVal( c4 ) ) ;

   PIPE4 *pipe = server4pipeGet( server, pipeId ) ;
   if ( pipe == 0 )   // receiver must have closed the pipe
   {
      connect4sendShort( connect, r4notConnected ) ;
      connect4sendFlush( connect ) ;
      u4free( messageReceive->data ) ;
      mem4free( server->messageRecvMemory, messageReceive ) ;
      return 0 ;
   }

   messageReceive->pipe = pipe ;
   l4add( &pipe->messageList, messageReceive ) ;
   connect4sendShort( connect, 0 ) ;

   if ( waitForReceipt != 0 )
   {
      MESSAGE4SEND *messageSend = (MESSAGE4SEND *)mem4createAllocNoZero( c4, &server->messageSendMemory, 10, sizeof( MESSAGE4SEND ), 10, 0 ) ;
      if ( messageSend == 0 )
         return error4describe( c4, e4memory, E96978, "Failure to sending message", 0, 0 ) ;

      l4add( &client->messageSendList, messageSend ) ;
      messageSend->message = messageReceive ;
      messageReceive->id = pipe->messageIdCount++ ;
      messageSend->sender = client ;
      connect4sendLong( connect, messageReceive->id ) ;
   }

   connect4sendFlush( connect ) ;

   return 0 ;
}



int server4clientSendCheck( SERVER4CLIENT *client )
{
   // checks to see if the message has been sent.  Just go through our list of messagesToSend, and look if messageId found...
   CONNECT4 *connect = &client->connect ;
   unsigned long messageId = connect4receiveLong( connect ) ;

   for ( MESSAGE4SEND *messageSend = 0 ;; )
   {
      messageSend = (MESSAGE4SEND *)l4next( &client->messageSendList, messageSend ) ;
      if ( messageSend == 0 )  // not found, we must have sent the message.
      {
         connect4sendShort( connect, 0 ) ;
         connect4sendFlush( connect ) ;
         return 0 ;
      }
      if ( messageSend->message->id == messageId )  // message found, must not have been sent to everybody yet
      {
         connect4sendShort( connect, r4continue ) ;
         connect4sendFlush( connect ) ;
         return 0 ;
      }
   }
}



int server4clientSendCancel( SERVER4CLIENT *client )
{
   CONNECT4 *connect = &client->connect ;
   unsigned long messageId = connect4receiveLong( connect ) ;

   for ( MESSAGE4SEND *messageSend = 0 ;; )
   {
      messageSend = (MESSAGE4SEND *)l4next( &client->messageSendList, messageSend ) ;
      if ( messageSend == 0 )  // not found, we must have sent the message.
         return 0 ;
      if ( messageSend->message->id == messageId )  // message found, must not have been sent to everybody yet
      {
         // cancel this message
         server4clientMessageRecvInitUndo( messageSend->sender, messageSend->message ) ;
         server4clientMessageSendInitUndo( client, messageSend ) ;
         return 0 ;
      }
   }
}



#ifndef S4OFF_TRAN
   /* not S4OFF_TRAN, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientTransactionInit( SERVER4CLIENT *client )
   {
      const char *charId, *pass, *licence;
      int rc ;
      unsigned short len ;
      long version ;
      TCP4ADDRESS tcpAddress ;

      // AS Feb 26/03 - Need to perform a final check to ensure encryption level is met here.
      CONNECT4 *connect = &client->connect ;
      CONNECT4LOW *conLow = connect->connectBuffer.connectLowPtr ;
      #ifdef S4PREPROCESS_COM /* LY 2003/05/22 : CONNECT4LOW.preprocess declared if S4PREPROCESS defined */
         if ( conLow->preprocess == 0 && client->server->requiresPreprocess == 1 )
            rc = e4preprocessRequired ;
         else
         {
      #endif
         // AS Sept 30/02 - mark that client did finally connect
         client->didConnectStatus = 1 ;

         // AS Apr 20/10 - for some reason, it is possible that the connection4data() here may be null.  Perhaps it is related to a client dicsonnecting for some reason in the middle of the connect?
         char *data = (char *)connection4data( &client->connection ) ;
         if ( data == 0 )
            return e4connection ;

         version = ntohl5(*(long *)data) ;
         charId = connection4data( &client->connection ) + sizeof( long ) ;
         pass = charId + strlen( charId ) + 1 ;
         if (CustomerStamp.securityOption >= 2)
         {
            licence = charId + strlen( charId ) + strlen(pass) + 2;
            rc = server4clientLicenceCheck(licence);
            if (rc < 0 )
            {
               connection4clear( &client->connection ) ;
               connection4addData(&client->connection, CustomerStamp.customerName, strlen(CustomerStamp.customerName)+1, NULL ) ;
               return(rc);
            }
         }

         tcpAddress = connect4peerTcpAddress( &client->connect ) ;
         // AS Mar 3/10 let's keep the clients ip address around for error reporting
         memcpy( &client->clientTcpAddress, &tcpAddress, sizeof( TCP4ADDRESS ) ) ;

         assert5port( "Build Numbering Support" );
         // AS July 23/02 - relax this constraint for testing
         #ifndef S4TESTING
            #ifndef S4STAMP_BUILD
               if (version != CustomerStamp.BuildNumber)  // CS 2002/06/18 compare build numbers
                  return e4version;
            #endif
         #endif

         rc = Server4clientTranAddUser( client, version, (unsigned long)client->id, charId, (short)strlen( charId ), pass, tcpAddress ) ;
         if (rc < 0)
            return(rc);
         len = (unsigned short)connection4len( &client->connection ) ;

         connection4clear( &client->connection ) ;
         rc = connection4addData(&client->connection, &client->id, 4, NULL ) ;
      #ifdef S4PREPROCESS_COM
         }
      #endif
      return rc ;
   }



   /* not S4OFF_TRAN, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientTranEofHalt( SERVER4CLIENT *client, char *name, const unsigned int nameLen )
   {
      // acts on the primary transaction file only
      CONNECTION4TRAN_EOF_HALT_INFO_OUT *out ;
      FILE4 *file ;
      #ifdef E4ANALYZE
         unsigned int len ;
      #endif

      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E70193 ) ;
      #endif

      if ( error4code( client->server->c4 ) < 0 )
         return e4codeBase ;

      file = &client->server->transFile.primary.file ;

      #ifdef E4ANALYZE
         if ( file->name == 0 )
            return error4( client->server->c4, e4info, E70193 ) ;
         len = strlen( file->name ) ;
         if ( len > nameLen )
            return error4( client->server->c4, e4info, E70193 ) ;
      #endif
      strcpy( name, file->name ) ;
      connection4clear( &client->connection ) ;
      connection4addData( &client->connection, NULL, sizeof( CONNECTION4TRAN_EOF_HALT_INFO_OUT ), (void **)&out ) ;
      out->eof = htonl5((unsigned long)file4longGetLo( file4lenLow( file ) ) ) ;
      return file4close( file ) ;
   }



   /* not S4OFF_TRAN, not S4OFF_COMMUNICATIONS, S4SERVER */
   int server4clientTranEof( SERVER4CLIENT *client )
   {
      // acts on the primary transaction file only
      CONNECTION4TRAN_EOF_INFO_OUT *out ;
      int rc ;

      #ifdef E4PARM_LOW
         if ( client == 0 )
            return error4( 0, e4parm_null, E70194 ) ;
      #endif

      file4flush( &client->server->transFile.primary.file ) ;
      connection4clear( &client->connection ) ;
      rc = connection4addData( &client->connection, NULL, sizeof( CONNECTION4TRAN_EOF_INFO_OUT ), (void **)&out ) ;
      out->eof = htonl5((unsigned long)file4longGetLo(file4lenLow( &client->server->transFile.primary.file ))) ;
      return rc ;
   }
#endif /* S4OFF_TRAN */



/* not S4OFF_COMMUNICATIONS, S4SERVER */
int server4clientLicenceCheck(const char *licence)
{
   // AS Sept 9/02 215 is not big enough to hold the stamp, make it 300
   char copyRight[300] ;
   int rc ;

   if ( strlen( licence ) == 0 )
      return e4invalidLicence ;

   rc = u4createCopyrightFromStamp( copyRight, sizeof( copyRight ) ) ;
   if ( rc != 0 )  // probably e4len, means buffer size not large enough
      return rc ;

   rc = memcmp( copyRight, licence, strlen( copyRight ) );
   if ( rc != 0 )
      return e4invalidLicence ;
   return rc ;
}


#ifdef S4WIN32
   // AS Sept. 3/02 - new functionality
   static int c4getFuncPtr( HINSTANCE hInst, const char * funcName, void **function )
   {
      // gets the pointer and sets rc to -1 if a failure
      *function = (void *)GetProcAddress( hInst, funcName ) ;
      if ( *function == 0 )
         return -1 ;
      return 0 ;
   }
#endif


// AS Oct 10/02 - client/server relate4count
int server4clientRelateCount( SERVER4CLIENT *client )
{
   CODE4 *c4 = client->server->c4 ;
   CONNECT4 *connect = &client->connect ;
   long relationId = connect4receiveLong( connect ) ;
   long expectedGo = -1 ;
   if ( relationId == -2 )  // used with testing to ensure we only go expected amount
   {
      expectedGo = connect4receiveLong( connect ) ;
      relationId = connect4receiveLong( connect ) ;
   }

   RELATION4 *relationOn = client4getRelation( client, relationId ) ;

   if ( relationOn == 0 )
   {
      connect4sendLong( connect, (long)e4relateRefer ) ;
      error4set( c4, 0 ) ;
   }
   else
   {
      #ifdef E4ANALYZE
         // set up the go's
         code4countPosReset( c4 ) ;
      #endif
      long count = relate4count( &relationOn->relate ) ;
      #ifdef E4ANALYZE
         if ( expectedGo != -1 && expectedGo != (long)code4countPosGet( c4 ) )
            connect4sendLong( connect, (long)e4result ) ;
         else
      #endif
            connect4sendLong( connect, count ) ;
   }
   connect4sendFlush( connect ) ;
   return 0 ;
}



// AS Sept. 3/02 - new functionality
int server4clientAdditionalFunction( SERVER4CLIENT *client )
{
   #ifdef S4WIN32
      // designed to link with an external user-supplied DLL.
      CODE4 *c4 = client->server->c4 ;
      CONNECT4 *connect = &client->connect ;
      SERVER4 *server = client->server ;

      long functionNumber = connect4receiveLong( connect ) ;
      long infoLen = connect4receiveLong( connect ) ;
      if ( server->addFuncInfoLen < infoLen )
      {
         if ( server->addFuncInfoLen != 0 )
         {
            u4free( server->addFuncInfo ) ;
            server->addFuncInfoLen = 0 ;
         }
         server->addFuncInfo = (char *)u4allocFree( c4, infoLen ) ;
         if ( server->addFuncInfo == 0 )
            return e4memory ;
         server->addFuncInfoLen = infoLen ;
      }
      connect4receive( connect, server->addFuncInfo, infoLen, code4timeoutVal( c4 ) ) ;

      if ( server->addDll == 0 )  // means not loaded yet...
      {
         // const char *dllName = &server->addDllName ;
         // if ( dllName == 0 || dllName[0] == 0 )  // means was not supplied, default to
         const char *dllName = "CBADD.DLL" ;
         server->addDll = LoadLibrary( dllName ) ;
         if ( server->addDll == 0 )
         {
            connect4sendShort( connect, e4loadlib ) ;
            connect4sendFlush( connect ) ;
            return 0 ;
         }
         int rc = c4getFuncPtr( server->addDll, "AdditionalFunctions", (void **)&server->addFunction ) ;
         if ( server->addFunction == 0 || rc != 0 )  // function not found
         {
            DWORD err = GetLastError() ;
            FreeLibrary( server->addDll ) ;
            server->addDll = 0 ;
            connect4sendShort( connect, e4loadlib ) ;
            connect4sendFlush( connect ) ;
            return 0 ;
         }
      }

      void *infoOut = 0 ;
      long infoLenOut = 0 ;
      server->addFunction( functionNumber, server->addFuncInfo, infoLen, &infoOut, &infoLenOut ) ;
      connect4sendShort( connect, 0 ) ; // indicate success
      if ( infoOut != 0 && infoLenOut > 0 )
      {
         connect4sendLong( connect, infoLenOut ) ;
         connect4send( connect, (char *)infoOut, infoLenOut ) ;
      }
      else
         connect4sendLong( connect, 0L ) ;
      connect4sendFlush( connect ) ;
      return 0 ;
   #else
      //this function is to link with a dll - not supported in non-windows
      connect4sendShort( connect, e4notSupported ) ;
      connect4sendFlush( connect ) ;
      return 0 ;
   #endif /* S4WIN32 */
}



static int server4clientMsgPreprocessBFU( SERVER4CLIENT *client, const CONNECTION4PREPROCESS_BASIC *data, CONNECTION4PREPROCESS_BASIC *decryptedData ) ;

int server4clientMsgPreprocessB( SERVER4CLIENT *client )
{
   #ifdef S4PREPROCESS_COM
      assert5port( "New function for basic (non public key) encryption" );
      // initialize encryption with client via basic exchange (userid/password aes encryption key)
      // this is simply the <userId><password>, with no spaces between them, and trimmed to 32 bytes (or
      // padded to 32 bytes with spaces)
      int rc = 0 ;
      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      CODE4 *c4 = client->server->c4 ;
      CONNECT4 *connect = &client->connect ;
      SERVER4 *server = client->server ;
      CONNECTION4PREPROCESS_BASIC data ;

      // AS Jan 22/03 - added presend of # bits to detect this as well
      short encryptBytes = connect4receiveShort( connect ) ;

      // read the encrypted data, including the user id, password, and symmetric aes key to use
      connect4receive( connect, &data, sizeof( CONNECTION4PREPROCESS_BASIC ), code4timeoutVal( c4 ) ) ;

      // AS Jan 22/03 - the # of encryptBytes may mismatch.  For example, if the user has sent us '32', it
      // means they are attempting 256-bit encryption, but we don't support that.
      int numServerBytes = server->numBits / 8 ;
      Bool5 mismatch = 0 ;
      // AS Mar 3/04 - The server may also be disabling encryption - we need to account for this too...
      if ( numServerBytes == 0 && encryptBytes != 0 )
         mismatch = 1 ;
      else
      {
         if ( numServerBytes == 1 )  // server set to 8-bit
         {
            if ( encryptBytes != 1 )
               mismatch = 1 ;
         }
         else // server set to 128-256 bit
         {
            if ( encryptBytes == 1 )
               mismatch = 1 ;
         }
      }

      if ( mismatch == 1 )
      {
         connect4sendShort( connect, e4preprocessMismatch ) ;
         connect4sendFlush( connect ) ;
         return 0 ;
      }

      // now, we need to find the user associated with this message (if valid)
      // go through each user and try decrypting the info until we find a match
      CONNECTION4PREPROCESS_BASIC decryptedData ;
      rc = server4clientMsgPreprocessBFU( client, &data, &decryptedData ) ;
      if ( rc != 0 )
      {
         connect4sendShort( connect, rc ) ;
         connect4sendFlush( connect ) ;
         return 0 ;
      }

      // we have found a valid user, extract the aes key from the decryptedData
      CONNECT4LOW *conLow = connect->connectBuffer.connectLowPtr ;

      if ( decryptedData.dataLen == 0 && server->requiresPreprocess == 1 )
         rc = e4preprocessRequired ;

      if ( decryptedData.dataLen == 0 )
      {
         connect4sendShort( connect, rc ) ;
         connect4sendFlush( connect ) ;
         return 0 ;
      }

      if ( rc == 0 )
      {
         // if here, we are ready to go...
         // start the encryption for reading...
         #ifdef S4ENCRYPT_DLL
            rc = con4lowPreprocessInit( c4, conLow, &decryptedData.data, decryptedData.dataLen ) ;
            if ( rc == 0 )
               con4lowSetEncryptRead( c4, conLow, 1 ) ;
         #else
            conLow->preprocess.encryptInit = encrypt4encryptInitHook( &conLow->preprocess, c4, &decryptedData.data, decryptedData.dataLen ) ;
            if ( conLow->preprocess.encryptInit == 0 )  // failed
               rc = -1 ;
            else
               conLow->preprocess.encryptRead = 1 ;
         #endif
      }

      // send the status back unencrypted
      connect4sendShort( connect, rc ) ;
      connect4sendFlush( connect ) ;

      if ( rc < 0 )
         return 0 ;

      // read encryption is enabled at this point.  This next message This next message will be encrypted with aes
      int dummy = connect4receiveShort( connect ) ;

      // initialize encryption for return message
      // receive/send timing short this is required for multi-thread support, we need to ensure that
      // both client and server have started communications
      // send timing short this is required for multi-thread support, we need to ensure that
      // both client and server have started communications
      // this next write will be encrypted,
      #ifdef S4ENCRYPT_DLL
         con4lowSetEncryptWrite( c4, conLow, 1 ) ;
      #else
         conLow->preprocess.encryptWrite = 1 ;
      #endif
      connect4sendShort( connect, 0 ) ;
      connect4sendFlush( connect ) ;

      return 0 ;
   #else
      // no pre-process available in this case
      int rc = 0 ;
      #ifdef E4ANALYZE
         if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
            return rc ;
      #endif

      CODE4 *c4 = client->server->c4 ;
      CONNECT4 *connect = &client->connect ;
      CONNECTION4PREPROCESS_BASIC data ;

      // AS Jan 22/03 - added presend of # bits to detect this as well
      short encryptBytes = connect4receiveShort( connect ) ;

      connect4receive( connect, &data, sizeof( CONNECTION4PREPROCESS_BASIC ), code4timeoutVal( c4 ) ) ;

      connect4sendShort( connect, e4notSupported ) ;
      connect4sendFlush( connect ) ;
      return 0 ;
   #endif
}



// AS May 13/02 data compression and encryption
int server4clientMsgPreprocessED( SERVER4CLIENT *client )
{
   assert5port( "New function for encryption" );
   // client does not want to perform encryption, is this ok with the server?
   CONNECT4 *connect = &client->connect ;
   SERVER4 *server = client->server ;

   if ( server->requiresPreprocess == 1 )
      connect4sendShort( connect, e4preprocessRequired ) ;
   else
      connect4sendShort( connect, 0 ) ;

   connect4sendFlush( connect ) ;
   return 0 ;
}



/*
int server4clientMsgPreprocessPK( SERVER4CLIENT *client )
{
   #ifdef E4ANALYZE
      int rc ;
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   CONNECT4 *connect = &client->connect ;

   short publicKeyLen = 0 ;
   connect4sendShort( connect, publicKeyLen ) ;
   connect4sendFlush( connect ) ;

   return 0 ;
}



// AS May 13/02 data compression and encryption
int server4clientMsgPreprocessEP( SERVER4CLIENT *client )
{
   // initialize encryption with client via private/public exchange
   int rc = 0 ;
   #ifdef E4ANALYZE
      if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
         return rc ;
   #endif

   CODE4 *c4 = client->server->c4 ;
   CONNECT4 *connect = &client->connect ;
   SERVER4 *server = client->server ;
   unsigned char symmetricKey[MAX4PUBLIC_KEY_SIZE] ;

   // we retrieve a symmetric key which has been encrypted with our public key
   short keyLen = connect4receiveShort( connect ) ;
   assert5( keyLen <= sizeof( symmetricKey ) ) ;
   connect4receive( connect, symmetricKey, keyLen, code4timeoutVal( c4 ) ) ;

   #ifdef S4PREPROCESS_COM
      if ( keyLen != 0 )  // client wants to perform encryption but we don't support it.
         rc = e4notSupported ;
   #else
      rc = e4notSupported ;
   #endif

   // send the status back unencrypted
   connect4sendShort( connect, rc ) ;

   if ( rc < 0 )
   {
      connect4sendFlush( connect ) ;
      return 0 ;
   }

   return 0 ;
}
*/


static int server4clientMsgPreprocessBFU( SERVER4CLIENT *client, const CONNECTION4PREPROCESS_BASIC *data, CONNECTION4PREPROCESS_BASIC *decryptedData )
{
   #ifdef S4PREPROCESS_COM
      assert5port( "New function for discovering a user when encrypted info is sent" );
      // returns r4success if user is found, error if not.
      // goes through each user in the accounts table and uses that accounts id/password to decrypt the
      // data.  It then checks to see if the decrypted data's id/password match the account.
      // if r4success, output paramater decryptedData contains the decrypted data
      DATA4 *acctTable = client->server->accounts.data ;
      CODE4 *c4 = client->server->c4 ;
      if ( acctTable == 0 )  // not found
         return e4invalidUserId ;
      int rc = d4top( acctTable ) ;
      if ( rc != 0 )  // not found
         return e4invalidUserId ;

      FIELD4 *user = d4field( acctTable, "ACCOUNTID" ) ;
      FIELD4 *pwd = d4field( acctTable, "PASSWORD" ) ;
      if ( user == 0 || pwd == 0 )  // not found
         return e4invalidUserId ;

      char userId[LEN4ACCOUNT_ID+1] ;
      char password[LEN4PASSWORD] ;
      char decryptKey[32] ;

      int rcStatus = e4invalidUserId ;  // default error
      #ifdef S4ENCRYPT_DLL
         CONNECT4LOW conLow ;  // use for holding encryption info
      #else
         PREPROCESS4 idEncrypt ;
      #endif

      for( ;; )
      {
         memcpy( userId, f4ptr( user ), LEN4ACCOUNT_ID ) ;
         userId[LEN4ACCOUNT_ID] = 0 ;
         c4upper( userId ) ;  // user id is case insensitive
         memcpy( password, f4ptr( pwd ), LEN4PASSWORD ) ;
         memset( decryptKey, ' ', sizeof( decryptKey ) ) ;

         // for decryption, use the account id followed by the password
         int pwdOffset = LEN4ACCOUNT_ID ;
         for ( ;; )
         {
            if ( userId[pwdOffset-1] != ' ' )
               break ;
            pwdOffset-- ;
            if ( pwdOffset == 0 )
               break ;
         }
         pwdOffset = min( pwdOffset, 32 ) ;
         memcpy( decryptKey, userId, pwdOffset ) ;
         if ( pwdOffset < 32 ) // include password
         {
            int maxCopy = min( 32 - pwdOffset, (int)f4len( pwd ) ) ;
            memcpy( decryptKey+pwdOffset, password, maxCopy ) ;
         }

         #ifdef S4ENCRYPT_DLL
            if ( con4lowPreprocessInit( c4, &conLow, decryptKey, sizeof( decryptKey ) ) != 0 )
               return error4describe( c4, e4notSupported, 81001, "failure in initializing basic encryption", 0, 0 ) ;
            // and decrypt the message...
            con4lowSetEncryptRead( c4, &conLow, 1 ) ;
            con4lowPostProcess( c4, conLow.preprocess, (unsigned char *)data, sizeof(CONNECTION4PREPROCESS_BASIC), (unsigned char *)decryptedData ) ;
            con4lowPreprocessInitUndo( c4, &conLow ) ;
            assert5( conLow.preprocess == 0 ) ;
         #else
            void *idEncryptInit = encrypt4encryptInitHook( &idEncrypt, c4, decryptKey, sizeof( decryptKey ) ) ;
            if ( idEncryptInit == 0 )  // failure to set up encryption
               return error4describe( c4, e4notSupported, 81001, "failure in initializing basic encryption", 0, 0 ) ;
            // and decrypt the message...
            encrypt4decryptHook( &idEncrypt, idEncryptInit, 0, 0, data, sizeof(CONNECTION4PREPROCESS_BASIC), decryptedData ) ;
            encrypt4encryptInitUndoHook( &idEncrypt, idEncryptInit ) ;
            idEncryptInit = 0 ;
         #endif

         if ( memcmp( userId, decryptedData->userId, LEN4ACCOUNT_ID ) == 0 )  // user id matches
         {
            if ( memcmp( f4ptr( pwd ), decryptedData->password, LEN4PASSWORD ) == 0 )  // password matches
               return 0 ;
            // otherwise just a bad password
            // there is a chance that the same user id with a different password exists, so keep looking but
            // change the default return code to bad password
            rcStatus = e4invalidPassword ;
         }

         // if ( account4userFound( client->server->accounts, decryptedDatauserId, password ) == 0 )  // found
         // {
         //    return 1 ;
         // }
         if ( d4skip( acctTable, 1L ) != 0 )  // end of table, not found...
            return rcStatus ;
      }
   #else
      return e4notSupported ;
   #endif
}
// AS Mar 17/03 don't include above functions if no communications
#endif /* not S4OFF_COMMUNICATIONS */
#endif /* S4SERVER */
