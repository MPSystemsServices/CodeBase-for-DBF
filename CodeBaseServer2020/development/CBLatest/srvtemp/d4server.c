/* d4server.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef S4SERVER
   #include "stamp5.hpp"

   #ifdef S4ODBC_BUILD
      #include "util5.hpp"
   #endif
   // AS Apr 20/04 - not used if threading is not enabled (odbc server component)
   #ifndef S4ODBC_BUILD
      extern STAMP5 CustomerStamp;
   #endif

   // AS July 29/02 - let's track a global server for error handling when no c4 present...
   SERVER4 *g_server = 0 ;

   #ifndef S4UNIX
      #ifdef __TURBOC__
         #pragma hdrstop
      #endif
   #endif

   #if !defined(S4OFF_ERROR) && !defined(S4UNIX) && !defined(S4TESTING) && !defined(S4WINDOWS)
      #include <conio.h>
   #endif

   #ifndef S4UNIX
      #include <direct.h>
      #include <process.h>
   #endif

   #ifdef E4DEBUG_INFO
      D4DISPLAY debugDisplay ;
   #endif

   #ifndef S4WINDOWS
      extern ERROR4DATA *e4errorData;
   #endif

   /*
      AS Jan 21/03 - The field info has been moved to d4srvcrt.c
   */



   // AS July 29/02 - let's track a global server for error handling when no c4 present...
   void S4FUNCTION server4setServerPtr( SERVER4 *server )
   {
      g_server = server ;
   }


   #ifdef TIMER5OUT
      void server4timerStart( SERVER4 *server, char *label )
      {
         if ( server->currentTimer == 0 )
            server->currentTimer = timer5create( server->timerMemory, label ) ;
         else
            server->currentTimer = timer5createSub( server->currentTimer, label ) ;

         if ( server->masterTimer == 0 )
            server->masterTimer = server->currentTimer ;
      }



      void server4timerStop( SERVER4 *server )
      {
         Timer5 *timer = server->currentTimer ;

         if ( timer == 0 )
            return ;

         timer5stop( timer ) ;

         timer5displayResults( timer, &server->timerFile ) ;

         server->currentTimer = timer->master ;

         if ( server->currentTimer == 0 )
            server->masterTimer = 0 ;

         timer5destroy( timer ) ;
      }
   #endif



   #if defined( E4DEBUG_INFO ) && defined( S4WIN32 )
      // void debug4displayInit( D4DISPLAY *disp, HWND h )
      // {
         // AS July 17/02 - don't actually display since the server now contains real info not an empty box
      // }



      void  S4FUNCTION debug4display( const char *str )
      {
         // AS July 17/02 - don't actually display since the server now contains real info not an empty box
         #ifdef S4TESTING
            u4writeErr( str, 1 ) ;
         #endif
      }
   #endif /* #if defined( E4DEBUG_INFO ) && defined( S4WINDOWS ) */



   SERVER4 *S4FUNCTION server4alloc( void )
   {
      /* use malloc/free since u4alloc/u4free will not be available since no CODE4 */
   //   return (SERVER4 *)malloc( (long)sizeof( SERVER4 ) ) ;

      // use new/delete so that for ole-db the Source5all gets constructed properly
      return new SERVER4 ;
   }



   void S4FUNCTION server4free( SERVER4 *server )
   {
   //   free( server ) ;

      delete server ;
   }



   int S4FUNCTION server4numClients( SERVER4 *server )
   {
      int count ;

      if ( server == 0 )
         return 0 ;
      CODE4 *c4 = server->c4 ;
      if ( c4 == 0 )
         return 0 ;
      // AS 07/31/00  - was not checking for error code here...
      // BCR 10/16/00 -- server4clientListReserve not defined for S4OFF_THREAD
      #ifndef S4OFF_THREAD
         int rc = server4clientListReserve( server ) ;
         if ( rc < 0 )
         {
            error4set( c4, 0 ) ;
            return 0 ;
         }
      #endif

      #ifdef S4OFF_THREAD
         count = l4numNodes( &server->clients ) ;
      #else
         count = l4numNodes( &server->clients.list ) ;
      #endif
      server4clientListRelease( server ) ;

      return count ;
   }



   #ifdef S4SERVER_GUI
      long S4FUNCTION server4maxUsers( SERVER4 *server )
      {
         if ( server == 0 )
            return 0 ;
         return server->maxUsers ;
      }



      long S4FUNCTION server4numTransCommitted( SERVER4 *server )
      {
         if ( server == 0 )
            return 0 ;
         return server->info.numTransCommitted ;
      }



      Bool5 S4FUNCTION server4autoRecovery( SERVER4 *server )
      {
         #ifdef S4OFF_TRAN
            return 0 ;
         #else
            // returns true if the auto recovery is enabled
            if ( server == 0 || server->c4 == 0 || server->c4->catalogClient == 0 || server->c4->catalogClient->trans.c4trans == 0 || server->c4->catalogClient->trans.c4trans->transFile == 0 )
               return 0 ;
            return (Bool5)( server->c4->catalogClient->trans.c4trans->transFile->backup.validState) ;
         #endif
      }



      long S4FUNCTION server4numTransRolledBack( SERVER4 *server )
      {
         if ( server == 0 )
            return 0 ;
         return server->info.numTransRolledBack ;
      }



      long S4FUNCTION server4numTables( SERVER4 *server )
      {
         if ( server == 0 )
            return 0 ;
         if ( server->c4 == 0 )
            return 0 ;
         return l4numNodes( &(server->c4->dataFileList) ) ;
      }



      long S4FUNCTION server4lockCount( SERVER4 *server )
      {
         if ( server == 0 )
            return 0 ;
         return server->info.lockCount ;
      }
   #endif



   unsigned short S4FUNCTION server4portNumber( SERVER4 *server )
   {
      unsigned short port = 0 ;
      if ( server )
         port = c4atoi( server->processId, strlen(server->processId) ) ;
      return port ;
   }



   /*
   char *S4FUNCTION server4protocol( SERVER4 *server )
   {
      if ( server == 0 )
         return 0 ;
      if ( server->c4 == 0 )
         return 0 ;
      return server->c4->protocol ;
   }
   */



   char *S4FUNCTION server4name( SERVER4 *server )
   {
      if ( server == 0 )
         return 0 ;

      return server->serverName ;
   }



   #ifndef S4OFF_TRAN
      static int server4transactionLock( SERVER4 *server )
      {
         // this function is used to lock the TRAN4LOCK_SERVER lock byte.
         // It does not appear to have any use except to avoid multiple accesses to the log file
         // if opened in shared mode.
         // also, there is never a corresponding unlock.
         // AS Mar 21/03 - for now, remove this...
         /*

         int rc ;
         #ifdef E4PARM_LOW
            if ( server == 0 )
               return error4( 0, e4parmNull, E93801 ) ;
         #endif

         rc = code4tranLockTransactions( &server->c4trans, TRAN4LOCK_MULTIPLE ) ;
         if ( rc < 0 )
            return rc ;
         if ( rc == r4locked )
            return error4( server->c4, e4server, E70191 ) ;
         rc = code4tranLockTransactions( &server->c4trans, TRAN4LOCK_SERVER ) ;
         if ( rc < 0 )
            return rc ;
         if ( rc == r4locked )
         {
            code4tranUnlockTransactions( &server->c4trans, TRAN4LOCK_MULTIPLE ) ;
            return error4( server->c4, e4server, E70191 ) ;
         }

         return rc ;
         */
         return 0 ;
      }
   #endif

   char config4name[240] ;
   int config4inited = 0 ;


   #ifdef E4ANALYZE
      static long g_server_count = 0 ;
   #endif


   #ifndef S4OFF_COMMUNICATIONS
      /* S4SERVER not S4OFF_COMMUNICATIONS */
      #ifndef S4OFF_THREAD
         void server4worker( void *serverVd )
         {
            /*
               FUNCTION

               server4worker( SERVER4 *server )

               NOTES

               This function is used by the worker threads as their main function.

               They wait on the SERVER4.workerSemaphore until signalled.

               When signalled, they remove an item from the SERVER4.socketsToService
                 list and then service the requested message (calling a function similar
                 to server4processMessage(), which interprets the message and then
                 services it).
            */
            SERVER4CLIENT *client ;
            int messageType, rc ;
            BOOL rcBool ;
            CONNECT4 *connect ;
            SERVER4 *server = (SERVER4 *)serverVd ;

            for ( ;; )
            {
               InterlockedIncrement( &server->c4->numWaitingWorkerThreads ) ;
               rcBool = semaphore4wait( &server->c4->workerSemaphore, WAIT4EVER ) ;
               InterlockedDecrement( &server->c4->numWaitingWorkerThreads ) ;
               if ( rcBool == FALSE || server->c4->shutdownWorkerThreads )  /* semaphore is invalid, shut thread down */
                  return ;

               // AS Sep 3/03 - Improved sequencing here.  What was happening was that the connection was sometimes
               // being disconnected after we removed it from the toService list, but before it was actually serviced.
               // this wasn't causing any critical errors but was causing general sequencing problems.
               // now we mark the connection as being serviced first until we are done with it.
               // connect = ((CONNECT4 *)list4mutexRemove( &server->c4->connectsToService ));
               list4mutexWait( &server->c4->connectsToService ) ;
               connect = (CONNECT4 *)l4pop( &server->c4->connectsToService.list ) ;
               connect->workState = CONNECT4RETRIEVING ;
               list4mutexRelease( &server->c4->connectsToService ) ;
               // AS Oct 31/01 - In some cases connect may be null...don't gpf
               // it happens when we are disconnecting, esp. with java clients, that we
               // release the to-service semaphore, but we actually disconnect the client
               // and remove it from the list, and thus we get here and the list is empty.
               if ( connect != 0 )
               {
                  // AS Sep 3/03 - Improved sequencing here.
                  assert5( connect->connectBuffer.connected == 1 && connect->workState == CONNECT4RETRIEVING ) ;
                  client = connect->client ;

                  for ( ;; )
                  {
                     /* next call receives and processes message */
                     /* AS 07/01/98 -- need to have exclusive client in case of error here... */
                     // AS 05/17/00 - modified to not require for message receive...
                     // code4enterExclusiveVerifyFirst( server->c4, client, 1 ) ;
                     // code4exitExclusiveVerifyLast( server->c4, client ) ;
                     messageType = connection4receiveMessage( &client->connection ) ;

                     #ifdef E4ANALYZE
                        g_server_count++ ;
                     #endif

                     if ( messageType < 0 )  /* failure, just cancel thread */
                     {
                        // some errors imply already disconnect (i.e. e4max and e4connectDenied)
                        /* LY 2002/11/19 : added check for e4version (causing disconnect in connection4receiveMessage()) */
                        // AS Jan 13/03 - also applies for e4invalidPassword and e4invalidUserId
                        // AS Sep 5/03 - New error e4badClient, means we have removed the client.
                        /* LY 2004/01/14 : added e4invalidLicense */
                        // AS Mar 21/06 - addeed e4invalidTcpAddress
                        switch( messageType )
                        {
                           case e4max:
                           case e4connectDenied:
                           case e4version:
                           case e4invalidPassword:
                           case e4invalidTcpAddress:
                           case e4invalidUserId:
                           case e4badClient:
                           case e4invalidLicence:
                              break ;
                           default:
                           #ifndef S4OFF_LOG
                              if ( server->c4->logConn > 0 )
                              {
                                 char buf[250] ;
                                 sprintf( buf, "ES0210:Server is disconnecting client due to invalid message of %ld for userId: [%s] and ip adress: [%s]",
                                               (long)messageType, client->account.accountId, client->account.tcpAddress ) ;
                                 assert5( strlen( buf ) < sizeof( buf ) ) ;
                                 code4logInfo( server->c4, buf ) ;
                              }
                           #endif
                           code4enterExclusiveVerifyFirst( server->c4, client, 1 ) ;
                           if ( connect->workState == CONNECT4RETRIEVING )
                           {
                              list4mutexWait( &server->c4->connectsToService ) ;
                              // AS Sep 3/03 - Improved sequencing here.  Allow disconnect to go through.
                              connect->workState = CONNECT4WORKING ;
                              list4mutexRelease( &server->c4->connectsToService ) ;
                           }
                           server4disconnect( server, client ) ;
                           code4exitExclusiveVerifyLast( server->c4, client ) ;
                        }
                        break ;
                     }

                     if ( messageType == STREAM4DISCONNECT
                        #ifdef S4JAVA
                          || messageType == JAVA4DISCONNECT
                        #endif
                        )
                     {
                        #ifndef S4OFF_LOG
                           if ( server->c4->logConn > 5 )
                           {
                              char buf[250] ;
                              sprintf( buf, "IS0090:Server has received a Disconnect request from the client for userId: [%s] and ip adress: [%s]",
                                       client->account.accountId, client->account.tcpAddress ) ;
                              assert5( strlen( buf ) < sizeof( buf ) ) ;
                              code4logInfo( server->c4, buf ) ;
                           }
                        #endif
                        break ;
                     }

                     assert5( ( client->connection.connected == 1 && connect->connectBuffer.connected == 1 && connect->workState == CONNECT4WORKING || connect->workState == CONNECT4SHUTDOWN ) ) ;

                     if ( client->connection.connected == 1 )
                        rc = connect4workContinue( &client->connect ) ;
                     else
                        break ;
                     if ( rc == r4finished )
                        break ;
                     if ( rc < 0 )  /* failure, just cancel thread */
                        return ;
                  }
               }
            }
         }
      #endif /* !S4OFF_THREAD */



      /* S4SERVER not S4OFF_COMMUNICATIONS */
      void server4accept( void *serverVoid )
      {
         /* main() for the server accept connection thread */

         SERVER4 *server = (SERVER4 *)serverVoid ;
         int rc ;
         CODE4 *c4 ;
         short portNo ;
         c4 = server->c4 ;

         c4->connectLowMemory = mem4create( c4, MEMORY4START_CONNECT_LOW, sizeof( CONNECT4LOW ), MEMORY4EXPAND_CONNECT_LOW, 0 ) ;
         if ( c4->connectLowMemory == 0 )
         {
            server->acceptThreadStatus = accept4statusFailed ;
            error4( c4, e4memory, E70169 ) ;
            return ;
         }
         c4->connectHalfMemory = mem4create( c4, 1, sizeof( CONNECT4HALF ), 2, 0 ) ;
         if ( c4->connectHalfMemory == 0 )
         {
            server->acceptThreadStatus = accept4statusFailed ;
            error4( c4, e4memory, E70169 ) ;
            return ;
         }
         #ifndef S4OFF_THREAD
            list4mutexInit(&c4->connectHalfListMutex) ;
         #endif

         assert5( server->processId != 0 ) ;
         portNo = (short)c4atol( server->processId, strlen( server->processId ) ) ;
         if ( connect4lowListen( &server->listenSocket, c4, &portNo, WS4MAX_PENDING_CONNECTS ) < 0 )
         {
            /* major, unrecoverable failure */
            server->acceptThreadStatus = accept4statusFailed ;
            error4( c4, e4result, E70169 ) ;
            return ;
         }
         #ifdef S4DEAD_CHECK
            server->timeCheck = time(NULL) + DEAD4CHECK_SERVER_WAITTIME ;
         #endif
         server->acceptThreadStatus = accept4statusInitialized ;
         #ifndef S4OFF_THREAD
            for( ;; )
            {
               rc = server4clientAccept( server, c4 ) ;
               if (rc != r4success && rc != e4max )
               {
                  server->acceptThreadStatus = accept4statusFailed ;
                  return ;
               }
               /* #if S4MAX_USERS != 0 */  /*CJ- Taken for Licincing and security reasons */
               /*if (CustomerStamp.maxConnections!=0)
                  if ( error4code(c4) < 0 || c4->shutdownWorkerThreads == 1 )*/
               /*#endif  */
               /*server4checkAllConnections(server) ;*/
            }
         #endif
      }
   #endif /* S4OFF_COMMUNICATIONS */



   #if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
      int server4transInitServerId( CODE4 *c4, SERVER4 *server )
      {
         int rc = server4transactionLock( server ) ;
         if ( rc < 0 )
            return rc ;

         long connectionId = code4connectionId( c4 ) ;
         TRAN4 *trans = &c4->catalogClient->trans ;

         // AS Apr 24/03 - Support for clientId < -2, to allow multiple server processes (odbc)
         assert5( c4->catalogClient->id <= SERVER4CLIENT_ID ) ;
         rc = tran4addUser( trans, c4->catalogClient->id, "SERVER", strlen( "SERVER" ) ) ;
         if ( rc < 0 )
            return error4( c4, e4result, E70168 ) ;
         else
            c4->catalogClient->transEnabled = 1 ;

         char transDefPath[LEN4PATH] ;
         // we are already positioned to the 'default' path, so just pass it in...
         u4nameCurrent( transDefPath, LEN4PATH, "" ) ;
         short pathLenVal = strlen( transDefPath ) + 1 ;
         // rc = tran4set( trans, trans->currentTranStatus, -1L, SERVER4CLIENT_ID, TRAN4DIRECTORY, pathLenVal, 0, 0 ) ;
         rc = tran4set( trans, trans->currentTranStatus, -1L, c4->catalogClient->id, TRAN4DIRECTORY, pathLenVal, 0, 0 ) ;
         if ( rc < 0 )
            return rc ;
         rc = tran4putData( trans, transDefPath, pathLenVal ) ;
         if ( rc < 0 )
            return rc ;
         rc = tran4lowAppend( trans, 0, 0 ) ;
         if ( rc < 0 )
            return rc ;

         // AS Apr 28/03 - made trans-shared a run-time switch
         if ( c4->transShared == 1 )
         {
            // AS Apr 21/03 - set the serverDataCount to offset to this id if using a shared transaction file (odbc server)
            server->serverDataCount = trans->c4trans->transFile->primary.userIdNo ;
            // AS Apr 21/03 - also set the clientIdCount to offset to this id if using a shared transaction file (odbc server)
            server->clientIdCount = server->serverDataCount ;
         }
         assert5( server->serverDataCount > 0 ) ;  // should always be at least 1

         return 0 ;
      }


      // AS July 5/02 - extract transaction handling out of server4init() to make easier to manipulate.
      static int server4initTransactions( CODE4 *c4, SERVER4 *server, const int logDisable, char *commandStartFail )
      {
         #ifdef E4DELETE_LOG
            u4remove( server->logName ) ;  // delete the log file when debugging
         #endif

         char *backLogName = 0 ;
         if ( server->doAutoRecovery == 1 )
            backLogName = server->backupLogName1 ;
         c4->logDisable = logDisable ;

         // If there is a command to run in the event of
         // failure, suppress the error message this time.
         int oldDisplayErrors = c4->displayErrors;
         if (strlen(commandStartFail) > 0)
            c4->displayErrors = 0;
         int rc = code4transFileEnable( &server->c4trans, server->logName, 0 ) ;
         c4->displayErrors = oldDisplayErrors;

         // AS Mar 21/03 - commandStartFail applies to the ODBC_ENABLED server, not the code used in the ODBC Engine
         #ifndef S4ODBC_BUILD
            // if there was a log file error and a recover command
            if (strlen(commandStartFail) > 0 && rc < 0)
            {
               STARTUPINFO si;
               PROCESS_INFORMATION pi;
               DWORD dwRC;

               ZeroMemory( &si, sizeof(si) );
               si.cb = sizeof(si);

               dwRC = CreateProcess(NULL, commandStartFail, NULL, NULL, false, 0, NULL, NULL, &si, &pi);
               if (dwRC == 0)
               {
                  // CreateProcess() failed
                  error4describeDefault(c4, e4server, E70241, commandStartFail, 0, 0);
                  return r4quit ;
               }

               // wait for the process to finish; timeout after 5 minutes
               dwRC = WaitForSingleObject(pi.hProcess, 5 * 60 * 1000);
               if (dwRC != WAIT_OBJECT_0)
               {
                  // something went wrong with the process; could be timed out
                  error4describeDefault(c4, e4server, E70241, commandStartFail, 0, 0);
                  return r4quit ;
               }

               GetExitCodeProcess(pi.hProcess, &dwRC);

               // for now, a return code of 0 means retry
               if (dwRC == 0)
               {
                  server->c4->currentClient->errorCode = 0;  // CS 2002/07/15 reset error code to zero
                  rc = code4transFileEnable( &server->c4trans, server->logName, 0 ) ;
               }
               else  // just quit
                  return r4quit ;
            }
         #endif /* !S4ODBC_BUILD */

         if ( rc == r4noExist )
         {
            // in the instance of LOG being LOG4COMPRESS, we don't need to open
            // the file until a transaction actually takes place, so don't create it
            // in that case
            if ( c4->logDisable != log4compressed )
               rc = code4transFileEnable( &server->c4trans, server->logName, 1 ) ;
         }

         if ( rc < 0 )
            return rc ;

         #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS )
            // AS Apr 28/03 - made trans-shared a run-time switch
            if( server->c4->transShared == 1 )
            {
               code4transServerShareInit( &server->c4trans ) ;
            }
         #else
            // do-auto-recovery not supported with ODBC
            if ( rc == 0 && server->doAutoRecovery )
            {
               rc = code4transFileEnableBackup( &server->c4trans, backLogName, 0 ) ;
               if ( rc == r4noExist )
                  rc = code4transFileEnableBackup( &server->c4trans, backLogName, 1 ) ;
            }
         #endif

         if ( c4->logDisable == log4disabled )
         {
            #if defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) && !defined( S4OFF_ODBC_TRANS )
               // AS Apr 28/03 - Support trans via registry setting
               if ( server->odbcTrans == 1 )
               {
                  assert5( 0 ) ;  // should never be possible to get here with odbc if odbc has transactions
               }
               else
            #endif
               {
                  if ( server->c4trans.transFile == 0 )
                  {
                     server->c4trans.transFile = &c4->server->transFile ;
                     if ( rc < 0 || server->c4trans.transFile == 0 )
                        return -1 ;
                  }

                  server->c4trans.transFile->primary.isDisabled = log4disabled ;
               }
         }
         else if ( c4->logDisable == log4compressed )
         {
            // AS Mar 20/03 - For ODBC (enabled/build), we don't support the log file compress, we only support
            // deleting it on startup.  One of the main reasons for this is to avoid 2 processes attempting to
            // create the log file at the same time.  Thus, we actually do create it at this point for ODBC_ENABLED,
            // and only support opening it with ODBC_BUILD.
            // with ODBC_BUILD, just ignore this
            #ifndef S4ODBC_BUILD
               // in this case, we either have a valid open transaction file or
               // there was none.  If the file was valid, just remove it now since
               // we don't need to keep it around...
               if ( server->c4trans.transFile != 0 )
               {
                  // need to delete it in this case
                  int rc = code4transInitUndo( &(server->c4trans) ) ;
                  if ( rc != 0 )
                     return rc ;

                  u4remove( server->logName ) ;
                  assert5( server->c4trans.transFile == 0 ) ;
               }


               server->c4trans.transFile = &c4->server->transFile ;
               if ( rc < 0 || server->c4trans.transFile == 0 )
                  return -1 ;
            #endif

            #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
               // AS Apr 28/03 - Support trans via registry setting
               // support log4compressed only if non-odbc servers support (i.e. not an ODBC server, or ODBC componenent does not support ODBC)
               Bool5 supportsCompressed = 1 ;
               #ifdef S4ODBC_BUILD
                  supportsCompressed = 0 ;
               #else
                  #if defined( S4ODBC_ENABLED ) && !defined( S4OFF_ODBC_TRANS )
                     if ( server->odbcTrans == 1 )
                        supportsCompressed = 0 ;
                  #endif
               #endif

               if ( supportsCompressed == 0 )
               {
                  // for ODBC-build / odbc-enabled
                  c4->logDisable = log4enabled ;
                  // and we must enable the log file
                  #ifdef S4ODBC_ENABLED
                     // create with regular server
                     rc = code4transFileEnable( &server->c4trans, server->logName, 1 ) ;
                  #else
                     // it should have been opened/enabled already...
                     if ( server->c4trans.transFile == 0 )
                        return error4( c4, e4trans, E70106 ) ;
                  #endif
               }
               else
            #endif
               {
                  server->c4trans.transFile->primary.isDisabled = log4compressed ;
                  return 0 ;
               }
         }

         if ( rc < 0 )
            return rc ;

         return server4transInitServerId( c4, server ) ;
      }
   #endif



   int S4FUNCTION server4init( SERVER4 *server, CODE4 *c4, const char *configName, Bool5 startAsService, long hWnd )
   {
      /* initializes the c4 structure and the server */
      /* if configName is null or a null string (""), then it is assumed that the default configuration is desired */

      /* ensure that this value is set at start because it affects the shutting down of the server.
         If it is not set and server4initUndo() is called, the program may get stuck in an endless loop */
      // CS 2001/04/03 hWnd is the window handle of the server.

      server->acceptThreadStatus = accept4statusNotStarted ;
      int rc = 0 ;

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E70105 ) ;
      #endif

      #ifdef OLEDB5BUILD
         alloc5init() ;
      #endif
      code4init( c4 ) ;
      #ifdef S4WINDOWS
         c4->hWnd = (HWND)hWnd ;
      #endif

      c4->runAsService = startAsService ;
      #ifdef OLEDB5BUILD
         server->source.c4 = c4 ;
      #endif
      c4->didAlloc = 1 ;  /* ensures that CODE4 gets freed when code4initUndo() is called */

      c4->displayErrors = 1 ;   /* display all errors that occur in case of initialization failure */

      c4->errTagName = 0 ;  /* esp. for ole-db, don't generate on the server side */

      // CS 2002/07/22 Moved SetCurrentDirectory to d4main.c

      #ifndef S4OFF_THREAD
         list4mutexInit( &server->clients ) ;
      #endif
      time( &server->info.serverStart ) ;

      server->serverDataCount = 1 ; // AS this value gets overridden later for shared-transaction processing

      server->c4 = c4 ;
      c4->server = server ;

      // AS Apr 24/03 - Support for clientId < -2, to allow multiple server processes (odbc)
      // as a result, we need this shared memory sooner (contains a serverid count)
      #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
         // AS 7/18/00 we also need to store the 'config file' name in shared memory in order that this info can be passed
         // since otherwise this is lost (the DBQ is set to the DEFPATH value, which may be different than the path of the config file)
         // setup the shared memory...
         server->odbcSharedMemory = share4init( c4, "ODBC4SHARE", SHARE4MEM_LEN, 0 ) ;  // simple flag...
         if ( server->odbcSharedMemory == 0 )
         {
            code4exitExclusive( c4, c4->catalogClient ) ;
            return error4describe( c4, e4server, E70101, "Failure creating ODBC4SHARE shared memory block", 0, 0 ) ;
         }
         assert5( SHARE4MEM_SERVER_COUNT_LEN == sizeof( long) ) ;
         long serverCount = 1 ;
         #ifdef S4ODBC_ENABLED
            if ( share4putData( server->odbcSharedMemory, SHARE4MEM_SERVER_COUNT_OFFSET, &serverCount, SHARE4MEM_SERVER_COUNT_LEN ) != 0 )
            {
               code4exitExclusive( c4, c4->catalogClient ) ;
               return error4describe( c4, e4server, E70101, "Failure accessing ODBC4SHARE shared memory block", 0, 0 ) ;
            }
            serverCount = 0 ;  // we are first
         #else
            if ( share4getData( server->odbcSharedMemory, SHARE4MEM_SERVER_COUNT_OFFSET, &serverCount, SHARE4MEM_SERVER_COUNT_LEN ) != 0 )
            {
               code4exitExclusive( c4, c4->catalogClient ) ;
               return error4describe( c4, e4server, E70101, "Failure accessing ODBC4SHARE shared memory block", 0, 0 ) ;
            }
            serverCount++ ;
            if ( share4putData( server->odbcSharedMemory, SHARE4MEM_SERVER_COUNT_OFFSET, &serverCount, SHARE4MEM_SERVER_COUNT_LEN ) != 0 )
            {
               code4exitExclusive( c4, c4->catalogClient ) ;
               return error4describe( c4, e4server, E70101, "Failure accessing ODBC4SHARE shared memory block", 0, 0 ) ;
            }
            serverCount-- ;
         #endif
      #endif

      rc = code4transInit( &server->c4trans, c4 ) ;
      if ( rc < 0 )
         return error4( c4, e4result, E70106 ) ;

      c4->catalogClient = (SERVER4CLIENT *)mem4createAllocZero( c4, &c4->clientMemory, 1, sizeof(SERVER4CLIENT), 1, 0 ) ;
      if ( c4->catalogClient == 0 )
         return e4memory ;
      code4enterExclusive( c4, c4->catalogClient, 1 ) ;

      // AS Apr 24/03 - Support for clientId < -2, to allow multiple server processes (odbc)
      // rc = server4clientInit( c4->catalogClient, server, SERVER4CLIENT_ID ) ; /*NULL*/
      #if ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) ) && !defined( S4OFF_ODBC_TRANS ) && !defined( S4OFF_TRAN )
         // AS Apr 28/03 - made trans-shared a run-time switch
         if( server->c4->transShared == 1 )
            rc = server4clientInit( c4->catalogClient, server, SERVER4CLIENT_ID - serverCount ) ;    // use the unique transaction id number as a basis for this value
         else
      #endif
            rc = server4clientInit( c4->catalogClient, server, SERVER4CLIENT_ID ) ;

      if ( rc < 0 )
      {
         code4exitExclusive( c4, c4->catalogClient ) ;
         return error4( c4, e4result, E70107 ) ;
      }

      c4->catalogClient->isStream = 1 ;  // suppress sending of messages on this client

      /* allow a single data4 for the configuration file... */

      c4->dataMemory = mem4create( c4, 1, sizeof( DATA4 ), 1, 0 ) ;
      if ( c4->dataMemory == 0 )
      {
         code4exitExclusive( c4, c4->catalogClient ) ;
         return 0 ;
      }

      c4->data4fileMemory = mem4create( c4, 1, sizeof(DATA4FILE), 1, 0 ) ;
      if ( c4->data4fileMemory == 0 )
      {
         code4exitExclusive( c4, c4->catalogClient ) ;
         return 0 ;
      }

      // AS 07/18/00 - must set up shared memory first because the configName is extracted for ODBC_BUILD processes
      #if defined( E4ANALYZE ) && defined( S4ODBC_ENABLED )
         // setup the shared memory for invoking debugger in other processes via this process...
         server->odbcSharedDebug = share4init( c4, "DRM4INVOKE", 1, 0 ) ;  // simple flag...
         if ( server->odbcSharedDebug == 0 )
         {
            code4exitExclusive( c4, c4->catalogClient ) ;
            return error4describe( c4, e4server, E70101, "Failure creating DRM4INVOKE shared memory block", 0, 0 ) ;
         }
         char setThisFlagInDebug = 0 ;  // change this at debug time to '1' invoke the debugger in other processes...
         if ( share4putData( server->odbcSharedDebug, 0, &setThisFlagInDebug, 1 ) != 0 )
         {
            code4exitExclusive( c4, c4->catalogClient ) ;
            return error4describe( c4, e4server, E70101, "Failure accessing DRM4INVOKE shared memory block", 0, 0 ) ;
         }
      #endif

      #ifdef S4ODBC_BUILD
         char configNameFromShare[LEN4PATH] ;
         if ( share4getData( server->odbcSharedMemory, SHARE4MEM_CONFIG_NAME_OFFSET, configNameFromShare, SHARE4MEM_CONFIG_NAME_LEN ) != 0 )
         {
            code4exitExclusive( c4, c4->catalogClient ) ;
            return error4describe( c4, e4server, E70101, "Failure accessing ODBC4SHARE shared memory block", 0, 0 ) ;
         }
         configName = configNameFromShare ;
      #endif

      if ( config4inited == 1 )
         configName = config4name ;
      else
      {
         char defConfigName[] = "S4SERVER.dbf" ;
         if ( configName == 0 )
            configName = defConfigName ;
         else
            if ( configName[0] == '\0' )
               configName = defConfigName ;
         u4nameCurrent( config4name, sizeof( config4name ), configName ) ;
         config4inited = 1 ;
      }

      u4nameCurrent( server->configName, sizeof( server->configName ), config4name ) ;

      #if defined( S4ODBC_ENABLED ) && !defined( S4ODBC_BUILD )
         char configNameForShare[LEN4PATH] ;
         memset( configNameForShare, 0, sizeof( configNameForShare ) ) ;
         strcpy( configNameForShare, server->configName ) ;
         if ( share4putData( server->odbcSharedMemory, SHARE4MEM_CONFIG_NAME_OFFSET, configNameForShare, SHARE4MEM_CONFIG_NAME_LEN ) != 0 )
         {
            code4exitExclusive( c4, c4->catalogClient ) ;
            return error4describe( c4, e4server, E70108, configName, 0, 0 ) ;
         }
      #endif

      #ifdef S4ODBC_BUILD
         unsigned char largeFileEnabledFlag ;
         if ( share4getData( server->odbcSharedMemory, SHARE4MEM_LARGE_OFFSET, &largeFileEnabledFlag, SHARE4MEM_LARGE_LEN ) != 0 )
         {
            code4exitExclusive( c4, c4->catalogClient ) ;
            return error4describe( c4, e4server, E70108, configName, 0, 0 ) ;
         }
         #ifdef S4FOX
            if ( largeFileEnabledFlag == 1 )
               code4largeOnLow( c4 ) ;
         #endif
      #else
         // if not the ODBC server component, we need to initialize connect acception.
         // in the ODBC server component we use the shared memory of the codebase server set below
         #ifdef S4ODBC_ENABLED
            if ( rc == 0 )
            {
               unsigned char connectAcceptFlag = 1 ;
               assert5( sizeof( connectAcceptFlag ) == SHARE4MEM_ACCEPT_LEN ) ;
               if ( share4putData( server->odbcSharedMemory, SHARE4MEM_ACCEPT_OFFSET, &connectAcceptFlag, SHARE4MEM_ACCEPT_LEN ) != 0 )
               {
                  code4exitExclusive( c4, c4->catalogClient ) ;
                  return error4describe( c4, e4server, E70108, configName, 0, 0 ) ;
               }

               // also set up the memory for whether or not large files are enabled
               unsigned char largeFileEnabledFlag = c4getLargeOn( c4 ) ? 1 : 0 ;
               assert5( sizeof( largeFileEnabledFlag ) == SHARE4MEM_LARGE_LEN ) ;
               if ( share4putData( server->odbcSharedMemory, SHARE4MEM_LARGE_OFFSET, &largeFileEnabledFlag, SHARE4MEM_LARGE_LEN ) )
               {
                  code4exitExclusive( c4, c4->catalogClient ) ;
                  return error4describe( c4, e4server, E70108, configName, 0, 0 ) ;
               }

               // by default the transaction file is not in use...
               // long emptyThreadId = 0 ;
               // assert5( sizeof( emptyThreadId ) == SHARE4MEM_TRANSACTION_LEN ) ;
               // rc = share4putData( server->odbcSharedMemory, SHARE4MEM_TRANSACTION_OFFSET, &emptyThreadId, SHARE4MEM_TRANSACTION_LEN ) ;
            }
         #else
            server->connectAccept = 1 ;
         #endif
      #endif

      #ifdef S4CLIPPER
         c4->autoOpen = 0 ;
      #endif
      int errOff_save = c4->errOff;
      c4->errOff = 1 ;  // CS 2003/07/18 generate error but suppress message
      DATA4 *config = d4open( c4, configName ) ;
      c4->errOff = errOff_save;
      if ( config == 0 )
      {
         char msg[32];  // CS 2003/07/18 include d4open error number in error message
         sprintf(msg, "open error: %d", error4code(c4));
         code4exitExclusive( c4, c4->catalogClient ) ;
         return error4describe( c4, e4server, E70108, configName, msg, 0 ) ;
      }
      #ifdef S4CLIPPER
         c4->autoOpen = 1 ;
      #endif

      c4->errFieldName = 0 ;
      rc = d4top( config ) ;
      if ( rc != 0 )
      {
         d4close( config ) ;
         config = 0 ;
         code4exitExclusive( c4, c4->catalogClient ) ;
         return error4describe( c4, e4server, E70109, configName, 0, 0 ) ;
      }

      Bool5 isEnteredExclusive = 1 ;

      for ( ;; )
      {
         FIELD4 *field = d4field( config, "SERVERID" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70105 ) ;
            break ;
         }
         int len = f4len( field ) ;
         char *ptr = f4ptr( field ) ;
         for ( ; len > 0 ; len-- )
         {
            if ( ptr[len-1] != ' ' )
               break ;
         }
         if ( len == 0 )   /* error because servername is mandatory */
         {
            rc = error4( c4, e4config, E70110 ) ;
            break ;
         }
         else
         {
            for( int i = 0 ; i < len ; i++ )
            {
               if ( ptr[i] == ' ' )
               {
                  rc = error4( c4, e4config, E70195 ) ;
                  break ;
               }
            }
            if ( rc < 0 )
               break ;
            #ifdef E4ANALYZE
               if ( (int)sizeof( server->serverName ) < (len + 1) )
               {
                  rc = error4( c4, e4struct, E70105 ) ;
                  break ;
               }
            #endif
            memcpy( server->serverName, ptr, (unsigned)len ) ;
            server->serverName[len] = 0 ;
         }

         field = d4field( config, "PROCESSID" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70204 ) ;
            break ;
         }
         if ( rc == 0 )
         len = f4len( field ) ;
         ptr = f4ptr( field ) ;
         for ( ; len > 0 ; len-- )
         {
            if ( ptr[len-1] != ' ' )
               break ;
         }
         if ( len != 0 )
         {
            server->processId = (char *)u4allocFree( c4, (long)len + 1L ) ;
            if ( server->processId == 0 )
            {
               rc = e4memory ;
               break ;
            }
            memcpy( server->processId, ptr, (unsigned)len ) ;
            server->processId[len] = 0 ;
         }

         field = d4field( config, "DEFPATH" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70196 ) ;
            break ;
         }
         len = f4len( field ) ;
         ptr = f4str( field ) ;
         char defPath[LEN4PATH] ;
         strcpy( defPath, ptr ) ;
         for ( ; len > 0 ; len-- )
         {
            if ( defPath[len-1] != ' ' )
               break ;
         }
         // AS 07/18/00 don't change the path until we have loaded the odbc executeables
         Bool5 changePath = 0 ;
         if ( len != 0 )
         {
            //  AS 07/18/00 - always trim blanks, makes ODBC work better
            defPath[len] = 0 ;  // CS 2002/06/19  chdir doesn't like trailing spaces
            #ifndef S4UNIX
               c4upper( defPath ) ;
            #endif

            changePath = 1 ;
         }

         // AS Apr 28/03 - Need some odbc info whether enabled or build
         #if defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )
            HKEY hk ;
            DWORD dwDisposition ;
            // AS 04/25/01 - Was setting the incorrect registry entry here...
            // const char *odbcPortNoKey = "SOFTWARE\\CodeBaseOdbc\\SERVER.INI\\CodeServerOdbc" ;
            const char *odbcPortNoKey = "SOFTWARE\\CodeBaseOdbc\\SERVER.INI\\CodeBaseOdbc" ;
            // the key should already exist in the registry (inserted at install time), so we don't need to specify the class type...
            LONG regStatus = RegCreateKeyEx( HKEY_LOCAL_MACHINE, odbcPortNoKey, 0, 0, REG_OPTION_NON_VOLATILE, KEY_WRITE, 0, &hk, &dwDisposition ) ;
            if ( regStatus != ERROR_SUCCESS )
            {
               char errNum[20] ;
               c4ltoa45( regStatus, errNum, 19 ) ;
               errNum[19] = 0 ;
               rc = error4describe( c4, e4config, E70238, "missing key: ", odbcPortNoKey, errNum ) ;
               break ;
            }
            const char *odbcDbqKey = "SOFTWARE\\ODBC\\ODBC.INI\\CodeBaseOdbc" ;
         #endif

         #if defined( S4ODBC_ENABLED )
            // get the ODBC fields and place them in the registry as required...
            field = d4field( config, "ODBCPORTNO" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70237 ) ;
               break ;
            }
            int odbcPortNo = f4int( field ) ;
            // put the port # into the registry where the ODBC engine will use it on start up...

            // AS May 2/03 - Some port numbers are invalid.  For example, a value of 0 causes the odbc server to
            // continually start innumerable processes.

            if ( odbcPortNo == 0 )
               odbcPortNo = 1583 ;

            char portNoChar[21] ;
            portNoChar[20] = 0 ;
            c4ltoa45( odbcPortNo, portNoChar, 20 ) ;
            char *ptrToStart = portNoChar ;
            // trim blanks at front, assume null-ended from c4ltoa45
            while ( ptrToStart[0] == ' ' )
               ptrToStart++ ;

            regStatus = RegSetValueEx( hk, "SvrPort", 0, REG_SZ, (BYTE *)ptrToStart, strlen( ptrToStart ) + 1 ) ;
            RegCloseKey( hk ) ;
            if ( regStatus != ERROR_SUCCESS )
            {
               rc = error4describe( c4, e4config, E70238, "failure to set key:", odbcPortNoKey, "MgrPort" ) ;
               break ;
            }

            // We use DEFPATH to indicate the DBQ unless DEFPATH is blank, in which case we use the server's default (config file)
            // directory as the path.
            char odbcDbq[LEN4PATH] ;
            int pathLen ;
            // get the path without the trailing backslash...
            if ( changePath == 1 )  // use defpath
            {
               strcpy( odbcDbq, defPath ) ;
               pathLen = strlen( odbcDbq ) ;
            }
            else   // no defpath, use the configuration file's path
               pathLen = u4namePathSpecial( odbcDbq, LEN4PATH, d4fileName( config ) ) ;

            regStatus = RegCreateKeyEx( HKEY_LOCAL_MACHINE, odbcDbqKey, 0, 0, REG_OPTION_NON_VOLATILE, KEY_WRITE, 0, &hk, &dwDisposition ) ;
            // const char *odbcDbqKey = "SOFTWARE\\ODBC\\ODBC.INI\\CodeBaseOdbc" ;
            // regStatus = RegCreateKeyEx( HKEY_CURRENT_USER, odbcDbqKey, 0, 0, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, 0, &hk, &dwDisposition ) ;
            if ( regStatus != ERROR_SUCCESS )
            {
               rc = error4describe( c4, e4config, E70238, "failure to open key", odbcDbqKey, 0 ) ;
               break ;
            }
            regStatus = RegSetValueEx( hk, "DBQ", 0, REG_SZ, (BYTE *)odbcDbq, pathLen + 1 ) ;
            if ( regStatus != ERROR_SUCCESS )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70238, "failure to set key", odbcDbqKey, "DBQ" ) ;
               break ;
            }

            // get the lock attempts
            field = d4field( config, "ODBCNMLOCK" ) ;
            if ( field == 0 )
            {
               RegCloseKey( hk ) ;
               rc = error4( c4, e4config, E70237 ) ;
               break ;
            }

            int lockAttempts = f4long( field ) ;
            if ( lockAttempts == 0 || lockAttempts < -1 ) // consider as -1
               lockAttempts = -1 ;
            char lockAttemptsChar[20] ;
            lockAttemptsChar[sizeof(lockAttemptsChar)-1] = 0 ;  // null terminate
            c4ltoa45( lockAttempts, lockAttemptsChar,  sizeof( lockAttemptsChar ) - 1 ) ;

            regStatus = RegSetValueEx( hk, ODBC4LOCK_ATTEMPTS, 0, REG_SZ, (BYTE *)lockAttemptsChar, strlen( lockAttemptsChar ) + 1 ) ;
            if ( regStatus != ERROR_SUCCESS )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70238, "failure to set key", odbcDbqKey, ODBC4LOCK_ATTEMPTS ) ;
               break ;
            }

            // get the Expose Recno (optional field, if not present, assume expose)
            Bool5 exposeRecno = 1 ;
            field = d4field( config, "EXPRECNO" ) ;
            if ( field != 0 )
               exposeRecno = f4true( field ) ;

            static char *trueStr = "yes" ;
            static char *falseStr = "no" ;

            if ( exposeRecno == 1 )
               regStatus = RegSetValueEx( hk, ODBC4EXPOSE_RECNO, 0, REG_SZ, (BYTE *)trueStr, strlen( trueStr ) + 1 ) ;
            else
               regStatus = RegSetValueEx( hk, ODBC4EXPOSE_RECNO, 0, REG_SZ, (BYTE *)falseStr, strlen( falseStr ) + 1 ) ;
            if ( regStatus != ERROR_SUCCESS )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70238, "failure to set key", odbcDbqKey, ODBC4EXPOSE_RECNO ) ;
               break ;
            }

            // get the collatins sequence
            field = d4field( config, "COLLATE" ) ;
            if ( field == 0 )
            {
               RegCloseKey( hk ) ;
               rc = error4( c4, e4config, E70237 ) ;
               break ;
            }

            if ( stricmp( f4str( field ), "general" ) == 0 )
               regStatus = RegSetValueEx( hk, ODBC4COLLATION, 0, REG_SZ, (BYTE *)"General", 8 ) ;
            else
               regStatus = RegSetValueEx( hk, ODBC4COLLATION, 0, REG_SZ, (BYTE *)"Machine", 8 ) ;

            if ( regStatus != ERROR_SUCCESS )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70238, "failure to set key", odbcDbqKey, ODBC4COLLATION ) ;
               break ;
            }

            // AS 02/06/01 - Also add 'Trim Blanks' for odbc to determine if blanks should be trimmed
            field = d4field( config, "TRIMBLANKS" ) ;
            if ( field == 0 )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70239, "TRIMBLANKS", 0, 0 ) ;
               break ;
            }

            // default if not set is 'true'
            if ( f4true( field ) == 0 )
               regStatus = RegSetValueEx( hk, ODBC4TRIM_BLANKS, 0, REG_SZ, (BYTE *)"No", 8 ) ;
            else
               regStatus = RegSetValueEx( hk, ODBC4TRIM_BLANKS, 0, REG_SZ, (BYTE *)"Yes", 8 ) ;

            if ( regStatus != ERROR_SUCCESS )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70238, "failure to set key", odbcDbqKey, ODBC4TRIM_BLANKS ) ;
               break ;
            }
            // need to save the start path for where to start the ODBC server
            char odbcStartPath[LEN4PATH] ;
            const char *odbcEngineName = ODBC4PROCESS_NAME ;
            u4nameCurrent( odbcStartPath, sizeof( odbcStartPath ), "" ) ;

         #endif /* defined( S4ODBC_ENABLED ) */

         #if defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )
            // AS Apr 28/03 - Support trans via registry setting
            field = d4field( config, "ODBCTRANS" ) ;
            if ( field == 0 )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70239, "ODBCTRANS", 0, 0 ) ;
               break ;
            }

            if ( f4true( field ) == 0 )
            {
               regStatus = RegSetValueEx( hk, ODBC4EXPOSE_TRANS, 0, REG_SZ, (BYTE *)"No", 8 ) ;
               server->odbcTrans = 0 ;
            }
            else
            {
               regStatus = RegSetValueEx( hk, ODBC4EXPOSE_TRANS, 0, REG_SZ, (BYTE *)"Yes", 8 ) ;
               server->odbcTrans = 1 ;
            }

            if ( regStatus != ERROR_SUCCESS )
            {
               RegCloseKey( hk ) ;
               rc = error4describe( c4, e4config, E70238, "failure to set key", odbcDbqKey, ODBC4EXPOSE_TRANS ) ;
               break ;
            }

            if ( server->odbcTrans == 1 )  // in this case we are sharing the transaction file
               c4->transShared = 1 ;

            RegCloseKey( hk ) ;
         #endif

         // AS 07/18/00 now that we have loaded the odbc files, we can change the default path
         if ( changePath == 1 )
         {
            if ( u4pathSet( defPath ) != 0 )
            {
               rc = error4describe( c4, e4config, E70198, defPath, 0, 0 ) ;
               break ;
            }
         }

         field = d4field( config, "SYSTEMPATH" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70196 ) ;
            break ;
         }
         len = f4len( field ) ;
         ptr = f4str( field ) ;
         for ( ; len > 0 ; len-- )
         {
            if ( ptr[len-1] != ' ' )
               break ;
         }

         if ( len == 0 )
            server->systemPath[0] = 0 ;
         else
         {
            memcpy( server->systemPath, ptr, len ) ;
            /* AS 09/28/99 --> was not inserting backslash here if required... */
            if ( server->systemPath[len] != S4DIR )
               server->systemPath[len++] = S4DIR ;
            server->systemPath[len] = 0 ;
         }

         #ifndef S4OFF_CATALOG
            field = d4field( config, "CATNAME" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70111 ) ;
               break ;
            }
            len = f4len( field ) ;
            ptr = f4ptr( field ) ;
            for ( ; len > 0 ; len-- )
            {
               if ( ptr[len-1] != ' ' )
                  break ;
            }
            char catalogName[LEN4PATH] ;
            if ( len == 0 )
               catalogName = 0 ;
            else
            {
               catalogName = (char *)u4allocFree( c4, (long)len + 1L ) ;
               if ( catalogName == 0 )
               {
                  rc = e4memory ;
                  break ;
               }
               memcpy( catalogName, ptr, (unsigned)len ) ;
                catalogName[len] = 0 ;
            }

            field = d4field( config, "ADDCATALOG" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70214 ) ;
               break ;
            }
            int addCatalog = f4int( field ) ;
            if ( addCatalog < 0 || addCatalog > 3 )
            {
               rc = error4( c4, e4config, E70215 ) ;
               break ;
            }

            field = d4field( config, "USECATALOG" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70216 ) ;
               break ;
            }
            int catalogStatus = f4int( field ) ;
            if ( catalogStatus < 0 || catalogStatus > 3 )
            {
               rc = error4( c4, e4config, E70217 ) ;
               break ;
            }
         #endif /* S4OFF_CATALOG */

         #ifndef S4OFF_COMMUNICATIONS
            #if ( defined( S4ENCRYPT_COM ) || defined( S4ENCRYPT_FILE ) )
               #ifdef S4ENCRYPT_DLL
                  rc = server4initEncrypt( server, config ) ;
               #else
                  rc = server4initPreprocess( server, config ) ;
               #endif
               if ( rc != 0 )
                  break ;
            #endif
         #endif
         // AS Oct 3/06 - for odbc, we need to get the encryption key for later use...
         #if defined( S4ODBC_BUILD )
            int keyLen = 0 ;  // AS Aug 25/09 - moved definition out of else loop since it is used later
            c4->encryptKeyLen = 0 ;
            field = d4field( config, "ENCRYPTLEV" ) ;
            if ( field == 0 )
               error4set( c4, 0 ) ;
            else
            {
               keyLen = min( f4long( field ), sizeof( c4->encryptKey ) ) ;
               c4->encryptKeyLen = keyLen ;
            }

            field = d4field( config, "ENCRYPTKEY" ) ;
            if ( field == 0 )
               error4set( c4, 0 ) ;
            else
            {
               int cpyLen ;
               if ( keyLen != c4->encryptKeyLen )
                  cpyLen = c4->encryptKeyLen ;
               else
                  cpyLen = min( f4len( field ), sizeof( c4->encryptKey ) ) ;
               c4->encryptKeyLen = cpyLen ;
               memcpy( c4->encryptKey, f4ptr( field ), cpyLen ) ;  // use f4ptr, may contain binary characters
            }
         #endif

         #ifndef S4OFF_TRAN
            field = d4field( config, "LOGNAME" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70112 ) ;
               break ;
            }
            len = f4len( field ) ;
            ptr = f4str( field ) ;
            for ( ; len > 0 ; len-- )
            {
               if ( ptr[len-1] != ' ' )
                  break ;
            }
            /* AS 11/10/98 -- modified so that if server->systempPath is not a fully quantified
               path, it won't be used as such.  Eg.  under old system, systempPath of "sub" would
               produce "\sub" off of the root drive instead of the defpath, or current, directory
               u4nameMakeFindDrive( server->logName, sizeof( server->logName ), server->systemPath, len == 0 ? defLogName : ptr ) ;
            */
            char defLogName[] = "S4SERVER.log" ;
            u4nameCurrentExtended( server->logName, sizeof( server->logName ), len == 0 ? defLogName : ptr, server->systemPath ) ;
            u4nameExt( server->logName, sizeof( server->logName ), "log", 0 ) ;  // ensure .log extension if not present

            // AS 09/15/00 - new fields for auto-recovery...
            field = d4field( config, "BACKLOG1" ) ;
            Bool5 doAutoRecovery = 1 ;
            if ( field == 0 )  // don't do auto-recovery
            {
               error4set( c4, 0 ) ;
               doAutoRecovery = 0 ;
            }

            if ( doAutoRecovery)
            {
               // check field.  If it is blank then don't do auto-recovery
               Bool5 isBlank = 1 ;
               for ( short fldContentsLoop = (short)f4len( field ) - 1 ; fldContentsLoop >= 0  ; fldContentsLoop-- )
               {
                  if ( (f4ptr( field ))[fldContentsLoop] != ' ' )
                  {
                     isBlank = 0 ;
                     break ;
                  }
               }
               if ( isBlank == 1 )
                  doAutoRecovery = 0 ;
            }

            if ( doAutoRecovery)
            {
               len = f4len( field  ) ;
               ptr = f4str( field ) ;
               u4nameCurrentExtended( server->backupLogName1, sizeof( server->backupLogName1 ), ptr, server->systemPath ) ;
               u4nameExt( server->backupLogName1, sizeof( server->backupLogName1 ), "log", 0 ) ;  // ensure .log extension if not present
               field = d4field( config, "BACKLOG2" ) ;
               if ( field == 0 )  // don't do auto-recovery
               {
                  error4set( c4, 0 ) ;
                  doAutoRecovery = 0 ;
               }
               // AS 02/20/01 - Don't do auto-recovery if the field is blank...
               if ( doAutoRecovery)
               {
                  // check field.  If it is blank then don't do auto-recovery
                  Bool5 isBlank = 1 ;
                  for ( short fldContentsLoop = (short)f4len( field ) - 1 ; fldContentsLoop >= 0  ; fldContentsLoop-- )
                  {
                     if ( (f4ptr( field ))[fldContentsLoop] != ' ' )
                     {
                        isBlank = 0 ;
                        break ;
                     }
                  }
                  if ( isBlank == 1 )
                     doAutoRecovery = 0 ;
               }
            }

            if ( doAutoRecovery)
            {
               // check field.  If it is blank then don't do auto-recovery
               Bool5 isBlank = 1 ;
               for ( unsigned long fldContentsLoop = f4len( field ) - 1 ; fldContentsLoop >= 0  ; fldContentsLoop++ )
               {
                  if ( (f4ptr( field ))[fldContentsLoop] != ' ' )
                  {
                     isBlank = 0 ;
                     break ;
                  }
               }
               if ( isBlank == 1 )
                  doAutoRecovery = 0 ;
            }

            if ( doAutoRecovery)
            {
               len = f4len( field  ) ;
               ptr = f4str( field ) ;
               u4nameCurrentExtended( server->backupLogName2, sizeof( server->backupLogName2 ), ptr, server->systemPath ) ;
               u4nameExt( server->backupLogName2, sizeof( server->backupLogName2 ), "log", 0 ) ;  // ensure .log extension if not present
               field = d4field( config, "PREBACKLOG" ) ;
               if ( field == 0 )  // don't do auto-recovery
               {
                  error4set( c4, 0 ) ;
                  doAutoRecovery = 0 ;
               }
            }

            if ( doAutoRecovery)
            {
               // check field.  If it is blank then don't do auto-recovery
               Bool5 isBlank = 1 ;
               for ( unsigned long fldContentsLoop = f4len( field ) - 1 ; fldContentsLoop >= 0  ; fldContentsLoop++ )
               {
                  if ( (f4ptr( field ))[fldContentsLoop] != ' ' )
                  {
                     isBlank = 0 ;
                     break ;
                  }
               }
               if ( isBlank == 1 )
                  doAutoRecovery = 0 ;
            }

            if ( doAutoRecovery)
            {
               len = f4len( field  ) ;
               ptr = f4str( field ) ;
               u4nameCurrentExtended( server->preBackupLogName, sizeof( server->preBackupLogName ), ptr, server->systemPath ) ;
               server->doAutoRecovery = 1 ;
            }
         #endif /* !S4OFF_TRAN */

         #ifdef S4DEBUG_LOG
            field = d4field( config, "DEBUGLOG" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70113 ) ;
               break ;
            }
            len = f4len( field ) ;
            ptr = f4ptr( field ) ;
            for ( ; len > 0 ; len-- )
            {
               if ( ptr[len-1] != ' ' )
                  break ;
            }
            /* AS 11/10/98 -- modified so that if server->systempPath is not a fully quantified
               path, it won't be used as such.  Eg.  under old system, systempPath of "sub" would
               produce "\sub" off of the root drive instead of the defpath, or current, directory
               u4nameMakeFindDrive( debugLogName, sizeof( debugLogName ), server->systemPath, len == 0 ? defDebugLogName  : ptr ) ;
            */
            char debugLogName[LEN4PATH] ;
            u4nameCurrentExtended( debugLogName, sizeof( debugLogName ), len == 0 ? defDebugLogName  : ptr, server->systemPath ) ;
         #endif /* S4DEBUG_LOG */

         #ifndef S4OFF_SECURITY
            field = d4field( config, "ACCTNAME" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70114 ) ;
               break ;
            }
            len = f4len( field ) ;
            ptr = f4ptr( field ) ;
            for ( ; len > 0 ; len-- )
            {
               if ( ptr[len-1] != ' ' )
                  break ;
            }
            /* AS 11/10/98 -- modified so that if server->systempPath is not a fully quantified
               path, it won't be used as such.  Eg.  under old system, systempPath of "sub" would
               produce "\sub" off of the root drive instead of the defpath, or current, directory
               u4nameMakeFindDrive( acctName, sizeof( acctName ), server->systemPath, len == 0 ? defAcctName  : ptr ) ;
            */
            char defAcctName[] = "ACCOUNT4" ;
            u4nameCurrentExtended( server->acctName, sizeof( server->acctName ), len == 0 ? defAcctName  : ptr, server->systemPath ) ;

            field = d4field( config, "PRIVNAME" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70115 ) ;
               break ;
            }
            len = f4len( field ) ;
            ptr = f4ptr( field ) ;
            for ( ; len > 0 ; len-- )
            {
               if ( ptr[len-1] != ' ' )
                  break ;
            }
            /* AS 11/10/98 -- modified so that if server->systempPath is not a fully quantified
               path, it won't be used as such.  Eg.  under old system, systempPath of "sub" would
               produce "\sub" off of the root drive instead of the defpath, or current, directory
               u4nameMakeFindDrive( privName, sizeof( privName ), server->systemPath, len == 0 ? defPrivName : ptr ) ;
            */
            char defPrivName[] = "PRIV4" ;
            u4nameCurrentExtended( server->privName, sizeof( server->privName ), len == 0 ? defPrivName : ptr, server->systemPath ) ;
         #endif /* S4OFF_SECURITY */

         field = d4field( config, "ERRNAME" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70116 ) ;
            break ;
         }
         len = f4len( field ) ;
         ptr = f4ptr( field ) ;
         for ( ; len > 0 ; len-- )
         {
            if ( ptr[len-1] != ' ' )
               break ;
         }
         char charSave ;
         char defErrorName[] = "S4SERVER.err" ;
         if ( len == 0 )
            ptr = defErrorName ;
         else
         {
            charSave = ptr[len] ;  // in case it goes into the next field, save and restore
            ptr[len] = 0 ;
         }
         /* AS 11/10/98 -- modified so that if server->systempPath is not a fully quantified
            path, it won't be used as such.  Eg.  under old system, systempPath of "sub" would
            produce "\sub" off of the root drive instead of the defpath, or current, directory
            u4nameMakeFindDrive( errName, sizeof( errName ), server->systemPath, len == 0 ? defErrorName : ptr ) ;
         */
         char errName[LEN4PATH] ;
         u4nameCurrentExtended( errName, sizeof( errName ), len == 0 ? defErrorName : ptr, server->systemPath ) ;
         if ( len != 0 )
            ptr[len] = charSave ;

         field = d4field( config, "NWORKTHRDS" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70116 ) ;
            break ;
         }
         long val = (unsigned int)f4long( field ) ;
         if ( val < 1 )
         {
            val = 1 ;
         }
         /* store # threads as negative until actually instigated, in case we
            need to shut down early */
         c4->numWorkerThreads = -val ;

         field = d4field( config, "MEMXBLOCK" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70117 ) ;
            break ;
         }
         len = f4int( field ) ;
         if ( len < 1 )
         {
            rc = error4( c4, e4config, E70118 ) ;
            break ;
         }
         c4->memExpandBlock = len ;

         field = d4field( config, "MEMXCLIENT" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70119 ) ;
            break ;
         }
         unsigned int val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1 )
         {
            rc = error4( c4, e4config, E70120 ) ;
            break ;
         }
         c4->memExpandClient = val2 ;

         field = d4field( config, "MEMXDATA" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70121 ) ;
            break ;
         }
         len = f4int( field ) ;
         if ( len < 1 )
         {
            rc = error4( c4, e4config, E70122 ) ;
            break ;
         }
          c4->memExpandData = len ;

         field = d4field( config, "MEMXLOCK" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70129 ) ;
            break ;
         }
         len = f4int( field ) ;
         if ( len < 1 )
         {
            rc = error4( c4, e4config, E70130 ) ;
            break ;
         }
          c4->memExpandLock = len ;

         #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
            // for CodeBaseDRM or the CodeServerODBC
            // to enabled sharing of files accross processes, do not keep them open...
            server->keepOpen = 0 ;
         #else
            field = d4field( config, "KEEPOPEN" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70123 ) ;
               break ;
            }
            if ( f4char( field ) == ' ' )
               server->keepOpen = 1 ;
            else
               server->keepOpen = f4true( field ) ;
         #endif

         field = d4field( config, "TCPBEGIN" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70234 ) ;
            break ;
         }
         if ( f4tcpAddress( field, &server->tcpBegin ) < 0 )  // invalid
         {
            rc = error4( c4, e4invalidTcpAddress, E70236 ) ;
            break ;
         }

         field = d4field( config, "TCPEND" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70235 ) ;
            break ;
         }
         rc = f4tcpAddress( field, &server->tcpEnd ) ;
         if ( rc < 0 )
         {
            rc = error4( c4, e4invalidTcpAddress, E70236 ) ;
            break ;
         }
         if ( rc == r4blankTcpAddress )
            memset( &server->tcpEnd, 255, sizeof( TCP4ADDRESS ) ) ;


         // AS Jan 23/04 - Support a suspension of performing the relate so other clients can perform actions
         field = d4field( config, "SUSPENDQ" ) ;
         if ( field == 0 )
         {
            // in this case assume suspension is dsired
            error4set( c4, 0 ) ;
            server->suspendQuery = 1 ;
         }
         else
         {
            if ( f4true( field ) )
               server->suspendQuery = 1 ;
            else
               server->suspendQuery = 0 ;
         }

         #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
            // for CodeBaseDRM or the CodeServerODBC
            c4->accessMode = OPEN4DENY_NONE ;
         #else
            field = d4field( config, "OPENMODE" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70210 ) ;
               break ;
            }
            if ( f4char( field ) == ' ' )
               c4->accessMode = OPEN4DENY_RW ;  // if config field blank, open exclusively by default
            else
            {
               rc = f4int( field ) ;
               if ( rc < 0 || rc > 2 )
               {
                  rc = error4( c4, e4config, E70212 ) ;
                  break ;
               }
               c4->accessMode = rc ;
            }
         #endif

         // AS Oct 22/02 - index validation available for server...
         Bool5 doValidate = 0 ;
         field = d4field( config, "VALIDATE" ) ;
         if ( field == 0 )
         {
            rc = error4describe( c4, e4config, E70239, "VALIDATE", 0, 0 ) ;
            break ;
         }

         doValidate = f4true( field ) ;

         // AS Sept 26/02 - new ability to log connection info into log file
         int logConn = 0 ;
         field = d4field( config, "LOGCONN" ) ;
         if ( field == 0 )
         {
            rc = error4describe( c4, e4config, E70239, "LOGCONN", 0, 0 ) ;
            break ;
         }

         logConn = f4long( field ) ;
         if ( logConn < 0 || logConn > 9 )
            logConn = 0 ;

         // AS May 30/03 - enable/disable log flushing - default is enabled. - improves performance (esp. ODBC), maybe cannot recover from crash.
         Bool5 logFlush = 0 ;
         field = d4field( config, "LOGFLUSH" ) ;
         if ( field == 0 )
         {
            rc = error4describe( c4, e4config, E70239, "LOGFLUSH", 0, 0 ) ;
            break ;
         }
         logFlush = f4true( field ) ;

         // AS Aug 26/02 - exclude when no transaction support (i.e. odbc)
         #ifndef S4OFF_TRAN
            // done for testing only at this point... comment out for now but leave as a future possibility
            Bool5 logDisable = log4enabled ;

            field = d4field( config, "LOG" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70222 ) ;
               break ;
            }
            else
            {
               if ( f4char( field ) == ' ' )
                  c4->log = LOG4ALWAYS ;
               else
               {
                  rc = f4int( field ) ;
                  assert5port( "Added support for compressing the log file when it exceeds a maximum size" ) ;
                  // AS Jul 5/02 - New setting to keep log file in a permanently compressed state - does not allow
                  // AS Mar 20/03 - For ODBC (enabled/build), we don't support the log file compress, we only support
                  // deleting it on startup.  One of the main reasons for this is to avoid 2 processes attempting to
                  // create the log file at the same time.  Thus, we actually do create it at this point for ODBC_ENABLED,
                  // and only support opening it with ODBC_BUILD.
                  if ( rc == LOG4COMPRESS )
                  {
                     // In this mode, we only log transactions.  In addition, once the log file reaches
                     // a specified size it will compress it at the next opportunity (compressing means it
                     // is deleted and the header (open) info is regenerated.  the next opportunity means
                     // when the next transaction is committed and there are no other outstanding transactions.
                     rc = LOG4TRANS ;
                     #ifndef S4OFF_TRAN
                        logDisable = log4compressed ;
                     #endif

                     // In this case we also need access to the MAX_LOG_SIZE
                     field = d4field( config, "MAXLOGSIZE" ) ;
                     if ( field == 0 )
                     {
                        rc = error4describe( c4, e4config, E70222, "or MAXLOGSIZE missing when LOG set to 5", 0, 0 ) ;
                        break ;
                     }
                     else
                     {
                        long lVal = f4long( field ) ;
                        if ( lVal < 0 )  // invalid to be negative...
                           rc = error4( c4, e4config, E70222 ) ;
                        server->maxLogSize = (unsigned long)lVal ;
                     }
                  }
                  else if ( rc == LOG4BACK_ONLY )
                  {
                     // not supported for ODBC
                     #if !defined( S4ODBC_ENABLED ) && !defined( S4ODBC_BUILD ) || defined( S4OFF_ODBC_TRANS )
                        // AS Aug 13/01 - Setting to allow logging into the backup log file only, not the primary log file
                        // In this mode we only want to log info to the backup log file.  However, in order for
                        // transactions to work we actually need to log them locally as well.  Therefore, the
                        // way this works is that the log file, if any, is deleted on startup, and is then used
                        // as required for 'real' transactions only.
                        // In the instance where backup logging is not actually being performed what happens is that
                        // logging is done as LOG4TRANS (in transaction only), and the log file is deleted here.
                        // this means that you lost the recovery feature due to logging but you still get normal
                        // transactions that work as long as the server doesn't crash during a transaction.
                        // This method (not backup logging, but LOG4BACK_ONLY) is not recommded
                        // AS Oct 18/01 - try to leave all disabled...
                        if ( doAutoRecovery == 0 )
                        {
                           // AS Oct 11/01 for LOG4BACK_ONLY, backup logging must be enabled.
                           // if it isn't, we have the side effect of acting as if LOG4TRANS is the value
                           // This is a special case however that the transaction file is not actually created
                           // unless a transaction is requested.  Also, the log file gets deleted below, so we
                          // never have the case that we can't start.  The log file is only used to handle
                          // actual physical transactions while the server is running in this case.
                           rc = LOG4TRANS ;
                        }
                        else
                        {
                           rc = LOG4ALWAYS ;
                        }
                        if ( rc < 0 || rc > 2 )
                        {
                           rc = error4( c4, e4config, E70221 ) ;
                           break ;
                        }
                        #ifndef S4OFF_TRAN
                           logDisable = log4disabled ;
                        #endif
                        // AS Aug 14/01 - also delete log file in this mode...
                        u4remove( server->logName ) ;
                     #else
                        // default to LOG4TRANS for ODBC if this mode selected
                        // AS May 14/03 - In ODBC this case, let's default to logcompress mode with max log size of 0
                        rc = LOG4TRANS ;
                        #ifndef S4OFF_TRAN
                           logDisable = log4compressed ;
                        #endif
                        server->maxLogSize = (unsigned long)0 ;
                     #endif
                  }
                  c4->log = rc ;
               }
            }
         #endif /* !S4OFF_TRAN */

         field = d4field( config, "MEMXINDEX" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70125 ) ;
            break ;
         }
         len = f4int( field ) ;
         if ( len < 1 )
         {
            rc = error4( c4, e4config, E70126 ) ;
            break ;
         }
         c4->memExpandIndex = len ;

         field = d4field( config, "MEMXTAG" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70131 ) ;
            break ;
         }
         len = f4int( field ) ;
         if ( len < 1 )
         {
            rc = error4( c4, e4config, E70132 ) ;
            break ;
         }
         c4->memExpandTag = len ;

         field = d4field( config, "MEMSZBLOCK" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70133 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 512 || ( ( val2 / 512 ) * 512 ) != val2 )
         {
            rc = error4( c4, e4config, E70134 ) ;
            break ;
         }
         c4->memSizeBlock = val2 ;

         field = d4field( config, "MEMSZBUF" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70135 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1024 || ( ( val2 / 1024 ) * 1024 ) != val2 )
         {
            rc = error4( c4, e4config, E70136 ) ;
            break ;
         }
         c4->memSizeBuffer = val2 ;

         field = d4field( config, "MEMSZMEMO" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70137 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         switch( code4indexFormat( c4 ) )
         {
            case r4cdx:
               if ( val2 < 33 || val2 > 16384 )
                  rc = error4( c4, e4config, E70138 ) ;
               break ;
            case r4mdx:
               if ( val2 < 512 || ( ( val2 / 512 ) * 512 != val2 ) )
                  rc = error4( c4, e4config, E70138 ) ;
               break ;
            default:
   /*            rc = error4( c4, e4notSupported, E70138 ) ; just let it not be used */
               break ;
         }
         if ( rc < 0 )
            break ;
         c4->memSizeMemo = val2 ;

         field = d4field( config, "MEMSZSB" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70139 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 2048 || ( ( val2 / 2048 ) * 2048 ) != val2 )
         {
            rc = error4( c4, e4config, E70140 ) ;
            break ;
         }
         c4->memSizeSortBuffer = val2 ;

         field = d4field( config, "MEMSZSP" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70141 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < c4->memSizeSortBuffer )
         {
            rc = error4( c4, e4config, E70142 ) ;
            break ;
         }
         c4->memSizeSortPool = val2 ;

         field = d4field( config, "MEMSTBLOCK" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70143 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1 )
         {
            rc = error4( c4, e4config, E70144 ) ;
            break ;
         }
         c4->memStartBlock = val2;

         field = d4field( config, "MEMSTCLIEN" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70145 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1 )
         {
            rc = error4( c4, e4config, E70146 ) ;
            break ;
         }
         c4->memStartClient = val2 ;

         field = d4field( config, "MEMSTMAX" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70149 ) ;
            break ;
         }
         val = f4long( field ) ;
         if ( val < (long)c4->memSizeBuffer )
         {
            rc = error4( c4, e4config, E70150 ) ;
            break ;
         }
         c4->memStartMax = val ;

         field = d4field( config, "MEMSTDATA" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70151 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1 )
         {
            rc = error4( c4, e4config, E70152 ) ;
            break ;
         }
         c4->memStartData = val2 ;

         field = d4field( config, "MEMSTLOCK" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70147 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1 )
         {
            rc = error4( c4, e4config, E70148 ) ;
            break ;
         }
         c4->memStartLock = val2 ;

         field = d4field( config, "MEMSTINDEX" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70155 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1 )
         {
            rc = error4( c4, e4config, E70156 ) ;
            break ;
         }
         c4->memStartIndex = val2 ;

         field = d4field( config, "MEMSTTAG" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70159 ) ;
            break ;
         }
         val2 = (unsigned int)f4long( field ) ;
         if ( val2 < 1 )
         {
            rc = error4( c4, e4config, E70160 ) ;
            break ;
         }
         c4->memStartTag = val2 ;

         field = d4field( config, "D4CREATE" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70201 ) ;
            break ;
         }
         if ( f4char( field ) == ' ' )
            server->doDataCreate = 1 ;
         else
            server->doDataCreate = f4true( field ) ;

         field = d4field( config, "I4CREATE" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70203 ) ;
            break ;
         }
         if ( f4char( field ) == ' ' )
            server->doIndexCreate = 1 ;
         else
            server->doIndexCreate = f4true( field ) ;

         field = d4field( config, "D4REINDEX" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70205 ) ;
            break ;
         }
         if ( f4char( field ) == ' ' )
            server->doReindex = 1 ;
         else
            server->doReindex = f4true( field ) ;

         field = d4field( config, "D4PACK" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70207 ) ;
            break ;
         }
         if ( f4char( field ) == ' ' )
            server->doPack = 1 ;
         else
            server->doPack = f4true( field ) ;

         field = d4field( config, "D4ZAP" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70209 ) ;
            break ;
         }
         if ( f4char( field ) == ' ' )
            server->doZap = 1 ;
         else
            server->doZap = f4true( field ) ;

         field = d4field( config, "D4CHECK" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70211 ) ;
            break ;
         }
         if ( f4char( field ) == ' ' )
            server->doCheck = 1 ;
         else
            server->doCheck = f4true( field ) ;

         field = d4field( config, "D4COMPRESS" ) ;
         if ( field == 0 )
         {
            rc = error4( c4, e4config, E70213 ) ;
            break ;
         }
         if ( f4char( field ) == ' ' )
            server->doCompress = 1 ;
         else
            server->doCompress = f4true( field ) ;

         #ifndef S4OPTIMIZE_OFF
            field = d4field( config, "OPTIMIZE" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70161 ) ;
               break ;
            }
            if ( f4char( field ) == ' ' )
               c4->optimize = -1 ;
            else
            {
               rc = f4int( field ) ;
               if ( rc < -1 || rc > 1 )
               {
                  rc = error4( c4, e4config, E70162 ) ;
                  break ;
               }
               #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
                  // AS 04/12/00 for ODBC to run seamlessly, we must not optimize shared files...
                  // therefore set the option to '-1' if it is normally set to '1'
                  if ( rc == 1 )
                     rc = -1 ;
               #endif
               c4->optimize = rc ;
            }

            field = d4field( config, "OPTIMIZEWR" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70163 ) ;
               break ;
            }
            if ( f4char( field ) == ' ' )
               c4->optimizeWrite = -1 ;
            else
            {
               rc = f4int( field ) ;
               if ( rc < -1 || rc > 1 )
               {
                  rc = error4( c4, e4config, E70164 ) ;
                  break ;
               }
               #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
                  // AS 04/12/00 for ODBC to run seamlessly, we must not optimize shared files...
                  // therefore set the option to '-1' if it is normally set to '1'
                  if ( rc == 1 )
                     rc = -1 ;
               #endif
               c4->optimizeWrite = rc ;
            }
         #endif

         #ifndef S4OFF_SECURITY
            field = d4field( config, "IDHANDLING" ) ;
            if ( field == 0 )
            {
               rc = error4( c4, e4config, E70165 ) ;
               break ;
            }
            rc = f4int( field ) ;
            if ( rc < 1 || rc > 4 )
            {
               rc = error4( c4, e4config, E70166 ) ;
               break ;
            }
            c4->idhandling = rc ;
         #endif /* S4OFF_SECURITY */

         char commandStartFail[LEN4PATH] = "";  // CS 2002/06/04
         field = d4field(config, "CMDSTFAIL");
         if (field)
         {
            len = f4len( field ) ;
            ptr = f4ptr( field ) ;
            memcpy(commandStartFail, ptr, len);
            c4trimN(commandStartFail, LEN4PATH);
         }

         c4->errFieldName = 1 ;
         d4close( config ) ;
         config = 0 ;
         code4dataFileCloseAll( c4 ) ;   /* ensure that files are really closed */
         mem4release( c4->dataMemory ) ;
         c4->dataMemory = 0 ;

         mem4release( c4->data4fileMemory ) ;
         c4->data4fileMemory = 0 ;

         code4exitExclusive( c4, c4->catalogClient ) ;
         isEnteredExclusive = 0 ;
         rc = server4clientInitUndo( c4->catalogClient ) ;
         if ( rc < 0 )
            break ;
         mem4free( c4->clientMemory, c4->catalogClient ) ;
         c4->catalogClient = 0 ;
         mem4release( c4->clientMemory ) ;
         c4->clientMemory = 0 ;
         c4->currentClient = 0 ;

         c4->catalogClient = (SERVER4CLIENT *)mem4createAllocZero( c4, &c4->clientMemory, c4->memStartClient, sizeof(SERVER4CLIENT), c4->memExpandClient, 0 ) ;
         if ( c4->catalogClient == 0 )
         {
            rc = error4( c4, e4memory, E70105 ) ;
            break ;
         }

         // AS 05/09/01 - Need to get exclusive access to catalog client here or else problems in some cases
         isEnteredExclusive = 1 ;
         code4enterExclusive( c4, c4->catalogClient, 1 ) ;

         c4->currentClient = c4->catalogClient ;
         #if ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) ) && !defined( S4OFF_ODBC_TRANS ) && !defined( S4OFF_TRAN )
            // AS Apr 28/03 - made trans-shared a run-time switch
            if( server->c4->transShared == 1 )
               rc = server4clientInit( c4->catalogClient, server, SERVER4CLIENT_ID - serverCount ) ;
            else
         #endif
               rc = server4clientInit( c4->catalogClient, server, SERVER4CLIENT_ID ) ;
         if ( rc < 0 )
         {
            rc = error4( c4, e4result, E70168 ) ;
            break ;
         }

         c4->catalogClient->isStream = 1 ;  // suppress sending of messages on this client

         int oldOpenError = c4->errOpen ;
         c4->errOpen = 0 ;
         c4->errorLog = (FILE4 *)u4allocEr( c4, sizeof( FILE4 ) ) ;
         if ( c4->errorLog == 0 )
         {
            rc = e4memory ;
            break ;
         }
         c4->errorLog->hand = INVALID4HANDLE ;  /* in case of error in log open */
         // AS Mar 17/03 - should be using internal open...
         rc = file4openInternal( c4->errorLog, c4, errName, 1, OPT4NONE ) ;
         c4->errOpen = oldOpenError ;
         if ( rc == r4noExist )
         {
            rc = file4createInternal( c4->errorLog, c4, errName, 1, OPT4NONE ) ;
            if ( rc != 0 )
            {
               rc = error4describe( c4, e4open, E70200, errName, 0, 0 ) ;
               break ;
            }
         }
         else
            if ( rc != 0 )
            {
               rc = error4describe( c4, e4open, E70199, errName, 0, 0 ) ;
               break ;
            }

         #if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
            #ifdef S4ODBC_BUILD
               if ( server->odbcTrans == 1 )
            #endif
               {
                  oldOpenError = c4->errOpen ;
                  int oldSafety = c4->safety ;
                  c4->errOpen = 0 ;
                  c4->safety = 0 ;
                  rc = server4initTransactions( c4, server, logDisable, commandStartFail ) ;
                  c4->errOpen = oldOpenError ;
                  c4->safety = oldSafety ;
                  if ( rc != 0 )  // < 0 or r4quit
                     break ;
               }
         #endif
         c4upper( server->serverName ) ;    /* to avoid networking case conflicts, always use upper-case */

         /* set info for default server-owned data files */
         #ifndef S4OFF_CATALOG
            if ( catalogName != 0 )
            {
               c4->catalog = cat4open( c4, catalogName ) ;
               if ( c4->catalog == 0 )
               {
                  rc = error4describe( c4, e4parm, E70183, catalogName, (char *)0, (char *)0 ) ;
                  break ;
               }
               #ifdef E4ANALYZE
                  rc = d4check( c4->catalog->data ) ;
                  if ( rc != 0 )
                  {
                     rc = error4describe( c4, rc, E70184, catalogName, (char *)0, (char *)0 ) ;
                     break ;
                  }
               #endif
               c4->catalog->catalogAdd = addCatalog ;
               c4->catalog->catalogStatus = catalogStatus ;
            }
         #endif

         #ifdef S4DEBUG_LOG
            if ( debugLogName != 0 )
            {
               rc = code4logInit( c4, debugLogName ) ;
               if ( rc < 0 )
                  break ;
            }
         #endif

         #ifndef S4OFF_CATALOG
            int oldUseCatalog ;
            if ( cat4avail( server->c4->catalog ) )
            {
               oldUseCatalog = server->c4->catalog->catalogStatus ;
               server->c4->catalog->catalogStatus = 0 ;
            }
            else
               oldUseCatalog = 0 ;
         #endif

         #ifndef S4OFF_SECURITY
            c4->errOpen = 0 ;
            int oldAccessMode = c4->accessMode ;
            c4->accessMode = OPEN4DENY_RW ;
            rc = 0 ;
            if ( isEnteredExclusive == 0 )
            {
               isEnteredExclusive = 1 ;
               code4enterExclusive( c4, c4->catalogClient, 1 ) ;
            }

            rc = account4init( c4, &server->accounts, server->acctName ) ;
            if ( rc < 0 )
               break ;

            rc = privilege4init( c4, &server->privileges, server->privName ) ;
            if ( rc < 0 )
               break ;

            code4exitExclusive( c4, c4->catalogClient ) ;
            isEnteredExclusive = 0 ;
            c4->errOpen = 1 ;
            c4->accessMode = oldAccessMode ;
         #endif

         #ifndef S4OFF_CATALOG
            if ( cat4avail( server->c4->catalog ) )
               server->c4->catalog->catalogStatus = oldUseCatalog ;
         #endif

         #ifndef S4OPTIMIZE_OFF
            code4optStart( c4 ) ;
         #endif

         c4->displayErrors = 0 ;   /* regular server error handling */
         // AS Sept 26/02 - new ability to log connection info into log file
         c4->logConn = logConn ;

         // AS May 30/03 - enable/disable log flushing - default is enabled. - improves performance (esp. ODBC), maybe cannot recover from crash.
         c4->logFlush = logFlush ;

         #ifndef S4ODBC_BUILD
            // AS Apr 25/03 - Was failing to check out flag
            if ( doValidate )
            {
               // AS Mar 19/03 - Don't perform validation within the ODBC component
               code4enterExclusive( c4, c4->catalogClient ) ;
               if ( code4validate( c4, 0, 1 ) < 0 )
                  rc = error4describe( c4, e4config, E70105, "Failure validating tables when VALIDATE configuration flag is set to true", 0, 0 ) ;
               code4exitExclusive( c4, c4->catalogClient ) ;
            }
         #endif

         #if defined( S4ODBC_ENABLED )
            // and now start the ODBCEngine... -  do this late in the sequence
            // must change back to start directory first else xml files are missing
            if ( changePath == 1 )
            {
               if ( u4pathSet( odbcStartPath ) != 0 )
               {
                  rc = error4describe( c4, e4config, E70198, defPath, 0, 0 ) ;
                  break ;
               }
            }

            // LY Dec 10/04 : repeate odbcEngineName (_spawnl() req. at least one cmd-line argument)
            c4->odbcEngineProcessId = _spawnl( _P_NOWAIT, odbcEngineName, odbcEngineName, 0 ) ;
            if ( c4->odbcEngineProcessId < 0 )
            {
               // AS 10/12/00 - add additional path for help in debugging this case
               // char fullDirectory[LEN4PATH] ;
               // u4nameCurrent( fullDirectory, sizeof( fullDirectory ), odbcEngineName ) ;
               char buff[120] ;
               sprintf( buff, "verify that executeable <%s> exists in server start-up directory", odbcEngineName ) ;
               rc = error4describe( c4, e4config, E70101, "unable to start CodeBase Server ODBC services:", odbcStartPath, buff ) ;
               break ;
            }

            if ( changePath == 1 )
            {
               if ( u4pathSet( defPath ) != 0 )
               {
                  rc = error4describe( c4, e4config, E70198, defPath, 0, 0 ) ;
                  break ;
               }
            }

            c4->odbcEngineStarted = 1 ;
         #endif

         break ;
      }

      if ( config != 0 )
      {
         d4close( config ) ;
         config = 0 ;
      }

      if ( isEnteredExclusive )
      {
         code4exitExclusive( c4, c4->catalogClient ) ;
         isEnteredExclusive = 0 ;
      }

      #ifndef S4OFF_CATALOG
         if ( catalogName != 0 )
         {
            u4free( catalogName ) ;
            catalogName = 0 ;
         }
      #endif

      if ( rc == 0 )
      {
         #ifdef S4JAVA
            server->javaSendBuffer = (char *)u4alloc( JAVA4SOCKET_SEND_BUFFER_SIZE ) ;
            if ( server->javaSendBuffer == 0 )
               return e4memory ;
            server->javaSendBufferLen = JAVA4SOCKET_SEND_BUFFER_SIZE ;
         #endif

         /* S4OFF_COMMUNICATIONS is defined when we want to do some direct testing into
            the server itself withtout the communications overhead - eg t5speed2.cpp
         */
         #ifndef S4OFF_COMMUNICATIONS
            #ifndef S4OFF_THREAD
               /* and now start the accept thread */
               assert5( server->processId != 0 ) ;
               server->acceptThreadStatus = accept4statusStarting ;
               server->acceptThreadHandle = _beginthread( server4accept, 5000, server ) ;
               if ( server->acceptThreadHandle == -1 )
                  return error4( c4, e4result, E70101 ) ;

               c4->workerThreadHandles = (unsigned long *)u4alloc( sizeof( unsigned long ) * -c4->numWorkerThreads ) ;
               if ( c4->workerThreadHandles == 0 )
                  return error4( c4, e4memory, E70101 ) ;

               /* and now start the worker threads */
               for ( int i = 0 ; i < -c4->numWorkerThreads ; i++ )
               {
                  c4->workerThreadHandles[i] = _beginthread( server4worker, 5000, server ) ;
                  if ( c4->workerThreadHandles[i] == -1 )
                  {
                     if ( i == 0 )  /* could not get any worker threads going, this is an error */
                        return error4( c4, e4result, E70101 ) ;

                     /* otherwise, just take the number of worker threads we obtained, and stop */
                     c4->numWorkerThreads = i ;
                     break ;
                  }
               }

              /* if we didn't stop early, set num threads now */
               if ( c4->numWorkerThreads < 0 )
                  c4->numWorkerThreads = -c4->numWorkerThreads ;
            #else
               server4accept(server);
            #endif /* #ifndef S4OFF_THREAD */
         #endif /*  #ifndef S4OFF_COMMUNICATIONS */
      }

      #ifndef S4OFF_THREAD
         event4init( &server->quitExitServerEvent ) ;
      #endif
      #if defined(S4LEN_PRINT) && defined(S4WIN32)
         LARGE_INTEGER freqVal ;
         QueryPerformanceFrequency( &freqVal ) ;
         char buffer[100] ;
         sprintf(buffer, "performance frequency: %d", freqVal ) ;
         debug4display( buffer ) ;
      #endif

      // AS Apr 20/04 - not used if threading is not enabled (odbc server component)
      #ifndef S4ODBC_BUILD
         #ifdef S4TESTING
            server->maxUsers = 10000 ;  // AS 08/17/99 for testing, allow more users...
         #else
            server->maxUsers = CustomerStamp.maxConnections ;
         #endif
      #endif

      #ifdef TIMER5OUT
         /* case where we want to perform timing of server operations */
         if ( rc == 0 )
         {
            char oldSafety = c4->safety ;
            c4->safety = 0 ;
            rc = file4create( &server->timerFile, c4, "TIMER5.OUT", 0 ) ;
            c4->safety = oldSafety ;
            if ( rc != 0 )
               rc = error4describe( c4, e4open, E70200, "TIMER5.OUT", 0, 0 ) ;
            server->timerMemory = timerMemory5create( c4 ) ;
            if ( server->timerMemory == 0 )
               rc = e4memory ;
         }
      #endif

      return rc ;
   }



   #ifndef S4OFF_COMMUNICATIONS
      int S4FUNCTION server4disconnect( SERVER4 *server, SERVER4CLIENT *client )
      {
         int rc ;

         #ifdef E4DEBUG
            /* it is possible that failures during the disconnect operation can cause a return to this code
               by another thread.  In that case, we just want to return since another thread is performing
               the disconnect already */
            if ( client->isDisconnecting == 1 )
               return error4( server->c4, e4struct, E91530 ) ;

            client->isDisconnecting = 1 ;
         #endif

         if (client->link.n == NULL )
            return r4success ;

         #ifndef S4OFF_THREAD
            connect4threadRegisterShutdown( &client->connect.connectBuffer.connectThread ) ;
         #endif

         server4clientListRemove( server, client ) ;

         #ifndef S4OFF_LOG
            if ( server->c4->logConn > 6 )
            {
               char buf[250] ;
               sprintf( buf, "IS0010:Server is disconnecting client due to server4disconnect() call for userId: [%s] and ip adress: [%s]",
                             client->account.accountId, client->account.tcpAddress ) ;
               assert5( strlen( buf ) < sizeof( buf ) ) ;
               code4logInfo( server->c4, buf ) ;
            }
         #endif

         rc = server4clientInitUndo( client, 0 ) ;
         mem4free( server->c4->clientMemory, client ) ;

         return rc ;
      }
   #endif /* S4OFF_COMMUNICATIONS */



   static int server4disconnectAllClients( SERVER4 *server )
   {
      CODE4 *c4 = server->c4 ;

      server4clientListReserve( server ) ;
      code4exitExclusive( c4, c4->catalogClient ) ;

      #ifdef E4ANALYZE
         if ( c4accessMutexCountNotZero( c4 ) )  // if not, something went wrong above when we entered final access mutex ourselves
         {
            server4clientListRelease( server ) ;  // AS July 22/02 - was not releasing list
            return error4( c4, e4info, E70170 ) ;
         }
      #endif

      for( SERVER4CLIENT *client = 0 ;; )
      {
         client = server4clientGetFirst( server ) ;

         if ( client == 0 )
            break ;

         #ifdef E4ANALYZE
            if ( client == c4->catalogClient )
            {
               server4clientListRelease( server ) ;
               return error4( c4, e4info, E70170 ) ;
            }
         #endif
         /* need to allow server4disconnect() to enter exclusive.  Note that at our point now, all worker threads are dead, so can shut-down ok, no race conditions*/
         #ifndef S4OFF_LOG
            if ( c4->logConn > 0 )
            {
               char buf[250] ;
               sprintf( buf, "IS0220:Server is disconnecting client due to server4disconnectAllClients() for userId: [%s] and ip adress: [%s]",
                        client->account.accountId, client->account.tcpAddress ) ;
               assert5( strlen( buf ) < sizeof( buf ) ) ;
               code4logInfo( c4, buf ) ;
            }
         #endif
         server4disconnect( server, client ) ;
      }

      // AS July 22/02 - changed function order to limit deadlock
      server4clientListRelease( server ) ;
      code4enterExclusive( c4, c4->catalogClient, 0 ) ;

      return 0 ;
   }



   int S4FUNCTION server4initUndo( SERVER4 *server )
   {
      #ifdef E4PARM_LOW
         if ( server == 0 )
            return error4( 0, e4parm_null, E70170 ) ;
      #endif

      CODE4 *c4 = server->c4 ;

      #ifdef E4ANALYZE
         if ( c4 == 0 )
            return error4( 0, e4struct, E70170 ) ;
      #endif

      #if defined( S4ODBC_ENABLED )
         // must halt the odbc engine if running...
         if ( c4->odbcEngineStarted == 1 )
         {
            c4->odbcEngineStarted = 0 ;
            odbc4shutdown( LNA_SHUTDOWN_GRACEFUL ) ;
         }
      #endif

      #if defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )
         if ( server->odbcSharedMemory != 0 )
         {
            share4initUndo( server->odbcSharedMemory ) ;
            server->odbcSharedMemory = 0 ;
         }
      #endif

      #if defined( E4ANALYZE ) && defined( S4ODBC_ENABLED )
         if ( server->odbcSharedDebug != 0 )
         {
            share4initUndo( server->odbcSharedDebug ) ;
            server->odbcSharedDebug = 0 ;
         }
      #endif

      #ifndef S4OFF_THREAD
         event4initUndo( &server->quitExitServerEvent ) ;
      #endif /* !S4OFF_THREAD */

      #if !defined( S4OFF_THREAD ) && !defined( S4OFF_COMMUNICATIONS )
         /* notify worker threads and the accept thread that we are shutting down */
         c4->shutdownWorkerThreads = 1 ;

         /* stop the accept thread - by closing the listenSocket */
         /* this way, new sockets with CONNECTION4IDLE cannot be created */
         /* AS 11/10/98 --> if listenSocket is not initialized yet, we can get stuck in an endless loop.  Happens when server fails to initialize. */
         int acceptThreadInitialized = 0 ;
         while( acceptThreadInitialized == 0 )
         {
            switch( server->acceptThreadStatus )
            {
               case accept4statusInitialized:
               case accept4statusFailed:
               case accept4statusNotStarted:
                  acceptThreadInitialized = 1 ;
                  break ;
               case accept4statusStarting:  // not ready yet
                  Sleep( 10 ) ;
                  break ;
            }
         }
         connect4lowClose( &server->listenSocket ) ;

         /* wait for the accept thread to shutdown now that we closed the listen socket */
         WaitForSingleObject( (HANDLE)server->acceptThreadHandle, INFINITE ) ;
      #endif /* #if !defined( S4OFF_THREAD ) && !defined( S4OFF_COMMUNICATIONS ) */

      #if defined( S4OFF_THREAD ) && !defined( S4OFF_COMMUNICATIONS )
         connect4lowClose( & server->listenSocket ) ;
      #endif

      /* now set the worker states to SERVER4SHUTDOWN so they will not service new incoming messages */
      // AS Mayh 30/06 - changed order to avoid deadlock...
      code4enterExclusive( c4, c4->catalogClient, 0 ) ;
      server4clientListReserve( server ) ;
      for ( SERVER4CLIENT *client = 0 ;; )
      {
         client = server4clientGetNext( server, client ) ;
         if ( client == 0 )
            break ;

         #ifdef E4ANALYZE
            if ( client == c4->catalogClient )
            {
               server4clientListRelease( server ) ;
               code4exitExclusive( c4, c4->catalogClient ) ; // AS July 22/02 - was not doing...
               return error4( c4, e4info, E70170 ) ;
            }
         #endif

         #if !defined( S4OFF_COMMUNICATIONS ) && !defined( S4OFF_THREAD )
            // AS Jan 23/04 - for all clients, note that we want to shut them down, if they are in a long operation
            client->connect.connectBuffer.connectionFailed = 1 ;
            if ( c4->accessMutexCount != 0 )
               if ( c4->currentClient != client )
                  connect4threadRegisterShutdown( &client->connect.connectBuffer.connectThread ) ;
         #endif
      }

      // AS Jan 23/04 - temporarily give control back to other clients to finish their shutdown procedure
      code4exitExclusive( c4, c4->catalogClient ) ;

      u4delaySec() ;

      // AS Jun 5/06 - release the list first...otherwise can get into deadlock
      server4clientListRelease( server ) ;
      code4enterExclusive( c4, c4->catalogClient, 0 ) ;

      #ifdef OLEDB5BUILD
         server->source.initUndo() ;
      #endif

      #if !defined( S4OFF_COMMUNICATIONS ) && !defined( S4OFF_THREAD )
         if ( c4->numWorkerThreads > 0 )
         {
            for ( int workerThreadIndex = 0 ; workerThreadIndex < c4->numWorkerThreads ; workerThreadIndex++ )
            {
               semaphore4release( &c4->workerSemaphore ) ;
            }

            // AS July 18/02 release the code4exclusive to allow them to run if required.
            code4exitExclusive( c4, c4->catalogClient ) ;

            /* AS 10/02/98 --> instead of infinite, wait up to 30 seconds only */
            if ( WaitForMultipleObjects( c4->numWorkerThreads, (HANDLE *)c4->workerThreadHandles, TRUE, 30000 ) == WAIT_TIMEOUT )
            {
               code4enterExclusive( c4, c4->catalogClient, 0 ) ;
               /* case where threads stuck.  disconnect clients and try again... */
               if ( server4disconnectAllClients( server ) < 0 )
               {
                  // AS July 22/02 - was not doing...
                  code4exitExclusive( c4, c4->catalogClient ) ;
                  return -1 ;
               }

               /* this time, ignore result, it should succeed.  If it doesn't, just fall through any way and shut down */
               WaitForMultipleObjects( c4->numWorkerThreads, (HANDLE *)c4->workerThreadHandles, TRUE, 30000 ) ;
            }
            else
               code4enterExclusive( c4, c4->catalogClient, 0 ) ;
            u4free( c4->workerThreadHandles ) ;
            c4->numWorkerThreads = 0 ;
         }
         semaphore4initUndo( &c4->workerSemaphore ) ;
      #endif /*  #if !defined( S4OFF_COMMUNICATIONS ) && !defined( S4OFF_THREAD ) */

      #ifndef S4OFF_COMMUNICATIONS
         if ( server4disconnectAllClients( server ) < 0 )
         {
            // AS July 22/02 - was not doing...
            code4exitExclusive( c4, c4->catalogClient ) ;
            return -1 ;
         }
      #endif /* S4OFF_COMMUNICATIONS */
      c4->displayErrors = 1 ;

      #ifndef S4OFF_SECURITY
         account4initUndo( &server->accounts ) ;
         privilege4initUndo( &server->privileges ) ;
      #endif

      // AS Sept 4/02 - init-undo for additional DLL
      #ifdef S4WIN32
         if ( server->addDll != 0 )
         {
            FreeLibrary( server->addDll ) ;
            server->addFunction = 0 ;
            server->addDll = 0 ;
         }
         if ( server->addFuncInfo != 0 )
         {
            u4free( server->addFuncInfo ) ;
            server->addFuncInfo = 0 ;
            server->addFuncInfoLen = 0 ;
         }
      #endif

      server4closeAll( server ) ;
      code4exitExclusive( c4, c4->catalogClient ) ;
      server4clientInitUndo( c4->catalogClient ) ;
      mem4free( c4->clientMemory, c4->catalogClient ) ;

      #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_ODBC_TRANS ) && !defined( S4OFF_TRAN )
         if ( server->c4trans.enabled != 0 )
            code4transInitUndo( &server->c4trans ) ;
         // AS Apr 28/03 - made trans-shared a run-time switch
         // AS MAy 20/03 - update for compile
         #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS ) && !defined( S4OFF_TRAN )
            if( server->c4->transShared == 1 )
               code4transServerShareInitUndo( &server->c4trans ) ;
         #endif
      #endif

      #ifdef S4JAVA
         u4free( server->javaSendBuffer ) ;
      #endif

      #ifdef S4PREPROCESS_PUBLIC
         if ( server->preprocessPKI != 0 )
         {
            public4keyInitUndo( server->preprocessPKI ) ;
            server->preprocessPKI = 0 ;
         }
      #endif
      /* AS 11/10/98 --> moved from top of function.  Don't free processId until accept thread
         is finished since that thread may use this variable, in particular if the shut down
         request comes before the server has initialized */
      if ( server->processId != 0 )
      {
         u4free( server->processId ) ;
         server->processId = 0 ;
      }

      mem4release( server->messageRecvMemory ) ;
      server->messageRecvMemory = 0 ;
      mem4release( server->messageSendMemory ) ;
      server->messageSendMemory = 0 ;

      #ifdef E4ANALYZE
         if ( c4accessMutexCountNotZero( c4 ) )
            return error4( 0, e4struct, E70170 ) ;
      #endif

      #ifdef TIMER5OUT
         /* case where we want to perform timing of server operations */
         file4close( &server->timerFile ) ;
         if ( server->timerMemory != 0 )
            timerMemory5destroy( server->timerMemory ) ;
      #endif

      server->c4 = 0 ;
      code4initUndo( c4 ) ;
      c4 = 0 ;

      #ifndef S4OFF_THREAD
         list4mutexInitUndo( &server->clients ) ;
      #endif

      #ifdef OLEDB5BUILD
         alloc5initUndo() ;
      #endif

      return 0 ;
   }



   int server4executeLogData( SERVER4 *server )
   {
      if ( server == 0 )
         return error4( 0, e4parm_null, E70174 ) ;
      return 0 ;
   }



   #ifdef E4ANALYZE
      int server4verify( SERVER4 *server, const int subs )
      {
         if ( server == 0 )
            return error4( 0, e4parm_null, E70175 ) ;

         if ( subs == 1 )
            return code4verify( server->c4, 1 ) ;

         return 0 ;
      }
   #endif



   DATA4 *server4open( SERVER4 *server, SERVER4CLIENT *client, const char *name, unsigned short int singleOpen, unsigned short int compatibility )
   {
      DATA4 *data, *dataOn ;
      // AS Oct 12/04 - in Clipper version this is done elsewhere...
      #ifndef S4CLIPPER
         INDEX4 *index ;
      #endif
      char nameBuf[258] ;
      CODE4 *c4 = server->c4 ;
      #ifdef S4FOX
         int oldCompatibility ;
      #endif

      #ifdef E4ANALYZE
         if ( server4verify( server, 1 ) < 0 )
            return 0 ;
         // AS May 22/03 - only with debug-print
         #ifdef S4OLEDEBUG_PRINT
            // AS May 16/03 - Perform some logging to help solve a c/s open error
            // need to know the clientId of opener and name
            // use the nameBuf to temp hold data
            sprintf( nameBuf, "server4open name: %s, clientId: %ld\r\n", name, (long)client->id ) ;
            log5( nameBuf ) ;
         #endif
      #endif

      /* check for a client's exclusive setting... */
      u4nameCurrent( nameBuf, sizeof( nameBuf ), name ) ;
      u4nameExt( nameBuf, sizeof(nameBuf), DBF4EXT, 0 ) ;
      DATA4FILE *d4 = dfile4data( c4, nameBuf ) ;
      if ( d4 != 0 )
      {
         if ( d4->singleClient != 0 )
         {
            // AS 05/06/99 --> we were failing to open in the case where the server was trying to get the
            // file into its schema table.  We need to relax the constraint of singleClient==client if
            // client == catalogClient (i.e. server itself attempting to open)... - added next line
            if ( client != c4->catalogClient )
            {
               if ( d4->singleClient != client )
               {
                  // AS Jul 12/06 - returning incorrect return code and info back to client
                  error4( c4, e4instance, E91102 ) ;
                  return 0 ;
               }
            }
         }
         else
            if ( c4->singleClient == 1 )
            {
               // AS May 16/03 - To fix a server-side closing issue - after create but before open it was possible that
               // the server would close the file, and if it was temporary it was being deleted.
               if ( d4->userCount <= 0 )
                  d4 = 0 ;  /* set singleClient as desired */
               else
               {
                  // AS Jul 12/06 - returning incorrect return code and info back to client
                  error4( c4, e4instance, E91102 ) ;
                  return 0 ;
               }
            }

         /* check for client's request for a single open */
         // AS May 16/03 - Consider d4 should not be null for test, comp fix
         if ( d4 != 0 && singleOpen == 1 )
         {
            for ( dataOn = 0 ;; )
            {
               dataOn = (DATA4 *)l4next( tran4dataList( &client->trans ), dataOn ) ;
               if ( dataOn == 0 )
                  break ;
               if ( dataOn->dataFile == d4 )
               {
                  // AS Jul 12/06 - returning incorrect return code and info back to client
                  error4( c4, e4instance, E91102 ) ;
                  return 0 ;
               }
            }
         }
      }

      #ifdef S4FOX
         assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
         oldCompatibility = c4->compatibility ;
         c4->compatibility = compatibility ;
      #endif
      data = d4open( c4, nameBuf ) ;
      #ifdef S4FOX
         c4->compatibility = oldCompatibility ;
      #endif

      if ( data == 0 )
         return 0 ;

      if ( data->dataFile == 0 )
      {
         d4close( data ) ;
         return 0 ;
      }

      #ifdef E4ANALYZE
         if ( server4verify( server, 1 ) < 0 )
         {
            d4close( data ) ;
            return 0 ;
         }
      #endif

      // AS Oct 12/04 - in Clipper version this is done elsewhere...
      #ifndef S4CLIPPER
         if ( data->indexes.lastNode != 0 )
         {
            index = (INDEX4 *)data->indexes.lastNode ;
            u4namePiece( index->accessName, LEN4PATH+1, name, 1, 0 ) ;
         }
      #endif

      return data ;
   }



   int S4FUNCTION server4quit( SERVER4 *server, int newVal )
   {
      #ifdef S4OLEDEBUG_PRINT
        log5( "server4quite called\r\n" ) ;

      #endif
      static int doQuit = 0 ;

      int oldVal = doQuit ;
      if ( newVal != -1 )
      {
         doQuit = newVal ;
         #ifdef S4COMTHREADS
            event4set( &server->quitExitServerEvent ) ;
         #endif
      }

      return oldVal ;
   }



   #ifndef S4OFF_COMMUNICATIONS
      // used with S4SLOW only for testing connection timeouts
      long g_slowTimeDelay = 0 ;


      static int server4clientSetSlowDelay( SERVER4CLIENT *client )
      {
         // for testing, etc. sets a delay loop on the server to test connection timeouts
         #ifdef S4SLOW
            g_slowTimeDelay = connect4receiveLong( &client->connect ) ;
         #endif

         return 0 ;
      }
   #endif /* !S4OFF_COMMUNICATIONS */



   #ifndef S4OFF_COMMUNICATIONS
      /* S4SERVER not S4OFF_COMMUNICATIONS */
      int S4FUNCTION server4processMessage( SERVER4 *server, SERVER4CLIENT *client )
      {
         int saveRc, doDisconnect ;
         unsigned short int len ;
         short saveError, sendError, rc ;
         static char empty[] = "" ;
         #ifndef S4OFF_TRAN
            int i, oldAccessMode ;
            char name[258] ;
         #endif

         #ifdef E4PARM_LOW
            if ( server == 0 || client == 0 )
               return error4( 0, e4parm_null, E70179 ) ;
         #endif

         CODE4 *c4 = server->c4 ;
         CONNECTION4 *connection = &client->connection ;

         #ifdef E4ANALYZE
            if ( ( rc = server4verify( server, 1 ) ) < 0 )
               return rc ;

            if ( connection == 0 )
               return error4( c4, e4struct, E70179 ) ;

            if ( ( rc = server4clientVerify( client, 1 ) ) < 0 )
               return rc ;
         #endif

         CONNECT4 *connect = connection->connect ;

         rc = 0 ;
         int actionType = connection4type( connection ) ;

         #ifdef S4OFF_THREAD
            server->info.numRequests += 1;
            client->info.numRequests += 1;
         #else
            InterlockedIncrement( &server->info.numRequests ) ;
            InterlockedIncrement( &client->info.numRequests ) ;
         #endif

         // AS 08/20/98 - done in worker loop
         //   /* need to now exclude out other clients from accessing the CODE4.
         //      this call also sets c4->currentClient to our client */
         //   code4enterExclusiveVerifyFirst( c4, client, 1 ) ;
         #ifdef S4OFF_THREAD
            c4->currentClient = client ;
            client->errorCode = 0 ;
            /* There are conchecks in various places that want 'accessMutexCount'
               to be non-zero. It is normally incremented in 'code4enterExclusive'.
               So, since that stuff has been removed here, we need to get it
               set properly. If the various calls in this routine for entering
               and exitting exclusive mode are re-enabled, make sure that even
               if S4OFF_THREAD is defined, that 'accessMutexCount' is handled
               properly. Alternatively, find all the conchecks on it, and make
               them not check it if S4OFF_THREAD is defined. -cg 99/10/29
               99/11/04: some tests are failing because 'code4Verify' is
               thinking things are locked when they aren't. So, need to clear
               accessMutexCount on any exit from this routine. */
            /* BCR 10/25/00 -- setting accessMutexCount here causes conflict
               with the settings in code4enterExclusive and code4exitExclusive */
            c4->accessMutexCount = 1 ;
         #endif

         #ifdef S4DISTRIBUTED
            if ( client->trans.currentTranStatus == r4partial && ( actionType != CON4COMMIT_PHASE_TWO && actionType != CON4DISCONNECT && actionType != CON4CLOSE && actionType != CON4UNLOCK ) )
               rc = error4( c4, e4commit, E70180 ) ;
            else
         #endif
         {
            c4setReadLock( c4, (char)connection4readLock( connection ) ) ;
            code4unlockAutoSet( c4, connection4unlockAuto( connection ) ) ;
            // AS Aug 29/03 - Support for memSizeMemoExpr which can change at any time
            #ifndef S4STAMP_BUILD
               code4memSizeMemoExprSet( c4, connection4memSizeMemoExpr( connection ) ) ;
            #endif

            if ( actionType >= STREAM4START_COUNT ) /* STREAM4 and MSG4 messages do not return a status to the caller */
               client->isStream = 1 ;
            else
               client->isStream = 0 ;

            #ifdef S4SLOW
               // allow testing with a delay loop to test connection timeouts, etc.
               if ( g_slowTimeDelay != 0 )
               {
                  #ifdef S4UNIX
                     struct timeval tv;

                     tv.tv_sec = g_slowTimeDelay / 1000;
                     tv.tv_usec = g_slowTimeDelay * 1000;
                     (void) select(0, 0, 0, 0, &tv);
                  #else
                     Sleep( g_slowTimeDelay ) ;
                  #endif
               }
            #endif

            // AS 05/06/99 --> if any exception occurs that is not caught, just remove the client...
            #ifdef OLEDB5BUILD
               try
               {
            #endif
                  switch ( actionType )  /* perform the action */
                  {
                     case CON4ADD_TAG:
                        rc = server4clientAddTag( client ) ;
                        break ;
                     case CON4APPEND:
                        rc = server4clientAppend( client ) ;
                        break ;
                     case CON4BOTTOM:
                        rc = server4clientBottom( client ) ;
                        break ;
                     case CON4CALC_CREATE:
                        rc = server4clientCalcCreate( client ) ;
                        break ;
                     case CON4CALC_RESET:
                        rc = server4clientCalcReset( client ) ;
                        break ;
                     case CON4CANCEL:
                        rc = server4clientCancel( client ) ;
                        if ( rc == r4exit )   /* client requested its own cancel, so can't return info to itself */
                        {
            //               code4exitExclusiveVerifyLast( c4, client ) ;
                           #ifdef S4OFF_THREAD
                              c4->accessMutexCount = 0 ;
                           #endif
                           return 0 ;
                        }
                        break ;
                     /* special functionality for testing the server */
                     case CON4CATALOG:
                        rc = server4clientCatalogSet( client ) ;
                        break ;
                     case CON4CHECK:
                        rc = server4clientCheck( client ) ;
                        break ;
                     case CON4CLOSE:
                        rc = server4clientClose( client ) ;
                        break ;
                     case CON4CLOSE_FILES:
                        rc = server4clientConnectCloseFiles( client ) ;
                        break ;
                     case CON4COMMIT_BOTH_PHASES:
                        rc = server4clientTransactionCommitBothPhases( client ) ;
                        break ;
                     case CON4COMMIT_PHASE_ONE:
                        rc = server4clientTransactionCommitPhaseOne( client ) ;
                        break ;
                     case CON4COMMIT_PHASE_TWO:
                        rc = server4clientTransactionCommitPhaseTwo( client ) ;
                        if ( rc == 0 )
                           client->info.numCompletedTransactions++ ;
                        break ;
                     case CON4CONFIG_NAME:
                        rc = server4clientConfigName( client ) ;
                        break ;
                     case CON4CONNECT:
                        {
                           doDisconnect = 0 ;
                           if (CustomerStamp.maxConnections!=0)
                           {
                              if ( server4numClients( server ) > server->maxUsers )
                              {
                                 #ifdef S4JAVA
                                    SERVER4CLIENT *tempClient;
                                    long numConnections = server4numClients( server ) ;

                                    #ifdef S4OFF_THREAD
                                       for ( tempClient = (SERVER4CLIENT *)l4first( &server->clients ) ; tempClient != NULL ; tempClient=(SERVER4CLIENT *)l4next( &server->clients,tempClient) )
                                    #else
                                       list4mutexWait( &server->clients ) ;
                                       for ( tempClient = (SERVER4CLIENT *)l4first( &server->clients.list ) ; tempClient != NULL ; tempClient=(SERVER4CLIENT *)l4next( &server->clients.list,tempClient) )
                                    #endif
                                       {
                                          if ( tempClient->javaClient )
                                             numConnections-- ;
                                       }
                                    #ifndef S4OFF_THREAD
                                       list4mutexRelease(&server->clients ) ;
                                    #endif
                                    if ( numConnections > server->maxUsers )
                                 #endif
                                    {
                                       doDisconnect = 1 ;
                                       rc = e4max ;
                                       break ;   /* don't continue with initialization */
                                    }
                              }
                           }
                           /* if allow connects is false, disallow this connection */
                           #ifdef S4ODBC_ENABLED
                              unsigned char flag = 0 ;
                              assert5( sizeof( flag ) == SHARE4MEM_ACCEPT_LEN ) ;
                              rc = share4getData( server->odbcSharedMemory, SHARE4MEM_ACCEPT_OFFSET, &flag, SHARE4MEM_ACCEPT_LEN ) ;
                              if ( rc < 0 || flag == 0 )
                           #else
                              if ( server->connectAccept == 0 )
                           #endif
                           {
                              doDisconnect = 1 ;
                              rc = e4connectDenied ;
                              break ;   /* don't continue with initialization */
                           }
                           #ifndef S4OFF_TRAN
                              rc = server4clientTransactionInit( client ) ;
                              if (rc < 0 )
                                 doDisconnect = 1 ;
                           #endif
                        }
                        break ;
                     case CON4CONNECT_ACCEPT_NEW:
                        rc = server4clientConnectAcceptNew( client ) ;
                        break ;
                     case CON4CONNECT_CUT:
                        if ( account4userAllowDisconnect( &client->account ) )
                           rc = server4clientConnectCut( client ) ;
                        else
                           rc = e4authorize ;
                        break ;
                     case CON4CONNECT_CUT_ALL:
                        if ( account4userAllowDisconnect( &client->account ) )
                          rc = server4clientConnectCutAll( client ) ;
                        else
                           rc = e4authorize ;
                        break ;
                     #ifdef S4TESTING
                        case CON4CRASH:   /* for testing, causes a server crash (immediate shutdown) */
                           #ifdef S4WINDOWS
                              u4terminate() ;
                           #else
                              exit( -1 ) ;
                           #endif
                           break ;
                        case MSG4TEST_SEND_RECEIVE:
                           #ifdef TIMER5OUT
                              server4timerStart( c4->server, "send/recv time " )  ;
                           #endif
                           server4clientTestSendReceive( client ) ;
                           #ifdef TIMER5OUT
                              server4timerStop( c4->server ) ;
                           #endif
                           break ;
                     #endif
                     // AS Jun 11/07 - new function to copy a database
                     case CON4COPY:
                        rc = server4clientDataCopy( client ) ;
                        break ;
                     case CON4CREATE:
                        rc = server4clientCreate( client ) ;
                        break ;
                     case STREAM4SWAP_LOGFILE:
                        // AS 09/15/00 - start using 2nd backup log file in place of 1st backup log file
                        rc = server4clientSwapLogFile( client ) ;
                        break ;
                     case CON4DATA_CODEPAGE:
                        rc = server4clientDataFileCodePage( client ) ;
                        break ;
                     // AS May 17/04 - client functionality to copmress the data file...
                     case CON4DATA_COMPRESS:
                        rc = server4clientDataCompress( client ) ;
                        break ;
                     case CON4DATA_FNAME:
                        rc = server4clientDataFileName( client ) ;
                        break ;
                     case CON4DATE_FORMAT:
                        rc = server4clientDateFormatSet( client ) ;
                        break ;
                     case STREAM4SET_SLOW_DELAY:
                        rc = server4clientSetSlowDelay( client ) ;
                        break ;
                     case STREAM4DISCONNECT:
                     case CON4DISCONNECT:   /* no return message since client has disconnected */
                        #ifndef S4OFF_TRAN
                           #ifdef S4DISTRIBUTED
                              if ( client->trans.currentTranStatus == r4partial )
                                 server4clientTransactionCommitPhaseTwo( client ) ;
                              else
                           #endif
                           #ifndef S4OFF_WRITE
                              if ( client->trans.currentTranStatus == r4active )
                                 server4clientTransactionRollback( client ) ;
                           #endif
                        #endif
                        #ifndef S4OFF_LOG
                           if ( c4->logConn > 0 )
                           {
                              char buf[250] ;
                              sprintf( buf, "IS0240:Server is disconnecting client due to STREAM4DISCONNECT or CON4DISCONNECT message for userId: [%s] and ip adress: [%s]",
                                       client->account.accountId, client->account.tcpAddress ) ;
                              assert5( strlen( buf ) < sizeof( buf ) ) ;
                              code4logInfo( c4, buf ) ;
                           }
                        #endif
                        rc = server4disconnect( server, client ) ;
                        if ( rc < 0 && rc > -2000 )
                        {
                           error4set( c4, 0 ) ;
                           rc = 0 ;
                        }
            //            code4exitExclusiveVerifyLast( c4, client ) ;
                        #ifdef S4OFF_THREAD
                           c4->accessMutexCount = 0 ;
                        #endif
                        return rc ;
// AS 01/31/01 - not available for now
//                     case CON4FIELDS_ADD:
//                        rc = server4clientFieldsAdd( client ) ;
//                        break ;
                     // AS Nov 7/03 - Modified d4flush behaviour in client/server to actually perform a physical file flush
                     case CON4FLUSH:
                        rc = server4clientFlush( client ) ;
                        break ;
                     case CON4GO:
                        rc = server4clientGo( client ) ;
                        break ;
                     case CON4INDEX_CLOSE:
                        rc = server4clientCloseIndex( client ) ;
                        break ;
                     case CON4INDEX_CREATE:
                        rc = server4clientCreateIndex( client ) ;
                        break ;
                     case CON4INDEX_FNAME:
                        rc = server4clientIndexFileName( client ) ;
                        break ;
                     case CON4INDEX_FORMAT:
                        rc = server4clientIndexFormat( client ) ;
                        break ;
                     case CON4INDEX_INFO:
                        rc = server4clientIndexInfo( client ) ;
                        break ;
                     case CON4INDEX_OPEN:
                        rc = server4clientOpenIndex( client ) ;
                        break ;
                     case CON4INDEX_REINDEX:
                        rc = server4clientReindexIndex( client ) ;
                        break ;
                     case CON4INFO:
                        rc = server4info( server, client ) ;
                        break ;
                     case CON4LOCK:
                        rc = server4clientLock( client ) ;
                        break ;
                     case CON4LOCK_GROUP:
                        rc = server4clientLockGroup( client ) ;
                        break ;
                     case CON4MEMO:
                        #ifdef S4OFF_MEMO
                           rc = 0 ;
                        #else
                           rc = server4clientMemoRead( client ) ;
                        #endif
                        break ;
                     case CON4MEMO_COMPRESS:
                        #ifdef S4OFF_MEMO
                           rc = 0 ;
                        #else
                           rc = server4clientMemoCompress( client ) ;
                        #endif
                        break ;
                     case CON4MODIFY:
                        rc = server4clientDataModify( client ) ;
                        break ;
                     case CON4OPEN:
                        rc = server4clientOpen( client ) ;
                        break ;
                     case CON4PACK:
                        rc = server4clientPack( client ) ;
                        break ;
                     case CON4PASSWORD_SET:
                        #ifdef S4OFF_SECURITY
                           rc = e4notSupported ;
                        #else
                           rc = server4clientPasswordSet( client ) ;
                        #endif
                        break ;
                     case CON4POSITION:
                        rc = server4clientPosition( client ) ;
                        break ;
                     case CON4POSITION_SET:
                        rc = server4clientPositionSet( client ) ;
                        break ;
                     case CON4RECCOUNT:
                        rc = server4clientReccount( client ) ;
                        break ;
                     case CON4REINDEX:
                        rc = server4clientReindex( client ) ;
                        break ;
                     case CON4RELATE_BOTTOM:
                        rc = server4clientRelateBottom( client ) ;
                        break ;
                     case CON4RELATE_DO:
                        rc = server4clientRelateDo( client ) ;
                        break ;
                     case CON4RELATE_DO_ONE:
                        rc = server4clientRelateDoOne( client ) ;
                        break ;
                     case CON4RELATE_FREE:
                        rc = server4clientRelateFree( client ) ;
                        break ;
                     case CON4RELATE_INIT:
                        rc = server4clientRelateInit( client ) ;
                        break ;
                     case CON4RELATE_LOCK:
                        #ifdef S4OLD_RELATE_LOCK
                           rc = server4clientRelateLock( client ) ;
                           break ;
                        #else
                           rc = e4notSupported ;
                        #endif
                        break ;
                     case CON4RELATE_OPT:
                        rc = server4clientRelateOpt( client ) ;
                        break ;
                     case CON4RELATE_SKIP:
                        rc = server4clientRelateSkip( client ) ;
                        break ;
                     case CON4RELATE_TOP:
                        rc = server4clientRelateTop( client ) ;
                        break ;
                     case CON4REMOVE:
                        rc = server4clientRemove( client ) ;
                        break ;
                     case CON4REMOVE_TAG:
                        rc = server4clientRemoveTag( client ) ;
                        break ;
                     case CON4RESTART:
                        server4clientListReserve( server ) ;
                        if ( server4numClients( server ) == 1 )
                        {
                           server4clientListRelease( server ) ;
                           server4quit( server, r4restart ) ;  /* this function does not return */
                        }
                        else
                           server4clientListRelease( server ) ;
                        rc = 0 ;
                        break ;
                     case CON4ROLLBACK:
                        rc = server4clientTransactionRollback( client ) ;
                        if ( rc == 0 )
                           client->info.numRollbacks++ ;
                        break ;
                     case CON4SEEK:
                        rc = server4clientSeek( client ) ;
                        break ;
                     case CON4SEEK_DBL:
                        rc = server4clientSeekDouble( client ) ;
                        break ;
                     /* AS Sep 7/11 c/s support for seek long long */
                     case CON4SEEK_LONGLONG:
                        rc = server4clientSeekLongLong( client ) ;
                        break ;
                     case CON4SERVER_OS:
                        rc = server4clientServerOS( client ) ;
                        break ;
                     case CON4SHUTDOWN:
                        if ( account4userAllowDisconnect( &client->account ) )
                        {
                           rc = 0 ;
                           server4quit( server, r4shutdown ) ;  /* this function does not return */
                        }
                        else
                           rc = e4authorize ;
                        break ;
                     case CON4SKIP:
                        rc = server4clientSkip( client ) ;
                        break ;
                     case CON4START:
                        rc = server4clientTransactionStart( client ) ;
                        if ( rc == 0 )
                           client->info.numTransactions++ ;
                        break ;
                     case CON4TAG_BOTTOM:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagBottom( client ) ;
                        break ;
                     case CON4TAG_CACHE_SKIP:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagCacheSkip( client ) ;
                        break ;
                     case CON4TAG_COUNT:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagCount( client ) ;
                        break ;
                     case CON4TAG_FNAME:
                        rc = server4clientTagFileName( client ) ;
                        break ;
                     case CON4TAG_OPEN:
                        #ifdef S4CLIPPER
                           rc = server4clientTagOpen( client ) ;
                        #else
                           rc = e4notSupported ;
                        #endif
                        break ;
                     // case CON4TAG_POSITION:  /* AS Oct 25/05 - support for tag functionality in client/server */
                     //    rc = server4clientTagPosition( client ) ;
                     //    break ;
                     // case CON4TAG_POSITION_SET:  /* AS Oct 25/05 - support for tag functionality in client/server */
                     //    rc = server4clientTagPosition( client ) ;
                     //    break ;
                     case CON4TAG_SEEK:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagSeek( client ) ;
                        break ;
                     case CON4TAG_EOF:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagEof( client ) ;
                        break ;
                     case CON4TAG_GO:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagGo( client ) ;
                        break ;
                     case CON4TAG_SKIP:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagSkip( client ) ;
                        break ;
                     case CON4TAG_POSITION:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagPosition( client ) ;
                        break ;
                     case CON4TAG_POSITION_SET:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagPositionSet( client ) ;
                        break ;
                     case CON4TAG_SYNCH:
                        rc = server4clientTagSync( client ) ;
                        break ;
                     case CON4TAG_EXPR_KEY:
                        rc = server4clientTagExprKey( client ) ;
                        break ;
                     // case CON4TAG_KEY:  // AS Feb 9/09 - replaced functionality with code to get key every time instead
                     //    rc = server4clientTagKey( client ) ;
                     //    break ;
                     case CON4TAG_TOP:  /* AS Oct 25/05 - support for tag functionality in client/server */
                        rc = server4clientTagTop( client ) ;
                        break ;
                     case CON4TOP:
                        rc = server4clientTop( client ) ;
                        break ;
                     case CON4TRAN_EOF:
                        #ifdef S4OFF_TRAN
                           rc = e4notSupported ;
                        #else
                           rc = server4clientTranEof( client ) ;
                        #endif
                        break ;
                     case CON4TRAN_EOF_HALT:
                        #ifdef S4OFF_TRAN
                           rc = e4notSupported ;
                        #else
                           rc = server4clientTranEofHalt( client, name, sizeof( name ) ) ;
                        #endif
                        break ;
                     case CON4UNIQUE_SET:
                        rc = server4clientUniqueSet( client ) ;
                        break ;
                     case CON4UNLOCK:
                        server4clientUnlock( client ) ;
                        break ;
                     // AS Apr 15/03 - support for unlock append by client
                     case CON4UNLOCK_APPEND:
                        server4clientUnlockAppend( client ) ;
                        break ;
                     // AS Aug 16/01 - new functionality - see if data file has changed
                     case CON4VERSION:
                        rc = server4clientVersion( client ) ;
                        break ;
                     case CON4WRITE:
                        rc = server4clientWrite( client ) ;
                        break ;
                     case CON4ZAP:
                        rc = server4clientZap( client ) ;
                        break ;

                     #ifndef S4OFF_BLAST
                        case STREAM4BLAST_TEST_READ:
                           rc = server4clientBlastTestRead( client ) ;
                           break ;
                        case STREAM4BLAST_TEST_WRITE:
                           rc = server4clientBlastTestWrite( client ) ;
                           break ;
                     #endif
                     case STREAM4CURRENT_DIRECTORY:
                        rc = server4clientCurrentDirectory( client ) ;
                        break ;
                     case STREAM4DIRECTORY:
                        server4clientDirectory( client ) ;
                        break ;
                     case STREAM4PING:
                        rc = server4clientPing( client ) ;
                        break ;
                     case STREAM4RECONNECT:
                        // this is the old connection setup whereby the server
                        // connects back to the client
                        rc = server4clientConnect( client ) ;
                        if ( rc )  // reconnect failed - close client down
                        {
                           #ifndef S4OFF_LOG
                              if ( c4->logConn > 0 )
                              {
                                 char buf[250] ;
                                 sprintf( buf, "IS0250:Server is disconnecting client due to failed reconnect for userId: [%s] and ip adress: [%s]",
                                          client->account.accountId, client->account.tcpAddress ) ;
                                 assert5( strlen( buf ) < sizeof( buf ) ) ;
                                 code4logInfo( c4, buf ) ;
                              }
                           #endif
                           server4disconnect( server, client ) ;
                        }
                        break ;
                     case STREAM4RECONNECT_NEW:
                        // this is the new connection setup whereby the client
                        // connects twice to the server
                        rc = server4clientConnectNew( client ) ;
                        if ( rc )  // reconnect failed - close client down
                           server4disconnect( server, client ) ;
                        break ;
                     case STREAM4STATUS:
                        rc = server4clientStatus( client ) ;
                        break ;
                     case STREAM4TABLES:
                        rc = server4clientTables( client ) ;
                        break ;

                     case STREAM4CLIENT_PIPE_RECV:
                        rc = server4clientReceive( client ) ;
                        break ;
                     case STREAM4CLIENT_PIPE_SEND:
                        rc = server4clientSend( client ) ;
                        break ;
//                     case STREAM4CLIENT_PIPE_CLOSE_SEND:
//                        rc = server4clientCloseSend( client ) ;
//                        break ;
                     case STREAM4CLIENT_PIPE_CLOSE_RECV:
                        rc = server4clientCloseRecv( client ) ;
                        break ;
                     case STREAM4CLIENT_PIPE_OPEN_SEND:
                        rc = server4clientOpenSend( client ) ;
                        break ;
                     case STREAM4CLIENT_PIPE_OPEN_RECV:
                        rc = server4clientOpenRecv( client ) ;
                        break ;
                     case STREAM4CLIENT_PIPE_SEND_CHECK:
                        rc = server4clientSendCheck( client ) ;
                        break ;
                     case STREAM4CLIENT_PIPE_SEND_CANCEL:
                        rc = server4clientSendCancel( client ) ;
                        break ;

                     #ifdef OLEDB5BUILD
                        case MSG5DB_ADD_COLUMN:
                           server4clientMsgDataAddColumn( client ) ;
                           break ;
                        case MSG5DB_FETCHNEXTRECNO:
                           server4clientMsgDataFetchNextRecno( client ) ;
                           break ;
                        case MSG5DB_SCHEMA_REQUEST_DIRECT:
                           server4clientMsgDataSchemaRequestDirect( client ) ;
                           break ;
                        case MSG5DB_ADD_DIRECTORY:
                           server4clientMsgAddDirectory( client ) ;
                           break ;
                        case MSG5DB_ADD_TAG:
                           server4clientMsgAddTag( client ) ;
                           break ;
                        case MSG5DB_COLUMN_INFO:
                           #ifdef TIMER5OUT
                              server4timerStart( c4->server, "column info " )  ;
                           #endif
                           server4clientMsgColumnInfo( client ) ;
                           #ifdef TIMER5OUT
                              server4timerStop( c4->server ) ;
                           #endif
                           break ;
                        case MSG5DB_TAG_SELECT:
                           server4clientMsgTagSelect( client ) ;
                           break ;
                        case MSG5DB_CURRENT_DIRECTORY:
                           server4clientMsgCurrentDirectory( client ) ;
                           break ;
                        case MSG5DB_FIND_OR_ADD_ENTRY:
                           server4clientMsgFindOrAddEntry( client ) ;
                           break ;
                        case MSG5DB_INDEX_COLUMN_INFO:
                           server4clientMsgIndexColumnInfo( client ) ;
                           break ;
                        case MSG5DB_INDEX_REMOVE:
                           server4clientMsgIndexRemove( client ) ;
                           break ;
                        case MSG5DB_INDEX_REQUEST_KEY:
                           server4clientMsgIndexRequestKey( client ) ;
                           break ;
                        case MSG5DB_INDEX_ROW_REQUEST:
                           server4clientMsgIndexRowRequest( client, 0 ) ;
                           break ;
                        case MSG5DB_INDEX_ROW_REQUEST_KEYS:
                           server4clientMsgIndexRowRequest( client, 1 ) ;
                           break ;
                        case MSG5DB_INDEX_DESTROY_RANGE:
                           server4clientMsgIndexDestroyRange( client ) ;
                           break ;
                        case MSG5DB_INDEX_SET_RANGE:
                           server4clientMsgIndexSetRange( client ) ;
                           break ;
            //            case MSG5DB_INDEX_UPDATE_FIELDSET:
            //               server4clientMsgIndexUpdateFieldset( client ) ;
            //               break ;
                        case MSG5LEN:
                           /* special length verify check available if both client and server compiled with E4DEBUG */
                           /* Apparently MSG5LEN is only passed if client in E4DEBUG */
                           #ifdef E4DEBUG
                              client->lastCommandWasLen = 1 ;
                              client->commandLen = connect4receiveLong( &client->connect ) ;
                           #else
                              connect4receiveLong( &client->connect ) ;  /* still read it off */
                           #endif
                           break ;
                        case  MSG5DB_RECCOUNT:
                           server4clientMsgReccount( client ) ;
                           break ;
                        case MSG5DB_REMOVE_COLUMN:
                           server4clientMsgDataRemoveColumn( client ) ;
                           break ;
                        case MSG5DB_REQUEST_DEFERRED_FIELD:
                           server4clientMsgRequestDeferredField( client ) ;
                           break ;
                        case MSG5DB_ROW_REQUEST_ARRAY:
                           server4clientMsgRowRequestArr( client ) ;
                           break ;
                        case MSG5DB_ROW_REQUEST_SEQUENTIAL:
                           #ifdef TIMER5OUT
                              server4timerStart( c4->server, "row request sequential " )  ;
                           #endif
                           server4clientMsgRowRequestSeq( client ) ;
                           #ifdef TIMER5OUT
                              server4timerStop( c4->server ) ;
                           #endif
                           break ;
                        case MSG5DB_ROWSET_DESTROY:
                           server4clientMsgRowsetDestroy( client ) ;
                           break ;
                        case MSG5DB_SCHEMA_REQUEST_SEQ:
                           server4clientMsgSchemaRequestSeq( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_CLOSE:
                           server4clientMsgDataClose( client ) ;
                           break ;
                        case MSG5DB_SESSION_INDEX_CLOSE:
                           server4clientMsgSessionIndexClose( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_CREATE:
                           server4clientMsgSessionDataCreate( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_DEFAULTS:
                           server4clientMsgSessionDefaults( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_DELETE:
                           server4clientMsgSessionDelete( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_OPEN:
                           server4clientMsgSessionDataOpen( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_SEEK:
                           server4clientMsgSessionSeek( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_WRITE:
                           server4clientMsgSessionWrite( client ) ;
                           break ;
                        case MSG5DB_SESSION_DATA_WRITE_DONE:
                           server4clientMsgSessionWriteDone( client ) ;
                           break ;
                        case MSG5DB_SESSION_INDEX_OPEN:
                           server4clientMsgSessionIndexOpen( client ) ;
                           break ;
                        case MSG5DB_SESSION_IN_TRANSACTION:
                           server4clientMsgInTransaction( client ) ;
                           break ;
                        case MSG5DB_SESSION_ISO_LEVEL:
                           server4clientMsgIsoLevel( client ) ;
                           break ;
                        case MSG5DB_SESSION_SCHEMA_OPEN:
                           server4clientMsgSessionSchemaOpen( client ) ;
                           break ;
                        case MSG5DB_SESSION_UNLOCK_ALL:
                           server4clientMsgSessionUnlockAll( client ) ;
                           break ;
                        case MSG5DB_SET_RESTRICTIONS:
                           server4clientMsgSetRestrictions( client ) ;
                           break ;
                        case MSG5DB_INTEGRATED_TAG_SELECT:
                           server4clientMsgIntegratedTagSelect( client ) ;
                           break ;
         //               case MSG5DB_INTEGRATED_TAG_EXISTS:
         //                  server4clientMsgIntegratedTagExists( client ) ;
         //                  break ;
                        case MSG5DB_INTEGRATED_ROW_REQUEST:
                           server4clientMsgIntegratedRowRequest( client ) ;
                           break ;
                        case MSG5DB_TBL_REMOVE:
                           server4clientMsgTableRemove( client ) ;
                           break ;
                        case MSG5DB_TBL_REMOVE_INDEXES:
                           server4clientMsgTableRemoveIndexes( client ) ;
                           break ;
                        case MSG5DB_TBL_WRITE_RESULT_INC:
                           server4clientMsgTableWriteResultInc( client ) ;
                           break ;
                        case MSG5DB_UPDATE_FIELDSET:
                           server4clientMsgUpdateFieldset( client ) ;
                           break ;
                        case MSG5DB_SCHEMA_POPULATE_INDEXES:
                           server4clientMsgSchemaPopulateIndexes( client ) ;
                           break ;
                        case MSG5DB_SCHEMA_POPULATE_COLUMNS:
                           server4clientMsgSchemaPopulateColumns( client ) ;
                           break ;
                     #endif
                     case MSG5SKIP:  // new version of skip that supports batch reading
                        rc = server4clientMsgSkip( client ) ;
                        break ;
                     case MSG5RELATE_SKIP_MULTI:   // new relate skip for batch reading
                        rc = server4clientMsgRelateSkipMulti( client ) ;
                        break ;
                     case MSG5WRITE_BATCH:   // new code for batch writing
                        rc = server4clientMsgWriteBatch( client ) ;
                        break ;
                     assert5port( "Added new functions" ) ;
                     case MSG5COMPRESS:  // AS May 29/02 new code to configure compression
                        rc = server4clientMsgCompress( client ) ;
                        break ;
                     case MSG5PREPROCESS_B:
                        rc = server4clientMsgPreprocessB( client ) ;
                        break ;
                     // #ifdef S4PREPROCESS_COM
                        case MSG5PREPROCESS_DISABLE:
                           rc = server4clientMsgPreprocessED( client ) ;
                           break ;
                        // case MSG5PREPROCESS_P:
                        //    rc = server4clientMsgPreprocessEP( client ) ;
                        //    break ;
                        // case MSG5PREPROCESS_PK:
                        //    rc = server4clientMsgPreprocessPK( client ) ;
                        //    break ;
                     // #endif
                     case MSG5ADD_FUNC:  // AS Sept. 3/02 - new functionality
                        rc = server4clientAdditionalFunction( client ) ;
                        break ;
                     case MSG5RELATE_COUNT:  // AS Oct 10/02 - client/server relate4count
                        rc = server4clientRelateCount( client ) ;
                        break ;
                     default:
                        rc = e4message ;
                  }
            #ifdef OLEDB5BUILD
               }
                  catch( Err5 & )
                  {
                     // this means the server produced an error which was not caught elsewhere.  This is
                     // basically a critical situation for the current client, so just disconnect it...
                     // AS 02/01/00 -- if client != currentClient, it means an exception ocurred during
                     // instance where currentClient was probably set to catalogClient.  This causes
                     // problems, and should have been caught and fixed as appropriate...
                     assert5( c4->currentClient == client ) ;
                     #ifndef S4OFF_LOG
                        if ( c4->logConn > 0 )
                        {
                           char buf[250] ;
                           sprintf( buf, "ES0260:Server is disconnecting client due to Exception Fault in Server for userId: [%s] and ip adress: [%s]",
                                    client->account.accountId, client->account.tcpAddress ) ;
                           assert5( strlen( buf ) < sizeof( buf ) ) ;
                           code4logInfo( c4, buf ) ;
                        }
                     #endif
                     server4disconnect( server, client ) ;
                     #ifdef S4OFF_THREAD
                        c4->accessMutexCount = 0 ;
                     #endif
                     // AS Sep 5/03 - To help recovery, return e4badClient, to avoid accessing the disconnected client object
                     return e4badClient ;
                  }
            #endif
         }

         #ifdef E4DEBUG
            if ( actionType != MSG5LEN )
               client->lastCommandWasLen = 0 ;

            // AS Sept 29/04 - is ok if > 0 as well...was happening in some instances
            assert5( server->c4->catalogClient->errorCode >= 0 ) ;

            if ( server4clientVerifyData4s( client ) != 0 )  // data's bad somehow, this is a severe error, disconnect client
            {
               #ifndef S4OFF_LOG
                  if ( c4->logConn > 0 )
                  {
                     char buf[250] ;
                     sprintf( buf, "ES0300:Server is disconnecting client due to invalid DATA4 for userId: [%s] and ip adress: [%s]",
                                   client->account.accountId, client->account.tcpAddress ) ;
                     assert5( strlen( buf ) < sizeof( buf ) ) ;
                     code4logInfo( c4, buf ) ;
                  }
               #endif
               server4disconnect( server, client ) ;
               #ifdef S4OFF_THREAD
                  c4->accessMutexCount = 0 ;
               #endif
               return -1 ;  // AS Sep 3/03 - We need to return an error code in this case to properly handle code backwards.
            }
         #endif

         if ( actionType >= STREAM4START_COUNT ) /* STREAM4 and MSG4 messages do not return a status to the caller */
         {
            if ( error4code( c4 ) == e4connect || rc == e4message )  // connection has gone bad - disconnect client
            {
               #ifndef S4OFF_LOG
                  if ( c4->logConn > 0 )
                  {
                     char buf[250] ;
                     sprintf( buf, "ES0320:Server is disconnecting client due to rc of %ld or errorCode of %ld for userId: [%s] and ip adress: [%s]",
                                   (long)rc, (long)error4code( c4 ), client->account.accountId, client->account.tcpAddress ) ;
                     assert5( strlen( buf ) < sizeof( buf ) ) ;
                     code4logInfo( c4, buf ) ;
                  }
               #endif
               server4disconnect( server, client ) ;
            }
            #ifdef S4OFF_THREAD
               c4->accessMutexCount = 0 ;
            #endif
            return 0 ;
         }

         c4->currentRelation = NULL ;  /* reset... */

         /* return status to user */
         if ( error4code( c4 ) != 0 && rc == 0 )
            rc = -1 ;

         if ( rc < 0 )
         {
            /* clear error return, and send result back to client, then continue */
            saveError = error4set( c4, 0 ) ;

            if ( saveError == 0 )   /* case where a return code error */
               saveError = rc ;

            if ( saveError <= -2100 || saveError == e4memory )   /* tell client it is a server error */
               sendError = e4server ;
            else
               sendError = saveError ;

            connection4setStatus( connection, sendError ) ;
            saveRc = connection4sendMessage( connection ) ;

            if ( saveError <= -2100 || saveError == e4memory )  /* server error, so re-set */
            {
               error4set( c4, (short)saveError ) ;
               saveRc = saveError ;
            }
         }
         else
         {
            connection4setStatus( connection, rc ) ;

            if ( rc == r4locked && client->isStream == 0 )
            {
               /* send pre-message --> the locked info */
               connect4sendShort( connect, STREAM4LOCKED_INFO ) ;
               connect4sendLong( connect, server->lockedLockItem ) ;

               if ( server->lockedFileName == NULL )
                  len = 0 ;
               else
                  len = strlen( server->lockedFileName ) + 1 ;

               connect4sendShort( connect, len ) ;
               connect4send( connect, server->lockedFileName, len ) ;

               if ( server->lockedUserName == NULL )
                  len = 0 ;
               else
                  len = strlen( server->lockedUserName ) + 1 ;

               connect4sendShort( connect, len ) ;
               connect4send( connect, server->lockedUserName, len ) ;

               if ( server->lockedNetName == NULL )
                  len = 0 ;
               else
                  len = strlen( server->lockedNetName ) + 1 ;

               connect4sendShort( connect, len ) ;
               connect4send( connect, server->lockedNetName, len ) ;
               saveRc = connect4sendFlush( connect ) ;
            }

            saveRc = connection4sendMessage( connection ) ;
         }
         error4set2( c4, 0 ) ;

         #ifndef S4OFF_TRAN
            if ( actionType == CON4TRAN_EOF_HALT && rc == 0 )  // acts on primary transaction file only
            {
               for ( i = 0 ; i < 10 ; i++ )    /* give 10 seconds for backup to start */
               {
                  u4delaySec() ;
               }

               oldAccessMode = c4->accessMode ;
               c4->accessMode = OPEN4DENY_RW ;
               if ( error4code( c4 ) < 0 )
               {
                  // code4exitExclusiveVerifyLast( c4, client ) ;
                  #ifdef S4OFF_THREAD
                     c4->accessMutexCount = 0 ;
                  #endif
                  return e4codeBase ;
               }
               if ( saveRc == 0 )
                  saveRc = error4set( c4, 0 ) ;
               for ( ;; )
               {
                  u4delaySec() ;
                  // AS Mar 17/03 - should be using internal open...
                  rc = file4openInternal( &server->transFile.primary.file, c4, name, 1, OPT4NONE ) ;
                  if ( rc == 0 )
                  {
                     file4close( &server->transFile.primary.file ) ;
                     c4->accessMode = OPEN4DENY_NONE ;
                     rc = file4openInternal( &server->transFile.primary.file, c4, name, 1, OPT4NONE ) ;
                     c4->accessMode = oldAccessMode ;
                     if ( rc == 0 )
                     {
                        error4set( c4, saveRc ) ;
                        rc = server4transactionLock( server ) ;
                        if ( rc == 0 )
                           break ;
                        file4close( &server->transFile.primary.file ) ;
                     }
                     error4set( c4, 0 ) ;
                     c4->accessMode = OPEN4DENY_RW ;
                  }
               }
               c4->accessMode = oldAccessMode ;
            }
         #endif

         if ( actionType == CON4CONNECT && doDisconnect == 1 )
         {
            #ifndef S4OFF_LOG
               if ( c4->logConn > 0 )
               {
                  char buf[250] ;
                  sprintf( buf, "IS0330:Server is disconnecting client due to actionType of CON4CONNECT and doDisconnect of 1 for userId: [%s] and ip adress: [%s]",
                           client->account.accountId, client->account.tcpAddress ) ;
                  assert5( strlen( buf ) < sizeof( buf ) ) ;
                  code4logInfo( c4, buf ) ;
               }
            #endif
            server4disconnect( server, client ) ;
            // AS Nov 18/02 - if saveRc == 0 in this case, we need to send back that
            // we disconnected the client to the worker thread
            if ( saveRc == 0 )
               saveRc = saveError ;  // tell caller we disconnected
         }
         #ifdef S4OFF_THREAD
            c4->accessMutexCount = 0 ;
         #endif
         return saveRc ;
      }
   #endif /* not S4OFF_COMMUNICATIONS */



   int server4closeAll( SERVER4 *server )
   {
      CODE4 *c4 = server->c4 ;
      int saveRc = 0 ;

      server4clientListReserve( server ) ;

      SERVER4CLIENT *clientOn ;
      for ( clientOn = server4clientGetFirst( server ) ; ; )
      {
         if ( clientOn == 0 )
            break ;

         int rc = tran4closeAll( &clientOn->trans ) ;

         if ( rc < 0 )
            saveRc = rc ;

         clientOn = server4clientGetNext( server, clientOn ) ;
      }

      server4clientListRelease( server ) ;

      if ( c4 != 0 )
      {
         // AS 05/04/99 - previously was not closing all files as expected, was returning instead...changed...
         if ( error4code( c4 ) < 0 && saveRc == 0 )
            saveRc = error4set( c4, 0 ) ;

         #ifndef S4OFF_CATALOG
            if ( c4->catalog != 0 )
               if ( c4->catalog->data != 0 )
               {
                  cat4close( c4->catalog ) ;
                  c4->catalog = 0 ;
               }
         #endif
      }

      // close all kept open tables (except validation table) prior to uninitializing validation table
      if ( c4 != 0 )
         code4dataFileCloseAll( c4 ) ;

      // AS Aug 01/02 - ensure that server->c4 set to zero first in case error occurs which would
      // otherwise use it...
      // AS Jan 1/03 - ensure server does not keep the validation table open
      int oldKeepOpen = server->keepOpen ;
      server->keepOpen = 0 ;
      code4validateUndo( c4 ) ;
      server->keepOpen = oldKeepOpen ;

      return saveRc ;
   }



   void S4FUNCTION server4exit( SERVER4 *server )
   {
      if ( server->c4 != 0 )
         error4set( server->c4, 0 ) ;

      server4initUndo( server ) ;
      server = 0 ;
   }



   int S4FUNCTION server4exitTest( SERVER4 *server )
   {
      if ( server == 0 )
         return r4exit ;

      if ( server->c4 == 0 )
      {
         server4exit( server ) ;
         return r4exit ;
      }

      if ( error4code( server->c4 ) < 0 )
      {
         server4exit( server ) ;
         return r4exit ;
      }

      return 0 ;
   }



   #ifndef S4OFF_COMMUNICATIONS
      int server4info( SERVER4 *server, SERVER4CLIENT *client )
      {
         CONNECTION4 *connection = &client->connection ;

         time_t tm ;
         time( &tm ) ;

         connection4clear( connection ) ;
         CONNECTION4SERVER_INFO_OUT *out ;
         connection4addData( connection, NULL, sizeof( CONNECTION4SERVER_INFO_OUT ), (void **)&out ) ;

         #ifdef S4MAX
            out->memMax = htonl5(u4max()) ;
         #else
            out->memMax = -1 ; /* No conversion because -1 is always 0xFFFFFFFF */
         #endif

         out->memAlloc = htonl5(u4allocated()) ;
         out->nOpenFiles = htons5( u4openFiles() ) ;

         out->numRequests = htonl5( server->info.numRequests ) ;
         double elapsedSeconds = difftime( tm, server->info.serverStart ) ;
         out->elapsedSeconds = htonl5( (long)elapsedSeconds ) ;

         server4clientListReserve( server ) ;
         out->numClients = htons5( (short)server4numClients( server ) ) ;

         #if defined( S4ODBC_ENABLED )
            // also include the count of OLE-DB clients...
            long numClientsOdbc = odbc4getClientCount() ;
            if ( numClientsOdbc >= 0 )   // just ignore if negative, maybe odbc engine down
            {
               assert5( numClientsOdbc <= USHRT_MAX ) ;
               out->numClientsOdbc = (unsigned short)numClientsOdbc ;
            }
         #else
            out->numClientsOdbc = 0 ;
         #endif

         SERVER4CLIENT *clientOn ;
         for ( clientOn = 0 ;; )
         {
            clientOn = server4clientGetNext( server, clientOn ) ;

            if ( clientOn == 0 )
               break ;

            CONNECTION4CLIENT_INFO *clientInfo ;
            connection4addData( connection, NULL, sizeof( CONNECTION4CLIENT_INFO ), (void **)&clientInfo ) ;
            clientInfo->numRequests = htonl5( clientOn->info.numRequests ) ;
            clientInfo->numTransactions = htonl5( clientOn->info.numTransactions ) ;
            clientInfo->numCompletedTransactions = htonl5( clientOn->info.numCompletedTransactions ) ;
            clientInfo->numRollbacks = htonl5( clientOn->info.numRollbacks ) ;

            #ifndef S4OFF_TRAN
               #ifdef S4OFF_WRITE
                  clientInfo->activeTransaction = htons5( r4inactive ) ;
               #else
                  clientInfo->activeTransaction = htons5((short)code4tranStatus( server->c4 )) ;
               #endif
            #endif

            elapsedSeconds = difftime( tm, clientOn->info.connectionStart ) ;
            clientInfo->elapsedSeconds = htonl5((long)elapsedSeconds) ;

            DATA4 *dataOn ;
            for ( dataOn = 0 ;; )
            {
               dataOn = (DATA4 *)l4next( tran4dataList( &client->trans ), dataOn ) ;

               if ( dataOn == 0 )
                  break ;

               clientInfo->numData++ ;
            }

            clientInfo->numData = htons5(clientInfo->numData) ;
         }

         server4clientListRelease( server ) ;

         return 0 ;
      }
   #endif /* S4OFF_COMMUNICATIONS */



   #ifndef S4WINDOWS
      static void e4errorOut( const char *ptr )
      {
         #ifdef S4UNIX
            printf("%s", ptr ) ;
         #else
            #if defined( S4TESTING ) && !defined( S4OFF_PRINT ) && !defined( S4WINDOWS )
               fprintf(stdprn, "%s", ptr ) ;
            #endif
            write( 1, ptr, (unsigned int) strlen(ptr) ) ;
         #endif
      }



      static void display( int err_code )
      {
         char buf[11] ;
         int i ;

         c4ltoa45( (long) err_code, buf, 6 ) ;
         buf[6] = 0 ;
         e4errorOut( buf ) ;

         for ( i = 0; e4errorData[i].errorData != 0; i++ )
         {
            if ( e4errorData[i].errorNum == err_code )
            {
               e4errorOut( "\r\n" ) ;
               e4errorOut( e4errorData[i].errorData ) ;
               break ;
            }
         }
      }



      int S4FUNCTION e4out( CODE4 *c4, const int err_code, const char *desc1,const  char *desc2, const char *desc3 )
      {
         char const *ptr ;
         int desc_number = 1 ;

         if ( c4 != 0 )
            error4set( c4, err_code ) ;

         #ifndef S4OFF_ERROR
            if ( c4 != 0 )
               if ( c4->errOff )
                  return err_code ;

            e4errorOut( E4_ERROR_NUM ) ;
            display ( err_code ) ;

            ptr = desc1 ;
            while ( (ptr != (char *)0) && (desc_number <= 3 ) )
            {
               e4errorOut( "\r\n" ) ;
               e4errorOut( ptr ) ;
               if ( desc_number++ == 1 )
                  ptr = desc2 ;
               else
                  ptr = desc3 ;
            }

            #ifdef S4UNIX
               e4errorOut( E4_ERROR_ENT ) ;
               getchar() ;
            #else
               e4errorOut( E4_ERROR_KEY ) ;
               #ifndef S4TESTING
                  getch() ;
               #endif
            #endif
         #endif

         return err_code ;
      }
   #endif



   static void server4errorOut( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
   {
      /* for the server, esp in multi-thread, need to disallow all the other worker
         threads from continuing if a server error is output, since server will
         go down after acknowledgement */
      #ifdef S4COMTHREADS
          if ( c4 != 0 )
            c4->shutdownWorkerThreads = 1 ;  // tell worker threads to stop servicing clients
      #endif
      error4out( c4, errCode1, errCode2, desc1, desc2, desc3 ) ;
   }



   void S4FUNCTION error4hook( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
   {
      /* log the error in the error log ??? */
      /* if it is a server error, then respond with error display */
      /* otherwise simply return */

      // #ifdef S4ODBC_BUILD
      //    log4invokeDebugger() ;
      // #endif

      // AS July 29/02 - we may be able to get a c4 from the gloabl server ptr...
      if ( c4 == 0 )
      {
         if ( g_server != 0 && g_server->c4 != 0 )
            c4 = g_server->c4 ;
      }

      if ( c4 != 0 )
      {
         // AS Sep 30/03 - if c4->errorLog == 0, just open s4server.err for error reporting - otherwise at least
         // with service we can miss configuration errors
         Bool5 openedLog = 0 ;
         if ( c4->errorLog == 0 )
         {
            static int inErrorSetup = 0 ;
            if ( inErrorSetup == 0 )  // don't recurse into this area in case an error occurs
            {
               inErrorSetup = 1 ;
               char errName[LEN4PATH] ;
               char defErrorName[] = "S4SERVER.err" ;
               c4->errorLog = (FILE4 *)u4allocEr( c4, sizeof( FILE4 ) ) ;
               if ( c4->errorLog != 0 )
               {
                  c4->errorLog->hand = INVALID4HANDLE ;  /* in case of error in log open */
                  if (  c4->server == 0 )   // AS Nov 19/08 if the server is not set up yet we cannot use it to get the error log (i.e. if a failure during server setup)
                     strcpy( errName, defErrorName ) ;
                  else
                     u4nameCurrentExtended( errName, sizeof( errName ), defErrorName, c4->server->systemPath ) ;
                  int oldOpenError = c4->errOpen ;
                  c4->errOpen = 0 ;
                  int rc = file4openInternal( c4->errorLog, c4, errName, 1, OPT4NONE ) ;
                  c4->errOpen = oldOpenError ;
                  if ( rc == r4noExist )
                     rc = file4createInternal( c4->errorLog, c4, errName, 1, OPT4NONE ) ;
                  if ( rc != 0 )
                  {
                     u4free( c4->errorLog ) ;
                  }
                  else
                     openedLog = 1 ;
               }
               inErrorSetup = 0 ;
            }
         }
         // AS Nov 6/02 - Check the errOff setting as well... - required for preprocess to avoid errors
         if ( c4->errorLog != 0 && c4->errOff != 1 )
         {
            if ( c4->errorLog->hand != INVALID4HANDLE  )
               error4logAppend( c4, errCode1, errCode2, desc1, desc2, desc3 ) ;
         }

         if ( openedLog == 1 )
         {
            u4free( c4->errorLog ) ;
         }

         error4set( c4, errCode1 ) ;

         if ( error4code2( c4 ) == 0 )
            error4set2( c4, errCode2 ) ;

         // AS Nov 6/02 - Check the errOff setting as well... - required for preprocess to avoid errors
         if ( c4->errOff == 1 )
            return ;

         if ( c4->displayErrors == 1 )   /* a server error */
         {
            server4errorOut( c4, errCode1, errCode2, desc1, desc2, desc3 ) ;
            if ( c4 != NULL )
               server4setDoExit( c4->server, r4exit ) ;
            return ;
         }
      }

      if ( errCode1 <= -2100 || errCode1 == e4version )   /* a server error or version mismatch */
      {
         server4errorOut( c4, errCode1, errCode2, desc1, desc2, desc3 ) ;
         if ( c4 != NULL )
            server4setDoExit( c4->server, r4exit ) ;
         return ;
      }

      switch( errCode1 )
      {
         case e4info:
         case e4memory:
         case e4parm:
         case e4parm_null:
         case e4opt:
         case e4optSuspend:
         // AS 02/27/01 - e4optFlush is not necessarily critical - in particular it is ok
         // when we lose the 'backup' log file...  just stops using it
         // case e4optFlush:
         case e4demo:
         case e4result:
         // case e4connection:
         case e4socket:
         case e4notSupported:
         case e4loadlib:
         #ifdef E4ANALYZE_ALL
            case e4verify:
         #endif
            server4errorOut( c4, errCode1, errCode2, desc1, desc2, desc3 ) ;
            if ( c4 != NULL )
               server4setDoExit( c4->server, r4exit ) ;
            break ;
         default:
            break ;
      }

      return ;
   }



   void S4FUNCTION server4setDoExit( SERVER4 S4PTR *server, int value )
   {
      #ifdef S4OLEDEBUG_PRINT
        log5( "server4setDoExit() called\r\n" ) ;
      #endif
      if ( server == 0 )   // must have already go this far in exiting
         return ;
      server->doExit = value ;
      #ifdef S4COMTHREADS
         event4set( &server->quitExitServerEvent ) ;
      #endif
   }



   int S4FUNCTION server4getDoExit( SERVER4 S4PTR *server )
   {
      return server->doExit ;
   }



   #ifdef S4COMTHREADS
      int S4FUNCTION server4waitOnQuitOrExit( SERVER4 *server )
      {
         // just wait on event to see if quit or exit is desired
         event4wait( &server->quitExitServerEvent, 1 ) ;
         #ifdef S4OLEDEBUG_PRINT
           log5( "server4waitOnQuitOrExit() has been activated\r\n" ) ;
         #endif

         // delay to ensure the client app get's its response back first before a failed connection
         // wait 2 seconds
         u4delaySec() ;
         u4delaySec() ;

         if ( server4getDoExit( server ) == r4exit )
         {
            #ifdef S4OLEDEBUG_PRINT
              log5( "server4waitOnQuitOrExit() value r4exit, so just returning r4exit\r\n" ) ;
            #endif
            return r4exit ;
         }

         if ( server4quit( server, -1 ) > 0 )
         {
            if ( server4quit( server, -1 ) == r4restart )
               return r4restart ;
            else  // must be r4shutdown
               return r4shutdown ;  // ensure we dont restart
         }

         // if got here, must be an error
         return error4( server->c4, e4server, E70102 ) ;
      }
   #endif



   long S4FUNCTION server4maxSockets( SERVER4 *server )
   {
      /* must be exported because used in d4main.c (server executeable) */
      return server->c4->maxSockets ;
   }



   int S4FUNCTION server4keepOpen(SERVER4 S4PTR * server, int keepOpen)
   {
      /* function will set the keepOpen flag and return the old value */
      int temp = server->keepOpen ;
      server->keepOpen = keepOpen ;
      return (temp);
   }



   char *S4FUNCTION server4systemPath( SERVER4 S4PTR *server)
   {
      return(server->systemPath);
   }



   SERVER4CLIENT S4PTR *S4FUNCTION c4CatalogClient( CODE4 S4PTR * c4 )
   {
      return( c4->catalogClient ) ;
   }



   /*
   int code4lockHook( CODE4 *c4, char *fileName, char *userId, char *netId, long lockItem, int numAttempts )
   {
      CONNECTION4 *connection ;

      connection = c4->currentClient->connection ;
      if ( connection != 0 )
         if ( connection4requestLockedInfo( connection ) == 1 )
         {
            c4->server->lockedFileName = fileName ;
            c4->server->lockedUserId = userId ;
            c4->server->lockedNetId = netId ;
            c4->server->lockedLockItem = lockItem ;
         }

      return r4locked ;
   }
   */

   // AS Feb 26/03 - For preprocess, the server requires several functions to obtain SERVER4 members
   CODE4 * S4FUNCTION server4getCode4( SERVER4 *server )
   {
      return server->c4 ;
   }



   void S4FUNCTION server4setPreprocess( SERVER4 *server, Bool5 val )
   {
      server->requiresPreprocess = val ;
   }



   SERVER4 * S4FUNCTION server4clientGetServer( SERVER4CLIENT *client )
   {
      return client->server ;
   }



   CONNECT4 * S4FUNCTION server4clientGetConnect( SERVER4CLIENT *client )
   {
      #ifdef S4OFF_COMMUNICATIONS
         return 0 ;
      #else
         return &client->connect ;
      #endif
   }
#endif  /* S4SERVER */
