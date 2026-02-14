/* d4server.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef D4SERVERH_INC
   #define D4SERVERH_INC

   #ifdef S4SERVER
      #ifdef TIMER5OUT
         #include "timer5.h"
      #endif

      struct SERVER4CLIENTSt ;

      #ifdef S4LOG
         struct LOG4DATASt ;
         struct LOG4FILESt ;
      #endif

      #define JAVA4SOCKET_SEND_BUFFER_SIZE 0xF000

      #ifdef E4DEBUG_INFO
         typedef struct
         {
            HWND      hWnd ;
            int          x,y ;

            #ifdef S4WINDOWS
               TEXTMETRIC   tm ;
               LPMSG        lpmsg ;
               MSG          msg ;  /* Last Message */
               int          didClose ;
               int          didQuit ;
               HCURSOR      hSaveCursor ;
            #endif
         } D4DISPLAY;

         void debug4displayInit( D4DISPLAY *disp, HWND h ) ;
         void  S4FUNCTION debug4display( const char *str ) ;
         extern D4DISPLAY debugDisplay ;
      #endif /* E4DEBUG_INFO */



      typedef struct
      {
         long numRequests ;
         unsigned long numTransactions ;
         unsigned long numCompletedTransactions ;
         unsigned long numRollbacks ;
         time_t connectionStart ;
      /*   used when server4info() is called */
      /*   time_t connectionTime ;*/
      } CLIENT4INFO ;



      typedef struct
      {
         /*   unsigned short numClients ;*/
         long numRequests ;
         unsigned short numDeniedConnections ;
         time_t serverStart ;

         /*   used when server4info() is called */
         /*   time_t serverTime ;*/
         #ifdef S4SERVER_GUI
            long numTransCommitted ;
            long numTransRolledBack ;
            long lockCount ;
         #endif
      } SERVER4INFO ;



      #ifdef OLEDB5BUILD
         class Session5 ;
         class Source5all ;
      #endif



      enum Accept4status
      {
         accept4statusNotStarted = 0 ,  /* preferrable to have set to 0 since that is initial value if not assigned - not started */
         accept4statusFailed,
         accept4statusInitialized,
         accept4statusStarting,
      } ;



      #ifdef S4WIN32
         // AS Sept 3/02 - new functionality for linking to a customer-provided dll.
         #ifndef S4FUNCTION2
            #define S4FUNCTION2 __stdcall
         #endif
         typedef int S4FUNCTION2 S4ADD_FUNCTION( long, const void S4PTR *, long, void S4PTR * S4PTR *, long S4PTR * ) ;
      #endif

      class SERVER4
      {
         #ifndef CODE4UNAVAILABLE
            public:
               CODE4 *c4 ;
               unsigned long clientIdCount ;
               unsigned long serverDataCount ;

            //   SERVER4() { memset( this, 0, sizeof( SERVER4 ) ) ; }

               // constructor doesn't work because it resets base class values also, need to get before that
               void * operator new( size_t s ) { void *ptr = malloc( s ) ; memset( ptr, 0, s); return ptr ; }
               void operator delete( void *p ) { free(p) ; }

               CODE4TRANS c4trans ;
               #ifdef S4OFF_THREAD
                  LIST4 clients ;  /* list of SERVER4CLIENT's */
               #else
                  LIST4MUTEX clients ;  /* list of SERVER4CLIENT's */
               #endif
               int keepOpen ;   /* == 1 if files should be kept open.  is set to '2' when we want to
                                   keepopen a temporary file (which otherwise gets closed and deleted
                                   on close even if keepOpen set to 1) */
               int doDataCreate, doIndexCreate, doReindex, doPack, doZap, doCheck, doCompress, doExit ;
               #ifdef S4LOG
                  int logSource ;   /* set to 1 if the source is the data log file */
                  struct LOG4DATASt  *dataLog ;
                  struct LOG4FILESt  *logFile ;
               #endif
               #ifndef S4OFF_TRAN
                  TRAN4FILE transFile ;
                  char logName[LEN4PATH] ;
                  // AS 09/15/00 - autorecovery coding
                  char backupLogName1[LEN4PATH] ;
                  char backupLogName2[LEN4PATH] ;
                  char preBackupLogName[LEN4PATH] ;
                  Bool5 doAutoRecovery ;
               #endif
               SERVER4INFO info ;
               #ifndef S4OFF_SECURITY
                  DATA4 *authorize ;  /* temp data base and data file authorization tables */
                  DATA4 *tableAuth, *dbAuth ;  /* table and authorization tables */
                  TAG4 *granteeTag ;    /* for dbAuth */
                  FIELD4 *a4userid, *a4table, *a4insert, *a4delete, *a4update, *a4read, *a4index, *a4open, *a4compress ;
                  FIELD4 *tb4user, *tb4tableName, *tb4insert, *tb4delete, *tb4update, *tb4read, *tb4index, *tb4open, *tb4compress ;
                  FIELD4 *dbuser, *dba, *resource, *pass ;
               #endif
               #ifdef S4DEAD_CHECK
                  char timeStamp ;
                  long timeCheck ;
               #endif
               CONNECTION4 defaultConnection ;   /* used to keep track of info for server-owned data files */
               const char *lockedFileName ;
               const char *lockedUserName ;
               const char *lockedNetName ;
               long lockedLockItem ;
               char serverName[S4SERVER_NAME_SIZE] ;
               char hostName[S4SERVER_NAME_SIZE] ;
               char *processId ;
               char systemPath[LEN4PATH] ;
               #if !defined( S4ODBC_STAND ) && defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) )
                  // AS 12/07/99 connect accept implemented via shared memory
                  SHARE4 *odbcSharedMemory ;
               #else
                  Bool5 connectAccept ;
               #endif

               // AS Apr 28/03 - Support trans via registry setting
               #if defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )
                  Bool5 odbcTrans ;   // set to 1 if we expose odbc transaction processing
               #endif

               // #if defined( S4SERVER ) && ( defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED ) ) && !defined( S4OFF_TRAN )
               //    MUTEX4 odbcTransMutex ;
               // #endif
               #if !defined( S4ODBC_STAND ) &&  defined( E4ANALYZE ) && defined( S4ODBC_ENABLED )
                  // share used in E4ANALYZE to invoke debuggers in other processes...just
                  // set the breakpoint in server4init() in d4server.c where this value is
                  // initialized, and hence turn on debugger in other process...
                  SHARE4 *odbcSharedDebug ;
               #endif
               #ifndef S4OFF_COMMUNICATIONS
                  unsigned long acceptThreadHandle ;
                  CONNECT4LOW listenSocket ;
               #endif /* S4OFF_COMMUNICATIONS */
               TCP4ADDRESS tcpBegin, tcpEnd ;
               char configName[LEN4PATH+1] ;
               #ifndef S4OFF_SECURITY  // CS 2000/05/02
                  char acctName[LEN4PATH+1] ;
                  char privName[LEN4PATH+1] ;
               #endif
               /* #if S4MAX_USERS != 0 */  /*CJ- Taken for Licensing and security reasons */
                  unsigned short int maxUsers ;
               /*#endif*/
               #ifdef S4JAVA
                  char *javaSendBuffer ;
                  unsigned short javaSendBufferLen ;
                  unsigned short javaSendLen ;
               #endif
               #ifndef S4OFF_SECURITY
                  ACCOUNT4 accounts ;
                  PRIVILEGE4 privileges ;
               #endif
               #ifdef S4COMTHREADS
                  EVENT4 quitExitServerEvent ;
               #endif
               #ifdef OLEDB5BUILD
                  Source5all source ;
               #endif
               #ifdef TIMER5OUT
                  /* case where we want to perform timing of server operations */
                  FILE4 timerFile ;
                  TimerMemory5 *timerMemory ;
                  Timer5 *currentTimer ;
                  Timer5 *masterTimer ;
               #endif
               Accept4status acceptThreadStatus ;
               long pipeIdCount ;
               MEM4 *messageRecvMemory ;
               MEM4 *messageSendMemory ;
               MEM4 *pipeMemory ;
               LIST4 pipes ;
               assert5port( "preprocess support" ) ;
               Bool5 requiresPreprocess ;
               void *preprocessPKI ;
               void *preprocessSK ;
               // AS Jul 5/02 - New setting to keep log file in a permanently compressed state
               assert5port( "log compression support" ) ;
               unsigned long maxLogSize ;  // max allowed size if 2 Gigs, to make f4long reading easier.
               // AS Jan 23/04 - Support a suspension of performing the relate so other clients can perform actions
               Bool5 suspendQuery ;
               #ifdef S4WIN32
                  // AS Sept 3/02 - new functionality for linking to a customer-provided dll.
                  HINSTANCE addDll ;
                  S4ADD_FUNCTION *addFunction ;
                  long addFuncInfoLen ;
                  char *addFuncInfo ;
                  // char addDllName[LEN4PATH] ;    // name of the dll to link to default would be CBADD.DLL if not specified
               #endif
               #if defined( S4ENCRYPT_HOOK ) || defined( S4ENCRYPT_COM )
                  long numBits ;
               #endif
               // AS Jun 5/06 - add some additional debugging here
               #if defined( E4MUTEX_CHECK ) && !defined( S4OFF_THREAD )
                  struct SERVER4CLIENTSt *clientListHolder ;
               #endif
         #endif
      } ;



      typedef struct
      {
         LINK4 link ;
         TCP4ADDRESS address ;
         short port ;
         char fromClient ;
         #ifdef S4WINSOCK
            SOCKET sock ;
         #endif
         #ifdef S4BERKSOCK
            int sock ;
         #endif
         SERVER4CLIENTSt *client ;
      } CONNECT4HALF ;



      typedef struct SERVER4CLIENTSt
      {
         #ifndef CODE4UNAVAILABLE
            LINK4 link ;
            TRAN4 trans ;
            SERVER4 *server ;
            #ifndef S4OFF_COMMUNICATIONS
               CONNECTION4 connection ;
               CONNECT4 connect ;
               // int connected ; Is there a connection, taken over by connection.connected
            #endif /* S4OFF_COMMUNICATIONS */
            LIST4 relations ;
            unsigned long relationIdCount ;
            CLIENT4INFO info ;

            #ifndef S4OFF_SECURITY
               // the following rights if TRUE mean ok, if FALSE means must check on
               // table-by-table basis via the privilege table

               ACCOUNT4USER account ;
            #endif

            LIST4 calcList ;

            long id ;
            long errorCode2 ;
            int errorCode ;
            int errCreate ;
            char readLock, readOnly ;
            int doRemove ;

            // AS 08/21/00 - message handling for interclient communication
            LIST4 messageSendList ;       // list of MESSAGE4SEND pipes for sending
            // LIST4 pipeRecvList ;       // list of pipes for receiving

            assert5port( "sending unlock codes on streamed sockets (batch skips) - new communications info" ) ;
            // Apr 24/02 - New code to support sending unlock code on streamed socket messages (i.e. new batch skip)
            Bool5 sendUnlock ;

            #ifndef S4OFF_WRITE
               int transEnabled ;
            #endif
            #ifdef S4JAVA
               char javaClient ;      /* flag used internally, true if a java client */
               int status ;
               DATA4 *currentData ;
               char accessMode, errDefaultUnique, safety, unlockAuto, log ;
               CONNECT4HALF *connectHalf ;   /* Don't count on this to be set - if it is, must reset client to 0 on uninitialize for half java to work */
            #endif
            #ifdef OLEDB5BUILD
               Session5 *session ;
               Array5tables tables ;
               Array5<Table5rowsetValues> rowsets ;
               Array5<Index5server> indexes ;
               Array5<Index5rangeInfo> indexRanges ;
            #endif
            #ifdef S4DEAD_CHECK
               char timeCount ;
            #endif
            #ifdef E4DEBUG
               /* for micro-messaging, can track lengths for debug purposes as
                  required if both client and server are E4DEBUG */
               long commandLen ;
               char lastCommandWasLen ;
            #endif
            int isStream ;    // if isStream message, no unlock status codes are sent back to client
            #ifndef S4OFF_COMMUNICATIONS
               #ifdef E4DEBUG
                  char isDisconnecting ;
               #endif
            #endif /* S4OFF_COMMUNICATIONS */
            #ifdef S4TESTING
               char *getBuf ;
               long getBufLen ;
            #endif
            #ifdef S4FOX
               unsigned int foxCreateIndexBlockSize ;    // block size to use for fox indexes, must be multiple of 512
               unsigned int foxCreateIndexMultiplier ;        // multiplier to use for fox indexes, must be divisible into foxCreateIndexBlockSize
            #endif
            #ifndef S4OFF_LOG
               char connectInfoStatus[1024] ;
            #endif
            short connectionErrStatus ;   // AS Sept 30/02 propogation info for connection status on errors (did we get connected)
            short didConnectStatus ;      // AS Sept 30/02 - set to true if the client did finish connecting to the server
            // AS Aug 29/03 - Support for memSizeMemoExpr which can change at any time
            unsigned short memSizeMemoExpr ;
            #if defined( E4ANALYZE ) || defined( S4TESTING )
               unsigned long testCount ;
            #endif
            // AS May 13/04 - configureable compression levels
            #if defined( S4COMPRESS )
               short fileCompressLevel ;  // the file compression level is specified on a per-client basis
            #endif
            // AS Mar 3/10 let's keep some info about the client for error reporting
            TCP4ADDRESS clientTcpAddress ;
         #endif
      } SERVER4CLIENT ;


      #ifdef __cplusplus
         extern "C" {
      #endif
            #ifdef S4OFF_THREAD
               #define server4clientListRemove( server, client ) ( l4remove( &((server)->clients), (LINK4 *)(client) ) )
               #define server4clientListAdd( server, client ) ( l4add( &((server)->clients), (LINK4 *)(client) ) )
               #define server4clientGetFirst( server ) ( (SERVER4CLIENT *)l4first( &((server)->clients) ) )
               #define server4clientGetNext( server, client ) ( (SERVER4CLIENT *)l4next( &((server)->clients), client ) )
               #define server4clientListReserve( server )
               #define server4clientListRelease( server )
            #else
               #define server4clientListRemove( server, client ) ( list4mutexRemoveLink( &((server)->clients), (LINK4 *)(client) ) )
               #define server4clientListAdd( server, client ) (  list4mutexAdd( &((server)->clients), (client) ) )
               #define server4clientGetFirst( server ) ( (SERVER4CLIENT *)l4first( &((server)->clients.list) ) )
               #define server4clientGetNext( server, client ) ( (SERVER4CLIENT *)l4next( &((server)->clients.list), (client) ) )
               // AS Jun 05/06 - add some additional debugging here
               #ifdef E4MUTEX_CHECK
                  int server4clientListReserve( SERVER4 *server ) ;
                  int server4clientListRelease( SERVER4 *server ) ;
               #else
                  #define server4clientListReserve( server ) list4mutexWait( &((server)->clients) )
                  #define server4clientListRelease( server ) list4mutexRelease( &((server)->clients) )
               #endif
            #endif
            S4EXPORT int S4FUNCTION server4waitOnQuitOrExit( SERVER4 S4PTR * ) ;
            S4EXPORT int S4FUNCTION server4quit( SERVER4 S4PTR *, int ) ;
            S4EXPORT long S4FUNCTION server4maxSockets( SERVER4 S4PTR * ) ;
            S4EXPORT SERVER4CLIENT S4PTR *S4FUNCTION c4CatalogClient( CODE4 S4PTR *c4) ;

            /* S4EXPORT char S4PTR *S4FUNCTION server4protocol( SERVER4 S4PTR * ) ; */
            S4EXPORT int S4FUNCTION server4numClients( SERVER4 S4PTR * ) ;
            #ifdef S4SERVER_GUI
               S4EXPORT long S4FUNCTION server4maxUsers( SERVER4 S4PTR * ) ;
               S4EXPORT long S4FUNCTION server4numTransCommitted( SERVER4 S4PTR * ) ;
               S4EXPORT long S4FUNCTION server4numTransRolledBack( SERVER4 S4PTR * ) ;
               S4EXPORT long S4FUNCTION server4numTables( SERVER4 S4PTR * ) ;
               S4EXPORT long S4FUNCTION server4lockCount( SERVER4 S4PTR * ) ;
               S4EXPORT Bool5 S4FUNCTION server4autoRecovery( SERVER4 S4PTR * ) ;
            #endif
            S4EXPORT unsigned short S4FUNCTION server4portNumber( SERVER4 S4PTR * ) ;
            S4EXPORT char S4PTR *S4FUNCTION server4name( SERVER4 S4PTR * ) ;
            S4EXPORT int S4FUNCTION server4securityOption( void ) ;
            S4EXPORT char S4PTR *S4FUNCTION server4CustomerFromStamp( void ) ;
            S4EXPORT char S4PTR *S4FUNCTION server4SerialFromStamp( void ) ;
            S4EXPORT SERVER4 S4PTR *S4FUNCTION server4alloc( void ) ;
            // AS July 29/02 - let's track a global server for error handling when no c4 present...
            S4EXPORT void S4FUNCTION server4setServerPtr( SERVER4 S4PTR * ) ;
            S4EXPORT void S4FUNCTION server4free( SERVER4 S4PTR * ) ;
            void S4FUNCTION server4exit( SERVER4 S4PTR * ) ;
            int S4FUNCTION server4exitTest( SERVER4 S4PTR * ) ;
            S4EXPORT int S4FUNCTION server4init( SERVER4 S4PTR *, CODE4 S4PTR *, const char S4PTR *, Bool5, long hWnd ) ;  // CS 2001/04/10 last parameter is long
            int server4initUser( Bool5 ) ;
            #ifdef E4DEBUG
               int server4clientVerifyData4s( SERVER4CLIENT * ) ;
            #endif
            int server4clientCloseData( SERVER4CLIENT *, DATA4 * ) ;
            int server4clientRestart( SERVER4CLIENT * ) ;
            int server4clientStatus( SERVER4CLIENT * ) ;
            int server4clientTranEofHalt( SERVER4CLIENT *, char *, const unsigned int ) ;
            int server4clientTranEof( SERVER4CLIENT * ) ;
            int server4clientConfigName( SERVER4CLIENT * ) ;
            int server4clientConnectAcceptNew( SERVER4CLIENT * ) ;
            int server4clientConnectCut( SERVER4CLIENT * ) ;
            int server4clientConnectCutAll( SERVER4CLIENT * ) ;
            void server4clientCheckConnection( SERVER4CLIENT * ) ;
            int server4clientConnectAcceptNew( SERVER4CLIENT * ) ;
            int server4clientConnectCloseFiles( SERVER4CLIENT * ) ;
            int server4clientTables( SERVER4CLIENT * ) ;
            int server4clientCurrentDirectory( SERVER4CLIENT * ) ;
            void server4clientDirectory( SERVER4CLIENT * ) ;
            S4EXPORT int S4FUNCTION server4keepOpen(SERVER4 S4PTR *, int ) ;
            S4EXPORT char *S4FUNCTION server4systemPath(SERVER4 S4PTR * ) ;

            #ifdef S4TESTING
               void server4clientTestSendReceive( SERVER4CLIENT * ) ;
            #endif

            #ifdef OLEDB5BUILD
               void server4clientMsgIntegratedTagSelect( SERVER4CLIENT *client ) ;
         //      void server4clientMsgIntegratedTagExists( SERVER4CLIENT *client ) ;
               void server4clientMsgIntegratedRowRequest( SERVER4CLIENT *client ) ;
               void server4clientMsgAddDirectory( SERVER4CLIENT *client ) ;
               void server4clientMsgAddTag( SERVER4CLIENT *client ) ;
               void server4clientMsgColumnInfo( SERVER4CLIENT *client ) ;
               void server4clientMsgCurrentDirectory( SERVER4CLIENT *client ) ;
               void server4clientMsgDataAddColumn( SERVER4CLIENT *client ) ;
               void server4clientMsgDataSchemaRequestDirect( SERVER4CLIENT *client ) ;
               void server4clientMsgDataFetchNextRecno( SERVER4CLIENT *client ) ;
               void server4clientMsgDataClose( SERVER4CLIENT *client ) ;
               void server4clientMsgDataRemoveColumn( SERVER4CLIENT *client ) ;
               void server4clientMsgFindOrAddEntry( SERVER4CLIENT *client ) ;
               void server4clientMsgIndexColumnInfo( SERVER4CLIENT *client ) ;
               void server4clientMsgIndexRemove( SERVER4CLIENT *client ) ;
               void server4clientMsgIndexRequestKey( SERVER4CLIENT *client ) ;
               void server4clientMsgIndexRowRequest( SERVER4CLIENT *client, int doKey ) ;
               void server4clientMsgIndexDestroyRange( SERVER4CLIENT *client ) ;
               void server4clientMsgIndexSetRange( SERVER4CLIENT *client ) ;
               void server4clientMsgInTransaction( SERVER4CLIENT *client ) ;
               void server4clientMsgIsoLevel( SERVER4CLIENT *client ) ;
               void server4clientMsgReccount( SERVER4CLIENT *client ) ;
               void server4clientMsgRequestDeferredField( SERVER4CLIENT *client ) ;
               void server4clientMsgRowRequestArr( SERVER4CLIENT *client ) ;
               void server4clientMsgRowRequestSeq( SERVER4CLIENT *client ) ;
               void server4clientMsgRowsetDestroy( SERVER4CLIENT *client ) ;
               void server4clientMsgSchemaRequestSeq( SERVER4CLIENT *client ) ;
               void server4clientMsgSchemaPopulateColumns( SERVER4CLIENT *client ) ;
               void server4clientMsgSchemaPopulateIndexes( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionDataCreate( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionDataOpen( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionDelete( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionDefaults( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionIndexOpen( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionIndexClose( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionSchemaOpen( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionUnlockAll( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionSeek( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionWrite( SERVER4CLIENT *client ) ;
               void server4clientMsgSessionWriteDone( SERVER4CLIENT *client ) ;
               void server4clientMsgSetRestrictions( SERVER4CLIENT *client ) ;
               void server4clientMsgTableRemove( SERVER4CLIENT *client ) ;
               void server4clientMsgTableRemoveIndexes( SERVER4CLIENT *client ) ;
               void server4clientMsgTableWriteResultInc( SERVER4CLIENT *client ) ;
               void server4clientMsgTagSelect( SERVER4CLIENT *client ) ;
               void server4clientMsgUpdateFieldset( SERVER4CLIENT *client ) ;
            #endif /* OLEDB5BUILD */

            int server4clientReceive( SERVER4CLIENT *client ) ;
            int server4clientSend( SERVER4CLIENT *client ) ;
            int server4clientOpenRecv( SERVER4CLIENT *client ) ;
            int server4clientCloseRecv( SERVER4CLIENT *client ) ;
//            int server4clientCloseSend( SERVER4CLIENT *client ) ;
            int server4clientOpenSend( SERVER4CLIENT *client ) ;
            int server4clientSendCheck( SERVER4CLIENT *client ) ;
            int server4clientSendCancel( SERVER4CLIENT *client ) ;


            int server4info( SERVER4 *, SERVER4CLIENT * ) ;
            S4EXPORT int S4FUNCTION server4initUndo( SERVER4 S4PTR * ) ;

            int server4closeAll( SERVER4 * ) ;
            int server4clientAccept(SERVER4 *, CODE4 * )  ;

            #ifdef S4DEAD_CHECK
               void server4verifyClients(SERVER4 *) ;
               SERVER4CLIENT *server4clientId( SERVER4 *, const long ) ;
            #endif

            CONNECT4HALF * code4findFirstConnection(CODE4 *, TCP4ADDRESS, short, short ) ;

            int S4FUNCTION server4connect( SERVER4 S4PTR * ) ;
            int S4FUNCTION server4disconnect( SERVER4 S4PTR *, SERVER4CLIENT S4PTR * ) ;
            SERVER4CLIENT *S4FUNCTION server4getMessage( SERVER4 S4PTR * ) ;
            #ifndef S4OFF_COMMUNICATIONS
               int S4FUNCTION server4processMessage( SERVER4 S4PTR *, SERVER4CLIENT S4PTR * ) ;
            #endif
            int server4executeLogData( SERVER4 * ) ;

            DATA4 * server4open( SERVER4 *, SERVER4CLIENT *, const char *, unsigned short int, unsigned short int ) ;

            #ifdef E4ANALYZE
               int server4verify( SERVER4 *, const int ) ;
            #endif

            int server4clientAddTag( SERVER4CLIENT * ) ;
            int server4clientRemoveTag( SERVER4CLIENT * ) ;
            int server4clientAppend( SERVER4CLIENT * ) ;
            int server4clientBlastTestWrite( SERVER4CLIENT * ) ;
            int server4clientBlastTestRead( SERVER4CLIENT * ) ;
            int server4clientBottom( SERVER4CLIENT * ) ;
            int server4clientCalcCreate( SERVER4CLIENT * ) ;
            int server4clientCalcReset( SERVER4CLIENT * ) ;
            int server4clientCancel( SERVER4CLIENT * ) ;
            int server4clientCatalogSet( SERVER4CLIENT * ) ;
            int server4clientClose( SERVER4CLIENT * ) ;
            int server4clientCloseIndex( SERVER4CLIENT * ) ;
            int server4clientCheck( SERVER4CLIENT * ) ;
            int server4clientConnect( SERVER4CLIENT * ) ;
            int server4clientConnectNew( SERVER4CLIENT * ) ;
            int java4messageConnect( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client ) ;
            int server4clientConnected( SERVER4CLIENT * ) ;
            int server4clientCreate( SERVER4CLIENT * ) ;
            int server4clientSwapLogFile( SERVER4CLIENT * ) ;

            int server4clientCreateIndex( SERVER4CLIENT * ) ;
            /*DATA4 * server4clientData( SERVER4CLIENT *, const unsigned long ) ;*/
            // AS May 17/04 - client functionality to copmress the data file...
            int server4clientDataCompress( SERVER4CLIENT *client ) ;
            // AS Jun 11/07 - new function to copy a database
            int server4clientDataCopy( SERVER4CLIENT * ) ;
            int server4clientDataModify( SERVER4CLIENT * ) ;
            int server4clientDataFileName( SERVER4CLIENT * ) ;
            int server4clientDataFileCodePage( SERVER4CLIENT * ) ;
            int server4clientDateFormatSet( SERVER4CLIENT * ) ;
            /*
            #ifndef S4OFF_SECURITY
               int server4clientDbAllRights( SERVER4 * ) ;
               int server4clientDbRights( SERVER4 *, long ) ;
               int server4clientDbAuthorize( SERVER4CLIENT *, const char *, const char * ) ;
               int server4securityInit( SERVER4 * ) ;
               int server4securityInitOne( SERVER4 *, long ) ;
            #endif
            */
//            int server4clientFieldsAdd( SERVER4CLIENT * ) ;
            int server4clientGo( SERVER4CLIENT * ) ;
            int server4clientIndexFileName( SERVER4CLIENT * ) ;
            /* AS Oct 25/05 - support for tag functionality in client/server */
            int server4clientTagBottom( SERVER4CLIENT * ) ;
            int server4clientTagCacheSkip( SERVER4CLIENT * ) ;
            int server4clientTagCount( SERVER4CLIENT * ) ;
            int server4clientTagEof( SERVER4CLIENT * ) ;
            int server4clientTagGo( SERVER4CLIENT * ) ;
            // int server4clientTagKey( SERVER4CLIENT * ) ; // AS Feb 9/09 - replaced functionality with code to get key every time instead
            int server4clientTagExprKey( SERVER4CLIENT * ) ;
            int server4clientTagSeek( SERVER4CLIENT * ) ;
            int server4clientTagSkip( SERVER4CLIENT * ) ;
            int server4clientTagPosition( SERVER4CLIENT * ) ;
            int server4clientTagPositionSet( SERVER4CLIENT * ) ;
            int server4clientTagTop( SERVER4CLIENT * ) ;
            int server4clientTagFileName( SERVER4CLIENT * ) ;
            int server4clientIndexFormat( SERVER4CLIENT * ) ;
            int server4clientServerOS( SERVER4CLIENT * ) ;
            int server4clientIndexInfo( SERVER4CLIENT * ) ;
            S4EXPORT int S4FUNCTION server4clientInit( SERVER4CLIENT S4PTR *, SERVER4 S4PTR *, const long ) ;
            int server4clientInitUndo( SERVER4CLIENT *, int doDelay = 1 ) ;
            int server4clientLicenceCheck(const char *);
            int server4clientLock( SERVER4CLIENT * ) ;
            int server4clientLockGroup( SERVER4CLIENT * ) ;

            #ifndef S4OFF_MEMO
               int server4clientMemoCompress( SERVER4CLIENT * ) ;
               int server4clientMemoRead( SERVER4CLIENT * ) ;
            #endif

            int server4clientOpen( SERVER4CLIENT * ) ;
            int server4clientOpenIndex( SERVER4CLIENT * ) ;
            int server4clientPack( SERVER4CLIENT * ) ;

            #ifndef S4OFF_SECURITY
               int server4clientPasswordSet( SERVER4CLIENT * ) ;
            #endif

            #if (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )) && !defined( S4OFF_ODBC_TRANS ) && !defined( S4OFF_TRAN )
               int code4transServerShareInit( CODE4TRANS *c4trans ) ;
               int code4transServerShareInitUndo( CODE4TRANS *c4trans ) ;
            #endif
            int server4clientPing( SERVER4CLIENT * ) ;
            int server4clientPosition( SERVER4CLIENT * ) ;
            int server4clientPositionSet( SERVER4CLIENT * ) ;
            int server4clientReccount( SERVER4CLIENT * ) ;
            int server4clientReindex( SERVER4CLIENT * ) ;
            int server4clientReindexIndex( SERVER4CLIENT * ) ;
            int server4clientRelateBottom( SERVER4CLIENT * ) ;
            int server4clientRelateDo( SERVER4CLIENT * ) ;
            int server4clientRelateDoOne( SERVER4CLIENT * ) ;
            int server4clientRelateFree( SERVER4CLIENT * ) ;
            int server4clientRelateInit( SERVER4CLIENT * ) ;

            #ifdef S4OLD_RELATE_LOCK
               int server4clientRelateLock( SERVER4CLIENT * ) ;
            #endif

            #ifdef S4ENCRYPT_DLL
               int server4initEncrypt( SERVER4 *server, DATA4 *config ) ;
            #else
               // int server4initPreprocess( SERVER4 *server, DATA4 *config ) ;
            #endif

            int server4clientRelateSkip( SERVER4CLIENT * ) ;
            int server4clientRelateOpt( SERVER4CLIENT * ) ;
            int server4clientRelateTop( SERVER4CLIENT * ) ;
            int server4clientRelateUnlock( SERVER4CLIENT * ) ;
            int server4clientRemove( SERVER4CLIENT * ) ;
            int server4clientSeek( SERVER4CLIENT * ) ;
            int server4clientSeekDouble( SERVER4CLIENT * ) ;
            /* AS Sep 7/11 c/s support for seek long long */
            int server4clientSeekLongLong( SERVER4CLIENT * ) ;
            int server4clientSkip( SERVER4CLIENT * ) ;
            assert5port( "new batch skip functions" ) ;
            /* AS Apr 10/02 - New functionality for advance-reading client/server (reading a block) */
            int server4clientMsgSkip( SERVER4CLIENT * ) ;
            int server4clientMsgRelateSkipMulti( SERVER4CLIENT * ) ;
            int server4clientTagSync( SERVER4CLIENT * ) ;

            assert5port( "new compression and preprocess functions" ) ;
            // AS May 13/02 data compression and preprocessing
            int server4clientMsgCompress( SERVER4CLIENT * ) ;
            int server4clientMsgPreprocessB( SERVER4CLIENT * ) ;
            // #ifdef S4PREPROCESS_COM
               int server4clientMsgPreprocessEP( SERVER4CLIENT * ) ;
               int server4clientMsgPreprocessED( SERVER4CLIENT * ) ;
               int server4clientMsgPreprocessPK( SERVER4CLIENT * ) ;
            // #endif
            int server4clientAdditionalFunction( SERVER4CLIENT * ) ;  // AS Sept. 3/02 - new functionality
            int server4clientRelateCount( SERVER4CLIENT * ) ;  // AS Oct 10/02 - client/server relate4count

            #ifdef S4CLIPPER
               int server4clientTagOpen( SERVER4CLIENT * ) ;
            #endif

            int server4clientTop( SERVER4CLIENT * ) ;
            int server4clientTransactionCommitBothPhases( SERVER4CLIENT * ) ;
            int server4clientTransactionCommitPhaseOne( SERVER4CLIENT * ) ;
            int server4clientTransactionCommitPhaseTwo( SERVER4CLIENT * ) ;
            int server4clientTransactionInit( SERVER4CLIENT * ) ;
            int server4clientTransactionRollback( SERVER4CLIENT * ) ;
            int server4clientTransactionStart( SERVER4CLIENT * ) ;
            int server4clientUniqueSet( SERVER4CLIENT * ) ;
            int S4FUNCTION server4clientUnlock( SERVER4CLIENT S4PTR * ) ;
            // AS Apr 15/03 - support for unlock append by client
            int server4clientUnlockAppend( SERVER4CLIENT * ) ;
            int server4clientVersion( SERVER4CLIENT * ) ;
            // AS Nov 7/03 - server flush
            int server4clientFlush( SERVER4CLIENT * ) ;
            int server4clientWrite( SERVER4CLIENT * ) ;
            assert5port( "new batch-write function" ) ;
            /* AS Apr 10/02 - New functionality for write buffering */
            int server4clientMsgWriteBatch( SERVER4CLIENT * ) ;
            int server4clientUnwrite( SERVER4CLIENT * ) ;
            int server4clientUnappend( SERVER4CLIENT * ) ;

            // AS Feb 26/03 - For preprocess, the server requires several functions to obtain SERVER4 members
            S4EXPORT CODE4 * S4FUNCTION server4getCode4( SERVER4 * ) ;
            S4EXPORT void S4FUNCTION server4setPreprocess( SERVER4 *, Bool5 val ) ;
            S4EXPORT SERVER4 * S4FUNCTION server4clientGetServer( SERVER4CLIENT * ) ;
            S4EXPORT CONNECT4 * S4FUNCTION server4clientGetConnect( SERVER4CLIENT * ) ;

            #ifdef E4ANALYZE
               int server4clientVerify( SERVER4CLIENT *, const int ) ;
            #endif

            int server4clientZap( SERVER4CLIENT * ) ;
            int server4serverTransactionComplete( SERVER4CLIENT * ) ;
            S4EXPORT void S4FUNCTION server4setDoExit( SERVER4 S4PTR *, int ) ;
            S4EXPORT int S4FUNCTION server4getDoExit( SERVER4 S4PTR * ) ;

            #ifdef TIMER5OUT
               void server4timerStart( SERVER4 *, char * ) ;
               void server4timerStop( SERVER4 * ) ;
            #endif /* TIMER5OUT */

            assert5port( "new function for use with compressed log files feature" ) ;
            // AS July 9/02 - New function for populating an in-progress server version of the log-file (for log4compress)
            int server4transInitServerId( CODE4 *c4, SERVER4 *server ) ;
            #ifdef S4WIN32
               S4EXPORT long local4main( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow ) ;
            #endif
            #ifdef S4ODBC_BUILD
               // for ODBC_BUILD DLL, we need to export this function...
               S4EXPORT int S4FUNCTION d4serverInitialize( const char S4PTR * ) ;
            #else
               // don't need to export in non-odbc-build
               int d4serverInitialize( const char * ) ;
            #endif

            #if defined( S4ODBC_ENABLED )
               #define LNA_SHUTDOWN_GRACEFUL 1
               #define LNA_SHUTDOWN_NOW      2
               int odbc4shutdown( int mode ) ;
               short odbc4getClientsNext( void *odbcClientsHandle, short lastClient ) ;
               short odbc4getClientsFirst( void *odbcClientsHandle ) ;
               short odbc4getClientsList( void **odbcClientsHandle ) ;
               void odbc4getTcpAddress( void *odbcClientsHandle, short clientHandle, TCP4ADDRESS *address ) ;
               const char *odbc4getAccountId( void *odbcClientsHandle, short clientHandle ) ;
               LONGLONG odbc4getLamUserId( void *odbcClientsHandle, short clientHandle ) ;
               void odbc4cutAllClients() ;
               void odbc4cutOneClient( LONGLONG connIdLongLong ) ;
               void odbc4freeClientsList( void *odbcClientsHandle ) ;
               long odbc4getClientCount() ;
            #endif
      #ifdef __cplusplus
         }
      #endif
   #endif /* S4SERVER */
#endif /* D4SERVERH_INC */
