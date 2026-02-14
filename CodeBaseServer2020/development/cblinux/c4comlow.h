#ifndef S4STAND_ALONE

#define ver4NT 1895
#define ver431 1931
#define ver495 1995
#define ver4Unix 1000

#ifdef S4BYTEORDER_3210
   #define htond(a) (a)
   #define ntohd(a) (a)
#else
   double htond(double) ;
   #define ntohd(a) htond((a))
#endif

#ifdef __cplusplus
   extern "C" {
#endif

int blast4testWrite( CODE4 *, long  ) ;
int blast4testRead( CODE4 *, long  ) ;

#ifndef S4OFF_THREAD
   void code4freeEvent( CODE4 *, EVENT4 *) ;
   EVENT4 *code4getEvent( CODE4 * ) ;
        EVENT4 *code4allocateEvent( CODE4 *) ;
   void code4clearEvents( CODE4 *c4) ;
#endif
S4EXPORT int S4FUNCTION code4osVersion ( void ) ;

//CJ July 23/01-needed to change the void* to C4ADDRESS* as Macintosh clients need to pass a class. Conversion
// to void* then back was losing the reference.
int connect4lowConnectReconnect( CONNECT4LOW *connection, C4ADDRESS *address, int addrLen ) ;
int connect4lowReconnect(CONNECT4LOW *conLow, C4ADDRESS *address, int addrLen ) ;
int connect4reconnect( CONNECT4 *connect, CODE4 *c4 ) ;
int connect4connect( CONNECT4 *, CODE4 *, unsigned short, const char * ) ;
void connect4disconnect( CONNECT4 * ) ;
int connect4init( CONNECT4 *, CONNECT4LOW **, CODE4 *) ;
void connect4initUndo( CONNECT4 *) ;

#ifdef S4WINSOCK
   // AS Jan 21/02 - Need to use a semaphore with the connect4low structure in windows because otherwise
   // we are trampling ourselves
   void connect4lowEnterExclusive( CONNECT4BUFFER *con4buffer ) ;
   void connect4lowExitExclusive( CONNECT4BUFFER *con4buffer ) ;
#else
   #define connect4lowEnterExclusive(a)
   #define connect4lowExitExclusive(a)
#endif

#ifdef E4DEBUG
   CONNECT4LOW *connect4bufferLowGet( CONNECT4BUFFER *con4buffer ) ;
   void connect4bufferLowSet( CONNECT4BUFFER *con4buffer, CONNECT4LOW *val ) ;
#else
   #define connect4bufferLowGet( con4buffer ) ((con4buffer)->connectLowPtr)
   #define connect4bufferLowSet( con4buffer, val ) ((con4buffer)->connectLowPtr = val)
#endif

#define connect4receive(a,b,c,d) connect4bufferReceive(&(a)->connectBuffer,(b),(c),(d))
#define connect4send(a,b,c) connect4bufferSend(&(a)->connectBuffer,(b),(c))
#define connect4sendLong(a,b) connect4bufferSendLong(&(a)->connectBuffer, (b))
#define connect4receiveChar(a) connect4bufferReceiveChar(&(a)->connectBuffer, code4timeoutVal( (a)->cb ) )
#define connect4receiveShort(a) connect4bufferReceiveShort(&(a)->connectBuffer, code4timeoutVal( (a)->cb ) )
#define connect4receiveString(a) connect4bufferReceiveString(&(a)->connectBuffer, code4timeoutVal( (a)->cb ) )
#define connect4receiveWideString(a) connect4bufferReceiveWideString(&(a)->connectBuffer, code4timeoutVal( (a)->cb ) )
#define connect4receiveLong(a) connect4bufferReceiveLong(&(a)->connectBuffer, code4timeoutVal( (a)->cb ) )
#define connect4sendChar(a,b) connect4bufferSendChar(&(a)->connectBuffer, (b))
#define connect4sendShort(a,b) connect4bufferSendShort(&(a)->connectBuffer, (b))
#define connect4sendString(a,b) connect4bufferSendString(&(a)->connectBuffer, (b))
#define connect4sendWideString(a,b) connect4bufferSendWideString(&(a)->connectBuffer, (b))
#define connect4sendFlush(a) connect4bufferSendFlush(&(a)->connectBuffer)
int connect4workContinue( CONNECT4 * ) ;

TCP4ADDRESS connect4peerTcpAddress( CONNECT4 * ) ;
TCP4ADDRESS connect4lowPeerTcpAddress( CONNECT4LOW * ) ;
TCP4ADDRESS connect4localTcpAddress( CONNECT4 * ) ;
TCP4ADDRESS connect4lowLocalTcpAddress( CONNECT4LOW * ) ;

CONNECT4BUFFER *connect4bufferAuxConnection( CONNECT4 *, int , int , int  ) ;
CONNECT4BUFFER *connect4bufferAuxConnectionGet( CONNECT4 *, int , int , int  ) ;
void connect4bufferAuxConnectionPut( CONNECT4BUFFER *, CONNECT4 * ) ;
#ifdef S4SERVER
   CONNECT4BUFFER *connect4bufferAuxConnectionSpecific( CONNECT4 *, short  ) ;
#endif
int connect4bufferConnect( CONNECT4BUFFER *, CONNECT4 *) ;
void connect4bufferDisconnect( CONNECT4BUFFER * ) ;
#ifndef S4OFF_THREAD
   // NET4MESSAGE *connect4bufferGetWriteBuffer( CONNECT4BUFFER * ) ;
   // NET4MESSAGE *connect4bufferReadRetrieve( CONNECT4BUFFER *, int ) ;
   NET4MESSAGE *connect4bufferWriteRetrieve( CONNECT4BUFFER *, int, int ) ;
   int connect4bufferInitRead( CONNECT4BUFFER *, int , int ) ;
#endif
#ifdef S4COMPRESS
   void connect4lowCompressConfigure( CONNECT4LOW *connect, short compressLevel, short minLength ) ;
   int connect4lowUncompress( unsigned char *buffer, int len, int maxLen ) ;
#endif
int connect4bufferInit( CONNECT4BUFFER *, CONNECT4 *, CONNECT4LOW **, int, int, int ) ;
void connect4bufferInitUndo( CONNECT4BUFFER * ) ;
S4EXPORT int S4FUNCTION connect4bufferReceive( CONNECT4BUFFER S4PTR *, void S4PTR *, long , int ) ;
S4EXPORT int S4FUNCTION connect4bufferSend( CONNECT4BUFFER S4PTR *, const void S4PTR *, long ) ;
S4EXPORT int S4FUNCTION connect4bufferSendLong( CONNECT4BUFFER S4PTR *, const long ) ;
S4EXPORT short S4FUNCTION connect4bufferReceiveShort( CONNECT4BUFFER S4PTR *, int ) ;
S4EXPORT long S4FUNCTION connect4bufferReceiveLong( CONNECT4BUFFER S4PTR *, int ) ;
S4EXPORT char *S4FUNCTION connect4bufferReceiveString( CONNECT4BUFFER S4PTR *, int ) ;
S4EXPORT WSTR5 *S4FUNCTION connect4bufferReceiveWideString( CONNECT4BUFFER S4PTR *, int ) ;
S4EXPORT int S4FUNCTION connect4bufferSendString( CONNECT4BUFFER S4PTR *, const char S4PTR *string ) ;
S4EXPORT int S4FUNCTION connect4bufferSendWideString( CONNECT4BUFFER S4PTR *, const WSTR5 S4PTR *wideString ) ;
S4EXPORT int S4FUNCTION connect4bufferSendShort( CONNECT4BUFFER S4PTR *, const short ) ;
S4EXPORT int S4FUNCTION connect4bufferSendFlush( CONNECT4BUFFER S4PTR * ) ;
S4EXPORT int S4FUNCTION connect4bufferSendChar( CONNECT4BUFFER S4PTR *, const char ) ;
S4EXPORT char S4FUNCTION connect4bufferReceiveChar( CONNECT4BUFFER S4PTR *, int ) ;

int address4getLocalNetID( C4NETID *, short *, CONNECT4LOW * ) ;
#ifndef S4MAC_TCP
        int address4makeName( C4ADDRESS *, short *, unsigned short , const char *) ;
#else
        int address4makeNameMac( C4ADDRESS **, unsigned short , const char *) ;
#endif
int address4makeNetID( C4ADDRESS *, short *, C4NETID , unsigned short  ) ;

/* void address4changePort( C4ADDRESS *, short *, unsigned short );*/
/* int address4getLocalAddress( C4ADDRESS *, short *, CONNECT4LOW * );*/
/* int address4make( C4ADDRESS *, short *, unsigned short , const char *);*/

int connect4lowAccept( CONNECT4LOW *, CONNECT4LOW *) ;
int connect4lowAcceptWait( CONNECT4LOW *, CONNECT4LOW *, int) ;
void connect4lowBlast( CONNECT4LOW *) ;
int connect4lowCheck( CONNECT4LOW * ) ;
int connect4lowClose( CONNECT4LOW * ) ;
//CJ July 23/01-needed to change the void* to C4ADDRESS* as Macintosh clients need to pass a class. Conversion
// to void* then back was losing the reference.
int connect4lowConnect( CONNECT4LOW *, C4ADDRESS *, int ) ;
int connect4lowConnectConnect( CONNECT4LOW *, C4ADDRESS *, int ) ;
#define connect4lowError( a ) ( connect4lowClose((a)))
int connect4lowGetPersonalAddress( CONNECT4LOW *, void *, int *) ;
int connect4lowListen( CONNECT4LOW *, CODE4 *, short *, int ) ;
int connect4lowMakeAddress(void *, int *, int , const char *) ;
// AS Jan 9/02 - Added timeoutInSeconds paramater to better affect timeout spans.
// AS May 13/02 - communications compression coding
int connect4lowRead( CONNECT4LOW *conlow, char *buffer, int len, long timeoutInSeconds, Bool5 ) ;
int connect4lowWrite( CONNECT4LOW *conlow, char *buffer, unsigned short len, Bool5 ) ;
#ifndef S4OFF_THREAD
   int connect4lowReadAsynchronous( CONNECT4LOW *, NET4MESSAGE *, EVENT4 *, Bool5 ) ;
   int connect4lowWriteAsynchronous( CONNECT4LOW *, NET4MESSAGE *, EVENT4 *, Bool5 ) ;
   int connect4lowWriteSynchronous( CONNECT4LOW *, NET4MESSAGE *, Bool5 ) ;
#endif

#ifndef S4OFF_THREAD
   void connect4threadCompleteRequest( CONNECT4THREAD *) ;
   int connect4threadInit( CONNECT4THREAD *, CODE4 *, CONNECT4BUFFER * ) ;
   void connect4threadInitUndo( CONNECT4THREAD *) ;
   void connect4threadReadCompleted( CONNECT4THREAD *, SIGNAL4ROUTINE *,  NET4MESSAGE *) ;
   int connect4threadReadRequest( CONNECT4THREAD *, NET4MESSAGE * ) ;
   SIGNAL4ROUTINE *connect4threadReadSignalRetrieve( CONNECT4THREAD *, SIGNAL4ROUTINE *, int ) ;
   // AS 05/15/02 - added atTop paramater
   void connect4threadReadStore( CONNECT4THREAD *, NET4MESSAGE *, Bool5 atTop ) ;
   #ifdef S4SERVER
      void connect4threadRegisterShutdown( CONNECT4THREAD *) ;
   #endif
   int connect4threadWorkContinue( CONNECT4THREAD * ) ;
   void connect4threadWriteCompleted( CONNECT4THREAD *, NET4MESSAGE *, SIGNAL4ROUTINE * ) ;
   void connect4threadWriteRequest( CONNECT4THREAD *, NET4MESSAGE *) ;
   SIGNAL4ROUTINE *connect4threadWriteSignalRetrieve( CONNECT4THREAD *, SIGNAL4ROUTINE *, int ) ;

   int event4init( EVENT4 * ) ;
   int event4initUndo( EVENT4 * ) ;
   int event4reset(EVENT4 *) ;
   /* int event4set( EVENT4 * ); */
   #define event4set( event ) ( SetEvent( (event)->handle ) )
   int event4wait( EVENT4 *, int ) ;

   void inter4( void * ) ;
   void inter4completeRequested(INTER4 *, CONNECT4THREAD * ) ;
   void inter4completeRequired(INTER4 *, SIGNAL4ROUTINE *) ;
   void inter4connectionCheck(INTER4 * ) ;
   void inter4error(INTER4 *, CONNECT4THREAD *) ;
   int inter4init(INTER4 *, CODE4 * ) ;
   void inter4initUndo(INTER4 *) ;
   void inter4readCompleted(INTER4 *, SIGNAL4ROUTINE * ) ;
   void inter4readRequested(INTER4 *, NET4MESSAGE * ) ;
   void inter4readRequired(INTER4 *, SIGNAL4ROUTINE * ) ;
   void inter4shutdown(INTER4 *, SIGNAL4ROUTINE *) ;
   void inter4shutdownRequest(INTER4 * ) ;
   void inter4write(INTER4 *, NET4MESSAGE *, SIGNAL4ROUTINE * ) ;
   void inter4writeCompleted(INTER4 *, SIGNAL4ROUTINE * ) ;
   void inter4writeRequested(INTER4 *, NET4MESSAGE * ) ;
   void inter4writeRequired(INTER4 *, SIGNAL4ROUTINE * ) ;

   void list4mutexAdd( LIST4MUTEX *, void * ) ;
   // AS May 15/02 - for compression/preprocessing sometimes need to put messages back on the list
   void list4mutexAddTop( LIST4MUTEX *, void * ) ;
   void list4mutexInit( LIST4MUTEX * ) ;
   void list4mutexInitUndo( LIST4MUTEX * ) ;

   void *list4mutexRemove( LIST4MUTEX * ) ;
   void list4mutexRemoveLink( LIST4MUTEX *, LINK4 * ) ;
   #ifdef E4LINK
      // AS July 23/02 - allow to track this in debug mode
      #define list4mutexHeld( l4, val ) ( ((l4)->isHeld = val ), 0)
      S4EXPORT int list4mutexWait( LIST4MUTEX *listMutex ) ;
      S4EXPORT void list4mutexRelease( LIST4MUTEX *listMutex ) ;
   #else
      #define list4mutexHeld( l4, val ) (0)
      #define list4mutexRelease( l4 ) ( ( !ReleaseMutex( (l4)->mutex ) ) ? error4( 0, e4result, E96928 ) : list4mutexHeld( (l4), 0 ) )
      #define list4mutexWait( l4 ) ( ( (l4)->isValid == 0 ) ? error4( 0, e4parm, E96931 ) :\
           ( ( WaitForSingleObject( (l4)->mutex, INFINITE ) == WAIT_FAILED ) ?  error4( 0, e4result, E96931 ) : list4mutexHeld( (l4), 1 ) ) )
   #endif

   /* int overlap4amountTransferred( OVERLAPPED *) */
   #define overlap4amountTransferred( a ) ( (a).InternalHigh )

   int signal4routineInit( SIGNAL4ROUTINE *, HANDLE, COMPLETION4ROUTINE *, void *, void *) ;
   int signal4arrayInit( SIGNAL4ROUTINE_ARRAY *, CODE4 *, int ) ;
   void signal4arrayInitUndo(SIGNAL4ROUTINE_ARRAY * ) ;
   int signal4arrayAdd(SIGNAL4ROUTINE_ARRAY *, SIGNAL4ROUTINE *, int ) ;
   void signal4arrayRemoveSignal( SIGNAL4ROUTINE_ARRAY *,  SIGNAL4ROUTINE * ) ;
   SIGNAL4ROUTINE *signal4arrayGet( SIGNAL4ROUTINE_ARRAY *, int ) ;
   /* SIGNAL4ROUTINE *signal4arrayWait( SIGNAL4ROUTINE_ARRAY *, unsigned long ) ; */
   int signal4arrayWait(SIGNAL4ROUTINE_ARRAY *, SIGNAL4ROUTINE **, unsigned long ) ;
   int signal4arrayWaitHere(SIGNAL4ROUTINE_ARRAY *, unsigned long, int, int, SIGNAL4ROUTINE ** ) ;
#endif /*!S4OFF_THREAD */
#endif

#ifdef __cplusplus
   }
#endif
