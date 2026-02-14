/* c4code.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */



#if defined( S4WINTEL ) && !defined( S4WINCE )
   #include <process.h>
#endif



#if S4VERSION != 6502
   #error Your CodeBase source version does not match your header file version.
#endif



#ifdef S4TESTING
   #include <fcntl.h>
   #ifdef S4UNIX
      #include <sys/types.h>
      #include <sys/stat.h>
   #else
      #include <sys\types.h>
      #include <sys\stat.h>
   #endif
#endif



#ifdef S4WINTEL
   #ifdef __TURBOC__
      #pragma hdrstop
      #if !defined(__DLL__) && !defined(S4OS2PM) && !defined(S4WIN32) && !defined(S4WINDOWS)
         extern unsigned _stklen ;
      #endif
   #endif

   #if defined(_MSC_VER) && !defined(__DLL__)
       #include <malloc.h>
   #endif
#endif



#ifdef S4CBPP
   #ifdef __cplusplus
      #include "d4data.hpp"
   #endif
#endif



#if defined(S4TESTING) || defined(S4TRACK_FILES)
   FILE4 s4test ;
#endif



#if defined(S4FILE_EXTENDED) && defined(S4FOX)
   C4STAMP largeStamp = {{"BBROYGBVGWE",40},{"LARGELOCK  ", 4}, 0, 0};
#endif



unsigned short f4memoNullChar = 0 ;  // CS 1999/09/15 changed from char to ushort for Unicode string
char *expr4buf = 0 ;

unsigned int numCode4 = 0 ;  /* used to determine when mem4reset() should be called */
extern int resetInProgress ;



#if defined(S4SEMAPHORE) && defined(S4OS2)
   #include <time.h>

   static char sem4mem[20], sem4expr[20] ;
   static HMTX hmtx4mem, hmtx4expr ;
#endif



#if defined(S4IBMOS2) && defined(S4OS2DLL) && !defined(__MULTI__)     /* no multi-thread code is being produced */
   int errno = 0 ;
#endif



#if defined( __DLL__ ) && defined( S4PASCAL ) && !defined( S4WIN32 )
   typedef char far* LPSTR ;
   typedef unsigned int HANDLE ;
   typedef unsigned short WORD ;
   #define PASCAL _pascal
   #define S4USE_WEP
#endif



#if defined( __DLL__ ) && !defined( S4PASCAL_DOS ) && defined( S4DLL )
   HINSTANCE cb5inst = (HINSTANCE)NULL ;
#endif



#if defined( __DLL__ ) && defined( S4OS2 )
   ULONG _dllmain (ULONG termflag, HMODULE modhandle)
   {
      #ifdef S4SEMAPHORE
         int i ;
         APIRET rc ;
         time_t t ;
      #endif

      if ( termflag == 0 )
      {
         #ifdef S4SEMAPHORE
            strcpy( sem4expr, "\\SEM32\\S4A" ) ;
            strcpy( sem4mem, "\\SEM32\\S4B" ) ;
            for ( i = 0 ; i < 100 ; i++ )
            {
               u4delaySec() ;
               time( &t ) ;
               t %= 10000L ;

               c4ltoa45( t, sem4expr + 10, -4 ) ;
               c4ltoa45( t, sem4mem + 10, -4 ) ;

               rc = DosCreateMutexSem( sem4expr, &hmtx4expr, 0, 0 ) ;
               if ( rc != 0 )
                  continue ;

               rc = DosCreateMutexSem( sem4mem, &hmtx4mem, 0, 0 ) ;
               if ( rc != 0 )
               {
                  DosCloseMutexSem( hmtx4expr ) ;
                  continue ;
               }
               return 1 ;
            }
         #else
            return 1 ;
         #endif
      }
      else
      {
         #ifdef S4SEMAPHORE
            DosCloseMutexSem( hmtx4mem ) ;
            DosCloseMutexSem( hmtx4expr ) ;
         #endif
      }

      return 1 ;
   }
#endif /* #if defined( __DLL__ ) && defined( S4OS2 ) */


#if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL )
   #ifdef __cplusplus
      extern "C"
   #endif
   #ifdef S4FLAT_THUNK
      BOOL WINAPI thk_ThunkConnect32 (LPSTR lpDll16, LPSTR lpDll32, HINSTANCE hDllInst, DWORD dwReason);
   #endif

   #ifdef S4WINCE
      BOOL APIENTRY DllMain( HANDLE hModule, DWORD reasonForCall, LPVOID reserved )
   #else
      BOOL APIENTRY DllMain( HINSTANCE hModule, DWORD reasonForCall, LPVOID reserved )
   #endif
   {
      if ( !cb5inst )
         cb5inst = (HINSTANCE) hModule ;

      #ifdef S4FLAT_THUNK
         if (!thk_ThunkConnect32("c4thk16.DLL", "c4dll.DLL", hModule, reasonForCall))
         {
            return FALSE;
         }
         switch (reasonForCall)
         {
            case DLL_PROCESS_ATTACH:
               break;
            case DLL_PROCESS_DETACH:
               break;
            case DLL_THREAD_ATTACH:
               break;
            case DLL_THREAD_DETACH:
               break;
         }
      #endif

      return TRUE;
   }
#endif



#if defined( __DLL__ ) && !defined( S4WIN32 ) && !defined( S4PASCAL_DOS ) && defined( S4WINDOWS )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int far PASCAL LibMain( HINSTANCE hInstance, WORD wDataSeg, WORD cbHeapSize, LPSTR lpCmdLine )
   {
      if ( !cb5inst )
         cb5inst = hInstance ;

      return 1 ;
   }
#endif



#if defined( __DLL__ ) && !defined( S4WIN32 )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4FUNCTION WEP( int nParameter )
   {
      return 1 ;
   }
#endif



#if defined( __DLL__ ) && !defined( S4PASCAL_DOS )
   HINSTANCE S4FUNCTION c4dllInst( void )
   {
      return cb5inst ;
   }
#endif


#ifndef S4SERVER
   long S4FUNCTION code4connectionId( CODE4 *c4 )   // CS 2001/05/17
   {
      #ifdef S4STAND_ALONE
         return 0;
      #else
         if (code4isConnected(c4))
            return c4->defaultServer.connect->clientId;

         return error4(c4, e4connect, E88057);
      #endif
   }
#endif


#if !defined(S4OFF_COMMUNICATIONS) && !defined(S4SERVER)
   /* not S4SERVER, not S4OFF_COMMUNICATIONS */
   static int code4userConnect( CODE4 *c4, const char *userName, const char *password )
   {
      int rc ;
      long version ;
      CONNECTION4 *connection ;
      static char defaultUser[] = "PUBLIC" ;

      if ( c4->defaultServer.connected == 0 )
         return 0 ;
      connection = &c4->defaultServer ;
      if ( connection == 0 )
         return 0 ;

      if ( userName == 0 )
         userName = defaultUser ;
      else
         if ( userName[0] == 0 )
            userName = defaultUser ;
      if ( password == 0 )
         password = "" ;

      connection4assign( connection, CON4CONNECT, 0L, 0L ) ;
      version = htonl5(S4VERSION) ;
      connection4addData( connection, &version, sizeof( version ), NULL ) ;
      connection4addData( connection, userName, strlen( userName ) + 1, NULL  ) ;
      connection4addData( connection, password, strlen( password ) + 1, NULL  ) ;
      if (c4->applicationVerify != NULL)
         connection4addData( connection, c4->applicationVerify, strlen( c4->applicationVerify ) + 1, NULL  ) ;
      else
         connection4addData( connection, " ", 2, NULL  ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return rc ;
      if ( connection4type( connection ) != CON4CONNECT )
         connection4error( connection, c4, e4net, E91005 ) ;
      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         if (rc == e4invalidLicence )
         {
            char *company;

            company = connection4data( connection );
            connection4errorDescribeExecute(connection, c4, rc,0,"This CodeBase software is licensed for use only with applications developed by ",company,0);
         }
         else
            connection4error( connection, c4, rc, E91005 ) ;
         return rc ;
      }
      if ( connection4len(connection) != sizeof(S4LONG) )
         return e4packetLen ;
      c4->clientConnect.clientId = *((long *)connection4data(connection)) ;
      /*CJ-04/28/99-These global members will be intialized on initalize of CODE4. */
      if ( rc >= 0 )
      {
         switch( code4indexFormat( c4 ) )
         {
            case r4cdx:
               s5fox = TRUE ;
               break ;
            case r4ntx:
               s5clipper = TRUE ;
               break ;
            case r4mdx:
               s5mdx = TRUE ;
               s5hasDescending = FALSE ;
               break ;
         }
      }

      return rc ;
   }



   short S4FUNCTION code4isConnected( CODE4 *c4 )
   {
      /* DESCRIPTION

         This function determines whether a connection has been established.
         If a connection has been established but has since been lost
         (e.g. the server has been shut down), the behavior is undefined. In
         stand-alone, it will always return false.

         RETURNS

           0 - The CODE4 is not connected or the library is built for stand-alone
         > 0 - The CODE4 is connected to the server.
         < 0 - A error occurred.
      */
      #ifdef S4STAND_ALONE
         return 0 ;
      #else
         if ( c4->defaultServer.connected )
         {
            if ( c4->defaultServer.connectionFailure < 0 )   /* means connection was deleted, must call code4initUndo() to recover */
               return error4( c4, e4connection, E81007 ) ;
            return r4connected ;
         }
         else
            return 0 ;
      #endif
   }


   /* not S4SERVER, not S4OFF_COMMUNICATIONS */
   int S4FUNCTION code4connect( CODE4 *c4, const char *serverId, const char *processId, const char *userName, const char *password, const char *protocol )
   {
      int rc ;
      unsigned short portNo;
      CONNECT4 *connect ;

      #ifdef E4VBASIC
         if ( c4parm_check( c4, 1, E91004 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E91004 ) ;
      #endif

      if ( c4->defaultServer.connected )
      {
         if ( c4->defaultServer.connectionFailure < 0 )   /* means connection was deleted, must call code4initUndo() to recover */
            return error4( c4, e4connection, E81007 ) ;
         return r4connected ;
      }

      c4->defaultServer.serverName[0] = 0 ;
      c4->defaultServer.port[0] = 0 ;
      c4->defaultServer.userName[0] = 0 ;
      c4->defaultServer.password[0] = 0 ;

      if ( serverId == 0 )
         serverId = DEF4SERVER_ID ;
      else
         if ( serverId[0] == 0 )
            serverId = DEF4SERVER_ID ;
      if ( processId == 0 )
         processId = DEF4PROCESS_ID ;
      else
         if ( processId[0] == 0 )
            processId = DEF4PROCESS_ID ;
      if (c4->connectLowMemory == NULL )
         c4->connectLowMemory = mem4create(c4, MEMORY4START_CONNECT_LOW, sizeof(CONNECT4LOW), MEMORY4EXPAND_CONNECT_LOW, 0 ) ;

      portNo = (unsigned short)c4atol(processId, strlen(processId));
      if (portNo == 0)
         return error4( c4, e4parm, E91004 ) ;

      connect = &c4->clientConnect ;
      rc = connect4connect(connect, c4, portNo, serverId ) ;
      if ( rc < 0 )
         return rc ; /* An error4() has already been called */

      connection4init(&c4->defaultServer, connect ) ;
      c4->defaultServer.connected = 1 ;
      #ifndef S4UTILS
         rc = code4userConnect( c4, userName, password ) ;
      #endif

      if ( rc == 0 )
      {
         rc = code4dateFormatSet( c4, code4dateFormat( c4 ) ) ;
         c4->ignoreCase = ! (code4serverOS( c4 ) & OS4UNIX ) ;  /* CS 2001/01/22 */
      }
      if ( rc < 0 )
         connection4initUndo( &c4->defaultServer ) ;

      // CS 2000/01/16 keep connection parameters
      if (serverId)
      {
         strncpy( c4->defaultServer.serverName, serverId, sizeof( c4->defaultServer.serverName ) ) ;
         c4->defaultServer.serverName[sizeof( c4->defaultServer.serverName ) - 1] = 0;
      }

      if (processId)
      {
         strncpy( c4->defaultServer.port, processId, sizeof( c4->defaultServer.port ) ) ;
         c4->defaultServer.port[sizeof( c4->defaultServer.port ) - 1] = 0;
      }

      if (userName)
      {
         strncpy( c4->defaultServer.userName, userName, sizeof( c4->defaultServer.userName ) ) ;
         c4->defaultServer.userName[sizeof( c4->defaultServer.userName ) - 1] = 0;
      }

      if (password)
      {
         strncpy( c4->defaultServer.password, password, sizeof( c4->defaultServer.password ) ) ;
         c4->defaultServer.password[sizeof( c4->defaultServer.password ) - 1] = 0;
      }

      return rc ;
   }



   #ifdef S4UTIL
      /* S4UTIL, not S4SERVER, not S4OFF_COMMUNICATIONS */
      int S4FUNCTION code4passwordSet( CODE4 *c4, const char *userId, const char *oldPass, const char *newPass ) ;
      {
         CONNECTION4 *connection ;
         unsigned short int len[3] ;

         #ifdef E4PARM_HIGH
            if ( userId == 0 || oldPass == 0 || newPass == 0 )
               return error4( c4, e4parmNull, E91017 ) ;
         #endif

         connection = c4->defaultServer->connect ;
         if ( connection == 0 )  /* not connected */
            return error4( c4, e4connection, E91017 ) ;

         connection4assign( connection, CON4PASSWORD_SET, 0L, 0L ) ;
         len[0] = strlen( userId ) ;
         len[1] = strlen( oldPass ) ;
         len[2] = strlen( newPass ) ;
         connection4addData( connection, len, sizeof( len ), NULL  ) ;
         connection4addData( connection, userId, len[0], NULL  ) ;
         connection4addData( connection, oldPass, len[1], NULL  ) ;
         connection4addData( connection, newPass, len[2], NULL  ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receive( connection ) ;
         if ( rc < 0 )
            return rc ;
         return connection4status( connection ) ;
      }
   #endif /* S4UTIL */
#endif /* !S4SERVER && !S4OFF_COMMUNICATIONS */



int g_extraInits = 0 ;   // track extra mem4inits that are done by code4allocLow, allow to release later


#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION code4initLow( CODE4 *c4, const char *defaultProtocol, long versionId, long structSize, char skipCom )
{
   /* skipCom is used to skip communications stuff for debug purposes (ats) to keep things simpler, we don't need to connect anyway */
   #if defined(S4TESTING) || defined(S4TRACK_FILES)
      int openFlag ;
      char *envLog ;
      char *logFile = "T4TEST.log" ;
   #endif
   int rc ;
   #if !defined(S4OFF_OPTIMIZE) && defined(S4OPTIMIZE_STATS)
      DATA4 *stat ;
      int oldOffErr ;
   #endif
   #ifndef S4STAND_ALONE
      #ifdef S4WINTEL
         WSADATA WSAData ;
      #endif
   #endif

   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
         return error4( 0, e4parm, E91001 ) ;
   #endif

   if ( versionId != S4VERSION )
   {
      char errHeader[20], errLib[20] ;
      #ifdef S4WINCE
         strcpy( errLib, "Library: ") ;
         _itoa( S4VERSION, errLib + 9, 10 ) ;
         strcpy( errHeader, "Headers: ") ;
         _itoa( versionId, errHeader + 9, 10 ) ;
      #else
         c4sprintf( errLib,    "Library: %ld", S4VERSION ) ;
         c4sprintf( errHeader, "Headers: %ld", versionId ) ;
      #endif
      return error4describe( 0, e4version, E91001, errLib, errHeader, 0 ) ;
   }

   if ( structSize )   /* bypass check if zero passed */
      if ( structSize != (long)sizeof( CODE4 ) )  /* error, switch mismatch most likely */
      {
         char errHeader[30], errLib[30] ;
         #ifdef S4WINCE
            strcpy( errLib, "Library struct size: " );
            _ltoa( sizeof( CODE4 ), errLib + 21, 10 );
            strcpy( errHeader, "Header struct size: " );
            _ltoa( structSize, errHeader + 20, 10 );
         #else
            /* LY 00/09/20 : cast sizeof() to long (win64) */
            c4sprintf( errLib, "Library struct size: %ld", (long)sizeof( CODE4 ));
            c4sprintf( errHeader, "Header struct size: %ld", structSize );
         #endif
         return error4describe( 0, e4verify, E81507, errLib, errHeader, 0 ) ;
      }

   /***********************************************************************

      This code must reside at the beginning of the function to ensure that
      no file open/create incorrectly auto-starts the optimization process

    ***********************************************************************/

   #if defined( S4TESTING ) || defined( S4TRACK_FILES )
      if ( numCode4 == 0 )
      {
         memset( &s4test, 0, sizeof( s4test ) ) ;
         s4test.hand = INVALID4HANDLE ;
      }
   #endif

   c4memset( (void *)c4, 0, sizeof( CODE4 ) ) ;

   #ifdef S4MACINTOSH
      c4->macDir = *(long *)0x398 ;
      c4->macVol = -(*(short *)0x214) ;
   #endif

   /* if a DLL, can't check the stack length since this is a separate executable */
   #if !defined( S4WIN32 ) && !defined( S4WINDOWS )
      #if !defined( __DLL__ ) && defined( __TURBOC__ ) && !defined( S4OS2PM )
         if ( _stklen < 5000U ) /* 5000 seems to be an appropriate minimum */
               return error4( 0, e4result, E81003 ) ;
      #endif
      #ifdef _MSC_VER
         if ( stackavail() < 5000U )
            error4( 0, e4result, E81003 ) ;
      #endif
   #endif

   if ( numCode4 == 0 && resetInProgress == 0 )
   {
      // if extraInits, then reduce by 1 instead of doing init now...to avoid not resetting memory...
      if ( g_extraInits == 0 )
         mem4init() ;
      else
         g_extraInits-- ;
   }
   numCode4++ ;

   #if defined( S4TESTING ) && defined( S4STAND_ALONE ) && ! defined( S4OFF_TRAN )
      c4->doTransLocking = 1 ;   /* for testing, to allow one program to have multiple transactions in progress */
   #endif

   #if defined(E4ANALYZE) || defined(E4VBASIC)
      c4->debugInt = E4DEBUG_INT ; /* Some random value for double checking. */
   #endif

   #ifndef S4OFF_MEMO
      c4->memSizeMemoExpr = 0x400 ;        /*  1024 */
   #endif

   c4->numericStrLen = 17 ;    /* default length for clipper numeric keys is 10  */
   c4->decimals = 2 ;

   /* Flags initialization */

   #ifndef S4SERVER
      c4setErrCreate( c4, 1 ) ;
   #endif
   c4->errDefaultUnique = r4uniqueContinue ;
   c4->errExpr = 1 ;
   c4->errFieldName = 1 ;
   c4->errOpen = 1 ;
   #ifndef S4OFF_INDEX
      c4->errTagName = 1 ;
   #endif
   c4->autoOpen = 1 ;
   c4->safety = 1 ;

   #ifndef S4OFF_MULTI
      c4->lockDelay = 100 ;
   #endif

   c4->collatingSequence = sort4machine ;   /* default to FoxPro 2.x values */
   c4->codePage = cp0 ;

   c4->memStartDataFile = 5 ;
   c4->memExpandDataFile = 5 ;
   #ifndef S4OFF_INDEX
      c4->memStartTagFile = 10 ;
      c4->memExpandTagFile =5 ;
   #endif

   #ifndef S4SINGLE
      c4->memStartLock = 5 ;
      c4->memExpandLock = 10 ;
   #endif

   #ifndef S4CLIPPER
      c4->memStartIndexFile = 5 ;
      c4->memExpandIndexFile = 5 ;
   #endif

   #ifndef S4OFF_MULTI
      c4->lockAttemptsSingle = 1 ;
   #endif

   #ifdef S4SERVER
      c4->currentClient = 0 ;
      #ifdef S4DEBUG_LOG
         c4->logFile.hand = INVALID4HANDLE ;
      #endif
      c4->lockAttempts = 1 ;
      c4->memStartClient = 5 ;
      c4->memExpandClient = 5 ;
      #ifdef S4CLIPPER
         c4->memStartIndex = 10 ;
         c4->memStartTag = 10 ;
         c4->memExpandIndex = 5 ;
         c4->memExpandTag = 10 ;
      #endif
   #else
      c4->errGo = 1 ;
      c4->errSkip = 1 ;
      c4->errRelate = 1 ;
      #ifndef S4OFF_MULTI
         c4->lockAttempts = WAIT4EVER ;   /* wait forever */
      #endif
      c4->singleOpen = 1 ;
      #ifndef S4OFF_ENFORCE_LOCK
         c4->lockEnforce = 0 ;
      #endif
      #ifdef S4OFF_OPTIMIZE
         #ifndef S4OFF_INDEX
            c4->memSizeBlock = 0x400 ;        /*  1024 */
         #endif
      #else
         c4->memSizeBlock = 0x400 ;        /*  1024 */
      #endif
      c4->memSizeSortPool = 0xF000 ;    /* 61440 */
      c4->memSizeSortBuffer = 0x8000 ;  /* 32768 */
      c4->memSizeBuffer = 0x8000 ;      /* 32768 */
      #ifndef S4OFF_MEMO
         c4->memSizeMemo = 0x200 ;         /*   512 */
      #endif
      c4->memStartData = 10 ;
      c4->memExpandData = 5 ;
      #ifndef S4OFF_INDEX
         c4->memExpandBlock = 10 ;
         c4->memStartBlock = 10 ;
         c4->memStartIndex = 10 ;
         c4->memStartTag = 10 ;
         c4->memExpandIndex = 5 ;
         c4->memExpandTag = 10 ;
      #endif
   #endif

   #ifdef S4CLIENT_OR_FOX
      c4->collateName = collate4none ;  // by default none, means machine and/or use old method
      c4->collateNameUnicode = collate4machine ;  // by default use machine order for unicode
      c4->codeCollateSetValue = collate4machine ;
      #ifndef S4OFF_INDEX
         c4->foxCreateIndexBlockSize = 512 ;  // default fox block size is 512 bytes...
         c4->foxCreateIndexMultiplier = 1 ;        // default fox multiplier is 1...
      #endif
   #endif

   #ifndef S4CLIENT
      c4->doIndexVerify = 1 ;
   #endif

   #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
      #ifndef S4CLIENT
         c4->log = LOG4TRANS ;  /* AS 09/08/98 - default changed */
      #endif
      #ifdef S4STAND_ALONE
         c4->logOpen = 1 ;
      #endif
   #endif

   #ifndef S4OFF_COMMUNICATIONS
      #ifdef S4TIMEOUT_HOOK
         c4->timeout = 100 ;
      #else
         c4->timeout = -1 ;
      #endif
   #endif

   #ifndef S4OPTIMIZE_OFF
      c4->doOpt = 1 ;   /* by default do optimization */
      #ifndef S4SERVER
         c4->optimize = OPT4EXCLUSIVE ;   /* by default optimize non-shared files */
         c4->optimizeWrite = OPT4EXCLUSIVE ;
      #endif  /* S4SERVER */
   #endif  /* not S4OPTIMIZE_OFF */

   #ifndef S4SERVER
      rc = code4tranInit( c4 ) ;
      if ( rc < 0 )
      {
         numCode4--;
         return error4( 0, rc, E91001 ) ;
      }

      #ifndef S4OFF_COMMUNICATIONS
         c4->clientDataCount = 1 ;
      #endif

      #ifndef S4LANGUAGE
         code4dateFormatSet( c4, "MM/DD/YY" ) ;
      #else
         #ifdef S4GERMAN
            code4dateFormatSet( c4, "DD.MM.YY" ) ;
         #endif
         #ifdef S4FRENCH
            code4dateFormatSet( c4, "MM/DD/YY" ) ;
         #endif
         #ifdef S4SWEDISH
            code4dateFormatSet( c4, "YYYY-MM-DD" ) ;
         #endif
         #ifdef S4FINNISH
            code4dateFormatSet( c4, "YYYY-MM-DD" ) ;
         #endif
         #ifdef S4NORWEGIAN
            code4dateFormatSet( c4, "DD-MM-YYYY" ) ;
         #endif
      #endif
   #endif

   #if defined( S4FILE_EXTENDED ) && defined( S4FOX ) && ( !defined( S4ODBC_BUILD ) || defined( S4STAND_ALONE ) )
      // skip this code with server S4ODBC_BUILD - in that case we check the S4ODBC_ENABLED CodeServer component for its setting of large
      // file offset to see if it should be enabled during the server4init() procedure.  This avoid having to set both components
      // of having server components with 2 different settings
      if ( largeStamp.doLargeLocking )
         code4largeOnLow( c4 ) ;
      else
         largeStamp.previousCode4init++ ;
   #endif

   #if !defined( S4CLIENT )
      long dwswitch = u4switch() ;
      if ( dwswitch & 1L )
         s5fox = TRUE ;
      if ( dwswitch & 2L )
         s5clipper = TRUE ;
      if ( dwswitch & 4L )
      {
         s5mdx = TRUE ;
         s5hasDescending = FALSE ;
      }
   #endif

   c4->initialized = 1 ;

   if ( numCode4 == 1 )
   {
      #ifdef S4TESTING
         openFlag = c4->errOpen ;
         int exclusiveFlag = c4->accessMode ;
         c4->errOpen = 0 ;
         c4->accessMode = OPEN4DENY_NONE ;
         envLog = getenv( "T4LOG" ) ;
         if ( envLog )           /* if env. var. exists, use it */
            logFile = envLog ;
         if ( file4open( &s4test, c4, logFile, 0 ) != r4success )
            file4create( &s4test, c4, logFile, 0 ) ;
         c4->accessMode = exclusiveFlag ;
         c4->errOpen = openFlag ;
      #else
         #ifdef S4TRACK_FILES
            openFlag = c4->errOpen ;
            c4->errOpen = 0 ;
            envLog = getenv( "CB51_LOG" ) ;
            if ( envLog )           /* if env. var. exists, use it */
               logFile = envLog ;
            if ( file4open( &s4test, c4, logFile, 0 ) != r4success )
               file4create( &s4test, c4, logFile, 0 ) ;
            c4->errOpen = openFlag ;
         #endif
      #endif
   }

   #ifndef S4SERVER
      #ifndef S4SINGLE
         #ifdef S4CB51
            code4unlockAutoSet( c4, LOCK4DATA ) ;
         #else
            code4unlockAutoSet( c4, LOCK4ALL ) ;
         #endif
      #endif
   #endif

   if ( skipCom != 1 )   /* for debug/ats, don't do stats/delay writes/advance reads */
   {
      #ifndef S4OFF_OPTIMIZE
         #ifdef S4OPTIMIZE_STATS
            stat = c4->statusDbf ;
            if ( stat == 0 )  /* open the stat database */
            {
               oldOffErr = c4->errOpen ;
               c4->errOpen = 0 ;
               c4->statusDbf = d4open( c4, "OPT4STAT" ) ;
               c4->errOpen = oldOffErr ;
               stat = c4->statusDbf ;
               if ( stat == 0 )
                  error4set( c4, 0 ) ;
               else
               {
                  d4optimize( stat, OPT4OFF ) ;
                  c4->typeFld = d4field( stat, "TYPE" ) ;
                  c4->fileNameFld = d4field( stat, "FILE_NAME" ) ;
                  c4->offsetFld = d4field( stat, "OFFSET" ) ;
                  c4->lengthFld = d4field( stat, "LENGTH" ) ;
                  if ( c4->typeFld == 0 || c4->fileNameFld == 0 || c4->offsetFld == 0 || c4->lengthFld == 0 )
                  {
                     d4close( stat ) ;
                     c4->statusDbf = 0 ;
                  }
               }
            }
         #endif
      #endif /* S4OFF_OPTIMIZE */

      #ifdef S4ENCRYPT_HOOK
         InitializeCriticalSection( &c4->fileEncryptedCritical ) ;
      #endif

      #if defined(S4WRITE_DELAY) || defined(S4READ_ADVANCE)
         clock_t clockStart ;
      #endif

      #ifdef S4WRITE_DELAY
         /* if the begin thread fails, then delay-writes are simply not enabled,
            which is ok because it will simply be bypassed. */
         /* CS 2000/05/10 Also disable delay-writes if begin thread succeeds,
            but thread fails to start after 5 seconds. Possible MS bug? */
         InitializeCriticalSection( &c4->critical4delayWriteList ) ;
         c4->pendingWriteEvent = CreateEvent( 0, TRUE, FALSE, 0 ) ;
         #ifdef S4WIN64  /* LY 00/09/20 : win64 */
            if ( c4->pendingWriteEvent != NULL )
         #else
            if ( c4->pendingWriteEvent != INVALID4HANDLE )
         #endif
         {
            rc = (int)_beginthread( file4writeDelayMain, 5000, c4 ) ;
            if ( rc != -1 )
            {
               clockStart = clock() ;
               while ( c4->delayWritesEnabled != 1 )  /* ensure thread starts to avoid initUndo thread corruptions */
               {
                  if ( ( clock() - clockStart ) / CLOCKS_PER_SEC >= 5 )
                  {
                     rc = -1 ;
                     break ;
                  }
                  Sleep( 0 ) ;
               }
            }
            if ( rc < 0 )
            {
               CloseHandle( c4->pendingWriteEvent ) ;
               #ifdef S4WIN64 /* LY 00/09/20 */
                  c4->pendingWriteEvent = NULL ;
               #else
                  c4->pendingWriteEvent = INVALID4HANDLE ;
               #endif
               DeleteCriticalSection( &c4->critical4delayWriteList ) ;
            }
         }
      #endif

      #ifdef S4READ_ADVANCE
         /* if the begin thread fails, then delay-reads are simply not enabled,
            which is ok because it will simply be bypassed. */
         /* CS 2000/05/10 Also disable delay-reads if begin thread succeeds,
            but thread fails to start after 5 seconds. Possible MS bug? */
         InitializeCriticalSection( &c4->critical4advanceReadList ) ;
         c4->pendingReadEvent = CreateEvent( 0, TRUE, FALSE, 0 ) ;
         #ifdef S4WIN64  /* LY 00/09/20 */
            if ( c4->pendingReadEvent != NULL )
         #else
            if ( c4->pendingReadEvent != INVALID4HANDLE )
         #endif
         {
            rc = (int)_beginthread( file4advanceReadMain, 5000, c4 ) ;
            if ( rc != -1 )
            {
               clockStart = clock() ;
               while ( c4->advanceReadsEnabled != 1 )  /* ensure thread starts to avoid initUndo thread corruptions */
               {
                  if ( ( clock() - clockStart ) / CLOCKS_PER_SEC >= 5 )
                  {
                     rc = -1 ;
                     break ;
                  }
                  Sleep( 0 ) ;
               }
            }
            if ( rc < 0 )
            {
               CloseHandle( c4->pendingReadEvent ) ;
               #ifdef S4WIN64
                  c4->pendingReadEvent = NULL ;
               #else
                  c4->pendingReadEvent = INVALID4HANDLE ;
               #endif
               DeleteCriticalSection( &c4->critical4advanceReadList ) ;
            }
         }
      #endif
   }

   /***********************************************************************

      This code must reside at the end of the function to ensure that no
      file open/create incorrectly auto-starts the optimization process

    ***********************************************************************/
   #ifndef S4OFF_OPTIMIZE
      #if !defined(S40S2) && !defined(S4WINDOWS) && defined(S4WINTEL)
         c4->memStartMax = 0x50000L ;
      #else
         c4->memStartMax = 0xF0000L ;
      #endif /* !S40S2 && !S4WINDOWS && S4WINTEL */
      // c4->hadOpt = 1 ;  // AS 03/08/01 - Don't disable the optimization on the get go...
      #ifdef S4SERVER
         c4->memMaxPercent = -1 ;   /* use server configuration file */
      #else
         // AS 04/20/01 - change to set values at code4init time
         c4->memMaxPercent = 25 ;   /* default use 25% of available memory */
         code4memStartMaxSet( c4, c4->memMaxPercent ) ;  /* start optimization if not enabled and not suspended */
      #endif

   #endif /* S4OFF_OPTIMIZE */

   c4->skipCom = skipCom ;

   if ( skipCom == 0 )
   {
      #ifndef S4STAND_ALONE
         #ifdef S4WINTEL
            rc = WSAStartup( MAKEWORD( 1,1 ), &WSAData ) ;
         #endif
         c4->ver4 = code4osVersion() ;
         if ( c4->ver4 <= 0 )
         {
            numCode4--;
            return error4( 0, e4result, E91001 ) ;
         }
         c4->readMessageBufferLen = READ4MESSAGE_BUFFER_LEN ;

         #ifdef S4SERVER
            #ifndef S4UNIX
               c4->maxSockets = WSAData.iMaxSockets ;
            #endif
            #ifndef S4OFF_THREAD
               list4mutexInit( &c4->connectsToService ) ;
            #endif
         #endif

         #ifndef S4OFF_THREAD
            list4mutexInit(&c4->writeBuffersAvail) ;
            list4mutexInit(&c4->connectBufferListMutex) ;
         #endif

         if (c4->ver4 == ver4NT )
            c4->readMessageNumBuffers = READ4MESSAGE_NUM_BUFFER ;
         else
            c4->readMessageNumBuffers = 1 ;
         c4->writeMessageBufferLen = WRITE4MESSAGE_BUFFER_LEN ;
         c4->writeMessageNumOutstanding = WRITE4MESSAGE_NUM_BUFFER ;

         #ifdef S4COMM_THREAD
            /* and now start the communications thread */
            c4->inter = (INTER4 *)u4alloc( sizeof( INTER4 ) ) ;
            if ( c4->inter == 0 )
            {
               numCode4--;
               return error4( 0, e4memory, E91001 ) ;
            }
            if ( inter4init( c4->inter, c4 ) < 0 )
            {
               numCode4--;
               return error4( 0, e4result, E91001 ) ;
            }
            c4->interThreadHandle = _beginthread( inter4, 5000, c4->inter ) ;
            if ( c4->interThreadHandle == -1 )
            {
               inter4initUndo( c4->inter ) ;
               numCode4--;
               return error4( 0, e4result, E91001 ) ;
            }
         #endif
      #endif

      #ifdef S4SERVER
         #ifndef S4OFF_THREAD
            c4->accessMutex = CreateMutex( NULL, FALSE, NULL ) ;
            semaphore4init( &c4->workerSemaphore ) ;
         #endif
      #endif
   }

   #ifdef TIMER5OUT
      /* case where we want to perform timing of c4 operations */
      if ( rc == 0 && skipCom == 0 )
      {
         c4->errOff = 1 ;
         char oldSafety = c4->safety ;
         c4->safety = 0 ;
         rc = file4create( &c4->timerFile, c4, "TIMER5.OUT", 0 ) ;
         c4->safety = oldSafety ;
         if ( rc != 0 )  /* maybe another CODE4 has it in use */
         {
            error4set( c4, 0 ) ;
            c4->safety = 0 ;
            rc = file4create( &c4->timerFile, c4, "TIMER5B.OUT", 0 ) ;
            c4->safety = oldSafety ;
            if ( rc != 0 )
            {
               error4set( c4, 0 ) ;
               c4->safety = 0 ;
               rc = file4create( &c4->timerFile, c4, "TIMER5C.OUT", 0 ) ;
               c4->safety = oldSafety ;
               if ( rc != 0 )
               {
                  error4set( c4, 0 ) ;
                  c4->safety = 0 ;
                  rc = file4create( &c4->timerFile, c4, "TIMER5D.OUT", 0 ) ;
                  c4->safety = oldSafety ;
                  if ( rc != 0 )
                     rc = error4describe( c4, e4open, E91001, "TIMER5.OUT", 0, 0 ) ;
               }
            }
         }
         c4->timerMemory = timerMemory5create( c4 ) ;
         if ( c4->timerMemory == 0 )
            rc = e4memory ;
         c4->errOff = 0 ;
      }
   #endif /* TIMER5OUT */

   #ifdef S4CLIENT_OR_FOX
      c4->compatibility = 25 ;
   #endif

   #ifdef S4WIN32  /* LY 99/06/17 : for CS */
      c4->memZeroAllocator = new Mem5zeroAllocator() ;
      c4->memNonZeroAllocator = new Mem5allocator() ;
   #endif

   #ifdef S4CASE_SEN
      c4->ignoreCase = 0 ;
   #else
      c4->ignoreCase = 1 ;
   #endif

   #if defined(S4WINCE) && defined(E4VBASIC)
      c4->fldInfo = 0 ;
      c4->fldInfoSize = 0 ;
      c4->fldInfoLength = 0 ;
      c4->tagInfo = 0 ;
      c4->tagInfoSize = 0 ;
      c4->tagInfoLength = 0 ;
   #endif

   c4->autoIncrementStart = 1.0 ;

   // AS 02/09/01 - add encryption
   #ifdef S4ENCRYPT_HOOK
      short keyLen = 0 ;
      const void *key = code4encryptGetKey( &keyLen ) ;
      c4->encryptInit = code4encryptInitHook( c4, key, keyLen ) ;
      #ifdef S4TESTING
         // if testing, ensure that encryption is on to ensure it gets tested
         c4->encrypt = 1 ;
      #endif
   #endif
   return 0 ;
}



#ifdef TIMER5OUT
   /* TIMER5OUT */
   void code4timerStart( CODE4 *code, char *label )
   {
      if ( code->currentTimer == 0 )
         code->currentTimer = timer5create( code->timerMemory, label ) ;
      else
         code->currentTimer = timer5createSub( code->currentTimer, label ) ;

      if ( code->masterTimer == 0 )
         code->masterTimer = code->currentTimer ;
   }



   /* TIMER5OUT */
   void code4timerStop( CODE4 *code )
   {
      Timer5 *timer = code->currentTimer ;

      if ( timer == 0 )
         return ;

      timer5stop( timer ) ;

      timer5displayResults( code->masterTimer, &code->timerFile ) ;

      code->currentTimer = timer->master ;

      if ( code->currentTimer == 0 )
         code->masterTimer = 0 ;

      timer5destroy( timer ) ;
   }
#endif /* TIMER5OUT */




/* if doInit is set to 1, code4init() is called on the allocated CODE4 */
CODE4 *S4FUNCTION code4allocLow( int doInit, const char *defaultProtocol, long versionId )
{
   #if defined( S4TESTING ) || defined( S4TRACK_FILES )
      // AS 08/16/99 - was sometimes not getting set, and then an error here causing endless loop...
      if ( numCode4 == 0 )
      {
         memset( &s4test, 0, sizeof( s4test ) ) ;
         s4test.hand = INVALID4HANDLE ;
      }
   #endif

   mem4init() ;
   g_extraInits++ ;

   CODE4 *c4 ;
   #ifndef S4CBPP
      c4 = (CODE4 *)u4alloc( (long)sizeof( CODE4 ) ) ;
   #else
      #ifdef __cplusplus
         c4 = (CODE4 *)u4alloc( (long)sizeof( Code4 ) ) ;
      #else
         c4 = (CODE4 *)u4alloc( (long)sizeof( CODE4 ) ) ;
      #endif
   #endif

   if ( c4 == 0 )
      return 0 ;

   if ( doInit == 1 )
   {
      code4initLow( c4, defaultProtocol, versionId, sizeof( CODE4 ), 0 ) ;
      c4->didAlloc = 1 ;
   }
   return c4 ;
}




CODE4 *S4FUNCTION code4initAllocLow( const char *defaultProtocol )
{
   CODE4 *c4 ;

   #ifndef S4CBPP
      c4 = (CODE4 *)u4alloc( (long)sizeof( CODE4 ) ) ;
   #else
      #ifdef __cplusplus
         c4 = (CODE4 *)u4alloc( (long)sizeof( Code4 ) ) ;
      #else
         c4 = (CODE4 *)u4alloc( (long)sizeof( CODE4 ) ) ;
      #endif
   #endif

   if ( c4 == 0 )
      return 0 ;

   code4initLow( c4, defaultProtocol, S4VERSION, sizeof( CODE4 ), 0 ) ;
   c4->didAlloc = 1 ;

   return c4 ;
}



unsigned short S4FUNCTION code4actionCode( CODE4 S4PTR *c4 )
{
   #ifdef TIME4STATUS
      return c4->actionCode ;
   #else
      error4( c4, e4notSupported, 0 ) ;
      return 0 ;
   #endif
}



void S4FUNCTION expr4calcDelete( CODE4 *c4, EXPR4CALC *calc )
{
   if ( calc == 0 )
      return;

   #ifndef S4SERVER
      if( calc->total != 0 )
      {
         if ( calc->total->resetExpression != 0 )
            expr4free( calc->total->resetExpression ) ;
         l4remove( &c4->totalList, calc->total ) ;
         mem4free( c4->totalMemory, calc->total ) ;
      }
   #endif
   #ifdef S4SERVER
      l4remove( &c4->currentClient->calcList, calc ) ;
   #else
      l4remove( &c4->calcList, calc ) ;
   #endif

   if ( calc->expr != 0 )
      expr4free( calc->expr ) ;

   mem4free( c4->calcMemory, calc ) ;
}



void S4FUNCTION code4calcReset( CODE4 *c4 )
{
   LIST4 *list ;
   #ifdef S4CLIENT
      CONNECTION4 *connection ;
      int rc ;
   #endif

   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
      {
         error4( 0, e4parm_null, E90921 ) ;
         return ;
      }
   #endif

   #ifdef S4SERVER
      list = &c4->currentClient->calcList ;
   #else
      list = &c4->calcList ;
   #endif

   if( list != 0 )
   {
      while( list->nLink > 0 )
         expr4calcDelete( c4, (EXPR4CALC *)list->lastNode ) ;

      #ifdef S4CLIENT
         if ( c4->defaultServer.connected )
         {
            connection = &c4->defaultServer ;
            rc = connection4assign( connection, CON4CALC_RESET, 0, 0 ) ;
            if ( rc < 0 )
               return ;
            connection4sendMessage( connection ) ;
            rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               return ;
            rc = connection4status( connection ) ;
            if ( rc < 0 )
               connection4error( connection, c4, rc, E90921 ) ;
         }
      #endif
   }
}



#ifdef S4MEM_PRINT
   extern char *write4buf ;
   extern FILE4SEQ_WRITE *file4seqPtr ;
#endif


#if defined( S4TESTING ) && defined( S4MEM_PRINT )
   extern int in4temp ;
#endif

static int code4initUndo2( CODE4 *c4, int doClose )
{
   #if defined( S4MULTI_SERVER ) && defined( S4CLIENT )
      SOCKET4 *socket, *socketNext ;
   #endif

   #ifndef S4SERVER
      int errCode ;
   #endif

   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
         return e4parm_null ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( c4, 1, E40138 ) )
         return -1 ;
   #endif

   if ( c4->initialized == 0 )  /* already uninitialized */
      return 0 ;

   #ifdef E4MISC
      if ( numCode4 == 0 )   /* code4initUndo already called... */
      {
         if ( doClose == 1 )   /* if zero, don't error out since severe already */
            return error4( 0, e4info, E81004 ) ;
         else
            return e4info ;
      }
   #endif

   error4set(c4, 0 ) ;

   #if defined(S4WINCE) && defined(E4VBASIC)
      if ( c4->fldInfo )
         field4infoFree( c4 ) ;
      if ( c4->tagInfo )
         tag4infoFree( c4 ) ;
   #endif

   #ifdef S4SEMAPHORE
      #ifdef S4OS2
         DosCloseMutexSem( hmtx4mem ) ;
         DosCloseMutexSem( hmtx4expr ) ;
      #endif
   #endif

   #ifndef S4OPTIMIZE_OFF
      code4optSuspend( c4 ) ;
   #endif

   #ifdef S4SERVER
      #ifndef S4UNIX
         CloseHandle( c4->accessMutex ) ;
      #endif
   #else
      #ifndef S4OFF_TRAN
         #ifndef S4CLIENT
            if ( code4transEnabled( c4 ) )
         #endif
         if ( code4tranStatus( c4 ) == r4active )
            code4tranRollback( c4 ) ;
      #endif

      #if !defined(S4SERVER) && !defined(S4OFF_TRAN)
         if ( c4->transFileName != 0 )
         {
            u4free( c4->transFileName ) ;
            c4->transFileName = 0 ;
         }
      #endif

      if ( doClose )
         code4close( c4 ) ;

      if ( c4->skipCom == 0 )
      {
         code4calcReset( c4 ) ;
         #ifndef S4OFF_COMMUNICATIONS
            #ifdef S4MULTI_SERVER
               socketNext = (SOCKET4 *)l4first( &c4->servers ) ;
               for ( ;; )
               {
                  socket = socketNext ;
                  if ( socket == 0 )
                     break ;
                  socketNext = (SOCKET4 *)l4next( &c4->servers, socket ) ;
                  socket4initUndo( socket ) ;
                  l4remove( &c4->servers, socket ) ;
                  socket4free( socket ) ;
               }
            #else
               connection4initUndo( &c4->defaultServer ) ;
            #endif
            c4->defaultServer.connected = 0 ;
         #endif
         #ifndef S4OFF_TRAN
            code4tranInitUndo( c4 ) ;
         #endif
      }
   #endif

   #ifdef S4OPTIMIZE_STATS
      if ( c4->statusDbf != 0 )
      {
         d4close( c4->statusDbf ) ;
         c4->statusDbf = 0 ;
      }
   #endif

   #ifdef S4ENCRYPT_HOOK
      code4encryptInitUndoHook( c4, c4->encryptInit ) ;
      if ( c4->fileEncryptedBuffer != 0 )
      {
         u4free( c4->fileEncryptedBuffer ) ;
         c4->fileEncryptedBuffer = 0 ;
         c4->fileEncryptedBufferLen = 0 ;
      }
   #endif

   #ifdef S4MNDX
      if ( c4->memoUseBuffer != 0 )
      {
         u4free( c4->memoUseBuffer ) ;
         c4->memoUseBuffer = 0 ;
      }
   #endif

   #ifndef S4OFF_INDEX
      mem4release( c4->indexMemory ) ;
      c4->indexMemory = 0 ;

      mem4release( c4->index4fileMemory ) ;
      c4->index4fileMemory = 0 ;
   #endif

   mem4release( c4->dataMemory ) ;
   c4->dataMemory = 0 ;

   mem4release( c4->data4fileMemory ) ;
   c4->data4fileMemory = 0 ;

   #ifndef S4OFF_MULTI
      mem4release( c4->lockMemory ) ;
      c4->lockMemory = 0 ;

      mem4release( c4->lockGroupMemory ) ;
      c4->lockGroupMemory = 0 ;

      mem4release( c4->lockLinkMemory ) ;
      c4->lockLinkMemory = 0 ;
   #endif

   #ifndef S4OFF_INDEX
      mem4release( c4->tagMemory ) ;
      c4->tagMemory = 0 ;

      mem4release( c4->tagFileMemory ) ;
      c4->tagFileMemory = 0 ;
   #endif

   #ifdef TIMER5OUT
      if ( c4->timerFile.hand != 0 )
      {
         file4close( &c4->timerFile ) ;
         c4->timerFile.hand = 0 ;
      }
   #endif

   mem4release( c4->dataListMemory ) ;
   c4->dataListMemory = 0 ;
   mem4release( c4->relateDataListMemory ) ;
   c4->relateDataListMemory = 0 ;
   mem4release( c4->relateMemory ) ;
   c4->relateMemory = 0 ;
   mem4release( c4->relateListMemory ) ;
   c4->relateListMemory = 0 ;
   mem4release( c4->relationMemory ) ;
   c4->relationMemory = 0 ;

   mem4release( c4->calcMemory ) ;
   c4->calcMemory = 0 ;

   mem4release( c4->bitmapMemory ) ;
   c4->bitmapMemory = 0 ;

   if ( c4->fieldBuffer != 0 )
   {
      u4free( c4->fieldBuffer ) ;
      c4->fieldBuffer = 0 ;
      c4->bufLen = 0 ;
   }

   if ( c4->storedKey != 0 )
   {
      u4free( c4->storedKey ) ;
      c4->storedKey = 0 ;
      c4->storedKeyLen = 0 ;
   }

   if ( c4->errorLog != 0 )
   {
      #ifdef S4WIN64
         if ( c4->errorLog->hand != NULL )
      #else
         if ( c4->errorLog->hand != INVALID4HANDLE )
      #endif
         file4close( c4->errorLog ) ;
      u4free( c4->errorLog ) ;
      c4->errorLog = 0 ;
   }

   #ifdef S4CLIENT
      // AS 08/22/00 - code for client to client messaging
      if ( c4->recvMessage != 0 )
      {
         u4free( c4->recvMessage ) ;
         c4->recvMessage = 0 ;
         c4->recvMessageLen = 0 ;
      }

      // AS 10/11/00 - Was not freeing this memory...
      if ( c4->lockedFileNameBuf != 0 )
      {
         u4free( c4->lockedFileNameBuf ) ;
         c4->lockedFileNameBuf = 0 ;
      }
      if ( c4->lockedUserNameBuf != 0 )
      {
         u4free( c4->lockedUserNameBuf ) ;
         c4->lockedUserNameBuf = 0 ;
      }
      if ( c4->lockedNetNameBuf != 0 )
      {
         u4free( c4->lockedNetNameBuf ) ;
         c4->lockedNetNameBuf = 0 ;
      }
   #endif

   #ifdef E4ANALYZE
      c4->debugInt = 0 ; /* Some random value for double checking. */
   #endif

   #ifdef S4DEBUG_LOG
      #ifdef S4SERVER
         code4logInitUndo( c4 ) ;
      #endif
   #endif

   #ifndef S4OFF_TRAN
      if ( c4->tranData != 0 )
      {
         u4free( c4->tranData ) ;
         c4->tranData = 0 ;
         c4->tranDataLen = 0 ;
      }
   #endif

   #ifdef S4CLIENT
      if ( c4->savedData != 0 )
         u4free( c4->savedData ) ;
   #endif

   if ( c4->skipCom == 0 )
   {
      #ifndef S4STAND_ALONE
         #ifndef S4OFF_THREAD
            #ifdef S4SERVER
               list4mutexInitUndo(&c4->connectsToService) ;
            #endif
            /* AS 08/02/99 there may still be elements on this list.  If so, must remove and
               delete them...
            */
            for( ;; )
            {
               NET4MESSAGE *message = (NET4MESSAGE *)l4first( &c4->writeBuffersAvail.list ) ;
               if ( message == 0 )
                  break ;

               l4remove( &c4->writeBuffersAvail.list, message ) ;
               assert5( message->inUse == 0 ) ;
               mem4free( c4->writeMemory, message ) ;
            }

            list4mutexInitUndo( &c4->writeBuffersAvail ) ;

            list4mutexInitUndo( &c4->connectBufferListMutex ) ;
         #endif
         #ifdef S4COMM_THREAD
            if ( c4->inter != 0 )
            {
               inter4shutdownRequest( c4->inter ) ;
               u4free( c4->inter ) ;
               c4->inter = 0 ;
               #ifdef S4TESTING
                  Sleep( 50 ) ;  // give the thread time to free up memory and register its free-up...
               #endif
            }
         #endif
         #ifdef S4WINTEL
            WSACleanup() ;
         #endif
      #endif
   }

   #ifndef S4OFF_COMMUNICATIONS
      mem4release( c4->connectLowMemory ) ;
      c4->connectLowMemory = 0 ;
      mem4release( c4->writeMemory ) ;  // don't release this until after all messages removed (done above)
      c4->writeMemory = 0 ;
   #endif

   if ( c4->applicationVerify != 0 )
   {
      u4free( c4->applicationVerify ) ;
      c4->applicationVerify = 0 ;
   }

   #ifdef S4SERVER
      // AS 03/31/00 moved from out of if below because it can happen with s4test I had failure
      c4->currentClient = 0 ;
      if ( c4->catalogClient != 0 )
      {
         mem4free( c4->clientMemory, c4->catalogClient ) ;
         c4->catalogClient = 0 ;
      }
      mem4release( c4->clientMemory ) ;
      c4->clientMemory = 0 ;
   #endif

   #ifdef S4TESTING
      if ( s4test.codeBase == c4 )
      {
         if ( s4test.hand != INVALID4HANDLE )
         {
            #ifdef S4MEM_PRINT
               if ( write4buf != 0 )
               {
                  in4temp = 1 ;
                  file4seqWriteFlush( file4seqPtr ) ;
                  u4free( write4buf ) ;
                  write4buf = 0 ;
                  in4temp = 0 ;
               }
            #endif
            file4close( &s4test ) ;
         }
      }
   #else
      #ifdef S4TRACK_FILES
         if ( s4test.codeBase == c4 )
            if ( s4test.hand != INVALID4HANDLE)
               file4close( &s4test ) ;
      #endif
   #endif

   #ifdef S4WRITE_DELAY
      /* set the event semaphore to terminate the delay-write thread */
      /* AS 08/16/99 --> must be after s4testing code, which uses the delay-write stuff */
      if ( c4->delayWritesEnabled )
      {
         c4->initUndoDelayWrite = CreateEvent( 0, TRUE, FALSE, 0 ) ;
         #ifdef S4WIN64
            if ( c4->initUndoDelayWrite != NULL )
         #else
            if ( c4->initUndoDelayWrite != INVALID_HANDLE_VALUE )
         #endif
         {
            InterlockedIncrement( &c4->uninitializeDelayWrite ) ;
            /* notify the write thread */
            if ( SetEvent( c4->pendingWriteEvent ) ) /* wait for the delay-write thread to uninitialize */
            {
               WaitForSingleObject( c4->initUndoDelayWrite, INFINITE ) ;
               Sleep( 0 ) ;
            }
            CloseHandle( c4->initUndoDelayWrite ) ;
            CloseHandle( c4->pendingWriteEvent ) ;
            DeleteCriticalSection( &c4->critical4delayWriteList ) ;
         }
      }
   #endif

   #ifdef S4ENCRYPT_HOOK
      DeleteCriticalSection( &c4->fileEncryptedCritical ) ;
   #endif

   #ifdef S4READ_ADVANCE
      /* set the event semaphore to terminate the advance-read thread */
      if ( c4->advanceReadsEnabled )
      {
         c4->initUndoAdvanceRead = CreateEvent( 0, TRUE, FALSE, 0 ) ;
         #ifdef S4WIN64
            if ( c4->initUndoAdvanceRead != NULL )
         #else
            if ( c4->initUndoAdvanceRead != INVALID_HANDLE_VALUE )
         #endif
         {
            InterlockedIncrement( &c4->uninitializeAdvanceRead ) ;
            /* notify the write thread */
            if ( SetEvent( c4->pendingReadEvent ) ) /* wait for the delay-write thread to uninitialize */
            {
               WaitForSingleObject( c4->initUndoAdvanceRead, INFINITE ) ;
               Sleep( 0 ) ;
            }
            CloseHandle( c4->initUndoAdvanceRead ) ;
            CloseHandle( c4->pendingReadEvent ) ;
            DeleteCriticalSection( &c4->critical4advanceReadList ) ;
         }
      }
   #endif

   #ifdef S4WIN32 /* LY 99/06/17 : for CS */
      delete c4->memZeroAllocator ;
      c4->memZeroAllocator = 0 ;

      delete c4->memNonZeroAllocator ;
      c4->memNonZeroAllocator = 0 ;
   #endif

   #if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD )
      mem4release( c4->eventMemory ) ;
      c4->eventMemory = 0 ;
   #endif /* #if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD ) */

   #ifdef S4WRITE_DELAY
      // AS 06/30/99 added mem4release for delay writes and advance reads
      if ( c4->delayWriteMemory != 0 )
      {
         mem4release( c4->delayWriteMemory ) ;
         c4->delayWriteMemory = 0 ;
      }
   #endif

   #ifdef S4READ_ADVANCE
      if ( c4->advanceReadMemory != 0 )
      {
         mem4release( c4->advanceReadMemory ) ;
         c4->advanceReadMemory = 0 ;
      }
   #endif

   #ifndef S4SERVER
      /* the server has no current SERVER4CLIENT to get error code of */
      errCode = error4code( c4 ) ;
   #endif

   c4->initialized = 0 ;
   if ( c4->didAlloc == 1 )
      u4free( c4 ) ;

   #if defined( S4FOX ) && !defined( S4OFF_INDEX )
      if ( numCode4 == 1 )  // free up collation information... but before decrementing numCode4 since freeing fails the
      {
         assert5( numCode4 == 1 ) ;  // must be > 0 for freeing to work, if > 1, shouldn't free yet
         collate4initUndo() ;
      }
   #endif

   numCode4-- ;

   #if defined(S4FILE_EXTENDED) && defined(S4FOX)
      if (numCode4 == 0 && largeStamp.previousCode4init)
      {
         largeStamp.previousCode4init=0;
         largeStamp.doLargeLocking=0;
      }
   #endif

   if ( numCode4 == 0 && resetInProgress == 0)   /* reset memory */
      mem4reset() ;

   #ifdef S4SERVER
      return 0 ;
   #else
      return errCode ;
   #endif
}



#if !defined(S4SERVER) && !defined(S4PALM)
   /* not S4SERVER */
   void S4FUNCTION code4exit( CODE4 *c4 )
   {
      int rc ;

      if ( c4 == 0 )
         rc = -1 ;
      else
      {
         rc = error4code( c4 ) ;
         code4initUndo2( c4, 0 ) ;
      }

      #ifndef S4WINDOWS
         exit( rc ) ;
      #else
         #ifdef S4TESTING
            u4terminate() ;
         #else
            #ifdef S4WINCE
               ExitThread(0) ;
            #else
               FatalAppExit( 0, E4_MESSAG_EXI ) ;
            #endif
         #endif
      #endif
   }
#endif /* !S4SERVER */



int S4FUNCTION code4initUndo( CODE4 *c4 )
{
   #ifdef S4SERVER
      return code4initUndo2( c4, 0 ) ;
   #else
      return code4initUndo2( c4, 1 ) ;
   #endif
}



#ifndef S4SERVER
   /* not S4SERVER */
   int S4FUNCTION code4close( CODE4 *c4 )
   {
      DATA4 *dataOn, *dataNext ;
      LIST4 *list ;

      #ifdef E4VBASIC
         if ( c4parm_check( c4, 1, E91003 ) )
            return -1 ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E91003 ) ;
      #endif

      list = tran4dataList( (&(c4->c4trans.trans)) ) ;
      for ( dataNext = (DATA4 *)l4first( list ) ; ; )
      {
         dataOn = dataNext ;
         if ( dataOn == 0 )
            break ;
         dataNext = (DATA4 *)l4next( list, dataNext ) ;
         if ( dataOn == dataNext )   /* error -- stuck in endless loop */
            return -1 ;
         #ifndef S4OFF_OPTIMIZE
            #ifdef S4OPTIMIZE_STATS
               /* don't close the internal opt tracking dbf here. */
               if ( dataOn != dataOn->codeBase->statusDbf )
            #endif
         #endif
            d4close( dataOn ) ;
      }

      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;
      return 0 ;
   }
#endif /* !S4SERVER */



DATA4FILE *dfile4data( CODE4 *c4, const char *aliasName )
{
   // DATA4FILE *dataOn ;
   // #ifdef E4MISC
   //    DATA4FILE *dataResult ;
   // #endif
   #ifdef S4MACINTOSH
      FSSpec macSpec ;
      Str255 macStr ;
      char len ;
   #endif

   #ifdef E4PARM_LOW
      if ( c4 == 0 || aliasName == 0 )
      {
         error4( c4, e4parm_null, E91102 ) ;
         return 0 ;
      }
   #endif

   DATA4FILE *dataOn = 0 ;
   #ifdef E4MISC
      DATA4FILE *dataResult = 0 ;
   #endif

   for(;;)
   {
      dataOn = (DATA4FILE *)l4next( &c4->dataFileList, dataOn ) ;
      if ( !dataOn )
         break ;

      if ( u4namecmp( aliasName, dfile4name( dataOn ), c4->ignoreCase ) == 0 )
      {
         #ifdef S4MACINTOSH
            strcpy( (char *)&macStr, aliasName ) ;
            /* LY 00/01/26 : S4CTOPSTR macro for newer CodeWarrior */
            S4CTOPSTR( (char *)&macStr ) ;
            FSMakeFSSpec( c4->macVol, c4->macDir, macStr, &macSpec);
            len = (*macSpec.name) + 1 ;    /*What is the length of the pascal string*/
            if (( macSpec.vRefNum == dataOn->file.macSpec.vRefNum ) && ( macSpec.parID == dataOn->file.macSpec.parID ) && ( memcmp(macSpec.name, dataOn->file.macSpec.name, (size_t)len)== 0 ) )
            {
         #endif
               #ifdef E4MISC
                  if ( dataResult != 0 )
                  {
                     error4( c4, e4info, E83501 ) ;
                     return 0 ;
                  }
                  dataResult = dataOn ;
               #else
                  return dataOn ;
               #endif  /* E4MISC */
         #ifdef S4MACINTOSH
            }
         #endif
      }
   }

   #ifdef E4MISC
      return dataResult ;
   #else
      return dataOn ;
   #endif
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
DATA4 *tran4dataName( TRAN4 *trans, const char *name, const long clientId, const int doPath )
{
   DATA4 *dataOn ;
   #ifdef E4MISC
      DATA4 *dataResult = 0 ;
      DATA4 *dataAlias = 0 ;
   #endif
   #ifndef S4CLIENT
      char name1[258] ;
   #endif

   #ifdef E4ANALYZE
      if ( tran4verify( trans, 1 ) < 0 )
         return 0 ;
   #endif

   dataOn = 0 ;

   #ifndef S4CLIENT
      u4nameCurrent( name1, sizeof( name1 ), name ) ;
      u4nameExt( name1, sizeof( name1 ), DBF4EXT, 0 ) ;
   #endif


   /* AS 02/02/99 first need to search all the aliases, then all the true paths.
      This is because if duplicate files open, may find the path to the wrong aliased file...
   */
   for (;; )
   {
      dataOn = (DATA4 *)l4next( tran4dataList( trans ), dataOn ) ;
      if ( dataOn == 0 )
         break ;

      if ( u4namecmp( name, d4alias( dataOn ), trans->c4trans->c4->ignoreCase ) == 0 )  /* try simple alias check */
      {
         #ifdef S4SERVER
            if ( clientId != 0 )
               dataOn->clientId = clientId ;
         #endif
         #ifdef E4MISC
            if ( dataAlias != 0 )
            {
               error4( trans->c4trans->c4, e4info, E83501 ) ;
               return 0 ;
            }
            dataAlias = dataOn ;
         #else
            return dataOn ;
         #endif
      }
   }

   #ifdef E4MISC
      if ( dataAlias != 0 )
         return dataAlias ;
   #endif

   #ifndef S4CLIENT
      for (;; )
      {
         dataOn = (DATA4 *)l4next( tran4dataList( trans ), dataOn ) ;
         if ( dataOn == 0 )
            break ;

         /* otherwise, try file name check */
         if ( doPath )
         {
            if ( u4namecmp( name1, dataOn->dataFile->file.name, trans->c4trans->c4->ignoreCase ) == 0 )
            {
               #ifdef S4SERVER
                  if ( clientId != 0 )
                     dataOn->clientId = clientId ;
               #endif
               #ifdef E4MISC
                  if ( dataResult != 0 )
                  {
                     error4( trans->c4trans->c4, e4info, E83501 ) ;
                     return 0 ;
                  }
                  dataResult = dataOn ;
               #else
                  return dataOn ;
               #endif
            }
         }
      }
   #endif /* !S4CLIENT */

   #ifdef E4MISC
      return dataResult ;
   #else
      return dataOn ;
   #endif
}



#ifdef S4SERVER
   /* S4SERVER */
   TRAN4 *code4idDataTrans( const CODE4 *c4, const long serverId, const long clientId )
   {
      /* gets the trans corresponding to the input code4 and serverid/clientid, without
         changing any data4 clientid values (on server) */

      /* reserve the client list during this process */
      server4clientListReserve( c4->server ) ;

      TRAN4 *foundTrans = 0 ;
      for( SERVER4CLIENT *client = 0 ;; )
      {
         // AS 10/21/99 support for single-thread server
         // client = (SERVER4CLIENT *)l4next( &c4->server->clients.list, client ) ;
         client = server4clientGetNext( c4->server, client ) ;
         if ( client == 0 )
            break ;
         const DATA4 *data = tran4dataPtrOnly( &client->trans, serverId, clientId ) ;
         if ( data != 0 )
         {
            foundTrans = &(client->trans) ;
            break ;
         }
      }

      server4clientListRelease( c4->server ) ;
      return foundTrans ;
   }



   /* S4SERVER */
   const char *code4idDataAlias( const CODE4 *c4, const long serverId, const long clientId )
   {
      /* gets the alias corresponding to the input code4 and serverid/clientid, without
         changing any data4 clientid values (on server) */

      /* reserve the client list during this process */
      server4clientListReserve( c4->server ) ;

      const char *aliasFound = 0 ;

      for( SERVER4CLIENT *client = 0 ;; )
      {
         client = server4clientGetNext( c4->server, client ) ;
         if ( client == 0 )
            break ;
         const DATA4 *data = tran4dataPtrOnly( &client->trans, serverId, clientId ) ;
         if ( data != 0 )
         {
            aliasFound = d4alias( data ) ;
            break ;
         }
      }

      server4clientListRelease( c4->server ) ;
      return aliasFound ;
   }
#endif /* S4SERVER */



const DATA4 *tran4dataPtrOnly( const struct TRAN4St *trans, const long serverId, const long clientId )
{
   /* gets the data4 corresponding to the serverId, but DOES NOT set the current clientId to clientId */
   DATA4 *dataOn ;
   #ifdef E4MISC
      DATA4 *dataResult ;
   #endif
   #ifdef S4SERVER
      int rc = 0 ;
   #endif

   dataOn = 0 ;
   #ifdef E4MISC
      dataResult = 0 ;
   #endif

   if ( serverId == 0 )  /* invalid, just return failure - clientId is 0 means we don't care, any data4 will do */
      return 0 ;

   for (;; )
   {
      dataOn = (DATA4 *)l4next( tran4dataList( trans ), dataOn ) ;
      if ( dataOn == 0 )
         break ;
      if ( data4serverId( dataOn ) == serverId )
         #ifndef S4SERVER
            if ( data4clientId( dataOn ) == clientId )
         #endif
         {
            #ifdef E4MISC
               if ( dataResult != 0 )
               {
                  error4( trans->c4trans->c4, e4info, E83501 ) ;
                  return 0 ;
               }
               dataResult = dataOn ;
            #else
               break ;
            #endif
         }
   }

   #ifdef E4MISC
      return dataResult ;
   #else
      return dataOn ;
   #endif
}



DATA4 *tran4data( TRAN4 *trans, const long serverId, const long clientId )
{
   /* gets the data4 corresponding to the serverId, and sets the current clientId to clientId */
   DATA4 *dataOn ;
   #ifdef E4MISC
      DATA4 *dataResult ;
   #endif
   #ifdef S4SERVER
      int rc = 0 ;
   #endif

   #ifdef E4ANALYZE
      if ( tran4verify( trans, 1 ) < 0 )
         return 0 ;
   #endif

   dataOn = 0 ;
   #ifdef E4MISC
      dataResult = 0 ;
   #endif

   if ( serverId == 0 )  /* invalid, just return failure - clientId is 0 means we don't care, any data4 will do */
      return 0 ;

   for (;; )
   {
      dataOn = (DATA4 *)l4next( tran4dataList( trans ), dataOn ) ;
      if ( dataOn == 0 )
         break ;
      if ( data4serverId( dataOn ) == serverId )
         #ifndef S4SERVER
            if ( data4clientId( dataOn ) == clientId )
         #endif
         {
            #ifdef S4SERVER
               dataOn->clientId = clientId ;
            #endif
            #ifdef E4MISC
               if ( dataResult != 0 )
               {
                  error4( trans->c4trans->c4, e4info, E83501 ) ;
                  return 0 ;
               }
               dataResult = dataOn ;
            #else
               break ;
            #endif
         }
   }

   #ifndef S4OFF_TRAN
      /* AS 03/16/99 - It is possible that the data holding the lock is in the list of
         'to be closed' files.  We should retrieve it in that case... */

      #ifdef E4MISC
         if ( dataResult == 0 )
      #else
         if ( dataOn == 0 )
      #endif
      {
         #ifdef E4MISC
            dataOn = 0 ;
         #endif
         for ( ;; )
         {
            dataOn = (DATA4 *)l4next( &(code4trans( trans->c4trans->c4 )->closedDataFiles), dataOn ) ;
            if ( dataOn == 0 )
               break ;
            if ( data4serverId( dataOn ) == serverId )
               #ifndef S4SERVER
                  if ( data4clientId( dataOn ) == clientId )
               #endif
               {
                  #ifdef S4SERVER
                     dataOn->clientId = clientId ;
                  #endif
                  #ifdef E4MISC
                     if ( dataResult != 0 )
                     {
                        error4( trans->c4trans->c4, e4info, E83501 ) ;
                        return 0 ;
                     }
                     dataResult = dataOn ;
                  #else
                     break ;
                  #endif
               }
         }
      }
   #endif

   #ifdef E4MISC
      return dataResult ;
   #else
      return dataOn ;
   #endif
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
DATA4 *S4FUNCTION code4data( CODE4 *c4, const char *aliasName )
{
   #ifdef S4SERVER
      RELATE4 *relate ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( c4, 1, E91102 ) )
         return 0 ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( c4 == 0 || aliasName == 0 )
      {
         error4( c4, e4parm_null, E91102 ) ;
         return 0 ;
      }
   #endif

   #ifdef S4SERVER
      if ( c4->currentRelation == 0 )
         return tran4dataName( code4trans( c4 ), aliasName, 0L, 1 ) ;

      relate = &(c4->currentRelation->relate) ;
      for ( ;; )
      {
         if ( relate == 0 )
            return 0 ;
         if ( u4namecmp( aliasName, d4alias( relate->data ), c4->ignoreCase ) == 0 )
            return relate->data ;
         if ( relate4next( &relate ) == 2 )
            return 0 ;
      }
   #else
      return tran4dataName( code4trans( c4 ), aliasName, 0L, 0 ) ;
   #endif
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
long S4FUNCTION code4version( CODE4 *c4 )
{
   /* this function must reside in the DLL in order to correctly return the
      DLL'S version of this value */
   return (long)S4VERSION ;
}



#ifdef S4CLIENT
   const char *S4FUNCTION code4serverName( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E91112 ) ;
            return 0 ;
         }
      #endif

      #ifdef S4OFF_COMMUNICATIONS
         return 0 ;
      #else
         if ( !c4->defaultServer.connected )
            return 0 ;
         return c4->serverName ;
      #endif
   }
#endif /* S4CLIENT */


long S4FUNCTION code4serverOS( CODE4 *c4 )
{
   #ifdef S4CLIENT
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E91110 ) ;
            return e4parm_null ;
         }
      #endif

      if ( c4->errorCode < 0 )
         return c4->errorCode ;

      if ( c4->serverOS != OS4UNKNOWN )  // already received this info...
         return c4->serverOS ;

      if ( !c4->defaultServer.connected )
         return OS4UNKNOWN ;

      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return e4connection ;
      connection4assign( connection, CON4SERVER_OS, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return rc ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         connection4error( connection, c4, rc, E91110 ) ;
         return rc ;
      }

      if ( connection4len(connection) != sizeof(S4LONG) )
         return connection4error( connection, c4, e4packetLen, E91110 ) ;

      c4->serverOS = *((long *)connection4data(connection)) ;

      return c4->serverOS ;
   #else /* S4CLIENT else */
      // in stand-alone configuration, this info is unknown...
      return OS4UNKNOWN ;  // CS 2001/04/11 combine into one function
   #endif
}


#ifdef S4CLIENT
   index4format S4FUNCTION code4indexFormat( CODE4 *c4 )
   {
      #ifdef S4OFF_INDEX
         return 0 ;
      #else
         CONNECTION4 *connection ;
         int rc ;

         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
            {
               error4( 0, e4parm_null, E91110 ) ;
               return r4unknown ;
            }
         #endif

         if ( c4->indexFormat != 0 )
            return c4->indexFormat ;

         if ( !c4->defaultServer.connected )
            return r4unknown ;

         connection = &c4->defaultServer ;
         if ( connection == 0 )
            return r4unknown ;
         connection4assign( connection, CON4INDEX_FORMAT, 0L, 0L ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return r4unknown ;

         rc = connection4status( connection ) ;
         if ( rc < 0 )
         {
            connection4error( connection, c4, rc, E91110 ) ;
            return r4unknown ;
         }

         index4format format = (index4format)rc ;

         #ifdef E4MISC
            switch( format )
            {
               case r4mdx:
               case r4cdx:
               case r4ntx:
                  break ;
               default:
                  error4( c4, e4info, E81102 ) ;
                  return r4unknown ;
            }
         #endif

         c4->indexFormat = format ;

         return format ;
      #endif
   }
#else /* S4CLIENT else */
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   /* not S4CLIENT */
   index4format S4FUNCTION code4indexFormat( CODE4 *c4 )
   {
      #ifdef S4OFF_INDEX
         return r4unknown ;
      #else
         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
            {
               error4( 0, e4parm_null, E91110 ) ;
               return r4unknown ;
            }
         #endif

         #ifdef S4MDX
            return r4mdx ;
         #endif
         #ifdef S4FOX
            return r4cdx ;
         #endif
         #ifdef S4CLIPPER
            return r4ntx ;
         #endif
      #endif
   }
#endif /* S4CLIENT else */



void S4FUNCTION code4largeOn( CODE4 *c4 )
{
   #if defined(S4STAND_ALONE) && !defined(S4OFF_WRITE) && !defined(S4OFF_TRAN) && defined(S4FILE_EXTENDED) && defined(S4FOX)
      if (largeStamp.doLargeLocking != 1 && code4numCodeBase() > 1)
         error4describe( c4, e4parm, E91001, "Large file stamp not initialized and more than one CODE4 initialized", 0, 0 ) ;
      if ( l4numNodes( &c4->dataFileList ) != 0 )  /* only allow when no data files open */
         error4describe( c4, e4parm, E91001, "Can not initialize large file support with open files", 0, 0 ) ;
      else
      {
         if (largeStamp.doLargeLocking == 0)
         {
            largeStamp.doLargeLocking=1;
            largeStamp.previousCode4init++;
         }
         code4largeOnLow( c4 ) ;
      }
   #else
      error4( c4, e4notSupported, E91013 ) ;
   #endif
}



#if defined(S4FILE_EXTENDED) && defined(S4FOX)
   void code4largeOnLow(CODE4 *c4 )
   {
      c4->largeFileOffset = S4LARGE_FILE_OFFSET ;
   }
#endif




#if defined(S4STAND_ALONE) && !defined(S4OFF_WRITE) && !defined(S4OFF_TRAN)
   /* not S4OFF_TRAN, not S4OFF_WRITE, S4STAND_ALONE */
   int S4FUNCTION code4logCreate( CODE4 *c4, const char *fileName, const char *userId )
   {
      int rc ;
      static char defaultUser[] = "PUBLIC" ;

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( c4, e4parm_null, E91013 ) ;
      #endif

      if ( userId == 0 )
         userId = defaultUser ;
      else
         if ( userId[0] == 0 )  /* null string */
            userId = defaultUser ;

      if ( c4->c4trans.transFile != 0 )
         return r4logOpen ;

      if ( fileName == 0 )
         fileName = "C4.log" ;
      else
         if ( fileName[0] == 0 )  /* null string */
            fileName = "C4.log" ;

      rc = code4transFileEnable( &c4->c4trans, fileName, 1 ) ;

      #ifndef S4UTILS
         tran4addUser( &c4->c4trans.trans, 0L, userId, (unsigned short int)strlen( userId ) ) ;
      #endif

      return rc ;
   }



   /* not S4OFF_TRAN, not S4OFF_WRITE, S4STAND_ALONE */
   const char *S4FUNCTION code4logFileName( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( c4, e4parm_null, E91014 ) ;
            return 0 ;
         }
      #endif

      if ( c4->c4trans.transFile == 0 )
         return 0 ;
      return c4->c4trans.transFile->file.name ;
   }



   /* not S4OFF_TRAN, not S4OFF_WRITE, S4STAND_ALONE */
   int S4FUNCTION code4logOpen( CODE4 *c4, const char *fileName, const char *userId )
   {
      int rc ;
      static char defaultUser[] = "PUBLIC" ;

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( c4, e4parm_null, E91015 ) ;
      #endif

      if ( userId == 0 )
         userId = defaultUser ;
      else
         if ( userId[0] == 0 )  /* null string */
            userId = defaultUser ;

      if ( c4->c4trans.transFile != 0 )
         return r4logOpen ;

      if ( fileName == 0 )
         fileName = "C4.log" ;
      else
         if ( fileName[0] == 0 )  /* null string */
            fileName = "C4.log" ;

      rc = code4transFileEnable( code4trans( c4 )->c4trans, fileName, 0 ) ;
      #ifdef S4ODBC_BUILD
         #ifdef S4STAND_ALONE
            // with odbc, create the log file if not open...
            if ( rc == r4noExist )
               rc = code4transFileEnable( code4trans( c4 )->c4trans, fileName, 1 ) ;
         #endif
      #endif

      #ifndef S4UTILS
         if ( rc == 0 )
            tran4addUser( &c4->c4trans.trans, 0L, userId, (unsigned short int)strlen( userId ) ) ;
      #endif
      return rc ;
   }



   /* not S4OFF_TRAN, not S4OFF_WRITE, S4STAND_ALONE */
   void S4FUNCTION code4logOpenOff( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( c4, e4parm_null, E91016 ) ;
            return ;
         }
      #endif

      c4->logOpen = 0 ;
   }
#endif /* !S4OFF_TRAN && !S4OFF_WRITE && S4STAND_ALONE */



const char *S4FUNCTION code4indexExtension( CODE4 *c4 )
{
   #ifdef E4VBASIC
      if ( c4parm_check( c4, 1, E91110 ) )
         return 0 ;
   #endif

   if ( c4->indexExtension[0] == 0 )
   {
      switch ( code4indexFormat( c4 ) )
      {
         case r4mdx:
            c4memcpy( c4->indexExtension, MDX4EXT, 3 ) ;
            break ;
         case r4cdx:
            c4memcpy( c4->indexExtension, CDX4EXT, 3 ) ;
            break ;
         case r4ntx:
            c4memcpy( c4->indexExtension, NTX4EXT, 3 ) ;
            break ;
         default:
            return 0 ;
      }
   }

   return c4->indexExtension ;
}



const char *S4FUNCTION code4memoExtension( CODE4 *c4 )
{
   if ( c4->memoExtension[0] == 0 )
   {
      switch ( code4indexFormat( c4 ) )
      {
         case r4mdx:
            c4memcpy( c4->memoExtension, DBT4EXT, 3 ) ;
            break ;
         case r4cdx:
            c4memcpy( c4->memoExtension, FPT4EXT, 3 ) ;
            break ;
         case r4ntx:
            c4memcpy( c4->memoExtension, DBT4EXT, 3 ) ;
            break ;
         default:
            return 0 ;
      }
   }

   return c4->memoExtension ;
}

#ifdef S4FOX
   #define S4FORMAT 1
#endif

#ifdef S4CLIPPER
   #ifdef S4FORMAT
      #error Choose only one CodeBase index file compatibility option.
   #endif
   #define S4FORMAT 2
#endif

#ifdef S4MDX
   #ifdef S4FORMAT
      #error Choose only one CodeBase index file compatibility option.
   #endif
   #define S4FORMAT 4
#endif

#ifdef S4CLIENT
   #ifndef S4FORMAT
      #define S4FORMAT 0
   #endif
#else
   #ifndef S4FORMAT
      #error You must define either S4FOX, S4CLIPPER or S4MDX
   #endif
#endif

#ifdef S4CLIENT
   #define S4CLIENT_VAL 0x8
#else
   #define S4CLIENT_VAL 0x0
#endif

#ifdef S4DOS
   #define S4OPERATING 0x0010  // S4DOS
#endif

#ifdef S4WIN16
   #ifdef S4OPERATING
      #error Choose only one of CodeBase switches S4DOS, S4WIN16, S4WIN32, S4OS2, S4UNIX, S4MACINTOSH or S4PASCAL_WIN
   #endif
   #define S4OPERATING 0x0020  // S4WIN16
#endif

#ifdef S4WIN32
   #ifdef S4OPERATING
      #error Choose only one of CodeBase switches S4DOS, S4WIN16, S4WIN32, S4OS2, S4UNIX, S4MACINTOSH or S4PASCAL_WIN
   #endif
   #define S4OPERATING 0x0040  // S4WIN32
#endif

#ifdef S4STAND_ALONE
   #define S4STAND_ALONE_VAL 0x80
#else
   #define S4STAND_ALONE_VAL 0x0
#endif

#ifdef S4OS2
   #ifdef S4OPERATING
      #error Choose only one of CodeBase switches S4DOS, S4WIN16, S4WIN32, S4OS2, S4UNIX, S4MACINTOSH or S4PASCAL_WIN
   #endif
   #define S4OPERATING 0x0100  // S4OS2
#endif

#ifdef S4UNIX
   #ifdef S4OPERATING
      #error Choose only one of CodeBase switches S4DOS, S4WIN16, S4WIN32, S4OS2, S4UNIX, S4MACINTOSH or S4PASCAL_WIN
   #endif
   #define S4OPERATING 0x0200  // S4UNIX
#endif

#ifdef S4MACINTOSH
   #ifdef S4OPERATING
      #error Choose only one of CodeBase switches S4DOS, S4WIN16, S4WIN32, S4OS2, S4UNIX, S4MACINTOSH or S4PASCAL_WIN
   #endif
   #define S4OPERATING 0x0400  // S4MACINTOSH
#endif

#ifdef S4PALM
   #ifdef S4OPERATING
      #error Choose only one of CodeBase switches S4DOS, S4WIN16, S4WIN32, S4OS2, S4UNIX, S4MACINTOSH or S4PALM
   #endif
   #define S4OPERATING 0x0800  // S4PALM
#endif

#ifndef S4OPERATING
   #ifndef S4PALM
      #error Must choose one of CodeBase switches S4DOS, S4WIN16, S4WIN32, S4OS2, S4UNIX, S4MACINTOSH or S4PASCAL_WIN
   #endif
#endif

#ifdef S4CB51
   #define S4CB51_VAL  0x1000
#else
   #define S4CB51_VAL  0
#endif

#ifdef S4SAFE
   #define S4SAFE_VAL  0x2000
#else
   #define S4SAFE_VAL  0
#endif

#ifdef S4LOCK_HOOK
   #define S4LOCK_HOOK_VAL 0x4000
#else
   #define S4LOCK_HOOK_VAL 0
#endif

#ifdef S4MAX
   #define S4MAX_VAL 0x8000
#else
   #define S4MAX_VAL 0
#endif

#ifdef S4TIMEOUT_HOOK
   #define S4TIMEOUT_HOOK_VAL 0x10000L
#else
   #define S4TIMEOUT_HOOK_VAL 0
#endif

#ifdef E4ANALYZE
   #define E4ANALYZE_VAL 0x20000L
#else
   #define E4ANALYZE_VAL 0
#endif

#ifdef E4DEBUG
   #define E4DEBUG_VAL 0x40000L
#else
   #define E4DEBUG_VAL 0
#endif

#ifdef E4HOOK
   #define E4HOOK_VAL 0x80000L
#else
   #define E4HOOK_VAL 0
#endif

#ifdef E4LINK
   #define E4LINK_VAL 0x100000L
#else
   #define E4LINK_VAL 0
#endif

#ifdef E4MISC
   #define E4MISC_VAL 0x200000L
#else
   #define E4MISC_VAL 0
#endif

#ifdef E4OFF
   #define E4OFF_VAL 0x400000L
#else
   #define E4OFF_VAL 0
#endif

#ifdef E4OFF_STRING
   #define E4OFF_STRING_VAL 0x800000L
#else
   #define E4OFF_STRING_VAL 0
#endif

#ifdef E4PARM_HIGH
   #define E4PARM_HIGH_VAL 0x1000000L
#else
   #define E4PARM_HIGH_VAL 0
#endif

#ifdef E4PAUSE
   #define E4PAUSE_VAL 0x2000000L
#else
   #define E4PAUSE_VAL 0
#endif

#ifdef E4STOP
   #define E4STOP_VAL 0x4000000L
#else
   #define E4STOP_VAL 0
#endif

#ifdef E4STOP_CRITICAL
   #define E4STOP_CRITICAL_VAL 0x8000000L
#else
   #define E4STOP_CRITICAL_VAL 0
#endif

#ifdef S4OFF_INDEX
   #define S4OFF_INDEX_VAL 0x10000000L
#else
   #define S4OFF_INDEX_VAL 0
#endif

#ifdef S4OFF_MEMO
   #define S4OFF_MEMO_VAL 0x20000000L
#else
   #define S4OFF_MEMO_VAL 0
#endif

#ifdef S4OFF_MULTI
   #define S4OFF_MULTI_VAL 0x40000000L
#else
   #define S4OFF_MULTI_VAL 0
#endif

#ifdef S4OFF_OPTIMIZE
   #define S4OFF_OPTIMIZE_VAL 0x80000000L
#else
   #define S4OFF_OPTIMIZE_VAL 0
#endif

/*
   no room for these switches

#ifdef S4OFF_REPORT
   #define S4OFF_REPORT_VAL 0x100000000
#else
   #define S4OFF_REPORT_VAL 0
#endif

#ifdef S4OFF_TRAN
   #define S4OFF_TRAN_VAL 0x200000000
#else
   #define S4OFF_TRAN_VAL 0
#endif

#ifdef S4OFF_WRITE
   #define S4OFF_WRITE_VAL 0x400000000
#else
   #define S4OFF_WRITE_VAL 0
#endif
*/


//#ifndef S4PALM
long S4FUNCTION u4switch()
{
   return (long) ( S4FORMAT + S4OPERATING + S4CB51_VAL + S4SAFE_VAL +
          S4LOCK_HOOK_VAL + S4MAX_VAL + S4TIMEOUT_HOOK_VAL + E4ANALYZE_VAL +
          E4DEBUG_VAL + E4HOOK_VAL + E4LINK_VAL + E4MISC_VAL + E4OFF_VAL +
          E4OFF_STRING_VAL + E4PARM_HIGH_VAL + E4PAUSE_VAL + E4STOP_VAL +
          E4STOP_CRITICAL_VAL + S4OFF_INDEX_VAL + S4OFF_MEMO_VAL +
          S4OFF_MULTI_VAL + S4OFF_OPTIMIZE_VAL +
/*          S4OFF_REPORT_VAL + S4OFF_TRAN_VAL + S4OFF_WRITE_VAL +  no room */
          S4CLIENT_VAL + S4STAND_ALONE_VAL ) ;
}
//#endif

#ifndef S4OFF_CONTROLS
   #define CTRL4SERVERNAMESIZE 260

   /*Structures for CodeControls Functions */

   typedef   struct ctrl4code_tag
   {
      LINK4       link ;
      HINSTANCE   hInst ;
      CODE4       *code ;
      int         alloc ;
      LIST4       form ;

   #ifdef S4CLIENT
      char        ServerName[CTRL4SERVERNAMESIZE] ;
   #endif

   }CTRL4CODE ;



   #ifdef __cplusplus
      extern "C" {
   #endif
   int         S4FUNCTION ctrl4addCode( HINSTANCE hInst ) ;                                 /*450*/
   void        S4FUNCTION ctrl4codeListInit( void ) ;                                        /*451*/
   void        S4FUNCTION ctrl4freeCtrlNode( CTRL4CODE *node ) ;
   void        S4FUNCTION ctrl4freeCodeList( void ) ;                                        /*452*/
   CTRL4CODE * S4FUNCTION ctrl4getCtrlCode( HINSTANCE hInst ) ;                              /*453*/
   void        S4FUNCTION ctrl4getServerName( HINSTANCE hInst,char *serverName,int strLen ) ; /* 5 oh something */
   void        S4FUNCTION ctrl4setServerName( HINSTANCE hInst,char *serverName ) ; /* 5 oh something */
   void        S4FUNCTION ctrl4initVBX( CODE4 *code,HINSTANCE hInstance,int initialize ) ;   /*454*/
   void        S4FUNCTION ctrl4initVBXUndo( CODE4 *code,HINSTANCE hInstance ) ;              /*455*/
   #ifdef __cplusplus
      }
   #endif



   /***************************************************************\
   *  List Containing CODE4 and hInstance
   *  structures for CodeControls
   \***************************************************************/

   LIST4    ctrl4codeListVBX ;


   void S4FUNCTION ctrl4codeListInit( void )
   {
         /*memset( &ctrl4codeListVBX,0,sizeof( LIST4 ) );*/
   }



   int S4FUNCTION ctrl4addCode( HINSTANCE hInst )
   {
      CTRL4CODE   *node ;

      if ( !hInst )
         return -1 ;

      node = ( CTRL4CODE * ) u4alloc( sizeof( CTRL4CODE ) ) ;
      if ( node )
      {
         node->hInst = hInst ;
              node->alloc = 1 ;
         node->code = NULL ;
         l4add( &ctrl4codeListVBX,node ) ;
      }
      return 0 ;
   }



   void S4FUNCTION ctrl4freeCtrlNode( CTRL4CODE *node )
   {
      if ( node )
      {
         l4remove( &ctrl4codeListVBX,node ) ;
         if ( node->code )
         {
            code4initUndo( node->code ) ;
            u4free( node->code ) ;
         }
         u4free( node ) ;
      }
   }



   void S4FUNCTION ctrl4freeCodeList( void )
   {
      CTRL4CODE   *node ;

      for( node = ( CTRL4CODE * ) l4first( &ctrl4codeListVBX );node != NULL; )
      {
         l4remove( &ctrl4codeListVBX,node ) ;
                   if ( node->code && node->alloc )
                   {
            code4initUndo( node->code ) ;
            u4free( node->code ) ;
                   }
         u4free( node ) ;
         node = ( CTRL4CODE * ) l4next( &ctrl4codeListVBX,node ) ;
      }
   }



   CTRL4CODE * S4FUNCTION ctrl4getCtrlCode( HINSTANCE hInst )
   {
      CTRL4CODE   *node,*returnnode = NULL ;

      if ( hInst == 0 )
         return NULL ;

      for( node = ( CTRL4CODE * ) l4first( &ctrl4codeListVBX );node != NULL; )
      {
         if ( node->hInst == hInst )
         {
            returnnode = node ;
            node = NULL ;
         }
         else
            node = ( CTRL4CODE * ) l4next( &ctrl4codeListVBX,node ) ;
      }
      return returnnode ;
   }



   void S4FUNCTION ctrl4initVBXUndo( CODE4 *code,HINSTANCE hInstance )
   {
      CTRL4CODE   *node ;

      node = ctrl4getCtrlCode( hInstance ) ;
      if ( node )
      {
         if ( code != node->code )
            code4initUndo( code ) ;

         code4initUndo( node->code ) ;
         if ( node->alloc )
         {
            u4free( node->code ) ;
         }
         l4remove( &ctrl4codeListVBX,node ) ;
         u4free( node ) ;
      }
   }



   void S4FUNCTION ctrl4initVBX( CODE4 *code, HINSTANCE hInstance, int initialize )
   {
      int dealloc ;
      CODE4 *oldcode ;
      CTRL4CODE *node ;

      if ( code )
      {
         node = ctrl4getCtrlCode( hInstance ) ;
         if ( !node )
         {
            ctrl4addCode( hInstance ) ;
            node = ctrl4getCtrlCode( hInstance ) ;
            if ( node )
               node->alloc = 0 ;
         }

         if ( node )
         {
            if ( node->code )
            {
               dealloc = 0 ;
               if ( node->alloc )
                  dealloc = IDYES ;
               else
                  dealloc = MessageBox( 0,
                  "Warning! Detected two calls to ctrl4init\nwithout intervening call to ctrl4initUndo!\n\nOk to free previously allocated CODE4 memory ?","Multiple CODE4 Detected"
                  ,MB_YESNO|MB_TASKMODAL|MB_ICONEXCLAMATION ) ;

               if ( dealloc == IDYES )
               {
                  oldcode = node->code ;
                  ctrl4initVBXUndo( node->code,hInstance ) ;
                  u4free( oldcode ) ;
                  node->code = NULL ;
               }
            }
            node->alloc = 0 ;
            node->code = code ;

            if ( initialize )
            {
               #ifdef S4CLIENT
                  char setProtocol = 0 ;

                  /* must choose a default protocol for C/C++ client/server dll
                     if user will be calling ctrl4init() */
                  #ifndef E4VBASIC
                     #ifndef S4PASCAL
                        setProtocol = 1 ;
                     #endif
                  #endif

                  code4initLow( code, 0, S4VERSION, sizeof( CODE4 ), 0 ) ;
               #else
                  code4init( code ) ;
               #endif
            }
         }
      }
   }



   #ifdef S4CLIENT
      void  S4FUNCTION ctrl4getServerName( HINSTANCE hInst,char *serverName,int strLen ) /*5 oh something*/
      {
         CTRL4CODE   *code = NULL ;

         if ( !hInst )
            return ;

         if ( !serverName || strLen <= 0 )
            return ;

         memset( serverName,0,strLen ) ;
         code = ctrl4getCtrlCode( hInst ) ;

         if ( code )
         {
            memcpy( serverName,code->ServerName,min( strLen,CTRL4SERVERNAMESIZE ) ) ;
         }
         return ;
      }



      void S4FUNCTION ctrl4setServerName( HINSTANCE hInst,char *serverName ) /*5 oh something*/
      {
         CTRL4CODE   *code = NULL ;

         if ( !hInst )
            return ;

         if ( !serverName )
            return ;

         code = ctrl4getCtrlCode( hInst ) ;

         if ( code )
         {
            memcpy( code->ServerName,serverName,CTRL4SERVERNAMESIZE ) ;
         }
         return ;
      }
   #endif /* S4CLIENT */



   #ifdef S4STAND_ALONE
      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      void  S4FUNCTION ctrl4getServerName( HINSTANCE hInst,char *serverName,int strLen ) /*5 oh something*/
      {
         return ;
      }



      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      void S4FUNCTION ctrl4setServerName( HINSTANCE hInst,char *serverName ) /*5 oh something*/
      {
         return ;
      }
   #endif /* S4STAND_ALONE */
#endif /* S4OFF_CONTROLS */



#ifdef S4SERVER
   #ifdef E4ANALYZE
   /* E4ANALYZE, S4SERVER */
   void S4FUNCTION code4enterExclusiveVerifyFirst( CODE4 *c4, SERVER4CLIENT *client, int doDelay )
   {
      /* when shutdown, doDelay is 0, continue with shutdown even
         without exclusive access
         ensure that this is the first time we grabbed this exclusive
      */

      // This code can't be here - we may be waiting to get access,
      // so count does not need to be zero now (at least windows)
      // if ( doDelay == 1 )
      //    assert5( c4accessMutexCountZero( c4 ) ) ;
      code4enterExclusive( c4, client, doDelay ) ;
      if ( doDelay == 1 )
         assert5( c4accessMutexCountOne( c4 ) ) ;
   }
   #endif /* E4ANALYZE */



   /* S4SERVER */
   void S4FUNCTION code4enterExclusive( CODE4 *c4, SERVER4CLIENT *client, int doDelay )
   {
      /* AS 09/30/98 --> in case of no-delay, still wait for 10 secs to allow processes to partially clean up
                         esp. in some instances */
      #ifndef S4UNIX
         WaitForSingleObject( c4->accessMutex, doDelay ? INFINITE : 10000 ) ;
      #endif
      #ifdef E4ANALYZE
         if ( doDelay )
            if ( c4accessMutexCountNotZero( c4 ) != 0 )
               if ( c4->currentClient != client )
                  error4( c4, e4parm, E70223 ) ;
      #endif
      if ( doDelay == 0 )
      {
   //      c4->currentClient = 0 ;  // ensure we don't get ourselves fried
         if ( c4accessMutexCountNotZero( c4 ) )
            if ( c4->currentClient != client )   // reset the access count to '1' since we are taking over control
               c4setAccessMutexCount( c4, 0 ) ;
      }
      c4setAccessMutexCount( c4, c4getAccessMutexCount( c4 ) + 1 ) ;
      c4->currentClient = client ;
   }



   #ifdef E4ANALYZE
      /* E4ANALYZE, S4SERVER */
      void S4FUNCTION code4exitExclusiveVerifyLast( CODE4 *c4, SERVER4CLIENT *client )
      {
         /* verifies that this is the last exit of the exclusive */
         assert5( c4accessMutexCountOne( c4 ) ) ;
         code4exitExclusive( c4, client ) ;
      }
   #endif /* E4ANALYZE */



   /* S4SERVER */
   void S4FUNCTION code4exitExclusive( CODE4 *c4, SERVER4CLIENT *client )
   {
      if ( c4->currentClient != client )  /* we do not have access anyway */
         return ;
      #ifdef E4ANALYZE_BREAK_ON_CLIENT_LEFT_ERROR_STATE
         /* if releasing last instance, should be no error code (to catch incorrectly leaving in error state) */
         /* must call before mutex count is 0 or error code is ignored since no current client */
            assert5( c4->accessMutexCount != 1 || error4code( c4 ) >= 0 ) ;
      #endif

      c4setAccessMutexCount( c4, c4getAccessMutexCount( c4 ) - 1 ) ;

      #ifndef S4UNIX
         ReleaseMutex( c4->accessMutex ) ;
      #endif

   /*
      if ((c4->accessMutexCount) == 0)   in case of no delay on enter exclusive, leave this be... (i.e. don't reset current client)
         c4->currentClient = 0 ;
   */
   }



   /* S4SERVER */
   int S4FUNCTION c4getAutoRecover(const CODE4 *c4 )
   {
      #ifdef EXCEPTION4REINDEX
         return 1 ;
      #else
         return 0 ;
      #endif
   }



   /* S4SERVER */
   int S4FUNCTION c4getLargeOn(const CODE4 *c4)
   {
      if ( c4 == 0 )
         return -1 ;
      return (c4->largeFileOffset) ;
   }
#else /* S4SERVER else */
   #ifndef S4CLIPPER
      /* not S4CLIPPER, not S4SERVER */
      INDEX4FILE *S4FUNCTION i4getIndexFileDo( const INDEX4 *i4 )
      {
         return i4->indexFile ;
      }
   #endif /* not S4CLIPPER */
#endif /* S4SERVER else */



#if !defined(S4OFF_TRAN) && !defined(S4OFF_WRITE)
   #ifdef S4CLIENT
      #ifndef S4INLINE
         /* S4CLIENT, not S4OFF_TRAN, not S4OFF_WRITE, not S4INLINE*/
         int S4FUNCTION code4tranInit2( CODE4 *c4, const char *fileName, const char *charId )
         {
            #ifdef E4PARM_LOW
               if ( c4 == 0 || fileName == 0 )
                  return error4( 0, e4parm_null, E93801 ) ;
            #endif

            return 0 ;
         }



         /* not S4INLINE, S4CLIENT, not S4OFF_TRAN, not S4OFF_WRITE */
         void code4tranInitUndo( CODE4 *c4 )
         {
            return ;
         }
      #endif /* S4INLINE */
   #else /* S4CLIENT else */
      /* not S4CLIENT, not S4OFF_TRAN, not S4OFF_WRITE, S4INLINE*/
      int code4tranInitUndoLow( TRAN4 *t4, const long clientId )
      {
         int rc ;

         if ( t4 == 0 )
            return 0 ;

         if ( t4->c4trans->enabled == 1 && t4->userId[0] != 0 )  /* if it has been initialized */
         {
            rc = tran4set( t4, t4->currentTranStatus, -1L, clientId, TRAN4INIT_UNDO, 0, 0L, 0L ) ;
            if ( rc < 0 )
               return rc ;
            if ( tran4lowAppend( t4, 0, 0 ) != 0 )
               return e4transAppend ;
            memset( t4->userId, 0, sizeof( t4->userId ) ) ;
         }

         t4->dataPos = 0 ;

         return 0 ;
      }
   #endif /* S4CLIENT */



   #ifdef S4STAND_ALONE
      /* S4STAND_ALONE, not S4OFF_TRAN, not S4OFF_WRITE */
      void code4tranInitUndo( CODE4 *c4 )
      {
         #ifdef E4PARM_LOW
            if ( c4 == 0 )
            {
               error4( 0, e4parm_null, E93804 ) ;
               return ;
            }
         #endif

         if ( code4transEnabled( c4 ) )
            code4transInitUndo( &c4->c4trans ) ;
      }



      /* S4STAND_ALONE, not S4OFF_TRAN, not S4OFF_WRITE */
      int S4FUNCTION code4tranInit2( CODE4 *c4, const char *fileName, const char *charId )
      {
         int rc ;

         #ifdef E4PARM_LOW
            if ( c4 == 0 || fileName == 0 )
               return error4( 0, e4parm_null, E93801 ) ;
         #endif

         c4->c4trans.c4 = c4 ;
         rc = code4transFileEnable( &c4->c4trans, fileName, 0 ) ;
         if ( rc < 0 )
            return rc ;
         c4->c4trans.trans.c4trans = &c4->c4trans ;

         if ( charId != 0 )
            return tran4addUser( &c4->c4trans.trans, 0L, charId, ( unsigned short )strlen( charId ) ) ;

         return 0 ;
      }
   #endif /* S4STAND_ALONE */
#endif /* !S4OFF_WRITE */



#ifdef S4STAND_ALONE
   /* S4STAND_ALONE */
   int S4FUNCTION code4tranInit( CODE4 *c4 )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93802 ) ;
      #endif

      c4->c4trans.c4 = c4 ;
      c4->c4trans.trans.c4trans = &c4->c4trans ;
      tran4dataListSet( &c4->c4trans.trans, &c4->c4trans.trans.localDataList ) ;
      return 0 ;
   }
#else /* S4STAND_ALONE else */
   /* not S4STAND_ALONE */
   int S4FUNCTION code4osVersion(void)
   {
      #ifdef S4WINTEL
         int rc ;
         OSVERSIONINFO verInfo ;

         verInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO) ;
         rc = GetVersionEx(&verInfo) ;
         if (!rc)
            return -1 ;
         switch ( verInfo.dwPlatformId )
         {
            case VER_PLATFORM_WIN32s:
               return ver431 ;
            case VER_PLATFORM_WIN32_WINDOWS:
               return ver495 ;
            case VER_PLATFORM_WIN32_NT:
               return ver4NT ;
            default:
               return -1 ;
         }
      #else
         return ver4Unix ;
      #endif
   }
#endif /* S4STAND_ALONE else */



short S4FUNCTION code4collate( CODE4 *c4, short collateType )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
         return error4( 0, e4parm_null, E91004 ) ;
   #endif

   #ifdef S4CLIENT_OR_FOX
      if ( collateType == -1 )
         return c4->codeCollateSetValue ;

      int oldCollateType = c4->codeCollateSetValue ;

      switch ( collateType )
      {
         case collate4none:
            c4->collateName = collate4none ;
            break ;
         case collate4machine:
            c4->collateName = collate4machine ;
            break ;
         case collate4generalCp1252:
            c4->collateName = collate4generalCp1252 ;
            break ;
         case collate4generalCp437:
            c4->collateName = collate4generalCp437 ;
            break ;
         case collate4test:
         case collate4special:
            c4->collateName = collate4test ;
            break ;
         case collate4general:  // code page specific determinant...
            c4->collateName = collate4none ;
            c4->collatingSequence = sort4general ;
            break ;
         default:
            return error4( 0, e4parm, E91004 ) ;
      }

      c4->codeCollateSetValue = collateType ;

      return oldCollateType ;
   #else
      return error4( c4, e4notSupported, E91004 ) ;
   #endif
}



short S4FUNCTION code4collateUnicode( CODE4 *c4, short collateType )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
         return error4( 0, e4parm_null, E91004 ) ;
   #endif

   #ifdef S4CLIENT_OR_FOX
      if ( collateType == -1 )
         return c4->codeCollateSetValueUnicode ;

      int oldCollateType = c4->codeCollateSetValueUnicode ;

      switch ( collateType )
      {
         case collate4none:
            c4->collateNameUnicode = collate4none ;
            break ;
         case collate4machine:
            c4->collateNameUnicode = collate4machine ;
            break ;
         case collate4generalCp1252:
            c4->collateNameUnicode = collate4generalCp1252 ;
            break ;
         case collate4generalCp437:
            c4->collateNameUnicode = collate4generalCp437 ;
            break ;
         case collate4test:
         case collate4special:
            c4->collateNameUnicode = collate4test ;
            break ;
         case collate4general:  // code page specific determinant...
             // Unicode first 256 characters are the same as CodePage 1252
            c4->collateNameUnicode = collate4generalCp1252 ;
            break ;
         default:
            return error4( 0, e4parm, E91004 ) ;
      }

      c4->codeCollateSetValueUnicode = collateType ;

      return oldCollateType ;
   #else
      return error4( c4, e4notSupported, E91004 ) ;
   #endif
}



#ifdef S4CLIENT
   /* S4CLIENT */
   int S4FUNCTION code4tranInit( CODE4 *c4 )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93802 ) ;
      #endif

      #ifndef S4OFF_TRAN
         if ( code4transEnabled( c4 ) )
            return 0 ;
      #endif

      c4->c4trans.c4 = c4 ;
      c4->c4trans.trans.c4trans = &c4->c4trans ;
      #ifndef S4OFF_TRAN
         c4->c4trans.enabled = 1 ;
      #endif
      tran4dataListSet( &c4->c4trans.trans, &c4->c4trans.trans.localDataList ) ;
      return 0 ;
   }
#endif /* S4CLIENT */
