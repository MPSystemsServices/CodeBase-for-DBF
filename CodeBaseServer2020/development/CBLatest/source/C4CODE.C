/* c4code.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */



/* LY 2002/10/10 : sprintf in s4stamp.cpp causes compile error with HPC SDK */
#ifndef S4OFF_COMMUNICATIONS
   #ifndef S4SERVER   // CS 2002/06/17
      #if !defined( S4TESTING_OLE )
         #include "stamp5.hpp"
         #include "s4stamp.cpp"
      #endif
   #endif
#endif

#include "zlib.h"

#ifndef S4OFF_COMMUNICATIONS  /* LY 2002/10/10 */
   #include "stamp5.hpp"
   extern STAMP5 CustomerStamp;
#endif

#if defined( S4WINTEL ) && !defined( S4WINCE )
   #include <process.h>
#endif

#if S4VERSION != 6503015
   #error Your CodeBase source version does not match your header file version.
#endif

#ifdef S4DEMO
   #import <trxtd200.dll> no_namespace
#endif

#if defined(S4TESTING) && !defined(S4WINCE)  /* LY 2002/10/04 : added S4WINCE */
   #include <fcntl.h>
   #ifndef S4MACINTOSH
      #ifdef S4UNIX
         #include <sys/types.h>
         #include <sys/stat.h>
      #else
         #include <sys\types.h>
         #include <sys\stat.h>
      #endif
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

#if defined( S4CBPP ) && defined( __cplusplus )
   #include "d4data.hpp"
#endif

#if defined(S4TESTING) || defined(S4TRACK_FILES)
   FILE4 s4test ;
#endif

// AS Oct 24/03 - Support for file_extended with MDX
#ifdef S4FILE_EXTENDED
   C4STAMP largeStamp = {{"BBROYGBVGWE",40},{"LARGELOCK  ", 4}, 0, 0};
#endif



#ifdef S4TESTING
   // AS Jun 20/02 do regular test without 'special options'
   /* LY July 9/03 : added static */
   static Bool5 g_testSpecial = 0 ;
   void S4FUNCTION c4testSetSpecial( Bool5 val )
   {
      // set to '1' to test memo compression, encryption, etc... 0 for normal testing
      g_testSpecial = val ;
   }

   Bool5 S4FUNCTION c4testGetSpecial()
   {
      return g_testSpecial ;
   }
#endif

unsigned short f4memoNullChar = 0 ;  // CS 1999/09/15 changed from char to ushort for Unicode string
char *expr4buf = 0 ;

unsigned int numCode4 = 0 ;  /* used to determine when mem4reset() should be called */
extern int resetInProgress ;

#ifdef S4MAC_TCP
   char initThread = 0 ;
#endif

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



#if defined( S4ENCRYPT_DLL ) && ( defined( S4ENCRYPT_HOOK ) || defined( S4ENCRYPT_COM ) )
   static void code4encryptFileInitUndo( CODE4 *c4 ) ;
   static void code4encryptCodeInitUndo( CODE4 *c4 ) ;
   static void code4encryptComInitUndo( CODE4 *c4 ) ;
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
   /* AS Oct 25/13 e.g. init and init undo of parts of code4 need to be done inside a critical section */
   #ifdef S4SEMAPHORE
      #ifdef S4WIN32
         CRITICAL_SECTION critical4code ;
      #endif
   #endif

   #ifdef __cplusplus
      extern "C"
   #endif
   #ifdef S4FLAT_THUNK
      BOOL WINAPI thk_ThunkConnect32 (LPSTR lpDll16, LPSTR lpDll32, HINSTANCE hDllInst, DWORD dwReason);
   #endif

   #ifdef S4DEMO
      BOOL demo4init(void)
      {
         /* This function runs the Modern TrialX software (www.modernsoftware.com).
            Returns true if the trial is valid. Otherwise false. */
         BOOL ret = false;
         try
         {
            CoInitialize(NULL);
            _mdnTrialPtr myTrial;
            memset(&myTrial,0,sizeof(myTrial));
            myTrial.CreateInstance("trxtd200.mdnTrial");
            myTrial->Key = OLESTR("642P28305u7b08No3cIV5Yk3VN1Js5OX6Iq92Z0F2a6A0Q0K2j8PCl6485F91128KbP52GA73PM2z06e1y56Q3rh2602cuyY069K035gJ2r6MM4rQB251817g83HS07I0e27CQrw0x");

            myTrial->AllowExtend = true;
            //myTrial->Popup = ccNever;
            myTrial->ShowWizardImage = false;
            myTrial->Style = ccDays;
            myTrial->Duration = 30;

            myTrial->Title = "CodeBase SQL 2.0 - Evaluation Version";
            myTrial->Caption = "CodeBase SQL 2.0";
            myTrial->Version = "";
            myTrial->Copyright = "© 1988-2003 Sequiter Software, Inc.";

            myTrial->MsgWelcome = "This is a # trial version of CodeBase SQL 2.0.";
            myTrial->MsgFirst = "Click OK to begin the trial period. Click Cancel to begin the trial period later.";

            myTrial->MsgTimeLeft = "Your trial has # days left.";
            if (myTrial->Status() != ccNotInstalled)
               myTrial->MsgTimeLeft = "Your trial has # left.";
            myTrial->MsgStart = "For information on purchasing CodeBase SQL 2.0 or for technical support, contact us at: \
                                 \r\n\tsales@codebase.com\r\n\tphone: (780) 437-2410\r\n\tfax: (780) 436-2999";
            myTrial->MsgAllow = "Thank you for evaluating CodeBase SQL 2.0.";

            if (myTrial->Check())
               ret = true;
         }
         catch(_com_error &e)
         {
            MessageBox(0,(char *)(e.Description()),"TrialX initialization",MB_ICONSTOP);
         }
         CoUninitialize();

         return ret;
      }
   #endif  /* S4DEMO */

   #ifdef S4WINCE
      BOOL APIENTRY DllMain( HANDLE hModule, DWORD reasonForCall, LPVOID reserved )
   #else
      BOOL APIENTRY DllMain( HINSTANCE hModule, DWORD reasonForCall, LPVOID reserved )
   #endif
   {
      #if defined( __DLL__ ) && !defined( S4PASCAL_DOS ) && defined( S4DLL )
         if ( !cb5inst )
            cb5inst = (HINSTANCE) hModule ;
      #endif

      #ifdef S4SEMAPHORE
         #ifdef S4WIN32
            if ( reasonForCall == DLL_PROCESS_ATTACH )
               InitializeCriticalSection( &critical4code ) ;
            if ( reasonForCall == DLL_PROCESS_DETACH )
               DeleteCriticalSection( &critical4code ) ;
         #endif
      #endif

      #ifdef S4FLAT_THUNK
         if (!thk_ThunkConnect32("c4thk16.DLL", "c4dll.DLL", hModule, reasonForCall))
         {
            return FALSE;
         }
      #endif

      switch (reasonForCall)
      {
         case DLL_PROCESS_ATTACH:
            #ifdef S4DEMO
               if (!demo4init())
                  return false;
            #endif
            break;
         case DLL_PROCESS_DETACH:
            break;
         case DLL_THREAD_ATTACH:
            break;
         case DLL_THREAD_DETACH:
            break;
      }

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



#if defined( __DLL__ ) && !defined( S4PASCAL_DOS ) && defined( S4DLL )
   HINSTANCE S4FUNCTION c4dllInst( void )
   {
      return cb5inst ;
   }
#endif


#ifdef S4WIN32 // LY Jul 16/04
   int S4FUNCTION c4getFuncPtr( HINSTANCE hInst, const char * funcName, void **function )
   {
      // gets the pointer and sets rc to -1 if a failure
      #ifdef S4WINCE /* LY Dec 9/03 */
         WSTR5 uFuncName[255] ;
         memset( uFuncName, 0, 255 * sizeof( unsigned short ) ) ;
         c4atou( funcName, uFuncName, strlen( funcName ) ) ;
         *function = (void *)GetProcAddress( hInst, uFuncName ) ;
      #else
         *function = (void *)GetProcAddress( hInst, funcName ) ;
      #endif
      if ( *function == 0 )
         return -1 ;
      return 0 ;
   }
#endif



/*
AS July 2/02 - Problems arise when using the dll load procedure with zlib, for now use static linking.

static int c4compressLoad( CODE4 *c4)
{
   if ( c4->zlibDll == 0 )
   {
      c4->zlibDll = LoadLibrary( "zlib.dll" ) ;
      if ( c4->zlibDll == 0 )  // if here, we must be able to compress or it is an error
         return error4describe( c4, e4notSupported, E91020, "Unable to load compression dll - zlib.dll", 0, 0 ) ;
      long rc = 0 ;
      c4getFuncPtr( c4->zlibDll, "compress2", (void **)&(c4->compress), &rc ) ;
      if ( rc != 0 || c4->compress == 0 )
         return error4describe( c4, e4notSupported, E91020, "Unable to load compression dll - zlib.dll", 0, 0 ) ;
      c4getFuncPtr( c4->zlibDll, "uncompress", (void **)&(c4->uncompress), &rc ) ;
      if ( rc != 0 || c4->uncompress == 0 )
         return error4describe( c4, e4notSupported, E91020, "Unable to load compression dll - zlib.dll", 0, 0 ) ;
   }

   return 0 ;
}
*/



// AS Jan 20/03 - Don't include if S4COMPRESS not defined
#ifdef S4COMPRESS
   int c4compress( CODE4 *c4, void *outputBuf, unsigned long *outLen, const void *inputBuf, unsigned long inputLen, int level, Bool5 includeLength )
   {
      // if includeLength flag is true, we include the output length at the start of the compressed buffer.
      assert5port( "added compressed memo entries support" ) ;

      unsigned long newOutLen ;
      unsigned char *newOutputBuf ;
      if ( includeLength )
      {
         if ( (*outLen) < sizeof( long ) )   // no room for length
            return -1 ;

         // AS June 20/02, by definition for us, the compression buffer data includes the length
         *((long *)outputBuf) = inputLen ;
         newOutLen = *outLen - sizeof( long ) ;
         newOutputBuf = (unsigned char *)outputBuf + sizeof( long ) ;
      }
      else
      {
         newOutLen = *outLen ;
         newOutputBuf = (unsigned char *)outputBuf ;
      }

      #ifdef S4COMPRESS_ZLIB
         // rc = c4->compress( (char *)outputBuf + sizeof( long ), &newOutLen, inputBuf, inputLen, level ) ;
         int rc = compress2( newOutputBuf, &newOutLen, (const unsigned char *)inputBuf, inputLen, level ) ;
         if ( rc != 0 )
            return rc ;
      #endif

      #ifdef S4COMPRESS_QUICKLZ
         if (c4->qlz_state_compress == 0)
         {
            // Allocate memory for the qlz_state_compress struct.
            c4->qlz_state_compress = u4allocErr(c4, qlz_get_setting(1));
            if (c4->qlz_state_compress == 0)
            {
               return error4code(c4);
            }
         }

         newOutLen = qlz_compress(inputBuf, (char*)newOutputBuf, inputLen, c4->qlz_state_compress);
         if (newOutLen <= 0)
         {
            // Compression failed. No reason available.
            return -1;
         }
      #endif

      if ( includeLength )
         *outLen = newOutLen + sizeof( long ) ;  // room for original length
      else
         *outLen = newOutLen ;

      return 0 ;
   }



   int c4uncompress( CODE4 *c4, void *outputBuf, unsigned long *outLen, const void *inputBuf, unsigned long inputLen )
   {
      assert5port( "added compressed memo entries support" ) ;

      #ifdef S4COMPRESS_ZLIB
         return uncompress( (unsigned char *)outputBuf, outLen, (const unsigned char *)inputBuf, inputLen ) ;
      #endif

      #ifdef S4COMPRESS_QUICKLZ
         if (c4->qlz_state_decompress == 0)
         {
            // Allocate memory for the qlz_state_decompress struct.
            c4->qlz_state_decompress = u4allocErr(c4, qlz_get_setting(2));
            if (c4->qlz_state_decompress == 0)
            {
               return error4code(c4);
            }
         }

         *outLen = qlz_decompress((char*)inputBuf, outputBuf, c4->qlz_state_decompress);
         return (*outLen > 0) ? 0 : -1;
      #endif
   }
#endif /* S4COMPRESS */



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
   // AS Jan 16/05 - S4UTILS not S4UTIL
   #ifdef S4UTILS
      /* S4UTIL, not S4SERVER, not S4OFF_COMMUNICATIONS */
      int S4FUNCTION code4passwordSet( CODE4 *c4, const char *userId, const char *oldPass, const char *newPass )
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
         connection4addData( connection, len, sizeof( len ), NULL ) ;
         connection4addData( connection, userId, len[0], NULL ) ;
         connection4addData( connection, oldPass, len[1], NULL ) ;
         connection4addData( connection, newPass, len[2], NULL ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receive( connection ) ;
         if ( rc < 0 )
            return rc ;
         return connection4status( connection ) ;
      }
   #endif /* S4UTIL */



   /* not S4SERVER, not S4OFF_COMMUNICATIONS */
   static int code4userConnect( CODE4 *c4, const char *userName, const char *password )
   {
      int rc ;
      long version ;
      CONNECTION4 *connection ;

      if ( c4->defaultServer.connected == 0 )
         return 0 ;
      connection = &c4->defaultServer ;
      if ( connection == 0 )
         return 0 ;

      connection4assign( connection, CON4CONNECT, 0L, 0L ) ;
      version = htonl5(CustomerStamp.BuildNumber) ;  // CS 2002/06/18  send build number to server instead of S4VERSION
      connection4addData( connection, &version, sizeof( version ), NULL ) ;
      connection4addData( connection, userName, strlen( userName ) + 1, NULL ) ;
      connection4addData( connection, password, strlen( password ) + 1, NULL ) ;
      if (c4->applicationVerify != NULL)
         connection4addData( connection, c4->applicationVerify, strlen( c4->applicationVerify ) + 1, NULL ) ;
      else
         connection4addData( connection, " ", 2, NULL ) ;
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
      // AS Sep 2/02 - moved from code4userConnect() to make it more general
      #ifndef S4UTILS
         static char defaultUser[] = "PUBLIC" ;

         if ( userName == 0 )
            userName = defaultUser ;
         else
         {
            if ( userName[0] == 0 )
               userName = defaultUser ;
            else
            {
               if ( strlen( userName ) > LEN4ACCOUNT_ID )  // the userid is too large...
               {
                  return error4describe( c4, e4invalidUserId, E84301, "input user name exceeded maximum user id legth of 20", 0, 0 ) ;
               }
            }
         }

         if ( password == 0 )
            password = "" ;
         else
         {
            if ( strlen( password ) > LEN4PASSWORD )  // the userid is too large...
            {
               return error4describe( c4, e4invalidPassword, E84301, "input password exceeded maximum passowrd legth of 20", 0, 0 ) ;
            }
         }
      #endif

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
      rc = connect4connect( connect, c4, portNo, serverId ) ;
      if ( rc != 0 )  // AS Sept 30/02 - was ignoring the timeout error, causing problems
      {
         assert5( c4->defaultServer.connected == 0 ) ;
         return rc ; /* An error4() has already been called */
      }

      connection4init(&c4->defaultServer, connect ) ;
      c4->defaultServer.connected = 1 ;

      // AS Mar 12/03 - Always ask server whether or not it requires encryption.
      // If it does require encryption we need to ensure we don't send unencrypted userid/password.
      #if defined( S4PREPROCESS_COM ) || defined( S4PREPROCESS_FILE )
         rc = code4serverPreprocessInit( c4, userName, password ) ;
      #else
         connect4sendShort( connect, MSG5PREPROCESS_DISABLE ) ;
         connect4sendFlush( connect ) ;
         rc = connect4receiveShort( connect ) ;
         if ( rc != 0 )
            error4( c4, rc, E91004 ) ;
      #endif

      #ifndef S4UTILS
         if ( rc == 0 )
            rc = code4userConnect( c4, userName, password ) ;
      #endif

      if ( rc == 0 )
      {
         rc = code4dateFormatSet( c4, code4dateFormat( c4 ) ) ;
         c4->ignoreCase = ! (code4serverOS( c4 ) & OS4UNIX ) ;  /* CS 2001/01/22 */
      }

      if ( rc < 0 )
      {
         connection4initUndo( &c4->defaultServer ) ;
         assert5( c4->defaultServer.connected == 0 ) ;
      }

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

      assert5( rc == 0 || c4->defaultServer.connected == 0 ) ;

      return rc ;
   }
#endif /* !S4SERVER && !S4OFF_COMMUNICATIONS */



/* LY July 8/03 : added static */
static int g_extraInits = 0 ;   // track extra mem4inits that are done by code4allocLow, allow to release later

// AS Aug 6/04 - ensure we only call this once per application (with off-multi if we actually have multiple
// CODE4's we have problems...
#if defined( VALID4ENABLE ) && defined( S4OFF_MULTI ) && defined( S4STAND_ALONE )
   static Bool5 valid4avail = 1 ;
#endif

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION code4initLow( CODE4 *c4, const char *defaultProtocol, long versionId, LONGLONG structSize, char skipCom )
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
         // AS Dec 13/05 - under Windows sprintf is becoming deprecated...
         #ifdef S4WINDOWS_VS5_PLUS
            sprintf_s( errLib,    sizeof( errLib ), "Library: %ld", S4VERSION ) ;
            sprintf_s( errHeader, sizeof( errHeader ), "Headers: %ld", versionId ) ;
         #else
            c4sprintf( errLib,    "Library: %ld", S4VERSION ) ;
            c4sprintf( errHeader, "Headers: %ld", versionId ) ;
         #endif
      #endif
      return error4describe( 0, e4version, E91001, errLib, errHeader, 0 ) ;
   }

   // CS 2007/02/21 Condense lines.
   if ( structSize != 0 && structSize != code4structSize(c4))
   {
      // error, switch mismatch most likely
      char errHeader[30], errLib[30] ;
      #ifdef S4WINCE
         strcpy( errLib, "Library struct size: " );
         _ltoa( code4structSize(c4), errLib + 21, 10 );
         strcpy( errHeader, "Header struct size: " );
         _ltoa( structSize, errHeader + 20, 10 );
      #else
         /* LY 00/09/20 : cast sizeof() to long (win64) */
         // AS Dec 13/05 - under Windows sprintf is becoming deprecated...
         #ifdef S4WINDOWS_VS5_PLUS
            sprintf_s( errLib, sizeof( errLib ), "Library struct size: %ld", code4structSize(c4));
            sprintf_s( errHeader, sizeof( errHeader ), "Header struct size: %ld", structSize );
         #else
            c4sprintf( errLib, "Library struct size: %ld", code4structSize(c4));
            c4sprintf( errHeader, "Header struct size: %ld", structSize );
         #endif
      #endif
      return error4describe( 0, e4verify, E81507, errLib, errHeader, 0 ) ;
   }

   /***********************************************************************

      This code must reside at the beginning of the function to ensure that
      no file open/create incorrectly auto-starts the optimization process

    ***********************************************************************/

   c4memset( (void *)c4, 0, sizeof( CODE4 ) ) ;

   #ifdef S4MACINTOSH
      #ifdef S4CARBON_APP  // LY Jul 16/04 : changed #if to #ifdef
         c4->macDir = 0 ;
         c4->macVol = 0 ;
      #else
         c4->macDir = *(long *)0x398 ;
         c4->macVol = -(*(short *)0x214) ;
      #endif
   #endif

   // AS Apr 5/07 - adjust...if the calling thread is not high priority we don't need to sleep
   #ifdef S4WIN32
      switch( GetThreadPriority( GetCurrentThread()))
      {
         case THREAD_PRIORITY_ABOVE_NORMAL:
         case THREAD_PRIORITY_HIGHEST:
            c4->highPriority = 1 ;
            break ;
         default:
            c4->highPriority = 0 ;
            break ;
      }
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

   #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
      EnterCriticalSection( &critical4code ) ;
   #endif

   #if defined( S4TESTING ) || defined( S4TRACK_FILES )
      if ( numCode4 == 0 )
      {
         memset( &s4test, 0, sizeof( s4test ) ) ;
         s4test.hand = INVALID4HANDLE ;
      }
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

   #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
      LeaveCriticalSection( &critical4code ) ;
   #endif

   #ifndef S4OFF_TRAN
      // AS Apr 28/03 - made trans-shared a run-time switch
      // AS Jun 20/03 removed this switch altogether, choice of sharing of log files is now completely run-time
      // #ifdef S4TRANS_FILE_SHARED
      //    c4->transShared = 1 ;
      // #endif

      // the default for sharing of log files is now as follows:
      // server - we leave it 0 here, if it is an ODBC server and the log file requests ODBC transactions,
      //          the server code sets transShared to 1, otherwise it is left 0.
      // s/a -    we set it to 1 here (the default).  If the user of the DLL is the ODBC engine, the ODBC engine
      //          resets the value to 0 because the ODBC engine itself does not share the log file.
      #ifdef S4STAND_ALONE
         c4->transShared = 1 ;
      #endif
   #endif
   // AS Jun 20/03 - Moved from c4code.c to allow setting of transShared to be done after code4init but prior
   // to opening log file (for c4dll.dll, odbc s/a uses exclusive log file, but non-odbc s/a shares)

   #if defined(E4ANALYZE) || defined(E4VBASIC)
      c4->debugInt = E4DEBUG_INT ; /* Some random value for double checking. */
   #endif

   #ifndef S4OFF_MEMO
      #ifndef S4SERVER
         // AS Aug 29/03 - This value is stored on a client by client basis
         c4->memSizeMemoExpr = 0x400 ;        /*  1024 */
      #endif
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
   // AS Dec 3/03 - we want the ability to set off the encryption error, in particular when we attempt to load it internally
   c4->errEncrypt = 1 ;
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
         #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
            EnterCriticalSection( &critical4code ) ;
         #endif

         numCode4--;

         #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
            LeaveCriticalSection( &critical4code ) ;
         #endif
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

   #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      // AS Jan 3/03 - If the table is empty, return a failure code
      c4->compressWrite = 0 ;  // by default don't support writing to compressed tables
      c4->compressArrayAllocCount = ( 65536 - sizeof( COMPRESS4WRITE_BLOCK ) ) / sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ;   // the number of array entries to add when expanding out the write block array - default to 64k worth
   #endif

   // AS Oct 24/03 - Support for file_extended with MDX
   #if defined( S4FILE_EXTENDED ) && ( !defined( S4ODBC_BUILD ) || defined( S4STAND_ALONE ) )
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
   c4->errUnlock = 1 ;  // AS Mar 26/03 - Added for odbc

   #if defined(S4TESTING) || defined(S4TRACK_FILES)   /* LY 2002/01/23 : avoid compile error */
      if ( numCode4 == 1 )  // only do if a single CODE4, otherwise we get in a recursive situation where the
                            // error file cannot get created
      if ( s4test.hand == INVALID4HANDLE )
      {
         #ifdef S4TESTING
            openFlag = c4->errOpen ;
            int exclusiveFlag = c4->accessMode ;
            c4->errOpen = 0 ;
            c4->accessMode = OPEN4DENY_NONE ;
            #ifndef S4WINCE   /* LY 2002/08/28 */
               envLog = getenv( "T4LOG" ) ;
               if ( envLog )           /* if env. var. exists, use it */
                  logFile = envLog ;
            #endif
            // AS May 24/02 - created file4openLow for internal use to indicate file types
            if ( file4openInternal( &s4test, c4, logFile, 0, OPT4NONE ) != r4success )
            {
               // AS Oct 24/02 - if S4OFF_MULTI is defined, this does not work with T4PYTHON2,
               // due to duplicate processes and logFile open by another process.  In that case
               // here allow for a failure and continue
               #ifdef S4OFF_MULTI
                  int createCode = c4->errCreate ;
                  c4->errCreate = 0 ;
               #endif
               int rcx = file4createInternal( &s4test, c4, logFile, 0, OPT4NONE ) ;
               #ifdef S4OFF_MULTI
                  c4->errCreate = createCode ;
                  if ( rcx != 0 ) // try another name...
                  {
                     logFile = "T4TEST2.log" ;
                     if ( file4openInternal( &s4test, c4, logFile, 0, OPT4NONE ) != r4success )
                     {
                        int rcx = file4createInternal( &s4test, c4, logFile, 0, OPT4NONE ) ;
                     }
                  }
               #endif
            }
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
   #endif
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

      // LY Jul 20/04 : added !S4MACINTOSH
      #if defined( S4PREPROCESS_FILE ) && !defined( S4SERVER ) && !defined( S4ENCRYPT_DLL ) && !defined( S4MACINTOSH )
         preprocess4fileInit( c4 ) ;
      #endif

      #if defined(S4WRITE_DELAY) || defined(S4READ_ADVANCE)
         clock_t clockStart ;
      #endif

      #ifdef S4WRITE_DELAY
         /* if the begin thread fails, then delay-writes are simply not enabled,
            which is ok because it will simply be bypassed. */
         /* CS 2000/05/10 Also disable delay-writes if begin thread succeeds,
            but thread fails to start after 5 seconds. Possible MS bug? */
         critical4sectionInit( &c4->critical4delayWriteList ) ;
         c4->pendingWriteEvent = CreateEvent( 0, TRUE, FALSE, 0 ) ;
         // AS Nov 21/05 - due to an apparent vb/xp sequencing problem...
         c4->delayWritesDisabled = 0 ;
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
                  // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
                  // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
                  u4sleep( c4 ) ;
               }
            }
            if ( rc < 0 )
            {
               // AS Nov 21/05 - due to an apparent vb/xp sequencing problem, indicate we cancelled the initialization
               c4->delayWritesDisabled = 1 ;
               CloseHandle( c4->pendingWriteEvent ) ;
               #ifdef S4WIN64 /* LY 00/09/20 */
                  c4->pendingWriteEvent = NULL ;
               #else
                  c4->pendingWriteEvent = INVALID4HANDLE ;
               #endif
               critical4sectionInitUndo( &c4->critical4delayWriteList ) ;
            }
         }
      #endif

      #ifdef S4READ_ADVANCE
         /* if the begin thread fails, then delay-reads are simply not enabled,
            which is ok because it will simply be bypassed. */
         /* CS 2000/05/10 Also disable delay-reads if begin thread succeeds,
            but thread fails to start after 5 seconds. Possible MS bug? */
         critical4sectionInit( &c4->critical4advanceReadList ) ;
         c4->pendingReadEvent = CreateEvent( 0, TRUE, FALSE, 0 ) ;
         // AS Nov 21/05 - due to an apparent vb/xp sequencing problem...
         c4->advanceReadsDisabled = 0 ;
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
                  // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
                  // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
                  u4sleep( c4 ) ;
               }
            }
            if ( rc < 0 )
            {
               // AS Nov 21/05 - due to an apparent vb/xp sequencing problem, indicate we cancelled the initialization
               c4->advanceReadsDisabled = 1 ;
               CloseHandle( c4->pendingReadEvent ) ;
               #ifdef S4WIN64
                  c4->pendingReadEvent = NULL ;
               #else
                  c4->pendingReadEvent = INVALID4HANDLE ;
               #endif
               critical4sectionInitUndo( &c4->critical4advanceReadList ) ;
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
         #ifndef S4JNI  /* LY 2003/05/28 : customer complaint about memory usage */
            c4->memMaxPercent = 25 ;   /* default use 25% of available memory */
            code4memStartMaxSet( c4, c4->memMaxPercent ) ;  /* start optimization if not enabled and not suspended */
         #endif
      #endif

   #endif /* S4OFF_OPTIMIZE */

   c4->skipCom = skipCom ;

   // AS Jan 9/02 - added support to have a timeout handling be set by user (via OLE-DB) on waiting for the server
   // to accept our connection.
   #if defined( S4WIN32 ) && !defined( S4OFF_THREAD ) && defined( S4CLIENT )
      c4->acceptTimeOut = CON4LOW_ACCEPT_TIME ;
   #endif

   if ( skipCom == 0 )
   {
      #ifndef S4STAND_ALONE
         #ifdef S4WINTEL
            rc = WSAStartup( MAKEWORD( 1,1 ), &WSAData ) ;
         #endif
         #ifdef S4MAC_TCP
            if( initThread == 0 )
            {
               new UMainThread ;  // CJ Jun 21/01 - Mac clients need the threading to be initialized only once
               initThread = 1 ;
            }
         #endif
         #ifdef S4MACOT_TCP
            #ifdef S4CARBON_APP
               InitOpenTransportInContext( kInitOTForApplicationMask, NULL) ;
            #else
               InitOpenTransport() ;
            #endif
         #endif
         c4->ver4 = code4osVersion() ;
         if ( c4->ver4 <= 0 )
         {
            #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
               EnterCriticalSection( &critical4code ) ;
            #endif

            numCode4--;

            #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
               LeaveCriticalSection( &critical4code ) ;
            #endif
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
               #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
                  EnterCriticalSection( &critical4code ) ;
               #endif
               numCode4--;
               #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
                  LeaveCriticalSection( &critical4code ) ;
               #endif
               return error4( 0, e4memory, E91001 ) ;
            }
            if ( inter4init( c4->inter, c4 ) < 0 )
            {
               #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
                  EnterCriticalSection( &critical4code ) ;
               #endif

               numCode4--;

               #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
                  LeaveCriticalSection( &critical4code ) ;
               #endif
               return error4( 0, e4result, E91001 ) ;
            }
            c4->interThreadHandle = _beginthread( inter4, 5000, c4->inter ) ;
            if ( c4->interThreadHandle == -1 )
            {
               inter4initUndo( c4->inter ) ;
               #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
                  EnterCriticalSection( &critical4code ) ;
               #endif

               numCode4--;

               #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
                  LeaveCriticalSection( &critical4code ) ;
               #endif
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

   #if defined( S4WIN32 ) && defined(__cplusplus) && defined(OLEDB5BUILD)
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

   // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE
   c4->limitKeySize = 1 ;    // by default the key size is limited to I4MAX_KEY_SIZE


   c4->autoIncrementStart = 1.0 ;

   #if defined( S4COMPRESS ) && !defined( S4SERVER )
      // AS May 13/04 - configureable compression levels
      c4->fileCompressLevel = 9 ;   // default to maximum file compression
   #endif

   // AS Aug 6/04 - just a note VALID4ENABLE is only defined internally if S4TESTING is defined, for testing purposes
   // it is never used in the release version.
   #if defined( VALID4ENABLE ) && defined( S4OFF_MULTI ) && defined( S4STAND_ALONE )
      // AS Oct 22/02 - make available to be called in server automatically even in non-multi
      if ( skipCom != 1 )   /* for debug/ats, don't do the validation */
      {
         // AS Aug 6/04 - ensure we only call this once per application (with off-multi if we actually have multiple
         // CODE4's we have problems...
         if ( valid4avail == 1 )
         {
            code4validate( c4, 0, 1 ) ;
            valid4avail = 0 ;
         }
      }
   #endif

   #ifdef S4TESTING
      // AS Jun 20/02 do regular test without 'special options'
      if ( g_testSpecial == 1 )
      {
         #if defined( S4CLIENT_OR_FOX ) && defined( S4COMPRESS )
            c4->compressedMemos = 1 ;    // test with compressed memos in this case!
         #endif
         c4->compatibility = 30 ;  // required for memo compression
      }
   #endif

   // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
   #ifdef RELATE4LOG
      if ( numCode4 == 1 )
      {
         int openFlag = c4->errOpen ;
         c4->errOpen = 0 ;
         if ( file4open( &c4->relateLogFile, c4, "RELATE4LOG", 0 ) != r4success )
            file4create( &c4->relateLogFile, c4, "RELATE4LOG", 0 ) ;
         c4->errOpen = openFlag ;
      }
   #endif

   // AS May 10/07 - initialize the transaction status to inactive...previously was incorrectly set to 0
   #if !defined( S4OFF_TRAN ) && !defined( S4SERVER )
      code4tranStatusSet( c4, r4inactive ) ;
   #endif

   #if !defined(S4OFF_COMPRESS) && defined(S4COMPRESS_QUICKLZ)
      c4->qlz_state_compress = 0;
      c4->qlz_state_decompress = 0;
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
   #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
      EnterCriticalSection( &critical4code ) ;
   #endif

   #if defined( S4TESTING ) || defined( S4TRACK_FILES )
      // AS 08/16/99 - was sometimes not getting set, and then an error here causing endless loop...
      if ( numCode4 == 0 )
      {
         memset( &s4test, 0, sizeof( s4test ) ) ;
         s4test.hand = INVALID4HANDLE ;
      }
   #endif

   // AS Feb 14/03 - If code4allocLow() is used in place of static CODE4's there was
   // a memory leak occurring due to an extra call to mem4init().
   if ( numCode4 == 0 && resetInProgress == 0 )
   {
      mem4init() ;
      g_extraInits++ ;
   }

   // AS Nov 5/2013 - need to set the numCode4 value earlier to avoid conflict with another user in this same code
   numCode4++ ;

   #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
      LeaveCriticalSection( &critical4code ) ;
   #endif

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
      // AS Feb 26/02 - Not taking into account possible failure of init (e.g. version mismatch)
      // CS 2007/03/21 Structure size need not be checked when CODE4 struct created within DLL. Therefore, pass 0 for structSize parameter.
      int rc = code4initLow( c4, defaultProtocol, versionId, 0, 0 ) ;
      if ( rc < 0 )
      {
         // on the uninit we need to reset all the values that were potentially set up.  We also need to mark that
         // there is at least 1 CODE4 because otherwise the u4free will fail...
         #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
            EnterCriticalSection( &critical4code ) ;
         #endif

         u4free( c4 ) ;
         numCode4-- ;
         g_extraInits-- ;
         if ( numCode4 == 0 && resetInProgress == 0)   /* reset memory */
            mem4reset() ;

         #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
            LeaveCriticalSection( &critical4code ) ;
         #endif

         return 0 ;
      }

      // AS Nov 5/2013 - a bit of a side-effect, both code4initLow and code4allocLow now increment the numCode4 counter.  If I have got here, it means I need to decrement the counter one more time
      #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
         EnterCriticalSection( &critical4code ) ;
      #endif

      numCode4-- ;

      #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
         LeaveCriticalSection( &critical4code ) ;
      #endif

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

   code4initLow( c4, defaultProtocol, S4VERSION, code4structSize(c4), 0 ) ;
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

   // AS Oct 25/01 - Do the 'initialized' check first because if it is not initialized, the
   // parm check will fail.

   if ( c4->initialized == 0 )  /* already uninitialized */
      return 0 ;

   #ifdef E4VBASIC
      if ( c4parm_check( c4, 1, E40138 ) )
         return -1 ;
   #endif

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

   #if defined(S4WINCE)  // CS 2001/06/18 always free memory
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

   #ifndef S4SERVER
      // AS Apr 30/02 - Allow for auto-freeing of relations on code4initUndo - server does on a per-client basis
      // need to do this near top while data4's not closed and connection still available
      for ( ;; )
      {
         RELATION4 *relation = (RELATION4 *)l4first( &c4->relations ) ;
         if ( relation == 0 )
            break ;
         relate4free( &relation->relate, 0 ) ;
      }
   #endif

   #ifdef S4SERVER
      #ifndef S4UNIX
         CloseHandle( c4->accessMutex ) ;
      #endif
      // AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
      // if ( doClose )
      // {
      //    code4validateUndo( c4 ) ;
      // }
   #else
      #ifndef S4OFF_TRAN
         #ifndef S4CLIENT
            if ( code4transEnabled( c4 ) )
         #endif
            {
               if ( code4tranStatus( c4 ) == r4active )
                  code4tranRollback( c4 ) ;
               // AS Aug 13/01 - If a failure occurs (client/server) in a rollback, the transaction
               // state may still be set to r4active - change this now physically so that close
               // files will for sure get reset
               if ( code4tranStatus( c4 ) == r4active )
                  code4tranStatusSet( c4, r4inactive ) ;
            }
      #endif

      #if !defined(S4SERVER) && !defined(S4OFF_TRAN)
         if ( c4->transFileName != 0 )
         {
            u4free( c4->transFileName ) ;
            c4->transFileName = 0 ;
         }
      #endif

      if ( doClose )
      {
         code4close( c4 ) ;
         #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
            #if defined( VALID4ENABLE ) && defined( S4OFF_MULTI ) && defined( S4STAND_ALONE )
               // AS Aug 6/04 - ensure we only call this once per application (with off-multi if we actually have multiple
               // CODE4's we have problems...
               if ( c4->validationTable != 0 )  // re-enable the validation after we undo...
                  valid4avail = 1 ;
            #endif
            code4validateUndo( c4 ) ;
         #endif
      }

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

   #ifdef S4ENCRYPT_DLL
      #ifdef S4PREPROCESS_FILE
         code4encryptFileInitUndo( c4 ) ;
      #endif
      #ifdef S4PREPROCESS_COM
         code4encryptCodeInitUndo( c4 ) ;
      #endif
   #else
      #ifdef S4PREPROCESS_FILE
         #ifndef S4WIN32   // LY Jul 20/04
            if ( c4->encryptInit )
               code4encryptFileInitUndoLow( c4 ) ;
            c4->encryptPreprocess = 0 ;
            c4->encryptPreprocessInit = 0 ;
            c4->encryptInit = 0 ;
         #else
            preprocess4fileInitUndo( c4 ) ;
         #endif
      #endif
      #ifdef S4PREPROCESS_COM
         preprocess4comInitUndo( c4 ) ;
      #endif
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

   // AS Nov 8/04 - fix general collation bug...
   if ( c4->fieldBuffer2 != 0 )
   {
      u4free( c4->fieldBuffer2 ) ;
      c4->fieldBuffer2 = 0 ;
      c4->bufLen2 = 0 ;
   }

   // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE
   if ( c4->tagAddBuf != 0 )
   {
      u4free( c4->tagAddBuf ) ;
      c4->tagAddBuf = 0 ;
      c4->tagAddBufLen = 0 ;
   }
   if ( c4->tagAddBuf2 != 0 )
   {
      u4free( c4->tagAddBuf2 ) ;
      c4->tagAddBuf2 = 0 ;
      c4->tagAddBufLen2 = 0 ;
   }

   // AS Jul 6/04 - s/a and server only
   #ifndef S4CLIENT
      if ( c4->savedKey != 0 )
      {
         u4free( c4->savedKey ) ;
         c4->savedKey = 0 ;
         c4->savedKeyLen = 0 ;
      }
   #endif

   if ( c4->storedKey != 0 )
   {
      u4free( c4->storedKey ) ;
      c4->storedKey = 0 ;
      c4->storedKeyLen = 0 ;
   }

   if ( c4->errorLog != 0 )
   {
      /* LY 2003/07/31 #ifdef S4WIN64
            if ( c4->errorLog->hand != NULL )
         #else
      */
         if ( c4->errorLog->hand != INVALID4HANDLE )
      // #endif
         file4close( c4->errorLog ) ;
      u4free( c4->errorLog ) ;
      c4->errorLog = 0 ;
   }

   #ifdef RELATE4LOG
      file4close( &c4->relateLogFile ) ;
   #endif

   #ifdef S4WIN32 // LY Jul 16/04
      // AS Apr 4/03 - stored procedure support
      if ( c4->addDll != 0 )
      {
         FreeLibrary( c4->addDll ) ;
         c4->addFunction = 0 ;
         c4->addDll = 0 ;
      }
   #endif

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
               // AS July 18/02 - it is possible that there are still links on this list, although we
               // have shut down the worker threads which service the list.  Therefore remove any items.
               while ( l4numNodes( &(c4->connectsToService.list) ) != 0 )
               {
                  if ( list4mutexRemove( &(c4->connectsToService) ) == 0 )  // no items left
                     break ;
               }

               list4mutexInitUndo(&c4->connectsToService) ;
            #endif
            /* AS 08/02/99 there may still be elements on this list.  If so, must remove and
               delete them...
            */
            if ( list4mutexWait( &c4->writeBuffersAvail ) == 0 )
            {
               for( ;; )
               {
                  NET4MESSAGE *message = (NET4MESSAGE *)l4first( &c4->writeBuffersAvail.list ) ;
                  if ( message == 0 )
                     break ;

                  l4remove( &c4->writeBuffersAvail.list, message ) ;
                  assert5( message->inUse == 0 ) ;
                  mem4free( c4->writeMemory, message ) ;
               }
               list4mutexRelease( &c4->writeBuffersAvail ) ;
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

   #ifdef S4PREPROCESS_FILE
      if ( c4->filePreprocessBuffer != 0 )
      {
         u4free( c4->filePreprocessBuffer ) ;
         c4->filePreprocessBuffer = 0 ;
      }
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

   #ifdef S4CLIENT
      // AS Sept. 26/02 - log connection info
      if ( c4->connLogFileOpen == 1 )
         code4logConn( c4, 0, 0 ) ;
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
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( c4 ) ;
            }
            CloseHandle( c4->initUndoDelayWrite ) ;
            CloseHandle( c4->pendingWriteEvent ) ;
            critical4sectionInitUndo( &c4->critical4delayWriteList ) ;
         }
      }
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
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( c4 ) ;
            }
            CloseHandle( c4->initUndoAdvanceRead ) ;
            CloseHandle( c4->pendingReadEvent ) ;
            critical4sectionInitUndo( &c4->critical4advanceReadList ) ;
         }
      }
   #endif

   #if defined( S4WIN32 ) && defined(__cplusplus) && defined(OLEDB5BUILD)
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

   if ( c4->lastErrorDescription )  // CS 2002/04/24
      u4free( c4->lastErrorDescription ) ;

   #if defined( S4CLIENT_OR_FOX ) && defined( S4COMPRESS )  /* LY 2002/12/27 */
      // AS Jun 19/02 - memo compression
      if ( c4->compressedMemosBuffer != 0 )
         u4free( c4->compressedMemosBuffer ) ;
   #endif

   #if !defined(S4STAND_ALONE) && !defined(S4OFF_THREAD) && defined(S4COMPRESS)  // CS 2011/05/05
      if ( c4->extraReadMessage )  // CS 2002/04/24
         u4free( c4->extraReadMessage ) ;
   #endif

   #ifdef S4LOCK_CHECK
      if ( c4->lockAlloc != 0 )
      {
         mem4release( c4->lockAlloc ) ;
         c4->lockAlloc = 0 ;
      }
   #endif

   #if !defined(S4OFF_COMPRESS) && defined(S4COMPRESS_QUICKLZ)
      if (c4->qlz_state_compress != 0)
      {
         u4free(c4->qlz_state_compress);
      }

      if (c4->qlz_state_decompress != 0)
      {
         u4free(c4->qlz_state_decompress);
      }
   #endif

   c4->initialized = 0 ;
   if ( c4->didAlloc == 1 )
      u4free( c4 ) ;

   #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
      EnterCriticalSection( &critical4code ) ;
   #endif

   #if defined( S4FOX ) && !defined( S4OFF_INDEX )
      if ( numCode4 == 1 )  // free up collation information... but before decrementing numCode4 since freeing fails the
      {
         assert5( numCode4 == 1 ) ;  // must be > 0 for freeing to work, if > 1, shouldn't free yet
         collate4initUndo() ;
      }
   #endif

   numCode4-- ;

   // AS Oct 24/03 - Support for file_extended with MDX
   #if defined(S4FILE_EXTENDED)
      if (numCode4 == 0 && largeStamp.previousCode4init)
      {
         largeStamp.previousCode4init=0;
         largeStamp.doLargeLocking=0;
      }
   #endif

   if ( numCode4 == 0 && resetInProgress == 0)   /* reset memory */
      mem4reset() ;

   #if defined( __DLL__ ) && defined( S4WIN32 ) && !defined( S4JOINT_OLEDB_DLL ) && defined( S4SEMAPHORE ) && defined( S4WIN32 )
      LeaveCriticalSection( &critical4code ) ;
   #endif

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

      #if  !defined(S4WINDOWS)
         exit( rc ) ;
      #elif defined(S4TESTING)
         u4terminate() ;
      #elif defined(S4WINCE)
         ExitThread(0) ;
      #else
         FatalAppExit( 0, TEXT(E4_MESSAG_EXI) ) ;
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
         // AS Oct 22/02 - not required to be single user
         #if !defined( S4CLIENT )
            /* don't close the internal validation table here */
            if ( dataOn != c4->validationTable )
         #endif
         {
            #if !defined( S4OFF_OPTIMIZE ) && defined( S4OPTIMIZE_STATS )
               /* don't close the internal opt tracking dbf here. */
               if ( dataOn != c4->statusDbf )
            #endif
            {
               d4close( dataOn ) ;
            }
         }
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
      #ifndef S4CLIENT
        FSSpec macSpec ;
        Str255 macStr ;
        char len ;
      #endif
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

      const char *dfileName = dfile4name( dataOn ) ;
      if ( dfileName == 0 )  // not sure when this could happen - but maybe a temporary table? - was GPF'ing here
         break ;
      if ( u4namecmp( aliasName, dfileName, c4->ignoreCase ) == 0 )
      {
         #if defined( S4MACINTOSH ) && defined( S4STAND_ALONE )
            CopyCStringToPascal( aliasName, macStr ) ;

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
         #if defined( S4MACINTOSH ) && defined( S4STAND_ALONE )
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
      // AS Sept. 23/02 - We need to know the index format for dbf type reasons as well, so don't return r4unknown if S4OFF_INDEX is defined
      //#ifdef S4OFF_INDEX
      //   return r4unknown ;
      //#else
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
      //#endif
   }
#endif /* S4CLIENT else */



void S4FUNCTION code4largeOn( CODE4 *c4 )
{
   // AS Oct 24/03 - Support for file_extended with MDX
   #if !defined(S4STAND_ALONE) || !defined(S4FILE_EXTENDED) || defined(S4CLIPPER)  // CS 2002/11/21 remove unnecessary conditions, improve logic
      error4describe( c4, e4notSupported, E96702, "code4largeOn not supported with this implementation", 0, 0 ) ;
   #else
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
   #endif
}



#if defined(S4FILE_EXTENDED)
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
         // AS Aug 5/02 - Also catch case where c4 not initilized but not null...
         if ( c4 == 0 )
            return error4( 0, e4parmNull, E91013 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( c4->debugInt != E4DEBUG_INT )
            return error4( 0, e4parm_null, E91013 ) ;
      #endif

      #ifdef E4VBASIC
         // CS 2002/08/29 only if E4VBASIC
         if ( c4parm_check( c4, 1, E91013 ) )
            return 0 ;
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
      #if defined( S4ODBC_BUILD ) && defined( S4STAND_ALONE )
         // with odbc, create the log file if not open...
         if ( rc == r4noExist )
            rc = code4transFileEnable( code4trans( c4 )->c4trans, fileName, 1 ) ;
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

// for the 2nd u4switch
#ifdef E4VBASIC
   #define S2_V4BASIC_VAL 1
#else
   #define S2_V4BASIC_VAL 0
#endif

#ifdef S4CONSOLE
   #define S2_CONSOLE_VAL 2
#else
   #define S2_CONSOLE_VAL 0
#endif

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


// AS Aug 5/02 - Add support for more switches
long S4FUNCTION u4switch2()
{
   return (long)S2_V4BASIC_VAL + S2_CONSOLE_VAL ;
}




// AS May 10/07 -- not used anymore...
/*
#ifndef S4OFF_CONTROLS
   #define CTRL4SERVERNAMESIZE 260

   // Structures for CodeControls Functions

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
   int         S4FUNCTION ctrl4addCode( HINSTANCE hInst ) ;                                 //450
   void        S4FUNCTION ctrl4codeListInit( void ) ;                                        //451
   void        S4FUNCTION ctrl4freeCtrlNode( CTRL4CODE *node ) ;
   void        S4FUNCTION ctrl4freeCodeList( void ) ;                                        //452
   CTRL4CODE * S4FUNCTION ctrl4getCtrlCode( HINSTANCE hInst ) ;                              //453
   void        S4FUNCTION ctrl4getServerName( HINSTANCE hInst,char *serverName,int strLen ) ; // 5 oh something
   void        S4FUNCTION ctrl4setServerName( HINSTANCE hInst,char *serverName ) ; // 5 oh something
   void        S4FUNCTION ctrl4initVBX( CODE4 *code,HINSTANCE hInstance,int initialize ) ;   //454
   void        S4FUNCTION ctrl4initVBXUndo( CODE4 *code,HINSTANCE hInstance ) ;              //455
   #ifdef __cplusplus
      }
   #endif



   // CS 2007/05/08 Remove CC VBX functions.


   #ifdef S4CLIENT
      void  S4FUNCTION ctrl4getServerName( HINSTANCE hInst, char *serverName, int strLen ) // oh something
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



      void S4FUNCTION ctrl4setServerName( HINSTANCE hInst,char *serverName ) // oh something
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

// AS May 10/07 -- not used anymore...
/*
   #ifdef S4STAND_ALONE
      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      void  S4FUNCTION ctrl4getServerName( HINSTANCE hInst,char *serverName,int strLen ) //5 oh something
      {
         return ;
      }



      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      void S4FUNCTION ctrl4setServerName( HINSTANCE hInst,char *serverName ) //5 oh something
      {
         return ;
      }
   #endif //
#endif //
*/



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
   #if defined( S4COM_PRINT ) && defined( S4TESTING ) && defined( E4FILE_LINE )
      void S4FUNCTION code4enterExclusiveDo( CODE4 *c4, SERVER4CLIENT *client, const char *file, int line, int doDelay )
   #else
      void S4FUNCTION code4enterExclusive( CODE4 *c4, SERVER4CLIENT *client, int doDelay )
   #endif
   {
      /* AS 09/30/98 --> in case of no-delay, still wait for 10 secs to allow processes to partially clean up
                         esp. in some instances */
      #ifndef S4UNIX
         WaitForSingleObject( c4->accessMutex, doDelay ? INFINITE : 10000 ) ;
      #endif
      #ifdef E4MUTEX_CHECK
         // AS May 30/06 - we shouldn't be trying to get the list if we already have the client list...we have to get this first
         // otherwise we may end up with a locking conflict...unless we already hold the mutex in which case this is ok

         if ( c4->server->clientListHolder == client )
         {
            if ( c4accessMutexCountNotZero( c4 ) )
            {
               if ( c4->currentClient != client )   // reset the access count to '1' since we are taking over control
                     error4( c4, e4parm, E70223 ) ;
            }
            else
               error4( c4, e4parm, E70223 ) ;
         }
      #endif

      #ifdef E4ANALYZE
         if ( doDelay )
         {
            if ( c4accessMutexCountNotZero( c4 ) != 0 )
            {
               if ( c4->currentClient != client )
                  error4( c4, e4parm, E70223 ) ;
            }
         }
      #endif

      #if defined( S4COM_PRINT ) && defined( S4TESTING )
         char outBuf[120] ;
         #ifdef E4FILE_LINE
            sprintf( outBuf, "ATTEMPT code4enterExclusive from file: %s, line: %ld for client: %ld", file, (long)line, client->id ) ;
         #else
            sprintf( outBuf, "ATTEMPT code4enterExclusive for client: %ld", client->id ) ;
         #endif
         u4writeErr( outBuf, 1 ) ;
      #endif

      if ( doDelay == 0 )
      {
         if ( c4accessMutexCountNotZero( c4 ) )
            if ( c4->currentClient != client )   // reset the access count to '1' since we are taking over control
               c4setAccessMutexCount( c4, 0 ) ;
      }

      c4setAccessMutexCount( c4, c4getAccessMutexCount( c4 ) + 1 ) ;
      c4->currentClient = client ;

      #if defined( S4COM_PRINT ) && defined( S4TESTING )
         #ifdef E4FILE_LINE
            sprintf( outBuf, "    SUCCESS code4enterExclusive from file: %s, line: %ld for client id: %ld", file, (long)line, client->id ) ;
         #else
            sprintf( outBuf, "    SUCCESS code4enterExclusive for client id: %ld", client->id ) ;
         #endif
         u4writeErr( outBuf, 1 ) ;
      #endif
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
   #if defined( S4COM_PRINT ) && defined( S4TESTING ) && defined( E4FILE_LINE )
      void S4FUNCTION code4exitExclusiveDo( CODE4 *c4, SERVER4CLIENT *client, const char *file, int line )
   #else
      void S4FUNCTION code4exitExclusive( CODE4 *c4, SERVER4CLIENT *client )
   #endif
   {
      if ( c4->currentClient != client )  /* we do not have access anyway */
         return ;
      #ifdef E4ANALYZE_BREAK_ON_CLIENT_LEFT_ERROR_STATE
         /* if releasing last instance, should be no error code (to catch incorrectly leaving in error state) */
         /* must call before mutex count is 0 or error code is ignored since no current client */
            assert5( c4->accessMutexCount != 1 || error4code( c4 ) >= 0 ) ;
      #endif

      #if defined( S4COM_PRINT ) && defined( S4TESTING )
         char outBuf[120] ;
         #ifdef E4FILE_LINE
            sprintf( outBuf, "EXIT code4exitExclusive from file: %s, line: %ld for client: %ld", file, (long)line, client->id ) ;
         #else
            sprintf( outBuf, "EXIT code4exitExclusive for client: %ld", client->id ) ;
         #endif
         u4writeErr( outBuf, 1 ) ;
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
            // AS Sept 26/02 - leave the user-id in place, this can help for user identification at later
            // points if required.
            // memset( t4->userId, 0, sizeof( t4->userId ) ) ;
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
         return error4( 0, e4parm_null, E96702 ) ;
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
         case collate4croatianCp1250:
            c4->collateName = collate4croatianCp1250 ;
            break ;
         case collate4croatianUpperCp1250:
            c4->collateName = collate4croatianUpperCp1250 ;
            break ;
         case collate4test:
         case collate4special:
            c4->collateName = collate4test ;
            break ;
         case collate4general:  // code page specific determinant...
            c4->collateName = collate4none ;
            c4->collatingSequence = sort4general ;
            break ;
         case collate4avaya1252: // LY Nov 29/04 : custom collation for Avaya (case-insensitive & accent-insensitive)
            c4->collateName = collate4avaya1252 ;
            break ;
         // AS Jun 30/08 - spanish collation support
         case collate4spanishCp1252:
            c4->collateName = collate4spanishCp1252 ;
            break ;
         case collate4spanishCp850:
            c4->collateName = collate4spanishCp850 ;
            break ;
         default:
            return error4( 0, e4parm, E96702 ) ;
      }

      c4->codeCollateSetValue = collateType ;

      return oldCollateType ;
   #else
      return error4( c4, e4notSupported, E96702 ) ;
   #endif
}



short S4FUNCTION code4collateUnicode( CODE4 *c4, short collateType )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
         return error4( 0, e4parm_null, E96702 ) ;
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
            return error4( 0, e4parm, E96702 ) ;
      }

      c4->codeCollateSetValueUnicode = collateType ;

      return oldCollateType ;
   #else
      return error4( c4, e4notSupported, E96702 ) ;
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



/* AS Sept 14/01 new functionality for validating the tables when CODE4 is initialized */
// AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   int code4validateModifyTemp( CODE4 *c4, const char *fullPathTableName, Bool5 isTempIn )
   {
      // used to modify the temp setting on an open file
      if ( c4->validationTable == 0 || c4 == 0 || fullPathTableName == 0 || fullPathTableName[0] == 0 )
         return error4( c4, e4parm, E91001 ) ;

      FIELD4 *name = d4field( c4->validationTable, "FULLNAME" ) ;
      FIELD4 *isTemp = d4field( c4->validationTable, "IS_TEMP" ) ;

      if ( name == 0 || isTemp == 0 )
         return -1 ;

      char keyToSeek[I4MAX_KEY_SIZE+1] ;
      int len = strlen( fullPathTableName ) ;
      if ( len > I4MAX_KEY_SIZE )
         len = I4MAX_KEY_SIZE ;
      memcpy( keyToSeek, fullPathTableName, len ) ;
      keyToSeek[len] = 0 ;
      c4upper(keyToSeek) ;

      d4tagSelect( c4->validationTable, d4tag( c4->validationTable, "FULLNAME" ) ) ;
      if ( d4seek( c4->validationTable, keyToSeek ) != 0 )  //  not found - an error since we were modifying only
         return error4( c4, e4result, E91001 ) ;

      // record found, so just modify the existing record
      if ( isTempIn == 1 )
         f4assignChar( isTemp, 'T' ) ;
      else
         f4assignChar( isTemp, 'F' ) ;

      // we want to ensure the changes get written to disk
      if ( d4flush( c4->validationTable ) != 0 )
         return -1 ;
      return 0 ;
   }



   int code4validateAddOpen( CODE4 *c4, const char *fullPathTableName, Bool5 isTempIn )
   {
      /*
         Called to note an open in the verification table.
      */
      if ( c4 == 0 || c4->validationTable == 0 || fullPathTableName == 0 || fullPathTableName[0] == 0 )
         return error4( c4, e4parm, E91001 ) ;

      DATA4 *valid = c4->validationTable ;
      FIELD4 *name = d4field( valid, "FULLNAME" ) ;
      FIELD4 *openCount = d4field( valid, "OPEN_COUNT" ) ;
      FIELD4 *isTemp = d4field( valid, "IS_TEMP" ) ;

      if ( name == 0 || openCount == 0 || isTemp == 0 )
         return -1 ;

      char keyToSeek[I4MAX_KEY_SIZE+1] ;
      int len = strlen( fullPathTableName ) ;
      if ( len > I4MAX_KEY_SIZE )
         len = I4MAX_KEY_SIZE ;
      memcpy( keyToSeek, fullPathTableName, len ) ;
      keyToSeek[len] = 0 ;
      c4upper(keyToSeek) ;

      int oldTagErr = c4->errTagName ;
      c4->errTagName = 0 ;
      TAG4 *tag = d4tag( valid, "FULLNAME" ) ;
      c4->errTagName = oldTagErr ;
      if ( tag == 0 )  // just try creating again
      {
         static TAG4INFO tags[] =
         {
            { "FULLNAME", "UPPER(FULLNAME)", ".NOT.DELETED()", 0, 0 },
            { 0,  0,  0, 0, 0 },
         } ;
         int oldSafety = c4->safety ;
         c4->safety = 0 ;
         INDEX4 *index = i4create( valid, 0, tags ) ;
         c4->safety = oldSafety ;
         if ( index == 0 )  // couldn't create index, this is a failure
            return -1 ;
         if ( d4tag( valid, "FULLNAME" ) == 0 )
            return -1 ;
      }

      #ifdef S4SERVER
         SERVER4CLIENT *curClient = c4->currentClient ;
         c4->currentClient = c4->catalogClient ;
      #endif

      d4tagSelect( valid, tag ) ;
      if ( d4seek( valid, keyToSeek ) == 0 )
      {
         // record found, so just modify the existing record
         f4assignInt( openCount, f4int( openCount ) + 1 ) ;
         if ( isTempIn == 1 )
            f4assignChar( isTemp, 'T' ) ;
         else
            f4assignChar( isTemp, 'F' ) ;
      }
      else
      {
         // record not found, so add a new one

         if ( d4appendStart( valid, 0 ) != 0 )
         {
            #ifdef S4SERVER
               c4->currentClient = curClient ;
            #endif
            return -1 ;
         }

         f4assign( name, fullPathTableName ) ;
         f4assignInt( openCount, 1 ) ;
         if ( isTempIn == 1 )
            f4assignChar( isTemp, 'T' ) ;
         else
            f4assignChar( isTemp, 'F' ) ;

         if ( d4append( valid ) != 0 )
         {
            #ifdef S4SERVER
               c4->currentClient = curClient ;
            #endif
            return -1 ;
         }
      }

      // we want to ensure the changes get written to disk
      int rc = d4flush( valid ) ;
      #ifndef S4OFF_MULTI
         // AS Apr 15/03 - support for new lockId for shared clone locking
         if ( rc == 0 )
            d4unlockLow( valid, data4lockId( valid ), 0 ) ;
      #endif

      #ifdef S4SERVER
         c4->currentClient = curClient ;
      #endif
      return rc ;
   }



   int code4validateAddClose( CODE4 *c4, const char *fullPathTableName, Bool5 isTempIn )
   {
      /*
         Called to note a close in the verification table.
         For temporary tables the record is deleted
         The isTemp is passed in as a check -- it should == the value in AddOpen, although a change
         to isTemp being true is supported (eg. if a table has been marked for removal)
      */
      if ( c4->validationTable == 0 || c4 == 0 || fullPathTableName == 0 || fullPathTableName[0] == 0 )
         return error4( c4, e4parm, E91001 ) ;

      DATA4 *valid = c4->validationTable ;

      FIELD4 *name = d4field( valid, "FULLNAME" ) ;
      FIELD4 *openCount = d4field( valid, "OPEN_COUNT" ) ;
      FIELD4 *isTemp = d4field( valid, "IS_TEMP" ) ;

      if ( name == 0 || openCount == 0 || isTemp == 0 )
         return -1 ;

      char keyToSeek[I4MAX_KEY_SIZE+1] ;
      int len = strlen( fullPathTableName ) ;
      if ( len > I4MAX_KEY_SIZE )
         len = I4MAX_KEY_SIZE ;
      memcpy( keyToSeek, fullPathTableName, len ) ;
      keyToSeek[len] = 0 ;
      c4upper(keyToSeek) ;

      #ifdef S4SERVER
         SERVER4CLIENT *curClient = c4->currentClient ;
         c4->currentClient = c4->catalogClient ;
      #endif

      d4tagSelect( valid, d4tag( valid, "FULLNAME" ) ) ;
      if ( d4seek( valid, keyToSeek ) == 0 )
      {
         // record found, so just modify the existing record
         #ifdef E4ANALYZE
            if ( f4int( openCount ) < 1 )
               return error4( c4, e4result, E91001 ) ;
         #endif
         f4assignInt( openCount, f4int( openCount ) - 1 ) ;
         if ( f4int( openCount ) == 0 )
         {
            if ( isTempIn == 1 )
            {
               f4assignChar( isTemp, 'T' ) ;
               d4delete( valid ) ;
            }
            else
            {
               #ifdef E4ANALYZE
                  // A table should not go from being temp to non-temp, otherwise we will have a critical
                  // problem that a permanant table may incorrectly get deleted on startup.
                  if ( f4true( isTemp ) )
                  {
                     #ifdef S4SERVER
                        c4->currentClient = curClient ;
                     #endif
                     return error4( c4, e4struct, E91001 ) ;
                  }
               #endif

               f4assignChar( isTemp, 'F' ) ;
            }
         }
      }

      // we want to ensure the changes get written to disk
      int rc = d4flush( valid ) ;
      #ifndef S4OFF_MULTI
         // AS Apr 15/03 - support for new lockId for shared clone locking
         if ( rc == 0 )
            d4unlockLow( valid, data4lockId( valid ), 0 ) ;
      #endif

      #ifdef S4SERVER
         c4->currentClient = curClient ;
      #endif
      return rc ;
   }



   int S4FUNCTION code4validate( CODE4 *c4, const char *validateTableName, Bool5 deleteTemp )
   {
      /*
         When the 'validate' option is on, code4validate() is used to check
         the validation table to ensure that all tables are in a valid state.
         If any tables are not in a valid state, they are reindexed.
         code4validate() will open the validation table if is not already open,
         and will create it if it doesn't yet exist.
         Because the validation table is included in the table, it gets inserted as
         the first record, so it will be reindexed first if there was a problem.

         If deleteTemp is true, files that are recorded as temporary but still open
         are deleted.  If deleteTemp is false, the validation table deletes only
         the record pertaining to the temporary file, not the data file itself.  This
         can be used as a safety measure to ensure that tables do not get removed
         incorrectly.

         Returns - 0 if no problems, < 0 if an error, > 0 == # of tables that were
                   left open and have been repaired (excluding the validation table)

         This option does not work with a shared validation table.  This means you can use
         it in stand/alone but it will not share well in multi-user.

      */

      int numRepaired = 0 ;

      if ( c4->validationTable == 0 )
      {
         // don't log this file

         if ( validateTableName == 0 )
            validateTableName = VALIDATE4NAME ;
         int oldErrOpen = c4->errOpen ;
         int oldAccessMode = c4->accessMode ;
         c4->accessMode = OPEN4DENY_RW ;
         #ifdef S4SERVER
            int oldSingleClient = c4->singleClient ;
            c4->singleClient = 1 ;
         #endif
         c4->errOpen = 0 ;
         #ifndef S4OFF_TRAN
            int oldLogOpen = c4->logOpen ;
            c4->logOpen = 0 ;
         #endif
         int oldLogVal = c4->log ;
         c4->log = LOG4OFF ;
         c4->validationTable = d4open( c4, validateTableName ) ;
         c4->log = oldLogVal ;
         #ifdef S4SERVER
            c4->singleClient = oldSingleClient ;
         #endif
         c4->accessMode = oldAccessMode ;
         #ifndef S4OFF_TRAN
            c4->logOpen = oldLogOpen ;
         #endif
         c4->errOpen = oldErrOpen ;

         if ( c4->validationTable == 0 )
         {
            static FIELD4INFO fields[] =
            {
              { "FULLNAME",   'C', I4MAX_KEY_SIZE_COMPATIBLE, 0 },  // The name is stored full path
              { "OPEN_COUNT", 'N', 8, 0 },    // The # of opens on the table - maximum of 99999999, arbitrarily chosen
              { "IS_TEMP" ,   'L', 1, 0},      // Set to true if the table is a temporary table - gets deleted on start up
              {0,0,0,0},
            } ;
            static TAG4INFO tags[] =
            {
               { "FULLNAME", "UPPER(FULLNAME)", ".NOT.DELETED()", r4unique, 0 },
               { 0,  0,  0, 0, 0 },
            } ;

            #ifndef S4OFF_TRAN
               c4->logOpen = 0 ;
            #endif
            int oldSafety = c4->safety ;
            c4->safety = 1 ;    // we don't want to overwrite the file if it is in use.
            c4->accessMode = OPEN4DENY_RW ;
            int oldLogVal = c4->log ;
            c4->log = LOG4OFF ;
            c4->validationTable = d4create( c4, validateTableName, fields, tags ) ;
            c4->log = oldLogVal ;
            c4->accessMode = oldAccessMode ;
            c4->safety = oldSafety ;
            #ifndef S4OFF_TRAN
               c4->logOpen = oldLogOpen ;
            #endif
            if ( c4->validationTable == 0 )
               return -1 ;

            int rc = code4validateAddOpen( c4, d4fileName( c4->validationTable ), 0 ) ;
            if ( rc != 0 )
            {
               d4close( c4->validationTable ) ;
               c4->validationTable = 0 ;
               return rc ;
            }
            // need to reset the field to '0' so that it passes validation.
            FIELD4 *openCount = d4field( c4->validationTable, "OPEN_COUNT" ) ;
            if ( openCount == 0 )
               return -1 ;
            f4assignInt( openCount, 0 ) ;  // mark as having been reindexed and being open
         }
      }


      FIELD4 *name = d4field( c4->validationTable, "FULLNAME" ) ;
      FIELD4 *openCount = d4field( c4->validationTable, "OPEN_COUNT" ) ;
      FIELD4 *isTemp = d4field( c4->validationTable, "IS_TEMP" ) ;

      if ( name == 0 || openCount == 0 || isTemp == 0 )
         return -1 ;

      // disable the validation table for the rest of this function in order that we don't
      // add entries to ourselves while we are trying to fix ourselves up.
      DATA4 *valid = c4->validationTable ;
      c4->validationTable = 0 ;

      // now scan through the file and take the appropriate actions
      int rc ;
      d4tagSelect( valid, 0 ) ;  // ensure we use record ordering to do validation table first
      for ( rc = d4top( valid ) ; rc == 0 ; rc = d4skip( valid, 1 ) )
      {
         if ( d4deletedInternal( valid ) == 0 )  // the record is not deleted, so it is valid and should be checked
         {
            if ( f4true( isTemp ) )
            {
               if ( deleteTemp )  // if we are to delete these files...
               {
                  // the table was marked as temporary, so it should have been deleted.  Delete it now.
                  DATA4 *tempTable = d4open( c4, f4str( name ) ) ;
                  // it tempTable == 0 the table doesn't exist, so we can just continue on, else must delete it
                  if ( tempTable == 0 )
                  {
                     error4set( c4, 0 ) ;
                  }
                  else
                  {
                     #ifdef S4OLEDEBUG_PRINT
                        log5( "Deleting temporary table from code4validate(): " ) ;
                        log5( f4str( name ) ) ;
                        log5( "\n" ) ;
                     #endif
                     d4remove( tempTable ) ;
                  }
               }
               numRepaired++ ;
               d4delete( valid ) ;   // mark the row as not being valid anymore
            }
            else if ( f4int( openCount ) != 0 )
            {
               // the table was left in an open state, so we must open and reindex if possible.
               if ( valid->recNum == 1 )
               {
                  // means the validation table
                  f4assignInt( openCount, 0 ) ;  // mark as having been reindexed and being open
                  d4reindex( valid ) ;
                  #ifndef S4OFF_MULTI
                     // AS Apr 15/03 - support for new lockId for shared clone locking
                     d4unlockLow( valid, data4lockId( valid ), 0 ) ;
                  #endif
                  d4top( valid ) ;
               }
               else
               {
                  DATA4 *tempTable = d4open( c4, f4str( name ) ) ;

                  // if we cannot open the table, just remove it from the table -- possibly was
                  // deleted external to CODE4 being restarted
                  if ( tempTable == 0 )
                  {
                     error4set( c4, 0 ) ;
                     #ifdef S4OLEDEBUG_PRINT
                        log5( "Deleting table entry (could not open) from code4validate(): " ) ;
                        log5( f4str( name ) ) ;
                        log5( "\n" ) ;
                     #endif
                     d4delete( valid ) ;   // mark the row as not being valid anymore
                  }
                  else
                  {
                     #ifdef S4OLEDEBUG_PRINT
                        log5( "Reindexing table from code4validate(): " ) ;
                        log5( f4str( name ) ) ;
                        log5( "\n" ) ;
                     #endif
                     // was able to open table, so reindex it.
                     d4reindex( tempTable ) ;

                     // if an error occurred in reindexing, just ignore it
                     error4set( c4, 0 ) ;

                     f4assignInt( openCount, 0 ) ;  // mark as having been reindexed
                     #ifdef S4SERVER
                        int oldKeepOpen = c4->server->keepOpen ;
                        c4->server->keepOpen = 0 ;
                     #endif
                     d4close( tempTable ) ;
                     #ifdef S4SERVER
                        c4->server->keepOpen = oldKeepOpen ;
                     #endif
                     numRepaired++ ;
                  }
               }
            }
         }
      }

      c4->validationTable = valid ;

      rc = d4top( valid ) ;
      if ( rc != 0 )
         return rc ;

      #ifdef E4ANALYZE
         /* LY July 9/03 : added (char *) to second param (compiler error on Linux) */
         if ( strnicmp( f4str( name ), (char *)valid->dataFile->file.name, strlen(valid->dataFile->file.name) ) != 0 )
            return error4( c4, e4result, E91001 ) ;
      #endif
      f4assignInt( openCount, 1 ) ;
      // AS Oct 23/02 - do a final packing of the table in server or if opened exclusive...
      #if defined( S4SERVER ) || defined( S4OFF_MULTI )
         if ( d4pack( c4->validationTable ) != 0 )
            return -1 ;
      #endif
      if ( d4flush( c4->validationTable ) != 0 )
         return -1 ;

      return numRepaired ;
   }



   int code4validateUndo( CODE4 *c4 )
   {
      // This function is called on code4initUndo to uninitialize the validation table
      DATA4 *valid = c4->validationTable ;
      if ( valid != 0 )
      {
         #ifdef E4ANALYZE
            // Verify that all the tables are marked as closed, otherwise it inidacates we missed a close somewhere.
            int rc ;

            FIELD4 *openCount = d4field( valid, "OPEN_COUNT" ) ;
            FIELD4 *isTemp = d4field( valid, "IS_TEMP" ) ;
            if ( openCount == 0 || isTemp == 0 )
               return error4( c4, e4result, E91001 ) ;

            d4tagSelect( valid, 0 ) ;  // ensure we use record ordering to do validation table first
            for ( rc = d4top( valid ) ; rc == 0 ; rc = d4skip( valid, 1 ) )
            {
               if ( d4recNo( valid ) == 1 )  // we should have 1 copy open
               {
                  #ifdef E4ANALYZE
                     if ( f4int( openCount ) != 1 )
                        return error4( c4, e4result, E91001 ) ;
                  #endif
                  f4assignInt( openCount, 0 ) ;  // reset it to 0.
               }
               #ifdef E4ANALYZE
                  else
                  {
                     // the tables should all be closed or deleted
                     if ( !d4deletedInternal( valid ) )
                     {
                        if ( f4int( openCount ) != 0 )
                           return error4describe( c4, e4result, E91001, "on valdation close open count not zero for table: ", f4str(d4field(valid,"FULLNAME")), 0 ) ;
                        if ( f4true( isTemp ) && !d4deletedInternal( valid ) )  // temp tables should be marked as deleted
                           return error4( c4, e4result, E91001 ) ;
                     }
                  }
               #endif
            }
         #endif
         // reset the validationTable pointer prior to closing to avoid using a closed table!
         c4->validationTable = 0 ;
         d4close( valid ) ;
      }

      return 0 ;
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */


// AS July 2/02 - modified function, checks to see if server is alive...
int S4FUNCTION code4ping(CODE4 *c4 )
{
   #ifdef S4CLIENT
      CONNECT4 *connect = &c4->clientConnect ;

      connect4sendShort( connect, STREAM4PING ) ;
      connect4sendLong(connect, 0 ) ;  // don't actually send anything
      long toSend = 4 ;

      connect4sendLong( connect, toSend ) ;  // get 4 bytes back
      connect4sendFlush( connect ) ;
      char data[4] ;
      return connect4receive(connect, &data, 4, code4timeoutVal(c4) ) ;
   #else
      return 0 ;
   #endif
}



int S4FUNCTION code4swapLogFile( CODE4 *c4, char *logName, int logNameLen )
{
   #ifdef S4CLIENT
      // used by backup/auto-recovery to swap (on the fly) the backup log file being used by the server.
      // this allows for the backup program on the backup computer to update its copies of the backup files
      // so that if it needs to be started less recovery work needs to be done.
      // inputs are 'logName, a character array to place the swapped out log file (i.e. the old one which should
      // be used for backup purposes).  len is the length of the name, and must be at least LEN4PATH

      assert5( logNameLen >= LEN4PATH ) ;  // ensure there is room for the name
      CONNECT4 *connect = &c4->clientConnect ;

      connect4sendShort( connect, STREAM4SWAP_LOGFILE ) ;
      connect4sendFlush( connect ) ;

      short rc = connect4receiveShort( connect ) ;
      if ( rc != 0 && rc != e4len )  // on 'e4len' we give back the name of the log file that caused the problem.
      {
         // handle timeout case
         if ( c4->errorCode == r4timeout )
            return r4timeout ;
         // AS June 01/01 a STREAM4DISCONNECT may come down the line if the server is shut down
         if ( rc == STREAM4DISCONNECT )
         {
            c4->defaultServer.connectionFailure = e4connection ;
            return error4( c4, e4connection, E90160 ) ;
         }
         return rc ;
      }

      short len = connect4receiveShort( connect ) ;
      assert5( len < LEN4PATH ) ;  // should not be larger..., need 1 byte to null-terminate
      connect4receive( connect, logName, len, code4timeoutVal( c4 ) ) ;
      logName[len] = 0 ;

      return rc ;
   #else
      return e4notSupported;
   #endif
}


// AS Sept. 3/02 - new function to support linkage with Server-side functionality
int S4FUNCTION code4additionalFunction( CODE4 *c4, long functionNumber, void *infoIn, long infoLenIn, void *infoOut, long *infoLenOut )
{
   #ifdef S4CLIENT
      assert5port( "this function should work on any platform (from the client side) - allows invoking of a function in a user dll on the server" ) ;
      // infoLenOut input is the maximum length of the infoOut pointer
      // returns e4loadLib if the dll could not be loaded by the server
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return e4parmNull ;
      #endif

      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      CONNECT4 *connect = c4getClientConnect( c4 ) ;
      connect4sendShort( connect, MSG5ADD_FUNC ) ;
      connect4sendLong( connect, functionNumber ) ;
      connect4sendLong( connect, infoLenIn ) ;
      connect4send( connect, infoIn, infoLenIn ) ;
      connect4sendFlush( connect ) ;
      short cbStatus = connect4receiveShort( connect ) ;  // the status CodeBase sends back, if any
      if ( cbStatus < 0 )
         return cbStatus ;

      long lenOut = connect4receiveLong( connect ) ;
      if ( lenOut != 0 )
      {
         long lenMax = *infoLenOut ;  // maximum size to copy over, discard rest.
         long lenReceive = min( lenOut, lenMax ) ;
         int rc = connect4receive( connect, infoOut, lenReceive, code4timeoutVal(c4) ) ;
         if ( rc != 0 )
            return rc ;

         *infoLenOut = lenReceive ;

         if ( lenReceive < lenOut ) // means there is extra info on the socket - just read it off
         {
            // not too efficient, but in general this code only is run if the client application makes
            // a bad decision about how much space to allocate).
            for ( lenReceive = lenOut - lenReceive ; lenReceive > 0 ; lenReceive-- )
            {
               char dummy ;
               rc = connect4receive( connect, &dummy, 1, code4timeoutVal(c4) ) ;
               if ( rc != 0 )
                  return rc ;
            }
         }
      }
      else
         *infoLenOut = 0 ;

      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;

      return 0 ;
   #else
      return e4notSupported ;
   #endif
}



int S4FUNCTION code4additionalFunctionOdbc( CODE4 *c4, long functionNumber, void *infoIn, long infoLenIn, void **infoOut, long *infoLenOut )
{
   // LY Jul 16/04 : added S4MACINTOSH
   #if defined( S4CLIENT ) || defined( S4WINCE ) || defined( S4MACINTOSH ) /* LY Dec 9/03 : added S4WINCE */
      return e4notSupported ;
   #else
      // AS Apr 4/01 - Made available in stand/alone for odbc stored procedures
      if ( c4->addDll == 0 )  // means not loaded yet...
      {
         const char *dllName = "CBADD.DLL" ;
         c4->addDll = LoadLibrary( dllName ) ;
         if ( c4->addDll == 0 )
            return e4notSupported ;
         int rc = c4getFuncPtr( c4->addDll, "AdditionalFunctions", (void **)&c4->addFunction ) ;
         if ( c4->addFunction == 0 || rc != 0 )  // function not found
         {
            DWORD err = GetLastError() ;
            FreeLibrary( c4->addDll ) ;
            c4->addDll = 0 ;
            return e4notSupported ;
         }
      }
      return c4->addFunction( functionNumber, infoIn, infoLenIn, infoOut, infoLenOut ) ;
   #endif
}



#ifndef S4STAND_ALONE
   #ifdef S4CLIENT
      static void code4logAppendNewLine( FILE4 *appendLog )
      {
         FILE4LONG len ;

         len = file4lenLow( appendLog ) ;
         file4writeInternal( appendLog, len, "\r\n", 2 ) ;
      }



      static void code4logAppend( CODE4 *c4, char *info )
      {
         // assumes file is open
         FILE4 *appendLog = &c4->connLogFile ;
         FILE4LONG pos ;

         char buffer[9];
         char dateStr[32];
         date4today( buffer ) ;
         date4format( buffer, dateStr, "CCYY/MM/DD  " ) ;
         date4timeNow( buffer ) ;
         c4strncat( dateStr, sizeof( dateStr ), buffer, 8 ) ;  // CS 2007/05/24 compile fix
         pos = file4lenLow( appendLog ) ;
         file4writeInternal( appendLog, pos, dateStr, c4strlen( dateStr ) ) ;
         code4logAppendNewLine( appendLog ) ;
         if ( info != 0 )
         {
            pos = file4lenLow( appendLog ) ;
            file4writeInternal( appendLog, pos, info, c4strlen( info ) ) ;
            code4logAppendNewLine( appendLog ) ;
         }
      }


      int S4FUNCTION code4logConn( CODE4 *c4, short level, const char *name )
      {
         if ( level <= 0 || level > 9 )
         {
            // AS Sept. 26/02 - log connection info
            if ( c4->connLogFileOpen == 1 )
            {
               file4close( &c4->connLogFile ) ;
               c4->connLogFileOpen = 0 ;
            }
            c4->logConn = 0 ;
            return 0 ;
         }

         if ( name == 0 )
            name = "CONNINFO.LOG" ;

         int oldErrOpen = c4->errOpen ;
         c4->errOpen = 0 ;
         int rc = file4open( &c4->connLogFile, c4, name, 0 ) ;
         c4->errOpen = oldErrOpen ;
         if ( rc != 0 )  // if can't open, create
         {
            int oldErrCreate = c4->errCreate ;
            c4->errCreate = 0 ;
            rc = file4create( &c4->connLogFile, c4, name, 0 ) ;
            c4->errCreate = oldErrCreate ;
            if ( rc != 0 )  // if can't create, ignore
               return e4create ;
         }
         c4->connLogFileOpen = 1 ;
         c4->logConn = level ;
         return 0 ;
      }
   #endif
#endif /* S4STAND_ALONE */



// AS Sept. 26/02 - log connection info
// AS Oct 5/04 - support for export for service
// AS Oct 18/04 - export for stand-alone...
void S4FUNCTION code4logInfoDo( CODE4 *c4, char *info )
{
   #ifndef S4STAND_ALONE
      if ( c4 == 0 )  // cannot log in this case (at least not currently)
         return ;

      #ifdef S4SERVER
         // log to the server error log file
         error4logAppend( c4, 0, 0, info, 0, 0 ) ;
      #else
         if ( c4->connLogFileOpen == 0 ) // if not open, don't log
            return ;

         // log to special log
         code4logAppend( c4, info ) ;
      #endif
   #endif /* S4STAND_ALONE */
}


// AS Jan 23/03 - Support for run-time loading of encryption
int S4FUNCTION code4encryptFile( CODE4 *c4, short flag )
{
   #ifdef S4ENCRYPT_HOOK
      // AS Oct 2/06 for odbc server part, it will load just like the standard stand/alone file encryption
      #if defined( S4SERVER ) && !defined( S4ODBC_BUILD )
         assert5( c4->encrypt != 0 ) ;
      #else
         #ifdef S4ENCRYPT_DLL // LY Jul 16/04
            if ( c4->encrypt == 0 )
         #else
            if ( !c4->encryptInit )
         #endif
         {
            // need to perform basic initialization - initialize with default key...
            int rc = code4encryptInit( c4, 0, -1 ) ;  // -1 means attempt to use 256 bit, use 8 if cannot
            // int rc = c4encryptLoad( c4 ) ;
            // AS Apr 22/03 - do check rc from init
            if ( rc != 0 )
               return rc ;

            #ifndef S4ENCRYPT_DLL   // LY Jul 16/04
               c4->encryptInit = 1 ;
            #endif
         }
      #endif
      #ifdef S4ENCRYPT_DLL // LY Jul 16/04
         assert5( c4->encrypt->encryptFileSet != 0 ) ;

         return c4->encrypt->encryptFileSet( c4, flag ) ;
      #else
         return code4encryptFileSet( c4, flag ) ;
      #endif
   #else
      return e4notSupported ;
   #endif
}



// AS Mar 31/04 - required for odbc build of server components
#if !defined( S4SERVER ) || defined( S4ODBC_BUILD )
   static int c4encryptLoad( CODE4 *c4, short numBits ) ;

   // AS May 20/03 - moved to always define
   // AS Jan 23/03 - Support for run-time loading of encryption
   int S4FUNCTION code4encryptInit( CODE4 *c4, const void *key, short keyLen )
   {
      #if defined( S4ENCRYPT_HOOK ) || defined( S4ENCRYPT_COM )
         #ifdef S4ENCRYPT_DLL // LY Jul 16/04
            if ( c4->encrypt != 0 )  // already initialized, this is an error.
         #else
            if ( c4->encryptInit )
         #endif
            return r4active ;

         #ifdef S4ENCRYPT_DLL // LY Jul 16/04
            int rc = c4encryptLoad( c4, keyLen * 8 ) ;
            if ( rc != 0 )
               return rc ;
         #endif
         const void *keyToUse = key ;
         if ( keyLen == -1 )  // meant use default, so use default len as well
         {
            // AS Oct 3/06 - for odbc server we need to use the setting in the server configuration file
            #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
               if ( c4->encryptKeyLen != 0 )
               {
                  keyToUse = c4->encryptKey ;
                  keyLen = c4->encryptKeyLen ;
               }
            #else
               keyLen = 0 ;
            #endif
         }
         #ifdef S4ENCRYPT_DLL // LY Jul 16/04
            assert5( c4->encrypt->encryptInit != 0 ) ;
            assert5( c4->encrypt->preprocess == 0 ) ;  // not assigned yet
            assert5( c4->encrypt->preprocessInit == 0 ) ;  // not assigned yet
            return c4->encrypt->encryptInit( c4, keyToUse, keyLen, &c4->encrypt->blockSize, &(c4->encrypt->preprocess), &(c4->encrypt->preprocessInit) ) ;
         #else
            int rc = code4encryptInitLow( c4, keyToUse, keyLen, &c4->encryptBlockSize, &(c4->encryptPreprocess), &(c4->encryptPreprocessInit) ) ;
            if ( rc == r4success )
               c4->encryptInit = 1 ;

            return rc ;
         #endif
      #else
         return e4notSupported ;
      #endif
   }
#endif



#ifdef S4ENCRYPT_DLL
   #if defined( S4ENCRYPT_HOOK ) || defined( S4ENCRYPT_COM )
      static void c4getFuncPtr( HINSTANCE hInst, const char * funcName, void **function, long *rc )
      {
         // gets the pointer and sets rc to -1 if a failure
         #ifdef S4WINCE // LY Dec 9/03
            WSTR5 uFuncName[512] ;
            memset( uFuncName, 0, 512 * sizeof( unsigned short ) ) ;
            c4atou( funcName, uFuncName, strlen( funcName ) ) ;
            *function = (void *)GetProcAddress( hInst, uFuncName ) ;
         #else
            *function = (void *)GetProcAddress( hInst, funcName ) ;
         #endif
         if ( *function == 0 )
            *rc = -1 ;
      }



      static void c4encryptUnload( CODE4 *c4 )
      {
         if ( c4->encrypt != 0 )
         {
            FreeLibrary( c4->encrypt->encryptDll ) ;
            c4->encrypt->encryptDll = 0 ;
            u4free( c4->encrypt ) ;
            c4->encrypt = 0 ;
         }
      }



      static int c4encryptLoad( CODE4 *c4, short numBits )
      {
         // numBits implies the encryption to use, set to -8 to indicate try 256 and if that fails try 8 bit
         switch( numBits )
         {
            case 8:
            case 128:
            case 192:
            case 256:
            case -8:
               break ;
            default:
               // AS Oct 2/06 - This isn't a critical error in odbc, it means the client passed in a bad value
               #ifdef S4ODBC_BUILD
                  return e4parm ;
               #else
                  return error4describe( c4, e4parm, E96702, "encryption num bits set to invalid value (must be 8, 128, 192 or 256)", 0, 0 ) ;
               #endif
         }

         if ( c4->encrypt == 0 )
         {
            c4->encrypt = (ENCRYPT4DLL *)u4allocFree( c4, sizeof( ENCRYPT4DLL ) ) ;
            if ( c4->encrypt == 0 )
               return e4memory ;
            ENCRYPT4DLL *encrypt = c4->encrypt ;
            // first try loading 256 bit encryption

            char *encryptDllName ;

            if ( numBits == 8 )
               encryptDllName = ENC4DLL8BIT_NAME ;
            else
               encryptDllName = ENC4DLL256BIT_NAME ;

            #ifdef S4WINCE  /* LY Dec 9/03 */
               WSTR5 uDllName[512] ;
               memset( uDllName, 0, 512 * sizeof(unsigned short) ) ;
               c4atou( encryptDllName, uDllName, strlen( encryptDllName ) ) ;
               encrypt->encryptDll = LoadLibrary( uDllName ) ;
            #else
               encrypt->encryptDll = LoadLibrary( encryptDllName ) ;
            #endif

            if ( encrypt->encryptDll == 0 )  // if here, we must be able to encrypt or it is an error
            {
               if ( numBits == -8 )
               {
                  encryptDllName = ENC4DLL8BIT_NAME ;
                  #ifdef S4WINCE  /* LY Dec 9/03 */
                     memset( uDllName, 0, 512 * sizeof(unsigned short) ) ;
                     c4atou( encryptDllName, uDllName, strlen( encryptDllName ) ) ;
                     encrypt->encryptDll = LoadLibrary( uDllName ) ;
                  #else
                     encrypt->encryptDll = LoadLibrary( encryptDllName ) ;
                  #endif
                  numBits = 8 ;
               }

               if ( encrypt->encryptDll == 0 )  // if here, we must be able to encrypt or it is an error
               {
                  u4free( c4->encrypt ) ;
                  c4->encrypt = 0 ;
                  // AS Dec 3/03 - we want the ability to set off the encryption error, in particular when we attempt to load it internally
                  if ( c4->errEncrypt )
                     return error4describe( c4, e4notSupported, E91001, "Unable to load encryption dll", encryptDllName, 0 ) ;
                  else
                     return e4notSupported ;
               }
            }
            else if ( numBits == -8 )  // we loaded 256 bit dll, so set level to 256 bits
               numBits = 256 ;

            long rc = 0 ;
            // AS Oct 2/06 - support for odbc components with file encryption
            #if !defined( S4SERVER ) || defined( S4ODBC_BUILD )
               c4getFuncPtr( encrypt->encryptDll, "code4encryptInitLow", (void **)&(encrypt->encryptInit), &rc ) ;
               if ( rc != 0 || encrypt->encryptInit == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptInitLow", encryptDllName ) ;
               }
            #else
               // AS Oct 2/06 - support for odbc components with file encryption
               //#if !defined( S4SERVER ) && !defined( S4ODBC_BUILD )
               c4getFuncPtr( encrypt->encryptDll, "server4initPreprocess", (void **)&(encrypt->encryptInit), &rc ) ;
               if ( rc != 0 || encrypt->encryptInit == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "server4initPreprocess", encryptDllName ) ;
               }
            #endif
            #ifdef S4ENCRYPT_HOOK
               c4getFuncPtr( encrypt->encryptDll, "code4encryptFileSet", (void **)&(encrypt->encryptFileSet), &rc ) ;
               if ( rc != 0 || encrypt->encryptFileSet == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptFileSet", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "code4encryptFileGetLow", (void **)&(encrypt->encryptFileGet), &rc ) ;
               if ( rc != 0 || encrypt->encryptFileGet == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptFileGetLow", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "code4encryptFileInitUndoLow", (void **)&(encrypt->encryptFileInitUndo), &rc ) ;
               if ( rc != 0 || encrypt->encryptFileInitUndo == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptFileInitUndoLow", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "code4setEncryptFileLow", (void **)&(encrypt->setEncryptFile), &rc ) ;
               if ( rc != 0 || encrypt->setEncryptFile == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4setEncryptFileLow", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "encrypt4encryptFileHook", (void **)&(encrypt->encryptFileHook), &rc ) ;
               if ( rc != 0 || encrypt->encryptFileHook == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "encrypt4encryptFileHook", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "encrypt4decryptHook", (void **)&(encrypt->decryptHook), &rc ) ;
               if ( rc != 0 || encrypt->decryptHook == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "encrypt4decryptHook", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "file4preprocess", (void **)&(encrypt->filePreprocess), &rc ) ;
               if ( rc != 0 || encrypt->filePreprocess == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "file4preprocess", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "file4preprocessDone", (void **)&(encrypt->filePreprocessDone), &rc ) ;
               if ( rc != 0 || encrypt->filePreprocessDone == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "file4preprocessDone", encryptDllName ) ;
               }
            #endif
            #ifdef S4ENCRYPT_COM
               c4getFuncPtr( encrypt->encryptDll, "code4encryptCodeInitUndoLow", (void **)&(encrypt->encryptCodeInitUndo), &rc ) ;
               if ( rc != 0 || encrypt->encryptCodeInitUndo == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptCodeInitUndoLow", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "encrypt4comInitUndo", (void **)&(encrypt->encryptComInitUndo), &rc ) ;
               if ( rc != 0 || encrypt->encryptComInitUndo == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptComInitUndoLow", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "encrypt4comInit", (void **)&(encrypt->encryptComInit), &rc ) ;
               if ( rc != 0 || encrypt->encryptComInit == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptComInitLow", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "connect4lowDecrypt", (void **)&(encrypt->conLowDecrypt), &rc ) ;
               if ( rc != 0 || encrypt->conLowDecrypt == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "connect4lowDecrypt", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "connect4lowEncrypt", (void **)&(encrypt->conLowEncrypt), &rc ) ;
               if ( rc != 0 || encrypt->conLowEncrypt == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "connect4lowEncrypt", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "connect4lowGetEncryptRead", (void **)&(encrypt->conLowGetEncryptRead), &rc ) ;
               if ( rc != 0 || encrypt->conLowGetEncryptRead == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "connect4lowGetEncryptRead", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "connect4lowSetEncryptRead", (void **)&(encrypt->conLowSetEncryptRead), &rc ) ;
               if ( rc != 0 || encrypt->conLowSetEncryptRead == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "connect4lowSetEncryptRead", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "connect4lowSetEncryptWrite", (void **)&(encrypt->conLowSetEncryptWrite), &rc ) ;
               if ( rc != 0 || encrypt->conLowSetEncryptWrite == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "connect4lowSetEncryptWrite", encryptDllName ) ;
               }
               c4getFuncPtr( encrypt->encryptDll, "connect4lowGetEncryptWrite", (void **)&(encrypt->conLowGetEncryptWrite), &rc ) ;
               if ( rc != 0 || encrypt->conLowGetEncryptWrite == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "connect4lowGetEncryptWrite", encryptDllName ) ;
               }
               #ifdef S4CLIENT
                  c4getFuncPtr( encrypt->encryptDll, "code4encryptConnectionLow", (void **)&(encrypt->encryptConnection), &rc ) ;
                  if ( rc != 0 || encrypt->encryptConnection == 0 )
                  {
                     c4encryptUnload( c4 ) ;
                     return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4encryptConnectionLow", encryptDllName ) ;
                  }
               #endif
            #endif
            #ifdef S4CLIENT
               c4getFuncPtr( encrypt->encryptDll, "code4serverPreprocessInitLow", (void **)&(encrypt->serverEncryptInit), &rc ) ;
               if ( rc != 0 || encrypt->serverEncryptInit == 0 )
               {
                  c4encryptUnload( c4 ) ;
                  return error4describe( c4, e4notSupported, E91001, "Missing function in encryption dll", "code4serverPrepocessInitLow", encryptDllName ) ;
               }
            #endif
         }

         return 0 ;
      }
   #endif

   // AS Oct 2/06 - this is not used for the ODBC components...they do file encryption directly called from within the API (the client passes the key in)
   // perhaps this is worth revisiting...
   #if defined( S4SERVER ) && !defined( S4ODBC_BUILD )
      #if defined( S4ENCRYPT_HOOK ) || defined( S4ENCRYPT_COM )
         int server4initEncrypt( SERVER4 *server, DATA4 *config )
         {
            CODE4 *c4 = server->c4 ;
            if ( c4->encrypt != 0 )  // already initialized, this is an error.
               return r4active ;
            // get the bit level to determine which to load
            FIELD4 *field = d4field( config, "ENCRYPTLEV" ) ;
            if ( field == 0 )  // this is ok, just assume it's not available...
            {
               // return error4describe( c4, e4config, E70239, "ENCRYPTLEV", 0, 0 ) ;
               // AS Jun 7/06 - if missing, just assume no encryption...
               // if this is missing
               error4set( c4, 0 ) ;
               server->numBits = 0 ;
            }
            else
               server->numBits = f4long( field ) ;
            // AS Sep 8/03 - if server->numBits == 0 , means no encryption
            if ( server->numBits == 0 )
               return 0 ;
            int rc = c4encryptLoad( c4, (short)server->numBits ) ;
            if ( rc != 0 )
               return rc ;
            assert5( c4->encrypt->encryptInit != 0 ) ;
            // assert5( c4->encrypt->preprocess == 0 ) ;  // not assigned yet
            // assert5( c4->encrypt->preprocessInit == 0 ) ;  // not assigned yet
            return c4->encrypt->encryptInit( server, config, &c4->encrypt->blockSize, &(c4->encrypt->preprocess), &(c4->encrypt->preprocessInit) ) ;
         }
      #endif
   #endif
   // AS Jan 23/03 - Support for run-time loading of encryption
   short code4encryptFileGet( CODE4 *c4 )
   {
      #ifdef S4ENCRYPT_HOOK
         #ifdef S4SERVER
            if ( c4->encrypt == 0 )  // not available (yet) - usually with config.dbf
               return 0 ;
         #else
            // AS Apr 24/03 - If c4->encrypt is not initialized, then obviously the flag is currently set to 0
            if ( c4->encrypt == 0 )  // not available (yet) - usually with config.dbf
               return 0 ;
         #endif
         assert5( c4->encrypt->encryptFileGet != 0 ) ;
         return c4->encrypt->encryptFileGet( c4) ;
      #else
         return e4notSupported ;
      #endif
   }



   static void code4encryptFileInitUndo( CODE4 *c4 )
   {
      #ifdef S4ENCRYPT_HOOK
         if ( c4->encrypt == 0 )  // not initialized
            return ;
         if ( c4->encrypt->encryptFileInitUndo != 0 )
            c4->encrypt->encryptFileInitUndo( c4 ) ;
         c4->encrypt->preprocess = 0 ;
         c4->encrypt->preprocessInit = 0 ;
         u4free( c4->encrypt ) ;
         c4->encrypt = 0 ;
      #endif
   }



   static void code4encryptCodeInitUndo( CODE4 *c4 )
   {
      #ifdef S4ENCRYPT_COM
         if ( c4->encrypt != 0 )
         {
            assert5( c4->encrypt->encryptCodeInitUndo != 0 ) ;
            c4->encrypt->encryptCodeInitUndo( c4 ) ;
         }
      #endif
      c4encryptUnload( c4 ) ;
   }



   int S4FUNCTION code4encryptConnection( CODE4 *c4, int level, int numBits )
   {
      #if defined( S4ENCRYPT_COM ) && defined( S4CLIENT )
         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
            {
               error4( c4, e4parmNull, E96702 ) ;
               return 0 ;
            }
         #endif

         if ( code4isConnected( c4 ) )
            return error4describe( c4, e4connection, E88056, "Cannot call code4encrypt() on an already connected connection", 0, 0 ) ;

         if ( c4->encrypt != 0 )  // already initialized, this is an error.
            return r4active ;

         int rc = c4encryptLoad( c4, (short)numBits ) ;
         if ( rc != 0 )
            return rc ;

         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->encryptConnection != 0 ) ;
         return c4->encrypt->encryptConnection( c4, level, numBits, &c4->encrypt->blockSize, &c4->encrypt->preprocess, &c4->encrypt->preprocessInit ) ;
      #else
         return e4notSupported ;
      #endif
   }



   #ifdef S4ENCRYPT_COM
      short con4lowGetEncryptRead( CODE4 *c4, CONNECT4LOW *conLow )
      {
         if ( conLow->preprocess == 0 )  // means we have not enabled yet, so write not enabled
            return 0 ;
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->conLowGetEncryptRead != 0 ) ;
         return c4->encrypt->conLowGetEncryptRead( conLow->preprocess ) ;
      }



      void con4lowSetEncryptRead( CODE4 *c4, CONNECT4LOW *conLow, short val )
      {
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->conLowSetEncryptRead != 0 ) ;
         c4->encrypt->conLowSetEncryptRead( conLow->preprocess, val ) ;
      }



      short con4lowGetEncryptWrite( CODE4 *c4, CONNECT4LOW *conLow )
      {
         if ( conLow->preprocess == 0 )  // means we have not enabled yet, so write not enabled
            return 0 ;
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->conLowGetEncryptWrite != 0 ) ;
         return c4->encrypt->conLowGetEncryptWrite( conLow->preprocess ) ;
      }



      void con4lowSetEncryptWrite( CODE4 *c4, CONNECT4LOW *conLow, short val )
      {
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->conLowSetEncryptWrite != 0 ) ;
         c4->encrypt->conLowSetEncryptWrite( conLow->preprocess, val ) ;
      }



      int con4lowPreProcess( CODE4 *c4, void *preprocess, unsigned char *message, int len )
      {
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->conLowEncrypt != 0 ) ;
         return c4->encrypt->conLowEncrypt( preprocess, message, len ) ;
      }



      int con4lowPostProcess( CODE4 *c4, void *preprocess, unsigned char *message, int len, unsigned char *messageOut )
      {
         // set messageOut to 0 to use message as output
         if ( messageOut == 0 )
            messageOut = message ;
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->conLowDecrypt != 0 ) ;
         return c4->encrypt->conLowDecrypt( preprocess, message, len, messageOut ) ;
      }



      void con4lowPreprocessInitUndo( CODE4 *c4, CONNECT4LOW *conLow )
      {
         if ( conLow->preprocess != 0 )
         {
            assert5( c4->encrypt != 0 ) ;
            assert5( c4->encrypt->encryptComInitUndo != 0 ) ;
            c4->encrypt->encryptComInitUndo( conLow->preprocess ) ;
            conLow->preprocess = 0 ;
            conLow->preprocessInit = 0 ;
         }
      }



      int con4lowPreprocessInit( CODE4 *c4, CONNECT4LOW *conLow, const void *key, short keyLen )
      {
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->encryptComInit != 0 ) ;
         return c4->encrypt->encryptComInit( c4, key, keyLen, &conLow->blockSize, &conLow->preprocess, &conLow->preprocessInit ) ;
      }
   #endif


   // AS Jul 28/05 - compile fix
   short code4encryptFileHook( CODE4 *c4, FILE4 *file, void *encryptInit )
   {
      #ifdef S4ENCRYPT_HOOK
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->encryptFileHook != 0 ) ;
         return c4->encrypt->encryptFileHook( c4->encrypt->preprocess, file, encryptInit ) ;
      #else
         return e4notSupported ;
      #endif
   }



   void code4fileDecrypt( FILE4 *f4, long pos, const void *ptrIn, long ptrInLen, void *ptrOut )
   {
      #ifdef S4ENCRYPT_HOOK
         CODE4 *c4 = f4->codeBase ;
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->decryptHook != 0 ) ;
         c4->encrypt->decryptHook( c4->encrypt->preprocess, c4->encrypt->preprocessInit, f4, pos, ptrIn, ptrInLen, ptrOut ) ;
      #endif
   }



   #ifdef S4CLIENT
      int code4serverPreprocessInit( CODE4 *c4, const char *userId, const char *password )
      {
         #ifdef S4PREPROCESS_COM
            CONNECT4 *connect = &c4->clientConnect ;

            if ( c4->encrypt == 0 )  // means encryption not enabled via function call
            {
               // in this case, as per documentation, attempt to tell server we do not want to perform encryption.
               // send MSG5PREPROCESS_B with no encryption
               connect4sendShort( connect, MSG5PREPROCESS_DISABLE ) ;
               connect4sendFlush( connect ) ;
               short status = connect4receiveShort( connect ) ;
               if ( status != 0 )
               {
                  if ( status != e4preprocessRequired )  // error not related to preprocess
                     return error4( c4, status, E96702);
                  // if here, then we must use preprocessing, so attempt to use it, try 8 bit first
                  // disable connection so we can bypass check.
                  int oldConnected = c4->defaultServer.connected ;
                  int oldOff = c4->errOff ;  // suspend error message
                  c4->errOff = 1 ;
                  c4->defaultServer.connected = 0 ;
                  int rc = code4encryptConnection( c4, r4passwordEncryption, 8 ) ;
                  if ( rc != 0 )  // 8-bit failed, try 256 bit
                  {
                     error4set( c4, 0 ) ;
                     rc = code4encryptConnection( c4, r4passwordEncryption, 128 ) ;
                  }
                  c4->defaultServer.connected = oldConnected ;
                  c4->errOff = oldOff ;
                  if ( rc != 0 )
                     return rc ;
               }
               else  // means was ok not to have encryption
                  return 0 ;
            }

            assert5( c4->encrypt->serverEncryptInit != 0 ) ;
            CONNECT4LOW *connectLow = connect->connectBuffer.connectLowPtr ;
            assert5( connectLow != 0 ) ;
            return c4->encrypt->serverEncryptInit( c4, userId, password, &connectLow->blockSize, &connectLow->preprocess, &connectLow->preprocessInit) ;
         #else
            return 0 ;
         #endif
      }
   #endif



   void *code4filePreprocess( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned long len )
   {
      #ifdef S4ENCRYPT_HOOK
         CODE4 *c4 = f4->codeBase ;
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->filePreprocess != 0 ) ;
         return c4->encrypt->filePreprocess( f4, pos, ptr, len ) ;
      #else
         return 0 ;
      #endif
   }


   void code4filePreprocessDone( FILE4 *f4 )
   {
      #ifdef S4ENCRYPT_HOOK
         CODE4 *c4 = f4->codeBase ;
         assert5( c4->encrypt != 0 ) ;
         assert5( c4->encrypt->filePreprocessDone != 0 ) ;
         c4->encrypt->filePreprocessDone( f4 ) ;
      #endif
   }
#endif /* S4ENCRYPT_DLL */


// AS Jun 10/03 - Moved from c4conlow.c to c4code.c
#if !defined( S4STAND_ALONE ) && defined( S4COMPRESS )
   // global compress level, allow modification primarly at start-up
   #define COMPRESS4DEFAULT_LEVEL 3
   // a minimum length before compression will be done
   #define COMPRESS4DEFAULT_MIN_LENGTH 1000
#endif

#ifdef S4COMPRESS
   // AS Jul 16/03 - moved to d4defs.h, made non-static, used in c4conlow.c
   // #define COMPRESS4MAX_LENGTH 16384   /* LY 2002/05/23 : moved outside of above #ifndef */
   unsigned char g_compressed[COMPRESS4MAX_LENGTH] ;
#endif


// AS May 13/04 - Make this function generally available for both client and server
#ifndef S4SERVER
   int S4FUNCTION code4compress( CODE4 *c4, short flag )
   {
      // used to enable communications compression by the client
      #ifdef S4COMPRESS
         #ifndef S4STAND_ALONE
            // simple on/off flag uses default paramaters to configure both client and server side
            if ( flag == 1 )
            {
               int rc = code4compressConfigure( c4, -1, COMPRESS4DEFAULT_LEVEL, COMPRESS4DEFAULT_MIN_LENGTH ) ;
               if ( rc < 0 )
                  return rc ;
               return code4compressConfigureServer( c4, -1, COMPRESS4DEFAULT_LEVEL, COMPRESS4DEFAULT_MIN_LENGTH ) ;
            }
            else
            {
               // turn off by setting level to 0 and start length at max length
               int rc = code4compressConfigure( c4, -1, 0, COMPRESS4MAX_LENGTH ) ;
               if ( rc < 0 )
                  return rc ;
               return code4compressConfigureServer( c4, -1, 0, COMPRESS4MAX_LENGTH ) ;
            }
         #endif
         // also return 0 if S4COMPRESS is defined in stand-alone - allows for general coding between c/s and s/a
         return 0 ;
      #else
         return e4notSupported ;
      #endif
   }



   // AS May 13/04 - moved from c4conlow.c to be generally available...
   int S4FUNCTION code4compressConfigure( CODE4 *c4, short fileLevel, short comLevel, short minLength )
   {
      // AS Dec 6/06 - check OFF_COMPRESS as well
      // CS 2011/04/29 Change to S4COMPRESS.
      #ifdef S4COMPRESS
         #ifdef S4CLIENT
            // used to configure the compression.  set comLevel to 0 to disable...
            // -1 means leave setting alone
            if ( comLevel != -1 || minLength != -1 )  // change a setting
            {
               #ifdef E4PARM_HIGH
                  if ( c4 == 0 || comLevel < -1 || minLength < -1 )
                     return error4( c4, e4parm, E96901 ) ;
               #endif
               CONNECT4 *connect = c4getClientConnect( c4 ) ;
               #ifdef E4PARM_HIGH
                  if ( connect == 0 || connect->connectBuffer.connectLowPtr == 0 )  // code4connect not called yet...
                     return error4( c4, e4parm, E88057 ) ;
               #endif
               connect4lowCompressConfigure( connect->connectBuffer.connectLowPtr, comLevel, minLength ) ;
            }
         #endif
         // AS Sep 14/04 - support for file level setting of compression (stand/alone)
         #ifdef S4STAND_ALONE
            #ifdef E4PARM_HIGH
               if ( c4 == 0 || fileLevel < -1 || fileLevel > 9 )
                  return error4( c4, e4parm, E96901 ) ;
            #endif

            if ( fileLevel != -1 )
               c4->fileCompressLevel = fileLevel ;
         #endif
      #endif
      return 0 ;
   }


   int S4FUNCTION code4compressConfigureServer( CODE4 *c4, short fileLevel, short comLevel, short minLength )
   {
      #ifdef S4CLIENT
         // tells the server what level of compression to use for communications sent to this client
         // -1 means leave setting alone
         #ifdef E4PARM_HIGH
            if ( c4 == 0 || comLevel < -1 || minLength < -1 )
               return error4( c4, e4parm, E96901 ) ;
            if ( connect == 0 )  // code4connect not called yet...
               return error4( c4, e4parm, E88057 ) ;
         #endif
         CONNECT4 *connect = c4getClientConnect( c4 ) ;
         connect4sendShort( connect, MSG5COMPRESS ) ;
         connect4sendShort( connect, fileLevel ) ;
         connect4sendShort( connect, comLevel ) ;
         connect4sendShort( connect, minLength ) ;
         connect4sendFlush( connect ) ;

         return 0 ;
      #else
         return e4notSupported ;  // LY May 14/04 : not returning value under S4STAND_ALONE
      #endif
   }
#endif /* !S4SERVER */



// AS Mar 31/04 - moved from c4baspas.c - required in odbc server
short S4FUNCTION code4unlockAutoCB( CODE4 *cb )
{
   #ifdef E4VBASIC
      if ( c4parm_check( cb, 1, E40133 ) ) return -1 ;
   #endif

   return code4unlockAuto( cb ) ;
}



void S4FUNCTION code4unlockAutoSetCB( CODE4 *cb, short value )
{
   #ifdef E4VBASIC
      if ( c4parm_check( cb, 1, E40133 ) ) return ;
   #endif

   code4unlockAutoSet( cb, value ) ;
}


