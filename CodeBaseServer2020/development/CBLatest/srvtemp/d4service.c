/* d4service.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.

   Used to run the CodeBase database server as a service (Windows NT/2000)
*/

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifndef S4MAX_USERS
   #error must define S4MAX_USERS to maximum users for server
#endif

#include "process.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 20000 ;
#endif

//char StampedlicenceString[]="WARNING\n\nThe license for this software is a run-time license for use with application(s) written \
//by the Organization or Person whose name is specified below. The license does not allow this software to be used with any application \
//programmatic database capabilities." ;

//   char UnStampedString[]= "\n\nCodeBase Server is licensed for use in accordance with the CodeBase 6.5 license Agreement. \
//\n\nThis license allows the CodeBase Server to be installed and run from one computer only. \
//Contact Sequiter Software, Inc. for distribution information." ;

SERVER4 *serverPtr = 0 ;
HWND hWndServer = NULL;
HINSTANCE g_hInstance;
SERVICE_STATUS serviceStatus ;
SERVICE_STATUS_HANDLE serviceStatusHandle ;

VOID _stdcall service4start( DWORD argc, LPTSTR *argv ) ;
VOID _stdcall service4ctrlHandler( DWORD opcode ) ;
DWORD service4init( DWORD argc, LPTSTR *argv, DWORD *specificError ) ;

VOID service4debugOut(LPSTR String, DWORD Status)
{
   CHAR Buffer[1024] ;
   if ( strlen( String ) < 1000 )
   {
      sprintf( Buffer, String, Status ) ;
      OutputDebugStringA(Buffer) ;
   }

   return ;
}



// CS 2002/06/13 Service does windowing, so main replaced by WinMain
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
   SERVICE_TABLE_ENTRY DispatchTable[] =
   {
      { "CodeBase Database Server", service4start },
      { NULL, NULL }
   } ;

   g_hInstance = hInstance;

   if ( !StartServiceCtrlDispatcher( DispatchTable ) )
      service4debugOut(" [CodeBase Database Server] StartServiceCtrlDispatcher error = %d\n", GetLastError() ) ;

  return 0;
}



DWORD g_argc = 0 ;
LPTSTR *g_argv = 0 ;

static void service4freeArgs()
{
   if ( g_argv != 0 )
   {
      for ( DWORD argLoop = 0 ; argLoop < g_argc ; argLoop++ )
      {
         if ( g_argv[argLoop] != 0 )
         {
            free(g_argv[argLoop]) ;
            g_argv[argLoop] = 0 ;
         }
      }
      free( g_argv ) ;
      g_argv = 0 ;
      g_argc = 0 ;
   }
}



static int service4copyArgs( DWORD argc, LPTSTR *argv )
{
   g_argc = argc ;
   int lenAlloc = argc * sizeof( LPTSTR ) ;
   g_argv = (LPTSTR *)malloc( lenAlloc ) ;
   memset( g_argv, 0, lenAlloc ) ;
   if ( g_argv != 0 )
   {
      for ( DWORD argLoop = 0 ; argLoop < argc ; argLoop++ )
      {
         int sLen = strlen( argv[argLoop] ) ;
         g_argv[argLoop] = (LPTSTR)malloc( sLen+1 ) ;
         if ( g_argv[argLoop] == 0 )
         {
            service4freeArgs() ;
            return -1 ;
         }
         strcpy( g_argv[argLoop], argv[argLoop] ) ;
      }
   }
   return 0 ;
}



DWORD service4init( DWORD argc, LPTSTR *argv, DWORD *specificError )
{
   CODE4 *c4;
   setService();

   // CS 2002/06/13 Draw window, but keep it hidden.
   // Window will be displayed when the user clicks on the tray icon.
   if (!InitApplication(g_hInstance))
      return error4( 0, e4server, E70101 ) ;

   hWndServer = InitInstance(g_hInstance, SW_HIDE);
   if (hWndServer == NULL)
      return error4( 0, e4server, E70101 ) ;

   *specificError = 0 ;
   c4 = code4alloc( 0 ) ;
   if ( c4 == 0 )
      return error4( 0, e4memory, E70101 ) ;

   c4setRunAsService( c4, 1 ) ;

   serverPtr = server4alloc() ;
   if ( serverPtr == 0 )
   {
      code4initUndo( c4 ) ;  // frees up memory for c4 as well...
      c4 = 0 ;
      return error4( 0, e4memory, E70101 ) ;
   }

   setServer4global(serverPtr);

   char *configName = 0 ;
   if ( argc > 1 )
      configName = argv[1] ;

   int rc = server4init( serverPtr, c4, configName, 1, 0 ) ;  // CS 2001/04/03 add parameter
   if ( rc < 0 )
   {
      server4initUndo( serverPtr ) ;
      server4free( serverPtr ) ;
      serverPtr = 0 ;
      *specificError = (rc == -1) ? e4server : rc ;  // CS 2000/05/15 return e4server instead of -1
      return ERROR_SERVICE_SPECIFIC_ERROR ;
   }

   // and save the command line paramaters for later use if server is restarted.
   if ( service4copyArgs( argc, argv ) != 0 )
   {
      server4initUndo( serverPtr ) ;
      server4free( serverPtr ) ;
      serverPtr = 0 ;
      *specificError = rc ;
      return ERROR_SERVICE_SPECIFIC_ERROR ;
   }

   /* CS 2000/09/07 Remove prompt for customer name and serial number.
      It is not needed in the service. */

   return NO_ERROR ;
}



static DWORD WINAPI service4server(LPVOID lpParameter)
{
   for ( ;; )
   {
      int rc = server4waitOnQuitOrExit( serverPtr ) ;

      if ( rc == r4exit )  // means hard exit
      {
         SendMessage( hWndServer, WM_CLOSE, 0, 0L ) ;
         PostQuitMessage(0) ;
         return 0 ;
      }

      if ( rc == r4restart )
      {
         while ( server4numClients( serverPtr ) != 0 )  // wait for the requester to finish up
            ;
      }
      else  // assume r4shutdown
         rc = r4shutdown ;  // ensure we dont restart

      if ( serverPtr != 0 )
      {
         server4initUndo( serverPtr ) ;
         server4free( serverPtr ) ;
         serverPtr = 0 ;
      }

      if ( rc == r4restart )
      {
         // need to reinitialize server...
         DWORD specificError ;
         DWORD status = service4init( g_argc, g_argv, &specificError ) ;
         if ( status != 0 )  // failed to reinitialize - just exit out
            break ;
      }
      else
         break ;
   }

   service4freeArgs() ;

   SendMessage( hWndServer, WM_CLOSE, 0, 0L ) ;
   PostQuitMessage(0) ;
   return 0 ;
}



VOID _stdcall service4start( DWORD argc, LPTSTR *argv )
{
   DWORD status ;
   DWORD specificError ;

   #ifdef E4DEBUG_STOP
      DebugBreak() ;
   #endif

   serviceStatus.dwServiceType      = SERVICE_WIN32_OWN_PROCESS ;
   serviceStatus.dwCurrentState     = SERVICE_START_PENDING ;
   serviceStatus.dwControlsAccepted = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN ;
   serviceStatus.dwWin32ExitCode    = 0 ;
   serviceStatus.dwServiceSpecificExitCode = 0 ;
   serviceStatus.dwCheckPoint       = 0 ;
   serviceStatus.dwWaitHint         = 0 ;

   serviceStatusHandle = RegisterServiceCtrlHandler( "CodeBase Database Server", service4ctrlHandler ) ;

   if (serviceStatusHandle == (SERVICE_STATUS_HANDLE)0 )
   {
      service4debugOut(" CodeBase Database Server RegisterServiceCtrlHandler failed %d\n", GetLastError()) ;
      return ;
   }

   // Initialization code goes here.
   status = service4init(argc,argv, &specificError) ;

   // Handle error condition
   if ( status != NO_ERROR )
   {
      serviceStatus.dwCurrentState  = SERVICE_STOPPED ;
      serviceStatus.dwCheckPoint    = 0 ;
      serviceStatus.dwWaitHint      = 0 ;
      serviceStatus.dwWin32ExitCode = status ;
      serviceStatus.dwServiceSpecificExitCode = specificError ;

      SetServiceStatus( serviceStatusHandle, &serviceStatus ) ;
      return ;
   }

   // Initialization complete - report running status.
   serviceStatus.dwCurrentState = SERVICE_RUNNING ;
   serviceStatus.dwCheckPoint   = 0 ;
   serviceStatus.dwWaitHint     = 0 ;

   if ( !SetServiceStatus( serviceStatusHandle, &serviceStatus ) )
   {
      status = GetLastError() ;
      service4debugOut( " CodeBase Database Server SetServiceStatus error %ld\n", status ) ;
   }

   // This is where the service does its work.
   service4debugOut( " CodeBase Database Server Returning the Main Thread \n", 0 ) ;

   server4taskbarIcon(NIM_ADD, NULL);  // CS 2002/07/24 move to function in d4main.c

   DWORD threadId;
   CreateThread(NULL, 0, service4server, 0, 0, &threadId);


   // CS 2002/06/13 loop for handling Windows messages
   MSG msg ;
   while (GetMessage(&msg, NULL, 0, 0))
   {
      TranslateMessage(&msg) ;
      DispatchMessage(&msg) ;
   }

   serviceStatus.dwCurrentState  = SERVICE_STOPPED ;
   serviceStatus.dwCheckPoint    = 0 ;
   serviceStatus.dwWaitHint      = 0 ;
   serviceStatus.dwWin32ExitCode = 0 ;
   serviceStatus.dwServiceSpecificExitCode = 0 ;

   SetServiceStatus( serviceStatusHandle, &serviceStatus ) ;
   return ;
}


#ifdef E4DEBUG
   static void local4logInfo( char *note )
   {
      static char *logName = "c:\\locallog.txt" ;
      int existHandle ;
      DWORD fdwAccess, fdwShareMode, fdwCreate ;

      HANDLE hand = INVALID4HANDLE ;

      fdwAccess = GENERIC_WRITE | GENERIC_READ ;
      fdwShareMode = 0 ;
      fdwCreate = CREATE_ALWAYS ;

      existHandle = file4exists( logName ) ;
      #ifdef S4UNICODE
         unsigned short nameU[256] ;
         c4atou(name, (unsigned short *)nameU, 256) ;
      #endif


      DWORD flagsAndAttributes ;

      flagsAndAttributes = FILE_FLAG_WRITE_THROUGH ;

      if ( existHandle >= 0 )   /* file exists and is available */
      {
         #ifdef S4UNICODE
            hand = CreateFile( nameU, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
         #else
            hand = CreateFile( logName, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
         #endif
         if ( hand == INVALID4HANDLE )
            return ;
      }
      else
      {
         #ifdef S4UNICODE
            hand = CreateFile( nameU, fdwAccess, fdwShareMode, 0, fdwCreate, flagsAndAttributes, 0 ) ;
         #else
            hand = CreateFile( logName, fdwAccess, fdwShareMode, 0, fdwCreate, flagsAndAttributes, 0 ) ;
         #endif
      }

      if ( hand == INVALID4HANDLE )
         return ;

      FILE4LONG fileLength = u4filelength( hand ) ;

      unsigned long urc ;

      if ( SetFilePointer( hand, file4longGetLo( fileLength ), 0, FILE_BEGIN ) == (DWORD)-1 )
      {
         CloseHandle( hand ) ;
         return ;
      }

      unsigned len = strlen( note ) ;
      if ( note != 0 )
      {
         if ( WriteFile( hand, note, len, &urc, NULL ) == 0 )
         {
            // means failed...
            DWORD err = GetLastError() ;
            #ifdef E4ANALYZE
               if ( err == 0 && urc != 0 )  // give us a chance to look at the rc...
                  urc = 0 ;
            #endif
         }
         len = urc ;
      }

      CloseHandle( hand ) ;
      return ;
   }
#endif


VOID _stdcall service4ctrlHandler( DWORD Opcode )
{
   DWORD status ;

   switch( Opcode )
   {
      case SERVICE_CONTROL_STOP:
      case SERVICE_CONTROL_SHUTDOWN:
      // Do whatever it takes to stop here.
         if ( serverPtr != 0 )
         {
            #ifdef E4DEBUG
               local4logInfo( "request to shutdown service from Windows\r\n" ) ;
            #endif
            serviceStatus.dwCurrentState  = SERVICE_STOP_PENDING ;
            serviceStatus.dwWin32ExitCode = 0 ;
            serviceStatus.dwCheckPoint = 0 ;
            serviceStatus.dwWaitHint = 0 ;

            SetServiceStatus( serviceStatusHandle, &serviceStatus ) ;
            server4quit( serverPtr, r4shutdown ) ;
            // and now wait for the server to end...
            for( int count = 0 ;; count++)
            {
               Sleep( 100 ) ;
               if ( serverPtr == 0 )  // done
                  break ;
               if ( count == 300 )    // if hasn't shut down after 30 seconds, go ahead and terminate...
               {
                  #ifdef E4DEBUG
                     local4logInfo( "failed 30 second delay, shutting down service anyway\r\n" ) ;
                  #endif
                  break ;
               }
               #ifndef S4OFF_LOG
                  if ( ((count % 30) == 0) && (count > 30) )
                  {
                     #ifdef E4DEBUG
                        local4logInfo( "waited 3 seconds, still shutting down...\r\n" ) ;
                     #endif
                     break ;
                  }
               #endif
            }
            #ifdef E4DEBUG
               local4logInfo( "finished shutting down service\r\n" ) ;
            #endif
        }

         serviceStatus.dwCurrentState  = SERVICE_STOPPED ;
         serviceStatus.dwWin32ExitCode = 0 ;
         serviceStatus.dwCheckPoint = 0 ;
         serviceStatus.dwWaitHint = 0 ;

         if ( !SetServiceStatus( serviceStatusHandle, &serviceStatus ) )
         {
             status = GetLastError() ;
             service4debugOut( " CodeBase Database Server SetServiceStatus error %ld\n", status ) ;
         }

         server4taskbarIcon(NIM_DELETE, NULL);  // CS 2002/07/24 change to function call

         return ;

      case SERVICE_CONTROL_INTERROGATE:
         // Fall through to send current status.
         break ;
   }

   // Send current status.
   if ( !SetServiceStatus( serviceStatusHandle, &serviceStatus ) )
   {
      status = GetLastError() ;
      service4debugOut( " CodeBase Database Server SetServiceStatus error  %ld\n", status ) ;
   }
   return ;
}
