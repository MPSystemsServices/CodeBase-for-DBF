#include <windows.h>
#include <stdio.h>

extern "C"
{
__declspec(dllimport) BOOL server4taskbarIcon(DWORD operation, const char *tip);
}

//HWND hWndServer = NULL;
//HINSTANCE g_hInstance;
SERVICE_STATUS serviceStatus ;
SERVICE_STATUS_HANDLE serviceStatusHandle ;

VOID _stdcall service4start( DWORD argc, LPTSTR *argv ) ;
VOID _stdcall service4ctrlHandler( DWORD opcode ) ;
DWORD service4init( DWORD argc, LPTSTR *argv, DWORD *specificError ) ;

// CS 2002/06/13 Service does windowing, so main replaced by WinMain
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
   SERVICE_TABLE_ENTRY DispatchTable[] =
   {
      { "CodeBase Database Server", service4start },
      { NULL, NULL }
   } ;

//   g_hInstance = hInstance;

   StartServiceCtrlDispatcher( DispatchTable );

  return 0;
}

DWORD service4init( DWORD argc, LPTSTR *argv, DWORD *specificError )
{
   char *configName = 0 ;
   if ( argc > 1 )
      configName = argv[1] ;

   return NO_ERROR ;
}



VOID _stdcall service4start( DWORD argc, LPTSTR *argv )
{
   DWORD status ;
   DWORD specificError ;

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

   SetServiceStatus( serviceStatusHandle, &serviceStatus ) ;

   server4taskbarIcon(NIM_ADD, NULL);  // CS 2002/07/24 move to function in d4main.c

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



VOID _stdcall service4ctrlHandler( DWORD Opcode )
{
   switch( Opcode )
   {
      case SERVICE_CONTROL_STOP:
      case SERVICE_CONTROL_SHUTDOWN:

         serviceStatus.dwCurrentState  = SERVICE_STOPPED ;
         serviceStatus.dwWin32ExitCode = 0 ;
         serviceStatus.dwCheckPoint = 0 ;
         serviceStatus.dwWaitHint = 0 ;

         SetServiceStatus( serviceStatusHandle, &serviceStatus ) ;

         server4taskbarIcon(NIM_DELETE, NULL);  // CS 2002/07/24 change to function call

         return ;

      case SERVICE_CONTROL_INTERROGATE:
         // Fall through to send current status.
         break ;
   }

   // Send current status.
   SetServiceStatus( serviceStatusHandle, &serviceStatus ) ;
   return ;
}
