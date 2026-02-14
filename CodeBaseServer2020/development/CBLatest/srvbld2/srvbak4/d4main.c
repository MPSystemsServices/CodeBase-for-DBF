#include <windows.h>
#include <stdio.h>
#include <time.h>
//#include "d4all.h"
   #define S4FOX
#define S4DLL
   #define S4WIN32           /* for Windows NT and Windows 95 */
   #define S4WINSOCK
#define DEF4SERVER_ID "localhost"
#define DEF4PROCESS_ID "23165"
 #define E4HOOK
   #define E4VBASIC
   #define E4PARM_HIGH
   #define E4PAUSE
 #define S4OFF_REPORT
#define S4FUNCTION __stdcall
#define S4EXPORT __declspec(dllexport)
         #define S4SERVER_GUI

extern "C"
{
   __declspec(dllexport) long FAR PASCAL MainWndProc( HWND, UINT, UINT, LONG ) ;
         unsigned d4server( void * ) ;
S4EXPORT BOOL server4taskbarIcon(DWORD operation, const char *tip);
}

#ifdef S4SERVER

   #define IDM_ABOUT 102
   #define ID_SERVERINFO 110
   #define ID_CONFIGINFO 130
   #define CWO_SERVER 0

   #if !defined( S4ODBC_BUILD ) && !defined( S4UNIX )
      HWND hWndGlobal, hWndAbout, hWndLicence, hWndConfig ;
      HINSTANCE hInst = NULL ;
         UINT timer = 0 ;

      BOOL server4taskbarIcon( DWORD operation, const char *tip )
      {
         NOTIFYICONDATA iconInfo ;

         iconInfo.cbSize = sizeof( NOTIFYICONDATA ) ;
         iconInfo.hWnd = hWndGlobal ;
         iconInfo.uID = 0 ;
         iconInfo.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP ;
         iconInfo.uCallbackMessage = WM_APP ;
         iconInfo.hIcon = ( HICON )LoadImage( hInst, "ICON_SERVER", IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR | LR_DEFAULTSIZE ) ;
         if ( tip )
         {
            strncpy( iconInfo.szTip, tip, 64 ) ;
            iconInfo.szTip[63] = 0 ;
         }
         else
            strcpy( iconInfo.szTip, "CodeBase Database Server" ) ;

         return Shell_NotifyIcon( operation, &iconInfo ) ;
      }



      BOOL S4FUNCTION InitApplication( HINSTANCE hInstance )
      {
         WNDCLASS wc ;

         if ( hInst == NULL )
            hInst = hInstance ;

         wc.style = 0 ;
         wc.lpfnWndProc = MainWndProc ;

         wc.cbClsExtra = 0 ;
         wc.cbWndExtra = 4 ;
         wc.hInstance = hInstance ;
         wc.hIcon = LoadIcon( hInstance, "ICON_SERVER" ) ;
         wc.hCursor = LoadCursor( ( HINSTANCE )NULL, IDC_ARROW ) ;
         #ifdef S4SERVER_GUI
            wc.hbrBackground = GetSysColorBrush( COLOR_3DFACE ) ;
         #else
            wc.hbrBackground = ( HBRUSH )GetStockObject( WHITE_BRUSH ) ;
         #endif
         wc.lpszMenuName = "MENU_MAIN" ;
         wc.lpszClassName = "TestWClass" ;

         return RegisterClass( &wc ) ;
      }


      void doServerGui( const HINSTANCE hInstance, const HGDIOBJ systemFont )
      {
      }


      HWND S4FUNCTION InitInstance( HINSTANCE hInstance, int nCmdShow )
      {
         char windowTitle[256] = "CodeBase 6.5 Database Server" ;

            hWndGlobal = CreateWindow( "TestWClass", windowTitle, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                                       650, 500, HWND_DESKTOP, ( HMENU )NULL, hInstance, ( LPVOID )NULL ) ;
            HGDIOBJ systemFont ;
            systemFont = GetStockObject( DEFAULT_GUI_FONT ) ;

         if ( hWndGlobal == NULL )
            return NULL ;

         SetWindowLong( hWndGlobal, CWO_SERVER, 0L ) ;

         ShowWindow( hWndGlobal, nCmdShow ) ;
         UpdateWindow( hWndGlobal ) ;

         doServerGui( hInstance, systemFont ) ;
         return hWndGlobal ;
      }



      void initAboutDialog( HWND hWnd, LPARAM lParam )
      {
      }



      LRESULT CALLBACK AboutProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
      {
         return FALSE ;
      }



      static void initAboutLicense( HWND hWnd, LPARAM lParam )
      {
      }



      LRESULT CALLBACK AboutLicence( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
      {
         return FALSE ;
      }



      #ifdef S4SERVER_GUI
         //Bool5 g_tranEnabled = 0 ;  // track whether it was ever enabled

         void UpdateWindowGUI()
         {
         }
      #endif



      LRESULT CALLBACK AboutConfig( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
      {
         return FALSE ;
      }



      void menu4about( HWND hWnd, DLGPROC proc )
      {
      }



      void menu4licence( HWND hWnd, DLGPROC proc )
      {
      }



      void start4config( HWND hWnd, DLGPROC proc )
      {
      }


      void mainParentNotify( UINT wParam, LONG lParam, HWND hWnd )
      {
      }



      #ifdef S4WIN32
         __declspec( dllexport ) long FAR PASCAL MainWndProc( HWND hWnd, UINT message, UINT wParam, LONG lParam )
      #else
         long FAR PASCAL _export MainWndProc( HWND hWnd, UINT message, UINT wParam, LONG lParam )
      #endif
      {
         static DLGPROC aboutproc, aboutlicence, startconfig ;
         static char buffer[256] ;
         static UINT WM4_TASKBARCREATED ;

         switch( message )
         {
            case WM_APP:  // CS 2002/06/13 mouse activity in the tray icon
               if ( lParam == WM_LBUTTONUP || lParam == WM_MBUTTONUP || lParam == WM_RBUTTONUP )
               {
                  // running as a service and the user clicked and released any mouse button
                  ShowWindow( hWndGlobal, SW_SHOW ) ;
                  UpdateWindow( hWndGlobal ) ;
               }
               break ;

            case WM_CREATE :
               aboutproc = (DLGPROC)MakeProcInstance( (FARPROC)AboutProc, GetWindowLong( hWnd, GWW_HINSTANCE ) ) ;
               aboutlicence = (DLGPROC)MakeProcInstance( (FARPROC)AboutLicence, GetWindowLong( hWnd, GWW_HINSTANCE ) ) ;
               startconfig = (DLGPROC)MakeProcInstance( (FARPROC)AboutConfig, GetWindowLong( hWnd, GWW_HINSTANCE ) ) ;
               WM4_TASKBARCREATED = RegisterWindowMessage( TEXT( "TaskbarCreated" ) );  // CS 2002/07/24
               break ;

            case WM_COMMAND :
               switch( wParam )
               {
                  case IDM_ABOUT :
                     menu4licence( hWnd, aboutlicence ) ;
                     return 0 ;
                  case ID_SERVERINFO :
                     menu4about( hWnd, aboutproc ) ;
                     return 0 ;
                  case ID_CONFIGINFO :
                     start4config( hWnd, startconfig ) ;
                     return 0 ;
                  default:
                     break ;
               }
               break ;

            #ifdef S4SERVER_GUI
               case WM_TIMER :
                  UpdateWindowGUI() ;
                  break ;

               case WM_SIZE :
                  switch(wParam)
                  {
                     case SIZE_MINIMIZED :
                        KillTimer( hWnd, timer ) ;
                        break ;

                     case SIZE_RESTORED :
                     case SIZE_MAXIMIZED :
                        timer = SetTimer( hWnd, 0, 2000, NULL ) ;
                        UpdateWindowGUI() ;
                        break ;

                     default :
                        break ;
                  }
                  break ;

               case WM_PARENTNOTIFY:
                  mainParentNotify( wParam, lParam, hWnd ) ;
                  break ;
            #endif /* S4SERVER_GUI */
            case WM_DESTROY:
               //server4setDoExit( serverPtr, r4exit ) ;
               PostQuitMessage( 0 ) ;
               return 0 ;
            // AS 03/28/01 - Need to handle the shutdown differently since query end does not allow the
            // process to continue in Windows 2000
            case WM_QUERYENDSESSION:
               #ifdef S4OLEDEBUG_PRINT
                  log5( "WM_QUERYENDSESSION being called\r\n" ) ;
               #endif
                  return TRUE ;
            case WM_QUIT :
            case WM_CLOSE:
                  ShowWindow( hWndGlobal, SW_HIDE ) ;

               // close other windows that might be open
               if ( hWndAbout )
                  SendMessage( hWndAbout, WM_CLOSE, 0, 0L ) ;
               FreeProcInstance( (FARPROC)aboutproc ) ;
               if( hWndLicence )
                  SendMessage( hWndLicence, WM_CLOSE, 0, 0L ) ;
               FreeProcInstance( (FARPROC)aboutlicence ) ;
               if( hWndConfig )
                  SendMessage( hWndConfig, WM_CLOSE, 0, 0L ) ;
               FreeProcInstance( (FARPROC)startconfig ) ;

               //if (isService)
                  return 0;

               break ;
            default:
               if ( message == WM4_TASKBARCREATED )
               {
                  HANDLE hEventLog = RegisterEventSource(0, "CBServer");
                  char p[256] = "WM4_TASKBARCREATED message received";
                  char *pp[] = {p};
                  ReportEvent(hEventLog, EVENTLOG_INFORMATION_TYPE, 0, 1002, 0, 1, 0, (LPCTSTR*)pp, 0);
                  DeregisterEventSource(hEventLog);

                  server4taskbarIcon( NIM_ADD, NULL );
               }
               break ;
          }
          return DefWindowProc( hWnd, message, wParam, lParam ) ;
      }



      long local4main( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow )
      {
          // called by WinMain as well as inside of SIMBA engine to start the server window and process...
         #ifdef S4COMTHREADS
            HANDLE serverThread ;
            unsigned int serverThreadId ;
            DWORD rc=WAIT_TIMEOUT ;
            MSG msg ;
         #endif

         if ( !hPrevInstance )
         {
            if ( !InitApplication( hInstance ) )
               return ( FALSE ) ;
            if ( !InitInstance( hInstance, nCmdShow ) )
               return ( FALSE ) ;

            hInst = hInstance ;

            #ifdef S4COMTHREADS
               serverThread = ( HANDLE )_beginthreadex( NULL, 0, &d4server, lpCmdLine, 0, &serverThreadId ) ;
               if ( serverThread != 0 )
               {
                  while ( GetMessage( &msg, NULL, 0, 0 ) )
                  {
                     TranslateMessage( &msg ) ;
                     DispatchMessage( &msg ) ;
                     rc = WaitForSingleObject( serverThread, 0 ) ;
                     if ( rc == WAIT_OBJECT_0 )
                        break ;
                     if ( rc != WAIT_TIMEOUT )
                     {
                        break ;
                     }
                  }
                  if ( rc == WAIT_TIMEOUT )
                     WaitForSingleObject( serverThread, INFINITE ) ;
                  CloseHandle( serverThread )  ;
               }
            #else
               d4server( lpCmdLine ) ;
            #endif

            PostQuitMessage( 0 ) ;
         }
         else
         {
            MessageBox( ( HWND )NULL, "An instance of the CodeBase database server already exists.\n Press OK to terminate this session.", "CodeBase Server Status", MB_ICONSTOP ) ;
            SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
            PostQuitMessage( 0 ) ;
         }

         return 0 ;
      }
   #endif  /* #if !defined( S4ODBC_BUILD ) && !defined( S4UNIX ) */



   #ifdef S4ODBC_BUILD
      // for ODBC_BUILD DLL, we need to export this function...
      int S4FUNCTION d4serverInitialize( const char *configName )
   #else
      // don't need to export in non-odbc-build
      int d4serverInitialize( const char *configName )
   #endif
   {
      return 0 ;
   }



         unsigned d4server( void *cmdLineVd )
      {
         char *cmdLine = ( char * )cmdLineVd ;  /* must be void for _beginthread */

         for ( ;; )
         {
            if ( d4serverInitialize( cmdLine ) != 0 )
            {
                   SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
                   PostQuitMessage( 0 ) ;
               return 0 ;
            }
         }
            SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
            PostQuitMessage( 0 ) ;
         return 0 ;
      }
   #endif /* !S4ODBC_BUILD */
