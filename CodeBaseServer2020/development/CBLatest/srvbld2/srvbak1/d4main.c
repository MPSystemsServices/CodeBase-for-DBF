#include "d4all.h"

#include "stamp5.hpp"
extern STAMP5 CustomerStamp ;

#ifdef S4UNIX
   #include <signal.h>
#else
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4ODBC_BUILD
   // ensure switches set correct...
   #ifdef S4COMM_THREAD
      #error simba build incorrectly built with communications
   #endif
#endif

#ifdef S4SERVER
   #ifndef S4MAX_USERS
      #error must define S4MAX_USERS to maximum users for server
   #endif

   #define IDM_ABOUT 102
   #define ID_SERVERINFO 110
   #define ID_CONFIGINFO 130
   #define CWO_SERVER 0

//   #define S4SERVER_GUI

   #ifdef S4COMTHREADS
      #include "process.h"
   #endif

   #ifdef __TURBOC__
      extern unsigned _stklen = 20000 ;
   #endif
   char StampedlicenceString[]="WARNING\n\nThe license for this software is a run-time license for use with application( s ) written \
by the Organization or Person whose name is specified below. The license does not allow this software to be used with any application \
programmatic database capabilities." ;

   char UnStampedString[]= "\n\nCodeBase Server is licensed for use in accordance with the CodeBase 6.5 license Agreement. \
\n\nThis license allows the CodeBase Server to be installed and run from one computer only. \
Contact Sequiter Software, Inc. for distribution information." ;

   SERVER4 *serverPtr = 0 ;
   Bool5 isService = false5;    // CS 2002/06/13

   // #ifdef S4ODBC_BUILD
   //  Bool5 serverInitialized = 0 ;   // used for Simba SQL know when CodeBase initialized...
   //  Bool5 SimbaServerRunning = 0 ;   // do we need to shut down the simba server on shutdown?
   // #endif
   CODE4 *c4 = 0 ;
   DATA4 *config ;
   char Cname[51], Serial[21], configBuff[255] ;
   #ifndef S4OFF_THREAD
      SEMAPHORE4 initConfig ;
   #endif
   FIELD4INFO configFields[]=
   {
      {"NAME", r4str, 50, 0 },
      {"SERIAL", r4str, 20, 0 },
      { 0, 0, 0, 0 },
   } ;

   // CS 2002/06/13 called by s4service.exe
   void S4FUNCTION setServer4global( SERVER4 *ptr )
   {
      serverPtr = ptr ;
      // AS July 29/02 - let's track a global server for error handling when no c4 present...
      //server4setServerPtr( ptr ) ;
   }

   // CS 2002/06/13 called by s4service.exe
   void S4FUNCTION setService( void )
   {
      isService = true5 ;
   }

   #if !defined( S4ODBC_BUILD ) && !defined( S4UNIX )
      HWND hWndGlobal, hWndAbout, hWndLicence, hWndConfig ;
      HINSTANCE hInst = NULL ;
      #ifdef S4SERVER_GUI
         #define TIMER_INTERVAL 2000  /* 2 seconds */
         HWND hUpTime, hRegistered, hSerialNo, hMaxUsers, hUsers, hConnections, hTables, hLocks, hAutoRecovery ;
         HWND hTranYes, hTranNo, hAdminButton, hExitButton ;
         time_t startTime ;
         UINT timer = 0 ;
      #endif

      // CS 2002/07/24 move here from d4service.c
      // Function: server4taskbarIcon
      // Description: Add or remove the server icon from the notification area.
      // Parameters:
      //    operation - What to do. Can be:
      //     NIM_ADD - Add the icon to the status area.
      //     NIM_DELETE - Delete the icon from the status area.
      //     NIM_MODIFY - Modify the icon in the status area.
      //    tip - The text that appears when the mouse is hovered over the
      //     icon. Maximum 63 characters. If NULL, the text
      //     "CodeBase Database Server" is used.
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
         #ifdef S4SERVER_GUI
            time( &startTime ) ;

            HWND hWndTmp ;

            /* Labels */
            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Server Up Time:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 15, 115, 15, hWndGlobal,
                      (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "User Information",
                      WS_CHILD | WS_VISIBLE | SS_CENTER, 130, 65, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Registered To:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 90, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Serial Number:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 118, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Max Users:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 160, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Backup Computer Logging:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 185, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Current",
                      WS_CHILD | WS_VISIBLE | SS_CENTER, 130, 215, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Succeeded",
                      WS_CHILD | WS_VISIBLE | SS_CENTER, 380, 215, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Failed",
                      WS_CHILD | WS_VISIBLE | SS_CENTER, 505, 215, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Users:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 240, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Transactions:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 255, 240, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Connections:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 265, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Tables:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 290, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hWndTmp = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "Data Locks:",
                      WS_CHILD | WS_VISIBLE | SS_RIGHT, 5, 315, 115, 15,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hWndTmp, WM_SETFONT, (WPARAM)systemFont, false ) ;


            /* Service Up Time */
            hUpTime = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                      WS_CHILD | WS_VISIBLE |  SS_LEFT, 130, 14, 460, 17,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hUpTime, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Registered To */
            hRegistered  = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                           WS_CHILD | WS_VISIBLE | WS_BORDER | SS_LEFT, 130, 89, 460, 17,
                           hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hRegistered, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Serial Number */
            hSerialNo    = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                           WS_CHILD | WS_VISIBLE | WS_BORDER | SS_LEFT, 130, 117, 460, 17,
                           hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hSerialNo, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Max Users */
            hMaxUsers    = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                           WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 130, 159, 230, 17,
                           hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hMaxUsers, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Auto Recovery Status */
            hAutoRecovery = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                            WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 130, 184, 230, 17,
                            hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hAutoRecovery, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Users */
            hUsers = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                     WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 130, 239, 115, 17,
                     hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hUsers, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Transactions */
            hTranYes = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                       WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 380, 239, 115, 17,
                       hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hTranYes, WM_SETFONT, (WPARAM)systemFont, false ) ;

            hTranNo  = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                       WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 505, 239, 115, 17,
                       hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hTranNo, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Connections */
            hConnections = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                           WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 130, 264, 115, 17,
                           hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hConnections, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Tables */
            hTables = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                      WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 130, 289, 115, 17,
                      hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hTables, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Data Locks */
            hLocks = CreateWindowEx( WS_EX_NOPARENTNOTIFY, "STATIC", "",
                     WS_CHILD | WS_VISIBLE | WS_BORDER | SS_RIGHT, 130, 314, 115, 17,
                     hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hLocks, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Start Administrator */
            hAdminButton = CreateWindowEx( NULL, "BUTTON", "Start Administrator",
                           WS_CHILD | WS_VISIBLE | WS_BORDER | BS_DEFPUSHBUTTON, 110, 375, 135, 35,
                           hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hAdminButton, WM_SETFONT, (WPARAM)systemFont, false ) ;

            /* Exit */
            // CS 2002/06/13 in service, "EXIT" button is labeled "Close", and does not terminate the server
            hExitButton = CreateWindowEx( NULL, "BUTTON", isService ? "Close" : "Exit",
                          WS_CHILD | WS_VISIBLE | WS_BORDER | BS_PUSHBUTTON, 325, 375, 135, 35,
                          hWndGlobal, (HMENU)NULL, hInstance, (LPVOID)NULL ) ;
            SendMessage( hExitButton, WM_SETFONT, (WPARAM)systemFont, false ) ;
         #endif
      }


      HWND S4FUNCTION InitInstance( HINSTANCE hInstance, int nCmdShow )
      {
         /*#ifdef S4WIN32  // CS 2002/07/22 moved this section from d4server.c
            DWORD num = 0 ;
            char exeName[MAX_PATH] ;
            char exePath[MAX_PATH] ;
            HMODULE hMod ;

            hMod = GetModuleHandle( 0 ) ;
            if ( hMod != NULL )
             num = GetModuleFileName( hMod, exeName, MAX_PATH ) ;

            if ( num == 0 || num >= LEN4PATH )
               return NULL ;
            else
            {
               u4namePath( exePath, MAX_PATH, exeName ) ;
               SetCurrentDirectory( exePath );    // CS 2001/05/09 set curdir to same as exe
            }
         #endif*/

         // CS 2002/07/11  Check for title suffix
         //int rc ;
         char windowTitle[256] = "CodeBase 6.5 Database Server" ;
         /*CODE4 c4temp ;
         FILE4 f4temp ;

         code4init( &c4temp ) ;
         c4setReadOnly( &c4temp, 0 ) ;
         c4setErrOpen( &c4temp, 0 ) ;
         // AS Mar 17/03 - should be using internal open...
         rc = file4openInternal( &f4temp, &c4temp, "TitleAppend.txt", 0, OPT4NONE ) ;
         if ( rc == r4success )
         {
            int startLen ;
            int appendLen ;
            strcat( windowTitle, " " ) ;
            startLen = strlen( windowTitle ) ;
            // AS Mar 17/03 - should be using internal read...
            FILE4LONG pos ;
            file4longAssign( pos, 0, 0 ) ;
            appendLen = file4readInternal( &f4temp, pos, windowTitle + startLen, 256 - startLen ) ;
            windowTitle[startLen + appendLen] = 0 ;
            file4close( &f4temp ) ;
         }
         code4initUndo( &c4temp ) ;*/

         // CS 2002/06/13 creates the server window
         // returns the server window handle, or NULL on error
         #ifdef S4SERVER_GUI
            hWndGlobal = CreateWindow( "TestWClass", windowTitle, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                                       650, 500, HWND_DESKTOP, ( HMENU )NULL, hInstance, ( LPVOID )NULL ) ;
            HGDIOBJ systemFont ;
            systemFont = GetStockObject( DEFAULT_GUI_FONT ) ;
         #else
            hWndGlobal = CreateWindow( "TestWClass", windowTitle, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                                        CW_USEDEFAULT, CW_USEDEFAULT, HWND_DESKTOP, ( HMENU )NULL, hInstance, ( LPVOID )NULL ) ;
         #endif

         if ( hWndGlobal == NULL )
            return NULL ;

         #ifdef E4DEBUG_INFO
            debug4displayInit( &debugDisplay, hWndGlobal ) ;
         #endif

         SetWindowLong( hWndGlobal, CWO_SERVER, 0L ) ;

         ShowWindow( hWndGlobal, nCmdShow ) ;
         UpdateWindow( hWndGlobal ) ;

         doServerGui( hInstance, systemFont ) ;
         return hWndGlobal ;
      }



      void initAboutDialog( HWND hWnd, LPARAM lParam )
      {
         char buffer[256], tmpStr[25], tmpStr3[30], tmpStrL[5] ;
         #ifdef S4WIN32
            MEMORYSTATUS memory ;
         #endif

         SetWindowLong( hWnd, DWL_USER, (long)(HMENU)lParam ) ;
         CODE4 *c4 = (CODE4 *)GetWindowLong( hWndGlobal, CWO_SERVER ) ;
         SendDlgItemMessage( hWnd, 102, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;
         wsprintf( buffer, "%s %d", tmpStr, 0 ) ;
         SendDlgItemMessage( hWnd, 102, WM_SETTEXT, 0, (LPARAM)( ( LPSTR )buffer ) ) ;
         SendDlgItemMessage( hWnd, 103, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;
         if ( c4 == 0 )
            wsprintf( buffer, "%s Unknown", tmpStr ) ;
         else
         {
            switch( r4cdx )
            {
               case r4cdx:
                  wsprintf( buffer, "%s FoxPro", tmpStr ) ;
                  break ;
               case r4ntx:
                  wsprintf( buffer, "%s Clipper", tmpStr ) ;
                  break ;
               case r4mdx:
                  wsprintf( buffer, "%s dBASE", tmpStr ) ;
                  break ;
               default:
                  wsprintf( buffer, "%s Unknown", tmpStr ) ;
                  break ;
            }
         }

         SendDlgItemMessage( hWnd, 103, WM_SETTEXT, 0, (LPARAM)( ( LPSTR )buffer ) ) ;

         SendDlgItemMessage( hWnd, 104, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;
         #ifdef S4WIN32
            GlobalMemoryStatus ( &memory ) ;
            wsprintf( buffer, "%s %ld KB", tmpStr, memory.dwAvailPhys/1024 ) ;
         #else
            wsprintf( buffer, "%s %ld KB", tmpStr, GetFreeSpace( 0 )/1024 ) ;
         #endif
         SendDlgItemMessage( hWnd, 104, WM_SETTEXT, sizeof( buffer )-1, (LPARAM)( ( LPSTR )buffer ) ) ;

         SendDlgItemMessage( hWnd, 105, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;
         #ifdef S4WIN32
            wsprintf( buffer, "%s %d%%", tmpStr, ( ( memory.dwAvailPhys * 100 ) / memory.dwTotalPhys ) ) ;
         #else
            wsprintf( buffer, "%s %d%%", tmpStr, GetFreeSystemResources( GFSR_SYSTEMRESOURCES ) ) ;
         #endif
         SendDlgItemMessage( hWnd, 105, WM_SETTEXT, sizeof( buffer )-1, (LPARAM)( ( LPSTR )buffer ) ) ;

         SendDlgItemMessage( hWnd, 109, WM_GETTEXT, sizeof( tmpStr3 )-1, (LPARAM)( ( LPSTR )tmpStr3 ) ) ;
         wsprintf( buffer, "%s %d", tmpStr3, 0 ) ;
         SendDlgItemMessage( hWnd, 109, WM_SETTEXT, sizeof( buffer )-1, (LPARAM)( ( LPSTR )buffer ) ) ;

         // 107 port number
         SendDlgItemMessage( hWnd, 107, WM_GETTEXT, sizeof( tmpStr3 )-1, (LPARAM)( ( LPSTR )tmpStr3 ) ) ;
         wsprintf( buffer, "%s %d", tmpStr3, 0 ) ;
         SendDlgItemMessage( hWnd, 107, WM_SETTEXT, sizeof( buffer )-1, (LPARAM)( ( LPSTR )buffer ) ) ;

         SendDlgItemMessage( hWnd, 106, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;
         if ( c4 == 0 )
            strcpy( tmpStrL,"Unknown" ) ;
         else
         {
               strcpy( tmpStrL,"OFF" ) ;
         }
         wsprintf( buffer, "%s %s", tmpStr, tmpStrL ) ;
         SendDlgItemMessage( hWnd, 106, WM_SETTEXT, sizeof( buffer )-1, (LPARAM)( ( LPSTR )buffer ) ) ;
      }



      LRESULT CALLBACK AboutProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
      {
         HMENU hMenu ;

         switch( message )
         {
            case WM_INITDIALOG:
               initAboutDialog( hWnd, lParam ) ;
               return TRUE ;

            case WM_COMMAND :
               switch(wParam)
               {
                  case IDOK :
                     hMenu = (HMENU)GetWindowLong( hWnd, DWL_USER ) ;
                     EnableMenuItem( hMenu, ID_SERVERINFO, MF_BYCOMMAND | MF_ENABLED ) ;
                     SendMessage( hWnd, WM_CLOSE, 0, 0L ) ;
                     return TRUE ;
                  default:
                     break ;
               }

               /* otherwise just fall through to close and destroy window */
            case WM_CLOSE :
               DestroyWindow( hWnd ) ;
               hWndAbout = 0 ;
               break ;

            default:
               break ;
         }

         return FALSE ;
      }



      static void initAboutLicense( HWND hWnd, LPARAM lParam )
      {
         char buffer[300], tmpStr[25] ;  // AS Sept 9/02 - increased buffer size to hold entire license.
         short security = 0 ;
         SetWindowLong( hWnd, DWL_USER, (long)(HMENU)lParam ) ;

         // CS 2002/06/19 display build number in About box
         // 124 Build number
         if ( 1234 == 0 )
            buffer[0] = 0 ;
         else
            sprintf( buffer, "Build %ld", 1234 ) ;
         SendDlgItemMessage( hWnd, 124, WM_SETTEXT, 0, (LPARAM)( ( LPSTR )buffer ) ) ;

         // 119 license text
         SendDlgItemMessage( hWnd, 119, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;

         if ( security >= 2 )
            wsprintf( buffer, "%s %s", tmpStr, StampedlicenceString ) ;
         else
            wsprintf( buffer, "NOTICE OF %s %s", tmpStr, UnStampedString ) ;

         SendDlgItemMessage( hWnd, 119, WM_SETTEXT, 0, (LPARAM)( ( LPSTR )buffer ) ) ;
         SendDlgItemMessage( hWnd, 120, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;

         if ( security >= 2 )
            wsprintf( buffer, "Application Developer %s", tmpStr ) ;
         else
            wsprintf( buffer, "Customer %s", tmpStr ) ;

         SendDlgItemMessage( hWnd, 120, WM_SETTEXT, 0, (LPARAM)( ( LPSTR )buffer ) ) ;
         SendDlgItemMessage( hWnd, 107, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;

         if ( security >= 2 )
            wsprintf( buffer, "%s  %s", tmpStr, "" ) ;
         else
            wsprintf( buffer, "%s  %s", tmpStr, Cname ) ;

         SendDlgItemMessage( hWnd, 107, WM_SETTEXT, sizeof( buffer )-1, (LPARAM)( ( LPSTR )buffer ) ) ;
         SendDlgItemMessage( hWnd, 108, WM_GETTEXT, sizeof( tmpStr )-1, (LPARAM)( ( LPSTR )tmpStr ) ) ;

         if ( security >= 2 )
            wsprintf( buffer, "%s Distribution Number:  %s", tmpStr, "" ) ;
         else
            wsprintf( buffer, "%s Serial Number: %s", tmpStr, Serial ) ;

         SendDlgItemMessage( hWnd, 108, WM_SETTEXT, sizeof( buffer )-1, (LPARAM)( ( LPSTR )buffer ) ) ;
      }



      LRESULT CALLBACK AboutLicence( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
      {
         HMENU hMenu ;

         switch( message )
         {
            case WM_INITDIALOG :
               initAboutLicense( hWnd, lParam ) ;
               return TRUE ;

            case WM_COMMAND :
               switch(wParam)
               {
                  case IDOK :
                     hMenu = (HMENU)GetWindowLong( hWnd, DWL_USER ) ;
                     EnableMenuItem( hMenu, IDM_ABOUT, MF_BYCOMMAND | MF_ENABLED ) ;
                     SendMessage( hWnd, WM_CLOSE, 0, 0L ) ;
                     return TRUE ;
                  default:
                     break ;
               }
               /* otherwise just close and destroy window */

            case WM_CLOSE :
               DestroyWindow( hWnd ) ;
               hWndLicence = 0 ;
               break ;

            default:
               break ;
         }
         return FALSE ;
      }



      #ifdef S4SERVER_GUI
         Bool5 g_tranEnabled = 0 ;  // track whether it was ever enabled

         void UpdateWindowGUI()
         {
            int days, hours, minutes, seconds ;
            char buff[100] ;
            time_t currentTime ;
            /* initialize these values with functions */
            long maxUsers, users, tranYes, tranNo, connections, tables, locks ;
            Bool5 tranEnabled ;

            /*if ( serverPtr != 0 )
            {
               maxUsers = server4maxUsers( serverPtr ) ;
               users = server4numClients( serverPtr ) ;

               tranYes = server4numTransCommitted( serverPtr ) ;      // # transactions successful
               tranNo = server4numTransRolledBack( serverPtr ) ;      // # transactions rolled back
               connections = users ;
               tables = server4numTables( serverPtr ) ;      // # of open tables ( physically ),
               locks = server4lockCount( serverPtr ) ;    // # of locks
               tranEnabled = server4autoRecovery( serverPtr ) ;
            }
            else*/
            {
               maxUsers = 0 ;
               users = 0 ;
               tranYes = 0 ;        // # transactions successful
               tranNo = 0 ;       // # transactions rolled back
               connections = 0 ;
               tables = 0 ;        // # of open tables
               locks = 0 ;         // # of locks
               tranEnabled = 0 ;
            }

            time( &currentTime ) ;
            seconds = ( int )currentTime - startTime ;
            minutes = seconds / 60 ;
            hours = minutes / 60 ;
            days = hours / 24 ;
            seconds = seconds % 60 ;
            minutes = minutes % 60 ;
            hours = hours % 24 ;  // CS 2001/03/01 24 hours in a day, not 60
            sprintf( buff, "%d Days, %d Hours, %d Minutes, %d Seconds", days, hours, minutes, seconds ) ;
            SetWindowText( hUpTime, buff ) ;

            // CS 2012/01/11 When connections are unlimited, Server window display "Unlimited" rather than 0.
            if (maxUsers == 0)
            {
               c4strcpy(buff, 100, "Unlimited");
            }
            else
            {
               ltoa( maxUsers, buff, 10 ) ;
            }
            SetWindowText( hMaxUsers, buff ) ;

            ltoa( users, buff, 10 ) ;
            SetWindowText( hUsers, buff ) ;

            ltoa( tranYes, buff, 10 ) ;
            SetWindowText( hTranYes, buff ) ;

            ltoa( tranNo, buff, 10 ) ;
            SetWindowText( hTranNo, buff ) ;

            ltoa( connections, buff, 10 ) ;
            SetWindowText( hConnections, buff ) ;

            ltoa( tables, buff, 10 ) ;
            SetWindowText( hTables, buff ) ;

            ltoa( locks, buff, 10 ) ;
            SetWindowText( hLocks, buff ) ;

            if ( tranEnabled == 0 )
            {
               if ( g_tranEnabled == 1 ) // means it was previously enabled, so report it as such
                  SetWindowText( hAutoRecovery, "*** Unexpectedly Disabled ***" ) ;
               else
                  SetWindowText( hAutoRecovery, "Disabled" ) ;
            }
            else
            {
               g_tranEnabled = 1 ;
               SetWindowText( hAutoRecovery, "Enabled" ) ;
            }
         }
      #endif



      LRESULT CALLBACK AboutConfig( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
      {

         switch( message )
         {
            case WM_INITDIALOG :
               return TRUE ;
            case WM_COMMAND :
               switch(wParam)
               {
                  case IDOK :
                     SendMessage( hWnd, WM_CLOSE, 0, 0L ) ;
                     return TRUE ;
                  default:
                     break ;
               }
               break ;
            case WM_CLOSE :
               /*{
                  int tempKeep = server4keepOpen( serverPtr, 0 ) ;
                  SendDlgItemMessage( hWnd, 125, WM_GETTEXT, sizeof( Cname )-1, (LPARAM)( ( LPSTR )Cname ) ) ;
                  SendDlgItemMessage( hWnd, 126, WM_GETTEXT, sizeof( Serial )-1, (LPARAM)( ( LPSTR )Serial ) ) ;
                  if ( strlen( Cname )==0 || strlen( Serial ) == 0 )
                  {
                     d4close( config ) ;
                     u4remove( configBuff ) ;
                  }
                  else
                  {
                     d4appendStart( config,0 ) ;
                     f4assign( d4field( config,"NAME" ), Cname ) ;
                     f4assign( d4field( config,"SERIAL" ), Serial ) ;
                     // AS Jul 17/03 - We need to call lock the append record as well as a code change was made for efficience to not do this in d4append(),
                     // but in D4append() instead.  We can't call D4append due to privilege tables not being open yet.
                     d4lockAppendRecord( config, 0 ) ;

                     d4append( config ) ;
                     #ifdef S4SERVER_GUI
                        SetWindowText( hRegistered, Cname ) ;
                        SetWindowText( hSerialNo, Serial ) ;
                        time( &startTime ) ;
                        UpdateWindowGUI() ;
                     #endif
                     d4close( config ) ;
                  }
                  server4keepOpen( serverPtr, tempKeep ) ;
               }*/
               #ifndef S4OFF_THREAD
                  //semaphore4release( &initConfig ) ;
                  ReleaseSemaphore( (&initConfig)->handle, 1, 0 );
               #endif
               DestroyWindow( hWnd ) ;
               hWndConfig = 0 ;
               break ;
            default:
               break ;
         }
         return FALSE ;
      }



      void menu4about( HWND hWnd, DLGPROC proc )
      {
         HINSTANCE hInstance ;
         HMENU hMenu ;

         hMenu = GetMenu( hWnd ) ;
         EnableMenuItem( hMenu, ID_SERVERINFO, MF_BYCOMMAND | MF_GRAYED ) ;

         #ifdef S4WIN32
            hInstance = ( HINSTANCE )GetWindowLong( hWnd, GWL_HINSTANCE ) ;
         #else
            hInstance = GetWindowWord( hWnd, GWW_HINSTANCE ) ;
         #endif
         hWndAbout = CreateDialogParam( hInstance, "DLGABOUT", hWnd, proc, (LPARAM)hMenu ) ;
         if ( hWndAbout == 0 )  // failed to create, re-enable the menu item
            EnableMenuItem( hMenu, ID_SERVERINFO, MF_BYCOMMAND | MF_ENABLED ) ;
      }



      void menu4licence( HWND hWnd, DLGPROC proc )
      {
         HINSTANCE hInstance ;
         HMENU hMenu ;

         hMenu = GetMenu( hWnd ) ;
         EnableMenuItem( hMenu, IDM_ABOUT, MF_BYCOMMAND | MF_GRAYED ) ;

         #ifdef S4WIN32
            hInstance = ( HINSTANCE )GetWindowLong( hWnd, GWL_HINSTANCE ) ;
         #else
            hInstance = GetWindowWord( hWnd, GWW_HINSTANCE ) ;
         #endif
         hWndLicence = CreateDialogParam( hInstance, "DLGABOUTLICENCE", hWnd, proc, (LPARAM)hMenu ) ;
      }



      void start4config( HWND hWnd, DLGPROC proc )
      {
         HINSTANCE hInstance ;

         #ifdef S4WIN32
            hInstance = ( HINSTANCE )GetWindowLong( hWnd, GWL_HINSTANCE ) ;
         #else
            hInstance = GetWindowWord( hWnd, GWW_HINSTANCE ) ;
         #endif
         hWndConfig = CreateDialogParam( hInstance, "DLGSTARTCONFIG", hWnd, proc, 0 ) ;
      }


      int server4getRegistryString( HKEY hKey, LPCTSTR lpSubKey, LPCTSTR lpValueName, LPTSTR data, const DWORD dataLen )
      {
         // CS 2002/07/25
         // Function: server4getRegistryString
         // Description: Get the value of a registry key.
         // Parameters:
         //    hKey: The primary key family, i.e. HKEY_CLASSES_ROOT,
         //     HKEY_CURRENT_CONFIG, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE,
         //     HKEY_USERS.
         //    lpSubKey: The name of the sub key. e.g. "Software\\Sequiter\\CodeBase Administrator"
         //    lpValueName: The name of the value. e.g. "BaseDir"
         //    data: Pointer to a buffer that receives the value's data.
         //    dataLen: The number of bytes that data points to.
         // Returns: On success, ERROR_SUCCESS.
         //    On error, a nonzero error code defined in WINERROR.H.
         HKEY hSubKey = NULL ;
         DWORD type, len = dataLen ;
         char path[MAX_PATH] = "" ;
         LONG rc ;

         rc = RegOpenKeyEx( hKey, lpSubKey, 0, KEY_QUERY_VALUE, &hSubKey ) ;

         if ( rc == ERROR_SUCCESS )
            rc = RegQueryValueEx( hSubKey, lpValueName, NULL, &type, ( LPBYTE )data, &len ) ;

         if ( hSubKey != NULL )
            RegCloseKey( hSubKey ) ;

         return rc ;
      }




      int server4setRegistryString( HKEY hKey, LPCTSTR lpSubKey, LPCTSTR lpValueName, LPCSTR data, const DWORD dataLen )
      {
         // CS 2002/07/25
         // Function: server4setRegistryString
         // Description: Create and set the value of a registry key.
         // Parameters:
         //    hKey: The primary key family, i.e. HKEY_CLASSES_ROOT,
         //     HKEY_CURRENT_CONFIG, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE,
         //     HKEY_USERS.
         //    lpSubKey: The name of the sub key. e.g. "Software\\Sequiter\\CodeBase Administrator"
         //    lpValueName: The name of the value. e.g. "BaseDir"
         //    data: Pointer to a buffer that contains the data for the value.
         //    dataLen: The number of bytes that data points to.
         // Returns: On success, ERROR_SUCCESS.
         //    On error, a nonzero error code defined in WINERROR.H.
         HKEY hSubKey = NULL ;
         DWORD type ;
         LONG rc ;

         rc = RegCreateKeyEx( hKey, lpSubKey, 0, "String", REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hSubKey, &type ) ;

         if ( rc == ERROR_SUCCESS )
            rc = RegSetValueEx( hSubKey, lpValueName, 0, REG_SZ, ( const LPBYTE )data, dataLen ) ;

         if ( hSubKey != NULL )
            RegCloseKey( hSubKey ) ;

         return rc ;
      }



      // CS 2002/07/25
      BOOL runProcess( LPTSTR lpCommandLine )
      {
         STARTUPINFO si ;
         PROCESS_INFORMATION pi ;

         ZeroMemory( &si, sizeof( si ) ) ;
         si.cb = sizeof( si ) ;

         return CreateProcess( NULL, lpCommandLine, NULL, NULL, false, 0, NULL, NULL, &si, &pi ) ;
      }



      void mainParentNotify( UINT wParam, LONG lParam, HWND hWnd )
      {
         switch( LOWORD(wParam) )
         {
            case WM_LBUTTONDOWN :
               if( HIWORD( lParam ) >= 375 && HIWORD( lParam ) <= 410 )
               {
                  if( LOWORD( lParam ) >= 110 && LOWORD( lParam ) <= 245 )
                  {
                     /* Start Administrator Button pressed */
                     char path[MAX_PATH] = "" ;

                     // CS 2002/07/25 look for the location of CBAdmin

                     // try in HKEY_CURRENT_USER
                     int rc = server4getRegistryString( HKEY_CURRENT_USER, "Software\\Sequiter\\CodeBase Administrator", "BaseDir", path, MAX_PATH ) ;
                     if ( rc != ERROR_SUCCESS )  // try in HKEY_LOCAL_MACHINE
                        rc = server4getRegistryString( HKEY_LOCAL_MACHINE, "Software\\Sequiter\\CodeBase Administrator", "BaseDir", path, MAX_PATH ) ;

                     if ( rc == ERROR_SUCCESS )
                     {
                        // run it
                        if ( path[strlen( path )-1] != '\\' )
                           strcat( path, "\\" );  // add backslash if not already there

                        strcat( path, "CBAdmin.exe" ) ;

                        if ( runProcess( path ) == 0 )
                           rc = GetLastError() ;
                     }

                     if ( rc != ERROR_SUCCESS )
                     {
                        // not found
                        path[0] = 0 ;
                        if( MessageBox( hWnd, "CodeBase Administrator could not be found. Would you like to find it yourself?",
                            "CBAdmin Not Found", MB_YESNO | MB_ICONINFORMATION ) == IDYES )
                        {
                           OPENFILENAME ofn ;
                           memset( &ofn, 0, sizeof( OPENFILENAME ) ) ;

                           ofn.lStructSize = sizeof( OPENFILENAME ) ;
                           ofn.hwndOwner = hWnd ;
                           ofn.lpstrFilter = "CodeBase Administrator\0CBADMIN.EXE\0" ;
                           ofn.nFilterIndex = 1 ;
                           ofn.lpstrFile = path ;
                           ofn.nMaxFile = MAX_PATH ;
                           ofn.Flags = OFN_EXPLORER | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY ;
                           ofn.lpstrTitle = "Locate CBAdmin.exe" ;
                           if( GetOpenFileName( &ofn ) )
                           {
                              server4setRegistryString( HKEY_LOCAL_MACHINE, "Software\\Sequiter\\CodeBase Administrator", "BaseDir", path, strlen( path ) - strlen( "\\CBAdmin.exe" ) ) ;
                              runProcess( path ) ;
                           }
                        } // if MessageBox
                     } // if rc
                  }
                  else if( LOWORD( lParam ) >= 325 && LOWORD( lParam ) <= 460 )
                  {
                     /* Exit Button pressed */
                     PostMessage( hWnd, WM_CLOSE, 0, 0L ) ;
                  }
               }
               break ;
            default :
               break ;
         }
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
               if ( isService && ( lParam == WM_LBUTTONUP || lParam == WM_MBUTTONUP || lParam == WM_RBUTTONUP ) )
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
                        timer = SetTimer( hWnd, 0, TIMER_INTERVAL, NULL ) ;
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
            case WM_ENDSESSION:
               {
                  #ifdef S4OLEDEBUG_PRINT
                    log5( "WM_ENDSESSION being called\r\n" ) ;
                  #endif
                  int count = 0 ;
                  for ( ;; )
                  {
                     /* wait up to 30 secs to give server chance to shutdown */
                     if ( serverPtr == 0 )  // we are shutdown
                        break ;
                     //u4delaySec() ;
                     if ( count++ == 30 )  // we have timed out, continue shutdown anyway
                     {
                        #ifdef S4OLEDEBUG_PRINT
                           log5( "count == 30, not done yet!\r\n" ) ;
                        #endif
                        break ;
                     }
                  }
               }
               break ;
            case WM_QUERYENDSESSION:
               #ifdef S4OLEDEBUG_PRINT
                  log5( "WM_QUERYENDSESSION being called\r\n" ) ;
               #endif
               if ( isService )
                  return TRUE ;
/*               else
               {
                  strcpy( buffer, "The CodeBase database server is still running. " ) ;
                  wsprintf( buffer+strlen( buffer ), "%d", 0 ) ;
                  strcat( buffer," clients still active. Click 'OK' to proceed with server shutdown or 'Cancel' to abort server shutdown." ) ;
                  if ( MessageBox( ( HWND )NULL, buffer, "Shutdown Warning", MB_OKCANCEL ) == IDOK )
                  {
                     // AS 03/28/01 - change to r4shutdown so it actually does shutdown ( instead of hard exit )
                     // server4setDoExit( serverPtr, r4exit ) ;
                     server4quit( serverPtr, r4shutdown ) ;
                     return TRUE ;
                  }
                  return 0 ;
               }*/
               return 0 ;
            case WM_QUIT :
            case WM_CLOSE:
               // AS Oct 24/02 - brought up to latest version
               //if (isService)
               {
                  // CS 2002/06/13 If running as a service, don't shut down the server; just hide the window.
                  ShowWindow( hWndGlobal, SW_HIDE ) ;
               }
               /*else
               {
                  if ( serverPtr != 0 )
                  {
                     #ifdef S4SERVER_GUI
                        if ( server4getDoExit( serverPtr ) == 0 )   // not already set to request shutdown (e.g. not codebase admin or error)
                        {
                           if( MessageBox( hWnd, "Are you sure you wish to close the CodeBase Server?", "Shutdown Warning", MB_YESNO ) == IDNO )
                              return 0 ;
                           KillTimer( hWnd, timer ) ;
                        }
                     #endif
                     server4setDoExit( serverPtr, r4exit ) ;
                  }
               }*/

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

               if (isService)
                  return 0;

               break ;
            default:
               if ( message == WM4_TASKBARCREATED )
                  server4taskbarIcon( NIM_ADD, NULL );    // CS 2002/07/24
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
                        //error4( 0, e4info, E70226 ) ;
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

            if ( serverPtr != 0 )
            {
               //server4initUndo( serverPtr ) ;
               //server4free( serverPtr ) ;
               //   serverPtr = 0 ;
               // AS July 29/02 - let's track a global server for error handling when no c4 present...
               setServer4global( 0 ) ;
            }

            // - AS 01/07/97 - hWndGlobal bad at this point already     SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
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
      // also used by simba implementation to initialize the server structures...
      /*c4 = code4alloc( 0 ) ;
      if ( c4 == 0 )
         return e4memory;

      serverPtr = server4alloc() ;
      if ( serverPtr == 0 )
      {
          code4initUndo( c4 ) ;  // frees up memory for c4 as well...
          c4 = 0 ;
          return e4memory;
      }
      else
      {
         // AS July 29/02 - let's track a global server for error handling when no c4 present...
         //server4setServerPtr( serverPtr ) ;
      }*/

      #if defined( S4ODBC_BUILD )
          HWND hWndGlobal = 0 ;
      #endif
      #if defined( S4UNIX )
          long hWndGlobal = 0 ;
      #endif
      /*if ( server4init( serverPtr, c4, configName, 0, (long)hWndGlobal ) < 0 )   // CS 2000/04/10 cast hwnd to long
      {
          //server4initUndo( serverPtr ) ;
          //server4free( serverPtr ) ;
          // AS July 29/02 - let's track a global server for error handling when no c4 present...
          setServer4global( 0 ) ;
          // serverPtr = 0 ;
          return -1 ;
      }*/

      return 0 ;
   }



   #ifndef S4ODBC_BUILD
      static int server4securityCheck( CODE4 *c4 )
      {
          /* CJ - This section of code determines where to place a splash screen to allow the user to enter in customer name and serial
          number upon startup.  If the Customer stamp has been initialized then the following code will not be executed. */
          /*if ( server4securityOption() < 2 )
          {
             int rc = server4initUser( true5 ) ;
             if ( rc != r4success )
                return rc ;

             #ifndef S4OFF_THREAD
                if ( !semaphore4wait( &initConfig, WAIT4EVER ) )
                    return -1 ;
                semaphore4release( &initConfig ) ;
                semaphore4initUndo( &initConfig ) ;
             #endif
             code4exitExclusive( c4, c4CatalogClient( c4 ) ) ;
             return 0 ;
          }

          strcpy( Cname, "" ) ;
          strcpy( Serial, "" ) ;
          #ifdef S4SERVER_GUI
             SetWindowText( hRegistered, Cname ) ;
             SetWindowText( hSerialNo, Serial ) ;
             time( &startTime ) ;
             UpdateWindowGUI() ;
          #endif*/

          return 0 ;
      }


      #ifdef S4COMTHREADS
         unsigned __stdcall d4server( void *cmdLineVd )
      #else
         unsigned d4server( void *cmdLineVd )
      #endif
      {
         int rc ;
         char *cmdLine = ( char * )cmdLineVd ;  /* must be void for _beginthread */
         #if !defined( S4COMTHREADS ) && !defined( S4UNIX )
            MSG msg ;
         #endif

         // AS 10/10/00 - Need to save and reset the server current directory path in order for the ODBC engine
         // to properly load if the server is restarted internally ( since it is located in same location and loaded directly )
         /*char tempPath[LEN4PATH], startPath[LEN4PATH] ;
         u4nameCurrent( tempPath, sizeof( tempPath ), "a" ) ;
         u4namePath( startPath, sizeof( startPath ), tempPath ) ;
         // remove the ending '\'
         int sLen = strlen( startPath ) ;
         if ( sLen > 0 && startPath[sLen-1] == '\\' )
            startPath[sLen-1] = 0 ;*/

         for ( ;; )
         {
            if ( d4serverInitialize( cmdLine ) != 0 )
            {
               #ifndef S4UNIX
                   SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
                   PostQuitMessage( 0 ) ;
               #endif
               return 0 ;
            }

            #ifndef S4UNIX
               SetWindowLong( hWndGlobal, CWO_SERVER, ( LONG )( c4 ) ) ;
            #endif

            rc = server4securityCheck( c4 ) ;
            /*if ( rc != 0 )
            {
               // AS Aug 15/01 - Display an error if the security check failed.
               //error4( c4, e4config, E70220 ) ;
               code4exitExclusive( c4, c4CatalogClient( c4 ) ) ;
               server4initUndo( serverPtr ) ;
               server4free( serverPtr ) ;
               serverPtr = 0 ;
               #ifndef S4UNIX
                   SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
                   PostQuitMessage( 0 ) ;
               #endif
               return 0 ;
            }*/

            #ifdef S4COMTHREADS
               while (1)
               {
                  Sleep(250);
               //rc = server4waitOnQuitOrExit( serverPtr ) ;
               }
               // CJ -06/09/99- Can not use assert5() as the Err5 objects are not exported in the S4DLL.
               //assert5( r4exit == 100 ) ;

               if ( rc == r4exit )  // means hard exit
               {
                   #ifdef S4OLEDEBUG_PRINT
                     log5( "d4server() hard exiting\r\n" ) ;
                   #endif
                   #ifndef S4UNIX
                      SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
                      PostQuitMessage( 0 ) ;
                   #endif
                   return 0 ;
               }

               if ( rc == r4restart )
               {
                  #ifdef S4OLEDEBUG_PRINT
                    log5( "d4server() restarting\r\n" ) ;
                  #endif
                  while ( 0 != 0 )  // wait for the requester to finish up
                  {
                     ;
                  }
               }
               else  // assume r4shutdown
                  rc = r4shutdown ;  // ensure we dont restart
            #else
               for( rc = 0 ; rc >= 0 ; )
               {
                  #ifndef S4UNIX
                     /* Give some other application a chance to run. */
                     if ( PeekMessage( &msg, 0, 0, 0, PM_REMOVE ) )
                     {
                        if ( hWndLicence == 0 || hWndAbout == 0 || !IsDialogMessage( hWndAbout, &msg )||!IsDialogMessage( hWndLicence, &msg ) )
                        {
                           TranslateMessage( &msg ) ;
                           DispatchMessage( &msg ) ;
                        }
                     }
                     #ifdef S4WIN32
                        /* Give up current time slice, and a bit */
                        Sleep( 250 ) ;
                     #else
                        /* allow device drivers to run... */
                        _asm mov ax, 0x1689
                        _asm mov bl, 1
                        _asm int 0x2f
                     #endif
                  #else
                     if ( connection4waitForWork( serverPtr ) < 0 )
                        return 0 ;
                  #endif

                  /*if ( server4getDoExit( serverPtr ) == 1 )
                  {
                     #ifndef S4UNIX
                        SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
                        PostQuitMessage( 0 ) ;
                     #endif
                     return 0 ;
                  }*/

/*                  if ( server4quit( serverPtr, -1 ) > 0 )
                  {
                     if ( server4quit( serverPtr, -1 ) == r4restart )
                     {
                        if ( 0 == 0 )
                        {
                           server4quit( serverPtr, 0 ) ;
                           rc = r4restart ;
                           break ;
                        }
                     }
                     else  // must be r4shutdown
                     {
                        rc = r4shutdown ;  // ensure we dont restart
                        break ;
                     }
                  }*/
               }
            #endif  /* S4COMTHREADS */

            #ifdef S4OLEDEBUG_PRINT
               log5( "d4server() preparing to exit\r\n" ) ;
            #endif

            /*if ( serverPtr != 0 )
            {
               #ifdef S4OLEDEBUG_PRINT
                 log5( "d4server() calling shutdown procedures for server\r\n" ) ;
               #endif
               server4initUndo( serverPtr ) ;
               server4free( serverPtr ) ;
               serverPtr = 0 ;
            }*/

            if ( rc != r4restart )
               break ;

            // reset the path as it was when the server started initially to allow proper loading of information
            /*int pathRc = u4pathSet( startPath ) ;
            if ( pathRc == -1 )
               error4describe( c4, e4config, E70102, "failed to set path", startPath, 0 ) ;
            if ( pathRc == -2 )
               error4describe( c4, e4config, E70102, "failed to set drive of path", startPath, 0 ) ;*/
         }
         #ifndef S4UNIX
            SendMessage( hWndGlobal, WM_CLOSE, 0, 0L ) ;
            PostQuitMessage( 0 ) ;
         #endif
         return 0 ;
      }



      int server4initUser( Bool5 prompt )
      {
      return 0;
      /*
         // CS 2002/06/11
         // prompt - If true, will prompt for registration info if required.
         //    If false, will never prompt for registration info.
         //    In a service application, this parameter will normally be false.
         int rc = r4success, temp ;
         #ifdef S4WIN32
            #ifndef S4OFF_THREAD
               semaphore4init( &initConfig ) ;
               semaphore4release( &initConfig ) ;
            #endif
            code4enterExclusive( c4, c4CatalogClient( c4 ), 1 ) ;
            u4nameCurrentExtended( configBuff, sizeof( configBuff ), "CONFIG4.dbf", server4systemPath( serverPtr ) ) ;
            int oldErrOpen = c4getErrOpen( c4 ) ;
            c4setErrOpen( c4, 0 ) ;
            config = d4open( c4, configBuff ) ;
            c4setErrOpen( c4, oldErrOpen ) ;
            if ( config == 0 )
            {
               if ( error4set( c4, 0 ) == r4noExist && prompt )
               {
                  config = d4create( c4, configBuff, configFields, 0 ) ;
                  if ( config != 0 )
                  {
                     #ifndef S4OFF_THREAD
                        if ( !semaphore4wait( &initConfig, 10 ) )
                           return e4result  ;
                     #endif
                     #ifndef S4UNIX
                        SendMessage( hWndGlobal, WM_COMMAND, ID_CONFIGINFO, 0L ) ;
                     #endif
                     return( rc ) ;
                  }
               }
               rc = r4noOpen ;
            }
            else
            {
               rc = d4top( config ) ;
               if ( rc == r4success )
               {
                  strcpy( Cname, f4str( d4field( config,"NAME" ) ) ) ;
                  strcpy( Serial, f4str( d4field( config,"SERIAL" ) ) ) ;
                  #ifdef S4SERVER_GUI
                     SetWindowText( hRegistered, Cname ) ;
                     SetWindowText( hSerialNo, Serial ) ;
                     time( &startTime ) ;
                     UpdateWindowGUI() ;
                  #endif
                  rc = 0 ;
               }
               else if( rc == r4eof && prompt )
               {
                  // This means the database exists but has no records therfore try and set the config again.
                  #ifndef S4OFF_THREAD
                     if ( !semaphore4wait( &initConfig, 10 ) )
                     {
                        return( e4result ) ;
                     }
                  #endif
                  #ifndef S4UNIX
                     SendMessage( hWndGlobal, WM_COMMAND, ID_CONFIGINFO, 0L ) ;
                  #endif
                  return( r4success ) ;
               }
               else
               {
                  rc = -1 ;
               }
               temp = server4keepOpen( serverPtr, 0 ) ;
               d4close( config ) ;
               config = 0 ;
               server4keepOpen( serverPtr, temp ) ;
            }
         #endif
         return rc ;*/
      }
   #endif /* !S4ODBC_BUILD */
#endif /* S4SERVER */

