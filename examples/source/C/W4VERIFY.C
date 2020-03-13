/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/*********************************************************************
w4verify.c   (c)Copyright Sequiter Software Inc., 1988-2001.
All rights reserved.

This example program is a Windows application similar in operation to
d4verify.c

*********************************************************************/

#include "d4all.h"
#include <windows.h>

CODE4 cb ;

/*********** Structure Declarations **********************************/

typedef struct
{
   #ifdef S4MEDIUM
      char *ptr ;
   #else
      LPSTR  ptr ;
   #endif
} D4PARSE_STR;

typedef struct
{
   HWND hWnd ;
   HINSTANCE hInst ;
   D4PARSE_STR parseStr ;
   int x,y ;
   TEXTMETRIC tm ;
   LPMSG lpmsg ;
   MSG msg ;  /* Last Message */
   int didClose ;
   int didQuit ;
   HCURSOR hSaveCursor ;
} D4DISPLAY;

FIELD4INFO fieldInfo[] =
{
   { "TEST_FIELD",'C',20,0 },
   { 0,0,0,0 },
};


/*********** Function Prototype and Global Variable Declarations *****/

HCURSOR hHourGlass, hSaveCursor ;
D4DISPLAY display ;
extern MSG msg ;
extern TEXTMETRIC tm ;

#ifdef S4WINCE
   int PASCAL WinMain(HINSTANCE, HINSTANCE, LPTSTR, int);
   void S4FUNCTION d4parseStringInit( D4PARSE_STR *, LPTSTR ) ;
   #define TextOut(a,b,c,d,e) ExtTextOut((a),(b),(c),0,NULL,(d),(e),NULL)
#else
   int PASCAL WinMain(HINSTANCE, HINSTANCE, LPSTR, int);
   void S4FUNCTION d4parseStringInit( D4PARSE_STR *, LPSTR ) ;
#endif
BOOL InitApplication(HINSTANCE) ;
BOOL InitInstance(HINSTANCE, int) ;
#ifdef S4WIN64
   LRESULT MainWndProc( HWND, UINT, WPARAM, LPARAM) ;
#else
   LRESULT CALLBACK MainWndProc(HWND, UINT, UINT, LONG) ;
#endif
void verifySwitches( void ) ;

void S4FUNCTION d4displayInit( HWND ) ;
void S4FUNCTION d4displayStr( char *, int ) ;
int S4FUNCTION d4displayQuit(void) ;


/*********** Windows initialization functions ************************/

BOOL InitApplication(HINSTANCE hInstance)
{
    WNDCLASS  wc;

    wc.style = (UINT) NULL;
    wc.lpfnWndProc = MainWndProc;

    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
       #ifdef S4WINCE
          wc.lpszMenuName =  NULL ;
          wc.hCursor = NULL ;
          wc.hIcon = NULL ; /* This is supported, but I don't know how to */
          wc.lpszClassName = L"TestWClass" ;
       #else
          wc.hCursor = LoadCursor( (HINSTANCE)NULL, IDC_ARROW ) ;
          wc.hIcon = (HICON)LoadIcon( (HINSTANCE)NULL, IDI_APPLICATION ) ;
          wc.lpszMenuName =  "TestMenu" ;
          wc.lpszClassName = "TestWClass" ;
       #endif

    return (RegisterClass(&wc));
}


BOOL InitInstance( HINSTANCE hInstance, int nCmdShow)
{
   HWND   hWnd;

   #ifdef S4WINCE
      hWnd = CreateWindow(
         L"TestWClass",
         L"Test CodeBase",
         WS_OVERLAPPED,
         CW_USEDEFAULT,
         CW_USEDEFAULT,
         CW_USEDEFAULT,
         CW_USEDEFAULT,
         (HWND) NULL,
         (HMENU) NULL,
         hInstance,
         NULL
      );
   #else
      hWnd = CreateWindow(
         "TestWClass",
         "Test CodeBase",
         WS_OVERLAPPEDWINDOW,
         CW_USEDEFAULT,
         CW_USEDEFAULT,
         CW_USEDEFAULT,
         CW_USEDEFAULT,
         (HWND) NULL,
         (HMENU) NULL,
         hInstance,
         NULL
      );
   #endif

   if (!hWnd)
      return (FALSE);

   SetTimer( hWnd, (UINT)hWnd, (UINT)1000, (TIMERPROC)NULL ) ;

   d4displayInit( hWnd ) ;
   display.hInst = hInstance ;     /* for t4filter.c */

   ShowWindow(hWnd, nCmdShow);
   UpdateWindow(hWnd);
   return (TRUE);
}

#ifdef S4WIN64
   LRESULT MainWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
#else
   LRESULT CALLBACK MainWndProc( HWND hWnd, UINT message, UINT wParam,
                             LONG lParam)
#endif
{
   switch (message)
   {
      case WM_DESTROY:
         PostQuitMessage(0);
         break;

      case WM_CLOSE:
         display.didClose =  1 ;
         return (DefWindowProc(hWnd, message, wParam, lParam));

      default:
         return (DefWindowProc(hWnd, message, wParam, lParam));
    }
    return (long) NULL;
}

/************ display text in Windows functions ***********************/

#ifndef S4WINCE
void S4FUNCTION d4parseStringInit( D4PARSE_STR *pStr, LPSTR p )
{
   memset( pStr, 0, sizeof(D4PARSE_STR) ) ;

   #ifdef S4MEDIUM
      lstrcpy( pointer, p ) ;
      pStr->ptr = pointer ;
   #else
      pStr->ptr =  p ;
   #endif
}
#else
void S4FUNCTION d4parseStringInit( D4PARSE_STR *pStr, LPTSTR p )
{
   memset( pStr, 0, sizeof(D4PARSE_STR) ) ;
   #ifdef S4MEDIUM
      lstrcpy( pointer, p ) ;
      pStr->ptr = pointer ;
   #else
      c4utoa(p) ;
      pStr->ptr = (char *)p ;
   #endif
}
#endif

void S4FUNCTION d4displayInit( HWND h )
{
   HDC hdc ;
   TEXTMETRIC tm ;

   memset( &display, 0, sizeof(D4DISPLAY) ) ;
   display.hWnd =  h ;
   display.lpmsg = &(display.msg) ;
   hdc =  GetDC(display.hWnd) ;
   GetTextMetrics( hdc, &tm ) ;
   display.tm = tm ;
   ReleaseDC(display.hWnd,hdc) ;
   display.x =  0 ;
   display.y =  0 ;
   display.didClose =  0 ;
   display.didQuit  =  0 ;
}

void  S4FUNCTION d4displayStr( char *str, int isNewLine )
{
   RECT rect ;
   HDC hdc ;
   SIZE size;
   int len, height, width ;
   #ifdef S4WINCE
      unsigned short blankLine[180], line[180], *tptr ;

      for ( tptr = blankLine; tptr<&blankLine[179]; *tptr++ = L' ' ) ;
      blankLine[179] = L'\0' ;
   #else
      char blankLine[180] ;

      memset(blankLine, 32, 179) ;
      blankLine[179] = '\0' ;
   #endif

   hdc =  GetDC(display.hWnd) ;
   len =   strlen(str) ;


   #ifdef S4WINCE
      c4atou(str, line, 180) ;
      GetTextExtentPoint32( hdc, line, len, &size ) ;
   #else
      GetTextExtentPoint( hdc, str, len, &size ) ;
   #endif

   height =  size.cy ;
   width  =  size.cx ;

   if ( isNewLine )
   {
      display.x  =  0 ;
      display.y +=  height +  display.tm.tmExternalLeading ;
   }

   GetClientRect( display.hWnd, &rect ) ;

   if ( (display.y+height) > rect.bottom )
   {
      display.y =  0 ;
      InvalidateRect( display.hWnd, &rect, 1 ) ;
   }

   #ifdef S4WINCE
      TextOut( hdc, display.x,display.y, line, len ) ;
   #else
      TextOut( hdc, display.x,display.y, str, len ) ;
   #endif

   if ( (display.y+(2*height)) > rect.bottom )
      TextOut( hdc, 0, 0, blankLine, 179 ) ;
   else
      TextOut( hdc, 0, (display.y+height+display.tm.tmExternalLeading), blankLine, 179 ) ;

   display.x +=  width ;

   ReleaseDC(display.hWnd,hdc) ;
}

int S4FUNCTION d4displayQuit( )
{
   /* If there is a message, the message is processed. */
   /* If the message says quit, then TRUE is returned. */
   if ( display.didQuit ) return 1 ;

   for (;;)
   {
      if ( ! display.didClose )
         if ( ! PeekMessage( display.lpmsg, display.hWnd, 0, 0, PM_NOREMOVE ) )
            return 0 ;

      if ( ! GetMessage( display.lpmsg, display.hWnd, 0, 0 ) )
      {
         #ifdef S4WINCE
            MessageBox( display.hWnd, L"", L"Program Completed", MB_OK ) ;
         #else
            MessageBox( display.hWnd, "", "Program Completed", MB_OK ) ;
         #endif
         display.didQuit =  1 ;
         return 1 ;
      }

      TranslateMessage(display.lpmsg);
      DispatchMessage(display.lpmsg);
   }
}

/*********************************************************************/


#ifdef S4WINCE
int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine,
#else
int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
#endif
                    int nCmdShow)
{
   struct timeb mStart, mEnd ;
   if (!hPrevInstance)
      if (!InitApplication(hInstance))
      {
         #ifdef S4WINCE
            MessageBox(0,L"",L"InitApplication Failed", MB_OK);
         #else
            MessageBox(0,"","InitApplication Failed", MB_OK);
         #endif
         return (FALSE);
      }

   if (!InitInstance(hInstance, nCmdShow))
   {
      #ifdef S4WINCE
         MessageBox(0, L"", L"InitInstance Failed", MB_OK);
      #else
         MessageBox(0, "", "InitInstance Failed", MB_OK);
      #endif
      return (FALSE);
   }
   d4parseStringInit( &display.parseStr, lpCmdLine ) ;

   code4init( &cb ) ;
   cb.hWnd = display.hWnd ;
   #ifdef S4DLL
      cb.hInst = display.hInst ;
   #endif

   verifySwitches() ;
   d4displayStr( "Switches Verified Successfully!", 1 ) ;

   cb.autoOpen = 0 ;
   cb.errOpen = 0 ;

   #ifdef S4CLIENT
      d4displayStr("Attempting implicit connection...", 1 ) ;
   #endif

   if( d4open( &cb, "TEST.DBF" ) != 0 )
      d4displayStr( "TEST.DBF Opened", 1 ) ;
   else
   {
      if( d4create(&cb,"TEST.DBF",fieldInfo,0) != 0 )
         d4displayStr( "TEST.DBF Created", 1) ;
   }
   code4close( &cb ) ;

   code4initUndo( &cb ) ;

   PostQuitMessage(0) ;

   ftime( &mStart ) ;
   for (;;)
   {
      double timeAmount ;
      ftime( &mEnd ) ;
      timeAmount = (double)mEnd.time +  - (double)mStart.time ;
      if ( timeAmount > 30 || d4displayQuit() )  // over 30 seconds, just terminate...
      {
         return (display.msg.wParam) ;
      }
      PostQuitMessage( 0 ) ;  // LY Oct 25/04 : try preventing test from hanging when PeekMessage() in d4displayQuit() returns non-zero
      // AS Oct 12/05 - at least in this case allow other applications to run...
      Sleep( 10 ) ;
   }
}

void verifySwitches( void )
{
   long d4switch ;

   d4switch = u4switch() ;

   d4displayStr( " ", 1 ) ;
   d4displayStr( " ", 1 ) ;
   d4displayStr( "Library Conditional Compilation Switches:", 1 ) ;
   if( d4switch & 1L )
      d4displayStr( "S4FOX - FoxPro Index File Compatibility", 1 ) ;
   if( d4switch & 2L )
      d4displayStr( "S4CLIPPER - Clipper Index File Compatibility", 1 ) ;
   if( d4switch & 4L )
      d4displayStr( "S4MDX - dBASE IV Index File Compatibility", 1 ) ;
   if( d4switch & 0x10L )
      d4displayStr( "S4DOS - DOS", 1 ) ;
   if( d4switch & 0x20L )
      d4displayStr( "S4WIN16 - Microsoft Windows 16-Bit Application", 1 ) ;
   if( d4switch & 0x40L )
      d4displayStr( "S4WIN32 - Microsoft Windows 32-Bit Application", 1 ) ;
   if( d4switch & 0x100L )
      d4displayStr( "S4OS2 - OS/2", 1 ) ;
   if( d4switch & 0x200L )
      d4displayStr( "S4UNIX - UNIX", 1 ) ;
   if( d4switch & 0x400L )
      d4displayStr( "S4MACINTOSH - MACINTOSH", 1 ) ;
   if( d4switch & 0x800L )
      d4displayStr( "S4PACAL_WIN - PASCAL Windows", 1 ) ;
   if( d4switch & 0x1000L )
      d4displayStr( "S4CB51 - CodeBase 5.1 compatibility", 1 ) ;
   if( d4switch & 0x2000L )
      d4displayStr( "S4SAFE  - Immediate File Length Updates", 1 ) ;
   if( d4switch & 0x4000L )
      d4displayStr( "S4LOCK_HOOK - Custom Lock Failure Function", 1 ) ;
   if( d4switch & 0x8000L )
      d4displayStr( "S4MAX - Maximum Memory Allocation Testing", 1 ) ;
   if( d4switch & 0x10000L )
      d4displayStr( "S4TIMEOUT_HOOK - Custom Timeout Hook Function", 1 ) ;
   if( d4switch & 0x20000L )
      d4displayStr( "E4ANALYZE - Structure Analysis CodeBase Error Checking", 1 ) ;
   if( d4switch & 0x40000L )
      d4displayStr( "E4DEBUG - Extended CodeBase Error Checking", 1 ) ;
   if( d4switch & 0x80000L )
      d4displayStr( "E4HOOK - Custom Error Function", 1 ) ;
   if( d4switch & 0x100000L )
      d4displayStr( "E4LINK - Link List CodeBase Error Checking", 1 ) ;
   if( d4switch & 0x200000L )
      d4displayStr( "E4MISC - Miscellaneous CodeBase Error Checking", 1 ) ;
   if( d4switch & 0x400000L )
      d4displayStr( "E4OFF - No CodeBase Error Display", 1 ) ;
   if( d4switch & 0x800000L )
      d4displayStr( "E4OFF_STRING - No Extended String CodeBase Error Display", 1 ) ;
   if( d4switch & 0x1000000L )
      d4displayStr( "E4PARM_HIGH - High-level Functional Parameter CodeBase Error Display", 1 ) ;
   if( d4switch & 0x2000000L )
      d4displayStr( "E4PAUSE - CodeBase Pause-on-error Enabled", 1 ) ;
   if( d4switch & 0x4000000L )
      d4displayStr( "E4STOP - CodeBase Stop-on-error Enabled", 1 ) ;
   if( d4switch & 0x8000000L )
      d4displayStr( "E4STOP_CRITICAL - CodeBase Stop-on-critical-error Enabled", 1 ) ;
   if( d4switch & 0x10000000L )
      d4displayStr( "S4OFF_INDEX - CodeBase Index Support Source Code Removed", 1 ) ;
   if( d4switch & 0x20000000L )
      d4displayStr( "S4OFF_MEMO - CodeBase Memo Support Source Code Removed", 1 ) ;
   if( d4switch & 0x40000000L )
      d4displayStr( "S4OFF_MULTI - CodeBase Multi-user Support Source Code Removed", 1 ) ;
   if( d4switch & 0x80000000L )
      d4displayStr( "S4OFF_OPTIMIZE - CodeBase Optimization Source Code Removed", 1 ) ;
   if( d4switch & 0x8L )
      d4displayStr( "S4CLIENT - CodeBase In Client/Server Mode", 1 ) ;
   if( d4switch & 0x80L )
      d4displayStr( "S4STAND_ALONE - CodeBase In Stand Alone Mode", 1 ) ;

/* no room for these switches
   if( d4switch & 0x100000000L )
      d4displayStr( "S4OFF_REPORT - CodeBase Reporter Source Code Removed", 1 ) ;
   if( d4switch & 0x200000000L )
      d4displayStr( "S4OFF_TRAN - CodeBase Transactions Support Source Code Removed", 1 ) ;
   if( d4switch & 0x400000000L )
      d4displayStr( "S4OFF_WRITE - CodeBase File Write Source Code Removed", 1 ) ;
*/

   #ifdef S4STAND_ALONE
      if( ( d4switch & 0x80L ) == 0 )
         error4describe( &cb, e4result, E87001, "S4STAND_ALONE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x80L )
         error4describe( &cb, e4result, E87001, "S4STAND_ALONE must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4CLIENT
      if( ( d4switch & 0x8L ) == 0 )
         error4describe( &cb, e4result, E87001, "S4CLIENT must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x8L )
         error4describe( &cb, e4result, E87001, "S4CLIENT must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_OPTIMIZE
      if( (d4switch & 0x80000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_OPTIMIZE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x80000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_OPTIMIZE must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_INDEX
      if( (d4switch & 0x10000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_INDEX must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x10000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_INDEX must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_MEMO
      if( (d4switch & 0x20000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_MEMO must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x20000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_MEMO must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_MULTI
      if( (d4switch & 0x40000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_MULTI must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x40000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_MULTI must be off in Library Build too.", 0, 0 ) ;
   #endif

/* no room for these switches
   #ifdef S4OFF_TRAN
      if( (d4switch & 0x200000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_TRAN must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x200000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_TRAN must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_WRITE
      if( (d4switch & 0x400000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_WRITE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x400000000L )
        L error4describe( &cb, e4result, E87001, "S4OFF_WRITE must be off in Library Build too.", 0, 0 ) ;
   #endif
*/

   #ifdef S4FOX
      if( (d4switch & 1L)  == 0 )
         error4describe( &cb, e4result, E87001, "S4FOX must be used in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4CLIPPER
      if( (d4switch & 2L)  == 0 )
         error4describe( &cb, e4result, E87001, "S4CLIPPER must be used in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4MDX
      if( (d4switch & 4L)  == 0 )
         error4describe( &cb, e4result, E87001, "S4MDX must be used in Library Build too.", 0, 0 ) ;
   #endif
}
