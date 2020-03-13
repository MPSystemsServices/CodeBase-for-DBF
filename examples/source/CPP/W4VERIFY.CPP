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
w4verify.cpp   (c)Copyright Sequiter Software Inc., 1988-2001.
All rights reserved.

This example program is a Windows application similar in operation to
d4verify.cpp

*********************************************************************/

#include "d4all.hpp"
#include <windows.h>


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
   HWND        hWnd ;
   HINSTANCE   hInst ;
   D4PARSE_STR parse_str ;
   int         x,y ;
   TEXTMETRIC  tm ;
   LPMSG       lpmsg ;
   MSG         msg ;  /* Last Message */
   int         did_close ;
   int         did_quit ;
   HCURSOR     hSaveCursor ;
} D4DISPLAY;

FIELD4INFO fieldInfo[] =
{
   { "TEST_FIELD",'C',20,0 },
   { 0,0,0,0 },
};

Code4 cb ;

/*********** Function Prototype and Global Variable Declarations *****/

HCURSOR hHourGlass, hSaveCursor ;
D4DISPLAY display ;
extern MSG msg ;
extern TEXTMETRIC tm ;

int PASCAL WinMain(HINSTANCE, HINSTANCE, LPSTR, int) ;
BOOL InitApplication(HINSTANCE) ;
BOOL InitInstance(HINSTANCE, int) ;
#ifdef S4WIN64
   LRESULT MainWndProc( HWND, UINT, WPARAM, LPARAM) ;
#else
   LRESULT CALLBACK MainWndProc(HWND, UINT, UINT, LONG) ;
#endif
extern void verifySwitches() ;

void S4FUNCTION d4displayInit( HWND ) ;
void S4FUNCTION d4parseStringInit( D4PARSE_STR *, LPSTR ) ;
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
    wc.hIcon = LoadIcon( (HINSTANCE) NULL, IDI_APPLICATION);
    wc.hCursor = LoadCursor( (HINSTANCE) NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wc.lpszMenuName =  "TestMenu";
    wc.lpszClassName = "TestWClass";

    return (RegisterClass(&wc));
}


BOOL InitInstance( HINSTANCE hInstance, int nCmdShow)
{
   HWND   hWnd;

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
         display.did_close =  1 ;
         return (DefWindowProc(hWnd, message, wParam, lParam));

      default:
         return (DefWindowProc(hWnd, message, wParam, lParam));
    }
    return (long) NULL;
}

/************ display text in Windows functions ***********************/

void S4FUNCTION d4parseStringInit( D4PARSE_STR *p_str, LPSTR p )
{
   memset( p_str, 0, sizeof(D4PARSE_STR) ) ;

   #ifdef S4MEDIUM
      lstrcpy( pointer, p ) ;
      p_str->ptr = pointer ;
   #else
      p_str->ptr =  p ;
   #endif
}

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
   display.did_close =  0 ;
   display.did_quit  =  0 ;
}

void  S4FUNCTION d4displayStr( char *str, int is_new_line )
{
   RECT rect ;
   HDC hdc ;
   SIZE size;
   int len, height, width ;
   char blank_line[180] ;

   memset(blank_line, 32, 179) ;
   blank_line[179] = '\0' ;

   hdc =  GetDC(display.hWnd) ;
   len =   strlen(str) ;

   GetTextExtentPoint( hdc, str, len, &size ) ;

   height =  size.cy ;
   width  =  size.cx ;

   if ( is_new_line )
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

   TextOut( hdc, display.x,display.y, str, len ) ;

   if ( (display.y+(2*height)) > rect.bottom )
      TextOut( hdc, 0, 0, blank_line, strlen(blank_line) ) ;
   else
      TextOut( hdc, 0, (display.y+height+display.tm.tmExternalLeading), blank_line, strlen(blank_line) ) ;

   display.x +=  width ;

   ReleaseDC(display.hWnd,hdc) ;
}

int S4FUNCTION d4displayQuit( )
{
   /* If there is a message, the message is processed. */
   /* If the message says quit, then TRUE is returned. */
   if ( display.did_quit ) return 1 ;

   for (;;)
   {
      if ( ! display.did_close )
         if ( ! PeekMessage( display.lpmsg, display.hWnd, 0, 0, PM_NOREMOVE ) )
            return 0 ;

      if ( ! GetMessage( display.lpmsg, display.hWnd, 0, 0 ) )
      {
         MessageBox( display.hWnd, "", "Program Completed", MB_OK ) ;
         display.did_quit =  1 ;
         return 1 ;
      }

      TranslateMessage(display.lpmsg);
      DispatchMessage(display.lpmsg);
   }
}

/*********************************************************************/


int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
                    int nCmdShow)
{
   Data4 data ;

   if (!hPrevInstance)
      if (!InitApplication(hInstance)){
         MessageBox(0,"","InitApplication Failed", MB_OK);
         return (FALSE);
      }

   if (!InitInstance(hInstance, nCmdShow)){
      MessageBox(0, "", "InitInstance Failed", MB_OK);
      return (FALSE);
   }
   d4parseStringInit( &display.parse_str, lpCmdLine ) ;

   cb.hWnd = display.hWnd ;
   #ifdef S4DLL
      cb.hInst = display.hInst ;
   #endif

   verifySwitches() ;
   d4displayStr( "Switches Verified Successfully!", 1 ) ;
   d4displayStr( " ", 1 ) ;

   cb.errOpen = 0 ;
   #ifdef N4OTHER
      cb.autoOpen = 0 ;
   #endif

   #ifdef S4CLIENT
      d4displayStr(" ", 1) ;
      d4displayStr ("Attempting implicit connection...", 1 ) ;
   #endif

   if( data.open( cb, "TEST.DBF" ) == r4success )
      d4displayStr( "TEST.DBF Opened", 1 ) ;
   else
   {
      /*
      Note: if we don't reset the Code4::errorCode member to zero,
      data.create() will return r4noOpen if successful, instead of
      r4success. This is because the previous data.open() call set
      the member value to r4noOpen, which is a non-error condition value,
      based on our setting the errOpen member to zero before calling
      data.open(). And since the upcoming data.create() function will not
      reset the errCode member if it is sucessful, the member will
      retain this value of r4noOpen. */

      cb.errorCode = 0;    /* or use cb.errorSet() function */

      if( data.create( cb,"TEST.DBF",fieldInfo) == r4success )
         d4displayStr( "TEST.DBF Created", 1 ) ;
   }

   cb.closeAll( ) ;
   cb.initUndo( ) ;

   PostQuitMessage(0) ;

   for (;;)
   {
       if (d4displayQuit() )
          return (display.msg.wParam) ;
   }
}

void verifySwitches()
{
   long d4switch ;

   d4switch = u4switch() ;

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
         error4describe( &cb, e4result, 87001, "S4STAND_ALONE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x80L )
         error4describe( &cb, e4result, 87001, "S4STAND_ALONE must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4CLIENT
      if( ( d4switch & 0x8L ) == 0 )
         error4describe( &cb, e4result, 87001, "S4CLIENT must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x8L )
         error4describe( &cb, e4result, 87001, "S4CLIENT must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_OPTIMIZE
      if( (d4switch & 0x80000000L )  ==  0 )
         error4describe( &cb, e4result, 87001, "S4OFF_OPTIMIZE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x80000000L )
         error4describe( &cb, e4result, 87001, "S4OFF_OPTIMIZE must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_INDEX
      if( (d4switch & 0x10000000L )  ==  0 )
         error4describe( &cb, e4result, 87001, "S4OFF_INDEX must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x10000000L )
         error4describe( &cb, e4result, 87001, "S4OFF_INDEX must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_MEMO
      if( (d4switch & 0x20000000L )  ==  0 )
         error4describe( &cb, e4result, 87001, "S4OFF_MEMO must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x20000000L )
         error4describe( &cb, e4result, 87001, "S4OFF_MEMO must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_MULTI
      if( (d4switch & 0x40000000L )  ==  0 )
         error4describe( &cb, e4result, 87001, "S4OFF_MULTI must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x40000000L )
         error4describe( &cb, e4result, 87001, "S4OFF_MULTI must be off in Library Build too.", 0, 0 ) ;
   #endif

/*
   #ifdef S4OFF_TRAN
      if( (d4switch & 0x200000000L )  ==  0 )
         error4describe( &cb, e4result, 87001, "S4OFF_TRAN must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x200000000L )
         error4describe( &cb, e4result, 87001, "S4OFF_TRAN must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_WRITE
      if( (d4switch & 0x400000000L )  ==  0 )
         error4describe( &cb, e4result, 87001, "S4OFF_WRITE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x400000000L )
         error4describe( &cb, e4result, 87001, "S4OFF_WRITE must be off in Library Build too.", 0, 0 ) ;
   #endif
*/

   #ifdef S4FOX
      if( (d4switch & 1L)  == 0 )
         error4describe( &cb, e4result, 87001, "S4FOX must be used in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4CLIPPER
      if( (d4switch & 2L)  == 0 )
         error4describe( &cb, e4result, 87001, "S4CLIPPER must be used in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4MDX
      if( (d4switch & 4L)  == 0 )
         error4describe( &cb, e4result, 87001, "S4MDX must be used in Library Build too.", 0, 0 ) ;
   #endif
}
