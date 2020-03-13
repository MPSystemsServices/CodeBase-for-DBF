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

/* r4drive2.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4OFF_REPORT

#ifdef S4WINDOWS
   extern HINSTANCE hInst;
   BOOL bUserAbort;
   HWND hDlgPrint;
#endif


#ifdef S4WINDOWS
/**************************************************************************
*  Print Abort Proc
**************************************************************************/
#ifdef P4ARGS_USED
   #pragma argsused
#endif
#ifndef S4WIN32
BOOL CALLBACK _export MPrintAbortProc( HDC hDC, int nCode )
#else
BOOL CALLBACK MPrintAbortProc( HDC hDC, int nCode )
#endif
{
   MSG msg;

   while( !bUserAbort && PeekMessage( &msg, (HWND)NULL, 0, 0, PM_REMOVE ) )
   {
      if( !hDlgPrint || !IsDialogMessage( hDlgPrint, &msg ) )
      {
         TranslateMessage( &msg );
         DispatchMessage( &msg );
      }
   }
   return !bUserAbort;
}

/***************************************************************************
* dialog proc for the dialog that comes up during printing and allows the
* user to cancel the print job
***************************************************************************/
#ifndef S4WIN32
BOOL CALLBACK _export PrintDialogProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#else
BOOL CALLBACK PrintDialogProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#endif
{
   REPORT4 *report;
   WORD cmd;

   switch (message)
   {
      case WM_INITDIALOG:
         /* store the report pointer in the windows extra bytes */
         SetWindowLong( hWnd, DWL_USER, lParam );
         report = (REPORT4 *)GetWindowLong( hWnd, DWL_USER );
         if( !report )
            break;

         /* set the window caption to the report name */
         if( lstrlen(report->report_caption ) != 0 )
            SetWindowText( hWnd, report->report_caption );  // CS 2001/06/05
         else
         {
            if( report->report_name )
               SetWindowText( hWnd, report->report_name );
            else
               SetWindowText( hWnd, TEXT("CodeReporter 2.0") );
         }
         EnableMenuItem( GetSystemMenu(hWnd,FALSE), SC_CLOSE, MF_GRAYED );
         SetFocus( GetDlgItem( hWnd, 101 ) );
         ReleaseCapture();
         return TRUE;

      case WM_COMMAND:
        cmd = GET_WM_COMMAND_ID( wParam, lParam );

         switch( cmd )
         {
            /* cancel the print job */
            case 101:
               bUserAbort = TRUE;
               EnableWindow( GetParent(hWnd), TRUE );
               DestroyWindow( hWnd );
               hDlgPrint = 0;
               return TRUE;
         }
         break;
   }

   return FALSE;
}

/**************************************************************************
*  This is a winproc for the transparent window which is created while
* processing the relation, it's only purpose is to absorb mouse clicks that
* an impatient user might make
**************************************************************************/
#ifndef S4WIN32
long CALLBACK _export MouseEatProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#else
long CALLBACK MouseEatProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#endif
{
   switch( message )
   {
      case WM_CREATE:
         BringWindowToTop( hWnd );
         break;
   }
   return(DefWindowProc( hWnd, message, wParam, lParam ) );
}


/**************************************************************************
* WinProc for the page window. Responsible for painting the appropriate
* portion of the page bitmap into the page window
**************************************************************************/
#ifndef S4WIN32
long CALLBACK _export OutputPageProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#else
long CALLBACK OutputPageProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#endif
{
   HDC hDC, bDC;
   PAINTSTRUCT ps;
   RECT rect;
   int xoffset, yoffset;
   HBITMAP hBit;
   BITMAP bm;
   SCROLLINFO sInfo;

   sInfo.cbSize = sizeof(sInfo);

   switch( message )
   {
      case WM_PAINT:
         hDC = BeginPaint( hWnd, &ps );
         bDC = (HDC)MGetWinP( hWnd, PGWIN_OFF_BDC );
         hBit = (HBITMAP)MGetWinP( hWnd, PGWIN_OFF_HBIT );
         GetObject( (HBITMAP)hBit, sizeof(BITMAP), &bm ) ;

         GetClientRect( hWnd, &rect ) ;

         //xoffset = GetScrollPos( GetParent(hWnd), SB_HORZ );
         sInfo.fMask = SIF_POS;
         GetScrollInfo( GetParent(hWnd), SB_HORZ, &sInfo );
         xoffset = sInfo.nPos;

         yoffset = MGetWinP( hWnd, PGWIN_OFF_YOFF );

         BitBlt( hDC, ps.rcPaint.left, ps.rcPaint.top, (ps.rcPaint.right-ps.rcPaint.left),
            (ps.rcPaint.bottom-ps.rcPaint.top), bDC, xoffset+ps.rcPaint.left, yoffset+ps.rcPaint.top, SRCCOPY );

         ValidateRect( hWnd, 0 ) ;
         EndPaint( hWnd, &ps );
         return 0;
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

/***************************************************************************
* WinProc for the output window: this window contains the page window and
* handles the scroll functionality, also the next page and close functionality
***************************************************************************/
#ifndef S4WIN32
long CALLBACK _export PreViewProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#else
long CALLBACK PreViewProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
#endif
{
   HWND pagewin;
   REPORT4 *report;
   RECT rect;
   int min, max, pos;
   HDC bDC;
   int xpos, width, height;
   HBITMAP hBit;
   BITMAP bm;
   int scrollinc, temp;
   WORD cmd;
   HCURSOR hCursor ;
   SCROLLINFO sInfo;

   sInfo.cbSize = sizeof(sInfo);

   switch( message )
   {
      case WM_CREATE:

         SetWindowLong( hWnd, PWIN_OFF_REPORT, (LONG)((CREATESTRUCT *)lParam)->lpCreateParams );
         report = (REPORT4 *)GetWindowLong( hWnd, PWIN_OFF_REPORT );
         /* destroy the transparent window */
         if( report->hWnd2 )
         {
            ReleaseCapture();
            DestroyWindow( report->hWnd2 );
            report->hWnd2 = (HWND)NULL;
         }
         break;

      case WM_CLOSE:
         /* enable the output windows parent and shift the focus */
         EnableWindow( GetParent(hWnd), TRUE );
         report = (REPORT4 *)GetWindowLong( hWnd, PWIN_OFF_REPORT );
         if( report && report->hWnd )
         {
            SetFocus( report->hWnd );
         }

         /* destroy the memory device context, bitmap and the window */
         bDC = (HDC)MGetWinP( hWnd, PWIN_OFF_BDC );
         hBit = (HBITMAP)SelectObject( bDC, (HBITMAP)MGetWinP( hWnd, PWIN_OFF_OLDOBJ ) );
         DeleteDC( bDC );
         DeleteObject( hBit );
         DestroyWindow( hWnd );

         /* post a message to the parent window indicating it is safe to free
            the report */
         PostMessage( report->hWnd, CRM_REPORTCLOSED, 0, 0L );
         break;

      case WM_SIZE:
         /* adjust the scroll ranges and the position of the page window */
         report = (REPORT4 *)GetWindowLong( hWnd, PWIN_OFF_REPORT );
         if( !report )
            break;
         GetClientRect( hWnd, &rect );
         if( report->dev_page_width <= rect.right )
         {
           xpos =(int)(rect.right - (int)report->dev_page_width)/2;
            width = (int)report->dev_page_width;
            SetScrollRange( hWnd, SB_HORZ, 0, 0, TRUE );
         }
         else
         {
            width = rect.right;
            xpos = 0;
            SetScrollRange( hWnd, SB_HORZ, 0, (int)(report->dev_page_width-rect.right), TRUE );
         }

         if( report->dev_page_height <= rect.bottom )
         {
            height = (int)report->dev_page_height;
            SetScrollRange( hWnd, SB_VERT, 0, 0, TRUE );
         }
         else
         {
            height = rect.bottom;
            SetScrollRange( hWnd, SB_VERT, 0, (int)(report->dev_page_height-rect.bottom), TRUE );
         }
         GetClientRect( hWnd, &rect );
         if( report->dev_page_width <= rect.right )
         {
            xpos =(int)(rect.right - (int)report->dev_page_width)/2;
            width = (int)report->dev_page_width;
            SetScrollRange( hWnd, SB_HORZ, 0, 0, TRUE );
         }
         else
         {
            width = rect.right;
            xpos = 0;
            SetScrollRange( hWnd, SB_HORZ, 0, (int)(report->dev_page_width-rect.right), TRUE );
         }

         if( report->dev_page_height <= rect.bottom )
         {
            height = (int)report->dev_page_height;
            SetScrollRange( hWnd, SB_VERT, 0, 0, TRUE );
         }
         else
         {
            height = rect.bottom;
            SetScrollRange( hWnd, SB_VERT, 0, (int)(report->dev_page_height-rect.bottom), TRUE );
         }
         pagewin = (HWND)MGetWinP( hWnd, PWIN_OFF_PGWIN );
         if( pagewin )
            SetWindowPos( pagewin, HWND_TOP, xpos, 0, width, height, SWP_NOZORDER );
         break;

      case WM_VSCROLL:
         /* adjust the scroll position and inform the page window*/
         report = (REPORT4 *)GetWindowLong( hWnd, PWIN_OFF_REPORT );
         if( !report )
            break;
         cmd = GET_WM_COMMAND_ID( wParam, lParam );

         //GetScrollRange( hWnd, SB_VERT, &min, &max );  // CS 2001/06/26 not supported in CE
         sInfo.fMask = SIF_RANGE;
         GetScrollInfo( hWnd, SB_VERT, &sInfo);
         min = sInfo.nMin;
         max = sInfo.nMax;

         //pos = GetScrollPos( hWnd, SB_VERT );  // CS 2001/06/26 not supported in CE
         sInfo.fMask = SIF_POS;
         GetScrollInfo( hWnd, SB_VERT, &sInfo );
         pos = sInfo.nPos;

         pagewin = (HWND)MGetWinP( hWnd, PWIN_OFF_PGWIN );
         GetClientRect( pagewin, &rect );
         switch( cmd )
         {
            case SB_PAGEDOWN:
               scrollinc = rect.bottom;
               break;

            case SB_PAGEUP:
               scrollinc = -1 * rect.bottom;
               break;

            case SB_LINEDOWN:
               scrollinc = 8;
               break;

            case SB_LINEUP:
               scrollinc = -8;
               break;

            case SB_THUMBPOSITION:
               scrollinc = GET_WM_COMMAND_CMD( wParam, lParam ) - pos;
               break;

            default:
               scrollinc = 0;
               break;
         }

         temp = MIN5( scrollinc, max - pos );
         scrollinc = MAX5( -1*pos, temp );

         if( scrollinc != 0 )
         {
            pos += scrollinc;
            MSetWinP( pagewin, PGWIN_OFF_YOFF, pos );
            //ScrollWindow( pagewin, 0, -1*scrollinc, NULL, NULL );  // CS 2001/06/26 not supported in CE
            ScrollWindowEx( pagewin, 0, -1*scrollinc, NULL, NULL, 0, 0, 0 );
            SetScrollPos( hWnd, SB_VERT, pos, TRUE );
         }
         return 0;

      case WM_HSCROLL:
         /* adjust the scroll position and inform the page window*/
         report = (REPORT4 *)GetWindowLong( hWnd, PWIN_OFF_REPORT );
         if( !report )
            break;
         cmd = GET_WM_COMMAND_ID( wParam, lParam );

         //GetScrollRange( hWnd, SB_HORZ, &min, &max );  // CS 2001/06/26 not supported in CE
         sInfo.fMask = SIF_RANGE;
         GetScrollInfo( hWnd, SB_HORZ, &sInfo );
         min = sInfo.nMin;
         max = sInfo.nMax;

         //pos = GetScrollPos( hWnd, SB_HORZ );  // CS 2001/06/26 not supported in CE
         sInfo.fMask = SIF_POS;
         GetScrollInfo( hWnd, SB_HORZ, &sInfo );
         pos = sInfo.nPos;

         pagewin = (HWND)MGetWinP( hWnd, PWIN_OFF_PGWIN );
         GetClientRect( pagewin, &rect );
         switch( wParam )
         {
            case SB_PAGEDOWN:
               scrollinc = rect.bottom;
               break;

            case SB_PAGEUP:
               scrollinc = -1 * rect.bottom;
               break;

            case SB_LINEDOWN:
               scrollinc = 8;
               break;

            case SB_LINEUP:
               scrollinc = -8;
               break;

            case SB_THUMBPOSITION:
               scrollinc = GET_WM_COMMAND_CMD( wParam, lParam ) - pos;
               break;

            default:
               scrollinc = 0;
               break;
         }
         temp = MIN5( scrollinc, max - pos );
         scrollinc = MAX5( -1*pos, temp );

         if( scrollinc != 0 )
         {
            pos += scrollinc;
            //ScrollWindow( pagewin, -1*scrollinc, 0, NULL, NULL );  // CS 2001/06/26 not supported in CE
            ScrollWindowEx( pagewin, -1*scrollinc, 0, NULL, NULL, 0, 0, 0 );
            SetScrollPos( hWnd, SB_HORZ, pos, TRUE );
         }
         return 0;

      case WM_COMMAND:
         cmd = GET_WM_COMMAND_ID( wParam, lParam );
         switch( cmd )
            {
            case 111: /* next page */
               /* generate the next page of the report into the bitmap, and
                  update the page window */
               report = (REPORT4 *)GetWindowLong( hWnd, PWIN_OFF_REPORT );
               if( !report )
                  break;
               pagewin = (HWND)MGetWinP( hWnd, PWIN_OFF_PGWIN );
               GetClientRect( pagewin, &rect );
               hBit = (HBITMAP)MGetWinP( hWnd, PWIN_OFF_HBIT );
               bDC = (HDC)MGetWinP( hWnd, PWIN_OFF_BDC );
               GetObject( (HBITMAP)hBit, sizeof(BITMAP), &bm ) ;

               PatBlt( bDC, 0, 0, bm.bmWidth, bm.bmHeight, WHITENESS ) ;
               GetClientRect( hWnd, &rect );
               MSetWinP( pagewin, PGWIN_OFF_YOFF, 0 );
               SetScrollPos( hWnd, SB_VERT, 0, TRUE );
               SetScrollPos( hWnd, SB_HORZ, 0, TRUE );
               hCursor = SetCursor( LoadCursor( (HINSTANCE)NULL, IDC_WAIT ) ) ;
               ShowCursor( TRUE ) ;
               report4generatePage( report, bDC );
               ShowCursor( FALSE ) ;
               SetCursor( hCursor ) ;
               if( report->end_report == 2 )
               {
                  //EnableMenuItem( GetMenu(hWnd), 111, MF_BYCOMMAND | MF_GRAYED );
                  EnableMenuItem( report->hMenu, 111, MF_BYCOMMAND | MF_GRAYED );
                  DrawMenuBar( hWnd );
                  if( !report->output_flag )
                     return 0;
               }
               InvalidateRect( hWnd, NULL, TRUE );
               break;

            case 444: /* close */
               /* reset the report members and destroy the output window */
               report = (REPORT4 *)GetWindowLong( hWnd, PWIN_OFF_REPORT );
               report->group_on = report->group_first = NULL;
               report->area_on = NULL;
               report->break_height = 0L;
               report->in_header = report->end_report = report->broken = 0;
               report->output_code = 1;
               report->dev_page_height = report->dev_page_width = 0;
               report->disp_bottom = report->ypos = 0;

               EnableWindow( GetParent(hWnd), TRUE );
               SetFocus( GetParent(hWnd) );
               bDC = (HDC)MGetWinP( hWnd, PWIN_OFF_BDC );
               hBit = (HBITMAP)SelectObject( bDC, (HBITMAP)MGetWinP( hWnd, PWIN_OFF_OLDOBJ ) );
               DeleteDC( bDC );
               DeleteObject( hBit );
               DestroyWindow( hWnd );
               PostMessage( report->hWnd, CRM_REPORTCLOSED, 0, 0L );
               return 0;
            }
            break;
      }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

/***************************************************************************
* creates the output and page windows, sets appropriate scroll ranges and
* generates the first page of the report
***************************************************************************/
int report4output_window( REPORT4 *report )
{
   HWND outputwindow, pagewin, hWnd;
   int width, height, xpos;
   RECT rect;
   HDC hDC, pDC, bDC;
   POINT pt, pt2;
   int num_bits, planes;
   HBITMAP hBit, hOld;
   BITMAP bm;
   LPTSTR tptr;
   HCURSOR hCursor ;
   HINSTANCE hInst;
   DWORD dwWindowStyle;

   //width = GetSystemMetrics( SM_CXFULLSCREEN ) ;
   //height = GetSystemMetrics( SM_CYFULLSCREEN ) ;
   SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
   width = rect.right - rect.left;
   height = rect.bottom - rect.top;

   report->hMenu = CreateMenu();
   AppendMenu( report->hMenu, MF_ENABLED | MF_STRING, 111, S4_REP_MENU_NEXT);
   AppendMenu( report->hMenu, MF_ENABLED | MF_STRING, 444, S4_REP_MENU_CLOSE);
   if( report->report_caption && lstrlen(report->report_caption) > 0 )
      tptr = report->report_caption;
   else
   {
      if( report->report_name && lstrlen(report->report_name) != 0 )
         tptr = report->report_name;
      else
         tptr = TEXT("CodeReporter 2.0");
   }

   dwWindowStyle = WS_POPUP | WS_CAPTION | WS_HSCROLL | WS_VSCROLL | WS_VISIBLE | WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_SYSMENU;
   #ifndef S4WINCE
      dwWindowStyle |= WS_MAXIMIZE | WS_THICKFRAME;
   #endif
   #if defined(S4WINCE)
      hInst = report->codeBase->hInst;
   #elif defined(S4WIN32)
      hInst = (HINSTANCE)GetWindowLong( report->hWnd, GWL_HINSTANCE);
   #else
      hInst = (HINSTANCE)GetWindowWord( (HWND)report->hWnd, GWW_HINSTANCE);
   #endif

   outputwindow = CreateWindow( TEXT("PreViewWin"), tptr, dwWindowStyle,
      0, 0, width, height, report->hWnd, report->hMenu, hInst, (LPVOID)report );
   if( !outputwindow )
   {
      error4describe( report->codeBase, e4repWin, 0L, E4_REP_OUTWIN, 0, 0 );
      return -1;
   }

   #ifdef S4WIN32
      SetClassLong( outputwindow, GCL_HCURSOR, (LONG)((HCURSOR)LoadCursor((HINSTANCE)NULL, IDC_ARROW)) );
   #else
      SetClassWord( (HWND)outputwindow, (int)GCW_HCURSOR, (WORD)(HCURSOR)LoadCursor((HINSTANCE)NULL, IDC_ARROW) );
   #endif
   SetCursor( LoadCursor( (HINSTANCE)NULL, IDC_ARROW ) );

   SetScrollRange( outputwindow, SB_HORZ, 0, 0, TRUE );
   SetScrollRange( outputwindow, SB_VERT, 0, 0, TRUE );

   hWnd = outputwindow;

   hDC = GetDC( hWnd );
   #ifndef S4WINCE
      SetMapMode( hDC, MM_HIENGLISH );
      SetMapMode( hDC, MM_ANISOTROPIC );
   #endif

   report4calc_obj_dev( report, hDC );

   pt.x = (int)(report->report_width +
          report->margin_left +
     report->margin_right);

   LPtoDP( hDC, &pt, 1 );

   if( report->screen_breaks )
   {
      if( !report->printerDC )
      {
         pDC = report4get_printerIC( report );
         pt.y = GetDeviceCaps( pDC, VERTSIZE );
         DeleteDC( pDC );
      }
      else
         pt.y = GetDeviceCaps( report->printerDC, VERTSIZE );
      pt2.y = (int)(((float)pt.y/25.4)*1000.0);
      pt2.x = 0;
      LPtoDP( hDC, &pt2, 1 );
      pt.y = (pt2.y < 0)?-1*pt2.y:pt2.y;
   }
   else
   {
      GetClientRect( hWnd, &rect );
      pt.y = rect.bottom;
   }

   bDC = CreateCompatibleDC( hDC );
   num_bits = GetDeviceCaps( hDC, BITSPIXEL );
   planes = GetDeviceCaps( hDC, PLANES );
   hBit = CreateBitmap( pt.x, pt.y, planes, num_bits, 0 );
   MSetWinP( hWnd, PWIN_OFF_BDC, bDC );
   MSetWinP( hWnd, PWIN_OFF_HBIT, hBit );
   hOld = (HBITMAP)SelectObject( bDC, hBit );
   MSetWinP( hWnd, PWIN_OFF_OLDOBJ, hOld );

   report->dev_page_height = pt.y;
   report->dev_page_width = pt.x;

   GetClientRect( hWnd, &rect );
   if( pt.x <= rect.right )
   {
      xpos = (rect.right - pt.x)/2;
      width = pt.x;
      SetScrollRange( hWnd, SB_HORZ, 0, 0, TRUE );
   }
   else
   {
      width = rect.right;
      xpos = 0;
      SetScrollRange( hWnd, SB_HORZ, 0, (pt.x-rect.right), TRUE );
   }

   if( pt.y <= rect.bottom )
   {
      height = pt.y;
      SetScrollRange( hWnd, SB_VERT, 0, 0, TRUE );
   }
   else
   {
      height = rect.bottom;
      SetScrollRange( hWnd, SB_VERT, 0, pt.y-rect.bottom, TRUE );
   }

   #if defined(S4WINCE)
      hInst = report->codeBase->hInst;
   #elif defined(S4WIN32)
      hInst = (HINSTANCE)GetWindowLong( report->hWnd, GWL_HINSTANCE);
   #else
      hInst = (HINSTANCE)GetWindowWord( (HWND)report->hWnd, GWW_HINSTANCE);
   #endif
   pagewin = CreateWindow( TEXT("OutputPageWin"), TEXT(""), WS_CHILD|WS_VISIBLE,
      xpos, 0, width, height,
      hWnd, NULL, hInst, (LPVOID)report );

   ReleaseDC( hWnd, hDC );

   if( !pagewin )
   {
      error4describe( report->codeBase, e4repWin, 0L, E4_REP_PAGEWIN, 0, 0 );
      return -1;
   }

   MSetWinP( pagewin, PGWIN_OFF_BDC, bDC );
   MSetWinP( pagewin, PGWIN_OFF_HBIT, hBit );
   MSetWinP( hWnd, PWIN_OFF_PGWIN, pagewin );


   #ifdef S4WIN32
      SetClassLong( pagewin, GCL_HCURSOR, (LONG)((HCURSOR)LoadCursor(NULL,IDC_ARROW)) );
   #else
      SetClassWord( (HWND)pagewin, (int)GCW_HCURSOR, (WORD)((HCURSOR)LoadCursor((HWND)NULL,IDC_ARROW)) );
   #endif
   SetCursor( LoadCursor((HINSTANCE)NULL,IDC_ARROW) );

   GetObject( (HBITMAP)hBit, sizeof( BITMAP ), &bm );
   PatBlt( bDC, 0, 0, bm.bmWidth, bm.bmHeight, WHITENESS );
   hCursor = SetCursor( LoadCursor( (HINSTANCE)NULL, IDC_WAIT ) ) ;
   ShowCursor( TRUE ) ;
   report4generatePage( report, bDC );
   ShowCursor( FALSE ) ;
   SetCursor( hCursor ) ;
   if( report->end_report == 2 )
   {
      //EnableMenuItem( GetMenu(hWnd), 111, MF_BYCOMMAND | MF_GRAYED );
      EnableMenuItem( report->hMenu, 111, MF_BYCOMMAND | MF_GRAYED );
      DrawMenuBar( hWnd );
   }
   InvalidateRect( pagewin, NULL, TRUE );

   if( MGetWinP(outputwindow,PWIN_OFF_BDC) == 0 || MGetWinP(outputwindow,PWIN_OFF_HBIT) == 0 )
   {
      DestroyWindow( outputwindow );
      return -1;
   }

   EnableWindow( report->hWnd, FALSE );

   return 0;
}
#endif /* #ifdef S4WINDOWS */


/************************************************************************
* convert a double val into the appropriate text string based on the
* formatting info in the object structure
*************************************************************************/
int report4conv_double( OBJ4 *obj, double doub_val, char *ptr )
{
   int     sig_digs = 0, sign_val, n_dec, pos, left, first_few, i, tdec;
   REPORT4 *report;
   char    *result, tbuf[11];

   #ifdef S4DEBUG
      if( !obj || !ptr )
         {
         return -1;
         }
   #endif

   report = obj->area->report;
   if( !obj->display_zero )
      if( doub_val == 0.0 )
         return 0 ;

   pos = 0 ;
   n_dec = tdec = obj->dec ;
   if( obj->numeric_type == obj4numPercent )
      n_dec += 2 ;

   if( obj->area->report->decimal_point == '\0' )
      obj->dec = 0;

   #ifdef S4NO_FCVT
      result =  f4fcvt( doub_val, n_dec, (int *)&sig_digs, (int *)&sign_val) ;
   #else
      // AS Dec 13/05 - under Windows fcvt is becoming deprecated...
      #ifdef S4WINDOWS_VS5_PLUS
         char buffer[50] ;
         _fcvt_s( buffer, sizeof( buffer ), doub_val, n_dec, (int *)&sig_digs, (int *)&sign_val) ;
         result = buffer ;
      #else
         result = fcvt( doub_val, n_dec, (int *)&sig_digs, (int *)&sign_val) ;
      #endif
   #endif

   if( result[0] == '0' )
      sig_digs = 0 ;

   if( obj->numeric_type == obj4numExponent )
   {
      if( sign_val )
      {
         if( obj->use_brackets )
            ptr[pos++] = '(' ;
         else
             ptr[pos++] = '-' ;
      }
      ptr[pos++] =  *result++ ;
      ptr[pos++] =  report->decimal_point ;
      if(obj->dec)
      {
         if( obj->dec <= (int)strlen(result)-1 )
            left = u4ncpy( ptr+pos, result, obj->dec+1 );
         else
            left = u4ncpy( ptr+pos, result, strlen(result) );
      }
      else
         left = 0;
      pos +=  left ;
      ptr[pos++] = 'e';
      memset( tbuf, 0, sizeof(tbuf) );
      c4ltoa45( sig_digs-1, tbuf, 10 ) ;
      while( tbuf[0] == ' ' )
      {
         for( i = 0; i < (int)strlen(tbuf); i++ )
            tbuf[i] = tbuf[i+1];
      }
       // AS Dec 13/05 vs 5.0 fixes
      // strcpy( ptr + pos, tbuf ) ;
      memcpy( ptr + pos, tbuf, strlen( tbuf ) + 1 ) ;
      pos +=  strlen( tbuf );
      obj->dec = tdec;
      return pos ;
   }

   if( obj->numeric_type == obj4numPercent )
   {
      if( doub_val < 0.01 && obj->dec == 0 )
      {
         sig_digs = 1;
         if( sign_val )
            sig_digs++ ;
      }
      else
         sig_digs += 2 ;
      n_dec -= 2 ;
   }

   if( n_dec <= -sig_digs )
      sign_val =  0 ;

   if( sign_val )
   {
      if( obj->use_brackets )
         ptr[pos++] = '(' ;
      else
          ptr[pos++] = '-' ;
   }

   if( obj->numeric_type == obj4numCurrency )
   {
      for( i = 0; i < (int)strlen( report->currency_symbol ); i++ )
         ptr[pos++] =  report->currency_symbol[i] ;
   }

   if( sig_digs > 0 )
   {
      first_few =  sig_digs % 3 ;
      if( first_few == 0 && sig_digs > 0 )
      first_few = 3 ;
      memcpy( ptr+pos, result, first_few ) ;
      pos+= first_few ;
      result +=  first_few ;
      sig_digs -=  first_few ;

      for(; sig_digs > 0; sig_digs -= 3 )
      {
         if( report->thousand_separator )
            ptr[pos++] =  report->thousand_separator ;

         memcpy( ptr+pos, result, 3 ) ;
         pos+= 3 ;
         result += 3 ;
      }

      if( obj->dec > 0 )
         ptr[pos++] =  report->decimal_point ;

      memcpy( ptr+pos, result, obj->dec ) ;
      pos +=  obj->dec ;
   }
   else
   {
      if( obj->leading_zero )
         ptr[pos++] = '0' ;

      if( obj->dec > 0 )
      {
         ptr[pos++] =  report->decimal_point ;
         memset( ptr+pos, '0', -sig_digs ) ;
         if( obj->dec > -sig_digs )
            memcpy( ptr+pos+ (-sig_digs), result, obj->dec - (-sig_digs) ) ;
         pos +=  obj->dec ;
      }

      if( pos == 0 )
         ptr[pos++] = '0' ;  /* Ensure at least a single zero */
   }

   if( sign_val  &&  obj->use_brackets )
      ptr[pos++] =  ')' ;

   if( obj->numeric_type == obj4numPercent )
      ptr[pos++] =  '%' ;

   obj->dec = tdec;
   return pos ;
}


/***************************************************************************
*
***************************************************************************/
int report4check_lookahead_object( OBJ4 *obj )
{
   OBJ4 *obj_on;

   if( obj->lookahead && obj->obj_type_num != obj4type_total )
      return 1;

   obj_on = (OBJ4 *)l4first( &obj->contained );
   while( obj_on )
   {
      if( report4check_lookahead_object( obj_on ) == 1 )
         return 1;
      obj_on = (OBJ4 *)l4next( &obj->contained, obj_on );
   }
   return 0;
}

/*************************************************************************
*
*************************************************************************/
void report4check_lookahead( REPORT4 *report )
{
   GROUP4 *group_on;
   AREA4 *area_on;
   OBJ4 *obj_on;

   group_on = (GROUP4 *)l4first( &report->groups );
   while( group_on )
   {
      group_on->lookahead = 0;

      area_on = (AREA4 *)l4first( &group_on->header_areas );
      while( area_on )
      {
         obj_on = (OBJ4 *)l4first( &area_on->objects );
         while( obj_on )
         {
            if( report4check_lookahead_object( obj_on ) == 1 )
               group_on->lookahead = 1;
            obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
         }
         area_on = (AREA4 *)l4next( &group_on->header_areas, area_on );
      }

      area_on = (AREA4 *)l4first( &group_on->footer_areas );
      while( area_on )
      {
         obj_on = (OBJ4 *)l4first( &area_on->objects );
         while( obj_on )
         {
            if( report4check_lookahead_object( obj_on ) == 1 )
               group_on->lookahead = 1;
            obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
         }
         area_on = (AREA4 *)l4next( &group_on->footer_areas, area_on );
      }

      group_on = (GROUP4 *)l4next( &report->groups, group_on );
   }
}

/***************************************************************************
*
***************************************************************************/
void obj4evaluate_lookahead( OBJ4 *obj )
{
   OBJ4 *obj_on;

   if( obj->lookahead )
   {
      if( obj->obj_type_num == obj4type_total )
         total4value_update( (TOTAL4 *)obj->data );
      obj4evaluate( obj );
   }

   obj_on = (OBJ4 *)l4first( &obj->contained );
   while( obj_on )
   {
      obj4evaluate_lookahead( obj_on );
      obj_on = (OBJ4 *)l4next( &obj->contained, obj_on );
   }
}

/***************************************************************************
*
***************************************************************************/
void group4evaluate_lookahead( GROUP4 *group )
{
   OBJ4 *obj_on;
   AREA4 *area_on;

   area_on = (AREA4 *)l4first( &group->header_areas );
   while( area_on )
   {
      obj_on = (OBJ4 *)l4first( &area_on->objects );
      while( obj_on )
      {
         obj4evaluate_lookahead( obj_on );
         obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
      }
      area_on = (AREA4 *)l4next( &group->header_areas, area_on );
   }

   area_on = (AREA4 *)l4first( &group->footer_areas );
   while( area_on )
   {
      obj_on = (OBJ4 *)l4first( &area_on->objects );
      while( obj_on )
      {
         obj4evaluate_lookahead( obj_on );
         obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
      }
      area_on = (AREA4 *)l4next( &group->footer_areas, area_on );
   }

}


/***************************************************************************
*
***************************************************************************/
void report4evaluate_lookahead( GROUP4 *group )
{
   long reccount = 0;
   char *ptr;
   int len, flag;
   PGROUP4 group_on;
   PREPORT4 report;

   if( !group )
      return;

   report = group->report;

   group_on = NULL;
   do
   {
      group_on = (PGROUP4)l4next( &report->groups, group_on );

      if( group_on->lastResetValue )
      {
         #ifdef S4WINDOWS
            group_on->held_reset_value = (LPTSTR)u4alloc( (lstrlen( group->lastResetValue ) + 1) * sizeof(TCHAR) );
         #else
            group_on->held_reset_value = (char *)u4alloc( strlen( group->lastResetValue ) + 1 );
         #endif
         if( !group_on->held_reset_value )
            return;

         if( group_on->held_reset_value )
         {
            #ifdef S4WINDOWS
               lstrcpy( group_on->held_reset_value, group_on->lastResetValue );
            #else
               strcpy( group_on->held_reset_value, group_on->lastResetValue );
            #endif
         }
      }
   } while( group_on != group );

   group4evaluate_lookahead( group );

   if ( group->resetExpression != NULL )
   {
      flag = 1;
      while( flag )
      {
         if( relate4skip( group->report->relate, 1L ) != 0 )
         {
            reccount++;
            break;
         }

         reccount++;

         group_on = NULL;
         do
         {
            group_on = (PGROUP4)l4next( &report->groups, group_on );

            if( group_on->resetExpression )
            {
               len = expr4vary( group->resetExpression, &ptr );
               if( c4memcmp( group->lastResetValue, ptr, len ) != 0 )
                  flag = 0;

               #ifdef S4UNICODE
                  c4atou(ptr, group->lastResetValue, (len + 1) * sizeof(TCHAR));
               #else
                  u4ncpy( group->lastResetValue, ptr, len + 1 );
               #endif
            }
         } while( group_on != group );

         if( flag )
            group4evaluate_lookahead( group );
      }
   }

   group_on = NULL;
   do
   {
      group_on = (PGROUP4)l4next( &report->groups, group_on );

      if( group_on->held_reset_value )
      {
         #ifdef S4WINDOWS
            lstrcpy( group_on->lastResetValue, group_on->held_reset_value );
         #else
            strcpy( group_on->lastResetValue, group_on->held_reset_value );
         #endif

         u4free( group_on->held_reset_value );
         group_on->held_reset_value = NULL;
      }
   } while( group_on != group );

   relate4skip( group->report->relate, (long)(-1 * reccount) );
}

/***************************************************************************
*
***************************************************************************/
int report4check_display_once_object( OBJ4 *obj )
{
   OBJ4 *obj_on;

   if( obj->display_once )
   {
      if( obj->last_display_val )
      {
         u4free( obj->last_display_val );
         obj->last_display_val = NULL;
      }

      if( obj->display_once_expr )
      {
         obj->last_display_val = (char *)
            u4allocFree(obj->area->report->codeBase, expr4len(obj->display_once_expr)+1);
      }
      else
      {
         obj->display_once = 0;
      }

      if( !obj->last_display_val )
         obj->display_once = 0;
   }

   obj_on = (OBJ4 *)l4first( &obj->contained );
   while( obj_on )
   {
      report4check_display_once_object( obj_on );
      obj_on = (OBJ4 *)l4next( &obj->contained, obj_on );
   }
   return 0;
}

/***************************************************************************
*
***************************************************************************/
void report4check_display_once( REPORT4 *report )
{
   GROUP4 *group_on;
   AREA4 *area_on;
   OBJ4 *obj_on;

   group_on = (GROUP4 *)l4first( &report->groups );
   while( group_on )
   {

      area_on = (AREA4 *)l4first( &group_on->header_areas );
      while( area_on )
      {
         obj_on = (OBJ4 *)l4first( &area_on->objects );
         while( obj_on )
         {
            if( report4check_display_once_object( obj_on ) == 1 )
               group_on->lookahead = 1;
            obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
         }
         area_on = (AREA4 *)l4next( &group_on->header_areas, area_on );
      }

      area_on = (AREA4 *)l4first( &group_on->footer_areas );
      while( area_on )
      {
         obj_on = (OBJ4 *)l4first( &area_on->objects );
         while( obj_on )
         {
            if( report4check_display_once_object( obj_on ) == 1 )
               group_on->lookahead = 1;
            obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
         }
         area_on = (AREA4 *)l4next( &group_on->footer_areas, area_on );
      }

      group_on = (GROUP4 *)l4next( &report->groups, group_on );
   }
}

#ifdef S4WINDOWS
/***************************************************************************
* this is the high level report output function. it initialises the various
* report members and then creates the appropriate output windows
***************************************************************************/
int S4FUNCTION report4do( REPORT4 *report )
{
   //char   *tptr, title[250];
   LPTSTR tptr;
   TCHAR title[250];
   ABORTPROC lpfnAbortProc;
   DLGPROC lpfnMPrintDlgProc;
   //LPSTR dialogtemplate;
   LPDLGTEMPLATE dialogtemplate;
   #ifndef S4WIN32
      GLOBALHANDLE dialoghandle;
   #endif
   DOCINFO di ;
   POINT pt;
   #ifdef S4WIN32
      HINSTANCE hInst;
   #endif

   #ifdef E4PARM_HIGH
      if( report == 0 )
         return error4( 0, e4parm_null, E95702 ) ;
   #endif

   if( (report4register_classes( report )) < 0 )
      return -1;

   if( report4pageInit( report ) < 0 )
      return -1;

   if( report->output_code == 1 )
   {
      if( (report4output_window( report )) < 0 )
         return -1;
   }
   else
   {
      EnableWindow( report->hWnd, FALSE );
      bUserAbort = FALSE;

      if( !report->printerDC )
        report->printerDC = report4get_printerDC( );

      SaveDC( report->printerDC );

      #ifndef S4WINCE
         SetMapMode( report->printerDC, MM_HIENGLISH );
         SetMapMode( report->printerDC, MM_ANISOTROPIC );
      #endif

      pt.x =(int)( report->report_width +
             (report->margin_left - report->hard_margin_left) +
        (report->margin_right - report->hard_margin_right));
      LPtoDP( report->printerDC, &pt, 1 );
      report->dev_page_width = pt.x;
      report->dev_page_height = GetDeviceCaps( report->printerDC, VERTRES );

      report4output_screen_fonts( report );

      report4calc_obj_dev( report, report->printerDC );

      RestoreDC( report->printerDC, -1 );

      #ifdef S4WIN32
         #if __BORLANDC__  // LY May 13/04 : remove < 0x550 to avoid compiler error for all Borland compilers
            lpfnMPrintDlgProc = (DLGPROC)MakeProcInstance( (FARPROC)PrintDialogProc, (HINSTANCE)GetWindowLong( report->hWnd, GWL_HINSTANCE) );
         #else
            lpfnMPrintDlgProc = PrintDialogProc;
         #endif
         dialogtemplate = PrintDlgTemplate() ;
      #else
         lpfnMPrintDlgProc = (DLGPROC)MakeProcInstance( (FARPROC)PrintDialogProc, (HINSTANCE)GetWindowWord( report->hWnd, GWW_HINSTANCE) );
         dialoghandle = NewDlgTemplate( DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU,
            40, 40, 120, 40, "", "", "CodeReporter 2.0", "", 0 );

         if( dialoghandle == (GLOBALHANDLE)NULL )
            return -1;

         if( AddDlgItem( dialoghandle, SS_CENTER | WS_CHILD | WS_VISIBLE | WS_GROUP,
             4, 6, 120, 12, "STATIC", "Cancel Printing", 0, "", -1 ) == FALSE )
         {
            GlobalFree( dialoghandle );
            return -1;
         }

         if( AddDlgItem( dialoghandle, BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP,
            44, 22, 32, 14, "BUTTON", "&Cancel", 0, "", 101 ) == FALSE )
         {
            GlobalFree( dialoghandle );
            return -1;
         }

         dialogtemplate = (LPSTR)GlobalLock( dialoghandle );
      #endif

      #ifdef S4WIN32
         #ifdef S4WINCE
            hInst = report->codeBase->hInst;
         #else
            hInst = (HINSTANCE)GetWindowLong( report->hWnd,GWL_HINSTANCE );
         #endif
         hDlgPrint = CreateDialogIndirectParam( hInst,
                                                dialogtemplate, report->hWnd,
                                                lpfnMPrintDlgProc, (LPARAM)report );
         #if __BORLANDC__  // LY May 13/04 : remove < 0x550 to avoid compiler error for all Borland compilers
            lpfnAbortProc = (ABORTPROC)MakeProcInstance((FARPROC)MPrintAbortProc, GetWindowLong( report->hWnd, GWL_HINSTANCE) );
         #else
            lpfnAbortProc = MPrintAbortProc;
         #endif
      #else
         hDlgPrint = CreateDialogIndirectParam( (HINSTANCE)GetWindowWord( report->hWnd,GWW_HINSTANCE ),
                                                (const void FAR*)dialogtemplate, report->hWnd,
                                                lpfnMPrintDlgProc, (LPARAM)report );
         lpfnAbortProc = (ABORTPROC)MakeProcInstance((FARPROC)MPrintAbortProc, (HINSTANCE)GetWindowWord( report->hWnd, GWW_HINSTANCE) );
      #endif

/*    Escape( report->printerDC, SETABORTPROC, 0, (LPSTR)lpfnAbortProc, NULL );*/
      SetAbortProc( report->printerDC, lpfnAbortProc );

      if( report->report_caption && lstrlen(report->report_caption) > 0 )
         tptr = report->report_caption;
      else
      {
         if( report->report_name && lstrlen(report->report_name) != 0 )
            tptr = report->report_name;
         else
         {
            wsprintf( title, TEXT("CodeReporter 2.0") );
            tptr = title;
         }
      }

      memset( &di, 0, sizeof( DOCINFO));
      di.cbSize = sizeof( DOCINFO ) ;
      di.lpszDocName = tptr ;
      #if ( WINVER >= 0x0400 )
         di.lpszDatatype = TEXT("") ;
         di.fwType = 0 ;
      #endif
/*    if( Escape( report->printerDC, STARTDOC, 10, tptr, NULL ) > 0 )*/
      if( StartDoc( report->printerDC, &di ) > 0 )
      {
         while( report->end_report != 2 && !bUserAbort )
         {
            if( StartPage( report->printerDC ) > 0 )
               report4generatePage( report, report->printerDC );
            else
               EndPage( report->printerDC ) ;
            if( !report->page_nums )
            {
/*             Escape( report->printerDC, NEWFRAME, 0, NULL, NULL );*/
               EndPage( report->printerDC ) ;
            }
            else
               if ( report->page_count >= report->start_page && report->page_count <= report->end_page )
               {
/*                Escape( report->printerDC, NEWFRAME, 0, NULL, NULL );*/
                  EndPage( report->printerDC ) ;
               }
         }
/*       Escape( report->printerDC, ENDDOC, 0, NULL, NULL );*/
         EndDoc( report->printerDC ) ;
      }
      if( !bUserAbort )
      {
         DestroyWindow( hDlgPrint );
      }

      #ifndef S4WIN32
         FreeProcInstance( (FARPROC)lpfnMPrintDlgProc );
         FreeProcInstance( (FARPROC)lpfnAbortProc );
      #endif

      PostMessage( report->hWnd, CRM_REPORTCLOSED, 0, 0L );
      ReleaseCapture();
      if( report->hWnd2 )
      {
         DestroyWindow( report->hWnd2 );
         report->hWnd2 = 0;
      }

      EnableWindow( report->hWnd, TRUE );
      SetFocus( report->hWnd );

   }
   report4pageFree( report );
   return 0;
}
#endif
#endif  /* S4OFF_REPORT */
