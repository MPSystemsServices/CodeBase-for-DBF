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

#include <windows.h>
#include "d4all.h"

long FAR PASCAL WndProc( HWND, UINT, WPARAM, LPARAM );

CODE4 cb ;
DATA4 *data ;

#ifdef __BORLANDC__
   #pragma argsused
#endif
int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdParam, int nCmdShow )
{
   static char szAppName[] = "testapp";
   HWND hwnd;
   WNDCLASS wc;

   if ( ! hPrevInstance )
   {
      wc.style = CS_HREDRAW | CS_VREDRAW;
      wc.lpfnWndProc = WndProc;
      wc.cbClsExtra = 0;
      wc.cbWndExtra = 0;
      wc.hInstance = hInstance;
      wc.hIcon = LoadIcon( NULL, IDI_APPLICATION );
      wc.hCursor = LoadCursor( NULL, IDC_ARROW );
      wc.hbrBackground = (HBRUSH)GetStockObject( WHITE_BRUSH );
      wc.lpszMenuName = NULL;
      wc.lpszClassName = szAppName;

      RegisterClass( &wc );
   }

   hwnd = CreateWindow( szAppName,
                        "CodeBase Test Program",
                        WS_OVERLAPPEDWINDOW,
                        CW_USEDEFAULT, CW_USEDEFAULT,
                        CW_USEDEFAULT, CW_USEDEFAULT,
                        NULL, NULL, hInstance,
                        NULL );

   ShowWindow( hwnd, nCmdShow );
   UpdateWindow( hwnd );

   code4init( &cb );
   cb.hWnd = hwnd;
   cb.hInst = hInstance;
   cb.autoOpen = 0;

   data = d4open( &cb, "INFO" );
   if (data)
      /* Cause a CodeBase message box to appear*/
      d4go( data, d4recCount( data ) + 1 ) ;

   code4initUndo( &cb );
   return TRUE ;
}

long FAR PASCAL WndProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   return( DefWindowProc(hwnd, msg, wParam, lParam) );
}
