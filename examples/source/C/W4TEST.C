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
W4TEST.C   (c)Copyright Sequiter Software Inc., 1990-1998.
All rights reserved.

This example program is a CodeBase 6 Windows Client application,
similar in operation to D4TEST.C

*********************************************************************/

#include "d4all.h"
#include <windows.h>

#ifdef WIN32
   #define LPUSTR LPWSTR    // pseudo UNICODE //
   #define UTEXT(a)  L##a
   #define CHSIZE 2
   #define CLASS_CODE DWORD
#else 
   #define LPUSTR LPSTR
   #define UTEXT(a) a
   #define CHSIZE 1
   #define CLASS_CODE BYTE
#endif

/*********** Structure Declarations **********************************/

typedef struct 
{
	DWORD style ;
	WORD  x ;
	WORD  y ;
	WORD  cx ;
	WORD  cy ;
   WORD  classCode ;
	LPUSTR text ;
	BYTE  dataLen ;
	LPUSTR data ;
	WORD  id ;
		
}  D4CHILD_INFO ;


#ifndef WIN32

typedef struct
{
	DWORD style ;
	BYTE cdit ;
	WORD  x ;
	WORD  y ;
	WORD  cx ;
	WORD  cy ;
	
} DLGTEMPLATE, *LPDLGTEMPLATE ;

typedef struct
{
	WORD  x ;
	WORD  y ;
	WORD  cx ;
	WORD  cy ;
	WORD  id ;
	DWORD style ;

} DLGITEMTEMPLATE, *LPDLGITEMTEMPLATE ;

#endif   /* ifndef WIN32 */

	
typedef struct
{
   LPSTR  ptr ;

} D4PARSE_STR;


typedef struct
{
   HWND        hWnd ;
   HANDLE      hInst ;
   D4PARSE_STR parseStr ;
   WORD        x,y ;
   TEXTMETRIC  tm ;
   MSG         msg ;  
   HCURSOR     hSaveCursor ;

} D4DISPLAY;


typedef struct
{
	char serverId[128] ;
	char processId[128] ;
	char protocol[48] ;
	char protocolDll[13] ;
	WORD protocolId ;
	
} D4SETTINGS ;

/*********** Function Prototype and Global Variable Declarations *****/

#define IDM_SETTINGS 100        // menu items for transport protocol 
#define IDM_START               101 

#define IDD_NONE                0xFFFF
#define IDD_SERV        201             // dialog child controls 
#define IDD_PROC                202
#define IDD_IPX         203
#define IDD_WS                  204
#define IDD_OK                  205
#define IDD_CANCEL      206

#define IPX_PROTOCOL    "IPX/SPX"
#define IPX_DLL         "C4SPX.DLL"

#define WS_PROTOCOL     "Windows Sockets"
#define WS_DLL                  "C4SOCK.DLL"

// control styles                         
#define TABGRP                  WS_GROUP | WS_TABSTOP                        
#define EDIT_ST                 ES_UPPERCASE | ES_AUTOHSCROLL | TABGRP | WS_BORDER
#define LABEL_ST        SS_RIGHT               
#define RBUTTON_ST      BS_AUTORADIOBUTTON
#define DBUTTON_ST      BS_DEFPUSHBUTTON | TABGRP
#define BUTTON_ST       BS_PUSHBUTTON | TABGRP                     
#define GBUTTON_ST      BS_GROUPBOX

#define CLS_BUTTON 0x80 
#define CLS_EDIT   0x81 
#define CLS_LABEL  0x82 

// child control info 
D4CHILD_INFO children[] =
{  
	{ LABEL_ST,                                             115,    10,     40,  12,        CLS_LABEL,  UTEXT("Server ID"),  0,     NULL, IDD_NONE  },
	{ LABEL_ST,                                             115,    24,     40,  12,        CLS_LABEL,  UTEXT("Process ID"), 0,     NULL, IDD_NONE  },
	{ EDIT_ST,                                              160,    8,              105, 12,        CLS_EDIT,   UTEXT("S4SERVER"),  0,      NULL, IDD_SERV },
	{ EDIT_ST,                                              160,    23,     105, 12,        CLS_EDIT,   UTEXT("23165"),      0,     NULL, IDD_PROC },
	{ GBUTTON_ST | WS_GROUP,        5,              5,              110, 70,        CLS_BUTTON, UTEXT("Protocols"), 0,      NULL, IDD_NONE  },
/*      { RBUTTON_ST | TABGRP,          10,     20,     40,  8,         CLS_BUTTON, UTEXT("IPX/SPX"),           0,      NULL, IDD_IPX },*/
	{ RBUTTON_ST,                                   10,     28,     95,  8,         CLS_BUTTON, UTEXT("TCP/IP - Windows Sockets"),  0, NULL, IDD_WS },      
	{ DBUTTON_ST,                                   160,    45,     32,  14,                CLS_BUTTON, UTEXT("OK"),                        0, NULL, IDD_OK },
	{ BUTTON_ST,                                    195,    45,     32,  14,                CLS_BUTTON, UTEXT("Cancel"),            0, NULL, IDD_CANCEL },  
	{ 0,                                                            0,              0,              0,        0,            0,            NULL,                                        0, NULL, 0   },
} ;
			
CODE4    cb;
DATA4    *dataFile = 0;
FIELD4   *fName, *lName, *address, *age, *birthDate, *married,
	 *amount, *comment;
char     gcAlignBytes ;         //records amount of alignment for first child control in dialog (32-bit only)

FIELD4INFO  field_info [] =
{
   {"F_NAME",r4str,10,0},      
   {"L_NAME",r4str,10,0},
   {"ADDRESS",r4str,15,0},
   {"AGE",r4num,2,0},          
   {"BIRTH_DATE",r4date,8,0},
   {"MARRIED",r4log,1,0},      
   {"AMOUNT",r4num,7,2},
   {"COMMENT",r4memo,10,0},    
   {0,0,0,0},
};


HCURSOR hHourGlass, hSaveCursor ;
D4DISPLAY display ;
extern MSG msg ;
extern TEXTMETRIC tm ;

int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int) ;
BOOL InitApplication(HINSTANCE) ;
BOOL InitInstance(HINSTANCE, int) ;
WORD lstrlenWBytes( LPUSTR ) ;
LRESULT CALLBACK MainWndProc(HWND, UINT, UINT, LONG) ;
BOOL CALLBACK PASCAL SettingsDlgProc( HWND, UINT, WPARAM, LPARAM ) ;

void AddNewRecord( LPSTR, LPSTR, LPSTR, int, int, double, LPSTR ) ;
WORD ConnectToServer( CODE4 *, D4SETTINGS * ) ;
WORD OpenDataFile( void );
void PrintRecords( void ) ;
void ResetDisplay( HWND ) ;

GLOBALHANDLE d4newDlgTemplate(DWORD, WORD, WORD, WORD, WORD, LPUSTR, LPUSTR, LPUSTR, LPUSTR, WORD) ;
GLOBALHANDLE d4createDialog( void ) ;
BOOL d4addDlgItem( GLOBALHANDLE, D4CHILD_INFO * ) ;
WORD  d4templateSize( GLOBALHANDLE ) ;
LPSTR d4trim( char *, char ) ;

void S4FUNCTION d4displayInit( HWND ) ;
void S4FUNCTION d4displayStr( char *, int ) ;


/********************* WinMain routine *********************************/

int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{  
	MSG msg ;
	
   if (!hPrevInstance)
      if (!InitApplication(hInstance)){
	 MessageBox(0, "", "InitApplication Failed", MB_OK); 
	 return (FALSE);
      }

   if (!InitInstance(hInstance, nCmdShow)){
      MessageBox(0, "", "InitInstance Failed", MB_OK); 
      return (FALSE);
   }

	while( GetMessage( &msg, (HWND)NULL, 0, 0 ) )
	{
		TranslateMessage( &msg ) ;
		DispatchMessage( &msg ) ;
	}      

   return msg.wParam ;
}

/************ Message Processing Routine ***********************/
LRESULT CALLBACK MainWndProc( HWND hWnd, UINT message, UINT wParam,LONG lParam)
{
	static GLOBALHANDLE dlgHandle ;
	static D4SETTINGS settings ;
	static D4SETTINGS * pSettings = &settings ;
	static HINSTANCE hInst ;
   static FARPROC DlgProc ;
   LPDLGTEMPLATE lpDlgTemplate ;

   switch (message)
   {
   	case WM_CREATE:
		   if ( ! (dlgHandle = d4createDialog( ) ) )
		   {
            MessageBox( hWnd, "Error.\nCannot create Settings dialog box. Cannot continue with example", 
                                    "W4Test Error", MB_ICONSTOP | MB_OK ) ;
				PostQuitMessage(0) ;
			}
			else
			{
				hInst = ((LPCREATESTRUCT) lParam)->hInstance ;
				
//            DlgProc = (DLGPROC)MakeProcInstance( (FARPROC)SettingsDlgProc, hInst ) ;
				strcpy( settings.serverId,      DEF4SERVER_ID ) ;
				strcpy( settings.processId,     DEF4PROCESS_ID ) ;
	         strcpy( settings.protocol,      WS_PROTOCOL ) ;
	         strcpy( settings.protocolDll, WS_DLL ) ;
		      settings.protocolId = IDD_WS ;
			}
			
			PostMessage( hWnd, WM_COMMAND, IDM_SETTINGS, 0 ) ;
   		return 0 ;                                      
      case WM_DESTROY:
         PostQuitMessage(0);
         return 0 ;
      case WM_COMMAND:
         switch( wParam )
         {       
            case IDM_START:
               ResetDisplay( hWnd ) ;
			         
               if( ConnectToServer( &cb, &settings ) != r4success )
               {
                  ResetDisplay( hWnd ) ;
                  return 0 ;            
               }             

               if( OpenDataFile() )
               {
                  AddNewRecord("Sarah"
                     ,"Webber"
                     ,"132-43 St."
                     ,32
                     ,1
                     ,147.99
                     ,"New Customer");
	   
                  AddNewRecord("John"
                     ,"Albridge"
                     ,"1232-76 Ave."
                     ,12
                     ,0
                     ,98.99
                     ,"");
	   
                  PrintRecords();

                  code4close( &cb ) ;
               }
	          
               d4displayStr( " ", 1 ) ;                                                                                        
               d4displayStr( "End of connection test.", 1 ) ;
					   
               code4initUndo( &cb ) ;
               return 0;
            case IDM_SETTINGS:                                                                          
               lpDlgTemplate = (LPDLGTEMPLATE)GlobalLock(dlgHandle) ;
               if( DialogBoxIndirectParam( hInst, lpDlgTemplate, hWnd, (DLGPROC)SettingsDlgProc, (LPARAM)pSettings) == -1 ) 
                  MessageBox( hWnd, "Error.\nCannot display Settings dialog box","W4Test Error", MB_ICONSTOP | MB_OK ) ;   
               return 0 ;
         }
			
   }
	return DefWindowProc(hWnd, message, wParam, lParam) ;
}

								 
/************ Dialog Procedure ***********************/
BOOL CALLBACK SettingsDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
	static D4SETTINGS tempSettings ;
	static D4SETTINGS *ptrSettings ;
		
	switch( msg )
	{
		case WM_INITDIALOG:
			ptrSettings = (D4SETTINGS *)lParam ;
			SetWindowText( GetDlgItem( hDlg, IDD_SERV ), ptrSettings->serverId ) ;
			SetWindowText( GetDlgItem( hDlg, IDD_PROC ), ptrSettings->processId ) ;
			CheckRadioButton( hDlg, IDD_IPX, IDD_WS, ptrSettings->protocolId ) ;
			lstrcpy( tempSettings.serverId, ptrSettings->serverId ) ;
			lstrcpy( tempSettings.processId, ptrSettings->processId ) ;
			lstrcpy( tempSettings.protocol, ptrSettings->protocol ) ;
			lstrcpy( tempSettings.protocolDll, ptrSettings->protocolDll ) ;
			tempSettings.protocolId = ptrSettings->protocolId  ;
						
			return TRUE ;
			
		case WM_SYSCOMMAND:
			if( wParam == SC_CLOSE )
			{
				EndDialog( hDlg, TRUE ) ;
				return TRUE ;
			}
				
		case WM_COMMAND:
			switch( LOWORD(wParam) )
			{
				case IDD_IPX:                                     // IPX/SPX Protocol 
					lstrcpy( tempSettings.protocol, IPX_PROTOCOL ) ;
					lstrcpy( tempSettings.protocolDll, IPX_DLL ) ;
					tempSettings.protocolId = IDD_IPX ;
					return TRUE ;
					
				case IDD_WS:                                      // Windows Sockets Protocol 
					lstrcpy( tempSettings.protocol, WS_PROTOCOL ) ;                         
					lstrcpy( tempSettings.protocolDll, WS_DLL ) ;
					tempSettings.protocolId = IDD_WS ;
					return TRUE ;
				
				case IDD_SERV:                                    // Server ID Name 
	       #ifdef WIN32
					if( HIWORD(wParam) == EN_CHANGE )
	       #else
					if( HIWORD(lParam) == EN_CHANGE )
	       #endif
					{
						GetWindowText( (HWND)LOWORD(lParam), tempSettings.serverId, sizeof(tempSettings.serverId) ) ;
						return TRUE ;
					}
					else
						break ;
					
				case    IDD_PROC:                                         // Process Id Name 
	       #ifdef WIN32
					if( HIWORD(wParam) == EN_CHANGE )
	       #else
					if( HIWORD(lParam) == EN_CHANGE )
	       #endif
					{
						GetWindowText( (HWND)LOWORD(lParam), tempSettings.processId, sizeof(tempSettings.processId) ) ;
						return TRUE ;
					}
					else             
						break ;
						
				case IDD_OK:
					if( strlen( d4trim( tempSettings.serverId, ' ' ) ) == 0 || 
					  ( tempSettings.protocolId == IDD_WS && strlen( d4trim( tempSettings.processId, ' ' )  ) == 0 ) )
					{
						MessageBox( hDlg, "Error. \nAll fields in this dialog box must have a value", 
																						"W4Test Error", MB_OK | MB_ICONSTOP ) ;
						return FALSE ;
					}
					else
					{
						lstrcpy( ptrSettings->serverId, tempSettings.serverId ) ;
						lstrcpy( ptrSettings->processId, tempSettings.processId ) ;
						lstrcpy( ptrSettings->protocol, tempSettings.protocol ) ; 
						lstrcpy( ptrSettings->protocolDll, tempSettings.protocolDll ) ;
						ptrSettings->protocolId = tempSettings.protocolId ;
						EndDialog( hDlg, 0 ) ;
						return TRUE ;
					}
					
				case IDD_CANCEL:                
					EndDialog( hDlg, 0 ) ;
					return TRUE ; 
				
			}      
	}  
	
	return FALSE ;
	
}

   
/*********** Example Support Routines **********************/

void AddNewRecord(LPSTR fNameStr
		 ,LPSTR lNameStr
		 ,LPSTR addressStr
		 ,int ageValue
		 ,int marriedValue
		 ,double amountValue
		 ,LPSTR commentStr)
{


   d4appendStart(dataFile,0);

   f4assign(fName,fNameStr);
   f4assign(lName,lNameStr);
   f4assign(address,addressStr);

   f4assignInt(age,ageValue);

   if(marriedValue)
      f4assign(married,"T");
   else
      f4assign(married,"F");

   f4assignDouble(amount,amountValue);
   f4memoAssign(comment,commentStr);

   d4append(dataFile);
   
}
	 
WORD ConnectToServer( CODE4 *cb, D4SETTINGS *settings ) 
{
   int rc ;      
   char buf[128] ;

   code4init( cb ) ;         
   
   sprintf( buf,  "Attempting to connect to server ...", 
						   settings->protocol ) ;
   d4displayStr( buf, 1 ) ;
      
   rc = code4connect( cb, settings->serverId, settings->processId, 
				    NULL, NULL, settings->protocolDll ) ;

   if ( rc != r4success )
   {
      MessageBox( 0, "W4Test could not connect to the server. " \
		     "Ensure that the CodeBase database server is " \
		     "running. If the server is running, check that " \
		     "you have supplied the correct server name and "
		     "possibly process id, that your network software " \
		     "supports the protocol you selected, and that " \
		     "you have the necessary software drivers loaded " \
		     "for that protocol. "
		     , "W4Test", MB_OK | MB_ICONSTOP ) ;

      code4initUndo( cb ) ;
   }  
   else
      d4displayStr( "Successful connection", 1 ) ;
      
   return rc ;
}
	    
WORD OpenDataFile( )
{

   cb.safety = 0;

   dataFile = d4create(&cb,"C4TEST.DBF",field_info,0);
   
   if( dataFile )
   {
	   
	   fName = d4field(dataFile,"F_NAME");
	   lName = d4field(dataFile,"L_NAME");
	   address = d4field(dataFile,"ADDRESS");
	   age = d4field(dataFile,"AGE");
	   birthDate = d4field(dataFile,"BIRTH_DATE");
	   married = d4field(dataFile,"MARRIED");
	   amount = d4field(dataFile,"AMOUNT");
	   comment = d4field(dataFile,"COMMENT");
	   
	   return 1 ;
	}            
	
	return 0 ;
}

void PrintRecords( )
{

   int        rc,ageValue;
   double     amountValue;
   char       nameStr[25];
   char       addressStr[20];
   char       dateStr[9];
   char       marriedStr[2];
   const char *commentStr;
   char       buf[64] ;
   
   for(rc = d4top(dataFile);rc == r4success
		      ;rc = d4skip(dataFile, 1L))
   {
      u4ncpy(nameStr,f4str(fName)
			     ,sizeof(nameStr));
      u4ncpy(addressStr,f4str(address)
			   ,sizeof(addressStr));
      ageValue = f4int(age);
      f4double2(amount, &amountValue);
 
      u4ncpy(dateStr,f4str(birthDate)
			      ,sizeof(dateStr));
      u4ncpy(marriedStr,f4str(married),
			    sizeof(marriedStr));
      commentStr = f4memoStr(comment);

      d4displayStr("------------------------------", 1);
      sprintf(buf, "Name     : %20s",nameStr);
      d4displayStr(buf, 1) ;
      sprintf(buf, "Address  : %15s",addressStr);
      d4displayStr(buf, 1) ;
      sprintf(buf, "Age : %3d   Married : %1s"
			  ,ageValue,marriedStr);
      d4displayStr(buf, 1) ;
      sprintf(buf, "Comment: %s",commentStr);
      d4displayStr(buf, 1) ;
      sprintf(buf, "Purchased this year : $%5.2lf ",
									amountValue);
      d4displayStr(buf, 1) ;
      d4displayStr( " ", 1 ) ;
   }
   
}

void ResetDisplay( HWND hWnd ) 
{  
	InvalidateRect( hWnd, NULL, TRUE ) ;
	UpdateWindow( hWnd ) ;  
	display.x = display.y = 0 ;           
}

/********** Windows initialization functions ***********************/

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
    wc.lpszMenuName = NULL;
    wc.lpszClassName = "TestWClass";

    return (RegisterClass(&wc));
}


BOOL InitInstance( HINSTANCE hInstance, int nCmdShow)
{ 

   HWND   hWnd;
   HMENU  hMenu ;

   hWnd = CreateWindow(
	"TestWClass",
	"CodeBase 6 - W4Test",
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

   hMenu = CreateMenu();
   
   if( ! hMenu )
      return (FALSE);

   AppendMenu( hMenu,   MF_STRING,  IDM_SETTINGS,       "&Settings" );
   AppendMenu( hMenu,   MF_STRING, IDM_START,           "S&tart" );

   SetMenu( hWnd, hMenu ) ;

   SetTimer( hWnd, (UINT)hWnd, (UINT)1000, (TIMERPROC)NULL ) ;

   d4displayInit( hWnd ) ;
   display.hInst = hInstance ;   

   ShowWindow(hWnd, nCmdShow);
   UpdateWindow(hWnd);
   return (TRUE);
}
	       
	       
/*********** display text in Windows functions **********************/

void S4FUNCTION d4displayInit( HWND h )
{

   HDC hdc ;
   TEXTMETRIC tm ;

   memset( &display, 0, sizeof(D4DISPLAY) ) ;
   display.hWnd =  h ;
   hdc =  GetDC(display.hWnd) ;
   GetTextMetrics( hdc, &tm ) ;
   display.tm = tm ;
   ReleaseDC(display.hWnd,hdc) ;
   display.x =  0 ;
   display.y =  0 ;
   
} 


void  S4FUNCTION d4displayStr( LPSTR str, int is_new_line )
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
      display.y +=  height +  (int)display.tm.tmExternalLeading ;
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
     

/*********** create Dialog in memory functions **********************/

void d4align( void** src, char alignOn )
{
	unsigned long l = (unsigned long)*src ;

	char result = alignOn - 1 ;
	l += result ;
	l >>= alignOn/2 ;
	l <<= alignOn/2 ;
	*src = (void*)l ;
}
	

GLOBALHANDLE d4createDialog( void )
{     

	int i = 0 ;
	GLOBALHANDLE dlgHandle = 0 ;
	
	DWORD dlgStyle = WS_POPUP | DS_MODALFRAME | WS_CAPTION | WS_SYSMENU ;                  
	
	dlgHandle = d4newDlgTemplate( dlgStyle, 40, 40, 270, 100, UTEXT(""), UTEXT(""), UTEXT("W4Test Settings"), UTEXT(""), 0 ) ;
	if( dlgHandle )
	{
		while( children[i].id )
		{
			if( ! d4addDlgItem( dlgHandle, &children[i] ) ) 
			{
				GlobalFree( dlgHandle ) ;
				break ;
			}
			i++ ;
		}
	}   
	return dlgHandle ;
	
}            

GLOBALHANDLE d4newDlgTemplate(DWORD Style, WORD X, WORD Y, WORD CX, WORD CY,
		       LPUSTR Menu, LPUSTR Class, LPUSTR Text,
		       LPUSTR TypeFace, WORD PtSize)
{    

   LPDLGTEMPLATE lpDT ;
   LPSTR lpsztemp ;
   GLOBALHANDLE hDTemplate;
   DWORD needed_size ;
	short copyLen ;

   needed_size = sizeof( DLGTEMPLATE ) + lstrlenWBytes(Menu) + CHSIZE +
      lstrlenWBytes(Class) + CHSIZE + lstrlenWBytes(Text) + CHSIZE;

	#ifdef WIN32
		needed_size += 2 ;      //for Win32 need DWORD alignment after template
	#endif

   if( TypeFace[0] )
      needed_size += lstrlenWBytes(TypeFace) + CHSIZE + sizeof(short);

   hDTemplate = GlobalAlloc( LHND, needed_size );

   if( hDTemplate == (GLOBALHANDLE)NULL )
      return hDTemplate;

   lpDT = (DLGTEMPLATE far * )GlobalLock( hDTemplate );

   //write the template information

   lpDT->style = WS_VISIBLE | Style;
   lpDT->x = X;
   lpDT->y = Y;
   lpDT->cx = CX;
   lpDT->cy = CY;

   lpsztemp = (LPSTR)lpDT + sizeof( *lpDT );
   copyLen = lstrlenWBytes(Menu) ;
   lpsztemp = (LPSTR)memcpy( lpsztemp, Menu, copyLen>80 ? 80+CHSIZE : copyLen+CHSIZE );
   lpsztemp += copyLen + CHSIZE;
	copyLen = lstrlenWBytes(Class) ;
   lpsztemp = (LPSTR)memcpy( lpsztemp, Class, copyLen>80 ? 80+CHSIZE : copyLen+CHSIZE  );
   lpsztemp += copyLen + CHSIZE;
	copyLen = lstrlenWBytes(Text) ;
   lpsztemp = (LPSTR)memcpy( lpsztemp, Text, copyLen>80 ? 81 : copyLen+CHSIZE  );
   lpsztemp +=  copyLen + CHSIZE ;

	if( TypeFace[0] )
   {
		copyLen = lstrlenWBytes(TypeFace) ;
      *((short S4PTR *)lpsztemp) = PtSize;
      memcpy( lpsztemp+sizeof(short), TypeFace,((copyLen>80)?81:copyLen + CHSIZE)   );
      lpDT->style |= DS_SETFONT;
   }

   GlobalUnlock( hDTemplate );

   return hDTemplate;
}

BOOL d4addDlgItem( GLOBALHANDLE dlgTmp, D4CHILD_INFO * ci )       
{ 

   LPDLGTEMPLATE lpDT;
   LPDLGITEMTEMPLATE lpDIT;
   LPSTR lpsztemp ;
   GLOBALHANDLE hDTemplate;
   UINT new_size, BlockSize, stringLen;
	#ifdef WIN32
	static char firstChild = TRUE ;
	#endif

   BlockSize = d4templateSize( dlgTmp );

   new_size = BlockSize + sizeof(DLGITEMTEMPLATE) + sizeof(CLASS_CODE) + 
				  lstrlenWBytes(ci->text) + CHSIZE ;

   // allocate a small extra amt of memory to ensure any garbage
   // following our allocation is not interpreted as 'extra' info
   // by DialogBoxIndirectParam()
   new_size += sizeof(long) ;

   hDTemplate = GlobalReAlloc( dlgTmp, new_size, LHND );

   if( hDTemplate == (GLOBALHANDLE)NULL )
      return FALSE;

   lpDT = (LPDLGTEMPLATE)GlobalLock( hDTemplate );

	lpDIT = (LPDLGITEMTEMPLATE)(((LPSTR)lpDT)+BlockSize);

	#ifdef WIN32
		if( firstChild )
		{
	LONG ptr ;
			ptr = (LONG)(lpDIT) ;
			d4align( (void**)&lpDIT, 4 );   //all child controls must be on DWORD boundary
			gcAlignBytes = (char)((LONG)lpDIT - ptr) ;
			firstChild = FALSE ;
		}
		else
		{
			d4align( (void**)&lpDIT, 4 ) ;          
		}
	#endif

   lpDIT->x = ci->x;
   lpDIT->y = ci->y;
   lpDIT->cx = (ci->cx < lpDT->cx)? ci->cx : lpDT->cx;
   lpDIT->cy = (ci->cy < lpDT->cy)? ci->cy: lpDT->cy;
   lpDIT->id = ci->id;
   lpDIT->style = WS_VISIBLE | WS_CHILD | ci->style;

   lpsztemp = (LPSTR)(lpDIT)+sizeof(*lpDIT);

	#ifdef WIN32
      *(PWORD)lpsztemp = 0xffff ;
      lpsztemp+=2 ;
	#endif

	*lpsztemp = (char)ci->classCode ;
   lpsztemp += CHSIZE ;
	stringLen = lstrlenWBytes(ci->text) ;
   lpsztemp = (LPSTR)memcpy( lpsztemp, ci->text, stringLen>80?80+CHSIZE:stringLen+CHSIZE);
   lpsztemp += stringLen + CHSIZE;

   if( ci->dataLen )
      memcpy( lpsztemp, ci->data, ci->dataLen );

   lpDT->cdit++;

   GlobalUnlock( hDTemplate );

   return TRUE;
}

WORD d4templateSize( GLOBALHANDLE hDlgTmp )
{ 

   LPDLGTEMPLATE lpDT;
   LPSTR lpstr, lpbase;
   WORD isize ;
   WORD i;

   lpDT = (DLGTEMPLATE S4PTR *)GlobalLock( hDlgTmp );
   lpbase= (LPSTR)lpDT;
   lpstr = (LPSTR)(lpDT + 1);
   isize = sizeof( *lpDT );
   lpstr = lpbase+(isize+=(lstrlenWBytes((LPUSTR)lpstr)+CHSIZE));
   lpstr = lpbase+(isize+=(lstrlenWBytes((LPUSTR)lpstr)+CHSIZE));
   lpstr = lpbase+(isize+=(lstrlenWBytes((LPUSTR)lpstr)+CHSIZE));

	lpstr += gcAlignBytes ;

   if((lpDT->style)&DS_SETFONT)
   {
      isize += sizeof(short);
      lpstr += sizeof(short);
      lpstr = lpbase+(isize+=(lstrlenWBytes((LPUSTR)lpstr)+CHSIZE));
   }

   for( i = 0; i < lpDT->cdit; i++ )
   {
      lpstr = lpbase+(isize+=sizeof(DLGITEMTEMPLATE)+gcAlignBytes);
		lpstr = lpbase+(isize+=sizeof(CLASS_CODE)) ; 
      lpstr = lpbase+(isize+=(lstrlenWBytes((LPUSTR)lpstr)+CHSIZE));

      #ifndef WIN32
	 if( *lpstr )
	 {
	    isize += *lpstr;
	    lpstr += *lpstr;
	 }

	 isize++;
      #endif

   }

   #ifdef WIN32
      isize += 2 ;   // need null WORD between child controls
   #endif

   GlobalUnlock( hDlgTmp );
  
   return isize;
   
}


LPSTR d4trim( char *data, char value )
{   

	int len = strlen( data ) - 1 ;
	
	while( data[len] == value )
	{
		data[len] = '\0' ;
		len--;
	}
	
	return data ;
}

WORD lstrlenWBytes( LPUSTR source )
{
   // need function for length in bytes of unicode string under Windows NT or Windows 95
	
	#ifndef WIN32
		return strlen( source ) ;
	#else
	{
		LPSTR p = (LPSTR)source ;
		WORD i = 0;
		do
		{
			if( ! p[i] )            //looking for 00 00 
				if( ! p[i+1] )
					return i ;
			i+=2 ; 
		}
		while( 1 ) ;
	}
	#endif
}
