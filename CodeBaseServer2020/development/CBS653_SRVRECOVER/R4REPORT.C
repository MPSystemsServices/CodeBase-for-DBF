/* r4report.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */
#include "d4all.h"
#ifndef S4OFF_REPORT
   #ifdef S4WINDOWS
      #include <commdlg.h>
   #endif

   void    S4FUNCTION code4calcReset( CODE4 S4PTR * ) ;

   /* Function: report4index_type()
      PARAMETERS: none
      DESCRIPTION: used by the CR executable to determine the index
      RETURNS: an integer value, defined in r4report.h, specifying the index */

   int S4FUNCTION report4index_type()
   {
      #ifndef S4CLIENT
         #ifdef S4MDX
            return r4mdx;
         #endif

         #ifdef S4CLIPPER
            return r4ntx;
         #endif

         #ifdef S4FOX
            return r4cdx;
         #endif
      #else
         return -1 ;
      #endif
   }

   int S4FUNCTION report4off_write()
   {

      #ifndef S4CLIENT
         #ifdef S4OFF_WRITE
            return 1;
         #else
            return 0;
         #endif
      #else
         return 0 ;
      #endif

   }

   /* FOR A DESCRIPTION SEE THE CODEREPORTER MANUAL */
   PREPORT4 S4FUNCTION report4init( RELATE4 *relate )
   {
      REPORT4 *r4 ;
      CODE4 *codeBase;
      #ifdef S4WINDOWS
      LOGFONT lfont;
      #endif

      #ifdef E4PARM_HIGH
              if( relate == 0 )
         {
            error4( 0, e4parm_null, E95702 ) ;
            return NULL ;
         }
      #endif

      codeBase = relate->codeBase;

      /* you can only have one report open at a time */
      if( codeBase->numReports != 0 )
      {
         error4describe( codeBase, e4report, 0L, E4_REP_ONE, 0, 0 );
         return NULL;
      }

      /* allocate the REPORT4 structure */
      r4 =  (REPORT4 *) u4allocFree( codeBase, sizeof(REPORT4) ) ;
      if ( r4 == 0 )
      {
         error4describe( codeBase, e4report, 0L, E4_REP_REPALLOC, 0, 0 );
         return NULL ;
      }

      /* reset the list members */
      memset( &r4->groups, 0, sizeof(r4->groups) );
      memset( &r4->styles, 0, sizeof(r4->styles) );
      memset( &r4->active_objects, 0, sizeof(r4->active_objects) );
      memset( &r4->obj_type_info, 0, sizeof(r4->obj_type_info) );

      /* set the codeBase, relate, and default_date_format members */
      r4->codeBase =  codeBase ;
      r4->relate =  relate ;
      #ifdef S4WINDOWS
         lstrcpy( r4->default_date_format, code4dateFormat( codeBase ) );
      #else
         strcpy( r4->default_date_format, code4dateFormat( codeBase ) );
      #endif

      /* turn off autoOpen of index files */
      r4->codeBase->autoOpen = 0;

      /* set the default margin and page size */
      r4->margin_left = 250;
      r4->margin_right = 250;
      r4->report_width =  8000 ;
      r4->report_height = 11000;

      /* set the default symbols */
      r4->decimal_point =  '.' ;
      r4->thousand_separator = ',' ;
      r4->currency_symbol[0] = '$';
      r4->currency_symbol[1] = '\0';

      /* by default output is to the screen */
      r4->output_code = r4->output_handle = 1;

      /* default units of display are points: applys to the CR executable */
      r4->units =  UNIT4POINTS ;

      /* no grid by default: applys to the CR executable */
      r4->sensitivity_x = r4->sensitivity_y = 0;

      /* by default save report with pathnames */
      r4->pathnames = 1;

      /* create the default style */
      #ifdef S4WINDOWS
         /* if not being called by the CR executable */
         if( !codeBase->s4cr2 )
         {
            memset( &lfont, 0, sizeof(R4LOGFONT) );
            strcpy(lfont.lfFaceName,"MS Serif" );
            #ifdef S4GERMAN
               style4create( (PREPORT4)r4, (PR4LOGFONT)&lfont, (char *)"Normal", (R4COLORREF)NULL, (int)10 );
            #else
               style4create( (PREPORT4)r4, (PR4LOGFONT)&lfont, (char *)"Plain Text", (R4COLORREF)NULL, (int)10 );
            #endif
            r4->active_style = (PSTYLE4)l4first( &r4->styles );
         }
      #else
         #ifdef S4GERMAN
            style4create( r4, "Normal", 0, NULL, 0, NULL );
         #else
            style4create( r4, "Plain Text", 0, NULL, 0, NULL );
         #endif
         r4->active_style = (PSTYLE4)l4first( &r4->styles );
      #endif

      /* create the title/summary and page header/footer groups */
      group4create_title_summary( r4 );
      group4create_pgheader_pgfooter( r4 );

      if( r4 )
         r4->codeBase->numReports = 1;
      else
         r4->codeBase->numReports = 0;

      return r4 ;
   }

   #ifdef S4WINDOWS
   /* frees the windows font associated with the object by releasing the
      font from the objects windows device context.  This function is
      only used through the report4free_styles fctn */
   void report4set_obj_font_free( OBJ4 *obj )
   {
      OBJ4 *obj_on;
      HDC hDC;

      obj_on = (OBJ4 *)l4first( &obj->contained );
      while( obj_on )
      {
         report4set_obj_font_free( obj_on );
         obj_on = (OBJ4 *)l4next( &obj->contained, obj_on );
      }

      hDC = GetDC( obj->hWnd );
      SelectObject( hDC, GetStockObject(SYSTEM_FONT) );
      ReleaseDC( obj->hWnd, hDC );
   }

   #endif

   void S4FUNCTION report4free_styles( REPORT4 *report )
   {
      #ifdef S4WINDOWS
      GROUP4 *group_on;
      AREA4 *area_on;
      OBJ4 *obj_on;
      #endif
      STYLE4 *style_on, *style_next;

      /* if this is a report in windows de-select the font from the device context
         of every object */
      #ifdef S4WINDOWS
      if( report->page_header_footer )
      {
         area_on = (AREA4 *)l4first( &report->page_header_footer->header_areas );
         while( area_on )
         {
            obj_on = (OBJ4 *)l4first( &area_on->objects );
            while( obj_on )
            {
               report4set_obj_font_free( obj_on );
               obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
            }
            area_on = (AREA4 *)l4next( &report->page_header_footer->header_areas, area_on );
         }

         area_on = (AREA4 *)l4first( &report->page_header_footer->footer_areas );
         while( area_on )
         {
            obj_on = (OBJ4 *)l4first( &area_on->objects );
            while( obj_on )
            {
               report4set_obj_font_free( obj_on );
               obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
            }
            area_on = (AREA4 *)l4next( &report->page_header_footer->footer_areas, area_on );
         }
      }

      group_on = (GROUP4 *)l4first( &report->groups );
      while( group_on )
      {
         area_on = (AREA4 *)l4first( &group_on->header_areas );
         while( area_on )
         {
            obj_on = (OBJ4 *)l4first( &area_on->objects );
            while( obj_on )
            {
               report4set_obj_font_free( obj_on );
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
               report4set_obj_font_free( obj_on );
               obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
            }
            area_on = (AREA4 *)l4next( &group_on->footer_areas, area_on );
         }
         group_on = (GROUP4 *)l4next( &report->groups, group_on );
      }
      #endif

      /* free the STYLE4 structures in the style list */
      style_on = (STYLE4 *)l4first( &report->styles );
      while( style_on )
      {
         style_next = (STYLE4 *)l4next( &report->styles, style_on );
         style4free( report, style_on );
         style_on = style_next;
      }
   }


   /* SEE the CODEREPORTER MANUAL FOR A DESCRIPTION */
   void S4FUNCTION report4freeLow( REPORT4 *r4, short free_relate, short close_files, short doPrinterFree )
   {
      GROUP4 *group_on, *group_next;
      RELATE4 *relate;
      POUT4OBJ o4obj;

      #ifdef E4PARM_HIGH
         if( r4 == 0 )
         {
            error4( 0, e4parm_null, E95702 ) ;
            return ;
         }
      #endif

      if( r4->codeBase->numReports != 1 )
      {
         error4describe( r4->codeBase, e4report, 0L, E4_REP_NONE, 0, 0 ) ;
      }

      r4->codeBase->numReports = 0 ;

      /* free the report name and file_name */
      if( r4->report_name )
         u4free( r4->report_name );

      if( r4->report_file_name )
         u4free( r4->report_file_name );

      /* free the reports styles */
      report4free_styles( r4 );

      /* free the page header/footer group */
      if( r4->page_header_footer )
      {
         group4free( r4->page_header_footer );
         r4->page_header_footer = NULL;
      }

      /* free all the other groups */
      group_on = (GROUP4 *)l4first( &r4->groups );
      while( group_on )
      {
         group_next = (GROUP4 *)l4next( &r4->groups, group_on );
         group4free( group_on );
         group_on = group_next;
      }

      /* free the additional record buffers in the DATA4 structures in the relation
          */
      relate = r4->relate;
      while( relate )
      {
         if( relate->oldRecord )
         {
            /* 01/07/96 AS - CodeReporter switches the oldRecord pointer and
               relate->oldRecord occasionally.  Since we now do a group
               allocation on the record buffers, need to swap pointers back
               if required to ensure we free the correct one
                    */
            if ( relate->oldRecord == relate->data->groupRecordAlloc )
            {
               relate->oldRecord = relate->data->record ;
               relate->data->record = relate->data->groupRecordAlloc ;
            }
            u4free( relate->oldRecord );
            relate->oldRecord = NULL;
         }
         relate4next( &relate );
      }

      /* if the free relate flag was set, free the relate */
      if (free_relate)
         relate4free(r4->relate, close_files);

      /* free the printer name */
      if( r4->printer_name )
         u4free( r4->printer_name );

      /* free the printer device context, if not called from CodeReporter */
      #ifdef S4WINDOWS
              if ( doPrinterFree )
                     if( r4->printerDC )
                            DeleteDC( r4->printerDC );
      #endif

      /* free all calculations/totals in the calc list */
      code4calcReset( r4->codeBase ) ;

      /* free up the memory pools for totals and calcs */
      mem4release( r4->codeBase->totalMemory ) ;
      r4->codeBase->totalMemory =  0 ;
      mem4release( r4->codeBase->calcMemory ) ;
      r4->codeBase->calcMemory =  0 ;

      /* free the report caption */
      if( r4->report_caption )
      {
         u4free( r4->report_caption );
      }

      /* free the output data file name */
      if( r4->dfile_name )
         u4free( r4->dfile_name );

      /* free any data file output OUT4OBJ structs */
      while( (o4obj = (POUT4OBJ)l4pop(&r4->output_objs)) != NULL )
         u4free( o4obj );

      u4free( r4 ) ;
   }

   #ifdef S4WINDOWS
   /* Get an information context for either the specified or default printer */
   HDC S4FUNCTION report4get_printerIC ( REPORT4 *report )
   {
      char szPrinter [64] ;
      char *szDevice, *szDriver, *szOutput ;

      #ifdef __BORLANDC__
         #pragma warn -pia
      #endif

      if ( report->printer_name == 0 )
      {
         GetProfileString( "windows", "device", ",,,", szPrinter, 80 ) ;

         if ( ! (szDevice = strtok( szPrinter, ",")) )  return 0 ;
         if ( ! (szDriver = strtok( NULL, ", ")) ) return 0 ;
         if ( ! (szOutput = strtok( NULL, ", ")) ) return 0 ;
      }
      else
      {
         GetProfileString( "devices", report->printer_name, "=,", szPrinter, 80) ;
         szDevice = report->printer_name ;
         if ( ! (szDriver = strtok( szPrinter, ", ")) ) return 0 ;
         if ( ! (szOutput = strtok( NULL, ", ")) ) return 0 ;
      }

      #ifdef __BORLANDC__
         #pragma warn .pia
      #endif

      return CreateIC (szDriver, szDevice, szOutput, NULL) ;
   }

   /* Get the device context for the default printer */
   HDC S4FUNCTION report4get_printerDC ( void )
   {
      static char szPrinter [80] ;
      char *szDevice, *szDriver, *szOutput ;

      GetProfileString( "windows", "device", ",,,", szPrinter, 80 ) ;

      #ifdef __BORLANDC__
         #pragma warn -pia
      #endif

      if ( ! (szDevice = strtok( szPrinter, ",")) )  return 0 ;
      if ( ! (szDriver = strtok( NULL, ", ")) ) return 0 ;
      if ( ! (szOutput = strtok( NULL, ", ")) ) return 0 ;

      #ifdef __BORLANDC__
         #pragma warn .pia
      #endif

      return CreateDC (szDriver, szDevice, szOutput, NULL) ;
   }

   /* Create the output fonts for printing */
   void report4output_screen_fonts( REPORT4 *report )
   {
      STYLE4 *style_on;
      #ifdef S4WIN32
         LOGFONT *plf;
      #else
         PR4LOGFONT plf;
      #endif
      int tempval;

      style_on = (STYLE4 *)l4first( &report->styles );
      while( style_on )
      {
         if( !style_on->printer_font )
         {
            #ifdef S4WIN32
               plf = &( style_on->ntFont ) ;
            #else
               plf = &( style_on->lfont ) ;
            #endif
            /* due to the different resolution of the printer and screen we have
            to temporarily change the lfHeight member of the LOGFONT struct and
            create a font for printing */
            tempval = plf->lfHeight;
            plf->lfHeight = -MulDiv(style_on->point_size,
               GetDeviceCaps(report->printerDC,LOGPIXELSY),72);

            style_on->printer_font = CreateFontIndirect((const LOGFONT FAR*)plf);
            plf->lfHeight = tempval;

         }
         style_on = (STYLE4 *)l4next( &report->styles, style_on );
      }
   }


   /* USED FOR CREATING A DIALOG TEMPLATE ON THE FLY, this is used to create
      the dialog used during printing in windows to allow cancellation of the
      printing process, the only reason this function is necessary is because
      we do not use a resource file with the CodeBase DLL */
   #ifdef S4WIN32
   GLOBALHANDLE PrintDlgTemplate( void )
   {
      LPDLGITEMTEMPLATE lpDIT ;
      LPDLGTEMPLATE lpDT ;
      LPWSTR lpsztemp ;
      LPWORD lpw ;
      GLOBALHANDLE hDTemplate;

      hDTemplate = GlobalAlloc( GMEM_MOVEABLE | GMEM_ZEROINIT, 1024 ) ;

      if( hDTemplate == NULL )
         return hDTemplate;

      lpDT = (LPDLGTEMPLATE)GlobalLock( hDTemplate );

      lpDT->style = WS_VISIBLE | DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU ;
      lpDT->cdit = 0 ;
      lpDT->x = 40;
      lpDT->y = 40;
      lpDT->cx = 120;
      lpDT->cy = 40;

      lpw = (LPWORD) (lpDT + 1 ) ;
      *lpw++ = 0 ; /* no Menu*/
      *lpw++ = 0 ; /* predefined class*/

      lpsztemp = (LPWSTR)lpw ;
      lstrcpyW( lpsztemp, L"CodeReporter" ) ;
      lpw = (LPWORD) (lpsztemp + lstrlenW(lpsztemp) + 1 ) ;

   /* dlg items */

   /* creating static text */
      lpDIT = (LPDLGITEMTEMPLATE)lpw ;
      lpDIT->x = 4;
      lpDIT->y = 6;
      lpDIT->cx = 120 ;
      lpDIT->cy = 12 ;
      lpDIT->id = 99;
      lpDIT->style = WS_VISIBLE | WS_CHILD | SS_CENTER | WS_GROUP ;

      lpw = (LPWORD) (lpDIT+1) ;
      *lpw++ = 0xFFFF ;
      *lpw++ = 0x0082 ; /*static class*/
      lpsztemp = (LPWSTR) lpw ;
      lstrcpyW( lpsztemp, L"Cancel Printing" ) ;
      lpw = (LPWORD) (lpsztemp + lstrlenW(lpsztemp)+1) ;
      *lpw++ = 0 ;

      lpDT->cdit++ ;

   /* creating cancel button */
      lpDIT = (LPDLGITEMTEMPLATE)lpw ;
      lpDIT->x = 44;
      lpDIT->y = 22;
      lpDIT->cx = 32 ;
      lpDIT->cy = 14 ;
      lpDIT->id = 101;
      lpDIT->style = BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP ;

      lpw = (LPWORD) (lpDIT+1) ;
      *lpw++ = 0xFFFF ;
      *lpw++ = 0x0080 ; /* button class*/
      lpsztemp = (LPWSTR) lpw ;
      lstrcpyW( lpsztemp, L"&Cancel" ) ;
      lpw = (LPWORD) (lpsztemp + lstrlenW(lpsztemp)+1) ;
      *lpw++ = 0 ;

      lpDT->cdit++ ;

      GlobalUnlock( hDTemplate );

      return hDTemplate;
   }
   #else
   GLOBALHANDLE NewDlgTemplate(DWORD Style, WORD X, WORD Y, WORD CX, WORD CY,
                               LPSTR Menu, LPSTR Class, LPSTR Text,
                               LPSTR TypeFace, WORD PtSize)
   {
      DlgTemplateHeader far *lpDT;
      LPSTR lpsztemp;
      GLOBALHANDLE hDTemplate;
      long needed_size;

      needed_size = sizeof( DlgTemplateHeader ) + lstrlen(Menu) + 1 +
         lstrlen(Class) + 1 + lstrlen(Text) + 1;

      if( TypeFace[0] )
         needed_size += lstrlen(TypeFace) + 1 + sizeof(short);

      hDTemplate = GlobalAlloc( GMEM_MOVEABLE | GMEM_ZEROINIT, needed_size );

      if( hDTemplate == (GLOBALHANDLE)NULL )
         return hDTemplate;

      lpDT = (DlgTemplateHeader far * )GlobalLock( hDTemplate );

      lpDT->dtStyle = WS_VISIBLE | Style;
      lpDT->dtX = X;
      lpDT->dtY = Y;
      lpDT->dtCX = CX;
      lpDT->dtCY = CY;
      lpsztemp = ((LPSTR)lpDT) + sizeof( *lpDT );
      lpsztemp = (char far *)memcpy( lpsztemp, Menu, ((lstrlen(Menu)>80)?81:lstrlen(Menu)+1) );
      lpsztemp += lstrlen(Menu)+1;
      lpsztemp = (char far *)memcpy( lpsztemp, Class, ((lstrlen(Class)>80)?81:lstrlen(Class)+1)  );
      lpsztemp += lstrlen(Class)+1;
      lpsztemp = (char far *)memcpy( lpsztemp, Text, ((lstrlen(Text)>80)?81:lstrlen(Text)+1)  );
      lpsztemp += lstrlen(Text)+1;

      if( TypeFace[0] )
      {
         *((short far *)lpsztemp) = PtSize;
         memcpy( lpsztemp+sizeof(short), TypeFace,((lstrlen(TypeFace)>80)?81:lstrlen(TypeFace)+1)   );
         lpDT->dtStyle |= DS_SETFONT;
      }

      GlobalUnlock( hDTemplate );

      return hDTemplate;
   }
   #endif

   /* USED FOR CREATING A DIALOG TEMPLATE ON THE FLY, this is used to create
      the dialog used during printing in windows to allow cancellation of the
      printing process, the only reason this function is necessary is because
      we do not use a resource file with the CodeBase DLL */
   #ifndef S4WIN32
   BOOL AddDlgItem( GLOBALHANDLE DlgTmp, DWORD Style, WORD X, WORD Y, WORD CX,
                    WORD CY, LPSTR Class, LPSTR Text, BYTE DataLen, LPSTR Data,
                    int id )
   {
      DlgTemplateHeader far *lpDT;
      DlgItemTemplateHeader far *lpDIT;
      LPSTR lpsztemp;
      GLOBALHANDLE hDTemplate;
      unsigned new_size, BlockSize;

      BlockSize = (unsigned) TemplateSize( DlgTmp );

      new_size = BlockSize + sizeof(DlgItemTemplateHeader) +
                 lstrlen(Class) + 1 + lstrlen(Text) + 1 + 1;

      hDTemplate = GlobalReAlloc( DlgTmp, new_size, GMEM_ZEROINIT | GMEM_MOVEABLE );

      if( hDTemplate == (GLOBALHANDLE)NULL )
         return FALSE;

      lpDT = (DlgTemplateHeader far * )GlobalLock( hDTemplate );

      lpDIT = (DlgItemTemplateHeader far *)(((LPSTR)lpDT)+BlockSize);
      lpDIT->dtilX = X;
      lpDIT->dtilY = Y;
      lpDIT->dtilCX = ((int)CX < lpDT->dtCX)?CX:lpDT->dtCX;
      lpDIT->dtilCY = ((int)CY < lpDT->dtCY)?CY:lpDT->dtCY;
      lpDIT->dtilID = id;
      lpDIT->dtilStyle = WS_VISIBLE | WS_CHILD | Style;

      lpsztemp = (char far *)(lpDIT)+sizeof(*lpDIT);
      lpsztemp = (char far *)memcpy( lpsztemp, Class, ((lstrlen(Class)>80)?81:lstrlen(Class)+1)  );
      lpsztemp += lstrlen(Class)+1;
      lpsztemp = (char far *)memcpy( lpsztemp, Text, ((lstrlen(Text)>80)?81:lstrlen(Text)+1)  );
      lpsztemp += lstrlen(Text)+1;

      if( DataLen )
         memcpy( lpsztemp, Data, DataLen );

      lpDT->dtItemCount++;

      GlobalUnlock( hDTemplate );

      return TRUE;
   }

   /* USED FOR CREATING A DIALOG TEMPLATE ON THE FLY, this is used to create
      the dialog used during printing in windows to allow cancellation of the
      printing process, the only reason this function is necessary is because
      we do not use a resource file with the CodeBase DLL */
   int TemplateSize( GLOBALHANDLE hDlgTmp )
   {
      DlgTemplateHeader far *lpDT;
      LPSTR lpstr, lpbase;
      int isize;
      int i;

      lpDT = (DlgTemplateHeader far *)GlobalLock( hDlgTmp );
      lpbase = (LPSTR)lpDT;
      lpstr = (LPSTR)( lpDT + 1 );
      isize = sizeof( *lpDT );
      lpstr = lpbase+(isize+=(lstrlen(lpstr)+1));
      lpstr = lpbase+(isize+=(lstrlen(lpstr)+1));
      lpstr = lpbase+(isize+=(lstrlen(lpstr)+1));

      if((lpDT->dtStyle)&DS_SETFONT)
      {
         isize += sizeof(short);
         lpstr += sizeof(short);
         lpstr = lpbase+(isize+=(lstrlen(lpstr)+1));
      }

      for( i = 0; i < lpDT->dtItemCount; i++ )
      {
         lpstr = lpbase+(isize+=sizeof(DlgItemTemplateHeader));
         lpstr = lpbase+(isize+=(lstrlen(lpstr)+1));
         lpstr = lpbase+(isize+=(lstrlen(lpstr)+1));
         if( *lpstr )
         {
            isize += *lpstr;
            lpstr += *lpstr;
         }
         isize++;
      }

      GlobalUnlock( hDlgTmp );
      return isize;
   }
   #endif
   /**************************************************************************
   * registers window classes for the output window and the page window
   **************************************************************************/
   int report4register_classes( PREPORT4 report )
   {
      WNDCLASS wc, temp ;
      HINSTANCE reginst;

      if( report->hWnd == (HWND)NULL )
      {
         #ifdef S4JAY
            MessageBox( NULL, "No parentwindow in report structure.", "Error", MB_OK );
         #endif
         return -1;
      }
      #ifdef S4WIN32
         reginst = (HINSTANCE)GetWindowLong( report->hWnd, GWL_HINSTANCE );
      #else
         reginst = (HINSTANCE)GetWindowWord( report->hWnd, GWW_HINSTANCE );
      #endif

/* PreViewWin */
      if( GetClassInfo( reginst, "PreViewWin" , &temp ) )
         UnregisterClass( "PreViewWin", reginst ) ;

      wc.cbClsExtra     = 0;
      wc.hInstance      = reginst;
      wc.hCursor        = LoadCursor((HINSTANCE)NULL,IDC_WAIT);
      wc.hbrBackground  = (HBRUSH)GetStockObject(LTGRAY_BRUSH);
      wc.lpszMenuName   = NULL;
      wc.style = CS_DBLCLKS | CS_GLOBALCLASS;
      wc.lpfnWndProc = PreViewProc;
      wc.cbWndExtra = PWIN_OFF_OLDOBJ + sizeof(long);
      wc.hIcon = (HICON)NULL;
      wc.lpszClassName = "PreViewWin";
      #ifdef S4JAY
         if( (RegisterClass( &wc )) == NULL )
         {
            MessageBox( NULL, "Failed to register PreViewWin class", "ERROR", MB_OK );
            return -1;
         }
      #else
         RegisterClass( &wc );
      #endif

/* OutputPageWin */
      if( GetClassInfo( reginst, "OutputPageWin" , &temp ) )
         UnregisterClass( "OutputPageWin", reginst ) ;

      wc.hbrBackground = (HBRUSH)GetStockObject( WHITE_BRUSH );
      wc.lpfnWndProc   = OutputPageProc;
      wc.lpszClassName = "OutputPageWin";
      #ifdef S4JAY
         if( (RegisterClass( &wc )) == NULL )
         {
            MessageBox( NULL, "Failed to register OutputPageWin class", "ERROR", MB_OK );
            return -1;
         }
      #else
         RegisterClass( &wc );
      #endif

/* MouseEat */
      if( GetClassInfo( reginst, "MouseEat" , &temp ) )
         UnregisterClass( "MouseEat", reginst ) ;

      wc.lpszClassName = "MouseEat";
      wc.lpfnWndProc   = MouseEatProc;
      #ifdef S4JAY
         if( (RegisterClass( &wc )) == NULL )
         {
            MessageBox( NULL, "Failed to register MouseEat class", "ERROR", MB_OK );
            return -1;
         }
      #else
         RegisterClass( &wc );
      #endif

      return 0;
   }

   #endif

   /****************************************************************************
   * recursively calculates the position (x,y) and size (w,h) of an object and
   * all the contained objects in device coordinates
   ****************************************************************************/

   #ifdef S4WINDOWS
      void report4obj_to_dev(OBJ4 *obj, HDC hDC )
   #else
      void report4obj_to_dev(OBJ4 *obj )
   #endif
   {
      OBJ4 *obj_on;
      #ifdef S4WINDOWS
         POINT pt[2];

         /* use the device context */
         pt[0].x = (int)obj->x;
         pt[0].y = (int)obj->y;
         pt[1].x = (int)obj->w;
         pt[1].y = (int)obj->h;
         LPtoDP( hDC, pt, 2 );
         obj->dev_x = pt[0].x;
         obj->dev_y = ABS5(pt[0].y);
         obj->dev_w = pt[1].x;
         obj->dev_h = ABS5(pt[1].y);
      #else
         /* in the non-windows case we simply transfer the 1000's of an inch
            values */
         obj->dev_x = (obj->x);
         obj->dev_y = (obj->y);
         obj->dev_w = (obj->w);
         obj->dev_h = (obj->h);
      #endif

      obj_on = (OBJ4 *)l4first( &obj->contained );
      while( obj_on )
      {
         #ifdef S4WINDOWS
            report4obj_to_dev( obj_on, hDC );
         #else
            report4obj_to_dev( obj_on );
         #endif
         obj_on = (OBJ4 *)l4next( &obj->contained, obj_on );
      }
      return;
   }

   /**************************************************************************
   * calculates group and object sizes in device coordinates
   ***************************************************************************/
   #ifdef S4WINDOWS
      #ifdef __cplusplus
         void report4calc_obj_dev(REPORT4 *report, HDC hDC )
      #else
         void report4calc_obj_dev(report, hDC )
         REPORT4 *report ;
         HDC hDC  ;
      #endif
   #else
      #ifdef __cplusplus
         void report4calc_obj_dev(REPORT4 *report )
      #else
         void report4calc_obj_dev(report )
         REPORT4 *report  ;
      #endif
   #endif
   {
      GROUP4 *group_on;
      AREA4 *area_on;
      OBJ4 *obj_on;
      #ifdef S4WINDOWS
         POINT pt[2];

         SetMapMode( hDC, MM_HIENGLISH );
         SetMapMode( hDC, MM_ANISOTROPIC );

         if( report->output_code == 0 )
         {
            pt[0].x = (int)(report->margin_left - report->hard_margin_left);
            pt[0].y = (int)(report->margin_top - report->hard_margin_top);
            pt[1].x = (int)(report->margin_right - report->hard_margin_right);
            pt[1].y = (int)(report->margin_bottom - report->hard_margin_bottom);
         }
         else
         {
            pt[0].x = (int)(report->margin_left);
            pt[0].y = (int)(report->margin_top);
            pt[1].x = (int)(report->margin_right);
            pt[1].y = (int)(report->margin_bottom);
         }
         LPtoDP( hDC, pt, 2 );
         report->dev_margin_left = pt[0].x;
         report->dev_margin_top  = ABS5(pt[0].y);
         report->dev_margin_right = pt[1].x;
         report->dev_margin_bottom = ABS5(pt[1].y);
      #else
         report->dev_margin_left = (report->margin_left);
         report->dev_margin_right = (report->margin_right);
         report->dev_margin_top = (report->margin_top);
         report->dev_margin_bottom = (report->margin_bottom);
      #endif

      group_on = report->page_header_footer;
      if( group_on )
      {
         area_on = (AREA4 *)l4first( &group_on->header_areas );
         while( area_on )
         {
            #ifdef S4WINDOWS
               pt[0].x = 0;
               pt[0].y = (UINT)area_on->height;
               LPtoDP( hDC, pt, 1 );
               area_on->height_dev = ABS5(pt[0].y);
            #else
               area_on->height_dev = (int)(area_on->height);
            #endif
            obj_on = (OBJ4 *)l4first( &area_on->objects );
            while( obj_on )
            {
               #ifdef S4WINDOWS
                  report4obj_to_dev( obj_on, hDC );
               #else
                  report4obj_to_dev( obj_on );
               #endif
               obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
            }
            area_on = (AREA4 *)l4next( &group_on->header_areas, area_on );
         }

         area_on = (AREA4 *)l4first( &group_on->footer_areas );
         while( area_on )
         {
            #ifdef S4WINDOWS
               pt[0].x = 0;
               pt[0].y = (UINT)area_on->height;
               LPtoDP( hDC, pt, 1 );
               area_on->height_dev = ABS5(pt[0].y);
            #else
               area_on->height_dev = (int)(area_on->height);
            #endif

            obj_on = (OBJ4 *)l4first( &area_on->objects );
            while( obj_on )
            {
               #ifdef S4WINDOWS
                  report4obj_to_dev( obj_on, hDC );
               #else
                  report4obj_to_dev( obj_on );
               #endif
               obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
            }
            area_on = (AREA4 *)l4next( &group_on->footer_areas, area_on );
         }
      }

      group_on = (GROUP4 *)l4first( &report->groups );
      while( group_on )
      {
         area_on = (AREA4 *)l4first( &group_on->header_areas );
         while( area_on )
         {
            #ifdef S4WINDOWS
               pt[0].x = 0;
               pt[0].y = (UINT)area_on->height;
               LPtoDP( hDC, pt, 1 );
               area_on->height_dev = ABS5(pt[0].y);
            #else
               area_on->height_dev = (int)(area_on->height);
            #endif

            obj_on = (OBJ4 *)l4first( &area_on->objects );
            while( obj_on )
            {
               #ifdef S4WINDOWS
                  report4obj_to_dev( obj_on, hDC );
               #else
                  report4obj_to_dev( obj_on );
               #endif
               obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
            }
            area_on = (AREA4 *)l4next( &group_on->header_areas, area_on );
         }

         area_on = (AREA4 *)l4first( &group_on->footer_areas );
         while( area_on )
         {
            #ifdef S4WINDOWS
               pt[0].x = 0;
               pt[0].y = (UINT)area_on->height;
               LPtoDP( hDC, pt, 1 );
               area_on->height_dev = ABS5(pt[0].y);
            #else
               area_on->height_dev = (int)(area_on->height);
            #endif

            obj_on = (OBJ4 *)l4first( &area_on->objects );
            while( obj_on )
            {
               #ifdef S4WINDOWS
                  report4obj_to_dev( obj_on, hDC );
               #else
                  report4obj_to_dev( obj_on );
               #endif
               obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on );
            }
            area_on = (AREA4 *)l4next( &group_on->footer_areas, area_on );
         }
         group_on = (GROUP4 *)l4next( &report->groups, group_on );
      }
   }


   /* make a call to the relate4querySet() function */
   int S4FUNCTION report4querySet( PREPORT4 report, char S4PTR *expr )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      return relate4querySet( report->relate, expr );
   }

   /* make a call to the relate4sortSet() function */
   int S4FUNCTION report4sortSet( PREPORT4 report, char S4PTR *expr )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      return relate4sortSet( report->relate, expr );
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4caption( PREPORT4 report, char S4PTR *caption )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      if( report->report_caption )
         u4free( report->report_caption );

      report->report_caption = NULL;

      if( !caption )
         return 0;

      report->report_caption = (char *)u4allocFree( report->codeBase, strlen(caption)+1 );
      if( !report->report_caption )
      {
         error4describe( report->codeBase, e4report, 0L, E4_REP_CAPALLOC, 0, 0 );
         return -1;
      }

      strcpy( report->report_caption, caption );

      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4hardResets( PREPORT4 report, int reset_flag )
   {
      int temp;

      if( !report )
         return -1;

      if( reset_flag < 0 )
      {
         error4describe( report->codeBase, e4parm, 0L, E4_REP_BADRESET, 0, 0 );
         return -1;
      }

      temp = report->hard_resets_flag;
      report->hard_resets_flag = reset_flag;

      return temp;
   }

   #ifdef S4WINDOWS
      /* Calls the printer setup/seletion common dialog in Windows */
      short S4FUNCTION report4printerSelect( PREPORT4 report )
      {
         static PRINTDLG pd;

         #ifdef E4PARM_HIGH
                 if( report == 0 )
                         return error4( 0, e4parm_null, E95702 ) ;
         #endif

         memset( &pd, 0, sizeof(PRINTDLG) );
         pd.lStructSize = sizeof( PRINTDLG );
         pd.hwndOwner = report->hWnd;
         pd.Flags = PD_RETURNDC | PD_PRINTSETUP;
         pd.hDevMode = (HGLOBAL)NULL;
         pd.hDevNames = (HGLOBAL)NULL;

         if( PrintDlg( &pd ) != 0 )
         {
            report->printerDC = pd.hDC;
            return 0;
         }
         else
         {
            return -1;
         }
      }
   #endif

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4output( PREPORT4 report, int output_handle, int use_styles )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      report->output_handle = output_handle;
      report->use_styles    = use_styles;

      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4pageSize( PREPORT4 report, long pg_height, long pg_width, int unit_type )
   {
      long pgh = pg_height, pgw = pg_width;

      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      if( unit_type )
      {
         pgw = (long)( pg_width * 100 );
         pgh = (long)(((float)pg_height * 1000.0)/6.0 + 0.5 );
      }

      report->report_width = pgw- report->margin_left - report->margin_right;
      report->report_height = pgh - report->margin_top - report->margin_bottom;

      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4margins( PREPORT4 report, long left, long right, long top, long bottom, int unit_type )
   {
      long rm = right, lm = left, tm = top, bm = bottom;

      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      if( unit_type )
      {
         rm = right * 100;
         lm = left * 100;
         tm = (int)(((float)top*1000.0)/6.0);
         bm = (int)(((float)bottom*1000.0)/6.0);
      }

      report->report_width = report->report_width + report->margin_left + report->margin_right;
      report->report_height = report->report_height + report->margin_top + report->margin_bottom;

      report->margin_right = rm;
      report->margin_left  = lm;
      report->margin_top   = tm;
      report->margin_bottom = bm;

      report->report_width = report->report_width - rm - lm;
      report->report_height = report->report_height - tm - bm;

      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4currency( PREPORT4 report, char *currency )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      if( !currency )
         report->currency_symbol[0] = '\0';
      else
         u4ncpy( report->currency_symbol, currency, sizeof(report->currency_symbol) );
      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4decimal( PREPORT4 report, char decimal )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      report->decimal_point = decimal;
      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4separator( PREPORT4 report, char separator )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      report->thousand_separator = separator;
      return 0;
   }


   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4titlePage( PREPORT4 report, int title_page )
   {
      int temp;

      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      if( title_page < 0 )
      {
         error4describe( report->codeBase, e4parm, 0L, E4_REP_BADTITLE, 0, 0 );
         return -1;
      }

      temp = report->pgbrk_title;
      report->pgbrk_title = title_page;

      return temp;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4toScreen( PREPORT4 report, short to_screen )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      if( to_screen < 0 )
      {
         error4describe( report->codeBase, e4parm, 0L, E4_REP_BADSCREEN, 0, 0 );
         return -1;
      }

      if( to_screen > 0 )
         report->output_code = to_screen;
      else
         report->output_code = 0;

      return 0;
   }

   #ifdef S4WINDOWS
      /* SEE THE CODEREPORTER MANUAL */
      int S4FUNCTION report4parent( PREPORT4 report, HWND parent )
      {
         #ifdef E4PARM_HIGH
                 if( report == 0 )
                         return error4( 0, e4parm_null, E95702 ) ;
         #endif

         report->hWnd = parent;
         return 0;
      }

      /* SEE THE CODEREPORTER MANUAL */
      HDC S4FUNCTION report4printerDC( PREPORT4 report, HDC hDC )
      {
         HDC temp;

         #ifdef E4PARM_HIGH
                 if( report == 0 )
                         return (HDC)error4( 0, e4parm_null, E95702 ) ;
         #endif

         temp = report->printerDC;
         report->printerDC = hDC;

         return temp;
      }
   #endif

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4dateFormat( PREPORT4 report, char *date_format  )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      if( !date_format )
      {
         return -1;
      }

      u4ncpy( report->default_date_format, date_format, sizeof(report->default_date_format) );
      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4pageSizeGet( PREPORT4 report, long *page_width, long *page_height )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      *page_width = report->dev_page_width;
      *page_height = report->dev_page_height;

      return 0;
   }

   /* SEE THE CODEREPORTER MANUAL */
   int S4FUNCTION report4pageMarginsGet( PREPORT4 report,
                        long S4PTR * margin_left, long S4PTR *margin_right,
                        long S4PTR *margin_top, long S4PTR *margin_bottom )
   {
      #ifdef E4PARM_HIGH
              if( report == 0 )
                      return error4( 0, e4parm_null, E95702 ) ;
      #endif

      *margin_right = report->dev_margin_right;
      *margin_left = report->dev_margin_left;
      *margin_bottom = report->dev_margin_bottom;
      *margin_top = report->dev_margin_top;

      return 0;

   }

   /* SEE THE CODEREPORTER MANUAL */
   #ifndef S4OFF_WRITE
      int S4FUNCTION report4dataDo( PREPORT4 report )
      {
         char *f4info;
         FIELD4INFO *f4infoi;
         DATA4 *dfile;
         POUT4OBJ oobj;
         long bsize = 8198;
         int  rc, i, trl;
         PGROUP4 group;
         #ifdef S4CLIENT
            int oldExclusive ;
         #endif

         #ifdef E4PARM_HIGH
                 if( report == 0 )
                         return error4( 0, e4parm_null, E95702 ) ;
         #endif

         if( !report->output_group || !report->dfile_name || report->output_objs.nLink <= 0 )
         {
            error4describe( report->codeBase, e4repData, 0L, E4_REP_NODSETUP, (char *)0, (char *)0 );
            return -1;
         }

         /* allocate an array of FIELD4INFO structures*/
         f4info = (char *)u4allocEr( report->codeBase, (report->output_objs.nLink+1)*sizeof(FIELD4INFO) );
         if( !f4info )
         {
            error4describe( report->codeBase, e4repData, 0L, E4_REP_NODCREATE, report->dfile_name, (char *)0 );
            return -1;
         }

         /* fill the FIELD4INFO structures*/
         oobj = NULL;
         for( i = 0; i < (int)report->output_objs.nLink; i++ )
         {
            f4infoi = (FIELD4INFO *)(f4info + i * sizeof(FIELD4INFO) );
            oobj = (POUT4OBJ)l4next( &report->output_objs, oobj );
            f4infoi->name = (char *)u4allocFree( report->codeBase, strlen(oobj->obj->field_name)+1 );
            strcpy( f4infoi->name, oobj->obj->field_name );
            f4infoi->type = oobj->obj->field_type;
            f4infoi->len = oobj->obj->field_len;
            f4infoi->dec = oobj->obj->field_dec;
         }

         trl = report->codeBase->readOnly;
         report->codeBase->readOnly = 0;
         #ifdef S4CLIENT
            oldExclusive = report->codeBase->accessMode ;
            report->codeBase->accessMode = 1 ;
         #endif

         /* create the new data file*/
         dfile = d4create( report->codeBase, report->dfile_name, (FIELD4INFO *)f4info, NULL );
         if( !dfile )
         {
            error4describe( report->codeBase, e4repData, 0L, E4_REP_NODCREATE, report->dfile_name, (char *)0 );
            u4free( f4info );
            report->codeBase->readOnly = trl;
            #ifdef S4CLIENT
               report->codeBase->accessMode = oldExclusive ;
            #endif
            return -1;
         }
         report->codeBase->readOnly = trl;
         #ifdef S4CLIENT
            report->codeBase->accessMode = oldExclusive ;
         #endif

         /* associate the fields in the new data file with the output objects in the*/
         /* report*/
         oobj = NULL;
         for( i = 0; i < (int)report->output_objs.nLink; i++ )
         {
            oobj = (POUT4OBJ)l4next( &report->output_objs, oobj );
            oobj->obj->field = d4field( dfile, oobj->obj->field_name );
         }

         /* temporarily remove all the internal group flags*/
         group = (PGROUP4) l4first( &report->groups );
         while( group )
         {
            group->trepeat_header = group->repeat_header;
            group->tswap_header = group->swap_header;
            group->tswap_footer = group->swap_footer;
            group->treset_page = group->reset_page;
            group->treset_pagenum = group->reset_pagenum;

            group->repeat_header = 0;
            group->swap_header   = 0;
            group->swap_footer   = 0;
            group->reset_page    = 0;
            group->reset_pagenum = 0;
            group = (PGROUP4)l4next( &report->groups, group );
         }

         report->for_dbf = 1;
         report->data_file = dfile;
         report->rcount = 0;
         report->dfile_buf = NULL;

         #ifndef S4CLIENT
         /* allocate as large a buffer as you can*/
         while( report->dfile_buf == NULL )
         {
            report->dfile_buf = (char *)u4allocFree( report->codeBase, bsize );
            if( report->dfile_buf == NULL )
               bsize -= 1000;
            if( bsize <= 0 )
               break;
         }
         if( report->dfile_buf == NULL )
         #ifndef __SC__
            return 0;
         #else
            return 0;
         #endif
         #endif /* S4CLIENT */

         /* set up the new datafile for fast writing*/
         #ifndef S4CLIENT
            file4seqWriteInitLow( &report->dfile_seq, &dfile->dataFile->file, dfile4recordPosition(dfile->dataFile, 1L),
                                 report->dfile_buf, (unsigned)bsize );
         #endif
         d4appendStart( dfile, 0 );
         if( f4info )
         {
            for( i = 0; i < (int)report->output_objs.nLink; i++ )
            {
               f4infoi = (FIELD4INFO *)(f4info + i * sizeof(FIELD4INFO) );
               u4free( f4infoi->name );
            }
            u4free( f4info );
         }

         /* start the report*/
         report4pageInit( report );
         #ifdef S4WINDOWS
            if( report->hWnd2 )
            {
               ReleaseCapture();
               DestroyWindow( report->hWnd2 );
               report->hWnd2 = (HWND)NULL;
            }
            while( (rc = report4generatePage( report, 0 )) >= 0 )
         #else
            while( (rc = report4generatePage( report )) >= 0 )
         #endif
         {
            if( rc == 2 )
               break;
         }
         /* clean up after the report*/
         report4pageFree( report );

         report->for_dbf = 0;

         /* flush the buffer to the new data file*/
         #ifndef S4CLIENT
            file4seqWriteFlush( &report->dfile_seq );
            #ifndef S4OFF_MULTI
               report->data_file->dataFile->file.lowAccessMode = 1;
            #endif
            report->data_file->dataFile->numRecs = report->rcount;
            dfile4updateHeader( report->data_file->dataFile, 1, 1, 0 );
         #endif
         d4close( report->data_file );

         #ifndef S4CLIENT
            if( report->dfile_buf )
            {
               u4free( report->dfile_buf );
               report->dfile_buf = NULL;
            }
         #endif

         group = (PGROUP4) l4first( &report->groups );
         while( group )
         {
            group->repeat_header = group->trepeat_header;
            group->swap_header   = group->tswap_header;
            group->swap_footer   = group->tswap_footer;
            group->reset_page    = group->treset_page;
            group->reset_pagenum = group->treset_pagenum;

            group = (PGROUP4)l4next( &report->groups, group );
         }

         oobj = NULL;
         for( i = 0; i < (int)report->output_objs.nLink; i++ )
         {
            oobj = (POUT4OBJ)l4next( &report->output_objs, oobj );
            oobj->obj->field = NULL;
         }


         return 0;
      }
   #endif  /* !S4OFF_WRITE */

   int S4FUNCTION report4separator_v( PREPORT4 report, char *separator )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( report, 6, 0 ) ) return -1 ;
      #endif

      #ifndef S4VB_DOS
         return report4separator( report, (char) separator[0] ) ;
      #else
         return report4separator( report, (char) c4str(separator)[0] ) ;
      #endif
   }
#endif  /* !S4OFF_REPORT */
