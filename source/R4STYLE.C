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

/* r4style.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4OFF_REPORT

PSTYLE4 S4FUNCTION style4lookup( REPORT4 *report, char *style_name )
{
   STYLE4 *style_on ;

   if( report == NULL || style_name == NULL )
      return NULL ;

   style_on = (STYLE4 *)l4first( &report->styles ) ;
   while( style_on )
   {
      //#ifdef S4WINDOWS
      //if( !lstrcmp( (LPCSTR)style_name, (LPCSTR)style_on->name ) )
      //#else
      if( !strcmp( (char *)style_name, (char *)style_on->name ) )
      //#endif
         return style_on ;

      style_on = (STYLE4 *)l4next( &report->styles, style_on ) ;
   }
   return NULL ;
}

BOOL S4FUNCTION style4free( REPORT4 *report, STYLE4 *style )
{
   if( report == NULL || style == NULL )
      return FALSE ;


   if( report )
      l4remove( &report->styles, style ) ;

   #ifdef S4WINDOWS
      if( style->screen_font )
      {
         DeleteObject( style->screen_font ) ;
         style->screen_font = (R4HFONT)NULL ;
      }

      if( style->printer_font )
      {
         DeleteObject( style->printer_font ) ;
         style->printer_font = (R4HFONT)NULL ;
      }
   #endif
   if( style->codes_before )
   {
      u4free( style->codes_before ) ;
      style->codes_before = NULL ;
      style->codes_before_len = 0 ;
   }
   if( style->codes_after )
   {
      u4free( style->codes_after ) ;
      style->codes_after = NULL ;
      style->codes_after_len = 0 ;
   }

   u4free( style ) ;
   return TRUE ;
}

BOOL S4FUNCTION style4delete( REPORT4 *report, char *style_name )
{
   STYLE4 *style_on ;

   if( report == NULL || style_name == NULL )
      return FALSE ;

   style_on = style4lookup( report, style_name ) ;
   if( !style_on )
      return FALSE ;

   style4free( report, style_on ) ;
   return TRUE ;

}

int S4FUNCTION style4color( PSTYLE4 style, R4COLORREF color )
{
   if( style == NULL )
      return -1 ;

   style->color = color ;
   return 0 ;
}

int S4FUNCTION style4default_set( PREPORT4 report, PSTYLE4 style )
{
   if( report == NULL || style == NULL )
      return -1 ;

   report->active_style = style ;

   return 0 ;
}

#ifdef S4WINDOWS
STYLE4 FAR* S4FUNCTION style4create( REPORT4 *report, PR4LOGFONT lfont,
                                          char *name, R4COLORREF color, int point_size )
{
   STYLE4 *style = NULL ;
   #ifdef S4WIN32
      LOGFONT ntFont ;
   #endif
   HDC hDC ;

   if( !name || name[0] == '\0' )
   {
      if( report )
         error4describe( report->codeBase, e4parm, 0L, E4_REP_NOSNAM, 0, 0 ) ;
      return NULL ;
   }

   if( report )
   {
      style = style4lookup( report, name ) ;
      if( style != NULL )
      {
         error4describe( report->codeBase, e4styleCreate, 0L, E4_REP_DUPSNAM, 0, 0 ) ;
         return NULL ;
      }
   }

   if( report )
      style = (PSTYLE4)u4allocFree( report->codeBase, sizeof(STYLE4) ) ;
   else
      style = (PSTYLE4)u4alloc( sizeof(STYLE4) ) ;

   if( style == NULL )
      return NULL ;

   style->color = color ;
   style->point_size = point_size ;
   u4ncpy( style->name, name, sizeof(style->name) ) ;
   memcpy( &style->lfont, lfont, sizeof(R4LOGFONT) ) ;

   /* lfHeight conversion is required to ensure retrieval of report from */
   /* a different resolution still creates the same point size font. */

   hDC = GetDC( NULL ) ;
   //style->lfont.lfHeight = -MulDiv( point_size, GetDeviceCaps( hDC, LOGPIXELSY ), 72 ) ;
   style->lfont.lfHeight = (long)(-1 * (LONGLONG)point_size * (LONGLONG)GetDeviceCaps( hDC, LOGPIXELSY ) / (LONGLONG)72) ;
   ReleaseDC ( NULL, hDC ) ;

   #ifdef S4WIN32
      ntFont.lfHeight         = lfont->lfHeight         ;
      ntFont.lfWidth          = lfont->lfWidth          ;
      ntFont.lfEscapement     = lfont->lfEscapement     ;
      ntFont.lfOrientation    = lfont->lfOrientation    ;
      ntFont.lfWeight         = lfont->lfWeight         ;
      ntFont.lfItalic         = lfont->lfItalic         ;
      ntFont.lfUnderline      = lfont->lfUnderline      ;
      ntFont.lfStrikeOut      = lfont->lfStrikeOut      ;
      ntFont.lfCharSet        = lfont->lfCharSet        ;
      ntFont.lfOutPrecision   = lfont->lfOutPrecision   ;
      ntFont.lfClipPrecision  = lfont->lfClipPrecision  ;
      ntFont.lfQuality        = lfont->lfQuality        ;
      ntFont.lfPitchAndFamily = lfont->lfPitchAndFamily ;
      lstrcpy( ntFont.lfFaceName, (LPCTSTR)lfont->lfFaceName ) ;
      memcpy( &style->ntFont, &ntFont, sizeof(LOGFONT) ) ;
      style->screen_font =  CreateFontIndirect( (const LOGFONT FAR*)&style->ntFont ) ;
      if ( style->screen_font == NULL )
      {
         u4free( style ) ;
         return NULL ;
      }
   #else
      style->screen_font =  CreateFontIndirect( (const LOGFONT FAR*)&style->lfont ) ;
      if ( style->screen_font == (R4HFONT)NULL )
      {
         u4free( style ) ;
         return NULL ;
      }
   #endif

   style->codes_before_len = style->codes_after_len = 0 ;
   style->codes_before = NULL ;
   style->codes_after = NULL ;

   if( report )
   {
      style->menu_id = 600 + report->styles.nLink ;
      l4add( &report->styles, style ) ;

      if ( report->active_style == NULL )
         report->active_style =  style ;
   }

   return style ;
}
#else

PSTYLE4 S4FUNCTION style4create( PREPORT4 report, char *name, int before_len, char *before, int after_len, char *after )
{
   STYLE4 *style = NULL ;

   if( !name || name[0] == '\0' )
   {
      if( report )
         error4describe( report->codeBase, e4parm, 0L, E4_REP_NOSNAM, 0, 0 ) ;
      return NULL ;
   }

   if( report )
   {
      style = style4lookup( report, name ) ;
      if( style != NULL )
      {
         error4describe( report->codeBase, e4styleCreate, 0L, E4_REP_DUPSNAM, 0, 0 ) ;
         return NULL ;
      }
   }

   if( report )
      style = (PSTYLE4)u4allocFree( report->codeBase, sizeof(STYLE4) ) ;
   else
      style = (PSTYLE4)u4alloc( sizeof(STYLE4) ) ;

   if( style == NULL )
      return NULL ;

   u4ncpy( style->name, name, sizeof( style->name ) ) ;

   if( before_len > 0 && before != NULL )
   {
      style->codes_before_len = before_len ;
      style->codes_before = (char *)u4alloc( before_len ) ;
      if( style->codes_before == NULL )
         return NULL ;

      memcpy( style->codes_before, before, before_len ) ;
   }

   if( after_len >0 && after != NULL )
   {
      style->codes_after_len = after_len ;
      style->codes_after = (char *)u4alloc( after_len ) ;
      if( style->codes_after == NULL )
      {
         if( style->codes_before != NULL )
            u4free( style->codes_before ) ;
         return NULL ;
      }
      memcpy( style->codes_after, after, after_len ) ;
   }

   if( report )
   {
      l4add( &report->styles, style ) ;

      if( report->active_style == NULL )
         report->active_style = style ;
   }

   return style ;
}

#endif

void style4swap( POBJ4 obj, PSTYLE4 old, PSTYLE4 newstyle )
{
   POBJ4 obj_on ;

   if( obj->style == old )
      obj->style = newstyle ;

   obj_on = (POBJ4)l4first( &obj->contained ) ;
   while( obj_on )
   {
      style4swap( obj_on, old, newstyle ) ;
      obj_on = (POBJ4)l4next( &obj->contained, obj_on ) ;
   }
}

void style4changeLoadedStyles( REPORT4 *report, STYLE4 *old_style, STYLE4 *new_style )
{
   GROUP4 *group_on ;
   AREA4 *area_on ;
   OBJ4 *obj_on ;


   area_on = (AREA4 *)l4first( &report->page_header_footer->header_areas ) ;
   while( area_on )
   {
      obj_on = (OBJ4 *)l4first( &area_on->objects ) ;
      while( obj_on )
      {
         if( obj_on->style == old_style )
         {
            style4swap( obj_on, old_style, new_style ) ;
         }
         obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on ) ;
      }
      area_on = (AREA4 *)l4next( &report->page_header_footer->header_areas, area_on ) ;
   }

   area_on = (AREA4 *)l4first( &report->page_header_footer->footer_areas ) ;
   while( area_on )
   {
      obj_on = (OBJ4 *)l4first( &area_on->objects ) ;
      while( obj_on )
      {
         if( obj_on->style == old_style )
         {
            style4swap( obj_on, old_style, new_style ) ;
         }
         obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on ) ;
      }
      area_on = (AREA4 *)l4next( &report->page_header_footer->footer_areas, area_on ) ;
   }

   group_on = (GROUP4 *)l4first( &report->groups ) ;
   while( group_on )
   {
      area_on = (AREA4 *)l4first( &group_on->header_areas ) ;
      while( area_on )
      {
         obj_on = (OBJ4 *)l4first( &area_on->objects ) ;
         while( obj_on )
         {
            if( obj_on->style == old_style )
            {
               style4swap( obj_on, old_style, new_style ) ;
            }
            obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on ) ;
         }
         area_on = (AREA4 *)l4next( &group_on->header_areas, area_on ) ;
      }

      area_on = (AREA4 *)l4first( &group_on->footer_areas ) ;
      while( area_on )
      {
         obj_on = (OBJ4 *)l4first( &area_on->objects ) ;
         while( obj_on )
         {
            if( obj_on->style == old_style )
            {
               style4swap( obj_on, old_style, new_style ) ;
            }
            obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on ) ;
         }
         area_on = (AREA4 *)l4next( &group_on->footer_areas, area_on ) ;
      }
      group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
   }

}

int S4FUNCTION report4styleSheetSave( REPORT4 *report, char *file_name )
{
   CODE4 *c4 ;
   FILE4 file ;
   FILE4SEQ_WRITE seq ;
   char name_buf[512], buf[1024] ;
   short sval ;
   STYLE4 *style_on ;
   int errCreate, safety ;

   #ifdef E4PARM_HIGH
      if( report == 0 )
         return error4( 0, e4parm_null, E95702 ) ;
   #endif

   c4 = report->codeBase ;

   u4ncpy( name_buf, file_name, sizeof(name_buf) ) ;
   u4nameExt( name_buf, sizeof(name_buf), "CRS", 0 ) ;
   errCreate = c4->errCreate ;
   safety = c4->safety ;

   c4->errCreate = 0 ;
   c4->safety = 0 ;

   if( (file4create( &file, c4, name_buf, 1 )) != 0 )
   {
      error4set( c4, 0 ) ;
      c4->errCreate = errCreate ;
      c4->safety = safety ;
      return -1 ;
   }

   file4seqWriteInit( &seq, &file, 0L, buf, sizeof(buf) ) ;

   // AS Dec 13/05 - under Windows strcat is becoming deprecated...
   int len = strlen( name_buf ) + 3 ;
   char *temp = (char *)u4allocErr( c4, len ) ;
   temp[0] = STYLE_MASK ;
   temp[1] = VERSION_MASK ;
   temp[2] = '\0' ;
   c4strcat(temp, len, name_buf) ;

   file4seqWrite( &seq, temp, 2 ) ;

   sval = (short)report->styles.nLink ;
   file4seqWrite( &seq, &sval, sizeof(short) ) ;

   style_on = (STYLE4 *)l4first( &report->styles ) ;
   while( style_on )
   {
      report4save_style( &seq, style_on ) ;
      style_on = (STYLE4 *)l4next( &report->styles, style_on ) ;
   }

   file4seqWriteFlush( &seq ) ;
   file4close( &file ) ;

   c4->errCreate = errCreate ;
   c4->safety = safety ;
   u4free( temp ) ;

   return 0 ;
}

int S4FUNCTION report4styleSheetLoad( REPORT4 *report, char *file_name, int override )
{
   FILE4 file ;
   FILE4SEQ_READ seq ;
   FILE4LONG pos ;
   int autoOpen, errCreate, errExpr, errOpen ;
   char name_buf[512] ;
   int m1, m2, m3, m4, i ;
   short sval ;
   CODE4 *c4 ;
   PSTYLE4 style, style_old ;
   char buf[8192] ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   c4 = report->codeBase ;

   autoOpen = c4->autoOpen ;
   errCreate = c4->errCreate ;
   errExpr = c4->errExpr ;
   errOpen = c4->errOpen ;

   c4->autoOpen = c4->errCreate = c4->errExpr = c4->errOpen = 0 ;

   u4ncpy( name_buf, file_name, sizeof(name_buf) ) ;
   u4nameExt( name_buf, sizeof(name_buf), "CRS", 0 ) ;
   #ifndef S4CASE_SEN
      c4upper( name_buf ) ;
   #endif

   if( file4open( &file, c4, name_buf, 1 ) != 0 )
   {
      error4set( c4, 0 ) ;
      return -1 ;
   }

   file4longAssign(pos,0L,0) ;
   file4seqReadInitDo( &seq, &file, pos, buf, sizeof(buf), 1) ;


   file4seqRead( &seq, &m1, 1 ) ;
   file4seqRead( &seq, &m2, 1 ) ;

   m3 = m1 & STYLE_MASK ;
   m4 = m2 & VERSION_MASK ;

   if( m3 != STYLE_MASK )
      return -1 ;

   #ifdef S4WIN32
      if( m4 != VERSION_MASK && m4 != (VERSION_MASK & VERSION_MASK_OLD) )
   #else
      if( m4 != VERSION_MASK )
   #endif
         return -1 ;

   file4seqRead( &seq, &sval, sizeof(short) ) ;
   for( i = 0; i < sval; i++ )
   {
      style = report4retrieve_style( &seq, NULL, m4 ) ;
      if( !style )
      {
         file4close( &file ) ;
         return -1 ;
      }

      if( (style_old = style4lookup( report, style->name )) != NULL )
      {
         if( !override )
         {
            style4free( NULL, style ) ;
         }
         else
         {
            style4changeLoadedStyles( report, style_old, style ) ;
            if( report->active_style == style_old )
               report->active_style = style ;
            style4free( report, style_old ) ;
            l4add( &report->styles, style ) ;
         }
      }
      else
      {
         if( !override )
         {
            style4free( NULL, style ) ;
         }
         else
         {
            style4changeLoadedStyles( report, style_old, style ) ;
            style4free( report, style_old ) ;
            l4add( &report->styles, style ) ;
         }
      }


   }

   c4->autoOpen = autoOpen ;
   c4->errCreate = errCreate ;
   c4->errExpr = errExpr ;
   c4->errOpen = errOpen ;
   file4close( &file ) ;
   return 0 ;

}

PSTYLE4 S4FUNCTION report4styleFirst( PREPORT4 report )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   return (PSTYLE4)l4first( &report->styles ) ;
}

PSTYLE4 S4FUNCTION report4styleNext( PREPORT4 report, PSTYLE4 style )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   return (PSTYLE4)l4next( &report->styles, style ) ;
}

PSTYLE4 S4FUNCTION report4styleLast( PREPORT4 report )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   return (PSTYLE4)l4last( &report->styles ) ;
}

PSTYLE4 S4FUNCTION report4stylePrev( PREPORT4 report, PSTYLE4 style )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   return (PSTYLE4)l4prev( &report->styles, style ) ;
}

int S4FUNCTION report4numStyles( PREPORT4 report )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   return (int)report->styles.nLink ;
}


PSTYLE4 S4FUNCTION report4styleSelected( PREPORT4 report )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   return report->active_style ;
}

int S4FUNCTION report4styleSelect( PREPORT4 report, PSTYLE4 style )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   if( !style )
   {
      error4describe( report->codeBase, e4styleSelect, 0L, E4_REP_SSELECT, 0, 0 ) ;
      return -1 ;
   }

   report->active_style = style ;
   return 0 ;
}


PSTYLE4 S4FUNCTION style4index( PREPORT4 report, int index )
{
   int i ;
   PSTYLE4 style = NULL ;

   if( !report || index < 0 )
   {
      if( report )
         error4describe( report->codeBase, e4styleIndex, 0L, E4_REP_SINDEX, 0, 0 ) ;
      return NULL ;
   }

   for( i = 0; i <= index; i++ )
   {
      style = (PSTYLE4)l4next( &report->styles, style ) ;
      if( style == NULL )
         break ;
   }

   return style ;
}
#endif   /* S4OFF_REPORT */
