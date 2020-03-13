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

/*r4save.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved  */

#include "d4all.h"

#ifndef S4OFF_REPORT

#ifdef S4CR2  /* required for building crep2.exe*/
   #define S4CONV_REP
#endif

int file_version ;
LIST4 name_list ;
char *lookahead_buf ;
int lookedahead_calc ;

#define CREP_MASKOLD 0xA5

void sort4calcList( REPORT4 *report )
{
   EXPR4CALC *calc_on, *calc_next ;
   EXPR4     *expr ;
   LIST4     temp_list ;
   CODE4     *codeBase  = report->codeBase ;
   int       terrExpr ;

   memcpy( &temp_list, &codeBase->calcList, sizeof(LIST4) ) ;
   memset( &codeBase->calcList, 0, sizeof(LIST4) ) ;

   terrExpr = codeBase->errExpr ;
   codeBase->errExpr = 0 ;

   while ( temp_list.nLink > 0 )
   {
      /* step through all calcs in the test list */
      calc_on = (EXPR4CALC *)l4first( &temp_list ) ;
      while( calc_on )
      {
         calc_next = (EXPR4CALC *)l4next( &temp_list, calc_on ) ;
         expr = NULL ;
         /* try re-parsing the current expression */
         expr = expr4parse( report->relate->data, expr4source(calc_on->expr) ) ;

         if ( expr )
         {
            expr4free( expr ) ;
            l4remove( &temp_list, calc_on ) ;
            l4add( &codeBase->calcList, calc_on ) ;
         }

         calc_on = calc_next ;
      }

   }

   codeBase->errExpr = terrExpr ;
}



int save4long( FILE4SEQ_WRITE *seq, long *lval )
{
   #ifdef S4BYTE_SWAP
      S4LONG tval ;

      tval = x4reverseLong( (void *)lval ) ;
      return file4seqWrite( seq, &tval, sizeof(tval) ) ;
   #else
      return file4seqWrite( seq, lval, sizeof(long) ) ;
   #endif

}



int save4short( FILE4SEQ_WRITE *seq, short *sval )
{
   #ifdef S4BYTE_SWAP
      short tval ;

      tval = x4reverseShort( (void *)sval ) ;
      return file4seqWrite( seq, &tval, sizeof(tval) ) ;
   #else
      return file4seqWrite( seq, sval, sizeof(short) ) ;
   #endif
}



int ret4long( FILE4SEQ_READ *seq, long *lval )
{
   #ifdef S4BYTE_SWAP
      int file_int ;

      file_int = file4seqRead( seq, lval, sizeof(S4LONG) ) ;
      if ( file_int < sizeof(S4LONG) )  return -1 ;
      *lval = x4reverseLong( (void *)lval ) ;
      return file_int ;
   #else
      return file4seqRead( seq, lval, sizeof(S4LONG) ) ;
   #endif

}



int ret4short( FILE4SEQ_READ *seq, short *sval )
{
   #ifdef S4BYTE_SWAP
      int file_int ;

      file_int = file4seqRead( seq, sval, sizeof(short) ) ;
      if ( file_int < sizeof(short) )  return -1 ;
      *sval = x4reverseShort( (void *)sval ) ;
      return file_int ;
   #else
      return file4seqRead( seq, sval, sizeof(short) ) ;
   #endif
}



int save4string( FILE4SEQ_WRITE *seq, const char *string )
{
   long str_len ;

   str_len = (long)strlen( string ) ;

   if ( save4long( seq, &str_len ) < 0 )
      return -1 ;
   if ( file4seqWrite( seq, string, (unsigned)(str_len + 1) ) < 0 )
      return -1 ;
   return 0 ;
}



int S4FUNCTION retrieve4string( FILE4SEQ_READ *seq, char *string, int string_len )
{
   long str_len ;

   if ( ret4long( seq, &str_len ) < sizeof(str_len) )
      return -1 ;


   if ( str_len >= string_len )
   {
      error4describeExecute( seq->file->codeBase, e4result, 0L, E4_RESULT_INT, 0, 0 ) ;
      return -1 ;
   }
   file4seqRead( seq, string, (unsigned)(str_len + 1) ) ;
   return (int)str_len ;
}



char *retrieve4string_alloc( FILE4SEQ_READ *seq )
{
   long str_len ;
   char *ret_string ;

   if ( ret4long( seq, &str_len ) < sizeof(str_len) )
      return NULL ;

   ret_string = (char *)u4allocFree( seq->file->codeBase, str_len+1 ) ;
   if ( !ret_string )
      return NULL ;

   if ( file4seqRead( seq, ret_string, (unsigned)(str_len+1) ) < (unsigned)(str_len+1) )
      {
      u4free( ret_string ) ;
      return NULL ;
      }

   return ret_string ;
}



DATA4 S4PTR* S4FUNCTION relate4lookup_data( RELATE4 *relate, char *dname )
{
   RELATE4 *relate_on ;

   relate_on = relate ;

   while( relate_on != NULL )
   {
      if ( c4stricmp( dname, d4alias(relate_on->data) ) == 0 )
         return relate_on->data ;
      relate4next( &relate_on ) ;
   }
   return NULL ;
}



int S4FUNCTION report4save_object( FILE4SEQ_WRITE *seq, OBJ4 *obj )
{
   long lval ;
   /* LY 2001/12/12 : increased size of out_buf to max alias len
      + max field len + 1 */
   char out_buf[LEN4DATA_ALIAS+11] ;
   TCHAR bname[13] ;
   OBJ4 *obj_on ;
   S4LONG x, y, w, h ;
   short sval ;

   x = (obj->x <= 0 )?1:obj->x ;
   y = (obj->y <= 0 )?1:obj->y ;
   w = (obj->w <= 0 )?1:obj->w ;
   h = (obj->h <= 0 )?1:obj->h ;

   #ifdef S4BYTE_SWAP
      x = x4reverseLong((void *)&x) ;
      y = x4reverseLong((void *)&y) ;
      w = x4reverseLong((void *)&w) ;
      h = x4reverseLong((void *)&h) ;
   #endif

   if ( save4long( seq, &(x) ) < 0 ||
       save4long( seq, &(y) ) < 0 ||
       save4long( seq, &(w) ) < 0 ||
       save4long( seq, &(h) ) < 0 )
      return -1 ;

   lval = (long)obj->num_chars ;
   if ( save4long( seq, &lval ) < 0 ||
       save4short( seq, &(obj->is_active) ) < 0 ||
       save4short( seq, &(obj->is_first) ) < 0 ||
       save4short( seq, &(obj->alignment) ) < 0 ||
       save4short( seq, &(obj->numeric_type) ) < 0 ||
       save4short( seq, &(obj->display_zero) ) < 0 ||
       save4short( seq, &(obj->dec) ) < 0 ||
       save4short( seq, &(obj->use_brackets) ) < 0 ||
       save4short( seq, &(obj->leading_zero) ) < 0 )
      return -1 ;

   if ( obj->display_once_expr )
   {
      if ( save4string( seq, expr4source(obj->display_once_expr) ) < 0 )
         return -1 ;
   }
   else
   {
      if ( save4string( seq, "" ) < 0 )
         return -1 ;
   }

   if ( obj->date_format )
   {
      if ( save4string( seq, obj->date_format ) < 0 )
         return -1 ;
   }
   else
   {
      if ( save4string( seq, "" ) < 0 )
         return -1 ;
   }

   if ( obj->wintext )
   {
      if ( save4string( seq, obj->wintext ) < 0 )
         return -1 ;
   }
   else
   {
      if ( save4string( seq, "wintext" ) < 0 )
         return -1 ;
   }

   if ( save4string( seq, obj->style->name )< 0 )
      return -1 ;

   if ( file4seqWrite( seq, &(obj->lookahead), sizeof(obj->lookahead) ) < 0 )
      return -1 ;

   lval = (long)obj->obj_type_num ;
   if ( save4long( seq, &lval ) < 0 )
      return -1 ;

   if ( strlen(obj->field_name) == 0 )
   {
      sval = 0 ;
      save4short( seq, &sval ) ;
   }
   else
   {
      sval = 1 ;
      save4short( seq, &sval ) ;
      save4string( seq, obj->field_name ) ;
      sval = obj->field_type ;
      save4short( seq, &sval ) ;
      sval = obj->field_len ;
      save4short( seq, &sval ) ;
      sval = obj->field_dec ;
      save4short( seq, &sval ) ;
   }

   switch( obj->obj_type_num )
   {
      case obj4type_bitmap1:
         //u4ncpy( bname, obj->area->report->report_name, 9 ) ;
         lstrcpy( bname, obj->area->report->report_name ) ;
         wsprintf( bname + lstrlen(bname), TEXT(".b%2d"), obj->area->report->bitmaps++ ) ;
         #ifdef S4WINDOWS
         #ifndef S4WIN32
         if ( (bmp4WriteDIB( bname, (HANDLE)obj->data )) != (BOOL)NULL )
            save4string( seq, bname ) ;
         else
         #endif
         #endif
            save4string( seq, "" ) ;
         break ;

      case obj4type_bitmap3:
      case obj4type_field:
         // AS Dec 13/05 vs 5.0 fixes
         #ifdef S4WINDOWS_VS5_PLUS
            sprintf_s( out_buf, sizeof( out_buf ), "%s|%s", f4name((FIELD4 *)obj->data), d4alias(((FIELD4 *)obj->data)->data) ) ;
         #else
            sprintf( out_buf, "%s|%s", f4name((FIELD4 *)obj->data), d4alias(((FIELD4 *)obj->data)->data) ) ;
         #endif
         if ( save4string( seq, out_buf ) < 0 )
            return -1 ;
         break ;

      case obj4type_expr:
         if ( save4string( seq, expr4source( (EXPR4 *)obj->data ) ) < 0 )
            return -1 ;
         break ;

      case obj4type_total:
         if ( save4string( seq, ((TOTAL4 *)obj->data)->calcPtr->name ) < 0 )
            return -1 ;
         break ;

      case obj4type_calc:
         if ( save4string( seq, ((EXPR4CALC *)obj->data)->name ) < 0 )
            return -1 ;
         break ;

      case obj4type_text:
      case obj4type_hline:
      case obj4type_vline:
      case obj4type_frame:
      case obj4type_bitmap2:
         if ( save4string( seq, "data" ) < 0 )
            return -1 ;
         break ;

   }

   lval = obj->contained.nLink ;
   save4long( seq, &lval ) ;
   obj_on = (OBJ4 *)l4first( &obj->contained ) ;
   while( obj_on )
   {
      report4save_object( seq, obj_on ) ;
      obj_on = (OBJ4 *)l4next( &obj->contained, obj_on ) ;
   }
   return 0 ;
}



int S4FUNCTION report4save_area( FILE4SEQ_WRITE *seq, AREA4 *area )
{
   OBJ4 *obj_on ;
   short num_objs ;

   if ( area->suppression_condition )
   {
      if ( save4string( seq, expr4source( area->suppression_condition ) ) < 0 )
         return -1 ;
   }
   else
   {
      if ( save4string( seq, " " ) < 0 )
         return -1 ;
   }

   if ( save4long( seq, &area->height ) < 0 ||
       save4short( seq, &area->is_header ) < 0 ||
       save4short( seq, &area->is_active ) < 0 ||
       save4short( seq, &area->allow_pagebreaks ) < 0 )
      return -1 ;

   num_objs = (short)area->objects.nLink ;
   if ( save4short( seq, &num_objs ) < 0 )
      return -1 ;

   obj_on =(OBJ4 *)l4first( &area->objects ) ;
   while( obj_on )
   {
      if ( report4save_object( seq, obj_on ) < 0 )
      {
         error4describe( area->report->codeBase, e4repSave, 0L, E4_REP_SOBJ, 0, 0 ) ;
         return -2 ;
      }
      obj_on = (OBJ4 *)l4next( &area->objects, obj_on ) ;
   }
   return 0 ;
}



int S4FUNCTION report4save_group( FILE4SEQ_WRITE *seq, GROUP4 *group )
{
   AREA4 *area_on ;
   short sval ;
   int rc ;

   save4string( seq, group->group_label ) ;

   if ( group->resetExpression )
      save4string( seq, expr4source( group->resetExpression ) ) ;
   else
      save4string( seq, "" ) ;

   if ( save4short( seq, &group->title_summary ) < 0)
      return -1 ;
   if ( save4short( seq, &group->position ) < 0)
      return -1 ;
   if ( save4short( seq, &group->is_active ) < 0)
      return -1 ;
   if ( save4short( seq, &group->swap_header ) < 0)
      return -1 ;
   if ( save4short( seq, &group->swap_footer ) < 0)
      return -1 ;
   if ( save4short( seq, &group->repeat_header ) < 0)
      return -1 ;
   if ( save4short( seq, &group->reset_page ) < 0 )
      return -1 ;
   if ( save4short( seq, &group->reset_pagenum ) < 0)
      return -1 ;

   sval = (short)group->header_areas.nLink ;
   if ( save4short( seq, &sval ) < 0 )
      return -1 ;
   area_on = (AREA4 *)l4first( &group->header_areas ) ;
   while( area_on )
   {
      if ( (rc = report4save_area( seq, area_on )) < 0 )
      {
         if ( rc == -1 )
            error4describe( group->report->codeBase, e4repSave, 0L, E4_REP_SAREA, 0, 0 ) ;
         return -2 ;
      }
      area_on = (AREA4 *)l4next( &group->header_areas, area_on ) ;
   }

   sval = (short)group->footer_areas.nLink ;
   if ( save4short( seq, &sval ) < 0 )
      return -1 ;
   area_on = (AREA4 *)l4first( &group->footer_areas ) ;
   while( area_on )
   {
      if ( report4save_area( seq, area_on ) < 0 )
      {
         if ( rc == -1 )
            error4describe( group->report->codeBase, e4repSave, 0L, E4_REP_SAREA, 0, 0 ) ;
         return -2 ;
      }
      area_on = (AREA4 *)l4next( &group->footer_areas, area_on ) ;
   }
   return 0 ;
}



int S4FUNCTION report4save_style( FILE4SEQ_WRITE *seq, STYLE4 *style )
{
   short sval ;
   R4BYTE bval ;

   save4string( seq, style->name ) ;

   bval = R4GETRVALUE( style->color ) ;
   if ( file4seqWrite( seq, &bval, sizeof(bval) ) < 0 )
      return -1 ;

   bval = R4GETGVALUE( style->color ) ;
   if ( file4seqWrite( seq, &bval, sizeof(bval) ) < 0 )
      return -1 ;

   bval = R4GETBVALUE( style->color ) ;
   if ( file4seqWrite( seq, &bval, sizeof(bval) ) < 0 )
      return -1 ;

   sval = style->point_size ;
   if ( save4short( seq, &sval ) < 0 )
      return -1 ;

   #ifdef S4WIN32
      if ( save4long( seq, (long *)&(style->lfont.lfHeight) ) < 0 ||
      save4long( seq, (long *)&(style->lfont.lfWidth) ) < 0 ||
      save4long( seq, (long *)&(style->lfont.lfEscapement) ) < 0 ||
      save4long( seq, (long *)&(style->lfont.lfOrientation) ) < 0 ||
      save4long( seq, (long *)&(style->lfont.lfWeight) ) < 0 )
         return -1 ;
   #else
      if ( save4short( seq, (short *)&(style->lfont.lfHeight) ) < 0 ||
      save4short( seq, (short *)&(style->lfont.lfWidth) ) < 0 ||
      save4short( seq, (short *)&(style->lfont.lfEscapement) ) < 0 ||
      save4short( seq, (short *)&(style->lfont.lfOrientation) ) < 0 ||
      save4short( seq, (short *)&(style->lfont.lfWeight) ) < 0 )
         return -1 ;
   #endif

   if ( file4seqWrite( seq, &(style->lfont.lfItalic), sizeof(style->lfont.lfItalic) ) < 0 ||
   file4seqWrite( seq, &(style->lfont.lfUnderline), sizeof(style->lfont.lfUnderline) ) < 0 ||
   file4seqWrite( seq, &(style->lfont.lfStrikeOut), sizeof(style->lfont.lfStrikeOut) ) < 0 ||
   file4seqWrite( seq, &(style->lfont.lfCharSet), sizeof(style->lfont.lfCharSet) ) < 0 ||
   file4seqWrite( seq, &(style->lfont.lfOutPrecision), sizeof(style->lfont.lfOutPrecision) ) < 0 ||
   file4seqWrite( seq, &(style->lfont.lfClipPrecision), sizeof(style->lfont.lfClipPrecision) ) < 0 ||
   file4seqWrite( seq, &(style->lfont.lfQuality), sizeof(style->lfont.lfQuality) ) < 0 ||
   file4seqWrite( seq, &(style->lfont.lfPitchAndFamily), sizeof(style->lfont.lfPitchAndFamily) ) < 0 ||
   save4string( seq, (char *)(style->lfont.lfFaceName) ) < 0 )
      return -1 ;

   if ( save4short( seq, &(style->codes_before_len) ) < 0 ||
   file4seqWrite( seq, (style->codes_before), style->codes_before_len ) < 0 ||
   save4short( seq, &(style->codes_after_len) ) < 0 ||
   file4seqWrite( seq, (style->codes_after), style->codes_after_len ) < 0 )
      return -1 ;

   return 0 ;
}



int S4FUNCTION report4save_calc( FILE4SEQ_WRITE *seq, EXPR4CALC *calc )
{
   short sval ;

   if ( calc->total )
      sval = 1 ;
   else
      sval = 0 ;
   if ( save4short( seq, &sval ) < 0 )
      return -1 ;

   if ( save4string( seq, calc->name ) < 0 ||
   save4string( seq, expr4source( calc->expr ) ) < 0 )
      return -1 ;

   if ( calc->total )
   {
      if ( calc->total->resetExpression )
         sval = 1 ;
      else
         sval = 0 ;
      if ( save4short( seq, &sval ) < 0 )
         return -1 ;

      if ( calc->total->resetExpression )
         if ( save4string( seq, expr4source(calc->total->resetExpression) ) < 0 )
            return -1 ;

      if ( calc->total->addCondition )
         sval = 1 ;
      else
         sval = 0 ;
      if ( save4short( seq, &sval ) < 0 )
         return -1 ;

      if ( calc->total->addCondition )
         if ( save4string(seq, expr4source( calc->total->addCondition) ) < 0 )
            return -1 ;

      sval = calc->total->logcon ;
      if ( save4short( seq, &sval ) < 0 )
         return -1 ;

      sval = calc->total->totalType ;
      if ( save4short( seq, &sval ) < 0 )
         return -1 ;

      if ( calc->total->lookahead )
         sval = 1 ;
      else
         sval = 0 ;
      if ( save4short( seq, &sval ) < 0 )
         return -1 ;
   }
   return 0 ;
}



int S4FUNCTION report4save_report( REPORT4 *report, FILE4SEQ_WRITE *seq )
{
   short sval ;
   GROUP4 *group_on ;
   STYLE4 *style_on ;
   EXPR4CALC *calc_on ;
   int rc ;

   sort4calcList( report ) ;

   #ifdef S4UNICODE
      TCHAR temp[128] ;

      lstrcpy(temp, report->report_name) ;
      c4utoa(temp) ;
      save4string( seq, (char *)temp ) ;
   #else
      save4string( seq, report->report_name ) ;
   #endif

   if ( report->report_caption )
   {
      #ifdef S4UNICODE
         lstrcpy(temp, report->report_caption) ;
         c4utoa(temp) ;
         save4string( seq, (char *)temp ) ;
      #else
         save4string( seq, report->report_caption ) ;
      #endif
   }
   else
      rc = save4string( seq, "" ) ;

   if ( rc < 0 )
      return -1 ;

   report->margin_top = (report->margin_top > 32000 )?0:report->margin_top ;
   report->margin_left = (report->margin_left > 32000 )?0:report->margin_left ;
   report->margin_right = (report->margin_right > 32000 )?0:report->margin_right ;
   report->margin_bottom = (report->margin_bottom > 32000 )?0:report->margin_bottom ;

   if ( save4long( seq, &report->report_width ) < 0 )
      return -1 ;
   if ( save4long( seq, &report->margin_top ) < 0 )
      return -1 ;
   if ( save4long( seq, &report->margin_bottom ) < 0 )
      return -1 ;
   if ( save4long( seq, &report->margin_left ) < 0 )
      return -1 ;
   if ( save4long( seq, &report->margin_right ) < 0 )
      return -1 ;

   sval = (short)report->codeBase->calcList.nLink ;
   if ( save4short( seq, &sval ) < 0 )
      return -1 ;

   calc_on = (EXPR4CALC *)l4first( &report->codeBase->calcList ) ;
   while( calc_on )
   {
      if ( report4save_calc( seq, calc_on ) < 0 )
      {
         error4describe( report->codeBase, e4repSave, 0L, E4_REP_SCALC, calc_on->name, 0 ) ;
         return -2 ;
      }
      calc_on = (EXPR4CALC *)l4next( &report->codeBase->calcList, calc_on ) ;
   }

   sval = (short)report->styles.nLink ;
   if ( save4short( seq, &sval ) < 0 )
      return -1 ;

   style_on = (STYLE4 *)l4first( &report->styles ) ;
   while( style_on )
   {
      if ( report4save_style( seq, style_on ) < 0 )
      {
         error4describe( report->codeBase, e4repSave, 0L, E4_REP_SSTYLE, style_on->name, 0 ) ;
         return -2 ;
      }
      style_on = (STYLE4 *)l4next( &report->styles, style_on) ;
   }

   if ( (rc = report4save_group( seq, report->page_header_footer )) < 0 )
   {
      if ( rc == -1 )
         error4describe( report->codeBase, e4repSave, 0L, E4_REP_SPHF, 0, 0 ) ;
      return -2 ;
   }

   sval = (short)report->groups.nLink ;
   if ( save4short( seq, &sval ) < 0 )
      return -1 ;

   group_on = (GROUP4 *)l4first( &report->groups ) ;
   while( group_on )
   {
      if ( (rc = report4save_group( seq, group_on )) < 0 )
      {
         if ( rc == -1 )
            error4describe( report->codeBase, e4repSave, 0L, E4_REP_SGROUP, 0, 0 ) ;
         return -2 ;
      }
      group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
   }

   if ( file4seqWrite( seq, report->currency_symbol, sizeof( report->currency_symbol ) ) < 0 )
      return -1 ;
   if ( file4seqWrite( seq, &report->thousand_separator, sizeof( report->thousand_separator ) ) < 0 )
      return -1 ;
   if ( file4seqWrite( seq, &report->decimal_point, sizeof( report->decimal_point ) ) < 0 )
      return -1 ;
   if ( save4short( seq, &report->pgbrk_title ) < 0 )
      return -1 ;
   if ( save4short( seq, &report->hard_resets_flag ) < 0 )
      return -1 ;

   if ( save4long( seq, &report->sensitivity_x ) < 0 )
      return -1 ;

   if ( save4long( seq, &report->sensitivity_y ) < 0 )
      return -1 ;

   if ( !report->output_group && !report->dfile_name )
   {
      sval = 0 ;
      save4short( seq, &sval ) ;
   }
   else
   {
      sval = 1 ;
      save4short( seq, &sval ) ;
      save4string( seq, report->output_group->group_label ) ;
      save4string( seq, report->dfile_name ) ;
   }

   if ( save4short( seq, &report->pathnames ) < 0 )
      return -1 ;

   return 0 ;
}



int relate4save_relate( RELATE4 *relate, FILE4SEQ_WRITE *seq, int path )
{
   RELATE4 *relate_on ;
   INDEX4  *index_on ;
   TAG4 *tag_on ;
   char name_buf[25] ;
   short code ;
   int rc, ttype ;
   short indcount ;

   for( relate_on = relate; relate_on != 0; )
   {
      if ( !path )
      {
         #ifndef S4CLIENT
            u4namePiece( name_buf, sizeof(name_buf), relate_on->data->dataFile->file.name, FALSE, TRUE ) ;
         #else
            u4namePiece( name_buf, sizeof(name_buf), d4fileName( relate_on->data ), FALSE, TRUE ) ;
         #endif
         rc = save4string( seq, name_buf ) ;
      }
      else
         #ifndef S4CLIENT
            rc = save4string( seq, relate_on->data->dataFile->file.name ) ;
         #else
            rc = save4string( seq, d4fileName( relate_on->data ) ) ;
         #endif

      if ( rc < 0 )
         return -1 ;

      rc = save4string( seq, d4alias( relate_on->data ) ) ;
      if ( rc < 0 )
         return -1 ;

      if ( save4short( seq, (short *)&(relate_on->matchLen) ) < 0 ||
          save4short( seq, (short *)&(relate_on->relationType) ) < 0 ||
          save4short( seq, (short *)&(relate_on->sortType) ) < 0 ||
          save4short( seq, (short *)&(relate_on->errorAction) ) < 0 )
         return -1 ;

      #ifndef S4CLIENT
         ttype = report4index_type() ;
      #else
         ttype = code4indexFormat( relate_on->codeBase ) ;
      #endif

      indcount = 0 ;
      if ( r4ntx == ttype )
      {
         index_on = (INDEX4 *)l4first( &relate_on->data->indexes ) ;
         while( index_on )
         {
            indcount += (short)index_on->tags.nLink ;
            index_on = (INDEX4 *)l4next( &relate_on->data->indexes, index_on ) ;
         }
      }
      else
      {
         indcount = (short)relate_on->data->indexes.nLink ;
      }

      if ( save4short( seq, (short *)&indcount ) < 0 )
         return -1 ;

      index_on = (INDEX4 *)l4first( &relate_on->data->indexes ) ;
      while( index_on )
      {
         if ( r4ntx == ttype )
         {
            for( tag_on = 0;; )
            {
               tag_on = (TAG4 *)l4next( &index_on->tags, tag_on ) ;
               if ( tag_on == NULL )
                  break ;
               if ( !path )
               {
                  #ifndef S4CLIENT
                     #ifdef N4OTHER
                        u4namePiece( name_buf, sizeof(name_buf), tag_on->tagFile->file.name, FALSE, TRUE ) ;
                     #else
                        u4namePiece( name_buf, sizeof(name_buf), i4fileName(tag_on->index), FALSE, TRUE ) ;
                     #endif
                  #else
                     /* when CodeServer supports Clipper indexes, will require fcn t4fileName */
                     /* u4namePiece( name_buf, sizeof(name_buf), t4fileName( tag_on ), FALSE, TRUE ); */
                  #endif
                  rc = save4string( seq, name_buf ) ;
               }
               else
                  #ifndef S4CLIENT
                     #ifdef N4OTHER
                        rc = save4string( seq, tag_on->tagFile->file.name ) ;
                     #else
                        rc = save4string( seq, i4fileName(tag_on->index ) ) ;
                     #endif
                  #else
                     /* when CodeServer supports Clipper indexes, will require fcn t4fileName */
                     /* rc = save4string( seq, t4fileName( tag_on ) ) ; */
                  #endif

               if ( rc < 0 )
                  return -1 ;
            }
         }
         else
         {
            if ( !path )
            {
               #ifndef S4CLIENT
                  #ifdef N4OTHER
                     u4namePiece( name_buf, sizeof(name_buf), index_on->file.name, FALSE, TRUE ) ;
                  #else
                     u4namePiece( name_buf, sizeof(name_buf), i4fileName(index_on ), FALSE, TRUE ) ;
                  #endif
               #else
                  u4namePiece( name_buf, sizeof(name_buf), i4fileName( index_on ), FALSE, TRUE ) ;
               #endif
               rc = save4string( seq, name_buf ) ;
            }
            else
               #ifndef S4CLIENT
                  #ifdef N4OTHER
                     rc = save4string( seq, index_on->file.name ) ;
                  #else
                     rc = save4string( seq, i4fileName(index_on) ) ;
                  #endif
               #else
                  rc = save4string( seq, i4fileName( index_on ) ) ;
               #endif

            if ( rc < 0 )
               return -1 ;
         }
         index_on = (INDEX4 *)l4next( &relate_on->data->indexes, index_on ) ;
      }

      if ( !relate_on->master )
      {
         tag_on = d4tagSelected( relate_on->data ) ;
         if ( tag_on )
            #ifdef S4CLIENT
               rc = save4string( seq, t4alias(tag_on) ) ;
            #else
               rc = save4string( seq, tag_on->tagFile->alias ) ;
            #endif
         else
            rc = save4string( seq, "\0" ) ;
         if ( rc < 0 )
            return -1 ;
      }
      else
      {
         if ( relate_on->dataTag != NULL )
            #ifdef S4CLIENT
               rc = save4string( seq, t4alias(relate_on->dataTag) ) ;
            #else
               rc = save4string( seq, relate_on->dataTag->tagFile->alias ) ;
            #endif
         else
            rc = save4string( seq, "\0" ) ;
         if ( rc < 0 )
            return -1 ;
     }

      if ( relate_on->masterExpr != 0 )
         rc = save4string( seq, expr4source( relate_on->masterExpr ) ) ;
      else
         rc = save4string( seq, "\0" ) ;

      if ( rc < 0 )
         return -1 ;

      code = relate4next( &relate_on ) ;
      if ( save4short( seq, &code ) < 0 )
         return -1 ;
   }

   if ( relate->relation->exprSource )
      rc = save4string( seq, relate->relation->exprSource ) ;
   else
      rc = save4string( seq, "\0" ) ;

   if ( rc < 0 )
      return -1 ;

   if ( relate->relation->sortSource )
      rc = save4string( seq, relate->relation->sortSource ) ;
   else
      rc = save4string( seq, "\0" ) ;

   if ( rc < 0 )
      return -1 ;

   return 0 ;
}



int S4FUNCTION report4retrieve_object( FILE4SEQ_READ *seq, AREA4 *area )
{
   long lval ;
   long x, y, w, h, num_chars, obj_type_num ;
   short is_active, is_first, lookahead, i, sval ;
   short alignment, numeric_type, display_zero, dec ;
   short use_brackets, leading_zero ;
   char *wintext, *data, style_name[20], *fname ;
   char *dname, date_format[20], *dexprSource ;
   EXPR4CALC *calc ;
   FIELD4 *field ;
   OBJ4 *obj ;
   STYLE4 *style ;
   DATA4 *d4 ;
   EXPR4 *expr, *dexpr ;
   int j ;
   char field_name[11], field_type = 0 ;
   short field_len = 0, field_dec = 0 ;
   POUT4OBJ oobj ;

   field_name[0] = '\0' ;
   obj = NULL ;

   if ( ret4long( seq, &x ) < sizeof(x) ||
       ret4long( seq, &y ) < sizeof(y) ||
       ret4long( seq, &w ) < sizeof(w) ||
       ret4long( seq, &h ) < sizeof(h) )
      return -1 ;

   if ( ret4long( seq, &num_chars ) < sizeof(num_chars) ||
        ret4short( seq, &is_active ) < sizeof(is_active) ||
        ret4short( seq, &is_first ) < sizeof(is_first) ||
        ret4short( seq, &alignment ) < sizeof(alignment) ||
        ret4short( seq, &numeric_type ) < sizeof(numeric_type) ||
        ret4short( seq, &display_zero ) < sizeof(display_zero) ||
        ret4short( seq, &dec ) < sizeof(dec) ||
        ret4short( seq, &use_brackets ) < sizeof(use_brackets) ||
        ret4short( seq, &leading_zero ) < sizeof(leading_zero)
   )
      return -1 ;

   dexprSource = retrieve4string_alloc( seq ) ;
   if ( !dexprSource )
      return -1 ;

   if ( dexprSource[0] == '\0' )
   {
      u4free( dexprSource ) ;
      dexprSource = NULL ;
   }
   else
      report4nchange( seq->file->codeBase, &dexprSource, 1, strlen(dexprSource) ) ;

   if ( retrieve4string( seq, date_format, sizeof(date_format) ) < 0 )
      return -1 ;

   wintext = retrieve4string_alloc( seq ) ;

   if ( !wintext )
      return -1 ;

   if ( wintext[0] == '\0' )
   {
      u4free( wintext ) ;
      wintext = NULL ;
   }

   if ( retrieve4string( seq, style_name, sizeof(style_name) ) < 0 )
      return -1 ;

   style = style4lookup( area->report, style_name ) ;

   if ( !style )
   {
      u4free( wintext ) ;
      return -1 ;
   }

   if ( ret4short( seq, &lookahead ) < sizeof(lookahead) ||
       ret4long( seq, &obj_type_num ) < sizeof(obj_type_num) )
   {
      u4free( wintext ) ;
      return -1 ;
   }

   if ( file_version >= 0x25 )
   {
      ret4short( seq, &sval ) ;
      if ( sval )
      {
         retrieve4string( seq, field_name, sizeof(field_name) ) ;
         ret4short( seq, &sval ) ;
         field_type = (char)sval ;
         ret4short( seq, &sval ) ;
         field_len = sval ;
         ret4short( seq, &sval ) ;
         field_dec = sval ;
      }
   }

   data = retrieve4string_alloc( seq ) ;
   if ( !data )
   {
      u4free( wintext ) ;
      return -1 ;
   }
   if ( data[0] == '\0' )
   {
      u4free( data ) ;
      data = NULL ;
   }

   switch( obj_type_num )
   {
      case obj4type_bitmap1:
         if ( !data )
            goto ObjErr1 ;
         #ifdef S4WINDOWS
         obj = obj4bitmapStaticCreate( area, bmp4GetDIB((LPSTR)data, area->report->codeBase), x, y, w, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         #else
         u4free( data ) ;
         data = NULL ;
         #endif
         break ;

      case obj4type_bitmap2:
         #ifdef S4WINDOWS
         obj = obj4bitmapFileCreate( area, wintext, x, y, w, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         if (wintext)
            u4free( wintext ) ;
         wintext = NULL ;
         #endif
         break ;

      case obj4type_bitmap3:
         #ifdef S4WINDOWS
         if ( !data )
            goto ObjErr1 ;
         report4nchange( area->report->codeBase, &data, 1, strlen(data) ) ;
         fname = dname = data ;
         for( i = 0; (unsigned)i < strlen(data); i++ )
            if ( data[i] == '|' )
               {
               data[i] = '\0' ;
               dname = data+i+1 ;
               break ;
               }
         if ( (d4 = relate4lookup_data( area->report->relate, dname )) == NULL ||
             (field = d4field( d4, fname )) == NULL )
            goto ObjErr1 ;
         obj = obj4bitmapFieldCreate( area, field, x, y, w, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         #endif
         break ;

      case obj4type_field: /* field */
         report4nchange( area->report->codeBase, &data, 1, strlen(data) ) ;
         fname = dname = data ;
         for( i = 0; (unsigned)i < strlen(data); i++ )
            if ( data[i] == '|' )
               {
               data[i] = '\0' ;
               dname = data+i+1 ;
               break ;
               }
         if ( (d4 = relate4lookup_data( area->report->relate, dname )) == NULL ||
             (field = d4field( d4, fname )) == NULL )
            goto ObjErr1 ;
         if ( (obj = obj4fieldCreate( area, field, x, y, w, h )) == NULL )
            goto ObjErr1 ;
         break ;

      case obj4type_expr: /* Expression */
         report4nchange( area->report->codeBase, &data, 1, strlen(data) ) ;
         expr = expr4parse( area->report->relate->data, data ) ;
         if ( !expr )
            goto ObjErr1 ;

         if ( (obj = obj4exprCreate( area, expr, x, y, w, h )) == NULL )
            goto ObjErr1 ;
         break ;

      case obj4type_total: /* TOTAL */
         calc = expr4calcLookup( seq->file->codeBase, 0, (char *)data, strlen((char *)data) ) ;
         if ( !calc || !calc->total )
            goto ObjErr1 ;

         obj = obj4totalCreate( area, calc->total, x, y, w, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         break ;

      case obj4type_calc: /* Calculation */
         calc = expr4calcLookup( seq->file->codeBase, 0, (char *)data, strlen((char *)data) ) ;
         if ( !calc )
            goto ObjErr1 ;
         obj = obj4calcCreate( area, calc, x, y, w, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         break ;

      case obj4type_text:
         obj = obj4textCreate( area, NULL, x, y, w, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         break ;

      case obj4type_hline:
         obj = obj4lineCreate( area, 0, x, y, w ) ;
         if ( !obj )
            goto ObjErr1 ;
         obj4lineWidth( obj, h ) ;
         break ;

      case obj4type_vline:
         obj = obj4lineCreate( area, 1, x, y, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         obj4lineWidth( obj, w ) ;
         break ;

      case obj4type_frame:
         obj = obj4frameCreate( area, x, y, w, h ) ;
         if ( !obj )
            goto ObjErr1 ;
         break ;

   }

   if ( obj )
   {
      if ( wintext )
         obj->wintext = wintext ;
      obj->style = style ;
      obj->lookahead = lookahead ;
      obj->obj_type_num = (int)obj_type_num ;
      obj->num_chars = (int)num_chars ;
      obj->alignment = alignment ;
      obj->numeric_type = numeric_type ;
      obj->display_zero = display_zero ;
      obj->dec = dec ;
      obj->use_brackets = use_brackets ;
      obj->leading_zero = leading_zero ;
      obj->field_len = field_len ;
      obj->field_dec = field_dec ;
      obj->field_type = field_type ;
      // AS Dec 13/05 vs 5.0 fixes
      c4strcpy( obj->field_name, sizeof( obj->field_name ), field_name ) ;
   }

   if ( strlen(field_name) > 0 && obj != NULL )
   {
      oobj = (POUT4OBJ)u4allocFree( area->report->codeBase, sizeof(OUT4OBJ) ) ;
      oobj->obj = obj ;
      l4add( &area->report->output_objs, oobj ) ;
   }

   if ( strlen(date_format) > 0 && obj != NULL )
   {
      int dlen = strlen(date_format) + 1 ;
      obj->date_format = (char *)u4allocFree( area->report->codeBase, dlen ) ;
      if ( obj->date_format == 0 )
         goto ObjErr1 ;
      // AS Dec 13/05 vs 5.0 fixes
      c4strcpy( obj->date_format, dlen, date_format ) ;
   }

   if ( dexprSource )
   {
      if ( obj != NULL )
      {
         dexpr = expr4parse( area->report->relate->data, dexprSource ) ;
         if ( !dexpr )
         {
            obj->display_once = 0 ;
            obj->display_once_expr = NULL ;
            obj->last_display_val = NULL ;
            error4set( area->report->codeBase, 0 ) ;
         }
         else
         {
            obj->display_once = 1 ;
            obj->display_once_expr = dexpr ;
         }
      }
      u4free( dexprSource ) ;
   }

   if ( data )
      u4free( data ) ;

   ret4long( seq, &lval ) ;
   for( j = 0; j < lval; j++ )
   {
      report4retrieve_object( seq, area ) ;
   }

   return 0 ;

ObjErr1:
   if ( data )
      u4free( data ) ;
   error4describe(area->report->codeBase,e4repRet,0L,E4_REP_ROBJ,wintext,0) ;
   if ( wintext )
      u4free( wintext ) ;

   return -1 ;
}



int S4FUNCTION report4retrieve_area_foo( FILE4SEQ_READ *seq, GROUP4 *group, char *buffer )
{
   AREA4 *area ;
   short is_header, is_active, allow_pagebreaks, num_objs, i ;
   long  height ;
   int   rc ;
   char  *cptr ;

   if (retrieve4string( seq, buffer, 1023 ) < 0 )
      return -1 ;
   c4trimN( buffer, 1023 ) ;

   if ( ret4long( seq, &height ) < sizeof(height) ||
   ret4short( seq, &is_header ) < sizeof(is_header) ||
   ret4short( seq, &is_active ) < sizeof(is_active) ||
   ret4short( seq, &allow_pagebreaks ) < sizeof(allow_pagebreaks) )
      return -1 ;

   cptr = buffer ;
   report4nchange( group->report->codeBase, &cptr, 0, 1024 ) ;

   area = area4create( group, height, is_header, (strlen(buffer)>0)?buffer:NULL ) ;
   if ( !area )
      return -2 ;

   area->is_active = is_active ;
   area->allow_pagebreaks = allow_pagebreaks ;

   if ( ret4short( seq, &num_objs ) < sizeof(num_objs) )
      return -1 ;

   for( i = 0; i < num_objs; i++ )
      if ( (rc = report4retrieve_object( seq, area )) < 0 )
      {
         if ( (rc == -1) || (rc == -2) )
         {
            error4describe( group->report->codeBase, e4repRet, 0L, E4_REP_ROBJ, 0, 0 ) ;
            return -2 ;
         }
      }

   return 0 ;
}



int S4FUNCTION report4retrieve_area( FILE4SEQ_READ *seq, GROUP4 *group )
{
   int retvalue ;
   char *buffer ;

   buffer = (char *) u4allocFree( seq->file->codeBase, 1024 ) ;
   if ( !buffer )
      return -1 ;

   retvalue = report4retrieve_area_foo( seq, group, buffer ) ;

   u4free( buffer ) ;

   return retvalue ;
}



int S4FUNCTION report4retrieve_group_foo( FILE4SEQ_READ *seq, REPORT4 *report, char *buffer )
{
   short sval, i, group_type ;
   char group_label[27], *cptr ;
   GROUP4 *group ;
   int rc ;

   retrieve4string( seq, group_label, sizeof(group_label) ) ;

   retrieve4string( seq, buffer, 1023 ) ;
   c4trimN( buffer, 1024 ) ;
   cptr = buffer ;
   report4nchange( report->codeBase, &cptr, 0, 1024 ) ;

   ret4short( seq, &group_type ) ;

   if ( group_type == 0 )
      group = group4create( report, group_label, (strlen(buffer)>0)?buffer:NULL ) ;
   else
      group = report->title_summary ;
   if ( !group )
      return -1 ;

   group->title_summary = group_type ;

   ret4short( seq, &group->position ) ;
   ret4short( seq, &group->is_active ) ;
   ret4short( seq, &group->swap_header ) ;
   ret4short( seq, &group->swap_footer ) ;
   ret4short( seq, &group->repeat_header ) ;
   ret4short( seq, &group->reset_page ) ;
   ret4short( seq, &group->reset_pagenum ) ;


   ret4short( seq, &sval ) ;
   for( i = 0; i < sval; i++ )
   {
      if ( (rc = report4retrieve_area( seq, group )) < 0 )
      {
         if ( rc == -1 )
            error4describe( report->codeBase, e4repRet, 0L, E4_REP_RHAREA, group->group_label, 0 ) ;
         group4free( group ) ;
         return -1 ;
      }
   }

   ret4short( seq, &sval ) ;
   for( i = 0; i < sval; i++ )
   {
      if ( (rc = report4retrieve_area( seq, group )) < 0 )
      {
         if ( rc == -1 )
            error4describe( report->codeBase, e4repRet, 0L, E4_REP_RFAREA, group->group_label, 0 ) ;
         group4free( group ) ;
         return -1 ;
      }
   }

   if ( group != report->title_summary )
   {
      l4remove( &report->groups, group ) ;
      l4add( &report->groups, group ) ;
   }

   i = 1 ;
   group = (PGROUP4)l4last( &report->groups ) ;
   while( group )
   {
      group->position = i++ ;
      group = (PGROUP4)l4prev( &report->groups, group ) ;
   }

   return 0 ;
}



int S4FUNCTION report4retrieve_group( FILE4SEQ_READ *seq, REPORT4 *report )
{
   int retvalue ;
   char *buffer ;

   buffer = (char *) u4allocFree( seq->file->codeBase, 1024 ) ;
   if ( !buffer )
      return -1 ;

   retvalue = report4retrieve_group_foo( seq, report, buffer ) ;

   u4free( buffer ) ;

   return retvalue ;
}



int S4FUNCTION report4retrieve_hdr_ftr_foo( FILE4SEQ_READ *seq, REPORT4 *report, char *buffer )
{
   short sval, i, group_type ;
   char group_label[27], *cptr ;
   GROUP4 *group ;
   int rc ;

   retrieve4string( seq, group_label, sizeof(group_label) ) ;
   retrieve4string( seq, buffer, 1023 ) ;
   c4trimN( buffer, 1024 ) ;
   cptr = buffer ;
   report4nchange( report->codeBase, &cptr, 0, 1024 ) ;

   ret4short( seq, &group_type ) ;
   group = report->page_header_footer ;
   if ( !group )
   {
      return -1 ;
   }

   group->header_footer = 1 ;

   ret4short( seq, &group->position ) ;
   ret4short( seq, &group->is_active ) ;
   ret4short( seq, &group->swap_header ) ;
   ret4short( seq, &group->swap_footer ) ;
   ret4short( seq, &group->repeat_header ) ;
   ret4short( seq, &group->reset_page ) ;
   ret4short( seq, &group->reset_pagenum ) ;


   ret4short( seq, &sval ) ;
   for( i = 0; i < sval; i++ )
   {
      if ( (rc = report4retrieve_area( seq, group )) < 0 )
      {
         if ( rc == -1 )
            error4describe( report->codeBase, e4repRet, 0L, E4_REP_RHAREA, group->group_label, 0 ) ;
         group4free( group ) ;
         report->page_header_footer = NULL ;
         return -2 ;
      }
   }

   ret4short( seq, &sval ) ;
   for( i = 0; i < sval; i++ )
   {
      if ( (rc = report4retrieve_area( seq, group )) < 0 )
      {
         if ( rc == -1 )
            error4describe( report->codeBase, e4repRet, 0L, E4_REP_RFAREA, group->group_label, 0 ) ;
         group4free( group ) ;
         report->page_header_footer = NULL ;
         return -2 ;
      }
   }

   return 0 ;
}



int S4FUNCTION report4retrieve_page_header_footer( FILE4SEQ_READ *seq, REPORT4 *report )
{
   int retvalue ;
   char *buffer ;

   buffer = (char *) u4allocFree( seq->file->codeBase, 1024 ) ;
   if ( !buffer )
      return -1 ;

   retvalue = report4retrieve_hdr_ftr_foo( seq, report, buffer ) ;

   u4free( buffer ) ;

   return retvalue ;
}



PSTYLE4 S4FUNCTION report4retrieve_style( FILE4SEQ_READ *seq, REPORT4 *report, int file_version )
{
   char name[20] ;
   STYLE4 *style ;
   short codes_before_len, codes_after_len ;
   R4BYTE rval, gval, bval ;
   short point_size ;
   R4LOGFONT lfont ;
   int rc ;
   #ifdef S4WIN32
      short ilfHeight, ilfWidth, ilfEscapement, ilfOrientation, ilfWeight ;
   #endif

   rc = retrieve4string( seq, name, sizeof(name) ) ;
   if ( rc < 0 )
      return NULL ;

   if ( file4seqRead( seq, &rval, sizeof(rval) ) < sizeof(rval) ||
   file4seqRead( seq, &gval, sizeof(gval) ) < sizeof(gval) ||
   file4seqRead( seq, &bval, sizeof(bval) ) < sizeof(bval) ||
   ret4short( seq, &point_size ) < sizeof(point_size) )
      return NULL ;

   #ifndef S4WIN32
      if ( ret4short( seq, (short *)&(lfont.lfHeight) ) < sizeof(lfont.lfHeight) ||
      ret4short( seq, (short *)&(lfont.lfWidth) ) < sizeof(lfont.lfWidth) ||
      ret4short( seq, (short *)&(lfont.lfEscapement) ) < sizeof(lfont.lfEscapement) ||
      ret4short( seq, (short *)&(lfont.lfOrientation) ) < sizeof(lfont.lfOrientation) ||
      ret4short( seq, (short *)&(lfont.lfWeight) ) < sizeof(lfont.lfWeight) )
         return NULL ;
   #else
      if ( file_version >= 0x28 )
      {
         if ( ret4long( seq, (long *)&(lfont.lfHeight) ) < sizeof(lfont.lfHeight) ||
         ret4long( seq, (long *)&(lfont.lfWidth) ) < sizeof(lfont.lfWidth) ||
         ret4long( seq, (long *)&(lfont.lfEscapement) ) < sizeof(lfont.lfEscapement) ||
         ret4long( seq, (long *)&(lfont.lfOrientation) ) < sizeof(lfont.lfOrientation) ||
         ret4long( seq, (long *)&(lfont.lfWeight) ) < sizeof(lfont.lfWeight) )
            return NULL ;
      }
      else
      {
         if ( ret4short( seq, (short *)&(ilfHeight) ) < sizeof(short) ||
         ret4short( seq, (short *)&(ilfWidth) ) < sizeof(short) ||
         ret4short( seq, (short *)&(ilfEscapement) ) < sizeof(short) ||
         ret4short( seq, (short *)&(ilfOrientation) ) < sizeof(short) ||
         ret4short( seq, (short *)&(ilfWeight) ) < sizeof(short) )
            return NULL ;
         else
         {
            lfont.lfHeight = ilfHeight ;
            lfont.lfWidth = ilfWidth ;
            lfont.lfEscapement = ilfEscapement ;
            lfont.lfOrientation = ilfOrientation ;
            lfont.lfWeight = ilfWeight ;
         }
      }
   #endif

   if ( file4seqRead( seq, &(lfont.lfItalic), sizeof(lfont.lfItalic) ) < sizeof(lfont.lfItalic) ||
   file4seqRead( seq, &(lfont.lfUnderline), sizeof(lfont.lfUnderline) ) < sizeof(lfont.lfUnderline)  ||
   file4seqRead( seq, &(lfont.lfStrikeOut), sizeof(lfont.lfStrikeOut) ) < sizeof(lfont.lfStrikeOut) ||
   file4seqRead( seq, &(lfont.lfCharSet), sizeof(lfont.lfCharSet) ) < sizeof(lfont.lfCharSet) ||
   file4seqRead( seq, &(lfont.lfOutPrecision), sizeof(lfont.lfOutPrecision) ) < sizeof(lfont.lfOutPrecision) ||
   file4seqRead( seq, &(lfont.lfClipPrecision), sizeof(lfont.lfClipPrecision) ) < sizeof(lfont.lfClipPrecision) ||
   file4seqRead( seq, &(lfont.lfQuality), sizeof(lfont.lfQuality) ) < sizeof(lfont.lfQuality) ||
   file4seqRead( seq, &(lfont.lfPitchAndFamily), sizeof(lfont.lfPitchAndFamily) ) < sizeof(lfont.lfPitchAndFamily) )
      return NULL ;

   rc = retrieve4string( seq, (char *)lfont.lfFaceName, LF_FACESIZE ) ;
   if ( rc < 0 )
      return NULL ;

   #ifdef S4WINDOWS
      style = style4create( report, &lfont, name, R4RGB(rval,gval,bval), point_size ) ;
      if ( !style )
         return NULL ;
   #else
      style = style4create( report, name, 0, NULL, 0, NULL ) ;
      if ( !style )
         return NULL ;
      style->color = R4RGB(rval,gval,bval) ;
      style->point_size = point_size ;
      memcpy( &style->lfont, &lfont, sizeof(R4LOGFONT) ) ;
   #endif

   ret4short( seq, &codes_before_len ) ;
   style->codes_before_len = codes_before_len ;
   if ( codes_before_len )
   {
      style->codes_before = (char *)u4alloc( codes_before_len ) ;
      if ( !style->codes_before )
      {
         u4free( style ) ;
         return NULL ;
      }
      file4seqRead( seq, (style->codes_before), codes_before_len ) ;
   }
   else
      style->codes_before = NULL ;


   ret4short( seq, &codes_after_len ) ;
   style->codes_after_len = codes_after_len ;
   if ( codes_after_len )
   {
      style->codes_after = (char *)u4alloc( codes_after_len ) ;
      if ( !style->codes_after )
      {
         u4free( style ) ;
         u4free( style->codes_before ) ;
         return NULL ;
      }
      file4seqRead( seq, (style->codes_after), codes_after_len ) ;
   }
   else
      style->codes_after = NULL ;

   return style ;
}



int S4FUNCTION report4retrieve_calc2( FILE4SEQ_READ *seq, REPORT4 *report )
{
   EXPR4 *expr ;
   EXPR4CALC *calc ;
   TOTAL4 *total ;

   char name[20], *expr_src = NULL, *reset_expr_src = NULL, *con_src = NULL ;
   short is_total, has_reset, totalType, lookahead, has_con, logcon ;

   if ( ret4short( seq, &is_total ) < sizeof(is_total) ||
    retrieve4string( seq, name, sizeof(name) ) < 0 )
      return -1 ;

   expr_src = retrieve4string_alloc( seq ) ;
   if ( !expr_src )
      return -1 ;

   report4nchange( report->codeBase, &expr_src, 1, strlen(expr_src) ) ;

   if ( is_total )
   {
      if ( ret4short( seq, &has_reset ) < sizeof(has_reset) )
      {
         u4free(expr_src) ;
         return -1 ;
      }
      if ( has_reset )
      {
         reset_expr_src = retrieve4string_alloc( seq ) ;
         if ( !reset_expr_src )
         {
            u4free( expr_src ) ;
            return -1 ;
         }
         report4nchange( report->codeBase, &reset_expr_src, 1, strlen(reset_expr_src) ) ;
      }


      if ( file_version >= 0x24 )
      {
         if ( ret4short( seq, &has_con ) < sizeof( has_con ) )
         {
            u4free( expr_src ) ;
            if ( reset_expr_src ) u4free( reset_expr_src ) ;
            return -1 ;
         }
         if ( has_con )
         {
            con_src = retrieve4string_alloc( seq ) ;
            if ( !con_src )
            {
               u4free( expr_src ) ;
               if ( reset_expr_src ) u4free( reset_expr_src ) ;
               return -1 ;
            }
            report4nchange( report->codeBase, &con_src, 1, strlen(con_src) ) ;
         }
         ret4short( seq, &logcon ) ;
      }

      if ( ret4short( seq, &totalType ) < sizeof(totalType) )
      {
         u4free( expr_src ) ;
         if ( reset_expr_src ) u4free( reset_expr_src ) ;
         return -1 ;
      }

      if ( file_version >= 0x22 )
      {
         if ( ret4short( seq, &lookahead ) < sizeof(lookahead) )
         {
            u4free( expr_src ) ;
            if ( reset_expr_src ) u4free( reset_expr_src ) ;
            return -1 ;
         }
      }

      total = total4create( report, name, expr_src, totalType, NULL ) ;
      if ( !total )
      {
         u4free( expr_src ) ;
         if ( reset_expr_src )
            u4free( reset_expr_src ) ;
         return -2 ;
      }
      if( reset_expr_src && strlen( reset_expr_src ) > 0 )
      {
         // 2010/07/27 Fixes by Jay Hasell
         //total->resetExpression = expr4parse( report->relate->data, reset_expr_src ) ;
         EXPR4 *totalResetExpression = (EXPR4 *)u4allocFree( report->relate->data->codeBase, sizeof(EXPR4) ) ;
         totalResetExpression->constants = (char *)u4alloc( strlen( reset_expr_src ) + 1 ) ;
         strcpy( totalResetExpression->constants, reset_expr_src ) ;
         total->resetExpression = totalResetExpression;
      }
      else
         total->resetExpression = NULL ;
      total->lookahead = lookahead ;
      total->logcon = logcon ;
      /* LY 2001/02/07 : con_src was not being expr4parse()'d */
      if ( con_src )
         total->addCondition = expr4parse( report->relate->data, con_src ) ;
      else
         total->addCondition = NULL ;
   }
   else
   {
      expr = expr4parse( report->relate->data, expr_src ) ;
      if ( expr )
      {
         calc = code4calcCreate( report->codeBase, expr, name ) ;
         if ( !calc )
         {
            expr4free( expr ) ;
            u4free( expr_src ) ;
            return -2 ;
         }
      }
      else
      {
         u4free( expr_src ) ;
         return -2 ;
      }
   }
   u4free( expr_src ) ;
   return 0 ;
}



/***********************************
REPORT variable needs to be passed/ created ???
*******************/
int S4FUNCTION report4retrieve_calc( FILE4SEQ_READ *seq, REPORT4 *report )
{
   short sval ;
   int i, rc ;
   EXPR4CALC *calc ;
//   EXPR4 *expr;  // CS 2001/04/20

   if ( ret4short( seq, &sval ) < sizeof(sval) )
      return 0 ;
   for ( i = 0; i < sval; i++ )
   {
      if ( (rc = report4retrieve_calc2( seq, report )) < 0 )
      {
         error4describe( report->codeBase, e4repRet, 0L, E4_REP_RCALC, 0, 0 ) ;
         if ( rc == -1 )
         {
            report4free( report, 1, 1 ) ;
            return 0 ;
         }
      }
   }

   calc = (EXPR4CALC *)l4first( &report->codeBase->calcList ) ;
   while( calc )
   {
      if ( calc->total && calc->total->resetExpression )
      {
         // 2010/07/27 Fixes by Jay Hasell
         if( calc->total->resetExpression->constants )
            calc->total->resetExpression = expr4parse( report->relate->data, calc->total->resetExpression->constants ) ;
         /* LY 2001/01/19 : replaced (char*) with expr4source() */
//         expr = expr4parse( report->relate->data, expr4source(calc->total->resetExpression) ) ;
//         u4free( calc->total->resetExpression ) ;
//         calc->total->resetExpression = expr ;
      }
      if ( calc->total && calc->total->addCondition )
      {
         /* LY 2001/01/19 : replaced (char*) with expr4source() */
//         expr = expr4parse( report->relate->data, expr4source(calc->total->addCondition) ) ;
//         u4free( calc->total->addCondition ) ;
//         calc->total->addCondition = expr ;
      }
      calc = (EXPR4CALC *)l4next( &report->codeBase->calcList, calc ) ;
   }

   return 1 ;
}



REPORT4 *S4FUNCTION report4retrieve_report_foo( FILE4SEQ_READ *seq, RELATE4 *relate, char *buffer )
{
   REPORT4 *report ;
   short sval ;
   int i, rc ;
/*   EXPR4CALC *calc; */
/*   EXPR4 *expr; */
   char ogroup_label[26] ;
   TOTAL4* total ;
   char *tmp_buf ;

   /* query expression requires a user-defined function, */
   /* then set buffer address */
   if ( lookedahead_calc )
      tmp_buf = lookahead_buf ;

   report = report4init( relate ) ;
   if ( !report )
      return NULL ;

   style4free( report, (STYLE4 *)l4first( &report->styles ) ) ;

   if ( lookedahead_calc )
   {
      memcpy( buffer, tmp_buf, 1024 ) ;
      tmp_buf += 1024 ;
   }
   else
   {
      if ( retrieve4string( seq, buffer, 1024 ) < 0 )
      {
         report4free( report, 1, 1 ) ;
         return NULL ;
      }
   }

   // AS Dec 13/05 vs 5.0 fixes
   int rnlen = (strlen(buffer)+1) * sizeof(TCHAR) ;
   report->report_name = (LPTSTR)u4allocEr( report->codeBase, rnlen ) ;
   if ( !report->report_name )
   {
      report4free( report, 1, 1 ) ;
      return NULL ;
   }
   #ifdef S4UNICODE
      c4atou(buffer, report->report_name, strlen(buffer) * sizeof(TCHAR)) ;
   #else
      c4strcpy( report->report_name, rnlen, buffer ) ;
   #endif

   // AS Dec 13/05 vs 5.0 fixes
   rnlen = strlen(seq->file->name) + 1 ;
   report->report_file_name = (char *)u4allocEr( report->codeBase, rnlen ) ;
   if ( !report->report_file_name )
   {
      report4free( report, 1, 1 ) ;
      return NULL ;
   }
   c4strcpy( report->report_file_name, rnlen, seq->file->name ) ;

   if ( lookedahead_calc )
   {
      memcpy( buffer, tmp_buf, 1024 ) ;
      tmp_buf += 1024 ;
   }
   else
   {
      if ( retrieve4string( seq, buffer, 1024 ) < 0 )
      {
         report4free( report, 1, 1 ) ;
         return NULL ;
      }
   }

   rnlen = (strlen(buffer)+1) * sizeof(TCHAR) ;
   report->report_caption = (LPTSTR)u4allocEr( report->codeBase, rnlen ) ;
   if ( !report->report_caption )
   {
      report4free( report, 1, 1 ) ;
      return NULL ;
   }
   #ifdef S4UNICODE
      c4atou(buffer, report->report_caption, rnlen ) ;
   #else
      // AS Dec 13/05 vs 5.0 fixes
      c4strcpy( report->report_caption, rnlen, buffer ) ;
   #endif


   if ( file_version >= 0x26 )
   {
/* implement buffer manipulation
   look into null terminating buffers!!! */
      if ( lookedahead_calc )
      {
         memcpy( &report->report_width, tmp_buf, sizeof(S4LONG) ) ;
         tmp_buf += sizeof(S4LONG) ;
         memcpy( &report->margin_top, tmp_buf, sizeof(S4LONG) ) ;
         tmp_buf += sizeof(S4LONG) ;
         memcpy( &report->margin_bottom, tmp_buf, sizeof(S4LONG) ) ;
         tmp_buf += sizeof(S4LONG) ;
         memcpy( &report->margin_left, tmp_buf, sizeof(S4LONG) ) ;
         tmp_buf += sizeof(S4LONG) ;
         memcpy( &report->margin_right, tmp_buf, sizeof(S4LONG) ) ;
      }
      else
      {
         ret4long( seq, &report->report_width ) ;
         ret4long( seq, &report->margin_top ) ;
         ret4long( seq, &report->margin_bottom ) ;
         ret4long( seq, &report->margin_left ) ;
         ret4long( seq, &report->margin_right ) ;
      }
      report->margin_top = (report->margin_top > 32000 )?0:report->margin_top ;
      report->margin_left = (report->margin_left > 32000 )?0:report->margin_left ;
      report->margin_right = (report->margin_right > 32000 )?0:report->margin_right ;
      report->margin_bottom = (report->margin_bottom > 32000 )?0:report->margin_bottom ;
   }

   if ( !lookahead_buf )
   {
      if ( report4retrieve_calc( seq, report ) == 0 )
         return NULL ;
   }
   else
   {
      u4free( lookahead_buf ) ;
      lookahead_buf = NULL ;

      /* removing the dummy report pointer and assigning a valid */
      /* report structure  */
      total = (TOTAL4 *)l4first( &report->codeBase->totalList ) ;
      if ( total )
         u4free( total->report ) ;
      while( total )
      {
         total->report = report ;
         total = (TOTAL4 *)l4next( &report->codeBase->totalList, total ) ;
      }
   }

   ret4short( seq, &sval ) ;
   for( i =  0; i < sval; i++ )
      if ( (report4retrieve_style( seq, report, file_version )) == NULL )
      {
         error4describe( report->codeBase, e4repRet, 0L, E4_REP_RSTYLE, 0, 0 ) ;
         report4free( report, 1, 1 ) ;
         return NULL ;
      }
   report->active_style = (STYLE4 *)l4first( &report->styles ) ;

   if ( (rc = report4retrieve_page_header_footer( seq, report )) < 0 )
   {
      if ( rc == -1 )
         error4describe( report->codeBase, e4repRet, 0L, E4_REP_RPHF, 0, 0 ) ;
      report4free( report, 1, 1 ) ;
      return NULL ;
   }

   ret4short( seq, &sval ) ;
   for( i =  0; i < sval; i++ )
      if ( (rc = report4retrieve_group( seq, report )) < 0 )
      {
         if ( rc == -1 )
            error4describe( report->codeBase, e4repRet, 0L, E4_REP_RGROUP, 0, 0 ) ;
         report4free( report, 1, 1 ) ;
         return NULL ;
      }

   file4seqRead( seq, report->currency_symbol, sizeof( report->currency_symbol ) ) ;
   file4seqRead( seq, &report->thousand_separator, sizeof( report->thousand_separator ) ) ;
   file4seqRead( seq, &report->decimal_point, sizeof( report->decimal_point ) ) ;

   if ( file_version < 0x26 )
   {
      ret4long( seq, &report->report_width ) ;
      ret4long( seq, &report->margin_top ) ;
      ret4long( seq, &report->margin_bottom ) ;
      ret4long( seq, &report->margin_left ) ;
      ret4long( seq, &report->margin_right ) ;
   }

   if ( file_version >= (int)0x21 )
   {
      ret4short( seq, &report->pgbrk_title ) ;
      ret4short( seq, &report->hard_resets_flag ) ;
   }

   if ( file_version >= (int)0x23 )
   {
      ret4long( seq, &report->sensitivity_x ) ;
      report->sensitivity_x = (report->sensitivity_x > 5000)?0:report->sensitivity_x ;
      ret4long( seq, &report->sensitivity_y ) ;
      report->sensitivity_y = (report->sensitivity_y > 5000)?0:report->sensitivity_y ;
   }

   if ( file_version >= 0x25 )
   {
      ret4short( seq, &sval ) ;
      if ( sval )
      {
         retrieve4string( seq, ogroup_label, sizeof(ogroup_label) ) ;
         report->output_group = report4groupLookup(report, ogroup_label ) ;
         report->dfile_name = retrieve4string_alloc( seq ) ;
      }
   }

   ret4short( seq, &report->pathnames ) ;
   if ( report->pathnames != 0 )
      report->pathnames = 1 ;

   return report ;
}



REPORT4 * S4FUNCTION report4retrieve_report( FILE4SEQ_READ *seq, RELATE4 *relate )
{
   char *buffer ;
   REPORT4 *retvalue ;

   buffer = (char *) u4allocFree( seq->file->codeBase, 1024 ) ;
   if ( !buffer )
      return NULL ;

   retvalue = report4retrieve_report_foo( seq, relate, buffer ) ;

   u4free( buffer ) ;
   return retvalue ;
}



int S4FUNCTION report4save_foo( REPORT4 *report, char *file_name, int path, char *buf, char *name_buf )
{
   CODE4 *c4 ;
   FILE4 file ;
   FILE4SEQ_WRITE seq ;
   int errCreate, safety, rc ;

   if ( !report || !file_name || file_name[0] == '\0' )
      return -1 ;

   c4 = report->codeBase ;
   report->bitmaps = 0 ;

   u4ncpy( name_buf, file_name, 512 ) ;
   u4nameExt( name_buf, 512, "REP", 0 ) ;
   errCreate = c4->errCreate ;
   safety = c4->safety ;

   c4->errCreate = 0 ;
   c4->safety = 0 ;

   if ( (rc = file4create( &file, c4, name_buf, 1 )) != 0 )
   {
      error4describe( report->codeBase, e4repSave, 0L, E4_REP_SFILE, 0, 0 ) ;
      error4set( c4, 0 ) ;
      c4->errCreate = errCreate ;
      c4->safety = safety ;
      return rc ;
   }

   file4seqWriteInit( &seq, &file, 0L, buf, 2048 ) ;

   name_buf[0] = CREP_MASK ;
   name_buf[1] = VERSION_MASK ;
   file4seqWrite( &seq, name_buf, 2 ) ;

   if ( relate4save_relate( report->relate, &seq, (report->pathnames == 1 || path == 1)) )
   {
      error4describe( report->codeBase, e4repSave, 0L, E4_REP_SREL, 0, 0 ) ;
      file4seqWriteFlush( &seq ) ;
      file4close( &file ) ;
      return -1 ;
   }

   /* LY 2001/04/18 : copied lines 189-195 from crep3sav.c */
   if ( report->report_file_name != NULL )
      u4free( report->report_file_name ) ;

   /* LY 2002/03/19 : name_buf being changed above; replaced name_buf with file.nameBuf */
   // AS Dec 13/05 vs 5.0 fixes
   int rnlen = strlen(file.nameBuf) + 1 ;
   report->report_file_name = (char *)u4allocEr( report->codeBase, rnlen ) ;
   if ( !report->report_file_name )
      return -1 ;
   c4strcpy( report->report_file_name, rnlen, file.nameBuf ) ;

   if ( report->report_name != NULL )
      u4free( report->report_name ) ;

   report->report_name = (LPTSTR)u4allocEr( report->codeBase, 10 * sizeof(TCHAR) ) ;
   if ( !report->report_name )
      return -1 ;

   #ifdef S4UNICODE
      TCHAR temp[128] ;
      lstrcpy(temp, report->report_name) ;
      c4utoa(temp) ;
      u4namePiece( (char *)temp, 10, report->report_file_name, 0, 0 ) ;
   #else
      u4namePiece( report->report_name, 10, report->report_file_name, 0, 0 ) ;
   #endif
   //#ifndef S4CASE_SEN
      //c4upper( report->report_name ) ;
   //#endif

   if ( report4save_report( report, &seq ) < 0 )
      return -1 ;

   file4seqWriteFlush( &seq ) ;
   file4close( &file ) ;
   return 0 ;
}



int S4FUNCTION report4save( REPORT4 *report, char *file_name, int path )
{
   char *buf, *name_buf ;
   int   retvalue ;

   #ifdef E4PARM_HIGH
      if ( report == 0 )
         return error4( 0, e4parm_null, E95702 ) ;
   #endif

   buf = (char *) u4allocFree( report->codeBase, 2048 ) ;
   if ( !buf )
      return -1 ;

   name_buf = (char *) u4allocFree( report->codeBase, 512 ) ;
   if ( !name_buf )
   {
      u4free( buf ) ;
      return -1 ;
   }

   retvalue = report4save_foo( report, file_name, path, buf, name_buf ) ;

   u4free( buf ) ;
   u4free( name_buf ) ;

   return retvalue ;
}



REPORT4 * S4FUNCTION report4retrieve2(CODE4 *c4, char *file_name, int open_files, char *pathname, char *buf, char *name_buf )
{
   FILE4 file ;
   FILE4SEQ_READ seq ;
   long pos ;
   REPORT4 *report = NULL ;
   RELATE4 *relate = NULL ;
   int autoOpen, errCreate, errExpr, errOpen ;
   int m1, m2, m3 ;

   if ( !c4 || !file_name || file_name[0] == '\0' )
      return NULL ;

   if ( c4->numReports > 0 )
   {
      error4describe( c4, e4repRet, 0L, E4_REP_RNUM, 0, 0 ) ;
      return NULL ;
   }

   autoOpen = c4->autoOpen ;
   errCreate = c4->errCreate ;
   errExpr = c4->errExpr ;
   errOpen = c4->errOpen ;

   c4->autoOpen = c4->errCreate = c4->errExpr = c4->errOpen = 0 ;

   u4ncpy( name_buf, file_name, 512 ) ;
   u4nameExt( name_buf, 512, "REP", 0 ) ;
   #ifndef S4CASE_SEN
      c4upper( name_buf ) ;
   #endif

   if ( file4open( &file, c4, name_buf, 1 ) != 0 )
   {
      error4set( c4, 0 ) ;
      /* LY 2002/02/01 : not restoring CODE4 member values */
      c4->errOpen = errOpen ;
      c4->errExpr = errExpr ;
      c4->errCreate = errCreate ;
      c4->autoOpen = autoOpen ;
      return NULL ;
   }

   /*
   file4longAssign(pos,0L,0) ;
   file4seqReadInitDo( &seq, &file, pos, buf, 2048, 0 ) ;
   */
   pos = 0 ;
   file4seqReadInit( &seq, &file, pos, buf, 2048 ) ;

   if ( file4seqRead( &seq, name_buf, 2 ) < 2 )
   {
      file4close( &file ) ;
      /* LY 2002/02/01 : not restoring CODE4 member values */
      c4->errOpen = errOpen ;
      c4->errExpr = errExpr ;
      c4->errCreate = errCreate ;
      c4->autoOpen = autoOpen ;
      return NULL ;
   }

   m1 = name_buf[0] ;
   m2 = name_buf[1] ;

   m3 = m1 & CREP_MASK ;

   if ( m3 != CREP_MASK )
   {
      file4close( &file ) ;
      c4->errOpen = errOpen ;
      c4->errExpr = errExpr ;
      c4->errCreate = errCreate ;
      c4->autoOpen = autoOpen ;

      error4describe( c4, e4repRet, 0L, E4_REP_RFILE, 0, 0 ) ;
      return NULL ;
   }

   if ( m2 < (char)START_VERSION_MASK || m2 > (char)VERSION_MASK )
   {
      file4close( &file ) ;
      c4->errOpen = errOpen ;
      c4->errExpr = errExpr ;
      c4->errCreate = errCreate ;
      c4->autoOpen = autoOpen ;
      error4describe( c4, e4repRet, 0L, E4_REP_RFILE2, 0, 0 ) ;
      return NULL ;
   }

   file_version = (int)m2 ;

   relate = relate4retrieve_relate( &seq, open_files, pathname, R4REPORT_FILE ) ;
   if ( !relate )
   {
      file4close( &file ) ;
      c4->errOpen = errOpen ;
      c4->errExpr = errExpr ;
      c4->errCreate = errCreate ;
      c4->autoOpen = autoOpen ;
      report4free_name_list() ;
      return NULL ;
   }
   report = report4retrieve_report( &seq, relate ) ;
   c4->errOpen = errOpen ;
   c4->errExpr = errExpr ;
   c4->errCreate = errCreate ;
   c4->autoOpen = autoOpen ;
   if ( report && report->default_date_format )
      code4dateFormatSet( c4, report->default_date_format ) ;
   file4close( &file ) ;
   report4free_name_list() ;
   return report ;
}



REPORT4 * S4FUNCTION report4retrieve(CODE4 *c4, char *file_name, int open_files, char *pathname )
{
   char *buf, *name_buf ;
   REPORT4  *retvalue ;

   buf = (char *) u4allocFree( c4, 2048 ) ;
   if ( !buf )
      return NULL ;

   name_buf = (char *) u4allocFree( c4, 512 ) ;
   if ( !name_buf )
   {
      u4free( buf ) ;
      return NULL ;
   }

   retvalue = report4retrieve2( c4, file_name, open_files, pathname, buf, name_buf  ) ;

   u4free( buf ) ;
   u4free( name_buf ) ;

   return retvalue ;
}



RELATE4 * S4FUNCTION relate4retrieve2( CODE4 *c4, char *file_name, int open_files, char *pathname, char *buf, char *name_buf )
{
   FILE4 file ;
   FILE4SEQ_READ seq ;
   /* FILE4LONG pos; */
   long pos ;
   RELATE4 *relate ;
   int autoOpen, errCreate, errExpr, errOpen ;
   int m1, m2, m3 ;

   autoOpen = c4->autoOpen ;
   errCreate = c4->errCreate ;
   errExpr = c4->errExpr ;
   errOpen = c4->errOpen ;

   u4ncpy( name_buf, file_name, 512 ) ;
   u4nameExt( name_buf, 512, "REL", 0 ) ;
   #ifndef S4CASE_SEN
      c4upper( name_buf ) ;
   #endif

   if ( file4open( &file, c4, name_buf, 1 ) != 0 )
   {
      error4set( c4, 0 ) ;
      report4free_name_list() ;
      return NULL ;
   }

   /*
   file4longAssign(pos,0L,0) ;
   file4seqReadInitDo( &seq, &file, pos, buf, 2048, 0 ) ;
   */
   pos = 0 ;
   file4seqReadInit( &seq, &file, pos, buf, 2048 ) ;

   file4seqRead( &seq, name_buf, 2 ) ;
   m1 = name_buf[0] ;
   m2 = name_buf[1] ;

   m3 = m1 & CREP_MASK ;

   if ( m3 != CREP_MASK )
   {
      error4describe( c4, e4repRet, 0L, E4_REP_RFILE, 0, 0 ) ;
      return NULL ;
   }

   if ( m2 < (char)START_VERSION_MASK || m2 > (char)VERSION_MASK )
   {
      error4describe( c4, e4repRet, 0L, E4_REP_RFILE2, 0, 0 ) ;
      return NULL ;
   }

   file_version = m2 ;
   relate = relate4retrieve_relate( &seq, open_files, pathname, R4RELATE_FILE ) ;

   c4->errOpen = errOpen ;
   c4->errExpr = errExpr ;
   c4->errCreate = errCreate ;
   c4->autoOpen = autoOpen ;
   file4close( &file ) ;
   report4free_name_list() ;
   return relate ;
}



RELATE4 * S4FUNCTION relate4retrieve(CODE4 *c4, char *file_name, int open_files, char *pathname )
{
   char *buf, *name_buf ;
   RELATE4* retvalue ;

   buf = (char *) u4allocFree( c4, 2048 ) ;
   if ( !buf )
      return (RELATE4*)NULL ;

   name_buf = (char *) u4allocFree( c4, 512 ) ;
   if ( !name_buf )
   {
      u4free( buf ) ;
      return (RELATE4*)NULL ;
   }

   retvalue = relate4retrieve2( c4, file_name, open_files, pathname, buf, name_buf  ) ;

   u4free( buf ) ;
   u4free( name_buf ) ;

   return retvalue ;
}



int S4FUNCTION   relate4save2( RELATE4 *relate, char *file_name, int save_paths, char *buf, char *name_buf )
{
   CODE4 *c4 ;
   FILE4 file ;
   FILE4SEQ_WRITE seq ;
   int errCreate, safety, rc ;

   c4 = relate->codeBase ;

   u4ncpy( name_buf, file_name, 512 ) ;
   u4nameExt( name_buf, 512, "REL", 0 ) ;
   errCreate = c4->errCreate ;
   safety = c4->safety ;

   c4->errCreate = 0 ;
   c4->safety = 0 ;

   if ( (rc = file4create( &file, c4, name_buf, 1 )) != 0 )
   {
      error4set( c4, 0 ) ;
      c4->errCreate = errCreate ;
      c4->safety = safety ;
      return rc ;
   }

   file4seqWriteInit( &seq, &file, 0L, buf, 2048 ) ;

   name_buf[0] = CREP_MASK ;
   name_buf[1] = VERSION_MASK ;

   file4seqWrite( &seq, name_buf, 2 ) ;

   if ( relate4save_relate( relate, &seq, save_paths ) )
   {
      file4seqWriteFlush( &seq ) ;
      file4close( &file ) ;
      return -1 ;
   }

   file4seqWriteFlush( &seq ) ;
   file4close( &file ) ;
   return 0 ;
}



int S4FUNCTION   relate4save(RELATE4 *relate, char *file_name, int save_paths )
{
   char *buf, *name_buf ;
   int retvalue ;

   buf = (char *) u4allocFree( relate->codeBase, 2048 ) ;
   if ( !buf )
      return -1 ;

   name_buf = (char *) u4allocFree( relate->codeBase, 512 ) ;
   if ( !name_buf )
   {
      u4free( buf ) ;
      return -1 ;
   }

   retvalue = relate4save2( relate, file_name, save_paths, buf, name_buf  ) ;

   u4free( buf ) ;
   u4free( name_buf ) ;

   return retvalue ;
}


#ifdef S4VB_DOS
   RELATE4 * relate4retrieve_v ( CODE4 *c4, char *file_name, int open_files, char *data_path_name )
   {
      char buf[257] ;

      u4vtoc(buf, sizeof(buf), data_path_name) ;

      return relate4retrieve( c4, c4str(file_name), open_files, buf  ) ;
   }



   int relate4save_v ( RELATE4 *r4, char *file_name, int save_paths )
   {
      return relate4save( r4, c4str(file_name), save_paths ) ;
   }



   PREPORT4 S4FUNCTION report4retrieve_v( CODE4 *c4, char *file_name, int open_files, char *data_path )
   {
      char buf[257] ;

      u4vtoc( buf, sizeof(buf), data_path ) ;

      return report4retrieve( c4, c4str(file_name), open_files, buf ) ;
   }



   int report4save_v( REPORT4 *report, char *file_name, int save_paths )
   {
      return report4save( report, c4str(file_name), save_paths ) ;
   }
#endif /* #ifdef S4VB_DOS */
#endif   /* S4OFF_REPORT */
