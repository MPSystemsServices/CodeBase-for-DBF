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


#include "d4all.h"
#ifndef S4OFF_REPORT

/************************************************************
 *
 * Function: obj4create()
 *
 *  PARAMETERS: AREA4* area - area to own the object
 *   long x - x coordinate in 1000's of an inch, relative to the upper left
 *            corner of the area, positive x to the right
 *   long y - y coordinate in 1000's of an inch, relative to the upper left
 *            corner of the area, positive y is down
 *   long w - width of the object in 1000's of an inch
 *   long h - height of the object in 1000's of an inch
 *
 *  DESCRIPTION: low level object creation function
 *
 *  RETURNS: pointer to an OBJ4 struct on success, NULL on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
OBJ4 * S4FUNCTION   obj4create( AREA4 *area, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   if( x > area->report->report_width || y > area->height ||
      x < 0 || y < 0 || w < 0 || h < 0 )
      return NULL ;

   if( !area )
   {
      return NULL ;
   }

   /* allocate the OBJ4 structure */
   obj = (POBJ4)u4allocEr( area->report->codeBase, sizeof( OBJ4 ) ) ;
   if( !obj )
   {
      error4describe( area->report->codeBase, e4objCreate, 0L, E4_REP_OBJMEM, 0, 0 ) ;
      return NULL ;
   }

   /* set the internal members */
   obj->area = area ;
   obj->style = area->report->active_style ;
   if( obj->style == NULL )
      obj->style = (PSTYLE4)l4first( &area->report->styles ) ;
   obj->alignment = justify4left ;
   obj->x = x ;
   obj->y = y ;
   obj->w = w ;
   obj->h = h ;
   obj->numeric_type = obj4numNumber ;
   obj->display_zero = 1 ;

   /* add the object to the areas object list */
   area4add_object( area, obj ) ;
   return obj ;
}

/************************************************************
 *
 * Function: obj4free()
 *
 *  PARAMETERS: OBJ4* obj - object to be freed
 *
 *  DESCRIPTION: releases all the memory associated with the object
 *
 *  RETURNS: none
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void S4FUNCTION   obj4free( OBJ4 *obj )
{
   AREA4 *area ;
   OBJ4 *obj_on ;
   LINK4 *link ;
   POUT4OBJ out_obj ;
   int cleanup = 0 ;

   area = obj->area ;

   /* if necessary remove the object from the selected object list */
   link = (LINK4 *)obj ;
   if( obj->is_active || obj->is_first )
   {
      l4remove( &area->report->active_objects, (link+1) ) ;
      obj->is_active = obj->is_first = 0 ;
   }

   /* if the object is in a list of objects remove it, it will either be in
      the object list of an area, or the object list of a containing object */
   if( !(!link->n && !link->p) )
   {
      /* remove the object from the database generation variables,
         if they exist */
      out_obj = (POUT4OBJ)l4first( &obj->area->report->output_objs ) ;
      while( out_obj )
      {
         if( out_obj->obj == obj )
         {
            l4remove( &obj->area->report->output_objs, out_obj ) ;
            u4free( out_obj ) ;
            out_obj = NULL ;
            cleanup = 1 ;
         }
         out_obj = (POUT4OBJ)l4next( &obj->area->report->output_objs, out_obj ) ;
      }

      if( cleanup && !(obj->area->report->output_objs.nLink) )
      {
         if( obj->area->report->dfile_name )
         {
            u4free( obj->area->report->dfile_name ) ;
            obj->area->report->dfile_name = NULL ;
         }
         if( obj->area->report->output_group )
            obj->area->report->output_group = NULL ;
      }

      if( obj->container )
      {
         l4remove( &obj->container->contained, obj ) ;
      }
      else
      {
         l4remove( &area->objects, obj ) ;
      }
   }

   /* if the object being freed contains any objects don't delete them, return
      them to the parent area */
   while( (obj_on = (OBJ4 *)l4pop( &obj->contained )) != NULL )
      area4add_object( area, obj_on ) ;

   /* free the associated memory */
   if( obj->wintext )
       u4free( obj->wintext ) ;

   if( obj->date_format )
      u4free( obj->date_format ) ;

   if( obj->eval_text )
      u4free( obj->eval_text ) ;

   if( obj->display_once_expr )
      expr4free( obj->display_once_expr ) ;

   if( obj->last_display_val )
      u4free( obj->last_display_val ) ;

   #ifdef S4WINDOWS
   if( obj->hWnd )
      DestroyWindow( obj->hWnd ) ;
   #endif

   u4free( obj ) ;
}


/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION   obj4fieldFree( OBJ4 *obj )
{
   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION   obj4fieldCreate( AREA4 *area, FIELD4 *field_ptr, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   if( !area )
      return NULL ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
   {
      return NULL ;
   }

   /* set the data member to the field */
   obj->data = (void *)field_ptr ;

   /* set the object type */
   obj->obj_type_num = obj4type_field ;

   /* set the default size and alignment */
   switch( f4typeInternal(field_ptr) )
   {
      case 'N':
         obj->alignment = justify4right ;
      case 'D':
         obj->num_chars = 10 ;
         obj->dec = f4decimals( field_ptr ) ;
         break ;

      case 'M':
         obj->num_chars = 42 ;
         break ;

      case 'L':
         obj->num_chars = 2 ;
         break ;

      case 'C':
         obj->num_chars = f4len( field_ptr )+2 ;
         break ;

      default:
         obj->num_chars = 12 ;
         break ;
   }

   return obj ;
}


/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION   obj4totalFree( OBJ4 *obj )
{
   if( obj->data )
   {
      /* a total is always created in conjunction with an object, therefore
         when we delete the object we delete the total as well */
      report4deleteCalc( obj->area->report, ((PTOTAL4)obj->data)->calcPtr ) ;
      return ;
   }
   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION   obj4totalCreate( AREA4 *area, TOTAL4 *total_ptr, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   if( !area )
      return NULL ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
   {
      return NULL ;
   }

   /* set the data ptr to the total */
   obj->data = (void *)total_ptr ;

   /* set the object type */
   obj->obj_type_num = obj4type_total ;

   /* set the internal object members */
   obj->num_chars = 12 ;
   obj->alignment = justify4right ;

   /* set the totals object pointer */
   total_ptr->obj = obj ;

   return obj ;
}

/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION   obj4calcFree( OBJ4 *obj )
{
   /* calculations are created independant of objects, therefore we don't
      delete the associated calculation when the object is deleted. Calc
      deletion is handled when the report is freed */
   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION   obj4calcCreate( AREA4 *area, EXPR4CALC *calcPtr, long x, long y, long w, long h )
{
   OBJ4 *obj ;
   int type ;

   if( !area )
      return NULL ;

   if( !calcPtr )
   {
      return NULL ;
   }

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
   {
      return NULL ;
   }

   /* set the data pointer and the object type */
   obj->data = (void *)calcPtr ;
   obj->obj_type_num = obj4type_calc ;

   /* set the default size and alignment */
   type = expr4type( calcPtr->expr ) ;
   switch( type )
   {
      case r4num:
      case r4numDoub:
         obj->num_chars = 10 ;
         obj->alignment = justify4right ;
         break ;

      case r4date:
      case r4dateDoub:
         obj->num_chars = strlen( area->report->default_date_format )+2 ;
         break ;

      case r4log:
         obj->num_chars = 2 ;
         break ;

      case r4int:  // AS Dec 12/05 support for r4int field type
         obj->num_chars = 10 ;
         obj->alignment = justify4right ;
         break ;

      default:
         obj->num_chars = expr4len( calcPtr->expr )+2 ;
         break ;
   }

   return obj ;
}

/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4exprFree( OBJ4 *obj )
{
   /* free the expression */
   EXPR4 *e = (EXPR4 *)(obj->data) ;
   expr4free( e ) ;
   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION obj4exprCreate( AREA4 *area, EXPR4 *expr_ptr, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   if( !area )
      return NULL ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
   {
      return NULL ;
   }

   /* set the data pointer and the object type */
   obj->data = (void *)expr_ptr ;
   obj->obj_type_num = obj4type_expr ;

   /* set the default length and alignment */
   obj->num_chars = expr4len( expr_ptr )+2 ;
   if( expr4type( expr_ptr ) == r4num || expr4type( expr_ptr ) == r4numDoub )
      obj->alignment = justify4right ;

   return obj ;
}

/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4textFree( OBJ4 *obj )
{
   obj4free( obj ) ;
}



/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION obj4textCreate( AREA4 *area, char *str_ptr, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   if( !area )
      return NULL ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
      return NULL ;

   int strLen = 0 ;
   if( str_ptr )
   {
      /* allocate space to hold the string */
      strLen = strlen(str_ptr) + 1 ;
      obj->wintext = (char *)u4allocFree( area->report->codeBase, strLen ) ;
      if( !obj->wintext )
      {
         obj4free( obj ) ;
         return NULL ;
      }
   }
   else
      obj->wintext = NULL ;

   /* set the object type */
   obj->obj_type_num = obj4type_text ;
   obj->num_chars = 0 ;

   if( str_ptr )
      c4strcpy( (char *)obj->wintext, strLen, str_ptr ) ;  // AS Dec 13/05 vs 5.0 fixes

   return obj ;
}



/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4lineFree( OBJ4 *obj )
{
   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION obj4lineCreate( AREA4 *area, int vertical, long x, long y, long length )
{
   OBJ4 *obj ;

   if( !area )
      return NULL ;

   if( vertical )
      obj = obj4create( area, x, y, 1, length ) ;
   else
      obj = obj4create( area, x, y, length, 1 ) ;

   if( !obj )
   {
      return NULL ;
   }

   /* set the line orientation */
   obj->alignment = vertical ;
   if( vertical )
      obj->obj_type_num = obj4type_vline ;
   else
      obj->obj_type_num = obj4type_hline ;

   obj->num_chars = 0 ;

   return obj ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4lineWidth( POBJ4 obj, long width )
{
   if( !obj || width < 1 )
      return -1 ;

   if( obj->obj_type_num == obj4type_vline )
      obj->w = width ;

   if( obj->obj_type_num == obj4type_hline )
      obj->h = width ;

   /* for frames the line width is stored in the num_chars member of OBJ4 */
   if( obj->obj_type_num == obj4type_frame )
      obj->num_chars = (int)width+2 ;

   obj->dec = (int)width ;
   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4frameFree( OBJ4 *obj )
{
   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION obj4frameCreate( AREA4 *area, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   if( !area )
      return NULL ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
   {
      return NULL ;
   }

   obj->obj_type_num = obj4type_frame ;

   obj->num_chars = 0 ;
   obj->alignment = 0 ;
   obj->display_zero = 0 ;

   return obj ;
}

#ifdef S4WINDOWS
/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4bitmapStaticFree( POBJ4 obj )
{
   if( obj->data )
   {
      #ifdef S4WINDOWS
         //GlobalFree( (HGLOBAL)obj->data ) ;
         HeapFree(GetProcessHeap(), 0, obj->data) ;
      #endif
      obj->data = NULL ;
   }

   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION obj4bitmapStaticCreate( AREA4 *area, HANDLE hDIB, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   if( !area )
      return NULL ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
      {
      return NULL ;
      }

   obj->obj_type_num = obj4type_bitmap1 ;
   obj->data = (void far *)hDIB ;

   obj->num_chars = 0 ;
   obj->alignment = 0 ;
   obj->display_zero = 0 ;

   return obj ;
}

/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4bitmapFileFree( POBJ4 obj )
{
   if( obj->data )
   {
      #ifdef S4WINDOWS
         //GlobalFree( (HGLOBAL)obj->data ) ;
         HeapFree(GetProcessHeap(), 0, obj->data ) ;
      #endif
      obj->data = NULL ;
   }

   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
POBJ4 S4FUNCTION obj4bitmapFileCreate( AREA4 *area, char *filename, long x, long y, long w, long h )
{
   OBJ4 *obj ;
   HANDLE hDIB ;

   if( !area )
      return NULL ;

   if( !filename )
   {
      return NULL ;
   }

   hDIB = bmp4GetDIB( filename, area->report->codeBase ) ;
   if (!hDIB)
      return 0 ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
   {
      return NULL ;
   }

   obj->obj_type_num = obj4type_bitmap2 ;
   #ifdef S4WINDOWS
      obj->data = (void far *)hDIB ;
   #else
      obj->data = NULL ;
   #endif
   int tlen = strlen(filename) + 1 ;
   obj->wintext = (char *)u4alloc( tlen ) ;
   if( !obj->wintext )
   {
      obj4bitmapFileFree( obj ) ;
      return NULL ;
   }
   else
      c4strcpy( obj->wintext, tlen, filename ) ;  // AS Dec 13/05 vs 5.0 fixes

   obj->num_chars = 0 ;
   obj->alignment = 0 ;
   obj->display_zero = 0 ;

   return obj ;
}

/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4bitmapFieldFree( POBJ4 obj )
{
   obj4free( obj ) ;
}

/* SEE THE CODEREPORTER MANUAL */
/* a bitmap field object is almost identical to a field object, the main
   difference how it is displayed */
POBJ4 S4FUNCTION obj4bitmapFieldCreate( AREA4 *area, FIELD4 *field, long x, long y, long w, long h )
{
   OBJ4 *obj ;

   obj = obj4create( area, x, y, w, h ) ;
   if( !obj )
   {
      return NULL ;
   }

   obj->obj_type_num = obj4type_bitmap3 ;
   obj->data = field ;

   int wlen = strlen( f4name( field ) ) + 3 + strlen( d4alias( field->data ) ) ;
   obj->wintext = (char *)u4alloc( wlen ) ;
   if( !obj->wintext )
   {
      obj4bitmapFileFree( obj ) ;
      return NULL ;
   }
   else
   {
      // AS Dec 13/05 vs 5.0 fixes
      #ifdef S4WINDOWS_VS5_PLUS
         sprintf_s( obj->wintext, wlen, "%s->%s", d4alias(field->data), f4name(field) ) ;
      #else
         sprintf( obj->wintext, "%s->%s", d4alias(field->data), f4name(field) ) ;
      #endif
   }

   obj->num_chars = 0 ;
   obj->alignment = 0 ;
   obj->display_zero = 0 ;

   return obj ;
}
#endif

/* this is a function for internal use only:  it simply uses a switch
   statement to pass the object to the appropriate free function */
void S4FUNCTION obj4delete( OBJ4 *obj )
{
   #ifdef S4WINDOWS
      if( obj->hWnd )
         DestroyWindow( obj->hWnd ) ;
      obj->hWnd = (HWND)NULL ;
   #endif

   switch( obj->obj_type_num )
      {
      case obj4type_field:
         obj4fieldFree( obj ) ;
         break ;

      case obj4type_expr:
         obj4exprFree( obj ) ;
         break ;

      case obj4type_total:
         obj4totalFree( obj ) ;
         break ;

      case obj4type_calc:
         obj4calcFree( obj ) ;
         break ;

      case obj4type_text:
         obj4textFree( obj ) ;
         break ;

      case obj4type_hline:
      case obj4type_vline:
         obj4lineFree( obj ) ;
         break ;

      #ifdef S4WINDOWS
      case obj4type_frame:
         obj4frameFree( obj ) ;
         break ;

      case obj4type_bitmap1:
         obj4bitmapStaticFree( obj ) ;
         break ;

      case obj4type_bitmap2:
         obj4bitmapFileFree( obj ) ;
         break ;

      case obj4type_bitmap3:
         obj4bitmapFieldFree( obj ) ;
         break ;
      #endif

      default:
         obj4free( obj ) ;

      }
}

/****************************************************************************
*  evaluates objects with dynamic values, stores the result in the eval_text
*  member of the object structure
****************************************************************************/
void obj4evaluate( OBJ4 *obj )
{
   int len ;
   double d, *d_ptr ;
   char buf[1024], *output_ptr, *result_ptr, *date_format ;
   char date_buf[8], *large_buf=NULL ;
   REPORT4 *report ;
   EXPR4 *expr ;
   #ifdef S4DATA_ALIGN
      int temp_int ;
   #endif

   if( !obj )
      return ;

   report = obj->area->report ;

   output_ptr = buf ;

   switch( obj->obj_type_num )
   {
      /******************/
      /* the value is already calculated in the TOTAL4 structures members we
         simply move this value into the appropriate member of the OBJ4 */
      case obj4type_total:
         switch( ((TOTAL4 *)obj->data)->totalType )
         {
            case total4lowest:
               d =  ((TOTAL4 *)obj->data)->low ;
               break ;

            case total4highest:
               d = ((TOTAL4 *)obj->data)->high ;
               break ;

            case total4sum:
               d = ((TOTAL4 *)obj->data)->total ;
               break ;

            case total4average:
               d = ((TOTAL4 *)obj->data)->total/((TOTAL4 *)obj->data)->count ;
               break ;
         }

         obj->dval = d ;

         /* if the value comes out to zero and zero display is off set the text
            to NULL and return */
         if( (d == 0 || (d < 1 && obj->dec == 0)) && !obj->display_zero )
         {
            if( obj->eval_text )
               u4free( obj->eval_text ) ;
            obj->eval_text = NULL ;
            obj->eval_len = 1 ;
            return ;
         }

         /* convert the double value into text */
         len = report4conv_double( obj, d, buf ) ;
         output_ptr = buf ;

         /* if necessary re-alloc the eval_text member of the OBJ4 */
         if( obj->eval_text && obj->eval_len < len+1 )
         {
            u4free( obj->eval_text ) ;
            obj->eval_text = NULL ;
            obj->eval_len = 0 ;
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;
         }

         if( obj->eval_text == NULL )
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;

         /* copy the text into the eval_text member and set the length */
         if( obj->eval_text )
         {
            obj->eval_len = len+1 ;
            memcpy( obj->eval_text, output_ptr, len+1 ) ;
         }
         break ;

      /******************/
      case obj4type_expr:
         /* evaluate the expression */

         len = expr4len( (EXPR4 *)obj->data ) ;
         if( len >= sizeof( buf ) )
         {
            large_buf = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;
            memset( (void *)large_buf, 0, len+1 ) ;

            output_ptr = large_buf ;
         }

         len = expr4vary( (EXPR4 *)obj->data, &output_ptr ) ;

         switch( expr4type( (EXPR4 *)obj->data ) )
         {
            case r4str:
               /* simply determine the length of the string */
               //#ifdef S4WINDOWS
               //if( lstrlen( output_ptr ) < len )
                  //len = lstrlen( output_ptr ) ;
               //#else
               if( strlen( output_ptr ) < (unsigned)len )
                  len = strlen( output_ptr ) ;
               //#endif
               break ;

            case r4num:
               /* convert the ascii string to a double */
               d = c4atod( output_ptr, expr4len((EXPR4 *)obj->data) ) ;

               /* if zero and zero display is off break */
               if( (d == 0 || ( d < 1 && obj->dec == 0 )) && !obj->display_zero )
               {
                  if( obj->eval_text )
                     u4free( obj->eval_text ) ;
                  obj->eval_text = NULL ;
                  obj->eval_len = 1 ;
                  return ;
               }

               /* convert the double to formatted text */
               obj->dval = d ;
               len = report4conv_double( obj, d, buf ) ;
               /* set the output pointer to the buffer with the formatted text */
               output_ptr = buf ;
               break ;

            case r4numDoub:
               d_ptr = (double *)output_ptr ;

               /* if zero and zero display is off break */
               if( (*d_ptr == 0 || (*d_ptr < 1 && obj->dec == 0)) && !obj->display_zero )
               {
                  if( obj->eval_text )
                     u4free( obj->eval_text ) ;
                  obj->eval_text = NULL ;
                  obj->eval_len = 1 ;
                  return ;
               }

               /* set the dval member of the object */
               obj->dval = *d_ptr ;

               /* convert to formatted text and set the output pointer */
               len = report4conv_double( obj, *d_ptr, buf ) ;
               output_ptr = buf ;
               break ;

            case r4dateDoub:
               d_ptr = (double *)output_ptr ;

               /* convert the double to a date and set the output ptr to the
                  buffer holding the date */
               if( *d_ptr >= 1.0E99 )
                  len = 0 ;
               else
               {
                  date4assign( date_buf, (long)*d_ptr ) ;
                  date4format( date_buf, buf, (obj->date_format)?obj->date_format:report->default_date_format ) ;
                  len = strlen( buf ) ;
                  output_ptr = buf ;
               }

               /* set the objects dval member */
               obj->dval = *d_ptr ;
               break ;

            case r4date:
               if( *output_ptr == ' ' )
                  len = 0 ;
               else
               {
                  if( report->for_dbf )
                     c4strcpy( buf, sizeof( buf ), output_ptr ) ;  // AS Dec 13/05 vs 5.0 fixes
                  else
                  {
                     /* format the date and set the output pointer */
                     date_format =  obj->date_format ? obj->date_format : report->default_date_format ;
                     date4format( output_ptr, buf, date_format ) ;
                  }
                  len =  strlen(buf) ;
                  output_ptr = buf ;
               }
               break ;

            /* ********************************************* */
            case r4log:
               len = 1 ;
               #ifdef S4DATA_ALIGN
               memcpy( &temp_int, output_ptr, sizeof(temp_int) ) ;
               if( temp_int == 1)
               #else
               if( *(int *)output_ptr == 1)
               #endif
               {
                  obj->dval = 1 ;
                  // AS Dec 13/05 vs 5.0 fixes
                  #ifdef S4WINDOWS_VS5_PLUS
                     sprintf_s( buf, "T" ) ;
                  #else
                     sprintf( buf, "T" ) ;
                  #endif
               }
               else
               {
                  obj->dval = 0 ;
                  // AS Dec 13/05 vs 5.0 fixes
                  #ifdef S4WINDOWS_VS5_PLUS
                     sprintf_s( buf, "F" ) ;
                  #else
                     sprintf( buf, "F" ) ;
                  #endif
               }
               output_ptr = buf ;
               break ;
         } /* end of switch statement for expression type */


         /* if there is already space allocated for the eval text but it is
            too small re-alloc */
         if( (obj->eval_text && obj->eval_len < len+1) )
         {
            u4free( obj->eval_text ) ;
            obj->eval_text = NULL ;
            obj->eval_len = 0 ;
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;
         }

         /* if no memory allocated for eval text */
         if( obj->eval_text == NULL )
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;

         /* place the tet into the eval_text member */
         if( obj->eval_text )
         {
            obj->eval_len = len+1 ;
            u4ncpy( obj->eval_text, output_ptr, len+1 ) ;
         }
         break ;

      /*********************************************************/
      /* the calc type of OBJECT is dealt with almost identically to
         the expression type immediately above */
      case obj4type_calc:
         expr = ((EXPR4CALC *)obj->data)->expr ;

         len = expr4len( expr ) ;
         if( len > sizeof( buf ) )
            break ;
         len = expr4vary( expr, &output_ptr ) ;
         switch( expr4type( expr ) )
         {
            case r4str:
               //#ifdef S4WINDOWS
               //if( lstrlen( output_ptr ) < len )
                  //len = lstrlen( output_ptr ) ;
               //#else
               if( strlen( output_ptr ) < (unsigned)len )
                  len = strlen( output_ptr ) ;
               //#endif
               break ;

            case r4num:
               d = c4atod( output_ptr, expr4len((EXPR4 *)obj->data) ) ;
               if( (d == 0 || (d < 1 && obj->dec == 0)) && !obj->display_zero )
               {
                  if( obj->eval_text )
                     u4free( obj->eval_text ) ;
                  obj->eval_text = NULL ;
                  obj->eval_len = 1 ;
                  return ;
               }
               obj->dval = d ;
               len = report4conv_double( obj, d, buf ) ;
               output_ptr = buf ;
               break ;

            case r4numDoub:
               d_ptr = (double *)output_ptr ;
               if( (*d_ptr == 0 || (*d_ptr < 1 && obj->dec == 0)) && !obj->display_zero )
               {
                  if( obj->eval_text )
                     u4free( obj->eval_text ) ;
                  obj->eval_text = NULL ;
                  obj->eval_len = 1 ;
                  return ;
               }
               obj->dval = *d_ptr ;
               len = report4conv_double( obj, *d_ptr, buf ) ;
               output_ptr = buf ;
               break ;

            case r4date:
               if( *output_ptr == ' ' )
                  len = 0 ;
               else
               {
                  if( report->for_dbf )
                     c4strcpy( buf, sizeof( buf  ), output_ptr ) ;  // AS Dec 13/05 vs 5.0 fixes
                  else
                  {
                     date_format =  obj->date_format ?
                        obj->date_format : report->default_date_format ;
                     date4format( output_ptr, buf, date_format ) ;
                  }
                  len =  strlen(buf) ;
                  output_ptr = buf ;
               }
               break ;

            case r4dateDoub:
               d_ptr = (double *)output_ptr ;
               if( *d_ptr >= 1.0E99 )
                  len = 0 ;
               else
               {
                  date4assign( date_buf, (long)*d_ptr ) ;
                  date4format( date_buf, buf,
                     (obj->date_format)?obj->date_format:report->default_date_format ) ;
                  len = strlen( buf ) ;
                  output_ptr = buf ;
               }
               obj->dval = *d_ptr ;
               break ;

            case r4log:
               len = 1 ;
               #ifdef S4DATA_ALIGN
               memcpy( &temp_int, output_ptr, sizeof(temp_int) ) ;
               if( temp_int == 1)
               #else
               if( *(int *)output_ptr == 1)
               #endif
               {
                  obj->dval = 1 ;
                  // AS Dec 13/05 vs 5.0 fixes
                  #ifdef S4WINDOWS_VS5_PLUS
                     sprintf_s( buf, "T" ) ;
                  #else
                     sprintf( buf, "T" ) ;
                  #endif
               }
               else
               {
                  obj->dval = 0 ;
                  // AS Dec 13/05 vs 5.0 fixes
                  #ifdef S4WINDOWS_VS5_PLUS
                     sprintf_s( buf, "F" ) ;
                  #else
                     sprintf( buf, "F" ) ;
                  #endif
               }
               output_ptr = buf ;
               break ;
         }

         if( obj->eval_text && obj->eval_len < len+1 )
         {
            u4free( obj->eval_text ) ;
            obj->eval_text = NULL ;
            obj->eval_len = 0 ;
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;
         }

         if( obj->eval_text == NULL )
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;

         if( obj->eval_text )
         {
            obj->eval_len = len+1 ;
            u4ncpy( obj->eval_text, output_ptr, len+1 ) ;
         }
         break ;


      #ifdef S4WINDOWS
      /*******************************/
      /* bitmap referenced by a file name in a data file field */
      case obj4type_bitmap3:
         /* get the file name from the field and place it in the
            eval_text member */
         result_ptr =  f4memoPtr((FIELD4 *)obj->data ) ;
         len =  f4memoLen((FIELD4 *)obj->data ) ;
         output_ptr = result_ptr ;

         if( obj->eval_text && obj->eval_len < len+1 )
         {
            u4free( obj->eval_text ) ;
            obj->eval_text = NULL ;
            obj->eval_len = 0 ;
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;
         }

         if( obj->eval_text == NULL )
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;

         if( obj->eval_text )
         {
            obj->eval_len = len+1 ;
            u4ncpy( obj->eval_text, output_ptr, len+1 ) ;
            c4trimN( obj->eval_text, len+1 ) ;
            obj->eval_len = strlen( obj->eval_text ) + 1 ;
         }
         break ;
      #endif

      /********************************************************/
      case obj4type_field:
         /* get the field contents */

      #ifdef S4FOX
         if( f4null( (FIELD4 *)obj->data ) )
         {
            output_ptr = "" ;
            len = strlen( output_ptr ) ;
         }
         else
         {
      #endif

         result_ptr =  f4memoPtr( (FIELD4 *)obj->data ) ;
         len =  f4memoLen((FIELD4 *)obj->data ) ;

      #ifdef S4FOX
         }

         if( !f4null( (FIELD4 *)obj->data ) )
         {
      #endif
            switch( f4typeInternal((FIELD4 *)obj->data) )
            {
               case r4str:
                  output_ptr = result_ptr ;
                  break ;

               case r4numDoub:
                  d = f4double((FIELD4 *)obj->data ) ;
                  if( (d == 0 || (d < 1 && obj->dec == 0)) && !obj->display_zero )
                  {
                     if( obj->eval_text )
                        u4free( obj->eval_text ) ;
                     obj->eval_text = NULL ;
                     obj->eval_len = 1 ;
                     return ;
                  }
                  obj->dval = d ;
                  len = report4conv_double( obj, d, buf ) ;
                  output_ptr = buf ;
                  break ;

               case r4num:
                  d = f4double((FIELD4 *)obj->data ) ;
                  if( (d == 0 || (d < 1 && obj->dec == 0)) && !obj->display_zero )
                  {
                     if( obj->eval_text )
                        u4free( obj->eval_text ) ;
                     obj->eval_text = NULL ;
                     obj->eval_len = 1 ;
                     return ;
                  }
                  obj->dval = d ;
                  len = report4conv_double( obj, d, buf ) ;
                  output_ptr = buf ;
                  break ;

               case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
               case r4dateTime:
                  int x ;
                  char date[8] ;
                  result_ptr = f4dateTime( (FIELD4 *)obj->data ) ;
                  if( *result_ptr == ' ')
                     output_ptr = result_ptr ;
                  else
                  {
                     for(x = 0;x < 8; x++)
                     {
                        date[x] = result_ptr[0] ;
                        result_ptr++ ;
                     }
                     date4format( date, output_ptr, ((obj->date_format)?obj->date_format:obj->area->report->default_date_format) ) ;
                     int pos = strlen( output_ptr ) ;
                     memcpy( output_ptr + pos, " ", 1 ) ; // AS Dec 13/05 vs 5.0 fixes
                     pos++ ;
                     int clen = strlen( result_ptr ) ;
                     memcpy( output_ptr+pos, result_ptr, clen ) ;
                     output_ptr[pos+clen] = 0 ;
                  }
                  len = strlen( output_ptr ) ;
                  break ;

               case r4date:
                  if( *result_ptr == ' ' )
                     len = 0 ;
                  else
                  {
                     if( report->for_dbf )
                     {
                        /* LY 2001/01/04 : result_ptr not always null-terminated after date */
                        c4strncpy( buf, sizeof( buf ), result_ptr, 8 ) ;
                     }
                     else
                     {
                        date4format( result_ptr, buf,
                        ((obj->date_format)?obj->date_format:obj->area->report->default_date_format) ) ;
                     }
                     len =  strlen(buf) ;
                     output_ptr = buf ;
                  }
                  break ;

               case r4log:
                  len =  1 ;
                  output_ptr = result_ptr ;
                  break ;

               case r4int:  // AS Dec 12/05 support for r4int field type
                  {
                     int iResult = *((int *)result_ptr) ;
                     c4ltoa45( iResult, buf, 9 ) ;
                     buf[9] = 0 ;
                     len =  strlen(buf) ;
                     output_ptr = buf ;
                  }
                  break ;

               default:
                  output_ptr =  result_ptr ;
                  break ;
            }
      #ifdef S4FOX
         }
      #endif

         if( obj->eval_text && obj->eval_len < len+1 )
         {
            u4free( obj->eval_text ) ;
            obj->eval_text = NULL ;
            obj->eval_len = 0 ;
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;
         }

         if( obj->eval_text == NULL )
            obj->eval_text = (char *)u4allocFree( obj->area->report->codeBase, len+1 ) ;

         if( obj->eval_text )
         {
            obj->eval_len = len+1 ;
            u4ncpy( obj->eval_text, output_ptr, len+1 ) ;
            c4trimN( obj->eval_text, len+1 ) ;
            obj->eval_len = strlen( obj->eval_text ) + 1 ;
         }
         break ;
   }

   if( large_buf )
      u4free( large_buf ) ;

   return ;
}

/****************************************************************************
* Sets the alignment of an object to justify4left, justify4right, or justify4center
****************************************************************************/
int S4FUNCTION obj4justify( POBJ4 obj, int alignment )
{
   int temp ;

   if( obj == NULL )
      return -1 ;

   if( !( alignment == justify4left || alignment == justify4right ||
   alignment == justify4center ) )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_OBJAL, 0, 0 ) ;
      return -1 ;
   }

   temp = obj->alignment ;
   obj->alignment = alignment ;

   return temp ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4dateFormat( POBJ4 obj, char S4PTR *date_format )
{
   char *temp ;

   if( obj == NULL )
      return -1 ;

   if( date_format != NULL )
   {
      int slen = strlen(date_format) + 1 ;
      temp = (char *)u4alloc( slen ) ;
      if( !temp )
         return -1 ;

      c4strcpy( temp, slen, date_format ) ;  // AS Dec 13/05 vs 5.0 fixes
      if( obj->date_format != NULL )
         u4free( obj->date_format ) ;
      obj->date_format = temp ;
      return 0 ;
   }
   else
   {
      if( obj->date_format != NULL )
         u4free( obj->date_format ) ;
      obj->date_format = NULL ;
      return 0 ;
   }
}


/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4decimals( POBJ4 obj, int decimals )
{
   if( !obj )
      return -1 ;

   obj->dec = decimals ;
   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4displayOnce( POBJ4 obj, char S4PTR *expr_string )
{
   EXPR4 S4PTR *expr ;

   if( !obj )
      return -1 ;

   if( !expr_string )
   {
      obj->display_once = 0 ;
      if( obj->display_once_expr )
         expr4free( obj->display_once_expr ) ;
      obj->display_once_expr = NULL ;
      return 0 ;
   }
   else
   {
      expr = NULL ;
      expr = expr4parse( obj->area->report->relate->data, expr_string ) ;
      if( !expr )
      {
         error4describe( obj->area->report->codeBase, e4report, 0L, E4_REP_DOEXPR, 0,  0 ) ;
         return -1 ;
      }
      if( obj->display_once_expr )
         expr4free( obj->display_once_expr ) ;
      obj->display_once = 1 ;
      obj->display_once_expr = expr ;
      return 0 ;
   }
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4displayZero( POBJ4 obj, int display_zero )
{
   if( !obj )
      return -1 ;

   if( display_zero < 0 )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_ZERO, 0, 0 ) ;
      return -1 ;
   }

   obj->display_zero = display_zero ;
   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4numericType( POBJ4 obj, int numeric_type )
{
   if( !obj )
      return -1 ;

   if( !(numeric_type==obj4numNumber || numeric_type==obj4numExponent
      || numeric_type==obj4numCurrency || numeric_type==obj4numPercent ) )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_NTYPE, 0, 0 ) ;
      return -1 ;
   }

   obj->numeric_type = numeric_type ;
   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4leadingZero( POBJ4 obj, int leading_zero )
{
   if( !obj )
      return -1 ;

   if( leading_zero < 0 )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_LZERO, 0, 0 ) ;
      return 0 ;
   }

   obj->leading_zero = leading_zero ;

   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4brackets( POBJ4 obj, int use_brackets )
{
   if( !obj )
      return -1 ;

   if( use_brackets < 0 )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_BRACK, 0, 0 ) ;
      return 0 ;
   }

   obj->use_brackets = use_brackets ;

   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4style( POBJ4 obj, PSTYLE4 style )
{
   int flag = 0 ;
   PSTYLE4 style_on ;

   if( !obj )
      return -1 ;

   style_on = (PSTYLE4)l4first( &obj->area->report->styles ) ;
   while( style_on )
   {
      if( style == style_on )
         flag = 1 ;

      style_on = (PSTYLE4)l4next( &obj->area->report->styles, style_on ) ;
   }

   if( !flag && style != NULL )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_OSTYLE, 0, 0 ) ;
      return -1 ;
   }

   obj->style = style ;

   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4frameCorners( POBJ4 obj, int rounded )
{
   int temp ;

   if( !obj )
      return -1 ;

   if( rounded < 0 )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_FCRN, 0, 0 ) ;
      return -1 ;
   }

   temp = (obj->alignment) ;
   obj->alignment = (rounded) ;

   return temp ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4frameFill( POBJ4 obj, int fill )
{
   int temp ;

   if( !obj )
      return -1 ;

   if( fill < 0 )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_FFILL, 0, 0 ) ;
      return -1 ;
   }

   temp = obj->display_zero ;
   obj->display_zero = fill ;

   return temp ;
}


/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION obj4lookAhead( POBJ4 obj, int lookahead )
{
   if( !obj )
      return -1 ;

   if( lookahead < 0 )
   {
      error4describe( obj->area->report->codeBase, e4parm, 0L, E4_REP_OLOOK, 0, 0 ) ;
      return -1 ;
   }

   if( obj->obj_type_num == obj4type_total )
   {
      ((PTOTAL4)obj->data)->lookahead = lookahead ;
   }
   else
      obj->lookahead = lookahead ;

   return 0 ;
}

/* SEE THE CODEREPORTER MANUAL */
void S4FUNCTION obj4remove( POBJ4 obj )
{
   if( obj->container )
   {
      l4remove( &obj->container->contained, obj ) ;
      obj->container = NULL ;
   }
   else
      l4remove( &obj->area->objects, obj ) ;
}
#endif   /* S4OFF_REPORT */
