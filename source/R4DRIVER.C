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

/* r4driver.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4OFF_REPORT
#ifdef __TURBOC__
   #pragma hdrstop
#endif
#ifdef S4WINTEL
   #ifndef S4WINDOWS
      #include <conio.h>
   #endif
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

#ifndef S4WINCE
   #include <time.h>  // CS 2001/06/25 no time.h in Win CE

   #include <fcntl.h>
   #include <errno.h>
#endif

#include <math.h>

#if defined(S4WINTEL) && !defined(S4WINCE)  // CS 2001/06/25 more headers not in Win CE
   #ifndef S4IBMOS2
      #ifndef __TURBOC__
         #include <sys\locking.h>
         #define S4LOCKING
      #endif
      #ifdef __ZTC__
         extern int  errno ;
      #endif
      #ifdef _MSC_VER
         #include <sys\types.h>
         #include <sys\locking.h>
      #endif
      #ifdef __TURBOC__
   /*      extern int cdecl errno ; */
      #endif
   #endif

   #include <sys\stat.h>
   #include <share.h>
#endif

#ifdef S4MACINTOSH
   #include <unix.h>
#endif

#ifdef S4DO_ERRNO
   extern int errno ;
#endif

#ifdef S4WINDOWS
extern HINSTANCE hInst ;
#endif

int report4calc_page_height( REPORT4 *report )
{
   int retval ;
   AREA4 *area_on ;

   if( report->for_dbf )
      return 31000 ;

   retval = (int)(report->dev_page_height - report->dev_margin_bottom) ;
   area_on = (AREA4 *)l4first( &report->page_header_footer->footer_areas ) ;
   while( area_on )
      {
      if( area_on->suppression_condition )
         {
         if( !expr4true( area_on->suppression_condition ) )
            retval -= area_on->height_dev ;
         }
      else
         retval -= area_on->height_dev ;
      area_on = (AREA4 *)l4next( &report->page_header_footer->footer_areas, area_on ) ;
      }

   return retval ;
}

#ifdef S4WINDOWS
int report4output_group_headers( GROUP4 *, HDC, GROUP4 *, int ) ;
#else
int report4output_group_headers( GROUP4 *, PAGE4 *, GROUP4 *, int ) ;
#endif

#ifdef S4WINDOWS
#ifdef __cplusplus
int report4output_group_headers( GROUP4 *group, HDC hDC, GROUP4 *group_first, int use_break )
#else
int report4output_group_headers(group, hDC, group_first, use_break )
GROUP4 *group ;
HDC hDC ;
GROUP4 *group_first ;
int use_break ;
#endif
#else
#ifdef __cplusplus
int report4output_group_headers( GROUP4 *group, PAGE4 *hDC, GROUP4 *group_first, int use_break )
#else
int report4output_group_headers(group, hDC, group_first, use_break )
GROUP4 *group ;
PAGE4 *hDC ;
GROUP4 *group_first ;
int use_break ;
#endif
#endif
{
   REPORT4 *report = group->report ;
   AREA4 *area_on = report->area_on, *garea_on ;
   int   aflag ;
   long  area_height ;

   if( group->title_summary )
      group->tsdone = 1 ;

   aflag = 0 ;
   garea_on = (PAREA4)l4first( &group->header_areas ) ;
   while( garea_on )
   {
      if( garea_on == area_on )
         aflag = 1 ;

      garea_on = (PAREA4)l4next( &group->header_areas, garea_on ) ;
   }

   if( !aflag )
      area_on = NULL ;

   if( (group->reset_page || group->reset_pagenum || group->swap_header) &&
     (group == group_first || report->hard_resets_flag) &&
      report->first )
   {
      if( !group->reset_flag )
      {
         group->reset_flag = 1 ;
         report4output_pgfooter( report, hDC ) ;
         report->group_on = group ;
         report->group_first = group_first ;
         report->area_on = NULL ;
         report->in_header = 1 ;
         return 0 ;
      }
      else
      {
         group->reset_flag = 0 ;
      }
   }

   if( !area_on )
      area_on = (AREA4 *)l4first( &group->header_areas ) ;

   while( area_on )
   {
      if( !report->broken )
         area_height = (long)area_on->height_dev + report->ypos ;
      else
         if( report->broken == 2 )
            area_height = (area_on->height_dev - report->break_height) + report->ypos ;
         else
            if( report->broken == 1 )
               area_height = report->break_height + report->ypos ;

      if( area_height > report->disp_bottom )
      {
         if( area_on->allow_pagebreaks )
         {
            report->break_height = report4output_area_break( area_on ) ;
            if( report->break_height )
            {
               report->broken = 1 ;
               report4output_area( area_on, hDC, 1 ) ;
            }
         }
         report4output_pgfooter( report, hDC ) ;
         report->group_on = group ;
         report->group_first = group_first ;
         report->area_on = area_on ;
         report->in_header = 1 ;
         return 0 ;
      }

      report4output_area( area_on, hDC, use_break ) ;

      area_on = (AREA4 *)l4next( &group->header_areas, area_on ) ;
   }

   if( group->title_summary && group->report->pgbrk_title &&
       group->header_areas.nLink > 0 )
   {
      report->group_on = (PGROUP4)l4next( &group->report->groups, group  ) ;
      report->group_first = group_first ;
      report->area_on = NULL ;
      report->in_header = 1 ;
      return 0 ;
   }

   return 1 ;
}

#ifdef S4WINDOWS
int report4output_group_footers(GROUP4 *, HDC, GROUP4 * ) ;
#else
int report4output_group_footers(GROUP4 *, PAGE4 *, GROUP4 * ) ;
#endif

#ifdef S4WINDOWS
#ifdef __cplusplus
int report4output_group_footers(GROUP4 *group, HDC hDC, GROUP4 *group_first )
#else
int report4output_group_footers(group, hDC, group_first )
GROUP4 *group ;
HDC hDC ;
GROUP4 *group_first ;
#endif
#else
#ifdef __cplusplus
int report4output_group_footers(GROUP4 *group, PAGE4 *hDC, GROUP4 *group_first )
#else
int report4output_group_footers(group, hDC, group_first )
GROUP4 *group ;
PAGE4 *hDC ;
GROUP4 *group_first ;
#endif
#endif
{
   long theight ;
   REPORT4 *report = group->report ;
   AREA4 *area_on = report->area_on, *harea ;
   long area_height ;


   if( !area_on )
      area_on = (AREA4 *)l4first( &group->footer_areas ) ;

   harea = area_on ;
   theight = 0 ;
   while( harea )
   {
      if( !harea->suppression_condition || expr4true(harea->suppression_condition) )
         theight += harea->height_dev ;
      harea = (PAREA4)l4next( &group->footer_areas, harea ) ;
   }

   if( group->swap_footer )
   if( (report->page_header_footer &&
      report->page_header_footer->footer_areas.nLink > 0) || report->ypos < report->disp_bottom - theight )
   if( group == group_first || report->hard_resets_flag ||
      ( group_first == report->title_summary && group == (PGROUP4)l4next( &report->groups, report->title_summary)) )
   {
      report4output_swapped_footer( group, hDC ) ;

      report->group_on = (GROUP4 *)l4prev( &report->groups, group ) ;
      if( report->group_on == report->title_summary )
      {
         if( !report->end_report )
         {
            report->group_on = (PGROUP4)l4next( &report->groups, report->group_on ) ;
            report->in_header = 1 ;
            report4swap_old_rec( report ) ;
            report->group_first = NULL ;
         }
         else
         {
            if( report->group_on->footer_areas.nLink <= 0 )
               report->end_report = 2 ;
            report->in_header = 0 ;
            report->group_first = group_first ;
         }
      }
      else
      {
         report->in_header = 0 ;
         report->group_first = group_first ;
      }
      report->area_on = NULL ;
      return 0 ;
   }

   while( area_on )
   {
      if( !report->broken )
         area_height = (long)area_on->height_dev + report->ypos ;
      else
         if( report->broken == 2 )
            area_height = ( (long)area_on->height_dev - report->break_height) + report->ypos ;
         else
            if( report->broken == 1 )
               area_height = report->break_height + report->ypos ;


      if( area_height > report->disp_bottom )
      {
         if( area_on->allow_pagebreaks )
         {
            report->break_height = report4output_area_break( area_on ) ;
            if( report->break_height )
            {
               report->broken = 1 ;
               report4output_area( area_on, hDC, 1 ) ;
            }
            /* report should not recalculate totals when
               this area is redisplayed on the next page */
            if( report->tdone == 1 )
               report->tdone = 2 ;
         }
         else
         {
            /* report should not recalculate totals when this
               area is redisplayed on the next page */
            report->tdone = 2 ;
         }

         report4output_pgfooter( report, hDC ) ;

         report->group_on = group ;
         report->group_first = group_first ;
         report->area_on = area_on ;
         report->in_header = 0 ;
         return 0 ;
      }
      report4output_area( area_on, hDC, 1 ) ;
      area_on = (AREA4 *)l4next( &group->footer_areas, area_on ) ;
   }
   return 1 ;
}

#ifdef S4WINDOWS
#ifdef __cplusplus
int S4FUNCTION report4generatePage(PREPORT4 report, HDC hDC )
#else
int S4FUNCTION report4generatePage(report, hDC )
PREPORT4 report ;
HDC hDC ;
#endif
#else
#ifdef __cplusplus
int S4FUNCTION report4generatePage( PREPORT4 report )
#else
int S4FUNCTION report4generatePage(report )
PREPORT4 report ;
#endif
#endif
{
   GROUP4 *group_first, *group_on ;
   TOTAL4 *total_on ;
   int rc, flag ;

   #ifndef S4WINDOWS
      PAGE4 *hDC ;
   #endif

   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   #ifdef S4DEBUG
      #ifdef S4WINDOWS
         if( !hDC && !report->for_dbf )
            return -1 ;
      #endif
      if( error4code( report->codeBase ) < 0 )
         return -1 ;
   #endif

   #ifndef S4WINDOWS
      hDC = &report->page_buf ;
      #ifdef S4NO_CHSIZE
         file4changeSize( &report->page_buf.file_buf, 0L ) ;
      #else
         chsize( (int)(report->page_buf.file_buf.hand), 0L ) ;
      #endif
      file4seqWriteInit( &hDC->seq_wr, &hDC->file_buf, 0L, hDC->fmem_buf, sizeof(hDC->fmem_buf) ) ;
   #endif

   report->output_flag = 0 ;
   while( report->output_flag == 0 )
   {

      if( report->end_report == 2 )
         return 2 ;

      report->codeBase->pageno++ ;
      report->page_count++ ;

      report->ypos = report->dev_margin_top ;

      report->disp_bottom = report4calc_page_height( report ) ;

      group_first = report->group_first ;
      group_on = report->group_on ;
      flag = report->in_header ;

      if( group_on && group_on->reset_pagenum )
         report->codeBase->pageno = 1 ;
/*
   JH 10/12/99 - This section of code is repetitive within below while( 1 )

      if( report->tdone == 0 )
      {
         report->tdone = 1 ;
         total_on = (TOTAL4 *)l4first( &report->codeBase->totalList ) ;
         while( total_on )
         {
            if( !total_on->obj || !total_on->obj->lookahead )
               total4value_update( total_on ) ;
            total_on = (TOTAL4 *)l4next( &report->codeBase->totalList, total_on ) ;
         }
      }
*/
      /* LY 01/02/06 - repeated lookahead code from 01/01/15 for book example */
      if ( report->tdone == 0 )
      {
         total_on = (TOTAL4 *)l4first( &report->codeBase->totalList ) ;
         while( total_on )
         {
            if (( !total_on->obj || !total_on->obj->lookahead )
               && ( total_on->lookahead ))
               total4value_update( total_on ) ;
            total_on = (TOTAL4 *)l4next( &report->codeBase->totalList, total_on ) ;
         }
      }

      if( group_first == NULL && group_on == report->title_summary )
      {
         if( group_on->lookahead )
            report4evaluate_lookahead( group_on ) ;

         if( (report4output_group_headers( group_on, hDC, group_first, 1 )) == 0 )
            goto GOODRET ;
         report->first = group_on->position ;

         group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
      }

      if( !group_on || (group_on && !group_on->swap_header) )
         report4output_pgheader( report, hDC ) ;
      else
      {
         if( flag )
         {
            report4output_group_headers( group_on, hDC, group_first, 1 ) ;
            group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
         }
      }

      if( report->codeBase->pageno > 1 )
         report4output_repeat_headers( report, hDC, group_first ) ;

      while( 1 )
      {
         report->disp_bottom = report4calc_page_height( report ) ;

         if( flag )
         {
            /* LY 2001/01/15 : duplicated totalling code before group header
               output for lookahead totals */
            if ( report->tdone == 0 )
            {
               total_on = (TOTAL4 *)l4first( &report->codeBase->totalList ) ;
               while( total_on )
               {
                  if (( !total_on->obj || !total_on->obj->lookahead )
                     && ( total_on->lookahead ))
                     total4value_update( total_on ) ;
                  total_on = (TOTAL4 *)l4next( &report->codeBase->totalList, total_on ) ;
               }
            }

            if( !group_on )
               group_on = (GROUP4 *)l4first( &report->groups ) ;
            if( group_on->title_summary && group_on->tsdone > 0 )
               group_on = (PGROUP4)l4next( &report->groups, group_on ) ;
            while( group_on )
            {
               if( group_on->lookahead )
                  report4evaluate_lookahead( group_on ) ;

               if( (report4output_group_headers( group_on, hDC, group_first, 1 )) == 0 )
                  goto GOODRET ;
               report->first = group_on->position ;

               group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
            }
/*
   JH 10/12/99
   The following if( report->tdone == 0 ) section was moved to here
   from right after the above if( flag ) due to a totalling problem
*/
            if( report->tdone == 0 )
            {
               report->tdone = 1 ;
               total_on = (TOTAL4 *)l4first( &report->codeBase->totalList ) ;
               while( total_on )
               {
                  if (( !total_on->obj || !total_on->obj->lookahead )
                     && ( !total_on->lookahead ))  /* LY 2001/01/15 : lookahead done above */
                     total4value_update( total_on ) ;
                  total_on = (TOTAL4 *)l4next( &report->codeBase->totalList, total_on ) ;
               }
            }

            report4make_old_rec( report ) ;

            while( (rc = relate4skip( report->relate, 1L )) == 0 )
            {
               report->tdone = 0 ;
               group_first = report4calc_first_change_group( report ) ;

               if( group_first )
                  break ;
               else
               {
                  report4make_old_rec( report ) ;

                  if( report->tdone == 0 )
                  {
                     report->tdone = 1 ;
                     total_on = (TOTAL4 *)l4first( &report->codeBase->totalList ) ;
                     while( total_on )
                     {
                        if( !total_on->obj || !total_on->obj->lookahead )
                           total4value_update( total_on ) ;
                        total_on = (TOTAL4 *)l4next( &report->codeBase->totalList, total_on ) ;
                     }
                  }
               }
            }

            if( rc == r4eof )
            {
               group_first = (GROUP4 *)l4first( &report->groups ) ;
               report->end_report = 1 ;
            }

            if( group_first == NULL )
               group_first = (GROUP4 *)l4first( &report->groups ) ;

            if( group_first->position == 0 && group_first->footer_areas.nLink == 0 )
               group_first = (GROUP4 *)l4next( &report->groups, group_first ) ;

            report4swap_old_rec( report ) ;
         }

         report->tdone = 1 ;

         flag = 1 ;

         report->area_on = NULL ;

         if( !group_on )
            group_on = (GROUP4 *)l4last( &report->groups ) ;

         while( group_on )
         {
            if( (report4output_group_footers( group_on, hDC, group_first )) == 0 )
            {
               if( group_on == group_first && report->end_report == 1 && report->broken != 2)
               {
                  /*do not end report if summmary group area still not outputted */
                  if (!(report->area_on != NULL) && (report->group_on->title_summary) )
                        report->end_report = 2 ;
                  report->tdone = 0 ;
               }

               /* if report has been broken, then the total should not be
                  recalculated at the beginning of the next page */
               if( group_on == group_first && report->broken != 2 && report->tdone != 2 )
                   if( report->tdone == 0 )
                      report->tdone = 2 ;
                   else
                      report->tdone = 0 ;
                   /* JH fix (04/14/99) */

               if( report->for_dbf && report->output_group == group_on )
               {
                  #ifdef S4CLIENT
                     d4append( report->data_file ) ;
                     d4appendStart( report->data_file, 0 ) ;
                  #else
                     // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with aes encryption
                     file4seqWrite( &report->dfile_seq, d4record(report->data_file), (unsigned)(dfile4recWidth(report->data_file->dataFile)) ) ;
                     report->rcount++ ;
                  #endif
               }
               goto GOODRET ;
            }

            if( report->for_dbf && report->output_group == group_on )
            {
               #ifdef S4CLIENT
                  d4append( report->data_file ) ;
                  d4appendStart( report->data_file, 0 ) ;
               #else
                  // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with aes encryption
                  file4seqWrite( &report->dfile_seq, d4record(report->data_file), (unsigned)(dfile4recWidth(report->data_file->dataFile)) ) ;
                  report->rcount++ ;
               #endif
            }

            if( group_on == group_first )
               break ;
            group_on = (GROUP4 *)l4prev( &report->groups, group_on ) ;
         }

         if( report->end_report == 1)
         {
            report->end_report = 2 ;
            report4output_pgfooter( report, hDC ) ;
            break ;
         }

         report->tdone = 0 ;
         report4swap_old_rec( report ) ;
      }
GOODRET:
      if( report->output_flag )
      {
         #ifndef S4WINDOWS
            file4seqWriteFlush( &report->page_buf.seq_wr ) ;
            report->page_buf.first_read = 0 ;
         #endif
      }
      else
      {
         if ( report->page_count >= report->start_page &&
              report->page_count <= report->end_page ) /* user not requesting skip page */
         {
             report->codeBase->pageno-- ;
             report->page_count-- ;
         }
      }

   } /* while (!report->output_flag) */
   return 0 ;
}

#ifdef S4WINDOWS
#ifdef __cplusplus
int report4output_area(PAREA4 area, HDC hDC, int use_break )
#else
int report4output_area(area, hDC, use_break )
PAREA4 area ;
HDC hDC ;
int use_break ;
#endif
#else
#ifdef __cplusplus
int report4output_area(PAREA4 area, PAGE4 *hDC, int use_break )
#else
int report4output_area(area, hDC, use_break )
PAREA4 area ;
PAGE4 *hDC ;
int use_break ;
#endif
#endif
{
   OBJ4 *obj_on ;
   int suppress = 0 ;

   if( area->suppression_condition )
   {
      suppress = expr4true( area->suppression_condition ) ;
   }

   if( suppress )
   {
      if ( area->height_dev > area->report->break_height )
      {
         if( area->report->broken == 1 )
            area->report->broken = 2 ;
         else
        {
            area->report->broken = 0 ;
            area->report->break_height = 0L ;
              }
      }
      return 0 ;
   }

   obj_on = (OBJ4 *)l4first( &area->objects ) ;
   while( obj_on )
   {
      if( area->report->broken == 0 || use_break == 0 )
      {
         report4output_object( obj_on, hDC ) ;
      }
      else
         if( area->report->broken == 1 )
         {
            if( obj_on->dev_y <= area->report->break_height )
            {
               report4output_object( obj_on, hDC ) ;
            }
         }
         else
            if( area->report->broken == 2 )
            {
               if( obj_on->dev_y >= area->report->break_height )
               {
                  report4output_object( obj_on, hDC ) ;
               }
            }
      obj_on = (OBJ4 *)l4next( &area->objects, obj_on ) ;
   }

   if( area->report->broken == 1 )
   {
      area->report->broken = 2 ;
      return 0 ;
   }

   if( area->report->broken == 2 )
   {
      if( area->report->for_dbf )
      {
         area->report->ypos = 0 ;
      }
      else
      {
         area->report->ypos += (area->height_dev - area->report->break_height ) ;
      }
      area->report->broken = 0; area->report->break_height = 0L ;
      return 0 ;
   }

   if( area->report->for_dbf )
      area->report->ypos = 0 ;
   else
      area->report->ypos += area->height_dev ;
   return 0 ;
}

#ifdef S4WINDOWS
#ifdef __cplusplus
int report4output_object(POBJ4 obj, HDC hDC )
#else
int report4output_object(obj, hDC )
POBJ4 obj ;
HDC hDC ;
#endif
#else
#ifdef __cplusplus
int report4output_object(POBJ4 obj, PAGE4 *hDC )
#else
int report4output_object(obj, hDC )
POBJ4 obj ;
PAGE4 *hDC ;
#endif
#endif
{
   REPORT4 *report ;
   #ifdef S4UNICODE
      TCHAR text[LEN4PATH];
   #endif
   //char *output_ptr ;
   char *cptr ;
   long len ;
   int offset_break ;
   OBJ4 *obj_on ;
   #ifdef S4WINDOWS
      LPTSTR ptext;
      PBITMAPINFO pbmi ;
      //HANDLE hDIB ;
      HPEN hpen ;
      HBRUSH hbrush ;
      //LOGBRUSH lbrush ;
      RECT rect ;
      UINT alignment ;
      POINT pt ;
      int lwidth, sdc ;
      HDC hdcBMP ;
      HBITMAP hBMP ;
      void *pvBits ;
      short numBMIColors ;
   #else
      long lval, obj_len, data_len ;
      int  i ;
   #endif

   #ifdef S4DEBUG
      if( !obj || (!hDC && !obj->area->report->for_dbf) )
         return 0 ;
   #endif

   report = obj->area->report ;
   if( report->output_code != 1 )
      if( !(report->start_page == 0 && report->end_page == 0) )
         if( report->page_count < report->start_page || report->page_count > report->end_page )
            return 0 ;

   #ifndef S4WINDOWS
      if( (obj->dev_x + obj->dev_w) > report->dev_page_width )
         return 0 ;
   #endif

   #ifdef S4WINDOWS
      SetBkMode( hDC, TRANSPARENT ) ;
   #endif

   if( error4code( report->codeBase ) < 0 )
      return -1 ;

   //output_ptr = NULL ;

   if( report->broken == 2 )
      offset_break = (int)report->break_height ;
   else
      offset_break = 0 ;

   if( obj->display_once && obj->display_once_expr && obj->last_display_val )
   {
      len = expr4vary( obj->display_once_expr, &cptr ) ;
      if( c4memcmp(obj->last_display_val, cptr, (int)len ) == 0 )
         return 0 ;

      memcpy( obj->last_display_val, cptr, (int)len ) ;
   }

   report->output_flag = 1 ;
   switch( obj->obj_type_num )
   {
      #ifdef S4WINDOWS
         case obj4type_bitmap3:
            if( report->for_dbf )
               break ;
            if( !obj->lookahead )
               obj4evaluate( obj ) ;
            if( obj->eval_text )
            {
               //hDIB = bmp4GetDIB( obj->eval_text, obj->area->report->codeBase ) ;
               pbmi = bmp4GetDIB( obj->eval_text, obj->area->report->codeBase ) ;
               //if( !hDIB )
               if( !pbmi )
                  break ;

               //pbmi = (PBITMAPINFO)GlobalLock(hDIB) ;
               /*StretchDIBits( hDC, (int)(obj->dev_x + report->dev_margin_left),
                  (int)(obj->dev_y + report->ypos - offset_break), (int)(obj->dev_w), (int)(obj->dev_h),
                  0, 0, (int)pDIB->bmiHeader.biWidth, (int)(pDIB->bmiHeader.biHeight),
                  bmp4FindDIBBits(pDIB), pDIB, DIB_RGB_COLORS, SRCCOPY ) ;*/
               StretchBlt( hDC, (int)(obj->dev_x + report->dev_margin_left),
                  (int)(obj->dev_y + report->ypos - offset_break), (int)(obj->dev_w),
                  (int)(obj->dev_h), hDC, 0, 0, (int)pbmi->bmiHeader.biWidth,
                  (int)(pbmi->bmiHeader.biHeight), SRCCOPY ) ;
               //GlobalUnlock( hDIB ) ;
               //GlobalFree( hDIB ) ;
               HeapFree( GetProcessHeap(), 0, pbmi ) ;
            }
            break ;
         case obj4type_bitmap1:
         case obj4type_bitmap2:
            if( report->for_dbf )
               break ;
            //pbmi = (PBITMAPINFO)GlobalLock((HANDLE)obj->data) ;
            pbmi = (PBITMAPINFO)obj->data ;
            if ( pbmi->bmiHeader.biBitCount == 0 ) // don't support JPG or PNG
               break ;
            /*StretchDIBits( hDC, (int)(obj->dev_x + report->dev_margin_left),
               (int)(obj->dev_y + report->ypos - offset_break),
               (int)obj->dev_w, (int)obj->dev_h,
               0, 0, (int)pbmi->bmiHeader.biWidth, (int)pbmi->bmiHeader.biHeight,
               (LPSTR)bmp4FindDIBBits((LPSTR)pbmi), pbmi, DIB_RGB_COLORS, SRCCOPY ) ;*/
            // LY Dec 9/04 : fixed so bitmap will be displayed
            hdcBMP = CreateCompatibleDC( hDC ) ;
            if ( !hdcBMP )
               break ;
            hBMP = CreateDIBSection( hdcBMP, pbmi, DIB_RGB_COLORS, &pvBits, 0, 0 ) ;
            if ( !hBMP )
            {
               DeleteDC( hdcBMP ) ;
               break ;
            }
            if ( pbmi->bmiHeader.biBitCount > 8 )
               numBMIColors = 0 ;
            else
               numBMIColors = (short)pow( (double)2, (double)pbmi->bmiHeader.biBitCount ) ;  // AS Dec 13/05 - compile fix
            memcpy( pvBits, &pbmi->bmiColors[numBMIColors], pbmi->bmiHeader.biSizeImage ) ;
            if ( SelectObject( hdcBMP, hBMP ) )
            {
               StretchBlt( hDC, (int)(obj->dev_x + report->dev_margin_left),
                  (int)(obj->dev_y + report->ypos - offset_break), (int)obj->dev_w,
                  (int)obj->dev_h, hdcBMP, 0, 0, (int)pbmi->bmiHeader.biWidth,
                  (int)pbmi->bmiHeader.biHeight, SRCCOPY ) ;
            }
            DeleteObject( hBMP ) ;
            DeleteDC( hdcBMP ) ;
            //GlobalUnlock((HANDLE)obj->data) ;
            break ;
         case obj4type_hline:
         case obj4type_vline:
         case obj4type_frame:
            if( report->for_dbf )
               break ;
            sdc = SaveDC( hDC ) ;
            #ifndef S4WINCE
               SetMapMode( hDC, MM_HIENGLISH ) ;
               SetMapMode( hDC, MM_ANISOTROPIC ) ;
            #endif
            pt.x = obj->dec ;
            LPtoDP( hDC, &pt, 1 ) ;
            lwidth = pt.x ;
            RestoreDC( hDC, sdc ) ;

            hbrush = (HBRUSH)NULL ;
            hpen = (HPEN)NULL ;
            if( obj->style )
            {
               /*lbrush.lbColor = obj->style->color ;
               lbrush.lbStyle = BS_SOLID ;
               lbrush.lbHatch = 0L ;
               hbrush = CreateBrushIndirect( &lbrush ) ;*/
               hbrush = CreateSolidBrush( obj->style->color );
               hpen = CreatePen( PS_SOLID, lwidth, obj->style->color ) ;
            }
            if( hbrush && hpen )
            {
               SelectObject( hDC, hbrush ) ;
               SelectObject( hDC, hpen ) ;
            }
            else
            {
               SelectObject( hDC, GetStockObject(BLACK_PEN) ) ;
               SelectObject( hDC, GetStockObject(BLACK_BRUSH) ) ;
            }

            #ifndef S4WINCE
               if( obj->obj_type_num == obj4type_hline )
               {
                  MoveToEx( hDC, (int)(obj->dev_x + report->dev_margin_left), (int)(obj->dev_y + obj->dev_h/2 + report->ypos - offset_break), NULL ) ;
                  LineTo( hDC, (int)(obj->dev_x + obj->dev_w + report->dev_margin_left), (int)(obj->dev_y + obj->dev_h/2 + report->ypos - offset_break) ) ;
               }
               if( obj->obj_type_num == obj4type_vline )
               {
                  MoveToEx( hDC, (int)(obj->dev_x + obj->dev_w/2 + report->dev_margin_left), (int)(obj->dev_y + report->ypos - offset_break), NULL ) ;
                  LineTo( hDC, (int)(obj->dev_x + obj->dev_w/2 + report->dev_margin_left), (int)(obj->dev_y + obj->dev_h + report->ypos - offset_break) ) ;
               }
            #endif

            if( obj->obj_type_num == obj4type_frame )
            {

               if( !obj->display_zero )
                  SelectObject( hDC, GetStockObject( WHITE_BRUSH ) ) ;

               if( obj->alignment == 0 )
               {
                  Rectangle( hDC, (int)(obj->dev_x + report->dev_margin_left + 1 + (lwidth/2)),
                      (int)(obj->dev_y + report->ypos - offset_break + 1 + (lwidth/2)),
                      (int)(obj->dev_x + obj->dev_w + report->dev_margin_left - (lwidth/2)),
                      (int)(obj->dev_y + obj->dev_h + report->ypos - offset_break - (lwidth/2)) ) ;
               }
               else
               {
                  RoundRect( hDC, (int)(obj->dev_x + report->dev_margin_left + 1 + (lwidth/2)),
                      (int)(obj->dev_y + report->ypos - offset_break + 1 + (lwidth/2)),
                      (int)(obj->dev_x + obj->dev_w + report->dev_margin_left - (lwidth/2)),
                      (int)(obj->dev_y + obj->dev_h + report->ypos - offset_break - (lwidth/2)),
                      20, 20 ) ;
               }
            }

            SelectObject( hDC, GetStockObject(BLACK_PEN) ) ;
            SelectObject( hDC, GetStockObject(BLACK_BRUSH) ) ;
            if( hbrush )
               DeleteObject( hbrush ) ;
            if( hpen )
               DeleteObject( hpen ) ;
            len = 0 ;
            break ;
         case obj4type_text:
            if( report->for_dbf )
            {
               if( obj->field )
                  f4assign( obj->field, obj->wintext ) ;
               break ;
            }

            //output_ptr = obj->wintext ;
            len = strlen( obj->wintext ) ;
            if( len > 0 )
            {
               if( obj->style )
               {
                  if( report->output_code == 1 )
                     SelectObject( hDC, obj->style->screen_font ) ;
                  else
                     SelectObject( hDC, obj->style->printer_font ) ;
                  SetTextColor( hDC, obj->style->color ) ;
               }

               rect.left   = (int)(obj->dev_x + report->dev_margin_left) ;
               rect.top    = (int)(obj->dev_y + report->ypos - offset_break) ;
               rect.right  = rect.left + (int)obj->dev_w ;
               rect.bottom = rect.top + (int)obj->dev_h ;

               switch( obj->alignment )
               {
                  case justify4left:
                     alignment = DT_LEFT ;
                     break ;
                  case justify4right:
                     alignment = DT_RIGHT ;
                     break ;
                  case justify4center:
                     alignment = DT_CENTER ;
                     break ;
               }

               #ifdef S4UNICODE
                  ptext = text;
                  c4atou(obj->wintext, ptext, LEN4PATH);
               #else
                  ptext = obj->wintext ;
               #endif

               DrawText( hDC, ptext, (int)len, &rect, DT_WORDBREAK | alignment | DT_NOPREFIX ) ;
            }
            break ;
         case obj4type_total:
            if( report->for_dbf )
            {
               if( obj->field )
               switch( ((PTOTAL4)obj->data)->totalType )
               {
                  case total4lowest:
                     f4assignDouble( obj->field, ((PTOTAL4)obj->data)->low ) ;
                     break ;

                  case total4highest:
                     f4assignDouble( obj->field, ((PTOTAL4)obj->data)->high ) ;
                     break ;

                  case total4count:
                     f4assignLong( obj->field, ((PTOTAL4)obj->data)->count ) ;
                     break ;

                  case total4sum:
                     f4assignDouble( obj->field, ((PTOTAL4)obj->data)->total ) ;
                     break ;

                  case total4average:
                     f4assignDouble( obj->field, (((PTOTAL4)obj->data)->total/((PTOTAL4)obj->data)->count) ) ;
                     break ;

               }
               break ;
            }
         case obj4type_expr:
         case obj4type_field:
         case obj4type_calc:
            if( report->for_dbf )
            {
               if( !obj->lookahead )
                  obj4evaluate( obj ) ;

               if( obj->field )
               switch( f4typeInternal(obj->field) )
               {
                  case r4str:
                  case r4date:
                     f4assign( obj->field, obj->eval_text ) ;
                     break ;

                  case r4log:
                     f4assignChar( obj->field, *(obj->eval_text) ) ;
                     break ;

                  case r4num:
                     f4assignDouble( obj->field, obj->dval ) ;
                     break ;
               }
               break ;
            }

            if( !obj->lookahead )
               obj4evaluate( obj ) ;

            if( obj->eval_text )
            {
               if( obj->style )
               {
                  if( report->output_code == 1 )
                     SelectObject( hDC, obj->style->screen_font ) ;
                  else
                     SelectObject( hDC, obj->style->printer_font ) ;
                  SetTextColor( hDC, obj->style->color ) ;
               }

               rect.left   = (int)(obj->dev_x + report->dev_margin_left) ;
               rect.top    = (int)(obj->dev_y + report->ypos - offset_break) ;
               rect.right  = rect.left + (int)obj->dev_w ;
               rect.bottom = rect.top + (int)obj->dev_h ;

               switch( obj->alignment )
               {
                  case justify4left:
                     alignment = DT_LEFT ;
                     break ;
                  case justify4right:
                     alignment = DT_RIGHT ;
                     break ;
                  case justify4center:
                     alignment = DT_CENTER ;
                     break ;
               }

               #ifdef S4UNICODE
                  ptext = text;
                  c4atou(obj->eval_text, ptext, LEN4PATH);
               #else
                  ptext = obj->eval_text ;
               #endif

               DrawText( hDC, ptext, obj->eval_len-1, &rect, DT_WORDBREAK | alignment | DT_NOPREFIX ) ;
            }
            break ;
      #else
         case obj4type_text:
            if( report->for_dbf )
            {
               if( obj->field )
                  f4assign( obj->field, obj->wintext ) ;
               break ;
            }
//            output_ptr = obj->wintext ;
            break ;
         case obj4type_total:
            if( obj->field && report->for_dbf )
            {
               switch( ((PTOTAL4)obj->data)->totalType )
               {
                  case total4lowest:
                     f4assignDouble( obj->field, ((PTOTAL4)obj->data)->low ) ;
                     break ;

                  case total4highest:
                     f4assignDouble( obj->field, ((PTOTAL4)obj->data)->high ) ;
                     break ;

                  case total4count:
                     f4assignLong( obj->field, ((PTOTAL4)obj->data)->count ) ;
                     break ;

                  case total4sum:
                     f4assignDouble( obj->field, ((PTOTAL4)obj->data)->total ) ;
                     break ;
               }
               break ;
            }
         case obj4type_expr:
         case obj4type_field:
         case obj4type_calc:
            if( report->for_dbf )
            {
               if( !obj->lookahead )
                  obj4evaluate( obj ) ;
               if( obj->field )
               switch( f4typeInternal(obj->field) )
               {
                  case r4str:
                  case r4date:
                     f4assign( obj->field, obj->eval_text ) ;
                     break ;

                  case r4log:
                     f4assignChar( obj->field, *(obj->eval_text) ) ;
                     break ;

                  case r4num:
                     f4assignDouble( obj->field, obj->dval ) ;
                     break ;
               }
               break ;
            }

            if( !obj->lookahead )
               obj4evaluate( obj ) ;

/*            if( obj->eval_text )
               output_ptr = obj->eval_text ;
            else
               output_ptr = NULL ;*/
            break ;
      #endif
   }

   #ifndef S4WINDOWS
      file4seqWrite( &hDC->seq_wr, &obj->obj_type_num, sizeof(short) ) ;

      obj_len = 5*sizeof(long) + 2*sizeof(short) ;
      switch( obj->obj_type_num )
      {
         case obj4type_text:
            obj_len += strlen(obj->wintext) ;
            break ;

         case obj4type_frame:
            obj_len += 2 ;
            break ;

         case obj4type_hline:
         case obj4type_vline:
            break ;

         default:
            obj_len += (obj->eval_len - 1) ;
            break ;
      }
      file4seqWrite( &hDC->seq_wr, &obj_len, sizeof(obj_len) ) ;

      lval = obj->dev_x + report->dev_margin_left ;
      file4seqWrite( &hDC->seq_wr, &lval, sizeof(lval) ) ;
      lval = obj->dev_y + report->ypos - offset_break ;
      file4seqWrite( &hDC->seq_wr, &lval, sizeof(lval) ) ;
      file4seqWrite( &hDC->seq_wr, &obj->dev_w, sizeof(obj->dev_w) ) ;
      file4seqWrite( &hDC->seq_wr, &obj->dev_h, sizeof(obj->dev_h) ) ;
      file4seqWrite( &hDC->seq_wr, &obj->alignment, sizeof(obj->alignment) ) ;

      if( obj->style )
      {
         file4seqWrite( &hDC->seq_wr, &obj->style->position, sizeof(obj->style->position) ) ;
      }
      else
      {
         i = 0 ;
         file4seqWrite( &hDC->seq_wr, &i, sizeof(short) ) ;
      }

      switch( obj->obj_type_num )
      {
         case obj4type_text:
            data_len = strlen(obj->wintext) ;
            file4seqWrite( &hDC->seq_wr, &data_len, sizeof(data_len) ) ;
            file4seqWrite( &hDC->seq_wr, obj->wintext, (unsigned)data_len ) ;
            break ;

         case obj4type_frame:
            data_len = 2 ;
            file4seqWrite( &hDC->seq_wr, &data_len, sizeof(data_len) ) ;
            file4seqWrite( &hDC->seq_wr, &obj->display_zero, 1 ) ;
            file4seqWrite( &hDC->seq_wr, &obj->alignment, 1 ) ;
            break ;

         case obj4type_hline:
         case obj4type_vline:
            data_len = 0 ;
            file4seqWrite( &hDC->seq_wr, &data_len, sizeof(data_len) ) ;
            break ;

         default:
            data_len = obj->eval_len - 1 ;
            file4seqWrite( &hDC->seq_wr, &data_len, sizeof(data_len) ) ;
            file4seqWrite( &hDC->seq_wr, obj->eval_text, (unsigned)data_len ) ;
            break ;
      }
   #endif

   obj_on = (OBJ4 *)l4first( &obj->contained ) ;
   while( obj_on )
   {
      #ifdef S4WINDOWS
         report4output_object( obj_on, hDC ) ;
      #else
         report4output_object( obj_on, hDC ) ;
      #endif
      obj_on = (OBJ4 *)l4next( &obj->contained, obj_on ) ;
   }
   return 0 ;
}

GROUP4 * S4FUNCTION report4calc_first_change_group( REPORT4 *report )
{
   GROUP4 *group_on, *change_group = NULL ;
   char  *ptr ;
   int len ;

   group_on = (GROUP4 *)l4first( &report->groups ) ;
   if( group_on->title_summary )
      group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
   while( group_on )
      {
      if( !group_on->resetExpression && !change_group )
         change_group = group_on ;

      if( group_on->resetExpression )
         {
         len = expr4vary( group_on->resetExpression, &ptr ) ;
         if( c4memcmp(group_on->lastResetValue, ptr, len) != 0 && !change_group )
            change_group = group_on ;

         memcpy( group_on->lastResetValue, ptr, len ) ;
         }
      group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
      }

   return change_group ;
}

int report4alloc_records( REPORT4 *report )
{
   RELATE4 *relate_on ;

   if( !report )
      {
      return -1 ;
      }

   if( error4code( report->codeBase ) < 0 )
      return -1 ;

   relate_on = &report->relate->relation->relate ;

   while(relate_on)
   {
      if( !relate_on->oldRecord )
         relate_on->oldRecord =
            (char *)u4allocFree( report->codeBase, relate_on->data->dataFile->recWidth + 1 ) ;

      if( relate_on->oldRecord == NULL )
         {
         return -1 ;
         }

      relate4next(&relate_on) ;
   }
   return 0 ;
}

int report4make_old_rec( REPORT4 *report )
{
   RELATE4 *relate ;

   if( !report )
      {
      return -1 ;
      }

   if( error4code( report->codeBase ) < 0 )
      return -1 ;

   relate = &report->relate->relation->relate ;

   while(relate)
   {
      memcpy(relate->oldRecord,relate->data->record,relate->data->dataFile->recWidth) ;

      relate4next(&relate) ;

   }

   return 0 ;
}

int report4swap_old_rec( REPORT4 *report )
{
   RELATE4 *relate_on ;
   char *tempptr ;
   #ifndef S4OFF_MEMO
      int i ;
   #endif

   if( !report )
      {
      return -1 ;
      }

   if( error4code( report->codeBase ) < 0 )
      return -1 ;

   relate_on = &report->relate->relation->relate ;

   while (relate_on )
   {
      tempptr = relate_on->data->record ;

      relate_on->data->record = relate_on->oldRecord ;
      relate_on->oldRecord = tempptr ;

      #ifndef S4OFF_MEMO
         if ( relate_on->data->fieldsMemo != 0 )
         {
            for ( i = 0; i < relate_on->data->dataFile->nFieldsMemo ; i++ )
               relate_on->data->fieldsMemo[i].status = 1 ;
         }
      #endif
      relate4next(&relate_on) ;
   }

   return 0 ;
}

int report4output_area_break( AREA4 *area )
{
   int test, flag ;
   OBJ4 *obj_on ;

   if( !area->allow_pagebreaks )
      return 0 ;

   test = (int)(area->report->disp_bottom - area->report->ypos + 1) ;
   flag = 0 ;

   if( area->report->break_height )
      test += (int)area->report->break_height ;

   while(  !flag && test > 0 )
      {
      flag = 1 ;
      test-- ;
      obj_on = (OBJ4 *)l4first( &area->objects ) ;
      while( obj_on )
         {
         if( test >=  obj_on->dev_y && test <= obj_on->dev_y+obj_on->dev_h )
            {
            flag = 0 ;
            test = (int)obj_on->dev_y ;
            obj_on = NULL ;
            }
         else
            obj_on = (OBJ4 *)l4next( &area->objects, obj_on ) ;
         }
      }

   return test ;
}

#ifdef S4WINDOWS
#ifdef __cplusplus
void report4output_pgheader(REPORT4 *report, HDC hDC )
#else
void report4output_pgheader(report, hDC )
REPORT4 *report ;
HDC hDC ;
#endif
#else
#ifdef __cplusplus
void report4output_pgheader(REPORT4 *report, PAGE4 *hDC )
#else
void report4output_pgheader(report, hDC )
REPORT4 *report ;
PAGE4 *hDC ;
#endif
#endif
{
   AREA4 *area_on ;
   short broken ;

   broken = report->broken ;
   report->broken = 0 ;

   area_on = (AREA4 *)l4first( &report->page_header_footer->header_areas ) ;
   while( area_on )
      {
      report4output_area( area_on, hDC, 1 ) ;
      area_on = (AREA4 *)l4next( &report->page_header_footer->header_areas, area_on ) ;

      }
   report->broken = broken ;
}

#ifdef S4WINDOWS
#ifdef __cplusplus
void report4output_pgfooter(REPORT4 *report, HDC hDC )
#else
void report4output_pgfooter(report, hDC )
REPORT4 *report ;
HDC hDC ;
#endif
#else
#ifdef __cplusplus
void report4output_pgfooter(REPORT4 *report, PAGE4 *hDC )
#else
void report4output_pgfooter(report, hDC )
REPORT4 *report ;
PAGE4 *hDC ;
#endif
#endif
{
   AREA4 *area_on ;
   short broken ;

   broken = report->broken ;
   report->broken = 0 ;

   report->ypos = report->disp_bottom ;
   area_on = (AREA4 *)l4first( &report->page_header_footer->footer_areas ) ;
   while( area_on )
      {
      report4output_area( area_on, hDC, 1 ) ;
      area_on = (AREA4 *)l4next( &report->page_header_footer->footer_areas, area_on ) ;
      }
   report->broken = broken ;
}

#ifdef S4WINDOWS
#ifdef __cplusplus
void report4output_repeat_headers(REPORT4 *report, HDC hDC, GROUP4 *group_first )
#else
void report4output_repeat_headers(report, hDC, group_first )
REPORT4 *report ;
HDC hDC ;
GROUP4 *group_first ;
#endif
#else
#ifdef __cplusplus
void report4output_repeat_headers(REPORT4 *report, PAGE4 *hDC, GROUP4 *group_first )
#else
void report4output_repeat_headers(report, hDC, group_first )
REPORT4 *report ;
PAGE4 *hDC ;
GROUP4 *group_first ;
#endif
#endif
{
   GROUP4 *group_on ;

   if( !report->first )
      return ;

   group_on = (GROUP4 *)l4first( &report->groups ) ;
   while( group_on != group_first )
   {
      if( group_on->repeat_header )
         report4output_group_headers( group_on, hDC, group_first, 0 ) ;
      group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
   }
}

#ifdef S4WINDOWS
#ifdef __cplusplus
void report4output_swapped_footer( GROUP4 *group, HDC hDC )
#else
void report4output_swapped_footer(group, hDC )
GROUP4 *group ;
HDC hDC ;
#endif
#else
#ifdef __cplusplus
void report4output_swapped_footer( GROUP4 *group, PAGE4 *hDC )
#else
void report4output_swapped_footer(group, hDC )
GROUP4 *group ;
PAGE4 *hDC ;
#endif
#endif
{
   REPORT4 *report = group->report ;
   AREA4 *area_on ;
   OBJ4 *obj_on ;
   int suppress ;
   long theight ;

   if( report->page_header_footer->footer_areas.nLink > 0 )
      report->ypos = report->disp_bottom ;
   else
   {
      theight = 0 ;
      area_on = (AREA4 *)l4first( &group->footer_areas ) ;
      while( area_on )
      {
         if( !area_on->suppression_condition || expr4true(area_on->suppression_condition) )
            theight += area_on->height_dev ;
         area_on = (AREA4 *)l4next( &group->footer_areas, area_on ) ;
      }
      report->ypos = report->disp_bottom - theight ;
   }

   area_on = (AREA4 *)l4first( &group->footer_areas ) ;
   while( area_on )
   {
      suppress = 0 ;
      if( area_on->suppression_condition )
      {
         suppress = expr4true( area_on->suppression_condition ) ;
      }

      if( suppress )
      {
         area_on = (PAREA4)l4next( &group->footer_areas, area_on ) ;
         continue ;
      }

      obj_on = (OBJ4 *)l4first( &area_on->objects ) ;
      while( obj_on )
      {
         if( report->ypos + obj_on->dev_y + obj_on->dev_h <
             report->dev_page_height - report->dev_margin_bottom )
         {
            report4output_object( obj_on, hDC ) ;
         }

         obj_on = (OBJ4 *)l4next( &area_on->objects, obj_on ) ;
      }
      report->ypos += area_on->height_dev ;
      area_on = (AREA4 *)l4next( &group->footer_areas, area_on ) ;
   }

   if( group->report->broken  == 2 )
   {
      group->report->broken = 0 ;
      group->report->break_height = 0 ;
   }

}

int S4FUNCTION report4pageInit( PREPORT4 report )
{
   int rc, i ;
   int len ;
   TOTAL4 *total_on ;
   GROUP4 *group_on ;
   char   *ptr ;
   PSTYLE4 style ;
   #ifdef S4WINDOWS
      HINSTANCE hInst;
   #else
      char name_buf[13] ;
      int tsafety ;
   #endif

   #ifdef E4PARM_HIGH
      if( report == 0 )
         return error4( 0, e4parm_null, E95702 ) ;
   #endif

   relate4skipEnable( report->relate, 1 ) ;

   #ifdef S4WINDOWS
      report->hWnd2 = 0 ;

      #if   defined(S4WINCE)
         hInst = report->codeBase->hInst;
      #elif defined(S4WIN32)
         hInst = (HINSTANCE)GetWindowLong(report->hWnd, GWL_HINSTANCE);
      #else
         hInst = (HINSTANCE)GetWindowWord(report->hWnd, GWW_HINSTANCE);
      #endif
      /*report->hWnd2 = CreateWindowEx( WS_EX_TRANSPARENT, "MouseEat", "EatInput",
         WS_POPUP | WS_CAPTION | WS_BORDER | WS_VISIBLE, 0, 0, 5, 5, report->hWnd,
         NULL, hInst, NULL ) ;*/
      // CS 2001/06/15 Had to hard code the WS_EX_TRANSPARENT constant because
      // Windows CE does not seem to define it.
      report->hWnd2 = CreateWindowEx( 0x00000020, TEXT("MouseEat"), TEXT("EatInput"),
         WS_POPUP | WS_CAPTION | WS_BORDER | WS_VISIBLE, 0, 0, 5, 5, report->hWnd,
         NULL, hInst, NULL ) ;

      if( report->hWnd2 )
      {
         #ifdef S4WIN32
            SetClassLong( report->hWnd2, GCL_HCURSOR, 0L ) ;
         #else
            SetClassWord( report->hWnd2, GCW_HCURSOR, 0 ) ;
         #endif
         SetCursor( LoadCursor((HINSTANCE)NULL,IDC_WAIT) ) ;
         SetCapture( report->hWnd2 ) ;
      }
   #endif

   rc = relate4top( report->relate ) ;
   switch( rc )
   {
      case r4terminate:
      case r4locked:
      case r4eof:
         break ;
   }

   if( rc < 0 )
      return -1 ;

   report->tdone = 0 ;
   report->page_no = report->page_count = 0 ;

   if( (report4alloc_records( report )) < 0 )
      return -1 ;

   i = 0 ;
   style = (PSTYLE4)l4first( &report->styles ) ;
   while( style )
   {
      style->position = i ;
      i++ ;
      style = (PSTYLE4)l4next( &report->styles, style ) ;
   }

   group_on = (GROUP4 *)l4first( &report->groups ) ;
   while( group_on )
   {
      if( group_on->title_summary )
         group_on->tsdone = 0 ;

      group_on->reset_flag = 0 ;
      if( group_on->lastResetValue )
      {
         u4free( group_on->lastResetValue ) ;
         group_on->lastResetValue = NULL ;
      }
      if( group_on->resetExpression )
      {
         len = expr4vary( group_on->resetExpression, &ptr ) ;
         group_on->lastResetValue = (LPTSTR)u4allocFree( report->codeBase, (len + 1) * sizeof(TCHAR) ) ;
         if( group_on->lastResetValue )
            memcpy( group_on->lastResetValue, ptr, len ) ;
      }
      group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;
   }

   total_on = (TOTAL4 *)l4first( &report->codeBase->totalList ) ;
   while( total_on )
   {
      if( total_on->lastAddValue )
      {
         u4free( total_on->lastAddValue ) ;
         total_on->lastAddValue = NULL ;
      }

      if( total_on->addCondition )
      {
         total_on->lastAddValue =
            (char *)u4allocFree( report->codeBase, expr4len(total_on->addCondition)+1 ) ;
      }

      total_on->donce = 0 ;

      if( total_on->lastResetValue )
      {
         u4free( total_on->lastResetValue ) ;
         total_on->lastResetValue = NULL ;
      }

      if( total_on->resetExpression )
      {
         total_on->lastResetValue = (char *)u4allocFree( report->codeBase, expr4len(total_on->resetExpression)+1 ) ;
      }

      total_on = (TOTAL4 *)l4next( &report->codeBase->totalList, total_on ) ;
   }

   report4check_lookahead( report ) ;

   report4check_display_once( report ) ;

   report->group_on = (GROUP4 *)l4first( &report->groups ) ;
   report->group_first = NULL ;
   report->area_on = NULL ;
   report->in_header = 1 ;
   report->end_report = 0 ;
   report->codeBase->pageno = 0 ;
   report->first = 0 ;

   total_on = (TOTAL4 *) l4first( &report->codeBase->totalList ) ;
   while( total_on )
   {
      total4value_reset( total_on ) ;
      total_on = (TOTAL4 *) l4next( &report->codeBase->totalList, total_on ) ;
   }

   #ifndef S4WINDOWS
   report4calc_obj_dev( report ) ;
   report->dev_page_width = report->report_width + report->margin_right + report->margin_left ;
   report->dev_page_height = report->report_height + report->margin_top + report->margin_bottom ;

   if( report->output_handle > 0 )
   {
      tsafety = report->codeBase->safety ;
      report->codeBase->safety = 0 ;
      // AS Dec 13/05 - improvements for function deprecation
      if( !report->report_file_name || *report->report_file_name =='\0' )
         c4strcpy( name_buf, sizeof(name_buf), "crep2.buf" ) ;
      else
      {
         u4namePiece( name_buf, sizeof(name_buf), report->report_file_name, 0, 1 ) ;
         u4nameExt( name_buf, sizeof(name_buf), "buf", 1 ) ;
      }
      if( file4create( &report->page_buf.file_buf, report->codeBase, name_buf, 1 ) < 0 )
      {
         error4describe( report->codeBase, e4repOut, 0L, E4_REP_PFILE, 0, 0 ) ;
         return -1 ;
      }
      report->codeBase->safety = tsafety ;
      file4seqWriteInit( &report->page_buf.seq_wr, &report->page_buf.file_buf, 0L,
         report->page_buf.fmem_buf, sizeof(report->page_buf.fmem_buf) ) ;
   }
   #endif

   return 0 ;
}

int S4FUNCTION report4pageFree( PREPORT4 report )
{
   char name_buf[512] ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   if( report->page_buf.object.info != NULL )
   {
      u4free( report->page_buf.object.info ) ;
      report->page_buf.object.info = NULL ;
      report->page_buf.object.info_len = 0 ;
   }

   if( report->page_buf.mem_buf != NULL )
   {
      u4free( report->page_buf.mem_buf ) ;
      report->page_buf.mem_buf = NULL ;
   }

   if( report->page_buf.file_buf.name != NULL )
   {
      c4strcpy( name_buf, sizeof( name_buf ), report->page_buf.file_buf.name ) ;
      file4close( &report->page_buf.file_buf ) ;
      u4remove( name_buf ) ;
   }

   report->page_buf.info_buf_len = 0 ;

   return 0 ;
}

POBJECT4 S4FUNCTION report4pageObjNext( PREPORT4 report )
{
   long obj_size ;
   POBJECT4 object ;
   FILE4SEQ_READ *file_buf ;
   FILE4LONG pos;

   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   if( report->page_buf.file_buf.name == NULL )
   {
      error4describe( report->codeBase, e4repOut, 0L, E4_REP_NOBUFPAGE, (char *)0, (char *)0 ) ;
      return NULL ;
   }

   if( !report->page_buf.first_read )
   {
      file4longAssign(pos,0L,0);
      file4seqReadInitDo( &report->page_buf.seq_rd, &report->page_buf.file_buf,
                          pos, report->page_buf.rmem_buf,sizeof(report->page_buf.rmem_buf),1 ) ;
   }

   file_buf = &report->page_buf.seq_rd ;

   object = &report->page_buf.object ;

   if( (file4seqRead( file_buf, &object->objtype, sizeof( object->objtype) )) == 0 )
      return NULL ;

   file4seqRead( file_buf, &obj_size, sizeof(obj_size) ) ;
   file4seqRead( file_buf, &object->x, sizeof(object->x) ) ;
   file4seqRead( file_buf, &object->y, sizeof(object->y) ) ;
   file4seqRead( file_buf, &object->w, sizeof(object->w) ) ;
   file4seqRead( file_buf, &object->h, sizeof(object->h) ) ;

   file4seqRead( file_buf, &object->alignment, sizeof(object->alignment) ) ;

   file4seqRead( file_buf, &object->style_index, sizeof(object->style_index) ) ;

   file4seqRead( file_buf, &object->info_len, sizeof(object->info_len) ) ;

   if( report->page_buf.info_buf_len < object->info_len )
   {
      if( object->info != NULL )
      {
         u4free( object->info ) ;
         object->info = NULL ;
      }

      object->info = (char *)u4allocFree( report->codeBase, object->info_len+1 ) ;
      if( !object->info )
      {
         error4describe( report->codeBase, e4repOut, 0L, E4_REP_OBJDATAAL, (char *)0, (char *)0 ) ;
         return NULL ;
      }

      memset( object->info, 0, (int)report->page_buf.info_buf_len ) ;
      report->page_buf.info_buf_len = object->info_len + 1 ;
      if( object->info_len > 0 )
         file4seqRead( file_buf, object->info, (unsigned)object->info_len ) ;
   }
   else
   {
      memset( object->info, 0, (int)report->page_buf.info_buf_len ) ;
      if( object->info_len > 0 )
    file4seqRead( file_buf, object->info, (unsigned)object->info_len ) ;
   }

   return object ;
}

POBJECT4 S4FUNCTION report4pageObjFirst( PREPORT4 report )
{
   FILE4LONG pos;

   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   if( report->page_buf.file_buf.name == NULL )
   {
      error4describe( report->codeBase, e4repOut, 0L, E4_REP_NOBUFPAGE, (char *)0, (char *)0 ) ;
      return NULL ;
   }
   file4longAssign(pos,0L,0);
   file4seqReadInitDo( &report->page_buf.seq_rd, &report->page_buf.file_buf,
                       pos, report->page_buf.rmem_buf,sizeof(report->page_buf.rmem_buf),1 ) ;

   report->page_buf.first_read = 1 ;
   return report4pageObjNext( report ) ;

}

#ifndef S4WINDOWS
void r4place_text( OBJECT4 *object, char *buffer, int lw, char *sbuffer )
{
   int ox, oy, oh, ow, lc ;
   int offset, length ;
   char *pbuf, *line, *sbuf, *bpt ;

   ox = (int)(((float)object->x/100.0)+0.5) ;
   oy = (int)(((float)object->y/1000.0)*6.0+0.5) ;
   oh = (int)(((float)object->h/1000.0)*6.0+0.5) ;
   ow = (int)(((float)object->w/100.0)+0.5) ;

   pbuf = (buffer + ox + oy * lw ) ;
   if( sbuffer )
      sbuf = (sbuffer + ox + oy * lw ) ;

   if( oh == 1 )
   {
      if( ow >= object->info_len && ox + ow <= lw )
      {
         offset = 0 ;
         if( object->alignment == justify4right )
            offset = ow - (int)object->info_len ;

         if( object->alignment == justify4center )
            offset = (ow - (int)object->info_len)/2 ;


         memcpy( pbuf + offset, object->info, (int)object->info_len ) ;
         if( sbuffer )
            memset( sbuf + offset, object->style_index, (int)object->info_len ) ;
         return ;
      }
      else
      {
         if( ox + ow <= lw )
            length = ow ;
         else
            length = lw - ox ;
         memcpy( pbuf, object->info, length ) ;
         if( sbuffer )
            memset( sbuf, object->style_index, length ) ;
      }
   }
   else
   {
      lc = length = 0 ;
      if( ox + ow <= lw )
         length = ow ;
      else
         length = lw - ox ;

      line = strtok( (char *)object->info, "\r\n" ) ;
      while( line && lc < oh )
      {
         if( strlen(line) > (unsigned)length )
         {
            while( strlen(line) > (unsigned)length && lc < oh )
            {
               bpt = line + length ;
               while( bpt != line && *bpt != ' ' )
                  bpt-- ;

               if( bpt == line )
                  bpt = line + length ;

               memcpy( pbuf, line, (bpt-line) ) ;
               if( sbuffer )
               {
                  memset( sbuf, object->style_index, (bpt-line) ) ;
                  sbuf += lw ;
               }
               lc++ ;
               line += (bpt-line) ;
               if( *line == ' ' )
                  line++ ;
               pbuf += lw ;
            }
            memcpy( pbuf, line, strlen(line) ) ;
            if( sbuffer )
            {
               memset( sbuf, object->style_index, strlen(line) ) ;
               sbuf += lw ;
            }
            pbuf += lw ;
            lc++ ;

         }
         else
         {
            offset = 0 ;
            if( object->alignment == justify4right )
               offset = length - strlen(line) ;
            if( object->alignment == justify4center )
               offset = (length - strlen(line))/2 ;

            memcpy( pbuf + offset, line, strlen(line) ) ;
            if( sbuffer )
            {
               memset( sbuf + offset, object->style_index, strlen(line) ) ;
               sbuf += lw ;
            }
            lc++ ;
            pbuf += lw ;
         }
         line = strtok( NULL, "\r\n" ) ;
      }
   }
}

int report4output_general_dos( PREPORT4 report )
{
   OBJECT4 *object ;
   char    *page_buf=NULL, *style_buf=NULL ;
   int     ph, pw, old_style, new_style, offset ;
   long    lpw, lph, i, j ;
   PSTYLE4 style ;

   report4pageSizeGet( report, &lpw, &lph ) ;
   pw = (int)(((float)lpw/100.0)+0.5) ;
   ph = (int)(((float)lph/1000.0)*6.0+0.5) ;

   page_buf = (char *)u4allocFree( report->codeBase, (pw*ph) ) ;
   if( !page_buf )
   {
      return -1 ;
   }

   if( report->use_styles )
   {
      style_buf = (char *)u4allocFree( report->codeBase, (pw*ph) ) ;
      if( !style_buf )
         return -1 ;
      memset( style_buf, 0, (pw*ph) ) ;
   }
   memset( page_buf, ' ', (pw*ph) ) ;

   object = report4pageObjFirst( report ) ;

   while( object )
   {
      switch( object->objtype )
      {
         case obj4type_field:
         case obj4type_expr:
         case obj4type_total:
         case obj4type_calc:
         case obj4type_text:
            if( report->use_styles )
               r4place_text( object, page_buf, pw, style_buf ) ;
            else
               r4place_text( object, page_buf, pw, NULL ) ;
            break ;
      }


      object = report4pageObjNext( report ) ;
   }

   style = NULL ;
   old_style = 0 ;
   for( i = 0; i < ph; i++ )
   {
      for( j = 0; j < pw; j++ )
      {
         if( report->use_styles )
         {
            offset = (int)(i*pw + j) ;
            #ifdef S4DATA_ALIGN
               memcpy( &new_style, style_buf+offset, sizeof(new_style) ) ;
            #else
               new_style = (int)(*(style_buf+offset)) ;
            #endif
            if( new_style != old_style )
            {
               if( style )
                  write( report->output_handle, style->codes_after, style->codes_after_len ) ;
               style = style4index( report, new_style ) ;
               if( style )
                  write( report->output_handle, style->codes_before, style->codes_before_len ) ;
               old_style = new_style ;
            }
         }
         write( report->output_handle, page_buf + (i*pw) + j, 1 ) ;
      }
      if( i < ph - 1 )
         write( report->output_handle, "\r\n", 2 ) ;
   }
   if( style )
      write( report->output_handle, style->codes_after, style->codes_after_len ) ;
   write( report->output_handle, "\f", 1 ) ;

   if( page_buf ) u4free( page_buf ) ;
   if( style_buf ) u4free( style_buf ) ;
   return 0 ;
}

int report4output_screen_dos( PREPORT4 report )
{
   OBJECT4 *object ;
   char    *page_buf=NULL ;
   int     i, ph, pw ;


   pw = (int)(((float)report->dev_page_width/100.0)+0.5) ;
   ph = (int)(((float)report->dev_page_height/1000.0)*6.0+0.5) ;

   page_buf = (char *)u4allocFree( report->codeBase, (pw*ph) ) ;
   if( !page_buf )
   {
      return -1 ;
   }

   memset( page_buf, ' ', (pw*ph) ) ;

   object = report4pageObjFirst( report ) ;

   while( object )
   {
      switch( object->objtype )
      {
         case obj4type_field:
         case obj4type_expr:
         case obj4type_total:
         case obj4type_calc:
         case obj4type_text:
            r4place_text( object, page_buf, pw, NULL ) ;
            break ;
      }


      object = report4pageObjNext( report ) ;
   }
   for( i = 0; i < ph; i++ )
   {
      write( 1, page_buf+(i*pw), pw-1 ) ;
      if( i < ph-1 )
         write( 1, "\r\n", 2 ) ;
   }

   #ifndef S4TESTING
      #ifndef S4WINTEL
         if( getchar() == 0x1b )
      #else
         #ifdef S4PASCAL_DOS
            fflush( stdin ) ;
            if( getchar() == 0x1b )
         #else
            if( getch() == 0x1b )
         #endif
      #endif
         return -1 ;
   #endif

   write( 1, "\r\n", 2 ) ;
   if( page_buf ) u4free( page_buf ) ;
   return 0 ;
}

int S4FUNCTION report4do( REPORT4 *report )
{
   int rc ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   if( report4pageInit( report ) < 0 )
      return -1 ;

   while( (rc = report4generatePage( report )) >= 0 )
   {
      if( rc == 2 )
         break ;

      if( report->output_handle == 1 )
      {
         if( report4output_screen_dos( report ) < 0 )
            break ;
      }
      else
      if( report->output_handle > 1 )
      {
         if( report4output_general_dos( report ) < 0 )
            break ;
      }
   }

   report4pageFree( report ) ;

   return 0 ;
}
#endif
#endif   /* S4OFF_REPORT */


