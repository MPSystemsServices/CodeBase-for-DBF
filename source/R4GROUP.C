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

/* r4group.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4OFF_REPORT
/************************************************************
 *
 * Function: group4create_group
 *
 *  PARAMETERS:
 *   REPORT4* report - pointer to the current report
 *   char* name - name for the new group
 *   char* resetExpression - character source for the group reset
 *
 *  DESCRIPTION: low level function for group creation
 *
 *  RETURNS: pointer to a GROUP4 structure on success, NULL on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
GROUP4 * group4create_group( REPORT4 *report, char *name, char *resetExpression )
{
   GROUP4 *group, *group_on ;
   EXPR4 *expr = NULL ;

   if( !report )
      return NULL ;

   /* check that the name is not a duplicate */
   if( name && name[0] != '\0' )
   {
      group_on=0 ;
      group_on=(GROUP4 *)l4next(&report->groups,group_on) ;
      while( group_on )
      {
         if( strcmp( group_on->group_label, name ) == 0 )
         {
            error4describe( report->codeBase, e4groupCreate, 0L, E4_REP_DUPGNAME, 0, 0 ) ;
            return NULL ;
         }
         group_on=(GROUP4 *)l4next(&report->groups,group_on) ;
      }
   }

   /* if there is a reset expression make sure that it is valid */
   if( resetExpression && resetExpression[0] != '\0' )
   {
      expr = expr4parse( report->relate->data, resetExpression ) ;
      if( ! expr )
      {
         error4describe( report->codeBase, e4groupCreate, 0L, E4_REP_GEXPR, 0, 0 ) ;
         return NULL ;
      }
   }

   /* allocate the GROUP4 structure */
   group =  (GROUP4 *) u4allocFree( report->codeBase, sizeof(GROUP4) ) ;

   if ( group == 0 ) /* error unable to create group */
      return NULL ;

   /* reset the header and footer area lists */
   memset( &group->header_areas, 0, sizeof(group->header_areas) ) ;
   memset( &group->footer_areas, 0, sizeof(group->footer_areas) ) ;

   /* set the reset expression */
   group->resetExpression = expr ;

   /* set the group name */
   if( name && name[0] != '\0' )
      u4ncpy( group->group_label, name, sizeof( group->group_label ) ) ;
   else
   {
      // AS Dec 13/05 vs 5.0 fixes
      #ifdef S4WINDOWS_VS5_PLUS
         sprintf_s( group->group_label, sizeof( group->group_label ), "Group %d", report->groups.nLink ) ;
      #else
         sprintf( group->group_label, "Group %d", report->groups.nLink ) ;
      #endif
   }

   group->report =  report ;

   return group ;
}

/************************************************************
 *
 * Function: group4create_title_summary()
 *
 *  PARAMETERS: REPORT4* report - report to create a title/summary group for
 *
 *  DESCRIPTION: creates a title/summary group for the report, unlike
 *   the page header/footer group the title summary is maintained as part
 *   of the overall group list.  It is the first group in the list.
 *   This function is automatically called in report4init()
 *
 *  RETURNS: pointer to GROUP4 on success, NULL on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
GROUP4 * S4FUNCTION group4create_title_summary( REPORT4 *report )
{
   GROUP4 *group, *group_on ;

   #ifdef S4GERMAN
      group = group4create_group( report, "Titel/SchluB", NULL ) ;
   #else
      group = group4create_group( report, "Title/Summary", NULL ) ;
   #endif

   if( !group )
      return NULL ;
   group_on = (GROUP4 *)l4first( &report->groups ) ;
   if( !group_on )
      l4add( &report->groups, group ) ;
   else
      l4addBefore( &report->groups, group_on, group ) ;

   group->title_summary = 1 ;
   report->title_summary = group ;
   return group ;
}

/************************************************************
 *
 * Function: group4create_pgheader_pgfooter()
 *
 *  PARAMETERS: REPORT4* report - pointer to the current report
 *
 *  DESCRIPTION: creates a page header/footer group for the report.
 *   this group is not kept in the reports group list.  This fctn.
 *   is called by report4init()
 *
 *  RETURNS: GROUP4 pointer on success, NULL on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
GROUP4 * S4FUNCTION group4create_pgheader_pgfooter( REPORT4 *report )
{
   GROUP4 *group ;

   #ifdef S4GERMAN
      group = group4create_group( report, "Seintenkopf/-fuB", NULL ) ;
   #else
      group = group4create_group( report, "Pg Header/Footer", NULL ) ;
   #endif

   if( !group )
      return NULL ;

   group->header_footer = 1 ;
   report->page_header_footer = group ;
   return group ;
}

/************************************************************
 *
 * Function: group4create()
 *
 *  PARAMETERS:
 *
 *  DESCRIPTION:           SEE the CR MANUAL
 *
 *  RETURNS:
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */

GROUP4 * S4FUNCTION group4create( REPORT4 *r4, char *name, char *resetExpression )
{
   GROUP4 *g4group, *group_on ;
   int i ;

   if( r4 == NULL )
   {
      /*error message; */
      return NULL ;
   }

   /* call the low level group create fctn */
   g4group = group4create_group( r4, name, resetExpression ) ;
   if( !g4group )
      return NULL ;

   /* the group is always added immediately after the title summary, thus
      becoming the reports outermost group */
   group_on = (PGROUP4)l4first( &r4->groups ) ;
   l4addAfter( &r4->groups, group_on, g4group ) ;

   /* set the position of each group, positioning runs reverse to the order
      of the list.  So the last group on the list will be group 1 (usually the
      report body) and the title/summary will be the last group, even though it
      is first on the list */
   i = 1 ;
   group_on = (GROUP4 *)l4last( &r4->groups ) ;
   while(group_on)
   {
      group_on->position =  i ;
      group_on=(GROUP4 *)l4prev(&r4->groups,group_on) ;
      i++ ;
   }

   return g4group ;
}

/************************************************************
 *
 * Function: group4free()
 *
 *  PARAMETERS: GROUP4* group - pointer to the group to be freed
 *
 *  DESCRIPTION: SEE THE MANUAL
 *
 *  RETURNS: none
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void S4FUNCTION   group4free( GROUP4 *group )
{
   AREA4 *area_on ;

   /* free the reset expression */
   if( group->resetExpression )
      expr4free( group->resetExpression ) ;

   /* free the stored reset value */
   if( group->lastResetValue )
      u4free( group->lastResetValue ) ;

   /* if in windows delete the info windows for the group header and footer,
      these are the little windows that display info on the currently selected
      area, etc. */
   #ifdef S4WINDOWS
      if( group->header_info_hWnd )
      {
         DestroyWindow( group->header_info_hWnd ) ;
         group->header_info_hWnd = 0 ;
      }
      if( group->footer_info_hWnd )
      {
         DestroyWindow( group->footer_info_hWnd ) ;
         group->footer_info_hWnd = 0 ;
      }
   #endif

   /* free the header areas */
   while( (area_on = (AREA4 *) l4first( &group->header_areas )) != NULL)
   {
      area4free( area_on ) ;

   }

   /* free the footer areas */
   while( (area_on = (AREA4 *) l4first( &group->footer_areas )) != NULL)
   {
      area4free( area_on ) ;

   }

   /* remove from the reports group list */
   if( group != group->report->page_header_footer )
      l4remove( &group->report->groups, group ) ;

   /* free the GROUP4 structure */
   u4free( group ) ;

}

/************************************************************
 *
 * Function: group4positionSet()
 *
 *  PARAMETERS: GROUP4* group - group to change position of
 *   int position - new position for the group
 *
 *  DESCRIPTION: SEE THE MANUAL
 *
 *  RETURNS: none
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void S4FUNCTION group4positionSet( GROUP4 *group, int position )
{
   GROUP4 *group_on ;
   REPORT4 *report ;
   int i ;

   #ifdef S4DEBUG
   if( group == NULL || position <= 0 )
   {
      if( group && position < 0 )
         error4describe( group->report->codeBase, e4parm, 0L, E4_REP_GPOS, 0, 0 ) ;
      return ;
   }
   #endif

   /* cannot change the position of the title/summary or page header/footer */
   if( group->header_footer || group->title_summary )
      return ;

   report = group->report ;

   /* position cannot be greater than the number of user defined groups */
   if( (unsigned long)position > (report->groups.nLink-1) )  // CS 2000/12/01
      position = report->groups.nLink - 1 ;

   /* remove the group from the list */
   l4remove( &report->groups, group ) ;

   /* redo the position numbering of the groups still in the list */
   i = 1 ;
   group_on = (GROUP4 *)l4last( &report->groups ) ;
   while( group_on )
   {
      group_on->position = i ;
      i++ ;
      group_on = (GROUP4 *)l4prev( &report->groups, group_on ) ;
   }

   /* start at the top of the list, which is the last positioned group.  We
      don't count the title/summary */
   group_on = (GROUP4 *)l4first( &report->groups ) ;
   if( group_on->title_summary )
      group_on = (GROUP4 *)l4next( &report->groups, group_on ) ;

   /* if there are no other groups or the position is 1 add to the end of the
      list */
   if( position == 1 || !group_on )
      l4add( &report->groups, group ) ;
   else
   {
      /* starting at the bottom of the list (first group) find where to insert
         the repositioned group */
      group_on = (PGROUP4)l4last( &report->groups ) ;
      while( group_on )
      {
         if( position <= group_on->position )
         {
            l4addAfter( &report->groups, group_on, group ) ;
            group_on = NULL ;
         }
         else
            group_on = (PGROUP4)l4prev( &report->groups, group_on ) ;
      }
   }

   /* redo the position members */
   i = 1 ;
   group_on = (GROUP4 *)l4last( &report->groups ) ;
   while( group_on )
   {
      group_on->position = i ;
      i++ ;
      group_on = (GROUP4 *)l4prev( &report->groups, group_on ) ;
   }

}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION group4swapHeader( PGROUP4 group, int swap_header )
{
   int temp ;

   if( !group || swap_header < 0 )
   {
      if( group && swap_header < 0 )
         error4describe( group->report->codeBase, e4parm, 0L, E4_REP_SWPHDR, 0, 0 ) ;
      return -1 ;
   }

   temp = group->swap_header ;
   group->swap_header = swap_header ;

   return temp ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION group4swapFooter( PGROUP4 group, int swap_footer )
{
   int temp ;

   if( !group || swap_footer < 0 )
   {
      if( group && swap_footer < 0 )
         error4describe( group->report->codeBase, e4parm, 0L, E4_REP_SWPFTR, 0, 0 ) ;
      return -1 ;
   }

   temp = group->swap_footer ;
   group->swap_footer = swap_footer ;

   return temp ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION group4repeatHeader( PGROUP4 group, int repeat_header )
{
   int temp ;

   if( !group || repeat_header < 0 )
   {
      if( group && repeat_header < 0 )
         error4describe( group->report->codeBase, e4parm, 0L, E4_REP_RPTHDR, 0, 0 ) ;
      return -1 ;
   }

   temp = group->repeat_header ;
   group->repeat_header = repeat_header ;

   return temp ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION group4resetPage( PGROUP4 group, int reset_page )
{
   int temp = 0 ;

   if( !group || reset_page < 0 )
   {
      if( group && reset_page < 0 )
         error4describe( group->report->codeBase, e4parm, 0L, E4_REP_RSTPG, 0, 0 ) ;
      return -1 ;
   }

   if( group->reset_page )
      temp = 1 ;

   group->reset_page = reset_page ;

   return temp ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION group4resetPageNum( PGROUP4 group, int reset_pagenum )
{
   int temp = 0 ;

   if( !group || reset_pagenum < 0 )
   {
      if( group && reset_pagenum < 0 )
         error4describe( group->report->codeBase, e4parm, 0L, E4_REP_RSTPGN, 0, 0 ) ;
      return -1 ;
   }

   if( group->reset_pagenum )
      temp = 1 ;

   group->reset_pagenum = reset_pagenum ;

   return temp ;
}

/* SEE THE CODEREPORTER MANUAL */
PGROUP4 S4FUNCTION report4groupFirst( PREPORT4 report )
{
   PGROUP4 group ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   group = (PGROUP4)l4last( &report->groups ) ;
   if( group == report->title_summary )
      group = NULL ;

   return group ;
}

/* SEE THE CODEREPORTER MANUAL */
PGROUP4 S4FUNCTION report4groupNext( PREPORT4 report, PGROUP4 group )
{
   PGROUP4 group_on ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   group_on = (PGROUP4)l4prev( &report->groups, group ) ;
   if( group_on == report->title_summary )
      group_on = NULL ;

    return group_on ;
}

/* SEE THE CODEREPORTER MANUAL */
PGROUP4 S4FUNCTION report4groupLast( PREPORT4 report )
{
   PGROUP4 group_on ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   group_on = (PGROUP4)l4first( &report->groups ) ;
   if( group_on == report->title_summary )
      group_on = (PGROUP4)l4next( &report->groups, group_on ) ;

   return group_on ;
}

/* SEE THE CODEREPORTER MANUAL */
PGROUP4 S4FUNCTION report4groupPrev( PREPORT4 report, PGROUP4 group )
{
   PGROUP4 group_on ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   group_on = (PGROUP4)l4next( &report->groups, group ) ;
   if( group_on == report->title_summary )
      return NULL ;
   else
      return group_on ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION report4numGroups( PREPORT4 report )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   return report->groups.nLink ;
}

/* SEE THE CODEREPORTER MANUAL */
PGROUP4 S4FUNCTION report4titleSummary( PREPORT4 report )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   return report->title_summary ;
}

/* SEE THE CODEREPORTER MANUAL */
PGROUP4 S4FUNCTION report4pageHeaderFooter( PREPORT4 report )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   return report->page_header_footer ;
}

/* SEE THE CODEREPORTER MANUAL */
PGROUP4 S4FUNCTION report4groupLookup( PREPORT4 report, char *group_name )
{
   PGROUP4 group_on ;

   #ifdef E4PARM_HIGH
           if( report == 0 )
      {
         error4( 0, e4parm_null, E95702 ) ;
         return NULL ;
      }
   #endif

   if( group_name == NULL || group_name[0] == '\0' )
   {
      error4describe( report->codeBase, e4parm, 0L, E4_REP_GRPLK, 0, 0 ) ;
      return NULL ;
   }

   group_on = (PGROUP4)l4first( &report->groups ) ;
   while( group_on )
   {
      if( strcmp( group_on->group_label, group_name ) == 0 )
         return group_on ;
      else
         group_on = (PGROUP4)l4next( &report->groups, group_on ) ;
   }

   return NULL ;
}

/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION group4resetExprSet( PGROUP4 group, char *expr_src )
{
   EXPR4 *expr ;

   if( !group )
   {
      return -1 ;
   }

   if( group->resetExpression )
      expr4free( group->resetExpression ) ;

   group->resetExpression = NULL ;

   if( expr_src != NULL )
   {
      expr = expr4parse( group->report->relate->data, expr_src ) ;
      if( expr )
      {
         group->resetExpression = expr ;
         return 0 ;
      }
      else
      {
         error4describe( group->report->codeBase, e4groupExpr, 0L, E4_REP_GEXPR, 0, 0 ) ;
         return -1 ;
      }
   }
   else
      return 0 ;
}
#endif   /* S4OFF_REPORT */
