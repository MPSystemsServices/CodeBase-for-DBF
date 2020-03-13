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

/* r4area.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4OFF_REPORT
/************************************************************
 ** Function: area4create()
 *
 *  PARAMETERS:
 *     GROUP4* group      - pointer to the containing GROUP4
 *     long    height     - height of the area in 1000's of an inch
 *     short   is_header  - flag indicating whether area is header or footer
 *     char*   expression - character string for suppression condition
 *
 *  DESCRIPTION: allocates the memory for an AREA4 and does initial setup
 *     of internal members, then adds to appropriate list in containing group
 *
 *  RETURNS: an AREA4 pointer on success, NULL on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
AREA4 * S4FUNCTION   area4create( GROUP4 *group, long height, short is_header, char *expression )
{
   AREA4 *area, *area_on;
   EXPR4 *expr;
   int i;

   if( !group )
      return NULL;

   if( height < 0 )
   {
      error4describe( group->report->codeBase, e4parm, 0L, E4_REP_AHEIGHT, 0, 0 );
      return 0;
   }

   /* is suppression condition is provided, parse and check */
   if( expression )
   {
      expr = expr4parse( group->report->relate->data, expression );
      if( expr == 0 )
      {
         error4describe( group->report->codeBase, e4areaCreate, 0L, E4_REP_AEXPR, 0, 0 );
         return 0;
      }
   }
   else
      expr = NULL;

   /* allocate AREA4 */
   area = (AREA4 *) u4allocFree( group->report->codeBase, sizeof(AREA4) );
   if(area == 0)
   {
      error4describe( group->report->codeBase, e4areaCreate, 0L, E4_REP_AMEM, 0, 0 );
      return 0;
   }

   /* set internal members */
   area->height = height;
   area->suppression_condition = expr;
   area->group = group;
   area->allow_pagebreaks = 1;
   area->report = group->report;
   area->is_header = is_header;

   /* insert AREA4 into appropriate list in GROUP4 struct and then re-number
      the areas */
   if( is_header )
   {
      l4add( &group->header_areas, area );
   }
   else
   {
      l4add( &group->footer_areas, area );
   }

   i = 1;
   if( is_header )
      area_on = (AREA4 *) l4first( &group->header_areas );
   else
      area_on = (AREA4 *) l4first( &group->footer_areas );
   while(area_on )
   {
      area_on->position = i;
      if( is_header )
         area_on = (AREA4 *) l4next( &group->header_areas, area_on );
      else
         area_on = (AREA4 *) l4next( &group->footer_areas, area_on );
      i++;
   }

   return area;
}

/************************************************************
 ** Function: area4free()
 *
 *  PARAMETERS: AREA4* area - pointer to area to be freed
 *
 *  DESCRIPTION: frees the specified area, including any contained objects
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void S4FUNCTION   area4free( AREA4 *area )
{
   OBJ4 *obj_on;

   /* free the supppression condition */
   if( area->suppression_condition != NULL )
   {
      expr4free( area->suppression_condition );
   }

   /* free the contained objects */
   while( (obj_on=(POBJ4)l4first(&area->objects)) != NULL )
   {
      obj4delete( obj_on );
   }

   /* Destroy the associated window */
   #ifdef S4WINDOWS
      if( area->hWnd )
      {
         DestroyWindow( area->hWnd );
         area->hWnd = 0;
      }
   #endif

   /* remove from the groups list */
   if( area->is_header )
      l4remove( &area->group->header_areas, area );
   else
      l4remove( &area->group->footer_areas, area );

   /* free the memory */
   u4free( area );

}

/************************************************************
 ** Function: area4insert_object()
 *
 *  PARAMETERS:  LIST4* list - list to insert the object into
 *               POBJ4  obj  - object to be inserted
 *
 *  DESCRIPTION: inserts the object into the list, if the
 *   object falls within the boundaries of another object it
 *   is inserted into the bounding objects list
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void area4insert_object( LIST4 *listPtr, POBJ4 obj )
{
   POBJ4 obj_on;

   obj_on = (POBJ4)l4first( listPtr );
   while( obj_on )
   {
      /* if the object is inside another object */
      if( obj->x > obj_on->x && obj->y > obj_on->y &&
          obj->w+obj->x < obj_on->w+obj_on->x &&
          obj->h+obj->y < obj_on->h+obj_on->y )
      {
         obj->container = obj_on;
         area4insert_object( &obj_on->contained, obj );
         return;
      }
      obj_on = (POBJ4)l4next( listPtr, obj_on );
   }

   /* add at beginning or end of list */
   if( !obj->background )
      l4add( listPtr, obj );
   else
      l4addBefore( listPtr, l4first(listPtr), obj );
   return;
}

/************************************************************
 ** Function: area4insert_y()
 *
 *  PARAMETERS: LIST4* list - list to insert object into
 *              POBJ4  obj_out - object to be inserted
 *
 *  DESCRIPTION: inserts the specified object into the list
 *   on the basis of the y coordinate
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void area4insert_y( LIST4 *listPtr, POBJ4 obj_out )
{
   POBJ4 obj_on;
   int add_flag;

   obj_on = (POBJ4)l4first( listPtr );
   if( !obj_on )
   {
      l4add( listPtr, obj_out );
   }
   else
   {
      add_flag = 0;
      while( obj_on )
      {
         if( obj_out->y < obj_on->y )
         {
            l4addBefore( listPtr, obj_on, obj_out );
            add_flag = 1;
         }

         if( !add_flag )
            obj_on = (POBJ4)l4next( listPtr, obj_on );
         else
            obj_on = NULL;
      }
      if( !add_flag )
         l4add( listPtr, obj_out );
   }

}

/************************************************************
 ** Function: area4insert_xrev()
 *
 *  PARAMETERS: LIST4* list - list to insert object into
 *              POBJ4  obj_out - object to be inserted
 *
 *  DESCRIPTION: inserts the specified object into the list
 *   on the basis of the x coordinate
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void area4insert_xrev( LIST4 *listPtr, POBJ4 obj_out )
{
   POBJ4 obj_on;
   int add_flag;

   obj_on = (POBJ4)l4first( listPtr );
   if( !obj_on )
   {
      l4add( listPtr, obj_out );
   }
   else
   {
      add_flag = 0;
      while( obj_on )
      {
         if( obj_out->x > obj_on->x )
         {
            l4addBefore( listPtr, obj_on, obj_out );
            add_flag = 1;
         }

         if( !add_flag )
            obj_on = (POBJ4)l4next( listPtr, obj_on );
         else
            obj_on = NULL;
      }
      if( !add_flag )
         l4add( listPtr, obj_out );
   }

}

/************************************************************
 ** Function: area4sort_list()
 *
 *  PARAMETERS: LIST4 *list - list containing OBJ4 structures
 *
 *  DESCRIPTION: sorts the list of objects
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void area4sort_list( LIST4 *listPtr )
{
   LIST4 hold_list, int_list;
   POBJ4 obj_on, obj_out, obj_bound;
   long  ybound;

   memset( &hold_list, 0, sizeof(LIST4) );
   memset( &int_list, 0, sizeof(LIST4) );

   while( (obj_out = (POBJ4)l4pop(listPtr)) != NULL )
   {
      area4insert_y( &hold_list, obj_out );
   }

   while( hold_list.nLink > 0 )
   {
      obj_bound = (POBJ4)l4first( &hold_list );
      l4remove( &hold_list, obj_bound );
      area4insert_xrev( &int_list, obj_bound );
      ybound = obj_bound->y;
      obj_on = (POBJ4)l4first( &hold_list );
      while( obj_on && obj_on->y == ybound )
      {
         l4remove( &hold_list, obj_on );
         area4insert_xrev( &int_list, obj_on );
         obj_on = (POBJ4)l4first( &hold_list );
      }

      while( (obj_out = (POBJ4)l4pop( &int_list)) != NULL )
         l4add( listPtr, obj_out );
   }
}

/************************************************************
 ** Function: area4add_object()
 *
 *  PARAMETERS: AREA4* area - area to which an object is to be added
 *              OBJ4*  obj_add - object to be added to the area
 *
 *  DESCRIPTION: adds an object to the specified area
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void S4FUNCTION area4add_object( AREA4 *area, OBJ4 *obj_add )
{
   POBJ4 pObj, pObjnext;
   long i, j;

   if( !area || !obj_add )
      return;

   obj_add->container = NULL;
   area4insert_object( &area->objects, obj_add );
   if( obj_add->x == 0 && obj_add->y == 0 && obj_add->w == 0 && obj_add->h == 0 )
      return;

   pObj = (POBJ4)l4first( &area->objects );
   j = area->objects.nLink;
   i = 0;
   while( pObj &&  i < j )
   {
      pObjnext = (POBJ4)l4next( &area->objects, pObj );
      if( pObj->x > obj_add->x && pObj->y > obj_add->y &&
          pObj->w + pObj->x < obj_add->w + obj_add->x &&
          pObj->h + pObj->y < obj_add->h + obj_add->y )
      {
         l4remove( &area->objects, pObj );
         area4insert_object( &area->objects, pObj );
      }
      pObj = pObjnext;
      i++;
   }
}

/************************************************************
 ** Function: area4sort_obj_tree()
 *
 *  PARAMETERS: PAREA4 area - pointer to the AREA4 struct containing the
 *    objects to be sorted
 *
 *  DESCRIPTION: sorts the objects in the areas object tree
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void S4FUNCTION area4sort_obj_tree( PAREA4 area )
{
   POBJ4 obj_on;

   area4sort_list( &area->objects );
   obj_on = (POBJ4)l4first( &area->objects );
   while( obj_on )
   {
      if( obj_on->contained.nLink > 0 )
         area4sort_list( &obj_on->contained );
      obj_on = (POBJ4)l4next( &area->objects, obj_on );
   }
}

/************************************************************
 ** Function: area4pageBreak()
 *
 *  PARAMETERS: PAREA4 area - area to toggle break flag of
 *              int    breaks - new value for flag
 *
 *  DESCRIPTION: toggles the flag allowing page breaks within the area
 *
 *  RETURNS: the previous value of the flag
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
int S4FUNCTION area4pageBreak( PAREA4 area, int breaks )
{
   int temp;

   if( area == NULL || breaks < 0 )
   {
      if( area )
         error4describe( area->report->codeBase, e4parm, 0L, E4_REP_PGBRK, 0, 0 );
      return -1;
   }

   temp = area->allow_pagebreaks;
   area->allow_pagebreaks = breaks;

   return temp;
}

/****************************************************************************
The rest of the functions contained in this file are documented in the
CodeReporter manual.
*/
PAREA4 S4FUNCTION group4headerFirst( PGROUP4 group )
{
   if( !group )
      return NULL;

   return (PAREA4)l4first( &group->header_areas );
}

PAREA4 S4FUNCTION group4headerNext( PGROUP4 group, PAREA4 area )
{
   if( !group )
      return NULL;

   return (PAREA4)l4next( &group->header_areas, area );
}

PAREA4 S4FUNCTION group4headerLast( PGROUP4 group )
{
   if( !group )
      return NULL;

   return (PAREA4)l4last( &group->header_areas );
}

PAREA4 S4FUNCTION group4headerPrev( PGROUP4 group, PAREA4 area )
{
   if( !group )
      return NULL;

   return (PAREA4)l4prev( &group->header_areas, area );
}

int S4FUNCTION group4numHeaders( PGROUP4 group )
{
   if( !group )
      return 0;

   return group->header_areas.nLink;
}

PAREA4 S4FUNCTION group4footerFirst( PGROUP4 group )
{
   if( !group )
      return NULL;

   return (PAREA4)l4first( &group->footer_areas );
}

PAREA4 S4FUNCTION group4footerNext( PGROUP4 group, PAREA4 area )
{
   if( !group  )
      return NULL;

   return (PAREA4)l4next( &group->footer_areas, area );
}

PAREA4 S4FUNCTION group4footerLast( PGROUP4 group )
{
   if( !group )
      return NULL;

   return (PAREA4)l4last( &group->footer_areas );
}

PAREA4 S4FUNCTION group4footerPrev( PGROUP4 group, PAREA4 area )
{
   if( !group )
      return NULL;

   return (PAREA4)l4prev( &group->footer_areas, area );
}

int S4FUNCTION group4numFooters( PGROUP4 group )
{
   if( !group )
      return 0;

   return group->footer_areas.nLink;
}


POBJ4 S4FUNCTION area4objFirst( PAREA4 area )
{
   if( !area )
   {
      return NULL;
   }

   return (POBJ4)l4first( &area->objects );
}

POBJ4 S4FUNCTION area4objNext( PAREA4 area, POBJ4 aobj )
{
   POBJ4 next_obj, obj;

   if( !area || !aobj )
   {
      if( area && !aobj )
         error4describe( area->report->codeBase, e4parm, 0L, E4_REP_OBJNXT, 0, 0 );
      return NULL;
   }

   obj = aobj;

   if( obj->contained.nLink > 0 )
   {
      return (POBJ4)l4first( &obj->contained );
   }

   for( ;; )
   {
      if( !obj )
         return obj;

      if( obj->container == 0 )
      {
         return (POBJ4)l4next( &area->objects, obj );
      }

      next_obj = (POBJ4)l4next( &obj->container->contained, obj );
      if( next_obj )
         return next_obj;

      obj = obj->container;
   }

}

POBJ4 S4FUNCTION area4objPrev( PAREA4 area, POBJ4 aobj )
{
   POBJ4 prev_obj, obj;

   if( !area || !aobj )
   {
      if( area && !aobj )
         error4describe( area->report->codeBase, e4parm, 0L, E4_REP_OBJPRV, 0, 0 );
      return NULL;
   }

   prev_obj = area4objFirst( area );
   if( prev_obj == aobj )
      return NULL;

   obj = area4objNext( area, prev_obj );
   while( obj && obj != aobj )
   {
      prev_obj = obj;
      obj = area4objNext( area, prev_obj );
   }

   return prev_obj;

}

POBJ4 S4FUNCTION area4objLast( PAREA4 area )
{
   POBJ4 obj, prev_obj;

   if( !area )
   {
      return NULL;
   }

   prev_obj = area4objFirst( area );
   obj = area4objNext( area, prev_obj );
   while( obj != NULL )
   {
      prev_obj = obj;
      obj = area4objNext( area, prev_obj );
   }

   return prev_obj;
}

int S4FUNCTION area4numObjects( PAREA4 area )
{
   int objcount;
   POBJ4 obj;

   if( !area )
   {
      return -1;
   }

   objcount = 0;
   obj = area4objFirst( area );
   while( obj )
   {
      objcount++;
      obj = area4objNext( area, obj );
   }

   return objcount;
}
#endif  /* S4OFF_REPORT */
