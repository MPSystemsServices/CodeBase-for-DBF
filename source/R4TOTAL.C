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

/* r4total.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4OFF_REPORT

#include <math.h>

/* resets the totals values */
void S4FUNCTION   total4value_reset( TOTAL4 *t4 )
{

   t4->low = (double)1.7 * pow(10.0, 308.0) ;
   t4->high = (double)-1.7 * pow(10.0, 308.0) ;
   t4->count = 0 ;
   t4->total = 0.0 ;

}

/* frees the TOTAL4 related memory, then deletes the calculation used to
   create the total */
void S4FUNCTION   total4free( TOTAL4 *total )
{
   if( total->lastResetValue )
   {
      u4free( total->lastResetValue ) ;
      total->lastResetValue = NULL ;
   }

   if( total->resetExpression )
   {
      expr4free( total->resetExpression ) ;
      total->resetExpression = NULL ;
   }

   if( total->lastAddValue )
   {
      u4free( total->lastAddValue ) ;
      total->lastAddValue = NULL ;
   }

   if( total->addCondition )
   {
      expr4free( total->addCondition ) ;
      total->addCondition = NULL ;
   }

   expr4calcDelete( total->report->codeBase, total->calcPtr ) ;

}

/* creates a total */
TOTAL4 * S4FUNCTION   total4create( REPORT4 *r4, char *name, char *expr_src, int totalType, char *reset_expr_src )
{
   EXPR4 *expr, *rexpr ;
   TOTAL4 *t4 ;

   if( !r4 )
      return NULL ;

   if( !name || name[0] == '\0' || !expr_src )
   {
      error4describe( r4->codeBase, e4parm, 0L, E4_REP_TCREATEVAL, 0, 0 ) ;
      return NULL ;
   }

   /* parse the expression */
   expr = expr4parse( r4->relate->data, expr_src ) ;
   if( !expr )
   {
      error4describe( r4->codeBase, e4totalCreate, 0L, E4_REP_TEXPR, 0, 0 ) ;
      return NULL ;
   }

   /* allocate the TOTAL4 structure */
   t4 = (TOTAL4 *)mem4createAllocZero( r4->codeBase, &r4->codeBase->totalMemory,
                                    5, sizeof(TOTAL4), 5, 0 ) ;
   if( !t4 )
   {
      error4describe( r4->codeBase, e4totalCreate, 0L, E4_REP_TMEM, 0, 0 ) ;
      expr4free( expr ) ;
      return NULL ;
   }

   /* create the calculation based on the expression */
   t4->calcPtr = code4calcCreate( r4->codeBase, expr, name ) ;
   if( !t4->calcPtr )
   {
      error4describe( r4->codeBase, e4totalCreate, 0L, E4_REP_TCALC, 0, 0 ) ;
      u4free( t4 ) ;
      expr4free( expr ) ;
      return NULL ;
   }

   /* set the flags and pointers */
   t4->calcPtr->total = t4 ;
   t4->report = r4 ;
   t4->totalType = totalType ;

   /* set the totals reset expression */
   if( reset_expr_src && reset_expr_src[0] != '\0' )
   {
      rexpr = expr4parse( r4->relate->data, reset_expr_src ) ;
      if( !rexpr )
      {
         error4describe( r4->codeBase, e4totalCreate, 0L, E4_REP_TREXPR, 0, 0 ) ;
         expr4free( expr ) ;
         u4free( t4 ) ;
         return NULL ;
      }
      t4->resetExpression = rexpr ;
   }

   l4add( &r4->codeBase->totalList, t4 ) ;

   total4value_reset( t4 ) ;

   return t4 ;
}

/* This function is used for evaluating look ahead totals, it skips ahead
   through the records incrementing the total appropriately until the reset
   condition is met */
void total4evaluate_lookahead( PTOTAL4 total )
{
   long reccount = 0 ;
   int  flag, len, addflag ;
   char *ptr ;
   double tvalue ;

   if( !total->resetExpression && total->donce )
      return ;

   total->donce = 1 ;
   addflag = 1 ;

   /* check the conditional add */
   if( total->addCondition )
   {
      addflag = 0 ;
      if( total->logcon == 0 )
      {
         len = expr4vary( total->addCondition, &ptr ) ;
         if( c4memcmp( total->lastAddValue, ptr, len ) != 0 )
         {
            memset( total->lastAddValue, 0, len+1 ) ;
            memcpy( total->lastAddValue, ptr, len ) ;
            addflag = 1 ;
         }
      }
      else
      {
         if( expr4true( total->addCondition ) )
            addflag = 1 ;
      }
   }

   /* increment the total for the current record */
   if( addflag == 1 )
   {
      tvalue = expr4double( total->calcPtr->expr ) ;
      total->total += tvalue ;
      total->count += 1 ;
      if ( tvalue < total->low )
         total->low = tvalue ;

      if( tvalue > total->high )
         total->high = tvalue ;
   }

   /* start going through the records */
   flag = 1 ;
   while( flag )
   {
      if( relate4skip( total->report->relate, 1L ) != 0 )
      {
         reccount++ ;
         break ;
      }

      reccount++ ;

      /* if this total has a reset expression */
      if( total->resetExpression )
      {
         len = expr4vary( total->resetExpression, &ptr ) ;
         /* if the reset condition is not met */
         if( c4memcmp( ptr, total->lastResetValue, len ) == 0 )
         {
            addflag = 1 ;

            /* check for a conditional total */
            if( total->addCondition )
            {
               addflag = 0 ;
               if( total->logcon == 0 )
               {
                  len = expr4vary( total->addCondition, &ptr ) ;
                  if( c4memcmp( total->lastAddValue, ptr, len ) != 0 )
                  {
                     memset( total->lastAddValue, 0, len+1 ) ;
                     memcpy( total->lastAddValue, ptr, len ) ;
                     addflag = 1 ;
                  }
               }
               else
               {
                  if( expr4true( total->addCondition ) )
                     addflag = 1 ;
               }
            }

            /* increment the total */
            if( addflag == 1 )
            {
               tvalue = expr4double( total->calcPtr->expr ) ;
               total->total += tvalue ;
               total->count += 1 ;
               if ( tvalue < total->low )
                  total->low = tvalue ;

               if( tvalue > total->high )
                  total->high = tvalue ;
            }
         }
         else /* if the reset condition is met */
         {
            flag = 0 ;
         }
      }
      else /* if no reset condition */
      {
         addflag = 1 ;

         /* check for a conditional total */
         if( total->addCondition )
         {
            addflag = 0 ;
            if( total->logcon == 0 )
            {
               len = expr4vary( total->addCondition, &ptr ) ;
               if( c4memcmp( total->lastAddValue, ptr, len ) != 0 )
               {
                  memset( total->lastAddValue, 0, len+1 ) ;
                  memcpy( total->lastAddValue, ptr, len ) ;
                  addflag = 1 ;
               }
            }
            else
            {
               if( expr4true( total->addCondition ) )
                  addflag = 1 ;
            }
         }

         /* increment the total for the current record */
         if( addflag == 1 )
         {
            tvalue = expr4double( total->calcPtr->expr ) ;
            total->total += tvalue ;
            total->count += 1 ;
            if ( tvalue < total->low )
               total->low = tvalue ;

            if( tvalue > total->high )
               total->high = tvalue ;
         }
      }

   }

   /* skip back the appropriate number of records */
   relate4skip( total->report->relate, (long)(-1 * reccount) ) ;

}

/* updates the totals internals */
void total4value_update( TOTAL4 *total )
{
   double tvalue ;
   int len, flag = 1 ;
   char *ptr ;
   int addflag ;

   /* check for a reset on the total */
   if( total->resetExpression )
   {
      len = expr4vary( total->resetExpression, &ptr ) ;
      if( (flag = c4memcmp(total->lastResetValue, ptr, len)) != 0 )
      {
         memset( total->lastResetValue, 0, len+1 ) ;
         memcpy( total->lastResetValue, ptr, len ) ;
         // AS De3c 13/05 MSVC 2005 compile fix
         total->low = (double)1.7 * pow( (double)10, (double)308 ) ;
         total->high = (double)-1.7 * pow( (double)10, (double)308 ) ;
         total->total = 0.0 ;
         total->count = 0 ;
      }
   }

   /* for non-lookahead totals */
   if( !total->lookahead )
   {
      addflag = 1 ;

      /* check for conditional total */
      /* LY 2001/02/06 : check that addCondition is valid EXPR4 */
      if( total->addCondition && total->addCondition->codeBase && total->addCondition->type < 'z' )
      {
         addflag = 0 ;
         if( total->logcon == 0 )
         {
            len = expr4vary( total->addCondition, &ptr ) ;
            if( c4memcmp( total->lastAddValue, ptr, len ) != 0 )
            {
               memset( total->lastAddValue, 0, len+1 ) ;
               memcpy( total->lastAddValue, ptr, len ) ;
               addflag = 1 ;
            }
         }
         else
         {
            if( expr4true( total->addCondition ) )
               addflag = 1 ;
         }
      }

      /* increment the total */
      if( addflag )
      {
         tvalue = expr4double( total->calcPtr->expr ) ;
         total->total += tvalue ;
         total->count += 1 ;
         if( tvalue < total->low )
            total->low = tvalue ;

         if( tvalue > total->high )
            total->high = tvalue ;
      }
   }

   /* evaluate the lookahead */
   if( total->lookahead && flag != 0 )
   {
      total4evaluate_lookahead( total ) ;
   }
}

/* returns a pointer to the TOTAL4 with the given name */
TOTAL4 * S4FUNCTION total4lookup( REPORT4 *report, char *name )
{
   TOTAL4 *total_on ;

   if( !report )
      return NULL ;

   if( !name || name[0] == '\0' )
   {
      error4describe( report->codeBase, e4parm, 0L, E4_REP_TLKP, 0, 0 ) ;
      return NULL ;
   }

   total_on = (TOTAL4 *) l4first( &report->codeBase->totalList ) ;
   while(total_on)
   {
      if ( strcmp( name, total_on->calcPtr->name ) == 0 )
         return total_on ;

      total_on = (TOTAL4 *) l4next( &report->codeBase->totalList,total_on) ;
   }
   return NULL ;
}


/* this function is used when checking whether objects are dependant on a
   certain total or calculation, also in deleting any dependant objects.
   If the function is being used to check for dependancies (delflag = 0) it
   simply returns 1 at the first dependant object found.  If delflag = 1 any
   dependant objects are delted */
int report4checkObjExpr( POBJ4 obj, int delflag )
{
   EXPR4CALC *calc_on ;
   POBJ4     obj_on, obj_next ;
   EXPR4     *expr ;
   CODE4     *cb ;
   int       rc ;

   cb = obj->area->report->codeBase ;

   obj_on = (POBJ4)l4first( &obj->contained ) ;
   while( obj_on )
   {
      obj_next = (POBJ4)l4next( &obj->contained, obj_on ) ;
      rc = report4checkObjExpr( obj_on, delflag ) ;
      if( rc == 1 )
         return rc ;
      obj_on = obj_next ;
   }

   switch( obj->obj_type_num )
   {
      case obj4type_expr:
         expr = expr4parse( obj->area->report->relate->data, expr4source((EXPR4 *)obj->data) ) ;
         if( expr )
         {
            expr4free( expr ) ;
         }
         else
         {
            if( delflag )
               obj4delete( obj ) ;
            else
               return 1 ;
         }
         break ;

      case obj4type_total:
         rc = 0 ;
         calc_on = (EXPR4CALC *)l4first( &cb->calcList ) ;
         while( calc_on )
         {
            if( calc_on == ((PTOTAL4)obj->data)->calcPtr )
            {
               rc = 1 ;
               calc_on = NULL ;
            }
            else
               calc_on = (EXPR4CALC *)l4next( &cb->calcList, calc_on ) ;
         }
         if( rc == 0 )
         {
            if( delflag )
            {
               obj->data = NULL ;
               obj4free( obj ) ;
            }
            else
               return 1 ;
         }
         break ;

      case obj4type_calc:
         rc = 0 ;
         calc_on = (EXPR4CALC *)l4first( &cb->calcList ) ;
         while( calc_on )
         {
            if( calc_on == (EXPR4CALC *)obj->data )
            {
               rc = 1 ;
               calc_on = NULL ;
            }
            else
               calc_on = (EXPR4CALC *)l4next( &cb->calcList, calc_on ) ;
         }
         if( rc == 0 )
         {
            if( delflag )
               obj4delete( obj ) ;
            else
               return 1 ;
         }
         break ;
   }

   return 0 ;
}


/* USED to delete a calc/total from the report */
int S4FUNCTION report4deleteCalc( PREPORT4 report, EXPR4CALC S4PTR *del_calc )
{
   POBJ4 obj_on, obj_next ;
   EXPR4 *expr ;
   LIST4 temp_calcList ;
   CODE4 *codeBase ;
   PGROUP4 group_on ;
   PAREA4  area_on ;
   EXPR4CALC *calc_on, *calc_next ;
   int   calc_transfered = 1, errExpr, rc = 0 ;

   if( report == NULL || del_calc == NULL )
   {

      return -1 ;
   }

   codeBase = report->codeBase ;

   /* separate the calculations so that a temporary list contains all
      calculations dependant on the calculation to be deleted */

   memcpy( &temp_calcList, &codeBase->calcList, sizeof(temp_calcList) ) ;
   memset( &codeBase->calcList, 0, sizeof(codeBase->calcList) ) ;

   if( l4seek( &temp_calcList, del_calc ) != 0 )
      l4remove( &temp_calcList, del_calc ) ;

   errExpr = codeBase->errExpr ;

   codeBase->errExpr = 0 ;

   while( calc_transfered )
   {
      calc_transfered = 0 ;
      calc_on = (EXPR4CALC *)l4first( &temp_calcList ) ;
      while( calc_on )
      {
         calc_next = (EXPR4CALC *)l4next( &temp_calcList, calc_on ) ;

         expr = expr4parse( calc_on->expr->data, expr4source(calc_on->expr) ) ;
         if( expr )
         {
            expr4free( expr ) ;
            l4remove( &temp_calcList, calc_on ) ;
            l4add( &codeBase->calcList, calc_on ) ;
            calc_transfered = 1 ;
         }

         calc_on = calc_next ;
      }
   }

   l4add( &codeBase->calcList, del_calc ) ;
   expr4calcDelete( codeBase, del_calc ) ;

   calc_on = (EXPR4CALC *)l4pop( &temp_calcList ) ;
   while( calc_on )
   {
      l4add( &codeBase->calcList, calc_on ) ;
      expr4calcDelete( codeBase, calc_on ) ;
      calc_on = (EXPR4CALC *)l4pop( &temp_calcList ) ;
   }


   /* Now check on all the objects in the report */
   /* start with the page header and footer */
   group_on = report->page_header_footer ;
   if( group_on )
   {
      area_on = (PAREA4)l4first( &group_on->header_areas ) ;
      while( area_on )
      {
         obj_on = (POBJ4)l4first( &area_on->objects ) ;
         while( obj_on )
         {
            obj_next = (POBJ4)l4next( &area_on->objects, obj_on ) ;
            rc = report4checkObjExpr( obj_on, 1 ) ;
            if( rc == 1 )
               obj_on = NULL ;
            else
               obj_on = obj_next ;
         }
         if( rc == 1 )
            area_on = NULL ;
         else
            area_on = (PAREA4)l4next( &group_on->header_areas, area_on ) ;
      }

      area_on = (PAREA4)l4first( &group_on->footer_areas ) ;
      while( area_on )
      {
         obj_on = (POBJ4)l4first( &area_on->objects ) ;
         while( obj_on )
         {
            obj_next = (POBJ4)l4next( &area_on->objects, obj_on ) ;
            rc = report4checkObjExpr( obj_on, 1 ) ;
            if( rc == 1 )
               obj_on = NULL ;
            else
               obj_on = obj_next ;
         }
         if( rc == 1 )
            area_on = NULL ;
         else
            area_on = (PAREA4)l4next( &group_on->footer_areas, area_on ) ;
      }
   }

   group_on = (PGROUP4)l4first( &report->groups ) ;
   if( rc == 1 )
      group_on = NULL ;
   while( group_on )
   {
      area_on = (PAREA4)l4first( &group_on->header_areas ) ;
      while( area_on )
      {
         obj_on = (POBJ4)l4first( &area_on->objects ) ;
         while( obj_on )
         {
            obj_next = (POBJ4)l4next( &area_on->objects, obj_on ) ;
            rc = report4checkObjExpr( obj_on, 1 ) ;
            if( rc == 1 )
               obj_on = NULL ;
            else
               obj_on = obj_next ;
         }
         if( rc == 1 )
            area_on = NULL ;
         else
            area_on = (PAREA4)l4next( &group_on->header_areas, area_on ) ;
      }

      area_on = (PAREA4)l4first( &group_on->footer_areas ) ;
      while( area_on )
      {
         obj_on = (POBJ4)l4first( &area_on->objects ) ;
         while( obj_on )
         {
            obj_next = (POBJ4)l4next( &area_on->objects, obj_on ) ;
            rc = report4checkObjExpr( obj_on, 1 ) ;
            if( rc == 1 )
               obj_on = NULL ;
            else
               obj_on = obj_next ;
         }
         if( rc == 1 )
            area_on = NULL ;
         else
            area_on = (PAREA4)l4next( &group_on->footer_areas, area_on ) ;
      }

      if( rc == 1 )
         group_on = NULL ;
      else
         group_on = (PGROUP4)l4next( &report->groups, group_on ) ;
   }



   report->codeBase->errExpr = errExpr ;

   return 0 ;

}


/* SEE THE CODEREPORTER MANUAL */
int S4FUNCTION total4addCondition( PTOTAL4 total, char *addCondition_src, int logcon )
{
   EXPR4 *expr ;

   if( !total || !addCondition_src || logcon < 0 )
      return -1 ;

   if( total->report == NULL )
      return -1 ;

   expr = expr4parse( total->report->relate->data, addCondition_src ) ;
   if( expr )
   {
      if( logcon && expr4type(expr) != r4log )
         return -1 ;

      if( total->addCondition )
         expr4free( total->addCondition ) ;
      total->addCondition = expr ;
      if( total->lastAddValue )
      {
         u4free( total->lastAddValue ) ;
         total->lastAddValue = NULL ;
      }
      total->logcon = logcon ;
      return 0 ;
   }
   else
      error4set( total->report->codeBase, 0 ) ;

   return -1 ;
}
#endif   /* S4OFF_REPORT */
