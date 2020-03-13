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

/* s4initfr.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4CLIENT
void relate4freeBitmaps( RELATE4 *relate )
{
   RELATE4 *relateOn ;

   #ifdef E4PARM_LOW
      if ( relate == 0 )
         error4( 0, e4parm_null, E91909 ) ;
   #endif

   for( relateOn = 0 ;; )
   {
      relateOn = (RELATE4 *)l4next( &relate->slaves, relateOn ) ;
      if ( relateOn == 0 )
         break ;
      relate4freeBitmaps( relateOn ) ;
   }

   u4free( relate->set.flags ) ;
   c4memset( (void *)&relate->set, 0, sizeof ( F4FLAG ) ) ;
}
#endif

/* frees up any extra memory that is not neccessarily required for CodeBase */
/* returns 0 if successfully freed info, else 0 */
static int sort4freeExtra( RELATE4 *relate, CODE4 *codeBase )
{
   #ifdef E4PARM_LOW
      if ( codeBase == 0 )
         error4( 0, e4parm_null, E91910 ) ;
   #endif

   #ifndef S4OPTIMIZE_OFF
      if ( codeBase->hasOpt && codeBase->opt.numBuffers )
      {
         code4optSuspend( codeBase ) ;
         codeBase->hadOpt = 0 ;  /* re-enable auto-start of memory optimization */
         error4set( codeBase, 0 ) ;
         return 0 ;
      }
   #endif

   #ifndef S4CLIENT
      if ( relate != 0 )
         if ( relate->relation->bitmapsFreed == 0 )
         {
            relate4freeBitmaps( relate ) ;
            relate->relation->bitmapsFreed = 1 ;
            return 0 ;
         }
   #endif

   return 1 ;
}

int S4FUNCTION sort4getInitFree( SORT4 *s4, RELATE4 *relate )
{
   CODE4 *codeBase ;
   int rc, oldPoolN, prev ;
   unsigned int oldPoolEntries, oldTotLen ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 )
         error4( 0, e4parm_null, E91911 ) ;
   #endif

   codeBase = s4->codeBase ;
   if ( error4code( codeBase ) < 0 )
      return e4codeBase ;

   oldPoolN = s4->poolN ;
   oldPoolEntries = s4->poolEntries ;
   oldTotLen = s4->totLen ;

   prev = 0 ;

   if ( s4->spoolsMax > 0 )
   {
      rc = sort4spoolsInit( s4, prev ) ;
      while( rc == e4memory )
      {
         if ( sort4freeExtra( relate, codeBase ) == 1 )
         {
            sort4free( s4 ) ;
            return error4( s4->codeBase, e4memory, E91911 ) ;
         }
         prev = 1 ;
         s4->poolEntries = oldPoolEntries ;
         s4->totLen = oldTotLen ;
         /* reset the pools */
         mem4release( s4->poolMemory ) ;
         s4->poolMemory =  mem4create( s4->codeBase, 1, (unsigned) s4->poolEntries*s4->totLen+sizeof(LINK4), 1, 1 ) ;
         for ( s4->poolN = 0 ; oldPoolN ; oldPoolN-- )
            if ( mem4allocNoZero( s4->poolMemory ) )
               s4->poolN++ ;
         rc = sort4spoolsInit( s4, prev ) ;
      }
   }
   else
   {
      sort4getMemInit( s4 ) ;
      return 0 ;
   }

   return rc ;
}

int S4FUNCTION sort4initFree( SORT4 *s4, CODE4 *c4, const int sortL, const int infoL, RELATE4 *relate )
{
   int rc ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 || c4 == 0 || sortL < 0 || infoL < 0 )
         error4( 0, e4parm_null, E91912 ) ;
   #endif

   sort4initSet( s4, c4, sortL, infoL ) ;
   rc = sort4initAlloc( s4 ) ;
   while( rc == e4memory )
   {
      if ( sort4freeExtra( relate, c4 ) == 1 )
      {
         sort4free( s4 ) ;
         return error4( s4->codeBase, e4memory, E91912 ) ;
      }
      rc = sort4initAlloc( s4 ) ;
   }
   return rc ;
}
