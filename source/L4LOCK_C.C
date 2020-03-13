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

/* l4lock_c.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4LOCK_CHECK

// AS Jan 27/10 - support for lock checking in multi-thread scenarios...moved to d4data.h and put on a CODE4 level

/* Saves information about all current locks  */
/*
typedef struct
{
   LINK4  link ;
   int     next ;
   int     prev ;
   int     hand ;
   long    startPos ;
   long    len ;
   long    endPos ;
} L4LOCK_INFO ;

static LIST4 lockList ;
static int first =  1 ;
static MEM4 *lockAlloc ;
*/

int l4lock_check( CODE4 *c4 )
{
   return c4->lockList.nLink ;
}

/* Returns true or false; true means found */
static L4LOCK_INFO *l4lockFind( CODE4 *c4, HANDLE h, long p, long l )
{
   L4LOCK_INFO *lPtr ;

   if ( c4->lockAlloc == 0 )
   {
      memset( &c4->lockList, 0, sizeof( LIST4 ) ) ;
      c4->lockAlloc =  mem4create( c4, 20, sizeof(L4LOCK_INFO), 20, 0 ) ;
   }

   for ( lPtr = 0; lPtr = (L4LOCK_INFO *) l4next(&c4->lockList, lPtr); )
   {
      if ( lPtr->hand == h  && lPtr->startPos == p && lPtr->len == l )
         return lPtr ;
      if ( lPtr->hand == h )
      {
         /* Check for Overlap. */
         if ( lPtr->startPos >= p && lPtr->startPos <= p+l-1  ||
              lPtr->endPos >= p   && lPtr->endPos   <= p+l-1  ||
              p >= lPtr->startPos && p <= lPtr->endPos        ||
              p+l-1 >= lPtr->startPos && p+l-1 <= lPtr->endPos )
              {
                 error4( c4, e4result, E86101 ) ;
                 return 0 ; ;
              }
      }
   }
   return 0 ;
}



void l4lockRemove( CODE4 *c4, HANDLE h, const char *fName, long p, long l )
{
   L4LOCK_INFO *lPtr ;

   lPtr =  l4lockFind( c4, h,p,l ) ;
   if ( lPtr == 0 )
   {
      char errBuf[120] ;
      sprintf( errBuf, "lock not found: at location: %ld, length: %ld", p, l ) ;
      error4describe( c4, e4result, E86102, errBuf, fName, 0 ) ;
   }
   else
   {
      l4remove( &c4->lockList, lPtr ) ;
      mem4free( c4->lockAlloc, lPtr ) ;
   }
}



void l4lockSave( CODE4 *c4, HANDLE h, long p, long l )
{
   L4LOCK_INFO *lPtr ;

   if ( l4lockFind(c4, h,p,l) != 0 )
   {
      error4( c4, e4result, E86101 ) ;
      return ;
   }

   lPtr =  (L4LOCK_INFO *) mem4allocZero( c4->lockAlloc ) ;
   if ( lPtr == 0 )
   {
      error4( c4, e4memory, E96102 ) ;
      return ;
   }
   l4add( &c4->lockList, lPtr ) ;

   lPtr->hand = h ;
   lPtr->startPos = p ;
   lPtr->len  = l ;
   lPtr->endPos =  p+l-1 ;
}

#endif
