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

/* f4flag.c  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

/* note:  This module is excluded from the CodeBase error handling
   This is because it may be convenient to call these functions regardless
   of the error state of CodeBase
*/

int S4FUNCTION f4flagInit( F4FLAG *f4, CODE4 *c4, const unsigned long nFlags, Bool5 discardOutOfRangeIn )
{
   // AS Jul 12/02 - Support for discardOutOfRange records...
   // in particular with transactions interacting with the relate module's bitmaps where the
   // # of rows in the bitmap may be less than the physical record count (and those records in
   // the tags).  In that case, we still want to discount the rows not in the set yet, so allow
   // the set to be smaller than the actual range, and just skip...
   // cushion was not being used, so removed it.
   #ifdef E4PARM_LOW
      if ( f4 == 0 || c4 == 0 )
         return error4( c4, e4parm, E90812 ) ;
   #endif

   c4memset( (void *)f4, 0, sizeof(F4FLAG) ) ;

   if ( nFlags == 0 )
      return 0 ;

   f4->codeBase = c4 ;

   f4->discardOutOfRange = discardOutOfRangeIn ;

   f4->flags = (unsigned char *)u4allocFree( c4, ( nFlags >> 3 ) + 2 ) ;

   f4->numFlags = nFlags ;

   if ( f4->flags == 0 )
      return e4memory ;
   return 0 ;
}



int S4FUNCTION f4flagReset( F4FLAG *f4, const unsigned long flagNum )
{
   unsigned char lowVal, setVal ;
   unsigned long highVal ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( f4->codeBase, e4parm, E90812 ) ;
   #endif

   if ( flagNum > f4->numFlags )
   {
      // AS Jul 12/02 - It is ok now if this happens in some cases...
      // in particular with transactions interacting with the relate module's bitmaps where the
      // # of rows in the bitmap may be less than the physical record count (and those records in
      // the tags).  In that case, we still want to discount the rows not in the set yet, so allow
      // the set to be smaller than the actual range, and just skip...
      if ( f4->discardOutOfRange == 1 )
         return 0 ;
      return error4( f4->codeBase, e4info, E90812 ) ;
   }

   #ifdef E4ANALYZE
      if ( f4->flags == 0 )
         return error4( f4->codeBase, e4info, E90812 ) ;
   #endif

   lowVal = (unsigned char) (flagNum & 0x7) ;
   highVal = flagNum >> 3 ;
   setVal = (unsigned char) (1 << lowVal) ;
   setVal = (unsigned char) ~setVal ;

   f4->flags[highVal] = (char) (setVal & f4->flags[highVal]) ;

   return 0 ;
}



int S4FUNCTION f4flagSet( F4FLAG *f4, const unsigned long flagNum )
{
   unsigned char lowVal, setVal ;
   unsigned long highVal ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( f4->codeBase, e4parm, E90812 ) ;
   #endif

   if ( flagNum > f4->numFlags )
   {
      // AS Jul 12/02 - It is ok now if this happens in some cases...
      // in particular with transactions interacting with the relate module's bitmaps where the
      // # of rows in the bitmap may be less than the physical record count (and those records in
      // the tags).  In that case, we still want to discount the rows not in the set yet, so allow
      // the set to be smaller than the actual range, and just skip...
      if ( f4->discardOutOfRange == 1 )
         return 0 ;
      return error4( f4->codeBase, e4info, E90812 ) ;
   }

   #ifdef E4ANALYZE
      if ( f4->flags == 0 )
         return error4( f4->codeBase, e4info, E90812 ) ;
   #endif

   lowVal = (unsigned char) (flagNum & 0x7) ;
   highVal = flagNum >> 3 ;
   setVal = (unsigned char) (1 << lowVal) ;

   f4->flags[highVal] = (char)(setVal | f4->flags[highVal]) ;

   return 0 ;
}



int S4FUNCTION f4flagSetRange( F4FLAG *f4, const unsigned long flagNum, const unsigned long numFlags )
{
   unsigned long iFlag ;
   int rc ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   for ( iFlag = 0; iFlag < numFlags; iFlag++ )
   {
      rc = f4flagSet( f4, flagNum + iFlag ) ;
      if ( rc < 0 )
         return rc ;
   }
   return 0 ;
}



int S4FUNCTION f4flagIsSet( F4FLAG *f4, const unsigned long flagNum )
{
   unsigned char lowVal, retVal ;
   unsigned long highVal ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   if ( flagNum > f4->numFlags )
   {
      // AS Jul 12/02 - It is ok now if this happens in some cases...
      // in particular with transactions interacting with the relate module's bitmaps where the
      // # of rows in the bitmap may be less than the physical record count (and those records in
      // the tags).  In that case, we still want to discount the rows not in the set yet, so allow
      // the set to be smaller than the actual range, and just skip...
      if ( f4->discardOutOfRange == 1 )  // return that it is not set...
         return 0 ;
      return error4( f4->codeBase, e4info, E90812 ) ;
   }

   #ifdef E4ANALYZE
      if ( f4->flags == 0 )
         return error4( f4->codeBase, e4info, E90812 ) ;
   #endif

   lowVal = (unsigned char) (flagNum & 0x7) ;
   highVal = flagNum >> 3 ;
   retVal = (unsigned char) ((1 << lowVal) & f4->flags[highVal])  ;

   return (int) retVal ;
}



int S4FUNCTION f4flagIsAllSet( F4FLAG *f4, const unsigned long flagNum, const unsigned long nFlags )
{
   int rc ;
   unsigned long iFlag ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   for ( iFlag = flagNum; iFlag <= nFlags; iFlag++ )
   {
      rc = f4flagIsSet( f4, iFlag ) ;
      if ( rc < 0 )
         return rc ;
      if ( rc == 0 )
         return 0 ;
   }
   return 1 ;
}



int S4FUNCTION f4flagIsAnySet( F4FLAG *f4, const unsigned long flagNum, const unsigned long nFlags )
{
   int rc ;
   unsigned long iFlag ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   for ( iFlag = flagNum; iFlag <= nFlags; iFlag++ )
      if ( (rc = f4flagIsSet( f4, iFlag )) < 0 )
         return rc ;
   return 0 ;
}



int S4FUNCTION f4flagAnd( F4FLAG *flagPtr, const F4FLAG *andPtr )
{
   unsigned numBytes ;

   #ifdef E4PARM_LOW
      if ( flagPtr == 0 || andPtr == 0 )
         return error4( 0, e4parm_null, E90812 ) ;
   #endif

   if ( andPtr->numFlags == 0 )
   {
      if ( flagPtr->numFlags == 0 )
         return 0 ;
      if ( flagPtr->isFlip != andPtr->isFlip )
         c4memset( (void *)flagPtr->flags, 1, (unsigned)((flagPtr->numFlags) / 8L + 2L ) ) ;
      else
         c4memset( (void *)flagPtr->flags, 0, (unsigned)((flagPtr->numFlags) / 8L + 2L ) ) ;
      return 0 ;
   }

   #ifdef E4ANALYZE
      if ( flagPtr->numFlags != andPtr->numFlags )
         return error4( flagPtr->codeBase, e4result, E90812 ) ;
   #endif

   numBytes = (unsigned)((flagPtr->numFlags+7)/8) ;

   #ifdef E4ANALYZE
      if ( flagPtr->numFlags != andPtr->numFlags )
         return error4( flagPtr->codeBase, e4result, E90812 ) ;
      if ( (unsigned long)numBytes != ( flagPtr->numFlags + 7L) / 8L )
         return error4( flagPtr->codeBase, e4info, E90812 ) ;
   #endif

   if ( flagPtr->isFlip != andPtr->isFlip )
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
            flagPtr->flags[numBytes] = (unsigned char) (~flagPtr->flags[numBytes] & andPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] &= ~andPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }
   else
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
            flagPtr->flags[numBytes] = (unsigned char) (~flagPtr->flags[numBytes] & ~andPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] &= andPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }

   return 0 ;
}



int S4FUNCTION f4flagOr( F4FLAG *flagPtr, const F4FLAG *orPtr )
{
   unsigned numBytes ;

   #ifdef E4PARM_LOW
      if ( flagPtr == 0 || orPtr == 0 )
         return error4( 0, e4parm_null, E90812 ) ;
   #endif

   if ( orPtr->numFlags == 0 )
      return 0 ;

   numBytes = (unsigned)(flagPtr->numFlags / 8L + 1L ) ;

   #ifdef E4ANALYZE
      if ( flagPtr->numFlags != orPtr->numFlags )
         return error4( flagPtr->codeBase, e4result, E90812 ) ;
      if ( (unsigned long)numBytes != ( flagPtr->numFlags / 8L + 1L ) )
         return error4( flagPtr->codeBase, e4info, E90812 ) ;
   #endif
   if ( flagPtr->isFlip != orPtr->isFlip )
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
           flagPtr->flags[numBytes] = (unsigned char) (~flagPtr->flags[numBytes] | orPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] |= ~orPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }
   else
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
            flagPtr->flags[numBytes] = (unsigned char)(~flagPtr->flags[numBytes] | ~orPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] |= orPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }
   return 0 ;
}



void S4FUNCTION f4flagFlipReturns( F4FLAG *flagPtr )
{
   #ifdef E4PARM_LOW
      if ( flagPtr == 0 )
      {
         error4( 0, e4parm_null, E90812 ) ;
         return ;
      }
   #endif

   flagPtr->isFlip = !flagPtr->isFlip ;
}



void S4FUNCTION f4flagSetAll( F4FLAG *flagPtr )
{
   #ifdef E4PARM_LOW
      if ( flagPtr == 0 )
      {
         error4( 0, e4parm_null, E90812 ) ;
         return ;
      }
   #endif

   c4memset( (void *)flagPtr->flags, 0xFF, (unsigned)((flagPtr->numFlags+7)/8) ) ;
}



// AS Jun 24/02 - New functionality for relate4count -- count the flags...
unsigned long f4flagCount( F4FLAG *flagPtr, unsigned long startPos )
{
   // ULONG_MAX (-1 in long) is returned in case of failure, else the # of 'on' flags is returned
   // we compensate for reversed flags, and also assume that there is an extra unused flag which
   // is ignored.
   // startPos allows the count to start at a different position from the start (this is useful
   // because for the bitmaps, we flag the record number, so the 0th position is ignored)
   #ifdef E4PARM_LOW
      if ( flagPtr == 0 )
      {
         error4( 0, e4parm_null, E90812 ) ;
         return ULONG_MAX ;
      }
   #endif

   // first count the flags...
   unsigned long count = 0 ;

   for ( unsigned long loop = startPos ; loop < flagPtr->numFlags ; loop++ )
   {
      if ( f4flagIsSet( flagPtr, loop ) )
         count++ ;
   }

   if ( flagPtr->isFlip ) // flip the results (the count is reversed)
      return (flagPtr->numFlags - startPos) - count ;

   return count ;
}
