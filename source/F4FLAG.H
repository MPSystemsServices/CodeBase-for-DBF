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

/* f4flag.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

typedef struct
{
   CODE4 S4PTR *codeBase ;
   unsigned char S4PTR *flags ;
   unsigned long  numFlags ;
   Bool5    isFlip ;
   // AS Jul 12/02 - It is ok now if this happens in some cases...
   // in particular with transactions interacting with the relate module's bitmaps where the
   // # of rows in the bitmap may be less than the physical record count (and those records in
   // the tags).  In that case, we still want to discount the rows not in the set yet, so allow
   // the set to be smaller than the actual range, and just skip...
   Bool5 discardOutOfRange ;
//   Bool5    special ;    /* special flag, used for flag lock module */
} F4FLAG ;

#ifdef __cplusplus
   extern "C" {
#endif

S4EXPORT int  S4FUNCTION f4flagInit( F4FLAG S4PTR *, CODE4 S4PTR *, const unsigned long, Bool5 ) ;
S4EXPORT int  S4FUNCTION f4flagSet( F4FLAG S4PTR *, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagReset( F4FLAG S4PTR *, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagIsSet( F4FLAG S4PTR *, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagIsAllSet( F4FLAG S4PTR *, const unsigned long, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagIsAnySet( F4FLAG S4PTR *, const unsigned long, const unsigned long ) ;
S4EXPORT void S4FUNCTION f4flagSetAll( F4FLAG S4PTR * ) ;
S4EXPORT int  S4FUNCTION f4flagSetRange( F4FLAG S4PTR *, const unsigned long, const unsigned long ) ;

// AS Jun 24/02 - New functionality for relate4count -- count the flags...
unsigned long f4flagCount( F4FLAG *, unsigned long ) ;

/* For Report Module */
S4EXPORT int  S4FUNCTION f4flagOr( F4FLAG S4PTR *, const F4FLAG S4PTR * ) ;
S4EXPORT int  S4FUNCTION f4flagAnd( F4FLAG S4PTR *, const F4FLAG S4PTR * ) ;
S4EXPORT void S4FUNCTION f4flagFlipReturns( F4FLAG S4PTR * ) ;

/* For Lock Module */
//#define flag4lockInit( f4, c4, nFlags ) f4flagInit( (f4), (c4), (nFlags), 1 )
//#define flag4lockLock( f4, n ) ( ( f4flagSet( (f4), (n) ) ) )
//#define flag4lockUnlock( f4, n ) ( f4flagReset( (f4), (n) ) )
//#define flag4lockEnabled( f4 ) ( (f4)->flags )
//#define flag4lockIsLocked( f4, n ) ( f4flagIsSet( (f4), (n) )
// #define flag4lockIsLockedFile( f4, n ) ( (f4)->isFlip )
// #define flag4lockIsLockedAppend( f4, n ) ( (f4)->special )
// #define flag4lockLockFile( f4, n ) ( (f4)->isFlip = 1, (f4)->special = 1 )
// #define flag4lockUnlockFile( f4, n ) ( (f4)->isFlip = 0, (f4)->special = 0 )
// #define flag4lockLockAppend( f4, n ) ( (f4)->special = 1 )
// #define flag4lockUnlockAppend( f4, n ) ( (f4)->special = 0 )

#ifdef __cplusplus
   }
#endif

