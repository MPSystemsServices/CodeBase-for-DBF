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

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main ()
{
   CODE4 code ;
   DATA4 *d ;
   FIELD4 *dateField ;

   char today[8];
   int oldLockAttempts, oldOpt, oldOptWrite ;
   code4init( &code ) ;
   oldLockAttempts = code.lockAttempts ;

   oldOpt = code.optimize;
   oldOptWrite = code.optimizeWrite ;

   code.lockAttempts = WAIT4EVER ;
   code.optimize = OPT4ALL ;
   code.optimizeWrite = OPT4ALL ;

   d = d4open( &code, "DATEFILE" ) ;
   if( code.errorCode < 0 )
      code4exit( &code ) ;

   d4lockAll( d ) ; /* lock the file for optimizations to take place*/

   dateField = d4field( d, "DATE" ) ;
   date4today( today );

   code4optStart( &code ) ;

   for( d4top( d ) ; ! d4eof( d ) ; d4skip( d, 1L ) )
      f4assign( dateField, today ) ;

   code4optSuspend( &code ) ;

   d4close( d ) ;

   code.lockAttempts = oldLockAttempts ;
   code.optimize = oldOpt ;
   code.optimizeWrite = oldOptWrite ;

   code4initUndo( &code );
}
