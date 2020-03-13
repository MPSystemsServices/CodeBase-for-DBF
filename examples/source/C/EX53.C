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

void main( void )
{
   CODE4 cb ;
   DATA4 *data, *extra ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;

   data = d4open( &cb, "INFO" ) ;

   /* Open the file exclusively, default optimization is the same as if
         d4optimize( data, OPT4EXCLUSIVE ) were called.*/
   /* open a shared file. */
   cb.accessMode = OPEN4DENY_NONE ;
   extra = d4open( &cb, "DATA" ) ;

   d4optimize( extra, OPT4ALL ) ;  /* read optimize the extra "DATA" file */

   code4optStart( &cb ) ;  /* Begin the memory optimizations.*/

   /* .... Some other code .... */
   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
