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

/*ex106.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 testFile ;

   code4init( &cb ) ;
   cb.safety = 0 ;
   file4create( &testFile, &cb, "TEMP.FIL", 0 ) ;
   code4optStart( &cb ) ;
   file4write( &testFile, 0, "Is this information written?", 27 ) ;
    /* Written to memory, not disk*/

   file4flush( &testFile ) ; /* Physically write to disk*/

   printf( "Flushing complete.\n") ;
   printf( "Check TEMP.FIL after you power off the computer.\n") ;

   code4initUndo( &cb ) ;
}

