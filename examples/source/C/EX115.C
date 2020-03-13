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

/*ex115.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#define DATE_OFFSET 16

void main( void )
{
   CODE4 cb ;
   FILE4 file ;
   char runDate[9] ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;

   date4today( runDate ) ; /* initialize to the system clock*/
   runDate[8] = 0 ;
   if( file4open( &file, &cb, "TEXT.FIL", 0 ) !=0 )
      return ;

   /* file is opened exclusively - no need to lock*/
   file4write( &file, DATE_OFFSET, runDate, sizeof(runDate) ) ;
   file4close( &file ) ;
   code4initUndo( &cb );
}
