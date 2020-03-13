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

long recallAll( DATA4 *d, int lockTries )
{
   TAG4 *saveSelected ;
   long count ;

   saveSelected = d4tagSelected( d ) ;
   d4tagSelect( d, NULL ) ; /* use record ordering */

   d4lockAll( d ) ;
   count = 0 ;

   for( d4top( d ) ;  !d4eof( d ) ; d4skip( d, 1 ) )
   {
       d4recall( d ) ;
       count++ ;
   }

   d4tagSelect( d, saveSelected ) ; /* reset the selected tag.*/
   return count ;
}

void main()
{
   CODE4 cb ;
   DATA4 *data ;
   int lockTries = WAIT4EVER ;
   long count  ;

   code4init( &cb ) ;
   data = d4open( &cb , "INFO" ) ;
   count = recallAll( data, lockTries ) ;
   printf( "record count is %d \n", count ) ;
   code4initUndo( &cb ) ;
}
