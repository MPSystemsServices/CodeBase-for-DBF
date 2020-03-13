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

/*ex71.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info ;
   TAG4 *defaultTag ;
   long count = 0 ;

   code4init( &cb ) ;
   info = d4open( &cb, "INFO" ) ; /* automatically open data & index file*/
   defaultTag = d4tagDefault( info ) ;
   d4tagSelect( info, defaultTag ) ; /* Select default tag*/

   for( d4top( info ) ; ! d4eof( info ) ; d4skip( info, 1 ) )
      count ++ ;

   printf( "%d records in tag ", count ) ;
   printf( "%s\n", t4alias( defaultTag ) );

   d4close( info ) ;
   code4initUndo( &cb ) ;
}
