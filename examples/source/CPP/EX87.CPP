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

//ex87.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

static int daysInMonth[] =
{ 0,31,28,31,30,31,30,31,31,30,31,30,31 } ;

void makeEndOfMonth( Date4 &d )
{
   int endOfMonth = daysInMonth[d.month( )] ;
   if( d.month( ) == 2 && d.isLeap( ) )
      endOfMonth++ ;

   d+= (endOfMonth - d.day( )) ;
}

void main( )
{
   Date4 today ;
   today.today( ) ;
   cout << today.str( ) << endl ;
   makeEndOfMonth( today ) ;
   cout << today.str( ) << endl ;
}
