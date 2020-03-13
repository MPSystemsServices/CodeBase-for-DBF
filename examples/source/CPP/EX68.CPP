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

//ex68.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 info( settings, "INFO" ) ; // automatically open data & index file
   Tag4 firstTag ;
   firstTag.initFirst( info ) ;

   info.select( firstTag ) ; // Select first tag of the first open index

   long count = 0L ;
   for( info.top( ) ;! info.eof( ) ; info.skip( ) )
      count++ ;

   cout << count << " records in tag " ;
   cout << firstTag.alias( ) << endl ;

   info.close( ) ;
   settings.initUndo( ) ;
}
