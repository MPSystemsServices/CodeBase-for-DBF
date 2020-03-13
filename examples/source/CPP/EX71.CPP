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

//ex71.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

long zapLast( Data4 info, long toDelete )
{
   cout << info.alias( ) << " has " << info.recCount( ) << " records." << endl ;

   // Remove the last 'toDelete' records in the data file.
   // endRec parameter defaults to 1 Billion

   info.zap( info.recCount( ) - toDelete + 1L) ;

   cout << endl << info.alias( ) << " now has " << info.recCount( )
        << " records." << endl ;

   return info.recCount( ) ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   zapLast( data, 10L ) ;
   cb.initUndo( ) ;
}
