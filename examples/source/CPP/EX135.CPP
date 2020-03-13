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

//ex135.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;

   // automatically open the INFO production index file
   Data4 data( cb, "INFO" ) ;

   // Copy the Index4 object for the production index file
   Index4 info = data.index( "INFO" ) ;

   cb.errOpen = 0 ;
   Index4 names( data, "NOT" ) ; // attempt a second index file
   if( !names.isValid( ))
   {
      cout << "NOT index file not opened" << endl ;
      cout << (names.index == NULL ? "NULL": "NOT NULL") << endl ;
   }

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
