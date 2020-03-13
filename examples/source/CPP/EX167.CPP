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

//ex167.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   char buf[300] ;
   Str4max str( buf, sizeof(buf) ) ;

   str.assign( "Information" ) ;
   str.insert( Str4ptr("My "), 0 ) ;
   str.insert( Str4ptr("Insert "), 3 ) ;

   // 'str' now contains "My Insert Information"
   cout << str.str( ) << endl ;
}
