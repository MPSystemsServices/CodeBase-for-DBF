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

//ex153.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4len str("123.5", 5 ) ;
   double d1 = (double) str  ;            // d1 becomes 123.5
   double d2 = (double) Str4ptr( "14.7"); // d2 becomes 14.7
   str.assign( "15" ) ;
   double d3 = (double) str  ;            // d3 becomes 15.0
   str.assign( "21a.4") ;
   double d4 = (double) str ;             // d4 becomes 21.0
   cout << d1 << endl ;
   cout << d2 << endl ;
   cout << d3 << endl ;
   cout << d4 << endl ;
}
