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

//ex155.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4len str("123.5", 5 ) ;
   long l1 = (long) str  ;             // l1 becomes 123
   long l2 = (long) Str4ptr( "14.7");  // l2 becomes  14
   str.assign( "15" ) ;
   long l3 = (long) str  ;             // l3 becomes  15
   str.assign( "21a.4") ;
   long l4 = (long) str ;              // l4 becomes  21
   long l5 = (long) Str4ptr("654321"); // l5 becomes 654321
   cout << l1 << endl ;
   cout << l2 << endl ;
   cout << l3 << endl ;
   cout << l4 << endl ;
   cout << l5 << endl ;
}
