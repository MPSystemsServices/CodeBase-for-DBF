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

//ex166.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( void )
{
   Str4large str ;
   /* The result in str will be "C B A" */
   str.encode( "ABC", "3 2 1", "123" ) ;
   cout << str.str( ) << endl ;

   /* The result in str will be "A&B&C" */
   str.encode( "ABC", "1&2&3", "123" ) ;
   cout << str.str( ) << endl ;

   /* The result in str will be "19901230" */
   str.encode( "30-12/1990", "789A4512", "123456789A" ) ;
   cout << str.str( ) << endl ;

   /* The result in str will be "12/30/90" */
   str.encode( "19901230", "EF/GH/CD", "ABCDEFGH" ) ;
   cout << str.str( ) << endl ;
}
