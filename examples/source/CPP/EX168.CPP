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

//ex168.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   Str4ptr alphabet( "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ) ;
   Str4ten  firstTen, middleTen, lastTen ;

   // 'firstTen' will contain "ABCDEFGHIJ"
   firstTen.assign( alphabet.left( 10 ) ) ;
   cout << firstTen.str( ) << endl ;

   // 'middleTen' will contain "IJKLMNOPQR"
   middleTen.assign( alphabet.substr(8,10) ) ;
   cout << middleTen.str( ) << endl ;

   // 'lastTen' will contain "QRSTUVWXYZ"
   lastTen.assign( alphabet.right( 10 ) ) ;
   cout << lastTen.str( ) << endl ;
}
