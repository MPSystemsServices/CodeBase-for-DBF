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

//ex157.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Str4ptr strBob1( "Bob" ) ;
   Str4ptr strBob2( "Bob" ) ;
   Str4ptr strBobby("Bobby" ) ;
   Str4ptr strabcdef( "abcdef" ) ;
   Str4ptr strabb("abb" ) ;

   // The result of the comparison is placed in r1, r2 and r3
   int r1 = strBob1 < strBobby ;  // r1 = 1
   int r2 = strBob1 < strBob2 ;   // r2 = 0
   int r3 = strabcdef < strabb ;  // r3 = 0
   cout << r1 << endl ;
   cout << r2 << endl ;
   cout << r3 << endl ;
}
