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

//ex151.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main()
{
   Code4 cb ;

   Str4flex wideString( cb ) ;
   wideString.setLen( 2000 ) ;
   wideString.set( 'S' ) ;

   // Fill up the entire screen
   cout << wideString.ptr() << endl ;

   Str4large s1 ;
   s1.assign( "Test String" ) ;

   Str4ptr test( "Test String" ) ;
   if ( s1 == test )
      cout << "This is always True !" << endl ;

   cb.initUndo( ) ;
}
