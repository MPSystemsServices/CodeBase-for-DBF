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

//ex78.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main( )
{
   Date4 independence = "17760704" ;
   Date4 dayAfter = independence + 1L ;
   Date4 dayBefore = independence - 1L ;

   cout << "Independence Day originally was on a "
        << independence.cdow( ) << endl ;

   cout << "The day after was "
        << dayAfter.format( "MMMM DD, 'YY" ) << endl ;

   cout << "The day before was "
        << dayBefore.format( "MMMM DD, 'YY" ) << endl ;
}
