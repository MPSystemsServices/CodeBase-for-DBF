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

//ex76.cpp
#include "d4all.hpp"

void main( )
{
   long yesterday ;
   Date4 today ;
   today.today( ) ; // Get the current date from the system clock

   yesterday = today - 1L ;

   Date4 tomorrow = yesterday + 2L ; // Date4::Date4( long ) called.

   cout << "Today is " << today.format( "MMM DD, CCYY" ) << endl ;
   cout << "The julian date for yesterday is " << yesterday << endl ;
   cout << "The julian date for tomorrow is " << (long) tomorrow << endl ;
}
