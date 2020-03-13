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

//ex80.cpp
#include "d4all.hpp"

void subtractYear( Date4 &d, int numYears )
{
   cout << "The date " << numYears << " years ago is " ;
   for( ; numYears ; numYears -- )
   {
      d -= 365L ;
      if( d.isLeap( ) )
         d-- ;
   }
   cout << d.str( ) << endl ;
}

void main( )
{
   Date4 today ;
   today.today( ) ;
   subtractYear( today, 10L ) ; // subtract 10 years from today
}
