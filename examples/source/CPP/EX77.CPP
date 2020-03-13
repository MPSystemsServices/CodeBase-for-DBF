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

#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 today ;
   today.today( ) ;
   Date4 tomorrow = today + 1L ;

   if( Date4(today++) == tomorrow )
   {
      cout << "This will never happen, since today" 
           << " is incremented after" << endl ;
   }
   if( today == tomorrow)
      cout << "This will always happen" << endl ;

   if( Date4(++today) == tomorrow )
   {
      cout << "This will never happen, since today"
           << " is incremented first" << endl ;
   }
}
