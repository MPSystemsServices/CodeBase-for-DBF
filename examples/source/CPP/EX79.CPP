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

//ex79.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

class myClass : public Date4
{
public:
   myClass( char *value, char *pict = NULL )
      { assign( value, (pict) ? pict:"CCYYMMDD" ) ; }
   void addWeek( long toAdd = 1 ) ;
} ;

void myClass::addWeek( long toAdd )
{
   *this += toAdd * 7L ;
}

void main( )
{
   myClass date( "Nov 11, 1995", "MMM DD, CCYY" ) ;
   cout << date.str( ) << " is the initial date" << endl ;
   date.addWeek( 2L ) ; // add 2 weeks to the date
   cout << date.str( ) << " is 2 weeks later" << endl ;
}
