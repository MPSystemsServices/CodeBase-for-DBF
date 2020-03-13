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

//ex114.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void displayTheRecord( Data4 d )
{
   int numFields = d.numFields( ), curField = 1 ;
   Field4memo genericField ;

   for( ; curField <= numFields; curField++ )
   {
      genericField.init( d, curField ) ;
      cout << genericField.str( ) << "\t" ;
   }
   cout << endl ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "DATA2" ) ;
   data.top( ) ;
   displayTheRecord( data ) ;
   cb.initUndo( ) ;
}
