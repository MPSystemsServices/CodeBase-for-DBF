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

//ex75.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Field4 bdate( data, "BIRTH_DATE" ) ;

   data.top( ) ;
   Date4 dt1( bdate.ptr( ) ) ;
   Date4 dt2( "JANUARY 12, 1990", "MMMMMMM DD, CCYY" ) ;

   if( dt1 > dt2 )
   {
      cout << "First record is after " << dt2.format( "MMM DD, CCYY" ) << endl;
      // displays 'First record is after JAN 12, 1990'
      if( dt1 == bdate )
         cout << "This is always true." << endl ;
   }
   cb.initUndo( ) ;
}
