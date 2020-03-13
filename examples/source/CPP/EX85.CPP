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

//ex85.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 dt( "19901002" ) ;
   char  result[20] ;

   dt.format( result, "YY.MM.DD" ) ;   // 'result' will contain "90.10.02"
    cout << result << endl ;
   dt.format( result, "CCYY.MM.DD" ) ; // 'result' will contain "1990.10.02"
    cout << result << endl ;
   dt.format( result, "MM/DD/YY" ) ;   // 'result' will contain "10/02/90"
    cout << result << endl ;
   dt.format( result, "MMM DD/CCYY" ) ;// 'result' will contain "Oct 02/1990"
    cout << result << endl ;

   cout << dt.format( "MMM DD/CCYY" ) << endl ; // outputs 'Oct 02/1990'
}
