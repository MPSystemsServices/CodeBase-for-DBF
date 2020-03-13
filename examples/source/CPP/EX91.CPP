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

//ex91.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Field4 bdate( data, "BIRTH_DATE") ;
   data.top( ) ;

   Date4 date1( bdate.ptr( ) ) ; // make a copy of the field's contents
   data.skip( ) ;
   Date4 date2( bdate.ptr( ) ) ;// make a copy of the field's contents

   if( date1.year( ) == date2.year( ) )
     cout << "The people in the first and second records were both born in "
          << date1.year( ) << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
