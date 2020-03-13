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

//ex98.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000;

void main( )
{
   Code4  cb ;
   Data4  info( cb, "INFO" ) ;
   Field4 birthDate(info, "BIRTH_DATE"), amount(info, "AGE" ) ;
   Date4  today, bDate ;
   long   ageInDays, currentValue ;

   cb.exitTest( ) ;
   info.top( ) ;
   info.go( 1L ) ;

   today.today( ) ;
   bDate.assign(birthDate.str());
   ageInDays =  long( today ) - long( bDate ) ;

   cout << "Age in days:  " << ageInDays << endl;

   /* Assume field "AGE" is of type Numeric or
                                    Floating Point */
   currentValue =  long( amount ) ;

   cout << "Current value is:  " << currentValue << endl;

   cb.closeAll( );
   cb.initUndo( );
}
