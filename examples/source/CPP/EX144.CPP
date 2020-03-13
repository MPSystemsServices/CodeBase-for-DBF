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

//ex144.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main()
{
   Code4 cb ;

   Data4  employee( cb, "EMPLOYEE" ) ;
   Data4  office( cb, "OFFICE" ) ;
   Data4  building( cb, "BUILDING" ) ;

   // Set up the tags.
   Tag4 officeNo( office, "OFF_NUM" ) ;
   Tag4 buildNo( building, "BUILD_NO" ) ;

   // Create the relations
   Relate4set master( employee ) ;

   Relate4 toOffice( master, office, "EMPLOYEE->OFFICE_NO", officeNo ) ;
   Relate4 toBuilding( toOffice, building, "OFFICE->BUILD_NO", buildNo ) ;

   // Go to employee, at record 2
   employee.go( 2L ) ;

   // Lock the data files and their index files.
   master.lockAdd( ) ;
   cb.lock( ) ;

   // This call causes the corresponding records in data files "OFFICE" and
   // "BUILDING" to be looked up.
   master.doAll( ) ;

   // Go to office, at record 3
   office.go( 3L ) ;

   // This call causes the building record to be looked up from the office
   toBuilding.doOne( ) ;

   //  ..  and so on

   cb.initUndo( ) ;
}
