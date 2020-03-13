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

/*ex123.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *employee, *office, *building ;
   TAG4 *officeNo, *buildNo ;
   RELATE4 *master, *toOffice, *toBuilding ;

   code4init( &cb ) ;
   employee = d4open( &cb, "EMPLOYEE" );
   office = d4open( &cb, "OFFICE" ) ;
   building = d4open( &cb, "BUILDING" ) ;

   /*set up the tags */
   officeNo = d4tag( office, "OFF_NUM" ) ;
   buildNo = d4tag( building, "BUILD_NO" ) ;

   /* Create the relations */
   master = relate4init( employee ) ;
   toOffice = relate4createSlave(master,office, "EMPLOYEE->OFFICE_NO", officeNo );
   toBuilding = relate4createSlave( toOffice, building, "OFFICE->BUILD_NO", buildNo ) ;
   /* Go to employee, at record 2*/
   d4go( employee, 2L ) ;

   /* Lock the data files and their index files.*/
   relate4lockAdd( master ) ;
   code4lock( &cb ) ;

   /* This call causes the corresponding records in data files "OFFICE"
      and "BUILDING" to be looked up.*/
   relate4doAll( master ) ;

   /* Go to office, at record 3*/
   d4go( office, 3L ) ;

   /* This call causes the building record to be positioned according to
   its master, the office data file. */
   relate4doOne( toBuilding ) ;

   /* ... and so on*/

   relate4free( master, 1 ) ;
   code4initUndo( &cb  ) ;
}
