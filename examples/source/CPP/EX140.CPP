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

// ex140.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 enroll( cb, "ENROLL" ) ;
   Data4 master( cb, "CLASSES" ) ;
   Data4 student( cb, "STUDENT" ) ;

   Tag4 enrollTag( enroll, "ENR_CODE" ) ;
   Tag4 studentTag( student, "STU_ID" ) ;

   Relate4set MasterRelation( master ) ;
   Relate4 relation1( MasterRelation, enroll, "CODE", enrollTag ) ;
   Relate4 relation2( relation1, student, "STU_ID_TAG", studentTag ) ;

   relation1.type( relate4scan ) ;
   MasterRelation.sortSet( "LEFT(STUDENT->L_NAME,8)+ENROLL->CODE" ) ;

   Field4 classCode( master, "CODE" ) ;
   Field4 classTitle( master, "TITLE" ) ;
   Field4 enrollStudentId( enroll, "STU_ID_TAG") ;
   Field4 studentName( student, "L_NAME" ) ;

   cb.exitTest( ) ;

   for(int rc = MasterRelation.top( ); rc != r4eof
   ; rc = MasterRelation.skip( ) )
   {
      cout << studentName.str( ) << " " ; // only one Str4::str per stmt.
      cout << enrollStudentId.str( ) << " " ;
      cout << classCode.str( ) << " " ;
      cout << classTitle.str( ) << endl ;
   }

   cout << "Number of records in " << master.alias( ) << " "
        << master.recCount( ) << endl ;

   MasterRelation.free( ) ;
   cb.initUndo( ) ;
}
