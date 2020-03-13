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

/***********************************************************************\
*                                                                       *
*   RELATE3.CPP     Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 7 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

Code4 codeBase ;
Data4 student, enrolment, classes ;
Field4 studentId, firstName, lastName, age, classCode, classTitle ;
Tag4 idTag, codeTag ;
Relate4set classRel ;
Relate4 studentRel, enrollRel ;

void printStudents( void ) ;

void openDataFiles()
{
   student.open( codeBase, "student" ) ;
   enrolment.open( codeBase, "enroll" ) ;
   classes.open( codeBase, "classes" ) ;

   studentId.init( student, "ID" ) ;
   firstName.init( student, "F_NAME" ) ;
   lastName .init( student, "L_NAME" ) ;
   age.init( student, "AGE" ) ;
   classCode.init( classes, "CODE" ) ;
   classTitle.init( classes, "TITLE" ) ;

   idTag.init( student, "STU_ID" ) ;
   codeTag.init( enrolment, "ENR_CODE" ) ;

   codeBase.exitTest( ) ;
}

void printStudents( )
{
   cout << endl << "       " << firstName.str( ) ;
   cout << " " << lastName.str( ) ;
   cout << " " << studentId.str( ) ;
   cout << " " << age.str( ) << endl ;
}

void setRelation( void )
{
   classRel.init( classes ) ;

   enrollRel.init( classRel, enrolment, "CODE", codeTag ) ;

   studentRel.init( enrollRel, student, "STU_ID_TAG", idTag ) ;
}

void printStudentList( char *expr, long direction )
{
   classRel.querySet( expr ) ;
   classRel.sortSet( "student->L_NAME + student->F_NAME" ) ;

   enrollRel.type( relate4scan ) ;

   int rc, endValue ;
   if( direction > 0 )
   {
      rc = classRel.top( ) ;
      endValue = r4eof ;
   }
   else
   {
      rc = classRel.bottom( ) ;
      endValue = r4bof ;
   }
   cout << classCode.str( ) ;
   cout << "  " << classTitle.str( ) << endl ;

   for ( ; rc != endValue ; rc = classRel.skip( direction ) )
      printStudents( ) ;

   cout << endl ;
}

void main( void )
{
   openDataFiles( ) ;

   setRelation( ) ;

   printStudentList( "CODE = 'MATH114 '", 1L ) ;

   printStudentList( "CODE = 'CMPT411 '", -1L ) ;

   codeBase.unlock( ) ;
   classRel.free( ) ;

   codeBase.initUndo( ) ;
}
