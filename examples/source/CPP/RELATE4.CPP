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
*   RELATE4.C     Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 7 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

Code4      codeBase( 0 ) ;
Data4      student, enrolment ;
Field4     id, fName, lName, age, cCode ;
Relate4    slave ;
Relate4set master ;
Tag4       idTag, nameTag ;

void openDataFiles( void ) ;
void setRelation( void ) ;
void printRecord( void ) ;

void openDataFiles()
{
   codeBase.init( ) ;

   student.open( codeBase,"student" ) ;
   enrolment.open( codeBase,"enroll");

   id.init( student,"ID");
   fName.init( student,"F_NAME");
   lName.init( student,"L_NAME");
   age.init( student,"AGE");
   cCode.init( enrolment,"C_CODE_TAG");

   nameTag.init( student,"STU_NAME");
   idTag.init( enrolment,"ENR_ID");

   codeBase.exitTest( ) ;
}

void setRelation()
{
   master.init( student ) ;
   slave.init( master, enrolment, "ID", idTag);
}

void seek( Data4 dataFile, Tag4 tag, Relate4set relation, char *key)
{
   Tag4 oldTag;

   oldTag.initSelected( dataFile ) ;
   dataFile.select( tag ) ;

   dataFile.seek( key ) ;
   relation.doAll( ) ;

   dataFile.select( oldTag ) ;
}

void printRecord()
{
   cout << fName.str( ) << " " ;
   cout << lName.str( ) << " " ;
   cout << id.str( ) << " " ;
   cout << age.str( ) << " " ;
   cout << cCode.str( ) << endl ;
}

void main( void )
{
   openDataFiles( ) ;

   setRelation( ) ;

   seek( student
       , nameTag
       , master, "Tyler           Harvey         ") ;

   printRecord( ) ;

   seek( student
       , nameTag
       , master, "Miller          Albert        ");

   printRecord( ) ;

   codeBase.unlock( ) ;
   master.free( ) ;

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
}
