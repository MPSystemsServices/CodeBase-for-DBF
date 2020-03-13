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

/* ex122.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   int rc ;
   CODE4 cb ;
   DATA4 *enroll, *master, *student ;
   TAG4 *enrollTag, *studentTag ;
   RELATE4 *MasterRelation, *relation1, *relation2 ;
   FIELD4 *classCode, *classTitle, *enrollStudentId, *studentName ;

   code4init( &cb ) ;
   enroll = d4open( &cb, "ENROLL" ) ;
   master = d4open( &cb, "CLASSES" ) ;
   student = d4open( &cb, "STUDENT" ) ;

   enrollTag = d4tag( enroll, "ENR_CODE" ) ;
   studentTag = d4tag( student, "STU_ID" ) ;

   MasterRelation = relate4init( master ) ;
   relation1 = relate4createSlave( MasterRelation, enroll, "CODE", enrollTag ) ;
   relation2 = relate4createSlave( relation1, student, "STU_ID_TAG", studentTag);

   relate4type( relation1, relate4scan ) ;
   relate4sortSet( MasterRelation,  "LEFT(STUDENT->L_NAME,8) + ENROLL->C_CODE_TAG" ) ;

   classCode = d4field( master, "CODE" ) ;
   classTitle = d4field( master, "TITLE" ) ;
   enrollStudentId = d4field( enroll, "STU_ID_TAG") ;
   studentName = d4field( student, "L_NAME" ) ;

   error4exitTest( &cb ) ;

   for(rc = relate4top( MasterRelation ); rc != r4eof;
                        rc = relate4skip( MasterRelation, 1L ) )
   {
      printf( "%s ", f4str( studentName )) ; /* only one f4str per statement*/
      printf( "%s ", f4str( enrollStudentId ) ) ;
      printf( "%s ", f4str( classCode ) ) ;
      printf( "%s\n", f4str(classTitle ) ) ;
   }

   printf("Number of records in %s is %d\n", d4alias(master),d4recCount(master));

   relate4free( MasterRelation, 1 ) ;
   code4initUndo( &cb ) ;
}
