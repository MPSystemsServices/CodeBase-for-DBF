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

// ex141.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

int seekMaster( Relate4 r, Tag4 masterTag, char *seekKey )
{
   Data4 master = r.master( ).data( ) ;
   master.select( masterTag ) ;
   int rc = master.seek( seekKey ) ; // seek for the requested value

   if( rc == r4success )
      r.doOne( ) ; // position the slave data file to the appropriate
                   // record
   return rc ;
}

void main( )
{
   Code4 cb ;
   Data4 enroll( cb, "ENROLL" ) ;
   Data4 master( cb, "CLASSES" ) ;
   Data4 student( cb, "STUDENT" ) ;

   Tag4 enrollTag( enroll, "ENR_CODE" ) ;
   Tag4 studentTag( student, "STU_ID" ) ;
   Tag4 classTag( master, "CODE_TAG" ) ;

   Relate4set MasterRelation( master ) ;
   Relate4 relation1( MasterRelation, enroll, "CODE", enrollTag ) ;
   Relate4 relation2( relation1, student, "STU_ID_TAG", studentTag ) ;

   relation1.type( relate4scan ) ;

   seekMaster( relation1, classTag, "CMPT401" ) ;
   MasterRelation.free( ) ;
   cb.initUndo( ) ;
}
