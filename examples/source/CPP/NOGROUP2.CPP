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
*   NOGROUP2.CPP  Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000; // for all Borland compilers
#endif

Code4 codeBase ;
Data4 dataFile ;
Tag4 nameTag, ageTag, idTag;

void printRecords( void )
{

}

void main( void )
{
   #if !defined(S4CLIPPER) && !defined(S4CLIENT)
      fprintf(stderr,"This example is only for Clipper.\n");
   #else
      #ifdef S4CLIENT
         codeBase.connect(0,0,0,0,0);
      #endif

      if (strcmp(codeBase.indexExtension(),"NTX") == 0)
      {
         codeBase.autoOpen = 0;
         codeBase.safety = 0;

         dataFile.open( codeBase, "STUDENT.DBF" ) ;

         nameTag.open( dataFile, "STU_NAME" ) ;
         ageTag.open( dataFile, "STU_AGE" ) ;
         idTag.open( dataFile, "STU_ID" ) ;

         dataFile.select( nameTag ) ;
         printRecords();

         codeBase.closeAll( ) ;
      }
      else
         fprintf(stderr,"This example is only for Clipper.\n");
   #endif

   codeBase.initUndo( ) ;
}
