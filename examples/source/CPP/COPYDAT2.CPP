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
*   COPYDAT2.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main(int argc,char *argv[])
{
   if(argc != 3)
   {
      printf("USAGE:COPYDATA <FROMFILE> <TOFILE>\n");
      exit(1);
   }

   Code4      codeBase;

   codeBase.safety = 0;
   Data4 dataFile( codeBase, argv[1] ) ;
   codeBase.exitTest( ) ;

   Field4info fields( dataFile ) ;  //copy the fields

   //obtain the Index4 object of the production index if one exists
   Index4 index = dataFile.index(dataFile.alias( ) ) ;
   Data4 dataCopy ;

   if( index.isValid( ) )
   {
      Tag4info tags( index ) ;
      dataCopy.create( codeBase, argv[2], fields.fields( ), tags.tags( ) ) ;
      tags.free( ) ;
   }
   else
      dataCopy.create(codeBase, argv[2], fields.fields( ) ) ;

   codeBase.closeAll( ) ;
   fields.free( ) ;
   codeBase.initUndo( ) ;
}
