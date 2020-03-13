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
*   DATAINFO.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000; // for all Borland compilers
#endif

int main(int argc,char *argv[])
{
   if(argc != 2)
   {
      printf(" USAGE: DATAINFO <FILENAME.DBF> \n");
      exit(0);
   }
   Code4    codeBase ;
   Data4    dataFile( codeBase, argv[1] ) ;
   codeBase.exitTest( ) ;

   long recCount = dataFile.recCount( ) ;
   int numFields = dataFile.numFields( ) ;
   long recWidth = dataFile.recWidth( ) ;
   const char *alias = dataFile.alias( ) ;

   cout << "\t\t+---------------------------------+" << endl ;
   printf( "\t\t¦ Data File: %12s         ¦\n",argv[1]);
   printf( "\t\t¦ Alias    : %12s         ¦\n",alias);
   printf( "\t\t¦                                 ¦\n");
   printf( "\t\t¦ Number of Records: %7ld      ¦\n",recCount);
   printf( "\t\t¦ Length of Record : %7ld      ¦\n",recWidth);
   printf( "\t\t¦ Number of Fields : %7d      ¦\n",numFields);
   printf( "\t\t¦                                 ¦\n");
   printf( "\t\t¦ Field Information :             ¦\n");
   printf( "\t\t¦---------------------------------¦\n");
   printf( "\t\t¦ Name       ¦ type ¦ len  ¦ dec  ¦\n");
   printf( "\t\t¦------------+------+------+------¦\n");

   for(int j = 1; j <= dataFile.numFields( ); j ++)
   {
      Field4 field( dataFile, j ) ;
      const char * name = field.name( ) ;
      char type = (char ) field.type( ) ;
      int len = (int) field.len( ) ;
      int dec = field.decimals( ) ;

      printf( "\t\t¦ %10s ¦   %c  ¦ %4d ¦ %4d ¦\n",name,type,len,dec);

   }
   cout << "\t\t+---------------------------------+" << endl ;

   dataFile.close( ) ;
   codeBase.initUndo( ) ;

   return 0;
}
