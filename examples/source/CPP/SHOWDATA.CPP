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
*   SHOWDATA.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

int main(int argc,char *argv[])
{
   if(argc != 2)
   {
      cout << "USAGE: SHOWDATA <FILENAME.DBF>"<< endl;
      exit(0) ;
   }

   Code4 codeBase ;
   Data4 dataFile( codeBase, argv[1] ) ;
   codeBase.exitTest( ) ;

   int numFields = dataFile.numFields( ) ;
   for( int rc = dataFile.top( ); rc == r4success ; rc = dataFile.skip( ))
   {
      for( int j = 1; j <= numFields; j++ )
      {
         Field4memo field( dataFile, j ) ;
         cout << " " << field.str( ) ;
      }
      cout << endl ;
   }
   dataFile.close( ) ;
   codeBase.initUndo( ) ;
   return 0 ;
}
