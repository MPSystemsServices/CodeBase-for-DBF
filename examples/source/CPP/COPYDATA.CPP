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
*   COPYDATA.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; // for all Borland compilers
#endif

void main(int argc,char *argv[])
{
   if(argc != 3)
   {
      cout << "USAGE: COPYDATA <FROM FILE>  <TO FILE>" << endl ;
      exit(1);
   }
   Code4 codeBase;
   Data4 dataFile( codeBase, argv[1] ), dataCopy ;
   codeBase.exitTest( ) ;

   Field4info fields( dataFile ) ;
   codeBase.safety = 0;
   dataCopy.create( codeBase,argv[2], fields.fields( )) ;
   fields.free( ) ;

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
}
