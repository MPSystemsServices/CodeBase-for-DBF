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

#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 dataFile ;

   settings.autoOpen = 0 ;
   settings.memSizeBlock = 0x800 ; // 2048
   settings.memSizeBuffer = 0x2000 ; // 8192

   settings.errDefaultUnique = r4unique ;
   dataFile.open( settings, "INFO" ) ;

   // this is equivalent to calling Code4::exitTest( )
   if( settings.errorCode )  settings.exit( ) ;

   // ...

   settings.initUndo( ) ;
}
