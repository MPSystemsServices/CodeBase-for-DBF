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

/*ex0.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 settings ;
   DATA4 *dataFile ;

   code4init( &settings ) ;
   settings.autoOpen = 0 ;
   settings.memSizeBlock = 0x800 ; /* 2048*/
   settings.memSizeBuffer = 0x2000 ; /* 8192*/

   settings.errDefaultUnique = r4unique ;

   dataFile = d4open( &settings, "INFO.DBF" ) ;

   /* this is equivalent to calling error4exitTest( )*/
   if( settings.errorCode < 0 )  code4exit( &settings ) ;

   /* ... */
   code4initUndo( &settings ) ;
}
