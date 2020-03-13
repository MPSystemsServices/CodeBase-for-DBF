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

//ex128.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers
#define DATE_OFFSET 16

void main( )
{
   Code4 cb ;
   cb.accessMode = OPEN4DENY_RW ;
   File4 file ;
   Date4 runDate ;

   runDate.today( ) ; // initialize to the system clock

   if( file.open( cb, "TEXT.FIL" ) !=0 )
      return ;

   // file is opened exclusively - no need to lock
   file.write( DATE_OFFSET, runDate );
   file.close( ) ;
   cb.initUndo( ) ;
}
