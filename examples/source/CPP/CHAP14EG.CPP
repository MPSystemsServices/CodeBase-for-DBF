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

//chap14eg.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int openFileEx( Code4 &codeBase,Data4 data, char *name)
{
   int oldAccessMode = codeBase.accessMode ;
   codeBase.accessMode = OPEN4DENY_RW ;

   data.open( codeBase, name ) ;

   codeBase.accessMode = oldAccessMode ;

   return data.isValid( ) ;
}

void main( )
{
   Code4 cb ;
   Data4 db ;
   openFileEx( cb, db, "INFO" ) ;
   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
