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

/*ex2.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info, *data;
   INDEX4 *infoIndex;

   code4init( &cb ) ;
   cb.autoOpen = 0 ;      /* Do not automatically open production index file. */
   cb.errOpen = 0 ;
   info = d4open( &cb, "INFO" ) ;

   infoIndex = i4open( info, "INFO" ) ;

   if( cb.errorCode < 0 )
      printf("Production index file is not opened.\n") ;

   /* DATA.DBF has a production index. Open it. */
   cb.autoOpen = 1 ;
   data = d4open( &cb, "DATA" ) ;

   /*  Some other code */

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
