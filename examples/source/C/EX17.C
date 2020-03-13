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

/*ex17.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void openAFile( CODE4 *cb )
{
   DATA4 *d ;

   /* 'd' falls out of scope.  Data file is still open*/
   d = d4open( cb, "INFO" ) ;
}

void main( void )
{
   CODE4 cb ;
   DATA4 *d ;

   code4init( &cb ) ;
   cb.autoOpen = 0 ;
   openAFile( &cb ) ;

   d = d4open( &cb, "DATA" ) ;   /* open a second file */
   printf("Number of records in DATA: %d\n",d4recCount( d ) ) ;

   code4close( &cb ) ; /* INFO and DATAFILE are both closed*/
   code4initUndo( &cb ) ;
}
