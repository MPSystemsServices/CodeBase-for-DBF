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

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

long recsInFile( CODE4 *cb, DATA4 *d )
{
   long count ;

   if( cb->errorCode ) return -1L ; /* an error occurred */

   count = d4recCount( d ) ; /* save the record count */

   return count ;
}

void main( )
{
   CODE4 cb ;
   DATA4 *data ;
   long count ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   count = recsInFile( &cb, data ) ;
   printf( "the number of records in the file is %d \n", count ) ;
   code4initUndo( &cb ) ;
}
