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

/*ex89.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main()
{
   CODE4  cb ;
   DATA4  *data ;
   EXPR4 *expr ;
   char *result ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA" ) ;

   d4go( data, 1L ) ;
   /* "FNAME" and "LNAME" are Character field names of data file "DATA.DBF"*/

   expr = expr4parse( data, "FNAME+\' \'+LNAME") ;
   expr4vary( expr, &result ) ;
   printf( "FNAME and LNAME for Record One: %s\n", result ) ;

   expr4free( expr ) ;
   d4close( data ) ;
   code4initUndo( &cb ) ;
}
