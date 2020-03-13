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

/*ex16.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4  *db ;
   EXPR4 *ex, *ex2 ;
   EXPR4CALC *names ;
   char *result ;

   code4init( &cb ) ;
   db = d4open( &cb, "DATA" ) ;
   d4top( db ) ;

   ex = expr4parse( db, "'HELLO '+TRIM(LNAME)+', '+TRIM(FNAME)" ) ;
   error4exitTest( &cb );

   names = code4calcCreate( &cb, ex, "NAMES" ) ;
   expr4vary(ex, &result) ;
   printf( "%s is the result\n", result ) ;

   ex2 = expr4parse( db, "'HELLO '+NAMES()" ) ; /*no space in dBASE function calls.*/
   error4exitTest( &cb );

   expr4vary(ex2, &result ) ;
   printf( "%s is the second result\n", result ) ;

   expr4free( ex2 ) ;

   code4calcReset( &cb ) ;
   code4initUndo( &cb );
}
