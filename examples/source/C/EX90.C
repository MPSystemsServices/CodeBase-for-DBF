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

/*ex90.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#define VOTE_AGE 18.0

void main( )
{
   CODE4 cb ;
   DATA4 *data ;
   EXPR4 *expr ;
   long count = 0;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   expr = expr4parse( data, "AGE" ) ;

   for( rc = d4top( data ) ; rc != r4eof ; rc = d4skip( data, 1 ) )
      if( expr4double( expr ) >= VOTE_AGE )
         count ++ ;

   printf( "Possible voters: %d\n", count ) ;

   expr4free( expr ) ;
   code4initUndo( &cb ) ;
}
