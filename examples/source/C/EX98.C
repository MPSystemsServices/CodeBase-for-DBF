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

/*ex98.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *comments ;
   long count ;

   code4init( &cb ) ;
   data = d4open( &cb , "DATA3" ) ;
   comments = d4field( data, "COMMENTS" ) ;

   error4exitTest( &cb ) ;
   count = 0 ;
   for( d4top( data ) ; !d4eof( data ) ; d4skip( data, 1) )
      if( f4memoLen( comments ) )
          count ++ ;

   printf( "There were %ld memo entries out of %ld records\n", count,
                                    d4recCount( data ));
   code4initUndo( &cb ) ;
}
