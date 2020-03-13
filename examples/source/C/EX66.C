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

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *name ;

   code4init( &cb ) ;
   data = d4open( &cb, "NAMES" ) ;
   /* Skip to the last record in the file whose NAME field is "John"*/

   code4optStart( &cb ) ;

   name = d4field( data, "F_NAME" ) ;

   for( d4bottom( data ) ;! d4bof( data ) ; d4skip( data, -1L ) )
      if ( strcmp( "John        ", f4str( name )) == 0 )  /* 0 indicates a find */
         break ;

   if( d4bof( data ) )
      printf( "John not located\n") ;
   else
      printf( "The last John is in record: %d\n",d4recNo( data ) ) ;

   printf(" the total number of records in file is %d\n", d4recCount(data)) ;
   d4close( data ) ;
   code4initUndo( &cb ) ;
}
