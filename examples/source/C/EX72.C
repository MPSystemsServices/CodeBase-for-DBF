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

/* ex72.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *name ;
   TAG4 *nameTag ;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   nameTag = d4tag( data, "INF_NAME" ) ;
   name = d4field( data, "NAME" ) ;

   error4exitTest( &cb ) ; /* check for errors*/
   d4lockAll( data ) ;
   d4tagSelect( data, nameTag ) ;

   for(rc = d4seek( data, "Fred" ) ; rc == r4success ; rc = d4skip( data, 1) )
   {
      if( memcmp( f4ptr( name ), "Fred", 4 ) == r4success )
         printf( "Fred in record %d.\n", d4recNo( data ) );
      else
         break ;
   }
   d4unlock( data ) ;

   d4close( data ) ;
   code4initUndo( &cb ) ;
}
