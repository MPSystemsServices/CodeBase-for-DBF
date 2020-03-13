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

/*ex127.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *name ;
   int rc ;
   long recNo ;
   char *info, *otherInfo ;

   SORT4 dbSort ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   name = d4field( data, "NAME" ) ;

   sort4init( &dbSort, &cb, f4len( name ), d4recWidth( data ) + 1 ) ;

   for( rc = d4top( data ); rc == r4success; rc = d4skip( data, 1L ) )
      sort4put( &dbSort, d4recNo(data), f4ptr(name), d4record(data) ) ;

   d4close( data ) ; /* database stored in dbSort.*/

   sort4getInit( &dbSort ) ; /* no more items to add.*/

   printf( "Database sorted on NAME: \n" ) ;
   while( sort4get( &dbSort, &recNo, &info, &otherInfo ) == 0 )
   {
      info[f4len(name) - 1] = 0 ;
      printf( "Record # %ld, Info %s, OtherInfo %s\n", recNo, info, otherInfo ) ;
   }

   sort4free( &dbSort ) ;
   code4initUndo( &cb ) ;
}
