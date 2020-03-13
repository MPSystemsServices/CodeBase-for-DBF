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

CODE4 cb ;
DATA4 *data ;
FIELD4 *fname ;

FIELD4INFO fieldInfo[] =
{
   { "FNAME", 'C', 20, 0 },
   { 0,0,0,0 },
} ;

#define NO_RECORDS 4

char *records[NO_RECORDS] =
{
   "HARRY",
   "JULES",
   "CLINT",
   "ROGER",
} ;

void main(void)
{
   int i ;

   code4init( &cb ) ;
   cb.safety = 0 ;
   data = d4create( &cb, "TO_DBF", fieldInfo, 0 ) ;
   if ( data )
   {
      fname = d4field( data, "fname" ) ;

      for ( i = 0 ; i < NO_RECORDS ; i++ )
      {
         d4appendStart( data, 0 ) ;
         f4assign( fname, records[i] ) ;
         if ( d4append( data ) )
            printf( "ERROR: Could not append record to database.\n" ) ;
      }
      printf( "The TO_DBF has been created.\n" ) ;
   }
   else
      printf( "ERROR: Could not create database.\n" ) ;

   code4initUndo( &cb ) ;
}



