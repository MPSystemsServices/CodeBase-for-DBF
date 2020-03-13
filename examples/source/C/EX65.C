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

/* ex65.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int SeekSeries( DATA4 *d, const char *s )
{
   int rc ;

   rc = d4seekNext( d, s ) ;

   if( rc == r4noTag || rc == r4entry || rc == r4locked || rc < 0 )
      return rc ;

   if( rc == r4after || rc == r4eof )
      rc = d4seek( d, s ) ;
   printf( " the found record %s \n", d4record( d ) ) ;
   return rc ;
}

void main()
{
   CODE4 cb ;
   DATA4 *data ;
   TAG4 *nameTag ;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "PEOPLE" ) ;
   nameTag = d4tag( data, "PPL_NAME") ;
   d4tagSelect( data, nameTag ) ;
   d4seek( data, "mickey" ) ;
   rc = SeekSeries( data, "mickey" ) ;
   code4initUndo( &cb ) ;
}
