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

/*ex97.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

char *createBufCopy( FIELD4 *f )
{
   char *buf ;

   buf = (char *) malloc(f4len( f ) +1 ) ;
   memcpy( buf, f4ptr( f ), f4len( f ) );
   buf[f4len( f )] = 0;
   return buf ;
}
void main()
{
   CODE4 cb ;
   DATA4 *data;
   FIELD4 *field ;
   char *buffer ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   field = d4field( data, "NAME" ) ;
   d4top( data ) ;
   buffer = createBufCopy( field ) ;
   printf( "the copy of the buffer is %s\n", buffer ) ;
   code4initUndo( &cb ) ;
}
