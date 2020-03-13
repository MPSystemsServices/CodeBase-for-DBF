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

/*ex95.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info, *data ;
   FIELD4 *infoName, *dataLname ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;
   info = d4open( &cb, "INFO" ) ;
   data = d4open( &cb, "DATA" ) ;
   error4exitTest( &cb ) ;

   infoName = d4field( info, "NAME" ) ;
   dataLname = d4field( data, "LNAME" ) ;

   for( d4top( info ), d4top( data ) ; !d4eof( info ) && !d4eof( data ) ;
   d4skip( info, 1 ), d4skip( data, 1 ) )
   f4assignField( infoName, dataLname ) ; /* copy 'LNAME' into 'NAME' */

   code4initUndo( &cb ) ;
}
