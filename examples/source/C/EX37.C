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
	FIELD4 *field ;

	code4init( &cb ) ;
	data = d4open( &cb, "DATA2" ) ;
 	field = d4field( data, "NAME" ) ;
   d4top( data ) ;
	if ( d4changed( data, -1 ) != 0 )     /*Displays FALSE */
		printf( "Changed status: TRUE\n") ;
	else
    	printf( "Changed status: FALSE\n") ;
	d4lockAll( data ) ;  /*CODE4.lockEnforce default value is true, */
								/* so you must explicitly lock the record before*/
								/* modifying it */
   f4assign( field, "TEMP DATA") ;
	if ( d4changed( data, -1 ) != 0 )     /*Displays TRUE */
		printf( "Changed status: TRUE\n") ;
	else
    	printf( "Changed status: FALSE\n") ;

   d4changed( data, 0 ) ;

   d4close( data ) ;      /* The top record is not flushed.*/
   code4initUndo( &cb ) ;
 }

