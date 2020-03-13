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

FIELD4INFO fields[ ] =
{
 	{ "NAME", 'C', 20, 0 },
 	{ "AGE", 'N', 3, 0 },
	{ 0,0,0,0 },
} ;

void main( )
{
	CODE4 cb ;
	DATA4 *data ;
	code4init( &cb ) ;
	cb.safety = 0 ;
	data = d4create( &cb, "PERSON", fields, 0 ) ;
	if( data )
		printf( "PERSON data file successfully created.\n" ) ;
	code4close( &cb ) ;
	code4initUndo( &cb ) ;
}

