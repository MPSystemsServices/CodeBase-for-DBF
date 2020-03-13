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

CODE4 cb ; /* CODE4 may be constructed globally.*/
DATA4  *data ;
FIELD4 *field ;

void main( void )
{
    code4init( &cb ) ;
	 cb.accessMode = OPEN4DENY_RW ;
 	 data = d4open( &cb, "DATABASE" ) ;
    code4optStart( &cb ) ;

    d4appendBlank( data ) ;

    /* Append a copy of record two.  (Assume record two exists.)*/
    d4go( data, 2 ) ;
    d4appendStart( data, 0) ;/* a false useMemoEntries parameter */
				/* Append a copy of record 2 including existing memo entries.*/
    d4append( data ) ;

    d4go( data, 2 ) ;
    d4appendStart( data, 1 ) ; /* a true parameter means use memo entries */
    d4append( data ) ;

    /* Set the record buffer to blank, change a field's value, and append
		the resulting record.*/
 	 d4appendStart( data, 0 ) ;
    d4blank( data ) ;

 	 field = d4field( data, "NAME" ) ;
    f4assign( field, "New field value" ) ;

    d4append( data ) ;
    /* close all open files and release any allocated memory */
    code4initUndo( &cb ) ;
}

