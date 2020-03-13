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

/*ex118.c*/
#include "d4all.h"  

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int addLotsOfRecords( DATA4 *d ) 
{
	INDEX4 *index ;
	int i ;

	index = d4index( d, "INFO2" ) ; /* get the secondary index file*/
    								
	if( index != NULL )
       i4close( index ) ;
 
    d4top( d ) ;
    for( i = 200 ; i ; i -- )
    {
       d4appendStart( d, 0 ) ;
       d4append( d ) ; /* make 200 copies of record 1*/
    }

    /* open the index file and update it*/
    index = i4open( d, "INFO2" ) ;
    return i4reindex( index ) ;
}

void main()
{
	CODE4 cb ;
	DATA4 *data ;

	code4init( &cb ) ;
	data = d4open( &cb, "INFO") ;
   i4open( data, "INFO2" ) ;
	addLotsOfRecords( data ) ;
	code4initUndo( &cb ) ;
}
