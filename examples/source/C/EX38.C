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

/* Check the validity of an index file. */
void main( int argc, char *argv[ 2 ] )
{
    if ( argc > 2 )
    {
     	CODE4 cb ;
		DATA4 *checkData ;
		INDEX4 *testIndex ;

		code4init( &cb ) ;
		cb.accessMode = OPEN4DENY_RW ;
      	cb.autoOpen = 0 ;  /* open index file manually.*/

 		checkData = d4open( &cb, argv[1] ) ;
 		testIndex = i4open( checkData, argv[2]);

       	error4exitTest( &cb ) ;
       	code4optStart( &cb ) ;

       	if ( d4check( checkData ) == r4success )
           printf("\nIndex is OK !!\n") ;
       	else
			printf("\nProblem with Index !!\n") ;
		code4initUndo( &cb );
    }
    else
       printf( "\nUsage: PROGRAM  DataFile  IndexFile\n") ;
 }

