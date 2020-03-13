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

/*ex13.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 cb ;
DATA4 *d ;

int retry( void )
{
    char rc ;
    printf("Record locked by another user.\n") ;
    printf("Retry? (Y or N)\n") ;
    rc = getchar( ) ;
    if( rc == 'N' ) return 0 ;
    return 1 ;
}

void modifyRecordValues( void )
{
   int rc ;
   char buf[8+2] ;
   FIELD4 *field ;

    cb.readLock = 1 ;
    cb.lockAttempts = 3 ;

    field = d4field( d, "DATE") ;

    while( ((rc = d4top( d )) == r4locked) && (retry( ) == 1) ) ;
    if( rc == r4locked ) return ;

    while( !d4eof( d ) )
    {
       printf("\nEnter new record value: \n") ;
       fgets(buf, sizeof(buf), stdin) ;
       f4assign( field, buf ) ;

       while( ((rc = d4skip( d, 1L )) == r4locked) && (retry( ) == 1)) ;
       if( rc == r4locked ) return ;
    }
}

void main()
{
   code4init( &cb ) ;
   d = d4open( &cb, "DATEFILE" ) ;
   modifyRecordValues();
   code4initUndo( &cb );
}
