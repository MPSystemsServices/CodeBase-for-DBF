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

/*ex99.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *comments, *name ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA3" ) ;
   comments = d4field( data, "COMMENTS" ) ;
   name = d4field( data, "NAME" ) ;

   d4top( data ) ;
   /* display the null terminated contents of the memo field*/
   printf( "Memo field contents: %s\n", f4memoPtr( comments ) ) ;

   /* display the non-null terminated contents of the NAME field.
      this displays NAME plus any additional fields in the record buffer */
   printf( "NAME field contents: %s\n", f4ptr( name ) ) ;

   code4initUndo( &cb ) ; /* close all files and free memory */
}
