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

/*ex96.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void displayFieldStats( FIELD4 *f )
{
   DATA4 *db ;

   db = f4data( f ) ;
   printf( "-----------------------------------------------------\n") ;
   printf( "DataFile: %s Field: %s\n", d4alias( db ), f4name( f ) ) ;
   printf( "Length: %d     Type : %c\n", f4len( f ), f4type( f ) ) ;
   printf( "Decimals: %d\n", f4decimals( f ) ) ;
   printf( "-----------------------------------------------------\n" ) ;
   return ;
}
void main()
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *field ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   field = d4field( data, "NAME" ) ;

   displayFieldStats( field );
   code4initUndo( &cb ) ;
}

