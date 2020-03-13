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

/*ex125.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info ;
   RELATE4 *TopMaster ;

   code4init( &cb ) ;
   info = d4open( &cb, "INFO" ) ;

   TopMaster = relate4init( info ) ;

   /* ... other code ...*/

   /* This relation tree is no longer needed. Create a new one. */
   relate4free( TopMaster, 0 ) ;

   TopMaster = relate4init( info ) ;

   /* ... other code ... */

   relate4free( TopMaster, 1 ) ;  /* Automatically close all files in the relation. */

   code4close( &cb ) ;     /* Close any remaining files. */
   code4initUndo( &cb ) ;
}
