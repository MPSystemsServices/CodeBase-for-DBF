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

/* ex87.c */
#include "d4all.h"

int display( CODE4 *cb, char *p )
{
   if( p == NULL )
      return error4describe( cb, e4parm, 0, "Null display string", 0, 0 ) ;
   printf( "%s\n", p ) ;
   return 0 ;
}

void main()
{
   CODE4 cb ;
   char p[] = "some string" ;

   code4init( &cb ) ;
   display( &cb, p ) ;
   display( &cb, 0 ) ;
   code4initUndo( &cb ) ;
}
