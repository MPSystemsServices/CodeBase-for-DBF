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

/*ex67.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 settings ;
   DATA4 *info;
   TAG4 *tag ;

   code4init( &settings ) ;
   info = d4open( &settings, "INFO" ) ;

   /* List the names of the tags in any production index file corresponding
      to "INFO" */

   printf( "Production index file tags for data file: %s\n", d4alias( info ) ) ;
         /*d4tagNext returns the first tag when NULL is passed to it */
   for( tag = d4tagNext(info, NULL); tag != NULL ; tag = d4tagNext( info, tag ))
      printf( "Tag name: %s\n", t4alias( tag ) ) ;

   code4initUndo( &settings ) ;
}
