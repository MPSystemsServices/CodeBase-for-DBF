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

void main( void )
{
   CODE4 cb ;
   DATA4 *prices ;
   TAG4 *tag ;

   code4init( &cb );
   cb.readOnly = 1 ;

   /* open a file on a drive without write access*/
   prices = d4open( &cb, " W:\\INFO.DBF" ) ;

   error4exitTest( &cb ) ;

   tag = d4tagDefault( prices ) ;
   d4tagSelect( prices, tag ) ;

   if( d4seek( prices, "SMITH" ) == 0 )
      printf( "SMITH is found\n") ;
   else
      printf( "SMITH is not found\n" ) ;
   code4initUndo( &cb ) ;
}
