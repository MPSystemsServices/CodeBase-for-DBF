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
   CODE4 codeBase ;
   DATA4 *dataFile ;

   code4init( &codeBase ) ;
   dataFile = d4open( &codeBase, "INFO" ) ;

   d4optimize( dataFile, OPT4ALL ) ;
   code4optStart( &codeBase ) ;

   d4top( dataFile ) ;
   printf( "Press ENTER when you want to refresh your data.\n") ;
   getchar( ) ;

   d4refresh( dataFile ) ;
   d4top( dataFile ) ;
 /* re-read the record from disk. */
   printf( "The latest information is: %s \n",d4record( dataFile ) ) ;

   d4close( dataFile ) ;
   code4initUndo( &codeBase ) ;
}
