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
   CODE4 code ;
	DATA4 *dataFile ;
   int delCount, rc ;

   code4init( &code ) ;
   code.accessMode = OPEN4DENY_RW ;

   dataFile = d4open( &code, "INFO" ) ;
   error4exitTest( &code ) ;

   /* initialize optimization with default settings. */
   code4optStart( &code ) ;

   delCount = 0 ;
   for( rc = d4top( dataFile ); rc == r4success ; rc = d4skip( dataFile, 1L ) )
      if( d4deleted( dataFile) )
         delCount++ ;

   printf("%d records are marked for deletion.\n", delCount ) ;

   code4initUndo( &code ) ;
}
