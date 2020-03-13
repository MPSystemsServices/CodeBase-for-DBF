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

void main( )
{
   CODE4 cb ;
	DATA4 *data ;
	int rc ;

	code4init ( &cb ) ;
   /* Do not add duplicate records to unique tags or the data file and
      return r4unique when attempted.*/
   cb.errDefaultUnique = r4unique ;

   data = d4open( &cb, "INFO" ) ;

   d4top( data ) ;
   d4appendStart( data, 0 ) ;
   rc = d4append( data ) ; /* append a duplicate copy of the top record*/

   if( rc == r4unique )
      printf( "Attempt to add a duplicate record failed.\n") ;
   else
   {
      printf("Attempt to add a duplicate record succeeded\n");
      printf("Record in both data and index file\n") ;
   }

   d4close( data ) ;
   code4initUndo( &cb ) ;
}

