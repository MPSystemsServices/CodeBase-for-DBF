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

void main(  )
{
   CODE4 cb ;
   DATA4 *data ;
   int rc ;
   short fieldNum ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;

   code4optStart( &cb ) ;

   for( rc = d4top( data ); rc != r4eof ; rc = d4skip( data, 1L ) )
   {
      printf( "\n" ) ;

      for( fieldNum = 1 ; fieldNum <= d4numFields( data ) ; fieldNum++ )
          printf( " %s \n", d4fieldJ(data, fieldNum)) ;
   }
   code4initUndo( &cb ) ;
 }
