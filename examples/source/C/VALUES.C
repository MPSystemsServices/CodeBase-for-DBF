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

CODE4 cb ;
DATA4 *data ;
FIELD4 *value ;

FIELD4INFO fieldInfo[] =
{
   { "VALUE", 'C', 10, 0 },
   { 0,0,0,0 },
} ;

void main(void)
{
   code4init( &cb ) ;
   cb.safety = 0 ;
   data = d4create( &cb, "VALUES", fieldInfo, 0 ) ;
   if ( data )
   {
      value = d4field( data, "VALUE" ) ;

      d4appendStart( data, 0 ) ;
      f4assign( value, "12.5" ) ;
      if ( d4append( data ) )
         printf( "ERROR: Could not append record to VALUES database.\n" ) ;
   }
   else
      printf( "ERROR: Could not create VALUES database.\n" ) ;

   code4initUndo( &cb ) ;
}
