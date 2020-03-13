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

/* ex119.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

static FIELD4INFO fieldInfo[ ] =
{
    { "FIELD_NAME", 'C', 10, 0 },
    { "VALUE", 'N', 7, 2 },
    { 0,0,0,0 }
} ;

TAG4INFO tagInfo[ ] =
{
    { "T_NAME", "FIELD_NAME", "FIELD_NAME > 'A'", 0,0 },
    { "NAME_TWO", "VALUE", "", e4unique, r4descending },
    { 0,0,0,0,0 }
} ;


void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   INDEX4 *index ;

   code4init( &cb ) ;
   cb.safety = 0 ;

   data = d4create( &cb, "DB_NAME", fieldInfo, 0 ) ;
   if ( data )
   {
      index = i4create( data, "name", tagInfo ) ;
      d4close( data ) ;
   }

   code4initUndo( &cb ) ;
}
