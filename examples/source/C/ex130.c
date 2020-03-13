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

void main()
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *field ;
   short j ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA1") ;

   /* list the fields that are character fields */
   for (j = 1 ; j < d4numFields( data ) ; j++ )
   {
      field = d4fieldJ( data, j ) ;
      if ( f4type( field ) == r4str )
         printf( "%s\n", f4name( field ) ) ;
   }

   code4initUndo( &cb ) ;
}
