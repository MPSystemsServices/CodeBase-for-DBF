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

void main ()
{
   CODE4 cb ;
   DATA4 *inventory ;
   FIELD4 *minOnHand, *onHand, *stockName;
   int oldOpt, oldExcl ;
   int count ;

   code4init( &cb );

   oldOpt = cb.optimize ; /* save old optimization setting.*/
   oldExcl = cb.accessMode ;

   cb.optimize = OPT4EXCLUSIVE ; /* optimize all new files */
   cb.accessMode = OPEN4DENY_RW ;

   inventory = d4open( &cb, "INVENT.DBF") ; /* Read optimized */

   minOnHand = d4field( inventory, "MIN_ON_HND" ) ;
   onHand = d4field( inventory, "ON_HAND" ) ;
   stockName = d4field( inventory, "ITEM" )  ;

   count = 0 ;
   if( cb.errorCode >= 0 )
   {
      code4optStart( &cb ) ;
      for( d4top( inventory ) ; ! d4eof( inventory) ; d4skip( inventory, 1L) )
         if( f4long(onHand) < f4long( minOnHand ) )
            count++ ;
   }
   code4optSuspend( &cb ) ;
   cb.optimize = oldOpt ;
   cb.accessMode = oldExcl ;
   d4close( inventory) ;
   printf( "%d items need to be restocked.\n", count) ;
   code4initUndo( &cb ) ;
}
