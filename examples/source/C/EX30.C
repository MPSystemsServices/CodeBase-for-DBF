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

/*ex30.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info ;
   FIELD4 *field ;
   long iRec ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW;
   info = d4open( &cb, "DATABASE.DBF") ;
   code4optStart( &cb ) ;

   field = d4field( info, "NAME") ;

   for(iRec = 1L ; iRec <= d4recCount( info ) ; iRec++ )
   {
      d4go( info, iRec ) ;
      f4assign( field, "New Data" ) ;
   }

   d4close( info ) ;
   code4initUndo( &cb ) ;
   code4exit( &cb ) ;
 }
