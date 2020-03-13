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

/***********************************************************************\
*                                                                       *
*   APPEND.C       Copyright (C) 1998 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 11 */

#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 cb ;
DATA4 *dataFrom, *dataTo ;
FIELD4 *infoFrom, *infoTo ;

void main(void)
{
   int rc1, rc2, rc ;

   code4init( &cb ) ;

/*   cb.optimize = OPT4ALL ;          */
/*   cb.optimizeWrite = OPT4ALL ;     */

   dataFrom = d4open( &cb, "from_dbf.dbf" ) ;
   dataTo = d4open( &cb, "to_dbf.dbf" ) ;
   error4exitTest( &cb ) ;

   code4optStart( &cb ) ;

   infoFrom = d4field( dataFrom, "NAME" ) ;
   infoTo = d4field( dataTo, "FNAME" ) ;

   cb.lockAttempts = 1 ;
   rc1 = d4lockFile( dataFrom ) ;
   rc2 = d4lockFile( dataTo ) ;
   if( rc1 != 0 || rc2 != 0 )
   {
      printf( "Locking Failed\n" ) ;
      exit(0) ;
   }

   for( rc = d4top(dataFrom); rc == 0
      ; rc = d4skip(dataFrom,1) )
   {
      d4appendStart( dataTo, 0 ) ;
      f4assignField( infoTo, infoFrom ) ;
      d4append( dataTo ) ;
   }

   d4unlock( dataFrom ) ;
   d4unlock( dataTo ) ;

   code4close( &cb ) ;
   code4initUndo( &cb );
}

