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
   FILE4 temp ;

   code4init( &cb ) ;
   cb.errCreate = 0 ;

   if( file4create( &temp, &cb, "NEWFILE.TXT", 1 ) == r4noCreate)
      /* File exists. Try in the temp directory.*/
      file4create( &temp, &cb, "C:\\temp\\NEWFILE.TXT", 1 ) ;

   if( cb.errorCode < 0 )
      code4exit( &cb ) ;

   /* Some other code*/

   code4initUndo( &cb ) ;
}
