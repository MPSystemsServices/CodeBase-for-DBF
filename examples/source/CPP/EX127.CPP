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

#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

int addToFile( Code4 &cb, File4 &file, Str4 &string )
{
   int oldLockAttempts = cb.lockAttempts ;
   cb.lockAttempts = 1 ;
   long fileSize = file.len( ) ;

   if( file.lock( fileSize, LONG_MAX ) == r4locked )
   {
      cout << "Cannot add to file, another user is writing" << endl ;
      cb.lockAttempts = oldLockAttempts ;
      return 1 ;
   }
   // lock succeeded, I may add to the file without corrupting anyone else's
   // writes

   file.write( fileSize, string ) ;

   file.unlock( fileSize, LONG_MAX ) ;
   cb.lockAttempts = oldLockAttempts ;
   return 0 ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "TEXT.FIL" ) ;
   Str4large info( "adding a string to the file" ) ;
   addToFile( cb, file, info ) ;
   cb.initUndo( ) ;
}
