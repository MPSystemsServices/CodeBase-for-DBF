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

/*ex105.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 textFile ;
   int rc ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;
   cb.errCreate = 0 ; /* Handle the errors at the application level*/
   rc = file4create( &textFile, &cb, "C:\\TEMP\\TEXT.FIL", 0 ) ;

   if( rc < 0 || rc == r4noCreate)
   {
      printf( "File 'C:\\TEMP\\TEXT.FIL' NOT created. Make sure it does not exist.\n") ;
      code4exit( &cb ) ;
   }
   file4write( &textFile, 0, "Some Sample Text", 16 ) ;

   file4close( &textFile ) ;
   code4initUndo( &cb ) ; /* flush changes and close file. */
}
