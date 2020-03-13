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

/*ex15.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 cb ;
DATA4 *data ;

int createFiles( void )
{
   FIELD4INFO fields [] =
   {
      { "NAME", 'C', 20, 0 },
      { "AGE", 'N', 3, 0 },
      { "BIRTHDATE", 'D', 8, 0 },
      { 0, 0, 0, 0 },
   };

   TAG4INFO tags [] =
   {
      { "INF1_NME", "NAME", 0, 0, 0 },
      { 0, 0, 0, 0, 0 },
   };

   cb.safety = 0 ; /* Turn off safety -- overwrite files*/
   data = d4create( &cb, "INFO1.DBF", fields, tags ) ;

   return cb.errorCode ;
}

void main()
{
   code4init( &cb );

   createFiles();

   code4initUndo( &cb ) ;
}
