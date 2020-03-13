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

/* ex70.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   TAG4 *tag ;

   code4init( &cb ) ;
   data = d4open( &cb, "DBF" ) ;
   tag = d4tag( data, "DBF_NAME" ) ; /* A tag with '.NOT.DELETED()' filter */

   d4top( data ) ; /* Position to the first record that is not deleted.*/
   d4delete( data ) ; /* The current record no longer is located in the tag*/

   d4tagSync( data, tag );/*The change to the record is flushed, and the datafile                       is positioned on a valid record.*/
   d4skip( data, 1L) ;
   /*... some other code ...*/

   code4initUndo( &cb ) ;
}
