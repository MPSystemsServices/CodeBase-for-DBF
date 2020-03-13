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

/*ex68.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int searchAll( DATA4 *d, char *value )
{
   TAG4 *origTag, *tag ;
   long origRecNo ;

   origTag = d4tagSelected( d ) ; /* Save the current tag*/
   origRecNo = d4recNo( d ) ;
               /*d4tagPrev returns the last tag when NULL is passed to it*/
   for( tag = d4tagPrev( d, NULL); tag != NULL; tag = d4tagPrev( d, NULL ))
   {
      d4tagSelect( d, tag ) ;
      if( d4seek( d, value ) == 0 )
      {
         d4tagSelect( d, origTag ) ;
         return d4recNo( d ) ;
      }
   }
   d4tagSelect( d, origTag ) ;
   d4go( d, origRecNo ) ;
   return -1 ;
}

void main()
{
   CODE4 cb ;
   DATA4 *data ;
   char key[] = "Abbott" ;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;

   rc = searchAll( data, key ) ;
   if (rc != -1 )
   {
      d4go( data, rc ) ;
      printf( "%s\n", d4record( data ) ) ;
   }
   code4initUndo( &cb ) ;
}
