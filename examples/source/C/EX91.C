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

/*ex91.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 settings ;
   DATA4 *db ;
   EXPR4 *fullName ;
   char *result, *name ;

   code4init( &settings ) ;
   db = d4open( &settings, "DATA" ) ;

   d4top( db ) ;
   fullName = expr4parse( db, "TRIM( LNAME )+', '+FNAME" ) ;
   name = (char * )malloc( expr4len( fullName ) ) ;
   expr4vary( fullName, &result ) ;
   strcpy( name, result );
   /* make copy of result which is copied over the next time expr4vary is called. */
   d4skip( db, 1 ) ;
   expr4vary( fullName, &result ) ;
   /* For illustration purposes only:
       Avoid using the expression module when the field functions will suffice */

   printf("%s is the first person in the data file\n", name ) ;
   printf("%s is the second person in the data file\n", result ) ;
   free( name ) ;
   code4initUndo( &settings ) ;
}
