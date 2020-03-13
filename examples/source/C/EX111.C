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

/*ex111.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

typedef struct
{
   short id ;
   char password[9] ;
} MY_STRUCT ;

int readUserInfo( FILE4 *file, MY_STRUCT *ms, int user)
{
   int rc ;
   rc = file4readAll( file, user*sizeof(MY_STRUCT), ms, sizeof(MY_STRUCT) ) ;
   if( rc != 0 )
   {
      printf( "Could not read user # %d\n", user ) ;
      return rc ;
   }
   printf( "id: %d password %s\n", ms->id, ms->password ) ;
   return 0 ;
}

void main()
{
   CODE4 cb ;
   FILE4 testFile ;
   MY_STRUCT *info ;
   int userNum ;

   code4init( &cb  );
   file4open( &testFile, &cb, "TEST.FIL", 0 ) ;
   info = ( MY_STRUCT * )malloc( sizeof( MY_STRUCT ) ) ;
   for (userNum = 0; userNum <= 9 ; userNum++ )
      readUserInfo( &testFile, info, userNum ) ;
   file4close( &testFile ) ;
   code4initUndo( &cb ) ;
}
