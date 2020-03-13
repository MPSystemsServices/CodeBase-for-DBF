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

//ex124.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

typedef struct myStructSt
{
   char id[6] ;
   int checkSum ;
   char password[15] ;
} MY_STRUCT ;

int readUserInfo( File4 &file, MY_STRUCT *ms, int user)
{
   int rc = file.readAll( user*sizeof(MY_STRUCT), ms, sizeof(MY_STRUCT) ) ;
   if( rc != 0 )
   {
      cout << "Could not read user #" << user << endl ;
      return rc ;
   }
   return 0 ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "PASS.FIL" ) ;
   MY_STRUCT info ;
   for (int user = 0 ; user <= 2 ; user++ )
      readUserInfo( file, &info, user ) ;
   cb.initUndo( ) ;
}
