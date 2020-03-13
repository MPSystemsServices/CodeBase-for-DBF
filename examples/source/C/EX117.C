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

/*ex117.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

typedef struct myStructSt
{
   short id ;
   char password[9] ;
} MY_STRUCT ;

int getNextStructure( FILE4SEQ_READ *seqFile, MY_STRUCT *ms )
{
   if( file4seqRead( seqFile, ms, sizeof( MY_STRUCT ) ) != sizeof( MY_STRUCT ) )
   {
      memset( ms, 0, sizeof(MY_STRUCT ) ) ;
      return 1 ;
   }
   return 0 ;
}

void main()
{
   CODE4 cb ;
   FILE4 passFile ;
   FILE4SEQ_READ readFile ;
   MY_STRUCT *person ;
   char buffer[0x1400] ;

   code4init( &cb ) ;
   file4open( &passFile, &cb, "TEST.FIL", 0 ) ;
   file4seqReadInit( &readFile, &passFile, 0, buffer, sizeof(buffer) );
   person = ( MY_STRUCT *) malloc( sizeof( MY_STRUCT ) ) ;
   getNextStructure( &readFile, person ) ;
   if ( person != NULL )
      printf( "id: %d password: %s\n", person->id, person->password ) ;

   file4close( &passFile ) ;
   code4initUndo( &cb  ) ;
}
