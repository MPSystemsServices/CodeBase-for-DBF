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

/* f4char.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
void S4FUNCTION f4assignChar( FIELD4 *field, const int chr )
{
   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90502 ) )
         return ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90502 ) ;
         return ;
      }
      switch( field->type )
      {
         // AS 10/25/00 - only valid if assigning to r4str or r4charBin types...
         case r4str:
         case r4charBin:
         case r4log:
            break ;
         default:
            error4( field->data->codeBase, e4parm, E81409 ) ;
            return ;
      }
   #endif

   if ( error4code( field->data->codeBase ) < 0 )
      return ;

   #ifndef S4SERVER
      #ifndef S4OFF_ENFORCE_LOCK
         if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
            if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
            {
               error4( field->data->codeBase, e4lock, E90502 ) ;
               return ;
            }
      #endif
   #endif

   f4blank( field ) ;
   *f4assignPtr( field ) = (char)chr ;
}
#endif

int S4FUNCTION f4char( const FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90503 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90503 ) ;
         return -1 ;
      }
      /* the types that include character info in their types are:  r4num, r4float, r4str, r4date, r4log */
      switch( field->type )
      {
         case r4date:
         case r4num:
         case r4float:
         case r4log:
         case r4str:
         case r4charBin:
            break ;
         default:
            return error4( field->data->codeBase, e4parm, E81409 ) ;
      }
   #endif

   /* Return the first character of the record buffer. */
   return *f4ptr( field ) ;
}
