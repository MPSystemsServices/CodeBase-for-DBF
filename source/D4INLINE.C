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

/* d4inline.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#if !defined( S4UNIX ) && defined( __TURBOC__ )
   #pragma hdrstop
#endif  /* #if !defined( S4UNIX ) && defined( __TURBOC__ ) */



#if defined( S4SERVER ) && !defined( S4SINGLE )
   int S4FUNCTION code4unlockAuto( CODE4 *c4 )
   {
      TRAN4 *trans = code4trans( c4 ) ;
      // c4trans may be null (if no active c4 for example)
      assert5 ( trans != 0 ) ;
      if ( trans == 0 )
          return 0 ;
      int rc = trans->unlockAuto ;
      #ifndef S4OFF_WRITE
         #ifndef S4OFF_TRAN
            if ( rc != 0 )
            {
               // AS Apr 28/03 - transcations are run-time in odbc now
               // AS Jun 20/03 - was checking wrong flag
               #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
                  if ( c4->server->odbcTrans )  // odbc build, no trans available
               #endif
                  {
                     if ( code4tranStatus( c4 ) == r4rollback )
                        return 0 ;
                     if ( code4transEnabled( c4 ) )
                        if ( code4tranStatus( c4 ) == r4active )
                           return 0 ;
                  }
            }
         #endif
      #endif

      return rc ;
   }
#endif /* #if defined( S4SERVER ) && !defined( S4SINGLE ) */



#if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4SINGLE )
   int code4unlockAutoSave( CODE4 *c4 )
   {
      return c4->c4trans.trans.savedUnlockAuto ;
   }
#endif /* #if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4SINGLE ) */



#if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4SINGLE )
   int S4FUNCTION code4unlockAuto( CODE4 *c4 )
   {
      return c4->c4trans.trans.unlockAuto ;
   }
#endif /* #if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4SINGLE ) */



#if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
   int S4FUNCTION code4tranStatus( CODE4 *c4 )
   {
      return c4->c4trans.trans.currentTranStatus ;
   }
#endif  /* #if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) */



#if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
   S4EXPORT int S4FUNCTION code4tranStatusSet( CODE4 *c4, const int val )
   {
      return c4->c4trans.trans.currentTranStatus = val ;
   }
#endif  /* #if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) */



#if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4SINGLE )
   int code4unlockAutoSave( CODE4 *c4 )
   {
      return c4->currentClient->trans.savedUnlockAuto ;
   }
#endif /* #if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4SINGLE ) */



#if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
   int S4FUNCTION code4tranStatus( CODE4 *c4 )
   {
      return c4->currentClient->trans.currentTranStatus ;
   }
#endif  /* #if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) */



#if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
   int S4FUNCTION code4tranStatusSet( CODE4 *c4, const int val )
   {
      return c4->currentClient->trans.currentTranStatus = val ;
   }
#endif  /* #if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) */



#if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
   int code4transEnabled( CODE4 *c4 )
   {
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
            return 0 ;
      #endif
      #ifdef S4CLIENT
         return ( c4->currentClient->trans.c4trans->enabled ) ;
      #else
         return ( c4->currentClient->trans.c4trans->enabled && code4tranStatus( c4 ) != r4rollback && code4tranStatus( c4 ) != r4off ) ;
      #endif
   }
#endif  /* #if !defined( S4INLINE ) && defined( S4SERVER ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) */



#if !defined( S4INLINE ) && defined( S4SERVER )
   TRAN4 *code4trans( CODE4 *c4 )
   {
      return &c4->currentClient->trans ;
   }
#endif  /* #if !defined( S4INLINE ) && defined( S4SERVER ) */



#if !defined( S4INLINE ) && !defined( S4CLIENT ) && !defined( S4OFF_TRAN )
   unsigned short int tran4entryLen( LOG4HEADER *header )
   {
      return sizeof( LOG4HEADER ) + header->dataLen + sizeof( TRAN4ENTRY_LEN ) ;
   }
#endif /* #if !defined( S4INLINE ) && d!defined( S4CLIENT ) && !defined( S4OFF_TRAN ) */



   #if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4OFF_TRAN )
      int code4transEnabled( CODE4 *c4 )
      {
         // AS Jun 20/03 - was checking wrong flag
         #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
            if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
               return 0 ;
         #endif
         #ifdef S4CLIENT
            return c4->c4trans.enabled ;
         #else
            return ( c4->c4trans.enabled && code4tranStatus( c4 ) != r4rollback && code4tranStatus( c4 ) != r4off ) ;
         #endif
      }
   #endif  /* #if !defined( S4INLINE ) && !defined( S4SERVER ) && !defined( S4OFF_TRAN ) */


#if !defined( S4INLINE ) && !defined( S4SERVER )
   TRAN4 *code4trans( CODE4 *c4 )
   {
      return &c4->c4trans.trans ;
   }
#endif /* #if !defined( S4INLINE ) && !defined( S4SERVER ) */



#ifndef S4INLINE
   int tran4dataListSet( TRAN4 *t4, LIST4 *list )
   {
      #ifdef E4PARM_LOW
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E93839 ) ;
      #endif
      t4->dataList = list ;
      return 0 ;
   }
#endif /* S4INLINE */



#ifndef S4INLINE
   LIST4 *S4FUNCTION tran4dataList( TRAN4 *t4 )
   {
      #ifdef E4PARM_LOW
         if ( t4 == 0 )
         {
            error4( 0, e4parm_null, E93838 ) ;
            return 0 ;
         }
      #endif
      return t4->dataList ;
   }
#endif /* S4INLINE */
