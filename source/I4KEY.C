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

/* i4key.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

char *S4FUNCTION t4getKey( TAG4 *t4 )
{
   // AS Oct 25/05 - new function available now...
   // #if !defined(S4CLIENT) && !defined(S4OFF_INDEX)
   #if !defined(S4OFF_INDEX)
      return tfile4key( t4->tagFile ) ;
   #else
      return 0 ;
   #endif
}

#if !defined(S4INDEX_OFF)
   char *S4FUNCTION tfile4key( TAG4FILE *t4 )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( t4->codeBase, 1, E91630 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
         {
            error4( 0, e4parm_null, E91630 ) ;
            return 0 ;
         }
      #endif

      if ( error4code( t4->codeBase ) < 0 )
         return 0 ;

      #ifdef S4CLIENT
         // just request directly from the server
         CODE4 *c4 = t4->codeBase ;
         CONNECTION4 *connection = &c4->defaultServer ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
            {
               error4( c4, e4struct, E91601 ) ;
               return 0 ;
            }
         #endif

         if ( t4->tagDataValid == 0 || t4->recNo == -1 )
            return 0 ;

         assert5( t4->currentKey != 0 ) ;

         return t4->currentKey ;

         /* AS Feb 9/09 - changed code to retrieve this information every time from the server
            int rc = connection4assign( connection, CON4TAG_KEY, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
            if ( rc < 0 )
               return 0 ;

            CONNECTION4TAG_KEY_INFO_IN *infoIn ;
            CONNECTION4TAG_KEY_INFO_OUT *out ;
            connection4addData( connection, NULL, sizeof(CONNECTION4TAG_KEY_INFO_IN), (void **)&infoIn ) ;
            memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
            infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
            // if ( t4->tagDataValid == 1 )  // use the starting recno...
               infoIn->startRecno = htonl5( t4->recNo ) ;
            // else
            //    infoIn->startRecno = htonl5( 0L ) ;
            u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
            rc = connection4sendMessage( connection ) ;
            if ( rc == 0 )
               rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               return 0 ;
            rc = connection4status( connection ) ;
            if ( rc < 0 )
               return 0 ;
            out = (CONNECTION4TAG_KEY_INFO_OUT *)connection4data( connection ) ;
            unsigned long keyLen = (unsigned long)(ntohl5(out->keyLen)) ;   // AS Jun 12/06 - conversion fix

            // use the c4->fieldBuffer for the temporary storage of the key
            if ( c4->bufLen <= keyLen )   // not room for field length + null
            {
               if ( u4allocAgain( c4, &c4->fieldBuffer, &c4->bufLen, keyLen ) < 0 )
               {
                  #ifdef E4STACK
                     error4stack( c4, e4memory, E90542 ) ;
                  #endif
                  return 0 ;
               }
            }
            memcpy( c4->fieldBuffer, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_KEY_INFO_OUT), keyLen ) ;
            return c4->fieldBuffer ;
         */
      #else
         B4BLOCK *b4 ;

         b4 = (B4BLOCK *)(t4->blocks.lastNode) ;

         if ( b4 == 0 )
            return 0 ;

         if ( b4->keyOn >= b4numKeys( b4 ) )  /* eof */
            return 0 ;

         return (char *)b4keyKey( b4, b4->keyOn ) ;
      #endif  /* S4CLIENT else */
   }
#endif  /* !S4OFF_INDEX */
