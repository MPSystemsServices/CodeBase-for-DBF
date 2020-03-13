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

/* i4positi.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4INDEX_OFF
   #ifdef S4CLIENT
      double S4FUNCTION tfile4position( TAG4FILE *t4 )
      {
         int rc ;
         CONNECTION4TAG_POSITION_INFO_IN *info ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return (double)error4( 0, e4parm_null, E91642 ) ;
         #endif

         CODE4 *c4 = t4->codeBase ;
         if ( error4code( c4 ) < 0 )
            return (double)e4codeBase ;

         CONNECTION4 *connection = &c4->defaultServer ;
         if ( connection == 0 )
            return e4connection ;

         rc = connection4assign( connection, CON4TAG_POSITION, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;
         connection4addData( connection, NULL, sizeof( CONNECTION4TAG_POSITION_INFO_IN ), (void **)&info ) ;
         if ( t4->tagDataValid == 1 )  // use the starting recno...
            info->startRecno = htonl5( t4->recNo ) ;
         else
            info->startRecno = htonl5( 0L ) ;
         t4->tagDataValid = 0 ;  // reset to invalid
         memcpy( info->tagName, t4->alias, LEN4TAG_ALIAS  ) ;
         info->tagName[LEN4TAG_ALIAS] = 0 ;
         // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
         u4ncpy( info->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( dc4, rc, E94701 ) ;
         rc = connection4status( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E94701 ) ;
         int conLen = connection4len( connection ) ;
         if ( conLen < sizeof( CONNECTION4TAG_POSITION_INFO_OUT ) )
            return error4( c4, e4packetLen, E94701 ) ;
         CONNECTION4TAG_POSITION_INFO_OUT *out ;
         out = (CONNECTION4TAG_POSITION_INFO_OUT *)connection4data( connection ) ;

         t4->recNo = ntohl5(out->rec) ;

         // AS Feb 9/09 - added support to return key as well
         t4->currentKeyLen = (unsigned long)(ntohl5(out->keyLen)) ;
         if ( conLen != sizeof( CONNECTION4TAG_POSITION_INFO_OUT ) + t4->currentKeyLen )
            return error4( c4, e4packetLen, E94701 ) ;
         if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
         {
            if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E90542 ) ;
               #endif
               return 0 ;
            }
         }
         memcpy( t4->currentKey, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_POSITION_INFO_OUT), t4->currentKeyLen ) ;

         t4->tagDataValid = 1 ;
         return ntohd( out->position ) ;
      }


      int S4FUNCTION tfile4positionSet( TAG4FILE *t4, const double ipos )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         CODE4 *c4 = t4->codeBase ;
         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;
         CONNECTION4 *connection = &c4->defaultServer ;
         if ( connection == 0 )
            return e4connection ;

         t4->tagDataValid = 0 ;  // reset to invalid
         tfile4cacheReset( t4 ) ;

         int rc = connection4assign( connection, CON4TAG_POSITION_SET, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;
         CONNECTION4TAG_POSITION_SET_INFO_IN *info ;
         connection4addData( connection, NULL, sizeof( CONNECTION4TAG_POSITION_SET_INFO_IN ), (void **)&info ) ;
         info->pos = htond( ipos ) ;
         memcpy( info->tagName, t4->alias, LEN4TAG_ALIAS  ) ;
         info->tagName[LEN4TAG_ALIAS] = 0 ;
         // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
         u4ncpy( info->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( dc4, rc, E94701 ) ;
         rc = connection4status( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E94701 ) ;
         if ( connection4len( connection ) != sizeof( CONNECTION4TAG_POSITION_SET_INFO_OUT ) )
            return error4( c4, e4packetLen, E94701 ) ;
         CONNECTION4TAG_POSITION_SET_INFO_OUT *out ;
         out = (CONNECTION4TAG_POSITION_SET_INFO_OUT *)connection4data( connection ) ;

         t4->recNo = ntohl5(out->rec) ;
         t4->tagDataValid = 1 ;
         return rc ;
      }
   #else
      /* 'pos' is an percentage, positioning is approximate. */
      int tfile4position2( TAG4FILE *t4, double *result )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         *result = tfile4position( t4 ) ;
         return 0 ;
      }


      #ifndef N4OTHER
         double tfile4positionDbl( TAG4FILE *t4 )
         {
            double pos ;
            B4BLOCK *blockOn ;
            int n, min, max ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return (double)e4codeBase ;

            pos = .5 ;
            min = max = 1 ;    /* is the position a minimal or maximal position? */
            for ( blockOn = (B4BLOCK *)t4->blocks.lastNode ; blockOn != 0 ; )
            {
               #ifdef S4FOX
                  n = blockOn->header.nKeys ;
               #else
                  n = blockOn->nKeys + 1 ;
                  if ( b4leaf( blockOn ) )
                     n-- ;
               #endif

               if( n == 0 )
                  max = 0 ;
               else
               {
                  if ( min == 1 )
                     if ( blockOn->keyOn != 0 )
                        min = 0 ;
                  if ( max == 1 )
                     if ( blockOn->keyOn != ( n - 1 ) )
                        max = 0 ;
                  pos = ( blockOn->keyOn + pos ) / n ;
               }

               blockOn = (B4BLOCK *)blockOn->link.p ;
               if ( blockOn == (B4BLOCK *)t4->blocks.lastNode )
                  break ;
            }

            if ( max == 1 )
               pos = 1.0 ;
            if ( min == 1 )
               pos = 0.0 ;

            #ifdef S4FOX
               if ( t4->header.descending )   /* backwards in file... */
                  return 1.0 - pos ;
               else
            #endif
            return pos ;
         }



         double S4FUNCTION tfile4position( TAG4FILE *t4 )
         {
            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return (double)error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return (double)e4codeBase ;

            return tfile4positionDbl( t4 ) ;
         }



         int S4FUNCTION tfile4positionSet( TAG4FILE *t4, const double ipos )
         {
            int rc, n, finalPos ;
            B4BLOCK *blockOn ;
            double pos ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            #ifndef S4SINGLE
               // AS Oct 11/05 - new paramater to improve performance...pass selected tag in if doing lookup for reading only...
               index4versionCheck( t4->indexFile, 0, 0 ) ;
            #endif

            #ifdef S4FOX
               if ( t4->header.descending )   /* backwards in file... */
                  pos = 1.0 - ipos ;
               else
            #endif
               pos = ipos ;

            if ( tfile4upToRoot( t4 ) < 0 )
               return -1 ;

            for(;;)
            {
               #ifdef E4PARM_LOW
                  if ( pos < 0.0 || pos > 1.0 )  /* Could be caused by rounding error ? */
                     return error4( t4->codeBase, e4parm, E81604 ) ;
               #endif

               blockOn = tfile4block( t4 ) ;

               #ifdef S4FOX
                  n = blockOn->header.nKeys ;
               #else
                  n = blockOn->nKeys+1 ;
                  if ( b4leaf( blockOn ) )
                     n-- ;
               #endif

               finalPos = (int)( n * pos ) ;
               if ( finalPos == n )
                  finalPos-- ;

               #ifdef S4FOX
                  b4go( blockOn, finalPos ) ;
               #else
                  blockOn->keyOn = finalPos ;
               #endif

               pos = pos*n - finalPos ;

               rc = tfile4down( t4 ) ;
               if ( rc < 0 )
                  return -1 ;
               if ( rc == 1 )
                  return 0 ;
            }
         }
      #else   /*  ifndef N4OTHER  */


         double tfile4positionDbl( TAG4FILE *t4 )
         {
            return tfile4position( t4 ) ;
         }



         double S4FUNCTION tfile4position( TAG4FILE *t4 )
         {
            double pos ;
            B4BLOCK *blockOn ;
            int n, min, max ;
            #ifdef S4CLIPPER
               int first = 1 ;
            #endif

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return (double)error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return (double)e4codeBase ;

            blockOn = (B4BLOCK *) t4->blocks.lastNode ;

            if ( !b4leaf(blockOn) )
               pos = 1.0 ;
            else
               pos = .5 ;

            min = max = 1 ;    /* is the position a minimal or maximal position? */

            for ( ; blockOn != 0 ; )
            {
               n = blockOn->nKeys + 1 ;
               #ifdef S4CLIPPER
                  if ( first )
                  {
                     if ( b4leaf( blockOn ) )
                        n-- ;
                     else /* if on a branch block, cannot be at min or max position */
                     {
                        min = 0 ;
                        max = 0 ;
                     }
                     first = 0 ;
                  }
               #else
                  if ( b4leaf( blockOn ) )
                     n-- ;
               #endif

               if( n == 0 )
                  max = 0 ;
               else
               {
                  if ( min == 1 )
                     if ( blockOn->keyOn != 0 )
                        min = 0 ;
                  if ( max == 1 )
                     if ( blockOn->keyOn != ( n - 1 ) )
                        max = 0 ;
                  pos = ( blockOn->keyOn + pos ) / n ;
               }

               blockOn = (B4BLOCK *) blockOn->link.p ;
               if ( blockOn == (B4BLOCK *)t4->blocks.lastNode )
                  break ;
            }
            if ( max == 1 )
               pos = 1.0 ;
            if ( min == 1 )
               pos = 0.0 ;

            #ifdef S4CLIPPER
               if ( t4->header.descending )   /* backwards in file... */
                  return 1.0 - pos ;
               else
            #endif
            return pos ;
         }



         int S4FUNCTION tfile4positionSet( TAG4FILE *t4, const double posIn )
         {
            int rc, n, finalPos ;
            B4BLOCK *blockOn ;
            double pos ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            #ifdef S4CLIPPER
               if ( t4->header.descending )   /* backwards in file... */
                  pos = 1.0 - posIn ;
               else
            #endif
            pos = posIn ;

            rc = tfile4upToRoot( t4 ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, rc, E91642 ) ;

            for(;;)
            {
               #ifdef E4PARM_LOW
                  if ( pos < 0.0 || pos > 1.0 )  /* Could be caused by rounding error ? */
                     return error4( 0, e4parm_null, E81604 ) ;
               #endif

               blockOn = tfile4block( t4 ) ;

               n = blockOn->nKeys + 1 ;
               if ( b4leaf( blockOn ) )
                  n-- ;

               finalPos = (int)( n * pos ) ;
               if ( finalPos == n )
                  finalPos-- ;

               blockOn->keyOn = finalPos ;
               pos = pos*n - finalPos ;

               rc = tfile4down( t4 ) ;
               if ( rc < 0 )
                  return -1 ;
               if ( rc == 1 )
                  return 0 ;
            }
         }
      #endif  /* #ifndef N4OTHER else */
      // AS Jul 14/09 - we can't use t4position as a direct mapping because it needs some extended functionality
      double S4FUNCTION t4position( TAG4 *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return (double)error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->tagFile->codeBase ) < 0 )
            return (double)e4codeBase ;

         if ( tfile4eof( t4->tagFile ) || ( t4->tagFile->blocks.lastNode == 0 ) )
            return 1.1 ;

         // the problem is that it is possible, if we are skipping forward through tags, that the block upwards will not have had its position updated (and may even be invalid)
         tfile4go( t4->tagFile, tfile4keyData( t4->tagFile )->value, tfile4recNo( t4->tagFile ), 0 ) ;

         return tfile4position( t4->tagFile ) ;
      }
   #endif /* S4CLIENT else */
#endif /* !S4OFF_INDEX */
