/* i4info.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

unsigned short S4FUNCTION tfile4isDescending( TAG4FILE *tag )
{
   #if defined( S4CLIENT ) || defined( S4OFF_INDEX )
      error4( tag->codeBase, e4notSupported, 0 ) ;
      return 0 ;
   #else
      #if defined( S4FOX )
         if ( tag->header.descending )
            return r4descending ;
         else
            return 0 ;
      #elif defined( S4CLIPPER )
         if ( tag->header.descending )
            return r4descending ;
         else
            return 0 ;
      #elif defined( S4MDX )
         if ( tag->header.typeCode & 8 )
            return r4descending ;
         else
            return 0 ;
      #endif
   #endif
}


// AS Jan 30/07 - exported for off-index...
int S4FUNCTION tfile4keyLenExport( TAG4FILE *tag )
{
   assert5( tag != 0 ) ;
   #ifdef S4OFF_INDEX
      return e4notSupported ;
   #else
      #ifdef S4CLIENT
         if ( tag->keyLen == 0 )
         {
            TAG4 *t4 = d4tag( tag->refData, tag->alias ) ;
            if ( t4 == 0 )
               return -1 ;
            TAG4INFO *temp = i4tagInfo( t4->index ) ;
            if ( temp == 0 )
               return -1 ;
            u4free( temp ) ;
            temp = 0 ;
         }

         assert5( tag->keyLen != 0 ) ;
         return tag->keyLen ;
      #else
         return tfile4keyLen( tag ) ;
      #endif
   #endif
}



#ifndef S4OFF_INDEX
   char *S4FUNCTION t4alias( TAG4 *t4 )
   {
      #ifdef E4VBASIC
         #ifdef S4CB51
            if ( c4parm_check( t4, 4, E40146 ) ) return 0 ;
         #else
            if ( c4parm_check( t4, 4, E91640 ) ) return 0 ;
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
         {
            error4( 0, e4parm_null, E91640 ) ;
            return 0 ;
         }
         if ( t4->tagFile == 0 )
         {
            error4( 0, e4parm, E91640 ) ;
            return 0 ;
         }
      #endif

      return t4->tagFile->alias ;
   }

   short S4FUNCTION t4descending( TAG4 *tag )
   {
      #ifndef S4CLIENT
         return tfile4isDescending( tag->tagFile ) ;
      #else
         /* CS 1999/04/22 In C/S, can't directly get
            descending info on tag so scan TAG4INFO array. */
         int rc ;
         TAG4INFO *tagInfo = i4tagInfo( tag->index );
         char *tagName = t4alias( tag ) ;

         for ( int i = 0 ; tagInfo[i].name ; i++ )
         {
            #ifdef S4CLIPPER
               rc = u4namecmp(tagName,tagInfo[i].name) ;
            #else
               #ifdef S4UNIX
                  rc = strcasecmp(tagName,tagInfo[i].name) ;
               #else
                  rc = stricmp(tagName,tagInfo[i].name) ;
               #endif
            #endif
            if ( rc == 0 )
            {
               short desc = tagInfo[i].descending ;
               u4free(tagInfo) ;
               return desc ;
            }
         }

         u4free(tagInfo) ;
         return error4( tag->index->codeBase, e4result, 0 ) ;  // The tag was not found in the array.
      #endif
   }

   /*
   #ifdef S4CLIENT
      int t4type( TAG4 *t4 )
      {
         if ( t4->tagFile->keyType != 0 )
            return t4->tagFile->keyType ;

         TAG4INFO *tagInfo = i4tagInfo( tag->index );
         char *tagName = t4alias( t4 ) ;

         for ( int i = 0 ; tagInfo[i].name ; i++ )
         {
            if ( stricmp(tagName,tagInfo[i].name) == 0 )
            {
               t4->tagFile->keyType = tagInfo[i].descending ;
               u4free(tagInfo) ;
               return t4->tagFile->keyType ;
            }
         }

         u4free(tagInfo) ;
         return error4( t4->index->codeBase, e4result, 0 ) ;  // The tag was not found in the array.
      }
   #endif
   */


   #ifdef S4CLIENT
      S4CONST char *S4FUNCTION t4exprLow( TAG4 *t4 )
      {
         #ifdef E4VBASIC
            #ifdef S4CB51
               if ( c4parm_check( t4, 4, E40148 ) ) return 0 ;
            #else
               if ( c4parm_check( t4, 4, E91641 ) ) return 0 ;
            #endif
         #endif

         #ifdef E4PARM_HIGH
            if ( t4 == 0 )
            {
               error4( 0, e4parm_null, E91641 ) ;
               return 0 ;
            }
            if ( t4->tagFile == 0 )
            {
               error4( 0, e4parm, E91641 ) ;
               return 0 ;
            }
         #endif

         if ( t4->tagFile->exprPtr == 0 )
         {
            TAG4INFO *temp = i4tagInfo( t4->index ) ;
            if ( temp == 0 )
               return 0 ;
            u4free( temp ) ;
            temp = 0 ;
            #ifdef E4ANALYZE
               if ( t4->tagFile->exprPtr == 0 )
               {
                  error4( 0, e4info, E91641 ) ;
                  return 0 ;
               }
            #endif
         }

         return t4->tagFile->exprPtr ;
      }


      int S4FUNCTION t4keyLenExported( TAG4 *tag )
      {
         assert5( tag != 0 ) ;

         if ( tag->tagFile->keyLen == 0 )
         {
            TAG4INFO *temp = i4tagInfo( tag->index ) ;
            if ( temp == 0 )
               return -1 ;
            u4free( temp ) ;
            temp = 0 ;
         }

         assert5( tag->tagFile->keyLen != 0 ) ;
         return tag->tagFile->keyLen ;
      }
   #endif /* S4CLIENT */

   #ifdef S4CLIENT
      extern unsigned short f4memoNullChar ;

      S4CONST char *S4FUNCTION t4filterLow( TAG4 *t4 )
      {
         TAG4INFO *temp ;

         #ifdef E4VBASIC
            #ifdef S4CB51
               if ( c4parm_check( t4, 4, E40149 ) )
                  return 0 ;
            #else
               if ( c4parm_check( t4, 4, E91641 ) )
                  return 0 ;
            #endif
         #endif

         #ifdef E4PARM_HIGH
            if ( t4 == 0 )
            {
               error4( 0, e4parm_null, E91641 ) ;
               return 0 ;
            }

            if ( t4->tagFile == 0 )
            {
               error4( 0, e4parm, E91641 ) ;
               return 0 ;
            }
         #endif

         if ( t4->tagFile->filterPtr == 0 )
         {
            temp = i4tagInfo( t4->index ) ;
            if ( temp == 0 )
            {
               error4( 0, e4memory, E91641 ) ;
               return 0 ;
            }
            u4free( temp ) ;
            temp = 0 ;
         }

         if ( t4->tagFile->filterPtr == 0 )
            return (char *)(&f4memoNullChar) ;   /* pointer to empty string */
         else
            return t4->tagFile->filterPtr ;
      }
   #endif /* S4CLIENT */


   #ifdef S4CLIENT
      TAG4INFO *S4FUNCTION i4tagInfo( INDEX4 *i4 )
      {
         int rc, offset, len ;
         unsigned int i ;

         #ifdef E4PARM_HIGH
            if ( i4 == 0 )
            {
               error4( 0, e4parm_null, E95501 ) ;
               return 0 ;
            }
         #endif

         CODE4 *c4 = i4->codeBase ;
         if ( error4code( c4 ) < 0 )
            return 0 ;

         CONNECTION4 *connection = i4->data->dataFile->connection ;
         connection4assign( connection, CON4INDEX_INFO, data4clientId( i4->data ), data4serverId( i4->data ) ) ;
         connection4addData( connection, i4->indexFile->accessName, strlen( i4->indexFile->accessName ) + 1, NULL ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
         {
            #ifdef E4STACK
               error4stack( c4, rc, E95501 ) ;
            #endif
            return 0 ;
         }

         rc = connection4status( connection ) ;
         if ( rc < 0 )
         {
            connection4error( connection, c4, rc, E95501 ) ;
            return 0 ;
         }

         if ( connection4len( connection ) < sizeof( CONNECTION4INDEX_INFO_OUT ) )
         {
            error4( c4, e4packetLen, E95501 ) ;
            return 0 ;
         }

         CONNECTION4INDEX_INFO_OUT *out = (CONNECTION4INDEX_INFO_OUT *)connection4data( connection ) ;
         out->numTags = ntohs5(out->numTags) ;
         if ( connection4len( connection ) < (long)(sizeof( CONNECTION4INDEX_INFO_OUT ) + (unsigned long)(out->numTags * sizeof( CONNECTION4TAG_INFO_FOR_I4INFO )) ))
         {
            error4( c4, e4packetLen, E95501 ) ;
            return 0 ;
         }

         assert5( sizeof( CONNECTION4TAG_INFO ) == sizeof( TAG4INFO ) ) ;
         CONNECTION4TAG_INFO *tagInfo = (CONNECTION4TAG_INFO *)u4allocFree( c4, ( out->numTags + 1 ) * sizeof( CONNECTION4TAG_INFO ) ) ;
         if ( tagInfo == 0 )
            return 0 ;

         offset = sizeof( CONNECTION4INDEX_INFO_OUT ) ;
         for ( i = 0 ; i < out->numTags ; i++ )
         {
            CONNECTION4TAG_INFO_FOR_I4INFO *tagInfoFromServer = ( CONNECTION4TAG_INFO_FOR_I4INFO *)((char *)out + offset) ;
            tagInfo[i].name.ptr = (char *)out + ntohs5(tagInfoFromServer->name.offset) ;
            tagInfo[i].expression.ptr = (char *)out + ntohs5( tagInfoFromServer->expression.offset ) ;
            offset += ( sizeof( CONNECTION4TAG_INFO_FOR_I4INFO ) + strlen( tagInfo[i].name.ptr )
                      + strlen( tagInfo[i].expression.ptr ) + 2 ) ;
            if ( tagInfoFromServer->filter.offset == 0 )
               tagInfo[i].filter.ptr = 0 ;
            else
            {
               tagInfo[i].filter.ptr = (char *)out + ntohs5(tagInfoFromServer->filter.offset) ;
               offset += ( strlen( tagInfo[i].filter.ptr ) + 1 ) ;
            }
            tagInfo[i].unique = ntohs5( tagInfoFromServer->unique ) ;
            tagInfo[i].descending = ntohs5( tagInfoFromServer->descending ) ;
            TAG4FILE *tagFile = d4tag( i4->data, tagInfo[i].name.ptr )->tagFile ;
            if ( tagFile == 0 )   /* just skip this tag */
            {
               out->numTags-- ;
               i-- ;
               continue ;
            }
            tagInfo[i].name.ptr = tagFile->alias ;   /* remove pointer to communication memory, and point to tag name */
            tagFile->keyLen = ntohs5( tagInfoFromServer->keyLen ) ;
            if ( tagFile->exprPtr == 0 )
            {
               len = strlen( tagInfo[i].expression.ptr ) ;
               if ( len != 0 )
               {
                  tagFile->exprPtr = (char *)u4allocFree( c4, len + 1 ) ;
                  if ( len == 0 )
                  {
                     error4( c4, e4memory, E95501 ) ;
                     u4free( tagInfo ) ;
                     return 0 ;
                  }
                  memcpy( tagFile->exprPtr, tagInfo[i].expression.ptr, len ) ;
                  tagFile->exprPtr[len] = 0 ;
                  tagInfo[i].expression.ptr = tagFile->exprPtr ;
               }
            }
            if ( tagFile->filterPtr == 0 )
            {
               if ( tagInfo[i].filter.ptr != 0 )
               {
                  len = strlen( tagInfo[i].filter.ptr ) ;
                  if ( len != 0 )
                  {
                     tagFile->filterPtr = (char *)u4allocFree( c4, len + 1 ) ;
                     if ( len == 0 )
                     {
                        error4( c4, e4memory, E95501 ) ;
                        u4free( tagInfo ) ;
                        return 0 ;
                     }
                     memcpy( tagFile->filterPtr, tagInfo[i].filter.ptr, len ) ;
                     tagFile->filterPtr[len] = 0 ;
                     tagInfo[i].filter.ptr = tagFile->filterPtr ;
                  }
               }
            }

         }

         return (TAG4INFO *)tagInfo ;
      }
   #endif



   #ifndef S4CLIENT
      TAG4INFO *S4FUNCTION i4tagInfo( INDEX4 *i4 )
      {
         #ifdef E4PARM_HIGH
            if ( i4 == 0 )
            {
               error4( 0, e4parm_null, E95501 ) ;
               return 0 ;
            }
         #endif

         if ( error4code( i4->codeBase ) < 0 )
            return 0 ;

         int numTags = 0 ;
         TAG4 *tagOn ;
         for( tagOn = 0 ;; )
         {
            tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
            if ( tagOn == 0 )
               break ;
            numTags++ ;
         }

         if ( error4code( i4->codeBase ) < 0 )
            return 0 ;
         TAG4INFO *tagInfo = (TAG4INFO *)u4allocFree( i4->codeBase, ( (long)numTags + 1L ) * sizeof( TAG4INFO ) ) ;
         if ( tagInfo == 0 )
            return 0 ;

         int i ;

         for( tagOn = 0, i = 0 ;; i++ )
         {
            tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
            if ( tagOn == 0 )
               return ( tagInfo ) ;
            tagInfo[i].name = tfile4alias( tagOn->tagFile ) ;
            tagInfo[i].expression = expr4source( tagOn->tagFile->expr ) ;
            tagInfo[i].filter = expr4source( tagOn->tagFile->filter ) ;
            tagInfo[i].unique = t4unique( tagOn ) ;
            tagInfo[i].descending = tfile4isDescending( tagOn->tagFile ) ;
         }
      }
   #endif  /* S4CLIENT */
#endif  /* S4OFF_INDEX */
