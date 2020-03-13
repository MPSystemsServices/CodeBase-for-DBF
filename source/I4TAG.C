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

/* i4tag.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef I4PRINT
   #include <sys\timeb.h>
   #include <time.h>
#endif

/* AS 06/22/99 -- needed for ole-db dll... */
#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   // client case resides in i4info.c
   int S4FUNCTION t4keyLenExported( TAG4 *tag )
   {
      return tag->tagFile->header.keyLen ;
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */


TAG4FILE *S4FUNCTION t4getTagFile( TAG4 *tag )
{
   if ( tag == 0 )
      return 0 ;

   // CS 2009/03/03 Return pointer in client/server.
   #ifdef S4OFF_INDEX
      return 0 ;
   #else
      return tag->tagFile ;
   #endif
}


#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   Collate4name S4FUNCTION t4getCollateName( TAG4 *tag )
   {
      #ifdef S4FOX
         return tag->tagFile->collateName ;
      #else
         return collate4none ;
      #endif
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */



#if !defined( S4OFF_INDEX )
   long S4FUNCTION tfile4dskipExport( TAG4FILE *t4, long numSkip )
   {
      // The purpose of this function is to help with skipping when a tag is flagged as descending.
      // This is because physically with FoxPro compatibility, for example, the tag is not actually in descending
      // order.  So when we call d4skip to say 'skip 1 forward' if a tag is descending we actually must skip
      // backwards.  But because dBase compatibility it is physically stored in descending order, in that case we
      // don't want to perform a backwards skip but just the regular skip.
      // Therefore, this function performs a consistent and correct low level tag skip based on the tag type (eg. otherwise
      // if you called tfile4skip() on a descending tag you would actually end up moving the wrong way)
      #ifdef S4CLIENT
         if ( code4indexFormat( t4->codeBase ) == r4mdx )  // is stored physically correct, just perform a normal skip
            return tfile4skip( t4, numSkip ) ;
         else // otherwise we must request a tfile4dskip
            return tfile4skipDo( t4, numSkip, 1 ) ;
      #else
         #ifdef S4HAS_DESCENDING
            if ( t4->header.descending )
               return -tfile4skip( t4, -numSkip ) ;  // is physically descending
            else
               return tfile4skip( t4, numSkip ) ;
         #else
            return tfile4skip( t4, numSkip ) ;
         #endif
      #endif
   }
#endif /* !defined( S4OFF_INDEX ) */

int S4FUNCTION tfile4exprKeyExport( TAG4FILE *tag, unsigned char **ptrPtr )
{
   // export for ODBC
   // AS Oct 25/05 - new function available now...
   #if defined( S4OFF_INDEX )
      return -1 ;   // not available in client/server
   #else
      return tfile4exprKey( tag, ptrPtr ) ;
   #endif
}


#ifndef S4OFF_INDEX
   #ifndef S4SERVER
      int S4FUNCTION t4uniqueSetLow( TAG4 *, const short, const char ) ;
   #endif /* !S4SERVER */
#endif

#if !defined( S4OFF_INDEX ) && !defined( S4CLIENT )
   /* !S4OFF_INDEX, !S4CLIENT */
   int i4readBlock( FILE4 *file, const B4NODE blockNo, B4BLOCK *parent, B4BLOCK *b4 )
   {
      /* uses parent to determine if an inconsistancy has arisen */
      /* return <0 = error, 0 = success, 1 = inconsistant */

      #ifdef E4PARM_LOW
         if ( file == 0 || b4nodeInvalid( blockNo ) || b4 == 0 )
            return error4( 0, e4parm, E91604 ) ;
      #endif

      TAG4FILE *tag = b4->tag ;
      #ifndef S4CLIPPER
         INDEX4FILE *i4file = tag->indexFile ;
      #endif
      CODE4 *c4 = tag->codeBase ;
      #ifdef S4BYTE_SWAP
         char *swapPtr ;
      #endif

      #ifdef S4MDX
         /* i.e. if ndx or mdx */
         #ifndef S4OFF_OPTIMIZE
            i4file->readBlockTag = tag ;
         #endif

         FILE4LONG pos ;
         // AS Nov 5/03 - support for large files
         file4longAssign( pos, I4MULTIPLY, 0 ) ;
         file4longMultiply( pos, blockNo ) ;
         int amtRead = file4readAllInternal( file, pos, &b4->nKeys, i4file->header.blockRw ) ;

         #ifndef S4OFF_OPTIMIZE
            i4file->readBlockTag = 0 ;
         #endif

         if ( amtRead < 0 )
            return -1 ;

         #ifdef S4BYTE_SWAP
            /* swap the numKeys value */
            b4->nKeys = x4reverseShort( (void *)&b4->nKeys ) ;

            /* position swapPtr at beginning of B4KEY's */
            swapPtr = (char *)&b4->nKeys ;
            swapPtr += 6 + sizeof(short) ;

            /* move through all B4KEY's to swap 'long' */
            for ( int i = 0 ; i < (int)b4numKeys( b4 ) ; i++ )
            {
               S4LONG longVal = x4reverseLong( (void *)swapPtr ) ;
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               swapPtr += tag->header.groupLen ;
            }

            /* mark lastPointer */
            if ( !b4leaf( b4 ) )
            {
               S4LONG longVal = x4reverseLong( (void *)swapPtr ) ;
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            }
         #endif  /* S4BYTE_SWAP */
      #endif

      int rc = 0 ;

      #ifdef S4FOX
         #ifndef S4OFF_OPTIMIZE
            i4file->readBlockTag = tag ;
         #endif

         FILE4LONG pos ;
         // file4longAssign( pos, I4MULTIPLY * blockNo, 0 ) ;
         b4nodeGetFilePosition( i4file, blockNo, &pos ) ;
         rc = file4readAllInternal( file, pos, &b4->header, i4blockSize( i4file ) ) ;

         #ifndef S4OFF_OPTIMIZE
            i4file->readBlockTag = 0 ;
         #endif

         if ( rc < 0 )
            return error4stack( c4, (short)rc, E91604 ) ;

         #ifdef E4DEBUG
            // 0 is always invalid - should either be ULONG_MAX or a valid value...
            if ( b4node( b4->header.leftNode ) == 0 || b4node( b4->header.rightNode ) == 0 )
               return error4( c4, e4struct, E91604 ) ;
         #endif

         #ifdef S4BYTE_SWAP
            b4->header.nodeAttribute = x4reverseShort( (void *)&b4->header.nodeAttribute ) ;
            b4->header.nKeys = x4reverseShort( (void *)&b4->header.nKeys ) ;
            b4->header.leftNode.node = x4reverseLong( (void *)&b4->header.leftNode ) ;
            b4->header.rightNode.node = x4reverseLong( (void *)&b4->header.rightNode ) ;

            /* if b4 is a leaf */
            // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
            if ( b4->header.nodeAttribute & 0x02 )
            {
               b4->nodeHdr.freeSpace = x4reverseShort( (void *)&b4->nodeHdr.freeSpace ) ;
               S4LONG longVal = x4reverseLong( (void *)&b4->nodeHdr.recNumMask[0] ) ;
               c4memcpy( (void *)&b4->nodeHdr.recNumMask[0], (void *)&longVal, sizeof(S4LONG) ) ;
            }
            else /* if b4 is a branch */
            {
               short shortVal = tag->header.keyLen + sizeof(S4LONG) ;
               /* position swapPtr to end of first key expression */
               swapPtr = (char *) &b4->nodeHdr.freeSpace + tag->header.keyLen ;

               /* move through all B4KEY's to swap 'long's */
               for ( int i = 0 ; i < (int)b4numKeys( b4 ) ; i++ )
               {
                  S4LONG longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += sizeof(S4LONG) ;
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += shortVal ;
               }
            }
         #endif

         // AS Jun 8/04 - for large key support, using a different trail byte counter
         if ( b4->header.nodeAttribute & 0x02 )      /* if b4 is a leaf */
         {
            // if the key len > 240, we need to adjust these values because the values stored in the file are only of
            // size 1 byte (fits key sizes up to 255), we need to add the missing 8 bits in this other case...
            if ( b4->tag->header.keyLen > 255 )
            {
               b4->trailByteCnt = (b4->nodeHdr.trailByteCnt << 8) + 255 ;
               b4->dupByteCnt = (b4->nodeHdr.dupByteCnt << 8) + 255  ;
            }
            else
            {
               b4->trailByteCnt = b4->nodeHdr.trailByteCnt ;
               b4->dupByteCnt = b4->nodeHdr.dupByteCnt ;
            }
         }
      #endif

      #ifdef S4CLIPPER
         #ifndef S4OFF_OPTIMIZE
            tag->readBlockTag = tag ;
         #endif

         FILE4LONG pos ;
         file4longAssign( pos, blockNo, 0 ) ;
         rc = file4readAllInternal( file, pos, &b4->nKeys, B4BLOCK_SIZE_INTERNAL ) ;

         #ifndef S4OFF_OPTIMIZE
            tag->readBlockTag = 0 ;
         #endif

         if ( rc < 0 )
            return error4stack( c4, rc, E91604 ) ;

         #ifdef S4BYTE_SWAP
            index4swapBlockClipper(&b4->nKeys, tag->header.keysMax, tag->header.groupLen ) ;
         #endif

         b4->fileBlock = blockNo / 512 ;
         assert5( b4->fileBlock != ULONG_MAX ) ;
      #endif

      if ( c4->doIndexVerify == 0 )
         return rc ;

      if ( b4numKeys( b4 ) == 0 )   /* added to free list... therefore bad */
      {
         #ifdef S4CLIPPER
            if ( tag->header.keysMax > 2 )  /* remote possibility of 0 keys if max = 2 and in a reindex mode */
               if ( tag->header.root != b4->fileBlock * I4MULTIPLY )  /* b4 has no keys but is not the root, must be a problem... */
         #else
            if ( b4nodesNotEqual( tag->header.root, blockNo ) )  /* b4 has no keys but is not the root, must be a problem... */
         #endif
         return 1 ;
      }


      // AS Nov 13/02 - do some additional checking on block...
      #if defined( S4FOX ) && defined( E4ANALYZE )
         if ( b4numKeys( b4 ) < 0 )
            return error4( c4, e4index, E81601 ) ;
         if ( (long)b4numKeys( b4 ) > (long)i4blockSize( i4file ) )
            return error4( c4, e4index, E81601 ) ;
         if ( b4leaf( b4 ) )
         {
            if ( b4->nodeHdr.freeSpace < 0 )
               return error4( c4, e4index, E81601 ) ;
            if ( (long)b4->nodeHdr.freeSpace  > (long)i4blockSize( i4file ) )
               return error4( c4, e4index, E81601 ) ;
         }
      #endif

      if ( parent != 0 )  /* check consistancy */
      {
         #ifdef S4MDX
            int eq = 0 ;
            int blkOn = b4numKeys( b4 ) - 1 ;
            int parOn ;

            if ( parent->keyOn >= b4numKeys( parent ) )
            {
               parOn = b4numKeys( parent ) - 1 ;
               eq = 1 ;
            }
            else
               parOn = parent->keyOn ;

            if ( !b4leaf( b4 ) )
            {
               blkOn = 0 ;
               if ( eq != 1 )
               {
                  if ( parOn == 0 )
                     eq = 2 ;
                  else
                  {
                     eq = 1 ;
                     parOn-- ;
                  }
               }
            }

            switch( eq )
            {
               case 0:
                  if ( tag->cmp( b4keyKey( parent, parOn ), b4keyKey( b4, blkOn ), tag->header.keyLen  ) != 0 )
                     rc = 1 ;
                  break ;
               case 1:
                  if ( tag->cmp( b4keyKey( parent, parOn ), b4keyKey( b4, blkOn ), tag->header.keyLen  ) > 0 )
                  {
                     #if defined( E4ANALYZE ) && defined( S4TESTING )
                        unsigned char *key1 = b4keyKey( parent, parOn ) ;
                        unsigned char *key2 = b4keyKey( b4, blkOn ) ;
                        rc = 1 ;
                     #endif
                  }
                  break ;
               case 2:
                  if ( tag->cmp( b4keyKey( parent, parOn ), b4keyKey( b4, blkOn ), tag->header.keyLen  ) < 0 )
                     rc = 1 ;
                  break ;
               #ifdef E4ANALYZE
                  default:
                     return error4( c4, e4index, E81601 ) ;
               #endif
            }
         #endif

         #ifdef S4FOX
            if ( rc == 0 )
               if ( b4recNo( parent, parent->keyOn) != b4recNo( b4, b4numKeys( b4 ) - 1 ) )
               {
                  rc = 1 ;
                  #ifdef E4ANALYZE
                     // to help in debugger debugging, get the recno's out...
                     long parentRecNo = b4recNo( parent, parent->keyOn) ;
                     long blockRecNo = b4recNo( b4, b4numKeys( b4 ) - 1 ) ;
                     long blockRecNo2 = b4recNo( b4, b4numKeys( b4 ) - 2 ) ;
                     #ifdef I4PRINT
                        char dump[255] ;
                        sprintf( dump, "i4readBlock corruption detection, recno mismatch between parent and child for parent blockNo: %ld, parent recNo: %ld, block blockNo: %ld, block recNo:%ld, block Recno2:%ld\r\n",
                                (long)b4node(parent->fileBlock), parentRecNo, (long)b4node(blockNo), blockRecNo, blockRecNo2 ) ;
                        log5( dump ) ;
                        sprintf( dump, "block numKeys: %ld\r\n", b4numKeys( b4 ) ) ;
                        log5( dump ) ;
                     #endif
                  #endif
               }
         #endif
         if ( rc == 0 )
         {
            #ifdef E4INDEX_VERIFY
               if ( b4verify( b4 ) == -1 )
                  return error4describe( c4, e4index, E91604, tag->alias, 0, 0 ) ;
               if ( b4verify( parent ) == -1 )
                  return error4describe( c4, e4index, E91604, tag->alias, 0, 0 ) ;
            #endif
         }
         #ifdef S4CLIPPER
            if ( rc == 0 )
            {
               if ( parent->keyOn < b4numKeys( parent ) )
               {
                  if ( tag->cmp( b4keyKey( parent, parent->keyOn ), b4keyKey( b4, b4numKeys( b4 ) - 1 ), tag->header.keyLen  ) < 0 )
                     rc = 1 ;
               }
               else
               {
                  if ( b4numKeys( b4 ) != 0 && b4numKeys( parent ) != 0 )   /* again, special instance whereby no key to compare with, so assume correct */
                     if ( tag->cmp( b4keyKey( parent, parent->keyOn - 1 ), b4keyKey( b4, 0 ), tag->header.keyLen  ) > 0 )
                        rc = 1 ;
               }
            }
         #endif
      }

      if ( rc == 1 )
      {
         #ifdef S4CLIPPER
            if ( tfile4lockTest( tag ) == 1 )  /* corrupt */
         #else
            if ( index4lockTest( i4file ) == 1 )  /* corrupt */
         #endif
            return error4describe( c4, e4index, E81607, tag->alias, 0, 0 ) ;
      }

      #ifdef I4PRINT
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            B4KEY_DATA *keyData = b4key( b4, b4numKeys(b4)-1 ) ;
            sprintf( dump, "***i4read block successful: tag %s, blockNo = %ld last key recNo = %ld, numKeys = %ld\r\n", tag->alias, (long)b4node(blockNo), (long)keyData->num, (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      return rc ;
      /* int i4readBlock( FILE4 *file, const long blockNo, B4BLOCK *parent, B4BLOCK *b4 ) */
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) */


#if !defined( S4OFF_INDEX ) && !defined( S4CLIENT )
   #if !defined( S4CLIPPER ) && defined( E4ANALYZE )
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, E4ANALYZE */
      B4BLOCK *S4FUNCTION tfile4block( TAG4FILE *t4 )
      {
         /* for debug purposes only, else is an inline function */
         if ( t4 == 0 )
         {
            error4( 0, e4parm_null, E91642 ) ;
            return 0 ;
         }
         if ( t4->blocks.lastNode == 0 )
         {
            error4( t4->codeBase, e4index, E91642 ) ;
            return 0 ;
         }
         return (B4BLOCK *)t4->blocks.lastNode ;
      }
   #endif
#endif /* !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX )
   #ifdef S4CLIENT
      /* !S4OFF_INDEX, S4CLIENT */
      int S4FUNCTION tfile4bottom( TAG4FILE *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         CODE4 *c4 = t4->codeBase ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         CONNECTION4 *connection = &c4->defaultServer ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
               return error4( c4, e4struct, E91642 ) ;
         #endif

         t4->tagDataValid = 0 ;  // reset to invalid
         tfile4cacheReset( t4 ) ;
         int rc = connection4assign( connection, CON4TAG_BOTTOM, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;

         CONNECTION4TAG_BOTTOM_INFO_IN *infoIn ;
         CONNECTION4TAG_BOTTOM_INFO_OUT *out ;
         connection4addData( connection, NULL, sizeof(CONNECTION4TAG_BOTTOM_INFO_IN), (void **)&infoIn ) ;
         memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
         infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
         u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         rc = connection4sendMessage( connection ) ;
         if ( rc == 0 )
            rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( data->codeBase, rc, E91642 ) ;
         int saveRc = connection4status( connection ) ;
         if ( saveRc < 0 )
            return connection4error( connection, c4, saveRc, E91642 ) ;
         out = (CONNECTION4TAG_BOTTOM_INFO_OUT *)connection4data( connection ) ;
         int conLen = connection4len( connection ) ;
         if ( conLen < sizeof( CONNECTION4TAG_BOTTOM_INFO_OUT ) )
            return error4( c4, e4packetLen, E91642 ) ;
         t4->recNo = ntohl5(out->recNo) ;

         // AS Feb 9/09 - added support to return key as well
         t4->currentKeyLen = (unsigned long)(ntohl5(out->keyLen)) ;
         if ( conLen != sizeof( CONNECTION4TAG_BOTTOM_INFO_OUT ) + t4->currentKeyLen )
            return error4( c4, e4packetLen, E94701 ) ;
         if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
         {
            if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E91642 ) ;
               #endif
               return 0 ;
            }
         }
         memcpy( t4->currentKey, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_BOTTOM_INFO_OUT), t4->currentKeyLen ) ;

         t4->tagDataValid = 1 ;

         return saveRc ;
      }
   #else
      #ifdef S4FOX
         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         int S4FUNCTION tfile4bottom( TAG4FILE *t4 )
         {
            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            if ( t4->header.descending )   /* if descending, go bottom means go top */
               return tfile4rlTop( t4 ) ;
            else
               return tfile4rlBottom( t4 ) ;
         }
      #endif /* S4FOX */



      #ifdef S4MDX
         /* !S4OFF_INDEX, !S4CLIENT, S4MDX */
         int S4FUNCTION tfile4bottom( TAG4FILE *t4 )
         {
            int rc, rc2 ;
            B4BLOCK *blockOn ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            rc = 2 ;

            while ( rc == 2 )
            {
               rc = tfile4upToRoot( t4 ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, rc, E91642 ) ;

               if ( rc != 2 )
               {
                  b4goEof( tfile4block(t4) ) ;
                  do
                  {
                     rc = tfile4down( t4 ) ;
                     if ( rc < 0 )
                        return error4stack( t4->codeBase, rc, E91642 ) ;
                     b4goEof( tfile4block( t4 ) ) ;
                  } while ( rc == 0 ) ;
               }

               if ( rc == 2 )   /* failed due to read while locked */
               {
                  rc2 = tfile4outOfDate( t4 ) ;
                  if ( rc2 < 0 )
                     return error4stack( t4->codeBase, rc2, E91642 ) ;
               }
            }

            blockOn = tfile4block( t4 ) ;
            #ifdef E4ANALYZE
               if ( blockOn == 0 )
                  return error4( t4->codeBase, e4result, E91642 ) ;
            #endif

            if ( blockOn->keyOn > 0 )
               blockOn->keyOn = b4numKeys( blockOn ) - 1 ;

            return 0 ;
         }
      #endif /* S4MDX */
   #endif /* defined( S4CLIENT ) else */
#endif /* !defined( S4OFF_INDEX ) */



#if !defined( S4OFF_INDEX ) && !defined( S4CLIENT )
   #if !defined( S4OFF_WRITE ) && defined( S4FOX )
      /* !S4OFF_INDEX, !S4CLIENT, !S4OFF_WRITE, S4FOX */
      int tfile4branchSplit( TAG4FILE *t4, B4BLOCK *oldBlock, B4BLOCK *newBlock )
      {
         int newLen, nNewKeys ;
         int gLen = t4->header.keyLen + 2*sizeof(S4LONG) ;
         char *oPos ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 || oldBlock == 0 || newBlock == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         /* NNNNOOOO  N - New, O - Old */
         nNewKeys = ( b4numKeys( oldBlock ) + 1 ) / 2 ;
         if ( oldBlock->keyOn > nNewKeys )
            nNewKeys-- ;
         newBlock->header.nKeys = (short)nNewKeys ;
         oldBlock->header.nKeys -= (short)nNewKeys ;

         newLen = b4numKeys( newBlock ) * gLen ;

         oPos = ((char *)&oldBlock->nodeHdr) + gLen * b4numKeys( oldBlock ) ;
         c4memcpy( (void *)&newBlock->nodeHdr, oPos, (unsigned int)newLen ) ;
         newBlock->header.nodeAttribute = 0 ;
         oldBlock->header.nodeAttribute = 0 ;
         newBlock->keyOn = oldBlock->keyOn - b4numKeys( oldBlock ) ;

         /* clear the old data */
         c4memset( oPos, 0, (unsigned int)newLen ) ;

         return 0 ;
      }
   #endif



   #ifdef S4MDX
      /* !S4OFF_INDEX, !S4CLIENT, S4MDX */
      void S4FUNCTION tfile4descending( TAG4FILE *tag, const unsigned short int setting )
      {
      } /* A do nothing function to facilitate index independent OLEDB dll*/
   #endif /* S4MDX */



   #if defined( S4FOX ) && defined( S4HAS_DESCENDING )
      /* !S4OFF_INDEX, !S4CLIENT, S4FOX, S4HAS_DESCENDING */
      void S4FUNCTION tfile4descending( TAG4FILE *tag, const unsigned short int setting )
      {
         #ifdef E4PARM_LOW
            if ( tag == 0 )
            {
               error4( 0, e4parm_null, E91642 ) ;
               return ;
            }
         #endif

         tag->header.descending = setting ;
      }
   #endif



   #ifndef S4CLIPPER
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4down( TAG4FILE *t4 )
      {
         /* Returns  2 - cannot go down due to out of date blocks 1 - Cannot move down; 0 - Success; -1 Error */
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         INDEX4FILE *i4 = t4->indexFile ;
         B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
         B4NODE blockDown, nextBlockDown ;
         b4nodeSetInvalid( &blockDown ) ;
         b4nodeSetInvalid( &nextBlockDown ) ;

         B4BLOCK *popBlock, *newBlock, *parent ;
         int rc ;

         if ( blockOn == 0 )    /* Read the root block */
         {
            if ( root4needsReading( t4->header.root ) )
            {
               FILE4LONG pos ;
               b4nodeGetFilePosition( i4, t4->headerOffset, &pos ) ;
               rc = file4readAllInternal( &i4->file, pos, &t4->header.root, sizeof( t4->header.root ) ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;
               #ifdef S4BYTE_SWAP
                  t4->header.root.node = x4reverseLong( (void *)&t4->header.root ) ;
               #endif
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "tfile4down read root: Tag: %s, i4file = %ld, root = %ld\r\n", t4->alias, (long)t4->indexFile, (long)b4node(t4->header.root) ) ;
                     log5( dump ) ;
                  }
               #endif
            }
            b4nodeAssignNode( &blockDown, t4->header.root ) ;
         }
         else
         {
            if ( b4leaf( blockOn ) )
               return 1 ;
            #ifdef S4FOX
               #ifdef S4DATA_ALIGN
                  c4memcpy( (void *)&blockDown, (void *)(((unsigned char *)&blockOn->nodeHdr)
                          + (blockOn->keyOn+1)*(2*sizeof(S4LONG) + t4->header.keyLen) - sizeof(S4LONG)), sizeof(S4LONG) ) ;
                  blockDown.node = x4reverseLong( (void *)&blockDown ) ;   /* LY 00/07/11 : from blockDown to blockDown.node */

               #else
                  b4nodeAssignLong( &blockDown, (unsigned long)x4reverseLong( (void *)( ((unsigned char *)&blockOn->nodeHdr)
                          + ( blockOn->keyOn + 1 ) * ( 2 * sizeof( S4LONG ) + t4->header.keyLen ) - sizeof( S4LONG ) ) ) ) ;
               #endif
               #ifdef S4ADVANCE_READ
                  /* AS 02/17/99 --> keyOn of 1 means on key '2', this means that if # keys == 1
                     and keyOn is 1, we are saying it is ok to look at entry '3', but in fact there
                     is no entry 3, so we should not be doings this.  Therefore, changed to
                     blockOn->keyOn + 2
                     if ( b4numKeys( blockOn ) >= (blockOn->keyOn + 1) )
                     */
                  if ( b4numKeys( blockOn ) >= (blockOn->keyOn + 2) )
                  {
                     #ifdef S4DATA_ALIGN
                        c4memcpy( (void *)&nextBlockDown,(void *)(((unsigned char *)&blockOn->nodeHdr)
                                + (blockOn->keyOn+2)*(2*sizeof(S4LONG) + t4->header.keyLen) - sizeof(S4LONG)), sizeof(S4LONG) ) ;
                        nextBlockDown.node = x4reverseLong( (void *)&blockDown ) ;  /* LY 00/10/31 : added .node */
                     #else
                        b4nodeAssignLong( &nextBlockDown, x4reverseLong( (void *)( ( (unsigned char *)&blockOn->nodeHdr )
                                + ( blockOn->keyOn + 2 ) * ( 2 * sizeof( S4LONG ) + t4->header.keyLen ) - sizeof( S4LONG ) ) ) ) ;
                     #endif
                  }
                  else
                     b4nodeSetInvalid( &nextBlockDown ) ;
               #endif
            #else
               blockDown = b4key(blockOn,blockOn->keyOn)->num ;
               #ifdef S4ADVANCE_READ
                  if ( b4numKeys( blockOn ) >= (blockOn->keyOn + 1) )
                     nextBlockDown = b4key(blockOn,blockOn->keyOn+1)->num ;
                  else
                     b4nodeSetInvalid( &nextBlockDown ) ;
               #endif
            #endif
            #ifdef E4ANALYZE
               if ( b4nodeInvalid( blockDown ) || b4node( blockDown ) == 0 )
                  return error4( t4->codeBase, e4index, E81602 ) ;
            #endif
         }

         /* Get memory for the new block */
         popBlock = (B4BLOCK *)l4pop( &t4->saved ) ;
         if ( popBlock == 0 )
         {
            newBlock = b4alloc( t4, blockDown) ;
            if ( newBlock == 0 )
               return error4stack( t4->codeBase, e4memory, E91642 ) ;
         }
         else
            newBlock = popBlock ;

         parent = (B4BLOCK *)l4last( &t4->blocks ) ;
         l4add( &t4->blocks, newBlock ) ;

         if ( popBlock == 0 || b4nodesNotEqual( newBlock->fileBlock, blockDown ) )
         {
            #ifndef S4OFF_WRITE
               rc = b4flush( newBlock ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;
            #endif

            rc = i4readBlock( &i4->file, blockDown, parent, newBlock ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;

            #if !defined( S4OFF_OPTIMIZE ) && defined( S4ADVANCE_READ )
               if ( b4nodeValid( nextBlockDown ) && ( i4->dataFile->hiPrio == -2 ) && ( b4leaf( newBlock ) ) )
               {
                  FILE4LONG filePos ;
                  b4nodeGetFilePosition( i4, nextBlockDown, &filePos ) ;
                  // AS Apr 13/04 - support for optimizing loarge files
                  // if ( file4longGetHi( filePos ) == 0 )  // only advance-read if < 4 gigs...
                  opt4advanceReadBuf( &i4->file, filePos, i4blockSize( i4 ) ) ;
               }
            #endif

            if ( rc == 1 )
            {
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "i4readBlock() failed for block %ld\r\n", (long)b4node( blockDown) ) ;
                     log5( dump ) ;
                  }
               #endif
               l4remove( &t4->blocks, newBlock ) ;
               l4add( &t4->saved, newBlock ) ;
               return 2 ;
            }

            b4nodeAssignNode( &newBlock->fileBlock, blockDown ) ;
            assert5( b4nodeValid( newBlock->fileBlock ) ) ;

            #ifdef S4FOX
               newBlock->builtOn = -1 ;
            #endif

            /* flush blocks, don't delete */
            for( blockOn = 0 ;; )
            {
               blockOn = (B4BLOCK *)l4next(&t4->saved,blockOn) ;
               if ( blockOn == 0 )
                  break ;
               #ifndef S4OFF_WRITE
                  rc = b4flush( blockOn ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;
               #endif
               b4nodeSetInvalid( &blockOn->fileBlock ) ;   /* make it invalid */
            }
         }

         #ifdef S4FOX
            return b4top( newBlock ) ;
         #else
            newBlock->keyOn = 0 ;
            return 0 ;
         #endif
      }
   #endif /* !S4CLIPPER */
#endif /* !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX )
   /* !S4OFF_INDEX */
   int S4FUNCTION tfile4eof( TAG4FILE *t4 )
   {
      #ifdef E4PARM_LOW
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91642 ) ;
      #endif

      #ifdef S4CLIENT
         if ( t4->tagDataValid == 1 )
         {
            if ( t4->recNo == -1 ) // means eof
               return 1 ;
            else // not eof
               return 0 ;
         }
         CODE4 *c4 = t4->codeBase ;
         CONNECTION4 *connection = &c4->defaultServer ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
               return error4( c4, e4struct, E91642 ) ;
         #endif

         int rc = connection4assign( connection, CON4TAG_EOF, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;

         CONNECTION4TAG_EOF_INFO_IN *infoIn ;
         CONNECTION4TAG_EOF_INFO_OUT *out ;
         connection4addData( connection, NULL, sizeof(CONNECTION4TAG_EOF_INFO_IN), (void **)&infoIn ) ;
         memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
         infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
         u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         rc = connection4sendMessage( connection ) ;
         if ( rc == 0 )
            rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( data->codeBase, rc, E91642 ) ;
         int saveRc = connection4status( connection ) ;
         if ( saveRc < 0 )
            return connection4error( connection, c4, saveRc, E91642 ) ;
         int conLen = connection4len( connection ) ;
         if ( conLen != sizeof( CONNECTION4TAG_EOF_INFO_OUT ) )
            return error4( c4, e4packetLen, E91642 ) ;
         out = (CONNECTION4TAG_EOF_INFO_OUT *)connection4data( connection ) ;

         int isEof = out->isEof ? 1 : 0 ;
         if ( t4->tagDataValid == 0 && isEof )  // we are now 'validly' at eof
         {
            t4->tagDataValid = 1 ;
            t4->recNo = -1 ;  // this will also mark the keydata so it return null
         }
         return isEof ;
      #else
         B4BLOCK *b4 ;

         if ( t4->blocks.lastNode == 0 )   // if tag not set to anywhere, it is not at eof (it is nowhere)
            return 0 ;

         b4 = tfile4block( t4 ) ;

         #ifdef E4ANALYZE
            if ( b4 == 0 )
               return e4result ;
         #endif

         #if defined( S4FOX )
            return( (b4->keyOn >= b4numKeys( b4 ) ) || (b4numKeys( b4 ) == 0) ) ;
         #elif defined( S4MDX )
            return( b4->keyOn >= b4numKeys( b4 ) ) ;
         #elif defined( S4CLIPPER )
            return ( b4->keyOn >= b4->nKeys ) ;
         #endif
      #endif
   }



   /* !S4OFF_INDEX */
   long S4FUNCTION tfile4count( TAG4FILE *t4 )
   {
      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91642 ) ;
      #endif

      #ifdef S4CLIENT
         CODE4 *c4 = t4->codeBase ;
         CONNECTION4 *connection = &c4->defaultServer ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
               return error4( c4, e4struct, E91642 ) ;
         #endif

         t4->tagDataValid = 0 ;  // we reposition as part of the count operation
         int rc = connection4assign( connection, CON4TAG_COUNT, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;

         CONNECTION4TAG_COUNT_INFO_IN *infoIn ;
         CONNECTION4TAG_COUNT_INFO_OUT *out ;
         connection4addData( connection, NULL, sizeof(CONNECTION4TAG_COUNT_INFO_IN), (void **)&infoIn ) ;
         memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
         infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
         u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         rc = connection4sendMessage( connection ) ;
         if ( rc == 0 )
            rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( data->codeBase, rc, E91642 ) ;
         int saveRc = connection4status( connection ) ;
         if ( saveRc < 0 )
            return connection4error( connection, c4, saveRc, E91642 ) ;
         int conLen = connection4len( connection ) ;
         if ( conLen < sizeof( CONNECTION4TAG_COUNT_INFO_OUT ) )
            return error4( c4, e4packetLen, E91642 ) ;
         out = (CONNECTION4TAG_COUNT_INFO_OUT *)connection4data( connection ) ;
         t4->recNo = ntohl5(out->recNo) ;

         // AS Feb 9/09 - added support to return key as well
         t4->currentKeyLen = (unsigned long)(ntohl5(out->keyLen)) ;
         if ( conLen != sizeof( CONNECTION4TAG_COUNT_INFO_OUT ) + t4->currentKeyLen )
            return error4( c4, e4packetLen, E91642 ) ;
         if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
         {
            if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E91642 ) ;
               #endif
               return 0 ;
            }
         }
         memcpy( t4->currentKey, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_COUNT_INFO_OUT), t4->currentKeyLen ) ;

         t4->tagDataValid = 1 ;

         return ntohl5( out->count ) ;
      #else
         long count = 0 ;
         int rc = tfile4top( t4 ) ;
         if ( rc != 0 || tfile4eof( t4 ) )  // in case no records exist...
         {
            if ( rc < 0 )
               return rc ;
            return count ;
         }

         count++ ;  // include the 1st record we are on (moved)
         for ( ;; )
         {
            // count++ ;  gives wrong value if > 10000 recs exist, moved line above for loop)
            int partCount = tfile4skip( t4, 10000L ) ;  // go in chuncks of 10000 to be more efficient
            if ( partCount < 0 )
               return partCount ;
            count += partCount ;
            if ( partCount != 10000 )
               break ;
         }

         return count ;
      #endif
   }
#endif /* !defined( S4OFF_INDEX ) */



#if !defined( S4OFF_INDEX ) && !defined( S4CLIENT )
   /* !S4OFF_INDEX, !S4CLIENT */
   int S4FUNCTION tfile4empty( TAG4FILE *tag )
   {
      B4BLOCK *b4 ;

      #ifdef E4PARM_LOW
         if ( tag == 0 )
            return error4( 0, e4parm_null, E91642 ) ;
      #endif

      b4 = tfile4block( tag ) ;
      if ( b4 == 0 )
         return 1 ;
      if ( b4numKeys( b4 ) == 0L )
         return 1 ;

      return 0 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) */


#ifndef S4OFF_INDEX
   // AS Oct 25/05 - new function available now...
   // #ifndef S4CLIENT
   #ifdef S4CLIPPER
      /* !S4OFF_INDEX, S4CLIPPER */
      int S4FUNCTION tfile4exprKey( TAG4FILE *tag, unsigned char **ptrPtr )
      {
         int len, oldDec ;

         #ifdef E4PARM_LOW
            if ( tag == 0 || ptrPtr == 0)
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         oldDec = tag->codeBase->decimals ;
         tag->codeBase->decimals = tag->header.keyDec ;

         len = expr4key( tag->expr, (char **)ptrPtr, tag ) ;

         tag->codeBase->decimals = oldDec ;

         return len ;
      }
   #endif /* S4CLIPPER */



   // AS Mar 15/06 - fix for mdx
   #if defined( S4CLIENT )
      /* !S4OFF_INDEX, !S4CLIPPER, E4PARM_LOW */
      int S4FUNCTION tfile4exprKey( TAG4FILE *tag, unsigned char **ptrPtr )
      {
         #ifdef E4PARM_LOW
            if ( tag == 0 || ptrPtr == 0)
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         // AS Oct 25/05 - new function available now...
         #ifdef S4CLIENT
            // just request directly from the server
            CODE4 *c4 = tag->codeBase ;
            CONNECTION4 *connection = &c4->defaultServer ;
            #ifdef E4ANALYZE
               if ( connection == 0 )
               {
                  error4( c4, e4struct, E91601 ) ;
                  return 0 ;
               }
            #endif

            if ( tag->tagDataValid == 0 )
               return 0 ;
            int rc = connection4assign( connection, CON4TAG_EXPR_KEY, data4clientId( tag->refData ), data4serverId( tag->refData ) ) ;
            if ( rc < 0 )
               return 0 ;

            CONNECTION4TAG_EXPR_KEY_INFO_IN *infoIn ;
            CONNECTION4TAG_EXPR_KEY_INFO_OUT *out ;
            connection4addData( connection, NULL, sizeof(CONNECTION4TAG_EXPR_KEY_INFO_IN), (void **)&infoIn ) ;
            memcpy( infoIn->tagName, tag->alias, LEN4TAG_ALIAS ) ;
            infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
            if ( tag->tagDataValid == 1 )  // use the starting recno...
               infoIn->startRecno = htonl5( tag->recNo ) ;
            else
               infoIn->startRecno = htonl5( 0L ) ;
            u4ncpy( infoIn->indexName, tag->indexFile->accessName, strlen( tag->indexFile->accessName ) + 1 ) ;
            rc = connection4sendMessage( connection ) ;
            if ( rc == 0 )
               rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               return 0 ;
            rc = connection4status( connection ) ;
            if ( rc < 0 )
               return 0 ;
            out = (CONNECTION4TAG_EXPR_KEY_INFO_OUT *)connection4data( connection ) ;
            unsigned long keyLen = (unsigned long)ntohl5(out->keyLen) ;  // AS Jun 12/06 - conversion fix

            // use the c4->fieldBuffer for the temporary storage of the key
            if ( c4->bufLen <= keyLen )   /* not room for field length + null */
            {
               if ( u4allocAgain( c4, &c4->fieldBuffer, &c4->bufLen, keyLen ) < 0 )
               {
                  #ifdef E4STACK
                     error4stack( c4, e4memory, E90542 ) ;
                  #endif
                  return 0 ;
               }
            }
            memcpy( c4->fieldBuffer, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_EXPR_KEY_INFO_OUT), keyLen ) ;
            *ptrPtr = (unsigned char *)c4->fieldBuffer ;
            return keyLen ;  // AS Jan 23/06 - ensure correct return code...
         #else
            return expr4key( tag->expr, (char **)ptrPtr, tag ) ;
         #endif
      }
   #endif



   #ifndef S4CLIENT
      #ifndef S4CLIPPER
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4freeAll( TAG4FILE *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         #ifdef I4PRINT
            #ifdef I4PRINT_TAG_NAME
               if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
            #endif
            {
               char dump[255] ;
               sprintf( dump, "freeing all tag info for tag: %s\r\n", t4->alias ) ;
               log5( dump ) ;
            }
         #endif

         while ( tfile4up( t4 ) == 0 ) ;
         return tfile4freeSaved( t4 ) ;
      }



      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4freeSaved( TAG4FILE *t4 )
      {
         B4BLOCK *blockOn ;
         int rc ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         #ifndef S4OFF_WRITE
            rc = tfile4update( t4 ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
         #endif

         for ( ;; )
         {
            blockOn = (B4BLOCK *)l4pop( &t4->saved ) ;
            if ( blockOn == 0 )
               return 0 ;
            #ifndef S4OFF_WRITE
               rc = b4flush( blockOn ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;
            #endif
            rc = b4free( blockOn ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
         }
      }
      #endif /* !S4CLIPPER */
   #endif /* #ifndef S4CLIENT */



   /* !S4OFF_INDEX, !S4CLIENT */
   int S4FUNCTION tfile4go( TAG4FILE *t4, const unsigned char *ptr, const unsigned long recNum, const int goAdd )
   {
      int rc ;
      #ifdef S4HAS_DESCENDING
         int oldDesc ;
      #endif

      assert5parmLow( t4 != 0 && ptr != 0 && recNum != 0L && recNum != ULONG_MAX && recNum != ULONG_MAX - 1 ) ;

      #ifdef S4CLIENT
         CODE4 *c4 = t4->codeBase ;
         CONNECTION4 *connection = &c4->defaultServer ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
               return error4( c4, e4struct, E91642 ) ;
         #endif

         t4->tagDataValid = 0 ;  // reset to invalid
         tfile4cacheReset( t4 ) ;

         rc = connection4assign( connection, CON4TAG_GO, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;

         CONNECTION4TAG_GO_INFO_IN *infoIn ;
         CONNECTION4TAG_GO_INFO_OUT *out ;
         connection4addData( connection, NULL, sizeof(CONNECTION4TAG_GO_INFO_IN), (void **)&infoIn ) ;
         memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
         infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
         u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         unsigned short keyLen = tfile4keyLen( t4 ) ;
         infoIn->keyLen = htons5( keyLen ) ;
         infoIn->goAdd = htons5( goAdd ) ;
         infoIn->recNum = htonl5( recNum ) ;
         connection4addData( connection, ptr, keyLen, NULL ) ;
         rc = connection4sendMessage( connection ) ;
         if ( rc == 0 )
            rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( data->codeBase, rc, E91642 ) ;
         int saveRc = connection4status( connection ) ;
         if ( saveRc < 0 )
            return connection4error( connection, c4, saveRc, E91642 ) ;
         if ( saveRc != 0 )
            return  saveRc ;
         out = (CONNECTION4TAG_GO_INFO_OUT *)connection4data( connection ) ;
         int conLen = connection4len( connection ) ;
         if ( conLen < sizeof( CONNECTION4TAG_GO_INFO_OUT ) )
            return error4( c4, e4packetLen, E91642 ) ;
         t4->recNo = ntohl5(out->rec) ;

         // AS Feb 9/09 - added support to return key as well
         t4->currentKeyLen = (unsigned long)(ntohl5(out->keyLen)) ;
         if ( conLen != sizeof( CONNECTION4TAG_GO_INFO_OUT ) + t4->currentKeyLen )
            return error4( c4, e4packetLen, E91642 ) ;
         if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
         {
            if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E91642 ) ;
               #endif
               return 0 ;
            }
         }
         memcpy( t4->currentKey, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_GO_INFO_OUT), t4->currentKeyLen ) ;

         t4->tagDataValid = 1 ;

         return saveRc ;
      #else
         #ifdef S4HAS_DESCENDING
            oldDesc = t4->header.descending ;
            t4->header.descending = 0 ;
         #endif

         rc = tfile4go2( t4, ptr, recNum, goAdd ) ;

         #ifdef S4HAS_DESCENDING
            t4->header.descending = (short)oldDesc ;
         #endif

         return rc ;
      #endif
   }


   #ifndef S4CLIENT
      #ifndef S4CLIPPER
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4goEof( TAG4FILE *t4 )
      {
         int rc ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         rc = tfile4bottom( t4 ) ;
         if ( rc != 0 )
            return rc ;

         #ifdef E4ANALYZE
            if ( tfile4block( t4 ) == 0 )
               return error4( 0, e4info, E91642 ) ;
         #endif

         #ifdef S4FOX
            if ( b4numKeys( tfile4block(t4) ) != 0 )
               tfile4block(t4)->keyOn = b4numKeys( tfile4block(t4) ) ;
         #else
            tfile4block(t4)->keyOn++ ;
         #endif

         return 0 ;
      }



      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      #ifdef S4FOX
      /* LY 00/04/07 : unsigned long to S4UNSIGNED_LONG for 64-bit HP-UX */
      static int tfile4go2fox( TAG4FILE *t4, const unsigned char *ptr, const S4UNSIGNED_LONG recNum, const int addGo )
      {
         int kLen = t4->header.keyLen ;
         /* LY 00/04/07 : unsigned long to S4UNSIGNED_LONG for 64-bit HP-UX */
         S4UNSIGNED_LONG rec = x4reverseLong( (void *)&recNum ) ;

         int rc2 ;
         do
         {
            /* Do initial search, moving up only as far as necessary */
            rc2 = tfile4upToRoot( t4 ) ;
            if ( rc2 < 0 )
               return error4stack( t4->codeBase, (short)rc2, E91642 ) ;

            if ( rc2 != 2 )
            {
               for( ;; ) /* Repeat until found */
               {
                  B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
                  #ifdef E4ANALYZE
                     if ( blockOn == 0 )
                        return error4( t4->codeBase, e4index, E91642 ) ;
                  #endif

                  int allBlank = 0 ;

                  if ( b4leaf(blockOn) )
                  {
                     int rc = b4seek( blockOn, (char *)ptr, kLen ) ;

                     if ( rc )    /* leaf seek did not end in perfect find */
                     {
                        #ifdef I4PRINT
                           // it isn't necessarily critical if not found on non add (eg. a failed seek), but was looking
                           // at a problem whereby the remove was not finding the key which should have been there.
                           if ( addGo == 0 )
                           {
                              #ifdef I4PRINT_TAG_NAME
                                 if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                              #endif
                              {
                                 char dump[255] ;
                                 sprintf( dump, "tfile4go2fox block failed on block #: %ld # keys in block: %ld, keyOn: %ld, recNum:%ld\r\n", (long)b4node( blockOn->fileBlock ), (long)b4numKeys( blockOn ), (long)blockOn->keyOn, (long)recNum ) ;
                                 log5( dump ) ;
                                 log4invokeDebugger();
                                 b4seek( blockOn, (char *)ptr, kLen ) ;  // seek again to look it up in the debugger...

                              }
                           }
                        #endif
                        return rc ;
                     }

                     /* now do the seek for recno on the leaf block */
                     blockOn = (B4BLOCK *)t4->blocks.lastNode ;

                     char hasSkipped = 0 ;
                     int trailCntFoundKey = x4trailCnt( blockOn, blockOn->keyOn ) ;
                     if ( trailCntFoundKey == t4->header.keyLen )
                        allBlank = 1 ;
                     else
                     {
                        if ( blockOn->keyOn == 0 )  /* could still be all blanks - 1st key */
                        {
                           allBlank = 1 ;
                           int i ;
                           for ( i = 0 ; i < t4->header.keyLen ; i++ )
                           {
                              // AS 06/19/00 - pChar, not ' '
                              if ( ptr[i] != t4->pChar )
                              {
                                 allBlank = 0 ;
                                 break ;
                              }
                           }

                           // AS 06/19/00
                           // also count the real number of blanks in the key in this case...
                           trailCntFoundKey = 0 ;
                           for ( i = t4->header.keyLen - 1 ; i >= 0 ; i-- )
                           {
                              // AS 06/19/00 - pChar, not ' '
                              if ( ptr[i] != t4->pChar )
                                 break ;
                              trailCntFoundKey++ ;
                           }
                        }
                        else
                           allBlank = 0 ;
                     }

                     for( ;; )
                     {
                        rec = tfile4recNo( t4 ) ;
                        if ( rec == ULONG_MAX || rec == ULONG_MAX - 1 )  // eof or no current block
                           return -1 ;

                        if ( addGo == 0 )
                           if ( rec == recNum )
                              return 0 ;

                        if ( rec >= recNum )
                        {
                           if ( !hasSkipped )
                              blockOn->curDupCnt = x4dupCnt( blockOn, blockOn->keyOn ) ;
                           return r4found ;
                        }

                        hasSkipped = 1 ;

                        rc = (int)tfile4skip( t4, 1L ) ;
                        if ( rc == -1 )
                           return -1 ;
                        if ( rc == 0 )
                        {
                           b4goEof( tfile4block( t4 ) ) ;
                           return r4found ;
                        }

                        /* AS 05/14/98 - 1 instance where this won't work:
                              < blank key, suddenly goes to all-blanks, then all blank is true, should stop but doesn't
                        */
                        int blankCountCurKey = x4trailCnt( blockOn, blockOn->keyOn ) ;
                        int dupCountCurKey = x4dupCnt( blockOn, blockOn->keyOn )  ;

                        if ( dupCountCurKey + blankCountCurKey != t4->header.keyLen )
                        {
                          /* case where key changed --> need to go back one to be on r4found value*/
                           return r4found ;
                        }

                        // AS 06/19/00 (t4indx2.c new test code) - fails in another instance:  the key goes from 0 blanks to 1 blank,
                        // and the key is < blank, and there are duplicates.  (eg. is "0x00001f, 0x000020 ) - search for '0x00001f'
                        // at this point we are at the '0x00001f' entry, for an add, so we are skipping until we have gone '1 too far'.
                        // now, after skipping blankCountCurKey == 1, x4dupCnt == 9, so above 'if' and below fails.
                        if ( trailCntFoundKey < blankCountCurKey )
                        {
                           // in this case, the key had say '0' blanks, and now we have moved to a key with '1' blank - we must be too far...
                           // (i.e. binary key)
                           return r4found ;
                        }

                        if ( blankCountCurKey == t4->header.keyLen && !allBlank )
                        {
                           /* done because our search key was < blank, but we found blank */
                           return r4found ;
                        }
                     }
                  }
                  else
                  {
                     int rc = b4rBrseek( blockOn, (char *)ptr, kLen, (S4LONG)rec ) ;  /* LY 00/04/07 : cast to S4LONG for 64-bit HP-UX */
                     // AS Apr 7/03 - typeCode will be 0x04 for r4candidate/e4candidate
                     if ( rc == 0 && t4->header.typeCode & 0x05 )
                        if ( b4recNo( tfile4block(t4), tfile4block(t4)->keyOn ) != recNum )
                           return r4found ;
                  }

                  rc2 = tfile4down( t4 ) ;
                  if ( rc2 < 0 )
                     return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
                  if ( rc2 == 2 )
                  {
                     int rc3 = tfile4outOfDate( t4 ) ;
                     if ( rc3 < 0 )
                        return error4stack( t4->codeBase, (short)rc3, E91642 ) ;
                     break ;
                  }
               }
            }
         } while ( rc2 == 2 ) ;
         return 0 ;
      }
      #endif /* S4FOX */



      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      #ifdef S4MDX
      static int tfile4go2mdx( TAG4FILE *t4, const unsigned char *ptr, const unsigned long recNum, const int addGo )
      {
         int rc = tfile4seek( t4, ptr, t4->header.keyLen ) ;
         if ( rc )
            return rc ;
         unsigned long recSave = tfile4recNo( t4 ) ;
         if ( recSave == recNum )
            return 0 ;

         /* else find the far end, and then skip back to where now or find */
         tfile4upToRoot( t4 ) ;
         for( ;; )
         {
            B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            #ifdef E4ANALYZE
               if ( blockOn == 0 )
                  return error4( t4->codeBase, e4index, E91642 ) ;
            #endif
            rc = b4seek( blockOn, (const char *)ptr, t4->header.keyLen ) ;
            while( rc == 0 )  /* perfect find, check next */
            {
               if ( b4skip( blockOn, 1L ) == 0 )
                  break ;
               if ( (*t4->cmp)( b4keyKey( blockOn, blockOn->keyOn ), ptr, t4->header.keyLen ) != 0 )
                  rc = r4after ;  /* stop loop and avoid rare return if b4leaf below */
            }

            if ( b4leaf( blockOn ) )
            {
               unsigned long tagRec = tfile4recNo( t4 ) ;
               if ( tagRec == recNum && rc == 0 )   /* found */
                  return 0 ;
               if ( tagRec == recSave )   /* didn't move */
                  return r4found ;
               break ;
            }
            int rc3 = tfile4down( t4 ) ;
            if ( rc3 < 0 )
               return error4stack( t4->codeBase, rc3, E91642 ) ;
         }

         for(;;)
         {
            rc = (int)b4skip( tfile4block( t4 ), -1L ) ;
            if ( rc == 0 ) /* try previous tag */
            {
               if ( tfile4skip( t4, -1L ) == 0 )
                  return r4found ;
            }
            unsigned long rec = tfile4recNo( t4 ) ;
            if ( rec == recSave )   /* failed to find */
               return r4found ;
            if ( rec == recNum )
               return 0 ;
         }
      }
      #endif /* S4MDX */



      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4go2( TAG4FILE *t4, const unsigned char *ptr, const unsigned long recNum, const int addGo )
      {
         /* addGo true if performing for add purposes instead of seek purposes */
         #ifdef E4PARM_LOW
            if ( t4 == 0 || ptr == 0 )
               return error4( 0, e4parm, E91642 ) ;
            if ( recNum <= 0L )
               return error4( 0, e4parm, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         #ifdef S4FOX
            return tfile4go2fox( t4, ptr, (S4UNSIGNED_LONG)recNum, addGo ) ; /* LY 00/04/07 : cast to S4UNSIGNED_LONG for 64-bit HP-UX */
         #endif

         #ifdef S4MDX
            return tfile4go2mdx( t4, ptr, recNum, addGo ) ;
         #endif
      }
      #endif /* !S4CLIPPER */
   #endif /* !S4CLIENT */
#endif /* !S4OFF_INDEX */




#ifndef S4OFF_INDEX
   #ifdef S4CLIENT
      S4EXPORT unsigned long S4FUNCTION tfile4recNo( TAG4FILE *t4 )
      {
         // returns 0 if the entire tag is empty
         if ( t4->tagDataValid == 1 )
            return t4->recNo ;

         if ( d4tagSelected( t4->refData )->tagFile == t4 )  // in this case, just return the data's record number...
         {
            if ( d4recCount( t4->refData ) == 0 ) // perhaps there are no records
               return 0 ;
            // if we are at eof, return ULONG_MAX - 1
            if ( d4eof( t4->refData ) == 1 )
               return ULONG_MAX-1 ;
            // t4->tagDataValid = 1 ;  // AS Feb 9/09 - don't mark the record as valid because we haven't actually positioned the tag
            t4->recNo = d4recNo( t4->refData ) ;
            return t4->recNo ;
         }

         // otherwise at an invalid unknown position...
         return ULONG_MAX - 1 ;
      }



      int S4FUNCTION tfile4seek( TAG4FILE *t4, const void *ptr, const int lenIn )
      {
         int rc ;
         CONNECTION4TAG_SEEK_INFO_IN *info ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )  // AS Jun 12/06 - conversion fix
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         CODE4 *c4 = t4->codeBase ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;   // AS Jun 12/06 - conversion fix

         CONNECTION4 *connection = &c4->defaultServer ;
         if ( connection == 0 )
            return e4connection ;

         t4->tagDataValid = 0 ;  // reset to invalid
         tfile4cacheReset( t4 ) ;

         rc = connection4assign( connection, CON4TAG_SEEK, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;
         connection4addData( connection, NULL, sizeof( CONNECTION4TAG_SEEK_INFO_IN ), (void **)&info ) ;
         memcpy( info->tagName, t4->alias, LEN4TAG_ALIAS  ) ;
         info->tagName[LEN4TAG_ALIAS] = 0 ;
         // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
         u4ncpy( info->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         info->keyLen = htonl5( lenIn ) ;
         connection4addData( connection, ptr, lenIn, NULL ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( dc4, rc, E91642 ) ;
         rc = connection4status( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E94701 ) ;
         int conLen = connection4len( connection ) ;
         if ( conLen < sizeof( CONNECTION4TAG_SEEK_INFO_OUT ) )
            return error4( c4, e4packetLen, E91642 ) ;
         CONNECTION4TAG_SEEK_INFO_OUT *out ;
         out = (CONNECTION4TAG_SEEK_INFO_OUT *)connection4data( connection ) ;

         t4->recNo = ntohl5(out->rec) ;

         // AS Feb 9/09 - added support to return key as well
         t4->currentKeyLen = (unsigned long)(ntohl5(out->keyLen)) ;
         if ( conLen != sizeof( CONNECTION4TAG_SEEK_INFO_OUT ) + t4->currentKeyLen )
            return error4( c4, e4packetLen, E91642 ) ;
         if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
         {
            if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E91642 ) ;
               #endif
               return 0 ;
            }
         }
         memcpy( t4->currentKey, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_SEEK_INFO_OUT), t4->currentKeyLen ) ;

         t4->tagDataValid = 1 ;
         return rc ;
      }
   #else
      #ifndef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         B4KEY_DATA *tfile4keyData( TAG4FILE *t4 )
         {
            B4BLOCK *b4 ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
               {
                  error4( 0, e4parm_null, E91642 ) ;
                  return 0 ;
               }
            #endif

            b4 = (B4BLOCK *)t4->blocks.lastNode ;
            #ifdef E4ANALYZE
               if ( b4 == 0 )
               {
                  error4( t4->codeBase, e4index, E91642 ) ;
                  return 0 ;
               }
               #ifdef S4FOX
                  if ( b4->keyOn >= b4->header.nKeys )     /* invalid key */
               #else
                  if ( b4->keyOn >= b4->nKeys )     /* invalid key */
               #endif
               {
                  error4( t4->codeBase, e4index, E91642 ) ;
                  return 0 ;
               }
            #endif
            return b4key( b4, b4->keyOn ) ;
         }
      #endif /* !S4CLIPPER */



      #if !defined( S4OFF_WRITE ) && defined( S4FOX )
         /* !S4OFF_INDEX, !S4CLIENT, !S4OFF_WRITE, S4FOX */
         int tfile4leafSplit( TAG4FILE *t4, B4BLOCK *oldBlock, B4BLOCK *newBlock )
         {
            #ifdef E4PARM_LOW
               if ( t4 == 0 || oldBlock == 0 || newBlock == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            // AS 08/14/01 - T4LARGE4.C -s1234 -i512 -r65044
            if ( b4numKeys( oldBlock ) == 1 )
            {
               // In this instance we get a case where the index block is full with only 1 key, and so the
               // input block here contains only 1 key.  This is a special case.  What we need to do is
               // have one empty block (where the record will get inserted) and one block with the key in place
               // this is based on the postion.  Notice that the newBlock comes after the old block.  If the
               // current position in the old block is keyOn == 1 (after key), we want the 'new' block to
               // be empty to contain the new entry.  If old block position is '0', we want the new block to
               // contain the key so the old block can get the new entry
               if ( oldBlock->keyOn == 0 )
               {
                  // copy the old data into the new block
                  memcpy( &newBlock->header, &oldBlock->header, i4blockSize( t4->indexFile ) ) ;
                  oldBlock->header.nKeys = 0 ;
                  oldBlock->curPos = ((char *)&newBlock->header) + i4blockSize( t4->indexFile ) ;
                  int rc = b4top( newBlock ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;
               }
               else
               {
                  // AS Aug 14/01 - Impossible to end up here because the way we do seeks we always go
                  // to the 'found' position.  Could conceivably end up here if appending at the end?
                  // for now this instance has never happened, the code here may not work, so just error out
                  return error4( t4->codeBase, e4index, E91642 ) ;
                  /*
                     memcpy( &newBlock->header, &oldBlock->header, i4blockSize( t4->indexFile ) ) ;
                     newBlock->header.nKeys = 0 ;
                     newBlock->keyOn = 0 ;
                     newBlock->curPos = ((char *)&newBlock->header) + i4blockSize( t4->indexFile ) ;
                     int rc = b4top( oldBlock ) ;
                     if ( rc < 0 )
                        return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                  */
               }
               return 0 ;
            }

            int kLen = t4->header.keyLen ;
            int iLen = oldBlock->nodeHdr.infoLen ;
            int bLen = i4blockSize( t4->indexFile ) - (sizeof(oldBlock->header)) - (sizeof(oldBlock->nodeHdr))
                       - b4numKeys( oldBlock ) * iLen - oldBlock->nodeHdr.freeSpace ;
            int oldDup = oldBlock->curDupCnt ;

            b4top( oldBlock ) ;
            int nKeys = b4numKeys( oldBlock ) / 2 ;
            #ifdef S4WIN64 /* LY 00/09/21 */
               UINT64 len = 0 ;
            #else
               int len = 0 ;
            #endif
            for ( ; len < b4numKeys( oldBlock ) - nKeys ; len++ )
            {
               int rc = b4skip( oldBlock, 1L ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;
            }

            /* build the key 1st key of the new block from one past new end of old block */
            b4key( oldBlock, oldBlock->keyOn ) ;

            /* copy the general information */
            c4memcpy( (void *)&newBlock->header, (void *)&oldBlock->header,
                    (sizeof( oldBlock->header)) + (sizeof(oldBlock->nodeHdr)) ) ;

            /* AS 06/19/98 - changes.60 fix #134, 2.6 version should not include blanks... */
            /* put 1st key of new block */
            if ( t4->indexFile->dataFile->compatibility == 26 && t4->filter != 0 )
               newBlock->curTrailCnt = 0 ;
            else
               newBlock->curTrailCnt = b4calcBlanks( oldBlock->builtKey->value, kLen, t4->pChar ) ;
            len = kLen - newBlock->curTrailCnt ;
            newBlock->curPos = ((char *)&newBlock->header) + i4blockSize( t4->indexFile ) - len ;
            c4memcpy( newBlock->curPos, oldBlock->builtKey->value, (unsigned int)len ) ;

            /* copy remaining key data */
            char *obdPos = ((char *)&oldBlock->header) + i4blockSize( t4->indexFile ) - bLen ;
            len = oldBlock->curPos - obdPos ;
            assert5( len >= 0 ) ;
            newBlock->curPos -= len ;
            c4memcpy( newBlock->curPos, obdPos, (unsigned int)len ) ;

            /* copy the info data */
            char *obiPos = oldBlock->data + oldBlock->keyOn * iLen ;
            c4memcpy( newBlock->data, obiPos, (unsigned int)nKeys * iLen ) ;

            /* clear the old data */
            int rc = b4skip( oldBlock, -1L ) ;  /* go to new last entry */
            if ( rc == 0 )
               if ( error4code( t4->codeBase ) < 0 )
                  return error4stack( t4->codeBase, (short)error4code( t4->codeBase ), E91642 ) ;
            #ifdef E4ANALYZE
               if ( obdPos < obiPos )
                  return error4( t4->codeBase, e4info, E81603 ) ;
            #endif
            c4memset( obiPos, 0, (unsigned int)(oldBlock->curPos - obiPos) ) ;

            /* now reset the place new info data for the first key */
            c4memset( newBlock->data, 0, (unsigned int)iLen ) ;
            unsigned char buffer[6] ;
            x4putInfo( &newBlock->nodeHdr, buffer, oldBlock->builtKey->num, newBlock->curTrailCnt, 0 ) ;
            c4memcpy( newBlock->data, (void *)buffer, (unsigned int)iLen ) ;

            newBlock->header.nKeys = (short)nKeys ;
            oldBlock->header.nKeys -= (short)nKeys ;
            newBlock->header.nodeAttribute = 2 ;
            oldBlock->header.nodeAttribute = 2 ;
            newBlock->nodeHdr.freeSpace = (short) (newBlock->curPos - newBlock->data
                                             - b4numKeys( newBlock ) * iLen) ;
            oldBlock->nodeHdr.freeSpace = (short) (oldBlock->curPos - oldBlock->data
                                             - b4numKeys( oldBlock ) * iLen) ;
            oldBlock->builtOn = -1 ;
            newBlock->builtOn = -1 ;

            rc = b4top( oldBlock ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
            rc = b4top( newBlock ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;

            /* make sure dupCnt is updated on blocks for insert */
            newBlock->curDupCnt = oldDup ;
            oldBlock->curDupCnt = oldDup ;

            return 0 ;
         }
      #endif /* if !defined( S4OFF_WRITE ) && defined( S4FOX ) */



      /* !S4OFF_INDEX, !S4CLIENT */
      int tfile4outOfDate( TAG4FILE *t4 )
      {
         #ifndef S4SINGLE
            #ifdef S4WINCE /* LY 2002/11/12 */
               SYSTEMTIME st ;
               WORD oldTime ;
            #else
               time_t oldTime ;
            #endif
         #endif

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         #ifdef S4SINGLE
            return error4describe( t4->codeBase, e4index, E81605, tfile4alias( t4 ), 0, 0 ) ;
         #else
            /* first make sure we are at a potential read conflict situation (otherwise must be corrupt file) */
            #ifdef S4CLIPPER
               if ( tfile4lockTest( t4 ) == 1 )
            #else
               if ( index4lockTest( t4->indexFile ) == 1 )
            #endif
               return error4describe( t4->codeBase, e4index, E81606, tfile4alias( t4 ), 0, 0 ) ;

            /* wait a second and refresh */
            #ifdef S4WINCE
               for ( GetLocalTime( &st ), oldTime = (st.wSecond * 1000) +
                  st.wMilliseconds ; (st.wSecond * 1000) + st.wMilliseconds <=
                  oldTime ; GetLocalTime( &st ) )
                  ;
            #else
               time( &oldTime) ;
               while ( time( (time_t *)0 ) <= oldTime) ;
            #endif
            tfile4freeAll( t4 ) ;
            #ifdef S4CLIPPER
               return file4refresh( &t4->file ) ;
            #else
               return file4refresh( &t4->indexFile->file ) ;
            #endif
         #endif
      }



      #ifdef S4FOX
         S4EXPORT unsigned long S4FUNCTION tfile4recNo( TAG4FILE *t4 )
         {
            /*
               returns -2L (or ULONG_MAX-1) if there is no current block
               returns -1L (or ULONG_MAX) if the block is at eof (i.e. tag at eof), ... no valid recno)
                 - this is considered an error - asking for the recNo when there is none...
               returns -1L (or ULONG_MAX) if an error
               returns 0 if the entire tag is empty
               otherwise returns the appropriate record number from the current block...
            */

            if ( t4->blocks.lastNode == 0 )
               return ULONG_MAX - 1 ;

            if ( b4numKeys( (B4BLOCK *)(t4->blocks.lastNode) ) == 0 )
               return 0L ;

            // AS May 30/01 - If this block is not a leaf block then there is no
            // active key.  This was failing in an obscure example with a d4skip
            // after a d4go call
            if ( !b4leaf( (B4BLOCK *)(t4->blocks.lastNode) ) )
               return ULONG_MAX - 1 ;

            if ( ((B4BLOCK *)(t4->blocks.lastNode))->keyOn >= b4numKeys( (B4BLOCK *)(t4->blocks.lastNode) ) )
               return ULONG_MAX ;

            return b4recNo( (B4BLOCK *)(t4->blocks.lastNode), ((B4BLOCK *)(t4->blocks.lastNode))->keyOn ) ;
         }
      #endif /* S4FOX */



      #ifdef S4MDX
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, S4MDX */
         S4EXPORT unsigned long S4FUNCTION tfile4recNo( TAG4FILE *t4 )
         {
            /*
               returns -2L (or ULONG_MAX-1) if there is no current block
               returns -2L (or ULONG_MAX-1) if the current block is not a leaf
               returns -1L (or ULONG_MAX) if an error
               returns 0 if the entire tag is empty
               otherwise returns the appropriate record number from the current block...
            */

            B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            if ( blockOn == 0 )
               return ULONG_MAX - 1 ;

            if ( b4numKeys( blockOn ) == 0 )
               return 0 ;
            if ( !b4leaf( blockOn ) )
               return ULONG_MAX - 1 ;

            return b4recNo( blockOn, blockOn->keyOn ) ;
         }
      #endif /* S4MDX */



      #ifdef S4FOX
         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         int tfile4rlBottom( TAG4FILE *t4 )
         {
            int rc, rc2 ;
            B4BLOCK *blockOn ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            do
            {
               rc = tfile4upToRoot( t4 ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;

               if ( rc != 2 )
               {
                  if ( b4numKeys( tfile4block( t4 ) ) != 0 )
                  {
                     rc2 = b4go( tfile4block( t4 ), (long)b4numKeys( tfile4block( t4 ) ) - 1L ) ;
                     if ( rc2 < 0 )
                        return error4stack( t4->codeBase, rc2, E91642 ) ;
                     do
                     {
                        rc = tfile4down( t4 ) ;
                        if ( rc < 0 )
                           return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                        rc2 = b4go( tfile4block( t4 ), (long)b4numKeys( tfile4block( t4 ) ) - 1L ) ;
                        if ( rc2 < 0 )
                           return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
                     } while ( rc == 0 ) ;
                  }
               }

               if ( rc == 2 )   /* failed due to read while locked */
               {
                  rc2 = tfile4outOfDate( t4 ) ;
                  if ( rc2 < 0 )
                     return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
               }
            } while ( rc == 2 ) ;

            blockOn = tfile4block( t4 ) ;

            #ifdef E4ANALYZE
               if ( blockOn == 0 )
                  return error4( t4->codeBase, e4result, E91642 ) ;
            #endif

            if ( blockOn->keyOn > 0 )
            {
               b4goEof( blockOn ) ;
               blockOn->keyOn-- ;  /* update keyOn after going to last spot */
            }

            return 0 ;
         }



         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         int tfile4rlTop( TAG4FILE *t4 )
         {
            int rc, rc2 ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            do
            {
               rc = tfile4upToRoot( t4 ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;

               if ( rc != 2 )
               {
                  do
                  {
                     b4top( (B4BLOCK *)t4->blocks.lastNode ) ;
                     if ( (rc = tfile4down(t4)) < 0 )
                        return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                  } while ( rc == 0 ) ;
               }

               if ( rc == 2 )   /* failed due to read while locked */
               {
                  rc2 = tfile4outOfDate( t4 ) ;
                  return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
               }
            } while ( rc == 2 ) ;
            return 0 ;
         }


         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         static void tfile4seekDescendKey( TAG4FILE *t4, unsigned char *cPtr, int *incPos, const int lenPtr, const int lenIn, int *dSet, int *tmpLenPtr )
         {
            // basically force a descending seek by modifying the key to increment it by 1, then seeking for that and skipping back once and see
            // if we found what we were looking for.
            // this coding just desends the key by incrementing it.
            // Needs to consider special cases of bytes == 0xFF (increment would otherwise make 0), and case of general sort sequence
            // where we only want to effect the root part of the key, not the trail bytes
            // AS 07/27/99 -- this has been superseded by more simple collations for foxPro

            /* AS 07/30/98
               Problems here.  Some basic information:
               General sequencing works as follows:  string "A A" becomes: "0x60 0x11 0x60 0x00 0x00 0x00".  The 1st 3 bytes are sorted on, the
               last 3 bytes are the special character indicators (but characters are sorted as if they were the same).  Thus, in order to do a true descending
               seek, we need to increment the base key by '1'.  This would be in the first half of the key.  Since the 1st half of the keys never contains
               values < 10 and the 2nd half always contains values < 10, go to the right of the key and keep moving left until we have bypassed all the
               < 10 values.  Then do the standard increment.  NOTE however, this fails if the whole thing is blank because in that
               case the 'blank' overtakes the actual key values and we get just"0x00 0x00 0x00 0x00 0x00 0x00).  Therefore this is a special case; if we pass th
               beginning of the key then we have an all-blank key, just look for 0x12 (space is normally 11),
               code doesn't take into account 0xff ordering.. ok?
            */
            // if ( vfpKey && tfile4type( t4 ) == r4str )
            if ( collation4get( t4->collateName )->collateType != collate4machineByteOrder )
            {
               // AS 04/25/00 coded back in - was failing in string case of general collated descending indexes (or relations which descended the index)
               // return error4( t4->codeBase, e4notSupported, E84907 ) ; // not coded to work for unicode, when not just divide by 2 because maybe only 1 char extra...
               for( (*incPos) = lenPtr-1 ; cPtr[(*incPos)] < 10 && (*incPos) > 0 ; (*incPos)-- ) ;

               assert5( (*incPos) >= 0 ) ;  /* if 0, just increment anyway, even if 0 */

               if ( (*incPos) == 0 && cPtr[(*incPos)] == 0 )  // special case, set to 0x12
               {
                  (*incPos) = lenIn/2 - 1 ;
                  cPtr[(*incPos)] = 18 ;
                  c4memset( cPtr, 0x11, (*incPos) ) ;
                  (*dSet) = 2 ;
                  (*incPos)-- ;
                  (*tmpLenPtr) = lenIn ;
                  return ;
               }

               cPtr[(*incPos)]++ ;
               (*tmpLenPtr) = (*incPos) + 1 ;
               (*incPos)-- ;   /* allows cPtr to be reset correctly later on */
               (*dSet) = 1 ;
               return ;
            }

            for( (*incPos) = lenPtr-1 ; (*dSet) == 0 && (*incPos) >=0 ; (*incPos)-- )
            {
               if ( cPtr[(*incPos)] != 0xFF )
               {
                  cPtr[(*incPos)]++ ;
                  (*dSet) = 1 ;
               }
               else
                  cPtr[(*incPos)] = 0 ;  // make sure increment by 1 total only, not by a whole order of magnitude
            }
         }



         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         static void tfile4seekDescendKeyUndo( TAG4FILE *t4, unsigned char *cPtr, const int incPos, const int lenPtr, const int lenIn, const int dSet, char *compareKey )
         {
            // undoes the descending process done in tfile4seekDescendKey
            // if ( collation4get( t4->collateName )->collateType != collate4machineByteOrder )
            // {
            //    if ( dSet == 2 )  // special case, set to 0x12
            //    {
            //       cPtr[(*incPos)] = 0 ;
            //       c4memset( cPtr, 0, (*incPos) ) ;
            //       return ;
            //    }
            //
            //    cPtr[(*incPos)]-- ;
            //    return ;
            // }

            // same sequence works for both general and regular

            if ( dSet == 2 )
            {
               cPtr[incPos+1] = 0 ; /* reset the searchPtr ; */
               c4memset( cPtr, 0, incPos + 1 ) ;
               return ;
            }

            cPtr[incPos+1]-- ; /* reset the searchPtr ; */
            if ( collation4get( t4->collateName )->collateType == collate4machineByteOrder )
            {
               // AS 03/25/99 was failing to reset the bytes after incPos to 0xFF in the case
               // of having to reset those bytes to 0 before...
               short lenToReset = lenPtr - incPos - 2 ;
               if ( lenToReset > 0 )
                  c4memset( &(cPtr[incPos+2]), 0xFF, lenToReset ) ;
            }

            #ifdef E4ANALYZE
               // in this case, we were passed in the initial key to ensure that we converted back properly for debug purposes
               if ( memcmp( cPtr, compareKey, lenIn ) != 0 )
               {
                  error4( t4->codeBase, e4result, E91642 ) ;
                  return ;
               }
            #endif
         }
      #endif /* S4FOX */


      #ifndef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         int S4FUNCTION tfile4seek( TAG4FILE *t4, const void *ptr, const int lenIn )
         {
            int rc, rc2 ;
            B4BLOCK *blockOn ;
            #ifdef S4FOX
               int incPos = 0 ;
               int tmpLenPtr ;
               int dSet = 0 ;
               unsigned char *cPtr = (unsigned char *)ptr ;
            #endif
            int lenPtr = lenIn ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 || ptr == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
               // AS 03/10/00 allow partial seeks for ODBC to look for null only (i.e. allow if nullable as well )
               if ( lenPtr != t4->header.keyLen && tfile4type( t4 ) != r4str && tfile4type( t4 ) != r5wstr && tfile4type( t4 ) != r5wstrLen )
               {
                  if ( !expr4nullLow( t4->expr, 0 ) )  // the key can't be NULL, shouldn't be doing a partial seek then because meaningless
                     return error4( t4->codeBase, e4parm, E91642 ) ;
               }
            #endif

            CODE4 *c4 = t4->codeBase ;
            if ( error4code( c4 ) < 0 )
               return e4codeBase ;

            if ( lenPtr > t4->header.keyLen )
               lenPtr = t4->header.keyLen ;
            // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
            #ifdef S4FOX
               tmpLenPtr = lenPtr ;
               #ifdef E4ANALYZE
                  char *compareKey = 0 ;
               #endif

               if ( t4->header.descending )   /* look for current item less one: */
               {
                  #ifdef E4ANALYZE
                     compareKey = (char *)u4alloc( lenIn ) ;
                     if ( compareKey == 0 )
                        return error4( c4, e4memory, E91642 ) ;
                     c4memcpy( compareKey, cPtr, lenIn ) ;
                  #endif
                  tfile4seekDescendKey( t4, cPtr, &incPos, lenPtr, lenIn, &dSet, &tmpLenPtr ) ;
               }
            #endif

            rc = 3 ;
            for( ;; ) /* Repeat until found */
            {
               while ( rc >= 2 )
               {
                  if ( rc == 2 )
                  {
                     rc2 = tfile4outOfDate( t4 ) ;
                     if ( rc2 < 0 )
                        return error4stack( c4, (short)rc2, E91642 ) ;
                  }
                  rc = tfile4upToRoot( t4 ) ;
                  if ( rc < 0 )
                     return error4stack( c4, (short)rc, E91642 ) ;
               }
               blockOn = (B4BLOCK *)t4->blocks.lastNode ;
               #ifdef E4ANALYZE
                  if ( blockOn == 0 )
                     return error4( c4, e4index, E91642 ) ;
               #endif

               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               #ifdef S4FOX
                  assert5( tmpLenPtr >= 0 ) ;
                  rc = b4seek( blockOn, (char *)ptr, tmpLenPtr ) ;
               #else
                  assert5( lenPtr >= 0 ) ;
                  rc = b4seek( blockOn, (char *)ptr, lenPtr ) ;
               #endif

               if ( rc < 0 )
                  return error4stack( c4, (short)rc, E91642 ) ;
               if ( b4leaf( blockOn ) )
                  break ;

               rc = tfile4down( t4 ) ;
               if ( rc < 0 )
                  return error4stack( c4, (short)rc, E91642 ) ;
            }

            #ifdef S4FOX
               if ( t4->header.descending )   /* must go back one! */
               {
                  // reset the search key to the previous value
                  // AS 04/25/00 enabled descending general sequence above, so code this as well
                  #ifdef E4ANALYZE
                     tfile4seekDescendKeyUndo( t4, cPtr, incPos, lenPtr, lenIn, dSet, compareKey ) ;
                     u4free( compareKey ) ;
                  #else
                     tfile4seekDescendKeyUndo( t4, cPtr, incPos, lenPtr, lenIn, dSet, 0 ) ;
                  #endif

                  if ( dSet )
                  {
                     if ( b4numKeys( blockOn ) == 0 )
                        rc = 0 ;
                     else
                     {
                        if ( blockOn->keyOn == 0 )   /* special case, must balance tree */
                        {
                           rc = (int)tfile4skip( t4, -1L ) ;
                           if ( rc == 0 )
                              rc2 = tfile4top( t4 ) ;
                           else
                              rc2 = tfile4go( t4, tfile4keyData( t4 )->value, tfile4recNo( t4 ), 0 ) ;
                           if ( rc2 < 0 )
                              return error4stack( c4, (short)rc2, E91642 ) ;
                        }
                        else
                           rc = (int)tfile4skip( t4, -1L ) ;
                     }
                     if ( rc == 0L )  /* bof = eof condition */
                     {
                        b4goEof( blockOn ) ;
                        rc = r4eof ;
                     }
                     else
                     {
                        blockOn = tfile4block( t4 ) ;
                        rc2 = b4go( blockOn, (long)blockOn->keyOn ) ;
                        if ( rc2 < 0 )
                           return error4stack( c4, (short)rc2, E91642 ) ;
                        // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                        // rc2 = u4keycmp( (void *)b4keyKey( tfile4block( t4 ), tfile4block( t4 )->keyOn ), ptr, (unsigned)lenPtr, (unsigned)t4->header.keyLen, 0, &t4->vfpInfo ) ;
                        rc2 = u4keycmp( (void *)b4keyKey( blockOn, blockOn->keyOn ), ptr, (unsigned)lenPtr, (unsigned)t4->header.keyLen, 0, collation4get( t4->collateName ) ) ;
                        if ( rc2 )
                           rc = r4after ;
                        else
                           rc = 0 ;  /* successful find */
                     }
                  }
                  else
                  {
                     if ( rc == 0 )  /* the item was found, so go top, */
                     {
                        rc2 = tfile4top( t4 ) ;
                        if ( rc2 < 0 )
                           return error4stack( c4, (short)rc2, E91642 ) ;
                     }
                     else  /* otherwise want an eof type condition */
                     {
                        b4goEof( blockOn ) ;
                        rc = r4eof ;
                     }
                  }
               }
            #endif
            return rc ;
         }
      #endif /* !S4CLIPPER */
   #endif /* !S4CLIENT */
#endif /* !S4OFF_INDEX */



#ifndef S4OFF_INDEX
   #ifdef S4CLIENT
      /* !S4OFF_INDEX, S4CLIENT */
      long S4FUNCTION tfile4skip( TAG4FILE *t4, long numSkip )
      {
         return tfile4skipDo( t4, numSkip, 0 );
      }



      /* !S4OFF_INDEX, S4CLIENT */
      long tfile4skipDo( TAG4FILE *t4, long numSkip, Bool5 tfile4dSkip )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
            {
               error4( 0, e4parm_null, E91642 ) ;
               return -numSkip ;
            }
         #endif

         CODE4 *c4 = t4->codeBase ;
         if ( error4code( c4 ) < 0 )
            return -numSkip ;

         CONNECTION4 *connection = &c4->defaultServer ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
            {
               error4( c4, e4struct, E91642 ) ;
               return -numSkip ;
            }
         #endif

         if ( t4->t4readBuffer == 1 && numSkip != 0 && t4->tagDataValid && tfile4dSkip != 1 )  // don't do if dskip is set
         {
            short skipDir = 1 ;
            long didSkip = 0 ;
            if ( numSkip < 0 )
               skipDir = -1 ;

            while ( numSkip != 0 )
            {
               int rc = tfile4skipCache( t4, t4->cache->numRowsIn * skipDir, t4->cache->modusOriginal ) ;
               if ( rc < 0 )    // error
                  return -numSkip  ;
               if ( rc != 0 )         // didn't skip this time...
                  return didSkip ;
               numSkip -= skipDir ;
               didSkip += skipDir ;
            }
            return didSkip ;
         }

         tfile4cacheReset( t4 ) ;

         int rc = connection4assign( connection, CON4TAG_SKIP, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return -numSkip ;

         CONNECTION4TAG_SKIP_INFO_IN *infoIn ;
         CONNECTION4TAG_SKIP_INFO_OUT *out ;
         connection4addData( connection, NULL, sizeof(CONNECTION4TAG_SKIP_INFO_IN), (void **)&infoIn ) ;
         memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
         infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
         if ( t4->tagDataValid == 1 )  // use the starting recno...
            infoIn->startRecno = htonl5( t4->recNo ) ;
         else
            infoIn->startRecno = htonl5( 0L ) ;
         t4->tagDataValid = 0 ;  // reset to invalid
         infoIn->numSkip = htonl5( numSkip ) ;
         infoIn->tfile4dskip = tfile4dSkip ;
         u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
         rc = connection4sendMessage( connection ) ;
         if ( rc == 0 )
            rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return -numSkip ;
         int saveRc = connection4status( connection ) ;
         if ( saveRc < 0 )
         {
            connection4error( connection, c4, saveRc, E91642 ) ;
            return -numSkip ;
         }
         int conLen = connection4len( connection ) ;
         if ( conLen < sizeof( CONNECTION4TAG_SKIP_INFO_OUT ) )
            return error4( c4, e4packetLen, E91642 ) ;
         out = (CONNECTION4TAG_SKIP_INFO_OUT *)connection4data( connection ) ;
         long numSkipped = ntohl5(out->numSkip) ;
         // if ( numSkipped != numSkip )  // must be bof/eof
         t4->recNo = ntohl5(out->recNo) ;

         // AS Feb 9/09 - added support to return key as well
         t4->currentKeyLen = (unsigned long)(ntohl5(out->keyLen)) ;
         if ( conLen != sizeof( CONNECTION4TAG_SKIP_INFO_OUT ) + t4->currentKeyLen )
            return error4( c4, e4packetLen, E91642 ) ;
         if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
         {
            if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E91642 ) ;
               #endif
               return 0 ;
            }
         }
         memcpy( t4->currentKey, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_SKIP_INFO_OUT), t4->currentKeyLen ) ;

         t4->tagDataValid = 1 ;

         return ntohl5( out->numSkip ) ;
      }
   #else
      #ifndef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         long S4FUNCTION tfile4skip( TAG4FILE *t4, long numSkip )
         {
            // returns the number of actual skips (negative for negative skips), and -numskip if a failure (note that when skipping
            // backwards (input of -x), this means a positive return value
            long numLeft ;
            B4BLOCK *blockOn ;
            int rc, seekAvail = 0 ;
            #ifdef S4FOX
               int saveDups ;
            #else
               int sign ;
            #endif

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
               {
                  error4( 0, e4parm_null, E91642 ) ;
                  return -numSkip ;
               }
            #endif
            CODE4 *c4 = t4->codeBase ;  // AS Jul 14/04 improve access

            if ( error4code( c4 ) < 0 )
               return -numSkip ;

            numLeft = numSkip ;

            blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            if ( blockOn == 0 )
            {
               if ( tfile4top( t4 ) < 0 )
                  return -numSkip ;
               blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            }

            #ifdef E4ANALYZE
               if ( ! b4leaf(blockOn) )
               {
                  error4( c4, e4index, E91642 ) ;
                  return -numSkip ;
               }
            #endif

            #ifdef S4FOX
               for(;;)
               {
                  numLeft -= b4skip( blockOn, numLeft ) ;
                  if ( numLeft == 0 )  /* Success */
                     return numSkip ;

                  B4NODE goTo ;

                  if ( numLeft > 0 )
                     b4nodeAssignNode( &goTo, blockOn->header.rightNode ) ;
                  else
                     b4nodeAssignNode( &goTo, blockOn->header.leftNode ) ;

                  if ( b4nodeInvalid( goTo ) )
                  {
                     if ( numSkip > 0 && t4->header.descending == 0 ||
                          numSkip <= 0 && t4->header.descending == 1 )
                     {
                        saveDups = tfile4block( t4 )->curDupCnt ;
                        rc = tfile4bottom( t4 ) ;
                        if ( rc < 0 )
                           return -numSkip ;
                        tfile4block( t4 )->curDupCnt = saveDups ;
                     }
                     else
                        if ( tfile4top(t4) < 0 )
                          return -numSkip ;
                     return (numSkip - numLeft) ;
                  }

                  #ifndef S4OFF_WRITE
                     if ( blockOn->changed )
                        if ( b4flush( blockOn ) < 0 )
                           return -numSkip ;
                  #endif

                  /* save the current key in case skip fails -- for leaf blocks only */
                  if ( b4leaf( blockOn ) )
                     if ( index4lockTest( t4->indexFile ) != 1 )
                     {
                        // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - make dynamic
                        if ( t4->header.keyLen + 2 * sizeof( S4LONG ) > c4->savedKeyLen )
                        {
                           if ( u4allocAgain( c4, &c4->savedKey, &c4->savedKeyLen, t4->header.keyLen + 2 * sizeof( S4LONG )  ) < 0 )
                              return e4memory ;
                        }
                        c4memcpy( c4->savedKey, (void *)b4keyKey( blockOn, b4numKeys( blockOn ) - 1 ), (unsigned int)t4->header.keyLen ) ;
                        seekAvail = 1 ;
                     }

                  rc = i4readBlock( &t4->indexFile->file, goTo, 0, blockOn ) ;
                  if ( rc < 0 )
                     return -numSkip ;

                  if ( rc == 1 )   /* failed on i/o, seek current spot to make valid */
                  {
                     if ( seekAvail == 0 )
                     {
                        error4( c4, e4index, E91642 ) ;
                        return -numSkip ;
                     }
                     #ifndef S4OPTIMIZE_OFF
                        #ifndef S4SINGLE
                           file4refresh( &t4->indexFile->file ) ;
                        #endif
                     #endif
                     rc = tfile4seek( t4, c4->savedKey, t4->header.keyLen ) ;
                     if ( rc < 0 )
                        return -numSkip ;
                     if ( rc == r4after )   /* means skipped 1 ahead */
                        numLeft-- ;
                  }

                  #ifdef S4READ_ADVANCE
                     #ifndef S4OFF_OPTIMIZE
                        if ( t4->indexFile->file.doBuffer )
                        {
                           /* advance-read the next block since skipping */
                           FILE4LONG readAdvTo ;

                           B4NODE node ;
                           if ( numSkip > 0 )
                              b4nodeAssignNode( &node, blockOn->header.rightNode ) ;
                           else
                              b4nodeAssignNode( &node, blockOn->header.leftNode ) ;

                           if ( b4nodeValid( node ) )
                           {
                              b4nodeGetFilePosition( t4->indexFile, node, &readAdvTo ) ;
                              if ( file4longGetHi( readAdvTo ) == 0 )
                                 opt4advanceReadBuf( &t4->indexFile->file, readAdvTo, i4blockSize( t4->indexFile ) ) ;  // AS May 31/04 - large file support
                           }
                        }
                     #endif
                  #endif

                  blockOn->fileBlock = goTo ;
                  assert5( b4nodeValid( blockOn->fileBlock ) ) ;
                  blockOn->builtOn = -1 ;
                  b4top( blockOn ) ;

                  if ( numLeft < 0 )
                     numLeft += b4numKeys( blockOn ) ;
                  else
                     numLeft -= 1 ;  /* moved to the next entry */
               }
            #endif

            #ifndef S4FOX
               if ( numSkip < 0)
                  sign = -1 ;
               else
                  sign = 1 ;

               for(;;)
               {
                  /* save the current key in case skip fails -- for leaf blocks only */
                  if ( b4leaf( blockOn ) )
                  {
                     // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - make dynamic
                     if ( t4->header.keyLen + 2 * sizeof( S4LONG ) > c4->savedKeyLen )
                     {
                        if ( u4allocAgain( c4, &c4->savedKey, &c4->savedKeyLen, t4->header.keyLen + 2 * sizeof( S4LONG )  ) < 0 )
                           return e4memory ;
                     }
                     c4memcpy( c4->savedKey, b4keyKey( blockOn, blockOn->keyOn ), t4->header.keyLen ) ;
                  }
                  while ( ( rc = tfile4down( t4 ) ) == 0 )
                     if ( sign < 0 )
                     {
                        blockOn = tfile4block( t4 ) ;
                        b4goEof( blockOn ) ;
                        if ( b4leaf( blockOn) )
                        {
                           blockOn->keyOn-- ;
                           #ifdef E4ANALYZE
                              if ( blockOn->keyOn < 0 )
                              {
                                 error4( c4, e4index, E91642 ) ;
                                 return -numSkip ;
                              }
                           #endif
                        }
                     }

                  if ( rc < 0 )
                     return -numSkip ;

                  if ( rc == 2 )   /* failed on i/o, seek current spot to make valid */
                  {
                     tfile4outOfDate( t4 ) ;
                     rc = tfile4seek( t4, c4->savedKey, t4->header.keyLen ) ;
                     if ( rc < 0 )
                        return -numSkip ;
                     if ( rc == r4after )   /* means skipped 1 ahead */
                        numLeft-- ;
                     continue ;
                  }

                  blockOn = tfile4block( t4 ) ;
                  if ( rc < 0 || blockOn == 0 )
                     return -numSkip ;

                  numLeft -= b4skip( blockOn, numLeft ) ;
                  if ( numLeft == 0 )      /* Success */
                     return numSkip ;

                  do  /* Skip 1 to the next leaf block  */
                  {
                     if ( (B4BLOCK *)blockOn->link.p == blockOn )
                     {
                        if ( numSkip > 0 )
                        {
                           if ( tfile4bottom( t4 ) < 0 )
                              return -numSkip ;
                        }
                        else
                           if ( tfile4top( t4 ) < 0 )
                              return -numSkip ;

                        return( numSkip - numLeft ) ;
                     }
                     else
                        if ( tfile4up( t4 ) < 0 )
                           return -numSkip ;

                     blockOn = (B4BLOCK *)t4->blocks.lastNode ;
                  }  while ( b4skip( blockOn, (long) sign) != sign) ;

                  numLeft -= sign ;
               }
            #endif
         }
      #endif /* !S4CLIPPER */



      #ifdef S4FOX
         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         static Bool5 tfile4setCollationSeqCollatable( TAG4FILE *t4 )
         {
            /* returns 1 if the tag is collatable (i.e. simple fields) */

            assert5( t4 != 0 ) ;
            assert5( t4->expr != 0 ) ;

            int fieldType = -1 ;

            for ( int infoIndex = 0 ; infoIndex < t4->expr->infoN ; infoIndex++ )
            {
               E4INFO *infoPtr = t4->expr->info + infoIndex ;
               switch ( infoPtr->functionI )
               {
                  // these are the only functions which are collatable - i.e. simple fields or
                  // simple concatenations...
                  // AS 03/30/00  - should work with everything except ASCEND, DESCEND, L2BIN
                  case E4FIELD_STR:
                  case E4FIELD_STR_CAT:
                  case E4FIELD_MEMO:
                  case E4STRING:
                  case E4CONCATENATE:
                  case E4CONCAT_TRIM:
                  case E4CONCAT_TWO:
                  case E4CHR:
                  case E4DEL:
                  case E4STR:
                  case E4STR+1:
                  case E4STRZERO:
                  case E4SUBSTR:
                  case E4TIME:
                  case E4UPPER:  // AS 03/08/00 - allow upper casing as well, since this produces a compatible string
                  case E4DTOS:
                  case E4DTOS+1:
                  case E4DTOC:
                  case E4DTOC+1:
                  case E4TRIM:
                  case E4LTRIM:
                  case E4ALLTRIM:
                  case E4LEFT:
                  case E4RIGHT:
                  case E4PADL:
                  case E4PADR:
                  case E4IIF:  // r4str entry only
                     // check that we don't have mixed types which are not collatable...
                     if ( fieldType == -1 )  // ok, just set it now...
                        fieldType = r4str ;
                     else
                     {
                        if ( fieldType != r4str )  // cannot mix unicode and string for collations...
                        {
                           // AS Aug 1/08 - allow 1 exception to the rule...if we have a simple expression that is just STR(unicode), then we can sort it...
                           if ( t4->expr->infoN == 2 && infoPtr->functionI == E4STR+1 )  // only have 2 info pieces, this one is STR() so the other is a unicode field.
                              fieldType = r4str ;
                           else
                           return 0 ;  // not collatable...
                        }
                     }
                     break ;
                  case E4FIELD_WSTR:
                  case E4FIELD_WSTR_LEN:
                  case E4FIELD_WSTR_CAT:
                  // AS May 17/05 - E4CONCATENATE+1 also acceptable (r5wstr types)
                  case E4WSTR_CONCATENATE:
                     // check that we don't have mixed types which are not collatable...
                     if ( fieldType == -1 )  // ok, just set it now...
                        fieldType = r5wstr ;
                     else
                        if ( fieldType != r5wstr )  // cannot mix unicode and string for collations...
                           return 0 ;  // not collatable...
                     break ;
                  // AS Oct 3/02 Also support the STR( double ) function
                  case E4FIELD_NUM_D:
                     // if the next info piece is e4str, then go ahead...
                     if ( ( infoIndex + 1 < t4->expr->infoN ) && ( t4->expr->info + infoIndex + 1 )->functionI == E4STR )
                     {
                        if ( fieldType == -1 )  // ok, just set it now...
                           fieldType = r4str ;
                        else
                           if ( fieldType != r4str )  // cannot mix unicode and string for collations...
                              return 0 ;  // not collatable...
                        break ;
                     }
                     return 0 ;
                  case E4FIELD_LOG:
                  case E4FIELD_NUM_S:
                  case E4FIELD_DATE_S:
                  case E4FIELD_DATE_D:
                     {
                        // AS May 10/06 is collatable if we are inside an ASCEND or DESCEND function
                        // if the next info piece is e4ascend or e4descend, then go ahead...
                        int nextFunction = ( t4->expr->info + infoIndex + 1 )->functionI ;
                        int descendStart = E4DESCEND ;
                        int ascendEnd = E4DESCEND + E4NUM_ASCEND_FUNCTIONS + E4NUM_DESCEND_FUNCTIONS ;
                        #ifdef E4DEBUG
                           int keyEqual = E4KEYEQUAL ;
                           assert5( ascendEnd == keyEqual ) ;  // ensure we have the function count correct
                        #endif
                        if ( ( infoIndex + 1 < t4->expr->infoN ) && (nextFunction >= descendStart && nextFunction < ascendEnd) )
                        {
                           if ( fieldType == -1 )  // ok, just set it now...
                              fieldType = r4str ;
                           else
                              if ( fieldType != r4str )  // cannot mix unicode and string for collations...
                                 return 0 ;  // not collatable...
                           break ;
                        }
                        // AS Feb 14/06 - if a date field, and next is DTOS it is ok...
                        if ( infoPtr->functionI == E4FIELD_DATE_S )
                        {
                           if ( ( infoIndex + 1 < t4->expr->infoN ) && ( t4->expr->info + infoIndex + 1 )->functionI == E4DTOS )
                           {
                              if ( fieldType == -1 )  // ok, just set it now...
                                 fieldType = r4str ;
                              else
                                 if ( fieldType != r4str )  // cannot mix unicode and string for collations...
                                    return 0 ;  // not collatable...
                              break ;
                           }
                        }
                        return 0 ;
                     }
                  // AS Apr 25/03 - It is also acceptable to have E4CHR( DOUBLE )
                  case E4DOUBLE:
                     // if the next info piece is e4chr, then go ahead...
                     if ( ( infoIndex + 1 < t4->expr->infoN ) && ( t4->expr->info + infoIndex + 1 )->functionI == E4CHR )
                     {
                        if ( fieldType == -1 )  // ok, just set it now...
                           fieldType = r4str ;
                        else
                           if ( fieldType != r4str )  // cannot mix unicode and string for collations...
                              return 0 ;  // not collatable...
                        break ;
                     }
                     return 0 ;
                  // AS May 10/06 - support for collating some ASCEND types
                  case E4DESCEND_STR:
                  case E4DESCEND_NUM:
                  case E4DESCEND_DATE_D:
                  case E4DESCEND_LOG:
                  case E4ASCEND_STR:
                  case E4ASCEND_NUM:
                  case E4ASCEND_DATE:
                  case E4ASCEND_DATE_D:
                  case E4ASCEND_LOG:
                     t4->hasAscendOrDescend = 1 ;
                     if ( fieldType == -1 )  // ok, just set it now...
                        fieldType = r4str ;
                     else
                        if ( fieldType != r4str )  // cannot mix unicode and string for collations...
                           return 0 ;  // not collatable...
                     break ;  // these ones are collatable...
                  default:
                     return 0 ;  // not collatable...
               }
            }

            return 1 ;  // no problems if got here, is collatable...
         }



         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         int tfile4setCollatingSeq( TAG4FILE *t4, Collate4name collateName, Bool5 setHeader )
         {
            /* if setHeader is true, it means we are calling this function from a create, and
               we want to set up the tag header strings to include the tag information.
               if setHeader is false, we only need to do a check at the end to ensure that
               they were set properly in debug case.
            */
            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            CODE4 *c4 = t4->codeBase ;
            short codePage = t4->indexFile->dataFile->codePage ;

            if ( error4code( c4 ) < 0 )
               return e4codeBase ;

            /* AS 07/27/99 --> in order to support backwards compatibility....
               if collateName == collate4none, then use the old settings to determine the sort
               sequence...

            */

            // AS 08/06/99 - we only want to support collations for expressions which are considered
            // collatable (see u4util.c collation notes).  Basically, any expression that includes
            // anything besides simple fields is considered uncollatable at this point.

            if ( collateName == collate4none )
            {
               // if the collating sequence is set to sort4machine, just use that sequence
               if ( c4->collatingSequence == sort4machine )
               {
                  t4->collateName = collate4machine ;
                  if ( setHeader )
                     c4memset( t4->header.sortSeq, 0, 8 ) ;
                  return 0 ;
               }

               // the user wants an alternate collating sequence from machine, must be one of the supported ones...
               // AS Jun 30/08 spanish collating support
               assert5( c4->collatingSequence == sort4general || c4->collatingSequence == sort4croatian ||
                        c4->collatingSequence == sort4croatianUpper || c4->collatingSequence == sort4spanish  ) ;

               // caller wants to set up the collation (setHeader = true), and the collation desired is not machine
               if ( setHeader && collateName != collate4machine )
               {
                  assert5( t4->expr != 0 ) ;  // required for collation check
                  // in this case, we need to check if the tag is actually collable (for example if it is a numeric
                  // tag we collate it as machine), so if non-character then set to machine and return
                  if ( tfile4setCollationSeqCollatable( t4 ) == 0 )   // none available...
                  {
                     t4->collateName = collate4machine ;
                     c4memset( t4->header.sortSeq, 0, 8 ) ;
                     return 0 ;
                  }
               }

               // if setHeader is true here, we are dealing with a character expression and we want to use collations
               // other than machine, we need to determine which collation we should actually use.
               // the collation depends on the code page of the data file, and the collation requested

               switch ( c4->collatingSequence )
               {
                  // AS Jun 30/08 spanish collating support
                  case sort4spanish:
                     switch( codePage )
                     {
                        case cp1252: // for windows ansi codePage we sort with general collation cp1252
                        case cp0: // cp0 defaults to windows ansi codepage (1252), so sort with general collation cp1252
                           t4->collateName = collate4spanishCp1252 ;
                           break ;
                        case cp850: // for windows ansi codePage we sort with general collation cp850
                           t4->collateName = collate4spanishCp850 ;
                           break ;
                        default:  // unknown and invalid
                           return error4describe( c4, e4notSupported, E91642, "attempt to use unsupported CodePage", 0, 0 ) ;
                     }
                     break ;
                  case sort4croatian:  // case sensitive croatian
                     t4->collateName = collate4croatianCp1250 ;
                     break ;
                  case sort4croatianUpper:  // case insensitive croatian
                     t4->collateName = collate4croatianUpperCp1250 ;
                     break ;
                  default:  // otherwise it is dependent on the code page
                     switch( codePage )
                     {
                        case cp1252: // for windows ansi codePage we sort with general collation cp1252
                        case cp0: // cp0 defaults to windows ansi codepage (1252), so sort with general collation cp1252
                           t4->collateName = collate4generalCp1252 ;
                           break ;
                        // AS Jun 9/04 - Support for CodePage 850
                        case cp850: // for windows ansi codePage we sort with general collation cp850
                           t4->collateName = collate4generalCp850 ;
                           break ;
                        case cp437: // for windows ansi codePage we sort with general collation cp437
                           t4->collateName = collate4generalCp437 ;
                           break ;
                        case cp1250:  // windows eastern european
                        case cp0004:  // windows eastern european (old)
                           t4->collateName = collate4croatianCp1250 ;  // default to croatian
                           break ;
                        default:  // unknown and invalid
                           return error4describe( c4, e4notSupported, E91642, "attempt to use unsupported CodePage", 0, 0 ) ;
                     }
               }

               // also, if done this way, we have a FoxPro compatible file, so mark the GENERAL entry
               // in the tag header...
               if ( setHeader )
               {
                  // for specialized sequences we use CBxxxxxx (where the xxxxxx varies) to indicate the actual collation
                  // for non-specialized sequences we use GENERAL which is FoxPro compatible to indicate we are using the
                  // GENERAL collating sequence.
                  switch( t4->collateName )
                  {
                     case collate4spanishCp850:
                     case collate4spanishCp1252:
                     case collate4croatianCp1250:  // case sensitive croatian
                     case collate4croatianUpperCp1250:  // case insensitive croatian
                        // generate name for header...
                        c4memcpy( t4->header.sortSeq, "CB", 2 ) ; // indicate we are using a CodeBase collation...
                        // use the collateName ordinal as the indicator.  Fill in with 0's on the left, so
                        // we get:  "CB00001" for CodeBase collation 1...
                        c4ltoa45( (long)t4->collateName, t4->header.sortSeq + 2, -5 ) ;
                        t4->header.sortSeq[7] = 0 ;  // null end the string
                        break ;
                     default:
                        c4memcpy( t4->header.sortSeq, "GENERAL\0", 8 ) ;
                        break ;
                  }
               }

               return 0 ;
            }

            if ( setHeader && collateName != collate4machine )
            {
               assert5( t4->expr != 0 ) ;  // required for collation check
               if ( tfile4setCollationSeqCollatable( t4 ) == 0 )   // none available...
                  collateName = collate4machine ;
            }

            t4->collateName = collateName ;
            if ( setHeader )
            {
               if ( collateName == collate4machine )
               {
                  // if machine, then just set the header to 0
                  c4memset( t4->header.sortSeq, 0, 8 ) ;
               }
               else
               {
                  // AS 09/02/99 We want to create the file as FoxPro-compatible if possible
                  // (i.e. if general collating sequence choice matches data4's code page...)
                  // AS Jun 9/04 - Support for CodePage 850
                  if ( ( collateName == collate4generalCp1252 && codePage == cp1252 ) ||
                       ( collateName == collate4generalCp437 && codePage == cp437  ) ||
                       ( collateName == collate4generalCp850 && codePage == cp850  )
                        )
                  {
                     c4memcpy( t4->header.sortSeq, "GENERAL\0", 8 ) ;
                  }
                  else
                  {
                     // generate name for header...
                     c4memcpy( t4->header.sortSeq, "CB", 2 ) ; // indicate we are using a CodeBase collation...
                     // use the collateName ordinal as the indicator.  Fill in with 0's on the left, so
                     // we get:  "CB00001" for CodeBase collation 1...
                     c4ltoa45( (long)collateName, t4->header.sortSeq + 2, -5 ) ;
                     t4->header.sortSeq[7] = 0 ;  // null end the string
                  }
               }
            }

            return 0 ;
         }
      #endif /* S4FOX */



      #if !defined( S4CLIPPER ) && !defined( S4OFF_WRITE )
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, !S4OFF_WRITE */
         B4BLOCK *tfile4split( TAG4FILE *t4, B4BLOCK *oldBlock )
         {
            #ifdef E4PARM_LOW
               if ( t4 == 0 || oldBlock == 0 )
               {
                  error4( 0, e4parm_null, E91642 ) ;
                  return 0 ;
               }
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return 0 ;

            #ifdef E4INDEX_VERIFY
               if ( b4verify( oldBlock ) == -1 )
               {
                  error4describe( oldBlock->tag->codeBase, e4index, E91642, t4->alias, 0, 0 ) ;
                  return 0 ;
               }
            #endif

            B4NODE newFileBlock = index4extend( t4->indexFile ) ;
            if ( b4nodeInvalid( newFileBlock ) )
               return 0 ;
            // AS Apr 16/10 - the node also should not be 0 which would imply we have rolled over to the start of the index file for some reason
            if ( b4node( newFileBlock) == 0 )
            {
               error4describe( oldBlock->tag->codeBase, e4index, E91642, t4->alias, 0, 0 ) ;
               return 0 ;
            }

            B4BLOCK *newBlock = b4alloc( t4, newFileBlock ) ;
            if ( newBlock == 0 )
               return 0 ;

            // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE
            newBlock->changed = 1 ;
            #ifdef S4FOX
               newBlock->dupByteCnt = oldBlock->dupByteCnt ;
               newBlock->trailByteCnt = oldBlock->trailByteCnt ;
            #endif
            oldBlock->changed = 1 ;

            #ifdef S4FOX
               int rc ;
               if ( b4leaf( oldBlock ) )
                  rc = tfile4leafSplit( t4, oldBlock, newBlock ) ;
               else
                  rc = tfile4branchSplit( t4, oldBlock, newBlock ) ;

               if ( rc < 0 )
                  return 0 ;

               newBlock->header.rightNode = oldBlock->header.rightNode ;
               newBlock->header.leftNode = oldBlock->fileBlock ;
               oldBlock->header.rightNode = newBlock->fileBlock ;

               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "tfile4split: Tagname: %s, newRightNode = %ld, newLeftNode = %ld, oldRightNode = %ld, OldLeftNode = %ld\r\n",
                              t4->alias, (long)b4node(newBlock->header.rightNode), (long)b4node(newBlock->header.leftNode), (long)b4node(oldBlock->header.rightNode), (long)b4node(oldBlock->header.leftNode) ) ;
                     log5( dump ) ;
                  }
               #endif

               if ( b4nodeValid( newBlock->header.rightNode ) )   /* must change left ptr for next block over */
               {
                  #ifdef S4BYTE_SWAP
                     newBlock->fileBlock.node = x4reverseLong( (void *)&newBlock->fileBlock ) ;
                  #endif
                  FILE4LONG pos ;
                  b4nodeGetFilePosition( t4->indexFile, newBlock->header.rightNode, &pos ) ;
                  file4longAdd( &pos, 2 * sizeof( short ) ) ;
                  rc = file4writeInternal( &t4->indexFile->file, pos, &newBlock->fileBlock, sizeof( newBlock->header.leftNode ) ) ;
                  if ( rc < 0 )
                     return 0 ;
                  #ifdef S4BYTE_SWAP
                     newBlock->fileBlock.node = x4reverseLong( (void *)&newBlock->fileBlock ) ;
                  #endif
               }
            #else
               /* NNNNOOOO  N - New, O - Old */
               newBlock->nKeys  = (short)( ( b4numKeys( oldBlock ) + 1 ) / 2 ) ;
               oldBlock->nKeys -= b4numKeys( newBlock ) ;
               newBlock->keyOn  = oldBlock->keyOn ;

               int totLen = t4->indexFile->header.blockRw - sizeof(oldBlock->nKeys) - sizeof(oldBlock->dummy) ;
               int newLen = b4numKeys( newBlock ) * t4->header.groupLen ;

               c4memcpy( (void *)b4key( newBlock, 0 ), (void *)b4key( oldBlock, 0 ), newLen ) ;
               c4memmove( b4key(oldBlock,0), b4key( oldBlock, b4numKeys( newBlock )), totLen - newLen ) ;
               oldBlock->keyOn = oldBlock->keyOn - b4numKeys( newBlock ) ;
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     struct _timeb mTime ;
                     char *outTime, dump[255] ;
                     _ftime( &mTime ) ;
                     outTime = ctime( &( mTime.time ) ) ;

                     sprintf( dump, "id = %ld, split: old-fb = %uld, new-fb = %uld, time: %s\r\n", (long)t4->indexFile, oldBlock->fileBlock, newBlock->fileBlock, outTime ) ;
                     log5( dump ) ;
                     B4KEY_DATA *keyData = b4key( newBlock, 1 )  ;
                     sprintf( dump, "          1st split new fb key: recno = %ld, key = %s\r\n", (long)keyData->num, (char *)keyData->value ) ;
                     log5( dump, 1 ) ;
                  }
               #endif
            #endif

            return newBlock ;
         }
      #endif /* #if !defined( S4CLIPPER ) && !defined( S4OFF_WRITE ) */
   #endif /* S4CLIENT else */
#endif /* !S4OFF_INDEX */



// AS Jan 30/07 - exported for off-index...
int S4FUNCTION t4versionCheckExport( TAG4 *t4, const int d1, const int d2 )
{
   #if defined( S4CLIENT ) || defined( S4OFF_INDEX )
      return e4notSupported ;
   #else
      #ifdef S4CLIPPER
         return tfile4versionCheck( t4->tagFile, d1, d2 ) ;  // AS Nov 25/03 - copmile fix
      #else
         #ifdef S4OFF_OPTIMIZE
            return i4versionCheck( t4->index, d1, d2 ) ;
         #else
            if ( t4->index->indexFile->file.doBuffer == 0 )
               return i4versionCheck( (t4)->index, d1, d2 ) ;
            else
               return 0 ;
         #endif
      #endif
   #endif
}



#ifndef S4OFF_INDEX
   #ifndef S4CLIPPER
      /* !S4OFF_INDEX, !S4CLIPPER */
      int S4FUNCTION tfile4top( TAG4FILE *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         CODE4 *c4 = t4->codeBase ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         #ifdef S4CLIENT
            CONNECTION4 *connection = &c4->defaultServer ;
            #ifdef E4ANALYZE
               if ( connection == 0 )
                  return error4( c4, e4struct, E91642 ) ;
            #endif

            t4->tagDataValid = 0 ;  // reset to invalid
            tfile4cacheReset( t4 ) ;
            int rc = connection4assign( connection, CON4TAG_TOP, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
            if ( rc < 0 )
               return rc ;

            CONNECTION4TAG_TOP_INFO_IN *infoIn ;
            CONNECTION4TAG_TOP_INFO_OUT *out ;
            connection4addData( connection, NULL, sizeof(CONNECTION4TAG_TOP_INFO_IN), (void **)&infoIn ) ;
            memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
            infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
            u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;
            rc = connection4sendMessage( connection ) ;
            if ( rc == 0 )
               rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               return error4stack( data->codeBase, rc, E91642 ) ;
            int saveRc = connection4status( connection ) ;
            if ( saveRc < 0 )
               return connection4error( connection, c4, saveRc, E94701 ) ;
            int conLen = connection4len( connection ) ;
            if ( conLen < sizeof( CONNECTION4TAG_TOP_INFO_OUT ) )
               return error4( c4, e4packetLen, E91642 ) ;
            out = (CONNECTION4TAG_TOP_INFO_OUT *)connection4data( connection ) ;
            t4->recNo = ntohl5(out->recNo) ;
            if ( t4->recNo == 0 )  // if the recno is invalid, must be an empty tag, just return saveRc (don't copy key or make tag valid)
               return saveRc ;

            // AS Feb 9/09 - added support to return key as well
            t4->currentKeyLen = (unsigned long)(ntohl5(out->keyLen)) ;
            if ( conLen != sizeof( CONNECTION4TAG_TOP_INFO_OUT ) + t4->currentKeyLen )
               return error4( c4, e4packetLen, E91642 ) ;
            if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
            {
               if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
               {
                  #ifdef E4STACK
                     error4stack( c4, e4memory, E91642 ) ;
                  #endif
                  return 0 ;
               }
            }
            memcpy( t4->currentKey, ((char *)connection4data( connection )) + sizeof(CONNECTION4TAG_TOP_INFO_OUT), t4->currentKeyLen ) ;

            t4->tagDataValid = 1 ;

            return saveRc ;
         #else
            #ifdef S4FOX
               if ( t4->header.descending )   /* if descending, go top means go bottom */
                  return tfile4rlBottom( t4 ) ;
               else
                  return tfile4rlTop( t4 ) ;
            #else
               int rc, rc2 ;

               do
               {
                  rc = tfile4upToRoot( t4 ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, rc, E91642 ) ;

                  if ( rc != 2 )
                  {
                     ((B4BLOCK *)t4->blocks.lastNode)->keyOn = 0 ;
                     do
                     {
                        if ( (rc = tfile4down(t4)) < 0 )
                           return error4stack( t4->codeBase, rc, E91642 ) ;
                        ((B4BLOCK *)t4->blocks.lastNode)->keyOn = 0 ;
                     } while ( rc == 0 ) ;
                  }

                  if ( rc == 2 )   /* failed due to read while locked */
                  {
                     rc2 = tfile4outOfDate( t4 ) ;
                     return error4stack( t4->codeBase, rc2, E91642 ) ;
                  }
               } while ( rc == 2 ) ;

               return 0 ;
            #endif
         #endif
      }
   #endif /* !S4CLIPPER */


   #ifndef S4CLIENT
      /* !S4OFF_INDEX, !S4CLIENT */
      int tfile4unique( TAG4FILE *tag, const short int errUnique )
      {
         #ifdef S4FOX
            // AS Apr 7/03 - typeCode will be 0x04 for r4candidate/e4candidate - simplify this comparison
            if ( tag->header.typeCode & 0x05 )
         #else
            if ( tag->header.unique )
         #endif
               return errUnique ;
         return 0 ;
      }



      /* !S4OFF_INDEX, !S4CLIENT */
      int tfile4up( TAG4FILE *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( t4->blocks.lastNode == 0 )
            return 1 ;

         LINK4 *lnk = (LINK4 *)l4pop( &t4->blocks ) ;

         l4add( &t4->saved, lnk ) ;
         return 0 ;
      }



      #ifndef S4CLIPPER
         #ifndef S4OFF_WRITE
            /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, !S4OFF_WRITE */
            int tfile4update( TAG4FILE *t4 )
            {
               B4BLOCK *blockOn ;
               int rc ;
               FILE4LONG pos ;

               // LY May 9/05 : added E4PARM_HIGH (if S4OFF_MULTI
               // and index creation fails due to locked index file, index4update()
               // still gets here with invalid TAG4FILE pointer)
               #if defined( E4PARM_LOW ) || defined( E4PARM_HIGH )
                  if ( t4 == 0 )
                  #ifdef E4PARM_LOW
                     return error4( 0, e4parm_null, E91642 ) ;
                  #else
                     return error4stack( 0, e4parm_null, E91642 ) ;
                  #endif
               #endif

               if ( error4code( t4->codeBase ) < 0 )
                  return e4codeBase ;

               for( blockOn = 0 ;; )
               {
                  blockOn = (B4BLOCK *)l4next(&t4->saved,blockOn) ;
                  if ( blockOn == 0 )
                     break ;
                  assert5( b4nodeValid( blockOn->fileBlock ) || blockOn->changed == 0 ) ;
                  rc = b4flush( blockOn ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;
               }

               for( blockOn = 0 ;; )
               {
                  blockOn = (B4BLOCK *)l4next(&t4->blocks,blockOn) ;
                  if ( blockOn == 0 )
                     break ;
                  rc = b4flush( blockOn ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;
               }

               if ( t4->rootWrite )
               {
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255] ;
                        sprintf( dump, "rootWriting: Tagname: %s, i4file = %ld, root = %ld\r\n", t4->alias, (long)t4->indexFile, (long)b4node(t4->header.root) ) ;
                        log5( dump ) ;
                     }
                  #endif
                  #ifdef S4BYTE_SWAP
                     t4->header.root.node = x4reverseLong( (void *)&t4->header.root ) ;
                  #endif

                  b4nodeGetFilePosition( t4->indexFile, t4->headerOffset, &pos ) ;
                  rc = file4writeInternal( &t4->indexFile->file, pos, &t4->header.root, sizeof(t4->header.root)) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;

                  #ifdef S4BYTE_SWAP
                     t4->header.root.node = x4reverseLong( (void *)&t4->header.root ) ;
                  #endif

                  t4->rootWrite = 0 ;
               }

               return 0 ;
            }
         #endif /* !S4OFF_WRITE */



         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         int tfile4upToRoot( TAG4FILE *t4 )
         {
            LINK4 *linkOn ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            for ( ;; )
            {
               linkOn = (LINK4 *)l4pop( &t4->blocks ) ;
               if ( linkOn == 0 )
                  return tfile4down( t4 ) ;
               l4add( &t4->saved, linkOn ) ;
            }
         }
      #endif /* !S4CLIPPER */
   #endif /* !S4CLIENT */



   /* !S4OFF_INDEX */
   short int S4FUNCTION t4unique( const TAG4 *tag )
   {
      #ifdef E4VBASIC
         #ifdef S4CB51
            if ( c4parm_check ( tag, 4, E40150 ) ) return 0 ;
         #else
            if ( c4parm_check ( tag, 4, E91639 ) ) return 0 ;
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( tag == 0 )
            return error4( 0, e4parm_null, E91639 ) ;
         if ( tag->tagFile == 0 )
            return error4( 0, e4parm, E91639 ) ;
      #endif

      #ifdef S4CLIENT
         return tag->errUnique ;
      #else
         return tfile4unique( tag->tagFile, tag->errUnique ) ;
      #endif
   }



   #ifndef S4SERVER
      /* !S4OFF_INDEX, !S4SERVER */
      int S4FUNCTION t4uniqueSet( TAG4 *t4, const short uniqueCode )
      {
         #ifdef E4VBASIC
            if ( c4parm_check ( t4, 4, E91601 ) ) return 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91601 ) ;
         #endif

         if ( uniqueCode == t4unique( t4 ) )  /* possibly both zero ok */
            return 0 ;

         /* if not zero matches, then neither should be zero */
         #ifdef E4PARM_HIGH
            #ifdef S4CLIENT_OR_FOX
               if ( t4unique( t4 ) == r4candidate )
               {
                  if ( uniqueCode != e4candidate )
                     return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
                  return t4uniqueSetLow( t4, uniqueCode, 1 ) ;
               }
               if ( t4unique( t4 ) == e4candidate )
               {
                  if ( uniqueCode != r4candidate )
                     return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
                  return t4uniqueSetLow( t4, uniqueCode, 1 ) ;
               }
            #endif
            if ( uniqueCode != e4unique && uniqueCode != r4unique && uniqueCode != r4uniqueContinue )
               return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
         #endif

         return t4uniqueSetLow( t4, uniqueCode, 1 ) ;
      }
   #endif /* !S4SERVER */



   #ifdef S4CLIENT
      /*  !S4OFF_INDEX, S4CLIENT */
      int S4FUNCTION t4uniqueSetLow( TAG4 *t4, const short uniqueCode, const char doZeroCheck )
      {
         CODE4 *c4 = t4->index->data->codeBase ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         #ifndef S4OFF_TRAN
            if ( code4transEnabled( c4 ) && doZeroCheck )  /* user cannot request from within transaction */
            {
               if ( code4trans( c4 )->currentTranStatus == r4active )
                  return error4( c4, e4transViolation, E81608 ) ;
            }
         #endif

         if ( uniqueCode == t4unique( t4 ) )
            return 0 ;

         /* verify that the tag is unique before setting */
         if ( doZeroCheck )
         {
            if ( t4unique( t4 ) == 0 )
               return error4( c4, e4parm, E81609 ) ;
         }

         CONNECTION4 *connection = t4->index->data->dataFile->connection ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
               return error4( c4, e4struct, E91601 ) ;
         #endif

         int rc = connection4assign( connection, CON4UNIQUE_SET, data4clientId( t4->index->data ), data4serverId( t4->index->data ) ) ;
         if ( rc < 0 )
            return rc ;

         CONNECTION4UNIQUE_INFO_IN *infoIn ;
         connection4addData( connection, NULL, sizeof(CONNECTION4UNIQUE_INFO_IN), (void **)&infoIn ) ;
         CONNECTION4UNIQUE_TAG_INFO *tagInfo ;
         connection4addData( connection, NULL, sizeof(CONNECTION4UNIQUE_TAG_INFO), (void **)&tagInfo ) ;
         infoIn->numTags = htons5(1) ;
         tagInfo->unique = htons5(uniqueCode) ;
         c4memcpy( tagInfo->alias, t4->tagFile->alias, LEN4TAG_ALIAS ) ;
         tagInfo->alias[LEN4TAG_ALIAS] = 0 ;
         connection4sendMessage( connection ) ;

         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E91601 ) ;

         rc = connection4status( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E91601 ) ;

         t4->errUnique = uniqueCode ;

         return rc ;
      }
   #endif /* S4CLIENT */



   #ifdef S4STAND_ALONE
   /* !S4OFF_INDEX, S4STAND_ALONE */
   int S4FUNCTION t4uniqueSetLow( TAG4 *t4, const short uniqueCode, const char doZeroCheck )
   {
      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91601 ) ;
         #ifdef S4CLIENT_OR_FOX
            if ( t4unique( t4 ) == r4candidate )  /* can't change a candidate setting */
            {
               if ( uniqueCode != e4candidate )
                  return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
            }
            else
               if ( t4unique( t4 ) == e4candidate )  /* can't change a candidate setting */
               {
                  if ( uniqueCode != r4candidate )
                     return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
               }
               else
         #endif
         if ( uniqueCode != e4unique && uniqueCode != r4unique && uniqueCode != r4uniqueContinue && uniqueCode != 0 )
            return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
      #endif

      /* verify that the tag is unqiue before setting */
      if ( doZeroCheck )
         if ( t4unique( t4 ) == 0 )
            return error4( t4->index->data->codeBase, e4parm, E81609 ) ;

      t4->errUnique = uniqueCode ;

      return 0 ;
   }
   #endif /* S4STAND_ALONE */


   #ifndef S4CLIENT
      int S4FUNCTION t4infoN( TAG4 *tag )
      {
         // for odbc - returns -1 if tag->expr is invalid
         // AS 06/22/00 the 'expr' member may be NULL if the expression is invalid but file is read-only (i.e. not changing, so just avoid tag)
         if ( tag->tagFile->expr == 0 )
            return -1 ;
         return tag->tagFile->expr->infoN ;
      }



      void S4FUNCTION t4exprContextSet( TAG4 *tag, DATA4 *data )
      {
         // for odbc
         expr4context( tag->tagFile->expr, data ) ;
      }



      FIELD4 * S4FUNCTION t4field( TAG4 *tag )
      {
         // for odbc
         // for odbc - returns 0 if tag->expr is invalid
         // AS 06/22/00 the 'expr' member may be NULL if the expression is invalid but file is read-only (i.e. not changing, so just avoid tag)
         if ( tag->tagFile->expr == 0 )
            return 0 ;
         return tag->tagFile->expr->info[0].fieldPtr ;
      }



      INDEX4 * S4FUNCTION t4index( TAG4 *tag )
      {
         // for odbc
         return tag->index ;
      }



      int S4FUNCTION tfile4exprNullLow( TAG4FILE *tagFile )
      {
         // for odbc
         return expr4nullLow( tagFile->expr, 0 ) ;
      }



      TAG4FILE * S4FUNCTION t4tagFile( TAG4 *tag )
      {
         // for odbc
         return tag->tagFile ;
      }
   #endif /* !S4CLIENT */



   // AS Oct 22/02 - support for t4seekN in client/server
   int t4seekNdo( DATA4 *data, TAG4 *tag, const char *seekValue, const short inputKeyLen, short doDataPosition )
   {
      // AS Nov 29/04 - fix for client version...also select tag and store
      // the same as d4seek(), with optional positioning of data file
      #ifdef E4PARM_HIGH
         if ( tag == 0 || seekValue == 0 || tag->tagFile == 0 )
            return error4( 0, e4parm_null, E91639 ) ;
      #endif

      TAG4 *tagSelected = d4tagSelected( data ) ;
      d4tagSelect( data, tag ) ;
      int rc ;
      #ifdef S4CLIENT
         // without a data position, main purpose is to give seek result
         rc = d4seekServer( data, seekValue, inputKeyLen, 0, CON4SEEK, (Bool5)doDataPosition ) ;
      #else
         rc = d4seekNLow( data, seekValue, inputKeyLen, doDataPosition ) ;
      #endif
      d4tagSelect( data, tagSelected ) ;

      return rc ;
   }



   #ifndef S4SERVER
      int S4FUNCTION t4seekN( TAG4 *tag, const char *seekValue, const short inputKeyLen, short doDataPosition )
      {
         return t4seekNdo( tag->index->data, tag, seekValue, inputKeyLen, doDataPosition ) ;
      }
   #endif
#endif /* S4OFF_INDEX */


#ifdef S4CLIENT
   // resets the cache for the next skip (e.g. if seek is called the cache gets reset)
   void tfile4cacheReset( TAG4FILE *t4 )
   {
      if ( t4->cache != 0 ) // reset cache position
      {
         t4->cache->pos = -1 ;
         t4->cache->modusCurrent = t4->cache->modusOriginal ;  // reset the cache size for doubling scenario
         t4->cache->cacheSize = t4->cache->numRowsIn ; // -> CORRECTION from customer AS Oct 8/09
      }
   }



   static int tfile4cacheInit( TAG4FILE *t4, long numRows, long modus, Bool5 modusReallocate )
   {
      // modusReallocate is set to 1 if for the purposes of this function we are reallocating if required to handle a larger data set
      #ifdef E4PARM_LOW
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91642 ) ;
      #endif

      CODE4 *c4 = t4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( t4->cache == 0 )
      {
         t4->cache = (KEY4CACHE *)u4allocEr( c4, sizeof( KEY4CACHE ) ) ;
         if ( t4->cache == 0 )
            return e4memory ;
         memset( t4->cache, 0, sizeof( KEY4CACHE ) ) ;
      }
      else
      {
         Bool5 doReset = 0 ;
         if ( modusReallocate == 1 )
         {
            if ( t4->cache->allocSize < numRows )  // we need to reallocate for more rows in this case only
               doReset = 1 ;
         }
         else
         {
            if ( modus == t4->cache->modusOriginal )  // no change in modus input,
            {
               if ( modus == 0 )
               {
                  if ( t4->cache->allocSize < numRows )  // we need to reset in this case only
                     doReset = 1 ;
               }
               else // modus not zero, so check if the numrows input has changed or not
               {
                  if ( numRows != t4->cache->numRowsIn )  // means reset the functionality
                     doReset = 1 ;
               }
            }
            else // the modus has changed, so reset
               doReset = 1 ;
         }

         if ( doReset == 0 )  // don't reset, just update if required and return
         {
            // don't change the cacheSize if it matches the original input value (modus changing)
            if ( numRows != t4->cache->numRowsIn )  // means reset the functionality
            {
               t4->cache->cacheSize = numRows ;
               if ( modusReallocate == 0 )  // if set to 1 it means we are reallocating based on results from server in which case we aren't really resetting this value
                  t4->cache->numRowsIn = numRows ;
            }
            return 0 ;
         }
      }

      // we still may not need to do the freeing up if the buffer is big enough and we are just resetting the starting values
      if ( t4->cache->allocSize < numRows )
      {
         // AS Jul 14/09 need to free the cache as it may be reallocating
         tfile4freeCache( t4, 0 ) ;
         t4->cache->recNos = (long *)u4allocEr( c4, sizeof( long ) * numRows ) ;
         if ( t4->cache->recNos == 0 )
            return e4memory ;

         t4->cache->keys = (char *)u4allocEr( c4, tfile4keyLen( t4 ) * numRows ) ;
         if ( t4->cache->keys == 0 )
         {
            u4free( t4->cache->recNos ) ;
            t4->cache->recNos = 0 ;
            return e4memory ;
         }
      }
      t4->cache->pos = -1 ;  // reset the position
      t4->cache->cacheSize = numRows ;
      t4->cache->allocSize = numRows ;
      t4->cache->currentSize = 0 ;
      if ( modusReallocate == 0 )  // in this case we also need to rest the starting values
      {
         t4->cache->numRowsIn = numRows ;
         t4->cache->modusOriginal = modus ;
         t4->cache->modusCurrent = modus ;
      }
      return 0 ;
   }



   void tfile4freeCache( TAG4FILE *t4, Bool5 doCache )
   {
      // optionally choose whether to free up the t4->cache (we don't want to in the case of reallocating the contents due to a size change)
      if ( t4 == 0 )
         return ;
      if ( t4->cache != 0 )
      {
         if ( t4->cache->recNos != 0 )
         {
            u4free( t4->cache->recNos ) ;
            t4->cache->recNos = 0 ;
         }
         if ( t4->cache->keys != 0 )
         {
            u4free( t4->cache->keys ) ;
            t4->cache->keys = 0 ;
         }
         t4->cache->allocSize = 0 ;
         t4->cache->cacheSize = 0 ;
         if ( doCache )
         {
            u4free( t4->cache ) ;
            t4->cache = 0 ;
         }
      }
   }
#endif /* S4CLIENT */



// AS Feb 6/09 - New functions for cacheing
// AS Jul 14/09 - Updates to extend functionality
S4EXPORT int S4FUNCTION tfile4skipCache( TAG4FILE S4PTR *t4, long numRowsIn, long modus )
{
   /*
      Skips one row forward in the tag, refreshing the cache from the server if required
      - if numRows is negative, then skips backwards 1 in the cache

      numRows is the number of rows to keep cached
      - if there are cached rows, this function just updates the current tag record with the info from the cache
      - if there are no cached rows, the next numRows are read from the server and cached

      Modus describes how the cacheing should be modified
      - 0 means original functionality as described above
      - >=1, numRowsIn is the number of rows to cache, and the number of rows to cache is doubled (a maximum of modus times) every time we reach the end of the cache.
        e.g. file4skipCache(tf4,5,4) -> On the first call, the server return 5 rows. If the caching is continued after 5 skips, the server returns 10 rows, then 20, 40, 80 rows.
        Each call after that returns 80 rows again. The 5 rows from numRowsin are doubled 4 times
      - <=0, numRowsIn remains the maximum number of rows to cache, and abs(modus) is the number of characters that have to match at the beginning of the key.
      - if the modus value is changed from previous calls it has the effect of reinitializing the functionality and any current bufferred rows are discarded
      - if the numRowsIn value changes from previous calls and modus is not zero  it has the effect of reinitializing the functionality and any current bufferred rows are discarded

      returns 0 if skipped one forwards, r4eof if at eof, <0 if error

      The following is required (on the TAG4FILE object)
      - A cache to hold the keys
      -- must be able to change in size since the numRows parameter may change
      - the cache won't be reallocated if a larger numRowsIn is passed in if the cache still contains rows in the buffer, but will reallocate on the next fetch if the size differs
      - A number indicating the size of the current cache (don't reallocate if it is large enough)
      - A number indicating the number of currently cached rows
      - cachePos - A signed number indicating the current location within the cached rows (set to -1 to indicate cache is cleared) (0 means 1st entry)
      - cache is considered clear if either the buffer is null or the current location is set to -1
     - when freeing the TAG4FILE() we need to free up the buffer
     - functions that will clear the cache:
        - tfile4top/tfile4bottom/tfile4skip/tfile4dskip/tfile4go/tfile4positionSet/tfile4seek
     - functions that will use the cache
        - tfile4eof(), tfile4exprKeyExport(), tfile4key(), tfile4position(), tfile4recNo()
   */

   #ifdef S4CLIENT
      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91642 ) ;
         if ( numRowsIn == 0 )
            return error4( t4->codeBase, e4parm, E91642 ) ;
         if ( t4->tagDataValid== 0 )  // not at a valid start position
            return error4describe( t4->codeBase, e4parm, E91642, "The call to tfile4skipCache() is using a tag that hasn't been positioned yet", 0, 0 ) ;
      #endif

      long numRows ;
      short posSkip ;
      if ( numRowsIn < 0 )
      {
         posSkip = -1 ;
         numRows = -numRowsIn ;
      }
      else
      {
         posSkip = 1 ;
         numRows = numRowsIn ;
      }

      CODE4 *c4 = t4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      CONNECTION4 *connection = &c4->defaultServer ;
      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( c4, e4struct, E91601 ) ;
      #endif

      // first check if already at eof, in which case just return that status
      int rc = tfile4eof( t4 ) ;
      if ( rc != 0 )
         return rc ;

      rc = tfile4cacheInit( t4, numRows, modus, 0 ) ;
      if ( rc != 0 )
         return rc ;

      if ( t4->cache == 0 )
         return error4( c4, e4struct, E91601 ) ;

      Bool5 readFromCache = 0 ;

      if ( t4->cache->pos == -1 || ((t4->cache->pos+posSkip) >= t4->cache->currentSize || (t4->cache->pos+posSkip) < 0 ) ) // need to re-read from server
      {
         if ( modus > 0 )
         {
            if ( t4->cache->modusCurrent > 0 && t4->cache->pos != -1 )  // adjust skipping
            {
               t4->cache->modusCurrent-- ;
               rc = tfile4cacheInit( t4, t4->cache->cacheSize * 2, modus, 1 ) ;
               if ( rc != 0 )
                  return rc ;

               if ( t4->cache == 0 )
                  return error4( c4, e4struct, E91601 ) ;

            }
            // reset the numRows value since we are using the modus to increase the size
            if ( numRowsIn < 0 )
               numRowsIn = -t4->cache->cacheSize ;
            else
               numRowsIn = t4->cache->cacheSize ;
            numRows = t4->cache->cacheSize ;
         }


         int rc = connection4assign( connection, CON4TAG_CACHE_SKIP, data4clientId( t4->refData ), data4serverId( t4->refData ) ) ;
         if ( rc < 0 )
            return rc ;

         CONNECTION4TAG_CACHE_INFO_IN *infoIn ;
         CONNECTION4TAG_CACHE_INFO_OUT *out ;
         connection4addData( connection, NULL, sizeof(CONNECTION4TAG_CACHE_INFO_IN), (void **)&infoIn ) ;
         memcpy( infoIn->tagName, t4->alias, LEN4TAG_ALIAS ) ;
         infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
         if ( t4->tagDataValid == 1 )  // use the starting recno...
            infoIn->startRecno = htonl5( t4->recNo ) ;
//         else
//            infoIn->startRecno = htonl5( 0L ) ;
         infoIn->numRows = htonl5( numRowsIn ) ;
         infoIn->modus =  htonl5( modus ) ;
         t4->tagDataValid = 0 ;  // reset to invalid
         u4ncpy( infoIn->indexName, t4->indexFile->accessName, strlen( t4->indexFile->accessName ) + 1 ) ;

         rc = connection4sendMessage( connection ) ;
         if ( rc == 0 )
            rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( data->codeBase, rc, E94701 ) ;

         int saveRc = connection4status( connection ) ;
         if ( saveRc < 0 )
            return connection4error( connection, c4, saveRc, E94701 ) ;
         if ( saveRc != 0 ) // e.g. r4eof
            return saveRc ;

         out = (CONNECTION4TAG_CACHE_INFO_OUT *)connection4data( connection ) ;
         int conLen = connection4len( connection ) ;
         if ( conLen < sizeof( CONNECTION4TAG_CACHE_INFO_OUT ) )
            return error4( c4, e4packetLen, E91642 ) ;

         long numRowsFromServer = ntohl5(out->numRows)*posSkip ;  // the # of rows retrieved (may be less than # requested, e.g. eof)
         long keyLen = ntohl5(out->keyLen) ;
         if ( numRowsFromServer > numRows ||
              numRowsFromServer < 0 ||
              conLen != (sizeof( CONNECTION4TAG_CACHE_INFO_OUT ) + numRowsFromServer * ( sizeof(long) + keyLen )) ||
              keyLen != tfile4keyLen( t4 ) )  // error
            return error4( c4, e4packetLen, E91642 ) ;

         // possibly may need to reallocate (if the # of rows is larger than our cache)
         rc = tfile4cacheInit( t4, max( numRowsFromServer, numRows ), modus, 1 ) ;
         if ( rc != 0 )
            return rc ;

         for ( int lp = 0 ; lp < numRowsFromServer ; lp++ )
         {
            t4->cache->recNos[lp] = ntohl5( *(long *)(connection4data(connection) + sizeof( CONNECTION4TAG_CACHE_INFO_OUT )+ lp * sizeof( long ) ) ) ;
            #ifdef E4MISC
               if ( t4->cache->recNos[lp] <= 0 || t4->cache->recNos[lp] > tfile4reccount( tagFile ) )
                  return error4( c4, e4packetLen, E91642 ) ;
            #endif
         }
         memcpy( t4->cache->keys, (char *)connection4data(connection) + sizeof( CONNECTION4TAG_CACHE_INFO_OUT ) + numRowsFromServer * sizeof( long ), numRowsFromServer * keyLen ) ;
         if ( posSkip == 1 )
            t4->cache->pos = 0 ;
         else
            t4->cache->pos = numRowsFromServer-1 ;
         t4->cache->currentSize = numRowsFromServer ;
         readFromCache = 1 ;
      }

      if ( t4->cache->pos != -1 && ( (t4->cache->pos < t4->cache->currentSize && posSkip == 1 ) || (t4->cache->pos > 0 && posSkip == -1 )) ) // if a valid cached row position, just return the cached row
      {
         if ( readFromCache == 0 )  //  increment position unless we just refreshed the cache
            t4->cache->pos += posSkip;
         t4->recNo = t4->cache->recNos[t4->cache->pos] ;
         t4->tagDataValid = 1 ;
         int keyLen = tfile4keyLen( t4 ) ;
         t4->currentKeyLen = keyLen ;
         if ( t4->currentKeyAllocLen < t4->currentKeyLen )   // not room for key
         {
            if ( u4allocAgain( c4, &t4->currentKey, &t4->currentKeyAllocLen, t4->currentKeyLen ) < 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E91642 ) ;
               #endif
               return e4memory ;
            }
         }
         memcpy( t4->currentKey, &t4->cache->keys[t4->cache->pos*t4->currentKeyLen], keyLen ) ;
         return 0 ;
      }
      else // still invalid, we must be at eof
      {
         t4->cache->pos = -1 ;
         return r4eof ;
      }
   #else
      return e4notSupported;
   #endif
}



S4EXPORT long S4FUNCTION t4readBuffer( TAG4 *t4, long numRecsToBuf )
{
      /*
         Similar to d4readBuffer but support for the tag functions instead of the data functions
         - uses the same cache as is used in tfile4skipCache()
         - numRecsToBuf is the size of the buffer in terms of number of records to buffer (>=0 are valid)
         - set numRecsToBuf to 0 to disable the bufferring and free the buffers.
         - recalling this function would reset the buffer, and free/reallocate it if the size has changed.
         - buffer resets if:  This function is called; the current record is repositioned via any call except tfile4skip (e.g. tfile4seek, tfile4top, tfile4bottom, tfile4positionSet)
         -- if you position back to the old position prior to calling tfile4skip() the buffer will be used again.
         - The buffer is filled only in calls to tfile4skip() with a paramate of 1 or -1.  The initial call to tfile4top()/tfile4bottom() just return the 1st record.
         - tfile4skip() - when the buffer has been emptied.  If the skip is done in a backwards direction the records are bufferred in the backwards direction
         - returns the # of records allocated to buffer
      */

   #ifdef S4CLIENT
      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91642 ) ;
         if ( numRecsToBuf < 0 )
            return error4( t4->tagFile->codeBase, e4parm, E91642 ) ;
      #endif

      TAG4FILE *tagFile = t4->tagFile ;

      if ( numRecsToBuf == 0 )
      {
         tfile4freeCache( tagFile, 1 ) ;
         tagFile->t4readBuffer = 0 ;
         return 0 ;
      }

      int rc = tfile4cacheInit( tagFile, numRecsToBuf, 0, 0 ) ;
      if ( rc != 0 )
         return rc ;

      tagFile->t4readBuffer = 1 ;
      tagFile->tagDataValid = 0 ;  // reset the start position
      return tagFile->cache->cacheSize ;
   #else
      return e4notSupported;
   #endif
}


