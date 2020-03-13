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

/* i4remove.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef I4PRINT
   #include <sys\timeb.h>
   #include <time.h>
#endif

#ifndef S4CLIENT
#ifndef S4OFF_WRITE
#ifndef S4OFF_INDEX

/* not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
int tfile4remove( TAG4FILE *tagFile, const unsigned char *ptr, const unsigned long rec )
{
   #ifdef E4PARM_LOW
      if ( tagFile == 0 )
         return error4( 0, e4parm_null, E95404 ) ;
   #endif

   #ifndef S4CLIPPER
      // AS Jan 9/03 - not supported for clipper
      // AS Dec 31/02 - added non-updating temporary indexes
      if ( tagFile->indexFile->nonUpdateable == 1 )
         return 0 ;
   #endif

   #ifdef I4PRINT
      #ifdef I4PRINT_TAG_NAME
         if ( stricmp( tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
      #endif
      {
         char dump[255];
         sprintf( dump, "tfile4remove called for Tag: record: %ld\r\n", tagFile->alias, (long)rec ) ;
         log5( dump ) ;
      }
   #endif

   #ifndef S4OFF_MULTI
      #ifdef E4ANALYZE
         #ifdef S4CLIPPER
            // AS Jan 18/02 - Ensure the index file is locked prior to the remove...
            if ( tagFile->fileLocked == 0 && tagFile->file.lowAccessMode != OPEN4DENY_RW )
            {
               return error4( 0, e4lock, E95404 ) ;
            }
         #endif
      #endif
   #endif

   int rc = tfile4go( tagFile, ptr, rec, 0 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
   if ( rc < 0 )
   {
      #ifdef I4PRINT
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            log5( "tfile4go failed\r\n" ) ;
         }
      #endif
      return error4stack( tagFile->codeBase, rc, E95404 ) ;
   }
   if ( rc )
   {
      #ifdef I4PRINT
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255];
            sprintf( dump, "tfile4go returned non-zero rc, rc = %ld for rec #: %ld\r\n", (long)rc, (long)rec ) ;
            log5( dump ) ;
         }
      #endif
      return r4entry ;
   }

   return tfile4removeCurrent( tagFile ) ;
}



/* not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
int tfile4removeCalc( TAG4FILE *tagFile, unsigned long rec )
{
   unsigned char *ptr ;

   #ifndef S4CLIPPER
      // AS Jan 9/03 - not supported for clipper
      // AS Dec 31/02 - added non-updating temporary indexes
      if ( tagFile->indexFile->nonUpdateable == 1 )
         return 0 ;
   #endif

   #ifdef I4PRINT
      char dump[255];
      sprintf( dump, "tfile4removeCalc called for tag: %s\r\n", tagFile->alias ) ;
      log5( dump ) ;
   #endif

   #ifdef E4PARM_LOW
      if ( tagFile == 0 )
         return error4( 0, e4parm_null, E95405 ) ;
   #endif

   if ( error4code( tagFile->codeBase ) < 0 )
      return e4codeBase ;

   tfile4exprKey( tagFile, &ptr ) ;
   return tfile4remove( tagFile, ptr, rec ) ;
}



#ifdef S4FOX
/* S4FOX, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
int tfile4removeBranch( TAG4FILE *tagFile, B4BLOCK *blockOn )
{
   /* remove the (current) branch block */

   INDEX4FILE *i4 = tagFile->indexFile ;

   if ( blockOn == (B4BLOCK *)l4first( &tagFile->blocks ) )
   {
      /* Root Block, do not delete */
      #ifdef E4ANALYZE
         if ( !b4nodeInvalid( blockOn->header.leftNode ) || !b4nodeInvalid( blockOn->header.rightNode ) || blockOn->header.nKeys != 1 )
            return error4describe( tagFile->codeBase, e4info, E80401, tfile4alias( tagFile ), i4->file.name, 0 ) ;
      #endif
      c4memset( blockOn->data, 0, i4blockSize( i4 ) - sizeof(B4STD_HEADER) - sizeof(B4NODE_HEADER) ) ;

      if ( !b4leaf( blockOn ) )   /* if not a leaf, then reset nodeHdr too */
      {
         c4memset( (void *)&blockOn->nodeHdr, 0, sizeof(B4NODE_HEADER) ) ;
         b4leafInit( blockOn ) ;
      }
      else
         blockOn->nodeHdr.freeSpace = (short)i4blockSize( i4 ) - sizeof( B4STD_HEADER ) - sizeof( B4NODE_HEADER ) ;

      blockOn->header.nKeys = 0 ;
      b4nodeSetInvalid( &blockOn->header.leftNode ) ;
      b4nodeSetInvalid( &blockOn->header.rightNode ) ;
      blockOn->keyOn = -1 ;
      blockOn->builtOn = -1 ;
      blockOn->header.nodeAttribute = 3 ;  /* root and leaf */
      blockOn->changed = 1 ;
   }
   else /* This block is to be deleted */
   {
      B4NODE lNode, rNode ;
      b4nodeAssignNode( &lNode, blockOn->header.leftNode ) ;
      b4nodeAssignNode( &rNode, blockOn->header.rightNode ) ;
      l4remove( &tagFile->blocks, blockOn ) ;

      if ( index4shrink( i4, blockOn->fileBlock ) < 0 )
         return 0 ;

      blockOn->changed = 0 ;

      if ( b4nodeValid( lNode ) )
      {
         FILE4LONG pos ;
         b4nodeGetFilePosition( i4, lNode, &pos ) ;
         // file4longAssign( pos, I4MULTIPLY, 0 ) ;
         // file4longMultiply( pos, lNode ) ;

         if ( file4readAllInternal( &i4->file, pos, &blockOn->header, i4blockSize( i4 ) ) < 0 )
            return 0 ;

         #ifdef S4BYTE_SWAP
            blockOn->header.nodeAttribute = x4reverseShort( (void *)&blockOn->header.nodeAttribute ) ;
            blockOn->header.nKeys = x4reverseShort( (void *)&blockOn->header.nKeys ) ;
            blockOn->header.leftNode.node = x4reverseLong( (void *)&blockOn->header.leftNode ) ;
            blockOn->header.rightNode.node = x4reverseLong( (void *)&blockOn->header.rightNode ) ;

            /* if block is a leaf */
            // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
            if (blockOn->header.nodeAttribute & 0x02 )
            {
               blockOn->nodeHdr.freeSpace = x4reverseShort( (void *)&blockOn->nodeHdr.freeSpace ) ;
               S4LONG longVal = x4reverseLong( (void *)&blockOn->nodeHdr.recNumMask[0] ) ;
               c4memcpy( (void *)&blockOn->nodeHdr.recNumMask[0], (void *)&longVal, sizeof(S4LONG) ) ;
            }
            else /* if block is a branch */
            {
               short shortVal = blockOn->tag->header.keyLen + sizeof(S4LONG) ;
               /* position swapPtr to end of first key expression */
               char *swapPtr = (char *) &blockOn->nodeHdr.freeSpace + blockOn->tag->header.keyLen ;

               /* move through all B4KEY's to swap 'long's */
               for ( int i = 0 ; i < (int) blockOn->header.nKeys ; i++ )
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

         b4nodeAssignNode( &blockOn->fileBlock, lNode ) ;
         b4nodeAssignNode( &blockOn->header.rightNode, rNode ) ;
         blockOn->changed = 1 ;
         b4flush( blockOn ) ;
      }

      if ( b4nodeValid( rNode ) )
      {
         FILE4LONG pos ;
         b4nodeGetFilePosition( i4, rNode, &pos ) ;
         // file4longAssign( pos, I4MULTIPLY, 0 ) ;
         // file4longMultiply( pos, rNode ) ;

         if ( file4readAllInternal( &i4->file, pos, &blockOn->header, i4blockSize( i4 ) ) < 0 )
            return 0 ;

         #ifdef S4BYTE_SWAP
            blockOn->header.nodeAttribute = x4reverseShort( (void *)&blockOn->header.nodeAttribute ) ;
            blockOn->header.nKeys = x4reverseShort( (void *)&blockOn->header.nKeys ) ;
            blockOn->header.leftNode.node = x4reverseLong( (void *)&blockOn->header.leftNode ) ;
            blockOn->header.rightNode.node = x4reverseLong( (void *)&blockOn->header.rightNode ) ;

            /* if block is a leaf */
            // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
            if (blockOn->header.nodeAttribute & 0x02 )
            {
               blockOn->nodeHdr.freeSpace = x4reverseShort( (void *)&blockOn->nodeHdr.freeSpace ) ;
               S4LONG longVal = x4reverseLong( (void *)blockOn->nodeHdr.recNumMask ) ;
               c4memcpy( (void *)blockOn->nodeHdr.recNumMask, (void *)&longVal, sizeof(S4LONG) ) ;
            }
            else /* if block is a branch */
            {
               short shortVal = blockOn->tag->header.keyLen + sizeof(S4LONG) ;
               /* position swapPtr to end of first key expression */
               char *swapPtr = (char *) &blockOn->nodeHdr.freeSpace + blockOn->tag->header.keyLen ;

               /* move through all B4KEY's to swap 'long's */
               for ( int i = 0 ; i < (int) blockOn->header.nKeys ; i++ )
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

         b4nodeAssignNode( &blockOn->fileBlock, rNode ) ;
         b4nodeAssignNode( &blockOn->header.leftNode, lNode ) ;
         blockOn->changed = 1 ;
         b4flush( blockOn ) ;
      }

      return b4free( blockOn ) ;
   }

   return 0 ;
}

#endif /* S4FOX */



#ifndef S4CLIPPER
/* not S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
int tfile4removeCurrent( TAG4FILE *tagFile )
{
   /* Remove the current key */

   // AS Dec 31/02 - added non-updating temporary indexes
   if ( tagFile->indexFile->nonUpdateable == 1 )
      return 0 ;

   #ifdef I4PRINT
      #ifdef I4PRINT_TAG_NAME
         if ( stricmp( tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
      #endif
      {
         char dump[255] ;
         sprintf( dump, "tfile4removeCurrent called for Tag:%s \r\n", tagFile->alias ) ;
         log5( dump ) ;
      }
   #endif

   INDEX4FILE *i4 = tagFile->indexFile ;

   #ifdef S4FOX
      i4->tagIndex->header.version = i4->versionOld+1 ;
      void *newKeyInfo = 0 ;

      unsigned long rec = INVALID4BLOCK_ID ;

      for ( B4BLOCK *b4 = (B4BLOCK *)tagFile->blocks.lastNode ; b4 ; )
      {
         Bool5 blRemoved = 0 ;
         if ( newKeyInfo == 0 )  /* then delete entry */
         {
            if ( b4lastpos( b4 ) == 0 )
            {
               #ifdef I4PRINT_ADD
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     log5( "removing branch block\r\n" ) ;
                  }
               #endif
               if ( b4 != (B4BLOCK *)l4first( &tagFile->blocks ) )
                  blRemoved = 1 ;
               tfile4removeBranch( tagFile, b4 ) ;
               b4 = (B4BLOCK *)tagFile->blocks.lastNode ;
            }
            else
            {
               Bool5 lessThanLast = 0 ;
               if ( b4->keyOn < b4lastpos( b4 ) )
                  lessThanLast = 1 ;
               #ifdef I4PRINT_ADD
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "removing key for position: %ld, num keys in block: %ld, lessThanLast: %ld\r\n", (long)b4->keyOn, (long)b4->header.nKeys, (long)lessThanLast ) ;
                     log5( dump ) ;
                  }
               #endif
               int rc = b4remove( b4 ) ;
               #ifdef I4PRINT_ADD
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "done removing key...num keys in block: %ld, rc: %ld\r\n", (long)b4->header.nKeys, (long)rc ) ;
                     log5( dump ) ;
                  }
               #endif
               if ( rc < 0 )
                  return rc ;
               if ( lessThanLast )
                  return 0 ;

               /* On last entry */
               b4goEof( b4 ) ;
               b4->keyOn-- ;  /* update keyOn for going to last spot */
               newKeyInfo = b4keyKey( b4, b4->keyOn ) ;
               rec = b4recNo( b4, b4->keyOn ) ;
            }
         }
         else  /* Adjust entry */
         {
            if ( b4brReplace( b4, (unsigned char *)newKeyInfo, rec ) < 0 )
               return -1 ;
            if ( b4->keyOn != b4lastpos( b4 ) )  /* not on end key, so exit, else continue */
               return 0 ;
         }

         if ( !blRemoved )
         {
            b4 = (B4BLOCK *)b4->link.p ;
            if ( b4 == (B4BLOCK *)tagFile->blocks.lastNode )
               break ;
         }
      }
   #endif

   #ifdef S4MDX
      Bool5 removeDone = 0 ;
      Bool5 updateReqd = 0 ;
      i4->changed = 1 ;
      tagFile->changed = 1 ;
      tagFile->header.version++ ;

      B4BLOCK *blockIterate = (B4BLOCK *)l4last( &tagFile->blocks ) ;
      for ( ;; )
      {
         B4BLOCK *b4 = blockIterate ;
         if ( b4 == 0 )
            break ;
         blockIterate = (B4BLOCK *)l4prev( &tagFile->blocks, blockIterate ) ;  /* Calculate the previous block while the current block exists. */

         if ( !removeDone )  /* either removing or else updating */
         {
            if ( b4lastpos( b4 ) == 0 )  /* delete block */
            {
               if ( b4 == (B4BLOCK *)l4first( &tagFile->blocks ) )  /* root block, don't delete */
               {
                  b4->changed = 1 ;
                  b4->keyOn = 0 ;
                  c4memset( (void *)&b4->nKeys, 0, i4->header.blockRw ) ;
                  if (tagFile->filter )  /* must modify the filter setting for dBASE IV compatibility */
                  {
                     FILE4LONG pos ;
                     b4nodeGetFilePosition( i4, tagFile->headerOffset, &pos ) ;
                     file4longAdd( &pos, sizeof( tagFile->header ) + 222 ) ;
                     file4writeInternal( &tagFile->indexFile->file, pos, (char *)"\0", (int)1 ) ;
                     tagFile->hasKeys = 0 ;
                     tagFile->hadKeys = 1 ;
                  }
                  return 0 ;
               }
               else
               {
                  l4remove( &tagFile->blocks, b4 ) ;
                  if ( index4shrink( i4, b4->fileBlock ) < 0 )
                     return -1 ;
                  b4free( b4 ) ;
                  b4 = 0 ;
               }
            }
            else  /* just remove entry */
            {
               if ( blockIterate && b4->keyOn == b4lastpos( b4 ) && b4lastpos( b4 ) > 0 )  /* save to become last key of the block for parentBlock update */
               {
                  // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - make dynamic
                  if ( tagFile->header.keyLen + 2 * sizeof( S4LONG ) > tagFile->codeBase->savedKeyLen )
                  {
                     if ( u4allocAgain( tagFile->codeBase, &tagFile->codeBase->savedKey, &tagFile->codeBase->savedKeyLen, tagFile->header.keyLen + 2 * sizeof( S4LONG ) ) < 0 )
                        return e4memory ;
                  }
                  c4memcpy( tagFile->codeBase->savedKey, b4keyKey( b4, b4->nKeys - 1 - b4leaf( b4 ) ), tagFile->header.keyLen ) ;
                  #ifdef I4PRINT
                     struct _timeb mTime ;
                     char *outTime, dump[255] ;
                     _ftime( &mTime ) ;
                     outTime = ctime( &( mTime.time ) ) ;

                     B4KEY_DATA *keyData = b4key( b4, b4->nKeys - 1 - b4leaf( b4 ) ) ;
                     sprintf( dump, "remove - saved entry: i4 = %ld, fileBlock: %uld, recNo = %uld, key = %.40s, time: %s\r\n", (long)i4,
                        (unsigned long)b4->fileBlock, keyData->num, (char *)keyData->value, outTime ) ;
                     log5( dump ) ;
                  #endif
                  updateReqd = 1 ;
               }
               else
                  updateReqd = 0 ;

               b4remove( b4 ) ;
               if ( !b4leaf( b4 ) && b4lastpos( b4 ) == 0 )  /* branch with only one entry, so have the entry take place of this block */
               {
                  if (b4 == (B4BLOCK *)l4first( &tagFile->blocks ) )   /* the entry becomes the new root */
                  {
                     /* first update the tags root */
                     tagFile->header.root = b4key( b4, 0 )->num ;
                     #ifdef S4BYTE_SWAP
                        tagFile->header.root = x4reverseLong( (void *)&tagFile->header.root ) ;
                     #endif
                     FILE4LONG pos ;
                     b4nodeGetFilePosition( i4, tagFile->headerOffset, &pos ) ;
                     file4writeInternal( &tagFile->indexFile->file, pos, (void *)&tagFile->header.root, sizeof(tagFile->header.root) ) ;
                     #ifdef S4BYTE_SWAP
                        tagFile->header.root = x4reverseLong( (void *)&tagFile->header.root ) ;
                     #endif
                     updateReqd = 0 ;
                  }
                  else  /* remove this branch block */
                  {
                     blockIterate->changed = 1 ;
                     b4key( blockIterate, blockIterate->keyOn )->num = b4key( b4, 0 )->num ;
                  }
                  l4remove( &tagFile->blocks, b4 ) ;
                  if ( index4shrink( tagFile->indexFile, b4->fileBlock ) < 0 )
                      return -1 ;
                  b4->changed = 0 ;
                  b4free( b4 ) ;
                  b4 = 0 ;
               }
               else
                  b4->keyOn = b4lastpos( b4 ) ;

               if ( !updateReqd )
                  return 0 ;
               removeDone = 1 ;
            }
         }
         else  /* Adjust entry - at most one update will be required in MDX */
         {
            if ( b4->keyOn < b4lastpos( b4 ) )
            {
               b4->changed = 1 ;
               c4memcpy( b4keyKey( b4, b4->keyOn), tagFile->codeBase->savedKey, tagFile->header.keyLen ) ;
               return 0 ;
            }
         }
      }
   #endif

   return 0 ;
}

#endif   /* ifndef S4CLIPPER  */



#ifdef S4CLIPPER

/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceBranch( TAG4FILE *, B4BLOCK * ) ;



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static void tfile4removeBlock( TAG4FILE *tagFile, B4BLOCK *block )
{
   // this routine assumes that the block being removed is on the 'saved' chain...
   assert5( block == (B4BLOCK *)tagFile->saved.lastNode ) ;
   tfile4shrink( tagFile, block->fileBlock ) ;
   l4pop( &tagFile->saved ) ;
   block->changed = 0 ;
   b4free( block ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefResetBlock( TAG4FILE *tagFile, B4BLOCK *block )
{
   /* reset the block */
   short offset = ( tagFile->header.keysMax + 2 + ( (tagFile->header.keysMax/2)*2 != tagFile->header.keysMax ) ) * sizeof(short) ;

   for ( short i = 0 ; i <= tagFile->header.keysMax ; i++ )
   {
      block->pointers[i] = (short)( tagFile->header.groupLen * i ) + offset ;
   }

   return 0 ;
}


/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static B4KEY_DATA *tfile4removeRefGetKeyData( TAG4FILE *tagFile, B4BLOCK *block )
{
   B4KEY_DATA *myKeyData = (B4KEY_DATA *)u4allocEr( tagFile->codeBase, tagFile->header.groupLen ) ;

   if ( myKeyData == 0 )
      return 0 ;

   if ( block->keyOn >= block->nKeys )
      c4memcpy( &(myKeyData->num), &( b4key( block, block->keyOn - 1 )->num), sizeof(S4LONG) + tagFile->header.keyLen ) ;
   else
      c4memcpy( &(myKeyData->num), &( b4key( block, block->keyOn )->num), sizeof(S4LONG) + tagFile->header.keyLen ) ;

   return myKeyData ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefReplaceRoot( TAG4FILE *tagFile, B4BLOCK *block )
{
   /* LY 00/02/18 : HP-UX */
   S4LONG reference ;
   c4memcpy( &reference, &b4key( block, 0 )->pointer, sizeof(S4LONG) ) ;

   tfile4shrink( tagFile, block->fileBlock ) ;
   l4pop( &tagFile->blocks ) ;  /* remove ourselves */
   block->changed = 0 ;
   b4free( block ) ;
   tagFile->header.root = reference ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefMoveLeftNeighbourRightMaxBlock( TAG4FILE *tagFile, B4BLOCK *blockOn, B4BLOCK *siblingBlock, B4BLOCK *blockUp )
{
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP */
      S4LONG reference ;
      c4memcpy( &reference, &b4key( blockOn, 0 )->pointer, sizeof(S4LONG) ) ;
   #else
      unsigned long reference = b4key( blockOn, 0 )->pointer ;
   #endif

   int blockUpKeyNo = 0 ;

   blockOn->keyOn = 0 ;
   b4insert( blockOn, b4keyKey( blockUp, blockUpKeyNo ), b4recNo( blockUp, blockUpKeyNo ), reference / 512 ) ;
   c4memcpy( b4keyKey( blockUp, blockUpKeyNo ), b4keyKey( siblingBlock, 0 ), tagFile->header.keyLen ) ;

   /* LY 00/02/18 : HP */
   c4memcpy( &b4key( blockUp, blockUpKeyNo )->num, &b4key( siblingBlock, 0 )->num, sizeof(S4LONG) ) ;
   c4memcpy( &b4key( blockOn, 1 )->pointer, &b4key( siblingBlock, 0 )->pointer, sizeof(S4LONG) ) ;
   siblingBlock->keyOn = 0 ;

   b4remove( siblingBlock ) ;

   blockUp->changed = 1 ;
   blockOn->changed = 1 ;
   siblingBlock->changed = 1 ;
   b4flush( siblingBlock ) ;
   b4free( siblingBlock ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefEnsureParentValid( TAG4FILE *tagFile, B4BLOCK *blockOn, B4BLOCK *siblingBlock, B4BLOCK *blockUp )
{
   if ( blockUp->nKeys == 0 )
   {
      /* just removed last entry... */
      if ( tagFile->header.root == blockUp->fileBlock * 512 )
      {
         // the 'blockUp' parentBlock block is the root
         tfile4removeRefReplaceRoot( tagFile, blockUp ) ;
         blockUp = 0 ;
         l4add( &tagFile->blocks, siblingBlock ) ;
      }
      else
      {
         b4flush( siblingBlock ) ;
         b4free( siblingBlock ) ;
         siblingBlock = 0 ;
         return tfile4balanceBranch( tagFile, blockUp ) ;
      }
   }
   else  /* save and free */
   {
      b4flush( siblingBlock ) ;
      b4free( siblingBlock ) ;
      siblingBlock = 0 ;
   }

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefMoveLeftNeighbourRightRegularBlock
(
   TAG4FILE *tagFile,
   B4BLOCK *blockOn,
   B4BLOCK *siblingBlock,
   B4BLOCK *blockUp
)
{
   /* LY 00/02/18 : HP-UX */
   S4LONG tempNum ;

   /* move current reference over, and delete from parentBlock */

   siblingBlock->keyOn = 0 ;
   /* LY 00/02/18 : HP-UX */
   c4memcpy( &tempNum, &b4key( blockUp, blockUp->keyOn )->num, sizeof(S4LONG) ) ;
   b4insert( siblingBlock, b4keyKey( blockUp, blockUp->keyOn ), tempNum, b4key( blockOn, 0 )->pointer / 512 ) ;
   b4remove( blockUp ) ;

   #ifdef E4INDEX_VERIFY
      if ( (B4BLOCK *)tagFile->saved.lastNode != blockOn )
         return error4( tagFile->codeBase, e4index, E81601 ) ;
   #endif

   l4pop( &tagFile->saved ) ;
   blockOn->changed = 0 ;
   tfile4shrink( tagFile, blockOn->fileBlock ) ;
   b4free( blockOn ) ;
   blockOn = 0 ;
   blockUp->changed = 1 ;
   siblingBlock->changed = 1 ;

   /* now check that parentBlock is still valid */
   return tfile4removeRefEnsureParentValid( tagFile, blockOn, siblingBlock, blockUp ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefMoveLeftNeighbourRight( TAG4FILE *tagFile, B4BLOCK *blockOn, B4BLOCK *blockUp )
{
   /* move one right neighbour into left */
   /* LY 00/02/18 : HP-UX */
   S4LONG neighborRef ;
   c4memcpy( &neighborRef, &b4key( blockUp, 1 )->pointer, sizeof(S4LONG) ) ;
   B4BLOCK *siblingBlock = b4alloc( tagFile, neighborRef ) ;

   if ( siblingBlock == 0 )
      return e4memory ;

   if ( i4readBlock( &tagFile->file, neighborRef, 0, siblingBlock ) < 0 )
   {
      b4free( siblingBlock ) ;
      siblingBlock = 0 ;
      return -1 ;
   }

   if ( siblingBlock->nKeys == tagFile->header.keysMax )
   {
      /* already maxed out, must shift */
      /* borrow for the current block */
      return tfile4removeRefMoveLeftNeighbourRightMaxBlock( tagFile, blockOn, siblingBlock, blockUp ) ;
   }
   else
   {
      /* move current reference over, and delete from parentBlock */
      return tfile4removeRefMoveLeftNeighbourRightRegularBlock( tagFile, blockOn, siblingBlock, blockUp ) ;
   }
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefMoveToRightNeighbourMaxBlock( TAG4FILE *tagFile, B4BLOCK *blockOn, B4BLOCK *siblingBlock, B4BLOCK *blockUp )
{
   /* LY 00/02/18 : HP-UX */
   S4LONG reference ;
   c4memcpy( &reference, &b4key( blockOn, 0 )->pointer, sizeof(S4LONG) ) ;
   int blockUpKeyNo = blockUp->keyOn - 1 ;

   blockOn->keyOn = 0 ;
   b4insert( blockOn, b4keyKey( blockUp, blockUpKeyNo ), b4key( blockUp, blockUp->keyOn - 1 )->num, b4key( siblingBlock, siblingBlock->nKeys )->pointer / 512 ) ;
   c4memcpy( b4keyKey( blockUp, blockUpKeyNo ), b4keyKey( siblingBlock, siblingBlock->nKeys - 1 ), tagFile->header.keyLen ) ;

   b4key( blockUp, blockUp->keyOn - 1 )->num = b4key( siblingBlock, siblingBlock->nKeys - 1 )->num ;
   b4key( blockOn, 1 )->pointer = reference ;

   siblingBlock->nKeys-- ;
   blockUp->changed = 1 ;
   blockOn->changed = 1 ;
   siblingBlock->changed = 1 ;
   b4flush( siblingBlock ) ;
   b4free( siblingBlock ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefMoveToRightNeighbourRightRegularBlock
(
   TAG4FILE *tagFile,
   B4BLOCK *blockOn,
   B4BLOCK *siblingBlock,
   B4BLOCK *blockUp
)
{
   /* LY 00/02/18 : HP-UX */
   S4LONG tempNum, tempPointer ;
   /* move current reference over, and delete from parentBlock */
   siblingBlock->keyOn = siblingBlock->nKeys ;
   /* just re-insert the last key, then change the values */
   /* LY 00/02/18 : HP-UX */
   c4memcpy( &tempNum, &b4key( siblingBlock, siblingBlock->keyOn - 1 )->num, sizeof(S4LONG) ) ;
   c4memcpy( &tempPointer, &b4key( siblingBlock, siblingBlock->keyOn )->pointer, sizeof(S4LONG) ) ;
   b4insert( siblingBlock, b4keyKey( siblingBlock, siblingBlock->keyOn - 1 ),
             tempNum, tempPointer / 512 ) ;

   c4memcpy( b4keyKey( siblingBlock, siblingBlock->nKeys - 1 ), b4keyKey( blockUp, blockUp->keyOn - 1 ), tagFile->header.keyLen ) ;
   /* LY 00/02/18 : HP-UX */
   c4memcpy( &b4key( siblingBlock, siblingBlock->nKeys - 1 )->num, &b4key( blockUp, blockUp->keyOn - 1 )->num, sizeof(S4LONG) ) ;
   c4memcpy( &b4key( siblingBlock, siblingBlock->nKeys )->pointer, &b4key( blockOn, 0 )->pointer, sizeof(S4LONG) ) ;

   #ifdef E4INDEX_VERIFY
      if ( (B4BLOCK *)tagFile->saved.lastNode != blockOn )
         return error4( tagFile->codeBase, e4index, E81601 ) ;
   #endif

   tfile4removeBlock( tagFile, blockOn ) ;
   blockOn = 0 ;
   /* LY 00/02/18 : HP-UX */
   c4memcpy( &b4key( blockUp, blockUp->keyOn )->pointer, &b4key( blockUp, blockUp->keyOn - 1 )->pointer, sizeof(S4LONG) ) ;
   blockUp->keyOn-- ;
   b4remove( blockUp ) ;
   blockUp->changed = 1 ;
   siblingBlock->changed = 1 ;

   return tfile4removeRefEnsureParentValid( tagFile, blockOn, siblingBlock, blockUp ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefMoveToRightNeighbour( TAG4FILE *tagFile, B4BLOCK *blockOn, B4BLOCK *blockUp )
{
   /* move to left neighbor */
   /* LY 00/02/18 : HP-UX */
   S4LONG neighborRef ;
   c4memcpy( &neighborRef, &b4key( blockUp, blockUp->keyOn - 1 )->pointer, sizeof(S4LONG) ) ;
   B4BLOCK *siblingBlock = b4alloc( tagFile, neighborRef ) ;
   if ( siblingBlock == 0 )
      return e4memory ;

   if ( i4readBlock( &tagFile->file, neighborRef, 0, siblingBlock ) < 0 )
   {
      b4free( siblingBlock ) ;
      siblingBlock = 0 ;
      return -1 ;
   }

   if ( siblingBlock->nKeys == tagFile->header.keysMax )  /* already maxed out */
   {
      /* borrow for the current block */
      return tfile4removeRefMoveToRightNeighbourMaxBlock( tagFile, blockOn, siblingBlock, blockUp ) ;
   }
   else
   {
      return tfile4removeRefMoveToRightNeighbourRightRegularBlock( tagFile, blockOn, siblingBlock, blockUp ) ;
   }
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRefOneKey( TAG4FILE *tagFile, B4BLOCK *blockOn )
{
   /*
      Case where only 1 key in the branch

      case where the reference is the only key left in the block
      we may need to do more collapsing

      take the branch, and put in place of me, then delete and add me
   */

   assert5( tagFile->blocks.nLink > 0 ) ;

   unsigned long reference ;
   if ( blockOn->keyOn >= blockOn->nKeys )
      /* LY 00/02/18 : HP-UX */
      c4memcpy( &reference, &b4key( blockOn, 0 )->pointer, sizeof(S4LONG) ) ;
   else
      /* LY 00/02/18 : HP-UX */
      c4memcpy( &reference, &b4key( blockOn, 1 )->pointer, sizeof(S4LONG) ) ;

   b4remove( blockOn ) ;

   assert5( tagFile->blocks.nLink > 0 ) ;

   if ( tagFile->blocks.nLink == 1 )
   {
      /* root block, so reference will take over */
      tfile4removeRefReplaceRoot( tagFile, blockOn ) ;
      blockOn = (B4BLOCK *)tagFile->blocks.lastNode ;
      return 0 ;
   }

   /* special case -- blocks contain very large keys.  try balancing */
   tfile4up( tagFile ) ;
   B4BLOCK *blockUp = (B4BLOCK *)tagFile->blocks.lastNode ;

   assert5( blockUp != 0 ) ;

   if ( blockUp->keyOn == 0 )
      return tfile4removeRefMoveLeftNeighbourRight( tagFile, blockOn, blockUp ) ;
   else
      return tfile4removeRefMoveToRightNeighbour( tagFile, blockOn, blockUp ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4removeRef( TAG4FILE *tagFile )
{
   /*
      The purpose of this function is to remove the current key in the
      current block and then add it back in.

      Basically gets called when we are removing keys and we have
      an empty block.  The block itself is empty, but there is a
      valid key in the parentBlock which points to the empty block.
      We must remove this key and get rid of the reference, but
      we have an extra key which was part of the reference, so
      we need to add it back in.
   */

   S4LONG tempNum ;  /* LY 00/02/18 : HP-UX */
   B4BLOCK *blockOn = (B4BLOCK *)tagFile->blocks.lastNode ;

   tfile4up( tagFile ) ;
   B4BLOCK *blockUp = (B4BLOCK *)tagFile->blocks.lastNode ;

   if ( blockUp == 0 )  /* root block only, so reference doesn't exist */
      return tfile4removeRefResetBlock( tagFile, blockOn ) ;

   // remove the block referred to
   tfile4removeBlock( tagFile, blockOn ) ;
   blockOn = blockUp ;

   B4KEY_DATA *myKeyData = tfile4removeRefGetKeyData( tagFile, blockOn ) ;
   if ( myKeyData == 0 )
      return e4memory ;

   int rc = 0 ;

   if ( blockOn->nKeys == 1 )
   {
      // case where the reference is the only key left in the block
      // we may need to do more collapsing
      rc = tfile4removeRefOneKey( tagFile, blockOn ) ;
   }
   else    /* just remove myself, add later */
   {
      b4remove( blockOn ) ;
      if ( blockOn->nKeys < tagFile->header.keysHalf && blockOn->fileBlock != tagFile->header.root )
      {
         /* if not root may have to balance the tree */
         rc = tfile4balanceBranch( tagFile, blockOn ) ;
      }
   }

   /* now add the removed reference back into the index */
   if ( rc == 0 )
   {     /* LY 00/02/18 : HP-UX */
      c4memcpy( &tempNum, &myKeyData->num, sizeof(S4LONG) ) ;
      rc = tfile4add( tagFile, (unsigned char *)myKeyData->value, tempNum, 0 ) ;
   }

   u4free( myKeyData ) ;

   return rc ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
enum Balance5mode
{
   nodeBalanced = 0,
   leafMode = 1,
   branchMode = 2
} ;



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceMergeLeafsLeftSibling( TAG4FILE *tagFile, B4BLOCK *block, B4BLOCK *siblingBlock )
{
   /*
      merge 2 leaf blocks, where the sibling block is to the right of 'block;

      all of the block keys are moved into the sibling
   */

   /* LY 00/02/18 : HP-UX */
   S4LONG tempNum ;
   // copy the keys from 'block' to 'sibling'
   for ( int keyIndex = 0 ; keyIndex < block->nKeys ; keyIndex++ )
   {
      b4goEof( siblingBlock ) ;
      B4KEY_DATA *bdata = b4key( block, keyIndex ) ;
      /* LY 00/02/18 : HP-UX */
      c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
      b4insert( siblingBlock, bdata->value, tempNum, 0L ) ;
   }

   tagFile->codeBase->doIndexVerify = 0 ;  /* avoid verify errors due to our partial removal */

   // move the blocks chain down to the leaf level
   do
   {
      tfile4down( tagFile ) ;
      if ( tagFile->blocks.lastNode == 0 )
         break ;
   } while( !b4leaf( (B4BLOCK *)tagFile->blocks.lastNode ) ) ;  /* don't use tfile4block() due to borland problems */

   tagFile->codeBase->doIndexVerify = 1 ;

   b4flush( siblingBlock ) ;

   /* delete 'block' and the reference to 'block' */
   return tfile4removeRef( tagFile ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceMergeBranchesLeftSibling( TAG4FILE *tagFile, B4BLOCK *parentBlock, B4BLOCK *block, B4BLOCK *siblingBlock )
{
   /* LY 00/02/18 : HP-UX */
   S4LONG tempNum, tempPointer ;
   block->keyOn = 0 ;

   assert5( parentBlock->keyOn >= 1 ) ;  // AS 05/15/99 --> next line subtracts by 1, if we are balancing with left sibling, there better be one...

   parentBlock->keyOn-- ;
   B4KEY_DATA *parentBlockKeyData = b4key( parentBlock, parentBlock->keyOn ) ;  // keydata in parentBlock for 'block' entry

   if ( block->keyOn == 0 && block->nKeys == 0 )  /* must do reference as well */
   {
      // case where branch block 'block' only contains a single entry, a reference to a child block
      /* LY 00/02/18 : HP-UX */
      S4LONG reference ;
      c4memcpy( &reference, &b4key( block, 0 )->pointer, sizeof(S4LONG) ) ;
      c4memcpy( &tempNum, &parentBlockKeyData->num, sizeof(S4LONG) ) ;
      c4memcpy( &tempPointer, &b4key( siblingBlock, siblingBlock->nKeys )->pointer, sizeof(S4LONG) ) ;
      b4insert( block, parentBlockKeyData->value, tempNum, tempPointer / 512 ) ;
      c4memcpy( &b4key( block, 1 ) ->pointer, &reference, sizeof(S4LONG) ) ;
   }
   else
   {
      // case where block contains at least 2 entries
      /* LY 00/02/18 : HP-UX */
      c4memcpy( &tempNum, &parentBlockKeyData->num, sizeof(S4LONG) ) ;
      c4memcpy( &tempPointer, &b4key( siblingBlock, siblingBlock->nKeys )->pointer, sizeof(S4LONG) ) ;
      b4insert( block, parentBlockKeyData->value, tempNum, tempPointer / 512 ) ;
   }

   for ( int neighborKeyIndex = siblingBlock->nKeys - 1 ; neighborKeyIndex >= 0 ; neighborKeyIndex-- )
   {
      block->keyOn = 0 ;
      parentBlockKeyData = b4key( siblingBlock, neighborKeyIndex ) ;
      /* LY 00/02/18 : HP-UX */
      c4memcpy( &tempNum, &parentBlockKeyData->num, sizeof(S4LONG) ) ;
      c4memcpy( &tempPointer, &parentBlockKeyData->pointer, sizeof(S4LONG) ) ;
      b4insert( block, parentBlockKeyData->value, tempNum, tempPointer / 512 ) ;
   }

   siblingBlock->changed = 0 ;
   tfile4shrink( tagFile, siblingBlock->fileBlock ) ;
   b4remove( parentBlock ) ;

   if ( parentBlock->nKeys < tagFile->header.keysHalf )
      return tfile4balanceBranch( tagFile, parentBlock ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceMergeLeafsRightSibling( TAG4FILE *tagFile, B4BLOCK *parentBlock, B4BLOCK *block, B4BLOCK *siblingBlock )
{
   /*
      parentBlock is at the right sibling at the start of function

      merge 2 branch blocks, where the sibling block is to the right of 'block;
      We do this by removing the right sibling...

      This requires a rotation.  We need to go from:

      parentBlock( ... [<parentBlock key><parentBlock ref>] [<parentLeft key><parentLeft ref>] ... )
         block( [<BlockRight key><BlockRight ref>] ... [<BlockLeft ref>] )  // no key for rightmost entry in branches
         right( [<rightRight key><rightRight ref>] ... [<rightLeft ref>] )  // no key for rightmost entry in branches

      to: (delete right) - must also handle the extra key in the parent...

      parentBlock( ... [<parnetLeft key><parentBlock ref>]...)
         block( [<BlockRight key><BlockRight ref>] ... [<parentBlock key><BlockLeft ref>]  // cont'd next line
                [<rightRight key><rightRight ref>] ... [<rightLeft ref>] )
   */

   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      S4LONG tempNum, tempLong ;
   #endif
   B4KEY_DATA *bdata = b4key( parentBlock, parentBlock->keyOn ) ;

   if ( block->keyOn == 0 && block->nKeys == 0 )  /* must do reference as well */
   {
      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
         S4LONG reference ;
         c4memcpy( &reference, &b4key( block, 0 )->pointer, sizeof(S4LONG) ) ;
         c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
         b4insert( block, bdata->value, tempNum, 0L ) ;
         c4memcpy( &b4key( block, 1 )->pointer, &reference, sizeof(S4LONG) ) ;
      #else
         long reference = b4key( block, 0 )->pointer ;
         b4insert( block, bdata->value, bdata->num, 0L ) ;
         b4key( block, 1 ) ->pointer = reference ;
      #endif
   }
   else
      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      {
              c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
         b4insert( block, bdata->value, tempNum, 0L ) ;
      }
      #else
         b4insert( block, bdata->value, bdata->num, 0L ) ;
      #endif

   for ( int neighborKeyIndex = 0 ; neighborKeyIndex < siblingBlock->nKeys - 1 ; neighborKeyIndex++ )
   {
      b4goEof( block ) ;
      bdata = b4key( siblingBlock, neighborKeyIndex ) ;
      #ifdef S4DATA_ALIGN
         c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
         b4insert( block, bdata->value, tempNum, 0L ) ;
      #else
         b4insert( block, bdata->value, bdata->num, 0L ) ;
      #endif
   }

   bdata = b4key( parentBlock, parentBlock->keyOn ) ;
   B4KEY_DATA *bdata2 = b4key( siblingBlock, siblingBlock->nKeys - 1 ) ;
   c4memcpy( bdata->value, bdata2->value, tagFile->header.keyLen )  ;
   c4memcpy( &( bdata->num ), &bdata2->num, sizeof(S4LONG ) )  ;

   parentBlock->keyOn++ ;
   parentBlock->changed = 1 ;

   tagFile->codeBase->doIndexVerify = 0 ;  /* avoid verify errors due to our partial removal */
   b4flush( block ) ;

   do
   {
      if ( tfile4down( tagFile ) == 1 )
         break ;
   } while( ((B4BLOCK *)tagFile->blocks.lastNode)->fileBlock != siblingBlock->fileBlock ) ;

   tagFile->codeBase->doIndexVerify = 1 ;

   if ( tfile4removeRef( tagFile ) != 0 )  /* will delete siblingBlock and will remove and re-add the reference if required */
      return -1 ;

   siblingBlock->changed = 0 ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceMergeBranchesRightSibling( TAG4FILE *tagFile, B4BLOCK *parentBlock, B4BLOCK *block, B4BLOCK *siblingBlock )
{
   /*
      Merges branch blocks when the sibling block is to the left of 'block'

      Moves block's keys into sibling and then removes block

      This routine assumes that our block is currently at the 'parent block' position and that
      the 'saved' list contains 'block' (i.e. just moved from there).
   */

   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      S4LONG tempNum, tempPointer ;
   #endif
   // AS 05/17/99 - assert pre-conditions
   assert5( (B4BLOCK *)tagFile->saved.lastNode == block ) ;
   assert5( (B4BLOCK *)tagFile->blocks.lastNode == parentBlock ) ;

   siblingBlock->keyOn = 0 ;
   B4KEY_DATA *bdata = b4key( parentBlock, parentBlock->keyOn ) ;
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
      c4memcpy( &tempPointer,  &b4key( block, block->nKeys )->pointer, sizeof(S4LONG) ) ;
      b4insert( siblingBlock, bdata->value, tempNum, tempPointer / 512 ) ;
   #else
      b4insert( siblingBlock, bdata->value, bdata->num, b4key( block, block->nKeys )->pointer / 512 ) ;
   #endif

   for ( int blockKeyIndex = block->nKeys - 1 ; blockKeyIndex >= 0 ; blockKeyIndex-- )
   {
      siblingBlock->keyOn = 0 ;
      bdata = b4key( block, blockKeyIndex ) ;
      #ifdef S4DATA_ALIGN
         c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
         c4memcpy( &tempPointer, &bdata->pointer, sizeof(S4LONG) ) ;
         b4insert( siblingBlock, bdata->value, tempNum, tempPointer / 512 ) ;
      #else
         b4insert( siblingBlock, bdata->value, bdata->num, bdata->pointer / 512 ) ;
      #endif
   }

   tfile4removeBlock( tagFile, block ) ;
   block = 0 ;
   b4flush( siblingBlock ) ;
   b4remove( parentBlock ) ;

   if ( parentBlock->nKeys < tagFile->header.keysHalf )
      return tfile4balanceBranch( tagFile, parentBlock ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static void tfile4balanceBlockRedistributeLeftSibling
(
   B4BLOCK *parentBlock,
   B4BLOCK *block,
   B4BLOCK *siblingBlock,
   const Balance5mode balanceMode,
   const int averageOfKeysInBlockAndSibling,
   int keyLen
)
{
   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
      S4LONG tempLong1, tempLong2 ;
   #endif
   block->keyOn = 0 ;
   parentBlock->keyOn-- ;
   B4KEY_DATA *bdata = b4key( parentBlock, parentBlock->keyOn ) ;

   if ( balanceMode == branchMode )
   {
      if ( block->nKeys == 0 )
      {
         #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
            S4LONG blockLeftmostChildFileBlock ;
            c4memcpy( &blockLeftmostChildFileBlock, &b4key( block, 0 )->pointer, sizeof(S4LONG) ) ;
            c4memcpy( &tempLong1, &bdata->num, sizeof(S4LONG) ) ;
            c4memcpy( &tempLong2, &b4key( siblingBlock, siblingBlock->nKeys )->pointer, sizeof(S4LONG) ) ;
            b4insert( block, bdata->value, tempLong1, tempLong2 / 512 ) ;
         #else
            unsigned long blockLeftmostChildFileBlock = b4key( block, 0 )->pointer ;
            b4insert( block, bdata->value, bdata->num, b4key( siblingBlock, siblingBlock->nKeys )->pointer / 512 ) ;
         #endif
         b4append( block, blockLeftmostChildFileBlock / 512 ) ;
      }
      else
      {
         #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
            c4memcpy( &tempLong1, &bdata->num, sizeof(S4LONG) ) ;
            c4memcpy( &tempLong2, &b4key( siblingBlock, siblingBlock->nKeys )->pointer, sizeof(S4LONG) ) ;
            b4insert( block, bdata->value, tempLong1, tempLong2 / 512 ) ;
         #else
            b4insert( block, bdata->value, bdata->num, b4key( siblingBlock, siblingBlock->nKeys )->pointer / 512 ) ;
         #endif
      }

      while ( siblingBlock->nKeys > averageOfKeysInBlockAndSibling && block->nKeys < averageOfKeysInBlockAndSibling )
      {
         block->keyOn = 0 ;
         siblingBlock->keyOn = siblingBlock->nKeys - 1 ;
         bdata = b4key( siblingBlock, siblingBlock->keyOn ) ;

         #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
            c4memcpy( &tempLong1, &bdata->num, sizeof(S4LONG) ) ;
            c4memcpy( &tempLong2, &bdata->pointer, sizeof(S4LONG) ) ;
            b4insert( block, bdata->value, tempLong1, tempLong2 / 512 ) ;
         #else
            b4insert( block, bdata->value, bdata->num, bdata->pointer / 512 ) ;
         #endif

         b4remove( siblingBlock ) ;
      }
      siblingBlock->keyOn = siblingBlock->nKeys - 1 ;

      /* and add a key back to the parentBlock */
      bdata = b4key( parentBlock, parentBlock->keyOn ) ;

      B4KEY_DATA *bdata2 ;
      bdata2 = b4key( siblingBlock, siblingBlock->nKeys - 1 ) ;

      c4memcpy( bdata->value, bdata2->value, keyLen )  ;
      c4memcpy( &(bdata->num), &bdata2->num, sizeof(S4LONG ) )  ;

      siblingBlock->keyOn = siblingBlock->nKeys ;
      b4remove( siblingBlock ) ;
   }
   else
   {
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
         c4memcpy( &tempLong1, &bdata->num, sizeof(S4LONG) ) ;
         b4insert( block, bdata->value, tempLong1, 0L ) ;
      #else
         b4insert( block, bdata->value, bdata->num, 0L ) ;
      #endif

      while ( siblingBlock->nKeys > averageOfKeysInBlockAndSibling && block->nKeys < averageOfKeysInBlockAndSibling )
      {
         block->keyOn = 0 ;
         siblingBlock->keyOn = siblingBlock->nKeys - 1 ;
         bdata = b4key( siblingBlock, siblingBlock->keyOn ) ;

         #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
            c4memcpy( &tempLong1, &bdata->num, sizeof(S4LONG) ) ;
            b4insert( block, bdata->value, tempLong1, 0L ) ;
         #else
            b4insert( block, bdata->value, bdata->num, 0L ) ;
         #endif

         b4remove( siblingBlock ) ;
      }
      siblingBlock->keyOn = siblingBlock->nKeys - 1 ;

      /* and add a key back to the parentBlock */
      bdata = b4key( parentBlock, parentBlock->keyOn ) ;

      B4KEY_DATA *bdata2 = b4key( siblingBlock, siblingBlock->keyOn ) ;
      c4memcpy( bdata->value, bdata2->value, keyLen )  ;
      c4memcpy( &(bdata->num), &bdata2->num, sizeof(S4LONG ) )  ;
      b4remove( siblingBlock ) ;
   }

   parentBlock->changed = 1 ;
   b4flush( siblingBlock ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static void tfile4balanceBlockRedistributeRightSibling
(
   TAG4FILE *tagFile,
   B4BLOCK *parentBlock,
   B4BLOCK *block,
   B4BLOCK *siblingBlock,
   const Balance5mode balanceMode,
   int averageOfKeysInBlockAndSibling,
   int keyLen
)
{
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      S4LONG tempNum, tempPointer ;
   #endif
   b4goEof( block ) ;

   if ( balanceMode == branchMode )
   {
      B4KEY_DATA *bdata = b4key( block, block->nKeys ) ;
      B4KEY_DATA *bdata2 = b4key( parentBlock, parentBlock->keyOn ) ;
      c4memcpy( bdata->value, bdata2->value, keyLen )  ;
      c4memcpy( &(bdata->num), &bdata2->num, sizeof(S4LONG ) )  ;
      block->nKeys++ ;

      while ( block->nKeys - (balanceMode == branchMode) < averageOfKeysInBlockAndSibling )
      {
         b4goEof( block ) ;
         siblingBlock->keyOn = 0 ;
         bdata = b4key( siblingBlock, siblingBlock->keyOn ) ;

         #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
            c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
            c4memcpy( &tempPointer, &bdata->pointer, sizeof(S4LONG) ) ;
            b4insert( block, bdata->value, tempNum, tempPointer / 512 ) ;
         #else
            b4insert( block, bdata->value, bdata->num, bdata->pointer / 512 ) ;
         #endif

         b4remove( siblingBlock ) ;
      }

      block->nKeys-- ;
      block->keyOn-- ;

      siblingBlock->keyOn = 0 ;

      /* and add a key back to the parentBlock */
      bdata = b4key( parentBlock, parentBlock->keyOn ) ;
      bdata2 = b4key( block, block->nKeys ) ;

      c4memcpy( bdata->value, bdata2->value, keyLen )  ;
      c4memcpy( &(bdata->num), &bdata2->num, sizeof(S4LONG ) )  ;
   }
   else
   {
      B4KEY_DATA *bdata = b4key( parentBlock, parentBlock->keyOn ) ;
      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
         c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
         b4insert( block, bdata->value, tempNum, 0L ) ;
      #else
         b4insert( block, bdata->value, bdata->num, 0L ) ;
      #endif

      while ( block->nKeys - (balanceMode == branchMode) < averageOfKeysInBlockAndSibling )
      {
         b4goEof( block ) ;
         siblingBlock->keyOn = 0 ;
         bdata = b4key( siblingBlock, siblingBlock->keyOn ) ;

         #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
            c4memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
            b4insert( block, bdata->value, tempNum, 0L ) ;
         #else
            b4insert( block, bdata->value, bdata->num, 0L ) ;
         #endif

         b4remove( siblingBlock ) ;
      }

      siblingBlock->keyOn = 0 ;

      /* and add a key back to the parentBlock */
      bdata = b4key( parentBlock, parentBlock->keyOn ) ;

      B4KEY_DATA *bdata2 = b4key( siblingBlock, siblingBlock->keyOn ) ;
      c4memcpy( bdata->value, bdata2->value, keyLen )  ;
      c4memcpy( &(bdata->num), &bdata2->num, sizeof(S4LONG ) )  ;
      b4remove( siblingBlock ) ;
   }

   parentBlock->changed = 1 ;
   b4flush( siblingBlock ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceBlockRedistribute
(
   TAG4FILE *tagFile,
   B4BLOCK *parentBlock,
   B4BLOCK *block,
   B4BLOCK *siblingBlock,
   const Balance5mode balanceMode,
   const char siblingIsLeftOfBlock
)
{
   int averageOfKeysInBlockAndSibling = ( block->nKeys + siblingBlock->nKeys + 1 ) / 2 ;

   if ( averageOfKeysInBlockAndSibling < tagFile->header.keysHalf )
      averageOfKeysInBlockAndSibling = tagFile->header.keysHalf ;

   if ( siblingIsLeftOfBlock )
      tfile4balanceBlockRedistributeLeftSibling( parentBlock, block, siblingBlock, balanceMode, averageOfKeysInBlockAndSibling, tagFile->header.keyLen ) ;
   else
      tfile4balanceBlockRedistributeRightSibling( tagFile, parentBlock, block, siblingBlock, balanceMode, averageOfKeysInBlockAndSibling, tagFile->header.keyLen ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceBlock
(
   TAG4FILE *tagFile,
   B4BLOCK *parentBlock,
   B4BLOCK *block,
   B4BLOCK *siblingBlock,
   char siblingIsLeftOfBlock,
   Balance5mode balanceMode
)
{
   // AS 05/17/99 this routine assumes that the tag is positioned at the 'parent' level and that the 'saved' block
   // is 'block' (this 2nd is not necessarily critical - maybe require further investigation to see
   assert5( (B4BLOCK *)tagFile->saved.lastNode == block ) ;
   assert5( (B4BLOCK *)tagFile->blocks.lastNode == parentBlock ) ;

   if ( balanceMode == branchMode && b4leaf( siblingBlock ) )
      return error4( tagFile->codeBase, e4index, E81601 ) ;

   if ( siblingBlock->nKeys + block->nKeys < tagFile->header.keysMax )
   {
      // the blocks can be combined because sum of keys < max
      if ( siblingIsLeftOfBlock )
      {
         if ( balanceMode == branchMode )
            return tfile4balanceMergeBranchesLeftSibling( tagFile, parentBlock, block, siblingBlock ) ;
         else
            return tfile4balanceMergeLeafsLeftSibling( tagFile, block, siblingBlock ) ;
      }
      else
      {
         /* put parentBlock entry, then all but one child, then last child to parentBlock */
         b4goEof( block ) ;
         if ( balanceMode == branchMode )
            return tfile4balanceMergeBranchesRightSibling( tagFile, parentBlock, block, siblingBlock ) ;
         else
            return tfile4balanceMergeLeafsRightSibling( tagFile, parentBlock, block, siblingBlock ) ;
      }
   }
   else  /* will have to redistribute keys */
      return tfile4balanceBlockRedistribute( tagFile, parentBlock, block, siblingBlock, balanceMode, siblingIsLeftOfBlock ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceBranchLeaf( TAG4FILE *tagFile, B4BLOCK *parentBlock, B4BLOCK *branch, B4BLOCK *leaf, Bool5 rightIsLeaf )
{
   /*
      This function performs a balance between an input branch block and an input leaf block.
      The input parentBlock block is a parentBlock of both blocks.

      Basically, we don't want a situation where a parentBlock block has one child which is a leaf
      and another is a branch.

      To solve problem we create a new branch block which contains only one member, the leaf
      block.  This new branch is placed into the parentBlock block in place of the leaf, and then
      the new branch and the existing branch blocks are balanced.

      PARAMATERS

      parentBlock - this is the block parent to both the branch and the leaf.  In addition, this
               parent block is positioned to the leaf block.
      rightIsLeaf - if true, then the leaf block is on the right of the branch block.  This is important
               as a sub-paramater to tfile4balanceBlock() which needs to know whether the parent block
               is balancing with a block on its right or its left.
   */
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      S4LONG tempPointer ;
   #endif

   tagFile->header.version++ ;  // mark tag as having changed

   // create a new branch block in tag
   unsigned long newFileBlock = tfile4extend( tagFile ) ;
   B4BLOCK *newBranchBlock = b4alloc( tagFile, newFileBlock ) ;
   if ( newBranchBlock == 0 )
      return -1 ;

   // put the current leaf's block as the only entry in the new branch block
   B4KEY_DATA *key = b4key( newBranchBlock, 0 ) ;
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      tempPointer = leaf->fileBlock * I4MULTIPLY ;
      c4memcpy( &key->pointer, &tempPointer, sizeof(S4LONG) ) ;
   #else
      key->pointer =  leaf->fileBlock * I4MULTIPLY ;
   #endif
   newBranchBlock->changed = 1 ;
   // insert the branch block into the list of blocks:
   // i.e. change block list from:  ...<parent><leaf>... to <parent><newBranch><leaf>
   l4add( &tagFile->blocks, newBranchBlock ) ;

   // replace the parents leaf entry with the new branch block
   key = b4key( parentBlock, parentBlock->keyOn ) ;
   #ifdef E4ANALYZE_ALL
      // do a final check to ensure that the parent blocks current position is at the leaf block
      if ( key->pointer != leaf->fileBlock * I4MULTIPLY )
         return error4( tagFile->codeBase, e4index, E95407 ) ;
   #endif
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      tempPointer = newFileBlock * I4MULTIPLY ;
      c4memcpy( &key->pointer, &tempPointer, sizeof(S4LONG) ) ;
   #else
      key->pointer = newFileBlock * I4MULTIPLY ;
   #endif
   parentBlock->changed = 1 ;

   // AS 05/15/99 --> case was occurring where the replaced (leaf) block was not left of the existing
   //                 (branch) block.  Yet, we were passing '1' as 2nd last paramater saying we were...
   //     -  changed to add input paramater and use that...
   // return tfile4balanceBlock( tagFile, parentBlock, newBranchBlock, branch, 1, branchMode ) ;
   Bool5 siblingIsLeftOfBlock ;
   if ( rightIsLeaf )
   {
      // currently parent on old 'leaf' position (which is on the left of the existing branch block)
      // therefore - left is old leaf, so sibling (ourself) is 'left' of block
      siblingIsLeftOfBlock = 1 ;
   }
   else
      siblingIsLeftOfBlock = 0 ;

   // AS 05/17/99 --> tfile4balanceBlock assumes that we are at the 'parent level'.  Since we added
   // new branch at current level, move up one... (also reqa. newBranchBlock be on 'saved' list...
   tfile4up( tagFile ) ;
   assert5( (B4BLOCK *)tagFile->saved.lastNode == newBranchBlock ) ;
   assert5( (B4BLOCK *)tagFile->blocks.lastNode == parentBlock ) ;

   return tfile4balanceBlock( tagFile, parentBlock, newBranchBlock, branch, siblingIsLeftOfBlock, branchMode ) ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceEnsureSiblingsAreLeafs( TAG4FILE *tagFile, B4BLOCK **parentBlock, B4BLOCK **block )
{
   /* make sure siblings are also leafs.  If a sibling is not a leaf, it means that we should
      not be a leaf either.  In that case, call function (tfile4balanceBranchLeaf) to modify
      the tagFile so that a branch block is inserted between the parent and ourselves.

      returns < 0 if error, else 0

      This function assumes that the block list is at 'block'

      On return from this function, the block list will be at the
      'parent' position.

      PARAMATERS

      parentBlock - output paramater which is assigned the parent of 'block'
               is set to NULL if block is the root block
   */
   assert5( *block != 0 && tagFile != 0 ) ;
//   assert5( tagFile->blocks.lastNode != (LINK4 *)(*parentBlock) ) ;

   // retrieve the parent block
   tfile4up( tagFile ) ;
   (*parentBlock) = (B4BLOCK *)tagFile->blocks.lastNode ;

   if ( (*parentBlock) != 0 )
   {
      // our block is not the root block (parentBlock is not null)

      // if the parentBlock is on key 0, don't care if siblings are leafs...
      if ( (*parentBlock)->keyOn > 0 )
      {
         // block is not first entry in parentBlock, check left sibling

         #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
            unsigned long leftSiblingFileBlock ;
            c4memcpy( &leftSiblingFileBlock, &b4key( (*parentBlock), (*parentBlock)->keyOn - 1 )->pointer, sizeof(S4LONG) ) ;
         #else
            unsigned long leftSiblingFileBlock = b4key( (*parentBlock), (*parentBlock)->keyOn - 1 )->pointer ;
         #endif

         int rc = 0 ;
         B4BLOCK *leftSiblingBlock = b4alloc( tagFile, 0L ) ;
         if ( i4readBlock( &tagFile->file, leftSiblingFileBlock, 0, leftSiblingBlock ) < 0 )
         {
            rc = -1 ;
         }
         else if ( b4leaf( leftSiblingBlock ) == 0 &&  b4leaf( *block ) == 1 )
         {
            // left is not a leaf (is branch), right is leaf...
            // means siblings is NOT a leaf.  This is an undesireable case, so balance the
            // sibling branch with our leaf block.
            Bool5 rightIsLeaf = 1 ;  // right is a leaf...
            rc = tfile4balanceBranchLeaf( tagFile, (*parentBlock), leftSiblingBlock, (*block), rightIsLeaf ) ;
         }
         b4free( leftSiblingBlock ) ;
         return rc ;
      }
   }

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceEnsureLeftSiblingIsBranch( TAG4FILE *tag, B4BLOCK *parentBlock, B4BLOCK *block )
{
   /* make sure left sibling is also a branch.  If a sibling is not a branch, it means that we should
      not be a branch either.  In that case, call function (tfile4balanceBranchLeaf) to modify
      the tagFile so that a branch block is inserted between the parent and ourselves.

      returns < 0 if error, else 0

      PARAMATERS

      parentBlock - input valid pointer to parent block of 'block'
      block - input valid pointer to block to be balanced
   */
   assert5( block != 0 && parentBlock != 0 ) ;

   // assumes that b4 block has siblings to the left (i.e. parent's current position is not 0)
   assert5( parentBlock->keyOn != 0 ) ;

   B4BLOCK *leftSiblingBlock = b4alloc( tag, 0L ) ;

   // get the sibling to the left...
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      S4LONG leftSiblingFileBlock ;
      c4memcpy( &leftSiblingFileBlock, &b4key( parentBlock, parentBlock->keyOn - 1 )->pointer, sizeof(S4LONG) ) ;
   #else
      unsigned long leftSiblingFileBlock = b4key( parentBlock, parentBlock->keyOn - 1 )->pointer ;
   #endif
   int rc = 0 ;
   if ( i4readBlock( &tag->file, leftSiblingFileBlock, 0, leftSiblingBlock ) < 0 )
      rc = -1 ;
   else if ( b4leaf( leftSiblingBlock ) == 0 &&  b4leaf( block ) == 1 )
   {
      // left sibling is a branch but we are a leaf... balance by making ourselves a branch
      Bool5 rightIsLeaf = 1 ;  // right is a leaf...
      rc = tfile4balanceBranchLeaf( tag, parentBlock, leftSiblingBlock, block, rightIsLeaf ) ;
   }
   else if ( b4leaf( leftSiblingBlock ) == 1 &&  b4leaf( block ) == 0 )
   {
      // left sibling is a leaf but we are a branch... balance by making our sibling a branch
      Bool5 rightIsLeaf = 0 ;  // right is a branch...
      rc = tfile4balanceBranchLeaf( tag, parentBlock, block, leftSiblingBlock, rightIsLeaf ) ;
   }
   // else both our sibling and ourselves are of the same type (both branch or both leaf)

   b4free( leftSiblingBlock ) ;

   return rc ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceEnsureRightSiblingIsBranch( TAG4FILE *tagFile, B4BLOCK *parentBlock, B4BLOCK *block )
{
   /* make sure left right is also a branch.  If a sibling is not a branch, it means that we should
      not be a branch either.  In that case, call function (tfile4balanceBranchLeaf) to modify
      the tagFile so that a branch block is inserted between the parent and ourselves.

      returns < 0 if error, else 0

      PARAMATERS

      parentBlock - input valid pointer to parent block of 'block'
      block - input valid pointer to block to be balanced

      this routine assumes that the 'current' block is at the parent level...
   */

   assert5( block != 0 && parentBlock != 0 ) ;
   assert5( parentBlock == (B4BLOCK *)tagFile->blocks.lastNode ) ;  // assume we are at the parent level...

   B4BLOCK *rightSiblingBlock = b4alloc( tagFile, 0L ) ;

   parentBlock->keyOn = 0 ;

   // read our right sibling
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
      S4LONG rightSiblingFileBlock ;
      c4memcpy( &rightSiblingFileBlock, &b4key( parentBlock, parentBlock->keyOn + 1 )->pointer, sizeof(S4LONG) ) ;
   #else
      unsigned long rightSiblingFileBlock = b4key( parentBlock, parentBlock->keyOn + 1 )->pointer ;
   #endif
   if ( i4readBlock( &tagFile->file, rightSiblingFileBlock, 0, rightSiblingBlock ) < 0 )
   {
      b4free( rightSiblingBlock ) ;
      return -1 ;
   }

   // AS 01/04/00 - Was not always freeing rightSiblingBlock - in particular if
   // neither of if's below failed.  Revise the code to be more consistent...

   int rc = 0 ;

   if ( b4leaf( rightSiblingBlock ) == 0 &&  b4leaf( block ) == 1 )
   {
      // right sibling is a branch but we are a leaf... balance by making us a branch
      Bool5 rightIsLeaf = 0 ;  // right is a branch...
      rc = tfile4balanceBranchLeaf( tagFile, parentBlock, rightSiblingBlock, block, rightIsLeaf ) ;
   }
   else if ( b4leaf( rightSiblingBlock ) == 1 &&  b4leaf( block ) == 0 )
   {
      // right sibling is a leaf but we are a branch... balance by making our sibling a branch
      Bool5 rightIsLeaf = 1 ;  // right is a leaf...
      rc = tfile4balanceBranchLeaf( tagFile, parentBlock, block, rightSiblingBlock, rightIsLeaf ) ;
   }

   // if neither above ifs were true...
   // both our sibling and ourselves are of the same type (both branch or both leaf)

   // the block should not be on any chains...
   assert5( rightSiblingBlock->link.n == 0 ) ;
   b4free( rightSiblingBlock ) ;
   return rc ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceEnsureSiblingsAreBranches( TAG4FILE *tagFile, B4BLOCK **parentBlock, B4BLOCK **block )
{
   /* make sure siblings are also branches

      Basically, we just check our neighbour next door, and balance as appropriate

      returns < 0 if error, else 0
      returns r4balanced if the tree is balanced
      returns 0 if need to do more balancing

      the block list should be positioned at the sibling block, and
      the parent should be also be set at the sibling keyOn position

      After returning from this function, the current block list position
      is at 'block' if the there is still balancing to be done (i.e.
      not r4balanced or error).

      PARAMATERS

      parentBlock - output paramater which is assigned the parent of 'block'
               is set to NULL if block is the root block
   */

   assert5( *block != 0 && tagFile != 0 ) ;

   // make current position at parentBlock
   tfile4up( tagFile ) ;
   (*parentBlock) = (B4BLOCK *)tagFile->blocks.lastNode ;

   if ( (*parentBlock) != 0 )
   {
      int rc ;

      if ( (*parentBlock)->keyOn > 0 )
      {
         // means we have a parent block and we have siblings to our left
         rc = tfile4balanceEnsureLeftSiblingIsBranch( tagFile, *parentBlock, *block ) ;
      }
      else
      {
         // means we have a parent block and we are the first entry (no left siblings) - check right sibling instead
         rc = tfile4balanceEnsureRightSiblingIsBranch( tagFile, *parentBlock, *block ) ;
      }

      if ( rc < 0 )
         return rc ;
   }

   if ( b4leaf( (*block) ) )
   {
      // AS 05/17/99 --> we don't want to return in a state of being in non-existent block position...
      if ( tagFile->blocks.lastNode == 0 )
         tfile4upToRoot( tagFile ) ;
      return r4balanced ;
   }

   // if we got here, then it means that we performed a leaf/branch
   // merge.  In that case, we went from <parent><sibling> to
   // <parent><newblock><block>  and the current position is <parent>
   // therefore, to set the current block list position to block,
   // we need to go down the list 2 times.

   tfile4down( tagFile ) ;
   tfile4down( tagFile ) ;
   (*block) = (B4BLOCK *)tagFile->blocks.lastNode ;
   tfile4up( tagFile ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceFullGetSibling
(
   TAG4FILE *tagFile,
   B4BLOCK **parentBlock,
   B4BLOCK **block,
   const Bool5 blockHasNoKeys,
   char *siblingIsLeftOfBlock,
   unsigned long *siblingFileBlock,
   Balance5mode *balanceMode
)
{
   /*
      Gets a sibling block with which to perform a balancing when
      we are performing a full balance.

      This function assumes that the block list is at the parent
      position.

      PARAMATERS

      siblingFileBlock - output paramters gets set to sibling block with which to
                         balance.

      siblingIsLeftOfBlock - is true if the sibling block is to the right of
                the 'block'.  If 'siblingIsLeftOfBlock' is false, the sibling is
                to the left of the 'block'.

      parentBlock - output paramater which is assigned the parent of 'block'
               is set to NULL if block is the root block
   */
   // should be at parent position, not block position... (unless block is a root block - i.e. tree with 1 block only)
//   assert5( ( tagFile->blocks.lastNode != (LINK4 *)(*block) ) || ((*block)->fileBlock == tagFile->header.root / 512) ) ;

   (*parentBlock) = (B4BLOCK *)tagFile->blocks.lastNode ;
   if ( blockHasNoKeys == 0 )
      (*parentBlock)->keyOn = (*parentBlock)->nKeys ;

   // move the chain down to the 'block' level
   int rc = tfile4down( tagFile ) ;

   if ( rc == 1 )
   {
      rc = tfile4balanceEnsureSiblingsAreLeafs( tagFile, parentBlock, block ) ;
      if ( rc != 0 )
         return rc ;
      // after balancing at the leaf level, there must be no more
      // balancing left to do, so balanced.
      // AS 05/17/99 --> should never leave this function in a state of no current block...
      if ( tagFile->blocks.lastNode == 0 )
         tfile4upToRoot( tagFile ) ;
      return r4balanced ;
   }
   else
   {
      // means we could go down further, block must be a branch block

      // the chain should be at the block/sibling level now...
      assert5( tagFile->blocks.lastNode != (LINK4 *)(*parentBlock) ) ;

      if ( blockHasNoKeys == 1 )  /* need to check the siblings to make sure also branches */
      {
         rc = tfile4balanceEnsureSiblingsAreBranches( tagFile, parentBlock, block ) ;
         if ( rc != 0 )
            return rc ;
         // after balancing at the branch level, the branches may still
         // be unbalanced, so continue balancing
         return r4continueBalancing ;
      }
   }

   (*block) = (B4BLOCK *)tagFile->blocks.lastNode ;
   (*block)->keyOn = (*block)->nKeys ;

   if ( (*block)->nKeys >= tagFile->header.keysHalf )
      return r4continueBalancing ;

   if ( b4leaf( (*block) ) )
      *balanceMode = leafMode ;

   *siblingFileBlock = b4key( (*parentBlock), (*parentBlock)->keyOn - 1 )->pointer ;
   *siblingIsLeftOfBlock = 1 ;

   // AS 05/19/99 --> move back up to parent level for return..
   tfile4up( tagFile ) ;

   // AS 05/17/99 this routine assumes that the tag is positioned at the 'parent' level and that the 'saved' block
   // is 'block' (this 2nd is not necessarily critical - maybe require further investigation to see
   assert5( (B4BLOCK *)tagFile->saved.lastNode == *block ) ;
   assert5( (B4BLOCK *)tagFile->blocks.lastNode == *parentBlock ) ;


   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceRootBlock( TAG4FILE *tagFile )
{
   /* Bascially, be definition, the root block may be in an unbalanced state
      because there may not be enough keys to keep it balanced.

      The only time it would be balanced is if it is a branch block
      with no keys (i.e. only a reference).  In that case, the
      reference can become the new root for the tag

      Assumes that the blocks chain is above the root level so
      that by going down 1 level we can obtain the root block.

      This function returns 'r4balanced' to indicate that the
      tree is balanced or returns an error condition.

      On valid return, the chain is positioned at the root
      block (i.e. one level below that when called)
   */

   assert5( (B4BLOCK *)tagFile->blocks.lastNode == 0 ) ;

   // retrieve the root block
   tfile4down( tagFile ) ;
   B4BLOCK *rootBlock = (B4BLOCK *)tagFile->blocks.lastNode ;

   if ( !b4leaf( rootBlock ) && rootBlock->nKeys == 0 )
   {
      // means the parent is a branch block with only a pointer
      // entry.  Therefore, the branch is not needed, so remove
      // it.

      unsigned long newRootFileBlock = b4key( rootBlock, 0 )->pointer ;

      // remove the old root from the file
      tfile4shrink( tagFile, rootBlock->fileBlock ) ;
      tfile4up( tagFile ) ;

      // remove the old root from the chain
      l4pop( &tagFile->saved ) ;

      // free up the root block structure without writing it to disk
      rootBlock->changed = 0 ;
      b4free( rootBlock ) ;
      rootBlock = 0 ;

      tagFile->header.root = newRootFileBlock ;

      // reposition the chain to be at the new root block
      tfile4down( tagFile ) ;
   }

   return r4balanced ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceNotFullGetSibling
(
   TAG4FILE *tagFile,
   B4BLOCK **parentBlock,
   B4BLOCK **block,
   const Bool5 blockHasNoKeys,
   char *siblingIsLeftOfBlock,
   unsigned long *siblingFileBlock,
   Balance5mode *balanceMode
)
{
   /*
      Gets a sibling block with which to perform a balancing when
      we are not performing a full balance.

      'siblingFileBlock' gets set to sibling block with which to
      balance.

      'siblingIsLeftOfBlock' is true if the sibling block is to the right of
      the 'block'.  If 'siblingIsLeftOfBlock' is false, the sibling is
      to the left of the 'block'.

      RETURNS:

      0 - use siblingFileBlock to balance the block
      r4balanced - the entire tree is balanced, can stop balancing
      r4continueBalancing - block is balanced, continue to balance
        the rest of the tree.
   */

//   assert5( (B4BLOCK *)tagFile->blocks.lastNode == block ) ;

   if ( (*balanceMode) == branchMode )  /* branch */
   {
      // if we are performing a branch balance, the easiest way is
      // to set 'block' to our parents block and then balance
      // that.
      (*block) = (*parentBlock) ;
      while( tagFile->blocks.lastNode != (LINK4 *)(*block) )  /* re-align ourselves */
         if ( tfile4up( tagFile ) == 1 )  // should not reach end...
            return error4describe( tagFile->codeBase, e4index, E81601, tfile4alias( tagFile ), 0, 0 ) ;
   }

   // get the parentBlock block by moving up one in the chain and retrieving it
   tfile4up( tagFile ) ;
   (*parentBlock) = (B4BLOCK *)tagFile->blocks.lastNode ;

   if ( (*parentBlock) == 0 )  // no parent, 'block' must be a root block
      return tfile4balanceRootBlock( tagFile ) ;

   if ( (*parentBlock)->nKeys == 0 )
   {
      // our parent by definition should be balanced, so if nKeys 0, must be error
      return error4describe( tagFile->codeBase, e4index, E81601, tfile4alias( tagFile ), 0, 0 ) ;
   }

   if ( (*block)->nKeys >= tagFile->header.keysHalf )
   {
      // we are balanced if we are 1/2 full or more
      *balanceMode = nodeBalanced ;
      return r4continueBalancing ;
   }

   // if here, then the block is not balanced...

   *siblingIsLeftOfBlock = ( (*parentBlock)->keyOn == (*parentBlock)->nKeys ) ;

   if ( *siblingIsLeftOfBlock )
      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4reinde.c on HP */
         c4memcpy( siblingFileBlock, &b4key( (*parentBlock), (*parentBlock)->keyOn - 1 )->pointer, sizeof(S4LONG) ) ;
      #else
         (*siblingFileBlock) = b4key( (*parentBlock), (*parentBlock)->keyOn - 1 )->pointer ;
      #endif
   else
      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4reinde.c on HP */
         c4memcpy( siblingFileBlock, &b4key( (*parentBlock), (*parentBlock)->keyOn + 1 )->pointer, sizeof(S4LONG) ) ;
      #else
         (*siblingFileBlock) = b4key( (*parentBlock), (*parentBlock)->keyOn + 1 )->pointer ;
      #endif

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static B4KEY_DATA *tfile4balanceSavePositionForAfterSeek
(
   TAG4FILE *tagFile,
   const B4BLOCK *b4,
   const B4BLOCK *siblingBlock,
   const B4BLOCK *parentBlock,
   int *siblingIsLeftOfEmptyBlock,
   Bool5 *blockHasNoKeys,
   const char siblingIsLeftOfBlock,
   const Balance5mode balanceMode
)
{
   /*
      save a key position for after-seek

      Basically is called prior to performing a balance so that
      we can re-find our current position.
   */
   B4KEY_DATA *myKeyData = (B4KEY_DATA *)u4allocEr( tagFile->codeBase, tagFile->header.groupLen ) ;
   if ( myKeyData == 0 )
      return 0 ;

   *siblingIsLeftOfEmptyBlock = 0 ;
   if ( b4->nKeys == 0 )   /* can't do a seek on, so after just go to far left */
   {
      *blockHasNoKeys = 1 ;
      if ( siblingIsLeftOfBlock == 1 && parentBlock != 0 )  /* get parents key, since performing a right-op */
      {
         c4memcpy( &(myKeyData->num), &( b4key( parentBlock, parentBlock->nKeys - 1 )->num), sizeof(S4LONG) + tagFile->header.keyLen ) ;
         *siblingIsLeftOfEmptyBlock = 1 ;
      }
      else
         c4memcpy( &(myKeyData->num), &( b4key( siblingBlock, 0 )->num), sizeof(S4LONG) + tagFile->header.keyLen ) ;
      assert5( myKeyData->num > 0 ) ;
   }
   else
   {
      *blockHasNoKeys = 0 ;
      /* AS 02/17/98 t5samp1.cpp - if only 1 key, else section fails because key '1' doesn't exist for leaf */
      /*   if ( b4->keyOn > 0 ) */
      if ( b4->keyOn > 0 || (b4->nKeys == 1 && balanceMode == leafMode ) )
         c4memcpy( &(myKeyData->num), &( b4key( b4, 0 )->num), sizeof(S4LONG) + tagFile->header.keyLen ) ;
      else
         c4memcpy( &(myKeyData->num), &( b4key( b4, 1 )->num), sizeof(S4LONG) + tagFile->header.keyLen ) ;
      assert5( myKeyData->num > 0 ) ;
   }

   return myKeyData ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceRepositionAfterSeek
(
   TAG4FILE *tagFile,
   B4BLOCK **block,
   const B4KEY_DATA *repositionKeyData,
   Bool5 *blockHasNoKeys,
   int siblingIsLeftOfEmptyBlock,
   Balance5mode *balanceMode,
   int *doFull
)
{
   /*
      Basically just re-find our position after a balance
   */
   assert5( repositionKeyData->num > 0 ) ;
   int rc = tfile4go( tagFile, repositionKeyData->value, repositionKeyData->num, 0 ) ;   /* returns -1 if error4code( codeBase ) < 0 */

   if ( (*blockHasNoKeys) == 1 ) /* can't find position, so just go to far left */
   {
      for ( ;; )
      {
         (*block) = tfile4block( tagFile ) ;
         if ( (*block)->nKeys < tagFile->header.keysHalf || b4leaf( (*block) ) )
         {
            if ( siblingIsLeftOfEmptyBlock == 1 )   /* in this case, don't reset position, just go up */
            {
               /* AS 11/27/97 below fix didn't work - tkay.c - change to just break without moving */
               tfile4upToRoot( tagFile ) ;
               (*blockHasNoKeys) = 0 ;   /* forces from the top again */
            }
            else
               tfile4upToRoot( tagFile ) ;

            // AS 05/17/99 --> was not resetting the 'block' member, so was no really going back to top.
            *block = tfile4block( tagFile ) ;
            *balanceMode = branchMode ;
            *doFull = 1 ;
            break ;
         }
         if ( siblingIsLeftOfEmptyBlock == 1 )
         {
            (*block)->keyOn = (*block)->nKeys ;
            tfile4down( tagFile ) ;
         }
         else
            if ( tfile4skip( tagFile, 1L ) != 1L )
               break ;
      }
   }
   else
   {
      (*block) = tfile4block( tagFile ) ;
   }

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceWithSibling
(
   TAG4FILE *tagFile,
   B4BLOCK **block,
   B4BLOCK *parentBlock,
   unsigned long *siblingFileBlock,
   const char siblingIsLeftOfBlock,
   Balance5mode *balanceMode,
   int *siblingIsLeftOfEmptyBlock,
   int *doFull,
   Bool5 *blockHasNoKeys
)
{
   /*
      Performs a balance between 'block' and the sibling block indicated
      by 'siblingFileBlock'

      Returns 0 sucess or < 0 error
   */
   // AS 05/17/99 this routine assumes that the tag is positioned at the 'parent' level and that the 'saved' block
   // is 'block' (this 2nd is not necessarily critical - maybe require further investigation to see
   assert5( (B4BLOCK *)tagFile->saved.lastNode == (*block) ) ;
   assert5( (B4BLOCK *)tagFile->blocks.lastNode == parentBlock ) ;
   B4BLOCK *siblingBlock = b4alloc( tagFile, 0L ) ;

   siblingBlock->fileBlock = *siblingFileBlock ;

   // get a block to balance with
   for ( ;; )
   {
      if ( i4readBlock( &tagFile->file, *siblingFileBlock, 0, siblingBlock ) < 0 )
      {
         b4free( siblingBlock ) ;
         return -1 ;
      }

      if ( (*balanceMode) == branchMode || b4leaf( siblingBlock ) )  /* if branch mode or leaf mode and leaf block found */
         break ;

      // if here, then we are performing a leaf balance, but the
      // block we have is a branch.  Therefore, get the next
      // block down (either the leftmost or the rightmost block
      // depending on where the sibling is) to do the balance
      // with.

      if ( siblingIsLeftOfBlock )
         #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
            c4memcpy( siblingFileBlock, &b4key( siblingBlock, siblingBlock->nKeys )->pointer, sizeof(S4LONG) ) ;
         #else
            *siblingFileBlock = b4key( siblingBlock, siblingBlock->nKeys )->pointer ;
         #endif
      else
         #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
            c4memcpy( siblingFileBlock, &b4key( siblingBlock, 0 )->pointer, sizeof(S4LONG) ) ;
         #else
            *siblingFileBlock = b4key( siblingBlock, 0 )->pointer ;
         #endif
   }

   // we want to ensure that we are at the same position after we
   // have done the balance, so save the position and then restore
   // it after.

   B4KEY_DATA *repositionKeyData = tfile4balanceSavePositionForAfterSeek( tagFile, (*block), siblingBlock, parentBlock, siblingIsLeftOfEmptyBlock, blockHasNoKeys, siblingIsLeftOfBlock, *balanceMode ) ;
   if ( repositionKeyData == 0 )
      return e4memory ;

   if ( tfile4balanceBlock( tagFile, parentBlock, (*block), siblingBlock, siblingIsLeftOfBlock, *balanceMode ) < 0 )
   {
      u4free( repositionKeyData ) ;
      return -1 ;
   }

   // AS 06/28/00 - was not freeing the sibling block when required... - ensure not on list first
   if ( siblingBlock->link.n == 0 )
   {
      b4free( siblingBlock ) ;
      siblingBlock = 0 ;
   }

   int rc = tfile4balanceRepositionAfterSeek( tagFile, block, repositionKeyData, blockHasNoKeys, *siblingIsLeftOfEmptyBlock, balanceMode, doFull ) ;
   u4free( repositionKeyData ) ;

   return rc ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
int tfile4balance( TAG4FILE *tagFile, B4BLOCK *block, int doFull )
{
   /* if doFull is true, the whole branch set will be balanced,
      not just the current leaf block and reqd.  Also, this will balance top down instead of the other way. */

   if ( !b4leaf( block ) )
      return 0 ;

   Balance5mode balanceMode ;

   if ( doFull )
   {
      tfile4upToRoot( tagFile ) ;
      balanceMode = branchMode ;
   }
   else
      balanceMode = leafMode ;

   Bool5 blockHasNoKeys = 0 ;

   B4BLOCK *parentBlock = 0 ;

   // continue balancing until the entire tree is balanced
   while ( balanceMode != nodeBalanced || doFull )
   {
      unsigned long siblingFileBlock = 0 ;
      int rc ;
      char siblingIsLeftOfBlock = -1 ;

      if ( doFull )
         rc = tfile4balanceFullGetSibling( tagFile, &parentBlock, &block, blockHasNoKeys, &siblingIsLeftOfBlock, &siblingFileBlock, &balanceMode ) ;
      else
         rc = tfile4balanceNotFullGetSibling( tagFile, &parentBlock, &block, blockHasNoKeys, &siblingIsLeftOfBlock, &siblingFileBlock, &balanceMode ) ;

      if ( rc < 0 )
         return -1 ;
      if ( rc == r4continueBalancing )
         continue ;
      if ( rc == r4balanced )
         break ;

      // if here, then we have a sibling block with which to balance
      assert5( siblingFileBlock != 0 ) ;
      assert5( siblingIsLeftOfBlock != -1 ) ;

      // AS 05/17/99 this routine assumes that the tag is positioned at the 'parent' level and that the 'saved' block
      // is 'block' (this 2nd is not necessarily critical - maybe require further investigation to see
      assert5( (B4BLOCK *)tagFile->saved.lastNode == block ) ;
      assert5( (B4BLOCK *)tagFile->blocks.lastNode == parentBlock ) ;

      int siblingIsLeftOfEmptyBlock = 0 ;
      rc = tfile4balanceWithSibling( tagFile, &block, parentBlock, &siblingFileBlock, siblingIsLeftOfBlock, &balanceMode, &siblingIsLeftOfEmptyBlock, &doFull, &blockHasNoKeys ) ;
      if ( rc < 0 )
         return rc ;

      if ( siblingIsLeftOfEmptyBlock == 1 && tagFile->blocks.lastNode != 0 )   /* in this case, don't reset position, just go up */
         block = tfile4block( tagFile ) ;

      if ( block == 0 )  /* in theory should never happen, but would maybe mean done */
         return 0 ;

      parentBlock = (B4BLOCK *)block->link.p ;
      if ( parentBlock == 0 || b4leaf( parentBlock ) )  /* no parent */
         break ;

      if ( parentBlock == block )  /* move down one to avoid problems (block should never be root) */
      {
         if ( tfile4down( tagFile ) == 0 )
         {
            block = (B4BLOCK *)tagFile->blocks.lastNode ;
            tfile4up( tagFile ) ;
         }
      }

      if ( balanceMode != branchMode || blockHasNoKeys != 1 )
      {
         if ( doFull )
         {
            if ( b4leaf( block ) )
               balanceMode = nodeBalanced ;
         }
         else
         {
            if ( parentBlock->nKeys < tagFile->header.keysHalf )  /* do parent as well */
               balanceMode = branchMode ;
            else
               balanceMode = nodeBalanced ;
         }
      }
   }

   // AS 05/17/99 --> this routine should not exit in a state where there is no 'current' block because
   // caller's assume they are on the most currently balanced block position.
   assert5( tagFile->blocks.lastNode != 0 ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
int tfile4getReplaceEntry( TAG4FILE *tagFile, B4KEY_DATA *insertSpot, B4BLOCK *saveBlock )
{
   // purpose of this function is to perform a remove on a branch block.  Basically, this involves
   // getting a replacement entry for the current position in the saveBlock.  This is done by going
   // down to the bottom level and getting a leaf entry.  That leaf entry is copied into 'insertSpot'
   // effectively 'removing' the current key from the branch block.  We then do a final balance on the
   // tree to ensure that it is balanced (only done if > 1 block in index and is now unbalanced.
   B4BLOCK *blockOn = saveBlock ;

   if ( b4leaf( blockOn ) )
      return 0 ;

   blockOn->keyOn = blockOn->keyOn + 1 ;

   // move down the the lowest level
   while ( !b4leaf( blockOn ) )
   {
      int rc = tfile4down( tagFile ) ;
      if ( rc < 0 || rc == 2 )
         return -1 ;
      blockOn = (B4BLOCK *)tagFile->blocks.lastNode ;
   }
   c4memcpy( &insertSpot->num, &(b4key( blockOn, blockOn->keyOn )->num), sizeof(S4LONG) + tagFile->header.keyLen ) ;
   saveBlock->changed = 1 ;
   b4remove( blockOn ) ;

   if ( blockOn->nKeys < tagFile->header.keysHalf && blockOn->fileBlock != tagFile->header.root )  /* if not root may have to balance the tree */
   {
      if ( tfile4balance( tagFile, blockOn, 0 ) < 0 )
         return -1 ;
      // AS 05/17/99 --> if we return 1, means we removed a 'branch' entry - we should be at an active level...
      assert5( tagFile->blocks.lastNode != 0 ) ;
   }

   return 1 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
static int tfile4balanceBranch( TAG4FILE *tagFile, B4BLOCK *b4 )
{
   #ifdef E4ANALYZE
      if ( b4leaf( b4 ) )
         return error4describe( tagFile->codeBase, e4index, E95406, tfile4alias( tagFile ), 0, 0 ) ;
   #endif

   if ( b4 == (B4BLOCK *)l4first( &tagFile->blocks ) )
   {
      if ( b4->nKeys == 0 )   /* empty, so just remove */
      {
         assert5( (B4BLOCK *)tagFile->blocks.lastNode == b4 ) ;  // assumes current position...

         unsigned long reference = b4key( b4, 0 )->pointer ;
         b4->changed = 0 ;
         tfile4shrink( tagFile, b4->fileBlock ) ;
         tfile4up( tagFile ) ;
         l4pop( &tagFile->saved ) ;
         b4free( b4 ) ;
         b4 = 0 ;
         tagFile->header.root = reference ;
      }
      return 0 ;
   }

   tfile4up( tagFile ) ;
   B4BLOCK *parentBlock = (B4BLOCK *)tagFile->blocks.lastNode ;

   #ifdef E4ANALYZE
      if ( b4key( parentBlock, parentBlock->keyOn )->pointer != I4MULTIPLY * b4->fileBlock )
         return error4describe( tagFile->codeBase, e4index, E95406, tfile4alias( tagFile ), 0, 0 ) ;
   #endif

   unsigned long tempFb ;
   Bool5 siblingIsLeftOfBlock ;
   if ( parentBlock->keyOn == parentBlock->nKeys )
   {
      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
         c4memcpy( &tempFb, &b4key( parentBlock, parentBlock->keyOn - 1 )->pointer, sizeof(S4LONG) ) ;
      #else
         tempFb = b4key( parentBlock, parentBlock->keyOn - 1 )->pointer ;
      #endif
      siblingIsLeftOfBlock = 1 ;
   }
   else
   {
      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
         c4memcpy( &tempFb, &b4key( parentBlock, parentBlock->keyOn + 1 )->pointer, sizeof(S4LONG) ) ;
      #else
         tempFb = b4key( parentBlock, parentBlock->keyOn + 1 )->pointer ;
      #endif
      siblingIsLeftOfBlock = 0 ;
   }

   B4BLOCK *b4temp = b4alloc( tagFile, 0L ) ;
   if ( b4temp == 0 )
      return error4stack( tagFile->codeBase, e4memory, E95406 ) ;

   int rc = i4readBlock( &tagFile->file, tempFb, 0, b4temp ) ;
   if ( rc < 0 )
   {
      b4free( b4temp ) ;
      return error4stack( tagFile->codeBase, rc, E95406 ) ;
   }

   rc = tfile4balanceBlock( tagFile, parentBlock, b4, b4temp, siblingIsLeftOfBlock, branchMode ) ;
   b4free( b4temp ) ;
   if ( rc < 0 )
      return error4stack( tagFile->codeBase, rc, E95406 ) ;

   return 0 ;
}



/* S4CLIPPER, not S4OFF_INDEX, not S4OFF_WRITE, not S4CLIENT */
int tfile4removeCurrent( TAG4FILE *tagFile )
{
   #ifndef S4CLIPPER
      // AS Jan 9/03 - not supported for clipper
      // AS Dec 31/02 - added non-updating temporary indexes
      if ( tagFile->indexFile->nonUpdateable == 1 )
         return 0 ;
   #endif

   tagFile->header.version = (short)( tagFile->header.oldVersion + 1L ) ;

   B4BLOCK *blockOn = (B4BLOCK *)tagFile->blocks.lastNode ;

   #ifdef E4ANALYZE
      if ( b4lastpos( blockOn ) == (b4leaf( blockOn ) ? -1 : 0) )
         return error4describe( tagFile->codeBase, e4index, E85401, tfile4alias( tagFile ), 0, 0 ) ;
   #endif

   switch( tfile4getReplaceEntry( tagFile, b4key( blockOn, blockOn->keyOn ), blockOn ) )
   {
      case 0 :   /* leaf delete */
         b4remove( blockOn ) ;
         if ( blockOn->nKeys == 0 )  /* last entry deleted! -- remove upward reference */
         {
            if ( tfile4removeRef( tagFile ) != 0 )
               return -1 ;
         }
         else
         {
            if ( blockOn->nKeys < tagFile->header.keysHalf && blockOn->fileBlock != tagFile->header.root )  /* if not root may have to balance the tree */
               return tfile4balance( tagFile, blockOn, 0 ) ;
         }
         break ;

      case 1 :   /* branch delete */
         if( tfile4block( tagFile )->nKeys == 0 )    /* removed the last key of */
            if ( tfile4removeRef( tagFile ) != 0 )
               return -1 ;
         break ;

      default:
         return error4describe( tagFile->codeBase, e4index, E81601, tfile4alias( tagFile ), 0, 0 ) ;
   }
   return 0 ;
}

#endif  /* S4CLIPPER */

#endif /* !S4OFF_WRITE */
#endif /* !S4OFF_INDEX */
#endif /* !S4CLIENT */
