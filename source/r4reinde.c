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

/* r4reinde.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TURBOC__ */



#if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT )
   #include "r4reinde.h"
#endif /* #if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



#if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined(S4CLIENT) && !defined(S4OFF_TRAN)
   void i4deleteRemoveKeys( INDEX4 *index )
   {
      TAG4 *tagOn ;
      TAG4KEY_REMOVED *removed ;

      for( tagOn = 0 ;; )
      {
         tagOn = (TAG4 *)l4next( &index->tags, tagOn ) ;
         if ( tagOn == 0 )
            break ;

         for ( ;; )
         {
            // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
            #ifdef SHARE4TAG_REMOVE
               removed =(TAG4KEY_REMOVED *)l4first( &tagOn->tagFile->removedKeys ) ;
            #else
               removed =(TAG4KEY_REMOVED *)l4first( &tagOn->removedKeys ) ;
            #endif
            if ( removed == 0 )
               break ;
            #ifdef SHARE4TAG_REMOVE
               l4remove( &tagOn->tagFile->removedKeys, removed ) ;
            #else
               l4remove( &tagOn->removedKeys, removed ) ;
            #endif
            u4free( removed ) ;
         }
      }
   }
#endif /* #if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined(S4CLIENT) && !defined(S4OFF_TRAN) */



#if defined( TIME4STATUS ) && defined( S4CLIENT )
   double S4FUNCTION code4status( CODE4 *c4 )
   {
      CONNECT4 *connect = c4getClientConnect( c4 ) ;
      double result ;
      if ( connect == 0 )
         return error4( c4, e4parm, E92906 ) ;
      connect4sendShort( connect, STREAM4BUSY_STATUS ) ;
      connect4sendFlush( connect ) ;
      connect4receive( connect, &result, sizeof( double ), code4timeoutVal( c4 ) ) ;
      return htond( result ) ;
   }
#endif /* #if defined( TIME4STATUS ) && defined( S4CLIENT )*/



#if defined( TIME4STATUS ) && !defined( S4CLIENT )

   // keep a status of the progress of the reindex
   // basically the timer layout is as follows:
   // at startup - percent done = 1% / numTags
   // after initialization - percent done = 2.5% / numTags
   // each tag - 95% / numTags
   // leaves 2.5% for ending sequence after all tags written out
   // we record each of the above increments.
   // in addition to the above increments, we set 2 CODE4 values (incrementVal, incrementMax)
   // the final percentage is c4->percentDone + ( c4->incrementWeight * c4->incrementVal / c4->incrementMax )
   // AS Jun 30/03 - Also support with d4pack now.  In that case, we weight the packing of the data file as reindexing 2 tags.

   // RESTRICTIONS:  The algorithm will give possible incorrect values if # recs in database > ULONG_MAX / 4 which is used as the base for the incrmentorMax
   double S4FUNCTION code4status( CODE4 *c4 )
   {
      // AS Feb 9/06 - added clipper support for packwithstatus
      double perc ;

      switch ( c4->actionCode )
      {
         case ACTION4NONE:
            perc = 1.0 ;
            break ;   // AS Apr 23/04 - was failing to break here
         case ACTION4INITIALIZING:  // CS 2000/01/16
            perc = 0.0 ;
            break ;   // AS Apr 23/04 - was failing to break here
         default:
            perc = c4->percentDone + ( c4->incrementWeight * c4->incrementVal / c4->incrementMax ) ;
            if ( perc > 1.0 )
               perc = 1.0 ;
            break ;   // AS Apr 23/04 - was failing to break here
      }
      return perc ;
   }


   // AS Feb 9/06 - added clipper support for packwithstatus
   // CS 2006/02/07 Added S4OFF_WRITE.
   #if !defined(S4OFF_WRITE)
      // REINDEX4STATUS *r4reindexStatusInit( REINDEX4STATUS *reindexStatus, INDEX4FILE *i4file )
      REINDEX4STATUS *r4reindexStatusInit( REINDEX4STATUS *reindexStatus, INDEX4 *i4 )
      {
         // AS Jun 30/03 - test d4reindexWithProgress
         if ( i4->codeBase->packStatus != 0 )
         {
            return i4->codeBase->packStatus ;
         }
         else
         {
            reindexStatus->c4 = i4->codeBase ;
            reindexStatus->numTags = 0 ;
            #ifdef S4CLIPPER
               for ( TAG4 *tagOn = 0 ;; reindexStatus->numTags++ )
               {
                  tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;
               }
            #else
               INDEX4FILE *i4file = i4->indexFile ;
               for ( TAG4FILE *tagOn = 0 ;; reindexStatus->numTags++ )
               {
                  tagOn = (TAG4FILE *)l4next( &i4file->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;
               }
            #endif
            reindexStatus->tagOn = 0 ;
            reindexStatus->c4->percentDone = 0.01 / reindexStatus->numTags ;  // a basic starting quantity based on number tags
            reindexStatus->c4->incrementWeight = .95 / reindexStatus->numTags ;
            reindexStatus->c4->incrementVal = 0 ;
            // we go through each record approx. twice - once while reading, once while writing
            // LY Dec 9/04 : set incrementMax = 1 if rec count == 0 (avoid divide by zero error)
            reindexStatus->c4->incrementMax = ( 2 * dfile4recCount( i4->data->dataFile, 1 ) > 1 ? 2 * dfile4recCount( i4->data->dataFile, 1 ) : 1 ) ;
            reindexStatus->c4->actionCode = ACTION4REINDEX ;  // CS 2000/01/16 move this line down
            return reindexStatus ;
         }
      }



      void r4reindexStatusInitDone( REINDEX4STATUS *reindexStatus )
      {
         // AS Jun 30/03 - test d4reindexWithProgress - we still include this for packing
         if ( reindexStatus->tagOn == 0 )
         {
            reindexStatus->c4->percentDone = 0.025 / reindexStatus->numTags ;  // a basic init done quantity based on number of tags
            reindexStatus->c4->actionCode = ACTION4REINDEX ;  // CS 2000/01/16 move this line down
         }
         else
         {
            // just add the increment in if we are in pack (there is a tag done)
            reindexStatus->c4->percentDone += 0.025 / reindexStatus->numTags ;  // a basic init done quantity based on number of tags
         }
      }



      void r4reindexStatusNextTag( REINDEX4STATUS *reindexStatus )
      {
         reindexStatus->tagOn++ ;
         double newPercent = .95 * reindexStatus->tagOn / reindexStatus->numTags ;
         assert5( newPercent >= reindexStatus->c4->percentDone ) ;
         reindexStatus->c4->percentDone = newPercent ;
         reindexStatus->c4->incrementVal = 0 ;
      }



      void r4reindexStatusFinalSet( REINDEX4STATUS *reindexStatus, double val )
      {
         reindexStatus->c4->percentDone = val ;
         reindexStatus->c4->incrementVal = 0 ;
         reindexStatus->tagOn = 0 ;
      }



      static void r4reindexStatusFinalLoop( REINDEX4STATUS *reindexStatus )
      {
         reindexStatus->tagOn++ ;
         double newPercent = .975 + .025 * reindexStatus->tagOn / reindexStatus->numTags ;
         assert5( newPercent >= reindexStatus->c4->percentDone ) ;
         reindexStatus->c4->percentDone = newPercent ;
         reindexStatus->c4->incrementVal = 0 ;
      }



      void r4reindexStatusInitUndo( REINDEX4STATUS *reindexStatus )
      {
         reindexStatus->c4->percentDone = 1.0 ;
         reindexStatus->c4->actionCode = ACTION4NONE ;  // CS 2000/01/16 move this line down
      }
   #endif /* S4OFF_WRITE */
#endif /* #if defined( TIME4STATUS ) && !defined( S4CLIENT ) */



#if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && defined( S4CLIENT )
   int S4FUNCTION i4reindex( INDEX4 *index )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( index, 0, E92101 ) )
            return -1 ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( index == 0 )
            return error4( 0, e4parm_null, E92101 ) ;
      #endif

      DATA4 *d4 = index->data ;
      if ( error4code( d4->codeBase ) < 0 )
         return e4codeBase ;

      CONNECTION4 *connection = d4->dataFile->connection ;
      if ( connection == 0 )
         return e4connection ;

      CODE4 *c4 = d4->codeBase ;
      int rc = connection4assign( connection, CON4INDEX_REINDEX, data4clientId( d4 ), data4serverId( d4 ) ) ;
      if ( rc < 0 )
         return rc ;
      connection4addData( connection, index->indexFile->accessName, sizeof( index->indexFile->accessName ), NULL ) ;
      rc = connection4repeat( connection ) ;
      if ( rc == r4locked )
         return r4locked ;
      if ( rc != 0 )
         return connection4error( connection, c4, rc, E92101 ) ;

      if ( connection4len( connection ) != sizeof( CONNECTION4REINDEX_INFO_OUT ) )
         return error4( c4, e4packetLen, E92101 ) ;
      CONNECTION4REINDEX_INFO_OUT *out ;
      out = (CONNECTION4REINDEX_INFO_OUT *)connection4data( connection ) ;
      if ( out->lockedDatafile )
      {
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         d4->dataFile->fileLockServerId = data4serverId( d4 ) ;
         d4->dataFile->fileLockLockId = data4lockId( d4 ) ;
      }

      d4->recNum = -1 ;
      d4->recNumOld = -1 ;
      // AS Jun 28/02 - not doing a proper blank - sometimes null data is present
      // c4memset( d4->record, ' ', dfile4recWidth( d4->dataFile ) ) ;
      d4blankLow( d4, d4->record ) ;

      return 0 ;
   }
#endif /* #if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && defined( S4CLIENT ) */



#if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT )
   int S4FUNCTION i4reindex( INDEX4 *i4 )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( i4, 0, E92101 ) )
            return -1 ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( i4 == 0  )
            return error4( 0, e4parm_null, E92101 ) ;
      #endif

      CODE4 *c4 = i4->codeBase ;

      #ifdef E4ANALYZE
         if ( c4 == 0 )
            return error4( 0, e4struct, E92101 ) ;
      #endif

      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #if !defined( S4OPTIMIZE_OFF ) && defined( S4LOW_MEMORY )
         int hasOpt = c4->hasOpt ;
         code4optSuspend( c4 ) ;
      #endif

      int rc ;
      DATA4 *data = i4->data ;

      #ifndef S4SINGLE
         // AS Jan 10/03 - Don't lock if index is not valid (still creating)
         // because we may not be locking it (create temporary non-updating)
         // and i4createLow() ensures the data file is locked if required.
         if ( i4->isValid != 0 )
         {
            rc = d4lockAllInternal( data, 1 ) ;
            if ( rc )
               return rc ;
         }
      #endif

      #ifndef S4OFF_TRAN
         /* reindex is allowed, but need to fix-up any unique settings */
         i4deleteRemoveKeys( i4 ) ;
      #endif

      R4REINDEX reindex ;
      INDEX4FILE *indexFile = i4->indexFile ;
      TAG4 *tagOn ;

      #ifdef TIME4STATUS
         // AS Jun 30/03 - test d4reindexWithProgress
         REINDEX4STATUS reindexStatus, *reindexStatusPtr ;
         reindexStatusPtr = r4reindexStatusInit( &reindexStatus, i4 ) ;
      #endif

      // AS Mar 26/04 - improved return code handling
      int saveRc = 0 ;

      for( ;; )
      {
         rc = r4reindexInit( &reindex, i4, indexFile ) ;
         if ( rc < 0 )
            break ;
         rc = r4reindexTagHeadersCalc( &reindex ) ;
         if ( rc < 0 )
            break ;
         rc = r4reindexBlocksAlloc(&reindex ) ;
         if ( rc < 0 )
            break ;

         #if !defined( S4SINGLE ) && defined( S4FOX )
            if ( indexFile->file.lowAccessMode != OPEN4DENY_RW )
               indexFile->tagIndex->header.version = indexFile->versionOld + 1 ;
         #endif

         #ifdef E4ANALYZE
            if ( i4->tags.nLink != indexFile->tags.nLink )
            {
               rc = e4struct ;
               break ;
            }
         #endif

         #ifdef S4FOX
            reindex.nBlocksUsed = 0 ;
            char tagName[LEN4TAG_ALIAS + 1] ;
            tagName[LEN4TAG_ALIAS] = '\0' ;

            if ( indexFile->tagIndex->header.typeCode >= 64 )  /* if .cdx */
            {
               reindex.tag = indexFile->tagIndex ;
               rc = sort4init( &reindex.sort, c4, indexFile->tagIndex->header.keyLen, 0 ) ;
               if ( rc < 0 )
                  break ;
               reindex.sort.cmp = (S4CMP_FUNCTION *)u4memcmp ;

               S4LONG tagIndex ;
               for( tagOn = 0, tagIndex = 1 ;; tagIndex++ )
               {
                  tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;
                  int len = c4strlen( tfile4alias( tagOn->tagFile ) ) ;
                  c4memset( tagName, ' ', LEN4TAG_ALIAS ) ;
                  c4memcpy( tagName, tfile4alias( tagOn->tagFile ), (unsigned int)len ) ;
                  B4NODE node ;
                  // AS 07/01/99 -- we only need 1024 bytes for the header, not related to block size...
                  assert5( B4BLOCK_SIZE_INTERNAL == 512 ) ;
                  b4nodeAssignLong( &node, 2 * B4BLOCK_SIZE_INTERNAL * tagIndex / i4multiplier( reindex.indexFile ) ) ;
                  rc = sort4put( &reindex.sort, b4node( node ), tagName, "" ) ;
                  if ( rc < 0 )
                     break ;
                  #ifdef E4MISC
                     reindex.keyCount++ ;
                  #endif /* E4MISC */
               }

               if ( rc < 0 )
                  break ;

               rc = r4reindexWriteKeys( &reindex, e4unique ) ;  /* tag index should have no uniques */
               if ( rc != 0 )
               {
                  r4reindexFree( &reindex ) ;
                  break ;
               }
            }
            else
               if ( reindex.nTags > 1 )   /* should only be 1 tag in an .idx */
               {
                  rc = e4index ;
                  break ;
               }
         #endif  /* S4FOX */

         #ifdef TIME4STATUS
            r4reindexStatusInitDone( reindexStatusPtr ) ;
         #endif

         for( tagOn = 0 ;; )
         {
            tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
            if ( tagOn == 0 )
               break ;
            reindex.tag = tagOn->tagFile ;
            #ifdef S4FOX
               reindex.nBlocksUsed = 0 ;
            #else  /* S4MDX */
               reindex.tag->header.version++ ;
            #endif  /* S4FOX */

            rc = expr4context( tagOn->tagFile->expr, data ) ;
            if ( rc < 0 )
               break ;
            if ( tagOn->tagFile->filter != 0 )
            {
               rc = expr4context( tagOn->tagFile->filter, data ) ;
               if ( rc < 0 )
                  break ;
            }

            rc = r4reindexSupplyKeys( &reindex ) ;
            if ( rc )
            {
               r4reindexFree( &reindex ) ;
               break ;
            }

            rc = r4reindexWriteKeys( &reindex, t4unique( tagOn ) ) ;
            // AS Mar 26/04 - r4uniqueContinue is ok here
            if ( rc == r4uniqueContinue )
            {
               saveRc = r4uniqueContinue ;
               rc = 0 ;
            }

            if ( rc != 0 )
            {
               r4reindexFree( &reindex ) ;
               break ;
            }

            #ifdef TIME4STATUS
               r4reindexStatusNextTag( reindexStatusPtr ) ;
            #endif
         }

         if ( rc != 0 )   /* r4unique or error */
            break ;

         #ifdef TIME4STATUS
            r4reindexStatusFinalSet( reindexStatusPtr, .955 ) ;
         #endif

         rc = r4reindexTagHeadersWrite( &reindex ) ;
         if ( rc < 0 )
            break ;

         #ifdef TIME4STATUS
            r4reindexStatusFinalSet( reindexStatusPtr, .975 ) ;
         #endif

         #ifdef S4FOX
            /* now must fix the right node branches for all blocks by moving leftwards */
            for( tagOn = 0 ; ; )
            {
               tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
               if ( tagOn == 0 )
                  break ;

               for( tfile4rlBottom( tagOn->tagFile ) ; tagOn->tagFile->blocks.lastNode ; tfile4up( tagOn->tagFile ) )
               {
                  B4BLOCK *block = tfile4block( tagOn->tagFile ) ;
                  B4NODE goTo ;
                  b4nodeAssignNode( &goTo, block->header.leftNode ) ;

                  while ( b4nodeValid( goTo ) )
                  {
                     #ifdef E4DEBUG
                        if ( b4nodeInvalid( goTo ) || b4node( goTo ) == 0 )  /* invalid value, esp. zero */
                           return error4( c4, e4struct, E92101 ) ;
                     #endif

                     B4NODE rNode ;
                     b4nodeAssignNode( &rNode, block->fileBlock ) ;
                     if ( block->changed )
                     {
                        rc = b4flush( block ) ;
                        if ( rc < 0 )
                           break ;
                     }

                     FILE4LONG fPos ;
                     b4nodeGetFilePosition( indexFile, goTo, &fPos ) ;
                     rc = file4readAllInternal( &indexFile->file, fPos, &block->header, i4blockSize( reindex.indexFile ) ) ;
                     if ( rc < 0 )
                        break ;

                     #ifdef S4BYTE_SWAP
                        block->header.nodeAttribute = x4reverseShort( (void *)&block->header.nodeAttribute ) ;
                        block->header.nKeys = x4reverseShort( (void *)&block->header.nKeys ) ;
                        block->header.leftNode.node = x4reverseLong( (void *)&block->header.leftNode ) ;
                        block->header.rightNode.node = x4reverseLong( (void *)&block->header.rightNode ) ;

                        // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
                        if (block->header.nodeAttribute & 0x02 ) /* if block is a leaf */
                        {
                           block->nodeHdr.freeSpace = x4reverseShort( (void *)&block->nodeHdr.freeSpace ) ;
                           /* LY 2001/07/22 : changed longVal from long for 64-bit */
                           S4LONG longVal = x4reverseLong( (void *)&block->nodeHdr.recNumMask[0] ) ;
                           c4memcpy( (void *)&block->nodeHdr.recNumMask[0], (void *)&longVal, sizeof(S4LONG) ) ;
                        }
                        else /* if block is a branch */
                        {
                           short shortVal = block->tag->header.keyLen + sizeof(S4LONG) ;
                           /* position swapPtr to end of first key expression */
                           char *swapPtr = (char *) &block->nodeHdr.freeSpace + block->tag->header.keyLen ;

                           /* move through all B4KEY's to swap 'long's */
                           for ( int i = 0 ; i < (int) block->header.nKeys ; i++ )
                           {
                              /* LY 2001/07/22 : changed longVal from long for 64-bit */
                              S4LONG longVal = x4reverseLong( (void *)swapPtr ) ;
                              c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                              swapPtr += sizeof(S4LONG) ;
                              longVal = x4reverseLong( (void *)swapPtr ) ;
                              c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                              swapPtr += shortVal ;
                           }
                        }
                     #endif

                     block->fileBlock = goTo ;
                     assert5( b4nodeValid( block->fileBlock ) ) ;
                     if ( b4nodesNotEqual( block->header.rightNode, rNode ) )  /* if a bad value */
                     {
                        b4nodeAssignNode( &block->header.rightNode, rNode ) ;
                        assert5( b4node( block->header.rightNode ) != 0 ) ;  // 0 always invalid...
                        block->changed = 1 ;
                     }
                     b4nodeAssignNode( &goTo, block->header.leftNode ) ;
                  }
                  if ( rc < 0 )
                     break ;
                  block->builtOn = -1 ;
                  rc = b4top( block ) ;
                  if ( rc < 0 )
                     break ;
               }
               if ( rc < 0 )
                  break ;
               // AS Jan 31/03 - flush the final updates to the file.  This is particularly important if creating a temporary index file.
               tfile4update( tagOn->tagFile ) ;

               #ifdef TIME4STATUS
                  r4reindexStatusFinalLoop( reindexStatusPtr ) ;
               #endif
            }
            if ( rc < 0 )
               break ;
         #endif  /* S4FOX */

         r4reindexFree( &reindex ) ;
         #if !defined( S4OPTIMIZE_OFF ) && defined( S4LOW_MEMORY )
            if ( hasOpt )
               code4optRestart( c4 ) ;
         #endif

         break ;
      }

      #ifdef TIME4STATUS
         r4reindexStatusInitUndo( reindexStatusPtr ) ;
      #endif

      if ( rc < 0 )
      {
         // AS 06/26/00 was not freeing up reindex stufff...
         r4reindexFree( &reindex ) ;
         return rc ;
      }

      data->recNum = -1 ;
      data->recNumOld = -1 ;
      d4blankLow( data, data->record ) ;

      // AS Mar 26/04 - improved return code handling
      if ( rc == 0 )
         return saveRc ;

      return rc ;
   }



   int r4reindexInit( R4REINDEX *r4, INDEX4 *i4, INDEX4FILE *indexFile )
   {
      #ifdef E4PARM_LOW
         if ( r4 == 0 || i4 == 0 || indexFile == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      c4memset( (void *)r4, 0, sizeof( R4REINDEX ) ) ;

      r4->indexFile = indexFile ;
      r4->data = i4->data ;
      r4->dataFile = indexFile->dataFile ;
      r4->codeBase = i4->codeBase ;

      r4->minKeysmax = INT_MAX ;
      r4->startBlock = 0 ;
      r4->sort.file.hand = INVALID4HANDLE ;

      #ifndef S4FOX
         r4->blocklen = indexFile->header.blockRw ;
      #endif  /* S4FOX */

      r4->bufferLen = i4->codeBase->memSizeSortBuffer ;
      if ( r4->bufferLen < 1024 )
         r4->bufferLen = 1024 ;

      r4->buffer = (char *)u4allocEr( i4->codeBase, (S4LONG)r4->bufferLen ) ;
      if ( r4->buffer == 0 )
         return e4memory ;

      #ifdef S4FOX
         b4nodeAssignLong( &r4->lastblock, 1024 / i4multiplier( indexFile ) ) ;  /* leave space for the index header block  */
      #endif  /* S4FOX */

      return 0 ;
   }



   void r4reindexFree( R4REINDEX *r4 )
   {
      // AS Apr 7/04 - need to ensure that the seqwrite is flushed to disk before freeing up the used buffer
      file4seqWriteFlush( &r4->seqwrite ) ;
      u4free( r4->buffer ) ;
      u4free( r4->startBlock ) ;
      sort4free( &r4->sort ) ;
   }



   int r4reindexBlocksAlloc( R4REINDEX *r4 )
   {
      S4LONG onCount ;

      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      #ifdef E4MISC
         if ( (unsigned)r4->minKeysmax > INT_MAX )
            return error4( r4->codeBase, e4info, E92102 ) ;
      #endif

      /* Calculate the block stack height */
      onCount = dfile4recCount( r4->dataFile, -2 ) ;
      /* AS 08/12/98 was invlaidly returinging onCount on minKeysmax error, changed by splitting lines */
      if ( onCount < 0 )
         return error4( r4->codeBase, (short)onCount, E92102 ) ;
      if ( r4->minKeysmax <= 1 )
         return error4( r4->codeBase, e4index, E92102 ) ;
      for ( r4->nBlocks = 2; onCount != 0L; r4->nBlocks++ )
         onCount /= r4->minKeysmax ;

      if ( r4->startBlock == 0 )
      {
         #ifdef S4FOX
            r4->startBlock = (R4BLOCK_DATA *)u4allocEr( r4->codeBase, (S4LONG)i4blockSize( r4->indexFile ) * r4->nBlocks ) ;
         #endif  /* S4FOX */

         #ifdef S4MDX
            r4->startBlock = (R4BLOCK_DATA *)u4allocEr( r4->codeBase, (S4LONG)r4->blocklen * r4->nBlocks ) ;
         #endif  /* S4MDX */
      }

      if ( r4->startBlock == 0 )
         return e4memory ;

      return 0 ;
   }



   int r4reindexSupplyKeys( R4REINDEX *r4 )
   {
      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      #ifdef E4MISC
         r4->keyCount = 0L ;
      #endif  /* E4MISC */

      TAG4FILE *t4 = r4->tag ;
      CODE4 *c4 = r4->codeBase ;
      // AS 10/05/00 in some cases with very large sorts, the sort module
      // was being exceded.  Calculate the maximum sort limits, and adjust
      // the pool size if necessary to increase the maximum capacity.
      DATA4FILE *dataFile = r4->dataFile ;
      S4LONG count = dfile4recCount( dataFile, -2L ) ;
      // the maximum # of spools allowable in total:  need 1 pointer available
      // for each spool
      // int maxSpools = ( c4->memSizeSortPool - sizeof( LINK4 ) ) / sizeof(char *) ;
      // spools required: # records * sizeof( pointer ) / pool size (once we run out of pointers, the pool is flushed)
      // int spoolsRequired = count * ( sizeof(char *) ) / ( c4->memSizeSortPool - sizeof( LINK4 ) ) ;

      int rc = sort4init( &r4->sort, c4, t4->header.keyLen, 0 ) ;
      if ( rc < 0 )
         return error4stack( c4, (short)rc, E92102 ) ;

      #ifdef S4FOX
         r4->sort.cmp = (S4CMP_FUNCTION *)u4memcmp ;
      #endif  /* S4FOX */

      #ifdef S4MDX
         r4->sort.cmp = (S4CMP_FUNCTION *)t4->cmp ;
      #endif  /* S4MDX */

      EXPR4 *filter = t4->filter ;
      if ( count < 0 )
         return error4stack( c4, (short)rc, E92102 ) ;

      rc = expr4context( r4->tag->expr, r4->data ) ;
      if ( rc < 0 )
         return rc ;
      if ( r4->tag->filter != 0 )
      {
         rc = expr4context( r4->tag->filter, r4->data ) ;
         if ( rc < 0 )
            return rc ;
      }

      FILE4SEQ_READ seqRead ;
      file4seqReadInitDo( &seqRead, &dataFile->file, dfile4recordPosition( dataFile, 1L ), r4->buffer, r4->bufferLen, 1 ) ;

      S4LONG iRec ;
      for ( iRec = 1L; iRec <= count; iRec++ )
      {
         rc = file4seqReadAll( &seqRead, dataFile->record, dfile4recWidth( dataFile ) ) ;
         if ( rc < 0 )
            return error4stack( c4, (short)rc, E92102 ) ;
         r4->data->recNum = iRec ;

         #ifndef S4MEMO_OFF
            for ( int fieldIndex = 0; fieldIndex < dataFile->nFieldsMemo; fieldIndex++ )
               f4memoReset( r4->data->fieldsMemo[fieldIndex].field ) ;
         #endif  /* S4MEMO_OFF */

         if ( filter )
         {
            int *filterResult ;
            rc = expr4vary( filter, (char **)&filterResult ) ;
            if ( rc < 0 )
            {
               #ifdef S4ADVANCE_READ
                  file4seqReadInitUndo( &seqRead ) ;
               #endif
               return error4stack( c4, (short)rc, E92102 ) ;
            }
            #ifdef E4MISC
               if ( expr4type( filter ) != r4log )
               {
                  #ifdef S4ADVANCE_READ
                     file4seqReadInitUndo( &seqRead ) ;
                  #endif
                  return error4( c4, e4result, E92102 ) ;
               }
            #endif  /* E4MISC */
            if ( ! *filterResult )
               continue ;
            t4->hasKeys = 1 ;
            #ifdef S4MDX
               t4->hadKeys = 0 ;
            #endif
         }

         unsigned char *keyResult ;
         tfile4exprKey( t4, &keyResult ) ;
         rc = sort4put( &r4->sort, iRec, keyResult, "" ) ;
         if ( rc < 0 )
         {
            #ifdef S4ADVANCE_READ
               file4seqReadInitUndo( &seqRead ) ;
            #endif
            return error4stack( c4, (short)rc, E92102 ) ;
         }
         #ifdef E4MISC
            r4->keyCount++ ;
         #endif  /* E4MISC */
         // BCR 10/18/00 -- InterlockedIncrement is a Windows thread library function
         #if defined( TIME4STATUS ) && !defined( S4OFF_THREAD )
            InterlockedIncrement( &(c4->incrementVal) ) ;
         #endif
      }

      #ifdef S4ADVANCE_READ
         file4seqReadInitUndo( &seqRead ) ;
      #endif

      return 0 ;
   }



   int r4reindexTagHeadersCalc( R4REINDEX *r4 )
   {
      #ifdef S4FOX
         #ifdef S4DATA_ALIGN
            unsigned int size, delta ;
         #endif
         int keysmax, exprType ;
      #endif  /* S4FOX */

      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      CODE4 *c4 = r4->codeBase ;
      r4->nTags = 0 ;
      INDEX4FILE *i4file = r4->indexFile ;
      for ( TAG4FILE *tag = 0 ;; )
      {
         tag = (TAG4FILE *)l4next( &i4file->tags, tag ) ;
         if ( tag == 0 )
            break ;
         int rc = tfile4freeAll( tag ) ;
         if ( rc < 0 )
            return error4stack( c4, (short)rc, E92102 ) ;

         expr4context( tag->expr, r4->data ) ;

         #ifdef S4FOX
            // AS 07/29/99 - need to know if unicode before calling key len...
            exprType = expr4type( tag->expr ) ;
            if ( exprType == r5wstr || exprType == r5wstrLen )
               tag->isUnicode = 1 ;
         #endif

         tag->header.keyLen = (short)expr4keyLen( tag->expr ) ;
         #ifdef S4FOX
            /* AS 08/17/99 --> was re-creating these memory objects when already allocated by open... */
            if ( tag->builtKeyMemory == 0 )
            {
               #ifdef S4DATA_ALIGN
                  size = (unsigned int)sizeof(S4LONG) + tag->header.keyLen ;
                  delta = sizeof(void *) - size % sizeof(void *) ;
                  tag->builtKeyMemory = mem4create( c4, 3, size + delta, 2, 0 ) ;
               #else
                  tag->builtKeyMemory = mem4create( c4, 3, (unsigned int)sizeof(S4LONG) + tag->header.keyLen + 1, 2, 0 ) ;
               #endif
            }

            if ( exprType < 0 )
               return error4stack( c4, (short)rc, E92102 ) ;

            tfile4initSeekConv( tag, exprType ) ;
            if ( tag->header.keyLen < 0 )
               return error4( c4, e4index, E92102 ) ;

            keysmax = ( i4blockSize( r4->indexFile ) - sizeof(B4STD_HEADER) ) / ( tag->header.keyLen + 2*sizeof(S4LONG) ) ;

            if ( keysmax < r4->minKeysmax )
               r4->minKeysmax = keysmax ;
         #endif  /* S4FOX */

         #ifdef S4MDX
            if ( tag->header.keyLen < 0 )
               return error4( c4, e4index, E92102 ) ;

            rc = expr4type( tag->expr ) ;
            if ( rc < 0 )
               return error4( c4, rc, E92102 ) ;

            tag->header.type = (char)rc ;

            if ( tag->header.type == r4dateDoub )
               tag->header.type = r4date ;

            if ( tag->header.type == r4numDoub )
               tag->header.type = r4num ;

            tfile4initSeekConv( tag, tag->header.type ) ;
            tag->header.groupLen = (short)(tag->header.keyLen+ 2*sizeof(S4LONG)-1) ;
            tag->header.groupLen-= (short)(tag->header.groupLen % sizeof(S4LONG)) ;
            tag->header.isDate = ( tag->header.type == r4date ) ? (short) 1 : (short) 0 ;
            tag->header.keysMax = (short)((i4file->header.blockRw - sizeof(short) - 6 - sizeof(S4LONG)) / tag->header.groupLen) ;

            if ( tag->header.keysMax < r4->minKeysmax )
               r4->minKeysmax = tag->header.keysMax ;

            tag->hasKeys = 0 ;
            tag->hadKeys = 1 ;
         #endif  /* S4MDX */

         // AS Jul 30/04 - always enforce this restriction since it is part of the documented functionality
         if ( I4MAX_KEY_SIZE_COMPATIBLE < tag->header.keyLen )
         {
            // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE
            // AS Dec 5/07 - if we are just reindexing the file (not creating) we don't need to do this check
            // - we may not know if we are creating or reindexing...but we can check the index block size.  If it mismatches the CODE4 setting (large indexes) we know we are not creating.
            // AS Feb 15/08 - c4indexBlockSizeGet only applies in fox
            #ifdef S4FOX
               if ( c4->limitKeySize == 1 && ( i4blockSize( tag->indexFile ) == c4indexBlockSizeGet( c4 ) ) )
            #else
               if ( c4->limitKeySize == 1  )
            #endif
            {
               if ( c4->oledbSchemaCreate == 1 )  // ensure via real max length (not-compatible)
               {
                  if ( I4MAX_KEY_SIZE < tag->header.keyLen )
                     return error4( c4, e4index, E82102 ) ;
               }
               else
                  return error4( c4, e4index, E82102 ) ;
            }
         }

         r4->nTags++ ;
      }

      #ifdef S4FOX
         if ( i4file->tagIndex->header.typeCode >= 64 )
         {
            FILE4LONG filePos ;
            b4nodeGetFilePosition( i4file, r4->lastblock, &filePos ) ;
            // for tag headers, we need 1024 bytes, or 2 * B4BLOCK_SIZE_INTERNAL + 1 extra block to contain
            // the block for the tagIndex (this is a full block).
            // this value is always small enough to fit into an unsigned long, so assign directly...
            file4longAdd( &filePos, (((S4UNSIGNED_LONG) r4->nTags) * 2 * B4BLOCK_SIZE_INTERNAL ) ) ;
            b4nodeSetFromFilePosition( i4file, &r4->lastblock, filePos ) ;
            tfile4initSeekConv( i4file->tagIndex, r4str ) ;
         }
         // we always add 1 block to last block before using, so compensate for that here...
         b4nodeSubtractBlocks( &r4->lastblock, i4file, 1 ) ;
      #endif  /* S4FOX */

      #ifdef S4MDX
         r4->lastblockInc = i4file->header.blockRw / 512 ;
         r4->lastblock = 4 + (r4->nTags-1)*r4->lastblockInc ;
      #endif  /* S4MDX */

      return 0 ;
   }



   TAG4FILE *index4fileFindITag( INDEX4FILE *indexFile, const int tagNum )
   {
      /* First 'iTag' starts at '1' for this specific routine */
      TAG4FILE *tagOn ;
      int iTag ;

      #ifdef E4PARM_LOW
         if ( indexFile == 0 || tagNum <= 0 )
         {
            error4( 0, e4parm, E92102 ) ;
            return 0 ;
         }
      #endif

      iTag = tagNum ;

      tagOn = (TAG4FILE *)l4first( &indexFile->tags ) ;

      while ( --iTag >= 1 )
      {
         tagOn = (TAG4FILE *) l4next( &indexFile->tags, tagOn ) ;
         if ( tagOn == 0 )
            return 0 ;
      }
      return tagOn ;
   }
#endif /* #if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



#if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) && defined( S4MDX )
   #define GARBAGE_LEN 518

   int r4reindexTagHeadersWrite( R4REINDEX *r4 )
   {
      /* First, calculate the T4DESC.leftChld, T4DESC.rightChld values, T4DESC.parent values */
      int rc, higher[49], lower[49], parent[49] ;
      TAG4FILE *tagOn, *tagPtr ;
      INDEX4FILE *i4 ;
      DATA4 *d4 ;
      CODE4 *c4 ;
      int nTag, iTag, jField, len, saveCode ;
      T4DESC tagInfo ;
      const char *ptr ;
      FILE4LONG fPos ;
      #ifdef S4BYTE_SWAP
         I4HEADER swapHeader ;
         T4HEADER swapTagHeader ;
      #endif  /* S4BYTE_SWAP */

      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      c4memset( (void *)higher, 0, sizeof( higher ) ) ;
      c4memset( (void *)lower,  0, sizeof( lower ) ) ;
      c4memset( (void *)parent, 0, sizeof( parent ) ) ;

      i4 = r4->indexFile ;
      d4 = r4->data ;
      c4 = r4->codeBase ;

      #ifdef E4ANALYZE
         if ( i4 == 0 || d4 == 0 )
            return error4( c4, e4struct, E92102 ) ;
      #endif

      tagOn = (TAG4FILE *) l4first( &i4->tags ) ;
      if ( tagOn != 0 )
      {
         nTag = 1 ;

         for ( ;; )
         {
            tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn) ;
            if ( tagOn == 0 )
               break ;
            nTag++ ;
            iTag = 1 ;
            for (;;)
            {
               tagPtr = index4fileFindITag( r4->indexFile, iTag ) ;
               #ifdef E4MISC
                  if ( tagPtr == 0 || iTag < 0 || iTag >= 48 || nTag > 48 )
                     return error4( c4, e4result, E92102 ) ;
               #endif  /* E4MISC */
               if ( u4memcmp( tagOn->alias, tagPtr->alias, sizeof(tagOn->alias)) < 0)
               {
                  if ( lower[iTag] == 0 )
                  {
                     lower[iTag] = nTag ;
                     parent[nTag] = iTag ;
                     break ;
                  }
                  else
                     iTag = lower[iTag] ;
               }
               else
               {
                  if ( higher[iTag] == 0 )
                  {
                     higher[iTag] = nTag ;
                     parent[nTag] = iTag ;
                     break ;
                  }
                  else
                     iTag = higher[iTag] ;
               }
            }
         }
      }

      /* Now write the headers */
      file4longAssign( fPos, 0, 0 ) ;
      file4seqWriteInitLow( &r4->seqwrite, &i4->file, fPos, r4->buffer, r4->bufferLen ) ;

      i4->header.eof = r4->lastblock + r4->lastblockInc ;
      i4->header.freeList = 0L ;
      u4yymmdd( i4->header.yymmdd ) ;

      #ifdef S4BYTE_SWAP
         c4memcpy( (void *)&swapHeader, (void *)&i4->header, sizeof(I4HEADER) ) ;

         swapHeader.blockChunks = x4reverseShort( (void *)&swapHeader.blockChunks ) ;
         swapHeader.blockRw = x4reverseShort( (void *)&swapHeader.blockRw ) ;
         swapHeader.slotSize = x4reverseShort( (void *)&swapHeader.slotSize ) ;
         swapHeader.numTags = x4reverseShort( (void *)&swapHeader.numTags ) ;
         swapHeader.eof = x4reverseLong( (void *)&swapHeader.eof ) ;
         swapHeader.freeList = x4reverseLong( (void *)&swapHeader.freeList ) ;

         rc = file4seqWrite( &r4->seqwrite, &swapHeader, sizeof(I4HEADER) ) ;
      #else
         rc = file4seqWrite( &r4->seqwrite, &i4->header, sizeof(I4HEADER) ) ;
      #endif  /* S4BYTE_SWAP */

      if ( rc < 0 )
         return error4stack( c4, rc, E92102 ) ;

      rc = file4seqWriteRepeat( &r4->seqwrite, 512-sizeof(I4HEADER)+17, 0 ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E92102 ) ;

      /* There is a 0x01 on byte 17 of the first 32 bytes. */
      rc = file4seqWrite( &r4->seqwrite, "\001", 1 ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E92102 ) ;

      rc = file4seqWriteRepeat( &r4->seqwrite, 14, 0 ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E92102 ) ;

      tagOn = (TAG4FILE *) l4first( &i4->tags ) ;

      for ( iTag = 0; iTag < 47; iTag++ )
      {
         c4memset( (void *)&tagInfo, 0, sizeof(tagInfo) ) ;

         if ( iTag < r4->nTags )
         {
            tagInfo.headerPos = 4 + (S4UNSIGNED_LONG) iTag * r4->lastblockInc ;
            b4nodeAssignNode( &tagOn->headerOffset, tagInfo.headerPos ) ;

            c4memcpy( (void *)tagInfo.tag, tagOn->alias, sizeof(tagInfo.tag) ) ;

            tagInfo.indexType = tagOn->header.type ;

            #ifdef S4BYTE_SWAP
               tagInfo.headerPos = x4reverseLong( (void *)&tagInfo.headerPos ) ;
               tagInfo.x1000 = 0x0010 ;
            #else
               tagInfo.x1000 = 0x1000 ;
            #endif  /* S4BYTE_SWAP */

            tagInfo.x2 = 2 ;
            tagInfo.leftChld = (char) lower[iTag+1] ;
            tagInfo.rightChld = (char) higher[iTag+1] ;
            tagInfo.parent = (char) parent[iTag+1] ;

            if ( i4->header.isProduction )
            {
               saveCode = c4->errFieldName ;
               c4->errFieldName = 0 ;
               jField = d4fieldNumber( d4, tagOn->expr->source ) ;
               c4->errFieldName = saveCode ;
               if ( jField > 0 )
               {
                  file4longAssign( fPos, ( jField + 1 ) * sizeof( FIELD4IMAGE ) - 1, 0 ) ;
                  rc = file4writeInternal( &r4->dataFile->file, fPos, "\001", 1 ) ;
                  if ( rc < 0 )
                     return error4stack( c4, rc, E92102 ) ;
               }
            }
            tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;
         }
         rc = file4seqWrite( &r4->seqwrite, &tagInfo, sizeof( T4DESC ) ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E92102 ) ;
      }

      for (tagOn = 0 ;; )
      {
         tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;
         if ( tagOn == 0 )
            break ;
         #ifdef S4BYTE_SWAP
            c4memcpy( (void *)&swapTagHeader, (void *)&tagOn->header, sizeof(T4HEADER) ) ;

            swapTagHeader.root = x4reverseLong( (void *)&swapTagHeader.root ) ;
            swapTagHeader.keyLen = x4reverseShort( (void *)&swapTagHeader.keyLen ) ;
            swapTagHeader.keysMax = x4reverseShort( (void *)&swapTagHeader.keysMax ) ;
            swapTagHeader.groupLen = x4reverseShort( (void *)&swapTagHeader.groupLen ) ;
            swapTagHeader.isDate = x4reverseShort( (void *)&swapTagHeader.isDate ) ;
            swapTagHeader.unique = x4reverseShort( (void *)&swapTagHeader.unique ) ;

            rc = file4seqWrite( &r4->seqwrite, &swapTagHeader, sizeof( T4HEADER ) ) ;
         #else
            rc = file4seqWrite( &r4->seqwrite, &tagOn->header, sizeof( T4HEADER ) ) ;
         #endif  /* S4BYTE_SWAP */
         if ( rc < 0 )
            return error4stack( c4, rc, E92102 ) ;

         ptr = tagOn->expr->source ;
         len = c4strlen( ptr ) ;
         rc = file4seqWrite( &r4->seqwrite, ptr, len) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E92102 ) ;

         rc = file4seqWriteRepeat( &r4->seqwrite, 221-len, 0 ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E92102 ) ;

         if( tagOn->filter != 0 )
         {
            rc = file4seqWriteRepeat( &r4->seqwrite, 1, 1 ) ;
            if ( rc < 0 )
               return error4stack( c4, rc, E92102 ) ;
            if ( tagOn->hasKeys )
               rc = file4seqWriteRepeat( &r4->seqwrite, 1, 1 ) ;
            else
               rc = file4seqWriteRepeat( &r4->seqwrite, 1, 0 ) ;
         }
         else
            rc = file4seqWriteRepeat( &r4->seqwrite, 2, 0 ) ;

         if ( rc < 0 )
            return error4stack( c4, rc, E92102 ) ;

         /* write extra space up to filter write point */
         rc = file4seqWriteRepeat( &r4->seqwrite, GARBAGE_LEN-3 , 0 ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E92102 ) ;

         if ( tagOn->filter == 0 )
            len = 0 ;
         else
         {
            ptr = tagOn->filter->source ;
            len = c4strlen(ptr) ;
            rc = file4seqWrite( &r4->seqwrite, ptr, len ) ;
            if ( rc < 0 )
               return error4stack( c4, rc, E92102 ) ;
         }
         rc = file4seqWriteRepeat( &r4->seqwrite, r4->blocklen - GARBAGE_LEN - len - 220 - sizeof(tagOn->header), 0 ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E92102 ) ;
      }
      // AS Nov 18/03 - large file support
      file4longAssign( fPos, i4->header.eof, 0 ) ;
      file4longMultiply( fPos, 512 ) ;
      rc = file4lenSetLow( &i4->file, fPos ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E92102 ) ;

      rc =  file4seqWriteFlush( &r4->seqwrite ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E92102 ) ;
      return 0 ;
   }



   // S4MDX
   int r4reindexWriteKeys( R4REINDEX *r4, short int errUnique )
   {
      char  lastKey[I4MAX_KEY_SIZE], *keyData ;
      int   isUnique, rc, isFirst ;
      S4LONG  keyRec ;
      FILE4LONG pos ;

      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      TAG4FILE *t4 = r4->tag ;

      r4->grouplen = t4->header.groupLen ;
      r4->valuelen = t4->header.keyLen ;
      r4->keysmax = t4->header.keysMax ;
      c4memset( (void *)r4->startBlock, 0, r4->nBlocks*r4->blocklen ) ;
      rc = sort4getInit( &r4->sort ) ;
      if ( rc < 0 )
         return error4stack( r4->codeBase, rc, E92102 ) ;

      // AS Jun 16/04 - the multipliciation may actually make the result into a LONGLONG, so split it up (large file support)
      file4longAssign( pos, ( r4->lastblock + r4->lastblockInc ), 0 ) ;
      file4longMultiply( pos, 512 ) ;
      file4seqWriteInitLow( &r4->seqwrite, &r4->indexFile->file, pos, r4->buffer,r4->bufferLen) ;

      // #ifdef E4MISC CJ-22/11/99 - always want to test to see if key is too long
         if ( I4MAX_KEY_SIZE_COMPATIBLE < r4->sort.sortLen )
         {
            if ( r4->codeBase->oledbSchemaCreate == 1 )  // ensure via real max length (not-compatible)
            {
               if ( I4MAX_KEY_SIZE < r4->sort.sortLen )
                  return error4( r4->codeBase, e4index, E82102 ) ;
            }
            else
               return error4( r4->codeBase, e4index, E82102 ) ;
         }
      // #endif

      isFirst = 1 ;
      isUnique = t4->header.unique ;
      // AS Mar 26/04 - improved return code handling
      int rcSave = 0 ;

      for(;;)  /* For each key to write */
      {
         #ifdef TIME4STATUS
            InterlockedIncrement( &(r4->codeBase->incrementVal) ) ;
         #endif
         void *dummyPtr ;
         rc = sort4get( &r4->sort, &keyRec, (void **) &keyData, &dummyPtr) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, rc, E92102 ) ;

         #ifdef E4MISC
            if ( r4->keyCount < 0L || r4->keyCount == 0L && rc != r4done || r4->keyCount > 0L && rc == r4done )
               return error4( r4->codeBase, e4info, E92102 ) ;
            r4->keyCount-- ;
         #endif

         if ( rc == r4done )  /* No more keys */
         {
            rc = r4reindexFinish( r4 ) ;
            if ( rc < 0 )
               return error4stack( r4->codeBase, rc, E92102 ) ;
            rc = file4seqWriteFlush( &r4->seqwrite ) ;
            if ( rc < 0 )
               return error4stack( r4->codeBase, rc, E92102 ) ;
            break ;
         }

         if ( isUnique )
         {
            if( isFirst )
               isFirst = 0 ;
            else
               if ( (*t4->cmp)( keyData, lastKey, r4->sort.sortLen) == 0 )
               {
                  switch( errUnique )
                  {
                     case e4unique:
                        return error4describe( r4->codeBase, e4unique, E82103, t4->alias, (char *)0, (char *)0 ) ;
                     // AS Mar 26/04 - need to ensure r4uniqueContinue eventually returned in this case
                     case r4unique:
                        return r4unique ;
                     // AS Mar 26/04 - need to ensure r4uniqueContinue eventually returned in this case
                     case r4uniqueContinue:
                        rcSave = r4uniqueContinue ;
                     default:
                        continue ;
                  }
               }
            c4memcpy( lastKey, keyData, r4->sort.sortLen ) ;
         }

         /* Add the key */
         rc = r4reindexAdd( r4, keyRec, (unsigned char *)keyData ) ;
         if ( rc < 0 )
            return rc ;
      }

      /* Now complete the tag header info. */
      t4->header.root = r4->lastblock ;
      return rcSave ;
   }



   static int r4reindexToDisk( R4REINDEX *r4 )
   {
      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      /* Writes out the current block and adds references to higher blocks */
      R4BLOCK_DATA *block = r4->startBlock ;
      int iBlock = 0 ;

      B4KEY_DATA *keyOn = (B4KEY_DATA *) (block->info + (block->nKeys-1) * r4->grouplen) ;

      #ifdef E4ANALYZE
         S4LONG dif = (char *) keyOn -  (char *) block ;
         if ( ( (unsigned)dif + r4->grouplen ) > r4->blocklen || dif < 0 )
            return error4( r4->codeBase, e4result, E92102 ) ;
      #endif

      for( ;; )
      {
         #ifdef S4BYTE_SWAP
            char *swapPtr ;
            int i ;
            S4LONG longVal ;
            short shortVal ;

            char *swap = (char *) u4allocEr( r4->codeBase, r4->blocklen + sizeof(S4LONG) ) ;
            if ( swap == 0 )
               return error4stack( r4->codeBase, e4memory, E92102 ) ;

            c4memcpy( (void *)swap, (void *)block, r4->blocklen ) ;
            /* position swapPtr at beginning of B4KEY's */
            swapPtr = swap ;
            swapPtr += 6 + sizeof(short) ;
            /* move through all B4KEY's to swap 'long' */
            for ( i = 0 ; i < (*(short *)swap) ; i++ )
            {
               longVal = x4reverseLong( (void *)swapPtr ) ;
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               swapPtr += r4->grouplen ;
            }

            longVal = x4reverseLong( (void *)swapPtr ) ;
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;

            /* swap the numKeys value */
            shortVal = x4reverseShort( (void *)swap ) ;
            c4memcpy( swap, (void *) &shortVal, sizeof(short) ) ;

            int rc = file4seqWrite( &r4->seqwrite, swap, r4->blocklen) ;
            u4free( swap ) ;
         #else
            int rc = file4seqWrite( &r4->seqwrite, block, r4->blocklen ) ;
         #endif  /* S4BYTE_SWAP */
         if ( rc < 0 )
            return error4stack( r4->codeBase, rc, E92102 ) ;

         if ( iBlock )
            c4memset( (void *)block, 0, r4->blocklen ) ;
         r4->lastblock += r4->lastblockInc ;

         block = (R4BLOCK_DATA *) ((char *)block + r4->blocklen) ;
         iBlock++ ;
         #ifdef E4ANALYZE
            if ( iBlock >= r4->nBlocks )
               return error4( r4->codeBase, e4info, E92102 ) ;
         #endif  /* E4ANALYZE */

         B4KEY_DATA *keyTo = (B4KEY_DATA *) (block->info +  block->nKeys * r4->grouplen) ;
         #ifdef E4ANALYZE
            dif = (char *) keyTo -  (char *) block  ;
            if ( ( (unsigned)dif + sizeof(S4LONG ) ) > r4->blocklen || dif < 0 )
               return error4( r4->codeBase, e4result, E92102 ) ;
         #endif  /* E4ANALYZE */
         keyTo->num = r4->lastblock ;

         if ( block->nKeys < r4->keysmax )
         {
            block->nKeys++ ;
            #ifdef E4ANALYZE
               if ( ((unsigned)dif+r4->grouplen) > r4->blocklen )
                  return error4( r4->codeBase, e4result, E92102 ) ;
            #endif  /* E4ANALYZE */
            c4memcpy( keyTo->value, keyOn->value, r4->valuelen ) ;
            return 0 ;
         }
      }
   }



   int r4reindexAdd( R4REINDEX *r4, const S4LONG rec, const unsigned char *keyValue )
   {
      /*
         DESCRIPTION

         This function is used to add an entry into the index.  It is only used to insert
         keys into leaf blocks, not branch blocks.

         PARAMATERS

         r4 is the reindex structure

         rec is the record number of the record from which the key was generated

         keyValue is the key to add to the index
      */
      int rc ;
      #ifdef E4ANALYZE
         S4LONG dif ;
      #endif

      #ifdef E4PARM_LOW
         if ( r4 == 0 || rec < 0 || keyValue == 0 )
            return error4( 0, e4parm, E92102 ) ;
      #endif

      R4BLOCK_DATA *startBlock = r4->startBlock ;
      if ( startBlock->nKeys >= r4->keysmax )
      {
         rc = r4reindexToDisk( r4 ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, rc, E92102 ) ;
         c4memset( (void *)startBlock, 0, r4->blocklen ) ;
      }

      B4KEY_DATA *keyTo = (B4KEY_DATA *)( startBlock->info + (startBlock->nKeys++) * r4->grouplen ) ;

      #ifdef E4ANALYZE
         dif = (char *) keyTo -  (char *) startBlock ;
         if ( ((unsigned)dif + r4->grouplen) > r4->blocklen || dif < 0 )
            return error4( r4->codeBase, e4index, E92102 ) ;
      #endif
      keyTo->num = rec ;
      c4memcpy( (void *)keyTo->value, keyValue, r4->valuelen ) ;

      return 0 ;
   }



   int r4reindexFinish( R4REINDEX *r4 )
   {
      int iBlock, rc ;
      B4KEY_DATA *keyTo ;
      #ifdef E4ANALYZE
         S4LONG dif ;
      #endif

      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      #ifdef S4BYTE_SWAP
         char *swapPtr ;
         int i ;
         S4LONG longVal ;
         short shortVal ;

         char *swap = (char *)u4allocEr( r4->codeBase, r4->blocklen ) ;
         if ( swap == 0 )
            return error4stack( r4->codeBase, e4memory, E92102 ) ;

         c4memcpy( (void *)swap, (void *)r4->startBlock, r4->blocklen ) ;
         /* position swapPtr at beginning of B4KEY's */
         swapPtr = swap ;
         swapPtr += 6 + sizeof(short) ;
         /* move through all B4KEY's to swap 'long' */
         for ( i = 0 ; i < (* (short *)swap) ; i++ )
         {
            longVal = x4reverseLong( (void *)swapPtr ) ;
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += r4->grouplen ;
         }
         /* swap the numKeys value */
         shortVal = x4reverseShort( (void *)swap ) ;
         c4memcpy( swap, (void *)&shortVal, sizeof(short) ) ;

         rc = file4seqWrite( &r4->seqwrite, swap ,r4->blocklen ) ;
         if ( rc < 0 )
         {
            u4free( swap ) ;
            return error4stack( r4->codeBase, rc, E92102 ) ;
         }
      #else
         rc = file4seqWrite( &r4->seqwrite, r4->startBlock, r4->blocklen ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, rc, E92102 ) ;
      #endif  /* S4BYTE_SWAP */

      r4->lastblock += r4->lastblockInc ;
      R4BLOCK_DATA *block = r4->startBlock ;

      for( iBlock=1; iBlock < r4->nBlocks; iBlock++ )
      {
         block = (R4BLOCK_DATA *)( (char *)block + r4->blocklen ) ;
         if ( block->nKeys >= 1 )
         {
            keyTo = (B4KEY_DATA *) (block->info + block->nKeys*r4->grouplen) ;
            #ifdef E4ANALYZE
               dif = (char *) keyTo  -  (char *) block ;
               if ( ( (unsigned)dif + sizeof(S4LONG ) ) > r4->blocklen  ||  dif < 0 )
                  return error4( r4->codeBase, e4index, E92102 ) ;
            #endif
            keyTo->num = r4->lastblock ;

            #ifdef S4BYTE_SWAP
               c4memcpy( (void *)swap, (void *)block, r4->blocklen ) ;
               /* position swapPtr at beginning of B4KEY's */
               swapPtr = swap ;
               swapPtr += 6 + sizeof(short) ;
               /* move through all B4KEY's to swap 'long' */
               for ( i = 0 ; i < (*(short *)swap) ; i++ )
               {
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += r4->grouplen ;
               }
               /* this is a branch */
               longVal = x4reverseLong( (void *)swapPtr ) ;
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;

               /* swap the numKeys value */
               shortVal = x4reverseShort( (void *)swap ) ;
               c4memcpy( swap, (void *) &shortVal, sizeof(short) ) ;

               rc = file4seqWrite( &r4->seqwrite, swap, r4->blocklen ) ;
               if ( rc < 0 )
               {
                  u4free( swap ) ;
                  return error4stack( r4->codeBase, rc, E92102 ) ;
               }
            #else
               rc = file4seqWrite( &r4->seqwrite, block, r4->blocklen ) ;
               if ( rc < 0 )
                  return error4stack( r4->codeBase, rc, E92102 ) ;
            #endif  /* S4BYTE_SWAP */

            r4->lastblock += r4->lastblockInc ;
         }
      }
      #ifdef S4BYTE_SWAP
         u4free( swap ) ;
      #endif  /* S4BYTE_SWAP */
      return 0 ;
   }
#endif /* #if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) && defined( S4MDX ) */



#if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) && defined( S4FOX )
   static int r4reindexToDisk( R4REINDEX *r4, const char *keyValue )
   {
      /* Writes out the current block and adds references to higher blocks */
      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      R4BLOCK_DATA *block = r4->startBlock ;

      S4LONG lRecno ;
      c4memcpy( (void *)&lRecno, ((unsigned char *) (&block->header)) + sizeof(B4STD_HEADER)
              + sizeof(B4NODE_HEADER) + (block->header.nKeys - 1) * r4->nodeHdr.infoLen, sizeof(S4LONG ) ) ;

      #ifdef S4DO_BYTEORDER
         lRecno = x4reverseLong( (void *)&lRecno ) ;
      #endif

      #ifdef S4DATA_ALIGN
         S4LONG longTemp ;
         c4memcpy( (void *)&longTemp, (void *)&r4->nodeHdr.recNumMask[0], sizeof(S4LONG) ) ;
         lRecno &= longTemp ;
      #else
         lRecno &= *(S4LONG *)&r4->nodeHdr.recNumMask[0] ;
      #endif

      lRecno = x4reverseLong( (void *)&lRecno ) ;

      #ifdef E4ANALYZE
         int iBlock = 0 ;
      #endif

      for( int tnUsed = 2 ;; tnUsed++ )
      {
         b4nodeAddBlocks( &r4->lastblock, r4->indexFile, 1 ) ;
         /* next line only works when on leaf branches... */
         b4nodeAssignNode( &block->header.rightNode, r4->lastblock ) ;
         b4nodeAddBlocks( &block->header.rightNode, r4->indexFile, 1 ) ;
         assert5( b4node( block->header.rightNode ) != 0 ) ;  // 0 always invalid...
         // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
         if ( block->header.nodeAttribute & 0x02 )  /* if leaf, record freeSpace */
            c4memcpy( ((char *)block) + sizeof( B4STD_HEADER ), (void *)&r4->nodeHdr.freeSpace, sizeof( r4->nodeHdr.freeSpace ) ) ;

         #ifdef S4BYTE_SWAP
            int i ;
            S4LONG longVal ;
            short shortVal ;

            char *swap = (char *)u4alloc( i4blockSize( r4->indexFile ) ) ;
            c4memcpy( (void *)swap, (void *)block, i4blockSize( r4->indexFile ) ) ;

            /* position at either B4NODE_HEADER (leaf) or data (branch) */
            char *swapPtr = swap + 2 * sizeof( short) + 2 * sizeof(S4LONG) ;

            /* if block is a leaf */
            // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
            if (r4->startBlock->header.nodeAttribute & 0x02 )
            {
               /* swap B4NODE_HEADER members */
               shortVal = x4reverseShort( (void *)swapPtr ) ; /* freeSpace */
               c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
               swapPtr += sizeof(short) ;

               longVal = x4reverseLong( (void *)swapPtr ) ;   /* recNumMask */
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            }
            else /* if block is a branch */
            {
               shortVal = r4->tag->header.keyLen + sizeof(S4LONG) ;

               /* position swapPtr to end of first key expression */
               swapPtr += r4->tag->header.keyLen ;

               /* move through all B4KEY's to swap 'long's */
               for ( i = 0 ; i < (int) block->header.nKeys ; i++ )
               {
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += sizeof(S4LONG) ;
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += shortVal ;
               }
            }

            /* reposition to B4STD_HEADER and swap members */
            swapPtr = swap ;

            shortVal = x4reverseShort( (void *)swapPtr ) ; /* nodeAttribute */
            c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
            swapPtr += sizeof(short) ;

            shortVal = x4reverseShort( (void *)swapPtr ) ; /* nKeys */
            c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
            swapPtr += sizeof(short) ;

            longVal = x4reverseLong( (void *)swapPtr ) ;   /* leftNode */
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += sizeof(S4LONG) ;

            longVal = x4reverseLong( (void *)swapPtr ) ;   /* rightNode */
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += sizeof(S4LONG) ;

            int rc = file4seqWrite( &r4->seqwrite, swap, i4blockSize( r4->indexFile ) ) ;
            u4free( swap ) ;
         #else
            int rc = file4seqWrite( &r4->seqwrite, block, i4blockSize( r4->indexFile ) ) ;
         #endif
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         c4memset( (void *)block, 0, i4blockSize( r4->indexFile ) ) ;
         b4nodeAssignNode( &block->header.leftNode, r4->lastblock ) ;
         assert5( b4node( block->header.leftNode ) != 0 ) ;  // 0 always invalid...
         b4nodeSetInvalid( &block->header.rightNode ) ;

         block = (R4BLOCK_DATA *) ((char *)block + i4blockSize( r4->indexFile ) ) ;
         #ifdef E4ANALYZE
            iBlock++ ;
            if ( iBlock >= r4->nBlocks )
               return error4( r4->codeBase, e4info, E92102 ) ;
         #endif  /* E4ANALYZE */

         if ( block->header.nKeys < r4->keysmax )
         {
            char *keyTo = ((char *) (&block->header)) + sizeof(B4STD_HEADER) + block->header.nKeys * r4->grouplen ;
            block->header.nKeys++ ;
            #ifdef E4ANALYZE
               if ( (char *) keyTo -  (char *) block + r4->grouplen > i4blockSize( r4->indexFile ) || (char *) keyTo -  (char *) block < 0 )
                  return error4( r4->codeBase, e4result, E92102 ) ;
            #endif  /* E4ANALYZE */
            c4memcpy( keyTo, (void *)keyValue, (unsigned int)r4->valuelen ) ;
            keyTo += r4->valuelen ;
            c4memcpy( keyTo, (void *)&lRecno, sizeof(S4LONG ) ) ;
            S4UNSIGNED_LONG revLb = x4reverseLong( (void *)&r4->lastblock ) ;
            c4memcpy( keyTo + sizeof(S4LONG ), (void *)&revLb, sizeof(S4LONG ) ) ;

            if ( block->header.nKeys < r4->keysmax )  /* then done, else do next one up */
            {
               if ( tnUsed > r4->nBlocksUsed )
                  r4->nBlocksUsed = tnUsed ;
               return 0 ;
            }
         }
         #ifdef E4ANALYZE
            else  /* should never occur */
               return error4( r4->codeBase, e4result, E92102 ) ;
         #endif  /* E4ANALYZE */
      }
   }



   int r4reindexTagHeadersWrite( R4REINDEX *r4 )
   {
      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      INDEX4FILE *i4file = r4->indexFile ;

      /* Now write the headers  */
      FILE4LONG pos ;
      /* LY July 7/03 : changed from 0 to 0L */
      file4longAssign( pos, 0, 0L ) ;
      file4seqWriteInitLow( &r4->seqwrite, &i4file->file, pos, r4->buffer, r4->bufferLen ) ;

      b4nodeAssignLong( &i4file->tagIndex->header.freeList, 0L ) ;
      unsigned int exprHdrLen = 5 * sizeof(short) ;
      b4nodeAssignNode( &i4file->eof, r4->lastblock ) ;
      b4nodeAddBlocks( &i4file->eof, r4->indexFile, 1 ) ;
      // i4file->eof = b4node( eofNode ) ;

      if ( i4file->tagIndex->header.typeCode >= 64 )
      {
         #ifdef S4BYTE_SWAP
            T4HEADER swapTagHeader ;
            c4memcpy( (void *)&swapTagHeader, (void *)&i4file->tagIndex->header, sizeof(T4HEADER) ) ;

            swapTagHeader.root.node = x4reverseLong( (void *)&swapTagHeader.root ) ;
            swapTagHeader.freeList.node = x4reverseLong( (void *)&swapTagHeader.freeList ) ;
            /* version is written in non-intel order */
            swapTagHeader.keyLen = x4reverseShort( (void *)&swapTagHeader.keyLen ) ;
            swapTagHeader.descending = x4reverseShort( (void *)&swapTagHeader.descending ) ;
            swapTagHeader.filterPos = x4reverseShort( (void *)&swapTagHeader.filterPos ) ;
            swapTagHeader.filterLen = x4reverseShort( (void *)&swapTagHeader.filterLen ) ;
            swapTagHeader.exprPos = x4reverseShort( (void *)&swapTagHeader.exprPos ) ;
            swapTagHeader.exprLen = x4reverseShort( (void *)&swapTagHeader.exprLen ) ;

            int rc = file4seqWrite( &r4->seqwrite, &swapTagHeader, LEN4HEADER_WR_TAG_INDEX ) ;  /* write first header part */
         #else
            i4file->tagIndex->header.version = x4reverseLong( (void *)&i4file->tagIndex->header.version ) ;
            int rc = file4seqWrite( &r4->seqwrite, &i4file->tagIndex->header, LEN4HEADER_WR_TAG_INDEX ) ;  /* write first header part */
            i4file->tagIndex->header.version = x4reverseLong( (void *)&i4file->tagIndex->header.version ) ;
         #endif
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         // AS 06/30/99 -- updated to include blocksize, multiplier...
         // rc = file4seqWriteRepeat( &r4->seqwrite, 478L, 0 ) ;
         rc = file4seqWriteRepeat( &r4->seqwrite, 466L, 0 ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         rc = file4seqWrite( &r4->seqwrite, &i4file->tagIndex->header.sortSeq, 8 ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         #ifdef S4BYTE_SWAP
            rc = file4seqWrite( &r4->seqwrite, &swapTagHeader.descending, exprHdrLen ) ;
         #else
            rc = file4seqWrite( &r4->seqwrite, &i4file->tagIndex->header.descending, exprHdrLen ) ;
         #endif
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         rc = file4seqWriteRepeat( &r4->seqwrite, 512L, 0 ) ;  /* no expression */
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
      }

      int iTag = 2 ;
      for ( TAG4FILE *tagOn = 0 ;; )
      {
         tagOn = (TAG4FILE *)l4next( &i4file->tags, tagOn ) ;
         if ( tagOn == 0 )
            break ;
         FILE4LONG fileHeaderPos ;
         // multiply result always < max long, so can assign directly
         // need 1024 or iTag * B4BLOCK_SIZE_INTERNAL to hold header info...
         if ( i4file->tagIndex->header.typeCode >= 64 )
            /* LY July 7/03 : changed from 0 to 0L */
            file4longAssign( fileHeaderPos, ((S4UNSIGNED_LONG)iTag) * (B4BLOCK_SIZE_INTERNAL), 0L ) ;
         else
            /* LY July 7/03 : changed from 0 to 0L */
            file4longAssign( fileHeaderPos, 0, 0L ) ;

         b4nodeSetFromFilePosition( i4file, &tagOn->headerOffset, fileHeaderPos ) ;

         #ifdef S4BYTE_SWAP
            T4HEADER swapTagHeader ;
            c4memcpy( (void *)&swapTagHeader, (void *)&tagOn->header, sizeof(T4HEADER) ) ;

            swapTagHeader.root.node = x4reverseLong( (void *)&swapTagHeader.root ) ;
            swapTagHeader.freeList.node = x4reverseLong( (void *)&swapTagHeader.freeList ) ;
            /* version is in non-intel ordering */
            swapTagHeader.keyLen = x4reverseShort( (void *)&swapTagHeader.keyLen ) ;
            swapTagHeader.descending = x4reverseShort( (void *)&swapTagHeader.descending ) ;
            swapTagHeader.filterPos = x4reverseShort( (void *)&swapTagHeader.filterPos ) ;
            swapTagHeader.filterLen = x4reverseShort( (void *)&swapTagHeader.filterLen ) ;
            swapTagHeader.exprPos = x4reverseShort( (void *)&swapTagHeader.exprPos ) ;
            swapTagHeader.exprLen = x4reverseShort( (void *)&swapTagHeader.exprLen ) ;

            int rc = file4seqWrite( &r4->seqwrite, &swapTagHeader, LEN4HEADER_WR ) ;
         #else
            tagOn->header.version = x4reverseLong( (void *)&tagOn->header.version ) ;
            int rc = file4seqWrite( &r4->seqwrite, &tagOn->header, LEN4HEADER_WR ) ;
            tagOn->header.version = x4reverseLong( (void *)&tagOn->header.version ) ;
         #endif
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         rc = file4seqWriteRepeat( &r4->seqwrite, 478L, 0 ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         rc = file4seqWrite( &r4->seqwrite, &tagOn->header.sortSeq, 8 ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         #ifdef S4BYTE_SWAP
            rc = file4seqWrite( &r4->seqwrite, &swapTagHeader.descending, exprHdrLen ) ;
         #else
            rc = file4seqWrite( &r4->seqwrite, &tagOn->header.descending, exprHdrLen ) ;
         #endif
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         const char *ptr = tagOn->expr->source ;
         int totLen = tagOn->header.exprLen ;
         rc = file4seqWrite( &r4->seqwrite, ptr, (unsigned int)tagOn->header.exprLen ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;

         if ( tagOn->filter != 0 )
         {
            ptr = tagOn->filter->source ;
            file4seqWrite( &r4->seqwrite, ptr, (unsigned int)tagOn->header.filterLen ) ;
            totLen += tagOn->header.filterLen ;
         }
         rc = file4seqWriteRepeat( &r4->seqwrite, (S4LONG)(B4BLOCK_SIZE_INTERNAL - totLen), 0 );
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         iTag += 2 ;
      }

      b4nodeGetFilePosition( i4file, i4file->eof, &pos ) ;

      int rc = file4lenSetLow( &i4file->file, pos ) ;
      if ( rc < 0 )
         return error4stack( r4->codeBase, (short)rc, E92102 ) ;

      rc = file4seqWriteFlush( &r4->seqwrite ) ;
      if ( rc < 0 )
         return error4stack( r4->codeBase, rc, E92102 ) ;

      return 0 ;
   }



   int r4reindexWriteKeys( R4REINDEX *r4, short int errUnique )
   {
      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      TAG4FILE *t4 = r4->tag ;
      unsigned short int kLen = (unsigned short int)t4->header.keyLen ;

      #ifdef E4MISC
         if ( I4MAX_KEY_SIZE_COMPATIBLE < r4->sort.sortLen )
         {
            // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE
            // - we may not know if we are creating or reindexing...but we can check the index block size.  If it mismatches the CODE4 setting (large indexes) we know we are not creating.
            if ( r4->codeBase->limitKeySize == 1 && ( i4blockSize( t4->indexFile ) == c4indexBlockSizeGet( r4->codeBase ) ) )
            {
               if ( r4->codeBase->oledbSchemaCreate == 1 )  // ensure via real max length (not-compatible)
               {
                  if ( I4MAX_KEY_SIZE < r4->sort.sortLen )
                     return error4( r4->codeBase, e4index, E82102 ) ;
               }
               else
                  return error4( r4->codeBase, e4index, E82102 ) ;
            }
         }
      #endif  /* E4MISC */

      // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - use the internal CODE4 field buffer instead...
      // char lastKey[I4MAX_KEY_SIZE] ;
      if ( r4->codeBase->bufLen < (unsigned)r4->sort.sortLen)
      {
         if ( u4allocAgain( r4->codeBase, &r4->codeBase->fieldBuffer, &r4->codeBase->bufLen, r4->sort.sortLen ) < 0 )
            return e4memory ;
      }
      char *lastKey = r4->codeBase->fieldBuffer ;

      c4memset( lastKey, r4->tag->pChar, kLen ) ;

      int cLen ;
      for ( cLen = 0 ; kLen ; kLen >>= 1, cLen++ ) ;

      kLen = (unsigned short int)t4->header.keyLen ;  /* reset the key length */
      r4->nodeHdr.trailCntLen = r4->nodeHdr.dupCntLen = (unsigned char)cLen ;

      // AS Jun 8/04 - for large key support, using a different trail byte counter
      // r4->trailByteCnt = (unsigned char)(0xFF >> ( 8 - ((cLen / 8) * 8 + cLen % 8))) ;
      while ( cLen > 8 )  // trim down so we just have the remainder - eg. if == 11, we trim down to 3 (the other 8 is a given based on the key length)
         cLen -= 8 ;
      r4->nodeHdr.trailByteCnt = (unsigned char)(0xFF >> ( 8 - ((cLen / 8) * 8 + cLen % 8))) ;
      // r4->dupByteCnt = r4->nodeHdr.trailByteCnt ;
      r4->nodeHdr.dupByteCnt = r4->nodeHdr.trailByteCnt ;

      S4LONG recCount ;
      if ( t4 == r4->indexFile->tagIndex ) /* the tag of tags, don't use record count, instead base on 1024*numTags, which is true value */
          recCount = r4->nTags * 1024 ;
      else
      {
         recCount = dfile4recCount( r4->dataFile, -2L ) ;
         if ( recCount < 0 )
            return error4stack( r4->codeBase, (short)recCount, E92102 ) ;
      }
      S4UNSIGNED_LONG rLen = (unsigned S4LONG)recCount ;

      for ( cLen = 0 ; rLen ; rLen>>=1, cLen++ ) ;
      r4->nodeHdr.recNumLen = (unsigned char) cLen ;
      if ( r4->nodeHdr.recNumLen < 12 )
         r4->nodeHdr.recNumLen = 12 ;

      int tLen ;
      for( tLen = r4->nodeHdr.recNumLen + r4->nodeHdr.trailCntLen + r4->nodeHdr.dupCntLen ;
           (tLen / 8)*8 != tLen ; tLen++, r4->nodeHdr.recNumLen++ ) ;  /* make at an 8-bit offset */

      r4->nodeHdr.infoLen = (unsigned char)((unsigned int)(r4->nodeHdr.recNumLen + r4->nodeHdr.trailCntLen + r4->nodeHdr.dupCntLen) / 8) ;

      // AS 06/19/00 also don't allow recNumLen to be > 32, since we only support that many records, and the mask cannot handle
      // larger values.  Was failing on approx. 70 million records
      if ( r4->nodeHdr.recNumLen > 32 )
         r4->nodeHdr.recNumLen = 32 ;

      S4UNSIGNED_LONG ff = 0xFFFFFFFFL ;
      rLen = ff >> ( sizeof(S4LONG)*8 - r4->nodeHdr.recNumLen ) ;
      c4memcpy( (void *)&r4->nodeHdr.recNumMask[0], (void *)&rLen, sizeof(S4LONG) ) ;

      r4->valuelen = t4->header.keyLen ;
      r4->grouplen = t4->header.keyLen + 2*sizeof(S4LONG) ;

      c4memset( (void *)r4->startBlock, 0, (unsigned int)r4->nBlocks * i4blockSize( r4->indexFile ) ) ;

      int onCount ;
      R4BLOCK_DATA *r4block ;
      for ( r4block = r4->startBlock, onCount = 0 ; onCount < r4->nBlocks;
            r4block = (R4BLOCK_DATA *) ( (char *)r4block + i4blockSize( r4->indexFile )), onCount++ )
      {
         c4memset( (void *)r4block, 0, i4blockSize( r4->indexFile ) ) ;
         b4nodeSetInvalid( &r4block->header.leftNode ) ;
         b4nodeSetInvalid( &r4block->header.rightNode ) ;
      }

      r4->nodeHdr.freeSpace = (short)i4blockSize( r4->indexFile ) - sizeof( B4STD_HEADER ) - sizeof( B4NODE_HEADER ) ;

      r4->keysmax = (i4blockSize( r4->indexFile ) - sizeof( B4STD_HEADER ) ) / (unsigned)r4->grouplen ;

      #ifdef E4ANALYZE
         if ( r4->nodeHdr.freeSpace <= 0 || r4->keysmax <= 0 )
            return error4( r4->codeBase, e4index, E92102 ) ;
      #endif
      int rc = sort4getInit( &r4->sort ) ;
      if ( rc < 0 )
         return error4stack( r4->codeBase, (short)rc, E92102 ) ;

      B4NODE filePosNode ;
      b4nodeAssignNode( &filePosNode, r4->lastblock ) ;
      b4nodeAddBlocks( &filePosNode, r4->indexFile, 1 ) ;
      FILE4LONG pos ;
      b4nodeGetFilePosition( r4->indexFile, filePosNode, &pos ) ;
      file4seqWriteInitLow( &r4->seqwrite, &r4->indexFile->file, pos, r4->buffer, r4->bufferLen ) ;

      int lastTrail = kLen ;   /* default is no available duplicates */
      // AS Apr 7/03 - r4candidate/e4candidate sets typeCode to 0x04, this is also a unique situation
      int isUnique = t4->header.typeCode & 0x05 ;
      int isFirst = 1 ;

      // AS Mar 26/04 - improved return code handling
      int rcSave = 0 ;

      for( ;; )  /* For each key to write */
      {
         #if defined( TIME4STATUS ) && !defined( S4OFF_THREAD )
            InterlockedIncrement( &(r4->codeBase->incrementVal) ) ;
         #endif
         unsigned char *keyData ;
         void *dummyPtr ;
         S4LONG keyRec ;
         rc = sort4get( &r4->sort, &keyRec, (void **) &keyData, &dummyPtr ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         #ifdef E4MISC
            if ( r4->keyCount < 0L || r4->keyCount == 0L && rc != r4done || r4->keyCount > 0L && rc == r4done )
               return error4( r4->codeBase, e4info, E92102 ) ;
            r4->keyCount-- ;
         #endif  /* E4MISC */

         if ( rc == r4done )  /* No more keys */
         {
            rc = r4reindexFinish( r4, lastKey ) ;
            if ( rc < 0 )
               return error4stack( r4->codeBase, (short)rc, E92102 ) ;
            rc = file4seqWriteFlush( &r4->seqwrite ) ;
            if ( rc < 0 )
               return error4stack( r4->codeBase, (short)rc, E92102 ) ;
            break ;
         }

         if ( isUnique )
         {
            if ( isFirst )
               isFirst = 0 ;
            else
            {
               if ( u4memcmp( keyData, lastKey, r4->sort.sortLen) == 0 )
               {
                  switch( errUnique )
                  {
                     case e4unique:
                     case e4candidate:
                        return error4describe( r4->codeBase, e4unique, E82103, t4->alias, (char *)0, (char *)0 ) ;
                     case r4unique:
                     case r4candidate:
                        return r4unique ;
                     // AS Mar 26/04 - need to ensure r4uniqueContinue eventually returned in this case
                     case r4uniqueContinue:
                        rcSave = r4uniqueContinue ;
                     default:
                        continue ;
                  }
               }
            }
         }

         /* Add the key */
         rc = r4reindexAdd( r4, keyRec, keyData, lastKey, &lastTrail ) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         c4memcpy( lastKey, keyData, r4->sort.sortLen ) ;
      }

      /* Now complete the tag header info. */
      b4nodeAssignNode( &t4->header.root, r4->lastblock ) ;
      return rcSave ;
   }



   int r4reindexAdd( R4REINDEX *r4, const S4LONG rec, const unsigned char *keyValue, const char *lastKey, int *lastTrail )
   {
      /*
         DESCRIPTION

         This function is used to add an entry into the index.  It is only used to insert
         keys into leaf blocks, not branch blocks.

         PARAMATERS

         r4 is the reindex structure

         rec is the record number of the record from which the key was generated

         keyValue is the key to add to the index
      */
      #ifdef E4PARM_LOW
         if ( r4 == 0 || rec < 0 || keyValue == 0 )
            return error4( 0, e4parm, E92102 ) ;
      #endif

      R4BLOCK_DATA *startBlock = r4->startBlock ;
      int kLen = r4->valuelen ;
      int iLen = r4->nodeHdr.infoLen ;

      int dupCnt ;

      if ( startBlock->header.nKeys == 0 )   /* reset */
      {
         dupCnt = 0 ;
         r4->curPos = ((char *)startBlock) + i4blockSize( r4->indexFile ) ;
         c4memcpy( ((char *)startBlock) + sizeof( B4STD_HEADER ), (void *)&r4->nodeHdr, sizeof( B4NODE_HEADER ) ) ;
         startBlock->header.nodeAttribute |= 2 ;   /* leaf block */
         *lastTrail = kLen ;
      }
      else
         dupCnt = b4calcDups( keyValue,(const unsigned char*)lastKey, kLen ) ;

      if ( dupCnt > kLen - *lastTrail )  /* don't allow duplicating trail bytes */
         dupCnt = kLen - *lastTrail ;

      int trail ;

      if ( dupCnt == kLen ) /* duplicate key */
         trail = 0 ;
      else
      {
         assert5( r4->data->dataFile->compatibility == 30 || r4->data->dataFile->compatibility == 25 || r4->data->dataFile->compatibility == 26 ) ;
         if ( r4->data->dataFile->compatibility == 26 && r4->tag->filter != 0 )
            trail = 0 ;
         else
            trail = b4calcBlanks( keyValue, kLen, r4->tag->pChar ) ;
      }

      *lastTrail = trail ;

      if ( dupCnt > kLen - *lastTrail )  /* watch for case where < ' ' exissts */
         dupCnt = kLen - *lastTrail ;

      int len = kLen - dupCnt - trail ;
      if ( r4->nodeHdr.freeSpace < iLen + len )
      {
         int rc = r4reindexToDisk(r4, lastKey) ;
         if ( rc < 0 )
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         r4->nodeHdr.freeSpace = (short)i4blockSize( r4->indexFile ) - sizeof( B4STD_HEADER ) - sizeof( B4NODE_HEADER ) ;
         dupCnt = 0 ;
         r4->curPos = ((char *)startBlock) + i4blockSize( r4->indexFile ) ;
         c4memcpy( ((char *)&startBlock->header) + sizeof( B4STD_HEADER ), (void *)&r4->nodeHdr, sizeof( B4NODE_HEADER ) ) ;
         startBlock->header.nodeAttribute |= 2 ;   /* leaf block */
         assert5( r4->data->dataFile->compatibility == 30 || r4->data->dataFile->compatibility == 25 || r4->data->dataFile->compatibility == 26 ) ;
         if ( r4->data->dataFile->compatibility == 26 && r4->tag->filter != 0 )
            trail = 0 ;
         else
            trail = b4calcBlanks( keyValue, kLen, r4->tag->pChar ) ;
         len = kLen - trail ;
      }

      r4->curPos -= len ;
      assert5( r4->curPos >= (char *)r4->startBlock ) ;  // ensure we don't overwrite the block...
      c4memcpy( r4->curPos, keyValue + dupCnt, (unsigned int)len ) ;
      char *infoPos = (char *)(&startBlock->header) + sizeof(B4STD_HEADER) + sizeof(B4NODE_HEADER) + startBlock->header.nKeys * iLen ;
      unsigned char buffer[6] ;
      x4putInfo( &r4->nodeHdr, buffer, rec, trail, dupCnt ) ;
      c4memcpy( infoPos, (void *)buffer, (unsigned int)iLen ) ;

      r4->nodeHdr.freeSpace -= (short) ( len + iLen ) ;  // AS Aug 24/04 - changed freeSpace to not be unsigned char (large key support)
      startBlock->header.nKeys++ ;
      return 0 ;
   }



   int r4reindexFinish( R4REINDEX *r4, char *keyValue )
   {
      #ifdef E4PARM_LOW
         if ( r4 == 0 )
            return error4( 0, e4parm_null, E92102 ) ;
      #endif

      R4BLOCK_DATA *block = r4->startBlock ;
      int rc ;

      if ( r4->nBlocksUsed <= 1 ) /* just output first block */
      {
         c4memcpy( ((char *)block) + sizeof( B4STD_HEADER ), (void *)&r4->nodeHdr, sizeof( B4NODE_HEADER ) ) ;

         block->header.nodeAttribute |= (short)3 ;   /* leaf and root block */

         #ifdef S4BYTE_SWAP
            char *swap = (char *)u4alloc(i4blockSize( r4->indexFile )) ;
            char *swapPtr ;
            int i ;
            S4LONG longVal ;
            short shortVal ;

            c4memcpy( (void *)swap, (void *)block, i4blockSize( r4->indexFile ) ) ;

            /* position at either B4NODE_HEADER (leaf) or data (branch) */
            swapPtr = swap + 2 * sizeof( short) + 2 * sizeof(S4LONG) ;

            // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
            if (block->header.nodeAttribute & 0x02 ) /* if block is a leaf */
            {
               /* swap B4NODE_HEADER members */
               shortVal = x4reverseShort( (void *)swapPtr ) ; /* freeSpace */
               c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
               swapPtr += sizeof(short) ;

               longVal = x4reverseLong( (void *)swapPtr ) ;   /* recNumMask */
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            }
            else /* if block is a branch */
            {
               shortVal = r4->tag->header.keyLen + sizeof(S4LONG) ;

               /* position swapPtr to end of first key expression */
               swapPtr += r4->tag->header.keyLen ;

               /* move through all B4KEY's to swap 'long's */
               for ( i = 0 ; i < (int)block->header.nKeys ; i++ )
               {
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += sizeof(S4LONG) ;
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += shortVal ;
               }
            }

            /* reposition to B4STD_HEADER and swap members */
            swapPtr = swap ;

            shortVal = x4reverseShort( (void *)swapPtr ) ; /* nodeAttribute */
            c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
            swapPtr += sizeof(short) ;

            shortVal = x4reverseShort( (void *)swapPtr ) ; /* nKeys */
            c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
            swapPtr += sizeof(short) ;

            longVal = x4reverseLong( (void *)swapPtr ) ;   /* leftNode */
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += sizeof(S4LONG) ;

            longVal = x4reverseLong( (void *)swapPtr ) ;   /* rightNode */
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += sizeof(S4LONG) ;

            /* LY 00/01/26 : cast return of i4blockSize() for Mac */
            rc = file4seqWrite( &r4->seqwrite, swap, (const unsigned int)i4blockSize( r4->indexFile ) ) ;
         #else
            #ifdef E4DEBUG
               /* should be -1 to indicate none, or a valid node value... */
               if ( b4node( block->header.leftNode ) == 0 || b4node( block->header.rightNode ) == 0 )
                  return error4( r4->codeBase, e4struct, E92102 ) ;
            #endif
            rc = file4seqWrite( &r4->seqwrite, block, i4blockSize( r4->indexFile ) ) ;
         #endif
         if ( rc < 0 )
         {
            #ifdef S4BYTE_SWAP
                u4free( swap ) ;  /* LY 00/05/08 : moved from line 1982 */
            #endif
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         }
         b4nodeAddBlocks( &r4->lastblock, r4->indexFile, 1 ) ;
         #ifdef S4BYTE_SWAP
             u4free( swap ) ;  /* LY 00/05/08 : moved from line 1982 */
         #endif
      }
      else
      {
         S4LONG lRecno ;
         c4memcpy( (void *)&lRecno, (void *) (((char *)(&block->header)) + sizeof(B4STD_HEADER)
                 + sizeof(B4NODE_HEADER) + (block->header.nKeys - 1) * r4->nodeHdr.infoLen), sizeof(S4LONG ) ) ;
         #ifdef S4DO_BYTEORDER
            lRecno = x4reverseLong( (void *)&lRecno ) ;
         #endif
         #ifdef S4DATA_ALIGN
            S4LONG longTemp ;
            c4memcpy( (void *)&longTemp, (void *)&r4->nodeHdr.recNumMask[0], sizeof(S4LONG) ) ;
            lRecno &= longTemp ;
         #else
            lRecno &= *(S4LONG *)&r4->nodeHdr.recNumMask[0] ;
         #endif

         // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
         if ( block->header.nodeAttribute & 0x02 )  /* if leaf, record freeSpace */
            c4memcpy( ((char *)block) + sizeof( B4STD_HEADER ), (void *)&r4->nodeHdr, sizeof( B4NODE_HEADER ) ) ;

         #ifdef S4BYTE_SWAP
            char *swap = (char *)u4alloc( i4blockSize( r4->indexFile ) ) ;
            char *swapPtr ;
            int i ;
            S4LONG longVal ;
            short shortVal ;
            c4memcpy( (void *)swap, (void *)r4->startBlock, i4blockSize( r4->indexFile ) ) ;

            /* position at either B4NODE_HEADER (leaf) or data (branch) */
            swapPtr = swap + 2 * sizeof( short) + 2 * sizeof(S4LONG) ;

            // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
            if (block->header.nodeAttribute & 0x02 ) /* if block is a leaf */
            {
               /* swap B4NODE_HEADER members */
               shortVal = x4reverseShort( (void *)swapPtr ) ; /* freeSpace */
               c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
               swapPtr += sizeof(short) ;

               longVal = x4reverseLong( (void *)swapPtr ) ;   /* recNumMask */
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            }
            else /* if block is a branch */
            {
               shortVal = r4->tag->header.keyLen + sizeof(S4LONG) ;

               /* position swapPtr to end of first key expression */
               swapPtr += r4->tag->header.keyLen ;

               /* move through all B4KEY's to swap 'long's */
               for ( i = 0 ; i < (int) block->header.nKeys ; i++ )
               {
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += sizeof(S4LONG) ;
                  longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += shortVal ;
               }
            }

            /* reposition to B4STD_HEADER and swap members */
            swapPtr = swap ;

            shortVal = x4reverseShort( (void *)swapPtr ) ; /* nodeAttribute */
            c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
            swapPtr += sizeof(short) ;

            shortVal = x4reverseShort( (void *)swapPtr ) ; /* nKeys */
            c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
            swapPtr += sizeof(short) ;

            longVal = x4reverseLong( (void *)swapPtr ) ;   /* leftNode */
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += sizeof(S4LONG) ;

            longVal = x4reverseLong( (void *)swapPtr ) ;   /* rightNode */
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += sizeof(S4LONG) ;

            rc = file4seqWrite( &r4->seqwrite, swap, i4blockSize( r4->indexFile ) ) ;
         #else
            rc = file4seqWrite(&r4->seqwrite, r4->startBlock, i4blockSize( r4->indexFile ) ) ;
         #endif
         if ( rc < 0 )
         {
            #ifdef S4BYTE_SWAP
               u4free( swap ) ;  /* LY 00/05/08 : moved from line 2081 */
            #endif
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         }

         b4nodeAddBlocks( &r4->lastblock, r4->indexFile, 1 ) ;

         lRecno = x4reverseLong( (void *)&lRecno ) ;

         for( int iBlock = 1 ; iBlock < r4->nBlocksUsed ; iBlock++ )
         {
            block = (R4BLOCK_DATA *) ((char *)block + i4blockSize( r4->indexFile )) ;
            char *keyTo = ((char *) (&block->header)) + sizeof(B4STD_HEADER) + block->header.nKeys * r4->grouplen ;
            block->header.nKeys++ ;
            #ifdef E4MISC
               if ( (char *) keyTo -  (char *) block + r4->grouplen > i4blockSize( r4->indexFile ) ||
                    (char *) keyTo -  (char *) block < 0 )
               {
                  #ifdef S4BYTE_SWAP
                     u4free( swap ) ;  /* LY 00/05/08 : moved from line 2081 */
                  #endif
                  return error4( r4->codeBase, e4result, E92102 ) ;
               }
            #endif  /* E4MISC */
            c4memcpy( keyTo, (void *)keyValue, (unsigned int)r4->valuelen ) ;
            keyTo += r4->valuelen ;
            c4memcpy( keyTo, (void *)&lRecno, sizeof(S4LONG ) ) ;
            S4UNSIGNED_LONG revLb = x4reverseLong( (void *)&r4->lastblock ) ;
            c4memcpy( keyTo + sizeof(S4LONG ), (void *)&revLb, sizeof(S4LONG ) ) ;

            if ( iBlock == r4->nBlocksUsed - 1 )
               block->header.nodeAttribute = 1 ;  /* root block */

            #ifdef S4BYTE_SWAP
               c4memcpy( (void *)swap, (void *)block, i4blockSize( r4->indexFile ) ) ;

               /* position at either B4NODE_HEADER (leaf) or data (branch) */
               swapPtr = swap + 2 * sizeof( short) + 2 * sizeof(S4LONG) ;

               // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
               if (block->header.nodeAttribute & 0x02 ) /* if block is a leaf */
               {
                  /* swap B4NODE_HEADER members */
                  shortVal = x4reverseShort( (void *)swapPtr ) ; /* freeSpace */
                  c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
                  swapPtr += sizeof(short) ;

                  longVal = x4reverseLong( (void *)swapPtr ) ;   /* recNumMask */
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               }
               else /* if block is a branch */
               {
                  shortVal = r4->tag->header.keyLen + sizeof(S4LONG) ;

                  /* position swapPtr to end of first key expression */
                  swapPtr += r4->tag->header.keyLen ;

                  /* move through all B4KEY's to swap 'long's */
                  for ( i = 0 ; i < (int) block->header.nKeys ; i++ )
                  {
                     longVal = x4reverseLong( (void *)swapPtr ) ;
                     c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                     swapPtr += sizeof(S4LONG) ;
                     longVal = x4reverseLong( (void *)swapPtr ) ;
                     c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                     swapPtr += shortVal ;
                  }
               }

               /* reposition to B4STD_HEADER and swap members */
               swapPtr = swap ;

               shortVal = x4reverseShort( (void *)swapPtr ) ; /* nodeAttribute */
               c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
               swapPtr += sizeof(short) ;

               shortVal = x4reverseShort( (void *)swapPtr ) ; /* nKeys */
               c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
               swapPtr += sizeof(short) ;

               longVal = x4reverseLong( (void *)swapPtr ) ;   /* leftNode */
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               swapPtr += sizeof(S4LONG) ;

               longVal = x4reverseLong( (void *)swapPtr ) ;   /* rightNode */
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               swapPtr += sizeof(S4LONG) ;

               /* LY 00/01/26 : cast return of i4blockSize() for Mac */
               rc = file4seqWrite( &r4->seqwrite, swap, (const unsigned int)i4blockSize( r4->indexFile ) ) ;
            #else
               rc = file4seqWrite( &r4->seqwrite, block, i4blockSize( r4->indexFile ) )  ;
            #endif
            if ( rc < 0 )
            {
               #ifdef S4BYTE_SWAP
                  u4free( swap ) ;  /* LY 00/05/08 : moved from line 2081 */
               #endif
               return error4stack( r4->codeBase, (short)rc, E92102 ) ;
            }

            b4nodeAddBlocks( &r4->lastblock, r4->indexFile, 1 ) ;
         }
         #ifdef S4BYTE_SWAP
            u4free( swap ) ;  /* LY 00/05/08 : moved from lines 1982 and 2081 */
         #endif
      }

      return 0 ;
   }
#endif /* #if !defined(S4OFF_WRITE) && !defined(S4INDEX_OFF) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) && defined( S4FOX ) */
