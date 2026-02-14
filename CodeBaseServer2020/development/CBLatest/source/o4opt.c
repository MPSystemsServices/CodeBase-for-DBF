/* o4opt.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifndef S4OPTIMIZE_OFF

static int opt4blockAdd( OPT4BLOCK *, FILE4 *, unsigned, long, FILE4LONG ) ;

static void opt4fileCancelReadSpBuffer( FILE4 *file ) ;
static OPT4BLOCK *opt4fileChooseBlock( FILE4 * ) ;
/* puts block onto bottom of lru-list and assigns the optlist to the block */
#define opt4listLruBottomPlace( o4, b4 ) ( l4add( &((o4)->list), &((b4)->lruLink) ), ( (b4)->optList = (o4) ) )
static void opt4listLruBottomShift( OPT4BLOCK * ) ;
static void opt4fileReadSpBuffer( FILE4 *, const FILE4LONG, int, int ) ;
static void opt4timeReset( OPT4 *, int, int ) ;
/* LY 99/06/21 : changed const long to const unsigned long in opt4fileReadFile */
#ifdef S4ADVANCE_TEST
   S4EXPORT unsigned S4FUNCTION opt4fileReadFile( FILE4 S4PTR *, const FILE4LONG, char S4PTR * ) ;
#else
   static unsigned opt4fileReadFile( FILE4 *, const FILE4LONG, char * ) ;
#endif

/* moves the block from the current position to the lru position (l4last)
   of the lru-list it is currently on.  Also updates the accessCount */
static void opt4listLruBottomShift( OPT4BLOCK *block )
{
   OPT4 *opt ;
   LINK4 *link ;
   LIST4 *list ;

   opt = &block->file->codeBase->opt ;

   list = &block->optList->list ;
   link = (LINK4 *)&block->lruLink ;
   if ( link != list->lastNode )
   {
      if ( list->selected == link )
         list->selected = (LINK4 *)l4prev( list, link ) ;
      l4remove( list, link ) ;
      l4add( list, link ) ;
   }

   if ( (unsigned int)(opt->accessTimeCount - block->accessTime) > opt->minAccessTimeVariation )
      block->accessTime = opt->accessTimeCount++ ;
   if ( opt->accessTimeCount == 0 )  /* time count got reset, so reset for everyone */
      opt4timeReset( opt, 0, 1 ) ;

   block->hitCount += block->file->hitCountAdd ;
}



// AS Apr 13/04 - support for optimizing large files
static int opt4blockAdd( OPT4BLOCK *block, FILE4 *file, unsigned blockLen, long hashVal, FILE4LONG position )
{
   #ifdef E4PARM_LOW
      if ( file == 0 || block == 0 || hashVal < 0 )
         return error4( 0, e4parm, E92508 ) ;
      if ( (unsigned long)hashVal >= file->codeBase->opt.numLists )
         return error4( 0, e4parm, E92508 ) ;
   #endif
   #ifdef E4ANALYZE
      if ( block->changed != 0 )
         return error4( file->codeBase, e4struct, E92508 ) ;
   #endif

   l4add( &file->codeBase->opt.lists[hashVal], block ) ;
   block->len = blockLen ;
   // AS Apr 13/04 - support for optimizing large files
   file4longAssignLong( block->pos, position ) ;
   block->file = file ;

   #ifdef E4ANALYZE_ALL
      /* AS 02/24/97 - it is possible for this to be ok, eg. writing tags to disk, later
         tags blocks written before earlier ones.
      if ( file->len != -1 )  // ensure we are not beyond file length - impossible...
         if ( (long)(block->pos + block->len) > file->len )
            error4( file->codeBase, e4opt, E92508 ) ;
      */
   #endif

   return 0 ;
}

int opt4blockClear( OPT4BLOCK *block )
{
   #ifdef E4PARM_LOW
      if ( block == 0 )
         return error4( 0, e4parm_null, E92508 ) ;
   #endif

   block->changed = 0 ;
   block->len = 0 ;
   file4longAssign( block->pos, 0, 0 ) ;
   block->file = 0 ;

   return 0 ;
}

#ifdef S4WRITE_DELAY
void S4CALL opt4writeCompletionRoutine( void *inDelay )
{
   FILE4WRITE_DELAY *delay ;

   delay = (FILE4WRITE_DELAY *)inDelay ;
   critical4sectionEnter( &delay->file->codeBase->opt.critical4optWrite ) ;
   l4add( &delay->file->codeBase->opt.delayAvail, (LINK4 *)delay->completionData ) ;
   critical4sectionLeave( &delay->file->codeBase->opt.critical4optWrite ) ;
}
#endif

#ifndef _MSC_VER
   #ifdef P4ARGS_USED
   #pragma argsused
#endif
#endif
static int opt4blockFlush( OPT4 *opt, OPT4BLOCK *block, char buffer, int doDelay )
{
   int rc, flushBuffer ;
   #ifdef S4WRITE_DELAY
      OPT4BLOCK *delayBlock ;
      LINK4 *delayLink ;
   #endif
   #ifdef __SC__   /* compiler bug - cpp */
      unsigned long long_val ;
   #endif

   #ifdef E4PARM_LOW
      if ( block == 0 )
         return error4( 0, e4parm_null, E92508 ) ;
   #endif

   #ifdef E4ANALYZE_ALL
      if ( block->file->hasDup == 1 )
      {
         assert5( file4longGetHi( block->pos ) == 0 ) ;  // don't support for large files
         if ( file4cmpPart( block->file->codeBase, block->data, block->file, file4longGetLo( block->pos ), block->len ) != 0 )
            return error4( block->file->codeBase, e4opt, E82503 ) ;
      }
   #endif

   if ( opt->readFile == block->file )  /* check to see if block has been temporarily read-buffered */
   {
      // AS Apr 13/04 - support for optimizing large files
      FILE4LONG readEnd ;
      file4longAssignLong( readEnd, opt->readStartPos ) ;
      file4longAdd( &readEnd, opt->bufferSize ) ;
      if ( file4longLessLong( block->pos, readEnd ) && file4longGreaterEqLong( block->pos, opt->readStartPos ) )
      {
         FILE4LONG posOffset ;
         file4longAssignLong( posOffset, block->pos ) ;
         file4longSubtractLong( &posOffset, &opt->readStartPos ) ;
         assert5( file4longGetHi( posOffset ) == 0 ) ;
         memcpy( opt->readBuffer + file4longGetLo( posOffset ), block->data, block->len ) ;
      }
   }

   if ( buffer && block->file->type == OPT4DBF )
   {
      if ( opt->writeFile != block->file )
         flushBuffer = 1 ;
      else
      {
         if ( opt->writeBlockCount == opt->maxBlocks )
            flushBuffer = 1 ;
         else
         {
            if ( !file4longEqualLong( opt->writeCurPos, block->pos ) )   /* not a consecutive write, so check out possibilities */
            {
               if ( file4longLessLong( opt->writeCurPos, block->pos ) )
               {
                  FILE4LONG adjustedPos ;
                  file4longAssignLong( adjustedPos, block->pos ) ;
                  file4longSubtractLong( &adjustedPos, &opt->writeCurPos ) ;
                  if ( file4longLess( adjustedPos, opt->blockSize ) )   /* partially filled block, can just extend */
                  {
                     file4longAssignLong( opt->writeCurPos, block->pos ) ;
                     flushBuffer = 0 ;
                  }
                  else
                     flushBuffer = 1 ;
               }
               else
                  flushBuffer = 1 ;
            }
            else   /* just want to write at current spot, so go ahead */
               flushBuffer = 0 ;
         }

      }

      #ifdef E4ANALYZE
         if ( flushBuffer < 0 || flushBuffer > 1 )
            return error4( block->file->codeBase, e4info, E92508 ) ;
      #endif

      if ( flushBuffer == 1 )
      {
         if ( ( rc = opt4flushWriteBuffer( opt ) ) != 0 )
            return rc ;
         opt->writeFile = block->file ;
         // AS Apr 13/04 - support for optimizing large files
         file4longAssignLong( opt->writeStartPos, block->pos ) ;
         file4longAssignLong( opt->writeCurPos, block->pos ) ;
      }
      FILE4LONG posOffset ;
      file4longAssignLong( posOffset, opt->writeCurPos ) ;
      file4longSubtractLong( &posOffset, &opt->writeStartPos ) ;
      assert5( file4longGetHi( posOffset ) == 0 ) ;
      memcpy( opt->writeBuffer + file4longGetLo( posOffset ), block->data, block->len ) ;
      file4longAdd( &opt->writeCurPos, block->len ) ;
      opt->writeBlockCount++ ;

      #ifdef E4ANALYZE_ALL
         if ( block->file->hasDup == 1 )
         {
            assert5( file4longGetHi( opt->writeStartPos ) == 0 ) ;  // don't support for large files
            if ( file4cmpPart( opt->writeFile->codeBase, opt->writeBuffer, opt->writeFile,
                 file4longGetLo( opt->writeStartPos ), (unsigned)(file4longGetLo( opt->writeCurPos ) - file4longGetLo( opt->writeStartPos )) ) != 0 )
               return error4( block->file->codeBase, e4opt, E82503 ) ;
         }
      #endif
   }
   else
   {
      #ifdef __SC__   /* compiler bug - cpp */
         if ( opt->writeFile == block->file )
         {
            long_val = opt->writeBlockCount ;
            long_val *= opt->blockSize ;

            if ( opt->writeCurPos >= (unsigned long) block->pos &&
                 (opt->writeCurPos - long_val ) <= (unsigned long)block->pos )
               if ( ( rc = opt4flushWriteBuffer( opt ) ) != 0 )
                  return rc ;
         }
      #else
         // AS Apr 13/04 - support for optimizing large files
         if ( opt->writeFile == block->file )
         {
            FILE4LONG comparePos, subtractLong ;
            file4longAssignLong( comparePos, opt->writeCurPos ) ;
            file4longAssign( subtractLong, opt->writeBlockCount, 0 ) ;
            file4longMultiply( subtractLong, opt->blockSize ) ;
            file4longSubtractLong( &comparePos, &subtractLong ) ;
            if ( file4longGreaterEqLong( opt->writeCurPos, block->pos ) && file4longLessEqLong( comparePos, block->pos ) )
               if ( ( rc = opt4flushWriteBuffer( opt ) ) != 0 )
                  return rc ;
         }
      #endif
      block->file->doBuffer = 0 ;
      #ifdef S4WRITE_DELAY
         if ( doDelay == 1 && opt->delayWriteBuffer != 0 )
         {
            for ( ;; )  /* wait until a block is available */
            {
               critical4sectionEnter( &opt->critical4optWrite ) ;
               delayLink = (LINK4 *)l4first( &opt->delayAvail ) ;
               if ( delayLink == 0 )
               {
                  critical4sectionLeave( &opt->critical4optWrite ) ;
                  // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
                  // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
                  u4sleep( block->file->codeBase ) ;
               }
               else
                  break ;
            }

            delayBlock = (OPT4BLOCK *)(delayLink - 1) ;
            l4remove( &opt->delayAvail, delayLink ) ;
            critical4sectionLeave( &opt->critical4optWrite ) ;
            memcpy( delayBlock->data, block->data, block->len ) ;

            if ( file4writeOpt( block->file, block->pos, delayBlock->data, block->len, 1, opt4writeCompletionRoutine, &delayBlock->lruLink ) != r4delay )
            {
               /* the delay links were not used, so put onto list */
               critical4sectionEnter( &opt->critical4optWrite ) ;
               l4add( &opt->delayAvail, (LINK4 *)&delayBlock->lruLink ) ;
               critical4sectionLeave( &opt->critical4optWrite ) ;
            }
         }
         else
      #endif
      {
         if ( ( rc = file4writeOpt( block->file, block->pos, block->data, block->len, 0, 0, 0 ) ) != 0 )
            return rc ;
      }
      block->file->doBuffer = 1 ;
   }

   block->changed = 0 ;

   return 0 ;
}


int opt4blockRemove( OPT4 *opt, OPT4BLOCK *block, int doFlush )
{
   /* esp. in delay-write, may return a different block back to user for
      use, instead of the current one
   */
   #ifdef E4PARM_LOW
      if ( block == 0 )
      {
         error4( 0, e4parm_null, E92508 ) ;
         return 0 ;
      }
   #endif

   if ( doFlush && block->changed )
   {
      block->file->doBuffer = 0 ;
      opt4blockFlush( opt, block, 1, 1 ) ;
      block->file->doBuffer = 1 ;
   }

   if ( block->file != 0 )
   {
      l4remove( &opt->lists[opt4fileHash( opt, block->file, block->pos )], block ) ;
      block->file = 0 ;
   }

   opt4blockClear( block ) ;

/*   return block ;  */
   return 0 ;
}



static void opt4fileDeletePartialStart( FILE4 *file, const FILE4LONG lowPos, const FILE4LONG hiPos )
{
   /* purpose is to look for an optimization block, and if one is found then remove part
      of its data.  This can occur when a file is being shortened by a lenSet call and the
      file is bufferred.
   */

   /* lowPos / blockSize because position must be on a block boundary */
   OPT4 *opt = &file->codeBase->opt ;
   // FILE4LONG fLong ;  - AS Sep 16/04 - unreferenced
   long hashVal = opt4fileHash( opt, file, lowPos ) ;
   FILE4LONG returnPos ;
   file4longAssignLong( returnPos, lowPos ) ;
   file4longDivide( returnPos, file->codeBase->opt.blockSize ) ;
   OPT4BLOCK *blockOn = opt4fileReturnBlock( file, returnPos, hashVal ) ;

   if ( blockOn )  /* block is in memory buffers, so partially delete */
   {
      if ( file4longLessEqLong( file->len, hiPos ) )   /* just removing all file, so merely delete */
      {
         // AS Apr 13/04 - support for optimizing large files
         FILE4LONG lenLong ;
         file4longAssignLong( lenLong, lowPos ) ;
         file4longSubtractLong( &lenLong, &blockOn->pos ) ;
         assert5( file4longGetHi( lenLong ) == 0 ) ;
         blockOn->len = file4longGetLo( lenLong ) ;
      }
      else  /* read the old data into the block - ignore errors since could be eof, else catch later */
      {
         /* LY July 7/03 : changed from 0 to 0L */
         // file4longAssignLong( fLong, lowPos ) ;
         FILE4LONG offset ;
         file4longAssignLong( offset, blockOn->pos ) ;
         file4longSubtractLong( &offset, &lowPos ) ;
         assert5( file4longGetHi( offset ) == 0 ) ;
         FILE4LONG offset2 ;
         file4longAssignLong( offset2, lowPos ) ;
         file4longSubtractLong( &offset2, &blockOn->pos ) ;
         assert5( file4longGetHi( offset2 ) == 0 ) ;
         file4readInternal( file, lowPos, (char *)blockOn->data + file4longGetLo( offset ), (unsigned)(opt->blockSize - file4longGetLo( offset2 )) ) ;
      }
   }
}



static void opt4fileDeleteBlock( FILE4 *file, const FILE4LONG onPos )
{
   /* looks for the block at the given pos, and if found removes it */
   /* onPos / blockSize because position must be on a block boundary */
   OPT4 *opt = &file->codeBase->opt ;

   // modify the input positoin to be on a valid block boundary
   // unsigned long onPosBoundary = onPos / file->codeBase->opt.blockSize ;
   // onPosBoundary *= file->codeBase->opt.blockSize ;
   FILE4LONG onPosBoundary ;
   file4longAssignLong( onPosBoundary, onPos ) ;
   file4longDivide( onPosBoundary, file->codeBase->opt.blockSize ) ;
   file4longMultiply( onPosBoundary, file->codeBase->opt.blockSize ) ;

   OPT4BLOCK *blockOn = opt4fileReturnBlock( file, onPosBoundary, opt4fileHash( opt, file, onPos ) ) ;

   if ( blockOn )  /* block in memory, so delete */
   {
      opt4blockRemove( opt, blockOn, 0 ) ;
      opt4blockLruTop( blockOn ) ;
      l4addBefore( &opt->avail, l4first( &opt->avail ), &blockOn->lruLink ) ;
   }
}



static void opt4fileDeletePartialEnd( FILE4 *file, const FILE4LONG onPos, const FILE4LONG hiPos )
{
   OPT4 *opt = &file->codeBase->opt ;
   OPT4BLOCK *blockOn = opt4fileReturnBlock( file, onPos, opt4fileHash( opt, file, onPos ) ) ;

   if ( blockOn )
   {
      if ( file4longLessEqLong( file->len, hiPos ) )
      {
         opt4blockRemove( opt, blockOn, 0 ) ;
         opt4blockLruTop( blockOn ) ;
         l4addBefore( &opt->avail, l4first( &opt->avail ), &blockOn->lruLink ) ;
      }
      else
      {
         /* LY July 7/03 : changed from 0 to 0L */
         FILE4LONG fLong ;
         file4longAssignLong( fLong, hiPos ) ;
         file4longSubtractLong( &fLong, &blockOn->pos ) ;
         assert5( file4longGetHi( fLong ) == 0 ) ;
         file4readInternal( file, onPos, blockOn->data, file4longGetLo( fLong ) ) ;
      }
   }
}


// AS Apr 13/04 - support for optimizing large files
static unsigned file4longShiftValue( FILE4LONG nonshifted, short numShift )
{
   // shifts such that we end up with a long value
   // AS Sep 30/04 - Compile fix
   #ifdef S4FILE_EXTENDED
      nonshifted.dLong = ( nonshifted.dLong << numShift ) ;
   #else
      nonshifted = ( nonshifted << numShift ) ;
   #endif
   // And we always blank out the high byte...
   unsigned shifted = file4longGetLo( nonshifted ) ;
   shifted = shifted >> numShift ;
   return shifted ;
}


static FILE4LONG file4longShiftValueLong( FILE4LONG nonshifted, short numShift )
{
   // shifts such that we end up with a FILE4LONG value
   // AS Sep 30/04 - Compile fix
   #ifdef S4FILE_EXTENDED
      nonshifted.dLong = nonshifted.dLong << numShift ;
      // AS Sep 19/07 - dealing with dLong we need to leave longHi in place otherwise we adjust the shift value incorrectly
      // nonshifted.piece.longHi = 0 ;  // the shifting does not do the high byte, but it should be zeroed out
      nonshifted.dLong = nonshifted.dLong >> numShift ;
   #else
      nonshifted = nonshifted << numShift ;
      // nonshifted.piece.longHi = 0 ;  // the shifting does not do the high byte, but it should be zeroed out
      nonshifted = nonshifted >> numShift ;
   #endif
   return nonshifted ;
}


static void opt4fileDeleteFromBufferredBlocks( FILE4 *file, const FILE4LONG lowPos, const FILE4LONG hiPos )
{
   FILE4LONG onPos ;
   OPT4 *opt = &file->codeBase->opt ;
   FILE4LONG endDeletePos ;
   file4longAssignLong( endDeletePos, hiPos ) ;
   file4longAdd( &endDeletePos, opt->blockSize - 1 ) ;

   #ifdef E4PARM_LOW
      if ( file == 0 )
      {
         error4( 0, e4parm, E92508 ) ;
         return ;
      }
   #endif

   /* first delete any partial block at the beginning of the range (i.e. the beginning of the block
      may be left intact)
   */
   FILE4LONG hashPos ;
   file4longAssignLong( hashPos, lowPos ) ;
   file4longAdd( &hashPos, file->codeBase->opt.blockSize - 1L ) ;
   if ( opt4fileHash( opt, file, lowPos ) != opt4fileHash( opt, file, hashPos ) )  /* not on a block boundary, so delete partial */
   {
      opt4fileDeletePartialStart( file, lowPos, hiPos ) ;
      /// onPos = ( (lowPos + opt->blockSize) >> opt->blockPower ) << opt->blockPower ;
      file4longAssignLong( onPos, lowPos ) ;
      file4longAdd( &onPos, opt->blockSize ) ;
      onPos = file4longShiftValueLong( onPos, opt->blockPower ) ;
   }
   else
      file4longAssignLong( onPos, lowPos ) ;

   /* now remove all the complete blocks that fall within the range to delete */
   while( file4longLessLong( onPos, endDeletePos ) )
   {
      opt4fileDeleteBlock( file, onPos ) ;
      file4longAdd( &onPos, opt->blockSize ) ;
   }

   /* now delete part of the last block -- i.e. at the upper part of the range.  If a partial
      remove, then it means we want to over-write the current block info with the actual
      file data (thus in effect removing the bufferred data)
   */
   file4longSubtract( &onPos, opt->blockSize ) ;
   if ( file4longLessLong( onPos, hiPos ) )
      opt4fileDeletePartialEnd( file, onPos, hiPos ) ;
}



static int opt4fileDeleteFlushAllBuffers( FILE4 *file )
{
   /*
      Flushes all the buffers because we are removing parts of an optimized file and we need
      to ensure that there are not any stray bits of the file hanging around in memory.

      We must make sure that there is no chance that any of the buffers flushed are re-read
      before we return from this function and the removal is completed.  Basically this
      means be careful that flushed buffers are not automatically further advance-read, etc.

      ERRORS:  Basically only a severe error can occur.  In this case the caller should not
      proceed but instead should return an error back out to its caller.
   */

   /* flush the write buffer first since it may use the delay write process to write out, so
      flush the delay-write afterwards.
   */
   if ( opt4flushWriteBuffer( &file->codeBase->opt ) < 0 )
      return -1 ;

   #ifdef S4WRITE_DELAY
      /* ensure we call with doWrite == 1, because the delay part may be outside of the delete range */
      if ( file4writeDelayFlush( file, 1 ) < 0 )
         return -1 ;
   #endif

   /* we must cancel advance read's before cancelling the read special buffer because the
      advance read may be reading into it, and we may then have a timing problem (i.e.
      the special buffer gets re-activated with an advance read before we can cancel the
      advance-read)
   */

   #ifdef S4READ_ADVANCE
      file4advanceCancel( file ) ;
   #endif

   opt4fileCancelReadSpBuffer( file ) ;

   return 0 ;
}



int opt4fileDelete( FILE4 *file, const FILE4LONG lowPos, const FILE4LONG hiPos )
{
   /* function to remove, without saving, parts of an optimized file

      lowPos is the start position of the range (where to start deleting)
      hiPos is the end position of the range (where to stop deleting)

      Need to consider removing from:
         bufferred blocks
         the write buffer
         the read buffer
         delay write and advance read buffers

      Since performance is NOT CRITICAL for this function (generally only gets called in
      cases where rolling back a transaction, and therefore unappending records from a
      file), the easiest and safest way to do this is:

      flush out the write buffer, the read buffer, and the delay buffers.

      then go ahead and remove any data from the bufferred blocks.
   */

   int rc = opt4fileDeleteFlushAllBuffers( file ) ;
   if ( rc < 0 )
      return rc ;
   opt4fileDeleteFromBufferredBlocks( file, lowPos, hiPos ) ;

   return 0 ;
}



// AS Nov 28/02 - need to extend out an existing block in some cases to contain more data if
// file4lenSet() is called.  We don't actually need to assign any data in that case, since
// at the low-level file we never actually are writing anything out.
void opt4fileExtend( FILE4 *file, const FILE4LONG newLen, const FILE4LONG startPos )
{
   OPT4 *opt = &file->codeBase->opt ;
   FILE4LONG startPosBoundary ;
   file4longAssignLong( startPosBoundary, startPos ) ;
   file4longDivide( startPosBoundary, file->codeBase->opt.blockSize ) ;
   file4longMultiply( startPosBoundary, file->codeBase->opt.blockSize ) ;
   long hashVal = opt4fileHash( opt, file, startPos ) ;
   OPT4BLOCK *blockOn = opt4fileReturnBlock( file, startPosBoundary, hashVal ) ;

   if ( blockOn )  /* block is in memory buffers, so extend it out */
   {
      FILE4LONG blockEndPos ;
      file4longAssignLong( blockEndPos, newLen ) ;
      file4longSubtractLong( &blockEndPos, &blockOn->pos ) ;
      // if the new pos > block max length, it means we are extending past the end of this block,
      // in which case just max out this block.
      if ( file4longGreater( blockEndPos, opt->blockSize ) )
         file4longAssign( blockEndPos, opt->blockSize, 0 ) ;
      assert5( file4longGetHi( blockEndPos ) == 0 ) ;
      blockOn->len = file4longGetLo( blockEndPos ) ;
   }
}



int opt4flushAll( OPT4 *opt, char doFree )
{
   OPT4BLOCK *blockOn ;
   LINK4 *linkOn, *nextLink ;
   LIST4 *flushList ;
   char i ;
   int rc, saveRc ;

   #ifdef E4PARM_LOW
      if ( opt == 0 )
         return error4( 0, e4parm_null, E92508 ) ;
   #endif

   saveRc = opt4flushWriteBuffer( opt ) ;

   for ( i = 0 ; i < OPT4NUM_LISTS ; i++ )
   {
      flushList = &opt->prio[i]->list ;

      for( linkOn = (LINK4 *)l4first( flushList ) ; linkOn != 0; )
      {
         blockOn = (OPT4BLOCK *)( linkOn - 1 ) ;
         if( blockOn->changed )
         {
            rc = opt4blockFlush( opt, blockOn, 1, 0 ) ;
            if ( rc != 0 )
               saveRc = rc ;
         }

         if ( doFree )
         {
            nextLink = (LINK4 *)l4next( flushList, linkOn ) ;
            l4remove( &opt->lists[ opt4fileHash( opt, blockOn->file, blockOn->pos )], blockOn ) ;
            opt4blockLruTop( blockOn ) ;
            l4add( &opt->avail, linkOn ) ;
            linkOn = nextLink ;
            opt4blockClear( blockOn ) ;
         }
         else
            linkOn = (LINK4 *)l4next( flushList, linkOn ) ;
      }
   }
   return saveRc ;
}

int opt4fileFlushList( OPT4 *opt, FILE4 *file, LIST4 *flushList, int doFree )
{
   OPT4BLOCK *blockOn ;
   LINK4 *linkOn, *nextLink ;

   #ifdef E4PARM_LOW
      if ( file == 0 || flushList == 0 )
         return error4( 0, e4parm, E92508 ) ;
   #endif

   for( linkOn = (LINK4 *)l4first( flushList ) ; linkOn != 0; )
   {
      blockOn = (OPT4BLOCK *)( linkOn - 1 ) ;
      if( blockOn->file == file )
      {
         if( blockOn->changed )
            if ( opt4blockFlush( opt, blockOn, 1, 0 ) < 0 )
               return -1 ;

         if ( doFree )
         {
            nextLink = (LINK4 *)l4next( flushList, linkOn ) ;
            l4remove( &opt->lists[ opt4fileHash( opt, file, blockOn->pos )], blockOn ) ;
            opt4blockLruTop( blockOn ) ;
            l4add( &opt->avail, linkOn ) ;
            linkOn = nextLink ;
            opt4blockClear( blockOn ) ;
         }
         else
            linkOn = (LINK4 *)l4next( flushList, linkOn ) ;
      }
      else
         linkOn = (LINK4 *)l4next( flushList, linkOn ) ;
   }
   return 0 ;
}



#ifdef S4WRITE_DELAY
void S4CALL opt4largeWriteCompletionRoutine( void *delay )
{
   /* just resets the avail flag */
   *((int *)(((FILE4WRITE_DELAY *)(delay))->completionData)) = 1 ;
}
#endif



int opt4flushWriteBuffer( OPT4 *opt )
{
   int rc, oldDoBuffer, oldBufferWrites ;
   FILE4 *file ;

   #ifdef E4PARM_LOW
      if ( opt == 0 )
         return error4( 0, e4parm_null, E92508 ) ;
   #endif

   if ( opt->writeBlockCount != 0 )
   {
      file = opt->writeFile ;
      #ifdef E4ANALYZE_ALL
         if ( file->hasDup == 1 )
         {
            assert5( file4longGetHi( opt->writeStartPos ) == 0 ) ;  // don't support for large files
            if ( file4cmpPart( file->codeBase, opt->writeBuffer, file, file4longGetLo( opt->writeStartPos ), (unsigned)(file4longGetLo( opt->writeCurPos ) - file4longGetLo( opt->writeStartPos )) ) != 0 )
               return error4( file->codeBase, e4opt, E80602 ) ;
         }
      #endif

      oldDoBuffer = file->doBuffer ;
      oldBufferWrites = file->bufferWrites ;

      // AS Jul 20/04 - with file compression (at least) it was possible that the file was being extended and these variables being
      // accessed inconsistently.  Ensure we have exclusive critical section access to the file before modifying these values
      #ifdef S4DELAY_WRITE_MT
         critical4sectionEnter( &file->critical4file ) ;
      #endif
      file->doBuffer = 0 ;
      file->bufferWrites = 0 ;
      #ifdef S4DELAY_WRITE_MT
         critical4sectionLeave( &file->critical4file ) ;
      #endif

      #ifdef S4WRITE_DELAY
         FILE4LONG tLong ;
         if ( opt->writeBuffer == opt->writeBufferActual )
         {
            opt->writeBufferActualAvail = 0 ;
            /* LY 4/28/99 : convert opt->writeStartPos to FILE4LONG */
            file4longAssignLong( tLong, opt->writeStartPos ) ;
            FILE4LONG writeLen ;
            file4longAssignLong( writeLen, opt->writeCurPos ) ;
            file4longSubtractLong( &writeLen, &opt->writeStartPos ) ;
            assert5( file4longGetHi( writeLen ) == 0 ) ;
            rc = file4writeDelay( file, tLong, opt->writeBuffer, file4longGetLo( writeLen ), opt4largeWriteCompletionRoutine, &opt->writeBufferActualAvail ) ;
            while ( opt->delayLargeBufferAvail != 1 )  /* wait so we can use this buffer */
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( file->codeBase ) ;
            }
            opt->writeBuffer = opt->delayLargeBuffer ;
         }
         else
         {
            #ifdef E4ANALYZE
               if ( opt->writeBuffer != opt->delayLargeBuffer )
                  return error4( 0, e4struct, E92508 ) ;
            #endif
            opt->delayLargeBufferAvail = 0 ;
            /* LY 4/28/99 : convert opt->writeStartPos to FILE4LONG */
            file4longAssignLong( tLong, opt->writeStartPos ) ;
            FILE4LONG writeLen ;
            file4longAssignLong( writeLen, opt->writeCurPos ) ;
            file4longSubtractLong( &writeLen, &opt->writeStartPos ) ;
            assert5( file4longGetHi( writeLen ) == 0 ) ;
            rc = file4writeDelay( file, tLong, opt->writeBuffer, file4longGetLo( writeLen ), opt4largeWriteCompletionRoutine, &opt->delayLargeBufferAvail ) ;
            while ( opt->writeBufferActualAvail != 1 )  /* wait so we can use this buffer */
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( file->codeBase ) ;
            }
            opt->writeBuffer = opt->writeBufferActual ;
         }
      #else
         FILE4LONG tLong ;
         /* LY July 7/03 : changed from 0 to 0L */
         file4longAssign( tLong, file4longGetLo( opt->writeStartPos ), 0L ) ;
         // LY Aug 11/04 : changed method of computing opt->writeCurPos - opt->writeStartPos
         FILE4LONG diff ;
         file4longAssignLong( diff, opt->writeCurPos ) ;
         file4longSubtractLong( &diff, &opt->writeStartPos ) ;
         rc = file4writeInternal( file, tLong, opt->writeBuffer, (unsigned)file4longGetLo( diff ) ) ;
      #endif
      // AS Jul 20/04 - with file compression (at least) it was possible that the file was being extended and these variables being
      // accessed inconsistently.  Ensure we have exclusive critical section access to the file before modifying these values
      #ifdef S4DELAY_WRITE_MT
         critical4sectionEnter( &file->critical4file ) ;
      #endif
      file->doBuffer = oldDoBuffer ;
      file->bufferWrites = oldBufferWrites ;
      #ifdef S4DELAY_WRITE_MT
         critical4sectionLeave( &file->critical4file ) ;
      #endif
      if ( rc < 0 )
         return rc ;
      file4longAssign( opt->writeStartPos, 0, 0 ) ;
      file4longAssign( opt->writeCurPos, 0, 0 ) ;
      opt->writeBlockCount = 0 ;
      opt->writeFile = 0 ;
   }

   return 0 ;
}



/* this function sets the 'selected' member of the lru-list to show which
   links are considered available. */
static void opt4fileMarkAvailable( OPT4 *opt, int useCount )
{
   OPT4LIST *optList ;
   OPT4BLOCK *block ;
   LINK4 *link ;
   LIST4 *list ;
   int i ;
   unsigned long linksLeft ;
   unsigned long elapsedTime ;

   for ( i = 0 ; i < OPT4NUM_LISTS ; i++ )
   {
      optList = opt->prio[i] ;

      if ( useCount )
      {
         optList->currentPrioCount-- ;
         if ( optList->currentPrioCount > 0 )
            continue ;
         optList->currentPrioCount = i ;  /* reset priority */
      }

      list = &optList->list ;

      for ( linksLeft = l4numNodes( list ), link = (LINK4 *)l4first( list ) ;; linksLeft--, link = (LINK4 *)l4next( list, link ) )
      {
         if ( link == 0 )
            break ;
         block = (OPT4BLOCK *)(link - 1) ;

         elapsedTime = opt->accessTimeCount - block->accessTime ;
         if ( elapsedTime < optList->minTime )  /* all blocks this new must remain on list */
            break ;

         /* other wise determine based on minimum links */

         elapsedTime = opt->readTimeCount - block->readTime ;
         if ( elapsedTime > optList->maxTime )  /* candidate for removal */
         {
            list->selected = link ;
            continue ;
         }

         /* else use maximum link information */
         if ( linksLeft > (unsigned long)optList->minLink )
         {
            list->selected = link ;
            continue ;
         }

         /* else at or below the minimum allowed links, so stop */
         break ;
      }

      if ( list->selected == 0 )   /* there were none available, so increase the time before a re-scan */
         optList->currentPrioCount++ ;
   }
}

static OPT4BLOCK *opt4fileChooseBlock( FILE4 *file )
{
   /* also sets the priority scheme */
   /* this function gets called when we need a list from which to optain a block for transferring info into or out of */
   LINK4 *lruLink ;
   LIST4 *chooseList ;
   OPT4 *opt ;
   long i, listAvg, l1, l2, l3 ;
   OPT4BLOCK *block ;

   #ifdef E4PARM_LOW
      if ( file == 0 )
      {
         error4( 0, e4parm_null, E92508 ) ;
         return 0 ;
      }
   #endif

   opt = &file->codeBase->opt ;

   if ( opt->avail.nLink )   // there is a block available on the avail list, so use that list
      chooseList = &opt->avail ;
   else
   {
      // blocks are sometimes shifted onto the bottom of a list when it is thought they
      // won't be needed.  At that time, they are also 'selected'
      // so, look for a selected block moving from lowest priority list to highest priority
      // list, and if a selected link is found, return that link
      for( i = 0 ; i < OPT4NUM_LISTS ; i++ )
      {
         chooseList = &opt->prio[i]->list ;
         if ( chooseList->selected != 0 )   /* we have the desired link */
            break ;
      }

      if ( i >= OPT4NUM_LISTS )
      {
         /* no candidate lists were found, so try finding something available */
         // call function which looks through lists and makes links available based
         // on a priority counting system
         opt4fileMarkAvailable( opt, 1 ) ;

         for( i = 0 ; i < OPT4NUM_LISTS ; i++ )
         {
            chooseList = &opt->prio[i]->list ;
            if ( chooseList->selected != 0 )   /* we have the desired link */
               break ;
         }

         if ( i >= OPT4NUM_LISTS )
         {
            /* no candidate lists were found, mark available for ALL lists, and
              do not use priority counting system */
            opt4fileMarkAvailable( opt, 0 ) ;

            for( i = 0 ; i < OPT4NUM_LISTS ; i++ )
            {
               chooseList = &opt->prio[i]->list ;
               if ( chooseList->selected != 0 )   /* we have the desired link */
                  break ;
            }

            if ( i >= OPT4NUM_LISTS )
            {
               /* no candidate lists were found, so try list normalization */
               l1 = l4numNodes( &opt->prio[0]->list ) ;
               l2 = l4numNodes( &opt->prio[1]->list ) ;
               l3 = l4numNodes( &opt->prio[2]->list ) ;
               listAvg = (l1 + l2 + l3) / 3  ;

               chooseList = 0 ;

               if ( listAvg > 0 )
               {
                  if ( l1 > listAvg )
                     chooseList = &opt->prio[0]->list ;
                  else
                  {
                     if ( l2 > listAvg )
                        chooseList = &opt->prio[1]->list ;
                     else
                        chooseList = &opt->prio[2]->list ;
                  }
               }
               else
               {
                  /* just try to get a link */
                  chooseList = &opt->prio[3]->list ;
                  if ( l4numNodes( chooseList ) == 0 )
                  {
                     chooseList = &opt->prio[4]->list ;
                     /* if ( l4numNodes( chooseList ) == 0 )
                        chooseList = &opt->prio[5]->list ; */
                  }

               }
            }
         }
      }
   }

   lruLink = (LINK4 *)l4first( chooseList ) ;
   if ( lruLink == 0 )   /* none were available, this is an error */
   {
      error4( 0, e4struct, E92508 ) ;
      return 0 ;
   }
   if ( lruLink == chooseList->selected )  /* lru link is selected, set selected to null */
      chooseList->selected = (LINK4 *)0 ;
   l4remove( chooseList, lruLink ) ;
   block = (OPT4BLOCK *)( lruLink - 1 ) ;
   return block ;
}

static void opt4blockUpgradePriorityCheck( OPT4BLOCK *block, OPT4 *opt )
{
   #ifndef S4OFF_INDEX
      #ifdef N4OTHER
         TAG4FILE *t4file ;
      #else
         INDEX4FILE *i4file ;
      #endif
   #endif
   DATA4FILE *d4file ;

   if ( block->optList == &opt->dbfLo )  /* maybe move to hi */
   {
      d4file = ((DATA4FILE *)block->file->ownerPtr) ;
      /* NOT moved to hi-priority if record size > 4k, to avoid memory congestion (except header pos 0 block) */
      if ( d4file != 0 )
      {
         // AS 06/26/00 - was incorrectly not moving if < 4096 instead of > 4096
         // if ( block->pos == 0 || (d4file->hiPrio == 1 && dfile4recWidth( d4file ) > 4096 ) )
         if ( file4longEqualZero( block->pos ) || (d4file->hiPrio == 1 && dfile4recWidth( d4file ) < 4096 ) )
         {
            l4remove( &block->optList->list, &block->lruLink ) ;
            opt4listLruBottomPlace( &opt->dbfHi, block ) ;
         }
      }
      return ;
   }

   #ifndef S4OFF_INDEX
      if ( block->optList == &opt->indexLo )  /* maybe move to hi */
      {
         #ifdef N4OTHER
            t4file = ((TAG4FILE *)block->file->ownerPtr) ;

            if ( t4file != 0 )
            {
               if ( t4file->readBlockTag == 0 || block->len != opt->blockSize )  /* not reading a block, so leave at low priority */
                  return ;

               /* first ensure that it is on a real index boundary, and that we read the whole block */
               if ( opt->blockSize / 2 == B4BLOCK_SIZE_INTERNAL )  /* must check both blocks */
               {
                  if ( b4dataLeaf( block->data, t4file->readBlockTag ) == 0 )
                  {
                     l4remove( &block->optList->list, &block->lruLink ) ;
                     opt4listLruBottomPlace( &opt->indexHi, block ) ;
                     return ;
                  }
                  if ( b4dataLeaf( (char *)block->data + B4BLOCK_SIZE_INTERNAL, t4file->readBlockTag ) == 0 )
                  {
                     l4remove( &block->optList->list, &block->lruLink ) ;
                     opt4listLruBottomPlace( &opt->indexHi, block ) ;
                  }
               }

               if ( opt->blockSize == B4BLOCK_SIZE_INTERNAL )
                  if ( b4dataLeaf( block->data, t4file->readBlockTag ) == 0 )
                  {
                     l4remove( &block->optList->list, &block->lruLink ) ;
                     opt4listLruBottomPlace( &opt->indexHi, block ) ;
                  }
            }
         #else
            i4file = ((INDEX4FILE *)block->file->ownerPtr) ;

            if ( i4file != 0 )
            {
               if ( i4file->readBlockTag == 0 || block->len != opt->blockSize )  /* not reading a block, so leave at low priority */
                  return ;

               /* first ensure that it is on a real index boundary, and that we read the whole block */
               #ifdef S4MDX
                  if ( (unsigned long)i4file->header.blockRw != opt->blockSize )
                     return ;

                  /* if it is a branch block, then upgrade it's priority */
                  if ( b4dataLeaf( block->data, i4file->readBlockTag ) == 0 )
                  {
                     l4remove( &block->optList->list, &block->lruLink ) ;
                     opt4listLruBottomPlace( &opt->indexHi, block ) ;
                  }
               #else
                  if ( opt->blockSize / 2 == B4BLOCK_SIZE_INTERNAL )  /* must check both blocks */
                  {
                     if ( b4dataLeaf( block->data, i4file->readBlockTag ) == 0 )
                     {
                        l4remove( &block->optList->list, &block->lruLink ) ;
                        opt4listLruBottomPlace( &opt->indexHi, block ) ;
                        return ;
                     }
                     if ( b4dataLeaf( (char *)block->data + B4BLOCK_SIZE_INTERNAL, i4file->readBlockTag ) == 0 )
                     {
                        l4remove( &block->optList->list, &block->lruLink ) ;
                        opt4listLruBottomPlace( &opt->indexHi, block ) ;
                     }
                  }

                  if ( opt->blockSize == B4BLOCK_SIZE_INTERNAL )
                     if ( b4dataLeaf( block->data, i4file->readBlockTag ) == 0 )
                     {
                        l4remove( &block->optList->list, &block->lruLink ) ;
                        opt4listLruBottomPlace( &opt->indexHi, block ) ;
                     }
               #endif
            }
         #endif
      }
   #endif /* S4OFF_INDEX */
}

static OPT4LIST *opt4listDetermine( OPT4 *opt, FILE4 *file, int hiPrio )
{
   switch ( file->type )
   {
      case OPT4DBF:
         if ( hiPrio == 1 )
            return &opt->dbfHi ;
         else
            return &opt->dbfLo ;
      case OPT4INDEX:
         if ( hiPrio == 1 )
            return &opt->indexHi ;
         else
            return &opt->indexLo ;
      default:
         return &opt->other ;
   }
}

/* gets an available block, and places it on the appropriate lru-list */
/* also sets the readTime and accessTime for the block */
static OPT4BLOCK *opt4fileGetBlock( OPT4 *opt, FILE4 *file, int hiPrio )
{
   OPT4BLOCK *block ;
   OPT4LIST *optList ;

   #ifdef E4PARM_LOW
      if ( file == 0 )
      {
         error4( 0, e4parm_null, E92508 ) ;
         return 0 ;
      }

   #endif

   block = opt4fileChooseBlock( file ) ;
   opt4blockRemove( opt, block, 1 ) ;

   optList = opt4listDetermine( opt, file, hiPrio ) ;
   opt4listLruBottomPlace( optList, block ) ;

   block->readTime = opt->readTimeCount++ ;
   if ( opt->readTimeCount == 0 )  /* time count got reset, so reset for everyone */
      opt4timeReset( opt, 1, 0 ) ;
   block->accessTime = ++opt->accessTimeCount ;

   return block ;
}

static void opt4timeReset( OPT4 *opt, int doReadTime, int doAccessTime )
{
   OPT4BLOCK *blockOn ;
   LIST4 *listOn ;
   int i ;

   /* for simplicity, just reset all blocks read-time to 0 (i.e. just read) */
   /* in general this function will rarely be called, thus the impact should be minor */

   for ( i = 0 ; i < OPT4NUM_LISTS ; i++ )
   {
      listOn = &opt->prio[i]->list ;
      blockOn = (OPT4BLOCK *)l4first( listOn ) ;
      for( ;; )
      {
         if ( blockOn == 0 )
            break ;
         if ( doReadTime )
            blockOn->readTime = 0 ;
         if ( doAccessTime )
            blockOn->accessTime = 0 ;
         blockOn = (OPT4BLOCK *)l4next( listOn, blockOn ) ;
      }
   }
}

// AS Apr 13/04 - for large file support, make non inline
long opt4fileHash( OPT4 *opt, FILE4 *file, FILE4LONG pos )
{
   // return ( (( file->hashInit + pos ) >> opt->blockPower ) & opt->mask ) ;
   // AS Sep 30/04 - Compile fix
   file4longAdd( &pos, file->hashInit ) ;
   #ifdef S4FILE_EXTENDED
      pos.dLong = pos.dLong >> opt->blockPower ;
      pos.dLong = pos.dLong & opt->mask ;
   #else
      pos = pos >> opt->blockPower ;
      pos = pos & opt->mask ;
   #endif
   assert5( file4longGetHi( pos ) == 0 ) ;
   return file4longGetLo( pos ) ;
}

/* 06/28/96 AS --> 16-bit, required len may be > sizeof unsigned because the
   value is added to around line 880 below; therefore must use longs */
int opt4fileWrite( FILE4 *file, FILE4LONG pos, unsigned len, const void *data, char changed )
{
   S4LONG hashVal ;
   S4UNSIGNED_LONG lenWritten ;
   FILE4LONG adjustedPos ;
   unsigned readLen, lenRead, extraRead ;
   OPT4BLOCK *blockOn ;
   OPT4 *opt ;
   int doUpgradeCheck = 0 ;

   #ifdef E4PARM_LOW
      if ( file == 0 || data == 0 )
         return error4( 0, e4parm, E92508 ) ;
   #endif

   #ifdef S4ADVANCE_READ
      /* ensure that the information to be written replaces any advanced-read
         information in order to ensure that the advance-read data is up to
         date */

      if ( changed == 1 )
         if ( file->hasHadPendingAdvanceRead )  /* for efficiency, avoid for non-advance-read files */
            file4advanceReadWriteOver( file, pos, len, data, 1 ) ;
   #endif

   // AS Apr 14/04 - support for optimizing large files
   // #ifdef S4FILE_EXTENDED
   //    #ifdef E4ANALYZE
   //       if ( (pos + len) < pos )  /* means overflow, we have exceeded 4 gig file limit */
   //          return error4( 0, e4parm, E92508 ) ;
   //    #endif
   // #endif

   opt = &file->codeBase->opt ;
   lenWritten = 0 ;
   FILE4LONG shiftedPos ;
   file4longAssignLong( shiftedPos, pos ) ;
   // shiftedPos.dLong = (shiftedPos.dLong << opt->numShift) >> opt->numShift ;
   // assert5( file4longGetHi( shiftedPos ) == 0 ) ;
   // extraRead = file4longGetLo( shiftedPos ) ;
   extraRead = file4longShiftValue( shiftedPos, opt->numShift ) ;
   file4longAssignLong( adjustedPos, pos ) ;
   file4longSubtract( &adjustedPos, extraRead ) ;
   if ( len > ( (unsigned long)opt->numBlocks * (unsigned long)opt->blockSize ) )
   {
      /* case where amount to write > total bufferred length - do a piece at a time */
      // adjustedPos = (S4UNSIGNED_LONG)( ((S4UNSIGNED_LONG)opt->numBlocks - 1) * (S4UNSIGNED_LONG)opt->blockSize ) ;
      file4longAssign( adjustedPos, opt->numBlocks - 1, 0 ) ;
      file4longMultiply( adjustedPos, opt->blockSize ) ;
      assert5( file4longGetHi( adjustedPos ) == 0 ) ;
      unsigned long adjustedPosLo = file4longGetLo( adjustedPos ) ;
      for ( lenWritten = 0L ; len > lenWritten ; lenWritten += adjustedPosLo )
      {
         if ( ( len - lenWritten ) < adjustedPosLo )
            adjustedPosLo = len - lenWritten ;
         FILE4LONG writePos ;
         file4longAssignLong( writePos, pos ) ;
         file4longAdd( &writePos, lenWritten ) ;
         if ( opt4fileWrite( file, writePos, adjustedPosLo, (char *)data + lenWritten, changed ) != (int) adjustedPosLo )
            return (int)lenWritten ;
      }
      return (int)lenWritten ;
      file4longAssign( adjustedPos, adjustedPosLo, 0 ) ;
   }
   len += extraRead ;

   do
   {
      hashVal = opt4fileHash( opt, file, adjustedPos ) ;
      readLen = (unsigned) ((len / (unsigned)opt->blockSize) ? opt->blockSize : len) ;
      blockOn = opt4fileReturnBlock( file, adjustedPos, hashVal ) ;
      if ( blockOn == 0 )
      {
         blockOn = opt4fileGetBlock( opt, file, 0 ) ;
         #ifdef E4ANALYZE
            if ( blockOn == 0 )
               return error4( 0, e4info, E92508 ) ;
         #endif
         if ( (S4UNSIGNED_LONG)( readLen - extraRead ) < opt->blockSize && file4longLessLong( adjustedPos, file4lenLow( file ) ) )
            lenRead = opt4fileReadFile( file, adjustedPos, (char *)blockOn->data ) ;
         else
         {
            FILE4LONG comparePos ;
            file4longAssignLong( comparePos, pos ) ;
            file4longAdd( &comparePos, lenWritten + opt->blockSize ) ;
            if ( file4longGreaterEqLong( file4lenLow( file ), comparePos ) )   /* mark block as full to avoid file len set below... */
               lenRead = (unsigned)opt->blockSize ;
            else
               lenRead = 0 ;
         }
         opt4blockAdd( blockOn, file, lenRead, hashVal, adjustedPos ) ;
         doUpgradeCheck = 1 ;
      }

      if ( readLen < (unsigned) extraRead )
         readLen = (unsigned) extraRead ;
      memcpy( (char *)blockOn->data + extraRead, (char *)data + lenWritten, (unsigned)(readLen - extraRead) ) ;
      if ( doUpgradeCheck == 1 )
         opt4blockUpgradePriorityCheck( blockOn, opt ) ;

      opt4listLruBottomShift( blockOn ) ;
      blockOn->changed |= changed ;
      len -= readLen ;
      lenWritten += readLen - extraRead ;
      extraRead = 0 ;
      file4longAdd( &adjustedPos, opt->blockSize ) ;
      FILE4LONG cmp1, cmp2 ;
      file4longAssignLong( cmp1, pos ) ;
      file4longAdd( &cmp1, lenWritten ) ;
      file4longAssignLong( cmp2, blockOn->pos ) ;
      file4longAdd( &cmp2, blockOn->len ) ;
      if ( file4longGreaterLong( cmp1, cmp2 ) )
      {
         // if ( file4longError( file->len ) == ULONG_MAX || ( file4longGetLo( file->len ) < ( pos + lenWritten ) ) ) /* file size has grown */
         if ( file4longError( file->len ) == ULONG_MAX || file4longLessLong( file->len, cmp1 ) ) /* file size has grown */
         {
            // if ( ( blockOn->pos + (S4UNSIGNED_LONG)blockOn->len < pos )
            //   && ( blockOn->pos + (S4UNSIGNED_LONG)blockOn->len > blockOn->pos ) )  /* LY 00/01/14 : pos + len can > 2^32-1 */
            if ( file4longLessLong( cmp2, pos )
               && ( file4longGreaterLong( cmp1, blockOn->pos ) ) )  /* LY 00/01/14 : pos + len can > 2^32-1 */
            {
               // memset( ((char *)blockOn->data) + blockOn->len, 0,(unsigned)( pos - ( blockOn->pos + blockOn->len ) ) ) ;
               FILE4LONG lenSet ;
               file4longAssignLong( lenSet, pos ) ;
               file4longSubtractLong( &lenSet, &blockOn->pos ) ;
               file4longSubtract( &lenSet, blockOn->len ) ;
               assert5( ( file4longGetHi( lenSet ) == 0 ) && file4longGetLo( lenSet ) <= (opt->blockSize - blockOn->len) ) ;  // AS Sep 20/04 - additional testing
               memset( ((char *)blockOn->data) + blockOn->len, 0, file4longGetLo( lenSet ) ) ;
            }
            // blockOn->len = (unsigned)( pos - blockOn->pos + lenWritten ) ;
            FILE4LONG lenSet ;
            file4longAssignLong( lenSet, pos ) ;
            file4longSubtractLong( &lenSet, &blockOn->pos ) ;
            file4longAdd( &lenSet, lenWritten ) ;
            assert5( file4longGetHi( lenSet ) == 0 ) ;
            blockOn->len =  file4longGetLo( lenSet ) ;
            if ( file->bufferWrites == 1 )
            {
               /* LY July 7/03 : changed from 0 to 0L */
               // AS Jul 29/04 - if the file is compressed, ensure the block structures are allocated as required
               #ifdef S4DELAY_WRITE_MT
                  if ( file->fileCreated )
                     critical4sectionEnter( &file->critical4file ) ;
               #endif
               FILE4LONG newLen = pos ;
               file4longAdd( &newLen, lenWritten ) ;
               // LY Jan 19/05 : added switches to avoid compiler error
               #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
                  if ( file->compressInfo != 0 && file->compressInfo->writeCompress != 0 )
                     file4compressSetLenDo( file, newLen ) ;
               #endif
               file4longAssignLong( file->len, newLen ) ;
               #ifdef S4TESTING
                  // LY Feb 2/05 : added #if's to avoid compiler errors
                  #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
                     if ( file->compressInfo != 0 && file->compressInfo->writeCompress != 0 )
                     {
                        if ( !file4longEqualLong( file->compressInfo->writeCompress->fileLen, file->len ) )
                        {
                           // force a gpf so we can debug into this...
                           memcpy( 0, "junk", 4 ) ;
                        }
                     }
                  #endif
               #endif
               #ifdef E4ANALYZE
                  /* make sure the file length really has grown */
                  if ( file->fileCreated )
                  {
                     // LY Feb 2/05 : added #if's to avoid compiler errors
                     #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
                     // AS Jul 9/04 - can't use u4filelength on compressed files becuase the real file length won't match (is compressed)
                     if ( file->compressInfo == 0 )
                     {
                     #endif
                        if ( file4longGreaterLong( u4filelength( file->hand ), file->len ) )
                        {
                           #ifdef S4DELAY_WRITE_MT
                              if ( file->fileCreated )
                                 critical4sectionLeave( &file->critical4file ) ;
                           #endif
                           return error4( file->codeBase, e4info, E92508 ) ;
                        }
                     // LY Feb 2/05 : added #if's to avoid compiler errors
                     #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
                     }
                     else
                     {
                        if ( file4longGreaterLong( file4lenLow( file ), file->len ) )
                        {
                           #ifdef S4DELAY_WRITE_MT
                              if ( file->fileCreated )
                                 critical4sectionLeave( &file->critical4file ) ;
                           #endif
                           return error4( file->codeBase, e4info, E92508 ) ;
                        }
                     }
                     #endif
                  }
               #endif
               #ifdef S4DELAY_WRITE_MT
                  if ( file->fileCreated )
                     critical4sectionLeave( &file->critical4file ) ;
               #endif
            }
         }
         else   /* we have written past our end of block, but not our block, update our block len, dummy info */
         {
            unsigned int a ;
            FILE4LONG b ;
            a = (unsigned) opt->blockSize ;
            // b = (unsigned)( file4longGetLo( file->len ) - blockOn->pos ) ;
            // blockOn->len = (a>b)?b:a ; /* minimum of a or b */
            file4longAssignLong( b, file->len ) ;
            file4longSubtractLong( &b, &blockOn->pos ) ;
            if ( file4longGetHi( b ) != 0 || file4longGetLo( b ) > a )  // b is larger so use a
               blockOn->len = a ;
            else
               blockOn->len = file4longGetLo( b ) ;
             /* AS 02/16/97 changed, else c/s MDX t5row5 failed */
         }
      }
   } while( len && blockOn->len == (unsigned)opt->blockSize ) ;

   return (unsigned) lenWritten ;
}

/* 06/28/96 AS --> 16-bit, required len may be > sizeof unsigned because the
   value is added to around line 880 below; therefore must use longs */
unsigned opt4fileRead( FILE4 *f4, FILE4LONG pos, void *data, unsigned len )
{
   S4LONG hashVal ;
   unsigned readLen ;
   int blocksNeeded ;
   OPT4BLOCK *blockOn ;
   DATA4FILE *d4file ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 || data == 0 )
      {
         error4( 0, e4parm, E92508 ) ;
         return 0 ;
      }
   #endif

   OPT4 *opt = &f4->codeBase->opt ;
   S4UNSIGNED_LONG lenRead = 0 ;
   // unsigned extraRead = (unsigned)((unsigned S4LONG)((unsigned S4LONG)pos << opt->numShift ) >> opt->numShift ) ;
   unsigned extraRead = file4longShiftValue( pos, opt->numShift ) ;

   if ( (unsigned S4LONG)len > ( opt->numBlocks * opt->blockSize ))
   {
      /* case where amount to read > total bufferred length - do a piece at a time */
      S4UNSIGNED_LONG adjustedPosLo = (S4LONG)( ((S4LONG)opt->numBlocks - 1) * (S4LONG)opt->blockSize ) ;

      for ( lenRead = 0 ; len > lenRead ; lenRead += adjustedPosLo )
      {
         if ( (len - lenRead ) < adjustedPosLo )
            adjustedPosLo = len - lenRead ;

         /* AS 09/09/99 - was returning < actual amount read in some cases */
         /* if ( opt4fileRead( f4, pos + lenRead, (char *)data + lenRead, (unsigned)adjustedPos ) != (unsigned)adjustedPos )
               return (unsigned)lenRead ; */
         FILE4LONG readPos ;
         file4longAssignLong( readPos, pos ) ;
         file4longAdd( &readPos, lenRead ) ;
         unsigned partialRead = opt4fileRead( f4, readPos, (char *)data + lenRead, (unsigned)adjustedPosLo ) ;
         if ( partialRead != (unsigned)adjustedPosLo )
            return (unsigned)lenRead + partialRead ;
      }

      #ifdef E4ANALYZE_ALL
         if ( f4->hasDup == 1 )
         {
            assert5( file4longGetHi( pos ) == 0 ) ;  // don't support for large files
            if ( file4cmpPart( f4->codeBase, data, f4, file4longGetLo( pos ), (unsigned)lenRead ) != 0 )
            {
               error4( f4->codeBase, e4opt, E80602 ) ;
               return 0 ;
            }
         }
      #endif

      return (unsigned)lenRead ;
   }
   len += extraRead ;

   FILE4LONG adjustedPos ;
   file4longAssignLong( adjustedPos, pos ) ;
   file4longSubtract( &adjustedPos, extraRead ) ;
   do
   {
      hashVal = opt4fileHash( opt, f4, adjustedPos ) ;
      readLen = (unsigned) ((len / opt->blockSize) ? opt->blockSize : len ) ;
      blockOn = opt4fileReturnBlock( f4, adjustedPos, hashVal ) ;

      if ( blockOn == 0 )  /* read from disk */
      {
         /* first ensure that the file is not a temporary file that is empty */
         if ( f4->fileCreated == 0 )  /* not created, so if not in memory then empty */
         {
            // AS July 26/01 - We may have actually read some data here (i.e.
            // it is part of a while loop), so return that part that we have read, not 0
            return lenRead ;
         }

         /* if force current is on, then pre-reading will be wasted, so just skip */
         if ( opt->forceCurrent != 1 )
         {
            if ( f4->type == OPT4DBF )
            {
               d4file = (DATA4FILE *)f4->ownerPtr ;
               if ( d4file != 0 )
                  if ( d4file->hiPrio == -1 )
                  {
                     opt4fileReadSpBuffer( f4, adjustedPos, -1, -1 ) ;
                     blockOn = opt4fileReturnBlock( f4, adjustedPos, hashVal ) ;
                  }
            }

            if ( blockOn == 0 )
            {
               /* depending on the amount to read, it may still make sense to
                  do advance reading -- quicker to read 2 blocks now than to
                  read 1 block twice */
               if ( len - readLen > 0 )  /* need to read more than 1 block */
               {
                  /* if the 2nd block is already in memory, don't bother */
                  FILE4LONG returnPos ;
                  file4longAssignLong( returnPos, adjustedPos ) ;
                  file4longAdd( &returnPos, opt->blockSize ) ;
                  blockOn = opt4fileReturnBlock( f4, returnPos, opt4fileHash( opt, f4, returnPos ) ) ;
                  if ( blockOn == 0 )  /* not there, so get */
                  {
                     /* check to see how many blocks we need */
                     blocksNeeded = 1 + (int)(len / readLen) ;  /* 1st block, plus any extras */
                     opt4fileReadSpBuffer( f4, adjustedPos, blocksNeeded, 1 ) ;
                     blockOn = opt4fileReturnBlock( f4, adjustedPos, hashVal ) ;
                  }
                  else  /* the 2nd block is there, so reset blockOn and read the first one */
                     blockOn = 0 ;
               }
            }
         }

         if ( blockOn == 0 )
         {
            blockOn = opt4fileGetBlock( opt, f4, 0 ) ;
            opt4blockAdd( blockOn, f4, opt4fileReadFile( f4, adjustedPos, (char *)blockOn->data ), hashVal, adjustedPos ) ;
            opt4blockUpgradePriorityCheck( blockOn, opt ) ;
         }
      }
      else
         if ( opt->forceCurrent == 1 )
         {
            if ( blockOn->changed == 0 && f4->bufferWrites == 0
               #ifndef S4OFF_MULTI
                  && f4->lowAccessMode == OPEN4DENY_NONE
               #endif
               )
               opt4fileReadFile( f4, adjustedPos, (char *)blockOn->data ) ;
         }

      opt4listLruBottomShift( blockOn ) ;

      if ( blockOn->len < readLen )
         readLen = blockOn->len ;
      if ( readLen < (unsigned)extraRead )
         readLen = (unsigned)extraRead ;
      memcpy( (char *)data + lenRead, (char *)blockOn->data + extraRead, (unsigned)(readLen - extraRead) ) ;
      len -= readLen ;
      lenRead += readLen - extraRead ;
      extraRead = 0 ;
      file4longAdd( &adjustedPos, opt->blockSize ) ;
   } while( len && blockOn->len == (unsigned) opt->blockSize ) ;

   #ifdef E4ANALYZE_ALL
      if ( f4->hasDup == 1 )
      {
         assert5( file4longGetHi( pos ) == 0 ) ;  // don't support for large files
         if ( file4cmpPart( f4->codeBase, data, f4, file4longGetLo( pos ), (unsigned)lenRead ) != 0 )
         {
            error4( f4->codeBase, e4opt, E80602 ) ;
            return 0 ;
         }
      }
   #endif

   return (unsigned)lenRead ;
}

void opt4blockLruTop( OPT4BLOCK *block )
{
   LIST4 *list ;
   LINK4 *l4link ;

   #ifdef E4PARM_LOW
      if ( block == 0 )
      {
         error4( 0, e4parm_null, E92508 ) ;
         return ;
      }
   #endif

   list = &block->optList->list ;
   l4link = &block->lruLink ;

   if ( list->selected == l4link )
      list->selected = (LINK4 *)l4prev( list, l4link ) ;

   l4remove( list, l4link ) ;
}

/* LY 99/06/21 : changed posIn from const long to const unsigned long in opt4fileReadFile */
#ifdef S4ADVANCE_TEST
   unsigned S4FUNCTION opt4fileReadFile( FILE4 *file, const FILE4LONG posIn, char *buf )
#else
   static unsigned opt4fileReadFile( FILE4 *file, const FILE4LONG posIn, char *buf )
#endif
{
   OPT4 *opt ;
   FILE4LONG pos ;
   FILE4LONG fPos ;
   #ifdef S4ADVANCE_READ
      unsigned int compareLen, copyOffset, len ;
   #endif

   #ifdef E4PARM_LOW
      if ( file == 0 || buf == 0 )
      {
         error4( 0, e4parm, E92508 ) ;
         return 0 ;
      }
   #endif

   pos = posIn ;
   opt = &file->codeBase->opt ;

   #ifdef S4ADVANCE_READ
      /* first check the advance-read buffer.  If the data is available there,
         then copy it out.  Can't do more than that here because this function
         is reading into a block already (don't want to overwrite that one by
         mistake */

      /* only copy if can get the entire block from advance-read (in theory
         must always be able to get none or all since on block boundaries */

      // AS 02/01/00 -- if Avail == AR4SET, then some of the variables we were looking at may have been changing.
      // therefore, we must only continue on if AR4FULL (or skip if AR4EMPTY)

      if ( opt->advanceLargeBufferAvail != AR4EMPTY )
      {
         while( opt->advanceLargeBufferAvail == AR4SET )  /* wait for the buffer to complete read, if reqd. */
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( file->codeBase ) ;
            }
         if ( opt->advanceLargeBufferAvail == AR4FULL && file == opt->advanceReadFile )
         {
            len = (unsigned)(opt->blockSize) ;

            /* if an overlap of any type occurs, then copy that part out first,
               and then take care of the remaining pieces */
            FILE4LONG cmp ;
            file4longAssignLong( cmp, opt->advanceLargePos ) ;
            file4longAdd( &cmp, opt->advanceLargeLen ) ;
            if ( file4longGreaterEqLong( pos, opt->advanceLargePos ) && file4longLessLong( pos, cmp ) )
            {
               if ( file4longEqualLong( pos, opt->advanceLargePos ) )  /* easy case */
               {
                  if ( opt->advanceLargeLen >= len )
                  {
                     // while( opt->advanceLargeBufferAvail == AR4SET )  /* wait for the buffer to complete read, if reqd. */
                     //    Sleep( 0 ) ;
                     assert5( opt->advanceLargeBufferAvail == AR4FULL ) ;
                     // if ( opt->advanceLargeBufferAvail == AR4FULL ) /* successful read */
                     // {
                     memcpy( buf, opt->advanceLargeBuffer, len ) ;
                     /* reset the position of the data for easier handling later */
                     file4longAdd( &opt->advanceLargePos, len ) ;
                     opt->advanceLargeLen -= len ;
                     memcpy( opt->advanceLargeBuffer, opt->advanceLargeBuffer + len, opt->advanceLargeLen ) ;
                     if ( opt->advanceLargeLen == 0 )  /* just remove */
                     {
                        opt->advanceLargeBufferAvail = AR4EMPTY ;
                        opt->advanceReadFile = 0 ;
                     }
                     return len ;
                     // }
                  }
               }
               else
                  if ( file4longGreaterLong( pos, opt->advanceLargePos ) )  /* full copy only */
                  {
                     // copyOffset = pos - opt->advanceLargePos ;
                     FILE4LONG copyOffsetLong ;
                     file4longAssignLong( copyOffsetLong, pos ) ;
                     file4longSubtractLong( &copyOffsetLong, &opt->advanceLargePos ) ;
                     assert5( file4longGetHi( copyOffsetLong ) == 0 ) ;
                     copyOffset = file4longGetLo( copyOffsetLong ) ;
                     compareLen = opt->advanceLargeLen - copyOffset ;
                     if ( compareLen >= len )  /* then ok */
                     {
                        assert5( opt->advanceLargeBufferAvail == AR4FULL ) ;
                        // while( opt->advanceLargeBufferAvail == AR4SET )  /* wait for the buffer to complete read, if reqd. */
                        //    Sleep( 0 ) ;
                        // if ( opt->advanceLargeBufferAvail == AR4FULL ) /* successful read */
                        // {
                        if ( compareLen == len )
                           opt->advanceLargeLen -= compareLen ;   /* remove trailer, since will get copied into buffers anyway */
                        memcpy( buf, opt->advanceLargeBuffer + copyOffset, len ) ;
                        return len ;
                        // }
                     }
                  }
            }
         }
      }
   #endif

   if ( opt->writeFile == file )
   {
      FILE4LONG cmp1 ;
      file4longAssignLong( cmp1, pos ) ;
      file4longAdd( &cmp1, opt->blockSize ) ;
      if ( file4longGreaterEqLong( cmp1, opt->writeStartPos ) && file4longLessLong( pos, opt->writeCurPos ) )
         opt4flushWriteBuffer( opt ) ;
   }

   /* 07/01/96 AS --> t4excl.c c/s failure after d4zap()
      do not exceed reading the file size if delay-write is true because
      it may still be on disk (i.e. file size may not yet be updated)
      len = file4readLow( file, pos, buf, (unsigned)(opt->blockSize) ) ;
   */

   /* LY July 7/03 : changed from 0 to 0L */
   file4longAssignLong( fPos, pos ) ;
   if ( file->bufferWrites != 0 )
   {
      if ( file4longError( file->len ) != ULONG_MAX )
      {
         // if ( file4longGetLo( file->len ) < pos + opt->blockSize )
         FILE4LONG cmp ;
         file4longAssignLong( cmp, pos ) ;
         file4longAdd( &cmp, opt->blockSize ) ;
         if ( file4longLessLong( file->len, cmp ) )
         {
            FILE4LONG lenRead ;
            file4longAssignLong( lenRead, file->len ) ;
            file4longSubtractLong( &lenRead, &pos ) ;
            assert5( file4longGetHi( lenRead ) == 0 ) ;
            unsigned amountRead = file4readLow( file, fPos, buf, file4longGetLo( lenRead ) ) ;
            return amountRead ;
         }
      }
   }

   unsigned amountRead = file4readLow( file, fPos, buf, (unsigned)(opt->blockSize) ) ;
   assert5( amountRead <= (unsigned)opt->blockSize ) ;
   return amountRead ;
}

/* this function performs larger reads into the given buffer with the
   desired length to read.   This is used for advance reads and when
   larger amounts of information are required
   returned is the length read */
static unsigned long opt4fileReadToBuffer( FILE4 *file, char *buf, FILE4LONG pos, unsigned int readLen )
{
   unsigned long len, saveBlockSize ;
   OPT4 *opt ;

   opt = &file->codeBase->opt ;
   saveBlockSize = opt->blockSize ;
   opt->blockSize = readLen ;
   len = opt4fileReadFile( file, pos, buf ) ;
   opt->blockSize = saveBlockSize ;

   return len ;
}

#ifdef S4ADVANCE_READ
   void S4CALL opt4readCompletionRoutine( void *advance )
   {
      FILE4ADVANCE_READ *advanceRead ;
      int *arFlag ;

      /* to verify safety, use critical section on the arFlag (from CODE4) */

      advanceRead = (FILE4ADVANCE_READ *)advance ;
      arFlag = (int *)(advanceRead->completionData) ;
      if ( advanceRead->usageFlag == r4canceled )
         *arFlag = AR4EMPTY ;
      else
         if ( *arFlag == AR4SET )  /* if reset to empty it means the read was cancelled by the main thread, so leave as empty */
         {
            /* verify that all was read */
            if ( advanceRead->file->codeBase->opt.advanceLargeLen == advanceRead->status )
               *arFlag = AR4FULL ;
            else
               *arFlag = AR4EMPTY ;
         }
   }
#endif /* S4ADVANCE_READ */



#ifdef S4ADVANCE_READ
   static void opt4fileReadAdvanceBuffer( OPT4 *opt, FILE4 *f4, void *buffer, FILE4LONG pos, unsigned int len )
   {
      long hashVal ;

      hashVal = opt4fileHash( opt, f4, pos ) ;
      if ( opt4fileReturnBlock( f4, pos, hashVal ) != 0 )  /* in memory, so don't perform the advance-read */
         return ;

      opt->advanceReadFile = f4 ;
      opt->advanceLargeBufferAvail = AR4SET ;
      file4advanceRead( f4, pos, buffer, len, opt4readCompletionRoutine, &opt->advanceLargeBufferAvail ) ;
   }
#endif /* S4ADVANCE_READ */



static void opt4fileCancelReadSpBuffer( FILE4 *file )
{
   /* cancels the special read buffer if it corresponds to the input file

      this may be handy in various instances, such as when a file is being deleted and we don't
      want deleted parts of the file to be in a buffer somewhere.
   */

   /* simply setting the file marker to 0 immediately makes the buffer useless. */
   if ( file->codeBase->opt.readFile == file )
      file->codeBase->opt.readFile = 0 ;
}



static void opt4fileReadSpBuffer( FILE4 *file, const FILE4LONG posIn, int numBlocks, int direction )
{
   /* this function does advance reads by reading in a whole buffer at
      a time, instead of just one block worth
      will advance-read numBlocks blocks.  If numBlocks == -1, then will advance
      read the whole read buffer, if direction == -1, then it self-detects it
      if either direction or numBlocks is -1, both must be -1
   */
   unsigned long saveBlockSize ;
   FILE4LONG curPos, endPos, pos ;
   OPT4BLOCK *blockOn ;
   OPT4 *opt ;
   long hashVal ;
   int readBlocks ;
   unsigned int curBlocks ;
   #ifdef S4ADVANCE_READ
      Bool5 posInAdvance ;  /* position to read was within the advance-read area */
      int blocksWithin ;  /* the number of blocks within the advance read once position-adjusted (i.e. the length relevanet to quantity reqd. - if numBlocks not -1 */
      int advanceNextRead, doAdvanceRead ;
      FILE4LONG advPos ;
      // AS Jan 9/2014 - there was a problem here that the file might not actually match opt->advnaceReadFile if we are actually just trying to clear out the previous advanceRead...
      FILE4 *advFile ;
   #endif

   #ifdef E4PARM_LOW
      if ( file == 0 )
      {
         error4( 0, e4parm_null, E92508 ) ;
         return ;
      }
      if ( ( direction == -1 && numBlocks != -1 ) || ( direction != -1 && numBlocks == -1 ) )
      {
         error4( file->codeBase, e4parm, E92508 ) ;
         return ;
      }
   #endif

   pos = posIn ;
   opt = &file->codeBase->opt ;
   saveBlockSize = opt->blockSize ;

   #ifdef S4ADVANCE_READ
      if ( numBlocks == -1 )
         doAdvanceRead = 1 ;
      else
         doAdvanceRead = 0 ;
      advanceNextRead = 0 ;

      /* first check the advance read-buffer, and see if it matches the
         information we want.  Even if it doesn't, if numBlocks is -1, then
         it gets copied out (to perform more advance-reading */
      if ( opt->advanceLargeBufferAvail == AR4SET ) /* wait on read if file matches */
         if ( file == opt->advanceReadFile )
            while( opt->advanceLargeBufferAvail == AR4SET )
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( file->codeBase ) ;
            }

      if ( opt->advanceLargeBufferAvail == AR4FULL )
      {
         if ( opt->advanceLargeLen == 0 )
         {
            opt->advanceLargeBufferAvail = AR4EMPTY ;
            opt->advanceReadFile = 0 ;
         }
         else
         {
            blocksWithin = 0 ;
            posInAdvance = 0 ;

            if ( file == opt->advanceReadFile )
            {
               FILE4LONG cmp ;
               file4longAssignLong( cmp, opt->advanceLargePos ) ;
               file4longAdd( &cmp, opt->advanceLargeLen ) ;
               file4longSubtract( &cmp, saveBlockSize ) ;
               if ( file4longGreaterEqLong( pos, opt->advanceLargePos ) && file4longLessEqLong( pos, cmp ) )
               {
                  posInAdvance = 1 ;
                  if ( numBlocks != -1 )
                  {
                     // blocksWithin = ( opt->advanceLargeLen + opt->advanceLargePos - pos  ) / saveBlockSize ;
                     FILE4LONG blocksLong ;
                     file4longAssignLong( blocksLong, opt->advanceLargePos ) ;
                     file4longAdd( &blocksLong, opt->advanceLargeLen ) ;
                     file4longSubtractLong( &blocksLong, &pos ) ;
                     file4longDivide( blocksLong, saveBlockSize ) ;
                     assert5( file4longGetHi( blocksLong ) == 0 ) ;
                     blocksWithin = file4longGetHi( blocksLong ) ;
                  }
               }
               else
                  posInAdvance = 0 ;
            }

            if ( file == opt->advanceReadFile || numBlocks == -1 )  /* extract the blocks from the advance-read buffer */
            {
               // advPos = curPos = opt->advanceLargePos ;
               file4longAssignLong( advPos, opt->advanceLargePos ) ;
               file4longAssignLong( curPos, opt->advanceLargePos ) ;
               // AS Jan 9/2014 - there was a problem here that the file might not actually match opt->advnaceReadFile if we are actually just trying to clear out the previous advanceRead...
               advFile = opt->advanceReadFile ;  // save the file that we are actually processing

               // hashVal = opt4fileHash( opt, file, curPos ) ;
               hashVal = opt4fileHash( opt, advFile, curPos ) ;
               opt->advanceLargeBufferAvail = AR4EMPTY ;
               opt->advanceReadFile = 0 ;

               for ( ;; )
               {
                  blockOn = opt4fileReturnBlock( advFile, curPos, hashVal ) ;

                  if ( blockOn == 0 )
                  {
                     blockOn = opt4fileGetBlock( opt, advFile, 0 ) ;
                     FILE4LONG offset ;
                     file4longAssignLong( offset, curPos ) ;
                     file4longSubtractLong( &offset, &advPos ) ;
                     assert5( file4longGetHi( offset ) == 0 ) ;
                     memcpy( blockOn->data, opt->advanceLargeBuffer + file4longGetLo( offset ), (unsigned)(saveBlockSize) ) ;
                     opt4blockAdd( blockOn, advFile, (unsigned)(saveBlockSize), hashVal, curPos ) ;
                     opt4blockUpgradePriorityCheck( blockOn, opt ) ;
                  }
                  else   /* update the lru status */
                     opt4listLruBottomShift( blockOn ) ;

                  file4longAdd( &curPos, saveBlockSize ) ;

                  #ifdef E4DEBUG
                     FILE4LONG cmp ;
                     file4longAssignLong( cmp, opt->advanceLargePos ) ;
                     file4longAdd( &cmp, opt->advanceLargeLen )  ;
                     if ( file4longGreaterLong( curPos, cmp ) )
                     {
                        error4( file->codeBase, e4struct, E92508 ) ;
                        return ;
                     }
                  #endif

                  FILE4LONG cmpx ;
                  file4longAssignLong( cmpx, opt->advanceLargePos ) ;
                  file4longAdd( &cmpx, opt->advanceLargeLen ) ;
                  if ( file4longEqualLong( curPos, cmpx ) )
                     break ;

                  hashVal++ ;
                  if ( (unsigned long)hashVal >= opt->numLists )
                     hashVal = opt4fileHash( opt, advFile, curPos ) ;
               }

               if ( file == advFile )  // AS Jan 9/2014 - this also had a fault that advReadFile would always be NULL so we wouldn't return at this point when we should.
               {
                  if ( numBlocks == -1 )
                  {
                     if ( posInAdvance == 1 )
                        return ;
                  }
                  else
                     if ( blocksWithin >= numBlocks )
                        return ;
               }
            }
         }
      }
   #endif

   if ( direction == -1 )
   {
      /* first do a sample to see which way to read */
      FILE4LONG returnPos ;
      file4longAssignLong( returnPos, pos ) ;
      file4longAdd( &returnPos, saveBlockSize ) ;
      blockOn = opt4fileReturnBlock( file, returnPos, opt4fileHash( opt, file, returnPos ) ) ;
      if ( blockOn != 0 ) /* read previous blocks, not next blocks */
      {
         if ( file4longGetHi( pos ) != 0 || file4longGetLo( pos ) > ( opt->bufferSize - saveBlockSize ) )
         {
            // pos -= ( opt->bufferSize - saveBlockSize ) ;
            file4longSubtract( &pos, ( opt->bufferSize - saveBlockSize ) ) ;
         }
         else
            file4longAssign( pos, 0, 0 ) ;
      }
   }

   readBlocks = 0 ;
   curBlocks = opt->maxBlocks ;

   for ( ;; )  /* may need to read more than the read-buffer worth if a large read */
   {
      if ( numBlocks != -1 )
      {
         if ( curBlocks > (unsigned int)( numBlocks - readBlocks ) )  /* don't over-read */
         {
            curBlocks = numBlocks - readBlocks ;
            numBlocks = -1 ;   /* will be done */
         }
      }
      unsigned long len = opt4fileReadToBuffer( file, opt->readBuffer, pos, (unsigned int)( curBlocks * saveBlockSize ) ) ;
      if ( len == 0 )
         return ;

      opt->readStartPos = pos ;
      opt->readFile = file ;

      /* first do the last block, in case there is a length issue */
      // curPos = pos + saveBlockSize * ( ( len - 1 ) >> opt->blockPower ) ;
      file4longAssign( curPos, saveBlockSize, 0 ) ;
      file4longMultiply( curPos, ( ( len - 1 ) >> opt->blockPower ) ) ;
      file4longAddLong( &curPos, &pos ) ;
      hashVal = opt4fileHash( opt, file, curPos ) ;
      blockOn = opt4fileReturnBlock( file, curPos, hashVal ) ;
      if ( blockOn == 0 )
      {
         blockOn = opt4fileGetBlock( opt, file, 0 ) ;
         file4longAssignLong( endPos, curPos ) ;
         file4longSubtractLong( &endPos, &pos ) ;
         assert5( file4longGetHi( endPos ) == 0 ) ;
         unsigned copyLen = (unsigned short)(len - file4longGetLo( endPos )) ;
         memcpy( blockOn->data, opt->readBuffer + file4longGetLo( endPos ), copyLen ) ;
         // FILE4LONG copyPos ;
         // file4longAssign( copyPos, copyLen, 0 ) ;
         opt4blockAdd( blockOn, file, copyLen, hashVal, curPos ) ;
         opt4blockUpgradePriorityCheck( blockOn, opt ) ;
      }

      if ( !file4longEqualLong( curPos, pos ) )
      {
         file4longSubtract( &curPos, saveBlockSize ) ;

         /* can just subtract one from hash value since it goes in order */
         for ( ;; file4longSubtract( &curPos, saveBlockSize ) )
         {
            hashVal-- ;
            if ( hashVal < 0 )
               hashVal = opt4fileHash( opt, file, curPos ) ;
            blockOn = opt4fileReturnBlock( file, curPos, hashVal ) ;
            if ( blockOn == 0 )
            {
               blockOn = opt4fileGetBlock( opt, file, 0 ) ;
               FILE4LONG offset ;
               file4longAssignLong( offset, curPos ) ;
               file4longSubtractLong( &offset, &pos ) ;
               assert5( file4longGetHi( offset ) == 0 ) ;
               memcpy( blockOn->data, opt->readBuffer + file4longGetLo( offset ), (unsigned)(saveBlockSize) ) ;
               opt4blockAdd( blockOn, file, (unsigned)(saveBlockSize), hashVal, curPos ) ;
               opt4blockUpgradePriorityCheck( blockOn, opt ) ;
            }
            else   /* update the lru status */
               opt4listLruBottomShift( blockOn ) ;
            if ( file4longEqualLong( curPos, pos ) )
            {
               #ifdef S4ADVANCE_READ
                  advanceNextRead = 1 ;
               #endif
               break ;
            }
         }
      }

      opt->readFile = 0 ;

      if ( numBlocks == -1 )
         break ;
      if ( len < opt->bufferSize )
         break ;

      readBlocks += curBlocks ;
      file4longAdd( &pos, opt->bufferSize ) ;
   }

   #ifdef S4ADVANCE_READ
      // AS Apr 15/04 - this code was never getting executed - doAdvanceRead was never -1...
      if ( opt->advanceLargeBufferAvail == AR4EMPTY && advanceNextRead == 1 && doAdvanceRead == 1 )   /* request an advance read for the next section */
      {
         if ( direction == 1 )
         {
            assert5( 0 ) ;  // I am not so sure about this code, the dual shift right just looks wrong...
            // advPos = ((unsigned long)(posIn >> opt->blockPower ) >> opt->blockPower ) + opt->bufferSize ;
            advPos = file4longShiftValueLong( advPos, opt->blockPower ) ;
            file4longAdd( &advPos, opt->bufferSize ) ;
         }
         else
         {
            // advPos = ((unsigned long)(posIn << opt->blockPower ) >> opt->blockPower ) - 2 * opt->bufferSize ;
            advPos = file4longShiftValueLong( posIn, opt->blockPower ) ;
            file4longSubtract( &advPos, 2 * opt->bufferSize ) ;
         }

         if ( file4longGreaterZero( advPos ) )
         {
            opt->advanceLargePos = advPos ;
            opt->advanceLargeLen = opt->bufferSize ;
            opt4fileReadAdvanceBuffer( opt, file, opt->advanceLargeBuffer, advPos, opt->bufferSize ) ;
         }
      }
   #endif

   return ;
}



OPT4BLOCK *opt4fileReturnBlock( FILE4 *file, FILE4LONG pos, long hashVal )
{
   OPT4CMP compare ;
   OPT4BLOCK *blockOn ;
   LIST4 *list ;

   #ifdef E4PARM_LOW
      if ( file->codeBase->opt.numBuffers == 0 || file == 0  || hashVal < 0 )
      {
         error4( 0, e4parm, E92508 ) ;
         return 0 ;
      }
      if ( (unsigned long)hashVal >= file->codeBase->opt.numLists )
      {
         error4( 0, e4parm, E92508 ) ;
         return 0 ;
      }
   #endif

   if ( file->doBuffer == 0 )
      return 0 ;

   list = &file->codeBase->opt.lists[hashVal] ;
   blockOn = (OPT4BLOCK *)l4first( list ) ;
   if ( blockOn != 0 )
   {
      compare.file = file ;
      memcpy( (void *)&compare.pos, (void *)&pos, sizeof( compare.pos ) ) ;

      for( ;; )
      {
         #ifdef S4WIN64 /* LY 00/11/03 : sizeof(compare) returning 16 */
            // AS Oct 21/04 - support for large file optimization
            if ( !c4memcmp( (void *)&blockOn->file, (void *)&compare, 16 ) )
         #else
            if ( !c4memcmp( (void *)&blockOn->file, (void *)&compare, sizeof( compare ) ) )
         #endif
         {
            #ifdef E4ANALYZE_ALL
               if ( file4longGetHi( file->len ) != -1 )  // ensure we are not beyond file length - impossible...
               {
                  FILE4LONG posCompare ;
                  file4longAssignLong( posCompare, blockOn->pos ) ;
                  file4longAdd( &posCompare, blockOn->len ) ;
                  if ( file4longGreaterLong( posCompare, file->len ) )
                     error4( file->codeBase, e4opt, E92508 ) ;
               }
            #endif
            return blockOn ;
         }
         blockOn = (OPT4BLOCK *)l4next( list, blockOn ) ;
         if ( blockOn == 0 )
            return 0 ;
      }
   }
   return 0 ;
}

#endif   /* not S4OPTIMIZE_OFF */
