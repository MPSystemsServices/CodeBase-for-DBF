/* o4opt.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifndef S4OPTIMIZE_OFF

static int opt4blockAdd( OPT4BLOCK *, FILE4 *, unsigned, long, unsigned long ) ;

static void opt4fileCancelReadSpBuffer( FILE4 *file ) ;
static OPT4BLOCK *opt4fileChooseBlock( FILE4 * ) ;
/* puts block onto bottom of lru-list and assigns the optlist to the block */
#define opt4listLruBottomPlace( o4, b4 ) ( l4add( &((o4)->list), &((b4)->lruLink) ), ( (b4)->optList = (o4) ) )
static void opt4listLruBottomShift( OPT4BLOCK * ) ;
static void opt4fileReadSpBuffer( FILE4 *, const unsigned long, int, int ) ;
static void opt4timeReset( OPT4 *, int, int ) ;
/* LY 99/06/21 : changed const long to const unsigned long in opt4fileReadFile */
#ifdef S4ADVANCE_TEST
   S4EXPORT unsigned S4FUNCTION opt4fileReadFile( FILE4 S4PTR *, const unsigned long, char S4PTR * ) ;
#else
   static unsigned opt4fileReadFile( FILE4 *, const unsigned long, char * ) ;
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

static int opt4blockAdd( OPT4BLOCK *block, FILE4 *file, unsigned blockLen, long hashVal, unsigned long position )
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
   block->pos = position ;
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
   block->pos = 0 ;
   block->file = 0 ;

   return 0 ;
}

#ifdef S4WRITE_DELAY
void S4CALL opt4writeCompletionRoutine( void *inDelay )
{
   FILE4WRITE_DELAY *delay ;

   delay = (FILE4WRITE_DELAY *)inDelay ;
   EnterCriticalSection( &delay->file->codeBase->opt.critical4optWrite ) ;
   l4add( &delay->file->codeBase->opt.delayAvail, (LINK4 *)delay->completionData ) ;
   LeaveCriticalSection( &delay->file->codeBase->opt.critical4optWrite ) ;
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
         if ( file4cmpPart( block->file->codeBase, block->data, block->file, block->pos, block->len ) != 0 )
            return error4( block->file->codeBase, e4opt, E82503 ) ;
   #endif

   if ( opt->readFile == block->file )  /* check to see if block has been temporarily read-buffered */
      if ( ( (unsigned long)block->pos < opt->readStartPos + opt->bufferSize ) && ( (unsigned long)block->pos >= opt->readStartPos ) )
         memcpy( opt->readBuffer + block->pos - opt->readStartPos, block->data, block->len ) ;

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
            if ( opt->writeCurPos != (unsigned long)block->pos )   /* not a consecutive write, so check out possibilities */
            {
               if ( opt->writeCurPos < (unsigned long)block->pos )
               {
                  if ( (unsigned long)block->pos - opt->writeCurPos < opt->blockSize )   /* partially filled block, can just extend */
                  {
                     opt->writeCurPos = (unsigned long)block->pos ;
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
         opt->writeStartPos = opt->writeCurPos = (unsigned long)block->pos ;
      }
      memcpy( opt->writeBuffer + (opt->writeCurPos - opt->writeStartPos), block->data, block->len ) ;
      opt->writeCurPos += block->len ;
      opt->writeBlockCount++ ;

      #ifdef E4ANALYZE_ALL
         if ( block->file->hasDup == 1 )
            if ( file4cmpPart( opt->writeFile->codeBase, opt->writeBuffer, opt->writeFile, opt->writeStartPos, (unsigned)(opt->writeCurPos - opt->writeStartPos) ) != 0 )
               return error4( block->file->codeBase, e4opt, E82503 ) ;
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
         if ( opt->writeFile == block->file )
            if ( opt->writeCurPos >= (unsigned long) block->pos &&
                 (opt->writeCurPos - opt->writeBlockCount * opt->blockSize ) <= (unsigned long)block->pos )
               if ( ( rc = opt4flushWriteBuffer( opt ) ) != 0 )
                  return rc ;
      #endif
      block->file->doBuffer = 0 ;
      #ifdef S4WRITE_DELAY
         if ( doDelay == 1 && opt->delayWriteBuffer != 0 )
         {
            for ( ;; )  /* wait until a block is available */
            {
               EnterCriticalSection( &opt->critical4optWrite ) ;
               delayLink = (LINK4 *)l4first( &opt->delayAvail ) ;
               if ( delayLink == 0 )
               {
                  LeaveCriticalSection( &opt->critical4optWrite ) ;
                  Sleep( 0 ) ;
               }
               else
                  break ;
            }

            delayBlock = (OPT4BLOCK *)(delayLink - 1) ;
            l4remove( &opt->delayAvail, delayLink ) ;
            LeaveCriticalSection( &opt->critical4optWrite ) ;
            memcpy( delayBlock->data, block->data, block->len ) ;

            if ( file4writeOpt( block->file, block->pos, delayBlock->data, block->len, 1, opt4writeCompletionRoutine, &delayBlock->lruLink ) != r4delay )
            {
               /* the delay links were not used, so put onto list */
               EnterCriticalSection( &opt->critical4optWrite ) ;
               l4add( &opt->delayAvail, (LINK4 *)&delayBlock->lruLink ) ;
               LeaveCriticalSection( &opt->critical4optWrite ) ;
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
      l4remove( &opt->lists[opt4fileHash( opt, block->file, (unsigned long)block->pos )], block ) ;
      block->file = 0 ;
   }

   opt4blockClear( block ) ;

/*   return block ;  */
   return 0 ;
}



static void opt4fileDeletePartialStart( FILE4 *file, const unsigned long lowPos, const unsigned long hiPos )
{
   /* purpose is to look for an optimization block, and if one is found then remove part
      of its data.  This can occur when a file is being shortened by a lenSet call and the
      file is bufferred.
   */

   /* lowPos / blockSize because position must be on a block boundary */
   OPT4 *opt = &file->codeBase->opt ;
   FILE4LONG fLong ;
   long hashVal = opt4fileHash( opt, file, lowPos ) ;
   OPT4BLOCK *blockOn = opt4fileReturnBlock( file, lowPos / file->codeBase->opt.blockSize, hashVal ) ;

   if ( blockOn )  /* block is in memory buffers , so partially delete */
   {
      if ( file4longGetLo( file->len ) <= hiPos )   /* just removing all file, so merely delete */
         blockOn->len = (unsigned)(lowPos - blockOn->pos) ;
      else  /* read the old data into the block - ignore errors since could be eof, else catch later */
      {
         file4longAssign( fLong, lowPos, 0 ) ;
         file4readInternal( file, fLong, (char *)blockOn->data + blockOn->pos - lowPos, (unsigned)(opt->blockSize - (lowPos - blockOn->pos)) ) ;
      }
   }
}



static void opt4fileDeleteBlock( FILE4 *file, const unsigned long onPos )
{
   /* looks for the block at the given pos, and if found removes it */
   /* onPos / blockSize because position must be on a block boundary */
   OPT4 *opt = &file->codeBase->opt ;

   // modify the input positoin to be on a valid block boundary
   unsigned long onPosBoundary = onPos / file->codeBase->opt.blockSize ;
   onPosBoundary *= file->codeBase->opt.blockSize ;
   OPT4BLOCK *blockOn = opt4fileReturnBlock( file, onPosBoundary, opt4fileHash( opt, file, (unsigned long)onPos ) ) ;

   if ( blockOn )  /* block in memory, so delete */
   {
      opt4blockRemove( opt, blockOn, 0 ) ;
      opt4blockLruTop( blockOn ) ;
      l4addBefore( &opt->avail, l4first( &opt->avail ), &blockOn->lruLink ) ;
   }
}



static void opt4fileDeletePartialEnd( FILE4 *file, const unsigned long onPos, const unsigned long hiPos )
{
   OPT4 *opt = &file->codeBase->opt ;
   OPT4BLOCK *blockOn = opt4fileReturnBlock( file, onPos, opt4fileHash( opt, file, onPos ) ) ;
   FILE4LONG fLong ;

   if ( blockOn )
   {
      if ( file4longGetLo( file->len ) <= hiPos )
      {
         opt4blockRemove( opt, blockOn, 0 ) ;
         opt4blockLruTop( blockOn ) ;
         l4addBefore( &opt->avail, l4first( &opt->avail ), &blockOn->lruLink ) ;
      }
      else
      {
         file4longAssign( fLong, onPos, 0 ) ;
         file4readInternal( file, fLong, blockOn->data, hiPos - blockOn->pos ) ;
      }
   }
}



static void opt4fileDeleteFromBufferredBlocks( FILE4 *file, const unsigned long lowPos, const unsigned long hiPos )
{
   unsigned long onPos ;
   OPT4 *opt = &file->codeBase->opt ;
   const unsigned long endDeletePos = hiPos + opt->blockSize - 1 ;

   #ifdef E4PARM_LOW
      if ( file == 0 || lowPos < 0 || hiPos < 0 )
      {
         error4( 0, e4parm, E92508 ) ;
         return ;
      }
   #endif

   /* first delete any partial block at the beginning of the range (i.e. the beginning of the block
      may be left intact)
   */
   if ( opt4fileHash( opt, file, lowPos ) != opt4fileHash( opt, file, lowPos + file->codeBase->opt.blockSize - 1L ) )  /* not on a block boundary, so delete partial */
   {
      opt4fileDeletePartialStart( file, lowPos, hiPos ) ;
      onPos = ( (lowPos + opt->blockSize) >> opt->blockPower ) << opt->blockPower ;
   }
   else
      onPos = lowPos ;

   /* now remove all the complete blocks that fall within the range to delete */
   while( onPos < endDeletePos )
   {
      opt4fileDeleteBlock( file, onPos ) ;
      onPos += opt->blockSize ;
   }

   /* now delete part of the last block -- i.e. at the upper part of the range.  If a partial
      remove, then it means we want to over-write the current block info with the actual
      file data (thus in effect removing the bufferred data)
   */
   onPos -= opt->blockSize ;
   if ( onPos < hiPos )
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



int opt4fileDelete( FILE4 *file, const unsigned long lowPos, const unsigned long hiPos )
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
            l4remove( &opt->lists[ opt4fileHash( opt, blockOn->file, (unsigned long)blockOn->pos )], blockOn ) ;
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
            l4remove( &opt->lists[ opt4fileHash( opt, file, (unsigned long)blockOn->pos )], blockOn ) ;
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
            if ( file4cmpPart( file->codeBase, opt->writeBuffer, file, opt->writeStartPos, (unsigned)(opt->writeCurPos - opt->writeStartPos) ) != 0 )
               return error4( file->codeBase, e4opt, E80602 ) ;
      #endif

      oldDoBuffer = file->doBuffer ;
      oldBufferWrites = file->bufferWrites ;
      file->doBuffer = 0 ;
      file->bufferWrites = 0 ;

      #ifdef S4WRITE_DELAY
         FILE4LONG tLong ;
         if ( opt->writeBuffer == opt->writeBufferActual )
         {
            opt->writeBufferActualAvail = 0 ;
            /* LY 4/28/99 : convert opt->writeStartPos to FILE4LONG */
            file4longAssign( tLong, opt->writeStartPos, 0 ) ;
            rc = file4writeDelay( file, tLong, opt->writeBuffer,
               (unsigned)(opt->writeCurPos - opt->writeStartPos), opt4largeWriteCompletionRoutine, &opt->writeBufferActualAvail ) ;
            while ( opt->delayLargeBufferAvail != 1 )  /* wait so we can use this buffer */
               Sleep( 0 ) ;
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
            file4longAssign( tLong, opt->writeStartPos, 0 ) ;
            rc = file4writeDelay( file, tLong, opt->writeBuffer, (unsigned)(opt->writeCurPos - opt->writeStartPos), opt4largeWriteCompletionRoutine, &opt->delayLargeBufferAvail ) ;
            while ( opt->writeBufferActualAvail != 1 )  /* wait so we can use this buffer */
               Sleep( 0 ) ;
            opt->writeBuffer = opt->writeBufferActual ;
         }
      #else
         FILE4LONG tLong ;
         file4longAssign( tLong, opt->writeStartPos, 0 ) ;
         rc = file4writeInternal( file, tLong, opt->writeBuffer, (unsigned)(opt->writeCurPos - opt->writeStartPos) ) ;
      #endif
      file->doBuffer = oldDoBuffer ;
      file->bufferWrites = oldBufferWrites ;
      if ( rc < 0 )
         return rc ;
      opt->writeStartPos = opt->writeCurPos = opt->writeBlockCount = 0 ;
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
         if ( block->pos == 0 || (d4file->hiPrio == 1 && dfile4recWidth( d4file ) < 4096 ) )
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

/* inlined now
long opt4fileHash( OPT4 *opt, FILE4 *file, unsigned long pos )
{
   return ( (( file->hashInit + pos ) >> opt->blockPower ) & opt->mask ) ;
}
*/

/* 06/28/96 AS --> 16-bit, required len may be > sizeof unsigned because the
   value is added to around line 880 below; therefore must use longs */
int opt4fileWrite( FILE4 *file, unsigned long pos, unsigned len, const void *data, char changed )
{
   S4LONG hashVal ;
   S4UNSIGNED_LONG lenWritten, adjustedPos ;
   unsigned readLen, lenRead, extraRead ;
   OPT4BLOCK *blockOn ;
   OPT4 *opt ;
   int doUpgradeCheck = 0 ;
   unsigned int a, b ; /* Used for min() reproduction */

   #ifdef E4PARM_LOW
      if ( file == 0 || pos < 0 || data == 0 )
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

   #ifdef S4FILE_EXTENDED
      #ifdef E4ANALYZE
         if ( (pos + len) < pos )  /* means overflow, we have exceeded 4 gig file limit */
            return error4( 0, e4parm, E92508 ) ;
      #endif
   #endif

   opt = &file->codeBase->opt ;
   lenWritten = 0 ;
   extraRead = (unsigned) ((unsigned S4LONG)((unsigned S4LONG)pos << opt->numShift ) >> opt->numShift ) ;
   adjustedPos = pos - extraRead ;
   if( len > ( (unsigned long)opt->numBlocks * (unsigned long)opt->blockSize ) )
   {
      /* case where amount to write > total bufferred length - do a piece at a time */
      adjustedPos = (S4UNSIGNED_LONG)( ((S4UNSIGNED_LONG)opt->numBlocks - 1) * (S4UNSIGNED_LONG)opt->blockSize ) ;
      for ( lenWritten = 0L ; len > lenWritten ; lenWritten += adjustedPos )
      {
         if ( ( len - lenWritten ) < adjustedPos )
            adjustedPos = len - lenWritten ;
         if ( opt4fileWrite( file, pos + lenWritten, (unsigned)adjustedPos, (char *)data + lenWritten, changed ) != (int)adjustedPos )
            return (int)lenWritten ;
      }
      return (int)lenWritten ;
   }
   len += extraRead ;

   do
   {
      hashVal = opt4fileHash( opt, file, (S4UNSIGNED_LONG)adjustedPos ) ;
      readLen = (unsigned) ((len / (unsigned)opt->blockSize) ? opt->blockSize : len) ;
      blockOn = opt4fileReturnBlock( file, adjustedPos, hashVal ) ;
      if ( blockOn == 0 )
      {
         blockOn = opt4fileGetBlock( opt, file, 0 ) ;
         #ifdef E4ANALYZE
            if ( blockOn == 0 )
               return error4( 0, e4info, E92508 ) ;
         #endif
         if ( (S4UNSIGNED_LONG)( readLen - extraRead ) < opt->blockSize && adjustedPos < file4longGetLo( file4lenLow( file ) ) )
            lenRead = opt4fileReadFile( file, adjustedPos, (char *)blockOn->data ) ;
         else
         {
            if ( file4longGetLo( file4lenLow( file ) ) >= (S4UNSIGNED_LONG) (pos + lenWritten + opt->blockSize) )   /* mark block as full to avoid file len set below... */
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
      adjustedPos += opt->blockSize ;
      if ( pos + lenWritten > blockOn->pos + (S4UNSIGNED_LONG)blockOn->len )
      {
         if ( file4longError( file->len ) == ULONG_MAX || ( file4longGetLo( file->len ) < ( pos + lenWritten ) ) ) /* file size has grown */
         {
            if ( ( blockOn->pos + (S4UNSIGNED_LONG)blockOn->len < pos )
               && ( blockOn->pos + (S4UNSIGNED_LONG)blockOn->len > blockOn->pos ) )  /* LY 00/01/14 : pos + len can > 2^32-1 */
               memset( ((char *)blockOn->data) + blockOn->len, 0,(unsigned)( pos - ( blockOn->pos + blockOn->len ) ) ) ;
            blockOn->len = (unsigned)( pos - blockOn->pos + lenWritten ) ;
            if ( file->bufferWrites == 1 )
            {
               file4longAssign( file->len, pos + lenWritten, 0 ) ;
               #ifdef E4ANALYZE
                  /* make sure the file length really has grown */
                  if ( file->fileCreated )
                     if ( file4longGetLo( u4filelength( file->hand ) ) > file4longGetLo( file->len ) )
                        return error4( file->codeBase, e4info, E92508 ) ;
               #endif
            }
         }
         else   /* we have written past our end of block, but not our block, update our block len, dummy info */
         {
            a = (unsigned) opt->blockSize ;
            b = (unsigned)( file4longGetLo( file->len ) - blockOn->pos ) ;
            blockOn->len = (a>b)?b:a ; /* minimum of a or b */
             /* AS 02/16/97 changed, else c/s MDX t5row5 failed */
         }
      }
   } while( len && blockOn->len == (unsigned)opt->blockSize ) ;

   return (unsigned) lenWritten ;
}

/* 06/28/96 AS --> 16-bit, required len may be > sizeof unsigned because the
   value is added to around line 880 below; therefore must use longs */
unsigned opt4fileRead( FILE4 *f4, unsigned long pos, void *data, unsigned len )
{
   S4LONG hashVal ;
   unsigned readLen ;
   int blocksNeeded ;
   OPT4BLOCK *blockOn ;
   DATA4FILE *d4file ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 || pos < 0 || data == 0 )
      {
         error4( 0, e4parm, E92508 ) ;
         return 0 ;
      }
   #endif

   OPT4 *opt = &f4->codeBase->opt ;
   S4UNSIGNED_LONG lenRead = 0 ;
   unsigned extraRead = (unsigned)((unsigned S4LONG)((unsigned S4LONG)pos << opt->numShift ) >> opt->numShift ) ;
   S4UNSIGNED_LONG adjustedPos = pos - extraRead ;

   if ( (unsigned S4LONG)len > ( opt->numBlocks * opt->blockSize ))
   {
      /* case where amount to read > total bufferred length - do a piece at a time */
      adjustedPos = (S4LONG)( ((S4LONG)opt->numBlocks - 1) * (S4LONG)opt->blockSize ) ;

      for ( lenRead = 0 ; len > lenRead ; lenRead += adjustedPos )
      {
         if ( (len - lenRead ) < adjustedPos )
            adjustedPos = len - lenRead ;

         /* AS 09/09/99 - was returning < actual amount read in some cases */
         /* if ( opt4fileRead( f4, pos + lenRead, (char *)data + lenRead, (unsigned)adjustedPos ) != (unsigned)adjustedPos )
               return (unsigned)lenRead ; */
         unsigned partialRead = opt4fileRead( f4, pos + lenRead, (char *)data + lenRead, (unsigned)adjustedPos ) ;
         if ( partialRead != (unsigned)adjustedPos )
            return (unsigned)lenRead + partialRead ;
      }

      #ifdef E4ANALYZE_ALL
         if ( f4->hasDup == 1 )
            if ( file4cmpPart( f4->codeBase, data, f4, pos, (unsigned)lenRead ) != 0 )
            {
               error4( f4->codeBase, e4opt, E80602 ) ;
               return 0 ;
            }
      #endif

      return (unsigned)lenRead ;
   }
   len += extraRead ;

   do
   {
      hashVal = opt4fileHash( opt, f4, (unsigned long)adjustedPos ) ;
      readLen = (unsigned) ((len / opt->blockSize) ? opt->blockSize : len ) ;
      blockOn = opt4fileReturnBlock( f4, adjustedPos, hashVal ) ;

      if ( blockOn == 0 )  /* read from disk */
      {
         /* first ensure that the file is not a temporary file that is empty */
         if ( f4->fileCreated == 0 )  /* not created, so if not in memory then empty */
            return (unsigned)0 ;

         /* if force current is on, then pre-reading will be wasted, so just skip */
         if ( opt->forceCurrent != 1 )
         {
            if ( f4->type == OPT4DBF )
            {
               d4file = (DATA4FILE *)f4->ownerPtr ;
               if ( d4file != 0 )
                  if ( d4file->hiPrio == -1 )
                  {
                     opt4fileReadSpBuffer( f4, (unsigned long)adjustedPos, -1, -1 ) ;
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
                  blockOn = opt4fileReturnBlock( f4, adjustedPos + opt->blockSize, opt4fileHash( opt, f4, (unsigned long)adjustedPos + opt->blockSize ) ) ;
                  if ( blockOn == 0 )  /* not there, so get */
                  {
                     /* check to see how many blocks we need */
                     blocksNeeded = 1 + (int)(len / readLen) ;  /* 1st block, plus any extras */
                     opt4fileReadSpBuffer( f4, (unsigned long)adjustedPos, blocksNeeded, 1 ) ;
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
      if ( readLen < (unsigned) extraRead )
         readLen = (unsigned) extraRead ;
      memcpy( (char *)data + lenRead, (char *)blockOn->data + extraRead, (unsigned)(readLen - extraRead) ) ;
      len -= readLen ;
      lenRead += readLen - extraRead ;
      extraRead = 0 ;
      adjustedPos += opt->blockSize ;
   } while( len && blockOn->len == (unsigned) opt->blockSize ) ;

   #ifdef E4ANALYZE_ALL
      if ( f4->hasDup == 1 )
         if ( file4cmpPart( f4->codeBase, data, f4, pos, (unsigned)lenRead ) != 0 )
         {
            error4( f4->codeBase, e4opt, E80602 ) ;
            return 0 ;
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
   unsigned S4FUNCTION opt4fileReadFile( FILE4 *file, const unsigned long posIn, char *buf )
#else
   static unsigned opt4fileReadFile( FILE4 *file, const unsigned long posIn, char *buf )
#endif
{
   OPT4 *opt ;
   unsigned long pos ;
   FILE4LONG fPos ;
   #ifdef S4ADVANCE_READ
      unsigned int compareLen, copyOffset, len ;
   #endif

   #ifdef E4PARM_LOW
      if ( file == 0 || posIn < 0 || buf == 0 )
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
            Sleep( 0 ) ;
         if ( opt->advanceLargeBufferAvail == AR4FULL && file == opt->advanceReadFile )
         {
            len = (unsigned)(opt->blockSize) ;

            /* if an overlap of any type occurs, then copy that part out first,
               and then take care of the remaining pieces */
            if ( pos >= opt->advanceLargePos && pos < opt->advanceLargePos + opt->advanceLargeLen )
            {
               if ( pos == opt->advanceLargePos )  /* easy case */
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
                     opt->advanceLargePos += len ;
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
                  if ( pos > opt->advanceLargePos )  /* full copy only */
                  {
                     copyOffset = pos - opt->advanceLargePos ;
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
      if ( (unsigned long)pos + opt->blockSize >= opt->writeStartPos && (unsigned long) pos < opt->writeCurPos )
         opt4flushWriteBuffer( opt ) ;

   /* 07/01/96 AS --> t4excl.c c/s failure after d4zap()
      do not exceed reading the file size if delay-write is true because
      it may still be on disk (i.e. file size may not yet be updated)
      len = file4readLow( file, pos, buf, (unsigned)(opt->blockSize) ) ;
   */

   file4longAssign( fPos, pos, 0 ) ;
   if ( file->bufferWrites != 0 )
   {
      if ( file4longError( file->len ) != ULONG_MAX )
         if ( file4longGetLo( file->len ) < pos + opt->blockSize )
         {
            unsigned amountRead = file4readLow( file, fPos, buf, (unsigned)(file4longGetLo( file->len ) - pos) ) ;
            return amountRead ;
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
static unsigned long opt4fileReadToBuffer( FILE4 *file, char *buf, unsigned long pos, unsigned int readLen )
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
   static void opt4fileReadAdvanceBuffer( OPT4 *opt, FILE4 *f4, void *buffer, long pos, unsigned int len )
   {
      long hashVal ;

      hashVal = opt4fileHash( opt, f4, (unsigned long)pos ) ;
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



static void opt4fileReadSpBuffer( FILE4 *file, const unsigned long posIn, int numBlocks, int direction )
{
   /* this function does advance reads by reading in a whole buffer at
      a time, instead of just one block worth
      will advance-read numBlocks blocks.  If numBlocks == -1, then will advance
      read the whole read buffer, if direction == -1, then it self-detects it
      if either direction or numBlocks is -1, both must be -1
   */
   unsigned long len, curPos, saveBlockSize, endPos, pos ;
   unsigned short copyPos ;
   OPT4BLOCK *blockOn ;
   OPT4 *opt ;
   long hashVal ;
   int readBlocks ;
   unsigned int curBlocks ;
   #ifdef S4ADVANCE_READ
      int posInAdvance ;  /* position to read was within the advance-read area */
      int blocksWithin ;  /* the number of blocks within the advance read once position-adjusted (i.e. the length relevanet to quantity reqd. - if numBlocks not -1 */
      int advanceNextRead, doAdvanceRead ;
      long advPos ;
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
               Sleep( 0 ) ;

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
               if ( pos >= (unsigned long)opt->advanceLargePos && pos <= opt->advanceLargePos + opt->advanceLargeLen - saveBlockSize )
               {
                  posInAdvance = 1 ;
                  if ( numBlocks != -1 )
                     blocksWithin = ( opt->advanceLargeLen + opt->advanceLargePos - pos  ) / saveBlockSize ;
               }
               else
                  posInAdvance = 0 ;
            }

            if ( file == opt->advanceReadFile || numBlocks == -1 )  /* extract the blocks from the advance-read buffer */
            {
               advPos = curPos = opt->advanceLargePos ;
               hashVal = opt4fileHash( opt, file, curPos ) ;
               opt->advanceLargeBufferAvail = AR4EMPTY ;
               opt->advanceReadFile = 0 ;

               for ( ;; )
               {
                  blockOn = opt4fileReturnBlock( file, curPos, hashVal ) ;

                  if ( blockOn == 0 )
                  {
                     blockOn = opt4fileGetBlock( opt, file, 0 ) ;
                     memcpy( blockOn->data, opt->advanceLargeBuffer + ( curPos - advPos ), (unsigned)(saveBlockSize) ) ;
                     opt4blockAdd( blockOn, file, (unsigned)(saveBlockSize), hashVal, curPos ) ;
                     opt4blockUpgradePriorityCheck( blockOn, opt ) ;
                  }
                  else   /* update the lru status */
                     opt4listLruBottomShift( blockOn ) ;

                  curPos += saveBlockSize ;

                  #ifdef E4DEBUG
                     if ( curPos > opt->advanceLargePos + opt->advanceLargeLen )
                     {
                        error4( file->codeBase, e4struct, E92508 ) ;
                        return ;
                     }
                  #endif

                  if ( curPos == opt->advanceLargePos + opt->advanceLargeLen )
                     break ;

                  hashVal++ ;
                  if ( (unsigned long)hashVal >= opt->numLists )
                     hashVal = opt4fileHash( opt, file, curPos ) ;
               }

               if ( file == opt->advanceReadFile )
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
      blockOn = opt4fileReturnBlock( file, pos+saveBlockSize, opt4fileHash( opt, file, pos+saveBlockSize ) ) ;
      if ( blockOn != 0 ) /* read previous blocks, not next blocks */
      {
         if ( pos > ( opt->bufferSize - saveBlockSize ) )
            pos -= ( opt->bufferSize - saveBlockSize ) ;
         else
            pos = 0 ;
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
      len = opt4fileReadToBuffer( file, opt->readBuffer, pos, (unsigned int)( curBlocks * saveBlockSize ) ) ;
      if ( len == 0 )
         return ;

      opt->readStartPos = pos ;
      opt->readFile = file ;

      /* first do the last block, in case there is a length issue */
      curPos = pos + saveBlockSize * ( ( len - 1 ) >> opt->blockPower ) ;
      hashVal = opt4fileHash( opt, file, curPos ) ;
      blockOn = opt4fileReturnBlock( file, curPos, hashVal ) ;
      if ( blockOn == 0 )
      {
         blockOn = opt4fileGetBlock( opt, file, 0 ) ;
         endPos = curPos - pos ;
         copyPos = (unsigned short)(len - endPos) ;
         memcpy( blockOn->data, opt->readBuffer + endPos, copyPos ) ;
         opt4blockAdd( blockOn, file, copyPos, hashVal, curPos ) ;
         opt4blockUpgradePriorityCheck( blockOn, opt ) ;
      }

      if ( curPos != pos )
      {
         curPos -= saveBlockSize ;

         /* can just subtract one from hash value since it goes in order */
         for ( ;; curPos -= saveBlockSize )
         {
            hashVal-- ;
            if ( hashVal < 0 )
               hashVal = opt4fileHash( opt, file, curPos ) ;
            blockOn = opt4fileReturnBlock( file, curPos, hashVal ) ;
            if ( blockOn == 0 )
            {
               blockOn = opt4fileGetBlock( opt, file, 0 ) ;
               memcpy( blockOn->data, opt->readBuffer + ( curPos - pos ), (unsigned)(saveBlockSize) ) ;
               opt4blockAdd( blockOn, file, (unsigned)(saveBlockSize), hashVal, curPos ) ;
               opt4blockUpgradePriorityCheck( blockOn, opt ) ;
            }
            else   /* update the lru status */
               opt4listLruBottomShift( blockOn ) ;
            if ( curPos == pos )
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
      pos += opt->bufferSize ;
   }

   #ifdef S4ADVANCE_READ
      if ( opt->advanceLargeBufferAvail == AR4EMPTY && advanceNextRead == 1 && doAdvanceRead == -1 )   /* request an advance read for the next section */
      {
         if ( direction == 1 )
            advPos = ((unsigned long)(posIn >> opt->blockPower ) >> opt->blockPower ) + opt->bufferSize ;
         else
            advPos = ((unsigned long)(posIn << opt->blockPower ) >> opt->blockPower ) - 2 * opt->bufferSize ;

         if ( advPos > 0 )
         {
            opt->advanceLargePos = advPos ;
            opt->advanceLargeLen = opt->bufferSize ;
            opt4fileReadAdvanceBuffer( opt, file, opt->advanceLargeBuffer, advPos, opt->bufferSize ) ;
         }
      }
   #endif

   return ;
}



OPT4BLOCK *opt4fileReturnBlock( FILE4 *file, unsigned long pos, long hashVal )
{
   OPT4CMP compare ;
   OPT4BLOCK *blockOn ;
   LIST4 *list ;

   #ifdef E4PARM_LOW
      if ( file->codeBase->opt.numBuffers == 0 || file == 0  || hashVal < 0 || pos < 0 )
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
            if ( !c4memcmp( (void *)&blockOn->file, (void *)&compare, 12 ) )
         #else
            if ( !c4memcmp( (void *)&blockOn->file, (void *)&compare, sizeof( compare ) ) )
         #endif
         {
            #ifdef E4ANALYZE_ALL
               if ( file4longGetHi( file->len ) != -1 )  // ensure we are not beyond file length - impossible...
                  if ( (unsigned long)(blockOn->pos + blockOn->len) > file4longGetLo( file->len ) )
                     error4( file->codeBase, e4opt, E92508 ) ;
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
