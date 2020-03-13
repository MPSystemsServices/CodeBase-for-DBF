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

/* f4write.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

#ifdef S4WINTEL
   #ifndef S4WINCE
      #include <sys\types.h>

      #ifndef __FLAT__
         typedef long off_t;
      #endif
      #if !defined( S4OFF_MULTI ) && !defined( S4IBMOS2 )
         #ifndef __TURBOC__
            #include <sys\locking.h>
            #define S4LOCKING
         #endif
         #ifdef _MSC_VER
            #include <sys\locking.h>
         #endif
      #endif
      #if !defined( S4OFF_OPTIMIZE ) && defined( E4ANALYZE_ALL )
         #include <sys\stat.h>
         #include <share.h>
         #include <fcntl.h>
      #endif
   #endif   /* LY 2002/11/12 : moved from #include <sys\types.h> above */
#endif


#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE )
   #include "zlib.h"
#endif

// AS May 24/02 - created file4createInternal for internal use to indicate file types
unsigned file4writeLowDoInternal( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len )
{
   /* returns the length written */
   #if defined( S4WIN32 ) || defined( S4MACINTOSH )
      unsigned long urc ;
   #endif
   #ifdef S4MACINTOSH
      long longPtrLen ;
   #endif

   critical4sectionVerify( &f4->critical4file ) ;

   #if defined( S4WIN32 )
      DWORD dwres ;
      DWORD err ;
      // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
      // Microsoft knowledgebase article #811693
      #ifdef S4WINCE
         for ( short numTries = 0 ;; numTries++ )
         {
      #endif
            dwres = SetFilePointer( f4->hand, file4longGetLo( pos ), file4longGetHiAddress( pos ), FILE_BEGIN ) ;
            err = GetLastError() ;
      #ifdef S4WINCE
            if ( dwres == INVALID_SET_FILE_POINTER && err != NO_ERROR )
            {
               if ( numTries > 10 )  // wait up to 5 seconds
                  break ;
               Bool5 doBreak = 0 ;
               switch( err )  // only retry on certain errors which occur with this problem.
               {
                  case ERROR_ACCESS_DENIED:
                  case ERROR_DEVICE_NOT_AVAILABLE:
                  case ERROR_DEVICE_REMOVED:
                  case ERROR_FILE_NOT_FOUND:
                     break ;
                  default:
                     doBreak = 1 ;
                     break ;
               }
               if ( doBreak )
                  break ;
               Sleep( 500 ) ;
            }
            else
               break ;
         }
      #endif
      if ( dwres == (DWORD)-1 && err != NO_ERROR  )
   #elif defined( S4MACINTOSH )
      if ( MAClseek( f4->hand, pos, 0, 1 ) != pos )
   #elif defined( S4WINDOWS )
      if ( _llseek( f4->hand, pos, 0 ) != pos )
   #elif defined( S4PALM )
      if ( FileSeek(f4->hand, pos, fileOriginBeginning ) != 0 )
   #elif defined( S4LSEEK )
      if ( f4lseek( f4->hand, pos, 0, 1 ) != pos )
   #else
      if ( lseek( f4->hand, (off_t)pos, 0 ) != (off_t)pos )
   #endif
      {
         error4describe( f4->codeBase, e4write, E90619, f4->name, "position failure", 0 ) ;
         return 0 ;
      }

   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      CODE4 *c4 = f4->codeBase ;

      if ( f4->preprocessed )
         ptr = code4filePreprocess( f4, pos, ptr, len ) ;
   #endif

   if ( ptr != 0 )
   {
      #if defined( S4WIN32 )
         BOOL blRes ;  // CS 2011/05/19 Declare outside of for loop.
         // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
         // Microsoft knowledgebase article #811693
         #ifdef S4WINCE
            for ( short numTries = 0 ;; numTries++ )
            {
         #endif
               blRes = WriteFile( f4->hand, ptr, len, &urc, NULL ) ;
         #ifdef S4WINCE
               if ( blRes == 0 )
               {
                  DWORD err = GetLastError() ;
                  if ( numTries > 10 )  // wait up to 5 seconds
                     break ;
                  Bool5 doBreak = 0 ;
                  switch( err )  // only retry on certain errors which occur with this problem.
                  {
                     case ERROR_ACCESS_DENIED:
                     case ERROR_DEVICE_NOT_AVAILABLE:
                     case ERROR_DEVICE_REMOVED:
                     case ERROR_FILE_NOT_FOUND:
                        break ;
                     default:
                        doBreak = 1 ;
                        break ;
                  }
                  if ( doBreak )
                     break ;
                  Sleep( 500 ) ;
               }
               else
                  break ;
            }
         #endif
         // AS Apr 7/04 - check the return code as well for possible problems...
         if ( blRes == 0 )
         {
            // means failed...
            DWORD err = GetLastError() ;
            #ifdef E4ANALYZE
               if ( err == 0 && urc != 0 )  // give us a chance to look at the rc...
                  urc = 0 ;
            #endif
         }
         len = urc ;

         // AS 10/16/00 - need to insert the large file marker if the file is that large now...
         #ifdef S4FILE_EXTENDED
            if ( f4->isLong == 0 )
            {
               FILE4LONG newLen ;
               file4longAssignLong( newLen, pos ) ;
               file4longAdd( &newLen, (long)len ) ;
               if ( file4longGetHi( newLen ) > 0 )
                  f4->isLong = 1 ;
            }
         #endif
      #elif defined( S4MACINTOSH )
         longPtrLen = (long)len ;
         urc = FSWrite( f4->hand, &longPtrLen, ptr ) ;
         if ( urc != 0 )  /* otherwise, len all was written, so leave as is */
            len = urc ;
      #elif defined( S4WINDOWS )
         len = (unsigned)_lwrite( f4->hand, (char *) ptr, len ) ;
      #elif defined( S4PALM )
         len = (unsigned)FileWrite(f4->hand,(void *)ptr,1,len,0);
      #else
         len = (unsigned)write( f4->hand, (char *)ptr, len ) ;
      #endif
   }

   // AS Dec 11/02 - Needed whether delay write or not
   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      if ( f4->preprocessed )
         code4filePreprocessDone( f4 ) ;
   #endif

   return len ;
}


// LY Jan 19/05 : added switches to avoid compiler errors
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
static unsigned file4writeCompressDo( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len )
{
   CODE4 *c4 = f4->codeBase ;
   critical4sectionVerify( &f4->critical4file ) ;

   COMPRESS4WRITE_HEADER *writeCompress = f4->compressInfo->writeCompress ;

   // the possibilities here are (a) we are writing to an existing spot, (b) we are writing at end of file, or a combination of the 2
   // also we may actually be writing to the uncommpressed area (e.g. the header...)
   if ( file4longLess( pos, f4->compressHeaderOffset ) )  // requesting to write into uncompressed header area
   {
      assert5( file4longGetHi( pos ) == 0 ) ;  // the header part had better be in the non-long file area

      unsigned long readPos = file4longGetLo( pos ) ;

      if ( readPos + len <= f4->compressHeaderOffset )  // all of the read is within the header area
         return file4writeLowDoInternal( f4, pos, ptr, len ) ;

      // in this case we want to write partially in header and partially in data.  Solve this by
      // writeing from the header first (physical write), then recall this function to write the rest
      unsigned physicalLen = f4->compressHeaderOffset - readPos ;
      unsigned physicalWrite = file4writeLowDoInternal( f4, pos, ptr, physicalLen ) ;
      if ( physicalWrite != physicalLen )  // could only do a partial read, we are done
         return physicalWrite ;
      // and now write the rest...
      unsigned leftToWrite = len - physicalWrite ;
      file4longAdd( &pos, physicalWrite ) ;
      assert5( file4longGetLo( pos ) == f4->compressHeaderOffset ) ;
      // AS Jan 2/02 - read Decompress, not on boundary, else doesn't get uncompressed...
      unsigned compressedWrite = file4writeCompressDo( f4, pos, (char *)ptr+physicalWrite, leftToWrite ) ;
      return physicalWrite + compressedWrite ;
   }

   // first extend the file if required, and then do the write...

   FILE4LONG fileLen = file4lenLow( f4 ) ;
   FILE4LONG finalLen ;
   file4longAssignLong( finalLen, pos ) ;
   file4longAdd( &finalLen, len ) ;
   // if ( file4longGreaterEqLong( finalLen, fileLen ) )
   if ( file4longGreaterLong( finalLen, fileLen ) )
   {
      int rc = file4lenSetLow( f4, finalLen ) ;
      if ( rc < 0 )
         return 0 ;
      // AS Jul 28/04 - do this as part of normal check with s4testing to look into a problem...
      #if defined( E4ANALYZE ) || defined( S4TESTING )
         // ensure the blocks have been extended out (was not working right)
         FILE4LONG lastBlockLong = finalLen ;
         file4longSubtract( &lastBlockLong, (f4->compressHeaderOffset) ) ;
         file4longDivide( lastBlockLong, f4->actualBlockSize ) ;
         unsigned long lastBlock = file4longGetLo( lastBlockLong ) ;
         if ( lastBlock >= writeCompress->totalBlocks )
         {
            memset( 0, '0', 4 ) ;  // force a gpf
            assert5( 0 ) ;
         }
      #endif
      // writeCompress->fileLen = finalLen ;
      // and update to disk
      // FILE4LONG writePos ;
      // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + 2 * sizeof( unsigned long ), 0 ) ;
      // file4writeLowDoInternal( f4, writePos, &writeCompress->fileLen, sizeof( writeCompress->fileLen ) ) ;  // write the updated free chain entry to disk
   }

   // now do the overwrite - break it up into the compression buffers and read in/write out each in turn...
   unsigned long lenLeft = len ;
   FILE4LONG blockOnLong = pos ;
   file4longSubtract( &blockOnLong, (f4->compressHeaderOffset) ) ;
   file4longDivide( blockOnLong, f4->actualBlockSize ) ;
   assert5( file4longGetHi( blockOnLong ) == 0 ) ;  // the offset must be sizeof long or smaller
   unsigned long blockOn = file4longGetLo( blockOnLong ) ;
   FILE4LONG maxLen ;
   file4longAssign( maxLen, 0, 0 ) ;
   // where do we really start reading
   FILE4LONG blockOffsetLong = pos ;
   file4longSubtract( &blockOffsetLong, f4->compressHeaderOffset ) ;
   unsigned long blockOffset = (unsigned long)(file4longMod( blockOffsetLong, f4->actualBlockSize )) ;

   int rc = 0 ;

   long ptrOffset = 0 ;
   Bool5 doneWrite = 0 ;  // AS Jan 16/03 - when done writing last block
   Bool5 writeEof = 0 ;   // set to true if we are writing at eof (no blocks available)
   unsigned long blockSize = writeCompress->blockSize * 1024 ;

   while ( lenLeft > 0 )
   {
      // unsigned long toCopyLen = 0 ;

      // check the existing buffer first, we may be able to use it...
      if ( blockOn >= writeCompress->totalBlocksUsed ) // eof
      {
         // in this case we are writing completely past the eof marker for the block (the block is empty because blockOn >= total used blocks)
         // therefore, the maximum amount to copy will be either the actual block size, or the lenLeft (the minimum of the 2)
         // assert5( blockOffset == 0 ) ; // I think this is right...there should be no block offset because we are writing past eof already
         unsigned long toCopyLen = f4->actualBlockSize ;
         toCopyLen = min( toCopyLen, lenLeft+blockOffset ) ;
         file4longAssign( maxLen, toCopyLen, 0 ) ;
         // unsigned long toCopyLen = f4->actualBlockSize ;
         // toCopyLen = min( toCopyLen, lenLeft ) ;
         // file4longAssign( maxLen, toCopyLen, 0 ) ;
         writeEof = 1 ;
         // and zero out the buffer to improve compression (don't keep garbage bytes around)
         file4longAssign( f4->uBufFilePos, -1, 0 ) ;
         if ( f4->uncompressBuffer != 0 )
            memset( f4->uncompressBuffer, 0, f4->actualBlockSize ) ;
      }
      else
      {
         // in this case we are writing into an existing block, possibly at the eof if it is the last block
         FILE4LONG compressOffset = compress4handlerOffsetGet( f4, blockOn ) ;

         // if write-compress is enabled and the file is opened fully shared, we need to always re-read from disk (the user
         // can enable read/write optimization with shared files to avoid this)
         Bool5 useCompressBuffer = 0 ;   // can we use the internal stored buffer?
         if ( file4longEqualLong( f4->uBufFilePos, compressOffset ) )  // means the block we want is in our temporary buffer, so use it if possible
         {
            if ( f4->compressInfo->writeCompress == 0 )
               useCompressBuffer = 1 ;
            #ifndef S4OFF_MULTI  // LY Feb 3/05
               else if ( f4->lowAccessMode != OPEN4DENY_NONE )   // if no-one else can write to the file, we can use the internal buffer
            #endif
               useCompressBuffer = 1 ;
         }

         unsigned long toCopyLen = f4->actualBlockSize ;
         toCopyLen = min( toCopyLen, lenLeft+blockOffset ) ;
         file4longAssign( maxLen, toCopyLen, 0 ) ;

         if ( useCompressBuffer )
         {
            // in this case we can just go ahead and use the compression buffer which is correctly pre-read.
            // the maximum amount to copy in this case is clearly the entire buffer size
            // unsigned long toCopyLen = f4->uBufFileLen ;
            // file4longAssign( maxLen, toCopyLen, 0 ) ;
            // file4longAssign( maxLen, f4->uBufFileLen, 0 ) ;
         }
         else
         {
            // in this case we want to load up the compress read buffer first...
            // and zero out the buffer to improve compression (don't keep garbage bytes around)
            file4longAssign( f4->uBufFilePos, -1, 0 ) ;
            if ( f4->uncompressBuffer != 0 )
               memset( f4->uncompressBuffer, 0, f4->actualBlockSize ) ;

            // toCopyLen = f4->actualBlockSize ;
            // toCopyLen = min( toCopyLen, lenLeft ) ;
            // file4longAssign( maxLen, toCopyLen, 0 ) ;
            // perform the read to stock the compression buffer - actually only need to read one byte
            FILE4LONG posRead ;
            file4longAssign( posRead, blockOn, 0 ) ;
            file4longMultiply( posRead, f4->actualBlockSize ) ;
            file4longAdd( &posRead, f4->compressHeaderOffset ) ;
            char buf[1] ;
            rc = file4readDecompressDo( f4, posRead, &buf, 1 ) ;
            if ( rc != 1 )  // eof
            {
               writeEof = 1 ;
               // in this case we are writing completely past the eof marker for the block (the block is empty because blockOn >= total used blocks)
               // therefore, the maximum amount to copy will be either the actual block size, or the lenLeft (the minimum of the 2)
               // unsigned long toCopyLen = f4->actualBlockSize ;
               // toCopyLen = min( toCopyLen, lenLeft ) ;
               // file4longAssign( maxLen, toCopyLen, 0 ) ;
            }
            else
            {
               // the maximum amount to copy in this case is clearly the entire buffer size
               // file4longAssign( maxLen, f4->uBufFileLen, 0 ) ;
            }
         }
      }

      // now copy the data into the read buffer - maxLen always == f4->actualBlockSize except in the case where we are performing a final block read, in which case maxLen == amount available
      // in the block.  In that case, there is no way we can possibly have a start offset > actual size of the available space in the buffer, unless we are requesting to read
      // past the eof, in which case we can read nothing...
      unsigned long lenToCopy ;
      if ( file4longLess( maxLen, blockOffset ) )
         lenToCopy = 0 ;
      else
      {
         FILE4LONG tempVal = maxLen ;
         file4longSubtract( &tempVal, blockOffset ) ;
         if ( file4longGreater( tempVal, (unsigned long)lenLeft ) )
            lenToCopy = lenLeft ;
         else
         {
            assert5( file4longGetHi( tempVal ) == 0 ) ;
            lenToCopy = file4longGetLo( tempVal ) ;
         }
      }
      // lenToCopy = min( lenToCopy, toCopyLen ) ;
      assert5( lenToCopy <= (unsigned long)f4->actualBlockSize ) ;
      memcpy( f4->uncompressBuffer + blockOffset, (char *)ptr + ptrOffset, lenToCopy ) ;

      unsigned long outLen = (long)(f4->actualBlockSize * 1.01 ) + 12 ;
      // and now compress it and write it back to disk...
      critical4sectionVerify( &f4->critical4file ) ;
      rc = c4compress( c4, f4->compressBuffer, &outLen, f4->uncompressBuffer, blockSize, c4getFileCompressLevel( c4 ), 0 ) ;
      switch ( rc )
      {
         case 0:
            break ;
         case Z_MEM_ERROR:
            error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress: insufficient memory", 0 ) ;
            return 0 ;
         case Z_BUF_ERROR:
            error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress: insufficient output buffer size", 0 ) ;
            return 0 ;
         case Z_DATA_ERROR:
            error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress: corrupt compressed data in file", 0 ) ;
            return 0 ;
         default:
            error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress compressed data file", 0 ) ;
            return 0 ;
      }
      COMPRESS4WRITE_ARRAY_ENTRY *entry ;
      // it may be that we have gone past the list of available array entries, in which case we need to set up a new list...
      if ( writeEof == 1 && blockOn > writeCompress->totalBlocks )
      {
         memset( 0, '0', 4 ) ;  // force a gpf
         assert5( 0 ) ;   // not supported yet - and should never happen...
      }

      entry = &(writeCompress->firstArrayBlock.writeArray[blockOn]) ;
      if ( blockOn >= writeCompress->totalBlocksUsed ) // we are into new block territory if we are >=, ensure the block is zero'd out
         memset( entry, 0, sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ) ;

      // now see if it fits into the existing block...
      if ( outLen <= entry->actualBlockLength )
      {
         // just re-use the existing space...
         assert5( outLen <= USHRT_MAX ) ;
         entry->compressedLength = (unsigned short)outLen ;
         unsigned urc = file4writeLowDoInternal( f4, entry->position, f4->compressBuffer, outLen ) ;
         if ( urc != outLen )
            return 0 ;
      }
      else
      {
         // first check the free list
         FILE4LONG nextBlockPos = writeCompress->freeChain ;
         FILE4LONG lastBlockPos ;
         file4longAssign( lastBlockPos, 0, 0 ) ;
         FILE4LONG blockAvail ;
         file4longAssign( blockAvail, 0, 0 ) ;
         COMPRESS4FREE_BLOCK freeBlock, nextBlock, lastBlock ;

         if ( !writeEof && file4longEqualZero( nextBlockPos ) )  // empty chain, add ourselves...
         {
            // if we are setting up a new block and we are writing at eof (a non-full block), we want to actually make the block
            // larger than it would otherwise be.  This is because it is expected that we will write more data to this block.
            // use the percentage of the buffer used as a basis for how much space we need...
            assert5( lenToCopy <= SHRT_MAX ) ;
            assert5( blockOffset <= SHRT_MAX ) ;
            assert5(lenToCopy + blockOffset <= SHRT_MAX ) ;
            short amountCompressed = (short)lenToCopy + (short)blockOffset ;
            // if ( amountCompressed < f4->actualBlockSize )
            // {
            //
            // }
            freeBlock.nextPos = writeCompress->freeChain ;
            freeBlock.blockLength = entry->actualBlockLength ;
            writeCompress->freeChain = entry->position ;
            file4writeLowDoInternal( f4, writeCompress->freeChain, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;  // write the free block to disk
            // file4writeInternal( f4, writeCompress->freeChain, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;  // write the free block to disk
            FILE4LONG writePos ;
            // LY Jul 23/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
            // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ), 0 ) ;
            file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)(&writeCompress->freeChain) - (unsigned long)writeCompress, 0 ) ;
            file4writeLowDoInternal( f4, writePos, &writeCompress->freeChain, sizeof(writeCompress->freeChain) ) ;  // write the updated free chain entry to disk
            // file4writeInternal( f4, writePos, &writeCompress->freeChain, sizeof(writeCompress->freeChain) ) ;  // write the updated free chain entry to disk
         }
         else
         {
            Bool5 placedAvailBlock = 0 ;   // have we updated the free chain with the block we had?
            if ( writeEof == 1 )
               placedAvailBlock = 1 ;  // no block to place

            for ( ;; )
            {
               if ( file4longEqualZero( nextBlockPos ) )   // means we have reached the end of the chain and didn't find a free block with enough space
               {
                  if ( placedAvailBlock == 0 )  // put the freed block at the end of the file
                  {
                     if ( file4longEqualZero( lastBlockPos ) )  // means put at the top of the chain
                     {
                        file4longAssign( freeBlock.nextPos, 0, 0 ) ;
                        freeBlock.blockLength = entry->actualBlockLength ;
                        writeCompress->freeChain = entry->position ;
                        file4writeLowDoInternal( f4, writeCompress->freeChain, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                        FILE4LONG writePos ;
                        // LY Sep 17/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
                        // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ), 0 ) ;
                        file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)(&writeCompress->freeChain) - (unsigned long)writeCompress, 0 ) ;
                        file4writeLowDoInternal( f4, writePos, &writeCompress->freeChain, sizeof( writeCompress->freeChain ) ) ;  // write the updated free chain entry to disk
                     }
                     else
                     {
                        file4longAssign( freeBlock.nextPos, 0, 0 ) ;
                        freeBlock.blockLength = entry->actualBlockLength ;
                        lastBlock.nextPos = entry->position ;
                        file4writeLowDoInternal( f4, lastBlock.nextPos, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                        file4writeLowDoInternal( f4, lastBlockPos, &lastBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                     }
                  }
                  break ;
               }

               // read in the next free block
               assert5( file4longLessEqLong( nextBlockPos, u4filelength( f4->hand ) ) ) ;
               if ( file4readOnBoundary( f4, nextBlockPos, &nextBlock, sizeof( COMPRESS4FREE_BLOCK ) ) != sizeof( COMPRESS4FREE_BLOCK ) )
               {
                  error4( c4, e4info, E90619 ) ;
                  return 0 ;
               }
               assert5( file4longLessEqLong( nextBlock.nextPos, u4filelength( f4->hand ) ) ) ;

               if ( file4longEqualZero( blockAvail ) ) // if blockAvail 0, means we are still looking for a free location with enough space
               {
                  if ( nextBlock.blockLength >= outLen )  // the next free block does have enough space
                  {
                     blockAvail = nextBlockPos ;
                     // also remove this block from the free chain
                     nextBlockPos = nextBlock.nextPos ;
                     assert5( file4longLessEqLong( nextBlockPos, u4filelength( f4->hand ) ) ) ;
                     // if lastBlockPos is 0, it means we are on the first member of the free list block
                     if ( file4longEqualZero( lastBlockPos ) )  // means put at the top of the chain
                     {
                        writeCompress->freeChain = nextBlock.nextPos ;
                        FILE4LONG writePos ;
                        // LY Sep 17/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
                        // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ), 0 ) ;
                        file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)(&writeCompress->freeChain) - (unsigned long)writeCompress, 0 ) ;
                        file4writeLowDoInternal( f4, writePos, &writeCompress->freeChain, sizeof( writeCompress->freeChain ) ) ;  // write the updated free chain entry to disk
                     }
                     else
                     {
                        lastBlock.nextPos = nextBlock.nextPos ;
                        file4writeLowDoInternal( f4, lastBlockPos, &lastBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                     }
                     if ( placedAvailBlock )
                        break ;
                     continue ;   // we removed this block, so go forward with the next one
                  }
               }

               if ( !placedAvailBlock && nextBlock.blockLength >= entry->actualBlockLength )  // place the old block
               {
                  if ( file4longEqualZero( lastBlockPos ) )  // means put at the top of the chain
                  {
                     freeBlock.nextPos = writeCompress->freeChain ;
                     freeBlock.blockLength = entry->actualBlockLength ;
                     writeCompress->freeChain = entry->position ;
                     file4writeLowDoInternal( f4, writeCompress->freeChain, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                     FILE4LONG writePos ;
                     // LY Sep 17/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
                     // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ), 0 ) ;
                     file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)(&writeCompress->freeChain) - (unsigned long)writeCompress, 0 ) ;
                     file4writeLowDoInternal( f4, writePos, &writeCompress->freeChain, sizeof( writeCompress->freeChain ) ) ;  // write the updated free chain entry to disk
                  }
                  else  // need to insert into the list
                  {
                     freeBlock.nextPos = lastBlock.nextPos ;
                     assert5( file4longLessEqLong( freeBlock.nextPos, u4filelength( f4->hand ) ) ) ;
                     freeBlock.blockLength = entry->actualBlockLength ;
                     lastBlock.nextPos = entry->position ;
                     assert5( file4longLessEqLong( lastBlock.nextPos, u4filelength( f4->hand ) ) ) ;
                     file4writeLowDoInternal( f4, lastBlock.nextPos, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                     file4writeLowDoInternal( f4, lastBlockPos, &lastBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                  }
                  placedAvailBlock = 1 ;
                  if ( !file4longEqualZero( blockAvail ) ) // done
                     break ;
               }

               lastBlockPos = nextBlockPos ;
               assert5( sizeof( lastBlock ) == sizeof( nextBlock ) ) ;
               memcpy( &lastBlock, &nextBlock, sizeof( lastBlock ) ) ;
               nextBlockPos = nextBlock.nextPos ;
            }
         }

         if ( file4longEqualZero( blockAvail ) ) // need to append a new block at the end of the file
            blockAvail = u4filelength( f4->hand ) ;

         entry->position = blockAvail ;
         assert5( outLen <= USHRT_MAX ) ;
         entry->actualBlockLength = entry->compressedLength = (unsigned short)outLen ;
         unsigned urc = file4writeLowDoInternal( f4, entry->position, f4->compressBuffer, outLen ) ;

         // and we also need to update the array information to disk...
         // first find the location of the array information...
         COMPRESS4WRITE_BLOCK *writeBlock = &(writeCompress->firstArrayBlock) ;
         FILE4LONG writeArrayPosition ;
         file4longAssign( writeArrayPosition, f4->compressHeaderOffset + sizeof( COMPRESS4WRITE_HEADER ), 0 ) ;  // for the first entry, the array info starts after the write header (fully written to disk)
         unsigned long arrayOffset = blockOn ;

         for( unsigned long entryCount = 0 ;; )
         {
            if ( writeBlock == 0 )  // corruption
            {
               error4( c4, e4info, E90619 ) ;
               return 0 ;
            }

            entryCount += writeBlock->numEntries ;
            if ( blockOn < entryCount )
            {
               // found the array, so update the position - we want to write out the actual entry used
               // how do we find that location?  We need to add the location of the array to the base write position
               file4longAdd( &writeArrayPosition, arrayOffset * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ) ;
               unsigned urc = file4writeLowDoInternal( f4, writeArrayPosition, entry, sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ) ;
               if ( blockOn >= writeCompress->totalBlocksUsed )  // update the block count as well
               {
                  unsigned long numToAdd = blockOn - writeCompress->totalBlocksUsed + 1 ;
                  writeCompress->totalBlocksUsed += numToAdd ;
                  FILE4LONG writePos ;
                  // LY Jul 30/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
                  //file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + sizeof( unsigned long ), 0 ) ;
                  file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)&writeCompress->totalBlocksUsed - (unsigned long)writeCompress, 0 ) ;
                  file4writeLowDoInternal( f4, writePos, &writeCompress->totalBlocksUsed, sizeof( writeCompress->totalBlocksUsed ) ) ;  // write the updated free chain entry to disk
                  assert5( writeCompress->totalBlocksUsed == blockOn + 1 ) ;
                  // it is ok if the total blocks is much less than the file length because when optimization is used the internal
                  // file length may actually be much larger, and we are just now bufferring this to disk here.
                  // #if defined( E4ANALYZE ) || defined( S4TESTING )
                  //    // verify that we never have a situation where the # blocks goes outside of the file length
                  //    FILE4LONG totalBlocks ;
                  //    file4longAssign( totalBlocks, writeCompress->totalBlocksUsed, 0 ) ;
                  //    file4longMultiply( totalBlocks, f4->actualBlockSize ) ;
                  //    file4longSubtract( &totalBlocks, f4->actualBlockSize-1) ;
                  //    #ifdef S4TESTING
                  //       if ( file4longLessLong( writeCompress->fileLen, totalBlocks ) )
                  //          memcpy( 0, "junk", 4 ) ;
                  //    #endif
                  //    assert5( file4longGreaterEqLong( writeCompress->fileLen, totalBlocks ) ) ;
                  // #endif
               }
               break ;
            }
            writeArrayPosition = writeBlock->nextBlock ;
            file4longAdd( &writeArrayPosition, 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ) ) ; // position to after the COMPRESS4WRITE_BLOCK
            arrayOffset -= writeBlock->numEntries ;
            writeBlock = writeBlock->nextBlockPtr ;
         }
      }

      ptrOffset += lenToCopy ;
      blockOffset = 0 ;   // future reads will start from the start of the buffer
      lenLeft -= lenToCopy ;
      blockOn++ ;
      if ( doneWrite == 1 )
      {
         f4->physicalFileLen = u4filelength( f4->hand ) ;  // the actual file length (compressed)
         return ptrOffset ;
      }
   }

   if ( rc < 0 )
      return 0 ;

   f4->physicalFileLen = u4filelength( f4->hand ) ;  // the actual file length (compressed)

   return len ;
}


int file4compressLock( FILE4 *f4, Bool5 *requiresUnlock )
{
   // lock a byte in the file so only 1 user can access the file at a time
   // if an unlock is required based on this call, requiresUnlock is set to true.  file4compresUnlock()
   // should only be called if requiresUnlock returns true from this function to avoid possible multi-user
   // problems.

   CODE4 *c4 = f4->codeBase ;
   critical4sectionVerify( &f4->critical4file ) ;

   #ifndef S4OFF_MULTI  // LY Feb 3/05
      if ( f4->lowAccessMode == OPEN4DENY_NONE && f4->compressInfo->isLocked == 0 && f4->compressInfo->writeCompress != 0 )   // others can access file, so must lock
         *requiresUnlock = 1 ;
      else
   #endif
      *requiresUnlock = 0 ;

   FILE4LONG lockPosition ;
   // lock the file during the write process - wait forever for this lock since it is only held for the short duration of reading and
   // writing

   if ( *requiresUnlock )   // others can access file, so must lock
   {
      int oldAttempts = c4->lockAttempts ;
      if ( c4->largeFileOffset != 0 )  // we need to have a large lock position to avoid overlocking with file locking
         file4longAssign( lockPosition, L4LOCK_POS_COMPRESS, c4->largeFileOffset * 2 ) ;
      else
         file4longAssign( lockPosition, L4LOCK_POS_COMPRESS, 0 ) ;
      c4->lockAttempts = -1 ;
      int rc = file4lockInternal( f4, file4longGetLo( lockPosition ), file4longGetHi( lockPosition ), 1L, 0 ) ;
      if ( rc != 0 )
      {
         *requiresUnlock = 0 ;  // the lock failed, so don't need to unlock
         return rc ;
      }
      f4->compressInfo->isLocked = 1 ;
      c4->lockAttempts = oldAttempts ;
      // also read an update of the header information in case it has changed based on access by another user
      // in particular read total blocks, total blocks used, file len, and free chain
      FILE4LONG readPos ;
      // LY Sep 17/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
      // file4longAssign( readPos, f4->compressHeaderOffset + 3 * sizeof( short ), 0 ) ;
      file4longAssign( readPos, f4->compressHeaderOffset + (unsigned long)(&f4->compressInfo->writeCompress->totalBlocks) - (unsigned long)f4->compressInfo->writeCompress, 0 ) ;
      file4readOnBoundary( f4, readPos, &f4->compressInfo->writeCompress->totalBlocks,  2 * sizeof( unsigned long ) + 2 * sizeof( FILE4LONG ) ) ;  // read the updated free chain entry to disk
   }

   return 0 ;
}



int file4compressUnlock( FILE4 *f4 )
{
   // unlocks byte for compression multi-user locking
   critical4sectionVerify( &f4->critical4file ) ;
   FILE4LONG lockPosition ;
   if ( f4->codeBase->largeFileOffset != 0 )  // we need to have a large lock position to avoid overlocking with file locking
      file4longAssign( lockPosition, L4LOCK_POS_COMPRESS, f4->codeBase->largeFileOffset * 2 ) ;
   else
      file4longAssign( lockPosition, L4LOCK_POS_COMPRESS, 0 ) ;

   int rc = file4unlockInternal( f4, file4longGetLo( lockPosition ), file4longGetHi( lockPosition ), 1L, 0 ) ;
   f4->compressInfo->isLocked = 0 ;
   return rc ;
}
#endif /* S4CLIENT */



// LY Jan 19/05 : added switches, changed #if-else order, to avoid compiler errors
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   // AS Jun 28/04 - support for compressed writing
   static unsigned file4writeCompress( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len )
   {
      CODE4 *c4 = f4->codeBase ;
      if ( f4->compressInfo == 0 ) // file is not compressed
         return file4writeLowDoInternal( f4, pos, ptr, len ) ;

      Bool5 requiresUnlock = 0 ;
      if ( file4compressLock( f4, &requiresUnlock ) != 0 )
         return 0 ;

      unsigned resultLen = file4writeCompressDo( f4, pos, ptr, len ) ;

      if ( requiresUnlock )   // others can access file, so must lock
         file4compressUnlock( f4 ) ;

      return resultLen ;
   }
#else
   #define file4writeCompress( f4, pos, ptr, len ) file4writeLowDoInternal( (f4), (pos), (ptr), (len) )
#endif



static unsigned file4writeLowDo( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len )
{
   unsigned urc ;
   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      critical4sectionEnter( &f4->critical4file ) ;
   #endif

   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      CODE4 *c4 = f4->codeBase ;
      short blockSize = 0 ;
      if ( f4->preprocessed == 1 )
         blockSize = code4getPreprocessBlockSize( c4 ) ;
      if ( f4->preprocessed == 1 && blockSize > 1 )  // are we preprocessing in a block boundary?
      {
         // we may need to adjust due to block optimization.  If required, read from the file to
         // fill in buffer for writing
         short posMod = (short)file4longMod( pos, blockSize ) ;
         FILE4LONG writePos ;
         unsigned writeLen = len ;
         short lenMod = 0 ;  // total modifications to length read
         file4longAssignLong( writePos, pos ) ;
         if ( posMod != 0 )
         {
            // need to adjust the start to write position.
            assert5( file4longGetLo( writePos ) >= (unsigned short)posMod ) ;
            file4longSubtract( &writePos, posMod ) ;
            writeLen += posMod ;
            lenMod += posMod ;
         }
         short curLenMod = writeLen % blockSize ;  // mod of the length that has been adjusted by posMod if applicable
         if ( curLenMod != 0 )
         {
            curLenMod = blockSize - curLenMod ;   // offset length to reach end of block
            // need to adjust the read length
            lenMod += curLenMod ;
            writeLen += curLenMod ;
         }
         if ( lenMod == 0 )
            urc = file4writeCompress( f4, pos, ptr, len ) ;
         else
         {
            // need to use read buffer to handle increased size
            if ( c4->filePreprocessBufferLen < writeLen )  // realloc
            {
               if ( c4->filePreprocessBuffer != 0 )
               {
                  u4free( c4->filePreprocessBuffer ) ;
                  c4->filePreprocessBuffer = 0 ;
                  c4->filePreprocessBufferLen = 0 ;
               }

               assert5( writeLen > 0 ) ;
               c4->filePreprocessBuffer = (char *)u4allocFree( c4, writeLen ) ;
               if ( c4->filePreprocessBuffer == 0 )
               {
                  // AS Dec 11/02 - Renamed for clarification
                  #ifdef S4DELAY_WRITE_MT
                     critical4sectionLeave( &f4->critical4file ) ;
                  #endif
                  return 0 ;
               }
               c4->filePreprocessBufferLen = writeLen ;
            }
            assert5( writeLen - lenMod == len ) ;  // initial length amount should match
            // now, first lets read the existing data into the buffer - note that if we cannot read due to
            // insufficient length this is an error since the file the does not end on the proper boundary.
            assert5( writeLen % blockSize == 0 ) ;
            urc = file4readLow( f4, writePos, c4->filePreprocessBuffer, writeLen ) ;
            if ( urc != writeLen )
            {
               // AS Jan 17/03 - with generic file preprocessing, it is possible that we are writing past the
               // end of file, in which case the read may not have read completely.  Check length first
               FILE4LONG len = file4lenLow( f4 ) ;
               // it is ok if writePos >= file length (writing past eof )
               if ( file4longLessLong( writePos, len ) )
               {
                  file4longSubtractLong( &len, &writePos ) ;
                  long maxReadPossible ;
                  if ( file4longGetHi( len ) != 0 )  // value is too large, error
                     maxReadPossible = urc + 1 ;  // ensure a mismatch
                  else
                     maxReadPossible = file4longGetLo( len ) ;
                  if ( urc != (unsigned)maxReadPossible )
                  {
                     // AS Dec 11/02 - Renamed for clarification
                     #ifdef S4DELAY_WRITE_MT
                        critical4sectionLeave( &f4->critical4file ) ;
                     #endif
                     error4describe( c4, e4info, E90619, f4->name, "read fail", 0 ) ;
                     return 0 ;
                  }
               }
            }
            // copy the data we are writing into the buffer we just read
            memcpy( (char *)c4->filePreprocessBuffer + posMod, ptr, writeLen - lenMod ) ;
            // and write the buffer (which is now on the correct boundaries) back out.
            urc = file4writeCompress( f4, writePos, c4->filePreprocessBuffer, writeLen ) ;
            urc = min ( urc, (writeLen - lenMod ) ) ;  // adjust output length to not include extra read for decryption only
            // don't worry if we over copy into the 'ptr' buffer from the encrypt buffer
         }
      }
      else
   #endif
         urc = file4writeCompress( f4, pos, ptr, len ) ;

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      critical4sectionLeave( &f4->critical4file ) ;
   #endif
   return urc ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
/*AS 06/20/97 doFlush paramater added otherwise delay write getting in endless loop */
int file4writeLow( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len, const int checkDelayList, const int checkAdvanceList, const int doFlush )
{
   unsigned urc ;
   #ifdef S4WRITE_DELAY
      FILE4WRITE_DELAY *writeDelay ;
      LINK4 *delayLink ;
      FILE4LONG tLong, tLong2, tLong3 ;
      unsigned posShort ;
   #endif

   CODE4 *c4 = f4->codeBase ;

   if ( f4->isReadOnly )
      return error4( c4, e4write, E80606 ) ;

   #ifdef S4ADVANCE_READ
      /* take care of over-writing advance-read buffers where appropriate */
      if ( c4->advanceReadsEnabled
         // AS Apr 13/04 - support for optimizing large files
         // #ifdef S4FILE_EXTENDED
         //    && ( f4->isLong == 0 ) && file4longGetHi( pos ) == 0
         // #endif
         )
         // AS Apr 13/04 - support for optimizing large files
         if ( f4->hasHadPendingAdvanceRead )  /* for efficiency, avoid for non-advance-read files */
            file4advanceReadWriteOver( f4, pos, len, ptr, 1 ) ;
   #endif

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      critical4sectionEnter( &f4->critical4file ) ;
   #endif

   /* now flush the transaction file if a data file */
   #if !defined( S4OFF_TRAN ) && !defined( S4OFF_OPTIMIZE )
      #ifdef S4SERVER
         // AS Jul 17/03 - c4->server may be null.  This happens if an error happens
         // during shutdown on server and we are writing out to the error log, and no
         // transfile exists

         if ( c4->server != 0 )
         {
            TRAN4FILE *t4 = c4->server->c4trans.transFile ;
            // AS May 30/03 - enable/disable log flushing - default is enabled. - improves performance (esp. ODBC), maybe cannot recover from crash.
            if ( f4->type == OPT4DBF && t4 != 0 && t4->needsFlushing )
            {
               t4->needsFlushing = 0 ;
               if ( c4->logFlush )
               {
                  if ( t4->primary.isDisabled == 0 )
                     file4flush( &(t4->primary.file) ) ;
                  if ( t4->backup.validState )
                     file4flush( &(t4->backup.file) ) ;
              }
            }
         }
      #else
         TRAN4FILE *t4 = code4trans( c4 )->c4trans->transFile ;
         if ( f4->type == OPT4DBF && (t4 != 0 ) && t4->needsFlushing )
         {
            t4->needsFlushing = 0 ;
            file4flush( &(t4->file) ) ;
         }
      #endif
   #endif

   #ifdef S4WRITE_DELAY
      if ( checkDelayList /* check to see if write request covers delayed areas */
         // AS Apr 13/04 - support for optimizing large files
         // #ifdef S4FILE_EXTENDED
         //    && f4->isLong == 0
         // #endif
         )
         if ( l4numNodes( &f4->delayWriteFileList ) != 0 )
         {
            #ifdef E4ANALYZE
               if ( file4longGetHi( pos ) != 0 )
               {
                  #ifdef S4DELAY_WRITE_MT
                     critical4sectionLeave( &f4->critical4file ) ;
                  #endif
                  return error4( c4, e4result, E80606 ) ;
               }
            #endif
            posShort = file4longGetLo( pos ) ;
            // AS 04/25/00 --> critical sectioning missing...
            critical4sectionEnter( &c4->critical4delayWriteList ) ;
            for ( delayLink = (LINK4 *)l4first( &f4->delayWriteFileList ) ;; )
            {
               if ( delayLink == 0 )
                  break ;
               writeDelay = (FILE4WRITE_DELAY *)(delayLink - 1 ) ;
               delayLink = (LINK4 *)l4next( &f4->delayWriteFileList, delayLink ) ;

               /* LY 4/27/99 : replaced binary operators with file4long*** */
               file4longAssignLong( tLong2, writeDelay->pos ) ;
               file4longAdd( &tLong2, writeDelay->len ) ;
               if ( file4longLessEq( tLong2, posShort ) )  /* outside of block */
                  continue ;
               file4longAssign( tLong2, posShort + (long) len, 0 ) ;
               if ( file4longLessEqLong( tLong2, writeDelay->pos ) ) /* outside of block */
                  continue ;

               /* now, if the delay piece belongs in the buffer, then read all
                  the info before the delay piece, copy the delay piece over,
                  and read all the info after the delay piece */
               while ( writeDelay->usageFlag == r4inUse )  /* is being written to disk, just wait until it is done... */
               {
                  // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
                  // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
                  u4sleep( c4 ) ;
               }

               if ( writeDelay->usageFlag == r4finished ) /* is written to disk, so can just ignore */
                  continue ;

               /* if the entire block is within the range, then can just delete
                  it, otherwise must write it now (note that, due to constant
                  contents of the delay-write buffer, cannot copy into it
                  directly */

               /* LY 4/27/99 : replaced binary operators with file4long*** */
               file4longAssign( tLong2, posShort, 0 ) ;
               file4longAssignLong( tLong3, writeDelay->pos) ;
               file4longAdd( &tLong3, writeDelay->len ) ;
               if ( file4longLessEqLong( tLong2, writeDelay->pos ) && file4longLessEq( tLong3, posShort + len ) ) /* remove */
                  writeDelay->status = 0 ;
               else /* otherwise must write the block now, and then continue */
               {
                  file4longAssignLong( tLong, writeDelay->pos ) ;
                  writeDelay->status = file4writeLow( writeDelay->file, tLong, writeDelay->data, writeDelay->len, 0, 1, 0 ) ;
               }

               writeDelay->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
               l4remove( &c4->delayWriteList, writeDelay ) ;
               l4remove( &f4->delayWriteFileList, &writeDelay->fileLink ) ;
               writeDelay->completionRoutine( writeDelay ) ;
               mem4free( c4->delayWriteMemory, writeDelay ) ;
            }
            // AS 04/25/00 --> critical sectioning missing...
            critical4sectionLeave( &c4->critical4delayWriteList ) ;
         }
   #endif

   urc = file4writeLowDo( f4, pos, ptr, len ) ;

   #ifdef S4DELAY_WRITE_MT
      critical4sectionLeave( &f4->critical4file ) ;
   #endif

   if ( urc != len )
      return error4describe( c4, e4write, E90619, f4->name, "failed to write expected length", 0 ) ;

   #ifndef S4OFF_MULTI
      /* doFlush is false in write-delay scenario where this is not relevant (i.e. gets
         flushed by non-delay thread) */
      if ( f4->codeBase->fileFlush && doFlush )
         file4flush( f4 ) ;
   #endif

   return 0 ;
}



#ifdef E4ANALYZE
   static long g_file_count = 0 ;
#endif


#ifndef S4OFF_OPTIMIZE
#ifdef P4ARGS_USED
   #pragma argsused
#endif
// AS Apr 13/04 - support for optimizing large files
#ifdef S4WRITE_DELAY
   int file4writeOpt( FILE4 *f4, FILE4LONG pos, const void *ptr, const unsigned len, int doDelay, S4DELAY_FUNCTION *completionRoutine, void *completionData )
#else
   int file4writeOpt( FILE4 *f4, FILE4LONG pos, const void *ptr, const unsigned len, int doDelay, void *completionRoutine, void *completionData )
#endif
{
   FILE4LONG tLong ;
   #ifndef S4OFF_OPTIMIZE
      long writeAmount ;
      #ifdef E4ANALYZE_ALL
         char buf[512] ;
         long bufWritePos, bufWriteLen ;
      #endif
   #endif

   CODE4 *c4 = f4->codeBase ;
   int rc = 0 ;

   if ( f4->doBuffer )
   {
      #ifdef S4OPTIMIZE_STATS
         DATA4 *stat = c4->statusDbf ;
         if ( stat != 0 )  /* track stats */
         {
            if ( f4 != &stat->dataFile->file )  /* don't do for the stat file! */
            {
               if ( d4appendStart( stat, 0 ) == 0 )
               {
                  f4assignChar( c4->typeFld, 'W' ) ;  /* high-level */
                  f4assign( c4->fileNameFld, f4->name ) ;
                  f4assignLong( c4->offsetFld, pos ) ;
                  f4assignLong( c4->lengthFld, len ) ;
                  d4append( stat ) ;
               }
            }
         }
      #endif

      writeAmount = (long)opt4fileWrite( f4, pos, len, ptr, f4->bufferWrites ) ;
      assert5( len < LONG_MAX ) ;  // opt4fileWrite takes an 'int' only, and returns an int, so len must be less than this or problems may ensue
      if ( writeAmount != (long)len )
         return error4describe( c4, e4write, E90619, f4->name, 0, 0 ) ;
   }

   if ( f4->doBuffer == 0 || f4->writeBuffer == 0 || f4->bufferWrites == 0 )
   {
      // AS Sep 3/03 - Was not properly closing temporary in-memory files.  Change here so we can track
      // when those files are being closed (in which case we do not want to create them or flush them to disk
      // at all)
      if ( f4->closingTempFile == 1 )  // don't bother writing in this case.
         return 0 ;
      if ( f4->fileCreated == 0 )
      {
         c4->opt.forceCurrent = 1 ;
         #ifdef S4CB51
            file4temp( f4, c4, (char *)f4->name, 1 );
         #else
            // AS May 24/02 - created file4createInternal for internal use to indicate file types
            file4tempLow( f4, c4, 1, 1, NULL, f4->type ) ;
         #endif
         c4->opt.forceCurrent = 0 ;
      }

      #ifdef E4ANALYZE_ALL
         if ( f4->hasDup == 1 )
         {
            assert5( file4longGetHi ( pos ) == 0 ) ;  // we don't support this analysis with large files
            if ( file4longLessLong( file4lenLow( f4 ), pos ) )
            {
               /* need to null out any extra file contents in order for file verification for optimization
                  to work properly */
               memset( buf, 0, sizeof( buf ) ) ;
               while ( file4longLessLong( file4lenLow( f4 ), pos ) )
               {
                  bufWritePos = file4longGetLo( file4lenLow( f4 ) ) ;
                  bufWriteLen = (file4longGetLo( pos ) - bufWritePos) ;
                  if ( bufWriteLen > (long)sizeof( buf ) )
                     bufWriteLen = (long)sizeof( buf ) ;
                  file4longAssign( tLong, bufWritePos, 0 ) ;
                  if ( file4writeInternal( f4, tLong, buf, (int)bufWriteLen ) != 0 )
                     break ;
               }
            }
         }
      #endif  /* E4ANALYZE_ALL */

      #ifdef S4OPTIMIZE_STATS
         DATA4 *stat = c4->statusDbf ;
         if ( stat != 0 )  /* track stats */
         {
            if ( f4 != &stat->dataFile->file )  /* don't do for the stat file! */
            {
               if ( d4appendStart( stat, 0 ) == 0 )
               {
                  f4assignChar( c4->typeFld, 'X' ) ;  /* low-level */
                  f4assign( c4->fileNameFld, f4->name ) ;
                  f4assignLong( c4->offsetFld, pos ) ;
                  f4assignLong( c4->lengthFld, len ) ;
                  d4append( stat ) ;
               }
            }
         }
      #endif

      /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
      file4longAssignLong( tLong, pos ) ;

      #ifdef S4WRITE_DELAY
         if ( doDelay == 1
            // AS Apr 13/04 - support for optimizing large files
            // #ifdef S4FILE_EXTENDED
            //    && f4->isLong == 0
            // #endif
            && file4longEqualZero( pos ) )
            rc = file4writeDelay( f4, tLong, ptr, len, completionRoutine, completionData ) ;
         else
      #endif
      {
         #ifdef E4ANALYZE
            g_file_count++ ;
         #endif
         rc = file4writeLow( f4, tLong, ptr, len, 1, 1, 1 ) ;
      }
   }

   #ifdef E4ANALYZE_ALL
      if ( f4->hasDup == 1 )
      {
         if ( f4->doBuffer == 1 || f4->link.n == 0 )
            if ( file4writePart( ptr, f4, file4longGetLo( pos ), len ) != 0 )
               return error4( c4, e4opt, E80602 ) ;
      }
   #endif

   return rc ;
}
#endif /* S4OFF_OPTIMIZE */



int S4FUNCTION file4write( FILE4 *f4, const long pos, const void *ptr, const unsigned len )
{
   FILE4LONG fpos ;

   /* LY July 07/03 : changed from 0 to 0L for Linux compiler */
   file4longAssign( fpos, pos, 0L ) ;

   return file4writeInternal( f4, fpos, ptr, len ) ;
}

int file4writeInternal( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len )
{
   CODE4 *c4 ;

   #ifdef E4PARM_HIGH
      if ( f4 == 0 )
         return error4( 0, e4parm_null, E90619 ) ;
      // AS Feb 28/02 - Was not considering case where the CODE4 member was null
      if ( f4->codeBase == 0 || file4longError( pos ) < 0 || ( ptr == 0 && len > 0 ) )
         return error4( f4->codeBase, e4parm, E90619 ) ;
      // AS Sep 3/03 - may be invalid if temporary
      #ifdef S4CLIENT
         if ( f4->hand == INVALID4HANDLE )
      #else
         if ( f4->hand == INVALID4HANDLE && ( f4->isTemporary == 0
            #ifndef S4OFF_OPTIMIZE  // LY Dec 9/03
               || f4->fileCreated == 1
            #endif
            ) )
      #endif
            return error4( f4->codeBase, e4parm, E90619 ) ;
   #endif

   c4 = f4->codeBase ;

   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( f4->isReadOnly )
      return error4( c4, e4write, E80606 ) ;

   #ifndef S4OFF_OPTIMIZE
      #ifdef S4ADVANCE_READ
         /* take care of over-writing advance-read buffers where appropriate */
         if ( c4->advanceReadsEnabled
            // AS Apr 13/04 - support for optimizing large files
            // #ifdef S4FILE_EXTENDED
            //    && f4->isLong == 0
            // #endif
            )
            // AS Apr 13/04 - support for optimizing large files
            if ( f4->hasHadPendingAdvanceRead )  /* for efficiency, avoid for non-advance-read files */
               file4advanceReadWriteOver( f4, pos, len, ptr, 1 ) ;
      #endif
      // AS Apr 13/04 - support for optimizing large files
      // #ifdef S4FILE_EXTENDED
         // if ( f4->isLong == 0 && file4longGetHi( pos ) == 0 &&
         //    ( (file4longGetLo( pos ) + len) > file4longGetLo( pos ) ) &&
         //    ( file4longGetLo( pos ) + len < 0xffffffff ) )  /* LY 2001/04/16 : pos+len can be >4GB in 64bit */
         //   return file4writeOpt( f4, file4longGetLo( pos ), ptr, len, 0, 0, 0 ) ;
         // else  /* not optimized in this case */
         // {
         //    if ( f4->doBuffer == 1 )
         //       file4optimize( f4, OPT4OFF, OPT4OTHER ) ;
         //    return file4writeLow( f4, pos, ptr, len, 1, 0, 1 ) ;
         // }
      // #else
         return file4writeOpt( f4, pos, ptr, len, 0, 0, 0 ) ;
      // #endif
   #else
      /* file4writeLow() itself takes care of the advance-read condition... */
      return file4writeLow( f4, pos, ptr, len, 1, 0, 1 ) ;
   #endif
}

#ifdef S4WRITE_DELAY
#define MEM4DELAY_START 10
#define MEM4DELAY_EXPAND 10



int file4writeDelay( FILE4 *f4, FILE4LONG pos, const void *data, const unsigned len, S4DELAY_FUNCTION *completionRoutine, void *completionData )
{
   FILE4WRITE_DELAY *writeDelay ;
   LINK4 *delayLink ;
   CODE4 *c4 ;
   int rc ;
   FILE4LONG tLong, tLong2 ;

   c4 = f4->codeBase ;

   #ifdef E4PARM_LOW
      if ( completionRoutine == 0 || c4 == 0 )
         error4( c4, e4write, E90619 ) ;
   #endif

   /* first check to see if the current delay-write over-rides an existing one */
   if ( l4numNodes( &f4->delayWriteFileList ) != 0 )
   {
      critical4sectionEnter( &c4->critical4delayWriteList ) ;

      for ( delayLink = (LINK4 *)l4first( &f4->delayWriteFileList ) ;; )
      {
         if ( delayLink == 0 )
            break ;
         writeDelay = (FILE4WRITE_DELAY *)(delayLink - 1 ) ;
         delayLink = (LINK4 *)l4next( &f4->delayWriteFileList, delayLink ) ;

         /* LY 4/27/99 : replaced binary operators with file4long*** */
         file4longAssignLong( tLong, writeDelay->pos ) ;
         file4longAdd( &tLong, (long) writeDelay->len ) ;
         if ( file4longLessEqLong( tLong, pos ) )  /* outside of block */
            continue ;
         file4longAssignLong( tLong, pos ) ;
         file4longAdd( &tLong, (long) len ) ;
         if ( file4longLessEqLong( tLong, writeDelay->pos ) ) /* outside of block */
            continue ;

         while ( writeDelay->usageFlag == r4inUse )  /* is being written to disk, just wait until it is done... */
         {
            // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
            // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
            u4sleep( c4 ) ;
         }

         if ( writeDelay->usageFlag == r4finished ) /* is written to disk, so can just ignore */
            continue ;

         /* we have a delay-write which overlaps the current request */

         /* if the entire block is within the range, then can just delete
            it, otherwise must write it now (note that, due to constant
            contents of the delay-write buffer, cannot copy into it
            directly */

         /* LY 4/27/99 : replaced binary operators with file4long*** */
         file4longAssignLong( tLong, writeDelay->pos ) ;
         file4longAdd( &tLong, writeDelay->len ) ;
         file4longAssignLong( tLong2, pos ) ;
         file4longAdd( &tLong2, len ) ;
         if ( file4longLessEqLong( pos, writeDelay->pos ) && file4longLessEqLong( tLong, tLong2 ) ) /* remove */
            writeDelay->status = 0 ;
         else /* otherwise must write the block now, and then continue */
         {
            // AS Apr 30/10 - don't flush at this time since we actually just want to write out this one disk block, and flushing sometimes has a side effect of locking the computer trying to flush other data with delay writes
            writeDelay->status = file4writeLow( writeDelay->file, writeDelay->pos, writeDelay->data, writeDelay->len, 0, 1, 0 ) ;
         }

         writeDelay->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
         l4remove( &f4->codeBase->delayWriteList, writeDelay ) ;
         l4remove( &f4->delayWriteFileList, &writeDelay->fileLink ) ;
         writeDelay->completionRoutine( writeDelay ) ;
         mem4free( c4->delayWriteMemory, writeDelay ) ;
      }
      critical4sectionLeave( &c4->critical4delayWriteList ) ;
   }

   if ( c4->delayWriteMemory == 0 )
      writeDelay = (FILE4WRITE_DELAY *)mem4createAllocZero( c4, &c4->delayWriteMemory, MEM4DELAY_START, sizeof( FILE4WRITE_DELAY ), MEM4DELAY_EXPAND, 0 ) ;
   else
      writeDelay = (FILE4WRITE_DELAY *)mem4allocZero( c4->delayWriteMemory ) ;

   if ( writeDelay == 0 )
      return error4( c4, e4memory, E90624 ) ;

   writeDelay->file = f4 ;
   writeDelay->data = (const char *)data ;
   writeDelay->len = len ;
   writeDelay->pos = pos ;
   writeDelay->usageFlag = r4queued ;
   writeDelay->completionRoutine = completionRoutine ;
   writeDelay->completionData = completionData ;

   if ( c4->delayWritesEnabled == 0 )   /* delay-writes not enabled, so just write */
   {
      rc = file4writeLow( f4, pos, data, len, 1, 1, 1 ) ;
      completionRoutine( writeDelay ) ;
      // AS 03/14/01 - Was not freeing up this memory as expected...
      mem4free( c4->delayWriteMemory, writeDelay ) ;
      return rc ;
   }

   critical4sectionEnter( &c4->critical4delayWriteList ) ;

   l4add( &c4->delayWriteList, writeDelay ) ;
   l4add( &f4->delayWriteFileList, &writeDelay->fileLink ) ;

   critical4sectionLeave( &c4->critical4delayWriteList ) ;

   SetEvent( c4->pendingWriteEvent ) ;  /* notify the write thread */
   // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
   // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
   u4sleep( c4 ) ;

   return r4delay ;
}



int file4writeDelayFlush( FILE4 *file, const int doWrite )
{
   /* flush out any delayed-writes for the file in question */
   /* if doWrite is false, then the blocks are just dumped (eg. temp. files) */
   FILE4WRITE_DELAY *writeDelay ;
   LINK4 *writeDelayLink, *saved ;
   CODE4 *c4 ;
   FILE4LONG tLong ;

   c4 = file->codeBase ;

   /* by obtaining the critical4delayWriteList critical section, we can
      guarantee that the other thread will be suspended.  Therefore
      the flushes for this file will get high-priority treatment, which
      is what is desired
   */

   /* AS 08/16/99 --> just exit if delay writes not enabled because critical section maybe not initialized...*/
   if ( c4->delayWritesEnabled == 0 )
      return 0 ;

   critical4sectionEnter( &c4->critical4delayWriteList ) ;

   /* go through the list and manually flush each one belonging to our file */

   for ( writeDelayLink = (LINK4 *)l4first( &file->delayWriteFileList ) ;; )
   {
      if ( writeDelayLink == 0 )
         break ;
      writeDelay = (FILE4WRITE_DELAY *)(writeDelayLink - 1) ;
      saved = (LINK4 *)l4next( &file->delayWriteFileList, writeDelayLink ) ;
      if ( writeDelay->usageFlag == r4queued )  /* do ourselves */
      {
         l4remove( &file->delayWriteFileList, writeDelayLink ) ;
         l4remove( &c4->delayWriteList, writeDelay ) ;
         if ( doWrite == 1 )
         {
            /* LY 4/27/99 : replaced binary operators with file4long*** */
            file4longAssignLong( tLong, writeDelay->pos ) ;
            // AS Apr 30/10 - don't flush at this time since we actually just want to write out this one disk block, and flushing sometimes has a side effect of locking the computer trying to flush other data with delay writes
            writeDelay->status = file4writeLow( writeDelay->file, tLong, writeDelay->data, writeDelay->len, 0, 1, 0 ) ;
         }
         else
            writeDelay->status = 0 ;
         writeDelay->usageFlag = r4finished ;
         writeDelay->completionRoutine( writeDelay ) ;
         mem4free( c4->delayWriteMemory, writeDelay ) ;
      }
      writeDelayLink = saved ;
   }

   critical4sectionLeave( &c4->critical4delayWriteList ) ;

   // AS Apr 11/05 - Requires some amount of improvement...the SetEvent should not get called repetitively
   #ifdef E4ANALYZE
      if ( l4numNodes( &file->delayWriteFileList ) > 1 )   /* in theory impossible, it means delay-write has 2 files writing at same time */
         return error4( c4, e4struct, E90624 ) ;
   #endif

   if ( l4numNodes( &file->delayWriteFileList ) != 0 )
   {
      // AS Apr 11/05 - there was one issue here due to a fix made in April to support compress-write support.
      // it is possible that our thread holds the files critical-write thread if we are performing a length adjustment here.
      // In that case we need to release the critical section...use the revised critical section handling code...
      Bool5 weHoldCriticalSection = 0 ;
      if ( file->critical4file.count != 0 && file->critical4file.threadOwner == GetCurrentThreadId() )
      {
         weHoldCriticalSection = 1 ;
         critical4sectionLeave( &file->critical4file ) ;
      }

      SetEvent( c4->pendingWriteEvent ) ;  /* notify the write thread */
      do
      {
         // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
         // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
         u4sleep( c4 ) ;
      } while (l4numNodes( &file->delayWriteFileList ) != 0 ) ;

      if ( weHoldCriticalSection )
         critical4sectionEnter( &file->critical4file ) ;
   }

   return 0 ;
}



#ifdef S4USE_INT_DELAY
   int _cdecl file4writeDelayMain( void *data )
#else
   void _cdecl file4writeDelayMain( void *data )
#endif
{
   /* main function for delay-write thread (i.e. paramater to CreateThread()) */
   CODE4 *c4 ;
   FILE4WRITE_DELAY *writeDelay ;
   FILE4LONG tLong ;

   c4 = (CODE4 *)data ;
   // AS Nov 21/05 - due to an apparent vb/xp sequencing problem, see if we cancelled the initialization
   if ( c4->delayWritesDisabled == 1 )
      return ;
   c4->delayWritesEnabled = 1 ;

   for ( ;; )
   {
      if ( l4numNodes( &c4->delayWriteList ) == 0 )
      {
         if ( c4->uninitializeDelayWrite == 1 )   /* shutdown */
         {
            SetEvent( c4->initUndoDelayWrite ) ;
            #ifdef S4USE_INT_DELAY
               return 0 ;
            #else
               return ;
            #endif
         }
         else
         {
            WaitForSingleObject( c4->pendingWriteEvent, INFINITE ) ;
            ResetEvent( c4->pendingWriteEvent ) ;
         }
      }
      else  /* perform a write on the first available block */
      {
         critical4sectionEnter( &c4->critical4delayWriteList ) ;
         writeDelay = (FILE4WRITE_DELAY *)l4first( &c4->delayWriteList ) ;
         if ( writeDelay == 0 )   /* maybe got removed by main thread, so none to write... */
         {
            critical4sectionLeave( &c4->critical4delayWriteList ) ;
            // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
            // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
            u4sleep( c4 ) ;
            continue ;
         }
         writeDelay->usageFlag = r4inUse ;
         critical4sectionLeave( &c4->critical4delayWriteList ) ;

         file4longAssignLong( tLong, writeDelay->pos ) ;
         writeDelay->status = file4writeLow( writeDelay->file, tLong, writeDelay->data, writeDelay->len, 0, 1, 0 ) ;
         writeDelay->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
         critical4sectionEnter( &c4->critical4delayWriteList ) ;
         l4remove( &c4->delayWriteList, writeDelay ) ;
         l4remove( &writeDelay->file->delayWriteFileList, &writeDelay->fileLink ) ;
         critical4sectionLeave( &c4->critical4delayWriteList ) ;
         writeDelay->completionRoutine( writeDelay ) ;
         mem4free( c4->delayWriteMemory, writeDelay ) ;
      }
   }
}
#endif /* S4WRITE_DELAY */



// AS Nov 26/02 - New function for data file compression
/* LY July 7/03 : added !defined( S4LUPACH ) */
// AS May 17/04 - client functionality to copmress the data file...
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && !defined( S4LUPACH )
   int file4copy( FILE4 *to, FILE4 *from )
   {
      // both files must be created and open.
      // the contents of the 'from' file are copied to the 'to' file
      assert5port( "function for copying files required for d4compress" ) ;
      CODE4 *c4 = to->codeBase ;

      int rc = -1 ;
      char buf[1024] ;
      FILE4LONG p1 ;

      if ( error4code( c4 ) != 0 )
         return -1 ;

      file4longAssign( p1, 0, 0L ) ;

      for( ;; )
      {
         unsigned lenRead = file4readInternal( from, p1, buf, sizeof( buf ) ) ;
         if ( lenRead == 0 ) // done
            break ;
         if ( file4writeInternal( to, p1, buf, lenRead ) != 0 )
            return -1 ;

         if ( lenRead < sizeof( buf ) )  // we are done
         {
            rc = 0 ;
            break ;
         }

         file4longAdd( &p1, sizeof( buf ) ) ;
      }

      if ( error4code( from->codeBase ) < 0 || !file4longEqualLong( file4lenLow( from ), file4lenLow( to ) ) )
         return -1 ;

      return 0 ;
   }
#endif /* !S4OFF_WRITE, S4STAND_ALONE, S4FOX */
