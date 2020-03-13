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

/* m4file.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.  */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

/*
   S4FOX memo handling ...

   keeps the 'eof' position available
   when writing a memo, if a new entry writes at 'eof'
   if an existing entry and can still fit in current spot, uses current spot.  If it can't,
     it uses eof.
   S4FOX does not maintain a free chain of available memo blocks.
*/

#ifdef S4COMPRESS
   // set the compress level relatively high
   assert5port( "added compressed memo entries support" ) ;
   // AS May 13/04 - support configureable compression
   // #define COMPRESS4MEMO_DEFAULT_LEVEL 7
#endif

#if !defined( S4CLIENT ) && !defined( S4OFF_MEMO )
   #if !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
      /* clipper version has no free chain--do not lock memo file */
      /* the lock is forced since a memo file lock only lasts if the .dbf file is locked */
      int memo4fileLock( MEMO4FILE *f4memo )
      {
         int rc = 0 ;

         CODE4 *c4 = f4memo->file.codeBase ;

         if ( f4memo->fileLock == 1 )
            return 0 ;

         if ( f4memo->file.hand == INVALID4HANDLE )
            return -1 ;

         // AS July 18/02 - don't lock file at low level if opened exclusively
         if ( f4memo->file.lowAccessMode != OPEN4DENY_RW )
         {
            int oldAttempts = c4->lockAttempts ;
            c4->lockAttempts = WAIT4EVER ;

            #ifdef S4MDX
               rc = file4lockInternal( &f4memo->file, L4LOCK_POS - 1L, 0, 2L, 0 ) ;
            #endif

            #ifdef S4FOX
               rc = file4lockInternal( &f4memo->file, L4LOCK_POS_OLD, c4->largeFileOffset, 1L, 0 ) ;
            #endif

            c4->lockAttempts = oldAttempts ;
            #ifndef S4OPTIMIZE_OFF
               file4refresh( &f4memo->file ) ;   /* make sure all up to date */
            #endif
         }

         if ( rc == 0 )
            f4memo->fileLock = 1 ;
         return rc ;
      }



      int memo4fileUnlock( MEMO4FILE *f4memo )
      {
         int rc = 0 ;

         if ( f4memo->fileLock == 0 )
            return 0 ;
         // AS July 18/02 - don't unlock file at low level if opened exclusively
         if ( f4memo->file.lowAccessMode != OPEN4DENY_RW )
         {
            #ifdef S4MDX
               rc = file4unlockInternal( &f4memo->file, L4LOCK_POS - 1L, 0, 2L, 0 ) ;
            #endif
            #ifdef S4FOX
               rc = file4unlockInternal( &f4memo->file, L4LOCK_POS_OLD, f4memo->file.codeBase->largeFileOffset, 1L, 0 ) ;
            #endif
         }
         if ( rc == 0 )
            f4memo->fileLock = 0 ;
         return rc ;
      }
   #endif /*  #if !defined( S4OFF_MULTI ) && !defined( S4CLIPPER ) */



   int memo4fileOpen( MEMO4FILE *f4memo, DATA4FILE *d4, char *name )
   {
      MEMO4HEADER  header ;
      int rc ;
      FILE4LONG pos ;

      f4memo->data = d4 ;

      // AS May 24/02 - created file4openInternal for internal use to indicate file types
      if ( file4openInternal( &f4memo->file, d4->c4, name, 1, OPT4OTHER ) )
         return -1 ;

      #ifndef S4OPTIMIZE_OFF
         file4optimize( &f4memo->file, d4->c4->optimize, OPT4OTHER ) ;
      #endif

      /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
      file4longAssign( pos, 0, 0L ) ;
      if ( (rc = file4readAllInternal(&f4memo->file, pos, &header, sizeof(header))) < 0 )
         return -1 ;

      #ifdef S4BYTE_SWAP
         #ifdef S4MFOX
            f4memo->blockSize = header.blockSize  ;
         #else
            #ifdef S4MNDX
               f4memo->blockSize = MEMO4SIZE ;
            #else
               f4memo->blockSize = x4reverseShort( (void *)&header.blockSize) ;
            #endif
         #endif
      #else
         #ifdef S4MFOX
            f4memo->blockSize = x4reverseShort( (void *)&header.blockSize ) ;
         #else
            #ifdef S4MNDX
               f4memo->blockSize = MEMO4SIZE ;
            #else
               f4memo->blockSize = header.blockSize ;
            #endif
         #endif
      #endif
      assert5( f4memo->blockSize != 0 ) ;
      return rc ;
   }



   #ifdef S4MFOX
      // LY Jul 20/04 : added !S4MACINTOSH
      #if !defined( S4LUPACH ) && !defined( S4MACINTOSH ) /* LY July 7/03 */
         assert5port( "added compressed memo entries support" ) ;
      #endif
      int memo4fileReadPart( MEMO4FILE *f4memo, long memoId, char **ptrPtr, unsigned *lenPtr, unsigned long offset, const unsigned readMax, long *type, Bool5 uncompress )
      {
         /* offset is # bytes from start of memo that reading should begin, readMax is
            the maximum possible that can be read (limited to an unsigned int, so is
            16-bit/32-bit compiler dependent.

            if uncompress is false any compressed memo fields are not uncompressed as it is read.  This is used when
            calling d4memoCompress() which actually just removes blank entries from the memo file, but we don't need to
            return the value back to a caller
         */
         unsigned long avail ;
         MEMO4BLOCK memoBlock ;
         FILE4LONG pos ;

         if ( memoId <= 0L )
         {
            *lenPtr = 0 ;
            return 0 ;
         }

         /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
         file4longAssign( pos, (unsigned long)memoId, 0L ) ;
         assert5( f4memo->blockSize != 0 ) ;
         file4longMultiply( pos, f4memo->blockSize ) ;

         if ( file4readAllInternal( &f4memo->file, pos, &memoBlock, sizeof( MEMO4BLOCK ) ) < 0)
            return -1 ;

         #ifndef S4BYTE_SWAP
            memoBlock.type = x4reverseLong( (void *)&memoBlock.type ) ;
            memoBlock.numChars = x4reverseLong( (void *)&memoBlock.numChars ) ;
         #endif
         file4longAdd( &pos, 2 * sizeof( S4LONG ) ) ;

         unsigned long actualLength = memoBlock.numChars ;
         long toReadLength = 0 ;
         // LY Jul 20/04 : added !S4MACINTOSH
         #if !defined( S4LUPACH ) && !defined( S4MACINTOSH )   /* LY July 15/03 */
            if ( memoBlock.type == 3L && uncompress == 1 )  // means compressed and we want to uncompress it...
            {
               toReadLength = memoBlock.numChars ;
               // cannot read partial if compressed...
               if ( offset != 0 )
                  return error4describe( f4memo->file.codeBase, e4notSupported, E95210, "partial reading of memos not supported with compression", 0, 0 ) ;

               // in this case, the first 4 bytes contain the actual memo field length...
               if ( file4readAllInternal( &f4memo->file, pos, &actualLength, sizeof( long ) ) < 0)
                  return -1 ;
               file4longAdd( &pos, sizeof( S4LONG ) ) ;
               toReadLength -= sizeof( long ) ;
               avail = actualLength ;
            }
            else
         #endif
         {
            // if not compressed, might be performing partial reading
            avail = actualLength - offset ;
            toReadLength = avail ;
         }

         if ( avail > (unsigned long)readMax )
         {
            avail = readMax ;
            toReadLength = avail ;
         }

         if ( avail > (unsigned long)*lenPtr )
         {
            // CS 2008/08/06 Check for memory size before allocating. u4alloc cannot allocate more than 2GB.
            // AS Aug 8/12 - If the memo file is compressed, the avail may be larger than the size of the memo file (e.g. a 1 entry memo file), int that case use the toReadLength
            if ( avail > 0x7FFFFFF0 ||
               (uncompress == 0 && avail > file4longGetLo( file4lenLow( &f4memo->file ) )) ||
               (uncompress == 1 && toReadLength > file4longGetLo( file4lenLow( &f4memo->file ) ))
               )
            {
               error4set( f4memo->file.codeBase, 0 ) ;
               return error4( f4memo->file.codeBase, e4memoCorrupt, E95210 ) ;
            }
            else
            {
               if ( *lenPtr > 0 )
                  u4free( *ptrPtr ) ;
               *ptrPtr = (char *)u4alloc( avail + 2 ) ;  // allow room for 2 NULL characters (terminating UTF-16 string)
               if ( *ptrPtr == 0 )
               {
                  #ifdef S4SERVER
                     error4set( f4memo->file.codeBase, e4memory ) ;
                     return e4memory ;
                  #else
                     return error4( f4memo->file.codeBase, e4memory, E95210 ) ;
                  #endif
               }
            }
         }

         *lenPtr = (unsigned)toReadLength ;
         *type = memoBlock.type ;

         if ( avail == 0 )
            return 0 ;
         int rc = 0 ;
         file4longAdd( &pos, offset ) ;
         #ifdef S4COMPRESS
            if ( memoBlock.type == 3L && uncompress == 1 )  // means compressed and we want to uncompress it...
            {
               CODE4 *c4 = f4memo->file.codeBase ;
               if ( (*lenPtr) > c4->compressedMemosBufferLen )
               {
                  if ( c4->compressedMemosBuffer != 0 )
                  {
                     u4free( c4->compressedMemosBuffer ) ;
                     c4->compressedMemosBuffer = 0 ;
                     c4->compressedMemosBufferLen = 0 ;
                  }
                  c4->compressedMemosBuffer = u4allocFree( c4, (*lenPtr) ) ;
                  if ( c4->compressedMemosBuffer == 0 )
                     return e4memory ;
                  c4->compressedMemosBufferLen = (*lenPtr) ;
               }
               rc = file4readAllInternal( &f4memo->file, pos, c4->compressedMemosBuffer, *lenPtr ) ;
               if ( rc != 0 )
                  return rc ;
               c4uncompress( f4memo->file.codeBase, *ptrPtr, &actualLength, c4->compressedMemosBuffer, *lenPtr ) ;
               *lenPtr = actualLength ;
            }
            else
         #endif /* S4COMPRESS */
            {
               rc = file4readAllInternal( &f4memo->file, pos, *ptrPtr, *lenPtr ) ;
            }

         return rc ;
      }
   #endif /* S4MFOX */



   #ifdef S4MNDX
      extern unsigned short f4memoNullChar ;
   #endif



   #ifndef S4MFOX
      int memo4fileRead( MEMO4FILE *f4memo, long memoId, char **ptrPtr, unsigned int *ptrLen )
      {
         FILE4LONG pos ;
         #ifdef S4MNDX
            unsigned int amtRead, lenRead, loop ;
            char *tPtr ;
            FILE4LONG p2 ;
         #else
            MEMO4BLOCK  memoBlock ;
            unsigned finalLen ;
         #endif

         CODE4 *c4 = f4memo->file.codeBase ;

         #ifdef S4MNDX
            if ( memoId <= 0L )
            {
               if ( *ptrPtr != (char *)(&f4memoNullChar) )
                  u4free( *ptrPtr ) ;
               *ptrPtr = 0 ;
               *ptrLen = 0 ;
               return 0 ;
            }

            assert5( f4memo->blockSize != 0 ) ;
            file4longAssign( pos, memoId * f4memo->blockSize, 0 ) ;

            amtRead = 0 ;

            if ( c4->memoUseBuffer == 0 )
            {
               c4->memoUseBuffer = (char*)u4allocEr( c4, MEMO4SIZE ) ;
               if ( c4->memoUseBuffer == 0 )
                  return e4memory ;
            }

            for( amtRead = 0 ;; )
            {
               file4longAssign( p2, file4longGetLo( pos ) + amtRead, 0 ) ;
               lenRead = file4readInternal( &f4memo->file, p2, c4->memoUseBuffer, MEMO4SIZE ) ;
               if ( lenRead <= 0 )
               {
                  // AS Apr 29/04 - If 0 is returned an error was not being generated causing a silent failure
                  // to work its way up the chain (in particular odbc if the memo file was bad)
                  if ( error4code( c4 ) == 0 )
                     error4( c4, e4memoCorrupt, E85203 ) ;
                  return -1 ;
               }

               for ( loop = 0 ; lenRead > 0 ; loop++, lenRead-- )
               {
                  if ( c4->memoUseBuffer[loop] == 0x1A ) /* if done */
                  {
                     if ( loop + amtRead > 0 )
                     {
                        if ( *ptrLen < amtRead + loop )
                        {
                           tPtr = (char *)u4allocEr( c4, amtRead + loop + 2 ) ;  // CS 1999/09/15 allow room for Unicode NULL character
                           if ( tPtr == 0 )
                              return e4memory ;
                           memcpy( tPtr, *ptrPtr, (int)amtRead ) ;
                           if ( *ptrPtr != (char *)(&f4memoNullChar) )
                              u4free( *ptrPtr ) ;
                           *ptrPtr = tPtr ;
                        }
                        *ptrLen = amtRead + loop ;
                        memcpy( *ptrPtr + amtRead, c4->memoUseBuffer, loop ) ;
                        (*ptrPtr)[amtRead+loop+0] = 0 ;  // CS 1999/09/15 make last 2 bytes NULL (Unicode NULL)
                        (*ptrPtr)[amtRead+loop+1] = 0 ;
                     }
                     else
                     {
                        tPtr = 0 ;
                        if ( *ptrPtr != (char *)(&f4memoNullChar) )
                           u4free( *ptrPtr ) ;
                        *ptrPtr = 0 ;
                        *ptrLen = 0 ;
                     }
                     return 0 ;
                  }
               }

               lenRead = loop ;

               if ( *ptrLen < amtRead + lenRead )
               {
                  tPtr = (char *)u4allocEr( c4, amtRead + lenRead + 2 ) ;  // CS 1999/09/15 allow room for Unicode NULL character
                  if ( tPtr == 0 )
                     return e4memory ;
                  if ( *ptrLen > 0 )
                  {
                     memcpy( tPtr, *ptrPtr, *ptrLen ) ;
                  }
                  // AS 06/26/00 was not freeing this up in clipper (t4desc1.c)
                  if ( (char *)(*ptrPtr) != (char *)(&f4memoNullChar) )
                     u4free( *ptrPtr ) ;
                  *ptrPtr = tPtr ;
                  *ptrLen = (unsigned)( amtRead + lenRead ) ;
               }
               memcpy( *ptrPtr + amtRead, c4->memoUseBuffer, lenRead ) ;

               amtRead += lenRead ;
            }
         #else  /* else S4MNDX */
            if ( memoId <= 0L )
            {
               *ptrLen = 0 ;
               return 0 ;
            }

            // AS Nov 24/03 - support for large files
            file4longAssign( pos, memoId, 0 ) ;
            assert5( f4memo->blockSize != 0 ) ;
            file4longMultiply( pos, f4memo->blockSize ) ;
            if ( file4readAllInternal( &f4memo->file, pos, &memoBlock, sizeof( MEMO4BLOCK ) ) < 0)
               return -1 ;

            #ifdef S4BYTE_SWAP
               memoBlock.startPos = x4reverseShort( (void *)&memoBlock.startPos ) ;
               memoBlock.numChars = x4reverseLong( (void *)&memoBlock.numChars ) ;
            #endif

            if ( memoBlock.minusOne != -1 )  /* must be an invalid entry, so return an empty entry */
            {
               *ptrLen = 0 ;
               return 0 ;
            }
            else
            {
               if ( memoBlock.numChars >= UINT_MAX )
                  return error4( c4, e4info, E95210 ) ;

               finalLen = (unsigned)memoBlock.numChars - 2 * ( sizeof(short) ) - ( sizeof(S4LONG) ) ;
               if ( finalLen > *ptrLen )
               {
                  if ( *ptrLen > 0 )
                     u4free( *ptrPtr ) ;
                  *ptrPtr = (char *)u4allocEr( c4, finalLen + 2 ) ;  // CS 1999/09/15 allow room for Unicode NULL character
                  if ( *ptrPtr == 0 )
                     return e4memory ;
               }
               *ptrLen = finalLen ;

               file4longAdd( &pos, memoBlock.startPos ) ;
               return file4readAllInternal( &f4memo->file, pos, *ptrPtr, finalLen ) ;
            }
         #endif /* S4MNDX */
      }
   #endif /* !S4MFOX */



   #ifndef S4OFF_WRITE
      #ifdef S4MFOX
         static int memo4fileNumRequiredBlocks( long memoLen, short blockSize )
         {
            /* The # of required blocks:

               the size of the total memo entry (memo length + sizeof header) + (blockSize - 1 : in
                      order to allow the integer div function to give us a '+1')
                  divided by
               the block size being used.
            */
            assert5( blockSize != 0 ) ;
            return (int)( ( memoLen + sizeof(MEMO4BLOCK) + blockSize - 1 ) / blockSize ) ;
         }



         static unsigned memo4fileGetNumberBlocks( MEMO4FILE *f4memo, long blockNo, short blockSize )
         {
            /* obtains the # of blocks in the given memo.  Simply does a read of the block header
               and returns the result.
            */

            FILE4LONG pos ;
            assert5( blockSize != 0 ) ;

            /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
            file4longAssign( pos, blockNo, 0L ) ;
            file4longMultiply( pos, blockSize ) ;

            MEMO4BLOCK oldMemoBlock ;
            file4readAllInternal( &f4memo->file, pos, (char *)&oldMemoBlock, sizeof(MEMO4BLOCK) ) ;

            #ifndef S4BYTE_SWAP
               oldMemoBlock.type = x4reverseLong( (void *)&oldMemoBlock.type ) ;
               oldMemoBlock.numChars = x4reverseLong( (void *)&oldMemoBlock.numChars ) ;
            #endif
            /* AS 02/11/99added sizeof( MEMO4BLOCK ) as in numRequired function agove to avoid wasting space */
            return  (unsigned)( ( oldMemoBlock.numChars + sizeof(MEMO4BLOCK) + blockSize - 1 ) / blockSize ) ;
         }



         static int memo4fileGetFileHeader( MEMO4FILE *f4memo, MEMO4HEADER *memoFileHeader, int *memoNeedsUnlocking )
         {
            /* read the memo file header form disk into the output MEMO4HEADER block */
            /* this function locks the memo file to do the read.  If the memo file was unlocked
               before the read and the lock succeeds, memoNeedsUnlocking is set to '1' to tell
               the caller that they will need to unlock the memo file before they exit, if they
               are to guarantee input/output locking consistency
             */

            int rc = 0 ;
            #if !defined( S4OFF_MULTI )
               int memoFileWasLocked = f4memo->data->memoFile.fileLock ;
               rc = memo4fileLock( &f4memo->data->memoFile ) ;
               if ( rc )
                  return rc ;
               if ( memoFileWasLocked == 0 )
                  *memoNeedsUnlocking = 1 ;
            #endif

            FILE4LONG pos ;
            /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
            file4longAssign( pos, 0, 0L ) ;
            unsigned lenRead = file4readInternal( &(f4memo->file), pos, memoFileHeader, sizeof( MEMO4HEADER ) ) ;
            #ifndef S4BYTE_SWAP
               memoFileHeader->nextBlock = x4reverseLong( (void *)&(memoFileHeader->nextBlock) ) ;
               /* block size isn't needed */
            #endif

            if ( error4code( f4memo->data->c4 ) < 0 )
               rc = -1 ;
            else
            {
               if ( lenRead != sizeof( MEMO4HEADER ) )
                  return file4readError( &f4memo->file, pos, sizeof( MEMO4HEADER ), "memo4fileWritePart" ) ;
            }

            return rc ;
         }



         static int memo4fileWriteFileHeader( MEMO4FILE *f4memo, MEMO4HEADER *memoFileHeader )
         {
            /* writes the input memoFileHeader to the memo file header position */

            #ifndef S4BYTE_SWAP
               memoFileHeader->nextBlock = x4reverseLong( (void *)&(memoFileHeader->nextBlock) ) ;
            #endif

            FILE4LONG pos ;
            /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
            file4longAssign( pos, 0, 0L ) ;
            return file4writeInternal( &f4memo->file, pos, memoFileHeader, sizeof( MEMO4HEADER ) ) ;
         }



         static long memo4fileNextWritePosition( MEMO4FILE *f4memo, int numBlocksRequiredForInputMemo )
         {
            /*
               this function gets the 'next' write position for the memo file, and also updates
               the 'new' next write position.

               the 'next' write position (i.e. the memo end of file) is stored in the header
               of the memo file (allows concurrent access by not using physical file length).

               We lock the memo file to perform our operations.  We also guarantee to unlock
               the memo file if it was unlocked going in (else we leave in the locked state)
            */
            /* we preserve the locking of the memo file.  If it is locked going in, it is locked
               going out.  If it is unlocked going in, it is unlocked going out */
            int memoNeedsUnlocking = 0 ;  /* means we need to unlock the memo before leaving */

            MEMO4HEADER memoFileHeader ;
            int rc = memo4fileGetFileHeader( f4memo, &memoFileHeader, &memoNeedsUnlocking ) ;
            long newMemoId = -1 ;
            if ( rc == 0 )
            {
               newMemoId = memoFileHeader.nextBlock ;
               memoFileHeader.nextBlock = newMemoId + numBlocksRequiredForInputMemo ;
               rc = memo4fileWriteFileHeader( f4memo, &memoFileHeader ) ;
            }

            #if !defined( S4OFF_MULTI )
               if ( memoNeedsUnlocking )
                  memo4fileUnlock( &f4memo->data->memoFile ) ;
            #endif

            if ( rc != 0 )
               return rc ;

            return newMemoId ;
         }



         int memo4fileWritePart( MEMO4FILE *f4memo, long *memoIdPtr, const char *ptr, const long memoLen, const long offset, const unsigned lenWrite, const long type )
         {
            /*
               Writes partial data to a memo record.
               Usage rules:
                  Must call this function with an offset == 0 to write 1st part of block
                  before any additional writing.  In addition, the memoLen must be
                  accurately set during the first call in order to reserve the correct
                  amount of memo space ahead of time.  Later calls just fill in data to
                  the reserved disk space.
                  lenWrite is the amount of data to write, offset is the number of
                  bytes from the beginning of the memo in which to write the data
                  Secondary calls to this function assume that everything has been
                  previously set up, and merely performs a file write to the reserved
                  space.  The space is not checked to see whether or not it actually
                  is in the bounds specified, so use with care.
            */
            #ifdef E4PARM_LOW
               if ( memoIdPtr == 0 )
                  return error4( 0, e4parm_null, E95208 ) ;
               if ( f4memo->file.hand == INVALID4HANDLE ) /* file closed! */
                  return error4( 0, e4parm, E95208 ) ;
            #endif

            /* AS 11/16/98 --> FoxPro now supports block size of '0' - which means '1' */
            /* the actual file contains a single header and then the whole memo.  Basically
               this means a block size which can vary in size. */
            short effectiveBlockSize ;

            if ( f4memo->blockSize == 0 )
               effectiveBlockSize = 1 ;
            else
            {
               assert5( f4memo->blockSize != 0 ) ;
               effectiveBlockSize = f4memo->blockSize ;
            }

            #ifdef E4DEBUG
               // LY Jul 20/04 : added !S4MACINTOSH
               #if !defined( S4LUPACH ) && !defined( S4MACINTOSH )   /* LY July 9/03 */
                  assert5port( "added compressed memo entries support" ) ;
                  if ( f4memo->data->compressedMemosSupported == 1 )
                  {
                     // ensure that the input is compressed if required...
                     if ( type != 3 )  // 3 means compressed
                     {
                        // the length had better be < the size of a block, else it should have been compressed
                        assert5( ( offset + memoLen ) <= effectiveBlockSize ) ;
                     }
                  }
               #endif
            #endif

            /* offset of 0 means we are calling this function for the first time for this memo, so
               we need to write out the initialization bytes as well. */
            if ( offset == 0 )
            {
               if ( memoLen == 0 )
               {
                  *memoIdPtr = 0L ;
                  return 0 ;
               }

               /* AS 11/16/98 --> FoxPro now supports a blockSize of 0, which means always allocate
                  enough for the given memo only

                  #ifdef E4MISC
                     if ( f4memo->blockSize <= 1 )
                        return error4( f4memo->data->c4, e4info, E85202 ) ;
                  #endif
               */

               int numBlocksRequiredForInputMemo = memo4fileNumRequiredBlocks( memoLen, effectiveBlockSize ) ;

               long blockNo ;
               unsigned numBlocksInExistingMemo ;

               if ( *memoIdPtr <= 0L )
               {
                  blockNo = 0L ;
                  numBlocksInExistingMemo = 0 ;
               }
               else
               {
                  blockNo = *memoIdPtr ;
                  numBlocksInExistingMemo = memo4fileGetNumberBlocks( f4memo, blockNo, effectiveBlockSize ) ;
               }

               if ( numBlocksInExistingMemo >= ((unsigned)numBlocksRequiredForInputMemo) && blockNo )  /* write to existing position */
               {
                  /* sufficient room in existing memo, so use it... */
                  *memoIdPtr = blockNo ;
               }
               else
               {
                  /* there is not enough room in the current, existing, memo, so we need to write
                     the memo block at the 'next available position' (i.e. the end of file).
                     This eof position is stored in the memo files header record.
                  */

                  *memoIdPtr = memo4fileNextWritePosition( f4memo, numBlocksRequiredForInputMemo ) ;
                  if ( error4code( f4memo->data->c4 ) < 0 )
                     return -1 ;
               }

               /* write the current memo entry to the given position */
               if ( memo4fileDump( f4memo, *memoIdPtr, ptr, lenWrite, memoLen, type ) < 0 )
                  return -1 ;
            }
            else
            {
               FILE4LONG pos ;
               /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
               file4longAssign( pos, *memoIdPtr, 0L ) ;
               file4longMultiply( pos, effectiveBlockSize ) ;
               file4longAdd( &pos, offset + sizeof( MEMO4BLOCK ) ) ;
               // AS May 28/02 - if block perprocessing is enabled, we need to write on the block boundary
               #ifdef S4PREPROCESS_FILE
                  return memo4fileWriteBoundary( f4memo, ptr, lenWrite, pos ) ;
               #else
                  return file4writeInternal( &f4memo->file, pos, ptr, lenWrite ) ;
               #endif
            }

            return 0 ;
         }
      #endif /* S4MFOX */



      #ifdef S4MFOX
         int memo4fileWrite( MEMO4FILE *f4memo, long *memoIdPtr, const char *ptr, unsigned ptrLen )
         {
            // AS June 19/02 - support added for compression...
            long mType = 1L ;

            #ifdef S4COMPRESS
               assert5port( "added compressed memo entries support" ) ;
               if ( f4memo->data->compressedMemosSupported == 1 )
               {
                  unsigned short effectiveBlockSize ;

                  if ( f4memo->blockSize == 0 )
                     effectiveBlockSize = 1 ;
                  else
                     effectiveBlockSize = f4memo->blockSize ;

                  if ( ptrLen > effectiveBlockSize )  // if the memo is > 1 memo block, then compress it to save space
                  {
                     CODE4 *c4 = f4memo->data->c4 ;
                     unsigned long mLen = (long)(ptrLen * 1.001 + 1) + 12 + sizeof( long ) ;  // extra bytes required - long for length, extra for zlib...
                     if ( mLen > c4->compressedMemosBufferLen )
                     {
                        if ( c4->compressedMemosBuffer != 0 )
                        {
                           u4free( c4->compressedMemosBuffer ) ;
                           c4->compressedMemosBuffer = 0 ;
                           c4->compressedMemosBufferLen = 0 ;
                        }
                        c4->compressedMemosBuffer = u4allocFree( c4, mLen  ) ;
                        if ( c4->compressedMemosBuffer == 0 )
                           return e4memory ;
                        c4->compressedMemosBufferLen = mLen ;
                     }
                     unsigned long outLen = c4->compressedMemosBufferLen ;
                     // AS May 13/04 - support configureable compression
                     int rc = c4compress( c4, c4->compressedMemosBuffer, &outLen, ptr, ptrLen, c4getFileCompressLevel( c4 ), 1 ) ;
                     if ( rc == r4success )  // not critical if doesn't work, just don't us if fails
                     {
                        ptr = (const char *)c4->compressedMemosBuffer ;
                        ptrLen = outLen ;
                        mType = 3L ;
                     }
                  }
               }
            #endif /* S4COMPRESS */
            return memo4fileWritePart( f4memo, memoIdPtr, ptr, (long)ptrLen, 0L, ptrLen, mType ) ;
         }
      #else
         #ifdef S4MNDX
            int memo4fileWrite( MEMO4FILE *f4memo, long *memoIdPtr, const char *ptr, const unsigned ptrLen )
            {
               FILE4LONG pos ;
               #ifndef S4OFF_MULTI
                  #ifndef S4CLIPPER
                     int rc, lockCond ;
                  #endif
               #endif
               MEMO4HEADER mh ;
               long lenRead ;
               char buf[MEMO4SIZE] ;
               int readSize, i ;

               #ifdef E4PARM_LOW
                  if ( memoIdPtr == 0 )
                     return error4( 0, e4parm_null, E95209 ) ;
               #endif

               if ( ptrLen == 0 )
               {
                  *memoIdPtr = 0L ;
                  return 0 ;
               }

               assert5( f4memo->blockSize != 0 ) ;
               unsigned long strNumBlocks = (((unsigned long)ptrLen + f4memo->blockSize-1) / f4memo->blockSize) ;

               if ( *memoIdPtr <= 0L )
                  *memoIdPtr = 0L ;
               else    /* read in old record to see if new entry can fit */
               {
                  readSize = 0 ;
                  file4longAssign( pos, *memoIdPtr * f4memo->blockSize, 0 ) ;

                  do
                  {
                     readSize += MEMO4SIZE ;

                     lenRead = file4readInternal( &f4memo->file, pos, buf, MEMO4SIZE ) ;
                     if ( lenRead <= 0 )
                        return file4readError( &f4memo->file, pos, MEMO4SIZE, "memo4fileWrite()" ) ;

                     for ( i=0 ; ((long) i) < lenRead ; i++ )
                        if ( buf[i] == (char)0x1A )  break ;

                     #ifdef E4MISC
                        if ( buf[i] != (char)0x1A && lenRead != MEMO4SIZE )
                           return error4( f4memo->file.codeBase, e4info, E85203 ) ;
                     #endif

                     file4longAssign( pos, file4longGetLo( pos ) + MEMO4SIZE, 0 ) ;
                  } while ( i >= MEMO4SIZE && buf[i] != (char) 0x1A ) ;  /* Continue if Esc is not located */

                  if ( ((unsigned)readSize) <= ptrLen )   /* there is not room */
                     *memoIdPtr = 0 ;
               }

               if ( *memoIdPtr == 0 )   /* add entry at eof */
               {
                  #if !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
                     lockCond = f4memo->data->memoFile.fileLock ;
                     rc = memo4fileLock( &f4memo->data->memoFile ) ;
                     if ( rc )
                        return rc ;
                  #endif

                  file4longAssign( pos, 0, 0 ) ;
                  lenRead = file4readInternal( &f4memo->file, pos, &mh, sizeof( mh ) ) ;
                  #ifdef S4BYTE_SWAP
                     mh.nextBlock = x4reverseLong( (void *)&mh.nextBlock ) ;
                  #endif
                  if ( error4code( f4memo->data->c4 ) < 0 )
                  {
                     #if !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
                        if ( !lockCond )
                           memo4fileUnlock( &f4memo->data->memoFile ) ;
                     #endif
                     return -1 ;
                  }

                  if ( lenRead != sizeof( mh ) )
                  {
                     #if !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
                        if ( !lockCond )
                           memo4fileUnlock( &f4memo->data->memoFile ) ;
                     #endif
                     file4longAssign( pos, 0, 0 ) ;
                     return file4readError( &f4memo->file, pos, sizeof( mh ), "memo4fileWrite()" ) ;
                  }

                  *memoIdPtr = mh.nextBlock ;
                  mh.nextBlock = *memoIdPtr + strNumBlocks ;
                  #ifdef S4BYTE_SWAP
                     mh.nextBlock = x4reverseLong( (void *)&mh.nextBlock ) ;
                  #endif

                  file4longAssign( pos, 0, 0 ) ;
                  file4writeInternal( &f4memo->file, pos, &mh, sizeof( mh ) ) ;

                  #if !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
                     if ( !lockCond )
                        memo4fileUnlock( &f4memo->data->memoFile ) ;
                  #endif

                  #ifdef S4BYTE_SWAP
                     mh.nextBlock = x4reverseLong( (void *)&mh.nextBlock ) ;
                  #endif
               }


               if ( memo4fileDump( f4memo, *memoIdPtr, ptr, ptrLen, ptrLen ) < 0 )
                  return -1 ;

               return 0 ;
            }
         #else
            int memo4fileWrite( MEMO4FILE *f4memo, long *memoIdPtr, const char *ptr, const unsigned ptrLen )
            {
               #ifdef E4PARM_LOW
                  if ( memoIdPtr == 0 )
                     return error4( 0, e4parm_null, E95209 ) ;
               #endif

               MEMO4CHAIN_ENTRY newEntry ;
               memset( (void *)&newEntry, 0, sizeof(newEntry) ) ;
               newEntry.blockNo = *memoIdPtr ;

               int strWritten = 0 ;  // is set to true when we have found a position for the new memo entry
               if ( ptrLen == 0 )
               {
                  strWritten = 1 ;
                  *memoIdPtr = 0 ;
               }

               /* Initialize information about the old memo entry into the new entry */
               if ( newEntry.blockNo <= 0L )
               {
                  if ( strWritten )
                  {
                     *memoIdPtr = 0L ;
                     return 0 ;
                  }
                  newEntry.num = 0 ;
               }
               else
               {
                  FILE4LONG pos ;
                  file4longAssign( pos, newEntry.blockNo, 0 ) ;
                  assert5( f4memo->blockSize != 0 ) ;
                  file4longMultiply( pos, f4memo->blockSize ) ;

                  MEMO4BLOCK oldMemoBlock ;
                  file4readAllInternal( &f4memo->file, pos, (char *)&oldMemoBlock, sizeof(oldMemoBlock) ) ;
                  #ifdef S4BYTE_SWAP
                     oldMemoBlock.startPos = x4reverseShort( (void *)&oldMemoBlock.startPos ) ;
                     oldMemoBlock.numChars = x4reverseLong( (void *)&oldMemoBlock.numChars ) ;
                  #endif
                  // AS 01/17/01 - If it is a valid memo block, should have '-1' in the '-1'
                  // area of the block.  This is just a test for memo block validity.
                  // was happening that transaction rollback was using the 'old' record's memo
                  // entries even though they had gone out of date and were on the free chain.
                  assert5( oldMemoBlock.minusOne == -1 ) ;

                  newEntry.num = (unsigned)(((long)oldMemoBlock.numChars + (long)f4memo->blockSize-1)/ f4memo->blockSize ) ;
               }

               // strNumBlocks is the # of memo blocks required to store the new memo entry
               unsigned long strNumBlocks = (((unsigned long)ptrLen + 2 * (sizeof(short)) + (sizeof(S4LONG)) + f4memo->blockSize - 1) / f4memo->blockSize ) ;

               if ( newEntry.num >= strNumBlocks && !strWritten )
               {
                  *memoIdPtr = newEntry.blockNo + newEntry.num - strNumBlocks ;
                  if ( memo4fileDump( f4memo, *memoIdPtr, ptr, ptrLen, ptrLen ) < 0 )
                     return -1 ;

                  strWritten = 1 ;
                  if ( newEntry.num == strNumBlocks )
                     return 0 ;

                  newEntry.num -= strNumBlocks ;
               }

               // if here, then we have to find a spot to put the entry (doesn't fit in existing spot)
               // there may or may not be an old memo entry at this point.

               #if !defined( S4OFF_MULTI )
                  int lockCond = f4memo->data->memoFile.fileLock ;
                  int rc = memo4fileLock( &f4memo->data->memoFile ) ;
                  if ( rc )
                     return rc ;
               #endif

               /* Initialize 'chain' */
               MEMO4CHAIN_ENTRY cur ;
               memset( (void *)&cur, 0, sizeof(cur) ) ;

               MEMO4CHAIN_ENTRY prev ;
               memset( (void *)&prev, 0, sizeof(prev) ) ;

               // scan through the chain of free blocks.  If a useable entry is found, use it.
               // otherwise use the 'eof' entry.  At the same time, look for a position to insert the
               // 'old' entry if it exists, into the chain of free blocks.
               // use 'prev' to hold the previous memo free block entry, and 'cur' to hold the
               // current memo free block entry
               // newEntry contains the current (old) entry information for the memo data that is
               // being replaced.
               for( ;; )
               {
                  if ( error4code( f4memo->data->c4 ) < 0 )
                  {
                     #if !defined( S4OFF_MULTI )
                        if ( !lockCond )
                           memo4fileUnlock( &f4memo->data->memoFile ) ;
                     #endif
                     return -1 ;
                  }

                  // flush out any changes to the old 'previous' entry
                  memo4fileChainFlush( f4memo, &prev ) ;

                  // save the information about the old 'previous' entry
                  long prevPrevEntry = prev.blockNo ;
                  long prevPrevNum = prev.num ;

                  // the old 'current' entry becomes the new 'previous' entry
                  memcpy( (void *)&prev, (void *)&cur, sizeof(prev) ) ;

                  // the 'free block' chain is kept in order from the start of the file
                  // if there was an old memo entry and the previous block 'next free' entry is greater
                  // than our own, then we have found the place to insert the old block into the chain.
                  if ( ( newEntry.blockNo > 0 ) && ( prev.next > newEntry.blockNo ) )
                  {
                     // put the old entry into the chain here...
                     memcpy( (void *)&cur, (void *)&newEntry, sizeof(cur) ) ;
                     newEntry.blockNo = 0 ;
                     cur.next  = prev.next ;
                     prev.next = cur.blockNo ;
                     cur.toDisk = prev.toDisk = 1 ;
                  }
                  else  // now update the 'current' position to the next free block in the chain...
                     memo4fileChainSkip( f4memo, &cur ) ;

                  /* See if the previous free entry and the current free entry can be combined to make
                     a larger free entry. */
                  if ( prev.blockNo + prev.num == cur.blockNo && prev.num )
                  {
                     // combine the 2 blocks to make a larger free entry
                     /* 'cur' becomes the combined groups. */
                     prev.toDisk = 0 ;
                     cur.toDisk  = 1 ;

                     cur.blockNo = prev.blockNo ;
                     if ( cur.num >= 0 )
                        cur.num  += prev.num ;
                     prev.blockNo = prevPrevEntry ;
                     prev.num = prevPrevNum ;
                  }

                  // if we have already found a place for the new memo (i.e. we are only looking for a place
                  // to put the old memo entry)
                  if ( strWritten )
                  {
                     // if there is no 'old' memo entry, then we are done after we flush the blocks which
                     // may have just placed the old memo entry into the free chain.
                     if ( newEntry.blockNo == 0 )
                     {
                        memo4fileChainFlush( f4memo, &prev ) ;
                        memo4fileChainFlush( f4memo, &cur ) ;
                        #if !defined( S4OFF_MULTI )
                           if ( !lockCond )
                              memo4fileUnlock( &f4memo->data->memoFile ) ;
                        #endif
                        return 0 ;
                     }
                  }
                  else  /* 'str' is not yet written, try the current entry */
                  {
                     if ( cur.next == -1 )  // no more free blocks in chain
                     {
                        // means that we are to use the end of file for write.
                        // at eof we always have enough room, so put it...
                        cur.num = strNumBlocks ;
                     }

                     // if the current block is >= size of that required, use it.
                     if ( cur.num >= strNumBlocks )
                     {
                        cur.num -= strNumBlocks ;  // reduce the # of free blocks by the # we require
                        *memoIdPtr = cur.blockNo + cur.num ;
                        memo4fileDump( f4memo, *memoIdPtr, ptr, ptrLen, ptrLen ) ;  // actually write the new memo out to disk
                        if ( cur.next == -1 ) /* if writing end of file */
                        {
                           /* For dBASE IV compatibility need to set file length to be a multiple of the blockSize... */
                           long fileLen  = file4longGetLo( file4lenLow( &f4memo->file ) ) ;
                           assert5( f4memo->blockSize != 0 ) ;
                           long extraLen = f4memo->blockSize -  fileLen % f4memo->blockSize ;
                           if ( extraLen != f4memo->blockSize )
                           {
                              FILE4LONG pos ;
                              file4longAssign( pos, fileLen + extraLen, 0 ) ;
                              file4lenSetLow( &f4memo->file, pos ) ;
                           }
                        }

                        strWritten = 1 ;  // we have found a position to write the new block.

                        if ( cur.num == 0 )  // means the current block contains no free room left (used it all with the new memo)
                        {
                           if ( cur.next == -1 ) /* End of file */
                              prev.next = cur.blockNo + strNumBlocks ;
                           else  // not end of file
                           {
                              // we need to do some massaging here.  We want to remove the 'current'
                              // block from the list of free blocks since we just used it for the new
                              // memo entry.
                              // we need to change the 'previous' entry to have a new 'next block'
                              // entry == to the current blocks 'next' entry.
                              prev.next = cur.next ;
                              // we also need to consider that we may still need to skip through the
                              // list of available blocks in order to place the 'old memo' entry
                              // into the list of available blocks.
                              // in this case, massage the current entry to be equivalent to the
                              // now 'previous' entry.  Since current to disk will be set to 0, this
                              // effectively means that we continue again from the new 'previous'
                              // location.
                              memcpy( &cur, &prev, sizeof( cur ) ) ;
                           }
                           prev.toDisk = 1 ;
                           cur.toDisk = 0 ;
                        }
                        else
                           cur.toDisk = 1 ;
                     }
                  }
               }
            }
         #endif /* S4MNDX */
      #endif /* !S4MFOX */
   #endif /* !S4OFF_WRITE */
#endif /* !S4OFF_MEMO && !S4CLIENT */
