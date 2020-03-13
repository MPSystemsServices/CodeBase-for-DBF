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

/* f4file.c (c)Copyright Sequiter Software Inc., 1988-2006.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

#if (defined( _MSC_VER ) && defined( S4WINDOWS ) && !defined( S4WINCE )) || defined( __SC__ )
   #include <dos.h>
#endif

#if defined( S4WINTEL ) && !defined( S4IBMOS2 )
   #ifndef S4WINCE   /* LY 2002/11/12 : moved from #include <sys\types.h> below */
      #if !defined( S4OFF_MULTI ) && !defined( __TURBOC__ )
         #include <sys\locking.h>
         #define S4LOCKING
      #endif
      #if defined( _MSC_VER )
         #include <sys\types.h>
         #include <sys\locking.h>
      #endif
   #endif
#endif

// AS May 17/04 - client/server functionality to copmress the data file...
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE )
   #include "zlib.h"
#endif

// AS Apr 11/05 - enabled general access to thse functions...
// AS Sep 8/04 - changed for debugging support
#if defined( S4WIN32 )
   void critical4sectionInit( CRITICAL4SECTION *criticalSection )
   {
      memset( criticalSection, 0, sizeof(CRITICAL4SECTION) ) ;
      InitializeCriticalSection( &(criticalSection->section) ) ;
   }



   void critical4sectionInitUndo( CRITICAL4SECTION *criticalSection )
   {
      DeleteCriticalSection( &(criticalSection->section) ) ;
      memset( criticalSection, 0, sizeof(CRITICAL4SECTION) ) ;
   }



   void critical4sectionEnter( CRITICAL4SECTION *criticalSection )
   {
      EnterCriticalSection( &(criticalSection->section) ) ;
      if ( criticalSection->count == 0 )
      {
         criticalSection->threadOwner = GetCurrentThreadId() ;
      }
      #ifdef E4ANALYZE
         else
         {
            assert5( criticalSection->threadOwner == GetCurrentThreadId() ) ;
         }
      #endif
      criticalSection->count++ ;
   }



   void critical4sectionLeave( CRITICAL4SECTION *criticalSection )
   {
      critical4sectionVerify( criticalSection ) ;
      criticalSection->count-- ;
      if ( criticalSection->count == 0 )
         criticalSection->threadOwner = 0 ;
      LeaveCriticalSection( &(criticalSection->section) ) ;
   }
#endif

#ifdef S4NO_FILELENGTH
   #if  defined( S4MACINTOSH )
      FILE4LONG S4FUNCTION u4filelength( int hand )
      {
         long fileLen ;

         if ( GetEOF(hand, &fileLen) != 0 )
            return error4( 0, e4result, E90603 ) ;

         return fileLen ;
      }
   #elif defined( S4WIN32 )



      FILE4LONG S4FUNCTION u4filelength( HANDLE hand )
      {
         FILE4LONG rc ;

         /* AS 07/07/99 Do a little more checking about long files to ensure a failure if
            the S4FILE_EXTENDED is not defined */

         unsigned long hiAddress ;
         unsigned long loAddress ;

         // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
         // Microsoft knowledgebase article #811693
         #ifdef S4WINCE
            for ( short numTries = 0 ;; numTries++ )
            {
         #endif
               loAddress = (long)GetFileSize( hand, &hiAddress ) ;
         #ifdef S4WINCE
               if ( loAddress == INVALID_FILE_SIZE )
               {
                  if ( numTries > 10 )  // wait up to 5 seconds
                     break ;
                  Bool5 doBreak = 0 ;
                  DWORD err = GetLastError() ;
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
         file4longAssign( rc, loAddress, hiAddress ) ;
         #ifndef S4FILE_EXTENDED
            if ( hiAddress != 0 || loAddress > 0x7FFFFFFF )  /* means the file is > 2 Gigs, but CodeBase not compiled to handle that... */
            {
               error4( 0, e4len, E90603 ) ;
               file4longAssignError( rc ) ;
            }
         #endif
         file4longCheckError( rc ) ;

         #ifdef E4PARM_LOW
            if ( file4longError( rc ) == (unsigned long)-1L )
            {
               error4( 0, e4result, E90603 ) ;
               file4longAssignError( rc ) ;
            }
         #endif

         return rc ;
      }
   #elif defined( S4UNIX )



      #include  <sys/types.h>
      #include  <sys/stat.h>

      FILE4LONG S4FUNCTION u4filelength( int hand )
      {
         struct stat strStat ;

         if (fstat( hand, &strStat ) )
            return error4( 0, e4result, E90603 ) ;

         /* LY 2001/07/18 : changed from (long) for large file support */
         return( (FILE4LONG) strStat.st_size ) ;
      }

      #if defined(S4FILE_EXTENDED) && !defined(S464BIT)  /* LY 00/06/01 : lost definitions from change on 98/12/22 */
         #ifdef S4BYTE_SWAP
            unsigned long file4longGetLo(FILE4LONG f1 ) { return *( ((long *)&f1)+1 ) ; }
         #else
            unsigned long file4longGetLo(FILE4LONG f1 ) { return *( ((long *)&f1) ) ; }
         #endif
      #endif
   #elif defined( S4PALM )



      FILE4LONG S4FUNCTION u4filelength( FileHand hand )
      {
         long fileLen,rc;
         rc = FileTell(hand,&fileLen,0);
         if (rc == -1)
            return error4( 0, e4result, E90603 ) ;

         return fileLen;
      }
   #elif defined( _MSC_VER )



      FILE4LONG S4FUNCTION u4filelength( int hand )
      {
         /*
            fstat() does not work correctly under Microsoft C++ 1.5 on a
            Novell drive so lseek() is used instead.
            On a Novell drive fstat() always returns the file length when
            the file was opened and does not reflect appends from other
            users.
         */
         long length, current ;

         current = lseek( hand, 0, SEEK_CUR ) ;
         length = lseek( hand, 0, SEEK_END ) ;
         lseek( hand, current, SEEK_SET ) ;

         return length ;
      }
   #else



      #include  <sys\stat.h>

      FILE4LONG S4FUNCTION u4filelength( int hand )
      {
         struct stat strStat ;

         if (fstat( hand, &strStat ) )
            return error4( 0, e4result, E90603 ) ;

         return( (long) strStat.st_size ) ;
      }
   #endif
#endif



#ifdef S4MACINTOSH
   //CJ - 17/12/99 - code completely rewritten to solve problems with perfromance with Macintosh.
   // reduced the need to find the length of the file each time.
   long MAClseek(int hand, long offset, int fromWhere, int extend )
   {
      long returnCode;

      returnCode = SetFPos(hand, fsFromStart, offset ) ;
      if (returnCode == 0)
         return offset ;
      else if ( returnCode == eofErr ) //offset is larger than the size of the file.
      {
         //CJ -31/01/00- When the file is on a network volume, and another user extends the length
         //SetFPos() does not work correctly unless GetEOF() is used first.
         //Using GetEOF() will reduce performance but until SetFPos works correctly GetEOF() will
         //need to be used.
         long fileLen ;
         GetEOF( hand, &fileLen) ;

         returnCode = SetFPos(hand, fsFromStart, offset ) ;
         if (returnCode == 0)
            return( offset ) ;
         else if ( returnCode == eofErr )
         {
            if (extend)  // if extend is set then set the size of the file and reposition to offset
            {
               SetEOF( hand, offset ) ;
               if ( SetFPos(hand, fsFromStart, offset ) == 0 )
                  return offset ;
               else
                  return -1L ;
            }
            else // otherwise set the file pointer to the end of the file.
            {
               /* LY 2002/06/18 : update offset to return actual value */
               if ( SetFPos(hand, fsFromLEOF, 0 ) == 0 &&
                  GetFPos( hand, &offset ) == 0 )
                  return offset ;
               else
                  return -1L ;
            }
         }
      }
      // CJ - if the function has not returned prevoiusly then SetFPos failed and return -1
      return ( -1L ) ;
   }
#endif



#ifdef S4LSEEK
   long f4lseek(FILE4 *f4, long offset, int fromWhere, int extend )
   {
      /* if extend is set, file is extended, else lseek to EOF */
      long fileLen ;

      if ( offset != 0 )
      {
         fileLen = u4filelength( f4->hand ) ;

         if (extend)
         {
            if ( fileLen < offset )
               file4changeSize( f4, offset ) ;
         }
         else
            if ( fileLen < offset )
            {
               if ( lseek( f4->hand, fileLen, 0 ) )
                  return offset ;
               else
                  return -1L ;
            }
      }
      return lseek( f4->hand, offset, fromWhere ) ;
   }
#endif



#ifdef S4NO_CHSIZE
   #define E4MAXLINE 129   /* maximum file path length */

   #if  defined( S4MACINTOSH )
      int S4FUNCTION file4changeSize( FILE4 *f4, FILE4LONG size )
      {
         if ( SetEOF( f4->hand, size ) != 0 )
            return error4( 0, e4result, E90604 ) ;

         return 0 ;
      }
   #elif defined( _MSC_VER )


      #ifdef S4WINDOWS
         int S4FUNCTION file4changeSize( FILE4 *f4, FILE4LONG size )
         {
            unsigned int rc, num ;
            char a ;

            a = (char)0x00 ;
            _llseek( f4->hand, size, SEEK_SET ) ;

            rc = _dos_write( f4->hand, &a, 0, &num ) ;
            if ( num != 0 || rc != 0 )
               return error4( f4->codeBase, e4lenSet, E90604 ) ;

            return 0 ;
         }
      #endif
   #elif defined( S4PALM )


      int S4FUNCTION file4changeSize( FILE4 *f4, FILE4LONG size )
      {
         const FILE4LONG currentLen = file4len(f4);

         if (size != currentLen)
         {
            Err rc;
            if (size > currentLen)
            {
               // make bigger
               rc = FileSeek(f4->hand,size,fileOriginBeginning);
            }

            if (size < currentLen)
            {
               // make smaller
               rc = FileTruncate(f4->hand,size);
            }

            if (rc != 0)
            {
               switch (rc)
               {
                  case fileErrMemError:
                     return error4(f4->codeBase,e4memory,E90604);
                  case fileErrInvalidParam:
                  case fileErrInvalidDescriptor:
                     return error4(f4->codeBase,e4parm,E90604);
                  case fileErrReadOnly:
                     return error4(f4->codeBase,e4lenSet,E80606);
                  default:
                     return error4(f4->codeBase,e4lenSet,E90604);
               }
            }
         }
         return r4success;
      }
   #else


      int S4FUNCTION file4changeSize( FILE4 *f4, FILE4LONG size )
      {
         return ftruncate( f4->hand, size ) ;
      }
   #endif
#endif   /* ifdef S4NO_CHSIZE */



// AS May 17/04 - client/server functionality to copmress the data file...
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   // AS Aug 1/03 - Large file support
   static FILE4LONG compress4handlerFileLen( COMPRESS4HANDLER *compress )
   {
      assert5( compress->isLongOrCompress >= 0 && compress->isLongOrCompress <= 2 ) ;
      switch( compress->isLongOrCompress )
      {
         case 0:
            {
               assert5( compress->shortCompress != 0 ) ;
               FILE4LONG len ;
               file4longAssign( len, compress->shortCompress->fileLen, 0 ) ;
               return len ;
            }
            break ;
         case 1:
            assert5( compress->longCompress != 0 ) ;
            return compress->longCompress->fileLen ;
            break ;
         default:  // case 2
            assert5( compress->writeCompress != 0 ) ;
            return compress->writeCompress->fileLen ;
            break ;
      }
   }


   static FILE4LONG compress4writeArrayPosition( COMPRESS4WRITE_HEADER *writeCompress, FILE4 *f4, unsigned long blockOn )
   {
      // returns the physical disk location of the given array position (for reading/writing to disk)

      // first find the location of the array information...
      COMPRESS4WRITE_BLOCK *writeBlock = &(writeCompress->firstArrayBlock) ;
      CODE4 *c4 = f4->codeBase ;
      FILE4LONG writeArrayPosition ;
      file4longAssign( writeArrayPosition, f4->compressHeaderOffset + sizeof( COMPRESS4WRITE_HEADER ), 0 ) ;  // for the first entry, the array info starts after the write header (fully written to disk)
      unsigned long arrayOffset = blockOn ;


      // internally there is an array of compressed block information (i.e. an array of block pointers for doing lookups...
      // so if we want to look up the address of blockNo 1000, we look in the array of blocks).  However, since the file size can
      // increase instead of keeping a single array of these blocks, over time we allocate more arrays of these blocks.  They
      // get put into a linked list.  What we need to do here is find the array (from the linked list) that contains the block
      // identified by 'blockOn'.  This is fairly straightforward, we just cycle through the list of blocks until we find the
      // one that holds our block (each item in the list is sequential)
      Bool5 updatedArrayPosition = 0 ;   // we have one chance to update the array position and retry the loop if required...
      for( unsigned long entryCount = 0 ;; )
      {
         if ( writeBlock == 0 )  // corruption
         {
            if ( updatedArrayPosition == 1 )
            {
               error4( c4, e4info, E90619 ) ;
               file4longAssignError( writeArrayPosition ) ;
               break ;
            }
            // AS Jan 18/06 this doesn't necessarily indicate corruption.  Instead what may have happened is that another user added an
            // array block.  Therefore, what we actually need to do is revise our array from disk and try again...
            // file4longAssign( writeArrayPosition, f4->compressHeaderOffset + sizeof( COMPRESS4WRITE_HEADER ), 0 ) ;  // for the first entry, the array info starts after the write header (fully written to disk)
            file4longAssign( writeArrayPosition, f4->compressHeaderOffset, 0 ) ;
            file4compressInitArrayOffsets( f4, writeCompress, writeArrayPosition ) ;
            entryCount = 0 ;
            writeBlock = &(writeCompress->firstArrayBlock) ;
            updatedArrayPosition = 1 ;
            arrayOffset = blockOn ;
            continue ;
         }
         entryCount += writeBlock->numEntries ;
         if ( blockOn < entryCount )
         {
            // found the array, so update the position - we want to write out the actual entry used
            // how do we find that location?  We need to add the location of the array to the base write position
            file4longAdd( &writeArrayPosition, arrayOffset * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ) ;
            // unsigned urc = file4writeLowDoInternal( f4, writeArrayPosition, entry, sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ) ;
            // if ( blockOn >= writeCompress->totalBlocksUsed )  // update the block count as well
            // {
            //    unsigned long numToAdd = blockOn - writeCompress->totalBlocksUsed + 1 ;
            //    writeCompress->totalBlocksUsed += numToAdd ;
            //    FILE4LONG writePos ;
            //    file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + sizeof( unsigned long ), 0 ) ;
            //    file4writeLowDoInternal( f4, writePos, &writeCompress->totalBlocksUsed, sizeof( writeCompress->totalBlocksUsed ) ) ;  // write the updated free chain entry to disk
            //    assert5( writeCompress->totalBlocksUsed == blockOn + 1 ) ;
            // }
            break ;
         }
         writeArrayPosition = writeBlock->nextBlock ;
         file4longAdd( &writeArrayPosition, 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ) ) ; // position to after the COMPRESS4WRITE_BLOCK
         arrayOffset -= writeBlock->numEntries ;
         writeBlock = writeBlock->nextBlockPtr ;
      }

      return writeArrayPosition ;
   }



   // AS Aug 1/03 - Large file support
   // AS Jul 8/04 - compress write support
   FILE4LONG compress4handlerOffsetGet( FILE4 *f4, unsigned long blockIndex )
   {
      // returns the physical file offset of the compressed block (i.e. where on disk to actually physically write the block)
      critical4sectionVerify( &f4->critical4file ) ;
      COMPRESS4HANDLER *compress = f4->compressInfo ;
      assert5( compress->isLongOrCompress >= 0 && compress->isLongOrCompress <= 2 ) ;
      switch( compress->isLongOrCompress )
      {
         case 0:  // means read-only short file (< 4 Gigs)
            {
               assert5( compress->shortCompress != 0 ) ;
               assert5( blockIndex <= (unsigned long)compress->shortCompress->numOffsets ) ;
               FILE4LONG len ;
               file4longAssign( len, compress->shortCompress->offsets[blockIndex], 0 ) ;
               return len ;
            }
         case 1:  // means read-only large file (> 4 Gigs)
            assert5( compress->longCompress != 0 ) ;
            assert5( blockIndex <= (unsigned long)compress->longCompress->numOffsets ) ;
            return compress->longCompress->offsets[blockIndex] ;
         default: // means we support read/write to the compressed file
            {
               COMPRESS4WRITE_HEADER *writeCompress = compress->writeCompress ;
               assert5( writeCompress != 0 ) ;
               assert5( blockIndex < writeCompress->totalBlocksUsed ) ;
               #ifndef S4OFF_MULTI  // LY Feb 3/05
               // if the file is opened fully shared, we need to read the information from disk every time (in case someone else
               // updated)  -- this is to avoid decompression errors...
                  if ( f4->lowAccessMode == OPEN4DENY_NONE )   // if no-one else can write to the file, we can use the internal buffer
                  {
                     FILE4LONG readPos = compress4writeArrayPosition( writeCompress, f4, blockIndex ) ;
                     if ( file4readOnBoundary( f4, readPos, &(writeCompress->firstArrayBlock.writeArray[blockIndex]), sizeof(COMPRESS4WRITE_ARRAY_ENTRY) ) != sizeof(COMPRESS4WRITE_ARRAY_ENTRY) )
                     {
                        FILE4LONG out ;
                        file4longAssignError( out ) ;
                        return out ;
                     }
                  }
               #endif

               return writeCompress->firstArrayBlock.writeArray[blockIndex].position ;
            }
      }
   }
#endif /* #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS ) */


FILE4LONG S4FUNCTION file4lenLow( FILE4 *f4 )
{
   FILE4LONG lrc ;

   #ifdef E4PARM_HIGH
      if ( f4 == 0 )
      {
         error4( 0, e4parm_null, E90605 ) ;
         file4longAssignError( lrc ) ;
         return lrc ;
      }
   #endif
   // AS Sep 3/03 - removed check, made to assertion later - to avoid case where file is not created yet (temp)

   #ifndef S4OFF_OPTIMIZE
      // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
      if ( file4getTemporary( f4 ) == 1 && f4->fileCreated == 0 )
      {
         /* 04/24/96 AS fix for c/s t4commit.c */
         if ( file4longError( f4->len ) == (unsigned long)-1L )
         {
            /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
            file4longAssign( lrc, 0, 0L ) ;
            return lrc ;
         }
         else
            return f4->len ;
      }
      if ( f4->doBuffer && file4longError( f4->len ) != (unsigned long)-1L )
         lrc = f4->len ;
      else
      {
   #endif
         assert5 ( f4->hand != INVALID4HANDLE ) ;
         // AS May 17/04 - client/server functionality to copmress the data file...
         #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
            if ( f4->compressInfo != 0 ) // file is compressed, return the length as indicated in the compressed header
            {
               // AS Jul 31/03 - Support for large file support
               lrc = compress4handlerFileLen( f4->compressInfo ) ;
            }
            else
         #endif
            {
               lrc = u4filelength( f4->hand ) ;
               if ( file4longError( lrc ) == (unsigned long)-1L )
                  error4describe( f4->codeBase, e4len, E90605, f4->name, 0, 0 ) ;
            }
   #ifndef S4OFF_OPTIMIZE
      }
   #endif

   #if defined( S4FILE_EXTENDED ) && !defined( S4OFF_MULTI )
      /* AS 07/07/99 -> do a little more checking - if file is large but large file handling is
         not enabled - not exclusive..., produce an error (i.e. to avoid locking errors)
      */
      if ( file4longGetHi( lrc ) != 0 || file4longGetLo( lrc ) > 0x7FFFFFFF )
      {
         // AS Oct 31/02 - Should be if other cannot write, not if can write
         if ( f4->lowAccessMode == OPEN4DENY_NONE )
            if ( f4->codeBase->largeFileOffset != S4LARGE_FILE_OFFSET )
            {
               /* case where we have a large shared file */
               error4describe( f4->codeBase, e4len, E90605, f4->name, 0, 0 ) ;
            }
      }
   #endif /* defined( S4FILE_EXTENDED ) && !defined( S4OFF_MULTI ) */

   // AS 03/08/01 - add so that we can buffer file length on reading as well... otherwise we were
   // only setting on the file4lenSet command...
   #ifndef S4OFF_OPTIMIZE
      if ( f4->doBuffer && f4->bufferWrites )
      {
         /* if len is -1 but bufferWrites true, may still have buffered data
            which must be removed, so get disk file length for reference */
         if ( f4->bufferWrites == 1 && f4->fileCreated != 0 && error4code( f4->codeBase ) == 0 )
         {
            f4->len = lrc ;
            #if defined( E4ANALYZE ) || defined( S4TESTING )
               // LY Feb 2/05 : added #if's to avoid compiler errors
               #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
                  if ( f4->compressInfo != 0 && f4->compressInfo->writeCompress != 0 && file4longGreaterEq( f4->len, f4->compressHeaderOffset ) )
                  {
                     // ensure the blocks have been extended out (was not working right)
                     FILE4LONG lastBlockLong = f4->len ;
                     file4longSubtract( &lastBlockLong, (f4->compressHeaderOffset) ) ;
                     file4longDivide( lastBlockLong, f4->actualBlockSize ) ;
                     unsigned long lastBlock = file4longGetLo( lastBlockLong ) ;
                     if ( lastBlock >= f4->compressInfo->writeCompress->totalBlocks )
                     {
                        memset( 0, '0', 4 ) ;  // force a gpf
                        assert5( 0 ) ;
                     }
                  }
               #endif
            #endif
            // AS Apr 15/04 - support for optimizing large files
            // if ( file4longGetHi( f4->len ) != 0 )  // no support for bufferring large files...
            //    error4( f4->codeBase, e4result, E90606 ) ;
         }
      }
   #endif

   return lrc ;
}



unsigned long S4FUNCTION file4lenLow2( FILE4 S4PTR *f4 )
{
   return file4longGetLo( file4lenLow( f4 ) ) ;
}



// AS Jun 28/04 - support for compressed writing - various changes in this function
static int file4lenSetLowDo( FILE4 *f4, FILE4LONG newLen )
{
   CODE4 *c4 = f4->codeBase ;
   int rc ;

   #if defined( S4MACINTOSH )
      rc = SetEOF( f4->hand, newLen ) ;
   #elif defined( S4WIN32 )
      // AS Dec 11/02 - Renamed for clarification
      #ifdef S4DELAY_WRITE_MT
         critical4sectionEnter( &f4->critical4file ) ;
      #endif
      #ifdef S4WRITE_DELAY
         if ( l4numNodes( &f4->delayWriteFileList ) != 0 )
         {
            // AS 04/25/00 --> critical sectioning missing...
            critical4sectionEnter( &c4->critical4delayWriteList ) ;
            LINK4 *delayLink ;
            for ( delayLink = (LINK4 *)l4first( &f4->delayWriteFileList ) ;; )
            {
               if ( delayLink == 0 )
                  break ;
               FILE4WRITE_DELAY *writeDelay = (FILE4WRITE_DELAY *)(delayLink - 1 ) ;
               delayLink = (LINK4 *)l4next( &f4->delayWriteFileList, delayLink ) ;
               /* now, if the delay-write is without the boundaries of
                  the len-set, then remove that part */

               /* LY 4/28/99 : replaced binary operators with file4long*** */
               FILE4LONG tLong ;
               file4longAssignLong( tLong, writeDelay->pos ) ;
               file4longAdd( &tLong, writeDelay->len ) ;
               if ( file4longGreaterLong( tLong, newLen ) )
               {
                  /* maybe is being written to disk now, in which case
                     must wait */
                  while ( writeDelay->usageFlag == r4inUse )  /* is being written to disk, just wait until it is done... */
                  {
                     // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
                     // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
                     u4sleep( f4->codeBase ) ;
                  }
                  if ( writeDelay->usageFlag == r4finished ) /* is written to disk, so can just ignore */
                     continue ;
                  if ( file4longGreaterLong( writeDelay->pos, newLen ) )   /* just remove */
                  {
                     writeDelay->status = 0 ;
                     writeDelay->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
                     l4remove( &f4->codeBase->delayWriteList, writeDelay ) ;
                     l4remove( &f4->delayWriteFileList, &writeDelay->fileLink ) ;
                     writeDelay->completionRoutine( writeDelay ) ;
                     mem4free( c4->delayWriteMemory, writeDelay ) ;
                  }
                  else  /* just reduce the length */
                  {
                     file4longAssignLong( tLong, newLen ) ;
                     file4longSubtractLong( &tLong, &writeDelay->pos ) ;
                     writeDelay->len = file4longGetLo( tLong ) ;
                  }
               }
            }
            // AS 04/25/00 --> critical sectioning missing...
            critical4sectionLeave( &c4->critical4delayWriteList ) ;
         }
      #endif
      // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
      // Microsoft knowledgebase article #811693
      DWORD err ;
      DWORD dwres ;
      #ifdef S4WINCE
         for ( short numTries = 0 ;; numTries++ )
         {
      #endif
            dwres = SetFilePointer( (HANDLE)f4->hand, file4longGetLo( newLen ), file4longGetHiAddress( newLen ), FILE_BEGIN ) ;
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
      if ( dwres == (DWORD)-1 && err != NO_ERROR )
      {
         // AS Dec 11/02 - Renamed for clarification
         #ifdef S4DELAY_WRITE_MT
            critical4sectionLeave( &f4->critical4file ) ;
         #endif
         return error4describe( c4, e4lenSet, E90606, f4->name, 0, 0 ) ;
      }
      if ( SetEndOfFile( (HANDLE)f4->hand ) )
         rc = 0 ;
      else
         rc = -1 ;
      // AS Dec 11/02 - Renamed for clarification
      #ifdef S4DELAY_WRITE_MT
         critical4sectionLeave( &f4->critical4file ) ;
      #endif
   #elif defined( __SC__ )
      rc = 0 ;
      dosFlush.x.ax = 0x4200;
      dosFlush.x.bx = f4->hand ;
      memcpy((void *)&dosFlush.x.dx,(void *)&newLen,2);
      memcpy((void *)&dosFlush.x.cx,((char *)&newLen)+2,2);
      intdos( &dosFlush, &dosFlush ) ;
      if ( dosFlush.x.cflag != 0 )
        return error4( c4, e4lenSet, E90606 ) ;
      dosFlush.h.ah = 0x40;
      dosFlush.x.bx = f4->hand ;
      dosFlush.x.cx = 0x00;
      intdos( &dosFlush, &dosFlush ) ;
      if ( dosFlush.x.cflag != 0 )
        return error4( c4, e4lenSet, E90606 ) ;
      rc = 0 ;
   #elif defined( S4NO_CHSIZE )
      rc = file4changeSize( f4, newLen ) ;
   #else
      rc = chsize( f4->hand, newLen ) ;
   #endif

   if ( rc < 0 )
      return error4describe( c4, e4lenSet, E90606, f4->name, (char *)0, (char *)0 ) ;

   return 0 ;
}


// LY Jan 18/05 : added additional switches (avoid compiler errors)
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   static int compress4writeFreeBlock( COMPRESS4WRITE_HEADER *writeCompress, FILE4 *f4, unsigned long blockOn )
   {
      critical4sectionVerify( &f4->critical4file ) ;

      // FILE4LONG writePos = compress4writeArrayPosition( writeCompress, f4, writeCompress->totalBlocks-1 ) ;
      COMPRESS4WRITE_ARRAY_ENTRY *entry = &(writeCompress->firstArrayBlock.writeArray[blockOn]) ;
      // first check the free list
      FILE4LONG nextBlockPos = writeCompress->freeChain ;
      FILE4LONG lastBlockPos ;
      file4longAssign( lastBlockPos, 0, 0 ) ;
      FILE4LONG blockAvail ;
      file4longAssign( blockAvail, 0, 0 ) ;
      COMPRESS4FREE_BLOCK freeBlock, nextBlock, lastBlock ;

      if ( file4longEqualZero( nextBlockPos ) )  // empty chain, add ourselves...
      {
         freeBlock.nextPos = writeCompress->freeChain ;
         freeBlock.blockLength = entry->actualBlockLength ;
         writeCompress->freeChain = entry->position ;
         file4writeLowDoInternal( f4, writeCompress->freeChain, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;  // write the free block to disk
         FILE4LONG writePos ;
         // LY Sep 17/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
         // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ), 0 ) ;
         file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)(&writeCompress->freeChain) - (unsigned long)writeCompress, 0 ) ;
         file4writeLowDoInternal( f4, writePos, &writeCompress->freeChain, sizeof(writeCompress->freeChain) ) ;  // write the updated free chain entry to disk
      }
      else
      {

         for ( ;; )
         {
            if ( file4longEqualZero( nextBlockPos ) )   // means we have reached the end of the chain and didn't find a free block with enough space
            {
               file4longAssign( freeBlock.nextPos, 0, 0 ) ;
               freeBlock.blockLength = entry->actualBlockLength ;
               lastBlock.nextPos = entry->position ;
               assert5( file4longLessEqLong( freeBlock.nextPos, u4filelength( f4->hand ) ) ) ;
               file4writeLowDoInternal( f4, lastBlock.nextPos, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
               assert5( file4longLessEqLong( lastBlock.nextPos, u4filelength( f4->hand ) ) ) ;
               file4writeLowDoInternal( f4, lastBlockPos, &lastBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
               break ;
            }

            unsigned urc = file4readOnBoundary( f4, nextBlockPos, &nextBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
            if ( urc != sizeof( COMPRESS4FREE_BLOCK ) )
               return error4( 0, e4result, E90603 ) ;

            if ( nextBlock.blockLength >= entry->actualBlockLength )  // place the old block
            {
               if ( file4longEqualZero( lastBlockPos ) )  // means put at the top of the chain
               {
                  freeBlock.nextPos = writeCompress->freeChain ;
                  freeBlock.blockLength = entry->actualBlockLength ;
                  writeCompress->freeChain = entry->position ;
                  assert5( file4longLessEqLong( freeBlock.nextPos, u4filelength( f4->hand ) ) ) ;
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
                  freeBlock.blockLength = entry->actualBlockLength ;
                  lastBlock.nextPos = entry->position ;
                  assert5( file4longLessEqLong( freeBlock.nextPos, u4filelength( f4->hand ) ) ) ;
                  file4writeLowDoInternal( f4, lastBlock.nextPos, &freeBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
                  assert5( file4longLessEqLong( lastBlock.nextPos, u4filelength( f4->hand ) ) ) ;
                  file4writeLowDoInternal( f4, lastBlockPos, &lastBlock, sizeof( COMPRESS4FREE_BLOCK ) ) ;
               }
               break ;
            }

            lastBlockPos = nextBlockPos ;
            assert5( sizeof( lastBlock ) == sizeof( nextBlock ) ) ;
            memcpy( &lastBlock, &nextBlock, sizeof( lastBlock ) ) ;
            nextBlockPos = nextBlock.nextPos ;
         }
      }

      return 0 ;
   }


   int file4compressSetLenDo( FILE4 *f4, FILE4LONG newLen )
   {
      critical4sectionVerify( &f4->critical4file ) ;

      // lengthening file...
      // check whether or not we have room in the compressed array handlers...
      FILE4LONG curLen = file4lenLow( f4 ) ;
      FILE4LONG lastBlock = newLen ;
      file4longSubtract( &lastBlock, f4->compressHeaderOffset ) ;
      file4longDivide( lastBlock, f4->actualBlockSize ) ;
      assert5( file4longGetHi( lastBlock ) == 0 ) ;  // the offset must be sizeof long or smaller
      unsigned long lastBlockLo = file4longGetLo( lastBlock ) ;

      COMPRESS4WRITE_HEADER *writeCompress = f4->compressInfo->writeCompress ;

      if ( file4longLessLong( newLen, curLen ) )   // concatenating file
      {
         // put all of the deleted blocks onto the free chain
         assert5( lastBlockLo <= writeCompress->totalBlocksUsed ) ;
         while ( (writeCompress->totalBlocksUsed - 1) > lastBlockLo )
         {
            compress4writeFreeBlock( writeCompress, f4, writeCompress->totalBlocksUsed-1 ) ;
            writeCompress->totalBlocksUsed-- ;
         }

         // and update the # of blocks to disk
         // and trim the actual listed file length
         writeCompress->fileLen = newLen ;
         FILE4LONG writePos ;
         // LY Jul 23/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
         // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + sizeof( unsigned long ), 0 ) ;
         file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)(&writeCompress->totalBlocksUsed) - (unsigned long)writeCompress, 0 ) ;
         file4writeLowDoInternal( f4, writePos, &writeCompress->totalBlocksUsed, sizeof( writeCompress->totalBlocksUsed ) + sizeof( writeCompress->fileLen )  ) ;

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

         return 0 ;
      }

      if ( lastBlockLo >= writeCompress->totalBlocks )
      {
         // need to extend and set up a new array block - also means internal allocation
         // set up new array size to be large enough for lastBlockNo (in case writing way to end of file) as well
         // as the additional blocks to add
         unsigned long blocksToAdd = (lastBlockLo - writeCompress->totalBlocks - 1) + writeCompress->arrayAllocCount ;  // -1 because we are allocating 1 for sure if going 1 forward
         // and re-allocate the internal write-header

         unsigned newLenLong = (blocksToAdd + writeCompress->totalBlocks) * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ;
         unsigned oldLenSave = (writeCompress->totalBlocks) * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ;
         unsigned oldLen = oldLenSave ;
         int rc = u4allocAgain( f4->codeBase, (char **)&(writeCompress->firstArrayBlock.writeArray), &oldLen, newLenLong ) ;
         if ( rc != 0 )
            return rc ;

         // and allocate a new array entry...find the last one...
         FILE4LONG writePos ;   // the location to update the 'next block' information for the last array block
         // AS Jan 19/06 - fix position to write...
         file4longAssign( writePos, f4->compressHeaderOffset + sizeof(COMPRESS4WRITE_HEADER) - sizeof( COMPRESS4WRITE_BLOCK ), 0 ) ;

         COMPRESS4WRITE_BLOCK *nextBlock = &(writeCompress->firstArrayBlock) ;
         for ( ;; )
         {
            if ( nextBlock->nextBlockPtr == 0 )  // found
               break ;
            writePos = nextBlock->nextBlock ;
            nextBlock = nextBlock->nextBlockPtr ;
         }

         nextBlock->nextBlockPtr = (COMPRESS4WRITE_BLOCK *)u4allocFree( f4->codeBase, sizeof( COMPRESS4WRITE_BLOCK ) ) ;
         if ( nextBlock->nextBlockPtr == 0 )
            return e4memory ;
         // and the disk pos...
         FILE4LONG fileLen = u4filelength( f4->hand ) ;
         nextBlock->nextBlock = fileLen ;
         // and update to disk
         unsigned urc = file4writeLowDoInternal( f4, writePos, nextBlock, sizeof( unsigned long ) + sizeof( FILE4LONG ) ) ;

         nextBlock = nextBlock->nextBlockPtr ;
         nextBlock->numEntries = blocksToAdd ;
         file4longAssign( nextBlock->nextBlock, 0, 0 ) ;
         nextBlock->nextBlockPtr = 0 ;
         nextBlock->writeArray = &(writeCompress->firstArrayBlock.writeArray[writeCompress->totalBlocks]) ;

         urc = file4writeLowDoInternal( f4, fileLen, nextBlock, sizeof( unsigned long ) + sizeof( FILE4LONG ) ) ;
         file4longAdd( &fileLen, 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ) ) ;
         urc = file4writeLowDoInternal( f4, fileLen, ((char *)(writeCompress->firstArrayBlock.writeArray))+oldLenSave, newLenLong-oldLenSave ) ;
         f4->physicalFileLen = u4filelength( f4->hand ) ;  // the actual file length (compressed)

         writeCompress->totalBlocks += blocksToAdd ;
         // and update to disk...
         FILE4LONG readPos ;
         // LY Sep 17/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
         // file4longAssign( readPos, f4->compressHeaderOffset + 3 * sizeof( short ), 0 ) ;
         file4longAssign( readPos, f4->compressHeaderOffset + (unsigned long)(&f4->compressInfo->writeCompress->totalBlocks) - (unsigned long)f4->compressInfo->writeCompress, 0 ) ;
         file4writeLowDoInternal( f4, readPos, &f4->compressInfo->writeCompress->totalBlocks,  sizeof( unsigned long ) ) ;
      }
      writeCompress->fileLen = newLen ;
      FILE4LONG writePos ;
      // LY Sep 17/04 : changed method of computing offset, due to possible miscalculations from data alignment and/or changes to data structure
      // file4longAssign( writePos, f4->compressHeaderOffset + 3 * sizeof( short ) + 2 * sizeof( unsigned long ), 0 ) ;
      file4longAssign( writePos, f4->compressHeaderOffset + (unsigned long)(&writeCompress->fileLen) - (unsigned long)writeCompress, 0 ) ;
      file4writeLowDoInternal( f4, writePos, &writeCompress->fileLen, sizeof( writeCompress->fileLen ) ) ;  // write the updated free chain entry to disk

      // just mark all of the relevant array entries
      unsigned long blockStart = f4->compressInfo->writeCompress->totalBlocksUsed + 1 ; // don't do the first block because it may be a border block
      while( blockStart <= lastBlockLo )
      {
         file4longAssign( writeCompress->firstArrayBlock.writeArray[blockStart].position, 0, 0 ) ;
         blockStart++ ;
      }

      return 0 ;
   }



   // AS Jun 28/04 - support for compressed writing
   static int file4compressSetLen( FILE4 *f4, FILE4LONG newLen )
   {
      critical4sectionVerify( &f4->critical4file ) ;

      // 2 cases - extending file or concatenating file
      FILE4LONG curLen = file4lenLow( f4 ) ;
      if ( file4longEqualLong( newLen, curLen ) )  // no change in length
         return 0 ;

      Bool5 requiresUnlock = 0 ;
      int rc = file4compressLock( f4, &requiresUnlock ) ;
      if ( rc != 0 )
         return rc ;

      rc = file4compressSetLenDo( f4, newLen ) ;

      if ( requiresUnlock )
         file4compressUnlock( f4 ) ;

      return rc ;
   }
#endif /* !S4CLIENT */


#ifdef S4FILE_EXTENDED
   int S4FUNCTION file4lenSet( FILE4 *f4, long newLen )
   {
      FILE4LONG lenSet ;

      /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
      file4longAssign( lenSet, newLen, 0L ) ;

      return file4lenSetLow( f4, lenSet ) ;
   }



   int file4lenSetLow( FILE4 *f4, FILE4LONG newLen )
#else
   int S4FUNCTION file4lenSetLow( FILE4 *f4, FILE4LONG newLen )
#endif
   {
      // AS Jun 28/04 - support for compressed writing - various changes in this function
      #ifdef E4PARM_HIGH
         if ( f4 == 0 || file4longError( newLen ) == (unsigned long)-1L )
            return error4( 0, e4parm_null, E90606 ) ;
      #endif

      CODE4 *c4 = f4->codeBase ;

      #ifdef E4ANALYZE
         if ( f4->hand == INVALID4HANDLE || c4 == 0 )
            return error4( c4, e4struct, E90606 ) ;
         if ( f4->isReadOnly )
            return error4( c4, e4struct, E80601 ) ;
      #else
         if ( c4 == 0 )
            return -1 ;
      #endif

      if ( error4code( c4 ) > 0 && error4code( c4 ) < 200 )  /* file error */
         return -1 ;

      if ( f4->isReadOnly )
         return error4( c4, e4parm, E80607 ) ;

      #ifdef S4FILE_EXTENDED
         if ( file4longGetHi( newLen ) )
            f4->isLong = 1 ;
         else
            f4->isLong = 0 ;
      #endif

      #ifndef S4OFF_OPTIMIZE
         // AS Aug 5/04 - We also need to ensure that we have the f4->critical section when calling this set len function so the length
         // isn't changed by another process (the virtual optimized length)
         #ifdef S4DELAY_WRITE_MT
            critical4sectionEnter( &f4->critical4file ) ;
         #endif

         if ( f4->doBuffer )
         {
            #ifdef S4TESTING
               Bool5 checkedLen = 0 ;
               FILE4LONG len1, len2 ;
            #endif
            /* if len is -1 but bufferWrites true, may still have buffered data
               which must be removed, so get disk file length for reference */
            if ( f4->bufferWrites == 1 && f4->fileCreated != 0 )
            {
               #ifdef S4TESTING
                  checkedLen = 1 ;
                  len1 = f4->len ;
               #endif
               f4->len = file4lenLow( f4 ) ;
               #ifdef S4TESTING
                  len2 = f4->len ;
               #endif
               // AS May 26/04 - support for optimizing large files
               // if ( file4longGetHi( f4->len ) != 0 )
               //    error4( 0, e4result, E90606 ) ;
            }
            // AS Nov 28/02 - there was a problem here, namely that we were extending
            // the internal file length, but the optimization buffer still had listed
            // its old length (which might be not on block boundary).  In that case, if
            // we later went to read that 'in memory' block we incorrectly only had
            // a partial read performed.  Therefore, we need to modify this existing block to
            // have the full length (whatever random data may be in memory is fine as the file
            // itself is just being extended without a write...
            // AS Jul 23/04 - need to use file4lenLow call here because we want to use the compressed stored length
            // in the case of compression, not the actual physical file length value listed...
            if ( file4longGreaterLong( f4->len, newLen ) )   /* must do a partial delete of memory */
               opt4fileDelete( f4, newLen, file4lenLow( f4 ) ) ;
            else
               opt4fileExtend( f4, newLen, file4lenLow( f4 ) ) ;

            if ( f4->bufferWrites )
               f4->len = newLen ;

         }

         #ifdef E4ANALYZE_ALL
            if ( f4->hasDup == 1 )
               if ( f4->doBuffer == 1 || f4->link.n == 0 && file4longGetHi( newLen ) == 0 )
                  if ( file4partLenSet( f4, file4longGetLo( newLen ) ) < 0 )
                     return error4( c4, e4opt, E80602 ) ;
         #endif

         // LY Jan 19/05 : added switches to avoid compiler error
         #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
            // AS Jul 28/04 - It looks like the compress set len must be called no matter what becuase we need to set out
            // that info...
            if ( f4->compressInfo != 0 )
            {
               int rc = file4compressSetLen( f4, newLen ) ;
               #ifdef S4DELAY_WRITE_MT // LY Apr 8/05 : avoid compiler error with S4OFF_THREAD
                  critical4sectionLeave( &f4->critical4file ) ;
               #endif
               return rc ;
            }
         #endif

         #ifdef S4DELAY_WRITE_MT
            critical4sectionLeave( &f4->critical4file ) ;
         #endif

         #ifdef S4SAFE
            if ( f4->fileCreated != 0 )   /* don't need to safeguard temporary files */
         #else
            /* E4ANALYZE must explicitly set file length for later verifications */
            #ifndef E4ANALYZE
               if ( f4->doBuffer == 0 || f4->bufferWrites == 0 )
            #endif
         #endif
      #endif
      {      /* needed !!! */
         #ifndef S4OFF_OPTIMIZE
            if ( f4->fileCreated == 1 )
         #endif
            {
               // LY Feb 9/05 : added ifdef's to avoid compiler errors
               #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
                  if ( f4->compressInfo != 0 )  // LY Sep 20/04 : need for S4OFF_OPTIMIZE
                     return file4compressSetLen( f4, newLen ) ;
                  else
               #endif
                  return file4lenSetLowDo( f4, newLen ) ;
            }
      }  /* needed ! ! ! */

      return 0 ;
   }



/* LY 2001/09/19 : used by d4dll.c */
int S4FUNCTION file4lenSetLow2( FILE4 S4PTR *f4, long len )
{
   return file4lenSet( f4, len ) ;
}



const char * S4FUNCTION file4nameLow( FILE4 S4PTR *f4 )
{
   return file4name( f4 ) ;
}



// AS Nov 26/02 - Support for data file compression
static unsigned file4readLowPhysical( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
{
   unsigned long urc ;
   #if defined( S4MACINTOSH )
      long rc ;
   #elif !defined( S4WIN32 )
      unsigned long rc ;
   #endif
   #if !defined( S4OFF_OPTIMIZE ) && defined( S4OPTIMIZE_STATS )
      DATA4 *stat ;
      CODE4 *c4 ;
   #endif
   short numTries ;  // CS 2011/05/19 Declare outside of for loop.

   critical4sectionVerify( &f4->critical4file ) ;

   #if defined( S4WIN32 )
      // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
      // Microsoft knowledgebase article #811693
      DWORD dwres ;
      #ifdef S4WINCE
         for ( numTries = 0 ;; numTries++ )
         {
      #endif
            dwres = SetFilePointer( (HANDLE)f4->hand, file4longGetLo( pos ), file4longGetHiAddress( pos ), FILE_BEGIN ) ;
      #ifdef S4WINCE
            DWORD err = GetLastError() ;
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

      file4longSetLo( pos, dwres ) ;
      file4longCheckError( pos ) ;
   #elif defined( S4PALM )
      rc = FileSeek( f4->hand, pos, fileOriginBeginning ) ;
      if (rc != 0)
         rc = (unsigned long)-1 ;
   #else
      #if defined( S4MACINTOSH )
         rc = MAClseek( f4->hand, pos, 0, 0 ) ;
      #elif defined( S4WINDOWS )
         rc = _llseek( f4->hand, pos, 0 ) ;
      #else
         #ifdef S4LSEEK
            rc = f4lseek( f4, pos, 0, 0 ) ;
         #else
            rc = lseek( f4->hand, pos, 0 ) ;
         #endif
      #endif
      if ( rc != pos )
         pos = (unsigned long)-1 ;  /* LY 2002/06/18 : changed rc to pos for below */
   #endif

   if ( file4longError( pos ) == (unsigned long)-1L )
   {
      file4readError( f4, pos, len, "file4readLow" ) ;
      return 0 ;
   }

   #if defined( S4WIN32 )
      // AS Aug 4/03 - Help debugging when reading
      // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
      // Microsoft knowledgebase article #811693
      #ifdef S4WINCE
         for ( numTries = 0 ;; numTries++ )
         {
      #endif
            // MessageBox( 0, L"toread", L"DEBUG", MB_OK | MB_ICONSTOP ) ;
            BOOL blRes = ReadFile( (HANDLE)f4->hand, ptr, len, &urc, 0 ) ;
            // MessageBox( 0, L"readdone", L"DEBUG", MB_OK | MB_ICONSTOP ) ;
      #ifdef S4WINCE
            if ( blRes == 0 )
            {
               // MessageBox( 0, L"error", L"DEBUG", MB_OK | MB_ICONSTOP ) ;
               DWORD err = GetLastError() ;
               if ( err != ERROR_ACCESS_DENIED || numTries > 4 )  // only retry if an access denied error
                  break ;
               Sleep( 500 ) ;
            }
            else
               break ;
         }
      #endif
      #ifdef E4ANALYZE
         if ( blRes == 0 )
         {
            DWORD err = GetLastError() ;
            if ( err == ERROR_HANDLE_EOF )
            {
               if ( urc > len )
               {
                  file4readError( f4, pos, len, "file4readLow" ) ;
                  return 0 ;
               }
            }
         }
      #endif
   #elif defined( S4MACINTOSH )
      rc = (long) len ;
      urc = FSRead( f4->hand, &rc, ptr ) ;
      if ( urc == 0 )
         urc = len ;
      if ( urc == eofErr )  /* attempt to read past EOF OK */
         urc = rc ;
   #elif defined( S4WINDOWS )
      urc = (unsigned)_lread( f4->hand, (char *)ptr, len ) ;
   #elif defined( S4PALM )
      urc = (unsigned)FileRead( f4->hand, (char *)ptr, len, 1, 0 ) ;
   #elif defined( S4LSEEK )
      /* if reading past EOF */
      if ( pos+len > u4filelength( f4->hand ) )
         urc = (unsigned)read( f4->hand, ptr, u4filelength(f4->hand) - pos ) ;
      else
         urc = (unsigned)read( f4->hand, ptr, len ) ;
   #else
      urc = (unsigned)read( f4->hand, (char *)ptr, len ) ;
   #endif

   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      if ( f4->preprocessed && urc > 0 )
         code4fileDecrypt( f4, file4longGetLo( pos ), ptr, urc, ptr ) ;
         // file4postProcess( &f4->codeBase->preprocess, preprocess4getInit( &(f4->codeBase->preprocess) ), f4, file4longGetLo( pos ), ptr, urc, ptr )
   #endif

   if ( urc > len )
   {
      file4readError( f4, pos, len, "file4readLow" ) ;
      return 0 ;
   }

   #ifndef S4OFF_OPTIMIZE
      #ifdef E4ANALYZE_ALL
         if ( f4->hasDup == 1 )
            if ( f4->doBuffer == 1 || f4->link.n == 0 )
               if ( file4cmpPart( f4->codeBase, ptr, f4, file4longGetLo( pos ), urc ) != 0 )
               {
                  error4( f4->codeBase, e4opt, E80602 ) ;
                  return 0 ;
               }
      #endif
   #endif

   return urc ;
}



unsigned file4readOnBoundary( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
{
   // considers reading on boundaries if required for post processing
   unsigned long urc = 0 ;
   #if !defined( S4PREPROCESS_FILE ) || defined( S4CLIENT )
      urc = file4readLowPhysical( f4, pos, ptr, len ) ;
   #else
      CODE4 *c4 = f4->codeBase ;
      short blockSize = 0 ;
      if ( f4->preprocessed != 0 )
         blockSize = code4getPreprocessBlockSize( c4 ) ;
      if ( blockSize <= 1 )
      {
         urc = file4readLowPhysical( f4, pos, ptr, len ) ;
      }
      else
      {
         // AS May 24/02 need to ensure that start position and length run on proper offsets.
         short posMod = (short)file4longMod( pos, blockSize ) ;
         FILE4LONG readPos ;
         unsigned readLen = len ;
         short lenMod = 0 ;  // total modifications to length read
         file4longAssignLong( readPos, pos ) ;
         if ( posMod != 0 )
         {
            // need to adjust the start to read position.
            assert5( file4longGetLo( readPos ) >= (unsigned short)posMod ) ;
            file4longSubtract( &readPos, posMod ) ;
            readLen += posMod ;
            lenMod += posMod ;
         }

         short curLenMod = readLen % blockSize ;  // mod of the length that has been adjusted by posMod if applicable
         if ( curLenMod != 0 )
         {
            curLenMod = blockSize - curLenMod ;   // offset length to reach end of block
            // need to adjust the read length
            lenMod += curLenMod ;
            readLen += curLenMod ;
         }

         if ( lenMod == 0 )
         {
            urc = file4readLowPhysical( f4, readPos, ptr, readLen ) ;
         }
         else
         {
            // need to use read buffer to handle increased size
            if ( c4->filePreprocessBufferLen < readLen )  // realloc
            {
               if ( c4->filePreprocessBuffer != 0 )
               {
                  u4free( c4->filePreprocessBuffer ) ;
                  c4->filePreprocessBuffer = 0 ;
                  c4->filePreprocessBufferLen = 0 ;
               }

               assert5( readLen > 0 ) ;
               c4->filePreprocessBuffer = (char *)u4allocFree( c4, readLen ) ;
               if ( c4->filePreprocessBuffer == 0 )
                  return 0 ;
               c4->filePreprocessBufferLen = readLen ;
            }
            assert5( readLen - lenMod == len ) ;  // initial length amount should match
            urc = file4readLowPhysical( f4, readPos, c4->filePreprocessBuffer, readLen ) ;
            /* LY May 3/04 : replaced min() (non-standard function) */
            urc = ( urc < (readLen - lenMod ) ? urc : (readLen - lenMod ) ) ;  // adjust output length to not include extra read for decryption only
            memcpy( ptr, (char *)c4->filePreprocessBuffer + posMod, urc ) ;
         }
      }
   #endif
   return urc ;
}


// AS Oct 7/03 - improve performance without compression
// AS May 17/04 - client/server functionality to copmress the data file...
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   unsigned file4readDecompressDo( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
   {
      critical4sectionVerify( &f4->critical4file ) ;

      if ( file4longLess( pos, f4->compressHeaderOffset ) )  // requesting to read in uncompressed header area
      {
         assert5( file4longGetHi( pos ) == 0 ) ;  // the header part had better be in the non-long file area

         unsigned long readPos = file4longGetLo( pos ) ;

         if ( readPos + len <= f4->compressHeaderOffset )  // all of the read is within the header area
            return file4readOnBoundary( f4, pos, ptr, len ) ;

         // in this case we want to read partially in header and partially in data.  Solve this by
         // reading from the header first (physical read), then recall this function to read the rest
         unsigned physicalLen = f4->compressHeaderOffset - readPos ;
         unsigned physicalRead = file4readOnBoundary( f4, pos, ptr, physicalLen ) ;
         if ( physicalRead != physicalLen )  // could only do a partial read, we are done
            return physicalRead ;
         // and now read the rest...
         unsigned leftToRead = len - physicalRead ;
         file4longAdd( &pos, physicalRead ) ;
         assert5( file4longGetLo( pos ) == f4->compressHeaderOffset ) ;
         // AS Jan 2/02 - read Decompress, not on boundary, else doesn't get uncompressed...
         unsigned compressedRead = file4readDecompressDo( f4, pos, (char *)ptr+physicalRead, leftToRead ) ;
         return physicalRead + compressedRead ;
      }

      // need to calculate read and perform decompressions...
      long lenLeft = len ;
      FILE4LONG blockOnLong = pos ;
      file4longSubtract( &blockOnLong, f4->compressHeaderOffset ) ;
      file4longDivide( blockOnLong, f4->actualBlockSize ) ;
      assert5( file4longGetHi( blockOnLong ) == 0 ) ;  // the offset must be sizeof long or smaller
      unsigned long blockOn = file4longGetLo( blockOnLong ) ;
      // long blockOn = ( readPos - f4->compressHeaderOffset ) / f4->actualBlockSize ;  // where do we really start reading
      FILE4LONG blockOffsetLong = pos ;
      file4longSubtract( &blockOffsetLong, f4->compressHeaderOffset ) ;
      // AS Mar 23/04 compile warning fix
      unsigned long blockOffset = (unsigned long)(file4longMod( blockOffsetLong, f4->actualBlockSize )) ;
      // unsigned long blockOffset = ( readPos - f4->compressHeaderOffset ) % f4->actualBlockSize ;

      int rc = 0 ;

      long ptrOffset = 0 ;
      Bool5 doneRead = 0 ;  // AS Jan 16/03 - when done reading last block
      // AS Jul 31/03 - Support for large file support
      FILE4LONG maxLen ;
      file4longAssign( maxLen, 0, 0 ) ;

      while ( lenLeft > 0 )
      {
         unsigned long outLen  ;

         // check the existing buffer first, we may be able to use it...
         // AS Aug 1/03 - Large file support
         if ( blockOn + 1 > (unsigned long)compress4handlerNumOffsetsGet( f4->compressInfo ) )
         {
            outLen = f4->uBufFileLen ;
            file4longAssign( maxLen, outLen, 0 ) ;
            // AS Jul 25/03 - If this is the last block, we are done reading (t4cmprss.c), was not setting in some cases
            if ( blockOn + 1 >= (unsigned long)compress4handlerNumOffsetsGet( f4->compressInfo ) )
               doneRead = 1 ;
         }
         else
         {
            FILE4LONG compressOffset ;
            compressOffset = compress4handlerOffsetGet( f4, blockOn ) ;

            // if write-compress is enabled and the file is opened fully shared, we need to always re-read from disk (the user
            // can enable read/write optimization with shared files to avoid this)
            Bool5 useCompressBuffer = 0 ;   // can we use the internal stored buffer?
            if ( file4longEqualLong( f4->uBufFilePos, compressOffset ) )
            {
               if ( f4->compressInfo->writeCompress == 0 )
                  useCompressBuffer = 1 ;
               #ifndef S4OFF_MULTI  // LY Feb 3/05
                  else if ( f4->lowAccessMode != OPEN4DENY_NONE )   // if no-one else can write to the file, we can use the internal buffer
               #endif
                  useCompressBuffer = 1 ;
            }

            if ( useCompressBuffer )
            {
               outLen = f4->uBufFileLen ;
               file4longAssign( maxLen, outLen, 0 ) ;
               // AS Jul 25/03 - If this is the last block, we are done reading (t4cmprss.c), was not setting in some cases
               if ( blockOn + 1 >= (unsigned long)compress4handlerNumOffsetsGet( f4->compressInfo ) )
                  doneRead = 1 ;
            }
            else
            {
               FILE4LONG toRead = compressOffset ;
               // file4longAssign( toRead, compressOffsetf4->compressInfo->offsets[blockOn], 0 ) ;

               FILE4LONG lenToReadLong ;
               unsigned long numOffsets = (unsigned long)compress4handlerNumOffsetsGet( f4->compressInfo ) ;
               // if last block, special handling - we don't have the next block to calculate the file position, so use the physical file length instead
               // AS Jul 5/04 - if we are using write compression, the length is stored in the array information...
               if ( f4->compressInfo->writeCompress != 0 )
               {
                  file4longAssign( lenToReadLong, f4->compressInfo->writeCompress->firstArrayBlock.writeArray[blockOn].compressedLength, 0 ) ;
               }
               else
               {
                  if ( blockOn + 1 >= numOffsets )
                  {
                     lenToReadLong = f4->physicalFileLen ;
                     file4longSubtractLong( &lenToReadLong, &compressOffset ) ;
                     // AS Jul 25/03 - If this is the last block, we are done reading (t4cmprss.c), was not setting in some cases
                     doneRead = 1 ;
                  }
                  else
                  {
                     // AS Jun 30/04 - if >, means we are reading past eof probably...
                     if ( blockOn + 1 > numOffsets )
                     {
                        return 0 ;  // past eof
                     }
                     else
                     {
                        lenToReadLong = compress4handlerOffsetGet( f4, blockOn+1 ) ;
                        file4longSubtractLong( &lenToReadLong, &compressOffset ) ;
                     }
                  }
               }

               assert5( file4longGetHi( lenToReadLong ) == 0 ) ;
               long lenToRead = file4longGetLo( lenToReadLong ) ;

               if ( lenToRead == 0 )  // nothing to read - done
                  return ptrOffset ;  // AS Jan 16/03 - return amount read which might not be zero

               assert5( lenToRead > 0 ) ;
               assert5( lenToRead <= (long)(f4->actualBlockSize * 1.01 ) + 12 ) ;

               // AS Mar 25/03 - assign these before accidental use
               outLen = f4->actualBlockSize ;
               file4longAssign( maxLen, outLen, 0 ) ;
               // read the next block
               rc = file4readOnBoundary( f4, toRead, f4->compressBuffer, lenToRead ) ;
               if ( rc != lenToRead )
                  break ;

               // verify length
               critical4sectionVerify( &f4->critical4file ) ;
               rc = c4uncompress( f4->codeBase, f4->uncompressBuffer, &outLen, f4->compressBuffer, lenToRead ) ;  // offset uncompress by 4 due to length value
               switch ( rc )
               {
                  case 0:
                     break ;
                  case Z_MEM_ERROR:
                     error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress: insufficient memory", 0 ) ;
                     break ;
                  case Z_BUF_ERROR:
                     error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress: insufficient output buffer size", 0 ) ;
                     break ;
                  case Z_DATA_ERROR:
                     error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress: corrupt compressed data in file", 0 ) ;
                     break ;
                  default:
                     error4describe( f4->codeBase, e4read, E90621, f4->name, "failed to uncompress compressed data file", 0 ) ;
                     break ;
               }

               if ( rc != 0 )
                  break ;

               assert5( outLen == f4->actualBlockSize ) ;

               // AS Jan 16/03 - Reduce outLen if we exceed the length of the file.  This is
               // because we compress extra bytes at the end of the file to fill out the block size
               // AS Mar 24/03 - Failing to take into account the compressHeaderOffset which adjusts the file position.
               long amountAvailInBlock = outLen - blockOffset ;  // the bytes available to us from this block (take blockOffset into account)
               // the maximum we could conceviably read is limited by the file length...
               // AS Jul 31/03 - Support for large file support
               maxLen = compress4handlerFileLen( f4->compressInfo ) ;
               file4longAdd( &maxLen, f4->compressHeaderOffset ) ;  // the point past which we are not allowed to read
               // now subtract the readPos from this to determine the amount left to physicall read
               file4longSubtractLong( &maxLen, &pos ) ;

               // now, if the maxLen to read exceeds the amountAvailInBlock, we need to reduce the amount we can actaully read to.
               // if ( amountAvailInBlock > maxLen )
               if ( file4longLess( maxLen, (unsigned long)amountAvailInBlock ) )
               {
                  // not necessarily the case...due to bufferring the file bufferred size may differ from the compressed size
                  // #ifdef S4TESTING
                     // force a gpf so we can debug into this...
                  //    if( blockOn != (unsigned long)compress4handlerNumOffsetsGet( f4->compressInfo ) - 1 )  // ensure we are on the last block in this case
                  //       memcpy( 0, "junk", 4 ) ;
                  // #endif
                  // assert5( blockOn == (unsigned long)compress4handlerNumOffsetsGet( f4->compressInfo ) - 1 ) ;  // ensure we are on the last block in this case
                  // long svLen = outLen ;
                  assert5( file4longGetHi( maxLen ) == 0 ) ;
                  outLen = file4longGetLo( maxLen ) + blockOffset ;
                  // maxLen = svLen ;
                  doneRead = 1 ;
               }
               //else

               file4longAssign( maxLen, outLen, 0 ) ;  // set maxLen to max we can read in current buffer block (== outLen, the size of the buffer as indicated below)

               // store the current position and length of buffer to allow for re-use without uncompression
               // AS Aug 1/03 - Large file support
               // f4->uBufFilePos = f4->compressInfo->offsets[blockOn] ;
               f4->uBufFilePos = compress4handlerOffsetGet( f4, blockOn ) ;
               f4->uBufFileLen = outLen ;
            }
         }

         // now copy the data into the read buffer - maxLen always == f4->actualBlockSize except in the case where we are performing a final block read, in which case maxLen == amount available
         // in the block.  In that case, there is no way we can possibly have a start offset > actual size of the available space in the buffer, unless we are requesting to read
         // past the eof, in which case we can read nothing...
         unsigned long lenToCopy ;
         // AS Jul 31/03 - Support for large file support
         // if ( maxLen < blockOffset )
         if ( file4longLess( maxLen, blockOffset ) )
            lenToCopy = 0 ;
         else
         {
            // AS Mar 24/03 - Code form Jan 16/03 above, we already have rerduced outLen as appopriate, so use maxLen, not given outLen
            // unsigned long lenToCopy = min( (unsigned long)lenLeft, outLen - blockOffset ) ;
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
         lenToCopy = min( lenToCopy, outLen ) ;
         assert5( blockOffset+lenToCopy <= f4->actualBlockSize ) ;
         memcpy( (char *)ptr + ptrOffset, f4->uncompressBuffer + blockOffset, lenToCopy ) ;
         ptrOffset += lenToCopy ;
         blockOffset = 0 ;   // future reads will start from the start of the buffer
         lenLeft -= lenToCopy ;
         blockOn++ ;
         if ( doneRead == 1 )
            return ptrOffset ;
      }

      if ( rc < 0 )
         return 0 ;

      return len ;
   }

   static unsigned file4readDecompress( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
   {
      // considers compression/decompression on file if required
      // need to consider compression
      if ( f4->compressInfo == 0 ) // file is not compressed
         return file4readOnBoundary( f4, pos, ptr, len ) ;

      Bool5 requiresUnlock = 0 ;
      if ( file4compressLock( f4, &requiresUnlock ) != 0 )
         return 0 ;

      unsigned outRc = file4readDecompressDo( f4, pos, ptr, len ) ;

      if ( requiresUnlock )   // others can access file, so must lock
         file4compressUnlock( f4 ) ;

      return outRc ;
   }
#else
   #define file4readDecompress( f4, pos, ptr, len ) ( file4readOnBoundary( (f4), (pos), (ptr), (len) ) )
#endif


static unsigned file4readLowDo( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
{
   // ensure all reading is done within a critical section
   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      critical4sectionEnter( &f4->critical4file ) ;
   #endif

   #if !defined( S4OFF_OPTIMIZE ) && defined( S4OPTIMIZE_STATS )
      c4 = f4->codeBase ;
      stat = c4->statusDbf ;
      if ( stat != 0 )  /* track stats */
      {
         if ( f4 != &stat->dataFile->file )  /* don't do for the stat file! */
         {
            if ( d4appendStart( stat, 0 ) == 0 )
            {
               f4assignChar( c4->typeFld, 'L' ) ;  /* low-level */
               f4assign( c4->fileNameFld, f4->name ) ;
               f4assignLong( c4->offsetFld, pos ) ;
               f4assignLong( c4->lengthFld, len ) ;
               d4append( stat ) ;
            }
         }
      }
   #endif

   unsigned urc = file4readDecompress( f4, pos, ptr, len ) ;

   #ifdef S4DELAY_WRITE_MT
      critical4sectionLeave( &f4->critical4file ) ;
   #endif

   return urc ;
}



unsigned file4readLow( FILE4 *f4, FILE4LONG pos, void *ptr, const unsigned lenToRead )
{
   /* this function also includes advance-read/delay-write checking */
   unsigned urc ;

   #ifdef S4ADVANCE_READ
      /* check the special advance-read-buffer for the file first, if it fits
         in entirely, then copy from there instead of performing read */
      // AS Apr 13/04 - support for optimizing large files
      // #ifdef S4FILE_EXTENDED
      //    if ( f4->isLong == 0 )  /* advance reading not done on long files */
      // #endif
      if ( f4->advanceReadBufStatus != AR4EMPTY )  /* if it is set put still in area, then wait for the advance-read to finish */
      {
         // AS 02/01/00 -- some info gets set when advance status is AR4SET, so wait until it is done before going on...
         while( f4->advanceReadBufStatus == AR4SET )
         {
            // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
            // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
            u4sleep( f4->codeBase ) ;
         }

         if ( f4->advanceReadBufStatus == AR4FULL ) /* successful read */
         {
            // AS Apr 13/04 - support for optimizing large files
            // unsigned posShort ;
            // int noAdvance = 0 ;

            // if ( file4longGetHi( pos ) != 0 )  /* we only advance read for files < 4 gigs */
            //    noAdvance = 1 ;

            // posShort = file4longGetLo( pos ) ;

            // if ( posShort > ULONG_MAX - lenToRead )  /* means we will exceed - i.e. large file */
            //    noAdvance = 1 ;

            // if ( noAdvance == 0 )
            // {
               // if ( ( posShort >= f4->advanceReadBufPos ) && ( file4longGetLo( pos ) + lenToRead ) <= ( f4->advanceReadBufPos + f4->advanceReadBufLen ) )
               FILE4LONG advanceEnd ;
               file4longAssignLong( advanceEnd, f4->advanceReadBufPos ) ;
               file4longAdd( &advanceEnd, f4->advanceReadBufLen ) ;
               FILE4LONG posEnd ;
               file4longAssignLong( posEnd, pos ) ;
               file4longAdd( &posEnd, lenToRead ) ;
               if ( file4longGreaterEqLong( pos, f4->advanceReadBufPos ) && file4longLessEqLong( posEnd, advanceEnd ) )
               {
                  // memcpy( (char *)ptr, f4->advanceReadBuf + posShort - f4->advanceReadBufPos, lenToRead ) ;
                  FILE4LONG copyPos ;
                  file4longAssignLong( copyPos, pos ) ;
                  file4longSubtractLong( &copyPos, &f4->advanceReadBufPos ) ;
                  assert5( file4longGetHi( copyPos ) == 0 ) ;
                  memcpy( (char *)ptr, f4->advanceReadBuf + file4longGetLo( copyPos ), lenToRead ) ;
                  return lenToRead ;
               }
            // }
         }
      }
   #endif

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      critical4sectionEnter( &f4->critical4file ) ;
   #endif

   #ifdef S4WRITE_DELAY
      /* make sure that the data to read isn't in memory */
      // AS Apr 13/04 - support for optimizing large files
      // int noDelay = 0 ;
      // if ( file4longGetHi( pos ) != 0 )  /* we only advance read for files < 4 gigs */
      //    noDelay = 1 ;
      // unsigned posShort = file4longGetLo( pos ) ;
      // if ( posShort > ULONG_MAX - lenToRead )  /* means we will exceed - i.e. large file */
      //    noDelay = 1 ;

      // if ( noDelay == 0 )
      // {
         urc = 0 ;
         if ( l4numNodes( &f4->delayWriteFileList ) != 0 )  /* check for pieces already in memory */
         {
            // AS 05/06/99 --> critical sectioning missing...
            CODE4 *c4 = f4->codeBase ;
            critical4sectionEnter( &c4->critical4delayWriteList ) ;
            for ( LINK4 *delayLink = 0 ;; )
            {
               delayLink = (LINK4 *)l4next( &f4->delayWriteFileList, delayLink ) ;
               if ( delayLink == 0 )
                  break ;
               FILE4WRITE_DELAY *writeDelay = (FILE4WRITE_DELAY *)(delayLink - 1 ) ;
               /* now, if the delay piece belongs in the buffer, then read al            the info before the delay piece, copy the delay piece over,
                  and read all the info after the delay piece */
               /* LY 4/28/99 : replaced binary operators with file4long*** */
               assert5( writeDelay->file != 0 ) ;  // assert that this is still valid...
               FILE4LONG tLong ;
               file4longAssignLong( tLong, writeDelay->pos ) ;
               file4longAdd( &tLong, writeDelay->len ) ;
               if ( file4longLessEqLong( tLong, pos ) )  /* outside of block */
                  continue ;

               file4longAssignLong( tLong, pos ) ;
               file4longAdd( &tLong, lenToRead ) ;
               if ( file4longLessEqLong( tLong, writeDelay->pos ) )  /* outside of block */
                  continue ;

               file4longAssignLong( tLong, writeDelay->pos ) ;
               file4longSubtractLong( &tLong, &pos ) ;
               long beforeLen = file4longGetLo( tLong ) ;

               if ( beforeLen < 0 )
                  beforeLen = 0 ;

               unsigned copyLen ;
               unsigned copyPos ;
               if ( beforeLen == 0 )
               {
                  /* LY 4/28/99 : replaced binary operators with file4long*** */
                  file4longAssignLong( tLong, pos ) ;
                  file4longSubtractLong( &tLong, &writeDelay->pos ) ;
                  assert5( file4longGetHi( tLong ) == 0 ) ;
                  copyPos = file4longGetLo( tLong ) ;
                  copyLen = lenToRead ;
               }
               else
               {
                  copyPos = 0 ;
                  copyLen = lenToRead - beforeLen ;
               }

               // AS 05/06/99 --> was failing if copyPos > writeDelay->len, in which case the negative
               // number was perceived as a very large positive number
               assert5( copyPos <= writeDelay->len ) ;
               if ( copyLen > ( writeDelay->len - copyPos ) )
                  copyLen = writeDelay->len - copyPos ;
               FILE4LONG afterPos ;
               file4longAssignLong( afterPos, pos ) ;
               file4longAdd( &afterPos, beforeLen + copyLen ) ;

               /* LY 4/28/99 : replaced binary operators with file4long*** */

               // AS 02/01/00 was problem here in calculating lengths.
               // we want to discover how many 'extra bytes' we need to
               // read that lie outside the range of the delay-write buffer
               // (i.e. we are copying some (or all) of the data from the
               // delay write buffer, but any extra needs to be read via
               // another read request).
               // this extra amount to read will be... the 'end of read position' - 'end of delay write buffer position'
               // 'end of read position' = posShort + lenToRead

               // AS 02/01/00 -> looks like a negative afterLen is ok, since we check later that > 0 before
               // reading.  Was failing here, for example, where: posShort = 11264, lenToRead = 1024, copyLen = 1024,
               // writeDelay->pos = 1024, writeDelay->len = 12288 (was over-reading after)
               // AS 05/06/99 -> should be copyLen, not writeDelay->len, since copyLen might be <, resulting in negative number
               // file4longAssign( tLong, posShort + len - writeLen, 0 ) ;
               // file4longAssign( tLong, posShort + lenToRead - copyLen, 0 ) ;
               file4longAssignLong( tLong, pos ) ;
               file4longAdd( &tLong, lenToRead - writeDelay->len ) ;
               file4longSubtractLong( &tLong, &(writeDelay->pos) ) ;
               long afterLen = file4longGetLo( tLong ) ;
               if ( beforeLen != 0 )
                  urc = file4readLow( f4, pos, ptr, beforeLen ) ;
               if ( urc == (unsigned int)beforeLen )
               {
                  assert5( copyLen <= (writeDelay->len - copyPos) ) ; // don't extract from out of memory range...
                  memcpy( (char *)ptr + beforeLen, writeDelay->data + copyPos, copyLen ) ;
                  urc += copyLen ;
                  if ( afterLen > 0 )  /* is negative if read ends within block */
                     urc += file4readLow( f4, afterPos, (char *)ptr + beforeLen + copyLen, afterLen ) ;
               }
               // AS 05/06/99 --> critical sectioning missing...
               assert5( writeDelay->file != 0 ) ;  // assert that this is still valid...
               critical4sectionLeave( &c4->critical4delayWriteList ) ;
               critical4sectionLeave( &f4->critical4file ) ;
               assert5( urc <= lenToRead ) ;   // AS 02/01/00 -- cannot have read more than was requested!
               return urc ;
            }
            // AS 05/06/99 --> critical sectioning missing...
            critical4sectionLeave( &c4->critical4delayWriteList ) ;
         }
      // }
   #endif

   urc = file4readLowDo( f4, pos, ptr, lenToRead ) ;

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      critical4sectionLeave( &f4->critical4file ) ;
   #endif

   #ifdef S4ADVANCE_READ
      /*
         We need to go through the advance read buffers and purge them of
         any duplicate reads of what we read.  Basically, if we are advance
         reading, there is a chance that the advance reads never advance-read
         in time, so if there are advance read buffers there is a good
         chance that we just read what they meant to read.  To ensure that
         advance-reading does not fall further behind, we need to cancel
         their reading now for those items which we just over-read.  This
         results in optimum efficiency for the advance-reads.
      */
      // AS Apr 13/04 - support for optimizing large files
      // #ifdef S4FILE_EXTENDED
      //    if ( f4->isLong == 0 )
      // #endif
         if ( f4->hasHadPendingAdvanceRead )  /* for efficiency, avoid for non-advance-read files */
            file4advanceReadWriteOver( f4, pos, lenToRead, ptr, 0 ) ;
   #endif

   assert5( urc <= lenToRead ) ;   // AS 02/01/00 -- cannot have read more than was requested!
   return urc ;
}



unsigned S4FUNCTION file4read( FILE4 *f4, const long posIn, void *ptr, const unsigned lenIn )
{
   FILE4LONG pos ;

   #ifdef E4PARM_HIGH
      if ( f4 == 0 || posIn < 0 || ptr == 0  )
      {
         error4( 0, e4parm_null, E90607 ) ;
         return 0 ;
      }
   #endif
   /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
   file4longAssign( pos, posIn, 0L ) ;

   return file4readInternal( f4, pos, ptr, lenIn ) ;
}



unsigned file4readInternal( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
{
   #ifndef S4OFF_OPTIMIZE
      unsigned urc ;
      #ifdef S4OPTIMIZE_STATS
         DATA4 *stat ;
         CODE4 *c4 ;
      #endif
   #endif

   #ifdef E4ANALYZE
      // AS Sep 3/03 - may be invalid if temporary
      #ifdef S4CLIENT
         if ( f4->hand == INVALID4HANDLE )
      #else
         if ( f4->hand == INVALID4HANDLE && ( f4->isTemporary == 0
         // AS Oct 18/06 - only in optimization case...
         #if !defined( S4OFF_MULTI ) && !defined( S4OFF_OPTIMIZE )  // LY Jul 12/04
            || f4->fileCreated == 1
         #endif
            ) )
      #endif
      {
         error4( f4->codeBase, e4parm, E90607 ) ;
         return 0 ;
      }
   #endif

   if ( error4code( f4->codeBase ) < 0 )
      return 0 ;

   if ( len == 0 )
      return 0 ;

   #ifndef S4OFF_OPTIMIZE
      if ( f4->doBuffer )
      {
         /* LY 99/10/12 : prevent reading past 2GB in optimization buffer */
         // AS Apr 14/04 - support for optimizing large files
         // if ( ( file4longGetHi( pos ) != 0 ) ||
         //    ( file4longGetLo( pos )+len > LONG_MAX ) )  /* means out of range, nothing read */
         //    return file4readLow( f4, pos, ptr, len ) ;
         #ifdef S4OPTIMIZE_STATS
            c4 = f4->codeBase ;
            stat = c4->statusDbf ;
            if ( stat != 0 )  /* track stats */
            {
               if ( f4 != &stat->dataFile->file )  /* don't do for the stat file! */
               {
                  if ( d4appendStart( stat, 0 ) == 0 )
                  {
                     f4assignChar( c4->typeFld, 'H' ) ;  /* high-level */
                     f4assign( c4->fileNameFld, f4->name ) ;
                     f4assignLong( c4->offsetFld, file4longGetLo( pos ) ) ;
                     f4assignLong( c4->lengthFld, file4longGetLo( len ) ) ;
                     d4append( stat ) ;
                  }
               }
            }
         #endif

         urc = (unsigned)opt4fileRead( f4, pos, ptr, len )  ;
         if ( urc > len )
         {
            file4readError( f4, pos, len, "file4read" ) ;
            return 0 ;
         }
         return urc ;
      }
      else
      {
         if ( f4->fileCreated == 0 )   /* cannot read from non-existant file */
            return 0 ;
   #endif
      return file4readLow( f4, pos, ptr, len ) ;

   #ifndef S4OFF_OPTIMIZE
      }
   #endif
}



#ifndef S4INTERNAL_COMPILE_CHECK
   int S4FUNCTION file4readAll( FILE4 *f4, const long posIn, void *ptr, const unsigned lenIn )
   {
      FILE4LONG pos ;

      #ifdef E4PARM_HIGH
         if ( f4 == 0 || posIn < 0 || ptr == 0  )
            return error4( 0, e4parm_null, E90608 ) ;
      #endif

      /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
      file4longAssign( pos, posIn, 0L ) ;

      return file4readAllInternal( f4, pos, ptr, lenIn ) ;
   }
#endif



int file4readAllInternal( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
{
   unsigned urc ;

   #ifdef E4ANALYZE
      if ( f4->hand == INVALID4HANDLE )
         return error4( f4->codeBase, e4parm, E90608 ) ;
   #endif

   if ( error4code( f4->codeBase ) < 0 )
      return -1 ;

   if ( len == 0 )
      return 0 ;

   #ifndef S4OFF_OPTIMIZE
      if ( f4->doBuffer )
      {
         // AS May 26/04 - support for optimizing large files
         // if ( file4longGetHi( pos ) != 0 )  /* means out of range, nothing read */
         //    return file4readError( f4, pos, len, "file4readAll" ) ;
         urc = opt4fileRead( f4, pos, ptr, len )  ;
         if ( urc != len )
            return file4readError( f4, pos, len, "file4readAll" ) ;
         return 0 ;
      }
      else
      {
         if ( f4->fileCreated == 0 )   /* cannot read from non-existant file */
            return error4( f4->codeBase, e4opt, E90607 ) ;
   #endif
      urc = file4readLow( f4, pos, ptr, len ) ;
      if ( urc != len )
         return file4readError( f4, pos, len, "file4readAllLow" ) ;
   #ifndef S4OFF_OPTIMIZE
      }
   #endif

   return 0 ;
}



int S4FUNCTION file4readError( FILE4 *f4, FILE4LONG pos, unsigned len, const char *location )
{
   char posBuf[40] ;

   c4memset( posBuf, 0, sizeof( posBuf ) ) ;
   c4memset( posBuf, ' ', sizeof( posBuf ) - 1 ) ;

   c4ltoa45( file4longGetLo( pos ), posBuf, 19 ) ;
   c4ltoa45( len, posBuf + 20, 19 ) ;

   return error4describe( f4->codeBase, e4read, E90621, f4->name, posBuf, location ) ;
}



int S4FUNCTION file4replace( FILE4 *keep, FILE4 *from )
{
   FILE4 tmp ;
   int rc ;
   char fromName[LEN4PATH] ;
   CODE4 *c4 ;
   #ifndef S4SINGLE
      char *buf ;
      unsigned bufSize ;
      #ifdef S4LOW_MEMORY
         #ifndef S4OFF_OPTIMIZE
            int hasOpt ;
         #endif
      #endif
      FILE4LONG pos ;
      FILE4LONG fLen ;
   #endif

   #ifdef E4PARM_LOW
      if ( keep == 0 || from == 0  )
         return error4( 0, e4parm_null, E90609 ) ;
   #endif

   rc = 0 ;
   c4 = from->codeBase ;
   #ifdef E4ANALYZE
      if ( from->isReadOnly || keep->isReadOnly )
         return error4( c4, e4parm, E90601 ) ;
   #endif

   #ifndef S4SINGLE
      if ( keep->lowAccessMode == OPEN4DENY_RW )
      {
   #endif
      c4memcpy( (void *)&tmp, (void *)keep, sizeof ( FILE4 ) ) ;  /* remember settings */

   /* 05/09/96 AS first, must flush the files to disk to avoid delay-writes
      which are based on the FILE4 pointer which is maintained, instead of
      the physical handle, which is not --> or at least unoptimize it */

      file4optimize( from, OPT4OFF, OPT4OTHER ) ;
      file4optimize( keep, OPT4OFF, OPT4OTHER ) ;
      file4flush( from ) ;
      file4flush( keep ) ;

      keep->hand = from->hand ;
      from->hand = tmp.hand ;
      keep->doAllocFree = 0 ;
      c4strncpy( fromName, sizeof( fromName ), from->name, sizeof( fromName ) ) ;  // AS Dec 13/05 vs 5.0 fixes
      from->name = keep->name ;
      // AS Sept 17/01 - We don't want to mess with the validation table here, if replacing a file, so just change the marker
      file4setTemporary( from, 1, 0 ) ;
      if ( file4close ( from ) )
         return -1 ;
      if ( file4close ( keep ) )
         return -1 ;

      if ( u4rename( fromName, tmp.name ) < 0 )
         return -1 ;

      // AS Nov 21/02 - ensure we use the same accessmode as the old file... (EXCLUSIVE)
      int oldAccessMode = c4->accessMode ;
      c4->accessMode = OPEN4DENY_RW ;
      // AS May 24/02 - created file4openLow for internal use to indicate file types
      rc = file4openInternal( keep, c4, tmp.name, 0, tmp.type ) ;
      c4->accessMode = oldAccessMode ;
      if ( rc != 0 )
         return -1 ;

      // AS Sept 17/01 - We don't want to mess with the validation table here, if replacing a file, so just change the marker
      file4setTemporary( keep, file4getTemporary( &tmp ), 0 ) ;
      keep->doAllocFree = tmp.doAllocFree ;
      #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
         keep->preprocessed = tmp.preprocessed ;  // CS 2000/04/30
      #endif
      if ( keep->doAllocFree == 1 )  /* AS 3/4/98 also must assign NameBuf or memory leakage */
         keep->nameBuf = (char *)tmp.name ;
      #ifndef S4OFF_OPTIMIZE
         if ( tmp.link.n != 0 )   /* file was optimized... */
            file4optimizeLow( keep, c4->optimize, tmp.type, tmp.expectedReadSize, tmp.ownerPtr ) ;
      #endif
   #ifndef S4SINGLE
      }
      else  /* can't lose the file handle if other user's have the file open, so just do a copy */
      {
         FILE4LONG len ;
         /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
         file4longAssign( len, 0, 0L ) ;

         file4lenSetLow( keep, len ) ;

         bufSize = c4->memSizeBuffer ;

         #ifdef S4LOW_MEMORY
            #ifndef S4OFF_OPTIMIZE
               hasOpt = c4->hasOpt && c4->opt.numBuffers ;
               code4optSuspend( c4 ) ;
            #endif
         #endif

         for ( ;; bufSize -= 0x800 )
         {
            if ( bufSize < 0x800 )  /* make one last try */
            {
               bufSize = 100 ;
               buf = (char *)u4allocEr( c4, (long)bufSize ) ;
               if ( buf == 0 )
                  return -1 ;
            }
            buf = (char *)u4alloc( (long)bufSize ) ;
            if ( buf )
               break ;
         }

         /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
         file4longAssign( pos, 0, 0L ) ;
         for( fLen = file4lenLow( from ) ; file4longGreaterZero( fLen ) ; file4longSubtract( &fLen, (long)bufSize ) )
         {
            if ( ( file4longLess( fLen, bufSize) ) )
               bufSize = file4longGetLo( fLen ) ;

            if ( file4readAllInternal( from, pos, buf, bufSize ) < 0 )
            {
               rc = -1 ;
               break ;
            }
            if ( file4writeInternal( keep, pos, buf, bufSize ) < 0 )
            {
               rc = -1 ;
               break ;
            }
            file4longAdd( &pos, bufSize ) ;
         }
         // AS Sept 17/01 - We don't want to mess with the validation table here, if replacing a file, so just change the marker
         file4setTemporary( from, 1, 0 ) ;
         file4close( from ) ;
         u4free( buf ) ;
         #ifdef S4LOW_MEMORY
            #ifndef S4OFF_OPTIMIZE
               if ( hasOpt )
                  code4optRestart( c4 ) ;
            #endif
         #endif
      }
   #endif
   return rc ;
}



#ifdef S4NO_ECVT
   #define S4NO_ECVTFCVT
#endif
#ifdef S4NO_FCVT
   #define S4NO_ECVTFCVT
#endif

#ifdef S4NO_ECVTFCVT

#define MAXIMUM 30
#define PRECISION 17

static char valueStr[33] ; //CJ -01/03/00 increased by one incase rounding adds one digit.
static double minus[] = { 1e-256, 1e-128, 1e-64, 1e-32, 1e-16, 1e-8, 1e-4, 1e-2, 1e-1, 1.0 } ;  /* LY 2002/12/27 : fixed typo */
static double plus[] = { 1e+256, 1e+128, 1e+64, 1e+32, 1e+16, 1e+8, 1e+4, 1e+2, 1e+1 } ;



#ifdef S4NO_ECVT
   char *f4ecvt( double value, int numdigits, int *decPtr, int *signPtr )
   {
      int dptr, count, j, k ;
      char *vPtr ;

      if ( numdigits < 0 )
         numdigits = 0 ;
      else
         if ( numdigits > MAXIMUM ) numdigits = MAXIMUM ;

      if ( value < 0.0 )
      {
         value = -value ;
         *signPtr = 1 ;
      }
      else
         *signPtr = 0 ;

      if ( value == 0.0 )
      {
         c4memset( valueStr, '0', numdigits ) ;
         dptr = 0 ;
      }
      else
      {
         dptr = 1 ;
         k = 256 ;
         count = 0 ;
         while ( value < 1.0 )
         {
           while ( value < minus[count+1] )
           {
              value *= plus[count] ;
              dptr -= k ;
           }
           k /= 2 ;
           count++ ;
         }
         k = 256 ;
         count = 0 ;
         while ( value >= 10.0 )
         {
           while ( value >= plus[count] )
           {
              value *= minus[count] ;
              dptr += k ;
           }
           k /= 2 ;
           count++ ;
         }

         for ( vPtr = &valueStr[0]; vPtr <= &valueStr[numdigits]; vPtr++ )
         {
            if ( vPtr >= &valueStr[PRECISION] )  *vPtr = '0' ;
            else
            {
               /* LY July 7/03 : explicit cast to int for Linux compiler */
               j = (int)value ;
               *vPtr = j + '0' ;
               value = ( value - j + 1.0e-15 ) * 10.0 ;
            }
         }
         --vPtr ;
         if ( *vPtr >= '5' )
         {
           while (1)
           {
              if ( vPtr == &valueStr[0] )
              {
                 dptr++ ;
                 valueStr[0] = '1' ;
                 break ;
              }
              *vPtr = 0 ;
              --vPtr ;
              if ( *vPtr != '9' )
              {
                 (*vPtr)++ ;
                 break ;
              }
           }
         }
      }
      *decPtr = dptr ;
      valueStr[numdigits] = 0 ;
      return valueStr ;
   }
#endif /* S4NO_ECVT */



#ifdef S4NO_FCVT
   //CJ -01/03/00 - reviewed code and made changes to the function for proper behavoir.
   char *f4fcvt( double value, int numdigitsAfterDec, int *decPtr, int *signPtr )
   {
      int dptr, count, j, k ;
      int numdigitsTotal ;
      char *vPtr ;
      double tempDbl ;

      memset( valueStr, 0, sizeof( valueStr ) ) ;  // LY Jan 4/04

      if ( numdigitsAfterDec < 0 )
         numdigitsAfterDec = 0 ;
      else
         if ( numdigitsAfterDec > MAXIMUM ) numdigitsAfterDec = MAXIMUM ;

      /* LY 00/07/21 - changed from numberdigitsAfterDec */
      numdigitsTotal = numdigitsAfterDec ; //CJ Starting point of how many digits.

      if ( value < 0.0 )
      {
         value = -value ;
         *signPtr = 1 ;
      }
      else
         *signPtr = 0 ;

      if ( value == 0.0 )
      {
         c4memset( valueStr, '0', numdigitsTotal ) ;
         dptr = 0 ;
      }
      else
      {
         dptr = 1 ;
         k = 256 ;
         count = 0 ;
         while ( value < 1.0 )
         {
           while ( value < minus[count+1] )
           {
              value *= plus[count] ;
              dptr -= k ;
           }
           k /= 2 ;
           count++ ;
         }
         k = 256 ;
         count = 0 ;
         while ( value >= 10.0 )
         {
           while ( value >= plus[count] )
           {
              value *= minus[count] ;
              dptr += k ;
           }
           k /= 2 ;
           count++ ;
         }

         if ( ( numdigitsAfterDec + dptr ) > 0 )
         {
           numdigitsTotal += dptr ;
           if ( numdigitsTotal > MAXIMUM )  numdigitsTotal = MAXIMUM ;
         }

         valueStr[0] = '0' ;
         for ( vPtr = &valueStr[1]; vPtr <= &valueStr[numdigitsTotal+1]; vPtr++ )
         {
            if ( vPtr >= &valueStr[PRECISION+1] )  *vPtr = '0' ;
            else
            {
               /* LY July 7/03 : explicit cast to int for Linux compiler */
               j = (int)value ;
               tempDbl = (double) j ;
               *vPtr = j + '0' ;
               // AS Mar 22/07 - this fix doesn't work for f4fcvt(0.83, 8,...)  -- if someone has the example where it is required perhaps that will solve; another one is    result = f4fcvt(0.00059999999999999995, 6, &dec, &sign);
               /* LY 2002/08/09 : fix(?) for 100000000000.0001 */
               // if ( value - j >= 1.0e-14 )
               //    value = ( value - j + 1.0e-15) * 10.0 ;
               // else
                  value = ( value - j ) * 10.0 ;
            }
         }
         /* LY 4/29/99 : solve problem with 99.999999, decimal 5, returning as 1 */
         //CJ July 05/01- Rewrote the section in the the following if clause.
         //the function was truncating the digit instead of rounding up ( unless the first unseen digit was a nine)
         if ( *(vPtr-1) >= '5' )
         {
            vPtr -= 2 ;
            while ( 1 )
            {
               if ( *vPtr != '9' )
               {
                  (*vPtr)++ ;
                  if ( vPtr == &valueStr[0] )
                  {
                     numdigitsTotal++ ;
                     dptr++ ;
                  }
                  break;
               }
               else
               {
                  *vPtr = '0' ;
               }
               if ( vPtr > &valueStr[0] )
                  --vPtr ;
            }
         }
      }
      *decPtr = dptr ;
      if ( valueStr[0] == '0' )
      {
         valueStr[numdigitsTotal+1] = 0 ;
         vPtr = &valueStr[1] ;
      }else
      {
         valueStr[numdigitsTotal] = 0 ;
         vPtr = &valueStr[0] ;
      }
      return vPtr ;
   }
#endif /*S4NO_FCVT */
#endif /* S4NO_ECVTFCVT*/

#undef S4NO_ECVTFCVT



/* LY 2001/09/19 : used by d4dll.c */
int S4FUNCTION file4alloc( FILE4 S4PTR * S4PTR *f4 )
{
   *f4 = 0 ;
   *f4 = (FILE4*) malloc( sizeof(FILE4) ) ;
   if ( *f4 == 0 )
      return -1 ;
   return 0 ;
}



void S4FUNCTION file4free( FILE4 S4PTR * S4PTR *f4 )
{
   free( *f4 ) ;
}



#ifdef S4READ_ADVANCE

#define MEM4ADVANCE_START 10
#define MEM4ADVANCE_EXPAND 10

// AS Apr 13/04 - support for optimizing large files
int S4FUNCTION file4advanceRead( FILE4 *f4, FILE4LONG pos, void *data, const unsigned len, S4ADVANCE_FUNCTION *completionRoutine, void *completionData )
{
   FILE4ADVANCE_READ *advanceRead ;
   CODE4 *c4 ;

   c4 = f4->codeBase ;

   if ( c4->advanceReadsEnabled == 0 )  /* not enabled */
      return 0 ;

   f4->hasHadPendingAdvanceRead = 1 ;   /* just mark the file as advance-read file */

   if ( c4->advanceReadMemory == 0 )
      advanceRead = (FILE4ADVANCE_READ *)mem4createAllocZero( c4, &c4->advanceReadMemory, MEM4ADVANCE_START, sizeof( FILE4ADVANCE_READ ), MEM4ADVANCE_EXPAND, 0 ) ;
   else
      advanceRead = (FILE4ADVANCE_READ *)mem4allocZero( c4->advanceReadMemory ) ;

   if ( advanceRead == 0 )
      return error4( c4, e4memory, E90624 ) ;

   advanceRead->file = f4 ;
   advanceRead->data = (char *)data ;
   // AS Apr 13/04 - support for optimizing large files
   advanceRead->len = len ;
   file4longAssignLong( advanceRead->pos, pos ) ;
   advanceRead->usageFlag = r4queued ;
   advanceRead->completionRoutine = completionRoutine ;
   advanceRead->completionData = completionData ;

   critical4sectionEnter( &c4->critical4advanceReadList ) ;

   l4add( &c4->advanceReadList, advanceRead ) ;
   l4add( &f4->advanceReadFileList, &advanceRead->fileLink ) ;

   critical4sectionLeave( &c4->critical4advanceReadList ) ;

   SetEvent( c4->pendingReadEvent ) ;  /* notify the write thread */
   // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
   // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
   u4sleep( c4 ) ;

   return 0 ;
}



int file4advanceCancel( FILE4 *f4 )
{
   /* cancels all advance-reads for the given file */
   FILE4ADVANCE_READ *advanceRead ;
   LINK4 *advanceReadLink, *saved ;
   CODE4 *c4 ;

   c4 = f4->codeBase ;

   critical4sectionEnter( &c4->critical4advanceReadList ) ;

   for ( advanceReadLink = (LINK4 *)l4first( &f4->advanceReadFileList ) ;; )
   {
      if ( advanceReadLink == 0 )
         break ;
      advanceRead = (FILE4ADVANCE_READ *)(advanceReadLink - 1) ;
      saved = (LINK4 *)l4next( &f4->advanceReadFileList, advanceReadLink ) ;
      if ( advanceRead->usageFlag == r4queued )  /* do ourselves */
      {
         l4remove( &f4->advanceReadFileList, advanceReadLink ) ;
         l4remove( &c4->advanceReadList, advanceRead ) ;
         advanceRead->status = 0 ;
         advanceRead->usageFlag = r4finished ;
         mem4free( c4->advanceReadMemory, advanceRead ) ;
      }
      advanceReadLink = saved ;
   }

   critical4sectionLeave( &c4->critical4advanceReadList ) ;

   for ( ;; )
   {
      /* now verify that the checkInUse read gets completed */
      if ( l4numNodes( &f4->advanceReadFileList ) == 0 )
         break ;
      #ifdef E4ANALYZE
         if ( l4numNodes( &f4->advanceReadFileList ) > 1 )   /* in theory impossible, it means delay-write has 2 files writing at same time */
            return error4( c4, e4struct, E90624 ) ;
      #endif
      SetEvent( c4->pendingReadEvent ) ;  /* notify the write thread */
      Sleep( 0 ) ;   /* give up our time slice to get the delay-write going */
   }

   #ifndef S4OFF_OPTIMIZE
      if ( c4->opt.advanceReadFile == f4 )
      {
         c4->opt.advanceLargeBufferAvail = AR4EMPTY ;
         c4->opt.advanceReadFile = 0 ;
      }
   #endif

   return 0 ;
}



#ifdef S4USE_INT_DELAY
   int _cdecl file4advanceReadMain( void *data )
#else
   void _cdecl file4advanceReadMain( void *data )
#endif
{
   CODE4 *c4 ;
   FILE4ADVANCE_READ *advanceRead ;
   FILE4LONG tLong ;

   c4 = (CODE4 *)data ;
   // AS Nov 21/05 - due to an apparent vb/xp sequencing problem, see if we cancelled the initialization
   if ( c4->advanceReadsDisabled == 1 )
      return ;
   c4->advanceReadsEnabled = 1 ;

   for ( ;; )
   {
      if ( l4numNodes( &c4->advanceReadList ) == 0 )
      {
         if ( c4->uninitializeAdvanceRead == 1 )   /* shutdown */
         {
            SetEvent( c4->initUndoAdvanceRead ) ;
            #ifdef S4USE_INT_DELAY
               return 0 ;
            #else
               return ;
            #endif
         }
         else
         {
            WaitForSingleObject( c4->pendingReadEvent, INFINITE ) ;
            ResetEvent( c4->pendingReadEvent ) ;
         }
      }
      else  /* perform a read on the first available block */
      {
         critical4sectionEnter( &c4->critical4advanceReadList ) ;

         advanceRead = (FILE4ADVANCE_READ *)l4first( &c4->advanceReadList ) ;
         if ( advanceRead == 0 )   /* maybe got removed by main thread, so none to read... */
         {
            critical4sectionLeave( &c4->critical4advanceReadList ) ;
            Sleep( 0 ) ;
            continue ;
         }
         advanceRead->usageFlag = r4inUse ;
         critical4sectionLeave( &c4->critical4advanceReadList ) ;

         file4longAssignLong( tLong, advanceRead->pos ) ;
         advanceRead->status = file4readLowDo( advanceRead->file, tLong, advanceRead->data, advanceRead->len ) ;
         advanceRead->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
         critical4sectionEnter( &c4->critical4advanceReadList ) ;
         l4remove( &c4->advanceReadList, advanceRead ) ;
         l4remove( &advanceRead->file->advanceReadFileList, &advanceRead->fileLink ) ;

         /* the completion routine may get reset by another routine which
            needed to call it */
         if ( advanceRead->completionRoutine != 0 )
            advanceRead->completionRoutine( advanceRead ) ;

         /* for reading, the critical section includes the completion routine
            because it checks the status flag before modifying it */
         critical4sectionLeave( &c4->critical4advanceReadList ) ;
         mem4free( c4->advanceReadMemory, advanceRead ) ;
      }
   }
}



// AS Apr 13/04 - support for optimizing large files
void file4advanceReadWriteOver( FILE4 *f4, FILE4LONG pos, const unsigned len, const void *data, const int doCancel )
{
   /* this function takes a write request, and ensures that any advance-read
      information which overlaps the write request is updated to reflect the
      changes */
   /* if doCancel is true, it means that advance-reads which overlap the positions
      partially will be canceled since they are out of date (i.e. based on a write
      overlap request, not a re-request which is also serviced here) */
   FILE4ADVANCE_READ *advanceRead ;
   LINK4 *advanceLink ;
   CODE4 *c4 ;
   int enteredCriticalSection = 0 ;
   #ifndef S4OFF_OPTIMIZE
      OPT4 *opt ;
   #endif

   c4 = f4->codeBase ;

   #ifndef S4OFF_OPTIMIZE
      if ( f4->fileCreated == 0 )  /* ensure file created, else no critical section, can't be advance-reads */
         return ;
   #endif

   // AS Apr 13/04 - support for optimizing large files
   // if ( pos > ULONG_MAX - len )  /* means we will exceed - i.e. large file */
   //    return ;

   /* try to avoid entering/leaving critical section if possible, because
      it is a costly operation.  It is safe to analyze the lists because
      the only adder to the lists is ourselves, and they get removed only
      after the operations are completed */

   /* first take care of the outstanding advance-reads */
   if ( l4numNodes( &f4->advanceReadFileList ) != 0 )
   {
      /* first lock out operations on the advance-read list */
      enteredCriticalSection = 1 ;
      critical4sectionEnter( &c4->critical4advanceReadList ) ;
      for ( advanceLink = (LINK4 *)l4first( &f4->advanceReadFileList ) ;; )
      {
         if ( advanceLink == 0 )
            break ;
         advanceRead = (FILE4ADVANCE_READ *)(advanceLink - 1 ) ;
         advanceLink = (LINK4 *)l4next( &f4->advanceReadFileList, advanceLink ) ;

         // AS Apr 13/04 - support for optimizing large files
         // if ( ( advanceRead->pos + advanceRead->len ) <= pos ) /* outside of block */
         FILE4LONG advanceEnd ;
         file4longAssignLong( advanceEnd, advanceRead->pos ) ;
         file4longAdd( &advanceEnd, advanceRead->len ) ;
         if ( file4longLessEqLong( advanceEnd, pos ) ) /* outside of block */
            continue ;
         // if ( ( pos + len ) <= advanceRead->pos )  /* outside of block */
         FILE4LONG posEnd ;
         file4longAssignLong( posEnd, pos ) ;
         file4longAdd( &posEnd, len ) ;
         if ( file4longLessEqLong( pos, advanceRead->pos ) )  /* outside of block */
            continue ;

         /* if the status is in-use, then just wait for it to finish */
         while ( advanceRead->usageFlag == r4inUse )
            Sleep( 0 ) ;

         /* in this case, the piece is finished, so take care of the completion
            routine ourselves */
         /* once that is finished, it is assumed that the completion routine
            will make it noticed in the handling code below for after-read
            advance-read data */
         if ( advanceRead->usageFlag == r4finished )
         {
            if ( advanceRead->completionRoutine != 0 )
               advanceRead->completionRoutine( advanceRead ) ;
            advanceRead->completionRoutine = 0 ;
            continue ;
         }

         /* now, if the advance piece belongs in the buffer, then read all
            the info before the delay piece, copy the delay piece over,
            and read all the info after the delay piece */

         /* if the entire block is within the range, then can just copy to
            it, otherwise cancel it */
         // AS Apr 13/04 - support for optimizing large files
         // if ( ( advanceRead->pos >= pos ) && ( pos + len >= advanceRead->pos + advanceRead->len ) ) /* copy it */
         if ( file4longGreaterEqLong( advanceRead->pos, pos ) && file4longGreaterEqLong( posEnd, advanceEnd ) ) /* copy it */
         {
            advanceRead->status = 0 ;
            // memcpy( advanceRead->data, (const char *)data + (advanceRead->pos - pos ), advanceRead->len ) ;
            FILE4LONG adjustedPos ;
            file4longAssignLong( adjustedPos, advanceRead->pos ) ;
            file4longSubtractLong( &adjustedPos, &pos ) ;
            assert5( file4longGetHi( adjustedPos ) == 0 ) ;
            memcpy( advanceRead->data, (const char *)data + file4longGetLo( adjustedPos ), advanceRead->len ) ;
            advanceRead->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
         }
         else
         {
            if ( doCancel )
               advanceRead->usageFlag = r4canceled ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
            else
               continue ;
         }

         l4remove( &f4->codeBase->advanceReadList, advanceRead ) ;
         l4remove( &f4->advanceReadFileList, &advanceRead->fileLink ) ;
         if ( advanceRead->completionRoutine != 0 )
            advanceRead->completionRoutine( advanceRead ) ;
         mem4free( f4->codeBase->advanceReadMemory, advanceRead ) ;
      }
   }

   /* now take care of the special-advance-read buffer */
   /* if it is not full, then either it is empty or else we serviced it
      already in the advance-read list */
   if ( f4->advanceReadBufStatus == AR4FULL )
   {
      if ( enteredCriticalSection == 0 )
      {
         enteredCriticalSection = 1 ;
         critical4sectionEnter( &c4->critical4advanceReadList ) ;
      }

      // AS Apr 13/04 - support for optimizing large files
      // if ( pos < ( f4->advanceReadBufPos + (long)f4->advanceReadBufLen ) )  /* outside of block */
      FILE4LONG advanceEnd ;
      file4longAssignLong( advanceEnd, f4->advanceReadBufPos ) ;
      file4longAdd( &advanceEnd, (long)f4->advanceReadBufLen  ) ;
      if ( file4longLessLong( pos, advanceEnd ) )  /* outside of block */
      {
         FILE4LONG posEnd ;
         file4longAssignLong( posEnd, pos ) ;
         file4longAdd( &posEnd, len ) ;
         if ( file4longGreaterLong( posEnd, f4->advanceReadBufPos ) )  /* outside of block */
         {
            // if ( ( f4->advanceReadBufPos >= pos ) && ( pos + len >= f4->advanceReadBufPos + f4->advanceReadBufLen ) ) /* copy it */
            if ( file4longGreaterEqLong( f4->advanceReadBufPos, pos ) && file4longGreaterEqLong( posEnd, advanceEnd ) ) /* copy it */
            {
               FILE4LONG copyPos ;
               file4longAssignLong( copyPos, f4->advanceReadBufPos ) ;
               file4longSubtractLong( &copyPos, &pos ) ;
               assert5( file4longGetHi( copyPos ) == 0 ) ;
               memcpy( f4->advanceReadBuf, (const char *)data + file4longGetLo( copyPos ), f4->advanceReadBufLen ) ;
            }
            else
               if ( doCancel )
                  f4->advanceReadBufStatus = AR4EMPTY ;
         }
      }
   }

   /* now take care of the optimized advance-read buffer */
   #ifndef S4OFF_OPTIMIZE
      opt = &c4->opt ;

      /* if it is not full, then either it is empty or else we serviced it
         already in the advance-read list */
      if ( opt->advanceLargeBufferAvail == AR4FULL )
      {
         if ( enteredCriticalSection == 0 )
         {
            enteredCriticalSection = 1 ;
            critical4sectionEnter( &c4->critical4advanceReadList ) ;
         }
         // AS Apr 13/04 - support for optimizing large files
         // if ( pos < ( opt->advanceLargePos + opt->advanceLargeLen ) )  /* outside of block */
         FILE4LONG advanceCombined ;
         file4longAssignLong( advanceCombined, opt->advanceLargePos ) ;
         file4longAdd( &advanceCombined, opt->advanceLargeLen ) ;
         if ( file4longLessLong( pos, advanceCombined ) )  /* outside of block */
         {
            // if ( ( pos + len ) > opt->advanceLargePos )  /* outside of block */
            FILE4LONG posEnd ;
            file4longAssignLong( posEnd, pos ) ;
            file4longAdd( &posEnd, len ) ;
            if ( file4longGreaterLong( posEnd, opt->advanceLargePos ) )  /* outside of block */
            {
               // if ( ( opt->advanceLargePos >= pos ) && ( pos + len >= opt->advanceLargePos + opt->advanceLargeLen ) ) /* copy it */
               FILE4LONG advanceEnd ;
               file4longAssignLong( advanceEnd, opt->advanceLargePos ) ;
               file4longAdd( &advanceEnd, opt->advanceLargeLen ) ;
               if ( file4longGreaterEqLong( opt->advanceLargePos, pos ) && file4longGreaterEqLong( posEnd, advanceEnd ) ) /* copy it */
               {
                  FILE4LONG copyPos ;
                  file4longAssignLong( copyPos, opt->advanceLargePos ) ;
                  file4longSubtractLong( &copyPos, &pos ) ;
                  assert5( file4longGetHi( copyPos ) == 0 ) ;
                  memcpy( opt->advanceLargeBuffer, (const char *)data + file4longGetLo( copyPos ), opt->advanceLargeLen ) ;
               }
               else
                  if ( doCancel )
                     opt->advanceLargeBufferAvail = AR4EMPTY ;
            }
         }
      }
   #endif

   if ( enteredCriticalSection )
      critical4sectionLeave( &c4->critical4advanceReadList ) ;
}



void S4CALL file4advanceReadBufCompletionRoutine( void *advance )
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
         if ( advanceRead->file->advanceReadBufLen == advanceRead->status )
            *arFlag = AR4FULL ;
         else
            *arFlag = AR4EMPTY ;
      }
}



void opt4advanceReadBuf( FILE4 *f4, FILE4LONG pos, unsigned len )
{
   /*
      This function gets called to request an advance-read for a file
      to advance-read into the optimization buffers.  For example index
      blocks may be pre-read as the index file is being skipped.
   */
   CODE4 *c4 ;
   LINK4 *advanceLink ;
   FILE4ADVANCE_READ *advanceRead ;
   #ifndef S4OFF_OPTIMIZE
      long hashVal ;
      FILE4LONG adjustedPos ;
      unsigned int extraRead ;
      OPT4 *opt ;
   #endif

   c4 = f4->codeBase ;

   /* if optimization is enabled, then first ensure that the block is not
      already in memory */
   #ifndef S4OFF_OPTIMIZE
      opt = &f4->codeBase->opt ;
      if ( len > opt->blockSize )  /* don't do multi-block reads */
         return ;

      // AS Apr 13/04 - support for optimizing large files
      extraRead = (unsigned) ((unsigned long)((unsigned long)file4longGetLo( pos )  << opt->numShift ) >> opt->numShift ) ;
      file4longAssignLong( adjustedPos, pos ) ;
      file4longSubtract( &adjustedPos, extraRead ) ;
      hashVal = opt4fileHash( opt, f4, adjustedPos ) ;
      if ( opt4fileReturnBlock( f4, pos, hashVal ) != 0 )
         return ;
   #endif

   if ( f4->advanceReadBuf == 0 )
   {
      f4->hasHadPendingAdvanceRead = 1 ;   /* just mark the file as advance-read file */
      f4->advanceReadBuf = (char *)u4alloc( len ) ;
      if ( f4->advanceReadBuf == 0 )
         return ;
      f4->advanceReadBufStatus = AR4EMPTY ;
      f4->advanceReadBufLen = len ;
   }
   else
   {
      if ( len > f4->advanceReadBufLen )   /* too large, don't bother */
         return ;
      if ( f4->advanceReadBufStatus == AR4SET || f4->advanceReadBufStatus == AR4FULL )
         // AS Apr 13/04 - support for optimizing large files
         if ( file4longEqualLong( f4->advanceReadBufPos, pos ) )  /* already advanced on this read */
            return ;
   }

   #ifndef S4OFF_OPTIMIZE
      if ( f4->advanceReadBufStatus == AR4FULL )  /* finished read, so place into optimization */
      {
         /* first ensure that it is not already bufferred */
         // AS Apr 13/04 - support for optimizing large files
         extraRead = (unsigned) ((unsigned long)((unsigned long)file4longGetLo( f4->advanceReadBufPos ) << opt->numShift ) >> opt->numShift ) ;
         file4longAssignLong( adjustedPos, f4->advanceReadBufPos ) ;
         file4longSubtract( &adjustedPos, extraRead ) ;
         hashVal = opt4fileHash( opt, f4, adjustedPos ) ;
         if ( opt4fileReturnBlock( f4, adjustedPos, hashVal ) == 0 )
         {
            /* the opt4fileWrite function can be used to place data into memory
               without marking as changed based on last paramater... */
            opt4fileWrite( f4, f4->advanceReadBufPos, f4->advanceReadBufLen, f4->advanceReadBuf, 0 ) ;
         }
         f4->advanceReadBufStatus = AR4EMPTY ;
      }
   #endif

   critical4sectionEnter( &c4->critical4advanceReadList ) ;
   if ( f4->advanceReadBufStatus == AR4SET )  /* must remove from list */
   {
      for ( advanceLink = (LINK4 *)l4first( &f4->advanceReadFileList ) ;; )
      {
         if ( advanceLink == 0 )
            break ;
         advanceRead = (FILE4ADVANCE_READ *)(advanceLink - 1 ) ;
         advanceLink = (LINK4 *)l4next( &f4->advanceReadFileList, advanceLink ) ;

         if ( advanceRead->completionRoutine == file4advanceReadBufCompletionRoutine )  /* spec buf */
         {
            if ( advanceRead->usageFlag != r4queued )  /* must just wait */
            {
               critical4sectionLeave( &c4->critical4advanceReadList ) ;
               while ( f4->advanceReadBufStatus == AR4SET )
                  Sleep( 0 ) ;
               critical4sectionEnter( &c4->critical4advanceReadList ) ;
               break ;
            }

            /* is queued, so can just remove */
            advanceRead->status = 0 ;
            advanceRead->usageFlag = r4finished ;
            l4remove( &f4->codeBase->advanceReadList, advanceRead ) ;
            l4remove( &f4->advanceReadFileList, &advanceRead->fileLink ) ;
            mem4free( f4->codeBase->advanceReadMemory, advanceRead ) ;
         }
      }
   }

   #ifndef S4OFF_OPTIMIZE
      /* in optimized case, make sure the read is done on a block boundary */
      extraRead = (unsigned) ((unsigned long)((unsigned long)file4longGetLo( pos ) << opt->numShift ) >> opt->numShift ) ;
      file4longAssignLong( adjustedPos, pos ) ;
      file4longSubtract( &adjustedPos, extraRead ) ;
      // AS July 5/02 - It is possible to misread the data on the off chance that we are off the boundary
      f4->advanceReadBufStatus = AR4SET ;
      f4->advanceReadBufPos = adjustedPos ;

      if ( len + extraRead > f4->advanceReadBufLen )
      {
         // here we have the scenario where we need to increase the advance read buffer to perform our read
         u4free( f4->advanceReadBuf ) ;
         f4->advanceReadBufLen = 0 ;
         f4->advanceReadBuf = (char *)u4alloc( len + extraRead ) ;
         if ( f4->advanceReadBuf == 0 )
         {
            critical4sectionLeave( &c4->critical4advanceReadList ) ;
            return ;
         }
         f4->advanceReadBufLen = len + extraRead ;
      }

      unsigned adjustedLen = f4->advanceReadBufLen ;

      critical4sectionLeave( &c4->critical4advanceReadList ) ;

      file4advanceRead( f4, adjustedPos, f4->advanceReadBuf, adjustedLen, file4advanceReadBufCompletionRoutine, &f4->advanceReadBufStatus ) ;
   #else
      f4->advanceReadBufStatus = AR4SET ;
      f4->advanceReadBufPos = pos ;

      critical4sectionLeave( &c4->critical4advanceReadList ) ;

      file4advanceRead( f4, pos, f4->advanceReadBuf, len, file4advanceReadBufCompletionRoutine, &f4->advanceReadBufStatus ) ;
   #endif
}
#endif /* S4READ_ADVANCE */


// AS Nov 26/02 - Support for data file compression
// AS May 17/04 - client/server functionality to copmress the data file...
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   // AS Aug 1/03 - Large file support
   int file4compressInit( FILE4 *file, COMPRESS4HANDLER *compress, long compressHeaderOffset )
   {
      // must set physical file len before assigning compress info which supresses the real file length
      file->physicalFileLen = file4lenLow( file ) ;  // the actual file length (compressed)
      file->compressInfo = compress ;
      file->compressHeaderOffset = compressHeaderOffset ;   // if we read before this offset, use normal file i/o, else use compressed

      long sizeInfo ;
      switch( compress->isLongOrCompress )
      {
         case 0:
            sizeInfo = sizeof( COMPRESS4DATA_SHORT ) + (compress4handlerNumOffsetsGet( compress ) - 1) * sizeof( long ) ;  // -1 because sizeof() includes 1 entry
            break ;
         case 1:
            sizeInfo = sizeof( COMPRESS4DATA_LONG ) + (compress4handlerNumOffsetsGet( compress ) - 1) * sizeof( FILE4LONG ) ;  // -1 because sizeof() includes 1 entry
            break ;
         case 2:
            sizeInfo = sizeof( COMPRESS4WRITE_HEADER ) + (compress4handlerNumOffsetsGet( compress ) - 1) * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ;  // -1 because sizeof() includes 1 entry
            break ;
      }

      file->compressDataOffset = compressHeaderOffset + sizeInfo ;
      file->actualBlockSize = compress4handlerBlockSizeGet( compress ) * 1024 ;
      file->uncompressBuffer = (char *)u4allocFree( file->codeBase, file->actualBlockSize ) ;
      if ( file->uncompressBuffer == 0 )
         return e4memory ;
      file->compressBuffer = (char *)u4allocFree( file->codeBase, (long)(file->actualBlockSize * 1.01 ) + 12 ) ;
      if ( file->compressBuffer == 0 )
         return e4memory ;
      file4longAssignError( file->uBufFilePos ) ;
      file->uBufFileLen = -1 ;
      return 0 ;
   }


   // AS Feb 18/04 - ensure we free the compression memory consistently via a function
   void compress4infoFree( COMPRESS4HANDLER *info )
   {
      if ( info->shortCompress != 0 )
      {
         assert5( info->isLongOrCompress == 0 ) ;
         u4free( info->shortCompress ) ;
      }
      if ( info->longCompress != 0 )
      {
         assert5( info->isLongOrCompress == 1 ) ;
         u4free( info->longCompress ) ;
      }
      if ( info->writeCompress != 0 )
      {
         assert5( info->isLongOrCompress == 2 ) ;
         COMPRESS4WRITE_BLOCK *nextBlock = &(info->writeCompress->firstArrayBlock) ;
         nextBlock = nextBlock->nextBlockPtr ;
         while ( nextBlock != 0 )
         {
            COMPRESS4WRITE_BLOCK *afterNextBlock = nextBlock->nextBlockPtr ;
            u4free( nextBlock ) ;
            nextBlock = afterNextBlock ;
         }
         u4free( info->writeCompress->firstArrayBlock.writeArray ) ;
         u4free( info->writeCompress ) ;
      }

      u4free( info ) ;
   }


   void file4compressInitUndo( FILE4 *file )
   {
      if ( file->uncompressBuffer != 0 )
      {
         u4free( file->uncompressBuffer ) ;
         file->uncompressBuffer = 0 ;
      }
      if ( file->compressBuffer != 0 )
      {
         u4free( file->compressBuffer ) ;
         file->compressBuffer = 0 ;
      }
      if ( file->compressInfo != 0 )
      {
         if ( file->freeCompressInfo == 1 )
         {
            // AS Aug 1/03 - more memory to free now
            // AS Feb 18/04 - ensure we free the compression memory consistently via a function
            compress4infoFree( file->compressInfo ) ;
         }
         file->compressInfo = 0 ;
      }
   }



   // AS Jan 18/06 - moved compression array reading from d4open to here.  Needed to generalize this code.
   //  In particular in multi-user another user may add an entry to the array.  In that case we need to read this array at run time.
   int file4compressInitArrayOffsets( FILE4 *file, COMPRESS4WRITE_HEADER *writeCompress, FILE4LONG pos )
   {
      #ifdef S4DELAY_WRITE_MT
         critical4sectionEnter( &file->critical4file ) ;
      #endif
      // this function reads in the list of array blocks for a compressed table.  (dbf files only supported).
      // if the file is extended, new array entries are added.  Sometimes therefore these need to be refreshed from disk.
      // it is also called when the table is first opened and compression initialized
      // read the first set of array offsets
      COMPRESS4WRITE_BLOCK *blockPtr = &(writeCompress->firstArrayBlock) ;
      // re-read the first arrays data...
      file4longAdd( &pos, sizeof( COMPRESS4WRITE_HEADER ) - 2 * sizeof( void *) - sizeof( FILE4LONG ) ) ;
      if ( file4readOnBoundary( file, pos, &blockPtr->nextBlock, sizeof( FILE4LONG ) ) != sizeof( FILE4LONG ) )
      {
         #ifdef S4DELAY_WRITE_MT
            critical4sectionLeave( &file->critical4file ) ;
         #endif
         return -1 ;
      }

      file4longAdd( &pos, 2 * sizeof( void *) + sizeof( FILE4LONG ) ) ;  // AS Apr 21/06 subtraction error fixed ... t4data problem
      unsigned long firstOffsets = blockPtr->numEntries ;
      unsigned lenRead = (firstOffsets) * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ;
      if ( file4readOnBoundary( file, pos, writeCompress->firstArrayBlock.writeArray, lenRead ) != lenRead )
      {
         #ifdef S4DELAY_WRITE_MT
            critical4sectionLeave( &file->critical4file ) ;
         #endif
         return -1 ;
      }

      unsigned long numOffsets = blockPtr->numEntries ;

      while ( !file4longEqualZero( blockPtr->nextBlock ) )
      {
         FILE4LONG arrayPos = blockPtr->nextBlock ;
         file4longAdd( &arrayPos, 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ) ) ;  // the array immediately follows the block data
         blockPtr->nextBlockPtr = (COMPRESS4WRITE_BLOCK *)u4allocFree( file->codeBase, sizeof(COMPRESS4WRITE_BLOCK) ) ;
         if ( file4readOnBoundary( file, blockPtr->nextBlock, blockPtr->nextBlockPtr, 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ) ) != 2 * sizeof( unsigned long ) + sizeof( FILE4LONG ) )
         {
            #ifdef S4DELAY_WRITE_MT
               critical4sectionLeave( &file->critical4file ) ;
            #endif
            return -1 ;
         }
         blockPtr = blockPtr->nextBlockPtr ;
         if ( file4readOnBoundary( file, arrayPos, &(writeCompress->firstArrayBlock.writeArray[numOffsets]), blockPtr->numEntries * sizeof(COMPRESS4WRITE_ARRAY_ENTRY) ) != blockPtr->numEntries * sizeof(COMPRESS4WRITE_ARRAY_ENTRY) )
         {
            #ifdef S4DELAY_WRITE_MT
               critical4sectionLeave( &file->critical4file ) ;
            #endif
            return -1 ;
         }

         blockPtr->writeArray = &(writeCompress->firstArrayBlock.writeArray[numOffsets]) ;
         numOffsets += blockPtr->numEntries ;
      }
      #ifdef S4DELAY_WRITE_MT
         critical4sectionLeave( &file->critical4file ) ;
      #endif
      return 0 ;
   }
#endif
