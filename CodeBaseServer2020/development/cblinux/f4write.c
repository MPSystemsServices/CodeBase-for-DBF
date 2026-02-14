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
   #endif
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
#endif


// AS May 24/02 - created file4createInternal for internal use to indicate file types
static unsigned file4writeLowDoInternal( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len )
{
   /* returns the length written */
   #if defined( S4WIN32 ) || defined( S4MACINTOSH )
      unsigned long urc ;
   #endif
   #ifdef S4MACINTOSH
      long longPtrLen ;
   #endif

   #if defined( S4WIN32 )
      if ( SetFilePointer( f4->hand, file4longGetLo( pos ), file4longGetHiAddress( pos ), FILE_BEGIN ) == (DWORD)-1 )
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
         error4describe( f4->codeBase, e4write, E90619, f4->name, 0, 0 ) ;
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
         WriteFile( f4->hand, ptr, len, &urc, NULL ) ;
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



static unsigned file4writeLowDo( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned len )
{
   unsigned urc ;
   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      EnterCriticalSection( &f4->critical4file ) ;
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
            urc = file4writeLowDoInternal( f4, pos, ptr, len ) ;
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
                     LeaveCriticalSection( &f4->critical4file ) ;
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
                        LeaveCriticalSection( &f4->critical4file ) ;
                     #endif
                     error4( c4, e4info, E90619 ) ;
                     return 0 ;
                  }
               }
            }
            // copy the data we are writing into the buffer we just read
            memcpy( (char *)c4->filePreprocessBuffer + posMod, ptr, writeLen - lenMod ) ;
            // and write the buffer (which is now on the correct boundaries) back out.
            urc = file4writeLowDoInternal( f4, writePos, c4->filePreprocessBuffer, writeLen ) ;
            urc = min ( urc, (writeLen - lenMod ) ) ;  // adjust output length to not include extra read for decryption only
            // don't worry if we over copy into the 'ptr' buffer from the encrypt buffer
         }
      }
      else
   #endif
         urc = file4writeLowDoInternal( f4, pos, ptr, len ) ;

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      LeaveCriticalSection( &f4->critical4file ) ;
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
         #ifdef S4FILE_EXTENDED
            && ( f4->isLong == 0 ) && file4longGetHi( pos ) == 0
         #endif
         )
         if ( f4->hasHadPendingAdvanceRead )  /* for efficiency, avoid for non-advance-read files */
            file4advanceReadWriteOver( f4, file4longGetLo( pos ), len, ptr, 1 ) ;
   #endif

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      EnterCriticalSection( &f4->critical4file ) ;
   #endif

   /* now flush the transaction file if a data file */
   #if !defined( S4OFF_TRAN ) && !defined( S4OFF_OPTIMIZE )
      #ifdef S4SERVER
         if ( f4->type == OPT4DBF && c4->server->c4trans.transFile != NULL )
            tran4fileFlush( c4->server->c4trans.transFile ) ;
      #else
         if ( f4->type == OPT4DBF && (code4trans( c4 ))->c4trans->transFile != NULL )
            tran4fileFlush( (code4trans( c4 ))->c4trans->transFile ) ;
      #endif
   #endif

   #ifdef S4WRITE_DELAY
      if ( checkDelayList /* check to see if write request covers delayed areas */
         #ifdef S4FILE_EXTENDED
            && f4->isLong == 0
         #endif
         )
         if ( l4numNodes( &f4->delayWriteFileList ) != 0 )
         {
            #ifdef E4ANALYZE
               if ( file4longGetHi( pos ) != 0 )
               {
                  #ifdef S4DELAY_WRITE_MT
                     LeaveCriticalSection( &f4->critical4file ) ;
                  #endif
                  return error4( c4, e4result, E80606 ) ;
               }
            #endif
            posShort = file4longGetLo( pos ) ;
            // AS 04/25/00 --> critical sectioning missing...
            EnterCriticalSection( &c4->critical4delayWriteList ) ;
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
                  Sleep( 0 ) ;

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
            LeaveCriticalSection( &c4->critical4delayWriteList ) ;
         }
   #endif

   urc = file4writeLowDo( f4, pos, ptr, len ) ;

   #ifdef S4DELAY_WRITE_MT
      LeaveCriticalSection( &f4->critical4file ) ;
   #endif

   if ( urc != len )
      return error4describe( c4, e4write, E90619, f4->name, 0, 0 ) ;

   #ifndef S4OFF_MULTI
      /* doFlush is false in write-delay scenario where this is not relevant (i.e. gets
         flushed by non-delay thread) */
      if ( f4->codeBase->fileFlush && doFlush )
         file4flush( f4 ) ;
   #endif

   return 0 ;
}



#ifndef S4OFF_OPTIMIZE
#ifdef P4ARGS_USED
   #pragma argsused
#endif
#ifdef S4WRITE_DELAY
   int file4writeOpt( FILE4 *f4, unsigned long pos, const void *ptr, const unsigned len, int doDelay, S4DELAY_FUNCTION *completionRoutine, void *completionData )
#else
   int file4writeOpt( FILE4 *f4, unsigned long pos, const void *ptr, const unsigned len, int doDelay, void *completionRoutine, void *completionData )
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
            if ( file4longGetLo( file4lenLow( f4 ) ) < pos )
            {
               /* need to null out any extra file contents in order for file verification for optimization
                  to work properly */
               memset( buf, 0, sizeof( buf ) ) ;
               while ( file4longGetLo( file4lenLow( f4 ) ) < pos )
               {
                  bufWritePos = file4longGetLo( file4lenLow( f4 ) ) ;
                  bufWriteLen = (pos - bufWritePos) ;
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

      file4longAssign( tLong, pos, 0L ) ;

      #ifdef S4WRITE_DELAY
         if ( doDelay == 1
            #ifdef S4FILE_EXTENDED
               && f4->isLong == 0
            #endif
            && pos == 0 )
            rc = file4writeDelay( f4, tLong, ptr, len, completionRoutine, completionData ) ;
         else
      #endif
      {
         rc = file4writeLow( f4, tLong, ptr, len, 1, 1, 1 ) ;
      }
   }

   #ifdef E4ANALYZE_ALL
      if ( f4->hasDup == 1 )
         if ( f4->doBuffer == 1 || f4->link.n == 0 )
            if ( file4writePart( ptr, f4, pos, len ) != 0 )
               return error4( c4, e4opt, E80602 ) ;
   #endif

   return rc ;
}
#endif /* S4OFF_OPTIMIZE */



int S4FUNCTION file4write( FILE4 *f4, const long pos, const void *ptr, const unsigned len )
{
   FILE4LONG fpos ;

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
      if ( f4->hand == INVALID4HANDLE )
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
            #ifdef S4FILE_EXTENDED
               && f4->isLong == 0
            #endif
            )
            if ( f4->hasHadPendingAdvanceRead )  /* for efficiency, avoid for non-advance-read files */
               file4advanceReadWriteOver( f4, file4longGetLo( pos ), len, ptr, 1 ) ;
      #endif
      #ifdef S4FILE_EXTENDED
         if ( f4->isLong == 0 && file4longGetHi( pos ) == 0 &&
            ( (file4longGetLo( pos ) + len) > file4longGetLo( pos ) ) &&
            ( file4longGetLo( pos ) + len < 0xffffffff ) )  /* LY 2001/04/16 : pos+len can be >4GB in 64bit */
            return file4writeOpt( f4, file4longGetLo( pos ), ptr, len, 0, 0, 0 ) ;
         else  /* not optimized in this case */
         {
            if ( f4->doBuffer == 1 )
               file4optimize( f4, OPT4OFF, OPT4OTHER ) ;
            return file4writeLow( f4, pos, ptr, len, 1, 0, 1 ) ;
         }
      #else
         return file4writeOpt( f4, file4longGetLo( pos ), ptr, len, 0, 0, 0 ) ;
      #endif
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
      EnterCriticalSection( &c4->critical4delayWriteList ) ;

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
            Sleep( 0 ) ;

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
            writeDelay->status = file4writeLow( writeDelay->file, writeDelay->pos, writeDelay->data, writeDelay->len, 0, 1, 1 ) ;

         writeDelay->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
         l4remove( &f4->codeBase->delayWriteList, writeDelay ) ;
         l4remove( &f4->delayWriteFileList, &writeDelay->fileLink ) ;
         writeDelay->completionRoutine( writeDelay ) ;
         mem4free( c4->delayWriteMemory, writeDelay ) ;
      }
      LeaveCriticalSection( &c4->critical4delayWriteList ) ;
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

   EnterCriticalSection( &c4->critical4delayWriteList ) ;

   l4add( &c4->delayWriteList, writeDelay ) ;
   l4add( &f4->delayWriteFileList, &writeDelay->fileLink ) ;

   LeaveCriticalSection( &c4->critical4delayWriteList ) ;

   SetEvent( c4->pendingWriteEvent ) ;  /* notify the write thread */
   Sleep( 0 ) ;

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

   EnterCriticalSection( &c4->critical4delayWriteList ) ;

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
            writeDelay->status = file4writeLow( writeDelay->file, tLong, writeDelay->data, writeDelay->len, 0, 1, 1 ) ;
         }
         else
            writeDelay->status = 0 ;
         writeDelay->usageFlag = r4finished ;
         writeDelay->completionRoutine( writeDelay ) ;
         mem4free( c4->delayWriteMemory, writeDelay ) ;
      }
      writeDelayLink = saved ;
   }

   LeaveCriticalSection( &c4->critical4delayWriteList ) ;

   for ( ;; )
   {
      /* now verify that the checkInUse write gets completed */
      if ( l4numNodes( &file->delayWriteFileList ) == 0 )
         break ;
      #ifdef E4ANALYZE
         if ( l4numNodes( &file->delayWriteFileList ) > 1 )   /* in theory impossible, it means delay-write has 2 files writing at same time */
            return error4( c4, e4struct, E90624 ) ;
      #endif
      SetEvent( c4->pendingWriteEvent ) ;  /* notify the write thread */
      Sleep( 0 ) ;   /* give up our time slice to get the delay-write going */
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
         EnterCriticalSection( &c4->critical4delayWriteList ) ;
         writeDelay = (FILE4WRITE_DELAY *)l4first( &c4->delayWriteList ) ;
         if ( writeDelay == 0 )   /* maybe got removed by main thread, so none to write... */
         {
            LeaveCriticalSection( &c4->critical4delayWriteList ) ;
            Sleep( 0 ) ;
            continue ;
         }
         writeDelay->usageFlag = r4inUse ;
         LeaveCriticalSection( &c4->critical4delayWriteList ) ;

         file4longAssignLong( tLong, writeDelay->pos ) ;
         writeDelay->status = file4writeLow( writeDelay->file, tLong, writeDelay->data, writeDelay->len, 0, 1, 0 ) ;
         writeDelay->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
         EnterCriticalSection( &c4->critical4delayWriteList ) ;
         l4remove( &c4->delayWriteList, writeDelay ) ;
         l4remove( &writeDelay->file->delayWriteFileList, &writeDelay->fileLink ) ;
         LeaveCriticalSection( &c4->critical4delayWriteList ) ;
         writeDelay->completionRoutine( writeDelay ) ;
         mem4free( c4->delayWriteMemory, writeDelay ) ;
      }
   }
}
#endif /* S4WRITE_DELAY */



/* LY 2003/07/07 */
// AS Nov 26/02 - New function for data file compression
#if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && !defined( S4LUPACH )
   int file4copy( FILE4 *to, FILE4 *from )
   {
      // both files must be created and open.
      // the contents of the 'from' file are copied to the 'to' file
      assert5port( "function for copying files required for d4compress" ) ;
      CODE4 *c4 = to->codeBase ;

      int rc = -1 ;
      char buf[1024] ;
      long p1 ;

      if ( error4code( c4 ) != 0 )
         return -1 ;

      p1 = 0L ;

      for( ;; )
      {
         unsigned lenRead = file4read( from, p1, buf, sizeof( buf ) ) ;
         if ( lenRead == 0 ) // done
            break ;
         if ( file4write( to, p1, buf, lenRead ) != 0 )
            return -1 ;

         if ( lenRead < sizeof( buf ) )  // we are done
         {
            rc = 0 ;
            break ;
         }

         p1 += sizeof( buf ) ;
      }

      if ( error4code( from->codeBase ) < 0 || file4len( from ) != file4len( to ) )
         return -1 ;

      return 0 ;
   }
#endif /* !S4OFF_WRITE, S4STAND_ALONE, S4FOX */
