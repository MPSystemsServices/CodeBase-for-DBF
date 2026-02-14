/* f4filese.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4ADVANCE_READ
void S4CALL file4seqReadCompletionRoutine( void *advance )
{
   FILE4SEQ_READ *seqRead ;

   /* the completion data points to the entire FILE4SEQ_READ structure */
   seqRead = ((FILE4SEQ_READ *)((FILE4ADVANCE_READ *)(advance))->completionData) ;

   if ( seqRead->buffer == seqRead->buf1 )  /* then this pre-read is the 2nd buffer */
   {
      seqRead->buf2status = ((FILE4ADVANCE_READ *)advance)->status ;
      seqRead->buf2avail = AR4FULL ;
   }
   else
   {
      seqRead->buf1status = ((FILE4ADVANCE_READ *)advance)->status ;
      seqRead->buf1avail = AR4FULL ;
   }
}

unsigned S4FUNCTION file4seqReadAdvance( FILE4SEQ_READ *seqRead, void *ptr, const unsigned len )
{
   unsigned int urc ;
   CODE4 *c4 ;

   /* avail is zero if here, so get the other, pre-read buffer (if/when
      available).  Then set the next one to read-ahead.  Actually, set the
      first one to pre-read first (since more optimized).  If neither are
      set up for advance-reading (i.e., is empty and not pre-read-set), then
      read the first one, and set the 2nd up for pre-reading */

   c4 = seqRead->file->codeBase ;

   #ifdef E4PARM_LOW
      if ( seqRead->avail != 0 )
      {
         error4( c4, e4parm_null, E90701 ) ;
         return 0 ;
      }
      #ifdef S4FILE_EXTENDED
         if ( seqRead->file->isLong != 0 )  /* should never get here then */
         {
            error4( c4, e4parm, E90701 ) ;
            return 0 ;
         }
      #endif
   #endif

   #ifdef E4ANALYZE
      if ( seqRead->doAdvance == 0 )
      {
         error4( c4, e4struct, E90701 ) ;
         return 0 ;
      }
   #endif

   if ( seqRead->buffer == seqRead->buf1 )
   {
      while ( seqRead->buf2avail == AR4SET )  /* wait for it to finish read */
         Sleep( 0 ) ;

      seqRead->buffer = seqRead->buf2 ;

      if ( seqRead->buf2avail == AR4EMPTY )  /* read current one now, advance-read other (after) */
      {
         urc = file4readLow( seqRead->file, seqRead->pos, seqRead->buffer, seqRead->nextReadLen ) ;
      }
      else
      {
         #ifdef E4ANALYZE
            if ( seqRead->buf2avail != AR4FULL )  /* invalid setting */
            {
               error4( c4, e4struct, E90701 ) ;
               return 0 ;
            }
         #endif
         urc = seqRead->buf2status ;
      }

      seqRead->avail = seqRead->working = urc ;

      if ( seqRead->working == UINT_MAX )
      {
         file4readError( seqRead->file, seqRead->pos, seqRead->nextReadLen, "file4seqRead" ) ;
         return 0 ;
      }

      #ifdef E4ANALYZE
         /* Make sure reading is aligned correctly for maximum speed */
         if ( ( file4longGetLo( seqRead->pos ) + seqRead->nextReadLen ) % 0x400 && seqRead->avail )
         {
            error4( c4, e4result, E90701 ) ;
            return 0 ;
         }
      #endif
      file4longAdd( &seqRead->pos, seqRead->working ) ;
      seqRead->nextReadLen = seqRead->total ;

      /* now pre-read the first one */
      if ( (long)urc == (long)seqRead->nextReadLen )  /* ensure that it is available for reading */
      {
         seqRead->buf1avail = AR4SET ;
         file4advanceRead( seqRead->file, file4longGetLo( seqRead->pos ), seqRead->buf1, seqRead->nextReadLen, file4seqReadCompletionRoutine, seqRead ) ;
      }
   }
   else
   {
      #ifdef E4ANALYZE
         if ( seqRead->buffer != seqRead->buf2 )
         {
            error4( c4, e4struct, E90701 ) ;
            return 0 ;
         }
      #endif
      while ( seqRead->buf1avail == AR4SET )  /* wait for it to finish read */
         Sleep( 0 ) ;

      seqRead->buffer = seqRead->buf1 ;

      if ( seqRead->buf1avail == AR4EMPTY )  /* read current one now, advance-read other (after) */
      {
         urc = file4readLow( seqRead->file, seqRead->pos, seqRead->buffer, seqRead->nextReadLen ) ;
      }
      else
      {
         #ifdef E4ANALYZE
            if ( seqRead->buf1avail != AR4FULL )  /* invalid setting */
            {
               error4( c4, e4struct, E90701 ) ;
               return 0 ;
            }
         #endif
         urc = seqRead->buf1status ;
      }

      seqRead->avail = seqRead->working = urc ;

      if ( seqRead->working == UINT_MAX )
      {
         file4readError( seqRead->file, seqRead->pos, seqRead->nextReadLen, "file4seqRead" ) ;
         return 0 ;
      }

      #ifdef E4ANALYZE
         /* Make sure reading is aligned correctly for maximum speed */
         if ( ( file4longGetLo( seqRead->pos ) + seqRead->nextReadLen ) % 0x400 && seqRead->avail )
         {
            error4( c4, e4result, E90701 ) ;
            return 0 ;
         }
      #endif
      file4longAdd( &seqRead->pos, seqRead->working ) ;
      seqRead->nextReadLen = seqRead->total ;

      /* now pre-read the first one */
      if ( (long)urc == (long)seqRead->nextReadLen )  /* ensure that it is available for reading */
      {
         seqRead->buf2avail = AR4SET ;
         file4advanceRead( seqRead->file, file4longGetLo( seqRead->pos ), seqRead->buf2, seqRead->nextReadLen, file4seqReadCompletionRoutine, seqRead ) ;
      }
   }

   return urc ;
}

/* ensures that any advance-reads are complete */
void file4seqReadInitUndo( const FILE4SEQ_READ *seqRead )
{
   if ( seqRead->doAdvance == 1 )
   {
      while ( seqRead->buf1avail == AR4SET )  /* wait for it to finish read */
         Sleep( 0 ) ;
      while ( seqRead->buf2avail == AR4SET )  /* wait for it to finish read */
         Sleep( 0 ) ;
   }
}
#endif  /* S4ADVANCE_READ */

unsigned S4FUNCTION file4seqRead( FILE4SEQ_READ *seqRead, void *ptr, unsigned len )
{
   CODE4 *c4 ;
   unsigned bufferI, copyBytes, urc ;

   #ifdef E4PARM_HIGH
      if ( seqRead == 0 || ( ptr == 0 && len ) )
      {
         error4( 0, e4parm_null, E90701 ) ;
         return 0 ;
      }
   #endif
   #ifdef E4ANALYZE
      if ( seqRead->file == 0 )
      {
         error4( 0, e4parm, E90701 ) ;
         return 0 ;
      }
   #endif

   c4 = seqRead->file->codeBase ;
   if ( c4 == 0 )  /* file closed */
      return 0 ;
   if ( error4code( c4 ) < 0 )
      return 0 ;

   if ( seqRead->buffer == 0 )
   {
      urc = file4readLow( seqRead->file, seqRead->pos, ptr, len ) ;
      file4longAdd( &seqRead->pos, urc ) ;
      return urc ;
   }

   if ( seqRead->avail == 0 )
   {
      #ifdef E4ANALYZE
         if ( file4longError( seqRead->pos ) < 0 )
         {
            error4( c4, e4result, E90701 ) ;
            return 0 ;
         }
      #endif

      #ifdef S4ADVANCE_READ
         if ( seqRead->doAdvance == 1
            #ifdef S4FILE_EXTENDED
               && seqRead->file->isLong == 0
            #endif
         )
            file4seqReadAdvance( seqRead, ptr, len ) ;
         else
      #endif
         {  /* brace needed for else above */
            urc = file4readLow( seqRead->file, seqRead->pos, seqRead->buffer, seqRead->nextReadLen ) ;
            seqRead->avail = seqRead->working = urc ;

            if ( seqRead->working == UINT_MAX )
            {
               file4readError( seqRead->file, seqRead->pos, seqRead->nextReadLen, "file4seqRead" ) ;
               return 0 ;
            }

            #ifdef E4ANALYZE
               /* Make sure reading is aligned correctly for maximum speed */
               #ifndef S4FILE_EXTENDED
                  /* the test is too much work to do at this point for extended... */
                  if ( ( seqRead->pos + file4longGetLo( seqRead->nextReadLen ) ) % 0x400 && seqRead->avail )
                  {
                     error4( c4, e4result, E90701 ) ;
                     return 0 ;
                  }
               #endif
            #endif
            file4longAdd( &seqRead->pos, seqRead->working ) ;
            seqRead->nextReadLen = seqRead->total ;
         }
   }

   if ( len <= seqRead->avail )
   {
      bufferI = seqRead->working - seqRead->avail ;
      c4memcpy( ptr, seqRead->buffer + bufferI, len ) ;
      seqRead->avail -= len ;
      return len ;
   }
   else
   {
      if ( seqRead->avail == 0 )
         return 0 ;

      bufferI = seqRead->working - seqRead->avail ;
      c4memcpy( ptr, seqRead->buffer + bufferI, seqRead->avail ) ;

      copyBytes = seqRead->avail ;
      seqRead->avail = 0 ;

      len -= copyBytes ;
      urc = file4seqRead( seqRead, (char *)ptr + copyBytes, len ) ;
      if ( error4code( c4 ) < 0 )
         return 0 ;

      return urc + copyBytes ;
   }
}

int S4FUNCTION file4seqReadAll( FILE4SEQ_READ *seqRead, void *ptr, const unsigned int len )
{
   unsigned lenRead ;

   #ifdef E4PARM_HIGH
      if ( seqRead == 0 || ( ptr == 0 && len ) )
         return error4( 0, e4parm_null, E90702 ) ;
   #endif
   #ifdef E4ANALYZE
      if ( seqRead->file == 0 )
         return error4( 0, e4parm, E90702 ) ;
   #endif

   lenRead = file4seqRead( seqRead, ptr, len ) ;
   if ( error4code( seqRead->file->codeBase ) < 0 )
      return -1 ;

   if ( lenRead != len )
      return file4readError( seqRead->file, seqRead->pos, seqRead->nextReadLen, "file4seqReadAll" ) ;
   return 0 ;
}

#ifndef S4INTERNAL_COMPILE_CHECK
int S4FUNCTION file4seqReadInit( FILE4SEQ_READ *seqRead, FILE4 *file, long startPos, void *ptr, const unsigned ptrLen )
{
   FILE4LONG pos ;
   file4longAssign( pos, startPos, 0 ) ;
   return file4seqReadInitDo( seqRead, file, pos, ptr, ptrLen, 0 ) ;
}
#endif

/* note that setting do-advance to '1' means that an un-init routine for the
   file4seqRead must be called (to ensure that there are no outstanding reads) */
S4EXPORT int S4FUNCTION file4seqReadInitDo( FILE4SEQ_READ *seqRead, FILE4 *file, FILE4LONG startPos, void *ptr, const unsigned ptrLen, const int doAdvance )
{
   #ifdef E4PARM_HIGH
      if ( file4longError( startPos ) < 0 || seqRead == 0 || file == 0 ) /* Don't check ptr. It can be null. */
         return error4( 0, e4parm, E90703 ) ;
   #endif

   c4memset( (void *)seqRead, 0, sizeof( FILE4SEQ_READ ) ) ;

   #ifndef S4OFF_OPTIMIZE
      opt4fileFlush( file, 1 ) ;
   #endif
   if ( ptr != 0 && ptrLen > 0 )
   {
      FILE4LONG compare ;
      #ifdef S4ADVANCE_READ
         if ( doAdvance == 1 && ptrLen >= 2048 )   /* need a minimum of 2k to make advance-reading worthwhile */
         {
            seqRead->doAdvance = 1 ;
            if ( (long)ptrLen > 65535 )   /* 32 bit o/s, can be > 64k */
               seqRead->total = (ptrLen / 2) & 0xFFFFFC00 ;  /* Make it a multiple of 1K */
            else
               seqRead->total = (ptrLen / 2) & 0xFC00 ;  /* Make it a multiple of 1K */
            seqRead->buf1 = (char *)ptr ;
            seqRead->buf2 = (char *)ptr + seqRead->total ;
            seqRead->buf1avail = AR4EMPTY ;
            seqRead->buf2avail = AR4EMPTY ;
            seqRead->buffer = seqRead->buf1 ;
         }
         else
         {  /* braces reqd. for else inside ifdef above */
            seqRead->doAdvance = 0 ;
      #endif
            if ( (long)ptrLen > 65535 )   /* 32 bit o/s, can be > 64k */
               seqRead->total = ptrLen & 0xFFFFFC00L ;  /* Make it a multiple of 1K */
            else
               seqRead->total = ptrLen & 0xFC00 ;  /* Make it a multiple of 1K */
            seqRead->buffer = (char *)ptr ;
      #ifdef S4ADVANCE_READ
         }
      #endif

      file4longAssign( compare, file4longGetLo( startPos ) % 0x400, file4longGetHi( startPos ) ) ;

      if ( file4longLess( compare, seqRead->total ) )
         seqRead->nextReadLen = seqRead->total - file4longGetLo( compare ) ;
   }
   file4longAssign( seqRead->pos, file4longGetLo( startPos ), file4longGetHi( startPos ) ) ;
   seqRead->file = file ;

   return 0 ;
}

#ifdef S4WRITE_DELAY
void S4CALL file4seqWriteCompletionRoutine( void *delay )
{
   /* just resets the avail flag */
   #ifdef S4WIN64 /* LY 00/10/17 */
      memset( ((FILE4WRITE_DELAY *)delay)->completionData, 0, sizeof(int) ) ;
      memset( ((FILE4WRITE_DELAY *)delay)->completionData, 1, 1 ) ;
   #else
      *((int *)(((FILE4WRITE_DELAY *)(delay))->completionData)) = 1 ;
   #endif
}

int S4FUNCTION file4seqWriteDelay( FILE4SEQ_WRITE *seqWrite )   /* not static due to testing exportation */
{
   CODE4 *c4 ;
   #ifdef S4WIN32
      #ifndef S4WRITE_DELAY
         long urc ;
      #endif
   #endif
   unsigned toWrite ;

   #ifdef E4PARM_HIGH
      if ( seqWrite == 0 )
         return error4( 0, e4parm_null, E90705 ) ;
   #endif

   if ( seqWrite->file == 0 )  /* nothing to flush, just return */
      return 0 ;
   c4 = seqWrite->file->codeBase ;

   if ( c4 == 0 )  /* file closed */
      return error4( 0, -120, E90704 ) ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;
   if ( seqWrite->buffer == 0 )
      return 0 ;

   #ifdef E4ANALYZE
      if ( file4longError( seqWrite->pos ) < 0 )
         return error4( c4, e4result, E90705 ) ;

      // AS 10/16/00 write-delay does support large files now
      //    #ifdef S4FILE_EXTENDED
      //       || file4longGetHi( seqWrite->pos ) != 0 || seqWrite->file->isLong != 0
      //    #endif
      // )
   #endif

   toWrite = seqWrite->working - seqWrite->avail ;

   if ( toWrite != 0 )
   {
      #ifndef S4OFF_OPTIMIZE
         #ifdef E4ANALYZE_ALL
            if ( seqWrite->file->hasDup == 1 )
               if ( file4writePart( seqWrite->buffer, seqWrite->file, file4longGetLo( seqWrite->pos ), toWrite ) != 0 )
                  return error4( c4, e4opt, E80602 ) ;
         #endif

         if ( seqWrite->file->doBuffer == 1 && seqWrite->file->bufferWrites == 1 )
         {
            if ( file4writeInternal( seqWrite->file, seqWrite->pos, seqWrite->buffer, toWrite ) != 0 )
               return error4describe( seqWrite->file->codeBase, e4write, E90705, seqWrite->file->name, 0, 0 ) ;
         }
         else
      #endif
         {   /* needed for else just above */
            /* queue up the 1/2 written to buffer for a delayed write, and
               then set the current buffer pointer to point to the other
               half (if and when it is not waiting to be delay written) */
            if ( seqWrite->buffer == seqWrite->buf1 )
            {
               seqWrite->buf1avail = 0 ;
               /* LY 4/28/99 : removed file4longGetLo from second param */
               file4writeDelay( seqWrite->file, seqWrite->pos, seqWrite->buffer, toWrite, file4seqWriteCompletionRoutine, &seqWrite->buf1avail ) ;
               while( seqWrite->buf2avail != 1 )  /* wait for write on other buffer piece to finish */
                  Sleep( 0 ) ;
               seqWrite->buffer = seqWrite->buf2 ;
            }
            else
            {
               seqWrite->buf2avail = 0 ;
               /* LY 4/28/99 : removed file4longGetLo from second param */
               file4writeDelay( seqWrite->file, seqWrite->pos, seqWrite->buffer, toWrite, file4seqWriteCompletionRoutine, &seqWrite->buf2avail ) ;
               while( seqWrite->buf1avail != 1 )  /* wait for write on other buffer piece to finish */
                  Sleep( 0 ) ;
               seqWrite->buffer = seqWrite->buf1 ;
            }
         }   /* needed for else above */

      /* file4flush( seqWrite->file )   VAX fix (flush req'd on write) */
      file4longAdd( &seqWrite->pos, toWrite ) ;
   }

   seqWrite->avail = seqWrite->working = seqWrite->total ;
   return 0 ;
}

int S4FUNCTION file4seqWriteFlush( FILE4SEQ_WRITE *seqWrite )
{
   int rc ;

   if ( seqWrite->file == 0 )  /* nothing to flush, just return */
      return 0 ;

   rc = file4seqWriteDelay( seqWrite ) ;
   file4writeDelayFlush( seqWrite->file, 1 ) ;

   /* and now, absolutely ensure that any delayed writes get to finish their
      completion routine before continuing (i.e. avoid memory corruption) */
   if ( seqWrite->buf2 != 0 )
   {
      while( seqWrite->buf2avail != 1 )  /* wait for write on other buffer piece to finish */
         Sleep( 0 ) ;
   }
   if ( seqWrite->buf1 != 0 )
   {
      while( seqWrite->buf1avail != 1 )  /* wait for write on other buffer piece to finish */
         Sleep( 0 ) ;
   }

   /* ensure that any optimized reads are dumped in order that new delayed
      writes will get recognized properly */
   #ifndef S4OFF_OPTIMIZE
      opt4fileFlush( seqWrite->file, 1 ) ;
   #endif

   return rc ;
}
#else

int S4FUNCTION file4seqWriteFlush( FILE4SEQ_WRITE *seqWrite )
{
   CODE4 *c4 ;
   long urc ;
   unsigned toWrite ;

   #ifdef E4PARM_HIGH
      if ( seqWrite == 0 )
         return error4( 0, e4parm_null, E90705 ) ;
   #endif

   if ( seqWrite->file == 0 )  /* nothing to flush, just return */
      return 0 ;
   c4 = seqWrite->file->codeBase ;

   if ( c4 == 0 )  /* file closed */
      return error4( 0, -120, E90704 ) ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;
   if ( seqWrite->buffer == 0 )
      return 0 ;

   #ifdef E4ANALYZE
      if( file4longError( seqWrite->pos ) < 0 )
         return error4( c4, e4result, E90705 ) ;
   #endif

   toWrite = seqWrite->working - seqWrite->avail ;

   if ( toWrite > 0 )
   {
      #ifndef S4OFF_OPTIMIZE
         #ifdef E4ANALYZE_ALL
            if ( seqWrite->file->hasDup == 1 )
               if ( file4writePart( seqWrite->buffer, seqWrite->file, seqWrite->pos, toWrite ) != 0 )
                  return error4( c4, e4opt, E80602 ) ;
         #endif

         if ( seqWrite->file->doBuffer == 1 && seqWrite->file->bufferWrites == 1 )
         {
            if ( file4writeInternal( seqWrite->file, seqWrite->pos, seqWrite->buffer, toWrite ) != 0 )
               return error4describe( seqWrite->file->codeBase, e4write, E90705, seqWrite->file->name, 0, 0 ) ;
         }
         else
      #endif
         {   /* needed for else just above */
            urc = file4writeLow( seqWrite->file, seqWrite->pos, seqWrite->buffer, toWrite, 1, 1, 1 ) ;
            if ( urc != 0 )
               return (int)urc ;
         }   /* needed for else above */

      /* file4flush( seqWrite->file )   VAX fix (flush req'd on write) */
      file4longAdd( &seqWrite->pos, toWrite ) ;
   }

   seqWrite->avail = seqWrite->working = seqWrite->total ;
   return 0 ;
}
#endif /* S4WRITE_DELAY */

int S4FUNCTION file4seqWrite( FILE4SEQ_WRITE *seqWrite, const void *buffer, const unsigned ptrLen )
{
   int rc ;
   unsigned firstLen ;
   CODE4 *c4 ;

   #ifdef E4PARM_HIGH
      if ( seqWrite == 0 || ( ptrLen && buffer == 0 ) )
         return error4( 0, e4parm_null, E90704 ) ;
   #endif

   c4 = seqWrite->file->codeBase ;
   if ( c4 == 0 )  /* file closed */
      return error4( 0, -120, E90704 ) ;

   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( seqWrite->buffer == 0 )
   {
      rc = file4writeInternal( seqWrite->file, seqWrite->pos, buffer, ptrLen ) ;
      #ifndef S4OFF_OPTIMIZE
         #ifdef E4ANALYZE_ALL
            if ( seqWrite->file->hasDup == 1 )
               if ( file4writePart( buffer, seqWrite->file, file4longGetLo( seqWrite->pos ), ptrLen ) != 0 )
                  return error4( c4, e4opt, E80602 ) ;
         #endif
      #endif
      file4longAdd( &seqWrite->pos, ptrLen ) ;
      return rc ;
   }

   if ( seqWrite->avail == 0 )
   {
      #ifdef S4WRITE_DELAY
         if ( file4seqWriteDelay( seqWrite ) < 0 )
            return -1 ;
      #else
         if ( file4seqWriteFlush( seqWrite ) < 0 )
            return -1 ;
      #endif
   }

   if ( seqWrite->avail >= ptrLen )
   {
      #ifdef E4ANALYZE
         if ( seqWrite->working < seqWrite->avail || ( seqWrite->working - seqWrite->avail + ptrLen ) > seqWrite->total )
            return error4( c4, e4result, E90704 ) ;
      #endif
      c4memcpy( seqWrite->buffer + ( seqWrite->working - seqWrite->avail ), buffer, ptrLen ) ;
      seqWrite->avail -= ptrLen ;
      return 0 ;
   }
   else
   {
      firstLen = seqWrite->avail ;

      #ifdef E4ANALYZE
         if ( seqWrite->working < seqWrite->avail || seqWrite->working > seqWrite->total )
            return error4( c4, e4result, E90704 ) ;
      #endif
      c4memcpy( seqWrite->buffer + ( seqWrite->working - seqWrite->avail ), buffer, seqWrite->avail ) ;

      seqWrite->avail = 0 ;

      return file4seqWrite( seqWrite, (char *)buffer + firstLen, ptrLen - firstLen ) ;
   }
}

#ifndef S4INTERNAL_COMPILE_CHECK
int S4FUNCTION file4seqWriteInit( FILE4SEQ_WRITE *seqWrite, FILE4 *file, const long startOffset, void *ptr, const unsigned ptrLen )
{
   FILE4LONG pos ;

   #ifdef E4PARM_HIGH
      if ( seqWrite == 0 || file == 0 || startOffset < 0 ) /* CS 1999/10/08 Don't check ptr. It can be null. */
         return error4( 0, e4parm, E90706 ) ;
   #endif

   file4longAssign( pos, startOffset, 0 ) ;
   return file4seqWriteInitLow( seqWrite, file, pos, ptr, ptrLen ) ;
}
#endif

/* note that the sequential functions are not generally useful when hashing is in place since hashing effectively performs bufferring */
int file4seqWriteInitLow( FILE4SEQ_WRITE *seqWrite, FILE4 *file, FILE4LONG startOffset, void *ptr, const unsigned ptrLen )
{
   FILE4LONG compare ;
   c4memset( (void *)seqWrite, 0, sizeof( FILE4SEQ_WRITE ) ) ;

   #ifndef S4OFF_OPTIMIZE
      opt4fileFlush( file, 1 ) ;
   #endif

   seqWrite->file = file ;

   if ( ptr != 0 )
   {
      #ifdef S4WRITE_DELAY
         seqWrite->total = (ptrLen / 2) & 0x7E00 ;  /* Make it a multiple of 512 bytes */
         seqWrite->buf1 = (char *)ptr ;
         seqWrite->buf2 = (char *)ptr + seqWrite->total ;
         seqWrite->buf1avail = 1 ;
         seqWrite->buf2avail = 1 ;
         seqWrite->buffer = seqWrite->buf1 ;
      #else
         if ( (long)ptrLen > 65535 )   /* 32 bit o/s, can be > 64k */
            seqWrite->total = ptrLen & 0xFFFFFC00L ;  /* Make it a multiple of 1K */
         else
            seqWrite->total = ptrLen & 0xFC00 ;  /* Make it a multiple of 1K */
         seqWrite->buffer = (char *)ptr ;
      #endif
      if ( seqWrite->total == 0 )  /* buffer is too small, so set it to 0 */
         seqWrite->buffer = 0 ;
      else
      {
         file4longAssign( compare, ( file4longGetLo( startOffset ) % 0x400 ), file4longGetHi( startOffset ) ) ;
         if ( file4longLess( compare, seqWrite->total ) )
            seqWrite->avail = seqWrite->working = seqWrite->total - file4longGetLo( compare ) ;
      }
   }
   file4longAssign( seqWrite->pos, file4longGetLo( startOffset ), file4longGetHi( startOffset ) ) ;

   return 0 ;
}

int S4FUNCTION file4seqWriteRepeat( FILE4SEQ_WRITE *seqWrite, const long nRepeat, const char ch )
{
   char buf[512] ;
   long numRepeat ;
   int rc ;

   #ifdef E4PARM_HIGH
      if ( seqWrite == 0 || nRepeat < 0 )
         return error4( 0, e4parm, E90707 ) ;
   #endif

   numRepeat = nRepeat ;
   c4memset( (void *)buf, ch, sizeof(buf) ) ;

   while ( (size_t)numRepeat > sizeof(buf) )  // CS 2000/12/01
   {
      rc = file4seqWrite( seqWrite, buf, (unsigned)sizeof( buf ) ) ;
      if ( rc < 0 )
         return error4stack( seqWrite->file->codeBase, (short)rc, E90707 ) ;
      numRepeat -= sizeof(buf) ;
   }

   return file4seqWrite( seqWrite, buf, (unsigned)numRepeat ) ;
}

