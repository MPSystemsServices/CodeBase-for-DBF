/* f4file.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

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
   #if !defined( S4OFF_MULTI ) && !defined( __TURBOC__ )
      #include <sys\locking.h>
      #define S4LOCKING
   #endif
   #if defined( _MSC_VER ) && !defined( S4WINCE )
      #include <sys\types.h>
      #include <sys\locking.h>
   #endif
#endif

#if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE )
   #include "zlib.h"
#endif

#ifdef S4NO_FILELENGTH
   #if  defined( S4MACINTOSH )
      FILE4LONG u4filelength( int hand )
      {
         long fileLen ;

         if ( GetEOF(hand, &fileLen) != 0 )
            return error4( 0, e4result, E90603 ) ;

         return fileLen ;
      }
   #elif defined( S4WIN32 )



      FILE4LONG u4filelength( HANDLE hand )
      {
         FILE4LONG rc ;

         /* AS 07/07/99 Do a little more checking about long files to ensure a failure if
            the S4FILE_EXTENDED is not defined */

         unsigned long hiAddress ;

         unsigned long loAddress = (long)GetFileSize( hand, &hiAddress ) ;
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

      FILE4LONG u4filelength( int hand )
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



      FILE4LONG u4filelength( FileHand hand )
      {
         long fileLen,rc;
         rc = FileTell(hand,&fileLen,0);
         if (rc == -1)
            return error4( 0, e4result, E90603 ) ;

         return fileLen;
      }
   #elif defined( _MSC_VER )



      FILE4LONG u4filelength( int hand )
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

      FILE4LONG u4filelength( int hand )
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
   #ifdef E4ANALYZE
      if ( f4->hand == INVALID4HANDLE )
      {
         error4( f4->codeBase, e4parm, E90605 ) ;
         file4longAssignError( lrc ) ;
         return lrc ;
      }
   #endif

   #ifndef S4OFF_OPTIMIZE
      // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
      if ( file4getTemporary( f4 ) == 1 && f4->fileCreated == 0 )
      {
         /* 04/24/96 AS fix for c/s t4commit.c */
         if ( file4longError( f4->len ) == (unsigned long)-1L )
         {
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
         #if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
            if ( f4->compressInfo != 0 ) // file is compressed, return the length as indicated in the compressed header
            {
               file4longAssign( lrc, f4->compressInfo->fileLen, 0L ) ;
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
            if ( file4longGetHi( f4->len ) != 0 )  // no support for bufferring large files...
               error4( f4->codeBase, e4result, E90606 ) ;
         }
      }
   #endif

   return lrc ;
}



unsigned long S4FUNCTION file4lenLow2( FILE4 S4PTR *f4 )
{
   return file4longGetLo( file4lenLow( f4 ) ) ;
}



#ifdef S4FILE_EXTENDED
   int S4FUNCTION file4lenSet( FILE4 *f4, long newLen )
   {
      FILE4LONG lenSet ;

      file4longAssign( lenSet, newLen, 0L ) ;

      return file4lenSetLow( f4, lenSet ) ;
   }



   int file4lenSetLow( FILE4 *f4, FILE4LONG newLen )
#else
   int S4FUNCTION file4lenSetLow( FILE4 *f4, FILE4LONG newLen )
#endif
   {
      int rc, isLong = 0 ;
      CODE4 *c4 ;
      #ifdef S4WRITE_DELAY
         FILE4WRITE_DELAY *writeDelay ;
         LINK4 *delayLink ;
      #endif
      #ifdef __SC__
         union REGS dosFlush ;
      #endif
      unsigned long newLenShort ;
      #ifdef S4WRITE_DELAY
         #ifdef S4WIN32
            FILE4LONG tLong ;
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( f4 == 0 || file4longError( newLen ) == (unsigned long)-1L )
            return error4( 0, e4parm_null, E90606 ) ;
      #endif

      c4 = f4->codeBase ;

      #ifdef E4ANALYZE
         if ( f4->hand == INVALID4HANDLE || f4->codeBase == 0 )
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

      newLenShort = file4longGetLo( newLen ) ;

      #ifndef S4OFF_OPTIMIZE
         if ( f4->doBuffer )
         {
            #ifdef S4FILE_EXTENDED
               if ( f4->isLong != 0 )  /* it means file is extenced past 4 gig mark, stop optimizing it */
               {
                  assert5( file4longGetHi( newLen ) >= 1 ) ; /* no error value */
                  file4optimize( f4, OPT4OFF, 0 ) ;
                  if ( f4->doBuffer != 0 )
                     return error4( 0, e4result, E90606 ) ;
               }
            #endif
         }
         if ( f4->doBuffer )
         {
            /* if len is -1 but bufferWrites true, may still have buffered data
               which must be removed, so get disk file length for reference */
            if ( f4->bufferWrites == 1 && f4->fileCreated != 0 )
            {
               f4->len = file4lenLow( f4 ) ;
               if ( file4longGetHi( f4->len ) != 0 )
                  error4( 0, e4result, E90606 ) ;
            }
            // AS Nov 28/02 - there was a problem here, namely that we were extending
            // the internal file length, but the optimization buffer still had listed
            // its old length (which might be not on block boundary).  In that case, if
            // we later went to read that 'in memory' block we incorrectly only had
            // a partial read performed.  Therefore, we need to modify this existing block to
            // have the full length (whatever random data may be in memory is fine as the file
            // itself is just being extended without a write...
            if ( file4longGetLo( f4->len ) > newLenShort )   /* must do a partial delete of memory */
               opt4fileDelete( f4, newLenShort, file4longGetLo( f4->len ) ) ;
            else
               opt4fileExtend( f4, newLenShort, file4longGetLo( f4->len ) ) ;
            if ( f4->bufferWrites )
               f4->len = newLen ;

            #ifdef E4ANALYZE
               else
                  if ( file4longError( f4->len ) != (unsigned long)-1L )
                     return error4( 0, e4result, E90606 ) ;
            #endif
         }

         #ifdef E4ANALYZE_ALL
            if ( f4->hasDup == 1 )
               if ( f4->doBuffer == 1 || f4->link.n == 0 )
                  if ( file4partLenSet( f4, newLenShort ) < 0 )
                     return error4( c4, e4opt, E80602 ) ;
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
            #if   defined( S4MACINTOSH )
               rc = SetEOF( f4->hand, newLen ) ;
            #elif defined( S4WIN32 )
               // AS Dec 11/02 - Renamed for clarification
               #ifdef S4DELAY_WRITE_MT
                  EnterCriticalSection( &f4->critical4file ) ;
               #endif
               #ifdef S4WRITE_DELAY
                  if ( l4numNodes( &f4->delayWriteFileList ) != 0 )
                  {
                     // AS 04/25/00 --> critical sectioning missing...
                     EnterCriticalSection( &c4->critical4delayWriteList ) ;
                     for ( delayLink = (LINK4 *)l4first( &f4->delayWriteFileList ) ;; )
                     {
                        if ( delayLink == 0 )
                           break ;
                        writeDelay = (FILE4WRITE_DELAY *)(delayLink - 1 ) ;
                        delayLink = (LINK4 *)l4next( &f4->delayWriteFileList, delayLink ) ;
                        /* now, if the delay-write is without the boundaries of
                           the len-set, then remove that part */

                        /* LY 4/28/99 : replaced binary operators with file4long*** */
                        file4longAssignLong( tLong, writeDelay->pos ) ;
                        file4longAdd( &tLong, writeDelay->len ) ;
                        if ( file4longGreater( tLong, newLenShort ) )
                        {
                           /* maybe is being written to disk now, in which case
                              must wait */
                           while ( writeDelay->usageFlag == r4inUse )  /* is being written to disk, just wait until it is done... */
                              Sleep( 0 ) ;
                           if ( writeDelay->usageFlag == r4finished ) /* is written to disk, so can just ignore */
                              continue ;
                           if ( file4longGreater( writeDelay->pos, newLenShort ) )   /* just remove */
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
                              file4longAssign( tLong, newLenShort, 0 ) ;
                              file4longSubtractLong( &tLong, &writeDelay->pos ) ;
                              writeDelay->len = file4longGetLo( tLong ) ;
                           }
                        }
                     }
                     // AS 04/25/00 --> critical sectioning missing...
                     LeaveCriticalSection( &c4->critical4delayWriteList ) ;
                  }
               #endif
               if ( SetFilePointer( (HANDLE)f4->hand, file4longGetLo( newLen ), file4longGetHiAddress( newLen ), FILE_BEGIN ) == (DWORD)-1 )
               {
                  // AS Dec 11/02 - Renamed for clarification
                  #ifdef S4DELAY_WRITE_MT
                     LeaveCriticalSection( &f4->critical4file ) ;
                  #endif
                  return error4describe( c4, e4lenSet, E90606, f4->name, 0, 0 ) ;
               }
               if ( SetEndOfFile( (HANDLE)f4->hand ) )
                  rc = 0 ;
               else
                  rc = -1 ;
               // AS Dec 11/02 - Renamed for clarification
               #ifdef S4DELAY_WRITE_MT
                  LeaveCriticalSection( &f4->critical4file ) ;
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

   #if defined( S4WIN32 )
      file4longSetLo( pos, SetFilePointer( (HANDLE)f4->hand, file4longGetLo( pos ), file4longGetHiAddress( pos ), FILE_BEGIN ) ) ;
      file4longCheckError( pos ) ;
   #elif defined( S4PALM )
      rc = FileSeek( f4->hand, pos, fileOriginBeginning ) ;
      if (rc != 0)
         rc = (unsigned long)-1 ;
   #else
      #if   defined( S4MACINTOSH )
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

   #if   defined( S4WIN32 )
      ReadFile( (HANDLE)f4->hand, ptr, len, &urc, 0 ) ;
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



static unsigned file4readOnBoundary( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
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
            urc = min ( urc, (readLen - lenMod ) ) ;  // adjust output length to not include extra read for decryption only
            memcpy( ptr, (char *)c4->filePreprocessBuffer + posMod, urc ) ;
         }
      }
   #endif
   return urc ;
}



static unsigned file4readDecompress( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
{
   // considers compression/decompression on file if required
   #if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      // need to consider compression
      if ( f4->compressInfo == 0 ) // file is not compressed
         return file4readOnBoundary( f4, pos, ptr, len ) ;

      assert5( file4longGetHi( pos ) == 0 ) ;  // not supported for large files

      unsigned long readPos = file4longGetLo( pos ) ;

      if ( readPos < f4->compressHeaderOffset )  // requesting to read in uncompressed header area
      {
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
         unsigned compressedRead = file4readDecompress( f4, pos, (char *)ptr+physicalRead, leftToRead ) ;
         return physicalRead + compressedRead ;
      }

      // need to calculate read and perform decompressions...
      long lenLeft = len ;
      long blockOn = ( readPos - f4->compressHeaderOffset ) / f4->actualBlockSize ;  // where do we really start reading
      unsigned long blockOffset = ( readPos - f4->compressHeaderOffset ) % f4->actualBlockSize ;

      int rc = 0 ;

      long ptrOffset = 0 ;
      Bool5 doneRead = 0 ;  // AS Jan 16/03 - when done reading last block

      while ( lenLeft > 0 )
      {
         unsigned long outLen  ;

         // check the existing buffer first, we may be able to use it...
         if ( f4->uBufFilePos == f4->compressInfo->offsets[blockOn] )
         {
            outLen = f4->uBufFileLen ;
         }
         else
         {
            FILE4LONG toRead ;
            file4longAssign( toRead, f4->compressInfo->offsets[blockOn], 0 ) ;

            long lenToRead ;
            // if last block, special handling
            if ( blockOn + 1 == f4->compressInfo->numOffsets )
               lenToRead = f4->physicalFileLen - f4->compressInfo->offsets[blockOn] ;  // use the filelength as the basis of the end position
            else
               lenToRead = f4->compressInfo->offsets[blockOn+1] - f4->compressInfo->offsets[blockOn] ;

            if ( lenToRead == 0 )  // nothing to read - done
               return ptrOffset ;  // AS Jan 16/03 - return amount read which might not be zero

            assert5( lenToRead > 0 ) ;

            // read the next block
            rc = file4readOnBoundary( f4, toRead, f4->compressBuffer, lenToRead ) ;
            if ( rc != lenToRead )
               break ;

            outLen = f4->actualBlockSize ;
            // verify length
            // assert5( lenToRead == *((long *)f4->compressBuffer) )
            // rc = c4uncompress( f4->codeBase, f4->uncompressBuffer, &outLen, f4->compressBuffer + sizeof( long ), lenToRead ) ;  // offset uncompress by 4 due to length value
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
            unsigned long maxLen = f4->compressInfo->fileLen -  readPos ;
            if ( outLen > maxLen )
            {
               outLen = maxLen ;
               doneRead = 1 ;
            }

            // store the current position and length of buffer to allow for re-use without uncompression
            f4->uBufFilePos = f4->compressInfo->offsets[blockOn] ;
            f4->uBufFileLen = outLen ;
         }

         // now copy the data into the read buffer
         assert5( outLen >= blockOffset ) ;
         unsigned long lenToCopy = min( (unsigned long)lenLeft, outLen - blockOffset ) ;
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
   #else
      return file4readOnBoundary( f4, pos, ptr, len ) ;  /* LY 2002/12/27 : fixed typo */
   #endif
}



static unsigned file4readLowDo( FILE4 *f4, FILE4LONG pos, void *ptr, unsigned len )
{
   // ensure all reading is done within a critical section
   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      EnterCriticalSection( &f4->critical4file ) ;
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
      LeaveCriticalSection( &f4->critical4file ) ;
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
      #ifdef S4FILE_EXTENDED
         if ( f4->isLong == 0 )  /* advance reading not done on long files */
      #endif
      if ( f4->advanceReadBufStatus != AR4EMPTY )  /* if it is set put still in area, then wait for the advance-read to finish */
      {
         // AS 02/01/00 -- some info gets set when advance status is AR4SET, so wait until it is done before going on...
         while( f4->advanceReadBufStatus == AR4SET )
            Sleep( 0 ) ;

         if ( f4->advanceReadBufStatus == AR4FULL ) /* successful read */
         {
            unsigned posShort ;
            int noAdvance = 0 ;

            if ( file4longGetHi( pos ) != 0 )  /* we only advance read for files < 4 gigs */
               noAdvance = 1 ;

            posShort = file4longGetLo( pos ) ;

            if ( posShort > ULONG_MAX - lenToRead )  /* means we will exceed - i.e. large file */
               noAdvance = 1 ;

            if ( noAdvance == 0 )
            {
               if ( ( posShort >= f4->advanceReadBufPos ) && ( file4longGetLo( pos ) + lenToRead ) <= ( f4->advanceReadBufPos + f4->advanceReadBufLen ) )
               {
                  memcpy( (char *)ptr, f4->advanceReadBuf + posShort - f4->advanceReadBufPos, lenToRead ) ;
                  return lenToRead ;
               }
            }
         }
      }
   #endif

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      EnterCriticalSection( &f4->critical4file ) ;
   #endif

   #ifdef S4WRITE_DELAY
      /* make sure that the data to read isn't in memory */
      int noDelay = 0 ;
      if ( file4longGetHi( pos ) != 0 )  /* we only advance read for files < 4 gigs */
         noDelay = 1 ;
      unsigned posShort = file4longGetLo( pos ) ;
      if ( posShort > ULONG_MAX - lenToRead )  /* means we will exceed - i.e. large file */
         noDelay = 1 ;

      if ( noDelay == 0 )
      {
         urc = 0 ;
         if ( l4numNodes( &f4->delayWriteFileList ) != 0 )  /* check for pieces already in memory */
         {
            // AS 05/06/99 --> critical sectioning missing...
            CODE4 *c4 = f4->codeBase ;
            EnterCriticalSection( &c4->critical4delayWriteList ) ;
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
               if ( file4longLessEq( tLong, posShort ) )  /* outside of block */
                  continue ;

               file4longAssign( tLong, posShort + lenToRead, 0 ) ;
               if ( file4longLessEqLong( tLong, writeDelay->pos ) )  /* outside of block */
                  continue ;

               file4longAssignLong( tLong, writeDelay->pos ) ;
               file4longSubtract( &tLong, posShort ) ;
               long beforeLen = file4longGetLo( tLong ) ;

               if ( beforeLen < 0 )
                  beforeLen = 0 ;

               unsigned copyLen, copyPos ;
               if ( beforeLen == 0 )
               {
                  /* LY 4/28/99 : replaced binary operators with file4long*** */
                  file4longAssign( tLong, posShort, 0 ) ;
                  file4longSubtractLong( &tLong, &writeDelay->pos ) ;
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
               file4longAssign( afterPos, posShort + beforeLen + copyLen, 0 ) ;

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
               file4longAssign( tLong, posShort + lenToRead - writeDelay->len, 0 ) ;
               file4longSubtractLong( &tLong, &writeDelay->pos ) ;
               long afterLen = file4longGetLo( tLong ) ;
               // assert5( afterLen >= 0 ) ;
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
               LeaveCriticalSection( &c4->critical4delayWriteList ) ;
               LeaveCriticalSection( &f4->critical4file ) ;
               assert5( urc <= lenToRead ) ;   // AS 02/01/00 -- cannot have read more than was requested!
               return urc ;
            }
            // AS 05/06/99 --> critical sectioning missing...
            LeaveCriticalSection( &c4->critical4delayWriteList ) ;
         }
      }
   #endif

   urc = file4readLowDo( f4, pos, ptr, lenToRead ) ;

   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4DELAY_WRITE_MT
      LeaveCriticalSection( &f4->critical4file ) ;
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
      #ifdef S4FILE_EXTENDED
         if ( f4->isLong == 0 )
      #endif
         if ( f4->hasHadPendingAdvanceRead )  /* for efficiency, avoid for non-advance-read files */
            file4advanceReadWriteOver( f4, file4longGetLo( pos ), lenToRead, ptr, 0 ) ;
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
      if ( f4->hand == INVALID4HANDLE )
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
         if ( ( file4longGetHi( pos ) != 0 ) ||
            ( file4longGetLo( pos )+len > LONG_MAX ) )  /* means out of range, nothing read */
            return file4readLow( f4, pos, ptr, len ) ;
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

         urc = (unsigned)opt4fileRead( f4, file4longGetLo( pos ), ptr, len )  ;
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
         if ( file4longGetHi( pos ) != 0 )  /* means out of range, nothing read */
            return file4readError( f4, pos, len, "file4readAll" ) ;
         urc = opt4fileRead( f4, file4longGetLo( pos ), ptr, len )  ;
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
      c4strncpy( fromName, from->name, sizeof( fromName ) ) ;
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
               j = (int)value ;
               tempDbl = (double) j ;
               *vPtr = j + '0' ;
               /* LY 2002/08/09 : fix(?) for 100000000000.0001 */
               if ( value - j >= 1.0e-14 )
                  value = ( value - j + 1.0e-15) * 10.0 ;
               else
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

int S4FUNCTION file4advanceRead( FILE4 *f4, unsigned pos, void *data, const unsigned len, S4ADVANCE_FUNCTION *completionRoutine, void *completionData )
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
   advanceRead->len = len ;
   advanceRead->pos = pos ;
   advanceRead->usageFlag = r4queued ;
   advanceRead->completionRoutine = completionRoutine ;
   advanceRead->completionData = completionData ;

   EnterCriticalSection( &c4->critical4advanceReadList ) ;

   l4add( &c4->advanceReadList, advanceRead ) ;
   l4add( &f4->advanceReadFileList, &advanceRead->fileLink ) ;

   LeaveCriticalSection( &c4->critical4advanceReadList ) ;

   SetEvent( c4->pendingReadEvent ) ;  /* notify the write thread */
   Sleep( 0 ) ;

   return 0 ;
}



int file4advanceCancel( FILE4 *f4 )
{
   /* cancels all advance-reads for the given file */
   FILE4ADVANCE_READ *advanceRead ;
   LINK4 *advanceReadLink, *saved ;
   CODE4 *c4 ;

   c4 = f4->codeBase ;

   EnterCriticalSection( &c4->critical4advanceReadList ) ;

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

   LeaveCriticalSection( &c4->critical4advanceReadList ) ;

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
         EnterCriticalSection( &c4->critical4advanceReadList ) ;

         advanceRead = (FILE4ADVANCE_READ *)l4first( &c4->advanceReadList ) ;
         if ( advanceRead == 0 )   /* maybe got removed by main thread, so none to read... */
         {
            LeaveCriticalSection( &c4->critical4advanceReadList ) ;
            Sleep( 0 ) ;
            continue ;
         }
         advanceRead->usageFlag = r4inUse ;
         LeaveCriticalSection( &c4->critical4advanceReadList ) ;

         file4longAssign( tLong, advanceRead->pos, 0 ) ;
         advanceRead->status = file4readLowDo( advanceRead->file, tLong, advanceRead->data, advanceRead->len ) ;
         advanceRead->usageFlag = r4finished ;  /* outside of critical section, to allow a wait for completion while keeping the critical section */
         EnterCriticalSection( &c4->critical4advanceReadList ) ;
         l4remove( &c4->advanceReadList, advanceRead ) ;
         l4remove( &advanceRead->file->advanceReadFileList, &advanceRead->fileLink ) ;

         /* the completion routine may get reset by another routine which
            needed to call it */
         if ( advanceRead->completionRoutine != 0 )
            advanceRead->completionRoutine( advanceRead ) ;

         /* for reading, the critical section includes the completion routine
            because it checks the status flag before modifying it */
         LeaveCriticalSection( &c4->critical4advanceReadList ) ;
         mem4free( c4->advanceReadMemory, advanceRead ) ;
      }
   }
}



void file4advanceReadWriteOver( FILE4 *f4, unsigned long pos, const unsigned len, const void *data, const int doCancel )
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

   if ( pos > ULONG_MAX - len )  /* means we will exceed - i.e. large file */
      return ;

   /* try to avoid entering/leaving critical section if possible, because
      it is a costly operation.  It is safe to analyze the lists because
      the only adder to the lists is ourselves, and they get removed only
      after the operations are completed */

   /* first take care of the outstanding advance-reads */
   if ( l4numNodes( &f4->advanceReadFileList ) != 0 )
   {
      /* first lock out operations on the advance-read list */
      enteredCriticalSection = 1 ;
      EnterCriticalSection( &c4->critical4advanceReadList ) ;
      for ( advanceLink = (LINK4 *)l4first( &f4->advanceReadFileList ) ;; )
      {
         if ( advanceLink == 0 )
            break ;
         advanceRead = (FILE4ADVANCE_READ *)(advanceLink - 1 ) ;
         advanceLink = (LINK4 *)l4next( &f4->advanceReadFileList, advanceLink ) ;

         if ( ( advanceRead->pos + advanceRead->len ) <= pos ) /* outside of block */
            continue ;
         if ( ( pos + len ) <= advanceRead->pos )  /* outside of block */
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
         if ( ( advanceRead->pos >= pos ) && ( pos + len >= advanceRead->pos + advanceRead->len ) ) /* copy it */
         {
            advanceRead->status = 0 ;
            memcpy( advanceRead->data, (const char *)data + (advanceRead->pos - pos ), advanceRead->len ) ;
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
         EnterCriticalSection( &c4->critical4advanceReadList ) ;
      }
      if ( pos < ( f4->advanceReadBufPos + (long)f4->advanceReadBufLen ) )  /* outside of block */
         if ( ( pos + (long)len ) > f4->advanceReadBufPos )  /* outside of block */
         {
            if ( ( f4->advanceReadBufPos >= pos ) && ( pos + len >= f4->advanceReadBufPos + f4->advanceReadBufLen ) ) /* copy it */
               memcpy( f4->advanceReadBuf, (const char *)data + (f4->advanceReadBufPos - pos ), f4->advanceReadBufLen ) ;
            else
               if ( doCancel )
                  f4->advanceReadBufStatus = AR4EMPTY ;
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
            EnterCriticalSection( &c4->critical4advanceReadList ) ;
         }
         if ( pos < ( opt->advanceLargePos + opt->advanceLargeLen ) )  /* outside of block */
            if ( ( pos + len ) > opt->advanceLargePos )  /* outside of block */
            {
               if ( ( opt->advanceLargePos >= pos ) && ( pos + len >= opt->advanceLargePos + opt->advanceLargeLen ) ) /* copy it */
                  memcpy( opt->advanceLargeBuffer, (const char *)data + (opt->advanceLargePos - pos ), opt->advanceLargeLen ) ;
               else
                  if ( doCancel )
                     opt->advanceLargeBufferAvail = AR4EMPTY ;
            }
      }
   #endif

   if ( enteredCriticalSection )
      LeaveCriticalSection( &c4->critical4advanceReadList ) ;
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



void opt4advanceReadBuf( FILE4 *f4, unsigned long pos, unsigned len )
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
      long hashVal, adjustedPos ;
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

      extraRead = (unsigned) ((unsigned long)((unsigned long)pos << opt->numShift ) >> opt->numShift ) ;
      adjustedPos = pos - extraRead ;
      hashVal = opt4fileHash( opt, f4, (unsigned long)adjustedPos ) ;
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
         if ( f4->advanceReadBufPos == pos )  /* already advanced on this read */
            return ;
   }

   #ifndef S4OFF_OPTIMIZE
      if ( f4->advanceReadBufStatus == AR4FULL )  /* finished read, so place into optimization */
      {
         /* first ensure that it is not already bufferred */
         extraRead = (unsigned) ((unsigned long)((unsigned long)f4->advanceReadBufPos << opt->numShift ) >> opt->numShift ) ;
         adjustedPos = f4->advanceReadBufPos - extraRead ;
         hashVal = opt4fileHash( opt, f4, (unsigned long)adjustedPos ) ;
         if ( opt4fileReturnBlock( f4, adjustedPos, hashVal ) == 0 )
         {
            /* the opt4fileWrite function can be used to place data into memory
               without marking as changed based on last paramater... */
            opt4fileWrite( f4, f4->advanceReadBufPos, f4->advanceReadBufLen, f4->advanceReadBuf, 0 ) ;
         }
         f4->advanceReadBufStatus = AR4EMPTY ;
      }
   #endif

   EnterCriticalSection( &c4->critical4advanceReadList ) ;
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
               LeaveCriticalSection( &c4->critical4advanceReadList ) ;
               while ( f4->advanceReadBufStatus == AR4SET )
                  Sleep( 0 ) ;
               EnterCriticalSection( &c4->critical4advanceReadList ) ;
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
      extraRead = (unsigned) ((unsigned long)((unsigned long)pos << opt->numShift ) >> opt->numShift ) ;
      adjustedPos = pos - extraRead ;
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
            LeaveCriticalSection( &c4->critical4advanceReadList ) ;
            return ;
         }
         f4->advanceReadBufLen = len + extraRead ;
      }

      unsigned adjustedLen = f4->advanceReadBufLen ;

      LeaveCriticalSection( &c4->critical4advanceReadList ) ;

      file4advanceRead( f4, adjustedPos, f4->advanceReadBuf, adjustedLen, file4advanceReadBufCompletionRoutine, &f4->advanceReadBufStatus ) ;
   #else
      f4->advanceReadBufStatus = AR4SET ;
      f4->advanceReadBufPos = pos ;

      LeaveCriticalSection( &c4->critical4advanceReadList ) ;

      file4advanceRead( f4, pos, f4->advanceReadBuf, len, file4advanceReadBufCompletionRoutine, &f4->advanceReadBufStatus ) ;
   #endif
}
#endif /* S4READ_ADVANCE */


// AS Nov 26/02 - Support for data file compression
#if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   int file4compressInit( FILE4 *file, COMPRESS4DATA *compress, long compressHeaderOffset )
   {
      // must set physical file len before assigning compress info which supresses the real file length
      file->physicalFileLen = file4len( file ) ;  // the actual file length (compressed)
      file->compressInfo = compress ;
      file->compressHeaderOffset = compressHeaderOffset ;   // if we read before this offset, use normal file i/o, else use compressed
      long sizeInfo = sizeof( COMPRESS4DATA ) + (compress->numOffsets - 1) * sizeof( long ) ;  // -1 because sizeof() includes 1 entry
      file->compressDataOffset = compressHeaderOffset + sizeInfo ;
      file->actualBlockSize = compress->blockSize * 1024 ;
      file->uncompressBuffer = (char *)u4allocFree( file->codeBase, file->actualBlockSize ) ;
      if ( file->uncompressBuffer == 0 )
         return e4memory ;
      file->compressBuffer = (char *)u4allocFree( file->codeBase, (long)(file->actualBlockSize * 1.01 ) + 12 ) ;
      if ( file->compressBuffer == 0 )
         return e4memory ;
      file->uBufFilePos = -1 ;
      file->uBufFileLen = -1 ;
      return 0 ;
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
            u4free( file->compressInfo ) ;
         file->compressInfo = 0 ;
      }
   }
#endif
