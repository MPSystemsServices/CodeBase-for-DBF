/* f4opt.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */
/* file-level optimizations */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OPTIMIZE_OFF
#ifdef E4ANALYZE_ALL
#ifndef S4UNIX
   #include <sys\stat.h>
   #include <share.h>
#endif
#include <fcntl.h>

int file4partLenSet( FILE4 *file, unsigned long newLen )
{
   #ifdef S4WINTEL
      HANDLE h1 ;
   #else
      int h1 ;
   #endif
   int rc ;
   #ifdef S4NO_CHSIZE
      int saveHand ;
   #endif
   #ifdef S4WIN32
      DWORD res ;
   #endif

   rc = 0 ;
   #ifdef S4WIN32
      h1 = CreateFile( file->dupName, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 ) ;
   #else
      h1 = sopen( file->dupName, (int)(O_BINARY | O_RDWR), SH_DENYRW, 0 ) ;
   #endif
   if ( h1 != INVALID4HANDLE )
   {
      #ifdef S4WIN32
         if ( SetFilePointer( h1, newLen, NULL, FILE_BEGIN ) == (DWORD)-1 )
         {
            CloseHandle( h1 ) ;
            return error4describe( file->codeBase, e4lenSet, E90606, file->dupName, 0, 0 ) ;
         }
         if ( SetEndOfFile( h1 ) )
            rc = 0 ;
         else
         {
            res = GetLastError() ;
            rc = -1 ;
         }
      #else
         #ifdef __SC__
            rc = 0 ;
            dosFlush.x.ax = 0x4200;
            dosFlush.x.bx = h1 ;
            memcpy((void *)&dosFlush.x.dx,(void *)&newLen,2);
            memcpy((void *)&dosFlush.x.cx,((char *)&newLen)+2,2);
            intdos( &dosFlush, &dosFlush ) ;
            if ( dosFlush.x.cflag != 0 )
            {
               close( h1 ) ;
               return error4( file->codeBase, e4lenSet, E90606 ) ;
            }
            dosFlush.h.ah = 0x40;
            dosFlush.x.bx = h1 ;
            dosFlush.x.cx = 0x00;
            intdos( &dosFlush, &dosFlush ) ;
            if ( dosFlush.x.cflag != 0 )
            {
               close( h1 ) ;
               return error4( file->codeBase, e4lenSet, E90606 ) ;
            }
            rc = 0
         #else
            #ifdef S4NO_CHSIZE
               saveHand = file->hand ;
               file->hand = h1 ;
               rc = file4changeSize( file, newLen ) ;
               file->hand = saveHand ;
            #else
               rc = chsize( h1, newLen ) ;
            #endif
         #endif
      #endif
   }

   if ( h1 > 0 )
   {
      #ifdef S4WIN32
         CloseHandle( h1 ) ;
      #else
         close( h1 ) ;
      #endif
   }

   return rc ;
}



int file4writePart( const void *buf, FILE4 *file, unsigned long pos, unsigned len )
{
   #ifdef S4WINTEL
      HANDLE h1 = INVALID4HANDLE;
   #else
      int h1 = INVALID4HANDLE;
   #endif
   unsigned rc = -1;
   long bufWriteLen ;
   unsigned long fileLen, rc1 ;
   char emptyBuf[512] ;

   if ( file->inUse == 1 )
      return 0 ;
   file->inUse = 1 ;

   for ( ;; )
   {
      #ifdef S4WIN32
         h1 = CreateFile( file->dupName, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 ) ;
         /* if ( h1 == INVALID4HANDLE ) */
         /*   h1 = -1 ;  */
      #else
         h1 = sopen( file->dupName, (int)(O_BINARY | O_RDWR), SH_DENYRW, 0 ) ;
      #endif
      if ( h1 == INVALID4HANDLE )
         break ;

      fileLen = file4longGetLo( u4filelength( h1 ) ) ;
      while( fileLen < pos )
      {
         memset( emptyBuf, 0, sizeof( emptyBuf ) ) ;
         bufWriteLen = pos - fileLen ;
         if ( bufWriteLen > (long)sizeof( emptyBuf ) )
            bufWriteLen = (long)sizeof( emptyBuf ) ;
         #ifdef S4WIN32
            rc1 = SetFilePointer( h1, fileLen, NULL, FILE_BEGIN ) ;
            if ( rc1 != (DWORD)-1 )
               rc1 = fileLen ;
         #else
            rc1 = (long)lseek( h1, (off_t)fileLen, 0 ) ;
         #endif
         if ( rc1 != fileLen )
            break ;
         #ifdef S4WIN32
            WriteFile( h1, emptyBuf, (int)bufWriteLen, (unsigned long *)&rc1, NULL ) ;
         #else
            rc1 = (long)write( h1, emptyBuf, (int)bufWriteLen ) ;
         #endif
         if ( rc1 != (unsigned long)len )
            break;
         fileLen = file4longGetLo( u4filelength( h1 ) ) ;
      }
      #ifdef S4WIN32
         rc1 = SetFilePointer( h1, pos, NULL, FILE_BEGIN ) ;
         if ( rc1 != (DWORD)-1 )
            rc1 = pos ;
      #else
         rc1 = (long)lseek( h1, (off_t)pos, 0 ) ;
      #endif
      if ( rc1 != pos )
         break ;

      #ifdef S4WIN32
         WriteFile( h1, buf, (int)len, (unsigned long *)&rc1, NULL ) ;
      #else
         rc1 = (long)write( h1, buf, len ) ;
      #endif
      if ( rc1 != (unsigned long)len )
         break;

      rc = 0 ;
      break;
   }

   if ( h1 != INVALID4HANDLE )
   {
      #ifdef S4WIN32
         CloseHandle( h1 ) ;
      #else
         close( h1 ) ;
      #endif
   }

   file->inUse = 0 ;
   return rc ;
}

int file4cmpPart( CODE4 *c4, void *bufIn, FILE4 *file, unsigned long pos, unsigned len )
{
   #ifdef S4WINTEL
      HANDLE h1 = INVALID4HANDLE ;
   #else
      int h1 = INVALID4HANDLE ;
   #endif
   unsigned long rc = -1, rc1 ;
   char buf1[512] ;
   long lenHold ;
   char *buf = (char *)bufIn ;

   if ( file->inUse == 1 )
      return 0 ;
   file->inUse = 1 ;

   lenHold = len ;

   if ( error4code( c4 ) != 0 )
      return -1 ;

   #ifdef S4WIN32
      h1 = CreateFile( file->dupName, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 ) ;
      /* if ( h1 == (int)INVALID_HANDLE_VALUE ) */
         /* h1 = -1 ; */
   #else
      h1 = sopen( file->dupName, (int)(O_BINARY | O_RDONLY), SH_DENYRW, 0 ) ;
   #endif

   if ( h1 > 0 )
      for( ;; )
      {
         #ifdef S4WIN32
            rc1 = SetFilePointer( h1, pos, NULL, FILE_BEGIN ) ;
            if ( rc1 != (DWORD)-1 )
               rc1 = pos ;
         #else
            rc1 = (long)lseek( h1, (off_t)pos, 0 ) ;
         #endif
         if ( rc1 != pos )
            break ;

         #ifdef S4WIN32
            ReadFile( h1, buf1, sizeof( buf1 ) > len ? len : sizeof( buf1 ), (unsigned long *)&rc1, NULL ) ;
         #else
            rc1 = (unsigned)read( h1, buf1, sizeof( buf1 ) > len ? len : sizeof( buf1 ) ) ;
         #endif
         if ( rc1 != (unsigned long)sizeof( buf1 ) && rc1 != (unsigned long)len )
            break ;

         if ( c4memcmp( buf + ( lenHold - len ), buf1, (unsigned)rc1 ) != 0 )
            break ;

         len -= (unsigned)rc1 ;
         if ( len == 0 )
         {
            rc = 0 ;
            break ;
         }

         pos += rc1 ;
      }

   if ( h1 != INVALID4HANDLE )
   {
      #ifdef S4WIN32
         CloseHandle( h1 ) ;
      #else
         close( h1 ) ;
      #endif
   }

   file->inUse = 0 ;
   if ( rc < 0 )
      return rc ;
   return 0 ;
}

int file4cmp( FILE4 *f1 )
{
   #ifdef S4WINTEL
      HANDLE h1 = INVALID4HANDLE ;
   #else
      int h1 = INVALID4HANDLE ;
   #endif
   int rc = -1 ;
   unsigned rc2 ;
   char buf1[512], buf2[512] ;
   long rct, p1, rc1 ;
   FILE4LONG tempLong ;

   if ( f1->inUse == 1 )
      return 0 ;
   f1->inUse = 1 ;

   if ( error4code( f1->codeBase ) != 0 || sizeof( buf1 ) != sizeof( buf2 ) )
      return -1 ;

   p1 = 0L ;

   #ifdef S4WIN32
      h1 = CreateFile( f1->dupName, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 ) ;
      /* if ( h1 == (int)INVALID_HANDLE_VALUE ) */
         /* h1 = -1 ; */
   #else
      h1 = sopen( f1->dupName, (int)(O_BINARY | O_RDONLY), SH_DENYRW, 0 ) ;
   #endif

   if ( h1 != INVALID4HANDLE )
      for( ;; )
      {
         #ifdef S4WIN32
            rct = SetFilePointer( h1, p1, NULL, FILE_BEGIN ) ;
            if ( rct != (DWORD)-1 )
               rct = p1 ;
         #else
            rct = (long)lseek( h1, (off_t)p1, 0 ) ;
         #endif
         if ( rct != p1 )
            break ;

         #ifdef S4WIN32
            ReadFile( h1, buf1, sizeof( buf1 ), (unsigned long *)&rc1, NULL ) ;
         #else
            rc1 = (unsigned)read( h1, buf1, sizeof( buf1 ) ) ;
         #endif
         f1->hasDup = 0 ;
         file4longAssign( tempLong, p1, 0 ) ;
         rc2 = file4readInternal( f1, tempLong, buf2, sizeof( buf2 ) ) ;
         f1->hasDup = 1 ;

         if ( rc1 != (long)rc2 )
            break ;

         if ( c4memcmp( buf1, buf2, (unsigned)rc1 ) != 0 )
            break ;

         if ( rc1 < sizeof( buf1 ) )
         {
            rc = 0 ;
            break ;
         }

         p1 += sizeof( buf1 ) ;
      }

   if ( h1 != INVALID4HANDLE )
   {
      #ifdef S4WIN32
         CloseHandle( h1 ) ;
      #else
         close( h1 ) ;
      #endif
   }

   error4set( f1->codeBase, 0 ) ;

   f1->inUse = 0 ;
   return rc ;
}



int file4copyx( CODE4 *c4, FILE4 *f1, char *f2 )
{
   #ifdef S4WINTEL
      HANDLE h1, h2 = INVALID4HANDLE ;
   #else
      int h1, h2 = INVALID4HANDLE ;
   #endif
   int rc = -1 ;
   long rct ;
   unsigned rc1, rc2 ;
   char buf[1024] ;
   long p1 ;
   #ifdef S4WIN32
      unsigned urc ;
   #endif

   if ( error4code( c4 ) != 0 )
      return -1 ;

   p1 = 0L ;

   #ifdef S4WIN32
      h2 = CreateFile( f2, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 ) ;
      /* if ( h2 == (int)INVALID_HANDLE_VALUE )  */
         /* h2 = -1 ; */
   #else
      h2 = sopen( f2, (int)(O_BINARY | O_RDWR), SH_DENYRW, 0 ) ;
   #endif
   h1 = f1->hand ;

   if ( h1 != INVALID4HANDLE && h2 != INVALID4HANDLE )
      for( ;; )
      {
         #ifdef S4WIN32
            rct = SetFilePointer( h1, p1, NULL, FILE_BEGIN ) ;
            if ( rct != (DWORD)-1 )
               rct = p1 ;
            rct = SetFilePointer( h2, p1, NULL, FILE_BEGIN ) ;
            if ( rct != (DWORD)-1 )
               rct = p1 ;
            ReadFile( h1, buf, sizeof( buf ), (unsigned long *)&urc, NULL ) ;
            rc1 = urc ;
            WriteFile( h2, buf, rc1, (unsigned long *)&rc2, NULL ) ;
         #else
            rct = (long)lseek( h1, (off_t)p1, 0 ) ;
            if ( rct != p1 )
               break ;
            rct = (long)lseek( h2, (off_t)p1, 0 ) ;
            if ( rct != p1 )
               break ;
            rc1 = (unsigned)read( h1, buf, sizeof( buf ) ) ;
            rc2 = (unsigned)write( h2, buf, rc1 ) ;
         #endif

         if ( rc1 != rc2 )
            break ;

         if ( rc1 < sizeof( buf ) )
         {
            rc = 0 ;
            break ;
         }

         p1 += sizeof( buf ) ;
      }

   if ( h2 != INVALID4HANDLE )
   {
      #ifdef S4WIN32
         CloseHandle( h2 ) ;
      #else
         close( h2 ) ;
      #endif
   }

   return rc ;
}
#endif
#endif

#ifdef E4ANALYZE
   static long g_counter = 0 ;
#endif

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION file4optimizeLow( FILE4 *file, const int optFlagIn, const int fileType, const long expectedReadSize, const void *ownerPtr )
{
   #ifdef S4OPTIMIZE_OFF
      return 0 ;
   #else
      OPT4 *opt ;
      int rc, optFlag ;
      double hitCountAdd ;
      #ifdef E4ANALYZE_ALL
         #ifndef S4OPTIMIZE_OFF
            HANDLE hand ;
            #ifdef S4MACINTOSH
               Str255 MACname ;
               FSSpec macSpec ;
            #endif
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( file == 0 || fileType < 0 || fileType > 3 || optFlagIn < -1 || optFlagIn > 1 )
            return error4( 0, e4parm, E90616 ) ;
      #endif

      // AS Apr 13/04 - support for large file optimization
      // #ifdef S4FILE_EXTENDED
      //    if ( file->isLong != 0 )  /* if is long, don't optimize */
      //       return 0 ;
      // #endif

      optFlag = optFlagIn ;
      rc = 0 ;

      opt = &file->codeBase->opt ;
      file->expectedReadSize = 0 ;
      file->ownerPtr = ownerPtr ;

      if ( optFlag == -1 )
      #ifdef S4OFF_MULTI
         optFlag = 1 ;
      #else
         // AS 06/21/00 - this was incorrect - according to docs, should not read-optimize if read-only is set, only
         // if the physical read-only file attribute is set.  In that case, the lowAccessMode is marked as exclusive,
         // so the readOnly part here is unnecessary...
         // optFlag = ( file->lowAccessMode != OPEN4DENY_NONE || file->isReadOnly ) ? 1 : 0 ;
         optFlag = ( file->lowAccessMode != OPEN4DENY_NONE ) ? 1 : 0 ;
      #endif

      if ( optFlag == 1 )
      {
         // AS May 24/02 - created file4openLow for internal use to indicate file types
         // AS Mar 4/03 - error here if file was optimized twice.  if file is optimized it will be
         // on the list
         // if ( file->doBuffer != 0 && (file->link.n == 0) )  /* already optimized */
         if ( file->doBuffer != 0 && (file->link.n != 0) )  /* already optimized */
            return 0 ;
         if ( opt->numBuffers > 0 )
         {
            file4longAssignError( file->len ) ;
            file->hashInit = opt->hashTrail * opt->blockSize ;
            #ifdef E4ANALYZE
               /* LY 2001/08/14 : un-nest file4lenLow in file4longError to
                  avoid "non-lvalue in unary &" in Linux */
               FILE4LONG tempFLen = file4lenLow( file ) ;
               if ( file4longError( tempFLen ) < 0 || opt->blockSize == 0 )
                  return error4( file->codeBase, e4info, E90616 ) ;
            #endif
            opt->hashTrail = (opt->hashTrail + file4longGetLo( file4lenLow( file ) ) / opt->blockSize) % opt->numBlocks ;
            file->doBuffer = 1 ;
         }
         else
            file->hashInit = - 1 ;

         #ifdef E4ANALYZE
            g_counter++ ;
            if ( g_counter == 10000 )  // just to avoid optimizing out.
            {
               file->type = 2 ;
               g_counter = 0 ;
            }
         #endif

         // AS May 24/02 - created file4openLow for internal use to indicate file types
         if ( file->link.n == 0 )   /* not on list, so add */
            l4add( &opt->optFiles, file ) ;
         file->type = (char)fileType ;
         rc = file4optimizeWrite( file, file->codeBase->optimizeWrite ) ;

         switch( fileType )
         {
            case OPT4DBF:
               if ( opt->blockSize == 0 ) /* probably not initialized, take CODE4 setting */
                  hitCountAdd = (double)expectedReadSize / (double)file->codeBase->memSizeBlock ;
               else
                  hitCountAdd = (double)expectedReadSize / (double)opt->blockSize ;
               if ( hitCountAdd > 1.0 )
                   file->hitCountAdd = 1.0 ;
                else
                   file->hitCountAdd = hitCountAdd ;
               break ;
         }

         #ifdef E4ANALYZE_ALL
            #ifndef S4OPTIMIZE_OFF
               tmpnam( file->dupName ) ;
               #ifdef S4WIN32
                  hand = CreateFile( file->dupName, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_WRITE | FILE_SHARE_READ, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 ) ;
               #else
                  hand = sopen( file->dupName, O_CREAT | O_TRUNC | O_RDWR, SH_DENYWR, S_IREAD  | S_IWRITE ) ;
               #endif

               // AS Apr 15/04 - at least for now don't duplicate large file names...
               if ( hand == INVALID4HANDLE || file4longGetHi( file4lenLow( file ) ) > 0 )
                  file->hasDup = 0 ;
               else
               {
                  #ifdef S4WIN32
                     CloseHandle( hand ) ;
                  #else
                     close( hand ) ;
                  #endif
                  if ( file4longGetLo( file4lenLow( file ) ) > 0 )
                  {
                     if ( file4copyx( file->codeBase, file, file->dupName ) < 0 )
                        file->hasDup = 0 ;
                     else
                        file->hasDup = 1 ;
                  }
                  else
                     file->hasDup = 1 ;
               }
            #endif
         #endif
      }
      else  /* 0 */
      {
         // AS May 24/02 - created file4openLow for internal use to indicate file types
         if ( file->link.n == 0 )   /* not optimized */
            return 0 ;

         #ifdef E4ANALYZE_ALL
            if ( file->hasDup == 1 )
            {
               /* first do a final verification */
               if ( file->doBuffer == 1 || file->link.n == 0 )
                  if ( file4cmp( file ) != 0 )
                     return error4( file->codeBase, e4opt, E80602 ) ;
               #ifndef S4MACINTOSH
                  u4remove( file->dupName ) ;
               #else
                  strcpy( (char *)MACname, file->dupName) ;
                  CtoPstr( (char *)MACname ) ;
                  if ( FSMakeFSSpec( file->macSpec.vRefNum, file->macSpec.parID, MACname, &macSpec ) == noErr )
                     FSpDelete( &macSpec ) ;
               #endif
               file->hasDup = 0 ;
            }
         #endif

         rc = file4optimizeWrite( file, 0 ) ;
         if ( rc == 0 )
         {
            if ( opt4fileFlush( file, 1 ) < 0 )
               return error4( file->codeBase, e4optFlush, E90616 ) ;
            l4remove( &opt->optFiles, file ) ;
            // AS May 24/02 - created file4openLow for internal use to indicate file types
//            file->type = OPT4NONE ;
            file->doBuffer = 0 ;
         }
      }

      // AS 04/20/01 - Enable file optimization if we are here and are optimizing and it is not enabled yet
      // AS Jul 2/03 - Don't enable if c4->hadOpt == 1, since that means we have called suspend (user called, so wait
      // til he/she calls code4optStart() again)
      if ( optFlag == 1 && file->codeBase->hasOpt == 0 && file->codeBase->hadOpt == 0 )
         code4optStart( file->codeBase ) ;

      return rc ;
   #endif
}

/* optFlag has the same definitions as C4CODE.optimizeWrite */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
// AS Sep 16/04 - Don't support write-optimization of compressed write files...there are sequencing problems
int S4FUNCTION file4optimizeWrite( FILE4 *file, int optFlag )
{
   #ifdef S4OPTIMIZE_OFF
      return 0 ;
   #else
      int rc ;

      rc = 0 ;

      #ifdef E4PARM_HIGH
         if( file == 0 || optFlag < -1 || optFlag > 1 )
            return error4( 0, e4parm, E90617 ) ;
      #endif

      if ( optFlag == file->writeBuffer )
         return rc ;

      // LY Jan 19/05 : added switches to avoid compiler error
      #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
         // AS Sep 16/04 - Don't support write-optimization of compressed write files...there are sequencing problems
         // which need to be solved first.
         if ( file->compressInfo != 0 )
            optFlag = 0 ;
      #endif

      switch ( optFlag )
      {
         case 0:
            if ( file->doBuffer )
               rc = opt4fileFlush( file, 1 ) ;
            file->writeBuffer = 0 ;
            break ;
         case  -1 :
            #ifdef S4OFF_MULTI
               if ( file->doBuffer )
                  file->bufferWrites = 1 ;
               file->writeBuffer = 1 ;
            #else
               if ( file->lowAccessMode != OPEN4DENY_RW )
               {
                  if ( file->doBuffer )
                  {
                     rc = opt4fileFlush( file, 1 ) ;
                     file->bufferWrites = 0 ;
                  }
                  file->writeBuffer = 0 ;
               }
               else
               {
                  if ( file->doBuffer )
                     file->bufferWrites = 1 ;
                  file->writeBuffer = 1 ;
               }
            #endif
            break ;
         case 1:
            #ifndef S4OFF_MULTI
               if ( file->lowAccessMode == OPEN4DENY_RW )
            #endif
            if ( file->doBuffer )
               file->bufferWrites = 1 ;
            file->writeBuffer = 1 ;
            break ;
         default:
            return 0 ;
      }

      return rc ;
   #endif
}

/* tries to actually turn on/off the write bufferring when locking/unlocking a file */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
void file4setWriteOpt( FILE4 *f4, int setOpt )
{
   #ifndef S4OPTIMIZE_OFF
      if ( setOpt == f4->bufferWrites )
         return ;
      if ( setOpt == 1 && f4->writeBuffer == 1 )
         f4->bufferWrites = 1 ;
      if ( setOpt == 0 && f4->writeBuffer == 1 )
         f4->bufferWrites = 0 ;
   #endif
}
