/* f4close.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

// AS Mar 22/06 - cleaned up
#if !defined( S4OFF_MULTI ) && defined( S4WINTEL ) && !defined( S4WINCE ) && !defined( S4IBMOS2 )
   #ifndef __TURBOC__
      #include <sys\locking.h>
      #define S4LOCKING
   #endif
   #ifdef _MSC_VER
      #include <sys\types.h>
      #include <sys\locking.h>
   #endif
#endif

#ifdef S4TRACK_FILES_OR_SERVER
   unsigned int numFiles5 ;
   #ifdef S4TRACK_FILES
      int f4print = 0 ; /* if f4print == 0 then stdout, else stdprn, no output if -1 */
      #ifndef S4TESTING
         extern FILE4 s4test ;
         void S4FUNCTION u4writeErr( const char * err_str, int newLine )
         {
            int errCode, flushCode ;
            long len ;

            if (s4test.hand != INVALID4HANDLE && err_str != 0 )
            {
               errCode = error4code( s4test.codeBase ) ;
               error4set( s4test.codeBase, 0 ) ;

               len = file4len( &s4test ) ;
               if( len >= 0 )
               {
                  if ( newLine == 1 )
                  {
                     file4write( &s4test, len, "\r\n", 2 ) ;
                     len += 2 ;
                  }
                  flushCode = s4test.codeBase->fileFlush ;
                  s4test.codeBase->fileFlush = 1 ;
                  file4lenSet( &s4test, len + strlen(err_str) ) ;
                  file4write( &s4test, len, (void *)err_str, strlen(err_str) ) ;
                  file4flush( &s4test ) ;
                  s4test.codeBase->fileFlush = flushCode ;
               }

               error4set( s4test.codeBase, errCode ) ;
            }
         }
      #endif  /* S4TESTING */
   #endif  /* S4TRACK_FILES */
#endif  /* S4TRACK_FILES_OR_SERVER */



#ifndef S4OFF_OPTIMIZE
static void file4closeCreateOptimizedFile( FILE4 *file )
{
   FILE4LONG len ;

   /* LY July 07/03 : changed from 0 to 0L for Linux compiler */
   file4longAssign( len, 0, 0L ) ;
   file4lenSetLow( file, len ) ;
   file4optimize( file, 0, 0 ) ;
}
#endif



#ifdef S4ADVANCE_READ
   static void  file4closeCancelAdvanceRead( FILE4 *file )
   {
      /* just cancel any oustanding reads */
      if ( l4numNodes( &file->advanceReadFileList ) != 0 )
         file4advanceCancel( file ) ;
      if ( file->advanceReadBuf != 0 )
      {
         u4free( file->advanceReadBuf ) ;
         file->advanceReadBuf = 0 ;
      }
   }
#else
   #define file4closeCancelAdvanceRead( file )
#endif

#ifdef S4WRITE_DELAY
   static void file4closeCompleteDelayWrites( FILE4 *file )
   {
      /* ensure that there are no outstanding file-writes delaying */
      if ( l4numNodes( &file->delayWriteFileList ) != 0 )
         file4writeDelayFlush( file, ( file4getTemporary( file ) ? 0 : 1 ) ) ;
   }
#else
   #define file4closeCompleteDelayWrites( file )
#endif

static int file4closeHandle( FILE4 *file )
{
   int rc ;

   // AS May 22/03 - only with debug-print
   #if defined( E4ANALYZE ) && defined( S4SERVER ) && defined( S4OLEDEBUG_PRINT )
      // AS May 16/03 - Perform some logging to help solve a c/s open error
      char buf[200] ;
      SERVER4CLIENT *client = file->codeBase->currentClient ;
      sprintf( buf, "file4closeHandle: name: %s, for clientId: %ld\r\n", file->name, (long)((client == 0) ? -1 : client->id) ) ;
      log5( buf ) ;
   #endif

   #if defined(S4WIN32)
      // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
      // Microsoft knowledgebase article #811693
      #ifdef S4WINCE
         for ( short numTries = 0 ;; numTries++ )
         {
      #endif
            rc = (int)CloseHandle( (HANDLE)file->hand ) ;
   #ifdef S4WINCE
         if ( rc == 0 )
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
   #elif defined(S4WINDOWS)
      rc = _lclose( file->hand ) ;
   #elif defined(S4MACINTOSH)
      rc = FSClose( file->hand ) ;
   #elif defined(S4PALM)
      rc = FileClose( file->hand ) ;
   #else
      rc = close( file->hand ) ;
   #endif

   #ifdef S4WINCE
      return ( rc ? 0 : -1 ) ;
   #else
      return rc ;
   #endif
}



static int file4closeRemoveTempFile( FILE4 *file )
{
   #if !defined( S4CLIENT ) && !defined( S4OFF_OPTIMIZE )
      // AS Sep 3/03 - The file may not have actually been created, in which case just return
      if ( file->fileCreated == 0 )
         return 0 ;
   #endif

   #ifdef S4MACINTOSH
      return FSpDelete( &file->macSpec) ;
   #else
      // AS May 22/03 - only with debug-print
      #if defined( E4ANALYZE ) && defined( S4SERVER ) && defined( S4OLEDEBUG_PRINT )
         // AS May 16/03 - Perform some logging to help solve a c/s open error
         char buf[200] ;
         SERVER4CLIENT *client = file->codeBase->currentClient ;
         sprintf( buf, "file4closeRemoveTempFile: name: %s, for clientId: %ld\r\n", file->name, (long)((client == 0) ? -1 : client->id) ) ;
         log5( buf ) ;
      #endif

      if ( u4remove( file->name ) < 0 )
         return error4( file->codeBase, e4remove, E90601 ) ;
      return 0 ;
   #endif
}



static void file4closeCleanStructure( FILE4 *file )
{
   if ( file->doAllocFree )
   {
      u4free( file->nameBuf ) ;
      file->name = 0 ;
   }

   // AS May 17/04 - client/server functionality to copmress the data file...
   #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      // AS Nov 26/02 - Support for data file compression
      if ( file->compressInfo != 0 )
         file4compressInitUndo( file ) ;
   #endif

   c4memset( (void *)file, 0, sizeof( FILE4 ) ) ;
   file->hand = INVALID4HANDLE ;
}



#ifdef S4TRACK_FILES_OR_SERVER
static void file4closeTrackFiles( FILE4 *file )
{
   numFiles5-- ;
   #ifdef S4TRACK_FILES
      if ( f4print != -1 )
      {
          #ifdef S4WINDOWS
             #ifdef S4TESTING
                if ( mem4displayPtr == 0 )
                   error4( c4, e4info, E50101 ) ;
                d4display_str( mem4displayPtr, "\r\nfile closed: ", 1 ) ;
                d4display_str( mem4displayPtr, f4print, file->name ) ;
             #else
                u4writeErr( "file closed: ", 1 ) ;
                u4writeErr( file->name, 0 ) ;
             #endif
          #else
             if ( f4print )
                fprintf( stdprn, "\r\nfile closed: %s", file->name ) ;
             else
                printf( "\r\nfile closed: %s", file->name ) ;
          #endif
       }
   #endif
}
#else
   #define file4closeTrackFiles( file )
#endif

#ifdef E4ANALYZE
   static long g_close_count = 0 ;
#endif

int S4FUNCTION file4close( FILE4 *file )
{
   CODE4 *c4 ;

   #ifdef E4PARM_HIGH
      if ( file == 0 )
         return error4( 0, e4parm_null, E90601 ) ;
   #endif

   // AS Sep 3/03 - the file handle may be invalid on a temporary non created file, in which case we continue anyway
   #ifdef S4CLIENT
      if ( file->isTemporary == 0 )
   #else
      if ( file->isTemporary == 0
      #ifndef S4OFF_OPTIMIZE  /* LY Dec 9/03 */
         || file->fileCreated == 1
      #endif
      )
   #endif
   {
      if ( file->hand == INVALID4HANDLE )
         return 0 ;
   }

   c4 = file->codeBase ;  /* file->coseBase maybe == 0 if file structure was not completely opened, so be careful with use */

   // AS Sep 3/03 - Was not properly closing temporary in-memory files.  Change here so we can track
   // when those files are being closed (in which case we do not want to create them or flush them to disk
   // at all)
   if ( file4getTemporary( file ) == 1 )
      file->closingTempFile = 1 ;

   #ifndef S4OFF_OPTIMIZE
      /* if file not created yet, we need to create it here */
      // AS Sep 3/03 - If the file is temporary we don't need to actually create it.
      if ( file->fileCreated == 0 && file4getTemporary( file ) == 0 )
      {
         #ifdef E4ANALYZE
            g_close_count++ ;
         #endif
         file4closeCreateOptimizedFile( file ) ;
      }
      else
      {
         file4optimize( file, 0, 0 ) ;
   #endif

         file4closeCancelAdvanceRead( file ) ;
         file4closeCompleteDelayWrites( file ) ;

         // AS Dec 11/02 - Renamed for clarification
         #ifdef S4DELAY_WRITE_MT
            critical4sectionInitUndo( &file->critical4file ) ;
         #endif

         if ( file4closeHandle( file ) < 0 )
         {
            if ( file->name == 0 )
               return error4( c4, e4close, E90601 ) ;
            else
               return error4describe( c4, e4close, E90601, file->name, 0, 0 ) ;
         }

         if ( file4getTemporary( file ) )
            file4closeRemoveTempFile( file ) ;
         #ifdef S4MACINTOSH
            else
            {
               if (FlushVol( 0, file->macSpec.vRefNum ) != 0 )
                  return error4( c4, e4optFlush, E90601 ) ;
            }
         #endif

         // AS Sept. 18/02 0 only mark as closes files that actually were created
         file4closeTrackFiles( file ) ;

   #ifndef S4OFF_OPTIMIZE
      }
   #endif

   // AS Nov 8/02 - sema4 locking
   #ifdef S4MUTEX4LOCK
      mutex4initUndo( &file->lockMutex ) ;
   #endif

   file4closeCleanStructure( file ) ;

   if ( c4 != 0 )
   {
      if ( error4code( c4 ) < 0 )
      {
         #ifdef S4SERVER
            if ( c4->currentClient != 0 )
         #endif
               return -1 ;
      }
   }

   return 0 ;
}
