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

/* f4flush.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef __WATCOMC__
   #ifdef M_I386
      #define S4USE_EBX
   #endif
#endif

#ifdef S4OS2
   #define S4USE_DUP
#endif

/* returns 1 if no handles available */
static int file4lowFlush( FILE4 *file )
{
   #ifdef S4PALM
      return 0 ;
   #else
      int rc ;

      #ifdef S4USE_DUP
         #ifndef S4NO_DUP
            int handle ;
         #endif
      #else
         #ifdef S4WINTEL
         #ifndef S4WIN32
            union REGS dosFlush ;
         #endif
         #endif
      #endif

      #ifdef S4MACINTOSH
         short vRefNum ;
      #endif

      #ifdef E4PARM_LOW
         if ( file == 0 )
            return error4( 0, e4parm, E90610 ) ;
      #endif

      #ifndef S4OFF_OPTIMIZE
         if ( file->fileCreated == 0 )  /* file not open */
            return 0 ;
      #endif
      if ( file->hand == INVALID4HANDLE )  /* file not open */
         return 0 ;

      #ifdef S4WRITE_DELAY
         if ( l4numNodes( &file->delayWriteFileList ) != 0 )
         {
            /* for now, just flush out any changed data.  Later allow reading
               of delay-write data */
            file4writeDelayFlush( file, 1 ) ;
         }

         critical4sectionEnter( &file->critical4file ) ;
      #endif

      rc = 0 ;

      #ifdef S4USE_DUP
         #ifndef S4NO_DUP
            handle = dup( file->hand ) ;
            if ( handle == INVALID4HANDLE )  /* means that there are no available handles */
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
            else
               if ( close( handle ) < 0 )
                  rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #endif
      #else
         #if defined( S4MACINTOSH )
            if ( GetVRefNum( file->hand, &vRefNum ) != 0 )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
            else
               if ( FlushVol( 0, vRefNum ) != 0 )
                  rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #elif defined( S4WIN32 )
            // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
            // Microsoft knowledgebase article #811693
            BOOL bres ;
            #ifdef S4WINCE
               for ( short numTries = 0 ;; numTries++ )
               {
            #endif
                  bres = FlushFileBuffers( (HANDLE) file->hand ) ;
            #ifdef S4WINCE
                  if ( !bres )
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
            if ( !bres )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #elif !defined( S4UNIX )
            dosFlush.h.ah = 0x68 ;
            #ifdef S4USE_EBX
               dosFlush.x.ebx = (unsigned int)file->hand ;
            #else
               dosFlush.x.bx = (unsigned int)file->hand ;
            #endif
            intdos( &dosFlush, &dosFlush ) ;
            if ( dosFlush.x.cflag != 0 )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #else
            if ( fsync(file->hand) < 0 )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #endif
      #endif

      #ifdef S4WRITE_DELAY
         critical4sectionLeave( &file->critical4file ) ;
      #endif

      return rc ;
   #endif
}

#ifndef S4OFF_OPTIMIZE
int opt4fileFlush( FILE4 *file, const int doFree )
{
   FILE4LONG realLen ;
   int saveRc, rc ;

   #ifdef E4PARM_LOW
      if ( file == 0 || doFree < 0 || doFree > 1 )
         return error4( 0, e4parm, E90610 ) ;
   #endif

   if ( file->doBuffer == 1 )
   {
      switch( file->type )
      {
         case OPT4DBF:
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.dbfLo.list, doFree ) ;
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.dbfHi.list, doFree ) ;
            break ;
         case OPT4INDEX:
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.indexLo.list, doFree ) ;
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.indexHi.list, doFree ) ;
            break ;
         default:
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.other.list, doFree ) ;
            break ;
      }
      if ( file->codeBase->opt.writeFile == file )  /* flush out the final file changes */
      {
         saveRc = opt4flushWriteBuffer( &file->codeBase->opt ) ;
         if ( saveRc < 0 )
            rc = saveRc ;
         #ifdef E4ANALYZE
            if ( file->codeBase->opt.writeFile == file )  /* we should have fixed this... */
               rc = error4( file->codeBase, e4opt, E80603 ) ;
         #endif
      }

      // AS 12/01/00 - changed to unsigned long value...
      if ( file4longError( file->len ) != ULONG_MAX )
      {
         file->doBuffer = 0 ;
         realLen = file->len ;

         #ifdef S4OFF_OPTIMIZE
            if ( file->fileCreated == 0 )
            {
               #ifdef E4ANALYZE
                  if ( file4longGetLo( realLen ) != 0 )
                     return error4( file->codeBase, e4opt, E80603 ) ;
               #endif
            }
            else  /* for file4len lines below */
         #endif
            {
               // AS Jun 22/06 - this code fails in the case where the file has been set to the nearest block length due to encryption...
               #if defined( S4PREPROCESS_FILE )
                  if ( file->preprocessed )  // in this case we need to adjust the real length of the file to put on the block boundary
                  {
                     short blockSize = code4getPreprocessBlockSize( file->codeBase ) ;
                     short posMod = (short)file4longMod( realLen, blockSize ) ;
                     if ( posMod != 0 )
                        file4longAdd( &realLen, (blockSize - posMod ) ) ;
                  }
               #endif

               if ( file4longGetLo( file4lenLow( file ) ) != file4longGetLo( realLen ) )   /* make sure the length is updated */
                  file4lenSetLow( file, realLen ) ;
            }

         file->doBuffer = 1 ;
      }
      if ( rc )
         return rc ;
   }

   return 0 ;
}
#endif /* S4OFF_OPTIMIZE */

int S4FUNCTION file4flush( FILE4 *file )
{
   int rc, err ;

   #ifdef E4PARM_HIGH
      if ( file == 0 )
         return error4( 0, e4parm_null, E90611 ) ;
   #endif

   if ( file->isReadOnly == 1 )
      return 0 ;

   #ifndef S4OFF_OPTIMIZE
      opt4fileFlush( file, 0 ) ;
   #endif

   err = error4set( file->codeBase, 0 ) ;
   rc = file4lowFlush( file ) ;
   if ( err != 0 )
      error4set( file->codeBase, (short)err ) ;

   if ( rc < 0 )
      return error4stack( file->codeBase, (short)rc, E90611 ) ;

   return 0 ;
}

#ifndef _MSC_VER
   #ifdef P4ARGS_USED
   #pragma argsused
#endif
#endif
int S4FUNCTION file4refresh( FILE4 *file )
{
   #ifndef S4OPTIMIZE_OFF
      #ifndef S4SINGLE
         int rc ;
         #ifdef E4PARM_HIGH
            if ( file == 0 )
               return error4( 0, e4parm_null, E90612 ) ;
         #endif

         if ( file->doBuffer == 0 || file->lowAccessMode != OPEN4DENY_NONE )
            return 0 ;

         rc = opt4fileFlush( file, 1 ) ;
         if ( rc < 0 )
            return error4stack( file->codeBase, rc, E90612 ) ;
         file4longAssignError( file->len ) ;
      #endif
   #endif
   return 0 ;
}
