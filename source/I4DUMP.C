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

/* i4dump.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#if defined( E4ANALYZE ) && defined( S4STAND_ALONE ) && !defined( S4OFF_INDEX ) && defined( S4MDX )
   static void output( int h, char * s )
   {
      #ifdef S4MACINTOSH
         long len = strlen(s) ;
         FSWrite(h, &len, s ) ;
      #else
         write( h, s, c4strlen(s) ) ;
      #endif
   }

   static int tfile4dumpDo( TAG4FILE *t4, int outHandle, int level, int displayAll )
   {
      char outBuf[200] ;
      int rc, iKey ;
      B4BLOCK *blockOn ;
      char buf[ I4MAX_KEY_SIZE ] ;
      int len ;

      rc = tfile4down( t4 ) ;
      if ( rc < 0 || rc == 2 )
         return -1 ;
      blockOn = tfile4block( t4 ) ;

      if ( b4leaf( blockOn ) && !displayAll )
      {
         tfile4up( t4 ) ;
         return 0 ;
      }

      if ( b4leaf( blockOn) )
      {
         M4PRINT( outBuf, "\r\n\r\nLeaf B4BLOCK #: %ld   Level %d\r\n\r\n", blockOn->fileBlock, level ) ;
         output( outHandle, outBuf ) ;
         output( outHandle, "Record Number   Key\r\n" ) ;
      }
      else
      {
         M4PRINT( outBuf, "\r\n\r\nBranch B4BLOCK #: %ld   Level %d\r\n\r\n", blockOn->fileBlock, level ) ;
         output( outHandle, outBuf ) ;
         output( outHandle, "FILE4 B4BLOCK      Key\r\n" ) ;
      }

      for ( iKey = 0; iKey < blockOn->nKeys; iKey++ )
      {
         M4PRINT( outBuf, "\r\n%10ld      ", b4key(blockOn,iKey)->num ) ;

         output( outHandle, outBuf ) ;
         if ( tfile4type( t4 ) == r4str )
         {
            len =  50 ;
            if ( len > t4->header.keyLen )
               len =  t4->header.keyLen ;

            c4memcpy( buf, b4keyKey( blockOn, iKey ), len ) ;
            buf[len] =  0 ;
            output( outHandle, buf ) ;
         }
      }

      if ( b4leaf( blockOn ) )
      {
         tfile4up( t4 ) ;
         return 0 ;
      }

      M4PRINT( outBuf, "\r\n%10ld      ", b4key( blockOn, iKey )->num ) ;
      output( outHandle, outBuf ) ;

      blockOn->keyOn = 0 ;
      do
      {
         rc = tfile4dumpDo( t4, outHandle, level + 1, displayAll ) ;
         if ( rc > 0 )
            return error4( t4->codeBase, e4result, E94905 ) ;
         if ( rc < 0 )
            return -1 ;
      } while ( b4skip( blockOn, 1) == 1 ) ;

      tfile4up(t4) ;
      return 0 ;
   }

   int tfile4dump( TAG4FILE *t4, int outHandle, const int displayAll )
   {
      int rc ;

      #ifdef E4PARM_LOW
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E94902 ) ;
      #endif

      if ( error4code( t4->codeBase ) < 0 )
         return -1 ;

      rc = index4lock( t4->indexFile, 0L ) ;
      if ( rc )
         return rc ;

      if ( tfile4freeAll( t4 ) < 0 )
         return -1 ;

      output( outHandle, "\r\n\r\n" ) ;
      tfile4dumpDo( t4, outHandle, 0, displayAll ) ;
      return index4unlock( t4->indexFile, 0L ) ;
   }
#endif /* E4ANALYZE && S4STAND_ALONE && !S4OFF_INDEX && S4MDX */
