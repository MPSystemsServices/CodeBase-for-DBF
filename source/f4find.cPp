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

/* f4find.cpp (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

// AS Jan 15/03 - Only needed if building server or building OLE-DB
#if defined( S4SERVER ) || defined( OLEDB5BUILD )
#if defined(__cplusplus) && ( defined(S4WIN32) || ( defined(S4UNIX) && !defined(S4STAND_ALONE ) ) )

   /* The Find4file class is a platform-independant API to find files.
      There is currently no functionality to find files without extensions. */

   Find4file::Find4file( const char *path, const char *fileName, const char *extension )
   {
      /* path is the directory to look for files in. If it ends in a file name,
            it will be discarded. If null, the current directory will be used.
         fileName is the name, without path or extension, of the file to
            search for. If null, find all files. On Win32, it may contain
            wildcards.
         extension is the extension of the files to find. If null, find all
            files in the directory. On Win32, it may contain wildcards.
            It does not matter if there is a leading dot. The extension is
            limited to 7 bytes. */
      char dir[LEN4PATH] ;
      if ( path )
         u4namePath( dir, LEN4PATH, path ) ;
      else
      {
         dir[0] = '.' ;
         dir[1] = S4DIR ;
         dir[2] = 0 ;
      }

      // Copy the file name to the fn member
      if ( fileName )
      {
         strncpy( fn, fileName, LEN4PATH ) ;
         fn[LEN4PATH-1] = 0 ;
      }
      else
         fn[0] = 0 ;

      // Copy the extension to the ext member. Omit the leading dot, if it exists.
      if ( extension )
      {
         strncpy( ext, extension + ((extension[0]=='.')?1:0), 8 ) ;
         ext[7] = 0 ;
      }
      else
         ext[0] = 0 ;

      #ifdef S4WIN32
         if ( fileName )
         {
            strcat( dir, fileName ) ;
            strcat( dir, "." ) ;
         }
         else
            strcat( dir, "*." ) ;

         if ( extension )
            strcat( dir, ext ) ;
         else
            strcat( dir, "*" ) ;

         #ifdef S4UNICODE  // CS 2001/06/22
            unsigned short wideDir[LEN4PATH] ;
            c4atou( dir, wideDir, LEN4PATH ) ;
            rc = FindFirstFile( wideDir, &findData ) ;
         #else
            rc = FindFirstFile( dir, &findData ) ;
         #endif
      #endif

      #ifdef S4UNIX
         findData = opendir( dir ) ;
      #endif
   }

   const char *Find4file::getNext(char *dest, size_t destLen )
   {
      /* Get the next matching file name.
         If dest is not null, the file name is copied into dest.
         destLen is the length of dest.
         If destLen is shorter than the file name size, the file name copied
         into dest will be truncated.
         Returns the file name or null if there are no more files. */
      #ifdef S4WIN32
         // LY 2003/07/31 #ifdef S4WIN64
         //    if ( rc == NULL )
         // #else
            if ( rc == INVALID_HANDLE_VALUE )
         // #endif
         {
            if ( dest )
               dest[0] = 0 ;
            return 0 ;
         }

         #ifdef S4UNICODE
            unsigned short tmpFileName[LEN4PATH] ;
            wcscpy( tmpFileName, findData.cFileName ) ;
            c4utoa( tmpFileName ) ;
         #endif
         if ( dest )
         {
            #ifdef S4UNICODE  // CS 2001/06/22
               strncpy( dest, (const char *)tmpFileName, destLen ) ;
            #else
               strncpy( dest, findData.cFileName, destLen ) ;
            #endif
            dest[destLen-1] = 0 ;
         }

         #ifdef S4UNICODE  // CS 2001/06/22
            strncpy( foundName, (const char*)tmpFileName, LEN4PATH ) ;
         #else
            strncpy( foundName, findData.cFileName, LEN4PATH ) ;
         #endif
         foundName[LEN4PATH-1] = 0 ;

         if ( ! FindNextFile( rc, &findData ) )
            close() ;  // no more files found, so close

         return foundName ;
      #endif

      #ifdef S4UNIX
         if ( !findData )
         {
            if ( dest )
               dest[0] = 0 ;
            return 0 ;
         }

         const dirent *entry ;
         for ( entry = readdir( findData ) ; entry ; entry = readdir( findData ) )
         {
            const short fnlen = strlen( fn ) ;
            const short extlen = strlen( ext ) ;
            const short foundlen = strlen( entry->d_name ) ;

            // Calculate expected length.
            if ( fnlen && extlen )
            {
               if ( foundlen != fnlen + 1 + extlen )
                  continue ;
            }
            else
            {
               short len = 0 ;
               len += fnlen ? 1 : fnlen ;  // file name
               len += 1 ;  // dot
               len += extlen ? 1 : extlen ;  // extension
               if ( foundlen < len )
                  continue ;
            }

            // Compare file name.
            if ( fnlen )
               if ( strncmp( entry->d_name, fn, fnlen ) )
                  continue ;

            // Calculate dot position relative to extension.
            if ( extlen )
               if ( entry->d_name[foundlen-extlen-1] != '.' )
                  continue ;

            // Calculate dot position relative to file name.
            if ( fnlen )
               if ( entry->d_name[fnlen] != '.' )
                  continue ;

            // Compare extension.
            if ( extlen )
               if ( strcmp( &entry->d_name[foundlen - extlen], ext ) )
                  continue ;

            // If we reach this point, all tests have passed
            // and the directory entry should be a match.
            if ( dest )
            {
               strncpy( dest, entry->d_name, destLen ) ;
               dest[destLen-1] = 0 ;
            }

            strncpy( foundName, entry->d_name, LEN4PATH ) ;
            foundName[LEN4PATH-1] = 0 ;
            return foundName ;
         }

         // If we reach this point, the for loop exited out normally,
         // which means that no more matches were found.
         if ( dest )
            dest[0] = 0 ;
         return 0 ;
      #endif
   }

   void Find4file::close()
   {
      /* Free any memory/structures. */
      #ifdef S4WIN32
         // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
         //    if ( rc != NULL )
         // #else
            if ( rc != INVALID_HANDLE_VALUE )
         // #endif
         {
            FindClose( rc ) ;
            // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
            //    rc = NULL ;
            // #else
               rc = INVALID_HANDLE_VALUE ;
            // #endif
         }
      #endif

      #ifdef S4UNIX
         closedir( findData ) ;
         findData = 0 ;
      #endif
   }
#endif /* #if defined(__cplusplus) && ( defined(S4WIN32) || ( defined(S4UNIX) && !defined(S4STAND_ALONE ) ) )*/
#endif /* #if defined( S4SERVER ) || defined( OLEDB5BUILD ) */
