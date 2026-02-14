/* u4name.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/*
   Functions available for manipulating file names:

   u4nameCreateMultiDirectories - create a name using 2 directory inputs
   u4nameExtWideFindExtensionPos - find the position of extension in wide file name
   u4nameExtWide - add a wide extension to a wide file name
   u4nameWideRemoveGivenExtension - remove a given extension for a wide file name
   u4nameExtFindExtensionPos - find the position of extension in a file name
   u4nameRemoveGivenExtension - remove a given extension for a file name
   u4nameExt - add an extension to a file name
   u4nameFix - remove any ("..\" and previous file path part) and ".\" from a file name
   u4nameCurrent - generate a file name using input name and current directory
   u4nameCurrentExtended - u4nameCurrent but takes an additional input path as well
   u4nameMakeFindDrive - use to call u4nameMake when you don't have a drive letter to start with
   u4nameMake - creates a file name given input drive, path,, and file name
   u4namePiece - extract file name with optional path and extension from input file name
   u4trim - trim blanks off of string
   u4nameChar - returns true iff a valid dBase field or function name character
   u4nameFindFileName - returns pointer to name part of path given length of non-name path, adjusts for backslash
   u4namePathSpecial - return path from a file name, excluding trailing backslash character
   u4namePath - return path from a file name, including trailing backslash character
   u4nameRetExt - retrieve extension from file name
   u4namecmp - compare file names, possibly ignoring case
   u4namencmp - same as u4namecmp, but specifying length
   u4pathSet( const char *path ) - sets the current drive/path to the input value
*/

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4WINTEL
   #ifndef S4WINCE
      #include <direct.h>
   #endif
#endif
#ifndef S4PALM
   #include <ctype.h>
#endif

#ifdef S4WIDECHAR
   static const WSTR5 g5_dot = '.' ;
   static const WSTR5 g5_backSlash = '\\' ;
   static const WSTR5 g5_dotWide = L'.' ;
   static const WSTR5 g5_backSlashWide = L'\\' ;

   int u4nameExtWideFindExtensionPos( const WSTR5 *name )
   {
      // this function returns the position within the string of the '.' character indicating
      // the file extension position.  Returned is the position of this value within the string.
      // if no such position exists, the string length is returned.

      int extPos = c4wcslen( name ) ;  /* LY 2001/09/13 : changed from wcslen */

      if ( extPos != 0 )
      {
         // go backwards through the string, until the first '.' character (we are at the
         // extension).  The cases of meeting a '\' or the beginning of the string mean that
         // no '.' was found, so there is no current extension (means append at current location).
         for ( int onPos = extPos - 1 ;; onPos-- )
         {
            if ( name[onPos] == g5_dotWide )
            {
               extPos = onPos ;
               break ;
            }

            if ( name[onPos] == g5_backSlashWide )
               break ;

            if ( onPos == 0 )
              break ;
         }
      }

      return extPos ;
   }


   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4FUNCTION u4nameExtWide( WSTR5 *name, int lenResult, const WSTR5 *extensionToAdd, const int doReplace )
   {
      /* u4nameExt() for wide strings */
      WSTR5 *ptr = name + c4wcslen( name ) - 1 ;  /* LY 2001/09/13 : changed from wcslen */

      // trim off any blanks at the end of the file name
      while ( *ptr == L' ' )
      {
         *ptr = L'\0' ;
         ptr-- ;
      }

      int fileNameLen = c4wcslen( name ) ;  /* LY 2001/09/13 : changed from wcslen */
      int extPos = u4nameExtWideFindExtensionPos( name ) ;  // get the position of the '.' character (or where it should go)

      // if there is an existing extension found, and doReplace is false, just return...
      if ( fileNameLen != extPos && !doReplace )
         return 0 ;

      // increment after the '.' in the extension ot add if it exists (i.e. input of '.dbf' or 'dbf' both mean the same...
      if ( *extensionToAdd == g5_dotWide )
         extensionToAdd++ ;

      #ifdef E4MISC
         int extLen = c4wcslen( extensionToAdd ) ;  /* LY 2001/09/13 : changed from wcslen */
         if ( extLen > 3 )
            extLen = 3 ;
         if ( lenResult <= extPos + extLen + 1 )
            return error4( 0, e4result, E94507 ) ;
      #endif

      name[extPos++] = g5_dotWide ;
      c4wcscpy( name + extPos, extensionToAdd ) ;  /* LY 2001/09/13 : changed from wcscpy */

      return 0 ;
   }



   int S4FUNCTION u4nameWideRemoveGivenExtension( WSTR5 *name, const WSTR5 *extensionToRemove )
   {
      // this function is used to remove the input extension if it exists.  For example, you
      // can remove the '.DBF' extensions from the file name while leaving all others intact.

      // Note that this function is case sensitive, so ensure that the inputs have been correctly
      // cased.

      /* u4nameExt() for wide strings */
      WSTR5 *ptr = name + c4wcslen( name ) - 1 ;  /* LY 2001/09/13 : changed from wcslen */

      // trim off any blanks at the end of the file name
      while ( *ptr == L' ' )
      {
         *ptr = L'\0' ;
         ptr-- ;
      }

      int fileNameLen = c4wcslen( name ) ;  /* LY 2001/09/13 : changed from wcslen */
      int extPos = u4nameExtWideFindExtensionPos( name ) ;  // get the position of the '.' character (or where it should go)

      // if there is no extension, just return...
      if ( fileNameLen == extPos )
         return 0 ;

      int dotPositionInString = extPos ;

      // the extPos will position us to the '.' character.  Adjust for input containing or not
      // containing this...
      if ( *extensionToRemove != g5_dotWide )  // not included, increment extPos...
         extPos++ ;

      #ifdef S4CASE_SEN
         // BCR 10/17/00 -- changed _wcscmp to wcscmp
         /* LY 2001/09/13 : changed from wcscmp */
         if ( c4wcscmp( &name[extPos], extensionToRemove ) == 0 )  // matches, so remove...
      #else
         if ( _wcsicmp( &name[extPos], extensionToRemove ) == 0 )  // matches, so remove...
      #endif
      {
         // clear out the extension characters in addition to the '.' character...
         c4memset( &name[dotPositionInString], 0, (fileNameLen-dotPositionInString) * 2 ) ;
      }

      return 0 ;
   }
#endif /* S4WIDECHAR */



int u4nameExtFindExtensionPos( const char *name )
{
   // this function returns the position within the string of the '.' character indicating
   // the file extension position.  Returned is the position of this value within the string.
   // if no such position exists, the string length is returned.

   int extPos = c4strlen( name ) ;

   if ( extPos != 0 )
   {
      // go backwards through the string, until the first '.' character (we are at the
      // extension).  The cases of meeting a '\' or the beginning of the string mean that
      // no '.' was found, so there is no current extension (means append at current location).
      for ( int onPos = extPos - 1 ;; onPos-- )
      {
         if ( name[onPos] == '.' )  /* LY 99/5/20 : replaced g5_dot with '.' */
         {
            extPos = onPos ;
            break ;
         }

         if ( name[onPos] == '\\' )  /* LY 99/5/20 : replaced g5_backSlash with '\\' */
            break ;

         if ( onPos == 0 )
           break ;
      }
   }

   return extPos ;
}


int S4FUNCTION u4nameRemoveGivenExtension( char *name, const char *extensionToRemove )
{
   // this function is used to remove the input extension if it exists.  For example, you
   // can remove the '.DBF' extensions from the file name while leaving all others intact.

   // Note that this function is case sensitive, so ensure that the inputs have been correctly
   // cased.

   /* u4nameExt() for wide strings */
   char *ptr = name + c4strlen( name ) - 1 ;

   // trim off any blanks at the end of the file name
   while ( *ptr == L' ' )
   {
      *ptr = L'\0' ;
      ptr-- ;
   }

   int fileNameLen = c4strlen( name ) ;
   int extPos = u4nameExtFindExtensionPos( name ) ;  // get the position of the '.' character (or where it should go)

   // if there is no extension, just return...
   if ( fileNameLen == extPos )
      return 0 ;

   int dotPositionInString = extPos ;

   // the extPos will position us to the '.' character.  Adjust for input containing or not
   // containing this...
   /* LY 99/5/20 : replaced g5_dot with '.' */
   if ( *extensionToRemove != '.' )  // not included, increment extPos...
      extPos++ ;

   // AS 01/19/01 - For WIN32, make this case insensitive...
   #ifdef S4WIN32
      if ( c4stricmp( &name[extPos], extensionToRemove ) == 0 )  // matches, so remove...
   #else
      if ( c4strcmp( &name[extPos], extensionToRemove ) == 0 )  // matches, so remove...
   #endif
   {
      // clear out the extension characters in addition to the '.' character...
      c4memset( &name[dotPositionInString], 0, fileNameLen - dotPositionInString ) ;
   }

   return 0 ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION u4nameExt( char *name, int lenResult, const char *extensionToAdd, const int doReplace )
{
   int fileNameLen, extPos, onPos ;
   char *ptr ;
   #ifdef E4MISC
      int extLen ;
   #endif

   ptr = name + (int)c4strlen( name ) - 1 ;

   // trim off any blanks at the end of the file name
   while ( *ptr == ' ' )
   {
      *ptr = '\0' ;
      ptr-- ;
   }

   extPos = fileNameLen = c4strlen( name ) ;

   if ( extPos != 0 )
      for( onPos = extPos-1 ;; onPos-- )
      {
         if ( name[onPos] == '.' )
         {
            extPos = onPos ;
            break ;
         }
         if ( name[onPos] == S4DIR )
            break ;
         if ( onPos == 0 )
           break ;
      }

   if ( fileNameLen != extPos &&  !doReplace )
      return 0 ;

   if ( *extensionToAdd == '.' )
      extensionToAdd++ ;

   #ifdef E4MISC
      extLen = c4strlen( extensionToAdd ) ;
      if ( extLen > 3 )
         extLen = 3 ;
      if ( lenResult <= extPos + extLen + 1 )
         return error4( 0, e4result, E94507 ) ;
   #endif

   name[extPos++] = '.' ;
   c4strcpy( name + extPos, extensionToAdd ) ;

   return 0 ;
}



static int u4nameFix( char *buf )
{
   /* takes the input buffer and removes any "..\" or ".\"pieces */
   int i, j, len, l2 ;

   len = c4strlen( buf ) ;

   for( i = 0 ; i < len - 2 ; i++ )
   {
      #ifdef S4UNIX
         if ( c4memcmp(buf+i, "../", 3 ) == 0 )
      #else
         if ( c4memcmp( buf + i, "..\\", 3 ) == 0 )
      #endif
      {
         len -= 3 ;
         c4memmove( buf+i, buf+i+3, (unsigned int)(len - i) ) ;
         if ( i >= 2 )   /* need to remove previous path part too */
         {
            if ( buf[i-1] != S4DIR )
               return error4( 0, e4name, E94510 ) ;
            for( j = i-2 ; j > 0 ; j-- )
            {
               if ( buf[j] == S4DIR )
               {
                  c4memmove( buf+j+1, buf+i, (unsigned int)(len - i) ) ;
                  l2 = i - j - 1 ;
                  len -= l2 ;
                  i -= l2 ;
                  break ;
               }
            }
         }
         i-- ;  /* compensate for position */
      }
   }

   for( i = 0 ; i < len - 1 ; i++ )
   {
   #ifdef S4UNIX
      if ( c4memcmp( buf + i, "./", 2 ) == 0 )
   #else
      if ( c4memcmp( buf + i, ".\\", 2 ) == 0 )
   #endif
      {
         len -= 2 ;
         c4memmove( buf+i, buf+i+2, (unsigned int)(len - i) ) ;
         i-- ;  /* compensate for position */
      }
   }

   buf[len] = 0 ;

   return 0 ;
}



int S4FUNCTION u4nameCreateMultiDirectories( char *outName, unsigned int outNameLen, const char *inputName, const char *inputDir1, const char *inputDir2 )
{
   /* - create a name using 2 directory inputs
      - the first input directory takes precedence over the second input directory...
      - the input directories can be file names, only the path portions are used.  If they are
        complete directories, trailing back-slashes must exist.
   */
   assert5( outName != 0 && inputName != 0 ) ;

   // easy way to do this function is to use inputDir1, then recursively call ourselves with a blank
   // second directory...

   if ( inputDir1 == 0 )
   {
      // no first directory, just copy input name...
      if ( c4strlen( inputName ) > outNameLen )
         return -1 ;
      c4strcpy( outName, inputName ) ;
   }
   else
   {
      // do this directory first...
      Bool5 includePath = 1 ;
      if ( inputName[0] == '\\' )  // if machine name case or root path, don't need to consider path
         includePath = 0 ;
      else
      {
         if ( inputName[1] == ':' )  // don't bother if a drive is included (overrides path)
            includePath = 0 ;
      }

      if ( includePath )
      {
         u4namePath( outName, outNameLen, inputDir1 ) ;
         if ( c4strlen( outName + c4strlen( inputName ) ) > outNameLen )
            return -1 ;
         c4strcat( outName, inputName ) ;
      }
      else
      {
         if ( c4strlen( inputName ) > outNameLen )
            return -1 ;
         c4strcpy( outName, inputName ) ;
      }
   }

   if ( inputDir2 != 0 )  // recursively call for 2nd directory...
   {
      char nameBuf[LEN4PATH] ;
      if ( u4nameCreateMultiDirectories( nameBuf, outNameLen, outName, inputDir2, 0 ) != 0 )
         return -1 ;
      if ( c4strlen( nameBuf ) > outNameLen )
         return -1 ;
      c4strcpy( outName, nameBuf ) ;
   }

   u4nameFix( outName ) ;

   return 0 ;
}



#ifndef S4WINTEL
   #if defined( S4MACINTOSH ) || defined( S4PALM )
      int S4FUNCTION u4nameCurrentExtended( char *buf, const int bufLen, const char *name, const char *path )
      {
         c4strcpy(buf, name) ;
         return 0 ;
      }
      int S4FUNCTION u4nameCurrent( char *buf, const int bufLen, const char *name )
      {
         c4strcpy(buf, name) ;
         return 0 ;
      }
   #else
      int S4FUNCTION u4nameCurrentExtended( char *buf, const int bufLen, const char *name, const char *path )
      {
         assert5 ( buf != 0 && name != 0 && path != 0 ) ;
         assert5 ( c4strlen( name ) < LEN4PATH ) ; // assure a reasonable size
         assert5 ( c4strlen( path ) < LEN4PATH ) ; // assure a reasonable size

         c4strcpy( buf, "" ) ;

         size_t nameLen = c4strlen( name ) ;

         if ( name[0] != S4DIR )  /* full path */
         {
            char tempPath[LEN4PATH];
            int pathLen = u4namePath( tempPath, LEN4PATH, path ) ;
            if ( pathLen < 0 )
               return pathLen ;

            if ( tempPath[0] != S4DIR )   /* full path */
            {
               if ( getcwd( buf, bufLen ) == NULL )
                  return error4( 0, e4parm, E94509 ) ;
               size_t cwdLen = c4strlen( buf ) ;
               buf[cwdLen] = S4DIR ;
               buf[cwdLen + 1] = 0 ;
            }

            if ( c4strlen( buf ) + pathLen + nameLen >= bufLen )
               return error4( 0, e4parm, E94509 ) ;
            if ( pathLen > 0 )
               c4strcat( buf, tempPath ) ;
         }

         if ( c4strlen( buf ) + nameLen > bufLen )
            return error4 ( 0, e4parm, E94509 ) ;
         c4strcat( buf, name ) ;

         return u4nameFix( buf ) ;
      }

      /* take the input file name and add the current drive and path if required */
      int S4FUNCTION u4nameCurrent( char *buf, const int bufLen, const char *name )
      {
         int namePos, len, len2 ;

         #ifdef E4PARM_LOW
            if ( buf == 0 || name == 0 )
               return error4( 0, e4parm_null, E94509 ) ;
         #endif

         if ( name[0] == S4DIR )  /* full path */
         {
            len = c4strlen( name ) ;
            if ( len+1 > bufLen )
               return error4( 0, e4parm, E94509 ) ;
            c4memcpy( buf, name, (unsigned int)len ) ;
            buf[len] = 0 ;
            return u4nameFix( buf ) ;
         }

         namePos = 0 ;
         #ifndef S4MACINTOSH
            if ( getcwd( buf, bufLen ) == 0 )
         #else
            if ( u4getPath( buf, bufLen ) == 0 )
         #endif
            return error4( 0, e4parm, E94509 ) ;

         len2 = c4strlen( buf ) ;
         len = c4strlen( name ) ;
         if ( len > 2 )
            for ( ;; )
            {
               #ifndef S4MACINTOSH
                  if ( c4memcmp( name + namePos, "../", 3 ) != 0 )
               #else
                  if ( c4memcmp( name + namePos, "..:", 3 ) != 0 )
               #endif
                  break;
               /* must remove part of the current path */
               if ( len2 > 2 )
                  len2-- ;
               for ( ;; )
               {
                  if ( len2 == 2 )
                     break ;
                  len2-- ;
                  if ( buf[len2] == S4DIR )
                     break ;
               }
               namePos += 3 ;
            }

         if ( buf[len2-1] != S4DIR )  /* need to add the backslash */
         {
            if ( len2 + 1 >= bufLen )
               return error4( 0, e4parm, E94509 ) ;
            buf[len2] = S4DIR ;
            len2++ ;
         }
         len -= namePos ;
         if ( len + 1 > bufLen - len2 )
            return error4( 0, e4parm, E94509 ) ;
         c4memcpy( buf + len2, name + namePos, (unsigned int)len ) ;
         buf[len2 + len] = 0 ;
         return u4nameFix( buf ) ;
      }
   #endif /* !S4MACINTOSH*/
#else
   #ifdef S4WINCE
      int S4FUNCTION u4nameCurrent( char *buf, const int bufLen, const char *name )
      {
         c4strcpy(buf, name) ;
         return 0 ;
      }
   #else
      int S4FUNCTION u4nameCurrentExtended( char *buf, const int bufLen, const char *name, const char *path )
      {
         /* similar to u4nameCurrent, but takes an additional path paramater as well.
            Used for ole-db when we need to combine all of:  current directory, catalog
            path (from properties), and input name

            PARAMATERS

            name - null ended string containing the name of the file.  This may include
                   partial or complete path information.
            path - null ended string containing an additional path to include with
                   the name.  This path may be a complete file name, the file name and
                   extension are just ignored and only the path is used.

            NOTES

            Creates name as follows:

            combines (path) and (name) to produce a new name which is then combined
            with the current path to create the final name.

            Note that path information may be included in the 'name' paramater as
            well.

            Basically, path information in 'name' may override path information from
            'path' and the current directory.  path information from 'path' can
            only override path information from the current directory.

            Thus:
            name = "c:\file" means that path and current directory have no effect
            name - "\file", path = "c:\" means final "c:\file\", curent directory has no effect.

            NOTE, the output of this function will contain a trailing backslash on the path
         */

         assert5 ( buf != 0 && name != 0 && path != 0 ) ;
         assert5 ( c4strlen( name ) < LEN4PATH ) ; // assure a reasonable size
         assert5 ( c4strlen( path ) < LEN4PATH ) ; // assure a reasonable size

         char nameBuf[LEN4PATH] ;
         char includePath = 1 ;
         int nLen = c4strlen( name ) ;

         if ( nLen == 0 )
         {
            // AS 11/24/98 if name is empty, should still use input path to return a current directory which has been modified
            // by the input path if available.
            u4namePath( nameBuf, sizeof( nameBuf ), path ) ;
         }
         else
         {
            /* first combine path and name, then just call u4nameCurrent() */
            if ( name[0] == '\\' )  // if machine name case or root path, don't need to consider path
               includePath = 0 ;
            else
            {
               if ( name[1] == ':' )  // don't bother if a drive is included (overrides path)
                  includePath = 0 ;
            }

            if ( includePath )
            {
               u4namePath( nameBuf, sizeof( nameBuf ), path ) ;
               int len = c4strlen( nameBuf ) ;
               if ( len != 0 )
               {
                  if ( nameBuf[len-1] != '\\' )  // ensure the '\' is included on path
                     strcat( nameBuf, "\\" ) ;
               }
               strcat( nameBuf, name ) ;
            }
            else
               c4strcpy( nameBuf, name ) ;
         }

         return u4nameCurrent( buf, bufLen, nameBuf ) ;
      }



      int S4FUNCTION u4nameCurrent( char *buf, const int bufLen, const char *name )
      {
         /* take the input file name and add the current drive and path if required */
         int namePos, len = 0, len2 = 0, i, isMachineName, lx ;
         unsigned int driveNo ;
         #ifdef __WATCOMC__
            unsigned origDrive, curDrive, dummy;
         #endif

         #ifdef E4PARM_LOW
            if ( buf == 0 || name == 0 )
               return error4( 0, e4parm_null, E94509 ) ;
         #endif

         /* also must consider machine name accesses (eg. "\\BARNEY\TEST...") */

         Bool5 noName ;
         if ( c4strlen( name ) == 0 )
            noName = 1 ;
         else
            noName = 0 ;

         if ( noName == 0 && name[0] == '\\' && name[1] == '\\' )
         {
            for ( namePos = 2 ;; namePos++ )
               if ( name[namePos] == '\\' || name[namePos] == 0 )
                  break ;
            namePos++ ;
            c4memcpy( buf, name, namePos ) ;
            buf[namePos] = 0 ;
            isMachineName = 1 ;
         }
         else
         {
            isMachineName = 0 ;
            #ifndef S4WINCE
               if ( noName == 1 || name[1] != ':' )   /* must get the default drive */
               {
                  #ifdef S4WIN32
                     #ifdef __BORLANDC__
                        driveNo = getdisk() + 1 ;  /* get disk returns one less than get drive */
                     #else
                        driveNo = _getdrive() ;
                     #endif
                  #else
                     #ifdef S4OS2
                        driveNo = _getdrive() ;
                     #else
                        _dos_getdrive( &driveNo ) ;
                     #endif
                  #endif
                  if ( !driveNo || driveNo > 26 )   // means the current directory is on a machine name (eg. \\server\c\dbf is current directory)
                  {
                     #ifdef S4WINCE
                        if (GetCurrentDirectory(buf + 2, bufLen - 2) == 0)
                     #else
                        #ifdef _MSC_VER
                           if ( _getcwd( buf, bufLen) == 0 )
                        #else
                           if ( getcwd( buf, bufLen) == 0 )
                        #endif
                     #endif
                           return error4( 0, e4parm, E94509 ) ;
                     namePos=0;
                     isMachineName = 1 ;
                  }
                  else
                  {
                     buf[0] = 'A' + (char)(driveNo - 1) ;
                     buf[1] = ':' ;
                     if ( noName == 0 && name[0] == '\\' )  /* just append the path */
                     {
                        len = c4strlen( name ) ;
                        if ( len + 3 > bufLen )
                           return error4( 0, e4parm, E94509 ) ;
                        c4memcpy( buf + 2, name, (unsigned int)len ) ;
                        buf[len + 2] = 0 ;
                        return u4nameFix( buf ) ;
                     }
                     namePos = 0 ;
                  }
               }
               else
               {
            #endif
               if ( name[2] == '\\' )  /* have the full path, so done */
               {
                  len = c4strlen( name ) ;
                  if ( len + 1 > bufLen )
                     return error4( 0, e4parm, E94509 ) ;
                  c4memcpy( buf, name, (unsigned int)len ) ;
                  buf[len] = 0 ;
                  return u4nameFix( buf ) ;
               }
            #ifndef S4WINCE
                  c4memcpy( buf, name, 2 ) ;  /* get the drive */
                  namePos = 2 ;
            }
            #endif

         }

         if ( isMachineName == 0 )
         {
            /* get the current path and add it to buf */
            buf[0] = toupper( buf[0] ) ;

            #ifdef __WATCOMC__
               _dos_getdrive(&origDrive);
               _dos_setdrive(buf[0]-'A'+1, &dummy);
               _dos_getdrive(&curDrive);
               if (curDrive != buf[0]-'A'+1)
               {
                  _dos_setdrive(origDrive, &dummy);
                  return error4( 0, e4parm, E94509 ) ;
               }
               if (getcwd(buf + 2, bufLen - 2) == 0)
               {
                  _dos_setdrive(origDrive, &dummy);
                  return error4( 0, e4parm, E94509 ) ;
               }
               _dos_setdrive(origDrive, &dummy);
               _dos_getdrive(&curDrive);
               if (curDrive != origDrive)
                  return error4( 0, e4parm, E94509 ) ;
            #else
               #ifdef S4WINCE
                  if (GetCurrentDirectory(buf + 2, bufLen - 2) == 0)
               #else
                  if ( _getdcwd( buf[0] - 'A' + 1, buf + 2, bufLen - 2 ) == 0 )
               #endif
                     return error4( 0, e4parm, E94509 ) ;
            #endif

            lx = c4strlen( buf ) ;

            /* AS 04/16/99 Novell Servers return blanks after the directory name, trim these off here... */
            while( lx > 0 )
            {
               if ( buf[lx-1] != ' ' )
                  break ;
               // trim space...
               buf[lx-1] = 0 ;
               lx-- ;
            }


            if (buf[3] == ':')
               for ( i = 2 ; i <= lx ; i++ )
                  buf[i-2] = buf[i];
         }

         len2 = c4strlen( buf ) ;
         if ( noName == 0 )
         {
            for ( ;; )
            {
               if ( c4memcmp( name + namePos, "..\\", 3 ) != 0 )
                  break;
               /* must remove part of the current path */
               if (isMachineName==1)
               {
                  for (; ; )
                     if ( buf[--len2] == '\\' )
                        break ;
               }
               else
               {
                  if ( len2 > 2 )
                     len2-- ;
                  for ( ;; )
                  {
                     if ( len2 == 2 )
                        break ;
                     len2-- ;
                     if ( buf[len2] == '\\' )
                        break ;
                  }
               }
               namePos += 3 ;
            }
            if ( name[0] != '\\' && buf[len2-1] != '\\' )  /* need to add the backslash */
            {
               if ( len2 + 1 >= bufLen )
                  return error4( 0, e4parm, E94509 ) ;
               buf[len2] = '\\' ;
               len2++ ;
            }
            len = c4strlen( name + namePos ) ;
            if ( len + 1 > bufLen - len2 )
               return error4( 0, e4parm, E94509 ) ;
            c4memcpy( buf + len2, name + namePos, (unsigned int)len ) ;
         }
         buf[len2 + len] = 0 ;
         return u4nameFix( buf ) ;
      }
   #endif  /*S4WINCE */
#endif  /*S4WINTEL */

void S4FUNCTION u4nameMakeFindDrive( char *buf, const int bufLen, const char *defaultDirectory, const char *fileName )
{
   char drive[3] ;
   char *drivePtr = 0 ;

   if ( c4strlen( fileName ) > 1 && ( fileName[1] == ':' || (fileName[0] == '\\' && fileName[1] == '\\') ) )  /* full path for file name */
      defaultDirectory = 0 ;
   else
   {
      if ( defaultDirectory[1] == ':' )
      {
         drive[0] = defaultDirectory[0] ;
         drive[1] = ':' ;
         drive[2] = 0 ;
         drivePtr = drive ;
         defaultDirectory += 2 ;
      }
   }
   u4nameMake( buf, bufLen, drivePtr, defaultDirectory, fileName ) ;
}

void u4nameMake( char *buf, const int bufLen, const char *defaultDrive, const char *defaultDirectory, const char *fileName )
{
   int defaultDirectoryLen, pos = 0 ;
   int needsDrive ;

   if ( c4strlen( fileName ) < 2 )
      needsDrive = 1 ;
   else
   {
      if ( fileName[1] != ':' )
      {
         if ( fileName [0] == '\\' && fileName [1] == '\\' )
            needsDrive = 0 ;
         else
            needsDrive = 1 ;
      }
      else
         needsDrive = 0 ;
   }

   if ( needsDrive )
      if ( defaultDrive != 0 )
         if ( c4strlen( defaultDrive ) == 2 )
         {
            c4memcpy( buf, defaultDrive, 2 ) ;
            pos += 2 ;
         }

   if ( defaultDirectory != 0 )
      defaultDirectoryLen = c4strlen( defaultDirectory ) ;
   else
      defaultDirectoryLen = 0 ;

   if ( fileName[0] != S4DIR  &&  defaultDirectoryLen > 0 )
   {
      if ( pos+2 >= bufLen )
         return ;
      buf[pos++] = S4DIR ;
      if ( defaultDirectory[0] == S4DIR )
         defaultDirectory++ ;

      defaultDirectoryLen = c4strlen(defaultDirectory) ;

      u4ncpy( buf+pos, defaultDirectory, (unsigned int)(bufLen - pos) ) ;
      pos += defaultDirectoryLen ;
   }

   if ( pos >= bufLen )
      return ;

   if ( pos > 0 )
   {
      if ( buf[pos-1] != S4DIR )
         buf[pos++] = S4DIR ;
      if ( fileName[0] == S4DIR  )
         fileName++ ;
   }

   u4ncpy( buf+pos, fileName, (unsigned int)(bufLen - pos ) ) ;
}

#ifdef P4ARGSUSED
   #pragma argsused
#endif
int S4FUNCTION u4namePiece( char *result, const unsigned int lenResult, const char *from, const int givePath, const int giveExt )
{
   unsigned namePos, extPos, onPos, pos, newLen, fromLen ;
   int arePastExt ;

   namePos = 0 ;
   arePastExt = 0 ;
   extPos = fromLen = c4strlen(from) ;
   if ( extPos == 0 )
   {
      *result = 0 ;
      return 0 ;
   }

   for( onPos = extPos-1;; onPos-- )
   {
      switch ( from[onPos] )
      {
         #ifndef S4MACINTOSH
            case S4DIR:
         #endif
         case ':':
            if ( namePos == 0 )
               namePos = onPos + 1 ;
            arePastExt = 1 ;
            break ;
         case '.':
            if ( !arePastExt )
            {
               extPos = onPos ;
               arePastExt = 1 ;
            }
            break ;
         default:
            break ;
      }

      if ( onPos == 0 )
         break ;
   }

   pos = 0 ;
   newLen = fromLen ;
   if ( !givePath )
   {
      pos = namePos ;
      newLen -= namePos ;
   }

   if ( !giveExt )
      newLen -= fromLen - extPos ;

   if ( newLen >= (unsigned) lenResult )
      newLen = lenResult - 1 ;

   c4memcpy( result, from+ pos, newLen ) ;
   result[newLen] = 0 ;

   return 0 ;
}



void S4FUNCTION u4trim( char *stringToTrim )
{
   // trims blanks off the end of the string...
   if ( stringToTrim == 0 )
      return ;

   long len = c4strlen( stringToTrim ) ;
   while ( len > 0 )
   {
      len-- ;
      if ( stringToTrim[len] == ' ' )
         stringToTrim[len] = 0 ;
      else
         break ;
   }
}



int S4FUNCTION u4nameChar( unsigned char ch)
{
   /* u4nameChar  Returns TRUE iff it is a valid dBase field or function name character */
   return ( ((ch>='a') && (ch<='z'))  || ((ch>='A') && (ch<='Z'))  ||
      #ifdef S4MDX
         ((ch>='0') && (ch<='9'))  || ch=='&' || ch=='@' ||
      #else
         ch>='0' && ch<='9'  ||
      #endif
         ch=='_'
/*            ch=='\\'  ||  ch=='.'  || ch=='_'  ||  ch==':' */
   #ifdef S4GERMAN
      #ifdef S4ANSI
         || ch== 196  ||  ch== 214  || ch== 220  ||  ch== 223
         || ch== 228  ||  ch== 246  || ch== 252
      #else
         || ch== 129  ||  ch== 132  || ch== 142  ||  ch== 148
         || ch== 153  ||  ch== 154  || ch== 225
      #endif
   #endif
   #ifdef S4FRENCH
      #ifdef S4ANSI
         || ch== 192 || ch== 194 || ch== 206 || ch== 207
         || ch== 212 || ch== 219 || ch== 224 || ch== 226
         || ch== 238 || ch== 239 || ch== 244 || ch== 251
         || (ch>= 199 && ch <= 203) || (ch >= 231 && ch <= 235)
      #else
         || ch== 128 || ch== 130 || ch== 131 || ch== 133
         || ch== 144 || ch== 147 || ch== 150 || (ch>= 135 && ch <= 140)
      #endif
   #endif
   /* LY 00/07/17 : removed extra ) when S4ANSI and S4*** def'd */
   #ifdef S4SWEDISH
      #ifdef S4ANSI
         || ch== 196 || ch== 197 || ch== 198 || ch== 201
         || ch== 214 || ch== 220 || ch== 228 || ch== 229
         || ch== 230 || ch== 233 || ch== 246 || ch== 252
      #else
         || ch== 129 || ch== 130 || ch== 132 || ch== 134
         || ch== 148 || ch== 153 || ch== 154 || (ch>= 142 && ch <= 146)
      #endif
   #endif
   #ifdef S4FINNISH
      #ifdef S4ANSI
         || ch== 196 || ch== 197 || ch== 198 || ch== 201
         || ch== 214 || ch== 220 || ch== 228 || ch== 229
         || ch== 230 || ch== 233 || ch== 246 || ch== 252
      #else
         || ch== 129 || ch== 130 || ch== 132 || ch== 134
         || ch== 148 || ch== 153 || ch== 154 || (ch>= 142 && ch <= 146)
      #endif
   #endif
   #ifdef S4NORWEGIAN
      #ifdef S4ANSI
         || ch== 196 || ch== 197 || ch== 198 || ch== 201
         || ch== 214 || ch== 220 || ch== 228 || ch== 229
         || ch== 230 || ch== 233 || ch== 246 || ch== 252
      #else
         || ch== 129 || ch== 130 || ch== 132 || ch== 134
         || ch== 148 || ch== 153 || ch== 154 || (ch>= 142 && ch <= 146)
      #endif
   #endif
         ) ;
}



const char * S4FUNCTION u4nameFindFileName( const char *fullName, int lenPath )
{
   // return pointer to file name part given path length.  Compensate for backslash if required
   // (i.e. backslash not included).
   if ( fullName[lenPath] == '\\' )
      lenPath++ ;
   return fullName + lenPath ;
}



int S4FUNCTION u4namePathSpecial( char *path, const unsigned int lenResult, const char *from )
{
   // same as u4namePath but no '\'  - for OLE-DB
   int len ;
   len = u4namePath( path, lenResult, from ) ;  // ensure we get the low-level complete file name

   if ( len >= 2 )
      if ( path[len-1] == '\\' )
      {
         path[len-1] = 0 ;
         len-- ;
      }

   return len ;
}

/* returns the length of the path in 'from', and copies the path in 'from' to result */
int S4FUNCTION u4namePath( char *result, const unsigned int lenResult, const char *from )
{
   long onPos ;

   u4namePiece( result, lenResult, from, 1, 0 ) ;
   for( onPos = 0 ; result[onPos] != 0 ; onPos++ ) ;

   for( ; onPos >= 0 ; onPos-- )
      if( result[onPos] == S4DIR || result[onPos] == ':' ) break ;   /* end of path */

   if( onPos < (long)lenResult )
      result[++onPos] = '\0' ;
   return (int)onPos ;
}



int S4FUNCTION u4namecmp( const char *string1, const char *string2, short ignoreCase )
{
   /* Do a filename compare on two strings.
      If built as client, ignoreCase is used to determine if the
         comparison is to be case sensitive or not.
      In STAND_ALONE, ignoreCase is not used. Instead, S4CASE_SEN
         is used to determine if the comparison is to be case sensitive or not.
   */
   int rc ;
   // AS 06/12/00 - appears to differ from contents, fixed.
   // #ifdef S4CLIENT
   #ifdef S4STAND_ALONE
      #ifdef S4CASE_SEN
         rc = c4strcmp( string1, string2 ) ;
      #else
         #ifdef S4UNIX
            rc = strcasecmp( string1, string2 ) ;
         #else
            #ifdef S4WINCE
               rc = c4stricmp( (char*)string1, (char*)string2 ) ;
            #else
               rc = c4stricmp( string1, string2 ) ;
            #endif
         #endif
      #endif
   #else
      if ( ignoreCase )
      {
         #ifdef S4UNIX
            rc = strcasecmp( string1, string2 ) ;
         #else
            rc = c4stricmp( string1, string2 ) ;
         #endif
      }
      else
         rc = c4strcmp( string1, string2 ) ;
   #endif
   return rc ;
}



int S4FUNCTION u4namencmp( const char *string1, const char *string2, size_t count, short ignoreCase )
{
   /* Do a filename compare on two strings.
      If built as client, ignoreCase is used to determine if the
         comparison is to be case sensitive or not.
      In STAND_ALONE, ignoreCase is not used. Instead, S4CASE_SEN
         is used to determine if the comparison is to be case sensitive or not.
      count is the number of characters to compare.
   */
   int rc ;
   #ifdef S4CLIENT
      #ifdef S4CASE_SEN
         rc = strncmp( string1, string2, count ) ;
      #else
         #ifdef S4UNIX
            rc = strncasecmp( string1, string2, count ) ;
         #else
            rc = strnicmp( string1, string2, count ) ;
         #endif
      #endif
   #else
      if ( ignoreCase )
      {
         #ifdef S4UNIX
            rc = strncasecmp( string1, string2, count ) ;
         #else
            #ifdef S4NO_STRNICMP
               rc = strnicmp( (char*)string1, (char*)string2, count ) ;
            #else
               rc = c4strnicmp( string1, string2, count ) ;
            #endif
         #endif
      }
      else
         rc = c4strncmp( string1, string2, count ) ;
   #endif
   return rc ;
}



/* returns the length of the extension in 'from', and copies the extension in 'from' to 'result' */
int u4nameRetExt( char *result, const int lenIn, const char *from )
{
   char len, name[LEN4PATH+1] ;
   int onPos ;

   int lenResult = lenIn ;
   c4memset( result, 0, lenIn ) ;

   #ifdef E4PARM_HIGH
      if ( result == 0 || lenResult < 3 || from == 0 )
         return error4( 0, e4parm, E94506 ) ;
   #endif

   // use u4namePiece to remove any path info such as "d:\.\..\", don't want to consider those '.'
   // characters as part of the extension.
   u4namePiece( name, LEN4PATH, from, 0, 1 ) ;

   // AS 01/13/99 --> should move backwards through string, not forwards.  For example, names like
   // "a.b.c" return extension of 'b.c' instead of 'c' as expected.
   len = 0 ;
   for( onPos = c4strlen( name ) - 1 ;  onPos >= 0 ; onPos-- )
   {
      if ( name[onPos] == '.' )
      {
         for ( onPos++ ; name[onPos] != 0 && lenResult-- > 0 ; onPos++, len++ )
            result[len] = name[onPos] ;
         break ;
      }
   }

   return len ;
}

#ifdef S4MACINTOSH
   char *u4getMacPath(CODE4 *c4, char *buf, int buflen )
   {
      int count;
      CInfoPBRec  myPB ;
      Str32 dirName ;
      char *dirNameC;
      char temp[258] ;

      buf[0] = '\0' ;
      count = 0 ;
      myPB.dirInfo.ioNamePtr = (StringPtr)&dirName ;
      myPB.dirInfo.ioVRefNum = c4->macVol ;
      myPB.dirInfo.ioDrParID = c4->macDir ;
      myPB.dirInfo.ioFDirIndex = -1 ;
      do
      {
         myPB.dirInfo.ioDrDirID = myPB.dirInfo.ioDrParID ;
         PBGetCatInfoSync(&myPB) ;
         dirNameC = p2cstr(dirName ) ;
         strcat(dirNameC, ":");
         count += c4strlen(dirNameC);
         if (count > buflen)
            return 0 ;
         c4strcpy(temp, dirNameC ) ;
         strcat(temp, buf ) ;
         c4strcpy(buf, temp ) ;
       }
       while  (myPB.dirInfo.ioDrDirID != fsRtDirID);
       return buf ;
   }
#endif


#ifdef S4SERVER
   // currently only used in server, so only define there
   int S4FUNCTION u4pathSet( const char *defPath )
   {
      // returns -2 if cannot set drive, -1 if cannot set path
      #ifndef S4UNIX
         // change drive if required...
         if ( defPath[1] == ':' )
         {
            int driveLetter = toupper( defPath[0] ) ;  // ensure upper case so subtract from 'A' works
            unsigned int drv1 = (unsigned int)( driveLetter - 'A' + 1 ) ;
            unsigned int drv2 ;
            #ifdef S4WIN32
               _chdrive( drv1 ) ;
               drv2 = _getdrive() ;
            #else
               _dos_setdrive( drv1, &drv2 ) ;
               if ( drv2 < drv1 )
                  return -2 ;
               _dos_getdrive( &drv2 ) ;
            #endif
            if ( drv2 != drv1 )
               return -2 ;
         }
      #endif

      // change directory now that the correct drive is chosen
      if ( chdir( defPath ) != 0 )
         return -1 ;

      return 0 ;
   }
#endif
