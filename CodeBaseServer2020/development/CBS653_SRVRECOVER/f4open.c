/* f4open.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

#ifdef S4WINTEL
   #ifndef S4IBMOS2
      #ifndef S4OFF_MULTI
         #ifndef __TURBOC__
            #include <sys\locking.h>
            #define S4LOCKING
         #endif
         #ifdef _MSC_VER
            #include <sys\locking.h>
         #endif
      #endif
      #ifdef __ZTC__
         extern int  errno ;
      #endif
   #endif
   #ifndef S4WINCE
      #include <share.h>
   #endif
#endif /* S4WINTEL */

#if !defined( S4WINCE ) && !defined( S4PALM )
   #include <fcntl.h>
   #include <errno.h>
#endif

#ifdef S4TRACK_FILES_OR_SERVER
   extern unsigned int numFiles5 ;
   #ifdef S4TRACK_FILES
      extern int f4print ;
   #endif
#endif

#ifdef S4ERRNO
   extern int errno ;
#endif

/*
**  file4openLow() generic outline:
**
**  - clear any postive error values
**  - if CODE4 is requesting read-only open, then:
**    (a) mark file as read only in FILE4 structure
**    (b) set the physical file open flag to read only
**  - check file physically to see if file has a physical read-only attribute
**    (note that this is different than only allowed to have read-only access
**     for example on a network drive) --> in some configurations it is not
**    possible to make this check.  But if it is true:
**    (a) mark file as read only in FILE4 structure
**    (b) set the physical file open flag to read only
**    (c) mark the FILE4 as exclusive open (since nobody can write to it, can
**        treat it as such
**  - set the shared flag based on CODE4::accessMode
**    3 possible settings: OPEN4DENY_NONE, OPEN4DENY_WRITE, OPEN4DENY_RW.
**    return failure if no the setting is invalid.
**  - physically attempt the file open
**  - if the file open failed, try to determine the cause of failure and
**    return the appropriate error message.  e4open is returned if the
**    specific failure cannot be determined.
**
**  Returns: 0 = success, e4... = failure (appropriate error condition)
**
**  Notes:
**     This routine shouldn't generate a CodeBase error
*/

#ifdef S4SERVER
   unsigned short u4openFiles( void )
   {
      return numFiles5 ;
   }
#endif /* S4SERVER */


#ifdef S4MACINTOSH
   static int file4openLow( FILE4 *file, CODE4 *c4, const char *name )
   {
      short perm ;
      Str255 MACname ;
      HParamBlockRec MACfile ;
      OSErr err ;
      short tempHand ;

      file->hand = INVALID4HANDLE ;
      error4set( c4, 0 ) ;  /* clear positive error values */

      if ( c4getReadOnly( c4 ) )
      {
         file->isReadOnly = 1 ;
         perm = 0x0001 ;
      }
      else
         perm = 0x0003 ;

      memcpy( MACname, name, sizeof(MACname) ) ;
      /* LY 00/01/26 : S4CTOPSTR macro for newer CodeWarrior */
      S4CTOPSTR( (char *)MACname ) ;  /* convert C string to Pascal string */

      switch (c4->accessMode)
      {
         case OPEN4DENY_NONE:
            break ;
         case OPEN4DENY_WRITE:
            perm |= 0x0020 ;
            break ;
         case OPEN4DENY_RW:
            perm |= 0x0030 ;
            break ;
         default:
            return e4access ;
      }

      if( FSMakeFSSpec( c4->macVol, c4->macDir, MACname, &file->macSpec ) != noErr)
         return e4fileFind;

      MACfile.accessParam.ioCompletion = NULL ;
      MACfile.accessParam.ioNamePtr = file->macSpec.name ;
      MACfile.accessParam.ioVRefNum = file->macSpec.vRefNum ;
      MACfile.accessParam.ioDenyModes = perm ;
      MACfile.fileParam.ioDirID = file->macSpec.parID ;

      err=PBHOpenDenySync((HParmBlkPtr)&MACfile);

      switch (err)
      {
         case noErr:
            file->hand = MACfile.ioParam.ioRefNum ;
            return 0 ;
         case tmfoErr:
            return e4numFiles ;
         case fnfErr:
            return e4fileFind ;
         case fLckdErr:
         case vLckdErr:
         case permErr:
         case afpAccessDenied:
         case afpDenyConflict:
            return e4permiss ;
         case opWrErr:
            return e4instance;
         case paramErr:
            break ;
         default:
            return e4open;
      }
      /*If we get to this point, then it means that PBHOpenDenySync is not supported*/
      /*We now have to use functions with less control.*/
      /*Use fsWrPerm because range locking won't work anyway. Failures happpen if not exclusive open */

      err = FSpOpenDF(&file->macSpec, fsWrPerm, &tempHand ) ;

      switch (err)
      {
         case noErr:
            file->hand = tempHand ;
            return 0 ;
         case nsvErr:
         case bdNamErr:
         case fnfErr:
         case dirNFErr:
            return e4fileFind ;
         case tmfoErr:
            return e4numFiles ;
         case opWrErr:
         case permErr:
         case afpAccessDenied:
            return e4permiss ;
         case ioErr:
         default:
            return e4open;
      }
   }
#endif /* S4MACINTOSH */

#ifdef S4UNIX
   static int file4openLow( FILE4 *file, CODE4 *c4, const char *name )
   {
      int rc = 0 ;
      int oflag ;
      #ifndef S4OFF_MULTI
         struct flock lck;
         #ifdef S4MULTIC4
            int pid, status;
         #endif
         lck.l_whence = SEEK_SET ;
         lck.l_start = (off_t) 0 ;
         lck.l_len = (off_t) 1000000000 ;
      #endif

      error4set( c4, 0 ) ;  /* clear positive error values */

      if ( c4getReadOnly( c4 ) == 1 )
      {
         file->isReadOnly = 1 ;
         #if defined(S4FILE_EXTENDED) && !defined(S464BIT)  /* LY 00/04/05 */
            oflag = (int)(O_RDONLY | O_LARGEFILE) ;
         #else
            oflag = (int)O_RDONLY ;
         #endif
      }
      else
         #if defined(S4FILE_EXTENDED) && !defined(S464BIT)  /* LY 00/04/05 */
            oflag = (int)(O_RDWR | O_LARGEFILE) ;
         #else
            oflag = (int)O_RDWR ;
         #endif

      file->hand = open( name, oflag ) ;

      if ( file->hand == INVALID4HANDLE )
      {
         /* note:  getting error classification is required to allow for
            occasional recovery from exceeded file handle problems */
         switch( errno )  /* try to get some clarification on the error */
         {
            case EACCES:
               return e4permiss ;
            case EMFILE:   /* allow try closing excess files and retrying */
               return e4numFiles ;
            case ENOENT:
               return e4fileFind ;
            default:
               return e4open ;
         }
      }

      #ifndef S4OFF_MULTI
         switch ( c4->accessMode )
         {
            case OPEN4DENY_NONE:
            case OPEN4DENY_WRITE:
               lck.l_type = F_RDLCK ;
               break;
            case OPEN4DENY_RW:
               lck.l_type = F_WRLCK ;
               break;
            default:
               close(file->hand);
               file->hand = INVALID4HANDLE;
               return e4open;
         }
         #ifdef S4MULTIC4
            if ((pid = fork())<0)
               return error4(c4, e4lock, E80609);
            if (pid ==0)               /* Child fork() */
            {
               rc = fcntl (file->hand, F_SETLK, &lck ) ;
               exit(rc);
            }
            while (wait(&status)!=pid)    /* Parent fork() */
               ;
            if ((WIFEXITED(status)==0))   /* If it didn't exit properly */
               return error4(c4, e4lock, E80609);
            if ((rc=WEXITSTATUS(status)) == 0)   /*IF it was able to lock*/
            {
               rc = fcntl (file->hand, F_SETLK, &lck ) ;
            }
         #else
            rc = fcntl (file->hand, F_SETLK, &lck ) ;
         #endif  /* !S4MULTIC4 */

         if (rc!=0)
         {
            close(file->hand);
            file->hand = INVALID4HANDLE ;
            return e4permiss;  // CS 2000/07/14 someone else has exclusive lock
         }
      #endif
      return 0 ;
   }
#endif /* S4UNIX */

#ifdef S4PALM
   static int file4openLow( FILE4 *file, CODE4 *c4, const char *name )
   {
      int rc = 0 ;
      const UInt16 cardNo = 0;
      const UInt32 type = 0;
      const UInt32 creator = 0;
      UInt32 openMode = 0 ;
      Err err;
/*      #ifndef S4OFF_MULTI
         struct flock lck;
         #ifdef S4MULTIC4
            int pid, status;
         #endif
         lck.l_whence = SEEK_SET ;
         lck.l_start = (off_t) 0 ;
         lck.l_len = (off_t) 1000000000 ;
      #endif*/

      file->hand = INVALID4HANDLE;

      if ( file4exists( name ) == -1 )
         return e4fileFind ;

      error4set( c4, 0 ) ;  // clear positive error values

      openMode = fileModeUpdate;

      if ( c4getReadOnly( c4 ) )
      {
         file->isReadOnly = 1 ;
         openMode |= fileModeReadOnly;
      }

      #ifdef S4OFF_MULTI
         openMode |= fileModeExclusive;
      #else
         switch (c4getAccessMode(c4))
         {
            case OPEN4DENY_NONE:
               break;
            case OPEN4DENY_RW:
            case OPEN4DENY_WRITE:
               openMode |= fileModeExclusive;
               break;
            default:
               return e4open;
         }
      #endif

      file->hand = FileOpen( cardNo, (char *)name, type, creator, openMode, &err ) ;

      if ( file->hand == INVALID4HANDLE )
      {
         /* note:  getting error classification is required to allow for
            occasional recovery from exceeded file handle problems */
         switch( err )  /* try to get some clarification on the error */
         {
            case fileErrMemError:
               return e4memory;
            case fileErrInvalidParam:
               return e4parm;
            case fileErrCorruptFile:
            case fileErrNotStream:
               return e4read;
            case fileErrInUse:
               return e4permiss ;
            case fileErrReadOnly:
               return e4access;
            case fileErrNotFound:
               return e4fileFind;
            default:
               return e4open ;
         }
      }

      #ifndef S4OFF_MULTI
         // TO DO: This code will not compile if S4OFF_MULTI is not defined
         switch ( c4->accessMode )
         {
            case OPEN4DENY_NONE:
            case OPEN4DENY_WRITE:
               lck.l_type = F_RDLCK ;
               break;
            case OPEN4DENY_RW:
               lck.l_type = F_WRLCK ;
               break;
            default:
               close(file->hand);
               file->hand = INVALID4HANDLE;
               return e4open;
         }
         rc = fcntl (file->hand, F_SETLK, &lck ) ;

         if (rc!=0)
         {
            close(file->hand);
            file->hand = INVALID4HANDLE ;
            return e4permiss;  // CS 2000/07/14 someone else has exclusive lock
         }
      #endif
      return 0 ;
   }
#endif /* S4PALM */

#ifdef S4WIN32
   static int file4openLow( FILE4 *file, CODE4 *c4, const char *name )
   {
      DWORD fdwAccess, fdwShareMode, fAttributes, err ;
      #ifdef S4UNICODE
         unsigned short nameU[256] ;
      #endif

      error4set( c4, 0 ) ;  /* clear positive error values */
      #ifdef S4UNICODE
         c4atou(name, nameU, 256) ;
         fAttributes = GetFileAttributes( nameU ) ;
      #else
         fAttributes = GetFileAttributes( name ) ;
      #endif
      if ( fAttributes != 0xFFFFFFFF && fAttributes & FILE_ATTRIBUTE_READONLY )
      {
         file->isReadOnly = 1 ;
         fdwAccess = GENERIC_READ ;
         #ifndef S4OFF_MULTI
            file->lowAccessMode = OPEN4DENY_RW ;  /* for a file read-only can treat as exclusive */
         #endif
      }
      else
      {
         if ( c4getReadOnly( c4 ) == 1 )
         {
            file->isReadOnly = 1 ;
            fdwAccess = GENERIC_READ ;
         }
         else
            fdwAccess = GENERIC_WRITE | GENERIC_READ ;
      }

      #ifdef S4OFF_MULTI
         #ifdef E4DEBUG
            /* open in shared mode for debugging to allow other programs to examine */
            fdwShareMode = FILE_SHARE_WRITE | FILE_SHARE_READ ;
         #else
            /* otherwise, if single-user, file should be opened exclusively */
            fdwShareMode = 0 ;
         #endif
      #else
         switch( c4->accessMode )
         {
            case OPEN4DENY_NONE:
               fdwShareMode = FILE_SHARE_WRITE | FILE_SHARE_READ ;
               break ;
            case OPEN4DENY_WRITE:
               fdwShareMode = FILE_SHARE_READ ;
               break ;
            case OPEN4DENY_RW:
               fdwShareMode = 0 ;
               break ;
            default:
               file->hand = INVALID4HANDLE ;
               return e4open ;
         }
      #endif  /* S4OFF_MULTI */

      /* 07/21/99 Added new support... if CODE4.fileFlush is true, do not allow lazy flushing of
         writes.  This is useful in Windows 95/98 where file writes otherwise may get delayed
         forever.
      */

      DWORD flagsAndAttributes ;

      if ( c4->fileFlush )
         flagsAndAttributes = FILE_FLAG_WRITE_THROUGH ;
      else
         flagsAndAttributes = FILE_ATTRIBUTE_NORMAL ;

      #ifdef S4WINCE
         file->hand = CreateFile( nameU, fdwAccess, fdwShareMode, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
      #else
         file->hand = CreateFile( name, fdwAccess, fdwShareMode, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
      #endif
      if ( file->hand == INVALID4HANDLE )
      {
         err = GetLastError( ) ;
         switch ( err )
         {
            case ERROR_FILE_NOT_FOUND:
            case ERROR_PATH_NOT_FOUND:
            case ERROR_INVALID_DRIVE:
            case ERROR_INVALID_NAME:
            case ERROR_BAD_PATHNAME:
            case ERROR_DIRECTORY:
            case ERROR_NO_MEDIA_IN_DRIVE:
            case ERROR_INVALID_SHARENAME:
               return e4fileFind;
            case ERROR_TOO_MANY_OPEN_FILES:
               return e4numFiles;
            case ERROR_SHARING_VIOLATION:
            case ERROR_ACCESS_DENIED:
            case ERROR_DRIVE_LOCKED:
               return e4permiss;
            case ERROR_HANDLE_DISK_FULL:
            case ERROR_DISK_FULL:
               return e4write;
            default:
               return e4open ;
         }
      }

      #ifdef S4MULTI_THREAD
         InitializeCriticalSection( &file->critical4file ) ;
      #endif

      #ifdef S4ADVANCE_READ
         file->advanceReadBufStatus = AR4EMPTY ;
      #endif
      return 0 ;
   }
#endif /* S4WIN32 */

#ifdef S4WIN16
   static int file4openLow( FILE4 *file, CODE4 *c4, const char *name )
   {
      int oflag, sMode ;

      error4set( c4, 0 ) ;  /* clear positive error values */

      /* check the file's dos read only attrib -- which improves performance */
      if ( c4getReadOnly( c4 ) == 1 )
      {
         file->isReadOnly = 1 ;
         oflag = (int)OF_READ ;
      }
      else
         oflag = (int)OF_READWRITE ;

      #ifdef S4OFF_MULTI
         #ifdef E4DEBUG
            /* open in shared mode for debugging to allow other programs to examine */
            sMode = (int)OF_SHARE_DENY_NONE ;
         #else
            /* otherwise, if single-user, file should be opened exclusively */
            sMode = (int)OF_SHARE_EXCLUSIVE ;
         #endif
      #else
         switch( c4->accessMode )
         {
            case OPEN4DENY_NONE:
               sMode = (int)OF_SHARE_DENY_NONE ;
               break ;
            case OPEN4DENY_WRITE:
               sMode = (int)OF_SHARE_DENY_WRITE ;
               break ;
            case OPEN4DENY_RW:
               sMode = (int)OF_SHARE_EXCLUSIVE ;
               break ;
            default:
               file->hand = INVALID4HANDLE ;
               return e4open ;
         }
      #endif

      file->hand = _lopen( name, oflag | sMode ) ;
      if ( file->hand == INVALID4HANDLE )
      {
         /* note:  getting error classification is required to allow for
            occasional recovery from exceeded file handle problems */
         switch( errno )  /* try to get some clarification on the error */
         {
            case EACCES:
               return e4permiss ;
            #ifdef __TURBOC__
               case EINVACC:
                  return e4access ;
            #endif
            case EMFILE:   /* allow try closing excess files and retrying */
               return e4numFiles ;
            case ENOENT:
               return e4fileFind ;
            default:
               return e4open ;
         }
      }

      return 0 ;
   }
#endif /* S4WIN16 */

#if defined( S4WINTEL ) && !defined( S4WIN16 ) && !defined( S4WIN32 )
   #ifndef S4OS2
      #ifdef __TURBOC__
         #define S4USE_CHMOD
      #endif
      #ifdef _MSC_VER
         #define S4USE_CHMOD
      #endif
   #endif

   static int file4openLow( FILE4 *file, CODE4 *c4, const char *name )
   {
      #ifdef _MSC_VER
         #ifdef S4USE_CHMOD
            unsigned attribute ;
         #endif
      #endif
      int oflag, shflag ;

      error4set( c4, 0 ) ;  /* clear positive error values */

      /* check the file's dos read only attrib -- which improves performance */
      #ifdef S4USE_CHMOD
         errno = 0 ;
         #ifdef __TURBOC__
            if ( _A_RDONLY & _rtl_chmod( name, 0 ) )
         #endif
         #ifdef _MSC_VER
            _dos_getfileattr( name, &attribute ) ;
            if (_A_RDONLY & attribute )
         #endif
         {
            if ( errno == ENOENT )  /* file doesn't exist */
            {
               file->hand = INVALID4HANDLE ;
               if ( c4->errOpen )
                  return e4fileFind ;
            }
            file->isReadOnly = 1 ;
            #ifndef S4OFF_MULTI
               file->lowAccessMode = OPEN4DENY_RW ;  /* for a file read-only can treat as exclusive */
            #endif
            oflag = (int)(O_BINARY | O_RDONLY) ;
         }
         else
         {
      #endif
            if ( c4getReadOnly( c4 ) == 1 )
            {
               file->isReadOnly = 1 ;
               oflag = (int)(O_BINARY | O_RDONLY) ;
            }
            else
            {
               oflag = (int)(O_BINARY | O_RDWR) ;
            }
      #ifdef S4USE_CHMOD
         }
      #endif

      #ifdef S4OFF_MULTI
         shflag = SH_DENYRW ;
      #else
         switch ( c4->accessMode )
         {
            case OPEN4DENY_NONE:
               shflag = SH_DENYNO ;
               break ;
            case OPEN4DENY_WRITE:
               shflag = SH_DENYWR ;
               break ;
            case OPEN4DENY_RW:
               shflag = SH_DENYRW ;
               break ;
            default:
               file->hand = INVALID4HANDLE ;
               return e4open ;
         }
      #endif

      file->hand = sopen( name, oflag, shflag, 0 ) ;

      if ( file->hand == INVALID4HANDLE )
      {
         /* note:  getting error classification is required to allow for
            occasional recovery from exceeded file handle problems */
         switch( errno )  /* try to get some clarification on the error */
         {
            case EACCES:
               return e4permiss ;
            #ifdef __TURBOC__
               case EINVACC:
                  return e4access ;
            #endif
            case EMFILE:   /* allow try closing excess files and retrying */
               return e4numFiles ;
            case ENOENT:
               return e4fileFind ;
            default:
               return e4open ;
         }
      }

      return 0 ;
   }

   #ifdef S4USE_CHMOD
      #undef S4USE_CHMOD
   #endif
#endif /* S4WIN32 && !S4WIN16 && !S4WINTEL */

int S4FUNCTION file4open( FILE4 *file, CODE4 *c4, S4CONST char *name, const int doAlloc )
{
   int rc, len ;
   #ifdef S4FILE_EXTENDED
      FILE4LONG lrc ;
   #endif

   #ifdef E4PARM_HIGH
      if ( file == 0 || c4 == 0 || name == 0 )
         return error4( c4, e4parm, E90615 ) ;
   #endif

   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   #ifndef S4OPTIMIZE_OFF
      code4memStartMaxSet( c4, c4->memMaxPercent ) ;  /* start optimization if not enabled and not suspended */
   #endif

   c4memset( (void *)file, 0, sizeof( FILE4 ) ) ;
   file->codeBase = c4 ;
   file->hand = INVALID4HANDLE ;

   rc = file4openLow( file, c4, name ) ;

   #ifdef S4SERVER
      if ( rc != 0 )
      {
         if ( rc != 0 && rc != e4fileFind && rc != e4permiss && rc != e4access )
         {
            error4set( c4, 0 ) ;
            rc = code4dataFileCloseAll( c4 ) ;
            if ( rc < 0 )
               return rc ;
            rc = file4openLow( file, c4, name ) ;
         }
         if ( c4getReadOnly( c4 ) == 0 && c4->readOnlyRequest == 1 )   /* try opening in read-only mode */
         {
            error4set( c4, 0 ) ;
            c4setReadOnly( c4, 1 ) ;
            rc = file4openLow( file, c4, name ) ;
            c4setReadOnly( c4, 0 ) ;
         }
      }
   #endif

   if ( rc != 0 )
   {
      if ( c4->errOpen )
         return error4describe( c4, rc, E90615, name, (char *) 0, (char *) 0 ) ;
      else
      {
         #if defined( S4SERVER ) || defined( S4ODBC_BUILD )
            if ( rc == e4fileFind )
            {
               error4set(c4, r4noExist) ;
               return ( r4noExist ) ;
            }
         #endif
         error4set( c4, r4noOpen ) ;
         return r4noOpen ;
      }
   }

   if ( doAlloc )
   {
      len = c4strlen( name ) + 1 ;
      file->nameBuf = (char *)u4allocFree( c4, (long)len ) ;
      if ( file->nameBuf == 0 )
      {
         file4close( file ) ;
         return error4( c4, e4memory, E90615 ) ;
      }
      file->doAllocFree = 1 ;
      u4ncpy( file->nameBuf, name, (unsigned int)len ) ;
      file->name = file->nameBuf ;
   }
   else
      file->name = name ;

   #ifndef S4OFF_MULTI
      file->lowAccessMode = c4->accessMode ;
   #endif
   #ifndef S4OPTIMIZE_OFF
      file->fileCreated = 1 ;
   #endif

   /* LY 4/27/99 */
   #ifdef S4FILE_EXTENDED
      lrc = file4lenLow( file ) ;
      if ( file4longGetHi( lrc ) > 0 )
         file->isLong = 1 ;
      else
         file->isLong = 0 ;
   #endif

   #ifdef S4TRACK_FILES_OR_SERVER
      numFiles5++ ;
      #ifdef S4TRACK_FILES
         if ( f4print != -1 )
         {
             #ifdef S4WINDOWS
                #ifdef S4TESTING
                   if ( mem4displayPtr == 0 )
                      error4( 0, e4info, E50101 ) ;
                   d4display_str( mem4displayPtr, "\r\nfile opened: ", 1 ) ;
                   d4display_str( mem4displayPtr, f4print, file->name ) ;
                #else
                   u4writeErr( "file opened: ", 1 ) ;
                   u4writeErr( file->name, 0 ) ;
                #endif
             #else
                if ( f4print )
                   fprintf( stdprn, "\r\nfile opened: %s", file->name ) ;
                else
                   printf( "\r\nfile opened: %s", file->name ) ;
             #endif
          }
      #endif
   #endif


   #ifdef S4ENCRYPT_HOOK
      file->encrypted = code4encryptEnabledHook( c4, file, c4->encryptInit ) ;
   #endif
   return rc ;
}

int S4FUNCTION file4openTest( FILE4 *f4 )
{
   return f4->hand != INVALID4HANDLE ;
}
