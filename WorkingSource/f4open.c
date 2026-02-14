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
      #ifndef S4WINCE   /* LY 2002/11/12 : moved from #include <share.h> below */
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

         #include <share.h>
      #endif
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
      SInt8 permFSpOpenDF = fsRdWrShPerm ;   // LY Sep 21/04 : for setting correct permissions to FSpOpenDF()

      file->hand = INVALID4HANDLE ;
      error4set( c4, 0 ) ;  /* clear positive error values */

      if ( c4getReadOnly( c4 ) )
      {
         file->isReadOnly = 1 ;
         perm = 0x0001 ;
      }
      else
         perm = 0x0003 ;

      #if TARGET_API_MAC_CARBON
         CopyCStringToPascal( name, MACname ) ;
      #else
         memcpy( MACname, name, sizeof(MACname) ) ;
         /* LY 00/01/26 : macro definition of function for newer CodeWarrior */
         S4CTOPSTR( (char *)MACname ) ;  /* convert C string to Pascal string */
      #endif

      switch (c4->accessMode)
      {
         case OPEN4DENY_NONE:
            break ;
         case OPEN4DENY_WRITE:
            perm |= 0x0020 ;
            permFSpOpenDF = fsWrPerm ;  // LY Sep 21/04 : under AFP on remote volumes, no way of getting R/W/deny-W, so open exclusively anyway
            break ;
         case OPEN4DENY_RW:
            perm |= 0x0030 ;
            permFSpOpenDF = fsWrPerm ;  // LY Sep 21/04 : under AFP on remote volumes, open will fail if R/W/deny-R/deny-W not possible
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

      if ( c4->readOnly )  // LY Sep 21/04 : set permission for read access only to data fork
         permFSpOpenDF = fsRdPerm ;

      err = FSpOpenDF(&file->macSpec, permFSpOpenDF, &tempHand ) ; // LY Aug 30/04 : changed from fsWrPerm to fsRdWrShPerm for file sharing

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
         /* LY 2001/07/18 : added !S4LINUX */
         /* LY 2002/08/21 : removed !S464BIT for AIX and HP-UX */
         #if defined(S4FILE_EXTENDED) && !defined(S4LINUX)  /* LY 00/04/05 */
            oflag = (int)(O_RDONLY | O_LARGEFILE) ;
         #else
            oflag = (int)O_RDONLY ;
         #endif
      }
      else
         /* LY 2001/07/18 : added !S4LINUX */
         /* LY 2002/08/21 : removed !S464BIT for AIX and HP-UX */
         #if defined(S4FILE_EXTENDED) && !defined(S4LINUX)  /* LY 00/04/05 */
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
               /* LY 2002/09/20 : fixed problem with multi-user write
                         because of F_RDLCK, now use it temporarily to
               test if file already opened with DENY_WRITE or DENY_RW;
               behavior is now
                                       (Windows Ver.)
                      \user  W           \user  W
                 u\   N R           u\   N R
                 s \1 O I           s \1 O I
                 e 2\ N T R         e 2\ N T R
                 r   \E E W         r   \E E W
                NONE  *            NONE  *
                WRITE *            WRITE
                   RW *               RW                                */
               lck.l_type = F_RDLCK ;
               break;
            case OPEN4DENY_WRITE:
               /* LY 2002/09/20
               lck.l_type = F_RDLCK ;
               break; */
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
         /* LY 2002/09/20 : see LY 2002/09/20 above */
         if ( c4->accessMode == OPEN4DENY_NONE )
         {
            lck.l_type = F_UNLCK ;
            fcntl (file->hand, F_SETLK, &lck ) ;
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
         WSTR5 nameU[256] ;
      #endif
                short numTries ;  // CS 2011/05/19 Declare outside of for loop.

      #ifdef S4WINCE
         if (name[1] == L':' && name[2] == L'\\')  // CS 2001/16/20 strip off driver letter in CE
            name += 2;
      #endif

      error4set( c4, 0 ) ;  /* clear positive error values */
      #ifdef S4UNICODE
         c4atou(name, nameU, 256) ;
      #endif

      // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
      // Microsoft knowledgebase article #811693
      #ifdef S4WINCE
         for ( numTries = 0 ;; numTries++ )
         {
      #endif
            #ifdef S4UNICODE
               fAttributes = GetFileAttributes( nameU ) ;
            #else
               fAttributes = GetFileAttributes( name ) ;
            #endif
      #ifdef S4WINCE
            if ( fAttributes == 0xFFFFFFFF )
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

      // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
      // Microsoft knowledgebase article #811693
      #ifdef S4WINCE
         for ( numTries = 0 ;; numTries++ )
         {
      #endif
            #ifdef S4UNICODE
               file->hand = CreateFile( nameU, fdwAccess, fdwShareMode, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
            #else
               file->hand = CreateFile( name, fdwAccess, fdwShareMode, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
            #endif
      #ifdef S4WINCE
            if ( file->hand == INVALID4HANDLE )
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
            case ERROR_LOGON_FAILURE:  // CS 2003/06/16 Add 6 errors to do with account restriction
            case ERROR_ACCOUNT_RESTRICTION:
            case ERROR_INVALID_LOGON_HOURS:
            case ERROR_INVALID_WORKSTATION:
            case ERROR_PASSWORD_EXPIRED:
            case ERROR_ACCOUNT_DISABLED:
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

      // AS Dec 11/02 - Renamed for clarification
      #ifdef S4DELAY_WRITE_MT
         critical4sectionInit( &file->critical4file ) ;
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



// AS Apr 18/06 - make external...used by d4data.hpp
// AS May 24/02 - exposed file4open only to users now to allow file type considerations to play a larger role
// (in particular to decide whether or not to encrypt files - only encrypt data/index/memos with block encryption)
int S4FUNCTION file4openInternal( FILE4 *file, CODE4 *c4, S4CONST char *name, const int doAlloc, char fileType )
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
   #ifndef S4OFF_OPTIMIZE
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
      // AS Sept. 18/02 - only count if successful
      if ( rc == 0 )
      {
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
      }
   #endif

   file->type = (char)fileType ;
   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      #ifdef S4ENCRYPT_DLL // LY Jul 19/04
         if ( c4->encrypt != 0 )
      #else
         if ( c4->encryptInit != 0 )
      #endif
         file->preprocessed = code4encryptFileHook( c4, file, code4getPreprocessInit( c4 ) ) ;
   #endif

   // AS Nov 8/02 - sema4 locking
   #ifdef S4MUTEX4LOCK
      len = c4strlen( name ) + 1 ;
      u4ncpy( file->lockMutexName, name, (unsigned int)len ) ;
      // also replace all '\' and '.' characters with dashes
      for ( int loop = c4strlen( name ) - 1 ; loop >= 0 ; loop-- )
      {
         if ( file->lockMutexName[loop] == '\\' || file->lockMutexName[loop] == '.' )
            file->lockMutexName[loop] = '-' ;
      }
      u4nameExt( file->lockMutexName, sizeof( file->lockMutexName ), "MUT", 1 ) ;
      c4upper( file->lockMutexName ) ;

      mutex4initShare( &(file->lockMutex), file->lockMutexName ) ;
   #endif

   return rc ;
}



#ifndef S4INTERNAL_COMPILE_CHECK
   // AS May 24/02 - exposed file4open only to users now to allow file type considerations to play a larger role
   // (in particular to decide whether or not to encrypt files - only encrypt data/index/memos with block encryption)
   int S4FUNCTION file4open( FILE4 *file, CODE4 *c4, S4CONST char *name, const int doAlloc )
   {
      // AS Jan 16/03 - Modified file type to indicate a user type so that it would become encrypted
      return file4openInternal( file, c4, name, doAlloc, OPT4USER ) ;
   }
#endif



int S4FUNCTION file4openTest( FILE4 *f4 )
{
   return f4->hand != INVALID4HANDLE ;
}


// AS Jan 15/03 - Support for generic file compression
int S4FUNCTION file4compressOpen( FILE4 *file, CODE4 *c4, S4CONST char *name )
{
   // AS Mar 23/04 - only in fox...
   // AS May 17/04 - client/server functionality to copmress the data file...
   #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      int rc = file4openInternal( file, c4, name, 0, OPT4OTHER ) ;  // also checks input paramaters
      if ( rc != 0 )
         return rc ;

      // AS Aug 1/03 - Large file support
      short version ;
      unsigned long numOffsets ;
      FILE4LONG pos ;
      file4longAssign( pos, 0, 0L ) ;
      rc = file4readAllInternal( file, pos, &version, sizeof( short ) ) ;
      if ( rc == 0 )
      {
         if ( version != 1 && version != 2 )
            rc = error4describe( c4, e4open, E90615, "Compressed file is corrupt or attempt to open non-compressed file as compressed, or failed to open compressesd file with correct encryption setting.", name, 0 ) ;
         else
         {
            file4longAssign( pos, 2 * sizeof( short ), 0 ) ;  // AS Nov 22/04 - ensure assigning to low position
            rc = file4readAllInternal( file, pos, &numOffsets, sizeof( long ) ) ;
         }
      }

      if ( rc == 0 )
      {
         // AS Jul 25/03 - Do a little verification here.  In particular with encrption we don't want to
         // actually start using the info if it hasn't been verified.
         FILE4LONG maxOffsetsLong = file4lenLow( file ) ;
         if ( version == 1 )
            file4longDivide( maxOffsetsLong, 4 ) ;
         else
            file4longDivide( maxOffsetsLong, 8 ) ;
         unsigned long maxOffsets ;
         if ( file4longGetHi( maxOffsetsLong ) != 0 )  // We don't actually support more offsets than can fit into a non-large file
            maxOffsets = ULONG_MAX ;
         else
            maxOffsets = file4longGetLo( maxOffsetsLong ) ;

         if ( numOffsets > maxOffsets )
            rc = error4describe( c4, e4open, E90615, "Compressed file is corrupt or attempt to open non-compressed file as compressed, or failed to open compressesd file with correct encryption setting.", name, 0 ) ;
         else
         {
            COMPRESS4HANDLER *compressInfo = (COMPRESS4HANDLER *)u4allocFree( c4, sizeof(COMPRESS4HANDLER) ) ;
            long sizeInfo ;
            switch ( version )
            {
               case 1:
                  compressInfo->isLongOrCompress = 0 ;
                  sizeInfo = sizeof( COMPRESS4DATA_SHORT ) + (numOffsets - 1) * sizeof( long ) ;  // -1 because sizeof() includes 1 entry
                  compressInfo->shortCompress = (COMPRESS4DATA_SHORT *)u4allocFree( c4, sizeInfo ) ;
                  break ;
               case 2:
                  compressInfo->isLongOrCompress = 1 ;
                  sizeInfo = sizeof( COMPRESS4DATA_LONG ) + (numOffsets - 1) * sizeof( FILE4LONG ) ;  // -1 because sizeof() includes 1 entry
                  compressInfo->longCompress = (COMPRESS4DATA_LONG *)u4allocFree( c4, sizeInfo ) ;
                  break ;
               case 3:
                  compressInfo->isLongOrCompress = 2 ;
                  sizeInfo = sizeof( COMPRESS4WRITE_HEADER ) + (numOffsets - 1) * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ;  // -1 because sizeof() includes 1 entry
                  compressInfo->writeCompress = (COMPRESS4WRITE_HEADER *)u4allocFree( c4, sizeInfo ) ;
                  break ;
               default:
                  rc = -1 ;
                  break ;
            }

            if ( compressInfo == 0 )
               rc = -1 ;
            if ( rc == 0 )
            {
               FILE4LONG pos ;
               file4longAssign( pos, 0, 0L ) ;
               rc = file4readAllInternal( file, pos, compress4handlerInfoGet( compressInfo ), sizeInfo ) ;
            }
            if ( rc == 0 )
            {
               file->isReadOnly = 1 ;  // don't allow writing to compressed files
               rc = file4compressInit( file, compressInfo, 0 ) ;
            }
            if ( rc != 0 )
               u4free( compressInfo ) ;
         }
      }

      if ( rc != 0 )
      {
         file4close( file ) ;
         return rc ;
      }

      file->freeCompressInfo = 1 ;

      return 0 ;
   #else
      return e4notSupported ;
   #endif
}
