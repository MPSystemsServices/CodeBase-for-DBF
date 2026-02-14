/* f4create.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

#ifdef S4TRACK_FILES_OR_SERVER
   extern unsigned int numFiles5 ;
   #ifdef S4TRACK_FILES
      extern int f4print ;
   #endif
#endif

#ifdef S4UNIX
   #include <sys/wait.h>  /* CS 2000/02/04 */
#endif

#ifdef S4WINTEL
   #ifndef S4IBMOS2
      #ifndef S4SINGLE
         #ifndef __TURBOC__
            #include <sys\locking.h>
            #define S4LOCKING
         #endif
         #ifdef _MSC_VER
            #include <sys\types.h>
            #include <sys\locking.h>
         #endif
      #endif
      #ifdef __ZTC__
         #ifndef S4WINDOWS
            extern int  errno ;
         #endif
      #endif
   #endif
   #ifndef S4WINCE
      #include <sys\stat.h>
      #include <share.h>
   #endif
#endif

#ifdef S4MACINTOSH
/*   #include <unix.h>*/
#endif
#if !defined(S4WINCE) && !defined(S4PALM)
   #include <fcntl.h>
#endif
#if !defined(S4WINDOWS) && !defined(S4WINCE) && !defined(S4PALM)
   #include <errno.h>
   #ifdef S4ERRNO
      extern int errno ;
   #endif
#endif

/*
**  file4createLow() generic outline:
**
**  - clear any postive error values
**  - set read/write access flag (for create it is always both read and write)
**  - set the shared flag based on CODE4::accessMode
**    3 possible settings: OPEN4DENY_NONE, OPEN4DENY_WRITE, OPEN4DENY_RW.
**    return failure if no the setting is invalid.
**  - set the safety (open existing) flag based on CODE4::safety
**  - determine if file exists
**    if it does exist, ensure that nobody else has it open (exclusive access)
**    many operating systems allow creates on open files, which result in
**    failures for the other applications.  Avoid this for safety reasons.
**  - physically attempt to create the file
**  - return the result
**
**  Returns: 0 = success, r4noCreate = failure
**
**  Notes:
**     This routine shouldn't generate a CodeBase error
*/

#ifdef S4MACINTOSH
static int file4createLow( FILE4 *file, CODE4 *c4, const char *name )
{
   short perm = 0x0003 ;   /*For creation you always have read write access*/
   Str255 MACname ;
   HParamBlockRec MACblock ;
   OSErr err ;
   short tempHand ;

   file->hand = INVALID4HANDLE ;
   error4set( c4, 0 ) ;  /* clear positive error values */

   memcpy( MACname, name, sizeof(MACname) ) ;
   /* LY 00/01/26 : macro definition of function for newer CodeWarrior */
   S4CTOPSTR( (char *)MACname ) ;  /* convert C string to Pascal string */

   err = FSMakeFSSpec( c4->macVol, c4->macDir, MACname, &file->macSpec ) ;
   switch (err)
   {
      case noErr:       /*File exists*/
         if (c4->safety)
            return r4noCreate ;
         else
            if(FSpDelete(&file->macSpec) < 0 )
               return r4noCreate ;
      case fnfErr:
         break ;
      default:
         return r4noCreate ;
   }

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
         return r4noCreate ;
   }

   if (FSpCreate(&file->macSpec, '????', '????', smSystemScript) != noErr)
      return r4noCreate ;

   MACblock.accessParam.ioCompletion = NULL ;
   MACblock.accessParam.ioNamePtr = file->macSpec.name ;
   MACblock.accessParam.ioVRefNum = file->macSpec.vRefNum ;
   MACblock.accessParam.ioDenyModes = perm ;
   MACblock.fileParam.ioDirID = file->macSpec.parID ;

   err = PBHOpenDenySync((HParmBlkPtr)&MACblock) ;
   if (err == noErr )
   {
      file->hand = MACblock.ioParam.ioRefNum ;
      return 0 ;
   }
   else if(err != paramErr)
   {
      FSpDelete(&file->macSpec) ;
      return r4noCreate ;
   }
   else  /*(err==paramErr)*/
   {

   /*If we get to this point, then it means that PBHOpenDenySync is not supported*/
   /*We now have to use functions with less control.*/
   /*Use fsWrPerm because range locking won't work anyway. Failures happen if not exclusive open */

      err = FSpOpenDF(&file->macSpec, fsWrPerm, &tempHand ) ;
      if (err==noErr)
      {
         file->hand = tempHand ;
         return 0 ;
      }
      else
      {
         FSpDelete(&file->macSpec) ;
         return r4noCreate ;
       }
   }
}
#endif /*S4MACINTOSH*/

#ifdef S4PALM
   int S4FUNCTION file4exists( const char *fileName )
   {
      /* existHandle will be >= 0 if the file exists and is allowed access

         -1 means file does not exist
       */

      FileHand hand ;
      Err e;
      hand = FileOpen(0,(char *)fileName,0,0,fileModeReadOnly | fileModeExclusive | fileModeAnyTypeCreator,&e);

      if ( e != 0 )
         return -1;

      FileClose( hand ) ;
      return 0 ;
   }
#endif

#ifdef S4UNIX
int S4FUNCTION file4exists( const char *fileName )
{
   /* existHandle will be >= 0 if the file exists and is allowed access

      -1 means file does not exist
    */

   int existHandle, hand ;

   hand = open(fileName, O_RDWR) ;

   if ( hand != INVALID4HANDLE )
      existHandle = 0 ;
   else
      existHandle = -1 ;
   close( hand ) ;

   return existHandle ;
}

#if 1

static int file4createLow( FILE4 *file, CODE4 *c4, const char *name )
{
   int pid, rc, status ;
   struct flock lck ;

   error4set( c4, 0 ) ;  /* clear positive error values */

   /* set to invalid by default, since not created yet. */
   file->hand = INVALID4HANDLE ;

   lck.l_whence = SEEK_SET;
   lck.l_start = 0;
   lck.l_len = 1000000000 ;
   lck.l_type = F_WRLCK ;

   if ( c4->safety )
   {
      /* Do not overwrite any existing files. */
      file->hand = open( name, O_CREAT | O_RDWR | O_EXCL, 0666 ) ;
      if ( file->hand == INVALID4HANDLE )
      {
         if ( errno == EMFILE )
            return e4numFiles ;     /* Perhaps close some and retry */
         return r4noCreate ;        /* no access or file already exists */
      }
   }
   else
   {
      /* We can overwrite a file if it is not locked by us or someone else. */
      if ( access( name, O_RDWR ) == 0 )
      {
         #ifndef S4OFF_MULTI
            /* File exists, and we can access it. Is it locked? We must make
               this test from another process to see if *we* have it locked. */
            pid = fork();
            if ( pid < 0 )
               return e4lock ;      /* Can't create child process */
            if ( pid == 0 )
            {
               /* Child process - see if file is locked. */
               file->hand = open( name, O_RDWR ) ;
               if (file->hand == INVALID4HANDLE )
                  exit( 1 ) ;
               rc = fcntl( file->hand, F_GETLK, &lck ) ;
               close( file->hand ) ;
               if ( rc == -1 || lck.l_type != F_UNLCK )
                  exit( 1 ) ;
               exit( 0 ) ;
            }
            while ( wait( &status ) != pid )
               ;
            if ( WIFEXITED( status ) == 0 )
               return e4lock ;      /* Didn't exit properly */
            if ( WEXITSTATUS( status ) != 0 )
               return e4lock;       /* Someone has it locked */
         #endif
         file->hand = open( name, O_RDWR ) ;
         if ( file->hand == INVALID4HANDLE )
            return r4noCreate ;
      }
      else if ( errno == ENOENT )
      {
         /* The file does not exist - create it. */
         file->hand = open(name, O_CREAT | O_RDWR | O_EXCL, 0666 );
         if ( file->hand == INVALID4HANDLE )
         {
            if ( errno == EMFILE )
               return e4numFiles ;
            return r4noCreate ;
         }
      }
      /* BCR 11/13/00 -- check for other potential errors before continuing */
      else if (errno == EACCES)
      {
         return e4access;
      }
      else
      {
         return r4noCreate;
      }
   }

   /* File exists, is not locked, and we have it open. Note that our later
      attempt to lock the file can fail, because someone else can lock it
      between our check for a lock and our actual lock. Note also, that
      the 'close' calls here will remove any lock on the file put there by
      this process. Since this is the code that is doing that (as well as
      code in f4openLow), this shouldn't be an issue. */

   #ifdef S4NO_CHSIZE
      file4changeSize( file, 0 ) ;
   #else
      chsize( file->hand, 0 ) ;
   #endif
   #ifndef S4NO_FCHMOD
      fchmod( file->hand, 0666 ) ;
   #endif
   #ifndef S4OFF_MULTI
      switch ( c4->accessMode )
      {
         case OPEN4DENY_NONE:
         case OPEN4DENY_WRITE:
            lck.l_type = F_RDLCK ;
            break ;
         case OPEN4DENY_RW:
            lck.l_type = F_WRLCK ;
            break ;
         default:
            close( file->hand ) ;
            file->hand = INVALID4HANDLE ;
            return r4noCreate ;
      }
      rc = fcntl( file->hand, F_SETLK, &lck ) ;
      if ( rc!=0 )
      {
         close( file->hand ) ;
         file->hand = INVALID4HANDLE ;
         return r4noCreate ;
      }
   #endif
   return 0 ;
}

#else

static int file4createLow( FILE4 *file, CODE4 *c4, const char *name )
{
   int oflag  ;
   int rc = 0 ;
   #ifndef S4OFF_MULTI
      struct flock lck ;
      #ifdef S4MULTIC4
         int pid, status;
      #endif

      lck.l_whence = SEEK_SET ;
      lck.l_start  = 0 ;
      lck.l_len = 1000000000 ;
   #endif

   error4set( c4, 0 ) ;  /* clear positive error values */

   /*set to invalid by default, since not created yet.*/
   file->hand = INVALID4HANDLE ;

   /* first determine the basic access values */
   oflag = (int)(O_CREAT | O_RDWR| O_EXCL) ;

   /*Check for files existence*/
   file->hand = open(name, oflag, 0666 ) ;
   if (file->hand == INVALID4HANDLE )
   {                        /*problem creating the file*/
      if ( errno == EACCES )  /*If this is the error, we would have lost it on the next open*/
         return r4noCreate ;
      if (errno == EMFILE )   /* Can't open any more files */
         return e4numFiles ;
      /* file should exist and we have permission for it */
      file->hand = open(name, O_RDWR) ;
      if (file->hand == INVALID4HANDLE )   /*Does the file not exist*/
      {
          /* Couldn't open the file */
          file->hand = INVALID4HANDLE ;
          return r4noCreate ;
      }
      /*file does exist*/
      if ( !c4->safety ) /*overwrite file if no one else is using it*/
      #ifndef S4OFF_MULTI
         {
            lck.l_type = F_WRLCK ;
            #ifdef S4MULTIC4
               if ( ( pid = fork() ) < 0 )
                  return e4lock ;
               if ( pid ==0 )               /* Child fork() */
               {
                  rc = fcntl( file->hand, F_GETLK, &lck ) ;
                  if (rc == -1 || lck.l_type != F_UNLCK)
                     exit(1);
                  exit(0);
               }
               while ( wait( &status ) != pid )    /* Parent fork() */
                  ;
               if( ( WIFEXITED( status ) == 0 ) )   /* If it didn't exit properly */
                  return e4lock ;
               rc = WEXITSTATUS( status ) ;  /*IF it was able to lock*/
            #else
               rc = fcntl( file->hand, F_GETLK, &lck ) ;
               if (rc != -1 && lck.l_type == F_UNLCK)
                  rc = 0;
               else
                  rc = 1;
            #endif  /* !S4MULTIC4 */
            if (rc!=0)
            {   /*someone else is using it*/
               close( file->hand ) ;
               file->hand = INVALID4HANDLE ;
               return r4noCreate ;
            }
            else  /*no one is using it so kill it*/
      #endif
      #ifdef S4NO_CHSIZE
         file4changeSize( file, 0 ) ;
      #else
         chsize( file->hand, 0 ) ;
      #endif
      #ifndef S4OFF_MULTI
         }
      #endif
      else
      {    /*Since the file exists, and we have safeties, we can't create*/
         close( file->hand ) ;
         file->hand = INVALID4HANDLE ;
         return r4noCreate ;
      }
   }
   #ifndef S4NO_FCHMOD
      fchmod( file->hand, 0666 ) ;
   #endif

   #ifndef S4OFF_MULTI
      switch ( c4->accessMode )
      {
         case OPEN4DENY_NONE:
         case OPEN4DENY_WRITE:
            lck.l_type = F_RDLCK ;
            break ;
         case OPEN4DENY_RW:
            lck.l_type = F_WRLCK ;
            break ;
         default:
            close(file->hand) ;
            file->hand = INVALID4HANDLE ;
            return r4noCreate ;
      }
      #ifdef S4MULTIC4
         if ((pid = fork())<0)
            return e4lock ;
         if (pid ==0)               /* Child fork() */
         {
            rc = fcntl (file->hand, F_SETLK, &lck ) ;
            exit(rc) ;
         }
         while (wait(&status)!=pid)    /* Parent fork() */
            ;
         if ((WIFEXITED(status)==0))   /* If it didn't exit properly */
            return e4lock ;
         if ((rc=WEXITSTATUS(status)) == 0)   /*IF it was able to lock*/
         {
            rc = fcntl (file->hand, F_SETLK, &lck ) ;
         }
      #else
         rc = fcntl (file->hand, F_SETLK, &lck ) ;
      #endif  /* !S4MULTIC4 */

      if (rc!=0)
      {
         close(file->hand) ;
         file->hand = INVALID4HANDLE ;
         return r4noCreate ;
      }
   #endif
   return 0 ;
}
#endif
#endif /* S4UNIX */

#ifdef S4PALM
static int file4createLow( FILE4 *file, CODE4 *c4, const char *name )
{
   const UInt16 cardNo = 0;
   const UInt32 fileType = 0;
   const UInt32 creator = 0;
   UInt32 openMode;
   if (c4getErrorCode(c4) < 0)
      return -1;

   c4setErrorCode(c4,0);

   file->hand = INVALID4HANDLE;

   openMode = fileModeReadWrite | fileModeAnyTypeCreator;
   #ifdef S4OFF_MULTI
      #ifndef E4DEBUG
         /* open in shared mode for debugging to allow other programs to examine */
         /* otherwise, if single-user, file should be opened exclusively */
         openMode |= fileModeExclusive ;
      #endif
   #else
      switch( c4->accessMode )
      {
         case OPEN4DENY_NONE:
            break ;
         case OPEN4DENY_WRITE:
         case OPEN4DENY_RW:
            openMode |= fileModeExclusive ;
            break ;
         default:
            return r4noCreate ;
      }
   #endif  /* S4OFF_MULTI */

   if (c4->safety)
      openMode |= fileModeDontOverwrite;

   file->hand = FileOpen(cardNo,(Char*)name,fileType,creator,openMode,0);

   if (file->hand == INVALID4HANDLE)
      return r4noCreate;
   else
      return r4success;
}
#endif
#ifdef S4WIN32
int S4FUNCTION file4exists( const char *fileName )
{
   /* existHandle will be >= 0 if the file exists and is allowed access

      -1 means file does not exist
    */

   int existHandle ;

   #ifdef S4WINCE
      unsigned short nameU[256] ;
      c4atou( fileName, (unsigned short *)nameU, 256 ) ;

//      HANDLE hand = CreateFile( nameU, 0, FILE_SHARE_WRITE|FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 ) ;
      WIN32_FIND_DATA dummy ;
      HANDLE hand = FindFirstFile( nameU, &dummy ) ;
      if ( hand != INVALID4HANDLE )    /* LY 00/07/04 : removed file-> */
          existHandle = 0 ;
      else
          existHandle = -1 ;
//      CloseHandle( hand ) ;
      FindClose(hand) ;
   #else
      #ifdef __BORLANDC__
         existHandle = access( fileName, 0 ) ;
      #else
         existHandle = _access( fileName, 0 ) ;
      #endif
   #endif

   return existHandle ;
}


// S4WIN32
static int file4createLow( FILE4 *file, CODE4 *c4, const char *name )
{
   int existHandle ;
   DWORD fdwAccess, fdwShareMode, fdwCreate ;

   error4set( c4, 0 ) ;  /* clear positive error values */

   /* set to invalid by default, since not created yet */
   #ifdef S4WIN64 /* LY 00/09/20 */
      file->hand = NULL ;
   #else
      file->hand = INVALID4HANDLE ;
   #endif

   /* first determine the basic access values */
   fdwAccess = GENERIC_WRITE | GENERIC_READ ;

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
            return r4noCreate ;
      }
   #endif  /* S4OFF_MULTI */

   if ( c4->safety )
      fdwCreate = CREATE_NEW ;
   else
      fdwCreate = CREATE_ALWAYS ;

   existHandle = file4exists( name ) ;
   #ifdef S4WINCE
      unsigned short nameU[256] ;
      c4atou(name, (unsigned short *)nameU, 256) ;
   #endif

   /* 07/21/99 Added new support... if CODE4.fileFlush is true, do not allow lazy flushing of
      writes.  This is useful in Windows 95/98 where file writes otherwise may get delayed
      forever.
   */

   DWORD flagsAndAttributes ;

   if ( c4->fileFlush )
      flagsAndAttributes = FILE_FLAG_WRITE_THROUGH ;
   else
      flagsAndAttributes = FILE_ATTRIBUTE_NORMAL ;

   if ( existHandle >= 0 )   /* file exists and is available */
   {
      /* if safety is != 0, function will fall through since file
         already exists */
      if ( c4->safety == 0 ) /* ensure exclusive access, otherwise problems ensue */
      {
         #ifdef S4UNICODE
            file->hand = CreateFile( nameU, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
         #else
            file->hand = CreateFile( name, GENERIC_WRITE | GENERIC_READ, 0, 0, OPEN_EXISTING, flagsAndAttributes, 0 ) ;
         #endif
         #ifdef S4WIN64 /* LY 00/09/20 */
            if ( file->hand == NULL )
         #else
            if ( file->hand == INVALID4HANDLE )
         #endif
            return r4noCreate ;
         CloseHandle( file->hand ) ;
         existHandle = -1 ;
      }
   }

   if ( existHandle < 0 )  /* file doesn't exist, or exclusive access is available, so attempt create */
   {
      #ifdef S4UNICODE
         file->hand = CreateFile( nameU, fdwAccess, fdwShareMode, 0, fdwCreate, flagsAndAttributes, 0 ) ;
      #else
         file->hand = CreateFile( name, fdwAccess, fdwShareMode, 0, fdwCreate, flagsAndAttributes, 0 ) ;
      #endif
   }

   #ifdef S4WIN64 /* LY 00/09/20 */
      if ( file->hand == NULL )
   #else
      if ( file->hand == INVALID4HANDLE )
   #endif
      return r4noCreate ;

   #ifdef S4MULTI_THREAD
      InitializeCriticalSection( &file->critical4file ) ;
   #endif

   return 0 ;
}
#endif /* S4WIN32 */



#ifdef S4WIN16
static int file4createLow( FILE4 *file, CODE4 *c4, const char *name )
{
   int existHandle, shmode, omode ;

   error4set( c4, 0 ) ;  /* clear positive error values */

   existHandle = access( name, 0 ) ;  /* if file doesn't exist, create */

   if ( existHandle != INVALID_HANDLE_VALUE )   /* file exists, see if open if not safety... */
   {
      if ( c4->safety == 0 )
      {
         file->hand = _lopen( name, OF_READWRITE | OF_SHARE_EXCLUSIVE ) ;
         if ( file->hand == INVALID4HANDLE )
            return r4noCreate ;
         _lclose( file->hand ) ;
         existHandle = -1 ;
      }
   }

   if ( existHandle < 0 )  /* if file doesn't exist, create */
   {
      /* do initial creation in exclusive mode */
      file->hand = _lcreat( name, 0 ) ;   /* attr == 0 : read/write permission */

      if ( file->hand != INVALID4HANDLE )  /* now that file is created, move to regular r/w mode */
      {
         _lclose( file->hand ) ;
         omode = OF_READWRITE ;
         switch( c4->accessMode )
         {
            case OPEN4DENY_RW:
               shmode = OF_SHARE_EXCLUSIVE ;
               break ;
            case OPEN4DENY_WRITE:
               shmode = OF_SHARE_DENY_WRITE ;
               break ;
            case OPEN4DENY_NONE:
               shmode = OF_SHARE_DENY_NONE ;
               break ;
            default:
               file->hand = INVALID4HANDLE ;
               return r4noCreate ;
         }
         file->hand = _lopen( name, omode | shmode ) ;
      }
   }

   if ( file->hand == INVALID4HANDLE )
      return r4noCreate ;

   return 0 ;
}
#endif /* S4WIN16 */

/* now for all other instances */
#ifdef S4WINTEL
#ifndef S4WIN32
#ifndef S4WIN16
static int file4createLow( FILE4 *file, CODE4 *c4, const char *name )
{
   int extraFlag, oflag, pmode, shmode ;

   error4set( c4, 0 ) ;  /* clear positive error values */

   oflag = (int)( O_CREAT | O_TRUNC | O_BINARY | O_RDWR ) ;
   pmode = (int)( S_IREAD  | S_IWRITE ) ;
   if ( c4->safety )
      extraFlag = O_EXCL ;
   else
      extraFlag = 0 ;

   /* ensure first that nobody else has access to the file */
   file->hand = sopen( name, O_RDWR, SH_DENYRW, pmode ) ;
   if ( file->hand == INVALID4HANDLE )
      switch ( errno )
      {
         case ENOENT:   /* file not found, so can create */
            break ;
         default:  /* access or misc. error, return failure */
            return r4noCreate ;
      }

   close( file->hand ) ;

   #ifdef S4OFF_MULTI
      shmode = SH_DENYRW ;
   #else
      switch( c4->accessMode )
      {
         case OPEN4DENY_RW:
            shmode = SH_DENYRW ;
            break ;
         case OPEN4DENY_WRITE:
            shmode = SH_DENYWR ;
            break ;
         case OPEN4DENY_NONE:
            shmode = SH_DENYNO ;
            break ;
         default:
            file->hand = INVALID4HANDLE ;
            return r4noCreate ;
      }
   #endif

   file->hand = sopen( name, extraFlag | oflag, shmode, pmode ) ;
   if ( file->hand == INVALID4HANDLE )
      return r4noCreate ;

   return 0 ;
}
#endif /* S4WINTEL */
#endif /* S4WIN32 */
#endif /* S4WIN16 */

int S4FUNCTION file4create( FILE4 *file, CODE4 *c4, S4CONST char *name, const int doAlloc )
{
   int rc, len ;

   #ifdef E4PARM_HIGH
      if ( file == 0 || c4 == 0 )
         return error4( c4, e4parm_null, E90602 ) ;
   #endif

   #ifndef S4OPTIMIZE_OFF
      code4memStartMaxSet( c4, c4->memMaxPercent ) ;  /* start optimization if not enabled and not suspended */
   #endif

   c4memset( (void *)file, 0, sizeof( FILE4 ) ) ;
   file->codeBase = c4 ;
   #ifdef S4WIN64 /* LY 00/09/20 */
      file->hand = NULL ;
   #else
      file->hand = INVALID4HANDLE ;
   #endif
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( name == 0 )
   {
      rc = file4tempLow( file, c4, c4->createTemp, c4->createTemp, NULL ) ;
      if ( rc == 0 )
         return 0 ;
   }
   else
      rc = file4createLow( file, c4, name ) ;

   #ifdef S4SERVER
      if ( rc == r4noCreate )   /* free up any open unused file handles */
      {
         error4set( c4, 0 ) ;
         rc = code4dataFileCloseAll( c4 ) ;
         if ( rc < 0 )
            return rc ;
         if ( name == 0 )
            return file4tempLow( file, c4, c4->createTemp, 1, NULL ) ;
         else
            rc = file4createLow( file, c4, name ) ;
      }
   #endif

   if ( rc < 0 )
      return rc ;

   if ( rc == r4noCreate )
   {
      if ( c4getErrCreate( c4 ) )
         return error4describe( c4, e4create, E90602, name, (char *) 0, (char *) 0 ) ;
      error4set( c4, r4noCreate ) ;
      return r4noCreate ;
   }

   if ( doAlloc && name != NULL )
   {
      len = c4strlen(name) + 1 ;
      file->nameBuf = (char *)u4allocEr( c4, (long)len ) ;
      if ( file->nameBuf == 0 )
      {
         file4close( file ) ;
         return e4memory ;
      }
      u4ncpy( file->nameBuf, name, (unsigned int)len ) ;
      file->name = file->nameBuf ;
      file->doAllocFree = 1 ;
   }
   else
      file->name = name ;

   #ifndef S4OFF_MULTI
      file->lowAccessMode = c4->accessMode ;
   #endif
   #ifndef S4OFF_OPTIMIZE
      file->fileCreated = 1 ;
   #endif
   if ( c4->createTemp == 1 )
      file->isTemp = 1 ;

   #ifdef S4TRACK_FILES_OR_SERVER
      #ifdef S4TRACK_FILES
         if ( f4print != -1 )
         {
             #ifdef S4WINDOWS
                #ifdef S4TESTING
                   if ( mem4displayPtr == 0 )
                      error4( 0, e4info, E50101 ) ;
                   d4display_str( mem4displayPtr, "\r\nfile created: ", 1 ) ;
                   d4display_str( mem4displayPtr, f4print, file->name ) ;
                #else
                   u4writeErr( "file created: ", 1 ) ;
                   u4writeErr( file->name, 0 ) ;
                #endif
             #else
                if ( f4print )
                   fprintf( stdprn, "\r\nfile created: %s", file->name ) ;
                else
                   printf( "\r\nfile created: %s", file->name ) ;
             #endif
          }
      #endif
      numFiles5++ ;
   #endif

   #ifdef S4ENCRYPT_HOOK
      file->encrypted = code4encryptEnabledHook( c4, file, c4->encryptInit ) ;
   #endif

   return 0 ;
}
