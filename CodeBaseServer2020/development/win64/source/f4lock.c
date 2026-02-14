/* f4lock.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef S4IBMOS2
   #define INCL_DOSFILEMGR
   #include <os2.h>
   #ifndef NO_ERROR
      #define NO_ERROR 0
      #define ERROR_LOCK_VIOLATION 33
      #define S4BYTE_DEFINED
   #endif
#endif

#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

#ifndef S4SINGLE

#ifndef S4WINTEL
   #include <unistd.h>
   #ifdef S4NO_LOCKF
      #include <fcntl.h>
   #endif
#else
   #ifndef S4WIN32
   #ifndef __TURBOC__
      #ifndef S4IBMOS2
         #include <sys\locking.h>
         #define S4LOCKING
      #endif
   #endif
   #ifdef __ZTC__
      extern int  errno ;
   #endif
   #ifdef _MSC_VER
      #include <sys\types.h>
   #endif
   #ifdef __TURBOC__
      #ifndef S4OS2
        #ifdef __BORLANDC__
           #include <sys\locking.h> /* Borland C++ compilers */
        #else
           #include <locking.h>             /* Turbo C++ for DOS compilers */
           /* #include <sys\locking.h> */   /* Turbo C++ for Win compilers */
        #endif
   /*   extern int cdecl errno ; */
      #endif
   #endif
/*   #include <sys\stat.h>*/
/*   #include <share.h>*/
   #endif
#endif

#endif

#if !defined( S4WIN32 ) && !defined( S4PALM )
/*   #include <fcntl.h>*/
   #include <errno.h>
#endif

#ifdef S4ERRNO
   extern int errno ;
#endif

#ifndef S4SINGLE
   #ifdef S4CLIENT
      static char this4application[] = "THIS APPLICATION" ;
      int dfile4registerLocked( DATA4FILE *locker, const long lockId )
      {
         CODE4 *c4 ;

         if ( locker == 0 )
            return 0 ;

         c4 = locker->c4 ;

         c4->lockedLockItem = lockId ;
         c4->lockedFileName = locker->accessName ;
         c4->lockedUserName = this4application ;
         c4->lockedNetName = this4application ;

         return 0 ;
      }
   #else
      int dfile4registerLocked( DATA4FILE *d4, const long lockId, int doExtra )
      {
         TRAN4 *trans ;
         CODE4 *c4 ;
         long clientId, serverId ;

         if ( d4 == 0 )
            return 0 ;

         c4 = d4->c4 ;

         #ifdef S4SERVER
            if ( c4->currentClient->isStream )  /* don't bother, we don't use anyway */
               return 0 ;
         #endif

         #ifdef E4PARM_LOW
            if ( lockId < -2L )
               return error4( c4, e4parm, E96701 ) ;
         #endif

         #ifdef S4SERVER
            c4->server->lockedLockItem = lockId ;
         #else
            c4->lockedLockItem = lockId ;
         #endif

         if ( doExtra )
         {
            switch( lockId )
            {
               case -1:  /* file */
                  clientId = d4->fileClientWriteLock ;
                  serverId = d4->fileServerWriteLock ;
                  break ;
               case 0:  /* append lock */
                  clientId = d4->appendClientLock ;
                  serverId = d4->appendServerLock ;
                  if ( clientId == 0 || serverId == 0 )  /* probably due to a file lock */
                  {
                     clientId = d4->fileClientWriteLock ;
                     serverId = d4->fileServerWriteLock ;
                  }
                  if ( clientId == 0 || serverId == 0 )  /* probably due to an exclusive open */
                  {
                     clientId = d4->tempClientLock ;
                     serverId = d4->tempServerLock ;
                  }
                  break ;
               default:  /* record number */
                  clientId = d4->tempClientLock ;
                  serverId = d4->tempServerLock ;
                  if ( clientId == 0 || serverId == 0 )  /* probably due to a file lock */
                  {
                     clientId = d4->fileClientWriteLock ;
                     serverId = d4->fileServerWriteLock ;
                  }
                  if ( clientId == 0 || serverId == 0 )  /* probably due to an exclusive open */
                  {
                     clientId = d4->tempClientLock ;
                     serverId = d4->tempServerLock ;
                  }
                  break ;
            }

            if ( clientId == 0 || serverId == 0 )  // means system has the lock...
               return 0 ;

            trans = code4idDataTrans( c4, serverId, clientId ) ;

            #ifdef S4SERVER
               if ( trans )
               {
                  c4->server->lockedFileName = code4idDataAlias( c4, serverId, clientId ) ;
                  c4->server->lockedUserName = trans->userId ;
                  c4->server->lockedNetName = trans->netId ;
               }
               else
               {  // CS for CJ 2000/05/04  code4idDataTrans might return null
                  c4->server->lockedFileName = 0 ;
                  c4->server->lockedUserName = 0 ;
                  c4->server->lockedNetName = 0 ;
               }
            #else
               if ( trans )
               {
                  c4->lockedFileName = code4idDataAlias( c4, serverId, clientId ) ;
                  c4->lockedUserName = trans->userId ;
                  c4->lockedNetName = trans->netId ;
               }
               else
               {
                  c4->lockedFileName = 0 ;
                  c4->lockedUserName = 0 ;
                  c4->lockedNetName = 0 ;
               }
            #endif
         }

         return 0 ;
      }
   #endif /* S4CLIENT */
#endif /* S4SINGLE */

#ifdef S4SERVER
   /* leave non-inlined in order to force const returns */
   const char *S4FUNCTION code4lockNetworkId( CODE4 *c4 )
   {
      return c4->server->lockedNetName ;
   }

   const char *S4FUNCTION code4lockUserId( CODE4 *c4 )
   {
      return c4->server->lockedUserName ;
   }

   const char *S4FUNCTION code4lockFileName( CODE4 *c4 )
   {
      return c4->server->lockedFileName ;
   }

   long S4FUNCTION code4lockItem( CODE4 *c4 )
   {
      return c4->server->lockedLockItem ;
   }
#else
   const char *S4FUNCTION code4lockFileName( CODE4 *c4 )
   {
      #ifdef S4OFF_MULTI
         return 0 ;
      #else
         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
            {
               error4( 0, e4parm_null, E91011 ) ;
               return 0 ;
            }
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( c4, 1, E91011 ) )
               return 0 ;
         #endif

         return c4->lockedFileName ;
      #endif
   }

   long S4FUNCTION code4lockItem( CODE4 *c4 )
   {
      #ifdef S4OFF_MULTI
         return -2 ;
      #else
         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
            {
               error4( 0, e4parm_null, E91012 ) ;
               return 0 ;
            }
         #endif
         return c4->lockedLockItem ;
      #endif
   }

   const char *S4FUNCTION code4lockNetworkId( CODE4 *c4 )
   {
      #ifdef S4OFF_MULTI
         return 0 ;
      #else
         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
            {
               error4( 0, e4parm_null, E91009 ) ;
               return 0 ;
            }
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( c4, 1, E91009 ) )
               return 0 ;
         #endif

         return c4->lockedNetName ;
      #endif
   }

   const char *S4FUNCTION code4lockUserId( CODE4 *c4 )
   {
      #ifdef S4OFF_MULTI
         return 0 ;
      #else
         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
            {
               error4( 0, e4parm_null, E91010 ) ;
               return 0 ;
            }
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( c4, 1, E91010 ) )
               return 0 ;
         #endif

         return c4->lockedUserName ;
      #endif
   }
#endif

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION file4lockInternal( FILE4 *file, unsigned long posStart, long posStartHi, unsigned long numBytes, long numBytesHi )
{
   #ifdef S4SINGLE
      return 0 ;
   #else
      int rc, numAttempts ;
      CODE4 *c4 ;
      #ifdef S4UNIX
         #ifdef S4MULTIC4
            int pid, status;
         #endif
         #ifdef S4NO_LOCKF
            struct flock fstruct ;
         #endif
      #endif
      #ifdef S4IBMOS2
         struct LockStrc
         {
            long FileOffset ;
            long RangeLength ;
         } L4RANGE ;
      #endif
      #ifdef S4MACINTOSH
         ParamBlockRec MACParmBlk ;
      #endif

      #ifdef E4PARM_HIGH
         if ( file == 0 )
            return error4( 0, e4parm_null, E90613 ) ;
         // AS Sep 19/01 - S4MDX does support negative locking if the o/s supports it, so don't do this test
         // AS July 25/01 - Was checking this if NOT MDX, instead of if it was MDX, t4fail was failing
         //#ifndef S4MDX
         //#ifdef S4MDX
         //   if ( (long)numBytes < 0 || (long)posStart < 0 )  // in MDX negative locking (i.e. very high byte locking) is not multi-user compatible with dBase
         //      return error4( 0, e4parm, E90613 ) ;
         //#endif
      #endif

      /* AS 01/06/98 - relaxed isReadOnly contraint.  Will lock even then.
         Generally f4lock() would only get called if user explicitly requested
         it;  they may want to force a lock, so allow them to do so.

         if ( numBytes == 0 || file->isReadOnly || file->lowAccessMode != OPEN4DENY_NONE )  // don't check error code
            return 0 ;

         AS 01/04/00 - if open in DENY_WRITE mode, we want to still enforce locks.
         this is because OLE-DB and ODBC perform locks to ensure records are committed.
         if locks are not placed on writes, the other client may place locks on the
         record but the value can still be changed or rolled back because no locks
         are actually placed.  t4odbc4.c displays problem.

         if ( numBytes == 0 || file->lowAccessMode != OPEN4DENY_NONE )  // don't check error code
            return 0 ;
      */

      // AS July 16/02 - if the file is opened exclusive, we shouldn't be calling this function (handled at a higher level)
      // otherwise we can get into deadlock with ourselves (when opening clones of data files)
      // I have been fixing this for fox, other indexes may need some work yet...
      assert5( file->lowAccessMode != OPEN4DENY_RW ) ;
      if ( numBytes == 0 || file->lowAccessMode == OPEN4DENY_RW )  /* don't check error code */
         return 0 ;

      #ifdef E4PARM_HIGH
         /* check before c4 since it will be invalid if file handle invalid */
         if ( file->hand == INVALID4HANDLE )
            return error4( 0, e4parm, E80608 ) ;
      #endif

      c4 = file->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifdef S4LOCK_HOOK
         numAttempts = 0 ;
      #else
         numAttempts = c4->lockAttempts ;
         if ( numAttempts == 0 )
            numAttempts = 1 ;
      #endif
      #ifndef S4WIN32
         errno = 0 ;
      #endif

      for( ;; )
      {
         #ifdef S4LOCKING
            #ifdef S4WINDOWS
               _llseek( file->hand, posStart, 0 ) ;
            #else
               lseek( file->hand, posStart, 0 ) ;
            #endif
            rc = locking( file->hand, LK_NBLCK, numBytes ) ;
         #else
            #ifdef S4UNIX
               #ifndef S4NO_LOCKF
                  lseek( file->hand, posStart, 0 ) ;
               #else
                  fstruct.l_type = F_WRLCK ;
                  fstruct.l_whence = SEEK_SET ;
                  fstruct.l_start = posStart ;
                  fstruct.l_len = numBytes ;
               #endif
               #ifdef S4MULTIC4
                  if ((pid = fork())<0)
                     return error4(c4, e4lock, E80609);
                  if (pid ==0)               /* Child fork() */
                  {
                     #ifndef S4NO_LOCKF
                        if ( numAttempts == WAIT4EVER )    /* sleep until lock */
                           rc = lockf( file->hand, F_LOCK, numBytes ) ;
                        else
                           rc = lockf( file->hand, F_TLOCK, numBytes ) ;
                     #else
                        if ( numAttempts == WAIT4EVER )    /* sleep until lock */
                           rc = fcntl( file->hand, F_SETLKW, &fstruct ) ;
                        else
                           rc = fcntl( file->hand, F_SETLK, &fstruct ) ;
                     #endif
                     exit(rc);
                  }
                  while (wait(&status)!=pid)    /* Parent fork() */
                     ;
                  if ((WIFEXITED(status)==0))   /* If it didn't exit properly */
                     return error4(c4, e4lock, E80609);
                  if ((rc=WEXITSTATUS(status)) == 0)   /*IF it was able to lock*/
                  {
                  #ifndef S4NO_LOCKF
                     if ( numAttempts == WAIT4EVER )    /* sleep until lock */
                        rc = lockf( file->hand, F_LOCK, numBytes ) ;
                     else
                        rc = lockf( file->hand, F_TLOCK, numBytes ) ;
                  #else
                     if ( numAttempts == WAIT4EVER )    /* sleep until lock */
                        rc = fcntl( file->hand, F_SETLKW, &fstruct ) ;
                     else
                        rc = fcntl( file->hand, F_SETLK, &fstruct ) ;
                  #endif
                  }
               #else
                  #ifndef S4NO_LOCKF
                     if ( numAttempts == WAIT4EVER )    /* sleep until lock */
                        rc = lockf( file->hand, F_LOCK, numBytes ) ;
                     else
                        rc = lockf( file->hand, F_TLOCK, numBytes ) ;
                  #else
                     if ( numAttempts == WAIT4EVER )    /* sleep until lock */
                        rc = fcntl( file->hand, F_SETLKW, &fstruct ) ;
                     else
                        rc = fcntl( file->hand, F_SETLK, &fstruct ) ;
                  #endif
               #endif  /*  !S4MULTIC4  */
            #endif  /*  S4UNIX  */

            #ifdef S4IBMOS2
               L4RANGE.FileOffset = posStart ;
               L4RANGE.RangeLength = numBytes ;

               rc = DosSetFileLocks( (HFILE) file->hand, 0L,(PFILELOCK) &L4RANGE, 100L, 0L ) ;
            #endif

            #ifdef S4MACINTOSH
               MACParmBlk.ioParam.ioRefNum = file->hand ;
               MACParmBlk.ioParam.ioReqCount = numBytes ;
               MACParmBlk.ioParam.ioPosMode = fsFromStart ;
               MACParmBlk.ioParam.ioPosOffset = posStart ;

               PBLockRangeSync( &MACParmBlk ) ;
               rc = MACParmBlk.ioParam.ioResult ;
            #endif

            #ifdef S4WIN32
               if ( LockFile( (HANDLE)file->hand, posStart, posStartHi, numBytes, numBytesHi ) )
                  rc = NO_ERROR ;
               else
                  rc = GetLastError() ;
            #else
               #ifdef __TURBOC__
                  rc = lock( file->hand, posStart, numBytes ) ;
               #endif
            #endif
         #endif

         #ifdef __ZTC__
            if (rc == 0 || errno == 1)
         #else
            #ifdef S4IBMOS2
               if (rc == NO_ERROR )
            #else
               #ifdef S4MACINTOSH
                  if (rc == 0 )
               #else
                  #ifdef S4WIN32
                     if (rc == NO_ERROR )
                  #else
                     if (rc == 0 || errno == EINVAL)
                  #endif
               #endif
            #endif
         #endif
            {
               #ifdef S4LOCK_CHECK
                  l4lock_save( file->hand, posStart, numBytes ) ;
               #endif
               return 0 ;
            }

         if ( rc == 0 )
         {
            #ifdef S4LOCK_CHECK
               l4lock_save( file->hand, posStart, numBytes ) ;
            #endif
            #ifndef S4OPTIMIZE_OFF
               file4setWriteOpt( file, 1 ) ;
            #endif
            return 0 ;
         }

         #ifdef S4WIN32
            if ( rc != ERROR_LOCK_VIOLATION )
         #else
            #ifdef _MSC_VER
               #if _MSC_VER == 600
                  #ifdef S4WINDOWS
                     if (errno != -1 )       /* Microsoft 6.00a does not return */
                  #else                      /* valid 'errno' under Windows,    */
                     if (errno != EACCES)    /* but performs locking correctly  */
                  #endif
               #else
                  if (errno != EACCES)
               #endif
            #else
               #ifdef __ZTC__
                  if (errno != 33)
               #else
                  #ifdef S4IBMOS2
                     if ( rc != ERROR_LOCK_VIOLATION )
                  #else
                     #ifdef S4MACINTOSH
                        if ( (rc != fLckdErr ) && (rc != afpRangeOverlap ) )
                     #else
                        if ((errno != EACCES) && (errno != EAGAIN) && (errno != 0) )
                     #endif
                  #endif
               #endif
            #endif
         #endif
            return error4describe( c4, e4lock,E90613,  file->name, (char *)0, (char *)0 ) ;

         #ifdef S4LOCK_HOOK
            rc = code4lockHook( c4, file->name, 0, 0, -2, ++numAttempts ) ;
            if( rc != 0 )
               return rc ;
         #else
            if ( numAttempts == 1 )
               return r4locked ;
            if ( numAttempts > 1 )
               numAttempts-- ;
         #endif
         #ifdef S4TEMP
            if ( d4display_quit( &display ) )
               return error4( c4, e4result, E80604 ) ;
         #endif

         u4delayHundredth( (unsigned int)c4->lockDelay ) ;   /* delay & try lock again */
      }
   #endif
}

#ifdef S4LOCK_MODE_REQD
   /* tests for workaround to Novell same process lock inconsistency/bug */
   /* returns '1' if overlapping locks are allowed by same process on file */
   int file4lockMode( FILE4 *file )
   {
      int rc, lockMode, oldLockAttempts ;
      CODE4 *c4 ;

      if ( file->hand == INVALID4HANDLE )
         return -1 ;

      c4 = file->codeBase ;

      lockMode = 0 ;
      oldLockAttempts = c4->lockAttempts ;
      c4->lockAttempts = 1 ;

      if ( file4lock( file, 0x50000000L, 2L ) != 0 )
      {
         c4->lockAttempts = oldLockAttempts ;
         return -1 ;
      }
      rc = file4lock( file, 0x50000000L, 1L ) ;
      if ( rc == 0 )
      {
         lockMode = 1 ;
         file4unlock( file, 0x50000000L, 1L ) ;
      }
      if ( rc < 0 )
         lockMode = rc ;

      file4unlock( file, 0x50000000L, 2L ) ;

      c4->lockAttempts = oldLockAttempts ;
      return lockMode ;
   }
#endif

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION file4unlockInternal( FILE4 *file, unsigned long posStart, long posStartHi, unsigned long numBytes, long numBytesHi )
{
   #ifndef S4SINGLE
      int rc ;

      #ifdef S4IBMOS2
         struct LockStrc
         {
            long FileOffset ;
            long RangeLength ;
         } L4RANGE ;
      #endif

      #ifdef S4MACINTOSH
         ParamBlockRec MACParmBlk ;
      #endif
      #ifdef S4NO_LOCKF
         struct flock fstruct ;
      #endif

      #ifdef E4PARM_HIGH
         if ( file == 0 )
            return error4( 0, e4parm_null, E90614 ) ;
         // AS Sep 19/01 - S4MDX does support negative locking if the o/s supports it, so don't do this test
         // AS July 25/01 - Was checking this if NOT MDS, instead of if it was MDX, t4fail was failing
         // #ifndef S4MDX
         // #ifdef S4MDX
         //    if ( (long)numBytes < 0 || (long)posStart < 0 )  // in MDX negative locking (i.e. very high byte locking) is not multi-user compatible with dBase
         //       return error4( file->codeBase, e4parm, E90614 ) ;
         // #endif
      #endif

      /* AS 01/06/98 - relaxed isReadOnly contraint.  Will lock even then.
         Generally f4lock() would only get called if user explicitly requested
         it;  they may want to force a lock, so allow them to do so.

         AS 01/04/00 - if open in DENY_WRITE mode, we want to still enforce locks.
         this is because OLE-DB and ODBC perform locks to ensure records are committed.
         if locks are not placed on writes, the other client may place locks on the
         record but the value can still be changed or rolled back because no locks
         are actually placed.  t4odbc4.c displays problem.

         if ( numBytes == 0 || file->lowAccessMode != OPEN4DENY_NONE )
            return 0 ;
      */

      // AS July 16/02 - if the file is opened exclusive, we shouldn't be calling this function (handled at a higher level)
      // otherwise we can get into deadlock with ourselves (when opening clones of data files)
      // I have been fixing this for fox, other indexes may need some work yet...
      assert5( file->lowAccessMode != OPEN4DENY_RW ) ;
      if ( numBytes == 0 || file->lowAccessMode == OPEN4DENY_RW )
         return 0 ;

      #ifndef S4OPTIMIZE_OFF
         file4setWriteOpt( file, 0 ) ;
      #endif

      #ifdef S4LOCK_CHECK
         l4lockRemove( file->hand, posStart, numBytes ) ;
      #endif

      #ifndef S4WIN32
         errno = 0 ;
      #endif

      #ifdef S4LOCKING
         #ifdef S4WINDOWS
            _llseek( file->hand, posStart, 0 ) ;
         #else
            lseek( file->hand, posStart, 0 ) ;
         #endif
         rc = locking( file->hand, LK_UNLCK, numBytes ) ;
      #else
         #ifdef S4UNIX
            #ifndef S4NO_LOCKF /* lockf() replaces locking() for SUN OS, AT&T */
               lseek( file->hand, posStart, 0 ) ;
               rc = lockf( file->hand, F_ULOCK, numBytes ) ;
            #else
               fstruct.l_type = F_UNLCK ;
               fstruct.l_whence = SEEK_SET ;
               fstruct.l_start = posStart ;
               fstruct.l_len = numBytes ;

               rc = fcntl( file->hand, F_SETLK, &fstruct ) ;
            #endif
         #endif

         #ifdef S4IBMOS2
            L4RANGE.FileOffset = posStart ;
            L4RANGE.RangeLength = numBytes ;

            rc = DosSetFileLocks( (HFILE)file->hand, (PFILELOCK)&L4RANGE, 0L, 100L , 0L ) ;
         #endif

         #ifdef S4MACINTOSH
            MACParmBlk.ioParam.ioRefNum = file->hand ;
            MACParmBlk.ioParam.ioReqCount = numBytes ;
            MACParmBlk.ioParam.ioPosMode = fsFromStart ;
            MACParmBlk.ioParam.ioPosOffset = posStart ;

            PBUnlockRangeSync( &MACParmBlk ) ;
            rc = MACParmBlk.ioParam.ioResult ;
         #endif

         #ifdef S4WIN32
            if ( UnlockFile( (HANDLE)file->hand, posStart, posStartHi, numBytes, numBytesHi ) )
               rc = NO_ERROR ;
            else
               rc = -1 ;
         #else
            #ifdef __TURBOC__
               rc = unlock( file->hand, posStart, numBytes ) ;
            #endif
         #endif
      #endif

      #ifdef __ZTC__
         if ( rc < 0  && errno != 1 )
      #else
         #if defined(S4IBMOS2) || defined(S4MACINTOSH)
            if (rc != 0 )
         #elif defined(S4WIN32)
            if( rc != NO_ERROR )
         #else
            if ( rc < 0  && errno != EINVAL )
         #endif
      #endif
         return error4describe( file->codeBase, e4unlock, E90614, file->name, (char *)0, (char *)0 ) ;
   #endif

   return 0 ;
}
