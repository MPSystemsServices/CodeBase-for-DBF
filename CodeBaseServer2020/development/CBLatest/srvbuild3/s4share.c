/* s4share.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

// used for shared memory...

#include "d4all.h"

#if defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) )
   void share4initUndo( SHARE4 *share )
   {
      // the input share is invalid after this function exits
      if ( share == 0 )
         return ;

      CloseHandle( share->sharedMemoryHandle ) ;
      if ( share->supportsLocking )
         file4close( &share->locker ) ;
      if ( share->wasU4alloc )
         u4free( share ) ;
      else
         free( share ) ;

   }



   SHARE4 *share4init( CODE4 *c4, const char *name, int size, Bool5 supportLocking )
   {
      // use a temporary file for the mapping...
      // call share4initUndo when done with share
      // AS Mar 26/03 - Added ability to support locking

      SHARE4 *share ;

      if ( c4 == 0 )  // in this case, c4 not initialized, cannot use u4alloc for allocating
      {
         share = (SHARE4 *)malloc( sizeof( SHARE4 ) ) ;
         if ( share == 0 )
            return 0 ;
         share->wasU4alloc = 0 ;
      }
      else
      {
         share = (SHARE4 *)u4allocFree( c4, sizeof( SHARE4 ) ) ;
         if ( share == 0 )
            return 0 ;
         share->wasU4alloc = 1 ;
      }

      // this function works even if already exists (just opens in that case)
      share->sharedMemoryHandle = CreateFileMapping( (HANDLE)0xFFFFFFFF, 0, PAGE_READWRITE, 0, size, name ) ;
      if ( share->sharedMemoryHandle == 0 )   // failure
      {
         if ( share->wasU4alloc )
            u4free( share ) ;
         else
            free( share ) ;
         return 0 ;
      }

      share->maxLen = size ;
      if ( supportLocking == 1 )  // support by creating a physical file for locking of the same name.
      {
         int oldAccessMode = c4->accessMode ;
         c4->accessMode = OPEN4DENY_NONE ;
         int oldErrOpen = c4->errOpen ;
         c4->errOpen = 0 ;
         int rc = file4openInternal( &share->locker, c4, name, 0, OPT4NONE ) ;
         if ( rc != 0 ) // try creating
         {
            int oldErrCreate = c4getErrCreate( c4 ) ;
            c4setErrCreate( c4, 0 ) ;
            rc = file4createInternal( &share->locker, c4, name, 0, OPT4NONE ) ;
            c4setErrCreate( c4, oldErrCreate ) ;
         }
         c4->errOpen = oldErrOpen ;
         c4->accessMode = oldAccessMode ;
         if ( rc != 0 )
         {
            if ( share->wasU4alloc )
               u4free( share ) ;
            else
               free( share ) ;
            return 0 ;
         }
         share->supportsLocking = 1 ;
      }
      else
         share->supportsLocking = 0 ;

      // AS Apr 24/03 - Store the system's memory allocation granularity (previously just used '8'
      SYSTEM_INFO info ;
      GetSystemInfo( &info ) ;
      share->allocationGranularity = info.dwAllocationGranularity ;

      return share ;
   }



   int share4putData( SHARE4 *share, int offset, void *buffer, int lenIn )
   {
      int base = offset / share->allocationGranularity ;
      base *= share->allocationGranularity ;
      int remainder = offset - base ;
      int len = remainder + lenIn ;

      if ( base + len > share->maxLen )
         return -1 ;

      void *data = MapViewOfFile( share->sharedMemoryHandle, FILE_MAP_WRITE, 0, base, len ) ;

      if ( data == 0 )
      {
         #ifdef E4ANALYZE
            // allow to retrieve in debugger.
            DWORD res = GetLastError() ;
         #endif
         return -1 ;
      }

      int rc = 0 ;

      // must use exception handling to access shared memory
      try
      {
         memcpy( (char *)data + remainder, buffer, lenIn ) ;
      }
      catch( ... )
      {
         rc = -1 ;
      }

      UnmapViewOfFile( data ) ;


      return rc ;
   }



   int share4getData( SHARE4 *share, int offset, void *buffer, int lenIn )
   {
      // must be on an  share->allocationGranularity byte alignment...
      int base = offset / share->allocationGranularity ;
      base *=  share->allocationGranularity ;
      int remainder = offset - base ;
      int len = remainder + lenIn ;

      if ( base + len > share->maxLen )
         return -1 ;

      void *data = MapViewOfFile( share->sharedMemoryHandle, FILE_MAP_READ, 0, base, len ) ;

      if ( data == 0 )
         return -1 ;

      int rc = 0 ;

      // must use exception handling to access shared memory
      try
      {
         memcpy( buffer, (char *)data + remainder, lenIn ) ;
      }
      catch( ... )
      {
         rc = -1 ;
      }

      UnmapViewOfFile( data ) ;

      return 0 ;
   }



   // AS Mar 25/03 - New functions added for locking/unlocking
   int share4lock( SHARE4 *share, long lockByte )
   {
      #ifdef E4ANALYZE
         if ( share->holdsLock == 1 && lockByte == TRAN4LOCK_START_COMMIT )  // should never be re-locking TRAN4LOCK_START_COMMIT if we hold the lock
            return error4( 0, e4parm, E96701 ) ;
      #endif
      if ( share->supportsLocking == 0 )
         return error4( 0, e4parm, E96701 ) ;
      int rc = file4lockInternal( &share->locker, lockByte, 0, 1, 0 ) ;
      #ifdef E4ANALYZE
         if ( rc == 0 && lockByte == TRAN4LOCK_START_COMMIT )
            share->holdsLock = 1 ;
      #endif
      #ifdef S4OLEDEBUG_PRINT
         static char outBuf[255] ;
         sprintf( outBuf, "\r\nAttempt to lock byte %ld, result was %ld", lockByte, (long)rc ) ;
         log5( outBuf ) ;
      #endif
      return rc ;
   }



   // AS Mar 25/03 - New functions added for locking/unlocking
   int share4unlock( SHARE4 *share, long lockByte )
   {
      #ifdef E4ANALYZE
         if ( share->holdsLock == 0 && lockByte == TRAN4LOCK_START_COMMIT )  // should never be un-locking TRAN4LOCK_START_COMMIT if we don't hold the lock
            return error4( 0, e4parm, E96701 ) ;
      #endif
      if ( share->supportsLocking == 0 )
         return error4( 0, e4parm, E96701 ) ;
      int rc = file4unlockInternal( &share->locker, lockByte, 0, 1, 0 ) ;
      #ifdef E4ANALYZE
         if ( rc == 0 && lockByte == TRAN4LOCK_START_COMMIT )
            share->holdsLock = 0 ;
      #endif
      #ifdef S4OLEDEBUG_PRINT
         static char outBuf[255] ;
         sprintf( outBuf, "\r\nAttempt to unlock byte %ld, result was %ld", lockByte, (long)rc ) ;
         log5( outBuf ) ;
      #endif
      return rc ;
   }
#endif /* defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) ) */
