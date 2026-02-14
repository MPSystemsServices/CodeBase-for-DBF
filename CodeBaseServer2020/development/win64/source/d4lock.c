/* d4lock.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4CLIENT
   int d4localLockSet( DATA4 *data, const long rec )
   {
      LOCK4LINK *lock, *lockOn ;
      CODE4 *c4 ;
      DATA4FILE *dfile ;
      SINGLE4DISTANT singleDistant ;
      #ifdef E4MISC
         long recSave ;
      #endif

      #ifdef E4PARM_LOW
         if ( data == 0 || rec < 1L )
            return error4( 0, e4parm_null, E92723 ) ;
      #endif

      if ( d4lockTest( data, rec ) == 1 )
         return 0 ;

      c4 = data->codeBase ;
      dfile = data->dataFile ;

      #ifdef E4MISC
         /* verify the order of the list */
         lock = (LOCK4LINK *)(dfile->lockedRecords.initIterate()) ;
         if ( lock != 0 )
            for ( ;; )
            {
               recSave = lock->recNo ;
               lock = (LOCK4LINK *)single4next( &lock->link ) ;
               if ( lock == 0 )
                  break ;
               if ( lock->recNo <= recSave )
                  return error4( c4, e4info, E91102 ) ;
            }
      #endif

      for ( lock = (LOCK4LINK *)(dfile->lockedRecords.initIterate()) ;; )
      {
         if ( lock == 0 )
            break ;
         if ( lock->data == data && lock->recNo == rec )
            return 0 ;
         lock = (LOCK4LINK *)single4next( &lock->link ) ;
      }

      if ( c4->lockLinkMemory == 0 )
      {
         c4->lockLinkMemory = mem4create( c4, c4->memStartLock, sizeof(LOCK4LINK), c4->memExpandLock, 0 ) ;
         if ( c4->lockLinkMemory == 0 )
            return e4memory ;
      }
      lock = (LOCK4LINK *)mem4allocNoZero( c4->lockLinkMemory ) ;
      if ( lock == 0 )
         return e4memory ;
      lock->data = data ;
      lock->recNo = rec ;

      single4distantInitIterate( &singleDistant, &dfile->lockedRecords ) ;
      for ( ;; )
      {
         lockOn = (LOCK4LINK *)single4distantToItem( &singleDistant ) ;
         if ( lockOn == 0 || lockOn->recNo > rec )
         {
            single4add( single4distantToSingle( &singleDistant ), &lock->link ) ;
            break ;
         }
         single4distantNext( &singleDistant ) ;
      }

      return 0 ;
   }
#endif

#ifdef S4CB51
   int S4FUNCTION d4lock_group( DATA4 *data, const long *recs, const int n_recs )
   {
      CODE4 *c4 ;
      #ifndef S4SINGLE
         int i, rc ;
      #endif

      #ifdef E4PARM_LOW
         if ( data == 0 || recs == 0 || n_recs < 0 )
            return error4( 0, e4parm_null, E92724 ) ;
      #endif

      c4 = data->codeBase ;

      #ifndef S4SINGLE
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         rc = d4lockTestFile( data ) ;
         if ( rc )
            return ( rc == 1 ? 0 : rc ) ;

         #ifndef S4CLIENT
            switch( code4unlockAuto( c4 ) )
            {
               case LOCK4ALL :
                  code4lockClear( c4 ) ;
                  rc = error4code( c4 ) ;
                  break ;
               case LOCK4DATA :
                  rc = d4unlockLow( data, data4clientId( data ), 0 ) ;
                  break ;
            }
            if( rc < 0 )
               return error4stack( c4, rc, E92724 ) ;
         #endif

         for ( i = 0 ; i < n_recs ; i++ )
         {
            rc = d4lockAdd( data, recs[i] ) ;
            if ( rc != 0 )
            {
               code4lockClear( c4 ) ;
               return rc ;
            }
         }

         return code4lock( c4 ) ;
      #else
         return 0 ;
      #endif
   }
#endif  /* S4CB51 */

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4lock( DATA4 *data, const long rec )
{
   return d4lockInternal( data, rec, 1 ) ;
}



int S4FUNCTION d4lockInternal( DATA4 *data, const long rec, Bool5 doUnlock, Lock4type lockType )
{
   // default setting for lockType is LOCK4WRITE
   #ifndef S4SINGLE
      int rc ;
      CODE4 *c4 ;
      #ifndef S4CLIENT
         Lock4 *lock ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92701 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 || rec < 1L || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
            return error4( 0, e4parm, E92701 ) ;
      #endif

      c4 = data->codeBase ;

      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      // AS 06/16/00 - was not checking for DENY_RW on data accessmode in this case
      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      #ifndef S4CLIENT
         // if file can be written to by others, we can only perform write-locks
         // AS 01/25/00 -- changed - if it can be LOCKED by others (i.e. non exclusive)
         // if ( data->dataFile->file.lowAccessMode == OPEN4DENY_NONE )
         if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW )
            lockType = lock4write ;
      #endif

      rc = d4lockTest( data, rec, lockType ) ;  /* if record or file already locked */
      if ( rc )  /* error or we have locked, or r4locked */
      {
         if ( rc == 1 )
            return 0 ;
         if ( rc == r4locked )
         {
            /* now, if another DATA4 has it locked, it may get unlocked below... */
            if ( doUnlock == 0 || code4unlockAuto( c4 ) != LOCK4ALL )  /* won't get unlocked in this case... */
            {
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
               else
                  return r4locked ;
            }
            #ifdef S4SERVER
               /* for server, ensure the lock is held by us if doUnlock is false */
               rc = dfile4lockTestFile( data->dataFile, 0, data4serverId( data ), lockType ) ;
               if ( rc == r4locked ) /* locked by another DATA4FILE, we won't get access to */
               {
                  if ( c4->lockAttempts == WAIT4EVER )
                     return error4( c4, e4lock, E81523 ) ;
                  else
                     return r4locked ;
               }
               if ( rc == 0 ) /* means nobody had it locked, must be a record lock */
                  if ( dfile4lockTest( data->dataFile, data4serverId( data ), rec, lock4any ) == 0 )  /* not for our serverId, so will fail */
                  {
                     if ( c4->lockAttempts == WAIT4EVER )
                        return error4( c4, e4lock, E81523 ) ;
                     else
                        return r4locked ;
                  }
            #endif
         }
         else
            return rc ;
      }

      #ifdef S4CLIENT
         /* verify that this app doesn't have it locked elsewhere */
         if ( data->dataFile->lockTest != 0 && code4unlockAuto( c4 ) != LOCK4ALL )
         {
            if ( c4->lockAttempts == WAIT4EVER )
               return error4( c4, e4lock, E81523 ) ;
            else
               return r4locked ;
         }
      #else
         rc = 0 ;
         if ( doUnlock )
         {
            switch( code4unlockAuto( c4 ) )
            {
               case LOCK4ALL :
                  rc = code4unlockDo( tran4dataList( data->trans ) ) ;
                  break ;

               case LOCK4DATA :
                  #ifndef S4INDEX_OFF
                     rc = dfile4unlockIndex( data->dataFile, data4serverId( data ) );
                     if( rc < 0 )
                        break ;
                  #endif
                  rc = d4unlockRecords( data, -1 ) ;
                  if ( rc == 0 )
                     rc = d4unlockAppend( data, -1 ) ;
                  break ;
               default:
                  break ;
            }
            if( rc < 0 )
               return error4stack( c4, rc, E92701 ) ;
         }
         #ifdef E4ANALYZE
            /* should never happen - our check above must have been inaccurate */
            if ( dfile4lockTestFileInternal( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) != 0 )
               return error4( c4, e4result, E92709 ) ;
         #endif
      #endif

      #ifdef S4CLIENT
         rc = dfile4lock( data->dataFile, data4clientId( data ), data4serverId( data ), rec, lockType ) ;
      #else
         if ( lockType == lock4write )
            rc = dfile4lock( data->dataFile, data4clientId( data ), data4serverId( data ), rec ) ;
         else
         {
            rc = 0 ;
            data->dataFile->recordLockReadCount++ ;
         }
      #endif
      if ( rc == 0 )
      {
         #ifdef S4CLIENT
            rc = d4localLockSet( data, rec ) ;
         #else
            if ( c4->lockMemory == 0 )
            {
               c4->lockMemory = mem4create( c4, c4->memStartLock, sizeof( Lock4 ), c4->memExpandLock, 0 ) ;
               if ( c4->lockMemory == 0 )
                  return -1 ;
            }

            lock = (Lock4 *)mem4allocNoZero( c4->lockMemory ) ;
            if ( lock == 0 )
               return -1 ;

            #ifdef S4SERVER
               lock->clientId = data4clientId( data ) ;
            #endif
            lock->data = data ;
            lock->recNum = rec ;
            lock->lockType = lockType ;

            data->lockedRecords.add( (Single4lock *)lock ) ;

            #ifdef S4LOCK_HASH
               data->dataFile->lockHash->add( lock ) ;
            #endif
         #endif
      }
      return rc ;
   #else
      return 0 ;
   #endif
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4lockAddAppend( DATA4 *d4 )
{
   #ifndef S4SINGLE
      LOCK4GROUP *lock ;
      CODE4 *c4 ;
      int rc ;

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm, E92719 ) ;
      #endif

      if ( ( rc = d4verify( d4, 1 ) ) < 0 )
         return rc ;

      c4 = d4->codeBase ;

      if ( c4->lockGroupMemory == 0 )
      {
         c4->lockGroupMemory = mem4create( c4, c4->memStartLock, sizeof(LOCK4GROUP), c4->memExpandLock, 0 ) ;
         if ( c4->lockGroupMemory == 0 )
            return 0 ;
      }

      lock = (LOCK4GROUP *)mem4allocNoZero( c4->lockGroupMemory ) ;
      if ( lock == 0 )
         return e4memory ;

      lock->data = d4 ;
      lock->id.type = LOCK4APPEND ;
      lock->id.serverId = data4serverId( d4 ) ;
      lock->id.clientId = data4clientId( d4 ) ;
      single4add( &d4->trans->locks, &lock->link ) ;
   #endif

   return 0 ;
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4lockAddAll( DATA4 *d4 )
{
   #ifndef S4SINGLE
      LOCK4GROUP *lock ;
      CODE4 *c4 ;
      int rc ;

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm, E92725 ) ;
      #endif

      if ( ( rc = d4verify( d4, 1 ) ) < 0 )
         return rc ;

      c4 = d4->codeBase ;

      if ( c4->lockGroupMemory == 0 )
      {
         c4->lockGroupMemory = mem4create( c4, c4->memStartLock, sizeof(LOCK4GROUP), c4->memExpandLock, 0 ) ;
         if ( c4->lockGroupMemory == 0 )
            return 0 ;
      }

      lock = (LOCK4GROUP *)mem4allocNoZero( c4->lockGroupMemory ) ;
      if ( lock == 0 )
         return error4stack( c4, e4memory, E92725 ) ;

      lock->data = d4 ;
      lock->id.type = LOCK4ALL ;
      lock->id.serverId = data4serverId( d4 ) ;
      lock->id.clientId = data4clientId( d4 ) ;
      single4add( &d4->trans->locks, &lock->link ) ;
   #endif

   return 0 ;
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4lockAddFile( DATA4 *d4 )
{
   #ifndef S4SINGLE
      LOCK4GROUP *lock ;
      CODE4 *c4 ;
      int rc ;

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm, E92720 ) ;
      #endif

      if ( ( rc = d4verify( d4, 1 ) ) < 0 )
         return rc ;

      c4 = d4->codeBase ;

      if ( c4->lockGroupMemory == 0 )
      {
         c4->lockGroupMemory = mem4create( c4, c4->memStartLock, sizeof(LOCK4GROUP), c4->memExpandLock, 0 ) ;
         if ( c4->lockGroupMemory == 0 )
            return 0 ;
      }

      lock = (LOCK4GROUP *)mem4allocNoZero( c4->lockGroupMemory ) ;
      if ( lock == 0 )
         return error4stack( c4, e4memory, E92720 ) ;

      lock->data = d4 ;
      lock->id.type = LOCK4FILE ;
      lock->id.serverId = data4serverId( d4 ) ;
      lock->id.clientId = data4clientId( d4 ) ;
      single4add( &d4->trans->locks, &lock->link ) ;
   #endif

   return 0 ;
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4lockAdd( DATA4 *d4, long rec )
{
   #ifndef S4SINGLE
      LOCK4GROUP *lock ;
      CODE4 *c4 ;
      int rc ;

      #ifdef E4PARM_HIGH
         if ( d4 == 0 || rec < 1L )
            return error4( 0, e4parm, E92721 ) ;
      #endif

      if ( ( rc = d4verify( d4, 1 ) ) < 0 )
         return rc ;

      c4 = d4->codeBase ;

      if ( c4->lockGroupMemory == 0 )
      {
         c4->lockGroupMemory = mem4create( c4, c4->memStartLock, sizeof(LOCK4GROUP), c4->memExpandLock, 0 ) ;
         if ( c4->lockGroupMemory == 0 )
            return error4stack( c4, e4memory, E92721 ) ;
      }

      lock = (LOCK4GROUP *)mem4allocNoZero( c4->lockGroupMemory ) ;
      if ( lock == 0 )
         return error4stack( c4, e4memory, E92721 ) ;

      lock->data = d4 ;
      lock->id.type = LOCK4RECORD ;
      lock->id.recNum = rec ;
      lock->id.serverId = data4serverId( d4 ) ;
      lock->id.clientId = data4clientId( d4 ) ;

      single4add( &d4->trans->locks, &lock->link ) ;
   #endif

   return 0 ;
}

int S4FUNCTION d4lockAll( DATA4 *data )
{
   return d4lockAllInternal( data, 1 ) ;
}

#ifdef S4CLIENT
   int S4FUNCTION d4lockAllInternal( DATA4 *data, Bool5 doUnlock )
   {
      int rc ;

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm, E92702 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      rc = d4lockTestFile( data ) ;
      if ( rc )
         return ( rc == 1 ? 0 : rc ) ;

      rc = dfile4lockAll( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
      if ( rc == 0 )
         data->dataFile->fileLock = data ;
      return rc ;
   }
#else
   /* locks database, memo, and index files */
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4FUNCTION d4lockAllInternal( DATA4 *data, Bool5 doUnlock )
   {
      #ifndef S4SINGLE
         int rc, rc2 ;

         #ifdef E4PARM_HIGH
            if ( data == 0 )
               return error4( 0, e4parm, E92702 ) ;
         #endif

         if ( error4code( data->codeBase ) < 0 )
            return e4codeBase ;

         /* d4lockFile will perform an auto unlock if required */
         rc = d4lockFileInternal( data, doUnlock ) ;

         #ifndef S4CLIPPER
            #ifndef S4MEMO_OFF
               if ( !rc )
                  if ( data->dataFile->nFieldsMemo > 0 )
                     rc = dfile4lockMemo( data->dataFile ) ;
            #endif
         #endif

         #ifndef S4INDEX_OFF
            if ( !rc )
               rc = d4lockIndex( data ) ;
         #endif

         if( rc )
         {
            switch( code4unlockAuto( data->codeBase ) )
            {
               case LOCK4ALL :
                  rc2 = code4unlockDo( tran4dataList( data->trans ) ) ;
                  break ;
               case LOCK4DATA :
                  rc2 = d4unlockLow( data, data4clientId( data ), 0 ) ;
                  break ;
               default:
                  rc2 = 0 ;
                  break ;
            }
            if( rc2 < 0 )
               return rc2 ;
         }

         return rc ;
      #else
         return 0 ;
      #endif
   }
#endif

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4lockAppend( DATA4 *data )
{
   return d4lockAppendInternal( data, 1 ) ;
}

int S4FUNCTION d4lockAppendInternal( DATA4 *data, Bool5 doUnlock )
{
   #ifdef S4SINGLE
      return 0 ;
   #else
      CODE4 *c4 ;
      int rc ;
      #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
         double tempDbl ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92708 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm, E92708 ) ;
      #endif

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      // AS 06/16/00 - was not checking for DENY_RW on data accessmode in this case
      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      #ifdef S4CLIENT
         rc = dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) ;
      #else
         rc = dfile4lockTestFileInternal( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) ;
         if ( rc == 0 )  // try read-locks as well - we want a write lock, so a read lock will conflict as well
            rc = dfile4lockTestFileInternal( data->dataFile, data4clientId( data ), data4serverId( data ), lock4read ) ;
      #endif

      if ( rc )  /* error or we have locked, or r4locked */
      {
         if ( rc == 1 )
            return 0 ;
         if ( rc == r4locked )
         {
            /* now, if another DATA4 has it locked, it may get unlocked below... */
            if ( doUnlock == 0 || code4unlockAuto( c4 ) != LOCK4ALL )  /* won't get unlocked in this case... */
            {
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
               else
                  return r4locked ;
            }
            #ifdef S4CLIENT
               if ( dfile4lockTestFile( data->dataFile, 0, data4serverId( data ), lock4write ) == 0 )
            #else
               if ( dfile4lockTestFileInternal( data->dataFile, 0, data4serverId( data ), lock4write ) == 0 )
            #endif
            {
               /* our serverId doesn't have it locked, it will fail */
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
               else
                  return r4locked ;
            }
         }
         else
            return rc ;
      }

      rc = dfile4lockTestAppend( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
      if ( rc )  /* error or we have locked, or r4locked */
      {
         if ( rc == 1 )
            return 0 ;
         if ( rc == r4locked )
         {
            /* now, if another DATA4 has it locked, it may get unlocked below... */
            if ( doUnlock == 0 || code4unlockAuto( c4 ) != LOCK4ALL )  /* won't get unlocked in this case... */
            {
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
               else
                  return r4locked ;
            }
            if ( dfile4lockTestAppend( data->dataFile, 0, data4serverId( data ) ) == 0 ) /* our serverId doesn't have it locked, it will fail */
            {
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
               else
                  return r4locked ;
            }
         }
         else
            return rc ;
      }

      #ifdef S4CLIENT
         rc = dfile4lockAppend( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
         if ( rc == 0 )
            data->dataFile->appendLock = data ;
         #ifdef E4LOCK
            return request4lockTest( d4, LOCK4APPEND, 0L, rc ) ;
         #else
            return rc ;
         #endif
      #else
         /* in append case, unlock records only with unlock auto in order to avoid memo reset */
         /* changed 06/16 because is failing on lock due to not unlocking */
         if ( doUnlock )
            switch( code4unlockAuto( c4 ) )
            {
               case LOCK4ALL :
                  if ( code4unlockDo( tran4dataList( data->trans ) ) != 0 )
                     return error4stack( c4, e4unlock, E92709 ) ;
                  break ;
               case LOCK4DATA :
                  #ifndef S4INDEX_OFF
                     if ( dfile4unlockIndex( data->dataFile, data4serverId( data ) )!= 0 )
                        return error4stack( c4, e4unlock, E92709 ) ;
                  #endif
                  if ( d4unlockRecords( data, -1 ) != 0 )
                     return error4stack( c4, e4unlock, E92709 ) ;
                  break;
               default:
                  break ;
            }

         #ifdef E4ANALYZE
            /* should never happen - our check above must have been inaccurate */
            if ( dfile4lockTestFileInternal( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) != 0 )
               return error4( c4, e4result, E92709 ) ;
         #endif
         rc = dfile4lockAppend( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
         // AS Oct 29/02 - don't assign automatically if using the utilities since we need to use the
         // actual source value in that case.
         #if defined( S4FOX ) && !defined( S4OFF_WRITE ) && !defined( S4UTILS )
            if ( rc == 0 )  // look to see if this is an auto-increment field...
            {
               // AS Sept 27/02 - if the file is open exclusively, we do not read the data because it will
               // not have been updated (don't lose the previous value)
               if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW && data->autoIncrementField != 0 )
               {
                  #ifndef S4OFF_MULTI
                     // in non-multi-user, need to refresh this value from disk...
                     #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
                        /* LY 2003/07/10 : restored tempDbl */
                        double tempDbl = dfile4getAutoIncrementValue( data->dataFile ) ;
                        memcpy( data->dataFile->autoIncrementVal, (char*)&tempDbl, sizeof(double) ) ;
                     #else
                        data->dataFile->autoIncrementVal = dfile4getAutoIncrementValue( data->dataFile ) ;
                     #endif
                  #endif
                  // AS Dec 9/02 - use centralized coding for this...
                  // AS - Don't actually assign it at this point, assign in the append code
                  // d4assignAutoIncrementField( data ) ;
                  // #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
                  //    #ifdef S4DATA_ALIGN
                  //       tempDbl = x4reverseDouble( (double*)data->dataFile->autoIncrementVal ) ;
                  //    #else
                  //       tempDbl = x4reverseDouble( &data->dataFile->autoIncrementVal ) ;
                  //    #endif
                  //    memcpy( f4ptr( data->autoIncrementField ), &tempDbl, sizeof( double ) ) ;
                  // #else
                  //    memcpy( f4ptr( data->autoIncrementField ), &(data->dataFile->autoIncrementVal), sizeof( double ) ) ;
                  // #endif
               }
            }
         #endif
         return rc ;
      #endif
   #endif
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4lockFile( DATA4 *data )
{
    return d4lockFileInternal( data, 1 ) ;
}

int S4FUNCTION d4lockFileInternal( DATA4 *data, Bool5 doUnlock, Lock4type lockType )
{
   #ifdef S4SINGLE
      return 0 ;
   #else
      CODE4 *c4 ;
      int rc ;
      #ifndef S4CLIENT
         int supportsReadLocks ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92709 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
            return error4( 0, e4parm, E92709 ) ;
      #endif

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      // AS 06/16/00 - was not checking for DENY_RW on data accessmode in this case
      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      #ifndef S4CLIENT
         // if file can be written to by others, we can only perform write-locks
         // AS 01/25/00 -- changed - if it can be LOCKED by others (i.e. non exclusive)
         // we can only perform write-locks (due to ODBC/OLE DB isolation levels)
         // if ( data->dataFile->file.lowAccessMode == OPEN4DENY_NONE )
         if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW )
         {
            lockType = lock4write ;
            supportsReadLocks = 0 ;
         }
         else
            supportsReadLocks = 1 ;

         rc = dfile4lockTestFileInternal( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) ;
      #else
         rc = dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) ;
         #ifdef S4TESTING
            if ( rc == 0 && lockType == lock4read )  // maybe a read lock, for testing only
               rc = dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4read ) ;
         #endif
      #endif
      if ( rc )  /* error or we have locked, or r4locked */
      {
         if ( rc == 1 )
            return 0 ;
         if ( rc == r4locked )
         {
            /* now, if another DATA4 has it locked, it may get unlocked below... */
            if ( doUnlock == 0 || code4unlockAuto( c4 ) != LOCK4ALL )  /* won't get unlocked in this case... */
            {
               if ( c4->lockAttempts == WAIT4EVER )
                  return error4( c4, e4lock, E81523 ) ;
               else
                  return r4locked ;
            }
            #ifdef S4SERVER
               if ( dfile4lockTestFile( data->dataFile, 0, data4serverId( data ), lock4write ) != 1 ) /* our serverId doesn't have it locked, it will fail */
               {
                  if ( c4->lockAttempts == WAIT4EVER )
                     return error4( c4, e4lock, E81523 ) ;
                  else
                     return r4locked ;
               }
            #endif
         }
         else
            return rc ;
      }

      #ifdef S4CLIENT
         return dfile4lockFile( data->dataFile, data4clientId( data ), data4serverId( data ), data, lockType ) ;
      #else
         if ( supportsReadLocks )
         {
            // lockTestFile returns 1 if we hold read-lock on file, r4locked if someone else does
            rc = dfile4lockTestFileInternal( data->dataFile, data4clientId( data ), data4serverId( data ), lock4read ) ;
            if ( lockType == lock4read )
            {
               if ( rc == 1 )
                  return 0 ;
            }
            else  /* for lock4write, either we will unlock, or it will remain locked */
            {
               if ( rc == r4locked )
               {
                  // now, if another DATA4 has it locked, it may get unlocked below...
                  if ( doUnlock == 0 || code4unlockAuto( c4 ) != LOCK4ALL )  // won't get unlocked in this case...
                  {
                     if ( c4->lockAttempts == WAIT4EVER )
                        return error4( c4, e4lock, E81523 ) ;
                     else
                        return r4locked ;
                  }
                  #ifdef S4SERVER
                     if ( dfile4lockTestFile( data->dataFile, 0, data4serverId( data ), lock4read ) != 1 ) // our serverId doesn't have it locked, it will fail
                     {
                        if ( c4->lockAttempts == WAIT4EVER )
                           return error4( c4, e4lock, E81523 ) ;
                        else
                           return r4locked ;
                     }
                  #endif
               }
            }
         }

         if ( doUnlock )
         {
            switch( code4unlockAuto( c4 ) )
            {
               case LOCK4ALL :
                  rc = code4unlockDo( tran4dataList( data->trans ) ) ;
                  break ;
               case LOCK4DATA :
                  rc = d4unlockLow( data, data4clientId( data ), 0 ) ;
                  break ;
               default:
                  rc = 0 ;
                  break ;
            }
            if( rc < 0 )
               return error4stack( c4, e4unlock, E92709 ) ;
         }
         #ifdef E4ANALYZE
            /* should never happen - our check above must have been inaccurate */
            if ( dfile4lockTestFileInternal( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) != 0 )
               return error4( c4, e4result, E92709 ) ;
         #endif
         rc = dfile4lockFile( data->dataFile, data4clientId( data ), data4serverId( data ), lockType ) ;
         if ( lockType == lock4read )
         {
            // remove all of our read locks (all our locks)
            Single4distant singleDistant ;
            Lock4 *lock ;

            singleDistant.initIterate( &data->lockedRecords ) ;
            for ( ;; )
            {
               lock = (Lock4 *)((Single4lock *)singleDistant.toItem()) ;
               if ( lock == 0 )
                  break ;
               assert5( lock->lockType == lock4read ) ;
               #ifdef S4SERVER
                  if ( data4clientId( data ) == lock->clientId )
                  {
               #endif
                  #ifdef S4LOCK_HASH
                     data->dataFile->lockHash->remove( lock ) ;
                  #endif
                  singleDistant.remove() ;  /* update before freeing memory */
                  mem4free( data->codeBase->lockMemory, lock ) ;
               #ifdef S4SERVER
                  }
                  else
                     singleDistant.next() ;
               #endif
            }

            if ( c4->lockMemory == 0 )
            {
               c4->lockMemory = mem4create( c4, c4->memStartLock, sizeof( Lock4 ), c4->memExpandLock, 0 ) ;
               if ( c4->lockMemory == 0 )
                  return -1 ;
            }

            lock = (Lock4 *)mem4allocNoZero( c4->lockMemory ) ;
            if ( lock == 0 )
               return -1 ;

            #ifdef S4SERVER
               lock->clientId = data4clientId( data ) ;
            #endif
            lock->data = data ;
            data->dataFile->fileReadLocks.add( (Single4lock *)lock ) ;
         }

         return rc ;
      #endif
   #endif
}

#if !defined(S4SINGLE) && defined(S4CLIENT) && defined(E4LOCK)
   /* this function does a compare of a lock with the server.  client and server
      should always return the same result (input value) */
   static int request4lockTest( DATA4 *data, short int lockType, long rec, int clientVal )
   {
      CONNECTION4 *connection ;
      CONNECTION4LOCK_INFO_IN *info ;
      int rc ;
      long recNum ;

      #ifdef E4PARM_LOW
         if ( DATA == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
         {
            return error4( 0, e4parm, E92722 ) ;
         }
      #endif

      if ( error4code( data->codeBase )
         return -1 ;

      connection = data->dataFile->connection ;
      if ( connection == 0 )
         rc = e4connection ;
      else
      {
         connection4assign( connection, CON4LOCK, data4clientId( data ), data4serverId( data ) ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4LOCK_INFO_IN ), &info ) ;
         // AS Nov 30/01 lockType is long not short
         // info->type = htons5( lockType ) ;
         info->type = htonl5( lockType ) ;
         info->test = htons5( 1 ) ;
         info->lockType = lock4write ;
         if ( lockType == LOCK4RECORD )
         {
            recNum = htonl5(rec) ;
            connection4addData( connection, &recNum, sizeof( recNum ), NULL ) ;
         }
         connection4send( connection ) ;
         rc = connection4receive( connection ) ;
         if ( rc == 0 )
         {
            rc = connection4status( connection ) ;
            if ( rc < 0 )
               return connection4error( connection, data->codeBase, rc, E92722 ) ;
         }
      }
      if ( rc != clientVal )
         return error4( data->codeBase, e4info, E92722 ) ;

      return rc ;
   }
#endif

#ifdef S4LOCK_HASH
   /* returns '1' if we have locked, 'r4locked' if someone else has locked */
   #define data4hashLockTest( d4, recNo, lockType ) ((d4)->dataFile->lockHash->find( (d4), (recNo), (lockType) ) )
   /*
   {
       find returns 1 if we have locked, r4locked if someone else, 0 if nobody
      return data->dataFile->lockHash->find( data, recNo, lockType ) ;
   }
   */
#endif

/* for client, this function also checks the data's CODE4 to see if another
   access point for the DATA4 has the desired lock.
   If it does, data->dataFile->lockTest is set to the offending DATA4 */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
/* returns r4locked if someone else locally has locked, 1 if success, 0 if not locked */
int S4FUNCTION d4lockTest( DATA4 *data, const long recNo, Lock4type lockType )
{
   #ifndef S4SINGLE
      int rc ;
      #ifndef S4CLIENT
         #ifndef S4LOCK_HASH
            Lock4 *lock ;
         #endif
      #endif
      DATA4FILE *d4file ;

      #ifdef E4PARM_HIGH
         if ( data == 0 || ( lockType != lock4read && lockType != lock4write && lockType != lock4any ) )
            return error4( 0, e4parm, E92703 ) ;
      #endif

      d4file = data->dataFile ;

      #ifdef S4CLIENT
         rc = dfile4lockTest( d4file, data4clientId( data ), data4serverId( data ), recNo, lockType ) ;
         if ( rc < 0 )
            return rc ;
         #ifdef E4LOCK
            return request4lockTest( data, LOCK4RECORD, recNo, rc )  ;
         #endif
         return rc ;
      #else
         rc = dfile4lockTestFileInternal( d4file, data4clientId( data ), data4serverId( data ), lock4write ) ;
         if ( rc )
            return rc ;
         rc = dfile4lockTestFileInternal( d4file, data4clientId( data ), data4serverId( data ), lock4read ) ;
         if ( lockType == lock4read )
         {
            if ( rc == 1 )  // if we have it locked, we have an internal conflict.  r4locked means ok, someone else has read-locked
               return r4locked ;
         }
         else
         {
            if ( rc == 1 || rc == r4locked )  // if we have it locked, we have an internal conflict, return r4locked
               return r4locked ;
         }
         #ifdef S4LOCK_HASH
            rc = data4hashLockTest( data, recNo, lockType ) ;
            /* AS 08/26/98 expanded functionality to upgrade lock...
                (old) return r4locked ;

                if we are here, then we hold a read lock but nobody else holds
                a write lock...
            */
            return rc ;
         #else
            for ( lock = (Lock4 *)data->lockedRecords.initIterate() ;; )
            {
               if ( lock == 0 )
                  break ;
               if ( lock->recNum == recNo && ( lock->lockType == lockType || lockType == lock4any ) )
                  return 1 ;
               lock = (Lock4 *)lock->next() ;
            }

            /* otherwise must go through everyone else's locks as well, to get r4locked possibility */
            return dfile4lockTest( d4file, 0, recNo, lockType ) ;
         #endif
      #endif
   #else
      return 1 ;
   #endif
}

#ifdef S4DOS
/* If negative locking is not supported, _close() will fail after calling
   _lseek() with a large value. This is known to occur when running in the
   Windows NT console.

   If the test fails, this function returns e4lock.
   Otherwise, r4success is returned. */
   int d4negativeLockTest(CODE4 *c4)
   {
      #ifdef S4PARM_HIGH
         #ifndef S4OFF_MULTI
            #ifndef S4NO_NEGATIVE_LOCK
               if (c4->accessMode == OPEN4DENY_NONE && c4->readOnly == 0)
               {
                  int h = _sopen(tmpnam(0),_O_CREAT+_O_TRUNC+_O_RDWR,_SH_DENYNO);
                  if (h != -1)
                  {
                     int rc;
                     _lseek(h,0xEFFFFFFF,0);
                     rc = _close(h);
                     if (rc == -1)
                        return e4lock;
                  }
               }
            #endif
         #endif
      #endif
      return r4success;
   }
#endif

int S4FUNCTION d4lockTestAppend( DATA4 *data )
{
   #ifndef S4SINGLE
      int rc ;

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm, E92704 ) ;
      #endif

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 1 ;
      #endif

      rc = dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) ;
      if ( rc != 1 )
      {
         if ( rc != 0 )
            return ( rc == r4locked ? 0 : rc ) ;

         rc = dfile4lockTestAppend( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
         if ( rc != 1 )
            return ( rc == r4locked ? 0 : rc ) ;
      }

      #ifdef S4CLIENT
         #ifdef E4LOCK
            return request4lockTest( data, LOCK4APPEND, 0L, rc )  ;
         #endif
      #endif

      return rc ;
   #else
      return 1 ;
   #endif
}

#ifdef S4CLIENT
   int S4FUNCTION d4lockTestFile( DATA4 *data )
   {
      #ifndef S4SINGLE
         int rc ;

         #ifdef E4PARM_HIGH
            if ( data == 0 )
               return error4( 0, e4parm_null, E92705 ) ;
         #endif

         // AS 06/16/00 - was not checking for DENY_RW on data accessmode in this case
         #ifdef S4SERVER
            if ( data->accessMode == OPEN4DENY_RW )
               return 1 ;
         #endif

         rc = dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) ;

         #ifdef E4LOCK
            return request4lockTest( data, LOCK4FILE, 0L, rc ) ;
         #else
            return rc ;
         #endif
      #else
         return 1 ;
      #endif
   }
#endif /* S4CLIENT */
