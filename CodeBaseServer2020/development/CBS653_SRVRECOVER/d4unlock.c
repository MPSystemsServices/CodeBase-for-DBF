/* d4unlock.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_MULTI
static int d4hasLocks( DATA4 *data, long clientId, long serverId )
{
   assert5( data != 0 ) ;
   #ifdef S4CLIENT
      LOCK4LINK *lock ;

      #ifdef L4LOCK_CHECK
         at return time, make sure call server to see if the expected lock matches...
      #endif

      if ( serverId == 0 )   /* likely failed open */
         return 0 ;

      DATA4FILE *d4file = data->dataFile ;
      assert5( d4file != 0 ) ;
      if ( d4file->fileLock != data && d4file->appendLock != data )
      {
         for ( lock = (LOCK4LINK *)(d4file->lockedRecords.initIterate()) ;; )
         {
            if ( lock == 0 )
               return 0 ;
            if ( lock->data == data )
               return 1 ;
            lock = (LOCK4LINK *)single4next( &lock->link ) ;
         }
      }

      return 1 ;
   #else
      Single4lock *lock ;

      if ( serverId == 0 )   /* likely failed open */
         return 0 ;

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      DATA4FILE *d4file = data->dataFile ;
      assert5( d4file != 0 ) ;

      if ( ( d4file->fileServerWriteLock == serverId && ( d4file->fileClientWriteLock == clientId || clientId == 0 ) ) ||
           ( ( d4file->appendClientLock == clientId || clientId == 0 ) && d4file->appendServerLock == serverId ) )
         return 1 ;

      Lock4 *readFileLocks ;
      readFileLocks = (Lock4 *)((Single4lock *)(d4file->fileReadLocks.initIterate())) ;
      for ( ;; )
      {
         if ( readFileLocks == 0 )
            break ;
         if ( readFileLocks->data == data )
         {
            #ifdef S4SERVER
               if ( readFileLocks->clientId == clientId || clientId == 0 )
            #endif
               return 1 ;
         }
         readFileLocks = (Lock4 *)(Single4lock *)((Single4lock *)(readFileLocks))->next() ;
      }

      lock = (Single4lock *)(data->lockedRecords.initIterate()) ;
      #ifdef S4SERVER
         for ( ;; )
         {
            if ( lock == 0 )
               return 0 ;
            if ( ( ((Lock4 *)lock)->clientId == clientId || clientId == 0 ) )
               return 1 ;
            lock = (Single4lock *)(lock->next()) ;
         }
      #else
         return ( (lock == 0) ? 0 : 1 ) ;
      #endif
   #endif
}
#endif

/*
#ifdef S4CLIENT
void d4unlockClientData( DATA4 *data )
{
   DATA4FILE *dfile ;
   LOCK4LINK *lock, *nextLock ;

   #ifdef S4SERVER
      if ( data->accessMode == OPEN4DENY_RW )
         return ;
   #endif

   dfile = data->dataFile ;

   if ( dfile->fileLock == data )
   {
      data->dataFile->numRecs = -1 ;
      dfile->fileLock = 0 ;
   }
   if ( dfile->appendLock == data )
   {
      data->dataFile->numRecs = -1 ;
      dfile->appendLock = 0 ;
   }
   lock = (LOCK4LINK *)l4first( &dfile->lockedRecords ) ;
   if ( lock != 0 )
      for ( ;; )
      {
         nextLock = (LOCK4LINK *)l4next( &dfile->lockedRecords, lock ) ;
         if ( lock->data == data )
         {
            l4remove( &dfile->lockedRecords, lock ) ;
            mem4free( data->codeBase->lockLinkMemory, lock ) ;
         }
         if ( nextLock == 0 )
            break ;
         lock = nextLock ;
      }

   return ;
}
#endif
*/

#ifndef S4OFF_MULTI
/* clientId if set to 0 will unlock all client instance of the data file,
   if set to a value will only unlock the given client instance */
static int d4unlockDo( DATA4 *data, const long clientId, char doReqdUpdate )
{
   CODE4 *c4 ;
   #ifdef S4CLIENT
      int rc ;
      CONNECTION4 *connection ;
   #else
      int rc, saveUnlockAuto ;
   #endif
   #ifdef S4SERVER
      #ifndef S4OFF_COMMUNICATIONS
         unsigned short int mType ;
         long ID ;
      #endif /* S4OFF_COMMUNICATIONS */
   #endif

   c4 = data->codeBase ;

   #ifndef S4OFF_TRAN
      #ifndef S4OFF_WRITE
         if ( code4transEnabled( c4 ) )
            if ( code4tranStatus( c4 ) == r4active )
               #ifdef S4STAND_ALONE
                  if ( data->dataFile->file.isReadOnly != 1 )  // AS 04/10/00 allow to close if readOnly
               #endif
                     return error4( c4, e4transViolation, E92801 ) ;
      #endif
   #endif

   #ifdef S4CLIENT
      if ( doReqdUpdate == 0 )
         if ( d4hasLocks( data, clientId, data4serverId( data ) ) == 0 )  /* first make sure there are locks to undo */
            return 0 ;

      rc =  d4update( data ) ;  /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc < 0 )
         return error4stack( c4, (short int)rc, E92801 ) ;

      if ( d4hasLocks( data, clientId, data4serverId( data ) ) == 0 )  /* first make sure there are locks to undo */
         return 0 ;

      /* in case of rollback and exclusive files, make sure count set to -1 */
      data->dataFile->numRecs = -1 ;

      connection = data->dataFile->connection ;
      if ( connection == 0 )
         return error4stack( c4, e4connection, E92801 ) ;
      connection4assign( connection, CON4UNLOCK, clientId, data4serverId( data ) ) ;

      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E92801 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E92801 ) ;

      /* AS 01/09/97, since now have STREAM4UNLOCK_DATA handles, don't need this code */
      /* cb51 compat, remove lock */
      /* if ( code4trans( c4 )->unlockAuto == 2 ) */
      /*   d4unlockClientData( data ) ; */

      return rc ;
   #else
      #ifndef S4OFF_WRITE
         #ifndef S4OFF_TRAN
            if ( code4transEnabled( c4 ) )
               if ( code4tranStatus( c4 ) != r4inactive )
                  return 0 ;
         #endif

         if ( doReqdUpdate == 0 )
            if ( d4hasLocks( data, clientId, data4serverId( data ) ) == 0 )  /* first make sure there are locks to undo */
               return 0 ;

         rc =  d4update( data ) ;  /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc < 0 )
            return error4stack( c4, (short int)rc, E92801 ) ;
         if ( d4hasLocks( data, clientId, data4serverId( data ) ) == 0 )  /* first make sure there are locks to undo */
            return 0 ;
      #else
         rc = 0 ;
      #endif

      saveUnlockAuto = code4unlockAuto( c4 ) ;
      if ( saveUnlockAuto == 0 )   /* leave if 1 or 2 -- don't change 2 */
         code4unlockAutoSet( c4, 1 ) ;

      #ifdef S4SERVER
         d4unlockData( data, clientId ) ;
         #ifndef S4OFF_COMMUNICATIONS
            #ifdef S4JAVA
               if ( c4->currentClient->javaClient == 0 )
            #endif
               if ( code4unlockAuto( c4 ) == LOCK4DATA )
                  if ( c4->currentClient->isStream == 0 )
                  {
                     /* send a STREAM4UNLOCK_DATA message to the client to tell it to unlock stuff */
                     mType = htons5(STREAM4UNLOCK_DATA) ;
                     connection4send( &c4->currentClient->connection, &mType, sizeof( mType ) ) ;
                     /* also send the clientId and serverId */
                     ID = htonl5( data->clientId ) ;
                     connection4send( &c4->currentClient->connection, &ID, sizeof( ID ) ) ;
                     ID = htonl5( data->serverId ) ;
                     connection4send( &c4->currentClient->connection, &ID, sizeof( ID ) ) ;
                  }
         #endif /* S4OFF_COMMUNICATIONS */
      #else
         d4unlockData( data ) ;
      #endif
      #ifndef S4CLIPPER
         #ifndef S4OFF_MEMO
            dfile4memoUnlock( data->dataFile ) ;
         #endif
      #endif
      #ifndef S4OFF_INDEX
         dfile4unlockIndex( data->dataFile, data4serverId( data ) ) ;
      #endif

      code4unlockAutoSet( c4, saveUnlockAuto ) ;

      if ( error4code( c4 ) < 0 )
         return -1 ;
      return rc ;
   #endif
}
#endif /* S4OFF_MULTI */

#ifndef S4OFF_MULTI
/* AS 07/08/97 externally, d4unlock() must doReqdUpdate due to fix #89
   in changes.60 / manual documentation.  Internally, this causes problems,
   so internally doReqdUpdate is always false */
int S4FUNCTION d4unlockLow( DATA4 *data, long clientId, char doReqdUpdate )
{
   int rc ;

   #ifdef S4CLIENT
      int oldLock = code4unlockAuto( data->codeBase ) ;
      code4unlockAutoSet( data->codeBase, LOCK4DATA ) ;
   #endif
   rc = d4unlockDo( data, clientId, doReqdUpdate ) ;
   #ifdef S4CLIENT
      code4unlockAutoSet( data->codeBase, oldLock ) ;
   #endif

   return rc ;
}
#endif

#ifndef S4SERVER
#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4unlock( DATA4 *data )
{
   #ifndef S4OFF_MULTI
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92801 ) ;
      #endif

      return d4unlockLow( data, data4clientId( data ), 1 ) ;
   #else
      return 0 ;
   #endif
}
#endif

/*#ifdef S4STAND_ALONE*/
#ifndef S4CLIENT

/* only unlocks the append byte */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4unlockAppend( DATA4 *data, long clientId )
{
   #ifndef S4OFF_MULTI
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92802 ) ;
      #endif
      if ( code4unlockAuto( data->codeBase ) == LOCK4OFF )
         return 0 ;

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      if ( clientId == -1 )
         clientId = data4clientId( data ) ;
      return dfile4unlockAppend( data->dataFile, clientId, data4serverId( data ) ) ;
   #else
      return 0 ;
   #endif
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int d4unlockData( DATA4 *data, long clientId )
{
   #ifndef S4OFF_MULTI
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92803 ) ;
      #endif
      if ( code4unlockAuto( data->codeBase ) == LOCK4OFF )
         return 0 ;

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      d4unlockFile( data, clientId ) ;
      d4unlockAppend( data, clientId ) ;
      d4unlockRecords( data, clientId ) ;
      if ( error4code( data->codeBase ) < 0 )
         return error4code( data->codeBase ) ;
   #endif
   return 0 ;
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int d4unlockFile( DATA4 *data, long clientId )
{
   #ifndef S4OFF_MULTI
      int rc ;
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92804 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92804 ) ;
      #endif

      if ( code4unlockAuto( data->codeBase ) == LOCK4OFF )
         return 0 ;

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      if ( clientId == -1 )
         clientId = data4clientId( data ) ;

      Single4distant distant ;
      distant.initIterate( &data->dataFile->fileReadLocks ) ;

      for ( ;; )  // just remove the read file locks, no locking required
      {
         if ( distant.toItem() == 0 )
            break ;
         if ( ((Lock4 *)((Single4lock *)(distant.toItem())))->data == data )
         {
            #ifdef S4SERVER
               if ( ((Lock4 *)((Single4lock *)(distant.toItem())))->clientId == clientId || clientId == 0 )
            #endif
            {
               distant.remove() ;
               continue ;
            }
         }
         distant.next() ;
      }

      rc = dfile4unlockFile( data->dataFile, clientId, data4serverId( data ) ) ;
      if ( rc < 0 )
         return error4stack( data->codeBase, rc, E92804 ) ;

      data->recNumOld =  -1 ;
      #ifndef S4OFF_MEMO
         data->memoValidated =  0 ;
      #endif
   #endif
   return 0 ;
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4unlockRecord( DATA4 *data, long rec )
{
   #ifndef S4OFF_MULTI
      #ifndef S4CLIENT
         Lock4 *lock ;
         Single4distant singleDistant ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92805 ) ;
      #endif

      if ( code4unlockAuto( data->codeBase ) == LOCK4OFF )
         return 0 ;

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      if ( rec == data->recNum )
      {
         data->recNumOld =  -1 ;
         #ifndef S4OFF_MEMO
            data->memoValidated =  0 ;
         #endif
      }

      #ifndef S4CLIENT
         singleDistant.initIterate( &data->lockedRecords ) ;
         for ( ;; )
         {
            lock = (Lock4 *)((Single4lock *)singleDistant.toItem()) ;
            if ( lock == 0 )
               break ;
            if ( lock->recNum == rec )
            {
               if ( lock->lockType == lock4write )
               {
                  if ( dfile4unlockRecordDo( data->dataFile, rec ) < 0 )
                     return -1 ;
               }
               else
                  data->dataFile->recordLockReadCount-- ;
               singleDistant.remove() ;
               #ifdef S4LOCK_HASH
                  data->dataFile->lockHash->remove( lock ) ;
               #endif
               mem4free( data->codeBase->lockMemory, lock ) ;
               break ;
            }
            singleDistant.next() ;
         }
      #endif /* S4CLIENT */
   #endif
   return 0 ;
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int d4unlockRecords( DATA4 *data, long clientId )
{
   #ifndef S4OFF_MULTI
      #ifndef S4CLIENT
         Lock4 *lock ;
         Single4distant singleDistant ;
      #endif
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92806 ) ;
      #endif

      if ( code4unlockAuto( data->codeBase ) == LOCK4OFF )
         return 0 ;

      #ifdef S4SERVER
         if ( data->accessMode == OPEN4DENY_RW )
            return 0 ;
      #endif

      data->recNumOld = -1 ;
      #ifndef S4OFF_MEMO
         data->memoValidated =  0 ;
      #endif

      #ifdef S4CLIENT
         return dfile4unlockRecords( data->dataFile, data4clientId( data ), data4serverId( data ) ) ;
      #else
         if ( clientId == -1 )
            clientId = data4clientId( data ) ;

         singleDistant.initIterate( &data->lockedRecords ) ;
         for ( ;; )
         {
            lock = (Lock4 *)((Single4lock *)singleDistant.toItem()) ;
            if ( lock == 0 )
               break ;
            #ifdef S4SERVER
               if ( clientId == lock->clientId || clientId == 0 )
               {
            #endif
                  if ( lock->lockType == lock4write )  // only physically lock/unlcok write locks
                  {
                     if ( dfile4unlockRecordDo( data->dataFile, lock->recNum ) < 0 )
                     {
                        data->lockedRecords.init() ;   /* clear out for error case */
                        return -1 ;
                     }
                  }
                  else
                     data->dataFile->recordLockReadCount-- ;
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
         return 0 ;
      #endif /* S4CLIENT */
   #else
      return 0 ;
   #endif
}
#endif /* S4CLIENT */
/*#endif  */ /* S4STAND_ALONE */

#ifndef S4OFF_MULTI
int code4unlockDo( LIST4 *dataList )
{
   DATA4 *dataOn ;
   CODE4 *c4 ;
   #ifdef S4CLIENT
      int oldLock ;
   #endif
   #ifdef S4SERVER
      #ifndef S4OFF_COMMUNICATIONS
         unsigned short int mType ;
      #endif /* S4OFF_COMMUNICATIONS */
   #endif

   c4 = 0 ;

   #ifdef E4PARM_HIGH
      if ( dataList == 0 )
         return error4( 0, e4parm_null, E92807 ) ;
   #endif

   #ifdef S4CLIENT
      /* for client, any request with LOCK4ALL should cause complete
         unlocking of everything at the lower level.
         Therefore, only need to call on a single database--but that
         database better have a lock.  if none have locks, call is
         avioded. */
      for( dataOn = 0 ;; )
      {
         dataOn = (DATA4 *)l4next( dataList, dataOn ) ;
         if ( dataOn == 0 )
            break ;
         if ( d4hasLocks( dataOn, data4clientId( dataOn ), data4serverId( dataOn ) ) != 0 )
         {
            c4 = dataOn->codeBase ;
            oldLock = code4unlockAuto( c4 ) ;
            code4unlockAutoSet( c4, LOCK4ALL ) ;
            d4unlockDo( dataOn,data4clientId(dataOn), 1 ) ;
            code4unlockAutoSet( c4, oldLock ) ;
            break ;
         }
      }
   #else
      for ( dataOn = 0 ;; )
      {
         dataOn = (DATA4 *)l4next( dataList, dataOn ) ;
         if ( dataOn == 0 )
            break ;
         /* reset record count because this function is likely called due to a transaction rollback */
         d4unlockLow( dataOn, 0, 0 ) ;  // 0 for clientId to ensure all get unlocked
         c4 = dataOn->codeBase ;
      }
   #endif

   if ( c4 != 0 )
   {
      #ifdef S4SERVER
         #ifndef S4OFF_COMMUNICATIONS
            #ifdef S4JAVA
               if ( c4->currentClient->javaClient == 0 )
            #endif
               if ( c4->currentClient->connection.connect != 0 && c4->currentClient->isStream == 0)
               {
                  /* send a STREAM4UNLOCK_ALL message to the client to tell it to unlock stuff */
                  mType = htons5(STREAM4UNLOCK_ALL) ;
                  connection4send( &c4->currentClient->connection, &mType, sizeof( mType ) ) ;
               }
         #endif /* S4OFF_COMMUNICATIONS */
      #endif

      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;
   }
   return 0 ;
}
#endif  /* S4OFF_MULTI */

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION code4unlock( CODE4 *c4 )
{
   #ifdef S4OFF_MULTI
      return 0 ;
   #else
      #ifndef S4OFF_WRITE
         #ifndef S4OFF_TRAN
            if ( code4transEnabled( c4 ) )
               if ( code4tranStatus( c4 ) == r4active )
               return error4( c4, e4transViolation, E92807 ) ;
         #endif
      #endif

      #ifdef S4SERVER
         return server4clientUnlock( c4->currentClient ) ;
      #else
         return code4unlockDo( tran4dataList( (&(c4->c4trans.trans)) ) ) ;
      #endif
   #endif
}

