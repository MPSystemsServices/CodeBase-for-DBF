/* c4trans.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4OLEDEBUG_PRINT
   #include "util5.hpp"
#endif

/* TRANSACTION FILE FORMAT
   -----------------------

   Every entry in the transaction file has the following format:

   [unisgned short int] - transaction entry length (including this length entry)
   [transaction data]
   [transaction header]

   The transaction file can be read either forwards or backwards in this
   fashion.  It is usually read backwards when performing rollbacks, but
   it is useful to read forwards for database history review purposes or
   in case the transaction file itself is corrupt at some point.


   A valid not-in-use transaction file looks as follows:

   <initial TRAN4SHUTDOWN entry> - indicates is not a backup, and that is valid, in particular
                                   may be no other entries in the log file.

   - various entries

   <final TRAN4SHUTDOWN entry> - indicates transaction file was properly shut down

   AS Aug 13/01 - New capability to not log data into primary log file.
   This works by not creating the primary log file or using it at all.  As long as no transactions
   are performed everything works ok.  If a transaction is started the log file is enabled for the
   duration of the transaction only, in order to allow rollbacks, etc.  If no transaction ever occurs
   then the log file is never created.

   TRANSACTION FILE LOCKING
   ------------------------
   If the log file is opened exlusive (normal server case), there is no locking done.
   If the file is opened shared (Stand/Alone or ODBC Server), locking is done as follows:
      - every process (Code4) holds a lock to determine its userIdNo.  This is done by cycling through
        the locks starting at location TRAN4LOCK_USERS+1.  Once a lock is obtained, the userIdNo
        is recorded.  The lock is left in place to reserve the userIdNo.
      - when the log file is opened, tran4fileLowStatusFile is called to verify the integrity.  This
        is done by cycling through the user-id locks.  If all locks are obtained, it means we are the
        first opener of the log file, so the status is verified.  If we fail to obtain the lock, it
        is assumed the log file is valid since it is already in use.  In addition, a single lock is placed
        to ensure only 1 user is accessing the lock area.  This is primarily done due to Novell bugs whereby
        you could obtain a field lock (over several bytes) even if another user held individual locks within the field.
      - There exist 2 locking options with the shared log file - full-locking, or write-locking.  Basically, full-locking
        locks the transaction file for the duration of a transaction.  This is efficient, but significantly impairs
        multi-user transactions.  define S4TRANS_FULL_LOCK_OFF to use the less efficient method where the transaction
        file is locked only when being written to.  the implementation is such that code4tranLockTransactions is always
        called in the lowAppend call, if it is not already locked, and unlocked if we did lock it.  At the higher
        code4tranStart() level, we lock the transaction file if S4TRANS_FULL_LOCK_OFF is not defined, thus resulting
        in the full-transaction lock functionality.
*/

#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
   void tran4lowCloseDelayed( TRAN4 *trans )
   {
      DATA4 *data ;

      for ( ;; )
      {
         data = (DATA4 *)l4pop( &trans->closedDataFiles ) ;
         if ( data == 0 )
            break ;
         assert5( trans == data->trans ) ;
         // AS Mar 20/03 - There is a chance that the data->trans is NULL, why I don't know
         LIST4 *list = tran4dataList( trans ) ;
         #ifdef E4ANALYZE
            if ( list == 0 )
               error4( data->codeBase, e4info, E93801 ) ;
         #endif
         l4add( list, data ) ;
         d4close( data ) ;
      }
   }


   #ifndef S4CLIENT
      static DATA4 *tran4dataFull( TRAN4 *, const long, const long ) ;
   #endif
   #ifdef E4ANALYZE
      static int code4transVerify( CODE4TRANS *, int ) ;
   #endif
   #ifndef S4SERVER
      static void code4invalidate( CODE4 * ) ;
   #endif
#endif /*!defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( E4ANALYZE )
   static int tran4fileLowVerify( TRAN4FILE_LOW *t4, int subs )
   {
      int rc ;

      if ( t4 == 0 )
         return error4( 0, e4parm_null, E93801 ) ;

      // AS Aug 15/01 - Log file may be disabled, in which case continue on.
      if ( t4->isDisabled == log4disabled )
         return 0 ;

      if ( t4->c4trans == 0 )
         return error4( 0, e4parm, E93801 ) ;

      if ( subs == 1 )
         if ( ( rc = code4transVerify( t4->c4trans, 1 ) ) < 0 )
            return rc ;

      return 0 ;
   }



   static int tran4fileVerify( TRAN4FILE *t4, int subs )
   {
      #ifdef S4SERVER
         int rc = tran4fileLowVerify( &(t4->primary), subs ) ;

         // AS Oct 11/01 - Only validate the backup if it is in a valid state.
         if ( rc >= 0 && t4->backup.validState )
            rc = tran4fileLowVerify( &(t4->backup), subs ) ;
         return rc ;
      #else
         return tran4fileLowVerify( t4, subs ) ;
      #endif
   }
#endif  /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( E4ANALYZE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   //CJ 31/10/00 - This function is called from many places in the above code.
   //Some work may have to be done for WINCE
   void tran4getTime( CODE4 *c4 )
   {
      #ifdef S4WIN32
         SYSTEMTIME nowT ;

         GetLocalTime( &nowT ) ;
         #ifdef S4WINCE
            memcpy(c4->transactionTime.year, &(nowT.wYear), 4) ;
         #else
            c4->transactionTime.year = nowT.wYear ;
         #endif
         c4->transactionTime.month = (char)nowT.wMonth ;
         c4->transactionTime.day = (char)nowT.wDay ;
         #ifdef S4WINCE
            long tempSeconds = (((nowT.wHour * 60) + nowT.wMinute) * 60 )+ nowT.wSecond ;
            memcpy(c4->transactionTime.seconds, &tempSeconds, 4) ;
         #else
            c4->transactionTime.seconds = (((nowT.wHour * 60) + nowT.wMinute) * 60 )+ nowT.wSecond ;
         #endif
      #else
         time_t temp ;
         time( &temp ) ;
         struct tm temp2 ;
         temp2 = *localtime( &temp ) ;
         c4->transactionTime.year = temp2.tm_year + 1900 ;
         c4->transactionTime.month = temp2.tm_mon + 1 ;
         c4->transactionTime.day = temp2.tm_mday ;
         c4->transactionTime.seconds = (((temp2.tm_hour * 60) + temp2.tm_min ) * 60 )+ temp2.tm_sec ;
      #endif
   }



   void tran4fileLowInit( TRAN4FILE_LOW *t4, CODE4TRANS *c4trans, TRAN4FILE *trans )
   {
      memset( t4, 0, sizeof( TRAN4FILE_LOW ) ) ;
      t4->c4trans = c4trans ;
      t4->transFile = trans ;
      memset( &t4->file, 0, sizeof( FILE4 ) ) ;
      // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
      //    t4->file.hand = NULL ;
      // #else
         t4->file.hand = INVALID4HANDLE ;
      // #endif
   }



   #ifndef S4SERVER
   static int tran4fileInit( TRAN4FILE *t4, CODE4TRANS *c4trans )
   {
      #ifdef S4SERVER
         tran4fileLowInit( &(t4->primary), c4trans, t4 ) ;
         tran4fileLowInit( &(t4->backup), c4trans, t4 ) ;
      #else
         tran4fileLowInit( t4, c4trans, t4 ) ;
      #endif

      t4->transId = -1 ;

      #ifdef E4ANALYZE
         return tran4fileVerify( t4, 0 ) ;
      #else
         return 0 ;
      #endif
   }
   #endif



   /* LY 99/08/13 : void *transactionData to char *transactionData */
   int S4FUNCTION tran4fileLowAppend( TRAN4FILE_LOW *t4, LOG4HEADER *header, const char *transactionData, int doImmediateFlushing )
   {
      /*
         Appends an entry to the transaction file.
         header - a valid transaction header structure, filled with appropriate information already
         transactionData - the data for the transaction (often a record buffer, file name to open, etc.)
         doImmediateFlushing - if true, the transaction file is flushed after writing the entry but before
                               returning from this function.  Often the data is left bufferred until
                               a rollback/commit or data file disk write occurs.

         The transaction file is locked if required

         The following is written to the transaction file in the given order:
            <length of the 'transactionData' portion of the transaction data> - used to get the position of the header data
                      when reading forward through file (via utilities)
            <'transactionData' transaction data>
            <'header' transaction header>
      */

      // AS July 12/01 Allow the primary log file to be disabled (backup logs still left intact if in use)
      // this just does a very low-level disable.  It prevents the log file from being created and written to
      // or read from, but everything else works as per normal.  In fact, transactions are still supported and
      // only fail if you try to roll them back.
      if ( t4->isDisabled == log4disabled )
         return 0 ;

      CODE4TRANS *c4trans = t4->c4trans ;
      CODE4 *c4 = c4trans->c4 ;
      #ifdef S4WINCE    /* LY 00/03/10 */
         S4LONG tempLong ;
      #endif

      #ifdef E4PARM_LOW
         if ( header == 0 )
            return error4( c4, e4parm_null, E93801 ) ;
      #endif

      int rc ;
      #ifdef E4ANALYZE
         if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
            return rc ;

         if ( t4->file.hand == INVALID4HANDLE )
            return error4( c4, e4struct, E93801 ) ;
      #endif

      /* AS 08/05/98 failure to lock transaction file in some instances, eg.  opening files, etc.
         Put in a catch-all here that we don't write to the file unless we lock it first */

      #if !defined( S4UTILS ) && !defined( S4OFF_MULTI )
         int isLocked ;   /* AS 08/05/98 - see comment below */

         // AS July 12/01 - Don't lock if it is the backup log file
         #ifdef S4SERVER
            if ( t4 == &(t4->transFile->backup) )
               isLocked = 1 ;  // mark as locked to avoid unlock code later
            else
         #endif
         {
            if ( t4->transFile->fileLocks & ( 1UL << ( TRAN4LOCK_MULTIPLE - TRAN4LOCK_BASE ) ) )
               isLocked = 1 ;
            else
            {
               isLocked = 0 ;
               code4tranLockTransactions( c4trans, TRAN4LOCK_MULTIPLE ) ;
            }

            if ( file4longError( tran4fileLowLen( t4 ) ) < 0 )
            {
               if ( isLocked == 0 )
                  code4tranUnlockTransactions( c4trans, TRAN4LOCK_MULTIPLE ) ;
               return (int)(file4longError( tran4fileLowLen( t4 ) ) ) ;
            }
         }
      #endif

      #ifdef S4WINCE
         memcpy( &tempLong, header->dataLen, sizeof(S4LONG) ) ;
         TRAN4ENTRY_LEN len = sizeof( LOG4HEADER ) + tempLong + sizeof( TRAN4ENTRY_LEN ) ;
      #else
         TRAN4ENTRY_LEN len = tran4entryLen( header ) ;
      #endif
      rc = file4writeInternal( &t4->file, tran4fileLowLen( t4 ), &len, sizeof( TRAN4ENTRY_LEN ) ) ;

      if ( rc == 0 )
         tran4fileLowLenAdd( t4, sizeof( len ) ) ;

      #ifdef S4WINCE
         if ( rc == 0 && tempLong != 0 )
      #else
         if ( rc == 0 && header->dataLen != 0 )
      #endif
      {
         #ifdef E4PARM_LOW
            if ( transactionData == 0 )
               rc = error4( c4, e4parm_null, E93801 ) ;
         #endif
         if ( rc == 0 )
            #ifdef S4WINCE
               rc = file4writeInternal( &t4->file, tran4fileLowLen( t4 ), transactionData, (unsigned int)tempLong ) ;
            #else
               rc = file4writeInternal( &t4->file, tran4fileLowLen( t4 ), transactionData, (unsigned int)header->dataLen ) ;
            #endif
         if ( rc == 0 )
            #ifdef S4WINCE
               {
                  memcpy( &tempLong, header->dataLen, sizeof(S4LONG) ) ;
                  tran4fileLowLenAdd( t4, tempLong ) ;
               }
            #else
               tran4fileLowLenAdd( t4, header->dataLen ) ;
            #endif
      }

      if ( rc == 0 )
         rc = file4writeInternal( &t4->file, tran4fileLowLen( t4 ), header, sizeof( LOG4HEADER ) ) ;
      if ( rc == 0 )
         tran4fileLowLenAdd( t4, sizeof( LOG4HEADER ) ) ;

      if ( rc == 0 )
      {
         #ifdef S4OFF_OPTIMIZE
            // AS May 30/03 - enable/disable log flushing - default is enabled. - improves performance (esp. ODBC), maybe cannot recover from crash.
            #ifdef S4SERVER
               if ( c4->logFlush )
            #endif
                  if ( doImmediateFlushing )
                     file4flush( &t4->file ) ;
         #else
            // AS Jun 26/03 - To improve performance, don't actually change the needsFlushing flag to 1 unless we are
            // doing a file update.  This helps in the odbc case where selects are done within a transaction.
            if ( t4->transFile->needsFlushing == 0 )
            {
               switch( header->type )
               {
                  case TRAN4OPEN:
                  case TRAN4OPEN_TEMP:
                  case TRAN4CLOSE:
                  case TRAN4START:
                  case TRAN4COMMIT_PHASE_ONE:  // if there were no writes, we don't actually need to flush here
                  case TRAN4COMMIT_PHASE_TWO:  // if there were no writes, we don't actually need to flush here
                  case TRAN4ROLLBACK:          // if there were no writes, we don't actually need to flush here
                  case TRAN4VOID:
                  case TRAN4CREATE:
                  // AS Jan 17/06 - if the file is encrypted, mark it as such with a new create marker
                  case TRAN4CREATE_ENCRYPT:
                  case TRAN4DIRECTORY:
                  case TRAN4INIT:              // not needed for flush either.
                     // don't do flushing in this case
                     break ;
                  default:
                     t4->transFile->needsFlushing = 1 ;
                     break ;
               }
            }

            if ( doImmediateFlushing )
            {
               #if !defined( S4UTILS ) && !defined( S4OFF_MULTI )
                  if ( isLocked == 0 )
                     code4tranUnlockTransactions( c4trans, TRAN4LOCK_MULTIPLE ) ;
               #endif
               // #if defined( S4SERVER ) && ( defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED ) )
               //    // ensure that only one user is accessing the transaction file via shared memory...
               //    mutex4release( &c4->server->odbcTransMutex ) ;
               // #endif
               // AS 03/08/01 - don't hard flush with utils...
               #ifndef S4UTILS
                  // AS May 30/03 - enable/disable log flushing - default is enabled. - improves performance (esp. ODBC), maybe cannot recover from crash.
                  #ifdef S4SERVER
                     if ( t4->transFile->needsFlushing )
                     {
                        t4->transFile->needsFlushing = 0 ;
                        if ( c4->logFlush )
                           file4flush( &(t4->file) ) ;
                     }
                  #else
                     if ( t4->needsFlushing )
                     {
                        t4->needsFlushing = 0 ;
                        file4flush( &(t4->file) ) ;
                     }
                  #endif
               #endif
            }
         #endif
      }

      #if !defined( S4UTILS ) && !defined( S4OFF_MULTI )
         if ( isLocked == 0 )
            code4tranUnlockTransactions( c4trans, TRAN4LOCK_MULTIPLE ) ;
      #endif
      return rc ;
   }



   int S4FUNCTION tran4fileAppend( TRAN4FILE *t4, LOG4HEADER *header, const char *transactionData, int doImmediateFlushing )
   {
      #ifdef S4SERVER
         int rc = tran4fileLowAppend( &(t4->primary), header, transactionData, doImmediateFlushing ) ;
         int rc2 = 0 ;
         if ( t4->backup.validState )
         {
            rc2 = tran4fileLowAppend( &(t4->backup), header, transactionData, doImmediateFlushing ) ;
            // AS 02/27/01 - if this fails, ist means the backup log file is no longer accessible.
            // in that case, we just uninitialize it and stop using it.
            if ( rc2 < 0 )
            {
               tran4fileLowClose( &(t4->backup) ) ;
               t4->backup.validState = 0 ;
               if ( t4->backup.c4trans->c4 != 0 )
                  error4set( t4->backup.c4trans->c4, 0 ) ;
            }
         }
         return rc ;
      #else
         return tran4fileLowAppend( t4, header, transactionData, doImmediateFlushing ) ;
      #endif
   }



   /* LY 99/08/13 : void *dta to char *dta */
   int S4FUNCTION tran4lowAppend( TRAN4 *t4, const char *dta, int doImmediateFlushing )
   {
      int rc ;

      #ifdef E4ANALYZE
         if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( t4->dataPos != 0 )
      {
         dta = t4->c4trans->c4->tranData ;
         t4->dataPos = 0 ;
      }

      t4->pos = (unsigned long)-1 ;

      // AS Jan 14/03 - CodeBase may be in an error state when this function is called.  In that case we
      // want to continue anyway because we may be inserting a valid transaction event even if an error has
      // occurred (e.g. a failed write due to unique index error)
      CODE4 *c4 = t4->c4trans->c4 ;
      int errCode = error4set( c4, 0 ) ;

      rc = tran4fileAppend( t4->c4trans->transFile, &t4->header, dta, doImmediateFlushing ) ;
      if ( errCode != 0 )
         error4set( c4, errCode ) ;

      memset( &t4->header, 0, sizeof( LOG4HEADER ) ) ;
      return rc ;
   }



   int tran4fileLowClose( TRAN4FILE_LOW *t4 )
   {
      // LY July 31/04
      // #ifdef S4WIN64 /* LY 00/09/20 */
      //   if ( t4->file.hand == NULL )
      // #else
         if ( t4->file.hand == INVALID4HANDLE )
      // #endif
         return 0 ;

      int saveRc = 0 ;

      #ifdef S4SERVER
         if ( t4->c4trans == 0 )  // must already be uninitialized
            return 0 ;

         // AS Aug 15/01 - Relax this constraint if doing temporary transaction only (case where not using log file normally)
         // AS Apr 4/03 - Cannot do this check as it violates a list mutex (another client may be active at this point)
         // assert5( t4->transFile->primary.isDisabled == log4tempUncompressed || t4->transFile->primary.isDisabled == log4tempEnabled || server4clientGetFirst( t4->c4trans->c4->server ) == 0 || (&(t4->transFile->primary) != t4)) ;
      #endif

      int saveErr = error4set( t4->c4trans->c4, 0 ) ;

      int rc = file4close( &t4->file ) ;
      if ( rc < 0 && saveRc == 0 )
         saveRc = rc ;

      error4set( t4->c4trans->c4 , saveErr ) ;

      return saveRc ;
   }



   int S4FUNCTION tran4fileClose( TRAN4FILE *t4 )
   {
      #ifdef S4SERVER
         int rc = tran4fileLowClose( &(t4->primary) ) ;
         int rc2 = tran4fileLowClose( &(t4->backup) ) ;
         if ( rc != 0 )
            return rc ;
         return rc2 ;
      #else
         return tran4fileLowClose( t4 ) ;
      #endif
   }



   int tran4fileLowCreate( TRAN4FILE_LOW *t4, const char *name )
   {
      char buf[258] ;
      int rc ;
      #ifndef S4UTILS
         LOG4HEADER header ;
      #endif
      CODE4 *c4 ;
      #ifndef S4OFF_MULTI
         int oldExcl ;
      #endif

      #ifdef E4ANALYZE
         if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      c4 = t4->c4trans->c4 ;
      assert5( t4->validState != 1 ) ; // error - trans file already open and valid

      #ifdef E4PARM_LOW
         if ( name == 0 )
            return error4( c4, e4parm_null, E93801 ) ;
      #endif

      // AS July 12/01 Allow the primary log file to be disabled (backup logs still left intact if in use)
      // this just does a very low-level disable.  It prevents the log file from being created and written to
      // or read from, but everything else works as per normal.  In fact, transactions are still supported and
      // only fail if you try to roll them back.
      if ( c4->logDisable )
      {
         t4->validState = 1 ;  // still mark it as valid...
         t4->isDisabled = log4disabled ;
      }
      else
      {
         t4->isDisabled = 0 ;
         memcpy( buf, name, strlen( name ) + 1 ) ;
         u4nameExt( buf, sizeof( buf ), "log", 0 ) ;
         #ifndef S4OFF_MULTI
            oldExcl = c4->accessMode ;
            #ifdef S4STAND_ALONE
               // AS Apr 3/03 - For ODBC transactions, allow the file to be opened exclusive.  This is a requirement
               // to allow multiple transactions at one time (we will then cycle through a list of log file names).
               // c4->accessMode = OPEN4DENY_NONE ;
               c4->accessMode = c4->logAccessMode ;
            #else
               c4->accessMode = OPEN4DENY_NONE ;
            #endif
         #endif
         // AS May 24/02 - created file4createInternal for internal use to indicate file types
         rc = file4createInternal( &t4->file, c4, buf, 1, OPT4NONE ) ;
         #ifndef S4OFF_MULTI
            c4->accessMode = oldExcl ;
         #endif
         // AS Feb 7/03 - For ODBC_BUILD, we want to avoid the error output on this...
         if ( c4->odbc == 1 && rc > 0 )
            return rc ;
         if ( rc != 0 )  /* AS 12/02/98 even if r4noCreate, should still error out for trans */
            return error4( c4, e4trans, E93801 ) ;
         t4->validState = 1;
         #ifndef S4UTILS
            memset( &header, 0, sizeof( header ) ) ;
            header.type = TRAN4SHUTDOWN ;
            #ifdef S4WINCE
               *((long *)header.serverDataId) = TRAN4VERSION_NUM ;
            #else
               #ifdef S4SERVER
                  // AS 09/21/00 - the backup log file uses a 'backup' version #:
                  if ( t4 == &(t4->transFile->backup) )
                     header.serverDataId = TRAN4VERSION_NUM_BACKUP ;
                  else
                     header.serverDataId = TRAN4VERSION_NUM ;
               #else
                  header.serverDataId = TRAN4VERSION_NUM ;
               #endif
            #endif
            rc = tran4fileLowAppend( t4, &header, "\0", 1 ) ;
            if ( rc < 0 )
               return( rc ) ;
         #endif
      }

      #ifdef E4ANALYZE
         return tran4fileLowVerify( t4, 1 ) ;
      #else
         return 0 ;
      #endif
   }



   int tran4fileCreate( TRAN4FILE *t4, const char *name )
   {
      #ifdef S4SERVER
         // applies only to primary file...
         int rc = tran4fileLowCreate( &(t4->primary), name ) ;
         return rc ;
      #else
         return tran4fileLowCreate( t4, name ) ;
      #endif
   }



   void tran4fileLowLenSet( TRAN4FILE_LOW *tranFile, FILE4LONG len )
   {
      file4longAssignLong( tranFile->fileLen, len ) ;
   }



   static void tran4fileLowLenUpdate( TRAN4FILE_LOW *t4 )
   {
      tran4fileLowLenSet( t4, file4lenLow( &t4->file ) ) ;
   }



   void tran4fileLenSet( TRAN4FILE *t4, FILE4LONG len )
   {
      #ifdef S4SERVER
         tran4fileLowLenSet( &(t4->primary), len ) ;
         tran4fileLowLenSet( &(t4->backup), len ) ;
      #else
         tran4fileLowLenSet( t4, len ) ;
      #endif
   }



   #ifndef S4UTILS
      static int tran4fileLowStatusFile( TRAN4FILE_LOW *t4 )
      {
         CODE4 *c4 ;
         FILE4LONG filePos, fPos2 ;
         int rc ;
         LOG4HEADER header ;
         TRAN4ENTRY_LEN entryLen;
         // AS Apr 28/03 - made trans-shared a run-time switch
         #if !defined( S4OFF_MULTI )
            long loop ;
            int oldNumAttempts ;
         #endif

         #ifdef E4ANALYZE
            if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
               return rc ;
            if ( t4->file.hand == INVALID4HANDLE )
               return error4( t4->c4trans->c4, e4struct, E93801 ) ;
         #endif

         c4 = t4->c4trans->c4 ;

         // AS Apr 28/03 - made trans-shared a run-time switch
         #if !defined( S4OFF_MULTI )
            if ( c4->transShared == 1 )
            {
               /* first see if the only user, in which case can verify */
               oldNumAttempts = c4->lockAttempts ;
               /* can't do overlay locking due to Novell inconsistencies... */
               /* reserve a byte so only one attempt to set up one at a time */
               c4->lockAttempts = -1 ;
               rc = file4lockInternal( &t4->file, TRAN4LOCK_USERS + TRAN4MAX_USERS + 1, 0, 1, 0 ) ;
               c4->lockAttempts = 1 ;
               for ( loop = 0 ; loop <= TRAN4MAX_USERS ; loop++ )
               {
                  rc = file4lockInternal( &t4->file, TRAN4LOCK_USERS + loop, 0, 1, 0 ) ;
                  if ( rc < 0 || rc == r4locked )
                     break ;
                  file4unlockInternal( &t4->file, TRAN4LOCK_USERS + loop, 0, 1, 0 ) ;
               }
               c4->lockAttempts = oldNumAttempts ;
               if ( rc != 0 )
               {
                  file4unlockInternal( &t4->file, TRAN4LOCK_USERS + TRAN4MAX_USERS + 1, 0, 1, 0 ) ;
                  if ( rc == r4locked )
                     return 0 ;
                  return rc ;
               }
            }
         #endif /* !defined( S4OFF_MULTI ) */

         for( ;; )
         {
            filePos = file4lenLow( &t4->file ) ;
            long fileLen = file4longGetLo( filePos ) ;

            #ifdef S4SERVER
               if ( fileLen == 0 )
               {
                  // if the backup file, the length may be 0...
                  if ( t4 == &(t4->transFile->backup) )
                     return 0 ;
               }
            #endif


            file4longSubtract( &filePos, sizeof( LOG4HEADER ) + sizeof(TRAN4ENTRY_LEN) ) ;

            // if ( file4longGetLo( filePos ) < sizeof( LOG4HEADER ) + sizeof(TRAN4ENTRY_LEN) )
            // the file should at least contain 2 entires 'start and shutdown'
            if ( fileLen < (long)(2 * (sizeof( LOG4HEADER ) + sizeof(TRAN4ENTRY_LEN))) )
            {
               rc = error4describe( c4, e4trans, E83801, t4->file.name, 0, 0 ) ;
               break ;
            }

            /* LY July 07/03 : change from 0 to 0L for Linux compiler */
            file4longAssign( fPos2, 0, 0L ) ;

            rc = file4readAllInternal( &t4->file, fPos2, &entryLen, sizeof( TRAN4ENTRY_LEN ) ) ;
            if ( rc < 0 )
               break ;

            if (entryLen != sizeof(LOG4HEADER) + sizeof(TRAN4ENTRY_LEN))
            {
               rc = error4describe( c4, e4read, E93801, t4->file.name, 0, 0 ) ;
               break ;
            }

            /* LY July 07/03 : change from 0 to 0L for Linux compiler */
            file4longAssign( fPos2, sizeof(TRAN4ENTRY_LEN), 0L ) ;

            rc = file4readAllInternal( &t4->file, fPos2, &header, sizeof( LOG4HEADER ) ) ;
            if ( rc < 0 )
               break ;

            if ( header.type == TRAN4BACKEDUP )
            {
               rc = error4describe( c4, e4trans, E83815, t4->file.name, 0, 0 ) ;
               break ;
            }

            rc = file4readAllInternal( &t4->file, filePos, &entryLen, sizeof( TRAN4ENTRY_LEN ) ) ;
            if ( rc < 0 )
               break ;

            if (entryLen != sizeof(LOG4HEADER) + sizeof(TRAN4ENTRY_LEN))
            {
               rc = error4describe( c4, e4trans, E83801, t4->file.name, 0, 0 ) ;
               break ;
            }

            file4longAdd( &filePos, sizeof(TRAN4ENTRY_LEN) ) ;

            rc = file4readAllInternal( &t4->file, filePos, &header, sizeof( LOG4HEADER ) ) ;
            if ( rc < 0 )
               break ;

            break ;
         }

         // AS Apr 28/03 - made trans-shared a run-time switch
         if ( c4->transShared == 1 )
            file4unlockInternal( &t4->file, TRAN4LOCK_USERS + TRAN4MAX_USERS + 1, 0, 1, 0 ) ;

         if ( rc < 0 )
            return rc ;

         if ( header.type == TRAN4SHUTDOWN )
            return 0 ;

         #ifdef S4SERVER
            // AS 09/19/00 - if this is the backup file for auto-recovery, TRAN4CLOSE_BACKUP is also acceptable
            if ( header.type == TRAN4CLOSE_BACKUP && ( t4 == &(t4->transFile->backup) ) )
               return 0 ;
         #endif

         // AS 01/02/00 - let's display the path name.  This is important especially with
         // auto recovery where the backup file may be causing the problem.
         return error4describe( c4, e4trans, E83801, t4->file.name, 0, 0 ) ;
      }
   #endif /* S4UTILS */



   int tran4fileLowOpen( TRAN4FILE_LOW *t4, const char *name )
   {
      int rc, oldOpenError, oldReadOnly ;
      char buf[258] ;
      CODE4 *c4 ;
      #ifndef S4OFF_MULTI
         int oldExcl ;
      #endif

      #ifdef E4PARM_LOW
         if ( name == 0 )
            return error4( t4->c4trans->c4, e4parm_null, E93801 ) ;
      #endif

      c4 = t4->c4trans->c4 ;

      #ifdef E4ANALYZE
         if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
            return rc ;
         if ( t4->file.hand != INVALID4HANDLE )   /* already open */
            return error4( c4, e4struct, E93801 ) ;
      #endif

      // AS July 12/01 Allow the primary log file to be disabled (backup logs still left intact if in use)
      // this just does a very low-level disable.  It prevents the log file from being created and written to
      // or read from, but everything else works as per normal.  In fact, transactions are still supported and
      // only fail if you try to roll them back.
      if ( c4->logDisable == log4disabled )
      {
         t4->validState = 1 ;  // still mark it as valid...
         t4->isDisabled = log4disabled ;
      }
      else
      {
         t4->isDisabled = 0 ;
         memcpy( buf, name, strlen( name ) + 1 ) ;
         u4nameExt( buf, sizeof( buf ), "log", 0 ) ;

         #ifndef S4OFF_MULTI
            oldExcl = c4->accessMode ;
            #ifdef S4UTILS
               c4->accessMode = OPEN4DENY_RW ;
            #else
               #ifdef S4STAND_ALONE
                  // AS Feb 5/03 - For ODBC transactions, allow the file to be opened exclusive.  This is a requirement
                  // to allow multiple transactions at one time (we will then cycle through a list of log file names).
                  // c4->accessMode = OPEN4DENY_NONE ;
                  c4->accessMode = c4->logAccessMode ;
               #else
                  // if odbc / odbc enabled, open shared
                  #if (defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )) && !defined( S4OFF_ODBC_TRANS )
                     // AS Apr 28/03 - made trans-shared a run-time switch
                     if ( c4->transShared == 1 )
                        c4->accessMode = OPEN4DENY_NONE ;
                     else
                  #endif
                        c4->accessMode = OPEN4DENY_RW ;
               #endif
            #endif
         #endif
         oldReadOnly = c4getReadOnly( c4 ) ;
         c4setReadOnly( c4, 0 ) ;
         oldOpenError = c4->errOpen ;
         c4->errOpen = 0 ;

         // AS May 24/02 - created file4openLow for internal use to indicate file types
         rc = file4openInternal( &t4->file, c4, buf, 1, OPT4NONE ) ;

         c4->errOpen = oldOpenError ;
         c4setReadOnly( c4, oldReadOnly ) ;
         #ifndef S4OFF_MULTI
            c4->accessMode = oldExcl ;
         #endif
         if ( (rc != r4noOpen && rc != r4noExist) && rc >= 0 )
         {
            #ifdef S4UTILS
               // AS 03/08/01 - in the utils, we do want to optimize the reading and writing
               // of the transaction files to speed up the operations - they are open exclusively,
               // and if anything fails we don't destroy the original files, so it is safe
               // to do this.
               #ifndef S4OFF_OPTIMIZE
                  // sizoef( LOG4HEADER ) used as a placeholder, but in fact with OPT4OTHER this paramater is unused
                  file4optimizeLow( &t4->file, c4->optimize, OPT4OTHER, sizeof( LOG4HEADER ), 0 ) ;
               #endif
            #else
               // AS Feb 12/03 - Allow propogation of rc back to caller
               int rc2 = tran4fileLowStatusFile( t4 ) ;
               if ( rc2 != 0 )
               {
                  t4->validState = 0 ;
                  file4close( &t4->file ) ;
                  return rc2 ;
               }
               else
                  t4->validState = 1 ;
            #endif /* S4UTILS */
         }
      }

      if ( rc != 0 )
         return rc ;

      #ifdef E4ANALYZE
         return tran4fileLowVerify( t4, 1 ) ;
      #else
         return 0 ;
      #endif
   }



   static int tran4fileOpen( TRAN4FILE *t4, const char *name )
   {
      #ifdef S4SERVER
         // AS 09/18/00 - only opens the primary file...
         int rc = 0 ;
         rc = tran4fileLowOpen( &(t4->primary), name ) ;
         return rc ;
      #else
         return tran4fileLowOpen( t4, name ) ;
      #endif
   }



   static long tran4fileGetNextTransId( TRAN4FILE *t4, TRAN4 *trans )
   {
      #ifdef S4WINCE
         S4LONG tempLong ;
      #endif
      int rc = 0 ;
      #ifdef E4ANALYZE
         if ( ( rc = tran4fileVerify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      CODE4 *c4 = trans->c4trans->c4 ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      if ( c4->transShared == 0 )
      {
         /* with a server with exclusive access to the transaction file, we just use
            an internal counter.  If the file is not open yet (transId == 0), then
            just get the last entry in the transaction file with a valid transaction
            id #, and offset from there.
         */
         if ( t4->transId > 0 )
         {
            if ( t4->transId == LONG_MAX )
               t4->transId = 0 ;
            ++t4->transId ;
         }
         else
         {
            #ifdef S4SERVER
               // AS July 22/01 - If the primary is disabled, get the trans id from the backup...
               if ( t4->primary.isDisabled != log4enabled )
               {
                  if ( t4->backup.validState == 1 )
                     t4->useBackup = 1 ;
               }
            #endif
            rc = tran4bottom( trans ) ;
            if ( rc < 0 )  /* no entries */
            {
               #ifdef S4SERVER
                  t4->useBackup = 0 ;
               #endif
               // AS Oct 19/01 - Ensure that negative rc returned if occurs.
               return rc ;
            }

            #ifdef S4WINCE  // LY Aug 19/04
               memcpy( (char*)&tempLong, &trans->header.transId, sizeof(S4LONG) ) ;
               while ( tempLong == 0 )
            #else
               while ( trans->header.transId == 0 )
            #endif
            {
               if ( tran4skip( trans, TRAN4BACKWARDS ) != 0 )
               {
                  break ;
               }
               #ifdef S4WINCE
                  memcpy( (char*)&tempLong, &trans->header.transId, sizeof(S4LONG) ) ;
               #endif
            }

            #ifdef S4SERVER
               t4->useBackup = 0 ;
            #endif

            #ifdef S4WINCE  // LY Aug 19/04
               memcpy( (char*)&tempLong, &trans->header.transId, sizeof(S4LONG) ) ;
               if ( tempLong < 0 )
            #else
               if ( trans->header.transId < 0 )
            #endif
               return e4trans ;

            #ifdef S4WINCE
               memcpy( &tempLong, trans->header.transId, sizeof(S4LONG) ) ;
               if ( tempLong == LONG_MAX )
            #else
               if ( trans->header.transId == LONG_MAX )
            #endif
               t4->transId = 0 ;
            else
               #ifdef S4WINCE
                  t4->transId = tempLong + 1 ;
               #else
                  t4->transId = trans->header.transId + 1 ;
               #endif
         }
      }
      else
      {
         /*
            if we are sharing the transaction file, it is a major slowdown to examine
            the log file every time to get the next transaction id #.  Therefore, what
            we do instead, is to use the 'user id #' which is generated from the lock
            offset into the transaction file (which gives this user a unique id).
            What happens, is since there is a max # of sharers of the transaction file,
            if every user starts with their user id # as a base and adds TRAN4MAX_USERS
            every time to get a new transaction id, we guarantee that every transaction
            has a unique id and it is calculated quickly.  If there is no current
            id (i.e. first transaction for this session), then we need to scan through
            the transaction file for the latest transaction id corresponding to our
            current 'userId'.  That id is used as the offset from which to calculate
            the next trans id.  Note that we cannot simply use the latest transaction
            id because with this method of adding TRAN4MAX_USERS every time, the
            newest transaction id may not be the largest value in the file since a
            different user which closed down earlier may have generated more transactions
            and hence a large maximum id in the file.
         */
         if ( t4->transId > 0 )
         {
            if ( t4->transId == LONG_MAX )
            {
               // AS 11/22/99 user id # moved to TRAN4FILE structure
               // AS Mar 20/03 - needed to update coding here
               #ifdef S4SERVER
                  t4->transId = t4->primary.userIdNo ;
               #else
                  t4->transId = t4->userIdNo ;
               #endif
            }
            t4->transId += TRAN4MAX_USERS ;
         }
         else
         {
            if ( tran4bottom( trans ) < 0 )  /* no entries */
               return 0 ;

            for ( ;; )
            {
               // AS 11/22/99 user id # moved to TRAN4FILE structure
               #ifdef S4WINCE
                  memcpy( (char*)&tempLong, &trans->header.transId, sizeof(S4LONG) ) ;
                  if ( tempLong % TRAN4MAX_USERS == t4->userIdNo )
               #else
                  #ifdef S4SERVER
                     if ( trans->header.transId % TRAN4MAX_USERS == t4->primary.userIdNo )
                  #else
                     if ( trans->header.transId % TRAN4MAX_USERS == t4->userIdNo )
                  #endif
               #endif
                  break ;
               if ( tran4skip( trans, TRAN4BACKWARDS ) != 0 )
                  break ;
            }

            #ifdef S4WINCE
               memcpy( (char*)&tempLong, trans->header.transId, sizeof(S4LONG) ) ;
               if ( tempLong < 0 )
            #else
               if ( trans->header.transId < 0 )
            #endif
               return e4trans ;

            // AS 11/22/99 user id # moved to TRAN4FILE structure
            #ifdef S464BIT
               #ifdef S4SERVER
                  if ( trans->header.transId == INT_MAX || ( trans->header.transId % TRAN4MAX_USERS != t4->primary.userIdNo ) )
               #else
                  if ( trans->header.transId == INT_MAX || ( trans->header.transId % TRAN4MAX_USERS != t4->userIdNo ) )
               #endif
            #else
               #ifdef S4WINCE
                  #ifdef S4SERVER
                     if ( tempLong == LONG_MAX || ( tempLong % TRAN4MAX_USERS != t4->primary.userIdNo ) )
                  #else
                     if ( tempLong == LONG_MAX || ( tempLong % TRAN4MAX_USERS != t4->userIdNo ) )
                  #endif
               #else
                  // AS Mar 20/03 - needed to update coding here
                  #ifdef S4SERVER
                     if ( trans->header.transId == LONG_MAX || ( trans->header.transId % TRAN4MAX_USERS != t4->primary.userIdNo ) )
                  #else
                     if ( trans->header.transId == LONG_MAX || ( trans->header.transId % TRAN4MAX_USERS != t4->userIdNo ) )
                  #endif
               #endif
            #endif
               #ifdef S4SERVER
                  t4->transId = t4->primary.userIdNo ;
               #else
                  t4->transId = t4->userIdNo ;
               #endif
            else
               #ifdef S4WINCE
                  t4->transId = tempLong + TRAN4MAX_USERS ;
               #else
                  t4->transId = trans->header.transId + TRAN4MAX_USERS ;
               #endif
         }
      }
      return t4->transId ;
   }



   static long tran4getTransId( TRAN4 *t4 )
   {
      #ifdef E4ANALYZE
         int rc ;

         if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( t4->transId > 0 )
         return t4->transId ;

      #ifdef E4ANALYZE
         if ( t4->c4trans->transFile == 0 )
            return error4( t4->c4trans->c4, e4struct, E93801 ) ;
      #endif

      t4->transId = tran4fileGetNextTransId( t4->c4trans->transFile, t4 ) ;
      return t4->transId ;
   }



   static int tran4fileLowRead( TRAN4FILE_LOW *t4, unsigned long posIn, LOG4HEADER *header, char **data, unsigned int *dataLen )
   {
      int rc ;
      FILE4LONG pos ;
      #ifdef S4WINCE    /* LY 00/03/10 */
         unsigned S4LONG tempLong ;
      #endif

      /* LY July 07/03 : change from 0 to 0L for Linux compiler */
      file4longAssign( pos, posIn, 0L ) ;

      #ifdef E4ANALYZE
         if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
            return rc ;
         if ( t4->file.hand == INVALID4HANDLE )
            return error4( t4->c4trans->c4, e4struct, E93801 ) ;
      #endif

      rc = file4readAllInternal( &t4->file, pos, header, sizeof( LOG4HEADER ) ) ;
      if ( rc < 0 )
         return rc ;
      #ifdef S4WINCE
         memcpy( &tempLong, header->dataLen, sizeof(S4LONG) ) ;
         if ( *dataLen < tempLong + 1 )
      #else
         if ( *dataLen < header->dataLen + 1 )
      #endif
         if ( u4allocAgain( t4->c4trans->c4, data, dataLen, (unsigned int)header->dataLen + 1 ) != 0 )
            return e4memory ;
      if ( header->dataLen > 0L )
      {
         #ifdef S4WINCE
            memcpy( &tempLong, header->dataLen, sizeof(S4LONG) ) ;
            file4longSubtract( &pos, tempLong ) ;
            rc = file4readAllInternal( &t4->file, pos, *data, (unsigned int)tempLong ) ;
         #else
            file4longSubtract( &pos, header->dataLen ) ;
            rc = file4readAllInternal( &t4->file, pos, *data, (unsigned int)header->dataLen ) ;
         #endif
         if ( rc < 0 )
            return rc ;
      }
      else
         (*data)[0] = '\0' ;
      return 0 ;
   }



   static int tran4fileRead( TRAN4FILE *t4, unsigned long posIn, LOG4HEADER *header, char **data, unsigned int *dataLen )
   {
      #ifdef S4SERVER
         if ( t4->useBackup == 1 )   // if the primary is disabled in some cases we still need to do a lookup, so do that here
            return tran4fileLowRead( &(t4->backup), posIn, header, data, dataLen ) ;
         else
            return tran4fileLowRead( &(t4->primary), posIn, header, data, dataLen ) ;
      #else
         return tran4fileLowRead( t4, posIn, header, data, dataLen ) ;
      #endif
   }



   int S4FUNCTION tran4read( TRAN4 *t4 )
   {
      #ifdef E4ANALYZE
         int rc ;

         if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
            return rc ;
         if ( t4->pos == (unsigned long)-1 )
            return error4( t4->c4trans->c4, e4info, E83802 ) ;
         if ( t4->c4trans->transFile == 0 )
            return error4( t4->c4trans->c4, e4info, E93801 ) ;
      #endif
      return tran4fileRead( t4->c4trans->transFile, t4->pos, &t4->header, &t4->c4trans->c4->tranData, &t4->c4trans->c4->tranDataLen ) ;
   }



   int S4FUNCTION tran4fileLowTop( TRAN4FILE_LOW *t4, TRAN4 *trans )
   {
      int rc ;
      unsigned lenRead ;
      TRAN4ENTRY_LEN len ;
      FILE4LONG pos ;

      #ifdef E4ANALYZE
         if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      /* LY July 07/03 : change from 0 to 0L for Linux compiler */
      file4longAssign( pos, 0, 0L ) ;

      lenRead = file4readInternal( &t4->file, pos, &len, sizeof( len ) ) ;
      if ( lenRead != sizeof( len ) )
         return r4bof ;

      trans->pos = len - sizeof( LOG4HEADER ) ;

      rc = tran4read( trans ) ;
      if ( rc < 0 )
      {
         trans->pos = (unsigned long)-1 ;
         return rc ;
      }
      return 0 ;
   }



   int S4FUNCTION tran4fileTop( TRAN4FILE *t4, TRAN4 *trans )
   {
      #ifdef S4SERVER
         // just read from the primary transaction file...
         return tran4fileLowTop( &(t4->primary), trans ) ;
      #else
         return tran4fileLowTop( t4, trans ) ;
      #endif
   }



   int S4FUNCTION tran4fileLowBottom( TRAN4FILE_LOW *t4, TRAN4 *trans )
   {
      int rc ;
      FILE4LONG pos ;

      #ifdef E4ANALYZE
         if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      pos = file4lenLow( &t4->file ) ;

      #ifdef E4ANALYZE
         if ( file4longGetHi( pos ) > 0 )   /* < 0, error is ok */
            return error4( 0, e4result, E93801 ) ;
      #endif

      trans->pos = file4longGetLo( pos ) - sizeof( LOG4HEADER ) ;
      if ( trans->pos == (unsigned long)-1 )
         return r4eof ;
      rc = tran4read( trans ) ;
      if ( rc < 0 )
      {
         trans->pos = (unsigned long)-1 ;
         return rc ;
      }
      return 0 ;
   }



   int S4FUNCTION tran4fileBottom( TRAN4FILE *t4, TRAN4 *trans )
   {
      #ifdef S4SERVER
         if ( t4->useBackup == 1 )   // if the primary is disabled in some cases we still need to do a lookup, so do that here
            return tran4fileLowBottom( &(t4->backup), trans ) ;
         else
            return tran4fileLowBottom( &(t4->primary), trans ) ;
      #else
         return tran4fileLowBottom( t4, trans ) ;
      #endif
   }



   int S4FUNCTION tran4fileLowSkip( TRAN4FILE_LOW *t4, TRAN4 *trans, const int direction )
   {
      int rc ;
      TRAN4ENTRY_LEN len ;
      FILE4LONG tempLen ;
      #ifdef S4WINCE    /* LY 00/03/10 */
         S4LONG tempLong ;
      #endif

      #ifdef E4PARM_LOW
         if ( t4 == 0 || trans == 0 || ( direction != TRAN4FORWARDS && direction != TRAN4BACKWARDS ) )
            return error4( 0, e4parm, E93801 ) ;
      #endif

      if ( trans->pos == (unsigned long)-1 )   /* empty file ... */
      {
         if ( direction == TRAN4BACKWARDS )
            return r4eof ;
         else
            return r4bof ;
      }

      #ifdef E4ANALYZE
         if ( ( rc = tran4fileLowVerify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      if ( direction == TRAN4BACKWARDS )
      {
         #ifdef S4WINCE
            memcpy( &tempLong, trans->header.dataLen, sizeof(S4LONG) ) ;
            if ( trans->pos < sizeof( LOG4HEADER ) + tempLong + sizeof( TRAN4ENTRY_LEN ) )
               return r4bof ;
            trans->pos -= sizeof( LOG4HEADER ) + tempLong + sizeof( TRAN4ENTRY_LEN ) ;
         #else
            if ( trans->pos < tran4entryLen( &trans->header ) )
               return r4bof ;
            trans->pos -= tran4entryLen( &trans->header ) ;
         #endif
         rc = tran4read( trans ) ;
         if ( rc< 0 )
         {
            trans->pos = (unsigned long)-1 ;
            return rc ;
         }
      }
      else
      {
         trans->pos += sizeof( LOG4HEADER ) ;
         tempLen = file4lenLow( &t4->file ) ;
         if ( file4longLessEq( tempLen, trans->pos + sizeof( TRAN4ENTRY_LEN ) ) )
            return r4eof ;
         /* LY July 07/03 : change from 0 to 0L for Linux compiler */
         file4longAssign( tempLen, trans->pos, 0L ) ;
         rc = file4readAllInternal( &t4->file, tempLen, &len, sizeof( TRAN4ENTRY_LEN ) ) ;
         if ( rc < 0 )
            return rc ;

         // AS 03/19/01 - len should always be at least as large as LOG4HEADER or the file is corrupt
         if ( len < sizeof( LOG4HEADER ) )
            return error4( 0, e4trans, E83809 ) ;

         trans->pos += len - sizeof( LOG4HEADER ) ;

         if ( trans->pos >= file4longGetLo( file4lenLow( &t4->file ) ) )
            return r4eof ;

         rc = tran4read( trans ) ;
         if ( rc< 0 )
         {
            trans->pos = (unsigned long)-1 ;
            return rc ;
         }
      }

      return 0 ;
   }



   int S4FUNCTION tran4fileSkip( TRAN4FILE *t4, TRAN4 *trans, const int direction )
   {
      #ifdef S4SERVER
         // just read from the primary transaction file...
         if ( t4->useBackup == 1 )   // if the primary is disabled in some cases we still need to do a lookup, so do that here
            return tran4fileLowSkip( &(t4->backup), trans, direction ) ;
         else
            return tran4fileLowSkip( &(t4->primary), trans, direction ) ;
      #else
         return tran4fileLowSkip( t4, trans, direction ) ;
      #endif
   }



   #ifndef S4INLINE
      long S4FUNCTION tran4id( TRAN4 *t4 )
      {
         #ifdef E4ANALYZE
            int rc ;

            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
         #endif

         return t4->header.transId ;
      }



      long S4FUNCTION tran4clientId( TRAN4 *t4 )
      {
         int rc ;

         #ifdef E4ANALYZE
            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
         #endif

         return t4->header.clientId ;
      }



      unsigned long S4FUNCTION tran4clientDataId( TRAN4 *t4 )
      {
         int rc ;

         #ifdef E4ANALYZE
            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
         #endif

         return t4->header.clientDataId ;
      }



      unsigned long S4FUNCTION tran4serverDataId( TRAN4 *t4 )
      {
         int rc ;

         #ifdef E4ANALYZE
            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
         #endif

         return t4->header.serverDataId ;
      }



      int S4FUNCTION tran4type( TRAN4 *t4 )
      {
         int rc ;

         #ifdef E4ANALYZE
            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
         #endif

         return t4->header.type ;
      }



      unsigned S4FUNCTION tran4len( TRAN4 *t4 )
      {
         int rc ;

         #ifdef E4ANALYZE
            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
         #endif

         return t4->header.dataLen ;
      }
   #endif  /* S4INLINE */



   /* data may be larger than can be contained in memory... new coding reqd */
   void *S4FUNCTION tran4getData( TRAN4 *t4, const long pos )
   {
      CODE4 *c4 ;
      #ifdef S4WINCE
         unsigned S4LONG tempLong ;
      #endif

      #ifdef E4ANALYZE
         if ( tran4verify( t4, 1 ) < 0 )
            return 0 ;
         if ( t4->c4trans->transFile == 0 )
         {
            error4( t4->c4trans->c4, e4struct, E93801 ) ;
            return 0 ;
         }
      #endif

      c4 = t4->c4trans->c4 ;

      if ( c4->tranData != 0 )
      {
         #ifdef S4WINCE
            memcpy( &tempLong, t4->header.dataLen, sizeof(S4LONG) ) ;
            if ( c4->tranDataLen < tempLong + 1 )
         #else
            if ( c4->tranDataLen < t4->header.dataLen + 1 )
         #endif
         {
            error4( c4, e4trans, E93801 ) ;
            return 0 ;
         }
         #ifdef S4WINCE
            c4->tranData[ tempLong ] = 0 ;
         #else
            c4->tranData[ t4->header.dataLen ] = 0 ;
         #endif
      }
      return c4->tranData + pos ;
   }



   int S4FUNCTION tran4set( TRAN4 *t4, const int status, const long id1, const long id2, const int tranCode,
                            const unsigned int dLen, const long clientId, const long serverId )
   {
      DATA4 *data ;
      #ifdef S4WINCE
         S4LONG tempLong ;
      #endif
      #ifdef E4ANALYZE
         int rc ;

         if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
            return rc ;
      #endif

      CODE4 *c4 = t4->c4trans->c4 ;

      if ( status == r4active || status == r4partial || tranCode == TRAN4START || tranCode == TRAN4ROLLBACK )
      {
         #ifdef S4WINCE
            if ( id1 == -1 )
            {
               tempLong = tran4getTransId( t4 ) ;
               memcpy( t4->header.transId, &tempLong, sizeof(S4LONG) ) ;
            }
            else
               memcpy( t4->header.transId, &id1, sizeof(S4LONG) ) ;
         #else
            if ( id1 == -1 )
               t4->header.transId = tran4getTransId( t4 ) ;
            else
               t4->header.transId = id1 ;
         #endif

         if ( t4->header.transId < 0 )
            return error4( c4, e4trans, E93801 ) ;
      }
      else
      {
         #ifdef S4WINCE
            memset( &t4->header.transId, 0, sizeof(S4LONG) ) ;
         #else
            t4->header.transId = 0 ;
         #endif
      }

      #ifdef E4ANALYZE
         if ( t4->c4trans->transFile == 0 )
            return error4( c4, e4struct, E93801 ) ;
      #endif

      // AS Apr 28/03 - made trans-shared a run-time switch
      #if !defined( S4UTILS ) && !defined ( S4SERVER )
         if ( c4->transShared == 1 )
         {
            // AS Apr 21/03 - For server, even if shared we need to use the id2 input (multiple clients)
            // for server shared log file, use read in userIdNo as well...
            #ifdef S4WINCE
               memcpy( t4->header.clientId, &t4->c4trans->transFile->userIdNo, sizeof(S4LONG) ) ;
            #else
               #ifdef S4SERVER
                  t4->header.clientId = t4->c4trans->transFile->primary.userIdNo ;
               #else
                  t4->header.clientId = t4->c4trans->transFile->userIdNo ;
               #endif
            #endif
         }
         else
            #ifdef S4WINCE
               memcpy( t4->header.clientId, &id2, sizeof(S4LONG) ) ;
            #else
               t4->header.clientId = id2 ;
            #endif
      #else
         t4->header.clientId = id2 ;
      #endif

      t4->header.type = tranCode ;

      #ifdef S4WINCE
         memcpy( t4->header.dataLen, &dLen, sizeof(S4LONG) ) ;
         memcpy( t4->header.clientDataId, &clientId, sizeof(S4LONG) ) ;
         memcpy( t4->header.serverDataId, &serverId, sizeof(S4LONG) ) ;
      #else
         t4->header.dataLen = dLen ;
         t4->header.clientDataId = clientId ;
         t4->header.serverDataId = serverId ;
      #endif

      memcpy( (void*)&t4->header.time, (void*)&c4->transactionTime, sizeof(LOG4TIME) ) ;

      if ( serverId != 0 && clientId != 0 )  /* ensure a valid data4 before bothering to call (i.e. maybe a non-data-related transaction item) */
      {
         data = tran4dataFull( t4, serverId, clientId ) ;
         if ( data != 0 && data->transChanged == 0 )   /* probably not a data-related operation (eg. tranAddUser, etc) */
         {
            // AS Jun 26/03 - depending on the TRANCODE, not necessary transChanged
            switch( tranCode )
            {
               case TRAN4OPEN:
               case TRAN4OPEN_TEMP:
               case TRAN4CLOSE:
               case TRAN4CREATE:
               // AS Jan 17/06 - if the file is encrypted, mark it as such with a new create marker
               case TRAN4CREATE_ENCRYPT:
               case TRAN4VOID:
                  break ;
               default:
                  data->transChanged = 1 ;
                  break ;
            }
         }
      }

      return 0 ;
   }



   int S4FUNCTION tran4putData( TRAN4 *t4, const void *dta, unsigned dLen )
   {
      CODE4 *c4 ;

      #ifdef E4ANALYZE
         int rc ;

         if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
            return rc ;
         if ( t4->c4trans->transFile == 0 || dLen <= 0 )
            return error4( t4->c4trans->c4, e4info, E93801 ) ;
      #endif

      c4 = t4->c4trans->c4 ;

      if ( t4->dataPos + dLen + 1 > c4->tranDataLen )
         if ( u4allocAgain( c4, &c4->tranData, &c4->tranDataLen, dLen + t4->dataPos + 1 ) != 0 )
            return e4memory ;

      memcpy( c4->tranData + t4->dataPos, dta, dLen ) ;
      t4->dataPos += dLen ;
      return 0 ;
   }



   #ifndef S4INLINE
      int S4FUNCTION tran4bottom( TRAN4 *t4 )
      {
         #ifdef E4ANALYZE
            int rc ;

            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
            if ( t4->c4trans->transFile == 0 )
               return error4( t4->c4trans->c4, e4struct, E93801 ) ;
         #endif
         return tran4fileBottom( t4->c4trans->transFile, t4 ) ;
      }



      int S4FUNCTION tran4top( TRAN4 *t4 )
      {
         #ifdef E4ANALYZE
            int rc ;

            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
            if ( t4->c4trans->transFile == 0 )
               return error4( t4->c4trans->c4, e4info, E93801 ) ;
         #endif
         return tran4fileTop( t4->c4trans->transFile, t4 ) ;
      }



      int S4FUNCTION tran4skip( TRAN4 *t4, int direction )
      {
         #ifdef E4ANALYZE
            int rc ;
         #endif

         #ifdef E4PARM_LOW
            if ( direction != TRAN4FORWARDS && direction != TRAN4BACKWARDS )
               return error4( 0, e4parm, E93801 ) ;
         #endif

         #ifdef E4ANALYZE
            if ( ( rc = tran4verify( t4, 1 ) ) < 0 )
               return rc ;
            if ( t4->c4trans->transFile == 0 )
               return error4( t4->c4trans->c4, e4struct, E93801 ) ;
         #endif
         return tran4fileSkip( t4->c4trans->transFile, t4, direction ) ;
      }
   #endif  /* S4INLINE */



   int tran4lowUnappend( TRAN4 *trans )
   {
      DATA4 *data ;
      int rc ;
      S4LONG recNo ;
      #ifdef S4WINCE
         S4LONG tempServerDataId, tempClientDataId ;
      #endif

      #ifdef E4PARM_LOW
         if ( trans == 0 )
            return error4( 0, e4parm_null, E93801 ) ;
      #endif

      #ifdef S4DATA_ALIGN
         memcpy(&recNo, tran4getData ( trans, 0L ), sizeof(S4LONG *) ) ;
      #else
         recNo = *((S4LONG *)(tran4getData( trans, 0L ) ) ) ;
      #endif

      #ifdef S4WINCE
         memcpy( &tempServerDataId, trans->header.serverDataId, sizeof(S4LONG) ) ;
         memcpy( &tempClientDataId, trans->header.clientDataId, sizeof(S4LONG) ) ;
         data = tran4dataFull( trans, tempServerDataId, tempClientDataId ) ;
      #else
         data = tran4dataFull( trans, tran4serverDataId( trans ), tran4clientDataId( trans ) ) ;
      #endif
      if ( data == 0 )
      {
         // AS Feb 14/03 - Have added support for complete closing of temporary files
         // now, so there is a chance the table will not be found if it was temporary,
         // in which case just return success.  There is a risk here that should be addressed.
         // return error4( trans->c4trans->c4, e4name, E93801 ) ;
         return 0 ;
      }

      #ifndef S4OFF_MULTI
         if ( d4lockTestAppend( data ) != 1 )
         {
            // AS Mar 4/10 - log more info about what is going on in this case
            CODE4 *c4 = trans->c4trans->c4 ;
            char moreInfo[80] ;
            #ifdef S4SERVER
               if ( data->dataFile->appendServerLock = 0 )
                  strncpy( moreInfo, "there are no locks on the append bytes", 50) ;
               else
                  strncpy( moreInfo, "there are locks on the append bytes for a different client", 60 ) ;
            #else
               strncpy( moreInfo, "stand alone version", 30 ) ;
            #endif
            rc = error4describe( c4, e4lock, E83804, "append bytes were not locked for data file:", d4alias( data ), moreInfo ) ;
            int oldSafety = c4->safety ;
            c4->safety = 0 ;
            int svCode = error4set( c4, 0 ) ;
            FILE4 file ;
            FILE4LONG pos ;
            file4longAssign( pos, 0, 0L ) ;
            if ( file4createInternal( &file, c4, "OLD_RECORD.FIL", 0, OPT4NONE ) != 0 )
            {
               error4describe( c4, e4lock, E83804, "Unable to create old record file", 0, 0 ) ;
            }
            else
            {
               file4writeInternal( &file, pos, data->record, dfile4recWidth( data->dataFile ) ) ;
               file4close( &file ) ;
            }
            if ( file4createInternal( &file, c4, "NEW_RECORD.FIL", 0, OPT4NONE ) != 0 )
            {
               error4describe( c4, e4lock, E83804, "Unable to create new record file", 0, 0 ) ;
            }
            else
            {
               file4writeInternal( &file, pos, tran4getData( trans, sizeof(S4LONG ) ), dfile4recWidth( data->dataFile ) ) ;
               file4close( &file ) ;
            }

            error4set( c4, svCode ) ;
            c4->safety = oldSafety ;
            return rc ;
         }
      #endif
      if ( d4recCount( data ) != recNo )
      {
         if ( d4recCount( data ) == recNo - 1 )   /* already unappended */
            return 0 ;
         else
            return error4( trans->c4trans->c4, e4rollback, E83805 ) ;
      }

      memcpy( data->record, tran4getData( trans, sizeof(S4LONG ) ), dfile4recWidth( data->dataFile ) ) ;
      rc = d4unappend( data ) ;
      if ( rc < 0 )
         return error4stack( trans->c4trans->c4, rc, E93801 ) ;

      return d4update( data ) ;
   }



   int tran4lowUnwrite( TRAN4 *trans )
   {
      DATA4 *data ;
      int rc, doSpecial ;
      S4LONG recNo ;
      char *rec, *saveRec = 0;
      CODE4 *c4 ;
      #ifndef S4MEMO_OFF
         int i ;
         unsigned S4LONG ptrLen, pos ;
      #endif
      #ifdef S4WINCE
         S4LONG tempServerDataId, tempClientDataId ;
      #endif

      #ifdef E4PARM_LOW
         if ( trans == 0 )
            return error4( 0, e4parm_null, E93801 ) ;
      #endif

      c4 = trans->c4trans->c4 ;
      #ifdef S4DATA_ALIGN
         memcpy(&recNo, tran4getData ( trans, 0L ), sizeof(S4LONG *) ) ;
      #else
         recNo = *((S4LONG *)(tran4getData( trans, 0L ) ) ) ;
      #endif
      rec = (char *)tran4getData( trans, (long)sizeof(S4LONG ) ) ;

      #ifdef S4WINCE
         memcpy( &tempServerDataId, trans->header.serverDataId, sizeof(S4LONG) ) ;
         memcpy( &tempClientDataId, trans->header.clientDataId, sizeof(S4LONG) ) ;
         data = tran4dataFull( trans, tempServerDataId, tempClientDataId ) ;
      #else
         data = tran4dataFull( trans, tran4serverDataId( trans ), tran4clientDataId( trans ) ) ;
      #endif
      if ( data == 0 )
      {
         // AS Feb 14/03 - Have added support for complete closing of temporary files
         // now, so there is a chance the table will not be found if it was temporary,
         // in which case just return success.  There is a risk here that should be addressed.
         // return error4( c4, e4name, E93801 ) ;
         return 0 ;
      }

      doSpecial = 0 ;
      if ( data->recordChanged == 1 )   /* override flush */
      {
         if ( d4recNo( data ) == recNo )
         {
            /* special, case (automatic rollback) -- save record for user */
            doSpecial = 1 ;
            saveRec = data->record ;
            data->record =(char*) u4allocFree( c4, dfile4recWidth( data->dataFile ) + 1 ) ;
            if ( data->record == 0 )
            {
               /* user will lose changes, due to low memory, but can at least rollback */
               data->record = saveRec ;
               doSpecial = 0 ;
            }
            else
               memcpy( data->record, saveRec, dfile4recWidth( data->dataFile ) ) ;
         }
         data->recordChanged = 0 ;
      }
      rc = 0 ;
      if ( doSpecial == 0 )
         rc = d4go( data, recNo ) ;

      if ( rc == 0 )
      {
         // AS Mar 23/10 add extra logging of rollback scenario
         #ifdef S4LOG_TRANS_ROLLBACK
            FILE4 rollbackLogFile ;
            int oldSaveOpen = c4->errOpen ;
            c4->errOpen = 0 ;
            int orc = file4openInternal( &rollbackLogFile, c4, "ROLLBACK.OUT", 0, OPT4NONE ) ;
            c4->errOpen = oldSaveOpen ;
            if ( orc != 0 ) // try creating the file instead
            {
               orc = file4createInternal( &rollbackLogFile, c4, "ROLLBACK.OUT", 0, OPT4NONE ) ;
            }
            if ( orc == 0 )  // file ready...
            {
               FILE4LONG pos = file4lenLow( &rollbackLogFile ) ;
               long recWidth = dfile4recWidth( data->dataFile ) ;
               const char *writeStr = "\r\ntran4lowunwrite\r\n" ;
               file4writeInternal( &rollbackLogFile, pos, writeStr, strlen(writeStr) ) ;
               file4longAdd( &pos, strlen(writeStr) ) ;
               file4writeInternal( &rollbackLogFile, pos, data->alias, strlen( data->alias ) ) ;  // the table name
               file4longAdd( &pos, strlen( data->alias ) ) ;
               file4writeInternal( &rollbackLogFile, pos, &recWidth, sizeof( recWidth ) ) ;  // write the record length
               file4longAdd( &pos, sizeof( recWidth ) ) ;
               file4writeInternal( &rollbackLogFile, pos, data->record, recWidth ) ;  // write the current record
               file4longAdd( &pos, recWidth ) ;
               file4writeInternal( &rollbackLogFile, pos, tran4getData( trans, sizeof(S4LONG ) ), recWidth ) ;  // write the record we are changing it to
               file4close( &rollbackLogFile ) ;
            }
         #endif

         #ifndef S4OFF_MULTI
            if ( d4lockTest( data, recNo, lock4write ) != 1 )
               if ( d4lockInternal( data, recNo, 0 ) != 0 )
               {
                  // AS Mar 4/10 - log more info about what is going on in this case
                  CODE4 *c4 = trans->c4trans->c4 ;
                  char moreInfo[80] ;
                  sprintf_s( moreInfo, "unable to lock record: %ld", recNo ) ;
                  rc = error4describe( c4, e4lock, E83804, "failure to lock record to complete transaction rollback for data file:", d4alias( data ), moreInfo ) ;
                  int oldSafety = c4->safety ;
                  c4->safety = 0 ;
                  int svCode = error4set( c4, 0 ) ;
                  FILE4 file ;
                  FILE4LONG pos ;
                  file4longAssign( pos, 0, 0L ) ;
                  if ( file4createInternal( &file, c4, "OLD_RECORD.FIL", 0, OPT4NONE ) != 0 )
                  {
                     error4describe( c4, e4lock, E83804, "Unable to create old record file", 0, 0 ) ;
                  }
                  else
                  {
                     file4writeInternal( &file, pos, data->record, dfile4recWidth( data->dataFile ) ) ;
                     file4close( &file ) ;
                  }
                  if ( file4createInternal( &file, c4, "NEW_RECORD.FIL", 0, OPT4NONE ) != 0 )
                  {
                     error4describe( c4, e4lock, E83804, "Unable to create new record file", 0, 0 ) ;
                  }
                  else
                  {
                     file4writeInternal( &file, pos, tran4getData( trans, sizeof(S4LONG ) ), dfile4recWidth( data->dataFile ) ) ;
                     file4close( &file ) ;
                  }

                  error4set( c4, svCode ) ;
                  c4->safety = oldSafety ;
               }
         #endif
      }

      if ( rc == 0 )
      {
         memcpy( d4record( data ), rec, (size_t)dfile4recWidth( data->dataFile ) ) ;
         #ifndef S4MEMO_OFF
            pos = (long)sizeof(S4LONG ) + 2L * dfile4recWidth( data->dataFile ) ;
            for ( i = 0; i < data->dataFile->nFieldsMemo; i++ )
            {
               // AS 01/17/01 - Problem here:  We have copied the 'previous' record into
               // our record buffer.  However, that 'previous' record may contain incorrect
               // memo field markers since if the memo field was updated the entry previously
               // used may now be invalid.  What we want to do is to re-write the 'rollback record'
               // memo block entries into the 'previous' record.  This will ensure that the
               // entries are up to date and the correct blocks are used for memo file
               // manipulation.
               FIELD4 *field = data->fieldsMemo[i].field ;
               memcpy( data->record + field->offset, data->recordOld + field->offset, field->len ) ;

               #ifdef S4DATA_ALIGN
                  /* LY July 25/03 : changed from sizeof(unsigned int*) */
                  memcpy(&ptrLen, tran4getData( trans, pos ), sizeof(ptrLen) ) ;
               #else
                  ptrLen = *( (unsigned int *)tran4getData( trans, pos ) ) ;
               #endif
               pos += sizeof( ptrLen ) ;
               // AS 01/25/01 - Even if len is zero, must assign because we
               // may be 'undoing' a memo assign as part of the rollback
               // if ( ptrLen != 0 )
               // AS Mar 23/05 - if a memo is unchanged we set both ptrlens to 0, so look at this case
               // if that is the case we don't need to perform any memo assign...
               long pos2 = pos+ptrLen ;
               unsigned S4LONG ptrLen2 ;
               #ifdef S4DATA_ALIGN
                  memcpy(&ptrLen2, tran4getData(trans, pos2), sizeof(ptrLen2));
               #else
                  ptrLen2 = *( (unsigned S4LONG *)tran4getData( trans, pos2 ) ) ;
               #endif
               if ( ptrLen != 0 || ptrLen2 != 0 )
                  f4memoAssignN( field, (char *)tran4getData( trans, pos ), (unsigned int)ptrLen ) ;

               pos += ptrLen ;
               #ifdef S4DATA_ALIGN
                  memcpy(&ptrLen, tran4getData(trans, pos), sizeof(ptrLen));
               #else
                  ptrLen = *( (unsigned S4LONG *)tran4getData( trans, pos ) ) ;
               #endif
               pos += sizeof( ptrLen ) + ptrLen ;
            }
         #endif
      }

      if ( rc == 0 )
      {
         /* within transaction stuff, do not lock or unlock record -- it should
            be in a locked stage already, and if not (ole-db chaos), it doesn't
            matter */
         rc = d4writeLow( data, recNo, 0, 0 ) ;
      }

      if ( rc == 0 )
         rc = d4update( data ) ;

      if ( doSpecial )
      {
         u4free( data->record ) ;
         data->record = saveRec ;
         data->recordChanged = 1 ;
      }

      return rc ;
   }



   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   /* clientId is a unique id for the transactee.  In the single user case, there is only one */
   int S4FUNCTION tran4lowStart( TRAN4 *trans, long clientId, int doUnlock )
   {
      int rc ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      #if !defined( S4OFF_MULTI ) && !defined (S4UTILS) && !defined( S4TRANS_FULL_LOCK_OFF )
         int oldLockAttempts ;
      #endif

      #ifdef E4PARM_HIGH
         if ( trans == 0 )
            return error4( 0, e4parm_null, E93801 ) ;
      #endif

      CODE4 *c4 = trans->c4trans->c4 ;

      if ( trans->c4trans->enabled == 0 )
      {
         #ifdef S4STAND_ALONE
            if ( c4->logOpen == 0 )
               return error4( c4, e4trans, E83814 ) ;
            rc = code4logOpen( c4, 0, 0 ) ;
         #else
            assert5( 0 ) ;  // should never get here - code below does not take backup server into account
            rc = code4transFileEnable( trans->c4trans, 0, 0 ) ;
         #endif
         if ( rc < 0 )
            return rc ;
      }

      if ( trans->c4trans->enabled != 1 )
         return error4( c4, e4trans, E83807 ) ;

      #ifdef S4STAND_ALONE
         if ( trans->currentTranStatus == r4active )   /* already in a transactional state */
            return error4( c4, e4trans, E93801 ) ;
      #endif

      if ( trans->c4trans->transFile->status != tran4notRollbackOrCommit )
         return error4( c4, e4trans, E83801 ) ;

      #ifndef S4OFF_MULTI
         if ( doUnlock == 1 )
         {
            rc = code4unlock( c4 ) ;
            if ( rc < 0 )
               return rc ;
         }
      #endif

      // AS Apr 28/03 - made trans-shared a run-time switch
      if ( c4->transShared == 1 )
      {
         // in shared mode there are 3 options:
         // off-multi - no locking
         // full-lock-off - only lock the transaction file while it is being written to
         // full lock - lock the transaction file for the duration of the transaction.  This is the most
         //             efficient but can cause locking contention whereby 1 user may get locked out, esp.
         //             in ODBC where transactions may be always set on.
         #if !defined( S4OFF_MULTI ) && !defined (S4UTILS) && !defined( S4TRANS_FULL_LOCK_OFF )
            oldLockAttempts = c4->lockAttempts ;
            c4->lockAttempts = WAIT4EVER ;
            rc = code4tranLockTransactions( trans->c4trans, TRAN4LOCK_MULTIPLE ) ;
            c4->lockAttempts = oldLockAttempts ;
            if ( rc < 0 )
               rc = error4( c4, rc, E93801 ) ;
         #endif
         tran4bottom( trans ) ;
      }

      /* AS 09/09/98 because the time function is expensive, only call when
         starting transactions.  The same time is uses for the entire transaction
      */
      tran4getTime( c4 ) ;
      rc = tran4set( trans, trans->currentTranStatus, -1L, clientId, TRAN4START, 0, 0L, 0L ) ;
      if ( rc < 0 )
         return rc ;
      if ( tran4lowAppend( trans, "\0", 0 ) != 0 )
         return e4transAppend ;

      trans->currentTranStatus = r4active ;

      #ifndef S4OFF_MULTI
         trans->savedUnlockAuto = trans->unlockAuto ;
         trans->unlockAuto = 0 ;
      #endif

      return 0 ;
   }



   #ifdef S4SERVER
      int code4tranRollbackSingle( CODE4 *c4 )
      {
         // This function is used (internally) to rollback a transaction as part of a
         // complete mini transaction.  As such, it does not count towards the # of
         // transactions performed by the server, so the count is decremented by 1.
         #ifdef E4PARM_LOW
            if ( c4 == 0 )
               return error4( 0, e4parm_null, E93828 ) ;
         #endif
         TRAN4 *trans = &(c4->currentClient->trans) ;
         TRAN4FILE *t4 = trans->c4trans->transFile ;
         int oldBackup = t4->useBackup ;
         if ( t4->primary.isDisabled != log4enabled )
            t4->useBackup = 1 ;
         // AS Jan 26/06 - also, we don't want to perform unlocks here...that will be done by higher level functions if required
         #ifdef S4SERVER
            int oldUnlock = code4unlockAuto( c4 ) ;
            code4unlockAutoSet( c4, LOCK4OFF ) ;
            // int oldTransUnlock = trans->unlockAuto ;
            trans->unlockAuto = 0 ;
            int oldSavedTransUnlock = trans->savedUnlockAuto ;
            trans->savedUnlockAuto = 0 ;
         #endif
         int rc = tran4lowRollback( trans, (c4)->currentClient->id, 0 ) ;
         #ifdef S4SERVER
            code4unlockAutoSet( c4, oldUnlock ) ;
            // trans->unlockAuto = oldTransUnlock ;
            // use the saved lock to reset both (that's what tran4lowRollback will do)
            trans->savedUnlockAuto = oldSavedTransUnlock ;
            trans->unlockAuto = oldSavedTransUnlock ;
         #endif
         t4->useBackup = oldBackup ;
         #ifdef S4SERVER_GUI
            if ( rc == 0 )  // don't count this as a real rollback
               c4->server->info.numTransRolledBack-- ;
         #endif
         return rc ;
      }
   #endif



   int S4FUNCTION tran4lowRollback( TRAN4 *trans, long id, const int doInvalidate )
   {
      #ifdef S4WINCE
         S4LONG tempLong ;
      #endif
      #ifdef E4PARM_HIGH
         if ( trans == 0 )
            return error4( 0, e4parm, E93801 ) ;
      #endif

      CODE4 *c4 = trans->c4trans->c4 ;
      int saveErr = error4set( c4, 0 ) ;
      long transId = tran4getTransId( trans ) ;

      #ifdef E4ANALYZE
         if ( transId <= 0 )
            return error4( 0, e4info, E93801 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( trans->c4trans->enabled != 1 )
            return error4( c4, e4rollback, E83807 ) ;
      #endif

      if ( trans->currentTranStatus != r4active )
         return error4( c4, e4transStatus, E83808 ) ;

      if ( trans->c4trans->transFile->status != tran4notRollbackOrCommit )
         return error4( c4, e4trans, E83801 ) ;
      trans->c4trans->transFile->status = tran4rollbackOrCommit ;

      trans->currentTranStatus = r4rollback ;

      /* first rollback all the transactions, then mark a rollback as having occurred */
      int rc = tran4bottom( trans ) ;
      if ( rc != 0 )
         return error4stack( c4, rc, E93801 ) ;

      int done ;
      for( done = 0, rc = 0; !done && !rc ; )
      {
         #ifdef S4WINCE
            memcpy( &tempLong, trans->header.transId, sizeof(S4LONG) ) ;
            if ( tempLong == transId )
         #else
            if ( tran4id( trans ) == transId )
         #endif
         {
            switch( tran4type( trans ) )
            {
               case TRAN4START:
                  done = 1 ;
                  break ;
               case TRAN4WRITE:
                  rc = tran4lowUnwrite( trans ) ;
                  break ;
               case TRAN4APPEND:
                  rc = tran4lowUnappend( trans ) ;
                  break ;
               case TRAN4VOID:   /* transaction connectioning to next was voided, so skip... */
               case TRAN4OPEN:
               case TRAN4OPEN_TEMP:
               case TRAN4CLOSE:
               // AS 09/29/00 - allow for TRAN4CREATE (which gets ignored on rollback)
               case TRAN4CREATE:
               // AS Jan 17/06 - if the file is encrypted, mark it as such with a new create marker
               case TRAN4CREATE_ENCRYPT:
                  break ;
               default:
                  rc = error4( c4, e4rollback, E83809 ) ;
            }
         }
         if ( !done && !rc )
            rc = tran4skip( trans, TRAN4BACKWARDS ) ;
      }
      if ( rc > 0 )
         rc = 0 ;

      if ( rc == 0 )
      {
         /* AS 09/09/98 because the time function is expensive, only call when
            starting transactions.  The same time is uses for the entire transaction
         */
         tran4getTime( c4 ) ;
         tran4set( trans, trans->currentTranStatus, transId, id, TRAN4ROLLBACK, 0, 0L, 0L ) ;
         if ( tran4lowAppend( trans, "\0", 1 ) != 0 )
            return e4transAppend ;
      }

      trans->currentTranStatus = r4inactive ;
      trans->transId = 0;

      #ifndef S4OFF_MULTI
         trans->unlockAuto = trans->savedUnlockAuto ;
         #if !defined( S4UTILS ) && !defined( S4TRANS_FULL_LOCK_OFF )
            if ( c4->transShared == 1 )
               rc = code4tranUnlockTransactions( trans->c4trans, TRAN4LOCK_MULTIPLE ) ;
         #endif
      #endif

      if ( saveErr != 0 )
         error4set( c4, saveErr ) ;

      if ( rc == 0 )
         trans->c4trans->transFile->status = tran4notRollbackOrCommit ;
      tran4lowCloseDelayed( trans ) ;
      #ifdef S4SERVER_GUI
         c4->server->info.numTransRolledBack++ ;
      #endif
      #ifdef S4SERVER
         if ( code4unlockAuto( c4 ) == 1 )
            rc = code4unlock( c4 ) ;
      #else
         if ( doInvalidate )
            code4invalidate( c4 ) ;
      #endif
      return rc ;
   }



   static int tran4lowRemoveKeys( TRAN4 *trans )
   {
      DATA4 *data ;
      #ifndef S4OFF_INDEX
         int rc, saveRc ;
         TAG4 *tag ;
         TAG4KEY_REMOVED *removed ;

         saveRc = 0 ;
         #ifndef S4CLIPPER
            int requiresUnlock = 0 ;
         #endif
      #endif

      for ( data = 0 ;; )  /* first do open files */
      {
         data =(DATA4 *) l4next( tran4dataList( trans ), data ) ;
         if ( data == 0 )
            break ;
         #ifndef S4OFF_INDEX
            int isLocked = 0 ;  // AS Feb 11/04 - initialize to 0
            for ( tag = 0 ;; )
            {
               tag = d4tagNext( data, tag ) ;
               if ( tag == 0 )
                  break ;
               for ( removed = 0 ;; )
               {
                  // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
                  #ifdef SHARE4TAG_REMOVE
                     removed = (TAG4KEY_REMOVED *)l4first( &tag->tagFile->removedKeys ) ;
                  #else
                     removed = (TAG4KEY_REMOVED *)l4first( &tag->removedKeys ) ;
                  #endif
                  if ( removed == 0 )
                     break ;
                  #ifndef S4OFF_MULTI  // LY Jun 21/04
                     // AS Nov 27/03 - ensure index locked
                     #ifdef S4CLIPPER
                        isLocked = tfile4lockTest( tag->tagFile ) ;
                        if ( isLocked == 0 )
                        {
                           rc = tfile4lock( tag->tagFile, data4serverId( data ) ) ;
                           if ( rc < 0 )
                           {
                              saveRc = rc ;
                              break ;
                           }
                        }
                     #else
                        if ( isLocked == 0 )
                        {
                           // AS Jan 6/04 - server fix
                           #ifdef S4SERVER
                              isLocked = dfile4lockTestIndex( data->dataFile, data4serverId( data ) ) ;
                           #else
                              isLocked = d4lockTestIndex( data ) ;
                           #endif
                           if ( isLocked == 0 )
                           {
                              rc = d4lockIndex( data ) ;
                              if ( rc < 0 )
                              {
                                 saveRc = rc ;
                                 break ;
                              }
                              isLocked = 1 ;
                              requiresUnlock = 1 ;
                           }
                        }
                     #endif
                  #endif
                  rc = tfile4remove( tag->tagFile, removed->key, removed->recno ) ;
                  if ( rc < 0 )
                     saveRc = rc ;
                  // AS May 8/07 - don't unlock until after all the keys are removed (moved below loop)
                  #ifndef S4OFF_MULTI  // LY Jun 21/04
                     #ifdef S4CLIPPER
                        if ( isLocked == 0 )  // it was not locked, so unlock it now
                           tfile4unlock( tag->tagFile, data4serverId( data ) ) ;
                     #endif
                  #endif
                  // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
                  #ifdef SHARE4TAG_REMOVE
                     l4remove( &tag->tagFile->removedKeys, removed ) ;
                  #else
                     l4remove( &tag->removedKeys, removed ) ;
                  #endif
                  u4free( removed ) ;
               }
            }
            #if !defined( S4OFF_INDEX ) && !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
               if ( requiresUnlock == 1 )
               {
                  d4unlockIndex( data ) ;
                  requiresUnlock = 0 ;   // reset the flag
                  isLocked = 0 ;
               }
            #endif
         #endif
      }

      for ( data = 0 ;; )  /* now do closed files */
      {
         data =(DATA4 *) l4next( &trans->closedDataFiles, data ) ;
         if ( data == 0 )
            break ;
         #ifndef S4OFF_INDEX
            int isLocked = 0 ;  // AS Feb 11/04 - initialize to 0
            for ( tag = 0 ;; )
            {
               tag = d4tagNext( data, tag ) ;
               if ( tag == 0 )
                  break ;
               for ( removed = 0 ;; )
               {
                  // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
                  #ifdef SHARE4TAG_REMOVE
                     removed = (TAG4KEY_REMOVED *)l4first( &tag->tagFile->removedKeys ) ;
                  #else
                     removed = (TAG4KEY_REMOVED *)l4first( &tag->removedKeys ) ;
                  #endif
                  if ( removed == 0 )
                     break ;
                  #ifndef S4OFF_MULTI  // LY Jun 21/04
                     // AS Nov 27/03 - ensure index locked
                     #ifdef S4CLIPPER
                        isLocked = tfile4lockTest( tag->tagFile ) ;
                        if ( isLocked == 0 )
                        {
                           rc = tfile4lock( tag->tagFile, data4serverId( data ) ) ;
                           if ( rc < 0 )
                           {
                              saveRc = rc ;
                              break ;
                           }
                        }
                     #else
                        if ( isLocked == 0 )
                        {
                           // AS Jan 6/04 - server fix
                           #ifdef S4SERVER
                              isLocked = dfile4lockTestIndex( data->dataFile, data4serverId( data ) ) ;
                           #else
                              isLocked = d4lockTestIndex( data ) ;
                           #endif
                           if ( isLocked == 0 )
                           {
                              rc = d4lockIndex( data ) ;
                              if ( rc < 0 )
                              {
                                 saveRc = rc ;
                                 break ;
                              }
                              isLocked = 1 ;
                              requiresUnlock = 1 ;
                           }
                        }
                     #endif
                  #endif
                  rc = tfile4remove( tag->tagFile, removed->key, removed->recno ) ;
                  if ( rc < 0 )
                     saveRc = rc ;
                  #ifndef S4OFF_MULTI  // LY Jun 21/04
                     #ifdef S4CLIPPER
                        if ( isLocked == 0 )  // it was not locked, so unlock it now
                           tfile4unlock( tag->tagFile, data4serverId( data ) ) ;
                     #endif
                  #endif
                  #if !defined( S4OFF_INDEX ) && !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
                     if ( requiresUnlock == 1 )
                     {
                        d4unlockIndex( data ) ;
                        requiresUnlock = 0 ;   // reset the flag
                        isLocked = 0 ;
                     }
                  #endif
                  #ifdef SHARE4TAG_REMOVE
                     l4remove( &tag->tagFile->removedKeys, removed ) ;
                  #else
                     l4remove( &tag->removedKeys, removed ) ;
                  #endif
                  u4free( removed ) ;
               }
            }
         #endif
      }

      #ifndef S4OFF_INDEX
         return saveRc ;
      #else
         return 0 ;
      #endif
   }



   static int tran4updateData( TRAN4 *trans )
   {
      DATA4 *data ;
      int rc, saveRc, oldTransStatus ;

      /* changes trans status to force an update */
      oldTransStatus = trans->currentTranStatus ;
      trans->currentTranStatus = r4off ;
      for ( data = 0, saveRc = 0 ;; )
      {
         data = (DATA4 *)l4next( trans->dataList, data ) ;
         if ( data == 0 )
            break ;
         if ( data->dataFile->fileChanged == 1 )
         {
            #ifndef S4OFF_MULTI
               if ( d4lockTestAppend( data ) == 1 )
            #endif
            {
               rc = dfile4updateHeader( data->dataFile, 1, 1, 1 ) ;
               if ( rc < 0 )
                  saveRc = rc ;
            }
         }
      }

      trans->currentTranStatus = oldTransStatus ;
      return saveRc ;
   }



   static int tran4lowUpdate( TRAN4 *trans )
   {
      DATA4 *dataOn ;
      int rc ;

      for ( dataOn = 0 ;; )
      {
         dataOn = (DATA4 *)l4next( tran4dataList( trans ), dataOn ) ;
         if ( dataOn == 0 )
            break ;
         if ( dataOn->transChanged )  /* if the data file was changed during the transaction... */
         {
            #ifndef S4OFF_MULTI
               rc = d4lockTestAppend( dataOn ) ;
               if ( rc < 0 )
                  return rc ;
               if ( rc == 1 )
                  dfile4updateHeader( dataOn->dataFile, 1, 1, 1 ) ;
            #else
               dfile4updateHeader( dataOn->dataFile, 1, 1, 1 ) ;
            #endif
            // AS Apr 16/02 - Need to flush the file, not just update it... - was losing changes...
            // AS Jun 26/03 - We don't actually need to hard-flush the indexes.  They can always be recovered by reindexing
            // rc = d4flush( dataOn ) ;
            rc = d4flushData( dataOn ) ;
            if ( rc != 0 )
               return rc ;
            dataOn->transChanged = 0 ;
         }
      }

      return 0 ;
   }



   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4FUNCTION tran4lowCommitPhaseOne( TRAN4 *trans, long id, CommitPhaseType phaseType )
   {
      /*
         phase types - single or dual.  If we are performing a dual-phase commit,
         then we don't flush the transaction file because it will be done at the
         end of the 2nd phase which is occurring right away anyway.
      */
      int rc ;

      #ifdef E4PARM_LOW
         if ( trans == 0 )
            return error4( 0, e4parm_null, E93801 ) ;
      #endif

      CODE4 *c4 = trans->c4trans->c4 ;

      if ( trans->c4trans->enabled != 1 )
         return error4( c4, e4trans, E83807 ) ;

      if ( trans->currentTranStatus != r4active )
         return 0 ;

      while ( trans->c4trans->transFile->status != tran4notRollbackOrCommit )  /* means another user is manipulating, wait for them to finish */
      {
         #ifdef S4OFF_MULTI
            return error4( c4, e4trans, E83801 ) ;  // another user should not manipulat if off-multi
         #else
            u4delayHundredth( 1 ) ;
         #endif
      }

      trans->c4trans->transFile->status = tran4rollbackOrCommit ;

      // ensure partial before update, otherwise flushes are delayed.
      trans->currentTranStatus = r4partial ;

      // AS 02/05/01 - tran4lowUpdate() flushes any outstanding changes.  If it fails (eg. appending duplicate
      // record), we want to fail, but set the transaction status back to notRollbackOrCommit...
      rc = tran4lowUpdate( trans ) ;
      if ( rc != 0 )
      {
         trans->c4trans->transFile->status = tran4notRollbackOrCommit ;
         return rc ;
      }

      /* AS 09/09/98 because the time function is expensive, only call when
         starting transactions.  The same time is uses for the entire transaction
      */
      tran4getTime( c4 ) ;
      rc = tran4set( trans, trans->currentTranStatus, -1L, id, TRAN4COMMIT_PHASE_ONE, 0, 0L, 0L ) ;
      if ( rc < 0 )
         return rc ;

      #ifdef S4MULTI_SERVER
         rc = tran4putData( trans, &numServers, sizeof( numServers ) ) ;
         if ( rc < 0 )
            return rc ;
      #endif

      if ( tran4lowAppend( trans, "\0", ( phaseType == singlePhaseCommit ) ? 1 : 0 ) != 0 )
         return e4transAppend ;

      return 0 ;
   }



   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4FUNCTION tran4lowCommitPhaseTwo( TRAN4 *trans, long id, int doUnlock )
   {
      CODE4 *c4 ;
      int rc ;

      #ifdef E4PARM_LOW
         if ( trans == 0 )
            return error4( 0, e4parm_null, E93801 ) ;
      #endif

      c4 = trans->c4trans->c4 ;

      if ( trans->currentTranStatus != r4partial )
         return error4( trans->c4trans->c4, e4commit, E83811 ) ;

      if ( trans->c4trans->transFile->status != tran4rollbackOrCommit )  /* we should be manipulating */
         return error4( c4, e4trans, E83801 ) ;

      rc = tran4set( trans, trans->currentTranStatus, -1, id, TRAN4COMMIT_PHASE_TWO, 0, 0L, 0L ) ;
      if ( rc < 0 )
         return rc ;
      if ( tran4lowAppend( trans, "\0", 1 ) != 0 )
         return e4transAppend ;

      rc = tran4lowRemoveKeys( trans ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E93801 ) ;
      trans->currentTranStatus = r4inactive ;
      trans->transId = 0;
      #ifndef S4OFF_MULTI
         trans->unlockAuto = trans->savedUnlockAuto ;
      #endif
      rc = tran4updateData( trans ) ;
      #ifndef S4OFF_MULTI
         if ( rc == 0 && doUnlock )
         {
            if ( code4unlockAuto( trans->c4trans->c4 ) == 1 )
               rc = code4unlock( trans->c4trans->c4 ) ;
         }
         // AS Apr 28/03 - made trans-shared a run-time switch
         #if !defined( S4UTILS ) && !defined( S4TRANS_FULL_LOCK_OFF )
            if ( c4->transShared == 1 )
               if ( rc == 0 )
                  rc = code4tranUnlockTransactions( trans->c4trans, TRAN4LOCK_MULTIPLE ) ;
         #endif
      #endif

      if ( rc == 0 )
         trans->c4trans->transFile->status = tran4notRollbackOrCommit ;

      tran4lowCloseDelayed( trans ) ;

      trans->currentTranStatus = r4inactive ;
      #ifndef S4OFF_MULTI
         trans->unlockAuto = trans->savedUnlockAuto ;
      #endif
      #ifdef S4SERVER_GUI
         c4->server->info.numTransCommitted++ ;
      #endif

      return rc ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE )
   int S4FUNCTION code4tranCommitPhaseOne( CODE4 *c4, CommitPhaseType phaseType )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      return tran4lowCommitPhaseOne( &c4->currentClient->trans, c4->currentClient->id, 1, phaseType ) ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE )
   int S4FUNCTION code4tranStart( CODE4 *c4 )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      return tran4lowStart( &c4->currentClient->trans, c4->currentClient->id, 0 ) ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE )
   int S4FUNCTION code4tranStartSingle( CODE4 *c4 )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      return tran4lowStart( &c4->currentClient->trans, c4->currentClient->idc4)->currentClient->id, 0 ) ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE )
   int S4FUNCTION code4tranCommitPhaseTwo( CODE4 *c4, int doUnlock )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      return tran4lowCommitPhaseTwo( &c4->currentClient->trans, c4->currentClient->id, doUnlock ) ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE )
   int S4FUNCTION code4tranRollback( CODE4 *c4 )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      return tran4lowRollback( &c4->currentClient->trans, c4->currentClient->id, 1 ) ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4SERVER ) && !defined( S4INLINE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   int S4FUNCTION code4tranCommit( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      if ( code4trans(c4)->currentTranStatus != r4partial )   /* do full commit */
         if ( code4tranCommitPhaseOne( c4, dualPhaseCommit ) != 0 )
            return -1 ;

      return code4tranCommitPhaseTwo( c4, 1 ) ;
   }
#endif /* #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
   int S4FUNCTION code4tranCommitSingle( CODE4 *c4 )
   {
      // This function is used (internally) to commit a transaction as part of a
      // complete mini transaction.  As such, it does not count towards the # of
      // transactions performed by the server, so the count is decremented by 1.
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      if ( code4tranCommitPhaseOne( c4, dualPhaseCommit ) != 0 )
         return -1 ;

      int rc = code4tranCommitPhaseTwo( c4, 0 ) ;
      #ifdef S4SERVER_GUI
         if ( rc == 0 )
            c4->server->info.numTransCommitted-- ;
      #endif

      return rc ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4SERVER )
   static void code4invalidate( CODE4 *c4 )
   {
      /* places all DATA4's into an invalid state */
      DATA4 *dataOn ;

      #ifdef E4PARM_LOW
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E93827 ) ;
            return ;
         }
      #endif

      for ( dataOn = 0 ;; )
      {
         dataOn = (DATA4 *)l4next( c4->c4trans.trans.dataList, dataOn ) ;
         if ( dataOn == 0 )
            break ;
         /* 04/24/96 --> d4blank() replacing memset due to S4FOX binary fields having non-blank contents to represent blank */
         dataOn->recNum = dataOn->recNumOld = -1 ;  /* ensure that d4blank works with lock-enforce on, plus reset record # */
         d4blank( dataOn ) ;
         d4changed( dataOn, 0 ) ;
         // AS Apr 10/03 - also free the index blocks since they are probably now out of date
         d4freeBlocks( dataOn ) ;
      }
   }
#endif  /* #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4SERVER ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4STAND_ALONE )
   // !S4OFF_TRAN, !S4OFF_WRITE, S4STAND_ALONE
   int S4FUNCTION code4tranCommitPhaseOne( CODE4 *c4, CommitPhaseType phaseType )
   {
      int saveErr, rc ;

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      saveErr = error4code( c4 ) ;
      if ( saveErr < 0 )
         error4set( c4, 0 ) ;

      rc = tran4lowCommitPhaseOne( &c4->c4trans.trans, 0, phaseType ) ;

      #ifndef S4OFF_MULTI
         if ( code4unlockAuto( c4 ) == 1 )
            if ( code4unlock( c4 ) != 0 )
               return error4( c4, e4unlock, E93828 ) ;
      #endif

      if ( saveErr == 0 )
         saveErr = rc ;
      if ( saveErr != 0 )
         error4set( c4, saveErr ) ;

      return rc ;
   }



   // !S4OFF_TRAN, !S4OFF_WRITE, S4STAND_ALONE
   int S4FUNCTION code4tranCommitPhaseTwo( CODE4 *c4, int doUnlock )
   {
      int saveErr, rc ;

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      if ( c4->c4trans.trans.currentTranStatus != r4partial )
         return error4( c4, e4transStatus, E83812 ) ;

      saveErr = error4code( c4 ) ;
      if ( saveErr < 0 )
         error4set( c4, 0 ) ;

      rc = tran4lowCommitPhaseTwo( &c4->c4trans.trans, 0, doUnlock ) ;

      if ( saveErr == 0 )
         saveErr = rc ;
      if ( saveErr != 0 )
         error4set( c4, saveErr ) ;

      return rc ;
   }



   // !S4OFF_TRAN, !S4OFF_WRITE, S4STAND_ALONE
   int S4FUNCTION code4tranStart( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93829 ) ;
      #endif

      return tran4lowStart( &c4->c4trans.trans, 0, 0 ) ;
   }



   // !S4OFF_TRAN, !S4OFF_WRITE, S4STAND_ALONE
   int S4FUNCTION code4tranStartSingle( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93829 ) ;
      #endif

      return tran4lowStart( &c4->c4trans.trans, 0, 0 ) ;
   }



   // !S4OFF_TRAN, !S4OFF_WRITE, S4STAND_ALONE
   int S4FUNCTION code4tranRollback( CODE4 *c4 )
   {
      int rc ;

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93830 ) ;
      #endif

      if ( code4transEnabled( c4 ) != 1 )
         return error4( c4, e4trans, E83807 ) ;

      rc = tran4lowRollback( &c4->c4trans.trans, 0, 1 ) ;
      if ( rc < 0 )
         return rc ;

      #ifdef S4OFF_MULTI
         return 0 ;
      #else
         return code4unlock( c4 ) ;
      #endif
   }
#endif  /* #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4STAND_ALONE ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4CLIENT )
   /* !S4OFF_TRAN, !S4OFF_WRITE, S4CLIENT */
   int S4FUNCTION code4tranCommitPhaseTwo( CODE4 *c4, int doUnlock )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93828 ) ;
      #endif

      if ( c4->c4trans.trans.currentTranStatus != r4partial )
         return error4( c4, e4transStatus, E83812 ) ;

      CONNECTION4 *connection = &c4->defaultServer ;
      connection4assign( connection, CON4COMMIT_PHASE_TWO, 0, 0 ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc >= 0 )
         rc = connection4status( connection ) ;

      return rc ;
   }
#endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4CLIENT ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4CLIENT )
   #ifdef S4MULTI_SERVER
      /* !S4OFF_TRAN, !S4OFF_WRITE, S4CLIENT */
      int S4FUNCTION code4tranCommitPhaseOne( CODE4 *c4, CommitPhaseType phaseType )
      {
         int saveRc, saveErr ;
         CONNECTION4 *connection ;
         unsigned int dataLen ;
         int rc ;
         char *data ;

         #ifdef E4ANALYZE
            connection = 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
               return error4( 0, e4parm_null, E93828 ) ;
         #endif

         if ( code4transEnabled( c4 ) != 1 )
            return error4( c4, e4trans, E83807 ) ;

         if ( c4->c4trans.trans.currentTranStatus != r4active )
            return error4( c4, e4trans, E83812 ) ;

         if ( c4->servers.nLink == 0 )
            return error4( c4, e4connection, E93828 ) ;

         code4writeBufferFlush( c4 ) ;

         if ( error4code( c4 ) < 0 )
         {
            saveErr = error4code( c4 ) ;
            error4set( c4, 0 ) ;
         }
         else
            saveErr = 0 ;

         saveRc = 0 ;

         socket4 = (SOCKET4 *)l4first( &c4->servers ) ;
         memset( &info, 0, sizeof( CONNECTION4COMMIT_INFO_IN ) ) ;
         info.numServers = htons5(c4->servers.nLink) ;

         dataLen = sizeof( CONNECTION4COMMIT_INFO_IN ) + c4->servers.nLink * S4STAND_ALONE_ALIAS_LEN ;
         data = (char *)u4allocFree( c4, dataLen ) ;
         if ( data == 0 )
            return error4stack( c4, e4memory, E93828 ) ;

         memcpy( data, &info, sizeof( CONNECTION4COMMIT_INFO_IN ) ) ;

         rc = 0 ;
         socket4 = 0 ;
         for( numServers = 0 ;; numServers++ )  /* prepare the info packets */
         {
            socket4 = (SOCKET4 *)l4next( &c4->servers, socket4 ) ;
            if ( socket4 == 0 )
               break ;
            connection = socket4->connect ;
            if ( connection == 0 )
               return error4( c4, e4info, E93828 ) ;
            memcpy( data + sizeof( CONNECTION4COMMIT_INFO_IN ) + numServers * S4STAND_ALONE_ALIAS_LEN, socket4->serverName, NEED A LOCAL FOR SERVER NAME, SET TO SIZE AND S4MAX_SERVER_NAME_SI
         }

         rc = 0 ;
         socket4 = 0 ;
         for( ;; )  /* set commit message to all connected servers */
         {
            socket4 = (SOCKET4 *)l4next( &c4->servers, socket4 ) ;
            if ( socket4 == 0 )
               break ;
            connection = socket4->connect ;
            connection4assign( connection, CON4COMMIT_PHASE_ONE, 0, 0 ) ;
            connection4addData( connection, data, dataLen, NULL ) ;
            connection4sendMessage( connection ) ;
            rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               break ;
            rc = connection4status( connection ) ;
            if ( rc != 0 )
               break ;
         }

         if ( rc != 0 )  /* rollback everybody */
         {
            for( ;; )
            {
               socket4 = (SOCKET4 *)l4prev( &c4->servers, socket4 ) ;
               if ( socket4 == 0 )
                  break ;
               connection = socket4->connect ;
               connection4assign( connection, S4CANCEL_TRANSACTION, 0, 0 ) ;
               connection4sendMessage( connection ) ;
               connection4receiveMessage( connection, rc ) ;
            }

            return rc ;
         }

         socket4 = 0 ;
         for( ;; )  /* complete the commits */
         {
            socket4 = (SOCKET4 *)l4next( &c4->servers, socket4 ) ;
            if ( socket4 == 0 )
               break ;
            connection = socket4->connect ;
            connection4assign( connection, CON4COMMIT_PHASE_ONE_PHASE_TWO, 0, 0 ) ;
            connection4sendMessage( connection ) ;
            rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
            {
               saveRc = rc ;
               error4set( c4, 0 ) ;
               continue ;
            }
            rc = connection4status( connection ) ;
            if ( rc < 0 )
            {
               saveRc = rc ;
               error4set( c4, 0 ) ;
               continue ;
            }
         }

         if ( saveErr == 0 )
            saveErr = saveRc ;
         if ( saveErr != 0 )
            error4set( c4, saveErr ) ;

         c4->c4trans.trans.currentTranStatus = r4inactive ;
         tran4lowCloseDelayed( &c4->c4trans.trans ) ;
         c4->c4trans.trans.currentTranStatus = r4partial ;
         return 0 ;
      }



   #else /* S4MULTI_SERVER else */
      /* !S4OFF_TRAN, !S4OFF_WRITE, S4CLIENT */
      int S4FUNCTION code4tranCommit( CODE4 *c4 )
      {
         int saveRc, saveErr ;
         CONNECTION4 *connection ;

         #ifdef E4ANALYZE
            connection = 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
               return error4( 0, e4parm_null, E93828 ) ;
         #endif

         if ( code4trans( c4 )->currentTranStatus == r4partial )   // do 2nd phase commit
            return code4tranCommitPhaseTwo( c4, 1 ) ;

         if ( code4transEnabled( c4 ) != 1 )
            return error4( c4, e4trans, E83807 ) ;

         if ( c4->c4trans.trans.currentTranStatus != r4active )
            return error4( c4, e4trans, E83812 ) ;

         if ( !c4->defaultServer.connected )
            return error4( c4, e4connection, E93828 ) ;

         // Apr 25/02 - ensure batched writes get flushed first
         code4writeBufferFlush( c4 ) ;

         if ( error4code( c4 ) < 0 )
         {
            saveErr = error4code( c4 ) ;
            error4set( c4, 0 ) ;
         }
         else
            saveErr = 0 ;

         saveRc = 0 ;

         connection = &c4->defaultServer ;
         connection4assign( connection, CON4COMMIT_BOTH_PHASES, 0, 0 ) ;
         connection4sendMessage( connection ) ;
         saveRc = connection4receiveMessage( connection ) ;
         if ( saveRc >= 0 )
            saveRc = connection4status( connection ) ;

         if ( saveErr == 0 )
            saveErr = saveRc ;
         if ( saveErr != 0 )
            error4set( c4, saveErr ) ;

         c4->c4trans.trans.currentTranStatus = r4inactive ;
         tran4lowCloseDelayed( &c4->c4trans.trans ) ;

         return saveErr ;
      }



      int S4FUNCTION code4tranCommitPhaseOne( CODE4 *c4, CommitPhaseType phaseType )
      {
         int saveRc, saveErr ;
         CONNECTION4 *connection ;

         #ifdef E4ANALYZE
            connection = 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
               return error4( 0, e4parm_null, E93828 ) ;
         #endif

         if ( code4transEnabled( c4 ) != 1 )
            return error4( c4, e4trans, E83807 ) ;

         if ( c4->c4trans.trans.currentTranStatus != r4active )
            return error4( c4, e4trans, E83812 ) ;

         if ( !c4->defaultServer.connected )
            return error4( c4, e4connection, E93828 ) ;

         code4writeBufferFlush( c4 ) ;

         if ( error4code( c4 ) < 0 )
         {
            saveErr = error4code( c4 ) ;
            error4set( c4, 0 ) ;
         }
         else
            saveErr = 0 ;

         saveRc = 0 ;

         connection = &c4->defaultServer ;
         connection4assign( connection, CON4COMMIT_PHASE_ONE, 0, 0 ) ;
         connection4sendMessage( connection ) ;
         saveRc = connection4receiveMessage( connection ) ;
         if ( saveRc >= 0 )
            saveRc = connection4status( connection ) ;

         if ( saveErr == 0 )
            saveErr = saveRc ;
         if ( saveErr != 0 )
            error4set( c4, saveErr ) ;

         c4->c4trans.trans.currentTranStatus = r4inactive ;
         tran4lowCloseDelayed( &c4->c4trans.trans ) ;
         c4->c4trans.trans.currentTranStatus = r4partial ;
         return 0 ;
      }
   #endif /* S4MULTI_SERVER else */



   /* !S4OFF_TRAN, !S4OFF_WRITE, S4CLIENT */
   int S4FUNCTION code4tranStart( CODE4 *c4 )
   {
      CONNECTION4 *connection ;
      int rc ;
      #ifdef S4MULTI_SERVER
         SOCKET4 *socket4 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93829 ) ;
      #endif

      // AS Apr 29/03 - transcations are run-time in odbc now
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
            return e4notSupported ;
      #endif

      if ( code4transEnabled( c4 ) != 1 )
         return error4( c4, e4trans, E83807 ) ;

      if ( c4->c4trans.trans.currentTranStatus == r4active )
         return error4( c4, e4trans, E93829 ) ;

      code4writeBufferFlush( c4 ) ;

      rc = 0 ;
      #ifdef S4MULTI_SERVER
         socket4 = 0 ;
         for( ;; )  /* set start message to all connected servers */
         {
            socket4 = (SOCKET4 *)l4next( &c4->servers, socket4 ) ;
            if ( socket4 == 0 )
            break ;
            connection = socket4->connect ;
            connection4assign( connection, CON4START, 0, 0 ) ;
            connection4sendMessage( connection ) ;
            rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               break ;
            rc = connection4status( connection ) ;
            if ( rc != 0 )
            {
               if ( rc < 0 )
                  connection4error( connection, c4, rc, E93829 ) ;
               break ;
            }
         }
      #else
         if ( !c4->defaultServer.connected )
            return error4( c4, e4connection, E93828 ) ;

         if ( c4->defaultServer.connected )
         {
            connection = &c4->defaultServer ;
            connection4assign( connection, CON4START, 0, 0 ) ;
            connection4sendMessage( connection ) ;
            rc = connection4receiveMessage( connection ) ;
            if ( rc >= 0 )
               rc = connection4status( connection ) ;
         }

         if ( rc < 0 )   // AS Jan 31/02 - generate an error here, don't just return a non-errored rc (from connect4status())
            return error4( c4, rc, E93829 ) ;
      #endif

      c4->c4trans.trans.currentTranStatus = r4active ;

      return rc ;
   }



   /* !S4OFF_TRAN, !S4OFF_WRITE, S4CLIENT */
   int S4FUNCTION code4tranRollback( CODE4 *c4 )
   {
      CONNECTION4 *connection ;
      int rc ;
      #ifdef S4MULTI_SERVER
         SOCKET4 *socket4 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E93830 ) ;
      #endif

      // AS Apr 29/03 - transcations are run-time in odbc now
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
            return e4notSupported ;
      #endif

      if ( code4transEnabled( c4 ) != 1 )
         return error4( c4, e4trans, E83807 ) ;

      if ( c4->c4trans.trans.currentTranStatus == r4inactive )
         return error4( c4, e4transStatus, E83808 ) ;

      code4writeBufferReset( c4 ) ;

      #ifdef S4MULTI_SERVER
         socket4 = 0 ;
         for( ;; )  /* set start message to all connected servers */
         {
            socket4 = (SOCKET4 *)l4next( &c4->servers, socket4 ) ;
            if ( socket4 == 0 )
               break ;
            connection = socket4->connect ;
            connection4assign( connection, TRAN4ROLLBACK, 0, 0 ) ;
            connection4sendMessage( connection ) ;
            rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               return error4( c4, rc, E93830 ) ;
            rc = connection4status( connection ) ;
            if ( rc < 0 )
               return connection4error( connection, c4, rc, E93830 ) ;
         }
      #else
         if ( !c4->defaultServer.connected )
            return error4( c4, e4connection, E93828 ) ;

         if ( c4->defaultServer.connected )
         {
            connection = &c4->defaultServer ;
            connection4assign( connection, CON4ROLLBACK, 0, 0 ) ;
            connection4sendMessage( connection ) ;
            rc = connection4receiveMessage( connection ) ;
            if ( rc >= 0 )
               rc = connection4status( connection ) ;
            if ( rc != 0 )
               return error4stack( c4, rc, E93830 ) ;
         }
      #endif

      c4->c4trans.trans.currentTranStatus = r4inactive ;

      /* 04/24/96 AS --> moved code4invalidate before unlock call, so that any
         changes left to records won't be flushed as a result of the unlock
         call */
      code4invalidate( c4 ) ;

      // AS Dec 17/02 - just a note, on the client side we always unlock on a rollback
      // no matter what the unlock-auto is set at.  This is due to reccount handling which
      // otherwise gets mixed up.
      rc = code4unlock( c4 ) ;
      if ( rc < 0 )
         return rc ;
      tran4lowCloseDelayed( &c4->c4trans.trans ) ;

      return 0 ;
   }
#endif /* #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && defined( S4CLIENT ) */



#if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
   #ifdef S4STAND_ALONE
      int S4FUNCTION code4transFileEnable( CODE4TRANS *c4trans, const char *logName, const int doCreate )
   #else
      int code4transFileEnable( CODE4TRANS *c4trans, const char *logName, const int doCreate )
   #endif
   {
      /*
         Enables the primary log file for logging (does not enable the backup log file)
      */
      #if defined( E4ANALYZE ) || !defined( S4CLIENT )
         int rc ;
      #endif

      // AS Apr 29/03 - transcations are run-time in odbc now
      CODE4 *c4 = c4trans->c4 ;

      // AS Jun 20/03 - Moved from c4code.c to allow setting of transShared to be done after code4init but prior
      // to opening log file (for c4dll.dll, odbc s/a uses exclusive log file, but non-odbc s/a shares)
      // AS Mar 21/03 - also need translocking in odbc server
      // #if ( defined( S4TESTING ) && defined( S4STAND_ALONE ) && ! defined( S4OFF_TRAN ) ) || ( defined( S4SERVER ) && (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )))
      // AS Aug 6/03 - Not available on client
      // AS Aug 29/03 - Not available on server either
      #ifdef S4STAND_ALONE
         if ( c4->transShared == 1 )
            c4->doTransLocking = 1 ;   /* for testing, to allow one program to have multiple transactions in progress */
      #endif
      // #endif

      #ifdef S4STAND_ALONE
         // AS Jun 19/03 - open shared if shared, else exclusive
         if ( c4->transShared == 1 )
            c4->logAccessMode = OPEN4DENY_NONE ;
         else
            c4->logAccessMode = OPEN4DENY_RW ;
      #endif

      // AS Jun 13/-3 - allow non-shared for stand-alone
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
            return e4notSupported ;
      #endif

      if ( c4trans->enabled == 1 )
         return 0 ;

      #ifdef S4STAND_ALONE
         rc = code4tranInitLow( &c4trans->trans, c4trans ) ;
         if ( rc < 0 )
            return rc ;
      #endif

      #ifdef E4ANALYZE
         if ( ( rc = code4transVerify( c4trans, 1 ) ) < 0 )
            return rc ;
      #endif

      #ifndef S4CLIENT
         rc = 0 ;

         if ( c4trans->enabled == 0 )
         {
            #ifndef S4SERVER
               if ( logName != 0 )
               {
                  if ( c4->transFileName != 0 )
                     u4free( c4->transFileName ) ;
                  int lnamelen = (long)strlen( logName ) + 1L ;
                  c4->transFileName = (char *)u4allocFree( c4, lnamelen ) ;
                  if ( c4->transFileName == 0 )
                     rc = e4memory ;
                  else
                     c4strcpy( c4->transFileName, lnamelen, logName ) ;  // AS Dec 13/05 vs 5.0 fixes
               }
               const char *logName = c4->transFileName ;
            #endif

            #ifdef S4SERVER
               if ( c4->logDisable == log4disabled )
               {
                  // mark the c4trans as having been set up so that the backup log file will get processed if enabled
                  c4trans->enabled = 1 ;
                  return 0 ;
               }
               const char *logName = c4->server->logName ;
            #endif

            if( logName != 0 )
            {
               #ifdef S4SERVER
                  tran4fileLowInit( &(c4->server->transFile.primary), c4trans, &(c4->server->transFile) ) ;
                  // c4->server->transFile.transId = -1 ;
               #else
                  rc = tran4fileInit( &c4->transFile, c4trans ) ;
               #endif
               if ( rc == 0 )
               {
                  #ifdef S4SERVER
                     c4trans->transFile = &c4->server->transFile ;
                  #else
                     c4trans->transFile = &c4->transFile ;
                  #endif
                  if ( doCreate == 0 )
                  {
                     #ifdef S4SERVER
                        rc = tran4fileLowOpen( &c4trans->transFile->primary, logName ) ;
                     #else
                        rc = tran4fileOpen( c4trans->transFile, logName ) ;
                     #endif
                  }
                  else
                  {
                     rc = tran4fileCreate( c4trans->transFile, logName ) ;
                  }
                  if ( rc == 0 )
                     c4trans->enabled = 1 ;
                  else
                  {
                     #ifdef S4SERVER
                        c4trans->transFile = 0 ;
                     #else
                        c4trans->transFile = 0 ;
                     #endif
                  }
               }
            }
         }
         if ( rc == 0 )
            c4trans->transFile->status = tran4notRollbackOrCommit ;
         else
         {
            #ifndef S4SERVER
               u4free( c4->transFileName ) ;
               c4->transFileName = 0 ;
            #endif
         }
         return rc ;
      #else
         return 0 ;
      #endif
   }



   // AS Not availble with ODBC builds
   #if defined( S4SERVER ) && !defined( S4ODBC_ENABLED ) && !defined( S4ODBC_BUILD )
      int S4FUNCTION code4transFileEnableBackup( CODE4TRANS *c4trans, const char *backupLogName, const int doCreate )
      {
         // sets up the backup log file for data redundancy

         int rc = 0 ;

         #ifdef E4ANALYZE
            if ( ( rc = code4transVerify( c4trans, 1 ) ) < 0 )
               return rc ;
         #endif

         CODE4 *c4 = c4trans->c4 ;

         // backup should not get enabled until the main log file is enabled
         assert5 ( c4trans->enabled == 1 ) ;

         if ( rc == 0 )
         {
            tran4fileLowInit( &(c4->server->transFile.backup), c4trans, &(c4->server->transFile) ) ;
            // allow the backup even if primary logging is disabled.
            Bool5 oldDisable = c4->logDisable ;
            c4->logDisable = 0 ;
            c4trans->transFile = &c4->server->transFile ;
            // AS Aug 15/01 - the transfile status was not getting properly initialized if there was no primary log file
            if ( c4trans->transFile->status == 0 )
               c4trans->transFile->status = tran4notRollbackOrCommit ;

            if ( doCreate == 0 )
               rc = tran4fileLowOpen( &c4->server->transFile.backup, backupLogName ) ;
            else
               rc = tran4fileLowCreate( &c4->server->transFile.backup, backupLogName ) ;
            c4->logDisable = oldDisable ;
         }

         if ( rc == 0 )
            tran4fileLowLenSet( &c4->server->transFile.backup, file4lenLow( &(c4->server->transFile.backup.file )) ) ;

         return rc ;
      }
   #endif



   #ifndef S4CLIENT
      int tran4addUser( TRAN4 *trans, const long clientId, const char *charId, const unsigned short int lenIn )
      {
         // Every client has a user-id associated with the transaction file.
         int rc ;
         short int netIdLen ;
         char *netId ;
         static char defaultUser[] = "PUBLIC" ;

         unsigned short int len = lenIn ;
         CODE4 *c4 = trans->c4trans->c4 ;

         #ifdef S4OLEDEBUG_PRINT
            log5( "attempting to tran4addUser(): charId = " ) ;
            log5( charId ) ;
            log5( " current trans enabled status is: " ) ;
            char clientIdBuf[11] ;
            c4ltoa45( trans->c4trans->enabled, clientIdBuf, 10 ) ;
            log5( clientIdBuf ) ;
            clientIdBuf[10] = 0 ;
            log5( " and current trans status is: " ) ;
            c4ltoa45( code4tranStatus( c4 ), clientIdBuf, 10 ) ;
            clientIdBuf[10] = 0 ;
            log5( clientIdBuf ) ;
            log5( "\r\n" ) ;
         #endif

         // AS Sept. 12/02 - always go ahead with this assignation procedure since we use this info
         // on determine lock info (at least on server), so we otherwise don't record it if transactions
         // are disabled (t4lock3.c)
         if ( len > sizeof( trans->userId ) )
            len = sizeof( trans->userId ) - 1 ;
         memcpy( trans->userId, charId, len ) ;
         trans->userId[len] = 0 ;

         if ( trans->c4trans->enabled == 1 && code4tranStatus( c4 ) != r4off )
         {
            // AS Apr 28/03 - made trans-shared a run-time switch
            if ( c4->transShared == 1 )
            {
               #ifdef S4OFF_MULTI
                  // AS 11/22/99 user id # moved to TRAN4FILE structure
                  trans->c4trans->transFile->userIdNo = 1 ;   /* only one user */
               #else /* S4OFF_MULTI else */
                  #ifdef S4OLEDEBUG_PRINT
                     log5( "  clientId set at: " ) ;
                     c4ltoa45( clientId, clientIdBuf, 10 ) ;
                     clientIdBuf[10] = 0 ;
                     log5( clientIdBuf ) ;
                     log5( "\r\n" ) ;
                  #endif
                  // AS Mar 21/03 Need to always get the id manually...for sure for ODBC Server
                  // if ( clientId == 0L )  /* need to manually get the id */
                  // {
                  // AS for the ODBC_BUILD, we may already have registered the userIdNo, in which case
                  // this next part is not needed.
                  // #ifdef S4ODBC_BUILD
                  #ifdef S4SERVER
                     if ( trans->c4trans->transFile->primary.userIdNo == 0 )
                  #else
                     if ( clientId == 0L )  /* need to manually get the id */
                  #endif
                     {
                        for ( int i = 0 ;; i++ )
                        {
                           if ( i >= TRAN4MAX_USERS )
                              return error4( c4, e4max, E83816 ) ;
                           int oldNumAttempts = c4->lockAttempts ;
                           c4->lockAttempts = 1 ;
                           #ifdef S4SERVER
                              rc = file4lockInternal( &trans->c4trans->transFile->primary.file, TRAN4LOCK_USERS + i + 1, 0, 1, 0 ) ;
                           #else
                              rc = file4lockInternal( &trans->c4trans->transFile->file, TRAN4LOCK_USERS + i + 1, 0, 1, 0 ) ;
                           #endif
                           c4->lockAttempts = oldNumAttempts ;
                           if ( rc == 0 )
                           {
                              #ifdef S4OLEDEBUG_PRINT
                                 log5( "  userId being assigned as: " ) ;
                                 c4ltoa45( i+1, clientIdBuf, 10 ) ;
                                 clientIdBuf[10] = 0 ;
                                 log5( clientIdBuf ) ;
                                 log5( "\r\n" ) ;
                              #endif
                              // AS 11/22/99 user id # moved to TRAN4FILE structure
                              #ifdef S4SERVER
                                 trans->c4trans->transFile->primary.userIdNo = i + 1 ;
                              #else
                                 trans->c4trans->transFile->userIdNo = i + 1 ;
                              #endif
                              break ;
                           }
                        }
                     }
               #endif /* S4OFF_MULTI else */
            }
            #ifdef S4SERVER
               netId = 0 ;
               if ( netId == 0 )
                  netIdLen = 0 ;
               else
                  netIdLen = strlen( netId ) ;
            #else
               netId = (char *)0 ;
               netIdLen = 0 ;
            #endif
            /* AS 09/09/98 because the time function is expensive, only call when
               starting transactions.  The same time is uses for the entire transaction
            */
            tran4getTime( c4 ) ;
            rc = tran4set( trans, trans->currentTranStatus, -1L, clientId, TRAN4INIT, len + netIdLen + sizeof( len ) + sizeof( netIdLen), 0L, 0L ) ;
            if ( rc < 0 )
               return rc ;
            if ( tran4putData( trans, (void *)&netIdLen, sizeof( netIdLen ) ) == e4memory )
               return e4memory ;
            if ( netIdLen != 0 )
               if ( tran4putData( trans, (void *)netId, (unsigned int)netIdLen ) == e4memory )
                  return e4memory ;
            if ( tran4putData( trans, (void *)&len, sizeof( len ) ) == e4memory )
               return e4memory ;
            if ( len == 0 )  /* empty char id */
            {
               if ( tran4putData( trans, (void *)defaultUser, strlen( defaultUser ) ) == e4memory )
                  return e4memory ;
            }
            else
               if ( tran4putData( trans, (void *)charId, len ) == e4memory )
                  return e4memory ;
            if ( tran4lowAppend( trans, 0, 0 ) != 0 )
               return e4transAppend ;
         }
         else
            return e4trans ;  /* must return error to client so it is known that code4tranInit failed */

         return 0 ;
      }
   #endif /* S4CLIENT */



   #ifdef S4SERVER
      // AS July 9/02 - New function for populating an in-progress server version of the log-file (for log4compress)
      int code4transFilePopulate( CODE4TRANS *c4trans )
      {
         // we need to update the log file to contain all the user-id info as well as all open tables
         CODE4 *c4 = c4trans->c4 ;
         SERVER4 *server = c4->server ;

         // first log the server id's info
         server4transInitServerId( c4, server ) ;

         list4mutexWait( &server->clients ) ;
         SERVER4CLIENT *clientOn ;
         SERVER4CLIENT *saveClient = c4->currentClient ;
         for ( clientOn = 0 ;; )
         {
            clientOn = (SERVER4CLIENT *)l4next( &server->clients.list, clientOn ) ;
            if ( clientOn == 0 )
               break ;
            // they should all be disabled at this point anyway...
            assert5( clientOn->transEnabled == 0 ) ;
            if ( clientOn->transEnabled == 0 )
            {
               c4->currentClient = clientOn ;  // for data open in particular, to ensure correct id's entered
               int rc = tran4addUser( &clientOn->trans, (unsigned long)clientOn->id, clientOn->account.accountId, strlen( clientOn->account.accountId )) ;
               if ( rc >= 0 )
                  clientOn->transEnabled = 1 ;
               // for each data file, log it...
               for ( DATA4 *dataOn = 0 ;; )
               {
                  dataOn = (DATA4 *)l4next( tran4dataList( &clientOn->trans ), dataOn ) ;
                  if ( dataOn == 0 )
                     break ;
                  // they should all be disabled at this point anyway...
                  assert5( dataOn->openWasLogged == 0 ) ;
                  // AS July 22/02 - Don't log it now because we don't actually know the clientId associated
                  // with it (that value is sent by the client at run time)
                  if ( dataOn->openWasLogged == 0 )
                  {
                     d4openConcludeSetupTransactions( dataOn ) ;
                     assert5( dataOn->openWasLogged == 1 ) ;  // should be logged now
                  }
               }
            }
         }
         c4->currentClient = saveClient ;
         list4mutexRelease( &server->clients ) ;
         return 0 ;
      }
   #endif /* S4SERVER  */
#endif /* #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) */



#ifndef S4OFF_MULTI
   void S4FUNCTION tran4freeLocks( CODE4 *c4, SINGLE4DISTANT *toFree )
   {
      LOCK4GROUP *lock ;

      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E91018 ) ;
            return ;
         }
      #endif

      for ( ;; )
      {
         lock = (LOCK4GROUP *)single4distantToItem( toFree ) ;
         if ( lock == 0 )
            return ;
         single4distantPop( toFree ) ;
         mem4free( c4->lockGroupMemory, lock ) ;
      }
   }



   #ifndef S4CLIENT
      static void tran4unlock( SINGLE4DISTANT *toAdd, SINGLE4DISTANT *toUnlock )
      {
         LOCK4GROUP *lock ;

         for ( ;; )
         {
            lock = (LOCK4GROUP *)single4distantToItem( toUnlock ) ;
            if ( lock == 0 )
               return ;
            single4distantPop( toUnlock ) ;
            single4add( single4distantToSingle( toAdd ), &lock->link ) ;
            lock4groupUnlock( lock ) ;
         }
      }
   #endif
#endif /* SOFF_MULTI */

#ifndef S4SERVER
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4FUNCTION code4lock( CODE4 *c4 )
   {
      #ifdef S4OFF_MULTI
         return 0 ;
      #else
         LOCK4GROUP *lock ;
         SINGLE4DISTANT locked ;
         int rc = 0 ;
         CODE4TRANS *c4trans ;
         #ifdef S4CLIENT
            short numLocks ;
            int outRc = 0 ;
            CONNECTION4 *connection ;
            CONNECTION4LOCK_GROUP_INFO_IN info ;
            LOCK4ID lId ;
            SINGLE4 *single ;
         #else
            SINGLE4 locks ;
            int saveErr, count, saveUnlockAuto ;
            TRAN4 *trans ;
         #endif

         #ifdef E4ANALYZE
            if ( ( rc = code4verify( c4, 1 ) ) < 0 )
               return rc ;
         #endif

         c4trans = &c4->c4trans ;
         #ifdef S4CLIENT
            single = single4initIterate( &c4trans->trans.locks ) ;
            for( numLocks = 0 ;; numLocks++ )
            {
               if ( single == 0 )
                  break ;
               single = single4next( single ) ;
            }
            if ( numLocks > 0 )
            {
               lock = (LOCK4GROUP *)single4initIterate( &c4trans->trans.locks ) ;
               connection = lock->data->dataFile->connection ;
               info.numLocks = htons5(numLocks) ;

               connection4assign( connection, CON4LOCK_GROUP, 0L, 0L ) ;
               connection4addData( connection, &info, sizeof( info ), NULL ) ;
               /* AS 01/09/97, since now have STREAM4UNLOCK_DATA handles, don't need this code */
                 /* must perform and register data unlock */
                 /* unlockData = ( code4unlockAuto( c4 ) == LOCK4DATA ) ; */
               for( ; numLocks > 0 ; numLocks-- )
               {
                  #ifdef E4ANALYZE
                     if ( lock == 0 )
                        return error4stack( c4, e4struct, E91008 ) ;
                  #endif
                  lId.type = htons5(lock->id.type) ;
                  lId.recNum = htonl5(lock->id.recNum) ;
                  // AS Apr 15/03 - support for new lockId for shared clone locking
                  lId.lockId = htonl5(lock->id.lockId) ;
                  lId.serverId = htonl5(lock->id.serverId) ;
                  connection4addData( connection, &lId, sizeof( LOCK4ID ), NULL ) ;
                  lock = (LOCK4GROUP *)single4next( &lock->link ) ;
               }
               outRc = connection4repeat( connection ) ;
               if ( outRc < 0 )
                  return connection4error( connection, c4, outRc, E91008 ) ;

               if ( outRc == r4locked )
                  return outRc ;

               single4distantInitIterate( &locked, &c4trans->trans.locks ) ;
               for( ;; )  /* now free lock memory, and record locks if required */
               {
                  lock = (LOCK4GROUP *)single4initIterate( &c4trans->trans.locks ) ;
                  if ( lock == 0 )
                     break ;
                  if ( outRc == 0 )   /* record lock */
                  {
                     switch( lock->id.type )
                     {
                        case LOCK4APPEND:
                           // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
                           lock->data->dataFile->appendLockServerId = data4serverId( lock->data ) ;
                           lock->data->dataFile->appendLockLockId = data4lockId( lock->data ) ;
                           break ;
                        case LOCK4FILE:
                           lock->data->dataFile->fileLockServerId = data4serverId( lock->data ) ;
                           lock->data->dataFile->fileLockLockId = data4lockId( lock->data ) ;
                           break ;
                        case LOCK4ALL:
                           lock->data->dataFile->fileLockServerId = data4serverId( lock->data ) ;
                           lock->data->dataFile->fileLockLockId = data4lockId( lock->data ) ;
                           break ;
                        case LOCK4RECORD:
                           d4localLockSet( lock->data, lock->id.recNum ) ;
                           break ;
                        #ifdef E4ANALYZE
                           default:
                              return error4( c4, e4lock, E81505 ) ;
                        #endif
                     }
                  }
                  assert5( lock == (LOCK4GROUP *)single4distantToItem( &locked ) ) ;
                  single4distantPop( &locked ) ;
                  mem4free( c4->lockGroupMemory, lock ) ;
               }
            }
            return outRc ;
         #else
            trans = &c4->c4trans.trans ;
            saveUnlockAuto = code4unlockAuto( c4 ) ;

            if ( saveUnlockAuto == 1 )
            {
               rc = code4unlockDo( tran4dataList( trans ) ) ;
               if ( rc < 0 )
                  return error4stack( c4, rc, E91008 ) ;
            }

            code4unlockAutoSet( c4, 0 ) ;

            single4init( &locks ) ;

            single4distantInitIterate( &locked, &locks ) ;
            for ( lock = 0, count = -1 ;; )
            {
               if ( lock == 0 )
               {
                  lock = (LOCK4GROUP *)single4next( &trans->locks ) ;
                  single4distantInitIterate( &trans->toLock, &trans->locks ) ;
                  count++ ;

                  if ( lock == 0 )
                  {
                     tran4freeLocks( c4, &locked ) ;
                     code4unlockAutoSet( c4, saveUnlockAuto ) ;
                     return 0 ;
                  }

                  if ( c4->lockAttemptsSingle != WAIT4EVER )
                     if ( count >= c4->lockAttemptsSingle )  /* timed out */
                     {
                        tran4unlock( &trans->toLock, &locked ) ;
                        code4unlockAutoSet( c4, saveUnlockAuto ) ;
                        return r4locked ;
                     }
               }

               switch( lock4groupLock( lock ) )
               {
                  case r4success:
                     assert5( lock == (LOCK4GROUP *)single4distantToItem( &trans->toLock ) ) ; /* ensure the lock we remove is the current lock */
                     single4distantPop( &trans->toLock ) ;
                     single4add( single4distantToSingle( &locked ), &lock->link ) ;
                     lock = (LOCK4GROUP *)single4distantToItem( &trans->toLock ) ;
                     break ;
                  case r4locked:
                     if ( c4->lockAttemptsSingle == 1 )  /* we only want one check, we failed, so set lock to 0 to exit out */
                        lock = 0 ;
                     else
                     {
                        single4distantInitIterate( &trans->toLock, single4distantToItem( &trans->toLock ) ) ;
                        lock = (LOCK4GROUP *)single4distantToItem( &trans->toLock ) ;
                     }
                     break ;
                  default:
                     saveErr = error4set( c4, 0 ) ;
                     tran4unlock( &trans->toLock, &locked ) ;
                     error4set( c4, saveErr ) ;
                     code4unlockAutoSet( c4, saveUnlockAuto ) ;
                     return -1 ;
               }
            }
            /* should never get here - just a double check - reqd for MSC 7.0 */
            return error4( c4, e4result, E91008 ) ;
         #endif  /* S4CLIENT */
      #endif  /* S4OFF_MULTI */
   }
#endif  /* not S4SERVER */



#ifdef S4SERVER
   int tran4closeAll( TRAN4 *trans )
   {
      DATA4 *dataOn, *dataNext ;
      int rc ;
      LIST4 *list ;

      #ifdef E4PARM_LOW
         if ( trans == 0 )
            return error4( 0, e4parm_null, E93801 ) ;
      #endif

      rc = 0 ;
      list = tran4dataList( trans ) ;
      #ifdef E4ANALYZE
         if ( list == 0 )
            return error4( trans->c4trans->c4, e4struct, E93801 ) ;
      #endif
      for ( dataNext = (DATA4 *)l4first( list ) ;; )
      {
         dataOn = dataNext ;
         if ( !dataOn )
            break ;
         dataNext = (DATA4 *)l4next( list, dataNext ) ;

         if ( d4close( dataOn ) < 0 )
            rc = -1 ;
      }

      if ( error4code( trans->c4trans->c4 ) < 0 )
         return -1 ;

      return rc ;
   }
#endif  /* S4SERVER */

#ifdef E4ANALYZE
   static int code4transVerify( CODE4TRANS *c4trans, int subs )
   {
      int rc ;

      if ( c4trans == 0 )
         return error4( 0, e4parm_null, E93832 ) ;

      if ( subs == 1 )
      {
         if ( ( rc = code4verify( c4trans->c4, 1 ) ) < 0 )
            return rc ;
      }
      else
         if ( c4trans->c4 == 0 )
            return error4( 0, e4struct, E93832 ) ;

      #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
         if ( c4trans->enabled == 1 )
            if ( c4trans->transFile == 0 && c4trans->c4->logDisable != log4disabled )
               return error4( c4trans->c4, e4struct, E93832 ) ;
      #endif

      return 0 ;
   }



   int tran4verify( TRAN4 *t4, int subs )
   {
      int rc ;

      if ( t4 == 0 )
         return error4( 0, e4parm_null, E93801 ) ;

      if ( t4->c4trans == 0 )
         return error4( 0, e4struct, E93801 ) ;

      #ifndef S4OFF_TRAN
         CODE4 *c4 = t4->c4trans->c4 ;

         // AS May 27/02 - in some cases c4 was null
         if ( c4 == 0 )
            return error4( c4, e4struct, E93801 ) ;

         if ( ( c4->tranDataLen != 0 && c4->tranData == 0 ) || ( c4->tranDataLen == 0 && c4->tranData != 0 ) )
            return error4( c4, e4struct, E93801 ) ;

         if ( ( t4->dataPos != 0 && c4->tranDataLen == 0 ) )
            return error4( c4, e4struct, E93801 ) ;
      #endif

      if ( subs == 1 )
         if ( ( rc = code4transVerify( t4->c4trans, 1 ) ) < 0 )
            return rc ;

      return 0 ;
   }
#endif  /* E4ANALYZE */



int code4tranInitLow( TRAN4 *t4, CODE4TRANS *c4trans )
{
   // AS 03/23/01 - this initializes various values, not all of which
   // are used in transaction processing - some are always used.
   #ifdef E4PARM_LOW
      if ( t4 == 0 || c4trans == 0 )
         return error4( 0, e4parm_null, E93834 ) ;
   #endif

   t4->c4trans = c4trans ;

   #ifndef S4OFF_WRITE
      t4->transId = -1 ;
      t4->dataPos = 0 ;
      t4->pos = (unsigned long)-1 ;
   #endif

   #ifdef S4CLIENT
      t4->dataIdCount = 1 ;
   #endif
   #ifdef S4SERVER
      t4->unlockAuto = 1 ;
   #endif

   #ifndef S4OFF_WRITE
      t4->currentTranStatus = r4inactive ;
   #endif

   tran4dataListSet( t4, &(t4->localDataList) ) ;

   #ifdef E4ANALYZE
      return tran4verify( t4, 0 ) ;
   #else
      return 0 ;
   #endif
}


#if !defined( S4OFF_TRAN ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) && !defined( S4UTILS )
static void code4transInitUndoMarkShutdown( CODE4TRANS *c4trans )
{
   // marks the transaction log file as validly shut dwon

   if ( c4trans->transFile->status == tran4notRollbackOrCommit )
   {
      LOG4HEADER header ;
      memset( &header, 0, sizeof( header ) ) ;
      header.type = TRAN4SHUTDOWN ;
      #ifdef S4WINCE
         S4LONG tempLong = TRAN4VERSION_NUM ;
         memcpy( header.serverDataId, &tempLong, sizeof(S4LONG) ) ;
      #else
         header.serverDataId = TRAN4VERSION_NUM ;
      #endif
      tran4fileAppend( c4trans->transFile, &header, "\0", 1 ) ;
   }
}
#endif



#if !defined( S4OFF_TRAN ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) && !defined( S4UTILS )
void tran4fileLowMarkBackupClose( TRAN4FILE_LOW *t4 )
{
   // marks the backup transaction log file as validly shut down as part of the backup/recovery procedure

   LOG4HEADER header ;
   memset( &header, 0, sizeof( header ) ) ;
   header.type = TRAN4CLOSE_BACKUP ;
   #ifdef S4WINCE
      S4LONG tempLong = TRAN4VERSION_NUM ;
      memcpy( header.serverDataId, &tempLong, sizeof(S4LONG) ) ;
   #else
      header.serverDataId = TRAN4VERSION_NUM ;
   #endif
   tran4fileLowAppend( t4, &header, "\0", 1 ) ;
}
#endif



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION code4transInitUndo( CODE4TRANS *c4trans )
{
   #ifdef E4PARM_LOW
      if ( c4trans == 0 )
         return error4( 0, e4parm_null, E93835 ) ;
   #endif

   #if defined( S4OFF_WRITE ) || defined( S4CLIENT ) || defined( S4OFF_TRAN )
      return 0 ;
   #else
      CODE4 *c4 = c4trans->c4 ;

      #ifndef S4SERVER
         if ( c4 != 0 )
         {
            if ( c4->transFileName != 0 )
            {
               u4free( c4->transFileName ) ;
               c4->transFileName = 0 ;
            }
         }
      #endif

      int oldError = error4set( c4trans->c4, 0 ) ;
      int rc = 0 ;
      if ( c4trans->transFile != 0 )
      {
         #ifndef S4UTILS
            // AS Apr 28/03 - made trans-shared a run-time switch
            if ( c4->transShared == 1 )
            {
               #ifndef S4OFF_MULTI
                  // AS Mar 21/03 - record sooner
                  int oldNumAttempts = c4->lockAttempts ;
               #endif

               #ifdef S4STAND_ALONE
                  code4tranInitUndoLow( &c4trans->trans, 0L ) ;
                  // if the current transaction status is not inactive, it means that we
                  // are shttuing down in an invalid state.  In that case, do not properly
                  // mark the transaction file as validly shut down...
                  if ( c4trans->trans.currentTranStatus == r4inactive )
               #endif
                  {
                     FILE4 *file ;
                     #ifdef S4SERVER
                        file = &c4trans->transFile->primary.file ;
                     #else
                        file = &c4trans->transFile->file ;
                     #endif

                     #ifdef S4OFF_MULTI
                        code4transInitUndoMarkShutdown( c4trans ) ;
                     #else
                        // see if we can lock the entire transaction file.  If we can, then
                        // we must be the only user using it, so mark the file as shutdown...

                        // AS Mar 21/03 - There is a chance that we do not have the file locked, in particular on start-up
                        // in server case if we want to delete the log file.  We will know this is the case if c4trans->transFile->primary.userIdNo == 0
                        #ifdef S4SERVER
                           if ( c4trans->transFile->primary.userIdNo != 0 )
                        #endif
                        {
                           // AS 11/22/99 user id # moved to TRAN4FILE structure
                           #ifdef S4SERVER
                              file4unlockInternal( file, TRAN4LOCK_USERS + c4trans->transFile->primary.userIdNo, 0, 1, 0 ) ;
                           #else
                              file4unlockInternal( file, TRAN4LOCK_USERS + c4trans->transFile->userIdNo, 0, 1, 0 ) ;
                           #endif
                           error4set( c4, 0 ) ;     /* LY 2002/08/08 : if above unlock fails, next lock returns without actual lock attempt */
                           c4->lockAttempts = 1 ;
                        }
                        if ( file4lockInternal( file, TRAN4LOCK_USERS, 0, TRAN4MAX_USERS, 0 ) != r4locked ) /* last user, so shutdown */
                        {
                           code4transInitUndoMarkShutdown( c4trans ) ;
                           file4unlockInternal( file, TRAN4LOCK_USERS, 0, TRAN4MAX_USERS, 0 ) ;
                        }
                     #endif /* S4OFF_MULTI else */
                  }

               #ifndef S4OFF_MULTI
                  c4->lockAttempts = oldNumAttempts ;
               #endif
            }
            else
               code4transInitUndoMarkShutdown( c4trans ) ;
            #ifndef S4OFF_MULTI
               short i = -1;
               while ( (1UL << ++i ) <= c4trans->transFile->fileLocks )
                  if ( c4trans->transFile->fileLocks & ( 1UL << i ) )
                     code4tranUnlockTransactions( c4trans, TRAN4LOCK_BASE + i ) ;
            #endif
         #endif /* S4UTILS */

         rc = tran4fileClose( c4trans->transFile ) ;
         c4trans->transFile = 0 ;
      }
      c4trans->enabled = 0 ;
      error4set( c4trans->c4, oldError ) ;
      return rc ;
   #endif /* ! defined( S4OFF_WRITE ) || defined( S4CLIENT ) || defined( S4OFF_TRAN ) */
}



#ifdef S4SERVER
   int code4transInit( CODE4TRANS *c4trans, CODE4 *c4 )
   {
      #ifdef E4PARM_LOW
         if ( c4trans == 0 || c4 == 0 )
            return error4( 0, e4parm_null, E93836 ) ;
      #endif

      #if !defined( S4OFF_TRAN ) && defined( E4ANALYZE )
         if ( c4trans->enabled != 0 )
            return error4( 0, e4struct, E93836 ) ;
      #endif

      memset( c4trans, 0, sizeof( c4trans ) ) ;

      #ifdef E4ANALYZE
         if ( c4->debugInt != E4DEBUG_INT )   /* not initialized */
            return error4( 0, e4struct, E81301 ) ;
      #endif
      c4trans->c4 = c4 ;

      #ifdef E4ANALYZE
         return code4transVerify( c4trans, 0 ) ;
      #else
         return 0 ;
      #endif
   }
#endif /* S4SERVER */



#ifndef S4CLIENT
   #ifndef S4OFF_MULTI
      #if !defined( S4OFF_TRAN )
         // AS Oct 31/02 - this function is available in S4UTILS, it is called with the combined
         // procesing application only.  Not sure if required, and is not unlocked locks are removed on close.
         int S4FUNCTION code4tranLockTransactions( CODE4TRANS *c4trans, long lockByte )
         {
            /* ensure only one process is accessing the transaction file */
            // AS July 12/01 Allow the primary log file to be disabled (backup logs still left intact if in use)
            // in that case, no locking is done...
            #ifdef S4SERVER
               #ifdef E4PARM_LOW
                   if ( c4trans == 0 )
                     return error4( 0, e4parmNull, E91008 ) ;
                   if ( c4trans->transFile == 0 )
                     return error4( c4trans->c4, e4parm, E91008 ) ;
               #endif
               if ( c4trans->transFile->primary.isDisabled == log4disabled )
                  return 0 ;
            #endif

            int rc, oldAttempts ;

            #ifdef E4ANALYZE
               if ( code4transVerify( c4trans, 1 ) < 0 )
                  return 0 ;
            #endif

            CODE4 *c4 = c4trans->c4 ;
            // AS Apr 28/03 - made trans-shared a run-time switch
            #if defined( S4TESTING )
               if ( c4->transShared == 1 )
               {
                  // AS May 20/03 - update for compile
                  #if ( defined( S4TESTING ) && defined( S4STAND_ALONE ) && ! defined( S4OFF_TRAN ) ) || ( defined( S4SERVER ) && (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )))
                     if ( c4trans->c4->doTransLocking == 0 )
                  #endif
                     {
                        #ifdef S4SERVER
                           tran4fileLowLenUpdate( &c4trans->transFile->primary ) ;
                        #else
                           tran4fileLowLenUpdate( c4trans->transFile ) ;
                        #endif
                        return 0 ;
                     }
               }
            #endif

            if ( lockByte < TRAN4LOCK_BASE )
               return e4parm ;
            if ( c4trans->transFile->fileLocks & ( 1UL << ( lockByte - TRAN4LOCK_BASE ) ) )
               return 0 ;
            oldAttempts = c4trans->c4->lockAttempts ;
            c4trans->c4->lockAttempts = WAIT4EVER ;  // try forever
            #ifdef S4SERVER
               // only lock on the primary file
               if ( c4trans->transFile->primary.file.lowAccessMode == OPEN4DENY_RW )  // don't lock exclusive file
                  rc = 0 ;
               else
                  rc = file4lockInternal( &c4trans->transFile->primary.file, lockByte, 0, 1L, 0 ) ;
            #else
               if ( c4trans->transFile->file.lowAccessMode == OPEN4DENY_RW )  // don't lock exclusive file
                  rc = 0 ;
               else
                  rc = file4lockInternal( &c4trans->transFile->file, lockByte, 0, 1L, 0 ) ;
            #endif
            c4trans->c4->lockAttempts = oldAttempts ;
            if ( rc == 0 )
               c4trans->transFile->fileLocks |= ( 1UL << ( lockByte - TRAN4LOCK_BASE ) ) ;
            /* track file length internally during a transaction to avoid the
               expensive system calls to determine the file length. */
            #ifdef S4SERVER
               tran4fileLowLenUpdate( &c4trans->transFile->primary ) ;
            #else
               tran4fileLowLenUpdate( c4trans->transFile ) ;
            #endif
            return rc ;
         }


         #ifndef S4UTILS
            int S4FUNCTION code4tranUnlockTransactions( CODE4TRANS *c4trans, long lockByte )
            {
               int rc ;

               #ifdef E4ANALYZE
                  if ( code4transVerify( c4trans, 1 ) < 0 )
                     return 0 ;
               #endif

               // AS Apr 28/03 - made trans-shared a run-time switch
               #if defined( S4TESTING )
                  CODE4 *c4 = c4trans->c4 ;

                  if ( c4->transShared == 1 )
                  {
                     // AS May 20/03 - update for compile
                     #if ( defined( S4TESTING ) && defined( S4STAND_ALONE ) && ! defined( S4OFF_TRAN ) ) || ( defined( S4SERVER ) && (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )))
                        if ( c4trans->c4->doTransLocking == 0 )
                     #endif
                           return 0 ;
                  }
               #endif

               /*   c4transSetFileLen( c4trans->transFile, -1 ) ; -- shouldn't be needed */
               if ( lockByte < TRAN4LOCK_BASE )
                  return e4parm ;
               if ( !(c4trans->transFile->fileLocks & ( 1UL << ( lockByte - TRAN4LOCK_BASE ) ) ) )
                  return e4unlock ;
               // AS Apr 4/03 - Was not checking exclusivity
               #ifdef S4SERVER
                  if ( c4trans->transFile->primary.file.lowAccessMode == OPEN4DENY_RW )  // don't lock exclusive file
               #else
                  if ( c4trans->transFile->file.lowAccessMode == OPEN4DENY_RW )  // don't lock exclusive file
               #endif
                  rc = 0 ;
               else
               {
                  #ifdef S4SERVER
                     // only lock on the primary file
                     rc = file4unlockInternal( &c4trans->transFile->primary.file, lockByte, 0, 1L, 0 ) ;
                  #else
                     rc = file4unlockInternal( &c4trans->transFile->file, lockByte, 0, 1L, 0 ) ;
                  #endif
               }
               if ( rc == 0 )
               {
                  c4trans->transFile->fileLocks &= !( 1UL << ( lockByte - TRAN4LOCK_BASE ) ) ;
               }
               return rc ;
            }
         #endif
      #endif /* #if !defined( S4OFF_TRAN )*/



      int tran4lock( TRAN4 *trans )
      {
         LOCK4GROUP *lock ;
         SINGLE4 locks ;
         SINGLE4DISTANT locked ;
         int saveErr, rc, saveUnlockAuto ;
         CODE4 *c4 ;

         #ifdef E4ANALYZE
            if ( ( rc = tran4verify( trans, 1 ) ) < 0 )
               return rc ;
         #endif

         c4 = trans->c4trans->c4 ;

         saveUnlockAuto = code4unlockAuto( c4 ) ;

         if ( saveUnlockAuto == 1 )
         {
            rc = code4unlockDo( tran4dataList( trans ) ) ;
            if ( rc < 0 )
               return error4stack( c4, rc, 0L ) ;
         }

         code4unlockAutoSet( c4, 0 ) ;

         single4init( &locks ) ;
         single4distantInitIterate( &locked, &locks ) ;
         single4distantInitIterate( &trans->toLock, &trans->locks ) ;
         lock = (LOCK4GROUP *)single4next( &trans->locks ) ;
         for ( ;; )
         {
            if ( lock == 0 )
            {
               tran4freeLocks( c4, &locked ) ;
               code4unlockAutoSet( c4, saveUnlockAuto ) ;
               #ifdef S4CLIENT
                  return saveRc ;
               #else
                  return 0 ;
               #endif
            }

            switch( lock4groupLock( lock ) )
            {
               case r4success:
                  assert5( lock == (LOCK4GROUP *)single4distantToItem( &trans->toLock ) ) ;  /* ensure we are removing the correct lock */
                  single4distantPop( &trans->toLock ) ;
                  single4add( &locks, &lock->link ) ;
                  lock = (LOCK4GROUP *)single4distantToItem( &trans->toLock ) ;
                  break ;
               case r4locked:
                  #ifdef S4SERVER
                     tran4unlock( &trans->toLock, &locked ) ;
                     return r4locked ;
                  #else
                     single4distantInitIterate( &trans->toLock, single4distantToItem( &trans->toLock ) ) ;
                     lock = (LOCK4GROUP *)single4distantToItem( &trans->toLock ) ;
                     break ;
                  #endif
               default:
                  saveErr = error4set( c4, 0 ) ;
                  tran4unlock( &trans->toLock, &locked ) ;
                  code4unlockAutoSet( c4, saveUnlockAuto ) ;
                  error4set( c4, saveErr ) ;
                  return -1 ;
            }
         }
         /* should never get here - just a double check - reqd for MSC 7.0 */
         return error4( c4, e4result, E91008 ) ;
      }
   #endif  /* S4OFF_MULTI */
#endif  /* !S4CLIENT */



#ifndef S4CLIENT
   static DATA4 *tran4dataFull( TRAN4 *trans, const long serverId, const long clientId )
   {
      #ifdef S4OFF_TRAN
         return 0 ;
      #else
         DATA4 *data ;
         LIST4 *oldList ;

         data = tran4data( trans, serverId, clientId ) ;
         if ( data != 0 )
            return data ;

         oldList = tran4dataList( trans ) ;
         tran4dataListSet( trans, &trans->closedDataFiles ) ;
         data = tran4data( trans, serverId, clientId ) ;
         tran4dataListSet( trans, oldList ) ;

         return data ;
      #endif
   }
#endif



#ifdef S4SERVER
   /* to get a DATA4 based on id instead of TRAN4 */
   DATA4 *code4idData( CODE4 *c4, const long serverId, const long clientId )
   {

      /* reserve the client list during this process */
      server4clientListReserve( c4->server ) ;

      DATA4 *foundData = 0 ;
      for( SERVER4CLIENT *client = 0 ;; )
      {
         client = server4clientGetNext( c4->server, client ) ;
         if ( client == 0 )
            break ;
         DATA4 *data = tran4data( &client->trans, serverId, clientId ) ;
         if ( data != 0 )
         {
            foundData = data ;
            break ;
         }
      }

      server4clientListRelease( c4->server ) ;
      return foundData ;
   }
#endif /* S4SERVER */



#ifndef S4CLIENT
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   /* are there any active transactions which conflict with the data4? */
   #ifdef E4ANALYZE
      int S4FUNCTION tran4active( CODE4 *c4, DATA4 *data )
   #else
      int tran4active( CODE4 *c4, DATA4 *data )
   #endif
   {
      #ifdef S4OFF_TRAN
         return 0 ;
      #else
         // AS Apr 29/03 - transcations are run-time in odbc now
         // AS Jun 20/03 - was checking wrong flag
         #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
            if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
               return 0 ;
         #endif
         if ( code4transEnabled( c4 ) )
            if ( code4trans( c4 )->currentTranStatus == r4active )
               return e4transViolation ;

         #ifdef S4OFF_MULTI
            return 0 ;
         #else
            #ifdef S4STAND_ALONE
               if ( data->logVal == LOG4TRANS )   /* not logging this datafile, and not a transaction in progress... */
                  return 0 ;
               if ( d4lockTestFile( data ) == 1 )
                  return 0 ;
               if ( data->dataFile->file.lowAccessMode != OPEN4DENY_NONE )
                  return 0 ;
               /* in stand-alone, can't check active transactions, so just deny */
               return error4( c4, e4transViolation, E81504 ) ;
            #endif

            #ifdef S4SERVER
               if ( data->accessMode != OPEN4DENY_NONE )  /* if denying others write access, then can proceed */
                  return 0 ;
               // AS Apr 15/03 - support for new lockId for shared clone locking
               if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4write ) == 1 )
                  return 0 ;
               /* if file not ultimately opened exclusively, may be problems */
               if ( data->dataFile->file.lowAccessMode == OPEN4DENY_NONE )
                  return error4( c4, e4transViolation, E81504 ) ;

               /* reserve the client list during this process */
               server4clientListReserve( c4->server ) ;

               SERVER4CLIENT *client ;
               for ( client = 0 ;; )
               {
                  client = server4clientGetNext( c4->server, client ) ;
                  if ( client == 0 )
                     break ;
                  if ( client->trans.c4trans->enabled )
                     if ( client->trans.currentTranStatus == r4active )
                     {
                        /* check if that user has a data4 using the same datafile */
                        DATA4 *dataLoop ;
                        for( dataLoop = 0 ;; )
                        {
                           dataLoop = (DATA4 *)l4next( client->trans.dataList, dataLoop ) ;
                           if ( dataLoop == 0 )
                              break ;
                           if ( dataLoop->readOnly == 0 )
                              if ( dataLoop->dataFile == data->dataFile )
                              {
                                 server4clientListRelease( c4->server ) ;
                                 return error4( c4, e4transViolation, E81504 ) ;
                              }
                        }
                     }
               }
               server4clientListRelease( c4->server ) ;
               return 0 ;
            #endif /* S4SERVER */
         #endif /* S4OFF_MULTI else */
      #endif  /* S4OFF_TRAN */
   }
#endif /* !S4CLIENT */


void S4FUNCTION code4lockClear( CODE4 *c4 )
{
   #ifndef S4OFF_MULTI
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E91018 ) ;
            return ;
         }
      #endif
      #ifdef S4SERVER
         single4distantInitIterate( &(c4->currentClient->trans.toLock), &(c4->currentClient->trans.locks ) ) ;
         tran4freeLocks( c4, &(c4->currentClient->trans.toLock) ) ;
      #else
         single4distantInitIterate( &(c4->c4trans.trans.toLock), &(c4->c4trans.trans.locks ) ) ;
         tran4freeLocks( c4, &(c4->c4trans.trans.toLock) ) ;
      #endif
   #endif
}



// AS Mar 17/03 - moved from c4trans.c
short S4FUNCTION code4tranStatusCB( CODE4 *cb )
{
   #ifdef E4VBASIC
      if ( c4parm_check( cb, 1, E40133 ) )
         return -1 ;
   #endif

   return code4tranStatus( cb ) ;
}


// AS Sep 24/03 - make available for export for ODBC
short S4FUNCTION code4tranStatusSetCB( CODE4 *cb, const int val )
{
   #ifdef E4VBASIC
      if ( c4parm_check( cb, 1, E40133 ) )
         return -1 ;
   #endif

   #ifndef S4OFF_TRAN   // LY Jun 22/04
      return code4tranStatusSet( cb, val ) ;
   #else
      return 0 ;
   #endif
}
