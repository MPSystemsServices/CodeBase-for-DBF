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

/* c4trans.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef S4OFF_TRAN
   #define TRAN4ENTRY_LEN unsigned S4LONG
#endif  /* S4OFF_TRAN */

#ifdef __cplusplus
   extern "C" {
#endif

enum CommitPhaseType
{
   singlePhaseCommit,    // if committing 1 phase at a time
   dualPhaseCommit       // if committing both phases at the same time
} ;

#ifdef S4SERVER
   #ifndef S4OFF_TRAN
      #ifndef S4OFF_WRITE
         void tran4fileLowMarkBackupClose( TRAN4FILE_LOW *t4 ) ;
         void tran4fileLowInit( TRAN4FILE_LOW *t4, CODE4TRANS *c4trans, TRAN4FILE *trans ) ;
         #define S4TRAN_DEFINED
         #ifdef S4INLINE
            #define code4tranStart( c4 )        ( tran4lowStart( &((c4)->currentClient->trans), (c4)->currentClient->id, 0 ) )
            #define code4tranStartSingle( c4 )  ( tran4lowStart( &((c4)->currentClient->trans), (c4)->currentClient->id, 0 ) )
            #define code4tranCommitPhaseOne( c4, tp ) (tran4lowCommitPhaseOne( &(c4)->currentClient->trans, (c4)->currentClient->id, (tp)))
            #define code4tranCommitPhaseTwo( c4, d ) (tran4lowCommitPhaseTwo( &(c4)->currentClient->trans, (c4)->currentClient->id, d ) )
            #define code4tranRollback( c4 )     ( tran4lowRollback( &((c4)->currentClient->trans), (c4)->currentClient->id, 1 ) )
            #define code4tranStatus( c4 ) ( (c4)->currentClient->trans.currentTranStatus )
            #define code4tranStatusSet( c4, val ) ( (c4)->currentClient->trans.currentTranStatus = (val) )
         #else
            S4EXPORT int S4FUNCTION code4tranCommitPhaseOne( CODE4 S4PTR *, CommitPhaseType ) ;
            S4EXPORT int S4FUNCTION code4tranCommitPhaseTwo( CODE4 S4PTR *, int ) ;
            S4EXPORT int S4FUNCTION code4tranStart( CODE4 S4PTR * ) ;
            S4EXPORT int S4FUNCTION code4tranStartSingle( CODE4 S4PTR * ) ;
            S4EXPORT int S4FUNCTION code4tranRollback( CODE4 S4PTR * ) ;
            S4EXPORT int S4FUNCTION code4tranStatus( CODE4 * ) ;
            S4EXPORT int S4FUNCTION code4tranStatusSet( CODE4 *, const int val ) ;
         #endif
         int code4tranRollbackSingle( CODE4 *c4 ) ;
      #endif
   #endif
   #ifdef S4TRAN_DEFINED
      #undef S4TRAN_DEFINED
   #else
      #define code4tranStart( c4 ) ( 0 )
      #define code4tranRollback( c4 ) ( 0 )
      #define code4tranStatus( c4 ) ( r4inactive )
   #endif
#else
   #ifdef __cplusplus
      extern "C" {
   #endif
   /* exported for utilities */
   S4EXPORT int S4FUNCTION code4tranCommitPhaseOne( CODE4 S4PTR *, enum CommitPhaseType phaseType ) ;
   S4EXPORT int S4FUNCTION code4tranCommitPhaseTwo( CODE4 S4PTR *, int ) ;
   S4EXPORT int S4FUNCTION code4tranInit( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4tranRollback( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4tranStart( CODE4 S4PTR * ) ;
   #ifdef S4OFF_TRAN
      #ifdef S4CLIENT
         #define code4tranStatus( c4 ) ( (c4)->c4trans.trans.currentTranStatus )
      #else
         #define code4tranStatus( c4 ) ( 0 )
         #define code4tranStatusSet( c4, b ) ( 0 ) // LY Dec 9/04
      #endif
   #else
      #ifndef S4OFF_WRITE
         S4EXPORT int S4FUNCTION code4tranStartSingle( CODE4 S4PTR * ) ;
         #ifdef S4INLINE
            #define code4tranStatus( c4 ) ( (c4)->c4trans.trans.currentTranStatus )
            #define code4tranStatusSet( c4, val ) ( (c4)->c4trans.trans.currentTranStatus = (val) )
         #else
            S4EXPORT int S4FUNCTION code4tranStatusSet( CODE4 *, const int val ) ;
         #endif
      #endif
   #endif
   #ifndef S4INLINE
      #ifdef S4CLIENT
         int code4tranInit2( CODE4 *, const char *, const char * ) ;
         void code4tranInitUndo( CODE4 * ) ;
      #endif
   #endif
   #ifdef S4STAND_ALONE
      S4EXPORT int S4FUNCTION code4tranInit2( CODE4 S4PTR *, const char S4PTR *, const char S4PTR * ) ;
      void code4tranInitUndo( CODE4 * ) ;
   #endif
   #ifdef __cplusplus
      }
   #endif
#endif  /* S4SERVER */
// AS Mar 17/03 - make available in server as well for ODBC
S4EXPORT short S4FUNCTION code4tranStatusCB( CODE4 * ) ;

// AS Sep 24/03 - make available for export for ODBC
S4EXPORT short S4FUNCTION code4tranStatusSetCB( CODE4 *, const int val ) ;
S4EXPORT int S4FUNCTION code4tranCommit( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4tranCommitSingle( CODE4 S4PTR * ) ;

#ifndef S4OFF_TRAN
   #ifndef S4CLIENT
      #define tran4fileLowLen( tranFile ) ( (tranFile)->fileLen )
      #define tran4fileLowLenAdd( tranFile, len ) ( file4longAdd( &((tranFile)->fileLen), (len) ) )
      void tran4fileLenSet( TRAN4FILE *tranFile, FILE4LONG len ) ;
      void tran4fileLowLenSet( TRAN4FILE_LOW *tranFile, FILE4LONG len ) ;
   #endif
#endif

#if !defined( S4OFF_OPTIMIZE ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
   #ifdef S4SERVER
      // AS May 30/03 - enable/disable log flushing - default is enabled. - improves performance (esp. ODBC), maybe cannot recover from crash.
      // #define tran4fileLowFlush( t4 ) ( (t4)->transFile->needsFlushing ? ( (t4)->transFile->needsFlushing = 0, file4flush( &((t4)->file) ) ) : 0 )
      // AS July 12/01 Allow the primary log file to be disabled (backup logs still left intact if in use)
      // so don't flush the primary file if not available
      // #define tran4fileFlush( t4 ) ( (t4)->needsFlushing ? ( (t4)->needsFlushing = 0, ( ((t4)->primary.isDisabled) ? 0 : file4flush( &((t4)->primary.file) ) ), ((t4)->backup.validState ? file4flush( &((t4)->backup.file)) : 0 ) ) : 0 )
   #else
      // #define tran4fileLowFlush( t4 ) ( (t4)->needsFlushing ? ( (t4)->needsFlushing = 0, file4flush( &((t4)->file) ) ) : 0 )
      // #define tran4fileFlush( t4 ) ( (t4)->needsFlushing ? ( (t4)->needsFlushing = 0, file4flush( &((t4)->file) ) ) : 0 )
   #endif
#endif

#ifdef S4SERVER
   int tran4fileLowClose( TRAN4FILE_LOW *t4 ) ;
   int tran4fileLowOpen( TRAN4FILE_LOW *t4, const char *name ) ;
   int tran4fileLowCreate( TRAN4FILE_LOW *t4, const char *name ) ;
#endif

#ifndef S4OFF_WRITE
   #ifndef S4CLIENT
      #ifndef S4OFF_TRAN
         #define TRAN4OPEN             1
         #define TRAN4OPEN_TEMP        2
         #define TRAN4CLOSE            3
         #define TRAN4START            4
         #define TRAN4COMMIT_PHASE_ONE 5
         #define TRAN4COMMIT_PHASE_TWO 6
         #define TRAN4ROLLBACK         7
         #define TRAN4WRITE            8
         #define TRAN4APPEND           9
         #define TRAN4VOID            10
         // AS 09/25/00 - added support for create for backup/recovery purposes
         #define TRAN4CREATE          11
         /* #define TRAN4CREATE_TEMP    11  Not Supported*/
         #define TRAN4PACK            12
         #define TRAN4ZAP             13
         #define TRAN4INIT            15
         #define TRAN4SHUTDOWN        16
         #define TRAN4BACKEDUP        17
         #define TRAN4INIT_UNDO       18
         // AS 09/19/00 - used for backup log files to indicate were closed by server as part of swap procedure (may happen in the middle of transactions)
         #define TRAN4CLOSE_BACKUP    19
         // AS 02/20/01 - allow for indication of a default path to allow for context sensitive directories (for backups, etc.)
         #define TRAN4DIRECTORY       20
         // AS Jan 17/06 - if the file is encrypted, mark it as such with a new create marker
         #define TRAN4CREATE_ENCRYPT  21

         #define E4_LOG_IFS "Log File - invalid file status"
         #define TRAN4FORWARDS  1
         #define TRAN4BACKWARDS 2
         #define TRAN4LOCK_BASE      (1000000000L)
         #define TRAN4LOCK_SERVER    TRAN4LOCK_BASE+0
         #define TRAN4LOCK_MULTIPLE  TRAN4LOCK_BASE+1
         #define TRAN4LOCK_BACKUP    TRAN4LOCK_BASE+2
         #define TRAN4LOCK_RESTORE   TRAN4LOCK_BASE+3
         #define TRAN4LOCK_FIX       TRAN4LOCK_BASE+4

         /* CJ- changed so older version of CodeUtil would report the correct error */
         #define TRAN4VERSION_NUM 3

         /* The version # for the backup log file for auto-recovery:  = 10 + TRAN4VERSION_NUM */
         #define TRAN4VERSION_NUM_BACKUP 10 + TRAN4VERSION_NUM

         // AS Apr 28/03 - made trans-shared a run-time switch
         #define TRAN4LOCK_USERS     TRAN4LOCK_BASE+1000
         #define TRAN4MAX_USERS      1000
      #endif  /* S4OFF_TRAN */
   #endif  /* not S4CLIENT */

   #ifdef __cplusplus
      extern "C" {
   #endif
   #ifndef S4OFF_TRAN
      #ifndef S4CLIENT
         int  tran4fileCreate( TRAN4FILE *, const char * ) ;
         #ifdef E4ANALYZE
            // required by ole-db
            S4EXPORT int S4FUNCTION tran4active( CODE4 S4PTR *, DATA4 S4PTR * ) ;
         #else
            int  tran4active( CODE4 *, DATA4 * ) ;
         #endif
         int  tran4addUser( TRAN4 *, const long, const char *, const unsigned short int ) ;
         /* log file examination functionality */
         S4EXPORT int  S4FUNCTION tran4fileAppend( TRAN4FILE S4PTR *, LOG4HEADER S4PTR *, const char S4PTR *, int ) ;  /* a null pointer means use internal value */
         S4EXPORT int  S4FUNCTION tran4fileBottom( TRAN4FILE S4PTR *, TRAN4 S4PTR * ) ;
         S4EXPORT int  S4FUNCTION tran4fileSkip( TRAN4FILE S4PTR *, TRAN4 S4PTR *, const int ) ;
         S4EXPORT int  S4FUNCTION tran4fileTop( TRAN4FILE S4PTR *, TRAN4 S4PTR * ) ;
         S4EXPORT int S4FUNCTION tran4fileClose( TRAN4FILE *t4 ) ;
         int  code4tranInitUndoLow( TRAN4 *, const long ) ;
         S4EXPORT int  S4FUNCTION tran4set(  TRAN4 S4PTR *, const int, const long, const long, const int, const unsigned int, const long, const long ) ;
         S4EXPORT int  S4FUNCTION tran4putData(  TRAN4 S4PTR *, const void *, unsigned ) ;
         /* log file examination functionality */
         #ifndef S4INLINE
            S4EXPORT int  S4FUNCTION tran4bottom( TRAN4 S4PTR * ) ;
            S4EXPORT unsigned long S4FUNCTION tran4clientDataId( TRAN4 S4PTR * ) ;
            S4EXPORT long S4FUNCTION tran4clientId( TRAN4 S4PTR * ) ;
            S4EXPORT long S4FUNCTION tran4id( TRAN4 S4PTR * ) ;
            S4EXPORT unsigned S4FUNCTION tran4len( TRAN4 S4PTR * ) ;
            S4EXPORT unsigned long S4FUNCTION tran4serverDataId( TRAN4 S4PTR * ) ;
            S4EXPORT int  S4FUNCTION tran4skip( TRAN4 S4PTR *, int ) ;
            S4EXPORT int  S4FUNCTION tran4top( TRAN4 S4PTR * ) ;
            S4EXPORT int  S4FUNCTION tran4type( TRAN4 S4PTR * ) ;
         #endif
         S4EXPORT void * S4FUNCTION tran4getData( TRAN4 S4PTR *, const long pos  ) ;
         S4EXPORT int S4FUNCTION tran4read( TRAN4 *t4 ) ;
      #endif  /* not S4CLIENT */
   #endif  /* S4OFF_TRAN */

   #ifndef S4OFF_TRAN
      #ifndef S4CLIENT
         S4EXPORT int S4FUNCTION tran4lowAppend( TRAN4 S4PTR *, const char S4PTR *, int ) ;
         S4EXPORT int S4FUNCTION tran4lowCommitPhaseOne( TRAN4 S4PTR *, long, enum CommitPhaseType ) ;
         S4EXPORT int S4FUNCTION tran4lowCommitPhaseTwo( TRAN4 S4PTR *, long, int ) ;
         S4EXPORT int S4FUNCTION tran4lowRollback( TRAN4 S4PTR *, long, const int ) ;
         S4EXPORT int S4FUNCTION tran4lowStart( TRAN4 S4PTR *, long, int ) ;
         int tran4lowUnappend( TRAN4 * ) ;
         int tran4lowUnwrite( TRAN4 * ) ;
         void tran4getTime( CODE4 * ) ;
      #endif  /* S4CLIENT */
   #endif  /* S4OFF_TRAN */
   #ifdef __cplusplus
      }
   #endif
#endif /* S4OFF_WRITE */

#ifdef S4SERVER
   // AS July 9/02 - New function for populating an in-progress server version of the log-file (for log4compress)
   int code4transFilePopulate( CODE4TRANS *c4trans ) ;
#endif

#ifndef S4CLIENT
   int tran4closeAll( struct TRAN4St * ) ;
   #ifndef S4SINGLE
      #ifdef S4SERVER
         int tran4lock( TRAN4 * ) ;
      #endif
      S4EXPORT int  S4FUNCTION code4tranLockTransactions( CODE4TRANS S4PTR *, long ) ;
      #ifndef S4UTILS
         S4EXPORT int  S4FUNCTION code4tranUnlockTransactions( CODE4TRANS S4PTR *, long ) ;
      #endif
   #endif  /* S4SINGLE */
#endif  /* S4CLIENT */
DATA4 *tran4data( struct TRAN4St *, const long, const long ) ;
DATA4 *tran4dataName( struct TRAN4St *, const char *, const long, const int ) ;

const DATA4 *tran4dataPtrOnly( const struct TRAN4St *, const long, const long ) ;

#ifdef E4ANALYZE
   int  tran4verify( TRAN4 *, int ) ;
#endif  /* E4ANALYZE */

int code4tranInitLow( TRAN4 *, CODE4TRANS * ) ;
int code4transInit( CODE4TRANS *, CODE4 * ) ;
S4EXPORT int S4FUNCTION code4transInitUndo( CODE4TRANS S4PTR * ) ;

#ifdef __cplusplus
   }
#endif
