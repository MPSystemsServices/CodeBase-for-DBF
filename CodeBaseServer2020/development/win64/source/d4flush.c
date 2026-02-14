/* d4flush.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
   // needed for server for ODBC engine
   short S4FUNCTION d4changed( DATA4 *data, short flag )
   {
      int previous ;

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94101 ) )
            return -1;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E94101 ) ;
      #endif

      #ifndef S4SERVER
         #ifndef S4OFF_ENFORCE_LOCK
            if ( flag > 0 )
               if ( data->codeBase->lockEnforce && data->recNum > 0L )
                  if ( d4lockTest( data, data->recNum ) != 1 )
                     return error4( data->codeBase, e4lock, E94101 ) ;
         #endif
      #endif

      previous = data->recordChanged ;

      if ( flag >= 0 )
         data->recordChanged = ( flag > 0 ) ;

      return previous ;
   }
#endif /* S4OFF_WRITE */



// AS Oct 23/02 - Need to use d4flush() in places in server where we want to also perform update which
// #ifndef S4SERVER

int S4FUNCTION d4flush( DATA4 *data )
{
   #ifdef S4OFF_WRITE
      return 0 ;
   #else
      #ifndef S4CLIENT
         int rc, saveRc ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E94102 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94102 ) )
            return -1 ;
      #endif

      #ifdef S4CLIENT
         return d4update( data );
      #else
         saveRc = d4flushData( data ) ;
         rc = dfile4flushIndex( data->dataFile ) ;
         if ( rc < 0 )
            saveRc = rc ;

         return saveRc ;
      #endif
   #endif /* S4OFF_WRITE */
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int d4flushData( DATA4 *data )
{
   #ifdef S4OFF_WRITE
      return 0 ;
   #else
      #ifndef S4CLIENT
         int rc, saveRc ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm, E94105 ) ;
      #endif

      #ifdef S4CLIENT
         return d4flush( data ) ;
      #else
         saveRc = d4update( data ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         rc = dfile4flushData( data->dataFile ) ;
         if ( rc < 0 )
            saveRc = rc ;
         return saveRc ;
      #endif
   #endif
}

// #endif /* S4SERVER */



#ifndef S4CLIENT
   int dfile4flush( DATA4FILE *data )
   {
      int rc, saveRc ;

      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif

      saveRc = dfile4flushData( data ) ;
      rc = dfile4flushIndex( data ) ;
      if ( rc < 0 )
         saveRc = rc ;

      return saveRc ;
   }



   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int dfile4flushIndex( DATA4FILE *data )
   {
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         int rc ;
         #ifndef S4INDEX_OFF
            #ifndef N4OTHER
               INDEX4FILE *indexOn ;
            #else
               TAG4FILE *tagOn ;
            #endif
         #endif

         if ( data == 0 )
            return error4( 0, e4parm, E91102 ) ;

         rc = 0 ;

         #ifndef S4INDEX_OFF
            #ifndef S4CLIPPER
               indexOn = (INDEX4FILE *)l4first( &data->indexes ) ;
               if ( indexOn )
                  do
                  {
                     if ( index4flush( indexOn ) )
                        rc = -1 ;
                     indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn ) ;
                  } while ( indexOn != 0 ) ;
            #else
               for( tagOn = 0 ;; )
               {
                  tagOn = dfile4tagNext( data, tagOn ) ;
                  if ( !tagOn )
                     break ;
                  if ( tfile4flush( tagOn ) )
                     rc = - 1 ;
               }
            #endif
         #endif

         return rc ;
      #endif
   }



   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int dfile4flushData( DATA4FILE *data )
   {
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         int rc ;

         #ifdef E4PARM_LOW
            if ( data == 0 )
               return error4( 0, e4parm, E91102 ) ;
         #endif

         /* 03/14/97 AS if file opened excl. with opt, was not flushing header */
         #ifndef S4OFF_MULTI
         if ( data->file.lowAccessMode == OPEN4DENY_RW )
         #endif
            rc = dfile4updateHeader( data, 1, 1, 0 ) ;
         rc = file4flush( &data->file ) ;
         #ifndef S4OFF_MEMO
// LY 2003/07/31            #ifdef S4WIN64 /* LY 00/09/20 */
//               if ( data->memoFile.file.hand != NULL )
//            #else
               if ( data->memoFile.file.hand != INVALID4HANDLE )
//            #endif
               return file4flush( &data->memoFile.file ) ;
         #endif
         return rc ;
      #endif
   }
#endif  /* S4CLIENT */



#ifndef S4SERVER
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4FUNCTION code4flush( CODE4 *c4 )
   {
      #ifdef S4OFF_WRITE
         return 0 ;
      #else
         DATA4 *dataOn ;
         int rc, rcReturn ;
         LIST4 *list ;

         #ifdef E4VBASIC
            if ( c4parm_check( c4, 1, E94107 ) )
               return -1;
         #endif  /* E4VBASIC */

         #ifdef E4PARM_HIGH
            if ( c4 == 0 )
               return error4( 0, e4parm, E94107 ) ;
         #endif

         rcReturn = 0 ;

         list = tran4dataList( (&(c4->c4trans.trans)) ) ;
         dataOn = (DATA4 *)l4first( list ) ;
         while ( dataOn )
         {
            rc = d4flush( dataOn ) ;   /* returns -1 if error4code( codeBase ) < 0 */
            if ( rc )
               rcReturn = rc ;
            dataOn = (DATA4 *)l4next( list, dataOn) ;
         }

         return rcReturn ;
      #endif
   }
#endif /* S4SERVER */



#ifndef S4OFF_WRITE
   #ifndef S4CLIENT
      #ifndef S4INDEX_OFF
         int dfile4updateIndexes( DATA4FILE *data )
         {
            #ifdef S4FOX
               INDEX4FILE *indexOn ;
            #else
               TAG4FILE *tagOn ;
            #endif
            int rc ;

            #ifdef E4PARM_LOW
               if ( data == 0 )
                  return error4( 0, e4parm_null, E91102 ) ;
            #endif

            rc = 0 ;

            #ifdef S4FOX
               indexOn = (INDEX4FILE *)l4first( &data->indexes ) ;
               if ( indexOn )
               do
               {
                  if ( index4update( indexOn ) < 0 )
                     rc = -1 ;
                  indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn ) ;
               } while ( indexOn != 0 ) ;
            #else
               for( tagOn = 0 ;; )
               {
                  tagOn = dfile4tagNext( data, tagOn ) ;
                  if ( !tagOn )
                     break ;
                  if ( tfile4update(tagOn) < 0 )
                     rc = -1 ;
               }
            #endif

            return rc ;
         }
      #endif
   #endif



   int S4FUNCTION d4update( DATA4 *data )
   {
      int rc ;

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94109 ) )
            return -1 ;
      #endif

      if ( data == 0 )
         return error4( 0, e4parm, E94109 ) ;

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      rc = d4updateRecord( data, 0, 1 ) ;

      #ifndef S4CLIENT
         if ( rc == 0 )
            if ( data->dataFile->fileChanged )   /* if the header is outdated */
               #ifdef S4OFF_MULTI
                  // ensure to update the auto increment field as well...
                  rc = dfile4updateHeader( data->dataFile, 1, 1, 1 ) ;
               #else
                  if ( d4lockTestAppend( data ) == 1 )   /* if we have changed the record count */
                     rc = dfile4updateHeader( data->dataFile, 1, 1, 1 ) ;
               #endif
         #ifndef S4INDEX_OFF
            if ( rc == 0 )
               dfile4updateIndexes( data->dataFile ) ;
         #endif
      #endif

      return rc ;
   }



   int d4updateRecord( DATA4 *data, const int doUnlock, const int doLock )
   {
      /*
         PARAMATERS:

         doUnlock - true if we want to unlock after the update.  Normally we leave the locks in place
                    note that the paramater is used to unlock internally only if no update actually occurs.
                    Otherwise it is left for d4writeLow() to perform the unlock.  Also note that this
                    means that the unlock will occur even if no actual record update is performed.
         doLock - true if we want to place a lock on the record prior to the update.  Normally we
                  always want to do this, but with OLE-DB we handle locks explicitly, so for performance
                  reasons we pass a '0' in that case to avoid the overhead of duplicate locking.
      */
      int rc ;
      #ifndef S4OFF_MULTI
         int explicitUnlock ;
      #endif
      #ifndef S4OFF_MEMO
         int i ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94110 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm, E94110 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( data->codeBase == 0 )
            return error4describe( 0, e4struct, E94110, data->alias, 0, 0 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      if ( data->recNum <= 0 || data->eofFlag )
      {
         #ifndef S4OFF_MEMO
            if ( data->fieldsMemo != 0 )
               for ( i = 0; i < data->dataFile->nFieldsMemo; i++ )
               {
                  assert5( data->fieldsMemo[i].field != 0 ) ;
                  f4memoReset( data->fieldsMemo[i].field ) ;
               }
         #endif
         data->recordChanged = 0 ;
         return 0 ;
      }

      if ( data->recordChanged )
      {
         rc = d4writeLow( data, data->recNum, doUnlock, doLock ) ;
         if ( rc )
            return rc ;
         #ifndef S4OFF_MULTI
            explicitUnlock = 0 ;
         #endif

         #ifndef S4OFF_MEMO
            for ( i = 0; i < data->dataFile->nFieldsMemo; i++ )
               f4memoReset( data->fieldsMemo[i].field ) ;
         #endif
         data->recNumOld = -1 ;
      }
      else
      {
         #ifndef S4OFF_MULTI
            explicitUnlock = doUnlock ;
         #endif
         #ifndef S4OFF_MEMO
            for ( i = 0; i < data->dataFile->nFieldsMemo; i++ )
               data->fieldsMemo[i].status = 1 ;
         #endif
      }

      #ifndef S4OFF_MULTI
         if ( explicitUnlock )   /* unlock records (unless entire file is locked)... */
         {
            if ( code4unlockAuto( data->codeBase ) != 0 )
            {
               #ifdef S4SERVER
                  if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) != 1 )
                     if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4read ) != 1 )
               #else
                  #ifndef S4OFF_TRAN
                     if ( code4transEnabled( data->codeBase ) )
                        if ( code4tranStatus( data->codeBase ) == r4active )
                           return 0 ;
                  #endif
                  if ( d4lockTestFile( data ) != 1 )
               #endif
               {
                  rc = d4unlockLow( data, data4clientId( data ), 0 ) ;
                  if ( rc == r4active )  /* just a transactional notification */
                     return 0 ;
                  return rc ;
               }
            }
         }
      #endif /* S4OFF_MULTI */

      return 0 ;
   }
#endif  /* S4OFF_WRITE */
