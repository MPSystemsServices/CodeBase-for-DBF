/* d4extra.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef E4ANALYZE
#ifdef P4ARGS_USED
   #pragma argsused
#endif
int code4verify( CODE4 *c4, int subs )
{
   if ( c4 == 0 )
      return error4( 0, e4parm_null, E93901 ) ;

   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;

   return 0 ;
}

int dfile4verify( DATA4FILE *d4, int subs )
{
   int rc ;

   if ( d4 == 0 )
      return error4( 0, e4parm_null, E91102 ) ;

   if ( d4->c4 == 0 )
      return error4( 0, e4struct, E91102 ) ;

   if ( subs == 1 )
      if ( ( rc = code4verify( d4->c4, 1 ) ) < 0 )
         return rc ;

   return 0 ;
}

#ifndef S4SINGLE
#ifndef S4CLIENT
int lock4groupVerify( LOCK4GROUP *lock, const int subs )
{
   int rc ;

   if ( lock->data == 0 )
      return error4( 0, e4parm_null, E93903 ) ;

   if ( subs == 1 )
      if ( ( rc = dfile4verify( lock->data->dataFile, 1 ) ) < 0 )
         return rc ;

/*
   if ( lock->link.n == 0 || lock->link.p == 0 )
      return error4( lock->data->c4, e4struct, E93903 ) ;
*/

   return 0 ;
}
#endif  /* S4CLIENT */
#endif  /* S4SINGLE */
#endif  /* E4ANALYZE */

#ifndef S4SINGLE
#ifndef S4CLIENT
int lock4groupLock( LOCK4GROUP *lock )
{
   int rc ;

#ifdef E4ANALYZE
   if ( ( rc = lock4groupVerify( lock, 1 ) ) < 0 )
      return rc ;
#endif

   switch( lock->id.type )
   {
      case LOCK4ALL:
         rc = d4lockAllInternal( lock->data, 0 ) ;
         break ;
      case LOCK4APPEND:
         rc = d4lockAppendInternal( lock->data, 0 ) ;
         break ;
      case LOCK4FILE:
         rc = d4lockFileInternal( lock->data, 0 ) ;
         break ;
      case LOCK4RECORD:
         rc = d4lockInternal( lock->data, lock->id.recNum, 0 ) ;
         break ;
      default:
         rc = error4( lock->data->codeBase, e4lock, E83901 ) ;
         break ;
   }

   return rc ;
}

int lock4groupUnlock( LOCK4GROUP *lock )
{
   int rc, oldUnlockAuto ;

   #ifdef E4ANALYZE
      if ( ( rc = lock4groupVerify( lock, 1 ) ) < 0 )
         return rc ;
   #endif

   oldUnlockAuto = code4unlockAuto( lock->data->codeBase ) ;
   code4unlockAutoSet( lock->data->codeBase, 1 ) ;

   switch ( lock->id.type )
   {
      case LOCK4ALL:
         #ifndef S4CLIPPER
            #ifndef S4MEMO_OFF
               dfile4memoUnlock( lock->data->dataFile ) ;
            #endif
         #endif
         #ifndef S4INDEX_OFF
            dfile4unlockIndex( lock->data->dataFile, lock->id.serverId ) ;
         #endif
         // AS Apr 15/03 - support for new lockId for shared clone locking
         rc = dfile4unlockFile( lock->data->dataFile, lock->id.lockId, lock->id.serverId ) ;
         break ;
      case LOCK4APPEND:
         rc = dfile4unlockAppend( lock->data->dataFile, lock->id.lockId, lock->id.serverId ) ;
         break ;
      case LOCK4FILE:
         rc = dfile4unlockFile( lock->data->dataFile, lock->id.lockId, lock->id.serverId ) ;
         break ;
      case LOCK4RECORD:
         rc = d4unlockRecord( lock->data, lock->id.recNum ) ;
         break ;
      default:
         rc = error4( lock->data->codeBase, e4info, E93801 ) ;
         break ;
   }

   code4unlockAutoSet( lock->data->codeBase, oldUnlockAuto ) ;

   return rc ;
}
#endif  /* S4CLIENT */
#endif  /* S4SINGLE */
