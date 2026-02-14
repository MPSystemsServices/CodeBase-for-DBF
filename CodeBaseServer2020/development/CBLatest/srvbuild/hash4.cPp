/* hash4.cpp   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4LOCK_HASH

void *Hash4lock::operator new( size_t s )
{
   void *ptr = u4alloc( s ) ;
   return ptr ;
}



void Hash4lock::operator delete( void *p )
{
   u4free( p ) ;
}



Hash4lock::Hash4lock()
{
   numEntries = 1 ;
   mask = numEntries - 1 ;
   ptrs = (Single4hash *)u4alloc( sizeof( Single4 ) ) ;
}



Hash4lock::~Hash4lock()
{
   u4free( ptrs ) ;
}



#ifdef E4HASH
   void Hash4lock::check()
   {
      for( int loop = 0 ; loop < numEntries ; loop++ )  // go through each list
      {
         Single4hash *single = (Single4hash *)ptrs[loop].initIterate() ;
         for( ;; )
         {
            Lock4 *lock = (Lock4 *)single ;
            if ( lock == 0 )
               break ;
            assert5 ( hash( lock->recNum ) == loop ) ;  // ensure on the correct list
            single = (Single4hash *)single->next() ;
         }
      }
   }
#endif



void Hash4lock::expand()   // double the entries in the hash
{
   #ifdef E4HASH
      check() ;
   #endif
   int oldCount = numEntries ;
   numEntries <<= 1 ;
   mask = numEntries - 1 ;
   int bitMask = numEntries ;
   Single4 *ptrsOld ;
   ptrsOld = ptrs ;
   ptrs = (Single4hash *)u4alloc( sizeof( Single4 ) * numEntries ) ;
   // is it more efficient to add all from the DATA4 lists again?
   for( int loop = 0 ; loop < oldCount ; loop++ )  // go through each list
   {
      Single4distant distant ;
      distant.initIterate( &(ptrsOld[loop]) ) ;
      for ( ;; )  // possibly remove and add...
      {
         Lock4 *lock = (Lock4 *)(Single4hash *)distant.toItem() ;
         if ( lock == 0 )
            break ;
         // if the bitMask (== numEntries) is on, then we need to
         // move the item to the other side of the hash, otherwise
         // it may stay where it is
// need to remove them all from old list and move over to new
         distant.remove() ;
         ptrs[hash(lock->recNum)].add( (Single4hash *)lock ) ; /* add to the new list */
//            distant.next() ;
      }
   }
   u4free( ptrsOld ) ;
   #ifdef E4HASH
      check() ;
   #endif
}



/*
#ifdef S4SERVER
   int Hash4lock::lockTest( struct DATA4St *data, long clientId, long recNo )
#else
   int Hash4lock::lockTest( struct DATA4St *data, long recNo )
#endif
{
   // returns '1' if we have locked, 'r4locked' if somebody else has locked
   Single4 *list = (ptrs[hash( recNo )]).initIterate() ;
   int rc = 0, doExpand = 0 ;  // expand after we find the item, since that is more efficient overall
   for( int counter = 0 ;; )
   {
      Lock4 *lock = (Lock4 *)(Single4hash *)list->next() ;
      if ( lock == 0 ) // done
      {
         if ( doExpand )
            expand() ;
         return rc ;  // either 0 or r4locked
      }
      if ( recNo == lock->recNum )
      {
         if ( lock->data == data )
            #ifdef S4SERVER
               if ( lock->clientId == clientId )
            #endif
            {
               if ( doExpand )
                  expand() ;
               return 1 ;
            }
         if ( lock->lockType == lock4write )  // only 1 allowed, not us...
         {
            if ( doExpand )
               expand() ;
            return r4locked ;
         }
         // we wan't to keep looking in the case of read-locks, to see if we hold a read lock
         rc = r4locked ;  // if we don't find our lock, it is r4locked
      }
      else
         if ( lock->lockType == lock4write )
         {
            counter++ ;
            if ( counter > HASH4LOCK_EXPAND )
               doExpand = 1 ;
         }
   }
}
*/



void Hash4lock::remove( Lock4 *lock )
{
   #ifdef E4HASH
      check() ;
   #endif

   int hVal = hash( lock->recNum ) ;
   Single4distant singleDistant ;
   singleDistant.initIterate( &(ptrs[hVal]) ) ;

   for( ;; )
   {
      /* note that the lock must be on the list.  If the list is empty, then we are calling
         a remove on a list for which the link does not exist */
      #ifdef E4HASH
         if ( singleDistant.toItem() == 0 )
            error4( 0, e4info, E92601 ) ;
      #endif
      if ( singleDistant.isLastLink() == 1 )  // assume done if we are last link on chain
      {
         // for debug purposes, ensure the item matches...
         #ifdef E4HASH
            assert5( (Single4hash *)singleDistant.toItem() == lock ) ;
         #endif
         singleDistant.remove() ;
         #ifdef E4HASH
            check() ;
         #endif
         return ;
      }
      // otherwise must do a comparison
      if ( (Single4hash *)singleDistant.toItem() == lock )
      {
         singleDistant.remove() ;
         #ifdef E4HASH
            check() ;
         #endif
         return ;
      }
      singleDistant.next() ;
   }
}



void Hash4lock::upgradeLock( DATA4 *data, long recNo, Lock4type lockType )
{
   #ifdef E4HASH
      check() ;
   #endif

   assert5( ptrs != 0 ) ;

   int hVal = hash( recNo ), numWriteLocksOnChain = 0 ;
   Single4distant singleDistant ;
   singleDistant.initIterate( &(ptrs[hVal]) ) ;

   for ( ;; )
   {
      Lock4 *lock = (Lock4 *)(Single4hash *)singleDistant.toItem() ;
      assert5( lock != 0 ) ;   // we do a find previous, so it better be here
      if ( lock->recNum == recNo && lock->data == data )
      {
         #ifdef S4SERVER
            assert5( lock->clientId == data4clientId( data ) ) ;
         #endif
         #ifndef S4OFF_MULTI
            if ( lockType == lock4write )  // physically lock to increment the write count
               dfile4lock( data->dataFile, data4clientId( data ), data4serverId( data ), recNo ) ;
            else  // phsically unlock
               dfile4unlockRecordDo( data->dataFile, recNo ) ;
         #endif
         lock->lockType = lockType ;   /* do the upgrade */
         return ;
      }
      singleDistant.next() ;
   }

}



// AS Dec 03/01 - server case where recNo == -1 (looking for any lock) was not working properly
#ifdef S4SERVER
   int Hash4lock::findAnyLock( DATA4 *data, Lock4type lockType )
   {
      long clientId = data4clientId( data ) ;

      // basically we want to go through every list that the hash table has, not just the primary one...
      for( int loop = 0 ; loop < numEntries ; loop++ )  // go through each list
      {
         Single4distant singleDistant ;
         singleDistant.initIterate( &(ptrs[loop]) ) ;

         for ( ;; )
         {
            Lock4 *lock = (Lock4 *)(Single4hash *)singleDistant.toItem() ;
            if ( lock == 0 )
               break ;
            if ( lock->data == data )
            {
               if ( lock->clientId == clientId || clientId == 0 )  // clientId is set to 0 for general enquiries...
                  if ( lock->lockType == lockType || lockType == lock4any )
                     return 1 ;
            }
            singleDistant.next() ;
         }
      }

      return 0 ;
   }
#endif


int Hash4lock::find( DATA4 *data, long recNo, Lock4type lockType )
{
   // find returns 1 if we have locked, r4locked if someone else, 0 if nobody
   #ifdef E4HASH
      check() ;
   #endif

   int doExpand = 0, hVal = hash( recNo ), numWriteLocksOnChain = 0 ;

   assert5( ptrs != 0 ) ;

   #ifdef E4PARM_LOW  //CJ 04/30/00 - do check to see if data4 and hval are valid.
      if (data == 0)
         return -1;

      if (hVal > numEntries)
         return -1;
   #endif

   // AS Dec 03/01 - server case where recNo == -1 (looking for any lock) was not working properly
   // because we only examined the list which matched the hash of '-1' i.e. hVal=hash( recNo ) below.
   // instead just call a different function
   #ifdef S4SERVER
      if ( recNo == -1 )
         return findAnyLock( data, lockType ) ;
   #endif

   Single4distant singleDistant ;
   singleDistant.initIterate( &(ptrs[hVal]) ) ;

   #ifdef S4SERVER
      long clientId = data4clientId( data ) ;
   #endif

   for ( ;; )
   {
      Lock4 *lock = (Lock4 *)(Single4hash *)singleDistant.toItem() ;
      if ( lock == 0 )
      {
         if ( doExpand )
            expand() ;
         return 0 ;
      }
      #ifdef S4SERVER
         // AS 02/14/01 - this function gets called when we just want to
         // determine if we hold any locks.  In that case, recNo passed in is -1
         // - done for cbadmin when we determine the lock status
         if ( recNo == -1 )
         {
            if ( lock->data == data )
            {
               #ifdef S4SERVER
                  if ( lock->clientId == clientId || clientId == 0 )  // clientId is set to 0 for general enquiries...
               #endif
                  if ( lock->lockType == lockType || lockType == lock4any )
                  {
                     if ( doExpand )
                        expand() ;
                     return 1 ;
                  }
            }
         }
      #endif
      if ( lock->recNum == recNo )
      {
         // AS Mar 19/03 - for ODBC, we consider any lock that any of our DATA4's hold as being our lock,
         // so return 1 if this is the case (this is due to ODBC tendency to use any DATA4, not the same one always)
         #if defined( S4ODBC_BUILD ) && defined( S4SERVER )
            if ( data->codeBase->odbc == 1 )
         #else
            if ( lock->data == data )
         #endif
         {
            #ifdef S4SERVER
               if ( lock->clientId == clientId )
            #endif
               if ( lock->lockType == lockType || lockType == lock4any )
               {
                  if ( doExpand )
                     expand() ;
                  return 1 ;
               }
         }

         // AS 04/20/99 The following problem occurred:  the lock->data != data (i.e. different DATA4's)
         // but the clientId was the same.  This occurs because the serverId's are actually different
         // (i.e. different DATA4's).  the clientId's are the same because we have 2 distinct clients
         // in client/server using the same base database, the clientId is the client's handle on the DATA4,
         // is the same in both...

         // AS 08/26/98 - should only be looking for requested lock type, or if other has locked
         //         if ( lock->lockType == lock4write || lockType == lock4write ) // only 1 user allowed, only 1 lock

         // only 1 user allowed, only 1 lock
         #ifdef S4SERVER
            // AS 06/24/99 --> was failing in case where lock->lockType was write and we were attempting
            // to perform a read lock.  That should be ok...
            // if ( lock->lockType == lock4write || (lockType == lock4write && ( clientId != lock->clientId || lock->data != data )))
            if ( ( lock->lockType == lock4write || lockType == lock4write ) && ( clientId != lock->clientId || lock->data != data ) )
         #else
            if ( lock->lockType == lock4write || (lockType == lock4write ))
         #endif
         {
            if ( doExpand )
               expand() ;
            #ifdef S4SERVER
               /* AS 04/01/99 --> t4lock.c was failing - not registering locks, so added in here... */
               data->dataFile->tempClientLock = lock->clientId ;
               data->dataFile->tempServerLock = lock->data->serverId ;
            #endif
            #ifndef S4OFF_MULTI
               dfile4registerLocked( data->dataFile, recNo, 1 ) ;
            #endif
            return r4locked ;
         }
      }
      if ( lock->lockType == lock4write )
      {
         numWriteLocksOnChain++ ;
         if ( numWriteLocksOnChain > HASH4LOCK_EXPAND )
            doExpand = 1 ;
      }
      singleDistant.next() ;
   }
}

#endif /* S4LOCK_HASH */
