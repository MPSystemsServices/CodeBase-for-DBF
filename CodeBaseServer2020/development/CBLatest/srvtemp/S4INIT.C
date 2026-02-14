/* s4init.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

/* no spools */
int S4FUNCTION sort4getMemInit( SORT4 *s4 )
{
   #ifdef E4PARM_LOW
      if ( s4 == 0 )
         return error4( 0, e4parm_null, E91901 ) ;
   #endif

   s4quick( (void **)s4->pointers, s4->pointersUsed, s4->cmp, s4->sortLen) ;
   #ifdef  E4ANALYZE
      if ( s4->seqwriteBuffer == 0 )
         return error4( s4->codeBase, e4info, E91901 ) ;
   #endif
   u4free( s4->seqwriteBuffer ) ;
   s4->seqwriteBuffer = 0 ;

   return 0 ;
}



int S4FUNCTION sort4getInit( SORT4 *s4 )
{
   int rc ;

   #ifdef E4PARM_HIGH
      if ( s4 == 0 )
         return error4( 0, e4parm_null, E91902 ) ;
   #endif

   if ( error4code( s4->codeBase ) < 0 )
      return e4codeBase ;

   if ( s4->spoolsMax > 0 )
   {
      rc = sort4spoolsInit( s4, 0 ) ;
      if ( rc == e4memory )
      {
         sort4free( s4 ) ;
         return error4( s4->codeBase, e4memory, E91902 ) ;
      }
   }
   else
   {
      sort4getMemInit( s4 ) ;
      return 0 ;
   }

   return rc ;
}



int S4FUNCTION sort4spoolsInit( SORT4 *s4, const int prevCall )
{
   unsigned int entriesPerSpool, spoolsPerPool, entriesUsed ;
   void *ptr, *memFree ;
   int rc ;
   char *poolEntry, *poolEntryIterate ;
   // AS 10/16/00 - support for large files added
   // S4LONG spoolDiskI ;
   FILE4LONG spoolDiskI ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 || prevCall < 0 || prevCall > 1 )
         return error4( 0, e4parm, E91903 ) ;
   #endif

   poolEntry = (char *) 0 ;
   if ( !prevCall )
   {
      rc = s4flush( s4 ) ;
      if ( rc < 0 )
         return error4stack( s4->codeBase, (short)rc, E91903 ) ;
      rc = file4seqWriteFlush(&s4->seqwrite ) ;
      if ( rc < 0 )
         return error4stack( s4->codeBase, (short)rc, E91903 ) ;
      #ifdef E4ANALYZE
         if (s4->seqwriteBuffer == 0 )
            return error4( s4->codeBase, e4info, E91903 ) ;
      #endif
      u4free( s4->seqwriteBuffer ) ;
      s4->seqwriteBuffer = 0 ;

      u4free( s4->pointers ) ;
      s4->pointers = 0 ;
   }

   // allocate memory for the disk spool handlers.  If running short on memory, free up the memory pools
   // (i.e. an s4->pool) to obtain more memory, and re-try.  If we free up all the memory pools and still
   // cannot allocate the spools, we have insufficient memory to proceed.
   for (;;)
   {
      // the spools max comes from the sort which has already been done.  Each time s4flush() is called
      // to flush out a part of the sort, a new spool is created
      s4->spoolPointer = (S4SPOOL *)u4alloc( (long)sizeof(S4SPOOL) * s4->spoolsMax ) ;
      if ( s4->spoolPointer )
         break ;

      if ( l4last( &s4->pool ) == 0 )
         return e4memory ;

      memFree = l4pop( &s4->pool ) ;
      mem4free( s4->poolMemory, memFree ) ;
      s4->poolN-- ;
   }

   for(;;)
   {
      /* Split up the pools between the spools - i.e. caluclate how many disk spools each memory pool will handle */
      if ( s4->poolN == 0 )
         spoolsPerPool = s4->spoolsMax ;
      else
         spoolsPerPool = ( s4->spoolsMax + s4->poolN - 1 ) / s4->poolN ;

      // what needs to happen is that a pool must contain at least 1 item for each spool in order to do its
      // sort.  For example, if you have 2 pools, and 6 spools, you have 3 spools for every pool.  In order
      // to sort the memory, you load as many items from each spool into each pool (note that the spools are
      // already sorted), and then you sort through the pool.  If you cannot load at least 1 item from each
      // spool into the pool, then you cannot sort beacuse at least 1 spool will not have a member in the pool,
      // and if it is excluded, it won't be guaranteed sorted into the proper sequence.
      // what we want to do here is calculate the # of entries for each spool that will be placed into the
      // pool.  (in the above example, if the poolEntries is say 24 (enough memory for 24 entries), and we
      // have 6 spools, but and 2 pools, (3 spolls per pool), then we can have 8 entries per spool.  i.e.
      // each time we load, we can (in theory) load up 8 records from every spool into the pool, for a total
      // of 24 entries in each pool (and with 2 pools, an overall loading of 48 entries)
      // the # of entries in a spool
      entriesPerSpool = s4->poolEntries / spoolsPerPool ;

      if ( entriesPerSpool == 0 )  // means that the pool is sharing so many spools that we cannot load at least 1 entry per spool, so cannot sort
      {
         // AS 10/05/00 - here is where we need the fix.
         // in this case, we need to allocate more pools becuase we have more spools than can be handled by the
         // current number of pools.  So try that, and if we cannot allocate, then return r4memory
         char *poolPtr = (char *)mem4allocNoZero( s4->poolMemory ) ;
         if ( poolPtr == 0 )
            return e4memory ;
         l4add( &s4->pool, poolPtr ) ;
         s4->poolN++ ;
         // in this case we likely will not be increasing the 's4->pointersMax' value, but that is not the
         // point at this point in the sort.  We just need more pools to hold values
         sort4initPointers( s4, poolPtr + sizeof( LINK4 ), s4->codeBase->memSizeSortPool - sizeof( LINK4 ) ) ;
         continue ;   // go back to the top and see if we have enough pools now...
      }

      // we have some pools, so we are done
      if ( s4->poolN != 0 )
         break ;

      // if here, then we do not have any pools.  Allocate a pool
      ptr = mem4allocNoZero( s4->poolMemory ) ;
      if ( ptr != 0 )
      {
         l4add( &s4->pool, ptr ) ;
         s4->poolN++ ;
      }
      else
      {
         // not enough memory to allocate a pool.  What we need to do is reduce the size of the pool
         // (and in effect reduce the # of entries that can be stored in a pool) and then re-try the
         // allocation
         s4->poolEntries /= 2 ;
         for ( ;; )
         {
            ptr = l4pop(&s4->pool) ;
            if ( ptr == 0 )
               break ;
            mem4free( s4->poolMemory, ptr ) ;
         }
         mem4release( s4->poolMemory ) ;
         s4->poolMemory = mem4create( s4->codeBase, 1, (unsigned) s4->poolEntries*s4->totLen+sizeof(LINK4), 1, 1 ) ;
      }
   }

   s4->spoolMemLen = entriesPerSpool * s4->totLen ;
   s4->spoolDiskLen = (S4LONG)s4->pointersInit * s4->totLen ;

   entriesUsed = s4->poolEntries+1 ;  /* Entries used in current pool. */
   poolEntryIterate = 0 ;

   /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
   file4longAssign( spoolDiskI, 0, 0L ) ;
   for ( ; s4->spoolsN < s4->spoolsMax; )
   {
      c4memmove( s4->spoolPointer+1, s4->spoolPointer, sizeof(S4SPOOL)*s4->spoolsN ) ;
      if ( entriesUsed + entriesPerSpool > s4->poolEntries )
      {
         entriesUsed = 0 ;
         poolEntryIterate= (char *) l4next( &s4->pool, poolEntryIterate);
         poolEntry = poolEntryIterate + sizeof(LINK4) ;
      }
      s4->spoolPointer->ptr = poolEntry ;
      poolEntry   += (s4->totLen*entriesPerSpool) ;
      entriesUsed += entriesPerSpool ;

      s4->spoolPointer->spoolI = s4->spoolsN++ ;
      file4longAssignLong( s4->spoolPointer->disk, spoolDiskI ) ;
      file4longAdd( &spoolDiskI, s4->spoolDiskLen ) ;

      s4->spoolPointer->len = 0 ;
      if ( s4->spoolsN < s4->spoolsMax )  // AS Nov 20/06 - not checking return code here...
      {
         rc = s4nextSpoolEntry(s4) ;
         if ( rc != 0 )
            return rc ;
      }
   }
   return 0 ;
}



int S4FUNCTION sort4init( SORT4 *s4, CODE4 *c4, const int sortL, const int infoL )
{
   #ifdef E4PARM_HIGH
      if ( s4 == 0 || c4 == 0 )
         return error4( c4, e4parm_null, E91904 ) ;
   #endif

   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   sort4initSet( s4, c4, sortL, infoL ) ;
   if ( sort4initAlloc( s4 ) == e4memory )
   {
      sort4free( s4 ) ;
      return error4( c4, e4memory, E91904 ) ;
   }
   return 0 ;
}



int S4FUNCTION sort4initSet( SORT4 *s4, CODE4 *c4, const int sortL, const int infoL )
{
   #ifdef E4PARM_LOW
      if ( s4 == 0 || c4 == 0 || sortL < 0 || infoL < 0 )
         return error4( 0, e4parm, E91905 ) ;
   #endif

   c4memset( (void *)s4, 0, sizeof(SORT4) ) ;
   s4->file.hand = INVALID4HANDLE ;

   s4->codeBase = c4 ;
   s4->cmp = (S4CMP_FUNCTION *)u4memcmp ;

   s4->sortLen = (unsigned int)sortL ;
   s4->infoLen = (unsigned int)infoL ;
   s4->infoOffset = s4->sortLen + sizeof(S4LONG) ;
   #ifdef S4DATA_ALIGN  /* LY 00/11/15 : ex105.cpp and S4WIN64 */
      s4->totLen = s4->infoOffset + s4->infoLen + (8L - (s4->infoOffset + s4->infoLen)%8L)%8L ;
   #else
      s4->totLen = s4->infoOffset + s4->infoLen ;
   #endif
   s4->poolEntries = ( c4->memSizeSortPool - sizeof( LINK4 ) ) / s4->totLen ;
   s4->pointersMax = c4->memSizeSortPool / sizeof( char * ) ;
   s4->isMemAvail = 1 ;
   return 0 ;
}



int S4FUNCTION sort4initAlloc( SORT4 *s4 )
{
   FILE4LONG start ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 )
         return error4( 0, e4parm_null, E91906 ) ;
   #endif

   if ( s4->seqwriteBuffer == 0 )
   {
      s4->seqwriteBuffer = (char *)u4alloc( (long)s4->codeBase->memSizeSortBuffer ) ;
      if ( s4->seqwriteBuffer == 0 )
         return e4memory ;
      /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
      file4longAssign( start, 0, 0L ) ;
      file4seqWriteInitLow( &s4->seqwrite, &s4->file, start, s4->seqwriteBuffer, s4->codeBase->memSizeSortBuffer ) ;
   }

   if ( s4->pointers == 0 )
      for(;;)
      {
         s4->pointers = (char **)u4alloc( (long)s4->pointersMax * sizeof(char *) ) ;
         if ( s4->pointers != 0 )
            break ;

         s4->pointersMax /= 2 ;
         if ( s4->pointersMax < 256 )
            return e4memory ;
      }

   #ifdef E4ANALYZE
      if ( s4->poolMemory )
         return error4( s4->codeBase, e4info, E81901 ) ;
   #endif

   s4->poolMemory = mem4create( s4->codeBase, 1, s4->codeBase->memSizeSortPool,1,1);
   if ( s4->poolMemory == 0 )
         return e4memory ;

   return 0 ;
}



void sort4initPointers( SORT4 *s4, char *availMem, unsigned int len )
{
   /* Assign 'pointers' */
   unsigned int n, i ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 || availMem == 0 )
      {
         error4( 0, e4parm_null, E91907 ) ;
         return ;
      }
   #endif

   n = len / s4->totLen ;
   i = s4->pointersInit ;

   s4->pointersInit += n ;
   if ( s4->pointersInit > s4->pointersMax )
      s4->pointersInit = s4->pointersMax ;

   for ( ; i < s4->pointersInit ; i++, availMem += s4->totLen )
      s4->pointers[i] = availMem ;
}



int S4FUNCTION sort4alloc( SORT4 S4PTR * S4PTR *s4 )
{
   *s4 = 0 ;
   *s4 = (SORT4 *) malloc( sizeof(SORT4) ) ;
   if ( *s4 == 0 )
      return -1 ;
   return 0 ;
}


void S4FUNCTION sort4assignCmp2( SORT4 S4PTR *s4, S4CMP_FUNCTION function )
{
   sort4assignCmp( s4, function ) ;
}


void S4FUNCTION sort4free2( SORT4 S4PTR * S4PTR *s4 )
{
   free( *s4 ) ;
}
