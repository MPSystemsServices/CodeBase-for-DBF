/* s4sort.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.

Internal sort information is saved as follows:
   Sort Info, Rec Num, Other Info

s4quick assumes there is an extra four bytes where the record number
is put.
*/

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

void s4deleteSpoolEntry( SORT4 *s4 )
{
   c4memcpy( (void *)s4->spoolPointer, (void *)( s4->spoolPointer + 1 ), --s4->spoolsN * sizeof( S4SPOOL ) ) ;
}



int s4flush( SORT4 *s4 )
{
   unsigned i ;
   int rc ;
   FILE4LONG startPos ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 )
         return error4( 0, e4parm_null, E91801 ) ;
   #endif

   rc = s4quick( (void **) s4->pointers, s4->pointersUsed, s4->cmp, s4->sortLen ) ;
   if ( rc < 0 )
      return error4stack( s4->codeBase, (short)rc, E91801 ) ;

   if ( s4->spoolsMax == 0 )  /* Temporary file must be created. */
   {
      file4tempLow( &s4->file, s4->codeBase, 1, 1, NULL ) ;
      file4longAssign( startPos, 0, 0 ) ;
      file4seqWriteInitLow( &s4->seqwrite, &s4->file, startPos, s4->seqwriteBuffer, s4->codeBase->memSizeSortBuffer ) ;
   }

   for ( i = 0; i < s4->pointersUsed; i++ )
   {
      if ( file4seqWrite( &s4->seqwrite, s4->pointers[i], s4->totLen) < 0 )
         return -1 ;
   }

   s4->pointersUsed = 0 ;
   if ( (unsigned S4LONG) s4->spoolsMax * (unsigned S4LONG) sizeof(S4SPOOL) >= (unsigned S4LONG) UINT_MAX )
   {
      sort4free( s4 ) ;
      return error4( s4->codeBase, e4memory, E91801 ) ;
   }
   s4->spoolsMax++ ;

   return 0 ;
}



int S4FUNCTION sort4free( SORT4 *s4 )
{
   void *poolPtr ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 )
         return error4( 0, e4parm_null, E91804 ) ;
   #endif

   u4free( s4->spoolPointer ) ;
   u4free( s4->pointers ) ;
   u4free( s4->seqwriteBuffer ) ;
   if ( s4->file.hand != INVALID4HANDLE )
      if ( file4close( &s4->file ) < 0 )
         return -1 ;

   for ( ;; )
   {
      poolPtr = l4pop( &s4->pool ) ;
      if ( poolPtr == 0 )
         break ;
      mem4free( s4->poolMemory, poolPtr ) ;
   }
   mem4release( s4->poolMemory ) ;

   c4memset( (void *)s4, 0, sizeof(SORT4) ) ;
   s4->file.hand = INVALID4HANDLE ;

   return 0 ;
}



int S4FUNCTION sort4get( SORT4 *s4, S4LONG *recPtr, void **sortData, void **infoPtr )
{
   char *ptr ;
   int rc ;
   #ifdef S464BIT
      S4LONG recl ;
   #endif

   #ifdef E4PARM_HIGH
      if ( s4 == 0 )
         return error4( 0, e4parm_null, E91802 ) ;
   #endif

   if ( error4code( s4->codeBase ) < 0 )
      return e4codeBase ;

   rc = sort4getPtrPtr( s4, &ptr ) ;
   if ( rc < 0 )
      return error4stack( s4->codeBase, (short)rc, E91802 ) ;
   if ( rc )
      return rc ;

   #ifdef S464BIT
      memcpy( (void *)&recl, ptr + s4->sortLen, sizeof(S4LONG) ) ;
      *recPtr = recl ;
   #else
      c4memcpy( (void *)recPtr, ptr + s4->sortLen, sizeof(S4LONG) ) ;
   #endif
   *sortData= (void *)ptr ;
   *infoPtr = (void *)( ptr + s4->infoOffset ) ;

   return 0 ;
}



int sort4getPtrPtr( SORT4 *s4, char **ptrPtr )
{
   if ( s4->pointers != 0 )  /* then no spooling was done */
   {
      if ( s4->pointersI < s4->pointersUsed )
      {
         *ptrPtr = (char *)s4->pointers[s4->pointersI++] ;
         return 0 ;
      }
      if ( sort4free( s4 ) < 0 )
         return -1 ;
      return r4done ;
   }

   if ( s4->spoolsN == 0 )
   {
      if ( sort4free( s4 ) < 0 )
         return -1 ;
      return r4done ;  /* None available */
   }

   if ( s4nextSpoolEntry( s4 ) < 0 )
      return -1 ;

   if ( s4->spoolsN == 0 )
   {
      if ( sort4free( s4 ) < 0 )
         return -1 ;
      return r4done ;
   }
   *ptrPtr = s4->spoolPointer->ptr + s4->spoolPointer->pos ;

   return 0 ;
}



int S4FUNCTION sort4put( SORT4 *s4, const S4LONG rec, const void *sortData, const void *info )
{
   char *ptr ;
   int rc ;
   unsigned ptrMemAvail, newEntries ;

   #ifdef E4PARM_HIGH
      if ( s4 == 0 || rec < 0L || sortData == 0 )
         return error4( 0, e4parm, E91803 ) ;
   #endif

   if ( error4code( s4->codeBase ) < 0 )
      return e4codeBase ;

   if ( s4->pointersUsed >= s4->pointersInit )
   {
      if ( s4->pointersUsed < s4->pointersMax && s4->isMemAvail )
      {
         ptr = (char *)mem4allocNoZero( s4->poolMemory ) ;
         if ( ptr == 0 )
         {
            ptrMemAvail = ( s4->pointersMax - s4->pointersUsed ) * ( sizeof( char * ) ) - ( sizeof(S4LONG) ) ;
            newEntries = ptrMemAvail / ( sizeof( char * ) + s4->totLen ) ;
            s4->pointersMax = s4->pointersUsed + newEntries ;
            sort4initPointers( s4, (char *)( s4->pointers + s4->pointersMax ), newEntries * s4->totLen ) ;
            s4->isMemAvail = 0  ;
         }
         else
         {
            l4add( &s4->pool, ptr ) ;
            s4->poolN++ ;
            sort4initPointers( s4, ptr + sizeof( LINK4 ), s4->codeBase->memSizeSortPool - sizeof( LINK4 ) ) ;
         }
      }
   }

   if ( s4->pointersUsed >= s4->pointersInit )
   {
      rc = s4flush( s4 ) ;
      if ( rc < 0 )
         return error4stack( s4->codeBase, (short)rc, E91803 ) ;
   }

   #ifdef E4ANALYZE
      if ( s4->pointersUsed >= s4->pointersInit )
         return error4( s4->codeBase, e4result, E91803 ) ;
   #endif

   ptr = s4->pointers[s4->pointersUsed++] ;
   c4memcpy( ptr, sortData, s4->sortLen ) ;
   c4memcpy( ptr+ s4->sortLen, (void *)&rec, sizeof( rec ) ) ;
   c4memcpy( ptr+ s4->infoOffset, info, s4->infoLen ) ;

   return 0 ;
}
