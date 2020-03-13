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

/* r4reindx.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4OFF_WRITE
#ifndef S4INDEX_OFF
#ifdef S4CLIPPER

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TURBOC__ */
#endif  /* S4UNIX */

#include "r4reinde.h"

#ifdef P4ARGS_USED
   #pragma argsused
#endif
B4KEY_DATA   *r4key( R4BLOCK_DATA *r4, int i, int keylen)
{
   return (B4KEY_DATA *)( (char *)&r4->nKeys + r4->blockIndex[i] ) ;
}

int S4FUNCTION i4reindex( INDEX4 *i4 )
{
   int rc ;
   TAG4 *tagOn ;
   DATA4 *data ;
   #ifndef S4OPTIMIZE_OFF
      #ifdef S4LOW_MEMORY
         int hasOpt ;
      #endif
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( i4, 0, E92101 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( i4 == 0  )
         return error4( 0, e4parm_null, E92101 ) ;
   #endif

   if ( error4code( i4->codeBase ) < 0 )
      return -1 ;

   data = i4->data ;

   #ifndef S4OPTIMIZE_OFF
      #ifdef S4LOW_MEMORY
         hasOpt = i4->codeBase->hasOpt && i4->codeBase->opt.numBuffers ;
         if ( hasOpt )
            code4optSuspend( i4->codeBase ) ;
      #endif
   #endif

   #ifndef S4SINGLE
      rc = d4lockAllInternal( data, 1 ) ;
      if ( rc )
         return rc ;
   #endif

   #ifndef S4OFF_TRAN
      /* reindex is allowed, but need to fix-up any unique settings */
      i4deleteRemoveKeys( i4 ) ;
   #endif

   // AS Feb 9/06 - added clipper support for packwithstatus
   #ifdef TIME4STATUS
      // AS Jun 30/03 - test d4reindexWithProgress
      REINDEX4STATUS reindexStatus, *reindexStatusPtr ;
      reindexStatusPtr = r4reindexStatusInit( &reindexStatus, i4 ) ;
   #endif

   #ifdef TIME4STATUS
      r4reindexStatusInitDone( reindexStatusPtr ) ;
   #endif

   // AS Mar 29/04 - r4uniqueContinue handling
   int saveRc = 0 ;
   for( tagOn = 0 ;; )
   {
      tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
      if ( tagOn == 0 )
         break ;

      rc = expr4context( tagOn->tagFile->expr, data ) ;
      if ( rc < 0 )
         return rc ;
      if ( tagOn->tagFile->filter != 0 )
      {
         rc = expr4context( tagOn->tagFile->filter, data ) ;
         if ( rc < 0 )
            return rc ;
      }

      rc = t4reindex( tagOn ) ;
      // AS Mar 29/04 - r4uniqueContinue is a special case and is ok
      if ( rc == r4uniqueContinue )
      {
         saveRc = r4uniqueContinue ;
         rc = 0 ;
      }
      if ( rc )
         return rc ;
      #ifdef TIME4STATUS
         r4reindexStatusNextTag( reindexStatusPtr ) ;
      #endif
   }

   #ifdef TIME4STATUS
      r4reindexStatusFinalSet( reindexStatusPtr, .955 ) ;
   #endif

   #ifndef S4OPTIMIZE_OFF
      #ifdef S4LOW_MEMORY
         if ( hasOpt )
            code4optRestart( i4->codeBase ) ;
      #endif
   #endif
   data->recNum = -1 ;
   data->recNumOld = -1 ;
   d4blankLow( data, data->record ) ;
   #ifdef TIME4STATUS
      r4reindexStatusInitUndo( reindexStatusPtr ) ;
   #endif
   return saveRc ;    // AS Mar 29/04 - r4uniqueContinue handling
}



int S4FUNCTION t4reindex( TAG4 *t4 )
{
   R4REINDEX reindex ;
   INDEX4 *i4 ;
   int rc ;
   #if !defined( S4OPTIMIZE_OFF ) && !defined( S4LOW_MEMORY )
      int hasOpt ;
   #endif
   B4KEY_DATA *bdata ;
   int i ;
   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp for HP */
      S4LONG tempNum ;
   #endif

   if ( error4code( t4->tagFile->codeBase ) < 0 )
      return -1 ;

   i4 = t4->index ;

   #if !defined( S4OPTIMIZE_OFF ) && !defined( S4LOW_MEMORY )
      hasOpt = i4->codeBase->hasOpt && i4->codeBase->opt.numBuffers ;
      if ( hasOpt )
         code4optSuspend( i4->codeBase ) ;
   #endif

   #ifndef S4SINGLE
      rc = d4lockIndex( i4->data ) ;
      if ( rc )
         return rc  ;
   #endif

   rc = r4reindexInit( &reindex, t4 ) ;
   if ( rc < 0 )
      return rc ;
   rc = r4reindexTagHeadersCalc( &reindex, t4->tagFile ) ;
   if ( rc == 0 )
      rc = r4reindexBlocksAlloc( &reindex ) ;

   reindex.nBlocksUsed = 0 ;

   if ( rc == 0 )
      rc = r4reindexSupplyKeys( &reindex, t4->tagFile ) ;

   if ( rc < 0 )
   {
      r4reindexFree( &reindex ) ;
      return rc ;
   }

   // AS Mar 26/04 - r4uniqueContinue is ok here
   int saveRc = 0 ;
   if ( rc == 0 )
   {
      rc = r4reindexWriteKeys( &reindex, t4->tagFile, t4unique( t4 ) ) ;
      if ( rc == r4uniqueContinue )
      {
         saveRc = r4uniqueContinue ;
         rc = 0 ;
      }
   }

   if ( rc == 0 )
      rc = r4reindexTagHeadersWrite( &reindex, t4->tagFile ) ;

   if ( rc )
   {
      r4reindexFree( &reindex ) ;
      return rc ;
   }

   // AS Nov 27/03 - turns out to be invalid in multi-user since another user may be shrinking the file
   #if defined( E4MISC ) && defined( S4SINGLE )
      t4->tagFile->checkEof = file4longGetLo( file4lenLow( &t4->tagFile->file ) ) - B4BLOCK_SIZE_INTERNAL ;  /* reset verify eof variable */
   #endif

   t4->index->codeBase->doIndexVerify = 0 ;  /* avoid verify errors due to our partial removal */
   tfile4bottom( t4->tagFile ) ;
   tfile4balance( t4->tagFile, tfile4block( t4->tagFile ), 1 ) ;
   t4->index->codeBase->doIndexVerify = 1 ;

   if ( reindex.stranded )   /* add stranded entry */
   {
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp for HP */
         memcpy( &tempNum, &reindex.stranded->num, sizeof(S4LONG) ) ;
         tfile4add( t4->tagFile, (unsigned char *)reindex.stranded->value, tempNum, t4unique( t4 ) ) ;
      #else
         tfile4add( t4->tagFile, (unsigned char *)reindex.stranded->value, reindex.stranded->num, t4unique( t4 ) ) ;
      #endif
   }

   /* and also add any extra block members */
   if ( reindex.startBlock->nKeys < t4->tagFile->header.keysHalf && reindex.nBlocksUsed > 1 )
   {
      for ( i = 0 ; i < reindex.startBlock->nKeys ; i++ )
      {
         bdata = r4key( reindex.startBlock, i, t4->tagFile->header.keyLen ) ;
         #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp for HP */
            memcpy( &tempNum, &bdata->num, sizeof(S4LONG) ) ;
            tfile4add( t4->tagFile, (unsigned char *)bdata->value, tempNum, t4unique( t4 ) ) ;
         #else
            tfile4add( t4->tagFile, (unsigned char *)bdata->value, bdata->num, t4unique( t4 ) ) ;
         #endif
      }
   }

   tfile4update( t4->tagFile ) ;

   r4reindexFree( &reindex ) ;
   #if !defined( S4OPTIMIZE_OFF ) && !defined( S4LOW_MEMORY )
      if ( hasOpt )
         code4optRestart( i4->codeBase ) ;
   #endif
   return saveRc ;
}

int r4reindexInit( R4REINDEX *r4, TAG4 *t4 )
{
   INDEX4 *i4 ;

   i4 = t4->index ;

   c4memset( r4, 0, sizeof( R4REINDEX ) ) ;

   r4->data = t4->index->data ;
   r4->codeBase = t4->tagFile->codeBase ;

   r4->minKeysmax = INT_MAX ;
   r4->startBlock = 0 ;
   r4->sort.file.hand = INVALID4HANDLE ;

   r4->bufferLen = i4->codeBase->memSizeSortBuffer ;
   if ( r4->bufferLen < 1024 )
      r4->bufferLen = 1024 ;

   r4->buffer = (char *)u4allocEr( r4->codeBase, r4->bufferLen ) ;
   if ( r4->buffer == 0 )
      return e4memory ;

   r4->tag = t4->tagFile ;

   return 0 ;
}



void r4reindexFree( R4REINDEX *r4 )
{
   // AS Apr 7/04 - need to ensure that the seqwrite is flushed to disk before freeing up the used buffer
   file4seqWriteFlush( &r4->seqwrite ) ;
   if ( r4->buffer != 0 )
   {
      u4free( r4->buffer ) ;
      r4->buffer = 0 ;
   }
   if ( r4->startBlock != 0 )
   {
      u4free( r4->startBlock ) ;
      r4->startBlock = 0 ;
   }
   sort4free( &r4->sort ) ;
}



int r4reindexBlocksAlloc( R4REINDEX *r4 )
{
   long onCount ;
   long numSub ;

   #ifdef E4ANALYZE
      if ( (unsigned) r4->minKeysmax > INT_MAX )
         return error4( 0, e4struct, E92102 ) ;
   #endif

   /* Calculate the block stack height */
   onCount = d4recCount( r4->data ) ;

   #ifdef E4MISC
      if ( onCount < 0 )
         return error4( r4->codeBase, (short int)onCount, E92102 ) ;
   #endif

   numSub = r4->minKeysmax ;
   for ( r4->nBlocks = 0 ; onCount > 0L ; r4->nBlocks++ )
   {
      onCount -= numSub ;
      numSub *= r4->minKeysmax ;
   }
   r4->nBlocks ++ ;
   if( r4->nBlocks < 2 )
      r4->nBlocks = 2 ;
   r4->startBlock = (R4BLOCK_DATA *) u4alloc( (long) ( B4BLOCK_SIZE_INTERNAL + 2 * sizeof( void *) ) * r4->nBlocks ) ;

   if ( r4->startBlock == 0 )
      return error4( r4->codeBase, e4memory, E82105 ) ;

   return 0 ;
}

int r4reindexSupplyKeys( R4REINDEX *r4, TAG4FILE *t4 )
{
   FILE4SEQ_READ seqRead ;
   EXPR4 *filter ;
   char *keyResult ;
   int rc, *filterResult ;
   long count, iRec ;
   DATA4 *d4 ;
   DATA4FILE *d4file ;
   #ifndef S4MEMO_OFF
      int i ;
   #endif

   d4 = r4->data ;
   d4file = d4->dataFile ;
   #ifdef E4MISC
      r4->keyCount = 0L ;
   #endif

   if ( sort4init( &r4->sort, r4->codeBase, t4->header.keyLen, 0 ) < 0 )
      return -1 ;
   r4->sort.cmp = t4->cmp ;

   rc = expr4context( r4->tag->expr, d4 ) ;
   if ( rc < 0 )
      return rc ;
   if ( r4->tag->filter != 0 )
   {
      rc = expr4context( r4->tag->filter, d4 ) ;
      if ( rc < 0 )
         return rc ;
   }

   filter = t4->filter ;
   count = dfile4recCount( d4file, -2L ) ;
   if ( count < 0 )
      return error4stack( r4->codeBase, (short)rc, E92102 ) ;

   file4seqReadInitDo( &seqRead, &d4file->file, dfile4recordPosition( d4file, 1L ), r4->buffer, r4->bufferLen, 1 ) ;

   for ( iRec = 1L; iRec <= count; iRec++ )
   {
      if ( file4seqReadAll( &seqRead, d4->record, dfile4recWidth( d4file ) ) < 0 )
         return -1 ;
      d4->recNum = iRec ;

      #ifndef S4MEMO_OFF
         for ( i = 0; i < d4file->nFieldsMemo; i++ )
            f4memoReset( d4->fieldsMemo[i].field ) ;
      #endif

      if ( filter )
      {
         rc = expr4vary( filter, (char **)&filterResult ) ;
         if ( rc < 0 )
         {
            #ifdef S4ADVANCE_READ
               file4seqReadInitUndo( &seqRead ) ;
            #endif
            return error4stack( r4->codeBase, (short)rc, E92102 ) ;
         }
         #ifdef E4MISC
            if ( expr4type( filter ) != r4log )
            {
               #ifdef S4ADVANCE_READ
                  file4seqReadInitUndo( &seqRead ) ;
               #endif
               return error4( r4->codeBase, e4result, E92102 ) ;
            }
         #endif  /* E4MISC */
         if ( ! *filterResult )
            continue ;
         t4->hasKeys = 1 ;
         #ifdef S4MDX
            t4->hadKeys = 0 ;
         #endif
      }

      tfile4exprKey( t4, (unsigned char **)&keyResult ) ;

      if ( sort4put( &r4->sort, iRec, keyResult, "" ) < 0)
      {
         #ifdef S4ADVANCE_READ
            file4seqReadInitUndo( &seqRead ) ;
         #endif
         return -1 ;
      }
      #ifdef E4MISC
         r4->keyCount++ ;
      #endif
      // AS Feb 9/06 - added clipper support for packwithstatus
      #if defined( TIME4STATUS ) && !defined( S4OFF_THREAD )
         InterlockedIncrement( &(r4->codeBase->incrementVal) ) ;
      #endif
   }

   #ifdef S4ADVANCE_READ
      file4seqReadInitUndo( &seqRead ) ;
   #endif

   return 0 ;
}

int  r4reindexTagHeadersCalc( R4REINDEX *r4, TAG4FILE *t4 )
{
   int rc, exprType ;

   if ( tfile4freeAll( t4 ) < 0 )
      return -1 ;

   t4->header.keyLen = t4->expr->keyLen = expr4keyLen( t4->expr ) ;
   t4->expr->keyDec = t4->header.keyDec ;
   if( t4->header.keyLen < 0 )
      return -1 ;


   exprType = expr4type( t4->expr ) ;
   if ( exprType < 0 )
      return exprType ;
   rc = tfile4initSeekConv( t4, exprType ) ;
   if ( rc < 0 )
      return rc ;

   t4->header.groupLen = t4->header.keyLen + 8 ;

   /* FOR S4CLIPPER compatibility:  if keyLen = 194, groupLen = 202, and keysHalf = 2, keyLen
      > 194, keysHalf = 1.

      S4CLIPPER keyLen ranges:  (groupLen = keyLen+8)
      195-338 : keysHalf = 1, keysMax = 2
      136-194 : keysHalf = 2, keysMax = 4
         -135 : keysHalf = 3, keysMax = 6

   */
   r4->keysHalf = t4->header.keysHalf = ( 1020 / ( t4->header.groupLen + 2 ) - 1)/ 2;
   if ( r4->keysHalf == 0 && t4->header.groupLen <= (I4MAX_KEY_SIZE + 8))  /* formula doesn't work for large key sizes, work around... for valid group len <= 346) */
      r4->keysHalf = t4->header.keysHalf = 1 ;
   // AS 05/12/99 -> if there is a filter included, the sign should be set to 7...(clipper compatibility issue)
   if ( t4->filter != 0 )
      t4->header.sign = 7 ;
   else
      t4->header.sign = 6 ;
   t4->header.keysMax = t4->header.keysHalf * 2 ;
   if ( t4->header.keysMax < 2 )
      return error4( t4->codeBase, e4index, E82102 ) ;   // probably key size just not large enough... (82102)

   if ( t4->header.keysMax < r4->minKeysmax )
      r4->minKeysmax = t4->header.keysMax ;

   r4->lastblockInc = B4BLOCK_SIZE_INTERNAL / 512 ;
   r4->lastblock = 0 ;

   return 0 ;
}

int r4reindexTagHeadersWrite( R4REINDEX *r4, TAG4FILE *t4 )
{
   FILE4LONG pos ;

   t4->header.eof = 0 ;

   if ( tfile4writeHeader( t4, writeEntireHeader ) < 0 )
      return -1 ;

   file4longAssign( pos, (r4->lastblock + r4->lastblockInc) * 512, 0 ) ;
   file4lenSetLow( &t4->file, pos ) ;
   return 0 ;
}

int r4reindexWriteKeys( R4REINDEX *r4, TAG4FILE *t4, short int errUnique )
{
   char  lastKey[I4MAX_KEY_SIZE] ;
   unsigned char *keyData ;
   int   isUnique, rc, isFirst ;
   void *dummyPtr ;
   S4LONG keyRec ;  /* LY 00/04/13 : 64-bit HP-UX */
   FILE4LONG pos ;

   r4->grouplen = t4->header.groupLen ;
   r4->valuelen = t4->header.keyLen ;
   r4->keysmax = t4->header.keysMax ;

   c4memset( r4->startBlock, 0, (int)(( (long)B4BLOCK_SIZE_INTERNAL + 2 * sizeof( void *) ) * r4->nBlocks) ) ;

   if ( sort4getInit( &r4->sort ) < 0 )
      return -1 ;

   file4longAssign( pos, ( r4->lastblock + r4->lastblockInc ) * 512, 0 ) ;
   file4seqWriteInitLow( &r4->seqwrite, &t4->file, pos, r4->buffer, r4->bufferLen ) ;

   #ifdef E4MISC
      if ( I4MAX_KEY_SIZE < r4->sort.sortLen )
         return error4( r4->codeBase, e4index, E82102 ) ;
   #endif

   c4memset( lastKey, 0, sizeof(lastKey) ) ;
   isUnique = t4->header.unique ;

   isFirst = 1 ;

   // AS Mar 26/04 - improved return code handling
   int rcSave = 0 ;

   for(;;)  /* For each key to write */
   {
      // AS Feb 9/06 - added clipper support for packwithstatus
      #ifdef TIME4STATUS
         InterlockedIncrement( &(r4->codeBase->incrementVal) ) ;
      #endif
      if ( (rc = sort4get( &r4->sort, &keyRec, (void **) &keyData, &dummyPtr)) < 0)
         return -1 ;

      #ifdef E4MISC
         if ( r4->keyCount < 0L  ||  r4->keyCount == 0L && rc != r4done ||  r4->keyCount > 0L && rc == r4done )
            return error4( r4->codeBase, e4info, E92102 ) ;
         r4->keyCount-- ;
      #endif

      if ( rc == r4done )  /* No more keys */
      {
         if ( r4reindexFinish( r4 ) < 0 )
            return -1 ;
         if ( file4seqWriteFlush( &r4->seqwrite ) < 0 )
            return -1 ;
         break ;
      }

      if ( isUnique )
      {
         if( isFirst )
            isFirst = 0 ;
         else
            if ( (*t4->cmp)( keyData, lastKey, r4->sort.sortLen) == 0 )
            {
               switch( errUnique )
               {
                  case e4unique:
                     return error4describe( r4->codeBase, e4unique, E82103, t4->alias, (char *)0, 0 ) ;
                  case r4unique:
                     return r4unique ;
                  // AS Mar 26/04 - need to ensure r4uniqueContinue eventually returned in this case
                  case r4uniqueContinue:
                     rcSave = r4uniqueContinue ;
                     continue ;
                  default:
                     continue ;
               }
            }

         c4memcpy( lastKey, keyData, r4->sort.sortLen ) ;
      }

      /* Add the key */
      rc = r4reindexAdd( r4, keyRec, keyData) ;  // AS Mar 29/04 - clean up error and return handling
      if ( rc < 0 )
         return rc ;
   }

   /* Now complete the tag header info. */
   t4->header.root = r4->lastblock * 512 ;

   return rcSave ;  // AS Mar 29/04 - clean up error and return handling
}



static int r4reindexToDisk( R4REINDEX *r4, long rec, const char *keyValue )
{
   /* Writes out the current block and adds references to higher blocks */
   R4BLOCK_DATA *block = r4->startBlock ;
   int iBlock= 0 ;
   #ifdef S4DATA_ALIGN
      S4LONG tempLong ;  /* LY 00/02/17 : t4seek.cpp for HP */
   #endif

   #ifdef E4MISC
      B4KEY_DATA *keyOn = r4key( block, block->nKeys, r4->grouplen ) ;
      long dif = (char *) keyOn -  (char *) &block->nKeys ;
      if ( dif+ r4->grouplen > B4BLOCK_SIZE_INTERNAL || dif < 0 )
         return error4( r4->codeBase, e4result, E92102 ) ;
   #endif

   for( unsigned long tnUsed = 2 ;; tnUsed++ )
   {
      #ifdef S4BYTE_SWAP
         char *swap = (char *)u4allocEr( r4->codeBase, B4BLOCK_SIZE_INTERNAL ) ;
         if ( swap == 0 )
            return -1 ;

         memcpy( (void *)swap, (void *)&block->nKeys, B4BLOCK_SIZE_INTERNAL ) ;

         index4swapBlockClipper( swap, r4->keysmax, r4->grouplen) ;

         if ( file4seqWrite( &r4->seqwrite, swap, B4BLOCK_SIZE_INTERNAL) < 0 )
            return -1 ;
         u4free( swap ) ;
      #else
         if ( file4seqWrite( &r4->seqwrite, &block->nKeys, B4BLOCK_SIZE_INTERNAL) < 0 )
            return -1 ;
      #endif
      if ( iBlock )
         c4memset( block, 0, B4BLOCK_SIZE_INTERNAL ) ;
      r4->lastblock += r4->lastblockInc ;

      block = (R4BLOCK_DATA *) ((char *)block + B4BLOCK_SIZE_INTERNAL + 2*sizeof(void *) ) ;
      iBlock++ ;
      #ifdef E4MISC
         if ( iBlock >= r4->nBlocks )
            return error4( r4->codeBase, e4info, E92102 ) ;
      #endif

      if ( block->nKeys == 0 )   /* set up the branch block... */
      {
         short offset = ( r4->keysmax + 2 + ( ( r4->keysmax / 2 ) * 2 != r4->keysmax ) ) * sizeof(short) ;
         block->blockIndex = &block->nKeys + 1 ;
         for ( int i = 0 ; i <= r4->keysmax ; i++ )
            block->blockIndex[i] = r4->grouplen * i + offset ;
         block->data = (char *) &block->nKeys + block->blockIndex[ 0 ] ;
      }

      B4KEY_DATA *keyTo = r4key( block, block->nKeys, r4->grouplen ) ;
      #ifdef E4MISC
         dif = (char *) keyTo -  (char *) block  ;
         if ( dif+sizeof(long) > B4BLOCK_SIZE_INTERNAL || dif < 0 )
            return error4( r4->codeBase, e4result, E92102 ) ;
      #endif
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp for HP */
         tempLong = r4->lastblock * 512 ;
         memcpy( &keyTo->pointer, &tempLong, sizeof(S4LONG) ) ;
      #else
         keyTo->pointer = r4->lastblock * 512 ;
      #endif

      if ( block->nKeys < r4->keysmax )
      {
         if ( tnUsed > r4->nBlocksUsed )
            r4->nBlocksUsed = tnUsed ;
         #ifdef E4MISC
            if ( dif+r4->grouplen > B4BLOCK_SIZE_INTERNAL )
               return error4( r4->codeBase, e4result, E92102 ) ;
         #endif
         keyTo->num = rec ;
         c4memcpy( keyTo->value, keyValue, r4->valuelen ) ;
         block->nKeys++ ;
         return 0 ;
      }
      #ifdef E4MISC
         if ( block->nKeys > r4->keysmax )
             return error4( r4->codeBase, e4info, E82104 ) ;
      #endif
   }
}



/* LY 00/04/13 : long rec to S4LONG for 64-bit HP-UX */
int r4reindexAdd( R4REINDEX *r4, const S4LONG rec, const unsigned char *keyValue )
{
   B4KEY_DATA *keyTo ;
   R4BLOCK_DATA *startBlock ;
   #ifdef E4MISC
      long  dif ;
   #endif
   short offset ;
   int i ;

   startBlock = r4->startBlock ;

   /* for NTX, if keysmax, then todisk() with the latest value... */

   if ( startBlock->nKeys == 0 )   /* first, so add references */
   {
      offset = ( r4->keysmax + 2 + ( ( r4->keysmax / 2 ) * 2 != r4->keysmax ) ) * sizeof(short) ;
      startBlock->blockIndex = &startBlock->nKeys + 1 ;  /* 1 short off of nKeys */
      for ( i = 0 ; i <= r4->keysmax ; i++ )
          startBlock->blockIndex[i] = r4->grouplen * i + offset ;
      startBlock->data = (char *)&startBlock->nKeys + startBlock->blockIndex[0] ;  /* first entry */
   }
   if ( startBlock->nKeys >= r4->keysmax )
   {
      if ( r4reindexToDisk( r4, rec, (const char*)keyValue ) < 0 )
         return -1 ;
      c4memset( startBlock, 0, B4BLOCK_SIZE_INTERNAL + 2 * sizeof( void *) ) ;
      return 0 ;
   }

   keyTo = r4key( startBlock, startBlock->nKeys++, r4->grouplen ) ;

   #ifdef E4MISC
      dif = (char *)keyTo -  (char *)startBlock ;
      if ( dif + r4->grouplen > B4BLOCK_SIZE_INTERNAL || dif < 0 )
         return error4( r4->codeBase, e4result, E92102 ) ;
   #endif
   keyTo->num = rec ;
   c4memcpy( keyTo->value, keyValue, r4->valuelen ) ;

   return 0 ;
}

int r4reindexFinish( R4REINDEX *r4 )
{
   B4KEY_DATA *keyTo ;
   #ifdef E4MISC
      long dif ;
   #endif
   unsigned long iBlock = 0 ;
   unsigned long tBlock ;

   #ifdef S4BYTE_SWAP
      char *swap, *swapPtr ;
      int j ;
      long longVal ;
      short shortVal ;
   #endif
   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp for HP */
      S4LONG tempLong ;
   #endif

   R4BLOCK_DATA *block = r4->startBlock, *temp_block ;

   short offset ;
   int i ;
   unsigned long pointer ;

   if ( r4->nBlocksUsed <= 1 )  /* empty database if nKeys = 0 */
   {
      if ( r4->startBlock->nKeys == 0 )   /* first, so add references */
      {
         offset = ( r4->keysmax + 2 + ( (r4->keysmax/2)*2 != r4->keysmax ) ) * sizeof(short) ;
         r4->startBlock->blockIndex = &r4->startBlock->nKeys + 1 ;  /* 1 short off of nKeys */
         for ( i = 0 ; i <= r4->keysmax ; i++ )
             r4->startBlock->blockIndex[i] = r4->grouplen * i + offset ;
         r4->startBlock->data = (char *) &r4->startBlock->nKeys + r4->startBlock->blockIndex[0] ;
      }
      iBlock ++ ;
      r4->stranded = 0 ;
      pointer = 0 ;

      #ifdef S4BYTE_SWAP
         swap = (char *)u4allocEr( r4->codeBase, B4BLOCK_SIZE_INTERNAL ) ;
         if ( swap == 0 )
            return error4describe( r4->codeBase, e4memory, 0, 0, 0, 0 ) ;

         memcpy( (void *)swap, (void *)&r4->startBlock->nKeys, B4BLOCK_SIZE_INTERNAL ) ;
                          /* position swapPtr at beginning of pointers */

         index4swapBlockClipper(swap, r4->keysmax, r4->grouplen) ;

         if ( file4seqWrite( &r4->seqwrite, swap, B4BLOCK_SIZE_INTERNAL) < 0 )
            return -1 ;
         u4free( swap ) ;
      #else
         if ( file4seqWrite( &r4->seqwrite, &r4->startBlock->nKeys, B4BLOCK_SIZE_INTERNAL) < 0 )
            return -1 ;
      #endif

      r4->lastblock += r4->lastblockInc ;
   }
   else if ( r4->startBlock->nKeys >= r4->keysHalf )
   {
      /* just grab the pointer for upward placement where belongs */
      r4->stranded = 0 ;

      #ifdef S4BYTE_SWAP
         swap = (char *) u4allocEr( r4->codeBase, B4BLOCK_SIZE_INTERNAL ) ;
         if ( swap == 0 )
            return -1 ;

         memcpy( (void *)swap, (void *)&r4->startBlock->nKeys, B4BLOCK_SIZE_INTERNAL ) ;

         index4swapBlockClipper(swap, r4->keysmax, r4->grouplen) ;

         if ( file4seqWrite( &r4->seqwrite, swap, B4BLOCK_SIZE_INTERNAL) < 0 )
            return -1 ;
         u4free( swap ) ;
      #else
         if ( file4seqWrite( &r4->seqwrite, &r4->startBlock->nKeys, B4BLOCK_SIZE_INTERNAL) < 0 )
            return -1 ;
      #endif

      r4->lastblock += r4->lastblockInc ;
      pointer = r4->lastblock*512 ;
      block = (R4BLOCK_DATA *) ((char *)block + B4BLOCK_SIZE_INTERNAL + 2*sizeof(void *) ) ;
      iBlock++ ;
   }
   else       /* stranded entry, so add after */
   {
      /* if less than 1/2 entries, will re-add the required keys later... */
      block = (R4BLOCK_DATA *) ((char *)block + B4BLOCK_SIZE_INTERNAL + 2*sizeof(void *) ) ;
      iBlock++ ;
      while( block->nKeys == 0 && iBlock < r4->nBlocksUsed )
      {
         block = (R4BLOCK_DATA *) ((char *)block + B4BLOCK_SIZE_INTERNAL + 2*sizeof(void *) ) ;
         iBlock++ ;
      }

      r4->stranded = r4key( block, block->nKeys - 1, 0 ) ;
      block->nKeys -- ;
      if( block->nKeys > 0 )
      {
         #ifdef S4BYTE_SWAP
            swap = (char *)u4allocEr( r4->codeBase, B4BLOCK_SIZE_INTERNAL ) ;
            if ( swap == 0 )
               return -1 ;

            memcpy( (void *)swap, (void *)&block->nKeys, B4BLOCK_SIZE_INTERNAL ) ;

            index4swapBlockClipper(swap, r4->keysmax, r4->grouplen ) ;

            if ( file4seqWrite( &r4->seqwrite, swap, B4BLOCK_SIZE_INTERNAL) < 0 )
               return -1 ;
            u4free( swap ) ;
         #else
            if ( file4seqWrite( &r4->seqwrite, &block->nKeys, B4BLOCK_SIZE_INTERNAL) < 0 )
               return -1 ;
         #endif

         r4->lastblock += r4->lastblockInc ;
         pointer = 0 ;
      }
      else
         pointer = r4key( block, block->nKeys, 0 )->pointer ;
      block = (R4BLOCK_DATA *) ((char *)block + B4BLOCK_SIZE_INTERNAL + 2*sizeof(void *) ) ;
      iBlock++ ;
   }

   /* now position to the last spot, and place the branch */
   if( iBlock < r4->nBlocksUsed )
   {
      if( block->nKeys <= r4->keysmax && pointer != 0 )
      {
         temp_block = block ;
         tBlock = iBlock ;
         while ( temp_block->nKeys == 0 && tBlock < r4->nBlocksUsed )
         {
            offset = ( r4->keysmax + 2 + ( ( r4->keysmax / 2 ) * 2 != r4->keysmax ) ) * sizeof(short) ;
            temp_block->blockIndex = &temp_block->nKeys + 1 ;  /* 1 short off of nKeys */
            for ( i = 0 ; i <= r4->keysmax ; i++ )
               temp_block->blockIndex[i] = r4->grouplen * i + offset ;
            temp_block->data = (char *)&temp_block->nKeys + temp_block->blockIndex[0] ;
            temp_block = (R4BLOCK_DATA *)((char *)temp_block + B4BLOCK_SIZE_INTERNAL + 2 * sizeof(void *) ) ;
            tBlock++ ;
         }

         /* now place the pointer for data that goes rightward */
         keyTo = r4key( block, block->nKeys, 0 ) ;
         keyTo->pointer = pointer ;
         pointer = 0 ;

         #ifdef S4BYTE_SWAP
            swap = (char *) u4allocEr( r4->codeBase, B4BLOCK_SIZE_INTERNAL ) ;
            if ( swap == 0 )
               return -1 ;

            memcpy( (void *)swap, (void *)&block->nKeys, B4BLOCK_SIZE_INTERNAL ) ;

            index4swapBlockClipper(swap, r4->keysmax, r4->grouplen) ;

            if ( file4seqWrite( &r4->seqwrite, swap, B4BLOCK_SIZE_INTERNAL) < 0 )
               return -1 ;
            u4free( swap ) ;
         #else
            if ( file4seqWrite( &r4->seqwrite, &block->nKeys, B4BLOCK_SIZE_INTERNAL) < 0 ) return -1 ;
         #endif

         r4->lastblock += r4->lastblockInc ;
         block = (R4BLOCK_DATA *) ((char *)block + B4BLOCK_SIZE_INTERNAL + 2*sizeof(void *) ) ;
         iBlock++ ;
      }
   }
   for(; iBlock < r4->nBlocksUsed; iBlock++ )
   {
      if ( block->nKeys == 0 )
      {
         offset = ( r4->keysmax + 2 + ( ( r4->keysmax / 2 ) * 2 != r4->keysmax ) ) * sizeof(short) ;
         block->blockIndex = &block->nKeys + 1 ;  /* 1 short off of nKeys */
         for ( i = 0 ; i <= r4->keysmax ; i++ )
            block->blockIndex[i] = r4->grouplen * i + offset ;
         block->data = (char *)&block->nKeys + block->blockIndex[0] ;
      }
      keyTo = r4key( block, block->nKeys, r4->grouplen ) ;
      #ifdef E4MISC
         dif = (char *)keyTo  -  (char *) block ;
         if ( dif + sizeof( long ) > B4BLOCK_SIZE_INTERNAL  ||  dif < 0 )
            return error4( r4->codeBase, e4result, E92102 ) ;
      #endif
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp for HP */
         tempLong = r4->lastblock * 512 ;
         memcpy( &keyTo->pointer, &tempLong, sizeof(S4LONG) ) ;
      #else
         keyTo->pointer = r4->lastblock * 512 ;
      #endif

      #ifdef S4BYTE_SWAP
         swap = (char *)u4allocEr( r4->codeBase, B4BLOCK_SIZE_INTERNAL ) ;
         if ( swap == 0 )
            return -1 ;

         memcpy( (void *)swap, (void *)&block->nKeys, B4BLOCK_SIZE_INTERNAL ) ;

         index4swapBlockClipper(swap, r4->keysmax, r4->grouplen) ;

         if ( file4seqWrite( &r4->seqwrite, swap, B4BLOCK_SIZE_INTERNAL) < 0 )
            return -1 ;
         u4free( swap ) ;
      #else
         if ( file4seqWrite( &r4->seqwrite, &block->nKeys, B4BLOCK_SIZE_INTERNAL) < 0)
            return -1;
      #endif

      r4->lastblock += r4->lastblockInc ;
      block = (R4BLOCK_DATA *)( (char *)block + B4BLOCK_SIZE_INTERNAL + 2 * sizeof(void *) ) ;
   }

   return 0 ;
}

#else  /* S4CLIPPER */

// used in index independent ole-db, so must always exist...
int S4FUNCTION t4reindex( TAG4 *t4 )
{
   return error4( 0, e4notSupported, E92102 ) ;
}


#endif  /* S4CLIPPER */

#endif  /* S4INDEX_OFF */
#endif  /* S4WRITE_OFF */
