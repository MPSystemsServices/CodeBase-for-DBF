/* b4block.c  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */
/* forced rebuild mod */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TURBOC__ */

#ifdef __BORLANDC__
   #if __BORLANDC__ < 0x500 && !defined(S4STAND_ALONE) && !defined(S4EXE_BUILD)
      #error Client/Server library build not supported with Borland C++ prior to version 5
   #endif
#endif

#ifdef _MSC_VER
   #if _MSC_VER < 1000 && !defined(S4STAND_ALONE) && !defined(S4EXE_BUILD)
      #error Client/Server library build not supported with Visual C++ prior to version 4
   #endif
#endif

#ifdef I4PRINT
   #include <sys\timeb.h>
   #include <time.h>
#endif

short S4FUNCTION x4reverseShort( const void *val )
{
   unsigned char out[2] ;
   const unsigned char *atVal ;

   atVal = (const unsigned char *)val ;

   #ifdef E4PARM_LOW
      if ( val == 0 )
      {
         error4( 0, e4parm, E90439 ) ;
         return 0 ;
      }
   #endif

   out[0] = atVal[1] ;
   out[1] = atVal[0] ;

   return (*(short *)out ) ;
}

S4LONG S4FUNCTION x4reverseLong( const void *val )
{
   unsigned char out[4] ;

   unsigned const char *atVal ;
   atVal = (const unsigned char *)val ;

   #ifdef E4PARM_LOW
      if ( val == 0 )
      {
         error4( 0, e4parm, E90439 ) ;
         return 0 ;
      }
   #endif

   #ifdef S4BYTEORDER_2301
      out[0] = atVal[2] ;
      out[1] = atVal[3] ;
      out[2] = atVal[0] ;
      out[3] = atVal[1] ;
   #else
      out[0] = atVal[3] ;
      out[1] = atVal[2] ;
      out[2] = atVal[1] ;
      out[3] = atVal[0] ;
   #endif

   return *(S4LONG *)out ;
}

#ifndef S4NO_LONGLONG
   LONGLONG S4FUNCTION x4reverseLongLong( const void *val )
   {
      unsigned char out[8] ;

      unsigned const char *atVal ;
      atVal = (const unsigned char *)val ;

      #ifdef E4PARM_LOW
         if ( val == 0 )
         {
            error4( 0, e4parm, E90439 ) ;
            return 0 ;
         }
      #endif

      out[0] = atVal[7] ;
      out[1] = atVal[6] ;
      out[2] = atVal[5] ;
      out[3] = atVal[4] ;
      out[4] = atVal[3] ;
      out[5] = atVal[2] ;
      out[6] = atVal[1] ;
      out[7] = atVal[0] ;

      return *(LONGLONG *)out ;
   }
#endif  // S4WIN32

double S4FUNCTION x4reverseDouble( const void *val )
{
   char returnValue[8] ;
   char *from, *to ;

   to = returnValue ;
   from = ((char *)val)+8 ;
   *to++ = *--from ;
   *to++ = *--from ;
   *to++ = *--from ;
   *to++ = *--from ;
   *to++ = *--from ;
   *to++ = *--from ;
   *to++ = *--from ;
   *to = *--from ;
   return (*(double *)returnValue) ;
}


#ifndef S4CLIENT
   int b4calcBlanks( const unsigned char *keyVal, const int len, const unsigned char pChar )
   {
      int a ;

      #ifdef E4PARM_LOW
         if ( keyVal == 0 || len < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      for ( a = len ; a > 0; a-- )
         if ( keyVal[a-1] != pChar )
            return ( len - a ) ;
      return len ;  /* all blanks */
   }
#endif /* S4CLIENT */

#if !defined(S4INDEX_OFF) && !defined(S4CLIENT)
   B4BLOCK *b4alloc( TAG4FILE *t4, const B4NODE fb )
   {
      B4BLOCK *b4 ;
      #ifdef S4CLIPPER
         short offset ;
         int i ;
      #endif  /* S4CLIPPER */

      #ifdef E4PARM_LOW
         if ( t4 == 0 )
         {
            error4( 0, e4parm, E90438 ) ;
            return 0 ;
         }
      #endif

      /* need to zero out the memory because callers assume they don't need to initialize stuff */
      #ifdef S4CLIPPER
         b4 = (B4BLOCK *)mem4allocErrZero( t4->blockMemory, t4->codeBase ) ;
      #else
         b4 = (B4BLOCK *)mem4allocErrZero( t4->indexFile->blockMemory, t4->codeBase ) ;
      #endif
      if ( b4 == 0 )
      {
         error4( t4->codeBase, e4memory, E90438 ) ;
         return 0 ;
      }

      b4->tag = t4 ;
      b4->fileBlock = fb ;

      #ifdef S4CLIPPER
         offset = ( b4->tag->header.keysMax + 2 +
            (( b4->tag->header.keysMax / 2) * 2 != b4->tag->header.keysMax ) )
            * sizeof(short) ;
         for ( i = 0 ; i <= b4->tag->header.keysMax ; i++ )
            b4->pointers[i] = (short)(( b4->tag->header.groupLen * i )) + offset ;
         b4->data = (B4KEY_DATA *) ((char *)&b4->nKeys + b4->pointers[0]) ;  /* first entry */
      #endif  /* S4CLIPPER */

      #ifdef S4FOX
         b4->builtKey = (B4KEY_DATA *)mem4allocErrNoZero( t4->builtKeyMemory, t4->codeBase ) ;
         b4->builtOn = -1 ;
      #endif  /* S4FOX */

      return b4 ;
   }



   int b4free( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      #ifdef S4FOX
         #ifdef E4ANALYZE
            if ( b4->changed )
               return error4( b4->tag->codeBase, e4info, E90438 ) ;
         #endif
         mem4free( b4->tag->builtKeyMemory, b4->builtKey ) ;
      #endif /* S4FOX */

      #ifdef S4CLIPPER
         mem4free( b4->tag->blockMemory, b4 ) ;
      #else
         mem4free( b4->tag->indexFile->blockMemory, b4 ) ;
      #endif
      return 0 ;
   }



   #ifdef S4MDX

   /* S4MDX */
   #ifndef S4OFF_WRITE
   int b4doFlush( B4BLOCK *b4 )
   {
      int rc ;
      INDEX4FILE *i4 ;
      TAG4FILE *t4file ;
      FILE4LONG fPos ;
      #ifdef S4BYTE_SWAP
         char *swap, *swapPtr ;
         int i ;
         S4LONG longVal ;
         short shortVal ;
      #endif  /* S4BYTE_SWAP */

      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      assert5( b4->changed ) ;   /* this function only gets called by b4flush() which checks this flag */

      #ifdef E4INDEX_VERIFY
         if ( b4verify( b4 ) == -1 )
            error4describe( b4->tag->codeBase, e4index, E90438, b4->tag->alias, 0, 0 ) ;
      #endif
      t4file = b4->tag ;
      i4 = t4file->indexFile ;
      file4longAssign( fPos, b4->fileBlock, 0 ) ;
      file4longMultiply( fPos, I4MULTIPLY ) ;

      #ifdef I4PRINT_BLOCK
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( t4file->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            struct _timeb mTime ;
            char *outTime, dump[255] ;
            _ftime( &mTime ) ;
            outTime = ctime( &( mTime.time ) ) ;

            B4KEY_DATA *keyData = b4key( b4, 0 ) ;
            sprintf( dump, "b4flush - 1st entry: i4 = %ld, fileBlock: %uld, recNo = %ld, key = %s, time: %s\r\n", (long)b4->tag->indexFile, (unsigned long)b4->fileBlock, (long)keyData->num, (char *)keyData->value, outTime ) ;
            log5( dump ) ;
         }
      #endif

      #ifdef S4BYTE_SWAP
         swap = (char *)u4allocEr( t4file->codeBase, t4file->indexFile->header.blockRw ) ;
         if ( swap == 0 )
            return error4stack( t4file->codeBase, e4memory, E90438 ) ;

         c4memcpy( (void *)swap, (void *)&b4->nKeys, t4file->indexFile->header.blockRw ) ;

         /* position swapPtr at beginning of B4KEY's */
         swapPtr = swap ;
         swapPtr += 6 + sizeof(short) ;

         /* move through all B4KEY's to swap 'long' */
         for ( i = 0 ; i < (*(short *)swap) ; i++ )
         {
            longVal = x4reverseLong( (void *)swapPtr ) ;
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
            swapPtr += t4file->header.groupLen ;
         }

         /* mark lastPointer */
         if ( !b4leaf( b4 ) )
         {
            longVal = x4reverseLong( (void *)swapPtr ) ;
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
         }

         /* swap the numKeys value */
         shortVal = x4reverseShort( (void *)swap ) ;
         c4memcpy( swap, (void *) &shortVal, sizeof(short) ) ;

         #ifndef S4OFF_OPTIMIZE
            i4->readBlockTag = t4file ;
         #endif

         rc = file4writeInternal( &i4->file, fPos, swap, i4->header.blockRw ) ;
         #ifndef S4OFF_OPTIMIZE
            i4->readBlockTag = 0 ;
         #endif
         u4free( swap ) ;
      #else
         #ifndef S4OFF_OPTIMIZE
            i4->readBlockTag = t4file ;
         #endif
         rc = file4writeInternal( &i4->file, fPos, &b4->nKeys, i4->header.blockRw ) ;
         #ifndef S4OFF_OPTIMIZE
            i4->readBlockTag = 0 ;
         #endif
      #endif  /* S4BYTE_SWAP */

      if ( rc < 0 )
         return error4stack( t4file->codeBase, rc, E90438 ) ;
      b4->changed = 0 ;
      return 0 ;
   }
   #endif /* S4OFF_WRITE */



   #ifndef S4INLINE
   /* S4MDX */
   void b4goEof( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
         {
            error4( 0, e4parm_null, E90438 ) ;
            return ;
         }
      #endif

      b4->keyOn = b4numKeys( b4 ) ;
   }
   #endif /* S4INLINE */



   #ifndef S4OFF_WRITE
      /* S4MDX */
      int b4insert( B4BLOCK *b4, const void *k, const unsigned long r )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || k == 0 || r == 0 || r == INVALID4BLOCK_ID )
               return error4( b4->tag->codeBase, e4parm, E90438 ) ;
         #endif

         B4KEY_DATA *dataPtr = b4key( b4, b4->keyOn ) ;
         B4KEY_DATA *nextPtr = b4key( b4, b4->keyOn+1 ) ;
         int leftLen = b4->tag->indexFile->header.blockRw - ( b4->keyOn + 1 ) * b4->tag->header.groupLen - sizeof(short) - sizeof(char[6]) ;

         #ifdef E4ANALYZE
            if ( b4->keyOn < 0 || b4->keyOn > b4numKeys( b4 ) || leftLen < 0 )
               return error4( b4->tag->codeBase, e4info, E90438 ) ;
         #endif

         c4memmove( nextPtr, dataPtr, leftLen ) ;
         b4->nKeys++ ;
         c4memcpy( dataPtr->value, k, b4->tag->header.keyLen ) ;
         #ifdef S464BIT
            S4LONG tempR = (S4LONG) r ;
            c4memcpy( (void *)&dataPtr->num, (void *)&tempR, sizeof(tempR) ) ;
         #else
            c4memcpy( (void *)&dataPtr->num, (void *)&r, sizeof(r) ) ;
         #endif
         b4->changed = 1 ;

         return 0 ;
      }
   #endif /* S4OFF_WRITE */

   /* S4MDX */
   /* based on the actual stored-data (from file) only, determine whether or
      not a leaf.  Used by file optimization routines */
   int b4dataLeaf( void *data, TAG4FILE *tag )
   {
      short nKeys ;
      char *info ;

      #ifdef E4PARM_LOW
         if ( data == 0 || tag == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      nKeys = * ((short *)data) ;
      // AS Pct 28/03 - verify valid if debug mode
      assert5( nKeys <= tag->header.keysMax ) ;
      info = (char *)data + ( sizeof( short ) + 6 ) ;

      return ( ((B4KEY_DATA *)((char *)info + tag->header.groupLen * nKeys))->num == 0L ) ;
   }

   /* S4MDX */
   #ifndef S4INLINE
      /* AS Nov 13/02 - export for dot */
      B4KEY_DATA *S4FUNCTION b4key( const B4BLOCK *b4, const int iKey )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || iKey < 0 )
            {
               error4( 0, e4parm, E90438 ) ;
               return 0 ;
            }
         #endif

         return (B4KEY_DATA *)((char *)&b4->info.num + b4->tag->header.groupLen * iKey) ;
      }

      /* S4MDX */
      unsigned char *b4keyKey( B4BLOCK *b4, const int iKey )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || iKey < 0 )
            {
               error4( 0, e4parm, E90438 ) ;
               return 0 ;
            }
         #endif

         return (unsigned char *)(((B4KEY_DATA *)((char *)&b4->info.num + b4->tag->header.groupLen * iKey ))->value ) ;
      }

      /* S4MDX */
      int b4lastpos( const B4BLOCK *b4 )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
               return error4( 0, e4parm_null, E90438 ) ;
         #endif

         return ( ( b4leaf( b4 ) ) ? ( b4numKeys( b4 ) - 1 ) : ( b4numKeys( b4 ) ) ) ;
      }

      /* S4MDX */
      int b4leaf( const B4BLOCK *b4 )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
               return error4( 0, e4parm_null, E90438 ) ;
         #endif

         return( b4key( b4, b4numKeys( b4 ) )->num == 0L ) ;
      }

      /* S4MDX */
      unsigned long b4recNo( const B4BLOCK *b4, const int i )
      {
         // returns ULONG_MAX (i.e. -1) in case of error
         #ifdef E4PARM_LOW
            if ( b4 == 0 || i < 0 )
            {
               error4( 0, e4parm, E90438 ) ;
               return ULONG_MAX ;
            }
         #endif

         return b4key( b4, i )->num ;
      }
   #endif /* S4INLINE */



   /* S4MDX */
   #ifndef S4OFF_WRITE
      int b4remove( B4BLOCK *b4 )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
               return error4( 0, e4parm_null, E90438 ) ;
         #endif

         B4KEY_DATA *keyCur = b4key( b4, b4->keyOn ) ;
         B4KEY_DATA *keyNext = b4key( b4, b4->keyOn + 1 ) ;

         int leftLen = b4->tag->indexFile->header.blockRw - sizeof( b4->nKeys ) - sizeof( b4->dummy ) - ( b4->keyOn + 1 ) * b4->tag->header.groupLen ;

         #ifdef E4ANALYZE
            if ( b4->keyOn < 0 || b4->keyOn > b4lastpos( b4 ) )
               return error4( b4->tag->codeBase, e4info, E90438 ) ;
         #endif

         #ifdef I4PRINT
            #ifdef I4PRINT_TAG_NAME
               if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
            #endif
            {
               struct _timeb mTime ;
               char *outTime, dump[255] ;
               _ftime( &mTime ) ;
               outTime = ctime( &( mTime.time ) ) ;

               B4KEY_DATA *keyData = b4key( b4, b4->keyOn ) ;
               sprintf( dump, "remove - removed entry: i4 = %ld, #keys = %ld, keyOn = %uld, fileBlock: %uld, recNo = %uld, key = %s, time:%s\r\n", (long)b4->tag->indexFile,
                  (long)b4->nKeys, (long)b4->keyOn, b4->fileBlock, keyData->num, (char *)keyData->value, outTime ) ;
               log5( dump ) ;
            }
         #endif

         #ifdef I4PRINT_BLOCK
            #ifdef I4PRINT_TAG_NAME
               if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
            #endif
            {
               keyData = b4key( b4, 0 ) ;
               sprintf( dump, "b4remove - 1st entry before: i4 = %ld, fileBlock: %uld, recNo = %uld, key = %s, time: %s\r\n", (long)b4->tag->indexFile,
                  b4->fileBlock, keyData->num, (char *)keyData->value, outTime ) ;
               log5( dump ) ;
            }
         #endif

         if ( leftLen > 0 )
            c4memmove( keyCur, keyNext, leftLen ) ;

         b4->nKeys-- ;
         b4->changed = 1 ;

         if ( b4leaf( b4 ) )
            c4memset( b4keyKey( b4, b4numKeys( b4 ) ), 0, b4->tag->header.keyLen ) ;
         #ifdef E4ANALYZE
            else
               if ( b4numKeys( b4 ) < b4->tag->header.keysMax )
                  c4memset( b4keyKey( b4, b4numKeys( b4 ) ), 0, b4->tag->header.keyLen ) ;
         #endif

         #ifdef I4PRINT_BLOCK
            #ifdef I4PRINT_TAG_NAME
               if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
            #endif
            {
               keyData = b4key( b4, 0 ) ;
               sprintf( dump, "b4remove - 1st entry after: i4 = %ld, fileBlock: %uld, recNo = %uld, key = %s, time: %s\r\n", (long)b4->tag->indexFile,
                  b4->fileBlock, keyData->num, (char *)keyData->value, outTime ) ;
               log5( dump ) ;
            }
         #endif

         return 0 ;
      }
   #endif /* S4OFF_WRITE */

   /* S4MDX */
   int b4seek( B4BLOCK *b4, const char *searchValue, const int len )
   {
      int rc, keyLower, keyUpper, saveRc, keyCur ;
      S4CMP_FUNCTION *cmp;

      #ifdef E4PARM_LOW
         if ( b4 == 0 || searchValue == 0 || len < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      /* keyCur must be between  keyLower and  keyUpper */
      keyLower = -1 ;
      keyUpper = b4numKeys( b4 ) ;

      cmp = b4->tag->cmp ;

      if ( keyUpper == 0 )
      {
         b4->keyOn = 0 ;
         return r4after ;
      }

      saveRc = 1 ;

      for( ;; )  /* Repeat until the key is found */
      {
         keyCur  = (keyUpper + keyLower) / 2  ;
         rc = (*cmp)( b4keyKey(b4,keyCur), searchValue, len ) ;

         if ( rc >= 0 )
         {
            keyUpper = keyCur ;
            saveRc = rc ;
         }
         else
            keyLower = keyCur ;

         if ( keyLower >= (keyUpper-1) )  /* then there is no exact match */
         {
            b4->keyOn =  keyUpper ;
            if ( saveRc )
               return r4after ;
            return 0 ;
         }
      }
   }

   /* S4MDX - AS Nov 13/02 - export for dot */
   int S4FUNCTION b4skip( B4BLOCK *b4, int numSkip )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
         {
            error4( 0, e4parm_null, E90438 ) ;
            return 0 ;
         }
      #endif

      int numLeft ;
      if ( numSkip > 0 )
      {
         numLeft = b4numKeys( b4 ) - b4->keyOn ;
         if ( b4leaf( b4 ) )
            if ( numLeft != 0 )
               numLeft -- ;
      }
      else
         numLeft = -b4->keyOn ;

      if ( ( numSkip <= 0L ) ? ( numLeft <= numSkip ) : ( numLeft >= numSkip) )
      {
         b4->keyOn = b4->keyOn + numSkip ;
         return numSkip ;
      }
      else
      {
         b4->keyOn = b4->keyOn + numLeft ;
         return numLeft ;
      }
   }
   #endif  /* S4MDX */

   #ifdef S4FOX
   /* S4FOX */
   void b4leafInit( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
         {
            error4( 0, e4parm_null, E90438 ) ;
            return ;
         }
      #endif

      TAG4FILE *t4 = b4->tag ;
      unsigned int keyLen = (unsigned int)t4->header.keyLen ;
      unsigned int cLen ;
      for ( cLen = 0 ; keyLen ; keyLen >>= 1, cLen++ ) ;

      b4->nodeHdr.trailCntLen = b4->nodeHdr.dupCntLen = (unsigned char)cLen ;

      b4->nodeHdr.trailByteCnt = (unsigned char)((unsigned char)0xFF >> ( 8 - ((cLen / 8) * 8 + cLen % 8))) ;
      b4->nodeHdr.dupByteCnt = b4->nodeHdr.trailByteCnt ;

      S4UNSIGNED_LONG rLen = (S4UNSIGNED_LONG)dfile4recCount( b4->tag->indexFile->dataFile, -2L ) ;

      for ( cLen = 0 ; rLen ; rLen>>=1, cLen++ ) ;

      b4->nodeHdr.recNumLen = (unsigned char) (cLen + (( 8 - ( 2 * (unsigned int)b4->nodeHdr.trailCntLen % 8 )) % 8)) ;
      if ( b4->nodeHdr.recNumLen < 12 )
         b4->nodeHdr.recNumLen = 12 ;

      for ( int tLen = b4->nodeHdr.recNumLen + b4->nodeHdr.trailCntLen + b4->nodeHdr.dupCntLen ; (tLen / 8)*8 != tLen ; tLen++, b4->nodeHdr.recNumLen++ )
      {
         ;  /* make at an 8-bit offset */
      }

      b4->nodeHdr.infoLen = (unsigned char)((unsigned int)(b4->nodeHdr.recNumLen + b4->nodeHdr.trailCntLen + b4->nodeHdr.dupCntLen) / 8) ;

      // AS 06/19/00 also don't allow recNumLen to be > 32, since we only support that many records, and the mask cannot handle
      // larger values.  Was failing on approx. 70 million records
      if ( b4->nodeHdr.recNumLen > 32 )
         b4->nodeHdr.recNumLen = 32 ;

      const S4UNSIGNED_LONG ff = 0xFFFFFFFFL ;
      rLen = ff >> ( sizeof(S4LONG)*8 - b4->nodeHdr.recNumLen ) ;
      c4memcpy( (void *)&b4->nodeHdr.recNumMask[0], (void *)&rLen, sizeof(S4LONG) ) ;

      b4->nodeHdr.freeSpace = (short)i4blockSize( t4->indexFile ) - sizeof( B4STD_HEADER ) - sizeof( B4NODE_HEADER ) ;
   }

   /* S4FOX */
   int b4calcDups( const unsigned char *ptr1, const unsigned char *ptr2, const int len )
   {
      int a ;

      #ifdef E4PARM_LOW
         if ( ptr1 == 0 || ptr2 == 0 || len < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      for ( a = 0 ; a < len; a++ )
         if ( ptr1[a] != ptr2[a] )
            return a ;
      return len ;  /* all duplicates */
   }



   /* S4FOX */
   #ifdef X4RECNO_NOT_INLINE
   S4UNSIGNED_LONG x4recNo( const B4BLOCK *b4, const int numInBlock )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 || numInBlock < 0 )
            return error4( 0, e4parm, E90439 ) ;
      #endif

      #ifdef S4DATA_ALIGN
         S4UNSIGNED_LONG longPtr ;  /* LY 00/10/31 : removed redundant unsigned */
         c4memcpy( (void *)&longPtr , b4->data + numInBlock * b4->nodeHdr.infoLen , sizeof(S4LONG) ) ;
         #ifdef S4DO_BYTEORDER
            longPtr = (unsigned S4LONG)x4reverseLong( (void *)&longPtr ) ;
         #endif
         S4UNSIGNED_LONG longVal ;  /* LY 00/10/31 : removed redundant unsigned */
         c4memcpy( (void *)&longVal , (void *)&b4->nodeHdr.recNumMask[0], sizeof(unsigned S4LONG) ) ;
         return ( longPtr & longVal ) ;
      #else
         #ifdef S4DO_BYTEORDER
            S4UNSIGNED_LONG longTemp = *(unsigned S4LONG *)( b4->data + numInBlock * b4->nodeHdr.infoLen ) ;
            longTemp = (unsigned S4LONG)x4reverseLong( (void *)&longTemp ) ;
            return ( longTemp & *(unsigned S4LONG *)&b4->nodeHdr.recNumMask[0] ) ;
         #else
            /* these 2 lines are inlined if not E4PARM_LOW */
            S4UNSIGNED_LONG *lPtr = (S4UNSIGNED_LONG *)( b4->data + numInBlock * b4->nodeHdr.infoLen ) ;
            return ( *lPtr & *(unsigned S4LONG *)&b4->nodeHdr.recNumMask[0] ) ;
         #endif
      #endif
   }
   #endif



   /* S4FOX */
   #ifdef X4DUPCNT_NOT_INLINE
      /* AS Nov 13/02 - export for dot */
      int S4FUNCTION x4dupCnt( const B4BLOCK *b4, const int numInBlock )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || numInBlock < 0 )
               return error4( 0, e4parm, E90439 ) ;
         #endif

         #ifdef S4DATA_ALIGN
            S4UNSIGNED_LONG longPtr ;
         #else
            unsigned long *lPtr ;
         #endif
         int pos ;
         if ( b4->nodeHdr.infoLen > 4 )  /* > size of long, so must do careful shifting and copying */
         {
            #ifdef E4ANALYZE
               if ( b4->nodeHdr.recNumLen <= 16 )
                  return error4( b4->tag->codeBase, e4info, E80401 ) ;
            #endif
            #ifdef S4DATA_ALIGN
               c4memcpy( (void *)&longPtr , b4->data + numInBlock * b4->nodeHdr.infoLen + 2, sizeof(S4LONG) ) ;
            #else
               lPtr = (unsigned long *)( b4->data + numInBlock * b4->nodeHdr.infoLen + 2 ) ;
            #endif
            pos = b4->nodeHdr.recNumLen - 16 ;
         }
         else
         {
            #ifdef S4DATA_ALIGN
               c4memcpy( (void *)&longPtr , b4->data + numInBlock * b4->nodeHdr.infoLen , sizeof(S4LONG) ) ;
            #else
               lPtr = (unsigned long *)( b4->data + numInBlock * b4->nodeHdr.infoLen ) ;
            #endif
            pos = b4->nodeHdr.recNumLen ;
         }

         #ifdef S4DATA_ALIGN
            #ifdef S4DO_BYTEORDER
               longPtr = x4reverseLong( (void *)&longPtr ) ;
            #endif
            return (int)( ( longPtr >> pos ) & b4->nodeHdr.dupByteCnt ) ;
         #else
            #ifdef S4DO_BYTEORDER
               unsigned long longVal = *lPtr ;
               longVal = x4reverseLong( (void *)&longVal) ;
               return (int)( ( longVal >> pos ) & b4->nodeHdr.dupByteCnt ) ;
            #else
               return (int)( ( *lPtr >> pos ) & b4->nodeHdr.dupByteCnt ) ;
            #endif
         #endif
      }
   #endif /* X4DUPCNT_NOT_INLINE */



   #ifdef X4TRAILCNT_NOT_INLINE
      /* S4FOX */
      /* AS Nov 13/02 - export for dot */
      int S4FUNCTION x4trailCnt( const B4BLOCK *b4, const int numInBlock )
      {
         int pos ;
         #ifdef S4DATA_ALIGN
            unsigned S4LONG longPtr ;
         #else
            unsigned long *lPtr ;
            #ifdef S4DO_BYTEORDER
               unsigned long longVal ;
            #endif
         #endif

         #ifdef E4PARM_LOW
            if ( b4 == 0 || numInBlock < 0 )
               return error4( 0, e4parm, E90439 ) ;
         #endif

         if ( b4->nodeHdr.infoLen > 4 )  /* > size of long, so must do careful shifting and copying */
         {
            #ifdef E4ANALYZE
               if ( b4->nodeHdr.recNumLen <= 16 )
                  return error4( b4->tag->codeBase, e4info, E90438 ) ;
            #endif
            #ifdef S4DATA_ALIGN
               c4memcpy( (void *)&longPtr , b4->data + numInBlock * b4->nodeHdr.infoLen + 2, sizeof(S4LONG) ) ;
            #else
               lPtr = (unsigned long *)( b4->data + numInBlock * b4->nodeHdr.infoLen + 2 ) ;
            #endif
            pos = b4->nodeHdr.recNumLen - 16 + b4->nodeHdr.dupCntLen ;
         }
         else
         {
            #ifdef S4DATA_ALIGN
               c4memcpy( (void *)&longPtr , b4->data + numInBlock * b4->nodeHdr.infoLen , sizeof(S4LONG) ) ;
            #else
               lPtr = (unsigned long *)( b4->data + numInBlock * b4->nodeHdr.infoLen ) ;
            #endif
            pos = b4->nodeHdr.recNumLen + b4->nodeHdr.dupCntLen;
         }

         #ifdef S4DATA_ALIGN
            #ifdef S4DO_BYTEORDER
               longPtr = x4reverseLong( (void *)&longPtr ) ;
            #endif
            return (int)( ( longPtr >> pos ) & b4->nodeHdr.trailByteCnt ) ;
         #else
            #ifdef S4DO_BYTEORDER
               longVal = *lPtr ;
               longVal = x4reverseLong( (void *)&longVal) ;
               return (int)( ( longVal >> pos ) & b4->nodeHdr.trailByteCnt ) ;
            #endif
            return (int)( ( *lPtr >> pos ) & b4->nodeHdr.trailByteCnt ) ;
         #endif
      }
   #endif /* X4TRAILCNT_NOT_INLINE */



   #ifndef S4OFF_WRITE
   /* S4FOX */
   /*
      For Intel architecture, the block info looks like this:  say the recNumLen = 14, trail = 5, dup = 5 (for
      a total of 3 bytes of info).  The Actual bits in use would be as follows:
      RRRR RRRR DDRR RRRR TTTT TDDD
      789A BCDE 4512 3456 1234 5123

      Here, the #'s on the 2nd line indicate the bit ordering of the info.
      This means that to create the actual Record #, for example, you take the 6 bits from the 2nd byte and
      pre-pend them to the 8 bits from the 1st byte.
  */
   int x4putInfo( const B4NODE_HEADER *b4nodeHdr, void *buffer, const S4UNSIGNED_LONG rec, const int trail, const int dupCnt )
   {
      int pos ;
      #ifdef S4DO_BYTEORDER
         #ifndef S4DATA_ALIGN
            S4UNSIGNED_LONG longTemp ;
         #endif
      #endif
      #ifdef S4DATA_ALIGN
         char unixBuf[6] ;
         char unixBuf2[4] ;
         S4UNSIGNED_LONG longVal, longTemp ;
         int doShift = 0 ;
      #else
         unsigned char *buf ;
         /* LY 2001/04/12 : unsigned long* to S4UNSIGNED_LONG for 64-bit */
         S4UNSIGNED_LONG *lPtr ;
      #endif

      #ifdef E4PARM_LOW
         if ( b4nodeHdr == 0 || buffer == 0 || rec == INVALID4BLOCK_ID || trail < 0 || dupCnt < 0 )
            return error4( 0, e4parm, E90439 ) ;
      #endif

      #ifdef S4DATA_ALIGN
         memset( unixBuf, 0, 6 ) ;
         c4memcpy( (void *)&longTemp , (void *)&b4nodeHdr->recNumMask[0], sizeof(S4LONG) ) ;
         longVal = rec & longTemp ;
         #ifdef S4DO_BYTEORDER
            longVal = x4reverseLong( (void *)&longVal ) ;
         #endif
         c4memcpy( unixBuf, (void *)&longVal, sizeof(S4LONG) ) ;
      #else
         buf = (unsigned char *) buffer ;
         /* LY 2001/04/12 : unsigned long* to S4UNSIGNED_LONG for 64-bit */
         lPtr = (S4UNSIGNED_LONG *)buf ;
         c4memset( buf, 0, 6 ) ;
         /* LY 2001/04/12 : unsigned long* to S4UNSIGNED_LONG for 64-bit */
         *lPtr = rec & *(S4UNSIGNED_LONG *)&b4nodeHdr->recNumMask[0] ;
         #ifdef S4DO_BYTEORDER
            *lPtr = x4reverseLong( (void *)lPtr ) ;
         #endif
      #endif

      if ( b4nodeHdr->infoLen > 4 )  /* > size of long, so must do careful shifting and copying */
      {
         #ifdef E4ANALYZE
            if ( b4nodeHdr->recNumLen <= 16 )
               return error4( 0, e4info, E80401 ) ;
         #endif
         #ifndef S4DATA_ALIGN
            /* LY 2001/04/12 : unsigned long* to S4UNSIGNED_LONG for 64-bit */
            lPtr = (S4UNSIGNED_LONG *)( buf + 2 ) ;  /* start at new pos */
         #else
            doShift = 1 ;
         #endif
         pos = b4nodeHdr->recNumLen - 16 ;
      }
      else
         pos = b4nodeHdr->recNumLen ;

      #ifdef S4DATA_ALIGN
         // LY 2001/07/24 longVal = ((unsigned long)dupCnt) << pos ;
         longVal = ((S4UNSIGNED_LONG)dupCnt) << pos ;
         pos += b4nodeHdr->dupCntLen ;
         // LY 2001/07/24 longVal |= ((unsigned long)trail) << pos ;
         longVal |= ((S4UNSIGNED_LONG)trail) << pos ;
         #ifdef S4DO_BYTEORDER
            longVal = x4reverseLong( (void *)&longVal ) ;
         #endif
         c4memcpy( unixBuf2, (void *)&longVal, sizeof(S4LONG) ) ;
         if (doShift)
         {
            unixBuf[2] |= unixBuf2[0] ;  /* must OR bytes 2 and 3 of 'buf' */
            unixBuf[3] |= unixBuf2[1] ;
            unixBuf[4] = unixBuf2[2] ;
            unixBuf[5] = unixBuf2[3] ;
         }
         else
         {
            unixBuf[0] |= unixBuf2[0] ;
            unixBuf[1] |= unixBuf2[1] ;
            unixBuf[2] |= unixBuf2[2] ;
            unixBuf[3] |= unixBuf2[3] ;
         }
         c4memcpy( (void *)buffer, unixBuf, 6 ) ;
      #else
         #ifdef S4DO_BYTEORDER
            longTemp = ((unsigned long)dupCnt) << pos ;
            pos += b4nodeHdr->dupCntLen ;
            longTemp |= ((unsigned long)trail) << pos ;
            longTemp = x4reverseLong( (void *)&longTemp ) ;
            *lPtr |= longTemp ;
         #else
            *lPtr |= ((unsigned long)dupCnt) << pos ;
            pos += b4nodeHdr->dupCntLen ;
            *lPtr |= ((unsigned long)trail) << pos ;
         #endif
      #endif

      return 0 ;
   }



   /* S4FOX */
   int b4insertLeaf( B4BLOCK *b4, const void *vkeyData, const S4UNSIGNED_LONG rec )
   {
      /*
         Note:  Fox Format is as follows:
         - if there are trailing blanks, they are always included... eg a blank key would have 0 duplicates all blanks always,
           never duplicates from previous blank keys
         - duplicates never take into account trailing blanks.  Therefore, you cannot have keylen = 10, <KEY1:trail=10,><dup=4>  This
           is invalid because duplicates cannot take trail into account.  Instead, you would have <dup=0> key "    xxxxxx"
           eg: key1 = "         ", key2 = "    xxxxxx".  In index, key1: dup=0, trail=10, key2: dup=0, key="    xxxxxx"
           Notice that even though it appears that there 'could' be duplicates, there in fact are not because in
           order for there to be a duplicate the data MUST actually be there to duplicate
      */
      #ifdef E4PARM_LOW
         if ( b4 == 0 || vkeyData == 0 || rec < 0L )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            sprintf( dump, "-->inserting into leaf block #: %ld for rec #%ld, at block pos %ld, num keys in block %ld \r\n", (long)b4node( b4->fileBlock ), (long)rec, (long)b4->keyOn, (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      #ifdef E4ANALYZE
         // AS Jan 17/02 - After inserstion should be 1 more key than before... verify this
         long oldNumKeys = (long)b4numKeys(b4) ;
      #endif

      unsigned char *keyData = (unsigned char *)vkeyData ;
      TAG4FILE *tag = b4->tag ;
      int keyLen = tag->header.keyLen ;
      b4->builtOn = -1 ;
      unsigned char infoLen = b4->nodeHdr.infoLen ;   // the # of bytes to store the <record#><# trail bytes><# duplicate bytes> in the key info area of the block

      /* 04/22/97 AS add if statement to reduce code, see if need to check mask first */
      #ifdef S4DATA_ALIGN
         S4UNSIGNED_LONG *rec2 = (S4UNSIGNED_LONG *)u4alloc( sizeof( S4UNSIGNED_LONG ) ) ;
         c4memcpy( rec2, &rec, sizeof( S4UNSIGNED_LONG ) ) ;

         /* LY 00/07/04 : changed mask2 from S4LONG */
         S4UNSIGNED_LONG *mask2 = (S4UNSIGNED_LONG *)u4alloc( sizeof( S4UNSIGNED_LONG ) ) ;
         c4memcpy( mask2, (void *)&b4->nodeHdr.recNumMask[0], sizeof( S4UNSIGNED_LONG ) ) ;

         if ((*rec2 & *mask2 ) != *rec2 )
      #else
         if ( (rec & (*((S4UNSIGNED_LONG *)(&b4->nodeHdr.recNumMask[0]))) ) != rec )
      #endif
      {
         /* if the record is > than the mask, must reset the block with new parameters: */
         char b = sizeof(S4UNSIGNED_LONG) * 8 ;

         S4UNSIGNED_LONG mask = 0x01L << (b-1) ;    /* set leftmost bit  */
         char a ;
         unsigned int recLen ;

         for ( recLen = 0, a = 0 ; a < b ; a++ )
         {
            if ( rec & mask )
            {
               recLen = (unsigned int)((int)b - a) ;
               break ;
            }
            mask >>= 1 ;
         }

         while ( recLen > (unsigned int)b4->nodeHdr.recNumLen )  // CS 2000/12/01
         {
            unsigned long oldRec = b4->keyOn ;
            int saveDups = b4->curDupCnt ;
            int rc = b4reindex( b4 ) ;
            if ( rc )
            {
               #ifdef S4DATA_ALIGN
                  //CJ-02/14/00-Make sure allocated memory is freed.  WinCe memory leak
                  u4free(rec2) ;
                  u4free(mask2) ;
                  rec2 = 0 ;
                  mask2 = 0 ;    /* LY 00/07/04 : split rec2 = mask2 = 0, signed vs. unsigned */
               #endif
               #ifdef I4PRINT_ADD
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "     after insertion, b4reindex failed\r\n" ) ;
                     log5( dump ) ;
                  }
               #endif
               return rc ;
            }
            infoLen = b4->nodeHdr.infoLen ;
            b4top( b4 ) ;
            b4skip( b4, oldRec ) ;
            b4->curDupCnt = saveDups ;
         }
      }

      #ifdef S4DATA_ALIGN
         //CJ-02/14/00-Make sure allocated memory is freed.  WinCe memory leak
         u4free(rec2) ;
         u4free(mask2) ;
         rec2 = 0 ;
         mask2 = 0 ;    /* LY 00/07/04 : split rec2 = mask2 = 0, signed vs. unsigned */
      #endif

      // AS 04/22/97 t4seek.c FoxPro 2.6 always has no blanks if filters exist (at least for character tags)
      int newKeyBlanks ;
      if ( tag->indexFile->dataFile->compatibility == 26 && tag->filter != 0 )
         newKeyBlanks = 0 ;
      else
         newKeyBlanks = b4calcBlanks( keyData, keyLen, tag->pChar ) ;

      int newKeyDups = 0 ;
      int reqdLen ;

      if ( b4numKeys( b4 ) == 0 )
      {
         if( b4->nodeHdr.freeSpace == 0 )  /* block needs initialization */
         {
            b4leafInit( b4 ) ;
            infoLen = b4->nodeHdr.infoLen ;
         }
         reqdLen = keyLen - newKeyBlanks ;
         b4->keyOn = 0 ;
         b4->curPos = ((char *)&b4->header) + i4blockSize( tag->indexFile ) - reqdLen ;
         #ifdef EXCEPTION4REINDEX
            // in this case recover by reindexing
            if ( reqdLen < 0 )
            {
               #ifdef I4PRINT_ADD
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "     after insertion, index found corrupt\r\n" ) ;
                     log5( dump ) ;
                  }
               #endif
               return error4( tag->codeBase, e4index, E80401 ) ;
            }
         #endif

         c4memcpy( b4->curPos, keyData, (unsigned int)reqdLen ) ;  /* key */
         unsigned char buffer[6] ;
         // AS 05/01/00 verify data is valid going in...
         assert5( newKeyBlanks <= keyLen ) ;
         x4putInfo( &b4->nodeHdr, buffer, rec, newKeyBlanks, 0 ) ;

         c4memcpy( b4->data, (void *)buffer, infoLen ) ;
      }
      else
      {
         if ( b4->keyOn == b4numKeys( b4 ) )  /* at end */
         {
            newKeyDups = b4->curDupCnt ;
            reqdLen = keyLen - newKeyBlanks - newKeyDups ;
            if ( reqdLen < 0 )
            {
               // AS 03/21/00 in instance where inserting key with trailing blanks and previous key has binary > 0x20, reqdLen ends up being < 0 in some cases (eg. 0x2001, inserting 0x2020 - trail = 2, dup = 1 , reqd len = -1)
               newKeyDups += reqdLen ;  // reqdLen negative, reduce the duplicates by the number of extra trailing bytes which could be considered as either trail or duplicate (we always treat as trail)
               reqdLen = 0 ;
            }
            assert5( reqdLen >= 0 && newKeyDups >= 0 && newKeyBlanks >= 0 ) ;
            if ( (int)b4->nodeHdr.freeSpace < ( reqdLen + (int)infoLen ) )  /* no room to add */
            {
               #ifdef I4PRINT_ADD
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "     after insertion, no room to add, returning 1\r\n" ) ;
                     log5( dump ) ;
                  }
               #endif
               return 1 ;
            }
            b4->curPos -= reqdLen ;
            c4memcpy( b4->curPos, keyData + b4->curDupCnt, (unsigned int)reqdLen ) ;  /* key */
            unsigned char buffer[6] ;
            // AS 05/01/00 verify data is valid going in...
            assert5( ( newKeyBlanks + newKeyDups ) <= keyLen ) ;
            x4putInfo( &b4->nodeHdr, buffer, rec, newKeyBlanks, newKeyDups ) ;
            c4memcpy( b4->data + b4->keyOn * infoLen , (void *)buffer, infoLen ) ;
         }
         else
         {
            int rightDups, oldRightDups, extraDups ;
            int rightBlanks = x4trailCnt( b4, b4->keyOn ) ;

            if ( b4->keyOn == 0 )   /* insert at top */
            {
               oldRightDups = 0 ;
               rightDups = b4calcDups( keyData, (unsigned char *)b4->curPos, keyLen - ( rightBlanks > newKeyBlanks ? rightBlanks : newKeyBlanks ) ) ;
               extraDups = rightDups ;
               newKeyDups = 0 ;
            }
            else /* insert in middle of block */
            {
               oldRightDups = x4dupCnt( b4, b4->keyOn ) ;
               /* want to calculate the number of duplicates between to the right key and the
                  one we are about to insert, so we can adjust the right-keys duplicate count if required.
                  Take the key length, subtract the blanks, and then discount the # of duplicates already for the next key */
               /* LY 00/03/22 : replaced max() for compilers that don't support it */
               int numAdditionalDuplicatesPossible = keyLen - ( rightBlanks > newKeyBlanks ? rightBlanks : newKeyBlanks ) - oldRightDups ;
               /* AS 02/20/98 t5cbug2.cpp fails if not coded here */
               if ( numAdditionalDuplicatesPossible < 0 )  /* happens when we have values less than blank but inserting blank characters */
               {
                  /* modify the left-blanks to disenclude the right dups */
                  // AS 06/08/00 if newKeyBlanks == keyLen (i.e. all blank), then leave as is - we always have accurate blank count...
                  /* AS 04/26/01 - overridden back to old code, was not causing the problem...

                   AS 04/26/01 - I suspect t5cbug2.cpp was incorrect.  additionalDuplicates is negative, it should
                     be just leaving newKeyBlanks alone since we must ALWAYS have an accurate
                     trail count.  Test is bstamp.cpp, which you can run to verify.
                  */
                  if ( newKeyBlanks != keyLen )
                     newKeyBlanks += numAdditionalDuplicatesPossible ;
                  extraDups = 0 ;
               }
               else
                  extraDups = b4calcDups( keyData + oldRightDups, (unsigned char *)b4->curPos, numAdditionalDuplicatesPossible ) ;
               rightDups = oldRightDups + extraDups ;
               newKeyDups = b4->curDupCnt ;
            }

            #ifdef E4ANALYZE_ALL
               if ( tag->header.typeCode & 0x01 )
                  if ( newKeyDups == tag->header.keyLen || rightDups == tag->header.keyLen )
                  {
                     #ifdef I4PRINT_ADD
                        #ifdef I4PRINT_TAG_NAME
                           if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                        #endif
                        {
                           char dump[255] ;
                           sprintf( dump, "     after insertion, index found corrupt\r\n" ) ;
                           log5( dump ) ;
                        }
                     #endif
                     return error4( tag->codeBase, e4info, E80402 ) ;
                  }
            #endif

            int rightLen = keyLen - rightBlanks - rightDups ;
            int newKeyLen = keyLen - newKeyDups - newKeyBlanks ;
            reqdLen = newKeyLen - extraDups ;

            // AS 03/16/00 there is a case where reqdLen is < 0, namely if we are inserting
            // whereby we have binary character keys and inserting say "0x012020" and '0x012019' and '0x012021'
            // already exist (i.e. dups is 7, current trail is '2'.  In this case we want to
            // reduce the dup count down, we always have an accurate trail count...
            if ( reqdLen < 0 )
            {
               newKeyDups += reqdLen ;
               newKeyLen -= reqdLen ;
               reqdLen = 0 ;
               // it must be case that we are not copying, so newKeyDups + leftTrail must == key len...
               assert5( newKeyDups + newKeyBlanks == keyLen ) ;
               assert5( newKeyDups >= 0 && newKeyLen >= 0 && newKeyBlanks >= 0 ) ;
            }

            if ( (int)b4->nodeHdr.freeSpace < (reqdLen + (int)infoLen) )  /* no room to add */
            {
               #ifdef I4PRINT_ADD
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255] ;
                     sprintf( dump, "     after insertion, no room to add, returning 1\r\n" ) ;
                     log5( dump ) ;
                  }
               #endif
               return 1 ;
            }

            if ( reqdLen != 0 )  /* shift data over */
            {
               int movLen = i4blockSize( tag->indexFile ) - sizeof( B4STD_HEADER ) - infoLen * b4numKeys( b4 )
                         - sizeof( B4NODE_HEADER ) - b4->nodeHdr.freeSpace  - ( ( (char *)&b4->header ) + i4blockSize( tag->indexFile ) - b4->curPos ) ;

               #ifdef EXCEPTION4REINDEX
                  // in this case recover by reindexing
                  if ( movLen < 0 )
                  {
                     #ifdef I4PRINT_ADD
                        #ifdef I4PRINT_TAG_NAME
                           if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                        #endif
                        {
                           char dump[255] ;
                           sprintf( dump, "     after insertion, index found corrupt\r\n" ) ;
                           log5( dump ) ;
                        }
                     #endif
                     return error4( tag->codeBase, e4index, E80401 ) ;
                  }
               #endif
               /* move and put keys */
               c4memmove( b4->curPos - movLen - reqdLen, b4->curPos - movLen, (unsigned int)movLen ) ;
            }
            b4->curPos += ( keyLen - rightBlanks - oldRightDups ) ;

            #ifdef EXCEPTION4REINDEX
               // in this case recover by reindexing
               if ( rightLen < 0 )
               {
                  #ifdef I4PRINT_ADD
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255] ;
                        sprintf( dump, "     after insertion, index found corrupt\r\n" ) ;
                        log5( dump ) ;
                     }
                  #endif
                  return error4( tag->codeBase, e4index, E80401 ) ;
               }
            #endif
            c4memmove( b4->curPos - newKeyLen - rightLen, b4->curPos - ( keyLen - rightDups - rightBlanks ), (unsigned int)rightLen ) ;
            b4->curPos -= newKeyLen ;
            #ifdef EXCEPTION4REINDEX
               // in this case recover by reindexing
               if ( newKeyLen < 0 )
               {
                  #ifdef I4PRINT_ADD
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255] ;
                        sprintf( dump, "     after insertion, index found corrupt\r\n" ) ;
                        log5( dump ) ;
                     }
                  #endif
                  return error4( tag->codeBase, e4index, E80401 ) ;
               }
            #endif
            c4memcpy( b4->curPos, keyData + newKeyDups, (unsigned int)newKeyLen ) ;

            /* move and put info */
            char *infoPos = b4->data + ( b4->keyOn ) * infoLen ;

            int memMovLen = (infoLen * ( b4numKeys( b4 ) - b4->keyOn ) ) ;
            #ifdef EXCEPTION4REINDEX
               // in this case recover by reindexing
               if ( memMovLen < 0 )
               {
                  #ifdef I4PRINT_ADD
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255] ;
                        sprintf( dump, "     after insertion, index found corrupt\r\n" ) ;
                        log5( dump ) ;
                     }
                  #endif
                  return error4( tag->codeBase, e4index, E80401 ) ;
               }
            #endif
            c4memmove( infoPos + infoLen, infoPos, (unsigned int)memMovLen ) ;
            unsigned char buffer[6] ;

            // AS 05/01/00 verify data is valid going in...
            assert5( ( newKeyBlanks + newKeyDups ) <= keyLen ) ;
            x4putInfo( &b4->nodeHdr, buffer, rec, newKeyBlanks, newKeyDups ) ;

            c4memcpy( infoPos, (void *)buffer, infoLen ) ;

            // AS 05/01/00 verify data is valid going in...
            assert5( ( rightBlanks + rightDups ) <= keyLen ) ;
            x4putInfo( &b4->nodeHdr, buffer, x4recNo( b4, b4->keyOn + 1 ), rightBlanks, rightDups ) ;
            c4memcpy( infoPos + infoLen, (void *)buffer, infoLen ) ;
         }
      }

      b4->changed = 1 ;
      b4->header.nKeys++ ;
      b4->nodeHdr.freeSpace -= (short) (reqdLen + (int)infoLen ) ;
      b4->curDupCnt = newKeyDups ;
      b4->curTrailCnt = newKeyBlanks ;

      #ifdef E4ANALYZE
         // AS Jan 17/02 - After inserstion should be 1 more key than before... verify this
         if ( (long)b4numKeys(b4) != oldNumKeys + 1 )
         {
            // log4invokeDebuggerDo() ;
            return error4( tag->codeBase, e4index, E80401 ) ;
         }
      #endif

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            sprintf( dump, "     after insertion: num keys in block %ld \r\n", (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      return 0 ;
   }



   /* S4FOX */
   static int b4insertBranchBalance( B4BLOCK *b4 )
   {
      /* returns 1 if unable to move keys around to make room */
      if ( b4numKeys( b4 ) != 2 )   /* only perform in tight situations */
         return 1;

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp(  b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            sprintf( dump, "-->insert balancing block #: %ld\r\n", (long)b4node( b4->fileBlock ) ) ;
            log5( dump ) ;
         }
      #endif

      TAG4FILE *tag = b4->tag ;
      B4BLOCK *parent = (B4BLOCK *)l4prev( &tag->blocks, b4 ) ;
      if ( parent == 0 )   /* no parent, so working on a root block, allow split */
         return 1 ;

      int gLen = tag->header.keyLen + 2 * sizeof(S4LONG ) ;

      if ( b4->keyOn == 0L )  /* try adding to right neighbor */
      {
         if ( parent->keyOn == b4numKeys( parent ) - 1 )  /* parent doesn't share blocks */
            return 1 ;

         if ( b4hasNoRightNeighbor( b4 ) == 0 )  /* no neighbor */
            return 1 ;

         B4BLOCK *right = b4alloc( tag, b4->header.rightNode ) ;
         if ( right == 0 )
            return 1 ;
         int rc = i4readBlock( &tag->indexFile->file, b4->header.rightNode, 0, right ) ;
         if ( rc < 0 )
         {
            b4free( right ) ;
            return 1 ;
         }

         if ( b4numKeys( right ) >= 2 )  /* already full */
         {
            b4free( right ) ;
            return 1 ;
         }

         char *nPos = ((char *)&right->nodeHdr) + gLen * b4numKeys( right ) ;
         char *oPos = ((char *)&right->nodeHdr) ;
         int memToCopy = gLen * b4numKeys( right ) ;
         #ifdef EXCEPTION4REINDEX
            // in this case recover by reindexing
            if ( memToCopy < 0 )
               return error4( tag->codeBase, e4index, E80401 ) ;
         #endif
         c4memcpy( nPos, oPos, memToCopy ) ;

         nPos = ((char *)&right->nodeHdr)  ;
         oPos = ((char *)&b4->nodeHdr) + gLen * ( b4numKeys( b4 ) - 1 ) ;
         #ifdef EXCEPTION4REINDEX
            // in this case recover by reindexing
            if ( gLen < 0 )
               return error4( tag->codeBase, e4index, E80401 ) ;
         #endif
         c4memcpy( nPos, oPos, gLen ) ;
         b4->header.nKeys-- ;
         right->header.nKeys++ ;

         nPos = ((char *)&parent->nodeHdr) + gLen * ( parent->keyOn ) ;
         oPos = ((char *)&b4->nodeHdr ) + gLen * ( b4numKeys( b4 ) - 1 ) ;
         c4memcpy( nPos, oPos, tag->header.keyLen + sizeof(S4LONG ) ) ;
         parent->changed = 1 ;
         right->changed = 1 ;
         b4flush( right ) ;
         b4free( right ) ;
      }
      else  /* try adding to left neighbor */
      {
         if ( parent->keyOn == 0 )  /* parent doesn't share blocks */
            return 1 ;

         if ( b4hasNoLeftNeighbor( b4 ) )  /* no neighbor */
            return 1 ;

         B4BLOCK *left = b4alloc( tag, b4->header.leftNode ) ;
         if ( left == 0 )
            return 1 ;
         int rc = i4readBlock( &tag->indexFile->file, b4->header.leftNode, 0, left ) ;
         if ( rc < 0 )
         {
            b4free( left ) ;
            return 1 ;
         }

         if ( b4numKeys( left ) >= 2 )  /* already full */
         {
            b4free( left ) ;
            return 1 ;
         }

         char *nPos = ((char *)&left->nodeHdr) + gLen * b4numKeys( left ) ;
         char *oPos = ((char *)&b4->nodeHdr) ;
         #ifdef EXCEPTION4REINDEX
            // in this case recover by reindexing
            if ( gLen < 0 )
               return error4( tag->codeBase, e4index, E80401 ) ;
         #endif
         c4memcpy( nPos, oPos, gLen ) ;

         b4->header.nKeys-- ;

         nPos = ((char *)&b4->nodeHdr) ;
         oPos = ((char *)&b4->nodeHdr) + gLen * b4numKeys( b4 ) ;
         int memToCopy = gLen * b4numKeys( b4 ) ;
         #ifdef EXCEPTION4REINDEX
            // in this case recover by reindexing
            if ( memToCopy < 0 )
               return error4( tag->codeBase, e4index, E80401 ) ;
         #endif
         c4memcpy( nPos, oPos, memToCopy ) ;

         b4->keyOn-- ;
         left->header.nKeys++ ;

         nPos = ((char *)&parent->nodeHdr) + gLen * ( parent->keyOn - 1 ) ;
         oPos = ((char *)&left->nodeHdr ) + gLen * ( b4numKeys( left ) - 1 ) ;
         c4memcpy( nPos, oPos, tag->header.keyLen + sizeof(S4LONG ) ) ;
         parent->changed = 1 ;
         left->changed = 1 ;
         b4flush( left ) ;
         b4free( left ) ;
      }

      return 0 ;
   }



   /* S4FOX */
   int b4insertBranch( B4BLOCK *b4, const void *k, const S4UNSIGNED_LONG r1, const S4UNSIGNED_LONG rin2, const char newFlag )
   {
      int gLen = b4->tag->header.keyLen + 2 * sizeof(S4LONG) ;

      #ifdef E4PARM_LOW
         if ( b4 == 0 || k == 0 || r1 == 0L || rin2 == INVALID4BLOCK_ID || rin2 == INVALID4BLOCK_ID )
            return error4( b4->tag->codeBase, e4parm, E90438 ) ;
      #endif

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp(  b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            sprintf( dump, "-->inserting into branch block #: %ld for r1 #:%ld and r2 #:%ld, block on key: %ld, # keys in block:%ld\r\n", (long)b4node( b4->fileBlock ), (long)r1, (long)rin2, (long)b4->keyOn, (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      int leftLen = i4blockSize( b4->tag->indexFile ) - sizeof( b4->header ) - gLen * b4numKeys( b4 ) ;
      if ( leftLen < gLen )  /* not enough room */
      {
         /* if only 2 keys in branch, try to do some swapping with neighbor */
         if ( b4insertBranchBalance( b4 ) == 1 )
            return 1 ;
      }

      char *dataPtr = ((char *)&b4->nodeHdr) + b4->keyOn * gLen ;
      char *nextPtr = dataPtr + gLen ;

      #ifdef E4ANALYZE
         if ( b4->keyOn < 0 || b4->keyOn > b4numKeys( b4 ) )
            return error4( 0, e4info, E90438 ) ;
      #endif

      int moveLen = gLen * ( b4numKeys( b4 ) - b4->keyOn ) ;

      #ifdef EXCEPTION4REINDEX
         // in this case recover by reindexing
         if ( moveLen < 0 )
            return error4( b4->tag->codeBase, e4index, E80401 ) ;
      #endif
      c4memmove( nextPtr, dataPtr, (unsigned int)moveLen ) ;
      b4->header.nKeys++ ;
      c4memcpy( dataPtr, k, (unsigned int)b4->tag->header.keyLen ) ;
      c4memset( dataPtr + gLen - 2 * sizeof(S4LONG), 0, sizeof(S4LONG) ) ;

      S4UNSIGNED_LONG r2 = x4reverseLong( (void *)&rin2 ) ;
      c4memcpy( dataPtr + gLen - 2 * sizeof(S4LONG), (void *)&r2, sizeof(S4LONG) ) ;

      S4UNSIGNED_LONG r = x4reverseLong( (void *)&r1 ) ;

      if ( !newFlag &&  (b4->keyOn + 1) != b4numKeys( b4 ) )
         c4memcpy(  nextPtr + gLen - sizeof(S4LONG), (void *)&r, sizeof(S4LONG) ) ;
      else
         c4memcpy( dataPtr + gLen - sizeof(S4LONG), (void *)&r, sizeof(S4LONG) ) ;

      b4->changed = 1 ;

      return 0 ;
   }


   /* S4FOX */
   #ifndef S4INLINE
      int b4insert( B4BLOCK *b4, const void *keyData, const unsigned long rec, const unsigned long rec2, const char newFlag )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || keyData == 0 || rec == 0L || rec == INVALID4BLOCK_ID || rec2 == INVALID4BLOCK_ID )
               return error4( 0, e4parm, E90438 ) ;
         #endif

         return ( b4leaf( b4 ) ? b4insertLeaf( b4, keyData, rec ) : b4insertBranch( b4, keyData, rec, rec2, newFlag ) ) ;
      }
   #endif



   /* S4FOX */
   int b4reindex( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      int spaceReqd = b4numKeys( b4 ) ;   /* 1 byte extra for each record */
      if ( spaceReqd > b4->nodeHdr.freeSpace )  /* not enough room */
         return 1 ;

      S4UNSIGNED_LONG rec ;
      int niLen = b4->nodeHdr.infoLen + 1 ;
      #ifdef EXCEPTION4REINDEX
         // in this case recover by reindexing
         if ( niLen < 0 )
            return error4( b4->tag->codeBase, e4index, E80401 ) ;
      #endif

      for ( int i = b4numKeys( b4 ) - 1 ; i >= 0 ; i-- )
      {
         int dupCnt = x4dupCnt( b4, i ) ;
         int trail = x4trailCnt( b4, i ) ;
         rec = x4recNo( b4, i ) ;
         c4memset( b4->data + i * niLen, 0, (unsigned int)niLen ) ;

         b4->nodeHdr.recNumLen += 8 ;  /* for the new info */
         b4->nodeHdr.infoLen++ ;
         unsigned char buffer[6] ;
         // AS 05/01/00 verify data is valid going in...
         assert5( ( trail + dupCnt ) <= b4->tag->header.keyLen ) ;
         x4putInfo( &b4->nodeHdr, buffer, rec, trail, dupCnt ) ;
         b4->nodeHdr.recNumLen -= 8 ;  /* to get the old info */
         b4->nodeHdr.infoLen-- ;
         c4memcpy( b4->data + i * niLen, (void *)buffer, (unsigned int)niLen ) ;
      }

      c4memcpy( (void *)&rec, (void *)&b4->nodeHdr.recNumMask[0], sizeof(S4LONG) ) ;
      rec |= (0x000000FFL << b4->nodeHdr.recNumLen ) ;
      c4memcpy( (void *)&b4->nodeHdr.recNumMask[0], (void *)&rec, sizeof(S4LONG) ) ;

      b4->nodeHdr.infoLen++ ;
      b4->nodeHdr.recNumLen += 8 ;
      b4->nodeHdr.freeSpace -= b4numKeys( b4 ) ;
      return 0 ;
   }



   /* S4FOX */
   int b4doFlush( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      assert5( b4->changed ) ;   /* this function only gets called by b4flush() which checks this flag */

      #ifdef I4PRINT_BLOCK
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            // struct _timeb mTime ;
            // char *outTime, dump[255] ;
            // _ftime( &mTime ) ;
            // outTime = ctime( &( mTime.time ) ) ;
            char dump[255];

            B4KEY_DATA *keyData = b4key( b4, b4numKeys(b4)-1 ) ;
            sprintf( dump, "b4flush - last entry: fileBlock: %uld, recNo = %ld, numKeys - %ld\r\n", (unsigned long)b4node(b4->fileBlock), (long)keyData->num, (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      TAG4FILE *t4file = b4->tag ;
      INDEX4FILE *i4 = t4file->indexFile ;

      FILE4LONG fPos ;
      b4getFilePosition( b4, &fPos ) ;

      #ifndef S4OFF_OPTIMIZE
         i4->readBlockTag = t4file ;
      #endif

      #ifdef S4BYTE_SWAP
         char *swap ;
         int i ;
         S4LONG longVal ;
         short shortVal ;
         swap = (char *)u4alloc( i4blockSize( i4 ) ) ;
         c4memcpy( (void *)swap, (void *)&b4->header.nodeAttribute, i4blockSize( i4 ) ) ;

         /* position at either B4NODE_HEADER (leaf) or data (branch) */
         char *swapPtr = swap + 2 * sizeof( short) + 2 * sizeof(S4LONG) ;

         /* if block is a leaf */
         if (b4->header.nodeAttribute >= 2 )
         {
            /* swap B4NODE_HEADER members */
            shortVal = x4reverseShort( (void *)swapPtr ) ; /* freeSpace */
            c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
            swapPtr += sizeof(short) ;

            longVal = x4reverseLong( (void *)swapPtr ) ;   /* recNumMask */
            c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
         }
         else /* if block is a branch */
         {
            shortVal = t4file->header.keyLen + sizeof(S4LONG) ;

            /* position swapPtr to end of first key expression */
            swapPtr += t4file->header.keyLen ;

            /* move through all B4KEY's to swap 'long's */
            for ( i = 0 ; i < (int)b4numKeys( b4 ) ; i++ )
            {
               longVal = x4reverseLong( (void *)swapPtr ) ;
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               swapPtr += sizeof(S4LONG) ;
               longVal = x4reverseLong( (void *)swapPtr ) ;
               c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               swapPtr += shortVal ;
            }
         }

         /* reposition to B4STD_HEADER and swap members */
         swapPtr = swap ;

         shortVal = x4reverseShort( (void *)swapPtr ) ; /* nodeAttribute */
         c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
         swapPtr += sizeof(short) ;

         shortVal = x4reverseShort( (void *)swapPtr ) ; /* nKeys */
         c4memcpy( swapPtr, (void *) &shortVal, sizeof(short) ) ;
         swapPtr += sizeof(short) ;

         longVal = x4reverseLong( (void *)swapPtr ) ;   /* leftNode */
         c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
         swapPtr += sizeof(S4LONG) ;

         longVal = x4reverseLong( (void *)swapPtr ) ;   /* rightNode */
         c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
         swapPtr += sizeof(S4LONG) ;

         int rc = file4writeInternal( &i4->file, fPos, swap, i4blockSize( i4 ) ) ;
         u4free( swap ) ;
      #else
         int rc = file4writeInternal( &i4->file, fPos, &b4->header.nodeAttribute, i4blockSize( i4 ) ) ;
      #endif

      #ifndef S4OFF_OPTIMIZE
         i4->readBlockTag = 0 ;
      #endif

      if ( rc < 0 )
         return rc ;
      b4->changed = 0 ;
      return 0 ;
   }
   #endif /* S4OFF_WRITE */



   #ifndef S4INLINE
   /* S4FOX */
   int b4go( B4BLOCK *b4, const int iKey )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 || iKey < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      return b4skip( b4, iKey - b4->keyOn ) ;
   }
   #endif /* S4INLINE */



   /* S4FOX */
   void b4goEof( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
         {
            error4( 0, e4parm_null, E90438 ) ;
            return ;
         }
      #endif

      b4->keyOn = b4numKeys( b4 ) ;
      b4->curPos = ((char *)&b4->header) + sizeof( B4STD_HEADER ) + sizeof( B4NODE_HEADER )
                    + b4numKeys( b4 ) * b4->nodeHdr.infoLen + b4->nodeHdr.freeSpace ;
   }



   /* S4FOX */
   B4KEY_DATA * S4FUNCTION b4key( B4BLOCK *b4, const int iKey )
   {
      int len ;

      #ifdef E4PARM_LOW
         if ( b4 == 0 || iKey > b4numKeys( b4 ) || iKey < 0 )
         {
            error4( 0, e4parm, E90438 ) ;
            return 0 ;
         }
      #endif

      if ( iKey == b4->builtOn )   /* already there! */
         return b4->builtKey ;

      char *val = (char *)(&b4->builtKey->value[0]) ;
      int kLen = b4->tag->header.keyLen ;

      if ( b4->header.nodeAttribute >= 2 ) /* leaf */
      {
         if ( b4->builtOn > iKey || b4->builtOn == -1 )
         {
            b4->builtOn = -1 ;
            b4->builtPos = ((char *)&b4->header) + i4blockSize( b4->tag->indexFile ) ;
         }

         for ( ; b4->builtOn != iKey ; )
         {
            b4->builtOn++ ;
            b4->curDupCnt = x4dupCnt( b4, b4->builtOn ) ;
            b4->curTrailCnt = x4trailCnt( b4, b4->builtOn ) ;
            len = kLen - b4->curDupCnt - b4->curTrailCnt ;
            // AS 03/08/00 - always look for this case, since it commonly indicates index corruption, and
            // we don't want a gpf to occur (esp. in server)
            // #ifdef EXCEPTION4REINDEX
               // in this case recover by reindexing
               if ( len < 0 )
               {
                  error4( b4->tag->codeBase, e4index, E80401 ) ;
                  return 0 ;
               }
            // #endif
            #ifdef E4ANALYZE
               if (len < 0 || len > kLen || ( b4->builtPos - len ) < b4->data )
               {
                  error4( b4->tag->codeBase, e4info, E80401 ) ;
                  return 0 ;
               }
            #endif
            b4->builtPos -= len ;
            c4memcpy( val + b4->curDupCnt, b4->builtPos, (unsigned int)len ) ;
            c4memset( val + kLen - b4->curTrailCnt, b4->tag->pChar, (unsigned int)b4->curTrailCnt ) ;
         }
         b4->builtKey->num = x4recNo( b4, iKey ) ;
      }
      else /* branch */
      {
         c4memcpy( val, (((char *)&b4->nodeHdr) + iKey*(2*sizeof(S4LONG ) + kLen ) ), (unsigned int)kLen ) ;
         // LY 2001/07/24 b4->builtKey->num = (unsigned long)x4reverseLong( (void *)( ((unsigned char *)&b4->nodeHdr)
         b4->builtKey->num = (S4UNSIGNED_LONG)x4reverseLong( (void *)( ((unsigned char *)&b4->nodeHdr)
                             + (iKey+1)*(2*sizeof(S4LONG) + kLen) - sizeof(S4LONG) ) ) ;
      }
      return b4->builtKey ;
   }



   /* S4FOX */
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int b4dataLeaf( void *data, TAG4FILE *tag )
   {
      /* based on the actual stored-data (from file) only, determine whether or not a leaf.  Used by file optimization routines */
      B4STD_HEADER *header ;
      #ifdef E4PARM_LOW
         if ( data == 0 || tag == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      header = (B4STD_HEADER *)data ;

      return( header->nodeAttribute >= 2 ) ;
   }



   #ifndef S4INLINE
   /* S4FOX */
   unsigned char *b4keyKey( B4BLOCK *b4, const int iKey )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
         {
            error4( 0, e4parm_null, E90438 ) ;
            return 0 ;
         }
      #endif

      return (unsigned char *)b4key( b4, iKey )->value ;
   }



   /* S4FOX */
   int b4lastpos( const B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      return b4numKeys( b4 ) - 1 ;
   }



   /* S4FOX */
   int b4leaf( const B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      return( b4->header.nodeAttribute >= 2 ) ;
   }
   #endif /* S4INLINE */



   /* S4FOX */
   #ifdef E4PARM_LOW
   unsigned long S4FUNCTION b4recNo( const B4BLOCK *b4, const int i )
   {
      // returns ULONG_MAX (i.e. -1) in case of error
      #ifdef E4PARM_LOW
         if ( b4 == 0 || i < 0 )
         {
            error4( 0, e4parm_null, E90438 ) ;
            return ULONG_MAX ;
         }
      #endif

      /* this code is inline in non-parm-low case, copied from d4declar.h */

      if ( b4->header.nodeAttribute >= 2 ) /* leaf */
         return x4recNo( b4, i ) ;
      else /* branch */
          return (unsigned long)x4reverseLong( (void *)(((unsigned char *)&b4->nodeHdr) +
             i * (2*sizeof(S4LONG) + b4->tag->header.keyLen) + b4->tag->header.keyLen ) ) ;
   }
   #endif



   /* S4FOX */
   #ifndef S4OFF_WRITE
   /* LY 2000/04/06 - unsigned long to S4UNSIGNED_LONG for 64-bit */
   int b4brReplace( B4BLOCK *b4, const unsigned char *str, const S4UNSIGNED_LONG r )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 || str == 0 || r == INVALID4BLOCK_ID )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      int keyLen = b4->tag->header.keyLen ;
      char *putPl = ( (char *)&b4->nodeHdr ) + b4->keyOn * ( 2 * sizeof(S4LONG ) + keyLen ) ;
      c4memcpy( putPl, str, (unsigned int)keyLen ) ;
      c4memcpy( b4->builtKey->value, str, (unsigned int)keyLen ) ;
      c4memset( putPl + keyLen, 0, sizeof(S4LONG) ) ;
      /* LY 2000/04/06 - unsigned long to S4UNSIGNED_LONG for 64-bit */
      // LY 2001/07/24 S4UNSIGNED_LONG rec = (unsigned long)x4reverseLong( (void *)&r ) ;
      S4UNSIGNED_LONG rec = (S4UNSIGNED_LONG)x4reverseLong( (void *)&r ) ;
      c4memcpy( putPl + keyLen, (void *)&rec, sizeof(S4LONG) ) ;

      b4->changed = 1 ;

      return 0 ;
   }
   #endif /* S4OFF_WRITE */



   /* S4FOX */
   /* LY 2000/04/07 - unsigned long to S4UNSIGNED_LONG for 64-bit */
   int b4rBrseek( B4BLOCK *b4, const char *searchValue, const int len, const S4UNSIGNED_LONG recNo )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 || searchValue == 0 || len < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      #ifdef E4ANALYZE_ALL
         /* incoming rec is swapped, so unswap it to see if it is valid */
         /* LY 2000/04/07 - unsigned long to S4UNSIGNED_LONG for 64-bit */
         S4UNSIGNED_LONG swapped = recNo ;
         swapped = x4reverseLong( &swapped ) ;
         if ( swapped == INVALID4BLOCK_ID )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      int rc = b4seek( b4, searchValue, len ) ;

      int keyLen = b4->tag->header.keyLen ;

      if ( rc == 0 )   /* string matches, so search on recNo */
      {
         for( ;; )
         {
            #ifdef S4DO_BYTEORDER
               /* LY 2000/04/07 - long to S4LONG for 64-bit */
               S4LONG lrecNo = x4reverseLong( (void *)&recNo ) ;
               S4LONG longVal = x4reverseLong( (void *)(((char *)&b4->nodeHdr) + b4->keyOn * ( 2 * sizeof(S4LONG) + keyLen) + keyLen ) ) ;
               if ( c4memcmp( (void *)&lrecNo, (void *)&longVal, sizeof(S4LONG) ) <= 0
            #else
               if ( c4memcmp( (void *)&recNo, ((char *)&b4->nodeHdr) + b4->keyOn * ( 2 * sizeof(S4LONG) + keyLen) + keyLen, sizeof(S4LONG) ) <= 0
            #endif
               || b4->keyOn >= ( b4numKeys( b4 ) - 1 ) )  /* best match, so done */
                  break ;
            else
               if( b4->keyOn < b4numKeys( b4 ) )
                  b4skip( b4, 1L ) ;

            if ( u4memcmp( (void *)b4keyKey(b4,b4->keyOn), searchValue, (unsigned int)len ) )   /* the key has changed, so stop here */
               break ;
         }
      }

      return rc ;
   }



   /* S4FOX */
   int b4seek( B4BLOCK *b4, const char *searchValue, const int len )
   {

      #ifdef E4PARM_LOW
         if ( b4 == 0 || searchValue == 0 || len < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      if ( b4numKeys( b4 ) == 0 )
      {
         b4->keyOn = 0 ;
         return r4after ;
      }

      if ( b4leaf( b4 ) )
         return b4leafSeek( b4, searchValue, len ) ;

      /* keyCur must be between  keyLower and  keyUpper */
      int keyLower = -1 ;
      int keyUpper = b4numKeys( b4 ) - 1 ;

      int saveRc = 1 ;
      int groupVal = 2 * sizeof(S4LONG ) + b4->tag->header.keyLen ;

      for( ;; )  /* Repeat until the key is found */
      {
         int keyCur = (keyUpper + keyLower) / 2 ;
         int rc = u4memcmp( (((char *)&b4->nodeHdr) + keyCur * groupVal ), searchValue, (unsigned int)len ) ;

         if ( rc >= 0 )
         {
            keyUpper = keyCur ;
            saveRc = rc ;
         }
         else
            keyLower = keyCur ;

         if ( keyLower >= (keyUpper-1) )  /* then there is no exact match */
         {
            b4->keyOn = keyUpper ;   /* branch block, just change keyOn */
            if ( saveRc )
               return r4after ;
            return 0 ;
         }
      }
   }



   #ifdef S4VFP_KEY
   int u4tailCmp( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t sLen )
   {
      unsigned char *data = (unsigned char *)dataPtr ;
      unsigned char *search = (unsigned char *)searchPtr ;
      unsigned int on = 0 ;

      for ( ; on < sLen ; on++ )
      {
         if ( search[on] !=17 || ( data[on] >= 10 && data[on] != 17 ) )
            break ;
      }

      return on ;
   }
   #endif



   /* S4FOX */
   int b4leafSeek( B4BLOCK *b4, const char *searchValue, const int l )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 || searchValue == 0 || l < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      TAG4FILE *tag = b4->tag ;
      int originalLen = l ;
      int keyLen = tag->header.keyLen ;
      int len = l ;

      #ifdef S4VFP_KEY
         // for vfp key, we disclude values of '17' which are equivalent to blanks (0x20 : ' '), so they are spaces...
         if ( tfile4vfpKey( tag ) && tag->expr->type == r4str )
            for ( ; len > 0 && searchValue[len-1] == 17 ; len-- ) ;
         int skippedSpaces = ( len == originalLen ) ? 0 : 1 ;
      #endif

      /* FoxPro 2.6 has no blanks if filters exist */
      assert5( tag->indexFile->dataFile->compatibility == 30 || tag->indexFile->dataFile->compatibility == 25 || tag->indexFile->dataFile->compatibility == 26 ) ;
      if (! ( tag->indexFile->dataFile->compatibility == 26 && tag->filter != 0 ) )
         len -= b4calcBlanks( (unsigned char *)searchValue, len, tag->pChar ) ;  /* don't compare blank bytes */
      int lenLessBlanks = len ;
      char allBlank ;

      if ( lenLessBlanks == 0 )   /* if all blanks, watch for records < blank */
      {
         len = originalLen ;
         allBlank = 1 ;
      }
      else
         allBlank = 0 ;

      int duplicates = 0 ;
      int saveDupCnt = 0 ;
      b4top( b4 ) ;

      int done, trailCnt, bytesSame, compareLen, significantBytesInTagKey, cLen, loop ;
      for(;;)
      {
         // duplicates == # of duplicates for current key in tag from previous key in tag, curDupCnt = # of matching bytes we have found so far
         if ( b4->curDupCnt == duplicates )
         {
            significantBytesInTagKey = keyLen - x4trailCnt( b4, b4->keyOn ) ;
            if ( allBlank && significantBytesInTagKey == 0 )   /* found a blank record */
               len = 0 ;
            compareLen = ( len < significantBytesInTagKey ? len : significantBytesInTagKey ) - b4->curDupCnt ;
            bytesSame = (*tag->cmp)( b4->curPos, searchValue + b4->curDupCnt, (unsigned int)compareLen ) ;

            if ( bytesSame == -1 )  // means the comparison revelaed we have gone too far (search value > key value in tag)
               return r4after ;

            if ( bytesSame == compareLen )  // all the bytes we compared matched, but sometimes we don't compare all the characters (eg. if there wre trailing blanks in the search key)
            {
               if ( (b4->curDupCnt + bytesSame == len ) )
               {
                  // means that all the bytes we wanted to match (i.e. len - search key less blanks) matched - if binary values and were blanks on search key, maybe not done

                  if ( len == originalLen )  // we compared all the bytes from the initial seek and they matched, so for sure we have a complete match
                     done = 1 ;
                  else  /* in case of binary, we may not be done if values < blank */
                  {
                     // this occurs when all the bytes we compared matched, but len != originalLen (i.e. we trimmed off blanks from the original search key)
                     trailCnt = x4trailCnt( b4, b4->keyOn ) ;

                     /* AS 10/18/99 --> this comparison is incorrect.  Sometimes not doing proper check.
                        significantBytesInTagKey == keyLen-trailCnt.
                        What we really want is to consider the orginal length, not the current length
                        - i.e. if the orginal key length > # significant bytes (not a parital seek)...
                        if ( significantBytesInTagKey > trailCnt + len )
                     */
                     if ( len < significantBytesInTagKey )  /* significant bytes beyond the trail, so check binary */
                     {
                        if ( originalLen > significantBytesInTagKey )  /* significant bytes beyond the trail, so check binary */
                        {
                           // basically - the (original) search key included more precision than the # of significant bytes in the current
                           // tag key entry, but the 'len' (i.e. original key less trailing blanks) is less than the significant bytes.
                           // eg.  "search value = "abc   ", and the tag key is "abcd  "  - original len was 6, len is 3, significant == 4
                           // in this case, we want to do a comparison between 'd' and the ' ' of the search value.  if 'd' was in fact
                           // a binary value (say 0x10), then (0x10) is < blank (0x20), so in actual fact we have not gone far enough
                           // in the seek, so we need to continue

                           cLen = keyLen - trailCnt ;
                           if ( originalLen - len < cLen )
                              cLen = originalLen - len ;
                           if ( c4memcmp( b4->curPos + compareLen, searchValue + len, (unsigned int)cLen ) < 0 )  /* binaries */
                              done = 0 ;
                           else
                              done = 1 ;
                        }
                        else
                        {
                           // another case to consider here:  len < significantBytesInTagKey (i.e. we maybe trimmed off blanks)
                           // however, originalLen is <= significantBytesInTagKey.
                           // here is a small example:  searching for partial "abc  " (len 3), and found "abcdefg  " (significant 7),
                           // but origianl len is 6, which is < significant bytes

                           cLen = keyLen - trailCnt ;
                           if ( originalLen - len < cLen )
                              cLen = originalLen - len ;

                           // AS 03/14/00 there was the following problem here.
                           // had key: 0x000000010000201f and looking for 0x0000000010002020 in a char key
                           // --     - looking for:  len = 6, original len = 8
                           // in this case, the seek was realizing correctly whether done or not, but the curDupCnt was
                           // getting set incorrectly?  yes.  It is one of those instances where when we come out from this
                           // function we need to know that compared to the actual key, there were 7 duplicates, even though
                           // if we were to insert the search value we would get a value with 6 duplicates and 2 trails.
                           done = 0 ;
                           int allMatched = 1 ;
                           for ( int extraByteIndex = 0 ; extraByteIndex < cLen ; extraByteIndex++ )
                           {
                              // AS 06/13/00 must use unsigned to avoid signed comparisons!
                              unsigned char char1 = (b4->curPos + compareLen + extraByteIndex)[0] ;
                              unsigned char char2 = (searchValue + lenLessBlanks + extraByteIndex)[0] ;
                              assert5( char1 >= 0 && char2 >= 0 ) ;   // must be an unsigned comparison or we have serious algorithmic failure - should be unsigneds
                              if ( char1 == char2 ) // we have a match...
                              {
                                 bytesSame++ ;  // another byte does match
                                 len++ ;  // ensure that on next comparison we must include this len as part of the key...
                              }
                              else  // we are done, no match
                              {
                                 allMatched = 0 ;
                                 if ( char1 > char2 ) // not match, but we are done (r4after) since value greater
                                 {
                                    done = 1 ;
                                    // AS 06/13/00 - was not increasing compareLen as compared to len (t5samp6), so on line 2018 below was comparing wrong values
                                    compareLen += extraByteIndex ;
                                 }
                                 break ;
                              }
                           }
                           if ( allMatched ) // they all matched, so done...
                              done = 1 ;
                        }
                     }
                     else
                        done = 1 ;

                     // need to recheck 'len' comparison because we may have modified the value above
                     if ( done == 1 && len != originalLen && significantBytesInTagKey > len )
                     {
                        // we need to do a final comparison to see if we are on the key or after.  Generally this occurs when
                        // we had trailing blanks in the block's key data, so we could not do a complete comparison???
                        #ifdef S4VFP_KEY
                           if ( skippedSpaces )
                           {
                              cLen = originalLen - ( len < significantBytesInTagKey ? len : significantBytesInTagKey ) ;
                              if ( u4tailCmp( b4->curPos + compareLen, searchValue + len, cLen ) != cLen )
                                 return r4after ;
                           }
                           else
                        #endif
                           {
                              if ( significantBytesInTagKey < originalLen )
                                 return r4after ;
                              #ifdef E4ANALYZE
                                 if ( len > originalLen )
                                    return error4( tag->codeBase, e4info, E90438 ) ;
                              #endif
                              if ( (*tag->cmp)( b4->curPos + compareLen, searchValue + len, (unsigned int)(originalLen - len) ) != (originalLen - len) )
                                 return r4after ;
                            }
                     }
                  }

                  if ( done == 1 )
                  {
                     if ( allBlank != 1 )   /* not all blanks, so significants matter */
                     {
                        b4->curDupCnt += bytesSame ;
                        if ( bytesSame != 0 )
                           saveDupCnt = 0 ;
                     }
                     else
                     {
                        b4->curDupCnt = 0 ;
                        saveDupCnt = 0 ;
                     }
                     return 0 ;
                  }
               }
               else   /* this with lines below added AS 10/29/97 for fix #110 in changes.60 */
               {
                  /* it is possible in binary seeks that we are too far now this would happen if the next
                     byte to seek is < 32 and the search key has trailing blanks as next byte  */
                  if ( ( (unsigned char)searchValue[b4->curDupCnt + bytesSame] < (unsigned char)tag->pChar ) && ( (b4->curDupCnt + bytesSame) == significantBytesInTagKey ) )
                  {
                     /* then remaining bytes must be blank, we are too far */
                     return r4after ;
                  }
                  if ( ( searchValue[b4->curDupCnt + bytesSame] == tag->pChar ) && ( (b4->curDupCnt + bytesSame) == significantBytesInTagKey ) )
                  {
                     /* we may have a match -- if the rest of the seek key is < = blanks (since significant bytes is matched, only blanks must be left in found key)
                        then remaining bytes must be blank, we are too far */
                     for ( loop = b4->curDupCnt + bytesSame ; loop < originalLen ; loop++ )
                     {
                        // AS 05/05/00 if value < 32 we were not stopping, if later data was >32 we then were setting loop to -1 and breaking.  If we have < 32, then
                        // we must be at r4after position because our current data is all blank... at least true if not general collation
                        if ( (unsigned char)searchValue[loop] != (unsigned char)tag->pChar )  /* not found */
                        {
                           if ( (unsigned char)searchValue[loop] > (unsigned char)tag->pChar )  /* not found */
                           {
                              loop = -1 ;
                              break ;
                           }
                           else // must be <...
                              break ;
                        }
                     }
                     if ( loop != -1 )
                     {
                        // AS 03/16/00 - it is possible that we have a return of '0' (i.e. found), if both
                        // the current key and and the search key have == blanks at end (i.e. trail cnt = original len-significant bytes)
                        if ( saveDupCnt != 0 )  // AS 03/24/00 special case, we need to reset for forAdd
                           b4->curDupCnt = saveDupCnt ;
                        if ( x4trailCnt( b4, b4->keyOn ) == ( originalLen - lenLessBlanks ) )
                           return 0 ;
                        // otherwise not both blanks, so we are too far...
                        return r4after ;
                     }
                  }
               }
            }

            b4->curDupCnt += bytesSame ;
            if ( bytesSame != 0 )
               saveDupCnt = 0 ;
         }

         b4->keyOn++ ;
         if ( b4->keyOn >= b4numKeys( b4 ) )
            return r4after ;
         duplicates = x4dupCnt( b4, b4->keyOn ) ;
         #ifdef E4ANALYZE
            if ( keyLen - duplicates - x4trailCnt( b4, b4->keyOn ) < 0 )  /* index corrupt */
               return error4( tag->codeBase, e4index, E90438 ) ;
         #endif
         b4->curPos -= keyLen - duplicates - x4trailCnt( b4, b4->keyOn ) ;
         if ( b4->curDupCnt > duplicates )
         {
            /* AS 03/11/97 there are some rare cases where this will happen ok, and that is
               if our search key contains blanks at the front and greater than blank at current
               seek position, but current key goes from binary pre-blank (i.e. < 0x20) on current
               key to blank, at which point trail-cnt takes over for duplicates so that we get
               one less duplicate, but in fact it is duplicate due to blank!!!
               i.e. what happened was that 'blanks' caused the duplicates to go down in
               favor of trails
            */
            if ( searchValue[b4->curDupCnt-1] == 0x20 ) /* is a blank, so possibly the case */
               if ( keyLen - x4trailCnt( b4, b4->keyOn ) < b4->curDupCnt )  /* it has happened! */
                  if ( keyLen - x4dupCnt( b4, b4->keyOn ) - x4trailCnt( b4, b4->keyOn ) == 0 )
                  {
                     /* what should we do? I'd say just continue */
                     /* we actually have to reset the curDupCnt because we need to re-scan some keys
                        just subtract the number of blanks we encountered
                     */
                     saveDupCnt = b4->curDupCnt ;  // AS 03/24/00 - in case next key has blanks, indicate that current has duplicates
                     b4->curDupCnt = duplicates = x4dupCnt( b4, b4->keyOn ) ;
                     b4->curPos -= keyLen - duplicates - x4trailCnt( b4, b4->keyOn ) ;
                  }
         }
         if ( b4->curDupCnt > duplicates )
         {
            if ( allBlank )  /* must reset the duplicates in this case only */
            {
               b4->curDupCnt = duplicates ;
               saveDupCnt = 0 ;
               if ( x4trailCnt( b4, b4->keyOn ) == len )   /* blank key found */
                  return 0 ;
            }
            return r4after ;
         }
      }
   }



   /* S4FOX */
   #ifndef S4OFF_WRITE
   int b4removeLeaf( B4BLOCK *b4 )
   {
      // there are 3 keys involved in the process:
      // the 'left' key, i.e. the key left of the key being removed
      // the 'right' key, i.e. the key right of the key being removed
      // the 'remove' key, i.e. the key being removed
      /*
         Note:  Fox Format is as follows:
         - if there are trailing blanks, they are always included... eg a blank key would have 0 duplicates all blanks always,
           never duplicates from previous blank keys
         - duplicates never take into account trailing blanks.  Therefore, you cannot have keylen = 10, <trail=10><dup=4>  This
           is invalid because duplicates cannot take trail into account.  Instead, you would have <dup=0> key "    xxxxxx"
      */
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      #ifdef I4PRINT
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255];
            sprintf( dump, "b4removeLeaf called for block: %ld, keyOn =%ld, NumKeys = %ld \r\n", (long)b4node(b4->fileBlock), (long)b4->keyOn, (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      #ifdef E4ANALYZE
         // AS Jan 18/02 - ensure that exactly 1 key is removed from block
         long numKeysInBlock = b4numKeys( b4 ) ;
      #endif

      b4->builtOn = -1 ;

      if ( b4numKeys( b4 ) == 1 )  /* removing last key */
      {
         b4->nodeHdr.freeSpace = (short)i4blockSize( b4->tag->indexFile ) - sizeof( B4NODE_HEADER ) - sizeof( B4STD_HEADER ) ;
         c4memset( b4->data, 0, (unsigned int)b4->nodeHdr.freeSpace ) ;
         b4->header.nKeys = 0 ;
         b4->keyOn = 0 ;
         b4->curPos = ((char *)&b4->header) + i4blockSize( b4->tag->indexFile ) ;
         b4->changed = 1 ;
         #ifdef I4PRINT
            #ifdef I4PRINT_TAG_NAME
               if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
            #endif
            {
               char dump[255];
               sprintf( dump, "removed last key from block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
               log5( dump ) ;
            }
         #endif
         return 0 ;
      }

      int keyLen = b4->tag->header.keyLen ;
      unsigned char iLen = b4->nodeHdr.infoLen ;
      int removeKeyDup = x4dupCnt( b4, b4->keyOn) ;  // # duplicate bytes in key we are removing

      int removeLen ;
      if ( b4->keyOn == b4numKeys( b4 ) - 1 )  /* at end */
      {
         removeLen = keyLen - removeKeyDup - x4trailCnt( b4, b4->keyOn ) ;

         #ifdef E4ANALYZE
            if ( removeLen < 0 )
            {
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255];
                     sprintf( dump, "b4removeLeaf detected index corruption for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                     log5( dump ) ;
                  }
               #endif
               return error4( b4->tag->codeBase, e4info, E80401 ) ;
            }
         #endif

         c4memset( b4->curPos, 0, (unsigned int)removeLen ) ;
         c4memset( b4->data + b4->keyOn * iLen, 0, iLen ) ;
         b4->keyOn-- ;
      }
      else
      {
         int removeKeyTrail = x4trailCnt( b4, b4->keyOn ) ;  // # trail bytes in key we are removing
         int removeKeyBytes = keyLen - removeKeyDup - removeKeyTrail ;  // # bytes stored as data for key we are removing (i.e. key len less duplicates and trail)
         char *oldPos = b4->curPos ;
         b4skip( b4, 1L ) ;
         int oldRightKeyDup = x4dupCnt( b4, b4->keyOn) ;
         int oldRightKeyTrail = x4trailCnt( b4, b4->keyOn ) ;
         // AS 03/17/00 there is a problem in the following scenario...
         // keys 0x...201f (dup=7, tr=0), 0x...2020,(dup=6,tr=2) 0x...2021(dup=6,tr=0)
         // if removing 2020, get 0x...201f (dup=7, tr=0), 0x...2021(dup=6,tr=0)
         // but this is wrong, dup should be 7.
         // probably need to do some detection/rebuilding in this case.
         // to see if this is case, if removing key has trail bytes and existing key
         // has blanks at the 1st position in the curpos, then we have this case...
         int newRightKeyDup = -1 ;   // the new # of duplicates in the 'right' key

         // AS 04/26/00 was not taking into account when removing the first key in the tag, in which case this does not apply but was failing
         Bool5 doKeyExamination = 0 ;  // some cases need to do a further key analysis
         if ( (b4->keyOn >= 2 ) && ( oldRightKeyDup + oldRightKeyTrail < keyLen ) )  // we actually have data at curPos
         {
            if ( b4->tag->pChar != 0 )
            {
               if ( ( b4->curPos[0] == b4->tag->pChar ) )
               {
                  // now, if the previous key had trail bytes and dup+trail == keylen,
                  // then we have the instance where it could be that we are losing
                  // information... - doesn't apply if removeKeyTrail == keyLen since then is all blank and we don't include trails as duplicates
                  // AS July 24/01 - If the key is all blank, we still may need to do investigation
                  // this makes sense from the point of view that we have a completely blank key
                  // in the middle of binaries (try t4indx2.c with param of 35 to show)
                  // if ( removeKeyTrail != keyLen )
                     if ( removeKeyTrail + removeKeyDup == keyLen )
                        doKeyExamination = 1 ;
               }
            }
         }

         if ( doKeyExamination == 1 )
         {
            #ifdef I4PRINT
               #ifdef I4PRINT_TAG_NAME
                  if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
               #endif
               {
                  char dump[255];
                  sprintf( dump, "doing key examination in remove for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                  log5( dump ) ;
               }
            #endif

            // need to do an analysis of the previous key and the 'next' key
            // to see what the real dup count is...
            // this is a very unusual case, so if a bit slower here, that is ok
            // char *oldBuiltKeyValue = (char *)b4->builtKey->value ;
            int saveKeyOn = b4->keyOn ;
            char *key1 = (char *)u4allocFree( b4->tag->codeBase, keyLen ) ;
            char *key2 = (char *)u4allocFree( b4->tag->codeBase, keyLen ) ;
            if ( key1 == 0 || key2 == 0 )
            {
               if ( key1 != 0 )
                  u4free( key1 ) ;
               if ( key2 != 0 )
                  u4free( key2 ) ;
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255];
                     sprintf( dump, "out of memory in remove block for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                     log5( dump ) ;
                  }
               #endif

               return error4( b4->tag->codeBase, e4memory, E90438 ) ;
            }
            c4memcpy( key1, b4key( b4, saveKeyOn - 2 )->value, keyLen ) ;
            c4memcpy( key2, b4key( b4, saveKeyOn )->value, keyLen ) ;

            newRightKeyDup = 0 ;
            for ( int keyIndex = 0 ; keyIndex < keyLen ; keyIndex++ )
            {
               if ( key1[keyIndex] != key2[keyIndex] )
                  break ;
               newRightKeyDup++ ;
            }

            // AS 04/26/01 - Was incorrectly allowing duplicate trail bytes.  In point of fact this is invalid.
            // for eg. if key1 = "ab     " (0 dup, 4 trail) and key2 == "ab  c ", this gets recored as 2 dup, 1 trail
            // and then the bytes are "  c".  saying dup of 4 (including blanks) is invalid in this case
            int key1trailCount = x4trailCnt( b4,  saveKeyOn - 2 ) ;
            int maxDupCnt = keyLen - key1trailCount ;
            if ( newRightKeyDup > maxDupCnt )
               newRightKeyDup = maxDupCnt ;

            assert5( b4->keyOn == saveKeyOn ) ;
            b4->builtOn = -1 ;
            u4free( key1 ) ;
            u4free( key2 ) ;
         }

         if ( newRightKeyDup == -1 )  // if not special calculated case above...
         {
            // AS 06/08/00 case if removing key is all blank and next key has 'blank' duplicates  (eg. "     " (5 bl), "   12" (3 dup)
            //  in this instance, the 'newRightKeyDup' bytes can be left as is if the previous key
            // (i.e. the key left of that being removed) is also all blank...
            Bool5 leaveDups = 0 ;
            if ( removeKeyTrail == keyLen )
               if ( b4->keyOn - 2 >= 0 )  // there is a key left of removal one
                  if ( x4trailCnt( b4, b4->keyOn - 2 ) == keyLen )
                     leaveDups = 1 ;

            if ( leaveDups == 1 )
               newRightKeyDup = oldRightKeyDup ;
            else
            {
               /* LY 00/03/22 : replaced min() for compilers that don't support it */
               newRightKeyDup = ( removeKeyDup < oldRightKeyDup ? removeKeyDup : oldRightKeyDup ) ;
            }
         }

         // we need to copy the significant bytes from the removed key into the key area of the new key.
         // we want all the bytes from that area that are significant to the new area (i.e. that are not
         // overwritten by the new ones, i.e. our duplicates.)
         int numBytesToCopyFromRemovedKey = keyLen - oldRightKeyTrail - oldRightKeyDup ;
         int newRightKeyBytes = keyLen - oldRightKeyTrail - newRightKeyDup ;

         // use the builtkey->value as a buffer to temporarily hold the information for copying.
         char *copyBuffer = (char *)b4->builtKey->value ;

         int lenToCopy = oldRightKeyDup - newRightKeyDup ;
         // there is a situation where lenToCopy could be < 0.  This is the instance where we remove a key
         // with trailing bytes and there is binary info on either side (eg. 0x2019, 0x2020(blank), 0x2021, removing 0x2020)
         // in that case, the # of duplicates for the right key actually increases
         if ( lenToCopy < 0 )
         {
            // populate the copyBuffer with the data we need to copy.  In this case, since
            // lenToCopy is < 0, we actually do not need to copy anything, we are only
            // going to want to remove data
            // c4memcpy( copyBuffer, oldPos, lenToCopy ) ;  /* copy prev dup bytes between the 2 keys only */
            // c4memcpy( copyBuffer, b4->curPos, (unsigned int)numBytesToCopyFromRemovedKey ) ;
            // c4memcpy( oldPos + removeKeyBytes - newRightKeyBytes, copyBuffer, (unsigned int)newRightKeyBytes ) ;
         }
         else
         {
            c4memcpy( copyBuffer, oldPos, lenToCopy ) ;  /* copy prev dup bytes between the 2 keys only */
            #ifdef EXCEPTION4REINDEX
               // in this case recover by reindexing
               if ( numBytesToCopyFromRemovedKey < 0 )
               {
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255];
                        sprintf( dump, "block corruption in remove for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                        log5( dump ) ;
                     }
                  #endif
                  return error4( b4->tag->codeBase, e4index, E80401 ) ;
               }
               if ( newRightKeyBytes < 0 )
               {
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255];
                        sprintf( dump, "block corruption in remove for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                        log5( dump ) ;
                     }
                  #endif
                  return error4( b4->tag->codeBase, e4index, E80401 ) ;
               }
            #endif
            c4memcpy( copyBuffer + ( oldRightKeyDup - newRightKeyDup ), b4->curPos, (unsigned int)numBytesToCopyFromRemovedKey ) ;
            c4memcpy( oldPos + removeKeyBytes - newRightKeyBytes, copyBuffer, (unsigned int)newRightKeyBytes ) ;
         }

         removeLen = removeKeyBytes + numBytesToCopyFromRemovedKey - newRightKeyBytes ;
         #ifdef E4ANALYZE
            if ( removeLen < 0 )
            {
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255];
                     sprintf( dump, "block corruption in remove for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                     log5( dump ) ;
                  }
               #endif
               return error4( b4->tag->codeBase, e4info, E80401 ) ;
            }
         #endif

         if ( removeLen > 0 )
         {
            int movLen = i4blockSize( b4->tag->indexFile ) - sizeof( B4STD_HEADER ) - iLen * b4numKeys( b4 )
                      - sizeof( B4NODE_HEADER ) - b4->nodeHdr.freeSpace - ( ( (char *)&b4->header ) + i4blockSize( b4->tag->indexFile ) - b4->curPos ) ;
            #ifdef E4ANALYZE
               if ( movLen < 0 )
               {
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255];
                        sprintf( dump, "block corruption in remove for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                        log5( dump ) ;
                     }
                  #endif
                  return error4( b4->tag->codeBase, e4info, E80401 ) ;
               }
            #endif

            #ifdef EXCEPTION4REINDEX
               // in this case recover by reindexing
               if ( movLen < 0 )
               {
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255];
                        sprintf( dump, "block corruption in remove for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                        log5( dump ) ;
                     }
                  #endif
                  return error4( b4->tag->codeBase, e4index, E80401 ) ;
               }
            #endif
            c4memmove( b4->curPos - movLen + removeLen, b4->curPos - movLen, (unsigned int)movLen ) ;

            c4memset( b4->curPos - movLen, 0, (unsigned int)removeLen ) ;
         }
         b4->keyOn-- ;

         /* move and put info */
         char *infoPos = b4->data + ( b4->keyOn ) * iLen ;
         int memToCopy = (iLen * ( b4numKeys( b4 ) - b4->keyOn ) ) ;
         #ifdef EXCEPTION4REINDEX
            // in this case recover by reindexing
            if ( memToCopy < 0 )
            {
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     char dump[255];
                     sprintf( dump, "block corruption in remove for block: %ld\r\n", (long)b4node(b4->fileBlock) ) ;
                     log5( dump ) ;
                  }
               #endif
               return error4( b4->tag->codeBase, e4index, E80401 ) ;
            }
         #endif
         c4memmove( infoPos, infoPos + iLen, (unsigned int)memToCopy ) ;
         unsigned char buffer[6] ;
         // AS 05/01/00 verify data is valid going in...
         assert5( ( oldRightKeyTrail + newRightKeyDup ) <= b4->tag->header.keyLen ) ;
         x4putInfo( &b4->nodeHdr, buffer, x4recNo( b4, b4->keyOn ), oldRightKeyTrail, (unsigned int)newRightKeyDup ) ;
         c4memcpy( infoPos, (void *)buffer, iLen ) ;
         c4memset( b4->data + (b4numKeys( b4 ) - 1 ) * iLen, 0, iLen ) ;
      }
      b4->changed = 1 ;
      b4->curPos += removeLen ;
      b4->header.nKeys-- ;
      b4->nodeHdr.freeSpace += (short)( (int)iLen + removeLen ) ;

      #ifdef E4ANALYZE
         // AS Jan 18/02 - ensure that exactly 1 key is removed from block
         if ( b4numKeys( b4 ) != numKeysInBlock - 1 )
         {
            // log4invokeDebuggerDo() ;
            return error4( b4->tag->codeBase, e4index, E80401 ) ;
         }
      #endif

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255] ;
            sprintf( dump, "     after removal: num keys in block %ld \r\n", (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      return 0 ;
   }



   /* S4FOX */
   int b4remove( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      if ( b4->header.nodeAttribute >= 2 ) /* leaf */
         return b4removeLeaf( b4 ) ;

      #ifdef I4PRINT
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( b4->tag->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            char dump[255];
            sprintf( dump, "b4remove brach called for block: %ld, keyOn =%ld, NumKeys = %ld \r\n", (long)b4node(b4->fileBlock), (long)b4->keyOn, (long)b4numKeys(b4) ) ;
            log5( dump ) ;
         }
      #endif

      int iLen = b4->tag->header.keyLen + 2 * sizeof(S4LONG ) ;
      char *keyCur = ((char *)&b4->nodeHdr) + iLen * b4->keyOn ;
      int len = (b4numKeys( b4 ) - b4->keyOn - 1) * iLen ;

      #ifdef E4ANALYZE
         if ( b4->keyOn < 0  || b4->keyOn > b4lastpos( b4 ) || len < 0 || (unsigned)len > (i4blockSize( b4->tag->indexFile ) - sizeof(B4STD_HEADER) - (unsigned)iLen ))
            return error4( b4->tag->codeBase, e4info, E90438 ) ;
      #endif

      if ( len > 0 )
         c4memmove( keyCur, keyCur + iLen, (unsigned int)len ) ;

      b4->header.nKeys-- ;
      c4memset( ((char *)&b4->nodeHdr) + b4numKeys( b4 ) * iLen, 0, (unsigned int)iLen ) ;
      b4->changed = 1 ;

      return 0 ;
   }
   #endif /* S4OFF_WRITE */



   /* S4FOX - AS Nov 13/02 - export for dot */
   int S4FUNCTION b4skip( B4BLOCK *b4, int numToSkip )  // can only skip by int amount within block
   {

      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      int nKeys = b4numKeys( b4 ) ;

      if ( b4->header.nodeAttribute < 2 )  /* branch */
      {
         int numSkipped ;
         if ( numToSkip > 0 )
            numSkipped = nKeys - b4->keyOn ;
         else
            numSkipped = -b4->keyOn ;

         if ( ( numToSkip <= 0L ) ? ( numSkipped <= numToSkip ) : (numSkipped >= numToSkip ) )
         {
            b4->keyOn += numToSkip ;
            return numToSkip ;
         }
         else
         {
            b4->keyOn += numSkipped ;
            return numSkipped ;
         }
      }
      else  /* leaf */
      {
         if ( nKeys == 0 )
            return 0 ;
         int kLen = b4->tag->header.keyLen ;
         if ( numToSkip > 0 )
         {
            if ( b4->keyOn + numToSkip >= nKeys )
            {
               b4->curPos = ((char *)&b4->header ) + sizeof( B4STD_HEADER ) + sizeof( B4NODE_HEADER )
                             + b4->nodeHdr.infoLen * nKeys + b4->nodeHdr.freeSpace ;
               numToSkip = nKeys - b4->keyOn - ( b4->keyOn != nKeys ) ;
               b4->keyOn = nKeys ;
               return numToSkip ;
            }
            for ( int skipIndex = numToSkip ; skipIndex != 0 ; skipIndex-- )
            {
               b4->keyOn++ ;
               b4->curPos -= ( kLen - x4dupCnt( b4, b4->keyOn ) - x4trailCnt( b4, b4->keyOn ) ) ;
            }
         }
         else
         {
            if ( b4->keyOn + numToSkip < 0 )
            {
               numToSkip = -b4->keyOn ;
               b4->keyOn = 0 ;
               b4->curPos = ((char *)&b4->header ) + i4blockSize( b4->tag->indexFile ) - kLen + x4trailCnt( b4, b4->keyOn ) ;
               return numToSkip ;
            }
            for( int skipIndex = numToSkip ; skipIndex != 0 ; skipIndex++ )
            {
               b4->curPos += ( kLen - x4dupCnt( b4, b4->keyOn ) - x4trailCnt( b4, b4->keyOn ) ) ;
               b4->keyOn-- ;
            }
         }

         return numToSkip ;
      }
   }

   #endif  /* S4FOX */

   #if defined( S4CLIPPER ) && !defined( S4OFF_WRITE )
      int b4doFlush( B4BLOCK *b4 )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
               return error4( 0, e4parm_null, E90438 ) ;
         #endif

         assert5( b4->changed ) ;   /* this function only gets called by b4flush() which checks this flag */

         #ifdef E4INDEX_VERIFY
            if ( b4verify( b4 ) == -1 )
               error4describe( b4->tag->codeBase, e4index, E90438, b4->tag->alias, 0, 0 ) ;
         #endif

         #ifndef S4OFF_OPTIMIZE
            b4->tag->readBlockTag = b4->tag ;
         #endif

         #ifdef S4BYTE_SWAP
            CHAR *swap = (char *)u4allocEr( b4->tag->codeBase, i4blockSize( b4->tag->indexFile ) ) ;
            if ( swap == 0 )
               return -1 ;

            c4memcpy( (void *)swap, (void *)&b4->nKeys, i4blockSize( b4->tag->indexFile ) ) ;

            index4swapBlockClipper(swap, b4->tag->header.keysMax, b4->tag->header.groupLen) ;

            int rc = file4writeInternal( &b4->tag->file, (unsigned long)b4->fileBlock * I4MULTIPLY, swap, i4blockSize( b4->tag->indexFile ) ) ;
            u4free( swap ) ;
         #else
            FILE4LONG pos ;
            file4longAssign( pos, b4->fileBlock, 0 ) ;
            file4longMultiply( pos, I4MULTIPLY ) ;
            int rc = file4writeInternal( &b4->tag->file, pos, &b4->nKeys, i4blockSize( b4->tag->indexFile ) ) ;
         #endif

         #ifndef S4OFF_OPTIMIZE
            b4->tag->readBlockTag = 0 ;
         #endif

         if ( rc < 0 )
            return rc ;

         b4->changed = 0 ;
         return 0 ;
      }
   #endif /* defined( S4CLIPPER ) && !defined( S4OFF_WRITE ) */


   #if defined( S4CLIPPER ) && !defined( S4OFF_WRITE )
      int b4append( B4BLOCK *b4, const unsigned long pointer )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || pointer == 0L || pointer == INVALID4BLOCK_ID )
               return error4( 0, e4parm, E90438 ) ;
         #endif

         #ifdef E4ANALYZE
            if ( b4leaf( b4 ) )
               return error4( b4->tag->codeBase, e4info, E90438 ) ;
         #endif

         b4goEof( b4 ) ;
         B4KEY_DATA *dataPtr = b4key( b4, b4->keyOn ) ;

         #ifdef E4ANALYZE
            int leftLen = i4blockSize( b4->tag->indexFile ) - b4->keyOn * b4->tag->header.groupLen - sizeof(short) - sizeof(char[2]) - sizeof(pointer) ;
            if ( b4->keyOn < 0  ||  b4->keyOn != b4numKeys( b4 ) || leftLen < 0 )
                return error4( b4->tag->codeBase, e4info, E90438 ) ;
         #endif

         unsigned long adjPointer = pointer * 512 ;

         c4memcpy( &dataPtr->pointer, &adjPointer, sizeof(pointer) ) ;
         b4->changed = 1 ;
         #ifdef E4INDEX_VERIFY
            if ( b4verify( b4 ) == -1 )
               return error4describe( b4->tag->codeBase, e4index, E90438, b4->tag->alias, 0, 0 ) ;
         #endif

         return 0 ;
      }
   #endif /* defined( S4CLIPPER ) && !defined( S4OFF_WRITE ) */




   #if defined( S4CLIPPER ) && !defined( S4OFF_WRITE ) && defined( E4DEBUG )
      void b4verifyPointers( const B4BLOCK *b4 )
      {
         // AS 05/19/99 -- do a little block verification...
         // due to max key len problem, cannot move last pointer around - must be a partial key only
         int lastPointer = b4->pointers[b4->tag->header.keysMax] ;

         for ( int blockPointersIndex = 0 ; blockPointersIndex < b4->tag->header.keysMax ; blockPointersIndex++ )
         {
            if ( b4->pointers[blockPointersIndex] >= lastPointer )
            {
               error4( b4->tag->codeBase, e4index, E90438 ) ;
               assert5( b4->pointers[blockPointersIndex] < lastPointer ) ;
            }
            for ( int j = blockPointersIndex + 1 ; j <= b4->tag->header.keysMax ; j++ )
            {
               assert5( b4->pointers[blockPointersIndex] != b4->pointers[j] ) ;
            }
         }
      }
   #endif



   #if defined( S4CLIPPER ) && !defined( S4OFF_WRITE )
      int b4append2( B4BLOCK *b4, const void *k, const unsigned long r, const unsigned long pointer )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || k == 0 || r < 0L || pointer == INVALID4BLOCK_ID )
               return error4( 0, e4parm, E90438 ) ;
         #endif

         b4goEof( b4 ) ;  // position keyOnto the end of the block...
         #ifdef E4DEBUG
            b4verifyPointers( b4 ) ;
         #endif

         if ( b4numKeys( b4 ) > 0 )
         {
            if ( !b4leaf( b4 ) )   /* in branch case, we have one 'extra' key reference, so don't overwrite that, instead use the next entry... */
            {
               b4->keyOn++ ;
               // AS 05/18/99 --> if we are using the maximum key size, there is a little bit less room actually
               // available in the index blocks.  In this case, we can't actually use the last entry to actually
               // fit a key in.  Therefore, we have to consider that case here now.  To get around that problem,
               // we need to actually do a true key swap here (basically just move the reference into the next
               // slot).
               if ( b4->keyOn == b4->tag->header.keysMax )
               {
                  #ifdef E4DEBUG
                     b4verifyPointers( b4 ) ;
                  #endif
                  // swap the pointers and then place the reference...
                  unsigned short tempPointer = b4->pointers[b4->keyOn] ;
                  b4->pointers[b4->keyOn] = b4->pointers[b4->keyOn-1] ;
                  b4->pointers[b4->keyOn-1] = tempPointer ;
                  B4KEY_DATA *toKeyPtr = b4key( b4, b4->keyOn - 1 ) ;
                  B4KEY_DATA *fromKeyPtr = b4key( b4, b4->keyOn ) ;
                  c4memcpy( &toKeyPtr->pointer, &fromKeyPtr->pointer, sizeof( long ) ) ;
                  // AS 05/19/99 --> cannot verify pointers yet because they may be bad until operation below finished
                  // #ifdef E4DEBUG
                  //    b4verifyPointers( b4 ) ;
                  // #endif
               }
            }
            // else
            // {
               // AS 05/18/99 --> in leaf case, was doing a swap of the entries here.  There appears to be no
               // good reason for this, and in fact causes problems (t5samp6.cpp) when key size == max size,
               // in which case the 'last' entry can be used for branch blocks references only.

               // if ( b4->keyOn < b4->tag->header.keysMax )
               // {
               //    // swap pointers[eof] and pointers[eof+1]...why???
               //    unsigned short temp = b4->pointers[b4numKeys( b4 )+1] ;
               //    b4->pointers[b4numKeys( b4 )+1] = b4->pointers[b4numKeys( b4 )] ;
               //    b4->pointers[b4numKeys( b4 )] = temp ;
               // }
            // }
         }

         // returns a pointer to the 'last+1' entry in the block, which is where we place the data...
         B4KEY_DATA *dataPtr = b4key( b4, b4->keyOn ) ;
         unsigned long adjPointer = pointer * 512 ;

         b4->nKeys++ ;

         // AS 05/18/99 --> ensure we do not overwrite the block...
         assert5( b4->pointers[b4->keyOn] + b4->tag->header.groupLen-2*sizeof(S4LONG) < i4blockSize( b4->tag->indexFile ) ) ;

      // AS 05/19/99 --> cannot verify pointers yet because they may be bad until operation below finished
      //   #ifdef E4DEBUG
      //      b4verifyPointers( b4 ) ;
      //   #endif
         c4memcpy( dataPtr->value, k, b4->tag->header.groupLen - 2 * sizeof( S4UNSIGNED_LONG ) ) ;
         c4memcpy( &dataPtr->num, &r, sizeof( r ) ) ;
         c4memcpy( &dataPtr->pointer, &adjPointer, sizeof(pointer) ) ;
      // AS 05/19/99 --> cannot verify pointers yet because they may be bad until operation below finished
      //   #ifdef E4DEBUG
      //      b4verifyPointers( b4 ) ;
      //   #endif
         b4->changed = 1 ;

         return 0 ;
      }
   #endif /* defined( S4CLIPPER ) && !defined( S4OFF_WRITE ) */


   #if defined( S4CLIPPER ) && !defined( S4OFF_WRITE )
      int b4insert( B4BLOCK *b4, const void *k, const unsigned long r, const unsigned long pointer )
      {
         /* clipper just puts at end and inserts into the index part insert means place before cur pos*/
         #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4data1.C and HP-UX */
            S4LONG tempPointer ;
         #endif
         #ifdef E4PARM_LOW
            if ( b4 == 0 || k == 0 || r == INVALID4BLOCK_ID || pointer == INVALID4BLOCK_ID )
               return error4( 0, e4parm, E90438 ) ;
         #endif

         #ifdef E4DEBUG
            b4verifyPointers( b4 ) ;
         #endif

         short insertPos = b4->keyOn ;

         /* put at block end: */
         assert5( b4->data != 0 ) ;
         b4append2( b4, k, r, pointer ) ;
         assert5( b4->data != 0 ) ;

      // AS 05/19/99 --> cannot verify pointers yet because they may be bad until operation below finished
      //   #ifdef E4DEBUG
      //      b4verifyPointers( b4 ) ;
      //   #endif

         short temp = b4->pointers[b4->keyOn] ;   /* save the placed position */

         int moveLen = sizeof(short) * ( b4lastpos( b4 ) - insertPos - (!b4leaf( b4 ) && b4numKeys( b4 ) < 2 ) ) ;
         #ifdef EXCEPTION4REINDEX
            // in this case recover by reindexing
            if ( moveLen < 0 )
               return error4( b4->tag->codeBase, e4index, E80401 ) ;
         #endif

         c4memmove( &b4->pointers[insertPos+1], &b4->pointers[insertPos], moveLen ) ;
         b4->pointers[insertPos] = temp ;

         #ifdef E4DEBUG
            b4verifyPointers( b4 ) ;
         #endif

         if ( b4leaf( b4 ) )
         {
            #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4data1.C and HP-UX */
              c4memcpy( &tempPointer, &b4key( b4, b4numKeys( b4 ) )->pointer, sizeof(S4LONG) ) ;
               if ( tempPointer != 0 )
                  c4memcpy( &b4key( b4, b4numKeys( b4 ) )->pointer, 0, sizeof(S4LONG) ) ;
            #else
               if ( b4key( b4, b4numKeys( b4 ) )->pointer != 0 )
                  b4key( b4, b4numKeys( b4 ) )->pointer = 0L ;
            #endif
         }

         return 0 ;
      }
   #endif /* S4OFF_WRITE */



   #ifdef S4CLIPPER
   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4data1.cpp and HP */
      int b4leaf( const B4BLOCK *b4 )
      {
         S4LONG tempPointer ;
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
               return error4( 0, e4parm_null, E90438 ) ;
         #endif

         c4memcpy( &tempPointer, &b4key( b4, 0 )->pointer, sizeof(S4LONG) ) ;
         return( tempPointer == 0L ) ;
      }


      unsigned long b4recNo( const B4BLOCK *b4, const int i )
      {
         // returns ULONG_MAX (i.e. -1) in case of error
         S4LONG tempNum ;
         #ifdef E4PARM_LOW
            if ( b4 == 0 || i < 0 )
            {
               error4( 0, e4parm, E90438 ) ;
               return ULONG_MAX ;
            }
         #endif

         c4memcpy( &tempNum, &b4key( b4, i )->num, sizeof(S4LONG) ) ;
         return tempNum ;
      }
   #endif


   #ifndef S4INLINE
      void b4goEof( B4BLOCK *b4 )
      {
         /* goes to one past the end of the block */
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
            {
               error4( 0, e4parm_null, E90438 ) ;
               return ;
            }
         #endif

         b4->keyOn = b4numKeys( b4 ) ;

         return 0 ;
      }
   #endif /* S4INLINE */



   #ifndef S4INLINE
      B4KEY_DATA *b4key( const B4BLOCK *b4, const int iKey )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || iKey < 0 )
            {
               error4( 0, e4parm, E90438 ) ;
               return 0 ;
            }
         #endif

         #ifdef E4ANALYZE
            if ( iKey > 2 + b4->tag->header.keysMax )
            {
               error4( b4->tag->codeBase, e4parm, E90438 ) ;
               return 0 ;
            }

            if ( ( b4->pointers[iKey] < (short)((sizeof(short)) * ( b4->tag->header.keysMax + 1 ) ) ) ||
               ( b4->pointers[iKey] > i4blockSize( b4->tag->indexFile ) - b4->tag->header.keyLen ) )
            {
               error4( b4->tag->codeBase, e4info, E90438 ) ;
               return 0 ;
            }
         #endif
         return  (B4KEY_DATA *)((char *)&b4->nKeys + b4->pointers[iKey] ) ;
      }
   #endif /* S4INLINE */



   #ifndef S4INLINE
      unsigned char *b4keyKey( B4BLOCK *b4, const int iKey )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 || iKey < 0 )
            {
               error4( 0, e4parm, E90438 ) ;
               return 0 ;
            }
         #endif
         return (unsigned char *) b4key( b4, iKey )->value ;
      }
   #endif /* S4INLINE */



   #ifndef S4INLINE
      int b4lastpos( const B4BLOCK *b4 )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
               return error4( 0, e4parm_null, E90438 ) ;
         #endif

         return ( ( b4leaf( b4 ) ) ? ( b4numKeys( b4 ) - 1 ) : ( b4numKeys( b4 ) ) ) ;
      }
   #endif /* S4INLINE */



   /* S4CLIPPER */
   int b4dataLeaf( void *data, TAG4FILE *tag )
   {
      /* based on the actual stored-data (from file) only, determine whether or
         not a leaf.  Used by file optimization routines */
      B4KEY_DATA *keyData ;
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4index5 and HP */
         S4LONG tempPointer ;
      #endif

      #ifdef E4PARM_LOW
         if ( data == 0 || tag == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      keyData = (B4KEY_DATA *)( (char *)data + *((short *)((char *)data + sizeof(short))) ) ;

      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4index5 and HP */
         c4memcpy( &tempPointer, &keyData->pointer, sizeof(S4LONG) ) ;
         return( tempPointer == 0L ) ;
      #else
         return( keyData->pointer == 0L ) ;
      #endif
   }



   /* S4CLIPPER */
   int b4remove( B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      #ifdef E4DEBUG
         b4verifyPointers( b4 ) ;
      #endif

      /* just delete this entry */
      short temp = b4->pointers[b4->keyOn] ;

      b4->changed = 1 ;
      assert5( b4->data != 0 ) ;

      // AS 05/18/99 -- should never use the 'last' entry because it may be reserved for branch
      // use only (i.e. there may be insufficient room to actually hold key data if maximum key
      // length is being used).  This code seemed irrelevant because of this new requirement,
      // so removing...
      // if ( b4leaf( b4 ) && ( b4->keyOn < b4->tag->header.keysMax ) )
      // {
      //
      //    // perform the removal by moving the
      //
      //    int numKeysToCopy = b4lastpos( b4 ) - b4->keyOn + 1 ;
      //    int lenToCopy = sizeof(short) * numKeysToCopy ;
      //
      //    c4memmove( &b4->pointers[b4->keyOn], &b4->pointers[b4->keyOn+1], lenToCopy ) ;
      //    b4->pointers[b4lastpos( b4 )+1] = temp ;
      // }
      // else
      // {

      if ( b4leaf( b4 ) )
      {
         int numKeysToCopy = b4lastpos( b4 ) - b4->keyOn ;
         int lenToCopy = sizeof(short) * numKeysToCopy ;

         c4memmove( &b4->pointers[b4->keyOn], &b4->pointers[b4->keyOn+1], lenToCopy ) ;
         b4->pointers[b4lastpos( b4 )] = temp ;
      }
      else
      {
         // AS 05/18/99 -- should never use the 'last' entry because it may be reserved for branch
         // use only (i.e. there may be insufficient room to actually hold key data if maximum key
         // length is being used).
         int numKeysToCopy = b4lastpos( b4 ) - b4->keyOn - 1 ;
         if ( numKeysToCopy >= 0 )  // don't do if we are removing the 'last' key (no work required in this case)
         {
            int lenToCopy = sizeof(short) * numKeysToCopy ;

            c4memmove( &b4->pointers[b4->keyOn], &b4->pointers[b4->keyOn+1], lenToCopy ) ;
            b4->pointers[b4lastpos( b4 )-1] = temp ;
            // AS 05/19/99 --> now must move the reference from lastpos+1 to lastPos
            B4KEY_DATA *toKeyPtr = b4key( b4, b4lastpos( b4 )-1 ) ;
            B4KEY_DATA *fromKeyPtr = b4key( b4, b4lastpos( b4 ) ) ;
            c4memcpy( &toKeyPtr->pointer, &fromKeyPtr->pointer, sizeof( long ) ) ;
         }
      }

      // }

      b4->nKeys-- ;

      assert5( b4->data != 0 ) ;
      if ( b4leaf( b4 ) )   // empty out the keuy...
      {
         // AS 05/18/99 -- should never use the 'last' entry because it may be reserved for branch
         // use only - changed as above...
         c4memset( b4key( b4, b4lastpos( b4 ) + 1 ), 0, b4->tag->header.keyLen + 2 * sizeof(S4LONG ) ) ;
         assert5( b4->data != 0 ) ;
      }

      #ifdef E4DEBUG
         b4verifyPointers( b4 ) ;
      #endif

      return 0 ;
   }



   int b4room( const B4BLOCK *b4 )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      #ifdef E4DEBUG
         b4verifyPointers( b4 ) ;
      #endif

      if ( b4leaf( b4 ) )
         return ( b4numKeys( b4 ) < b4->tag->header.keysMax ) ;

      return( ( b4numKeys( b4 ) < b4->tag->header.keysMax ) && ( ( i4blockSize( b4->tag->indexFile ) -
         b4numKeys( b4 ) * b4->tag->header.groupLen - sizeof(short) - 2*sizeof(char)) >= sizeof(S4LONG) ) ) ;
   }



   int b4seek( B4BLOCK *b4, const char *searchValue, const int len )
   {
      #ifdef E4PARM_LOW
         if ( b4 == 0 || searchValue == 0 || len < 0 )
            return error4( 0, e4parm, E90438 ) ;
      #endif

      #ifdef E4DEBUG
         b4verifyPointers( b4 ) ;
      #endif

      /* keyOn must be between  keyLower and  keyUpper */
      int keyLower = -1 ;
      int keyUpper = b4numKeys( b4 ) ;

      if ( keyUpper == 0 )
      {
         b4->keyOn = 0 ;
         return r4after ;
      }

      int saveRc = 1 ;

      S4CMP_FUNCTION *cmp = b4->tag->cmp ;

      for( ;; )  /* Repeat until the key is found */
      {
         int keyCur  = (keyUpper + keyLower) / 2  ;
         int rc = (*cmp)( b4keyKey(b4,keyCur), searchValue, len ) ;

         if ( rc >= 0 )
         {
            keyUpper = keyCur ;
            saveRc = rc ;
         }
         else
            keyLower = keyCur ;

         if ( keyLower >= (keyUpper-1) )  /* then there is no exact match */
         {
            b4->keyOn = keyUpper ;
            if ( saveRc )
               return r4after ;
            return 0 ;
         }
      }
   }


   /* - AS Nov 13/02 - export for dot */
   int S4FUNCTION b4skip( B4BLOCK *b4, int n )
   {
      int numLeft ;

      #ifdef E4PARM_LOW
         if ( b4 == 0 )
            return error4( 0, e4parm_null, E90438 ) ;
      #endif

      #ifdef E4DEBUG
         b4verifyPointers( b4 ) ;
      #endif

      if ( n > 0 )
      {
         numLeft = b4numKeys( b4 ) - b4->keyOn ;
         if ( b4leaf( b4 ) )
         if ( numLeft != 0 )
            numLeft -- ;
      }
      else
         numLeft = -b4->keyOn ;

      if ( ( n <= 0L ) ? ( numLeft <= n ) : ( numLeft >= n ) )
      {
         b4->keyOn = b4->keyOn + (int) n ;
         return n ;
      }
      else
      {
         b4->keyOn = b4->keyOn + numLeft ;
         return numLeft ;
      }
   }
   #endif /* S4CLIPPER */



   #ifdef E4INDEX_VERIFY
      int b4verify( B4BLOCK *b4 )
      {
         #ifdef E4PARM_LOW
            if ( b4 == 0 )
               return error4( 0, e4parm_null, E90438 ) ;
         #endif

         if ( b4->tag->codeBase->doIndexVerify == 0 )
            return 0 ;

         int i ;
         #ifdef S4FOX
            int holdDup = b4->curDupCnt ;
            int holdTrail = b4->curTrailCnt ;
         #endif

         #ifdef S4CLIPPER
            /* block internal verification... */
            for ( i = 0 ; i < b4->tag->header.keysMax ; i++ )
            {
               for ( int j = i + 1 ; j <= b4->tag->header.keysMax ; j++ )
               {
                  if ( b4->pointers[i] == b4->pointers[j] )
                  {
                     #ifdef E4ANALYZE_ALL
                        return error4describe( b4->tag->codeBase, e4index, E80403, b4->tag->alias, 0, 0 ) ;
                     #else
                        return error4( b4->tag->codeBase, e4index, E80403 ) ;
                     #endif
                  }
               }
            }

            /* validate key count */
            if ( b4numKeys( b4 ) < b4->tag->header.keysHalf && b4->tag->header.root / 512 != b4->fileBlock && b4->tag->header.keysHalf > 1 )   /* allowed if only max 2 keys per index block */
               return error4describe( b4->tag->codeBase, e4index, E85709, b4->tag->alias, 0, 0 ) ;
         #endif

         /* all keys in a given block should be of the same type... - branch = 1 */
         #ifndef S4FOX
            int iType = b4leaf( b4 ) ? 0 : 1 ;
         #endif

         /* key order verification */
         #ifdef S4CLIPPER
            for ( i = 0 ; i <= b4numKeys( b4 ) ; i++ )
            {
               unsigned long val = b4key( b4, i )->pointer ;
               if ( ( b4key( b4, i )->pointer ? 1 : 0 ) != iType )
               {
                  #ifdef E4ANALYZE_ALL
                     return error4describe( b4->tag->codeBase, E80405, e4index, b4->tag->alias, 0, 0 ) ;
                  #else
                     return error4( b4->tag->codeBase, e4index, E80405 ) ;
                  #endif
               }
            }
         #endif

         for ( i = 0 ; i < b4numKeys( b4 ) - 1 ; i++ )
         {
            #ifdef S4FOX
               if ( u4keycmp( b4keyKey( b4, i), b4keyKey( b4, i + 1 ), (unsigned)b4->tag->header.keyLen, (unsigned)b4->tag->header.keyLen, 0, collation4get( b4->tag->collateName ) ) > 0 )
            #else
               if ( (*b4->tag->cmp)( b4keyKey( b4, i ), b4keyKey( b4, i + 1 ), b4->tag->header.keyLen ) > 0 )
            #endif
               {
                  #ifdef S4FOX
                     /* AS 06/22/99 - not necessarily invalid.  for character keys the order is
                        0x20,0x01,...0x19,0x21,..0xff.  So if rc > 0, check whether really is just
                        a blank problem...
                     */
                     int rc = u4keycmp( b4keyKey( b4, i), b4keyKey( b4, i + 1 ), (unsigned)b4->tag->header.keyLen, (unsigned)b4->tag->header.keyLen, 0, collation4get( b4->tag->collateName ) ) ;
                     TAG4FILE *t4 = b4->tag ;
                     if ( rc > 0 && t4->pChar == 0x20 )
                     {
                        for ( int ki = 0 ; i < t4->header.keyLen ; ki++ )
                        {
                           if ( b4keyKey( b4, i )[ki] != b4keyKey( b4, i + 1 )[ki] )
                           {
                              if ( b4keyKey( b4, i )[ki] < b4keyKey( b4, i + 1 )[ki] )  // we are done, and result is ok...
                              {
                                 rc = -1 ;
                                 break ;
                              }
                              /* if oldKey is 0x20 and newPtr is < 0x20, this is ok... */
                              if ( b4keyKey( b4, i )[ki] == 0x20 )  // newPtr is < this, just skip this byte...
                                 continue ;
                              // otherwise we got here and we really do have a mismatch because oldKey != 0x20
                              rc = 1 ;
                              break ;
                           }
                        }
                     }
                     if ( rc > 0 )
                  #endif
                  #ifdef E4ANALYZE_ALL
                     return error4describe( b4->tag->codeBase, e4index, E80406, b4->tag->alias, 0, 0 ) ;
                  #else
                     return error4( b4->tag->codeBase, e4index, E80406 ) ;
                  #endif
               }
         }

         #ifdef S4FOX
            b4->curDupCnt = holdDup ;
            b4->curTrailCnt = holdTrail ;
         #endif

         return 0 ;
      }
   #endif /* E4INDEX_VERIFY */
#endif // !S4INDEX_OFF && !S4CLIENT
