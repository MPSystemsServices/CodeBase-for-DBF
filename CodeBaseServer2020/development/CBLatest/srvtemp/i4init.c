/* i4init.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif


#if !defined( S4CLIENT ) && defined( S4MDX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4CALL t4cmpDoub( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t len )
   {
      double dif ;
      #ifdef S4DATA_ALIGN
         double d1, d2 ;

         memcpy( (void *)&d1, dataPtr, sizeof(double) ) ;
         memcpy( (void *)&d2, searchPtr, sizeof(double) ) ;
         #ifdef S4BYTE_SWAP
            d1 = x4reverseDouble( &d1 ) ;
            d2 = x4reverseDouble( &d2 ) ;
         #endif
         dif = d1 - d2 ;
      #else
         #ifdef S4BYTE_SWAP
            dif = x4reverseDouble((double *)dataPtr) -
               x4reverseDouble((double *)searchPtr) ;
         #else
            dif = *((double *)dataPtr) - *((double *)searchPtr) ;
         #endif
      #endif

      if ( dif > E4ACCURACY )
         return r4after ;
      if ( dif < -E4ACCURACY )
         return -1 ;
      return r4success ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4MDX ) */



#if !defined( S4CLIENT ) && defined( S4MDX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4CALL t4descCmpDoub( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t len )
   {
      return -1 * t4cmpDoub( dataPtr, searchPtr, 0 ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4MDX ) */



#if !defined( S4CLIENT ) && defined( S4MDX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4CALL t4descBcdCmp( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t len )
   {
      return -1 * c4bcdCmp( dataPtr, searchPtr, 0 ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4MDX ) */



#if !defined( S4CLIENT ) && defined( S4MDX )
   int S4CALL t4descMemcmp( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t len )
   {
      return -1 * u4memcmp( dataPtr, searchPtr, len ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4MDX ) */



#if !defined( S4CLIENT ) && ( defined( S4MDX ) || defined( S4CLIPPER ) )
   void t4noChangeDouble( char *result, const double d )
   {
      c4memcpy( result, (void *)&d, sizeof(double) ) ;
   }
#endif /* !defined( S4CLIENT ) && ( defined( S4MDX ) || defined( S4CLIPPER ) ) */



#if !defined( S4CLIENT )
   void t4noChangeStr( COLLATE4 *collate, char *a, const char *b, const int l, int *lenOut )
   {
      c4memcpy( a, b, (unsigned int)l ) ;
      #ifdef S4FOX
         *lenOut = l ;
      #endif
   }
#endif


#if !defined( S4CLIENT ) && defined( S4MDX ) && !defined( S4OFF_INDEX )
   int tfile4init( TAG4FILE *t4, INDEX4 *i4, T4DESC *tagInfo )
   {
      // CODE4 *c4 ;
      FILE4SEQ_READ seqread ;
      char buffer[1024], garbageBuffer[518], exprBuf[I4MAX_EXPR_SIZE+1], *ptr ;
      int len ;
      FILE4LONG pos ;

      #ifdef E4PARM_LOW
         if ( i4 == 0 || t4 == 0 || tagInfo == 0 )
            return error4( 0, e4parm, E94904 ) ;
      #endif

      CODE4 *c4 = i4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return -1 ;

      t4->indexFile = i4->indexFile ;
      t4->codeBase = c4 ;
      t4->cmp = (S4CMP_FUNCTION *)u4memcmp ;

      b4nodeAssignNode( &t4->headerOffset, tagInfo->headerPos ) ;
      // b4nodeAddBlocks( &t4->headerOffset, i4->indexFile, 1 ) ;
      b4nodeGetFilePosition( i4->indexFile, t4->headerOffset, &pos ) ;
      file4seqReadInitDo( &seqread, &i4->indexFile->file, pos, buffer, sizeof(buffer), 1 ) ;
      if ( file4seqReadAll( &seqread, &t4->header, sizeof(T4HEADER)) < 0 )
         return -1 ;

      #ifdef S4BYTE_SWAP
         t4->header.keyLen = x4reverseShort( (void *)&t4->header.keyLen ) ;
         t4->header.keysMax = x4reverseShort( (void *)&t4->header.keysMax ) ;
         t4->header.groupLen = x4reverseShort( (void *)&t4->header.groupLen ) ;
         t4->header.isDate = x4reverseShort( (void *)&t4->header.isDate ) ;
         t4->header.unique = x4reverseShort( (void *)&t4->header.unique ) ;
      #endif

      t4->header.root = INVALID4BLOCK_ID ;

      u4ncpy( t4->alias, tagInfo->tag, sizeof(t4->alias) ) ;
      c4trimN( t4->alias, sizeof(t4->alias) ) ;
      c4upper( t4->alias ) ;

      file4seqReadAll( &seqread, exprBuf, sizeof(exprBuf)-1 ) ;
      c4trimN( exprBuf, sizeof(exprBuf) ) ;
      t4->expr = expr4parseLow( i4->data, exprBuf, t4 ) ;
      if ( !t4->expr )
         return -1 ;

      len = expr4keyLen( t4->expr ) ;
      if ( len < 0 )
         return error4stack( c4, len, E94904 ) ;

      if ( t4->header.keyLen != (short)len )
         return error4describe( c4, e4index, E84901, i4->indexFile->file.name, 0, 0 ) ;

      tfile4initSeekConv( t4, t4->header.type ) ;

      file4seqReadAll( &seqread, garbageBuffer, sizeof(garbageBuffer) ) ;

      file4seqReadAll( &seqread, exprBuf, sizeof(exprBuf)-1 ) ;
      c4trimN( exprBuf, sizeof(exprBuf) ) ;

      if ( garbageBuffer[1] == 1 )   /* Q&E support ... has filter */
      {
         if ( exprBuf[0] != 0 )
         {
            if ( garbageBuffer[2] == 1 )
               t4->hasKeys = t4->hadKeys = 1 ;
            else
               t4->hasKeys = t4->hadKeys = 0 ;

            t4->filter = expr4parseLow( i4->data, exprBuf, t4 ) ;
            if ( t4->filter == 0 )
               return -1 ;

            if ( expr4context( t4->filter, i4->data ) < 0 )
               return -1 ;

            #ifdef S4FOX
               len = expr4key( t4->filter, &ptr, t4 ) ;
            #else
               len = expr4key( t4->filter, &ptr, 0 ) ;
            #endif
            if ( len < 0 )
               return -1 ;
            if ( expr4type( t4->filter ) != 'L' )
               error4describe( c4, e4index, E84903, i4->indexFile->file.name, 0, 0 ) ;
         }
      }
      return 0 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4MDX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4MDX ) && !defined( S4OFF_INDEX )
   int tfile4initSeekConv( TAG4FILE *t4, int keyType )
   {
      int isDesc ;

      isDesc = t4->header.typeCode & 8 ;

      switch( keyType )
      {
         case r4num:
            if ( isDesc )
               t4->cmp = (S4CMP_FUNCTION *)t4descBcdCmp ;
            else
               t4->cmp = (S4CMP_FUNCTION *)c4bcdCmp ;

            t4->stok = c4bcdFromA ;
            t4->dtok = c4bcdFromD ;
            break ;
         case r4date:
            if ( isDesc )
               t4->cmp = (S4CMP_FUNCTION *)t4descCmpDoub ;
            else
               t4->cmp = (S4CMP_FUNCTION *)t4cmpDoub ;
            t4->stok = t4strToDateMdx ;
            t4->dtok = t4noChangeDouble ;
            break ;
         case r4str:
            if ( isDesc )
               t4->cmp = (S4CMP_FUNCTION *)t4descMemcmp ;
            else
               t4->cmp = (S4CMP_FUNCTION *)u4memcmp ;
            t4->stok = t4noChangeStr ;
            t4->dtok = 0 ;
            break ;
         case r5wstr:
         case r5wstrLen:
            if ( isDesc )
               t4->cmp = (S4CMP_FUNCTION *)t4descMemcmp ;
            else
               t4->cmp = (S4CMP_FUNCTION *)u4memcmp ;
            // AS 08/11/99 --> must reverse bytes to be in sort order...
            // t4->stok = t4noChangeStr ;
            t4->stok = t4unicodeToMachine ;
            t4->dtok = 0 ;
            break ;
         default:
            return error4( t4->codeBase, e4index, E82901 ) ;  /* AS 02/10/99 changed from e4info so stop-critical doesn't halt in ole-db */
      }
      #ifdef S4UNIX  // CS 2000/05/25
         t4->keyType = keyType ;
      #endif

      return 0 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4MDX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX )
   #ifdef S4LANGUAGE
      extern unsigned char v4map[256];
   #else
      #ifdef S4ANSI
         extern unsigned char v4map[256];
      #endif
   #endif
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX )
   int S4CALL t4cdxCmp( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t len )
   {
      unsigned char *data = (unsigned char *)dataPtr ;
      unsigned char *search = (unsigned char *)searchPtr ;
      unsigned on ;

      for( on = 0 ; on < len ; on++ )
      {
         if ( data[on] != search[on] )
         {
            #ifdef S4VMAP
               if ( v4map[data[on]] > v4map[search[on]] ) return -1 ;  /* gone too far */
            #else
               if ( data[on] > search[on] ) return -1 ;  /* gone too far */
            #endif
            break ;
         }
      }

      return on ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX )
   int tfile4init( TAG4FILE *t4, INDEX4 *i4, FILE4LONG filePos, unsigned char *name )
   {
      #ifdef E4PARM_LOW
         if ( i4 == 0 || t4 == 0 || name == 0 )
            return error4( 0, e4parm, E94904 ) ;
      #endif

      CODE4 *c4 = i4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return -1 ;

      INDEX4FILE *i4file = i4->indexFile ;

      t4->indexFile = i4file ;
      t4->codeBase = c4 ;

      b4nodeSetFromFilePosition( i4file, &t4->headerOffset, filePos ) ;
      T4HEADER *header = &t4->header ;
      b4nodeSetInvalid( &header->root ) ;
      t4->cmp = (S4CMP_FUNCTION *)t4cdxCmp ;

      FILE4LONG pos ;
      file4longAssignLong( pos, filePos ) ;
      // AS 06/30/99 -- updated to include blocksize, multiplier...
      // unsigned int topSize = 2 * sizeof(S4LONG) + 4*sizeof(char) + sizeof(short) + 2 * sizeof(unsigned char) ;
      unsigned int topSize = 2 * sizeof(S4LONG) + 4*sizeof(char) + sizeof(short) + 2 * sizeof(unsigned char) + 3 * sizeof(S4UNSIGNED_LONG) ;

      FILE4 *file = &i4file->file ;
      if ( file4readAllInternal( file, pos, header, topSize ) < 0 )
         return 0 ;

      // AS 06/30/99 -- updated to include blocksize, multiplier...
      // file4longAssign( pos, filePos + (long)topSize + 478L, 0 ) ;
      file4longAssignLong( pos, filePos ) ;
      file4longAdd( &pos, (long)topSize + 466L ) ;

      if ( file4readAllInternal( file, pos, &(header->sortSeq), ( 8 * sizeof(char) ) ) < 0 )
         return 0 ;

      header->sortSeq[7] = '\0' ;
      // AS 06/30/99 -- updated to include blocksize, multiplier...
      // file4longAssign( pos, filePos + (long)topSize + 486L, 0 ) ;
      file4longAssignLong( pos, filePos ) ;
      file4longAdd( &pos, (long)topSize + 474L ) ;

      if ( file4readAllInternal( file, pos, &(header->descending), ( 5 * sizeof(short) ) ) < 0 )
         return 0 ;

      #ifdef S4BYTE_SWAP
         header->root.node = x4reverseLong( (void *)&header->root ) ;
         header->freeList.node = x4reverseLong( (void *)&header->freeList ) ;
         /* version is already stored in intel format */
         header->keyLen = x4reverseShort( (void *)&header->keyLen ) ;
         header->descending = x4reverseShort( (void *)&header->descending ) ;
         header->filterPos = x4reverseShort( (void *)&header->filterPos ) ;
         header->filterLen = x4reverseShort( (void *)&header->filterLen ) ;
         header->exprPos = x4reverseShort( (void *)&header->exprPos ) ;
         header->exprLen = x4reverseShort( (void *)&header->exprLen ) ;
      #else
         header->version = x4reverseLong( (void *)&header->version ) ;
      #endif

      u4ncpy( t4->alias, (char *)name, sizeof(t4->alias) ) ;
      c4trimN( t4->alias, sizeof(t4->alias) ) ;
      c4upper( t4->alias ) ;

      if ( *header->sortSeq == '\0' )
      {
         if ( tfile4setCollatingSeq( t4, collate4machine, 0 ) < 0 )
            return error4( c4, e4index, E84907 ) ;
      }
      #ifdef S4GENERAL
         else if ( c4strcmp( header->sortSeq, "GENERAL" ) == 0 )
         {
            short codePage = t4->indexFile->dataFile->codePage ;
            // collation depends on datafile code page...
            if ( codePage == cp1252 ) // windows ansi...
            {
               if ( tfile4setCollatingSeq( t4, collate4generalCp1252, 0 ) < 0 )
                  return error4( c4, e4index, E84907 ) ;
            }
            if ( codePage == cp0 ) // code page 0 uses windows ansi by default
            {
               if ( tfile4setCollatingSeq( t4, collate4generalCp1252, 0 ) < 0 )
                  return error4( c4, e4index, E84907 ) ;
            }
            if ( codePage == cp437 )  // US MS-DOS
            {
               if ( tfile4setCollatingSeq( t4, collate4generalCp437, 0 ) < 0 )
                  return error4( c4, e4index, E84907 ) ;
            }
            // AS Jun 9/04 - Support for CodePage 850
            if ( codePage == cp850 )   // European MS-DOS
            {
               if ( tfile4setCollatingSeq( t4, collate4generalCp850, 0 ) < 0 )
                  return error4( c4, e4index, E84907 ) ;
            }
            if ( codePage == cp1250 || codePage == cp0004 )  // AS Dec 30/02 - not supported - must use non-general sorting (croatian)
               return error4( c4, e4index, E84907 ) ;
         }
      #endif
      else if ( c4memcmp( header->sortSeq, "CB", 2 ) == 0 )
      {
         // new CodeBase only (non-Fox compatible) support for additional collations.
         // we get the ordinal of the sequence by the 5-digit number following the 'CB'
         // the ordinal == colateName
         int sortOrdinal = c4atoi( header->sortSeq + 2, 5 ) ;
         if ( collate4arrayIndex( sortOrdinal ) >= NUM4AVAIL_COLLATION_ENTRIES ) // # > than collations we support
            return error4( c4, e4index, E84907 ) ;
         t4->collateName = (Collate4name)sortOrdinal ;
      }
      else
         return error4( c4, e4index, E84907 ) ;

      if ( header->typeCode < 0x80 )  /* non-compound header; so expression */
      {
         char exprBuf[I4MAX_EXPR_SIZE+1] ;
         #ifdef E4ANALYZE
            if ( header->exprLen+1 > sizeof( exprBuf ) )
                return error4( c4, e4info, E84902 ) ;
         #endif
         file4longAssignLong( pos, filePos ) ;
         // expression starts at a 512 byte offset...
         file4longAdd( &pos, B4BLOCK_SIZE_INTERNAL ) ;
         file4readAllInternal( file, pos, exprBuf, (unsigned int)header->exprLen ) ;
         exprBuf[header->exprLen] = '\0' ;
         t4->expr = expr4parseLow( i4->data, exprBuf, t4 ) ;
         if ( t4->expr == 0 )
            return -1 ;

         // AS 07/29/99 - need to know if unicode before calling key len...
         if ( t4->expr->type == r5wstr || t4->expr->type == r5wstrLen )
            t4->isUnicode = 1 ;

         // AS 07/18/00 - need to setup collation from disk before calling keyLen...
         COLLATE4 *collate = collation4get( t4->collateName ) ;
         if ( collate->collateType != collate4machineByteOrder )
         {
            if ( t4->isUnicode == 0 )
            {
               if ( collate->charToKeyTranslationArray == 0 )
               {
                  if ( collate4setupReadFromDisk( t4->codeBase, t4->collateName ) != 0 )
                     return error4( t4->codeBase, e4notSupported, E84907 ) ;
               }
            }
            else
            {
               if ( collate->unicodeToKeyTranslationArray == 0 )
                  if ( collate->charToKeyTranslationArray == 0 )
                  {
                     if ( collate4setupReadFromDisk( t4->codeBase, t4->collateName ) != 0 )
                        return error4( t4->codeBase, e4notSupported, E84907 ) ;
                  }
            }
         }

         // AS Dec 5/05 - fix for OFF_MEMO
         #ifndef S4OFF_MEMO
            // AS Feb 11/04 - need to know if an expression includes a memo field
            if ( t4->expr->hasMemoField == 1 && t4->collateName != collate4none )
            {
               // assume/verify it is only a simple field
               // LY Jan 7/05 : replace direct check of COLLATE4.keySizeCharPerCharAdd with collate4simpleMapping()
               // short numExtraCharsPerChar = collation4get( t4->collateName )->keySizeCharPerCharAdd ;
               // if ( numExtraCharsPerChar > 0 )  // need to reduce the memo key size down for collation support
               COLLATE4 *collate = collation4get( t4->collateName ) ;
               if ( !collate4simpleMapping( collate ) )
               {
                  if ( t4->expr->info[0].len >= I4MAX_KEY_SIZE_COMPATIBLE - 1 )
                  {
                     short numExtraCharsPerChar = collate->keySizeCharPerCharAdd ;
                     t4->expr->info[0].len /= (1 + numExtraCharsPerChar) ;
                     t4->expr->len /= (1 + numExtraCharsPerChar) ;
                  }
               }
            }
         #endif

         int len = expr4keyLen( t4->expr ) ;
         if ( len < 0 )
            return -1 ;

         if ( header->keyLen != len )
            return error4describe( c4, e4index, E84901, file->name, 0, 0 ) ;

         tfile4initSeekConv(t4, t4->expr->type ) ;

         if ( header->typeCode & 0x08 )   /* For clause (filter) exists */
         {
            file4longAssignLong( pos, filePos ) ;
            // AS Jun 8/04 - need to use the B4BLOCK_SIZE_INTERNAL offset, not the actual block size...
            file4longAdd( &pos, B4BLOCK_SIZE_INTERNAL + header->exprLen ) ;
            file4readAllInternal( file, pos, exprBuf, (unsigned int)header->filterLen ) ;
            exprBuf[header->filterLen] = '\0' ;

            t4->filter = expr4parseLow( i4->data, exprBuf, t4 ) ;
            if ( t4->filter == 0 )
               return -1 ;
            if ( expr4context( t4->filter, i4->data ) < 0 )
               return -1 ;

            char *ptr ;
            #ifdef S4FOX
               len = expr4key( t4->filter, &ptr, t4 ) ;
            #else
               len = expr4key( t4->filter, &ptr, 0 ) ;
            #endif
            if ( len < 0 )
               return -1 ;
            if ( expr4type( t4->filter ) != 'L' )
               error4describe( c4, e4index, E84903, file->name, 0, 0 ) ;
         }
      }
      else
      {
         // AS 03/08/00 in this case, not calling tfile4initSeekConv() for the tagIndex, so not setting the pChar
         // as appropriate... just do it now.
         t4->pChar = ' ' ;
      }

      #ifdef S4DATA_ALIGN
         unsigned int size = (unsigned int)sizeof(S4LONG) + header->keyLen ;
         unsigned int delta = sizeof(void *) - size % sizeof(void *);
         t4->builtKeyMemory = mem4create( c4, 3, size + delta, 2, 0 ) ;
      #else
         t4->builtKeyMemory = mem4create( c4, 3, (unsigned int)sizeof(S4LONG) + header->keyLen + 1, 2, 0 ) ;
      #endif

      // AS 06/30/99 -- updated to include blocksize, multiplier...
      if ( header->codeBaseNote == 0xABCD )
      {
         // means that blockSize and multiplier stored here...
         // AS ensure that only set once, that set to defaults...
         assert5( i4file->blockSize == B4BLOCK_SIZE_INTERNAL && i4file->multiplier == 1 ) ;
         // ensure the integrity of the data...
         if ( ( header->blockSize % header->multiplier ) != 0 )  // there is a remainder - this is an error...
            return error4( c4, e4index, 85305 ) ;
         if ( (header->blockSize % B4BLOCK_SIZE_INTERNAL) != 0 || header->blockSize < B4BLOCK_SIZE_INTERNAL )  // must be multiple of 512
            return error4( c4, e4index, 85305 ) ;
         i4file->blockSize = header->blockSize ;
         i4file->multiplier = header->multiplier ;
      }

      return 0 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX )
   int tfile4initSeekConv( TAG4FILE *t4, int type )
   {
      t4->cmp = (S4CMP_FUNCTION *)t4cdxCmp ;

      switch( type )
      {
         case r4date:
         case r4dateDoub:
            t4->stok = t4dtstrToFox ;
            t4->dtok = t4dblToFox ;
            t4->pChar = '\0' ;
            break ;
         case r4numDoub:
            if ( expr4currency( t4->expr ) )  /* then should be converted to a currency */
            {
               t4->stok = t4strToCur ;
               t4->dtok = t4dblToCurFox ;
               t4->pChar = '\0' ;
               break ;
            }
            /* else fall through, same as r4num */
         case r4num:
            t4->stok = t4strToFox ;
            t4->dtok = t4dblToFox ;
            t4->pChar = '\0' ;
            break ;
         case r4str:
            {
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               COLLATE4 *collate = collation4get( t4->collateName ) ;
               if ( collate->collateType == collate4machineByteOrder )
               {
                  t4->stok = t4noChangeStr ;
                  t4->dtok = 0 ;
                  t4->pChar = ' ' ;
               }
               else
               {
                  if ( collate->charToKeyTranslationArray == 0 )
                  {
                     if ( collate4setupReadFromDisk( t4->codeBase, t4->collateName ) != 0 )
                        return error4( t4->codeBase, e4notSupported, E84907 ) ;
                  }

                  t4->dtok = 0 ;
                  t4->pChar = '\0' ;   /* if this changes check t4strToVFPKey() to ensure correctness */
                  switch( collate->collateType )
                  {

                     case collate4subSortCompress:
                        t4->stok = t4convertSubSortCompressChar ;
                        break ;
                     // AS Dec 30/02 - implement simple compression
                     case collate4simple:
                        t4->stok = t4convertSimpleChar ;
                        break ;
                     // case collate4subSort: - not implemented yet
                     // case collate4compress: - not implemented yet
                     default:
                        return error4( t4->codeBase, e4notSupported, E84907 ) ;
                  }
               }
            }
            break ;
         case r5wstr:
         case r5wstrLen:
            {
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               t4->isUnicode = 1 ;
               COLLATE4 *collate = collation4get( t4->collateName ) ;

               t4->pChar = '\0' ;   /* if this changes check t4strToVFPKey() to ensure correctness */
               if ( collate->collateType == collate4machineByteOrder )
               {
                  // AS 07/30/99 must reverse the bytes in unicode to make in machine order
                  t4->stok = t4unicodeToMachine ;
                  t4->dtok = 0 ;
               }
               else
               {
                  t4->dtok = 0 ;
                  // create the arrays if they are not there...
                  if ( collate->unicodeToKeyTranslationArray == 0 )
                  {
                     if ( collate->charToKeyTranslationArray == 0 )
                     {
                        if ( collate4setupReadFromDisk( t4->codeBase, t4->collateName ) != 0 )
                           return error4( t4->codeBase, e4notSupported, E84907 ) ;
                     }

                     // the loading may have loaded it.  If not, then it must have loaded a
                     // character translation, so transform that...
                     if ( collate->unicodeToKeyTranslationArray == 0 )
                        collate4setupUnicodeFromChar( collate ) ;
                  }

                  switch( collate->collateType )
                  {

                     case collate4subSortCompress:
                        t4->stok = t4convertSubSortCompressUnicode ;
                        break ;
                     // case collate4simple: - not implemented yet
                     // case collate4subSort: - not implemented yet
                     // case collate4compress: - not implemented yet
                     default:
                        return error4( t4->codeBase, e4notSupported, E84907 ) ;
                  }
               }
            }
            break ;
         case r4log:
            t4->stok = t4strToLog ;
            t4->dtok = 0 ;
            break ;
         #ifdef S4FOX
            // AS Jul 21/05 - Support for new field type binary float
            case r4floatBin:
               t4->stok = t4strToFloat ;
               t4->dtok = t4dblToFloat ;
               t4->pChar = '\0' ;
               break ;
            case r5ui4:
            case r5ui2:   // treated as a 4 byte unsigned int in index
               t4->stok = t4strToUnsignedInt ;
               t4->dtok = t4dblToUnsignedInt ;
               t4->pChar = '\0' ;
               break ;
            case r5i2:    // treated as a 4 byte int in index
            case r4int:
               t4->stok = t4strToInt ;
               t4->dtok = t4dblToInt ;
               t4->pChar = '\0' ;
               break ;
            #if defined( S4WIN32 ) || defined( S4MACINTOSH )   // LY Aug 13/04
               /* LY 4/29/99 : no equivalent of atoi64() for Mac => no c4atoLongLong => t4strToLongLong */
               // #ifndef S4NO_ATOLONGLONG
                  case r5i8:
                     t4->stok = t4strToLongLong ;
                     t4->dtok = t4dblToLongLong ;
                     t4->pChar = '\0' ;
                     break ;
               // #endif
            #endif
            case r5dbDate:
               t4->stok = t4dtstrToDbDate ;
               t4->dtok = t4dblToDbDate ;
               t4->pChar = '\0' ;
               break ;
            case r5dbTime:
               t4->stok = t4strToTime ;
               t4->dtok = 0 ;
               t4->pChar = '\0' ;
               break ;
            case r4dateTime:   // doesn't support 3 decimal fraction
               t4->stok = t4strToDateTime ;
               t4->dtok = 0 ;
               t4->pChar = '\0' ;
               break ;
            case r4dateTimeMilli:   // AS Mar 10/03 - supports 3 decimal fraction
               t4->stok = t4strToDateTimeMilli ;
               t4->dtok = 0 ;
               t4->pChar = '\0' ;
               break ;
            case r4currency:
               t4->stok = t4strToCur ;
               // AS 06/23/00 - was not converting to fox type, updated this...
               // t4->dtok = t4dblToCur ;
               t4->dtok = t4dblToCurFox ;
               t4->pChar = '\0' ;
               break ;
            case r5dbTimeStamp: // supports 9 decimal fraction
               t4->stok = t4strToDbTimeStamp ;
               t4->dtok = 0 ;
               t4->pChar = '\0' ;
               break ;
         #endif

         default:
            return error4( t4->codeBase, e4info, E82901 ) ;
      }
      #ifdef S4UNIX
         switch( type )
         {
            case r4numDoub:
               t4->keyType = r4num ;
               break ;
            case r4dateDoub:
               t4->keyType = r4date ;
               break ;
            default:  // CS 2000/05/25
               t4->keyType = type ;
         }
      #endif

      return 0 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4CLIPPER )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int S4CALL t4cmpDoub( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t len )
   {
      double dif ;
      #ifdef S4DATA_ALIGN
         double d1, d2 ;
         memcpy( &d1, dataPtr, sizeof(double) ) ;
         memcpy( &d2, searchPtr, sizeof(double) ) ;
         dif = d1 - d2 ;
      #else
         dif = *((double *)dataPtr) - *((double *)searchPtr) ;
      #endif

      if ( dif > E4ACCURACY )  return r4after ;
      if ( dif < -E4ACCURACY ) return -1 ;
      return r4success ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4CLIENT ) && defined( S4CLIPPER ) && !defined( S4OFF_INDEX )
   int tfile4initSeekConv( TAG4FILE *t4, int keyType )
   {
      switch( keyType )
      {
         case r4date:
         case r4dateDoub:
            t4->cmp = (S4CMP_FUNCTION *)u4memcmp ;
            t4->stok = t4noChangeStr ;
            t4->dtok = t4dateDoubToStr ;
            #ifdef S4UNIX
               t4->keyType = r4date ;
            #endif
            break ;
         case r4num:
         case r4numDoub:
            t4->cmp = (S4CMP_FUNCTION *)u4memcmp ;
            t4->stok = t4strToClip ;
            t4->dtok = 0 ;
            #ifdef S4UNIX
               t4->keyType = r4num ;
            #endif
            break ;
         case r4str:
            t4->cmp = (S4CMP_FUNCTION *)u4memcmp ;
            t4->stok = t4noChangeStr ;
            t4->dtok = 0 ;
            #ifdef S4UNIX
               t4->keyType = r4str ;
            #endif
            break ;
         case r5wstr:
         case r5wstrLen:
            t4->cmp = (S4CMP_FUNCTION *)u4memcmp ;
            // AS 08/11/99 --> must reverse bytes to be in sort order...
            // t4->stok = t4noChangeStr ;
            t4->stok = t4unicodeToMachine ;
            t4->dtok = 0 ;
            #ifdef S4UNIX
               t4->keyType = keyType ;
            #endif
            break ;
         default:
            return error4( t4->codeBase, e4tagInfo, E82901 ) ;
      }
      return 0 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4CLIPPER ) && !defined( S4OFF_INDEX ) */
