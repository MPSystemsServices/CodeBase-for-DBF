/* i4tag.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef I4PRINT
   #include <sys\timeb.h>
   #include <time.h>
#endif

/* AS 06/22/99 -- needed for ole-db dll... */
#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   // client case resides in i4info.c
   int S4FUNCTION t4keyLenExported( TAG4 *tag )
   {
      return tag->tagFile->header.keyLen ;
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */


TAG4FILE *S4FUNCTION t4getTagFile( TAG4 *tag )
{
   if ( tag == 0 )
      return 0 ;

   #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
      return tag->tagFile ;
   #else
      return 0 ;
   #endif
}


#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   Collate4name S4FUNCTION t4getCollateName( TAG4 *tag )
   {
      #ifdef S4FOX
         return tag->tagFile->collateName ;
      #else
         return collate4none ;
      #endif
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   long S4FUNCTION tfile4dskipExport( TAG4FILE *t4, long numSkip )
   {
      #ifdef S4HAS_DESCENDING
         if ( t4->header.descending )
            return -tfile4skip( t4, -numSkip ) ;
         else
            return tfile4skip( t4, numSkip ) ;
      #else
         return tfile4skip( t4, numSkip ) ;
      #endif
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */

// AS 07/27/99 -- this has been superseded by more simple collations for foxPro
/*
T4VFP * S4FUNCTION t4getVfpInfo( TAG4 *tag )
{
   // required by ole-db
   #ifdef S4FOX
      return &tag->tagFile->vfpInfo ;
   #else
      return 0 ;
   #endif
}
*/

int S4FUNCTION tfile4exprKeyExport( TAG4FILE *tag, unsigned char **ptrPtr )
{
   // export for ODBC
   #if defined( S4CLIENT ) || defined( S4OFF_INDEX )
      return -1 ;   // not available in client/server
   #else
      return tfile4exprKey( tag, ptrPtr ) ;
   #endif
}


#ifndef S4OFF_INDEX
   #ifndef S4SERVER
      int S4FUNCTION t4uniqueSetLow( TAG4 *, const short, const char ) ;
   #endif /* !S4SERVER */


   #ifndef S4CLIENT
      /* !S4OFF_INDEX, !S4CLIENT */
      int i4readBlock( FILE4 *file, const B4NODE blockNo, B4BLOCK *parent, B4BLOCK *b4 )
      {
         /* uses parent to determine if an inconsistancy has arisen */
         /* return <0 = error, 0 = success, 1 = inconsistant */

         #ifdef E4PARM_LOW
            if ( file == 0 || b4nodeInvalid( blockNo ) || b4 == 0 )
               return error4( 0, e4parm, E91604 ) ;
         #endif

         TAG4FILE *tag = b4->tag ;
         #ifndef S4CLIPPER
            INDEX4FILE *i4file = tag->indexFile ;
         #endif
         CODE4 *c4 = tag->codeBase ;
         #ifdef S4BYTE_SWAP
            char *swapPtr ;
         #endif

         #ifdef S4MDX
            /* i.e. if ndx or mdx */
            #ifndef S4OFF_OPTIMIZE
               i4file->readBlockTag = tag ;
            #endif

            FILE4LONG pos ;
            file4longAssign( pos, I4MULTIPLY * blockNo, 0 ) ;
            int amtRead = file4readAllInternal( file, pos, &b4->nKeys, i4file->header.blockRw ) ;

            #ifndef S4OFF_OPTIMIZE
               i4file->readBlockTag = 0 ;
            #endif

            if ( amtRead < 0 )
               return -1 ;

            #ifdef S4BYTE_SWAP
               /* swap the numKeys value */
               b4->nKeys = x4reverseShort( (void *)&b4->nKeys ) ;

               /* position swapPtr at beginning of B4KEY's */
               swapPtr = (char *)&b4->nKeys ;
               swapPtr += 6 + sizeof(short) ;

               /* move through all B4KEY's to swap 'long' */
               for ( int i = 0 ; i < (int)b4numKeys( b4 ) ; i++ )
               {
                  S4LONG longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                  swapPtr += tag->header.groupLen ;
               }

               /* mark lastPointer */
               if ( !b4leaf( b4 ) )
               {
                  S4LONG longVal = x4reverseLong( (void *)swapPtr ) ;
                  c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
               }
            #endif  /* S4BYTE_SWAP */
         #endif

         int rc = 0 ;

         #ifdef S4FOX
            #ifndef S4OFF_OPTIMIZE
               i4file->readBlockTag = tag ;
            #endif

            FILE4LONG pos ;
            // file4longAssign( pos, I4MULTIPLY * blockNo, 0 ) ;
            b4nodeGetFilePosition( i4file, blockNo, &pos ) ;
            rc = file4readAllInternal( file, pos, &b4->header, i4blockSize( i4file ) ) ;

            #ifndef S4OFF_OPTIMIZE
               i4file->readBlockTag = 0 ;
            #endif

            if ( rc < 0 )
               return error4stack( c4, (short)rc, E91604 ) ;

            #ifdef E4DEBUG
               // 0 is always invalid - should either be ULONG_MAX or a valid value...
               if ( b4node( b4->header.leftNode ) == 0 || b4node( b4->header.rightNode ) == 0 )
                  return error4( c4, e4struct, E91604 ) ;
            #endif

            #ifdef S4BYTE_SWAP
               b4->header.nodeAttribute = x4reverseShort( (void *)&b4->header.nodeAttribute ) ;
               b4->header.nKeys = x4reverseShort( (void *)&b4->header.nKeys ) ;
               b4->header.leftNode.node = x4reverseLong( (void *)&b4->header.leftNode ) ;
               b4->header.rightNode.node = x4reverseLong( (void *)&b4->header.rightNode ) ;

               /* if b4 is a leaf */
               if ( b4->header.nodeAttribute >= 2 )
               {
                  b4->nodeHdr.freeSpace = x4reverseShort( (void *)&b4->nodeHdr.freeSpace ) ;
                  S4LONG longVal = x4reverseLong( (void *)&b4->nodeHdr.recNumMask[0] ) ;
                  c4memcpy( (void *)&b4->nodeHdr.recNumMask[0], (void *)&longVal, sizeof(S4LONG) ) ;
               }
               else /* if b4 is a branch */
               {
                  short shortVal = tag->header.keyLen + sizeof(S4LONG) ;
                  /* position swapPtr to end of first key expression */
                  swapPtr = (char *) &b4->nodeHdr.freeSpace + tag->header.keyLen ;

                  /* move through all B4KEY's to swap 'long's */
                  for ( int i = 0 ; i < (int)b4numKeys( b4 ) ; i++ )
                  {
                     S4LONG longVal = x4reverseLong( (void *)swapPtr ) ;
                     c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                     swapPtr += sizeof(S4LONG) ;
                     longVal = x4reverseLong( (void *)swapPtr ) ;
                     c4memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                     swapPtr += shortVal ;
                  }
               }
            #endif
         #endif

         #ifdef S4CLIPPER
            #ifndef S4OFF_OPTIMIZE
               tag->readBlockTag = tag ;
            #endif

            FILE4LONG pos ;
            file4longAssign( pos, blockNo, 0 ) ;
            rc = file4readAllInternal( file, pos, &b4->nKeys, B4BLOCK_SIZE_INTERNAL ) ;

            #ifndef S4OFF_OPTIMIZE
               tag->readBlockTag = 0 ;
            #endif

            if ( rc < 0 )
               return error4stack( c4, rc, E91604 ) ;

            #ifdef S4BYTE_SWAP
               index4swapBlockClipper(&b4->nKeys, tag->header.keysMax, tag->header.groupLen ) ;
            #endif

            b4->fileBlock = blockNo / 512 ;
            assert5( b4->fileBlock != ULONG_MAX ) ;
         #endif

         if ( c4->doIndexVerify == 0 )
            return rc ;

         if ( b4numKeys( b4 ) == 0 )   /* added to free list... therefore bad */
         {
            #ifdef S4CLIPPER
               if ( tag->header.keysMax > 2 )  /* remote possibility of 0 keys if max = 2 and in a reindex mode */
                  if ( tag->header.root != b4->fileBlock * I4MULTIPLY )  /* b4 has no keys but is not the root, must be a problem... */
            #else
               if ( b4nodesNotEqual( tag->header.root, blockNo ) )  /* b4 has no keys but is not the root, must be a problem... */
            #endif
            return 1 ;
         }


         // AS Nov 13/02 - do some additional checking on block...
         #if defined( S4FOX ) && defined( E4ANALYZE )
            if ( b4numKeys( b4 ) < 0 )
               return error4( c4, e4index, E81601 ) ;
            if ( (long)b4numKeys( b4 ) > (long)i4blockSize( i4file ) )
               return error4( c4, e4index, E81601 ) ;
            if ( b4leaf( b4 ) )
            {
               if ( b4->nodeHdr.freeSpace < 0 )
                  return error4( c4, e4index, E81601 ) ;
               if ( (long)b4->nodeHdr.freeSpace  > (long)i4blockSize( i4file ) )
                  return error4( c4, e4index, E81601 ) ;
            }
         #endif

         if ( parent != 0 )  /* check consistancy */
         {
            #ifdef S4MDX
               int eq = 0 ;
               int blkOn = b4numKeys( b4 ) - 1 ;
               int parOn ;

               if ( parent->keyOn >= b4numKeys( parent ) )
               {
                  parOn = b4numKeys( parent ) - 1 ;
                  eq = 1 ;
               }
               else
                  parOn = parent->keyOn ;

               if ( !b4leaf( b4 ) )
               {
                  blkOn = 0 ;
                  if ( eq != 1 )
                  {
                     if ( parOn == 0 )
                        eq = 2 ;
                     else
                     {
                        eq = 1 ;
                        parOn-- ;
                     }
                  }
               }

               switch( eq )
               {
                  case 0:
                     if ( tag->cmp( b4keyKey( parent, parOn ), b4keyKey( b4, blkOn ), tag->header.keyLen  ) != 0 )
                        rc = 1 ;
                     break ;
                  case 1:
                     if ( tag->cmp( b4keyKey( parent, parOn ), b4keyKey( b4, blkOn ), tag->header.keyLen  ) > 0 )
                        rc = 1 ;
                     break ;
                  case 2:
                     if ( tag->cmp( b4keyKey( parent, parOn ), b4keyKey( b4, blkOn ), tag->header.keyLen  ) < 0 )
                        rc = 1 ;
                     break ;
                  #ifdef E4ANALYZE
                     default:
                        return error4( c4, e4index, E81601 ) ;
                  #endif
               }
            #endif

            #ifdef S4FOX
               if ( rc == 0 )
                  if ( b4recNo( parent, parent->keyOn) != b4recNo( b4, b4numKeys( b4 ) - 1 ) )
                  {
                     rc = 1 ;
                     #ifdef E4ANALYZE
                        // to help in debugger debugging, get the recno's out...
                        long parentRecNo = b4recNo( parent, parent->keyOn) ;
                        long blockRecNo = b4recNo( b4, b4numKeys( b4 ) - 1 ) ;
                        long blockRecNo2 = b4recNo( b4, b4numKeys( b4 ) - 2 ) ;
                        #ifdef I4PRINT
                           char dump[255] ;
                           sprintf( dump, "i4readBlock corruption detection, recno mismatch between parent and child for parent blockNo: %ld, parent recNo: %ld, block blockNo: %ld, block recNo:%ld, block Recno2:%ld\r\n",
                                   (long)b4node(parent->fileBlock), parentRecNo, (long)b4node(blockNo), blockRecNo, blockRecNo2 ) ;
                           log5( dump ) ;
                           sprintf( dump, "block numKeys: %ld\r\n", b4numKeys( b4 ) ) ;
                           log5( dump ) ;
                        #endif
                     #endif
                  }
            #endif
            if ( rc == 0 )
            {
               #ifdef E4INDEX_VERIFY
                  if ( b4verify( b4 ) == -1 )
                     return error4describe( c4, e4index, E91604, tag->alias, 0, 0 ) ;
                  if ( b4verify( parent ) == -1 )
                     return error4describe( c4, e4index, E91604, tag->alias, 0, 0 ) ;
               #endif
            }
            #ifdef S4CLIPPER
               if ( rc == 0 )
               {
                  if ( parent->keyOn < b4numKeys( parent ) )
                  {
                     if ( tag->cmp( b4keyKey( parent, parent->keyOn ), b4keyKey( b4, b4numKeys( b4 ) - 1 ), tag->header.keyLen  ) < 0 )
                        rc = 1 ;
                  }
                  else
                  {
                     if ( b4numKeys( b4 ) != 0 && b4numKeys( parent ) != 0 )   /* again, special instance whereby no key to compare with, so assume correct */
                        if ( tag->cmp( b4keyKey( parent, parent->keyOn - 1 ), b4keyKey( b4, 0 ), tag->header.keyLen  ) > 0 )
                           rc = 1 ;
                  }
               }
            #endif
         }

         if ( rc == 1 )
         {
            #ifdef S4CLIPPER
               if ( tfile4lockTest( tag ) == 1 )  /* corrupt */
            #else
               if ( index4lockTest( i4file ) == 1 )  /* corrupt */
            #endif
               return error4describe( c4, e4index, E81607, tag->alias, 0, 0 ) ;
         }

         #ifdef I4PRINT
            #ifdef I4PRINT_TAG_NAME
               if ( stricmp( tag->alias, I4PRINT_TAG_NAME ) == 0 )
            #endif
            {
               char dump[255] ;
               B4KEY_DATA *keyData = b4key( b4, b4numKeys(b4)-1 ) ;
               sprintf( dump, "***i4read block successful: blockNo = %ld last key recNo = %ld, numKeys = %ld\r\n", (long)b4node(blockNo), (long)keyData->num, (long)b4numKeys(b4) ) ;
               log5( dump ) ;
            }
         #endif


         return rc ;
         /* int i4readBlock( FILE4 *file, const long blockNo, B4BLOCK *parent, B4BLOCK *b4 ) */
      }



      #if !defined( S4CLIPPER ) && defined( E4ANALYZE )
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, E4ANALYZE */
         B4BLOCK *S4FUNCTION tfile4block( TAG4FILE *t4 )
         {
            /* for debug purposes only, else is an inline function */
            if ( t4 == 0 )
            {
               error4( 0, e4parm_null, E91642 ) ;
               return 0 ;
            }
            if ( t4->blocks.lastNode == 0 )
            {
               error4( t4->codeBase, e4index, E91642 ) ;
               return 0 ;
            }
            return (B4BLOCK *)t4->blocks.lastNode ;
         }
      #endif



      #ifdef S4FOX
         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         int S4FUNCTION tfile4bottom( TAG4FILE *t4 )
         {
            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            if ( t4->header.descending )   /* if descending, go bottom means go top */
               return tfile4rlTop( t4 ) ;
            else
               return tfile4rlBottom( t4 ) ;
         }
      #endif /* S4FOX */



      #ifdef S4MDX
         /* !S4OFF_INDEX, !S4CLIENT, S4MDX */
         int S4FUNCTION tfile4bottom( TAG4FILE *t4 )
         {
            int rc, rc2 ;
            B4BLOCK *blockOn ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            rc = 2 ;

            while ( rc == 2 )
            {
               rc = tfile4upToRoot( t4 ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, rc, E91642 ) ;

               if ( rc != 2 )
               {
                  b4goEof( tfile4block(t4) ) ;
                  do
                  {
                     rc = tfile4down( t4 ) ;
                     if ( rc < 0 )
                        return error4stack( t4->codeBase, rc, E91642 ) ;
                     b4goEof( tfile4block( t4 ) ) ;
                  } while ( rc == 0 ) ;
               }

               if ( rc == 2 )   /* failed due to read while locked */
               {
                  rc2 = tfile4outOfDate( t4 ) ;
                  if ( rc2 < 0 )
                     return error4stack( t4->codeBase, rc2, E91642 ) ;
               }
            }

            blockOn = tfile4block( t4 ) ;
            #ifdef E4ANALYZE
               if ( blockOn == 0 )
                  return error4( t4->codeBase, e4result, E91642 ) ;
            #endif

            if ( blockOn->keyOn > 0 )
               blockOn->keyOn = b4numKeys( blockOn ) - 1 ;

            return 0 ;
         }
      #endif /* S4MDX */



      #if !defined( S4OFF_WRITE ) && defined( S4FOX )
         /* !S4OFF_INDEX, !S4CLIENT, !S4OFF_WRITE, S4FOX */
         int tfile4branchSplit( TAG4FILE *t4, B4BLOCK *oldBlock, B4BLOCK *newBlock )
         {
            int newLen, nNewKeys ;
            int gLen = t4->header.keyLen + 2*sizeof(S4LONG) ;
            char *oPos ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 || oldBlock == 0 || newBlock == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            /* NNNNOOOO  N - New, O - Old */
            nNewKeys = ( b4numKeys( oldBlock ) + 1 ) / 2 ;
            if ( oldBlock->keyOn > nNewKeys )
               nNewKeys-- ;
            newBlock->header.nKeys = (short)nNewKeys ;
            oldBlock->header.nKeys -= (short)nNewKeys ;

            newLen = b4numKeys( newBlock ) * gLen ;

            oPos = ((char *)&oldBlock->nodeHdr) + gLen * b4numKeys( oldBlock ) ;
            c4memcpy( (void *)&newBlock->nodeHdr, oPos, (unsigned int)newLen ) ;
            newBlock->header.nodeAttribute = 0 ;
            oldBlock->header.nodeAttribute = 0 ;
            newBlock->keyOn = oldBlock->keyOn - b4numKeys( oldBlock ) ;

            /* clear the old data */
            c4memset( oPos, 0, (unsigned int)newLen ) ;

            return 0 ;
         }
      #endif



      #ifdef S4MDX
         /* !S4OFF_INDEX, !S4CLIENT, S4MDX */
         void S4FUNCTION tfile4descending( TAG4FILE *tag, const unsigned short int setting )
         {
         } /* A do nothing function to facilitate index independent OLEDB dll*/
      #endif /* S4MDX */



      #if defined( S4FOX ) && defined( S4HAS_DESCENDING )
         /* !S4OFF_INDEX, !S4CLIENT, S4FOX, S4HAS_DESCENDING */
         void S4FUNCTION tfile4descending( TAG4FILE *tag, const unsigned short int setting )
         {
            #ifdef E4PARM_LOW
               if ( tag == 0 )
               {
                  error4( 0, e4parm_null, E91642 ) ;
                  return ;
               }
            #endif

            tag->header.descending = setting ;
         }
      #endif



      #ifndef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         int tfile4down( TAG4FILE *t4 )
         {
            /* Returns  2 - cannot go down due to out of date blocks 1 - Cannot move down; 0 - Success; -1 Error */
            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            INDEX4FILE *i4 = t4->indexFile ;
            B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            B4NODE blockDown, nextBlockDown ;
            b4nodeSetInvalid( &blockDown ) ;
            b4nodeSetInvalid( &nextBlockDown ) ;

            B4BLOCK *popBlock, *newBlock, *parent ;
            int rc ;

            if ( blockOn == 0 )    /* Read the root block */
            {
               if ( root4needsReading( t4->header.root ) )
               {
                  FILE4LONG pos ;
                  b4nodeGetFilePosition( i4, t4->headerOffset, &pos ) ;
                  rc = file4readAllInternal( &i4->file, pos, &t4->header.root, sizeof( t4->header.root ) ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                  #ifdef S4BYTE_SWAP
                     t4->header.root.node = x4reverseLong( (void *)&t4->header.root ) ;
                  #endif
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255] ;
                        sprintf( dump, "tfile4down read root: i4file = %ld, root = %ld\r\n", (long)t4->indexFile, (long)b4node(t4->header.root) ) ;
                        log5( dump ) ;
                     }
                  #endif
               }
               b4nodeAssignNode( &blockDown, t4->header.root ) ;
            }
            else
            {
               if ( b4leaf( blockOn ) )
                  return 1 ;
               #ifdef S4FOX
                  #ifdef S4DATA_ALIGN
                     c4memcpy( (void *)&blockDown, (void *)(((unsigned char *)&blockOn->nodeHdr)
                             + (blockOn->keyOn+1)*(2*sizeof(S4LONG) + t4->header.keyLen) - sizeof(S4LONG)), sizeof(S4LONG) ) ;
                     blockDown.node = x4reverseLong( (void *)&blockDown ) ;   /* LY 00/07/11 : from blockDown to blockDown.node */

                  #else
                     b4nodeAssignLong( &blockDown, (unsigned long)x4reverseLong( (void *)( ((unsigned char *)&blockOn->nodeHdr)
                             + ( blockOn->keyOn + 1 ) * ( 2 * sizeof( S4LONG ) + t4->header.keyLen ) - sizeof( S4LONG ) ) ) ) ;
                  #endif
                  #ifdef S4ADVANCE_READ
                     /* AS 02/17/99 --> keyOn of 1 means on key '2', this means that if # keys == 1
                        and keyOn is 1, we are saying it is ok to look at entry '3', but in fact there
                        is no entry 3, so we should not be doings this.  Therefore, changed to
                        blockOn->keyOn + 2
                        if ( b4numKeys( blockOn ) >= (blockOn->keyOn + 1) )
                        */
                     if ( b4numKeys( blockOn ) >= (blockOn->keyOn + 2) )
                     {
                        #ifdef S4DATA_ALIGN
                           c4memcpy( (void *)&nextBlockDown,(void *)(((unsigned char *)&blockOn->nodeHdr)
                                   + (blockOn->keyOn+2)*(2*sizeof(S4LONG) + t4->header.keyLen) - sizeof(S4LONG)), sizeof(S4LONG) ) ;
                           nextBlockDown.node = x4reverseLong( (void *)&blockDown ) ;  /* LY 00/10/31 : added .node */
                        #else
                           b4nodeAssignLong( &nextBlockDown, x4reverseLong( (void *)( ( (unsigned char *)&blockOn->nodeHdr )
                                   + ( blockOn->keyOn + 2 ) * ( 2 * sizeof( S4LONG ) + t4->header.keyLen ) - sizeof( S4LONG ) ) ) ) ;
                        #endif
                     }
                     else
                        b4nodeSetInvalid( &nextBlockDown ) ;
                  #endif
               #else
                  blockDown = b4key(blockOn,blockOn->keyOn)->num ;
                  #ifdef S4ADVANCE_READ
                     if ( b4numKeys( blockOn ) >= (blockOn->keyOn + 1) )
                        nextBlockDown = b4key(blockOn,blockOn->keyOn+1)->num ;
                     else
                        b4nodeSetInvalid( &nextBlockDown ) ;
                  #endif
               #endif
               #ifdef E4ANALYZE
                  if ( b4nodeInvalid( blockDown ) || b4node( blockDown ) == 0 )
                     return error4( t4->codeBase, e4index, E81602 ) ;
               #endif
            }

            /* Get memory for the new block */
            popBlock = (B4BLOCK *)l4pop( &t4->saved ) ;
            if ( popBlock == 0 )
            {
               newBlock = b4alloc( t4, blockDown) ;
               if ( newBlock == 0 )
                  return error4stack( t4->codeBase, e4memory, E91642 ) ;
            }
            else
               newBlock = popBlock ;

            parent = (B4BLOCK *)l4last( &t4->blocks ) ;
            l4add( &t4->blocks, newBlock ) ;

            if ( popBlock == 0 || b4nodesNotEqual( newBlock->fileBlock, blockDown ) )
            {
               #ifndef S4OFF_WRITE
                  rc = b4flush( newBlock ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;
               #endif

               rc = i4readBlock( &i4->file, blockDown, parent, newBlock ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;

               #if !defined( S4OFF_OPTIMIZE ) && defined( S4ADVANCE_READ )
                  if ( b4nodeValid( nextBlockDown ) && ( i4->dataFile->hiPrio == -2 ) && ( b4leaf( newBlock ) ) )
                  {
                     FILE4LONG filePos ;
                     b4nodeGetFilePosition( i4, nextBlockDown, &filePos ) ;
                     if ( file4longGetHi( filePos ) == 0 )  // only advance-read if < 4 gigs...
                        opt4advanceReadBuf( &i4->file, file4longGetLo( filePos ), i4blockSize( i4 ) ) ;
                  }
               #endif

               if ( rc == 1 )
               {
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255] ;
                        sprintf( dump, "i4readBlock() failed for block %ld\r\n", (long)b4node( blockDown) ) ;
                        log5( dump ) ;
                     }
                  #endif
                  l4remove( &t4->blocks, newBlock ) ;
                  l4add( &t4->saved, newBlock ) ;
                  return 2 ;
               }

               b4nodeAssignNode( &newBlock->fileBlock, blockDown ) ;
               assert5( b4nodeValid( newBlock->fileBlock ) ) ;

               #ifdef S4FOX
                  newBlock->builtOn = -1 ;
               #endif

               /* flush blocks, don't delete */
               for( blockOn = 0 ;; )
               {
                  blockOn = (B4BLOCK *)l4next(&t4->saved,blockOn) ;
                  if ( blockOn == 0 )
                     break ;
                  #ifndef S4OFF_WRITE
                     rc = b4flush( blockOn ) ;
                     if ( rc < 0 )
                        return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                  #endif
                  b4nodeSetInvalid( &blockOn->fileBlock ) ;   /* make it invalid */
               }
            }

            #ifdef S4FOX
               return b4top( newBlock ) ;
            #else
               newBlock->keyOn = 0 ;
               return 0 ;
            #endif
         }
      #endif /* !S4CLIPPER */



      // #ifdef E4ANALYZE    CJ 06/24/99 tfile4eof must be a called function in all index formats
      /* !S4OFF_INDEX, !S4CLIENT */
      int S4FUNCTION tfile4eof( TAG4FILE *t4 )
      {
         B4BLOCK *b4 ;

         if ( t4->blocks.lastNode == 0 )   // if tag not set to anywhere, it is not at eof (it is nowhere)
            return 0 ;

         b4 = tfile4block( t4 ) ;

         #ifdef E4ANALYZE
            if ( b4 == 0 )
               return e4result ;
         #endif

         #if   defined( S4FOX )
            return( (b4->keyOn >= b4numKeys( b4 ) ) || (b4numKeys( b4 ) == 0) ) ;
         #elif defined( S4MDX )
            return( b4->keyOn >= b4numKeys( b4 ) ) ;
         #elif defined( S4CLIPPER )
            return ( b4->keyOn >= b4->nKeys ) ;
         #endif
      }
      //#endif /* E4ANALYZE */



      /* !S4OFF_INDEX, !S4CLIENT */
      int S4FUNCTION tfile4empty( TAG4FILE *tag )
      {
         B4BLOCK *b4 ;

         #ifdef E4PARM_LOW
            if ( tag == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         b4 = tfile4block( tag ) ;
         if ( b4 == 0 )
            return 1 ;
         if ( b4numKeys( b4 ) == 0L )
            return 1 ;

         return 0 ;
      }
   #endif /* !S4CLIENT */


   #ifndef S4CLIENT
      #ifdef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, S4CLIPPER */
         int S4FUNCTION tfile4exprKey( TAG4FILE *tag, unsigned char **ptrPtr )
         {
            int len, oldDec ;

            #ifdef E4PARM_LOW
               if ( tag == 0 || ptrPtr == 0)
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            oldDec = tag->codeBase->decimals ;
            tag->codeBase->decimals = tag->header.keyDec ;

            len = expr4key( tag->expr, (char **)ptrPtr, tag ) ;

            tag->codeBase->decimals = oldDec ;

            return len ;
         }
      #endif /* S4CLIPPER */



      #if !defined( S4CLIPPER ) && defined( E4PARM_LOW )
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, E4PARM_LOW */
         int S4FUNCTION tfile4exprKey( TAG4FILE *tag, unsigned char **ptrPtr )
         {
            #ifdef E4PARM_LOW
               if ( tag == 0 || ptrPtr == 0)
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            return expr4key( tag->expr, (char **)ptrPtr, tag ) ;
         }
      #endif



      #ifndef S4CLIPPER
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4freeAll( TAG4FILE *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         #ifdef I4PRINT
            #ifdef I4PRINT_TAG_NAME
               if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
            #endif
            {
               log5( "freeing all tag info\r\n" ) ;
            }
         #endif

         while ( tfile4up( t4 ) == 0 ) ;
         return tfile4freeSaved( t4 ) ;
      }



      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4freeSaved( TAG4FILE *t4 )
      {
         B4BLOCK *blockOn ;
         int rc ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         #ifndef S4OFF_WRITE
            rc = tfile4update( t4 ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
         #endif

         for ( ;; )
         {
            blockOn = (B4BLOCK *)l4pop( &t4->saved ) ;
            if ( blockOn == 0 )
               return 0 ;
            #ifndef S4OFF_WRITE
               rc = b4flush( blockOn ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;
            #endif
            rc = b4free( blockOn ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
         }
      }
      #endif /* !S4CLIPPER */



      /* !S4OFF_INDEX, !S4CLIENT */
      int S4FUNCTION tfile4go( TAG4FILE *t4, const unsigned char *ptr, const unsigned long recNum, const int goAdd )
      {
         int rc ;
         #ifdef S4HAS_DESCENDING
            int oldDesc ;
         #endif

         assert5parmLow( t4 != 0 && ptr != 0 && recNum != 0L && recNum != ULONG_MAX && recNum != ULONG_MAX - 1 ) ;

         #ifdef S4HAS_DESCENDING
            oldDesc = t4->header.descending ;
            t4->header.descending = 0 ;
         #endif

         rc = tfile4go2( t4, ptr, recNum, goAdd ) ;

         #ifdef S4HAS_DESCENDING
            t4->header.descending = (short)oldDesc ;
         #endif

         return rc ;
      }



      #ifndef S4CLIPPER
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4goEof( TAG4FILE *t4 )
      {
         int rc ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         rc = tfile4bottom( t4 ) ;
         if ( rc != 0 )
            return rc ;

         #ifdef E4ANALYZE
            if ( tfile4block( t4 ) == 0 )
               return error4( 0, e4info, E91642 ) ;
         #endif

         #ifdef S4FOX
            if ( b4numKeys( tfile4block(t4) ) != 0 )
               tfile4block(t4)->keyOn = b4numKeys( tfile4block(t4) ) ;
         #else
            tfile4block(t4)->keyOn++ ;
         #endif

         return 0 ;
      }



      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      #ifdef S4FOX
      /* LY 00/04/07 : unsigned long to S4UNSIGNED_LONG for 64-bit HP-UX */
      static int tfile4go2fox( TAG4FILE *t4, const unsigned char *ptr, const S4UNSIGNED_LONG recNum, const int addGo )
      {
         int kLen = t4->header.keyLen ;
         /* LY 00/04/07 : unsigned long to S4UNSIGNED_LONG for 64-bit HP-UX */
         S4UNSIGNED_LONG rec = x4reverseLong( (void *)&recNum ) ;

         int rc2 ;
         do
         {
            /* Do initial search, moving up only as far as necessary */
            rc2 = tfile4upToRoot( t4 ) ;
            if ( rc2 < 0 )
               return error4stack( t4->codeBase, (short)rc2, E91642 ) ;

            if ( rc2 != 2 )
            {
               for( ;; ) /* Repeat until found */
               {
                  B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
                  #ifdef E4ANALYZE
                     if ( blockOn == 0 )
                        return error4( t4->codeBase, e4index, E91642 ) ;
                  #endif

                  int allBlank = 0 ;

                  if ( b4leaf(blockOn) )
                  {
                     int rc = b4seek( blockOn, (char *)ptr, kLen ) ;

                     if ( rc )    /* leaf seek did not end in perfect find */
                     {
                        #ifdef I4PRINT
                           // it isn't necessarily critical if not found on non add (eg. a failed seek), but was looking
                           // at a problem whereby the remove was not finding the key which should have been there.
                           if ( addGo == 0 )
                           {
                              #ifdef I4PRINT_TAG_NAME
                                 if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                              #endif
                              {
                                 char dump[255] ;
                                 sprintf( dump, "tfile4go2fox block failed on block #: %ld # keys in block: %ld, keyOn: %ld, recNum:%ld\r\n", (long)b4node( blockOn->fileBlock ), (long)b4numKeys( blockOn ), (long)blockOn->keyOn, (long)recNum ) ;
                                 log5( dump ) ;
                                 log4invokeDebuggerDo();
                                 b4seek( blockOn, (char *)ptr, kLen ) ;  // seek again to look it up in the debugger...

                              }
                           }
                        #endif
                        return rc ;
                     }

                     /* now do the seek for recno on the leaf block */
                     blockOn = (B4BLOCK *)t4->blocks.lastNode ;

                     char hasSkipped = 0 ;
                     int trailCntFoundKey = x4trailCnt( blockOn, blockOn->keyOn ) ;
                     if ( trailCntFoundKey == t4->header.keyLen )
                        allBlank = 1 ;
                     else
                     {
                        if ( blockOn->keyOn == 0 )  /* could still be all blanks - 1st key */
                        {
                           allBlank = 1 ;
                           int i ;
                           for ( i = 0 ; i < t4->header.keyLen ; i++ )
                           {
                              // AS 06/19/00 - pChar, not ' '
                              if ( ptr[i] != t4->pChar )
                              {
                                 allBlank = 0 ;
                                 break ;
                              }
                           }

                           // AS 06/19/00
                           // also count the real number of blanks in the key in this case...
                           trailCntFoundKey = 0 ;
                           for ( i = t4->header.keyLen - 1 ; i >= 0 ; i-- )
                           {
                              // AS 06/19/00 - pChar, not ' '
                              if ( ptr[i] != t4->pChar )
                                 break ;
                              trailCntFoundKey++ ;
                           }
                        }
                        else
                           allBlank = 0 ;
                     }

                     for( ;; )
                     {
                        rec = tfile4recNo( t4 ) ;
                        if ( rec == ULONG_MAX || rec == ULONG_MAX - 1 )  // eof or no current block
                           return -1 ;

                        if ( addGo == 0 )
                           if ( rec == recNum )
                              return 0 ;

                        if ( rec >= recNum )
                        {
                           if ( !hasSkipped )
                              blockOn->curDupCnt = x4dupCnt( blockOn, blockOn->keyOn ) ;
                           return r4found ;
                        }

                        hasSkipped = 1 ;

                        rc = (int)tfile4skip( t4, 1L ) ;
                        if ( rc == -1 )
                           return -1 ;
                        if ( rc == 0 )
                        {
                           b4goEof( tfile4block( t4 ) ) ;
                           return r4found ;
                        }

                        /* AS 05/14/98 - 1 instance where this won't work:
                              < blank key, suddenly goes to all-blanks, then all blank is true, should stop but doesn't
                        */
                        int blankCountCurKey = x4trailCnt( blockOn, blockOn->keyOn ) ;
                        int dupCountCurKey = x4dupCnt( blockOn, blockOn->keyOn )  ;

                        if ( dupCountCurKey + blankCountCurKey != t4->header.keyLen )
                        {
                          /* case where key changed --> need to go back one to be on r4found value*/
                           return r4found ;
                        }

                        // AS 06/19/00 (t4indx2.c new test code) - fails in another instance:  the key goes from 0 blanks to 1 blank,
                        // and the key is < blank, and there are duplicates.  (eg. is "0x00001f, 0x000020 ) - search for '0x00001f'
                        // at this point we are at the '0x00001f' entry, for an add, so we are skipping until we have gone '1 too far'.
                        // now, after skipping blankCountCurKey == 1, x4dupCnt == 9, so above 'if' and below fails.
                        if ( trailCntFoundKey < blankCountCurKey )
                        {
                           // in this case, the key had say '0' blanks, and now we have moved to a key with '1' blank - we must be too far...
                           // (i.e. binary key)
                           return r4found ;
                        }

                        if ( blankCountCurKey == t4->header.keyLen && !allBlank )
                        {
                           /* done because our search key was < blank, but we found blank */
                           return r4found ;
                        }
                     }
                  }
                  else
                  {
                     int rc = b4rBrseek( blockOn, (char *)ptr, kLen, (S4LONG)rec ) ;  /* LY 00/04/07 : cast to S4LONG for 64-bit HP-UX */
                     if ( rc == 0 && t4->header.typeCode & 0x01 )
                        if ( b4recNo( tfile4block(t4), tfile4block(t4)->keyOn ) != recNum )
                           return r4found ;
                  }

                  rc2 = tfile4down( t4 ) ;
                  if ( rc2 < 0 )
                     return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
                  if ( rc2 == 2 )
                  {
                     int rc3 = tfile4outOfDate( t4 ) ;
                     if ( rc3 < 0 )
                        return error4stack( t4->codeBase, (short)rc3, E91642 ) ;
                     break ;
                  }
               }
            }
         } while ( rc2 == 2 ) ;
         return 0 ;
      }
      #endif /* S4FOX */



      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      #ifdef S4MDX
      static int tfile4go2mdx( TAG4FILE *t4, const unsigned char *ptr, const unsigned long recNum, const int addGo )
      {
         int rc = tfile4seek( t4, ptr, t4->header.keyLen ) ;
         if ( rc )
            return rc ;
         unsigned long recSave = tfile4recNo( t4 ) ;
         if ( recSave == recNum )
            return 0 ;

         /* else find the far end, and then skip back to where now or find */
         tfile4upToRoot( t4 ) ;
         for( ;; )
         {
            B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            #ifdef E4ANALYZE
               if ( blockOn == 0 )
                  return error4( t4->codeBase, e4index, E91642 ) ;
            #endif
            rc = b4seek( blockOn, (const char *)ptr, t4->header.keyLen ) ;
            while( rc == 0 )  /* perfect find, check next */
            {
               if ( b4skip( blockOn, 1L ) == 0 )
                  break ;
               if ( (*t4->cmp)( b4keyKey( blockOn, blockOn->keyOn ), ptr, t4->header.keyLen ) != 0 )
                  rc = r4after ;  /* stop loop and avoid rare return if b4leaf below */
            }

            if ( b4leaf( blockOn ) )
            {
               unsigned long tagRec = tfile4recNo( t4 ) ;
               if ( tagRec == recNum && rc == 0 )   /* found */
                  return 0 ;
               if ( tagRec == recSave )   /* didn't move */
                  return r4found ;
               break ;
            }
            int rc3 = tfile4down( t4 ) ;
            if ( rc3 < 0 )
               return error4stack( t4->codeBase, rc3, E91642 ) ;
         }

         for(;;)
         {
            rc = (int)b4skip( tfile4block( t4 ), -1L ) ;
            if ( rc == 0 ) /* try previous tag */
            {
               if ( tfile4skip( t4, -1L ) == 0 )
                  return r4found ;
            }
            unsigned long rec = tfile4recNo( t4 ) ;
            if ( rec == recSave )   /* failed to find */
               return r4found ;
            if ( rec == recNum )
               return 0 ;
         }
      }
      #endif /* S4MDX */



      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4go2( TAG4FILE *t4, const unsigned char *ptr, const unsigned long recNum, const int addGo )
      {
         /* addGo true if performing for add purposes instead of seek purposes */
         #ifdef E4PARM_LOW
            if ( t4 == 0 || ptr == 0 )
               return error4( 0, e4parm, E91642 ) ;
            if ( recNum <= 0L )
               return error4( 0, e4parm, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         #ifdef S4FOX
            return tfile4go2fox( t4, ptr, (S4UNSIGNED_LONG)recNum, addGo ) ; /* LY 00/04/07 : cast to S4UNSIGNED_LONG for 64-bit HP-UX */
         #endif

         #ifdef S4MDX
            return tfile4go2mdx( t4, ptr, recNum, addGo ) ;
         #endif
      }
      #endif /* !S4CLIPPER */
   #endif /* !S4CLIENT */
#endif /* !S4OFF_INDEX */




#ifndef S4OFF_INDEX
   #ifndef S4CLIENT
      #ifndef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         B4KEY_DATA *tfile4keyData( TAG4FILE *t4 )
         {
            B4BLOCK *b4 ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
               {
                  error4( 0, e4parm_null, E91642 ) ;
                  return 0 ;
               }
            #endif

            b4 = (B4BLOCK *)t4->blocks.lastNode ;
            #ifdef E4ANALYZE
               if ( b4 == 0 )
               {
                  error4( t4->codeBase, e4index, E91642 ) ;
                  return 0 ;
               }
               #ifdef S4FOX
                  if ( b4->keyOn >= b4->header.nKeys )     /* invalid key */
               #else
                  if ( b4->keyOn >= b4->nKeys )     /* invalid key */
               #endif
               {
                  error4( t4->codeBase, e4index, E91642 ) ;
                  return 0 ;
               }
            #endif
            return b4key( b4, b4->keyOn ) ;
         }
      #endif /* !S4CLIPPER */



      #if !defined( S4OFF_WRITE ) && defined( S4FOX )
         /* !S4OFF_INDEX, !S4CLIENT, !S4OFF_WRITE, S4FOX */
         int tfile4leafSplit( TAG4FILE *t4, B4BLOCK *oldBlock, B4BLOCK *newBlock )
         {
            #ifdef E4PARM_LOW
               if ( t4 == 0 || oldBlock == 0 || newBlock == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            // AS 08/14/01 - T4LARGE4.C -s1234 -i512 -r65044
            if ( b4numKeys( oldBlock ) == 1 )
            {
               // In this instance we get a case where the index block is full with only 1 key, and so the
               // input block here contains only 1 key.  This is a special case.  What we need to do is
               // have one empty block (where the record will get inserted) and one block with the key in place
               // this is based on the postion.  Notice that the newBlock comes after the old block.  If the
               // current position in the old block is keyOn == 1 (after key), we want the 'new' block to
               // be empty to contain the new entry.  If old block position is '0', we want the new block to
               // contain the key so the old block can get the new entry
               if ( oldBlock->keyOn == 0 )
               {
                  // copy the old data into the new block
                  memcpy( &newBlock->header, &oldBlock->header, i4blockSize( t4->indexFile ) ) ;
                  oldBlock->header.nKeys = 0 ;
                  oldBlock->curPos = ((char *)&newBlock->header) + i4blockSize( t4->indexFile ) ;
                  int rc = b4top( newBlock ) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, (short)rc, E91642 ) ;
               }
               else
               {
                  // AS Aug 14/01 - Impossible to end up here because the way we do seeks we always go
                  // to the 'found' position.  Could conceivably end up here if appending at the end?
                  // for now this instance has never happened, the code here may not work, so just error out
                  return error4( t4->codeBase, e4index, E91642 ) ;
                  /*
                     memcpy( &newBlock->header, &oldBlock->header, i4blockSize( t4->indexFile ) ) ;
                     newBlock->header.nKeys = 0 ;
                     newBlock->keyOn = 0 ;
                     newBlock->curPos = ((char *)&newBlock->header) + i4blockSize( t4->indexFile ) ;
                     int rc = b4top( oldBlock ) ;
                     if ( rc < 0 )
                        return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                  */
               }
               return 0 ;
            }

            int kLen = t4->header.keyLen ;
            int iLen = oldBlock->nodeHdr.infoLen ;
            int bLen = i4blockSize( t4->indexFile ) - (sizeof(oldBlock->header)) - (sizeof(oldBlock->nodeHdr))
                       - b4numKeys( oldBlock ) * iLen - oldBlock->nodeHdr.freeSpace ;
            int oldDup = oldBlock->curDupCnt ;

            b4top( oldBlock ) ;
            int nKeys = b4numKeys( oldBlock ) / 2 ;
            #ifdef S4WIN64 /* LY 00/09/21 */
               UINT64 len = 0 ;
            #else
               int len = 0 ;
            #endif
            for ( ; len < b4numKeys( oldBlock ) - nKeys ; len++ )
            {
               int rc = b4skip( oldBlock, 1L ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;
            }

            /* build the key 1st key of the new block from one past new end of old block */
            b4key( oldBlock, oldBlock->keyOn ) ;

            /* copy the general information */
            c4memcpy( (void *)&newBlock->header, (void *)&oldBlock->header,
                    (sizeof( oldBlock->header)) + (sizeof(oldBlock->nodeHdr)) ) ;

            /* AS 06/19/98 - changes.60 fix #134, 2.6 version should not include blanks... */
            /* put 1st key of new block */
            if ( t4->indexFile->dataFile->compatibility == 26 && t4->filter != 0 )
               newBlock->curTrailCnt = 0 ;
            else
               newBlock->curTrailCnt = b4calcBlanks( oldBlock->builtKey->value, kLen, t4->pChar ) ;
            len = kLen - newBlock->curTrailCnt ;
            newBlock->curPos = ((char *)&newBlock->header) + i4blockSize( t4->indexFile ) - len ;
            c4memcpy( newBlock->curPos, oldBlock->builtKey->value, (unsigned int)len ) ;

            /* copy remaining key data */
            char *obdPos = ((char *)&oldBlock->header) + i4blockSize( t4->indexFile ) - bLen ;
            len = oldBlock->curPos - obdPos ;
            assert5( len >= 0 ) ;
            newBlock->curPos -= len ;
            c4memcpy( newBlock->curPos, obdPos, (unsigned int)len ) ;

            /* copy the info data */
            char *obiPos = oldBlock->data + oldBlock->keyOn * iLen ;
            c4memcpy( newBlock->data, obiPos, (unsigned int)nKeys * iLen ) ;

            /* clear the old data */
            int rc = b4skip( oldBlock, -1L ) ;  /* go to new last entry */
            if ( rc == 0 )
               if ( error4code( t4->codeBase ) < 0 )
                  return error4stack( t4->codeBase, (short)error4code( t4->codeBase ), E91642 ) ;
            #ifdef E4ANALYZE
               if ( obdPos < obiPos )
                  return error4( t4->codeBase, e4info, E81603 ) ;
            #endif
            c4memset( obiPos, 0, (unsigned int)(oldBlock->curPos - obiPos) ) ;

            /* now reset the place new info data for the first key */
            c4memset( newBlock->data, 0, (unsigned int)iLen ) ;
            unsigned char buffer[6] ;
            x4putInfo( &newBlock->nodeHdr, buffer, oldBlock->builtKey->num, newBlock->curTrailCnt, 0 ) ;
            c4memcpy( newBlock->data, (void *)buffer, (unsigned int)iLen ) ;

            newBlock->header.nKeys = (short)nKeys ;
            oldBlock->header.nKeys -= (short)nKeys ;
            newBlock->header.nodeAttribute = 2 ;
            oldBlock->header.nodeAttribute = 2 ;
            newBlock->nodeHdr.freeSpace = (short) (newBlock->curPos - newBlock->data
                                             - b4numKeys( newBlock ) * iLen) ;
            oldBlock->nodeHdr.freeSpace = (short) (oldBlock->curPos - oldBlock->data
                                             - b4numKeys( oldBlock ) * iLen) ;
            oldBlock->builtOn = -1 ;
            newBlock->builtOn = -1 ;

            rc = b4top( oldBlock ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
            rc = b4top( newBlock ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;

            /* make sure dupCnt is updated on blocks for insert */
            newBlock->curDupCnt = oldDup ;
            oldBlock->curDupCnt = oldDup ;

            return 0 ;
         }
      #endif /* if !defined( S4OFF_WRITE ) && defined( S4FOX ) */



      /* !S4OFF_INDEX, !S4CLIENT */
      int tfile4outOfDate( TAG4FILE *t4 )
      {
         #ifndef S4SINGLE
            time_t oldTime ;
         #endif

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         #ifdef S4SINGLE
            return error4describe( t4->codeBase, e4index, E81605, tfile4alias( t4 ), 0, 0 ) ;
         #else
            /* first make sure we are at a potential read conflict situation (otherwise must be corrupt file) */
            #ifdef S4CLIPPER
               if ( tfile4lockTest( t4 ) == 1 )
            #else
               if ( index4lockTest( t4->indexFile ) == 1 )
            #endif
               return error4describe( t4->codeBase, e4index, E81606, tfile4alias( t4 ), 0, 0 ) ;

            /* wait a second and refresh */
            time( &oldTime) ;
            while ( time( (time_t *)0 ) <= oldTime) ;
            tfile4freeAll( t4 ) ;
            #ifdef S4CLIPPER
               return file4refresh( &t4->file ) ;
            #else
               return file4refresh( &t4->indexFile->file ) ;
            #endif
         #endif
      }



      #ifdef S4FOX
         S4EXPORT unsigned long S4FUNCTION tfile4recNo( TAG4FILE *t4 )
         {
            /*
               returns -2L (or ULONG_MAX-1) if there is no current block
               returns -1L (or ULONG_MAX) if the block is at eof (i.e. tag at eof), ... no valid recno)
                 - this is considered an error - asking for the recNo when there is none...
               returns -1L (or ULONG_MAX) if an error
               returns 0 if the entire tag is empty
               otherwise returns the appropriate record number from the current block...
            */
            if ( t4->blocks.lastNode == 0 )
               return ULONG_MAX - 1 ;

            if ( b4numKeys( (B4BLOCK *)(t4->blocks.lastNode) ) == 0 )
               return 0L ;

            // AS May 30/01 - If this block is not a leaf block then there is no
            // active key.  This was failing in an obscure example with a d4skip
            // after a d4go call
            if ( !b4leaf( (B4BLOCK *)(t4->blocks.lastNode) ) )
               return ULONG_MAX - 1 ;

            if ( ((B4BLOCK *)(t4->blocks.lastNode))->keyOn >= b4numKeys( (B4BLOCK *)(t4->blocks.lastNode) ) )
               return ULONG_MAX ;

            return b4recNo( (B4BLOCK *)(t4->blocks.lastNode), ((B4BLOCK *)(t4->blocks.lastNode))->keyOn ) ;
         }
      #endif /* S4FOX */



      #ifdef S4MDX
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, S4MDX */
         S4EXPORT unsigned long S4FUNCTION tfile4recNo( TAG4FILE *t4 )
         {
            /*
               returns -2L (or ULONG_MAX-1) if there is no current block
               returns -2L (or ULONG_MAX-1) if the current block is not a leaf
               returns -1L (or ULONG_MAX) if an error
               returns 0 if the entire tag is empty
               otherwise returns the appropriate record number from the current block...
            */

            B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            if ( blockOn == 0 )
               return ULONG_MAX - 1 ;

            if ( b4numKeys( blockOn ) == 0 )
               return 0 ;
            if ( !b4leaf( blockOn ) )
               return ULONG_MAX - 1 ;

            return b4recNo( blockOn, blockOn->keyOn ) ;
         }
      #endif /* S4MDX */



      #ifdef S4FOX
         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         int tfile4rlBottom( TAG4FILE *t4 )
         {
            int rc, rc2 ;
            B4BLOCK *blockOn ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            do
            {
               rc = tfile4upToRoot( t4 ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;

               if ( rc != 2 )
               {
                  if ( b4numKeys( tfile4block( t4 ) ) != 0 )
                  {
                     rc2 = b4go( tfile4block( t4 ), (long)b4numKeys( tfile4block( t4 ) ) - 1L ) ;
                     if ( rc2 < 0 )
                        return error4stack( t4->codeBase, rc2, E91642 ) ;
                     do
                     {
                        rc = tfile4down( t4 ) ;
                        if ( rc < 0 )
                           return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                        rc2 = b4go( tfile4block( t4 ), (long)b4numKeys( tfile4block( t4 ) ) - 1L ) ;
                        if ( rc2 < 0 )
                           return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
                     } while ( rc == 0 ) ;
                  }
               }

               if ( rc == 2 )   /* failed due to read while locked */
               {
                  rc2 = tfile4outOfDate( t4 ) ;
                  if ( rc2 < 0 )
                     return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
               }
            } while ( rc == 2 ) ;

            blockOn = tfile4block( t4 ) ;

            #ifdef E4ANALYZE
               if ( blockOn == 0 )
                  return error4( t4->codeBase, e4result, E91642 ) ;
            #endif

            if ( blockOn->keyOn > 0 )
            {
               b4goEof( blockOn ) ;
               blockOn->keyOn-- ;  /* update keyOn after going to last spot */
            }

            return 0 ;
         }



         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         int tfile4rlTop( TAG4FILE *t4 )
         {
            int rc, rc2 ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return e4codeBase ;

            do
            {
               rc = tfile4upToRoot( t4 ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, (short)rc, E91642 ) ;

               if ( rc != 2 )
               {
                  do
                  {
                     b4top( (B4BLOCK *)t4->blocks.lastNode ) ;
                     if ( (rc = tfile4down(t4)) < 0 )
                        return error4stack( t4->codeBase, (short)rc, E91642 ) ;
                  } while ( rc == 0 ) ;
               }

               if ( rc == 2 )   /* failed due to read while locked */
               {
                  rc2 = tfile4outOfDate( t4 ) ;
                  return error4stack( t4->codeBase, (short)rc2, E91642 ) ;
               }
            } while ( rc == 2 ) ;
            return 0 ;
         }


         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         static void tfile4seekDescendKey( TAG4FILE *t4, unsigned char *cPtr, int *incPos, const int lenPtr, const int lenIn, int *dSet, int *tmpLenPtr )
         {
            // basically force a descending seek by modifying the key to increment it by 1, then seeking for that and skipping back once and see
            // if we found what we were looking for.
            // this coding just desends the key by incrementing it.
            // Needs to consider special cases of bytes == 0xFF (increment would otherwise make 0), and case of general sort sequence
            // where we only want to effect the root part of the key, not the trail bytes
            // AS 07/27/99 -- this has been superseded by more simple collations for foxPro

            /* AS 07/30/98
               Problems here.  Some basic information:
               General sequencing works as follows:  string "A A" becomes: "0x60 0x11 0x60 0x00 0x00 0x00".  The 1st 3 bytes are sorted on, the
               last 3 bytes are the special character indicators (but characters are sorted as if they were the same).  Thus, in order to do a true descending
               seek, we need to increment the base key by '1'.  This would be in the first half of the key.  Since the 1st half of the keys never contains
               values < 10 and the 2nd half always contains values < 10, go to the right of the key and keep moving left until we have bypassed all the
               < 10 values.  Then do the standard increment.  NOTE however, this fails if the whole thing is blank because in that
               case the 'blank' overtakes the actual key values and we get just"0x00 0x00 0x00 0x00 0x00 0x00).  Therefore this is a special case; if we pass th
               beginning of the key then we have an all-blank key, just look for 0x12 (space is normally 11),
               code doesn't take into account 0xff ordering.. ok?
            */
            // if ( vfpKey && tfile4type( t4 ) == r4str )
            if ( collation4get( t4->collateName )->collateType != collate4machineByteOrder )
            {
               // AS 04/25/00 coded back in - was failing in string case of general collated descending indexes (or relations which descended the index)
               // return error4( t4->codeBase, e4notSupported, E84907 ) ; // not coded to work for unicode, when not just divide by 2 because maybe only 1 char extra...
               for( (*incPos) = lenPtr-1 ; cPtr[(*incPos)] < 10 && (*incPos) > 0 ; (*incPos)-- ) ;

               assert5( (*incPos) >= 0 ) ;  /* if 0, just increment anyway, even if 0 */

               if ( (*incPos) == 0 && cPtr[(*incPos)] == 0 )  // special case, set to 0x12
               {
                  (*incPos) = lenIn/2 - 1 ;
                  cPtr[(*incPos)] = 18 ;
                  c4memset( cPtr, 0x11, (*incPos) ) ;
                  (*dSet) = 2 ;
                  (*incPos)-- ;
                  (*tmpLenPtr) = lenIn ;
                  return ;
               }

               cPtr[(*incPos)]++ ;
               (*tmpLenPtr) = (*incPos) + 1 ;
               (*incPos)-- ;   /* allows cPtr to be reset correctly later on */
               (*dSet) = 1 ;
               return ;
            }

            for( (*incPos) = lenPtr-1 ; (*dSet) == 0 && (*incPos) >=0 ; (*incPos)-- )
            {
               if ( cPtr[(*incPos)] != 0xFF )
               {
                  cPtr[(*incPos)]++ ;
                  (*dSet) = 1 ;
               }
               else
                  cPtr[(*incPos)] = 0 ;  // make sure increment by 1 total only, not by a whole order of magnitude
            }
         }



         /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
         static void tfile4seekDescendKeyUndo( TAG4FILE *t4, unsigned char *cPtr, const int incPos, const int lenPtr, const int lenIn, const int dSet, char *compareKey )
         {
            // undoes the descending process done in tfile4seekDescendKey
            // if ( collation4get( t4->collateName )->collateType != collate4machineByteOrder )
            // {
            //    if ( dSet == 2 )  // special case, set to 0x12
            //    {
            //       cPtr[(*incPos)] = 0 ;
            //       c4memset( cPtr, 0, (*incPos) ) ;
            //       return ;
            //    }
            //
            //    cPtr[(*incPos)]-- ;
            //    return ;
            // }

            // same sequence works for both general and regular

            if ( dSet == 2 )
            {
               cPtr[incPos+1] = 0 ; /* reset the searchPtr ; */
               c4memset( cPtr, 0, incPos + 1 ) ;
               return ;
            }

            cPtr[incPos+1]-- ; /* reset the searchPtr ; */
            if ( collation4get( t4->collateName )->collateType == collate4machineByteOrder )
            {
               // AS 03/25/99 was failing to reset the bytes after incPos to 0xFF in the case
               // of having to reset those bytes to 0 before...
               short lenToReset = lenPtr - incPos - 2 ;
               if ( lenToReset > 0 )
                  c4memset( &(cPtr[incPos+2]), 0xFF, lenToReset ) ;
            }

            #ifdef E4ANALYZE
               // in this case, we were passed in the initial key to ensure that we converted back properly for debug purposes
               if ( memcmp( cPtr, compareKey, lenIn ) != 0 )
               {
                  error4( t4->codeBase, e4result, E91642 ) ;
                  return ;
               }
            #endif
         }
      #endif /* S4FOX */


      #ifndef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         int S4FUNCTION tfile4seek( TAG4FILE *t4, const void *ptr, const int lenIn )
         {
            int rc, rc2 ;
            B4BLOCK *blockOn ;
            #ifdef S4FOX
               int incPos = 0 ;
               int tmpLenPtr ;
               int dSet = 0 ;
               unsigned char *cPtr = (unsigned char *)ptr ;
            #endif
            int lenPtr = lenIn ;

            #ifdef E4PARM_LOW
               if ( t4 == 0 || ptr == 0 )
                  return error4( 0, e4parm_null, E91642 ) ;
               // AS 03/10/00 allow partial seeks for ODBC to look for null only (i.e. allow if nullable as well )
               if ( lenPtr != t4->header.keyLen && tfile4type( t4 ) != r4str && tfile4type( t4 ) != r5wstr && tfile4type( t4 ) != r5wstrLen )
               {
                  if ( !expr4nullLow( t4->expr, 0 ) )  // the key can't be NULL, shouldn't be doing a partial seek then because meaningless
                     return error4( t4->codeBase, e4parm, E91642 ) ;
               }
            #endif

            CODE4 *c4 = t4->codeBase ;
            if ( error4code( c4 ) < 0 )
               return e4codeBase ;

            if ( lenPtr > t4->header.keyLen )
               lenPtr = t4->header.keyLen ;
            // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
            #ifdef S4FOX
               tmpLenPtr = lenPtr ;
               #ifdef E4ANALYZE
                  char *compareKey = 0 ;
               #endif

               if ( t4->header.descending )   /* look for current item less one: */
               {
                  #ifdef E4ANALYZE
                     compareKey = (char *)u4alloc( lenIn ) ;
                     if ( compareKey == 0 )
                        return error4( c4, e4memory, E91642 ) ;
                     c4memcpy( compareKey, cPtr, lenIn ) ;
                  #endif
                  tfile4seekDescendKey( t4, cPtr, &incPos, lenPtr, lenIn, &dSet, &tmpLenPtr ) ;
               }
            #endif

            rc = 3 ;
            for( ;; ) /* Repeat until found */
            {
               while ( rc >= 2 )
               {
                  if ( rc == 2 )
                  {
                     rc2 = tfile4outOfDate( t4 ) ;
                     if ( rc2 < 0 )
                        return error4stack( c4, (short)rc2, E91642 ) ;
                  }
                  rc = tfile4upToRoot( t4 ) ;
                  if ( rc < 0 )
                     return error4stack( c4, (short)rc, E91642 ) ;
               }
               blockOn = (B4BLOCK *)t4->blocks.lastNode ;
               #ifdef E4ANALYZE
                  if ( blockOn == 0 )
                     return error4( c4, e4index, E91642 ) ;
               #endif

               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               #ifdef S4FOX
                  assert5( tmpLenPtr >= 0 ) ;
                  rc = b4seek( blockOn, (char *)ptr, tmpLenPtr ) ;
               #else
                  assert5( lenPtr >= 0 ) ;
                  rc = b4seek( blockOn, (char *)ptr, lenPtr ) ;
               #endif

               if ( rc < 0 )
                  return error4stack( c4, (short)rc, E91642 ) ;
               if ( b4leaf( blockOn ) )
                  break ;

               rc = tfile4down( t4 ) ;
               if ( rc < 0 )
                  return error4stack( c4, (short)rc, E91642 ) ;
            }

            #ifdef S4FOX
               if ( t4->header.descending )   /* must go back one! */
               {
                  // reset the search key to the previous value
                  // AS 04/25/00 enabled descending general sequence above, so code this as well
                  #ifdef E4ANALYZE
                     tfile4seekDescendKeyUndo( t4, cPtr, incPos, lenPtr, lenIn, dSet, compareKey ) ;
                     u4free( compareKey ) ;
                  #else
                     tfile4seekDescendKeyUndo( t4, cPtr, incPos, lenPtr, lenIn, dSet, 0 ) ;
                  #endif

                  if ( dSet )
                  {
                     if ( b4numKeys( blockOn ) == 0 )
                        rc = 0 ;
                     else
                     {
                        if ( blockOn->keyOn == 0 )   /* special case, must balance tree */
                        {
                           rc = (int)tfile4skip( t4, -1L ) ;
                           if ( rc == 0 )
                              rc2 = tfile4top( t4 ) ;
                           else
                              rc2 = tfile4go( t4, tfile4keyData( t4 )->value, tfile4recNo( t4 ), 0 ) ;
                           if ( rc2 < 0 )
                              return error4stack( c4, (short)rc2, E91642 ) ;
                        }
                        else
                           rc = (int)tfile4skip( t4, -1L ) ;
                     }
                     if ( rc == 0L )  /* bof = eof condition */
                     {
                        b4goEof( blockOn ) ;
                        rc = r4eof ;
                     }
                     else
                     {
                        rc2 = b4go( tfile4block( t4 ), (long)tfile4block( t4 )->keyOn ) ;
                        if ( rc2 < 0 )
                           return error4stack( c4, (short)rc2, E91642 ) ;
                        // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                        // rc2 = u4keycmp( (void *)b4keyKey( tfile4block( t4 ), tfile4block( t4 )->keyOn ), ptr, (unsigned)lenPtr, (unsigned)t4->header.keyLen, 0, &t4->vfpInfo ) ;
                        rc2 = u4keycmp( (void *)b4keyKey( tfile4block( t4 ), tfile4block( t4 )->keyOn ), ptr, (unsigned)lenPtr, (unsigned)t4->header.keyLen, 0, collation4get( t4->collateName ) ) ;
                        if ( rc2 )
                           rc = r4after ;
                        else
                           rc = 0 ;  /* successful find */
                     }
                  }
                  else
                  {
                     if ( rc == 0 )  /* the item was found, so go top, */
                     {
                        rc2 = tfile4top( t4 ) ;
                        if ( rc2 < 0 )
                           return error4stack( c4, (short)rc2, E91642 ) ;
                     }
                     else  /* otherwise want an eof type condition */
                     {
                        b4goEof( blockOn ) ;
                        rc = r4eof ;
                     }
                  }
               }
            #endif
            return rc ;
         }
      #endif /* !S4CLIPPER */
   #endif /* !S4CLIENT */
#endif /* !S4OFF_INDEX */



#ifndef S4OFF_INDEX
   #ifndef S4CLIENT
      #ifndef S4CLIPPER
         /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
         long S4FUNCTION tfile4skip( TAG4FILE *t4, long numSkip )
         {
            // returns the number of actual skips (negative for negative skips), and -numskip if a failure (note that when skipping
            // backwards (input of -x), this means a positive return value
            long numLeft ;
            B4BLOCK *blockOn ;
            int rc, seekAvail = 0 ;
            #ifdef S4FOX
               int saveDups ;
            #else
               int sign ;
            #endif

            #ifdef E4PARM_LOW
               if ( t4 == 0 )
               {
                  error4( 0, e4parm_null, E91642 ) ;
                  return -numSkip ;
               }
            #endif

            if ( error4code( t4->codeBase ) < 0 )
               return -numSkip ;

            numLeft = numSkip ;

            blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            if ( blockOn == 0 )
            {
               if ( tfile4top( t4 ) < 0 )
                  return -numSkip ;
               blockOn = (B4BLOCK *)t4->blocks.lastNode ;
            }

            #ifdef E4ANALYZE
               if ( ! b4leaf(blockOn) )
               {
                  error4( t4->codeBase, e4index, E91642 ) ;
                  return -numSkip ;
               }
            #endif

            #ifdef S4FOX
               for(;;)
               {
                  numLeft -= b4skip( blockOn, numLeft ) ;
                  if ( numLeft == 0 )  /* Success */
                     return numSkip ;

                  B4NODE goTo ;

                  if ( numLeft > 0 )
                     b4nodeAssignNode( &goTo, blockOn->header.rightNode ) ;
                  else
                     b4nodeAssignNode( &goTo, blockOn->header.leftNode ) ;

                  if ( b4nodeInvalid( goTo ) )
                  {
                     if ( numSkip > 0 && t4->header.descending == 0 ||
                          numSkip <= 0 && t4->header.descending == 1 )
                     {
                        saveDups = tfile4block( t4 )->curDupCnt ;
                        rc = tfile4bottom( t4 ) ;
                        if ( rc < 0 )
                           return -numSkip ;
                        tfile4block( t4 )->curDupCnt = saveDups ;
                     }
                     else
                        if ( tfile4top(t4) < 0 )
                          return -numSkip ;
                     return (numSkip - numLeft) ;
                  }

                  #ifndef S4OFF_WRITE
                     if ( blockOn->changed )
                        if ( b4flush( blockOn ) < 0 )
                           return -numSkip ;
                  #endif

                  /* save the current key in case skip fails -- for leaf blocks only */
                  if ( b4leaf( blockOn ) )
                     if ( index4lockTest( t4->indexFile ) != 1 )
                     {
                        c4memcpy( t4->codeBase->savedKey, (void *)b4keyKey( blockOn, b4numKeys( blockOn ) - 1 ), (unsigned int)t4->header.keyLen ) ;
                        seekAvail = 1 ;
                     }

                  rc = i4readBlock( &t4->indexFile->file, goTo, 0, blockOn ) ;
                  if ( rc < 0 )
                     return -numSkip ;

                  if ( rc == 1 )   /* failed on i/o, seek current spot to make valid */
                  {
                     if ( seekAvail == 0 )
                     {
                        error4( t4->codeBase, e4index, E91642 ) ;
                        return -numSkip ;
                     }
                     #ifndef S4OPTIMIZE_OFF
                        #ifndef S4SINGLE
                           file4refresh( &t4->indexFile->file ) ;
                        #endif
                     #endif
                     rc = tfile4seek( t4, t4->codeBase->savedKey, t4->header.keyLen ) ;
                     if ( rc < 0 )
                        return -numSkip ;
                     if ( rc == r4after )   /* means skipped 1 ahead */
                        numLeft-- ;
                  }

                  #ifdef S4READ_ADVANCE
                     #ifndef S4OFF_OPTIMIZE
                        if ( t4->indexFile->file.doBuffer )
                        {
                           /* advance-read the next block since skipping */
                           FILE4LONG readAdvTo ;

                           B4NODE node ;
                           if ( numSkip > 0 )
                              b4nodeAssignNode( &node, blockOn->header.rightNode ) ;
                           else
                              b4nodeAssignNode( &node, blockOn->header.leftNode ) ;

                           if ( b4nodeValid( node ) )
                           {
                              b4nodeGetFilePosition( t4->indexFile, node, &readAdvTo ) ;
                              if ( file4longGetHi( readAdvTo ) == 0 )
                                 opt4advanceReadBuf( &t4->indexFile->file, file4longGetLo( readAdvTo ), i4blockSize( t4->indexFile ) ) ;
                           }
                        }
                     #endif
                  #endif

                  blockOn->fileBlock = goTo ;
                  assert5( b4nodeValid( blockOn->fileBlock ) ) ;
                  blockOn->builtOn = -1 ;
                  b4top( blockOn ) ;

                  if ( numLeft < 0 )
                     numLeft += b4numKeys( blockOn ) ;
                  else
                     numLeft -= 1 ;  /* moved to the next entry */
               }
            #endif

            #ifndef S4FOX
               if ( numSkip < 0)
                  sign = -1 ;
               else
                  sign = 1 ;

               for(;;)
               {
                  /* save the current key in case skip fails -- for leaf blocks only */
                  if ( b4leaf( blockOn ) )
                     c4memcpy( t4->codeBase->savedKey, b4keyKey( blockOn, blockOn->keyOn ), t4->header.keyLen ) ;

                  while ( ( rc = tfile4down( t4 ) ) == 0 )
                     if ( sign < 0 )
                     {
                        blockOn = tfile4block( t4 ) ;
                        b4goEof( blockOn ) ;
                        if ( b4leaf( blockOn) )
                        {
                           blockOn->keyOn-- ;
                           #ifdef E4ANALYZE
                              if ( blockOn->keyOn < 0 )
                              {
                                 error4( t4->codeBase, e4index, E91642 ) ;
                                 return -numSkip ;
                              }
                           #endif
                        }
                     }

                  if ( rc < 0 )
                     return -numSkip ;

                  if ( rc == 2 )   /* failed on i/o, seek current spot to make valid */
                  {
                     tfile4outOfDate( t4 ) ;
                     rc = tfile4seek( t4, t4->codeBase->savedKey, t4->header.keyLen ) ;
                     if ( rc < 0 )
                        return -numSkip ;
                     if ( rc == r4after )   /* means skipped 1 ahead */
                        numLeft-- ;
                     continue ;
                  }

                  blockOn = tfile4block( t4 ) ;
                  if ( rc < 0 || blockOn == 0 )
                     return -numSkip ;

                  numLeft -= b4skip( blockOn, numLeft ) ;
                  if ( numLeft == 0 )      /* Success */
                     return numSkip ;

                  do  /* Skip 1 to the next leaf block  */
                  {
                     if ( (B4BLOCK *)blockOn->link.p == blockOn )
                     {
                        if ( numSkip > 0 )
                        {
                           if ( tfile4bottom( t4 ) < 0 )
                              return -numSkip ;
                        }
                        else
                           if ( tfile4top( t4 ) < 0 )
                              return -numSkip ;

                        return( numSkip - numLeft ) ;
                     }
                     else
                        if ( tfile4up( t4 ) < 0 )
                           return -numSkip ;

                     blockOn = (B4BLOCK *)t4->blocks.lastNode ;
                  }  while ( b4skip( blockOn, (long) sign) != sign) ;

                  numLeft -= sign ;
               }
            #endif
         }
      #endif /* !S4CLIPPER */



      #ifdef S4FOX
      /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
      static Bool5 tfile4setCollationSeqCollatable( TAG4FILE *t4 )
      {
         /* returns 1 if the tag is collatable (i.e. simple fields) */

         assert5( t4 != 0 ) ;
         assert5( t4->expr != 0 ) ;

         int fieldType = -1 ;

         for ( int infoIndex = 0 ; infoIndex < t4->expr->infoN ; infoIndex++ )
         {
            E4INFO *infoPtr = t4->expr->info + infoIndex ;
            switch ( infoPtr->functionI )
            {
               // these are the only functions which are collatable - i.e. simple fields or
               // simple concatenations...
               // AS 03/30/00  - should work with everything except ASCEND, DESCEND, L2BIN
               case E4FIELD_STR:
               case E4FIELD_STR_CAT:
               case E4FIELD_MEMO:
               case E4STRING:
               case E4CONCATENATE:
               case E4CONCAT_TRIM:
               case E4CONCAT_TWO:
               case E4CHR:
               case E4DEL:
               case E4STR:
               case E4STR+1:
               case E4STRZERO:
               case E4SUBSTR:
               case E4TIME:
               case E4UPPER:  // AS 03/08/00 - allow upper casing as well, since this produces a compatible string
               case E4DTOS:
               case E4DTOS+1:
               case E4DTOC:
               case E4DTOC+1:
               case E4TRIM:
               case E4LTRIM:
               case E4ALLTRIM:
               case E4LEFT:
               case E4RIGHT:
               case E4PADL:
               case E4PADR:
               case E4IIF:  // r4str entry only
                  // check that we don't have mixed types which are not collatable...
                  if ( fieldType == -1 )  // ok, just set it now...
                     fieldType = r4str ;
                  else
                     if ( fieldType != r4str )  // cannot mix unicode and string for collations...
                        return 0 ;  // not collatable...
                  break ;
               case E4FIELD_WSTR:
               case E4FIELD_WSTR_LEN:
               case E4FIELD_WSTR_CAT:
                  // check that we don't have mixed types which are not collatable...
                  if ( fieldType == -1 )  // ok, just set it now...
                     fieldType = r5wstr ;
                  else
                     if ( fieldType != r5wstr )  // cannot mix unicode and string for collations...
                        return 0 ;  // not collatable...
                  break ;
               // AS Oct 3/02 Also support the STR( double ) function
               case E4FIELD_NUM_D:
                  // if the next info piece is e4str, then go ahead...
                  if ( ( infoIndex + 1 < t4->expr->infoN ) && ( t4->expr->info + infoIndex + 1 )->functionI == E4STR )
                     break ;
                  return 0 ;
               default:
                  return 0 ;  // not collatable...
            }
         }

         return 1 ;  // no problems if got here, is collatable...
      }



      /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
      int tfile4setCollatingSeq( TAG4FILE *t4, Collate4name collateName, Bool5 setHeader )
      {
         /* if setHeader is true, it means we are calling this function from a create, and
            we want to set up the tag header strings to include the tag information.
            if setHeader is false, we only need to do a check at the end to ensure that
            they were set properly in debug case.
         */
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         /* AS 07/27/99 --> in order to support backwards compatibility....
            if collateName == collate4none, then use the old settings to determine the sort
            sequence...

         */

         // AS 08/06/99 - we only want to support collations for expressions which are considered
         // collatable (see u4util.c collation notes).  Basically, any expression that includes
         // anything besides simple fields is considered uncollatable at this point.

         if ( collateName == collate4none )
         {
            if ( t4->codeBase->collatingSequence == sort4machine )
            {
               t4->collateName = collate4machine ;
               if ( setHeader )
                  c4memset( t4->header.sortSeq, 0, 8 ) ;
               return 0 ;
            }

            assert5( t4->codeBase->collatingSequence == sort4general || t4->codeBase->collatingSequence == sort4croatian ||
                     t4->codeBase->collatingSequence == sort4croatianUpper  ) ;

            if ( setHeader && collateName != collate4machine )
            {
               assert5( t4->expr != 0 ) ;  // required for collation check
               if ( tfile4setCollationSeqCollatable( t4 ) == 0 )   // none available...
               {
                  t4->collateName = collate4machine ;
                  c4memset( t4->header.sortSeq, 0, 8 ) ;
                  return 0 ;
               }
            }

            // now, the collation depends on the code page of the data file
            if ( t4->indexFile->dataFile->codePage == cp1252 ) // windows ansi...
               t4->collateName = collate4generalCp1252 ;

            // now, the collation depends on the code page of the data file
            if ( t4->indexFile->dataFile->codePage == cp1250 ) // windows eastern european...
            {
               // AS Jan 8/03 - Support for upper-case croatian
               if ( t4->codeBase->collatingSequence == sort4croatian )
                  t4->collateName = collate4croatianCp1250 ;
               if ( t4->codeBase->collatingSequence == sort4croatianUpper )
                  t4->collateName = collate4croatianUpperCp1250 ;
               // generate name for header...
               c4memcpy( t4->header.sortSeq, "CB", 2 ) ; // indicate we are using a CodeBase collation...
               // use the collateName ordinal as the indicator.  Fill in with 0's on the left, so
               // we get:  "CB00001" for CodeBase collation 1...
               c4ltoa45( (long)t4->collateName, t4->header.sortSeq + 2, -5 ) ;
               t4->header.sortSeq[7] = 0 ;  // null end the string
               return 0 ;
            }

            if ( t4->indexFile->dataFile->codePage == cp437 ) // windows ansi...
               t4->collateName = collate4generalCp437 ;

            // cp0 defaults to windows ansi...
            if ( t4->indexFile->dataFile->codePage == cp0 )
               t4->collateName = collate4generalCp1252 ;

            // also, if done this way, we have a FoxPro compatible file, so mark the GENERAL entry
            // in the tag header...
            if ( setHeader )
               c4memcpy( t4->header.sortSeq, "GENERAL\0", 8 ) ;

            return 0 ;
         }

         if ( setHeader && collateName != collate4machine )
         {
            assert5( t4->expr != 0 ) ;  // required for collation check
            if ( tfile4setCollationSeqCollatable( t4 ) == 0 )   // none available...
               collateName = collate4machine ;
         }

         t4->collateName = collateName ;
         if ( setHeader )
         {
            if ( collateName == collate4machine )
            {
               // if machine, then just set the header to 0
               c4memset( t4->header.sortSeq, 0, 8 ) ;
            }
            else
            {
               // AS 09/02/99 We want to create the file as FoxPro-compatible if possible
               // (i.e. if general collating sequence choice matches data4's code page...)
               if ( ( collateName == collate4generalCp1252 && t4->indexFile->dataFile->codePage == cp1252 ) ||
                    ( collateName == collate4generalCp437 && t4->indexFile->dataFile->codePage == cp437  ) )
               {
                  c4memcpy( t4->header.sortSeq, "GENERAL\0", 8 ) ;
               }
               else
               {
                  // generate name for header...
                  c4memcpy( t4->header.sortSeq, "CB", 2 ) ; // indicate we are using a CodeBase collation...
                  // use the collateName ordinal as the indicator.  Fill in with 0's on the left, so
                  // we get:  "CB00001" for CodeBase collation 1...
                  c4ltoa45( (long)collateName, t4->header.sortSeq + 2, -5 ) ;
                  t4->header.sortSeq[7] = 0 ;  // null end the string
               }
            }
         }

         return 0 ;
      }



      /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
      // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
      /*
      int tfile4setCodePage( TAG4FILE *t4, const int type )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         // only if the tag is general should we verify the codepage
         // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
         // if ( t4->vfpInfo.sortType == sort4general )
         if ( collation4get( t4->collateName )->collateType != collate4machineByteOrder )
         {
            switch ( type )
            {
               case cp0:
                  t4->vfpInfo.cpPtr = CodePage_1252 ;   // set codepage 1252 as the default codepage
                  break ;
               #ifdef S4CODEPAGE_1252
                  case cp1252:
                     t4->vfpInfo.cpPtr = CodePage_1252 ;
                     break ;
               #endif
               #ifdef S4CODEPAGE_437
                  case cp437:
                     t4->vfpInfo.cpPtr = CodePage_437 ;
                     break ;
               #endif
               default:
                  return error4( t4->codeBase, e4parm, E81610 ) ;
            }
            t4->vfpInfo.codePage = type ;
         }
         else
         {
            t4->vfpInfo.cpPtr = 0 ;
            t4->vfpInfo.codePage = cp0 ;
         }

         return 0 ;
      }
      */
      #endif /* S4FOX */



      #ifndef S4CLIPPER
      #ifndef S4OFF_WRITE
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, !S4OFF_WRITE */
      B4BLOCK *tfile4split( TAG4FILE *t4, B4BLOCK *oldBlock )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 || oldBlock == 0 )
            {
               error4( 0, e4parm_null, E91642 ) ;
               return 0 ;
            }
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return 0 ;

         #ifdef E4INDEX_VERIFY
            if ( b4verify( oldBlock ) == -1 )
            {
               error4describe( oldBlock->tag->codeBase, e4index, E91642, oldBlock->tag->alias, 0, 0 ) ;
               return 0 ;
            }
         #endif

         B4NODE newFileBlock = index4extend( t4->indexFile ) ;
         if ( b4nodeInvalid( newFileBlock ) )
            return 0 ;

         B4BLOCK *newBlock = b4alloc( t4, newFileBlock ) ;
         if ( newBlock == 0 )
            return 0 ;

         newBlock->changed = 1 ;
         oldBlock->changed = 1 ;

         #ifdef S4FOX
            int rc ;
            if ( b4leaf( oldBlock ) )
               rc = tfile4leafSplit( t4, oldBlock, newBlock ) ;
            else
               rc = tfile4branchSplit( t4, oldBlock, newBlock ) ;

            if ( rc < 0 )
               return 0 ;

            newBlock->header.rightNode = oldBlock->header.rightNode ;
            newBlock->header.leftNode = oldBlock->fileBlock ;
            oldBlock->header.rightNode = newBlock->fileBlock ;

            if ( b4nodeValid( newBlock->header.rightNode ) )   /* must change left ptr for next block over */
            {
               #ifdef S4BYTE_SWAP
                  newBlock->fileBlock.node = x4reverseLong( (void *)&newBlock->fileBlock ) ;
               #endif
               FILE4LONG pos ;
               b4nodeGetFilePosition( t4->indexFile, newBlock->header.rightNode, &pos ) ;
               file4longAdd( &pos, 2 * sizeof( short ) ) ;
               rc = file4writeInternal( &t4->indexFile->file, pos, &newBlock->fileBlock, sizeof( newBlock->header.leftNode ) ) ;
               if ( rc < 0 )
                  return 0 ;
               #ifdef S4BYTE_SWAP
                  newBlock->fileBlock.node = x4reverseLong( (void *)&newBlock->fileBlock ) ;
               #endif
            }
         #else
            /* NNNNOOOO  N - New, O - Old */
            newBlock->nKeys  = (short)( ( b4numKeys( oldBlock ) + 1 ) / 2 ) ;
            oldBlock->nKeys -= b4numKeys( newBlock ) ;
            newBlock->keyOn  = oldBlock->keyOn ;

            int totLen = t4->indexFile->header.blockRw - sizeof(oldBlock->nKeys) - sizeof(oldBlock->dummy) ;
            int newLen = b4numKeys( newBlock ) * t4->header.groupLen ;

            c4memcpy( (void *)b4key( newBlock, 0 ), (void *)b4key( oldBlock, 0 ), newLen ) ;
            c4memmove( b4key(oldBlock,0), b4key( oldBlock, b4numKeys( newBlock )), totLen - newLen ) ;
            oldBlock->keyOn = oldBlock->keyOn - b4numKeys( newBlock ) ;
            #ifdef I4PRINT
               #ifdef I4PRINT_TAG_NAME
                  if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
               #endif
               {
                  struct _timeb mTime ;
                  char *outTime, dump[255] ;
                  _ftime( &mTime ) ;
                  outTime = ctime( &( mTime.time ) ) ;

                  sprintf( dump, "id = %ld, split: old-fb = %uld, new-fb = %uld, time: %s\r\n", (long)t4->indexFile, oldBlock->fileBlock, newBlock->fileBlock, outTime ) ;
                  log5( dump ) ;
                  B4KEY_DATA *keyData = b4key( newBlock, 1 )  ;
                  sprintf( dump, "          1st split new fb key: recno = %ld, key = %s\r\n", (long)keyData->num, (char *)keyData->value ) ;
                  log5( dump, 1 ) ;
               }
            #endif
         #endif

         return newBlock ;
      }
      #endif /* !S4OFF_WRITE */
      #endif /* !S4CLIPPER */



      #ifdef S4FOX
      /* !S4OFF_INDEX, !S4CLIENT, S4FOX */
      int S4FUNCTION tfile4top( TAG4FILE *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         if ( t4->header.descending )   /* if descending, go top means go bottom */
            return tfile4rlBottom( t4 ) ;
         else
            return tfile4rlTop( t4 ) ;
      }
      #endif /* S4FOX */



      #ifdef S4MDX
      /* !S4OFF_INDEX, !S4CLIENT, S4MDX */
      int S4FUNCTION tfile4top( TAG4FILE *t4 )
      {
         int rc, rc2 ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         do
         {
            rc = tfile4upToRoot( t4 ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, rc, E91642 ) ;

            if ( rc != 2 )
            {
               ((B4BLOCK *)t4->blocks.lastNode)->keyOn = 0 ;
               do
               {
                  if ( (rc = tfile4down(t4)) < 0 )
                     return error4stack( t4->codeBase, rc, E91642 ) ;
                  ((B4BLOCK *)t4->blocks.lastNode)->keyOn = 0 ;
               } while ( rc == 0 ) ;
            }

            if ( rc == 2 )   /* failed due to read while locked */
            {
               rc2 = tfile4outOfDate( t4 ) ;
               return error4stack( t4->codeBase, rc2, E91642 ) ;
            }
         } while ( rc == 2 ) ;

         return 0 ;
      }
      #endif /* S4MDX */



      /* !S4OFF_INDEX, !S4CLIENT */
      int tfile4unique( TAG4FILE *tag, const short int errUnique )
      {
         #ifdef S4FOX
            if ( ( tag->header.typeCode & 0x01 ) || ( tag->header.typeCode & 0x04 ) )
         #else
            if ( tag->header.unique )
         #endif
               return errUnique ;
         return 0 ;
      }



      /* !S4OFF_INDEX, !S4CLIENT */
      int tfile4up( TAG4FILE *t4 )
      {
         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( t4->blocks.lastNode == 0 )
            return 1 ;

         LINK4 *lnk = (LINK4 *)l4pop( &t4->blocks ) ;

         l4add( &t4->saved, lnk ) ;
         return 0 ;
      }



      #ifndef S4CLIPPER
      #ifndef S4OFF_WRITE
      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER, !S4OFF_WRITE */
      int tfile4update( TAG4FILE *t4 )
      {
         B4BLOCK *blockOn ;
         int rc ;
         FILE4LONG pos ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         if ( error4code( t4->codeBase ) < 0 )
            return e4codeBase ;

         for( blockOn = 0 ;; )
         {
            blockOn = (B4BLOCK *)l4next(&t4->saved,blockOn) ;
            if ( blockOn == 0 )
               break ;
            assert5( b4nodeValid( blockOn->fileBlock ) || blockOn->changed == 0 ) ;
            rc = b4flush( blockOn ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
         }

         for( blockOn = 0 ;; )
         {
            blockOn = (B4BLOCK *)l4next(&t4->blocks,blockOn) ;
            if ( blockOn == 0 )
               break ;
            rc = b4flush( blockOn ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;
         }

         if ( t4->rootWrite )
         {
            #ifdef I4PRINT
               #ifdef I4PRINT_TAG_NAME
                  if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
               #endif
               {
                  char dump[255] ;
                  sprintf( dump, "rootWriting: i4file = %ld, root = %ld\r\n", (long)t4->indexFile, (long)b4node(t4->header.root) ) ;
                  log5( dump ) ;
               }
            #endif
            #ifdef S4BYTE_SWAP
               t4->header.root.node = x4reverseLong( (void *)&t4->header.root ) ;
            #endif

            b4nodeGetFilePosition( t4->indexFile, t4->headerOffset, &pos ) ;
            rc = file4writeInternal( &t4->indexFile->file, pos, &t4->header.root, sizeof(t4->header.root)) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, (short)rc, E91642 ) ;

            #ifdef S4BYTE_SWAP
               t4->header.root.node = x4reverseLong( (void *)&t4->header.root ) ;
            #endif

            t4->rootWrite = 0 ;
         }

         return 0 ;
      }
      #endif /* !S4OFF_WRITE */



      /* !S4OFF_INDEX, !S4CLIENT, !S4CLIPPER */
      int tfile4upToRoot( TAG4FILE *t4 )
      {
         LINK4 *linkOn ;

         #ifdef E4PARM_LOW
            if ( t4 == 0 )
               return error4( 0, e4parm_null, E91642 ) ;
         #endif

         for ( ;; )
         {
            linkOn = (LINK4 *)l4pop( &t4->blocks ) ;
            if ( linkOn == 0 )
               return tfile4down( t4 ) ;
            l4add( &t4->saved, linkOn ) ;
         }
      }
      #endif /* !S4CLIPPER */



      // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
      /*
      #ifdef S4FOX
      #ifdef S4VFP_KEY
      int tfile4vfpKey( TAG4FILE *t4 )
      {
         // returns 1 (true) if tag uses double-length keys--otherwise 0 (false) is returned
         switch( t4->vfpInfo.sortType )
         {
            case sort4machine:
               return 0 ;
            case sort4general:
               return 1 ;
            default:
               return error4( 0, e4parm, E91642 ) ;
         } ;
      }
      #endif // S4VFP_KEY
      #endif // S4FOX
      */
   #endif /* !S4CLIENT */



   /* !S4OFF_INDEX */
   short int S4FUNCTION t4unique( const TAG4 *tag )
   {
      #ifdef E4VBASIC
         #ifdef S4CB51
            if ( c4parm_check ( tag, 4, E40150 ) ) return 0 ;
         #else
            if ( c4parm_check ( tag, 4, E91639 ) ) return 0 ;
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( tag == 0 )
            return error4( 0, e4parm_null, E91639 ) ;
         if ( tag->tagFile == 0 )
            return error4( 0, e4parm, E91639 ) ;
      #endif

      #ifdef S4CLIENT
         return tag->errUnique ;
      #else
         return tfile4unique( tag->tagFile, tag->errUnique ) ;
      #endif
   }



   #ifndef S4SERVER
   /* !S4OFF_INDEX, !S4SERVER */
   int S4FUNCTION t4uniqueSet( TAG4 *t4, const short uniqueCode )
   {
      #ifdef E4VBASIC
         if ( c4parm_check ( t4, 4, E91601 ) ) return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91601 ) ;
      #endif

      if ( uniqueCode == t4unique( t4 ) )  /* possibly both zero ok */
         return 0 ;

      /* if not zero matches, then neither should be zero */
      #ifdef E4PARM_HIGH
         #ifdef S4CLIENT_OR_FOX
            if ( t4unique( t4 ) == r4candidate )
            {
               if ( uniqueCode != e4candidate )
                  return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
               return t4uniqueSetLow( t4, uniqueCode, 1 ) ;
            }
            if ( t4unique( t4 ) == e4candidate )
            {
               if ( uniqueCode != r4candidate )
                  return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
               return t4uniqueSetLow( t4, uniqueCode, 1 ) ;
            }
         #endif
         if ( uniqueCode != e4unique && uniqueCode != r4unique && uniqueCode != r4uniqueContinue )
            return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
      #endif

      return t4uniqueSetLow( t4, uniqueCode, 1 ) ;
   }
   #endif /* !S4SERVER */



   #ifdef S4CLIENT
      /*  !S4OFF_INDEX, S4CLIENT */
      int S4FUNCTION t4uniqueSetLow( TAG4 *t4, const short uniqueCode, const char doZeroCheck )
      {
         CODE4 *c4 = t4->index->data->codeBase ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         #ifndef S4OFF_TRAN
            if ( code4transEnabled( c4 ) && doZeroCheck )  /* user cannot request from within transaction */
            {
               if ( code4trans( c4 )->currentTranStatus == r4active )
                  return error4( c4, e4transViolation, E81608 ) ;
            }
         #endif

         if ( uniqueCode == t4unique( t4 ) )
            return 0 ;

         /* verify that the tag is unique before setting */
         if ( doZeroCheck )
         {
            if ( t4unique( t4 ) == 0 )
               return error4( c4, e4parm, E81609 ) ;
         }

         CONNECTION4 *connection = t4->index->data->dataFile->connection ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
               return error4( c4, e4struct, E91601 ) ;
         #endif

         int rc = connection4assign( connection, CON4UNIQUE_SET, data4clientId( t4->index->data ), data4serverId( t4->index->data ) ) ;
         if ( rc < 0 )
            return rc ;

         CONNECTION4UNIQUE_INFO_IN *infoIn ;
         connection4addData( connection, NULL, sizeof(CONNECTION4UNIQUE_INFO_IN), (void **)&infoIn ) ;
         CONNECTION4UNIQUE_TAG_INFO *tagInfo ;
         connection4addData( connection, NULL, sizeof(CONNECTION4UNIQUE_TAG_INFO), (void **)&tagInfo ) ;
         infoIn->numTags = htons5(1) ;
         tagInfo->unique = htons5(uniqueCode) ;
         c4memcpy( tagInfo->alias, t4->tagFile->alias, LEN4TAG_ALIAS ) ;
         tagInfo->alias[LEN4TAG_ALIAS] = 0 ;
         connection4sendMessage( connection ) ;

         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E91601 ) ;

         rc = connection4status( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E91601 ) ;

         t4->errUnique = uniqueCode ;

         return rc ;
      }
   #endif /* S4CLIENT */



   #ifdef S4STAND_ALONE
   /* !S4OFF_INDEX, S4STAND_ALONE */
   int S4FUNCTION t4uniqueSetLow( TAG4 *t4, const short uniqueCode, const char doZeroCheck )
   {
      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91601 ) ;
         #ifdef S4CLIENT_OR_FOX
            if ( t4unique( t4 ) == r4candidate )  /* can't change a candidate setting */
            {
               if ( uniqueCode != e4candidate )
                  return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
            }
            else
               if ( t4unique( t4 ) == e4candidate )  /* can't change a candidate setting */
               {
                  if ( uniqueCode != r4candidate )
                     return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
               }
               else
         #endif
         if ( uniqueCode != e4unique && uniqueCode != r4unique && uniqueCode != r4uniqueContinue && uniqueCode != 0 )
            return error4( t4->index->data->codeBase, e4parm, E91601 ) ;
      #endif

      /* verify that the tag is unqiue before setting */
      if ( doZeroCheck )
         if ( t4unique( t4 ) == 0 )
            return error4( t4->index->data->codeBase, e4parm, E81609 ) ;

      t4->errUnique = uniqueCode ;

      return 0 ;
   }
   #endif /* S4STAND_ALONE */


   #ifndef S4CLIENT
      int S4FUNCTION t4infoN( TAG4 *tag )
      {
         // for odbc - returns -1 if tag->expr is invalid
         // AS 06/22/00 the 'expr' member may be NULL if the expression is invalid but file is read-only (i.e. not changing, so just avoid tag)
         if ( tag->tagFile->expr == 0 )
            return -1 ;
         return tag->tagFile->expr->infoN ;
      }



      void S4FUNCTION t4exprContextSet( TAG4 *tag, DATA4 *data )
      {
         // for odbc
         expr4context( tag->tagFile->expr, data ) ;
      }



      FIELD4 * S4FUNCTION t4field( TAG4 *tag )
      {
         // for odbc
         // for odbc - returns 0 if tag->expr is invalid
         // AS 06/22/00 the 'expr' member may be NULL if the expression is invalid but file is read-only (i.e. not changing, so just avoid tag)
         if ( tag->tagFile->expr == 0 )
            return 0 ;
         return tag->tagFile->expr->info[0].fieldPtr ;
      }



      INDEX4 * S4FUNCTION t4index( TAG4 *tag )
      {
         // for odbc
         return tag->index ;
      }



      int S4FUNCTION tfile4exprNullLow( TAG4FILE *tagFile )
      {
         // for odbc
         return expr4nullLow( tagFile->expr, 0 ) ;
      }



      TAG4FILE * S4FUNCTION t4tagFile( TAG4 *tag )
      {
         // for odbc
         return tag->tagFile ;
      }
   #endif /* !S4CLIENT */



   // AS Oct 22/02 - support for t4seekN in client/server
   int t4seekNdo( DATA4 *data, TAG4 *tag, const char *seekValue, const short inputKeyLen, short doDataPosition )
   {
      #ifdef S4CLIENT
         // without a data position, main purpose is to give seek result
         return d4seekServer( data, seekValue, inputKeyLen, 0, CON4SEEK, (Bool5)doDataPosition ) ;
      #else
         // the same as d4seek(), with optional positioning of data file
         #ifdef E4PARM_HIGH
            if ( tag == 0 || seekValue == 0 || tag->tagFile == 0 )
               return error4( 0, e4parm_null, E91639 ) ;
         #endif

         TAG4 *tagSelected = d4tagSelected( data ) ;
         d4tagSelect( data, tag ) ;
         int rc = d4seekNLow( data, seekValue, inputKeyLen, doDataPosition ) ;
         d4tagSelect( data, tagSelected ) ;

         return rc ;
      #endif
   }



   #ifndef S4SERVER
      int S4FUNCTION t4seekN( TAG4 *tag, const char *seekValue, const short inputKeyLen, short doDataPosition )
      {
         return t4seekNdo( tag->index->data, tag, seekValue, inputKeyLen, doDataPosition ) ;
      }
   #endif
#endif /* S4OFF_INDEX */
