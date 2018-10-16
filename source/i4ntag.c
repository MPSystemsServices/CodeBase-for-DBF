/* i4ntag.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4CLIENT

/* S4CLIENT */
TAG4 *S4FUNCTION t4openLow( DATA4 *d4, INDEX4 *i4ndx, const char *fileName, const char *indexName )
{
   CODE4 *c4 ;
   CONNECTION4OPEN_TAG_INFO_IN *dataIn ;
   CONNECTION4OPEN_TAG_INFO_OUT *out ;
   CONNECTION4 *connection ;
   int rc ;
   TAG4 *returnTag ;

   #ifdef E4VBASIC
      if ( c4parm_check( d4, 2, E94903 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( d4 == 0 || fileName == 0 )
      {
         error4( 0, e4parm_null, E94903 ) ;
         return 0 ; ;
      }
   #endif

   c4 = d4->codeBase ;
   if ( error4code( c4 ) < 0 )
      return 0 ;

   switch( code4indexFormat( c4 ) )
   {
      case r4ntx:
         break ;
      default:
      {
         error4( c4, e4notSupported, E81719 ) ;
         return 0 ;
      }
   }

   if ( strlen( fileName ) > LEN4PATH )
   {
      error4( c4, e4name, E94903 ) ;
      return 0 ;
   }

   connection = d4->dataFile->connection ;
   if ( connection == 0 )
   {
      error4( c4, e4connection, E81704 ) ;
      return 0 ;
   }
   connection4assign( connection, CON4TAG_OPEN, data4clientId( d4 ), data4serverId( d4 ) ) ;
   connection4addData( connection, NULL, sizeof( CONNECTION4OPEN_TAG_INFO_IN ), (void **)&dataIn ) ;
   dataIn->openForCreate = htons5(c4->openForCreate) ;

   #ifdef S4SINGLE
      dataIn->exclusiveClient = 1 ;
   #else
      if ( c4->singleOpen == OPEN4DENY_RW )
         dataIn->accessMode = htons5(OPEN4DENY_RW) ;
      else
         dataIn->accessMode = htons5(c4->accessMode) ;
   #endif

   /* AS 07/21/99 - added for win 95/98 to avoid endless lazy writes*/
   dataIn->fileFlush = c4->fileFlush ;
   dataIn->readOnly = c4->readOnly ;
   dataIn->safety = c4->safety ;  /* for catalog */
   dataIn->errDefaultUnique = htons5(c4->errDefaultUnique) ;
   u4ncpy( dataIn->tagName, fileName, sizeof( dataIn->tagName ) ) ;
   if ( i4ndx != 0 )
      dataIn->hasIndex = 1 ;

   if ( indexName != 0 )
      u4ncpy( dataIn->indexName, indexName, sizeof( dataIn->indexName ) ) ;

   if ( i4ndx != 0 )
      dataIn->nameLen = strlen( i4ndx->indexFile->accessName ) + 1 ;
   if ( i4ndx != 0 )
      connection4addData( connection, i4ndx->indexFile->accessName, dataIn->nameLen, NULL ) ;
   dataIn->nameLen = ntohs5( dataIn->nameLen ) ;
   connection4sendMessage( connection ) ;
   rc = connection4receiveMessage( connection ) ;
   if ( rc < 0 )
   {
      error4( c4, rc, E81701 ) ;
      return 0 ;
   }
   rc = connection4status( connection ) ;
   if ( rc != 0 )
   {
      if ( rc < 0 )
      {
         if ( c4->errOpen == 0 )
         {
            if ( error4code( c4 ) >= 0 )
               error4set( c4, r4noOpen ) ;
         }
         else
            connection4error( connection, c4, rc, E94903 ) ;
      }
      return 0 ;
   }

   out = (CONNECTION4OPEN_TAG_INFO_OUT *)connection4data( connection ) ;

   rc =  client4indexSetup( c4, d4, d4->dataFile, 1, 0, (char *)out + sizeof(CONNECTION4OPEN_TAG_INFO_OUT),
        (unsigned int)connection4len( connection ), (char*)fileName, i4ndx ) ;
   if ( rc < 0 )
   {
      error4( c4, e4connection, E94903 ) ;
      return 0 ;
   }

   if ( rc == r4noOpen )  // means client already has it open
   {
      if ( c4->errOpen == 0 )
          error4set( c4, r4noOpen ) ;
      else
          error4( c4, e4instance, E91706 ) ;
      return 0 ;
   }

   i4setup( c4, d4, (char *)fileName, 0, out->autoOpened, i4ndx ) ;

   /* AS 10/07/99 Should use the tag name, not the full fileName... */
   char tagLookup[LEN4TAG_ALIAS + 1] ; // LY Apr 6/04 : +1 for null terminator
   u4namePiece( tagLookup, LEN4TAG_ALIAS, fileName, 0, 0 ) ;

   returnTag = d4tag( d4, tagLookup ) ;
   #ifndef S4OFF_TRAN
      if ( returnTag )   /* CS 10/07/99 GPF Fix */
         returnTag->isValid = 1 ;
   #endif
   return returnTag ;
}



/* S4CLIENT */
int S4FUNCTION t4close( TAG4 *t4 )
{
   INDEX4 *i4 ;
   CODE4 *c4 ;

   #ifdef E4VBASIC
      if ( c4parm_check( t4, 4, E91637 ) )
         return -1 ;
   #endif

   i4 = t4->index ;
   c4 = i4->codeBase ;

   if ( code4indexFormat( c4 ) != r4ntx )  /* function not supported */
      return error4( c4, e4notSupported, E81719 ) ;

   if ( l4numNodes( &i4->tags ) == 1 )   /* only the one tag, so remove index */
      return i4closeLow( i4 ) ;
   else /* just free up the one tag so it is no longer available */
   {
      l4remove( &i4->tags, t4 ) ;
      mem4free( c4->tagMemory, t4 ) ;
   }

   return 0 ;
}
#endif /* S4CLIENT */



#ifndef S4CLIENT
#ifndef S4OFF_INDEX

/* not S4CLIENT, not S4OFF_INDEX */
int tfile4type( TAG4FILE *t4 )
{
 #ifdef S4FOX
    return t4->expr->type ;
 #endif
 #ifdef S4CLIPPER
    return t4->expr->type ;
 #endif
 #ifdef S4MDX
    return t4->header.type ;
 #endif
}



#ifdef S4CLIPPER
#ifdef E4ANALYZE

/* for debug purposes only, else is an inline function */
/* S4CLIPPER, E4ANALYZE, not S4CLIENT, not S4OFF_INDEX */
B4BLOCK *S4FUNCTION tfile4block( TAG4FILE *t4 )
{
   if ( t4 == 0 )
   {
      error4( 0, e4parm_null, E91642 ) ;
      return 0 ;
   }
   if ( t4->blocks.lastNode == 0 )
   {
      error4( 0, e4struct, E91642 ) ;
      return 0 ;
   }
   return (B4BLOCK *)t4->blocks.lastNode ;
}
#endif /* E4ANALYZE */



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4rlBottom( TAG4FILE *t4 )
{
   int rc ;
   B4BLOCK *blockOn ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return -1 ;

   do
   {
      rc = tfile4upToRoot( t4 ) ;
      if ( rc < 0 )
         return -1 ;

      if ( rc != 2 )
      {
         b4goEof( tfile4block( t4 ) ) ;
         do
         {
            rc = tfile4down( t4 ) ;
            if ( rc < 0 )
               return -1 ;
            b4goEof( tfile4block( t4 ) ) ;
         } while ( rc == 0 ) ;
      }

      if ( rc == 2 )   /* failed due to read while locked */
         tfile4outOfDate( t4 ) ;
   } while ( rc == 2 ) ;

   blockOn = tfile4block(t4) ;
   if ( blockOn->keyOn > 0 )
   {
      blockOn->keyOn = blockOn->nKeys-1 ;
      assert5( blockOn->nKeys <= t4->header.keysMax && blockOn->nKeys > 0 ) ;
   }

   #ifdef E4ANALYZE
      if ( blockOn->keyOn < 0 )
         return error4( t4->codeBase, e4info, E91642 ) ;
   #endif

   return 0 ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
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



#ifdef S4HAS_DESCENDING
/* S4CLIPPER, HAS_DESCENDING, not S4CLIENT, not S4OFF_INDEX */
void S4FUNCTION tfile4descending( TAG4FILE *tag, const unsigned short int setting )
{
   tag->header.descending = setting ;
}
#endif



/* Returns  1 - Cannot move down; 0 - Success; -1 Error */
/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4down( TAG4FILE *t4 )
{
   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   B4BLOCK *blockOn = (B4BLOCK *)t4->blocks.lastNode ;

   unsigned long blockDown ;
   if ( blockOn == 0 )    /* Read the root block */
   {
      if ( t4->header.root == 0L || t4->header.root == INVALID4BLOCK_ID )
      {
         FILE4LONG pos ;
         file4longAssign( pos, t4->header.headerOffset + 2 * sizeof(short), 0 ) ;
         if ( file4readAllInternal( &t4->file, pos, &t4->header.root, 2 * sizeof(S4LONG)) < 0 )
            return -1 ;

         #ifdef S4BYTE_SWAP
            t4->header.root = x4reverseLong( (void *)&t4->header.root ) ;
            t4->header.eof = x4reverseLong( (void *)&t4->header.eof ) ;
         #endif
      }
      blockDown = t4->header.root ;
   }
   else
   {
      if ( b4leaf( blockOn ) )
         return 1 ;
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp for HP */
         memcpy( &blockDown, &b4key( blockOn, blockOn->keyOn )->pointer, sizeof(S4LONG) ) ;
      #else
         blockDown = b4key( blockOn, blockOn->keyOn )->pointer ;
      #endif
      #ifdef E4ANALYZE
         if ( blockDown <= 0L || blockOn->nKeys -1 > t4->header.keysMax )
            error4( t4->codeBase, e4info, E81602 ) ;
      #endif
   }

   /* Get memory for the new block */
   B4BLOCK *popBlock = (B4BLOCK *)l4pop( &t4->saved ) ;
   B4BLOCK *newBlock = popBlock ;
   if ( newBlock == 0 )
      newBlock = b4alloc( t4, blockDown ) ;
   if ( newBlock == 0 )
      return -1 ;
   B4BLOCK *parent = (B4BLOCK *)l4last( &t4->blocks ) ;
   l4add( &t4->blocks, newBlock ) ;

   if ( popBlock == 0  ||  newBlock->fileBlock*I4MULTIPLY != blockDown )
   {
      #ifndef S4OFF_WRITE
         if ( newBlock->changed == 1 )
            if ( b4flush(newBlock) < 0 )
               return -1 ;
      #endif

      int rc = i4readBlock( &t4->file, blockDown, parent, newBlock ) ;

      if ( rc < 0 )
         return -1 ;

      if ( rc == 1 )
      {
         l4remove( &t4->blocks, newBlock ) ;
         l4add( &t4->saved, newBlock ) ;
         return 2 ;
      }

      for ( ;; )
      {
         blockOn = (B4BLOCK *)l4pop( &t4->saved ) ;
         if ( blockOn == 0 )
            break ;
         #ifndef S4OFF_WRITE
            if ( b4flush(blockOn) < 0 )
               return -1 ;
         #endif
         b4free( blockOn ) ;
      }
   }

   newBlock->keyOn = 0 ;
   return 0 ;
}



#ifndef S4OFF_WRITE
/* S4CLIPPER, not S4OFF_WRITE, not S4CLIENT, not S4OFF_INDEX */
int tfile4flush( TAG4FILE *t4 )
{
   int rc ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E94901 ) ;
   #endif

   rc = tfile4update( t4 ) ;
   #ifndef S4OPTIMIZE_OFF
      if ( rc )
         rc = file4flush( &t4->file ) ;
   #endif
   return rc ;
}
#endif



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4freeAll( TAG4FILE *t4 )
{
   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   while ( tfile4up( t4 ) == 0 ) ;
   return tfile4freeSaved( t4 ) ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4freeSaved( TAG4FILE *t4 )
{
   B4BLOCK *blockOn ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   #ifndef S4OFF_WRITE
      if ( tfile4update( t4 ) < 0 )
         return -1 ;
   #endif

   for ( ;; )
   {
      blockOn = (B4BLOCK *)l4pop( &t4->saved ) ;
      if ( blockOn == 0 )
         return 0 ;
      #ifndef S4OFF_WRITE
         if ( b4flush( blockOn ) < 0 )
            return -1 ;
      #endif
      b4free( blockOn ) ;
   }
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
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
   return b4key( b4, b4->keyOn ) ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
unsigned long S4FUNCTION tfile4recNo( TAG4FILE *t4 )
{
   B4BLOCK *blockOn ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return (long)error4( 0, e4parm_null, E91642 ) ;
   #endif

   blockOn = (B4BLOCK *)t4->blocks.lastNode ;
   if ( blockOn == 0 )
      return ULONG_MAX - 1 ;
   if ( blockOn->keyOn >= blockOn->nKeys )
      return ULONG_MAX ;

   return b4recNo( blockOn, blockOn->keyOn ) ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int S4FUNCTION tfile4seek( TAG4FILE *t4, const void *ptr, const int lenPtrIn )
{
   int rc, lenPtr ;
   B4BLOCK *blockOn ;
   int upperFnd, upperAft, incPos, dSet ;
   unsigned char *cPtr ;
   dSet = 0 ;
   cPtr = (unsigned char *)ptr ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 || ptr == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;
   lenPtr = lenPtrIn ;

   if ( lenPtr > t4->header.keyLen )
      lenPtr = t4->header.keyLen ;

   if ( t4->header.descending )   /* look for current item less one: */
   {
      for( incPos = lenPtr-1 ; dSet == 0 && incPos >=0 ; incPos-- )
      {
         if ( cPtr[incPos] == 0xFF )
            cPtr[incPos] = 0x00 ;
         else
         {
            cPtr[incPos]++ ;
            dSet = 1 ;
         }
      }
   }

   rc = 3 ;
   for(;;) /* Repeat until found */
   {
      while ( rc >= 2 )
      {
         if ( rc == 2 )
            tfile4outOfDate( t4 ) ;
         rc = tfile4upToRoot( t4 ) ;
         upperFnd = 0 ;
         upperAft = 0 ;

         if ( rc < 0 )
            return -1 ;
      }
      blockOn = (B4BLOCK *)t4->blocks.lastNode ;
      #ifdef E4ANALYZE
         if ( blockOn == 0 )
            return error4( t4->codeBase, e4info, E91642 ) ;
      #endif

      rc = b4seek( blockOn, (char *)ptr, lenPtr ) ;
      if ( b4leaf( blockOn ) )
      {
         if ( rc == r4after && upperAft && blockOn->keyOn >= blockOn->nKeys )
             while( upperAft-- > 1 )
                tfile4up( t4 ) ;
         if ( rc == 0 || !upperFnd )
            break ;

         while( upperFnd-- > 1 )
             tfile4up( t4 ) ;
         rc = 0 ;
         break ;
      }
      if ( rc == 0 )
      {
         upperFnd = 1 ;
         upperAft = 0 ;
      }
      else
         if ( rc == r4after && !upperFnd && !( blockOn->keyOn >= blockOn->nKeys ) )
            upperAft = 1 ;

      rc = tfile4down( t4 ) ;
      if ( rc < 0 )
         return -1 ;

      if ( upperFnd )
         upperFnd++ ;
      if ( upperAft )
         upperAft++ ;
   }
   if ( t4->header.descending )   /* must go back one! */
   {
      /* reset the search_ptr ; */
      incPos++ ;
      cPtr[incPos]-- ;
      c4memset( cPtr + incPos + 1, 0xFF, lenPtr - incPos - 1 ) ;
      if ( dSet )
      {
         rc = (int)tfile4skip( t4, -1L ) ;
         if ( rc == 0L )  /* bof = eof condition */
         {
            b4goEof( blockOn ) ;
            rc = r4eof ;
         }
         else
         {
            if ( (u4memcmp)( b4keyKey( tfile4block( t4 ), tfile4block( t4 )->keyOn ), ptr, lenPtr ) )
               rc = r4after ;
            else
               rc = 0 ;  /* successful find */
         }
      }
      else
      {
         if ( rc == 0 )  /* the item was found, so go top, */
            tfile4top( t4 ) ;
         else  /* otherwise want an eof type condition */
         {
            b4goEof( blockOn ) ;
            rc = r4eof ;
         }
      }
   }
   return rc ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
long S4FUNCTION tfile4skip( TAG4FILE *t4, long numSkip )
{
   int rc, sign, seekSpecial ;
   B4BLOCK *blockOn ;

   long j ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return (long)error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   if ( numSkip < 0)
      sign = -1 ;
   else
      sign = 1 ;

   blockOn = (B4BLOCK *)t4->blocks.lastNode ;
   if ( blockOn == 0 )
   {
      rc = tfile4top( t4 ) ;
      if ( rc < 0 )
         return -numSkip ;
      blockOn = (B4BLOCK *)t4->blocks.lastNode ;
   }

   for( j = numSkip; j != 0; j -= sign )  /* skip 1 * numSkip */
   {
      if ( b4leaf(blockOn) )
      {
         if ( b4skip( blockOn, (long) sign) != sign )  /* go up */
         {
            int go_on = 1 ;
            while ( go_on )
            {
               if ( l4prev( &t4->blocks, t4->blocks.lastNode ) == 0 )  /* root block */
               {
                  if ( numSkip > 0 && t4->header.descending == 0 || numSkip <= 0 && t4->header.descending == 1 )
                  {
                     if ( tfile4bottom( t4 ) < 0 )
                        return -numSkip ;
                  }
                  else
                     if ( tfile4top( t4 ) < 0 )
                        return -numSkip ;

                  return ( numSkip - j ) ;
               }

               rc = tfile4up( t4 ) ;
               blockOn = tfile4block( t4 ) ;
               if ( rc != 0 ) return -1 ;

               if ( sign > 0 )  /* forward skipping */
               {
                  if ( !( blockOn->keyOn >= blockOn->nKeys ) )
                     go_on = 0 ;
               }
               else   /* backward skipping */
               {
                  if ( ! (blockOn->keyOn == 0) )
                  {
                     b4skip( blockOn, -1L ) ;
                     go_on = 0 ;
                  }
               }
            }
         }
      }
      else
      {
         if ( sign > 0 )
            b4skip( blockOn, 1L ) ;

         /* save the current key in case skip fails */
         if ( blockOn->keyOn >= blockOn->nKeys )  /* case where no proper key to copy exists */
         {
            if ( blockOn->nKeys == 0 )   /* invalid, so must discard this block */
            {
               /* codeBase can't recover from this gracefully, so just go back up saying
                  that couldn't skip */
               return 0 ;
            }

            /* not on a true key, so instead just go to the old position, and then try the skip again (unless r4after)*/
            // LY Oct 15/04 : t4write.c crashing due to t4->codeBase->savedKey == 0
            // AS Oct 29/04 - fix compiler warnings...
            if ( (unsigned)t4->header.keyLen > t4->codeBase->savedKeyLen )
            {
               if ( u4allocAgain( t4->codeBase, &t4->codeBase->savedKey, &t4->codeBase->savedKeyLen, t4->header.keyLen  ) < 0 )
                  return e4memory ;
               t4->codeBase->savedKeyLen = t4->header.keyLen ;
            }
            c4memcpy( t4->codeBase->savedKey, b4keyKey( blockOn, blockOn->keyOn - 1 ), t4->header.keyLen ) ;
            seekSpecial = 1 ;
         }
         else
         {
            // LY Oct 15/04 : t4write.c crashing due to t4->codeBase->savedKey == 0
            // AS Oct 29/04 - fix compiler warnings...
            if ( (unsigned)t4->header.keyLen > t4->codeBase->savedKeyLen )
            {
               if ( u4allocAgain( t4->codeBase, &t4->codeBase->savedKey, &t4->codeBase->savedKeyLen, t4->header.keyLen  ) < 0 )
                  return e4memory ;
               t4->codeBase->savedKeyLen = t4->header.keyLen ;
            }
            c4memcpy( t4->codeBase->savedKey, b4keyKey( blockOn, blockOn->keyOn ), t4->header.keyLen ) ;
            seekSpecial = 0 ;
         }

         while ( (rc = tfile4down( t4 ) ) == 0 )
         {
            if ( sign < 0 )
            {
               blockOn = tfile4block( t4 ) ;
               b4goEof( blockOn ) ;
               if ( b4leaf( blockOn ) )
                  blockOn->keyOn-- ;
            }
         }
         if ( rc < 0 )
            return -numSkip ;

         if ( rc == 2 )   /* failed on i/o, seek current spot to make valid */
         {
            #ifndef S4OPTIMIZE_OFF
            #ifndef S4SINGLE
               file4refresh( &t4->file ) ;
            #endif
            #endif
            rc = tfile4seek( t4, t4->codeBase->savedKey, t4->header.keyLen ) ;
            if ( rc < 0 )
               return -numSkip ;
            if ( rc == r4after )   /* means skipped 1 ahead */
               if ( j != 1 )
                  j-- ;
            if ( seekSpecial == 1 )  /* means we are actually on wrong pos, need to skip one */
               j += sign ;   /* pre-increment # of skip times */
         }

         blockOn = tfile4block( t4 ) ;
      }
   }
   return numSkip ;
}



#ifndef S4OFF_WRITE
/* NTX only needs to do a copy and adjust the index pointers */
/* if extraOld is true then the extra key is placed in old, otherwise in new */
/* S4CLIPPER, not S4OFF_WRITE, not S4CLIENT, not S4OFF_INDEX */
B4BLOCK *tfile4split( TAG4FILE *t4, B4BLOCK *oldBlock, const int extraOld )
{
   if ( error4code( t4->codeBase ) < 0 )
      return 0 ;

   #ifdef E4INDEX_VERIFY
      if ( b4verify( oldBlock ) == -1 )
         error4describe( oldBlock->tag->codeBase, e4index, E91642, oldBlock->tag->alias, 0, 0 ) ;
   #endif

   #ifdef E4DEBUG
      b4verifyPointers( oldBlock ) ;
   #endif

   unsigned long newFileBlock = tfile4extend( t4 ) ;
   B4BLOCK *newBlock = b4alloc( t4, newFileBlock ) ;
   if ( newBlock == 0 )
      return 0 ;

   newBlock->changed = 1 ;
   oldBlock->changed = 1 ;

   assert5( oldBlock->data != 0 ) ;
   assert5( newBlock->data != 0 ) ;

   c4memcpy( newBlock->data, oldBlock->data, B4BLOCK_SIZE_INTERNAL - ( t4->header.keysMax + 2 ) * sizeof(short) ) ;

   if ( extraOld )
   {
      newBlock->nKeys = oldBlock->nKeys / 2 ;
      oldBlock->nKeys -= newBlock->nKeys ;
      if ( oldBlock->nKeys == newBlock->nKeys )
      {
         oldBlock->nKeys-- ;
         newBlock->nKeys++ ;
      }
      // newBlock->nKeys must be non-zero because extra key goes into old block if here
      assert5( newBlock->nKeys <= t4->header.keysMax && newBlock->nKeys > 0 ) ;
      assert5( oldBlock->nKeys <= t4->header.keysMax && (oldBlock->nKeys > 0 || newBlock->nKeys == 2 )) ;
   }
   else
   {
      newBlock->nKeys = oldBlock->nKeys ;
      oldBlock->nKeys = oldBlock->nKeys / 2 ;
      newBlock->nKeys -= oldBlock->nKeys ;
      assert5( newBlock->nKeys <= t4->header.keysMax && (newBlock->nKeys > 0 || oldBlock->nKeys == 2 )) ;
      // oldBlock->nKeys must be non-zero because extra key goes into new block if here
      assert5( oldBlock->nKeys <= t4->header.keysMax && oldBlock->nKeys > 0 ) ;
   }

   int isBranch = !b4leaf( oldBlock ) ;

   // AS 05/18/99 -- in branch case, don't move the last reference pointer over because if we have max key size
   // we don't actually have room for a key.  Instead copy the reference over...
   assert5( newBlock->nKeys >= 0 ) ;
   if ( isBranch )  // AS 05/18/99 -- here is the code to copy the extra entry...
   {
      c4memcpy( newBlock->pointers, &oldBlock->pointers[oldBlock->nKeys + 1], (newBlock->nKeys - 1) * sizeof(short) ) ;
      c4memcpy( &newBlock->pointers[newBlock->nKeys-1], oldBlock->pointers, (oldBlock->nKeys + 1) * sizeof(short) ) ;
      // copy the extra pointer over to the available slot...
      B4KEY_DATA *toKeyPtr = b4key( newBlock, newBlock->nKeys - 1 ) ;
      B4KEY_DATA *fromKeyPtr = b4key( oldBlock,  oldBlock->nKeys + newBlock->nKeys ) ;
      c4memcpy( &toKeyPtr->pointer, &fromKeyPtr->pointer, sizeof( long ) ) ;
      newBlock->nKeys -= 1 ;
   }
   else
   {
      c4memcpy( newBlock->pointers, &oldBlock->pointers[oldBlock->nKeys], newBlock->nKeys * sizeof(short) ) ;
      c4memcpy( &newBlock->pointers[newBlock->nKeys], oldBlock->pointers, oldBlock->nKeys * sizeof(short) ) ;
      /* leaf blocks need one more copy */
      newBlock->pointers[t4->header.keysMax] = oldBlock->pointers[t4->header.keysMax] ;
   }

   #ifdef E4DEBUG
      b4verifyPointers( oldBlock ) ;
      b4verifyPointers( newBlock ) ;
   #endif

   newBlock->keyOn = oldBlock->keyOn - oldBlock->nKeys - isBranch ;
   if ( newBlock->keyOn < 0 && extraOld == 1 )  // not adding to new anyway, just set to 0 for safety
       newBlock->keyOn = 0 ;
   // AS 05/05/98 is possible for newKeys to have 0 keys if new key going into newBlock
   assert5( newBlock->nKeys <= t4->header.keysMax && (newBlock->nKeys > 0 || (extraOld == 0 && (oldBlock->nKeys + isBranch ) == 2) )) ;
   assert5( newBlock->keyOn >= 0 && oldBlock->keyOn >= 0 ) ;

   return newBlock ;
}
#endif  /* S4OFF_WRITE */



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4rlTop( TAG4FILE *t4 )
{
   int rc ;

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
         return -1 ;

      if ( rc != 2 )
      {
         ((B4BLOCK *)t4->blocks.lastNode)->keyOn = 0 ;
         do
         {
            if ( (rc = tfile4down(t4)) < 0 )
               return -1 ;
            ((B4BLOCK *)t4->blocks.lastNode)->keyOn = 0 ;
         } while ( rc == 0 ) ;
      }

      if ( rc == 2 )   /* failed due to read while locked */
         tfile4outOfDate( t4 ) ;
   } while ( rc == 2 ) ;

   return 0 ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
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



#ifndef S4OFF_WRITE
/* S4CLIPPER, not S4OFF_WRITE, not S4CLIENT, not S4OFF_INDEX */
int tfile4update( TAG4FILE *t4 )
{

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   if ( tfile4updateHeader( t4 ) < 0 )
      return -1 ;

   B4BLOCK *blockOn ;
   for( blockOn = 0 ;; )
   {
      blockOn = (B4BLOCK *)l4next( &t4->saved ,blockOn ) ;
      if ( blockOn == 0 )
         break ;
      if ( b4flush(blockOn) < 0 )
         return -1 ;
   }

   for( blockOn = 0 ;; )
   {
      blockOn = (B4BLOCK *)l4next( &t4->blocks, blockOn ) ;
      if ( blockOn == 0 )
         break ;
      if ( b4flush( blockOn ) < 0 )
         return -1 ;
   }

   if ( t4->rootWrite )
   {
      #ifdef S4BYTE_SWAP
         t4->header.root = x4reverseLong( (void *)&t4->header.root ) ;
         t4->header.eof = x4reverseLong( (void *)&t4->header.eof ) ;
      #endif
      FILE4LONG pos ;
      file4longAssign( pos, t4->headerOffset + 2*sizeof( short ), 0 ) ;
      if ( file4writeInternal( &t4->file, pos, &t4->header.root, 2*sizeof(S4LONG) ) < 0 )
         return -1 ;
      #ifdef S4BYTE_SWAP
         t4->header.root = x4reverseLong( (void *)&t4->header.root ) ;
         t4->header.eof = x4reverseLong( (void *)&t4->header.eof ) ;
      #endif
      t4->rootWrite = 0 ;
   }

   return 0 ;
}
#endif /* S4OFF_WRITE */



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
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
         return tfile4down(t4) ;
      l4add( &t4->saved, linkOn ) ;
   }
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4close( TAG4FILE *t4, DATA4FILE *d4 )
{
   CODE4 *c4 ;
   int finalRc ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91638 ) ;
   #endif

   c4 = t4->codeBase ;
   // AS 01/24/00 -- if opening a group file with a missing tag, i4open() was returning 0
   // but not setting the errorCode to r4noOpen if errOpen was 0.  This was because the
   // positive error code was not being saved here...
   finalRc = error4set( c4, 0 ) ;

   #ifdef E4ANALYZE
      if ( t4->userCount <= 0 )
         return error4( c4, e4struct, E91638 ) ;
   #endif

   /* AS 07/22/99 if keepOpen == 2, we also need to avoid userCount decrement... */
   #ifdef S4SERVER
      if ( c4->server->keepOpen != 2 || file4getTemporary( &t4->file ) != 1 )
      {
   #endif
         t4->userCount-- ;
         if ( t4->userCount == 0 )
         {
            if ( tfile4freeAll( t4 ) < 0 )
               finalRc = error4set( c4, 0) ;
            if ( t4->expr )
               expr4free( t4->expr ) ;
            if ( t4->filter )
               expr4free( t4->filter ) ;
            mem4release( t4->blockMemory ) ;
            t4->blockMemory = 0 ;
            if ( file4openTest( &t4->file ) )
            {
               // AS 11/27/00 - doRemove must be set on client structure
               // if ( c4->doRemove == 1 )
               if ( c4getDoRemove( c4 ) == 1 )
                  file4setTemporary( &t4->file, 1, 0 ) ;
               if ( file4close( &t4->file ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
            }
            if ( t4->link.n != (LINK4 *)0 )
               l4remove( &d4->tagfiles, t4 ) ;
            mem4free( c4->tagFileMemory, t4 ) ;
            error4set( c4, finalRc ) ;
         }
   #ifdef S4SERVER
      }
   #endif

   return finalRc ;
}



// AS Oct 20/04 - need an internal close to bypass the transaction check for internal close requests (temporary files)
/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int t4closeLow( TAG4 *t4 )
{
   int finalRc ;
   DATA4 *d4 ;
   CODE4 *c4 ;

   if ( l4seek( &t4->index->tags, t4 ) == 1 )  /* i4close removes the tag, so if still there, was not called from i4closeLow() */
   {
      if ( l4numNodes( &t4->index->tags ) == 1 )   /* only the one tag, so remove index */
         return i4closeLow( t4->index ) ;
      else  /* must remove from the list manually */
         l4remove( &t4->index->tags, t4 ) ;
   }

   c4 = t4->tagFile->codeBase ;
   d4 = t4->index->data ;

   finalRc = error4set( c4, 0 ) ;

   #ifndef S4OFF_WRITE
      #ifndef S4OFF_TRAN
         if ( t4->isValid == 1 ) /* if invalid (failed create/open) then allow close */
      #endif
            if ( d4 )
               if ( d4update( d4 ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
   #endif

   #ifndef S4SINGLE
      if ( tfile4unlock( t4->tagFile, data4serverId( t4->index->data ) ) < 0 )
         finalRc = error4set( c4, 0 ) ;
   #endif
   /* 03/11/98 AS expression data mismatch problem --> if same data4 re-allocated */
   if ( t4->tagFile->expr != NULL ) /* could be NULL if improper open or if unsupported expression (read-only) */
      if ( t4->tagFile->expr->data == t4->index->data )
         t4->tagFile->expr->data = 0 ;

   if ( tfile4close( t4->tagFile, d4->dataFile ) < 0 )
      finalRc = error4set( c4, 0 ) ;
   mem4free( c4->tagMemory, t4 ) ;

   error4set( c4, finalRc ) ;

   return finalRc ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int S4FUNCTION t4close( TAG4 *t4 )
{
   #ifdef E4VBASIC
      if ( c4parm_check( t4, 4, E91637 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91637 ) ;
   #endif

   CODE4 *c4 = t4->tagFile->codeBase ;
   #ifndef S4OFF_TRAN
      /* May 10/96 AS added line below which otherwise caused bad indexes not to close good tags */
      if ( t4->index->isValid == 1 ) /* if invalid (failed create/open) then allow close */
         if ( t4->isValid == 1 ) /* if invalid (failed create/open) then allow close */
            if ( code4transEnabled( c4 ) )
               if ( code4trans( c4 )->currentTranStatus == r4active )  /* disallow on current active only */
                  return error4( c4, e4transViolation, E81522 ) ;
   #endif

   return t4closeLow( t4 ) ;
}



#ifndef S4OFF_WRITE
/* S4CLIPPER, not S4OFF_WRITE, not S4CLIENT, not S4OFF_INDEX */
unsigned long tfile4extend( TAG4FILE *t4 )
{
   CODE4 *c4 = t4->codeBase ;
   FILE4LONG pos ;

   if ( error4code( c4 ) < 0 )
      return INVALID4BLOCK_ID ;

   #ifdef E4ANALYZE
      if ( t4->header.version == t4->header.oldVersion )
      {
         error4( c4, e4info, E91636 ) ;
         return INVALID4BLOCK_ID ;
      }
   #endif

   unsigned long oldEof = t4->header.eof ;

   #ifdef S4SINGLE
      if ( oldEof != 0 )   /* case where free-list exists */
         t4->header.eof = 0L ;
      else
      {
   #endif
         oldEof = file4longGetLo( file4lenLow( &t4->file ) ) ;

         // AS Nov 27/03 - turns out to be invalid in multi-user since another user may be shrinking the file
         #if defined( E4MISC ) && defined( S4SINGLE )
            if ( oldEof <= t4->checkEof )
            {
               error4( c4, e4info, E91636 ) ;
               return INVALID4BLOCK_ID ;
            }
            t4->checkEof = oldEof ;
         #endif

         file4longAssign( pos, file4longGetLo( file4lenLow( &t4->file ) ) + 1024, 0 ) ;
         file4lenSetLow( &t4->file, pos ) ; /* and extend the file */
   #ifdef S4SINGLE
      }
   #endif

   return oldEof / 512 ;
}
#endif /* S4OFF_WRITE */



#ifdef P4ARGS_USED
   #pragma argsused
#endif
/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4go2( TAG4FILE *t4, const unsigned char *ptr, const unsigned long recNum, const int goAdd )
{
   int rc ;
   unsigned long rec ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 || ptr == 0 || recNum < 1 )
         return error4( 0, e4parm, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   rc = tfile4seek( t4, ptr, t4->header.keyLen ) ;
   if ( rc )
      return rc ;

   for( ;; )
   {
      rec = tfile4recNo( t4 ) ;
      if (rec == recNum )
         return 0 ;

      rc = (int)tfile4skip( t4, 1L ) ;
      if ( rc == -1 )
         return -1 ;
      if ( rc == 0 )
      {
         b4goEof( tfile4block( t4 ) ) ;
         return r4found ;
      }

      if ( (*t4->cmp)( tfile4keyData( t4 )->value, ptr, t4->header.keyLen ) )
         return r4found ;
   }
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
static TAG4FILE *tfile4open( DATA4 *d4, const char *fileName )
{
   TAG4FILE *tfile ;
   CODE4 *c4 ;
   char buf[258], buffer[1024] ;
   char exprBuf[I4MAX_EXPR_SIZE + 1] ;
   char *ptr, garbage ;
   FILE4SEQ_READ seqRead ;
   int rc, len, oldTagNameError ;
   FILE4LONG pos ;

   c4 = d4->codeBase ;

   u4ncpy( buf, fileName, sizeof( buf ) ) ;
   u4nameExt( buf, sizeof(buf), TAG4EXT, 0 ) ;

   oldTagNameError = c4->errTagName ;
   c4->errTagName = 0 ;
   tfile = dfile4tag( d4->dataFile, buf ) ;
   c4->errTagName = oldTagNameError ;
   if ( tfile != 0 ) /* because t4open() verifies no duplicates, this must be a duplicate data4 instance */
   {
      /* changed 09/19/95 - test program t4skip.c */
      tfile->userCount++ ;
      return tfile ;
      /* only one instance ever allowed */
/*      error4( c4, e4instance, E94906 ) ;*/
/*      return 0 ; */

      /*
      #ifndef S4SERVER
         if ( c4->singleOpen != OPEN4SPECIAL )   only one instance allowed...
         {
            error4( c4, e4instance, E94906 ) ;
            return 0 ;
         }
      #endif

      tfile->userCount++ ;
      return tfile ;
      */
   }

   if ( c4->tagFileMemory == 0 )
   {
      c4->tagFileMemory = mem4create( c4, c4->memStartTagFile, sizeof(TAG4FILE), c4->memExpandTagFile, 0 ) ;
      if ( c4->tagFileMemory == 0 )
         return 0 ;
   }

   tfile = (TAG4FILE *)mem4allocZero( c4->tagFileMemory ) ;
   if ( tfile == 0 )
      return 0 ;

   tfile->file.hand = INVALID4HANDLE ;
   tfile->codeBase = c4 ;
   tfile->userCount = 1 ;
   if ( tfile->blockMemory == 0 )
      tfile->blockMemory = mem4create( c4, c4->memStartBlock, (sizeof(B4BLOCK)) + B4BLOCK_SIZE_INTERNAL -
                                     (sizeof(B4KEY_DATA)) - (sizeof(short)) - (sizeof(char[2])),
                                     c4->memExpandBlock, 0 ) ;

   if ( tfile->blockMemory == 0 )
   {
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }

   // AS Apr 28/04 - use internal version
   rc = file4openInternal( &tfile->file, c4, buf, 1, OPT4INDEX ) ;
   if ( rc != 0 )
   {
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }

   #ifndef S4OPTIMIZE_OFF
      file4optimizeLow( &tfile->file, c4->optimize, OPT4INDEX, 0, tfile ) ;
   #endif

   file4longAssign( pos, 0, 0 ) ;
   file4seqReadInitDo( &seqRead, &tfile->file, pos, buffer, 1024, 1 ) ;
   #ifndef S4STRUCT_PAD
      if ( file4seqReadAll( &seqRead, &tfile->header.sign, sizeof(I4IND_HEAD_WRITE) ) < 0 )
   #else
      if ( file4seqReadAll( &seqRead, &tfile->header.sign, sizeof(I4IND_HEAD_WRITE)-2 ) < 0 )  /* Subtract 2 because sizeof is actually 22, not 24 */
   #endif
   {
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }
   #ifdef S4BYTE_SWAP
      tfile->header.sign = x4reverseShort( (void *)&tfile->header.sign ) ;
      tfile->header.version = x4reverseShort( (void *)&tfile->header.version ) ;
      tfile->header.root = x4reverseLong( (void *)&tfile->header.root ) ;
      tfile->header.eof = x4reverseLong( (void *)&tfile->header.eof ) ;
      tfile->header.groupLen = x4reverseShort( (void *)&tfile->header.groupLen ) ;
      tfile->header.keyLen = x4reverseShort( (void *)&tfile->header.keyLen ) ;
      tfile->header.keyDec = x4reverseShort( (void *)&tfile->header.keyDec ) ;
      tfile->header.keysMax = x4reverseShort( (void *)&tfile->header.keysMax ) ;
      tfile->header.keysHalf = x4reverseShort( (void *)&tfile->header.keysHalf ) ;
   #endif
   tfile->header.headerOffset = 0 ;

   /* Perform some checks */
   /* AS 5/12/99 - clipper version, if there is a filter, the sign is incremented to ...'7'.  This
      should be valid.  */
   if ( tfile->header.keyLen > I4MAX_KEY_SIZE || tfile->header.keyLen <= 0 ||
      tfile->header.keysMax != 2* tfile->header.keysHalf || tfile->header.keysHalf <= 0 ||
      tfile->header.groupLen != tfile->header.keyLen+ 8 ||
      (tfile->header.sign != 0x6 && tfile->header.sign != 0x106 && tfile->header.sign != 0x7 && tfile->header.sign != 0x107 )
       )
   {
      error4describe( c4, e4index, E84904, buf, 0, 0 ) ;
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }

   tfile->cmp = (S4CMP_FUNCTION *)u4memcmp ;
   tfile->header.root = INVALID4BLOCK_ID ;
   tfile->header.oldVersion = tfile->header.version ;

   u4namePiece( tfile->alias, sizeof( tfile->alias ), fileName, 0, 0 ) ;
   /* LY 2001/03/14 : if file name in .cgp is lowercase, alias is lowercase,
      and tag search fails in i4tag() against uppercase tag name */
   c4upper( tfile->alias ) ;

   file4seqReadAll( &seqRead, exprBuf, sizeof( exprBuf ) - 1 ) ;
   c4trimN( exprBuf, sizeof( exprBuf ) ) ;
   tfile->expr = expr4parseLow( d4, exprBuf, tfile ) ;
   if ( tfile->expr == 0 )
   {
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }
   tfile->expr->keyLen = tfile->header.keyLen ;
   tfile->expr->keyDec = tfile->header.keyDec ;

   if ( expr4context( tfile->expr, d4 ) < 0 )
   {
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }
   len = expr4keyLen( tfile->expr ) ;
   if ( len < 0 )
   {
      error4describe( c4, e4info, 84906, exprBuf, 0, 0 ) ;
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }

   /* AS 11/05/98 Added to read 1 extra byte, was reading off by 1 byte.  Caused problems if
      reindexing through code base occurred - also don't need to read garbage byte anymore */
   file4seqReadAll( &seqRead, &garbage, 1 ) ;
   file4seqReadAll( &seqRead, &tfile->header.unique, sizeof( tfile->header.unique ) ) ;
/*   file4seqReadAll( &seqRead, &garbage, sizeof( garbage ) ) ; */
   file4seqReadAll( &seqRead, &tfile->header.descending, sizeof( tfile->header.descending ) ) ;
   #ifdef S4BYTE_SWAP
      tfile->header.unique = x4reverseShort( (void *)&tfile->header.unique ) ;
      tfile->header.descending = x4reverseShort( (void *)&tfile->header.descending ) ;
   #endif
   file4seqReadAll( &seqRead, exprBuf, sizeof( exprBuf ) - 1 ) ;
   c4trimN( exprBuf, sizeof( exprBuf ) ) ;
   if ( exprBuf[0] != 0 )
   {
      tfile->filter = expr4parseLow( d4, exprBuf, tfile ) ;
      if ( tfile->filter != 0 )
      {
         if ( expr4context( tfile->filter, d4 ) < 0 )
         {
            tfile4close( tfile, d4->dataFile ) ;
            return 0 ;
         }
         len = expr4key( tfile->filter, &ptr, 0 ) ;
         if ( len < 0 )
         {
            tfile4close( tfile, d4->dataFile ) ;
            return 0 ;
         }
         if ( expr4type( tfile->filter ) != 'L' )
         {
            tfile4close( tfile, d4->dataFile ) ;
            return 0 ;
         }
      }
   }

/*   if( tfile->header.unique )
      tfile->uniqueError = c4->errDefaultUnique ;
*/
   l4add( &d4->dataFile->tagfiles, tfile ) ;   /* add the tag to its index list */

   tfile4initSeekConv( tfile, (char)expr4type( tfile->expr ) ) ;

   if ( tfile->blockMemory == 0 )
      tfile->blockMemory = mem4create( c4, c4->memStartBlock, (sizeof(B4BLOCK)) + B4BLOCK_SIZE_INTERNAL -
                           (sizeof(B4KEY_DATA)) - (sizeof(short)) - (sizeof(char[2])), c4->memExpandBlock, 0 ) ;

   if ( tfile->blockMemory == 0 )
   {
      #ifdef E4STACK
         error4stack( c4, e4memory, E94906 ) ;
      #endif
      tfile4close( tfile, d4->dataFile ) ;
      return 0 ;
   }

   if ( d4->dataFile->indexLocked == 1 )   /* index locked, so lock this tag as well */
      tfile4lock( tfile, data4serverId( d4 ) ) ;
   return tfile ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
TAG4 *S4FUNCTION t4openLow( DATA4 *d4, INDEX4 *i4ndx, const char *fileName, const char *dummy )
{
   CODE4 *c4 ;
   INDEX4 *i4 ;
   TAG4 *t4 ;
   int oldTagErr ;
//   char tagName[32];

   #ifdef E4VBASIC
      if ( c4parm_check( d4, 2, E94903 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( d4 == 0 || fileName == 0 )
      {
         error4( 0, e4parm_null, E94903 ) ;
         return 0 ; ;
      }
   #endif

   c4 = d4->codeBase ;
   if ( error4code( c4 ) < 0 )
      return 0 ;

   oldTagErr = c4->errTagName ;
   c4->errTagName = 0 ;
   /* LY 2001/07/11 : if opening Clipper indexes with same alias but different
      extensions, -69 error occurs
   u4namePiece(tagName,32,fileName,0,0) ;
   if ( d4tag( d4, tagName ) ) */
   // AS Jan 24/02 - This doesn't work if a path is included because the path is used instead of
   // the root name.  So instead grab the name and extension.
   char nameBuf[LEN4PATH+1] ;
   u4namePiece( nameBuf, LEN4PATH, fileName, 0, 1 ) ;
   if ( d4tag( d4, nameBuf ) )
   {
      error4describe( c4, e4instance, E85308, nameBuf, 0, 0 ) ;
      c4->errTagName = oldTagErr ;
      return 0 ;
   }
   c4->errTagName = oldTagErr ;
   error4set( c4, 0 ) ;

   if ( i4ndx == 0 )   /* must create an index for the tag */
   {
      if ( c4->indexMemory == 0 )
      {
         c4->indexMemory = mem4create( c4, c4->memStartIndex, sizeof( INDEX4 ), c4->memExpandIndex, 0 ) ;
         if ( c4->indexMemory == 0 )
            return 0 ;
      }

      i4 = (INDEX4 *)mem4allocZero( c4->indexMemory ) ;
      if ( i4 == 0 )
      {
         #ifdef E4STACK
            error4stack( c4, e4memory, E94903 ) ;
         #endif
         return 0 ;
      }

      i4->codeBase = c4 = d4->codeBase ;
      /* 09/22/95 AS - changed last parm to 1 from 0 for t4group (c/s access name needs extension if provided) */
      /* 05/21/97 AS - changed 2nd last parm to 1 from 0 -- (c/s access name needs path if provided) */
      u4namePiece( i4->accessName, sizeof( i4->accessName ), fileName, 1, 1 ) ;
    }
   else
      i4 = i4ndx ;

   if ( c4->tagMemory == 0 )
   {
      c4->tagMemory = mem4create( c4, c4->memStartTag, sizeof(TAG4), c4->memExpandTag, 0 ) ;
      if ( c4->tagMemory == 0 )
      {
         if ( i4ndx == 0 )
            mem4free( c4->indexMemory, i4 ) ;
         return 0 ;
      }
   }

   t4 = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
   if ( t4 == 0 )
   {
      #ifdef E4STACK
         error4stack( c4, e4memory, E94903 ) ;
      #endif
      if ( i4ndx == 0 )
         mem4free( c4->indexMemory, i4 ) ;
      return 0 ;
   }

   t4->tagFile = tfile4open( d4, fileName ) ;
   if ( t4->tagFile == 0 )
   {
      #ifdef E4STACK
         error4stack( c4, e4memory, E94903 ) ;
      #endif
      mem4free( c4->tagMemory, t4 ) ;
      if ( i4ndx == 0 )
         mem4free( c4->indexMemory, i4 ) ;
      return 0 ;
   }

   // AS Oct 12/04 - We need to track the index access name associated with the TAG4FILE in case the indexfile gets opened again.
   // in particular in client/server to keep the index associated with the correct structure in case it is opened again by another client.
   if ( i4ndx )   // LY Oct 22/04 : avoid access violation if i4ndx == 0
      strcpy( t4->tagFile->indexAccessName, i4ndx->accessName ) ;

   if ( t4->tagFile->header.unique )
      t4->errUnique = c4->errDefaultUnique ;
   t4->index = i4 ;
   if ( i4ndx == 0 )
   {
      i4->data = d4 ;
      l4add( &d4->indexes, i4 ) ;
   }

   l4add( &t4->index->tags, t4 ) ;
   #ifndef S4OFF_TRAN
      t4->isValid = 1 ;
   #endif
   #ifdef S4SERVER
      /* AS 04/05/01 - if the client requests the open after a create and it is temp, mark it as such */
      if ( c4->createTemp == 1 )  // means the client is requesting this open as temp part of i4create
         file4setTemporary( &t4->tagFile->file, 1, 0 ) ;
   #endif
   return t4 ;
}



#ifndef S4OFF_WRITE

#ifdef P4ARGS_USED
   #pragma argsused
#endif
/* S4CLIPPER, not S4OFF_WRITE, not S4CLIENT, not S4OFF_INDEX */
int tfile4shrink( TAG4FILE *t4, unsigned long blockNo )
{
   #ifdef S4SINGLE
      t4->header.eof = blockNo * 512 ;
   #endif
   // AS Nov 27/03 - with E4MISC, if this block is our checkEof block, the file is getting shrunk, so we need to modify.
   // was a problem with t4rel2.c - for check purposes only, so allow to make much smaller in the shrink case
   // AS Nov 27/03 - turns out to be invalid in multi-user since another user may be shrinking the file
   #if defined( E4MISC ) && defined( S4SINGLE )
      if ( t4->checkEof >= blockNo * 512 )
         t4->checkEof = (blockNo - 2) * 512 ;  // subtract 1024, which is the block size
   #endif
   return 0 ;
}



#ifdef S4BYTE_SWAP
static void tfile4swapHeaderForWrite( T4HEADER *header, I4IND_HEAD_WRITE *swappedWriteHeader, index4headerWrite desiredWrite )
{
   /*   If desiredWrite is updateOnly then only those fields header fields which can change will be
        swapped:

        i.e.: the update fields are:
           version, root, eof
   */
   /* LY 99/06/21 : made swappedWriteHeader and header pointers in param, updated same vars in code below*/
   swappedWriteHeader->version = x4reverseShort( (void *)&header->version ) ;
   swappedWriteHeader->root = x4reverseLong( (void *)&header->root ) ;
   swappedWriteHeader->eof = x4reverseLong( (void *)&header->eof ) ;

   if ( desiredWrite == writeEntireHeader )
   {
      swappedWriteHeader->sign = x4reverseShort( (void *)&header->sign ) ;
      swappedWriteHeader->groupLen = x4reverseShort( (void *)&header->groupLen ) ;
      swappedWriteHeader->keyLen = x4reverseShort( (void *)&header->keyLen ) ;
      swappedWriteHeader->keyDec = x4reverseShort( (void *)&header->keyDec ) ;
      swappedWriteHeader->keysMax = x4reverseShort( (void *)&header->keysMax ) ;
      swappedWriteHeader->keysHalf = x4reverseShort( (void *)&header->keysHalf ) ;
   }
}
#endif



#ifdef S4STRUCT_PAD
   #define I4IND_HEADER_WRITE_SIZE (sizeof(I4IND_HEAD_WRITE) - 2 )
#else
   #define I4IND_HEADER_WRITE_SIZE (sizeof(I4IND_HEAD_WRITE) )
#endif



/* S4CLIPPER, not S4OFF_WRITE, not S4CLIENT, not S4OFF_INDEX */
int tfile4writeHeader( TAG4FILE *t4, index4headerWrite desiredWrite )
{
   /*   If desiredWrite is updateOnly then only those fields header fields which can change will be
        written:

        i.e.: the update fields are:
           version, root, eof
   */
   char seqBuf[B4BLOCK_SIZE_INTERNAL] ;
   FILE4SEQ_WRITE seqwrite ;
   FILE4LONG writePos ;
   FILE4 *file = &t4->file ;
   int exprSourceLen, filterSourceLen ;
   const char *exprSourcePtr, *filterSourcePtr ;
   #ifdef S4BYTE_SWAP
      I4IND_HEAD_WRITE swap ;

      /* swap the various values which will be written*/
      short uniqueValueToWrite = x4reverseLong( (void *)&t4->header.unique ) ;
      short descendingValueToWrite = x4reverseLong( (void *)&t4->header.descending ) ;
      tfile4swapHeaderForWrite( &t4->header, &swap, desiredWrite ) ;

      char *headerWritePtr = (char *) &swap ;   /* LY 99/06/21 : explicit cast */
   #else
      char *headerWritePtr = (char *)(&t4->header.sign) ;
      short uniqueValueToWrite = t4->header.unique ;
      short descendingValueToWrite = t4->header.descending ;
   #endif

   /* Now write the header */

   /* basically updateOnly is a special case.  It is not placed in its own function in order
      to keep all the writes to the header of the tag all in one place for easier maintenance */
   if ( desiredWrite == updateOnly )
   {
      /* the update position starts at the version position in the file */
      file4longAssign( writePos, INDEX5VERSION_POS, 0 ) ;
      /* write out: version (short), root (long), and eof (long).  They are all together in the file */
      return file4writeInternal( file, writePos, headerWritePtr + INDEX5VERSION_POS, 2 * sizeof( S4LONG ) + sizeof( short ) ) ;
   }

   /* the header is at the INDEX5SIGN_POS area of the file - i.e. sign is 1st memory of header */
   // LY Mar 28/05 : changed for 64-bit FILE4LONG
   // file4seqWriteInitLow( &seqwrite, file, INDEX5SIGN_POS, seqBuf, sizeof( seqBuf ) ) ;
   FILE4LONG newPos ;
   file4longAssign( newPos, INDEX5SIGN_POS, 0 ) ;
   file4seqWriteInitLow( &seqwrite, file, newPos, seqBuf, sizeof( seqBuf ) ) ;

   file4seqWrite( &seqwrite, headerWritePtr, I4IND_HEADER_WRITE_SIZE ) ;

   /* write out the expression part */
   exprSourcePtr = t4->expr->source ;
   exprSourceLen = c4strlen( exprSourcePtr ) ;
   if ( exprSourceLen > I4MAX_EXPR_SIZE )
      return error4( t4->codeBase, e4index, E82106 ) ;

   file4seqWrite( &seqwrite, exprSourcePtr, exprSourceLen ) ;

   /* pad out the area after the expression to make always a consistent size (256 bytes) */
   /* in header, 256 byters allocated for expression, though max expr size is 255 (I4MAX_EXPR_SIZE 255)
      so for filler we need to write out 256 - len (OR I4MAX_EXPR_SIZE+1 - LEN ) */
   file4seqWriteRepeat( &seqwrite, I4MAX_EXPR_SIZE - exprSourceLen + 1, 0 ) ;

   /* write out the unique and descending */
   file4seqWrite( &seqwrite, &uniqueValueToWrite, sizeof( uniqueValueToWrite ) ) ;
//   file4seqWriteRepeat( &r4->seqwrite, 1, (char)0 ) ;
   file4seqWrite( &seqwrite, &descendingValueToWrite, sizeof( descendingValueToWrite ) ) ;

   /* note that the filter comes at the end, so we don't need to do any padding -- at
      the end of the function we pad everything out to meet the required file length
   */
   if ( t4->filter != 0 )
   {
      /* write out the filter part only if there is a filter */

      filterSourcePtr = t4->filter->source ;
      filterSourceLen = c4strlen( filterSourcePtr ) ;
      if ( filterSourceLen > I4MAX_EXPR_SIZE )
         return error4( t4->codeBase, e4index, E82106 ) ;

      file4seqWrite( &seqwrite, filterSourcePtr, filterSourceLen ) ;
   }

   #ifdef E4ANALYZE
      /* there should have been enough room in the seq buffer to hold B4BLOCK_SIZE_INTERNAL - i.e. the
         entire header.  Make sure of this
      */
      if ( B4BLOCK_SIZE_INTERNAL - (seqwrite.working - seqwrite.avail ) < 0 )
         return error4( t4->codeBase, e4index, E82103 ) ;
   #endif

   /* pad out the header to the complete size of a header block */
   // AS Jun 5/03 - If we have gone to the next seqwrite block (512), then we were overwriting here
   // because we weren't taking the seqwrite.pos into account.
   // file4seqWriteRepeat( &seqwrite, B4BLOCK_SIZE_INTERNAL - (seqwrite.working - seqwrite.avail) , 0 ) ;
   // LY Mar 28/05 : changed for 64-bit FILE4LONG
   // file4seqWriteRepeat( &seqwrite, B4BLOCK_SIZE_INTERNAL - seqwrite.pos - (seqwrite.working - seqwrite.avail) , 0 ) ;
   file4longAssign( newPos, B4BLOCK_SIZE_INTERNAL, 0 ) ;
   file4longSubtractLong( &newPos, &seqwrite.pos ) ;
   file4longSubtract( &newPos, seqwrite.working - seqwrite.avail ) ;
   file4seqWriteRepeat( &seqwrite, file4longGetLo( newPos ), 0 ) ;

   if ( file4seqWriteFlush( &seqwrite ) < 0 )
      return -1 ;

   return 0 ;
}


/* S4CLIPPER, not S4OFF_WRITE, not S4CLIENT, not S4OFF_INDEX */
int tfile4updateHeader( TAG4FILE *t4 )
{
   T4HEADER *header = &t4->header ;

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   if ( header->oldVersion != header->version )
   {
      if ( tfile4writeHeader( t4, updateOnly ) < 0 )
         return -1 ;
      header->oldVersion = header->version ;
   }

   return 0;
}
#endif  /* S4OFF_WRITE */



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
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

   tfile4block(t4)->keyOn++ ;

   return 0 ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int tfile4doVersionCheck( TAG4FILE *t4, int doSeek, int updateVersion )
{
   #ifndef S4SINGLE
      int rc, needSeek ;
      B4BLOCK *b4 ;
      CODE4 *c4 ;
      FILE4LONG fLong ;

      #ifdef E4PARM_LOW
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E91635 ) ;
      #endif

      c4 = t4->codeBase ;

      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( tfile4lockTest( t4 ) == 1 )
         return 0 ;

      #ifndef S4OPTIMIZE_OFF
         /* make sure read from disk */
         if ( t4->file.doBuffer )
            c4->opt.forceCurrent = 1 ;
      #endif

      file4longAssign( fLong, 0, 0 ) ;
      rc = file4readAllInternal( &t4->file, fLong, &t4->header.sign , 2 * sizeof( short ) + 2 * sizeof(S4LONG ) ) ;
      #ifndef S4OPTIMIZE_OFF
         if ( t4->file.doBuffer )
            c4->opt.forceCurrent = 0 ;
      #endif
      if ( rc < 0 )
         return rc ;
      #ifdef S4BYTE_SWAP
         t4->header.sign = x4reverseShort( (void *)&t4->header.sign ) ;
         t4->header.version = x4reverseShort( (void *)&t4->header.version ) ;
         t4->header.root = x4reverseLong( (void *)&t4->header.root ) ;
         t4->header.eof = x4reverseLong( (void *)&t4->header.eof ) ;
         /* t4->header.groupLen = x4reverseShort( (void *)&t4->header.groupLen ) ;  */
         /* t4->header.keyLen = x4reverseShort( (void *)&t4->header.keyLen ) ;      */
         /* t4->header.keyDec = x4reverseShort( (void *)&t4->header.keyDec ) ;      */
         /* t4->header.keysMax = x4reverseShort( (void *)&t4->header.keysMax ) ;    */
         /* t4->header.keysHalf = x4reverseShort( (void *)&t4->header.keysHalf ) ;  */
      #endif

      if ( t4->header.version == t4->header.oldVersion )
         return 0 ;

      if ( updateVersion == 1 )
         t4->header.oldVersion = t4->header.version ;
      else
         t4->header.version = t4->header.oldVersion ;

      /* remember the old position */
      needSeek = 0 ;
      if ( doSeek )
      {
         b4 = (B4BLOCK *)t4->blocks.lastNode ;  /* can't use tfile4block( t4 ) since might be null */
         if ( b4 != 0 )
         {
            if ( tfile4eof( t4 ) )
               needSeek = 2 ;
            else
/*               if ( b4leaf( b4 ) && b4->nKeys != 0 )
                 changed line 04/09/96 AS --> if on a branch, and no seek is performed, a gpf or general
                 error can later occur (in d4seek) --> this should be ok for S4CLIPPER */
               if ( b4->nKeys != 0 )
               {
                  // LY Oct 15/04 : t4data2.c crashing due to c4->savedKey == 0
                  if ( t4->header.keyLen + 2 * sizeof( S4LONG ) > c4->savedKeyLen )
                  {
                     if ( u4allocAgain( c4, &c4->savedKey, &c4->savedKeyLen, t4->header.keyLen + 2 * sizeof( S4LONG )  ) < 0 )
                        return e4memory ;
                     c4->savedKeyLen = t4->header.keyLen + 2 * sizeof( S4LONG ) ;
                  }
                  memcpy( c4->savedKey, b4key( b4, b4->keyOn ), t4->header.keyLen + 2 * sizeof(S4LONG ) ) ;
                  needSeek = 1 ;
               }
         }
      }

      if ( tfile4freeAll( t4 ) < 0 )  /* Should be a memory operation only */
         #ifdef E4ANALYZE
            return error4( c4, e4result, E91635 ) ;
         #else
            return e4result ;
         #endif

      switch( needSeek )
      {
         case 1:
            #ifdef E4ANALYZE_ALL
               if ( tfile4go( t4, ((B4KEY_DATA *)c4->savedKey)->value, ((B4KEY_DATA *)c4->savedKey)->num, 0 ) != 0 )
                  return error4( c4, e4index, E91635 ) ;
            #else
               tfile4go( t4, ((B4KEY_DATA *)c4->savedKey)->value, ((B4KEY_DATA *)c4->savedKey)->num, 0 ) ;
            #endif
            break ;
         case 2:
            tfile4goEof( t4 ) ;
            break ;
      }

   #endif
   return 0;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int S4FUNCTION i4versionCheck( INDEX4 *index, const int d1, const int d2 )
{
   return error4( index->codeBase, e4notSupported, E94903 ) ;
}



/* S4CLIPPER, not S4CLIENT, not S4OFF_INDEX */
int S4FUNCTION tfile4versionCheck( TAG4FILE *t4, const int doSeek, const int updateVersion )
{
   /*   AS 02/02/99 --> even if bufferred, in this case we must do a version check because we
        want to get the latest -- and we have already dumped the buffers from memory anyway,
        so do a version check no matter what if not locked...
      #ifndef S4OPTIMIZE_OFF
        if ( t4->file.doBuffer == 0 )
      #endif
   */
   #ifndef S4SINGLE
      if ( tfile4lockTest( t4 ) != 1 )
   #endif
      return tfile4doVersionCheck( t4, doSeek, updateVersion ) ;
   return 0 ;
}


#else  /* S4CLIPPER */

int S4FUNCTION t4close( TAG4 *t4 )
{
   return error4( 0, e4notSupported, E81719 ) ;
}

#endif  /* S4CLIPPER */
#endif  /* S4OFF_INDEX */



#ifdef S4VB_DOS
#ifdef S4CLIPPER

/* S4VB_DOS, S4CLIPPER, not S4CLIENT */
TAG4 *S4FUNCTION tfile4open_v(DATA4 *d4, char *name)
{
   return tfile4open( d4, 0, c4str(name) ) ;
}
#endif  /* S4CLIPPER */
#endif  /* S4VB_DOS */
#endif  /* S4CLIENT */
