/* i4check.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef I4PRINT
   #include <sys\timeb.h>
   #include <time.h>
#endif

#ifdef S4CLIENT
int S4FUNCTION d4check( DATA4 *d4 )
{
   #ifdef S4OFF_INDEX
      return 0 ;
   #else
      CONNECTION4 *connection ;
      int rc ;
      CONNECTION4CHECK_INFO_OUT *out ;

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E95702 ) ;
      #endif

      #ifndef S4OFF_MULTI
         rc = d4lockFile( d4 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc )
            return rc ;
      #endif

      // Apr 25/02 - ensure batched writes get flushed first
      code4writeBufferFlush( d4->codeBase ) ;
      if ( error4code( d4->codeBase ) < 0 )  // check if write buffer flush returned an error
         return error4code( d4->codeBase ) ;

      connection = d4->dataFile->connection ;
      if ( connection == 0 )
         return e4connection ;
      connection4assign( connection, CON4CHECK, data4clientId( d4 ), data4serverId( d4 ) ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return rc ;
      rc = connection4status( connection ) ;
      if ( rc != 0 )
         return connection4error( connection, d4->codeBase, rc, E95702 ) ;

      if ( connection4len( connection ) != sizeof( CONNECTION4CHECK_INFO_OUT ) )
         return error4( d4->codeBase, e4packetLen, E95702 ) ;
      out = (CONNECTION4CHECK_INFO_OUT *)connection4data( connection ) ;
      if ( out->lockedDatafile == 1 )
         d4->dataFile->fileLock = d4 ;
      return 0 ;
   #endif  /* S4OFF_INDEX */
}
#else

#ifndef S4OFF_INDEX

typedef struct
{
   F4FLAG flag ;

   TAG4FILE *tag ;
   char *oldKey ;
   unsigned long oldRec ;
   unsigned long numRecs ;
   int doCompare ;  /* Do not compare the first time */
   CODE4 *codeBase ;
   DATA4 *data ;
} C4CHECK ;

static int c4checkInit( C4CHECK *check, CODE4 *cb, TAG4FILE *t4, long nRecs, DATA4 *d4 )
{
   int rc ;

   c4memset( (void *)check, 0, sizeof(C4CHECK) ) ;

   rc = f4flagInit( &check->flag, cb, (unsigned long)nRecs, 0 ) ;
   if ( rc < 0 )
      return rc ;

   check->codeBase = cb ;
   check->tag = t4 ;
   check->numRecs = nRecs ;
   check->data = d4 ;

   check->oldKey = (char *)u4allocFree( t4->codeBase, (long)t4->header.keyLen ) ;
   if (check->oldKey == 0)
      return e4memory ;
   return 0 ;
}



static void c4checkFree( C4CHECK *c4 )
{
   u4free( c4->flag.flags ) ;
   u4free( c4->oldKey ) ;
}



static int c4checkRecord( C4CHECK *check )
{
   TAG4FILE *t4 = check->tag ;
   CODE4 *c4 = check->codeBase ;
   #ifdef S4FOX
      int i ;
   #endif
   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4group.c on HP */
      S4UNSIGNED_LONG tempLong ;  /* LY 2001/07/20 : changed from unsigned long for 64-bit */
   #endif

   B4KEY_DATA *keyData = tfile4keyData( t4 ) ;
   if ( keyData == 0 )
      return error4( c4, e4index, E95701 ) ;

   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4group.c on HP */
      memcpy( &tempLong, &keyData->num, sizeof(S4LONG) ) ;
      if ( tempLong == 0 || tempLong == INVALID4BLOCK_ID  ||  tempLong > check->numRecs )
   #else
      if ( keyData->num == 0 || keyData->num == INVALID4BLOCK_ID  ||  keyData->num > check->numRecs )
   #endif
      return error4describe( c4, e4index, E85703, check->tag->alias, (char *)0, (char *)0 ) ;

   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4group.c on HP */
      if ( f4flagIsSet( &check->flag, (unsigned long)tempLong) )
   #else
      if ( f4flagIsSet( &check->flag, keyData->num) )
   #endif
      return error4describe( c4, e4index, E85703, check->tag->alias, (char *)0, (char *)0 ) ;
   else
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4group.c on HP */
         f4flagSet( &check->flag, (unsigned long)tempLong ) ;
      #else
         f4flagSet( &check->flag, keyData->num ) ;
      #endif

   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4group.c on HP */
      int rc = d4go( check->data, tempLong ) ;
   #else
      int rc = d4go( check->data, keyData->num ) ;
   #endif
   if ( rc < 0 )
      return rc ;
   rc = expr4context( t4->expr, check->data ) ;
   if ( rc < 0 )
      return rc ;

   unsigned char *newPtr ;
   int len = tfile4exprKey( t4, &newPtr ) ;

   if ( len != t4->header.keyLen )
      return error4describe( c4, e4index, E85704, t4->alias, (char *)0, (char *)0 ) ;

   if ( t4->filter )
   {
      rc = expr4context( t4->filter, check->data ) ;
      if ( rc < 0 )
         return rc ;
      if ( !expr4true( t4->filter ) )  /* means record should not be in the index, but it is... */
         return error4describe( c4, e4index, E85705, t4->alias, (char *)0, (char *)0 ) ;
   }

   #ifdef S4MDX
      if ( expr4type( t4->expr ) == r4num )
      {
         if ( c4bcdCmp( newPtr, keyData->value, 0 ) != 0 )
            return error4describe( c4, e4index, E85705, t4->alias, (char *)0, (char *)0 ) ;
      }
      else
   #endif
   if ( c4memcmp( newPtr, keyData->value, (unsigned int)t4->header.keyLen ) != 0 )
   {
      #ifdef I4PRINT
         struct _timeb mTime ;
         char *outTime, dump[120] ;
         B4BLOCK *block = (B4BLOCK *)t4->blocks.lastNode ;
         _ftime( &mTime ) ;
         outTime = ctime( &( mTime.time ) ) ;

         sprintf( dump, "id = %ld, mismatch: rec = %uld, fb = %uld, dbf key = %20s, idx key = %20s", (long)t4->indexFile, keyData->num,
                  block->fileBlock, (char *)newPtr, (char *)keyData->value ) ;
         u4writeErr( dump, 1 ) ;
         u4writeErr( outTime, 0 ) ;
      #endif
      return error4describe( c4, e4index, E85705, t4->alias, (char *)0, (char *)0  ) ;
   }

   #ifdef S4FOX
      /* blanks at the end of a key MUST be recorded as trailing blanks, and
         not included in the key.  Some early versions of CodeBase erroneously
         put blanks as part of the key and not as trails.  Catch that here
         because it causes problems with FoxPro and CodeBase */
      /* There is an exception in one version of FoxPro (2.6) which strangely
         included trail blanks in some instances */
      for ( i = t4->header.keyLen ; i > 0 ; i-- )
      {
         if ( keyData->value[i-1] != t4->pChar )
            break ;
      }

      assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
      if ( check->data->dataFile->compatibility == 26 && t4->filter != 0 )
      {
         if ( x4trailCnt( tfile4block( t4 ), tfile4block( t4 )->keyOn ) != 0 )
            error4describe( c4, e4index, E85712, t4->alias, (char *)0, (char *)0 ) ;
      }
      else
      {
         if ( x4trailCnt( tfile4block( t4 ), tfile4block( t4 )->keyOn ) != (t4->header.keyLen - i ) )
         {
            // AS 03/14/00 there is one case where this does not hold.  Namely, if we have
            // keys as:  0x0001201f, 0x00012020, 0x00012021.  For key 2 in actual fact
            // we need a duplicate of '7' and a trail of '1' in order for comparisons
            // to work proper...  i.e. this is a case where trail + dup == key len...
            if ( x4trailCnt( tfile4block( t4 ), tfile4block( t4 )->keyOn ) + x4dupCnt( tfile4block( t4 ), tfile4block( t4 )->keyOn ) != t4->header.keyLen )
               error4describe( c4, e4index, E85712, t4->alias, (char *)0, (char *)0 ) ;
         }
      }
   #endif

   if ( check->doCompare )
   {
      #ifdef S4FOX
         #ifdef S4VFP_KEY
            if ( tfile4type( t4 ) != r4str )
               rc = c4memcmp( check->oldKey, newPtr, (unsigned int)t4->header.keyLen ) ;
            else
         #endif
            {
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // rc = u4keycmp( check->oldKey, newPtr, (unsigned int)t4->header.keyLen, (unsigned int)t4->header.keyLen, 0, &t4->vfpInfo ) ;
               rc = u4keycmp( check->oldKey, newPtr, (unsigned int)t4->header.keyLen, (unsigned int)t4->header.keyLen, 0, collation4get( t4->collateName ) ) ;
               /* AS 06/22/99 - not necessarily invalid.  for character keys the order is
                  0x20,0x01,...0x19,0x21,..0xff.  So if rc > 0, check whether really is just
                  a blank problem...
                  AS 05/05/00 -- only applies to non-machine collation...
               */
               if ( t4->collateName != collate4none && t4->collateName != collate4machine )
               {
                  if ( rc > 0  && t4->pChar == 0x20 )
                  {
                     for ( i = 0 ; i < t4->header.keyLen ; i++ )
                     {
                        if ( check->oldKey[i] != newPtr[i] )
                        {
                           if ( check->oldKey[i] < newPtr[i] )  // we are done, and result is ok...
                           {
                              rc = -1 ;
                              break ;
                           }
                           /* if oldKey is 0x20 and newPtr is < 0x20, this is ok... */
                           if ( check->oldKey[i] == 0x20 )  // newPtr is < this, just skip this byte...
                              continue ;
                           // otherwise we got here and we really do have a mismatch because oldKey != 0x20
                           rc = 1 ;
                           break ;
                        }
                     }
                  }
               }
            }
      #else
         rc = (*t4->cmp)( check->oldKey, newPtr, (unsigned int)t4->header.keyLen ) ;
      #endif

      if ( rc > 0)
      {
         error4describe( c4, e4index, E85706, t4->alias, (char *)0, (char *)0 ) ;
      }
      #ifdef S4FOX
         if ( rc == 0  &&  keyData->num <= check->oldRec )
            error4describe( c4, e4index, E85707, t4->alias, (char *)0, (char *)0 ) ;
      #endif /* S4FOX */

      #ifdef S4FOX
         if ( t4->header.typeCode & 0x01 )
      #else
         if ( t4->header.unique )
      #endif /* S4FOX */
      if ( rc == 0 )
         error4describe( c4, e4index, E85708, t4->alias, (char *)0, (char *)0 ) ;
   }
   else
      check->doCompare = 1 ;

   c4memcpy( check->oldKey, newPtr, (unsigned int)t4->header.keyLen ) ;

   #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4group.c on HP */
      memcpy( &tempLong, &keyData->num, sizeof(S4LONG) ) ;
      check->oldRec = tempLong ;
   #else
      check->oldRec = keyData->num ;
   #endif

   if ( error4code( c4 ) < 0 )
      return error4code( c4 )  ;
   return 0 ;
}

#ifdef S4CLIPPER
static int tfile4blockCheck( TAG4FILE *t4, int firstTime )
{
   B4BLOCK *b4 ;
   int i, bType, rc ;
   CODE4 *c4 ;

   if ( firstTime )
      tfile4upToRoot( t4 ) ;

   c4 = t4->codeBase ;
   b4 = (B4BLOCK *)t4->blocks.lastNode ;
   if ( b4 == 0 )
      return 0 ;
   if ( b4->nKeys < t4->header.keysHalf && t4->header.root / 512 != b4->fileBlock )
      return error4describe( c4, e4index, E85709, tfile4alias( t4 ), (char *)0, (char *)0 ) ;
   if ( !b4leaf( b4 ) )
   {
      for ( i = 0 ; i <= b4->nKeys ; i++ )
      {
         b4->keyOn = i ;
         rc = tfile4down( t4 ) ;
         if ( rc != 0 )
            return error4describe( c4, e4index, E81601, tfile4alias( t4 ), 0, 0 ) ;
         if ( i == 0 )
            bType = b4leaf( (B4BLOCK *)t4->blocks.lastNode ) ;
         else
            if ( bType != b4leaf( (B4BLOCK *)t4->blocks.lastNode ) )
               return error4describe( c4, e4index, E85709, tfile4alias( t4 ), "index tag is not properly balanced-leaf and branch nodes both detected as children of same block", (char *)0 ) ;
         rc = tfile4blockCheck( t4, 0 ) ;
         if ( rc != 0 )
            return rc ;
         rc = tfile4up( t4 ) ;
         if ( rc != 0 )
            return error4describe( c4, e4index, E81601, tfile4alias( t4 ), (char *)0, (char *)0 ) ;
      }
   }
   return 0 ;
}
#endif

int t4check( TAG4 *t4 )
{
   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E95703 ) ;
   #endif

   DATA4 *d4 = t4->index->data ;
   CODE4 *c4 = d4->codeBase ;

   int rc = expr4context( t4->tagFile->expr, d4 ) ;
   if ( rc < 0 )
      return rc ;

   if ( t4->tagFile->filter != 0 )
   {
      rc = expr4context( t4->tagFile->filter, d4 ) ;
      if ( rc < 0 )
         return rc ;
   }

   #ifndef S4OFF_MULTI
      rc = d4lockFileInternal( d4, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc != 0 )
         return rc ;
      #ifdef S4CLIPPER
         rc = i4lock( t4->index ) ;
      #else
         rc = index4lock( t4->tagFile->indexFile, data4serverId( d4 ) ) ;
      #endif
      if ( rc != 0 )
         return rc ;
      rc = d4refresh( d4 ) ;
      if ( rc != 0 )
         return rc ;
   #endif

   #ifndef S4OFF_WRITE
      rc = d4updateRecord( d4, 0, 1 ) ;
      if ( rc < 0 )
         return rc ;
      if ( rc )
         return error4( c4, rc, E95703 ) ;
   #endif

   TAG4 *oldSelectedTag = d4tagSelected( d4 ) ;
   d4tagSelect( d4, t4 ) ;

   #ifdef S4CLIPPER
      rc = tfile4blockCheck( t4->tagFile, 1 ) ;
      if ( rc != 0 )
         return rc ;
   #endif

   long baseSize = d4recCount( d4 ) ;
   if ( baseSize < 0L )
      return (int)baseSize ;

   rc = d4top( d4 ) ;
   if (rc < 0 )
      return rc ;
   if ( rc == 0 )
      rc = 1 ;

   if ( baseSize == 0L )
   {
      if ( tfile4skip( t4->tagFile, 1L ) == 0 )
      {
         d4tagSelect( d4, oldSelectedTag ) ;
         return( 0 ) ;
      }
      else
         return error4describe( c4, e4index, E85710, d4alias( d4 ), tfile4alias( t4->tagFile ), (char *)0 ) ;
   }

   C4CHECK check ;
   int rc2 = c4checkInit( &check, c4, t4->tagFile, baseSize, d4 ) ;
   if ( rc2 < 0 )
      return rc2 ;

   unsigned long loop = 0 ;
   #ifdef S4PRINTF_OUT
      printf( "On Rec %10ld\n", loop ) ;
   #endif

   while ( rc == 1 )
   {
      rc = c4checkRecord( &check ) ;
      if ( rc )
         break ;
       rc = (int)tfile4skip( t4->tagFile, 1L ) ;
      if ( rc < 0 )
         break ;
      loop++ ;
      #ifdef S4PRINTF_OUT
         if ( (loop % 100) == 0 )
            printf( "\b\b\b\b\b\b\b\b\b\b%10ld", loop ) ;
      #endif
   }

   if ( rc < 0 )
   {
      c4checkFree( &check ) ;
      return rc ;
   }

   int isRecord = 1 ;

   /* Now Test for Duplication */
   for ( long onRec = 1;  onRec <= baseSize; onRec++)
   {
      if ( t4->tagFile->filter != 0 )
      {
         if ( d4go( d4, onRec ) < 0 )
            break ;
         rc2 = expr4context( t4->tagFile->filter, check.data ) ;
         if ( rc2 < 0 )
            return rc2 ;

         isRecord = expr4true( t4->tagFile->filter ) ;
      }

      if ( f4flagIsSet( &check.flag, (unsigned long)onRec ) )
      {
         if ( !isRecord )
         {
            error4describe( c4, e4index, E95703, t4->tagFile->alias, (char *)0, (char *)0 ) ;
            break ;
         }
      }
      else
      {
         if ( ! isRecord )
            continue ;

         #ifdef S4FOX
            if ( t4->tagFile->header.typeCode & 0x01 )
         #else
            if ( t4->tagFile->header.unique )
         #endif
            {
               if ( d4go(d4,onRec) < 0 )
                  break ;
               if ( expr4context( t4->tagFile->expr, check.data ) < 0 )
                  break ;
               unsigned char *ptr ;
               if ( tfile4exprKey( t4->tagFile, &ptr) < 0 )
                  break ;
               if ( tfile4seek( t4->tagFile, ptr, expr4keyLen( t4->tagFile->expr ) ) == 0 )
                  continue ;
            }

         error4describe( c4, e4index, E85711, t4->tagFile->alias, 0, 0 ) ;
         break ;
      }
   }

   c4checkFree( &check ) ;
   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;

   /* Now make sure the block key pointers match the blocks they point to. */
   /* This needs to be true for d4seek to function perfectly. */

   rc = d4bottom( d4 ) ;
   if ( rc < 0 )
      return rc ;

   if ( rc == 3 )
   {
      d4tagSelect( d4, oldSelectedTag ) ;
      return 0 ;
   }

   for(;;)
   {
      #ifdef S4FOX
         int keysSkip = -tfile4block(t4->tagFile)->header.nKeys ;
      #else
         int keysSkip = -tfile4block(t4->tagFile)->nKeys ;
      #endif

      rc = (int)tfile4skip( t4->tagFile, (long) keysSkip ) ;
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;
      if ( rc != keysSkip )
      {
         d4tagSelect( d4, oldSelectedTag ) ;
         return 0 ;
      }

      B4BLOCK *blockOn = (B4BLOCK *)t4->tagFile->blocks.lastNode ;
      if ( blockOn == 0 )
         return error4describe( c4, e4index, E85712, tfile4alias( t4->tagFile ), (char *)0, (char *)0 ) ;

      #ifdef S4FOX
         unsigned char *tempVal = (unsigned char *)u4allocFree( c4, (long)t4->tagFile->header.keyLen ) ;
         if ( tempVal == 0 )
            return error4stack( c4, e4memory, E95703 ) ;
         c4memcpy( tempVal, (void *)b4keyKey( blockOn, blockOn->keyOn ), (unsigned int)t4->tagFile->header.keyLen ) ;
         unsigned long tempLong = b4recNo( blockOn, blockOn->keyOn ) ;

         if ( tfile4go( t4->tagFile, tempVal, tempLong, 0 ) )
         {
            u4free( tempVal ) ;
            return error4describe( c4, e4index, E85712, tfile4alias( t4->tagFile ), (char *)0, (char *)0 ) ;
         }
         u4free( tempVal ) ;
      #endif

      #ifndef S4CLIPPER
         for ( ;; )
         {
            blockOn = (B4BLOCK *)blockOn->link.p ;
            if ( blockOn == 0 )
               break ;
            if ( blockOn == (B4BLOCK *)t4->tagFile->blocks.lastNode )
               break ;

            #ifdef S4FOX
               if ( blockOn->keyOn < blockOn->header.nKeys )
            #else
               if ( blockOn->keyOn < blockOn->nKeys )
            #endif
               {
                  B4KEY_DATA *keyBranch = b4key( blockOn, blockOn->keyOn ) ;
                  B4KEY_DATA *keyLeaf = b4key( tfile4block( t4->tagFile ), tfile4block( t4->tagFile )->keyOn ) ;

                  if ( c4memcmp( keyBranch->value, keyLeaf->value, (unsigned int)t4->tagFile->header.keyLen) != 0 )
                     return error4describe( c4, e4index, E85712, tfile4alias( t4->tagFile ), (char *)0, (char *)0 ) ;

                  break ;
               }
         }
         if ( blockOn == 0 )
            return error4describe( c4, e4index, E85712, tfile4alias( t4->tagFile ), (char *)0, (char *)0 ) ;
      #endif
   }
}

#ifdef S4FOX
static unsigned long getFlagFromNode( INDEX4FILE *indexFile, B4NODE node )
{
   // we need to get a unique ordered value to have an uninterupted flag sequence.
   // we need to take into account:  the block multiplier (what to multiply block size by to
   //   get physical file offset), and block size (indicates how much file offset seperation
   //   between flags)

   FILE4LONG filePos ;
   b4nodeGetFilePosition( indexFile, node, &filePos ) ;
   // AS actually, we use the 512 size as flag size, means for large blocks must set multiple times.
   // need to do it this way because index headers are always 1024 bytes not matter what...
   file4longDivide( filePos, B4BLOCK_SIZE_INTERNAL ) ;
   assert5( file4longGetHi( filePos ) == 0 ) ;  // should be impossible or file is too big for nodes!
   return file4longGetLo( filePos ) ;
}




static int flag4blocks( TAG4FILE *t4, F4FLAG *f4, B4NODE *node1, B4NODE *node2, B4NODE *node3 )
{
   int i, rc ;
   B4BLOCK *blockOn ;

   rc = tfile4down( t4 ) ;
   if ( rc < 0 )
      return rc ;
   if ( rc == 2 )
      return e4index ;
   if ( rc == 1 )
      return error4( t4->codeBase, e4index, E95704 ) ;

   blockOn = tfile4block(t4) ;

   unsigned long flagNo = getFlagFromNode( t4->indexFile, blockOn->fileBlock ) ;

   /* do verification on nodes - node2 should == our file block,
                                 node1 should == our left node
                                 node3 should == our right node
   */

   if ( !b4nodeUnknown( *node2 ) )
      if ( b4nodesNotEqual( *node2, blockOn->fileBlock ) )
         return error4( t4->codeBase, e4index, E81601 ) ;

   if ( !b4nodeUnknown( *node1 ) )
      if ( b4nodesNotEqual( *node1, blockOn->header.leftNode ) )
         return error4( t4->codeBase, e4index, E81601 ) ;

   if ( !b4nodeUnknown( *node3 ) )
      if ( b4nodesNotEqual( *node3, blockOn->header.rightNode ) )
         return error4( t4->codeBase, e4index, E81601 ) ;

   #ifdef S4FOX
      // each flag size is 512 bytes, for blocks larger than this, set multiplie flags...
      long numFlagsInBlock = i4blockSize( t4->indexFile ) / B4BLOCK_SIZE_INTERNAL ;
      for ( long setIndex = 0 ; setIndex < numFlagsInBlock ; setIndex++ )
      {
         if ( f4flagIsSet( f4, flagNo + setIndex  ) )
            return error4( t4->codeBase, e4index, E81601 ) ;

         rc = f4flagSet( f4, flagNo + setIndex ) ;
         if ( rc < 0 )
            return rc ;
      }
   #else
      if ( f4flagIsSet( f4, flagNo ) )
         return error4( t4->codeBase, e4index, E81601 ) ;

      rc = f4flagSet( f4, flagNo ) ;
      if ( rc < 0 )
         return rc ;
   #endif

   if ( !b4leaf( blockOn ) )
   {
      if ( b4nodeInvalid( blockOn->header.leftNode ) )
      {
         // we are the left-most block on our level, and since we check from left to right,
         // our leftmost child must not have any left block nodes either.
         b4nodeSetInvalid( node1 ) ;
      }
      else
      {
         // we are not left-most block, and we don't know what our childs left neighbour should
         // be, so indicate this by using -2 (unknown)
         b4nodeSetUnknown( node1 ) ;
      }

      // our child's node is equal to our first entries node...
      b4nodeAssignLong( node2, b4key( blockOn, 0 )->num ) ;

      for( i = 0 ; i < blockOn->header.nKeys ; i++ )
      {
         b4go( blockOn, i ) ;

         if ( i == blockOn->header.nKeys - 1 && b4nodeInvalid( blockOn->header.rightNode ) )
         {
            // we are rightmost node, so our rightmost child must have no right neighbour blocks
            b4nodeSetInvalid( node3 ) ;
         }
         else
         {
            // we are not right-most block, and we don't know what our right-most childs
            // node should be, so mark as unknown...
            b4nodeSetUnknown( node3 ) ;
         }

         rc = flag4blocks( t4, f4, node1, node2, node3 ) ;
         if ( rc < 0 )
            return rc ;
      }
   }

   // go to next block -- means node1 (left) becomes our current node
   // and the current node becomes our 'right' node
   b4nodeAssignNode( node1, blockOn->fileBlock ) ;
   b4nodeAssignNode( node2, blockOn->header.rightNode ) ;
   tfile4up( t4 ) ;
   return 0 ;
}
#else



static int flag4blocks( TAG4FILE *t4, F4FLAG *f4 )
{
   int i, rc ;
   B4BLOCK *blockOn ;

   rc = tfile4down( t4 ) ;
   if ( rc < 0 )
      return rc ;
   if ( rc == 2 )
      return -1 ;
   if ( rc == 1 )
      return error4( t4->codeBase, e4index, E95704 ) ;

   blockOn = tfile4block(t4) ;

   #ifdef S4CLIPPER
      unsigned long flagNo = (blockOn->fileBlock) * I4MULTIPLY / B4BLOCK_SIZE_INTERNAL ;
   #else
      // unsigned long flagNo = (blockOn->fileBlock-4) * I4MULTIPLY / t4->indexFile->header.blockRw ;
      // AS Nov 19/03 - large file support
      // unsigned long flagNo = (blockOn->fileBlock-4) * I4MULTIPLY / i4blockSize( t4->indexFile ) ;
      FILE4LONG flagNoLong ;
      file4longAssign( flagNoLong, (blockOn->fileBlock-4), 0 ) ;
      file4longMultiply( flagNoLong, I4MULTIPLY ) ;
      file4longDivide( flagNoLong, i4blockSize( t4->indexFile ) ) ;
      assert5( file4longGetHi( flagNoLong ) == 0 ) ;
      unsigned long flagNo = file4longGetLo( flagNoLong ) ;
   #endif

   if ( f4flagIsSet( f4, flagNo ) )
      return error4( t4->codeBase, e4index, E81601 ) ;

   rc = f4flagSet( f4, flagNo ) ;
   if ( rc < 0 )
      return rc ;
   if ( ! b4leaf(blockOn) )
   {
      #ifdef S4MDX
         for( i = 0; i <= blockOn->nKeys; i++ )
         {
            blockOn->keyOn = i ;
            rc = flag4blocks( t4, f4 ) ;
            if ( rc < 0 )
               return rc ;
         }
      #endif
      #ifdef S4CLIPPER
         for( i = 0; i <= blockOn->nKeys; i++ )
         {
            blockOn->keyOn = i ;
            rc = flag4blocks( t4, f4 ) ;
            if ( rc < 0 )
               return rc ;
         }
      #endif
   }

   tfile4up(t4) ;
   return 0 ;
}
#endif   /*  ifdef S4FOX   */

/* checks that all blocks in the file are on free list or are being used */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
static int i4checkBlocks( INDEX4 *i4 )
{
   #ifndef S4CLIPPER
      #ifndef S4OFF_MULTI
         int rc = d4lockIndex( i4->data ) ;
         if ( rc < 0 )
            return rc ;
      #endif

      INDEX4FILE *i4file = i4->indexFile ;

      #ifdef S4MDX
         // AS Oct 28/03 - support for large files
         //totBlocks = ( len - 2048 ) / i4file->header.blockRw ;
         // S4LONG len = file4longGetLo( file4lenLow( &i4file->file ) ) ;
         FILE4LONG totBlocksLong = file4lenLow( &i4file->file ) ;
         file4longSubtract( &totBlocksLong, 2048 ) ;
         file4longDivide( totBlocksLong, i4blockSize( i4file ) ) ;
         assert5( file4longGetHi( totBlocksLong ) == 0 ) ;
         S4LONG totBlocks = file4longGetLo( totBlocksLong ) ;
      #else
         // actual blocks set by 512, based on sizeof header...
         B4NODE eofBlockNo ;
         b4nodeSetFromFilePosition( i4file, &eofBlockNo, file4lenLow( &i4file->file ) ) ;
         long numFlagsInBlock = i4blockSize( i4file ) / B4BLOCK_SIZE_INTERNAL ;
         S4LONG totBlocks = b4node( eofBlockNo ) * numFlagsInBlock / (i4blockSize( i4file ) / i4multiplier( i4file ) ) ;
      #endif

      /* First Flag for the Free Chain */
      F4FLAG flags ;
      f4flagInit( &flags, i4->codeBase, (unsigned long)totBlocks, 0 ) ;


      CODE4 *c4 = i4->codeBase ;
      Bool5 checkFreeList = 1 ;

      #ifdef S4FOX
         #ifdef S4COMIX
            // for 2.5 or 2.6 compatible files, don't use free list entry in file (use local copy only)
            if ( i4file->dataFile->compatibility != 30 )
               checkFreeList = 0 ;
         #endif
         if ( checkFreeList == 1 )
         {
            B4NODE freeBlock ;
            b4nodeAssignNode( &freeBlock, i4file->tagIndex->header.freeList ) ;
            for ( ; b4node( freeBlock ) != 0 ; )
            {
               if ( b4nodesEqual( freeBlock, eofBlockNo )  ||  error4code( c4 ) < 0 )
                  break ;

               unsigned long flagNo = getFlagFromNode( i4file, freeBlock ) ;

               if ( b4node( freeBlock ) >= b4node( eofBlockNo ) )
               {
                  error4( c4, e4index, E85701 ) ;
                  break ;
               }

               // each flag size is 512 bytes, for blocks larger than this, set multiplie flags...
               for ( int setIndex = 0 ; setIndex < numFlagsInBlock ; setIndex++ )
               {
                  if ( f4flagIsSet( &flags, flagNo + setIndex ) )
                  {
                     error4( c4, e4index, E85701 ) ;
                     break ;
                  }
                  f4flagSet( &flags, flagNo + setIndex ) ;
               }

               FILE4LONG pos ;
               b4nodeGetFilePosition( i4file, freeBlock, &pos ) ;
               file4readAllInternal( &i4file->file, pos, &freeBlock, sizeof( freeBlock ) ) ;

               #ifdef S4BYTE_SWAP
                  freeBlock.node = x4reverseLong( (void *)&freeBlock ) ;
               #endif
            }
         }
         /* do the header tag */
         TAG4FILE *tagOn = i4file->tagIndex ;
         unsigned long flagNo = b4node( tagOn->headerOffset ) ;
         if ( f4flagIsSet( &flags, flagNo ) )
            return error4( i4->codeBase, e4index, E81601 ) ;
         f4flagSet( &flags, flagNo ) ;
         f4flagSet( &flags, flagNo + 1L ) ;  /* tag header is 2 blocks long */
         double multiplier = ((double)i4multiplier( i4file )) / B4BLOCK_SIZE_INTERNAL ;

         if ( tfile4freeAll( tagOn ) >= 0 )
         {
            B4NODE node1, node2, node3 ;
            b4nodeSetInvalid( &node1 ) ;
            b4nodeAssignNode( &node2, tagOn->header.root ) ;
            b4nodeSetInvalid( &node3 ) ;
            flag4blocks( tagOn, &flags, &node1, &node2, &node3 ) ;

            /* Now Flag for each block in each tag */
            for ( tagOn = 0 ;; )
            {
               tagOn = (TAG4FILE *)l4next( &i4file->tags,tagOn ) ;
               if ( tagOn == 0 )
                  break ;
               flagNo = (unsigned long)( ((double)b4node( tagOn->headerOffset )) * multiplier ) ;
               if ( f4flagIsSet( &flags, flagNo ) )
                  return error4( i4->codeBase, e4index, E81601 ) ;
               f4flagSet( &flags, flagNo ) ;
               f4flagSet( &flags, flagNo + 1L ) ;  /* tag header is 2 blocks long */

               if ( tfile4freeAll( tagOn ) < 0 )
                  break ;
               b4nodeSetInvalid( &node1) ;
               b4nodeAssignNode( &node2, tagOn->header.root ) ;
               b4nodeSetInvalid( &node3 ) ;
               if ( b4nodeInvalid( node2 ) )
               {
                  FILE4LONG pos ;
                  b4nodeGetFilePosition( i4file, tagOn->headerOffset, &pos ) ;
                  if ( file4readAllInternal( &i4file->file, pos, &node2, sizeof(node2)) < 0 )
                     return error4( i4->codeBase, e4index, E81601 ) ;
                  #ifdef S4BYTE_SWAP
                     node2.node = x4reverseLong( (void *)&node2 ) ;
                  #endif
               }
               flag4blocks( tagOn, &flags, &node1, &node2, &node3 ) ;
            }
         }
      #else
         unsigned long freeBlock ;
         // AS 05/07/99 --> inaccurate - the file length is not always updated all the way, so bad
         // results were occasionally occurring in mdx.
         // eofBlockNo = len/I4MULTIPLY ;
         S4UNSIGNED_LONG eofBlockNo = i4file->header.eof ;
         for ( freeBlock = i4file->header.freeList ; freeBlock ; )
         {
            if ( freeBlock == eofBlockNo  ||  error4code( c4 ) < 0 )
               break ;

            // unsigned long flagNo = (unsigned long )((freeBlock-4)*I4MULTIPLY/i4file->header.blockRw) ;
            unsigned long flagNo = (unsigned long )( ( freeBlock - 4 ) * I4MULTIPLY / i4blockSize( i4file ) ) ;

            if ( freeBlock >= eofBlockNo )
            {
               error4( c4, e4index, E85701 ) ;
               break ;
            }

            if ( f4flagIsSet( &flags, flagNo ) )
            {
               error4( c4, e4index, E85701 ) ;
               break ;
            }
            f4flagSet( &flags, flagNo ) ;

            FILE4LONG pos ;
            file4longAssign( pos, freeBlock * I4MULTIPLY + sizeof(S4LONG), 0 ) ;
            file4readAllInternal( &i4file->file, pos, &freeBlock, sizeof( freeBlock ) ) ;

            #ifdef S4BYTE_SWAP
               freeBlock = x4reverseLong( (void *)&freeBlock ) ;
            #endif
         }

         /* Read header information to flag the tag header blocks */
         T4DESC  desc[48] ;
         FILE4LONG pos ;
         file4longAssign( pos, 512, 0 ) ;
         file4readAllInternal( &i4file->file, pos, desc, sizeof(desc) ) ;

         /* Now Flag for each block in each tag */
         int i = 1 ;

         for ( TAG4FILE *tagOn = 0 ;; i++ )
         {
            tagOn = (TAG4FILE *)l4next( &i4file->tags, tagOn ) ;
            if ( tagOn == 0 )
               break ;
            #ifdef S4BYTE_SWAP
               desc[i].headerPos = x4reverseLong( (void *)&desc[i].headerPos ) ;
               desc[i].x1000 = 0x1000 ;
            #endif

            unsigned long flagNo = (unsigned long) ((desc[i].headerPos * I4MULTIPLY - 2048) / (long) i4file->header.blockRw) ;
            if ( f4flagIsSet( &flags, flagNo ) )
               return error4( i4->codeBase, e4index, E81601 ) ;
            f4flagSet( &flags, flagNo ) ;

            if ( tfile4freeAll(tagOn) < 0 )
               break ;
            flag4blocks( tagOn, &flags ) ;
         }
      #endif

      if ( checkFreeList )
         if ( f4flagIsAllSet( &flags, 0UL, (unsigned long)totBlocks - 1L ) == 0 )
            error4( i4->codeBase, e4index, E85702 ) ;

      u4free( flags.flags ) ;
      if ( error4code( i4->codeBase ) < 0 )
          return error4code( i4->codeBase ) ;

   #endif
   return 0 ;
}

int i4check( INDEX4 *i4 )
{
   int rc ;
   TAG4 *tagOn ;
   #ifdef S4HAS_DESCENDING
      int oldDesc ;
   #endif

   #ifdef E4PARM_HIGH
      if ( i4 == 0 )
         return error4( 0, e4parm_null, E95705 ) ;
   #endif

   if ( error4code( i4->codeBase ) < 0 )
      return e4codeBase ;

   #ifndef S4OFF_WRITE
      #ifdef S4CLIPPER
         rc = i4update( i4 ) ;
      #else
         rc = index4update( i4->indexFile ) ;
      #endif
      if ( rc < 0 )
         return rc ;
   #endif

   rc = i4checkBlocks( i4 ) ;
   if ( rc < 0 )
      return rc ;

   for( tagOn = 0 ;; )
   {
      tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
      if ( tagOn == 0 )
         return 0 ;
      #ifdef S4HAS_DESCENDING
         oldDesc = tagOn->tagFile->header.descending ;
         tagOn->tagFile->header.descending = 0 ;   /* force ascending */
         rc = t4check( tagOn ) ;
         tagOn->tagFile->header.descending = (short)oldDesc ;   /* return to previous */
         if ( rc < 0 )
            return rc ;
      #else
         rc = t4check( tagOn ) ;
         if ( rc < 0 )
            return rc ;
      #endif
   }
}

#endif  /* S4OFF_INDEX */

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4check( DATA4 *d4 )
{
   #ifdef S4OFF_INDEX
      return 0 ;
   #else
      INDEX4 *indexOn ;
      TAG4 *oldTag ;
      int rc ;

      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
            return error4( 0, e4parm_null, E95702 ) ;
      #endif

      oldTag = d4tagSelected( d4 ) ;

      #ifndef S4OFF_WRITE
         // AS 07/07/00 - was not (at least in mdx) flushing blocks out until after we had
         // detected file length, so was having block problems.  Free all blocks here to avoid... (t4index5.c/mdx/server)
         d4freeBlocks( d4 ) ;
         rc = d4updateRecord( d4, 0, 1 ) ;
         if ( rc != 0 )  /* either an error or r4unique */
            return rc ;
      #endif

      #ifndef S4OFF_MULTI
         rc = d4lockFileInternal( d4, 1, lock4write ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc )
            return rc ;
      #endif

      #ifndef S4OFF_TRAN
         rc = tran4active( d4->codeBase, d4 ) ;
         if ( rc != 0 )
            return error4( d4->codeBase, rc, E81517 ) ;
      #endif

      for( indexOn = 0 ;; )
      {
         indexOn = (INDEX4 *)l4next( &d4->indexes, indexOn ) ;
         if ( indexOn == 0 )
         {
            rc = 0 ;
            break ;
         }
         rc = i4check( indexOn ) ;
         if ( rc < 0 )
            break ;
      }

      d4tagSelect( d4, oldTag ) ;
      return rc ;
   #endif  /* S4OFF_INDEX */
}

#endif /* S4CLIENT */

