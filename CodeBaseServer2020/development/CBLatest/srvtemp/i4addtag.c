/* i4addtag.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef I4PRINT_ADD
   #include <sys\timeb.h>
   #include <time.h>
#endif


#if defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   int tfile4add( TAG4FILE *t4, unsigned char *keyInfo, const unsigned long recNum, short int errUnique )
   {
      #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4index2.c on HP */
         S4LONG tempLong ;
      #endif
      #ifdef E4PARM_LOW
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E95402 ) ;
         if ( keyInfo == 0 || recNum == 0 || recNum == INVALID4BLOCK_ID )
            return error4( t4->codeBase, e4parm_null, E95402 ) ;
      #endif

      #ifndef S4CLIPPER
         // AS Jan 9/03 - not supported for clipper
         // AS Dec 31/02 - added non-updating temporary indexes
         if ( t4->indexFile->nonUpdateable == 1 )
            return 0 ;
      #endif

      unsigned long rec = recNum ;
      CODE4 *c4 = t4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      unsigned short int oldDesc = t4->header.descending ;
      tfile4descending( t4, 0 ) ;

      int rc = tfile4seek( t4, (char *)keyInfo, t4->header.keyLen ) ;

      #ifndef S4OFF_MULTI
         /* for run-time multi-user compatibility with Clipper, must perform
            the insertion at the end of the list of keys.  This is slower than
            CodeBase's insertion at the beginning (i.e. S4OFF_MULTI is defined) */
         if ( rc == 0 && errUnique == 0 )
         {
            int dSet = 0 ;
            int incPos = t4->header.keyLen - 1 ;

            #ifdef E4ANALYZE_ALL
               unsigned long findVal = 0 ;
            #endif

            for( ; dSet == 0 && incPos >= 0 ; incPos-- )
            {
               if ( keyInfo[incPos] != 0xFF )
               {
                  keyInfo[incPos]++ ;
                  dSet = 1 ;
                  if ( tfile4seek( t4, (char *)keyInfo, t4->header.keyLen ) < 0 )
                     return -1 ;
                  keyInfo[incPos]-- ;
                  #ifdef E4ANALYZE_ALL
                     findVal = tfile4recNo( t4 ) ;
                  #endif
               }
            }

            #ifdef E4ANALYZE_ALL
               rc = tfile4seek( t4, (char *)keyInfo, t4->header.keyLen ) ;
               if ( rc != 0 || errUnique != 0 )
                  findVal = ULONG_MAX - 1L ;

               for(;;)
               {
                  if ( (*t4->cmp)( tfile4keyData( t4 )->value, keyInfo, t4->header.keyLen ) == 0 )
                  {
                     int trc = tfile4skip( t4, 1L ) ;
                     if ( trc == 0L )
                     {
                        b4goEof( tfile4block( t4 ) ) ;
                        break ;
                     }
                  }
                  else
                     break ;
               }
               if ( tfile4recNo( t4 ) != findVal )
                  return error4( c4, e4index, E85402 ) ;
            #endif
         }
      #endif

      tfile4descending( t4, oldDesc ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E95402 ) ;

      if ( rc == 0 )
      {
         switch ( errUnique )
         {
            case e4unique:
               return error4describe( c4, e4unique, E95402, tfile4alias( t4 ), t4->file.name, 0 ) ;
            case r4unique:
               return r4unique ;
            case r4uniqueContinue:
               return r4uniqueContinue ;
            default:
               break ;
         }
      }

      B4BLOCK *oldBlock = tfile4block(t4) ;

      assert5( oldBlock != 0 ) ;
      assert5( oldBlock->data != 0 ) ;

      unsigned long oldFileBlock = 0 ;
      unsigned long newFileBlock = 0 ;

      t4->header.version = (short)(t4->header.oldVersion + 1L) ;

      while( !b4leaf( oldBlock ) )
      {
         rc = tfile4down( t4 ) ;
         if ( rc < 0 || rc == 2 )
            return -1 ;
         oldBlock = tfile4block( t4 ) ;
         if ( b4leaf( oldBlock ) )
            oldBlock->keyOn = b4lastpos( oldBlock ) + 1 ;
         else
            oldBlock->keyOn = b4lastpos( oldBlock ) ;
      }

      // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - use the internal CODE4 buffers instead...
      // unsigned char oldKeyPtr[I4MAX_KEY_SIZE] ;
      // unsigned char keyData[I4MAX_KEY_SIZE+1] ;    /* temporary storage for the key data (max size 100) */
      if ( c4->tagAddBufLen < (unsigned)t4->header.keyLen )
      {
         if ( u4allocAgain( c4, &c4->tagAddBuf, &c4->tagAddBufLen, t4->header.keyLen ) < 0 )
            return e4memory ;
         if ( u4allocAgain( c4, &c4->tagAddBuf2, &c4->tagAddBufLen2, t4->header.keyLen + 1 ) < 0 )
            return e4memory ;
      }
      // AS Sep 30/04 compile fix
      unsigned char *oldKeyPtr = (unsigned char *)c4->tagAddBuf ;
      unsigned char *keyData = (unsigned char *)c4->tagAddBuf2 ;

      for(;;)
      {
         if ( oldBlock == 0 )
         {
            /* Must create a new root block */
            unsigned long extendBlock = tfile4extend( t4 ) ;
            if ( extendBlock == INVALID4BLOCK_ID )
               return -1 ;

            B4BLOCK *rootBlock = b4alloc( t4, extendBlock ) ;
            if ( rootBlock == 0 )
               return -1 ;

            l4add( &t4->blocks, rootBlock ) ;

            /* need to set root first so that b4insert() will record that current
               block is the root block, which is vital to its functioning */
            t4->header.root = rootBlock->fileBlock * 512 ;
            t4->rootWrite = 1 ;
            b4insert( rootBlock, keyInfo, rec, oldFileBlock ) ;
            b4append( rootBlock, newFileBlock ) ;

            return 0 ;
         }

         assert5( oldBlock->data != 0 ) ;

         if ( b4room( oldBlock ) )
         {
            if ( b4leaf( oldBlock ) )
               b4insert( oldBlock, keyInfo, rec, 0L ) ;
            else   /* update the current pointer, add the new branch */
            {
               #ifdef E4ANALYZE
                  if ( oldBlock->nKeys == 0 )
                     return error4( t4->codeBase, e4index, E95402 ) ;
               #endif
               B4KEY_DATA *atNew = b4key( oldBlock, oldBlock->keyOn ) ;
              #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4index2.c on HP */
                       tempLong = newFileBlock * 512 ;
                  memcpy( &atNew->pointer, &tempLong, sizeof(S4LONG) ) ;
                    #else
                  atNew->pointer = newFileBlock * 512 ;
               #endif
               b4insert( oldBlock, keyInfo, rec, oldFileBlock ) ;
            }
            return 0 ;
         }

         l4pop( &t4->blocks ) ;

         /* NNNNOOOO  N - New, O - Old */
         /* The new block's end key gets added to the block just up */
         B4BLOCK *newBlock = 0 ;
         B4BLOCK *wchBlock = 0 ;

         if ( oldBlock->keyOn < (t4->header.keysHalf + ( b4leaf( oldBlock ) ? 0 : 1 ) ) )
         {
            /* AS 05/05/98 had 0 as 3rd param here, 1 for split below, I think this was incorrect, so changed
             maybe should be dependent on leaf? */
            newBlock= tfile4split( t4, oldBlock, 1 ) ;
            if ( newBlock == 0 )
               return -1 ;
            wchBlock = oldBlock ;
         }
         else
         {
            newBlock= tfile4split( t4, oldBlock, 0 ) ;
            if ( newBlock == 0 )
               return -1 ;
            wchBlock = newBlock ;
         }

         assert5( newBlock != 0 ) ;
         assert5( wchBlock != 0 ) ;

         if ( b4leaf( wchBlock ) )
         {
            b4insert( wchBlock, keyInfo, rec, 0L ) ;
            if ( newBlock->nKeys <= t4->header.keysHalf )   /* add a key from the old block!, must have info in newBlock because oldBlock gets deleted below */
            {
               #ifdef E4ANALYZE
                  if ( oldBlock->nKeys <= t4->header.keysHalf )  /* impossible */
                     #ifdef S4CLIPPER
                        return error4describe( t4->codeBase, e4index, E81601, tfile4alias( t4 ), t4->file.name, 0 ) ;
                     #else
                        return error4describe( t4->codeBase, e4index, E81601, tfile4alias( t4 ), t4->indexFile->file.name, 0 ) ;
                     #endif
               #endif
               oldBlock->keyOn = oldBlock->nKeys - 1 ;
               c4memcpy( keyData, b4keyKey( oldBlock, oldBlock->keyOn ), t4->header.keyLen ) ;
               keyInfo = keyData ;
               rec = b4key( oldBlock, oldBlock->keyOn )->num ;
               b4remove( oldBlock ) ;
               newBlock->keyOn = 0 ;
               b4insert( newBlock, keyInfo, rec, 0 ) ;
            }
            newBlock->keyOn = 0 ;
            c4memcpy( keyData, b4keyKey( newBlock, newBlock->keyOn ), t4->header.keyLen ) ;
            keyInfo = keyData ;
            #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4seek.cpp on HP */
               memcpy( &rec, &b4key( newBlock, newBlock->keyOn )->num, sizeof(S4LONG) ) ;
            #else
               rec = b4key( newBlock, newBlock->keyOn )->num ;
            #endif
            b4remove( newBlock ) ;
         }
         else
         {
            /* now get the key to place upwards */
            if ( wchBlock->nKeys == 0 )   /* treat like a root block */
            {
               if ( wchBlock == oldBlock )  /* not a problem if number of keys very small ?? */
               {
                  if ( wchBlock->keyOn == 1 )   /* at end, so must combine with new block alterations */
                  {
                     wchBlock->nKeys = 1 ;  /* now reset to the proper value */
                     /* don't actually need to do an insert, just set the file block value */
                     /* but need to copy values in so that later copy for data gives correct results */
                     c4memcpy( b4keyKey( oldBlock, 1), keyInfo, t4->header.keyLen ) ;
                     b4key( oldBlock, 1 )->num = rec ;
                     B4KEY_DATA *atNew = b4key( oldBlock, 1 ) ;
                     atNew->pointer = oldFileBlock * 512 ;
                     atNew = b4key( newBlock, 0 ) ;  /* currently set to old value, reset to new */
                     atNew->pointer = newFileBlock * 512 ;
                  }
                  else  /* simple insert */
                  {
                     wchBlock->nKeys = 1 ;  /* to get a proper insert */
                     b4insert( wchBlock, keyInfo, rec, oldFileBlock ) ;
                     B4KEY_DATA *atNew = b4key( wchBlock, 1 ) ;
                     atNew->pointer = newFileBlock * 512 ;
                     c4memcpy( oldKeyPtr, b4keyKey( oldBlock, 1 ), t4->header.keyLen ) ;
                     keyInfo = oldKeyPtr ;
                     rec = b4key( oldBlock, 1 )->num ;
                     wchBlock->nKeys = 1 ;  /* now reset to the proper value */
                  }
               }
               else
               {
                  b4insert( wchBlock, keyInfo, rec, oldFileBlock ) ;
                  b4append( wchBlock, newFileBlock ) ;
               }
            }
            else
            {
               if ( wchBlock->keyOn > wchBlock->nKeys && wchBlock == oldBlock )
               {
                  B4KEY_DATA *atNew = b4key( newBlock, 0 ) ;
                  #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4index2.c on HP */
                               tempLong = newFileBlock * 512 ;
                     memcpy( &atNew->pointer, &tempLong, sizeof(S4LONG) ) ;
                  #else
                     atNew->pointer = newFileBlock * 512 ;
                  #endif
               }
               else
               {
                  B4KEY_DATA *atNew = b4key( wchBlock, wchBlock->keyOn ) ;
                  #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4index2.c on HP */
                     tempLong = newFileBlock * 512 ;
                     memcpy( &atNew->pointer, &tempLong, sizeof(S4LONG) ) ;
                       #else
                     atNew->pointer = newFileBlock * 512 ;
                  #endif
               }
               b4insert( wchBlock, keyInfo, rec, oldFileBlock ) ;
            }
            c4memcpy( oldKeyPtr, b4keyKey( oldBlock, b4lastpos( oldBlock )), t4->header.keyLen ) ;
            keyInfo = oldKeyPtr ;
            #ifdef S4DATA_ALIGN  /* LY 00/02/17 : t4index2.c on HP */
               memcpy( &rec, &b4key( oldBlock, b4lastpos( oldBlock ) )->num, sizeof(S4LONG) ) ;
            #else
               rec = b4key( oldBlock, b4lastpos( oldBlock ) )->num ;
            #endif
         }

         #ifdef E4INDEX_VERIFY
            rc = b4verify( oldBlock ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, rc, E91642 ) ;
            rc = b4verify( newBlock ) ;
            if ( rc < 0 )
               return error4stack( t4->codeBase, rc, E91642 ) ;
         #endif

         l4add( &t4->saved, newBlock ) ;
         newFileBlock = newBlock->fileBlock ;
         oldFileBlock = oldBlock->fileBlock ;
         if ( b4flush( oldBlock ) < 0 )
            return -1 ;
         b4free( oldBlock ) ;
         oldBlock = (B4BLOCK *) t4->blocks.lastNode ;
      }
   }
#endif /* defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) */



#if defined( E4ANALYZE ) && defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   static void tfile4verifyBlocksValid( TAG4FILE *tfile )
   {
      // do a quick scan to ensure integrity of blocks...

      for ( B4BLOCK *blockOn = 0 ;; )
      {
         blockOn = (B4BLOCK *)l4next( &tfile->blocks, blockOn ) ;
         if ( blockOn == 0 )
            break ;
         assert5( blockOn->data != 0 ) ;
      }

      for ( B4BLOCK *saveBlockOn = 0 ;; )
      {
         saveBlockOn = (B4BLOCK *)l4next( &tfile->saved, saveBlockOn ) ;
         if ( saveBlockOn == 0 )
            break ;
         assert5( saveBlockOn->data != 0 ) ;
      }
   }
#endif /* #if defined( E4ANALYZE ) && defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )*/



#if defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   int t4addCalc( TAG4 *t4, long rec )
   {
      int rc ;
      char *ptr ;
      TAG4FILE *tfile ;
      #ifndef S4OFF_TRAN
         TAG4KEY_REMOVED *removed ;
      #endif

      #ifdef E4PARM_LOW
         if ( t4 == 0 || rec < 1 )
            return error4( 0, e4parm, E95403 ) ;
      #endif

      #ifndef S4CLIPPER
         // AS Jan 9/03 - not supported for clipper
         // AS Dec 31/02 - added non-updating temporary indexes
         if ( t4->indexFile->nonUpdateable == 1 )
            return 0 ;
      #endif

      tfile = t4->tagFile ;

      if ( error4code( tfile->codeBase ) < 0 )
         return e4codeBase ;

      if ( tfile->filter )
         if ( !expr4true( tfile->filter ) )
            return 0;

      rc = tfile4exprKey( tfile, (unsigned char **)&ptr ) ;
      if ( rc < 0 )
         return error4stack( tfile->codeBase, rc, E95403 ) ;

      #ifndef S4OFF_TRAN
         if ( code4tranStatus( tfile->codeBase ) == r4active && ( t4unique( t4 ) == r4unique ||
              t4unique( t4 ) == e4unique ) )  /* just remove from the removed list */
         {
            removed = t4keyFind( t4, 0L, ptr ) ;
            if ( removed != 0 )
            {
               #ifdef I4PRINT
                  char dump[255];
                  sprintf( dump, "t4keyFind special case for tag: %s\r\n", t4->alias ) ;
                  log5( dump ) ;
               #endif
               if ( tfile4remove( tfile, removed->key, removed->recno ) < 0 )
                  return -1 ;
               // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
               #ifdef SHARE4TAG_REMOVE
                  l4remove( &t4->tagFile->removedKeys, removed ) ;
               #else
                  l4remove( &t4->removedKeys, removed ) ;
               #endif
               u4free( removed ) ;
               return 0 ;
            }
         }
      #endif
      #ifdef E4ANALYZE
         // do a quick scan to ensure integrity of blocks...
         tfile4verifyBlocksValid( tfile ) ;
      #endif
      rc = tfile4add( tfile, (unsigned char *)ptr, rec, t4unique( t4 ) ) ;
      #ifdef E4ANALYZE
         // do a quick scan to ensure integrity of blocks...
         tfile4verifyBlocksValid( tfile ) ;
      #endif
      return rc ;
   }
#endif /* defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) */



#if defined( S4FOX ) && !defined( S4OFF_MULTI ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   static B4NODE tfile4swap( B4BLOCK *parent, B4BLOCK *left )
   {
      /* (temporary) fix for FoxPro multi-user compatibility swaps parent and right blocks */
      B4NODE tempFb = left->fileBlock ;
      left->fileBlock = parent->fileBlock ;
      parent->fileBlock = tempFb ;

      assert5( b4nodeValid( left->fileBlock ) ) ;
      assert5( b4nodeValid( parent->fileBlock ) ) ;

      INDEX4FILE *i4file = parent->tag->indexFile ;

      /* now update neighbours */
      if ( b4nodeValid( left->header.rightNode ) )
      {
         // update old left blocks right neighbour by replacing right neighbours left node

         FILE4LONG pos ;
         // set position to byte offset of right neighbours left node
         b4nodeGetFilePosition( i4file, left->header.rightNode, &pos ) ;
         file4longAdd( &pos, 2 * sizeof( short ) ) ;
         #ifdef S4BYTE_SWAP
            S4LONG longVal = x4reverseLong( (void *)&left->fileBlock ) ;
            file4writeInternal( &parent->tag->indexFile->file, pos, &longVal, sizeof( B4NODE ) ) ;
         #else
            file4writeInternal( &parent->tag->indexFile->file, pos, &left->fileBlock, sizeof( B4NODE ) ) ;
         #endif
      }

      if ( b4nodeValid( left->header.leftNode ) )
      {
         // update old left blocks left neighbour by replacing left neighbours right node
         FILE4LONG pos ;

         // set position to byte offset of left neighbours right node
         b4nodeGetFilePosition( i4file, left->header.leftNode, &pos ) ;
         file4longAdd( &pos, 2 * sizeof( short ) + sizeof( B4NODE ) ) ;
         #ifdef S4BYTE_SWAP
            S4LONG longVal = x4reverseLong( (void *)&left->fileBlock ) ;
            file4writeInternal( &parent->tag->indexFile->file, pos, &longVal, sizeof( B4NODE ) ) ;
         #else
            file4writeInternal( &parent->tag->indexFile->file, pos, &left->fileBlock, sizeof( B4NODE ) ) ;
         #endif
      }

      if ( b4nodeValid( parent->header.rightNode ) )
      {
         // update old parent blocks right neighbour by replacing right neighbours left node
         FILE4LONG pos ;

         // set position to byte offset of right neighbours left node
         b4nodeGetFilePosition( i4file, parent->header.rightNode, &pos ) ;
         file4longAdd( &pos, 2 * sizeof(short) ) ;
         #ifdef S4BYTE_SWAP
            S4LONG longVal = x4reverseLong( (void *)&parent->fileBlock ) ;
            file4writeInternal( &parent->tag->indexFile->file, pos, &longVal, sizeof( B4NODE ) ) ;
         #else
            file4writeInternal( &parent->tag->indexFile->file, pos, &parent->fileBlock, sizeof( B4NODE ) ) ;
         #endif
      }

      if ( b4nodeValid( parent->header.leftNode ) )
      {
         // update old parent blocks left neighbour by replacing left neighbours right node
         FILE4LONG pos ;

         // set position to byte offset of left neighbours right node
         b4nodeGetFilePosition( i4file, parent->header.leftNode, &pos ) ;
         file4longAdd( &pos, 2 * sizeof(short) + sizeof( B4NODE ) ) ;
         #ifdef S4BYTE_SWAP
            S4LONG longVal = x4reverseLong( (void *)&parent->fileBlock ) ;
            file4writeInternal( &parent->tag->indexFile->file, pos, &longVal, sizeof( B4NODE ) ) ;
         #else
            file4writeInternal( &parent->tag->indexFile->file, pos, &parent->fileBlock, sizeof( B4NODE ) ) ;
         #endif
      }

      return left->fileBlock ;
   }
#endif /* #if defined( S4FOX ) && !defined( S4OFF_MULTI ) !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) */



#if defined( S4FOX ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   int tfile4add( TAG4FILE *t4, const unsigned char *keyInfo, const unsigned long recIn, short int errUnique )
   {
      int rc ;

      // AS Dec 31/02 - added non-updating temporary indexes
      if ( t4->indexFile->nonUpdateable == 1 )
         return 0 ;

      /* in fox case the tfile4add might not actually add the key */
      for( ;; )
      {
         rc = tfile4addDo( t4, keyInfo, recIn, errUnique ) ;
         if ( rc != 1 )
            return rc ;
      }
   }
#endif /* #if defined( S4FOX ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) */



#if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   int tfile4addDo( TAG4FILE *t4, const unsigned char *keyInfo, unsigned long rec, short int errUnique )
   {
      /* errUnique must be 0 if the tag is not unique, otherwise must contain unique error */
      #ifdef E4PARM_LOW
         if ( t4 == 0 )
            return error4( 0, e4parm_null, E95402 ) ;
         if ( keyInfo == 0 || rec == 0 || rec == INVALID4BLOCK_ID )
            return error4( t4->codeBase, e4parm_null, E95402 ) ;
      #endif

      // AS Dec 31/02 - added non-updating temporary indexes
      if ( t4->indexFile->nonUpdateable == 1 )
         return 0 ;

      CODE4 *c4 = t4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      INDEX4FILE *i4 = t4->indexFile ;

      #ifdef S4FOX
         unsigned long rec1 = 0L ;
         unsigned long rec2 = 0L ;
         const unsigned char *tempKey = 0 ;
         if ( t4->expr != 0 )
            switch( errUnique )  /* ensure not a null add if r4/e4 candidate */
            {
               case r4candidate:
                  if ( expr4nullLow( t4->expr, 1 ) == 1 )
                     return r4unique ;
                  break ;
               case e4candidate:
                  if ( expr4nullLow( t4->expr, 1 ) == 1 )
                     return error4describe( c4, e4unique, E95402, tfile4alias( t4 ), i4->file.name, 0 ) ;
                  break ;
               default:
                  break ;
            }

         int rc = tfile4go( t4, keyInfo, rec, 1 ) ;
      #else
         int rc = tfile4seek( t4, keyInfo, t4->header.keyLen ) ;
      #endif

      if ( rc < 0 )
         return error4stack( c4, rc, E95402 ) ;

      #ifdef I4PRINT_ADD
         #ifdef I4PRINT_TAG_NAME
            if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
         #endif
         {
            struct _timeb mTime ;
            char *outTime, dump[255] ;
            _ftime( &mTime ) ;
            outTime = ctime( &( mTime.time ) ) ;
            B4BLOCK *i4block = (B4BLOCK *)t4->blocks.lastNode ;

            sprintf( dump, "id = %ld, add tag key: fb = %uld, rec = %ld, key = %.50s, time: %s\r\n", (long)i4, i4block->fileBlock, (long)rec, (char *)keyInfo, outTime ) ;
            log5( dump ) ;
            i4block = (B4BLOCK *)l4first( &t4->blocks ) ;
            long i4blockRecNo = b4recNo( i4block, i4block->keyOn) ;
            sprintf( dump, "add extra info: rc = %ld, root block: fb = %uld, curKey # = %ld, recNoAtKey: %ld\r\n", rc, i4block->fileBlock, i4block->keyOn, i4blockRecNo ) ;
            log5( dump ) ;
            char cnt = 2 ;
            for( ;; )
            {
               i4block = (B4BLOCK *)l4next( &t4->blocks, i4block ) ;
               if ( i4block != 0 && i4block != (B4BLOCK *)t4->blocks.lastNode )
               {
                  sprintf( dump, "add extra info: rc = %ld, %ldnd block: fb = %uld, curKey # = %ld\r\n", rc, (long)cnt, i4block->fileBlock, i4block->keyOn ) ;
                  log5( dump ) ;
                  cnt++ ;
               }
               else
                  break ;
            }
         }
      #endif

      #ifdef S4FOX
         if ( rc == 0 )
         {
            switch( errUnique )
            {
               case e4unique:
               case e4candidate:
                  return error4describe( c4, e4unique, E95402, tfile4alias( t4 ), i4->file.name, 0 ) ;
               case r4candidate :
                  return r4unique ;
               default:
                  break ;
            }
         }

         if ( errUnique && rc == r4found )
      #else
         if ( errUnique && rc == 0 )
      #endif
         {
            switch ( errUnique )
            {
               #ifdef S4FOX
                  case e4candidate:
               #endif
               case e4unique:
                  return error4describe( c4, e4unique, E95402, tfile4alias( t4 ), i4->file.name, 0 ) ;
               #ifdef S4FOX
                  case r4candidate:
               #endif
               case r4unique:
                  return r4unique ;
               case r4uniqueContinue:
                  return r4uniqueContinue ;
               default:
                  break ;
            }
         }

      if ( t4->filter && !t4->hasKeys )
      {
         FILE4LONG pos ;
         b4nodeGetFilePosition( i4, t4->headerOffset, &pos ) ;
         file4longAdd( &pos, sizeof( t4->header ) + 222 ) ;
         file4writeInternal( &i4->file, pos, (char *) "\0", 1 ) ;
         t4->hasKeys = (char)1 ;
         #ifdef S4MDX
            t4->hadKeys = (char)0 ;
         #endif
      }

      B4BLOCK *oldBlock = tfile4block( t4 ) ;
      assert5( b4nodeValid( oldBlock->fileBlock ) ) ;
      B4NODE oldFileBlock ;
      b4nodeAssignLong( &oldFileBlock, 0 ) ;

      #ifdef S4FOX
         int doInsert = 1 ;
         int updateReqd = 0 ;
         int didAdd = 1 ;

         for( ;; )
         {
            if ( doInsert == 1 )
            {
               // #ifdef I4PRINT_ADD
               //    #ifdef I4PRINT_TAG_NAME
               //       if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
               //    #endif
               //    {
               //       char dump[255];
               //       sprintf( dump, "      inserting into block) ;
               //       log5( dump ) ;
               //    }
               // #endif
               i4->tagIndex->header.version = i4->versionOld + 1 ;
               if ( oldBlock == 0 )
               {
                  /* Must create a new root block */
                  B4NODE extendBlock = index4extend( i4 ) ;
                  if ( b4nodeInvalid( extendBlock ) )
                     return -1 ;

                  B4BLOCK *rootBlock = b4alloc( t4, extendBlock ) ;
                  if ( rootBlock == 0 )
                     return -1 ;

                  b4nodeSetInvalid( &rootBlock->header.leftNode ) ;
                  b4nodeSetInvalid( &rootBlock->header.rightNode ) ;
                  rootBlock->header.nodeAttribute = 1 ;

                  l4add( &t4->blocks, rootBlock ) ;

                  #ifndef S4OFF_MULTI
                     if ( t4->indexFile->file.lowAccessMode != OPEN4DENY_RW )
                     {
                        oldFileBlock = tfile4swap( rootBlock, (B4BLOCK *)l4last( &t4->saved ) ) ;
                        if ( b4nodeInvalid( oldFileBlock ) )
                           return -1 ;
                     }

                  #endif

                  b4top( rootBlock ) ;
                  rc = b4insert( rootBlock, tempKey, rec, rec2, 1 ) ;
                  if ( rc < 0 )
                     return rc ;
                  rc = b4insert( rootBlock, keyInfo, b4node( oldFileBlock ), rec1, 1 ) ;
                  if ( rc < 0 )
                     return rc ;

                  b4nodeAssignNode( &t4->header.root, rootBlock->fileBlock ) ;
                  t4->rootWrite = 1 ;
                  if ( didAdd == 0 )
                     return 1 ;
                  return 0 ;
               }

               if ( (rc = b4insert( oldBlock, keyInfo, rec, rec1, 0 )) != 1 )
               {
                  if ( rc == 0 )
                  {
                     if ( b4leaf( oldBlock ) )
                     {
                        tempKey = keyInfo ;
                        rec2 = rec ;
                     }
                     if ( oldBlock->keyOn == oldBlock->header.nKeys - 1 )
                        updateReqd = 1 ;
                     doInsert = 0 ;
                     continue ;
                  }
                  else
                     return rc ;
               }
               else
               {
                  l4pop( &t4->blocks ) ;
                  int keyOn = oldBlock->keyOn ;

                  /* The new block's end key gets added to the block just up */
                  #ifdef I4PRINT_ADD
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        log5("splitting block\r\n");
                     }
                  #endif
                  B4BLOCK *newBlock = tfile4split( t4, oldBlock ) ;
                  if ( newBlock == 0 )
                     return -1 ;

                  l4add( &t4->saved, oldBlock ) ;

                  // AS 08/14/01 - T4LARGE4.C -s1234 -i512 -r65044 (see i4tag.c tfile4leafSplit)
                  // if ( keyOn < oldBlock->header.nKeys )
                  if ( keyOn < oldBlock->header.nKeys || oldBlock->header.nKeys == 0 && keyOn == 0 )
                  {
                     b4go( oldBlock, keyOn ) ;
                     rc = b4insert( oldBlock, keyInfo, rec, rec1, 0 ) ;
                  }
                  else
                  {
                     b4go( newBlock, (keyOn - oldBlock->header.nKeys) ) ;
                     rc = b4insert( newBlock, keyInfo, rec, rec1, 0 ) ;
                     if ( rc == 0 )  /* if there was room to insert and on the key, need to change the upper block entry */
                        if ( newBlock->keyOn == newBlock->header.nKeys - 1 )
                           updateReqd = 1 ;
                  }

                  if ( rc == 1 )  /* was not possible to insert the key */
                     didAdd = 0 ;

                  #ifdef E4INDEX_VERIFY
                     rc = b4verify( oldBlock ) ;
                     if ( rc < 0 )
                        return error4stack( t4->codeBase, rc, E91642 ) ;
                     rc = b4verify( newBlock ) ;
                     if ( rc < 0 )
                        return error4stack( t4->codeBase, rc, E91642 ) ;
                  #endif

                  /* Now add to the block just up */
                  b4goEof( oldBlock ) ;
                  oldBlock->keyOn-- ;

                  keyInfo = b4keyKey( oldBlock, oldBlock->keyOn ) ;
                  b4nodeAssignNode( &oldFileBlock, oldBlock->fileBlock ) ;
                  rec1 = b4recNo( oldBlock, oldBlock->keyOn ) ;

                  rec = b4node( newBlock->fileBlock ) ;
                  rc = b4flush(newBlock) ;
                  if ( rc < 0 )
                     return error4stack( t4->codeBase, rc, E91642 ) ;

                  b4goEof( newBlock ) ;
                  newBlock->keyOn-- ;

                  // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - make dynamic
                  if ( t4->header.keyLen + 2 * sizeof( S4LONG ) > c4->savedKeyLen )
                  {
                     if ( u4allocAgain( c4, &c4->savedKey, &c4->savedKeyLen, t4->header.keyLen + 2 * sizeof( S4LONG )  ) < 0 )
                        return e4memory ;
                  }
                  tempKey = (unsigned char *)c4->savedKey ;
                  c4memcpy( (void *)tempKey, (void *)b4keyKey( newBlock, newBlock->keyOn ), (unsigned int)t4->header.keyLen ) ;
                  rec2 = b4recNo( newBlock, newBlock->keyOn ) ;
                  if( newBlock->keyOn == newBlock->header.nKeys - 1 )
                     updateReqd = 1 ;
                  b4free( newBlock ) ;
               }
            }
            else
            {
               LINK4 *savedLink = (LINK4 *)l4pop( &t4->blocks ) ;
               l4add( &t4->saved, savedLink ) ;  /* cannot l4pop param to l4add */
            }

            oldBlock = (B4BLOCK *)t4->blocks.lastNode ;

            if ( oldBlock == 0 )
            {
               if ( doInsert == 0 )
               {
                  if ( didAdd == 0 )
                     return 1 ;
                  return 0 ;
               }
            }
            else
               if ( updateReqd )  /* may have to update a parent block */
               {
                  #ifdef I4PRINT_ADD
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( t4->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        char dump[255];
                        B4NODE blockNoInParent ;
                        b4nodeAssignLong( &blockNoInParent, (unsigned long)x4reverseLong( (void *)( ((unsigned char *)&oldBlock->nodeHdr)
                               + ( oldBlock->keyOn + 1 ) * ( 2 * sizeof( S4LONG ) + t4->header.keyLen ) - sizeof( S4LONG ) ) ) ) ;

                        sprintf( dump, "      updating parent block, block #: %ld, key On: %ld, key Count: %ld with rec#: %ld and block ref#: %ld\r\n",
                           (long)b4node( oldBlock->fileBlock ), (long)oldBlock->keyOn, (long)b4numKeys(oldBlock), (long)rec2, (long)b4node(blockNoInParent) ) ;
                        log5( dump ) ;
                     }
                  #endif
                  /* LY 00/04/06 : cast to S4UNSIGNED_LONG for 64-bit HP-UX */
                  if ( b4brReplace( oldBlock, tempKey, (S4UNSIGNED_LONG)rec2 ) < 0 )
                     return -1 ;
                  if ( oldBlock->keyOn != oldBlock->header.nKeys - 1 )  /* done reqd updates */
                     updateReqd = 0 ;
               }
         }
      #else              /* if not S4FOX  */
         i4->changed = 1 ;
         t4->changed = 1 ;
         t4->header.version++ ;

         for(;;)
         {
            if ( oldBlock == 0 )
            {
               /* Must create a new root block */
               unsigned long extendBlock = index4extend( i4 ) ;
               if ( extendBlock == INVALID4BLOCK_ID )
                  return -1 ;

               B4BLOCK *rootBlock = b4alloc( t4, extendBlock) ;
               if ( rootBlock == 0 )
                  return -1 ;

               l4add( &t4->blocks, rootBlock ) ;

               b4insert( rootBlock, keyInfo, oldFileBlock ) ;
               b4insert( rootBlock, keyInfo, rec ) ;
               rootBlock->nKeys-- ;
               t4->header.root = rootBlock->fileBlock ;
               t4->rootWrite = 1 ;
               return 0 ;
            }

            if ( oldBlock->nKeys < oldBlock->tag->header.keysMax )
            {
               b4insert( oldBlock, keyInfo, rec ) ;
               return 0 ;
            }

            l4pop( &t4->blocks ) ;

            int isBranch = b4leaf( oldBlock )  ?  0 : 1 ;

            /* NNNNOOOO  N - New, O - Old */
            /* The new block's end key gets added to the block just up */
            B4BLOCK *newBlock= tfile4split( t4, oldBlock ) ;
            if ( newBlock == 0 )
               return -1 ;

            l4add( &t4->saved, newBlock ) ;

            newBlock->nKeys -= (short)isBranch ;
            if ( newBlock->keyOn < (newBlock->nKeys+isBranch) )
               b4insert( newBlock, keyInfo, rec ) ;
            else
               b4insert( oldBlock, keyInfo, rec ) ;

            #ifdef E4INDEX_VERIFY
               rc = b4verify( oldBlock ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, rc, E91642 ) ;
               rc = b4verify( newBlock ) ;
               if ( rc < 0 )
                  return error4stack( t4->codeBase, rc, E91642 ) ;
            #endif

            /* Now add to the block just up */
            newBlock->keyOn = b4lastpos(newBlock) ;

            keyInfo = b4keyKey( newBlock, newBlock->keyOn ) ;
            rec = newBlock->fileBlock ;

            oldFileBlock = oldBlock->fileBlock ;
            if ( b4flush(oldBlock) < 0 )
               return -1 ;

            b4free( oldBlock ) ;
            oldBlock = (B4BLOCK *) t4->blocks.lastNode ;
         }
      #endif
   }
#endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) */



#if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   int t4addCalc( TAG4 *t4, long rec )
   {
      int len ;
      unsigned char *ptr ;
      TAG4FILE *tfile ;
      #ifndef S4OFF_TRAN
         TAG4KEY_REMOVED *removed ;
      #endif

      #ifdef E4PARM_LOW
         if ( t4 == 0 || rec < 1 )
            return error4( 0, e4parm, E95403 ) ;
      #endif

      tfile = t4->tagFile ;

      if ( error4code( tfile->codeBase ) < 0 )
         return e4codeBase ;

      if ( tfile->filter )
         if ( !expr4true( tfile->filter ) )
            return 0;

      len = tfile4exprKey( tfile, (unsigned char **)&ptr ) ;
      if ( len < 0 )
         return error4stack( tfile->codeBase, len, E95403 ) ;

      #ifdef E4ANALYZE
         if ( len != tfile->header.keyLen )
            return error4describe( tfile->codeBase, e4index, E95403, tfile4alias( tfile ), 0, 0 ) ;
      #endif

      #ifndef S4OFF_TRAN
         if ( code4tranStatus( tfile->codeBase ) == r4active && ( t4unique( t4 ) == r4unique || t4unique( t4 ) == e4unique
              #ifdef S4FOX
                 || t4unique( t4 ) == r4candidate
              #endif
              ) )  /* just remove from the removed list */
         {
            removed = t4keyFind( t4, rec,(char *)ptr ) ;
            /* if found it means it is already there, so just remove from list */
            if ( removed != 0 )
            {
               // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
               #ifdef SHARE4TAG_REMOVE
                  l4remove( &t4->tagFile->removedKeys, removed ) ;
               #else
                  l4remove( &t4->removedKeys, removed ) ;
               #endif
               u4free( removed ) ;
               return 0 ;
            }
            removed = t4keyFind( t4, 0L, (char *)ptr ) ;
            if ( removed != 0 )
            {
               #ifdef I4PRINT
                  char dump[255];
                  sprintf( dump, "t4keyFind special case for tag: %s\r\n", t4->tagFile->alias ) ;
                  log5( dump ) ;
               #endif
               if ( tfile4remove( tfile, removed->key, removed->recno ) < 0 )
                  return -1 ;
               // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
               #ifdef SHARE4TAG_REMOVE
                  l4remove( &t4->tagFile->removedKeys, removed ) ;
               #else
                  l4remove( &t4->removedKeys, removed ) ;
               #endif
               u4free( removed ) ;
            }
         }
      #endif
      return tfile4add( tfile, (unsigned char *)ptr, rec, t4unique( t4 ) ) ;
   }
#endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) */
