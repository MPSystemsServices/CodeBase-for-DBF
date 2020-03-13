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

/* i4add.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   int S4FUNCTION t4uniqueModify( TAG4 *tag, int newUnique )
   {
      TAG4FILE *tagFile ;
      FILE4LONG pos ;

      #ifdef E4PARM_HIGH
         if ( tag == 0 )
            return error4( 0, e4parm_null, E91716 ) ;
         if ( newUnique != e4unique && newUnique != r4unique && newUnique !=
              r4uniqueContinue && newUnique != 0 )
            #ifdef S4FOX
               if ( newUnique != r4candidate && newUnique != e4candidate )
            #endif
               return error4( 0, e4parm, E91716 ) ;
      #endif

      tagFile = tag->tagFile ;
      tag->errUnique = newUnique ;

      #ifdef S4FOX
         tagFile->header.typeCode &= 0xFA ;  /* clear the unqiuye setting */
         if ( newUnique == r4candidate || newUnique == e4candidate )
            tagFile->header.typeCode |= 0x04 ;
         else
            if ( newUnique == r4unique || newUnique == e4unique || newUnique == r4uniqueContinue )
               tagFile->header.typeCode |= 0x01 ;
         b4nodeGetFilePosition( tagFile->indexFile, tagFile->headerOffset, &pos ) ;
         file4longAdd( &pos, 3 * sizeof( S4LONG ) + sizeof( short ) ) ;
      #endif
      #ifdef S4MDX
         tagFile->header.typeCode &= 0xDF ;
         if ( newUnique == r4unique || newUnique == e4unique || newUnique == r4uniqueContinue )
         {
            tagFile->header.typeCode |= 0x40 ;
            tagFile->header.unique = 0x4000 ;
         }
         b4nodeGetFilePosition( tagFile->indexFile, tagFile->headerOffset, &pos ) ;
         file4longAdd( &pos, tagFile->headerOffset + 8 ) ;
      #endif
      #ifdef S4CLIPPER
         if ( newUnique == r4unique || newUnique == e4unique || newUnique == r4uniqueContinue )
            tagFile->header.unique = 1 ;
         else
            tagFile->header.unique = 0 ;
         /* in header, 256 byters allocated for expression, though max expr size is 255 (I4MAX_EXPR_SIZE 255), so add 1 */
         file4longAssign( pos, I4MAX_EXPR_SIZE + 22 + 1, 0 ) ;
         return file4writeInternal( &tagFile->file, pos, &tagFile->header.unique, sizeof(tagFile->header.unique)) ;
      #else
         return file4writeInternal( &tagFile->indexFile->file, pos, &tagFile->header.typeCode, sizeof(tagFile->header.typeCode)) ;
      #endif
   }
#endif /* #if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if defined( S4CLIENT )
   int S4FUNCTION i4tagAdd( INDEX4 *i4, const TAG4INFO *tagData )
   {
      CONNECTION4 *connection ;
      CONNECTION4TAG_ADD_INFO_IN *dataIn ;
      CONNECTION4TAG_ADD_INFO_OUT *dataOut ;
      CONNECTION4TAG_INFO *tinfo ;
      unsigned int len, len2, len3 ;
      DATA4 *d4 ;
      TAG4 *tag ;
      CODE4 *c4 ;
      int rc ;
      short j, offset ;
      short numTags ;

      #ifdef E4PARM_HIGH
         if ( i4 == 0 || tagData == 0 )
            return error4( 0, e4parm_null, E91717 ) ;
      #endif

      d4 = i4->data ;
      c4 = i4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifndef S4OFF_WRITE
         rc = d4updateRecord( d4, 0, 1 ) ;
         if ( rc )
            return rc ;
      #endif

      if ( d4->readOnly == 1 )
         return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;
      error4set( c4, 0 ) ;  /* Make sure it is not 'r4unique' or 'r4noCreate'. */

      connection = d4->dataFile->connection ;
      if ( connection == 0 )
         return error4( c4, e4parm, E91717 ) ;

      connection4assign( connection, CON4ADD_TAG, data4clientId( d4 ), data4serverId( d4 ) ) ;

      connection4addData( connection, NULL, sizeof(CONNECTION4TAG_ADD_INFO_IN), (void **)&dataIn ) ;
      u4ncpy( dataIn->indexFileName, i4->indexFile->accessName, strlen( i4->indexFile->accessName ) + 1 ) ;
      for( numTags = 0 ; tagData[numTags].name != 0; numTags++ )
      {
         ;
      }
      dataIn->numTags = htons5(numTags) ;
      // AS May 6/02 - Added for clipper support, need safety since tag files get created
      dataIn->safety = c4->safety ;

      len = 0 ;
      offset = sizeof( CONNECTION4TAG_ADD_INFO_IN ) ;

      for ( j = 0 ; j != numTags ; j++ )
      {
         len = strlen( tagData[j].name ) + 1 ;
         offset += sizeof( CONNECTION4TAG_INFO ) ;
         connection4addData( connection, NULL, sizeof(CONNECTION4TAG_INFO), (void **)&tinfo ) ;
         tinfo->name.offset = htons5(offset);
         len2 = strlen( tagData[j].expression ) + 1 ;
         offset += len ;
         tinfo->expression.offset = htons5(offset) ;
         offset += len2 ;
         if ( tagData[j].filter == 0 )
         {
            len3 = 0 ;
            tinfo->filter.offset = 0 ;
         }
         else
         {
            len3 = strlen( tagData[j].filter ) + 1 ;
            tinfo->filter.offset = htons5(offset) ;
         }
         offset += len3 ;
         tinfo->unique = htons5(tagData[j].unique) ;
         tinfo->descending = htons5(tagData[j].descending) ;
         connection4addData( connection, tagData[j].name, len, NULL ) ;
         connection4addData( connection, tagData[j].expression, len2, NULL ) ;
         if ( len3 != 0 )
            connection4addData( connection, tagData[j].filter, len3, NULL ) ;
      }

      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E91717 ) ;
      if ( connection4type( connection ) != CON4ADD_TAG )
         return error4( c4, e4connection, E81705 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E91717 ) ;

      if ( rc == r4unique )
      {
         // AS Apr 23/04 - also set the error code in this case...
         error4set( c4, rc ) ;
         return r4unique ;
      }

      if ( connection4len( connection ) != sizeof( CONNECTION4TAG_ADD_INFO_OUT ) )
         return error4( c4, e4packetLen, E91717 ) ;

      dataOut = ( CONNECTION4TAG_ADD_INFO_OUT *)connection4data( connection ) ;

      if ( dataOut->lockedDatafile )
      {
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         d4->dataFile->fileLockLockId = data4lockId( d4 ) ;
         d4->dataFile->fileLockServerId = data4serverId( d4 ) ;
      }

      for ( j = 0 ; tagData[j].name != 0 ; j++ )
      {
         tag = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
         if ( tag == 0 )
            return e4memory ;
         tag->tagFile = (TAG4FILE *)mem4allocZero( c4->tagFileMemory ) ;
         if ( tag->tagFile == 0 )
            return e4memory ;
         tag->index = i4 ;
         tag->tagFile->codeBase = c4 ;
         tag->tagFile->indexFile = i4->indexFile ;

         u4ncpy( tag->tagFile->alias, tagData[j].name, sizeof(tag->tagFile->alias) - 1 ) ;
         c4upper( tag->tagFile->alias ) ;

         if ( tagData[j].expression == 0 )
            return error4describe( c4, e4tagInfo, E85303, tagData[j].name, tagData[j].expression, 0 ) ;

         if ( error4code( c4 ) < 0 )
            break ;
         l4add( &i4->indexFile->tags, tag->tagFile ) ;
         l4add( &i4->tags, tag ) ;
         t4uniqueSetLow( tag, tagData[j].unique, 0 ) ;
      }

      // AS Apr 23/04 - actually need to return the rc code (e.g. r4uniqueContinue) - t4index2.c
      if ( rc == r4uniqueContinue )
         error4set( c4, rc ) ;
      return rc ;
   }
#endif /* #if defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT )
   #include "r4reinde.h"
#endif



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) && defined( S4MDX )
   #define GARBAGE_LEN 518

   static int index4fileTagHeadersWriteSp( INDEX4FILE *i4, DATA4 *d4 )
   {
      // goes through the index4file and writes out the tags based on those in the links...
      /* First, calculate the T4DESC.leftChld, T4DESC.rightChld values, T4DESC.parent values */

      int higher[49], lower[49], parent[49] ;
      c4memset( (void *)higher, 0, sizeof(higher) ) ;
      c4memset( (void *)lower,  0, sizeof(lower) ) ;
      c4memset( (void *)parent, 0, sizeof(parent) ) ;

      TAG4FILE *tagOn = (TAG4FILE *)l4first( &i4->tags ) ;
      if ( tagOn != 0 )
      {
         int nTag = 1 ;

         for ( ;; )
         {
            tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn) ;
            if ( tagOn == 0 )
               break ;
            nTag++ ;
            int iTag = 1 ;
            for (;;)
            {
               TAG4FILE *tagPtr = index4fileFindITag( i4, iTag ) ;
               #ifdef E4MISC
                  if ( tagPtr == 0 || iTag < 0 || iTag >= 48 || nTag > 48 )
                     return error4( tagOn->codeBase, e4result, E92102 ) ;
               #endif  /* E4MISC */
               if ( u4memcmp( tagOn->alias, tagPtr->alias, sizeof(tagOn->alias)) < 0)
               {
                  if ( lower[iTag] == 0 )
                  {
                     lower[iTag] = nTag ;
                     parent[nTag] = iTag ;
                     break ;
                  }
                  else
                     iTag = lower[iTag] ;
               }
               else
               {
                  if ( higher[iTag] == 0 )
                  {
                     higher[iTag] = nTag ;
                     parent[nTag] = iTag ;
                     break ;
                  }
                  else
                     iTag = higher[iTag] ;
               }
            }
         }
      }

      /* Now write the headers */
      FILE4LONG pos ;
      file4longAssign( pos, 0, 0 ) ;
      FILE4SEQ_WRITE seqwrite ;
      char buffer[2048] ;
      file4seqWriteInitLow( &seqwrite, &i4->file, pos, buffer, sizeof( buffer ) ) ;

      u4yymmdd( i4->header.yymmdd ) ;

      #ifdef S4BYTE_SWAP
         I4HEADER swapHeader ;
         memcpy( (void *)&swapHeader, (void *)&i4->header, sizeof(I4HEADER) ) ;

         swapHeader.blockChunks = x4reverseShort( (void *)&swapHeader.blockChunks ) ;
         swapHeader.blockRw = x4reverseShort( (void *)&swapHeader.blockRw ) ;
         swapHeader.slotSize = x4reverseShort( (void *)&swapHeader.slotSize ) ;
         swapHeader.numTags = x4reverseShort( (void *)&swapHeader.numTags ) ;
         swapHeader.eof = x4reverseLong( (void *)&swapHeader.eof ) ;
         swapHeader.freeList = x4reverseLong( (void *)&swapHeader.freeList ) ;

         file4seqWrite( &seqwrite, &swapHeader, sizeof(I4HEADER) ) ;
      #else
         file4seqWrite( &seqwrite, &i4->header, sizeof(I4HEADER) ) ;
      #endif  /* S4BYTE_SWAP */

      file4seqWriteRepeat( &seqwrite, 512 - sizeof( I4HEADER ) + 17, 0 ) ;
      /* There is a 0x01 on byte 17 of the first 32 bytes. */
      file4seqWrite( &seqwrite, "\001", 1 ) ;
      file4seqWriteRepeat( &seqwrite, 14, 0 ) ;

      tagOn = (TAG4FILE *)l4first( &i4->tags ) ;

      for ( int iTag = 0; iTag < 47; iTag++ )
      {
         T4DESC tagInfo ;
         c4memset( (void *)&tagInfo, 0, sizeof(tagInfo) ) ;

         int nTags = l4numNodes( &i4->tags ) ;
         if ( iTag < nTags )
         {
            if ( tagOn == 0 )  /* means that there are less tags in list than according to count in index header - corrupt */
               return error4( i4->codeBase, e4index, E92102 ) ;
            if ( b4node( tagOn->headerOffset ) == 0 )
               b4nodeSetFromFilePosition( i4, &tagOn->headerOffset, file4lenLow( &i4->file ) ) ;

            b4nodeAssignNode( &tagInfo.headerPos, tagOn->headerOffset ) ;

            c4memcpy( (void *)tagInfo.tag, tagOn->alias, sizeof(tagInfo.tag) ) ;

            tagInfo.indexType = tagOn->header.type ;

            #ifdef S4BYTE_SWAP
               tagInfo.headerPos = x4reverseLong( (void *)&tagInfo.headerPos ) ;
               tagInfo.x1000 = 0x0010 ;
            #else
               tagInfo.x1000 = 0x1000 ;
            #endif  /* S4BYTE_SWAP */

            tagInfo.x2 = 2 ;
            tagInfo.leftChld = (char) lower[iTag+1] ;
            tagInfo.rightChld = (char) higher[iTag+1] ;
            tagInfo.parent = (char) parent[iTag+1] ;

            if ( i4->header.isProduction )
            {
               int saveCode = i4->codeBase->errFieldName ;
               i4->codeBase->errFieldName = 0 ;
               int jField = d4fieldNumber( d4, tagOn->expr->source ) ;
               i4->codeBase->errFieldName = saveCode ;
               if ( jField > 0 )
               {
                  file4longAssign( pos, ( jField + 1 ) * sizeof( FIELD4IMAGE ) - 1, 0 ) ;
                  file4writeInternal( &d4->dataFile->file, pos, "\001", 1 ) ;
               }
            }
            tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;
         }
         if ( file4seqWrite( &seqwrite, &tagInfo, sizeof(T4DESC)) < 0 )
            return -1 ;
      }

      return file4seqWriteFlush( &seqwrite ) ;

      return 0 ;
   }
#endif /* #if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) && defined( S4MDX ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT )
   static int i4addOneTag( INDEX4 *i4, const TAG4INFO *tagData )
   {
      #ifdef S4FOX
         int keysMax, exprType ;
         B4BLOCK *b4 ;
         int totLen, exprHdrLen ;
         #ifdef S4BYTE_SWAP
            T4HEADER swapTagHeader ;
            char *swapPtr ;
            S4LONG longVal ;
            short shortVal ;
            int i ;
         #endif
         #ifdef S4DATA_ALIGN
            unsigned int size, delta ;
         #endif
      #endif
      #ifdef S4MDX
         #ifdef S4BYTE_SWAP
            T4HEADER swapTagHeader ;
         #endif  /* S4BYTE_SWAP */
         int len ;
      #endif

      #ifdef E4PARM_LOW
         if ( i4 == 0 || tagData == 0 )
            return error4( 0, e4parm_null, E91716 ) ;
      #endif

      if ( error4code( i4->codeBase ) < 0 )
         return e4codeBase ;

      DATA4 *d4 = i4->data ;
      CODE4 *c4 = d4->codeBase ;

      INDEX4FILE *i4file = i4->indexFile ;

      R4REINDEX reindex ;
      int rc = r4reindexInit( &reindex, i4, i4file ) ;
      if ( rc < 0 )
         return error4stack( c4, (short)rc, E91716 ) ;

      TAG4 *tagPtr = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
      FILE4SEQ_WRITE *seqWrite = &reindex.seqwrite ;

      TAG4FILE *tagFile ;

      if ( tagPtr == 0 )
         rc = error4stack( c4, e4memory, E91716 ) ;
      else
      {
         c4memset( (void *)tagPtr, 0, sizeof( TAG4 ) ) ;
         tagPtr->tagFile = (TAG4FILE *)mem4allocZero( c4->tagFileMemory ) ;
         tagFile = tagPtr->tagFile ;
         tagPtr->index = i4 ;
         if ( tagFile == 0 )
            rc = error4stack( c4, e4memory, E91716 ) ;
         else
         {
            c4memset( (void *)tagFile, 0, sizeof( TAG4FILE ) ) ;
            tagFile->codeBase = c4 ;
            tagFile->indexFile = i4file ;
            u4ncpy( tagFile->alias, tagData[0].name, sizeof(tagFile->alias) ) ;
            c4upper( tagFile->alias ) ;
         }
      }

      // AS Mar 26/04 - improved return code handling
      int saveRc = 0 ;

      #ifdef S4MDX
         /* AS 01/06/99 - increment numTags always, if an error, then always decrement in clean up, and don't need ifs */
         i4file->header.numTags++ ;

         for( ;; )  // use this simply to handle error-handling...
         {
            if( i4file->header.numTags == 48 )  /* already at max */
            {
               rc = error4( c4, e4tagInfo, E85306 ) ;
               break ;
            }

            tagFile->header.typeCode = 0x10 ;
            if ( tagData[0].unique )
            {
               tagFile->header.typeCode += 0x40 ;
               tagFile->header.unique = 0x4000 ;
               tagPtr->errUnique = tagData[0].unique ;

               // AS Mar 26/04 - normalize and always check this
               if ( tagData[0].unique != e4unique && tagData[0].unique != r4unique &&
                    tagData[0].unique != r4uniqueContinue )
               {
                  rc = error4describe( c4, e4tagInfo, E85301, tagData[0].name, 0, 0 ) ;
                  break ;
               }
            }

            if ( tagData[0].descending)
            {
               tagFile->header.typeCode += 0x08 ;
               #ifdef E4PARM_HIGH
                  if ( tagData[0].descending != r4descending )
                  {
                     rc = error4( c4, e4info, E81712 ) ;
                     break ;
                  }
               #endif
            }

            if ( tagData[0].expression == 0 )
            {
               rc = error4describe( c4, e4tagInfo, E85303, tagData[0].name, tagData[0].expression, 0 ) ;
               break ;
            }

            tagFile->expr = expr4parseLow( d4, tagData[0].expression, tagFile ) ;
            if( tagFile->expr == 0 )
            {
               if ( error4code( c4 ) == 0 )
                  rc = error4( c4, e4info, E91716 ) ;
               else
                  rc = error4code( c4 ) ;
               break ;
            }

            if ( tagData[0].filter != 0 )
               if ( *(tagData[0].filter) != '\0' )
               {
                  tagFile->filter = expr4parseLow( d4, tagData[0].filter, tagFile ) ;
                  if ( tagFile->filter == 0 )
                  {
                     if ( error4code( c4 ) == 0 )
                        rc = error4( c4, e4info, E91716 ) ;
                     else
                        rc = error4code( c4 ) ;
                     break ;
                  }
               }

            if ( error4code( c4 ) < 0 )
            {
               rc = error4code( c4 ) ;
               break ;
            }

            l4add( &i4->tags, tagPtr ) ;
            l4add( &i4file->tags, tagFile ) ;

            tagFile->header.keyLen = expr4keyLen( tagFile->expr ) ;
            if ( tagFile->header.keyLen < 0 )
            {
               rc = tagFile->header.keyLen ;
               break ;
            }

            tagFile->header.type = (char)expr4type( tagFile->expr ) ;

            if ( tagFile->header.type == r4dateDoub )
               tagFile->header.type = r4date ;

            if ( tagFile->header.type == r4numDoub )
               tagFile->header.type = r4num ;

            rc = tfile4initSeekConv( tagFile, tagFile->header.type ) ;
            if ( rc != 0 )
               break ;

            tagFile->header.groupLen = tagFile->header.keyLen+ 2*sizeof(S4LONG)-1 ;
            tagFile->header.groupLen-= tagFile->header.groupLen % sizeof(S4LONG) ;

            tagFile->header.keysMax = (reindex.indexFile->header.blockRw - sizeof(short) - 6 - sizeof(S4LONG)) /
               tagFile->header.groupLen;

            if ( tagFile->header.keysMax < reindex.minKeysmax )
               reindex.minKeysmax = tagFile->header.keysMax ;

            tagFile->hasKeys = 0 ;
            tagFile->hadKeys = 1 ;

            rc = r4reindexBlocksAlloc( &reindex ) ;
            if ( rc < 0 )
               break ;

            reindex.nTags = i4file->header.numTags ;

            reindex.lastblockInc = reindex.indexFile->header.blockRw / 512 ;

            reindex.lastblock = file4longGetLo( file4lenLow( &reindex.indexFile->file ) ) / 512 - reindex.lastblockInc ;
            if ( reindex.lastblock == INVALID4BLOCK_ID )
            {
               rc = -1 ;
               break ;
            }

            reindex.tag = tagFile ;

            rc = r4reindexSupplyKeys( &reindex ) ;
            if ( rc < 0 )
               break ;

            rc = r4reindexWriteKeys( &reindex, t4unique( tagPtr ) ) ;
            // AS Mar 26/04 - r4uniqueContinue is ok here
            if ( rc == r4uniqueContinue )
            {
               saveRc = r4uniqueContinue ;
               rc = 0 ;
            }
            if ( rc != 0 )
               break ;

            // update the binary tree of tag names...
            rc = index4fileTagHeadersWriteSp( reindex.indexFile, i4->data ) ;
            if ( rc < 0 )
               break ;

            i4file->header.eof = reindex.lastblock + reindex.lastblockInc + 2 ;
            // AS 09/25/00 was not flushing this new 'eof' to disk, so was causing problems if re-read later
            i4file->changed =-1 ;
            int rc = index4updateHeader( i4file ) ;
            if ( rc < 0 )
               break ;

            rc = file4seqWriteFlush( seqWrite ) ;
            if ( rc < 0 )
               break ;

            FILE4LONG pos ;
            b4nodeGetFilePosition( i4file, tagFile->headerOffset, &pos ) ;
            file4seqWriteInitLow( seqWrite, &i4file->file, pos, reindex.buffer, reindex.bufferLen ) ;

            #ifdef S4BYTE_SWAP
               memcpy( (void *)&swapTagHeader, (void *)&tagFile->header, sizeof(T4HEADER) ) ;

               swapTagHeader.root = x4reverseLong( (void *)&swapTagHeader.root ) ;
               swapTagHeader.keyLen = x4reverseShort( (void *)&swapTagHeader.keyLen ) ;
               swapTagHeader.keysMax = x4reverseShort( (void *)&swapTagHeader.keysMax ) ;
               swapTagHeader.groupLen = x4reverseShort( (void *)&swapTagHeader.groupLen ) ;
               swapTagHeader.isDate = x4reverseShort( (void *)&swapTagHeader.isDate ) ;
               swapTagHeader.unique = x4reverseShort( (void *)&swapTagHeader.unique ) ;

               rc = file4seqWrite( seqWrite, &swapTagHeader, sizeof(T4HEADER) ) ;
               if ( rc < 0 )
                  break ;
            #else
               rc = file4seqWrite( seqWrite, &tagFile->header, sizeof(T4HEADER) ) ;
               if ( rc < 0 )
                  break ;
            #endif  /* S4BYTE_SWAP */

            const char *ptr = tagFile->expr->source ;
            len = c4strlen( ptr ) ;

            rc = file4seqWrite( seqWrite, ptr, len ) ;
            if ( rc < 0 )
               break ;

            rc = file4seqWriteRepeat( seqWrite, 221-len, 0 ) ;
            if ( rc < 0 )
               break ;

            if( tagFile->filter != 0 )
            {
               rc = file4seqWriteRepeat( seqWrite, 1, 1 ) ;
               if ( rc < 0 )
                  break ;

               if ( tagFile->hasKeys )
                  rc = file4seqWriteRepeat( seqWrite, 1, 1 ) ;
               else
                  rc = file4seqWriteRepeat( seqWrite, 1, 0 ) ;
               if ( rc < 0 )
                  break ;
            }
            else
            {
               rc = file4seqWriteRepeat( seqWrite, 2, 0 ) ;
               if ( rc < 0 )
                  break ;
            }

            /* write extra space up to filter write point */
            rc = file4seqWriteRepeat( seqWrite, GARBAGE_LEN - 3, 0 ) ;
            if ( rc < 0 )
               break ;

            if ( tagFile->filter == 0 )
               len = 0 ;
            else
            {
               ptr = tagFile->filter->source ;
               len = c4strlen(ptr) ;
               rc = file4seqWrite( seqWrite, ptr, len ) ;
               if ( rc < 0 )
                  break ;
            }

            rc = file4seqWriteRepeat( seqWrite, reindex.blocklen - GARBAGE_LEN - len - 220 - sizeof(tagFile->header), 0 );
            if ( rc < 0 )
               break ;
            rc = file4seqWriteFlush(seqWrite ) ;
            if ( rc == 0 )
            {
               file4longAssign( pos, i4file->header.eof * 512, 0 ) ;
               rc = file4lenSetLow( &i4file->file, pos ) ;
            }
            break ;  // end loop for error handling
         }

         if ( rc < 0 )
            file4seqWriteFlush(seqWrite ) ;
      #endif

      #ifdef S4FOX
         for ( ; rc == 0 ; )
         {
            tagFile->header.typeCode  = 0x60 ;  /* compact */
            if ( tagData[0].unique )
            {
               tagFile->header.typeCode += 0x01 ;
               tagPtr->errUnique = tagData[0].unique ;

               // AS Mar 26/04 - normalize and always check this
               if ( tagData[0].unique != e4unique && tagData[0].unique != r4unique &&
                    tagData[0].unique != r4uniqueContinue && tagData[0].unique != r4candidate &&
                    tagData[0].unique != e4candidate )
               {
                  rc = error4describe( c4, e4tagInfo, E85301, tagData[0].name, 0, 0 ) ;
                  break ;
               }
            }
            if ( tagData[0].descending)
            {
               tagFile->header.descending = 1 ;
               #ifdef E4PARM_HIGH
                  if ( tagData[0].descending != r4descending )
                  {
                     rc = error4( c4, e4info, E81712 ) ;
                     break ;
                  }
               #endif
            }

            if ( tagData[0].expression == 0 )
            {
               rc = error4describe( c4, e4tagInfo, E85303, tagData[0].name, tagData[0].expression, 0 ) ;
               break ;
            }

            tagFile->expr = expr4parseLow( d4, tagData[0].expression, tagFile ) ;
            if ( tagFile->expr == 0 )
            {
               if ( error4code( c4 ) == 0 )
                  rc = error4( c4, e4info, E91716 ) ;
               else
                  rc = error4code( c4 ) ;
               break ;
            }

            // AS 10/01/99 --> typeCode of 0x02 for nullable...
            if ( expr4nullLow( tagFile->expr, 0 ) )  // means nullable expression...
               tagFile->header.typeCode += 0x02 ;

            tagFile->header.exprLen = c4strlen( tagFile->expr->source ) + 1 ;

            Collate4name collateNameForTag ;
            if ( tagFile->isUnicode )
               collateNameForTag = c4->collateNameUnicode ;
            else
               collateNameForTag = c4->collateName ;

            assert5( tagFile->expr != 0 ) ;  // required for collation determination

            /* set the tag's collating sequence */
            if ( tfile4setCollatingSeq( tagFile, collateNameForTag, 1 ) < 0 )
            {
               rc = error4( c4, e4index, E84907 ) ;
               break ;
            }

            // AS Dec 5/05 - fix for OFF_MEMO
            #ifndef S4OFF_MEMO
               // AS Feb 11/04 - need to know if an expression includes a memo field
               if ( tagFile->expr->hasMemoField == 1 && tagFile->collateName != collate4none )
               {
                  // assume/verify it is only a simple field
                  // LY Jan 7/05 : replace direct comparison of COLLATE4.keySizeCharPerCharAdd with collate4simpleMapping()
                  // short numExtraCharsPerChar = collation4get( tagFile->collateName )->keySizeCharPerCharAdd ;
                  // if ( numExtraCharsPerChar > 0 )  // need to reduce the memo key size down for collation support
                  COLLATE4 *collate = collation4get( tagFile->collateName ) ;
                  if ( !collate4simpleMapping( collate ) )
                  {
                     if ( tagFile->expr->info[0].len >= I4MAX_KEY_SIZE_COMPATIBLE - 1 )
                     {
                        short numExtraCharsPerChar = collate->keySizeCharPerCharAdd ;
                        tagFile->expr->info[0].len /= (1 + numExtraCharsPerChar) ;
                        tagFile->expr->len /= (1 + numExtraCharsPerChar) ;
                     }
                  }
               }
            #endif

            // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
            // if ( tfile4setCodePage( tagFile, i4->data->codePage ) < 0 )
            // {
            //    rc = error4( c4, e4index, E91642 ) ;
            //    break ;
            // }

            if ( tagData[0].filter != 0 )
            {
               if ( *( tagData[0].filter ) != '\0' )
               {
                  tagFile->header.typeCode += 0x08 ;
                  tagFile->filter = expr4parseLow( d4, tagData[0].filter, tagFile ) ;
                  if ( tagFile->filter == 0 )
                  {
                     if ( error4code( c4 ) == 0 )
                        rc = error4( c4, e4info, E91716 ) ;
                     else
                        rc = error4code( c4 ) ;
                     break ;
                  }
                  tagFile->header.filterLen = c4strlen( tagFile->filter->source ) ;
               }
            }

            tagFile->header.filterLen++ ;  /* minimum of 1, for the '\0' */
            tagFile->header.filterPos = tagFile->header.exprLen ;

            if ( error4code( c4 ) < 0 || error4code( c4 ) == r4unique )
            {
               rc = error4code( c4 ) ;
               break ;
            }

            reindex.tag = tagFile ;
            reindex.nBlocksUsed = 0 ;

            // we set lastblock to point to last block in file...
            b4nodeSetFromFilePosition( i4file, &reindex.lastblock, file4lenLow( &i4file->file ) ) ;
            b4nodeSubtractBlocks( &reindex.lastblock, i4file, 1 ) ;

            tagFile->header.keyLen = expr4keyLen( tagFile->expr ) ;

            #ifdef S4DATA_ALIGN
               size = (unsigned int)sizeof(S4LONG) + tagFile->header.keyLen ;
               delta = sizeof(void *) - size % sizeof(void *);
               tagFile->builtKeyMemory = mem4create( c4, 3, size + delta, 2, 0 ) ;
            #else
               tagFile->builtKeyMemory = mem4create( c4, 3, (unsigned int)sizeof(S4LONG) + tagFile->header.keyLen + 1, 2, 0 ) ;
            #endif

            exprType = expr4type( tagFile->expr ) ;
            if ( exprType < 0 )
            {
               rc = exprType ;
               break ;
            }

            tfile4initSeekConv( tagFile, exprType ) ;

            if ( tagFile->header.keyLen < 0 )
            {
               rc = tagFile->header.keyLen ;
               break ;
            }

            keysMax = ( i4blockSize( reindex.indexFile ) - sizeof(B4STD_HEADER) ) / ( tagFile->header.keyLen + 2*sizeof(S4LONG) ) ;

            if ( keysMax < reindex.minKeysmax )
               reindex.minKeysmax = keysMax ;

            rc = r4reindexBlocksAlloc( &reindex ) ;
            if ( rc < 0 )
               break ;

            rc = r4reindexSupplyKeys( &reindex ) ;
            if ( rc < 0 )
               break ;

            // AS Mar 26/04 - r4unique settings must be considered here as well...
            rc = r4reindexWriteKeys( &reindex, t4unique( tagPtr ) ) ;
            // AS Mar 26/04 - r4uniqueContinue is ok here
            if ( rc == r4uniqueContinue )
            {
               saveRc = r4uniqueContinue ;
               rc = 0 ;
            }
            if ( rc != 0 )
               break ;

            #ifdef S4BYTE_SWAP
               memcpy( (void *)&swapTagHeader, (void *)&tagFile->header, sizeof(T4HEADER) ) ;

               swapTagHeader.root.node = x4reverseLong( (long *)&swapTagHeader.root ) ;
               swapTagHeader.freeList.node = x4reverseLong( (long *)&swapTagHeader.freeList ) ;
               /* version is stored in non-intel format */
               swapTagHeader.keyLen = x4reverseShort( (void *)&swapTagHeader.keyLen ) ;
               swapTagHeader.descending = x4reverseShort( (void *)&swapTagHeader.descending ) ;
               swapTagHeader.filterPos = x4reverseShort( (void *)&swapTagHeader.filterPos ) ;
               swapTagHeader.filterLen = x4reverseShort( (void *)&swapTagHeader.filterLen ) ;
               swapTagHeader.exprPos = x4reverseShort( (void *)&swapTagHeader.exprPos ) ;
               swapTagHeader.exprLen = x4reverseShort( (void *)&swapTagHeader.exprLen ) ;

               rc = file4seqWrite( seqWrite, &swapTagHeader, LEN4HEADER_WR ) ;
            #else
               tagFile->header.version = x4reverseShort((void *)&tagFile->header.version) ;
               rc = file4seqWrite( seqWrite, &tagFile->header, LEN4HEADER_WR ) ;
               tagFile->header.version = x4reverseShort((void *)&tagFile->header.version) ;
            #endif

            if ( rc < 0 )
               break ;

            rc = file4seqWriteRepeat( seqWrite, 478L, 0 ) ;
            if ( rc < 0 )
               break ;
            rc = file4seqWrite( seqWrite, &tagFile->header.sortSeq, 8 ) ;

            if ( rc < 0 )
               break ;

            exprHdrLen = 5*sizeof(short) ;

            #ifdef S4BYTE_SWAP
               rc = file4seqWrite( seqWrite, &swapTagHeader.descending, exprHdrLen ) ;
            #else
               rc = file4seqWrite( seqWrite, &tagFile->header.descending, (unsigned int)exprHdrLen ) ;
            #endif
            if ( rc < 0 )
               break ;

            const char *ptr = tagFile->expr->source ;
            totLen = tagFile->header.exprLen ;
            rc = file4seqWrite( seqWrite, ptr, (unsigned int)tagFile->header.exprLen ) ;
            if ( rc < 0 )
               break ;

            if ( tagFile->filter != 0 )
            {
               ptr = tagFile->filter->source ;
               rc = file4seqWrite( seqWrite, ptr, (unsigned int)tagFile->header.filterLen ) ;
               if ( rc < 0 )
                  break ;
               totLen += tagFile->header.filterLen ;
            }
            // AS Mar 29/04 - need to adjust for variable block sizes
            // rc = file4seqWriteRepeat( seqWrite, (long)i4blockSize( reindex.indexFile ) - totLen, 0 );
            rc = file4seqWriteRepeat( seqWrite, (S4LONG)(B4BLOCK_SIZE_INTERNAL - totLen), 0 );
            if ( rc < 0 )
               break ;

            rc = file4seqWriteFlush(seqWrite ) ;
            if ( rc < 0 )
               break ;

            /* now must fix the right node branches for all blocks by moving leftwards */
            if ( rc == 0 )
            {
               for( tfile4rlBottom( tagFile ) ; tagFile->blocks.lastNode ; tfile4up( tagFile ) )
               {
                  b4 = tfile4block( tagFile ) ;
                  B4NODE goTo = b4->header.leftNode ;

                  while ( b4nodeValid( goTo ) && rc == 0 )
                  {
                     B4NODE rNode = b4->fileBlock ;
                     if ( b4->changed )
                     {
                        rc = b4flush( b4 ) ;
                        if ( rc < 0 )
                           break ;
                     }

                     FILE4LONG pos ;
                     b4nodeGetFilePosition( tagFile->indexFile, goTo, &pos ) ;
                     rc = file4readAllInternal( &tagFile->indexFile->file, pos, &b4->header, i4blockSize( reindex.indexFile ) ) ;
                     if ( rc < 0 )
                        break ;

                     #ifdef S4BYTE_SWAP
                        b4->header.nodeAttribute = x4reverseShort( (void *)&b4->header.nodeAttribute ) ;
                        b4->header.nKeys = x4reverseShort( (void *)&b4->header.nKeys ) ;
                        b4->header.leftNode.node = x4reverseLong( (long *)&b4->header.leftNode ) ;
                        b4->header.rightNode.node = x4reverseLong( (long *)&b4->header.rightNode ) ;

                        /* if b4 is a leaf */
                        // AS Feb 3/04 - Fox 8.0 support, if the '2' bit is on, it is a leaf block, not if >= 2
                        if (b4->header.nodeAttribute  & 0x02 )
                        {
                           b4->nodeHdr.freeSpace = x4reverseShort( (void *)&b4->nodeHdr.freeSpace ) ;
                           longVal = x4reverseLong( (void *)&b4->nodeHdr.recNumMask[0] ) ;
                           memcpy( (void *)&b4->nodeHdr.recNumMask[0], (void *)&longVal, sizeof(S4LONG) ) ;
                        }
                        else   /* if b4 is a branch */
                        {
                           shortVal = b4->tag->header.keyLen + sizeof(S4LONG) ;
                           /* position swapPtr to end of first key expression */
                           swapPtr = (char *)&b4->nodeHdr.freeSpace + b4->tag->header.keyLen ;

                           /* move through all B4KEY's to swap 'long's */
                           for ( i = 0 ; i < (int)b4numKeys( b4 ) ; i++ )
                           {
                              longVal = x4reverseLong((void *)swapPtr ) ;
                              memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                              swapPtr += sizeof(S4LONG) ;
                              longVal = x4reverseLong((void *)swapPtr ) ;
                              memcpy( swapPtr, (void *) &longVal, sizeof(S4LONG) ) ;
                              swapPtr += shortVal ;
                           }
                        }
                     #endif

                     b4->fileBlock = goTo ;
                     assert5( b4nodeValid( b4->fileBlock ) ) ;
                     if ( b4nodesNotEqual( b4->header.rightNode, rNode ) ) /* if a bad value */
                     {
                        b4->header.rightNode = rNode ;
                        b4->changed = 1 ;
                     }
                     goTo = b4->header.leftNode ;
                  }

                  if ( rc < 0 )
                     break ;

                  b4->builtOn = -1 ;
                  b4top( b4 ) ;
               }
            }

            l4add( &i4->tags, tagPtr ) ;
            l4add( &i4file->tags, tagFile ) ;

            b4nodeAssignNode( &tagFile->headerOffset, reindex.lastblock ) ;

            b4nodeAddBlocks( &tagFile->headerOffset, i4file, 1 ) ;

            // AS 07/06/99 -- need to have alias with trailing blanks...
            unsigned char tagName[LEN4TAG_ALIAS + 1] ;
            tagName[LEN4TAG_ALIAS] = '\0' ;
            c4memset( tagName, ' ', LEN4TAG_ALIAS ) ;
            int len = c4strlen( tfile4alias( tagFile ) ) ;
            c4memcpy( tagName, tfile4alias( tagFile ), (unsigned int)len ) ;

            // AS Dec 8/04 - assign the eof first becuase this tfile4add below may actually want to extend the file
            // if there are many tags and we need to split the tag of tags tag.
            if ( rc == 0 )
            {
               FILE4LONG pos = file4lenLow( &i4file->file ) ;
               b4nodeSetFromFilePosition( i4file, &i4file->eof, pos ) ;
            }

            rc = tfile4add( i4file->tagIndex, tagName, b4node( tagFile->headerOffset ), t4unique( tagPtr ) ) ;
            if ( rc == 0 )
            {
               // AS Mar 29/04 - take multiplier into affect - just us the actual physical file length instead of creating the value
               FILE4LONG pos = file4lenLow( &i4file->file ) ;

               // B4NODE numNodes = reindex.lastblock ;
               // b4nodeGetFilePosition( i4file, reindex.lastblock, &pos ) ;
               // file4longAdd( &pos, 3 * B4BLOCK_SIZE_INTERNAL ) ;
               b4nodeSetFromFilePosition( i4file, &i4file->eof, pos ) ;
               // rc = file4lenSetLow( &i4file->file, pos ) ;
            }
            break ;
         }

         if ( rc <= 0 )
            file4seqWriteFlush(seqWrite ) ;
      #endif

      r4reindexFree( &reindex ) ;
      if ( rc != 0 || error4code( i4->codeBase ) < 0 )
      {
         #ifdef S4MDX
            /* AS 01/06/98 - clean-up, always incremented, so always decrement. */
            i4file->header.numTags-- ;
         #endif
         if ( tagPtr != 0 )
         {
            if ( tagPtr->link.n != 0 )  /* must remove from link */
               l4remove( &i4->tags, tagPtr ) ;
            if ( tagFile != 0 )
            {
               if ( tagFile->link.n != 0 )  /* must remove from link */
                  l4remove(  &i4file->tags, tagFile  ) ;
               // AS 04/23/99 added freeing up of expressions and filters
               if ( tagFile->expr != 0 )
               {
                  expr4free( tagFile->expr ) ;
                  tagFile->expr = 0 ;
               }
               if ( tagFile->filter != 0 )
               {
                  expr4free( tagFile->filter ) ;
                  tagFile->filter = 0 ;
               }
               mem4free( c4->tagFileMemory, tagFile ) ;
            }
            mem4free( c4->tagMemory, tagPtr ) ;
         }
         return rc ;
      }

      /* ensure that the tagIndex addition gets flushed to disk, otherwise problems may ensue
         because we generally don't flush changes to that tag */
      rc = index4update( i4file ) ;
      // AS Mar 26/04 - improved return code handling
      if ( rc != 0 )
         return rc ;

      return saveRc ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT )
   static int i4tagAssociate( INDEX4 *i4 )
   {
      /* adds TAG4 entries to all other i4 structures referring to the same index file */
      #ifdef E4PARM_LOW
         if ( i4 == 0 )
            return error4( 0, e4parm_null, E91718 ) ;
      #endif

      CODE4 *c4 = i4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      LIST4 *list ;
      #ifdef S4SERVER
         server4clientListReserve( c4->server ) ;
         SERVER4CLIENT *client ;
         for( client = 0 ;; )
         {
            client = server4clientGetNext( c4->server, client ) ;
            if ( client == 0 )
               break ;
            list = tran4dataList( &client->trans ) ;
      #else
         list = tran4dataList( &c4->c4trans.trans ) ;
      #endif

         for( DATA4 *dataOn = 0 ;; )
         {
            dataOn = (DATA4 *)l4next( list, dataOn ) ;
            if ( dataOn == 0 )
               break ;
            for( INDEX4 *indexOn = 0 ;; )
            {
               indexOn = (INDEX4 *)l4next( &dataOn->indexes, indexOn ) ;
               if ( indexOn == 0 || indexOn == i4 )
                  break ;
               if ( indexOn->indexFile == i4->indexFile )
               {
                  for ( TAG4 *tagOn = 0 ;; )
                  {
                     tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
                     if ( tagOn == 0 )
                        break ;
                     TAG4 *oldTag = 0 ;
                     for ( ;; )
                     {
                        oldTag = (TAG4 *)l4next( &indexOn->tags, oldTag ) ;
                        if ( oldTag == 0 )
                           break ;
                        if ( oldTag->tagFile == tagOn->tagFile )
                           break ;
                     }
                     if ( oldTag == 0 )
                     {
                        TAG4 *newTag = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
                        if ( newTag == 0 )
                        {
                           #ifdef S4SERVER
                              server4clientListRelease( c4->server ) ;
                           #endif
                           return error4stack( c4, e4memory, E91718 ) ;
                        }
                        newTag->index = indexOn ;
                        newTag->tagFile = tagOn->tagFile ;
                        #ifdef S4SERVER
                           newTag->errUnique = t4unique( tagOn ) ;
                        #endif
                        l4add( &indexOn->tags, newTag ) ;
                     }
                  }
               }
            }
         }
      #ifdef S4SERVER
         }
         server4clientListRelease( c4->server ) ;
      #endif

      return 0 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && defined( S4CLIPPER ) && !defined( S4CLIENT )
   int S4FUNCTION i4tagAdd( INDEX4 *i4, const TAG4INFO *tagDataIn )
   {
      // added for ODBC - just creates a tag and adds entry to group file
      #ifdef E4PARM_HIGH
         if ( i4 == 0 || tagDataIn == 0 )
            return error4( 0, e4parmNull, E91718 ) ;
      #endif
      // AS Mar 29/04 - check integrity of i4 as well
      #ifdef E4PARM_LOW
         if ( i4->data == 0 || i4->data->codeBase == 0 )
            return error4( 0, e4parm, E91718 ) ;
      #endif
      assert5( i4 != 0 && tagDataIn != 0 ) ;

      // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
      // AS Jun 12/02 - Added support for re-use of deleted records if a tag name called 'DEL4REUSE' is created.
      // TAG4INFO reuseTag[] =
      // {
      //    { DEL4REUSE_TAG_NAME, "RECNO()", "DELETED()",0,0},
      //    {0,0,0,0,0}
      // } ;

      CODE4 *c4 = i4->data->codeBase ;

      // AS Jun 2/03 - Added recording if data file has a tag with an expression referring to a memo field
      i4->data->hasMemoExpr = r4uninitializedMemoTagExpression ;  // just reset, it gets set automatically if required

      const TAG4INFO *tagData = 0 ;
      // AS 01/23/01 - tagData is an array...
      // AS May 25/07 - handle r4uniqueContinue...
      int saveRc = 0 ;

      for( tagData = tagDataIn ;; tagData++)
      {
         if ( tagData->name == 0 )
         {
            error4set( c4, saveRc ) ;
            return saveRc ;
         }
         TAG4 *tag = 0 ;
         // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
         // if ( stricmp( tagData->name, DEL4REUSE_TAG_NAME ) == 0 )
         // {
         //    tag = t4create( i4->data, reuseTag, i4, 0 ) ;
         //    if ( tag != 0 )
         //       i4->data->dataFile->appendTag = tag->tagFile ;
         // }
         // else
            tag = t4create( i4->data, tagData, i4, 0 ) ;

         if ( tag == 0 )  // failed to create...
             return error4code( c4 ) ;

         // AS 06/15/99 - need to reindex now because t4create does not reindex when adding to existing index
         int rc = t4reindex( tag ) ;
         // AS May 25/07 - r4uniqueContinue is ok...it means ignore...
         if ( rc == r4uniqueContinue )
            saveRc = rc ;
         else if ( rc != 0 )
         {
            error4set( c4, 0 ) ;
            t4close( tag ) ;
            // AS Mar 29/04 - ensure error code set back before return
            error4set( c4, rc ) ;
            return rc ;
         }

         // open group file and append entry... if tag create succeeded
         rc = i4addEntryToGroupFile( i4, tagData->name ) ;
         if ( rc != 0 )
            return rc ;
      }
   }
#endif /* #if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT )
   int S4FUNCTION i4tagAdd( INDEX4 *i4, const TAG4INFO *tagData )
   {
      /* takes an array of TAG4INFO and adds the input tags to the already existing index file i4 */
      int i, saveRc, rc ;

      #ifdef E4PARM_HIGH
         if ( i4 == 0 || tagData == 0 )
            return error4( 0, e4parm_null, E91717 ) ;
      #endif

      CODE4 *c4 = i4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;
      DATA4 *d4 = i4->data ;

      // AS Jun 2/03 - Added recording if data file has a tag with an expression referring to a memo field
      d4->hasMemoExpr = r4uninitializedMemoTagExpression ;  // just reset, it gets set automatically if required

      #ifndef S4OFF_WRITE
         rc = d4updateRecord( d4, 0, 1 ) ;
         if ( rc )
            return rc ;
      #endif

      if ( d4->readOnly == 1 )
         return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;

      #ifndef S4SINGLE
         rc = d4lockIndex( d4 ) ;
         if ( rc < 0 )
            return rc ;
      #endif  /* S4SINGLE */

      #ifndef S4OFF_OPTIMIZE
         #ifdef S4LOW_MEMORY
            int hasOpt = c4->hasOpt ;
            rc = code4optSuspend( c4 ) ;
            if ( rc < 0 )
               return rc ;
         #endif
      #endif  /* not S4OFF_OPTIMIZE */

      #ifndef S4SINGLE
         #ifdef S4FOX
            if ( i4->indexFile->file.lowAccessMode != OPEN4DENY_RW )
               i4->indexFile->tagIndex->header.version =  i4->indexFile->versionOld + 1 ;
         #endif
      #endif

      // AS Jun 12/02 - Added support for re-use of deleted records if a tag name called 'DEL4REUSE' is created.
      TAG4INFO reuseTag[] =
      {
         { DEL4REUSE_TAG_NAME, "RECNO()", "DELETED()",0,0},
         {0,0,0,0,0}
      } ;

      saveRc = 0 ;
      for ( i = 0 ; tagData[i].name ; i++ )
      {
         #ifdef E4MISC
            int oldTagError = c4->errTagName ;
            c4->errTagName = 0 ;
            if ( d4tag( d4, tagData[i].name ) != 0 )
            {
               saveRc = error4( c4, e4index, E81713 ) ;
               break ;
            }
            c4->errTagName = oldTagError ;
         #endif

         // AS Jun 12/02 - Added support for re-use of deleted records if a tag name called 'DEL4REUSE' is created.
         if ( c4stricmp( tagData[i].name, DEL4REUSE_TAG_NAME ) == 0 )   /* LY 2002/10/04 : replaced stricmp() */
         {
            rc = i4addOneTag( i4, reuseTag ) ;
            if ( rc == 0 )
               i4->data->dataFile->appendTag = dfile4tag( i4->data->dataFile, DEL4REUSE_TAG_NAME ) ;  // it should exist, don't error
         }
         else
            rc = i4addOneTag( i4, &tagData[i] ) ;

         if ( rc != 0 )
         {
            saveRc = rc ;
            // AS Mar 26/04 - this is not necessarily a unique error...
            switch( rc )
            {
               case r4unique:  // need to return -1 for error purposes
                  error4set( c4, r4unique ) ;
                  break ;
               case r4uniqueContinue:  // need to return -1 for error purposes
                  error4set( c4, r4uniqueContinue ) ;
                  break ;
               default:
                  if ( rc < 0 )
                     break ;
                  break ;
            }
         }
      }

      #ifndef S4OFF_OPTIMIZE
         #ifdef S4LOW_MEMORY
            if ( hasOpt )
               code4optRestart( c4 ) ;
         #endif
      #endif
      if ( rc >= 0 )
         rc = i4tagAssociate( i4 ) ;
      if ( rc < 0 && saveRc == 0 )
         saveRc = rc ;

      #ifdef E4ANALYZE
         // verify that the index is ok...
         if ( saveRc == 0 )
         {
            // AS Apr 29/03 - transcations are run-time in odbc now
            // AS Jun 20/03 - was checking wrong flag
            #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
               if ( c4->server->odbcTrans == 1 )  // odbc build, no trans available
            #endif
               #ifndef S4OFF_TRAN
                  // AS Feb 10/03 - If a transaction is ongoing, don't run check since will fail
                  if ( code4transEnabled( c4 ) && code4trans( c4 )->currentTranStatus != r4active )
               #endif
                  {
                     if ( d4check( d4 ) < 0 )
                        return error4( c4, e4remove, E81306 ) ;
                  }
         }
      #endif

      return saveRc ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER )
   int i4indexRemove( INDEX4 *index )
   {
      /* remove all tags/indexes from data file */
      char indexPathName[LEN4PATH] ;
      TAG4 *tag ;

      for ( tag = 0 ; ; )
      {
         tag = (TAG4 *)l4first( &index->tags ) ;
         if ( tag == 0 )
            break ;
         if ( tag->tagFile->userCount != 1
         #ifndef S4OFF_MULTI
            || tag->tagFile->file.lowAccessMode != OPEN4DENY_RW
         #endif
         )
            return error4( tag->index->codeBase, e4remove, E81306 ) ;

         u4nameCurrent( indexPathName, sizeof( indexPathName ), tag->tagFile->file.name ) ;

         t4close( tag ) ;

         u4remove( indexPathName ) ;
      }

      /* i4closeLow( index ) ;  not required because t4close removes index from list */

      return 0 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4CLIPPER )
   int i4indexRemove( INDEX4 *index )
   {
      INDEX4FILE *i4file ;
      char indexPathName[LEN4PATH] ;

      i4file = index->indexFile ;

      if ( i4file->userCount != 1
         #ifndef S4OFF_MULTI
            || i4file->file.lowAccessMode != OPEN4DENY_RW
         #endif
         )
         return error4( index->data->codeBase, e4remove, E81306 ) ;

      u4nameCurrent( indexPathName, sizeof( indexPathName ), i4file->file.name ) ;

      if ( i4file != 0 )  /* means still around, force closure */
      {
         if ( index4isProduction( i4file ) )
         {
            #ifdef S4SERVER
               DATA4FILE *dataFile = index->data->dataFile ;
               // AS 03/06/00 - just mark the bit, don't remove memo
               #ifdef S4FOX
                  dataFile->hasMdxMemo = dataFile->hasMdxMemo & (~0x01) ;  // bit 1 is for index
               #else
                  dataFile->hasMdxMemo = 0 ;
               #endif
               // AS 03/06/00 - was not removing from the datafile, so was getting open error...
               FILE4LONG pos ;
               file4longAssign( pos, ( 4 + sizeof( S4LONG ) + 2 * sizeof( short ) + sizeof( char[16] ) ), 0 ) ;
               #ifdef S4FOX
                  file4writeInternal( &dataFile->file, pos, &(dataFile->hasMdxMemo), sizeof( char ) ) ;
               #else
                  file4writeInternal( &dataFile->file, pos, &(dataFile->hasMdxMemo), sizeof( dataFile->hasMdxMemo ) ) ;
               #endif
            #else
               index->data->dataFile->openMdx = 0 ;
            #endif
         }
         i4closeLow( index ) ;
      }

      u4remove( indexPathName ) ;

      return 0 ;
   }
#endif /*!defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4CLIPPER ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
   int S4FUNCTION d4indexesRemove( DATA4 *data )
   {
      INDEX4 *index ;

      for ( index = 0 ;; )
      {
         index = (INDEX4 *)l4first( &data->indexes ) ;
         if ( index == 0 )
            break ;
         if ( i4indexRemove( index ) < 0 )
            return -1 ;
      }

      return 0 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER )
   static int file4getNextLine( FILE4 *file, int *currentPosition, char *charBuf, int lenBuf )
   {
      // fills in buffer with data from the next line in the file...
      // return 0 if no more data available
      // assumes maximum line length and input buffer of lenBuf

      FILE4LONG readPos ;
      // LY Mar 28/05 : changed for 64-bit FILE4LONG
      // file4longAssignLong( readPos, *currentPosition ) ;
      file4longAssign( readPos, *currentPosition, 0 ) ;
      int len = file4readLow( file, readPos, charBuf, lenBuf ) ;
      int charOn ;

      if ( len == 0 )
         return 0 ;

      for ( charOn = 0 ; charOn < len ; charOn++ )
      {
         switch( charBuf[charOn] )
         {
            case '\r':
            case '\n':
            case '\x1A':
               if ( charOn == 0 )  // means was an empty string, keep on looking
               {
                  // shift the data over, reset the counters, and keep goins
                  c4memmove( charBuf, charBuf+1, len-1 ) ;
                  len-- ;
                  charOn-- ;
                  continue ;
               }
               charBuf[charOn] = 0 ;  // null-end string
               *currentPosition += (charOn + 1) ;  // update end of position
               // also trim out the next eol character if required (i.e. \r\n)
               charOn++ ;
               if ( charOn < lenBuf )
                  if ( charBuf[charOn] == '\r' || charBuf[charOn] == '\n' )
                     (*currentPosition)++ ;
               return 1 ;
         }
      }

      // in this case, possibly just end of file marker (for sure, exactly)
      charBuf[charOn] = 0 ;  // null-end string
      return 1 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER )
   static void file4removePart( FILE4 *file, int startPos, int endPos )
   {
      // function removes parts of file starting and including at startPos, ending and including at endPos
      // assumes 8k buffer is large enough to hold entire contents of file...
      char fileContents[8096] ;

      FILE4LONG readPos ;
      // LY Mar 28/05 : changed for 64-bit FILE4LONG
      // file4longAssignLong( readPos, 0 ) ;
      file4longAssign( readPos, 0, 0 ) ;
      int len = file4readLow( file, readPos, fileContents, sizeof( fileContents ) ) ;
      if ( len < 0 || len == 8096 )  // error or did not all fit in buffer
      {
         error4( file->codeBase, e4info, E91716 ) ;
         return ;
      }

      int lenCopy = len - endPos ;
      assert5( lenCopy >= 0 ) ;
      c4memcpy( fileContents + startPos, fileContents + endPos, lenCopy ) ;
      len -= (endPos-startPos) ;
      FILE4LONG writePos ;
      // LY Mar 28/05 : changed for 64-bit FILE4LONG
      // file4longAssignLong( writePos, 0 ) ;
      // file4longAssign( writePos, len ) ;
      file4longAssign( writePos, 0, 0 ) ;
      file4writeLow( file, writePos, fileContents, len, 1, 1, 0 ) ;
      file4longAssign( writePos, len, 0 ) ;
      file4lenSetLow( file, writePos ) ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER )
   void i4removeTagEntryFromGroupFile( INDEX4 *index, const char *fullPathTagNameIn )
   {
      // this function removes the tag's entry from the group file as part of a 'remove tag'
      // operation

      // note that we have input the full path name, but the actual group file may
      // consist of aliases.  What we need to do is get each name, and then remove
      // the one which is the match.

      char fullPathTagName[LEN4PATH+1] ;
      c4strcpy( fullPathTagName, sizeof(fullPathTagName), fullPathTagNameIn ) ;  // AS Jan 3/06 VS 2005 clipper fix
      c4upper( fullPathTagName ) ;

      // AS 11/01/00 - was not taking data file path into account on accessName which is based from there...
      char groupFileNameWithPath[LEN4PATH] ;
      char dataFilePath[LEN4PATH] ;
      assert5( index->data->dataFile->file.name != 0 ) ;
      u4namePath( dataFilePath, sizeof( dataFilePath ), index->data->dataFile->file.name ) ;
      u4nameCurrentExtended( groupFileNameWithPath, sizeof( groupFileNameWithPath ), index->accessName, dataFilePath ) ;
      u4nameExt( groupFileNameWithPath, sizeof( groupFileNameWithPath ), GROUP4EXT, 1 ) ;
      FILE4 groupFile ;
      // AS Apr 28/04 - use internal version
      if ( file4openInternal( &groupFile, index->data->codeBase, groupFileNameWithPath, 0, OPT4OTHER ) )  // cannot access file, just return
         return ;

      int currentPosition = 0 ;
      char lineBuf[LEN4PATH+1] ;

      for ( ;; )
      {
         int oldPosition = currentPosition ;
         if ( file4getNextLine( &groupFile, &currentPosition, lineBuf, sizeof( lineBuf ) ) == 0 )
            break ;
         // use the path of the .cgp file as the base of the .ntx file path.  This
         // is because the .ntx may not be in the current sub-directory, and when
         // opening the tag, the .cgp path is used as the base...
         char groupFilePathBuf[LEN4PATH+1] ;
         u4namePath( groupFilePathBuf, sizeof( groupFilePathBuf ), groupFileNameWithPath ) ;

         char fullLinePathBuf[LEN4PATH+1] ;
         // AS 11/01/00 - path should be in light of the group file, not the data file...

         u4nameCurrentExtended( fullLinePathBuf, sizeof( fullLinePathBuf ), lineBuf, groupFilePathBuf ) ;
         u4nameExt( fullLinePathBuf, sizeof( fullLinePathBuf ), NTX4EXT, 0 ) ;
         c4upper( fullLinePathBuf ) ;
         if ( u4namecmp( fullLinePathBuf, fullPathTagName, index->data->codeBase->ignoreCase ) == 0 )  // found
         {
            file4removePart( &groupFile, oldPosition-1, currentPosition - 1 ) ;
            break ;
         }
      }

      // AS 11/01/00 - If the group file has length 0, then delete it (i.e. last tag removed)
      // LY Mar 28/05 : changed for 64-bit FILE4LONG
      // if ( file4lenLow( &groupFile ) == 0 )
      if ( file4longEqualZero( file4lenLow( &groupFile ) ) )
         file4setTemporary( &groupFile, 1, 0 ) ;  // has the effect of removing the file
      file4close( &groupFile ) ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER )
   int S4FUNCTION i4tagRemove( TAG4 *tag )
   {
      char indexPathName[LEN4PATH] ;
      DATA4FILE *data = tag->index->data->dataFile ;
      CODE4 *c4 = data->c4 ;

      if ( tag->tagFile->userCount != 1
      #ifndef S4OFF_MULTI
         || tag->tagFile->file.lowAccessMode != OPEN4DENY_RW
      #endif
      )
         return error4( c4, e4remove, E81306 ) ;

      // AS Jun 2/03 - Added recording if data file has a tag with an expression referring to a memo field
      tag->index->data->hasMemoExpr = r4uninitializedMemoTagExpression ;  // just reset, it gets set automatically if required

      u4nameCurrent( indexPathName, sizeof( indexPathName ), tag->tagFile->file.name ) ;
      // AS 01/24/00 -- need to consider that need to remove tag entry from the .cgp file...
      if ( tag->index->accessName[0] != 0 )  // valid group file, so must remove entry...
         i4removeTagEntryFromGroupFile( tag->index, tag->tagFile->file.name ) ;

      // AS 01/24/00 -- actually, t4close takes care of this already...
      // if ( l4numNodes( &tag->index->tags ) == 1 ) /* close the index */
      // {
      //    i4closeLow( tag->index ) ;
      // }
      // else  /* close the tag only */
      // {
         // AS 09/16/99 -- previous tfile4close() was not cleaning up the TAG4 structures...
         t4close( tag ) ;
      // }

      u4remove( indexPathName ) ;

      /* may need to remove the tag name from the group file... */
      /* see impnotes.txt, not implemented to handle group file */

      return 0 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4FOX )
   static int i4tagRemoveBlocksFox( TAG4FILE *t4, B4NODE *node1, B4NODE *node2, B4NODE *node3 )
   {
      int rc = tfile4down( t4 ) ;
      if ( rc < 0 )
         return rc ;
      if ( rc == 2 )
         return e4index ;
      if ( rc == 1 )
         return error4( t4->codeBase, e4index, E95704 ) ;

      B4BLOCK *blockOn = tfile4block( t4 ) ;

      B4NODE blockNo = blockOn->fileBlock ;

      // do all the children of this block, then remove ourselves...

      if ( !b4leaf( blockOn ) )
      {
         if ( b4nodeInvalid( blockOn->header.leftNode ) )
            b4nodeSetInvalid( node1 ) ;
         else
            b4nodeSetUnknown( node1 ) ;

         b4node( *node2 ) = b4key( blockOn, 0 )->num ;

         for( int i = 0 ; i < blockOn->header.nKeys ; i++ )
         {
            b4go( blockOn, (long)i ) ;

            if ( i == blockOn->header.nKeys - 1 && b4nodeInvalid( blockOn->header.rightNode ) )
               b4nodeSetInvalid( node3 ) ;
            else
               b4nodeSetUnknown( node3 ) ;

            rc = i4tagRemoveBlocksFox( t4, node1, node2, node3 ) ;
            if ( rc < 0 )
               return rc ;
         }
      }

      b4node( *node1 ) = b4node( blockOn->fileBlock ) ;
      b4node( *node2 ) = b4node( blockOn->header.rightNode ) ;

      tfile4up( t4 ) ;

      index4shrink( t4->indexFile, blockNo ) ;

      return 0 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4MDX )
   static int i4tagRemoveBlocksMdx( TAG4FILE *t4 )
   {
      int rc = tfile4down( t4 ) ;
      if ( rc < 0 )
         return rc ;
      if ( rc == 2 )
         return -1 ;
      if ( rc == 1 )
         return error4( t4->codeBase, e4index, E95704 ) ;

      B4BLOCK *blockOn = tfile4block( t4 ) ;

      if ( !b4leaf( blockOn ) )
      {
         for( int branchBlockKeyIndex = 0 ; branchBlockKeyIndex <= blockOn->nKeys ; branchBlockKeyIndex++ )
         {
            blockOn->keyOn = branchBlockKeyIndex ;
            rc = i4tagRemoveBlocksMdx( t4 ) ;
            if ( rc < 0 )
               return rc ;
         }
      }

      tfile4up(t4) ;

      index4shrink( t4->indexFile, blockOn->fileBlock ) ;

      return 0 ;
   }
#endif /* #if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4MDX ) */



#if !defined( S4OFF_INDEX ) && defined( S4CLIENT )
   int S4FUNCTION i4tagRemove( TAG4 *tag )
   {
      #ifdef E4PARM_HIGH
         if ( tag == 0 )
            return error4( 0, e4parmNull, E91709 ) ;
      #endif

      CONNECTION4 *connection ;
      CONNECTION4TAG_REMOVE_INFO_IN *info ;
      DATA4 *d4 = tag->index->data ;
      CODE4 *c4 = d4->codeBase ;
      int rc ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifndef S4OFF_WRITE
         rc = d4updateRecord( d4, 0, 1 ) ;
         if ( rc )
            return rc ;
      #endif

      if ( d4->readOnly == 1 )
         return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;
      error4set( c4, 0 ) ;  //

      connection = d4->dataFile->connection ;
      if ( connection == 0 )
         return error4( c4, e4parm, E91717 ) ;
      rc = connection4assign( connection, CON4REMOVE_TAG, data4clientId( d4 ), data4serverId( d4 ) ) ;
      if ( rc < 0 )
         return rc ;
      connection4addData( connection, NULL, sizeof( CONNECTION4TAG_REMOVE_INFO_IN ), (void **)&info ) ;
      memcpy( info->tagName, tag->tagFile->alias, LEN4TAG_ALIAS ) ;
      info->tagName[LEN4TAG_ALIAS] = 0 ;
      // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
      u4ncpy( info->indexFileName, tag->tagFile->indexFile->accessName, strlen( tag->tagFile->indexFile->accessName ) + 1 ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( data->codeBase, rc, E94701 ) ;
      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E94701 ) ;
      // AS 01/23/01 - Must remove the tag...
      INDEX4 *index = tag->index ;
      l4remove( &index->tags, tag ) ;
      t4free( tag, 1 ) ;
      // now delete and remove index if no more tags...
      if ( l4numNodes( &index->tags ) == 0 )
         i4close( index ) ;

      return 0 ;
   }
#endif  // #if !defined( S4OFF_INDEX ) && defined( S4CLIENT )




#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4CLIPPER )
   int S4FUNCTION i4tagRemove( TAG4 *tag )
   {
      #ifdef E4PARM_HIGH
         if ( tag == 0 )
            return error4( 0, e4parmNull, E91709 ) ;
      #endif
      DATA4 *data = tag->index->data ;
      DATA4FILE *dataFile = data->dataFile ;
      CODE4 *c4 = dataFile->c4 ;
      TAG4FILE *tagFile = tag->tagFile ;
      INDEX4FILE *indexFile = tagFile->indexFile ;

      // AS Jun 2/03 - Added recording if data file has a tag with an expression referring to a memo field
      data->hasMemoExpr = r4uninitializedMemoTagExpression ;  // just reset, it gets set automatically if required

      if ( indexFile->userCount != 1
         #ifndef S4OFF_MULTI
            || indexFile->file.lowAccessMode != OPEN4DENY_RW
         #endif
         )
         return error4( c4, e4remove, E81306 ) ;

      if ( l4numNodes( &(indexFile->tags) ) == 1 )  /* last tag, so delete index file */
      {
         char indexPathName[LEN4PATH] ;
         u4nameCurrent( indexPathName, sizeof( indexPathName ), tag->index->indexFile->file.name ) ;
         i4closeLow( tag->index ) ;

         for( ;; )
         {
            INDEX4FILE *i4file = dfile4index( dataFile, indexPathName ) ;
            if ( i4file != 0 )  /* means still around, force closure */
            {
               if ( index4isProduction( i4file ) )
               {
                  #ifdef S4MDX
                     i4file->header.isProduction = 0 ;
                  #endif
                  // AS 03/28/00 need to do this in stand/alone as well, or data file not marked as having index removed
                  #ifdef S4FOX
                     // AS 03/06/00 - just mark the bit, don't remove memo
                     // leave other flags (memo) in place
                     dataFile->hasMdxMemo = dataFile->hasMdxMemo & (~0x01) ;  // bit 1 is for index
                  #else
                     dataFile->hasMdxMemo = 0 ;
                  #endif
                  // AS 03/06/00 - was not removing from the datafile, so was getting open error...
                  FILE4LONG pos ;
                  /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
                  file4longAssign( pos, ( 4 + sizeof( S4LONG ) + 2 * sizeof( short ) + sizeof( char[16] ) ), 0L ) ;
                  #ifdef S4FOX
                     file4writeInternal( &dataFile->file, pos, &(dataFile->hasMdxMemo), sizeof( char ) ) ;
                  #else
                     file4writeInternal( &dataFile->file, pos, &(dataFile->hasMdxMemo), sizeof( dataFile->hasMdxMemo ) ) ;
                  #endif
                  #ifndef S4SERVER
                     dataFile->openMdx = 0 ;
                  #endif

               }
               // AS 03/06/00 if the index4close fails, it is not closed, so can end up in endless loop.
               // stop this by breaking if error...
               if ( index4close( i4file ) < 0 )
                  break ;
            }
            else  /* done */
               break ;
         }

         u4remove( indexPathName ) ;
      }
      else  /* just remove a tag from the file */
      {
         #ifdef S4FOX
            // must remove entry from indexes and then put all blocks on the free chain.
            // AS 06/07/99 --> alias name not good enough, need a length 10 alias with
            // blanks at end, not nulls...
            unsigned char aliasWithBlanksAtEnd[LEN4TAG_ALIAS+1] ;
            if ( c4strlen( tagFile->alias ) > LEN4TAG_ALIAS )  // bad index id...
               return error4( c4, e4index, E81306 ) ;
            char numBytesToCopy = c4strlen( tagFile->alias ) ;
            c4memcpy( aliasWithBlanksAtEnd, tagFile->alias, numBytesToCopy ) ;
            // AS 06/08/99 s/a fox t5samp1 - appears actually needs nulls, not blanks...
            // memset( aliasWithBlanksAtEnd + numBytesToCopy, ' ', sizeof( aliasWithBlanksAtEnd ) - numBytesToCopy ) ;
            // AS 07/06/99 --> do need to include blanks - check out r4reinde.c, does same thing... - maybe add tag is missing...
            c4memset( aliasWithBlanksAtEnd + numBytesToCopy, ' ', sizeof( aliasWithBlanksAtEnd ) - numBytesToCopy ) ;
            // memset( aliasWithBlanksAtEnd + numBytesToCopy, 0, sizeof( aliasWithBlanksAtEnd ) - numBytesToCopy ) ;
            aliasWithBlanksAtEnd[LEN4TAG_ALIAS] = 0 ;
            int rc = tfile4remove( indexFile->tagIndex, aliasWithBlanksAtEnd, b4node( tagFile->headerOffset ) ) ;
            if ( rc != 0 )  // failure to remove... index probably does not exist...
               return error4( c4, e4remove, E81306 ) ;

            // first do the tag header (2 blocks long)
            B4NODE blockNo ;
            // AS headerOffset generally == physical file offset.  For S4FOX use multiplier to modify...
            // (i.e. it is == a simple node )
            FILE4LONG normalizerOfHeaderOffset ;
            b4nodeGetFilePosition( indexFile, tagFile->headerOffset, &normalizerOfHeaderOffset ) ;
            file4longDivide( normalizerOfHeaderOffset, i4blockSize( indexFile ) ) ;
            file4longMultiply( normalizerOfHeaderOffset, i4blockSize( indexFile ) ) ;
            b4nodeSetFromFilePosition( indexFile, &blockNo, normalizerOfHeaderOffset ) ;

            index4shrink( indexFile, blockNo ) ;
            b4nodeAddBlocks( &blockNo, indexFile, 1 ) ;
            index4shrink( indexFile, blockNo ) ;

            if ( tfile4freeAll( tagFile ) < 0 )  // need to clear reset the blocks...
               return error4( c4, e4remove, E81306 ) ;

            // now go through all the blocks in the tag and delete them one by one...
            B4NODE node2 ;
            b4nodeAssignNode( &node2, tagFile->header.root ) ;
            if ( b4nodeInvalid( node2 ) )  // means not read in from disk, do so now...
            {
               FILE4LONG pos ;
               b4nodeGetFilePosition( indexFile, tagFile->headerOffset, &pos ) ;
               if ( file4readAllInternal( &indexFile->file, pos, &node2, sizeof(node2)) < 0 )
                  return error4( c4, e4index, E81601 ) ;
               #ifdef S4BYTE_SWAP
                  node2.node = x4reverseLong( (long *)&node2 ) ;
               #endif
            }
            B4NODE node1, node3 ;
            b4nodeSetInvalid( &node1 ) ;
            b4nodeSetInvalid( &node3 ) ;
            if ( i4tagRemoveBlocksFox( tagFile, &node1, &node2, &node3 ) != 0 )
               return error4( c4, e4remove, E81306 ) ;
         #endif

         #ifdef S4MDX
            // need to take all the blocks and make them 'available'.
            // AS 06/07/99 --> blockNo was being calculated incorrectly.  It needs to take into
            // account the I4MULTIPLY factor, not the r/w value...
            // long blockNo = (int) (tagFile->headerOffset) / (long)indexFile->header.blockRw ;
            B4NODE blockNo ;
            b4nodeAssignNode( &blockNo, tagFile->headerOffset ) ;
            index4shrink( indexFile, blockNo ) ;
            if ( tfile4freeAll( tagFile ) < 0 )  // need to clear reset the blocks...
               return error4( c4, e4remove, E81306 ) ;
            if ( i4tagRemoveBlocksMdx( tagFile ) != 0 )
               return error4( c4, e4remove, E81306 ) ;
         #endif

         // now remove the tag from the linked lists, etc.
         if ( data->tagSelected == tag )
            data->tagSelected = 0 ;
         l4remove( &tag->index->tags, tag ) ;
         mem4free( c4->tagMemory, tag ) ;
         l4remove( &indexFile->tags, tagFile ) ;
         if ( tfile4free( tagFile ) < 0 )
            return error4( c4, e4remove, E81306 ) ;

         #ifdef S4MDX
            // AS 06/23/99 --> was not reducing # tags in header properly...
            indexFile->header.numTags-- ;
            // update the binary tree of tag names...
            int rc = index4fileTagHeadersWriteSp( indexFile, data ) ;
            if ( rc < 0 )
               return error4( c4, e4remove, E81306 ) ;
         #endif
      }

      #ifdef E4ANALYZE
         // verify that the index is ok...
         // AS Feb 10/03 - If a transaction is ongoing, don't run check since will fail
         // AS Apr 29/03 - transcations are run-time in odbc now
         // AS Jun 20/03 - was checking wrong flag
         #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
            if ( c4->server->odbcTrans == 1 )  // odbc build, no trans available
         #endif
            #ifndef S4OFF_TRAN
               if ( code4transEnabled( c4 ) && code4trans( c4 )->currentTranStatus != r4active )
            #endif
               {
                  if ( d4check( data ) != 0 )
                     return -1 ;
               }
      #endif

      return 0 ;
   }
#endif /* !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && !defined( S4CLIPPER ) */
