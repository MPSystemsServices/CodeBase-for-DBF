/* m4memo.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.  */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif



#if !defined( S4OFF_MEMO ) && defined( S4CLIENT ) && !defined( S4OFF_WRITE )
   int S4FUNCTION d4memoCompress( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E95201 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E95201 ) ;
      #endif

      CODE4 *c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( data->readOnly == 1 )
         return error4describe( c4, e4write, E80606, d4alias( data ), 0, 0 ) ;

      if ( data->dataFile->nFieldsMemo == 0 )
         return 0 ;

      int rc = d4update( data ) ;
      if ( rc )
         return rc ;

      CONNECTION4 *connection = data->dataFile->connection ;
      if ( connection == 0 )
         return error4stack( c4, e4connection, E95201 ) ;

      data->count = -1 ;
      data->dataFile->numRecs = -1 ;
      connection4assign( connection, CON4MEMO_COMPRESS, data4clientId( data ), data4serverId( data ) ) ;
      rc = connection4repeat( connection ) ;
      if ( rc < 0 )
         connection4error( connection, c4, rc, E95201 ) ;

      return rc ;
   }
#endif /* #if !defined( S4OFF_MEMO ) && defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */



#if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && defined( S4MFOX )
   // AS 09/06/99 --> update to unsigned long for large files...
   unsigned long memo4lenPart( MEMO4FILE *f4memo, long memoId )
   {
      // returns ULONG_MAX if an error occurs...
      if ( memoId <= 0L )
         return 0 ;

      FILE4LONG pos ;
      file4longAssign( pos, memoId, 0 ) ;
      file4longMultiply( pos, f4memo->blockSize ) ;
      MEMO4BLOCK memoBlock ;
      int rc = file4readAllInternal( &f4memo->file, pos, &memoBlock, sizeof( MEMO4BLOCK ) ) ;
      if ( rc < 0 )
         return ULONG_MAX ;

      #ifdef S4BYTE_SWAP
         return memoBlock.numChars ;
      #else
         /* S4FOX keeps the numChars entry in reversed long form */
         return x4reverseLong( (void *)&memoBlock.numChars ) ;
      #endif
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && defined( S4MFOX ) */



#if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE )
   int S4FUNCTION d4memoCompress( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E95201 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E95201 ) ;
      #endif

      CODE4 *c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( data->dataFile->nFieldsMemo == 0 )
         return 0 ;

      int rc = d4update( data ) ;
      if ( rc )
         return rc ;

      #ifndef S4OFF_MULTI
         rc = d4lockFileInternal( data, 1 ) ;
         if ( rc )
            return rc ;
      #endif

      #ifndef S4OFF_TRAN
         if ( d4transEnabled( data, 0 ) )
            if ( tran4active( c4, data ) != 0 )
               return error4( c4, e4transViolation, E81502 ) ;
      #endif

      /* AS 01/26/99 fix becuase memoCompress leaves record changed flag to true*/
      rc = dfile4memoCompress( data->dataFile, data ) ;
      #ifdef S4FOX
         data->recordChanged = 0 ;
         /* AS 06/16/99 --> server case was causing r4entry on empty
            database.  current position does not matter for server.
         */
         #ifndef S4SERVER
            if ( rc == 0 )
            {
               // AS 10/18/99 --> maybe at 'eof' or past end of file, as well...
               // if ( d4recNo( data ) == -1 )
               if ( d4recNo( data ) == -1 || d4recNo( data ) > d4recCount( data ) )
               {
                  rc = d4goEof( data ) ;
                  if ( rc == r4eof )
                     rc = 0 ;
               }
               else
                  rc = d4go( data, d4recNo( data ) ) ;
            }
         #endif
      #endif
      return rc ;
   }



   int dfile4memoCompress( DATA4FILE *data, DATA4 *d4 )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( d4, 2, E95201 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E95201 ) ;
      #endif

      CODE4 *c4 = data->c4 ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( data->memoFile.file.hand == INVALID4HANDLE )
         return 0 ;

      int rc ;

      #if !defined( S4OFF_MULTI ) && !defined( S4CLIPPER )
         #ifdef S4SERVER
            rc = dfile4lockMemo( data ) ;
         #else
            rc = dfile4lockMemo( data ) ;
         #endif
         if ( rc )
            return rc ;
      #endif

      unsigned int saveFlag = c4->memSizeMemo ;
      c4->memSizeMemo = (unsigned int)data->memoFile.blockSize ;

      MEMO4FILE newFile ;
      rc = memo4fileCreate( &newFile, c4, data, 0 ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E91102 ) ;

      c4->memSizeMemo = saveFlag ;
      newFile.blockSize = data->memoFile.blockSize ;

      char *rdBuf = 0 ;
      char *wrBuf = 0 ;
      unsigned int bufSize = c4->memSizeBuffer ;

      for ( ; bufSize > data->recWidth ; bufSize -= 0x800 )
      {
         rdBuf = (char *)u4allocFree( c4, (long)bufSize ) ;
         if ( rdBuf == 0 )
            continue ;

         wrBuf = (char *)u4allocFree( c4, (long)bufSize ) ;
         if ( wrBuf )
            break ;

         u4free( rdBuf ) ;
         rdBuf = 0 ;
      }

      FILE4SEQ_READ rd ;
      #ifdef S4ADVANCE_READ
         file4seqReadInitDo( &rd, &data->file, dfile4recordPosition( data, 1L ), rdBuf, bufSize, 1 ) ;
      #else
         file4seqReadInitDo( &rd, &data->file, dfile4recordPosition( data, 1L ), rdBuf, bufSize, 0 ) ;
      #endif

      FILE4SEQ_WRITE wr ;
      file4seqWriteInitLow( &wr, &data->file, dfile4recordPosition( data, 1L ), wrBuf, bufSize ) ;

      long curCount = dfile4recCount( data, data4serverId( d4 )  ) ;
      char *ptr = 0 ;
      unsigned int ptrLen = 0 ;
      unsigned int ptrMax = 0 ;
      long memoType = 1 ;

      for ( long iRec= 1L ; iRec <= curCount && rc == 0 ; iRec++ )
      {
         if ( file4seqReadAll( &rd, d4->record, data->recWidth ) < 0 )
            break ;

         for ( int i = 0 ; i < d4->dataFile->nFieldsMemo ; i++ )
         {
            FIELD4 *field = d4->fieldsMemo[i].field ;

            #ifdef S4FOX
               unsigned long pos = 0L ;

               long memoLen ;
               if ( f4null( field ) == 1 )
                  memoLen = 0 ;
               else
                  memoLen = memo4lenPart( &data->memoFile, f4long( field ) ) ;

               // AS 09/06/99 == ULONG_MAX if read failure, in which case the entry is just removed...
               if ( (unsigned long)memoLen == ULONG_MAX )  // means a corrupt memo file...
               {
                   rc = error4( c4, e4memoCorrupt, E91102 ) ;
                  break ;
               }


               if ( memoLen > 0L )
               {
                  long newId = 0L ;
                  do
                  {
                     #ifdef E4ANALYZE
                        if ( pos > (unsigned long)memoLen )
                        {
                           rc = error4( c4, e4memoCorrupt, E91102 ) ;
                           break ;
                        }
                     #endif

                     ptrLen = ptrMax ;

                     if ( memo4fileReadPart( &data->memoFile, f4long( field ), &ptr, &ptrLen, pos, UINT_MAX - 100, &memoType ) < 0 )
                     {
                        rc = -1 ;
                        break ;
                     }

                     if ( ptrLen > ptrMax )
                        ptrMax = ptrLen ;

                     if ( memo4fileWritePart( &newFile, &newId, ptr, memoLen, pos, ptrLen, memoType ) < 0 )
                     {
                        rc = -1 ;
                        break ;
                     }
                     pos += ptrLen ;
                  } while( pos != (unsigned long)memoLen ) ;
                  f4assignLong( field, newId ) ;
               }
               else
                  f4assignLong( field, 0L ) ;
            #else
               ptrLen = ptrMax ;
               if ( memo4fileRead( &data->memoFile, f4long(field), &ptr, &ptrLen ) < 0 )
               {
                  rc = -1 ;
                  break ;
               }

               if ( ptrLen > ptrMax )
                  ptrMax = ptrLen ;
               else
                  if ( ptrLen == 0 && ptr == 0 )
                     ptrMax = 0 ;

               long newId = 0L ;
               if ( memo4fileWrite( &newFile, &newId, ptr, ptrLen ) < 0 )
               {
                  rc = -1 ;
                  break ;
               }

               c4ltoa45( newId, f4ptr(field), - ((int) field->len) ) ;
            #endif
         }
         file4seqWrite( &wr, d4->record, data->recWidth ) ;
      }

      if ( rc < 0 )
         file4close( &newFile.file ) ;  /* error occurred */
      else
         file4seqWriteFlush(&wr) ;

      #ifdef S4ADVANCE_READ
         file4seqReadInitUndo( &rd ) ;
      #endif

      u4free( ptr ) ;
      u4free( rdBuf ) ;
      u4free( wrBuf ) ;

      if ( rc == 0 )
      {
         rc = file4replace( &data->memoFile.file, &newFile.file ) ;
         if ( rc == 0 )
         {
            FILE4LONG fileLen = file4lenLow( &data->memoFile.file ) ;
            #ifdef S4MMDX
               if ( file4longLess( fileLen, (unsigned long)data->memoFile.blockSize ) )
               {
                  FILE4LONG fLong ;
                  file4longAssign( fLong, data->memoFile.blockSize, 0 ) ;
                  rc = file4lenSetLow( &data->memoFile.file, fLong ) ;
               }
            #else
               if ( file4longLess( fileLen, 512 ) )
               {
                  FILE4LONG fLong ;
                  file4longAssign( fLong, 512, 0 ) ;
                  rc = file4lenSetLow( &data->memoFile.file, fLong ) ;
               }
            #endif
         }
      }

      return rc ;
   }
#endif /* !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE ) */



#if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && !defined( S4MFOX ) && !defined( S4MNDX )
   int memo4fileChainFlush( MEMO4FILE *f4memo, MEMO4CHAIN_ENTRY *chain )
   {
      if ( chain->toDisk )
      {
         chain->toDisk = 0 ;
         FILE4LONG pos ;
         file4longAssign( pos, chain->blockNo, 0 ) ;
         file4longMultiply( pos, f4memo->blockSize ) ;
         #ifdef S4BYTE_SWAP
            MEMO4CHAIN_ENTRY swap ;
            memcpy( (void *)&swap, (void *)chain, sizeof( MEMO4CHAIN_ENTRY ) ) ;
            swap.next = x4reverseLong( (void *)&swap.next ) ;
            swap.num = x4reverseLong( (void *)&swap.num ) ;

            return file4writeInternal( &f4memo->file, pos, &swap, 2 * sizeof(S4LONG) ) ;
         #else
            return file4writeInternal( &f4memo->file, pos, chain, 2 * sizeof(S4LONG) ) ;
         #endif
      }
      return 0 ;
   }



   int memo4fileChainSkip( MEMO4FILE *f4memo, MEMO4CHAIN_ENTRY *chain )
   {
      chain->toDisk = 0 ;
      chain->blockNo = chain->next ;

      if ( chain->next == ULONG_MAX )  // -1
      {
         chain->num = -1 ;
         chain->next = -1 ;
         return 0 ;
      }

      FILE4LONG pos ;
      file4longAssign( pos, chain->next, 0 ) ;
      file4longMultiply( pos, f4memo->blockSize ) ;

      unsigned lenRead = file4readInternal( &f4memo->file, pos, chain, sizeof( chain->next ) + sizeof( chain->num ) ) ;

      if ( error4code( f4memo->data->c4 ) < 0 )
         return -1 ;

      if ( lenRead == 0 )
      {
         chain->num = -1 ;
         chain->next = -1 ;
         return 0 ;
      }

      #ifdef S4BYTE_SWAP
         chain->next = x4reverseLong( (void *)&chain->next ) ;
         chain->num = x4reverseLong( (void *)&chain->num ) ;
      #endif

      if ( lenRead != sizeof( chain->next ) + sizeof( chain->num ) )
         return file4readError( &f4memo->file, pos, sizeof(chain->next)+sizeof(chain->num), "memo4fileChainSkip" ) ;

      return 0 ;
   }
#endif /* if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && !defined( S4MFOX ) && !defined( S4MNDX ) */



#if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && !defined( S4OFF_MULTI )
   int d4validateMemoIds( DATA4 *data )
   {
      /* Make the memo file entries current */
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E95203 ) ;
      #endif

      if ( data->memoValidated )
         return 0 ;

      int rc ;
      if ( data->recNum > 0 )
      {
         #ifdef S4SERVER
            rc = d4lockInternal( data, data->recNum, 0 ) ;
         #else
            rc = d4lockInternal( data, data->recNum, 1 ) ;
         #endif
         if ( rc )
            return rc ;
      }

      rc = d4readOld( data, data->recNum ) ;
      if ( rc < 0 )
         return error4stack( data->codeBase, rc, E95203 ) ;

      if ( data->recordChanged == 0 )   /* if the record has changed, leave intact */
      {
         for ( int i = 0 ; i < data->dataFile->nFieldsMemo ; i++ )
         {
            if ( data->fieldsMemo[i].isChanged == 0 )
            {
               char *fromPtr = data->recordOld + data->fieldsMemo[i].field->offset ;
               memcpy( f4ptr( data->fieldsMemo[i].field ), fromPtr, f4len( data->fieldsMemo[i].field ) ) ;  /* need f4len() because S4FOX 3.0 has 4 byte memos, not 10 byte */
            }
         }
      }

      data->memoValidated = 1 ;
      return 0 ;
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && !defined( S4OFF_MULTI ) */
