/* m4create.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.  */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifndef S4CLIENT
#ifndef S4MEMO_OFF
#ifndef S4OFF_WRITE

/* if name is null, a temporary memo file is created, if d4 is null name must exist for create to work */
int memo4fileCreate( MEMO4FILE *m4file, CODE4 *c4, DATA4FILE *d4, const char *name )
{
   #ifdef E4PARM_LOW
      if ( c4 == 0 )
         return error4( c4, e4parm_null, E95207 ) ;
      if ( d4 == 0 && name == 0 )
         return error4( c4, e4parm_null, E95207 ) ;
   #endif

   m4file->data = d4 ;

   #ifndef S4MNDX
      #ifdef S4MFOX
         // AS 01/06/99 -- values < 33 allowed (in particular, 1) -- this occurs when this
         // function is called by memocompress, for example
         // if ( c4->memSizeMemo < 33 )
         //    c4->memSizeMemo = 33 ;
      #else
         if ( c4->memSizeMemo % 512 != 0 || c4->memSizeMemo == 0 )
            c4->memSizeMemo = ((c4->memSizeMemo / 512) + 1) * 512 ;
      #endif

      #ifdef E4MISC
         #ifdef S4MFOX
            if ( c4->memSizeMemo > 512 * 32 )
               return error4( 0, e4memoCreate, E85201 ) ;
         #else
            if ( sizeof( MEMO4HEADER ) > c4->memSizeMemo )
               return error4( c4, e4memoCreate, E95207 ) ;

            if ( c4->memSizeMemo > 512 * 63 )
               return error4( 0, e4memoCreate, E85201 ) ;
         #endif
      #endif
   #endif

   MEMO4HEADER *headerPtr = (MEMO4HEADER *)u4allocEr( c4, (long)sizeof( MEMO4HEADER ) ) ;
   if ( headerPtr == 0 )
      return -1 ;

   #ifdef S4MFOX
      m4file->blockSize = (short)c4->memSizeMemo ;
      if ( m4file->blockSize > 512 )
         headerPtr->nextBlock = 0x01000000 ;
      else
      {
         headerPtr->nextBlock = B4BLOCK_SIZE_INTERNAL / m4file->blockSize ;
         #ifndef S4BYTE_SWAP
            headerPtr->nextBlock = x4reverseLong( (void *)&headerPtr->nextBlock) ;
         #endif
      }
      #ifndef S4BYTE_SWAP
         headerPtr->blockSize = x4reverseShort( (void *)&m4file->blockSize ) ;
      #else
         headerPtr->blockSize = m4file->blockSize ;
      #endif
   #else
      #ifdef S4MNDX
         m4file->blockSize = MEMO4SIZE ;
         headerPtr->nextBlock = 1 ;
      #else
         headerPtr->nextBlock = 1 ;
         headerPtr->x102 = 0x102 ;
         m4file->blockSize = headerPtr->blockSize = (short)c4->memSizeMemo ;
      #endif
   #endif

   char buf[258] ;
   if ( name == 0 )
   {
      int saveFlag = c4getErrCreate( c4 ) ;
      int safety = c4->safety ;
      c4setErrCreate( c4, 0 ) ;
      c4->safety = 1 ;   /* AS 03/21/97 safety should be on to avoid file overwrite, not false! */

      /* use the d4 path to ensure the file gets created in the correct location */
      #ifdef S4MACINTOSH
         long macDirTemp = c4->macDir ;
         int macVolTemp = c4->macVol ;
         c4->macDir = d4->file.macSpec.parID ;
         c4->macVol = d4->file.macSpec.vRefNum ;
         int pos = 0 ;
      #else
         int pos = u4namePath( buf, sizeof( buf ) - 14, d4->file.name ) ;
      #endif

      strcpy( buf + pos, "TEMP" ) ;
      int rc ;

      for ( int i = 0 ;; )
      {
         if ( i >= 100 )
         {
            rc = error4( c4, e4create, E80605 ) ;
            break ;
         }
         i++ ;

         u4delayHundredth( 50 ) ;
         #if defined(S4WINCE) || defined(S4WIN32)
            SYSTEMTIME st ;
            GetLocalTime(&st) ;
            WORD t = st.wMilliseconds ;
         #else
            time_t t ;
            time( &t );
         #endif
         t %= 10000L ;

         c4ltoa45( t, buf + pos + 4, -4 ) ;
         strcpy( buf + pos + 8, ".tmp" ) ;
         rc = file4create( &m4file->file, c4, buf, 1 ) ;
         if ( rc <= 0 )
            break ;
      }
      #ifdef S4MACINTOSH
         c4->macDir = macDirTemp ;
         c4->macVol = macVolTemp ;
      #endif

      c4setErrCreate( c4, saveFlag ) ;
      c4->safety = safety ;

      if ( rc < 0 )
      {
         u4free( headerPtr ) ;
         return error4( 0, rc, E95207 ) ;
      }
   }
   else
   {
      u4ncpy( buf, name, sizeof( buf ) ) ;
      #ifndef S4MFOX
         #ifndef S4MNDX
            char memoName[9] ;
            memset( memoName, 0, sizeof( memoName ) ) ;
            u4namePiece( memoName, sizeof(memoName), name, 0, 0 ) ;
            memcpy( headerPtr->fileName, memoName, 8 ) ;
         #endif
      #endif
      u4nameExt( buf, sizeof(buf), MEMO4EXT, 1 ) ;

      int rc = file4create( &m4file->file, c4, buf, 1 ) ;
      if ( rc != 0 )
      {
         u4free( headerPtr ) ;
         return error4( c4, (short)rc, E85201 ) ;
      }
   }

   #ifdef S4BYTE_SWAP
      #ifndef S4MFOX
         headerPtr->nextBlock = x4reverseLong( (void *)&headerPtr->nextBlock ) ;
         #ifndef S4MNDX
            headerPtr->x102 = 0x201 ;
            headerPtr->blockSize = x4reverseShort( (void *)&headerPtr->blockSize ) ;
         #endif
      #endif
   #endif

   FILE4LONG fPos ;
   file4longAssign( fPos, 0, 0 ) ;
   int rc = file4writeInternal( &m4file->file, fPos, headerPtr, sizeof(  MEMO4HEADER ) ) ;

   #ifdef S4MFOX
      file4longAssign( fPos, 512, 0 ) ;
      file4lenSetLow( &m4file->file, fPos ) ;
   #endif

   u4free( headerPtr ) ;
   return rc ;
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif
#ifdef S4MFOX
   int memo4fileDump( MEMO4FILE *f4memo, const long memoId, const char *ptr, const unsigned len, const long memoLen, const long type )
#else
   int memo4fileDump( MEMO4FILE *f4memo, const long memoId, const char *ptr, const unsigned len, const long memoLen )
#endif
{
   #ifdef S4MNDX
      char oneA = 0x1A ;
   #else
      MEMO4BLOCK  memoBlock ;

      #ifdef S4MFOX
         #ifdef S4BYTE_SWAP
            memoBlock.type = type ;
            memoBlock.numChars = memoLen ;
         #else
            memoBlock.type = x4reverseLong( (void *)&type ) ;
            memoBlock.numChars = x4reverseLong( (void *)&memoLen ) ;
         #endif
      #else
         memoBlock.minusOne = -1 ;
         memoBlock.startPos = sizeof(S4LONG) + 2 * sizeof( short ) ;
         memoBlock.numChars = memoBlock.startPos + memoLen ;
         #ifdef S4BYTE_SWAP
            memoBlock.startPos = x4reverseShort( (void *)&memoBlock.startPos ) ;
            memoBlock.numChars = x4reverseLong( (void *)&memoBlock.numChars ) ;
         #endif
      #endif
   #endif

   FILE4LONG pos ;
   file4longAssign( pos, memoId, 0 ) ;
   file4longMultiply( pos, f4memo->blockSize ) ;

   FILE4LONG compare ;
   file4longAssignLong( compare, pos ) ;
   file4longAdd( &compare, len ) ;
   FILE4LONG fileLen = file4lenLow( &f4memo->file ) ;
   Bool5 doLenSet = ( file4longLessLong( fileLen, compare ) ? 1 : 0 ) ;

   int rc ;

   #if defined( E4MISC ) && defined( S4MDX )
      // AS 06/07/00 make sure that the position to write to is not too far out (i.e. maybe bad
      // data in memo file) - use a 4 extra block overhead
      if ( pos > (fileLen + f4memo->blockSize * 4 ) )  // writing at least 4 blocks after end of file - must be corrupt
         return error4( f4memo->data->c4, e4memoCorrupt, E95207 ) ;
   #endif

   #ifdef S4MNDX
      rc = file4writeInternal( &f4memo->file, pos, ptr, len ) ;
      if ( rc != 0 )
         return rc ;
      file4longAdd( &pos, len ) ;
      rc = file4writeInternal( &f4memo->file, pos, &oneA, 1 ) ;
   #else
      rc = file4writeInternal( &f4memo->file, pos, &memoBlock, sizeof( MEMO4BLOCK ) ) ;
      if ( rc != 0 )
         return rc ;
      #ifdef S4MFOX
         file4longAdd( &pos, sizeof( MEMO4BLOCK) ) ;
      #else
         file4longAdd( &pos, (sizeof( S4LONG ) + 2 * sizeof( short )) ) ;
      #endif
      rc = file4writeInternal( &f4memo->file, pos, ptr, len ) ;
   #endif

   if ( rc < 0 )
      return rc ;

   if ( doLenSet )
   {
      // verify that the file ends on an even blockSize boundary.  If it doesn't, extend the file
      // so that it does.
      FILE4LONG finalLen = file4lenLow( &f4memo->file ) ;
      FILE4LONG finalNumBlocks ;
      file4longAssignLong( finalNumBlocks, finalLen ) ;
      file4longDivide( finalNumBlocks, f4memo->blockSize ) ;
      FILE4LONG checkLength ;
      file4longAssignLong( checkLength, finalNumBlocks ) ;
      file4longMultiply( checkLength, f4memo->blockSize ) ;

      // AS 05/11/00 was lengthening if they were equal, not if they were not equal
      // if ( file4longEqualLong( checkLength, finalLen ) != 0 )
      if ( file4longEqualLong( checkLength, finalLen ) == 0 )  // if they are not equal, then we need to extend the file to full width.
      {
         #ifdef E4ANALYZE
         /*
            if ( (unsigned long)(tp+1)*f4memo->blockSize <= file4longGetLo( file4lenLow( &f4memo->file ) ) ||
                 (unsigned long)(tp+1)*f4memo->blockSize <= pos + len )
              return error4( f4memo->data->c4, e4info, E95207 ) ;
         */
         #endif
         file4longAssignLong( finalLen, finalNumBlocks ) ;
         file4longAdd( &finalLen, 1 ) ;
         file4longMultiply( finalLen, f4memo->blockSize ) ;
         file4lenSetLow( &f4memo->file, finalLen ) ;
      }
   }

   return 0 ;
}

#endif  /* S4OFF_WRITE */
#endif  /* S4MEMO_OFF */
#endif  /* S4CLIENT */
