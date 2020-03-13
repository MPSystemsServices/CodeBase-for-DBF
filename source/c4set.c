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

 //VCNOTE:[MERGED_8.13_+_8.9.6,_base=8.9]
/* c4set.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */
/* code to get/set CODE4 settings */

/* functions used to set and get CODE4 flags from outside of a DLL in cases
   where the structures are unknown (eg. index independent program) */
/* cannot be defines */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */


#ifndef S4SERVER
   void S4FUNCTION code4autoIncrementStart( CODE4 *c4, double val )
   {
      c4->autoIncrementStart = val ;
   }



   void S4FUNCTION c4setTransFileName( CODE4 *c4, char *name )
   {
      #if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
         if ( c4->transFileName != 0 )
         {
            u4free( c4->transFileName ) ;
            c4->transFileName = 0 ;
         }
         int len = c4strlen( name ) + 1 ;
         c4->transFileName = (char *)u4alloc( len ) ;
         if ( c4->transFileName != 0 )
            c4memcpy( c4->transFileName, name, len ) ;
      #endif
   }



   // AS Aug 13/03 - don't access the CODE4 structure directly
   void S4FUNCTION c4setTransShared( CODE4 *c4, short val )
   {
      #if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
         c4->transShared = (unsigned char)val ;
      #endif
   }



   void S4FUNCTION code4unlockAutoSetDo( CODE4 *c4, int val )
   {
      // AS 06/09/00 was not compiling in S4OFF_MULTI
      #ifndef S4OFF_MULTI
         c4->c4trans.trans.unlockAuto = val ;
      #endif
   }



   int S4FUNCTION c4getAccessMode( const CODE4 *c4 )
   {
      return c4->accessMode ;
   }



   short S4FUNCTION code4indexBlockSize( CODE4 *c4 )
   {
      #if !defined( S4CLIENT_OR_FOX ) || defined( S4OFF_INDEX ) || defined( S4PALM )
         return error4( c4, e4notSupported, E96701 ) ;
      #else
         if ( !s5fox )
            error4( c4, e4notSupported, E96701 ) ;

         if (  c4->foxCreateIndexMultiplier == 1 )  // means fox defaults
            return -1 ;

         return c4->foxCreateIndexBlockSize ;
      #endif
   }
#endif /* !S4SERVER */


#ifdef S4FOX
   // AS Jul 6/06 - need a function to get the real index block size, helps in client/server
   // not in client/server
   unsigned short c4indexBlockSizeGet( CODE4 *c4 )
   {
      #ifdef S4OFF_INDEX
         // CS 2007/01/30
         return e4notSupported;
      #else
         #ifdef S4SERVER
            return c4->currentClient->foxCreateIndexBlockSize ;
         #else
            return c4->foxCreateIndexBlockSize ;
         #endif
      #endif
   }
#endif



short c4indexMultiplierGet( CODE4 *c4 )
{
   #ifdef S4OFF_INDEX
      // CS 2007/01/30
      return e4notSupported;
   #else
      #ifdef S4SERVER
         #ifdef S4FOX
            return c4->currentClient->foxCreateIndexMultiplier ;
         #else
            return error4( c4, e4notSupported, E96701 ) ;
         #endif
      #else
         #ifdef S4CLIENT_OR_FOX
            if ( code4indexFormat( c4 ) == r4cdx )
               return c4->foxCreateIndexMultiplier ;
            else
         #endif
            return error4( c4, e4notSupported, E96701 ) ;
      #endif
   #endif
}



#ifndef S4SERVER
   // CS 1999/08/26 return short for VB
   short S4FUNCTION code4indexBlockSizeSet( CODE4 *c4, short val )
   {
      #if defined( S4CLIENT_OR_FOX ) && !defined( S4OFF_INDEX ) && !defined( S4PALM )
         // AS Jul 5/06 - it is ok to set this if we are not connected yet...
         // if ( s5fox )
         if ( s5clipper == 0 && s5mdx == 0 )
         {
            if ( val == -1 )  // means set it to foxpro compatible defaults
            {
               c4->foxCreateIndexBlockSize = 512 ;
               c4->foxCreateIndexMultiplier = 1 ;
            }
            else
            {
               // must be greater than or equal to 512
               if ( val >= 512 )
               {
                  // must be a multiple of 2...
                  for ( short valToCheck = val ;; )
                  {
                     if ( valToCheck == 512 )   // is a multiple of 2...
                        break ;
                     if ( valToCheck % 2 != 0 ) // not a multiple of 2...
                        return error4( c4, e4parm, E96701 ) ;
                     valToCheck /= 2 ;
                  }

                  c4->foxCreateIndexBlockSize = val ;
                  // if not-compatible, we set multiplier to 1024 (or 512 if val is 512, since must be a multiple of this)
                  if ( val == 512 )
                     c4->foxCreateIndexMultiplier = 512 ;
                  else
                     c4->foxCreateIndexMultiplier = 1024 ;
               }
               else
                  return error4( c4, e4parm, E96701 ) ;
            }
         }
         else
            return error4( c4, e4notSupported, E96701 ) ;

         return 0 ;
      #else
         return error4( c4, e4notSupported, E96701 ) ;
      #endif
   }
#endif /* !S4SERVER */



#ifndef S4SERVER
   int S4FUNCTION c4getAutoOpen( const CODE4 *c4 )
   {
      return c4->autoOpen ;
   }
#endif /* !S4SERVER */



#ifdef S4CLIENT
   CONNECT4 *S4FUNCTION c4getClientConnect( CODE4 *c4 )
   {
      return &c4->clientConnect ;
   }
#endif /* S4CLIENT */



#ifndef S4SERVER
   short S4FUNCTION c4getCompatibility( const CODE4 *c4 )
   {
      return c4->compatibility ;
   }



   Bool5 S4FUNCTION c4getCompressedMemos( const CODE4 *c4 )
   {
      #if defined( S4CLIENT_OR_FOX ) && defined( S4COMPRESS )
         return c4->compressedMemos ;
      #else
         return 0;
      #endif
   }



   int S4FUNCTION c4getCreateTemp( const CODE4 *c4 )
   {
      return c4->createTemp ;
   }



   char *S4FUNCTION c4getAppVerify( const CODE4 *c4 )
   {
      return c4->applicationVerify ;
   }



   int S4FUNCTION c4getDoRemove( const CODE4 *c4 )
   {
      return c4->doRemove ;
   }



   int S4FUNCTION c4getErrorCode( const CODE4 *c4 )
   {
      return error4code( c4 ) ;
   }



   int S4FUNCTION c4getErrCreate( const CODE4 *c4 )
   {
      return c4->errCreate ;
   }



   int S4FUNCTION c4getErrDefaultUnique( const CODE4 *c4 )
   {
      return c4->errDefaultUnique ;
   }



   int S4FUNCTION c4getFileFlush( const CODE4 *c4 )
   {
      return c4->fileFlush ;
   }



   void S4FUNCTION c4setFileFlush( CODE4 *c4, int val )
   {
      c4->fileFlush = val ;
   }



   int S4FUNCTION c4getCollatingSequence( const CODE4 *c4 )
   {
      return c4->collatingSequence ;
   }



   int S4FUNCTION c4getErrExpr( const CODE4 *c4 )
   {
      return c4->errExpr ;
   }



   int S4FUNCTION c4getErrFieldName( const CODE4 *c4 )
   {
      return c4->errFieldName ;
   }



   int S4FUNCTION c4getErrGo( const CODE4 *c4 )
   {
      return c4->errGo ;
   }



   int S4FUNCTION c4getErrOff( const CODE4 *c4 )
   {
      return c4->errOff ;
   }



   int S4FUNCTION c4getErrOpen( const CODE4 *c4 )
   {
      return c4->errOpen ;
   }



   int S4FUNCTION c4getErrRelate( const CODE4 *c4 )
   {
      return c4->errRelate ;
   }



   int S4FUNCTION c4getErrSkip( const CODE4 *c4 )
   {
      return c4->errSkip ;
   }



   int S4FUNCTION c4getErrTagName( const CODE4 *c4 )
   {
      return c4->errTagName ;
   }



   int S4FUNCTION c4getLockAttempts( const CODE4 *c4 )
   {
      return c4->lockAttempts ;
   }


   /* LY 2001/09/06 */
   int S4FUNCTION c4getLockAttemptsSingle( const CODE4 *c4 )
   {
      return c4->lockAttemptsSingle ;
   }


   /* LY 2001/09/06 */
   int S4FUNCTION c4getLockDelay( const CODE4 *c4 )
   {
      return c4->lockDelay ;
   }


   int S4FUNCTION c4getLockEnforce( const CODE4 *c4 )
   {
      return c4->lockEnforce ;
   }


   #if defined(S4STAND_ALONE) && !defined(S4OFF_TRAN) && !defined(S4OFF_WRITE)
      int S4FUNCTION c4getLogOpen( const CODE4 *c4 )
      {
         return c4->logOpen ;
      }
   #endif /* !S4OFF_WRITE && !S4OFF_TRAN && S4STAND_ALONE */



   int S4FUNCTION c4getLog( const CODE4 *c4 )
   {
      return c4->log ;
   }



   #if defined( S4WIN32 ) && defined(__cplusplus) && defined(OLEDB5BUILD)
      Mem5zeroAllocator *S4FUNCTION c4getMemZeroAllocator( const CODE4 *c4 )
      {
         return( c4->memZeroAllocator );
      }
   #endif


   char S4FUNCTION c4getOledbSchemaCreate( const CODE4 *c4 )
   {
      return c4->optimize ;
   }



   int S4FUNCTION c4getOptimize( const CODE4 *c4 )
   {
      return c4->optimize ;
   }



   int S4FUNCTION c4getOptimizeWrite( const CODE4 *c4 )
   {
      return c4->optimizeWrite ;
   }



   int S4FUNCTION c4getReadLockDo( const CODE4 *c4 )
   {
      return c4->readLock ;
   }



   int S4FUNCTION c4getReadOnlyDo( const CODE4 *c4 )
   {
      return c4->readOnly ;
   }



   int S4FUNCTION c4getSafety( const CODE4 *c4 )
   {
      return c4->safety ;
   }



   int S4FUNCTION c4getSingleOpen( const CODE4 *c4 )
   {
      return c4->singleOpen ;
   }



   const char *S4FUNCTION t4getExprSource( TAG4 *t4 )
   {
      return t4expr( t4 ) ;
   }



   void S4FUNCTION c4setAccessMode( CODE4 *c4, char val )
   {
      c4->accessMode = val ;
   }



   // AS Feb 5/03 - For ODBC transactions, allow the file to be opened exclusive.  This is a requirement
   // to allow multiple transactions at one time (we will then cycle through a list of log file names).
   void S4FUNCTION c4setLogAccessMode( CODE4 *c4, char val )
   {
      #if defined( S4STAND_ALONE ) && !defined( S4OFF_TRAN )
         c4->logAccessMode = val ;
      #endif
   }



   void S4FUNCTION c4setAutoOpen( CODE4 *c4, int val )
   {
      c4->autoOpen = val ;
   }



   void S4FUNCTION code4verifySet( CODE4 *c4, const char *val )
   {
      // AS July 19/02 - the string actually requires as much as 270 bytes, increase this here.
      if ( c4->applicationVerify == 0 )
         c4->applicationVerify = (char *)u4allocFree( c4, 300 ) ;
      if ( c4->applicationVerify != 0 )
         c4strncpy( c4->applicationVerify, 300, val, 299 ) ;  // AS Dec 13/05 vs 5.0 fixes
   }



   void S4FUNCTION c4setCompatibility( CODE4 *c4, short val )
   {
      c4->compatibility = val ;
   }



   void S4FUNCTION c4setCompressedMemos( CODE4 *c4, Bool5 val )
   {
      #if defined( S4CLIENT_OR_FOX ) && defined( S4COMPRESS )
         c4->compressedMemos = val ;
      #else
         error4describe(c4, e4notSupported, 0, "c4setCompressedMemos", 0, 0);
      #endif
   }



   void S4FUNCTION c4setCreateTemp( CODE4 *c4, int val )
   {
      c4->createTemp = val ;
   }



   void S4FUNCTION c4setDoRemove( CODE4 *c4, int val )
   {
      c4->doRemove = val ;
   }



   void S4FUNCTION c4setErrorCode( CODE4 *c4, int val )
   {
      error4set( c4, val ) ;
   }



   void S4FUNCTION c4setErrCreate( CODE4 *c4, int val )
   {
      c4->errCreate = val ;
   }



   void S4FUNCTION c4setErrExpr( CODE4 *c4, int val )
   {
      c4->errExpr = val ;
   }



   void S4FUNCTION c4setErrFieldName( CODE4 *c4, int val )
   {
      c4->errFieldName = val ;
   }



   void S4FUNCTION c4setErrGo( CODE4 *c4, int val )
   {
      c4->errGo = val ;
   }



   void S4FUNCTION c4setErrOff( CODE4 *c4, int val )
   {
      c4->errOff = val ;
   }



   #ifndef S4CLIENT
      void S4FUNCTION c4setMinCountOff( CODE4 *c4, int val )
      {
         c4->minCountOff = val ;
      }
   #endif



   void S4FUNCTION c4setErrOpen( CODE4 *c4, int val )
   {
      c4->errOpen = val ;
   }



   void S4FUNCTION c4setErrRelate( CODE4 *c4, int val )
   {
      c4->errRelate = val ;
   }



   void S4FUNCTION c4setErrSkip( CODE4 *c4, int val )
   {
      c4->errSkip = val ;
   }



   void S4FUNCTION c4setErrTagName( CODE4 *c4, int val )
   {
      c4->errTagName = val ;
   }



   #if defined( S4WINDOWS ) && !defined( S4CODE_SCREENS )
      void S4FUNCTION c4setHWnd( CODE4 *c4, HWND val )
      {
         c4->hWnd = val ;
      }
   #else /* S4WINDOWS else */



      void S4FUNCTION c4setHWnd( CODE4 *c4, void *val )
      {
      }
   #endif /* S4WINDOWS else */



   void S4FUNCTION c4setLockAttempts( CODE4 *c4, int val )
   {
      c4->lockAttempts = val ;
   }


   /* LY 2001/09/06 */
   void S4FUNCTION c4setLockAttemptsSingle( CODE4 *c4, int val )
   {
      c4->lockAttemptsSingle = val ;
   }


   /* LY 2001/09/06 */
   void S4FUNCTION c4setLockDelay( CODE4 *c4, int val )
   {
      c4->lockDelay = val ;
   }


   void S4FUNCTION c4setLockEnforce( CODE4 *c4, int val )
   {
      c4->lockEnforce = val ;
   }



   #if defined(S4STAND_ALONE) && !defined(S4OFF_TRAN) && !defined(S4OFF_WRITE)
      void S4FUNCTION c4setLogOpen( CODE4 *c4, int val )
      {
         c4->logOpen = val ;
      }
   #endif /* !S4OFF_WRITE && !S4OFF_TRAN && !S4STAND_ALONE */



   void S4FUNCTION c4setLog( CODE4 *c4, int val )
   {
      c4->log = val ;
   }



   void S4FUNCTION c4setOledbSchemaCreate( CODE4 *c4, char val )
   {
      c4->oledbSchemaCreate = val ;
   }



   void S4FUNCTION c4setOptimize( CODE4 *c4, int val )
   {
      c4->optimize = val ;
   }



   void S4FUNCTION c4setOptimizeWrite( CODE4 *c4, int val )
   {
      c4->optimizeWrite = val ;
   }



   void S4FUNCTION c4setReadLockDo( CODE4 *c4, char val )
   {
      c4->readLock = val ;
   }



   void S4FUNCTION c4setReadOnlyDo( CODE4 *c4, int val )
   {
      c4->readOnly = val ;
   }



   void S4FUNCTION c4setSafety( CODE4 *c4, char val )
   {
      c4->safety = val ;
   }



   void S4FUNCTION c4setAttemptUpgrade( CODE4 S4PTR *c4, short val )
   {
      c4->attemptUpgrade = val ;
   }



   void S4FUNCTION c4setSingleOpen( CODE4 *c4, short val )
   {
      c4->singleOpen = val ;
   }



   void S4FUNCTION c4setErrDefaultUnique( CODE4 *c4, short val )
   {
      c4->errDefaultUnique = val ;
   }



   void S4FUNCTION c4setCodePage( CODE4 *c4, int val )
   {
      // AS Sep 25/03 - Ensure it is being set to a supported value
      switch( val )
      {
         case cp0:
         case cp0004:
         case cp437:
         // AS Jun 9/04 - Support for CodePage 850
         case cp850:
         case cp1250:
         case cp1252:
            c4->codePage = val ;
            break ;
         default:
            error4describe( c4, e4parm, E96702, "unsupported CodePage", 0, 0 ) ;
            break ;
      }
   }



   int S4FUNCTION c4getCodePage( const CODE4 *c4 )
   {
      return c4->codePage ;
   }



   void S4FUNCTION c4setCollatingSequence( CODE4 *c4, int val )
   {
      // AS Sep 25/03 - Ensure it is being set to a supported value
      switch( val )
      {
         case sort4machine:
         case sort4general:
         case sort4croatian:
         case sort4spanish:  // AS Jun 30/08 - support for spanish collation
         case sort4croatianUpper:
            c4->collatingSequence = val ;
            break ;
         default:
            error4describe( c4, e4parm, E96702, "unsupported Collating Sequence", 0, 0 ) ;
            break ;
      }
   }



   void S4FUNCTION c4setMemSizeBuffer( CODE4 *c4, unsigned val )
   {
      c4->memSizeBuffer = val ;
   }



   int S4FUNCTION c4getClipperKeyLen( const CODE4 *c4 )
   {
      #ifdef S4CLIPPER
         return c4->numericStrLen ;
      #else
         return error4( 0, e4notSupported, 0 ) ;  // can't use CODE4 because input as const...(can't set error code)
      #endif
   }



   void S4FUNCTION code4collateNameSet( CODE4 *c4, Collate4name val )
   {
      #ifdef S4CLIENT_OR_FOX
         c4->collateName = val ;
      #endif
   }


   Collate4name S4FUNCTION code4collateName ( CODE4 *c4 )
   {
      #ifdef S4CLIENT_OR_FOX
         return c4->collateName ;
      #else
         return collate4none ;
      #endif
   }


   void S4FUNCTION code4collateNameUnicodeSet( CODE4 *c4, Collate4name val )
   {
      #ifdef S4CLIENT_OR_FOX
         c4->collateNameUnicode = val ;
      #endif
   }


   Collate4name S4FUNCTION code4collateNameUnicode ( CODE4 *c4 )
   {
      #ifdef S4CLIENT_OR_FOX
         return c4->collateNameUnicode ;
      #else
         return collate4none ;
      #endif
   }
#endif /* !S4SERVER */



#if defined( S4SERVER ) && defined( S4FOX )
   // CS 1999/08/26 return short for VB
   short S4FUNCTION code4indexBlockSizeSet( CODE4 S4PTR *c4, short val )
   {
      /* does not set error code, but returns e4parm if invalid value given */
      assert5( c4 != 0 ) ;
      SERVER4CLIENT *clientToUse = c4->currentClient ;
      if ( clientToUse == 0 )
         clientToUse = c4->catalogClient ;
      if ( val == -1 )  // means set it to foxpro compatible defaults
      {
         clientToUse->foxCreateIndexBlockSize = 512 ;
         clientToUse->foxCreateIndexMultiplier = 1 ;
      }
      else
      {
         // must be greater than or equal to 512
         if ( val >= 512 )
         {
            // must be a multiple of 2...
            for ( short valToCheck = val ;; )
            {
               if ( valToCheck == 512 )   // is a multiple of 2...
                  break ;
               if ( valToCheck % 2 != 0 ) // not a multiple of 2...
                  return e4parm ;
               valToCheck /= 2 ;
            }

            clientToUse->foxCreateIndexBlockSize = val ;
            // if not-compatible, we set multiplier to 1024 (or 512 if val is 512, since must be a multiple of this)
            if ( val == 512 )
               clientToUse->foxCreateIndexMultiplier = 512 ;
            else
               clientToUse->foxCreateIndexMultiplier = 1024 ;
         }
         else
            return e4parm ;
      }
      return 0 ;
   }
#endif /* S4SERVER && S4FOX */



#if !defined( S4SERVER )
   short S4FUNCTION code4getErrorCode( const CODE4 *cb )
   {
      #ifdef E4PARM_HIGH
         if ( cb == 0 )
            return error4( 0, e4parm_null, E96702 ) ;
      #endif
      return (short)cb->errorCode ;
   }
#endif /* #if !defined( S4SERVER ) */



#if !defined( S4SERVER )
   long S4FUNCTION code4getErrorCode2( const CODE4 *cb )
   {
      #ifdef E4PARM_HIGH
         if ( cb == 0 )
            return error4( 0, e4parm_null, E96702 ) ;
      #endif
      return cb->errorCode2 ;
   }
#endif /* #if !defined( S4SERVER ) */


#ifdef S4SERVER
   void S4FUNCTION c4setRunAsService( CODE4 *c4, int val )
   {
      c4->runAsService = val ;
   }
#endif /* S4SERVER */


#ifndef S4SERVER
   // AS Feb 12/03 - Added new switch odbc for internal use.
   void S4FUNCTION c4setOdbc( CODE4 *c4, Bool5 val )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E96702 ) ;
            return ;
         }
      #endif
      c4->odbc = val ;
   }
#endif



// AS Jan 9/02 - code written for use via OLE-DB to timeout on accepting a connection (was previously a define
// set to 300 seconds).
#if defined( S4WIN32 ) && !defined( S4OFF_THREAD ) && defined( S4CLIENT )
   void S4FUNCTION c4setAcceptTimeOut( CODE4 *c4, long val )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E96702 ) ;
            return ;
         }
      #endif
      // setting this value to 0 or -1 means to retry forever
      // any value is valid, but at the low level we actually perform a connection retry in 2 second intervals, a minimum
      // of twise.  So any value between 1 and 3 is equivalent to 4.
      c4->acceptTimeOut = val ;
   }
#endif


// AS Jun 24/02 - function to perform data movement counting for testing
#if !defined( S4CLIENT ) && (defined( E4ANALYZE ) || defined( S4TESTING ))
   void S4FUNCTION code4countPosReset( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E96702 ) ;
            return ;
         }
      #endif
      // reset count to 0
      #ifdef S4SERVER
         c4->currentClient->testCount = 0 ;
      #else
         c4->testCount = 0 ;
      #endif
   }



   unsigned long S4FUNCTION code4countPosGet( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E96702 ) ;
            return 0 ;
         }
      #endif
      // return current count
      #ifdef S4SERVER
         return c4->currentClient->testCount ;
      #else
         return c4->testCount ;
      #endif
   }
#endif



// AS Jan 24/03 - isolate encryption to a dll
void *S4FUNCTION c4getPreprocess( CODE4 *c4 )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
      {
         error4( 0, e4parm_null, E96702 ) ;
         return 0 ;
      }
   #endif

   #if defined( S4ENCRYPT_FILE ) || ( defined( S4PREPROCESS_COM ) && defined ( S4CLIENT ) )
       #ifdef S4ENCRYPT_DLL   // LY Jul 16/04
          return c4->encrypt->preprocess ;
       #else
         return c4->encryptPreprocess ;
       #endif
   #else
      return 0 ;
   #endif
}


Bool5 S4FUNCTION c4getFileAccessed( CODE4 *c4 )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
      {
         error4( 0, e4parm_null, E96702 ) ;
         return -1 ;
      }
   #endif

   #if defined( S4PREPROCESS_FILE  ) && defined( S4STAND_ALONE )
      return c4->fileAccessed ;
   #else
      return 0 ;
   #endif
}



#if defined( S4ENCRYPT_DLL ) && defined( S4PREPROCESS_FILE )
   int S4FUNCTION code4setPreprocessFile( CODE4 *c4, short val )  // LY Dec 11/03 : added S4FUNCTION
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E96702 ) ;
      #endif
      // AS May 20/03 - avoid server compile error
      // AS Jul 16/03 - this was wrnong, just don't load
      if ( c4->encrypt == 0 )
      {
         #ifdef S4SERVER
            return e4notSupported ;
         #else
            {
               // AS Apr 24/03 - if not initialized, attempt to do so
               int rc = code4encryptInit( c4, 0, -1 ) ;
               if ( rc != 0 )
                  return rc ;
            }
         #endif
      }
      return c4->encrypt->setEncryptFile( c4, val ) ;
   }
#endif


// AS Apr 11/03 - New feature allows unlock appending as well as locking of clones
#ifndef S4SERVER
   void S4FUNCTION c4setShareCloneLocks( CODE4 *c4, short val )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( 0, e4parm_null, E96702 ) ;
            return ;
         }
      #endif
      c4->shareCloneLocks = val ;
   }
#endif



long S4FUNCTION c4getTimeout( CODE4 *c4 )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
         return error4( 0, e4parm_null, E96702 ) ;
   #endif

   #ifdef S4STAND_ALONE
      return e4notSupported ;
   #else
      return c4->timeout ;
   #endif
}



// AS Apr 27/04 - need functions to set/retrieve decimals from odbc
int S4FUNCTION c4getDecimals( CODE4 *c4 )
{
   return c4->decimals ;
}



void S4FUNCTION c4setDecimals( CODE4 *c4, int numDec )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
      {
         error4( 0, e4parm_null, E96702 ) ;
         return ;
      }
   #endif
   c4->decimals = numDec ;
}


// AS Jan 26/06 - Support for limiting the key size
void S4FUNCTION code4limitKeySizeSet( CODE4 *c4, short val )
{
   #ifdef E4PARM_HIGH
      if ( c4 == 0 )
      {
         error4( 0, e4parm_null, E96702 ) ;
         return ;
      }
   #endif
   c4->limitKeySize = (char)val ;  // CS 2007/04/27 Cast to char to avoid warning.
}
