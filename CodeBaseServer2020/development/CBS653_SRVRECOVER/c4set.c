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



#ifndef S4SERVER
   // CS 1999/08/26 return short for VB
   short S4FUNCTION code4indexBlockSizeSet( CODE4 *c4, short val )
   {
      #if defined( S4CLIENT_OR_FOX ) && !defined( S4OFF_INDEX ) && !defined( S4PALM )
         if ( s5fox )
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



   #ifdef S4WIN32
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



   void S4FUNCTION c4setAutoOpen( CODE4 *c4, int val )
   {
      c4->autoOpen = val ;
   }



   void S4FUNCTION code4verifySet( CODE4 *c4, const char *val )
   {
      if ( c4->applicationVerify == 0 )
         c4->applicationVerify = (char *)u4allocFree( c4, 250 ) ;
      if ( c4->applicationVerify != 0 )
         c4strncpy( c4->applicationVerify, val, 249 ) ;
   }



   void S4FUNCTION c4setCompatibility( CODE4 *c4, short val )
   {
      c4->compatibility = val ;
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
      c4->codePage = val ;
   }



   int S4FUNCTION c4getCodePage( const CODE4 *c4 )
   {
      return c4->codePage ;
   }



   void S4FUNCTION c4setCollatingSequence( CODE4 *c4, int val )
   {
      c4->collatingSequence = val ;
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
      return (short)cb->errorCode ;
   }
#endif /* #if !defined( S4SERVER ) */



#if !defined( S4SERVER )
   long S4FUNCTION code4getErrorCode2( const CODE4 *cb )
   {
      return cb->errorCode2 ;
   }
#endif /* #if !defined( S4SERVER ) */


#ifdef S4SERVER
   void S4FUNCTION c4setRunAsService( CODE4 *c4, int val )
   {
      c4->runAsService = val ;
   }
#endif /* S4SERVER */


void S4FUNCTION code4errorCallback( CODE4 S4PTR *c4, ERROR_CALLBACK errorCallback )  // CS 2001/03/26 add
{
   c4->errorCallback = errorCallback ;
}
