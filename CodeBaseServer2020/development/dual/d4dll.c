/* d4dll.c   (c)Copyright Sequiter Software Inc., 2001.  All rights reserved. */

/*
   This adds support for dual dlls (both stand alone and client server simultaneous)
   It gets included instead of d4all.h by the application

   KNOWN PROBLEMS:

   - memory alloc'd by file4open/file4create must be freed by file4close/file4replace

*/


#include "d4dll.h"

#ifndef D4DLL_INC /* declare below only once when compiling multiple files */

#define D4DLL_INC

CODE4 *g_c4 = 0 ;   // we need a global accessor for functions which don't have a c4 available
HANDLE hG_c4 ;
long numC4 = 0 ;
LIST4 c4list ; /* LY 2002/06/17 : to avoid c4parm_check errors from passing
               wrong CODE4 to global C4 function (e.g. g_c4 has client
               code4errOffLow() but is passed stand-alone CODE4) */

void S4FUNCTION c4getFuncPtr( HINSTANCE hInst, const char * funcName, void **function, long *rc )
{
   // gets the pointer and sets rc to -1 if a failure
   *function = 0 ;
   *function = (void *)GetProcAddress( hInst, funcName ) ;
   if ( *function == 0 )
      *rc = -1 ;
}

int S4FUNCTION c4getAccessMode( const CODE4 S4PTR *c4 )
{
   return c4->c4getAccessMode( c4->c4 ) ;
}

int S4FUNCTION c4getAutoOpen( const CODE4 S4PTR *c4 )
{
   return c4->c4getAutoOpen( c4->c4 ) ;
}

int S4FUNCTION c4getCodePage( const CODE4 S4PTR *c4 )
{
   return c4->c4getCodePage( c4->c4 ) ;
}

short S4FUNCTION c4getCompatibility( const CODE4 S4PTR *c4 )
{
   return c4->c4getCompatibility( c4->c4 ) ;
}

int S4FUNCTION c4getCreateTemp( const CODE4 S4PTR *c4 )
{
   return c4->c4getCreateTemp( c4->c4 ) ;
}

int S4FUNCTION c4getErrCreate( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrCreate( c4->c4 ) ;
}

int S4FUNCTION c4getErrDefaultUnique( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrDefaultUnique( c4->c4 ) ;
}

int S4FUNCTION c4getErrExpr( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrExpr( c4->c4 ) ;
}

int S4FUNCTION c4getErrFieldName( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrFieldName( c4->c4 ) ;
}

int S4FUNCTION c4getErrGo( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrGo( c4->c4 ) ;
}

int S4FUNCTION c4getErrOff( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrOff( c4->c4 ) ;
}

int S4FUNCTION c4getErrOpen( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrOpen( c4->c4 ) ;
}

int S4FUNCTION c4getErrorCode( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrorCode( c4->c4 ) ;
}

int S4FUNCTION c4getErrRelate( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrRelate( c4->c4 ) ;
}

int S4FUNCTION c4getErrSkip( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrSkip( c4->c4 ) ;
}

int S4FUNCTION c4getErrTagName( const CODE4 S4PTR *c4 )
{
   return c4->c4getErrTagName( c4->c4 ) ;
}

int S4FUNCTION c4getFileFlush( const CODE4 S4PTR *c4 )
{
   return c4->c4getFileFlush( c4->c4 ) ;
}

int S4FUNCTION c4getLockAttempts( const CODE4 S4PTR *c4 )
{
   return c4->c4getLockAttempts( c4->c4 ) ;
}

int S4FUNCTION c4getLockAttemptsSingle( const CODE4 S4PTR *c4 )
{
   return c4->c4getLockAttempts( c4->c4 ) ;
}

int S4FUNCTION c4getLockDelay( const CODE4 S4PTR *c4 )
{
   return c4->c4getLockDelay( c4->c4 ) ;
}

int S4FUNCTION c4getLockEnforce( const CODE4 S4PTR *c4 )
{
   return c4->c4getLockEnforce( c4->c4 ) ;
}

int S4FUNCTION c4getLog( const CODE4 S4PTR *c4 )
{
   return c4->c4getLog( c4->c4 ) ;
}

int S4FUNCTION c4getOptimize( const CODE4 S4PTR *c4 )
{
   return c4->c4getOptimize( c4->c4 ) ;
}

int S4FUNCTION c4getOptimizeWrite( const CODE4 S4PTR *c4 )
{
   return c4->c4getOptimizeWrite( c4->c4 ) ;
}

int S4FUNCTION c4getReadLockDo( const CODE4 S4PTR *c4 )
{
   return c4->c4getReadLockDo( c4->c4 ) ;
}

int S4FUNCTION c4getReadOnlyDo( const CODE4 S4PTR *c4 )
{
   return c4->c4getReadOnlyDo( c4->c4 ) ;
}

int S4FUNCTION c4getSafety( const CODE4 S4PTR *c4 )
{
   return c4->c4getSafety( c4->c4 ) ;
}

int S4FUNCTION c4getSingleOpen( const CODE4 S4PTR *c4 )
{
   return c4->c4getSingleOpen( c4->c4 ) ;
}


void S4FUNCTION c4setAccessMode( CODE4 *c4, char val )
{
   c4->c4setAccessMode( c4->c4, val ) ;
}

void S4FUNCTION c4setAcceptTimeOut( CODE4 *c4, long val )
{
   c4->c4setAcceptTimeOut( c4->c4, val ) ;
}

void S4FUNCTION c4setAutoOpen( CODE4 *c4, int val )
{
   c4->c4setAutoOpen( c4->c4, val ) ;
}

void S4FUNCTION c4setCodePage( CODE4 S4PTR *c4, int val )
{
   c4->c4setCodePage( c4->c4, val ) ;
}

void S4FUNCTION c4setCompatibility( CODE4 S4PTR *c4, short val )
{
   c4->c4setCompatibility( c4->c4, val ) ;
}

void S4FUNCTION c4setCreateTemp( CODE4 S4PTR *c4, int val )
{
   c4->c4setCreateTemp( c4->c4, val ) ;
}

void S4FUNCTION c4setErrCreate( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrCreate( c4->c4, val ) ;
}

void S4FUNCTION c4setErrDefaultUnique( CODE4 S4PTR *c4, short val )
{
   c4->c4setErrDefaultUnique( c4->c4, val ) ;
}

void S4FUNCTION c4setErrExpr( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrExpr( c4->c4, val ) ;
}

void S4FUNCTION c4setErrGo( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrGo( c4->c4, val ) ;
}

void S4FUNCTION c4setErrOff( CODE4 *c4, int val )
{
   c4->c4setErrOff( c4->c4, val ) ;
}

void S4FUNCTION c4setErrFieldName( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrFieldName( c4->c4, val ) ;
}

void S4FUNCTION c4setErrOpen( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrOpen( c4->c4, val ) ;
}

void S4FUNCTION c4setErrorCode( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrorCode( c4->c4, val ) ;
}

void S4FUNCTION c4setErrRelate( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrRelate( c4->c4, val ) ;
}

void S4FUNCTION c4setErrSkip( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrSkip( c4->c4, val ) ;
}

void S4FUNCTION c4setErrTagName( CODE4 S4PTR *c4, int val )
{
   c4->c4setErrTagName( c4->c4, val ) ;
}

void S4FUNCTION c4setFileFlush( CODE4 S4PTR *c4, int val )
{
   c4->c4setFileFlush( c4->c4, val ) ;
}

void S4FUNCTION c4setLockAttempts( CODE4 S4PTR *c4, int val )
{
   c4->c4setLockAttempts( c4->c4, val ) ;
}

void S4FUNCTION c4setLockAttemptsSingle( CODE4 S4PTR *c4, int val )
{
   c4->c4setLockAttemptsSingle( c4->c4, val ) ;
}

void S4FUNCTION c4setLockDelay( CODE4 S4PTR *c4, int val )
{
   c4->c4setLockDelay( c4->c4, val ) ;
}

void S4FUNCTION c4setLockEnforce( CODE4 S4PTR *c4, int val )
{
   c4->c4setLockEnforce( c4->c4, val ) ;
}

void S4FUNCTION c4setLog( CODE4 S4PTR *c4, int val )
{
   c4->c4setLog( c4->c4, val ) ;
}

void S4FUNCTION c4setOptimize( CODE4 S4PTR *c4, int val )
{
   c4->c4setOptimize( c4->c4, val ) ;
}

void S4FUNCTION c4setOptimizeWrite( CODE4 S4PTR *c4, int val )
{
   c4->c4setOptimizeWrite( c4->c4, val ) ;
}

void S4FUNCTION c4setReadLockDo( CODE4 S4PTR *c4, char val)
{
   c4->c4setReadLockDo( c4->c4, val ) ;
}

void S4FUNCTION c4setReadOnlyDo( CODE4 S4PTR *c4, int val)
{
   c4->c4setReadOnlyDo( c4->c4, val ) ;
}

void S4FUNCTION c4setSafety( CODE4 *c4, char val )
{
   c4->c4setSafety( c4->c4, val ) ;
}

void S4FUNCTION c4setSingleOpen( CODE4 S4PTR *c4, short val )
{
   c4->c4setSingleOpen( c4->c4, val ) ;
}

double S4FUNCTION c4atod( const char S4PTR *date, const int val )
{
   return g_c4->c4atod( date, val ) ;
}

int S4FUNCTION c4atoi( const char S4PTR *date, const int val )
{
   return g_c4->c4atoi( date, val ) ;
}

long S4FUNCTION c4atol( const char S4PTR *ptr, const int val)
{
   return g_c4->c4atol( ptr, val ) ;
}

int S4FUNCTION c4dtoa45( double dblVal, char *outBuffer, int len, int dec )
{
   return g_c4->c4dtoa45( dblVal, outBuffer, len, dec ) ;
}

void S4FUNCTION c4encode( char S4PTR *to, const char S4PTR *f, char S4PTR *tTo, const char S4PTR *tF )
{
   g_c4->c4encode( to, f, tTo, tF ) ;
}

void S4FUNCTION c4ltoa45( long lVal, char *ptr, int num )
{
   g_c4->c4ltoa45( lVal, ptr, num ) ;
}

void S4FUNCTION c4trimN( char S4PTR *str, int nCh )
{
   g_c4->c4trimN( str, nCh ) ;
}

void S4FUNCTION c4upper( char *str )
{
   g_c4->c4upper( str ) ;
}

CODE4 * S4FUNCTION code4allocDll( const char *dllName )
{
   long rc = 0 ;

   CODE4 *c4 = (CODE4 *)malloc( sizeof( CODE4 ) ) ;
   if ( c4 == 0 )
      return 0 ;
   memset( c4, 0, sizeof( CODE4 ) ) ;

   c4->hInst = LoadLibrary( dllName ) ;
   if ( c4->hInst == NULL )
   {
      DWORD err = GetLastError();
      free( c4 ) ;
      return 0 ;
   }

   c4getFuncPtr( c4->hInst, "code4allocLow", (void **)&(c4->code4alloc), &rc ) ;
   if ( c4->code4alloc != 0 )
   {
      c4->c4 = c4->code4alloc( 1, DEF4PROTOCOL, S4VERSION ) ;
      if ( c4->c4 == 0 )
         rc = 0 ;
   }

   c4getFuncPtr( c4->hInst, "code4accessMode", (void **)&(c4->code4accessMode), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4actionCode", (void **)&(c4->code4actionCode), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4autoIncrementStart", (void **)&(c4->code4autoIncrementStart), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4autoOpen", (void **)&(c4->code4autoOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4calcCreate", (void **)&(c4->code4calcCreate), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4calcReset", (void **)&(c4->code4calcReset), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4codePage", (void **)&(c4->code4codePage), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4collate", (void **)&(c4->code4collate), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4collateUnicode", (void **)&(c4->code4collateUnicode), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4collatingSequence", (void **)&(c4->code4collatingSequence), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4compatibility", (void **)&(c4->code4compatibility), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4compressedMemos", (void **)&(c4->code4compressedMemos), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4compress", (void **)&(c4->code4compress), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4compressConfigure", (void **)&(c4->code4compressConfigure), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4compressConfigureServer", (void **)&(c4->code4compressConfigureServer), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4connect", (void **)&(c4->code4connect), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4close", (void **)&(c4->code4close), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4createTemp", (void **)&(c4->code4createTemp), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4dateFormat", (void **)&(c4->code4dateFormat), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4dateFormatSet", (void **)&(c4->code4dateFormatSet), &rc ) ;
   #if defined(S4ENCRYPT_HOOK) || defined(S4ENCRYPT_COM) || defined(S4PREPROCESS_COM) || defined(S4PREPROCESS_FILE)
      c4getFuncPtr( c4->hInst, "code4encryptConnection", (void **)&(c4->code4encryptConnection), &rc ) ;
      c4getFuncPtr( c4->hInst, "code4encryptFile", (void **)&(c4->code4encryptFile), &rc ) ;
   #endif
   c4getFuncPtr( c4->hInst, "code4errCreate", (void **)&(c4->code4errCreate), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errDefaultUnique", (void **)&(c4->code4errDefaultUnique), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errExpr", (void **)&(c4->code4errExpr), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errFieldName", (void **)&(c4->code4errFieldName), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errGo", (void **)&(c4->code4errGo), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errOff", (void **)&(c4->code4errOff), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errorCode", (void **)&(c4->code4errorCode), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errOpen", (void **)&(c4->code4errOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errRelate", (void **)&(c4->code4errRelate), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errSkip", (void **)&(c4->code4errSkip), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4errTagName", (void **)&(c4->code4errTagName), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4exit", (void **)&(c4->code4exit), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4flush", (void **)&(c4->code4flush), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4fileFlush", (void **)&(c4->code4fileFlush), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4hInst", (void **)&(c4->code4hInst), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4hWnd", (void **)&(c4->code4hWnd), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4indexBlockSize", (void **)&(c4->code4indexBlockSize), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4indexBlockSizeSet", (void **)&(c4->code4indexBlockSizeSet), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4indexExtension", (void **)&(c4->code4indexExtension), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4indexFormat", (void **)&(c4->code4indexFormat), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4initUndo", (void **)&(c4->code4initUndo), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4largeOn", (void **)&(c4->code4largeOn), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lock", (void **)&(c4->code4lock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockAttempts", (void **)&(c4->code4lockAttempts), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockAttemptsSingle", (void **)&(c4->code4lockAttemptsSingle), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockClear", (void **)&(c4->code4lockClear), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockDelay", (void **)&(c4->code4lockDelay), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockEnforce", (void **)&(c4->code4lockEnforce), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockFileName", (void **)&(c4->code4lockFileName), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockItem", (void **)&(c4->code4lockItem), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockNetworkId", (void **)&(c4->code4lockNetworkId), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4lockUserId", (void **)&(c4->code4lockUserId), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4log", (void **)&(c4->code4log), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4logCreate", (void **)&(c4->code4logCreate), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4logFileName", (void **)&(c4->code4logFileName), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4logOpen", (void **)&(c4->code4logOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4logOpenOff", (void **)&(c4->code4logOpenOff), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memExpandBlock", (void **)&(c4->code4memExpandBlock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memExpandData", (void **)&(c4->code4memExpandData), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memExpandIndex", (void **)&(c4->code4memExpandIndex), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memExpandLock", (void **)&(c4->code4memExpandLock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memExpandTag", (void **)&(c4->code4memExpandTag), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memoCompress", (void **)&(c4->code4memoCompress), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memSizeBlock", (void **)&(c4->code4memSizeBlock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memSizeBuffer", (void **)&(c4->code4memSizeBuffer), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memSizeMemo", (void **)&(c4->code4memSizeMemo), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memSizeMemoExpr", (void **)&(c4->code4memSizeMemoExpr), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memSizeSortBuffer", (void **)&(c4->code4memSizeSortBuffer), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memSizeSortPool", (void **)&(c4->code4memSizeSortPool), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memStartBlock", (void **)&(c4->code4memStartBlock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memStartData", (void **)&(c4->code4memStartData), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memStartIndex", (void **)&(c4->code4memStartIndex), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memStartLock", (void **)&(c4->code4memStartLock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memStartMax", (void **)&(c4->code4memStartMax), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4memStartTag", (void **)&(c4->code4memStartTag), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4optAll", (void **)&(c4->code4optAll), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4optimize", (void **)&(c4->code4optimize), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4optimizeWrite", (void **)&(c4->code4optimizeWrite), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4optStart", (void **)&(c4->code4optStart), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4optSuspend", (void **)&(c4->code4optSuspend), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4ping", (void **)&(c4->code4ping), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4readLock", (void **)&(c4->code4readLock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4readOnly", (void **)&(c4->code4readOnly), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4safety", (void **)&(c4->code4safety), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4singleOpen", (void **)&(c4->code4singleOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4timeout", (void **)&(c4->code4timeout), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4timeoutSet", (void **)&(c4->code4timeoutSet), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4tranCommit", (void **)&(c4->code4tranCommit), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4tranRollback", (void **)&(c4->code4tranRollback), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4tranStart", (void **)&(c4->code4tranStart), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4tranStatusCB", (void **)&(c4->code4tranStatusCB), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4unlock", (void **)&(c4->code4unlock), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4unlockAutoCB", (void **)&(c4->code4unlockAutoCB), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4unlockAutoSetCB", (void **)&(c4->code4unlockAutoSetCB), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4useGeneralTagsInRelate", (void **)&(c4->code4useGeneralTagsInRelate), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4verifySet", (void **)&(c4->code4verifySet), &rc ) ;
   c4getFuncPtr( c4->hInst, "code4validate", (void **)&(c4->code4validate), &rc ) ;

   c4getFuncPtr( c4->hInst, "c4getAccessMode", (void **)&(c4->c4getAccessMode), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getAutoOpen", (void **)&(c4->c4getAutoOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getCodePage", (void **)&(c4->c4getCodePage), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getCompatibility", (void **)&(c4->c4getCompatibility), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getCreateTemp", (void **)&(c4->c4getCreateTemp), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrCreate", (void **)&(c4->c4getErrCreate), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrDefaultUnique", (void **)&(c4->c4getErrDefaultUnique), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrExpr", (void **)&(c4->c4getErrExpr), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrFieldName", (void **)&(c4->c4getErrFieldName), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrGo", (void **)&(c4->c4getErrGo), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrOff", (void **)&(c4->c4getErrOff), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrOpen", (void **)&(c4->c4getErrOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrorCode", (void **)&(c4->c4getErrorCode), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrRelate", (void **)&(c4->c4getErrRelate), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrSkip", (void **)&(c4->c4getErrSkip), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getErrTagName", (void **)&(c4->c4getErrTagName), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getFileFlush", (void **)&(c4->c4getFileFlush), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getLockAttempts", (void **)&(c4->c4getLockAttempts), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getLockAttemptsSingle", (void **)&(c4->c4getLockAttemptsSingle), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getLockDelay", (void **)&(c4->c4getLockDelay), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getLockEnforce", (void **)&(c4->c4getLockEnforce), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getLog", (void **)&(c4->c4getLog), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getOptimize", (void **)&(c4->c4getOptimize), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getOptimizeWrite", (void **)&(c4->c4getOptimizeWrite), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getReadLockDo", (void **)&(c4->c4getReadLockDo), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getReadOnlyDo", (void **)&(c4->c4getReadOnlyDo), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getSafety", (void **)&(c4->c4getSafety), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4getSingleOpen", (void **)&(c4->c4getSingleOpen), &rc ) ;

   c4getFuncPtr( c4->hInst, "c4setAccessMode", (void **)&(c4->c4setAccessMode), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setAcceptTimeOut", (void **)&(c4->c4setAcceptTimeOut), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setAutoOpen", (void **)&(c4->c4setAutoOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setCodePage", (void **)&(c4->c4setCodePage), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setCompatibility", (void **)&(c4->c4setCompatibility), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setCreateTemp", (void **)&(c4->c4setCreateTemp), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrCreate", (void **)&(c4->c4setErrCreate), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrDefaultUnique", (void **)&(c4->c4setErrDefaultUnique), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrExpr", (void **)&(c4->c4setErrExpr), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrFieldName", (void **)&(c4->c4setErrFieldName), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrGo", (void **)&(c4->c4setErrGo), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrOff", (void **)&(c4->c4setErrOff), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrOpen", (void **)&(c4->c4setErrOpen), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrorCode", (void **)&(c4->c4setErrorCode), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrRelate", (void **)&(c4->c4setErrRelate), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrSkip", (void **)&(c4->c4setErrSkip), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setErrTagName", (void **)&(c4->c4setErrTagName), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setFileFlush", (void **)&(c4->c4setFileFlush), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setLockAttempts", (void **)&(c4->c4setLockAttempts), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setLockAttemptsSingle", (void **)&(c4->c4setLockAttemptsSingle), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setLockDelay", (void **)&(c4->c4setLockDelay), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setLockEnforce", (void **)&(c4->c4setLockEnforce), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setLog", (void **)&(c4->c4setLog), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setOptimize", (void **)&(c4->c4setOptimize), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setOptimizeWrite", (void **)&(c4->c4setOptimizeWrite), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setReadLockDo", (void **)&(c4->c4setReadLockDo), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setReadOnlyDo", (void **)&(c4->c4setReadOnlyDo), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setSafety", (void **)&(c4->c4setSafety), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4setSingleOpen", (void **)&(c4->c4setSingleOpen), &rc ) ;

   c4getFuncPtr( c4->hInst, "c4atod", (void **)&(c4->c4atod), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4atoi", (void **)&(c4->c4atoi), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4atol", (void **)&(c4->c4atol), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4dtoa45", (void **)&(c4->c4dtoa45), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4encode", (void **)&(c4->c4encode), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4ltoa45", (void **)&(c4->c4ltoa45), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4trimN", (void **)&(c4->c4trimN), &rc ) ;
   c4getFuncPtr( c4->hInst, "c4upper", (void **)&(c4->c4upper), &rc ) ;

   c4getFuncPtr( c4->hInst, "date4assignLow", (void **)&(c4->date4assignLow), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4cdow", (void **)&(c4->date4cdow), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4cmonth", (void **)&(c4->date4cmonth), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4dow", (void **)&(c4->date4dow), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4format", (void **)&(c4->date4format), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4formatMdx", (void **)&(c4->date4formatMdx), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4init", (void **)&(c4->date4init), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4isLeap", (void **)&(c4->date4isLeap), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4long", (void **)&(c4->date4long), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4today", (void **)&(c4->date4today), &rc ) ;
   c4getFuncPtr( c4->hInst, "date4timeNow", (void **)&(c4->date4timeNow), &rc ) ;

   c4getFuncPtr( c4->hInst, "error4callback", (void **)&(c4->error4callback), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4default", (void **)&(c4->error4default), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4describeDefault", (void **)&(c4->error4describeDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4describeVB", (void **)&(c4->error4describeVB), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4exitTest", (void **)&(c4->error4exitTest), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4file", (void **)&(c4->error4file), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4lastDescription", (void **)&(c4->error4lastDescription), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4set", (void **)&(c4->error4set), &rc ) ;
   c4getFuncPtr( c4->hInst, "error4text", (void **)&(c4->error4text), &rc ) ;

   c4getFuncPtr( c4->hInst, "file4alloc", (void **)&(c4->file4alloc), &rc ) ;

   c4getFuncPtr( c4->hInst, "l4addAfter", (void **)&(c4->l4addAfter), &rc ) ;
   c4getFuncPtr( c4->hInst, "l4addBefore", (void **)&(c4->l4addBefore), &rc ) ;
   c4getFuncPtr( c4->hInst, "l4prev", (void **)&(c4->l4prev), &rc ) ;
   c4getFuncPtr( c4->hInst, "l4remove", (void **)&(c4->l4remove), &rc ) ;

   c4getFuncPtr( c4->hInst, "mem4allocDefault", (void **)&(c4->mem4allocDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "mem4createDefault", (void **)&(c4->mem4createDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "mem4freeDefault", (void **)&(c4->mem4freeDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "mem4release", (void **)&(c4->mem4release), &rc ) ;

   c4getFuncPtr( c4->hInst, "sort4alloc", (void **)&(c4->sort4alloc), &rc ) ;
   c4getFuncPtr( c4->hInst, "sort4free2", (void **)&(c4->sort4free2), &rc ) ;
   c4getFuncPtr( c4->hInst, "sort4init", (void **)&(c4->sort4init), &rc ) ;

   c4getFuncPtr( c4->hInst, "u4allocDefault", (void **)&(c4->u4allocDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4allocAgainDefault", (void **)&(c4->u4allocAgainDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4allocErDefault", (void **)&(c4->u4allocErDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4allocFreeDefault", (void **)&(c4->u4allocFreeDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4freeDefault", (void **)&(c4->u4freeDefault), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4memCpy", (void **)&(c4->u4memCpy), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4nameChar", (void **)&(c4->u4nameChar), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4nameExt", (void **)&(c4->u4nameExt), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4namePiece", (void **)&(c4->u4namePiece), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4ncpy", (void **)&(c4->u4ncpy), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4remove", (void **)&(c4->u4remove), &rc ) ;
   c4getFuncPtr( c4->hInst, "u4yymmdd", (void **)&(c4->u4yymmdd), &rc ) ;

   c4getFuncPtr( c4->hInst, "v4Cstring", (void **)&(c4->v4Cstring), &rc ) ;
   c4getFuncPtr( c4->hInst, "v4Cstringfree", (void **)&(c4->v4Cstringfree), &rc ) ;

   l4init( &c4->d4list ) ;
   c4->hD4List = CreateEvent( 0, 1, 1, "hD4List" ) ;

   if ( rc == -1 )
   {
      if ( c4->c4 != 0 && c4->code4initUndo != 0 )
         c4->code4initUndo( c4->c4 ) ;
      FreeLibrary( c4->hInst ) ;
      free( c4 ) ;
      c4 = 0 ;
      return 0 ;
   }

   hG_c4 = CreateEvent( 0, 1, 1, "hG_c4" ) ;
   ResetEvent( hG_c4 ) ;
   if ( g_c4 == 0 )
   {
      g_c4 = c4 ;
      l4init( &c4list ) ;
   }
   l4add( &c4list, c4 ) ;
   numC4++ ;
   SetEvent( hG_c4 ) ;

   return c4 ;
}

short S4FUNCTION code4accessMode( CODE4 *cb, short value )
{
   return cb->code4accessMode( cb->c4, value ) ;
}

unsigned short S4FUNCTION code4actionCode( CODE4 S4PTR *cb )
{
   return cb->code4actionCode( cb->c4 ) ;
}

void S4FUNCTION code4autoIncrementStart( CODE4 S4PTR *cb, double value )
{
   cb->code4autoIncrementStart( cb->c4, value ) ;
}

short S4FUNCTION code4autoOpen( CODE4 *cb, short value )
{
   return cb->code4autoOpen( cb->c4, value ) ;
}

short S4FUNCTION code4codePage( CODE4 *cb, short value )
{
   return cb->code4codePage( cb->c4, value ) ;
}

short S4FUNCTION code4collateUnicode( CODE4 S4PTR *c4, short collateType )
{
   return c4->code4collateUnicode( c4->c4, collateType ) ;
}

short S4FUNCTION code4collatingSequence( CODE4 *cb, short value )
{
   return cb->code4collatingSequence( cb->c4, value ) ;
}

short S4FUNCTION code4compatibility( CODE4 *cb, short value )
{
   return cb->code4compatibility( cb->c4, value ) ;
}

int S4FUNCTION code4compress( CODE4 *cb, short value )
{
   return cb->code4compress( cb->c4, value ) ;
}

int S4FUNCTION code4compressConfigure( CODE4 *cb, short fileLevel, short comLevel, short minLength )
{
   return cb->code4compressConfigure( cb->c4, fileLevel, comLevel, minLength ) ;
}

int S4FUNCTION code4compressConfigureServer( CODE4 *cb, short fileLevel, short comLevel, short minLength )
{
   return cb->code4compressConfigureServer( cb->c4, fileLevel, comLevel, minLength ) ;
}

short S4FUNCTION code4createTemp( CODE4 *cb, short value )
{
   return cb->code4createTemp( cb->c4, value ) ;
}

int S4FUNCTION code4encryptConnection( CODE4 *c4, int level, int numBits )
{
   return c4->code4encryptConnection( c4->c4, level, numBits ) ;
}

void S4FUNCTION code4encryptFile( CODE4 *c4, short flag )
{
   c4->code4encryptFile( c4->c4, flag ) ;
}

short S4FUNCTION code4errCreate( CODE4 *cb, short value )
{
   return cb->code4errCreate( cb->c4, value ) ;
}

short S4FUNCTION code4errDefaultUnique( CODE4 *cb, short value )
{
   return cb->code4errDefaultUnique( cb->c4, value ) ;
}

short S4FUNCTION code4errExpr( CODE4 *cb, short value )
{
   return cb->code4errExpr( cb->c4, value ) ;
}

short S4FUNCTION code4errFieldName( CODE4 *cb, short value )
{
   return cb->code4errFieldName( cb->c4, value ) ;
}

short S4FUNCTION code4errGo( CODE4 *cb, short value )
{
   return cb->code4errGo( cb->c4, value ) ;
}

short S4FUNCTION code4errOff( CODE4 *cb, short value )
{
   return cb->code4errOff( cb->c4, value ) ;
}

short S4FUNCTION code4errOffLow( void *cb, short value )
{
   CODE4 *currC4 = 0 ;
   int found = 0 ;

   ResetEvent( hG_c4 ) ;
   for( currC4 = (CODE4 *)l4next( &c4list, currC4 ); currC4 != 0 && !found; )
   {
      if ( currC4->c4 == cb )
         found = 1 ;
      else
         currC4 = (CODE4 *)l4next( &c4list, currC4 ) ;
   }
   SetEvent( hG_c4 ) ;

   if ( currC4 )
      return currC4->code4errOff( cb, value ) ;
   else
      return g_c4->error4default( g_c4->c4, e4parm, 99999L ) ;
}

short S4FUNCTION code4errOpen( CODE4 *cb, short value )
{
   return cb->code4errOpen( cb->c4, value ) ;
}

short S4FUNCTION code4errorCode( CODE4 *cb, short value )
{
   return cb->code4errorCode( cb->c4, value ) ;
}

short S4FUNCTION code4errRelate( CODE4 *cb, short value )
{
   return cb->code4errRelate( cb->c4, value ) ;
}

short S4FUNCTION code4errSkip( CODE4 *cb, short value )
{
   return cb->code4errSkip( cb->c4, value ) ;
}

short S4FUNCTION code4errTagName( CODE4 *cb, short value )
{
   return cb->code4errTagName( cb->c4, value ) ;
}

short S4FUNCTION code4fileFlush( CODE4 *cb, short value )
{
   return cb->code4fileFlush( cb->c4, value ) ;
}

int S4FUNCTION code4initUndo( CODE4 *c4 )
{
   DATA4 *d4;
   int rc = 0 ;
   WaitForSingleObject( c4->hD4List, INFINITE ) ;
   ResetEvent( c4->hD4List ) ;
   // AS Jan 27/06 -  merged from other project...must not be called closeAll which conflicts for C++ with function name
   c4->closeAllList = 1 ;
   for ( d4 = (DATA4 *)l4first( &c4->d4list ); d4 != 0 ; d4 = (DATA4 *)l4first( &c4->d4list ) )
      d4close( d4 ) ;   /* use wrapper version of d4close() to free memory from d4***() */
   c4->closeAllList = 0 ;
   SetEvent( c4->hD4List ) ;


   WaitForSingleObject( hG_c4, INFINITE ) ;
   ResetEvent( hG_c4 ) ;
   l4remove( &c4list, c4 ) ;
   numC4-- ;
   if ( c4 != g_c4 )
   {
      rc = c4->code4initUndo( c4->c4 ) ;
      if ( rc != 0 )
         return rc ;
      FreeLibrary( c4->hInst ) ; /* LY 2002/02/15 : missing FreeLibrary */
      CloseHandle( c4->hD4List ) ;
      free( c4 ) ;
   }

   if ( numC4 == 0 )
   {
      rc = g_c4->code4initUndo( g_c4->c4 ) ;
      if ( rc != 0 )
      {
         SetEvent( hG_c4 ) ;
         return rc ;
      }
      FreeLibrary( g_c4->hInst ) ;   /* LY 2002/02/15 : missing FreeLibrary */
      CloseHandle( g_c4->hD4List ) ;
      /* LY 2002/05/29 : if two processes running, not signalling hG_c4 before
         closing will block other process */
      SetEvent( hG_c4 ) ;
      CloseHandle( hG_c4 ) ;  /* LY 2002/02/20 : not closing hG_c4 */
      free( g_c4 ) ;
      g_c4 = 0 ;
   }
   else
      SetEvent( hG_c4 ) ;

   return rc ;
}


/* memory leak if c4 not global c4 */
int S4FUNCTION code4initUndoLow( void *c4 )
{
   DATA4 *d4;
   int rc = 0 ;

   if ( g_c4->c4 == c4 )
   {
      WaitForSingleObject( g_c4->hD4List, INFINITE ) ;
      ResetEvent( g_c4->hD4List ) ;
      // AS Jan 27/06 -  merged from other project...must not be called closeAll which conflicts for C++ with function name
      g_c4->closeAllList = 1 ;
      for ( d4 = (DATA4 *)l4first( &g_c4->d4list ); d4 != 0 ; d4 = (DATA4 *)l4first( &g_c4->d4list ) )
         d4close( d4 ) ;   /* use wrapper version of d4close() to free memory from d4***() */
      g_c4->closeAllList = 0 ;
      SetEvent( g_c4->hD4List ) ;
   }

   if ( c4 != g_c4->c4 )
   {
      rc = g_c4->code4initUndo( c4 ) ;
      if ( rc != 0 )
         return rc ;
   }

   WaitForSingleObject( hG_c4, INFINITE ) ;
   ResetEvent( hG_c4 ) ;
   numC4-- ;
   if ( numC4 == 0 )
   {
      rc = g_c4->code4initUndo( g_c4->c4 ) ;
      if ( rc != 0 )
      {
         SetEvent( hG_c4 ) ;
         return rc ;
      }
      FreeLibrary( g_c4->hInst ) ;   /* LY 2002/02/15 : missing FreeLibrary */
      CloseHandle( g_c4->hD4List ) ;
      /* LY 2002/05/29 : if two processes running, not signalling hG_c4 before
         closing will block other process */
      SetEvent( hG_c4 ) ;
      CloseHandle( hG_c4 ) ;  /* LY 2002/02/20 : not closing hG_c4 */
      free( g_c4 ) ;
      g_c4 = 0 ;
   }
   else
      SetEvent( hG_c4 ) ;

   return rc ;
}


int S4FUNCTION code4logCreate( CODE4 *c4, const char *fileName, const char *userId )
{
   return c4->code4logCreate( c4->c4, fileName, userId ) ;
}

int S4FUNCTION code4logOpen( CODE4 *c4, const char *fileName, const char *userId )
{
   return c4->code4logOpen( c4->c4, fileName, userId ) ;
}

EXPR4CALC S4FUNCTION code4calcCreate( CODE4 S4PTR *c4, EXPR4 S4PTR *expr, const char S4PTR *name )
{
   return c4->code4calcCreate( c4->c4, expr->e4, name ) ;
}

void S4FUNCTION code4calcReset( CODE4 S4PTR *c4 )
{
   c4->code4calcReset( c4->c4 ) ;
}

int S4FUNCTION code4close( CODE4 S4PTR *c4 )
{
   DATA4 *d4;
   WaitForSingleObject( c4->hD4List, INFINITE ) ;
   ResetEvent( c4->hD4List ) ;
   // AS Jan 27/06 -  merged from other project...must not be called closeAll which conflicts for C++ with function name
   c4->closeAllList = 1 ;
   for ( d4 = (DATA4 *)l4first( &c4->d4list ); d4 != 0 ; d4 = (DATA4 *)l4first( &c4->d4list ) )
      d4close( d4 ) ;   /* use wrapper version of d4close() to free memory from d4***() */
   c4->closeAllList = 0 ;
   SetEvent( c4->hD4List ) ;

   return c4->code4close( c4->c4 ) ;
}

int S4FUNCTION code4connect( CODE4 S4PTR *c4, const char S4PTR *serverId, const char S4PTR *processId, const char S4PTR *userId, const char S4PTR *password, const char S4PTR *protocol )
{
   return c4->code4connect( c4->c4, serverId, processId, userId, password, protocol ) ;
}

short S4FUNCTION code4collate( CODE4 S4PTR *c4, short collateType )
{
   return c4->code4collate( c4->c4, collateType ) ;
}

DATA4 * S4FUNCTION code4data( CODE4 S4PTR *c4, const char S4PTR *name )
{
   DATA4 *d4 ;
   char alias[LEN4PATH] ;

   memset( alias, 0, sizeof(alias) ) ;
   c4->u4namePiece( alias, sizeof(alias), name, 0, 0 ) ;

   if ( c4->numD4 > 0 )
   {
      WaitForSingleObject( c4->hD4List, INFINITE ) ;
      ResetEvent( c4->hD4List ) ;
      for ( d4 = (DATA4*) l4first( &c4->d4list ); d4 != 0; d4 = (DATA4*) l4next( &c4->d4list, d4 ) )
      {
         // AS Jul 20/06 - incorrect...if the field name is a substring, we get the wrong field eg. ("data" and "data_len")
         if ( stricmp( d4->alias, alias ) == 0 )
         {
            SetEvent( c4->hD4List ) ;
            return d4 ;
         }
      }
      SetEvent( c4->hD4List ) ;
   }

   return 0 ;
}

const char S4PTR *S4FUNCTION code4dateFormat( CODE4 S4PTR *c4 )
{
   return (const char*) c4->code4dateFormat( c4->c4 ) ;
}

int S4FUNCTION code4dateFormatSet( CODE4 S4PTR *c4, const char S4PTR *fmt )
{
   return c4->code4dateFormatSet( c4->c4, fmt ) ;
}

void S4FUNCTION code4exit( CODE4 S4PTR *c4 )
{
   c4->code4exit( c4->c4 ) ;
}

int S4FUNCTION code4flush( CODE4 S4PTR *c4 )
{
   return c4->code4flush( c4->c4 ) ;
}

long  S4FUNCTION code4hInst( CODE4 *c4, long value )
{
   return c4->code4hInst( c4->c4, value ) ;
}

long  S4FUNCTION code4hWnd( CODE4 *c4, long value )
{
   return c4->code4hWnd( c4->c4, value ) ;
}

short S4FUNCTION code4indexBlockSize( CODE4 S4PTR *c4)
{
   return c4->code4indexBlockSize( c4->c4 ) ;
}

short S4FUNCTION code4indexBlockSizeSet( CODE4 S4PTR *c4, short val )
{
   return c4->code4indexBlockSizeSet( c4->c4, val ) ;
}

enum index4format S4FUNCTION code4indexFormat( CODE4 S4PTR *c4 )
{
   return (enum index4format) c4->code4indexFormat( c4->c4 ) ;
}

const char S4PTR * S4FUNCTION code4indexExtension( CODE4 S4PTR *c4 )
{
   return (const char*) c4->code4indexExtension( c4->c4 ) ;
}

void S4FUNCTION code4largeOn( CODE4 S4PTR *c4 )
{
   c4->code4largeOn( c4->c4 ) ;
}

int S4FUNCTION code4lock( CODE4 S4PTR *c4 )
{
   return c4->code4lock( c4->c4 ) ;
}

short S4FUNCTION code4lockAttempts( CODE4 *cb, short value )
{
   return cb->code4lockAttempts( cb->c4, value ) ;
}

short S4FUNCTION code4lockAttemptsSingle( CODE4 *cb, short value )
{
   return cb->code4lockAttemptsSingle( cb->c4, value ) ;
}

void S4FUNCTION code4lockClear( CODE4 S4PTR *c4 )
{
   c4->code4lockClear( c4->c4 ) ;
}

long  S4FUNCTION code4lockDelay( CODE4 *cb, long value )
{
   return cb->code4lockDelay( cb->c4, value ) ;
}

short S4FUNCTION code4lockEnforce( CODE4 *cb, short value )
{
   return cb->code4lockEnforce( cb->c4, value ) ;
}

const char S4PTR *S4FUNCTION code4lockFileName( CODE4 S4PTR *c4 )
{
   return (const char*) c4->code4lockFileName( c4->c4 ) ;
}

long S4FUNCTION code4lockItem( CODE4 S4PTR *c4 )
{
   return c4->code4lockItem( c4->c4 ) ;
}

const char S4PTR *S4FUNCTION code4lockNetworkId( CODE4 S4PTR *c4 )
{
   return (const char*) c4->code4lockNetworkId( c4->c4 ) ;
}

const char S4PTR *S4FUNCTION code4lockUserId( CODE4 S4PTR *c4 )
{
   return (const char*) c4->code4lockUserId( c4->c4 ) ;
}

short S4FUNCTION code4log( CODE4 *cb, short value )
{
   return cb->code4log( cb->c4, value ) ;
}

const char S4PTR *S4FUNCTION code4logFileName( CODE4 S4PTR *c4 )
{
   return (const char*) c4->code4logFileName( c4->c4 ) ;
}

void S4FUNCTION code4logOpenOff( CODE4 S4PTR *c4 )
{
   c4->code4logOpenOff( c4->c4 ) ;
}

short S4FUNCTION code4memExpandBlock( CODE4 *c4, short value )
{
   return c4->code4memExpandBlock( c4->c4, value ) ;
}

short S4FUNCTION code4memExpandData( CODE4 *c4, short value )
{
   return c4->code4memExpandData( c4->c4, value ) ;
}

short S4FUNCTION code4memExpandIndex( CODE4 *c4, short value )
{
   return c4->code4memExpandIndex( c4->c4, value ) ;
}

short S4FUNCTION code4memExpandLock( CODE4 *c4, short value )
{
   return c4->code4memExpandLock( c4->c4, value ) ;
}

short S4FUNCTION code4memExpandTag( CODE4 *c4, short value )
{
   return c4->code4memExpandTag( c4->c4, value ) ;
}

int S4FUNCTION code4memoCompress( CODE4 *c4, short flag )
{
   return c4->code4memoCompress( c4->c4, flag ) ;
}

long  S4FUNCTION code4memSizeBlock( CODE4 *c4, long value )
{
   return c4->code4memSizeBlock( c4->c4, value ) ;
}

long  S4FUNCTION code4memSizeBuffer( CODE4 *c4, long value )
{
   return c4->code4memSizeBuffer( c4->c4, value ) ;
}

short S4FUNCTION code4memSizeMemo( CODE4 *c4, short value )
{
   return c4->code4memSizeMemo( c4->c4, value ) ;
}

long  S4FUNCTION code4memSizeMemoExpr( CODE4 *c4, long value )
{
   return c4->code4memSizeMemoExpr( c4->c4, value ) ;
}

long  S4FUNCTION code4memSizeSortBuffer( CODE4 *c4, long value )
{
   return c4->code4memSizeSortBuffer( c4->c4, value ) ;
}

long  S4FUNCTION code4memSizeSortPool( CODE4 *c4, long value )
{
   return c4->code4memSizeSortPool( c4->c4, value ) ;
}

short S4FUNCTION code4memStartBlock( CODE4 *c4, short value )
{
   return c4->code4memStartBlock( c4->c4, value ) ;
}

short S4FUNCTION code4memStartData( CODE4 *c4, short value )
{
   return c4->code4memStartData( c4->c4, value ) ;
}

short S4FUNCTION code4memStartIndex( CODE4 *c4, short value )
{
   return c4->code4memStartIndex( c4->c4, value ) ;
}

short S4FUNCTION code4memStartLock( CODE4 *c4, short value )
{
   return c4->code4memStartLock( c4->c4, value ) ;
}

long  S4FUNCTION code4memStartMax( CODE4 *c4, long value )
{
   return c4->code4memStartMax( c4->c4, value ) ;
}

short S4FUNCTION code4memStartTag( CODE4 *c4, short value )
{
   return c4->code4memStartTag( c4->c4, value ) ;
}

int S4FUNCTION code4optAll( CODE4 S4PTR *c4 )
{
   return c4->code4optAll( c4->c4 ) ;
}

short S4FUNCTION code4optimize( CODE4 *cb, short value )
{
   return cb->code4optimize( cb->c4, value ) ;
}

short S4FUNCTION code4optimizeWrite( CODE4 *cb, short value )
{
   return cb->code4optimizeWrite( cb->c4, value ) ;
}

int S4FUNCTION code4optStart( CODE4 S4PTR *c4 )
{
   return c4->code4optStart( c4->c4 ) ;
}

int S4FUNCTION code4optSuspend( CODE4 S4PTR *c4 )
{
   return c4->code4optSuspend( c4->c4 ) ;
}

int S4FUNCTION code4ping( CODE4 S4PTR *c4 )
{
   return c4->code4ping( c4->c4 ) ;
}

short S4FUNCTION code4readLock( CODE4 *cb, short value )
{
   return cb->code4readLock( cb->c4, value ) ;
}

short S4FUNCTION code4readOnly( CODE4 *cb, short value )
{
   return cb->code4readOnly( cb->c4, value ) ;
}

short S4FUNCTION code4safety( CODE4 *cb, short value )
{
   return cb->code4safety( cb->c4, value ) ;
}

short S4FUNCTION code4singleOpen( CODE4 *cb, short value )
{
   return cb->code4singleOpen( cb->c4, value ) ;
}

long S4FUNCTION code4timeout( CODE4 S4PTR *c4 )
{
   return c4->code4timeout( c4->c4 ) ;
}

void S4FUNCTION code4timeoutSet( CODE4 S4PTR *c4, long setting )
{
   c4->code4timeoutSet( c4->c4, setting ) ;
}

int S4FUNCTION code4tranCommit( CODE4 S4PTR *c4 )
{
   return c4->code4tranCommit( c4->c4 ) ;
}

int S4FUNCTION code4tranRollback( CODE4 S4PTR *c4 )
{
   return c4->code4tranRollback( c4->c4 ) ;
}

int S4FUNCTION code4tranStart( CODE4 S4PTR *c4 )
{
   return c4->code4tranStart( c4->c4 ) ;
}

short S4FUNCTION code4tranStatusCB( CODE4 S4PTR *c4 )
{
   return c4->code4tranStatus( c4->c4 ) ;
}

int S4FUNCTION code4unlock( CODE4 S4PTR *c4 )
{
   return c4->code4unlock( c4->c4 ) ;
}

short S4FUNCTION code4unlockAutoCB( CODE4 S4PTR *c4 )
{
   return c4->code4unlockAutoCB( c4->c4 ) ;
}

void S4FUNCTION code4unlockAutoSetCB( CODE4 *c4, short autoUnlock )
{
   c4->code4unlockAutoSetCB( c4->c4, autoUnlock ) ;
}

S4EXPORT short S4FUNCTION code4useGeneralTagsInRelate( CODE4 *c4, short value )
{
   return c4->code4useGeneralTagsInRelate( c4->c4, value ) ;
}

void S4FUNCTION code4verifySet( CODE4 S4PTR *c4, const char S4PTR *val )
{
   c4->code4verifySet( c4->c4, val ) ;
}

int S4FUNCTION code4validate( CODE4 *c4, const char *name, Bool5 deleteTemp )
{
   return c4->code4validate( c4->c4, name, deleteTemp ) ;
}

static int d4setupFunctions( CODE4 *c4, DATA4 *d4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "d4alias", (void **)&d4->d4alias, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4aliasSet", (void **)&d4->d4aliasSet, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4append", (void **)&d4->d4append, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4appendBlank", (void **)&d4->d4appendBlank, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4appendStart", (void **)&d4->d4appendStart, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4blank", (void **)&d4->d4blank, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4bof", (void **)&d4->d4bof, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4bottom", (void **)&d4->d4bottom, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4changed", (void **)&d4->d4changed, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4check", (void **)&d4->d4check, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4close", (void **)&d4->d4close, &rc ) ;
   //c4getFuncPtr( c4->hInst, "d4copyTable", (void **)&d4->d4copyTable, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4delete", (void **)&d4->d4delete, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4deleted", (void **)&d4->d4deleted, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4eof", (void **)&d4->d4eof, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4field", (void **)&d4->d4field, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4fieldInfo", (void **)&d4->d4fieldInfo, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4fieldJ", (void **)&d4->d4fieldJ, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4fieldNumber", (void **)&d4->d4fieldNumber, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4fieldsAdd", (void **)&d4->d4fieldsAdd, &rc ) ;
   //c4getFuncPtr( c4->hInst, "d4modifyStructure", (void **)&d4->d4modifyStructure, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4fieldsRemove", (void **)&d4->d4fieldsRemove, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4fileName", (void **)&d4->d4fileName, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4flush", (void **)&d4->d4flush, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4freeBlocks", (void **)&d4->d4freeBlocks, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4goLow", (void **)&d4->d4goLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4goBof", (void **)&d4->d4goBof, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4goEof", (void **)&d4->d4goEof, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4index", (void **)&d4->d4index, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4indexList", (void **)&d4->d4indexList, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lock", (void **)&d4->d4lock, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAdd", (void **)&d4->d4lockAdd, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAddAll", (void **)&d4->d4lockAddAll, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAddAppend", (void **)&d4->d4lockAddAppend, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAddFile", (void **)&d4->d4lockAddFile, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAll", (void **)&d4->d4lockAll, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAllVB", (void **)&d4->d4lockAllVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAppend", (void **)&d4->d4lockAppend, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockAppendVB", (void **)&d4->d4lockAppendVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockFile", (void **)&d4->d4lockFile, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockFileVB", (void **)&d4->d4lockFileVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockTest", (void **)&d4->d4lockTest, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4lockVB", (void **)&d4->d4lockVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4log", (void **)&d4->d4log, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4logStatusCB", (void **)&d4->d4logStatusCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4logVB", (void **)&d4->d4logVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4memoCompress", (void **)&d4->d4memoCompress, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4numFields", (void **)&d4->d4numFields, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4openClone", (void **)&d4->d4openClone, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4optimize", (void **)&d4->d4optimize, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4optimizeVB", (void **)&d4->d4optimizeVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4optimizeWrite", (void **)&d4->d4optimizeWrite, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4optimizeWriteVB", (void **)&d4->d4optimizeWriteVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4pack", (void **)&d4->d4pack, &rc ) ;
   // AS Feb 22/06 - support for new functionality...
   c4getFuncPtr( c4->hInst, "d4packWithProgress", (void **)&d4->d4packWithProgress, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4position", (void **)&d4->d4position, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4positionSet", (void **)&d4->d4positionSet, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4readBuffer", (void **)&d4->d4readBuffer, &rc ) ;
   // AS Feb 22/06 - support for new functionality...
   c4getFuncPtr( c4->hInst, "d4readBufferConfigure", (void **)&d4->d4readBufferConfigure, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4recall", (void **)&d4->d4recall, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4recCountDo2", (void **)&d4->d4recCountDo2, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4recNoLow", (void **)&d4->d4recNoLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4recordLow", (void **)&d4->d4recordLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4recWidthLow", (void **)&d4->d4recWidthLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4recWidth_v", (void **)&d4->d4recWidth_v, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4refresh", (void **)&d4->d4refresh, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4refreshRecord", (void **)&d4->d4refreshRecord, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4reindex", (void **)&d4->d4reindex, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4reindexWithProgress", (void **)&d4->d4reindexWithProgress, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4remove", (void **)&d4->d4remove, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seek", (void **)&d4->d4seek, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seekDouble", (void **)&d4->d4seekDouble, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seekN", (void **)&d4->d4seekN, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seekNext", (void **)&d4->d4seekNext, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seekNextDouble", (void **)&d4->d4seekNextDouble, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seekNextN", (void **)&d4->d4seekNextN, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seekLongLong", (void **)&d4->d4seekLongLong, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4seekNextLongLong", (void **)&d4->d4seekNextLongLong, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4skip", (void **)&d4->d4skip, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4skipCache", (void **)&d4->d4skipCache, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4tag", (void **)&d4->d4tag, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4tagDefault", (void **)&d4->d4tagDefault, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4tagNext", (void **)&d4->d4tagNext, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4tagPrev", (void **)&d4->d4tagPrev, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4tagSelect", (void **)&d4->d4tagSelect, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4tagSelected", (void **)&d4->d4tagSelected, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4tagSync", (void **)&d4->d4tagSync, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4top", (void **)&d4->d4top, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4unlock", (void **)&d4->d4unlock, &rc ) ;
   // AS Feb 22/06 - support for new functionality...
   c4getFuncPtr( c4->hInst, "d4unlockAppend", (void **)&d4->d4unlockAppend, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4versionCB", (void **)&d4->d4versionCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4writeLow", (void **)&d4->d4writeLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4writeBuffer", (void **)&d4->d4writeBuffer, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4writeVB", (void **)&d4->d4writeVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "d4zap", (void **)&d4->d4zap, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoWriteFinish", (void **)&d4->f4memoWriteFinish, &rc ) ;

   return rc ;
}

S4CONST char S4PTR * S4FUNCTION d4alias( S4CONST DATA4 S4PTR *d4 )
{
   return (const char*) d4->d4alias( d4->d4 ) ;
}

void S4FUNCTION d4aliasSet( DATA4 S4PTR *d4, const char S4PTR *name )
{
   d4->d4aliasSet( d4->d4, name ) ;
   memset( d4->alias, 0, sizeof(d4->alias) ) ;
   if ( strlen(name) > LEN4PATH )
      strncpy( d4->alias, name, LEN4PATH ) ;
   else
      strcpy( d4->alias, name ) ;
}

int S4FUNCTION d4append( DATA4 S4PTR *d4 )
{
   return d4->d4append( d4->d4 ) ;
}

int S4FUNCTION d4appendBlank( DATA4 S4PTR *d4 )
{
   return d4->d4appendBlank( d4->d4 ) ;
}

short S4FUNCTION d4appendStart( DATA4 S4PTR *d4, short useMemo )
{
   return d4->d4appendStart( d4->d4, useMemo ) ;
}

void S4FUNCTION d4blank( DATA4 S4PTR *d4 )
{
   d4->d4blank( d4->d4 ) ;
}

int S4FUNCTION d4bof( DATA4 S4PTR *d4 )
{
   return d4->d4bof( d4->d4 ) ;
}

int S4FUNCTION d4bottom( DATA4 S4PTR *d4 )
{
   return d4->d4bottom( d4->d4 ) ;
}

short S4FUNCTION d4changed( DATA4 S4PTR *d4, short flag )
{
   return d4->d4changed( d4->d4, flag ) ;
}

int S4FUNCTION d4check( DATA4 S4PTR *d4 )
{
   return d4->d4check( d4->d4 ) ;
}

int S4FUNCTION d4close( DATA4 *d4 )
{
   FIELD4 *f4;
   INDEX4 *i4;
   TAG4 *t4;
   int rc;

   if ( d4->numF4 )
      for ( f4 = (FIELD4*)l4first( &d4->f4list ); f4 != 0 ; f4 = (FIELD4*)l4first( &d4->f4list ) )
      {
         l4remove( &d4->f4list, f4 ) ;
         free( f4 ) ;
      }

   if ( d4->numI4 )
      for ( i4 = (INDEX4*)l4first( &d4->i4list ); i4 != 0 ; i4 = (INDEX4*)l4first( &d4->i4list ) )
      {
         l4remove( &d4->i4list, i4 ) ;
         free( i4 ) ;
      }

   if ( d4->numT4 )
      for ( t4 = (TAG4*)l4first( &d4->t4list ); t4 != 0 ; t4 = (TAG4*)l4first( &d4->t4list ) )
      {
         l4remove( &d4->t4list, t4 ) ;
         free( t4 ) ;
      }

   rc = d4->d4close( d4->d4 ) ;

   // AS Jan 27/06 -  merged from other project...must not be called closeAll which conflicts for C++ with function name
   if ( d4->c4->closeAllList == 0 )
   {
      WaitForSingleObject( d4->c4->hD4List, INFINITE ) ;
      ResetEvent( d4->c4->hD4List ) ;
   }
   d4->c4->l4remove( &d4->c4->d4list, d4 ) ;
   d4->c4->numD4-- ;
   if ( d4->c4->closeAllList == 0 )
      SetEvent( d4->c4->hD4List ) ;
   free( d4 ) ;
   return rc ;
}

DATA4 *S4FUNCTION d4compress( DATA4 *data, const char *compressedName, short blockSize )
{
   error4describe( data->c4, (int)e4notSupported, 94414L, "d4compress not yet supported in dual dll support", 0, 0 ) ;
   return 0 ;
}


int S4FUNCTION d4copyTable( DATA4 S4PTR *data, const char *destFolder, short includeIndex )
{
   error4describe( data->c4, (int)e4notSupported, 94414L, "d4copyTable not yet supported in dual dll support", 0, 0 ) ;
   return 0 ;
}


DATA4 *S4FUNCTION d4create( CODE4 *c4, const char *name, const FIELD4INFO S4PTR *fieldData, const TAG4INFO *tagInfo )
{
   long rc = 0 ;
   DATA4 *d4 = (DATA4 *)malloc( sizeof( DATA4 ) ) ;
   if ( d4 == 0 )
      return 0 ;
   memset( d4, 0, sizeof( DATA4 ) ) ;

   c4getFuncPtr( c4->hInst, "d4create", (void **)&d4->d4create, &rc ) ;
   if ( rc != 0 )
   {
      free( d4 ) ;
      return 0 ;
   }

   d4->d4 = d4->d4create( c4->c4, name, fieldData, tagInfo ) ;
   if ( d4->d4 == 0 )
   {
      free( d4 ) ;
      return 0 ;
   }

   rc = d4setupFunctions( c4, d4 ) ;
   if ( rc < 0 )
   {
      if ( d4->d4close != 0 )
         d4->d4close( d4->d4 ) ;
      free( d4 ) ;
      return 0 ;
   }

   WaitForSingleObject( c4->hD4List, INFINITE ) ;
   ResetEvent( c4->hD4List ) ;
   c4->l4add( &c4->d4list, d4 ) ;
   c4->numD4++ ;
   SetEvent( c4->hD4List ) ;
   d4->c4 = c4 ;
   memset( d4->alias, 0, sizeof(d4->alias) ) ;
   strcpy( d4->alias, (const char*)d4->d4alias( d4->d4 ) ) ;

   return d4 ;
}

void S4FUNCTION d4delete( DATA4 S4PTR *d4 )
{
   d4->d4delete( d4->d4 ) ;
}

int S4FUNCTION d4deleted( DATA4 S4PTR *d4 )
{
   return d4->d4deleted( d4->d4 ) ;
}

int S4FUNCTION d4eof( DATA4 S4PTR *d4 )
{
   return d4->d4eof( d4->d4 ) ;
}

FIELD4 S4PTR * S4FUNCTION d4field( DATA4 S4PTR *d4, const char S4PTR *name )
{
   int rc ;
   FIELD4 *f4 ;

   if ( d4->numF4 > 0 )
      for ( f4 = (FIELD4*) l4first( &d4->f4list ) ; f4 != 0 ; f4 = (FIELD4*) l4next( &d4->f4list, f4 ) )
      {
         // AS Jul 20/06 - incorrect...if the field name is a substring, we get the wrong field eg. ("data" and "data_len")
         if ( stricmp( f4->name, name ) == 0 )
            return f4 ;
      }

   f4 = (FIELD4*) malloc( sizeof(FIELD4) ) ;
   if ( f4 == 0 )
      return 0 ;
   memset( f4, 0, sizeof(FIELD4) ) ;

   f4->f4 = d4->d4field( d4->d4, name ) ;
   if ( f4->f4 == 0 )
   {
      free( f4 ) ;
      return 0 ;
   }

   rc = f4setupFunctions( d4->c4, f4 ) ;
   if ( rc != 0 )
   {
      free( f4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->f4list, f4 ) ;
   f4->d4 = d4 ;
   d4->numF4++ ;
   memset( f4->name, 0, 11 ) ;
   strcpy( f4->name, name ) ;
   f4->ordinal = f4->f4number( f4->f4 ) ;

   return f4 ;
}

FIELD4INFO S4PTR * S4FUNCTION d4fieldInfo( DATA4 S4PTR *d4 )
{
   return (FIELD4INFO*) d4->d4fieldInfo( d4->d4 ) ;
}

FIELD4 S4PTR * S4FUNCTION d4fieldJ( DATA4 S4PTR *d4, short jField )
{
   int rc ;
   FIELD4 *f4 ;

   if ( d4->numF4 > 0 )
      for ( f4 = (FIELD4*) l4first( &d4->f4list ) ; f4 != 0 ; f4 = (FIELD4*) l4next( &d4->f4list, f4 ) )
         if ( f4->ordinal == jField )
            return f4 ;

   f4 = (FIELD4*) malloc( sizeof(FIELD4) ) ;
   if ( f4 == 0 )
      return 0 ;
   memset( f4, 0, sizeof(FIELD4) ) ;

   f4->f4 = d4->d4fieldJ( d4->d4, jField ) ;
   if ( f4->f4 == 0 )
   {
      free( f4 ) ;
      return 0 ;
   }

   rc = f4setupFunctions( d4->c4, f4 ) ;
   if ( rc != 0 )
   {
      free( f4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->f4list, f4 ) ;
   f4->d4 = d4 ;
   d4->numF4++ ;
   memset( f4->name, 0, 11 ) ;
   strcpy( f4->name, (const char*)f4->f4name( f4->f4 ) ) ;
   f4->ordinal = jField ;

   return f4 ;
}

int S4FUNCTION d4fieldNumber( DATA4 S4PTR *d4, const char S4PTR *name )
{
   return d4->d4fieldNumber( d4->d4, name ) ;
}

DATA4 S4PTR * S4FUNCTION d4fieldsAdd( DATA4 S4PTR *oldData, short nFields, FIELD4INFO S4PTR *fieldsToAdd )
{
   DATA4 *newData ;
   long rc = 0 ;

   newData = (DATA4 *)malloc( sizeof( DATA4 ) ) ;
   if ( newData == 0 )
      return 0 ;
   memset( newData, 0, sizeof( DATA4 ) ) ;

   newData->d4 = oldData->d4fieldsAdd( oldData->d4, nFields, fieldsToAdd ) ;
   if ( newData->d4 == 0 )
   {
      free( newData ) ;
      return 0 ;
   }

   rc = d4setupFunctions( oldData->c4, newData ) ;
   if ( rc < 0 )
   {
      free( newData ) ;
      return 0 ;
   }

   newData->c4 = oldData->c4 ;
   WaitForSingleObject( newData->c4->hD4List, INFINITE ) ;
   ResetEvent( newData->c4->hD4List ) ;
   g_c4->l4add( &newData->c4->d4list, newData ) ;
   l4remove( &newData->c4->d4list, oldData ) ;
   SetEvent( newData->c4->hD4List ) ;
   free( oldData ) ;
   memset( newData->alias, 0, sizeof(newData->alias) ) ;
   strcpy( newData->alias, (const char*)newData->d4alias( newData->d4 ) ) ;

   return newData ;
}

DATA4 S4PTR * S4FUNCTION d4modifyStructure( DATA4 S4PTR *oldData, FIELD4INFO S4PTR *fieldsToAdd, TAG4INFO S4PTR *tagsToAdd )
{
   error4describe( oldData->c4, (int)e4notSupported, 94414L, "d4modifyStructure not yet supported in dual dll support", 0, 0 ) ;
   return 0 ;
}

DATA4 S4PTR * S4FUNCTION d4fieldsRemove( DATA4 S4PTR *oldData, short nFields, char S4PTR * S4PTR *name )
{
   DATA4 *newData ;
   long rc = 0 ;

   newData = (DATA4 *)malloc( sizeof( DATA4 ) ) ;
   if ( newData == 0 )
      return 0 ;
   memset( newData, 0, sizeof( DATA4 ) ) ;

   newData->d4 = oldData->d4fieldsRemove( &oldData->d4, nFields, name ) ;
   if ( newData->d4 == 0 )
   {
      free( newData ) ;
      return 0 ;
   }

   rc = d4setupFunctions( oldData->c4, newData ) ;
   if ( rc < 0 )
   {
      free( newData ) ;
      return 0 ;
   }

   newData->c4 = oldData->c4 ;
   WaitForSingleObject( newData->c4->hD4List, INFINITE ) ;
   ResetEvent( newData->c4->hD4List ) ;
   g_c4->l4add( &newData->c4->d4list, newData ) ;
   l4remove( &newData->c4->d4list, oldData ) ;
   SetEvent( newData->c4->hD4List ) ;
   free( oldData ) ;
   memset( newData->alias, 0, sizeof(newData->alias) ) ;
   strcpy( newData->alias, (const char*)newData->d4alias( newData->d4 ) ) ;

   return newData ;
}

const char S4PTR *S4FUNCTION d4fileName( DATA4 S4PTR *d4 )
{
   return (const char*) d4->d4fileName( d4->d4 ) ;
}

int S4FUNCTION d4flush( DATA4 S4PTR *d4 )
{
   return d4->d4flush( d4->d4 ) ;
}

int S4FUNCTION d4freeBlocks( DATA4 S4PTR *d4 )
{
   return d4->d4freeBlocks( d4->d4 ) ;
}

int S4FUNCTION d4goLow( DATA4 S4PTR *d4, const long recNo, short goForWrite )
{
   return d4->d4goLow( d4->d4, recNo, goForWrite ) ;
}

int S4FUNCTION d4goBof( DATA4 S4PTR *d4 )
{
   return d4->d4goBof( d4->d4 ) ;
}

int S4FUNCTION d4goEof( DATA4 S4PTR *d4 )
{
   return d4->d4goEof( d4->d4 ) ;
}

INDEX4 S4PTR * S4FUNCTION d4index( DATA4 S4PTR *d4, const char S4PTR *name )
{
   INDEX4 *i4 ;
   int rc ;
   char tempName[LEN4PATH] ;

   memset( tempName, 0, sizeof(tempName) ) ;
   if ( name == 0 )
      strcpy( tempName, (const char*) d4->d4alias( d4->d4 ) ) ;
   else
      d4->c4->u4namePiece( tempName, sizeof(tempName), name, 0, 0 ) ;

   if ( d4->numI4 > 0 )
      for ( i4 = (INDEX4*) l4first( &d4->i4list ) ; i4 != 0 ; i4 = (INDEX4*) l4next( &d4->i4list, i4 ) )
      {
         // AS Jul 20/06 - incorrect...if the field name is a substring, we get the wrong field eg. ("data" and "data_len")
         if ( stricmp( (const char*)i4->name, (const char*)tempName ) == 0 )
            return i4 ;
      }

   i4 = (INDEX4*) malloc( sizeof(INDEX4) ) ;
   if ( i4 == 0 )
      return 0 ;
   memset( i4, 0, sizeof(INDEX4) ) ;

   i4->i4 = d4->d4index( d4->d4, name ) ;
   if ( i4->i4 == 0 )
   {
      free( i4 ) ;
      return 0 ;
   }

   rc = i4setupFunctions( d4->c4, i4 ) ;
   if ( rc != 0 )
   {
      free( i4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->i4list, i4 ) ;
   d4->numI4++ ;
   i4->d4 = d4 ;
   memset( i4->name, 0, sizeof(i4->name) ) ;
   strcpy( i4->name, (const char*) tempName ) ;

   return i4 ;
}

LIST4 S4PTR * S4FUNCTION d4indexList( DATA4 S4PTR *d4 )
{
   return (LIST4 *)d4->d4indexList( d4->d4 ) ;
}

int S4FUNCTION d4lock( DATA4 S4PTR *d4, const long recNum )
{
   return d4->d4lock( d4->d4, recNum ) ;
}

int S4FUNCTION d4lockAdd( DATA4 S4PTR *d4, long recNum )
{
   return d4->d4lockAdd( d4->d4, recNum ) ;
}

int S4FUNCTION d4lockAddAll( DATA4 S4PTR *d4 )
{
   return d4->d4lockAddAll( d4->d4 ) ;
}

int S4FUNCTION d4lockAddAppend( DATA4 S4PTR *d4 )
{
   return d4->d4lockAddAppend( d4->d4 ) ;
}

int S4FUNCTION d4lockAddFile( DATA4 S4PTR *d4 )
{
   return d4->d4lockAddFile( d4->d4 ) ;
}

int S4FUNCTION d4lockAll( DATA4 S4PTR *d4 )
{
   return d4->d4lockAll( d4->d4 ) ;
}

int S4FUNCTION d4lockAllVB( DATA4 S4PTR *d4 )
{
   return d4->d4lockAllVB( d4->d4 ) ;
}

int S4FUNCTION d4lockAppend( DATA4 S4PTR *d4 )
{
   return d4->d4lockAppend( d4->d4 ) ;
}

int S4FUNCTION d4lockAppendVB( DATA4 S4PTR *d4 )
{
   return d4->d4lockAppendVB( d4->d4 ) ;
}

int S4FUNCTION d4lockFile( DATA4 S4PTR *d4 )
{
   return d4->d4lockFile( d4->d4 ) ;
}

int S4FUNCTION d4lockFileVB( DATA4 S4PTR *d4 )
{
   return d4->d4lockFileVB( d4->d4 ) ;
}

int S4FUNCTION d4lockVB( DATA4 *d4, const long recNum )
{
   return d4->d4lockVB( d4->d4, recNum ) ;
}

int S4FUNCTION d4lockTest( DATA4 S4PTR *d4, const long recNo, enum Lock4type lockType )
{
   return d4->d4lockTest( d4->d4, recNo, lockType ) ;
}

int S4FUNCTION d4log( DATA4 S4PTR *d4, const int logging )
{
   return d4->d4log( d4->d4, logging ) ;
}

short S4FUNCTION d4logStatusCB( DATA4 *d4 )
{
   return d4->d4logStatusCB( d4->d4 ) ;
}

short S4FUNCTION d4logVB( DATA4 *d4, short logFlag )
{
   return d4->d4logVB( d4->d4, logFlag ) ;
}

int S4FUNCTION d4memoCompress( DATA4 S4PTR *d4 )
{
   return d4->d4memoCompress( d4->d4 ) ;
}

short int S4FUNCTION d4numFields( DATA4 S4PTR *d4 )
{
   return d4->d4numFields( d4->d4 ) ;
}

DATA4 S4PTR *S4FUNCTION d4open( CODE4 *c4, const char *name )
{
   DATA4 *d4 ;
   long rc = 0 ;

   d4 = (DATA4 *)malloc( sizeof( DATA4 ) ) ;
   if ( d4 == 0 )
      return 0 ;
   memset( d4, 0, sizeof( DATA4 ) ) ;

   c4getFuncPtr( c4->hInst, "d4open", (void **)&d4->d4open, &rc ) ;
   if ( rc != 0 )
   {
      free( d4 ) ;
      return 0 ;
   }

   d4->d4 = d4->d4open( c4->c4, name ) ;
   if ( d4->d4 == 0 )
   {
      free( d4 ) ;
      return 0 ;
   }

   rc = d4setupFunctions( c4, d4 ) ;
   if ( rc < 0 )
   {
      if ( d4->d4close != 0 )
         d4->d4close( d4->d4 ) ;
      free( d4 ) ;
      return 0 ;
   }

   WaitForSingleObject( c4->hD4List, INFINITE ) ;
   ResetEvent( c4->hD4List ) ;
   c4->l4add( &c4->d4list, d4 ) ;
   c4->numD4++ ;
   SetEvent( c4->hD4List ) ;
   d4->c4 = c4 ;
   memset( d4->alias, 0, sizeof(d4->alias) ) ;
   strcpy( d4->alias, (const char*)d4->d4alias( d4->d4 ) ) ;

   return d4 ;
}

DATA4 S4PTR *S4FUNCTION d4openClone( DATA4 S4PTR *data )
{
   DATA4 *d4 ;
   long rc = 0 ;

   d4 = (DATA4 *)malloc( sizeof( DATA4 ) ) ;
   if ( d4 == 0 )
      return 0 ;
   memset( d4, 0, sizeof( DATA4 ) ) ;

   d4->d4 = data->d4openClone( data->d4 ) ;
   if ( d4->d4 == 0 )
   {
      free( d4 ) ;
      return 0 ;
   }

   rc = d4setupFunctions( data->c4, d4 ) ;
   if ( rc < 0 )
   {
      free( d4 ) ;
      return 0 ;
   }

   WaitForSingleObject( data->c4->hD4List, INFINITE ) ;
   ResetEvent( data->c4->hD4List ) ;
   g_c4->l4add( &data->c4->d4list, d4 ) ;
   g_c4->numD4++ ;
   SetEvent( data->c4->hD4List ) ;
   d4->c4 = data->c4 ;
   memset( d4->alias, 0, sizeof(d4->alias) ) ;
   strcpy( d4->alias, (const char*)d4->d4alias( d4->d4 ) ) ;

   return d4 ;
}

int S4FUNCTION d4optimize( DATA4 S4PTR *d4, const int optFlag )
{
   return d4->d4optimize( d4->d4, optFlag ) ;
}

short S4FUNCTION d4optimizeVB( DATA4 *d4, short flag )
{
   return d4->d4optimizeVB( d4->d4, flag ) ;
}

int S4FUNCTION d4optimizeWrite( DATA4 S4PTR *d4, const int optFlag )
{
   return d4->d4optimizeWrite( d4->d4, optFlag ) ;
}

short S4FUNCTION d4optimizeWriteVB( DATA4 *d4, short flag )
{
   return d4->d4optimizeWriteVB( d4->d4, flag ) ;
}

int S4FUNCTION d4pack( DATA4 S4PTR *d4 )
{
   return d4->d4pack( d4->d4 ) ;
}

// AS Feb 22/06 - support for new functionality...
short S4FUNCTION d4packWithProgress( DATA4 S4PTR *data, REINDEX_CALLBACK callback, long milliseconds )
{
   return data->d4packWithProgress( data->d4, callback, milliseconds ) ;
}

double S4FUNCTION d4position( DATA4 S4PTR *d4 )
{
   return d4->d4position( d4->d4 ) ;
}

long S4FUNCTION d4positionSet( DATA4 S4PTR *d4, const double pos)
{
   return d4->d4positionSet( d4->d4, pos ) ;
}

long S4FUNCTION d4readBuffer( DATA4 S4PTR *d4, long numRecs, short doMemos )
{
   return d4->d4readBuffer( d4->d4, numRecs, doMemos ) ;
}

// AS Feb 22/06 - support for new functionality...
int S4FUNCTION d4readBufferConfigure( DATA4 S4PTR *d4, long numRecs )
{
   return d4->d4readBufferConfigure( d4->d4, numRecs ) ;
}

void S4FUNCTION d4recall( DATA4 S4PTR *d4 )
{
   d4->d4recall( d4->d4 ) ;
}

long S4FUNCTION d4recNoLow( DATA4 S4PTR *d4 )
{
   return d4->d4recNoLow( d4->d4 ) ;
}

int S4FUNCTION d4remove( DATA4 *d4 )
{
   int rc = d4->d4remove( d4->d4 ) ;
   if ( rc == 0 )
   {
      WaitForSingleObject( d4->c4->hD4List, INFINITE ) ;
      ResetEvent( d4->c4->hD4List ) ;
      l4remove( &d4->c4->d4list, d4 ) ;
      g_c4->numD4-- ;
      SetEvent( d4->c4->hD4List ) ;
      free( d4 ) ;
   }
   return rc ;
}

long S4FUNCTION d4recCountDo( DATA4 *data )
{
   // AS d4recCountDo is exporeted, cannot modify, but call new d4recCountDo2 instead
   return d4recCountDo2( data, 0 ) ;
}

long S4FUNCTION d4recCountDo2( DATA4 *d4, Bool5 assumeLocked )
{
   return d4->d4recCountDo2( d4->d4, assumeLocked ) ;
}

char S4PTR *S4FUNCTION d4recordLow( DATA4 S4PTR *d4 )
{
   return (char*) d4->d4recordLow( d4->d4 ) ;
}

unsigned long S4FUNCTION d4recWidthLow( DATA4 S4PTR *d4 )
{
   return d4->d4recWidthLow( d4->d4 ) ;
}

long S4FUNCTION d4recWidth_v( DATA4 *d4 )
{
   return d4->d4recWidth_v( d4->d4 ) ;
}

int S4FUNCTION d4refresh( DATA4 S4PTR *d4 )
{
   return d4->d4refresh( d4->d4 ) ;
}

int S4FUNCTION d4refreshRecord( DATA4 S4PTR *d4 )
{
   return d4->d4refreshRecord( d4->d4 ) ;
}

int S4FUNCTION d4reindex( DATA4 S4PTR *d4 )
{
   return d4->d4reindex( d4->d4 ) ;
}

short S4FUNCTION d4reindexWithProgress( DATA4 S4PTR *data, REINDEX_CALLBACK callback, long milliseconds )
{
   return data->d4reindexWithProgress( data->d4, callback, milliseconds ) ;
}

int S4FUNCTION d4seek( DATA4 S4PTR *d4, const char S4PTR *ptr )
{
   return d4->d4seek( d4->d4, ptr ) ;
}

int S4FUNCTION d4seekDouble( DATA4 S4PTR *d4, const double d )
{
   return d4->d4seekDouble( d4->d4, d ) ;
}

short S4FUNCTION d4seekN( DATA4 S4PTR *d4, const char S4PTR *ptr, const short len )
{
   return d4->d4seekN( d4->d4, ptr, len ) ;
}

int S4FUNCTION d4seekNextDouble( DATA4 S4PTR *d4, const double d)
{
   return d4->d4seekNextDouble( d4->d4, d ) ;
}

int S4FUNCTION d4seekNext( DATA4 S4PTR *d4, const char S4PTR *ptr )
{
   return d4->d4seekNext( d4->d4, ptr ) ;
}

short S4FUNCTION d4seekNextN( DATA4 S4PTR *d4, const char S4PTR *ptr, const short len )
{
   return d4->d4seekNextN( d4->d4, ptr, len ) ;
}

int S4FUNCTION d4seekLongLong( DATA4 S4PTR *d4, LONGLONG dKey )
{
   return d4->d4seekLongLong( d4->d4, dKey ) ;
}

int S4FUNCTION d4seekNextLongLong( DATA4 S4PTR *d4, LONGLONG dKey )
{
   return d4->d4seekNextLongLong( d4->d4, dKey ) ;
}

int S4FUNCTION d4skip( DATA4 S4PTR *d4, const long numRecords )
{
   return d4->d4skip( d4->d4, numRecords ) ;
}

int S4FUNCTION d4skipCache( DATA4 S4PTR *d4, const long nCache )
{
   return d4->d4skipCache( d4->d4, nCache ) ;
}

TAG4 S4PTR * S4FUNCTION d4tag( DATA4 S4PTR *d4, const char S4PTR * const name )
{
   TAG4 *t4 ;
   int rc ;

   if ( d4->numT4 > 0 )
      for ( t4 = (TAG4*) l4first( &d4->t4list ) ; t4 != 0 ; t4 = (TAG4*) l4next( &d4->t4list, t4 ) )
      {
         // AS Jul 20/06 - incorrect...if the field name is a substring, we get the wrong field eg. ("data" and "data_len")
         if ( stricmp( t4->alias, name ) == 0 )
            return t4 ;
      }

   t4 = (TAG4*) malloc( sizeof(TAG4) ) ;
   if ( t4 == 0 )
      return 0 ;
   memset( t4, 0, sizeof(TAG4) ) ;

   t4->t4 = d4->d4tag( d4->d4, name ) ;
   if ( t4->t4 == 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   rc = t4setupFunctions( d4->c4, t4 ) ;
   if ( rc != 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->t4list, t4 ) ;
   d4->numT4++ ;
   t4->d4 = d4 ;
   memset( t4->alias, 0, sizeof(t4->alias) ) ;
   strcpy( t4->alias, (const char*)t4->t4alias( t4->t4 ) ) ;

   return t4 ;
}

TAG4 S4PTR * S4FUNCTION d4tagDefault( DATA4 S4PTR *d4 )
{
   TAG4 *t4 ;
   int rc ;
   void *tempPtr ;

   tempPtr = d4->d4tagDefault( d4->d4 ) ;
   if ( tempPtr == 0 )
      return 0 ;

   if ( d4->numT4 > 0 )
      for ( t4 = (TAG4*) l4first( &d4->t4list ) ; t4 != 0 ; t4 = (TAG4*) l4next( &d4->t4list, t4 ) )
         if ( t4->t4 == tempPtr )
            return t4 ;

   t4 = (TAG4*) malloc( sizeof(TAG4) ) ;
   if ( t4 == 0 )
      return 0 ;
   memset( t4, 0, sizeof(TAG4) ) ;

   rc = t4setupFunctions( d4->c4, t4 ) ;
   if ( rc != 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->t4list, t4 ) ;
   d4->numT4++ ;
   t4->t4 = tempPtr ;
   t4->d4 = d4 ;
   memset( t4->alias, 0, sizeof(t4->alias) ) ;
   strcpy( t4->alias, (const char*)t4->t4alias( t4->t4 ) ) ;

   return t4 ;
}

TAG4 S4PTR * S4FUNCTION d4tagNext( DATA4 S4PTR *d4, TAG4 S4PTR *tagOn )
{
   TAG4 *t4 ;
   int rc ;
   void *tempPtr ;

   if ( tagOn == 0 )
      tempPtr = d4->d4tagNext( d4->d4, 0 ) ;
   else
      tempPtr = d4->d4tagNext( d4->d4, tagOn->t4 ) ;
   if ( tempPtr == 0 )
      return 0 ;

   if ( d4->numT4 > 0 )
      for ( t4 = (TAG4*) l4first( &d4->t4list ) ; t4 != 0 ; t4 = (TAG4*) l4next( &d4->t4list, t4 ) )
         if ( t4->t4 == tempPtr )
            return t4 ;

   t4 = (TAG4*) malloc( sizeof(TAG4) ) ;
   if ( t4 == 0 )
      return 0 ;
   memset( t4, 0, sizeof(TAG4) ) ;

   rc = t4setupFunctions( d4->c4, t4 ) ;
   if ( rc != 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->t4list, t4 ) ;
   d4->numT4++ ;
   t4->t4 = tempPtr ;
   t4->d4 = d4 ;
   memset( t4->alias, 0, sizeof(t4->alias) ) ;
   strcpy( t4->alias, (const char*)t4->t4alias( t4->t4 ) ) ;

   return t4 ;
}

TAG4 S4PTR * S4FUNCTION d4tagPrev( DATA4 S4PTR *d4, TAG4 S4PTR *tagOn )
{
   TAG4 *t4 ;
   int rc ;
   void *tempPtr ;

   if ( tagOn == 0 )
      tempPtr = d4->d4tagPrev( d4->d4, 0 ) ;
   else
      tempPtr = d4->d4tagPrev( d4->d4, tagOn->t4 ) ;
   if ( tempPtr == 0 )
      return 0 ;

   if ( d4->numT4 > 0 )
      for ( t4 = (TAG4*) l4first( &d4->t4list ) ; t4 != 0 ; t4 = (TAG4*) l4next( &d4->t4list, t4 ) )
         if ( t4->t4 == tempPtr )
            return t4 ;

   t4 = (TAG4*) malloc( sizeof(TAG4) ) ;
   if ( t4 == 0 )
      return 0 ;
   memset( t4, 0, sizeof(TAG4) ) ;

   rc = t4setupFunctions( d4->c4, t4 ) ;
   if ( rc != 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->t4list, t4 ) ;
   d4->numT4++ ;
   t4->t4 = tempPtr ;
   t4->d4 = d4 ;
   memset( t4->alias, 0, sizeof(t4->alias) ) ;
   strcpy( t4->alias, (const char*)t4->t4alias( t4->t4 ) ) ;

   return t4 ;
}

void S4FUNCTION d4tagSelect( DATA4 S4PTR *d4, TAG4 S4PTR *tag )
{
   if ( tag == 0 )
      d4->d4tagSelect( d4->d4, 0 ) ;
   else
      d4->d4tagSelect( d4->d4, tag->t4 ) ;
}

TAG4 S4PTR * S4FUNCTION d4tagSelected( DATA4 S4PTR *d4 )
{
   TAG4 *t4 ;
   int rc ;
   void *tempPtr ;

   tempPtr = d4->d4tagSelected( d4->d4 ) ;
   if ( tempPtr == 0 )
      return 0 ;

   if ( d4->numT4 > 0 )
      for ( t4 = (TAG4*) l4first( &d4->t4list ) ; t4 != 0 ; t4 = (TAG4*) l4next( &d4->t4list, t4 ) )
         if ( t4->t4 == tempPtr )
            return t4 ;

   t4 = (TAG4*) malloc( sizeof(TAG4) ) ;
   if ( t4 == 0 )
      return 0 ;
   memset( t4, 0, sizeof(TAG4) ) ;

   rc = t4setupFunctions( d4->c4, t4 ) ;
   if ( rc != 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->t4list, t4 ) ;
   d4->numT4++ ;
   t4->t4 = tempPtr ;
   memset( t4->alias, 0, sizeof(t4->alias) ) ;
   strcpy( t4->alias, (const char*)t4->t4alias( t4->t4 ) ) ;

   return t4 ;
}

int S4FUNCTION d4tagSync( DATA4 S4PTR *d4, TAG4 S4PTR * const tag )
{
   return d4->d4tagSync( d4->d4, tag->t4 ) ;
}

int S4FUNCTION d4top( DATA4 S4PTR *d4 )
{
   return d4->d4top( d4->d4 ) ;
}

int S4FUNCTION d4unlock( DATA4 S4PTR *d4 )
{
   return d4->d4unlock( d4->d4 ) ;
}

// AS Feb 22/06 - support for new functionality...
int S4FUNCTION d4unlockAppend( DATA4 S4PTR *d4 )
{
   return d4->d4unlockAppend( d4->d4 ) ;
}

char S4FUNCTION d4versionCB( DATA4 S4PTR *d4 )
{
   return d4->d4versionCB( d4->d4 ) ;
}

// AS Feb 22/06 - support for new functionality...
long S4FUNCTION d4versionNumber( DATA4 *data )
{
   return data->d4versionNumber( data->d4 ) ;
}


int S4FUNCTION d4writeLow( DATA4 S4PTR *d4, const long recIn, const int unlock, const int doLock )
{
   return d4->d4writeLow( d4->d4, recIn, unlock, doLock ) ;
}

long S4FUNCTION d4writeBuffer( DATA4 S4PTR *d4, long numRecs )
{
   return d4->d4writeBuffer( d4->d4, numRecs ) ;
}

short S4FUNCTION d4writeVB(DATA4 *d4, long recno)
{
   return d4->d4writeVB( d4->d4, recno ) ;
}

int S4FUNCTION d4zap( DATA4 *d4, const long r1, const long r2 )
{
   return d4->d4zap( d4->d4, r1, r2 ) ;
}

int S4FUNCTION f4memoWriteFinish( DATA4 *data )
{
   return data->f4memoWriteFinish( data->d4 ) ;
}

int S4FUNCTION date4assignLow( char S4PTR *date, const long julianDay, int isOle )
{
   return g_c4->date4assignLow( date, julianDay, isOle ) ;
}

S4CONST char *S4FUNCTION date4cdow( const char S4PTR *date )
{
   return (const char*) g_c4->date4cdow( date ) ;
}

S4CONST char *S4FUNCTION date4cmonth( const char S4PTR *date )
{
   return (const char*) g_c4->date4cmonth( date ) ;
}

short S4FUNCTION date4day_v( char *dateString )
{
   return date4day( dateString ) ;
}

int S4FUNCTION date4dow( const char S4PTR *date )
{
   return g_c4->date4dow( date ) ;
}

void S4FUNCTION date4format( const char S4PTR *date, char S4PTR *result, char S4PTR *picture )
{
   g_c4->date4format( date, result, picture ) ;
}

double S4FUNCTION date4formatMdx( const char S4PTR *datePtr )
{
   return g_c4->date4formatMdx( (void *)datePtr ) ;
}

void S4FUNCTION date4init( char S4PTR *date, const char S4PTR *value, char S4PTR *picture )
{
   g_c4->date4init( date, value, picture ) ;
}

int S4FUNCTION date4isLeap( const char S4PTR *date )
{
   return g_c4->date4isLeap( date ) ;
}

long S4FUNCTION date4long( const char S4PTR *date )
{
   return g_c4->date4long( date ) ;
}

short S4FUNCTION date4month_v( char *dateString )
{
   return date4month( dateString ) ;
}

void S4FUNCTION date4timeNow( char S4PTR *date )
{
   g_c4->date4timeNow( date ) ;
}

void S4FUNCTION date4today( char S4PTR *date )
{
   g_c4->date4today( date ) ;
}

short S4FUNCTION date4year_v( char *dateString )
{
   return date4year( dateString ) ;
}

void S4FUNCTION error4callback( CODE4 S4PTR *c4, ERROR_CALLBACK errorCallback )
{
   c4->error4callback( c4->c4, errorCallback ) ;
}

int S4FUNCTION error4default( CODE4 S4PTR *c4, const int errCode1, const long errCode2 )
{
   return c4->error4default( c4->c4, errCode1, errCode2 ) ;
}

int S4FUNCTION error4describeDefault( CODE4 S4PTR *c4, const int errCode1, const long errCode2, const char S4PTR *s1, const char S4PTR *s2, const char S4PTR *s3 )
{
   return c4->error4describeDefault( c4->c4, errCode1, errCode2, s1, s2, s3 ) ;
}

short S4FUNCTION error4describeVB( CODE4 *c4, short errCode, long extraInfo, char* desc1, char* desc2, char* desc3 )
{
   return c4->error4describeVB( c4->c4, errCode, extraInfo, desc1, desc2, desc3 ) ;
}

void S4FUNCTION error4exitTest( CODE4 S4PTR *c4 )
{
   c4->error4exitTest( c4->c4 ) ;
}

int S4FUNCTION error4file( CODE4 S4PTR *c4, S4CONST char S4PTR *name, const int overwrite )
{
   return c4->error4file( c4->c4, name, overwrite ) ;
}

const char *S4FUNCTION error4lastDescription(CODE4 *c4)
{
   return (const char*)g_c4->error4lastDescription( c4->c4 ) ;
}

const char *S4FUNCTION error4lastDescriptionLow(void *c4)
{
   CODE4 *currC4 = 0 ;
   int found = 0 ;

   ResetEvent( hG_c4 ) ;
   for( currC4 = (CODE4 *)l4next( &c4list, currC4 ); currC4 != 0 ; currC4 = (CODE4 *)l4next( &c4list, currC4 ) )
   {
      if ( currC4->c4 == c4 )
      {
         found = 1 ;
         break ;
      }
   }
   SetEvent( hG_c4 ) ;

   if ( currC4 )
      return (const char*)currC4->error4lastDescription( c4 ) ;
   else
      return "" ;
}

int S4FUNCTION error4set( CODE4 S4PTR *c4, const int errCode )
{
   return c4->error4set( c4->c4, errCode ) ;
}

const char S4PTR * S4FUNCTION error4text( CODE4 S4PTR *c4, const long errCode )
{
   return (const char*) c4->error4text( c4->c4, errCode ) ;
}

short S4FUNCTION error4VB( CODE4 *c4, short errCode, long extraInfo )
{
   return (short) error4( c4, (int)errCode, extraInfo ) ;
}

static int e4setupFunctions( CODE4 *c4, EXPR4 *e4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "expr4double", (void **)&e4->expr4double, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4freeCB", (void **)&e4->expr4freeCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4lenCB", (void **)&e4->expr4lenCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4nullLow", (void **)&e4->expr4nullLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4parseCB", (void **)&e4->expr4parseCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4parseLow", (void **)&e4->expr4parseLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4source", (void **)&e4->expr4source, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4str", (void **)&e4->expr4str, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4true", (void **)&e4->expr4true, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4typeCB", (void **)&e4->expr4typeCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "expr4vary", (void **)&e4->expr4vary, &rc ) ;

   return 0 ;
}

DATA4 S4PTR* S4FUNCTION expr4dataCB( EXPR4 S4PTR *e4 )
{
   return e4->d4 ;
}

double S4FUNCTION expr4double( EXPR4 S4PTR *e4 )
{
   return e4->expr4double( e4->e4 ) ;
}

void S4FUNCTION expr4freeCB( EXPR4 S4PTR *e4 )
{
   e4->expr4freeCB( e4->e4 ) ;
   free( e4 ) ;
}

long S4FUNCTION expr4lenCB( EXPR4 S4PTR *e4 )
{
   return e4->expr4lenCB( e4->e4 ) ;
}

short S4FUNCTION expr4nullLow( const EXPR4 S4PTR *e4, const short forAdd )
{
   return e4->expr4nullLow( e4->e4, forAdd ) ;
}

EXPR4 S4PTR* S4FUNCTION expr4parseCB( DATA4 *d4, char S4PTR *string )
{
   EXPR4 *e4 ;

   e4 = (EXPR4*) malloc( sizeof(EXPR4) ) ;
   if ( e4 == 0 )
      return 0 ;
   memset( e4, 0, sizeof(EXPR4) ) ;

   /* if can't get ptr to expr4freeCB(), then can't free low-level EXPR4 */
   if ( e4setupFunctions( d4->c4, e4 ) != 0 )
   {
      free( e4 ) ;
      return 0 ;
   }

   e4->e4 = e4->expr4parseCB( d4->d4, string ) ;
   if ( e4->e4 == 0 )
   {
      free( e4 ) ;
      return 0 ;
   }

   return e4 ;
}

EXPR4 S4PTR *S4FUNCTION expr4parseLow( DATA4 S4PTR *d4, const char S4PTR *exprPtr, void *tagPtr )
{
   EXPR4 *e4 ;

   e4 = (EXPR4*) malloc( sizeof(EXPR4) ) ;
   if ( e4 == 0 )
      return 0 ;
   memset( e4, 0, sizeof(EXPR4) ) ;

   /* if can't get ptr to expr4freeCB(), then can't free low-level EXPR4 */
   if ( e4setupFunctions( d4->c4, e4 ) != 0 )
   {
      free( e4 ) ;
      return 0 ;
   }

   e4->e4 = e4->expr4parseLow( d4->d4, exprPtr, tagPtr ) ;
   if ( e4->e4 == 0 )
   {
      free( e4 ) ;
      return 0 ;
   }

   return e4 ;
}

S4CONST char S4PTR *S4FUNCTION expr4source( const EXPR4 S4PTR *e4 )
{
   return (const char*) e4->expr4source( e4->e4 ) ;
}

const char S4PTR *S4FUNCTION expr4str( EXPR4 S4PTR *e4 )
{
   return (const char*) e4->expr4str( e4->e4 ) ;
}

int S4FUNCTION expr4true( EXPR4 S4PTR *e4 )
{
   return e4->expr4true( e4->e4 ) ;
}

short S4FUNCTION expr4typeCB( EXPR4 *e4 )
{
   return e4->expr4typeCB( e4->e4 ) ;
}

int S4FUNCTION expr4vary( EXPR4 S4PTR *e4, char S4PTR * S4PTR *ptrPtr )
{
   return e4->expr4vary( e4->e4, ptrPtr ) ;
}

static int f4setupFunctions( CODE4 *c4, FIELD4 *f4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "f4accessMode", (void **)&f4->f4accessMode, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assign", (void **)&f4->f4assign, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignChar", (void **)&f4->f4assignChar, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignCharVB", (void **)&f4->f4assignCharVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignCurrency", (void **)&f4->f4assignCurrency, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignDateTime", (void **)&f4->f4assignDateTime, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignDouble", (void **)&f4->f4assignDouble, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignField", (void **)&f4->f4assignField, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignInt", (void **)&f4->f4assignInt, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignIntVB", (void **)&f4->f4assignIntVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignLong", (void **)&f4->f4assignLong, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignLongLong", (void **)&f4->f4assignLongLong, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignN", (void **)&f4->f4assignN, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignNVB", (void **)&f4->f4assignNVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignNotNull", (void **)&f4->f4assignNotNull, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignNull", (void **)&f4->f4assignNull, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignPtr", (void **)&f4->f4assignPtr, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4assignUnicode", (void **)&f4->f4assignUnicode, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4blank", (void **)&f4->f4blank, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4char", (void **)&f4->f4char, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4currency", (void **)&f4->f4currency, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4dateTime", (void **)&f4->f4dateTime, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4decimals", (void **)&f4->f4decimals, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4double", (void **)&f4->f4double, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4double2", (void **)&f4->f4double2, &rc ) ;
   // AS Feb 22/06 - support for new functionality...
   c4getFuncPtr( c4->hInst, "f4float", (void **)&f4->f4float, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4int", (void **)&f4->f4int, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4isMemo", (void **)&f4->f4isMemo, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4len", (void **)&f4->f4len, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4len_v", (void **)&f4->f4len_v, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4lockEnforce", (void **)&f4->f4lockEnforce, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4long", (void **)&f4->f4long, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4longLong", (void **)&f4->f4longLong, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoAssign", (void **)&f4->f4memoAssign, &rc ) ;
   // AS Feb 22/06 - support for new functionality...
   c4getFuncPtr( c4->hInst, "f4memoAssignFile", (void **)&f4->f4memoAssignFile, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoFile", (void **)&f4->f4memoFile, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoAssignN", (void **)&f4->f4memoAssignN, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoAssignNVB", (void **)&f4->f4memoAssignNVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoAssignUnicode", (void **)&f4->f4memoAssignUnicode, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoChanged", (void **)&f4->f4memoChanged, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoFree", (void **)&f4->f4memoFree, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoLen", (void **)&f4->f4memoLen, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoLenMax", (void **)&f4->f4memoLenMax, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoNcpy", (void **)&f4->f4memoNcpy, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoPtr", (void **)&f4->f4memoPtr, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoReadPart", (void **)&f4->f4memoReadPart, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoSetLen", (void **)&f4->f4memoSetLen, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoStr", (void **)&f4->f4memoStr, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4memoWritePart", (void **)&f4->f4memoWritePart, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4name", (void **)&f4->f4name, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4ncpy", (void **)&f4->f4ncpy, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4null", (void **)&f4->f4null, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4nullable", (void **)&f4->f4nullable, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4number", (void **)&f4->f4number, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4ptr", (void **)&f4->f4ptr, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4str", (void **)&f4->f4str, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4true", (void **)&f4->f4true, &rc ) ;
   c4getFuncPtr( c4->hInst, "f4type", (void **)&f4->f4type, &rc ) ;

   return rc ;
}

int S4FUNCTION f4accessMode( FIELD4 *f4 )
{
   return f4->f4accessMode( f4->f4 ) ;
}

void S4FUNCTION f4assign( FIELD4 S4PTR *f4, const char S4PTR *ptr )
{
   f4->f4assign( f4->f4, ptr ) ;
}

void S4FUNCTION f4assignChar( FIELD4 S4PTR *f4, const int chr )
{
   f4->f4assignChar( f4->f4, chr ) ;
}

void S4FUNCTION f4assignCharVB( FIELD4 *f4, short character )
{
   f4->f4assignCharVB( f4->f4, character ) ;
}

void S4FUNCTION f4assignCurrency( FIELD4 S4PTR *f4, const char S4PTR *ptr )
{
   f4->f4assignCurrency( f4->f4, ptr ) ;
}

void S4FUNCTION f4assignDateTime( FIELD4 S4PTR *f4, const char S4PTR *ptr )
{
   f4->f4assignDateTime( f4->f4, ptr ) ;
}

void S4FUNCTION f4assignDouble( FIELD4 S4PTR *f4, const double d )
{
   f4->f4assignDouble( f4->f4, d ) ;
}

void S4FUNCTION f4assignField( FIELD4 S4PTR *fieldTo, const FIELD4 S4PTR *fieldFrom )
{
   fieldTo->f4assignField( fieldTo->f4, fieldFrom->f4 ) ;
}

void S4FUNCTION f4assignInt( FIELD4 S4PTR *f4, const int value )
{
   f4->f4assignInt( f4->f4, value ) ;
}

void S4FUNCTION f4assignIntVB( FIELD4 *f4, const short iValue )
{
   f4->f4assignIntVB( f4->f4, iValue ) ;
}

void S4FUNCTION f4assignLong( FIELD4 S4PTR *f4, const long value )
{
   f4->f4assignLong( f4->f4, value ) ;
}

// AS Feb 22/06 - support for new functionality...
void S4FUNCTION f4assignLongLong( FIELD4 S4PTR *f4, const LONGLONG value )
{
   f4->f4assignLongLong( f4->f4, value ) ;
}

void S4FUNCTION f4assignNull( FIELD4 S4PTR *f4 )
{
   f4->f4assignNull( f4->f4 ) ;
}

void S4FUNCTION f4assignN( FIELD4 S4PTR *f4, const char S4PTR *ptr, const unsigned int ptrLen )
{
   f4->f4assignN( f4->f4, ptr, ptrLen ) ;
}

void S4FUNCTION f4assignNVB( FIELD4 *f4, const char* data, short dataLen )
{
   f4->f4assignNVB( f4->f4, data, dataLen ) ;
}

void S4FUNCTION f4assignNotNull( FIELD4 S4PTR *f4 )
{
   f4->f4assignNotNull( f4->f4 ) ;
}

char S4PTR * S4FUNCTION f4assignPtr( FIELD4 S4PTR *f4 )
{
   return (char*) f4->f4assignPtr( f4->f4 ) ;
}

void S4FUNCTION f4assignUnicode( FIELD4 S4PTR *f4, const WSTR5 *data )
{
   f4->f4assignUnicode( f4->f4, data ) ;
}

void S4FUNCTION f4blank( FIELD4 S4PTR *f4 )
{
   f4->f4blank( f4->f4 ) ;
}

int S4FUNCTION f4char( const FIELD4 S4PTR *f4 )
{
   return f4->f4char( f4->f4 ) ;
}

char S4PTR *S4FUNCTION f4currency( const FIELD4 S4PTR *f4, short numDec )
{
   return (char*) f4->f4currency( f4->f4, numDec ) ;
}

DATA4 S4PTR *S4FUNCTION f4data( const FIELD4 S4PTR *f4 )
{
   return (DATA4*) f4->d4 ;
}

char S4PTR *S4FUNCTION f4dateTime( const FIELD4 S4PTR *f4 )
{
   return (char*) f4->f4dateTime( f4->f4 ) ;
}

int S4FUNCTION f4decimals( const FIELD4 S4PTR *f4 )
{
   return f4->f4decimals( f4->f4 ) ;
}

double S4FUNCTION f4double( const FIELD4 S4PTR *f4 )
{
   return f4->f4double( f4->f4 ) ;
}

int S4FUNCTION f4double2( const FIELD4 S4PTR *f4, double S4PTR *ptr )
{
   return f4->f4double2( f4->f4, ptr ) ;
}

// AS Feb 22/06 - support for new functionality...
float S4FUNCTION f4float( const FIELD4 S4PTR *f4 )
{
   return f4->f4float( f4->f4 ) ;
}

int S4FUNCTION f4int( const FIELD4 S4PTR *f4 )
{
   return f4->f4int( f4->f4 ) ;
}

int S4FUNCTION f4isMemo( FIELD4 S4PTR *f4 )
{
   return f4->f4isMemo( f4->f4 ) ;
}

unsigned long S4FUNCTION f4len( const FIELD4 S4PTR *f4 )
{
   return f4->f4len( f4->f4 ) ;
}

short S4FUNCTION f4len_v( FIELD4 *f4 )
{
   return f4->f4len_v( f4->f4 ) ;
}

int S4FUNCTION f4lockEnforce( FIELD4 *f4 )
{
   return f4->f4lockEnforce( f4->f4 ) ;
}

long S4FUNCTION f4long( const FIELD4 S4PTR *f4 )
{
   return f4->f4long( f4->f4 ) ;
}

// AS Feb 22/06 - support for new functionality...
LONGLONG S4FUNCTION f4longLong( FIELD4 S4PTR *f4 )
{
   return f4->f4longLong( f4->f4 ) ;
}

int S4FUNCTION f4memoAssign( FIELD4 S4PTR *f4, const char S4PTR *ptr )
{
   return f4->f4memoAssign( f4->f4, ptr ) ;
}

int S4FUNCTION f4memoAssignFile( FIELD4 S4PTR *field, S4CONST char S4PTR *fileNameFrom )
{
   return field->f4memoAssignFile( field->f4, fileNameFrom ) ;
}

int S4FUNCTION f4memoFile( FIELD4 S4PTR *field, S4CONST char S4PTR *fileNameFrom )
{
   return field->f4memoFile( field->f4, fileNameFrom ) ;
}

int S4FUNCTION f4memoAssignN( FIELD4 S4PTR *f4, const char S4PTR *ptr, const unsigned int ptrLen )
{
   return f4->f4memoAssignN( f4->f4, ptr, ptrLen ) ;
}

short S4FUNCTION f4memoAssignNVB( FIELD4 *f4, char* data, short dataLen )
{
   return f4->f4memoAssignNVB( f4->f4, data, dataLen ) ;
}

short S4FUNCTION f4memoAssignUnicode( FIELD4 *f4, const WSTR5 *data )
{
   return f4->f4memoAssignUnicode( f4->f4, data ) ;
}

int S4FUNCTION f4memoFree( FIELD4 S4PTR *f4 )
{
   return f4->f4memoFree( f4->f4 ) ;
}

void S4FUNCTION f4memoChanged( FIELD4 S4PTR *f4, int value )
{
   f4->f4memoChanged( f4->f4, value ) ;
}

unsigned long S4FUNCTION f4memoLen( FIELD4 S4PTR *f4 )
{
   return f4->f4memoLen( f4->f4 ) ;
}

unsigned int S4FUNCTION f4memoLenMax( FIELD4 S4PTR *f4 )
{
   return f4->f4memoLenMax( f4->f4 ) ;
}

unsigned long S4FUNCTION f4memoNcpy( FIELD4 S4PTR *f4, char S4PTR *ptr, const unsigned int ptrLen )
{
   return f4->f4memoNcpy( f4->f4, ptr, ptrLen ) ;
}

char S4PTR * S4FUNCTION f4memoPtr( FIELD4 S4PTR *f4 )
{
   return (char*) f4->f4memoPtr( f4->f4 ) ;
}

unsigned int S4FUNCTION f4memoReadPart( FIELD4 S4PTR *f4, char S4PTR *outBuffer, unsigned int readLen, unsigned int startPos )
{
   return f4->f4memoReadPart( f4->f4, outBuffer, readLen, startPos ) ;
}

int S4FUNCTION f4memoSetLen( FIELD4 *f4, const unsigned int newLen )
{
   return f4->f4memoSetLen( f4->f4, newLen ) ;
}

S4CONST char S4PTR * S4FUNCTION f4memoStr( FIELD4 S4PTR *f4 )
{
   return (const char*) f4->f4memoStr( f4->f4 ) ;
}

int S4FUNCTION f4memoWritePart( FIELD4 *f4, char *dataToWrite, unsigned int dataLen, long memoLen, long offset )
{
   return f4->f4memoWritePart( f4->f4, dataToWrite, dataLen, memoLen, offset ) ;
}

S4CONST char S4PTR * S4FUNCTION f4name( S4CONST FIELD4 S4PTR *f4 )
{
   return (const char*) f4->f4name( f4->f4 ) ;
}

unsigned long S4FUNCTION f4ncpy( FIELD4 S4PTR *f4, char S4PTR *ptr, const unsigned int ptrLen )
{
   return f4->f4ncpy( f4->f4, ptr, ptrLen ) ;
}

int S4FUNCTION f4null( const FIELD4 S4PTR *f4 )
{
   return f4->f4null( f4->f4 ) ;
}

int S4FUNCTION f4nullable( const FIELD4 *f4 )
{
   return f4->f4nullable( f4->f4 ) ;
}

int S4FUNCTION f4number( const FIELD4 S4PTR *f4 )
{
   return f4->f4number( f4->f4 ) ;
}

char S4PTR * S4FUNCTION f4ptr( const FIELD4 S4PTR *f4 )
{
   return (char*) f4->f4ptr( f4->f4 ) ;
}

char S4PTR * S4FUNCTION f4str( FIELD4 S4PTR *f4 )
{
   return (char*) f4->f4str( f4->f4 ) ;
}

int S4FUNCTION f4true( const FIELD4 S4PTR *f4 )
{
   return f4->f4true( f4->f4 ) ;
}

int S4FUNCTION f4type( const FIELD4 S4PTR *f4 )
{
   return f4->f4type( f4->f4 ) ;
}

static int file4setupFunctions( CODE4 *c4, FILE4 *f4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "file4close", (void **)&f4->file4close, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4create", (void **)&f4->file4create, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4flush", (void **)&f4->file4flush, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4free", (void **)&f4->file4free, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4lenLow2", (void **)&f4->file4lenLow2, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4lenSetLow2", (void **)&f4->file4lenSetLow2, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4lockInternal", (void **)&f4->file4lockInternal, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4nameLow", (void **)&f4->file4nameLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4open", (void **)&f4->file4open, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4optimizeLow", (void **)&f4->file4optimizeLow, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4optimizeWrite", (void **)&f4->file4optimizeWrite, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4read", (void **)&f4->file4read, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4readAll", (void **)&f4->file4readAll, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4refresh", (void **)&f4->file4refresh, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4replace", (void **)&f4->file4replace, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4unlockInternal", (void **)&f4->file4unlockInternal, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4write", (void **)&f4->file4write, &rc ) ;

   c4getFuncPtr( c4->hInst, "file4seqReadAlloc", (void **)&f4->file4seqReadAlloc, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4seqReadFree", (void **)&f4->file4seqReadFree, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4seqReadInit", (void **)&f4->file4seqReadInit, &rc ) ;

   c4getFuncPtr( c4->hInst, "file4seqWriteAlloc", (void **)&f4->file4seqWriteAlloc, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4seqWriteFree", (void **)&f4->file4seqWriteFree, &rc ) ;
   c4getFuncPtr( c4->hInst, "file4seqWriteInit", (void **)&f4->file4seqWriteInit, &rc ) ;

   return rc ;
}

int    S4FUNCTION file4close( FILE4 S4PTR *f4 )
{
   int rc ;

   rc = f4->file4close( f4->f4 ) ;
   if ( rc != 0 )
      return rc ;

   f4->file4free( &f4->f4 ) ;  /* free memory alloc'd by file4alloc() */

   return rc ;
}

int    S4FUNCTION file4create( FILE4 S4PTR *f4, CODE4 S4PTR *c4, S4CONST char S4PTR *name, const int doAlloc )
{
   long rc = 0 ;

   rc = c4->file4alloc( &f4->f4 ) ;
   if ( f4->f4 == 0 )
      return rc ;

   rc = file4setupFunctions( c4, f4 ) ;
   if ( rc != 0 )
   {
      f4->file4free( &f4->f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
      return rc ;
   }

   rc = f4->file4create( f4->f4, c4->c4, name, doAlloc ) ;
   if ( rc != 0 )
      f4->file4free( &f4->f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
   f4->code4 = c4 ;

   return rc ;
}

int    S4FUNCTION file4flush( FILE4 S4PTR *f4 )
{
   return f4->file4flush( f4->f4 ) ;
}

unsigned long S4FUNCTION file4lenLow2( FILE4 S4PTR *f4 )
{
   return f4->file4lenLow2( f4->f4 ) ;
}

int S4FUNCTION file4lenSetLow2( FILE4 S4PTR *f4, long len )
{
   return f4->file4lenSetLow2( f4->f4, len ) ;
}

int    S4FUNCTION file4lockInternal( FILE4 S4PTR *f4, unsigned long posStart, long posStartHi, unsigned long numBytes, long numBytesHi )
{
   return f4->file4lockInternal( f4->f4, posStart, posStartHi, numBytes, numBytesHi ) ;
}

const char * S4FUNCTION file4nameLow( FILE4 S4PTR *f4 )
{
   return (const char*) f4->file4nameLow( f4->f4 ) ;
}

int    S4FUNCTION file4open( FILE4 S4PTR *f4, CODE4 S4PTR *c4, S4CONST char S4PTR *name, const int doAlloc )
{

   long rc = 0 ;

   rc = c4->file4alloc( &f4->f4 ) ;
   if ( f4->f4 == 0 )
      return rc ;

   rc = file4setupFunctions( c4, f4 ) ;
   if ( rc != 0 )
   {
      f4->file4free( &f4->f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
      return rc ;
   }

   rc = f4->file4open( f4->f4, c4->c4, name, doAlloc ) ;
   if ( rc != 0 )
      f4->file4free( &f4->f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
   f4->code4 = c4 ;

   return rc ;
}

int    S4FUNCTION file4optimizeLow( FILE4 S4PTR *f4, const int optFlagIn, const int fileType, const long expectedReadSize, const void S4PTR *ownerPtr )
{
   return f4->file4optimizeLow( f4->f4, optFlagIn, fileType, expectedReadSize, ownerPtr ) ;
}

int    S4FUNCTION file4optimizeWrite( FILE4 S4PTR *f4, const int optFlag )
{
   return f4->file4optimizeWrite( f4->f4, optFlag ) ;
}

unsigned int S4FUNCTION file4read( FILE4 S4PTR *f4, const long pos, void S4PTR *ptr, const unsigned int len )
{
   return f4->file4read( f4->f4, pos, ptr, len ) ;
}

int    S4FUNCTION file4readAll( FILE4 S4PTR *f4, const long pos, void S4PTR *ptr, const unsigned int len )
{
   return f4->file4readAll( f4->f4, pos, ptr, len ) ;
}

int    S4FUNCTION file4refresh( FILE4 S4PTR *f4 )
{
   return f4->file4refresh( f4->f4 ) ;
}

int    S4FUNCTION file4replace( FILE4 S4PTR *keepFile, FILE4 S4PTR *newFile )
{
   int rc ;

   rc = keepFile->file4replace( keepFile->f4, newFile->f4 ) ;
   if ( rc != 0 )
      return rc ;

   keepFile->file4free( newFile->f4 ) ;

   return rc ;
}

int    S4FUNCTION file4unlockInternal( FILE4 S4PTR *f4, unsigned long posStart, long posStartHi, unsigned long numBytes, long numBytesHi )
{
   return f4->file4unlockInternal( f4->f4, posStart, posStartHi, numBytes, numBytesHi ) ;
}

int    S4FUNCTION file4write( FILE4 S4PTR *f4, const long pos, const void S4PTR *ptr, const unsigned int len )
{
   return f4->file4write( f4->f4, pos, ptr, len ) ;
}

unsigned int S4FUNCTION file4seqRead( FILE4SEQ_READ S4PTR *seqRead, void S4PTR *ptr, unsigned ptrLen )
{
   return seqRead->file4seqRead( seqRead->f4seqRead, ptr, ptrLen ) ;
}

int S4FUNCTION file4seqReadAll( FILE4SEQ_READ S4PTR *seqRead, void S4PTR *ptr, const unsigned int ptrLen )
{
   return seqRead->file4seqReadAll( seqRead->f4seqRead, ptr, ptrLen ) ;
}

int S4FUNCTION file4seqReadInit( FILE4SEQ_READ S4PTR *seqRead, FILE4 S4PTR *file, long startPos, void S4PTR *ptr, const unsigned ptrLen )
{
   long rc = 0 ;

   rc = file->file4seqReadAlloc( &seqRead->f4seqRead ) ;

   if ( rc == 0 )
   {
      rc = file->file4seqReadInit( seqRead->f4seqRead, file->f4, startPos, ptr, ptrLen ) ;

      if ( rc == 0 )
      {
         c4getFuncPtr( file->code4->hInst, "file4seqRead", (void **)&seqRead->file4seqRead, &rc ) ;
         c4getFuncPtr( file->code4->hInst, "file4seqReadFree", (void **)&seqRead->file4seqReadFree, &rc ) ;
         c4getFuncPtr( file->code4->hInst, "file4seqReadAll", (void **)&seqRead->file4seqReadAll, &rc ) ;
      }
      else
         file->file4seqReadFree( &seqRead->f4seqRead ) ;
   }

   return rc ;
}

int S4FUNCTION file4seqWrite( FILE4SEQ_WRITE S4PTR *seqWrite, const void S4PTR *ptr, const unsigned int ptrLen )
{
   return seqWrite->file4seqWrite( seqWrite->f4seqWrite, ptr, ptrLen ) ;
}

int S4FUNCTION file4seqWriteFlush( FILE4SEQ_WRITE S4PTR *seqWrite )
{
   return seqWrite->file4seqWriteFlush( seqWrite->f4seqWrite ) ;
}

int S4FUNCTION file4seqWriteInit( FILE4SEQ_WRITE S4PTR *seqWrite, FILE4 S4PTR *file, const long startPos, void S4PTR *ptr, const unsigned int ptrLen )
{
   long rc = 0 ;

   rc = file->file4seqWriteAlloc( &seqWrite->f4seqWrite ) ;

   if ( rc == 0 )
   {
      rc = file->file4seqWriteInit( seqWrite->f4seqWrite, file->f4, startPos, ptr, ptrLen ) ;

      if ( rc == 0 )
      {
         c4getFuncPtr( file->code4->hInst, "file4seqWrite", (void **)&seqWrite->file4seqWrite, &rc ) ;
         c4getFuncPtr( file->code4->hInst, "file4seqWriteFlush", (void **)&seqWrite->file4seqWriteFlush, &rc ) ;
         c4getFuncPtr( file->code4->hInst, "file4seqWriteFree", (void **)&seqWrite->file4seqWriteFree, &rc ) ;
         c4getFuncPtr( file->code4->hInst, "file4seqWriteRepeat", (void **)&seqWrite->file4seqWriteRepeat, &rc ) ;
      }
      else
         file->file4seqWriteFree( &seqWrite->f4seqWrite ) ;
   }

   return rc ;
}

int S4FUNCTION file4seqWriteRepeat( FILE4SEQ_WRITE S4PTR *seqWrite, const long nRepeat, const char ch )
{
   return seqWrite->file4seqWriteRepeat( seqWrite->f4seqWrite, nRepeat, ch ) ;
}

static int i4setupFunctions( CODE4 *c4, INDEX4 *i4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "i4close", (void **)&i4->i4close, &rc ) ;
   c4getFuncPtr( c4->hInst, "i4fileName", (void **)&i4->i4fileName, &rc ) ;
   c4getFuncPtr( c4->hInst, "i4reindex", (void **)&i4->i4reindex, &rc ) ;
   c4getFuncPtr( c4->hInst, "i4tag", (void **)&i4->i4tag, &rc ) ;
   c4getFuncPtr( c4->hInst, "i4tagAdd", (void **)&i4->i4tagAdd, &rc ) ;
   c4getFuncPtr( c4->hInst, "i4tagInfo", (void **)&i4->i4tagInfo, &rc ) ;

   return rc ;
}

int S4FUNCTION i4close( INDEX4 S4PTR *i4 )
{
   TAG4 *t4;
   int rc = 0 ;

   if ( i4->numT4 > 0 )
      for ( t4 = (TAG4*)l4first( &i4->t4list ); t4 != 0 ; t4 = (TAG4*)l4first( &i4->t4list ) )
      {
         l4remove( &i4->t4list, t4 ) ;
         free( t4 ) ;
      }

   rc = i4->i4close( i4->i4 ) ;
   if ( rc != 0 )
      return rc ;

   l4remove( &i4->d4->i4list, i4 ) ;
   free( i4 ) ;

   return rc ;
}

INDEX4 S4PTR *S4FUNCTION i4create( DATA4 S4PTR *d4, const char S4PTR *name, const TAG4INFO S4PTR *tagData )
{
   INDEX4 *i4 ;
   long rc ;

   i4 = (INDEX4*) malloc( sizeof(INDEX4) ) ;
   if ( i4 == 0 )
      return 0 ;
   memset( i4, 0, sizeof(INDEX4) ) ;

   c4getFuncPtr( d4->c4->hInst, "i4create", (void **)&i4->i4create, &rc ) ;

   i4->i4 = i4->i4create( d4->d4, name, tagData ) ;
   if ( i4->i4 == 0 )
   {
      free( i4 ) ;
      return 0 ;
   }

   rc = i4setupFunctions( d4->c4, i4 ) ;
   if ( rc != 0 )
   {
      free( i4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->i4list, i4 ) ;
   d4->numI4++ ;
   i4->d4 = d4 ;
   memset( i4->name, 0, sizeof(i4->name) ) ;
   // LY Aug 9/04 : name may be null
   if ( name == 0 )
      strcpy( i4->name, (const char*) d4->d4alias( d4->d4 ) ) ;
   else
      d4->c4->u4namePiece( i4->name, sizeof(i4->name), name, 0, 0 ) ;

   return i4 ;
}

short S4FUNCTION i4createWithProgress( DATA4 S4PTR *d4, const char S4PTR *name, const TAG4INFO S4PTR *tagData, REINDEX_CALLBACK callback, long milliseconds )
{
   INDEX4 *i4 ;
   long rc ;

   i4 = (INDEX4*) malloc( sizeof(INDEX4) ) ;
   if ( i4 == 0 )
      return 0 ;
   memset( i4, 0, sizeof(INDEX4) ) ;

   c4getFuncPtr( d4->c4->hInst, "i4createWithProgress", (void **)&i4->i4createWithProgress, &rc ) ;

   rc = i4->i4createWithProgress( d4->d4, name, tagData, callback, milliseconds ) ;
   if ( rc != 0 )
   {
      free( i4 ) ;
      return (short)rc ;
   }

   rc = i4setupFunctions( d4->c4, i4 ) ;
   if ( rc != 0 )
   {
      free( i4 ) ;
      return (short)rc ;
   }

   d4->c4->l4add( &d4->i4list, i4 ) ;
   d4->numI4++ ;
   i4->d4 = d4 ;
   memset( i4->name, 0, sizeof(i4->name) ) ;
   // LY Aug 9/04 : name may be null
   if ( name == 0 )
      strcpy( i4->name, (const char*) d4->d4alias( d4->d4 ) ) ;
   else
      d4->c4->u4namePiece( i4->name, sizeof(i4->name), name, 0, 0 ) ;

   return 0 ;
}

const char S4PTR *S4FUNCTION i4fileName( INDEX4 S4PTR *i4 )
{
   return (const char*) i4->i4fileName( i4->i4 ) ;
}

INDEX4 S4PTR *S4FUNCTION i4open( DATA4 S4PTR *d4, const char S4PTR *name )
{
   INDEX4 *i4 ;
   long rc ;

   i4 = (INDEX4*) malloc( sizeof(INDEX4) ) ;
   if ( i4 == 0 )
      return 0 ;
   memset( i4, 0, sizeof(INDEX4) ) ;

   c4getFuncPtr( d4->c4->hInst, "i4open", (void **)&i4->i4open, &rc ) ;

   i4->i4 = i4->i4open( d4->d4, name ) ;
   if ( i4->i4 == 0 )
   {
      free( i4 ) ;
      return 0 ;
   }

   rc = i4setupFunctions( d4->c4, i4 ) ;
   if ( rc != 0 )
   {
      free( i4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->i4list, i4 ) ;
   d4->numI4++ ;
   i4->d4 = d4 ;
   memset( i4->name, 0, sizeof(i4->name) ) ;
   if ( name )
      strcpy( i4->name, name ) ;
   else
      // LY Aug 9/04 : do not copy entire string from i4fileName() to i4->name
      d4->c4->u4namePiece( i4->name, sizeof(i4->name), i4->i4fileName( i4->i4 ), 0, 0 ) ;

   return i4 ;
}

int S4FUNCTION i4reindex( INDEX4 S4PTR *i4 )
{
   return i4->i4reindex( i4->i4 ) ;
}

TAG4 S4PTR *S4FUNCTION i4tag( INDEX4 S4PTR *i4, const char S4PTR *name )
{
   TAG4 *t4 ;
   int rc ;

   if ( i4->numT4 > 0 )
      for ( t4 = (TAG4*) l4first( &i4->t4list ) ; t4 != 0 ; t4 = (TAG4*) l4next( &i4->t4list, t4 ) )
      {
         // AS Jul 20/06 - incorrect...if the field name is a substring, we get the wrong field eg. ("data" and "data_len")
         if ( stricmp( t4->alias, name ) == 0 )
            return t4 ;
      }

   t4 = (TAG4*) malloc( sizeof(TAG4) ) ;
   if ( t4 == 0 )
      return 0 ;
   memset( t4, 0, sizeof(TAG4) ) ;

   t4->t4 = i4->i4tag( i4->i4, name ) ;
   if ( t4->t4 == 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   rc = t4setupFunctions( i4->d4->c4, t4 ) ;
   if ( rc != 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   i4->d4->c4->l4add( &i4->t4list, t4 ) ;
   i4->numT4++ ;
   memset( t4->alias, 0, sizeof(t4->alias) ) ;
   strcpy( t4->alias, (const char*)t4->t4alias( t4->t4 ) ) ;

   return t4 ;
}

int S4FUNCTION i4tagAdd( INDEX4 S4PTR *i4, const TAG4INFO S4PTR *tagData )
{
   return i4->i4tagAdd( i4->i4, tagData ) ;
}

TAG4INFO *S4FUNCTION i4tagInfo( INDEX4 *i4 )
{
   return (TAG4INFO*) i4->i4tagInfo( i4->i4 ) ;
}

int S4FUNCTION i4tagRemove( TAG4 S4PTR *t4 )
{
   return t4->i4tagRemove( t4->t4 ) ;
}

void S4FUNCTION l4addAfter( LIST4 S4PTR *listIn, void S4PTR *anchor, void S4PTR *item )
{
   g_c4->l4addAfter( listIn, anchor, item ) ;
}

void S4FUNCTION l4addBefore( LIST4 S4PTR *listIn, void S4PTR *anchor, void S4PTR *item )
{
   g_c4->l4addBefore( listIn, anchor, item ) ;
}

void S4PTR *S4FUNCTION l4prev( const LIST4 S4PTR *list, const void S4PTR *item )
{
   return g_c4->l4prev( list, item ) ;
}

void S4PTR *S4FUNCTION l4remove( LIST4 S4PTR *list, void S4PTR *item )
{
   return g_c4->l4remove( list, item ) ;
}

void S4PTR *S4FUNCTION mem4allocDefault( MEM4 S4PTR *m4, int size )
{
   return g_c4->mem4allocDefault( m4, size ) ;
}

MEM4 S4PTR *S4FUNCTION mem4createDefault( CODE4 S4PTR *c4, int start, const unsigned int uSize, int expand, const int isTemp )
{
   return (MEM4 *)c4->mem4createDefault( c4->c4, start, uSize, expand, isTemp ) ;
}

int S4FUNCTION mem4freeDefault( MEM4 S4PTR *m4, void S4PTR *freePtr )
{
   return g_c4->mem4freeDefault( m4, freePtr ) ;
}

void S4FUNCTION mem4release( MEM4 S4PTR *m4 )
{
   g_c4->mem4release( m4 ) ;
}

static int r4setupFunctions( CODE4 *c4, RELATE4 *r4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "relate4bottom", (void **)&r4->relate4bottom, &rc ) ;
   // AS Feb 22/06 - support for new functionality...
   c4getFuncPtr( c4->hInst, "relate4callbackInit", (void **)&r4->relate4callbackInit, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4callbackInitUndo", (void **)&r4->relate4callbackInitUndo, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4querySetCallbackInit", (void **)&r4->relate4querySetCallbackInit, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4querySetCallbackInitUndo", (void **)&r4->relate4querySetCallbackInitUndo, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4changed", (void **)&r4->relate4changed, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4count", (void **)&r4->relate4count, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4createSlave", (void **)&r4->relate4createSlave, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4doAll", (void **)&r4->relate4doAll, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4doOne", (void **)&r4->relate4doOne, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4eof", (void **)&r4->relate4eof, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4errorActionVB", (void **)&r4->relate4errorActionVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4freeVB", (void **)&r4->relate4freeVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4init", (void **)&r4->relate4init, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4lockAdd", (void **)&r4->relate4lockAdd, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4masterExprCB", (void **)&r4->relate4masterExprCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4matchLenVB", (void **)&r4->relate4matchLenVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4next", (void **)&r4->relate4next, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4optimizeable", (void **)&r4->relate4optimizeable, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4querySet", (void **)&r4->relate4querySet, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4readBuffer", (void **)&r4->relate4readBuffer, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4retain", (void **)&r4->relate4retain, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4skip", (void **)&r4->relate4skip, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4skipEnableVB", (void **)&r4->relate4skipEnableVB, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4skipMaster", (void **)&r4->relate4skipMaster, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4sortSet", (void **)&r4->relate4sortSet, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4top", (void **)&r4->relate4top, &rc ) ;
   c4getFuncPtr( c4->hInst, "relate4typeVB", (void **)&r4->relate4typeVB, &rc ) ;

   return rc ;
}

int S4FUNCTION relate4bottom( RELATE4 S4PTR *r4 )
{
   return r4->relate4bottom( r4->r4 ) ;
}

int S4FUNCTION relate4callbackInit( RELATE4 *r4, QUERY_CALLBACK callback, long numMicroseconds )
{
   return r4->relate4callbackInit( r4->r4, callback, numMicroseconds ) ;
}

int S4FUNCTION relate4callbackInitUndo( RELATE4 *r4 )
{
   return r4->relate4callbackInitUndo( r4->r4 ) ;
}

int S4FUNCTION relate4querySetCallbackInit( RELATE4 *r4, QUERY_SET_CALLBACK callback )
{
   return r4->relate4querySetCallbackInit( r4->r4, callback ) ;
}

int S4FUNCTION relate4querySetCallbackInitUndo( RELATE4 *r4 )
{
   return r4->relate4querySetCallbackInitUndo( r4->r4 ) ;
}

int S4FUNCTION relate4changed( RELATE4 S4PTR *r4 )
{
   return r4->relate4changed( r4->r4 ) ;
}

unsigned long S4FUNCTION relate4count( RELATE4 S4PTR *r4 )
{
   return r4->relate4count( r4->r4 ) ;
}

RELATE4 S4PTR * S4FUNCTION relate4createSlave( RELATE4 S4PTR *master, DATA4 S4PTR *slaveData, const char S4PTR *masterExpr, TAG4 S4PTR *slaveTag )
{
   RELATE4 *r4 ;
   long rc = 0 ;

   r4 = (RELATE4*) malloc( sizeof(RELATE4) ) ;
   if ( r4 == 0 )
      return 0 ;
   memset( r4, 0, sizeof(RELATE4) ) ;

   rc = r4setupFunctions( master->d4->c4, r4 ) ;
   if ( rc != 0 )
   {
      free( r4 ) ;
      return 0 ;
   }

   r4->r4 = master->relate4createSlave( master->r4, slaveData->d4, masterExpr, slaveTag->t4 ) ;
   if ( r4->r4 == 0 )
   {
      free( r4 ) ;
      return 0 ;
   }

   master->d4->c4->l4add( &master->slaveList, r4 ) ;
   master->numR4++ ;
   r4->masterR4 = master ;
   r4->d4 = slaveData ;
   r4->t4 = slaveTag ;

   return r4 ;
}

DATA4 S4PTR* S4FUNCTION relate4dataCB( RELATE4 S4PTR *r4 )
{
   return r4->d4 ;
}

TAG4 S4PTR* S4FUNCTION relate4dataTagCB( RELATE4 *r4 )
{
   return r4->t4 ;
}

int S4FUNCTION relate4doAll( RELATE4 S4PTR *r4 )
{
   return r4->relate4doAll( r4->r4 ) ;
}

int S4FUNCTION relate4doOne( RELATE4 S4PTR *r4 )
{
   return r4->relate4doOne( r4->r4 ) ;
}

int S4FUNCTION relate4eof( RELATE4 S4PTR *r4 )
{
   return r4->relate4eof( r4->r4 ) ;
}

short S4FUNCTION relate4errorActionVB( RELATE4 S4PTR *r4, short code )
{
   return r4->relate4errorActionVB( r4->r4, code ) ;
}

void relate4freeSlaves( RELATE4 S4PTR *r4, RELATE4 S4PTR *tempPtr )
{
   l4remove( &r4->slaveList, tempPtr ) ;
   free( tempPtr ) ;
   r4->numR4-- ;
}

int relate4closeSlaves( RELATE4 S4PTR *r4, short doClose )
{
   RELATE4 *tempPtr ;
   int rc = 0 ;

   if ( r4->numR4 > 0 )
      for( tempPtr = (RELATE4*) l4first( &r4->slaveList ); tempPtr != 0 && rc == 0; tempPtr = (RELATE4*) l4first( &r4->slaveList ) )
      {
         rc = relate4closeSlaves( tempPtr, doClose ) ;
         relate4freeSlaves( r4, tempPtr ) ;
      }
   if ( doClose )
   {
      d4close( r4->d4 ) ;
//      l4remove( &r4->d4->c4->d4list, r4->d4 ) ; /* LY 2002/11/19 : actual DATA4s already freed; remove wrapper DATA4s from list */
   }

   return rc ;
}

short S4FUNCTION relate4freeVB( RELATE4* r4, short doClose )
{
   int rc = 0 ;

   rc = r4->relate4freeVB( r4->r4, 0 ) ;
   if ( rc != 0 )
      return rc ;

   rc = relate4closeSlaves( r4, doClose ) ;
   if ( rc != 0 )
      return rc ;

   free( r4 ) ;   /* LY 2002/04/11 : wasn't freeing RELATE4 */

   return rc ;
}

RELATE4 S4PTR * S4FUNCTION relate4init( DATA4 S4PTR *d4 )
{
   RELATE4 *r4 ;
   long rc = 0 ;

   r4 = (RELATE4*) malloc( sizeof(RELATE4) ) ;
   if ( r4 == 0 )
      return 0 ;
   memset( r4, 0, sizeof(RELATE4) ) ;

   rc = r4setupFunctions( d4->c4, r4 ) ;
   if ( rc != 0 )
   {
      free( r4 ) ;
      return 0 ;
   }

   r4->r4 = r4->relate4init( d4->d4 ) ;
   if ( r4->r4 == 0 )
   {
      free( r4 ) ;
      return 0 ;
   }

   r4->d4 = d4 ;

   return r4 ;
}

int S4FUNCTION relate4lockAdd( RELATE4 S4PTR *r4 )
{
   return r4->relate4lockAdd( r4->r4 ) ;
}

RELATE4 S4PTR* S4FUNCTION relate4masterCB( RELATE4 S4PTR *r4 )
{
   return r4->masterR4 ;
}

const char S4PTR* S4FUNCTION relate4masterExprCB( RELATE4 *r4 )
{
   return (const char*) r4->relate4masterExprCB( r4->r4 ) ;
}

short S4FUNCTION relate4matchLenVB( RELATE4 S4PTR *r4, short len )
{
   return r4->relate4matchLenVB( r4->r4, len ) ;
}

int S4FUNCTION relate4next( RELATE4 **r4 )
{
   RELATE4 *masterR4, *tempR4 ;
   void *tempPtr = (*r4)->r4 ;
   int i, rc = (*r4)->relate4next( &tempPtr ) ;

   if ( rc != r4complete )
   {
      masterR4 = *r4 ;
      if ( rc < 0 )
      {
         for ( i = rc ; i < 0 ; i++ )
            masterR4 = masterR4->masterR4 ;
      }

      for( tempR4 = (RELATE4 *) l4first( &masterR4->slaveList ); tempR4 != 0 ; tempR4 = (RELATE4*) l4next( &masterR4->slaveList, tempR4 ) )
      {
         if ( tempR4->r4 == tempPtr )
         {
            *r4 = tempR4 ;
            break ;
         }
      }
      if ( !tempR4 )
      {
         if ( masterR4->masterR4 )
         {
            masterR4 =  masterR4->masterR4 ;
            for( tempR4 = (RELATE4 *) l4first( &masterR4->slaveList ); tempR4 != 0 ; tempR4 = (RELATE4*) l4next( &masterR4->slaveList, tempR4 ) )
            {
               if ( tempR4->r4 == tempPtr )
               {
                  *r4 = tempR4 ;
                  break ;
               }
            }
         }
      }
      if ( !tempR4 )
         return g_c4->error4( 0, e4info, 94414L ) ;
   }
   else
      *r4 = 0 ;

   return rc ;
}

int S4FUNCTION relate4optimizeable( RELATE4 S4PTR *r4 )
{
   return r4->relate4optimizeable( r4->r4 ) ;
}

int S4FUNCTION relate4querySet( RELATE4 S4PTR *r4, const char S4PTR *queryStr )
{
   return r4->relate4querySet( r4->r4, queryStr ) ;
}

long S4FUNCTION relate4readBuffer( RELATE4 S4PTR *r4, long numRecs, short doMemos )
{
   return r4->relate4readBuffer( r4->r4, numRecs, doMemos ) ;
}

int S4FUNCTION relate4retain( RELATE4 S4PTR *r4, int flag )
{
   return r4->relate4retain( r4->r4, flag ) ;
}

int S4FUNCTION relate4skip( RELATE4 S4PTR *r4, const long numRecs )
{
   return r4->relate4skip( r4->r4, numRecs ) ;
}

short S4FUNCTION relate4skipEnableVB( RELATE4 S4PTR *r4, short doEnable )
{
   return r4->relate4skipEnableVB( r4->r4, doEnable ) ;
}

int S4FUNCTION relate4skipMaster( RELATE4 *r4, long numSkip )
{
   return r4->relate4skipMaster( r4->r4, numSkip ) ;
}

int S4FUNCTION relate4sortSet( RELATE4 S4PTR *r4, const char S4PTR *sortExpr )
{
   return r4->relate4sortSet( r4->r4, sortExpr ) ;
}

int S4FUNCTION relate4top( RELATE4 S4PTR *r4 )
{
   return r4->relate4top( r4->r4 ) ;
}

short S4FUNCTION relate4typeVB( RELATE4 S4PTR *r4, short relateType )
{
   return r4->relate4typeVB( r4->r4, relateType ) ;
}

int S4FUNCTION sort4init( SORT4 S4PTR *s4, CODE4 S4PTR *c4, const int sortLen, const int infoLen )
{
   long rc = 0 ;

   rc = c4->sort4alloc( &s4->s4 ) ;

   if ( rc == 0 )
   {
      rc = c4->sort4init( s4->s4, c4->c4, sortLen, infoLen ) ;

      if ( rc == 0 )
      {
         c4getFuncPtr( c4->hInst, "sort4assignCmp2", (void **)&s4->sort4assignCmp2, &rc ) ;
         c4getFuncPtr( c4->hInst, "sort4free", (void **)&s4->sort4free, &rc ) ;
         c4getFuncPtr( c4->hInst, "sort4free2", (void **)&s4->sort4free2, &rc ) ;
         c4getFuncPtr( c4->hInst, "sort4get", (void **)&s4->sort4get, &rc ) ;
         c4getFuncPtr( c4->hInst, "sort4getInit", (void **)&s4->sort4getInit, &rc ) ;
         c4getFuncPtr( c4->hInst, "sort4put", (void **)&s4->sort4put, &rc ) ;
      }
      else
         c4->sort4free2( &s4->s4 ) ;
   }

   return rc ;
}

void S4FUNCTION sort4assignCmp2( SORT4 S4PTR *s4, S4CMP_FUNCTION function )
{
   s4->sort4assignCmp2( s4->s4, function ) ;
}

int S4FUNCTION sort4free( SORT4 S4PTR *s4 )
{
   return s4->sort4free( s4->s4 ) ;
}

int S4FUNCTION sort4get( SORT4 S4PTR *s4, S4LONG S4PTR *recPtr, void S4PTR * S4PTR *sortData, void S4PTR * S4PTR *infoPtr )
{
   return s4->sort4get( s4->s4, recPtr, sortData, infoPtr ) ;
}

int S4FUNCTION sort4getInit( SORT4 S4PTR *s4 )
{
   return s4->sort4getInit( s4->s4 ) ;
}

int S4FUNCTION sort4put( SORT4 S4PTR *s4, const S4LONG rec, const void S4PTR *sortData, const void S4PTR *infoPtr )
{
   return s4->sort4put( s4->s4, rec, sortData, infoPtr ) ;
}

static int tfile4setupFunctions( CODE4 *c4, TAG4FILE *tf4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "tfile4bottom", (void **)&tf4->tfile4bottom, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4count", (void **)&tf4->tfile4count, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4eof", (void **)&tf4->tfile4eof, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4go", (void **)&tf4->tfile4go, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4key", (void **)&tf4->tfile4key, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4keyLenExport", (void **)&tf4->tfile4keyLenExport, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4position", (void **)&tf4->tfile4position, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4positionSet", (void **)&tf4->tfile4positionSet, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4recNo", (void **)&tf4->tfile4recNo, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4seek", (void **)&tf4->tfile4seek, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4skip", (void **)&tf4->tfile4skip, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4skipCache", (void **)&tf4->tfile4skipCache, &rc ) ;
   c4getFuncPtr( c4->hInst, "tfile4top", (void **)&tf4->tfile4top, &rc ) ;

   return rc ;
}

static int t4setupFunctions( CODE4 *c4, TAG4 *t4 )
{
   long rc = 0 ;

   c4getFuncPtr( c4->hInst, "t4alias", (void **)&t4->t4alias, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4close", (void **)&t4->t4close, &rc ) ;
   //c4getFuncPtr( c4->hInst, "t4count", (void **)&t4->t4count, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4descending", (void **)&t4->t4descending, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4exprCB", (void **)&t4->t4exprCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4getExprSource", (void **)&t4->t4getExprSource, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4getTagFile", (void **)&t4->t4getTagFile, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4filterCB", (void **)&t4->t4filterCB, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4readBuffer", (void **)&t4->t4readBuffer, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4seekN", (void **)&t4->t4seekN, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4tagInfo", (void **)&t4->t4tagInfo, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4unique", (void **)&t4->t4unique, &rc ) ;
   c4getFuncPtr( c4->hInst, "t4uniqueSetVB", (void **)&t4->t4uniqueSetVB, &rc ) ;

   c4getFuncPtr( c4->hInst, "i4tagRemove", (void **)&t4->i4tagRemove, &rc ) ;

   return rc ;
}

int S4FUNCTION tfile4bottom( TAG4FILE S4PTR *tf4)
{
   return (int)tf4->tfile4bottom( tf4->tf4) ;
}

long S4FUNCTION tfile4count( TAG4FILE S4PTR *tf4)
{
   return (long)tf4->tfile4count( tf4->tf4) ;
}

int S4FUNCTION tfile4eof( TAG4FILE S4PTR *tf4)
{
   return (int)tf4->tfile4eof( tf4->tf4) ;
}

char *S4FUNCTION tfile4key( TAG4FILE S4PTR *tf4)
{
   return (char*)tf4->tfile4key( tf4->tf4) ;
}

int S4FUNCTION tfile4keyLenExport( TAG4FILE S4PTR *tf4)
{
   return (int)tf4->tfile4keyLenExport( tf4->tf4) ;
}

double S4FUNCTION tfile4position( TAG4FILE S4PTR *tf4)
{
   return (double)tf4->tfile4position( tf4->tf4) ;
}

int S4FUNCTION tfile4positionSet( TAG4FILE S4PTR *tf4, const double ipos)
{
   return (int)tf4->tfile4positionSet( tf4->tf4, ipos) ;
}

int S4FUNCTION tfile4go( TAG4FILE S4PTR *tf4, const unsigned char *ptr, const unsigned long recNum, const int goAdd)
{
   return (int)tf4->tfile4go( tf4->tf4, ptr, recNum, goAdd) ;
}

long S4FUNCTION tfile4recNo( TAG4FILE S4PTR *tf4)
{
   return (long)tf4->tfile4recNo( tf4->tf4) ;
}

int S4FUNCTION tfile4seek( TAG4FILE S4PTR *tf4, const void *ptr, const int lenPtrIn )
{
   return (int)tf4->tfile4seek( tf4->tf4, ptr, lenPtrIn ) ;
}

int S4FUNCTION tfile4skip( TAG4FILE S4PTR *tf4, long numSkip )
{
   return (int)tf4->tfile4skip( tf4->tf4, numSkip ) ;
}

int S4FUNCTION tfile4skipCache( TAG4FILE S4PTR *tf4, long numRowsin )
{
   return (int)tf4->tfile4skipCache( tf4->tf4, numRowsin ) ;
}

int S4FUNCTION tfile4top( TAG4FILE S4PTR *tf4)
{
   return (int)tf4->tfile4top( tf4->tf4) ;
}

char S4PTR *S4FUNCTION t4alias( TAG4 S4PTR *t4 )
{
   return (char *)t4->t4alias( t4->t4 ) ;
}

int S4FUNCTION t4close( TAG4 S4PTR *t4 )
{
   int rc = 0 ;

   l4remove( &t4->d4->t4list, t4 ) ;
   rc = t4->t4close( t4->t4 ) ;
   if ( rc != 0 )
      return rc ;

   free( t4 ) ;

   return rc ;
}

/*long S4FUNCTION t4count( TAG4 *t4 )
{
   return t4->t4count( t4->t4 ) ;
}*/

short S4FUNCTION t4descending( TAG4 *t4 )
{
   return t4->t4descending( t4->t4 ) ;
}

const char S4PTR *S4FUNCTION t4exprCB( TAG4 S4PTR *t4 )
{
   return (const char*) t4->t4exprCB( t4->t4 ) ;
}

const char S4PTR *S4FUNCTION t4getExprSource( TAG4 S4PTR *t4 )
{
   return (const char*) t4->t4getExprSource( t4->t4 ) ;
}

TAG4FILE S4PTR *S4FUNCTION t4getTagFile( TAG4 S4PTR *t4 )
{
   TAG4FILE *tf4 ;
   long rc ;
   void *tempPtr;

   tempPtr = t4->t4getTagFile( t4->t4 ) ;
   if ( tempPtr == 0 )
      return 0 ;

   if ( t4->d4->numTF4 > 0 )
      for ( tf4 = (TAG4FILE*) l4first( &t4->d4->tf4list ) ; tf4 != 0 ; tf4 = (TAG4FILE*) l4next( &t4->d4->tf4list, tf4 ) )
         if ( tf4->tf4 == tempPtr )
            return tf4 ;

   tf4 = (TAG4FILE*) malloc( sizeof(TAG4FILE) ) ;
   if ( tf4 == 0 )
      return 0 ;
   memset( tf4, 0, sizeof(TAG4FILE) ) ;

   rc = tfile4setupFunctions( t4->d4->c4, tf4 ) ;
   if ( rc != 0 )
   {
      free( tf4 ) ;
      return 0 ;
   }

   t4->d4->c4->l4add( &t4->d4->tf4list, tf4 ) ;
   t4->d4->numTF4++ ;
   tf4->tf4 = tempPtr;

   return tf4 ;
}

const char S4PTR *S4FUNCTION t4filterCB( TAG4 S4PTR *t4 )
{
   return (const char*) t4->t4filterCB( t4->t4 ) ;
}

TAG4 *S4FUNCTION t4openCB(DATA4 S4PTR *d4, char S4PTR *name )
{
   TAG4 *t4 ;
   long rc ;

   t4 = (TAG4*) malloc( sizeof(TAG4) ) ;
   if ( t4 == 0 )
      return 0 ;
   memset( t4, 0, sizeof(TAG4) ) ;

   c4getFuncPtr( d4->c4->hInst, "t4openCB", (void **)&t4->t4openCB, &rc ) ;

   t4->t4 = t4->t4openCB( d4->d4, name ) ;
   if ( t4->t4 == 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   rc = t4setupFunctions( d4->c4, t4 ) ;
   if ( rc != 0 )
   {
      free( t4 ) ;
      return 0 ;
   }

   d4->c4->l4add( &d4->t4list, t4 ) ;
   d4->numT4++ ;
   t4->d4 = d4 ;
   memset( t4->alias, 0, sizeof(t4->alias) ) ;
   strcpy( t4->alias, (const char*)t4->t4alias( t4->t4 ) ) ;

   return t4 ;
}

long S4FUNCTION t4readBuffer( TAG4 S4PTR *t4, long numRecsToBuf )
{
   return (long) t4->t4readBuffer( t4->t4, numRecsToBuf ) ;
}

int S4FUNCTION t4seekN( TAG4 S4PTR *t4, const char S4PTR *seekValue, const short inputKeyLen, short doDataPosition )
{
   return t4->t4seekN( t4->t4, seekValue, inputKeyLen, doDataPosition ) ;
}

TAG4INFO S4PTR * S4FUNCTION t4tagInfo( TAG4 S4PTR *t4 )
{
   return (TAG4INFO *)t4->t4tagInfo( t4->t4 ) ;
}

short int S4FUNCTION t4unique( const TAG4 S4PTR *t4 )
{
   return t4->t4unique( t4->t4 ) ;
}

short S4FUNCTION t4uniqueSetVB( TAG4 S4PTR *t4, const short val )
{
   return t4->t4uniqueSetVB( t4->t4, val ) ;
}

void S4PTR *S4FUNCTION u4allocDefault( long numBytes )
{
   return g_c4->u4allocDefault( numBytes ) ;
}

int S4FUNCTION u4allocAgainDefault( CODE4 S4PTR *c4, char S4PTR * S4PTR *ptrPtr, unsigned int S4PTR *lenPtr, const unsigned int newLen )
{
   return c4->u4allocAgainDefault( c4->c4, ptrPtr, lenPtr, newLen ) ;
}

void S4PTR *S4FUNCTION u4allocErDefault( CODE4 S4PTR *c4, long len )
{
   return c4->u4allocErDefault( c4->c4, len ) ;
}

void S4PTR *S4FUNCTION u4allocFreeDefault( CODE4 S4PTR *c4, long len )
{
   return c4->u4allocFreeDefault( c4->c4, len ) ;
}

int S4FUNCTION u4freeDefault( void S4PTR *ptr )
{
   return g_c4->u4freeDefault( ptr ) ;
}

void S4FUNCTION u4memCpy( char *dest, char *source, long len)
{
   g_c4->u4memCpy( dest, source, len ) ;
}

int S4FUNCTION u4nameChar( unsigned char ch)
{
   return g_c4->u4nameChar( ch ) ;
}

int S4FUNCTION u4nameExt( char S4PTR *name, int lenMax, const char S4PTR *ext, const int doReplace )
{
   return g_c4->u4nameExt( name, lenMax, ext, doReplace ) ;
}

int S4FUNCTION u4namePiece( char S4PTR *result, const unsigned int lenResult, const char S4PTR *name, const int givePath, const int giveExt )
{
   return g_c4->u4namePiece( result, lenResult, name, givePath, giveExt ) ;
}

int S4FUNCTION u4remove( const char S4PTR *ptr ) /* used for testing */
{
   return g_c4->u4remove( ptr ) ;
}

unsigned long S4FUNCTION u4ncpy( char S4PTR *to, const char S4PTR *from, const unsigned int l )
{
   return g_c4->u4ncpy( to, from, l ) ;
}

void S4FUNCTION u4yymmdd( char S4PTR *result )
{
   g_c4->u4yymmdd( result ) ;
}

long S4FUNCTION v4Cstring( char S4PTR *string )
{
   return g_c4->v4Cstring( string ) ;
}

void S4FUNCTION v4Cstringfree( char S4PTR *string )
{
   g_c4->v4Cstringfree( string ) ;
}

#endif   /* D4DLL_INC */
