/* d4dll.h   (c)Copyright Sequiter Software Inc., 2001.  All rights reserved. */

/*
   This adds support for dual dlls (both stand alone and client server simultaneous)
   It gets included instead of d4all.h by the application

   Supported only under Windows 32-bit as a static library
*/

#include <windows.h>
#define S4STAND_ALONE
#define S4WIN32
#define S4FOX

#define S4COMPRESS_ZLIB
//#define S4COMPRESS_QUICKLZ

#if defined(_MSC_VER) && !defined(S4WINCE) && !defined(S4WIN64)
   #if _MSC_VER >= 900
      #pragma pack(push,1)
   #else
      #pragma pack(1)
   #endif
#else
   #ifdef __BORLANDC__
      #pragma pack(1)
   #endif
#endif

#include "d4defs.h"

#define S4VERSION 6503013

#ifndef S4FUNCTION2
   #define S4FUNCTION2 __stdcall
#endif

// AS Feb 22/06 - support for new functionality...
typedef short (CALLBACK* QUERY_CALLBACK)(long) ;
typedef const char * (CALLBACK* QUERY_SET_CALLBACK)(struct RELATE4St *, const char *) ;

typedef int S4CALL S4CMP_FUNCTION( S4CMP_PARM, S4CMP_PARM, size_t) ;
typedef int (S4CALL *S4CMP_FUNCTION_PTR)( S4CMP_PARM, S4CMP_PARM, size_t ) ;

// The specification for the name is:  <RETURN TYPE>4<PARAM1TYPE>_<PARAM2TYPE>...
// For example, a function of type :  char *function( ptr *, long ) becomes PTR4PTR_LONG
// note that for these functions all pointers get cast as void, so any pointer type == pointer
// use CPTR for a const pointer

typedef char S4FUNCTION2 CHAR4PTR( void S4PTR * ) ;
typedef char S4FUNCTION2 CHAR4PTR_CHAR( void S4PTR *, char ) ;
typedef short S4FUNCTION2 SHORT4CPTR( const void S4PTR * ) ;
typedef short S4FUNCTION2 SHORT4PTR( void S4PTR * ) ;
typedef short S4FUNCTION2 SHORT4PTR_SHORT( void S4PTR *, short ) ;
typedef short S4FUNCTION2 SHORT4PTR_CSHORT( void S4PTR *, const short ) ;
typedef short S4FUNCTION2 SHORT4PTR_CPTR( void S4PTR *, const void S4PTR * ) ;
typedef short S4FUNCTION2 SHORT4CPTR_CSHORT( const void S4PTR *, const short ) ;
typedef short S4FUNCTION2 SHORT4PTR_LONG( void S4PTR *, long ) ;
typedef short S4FUNCTION2 SHORT4PTR_PTR_SHORT( void S4PTR *, void S4PTR *, short ) ;
typedef short S4FUNCTION2 SHORT4PTR_PTR_LONG( void S4PTR *, void S4PTR *, long ) ;
typedef short S4FUNCTION2 SHORT4PTR_PTR_CPTR_CPTR_LONG( void S4PTR *, const void S4PTR *, const void S4PTR *, void S4PTR *, long ) ;
typedef short S4FUNCTION2 SHORT4PTR_CPTR_CSHORT( void S4PTR *, const void S4PTR *, const short ) ;
typedef short S4FUNCTION2 SHORT4PTR_SHORT_LONG_PTR_PTR_PTR( void S4PTR *, short, long, void S4PTR *, void S4PTR *, void S4PTR * ) ;
typedef unsigned short S4FUNCTION2 USHORT4PTR( void S4PTR * ) ;
typedef int S4FUNCTION2 INT4UCHAR( unsigned char ) ;
typedef int S4FUNCTION2 INT4PTR( void S4PTR * ) ;
typedef int S4FUNCTION2 INT4CPTR( const void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_INT( void S4PTR *, int ) ;
typedef int S4FUNCTION2 INT4PTR_LONG( void S4PTR *, long ) ;
typedef int S4FUNCTION2 INT4PTR_SHORT( void S4PTR *, short ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR( void S4PTR *, const void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_PTR( void S4PTR *, void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_QSCB( void S4PTR *, QUERY_SET_CALLBACK ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_LONG( void S4PTR *, void S4PTR *, long ) ;
typedef int S4FUNCTION2 INT4CPTR_PTR( const void S4PTR *, void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_CINT( void S4PTR *, const int ) ;
typedef int S4FUNCTION2 INT4PTR_CUINT( void S4PTR *, const unsigned int ) ;
typedef int S4FUNCTION2 INT4CPTR_CINT( const void S4PTR *, const int ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG( void S4PTR *, const long ) ;
typedef int S4FUNCTION2 INT4PTR_CULONG( void S4PTR *, const unsigned long ) ;
typedef int S4FUNCTION2 INT4PTR_CDBL( void S4PTR *, const double ) ;
typedef int S4FUNCTION2 INT4PTR_LONG_INT( void S4PTR *, long, int ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG_INT( void S4PTR *, const long, int ) ;
typedef int S4FUNCTION2 INT4PTR_INT_INT( void S4PTR *, int, int ) ;
typedef int S4FUNCTION2 INT4PTR_CINT_CLONG( void S4PTR *, const int, const long ) ;
typedef int S4FUNCTION2 INT4PTR_LONG_LONG( void S4PTR *, long, long ) ;
typedef unsigned int S4FUNCTION2 UINT4PTR_PTR_UINT( void S4PTR *, void S4PTR *, unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_CUINT( void S4PTR *, void S4PTR *, const unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR_CPTR( void S4PTR *, const void S4PTR *, const void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR_CINT( void S4PTR *, const void S4PTR *, const int ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR_CUINT( void S4PTR *, const void S4PTR *, const unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR_CUCHAR( void S4PTR *, const void S4PTR *, const unsigned char ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR_CLONG_CINT( void S4PTR *, const void S4PTR *, long, int ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR_CSHORT_SHORT( void S4PTR *, const void S4PTR *, const short, short ) ;
typedef int S4FUNCTION2 INT4PTR_CPTR_CPTR_CPTR_CPTR_CPTR( void S4PTR*, const void S4PTR*, const void S4PTR*, const void S4PTR*, const void S4PTR*, const void S4PTR* ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG_CCHAR( void S4PTR *, const long, const char ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG_SHORT( void S4PTR *, const long, short ) ;
typedef int S4FUNCTION2 INT4DBL_PTR_INT_INT( double, void S4PTR *, int, int ) ;
typedef int S4FUNCTION2 INT4PTR_INT_CPTR_CINT( void S4PTR *, int, const void S4PTR *, const int ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG_CINT_CINT( void S4PTR *, const long, const int, const int ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG_PTR_CUINT( void S4PTR *, const long, void S4PTR*, const unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG_CPTR_CUINT( void S4PTR *, const long, const void S4PTR*, const unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_CLONG_CPTR_CPTR( void S4PTR *, const long, const void S4PTR *, const void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_CINT_CINT( void S4PTR *, void S4PTR *, const int, const int ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_CPTR_CINT( void S4PTR *, void S4PTR *, const void S4PTR *, const int ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_PTR_CUINT( void S4PTR *, void S4PTR *, void S4PTR *, const unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_CLONG_UCHAR( void S4PTR *, void S4PTR *, const long, unsigned char ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_PTR_PTR( void S4PTR *, void S4PTR *, void S4PTR *, void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_UINT_LONG_LONG( void S4PTR *, void S4PTR *, unsigned int, long, long ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_LONG_PTR_CUINT( void S4PTR *, void S4PTR *, long, void S4PTR *, const unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_PTR_CLONG_PTR_CUINT( void S4PTR *, void S4PTR *, const long, void S4PTR *, const unsigned int ) ;
typedef int S4FUNCTION2 INT4PTR_CUINT_CPTR_CINT_CINT( void S4PTR *, const unsigned int, const void S4PTR*, const int, const int ) ;
typedef int S4FUNCTION2 INT4PTR_CINT_CINT_CLONG_CPTR( void S4PTR *, const int, const int, const long, const void S4PTR* ) ;
typedef int S4FUNCTION2 INT4PTR_ULONG_LONG_ULONG_LONG( void S4PTR *, unsigned long, long, unsigned long, long ) ;
typedef int S4FUNCTION2 INT4PTR_CINT_CLONG_CPTR_CPTR_CPTR( void S4PTR *, const unsigned int, const long, const void S4PTR *, const void S4PTR *, const void S4PTR * ) ;
typedef int S4FUNCTION2 INT4PTR_SHORT_SHORT_SHORT( void S4PTR *, short, short, short ) ;
typedef int S4FUNCTION2 INT4PTR_LONGLONG( void S4PTR *, LONGLONG ) ;

typedef long S4FUNCTION2 LONG4PTR( void S4PTR * ) ;
typedef long S4FUNCTION2 LONG4CPTR( const void S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
typedef long S4FUNCTION2 LONGLONG4PTR( void S4PTR * ) ;
typedef float S4FUNCTION2 FLOAT4CPTR( const void S4PTR * ) ;
typedef double S4FUNCTION2 DBL4PTR( void S4PTR * ) ;
typedef double S4FUNCTION2 DBL4CPTR( const void S4PTR * ) ;
typedef double S4FUNCTION2 DBL4CPTR_CINT( const void S4PTR *, const int ) ;
typedef long S4FUNCTION2 LONG4PTR_CDBL( void S4PTR *, const double ) ;
typedef long S4FUNCTION2 LONG4PTR_UCHAR( void S4PTR *, unsigned char ) ;
typedef long S4FUNCTION2 LONG4PTR_LONG( void S4PTR *, long ) ;
typedef long S4FUNCTION2 LONG4PTR_LONG_SHORT( void S4PTR *, long, short ) ;
typedef long S4FUNCTION2 LONG4PTR_CLONG_INT( void S4PTR *, const long, int ) ;
typedef long S4FUNCTION2 LONG4CPTR_CINT( const void S4PTR *, const int ) ;
typedef unsigned int S4FUNCTION2 UINT4PTR( void S4PTR * ) ;
typedef unsigned int S4FUNCTION2 UINT4PTR_CLONG_PTR_CUINT( void S4PTR *, const long, void S4PTR *, const unsigned int ) ;
typedef unsigned int S4FUNCTION2 UINT4PTR_PTR_UINT_UINT( void S4PTR *, void S4PTR *, unsigned int, unsigned int ) ;
typedef unsigned long S4FUNCTION2 ULONG4PTR( void S4PTR * ) ;
typedef unsigned long S4FUNCTION2 ULONG4CPTR( const void S4PTR * ) ;
typedef unsigned long S4FUNCTION2 ULONG4PTR_PTR_CUINT( void S4PTR *, void S4PTR *, const unsigned int ) ;
typedef unsigned long S4FUNCTION2 ULONG4PTR_CPTR_CUINT( void S4PTR *, const void S4PTR *, const unsigned int ) ;

typedef void S4PTR *S4FUNCTION2 PTR4INT_PTR_LONG( int, void S4PTR *, long ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_PTR( void S4PTR *, void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_INT( void S4PTR *, int ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_LONG( void S4PTR *, long ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_CPTR( void S4PTR *, const void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4CPTR_CPTR( const void S4PTR *, const void S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
typedef void S4PTR *S4FUNCTION2 PTR4PTR_CPTR_SHORT( void S4PTR *, const void S4PTR *, short ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_CPTR_CPTR_CPTR( void S4PTR *, const void S4PTR *, const void S4PTR *, const void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_PTR_CPTR( void S4PTR *, void S4PTR *, const void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_CPTR_PTR( void S4PTR *, const void S4PTR *, void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_CPTR_CPTR( void S4PTR *, const void S4PTR *, const void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR( void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4CPTR( const void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4LONG( long ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_SHORT( void S4PTR *, short ) ;
typedef void S4PTR *S4FUNCTION2 PTR4CPTR_SHORT( const void S4PTR *, short ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_SHORT_PTR( void S4PTR *, short, void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_PTR_PTR( void S4PTR *, void S4PTR *, void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR4PTR_PTR_CPTR_PTR( void S4PTR *, void S4PTR *, const void S4PTR *, void S4PTR * ) ;
typedef void S4PTR *S4FUNCTION2 PTR_PTR_INT_CUINT_INT_CINT( void S4PTR *, int, const unsigned int, int, const int ) ;

typedef const void S4PTR *S4FUNCTION2 CPTR4PTR( void S4PTR * ) ;
typedef const void S4PTR *S4FUNCTION2 CPTR4PTR_CLONG( void S4PTR *, const long ) ;

typedef void S4FUNCTION2 VOID4PTR_CHAR( void S4PTR *, char ) ;
typedef void S4FUNCTION2 VOID4PTR( void S4PTR * ) ;
typedef void S4FUNCTION2 VOID4CPTR( const void S4PTR * ) ;
typedef void S4FUNCTION2 VOID4PTR_LONG( void S4PTR *, long ) ;
typedef void S4FUNCTION2 VOID4PTR_CLONG( void S4PTR *, const long ) ;
// AS Feb 22/06 - support for new functionality...
typedef void S4FUNCTION2 VOID4PTR_CLONGLONG( void S4PTR *, const LONGLONG ) ;
typedef void S4FUNCTION2 VOID4PTR_INT( void S4PTR *, int ) ;
typedef void S4FUNCTION2 VOID4PTR_CINT( void S4PTR *, const int ) ;
typedef void S4FUNCTION2 VOID4PTR_SHORT( void S4PTR *, short ) ;
typedef void S4FUNCTION2 VOID4PTR_CSHORT( void S4PTR *, const short ) ;
typedef void S4FUNCTION2 VOID4PTR_DBL( void S4PTR *, double ) ;
typedef void S4FUNCTION2 VOID4PTR_PTR( void S4PTR *, void S4PTR * ) ;
typedef void S4FUNCTION2 VOID4PTR_CMPFUNCTION( void S4PTR *, S4CMP_FUNCTION ) ;
typedef void S4FUNCTION2 VOID4PTR_PTR_PTR( void S4PTR *, void S4PTR *, void S4PTR * ) ;
typedef void S4FUNCTION2 VOID4PTR_CPTR( void S4PTR *, const void S4PTR * ) ;
typedef void S4FUNCTION2 VOID4LONG_PTR_INT( long , void S4PTR *, int ) ;
typedef void S4FUNCTION2 VOID4PTR_PTR_LONG( void S4PTR *, void S4PTR *, long ) ;
typedef void S4FUNCTION2 VOID4PTR_CPTR_SHORT( void S4PTR *, const void S4PTR *, short ) ;
typedef void S4FUNCTION2 VOID4PTR_CPTR_CUINT( void S4PTR *, const void S4PTR *, const unsigned int ) ;
typedef void S4FUNCTION2 VOID4CPTR_PTR_PTR( const void S4PTR *, void S4PTR *, void S4PTR * ) ;
typedef void S4FUNCTION2 VOID4PTR_CPTR_PTR( void S4PTR *, const void S4PTR *, void S4PTR * ) ;
typedef void S4FUNCTION2 VOID4PTR_CPTR_PTR_CPTR( void S4PTR *, const void S4PTR *, void S4PTR *, const void S4PTR * ) ;

typedef const void S4PTR *S4FUNCTION2 CPTR4CPTR( const void S4PTR * ) ;

#define DEF4SERVER_ID "localhost"
#define DEF4PROCESS_ID "23165"
typedef short (CALLBACK* REINDEX_CALLBACK)(double);

#define relate4blank 105
#define relate4skipRec 106
#define relate4terminate 107
#define relate4exact 108
#define relate4scan 109
#define relate4approx 110
#define relate4skip_rec relate4skipRec

#define OPT4DBF    1
#define OPT4INDEX  2
#define OPT4OTHER  3

enum index4format
{
   r4cdx =     200,
   r4mdx =     201,
   r4ntx =     202,
   r4unknown = 204

   /* r4ndx = 203  - not used anymore */
} ;

enum Lock4type
{
   /* AS this should be ok with Borland because it is not included in any shared
      or exported structures */
   lock4read = 0,
   lock4write = 1,
   lock4any = 2
} ;

typedef struct S4EXPORT l4linkSt
{
   struct l4linkSt S4PTR *n, S4PTR *p ;
} LINK4 ;

typedef struct
{
   S4CONV( LINK4 S4PTR *lastNode, LINK4 S4PTR *last_node ) ; /* the last Link */
   LINK4 S4PTR *selected ;
   /* AS 04/29/99 --> changed link list to handle more links (from 64k to 4 gig) */
   S4CONV( unsigned long nLink, unsigned long n_link ) ; /* The number of links in the list */
} LIST4 ;

typedef struct  /* Creating Data File */
{
   char *name ;
   short int type ;
   unsigned short int len ;
   unsigned short int dec ;
   unsigned short int nulls ;  /* are nulls allowed? */
} FIELD4INFO ;

typedef struct
{
   char S4PTR  *name ;
   S4CONST char S4PTR *expression ;
   S4CONST char S4PTR *filter ;
   short int unique ;
   unsigned short int descending ;
} TAG4INFO ;

#ifdef D4DLL_CPP
   class S4CLASS CODE4
   {
   public :
#else
   typedef struct
   {
#endif
   LINK4 link ;

   void *c4 ;   // pointer to the actual CODE4 structure from the dll
   HINSTANCE hInst ;

   LIST4 d4list ; /* list of DATA4s produced by d4open/d4create for code4data() */
   int numD4 ; /* number of DATA4s in d4list */
   HANDLE hD4List ;
   // AS Jan 27/06 -  merged from other project...must not be called closeAll which conflicts for C++ with function name
   char closeAllList ; /* non-zero if closing DATA4s using code4close/code4initUndo */

   SHORT4PTR_SHORT *code4accessMode ;
   USHORT4PTR *code4actionCode ;
   PTR4INT_PTR_LONG *code4alloc ;
   VOID4PTR_DBL *code4autoIncrementStart ;
   SHORT4PTR_SHORT *code4autoOpen ;
   PTR4PTR_PTR_CPTR *code4calcCreate ;
   VOID4PTR *code4calcReset ;
   SHORT4PTR_SHORT *code4codePage ;
   SHORT4PTR_SHORT *code4collate ;
   SHORT4PTR_SHORT *code4collateUnicode ;
   SHORT4PTR_SHORT *code4collatingSequence ;
   SHORT4PTR_SHORT *code4compatibility ;
   CHAR4PTR_CHAR *code4compressedMemos ;
   INT4PTR_SHORT *code4compress ;
   INT4PTR_SHORT_SHORT_SHORT *code4compressConfigure ;
   INT4PTR_SHORT_SHORT_SHORT *code4compressConfigureServer ;
   SHORT4PTR_SHORT *code4createTemp ;
   INT4PTR *code4close ;
   INT4PTR_CPTR_CPTR_CPTR_CPTR_CPTR *code4connect ;
   PTR4PTR *code4dateFormat ;
   INT4PTR_CPTR *code4dateFormatSet ;
   INT4PTR_INT_INT *code4encryptConnection ;
   VOID4PTR_SHORT *code4encryptFile ;
   SHORT4PTR_SHORT *code4errCreate ;
   SHORT4PTR_SHORT *code4errDefaultUnique ;
   SHORT4PTR_SHORT *code4errExpr ;
   SHORT4PTR_SHORT *code4errFieldName ;
   SHORT4PTR_SHORT *code4errGo ;
   SHORT4PTR_SHORT *code4errOff ;
   SHORT4PTR_SHORT *code4errOpen ;
   SHORT4PTR_SHORT *code4errorCode ;
   SHORT4PTR_SHORT *code4errRelate ;
   SHORT4PTR_SHORT *code4errSkip ;
   SHORT4PTR_SHORT *code4errTagName ;
   VOID4PTR *code4exit ;
   SHORT4PTR_SHORT *code4fileFlush ;
   INT4PTR *code4flush ;
   LONG4PTR_LONG *code4hInst ;
   LONG4PTR_LONG *code4hWnd ;
   SHORT4PTR *code4indexBlockSize ;
   SHORT4PTR_SHORT *code4indexBlockSizeSet ;
   INT4PTR *code4indexFormat ;
   CPTR4PTR *code4indexExtension ;
   INT4PTR *code4initUndo ;
   VOID4PTR *code4largeOn ;
   INT4PTR *code4lock ;
   SHORT4PTR_SHORT *code4lockAttempts ;
   SHORT4PTR_SHORT *code4lockAttemptsSingle ;
   VOID4PTR *code4lockClear ;
   SHORT4PTR_LONG *code4lockDelay ;
   SHORT4PTR_SHORT *code4lockEnforce ;
   CPTR4PTR *code4lockFileName ;
   LONG4PTR *code4lockItem ;
   CPTR4PTR *code4lockNetworkId ;
   CPTR4PTR *code4lockUserId ;
   SHORT4PTR_SHORT *code4log ;
   INT4PTR_CPTR_CPTR *code4logCreate ;
   CPTR4PTR *code4logFileName ;
   INT4PTR_CPTR_CPTR *code4logOpen ;
   VOID4PTR *code4logOpenOff ;
   SHORT4PTR_SHORT *code4memExpandBlock ;
   SHORT4PTR_SHORT *code4memExpandData ;
   SHORT4PTR_SHORT *code4memExpandIndex ;
   SHORT4PTR_SHORT *code4memExpandLock ;
   SHORT4PTR_SHORT *code4memExpandTag ;
   INT4PTR_SHORT *code4memoCompress ;
   LONG4PTR_LONG *code4memSizeBlock ;
   LONG4PTR_LONG *code4memSizeBuffer ;
   SHORT4PTR_SHORT *code4memSizeMemo ;
   LONG4PTR_LONG *code4memSizeMemoExpr ;
   LONG4PTR_LONG *code4memSizeSortBuffer ;
   LONG4PTR_LONG *code4memSizeSortPool ;
   SHORT4PTR_SHORT *code4memStartBlock ;
   SHORT4PTR_SHORT *code4memStartData ;
   SHORT4PTR_SHORT *code4memStartIndex ;
   SHORT4PTR_SHORT *code4memStartLock ;
   LONG4PTR_LONG *code4memStartMax ;
   SHORT4PTR_SHORT *code4memStartTag ;
   SHORT4PTR_SHORT *code4optimize ;
   SHORT4PTR_SHORT *code4optimizeWrite ;
   INT4PTR *code4optAll ;
   INT4PTR *code4optStart ;
   INT4PTR *code4optSuspend ;
   INT4PTR *code4ping ;
   SHORT4PTR_SHORT *code4readLock ;
   SHORT4PTR_SHORT *code4readOnly ;
   SHORT4PTR_SHORT *code4safety ;
   SHORT4PTR_SHORT *code4singleOpen ;
   LONG4PTR *code4timeout ;
   VOID4PTR_LONG *code4timeoutSet ;
   INT4PTR *code4tranCommit ;
   INT4PTR *code4tranRollback ;
   INT4PTR *code4tranStart ;
   SHORT4PTR *code4tranStatusCB ;
   INT4PTR *code4unlock ;
   INT4PTR *code4unlockAutoCB ;
   VOID4PTR_SHORT *code4unlockAutoSetCB ;
   SHORT4PTR_SHORT *code4useGeneralTagsInRelate ;
   VOID4PTR_CPTR *code4verifySet ;
   INT4PTR_CPTR_CUCHAR *code4validate ;

   INT4CPTR *c4getAccessMode ;
   INT4CPTR *c4getAutoOpen ;
   INT4CPTR *c4getCodePage ;
   SHORT4CPTR *c4getCompatibility ;
   CHAR4PTR *c4getCompressedMemos ;
   INT4CPTR *c4getCreateTemp ;
   INT4CPTR *c4getErrCreate ;
   INT4CPTR *c4getErrDefaultUnique ;
   INT4CPTR *c4getErrExpr ;
   INT4CPTR *c4getErrFieldName ;
   INT4CPTR *c4getErrGo ;
   INT4CPTR *c4getErrOff ;
   INT4CPTR *c4getErrOpen ;
   INT4CPTR *c4getErrorCode ;
   INT4CPTR *c4getErrRelate ;
   INT4CPTR *c4getErrSkip ;
   INT4CPTR *c4getErrTagName ;
   INT4CPTR *c4getFileFlush ;
   INT4CPTR *c4getLockAttempts ;
   INT4CPTR *c4getLockAttemptsSingle ;
   INT4CPTR *c4getLockDelay ;
   INT4CPTR *c4getLockEnforce ;
   INT4CPTR *c4getLog ;
   INT4CPTR *c4getOptimize ;
   INT4CPTR *c4getOptimizeWrite ;
   INT4CPTR *c4getReadLockDo ;
   INT4CPTR *c4getReadOnlyDo ;
   INT4CPTR *c4getSafety ;
   INT4CPTR *c4getSingleOpen ;

   VOID4PTR_CHAR *c4setAccessMode ;
   VOID4PTR_LONG *c4setAcceptTimeOut ;
   VOID4PTR_INT *c4setAutoOpen ;
   VOID4PTR_INT *c4setCodePage ;
   VOID4PTR_SHORT *c4setCompatibility ;
   VOID4PTR_CHAR *c4setCompressedMemos ;
   VOID4PTR_INT *c4setCreateTemp ;
   VOID4PTR_INT *c4setErrCreate ;
   VOID4PTR_SHORT *c4setErrDefaultUnique ;
   VOID4PTR_INT *c4setErrExpr ;
   VOID4PTR_INT *c4setErrFieldName ;
   VOID4PTR_INT *c4setErrGo ;
   VOID4PTR_INT *c4setErrOff ;
   VOID4PTR_INT *c4setErrOpen ;
   VOID4PTR_INT *c4setErrorCode ;
   VOID4PTR_INT *c4setErrRelate ;
   VOID4PTR_INT *c4setErrSkip ;
   VOID4PTR_INT *c4setErrTagName ;
   VOID4PTR_INT *c4setFileFlush ;
   VOID4PTR_INT *c4setLockAttempts ;
   VOID4PTR_INT *c4setLockAttemptsSingle ;
   VOID4PTR_INT *c4setLockDelay ;
   VOID4PTR_INT *c4setLockEnforce ;
   VOID4PTR_INT *c4setLog ;
   VOID4PTR_INT *c4setOptimize ;
   VOID4PTR_INT *c4setOptimizeWrite ;
   VOID4PTR_CHAR *c4setReadLockDo ;
   VOID4PTR_INT *c4setReadOnlyDo ;
   VOID4PTR_CHAR *c4setSafety ;
   VOID4PTR_SHORT *c4setSingleOpen ;

   DBL4CPTR_CINT *c4atod ;
   INT4CPTR_CINT *c4atoi ;
   LONG4CPTR_CINT *c4atol ;
   INT4DBL_PTR_INT_INT *c4dtoa45 ;
   VOID4PTR_CPTR_PTR_CPTR *c4encode ;
   VOID4LONG_PTR_INT *c4ltoa45 ;
   VOID4PTR_INT *c4trimN ;
   VOID4PTR *c4upper ;

   // AS Jul 18/06 - need new function for c4strcpy...under Windows strcpy is becoming deprecated...
   #ifdef S4WINDOWS_VS5_PLUS
      #define c4strcpy( a, aLen, b ) strcpy_s( (a), (aLen), (b) )
      #define c4strncpy( a, aLen, b, c ) strncpy_s( (a), aLen, (b), (c) )
   #else
      #define c4strncpy( a, aLen, b, c ) strncpy( (a), (b), (c) )
      #define c4strcpy( a, aLen, b ) strcpy( (a), (b) )
   #endif

   INT4PTR_CLONG_INT *date4assignLow ;
   CPTR4CPTR *date4cdow ;
   CPTR4CPTR *date4cmonth ;
   INT4CPTR *date4dow ;
   VOID4CPTR_PTR_PTR *date4format ;
   DBL4PTR *date4formatMdx ;
   VOID4PTR_CPTR_PTR *date4init ;
   INT4CPTR *date4isLeap ;
   LONG4CPTR *date4long ;
   VOID4PTR *date4timeNow ;
   VOID4PTR *date4today ;

   VOID4PTR_PTR *error4callback ;
   INT4PTR_CINT_CLONG *error4default ;
   INT4PTR_CINT_CLONG_CPTR_CPTR_CPTR *error4describeDefault ;
   SHORT4PTR_SHORT_LONG_PTR_PTR_PTR *error4describeVB ;
   VOID4PTR *error4exitTest ;
   INT4PTR_CPTR_CINT *error4file ;
   CPTR4PTR *error4lastDescription ;
   INT4PTR_CINT *error4set ;
   CPTR4PTR_CLONG *error4text ;

   INT4PTR *file4alloc ;   /* allocate memory to internal FILE4 struct */

   VOID4PTR_PTR_PTR *l4addAfter ;
   VOID4PTR_PTR_PTR *l4addBefore ;
   PTR4CPTR_CPTR *l4prev ;
   PTR4PTR_PTR *l4remove ;

   PTR4PTR_INT *mem4allocDefault ;
   PTR_PTR_INT_CUINT_INT_CINT *mem4createDefault ;
   INT4PTR_PTR *mem4freeDefault ;
   VOID4PTR *mem4release ;

   INT4PTR *sort4alloc ;
   VOID4PTR *sort4free2 ;
   INT4PTR_PTR_CINT_CINT *sort4init ;

   PTR4LONG *u4allocDefault ;
   INT4PTR_PTR_PTR_CUINT *u4allocAgainDefault ;
   PTR4PTR_LONG *u4allocErDefault ;
   PTR4PTR_LONG *u4allocFreeDefault ;
   INT4PTR *u4freeDefault ;
   VOID4PTR_PTR_LONG *u4memCpy ;
   INT4UCHAR *u4nameChar ;
   INT4PTR_INT_CPTR_CINT *u4nameExt ;
   INT4PTR_CUINT_CPTR_CINT_CINT *u4namePiece ;
   INT4CPTR *u4remove ;
   ULONG4PTR_CPTR_CUINT *u4ncpy ;
   VOID4PTR *u4yymmdd ;

   LONG4PTR *v4Cstring ;
   VOID4PTR *v4Cstringfree ;
#ifdef D4DLL_CPP
   } ;
#else
   } CODE4 ;
#endif

#ifdef S4WIN32
typedef void (__stdcall *ERROR_CALLBACK)
#else
typedef void (*ERROR_CALLBACK)
#endif
(
   #ifdef S4CBPP
      CODE4 S4PTR *c4,
   #else
      struct CODE4St *c4,
   #endif
   short errCode,
   long errCode2,
   const char S4PTR *desc1,
   const char S4PTR *desc2,
   const char S4PTR *desc3
) ;  // CS 2001/03/26  add for error callback

typedef struct
{
   void *f4 ;
   CODE4 *code4 ; /* CODE4 used for file4create() */

   INT4PTR *file4close ;
   INT4PTR_PTR_CPTR_CINT *file4create ;
   INT4PTR *file4flush ;
   VOID4PTR *file4free ;
   ULONG4PTR *file4lenLow2 ;
   INT4PTR_LONG *file4lenSetLow2 ;
   INT4PTR_ULONG_LONG_ULONG_LONG *file4lockInternal ;
   CPTR4PTR *file4nameLow ;
   INT4PTR_PTR_CPTR_CINT *file4open ;
   INT4PTR_CINT_CINT_CLONG_CPTR *file4optimizeLow ;
   INT4PTR_CINT *file4optimizeWrite ;
   UINT4PTR_CLONG_PTR_CUINT *file4read ;
   INT4PTR_CLONG_PTR_CUINT *file4readAll ;
   INT4PTR *file4refresh ;
   INT4PTR_PTR *file4replace ;
   INT4PTR_ULONG_LONG_ULONG_LONG *file4unlockInternal ;
   INT4PTR_CLONG_CPTR_CUINT *file4write ;

   INT4PTR *file4seqReadAlloc ;
   VOID4PTR *file4seqReadFree ;
   INT4PTR_PTR_LONG_PTR_CUINT *file4seqReadInit ;

   INT4PTR *file4seqWriteAlloc ;
   VOID4PTR *file4seqWriteFree ;
   INT4PTR_PTR_CLONG_PTR_CUINT *file4seqWriteInit ;
} FILE4 ;

struct DATA4st ;

typedef struct
{
   LINK4 link ;

   void *i4 ;
   char name[LEN4PATH] ;

   struct DATA4st *d4 ;  /* pointer to DATA4 used by d4index/i4create/i4open */

   LIST4 t4list ; /* list of TAG4s obtained by i4tag */
   long numT4 ;   /* number of TAG4s in list t4list */

   INT4PTR *i4close ;
   PTR4PTR_CPTR_CPTR *i4create ;
   SHORT4PTR_PTR_CPTR_CPTR_LONG *i4createWithProgress ;
   CPTR4PTR *i4fileName ;
   PTR4PTR_CPTR *i4open ;
   INT4PTR *i4reindex ;
   PTR4PTR_CPTR *i4tag ;
   INT4PTR_CPTR *i4tagAdd ;
   PTR4PTR *i4tagInfo ;

} INDEX4 ;

typedef struct
{
   LINK4 link ;

   void *t4 ;
   char alias[LEN4PATH] ;
   struct DATA4st *d4 ; /* pointer to DATA4 used in t4open, d4tagNext, d4tagPrev */

   PTR4PTR *t4alias ;
   INT4PTR *t4close ;
   LONG4PTR *t4count ;
   SHORT4PTR *t4descending ;
   CPTR4PTR *t4exprCB ;
   CPTR4PTR *t4filterCB ;
   CPTR4PTR *t4getExprSource;
   PTR4PTR *t4getTagFile ;
   LONG4PTR_LONG *t4readBuffer;
   PTR4PTR_PTR *t4openCB ;
   INT4PTR_CPTR_CSHORT_SHORT *t4seekN ;
   PTR4PTR *t4tagInfo ;
   SHORT4CPTR *t4unique ;
   SHORT4PTR_CSHORT *t4uniqueSetVB ;

   INT4PTR *i4tagRemove ;
} TAG4 ;

typedef struct
{
   LINK4 link ;
   void *tf4;

   INT4PTR *tfile4bottom;
   LONG4PTR *tfile4count;
   INT4PTR *tfile4eof;
   INT4PTR_CPTR_CLONG_CINT *tfile4go;
   PTR4PTR *tfile4key;
   INT4PTR *tfile4keyLenExport;
   DBL4PTR *tfile4position;
   LONG4PTR_CDBL *tfile4positionSet;
   ULONG4PTR *tfile4recNo;
   INT4PTR_CPTR_CINT *tfile4seek;
   LONG4PTR_LONG *tfile4skip;
   INT4PTR_LONG *tfile4skipCache;
   INT4PTR *tfile4top;
} TAG4FILE;

typedef struct DATA4st
{
   LINK4 link ;

   CODE4 *c4 ;  /* pointer to CODE4 used by d4create/d4open */

   void *d4 ;
   char alias[LEN4PATH] ;

   LIST4 f4list ;  /* list of FIELD4s produced by d4field/d4fieldJ */
   long numF4 ;   /* current number of FIELD4s in list f4list */

   LIST4 i4list ;   /* list of INDEX4s produced by d4index */
   long numI4 ;   /* current number of INDEX4s in list i4list */

   LIST4 t4list ;     /* list of TAG4s produced by d4tag*** */
   long numT4 ;   /* current number of TAG4s in list t4list */

   LIST4 tf4list ;     /* list of TAG4FILEs produced by t4getTagFile */
   long numTF4 ;   /* current number of TAG4FILEs in list tf4list */

   CPTR4CPTR *d4alias ;
   VOID4PTR_CPTR *d4aliasSet ;
   INT4PTR *d4append ;
   INT4PTR *d4appendBlank ;
   SHORT4PTR_SHORT *d4appendStart ;
   VOID4PTR *d4blank ;
   INT4PTR *d4bof ;
   INT4PTR *d4bottom ;
   SHORT4PTR_SHORT *d4changed ;
   INT4PTR *d4check ;
   // AS Feb 22/06 - support for new functionality...
   PTR4PTR_CPTR_SHORT *d4compress ;
   INT4PTR_CPTR_CINT *d4copyTable ;
   PTR4PTR_CPTR_CPTR_CPTR *d4create ;
   INT4PTR *d4close ;
   VOID4PTR *d4delete ;
   INT4PTR *d4deleted ;
   INT4PTR *d4eof ;
   PTR4PTR_CPTR *d4field ;
   PTR4PTR *d4fieldInfo ;
   PTR4PTR_SHORT *d4fieldJ ;
   INT4PTR_CPTR *d4fieldNumber ;
   PTR4PTR_SHORT_PTR *d4fieldsAdd ;
   PTR4PTR_PTR_PTR *d4modifyStructure ;
   PTR4PTR_SHORT_PTR *d4fieldsRemove ;
   CPTR4PTR *d4fileName ;
   INT4PTR *d4flush ;
   INT4PTR *d4freeBlocks ;
   INT4PTR_CLONG_SHORT *d4goLow ;
   INT4PTR *d4goBof ;
   INT4PTR *d4goEof ;
   PTR4PTR_CPTR *d4index ;
   PTR4PTR *d4indexList ;
   INT4PTR_CLONG *d4lock ;
   INT4PTR_LONG *d4lockAdd ;
   INT4PTR *d4lockAddAll ;
   INT4PTR *d4lockAddAppend ;
   INT4PTR *d4lockAddFile ;
   INT4PTR *d4lockAll ;
   INT4PTR *d4lockAllVB ;
   INT4PTR *d4lockAppend ;
   INT4PTR *d4lockAppendVB ;
   INT4PTR *d4lockFile ;
   INT4PTR *d4lockFileVB ;
   INT4PTR_LONG_INT *d4lockTest ;
   INT4PTR_CLONG *d4lockVB ;
   INT4PTR_CINT *d4log ;
   SHORT4PTR *d4logStatusCB ;
   SHORT4PTR_SHORT *d4logVB ;
   INT4PTR *d4memoCompress ;
   SHORT4PTR *d4numFields ;
   PTR4PTR_CPTR *d4open ;
   PTR4PTR *d4openClone ;
   INT4PTR_CINT *d4optimize ;
   SHORT4PTR_SHORT *d4optimizeVB ;
   INT4PTR_CINT *d4optimizeWrite ;
   SHORT4PTR_SHORT *d4optimizeWriteVB ;
   INT4PTR *d4pack ;
   // AS Feb 22/06 - support for new functionality...
   SHORT4PTR_PTR_LONG *d4packWithProgress ;
   DBL4PTR *d4position ;
   LONG4PTR_CDBL *d4positionSet ;
   LONG4PTR_LONG_SHORT *d4readBuffer ;
   // AS Feb 22/06 - support for new functionality...
   INT4PTR_LONG *d4readBufferConfigure ;
   VOID4PTR *d4recall ;
   LONG4PTR_UCHAR *d4recCountDo2 ;
   LONG4PTR *d4recNoLow ;
   PTR4PTR *d4recordLow ;
   ULONG4PTR *d4recWidthLow ;
   LONG4PTR *d4recWidth_v ;
   INT4PTR *d4refresh ;
   INT4PTR *d4refreshRecord ;
   INT4PTR *d4reindex ;
   SHORT4PTR_PTR_LONG *d4reindexWithProgress ;
   INT4PTR *d4remove ;
   INT4PTR_CPTR *d4seek ;
   INT4PTR_CDBL *d4seekDouble ;
   SHORT4PTR_CPTR_CSHORT *d4seekN ;
   INT4PTR_CPTR *d4seekNext ;
   INT4PTR_CDBL *d4seekNextDouble ;
   SHORT4PTR_CPTR_CSHORT *d4seekNextN ;
        INT4PTR_LONGLONG *d4seekLongLong ;
        INT4PTR_LONGLONG *d4seekNextLongLong ;
   INT4PTR_CLONG *d4skip ;
   INT4PTR_CLONG *d4skipCache ;
   PTR4PTR_CPTR *d4tag ;
   PTR4PTR *d4tagDefault ;
   PTR4PTR_PTR *d4tagNext ;
   PTR4PTR_PTR *d4tagPrev ;
   VOID4PTR_PTR *d4tagSelect ;
   PTR4PTR *d4tagSelected ;
   INT4PTR_CPTR *d4tagSync ;
   INT4PTR *d4top ;
   INT4PTR *d4unlock ;
   // AS Feb 22/06 - support for new functionality...
   INT4PTR *d4unlockAppend ;
   CHAR4PTR *d4versionCB ;
   LONG4PTR *d4versionNumber ;
   LONG4PTR_LONG *d4writeBuffer ;
   INT4PTR_CLONG_CINT_CINT *d4writeLow ;
   SHORT4PTR_LONG *d4writeVB ;
   INT4PTR_LONG_LONG *d4zap ;

   INT4PTR *f4memoWriteFinish ;
} DATA4 ;

typedef struct
{
   LINK4 link ;

   void *f4 ;
   char name[11] ;
   int ordinal ;  /* 1st field, 2nd field, etc. */
   DATA4 *d4 ;  /* pointer to DATA4 used by d4field/d4fieldJ - for f4data() */

   INT4PTR *f4accessMode ;
   VOID4PTR_CPTR *f4assign ;
   VOID4PTR_CINT *f4assignChar ;
   VOID4PTR_SHORT *f4assignCharVB ;
   VOID4PTR_CPTR *f4assignCurrency ;
   VOID4PTR_CPTR *f4assignDateTime ;
   VOID4PTR_DBL *f4assignDouble ;
   VOID4PTR_CPTR *f4assignField ;
   VOID4PTR_CINT *f4assignInt ;
   VOID4PTR_CSHORT *f4assignIntVB ;
   VOID4PTR_CLONG *f4assignLong ;
   // AS Feb 22/06 - support for new functionality...
   VOID4PTR_CLONGLONG *f4assignLongLong ;
   VOID4PTR_CPTR_CUINT *f4assignN ;
   VOID4PTR_CPTR_SHORT *f4assignNVB ;
   VOID4PTR *f4assignNotNull ;
   VOID4PTR *f4assignNull ;
   PTR4PTR *f4assignPtr ;
   VOID4PTR_CPTR *f4assignUnicode ;
   VOID4PTR *f4blank ;
   INT4CPTR *f4char ;
   PTR4CPTR_SHORT *f4currency ;
   PTR4CPTR *f4dateTime ;
   INT4CPTR *f4decimals ;
   DBL4CPTR *f4double ;
   INT4CPTR_PTR *f4double2 ;
   // AS Feb 22/06 - support for new functionality...
   FLOAT4CPTR *f4float ;
   INT4CPTR *f4int ;
   INT4PTR *f4isMemo ;
   ULONG4CPTR *f4len ;
   SHORT4PTR *f4len_v ;
   INT4PTR *f4lockEnforce ;
   LONG4CPTR *f4long ;
   // AS Feb 22/06 - support for new functionality...
   LONGLONG4PTR *f4longLong ;
   INT4PTR_CPTR *f4memoAssign ;
   // AS Feb 22/06 - support for new functionality...
   INT4PTR_CPTR *f4memoAssignFile ;
   INT4PTR_CPTR *f4memoFile ;
   INT4PTR_CPTR_CUINT *f4memoAssignN ;
   SHORT4PTR_PTR_SHORT *f4memoAssignNVB ;
   SHORT4PTR_CPTR *f4memoAssignUnicode ;
   VOID4PTR_INT *f4memoChanged ;
   INT4PTR *f4memoFree ;
   ULONG4PTR *f4memoLen ;
   UINT4PTR *f4memoLenMax ;
   ULONG4PTR_PTR_CUINT *f4memoNcpy ;
   PTR4PTR *f4memoPtr ;
   UINT4PTR_PTR_UINT_UINT *f4memoReadPart ;
   INT4PTR_CUINT *f4memoSetLen ;
   CPTR4PTR *f4memoStr ;
   INT4PTR_PTR_UINT_LONG_LONG *f4memoWritePart ;
   CPTR4CPTR *f4name ;
   ULONG4PTR_PTR_CUINT *f4ncpy ;
   INT4CPTR *f4null ;
   INT4CPTR *f4nullable ;
   INT4CPTR *f4number ;
   PTR4CPTR *f4ptr ;
   PTR4PTR *f4str ;
   INT4CPTR *f4true ;
   INT4CPTR *f4type ;

} FIELD4 ;

typedef void S4PTR * EXPR4CALC ;

typedef struct
{
   void *e4 ;

   DATA4 *d4 ; /* pointer to DATA4 used by expr4parse */

   DBL4PTR *expr4double ;
   VOID4PTR *expr4freeCB ;
   LONG4PTR *expr4lenCB ;
   SHORT4CPTR_CSHORT *expr4nullLow ;
   PTR4PTR_PTR *expr4parseCB ;
   PTR4PTR_CPTR_PTR *expr4parseLow ;
   CPTR4CPTR *expr4source ;
   CPTR4PTR *expr4str ;
   INT4PTR *expr4true ;
   SHORT4PTR *expr4typeCB ;
   INT4PTR_PTR *expr4vary ;

} EXPR4 ;

struct RELATE4st ;

typedef struct RELATE4st
{
   LINK4 link ;

   void *r4 ;

   DATA4 *d4 ; /* pointer to DATA4 used in relate4init/relate4createSlave */
   TAG4 *t4 ;  /* pointer to slave TAG4 if this is slave produced by relate4createSlave */
   struct RELATE4st *masterR4 ;  /* pointer to master RELATE4 if this is slave */

   LIST4 slaveList ; /* list of slave RELATE4s */
   long numR4 ;   /* number of RELATE4s in list slaveList */

   INT4PTR *relate4bottom ;
   INT4PTR_PTR_LONG *relate4callbackInit ;
   INT4PTR *relate4callbackInitUndo ;
   INT4PTR_QSCB *relate4querySetCallbackInit ;
   INT4PTR *relate4querySetCallbackInitUndo ;
   INT4PTR *relate4changed ;
   ULONG4PTR *relate4count ;
   PTR4PTR_PTR_CPTR_PTR  *relate4createSlave ;
   INT4PTR *relate4doAll ;
   INT4PTR *relate4doOne ;
   INT4PTR *relate4eof ;
   SHORT4PTR_SHORT *relate4errorActionVB ;
   SHORT4PTR_SHORT *relate4freeVB ;
   PTR4PTR *relate4init ;
   INT4PTR *relate4lockAdd ;
   CPTR4PTR *relate4masterExprCB ;
   SHORT4PTR_SHORT *relate4matchLenVB ;
   INT4PTR *relate4next ;
   INT4PTR *relate4optimizeable ;
   INT4PTR_CPTR *relate4querySet ;
   LONG4PTR_LONG_SHORT *relate4readBuffer ;
   INT4PTR_INT *relate4retain ;
   INT4PTR_CLONG *relate4skip ;
   SHORT4PTR_SHORT *relate4skipEnableVB ;
   INT4PTR_LONG *relate4skipMaster ;
   INT4PTR_CPTR *relate4sortSet ;
   INT4PTR *relate4top ;
   SHORT4PTR_SHORT *relate4typeVB ;

} RELATE4 ;

typedef struct MEM4st
{
   LINK4 link ;

   LIST4 chunks ;      /* Chunks of pieces */
   LIST4 pieces ;      /* A list of available memory pieces */

   int unitStart;    /* The starting # of entries for the Memory Type */
   unsigned   unitSize ;   /* The size of each allocated piece */
   int unitExpand ; /* The expansion # of entries for the Memory Type */
   int nRepeat ;    /* The number of times entry returned for 'new' */
                    /* If nRepeat is '-1', it is a temporary entry. */
   int nUsed ;      /* The number of entries used */
   #ifdef S4DOS    /* no memory sharing under DOS, so can use a CODE4 for limited memory */
      #ifdef S4CBPP
         CODE4 S4PTR    *codeBase ;
      #else
         struct CODE4St *codeBase ;
      #endif
   #endif
}  MEM4 ;

typedef struct
{
   void *f4seqRead ;

   UINT4PTR_PTR_UINT *file4seqRead ;
   VOID4PTR *file4seqReadFree ;
   INT4PTR_PTR_CUINT *file4seqReadAll ;

} FILE4SEQ_READ ;

typedef struct
{
   void *f4seqWrite ;

   INT4PTR_CPTR_CUINT *file4seqWrite ;
   INT4PTR *file4seqWriteFlush ;
   VOID4PTR *file4seqWriteFree ;
   INT4PTR_CLONG_CCHAR *file4seqWriteRepeat ;

} FILE4SEQ_WRITE ;


typedef struct
{
   void *s4 ;

   VOID4PTR_CMPFUNCTION *sort4assignCmp2 ;
   INT4PTR *sort4free ;
   VOID4PTR *sort4free2 ;
   INT4PTR_PTR_PTR_PTR *sort4get ;
   INT4PTR *sort4getInit ;
   INT4PTR_CLONG_CPTR_CPTR *sort4put ;

} SORT4 ;

void S4FUNCTION c4getFuncPtr( HINSTANCE , const char * , void ** , long * ) ;

S4EXPORT short S4FUNCTION code4accessMode( CODE4 *cb, short value ) ;
S4EXPORT unsigned short S4FUNCTION code4actionCode( CODE4 S4PTR * ) ;
S4EXPORT CODE4 *S4FUNCTION code4allocDll( const char *dllName ) ;
S4EXPORT void S4FUNCTION code4autoIncrementStart( CODE4 S4PTR *, double ) ;
S4EXPORT short S4FUNCTION code4autoOpen( CODE4 *cb, short value ) ;
S4EXPORT EXPR4CALC S4FUNCTION code4calcCreate( CODE4 S4PTR *, EXPR4 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION code4calcReset( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4codePage( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4close( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4collate( CODE4 S4PTR *, short ) ;
S4EXPORT short S4FUNCTION code4collateUnicode( CODE4 S4PTR *c4, short collateType ) ;
S4EXPORT short S4FUNCTION code4collatingSequence( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4compatibility( CODE4 *cb, short value ) ;
S4EXPORT Bool5 S4FUNCTION code4compressedMemos( CODE4 *cb, Bool5 value ) ;
S4EXPORT int S4FUNCTION code4compress( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4compressConfigure( CODE4 *cb, short, short, short ) ;
S4EXPORT int S4FUNCTION code4compressConfigureServer( CODE4 *cb, short, short, short ) ;
S4EXPORT short S4FUNCTION code4createTemp( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4connect( CODE4 S4PTR *, const char S4PTR *, const char S4PTR *, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT DATA4 S4PTR *S4FUNCTION code4data( CODE4 S4PTR *, const char S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4dateFormat( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4dateFormatSet( CODE4 S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION code4encryptConnection( CODE4 *, int, int ) ;
S4EXPORT void S4FUNCTION code4encryptFile( CODE4 *, short ) ;
S4EXPORT short S4FUNCTION code4errCreate( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errDefaultUnique( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errExpr( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errFieldName( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errGo( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errOff( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errOffLow( void *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errOpen( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errorCode( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errRelate( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errSkip( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4errTagName( CODE4 *cb, short value ) ;
S4EXPORT void S4FUNCTION code4exit( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4fileFlush( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4flush( CODE4 S4PTR * ) ;
S4EXPORT long  S4FUNCTION code4hInst( CODE4 *cb, long value ) ;
S4EXPORT long  S4FUNCTION code4hWnd( CODE4 *cb, long value ) ;
S4EXPORT short S4FUNCTION code4indexBlockSize( CODE4 S4PTR *) ;
S4EXPORT short S4FUNCTION code4indexBlockSizeSet( CODE4 S4PTR *, short ) ;
S4EXPORT const char S4PTR * S4FUNCTION code4indexExtension( CODE4 S4PTR * ) ;
S4EXPORT enum index4format S4FUNCTION code4indexFormat( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4initUndo( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4initUndoLow( void * ) ;
S4EXPORT void S4FUNCTION code4largeOn( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4lock( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4lockAttempts( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4lockAttemptsSingle( CODE4 *cb, short value ) ;
S4EXPORT void S4FUNCTION code4lockClear( CODE4 S4PTR * ) ;
S4EXPORT long  S4FUNCTION code4lockDelay( CODE4 *cb, long value ) ;
S4EXPORT short S4FUNCTION code4lockEnforce( CODE4 *cb, short value ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4lockFileName( CODE4 S4PTR * ) ;
S4EXPORT long S4FUNCTION code4lockItem( CODE4 S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4lockNetworkId( CODE4 S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4lockUserId( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4log( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4logCreate( CODE4 S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4logFileName( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4logOpen( CODE4 S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION code4logOpenOff( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4memExpandBlock( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4memExpandData( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4memExpandIndex( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4memExpandLock( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4memExpandTag( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4memoCompress( CODE4 *c4, short flag ) ;
S4EXPORT long  S4FUNCTION code4memSizeBlock( CODE4 *cb, long value ) ;
S4EXPORT long  S4FUNCTION code4memSizeBuffer( CODE4 *cb, long value ) ;
S4EXPORT short S4FUNCTION code4memSizeMemo( CODE4 *cb, short value ) ;
S4EXPORT long  S4FUNCTION code4memSizeMemoExpr( CODE4 *cb, long value ) ;
S4EXPORT long  S4FUNCTION code4memSizeSortBuffer( CODE4 *cb, long value ) ;
S4EXPORT long  S4FUNCTION code4memSizeSortPool( CODE4 *cb, long value ) ;
S4EXPORT short S4FUNCTION code4memStartBlock( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4memStartData( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4memStartIndex( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4memStartLock( CODE4 *cb, short value ) ;
S4EXPORT long  S4FUNCTION code4memStartMax( CODE4 *cb, long value ) ;
S4EXPORT short S4FUNCTION code4memStartTag( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4optAll( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4optimize( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4optimizeWrite( CODE4 *cb, short value ) ;
S4EXPORT int S4FUNCTION code4optStart( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4optSuspend( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4ping( CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION code4readLock( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4readOnly( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4safety( CODE4 *cb, short value ) ;
S4EXPORT short S4FUNCTION code4singleOpen( CODE4 *cb, short value ) ;
S4EXPORT long S4FUNCTION code4timeout( CODE4 S4PTR * ) ;
S4EXPORT void S4FUNCTION code4timeoutSet( CODE4 S4PTR *, long ) ;
S4EXPORT int S4FUNCTION code4tranCommit( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4tranRollback( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4tranStart( CODE4 S4PTR * ) ;
#define code4tranStatus( c4 ) code4tranStatusCB( (c4) )
S4EXPORT short S4FUNCTION code4tranStatusCB( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4unlock( CODE4 S4PTR * ) ;
#define code4unlockAuto( c4 ) code4unlockAutoCB( (c4) )
S4EXPORT short S4FUNCTION code4unlockAutoCB( CODE4 * ) ;
#define code4unlockAutoSet( c4, val ) code4unlockAutoSetCB( (c4), (val) )
S4EXPORT void S4FUNCTION code4unlockAutoSetCB( CODE4 *, short ) ;
S4EXPORT short S4FUNCTION code4useGeneralTagsInRelate( CODE4 *, short ) ;
S4EXPORT int S4FUNCTION code4validate( CODE4 *, const char *, Bool5 ) ;
S4EXPORT void S4FUNCTION code4verifySet( CODE4 S4PTR *, const char S4PTR * ) ;

S4EXPORT int S4FUNCTION c4getAccessMode( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getAutoOpen( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getCodePage( const CODE4 S4PTR * ) ;
S4EXPORT short S4FUNCTION c4getCompatibility( const CODE4 S4PTR * ) ;
S4EXPORT Bool5 S4FUNCTION c4getCompressedMemos( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getCreateTemp( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrCreate( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrDefaultUnique( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrExpr( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrFieldName( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrGo( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrOff( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrOpen( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrorCode( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrRelate( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrSkip( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getErrTagName( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getFileFlush( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getLockAttempts( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getLockAttemptsSingle( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getLockDelay( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getLockEnforce( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getLog( const CODE4 S4PTR * ) ;
/*
Leave out the c4getMem functions unless it turns out they are required
*/

S4EXPORT int S4FUNCTION c4getOptimize( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getOptimizeWrite( const CODE4 S4PTR * ) ;
#define c4getReadLock( c4 )  c4getReadLockDo( (c4) )
S4EXPORT int S4FUNCTION c4getReadLockDo( const CODE4 S4PTR * ) ;
#define c4getReadOnly( c4 )  c4getReadOnlyDo( (c4) )
S4EXPORT int S4FUNCTION c4getReadOnlyDo( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getSafety( const CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION c4getSingleOpen( const CODE4 S4PTR * ) ;

S4EXPORT void S4FUNCTION c4setAccessMode( CODE4 S4PTR *, char ) ;
S4EXPORT void S4FUNCTION c4setAutoOpen( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setCodePage( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setCompatibility( CODE4 S4PTR *, short ) ;
S4EXPORT void S4FUNCTION c4setCompressedMemos( CODE4 S4PTR *, Bool5 ) ;
S4EXPORT void S4FUNCTION c4setCreateTemp( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrCreate( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrDefaultUnique( CODE4 S4PTR *, short ) ;
S4EXPORT void S4FUNCTION c4setErrExpr( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrFieldName( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrGo( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrOff( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrOpen( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrorCode( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrRelate( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrSkip( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setErrTagName( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setFileFlush( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setLockAttempts( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setLockAttemptsSingle( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setLockDelay( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setLockEnforce( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setLog( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setOptimize( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setOptimizeWrite( CODE4 S4PTR *, int ) ;
#define c4setReadLock( c4, val )      c4setReadLockDo( (c4), (val) )
S4EXPORT void S4FUNCTION c4setReadLockDo( CODE4 S4PTR *, char ) ;
S4EXPORT void S4FUNCTION c4setReadOnlyDo( CODE4 S4PTR *, int ) ;
S4EXPORT void S4FUNCTION c4setSafety( CODE4 S4PTR *, char ) ;
S4EXPORT void S4FUNCTION c4setSingleOpen( CODE4 S4PTR *, short ) ;

S4EXPORT double S4FUNCTION c4atod( const char S4PTR *, const int ) ;
S4EXPORT int S4FUNCTION c4atoi( const char S4PTR *, const int ) ;
S4EXPORT long S4FUNCTION c4atol( const char S4PTR *, const int ) ;
S4EXPORT int  S4FUNCTION c4dtoa45( double, char *, int, int ) ;
S4EXPORT void S4FUNCTION c4encode( char S4PTR *, const char S4PTR *, char S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION c4ltoa45( long, char *, int ) ;
S4EXPORT void S4FUNCTION c4trimN( char S4PTR *, int ) ;
#define c4stricmp( a, b ) stricmp( a, b ) /* for testing */
S4EXPORT void S4FUNCTION c4upper( char * ) ;

S4EXPORT S4CONST char S4PTR * S4FUNCTION d4alias( S4CONST DATA4 S4PTR * ) ;
S4EXPORT void S4FUNCTION d4aliasSet( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION d4append( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4appendBlank( DATA4 S4PTR * ) ;
S4EXPORT short S4FUNCTION d4appendStart( DATA4 S4PTR *, short ) ;
S4EXPORT void S4FUNCTION d4blank( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4bof( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4bottom( DATA4 S4PTR * ) ;
S4EXPORT short S4FUNCTION d4changed( DATA4 S4PTR *, short ) ;
S4EXPORT int S4FUNCTION d4check( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4close( DATA4 S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT DATA4 S4PTR * S4FUNCTION d4compress( DATA4 S4PTR *data, const char S4PTR *compressedName, short blockSize ) ;
S4EXPORT int S4FUNCTION d4copyTable( DATA4 S4PTR *, const char *, short ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4create( CODE4 S4PTR *, const char S4PTR *, const FIELD4INFO S4PTR *, const TAG4INFO S4PTR * ) ;
S4EXPORT void S4FUNCTION d4delete( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4deleted( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4eof( DATA4 S4PTR * ) ;
S4EXPORT FIELD4 S4PTR * S4FUNCTION d4field( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT FIELD4INFO S4PTR * S4FUNCTION d4fieldInfo( DATA4 S4PTR * ) ;
S4EXPORT FIELD4 S4PTR * S4FUNCTION d4fieldJ( DATA4 S4PTR *, short ) ;
S4EXPORT int S4FUNCTION d4fieldNumber( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4fieldsAdd( DATA4 S4PTR *, short, FIELD4INFO S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4modifyStructure( DATA4 S4PTR *, FIELD4INFO S4PTR *, TAG4INFO S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4fieldsRemove( DATA4 S4PTR *, short, char S4PTR * S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION d4fileName( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4flush( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4freeBlocks( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4goLow( DATA4 S4PTR *, const long, short ) ;
#define d4go( d4, recNo ) ( d4goLow( (d4), (recNo), 1 ) )
S4EXPORT int S4FUNCTION d4goBof( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4goEof( DATA4 S4PTR * ) ;
S4EXPORT INDEX4 S4PTR * S4FUNCTION d4index( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT LIST4 S4PTR * S4FUNCTION d4indexList( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lock( DATA4 S4PTR *, const long ) ;
S4EXPORT int S4FUNCTION d4lockAdd( DATA4 S4PTR *, long ) ;
S4EXPORT int S4FUNCTION d4lockAddAll( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockAddAppend( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockAddFile( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockAll( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockAllVB( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockAppend( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockAppendVB( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockFile( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockFileVB( DATA4 * ) ;
S4EXPORT int S4FUNCTION d4lockTest( DATA4 S4PTR *, const long, enum Lock4type lockType ) ;
S4EXPORT int S4FUNCTION d4lockVB( DATA4 *, const long ) ;
S4EXPORT int S4FUNCTION d4log( DATA4 S4PTR *, const int ) ;
#define d4logStatus( d4 ) ( d4logStatusCB( d4 ) )
S4EXPORT short S4FUNCTION d4logStatusCB( DATA4 * ) ;
S4EXPORT short S4FUNCTION d4logVB( DATA4 *d4, short logFlag ) ;
S4EXPORT int S4FUNCTION d4memoCompress( DATA4 S4PTR * ) ;
S4EXPORT short int S4FUNCTION d4numFields( DATA4 S4PTR * ) ;
S4EXPORT DATA4 S4PTR *S4FUNCTION d4open( CODE4 S4PTR *, const char S4PTR * ) ;
S4EXPORT DATA4 S4PTR *S4FUNCTION d4openClone( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4optimize( DATA4 S4PTR *, const int ) ;
S4EXPORT short S4FUNCTION d4optimizeVB( DATA4 *d4, short flag ) ;
S4EXPORT int S4FUNCTION d4optimizeWrite( DATA4 S4PTR *, const int ) ;
S4EXPORT short S4FUNCTION d4optimizeWriteVB( DATA4 *d4, short flag ) ;
S4EXPORT int S4FUNCTION d4pack( DATA4 S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT short S4FUNCTION d4packWithProgress( DATA4 S4PTR *data, REINDEX_CALLBACK callback, long milliseconds ) ;
S4EXPORT double S4FUNCTION d4position( DATA4 S4PTR * ) ;
S4EXPORT long S4FUNCTION d4positionSet( DATA4 S4PTR *, const double ) ;
S4EXPORT long S4FUNCTION d4readBuffer( DATA4 S4PTR *, long, short ) ;
// AS Feb 22/06 - support for new functionality...
int S4FUNCTION d4readBufferConfigure( DATA4 S4PTR *d4, long numRecs ) ;
S4EXPORT void S4FUNCTION d4recall( DATA4 S4PTR * ) ;
#define d4recCount( d4 ) ( d4recCountDo2( d4, 0 ) )
S4EXPORT long S4FUNCTION d4recCountDo( DATA4 S4PTR * ) ;
S4EXPORT long S4FUNCTION d4recCountDo2( DATA4 S4PTR *, Bool5 ) ;
#define d4recNo( d4 )  d4recNoLow( d4 )
S4EXPORT long S4FUNCTION d4recNoLow( DATA4 S4PTR * ) ;
#define d4record( d4 ) d4recordLow( d4 )
S4EXPORT char S4PTR *S4FUNCTION d4recordLow( DATA4 S4PTR * ) ;
#define d4recWidth( d4 ) d4recWidthLow( d4 )
S4EXPORT unsigned long S4FUNCTION d4recWidthLow( DATA4 S4PTR * ) ;
S4EXPORT long S4FUNCTION d4recWidth_v( DATA4 *data ) ;
S4EXPORT int S4FUNCTION d4refresh( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4refreshRecord( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4reindex( DATA4 S4PTR * ) ;
S4EXPORT short S4FUNCTION d4reindexWithProgress( DATA4 S4PTR *data, REINDEX_CALLBACK callback, long milliseconds ) ;
S4EXPORT int S4FUNCTION d4remove( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4seek( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION d4seekDouble( DATA4 S4PTR *, const double ) ;
S4EXPORT short S4FUNCTION d4seekN( DATA4 S4PTR *, const char S4PTR *, const short ) ;
S4EXPORT int S4FUNCTION d4seekNext( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION d4seekNextDouble( DATA4 S4PTR *, const double ) ;
S4EXPORT int S4FUNCTION d4seekLongLong( DATA4 S4PTR *, LONGLONG ) ;
S4EXPORT int S4FUNCTION d4seekNextLongLong( DATA4 S4PTR *, LONGLONG ) ;
S4EXPORT short S4FUNCTION d4seekNextN( DATA4 S4PTR *, const char S4PTR *, const short ) ;
S4EXPORT int S4FUNCTION d4skip( DATA4 S4PTR *, const long ) ;
S4EXPORT TAG4 S4PTR * S4FUNCTION d4tag( DATA4 S4PTR *, const char S4PTR * const ) ;
S4EXPORT TAG4 S4PTR * S4FUNCTION d4tagDefault( DATA4 S4PTR * ) ;
S4EXPORT TAG4 S4PTR * S4FUNCTION d4tagNext( DATA4 S4PTR *, TAG4 S4PTR * ) ;
S4EXPORT TAG4 S4PTR * S4FUNCTION d4tagPrev( DATA4 S4PTR *, TAG4 S4PTR * ) ;
S4EXPORT void S4FUNCTION d4tagSelect( DATA4 S4PTR *, TAG4 S4PTR * ) ;
S4EXPORT TAG4 S4PTR * S4FUNCTION d4tagSelected( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4tagSync( DATA4 S4PTR *, TAG4 S4PTR * const ) ;
S4EXPORT int S4FUNCTION d4top( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4unlock( DATA4 S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT int S4FUNCTION d4unlockAppend( DATA4 * ) ;
S4EXPORT char S4FUNCTION d4versionCB( DATA4 S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT long S4FUNCTION d4versionNumber( DATA4 *data ) ;
S4EXPORT long S4FUNCTION d4writeBuffer( DATA4 S4PTR *, long ) ;
#define d4write( a, b ) ( d4writeLow( a, b, 0, 1 ) )
S4EXPORT int S4FUNCTION d4writeLow( DATA4 S4PTR *, const long, const int, const int ) ;
S4EXPORT short S4FUNCTION d4writeVB(DATA4 *data, long recno) ;

S4EXPORT int S4FUNCTION d4zap( DATA4 S4PTR *, const long, const long ) ;

#define date4assign( a, b ) ( date4assignLow( (a), (b), 0 ) )
S4EXPORT int S4FUNCTION date4assignLow( char S4PTR *, const long, int ) ;
S4EXPORT S4CONST char *S4FUNCTION date4cdow( const char S4PTR * ) ;
S4EXPORT S4CONST char *S4FUNCTION date4cmonth( const char S4PTR * ) ;
#define date4day( datePtr )    ( (int)c4atol( (datePtr) + 6, 2 ) )
S4EXPORT short S4FUNCTION date4day_v( char * ) ;
S4EXPORT int S4FUNCTION date4dow( const char S4PTR * ) ;
S4EXPORT void S4FUNCTION date4format( const char S4PTR *, char S4PTR *, char S4PTR * ) ;/* 'dt' may be 'result'*/
S4EXPORT double S4FUNCTION date4formatMdx( const char S4PTR * ) ;
S4EXPORT void S4FUNCTION date4init( char S4PTR *, const char S4PTR *, char S4PTR * ) ;
S4EXPORT int S4FUNCTION date4isLeap( const char S4PTR * ) ;
S4EXPORT long S4FUNCTION date4long( const char S4PTR * ) ;
#define date4month( datePtr )  ( (int)c4atol( (datePtr) + 4, 2 ) )
S4EXPORT short S4FUNCTION date4month_v( char * ) ;
S4EXPORT void S4FUNCTION date4timeNow( char S4PTR * ) ;
S4EXPORT void S4FUNCTION date4today( char S4PTR * ) ;
#define date4year( yearPtr )   ( (int)c4atol( (yearPtr), 4 ) )
S4EXPORT short S4FUNCTION date4year_v( char * ) ;

// expose error4set so we can run our tests
S4EXPORT void S4FUNCTION error4callback( CODE4 S4PTR *c4, ERROR_CALLBACK errorCallback ) ;  // CS 2001/03/26 add
#define error4( a, b, c ) error4default( a, b, c )
S4EXPORT int S4FUNCTION error4default( CODE4 S4PTR *, const int, const long ) ;
#define error4describe( a, b, c, d, e, f ) error4describeDefault( a, b, c, d, e, f )
S4EXPORT int S4FUNCTION error4describeDefault( CODE4 S4PTR *, const int, const long, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT short S4FUNCTION error4describeVB( CODE4 *c4, short errCode, long extraInfo, char* desc1, char* desc2, char* desc3 ) ;
S4EXPORT void S4FUNCTION error4exitTest( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION error4file( CODE4 S4PTR *, S4CONST char S4PTR *, const int ) ;
S4EXPORT const char *S4FUNCTION error4lastDescription(CODE4 *) ;
S4EXPORT const char *S4FUNCTION error4lastDescriptionLow(void *) ;
S4EXPORT int S4FUNCTION error4set( CODE4 S4PTR *, const int ) ;
S4EXPORT const char S4PTR * S4FUNCTION error4text( CODE4 S4PTR *, const long ) ;
S4EXPORT short S4FUNCTION error4VB( CODE4 *, short, long ) ;

#define expr4data( e4 ) ( expr4dataCB( (e4) ) )
S4EXPORT DATA4 S4PTR* S4FUNCTION expr4dataCB( EXPR4 S4PTR * ) ;
S4EXPORT double S4FUNCTION expr4double( EXPR4 S4PTR * ) ;
#define expr4free( e4 ) ( expr4freeCB( (e4) ) )
S4EXPORT void S4FUNCTION expr4freeCB( EXPR4 S4PTR * ) ;
#define expr4len( e4 ) ( expr4lenCB( (e4) ) )
S4EXPORT long S4FUNCTION expr4lenCB( EXPR4 S4PTR * ) ;  /* LY 99/08/12 : updated for Sandage fix */
#define expr4null( e4 ) ( expr4nullLow( (e4), 1 ) )
S4EXPORT short S4FUNCTION expr4nullLow( const EXPR4 S4PTR *, const short ) ;
#define expr4parse( a, b ) ( expr4parseLow( (a), (b), 0 ) )
EXPR4 S4PTR* S4FUNCTION expr4parseCB( DATA4 *data, char S4PTR *string ) ;
S4EXPORT EXPR4 S4PTR *S4FUNCTION expr4parseLow( DATA4 S4PTR *, const char S4PTR *, void * ) ;
S4EXPORT S4CONST char S4PTR *S4FUNCTION expr4source( const EXPR4 S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION expr4str( EXPR4 S4PTR * ) ;
S4EXPORT int S4FUNCTION expr4true( EXPR4 S4PTR * ) ;
#define expr4type( e4 ) ( expr4typeCB( (e4) ) )
S4EXPORT short S4FUNCTION expr4typeCB( EXPR4 * ) ;
S4EXPORT int S4FUNCTION expr4vary( EXPR4 S4PTR *, char S4PTR * S4PTR * ) ;
S4EXPORT void S4FUNCTION c4setAcceptTimeOut( CODE4 S4PTR *, long ) ;

S4EXPORT int S4FUNCTION f4accessMode( FIELD4 * ) ;
S4EXPORT void S4FUNCTION f4assign( FIELD4 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignChar( FIELD4 S4PTR *, const int ) ;
S4EXPORT void S4FUNCTION f4assignCharVB( FIELD4 *f4, short character ) ;
S4EXPORT void S4FUNCTION f4assignCurrency( FIELD4 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignDateTime( FIELD4 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignDouble( FIELD4 S4PTR *, const double ) ;
S4EXPORT void S4FUNCTION f4assignField( FIELD4 S4PTR *, const FIELD4 S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignInt( FIELD4 S4PTR *, const int ) ;
S4EXPORT void S4FUNCTION f4assignIntVB( FIELD4 *field, const short iValue ) ;
S4EXPORT void S4FUNCTION f4assignLong( FIELD4 S4PTR *, const long ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT void S4FUNCTION f4assignLongLong( FIELD4 S4PTR *, const LONGLONG ) ;
S4EXPORT void S4FUNCTION f4assignN( FIELD4 S4PTR *, const char S4PTR *, const unsigned int ) ;
S4EXPORT void S4FUNCTION f4assignNVB( FIELD4 *field, const char* data, short dataLen ) ;
S4EXPORT void S4FUNCTION f4assignNotNull( FIELD4 S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignNull( FIELD4 S4PTR * ) ;
S4EXPORT char S4PTR * S4FUNCTION f4assignPtr( FIELD4 S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignUnicode( FIELD4 S4PTR *, const WSTR5 * ) ;
S4EXPORT void S4FUNCTION f4blank( FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4char( const FIELD4 S4PTR * ) ;
S4EXPORT char S4PTR *S4FUNCTION f4currency( const FIELD4 S4PTR *, short ) ;
S4EXPORT DATA4 S4PTR *S4FUNCTION f4data( const FIELD4 S4PTR * ) ;
S4EXPORT char S4PTR *S4FUNCTION f4dateTime( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4decimals( const FIELD4 S4PTR * ) ;
S4EXPORT double S4FUNCTION f4double( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4double2( const FIELD4 S4PTR *, double S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT float S4FUNCTION f4float( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4int( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4isMemo( FIELD4 *field ) ;
S4EXPORT unsigned long S4FUNCTION f4len( const FIELD4 S4PTR * ) ;
S4EXPORT short S4FUNCTION f4len_v( FIELD4 * ) ;
S4EXPORT int S4FUNCTION f4lockEnforce( FIELD4 * ) ;
S4EXPORT long S4FUNCTION f4long( const FIELD4 S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT LONGLONG S4FUNCTION f4longLong( FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4memoAssign( FIELD4 S4PTR *, const char S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT int S4FUNCTION f4memoAssignFile( FIELD4 S4PTR *field, S4CONST char S4PTR *fileNameFrom ) ;
S4EXPORT int S4FUNCTION f4memoFile( FIELD4 S4PTR *field, S4CONST char S4PTR *fileNameTo ) ;
S4EXPORT int S4FUNCTION f4memoAssignN( FIELD4 S4PTR *, const char S4PTR *, const unsigned int ) ;
S4EXPORT short S4FUNCTION f4memoAssignNVB( FIELD4 *field, char* data, short dataLen ) ;
S4EXPORT short S4FUNCTION f4memoAssignUnicode( FIELD4 S4PTR *, const WSTR5 * ) ;
S4EXPORT void S4FUNCTION f4memoChanged( FIELD4 S4PTR *, int ) ;
S4EXPORT int S4FUNCTION f4memoFree( FIELD4 S4PTR * ) ;
S4EXPORT unsigned long S4FUNCTION f4memoLen( FIELD4 S4PTR * ) ;
S4EXPORT unsigned int S4FUNCTION f4memoLenMax( FIELD4 S4PTR * ) ;
S4EXPORT unsigned long S4FUNCTION f4memoNcpy( FIELD4 S4PTR *, char S4PTR *, const unsigned int ) ;
S4EXPORT char S4PTR * S4FUNCTION f4memoPtr( FIELD4 S4PTR * ) ;
S4EXPORT unsigned int S4FUNCTION f4memoReadPart( FIELD4 S4PTR *, char S4PTR *, unsigned int, unsigned int ) ;
S4EXPORT int S4FUNCTION f4memoSetLen( FIELD4 *, const unsigned int ) ;
S4EXPORT S4CONST char S4PTR * S4FUNCTION f4memoStr( FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4memoWriteFinish( DATA4 *data ) ;
S4EXPORT int S4FUNCTION f4memoWritePart( FIELD4 *field, char *dataToWrite, unsigned int dataLen, long memoLen, long offset ) ;
S4EXPORT S4CONST char S4PTR * S4FUNCTION f4name( S4CONST FIELD4 S4PTR * ) ;
S4EXPORT unsigned long S4FUNCTION f4ncpy( FIELD4 S4PTR *, char S4PTR *, const unsigned int ) ;
S4EXPORT int S4FUNCTION f4null( const FIELD4 S4PTR * ) ;
// AS Feb 22/06 - support for new functionality...
S4EXPORT int S4FUNCTION f4nullable( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4number( const FIELD4 S4PTR * ) ;
S4EXPORT char S4PTR * S4FUNCTION f4ptr( const FIELD4 S4PTR * ) ;
S4EXPORT char S4PTR * S4FUNCTION f4str( FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4true( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4type( const FIELD4 S4PTR * ) ;

S4EXPORT int S4FUNCTION file4alloc( FILE4 S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4close( FILE4 S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4create( FILE4 S4PTR *, CODE4 S4PTR *, S4CONST char S4PTR *, const int ) ;
S4EXPORT int    S4FUNCTION file4flush( FILE4 S4PTR * ) ;
#define file4len( a ) ( file4lenLow2( (a) ) )
S4EXPORT unsigned long S4FUNCTION file4lenLow2( FILE4 S4PTR * ) ;
#define file4lenSet( a, b ) ( file4lenSetLow2( (a), (b) ) )
S4EXPORT int S4FUNCTION file4lenSetLow2( FILE4 S4PTR *, long ) ;
#define file4lock( f4, a, b ) file4lockInternal( (f4), (unsigned long)a, 0, (unsigned long)b, 0 )
S4EXPORT int    S4FUNCTION file4lockInternal( FILE4 S4PTR *, unsigned long, long, unsigned long, long ) ;
#define file4name( a ) ( file4nameLow( (a) ) )
S4EXPORT const char * S4FUNCTION file4nameLow( FILE4 S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4open( FILE4 S4PTR *, CODE4 S4PTR *, S4CONST char S4PTR *, const int ) ;
#define file4optimize( f4, i1, i2 ) ( file4optimizeLow( (f4), (i1), (i2), 0, 0 ) )
S4EXPORT int    S4FUNCTION file4optimizeLow( FILE4 S4PTR *, const int, const int, const long, const void S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4optimizeWrite( FILE4 S4PTR *, const int ) ;
S4EXPORT unsigned int S4FUNCTION file4read( FILE4 S4PTR *, const long, void S4PTR *, const unsigned int ) ;
S4EXPORT int    S4FUNCTION file4readAll( FILE4 S4PTR *, const long, void S4PTR *, const unsigned int ) ;
S4EXPORT int    S4FUNCTION file4refresh( FILE4 S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4replace( FILE4 S4PTR *, FILE4 S4PTR * ) ;
#define file4unlock( f4, a, b ) file4unlockInternal( (f4), (unsigned long)(a), 0, (unsigned long)(b), 0 )
S4EXPORT int    S4FUNCTION file4unlockInternal( FILE4 S4PTR *, unsigned long, long, unsigned long, long ) ;
S4EXPORT int    S4FUNCTION file4write( FILE4 S4PTR *, const long, const void S4PTR *, const unsigned int ) ;

S4EXPORT int S4FUNCTION file4seqReadInit( FILE4SEQ_READ S4PTR *, FILE4 S4PTR *, long, void S4PTR *, const unsigned ) ;
S4EXPORT unsigned int S4FUNCTION file4seqRead( FILE4SEQ_READ S4PTR *, void S4PTR *, unsigned ) ;
S4EXPORT int S4FUNCTION file4seqReadAll( FILE4SEQ_READ S4PTR *, void S4PTR *, const unsigned int ) ;

S4EXPORT int S4FUNCTION file4seqWrite( FILE4SEQ_WRITE S4PTR *, const void S4PTR *, const unsigned int ) ;
S4EXPORT int S4FUNCTION file4seqWriteFlush( FILE4SEQ_WRITE S4PTR * ) ;
S4EXPORT int S4FUNCTION file4seqWriteInit( FILE4SEQ_WRITE S4PTR *, FILE4 S4PTR *, const long, void S4PTR *, const unsigned int ) ;
S4EXPORT int S4FUNCTION file4seqWriteRepeat( FILE4SEQ_WRITE S4PTR *, const long, const char ) ;

S4EXPORT int S4FUNCTION i4close( INDEX4 S4PTR * ) ;
S4EXPORT INDEX4 S4PTR *S4FUNCTION i4create( DATA4 S4PTR *, const char S4PTR *, const TAG4INFO S4PTR * ) ; /* 0 name -> productn */
S4EXPORT short S4FUNCTION i4createWithProgress( DATA4 S4PTR *, const char S4PTR *, const TAG4INFO S4PTR *, void S4PTR *, long ) ; /* 0 name -> productn */
S4EXPORT const char S4PTR *S4FUNCTION i4fileName( INDEX4 S4PTR * ) ;
S4EXPORT INDEX4 S4PTR *S4FUNCTION i4open( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION i4reindex( INDEX4 S4PTR * ) ;
S4EXPORT TAG4 S4PTR *S4FUNCTION i4tag( INDEX4 S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION i4tagAdd( INDEX4 S4PTR *, const TAG4INFO S4PTR * ) ;
S4EXPORT TAG4INFO *S4FUNCTION i4tagInfo( INDEX4 * ) ;
S4EXPORT int S4FUNCTION i4tagRemove( TAG4 S4PTR * ) ;

#define l4add( list, item ) l4addAfter( (list), (list)->lastNode, (item) )
S4EXPORT void S4FUNCTION l4addAfter( LIST4 S4PTR *, void S4PTR *, void S4PTR * ) ;
S4EXPORT void S4FUNCTION l4addBefore( LIST4 S4PTR *, void S4PTR *, void S4PTR * ) ;
#define l4init( l4 ) ( (void)( (l4)->selected = 0, (l4)->nLink = 0, (l4)->lastNode = 0 ) )
#define l4first( l4 )         ((void *)( ( (l4)->lastNode == 0 ) ? 0 : ( (l4)->lastNode->n ) ))
#define l4last( l4 )          ((void *)( (l4)->lastNode ))
#define l4next( list, link )  ((void *)( ( (void *)(link) == (list)->lastNode ) ? 0 : ( (void *)(link) == 0 ) ? l4first( list ) : (void *)(((LINK4 *)(link))->n) ))
#define l4numNodes( l4 ) ( (l4)->nLink )
#define l4pop( list )         ( l4remove( (list), (list)->lastNode ) )
S4EXPORT void S4PTR *S4FUNCTION l4prev( const LIST4 S4PTR *, const void S4PTR * ) ;
S4EXPORT void S4PTR *S4FUNCTION l4remove( LIST4 S4PTR *, void S4PTR * ) ;

S4EXPORT void S4PTR *S4FUNCTION mem4allocDefault( MEM4 S4PTR *, int ) ;
#define mem4alloc( a ) mem4allocDefault( a, 1 )
#define mem4allocZero( a ) mem4allocDefault( a, 1 )
S4EXPORT MEM4 S4PTR *S4FUNCTION mem4createDefault( CODE4 S4PTR *, int, const unsigned int, int, const int ) ;
#define mem4create( a, b, c, d, e ) mem4createDefault( a, b, c, d, e )
S4EXPORT int S4FUNCTION mem4freeDefault( MEM4 S4PTR *, void S4PTR * ) ;
#define mem4free( a, b ) ( mem4freeDefault( a, b ), b = 0 )
S4EXPORT void S4FUNCTION mem4release( MEM4 S4PTR * ) ;

S4EXPORT int S4FUNCTION relate4bottom( RELATE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4callbackInit( RELATE4 S4PTR *, QUERY_CALLBACK, long ) ;
S4EXPORT int S4FUNCTION relate4callbackInitUndo( RELATE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4querySetCallbackInit( RELATE4 S4PTR *, QUERY_SET_CALLBACK ) ;
S4EXPORT int S4FUNCTION relate4querySetCallbackInitUndo( RELATE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4changed( RELATE4 S4PTR * ) ;  /* Slave has been added or freed */
S4EXPORT unsigned long S4FUNCTION relate4count( RELATE4 S4PTR * ) ;
S4EXPORT RELATE4 S4PTR * S4FUNCTION relate4createSlave( RELATE4 S4PTR *, DATA4 S4PTR *, const char S4PTR *, TAG4 S4PTR * ) ;
#define relate4data( r4 ) ( relate4dataCB( (r4) ) )
S4EXPORT DATA4 S4PTR* S4FUNCTION relate4dataCB( RELATE4 S4PTR * ) ;
#define relate4dataTag( r4 ) ( relate4dataTagCB( (r4) ) )
S4EXPORT TAG4 S4PTR* S4FUNCTION relate4dataTagCB( RELATE4 * ) ;
S4EXPORT int S4FUNCTION relate4doAll( RELATE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4doOne( RELATE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4eof( RELATE4 S4PTR * ) ;
#define relate4errorAction( r4, code ) ( relate4errorActionVB( (r4), (code) ) )
S4EXPORT short S4FUNCTION relate4errorActionVB( RELATE4 S4PTR *, short ) ;
#define relate4free( r4, doClose ) ( relate4freeVB( (r4), (doClose) ) )
S4EXPORT short S4FUNCTION relate4freeVB( RELATE4 S4PTR * , short ) ;
S4EXPORT RELATE4 S4PTR * S4FUNCTION relate4init( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4lockAdd( RELATE4 S4PTR * ) ;
#define relate4master( r4 ) ( relate4masterCB( (r4) ) )
S4EXPORT RELATE4 S4PTR* S4FUNCTION relate4masterCB( RELATE4 S4PTR * ) ;
#define relate4masterExpr( r4 ) ( relate4masterExprCB( (r4) ) )
S4EXPORT const char S4PTR* S4FUNCTION relate4masterExprCB( RELATE4 * ) ;
#define relate4matchLen( r4 ) ( relate4matchLenVB( (r4) ) )
S4EXPORT short S4FUNCTION relate4matchLenVB( RELATE4 S4PTR *, short ) ;
S4EXPORT int S4FUNCTION relate4next( RELATE4 S4PTR * S4PTR *) ;
S4EXPORT int S4FUNCTION relate4optimizeable( RELATE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4querySet( RELATE4 S4PTR *, const char S4PTR * ) ;
S4EXPORT long S4FUNCTION relate4readBuffer( RELATE4 S4PTR *, long, short ) ;
S4EXPORT int S4FUNCTION relate4retain( RELATE4 S4PTR *, int ) ;
S4EXPORT int S4FUNCTION relate4skip( RELATE4 S4PTR *, const long ) ;        /* Extended record skip */
#define relate4skipEnable( r4, doEnable ) ( relate4skipEnable( (r4), (doEnable) ) )
S4EXPORT short S4FUNCTION relate4skipEnableVB( RELATE4 S4PTR *, short ) ;
S4EXPORT int S4FUNCTION relate4skipMaster( RELATE4 *, long ) ;
S4EXPORT int S4FUNCTION relate4sortSet( RELATE4 S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION relate4top( RELATE4 S4PTR * ) ;
#define relate4type( r4, relateType ) ( relate4typeVB( (r4), (relateType) ) )
S4EXPORT short S4FUNCTION relate4typeVB( RELATE4 S4PTR *, short ) ;

S4EXPORT int S4FUNCTION sort4init( SORT4 S4PTR *, CODE4 S4PTR *, const int, const int ) ;
S4EXPORT void S4FUNCTION sort4assignCmp2( SORT4 S4PTR *, S4CMP_FUNCTION ) ;
S4EXPORT int S4FUNCTION sort4free( SORT4 S4PTR * ) ;
S4EXPORT int S4FUNCTION sort4get( SORT4 S4PTR *, S4LONG S4PTR *, void S4PTR * S4PTR *, void S4PTR * S4PTR * ) ;
S4EXPORT int S4FUNCTION sort4getInit( SORT4 S4PTR * ) ;
S4EXPORT int S4FUNCTION sort4put( SORT4 S4PTR *, const S4LONG, const void S4PTR *, const void S4PTR * ) ;

S4EXPORT char S4PTR *S4FUNCTION t4alias( TAG4 S4PTR * ) ;
S4EXPORT int S4FUNCTION t4close( TAG4 S4PTR * ) ;
//S4EXPORT long S4FUNCTION t4count( TAG4 S4PTR * ) ;
S4EXPORT short S4FUNCTION t4descending( TAG4 * ) ;
#define t4expr( t4 ) ( t4exprCB( (t4) ) )
S4EXPORT const char S4PTR *S4FUNCTION t4exprCB( TAG4 S4PTR * ) ;
#define t4filter( t4 ) ( t4filterCB( (t4) ) )
S4EXPORT const char S4PTR *S4FUNCTION t4filterCB( TAG4 S4PTR * ) ;
#define t4open( d4, name ) ( t4openCB( (d4), (name) ) )
S4EXPORT TAG4 *S4FUNCTION t4openCB(DATA4 S4PTR *, char S4PTR * ) ;
S4EXPORT int S4FUNCTION t4seekN( TAG4 S4PTR *, const char S4PTR *, const short inputKeyLen, short ) ;
S4EXPORT TAG4INFO S4PTR * S4FUNCTION t4tagInfo( TAG4 S4PTR * ) ;
S4EXPORT short int S4FUNCTION t4unique( const TAG4 S4PTR * ) ;
#define t4uniqueSet( t4, val ) ( t4uniqueSetVB( (t4), (val) ) )
S4EXPORT short S4FUNCTION t4uniqueSetVB( TAG4 S4PTR *, const short ) ;

#define u4alloc( a ) ( u4allocDefault( a ) )
S4EXPORT void S4PTR *S4FUNCTION u4allocDefault( long ) ;
#define u4allocAgain( a, b, c, d ) ( u4allocAgainDefault( a, b, c, d ) )
S4EXPORT int S4FUNCTION u4allocAgainDefault( CODE4 S4PTR *, char S4PTR * S4PTR *, unsigned int S4PTR *, const unsigned int ) ;
#define u4allocErr( a, b ) ( u4allocErDefault( (a), (b) ) )
#define u4allocEr( a, b ) ( u4allocErDefault( a, b ) )
S4EXPORT void S4PTR *S4FUNCTION u4allocErDefault( CODE4 S4PTR *, long ) ;
#define u4allocFree( a, b ) ( u4allocFreeDefault( a, b ) )
S4EXPORT void S4PTR *S4FUNCTION u4allocFreeDefault( CODE4 S4PTR *, long ) ;
#define u4free( a ) ( u4freeDefault( a ), a = 0 )
S4EXPORT int S4FUNCTION u4freeDefault( void S4PTR * ) ;
S4EXPORT void S4FUNCTION u4memCpy( char *dest, char *source, long len) ;
S4EXPORT int S4FUNCTION u4nameChar( unsigned char ) ;
S4EXPORT int S4FUNCTION u4nameExt( char S4PTR *, int, const char S4PTR *, const int ) ;
S4EXPORT int S4FUNCTION u4namePiece( char S4PTR *, const unsigned int, const char S4PTR *, const int, const int ) ;
S4EXPORT unsigned long S4FUNCTION u4ncpy( char S4PTR *, const char S4PTR *, const unsigned int ) ;
S4EXPORT int S4FUNCTION u4remove( const char S4PTR * ) ;  /* used for testing */
S4EXPORT void S4FUNCTION u4yymmdd( char S4PTR * ) ;

S4EXPORT long S4FUNCTION v4Cstring(char S4PTR *) ;
S4EXPORT void S4FUNCTION v4Cstringfree(char S4PTR *) ;

static int d4setupFunctions( CODE4*, DATA4* ) ;
static int e4setupFunctions( CODE4*, EXPR4* ) ;
static int f4setupFunctions( CODE4*, FIELD4* ) ;
static int file4setupFunctions( CODE4*, FILE4* ) ;
static int i4setupFunctions( CODE4*, INDEX4* ) ;
static int r4setupFunctions( CODE4*, RELATE4* ) ;
static int t4setupFunctions( CODE4*, TAG4* ) ;

#if defined(_MSC_VER) && !defined(S4WINCE) && !defined(S4WIN64)
   #if _MSC_VER >= 900
      #pragma pack(pop)
   #else
      #pragma pack()
   #endif
#else
   #ifdef __BORLANDC__
      #pragma pack()
   #endif
#endif
