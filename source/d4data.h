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

/* d4data.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

// AS Feb 6/06 - add some logging to help diagnose relate suspend issues
#ifdef RELATE4LOG
   #ifdef S4UNIX
      #include <sys/timeb.h>
   #else
      #include <sys\timeb.h>
   #endif
#endif

typedef int S4CALL S4CMP_FUNCTION( S4CMP_PARM, S4CMP_PARM, size_t) ;

/* typedef required to work around VC++ 1.50 bug in definition of Sort4.assignCmp() */
typedef int (S4CALL *S4CMP_FUNCTION_PTR)( S4CMP_PARM, S4CMP_PARM, size_t ) ;
typedef short BOOL4  ;

enum Collate4type
{
   /* the various collation types in terms of their characteristics */
   /* this can either be used as the entry directly or as a bit mask. */
   collate4machineByteOrder = 0,
   collate4simple = 1,
   collate4subSort = 2,
   collate4compress = 4,
   collate4subSortCompress = 6,   /* subSort + compress */
   collate4unknown = 16        /* unloaded/unknow sequences */
} ;



typedef struct
{
   /* 1 entry of this struct object for every collation we support */
   enum Collate4type collateType ;           // used to determine what translation function is required

   void *charToKeyTranslationArray ;    // if NULL then the translation must be read from disk

   void *unicodeToKeyTranslationArray ;

   void *charToKeyCompressionArray ;   // if NULL then the translation must be read from disk or
                                       // generated from charToKeyTranslationArray

   void *unicodeToKeyCompressionArray ;

   // # chars per character to add to the key size (eg. if a subsort, then 1, else 0)
   // note that we need # of chars, not # bytes else cannot support expansion properly.
   unsigned short keySizeCharPerCharAdd ;

   // the character which indicates expansion/compression
   unsigned short expandOrCompressChar ;
   // unsigned short expandOrCompressUnicode ; - not needed.  For both char/unicode, use the char
   // value becuase it will be otherwise unused.  For unicode, only unicode value use

   // the character which indicates no tail bytes...
   unsigned short noTailChar ;
   // unsigned short noTailUnicode ; - not needed.  For both char/unicode, use the char
   // value becuase it will be otherwise unused.  For unicode, only unicode value use

   Bool5 didAllocChar ;    // did we allocate the character arrays?
   Bool5 didAllocUnicode ; // did we allocate the unicode arrays?

   unsigned short expandOrCompressUnicode ;  // potentially calculated field x4reverseShort( expand... )
   unsigned short noTailUnicode ;  // potentially calculated field x4reverseShort( expand... )
   Bool5 lossOfData ;    // needed in some cases to know if backward conversions are possible

   // AS Oct 30/03 To help with a seek partial key problem, store a flag in this structure for this case
   // this avoids an additional paramater to hundreds of functions.
   Bool5 considerPartialSeek ;
   Bool5 hasNull ;
   short maxKeyLen ;
   short lenIn ;
} COLLATE4 ;



enum Collate4name
{
   /* these numbers must correspond 1 to 1 with the array entry # in collationArray and
      the record # in the collation info table.
      the formula is:  Collate4name == record # in collation info table
                       Collaet4name - 1 == array entry number in collationArray
      functions are available for this (i.e. #defines):
         collate4arrayIndex( Collate4name collateName )
         collate4infoTableRecno( Collate4name collateName )
   */
   collate4none = 0,           // if no collation in particular is set...
   collate4machine = 1,

   /* NOTE:  For Unicode, the first 256 characters are identical as CodePage 1252 (i.e. Windows ANSI)
      Therefore, it is most preferable to use collate4generalCp1252 to sort the first 256 unicode
      characters in sequence.  The remaining characters get sorted in machine sequence.
   */
   collate4generalCp1252 = 2,  // general collation for CodePage 1252
   collate4generalCp437 = 3,   // general collation for CodePage 437 or 0
   collate4test = 4,           // test collation.  Users may use this one for customized ones...
   collate4croatianCp1250 = 5, // Croatian Collation for CodeBase 1250
   collate4croatianUpperCp1250 = 6,  // Upper Case Croatian Collation for CodeBase 1250
   collate4generalCp850 = 7,     // general collation for CodePage 850
   collate4avaya1252 = 8,  // LY Nov 29/04 : custom collation for Avaya (case-insensitive & accent-insensitive)
   collate4spanishCp1252 = 9,          // 2nd test collation (if multiple code pages required as an example).  Users may use this one for customized ones...
   collate4spanishCp850 = 10,          // 2nd test collation (if multiple code pages required as an example).  Users may use this one for customized ones...
} ;



#ifdef S4CLIENT_OR_FOX
   // array model used for single character with subsort
   typedef struct
   {
      unsigned char headChar ;  // special character to indicate expansion/compression
      unsigned char tailChar ;  // if expansion/compression, an index into the
                       // expansion/compression array, else valid tail
                       // character or special character to indicate to tail
                       // character
   } Translate4arrayChar ;



   typedef struct
   {
      unsigned short headChar ;  // special character to indicate expansion/compression
      unsigned char tailChar ;  // if expansion/compression, an index into the
                       // expansion/compression array, else valid tail
                       // character or special character to indicate to tail
                       // character
   } Translate4arrayUnicode ;



   enum Expansion4orCompression
   {
      expand4 = 1,     // means the character expands out to 2 characters (eg. german esstset)
      compress4 = 2,   // means the character if followed by another special character, compresses to 1 character (eg. spanish 'll' and 'ch')
      done4 = 3        // means reached end of array - sometimes need to count # elements of array.
   } ;



   typedef struct  // indexed via translate4tail
   {
      // are we doing expansion or compression for this character?
      enum Expansion4orCompression type ;
      union
      {
         struct {
            // index into the translation array
            unsigned int resultingChar1ForExpansionIndex ;
            unsigned int resultingChar2ForExpansionIndex ;  // second expansion character
         } expansion ;
         struct {
            // what next character in input string indicates compression
            // index into the translation array
            unsigned int nextCharacterToIndicateCompressionIndex ;
            unsigned int resultingCharForCompressionIndex ;
         } compression ;
      } expComp ;
   }  Expansion4compressionArray ;



   // AS Jun 9/04 - Added cp850 general collation
   // LY Nov 29/04 : added custom collation for Avaya (case-insensitive and accent-insensitive)
   // AS Jun 30/08 - added 2 spanish collations
   #define NUM4AVAIL_COLLATION_ENTRIES 10

   extern COLLATE4 collationArray[NUM4AVAIL_COLLATION_ENTRIES] ;

   /* we use the collateName enum - 1 to index into the collationArray... */
   #define collate4arrayIndex( collateName ) ( (int)((collateName) - 1) )

   /* returns a pointer to a COLLATE4 based on the collateName */
   #define collation4get( collateName ) (&(collationArray[collate4arrayIndex( collateName )]) )
   /* we use the collateName enum to determine record number of collate info table... */
   #define collate4infoTableRecno( collateName )  ((int)(collateName))



   /* do some assertions to guarantee the validity of the arrays... */
   //assert5( collationArray[collate4arrayIndex( collate4machine )].collateType == collate4machineByteOrder ) ;
#else
   // should never get called, but needs to be here for OLE-DB dll
   #define collation4get( collateName ) ( throw Err5internal( 0 ), 0 )
#endif /* S4FOX */



#ifdef __SC__
   typedef void _cdecl C4DTOK( char S4PTR *, const double ) ;
   typedef void _cdecl C4STOK( COLLATE4 S4PTR *, char S4PTR *, const char S4PTR *, const int, int * ) ;
#else
   typedef void C4DTOK( char S4PTR *, const double ) ;
   typedef void C4STOK( COLLATE4 S4PTR *, char S4PTR *, const char S4PTR *, const int, int * ) ;
#endif

#ifdef S4FOX
   typedef unsigned char translatedChars[256] ;
   typedef unsigned char compressedChars[2] ;
#endif

#if defined(__cplusplus) && !defined(S5OLE_DB)
   #define C4PLUS_PLUS
#endif

/* LY 99/5/6 : BOOL is non-ANSI data type */
#if !defined(BOOL)
   typedef int BOOL ;
#endif

extern BOOL s5fox;
extern BOOL s5mdx;
extern BOOL s5clipper;
extern BOOL s5hasDescending;

struct DATA4St ;
struct FIELD4St ;
struct DATA4FILESt ;

#ifdef S4COMPILE_TEST
   #ifndef S4OFF_MEMO
      struct F4MEMOSt ;
   #endif
   #ifndef S4OFF_INDEX
      struct INDEX4St ;
      struct TAG4St ;
      struct TAG4FILESt ;
      #ifndef S4CLIPPER
         struct INDEX4FILESt ;
      #endif
   #endif
   #ifndef S4OFF_TRAN
      struct S4CLASS TRAN4FILESt ;
      struct S4CLASS CODE4TRANSSt ;
   #endif
#else
   struct F4MEMOSt ;
   struct INDEX4St ;
   struct TAG4St ;
   struct TAG4FILESt ;
   struct S4CLASS TRAN4FILESt ;
   struct S4CLASS CODE4TRANSSt ;
   #ifndef S4CLIPPER
      struct INDEX4FILESt ;
   #endif
#endif /* S4COMPILE_TEST */


// AS 07/27/99 -- this has been superseded by more simple collations for foxPro
// struct T4VFPSt ;

#ifdef S4CBPP
   class S4CLASS FILE4 ;
   class S4CLASS CODE4 ;
   class S4CLASS Code4 ;

   typedef int ERROR4FUNCTION( Code4 S4PTR *, short int, long ) ;
   typedef int ERROR4DESCRIBE_FUNCTION( Code4 S4PTR *, short int, long, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   typedef int ERROR4STACK( Code4 S4PTR *, short int, long ) ;
   typedef int ERROR4SET( Code4 S4PTR *, short int ) ;
#else
   struct FILE4St ;
   struct CODE4St ;

   typedef int ERROR4FUNCTION( struct CODE4St S4PTR *, short int, long ) ;
   typedef int ERROR4DESCRIBE_FUNCTION( struct CODE4St S4PTR *, short int, long, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   typedef int ERROR4STACK( struct CODE4St S4PTR *, short int, long ) ;
   typedef int ERROR4SET( struct CODE4St S4PTR *, short int ) ;
#endif

// AS Feb 9/06 - added clipper support for packwithstatus
#if defined( TIME4STATUS ) && defined( S4WIN32 ) && !defined( S4OFF_WRITE )
   struct REINDEX4STATUSSt ;
#endif

#if !defined(S4OFF_COMMUNICATIONS) && defined(S4CLIENT)
   struct CONNECTION4St ;
#endif

#ifdef S4MAC_TCP  //CJ Jul 17/01-define for Macintosh client connection C4CONLOW strucuture type.
   class Connection4macClient ;
#endif


typedef struct l4linkSt
{
   struct l4linkSt S4PTR *n, S4PTR *p ;
} LINK4 ;



#ifdef S5OLE
   typedef struct S5CLASS
#else
   #ifdef S4CBPP
      typedef struct S4CLASS
   #else
      typedef struct
   #endif
#endif
{
   S4CONV( LINK4 S4PTR *lastNode, LINK4 S4PTR *last_node ) ; /* the last Link */
   LINK4 S4PTR *selected ;
   /* AS 04/29/99 --> changed link list to handle more links (from 64k to 4 gig) */
   S4CONV( unsigned long nLink, unsigned long n_link ) ; /* The number of links in the list */
   #if defined( E4ANALYZE ) && !defined( S4UTILS )
      struct LIST4MUTEXSt *listMutex ;   // non-null if the list is a list-mutex - ensures we have mutexes held when manipulating lists
   #else
      void *space1 ;
   #endif
} LIST4 ;



typedef struct s4singleSt
{
   struct s4singleSt *nextLink ;
} SINGLE4 ;



typedef struct s4singleDistantSt
{
   struct s4singleDistantSt *nextLink ;
} SINGLE4DISTANT ;



typedef struct
{
   LINK4 link ;
   double  data ;  /* Make sure it is on a boundry good for at least a double  */
} Y4CHUNK ;



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



#if !defined(S4OFF_OPTIMIZE)
   typedef struct
   {
      LIST4 list ;

      unsigned short int minLink ;
      unsigned long maxTime ;
      unsigned long minTime ;
      char currentPrioCount ;    /* should this list be scanned for avail blocks? */
   } OPT4LIST ;
#endif


// AS Apr 13/04 - move FILE4LONG higher in the file, it is referred to earlier now...
/* temporarily allow everything to work if S4FILE_EXTENDED not defined (for development purposes only) */
#ifdef S4FILE_EXTENDED
   #ifndef S464BIT
      #ifndef S4UNIX
         typedef struct
         {
            unsigned long longLo ;  /* must be unsigned, can be up to 4 gig */
            #ifdef S4WIN32
               /* for 64 bit file addressing (files > 4 GIG) */
               long longHi ;  /* signed value, negative could be error or that the whole value is negative (-1 means longLo is negative) */
            #endif
         } FILE4LONGPIECE ;

         typedef union
         {
            FILE4LONGPIECE piece ;
            DWORDLONG dLong ;
         } FILE4LONG ;

         // AS 09/17/99 --> coded to allow for type coercion...
         // AS 11/30/99 --> inline not avail in C compile...
         /* LY 00/11/16 : export function due to failure of Microsoft IA-64 compiler */
         #if defined(__cplusplus) && !defined(S4WIN64)
            // cannot be #define because we call it with a calculated value sometimes
            inline FILE4LONG file4longCoerce( LONGLONG val )
            {
               // assert5( sizeof( val ) == sizeof( FILE4LONG ) ) ;
               return *(((FILE4LONG *)(&val))) ;
            }
         #else
            // cannot be #define because we call it with a calculated value sometimes
            // placed in d4data.c
            // CS 2000/04/03 must export for non-C++ applications
            S4EXPORT FILE4LONG S4FUNCTION file4longCoerce( LONGLONG val ) ;
         #endif

         #define file4longAssign( f2, longLoIn, longHiIn ) ( ((f2).piece.longLo) = (longLoIn), ((f2).piece.longHi) = (longHiIn) )
         #define file4longAssignLong( f1, f2 ) ( (f1).dLong = (f2).dLong )
         #define file4longCmp( f1, f2 ) ( ( (f1).piece.longLo == (f2).piece.longLo && (f1).piece.longHi == (f2).piece.longHi ) ? 0 : 1 )
         #define file4longGetLo( f1 ) ( (f1).piece.longLo )
         #define file4longGetHi( f1 ) ( (f1).piece.longHi )
         #define file4longEqual( f1, val ) ( (f1).piece.longLo == (val) && (f1).piece.longHi == 0 )
         #define file4longEqualLong( f1, f2 ) ( (f1).piece.longLo == (f2).piece.longLo && (f1).piece.longHi == (f2).piece.longHi )
         #define file4longEqualZero( f1 ) ( (f1).piece.longLo == 0 && (f1).piece.longHi == 0 )
         #define file4longGreater( f1, val ) ( (f1).piece.longLo > (val) || (f1).piece.longHi > 0 )
         #define file4longGreaterEq( f1, val ) ( (f1).piece.longLo >= (val) || (f1).piece.longHi > 0 )
         #define file4longGreaterLong( f1, f2 ) ( ((f1).piece.longHi > (f2).piece.longHi ) || ( (f1).piece.longHi == (f2).piece.longHi && (f1).piece.longLo > (f2).piece.longLo ) )
         #define file4longGreaterEqLong( f1, f2 ) ( ((f1).piece.longHi > (f2).piece.longHi ) || ( (f1).piece.longHi == (f2).piece.longHi && (f1).piece.longLo >= (f2).piece.longLo ) )
         #define file4longGreaterZero( f1 ) ( ((f1).piece.longLo > 0 && (f1).piece.longHi >= 0 ) ||((f1).piece.longHi > 0))
         #define file4longGreaterEqZero( f1 ) ( (f1).piece.longLo >= 0 && (f1).piece.longHi >= 0 )
         #define file4longLess( f1, val ) ( (f1).piece.longLo < (val) && (f1).piece.longHi == 0 )
         #define file4longLessLong( f1, f2 )( ((f1).piece.longHi < (f2).piece.longHi) || ( ((f1).piece.longHi == (f2).piece.longHi) && ((f1).piece.longLo < (f2).piece.longLo)) )
         #define file4longLessEq( f1, val ) ( (f1).piece.longLo <= (val) && (f1).piece.longHi == 0 )
         #define file4longLessEqLong( f1, f2 ) ( ((f1).piece.longHi < (f2).piece.longHi)|| ( ((f1).piece.longHi == (f2).piece.longHi) && ((f1).piece.longLo <= (f2).piece.longLo)) )
         #define file4longSetLo( f1, val ) ( (f1).piece.longLo = val )
         #define file4longMultiply( f1, f2 ) ( (f1).dLong *= (f2))
         #define file4longDivide( f1, f2 ) ( (f1).dLong /= (f2))
         #define file4longMod( f1, f2 ) ( (f1).dLong % (f2))
         #define file4longSubtract( f1, val ) ( (*(f1)).dLong -= (val) )
         #define file4longSubtractLong( f1, f2 ) ( (*(f1)).dLong -= (*(f2)).dLong )
      /*   #define file4longSubtractLongLong( f1, f2 ) ( (*(f1)).dLong -= ((f2)).dLong ) */
      /*   #define file4longSubtractLongLong( FILE4LONG *f1, FILE4LONG f2 ) */

         #define file4longAdd( f1, val ) ( (*(f1)).dLong += (val) )
         #define file4longAddLong( f1, f2 ) ( (*(f1)).dLong += (*(f2)).dLong )
         #define file4longAssignError( f2 ) ( (f2).piece.longHi = ULONG_MAX )
         #define file4longError( f2 ) ( (unsigned long)(f2).piece.longHi )
         #define file4longGetHiAddress( f1 ) (&((f1).piece.longHi))
         #define file4longCheckError( f1 ) ( ((f1).piece.longLo == ULONG_MAX ) ? ( ( GetLastError() != NO_ERROR ) ? ( (f1).piece.longHi = ULONG_MAX ) : 0 ) : 0 )
      #else
         #define FILE4LONG off_t

         #define file4longCoerce( val ) ( (off_t)val )
         #define file4longAdd( f1, val ) ( *(f1) += (val) )
         #define file4longAddLong( f1, f2 ) ( *(f1) += *(f2) )
         #define file4longAssignLong( f1, f2 ) ( (f1) = (f2) )
         #ifdef S4BYTE_SWAP
            #define file4longAssign( f2, longLoIn, longHiIn ) (*((long *)&(f2)) = (longHiIn), *((long *)&(f2)+1) = (longLoIn) )
            #define file4longAssignError( f2 ) ( *( (unsigned long *)&(f2) ) = ULONG_MAX )
            #define file4longError( f2 ) ( *( (unsigned long *)&(f2) ) )
            #define file4longGetHi( f1 ) ( *( (long *)&(f1) ) )
            unsigned long file4longGetLo( FILE4LONG ) ;
         #else
            #define file4longAssign( f2, longLoIn, longHiIn ) (*((long *)&(f2)) = (longLoIn), *((long *)&(f2)+1) = (longHiIn) )
            #define file4longAssignError( f2 ) ( *( (unsigned long *)&(f2)+1 ) = ULONG_MAX )
            #define file4longError( f2 ) ( *( (unsigned long *)&(f2)+1 ) )
            #define file4longGetHi( f1 ) ( *( (long *)&(f1)+1 ) )
            unsigned long file4longGetLo( FILE4LONG ) ;
         #endif
         #define file4longDivide( f1, f2 ) ( (f1) /= (f2))
         #define file4longGreaterEqLong( f1, f2 ) ( (f1) >= (f2) )
         #define file4longGreaterLong( f1, f2 ) ( (f1) > (f2) )
         #define file4longGreaterZero( f1 ) ( (f1) > 0 )
         #define file4longGreaterEqZero( f1 ) ( (f1) >= 0 )
         #define file4longLess( f1, val ) ( (f1) < (val) )
         #define file4longLessEq( f1, val ) ( (f1) <= (val) )
         #define file4longMultiply( f1, f2 ) ( (f1) *= (f2))
         #define file4longSubtract( f1, val ) ( *(f1) -= (val) )
         /* LY 4/29/99 */
         #define file4longLessEqLong( f1, f2 ) ( (f1) <= (f2) )
         #define file4longEqualLong( f1, f2 ) ( (f1) == (f2) )  /* LY 00/05/16 - large memo file offset */
         #define file4longEqualZero( f1 ) ( (f1) == 0 )
         #define file4longLessLong( f1, f2 ) ( (f1) < (f2) )
         /* LY 2001/07/18 : added longSubtractLong */
         #define file4longSubtractLong( f1, f2 ) ( *(f1) -= *(f2) )
      #endif /* #ifndef S4UNIX */
   #else
      #define FILE4LONG off_t /* LY 2002/08/21 : changed from S4LONG */
      #define file4longCoerce( val ) ( val )

      #define file4longAdd( f1, val ) ( *(f1) += (val) )
      #define file4longAddLong( f1, f2 ) ( (*(f1)) += (*(f2)) )
      #define file4longAssign( f2, longLoIn, longHiIn ) ((f2) = (longLoIn) + (longHiIn<<32))
      #define file4longAssignError( f2 ) ( (f2) = ULONG_MAX )
      #define file4longAssignLong( f1, f2 ) ( (f1) = (f2) )
      #define file4longCheckError( f1 )
      #define file4longCmp( f1, f2 ) ( (f1) == (f2)  ? 0 : 1 )
      #define file4longError( f2 ) ( f2 )
      #define file4longGetHi( f1 ) ( f1 >> 32 )
      #define file4longGetHiAddress( f1 ) (0)
      #define file4longGetLo( f1 ) ( f1 & 0x00000000FFFFFFFF )
      #define file4longGreater( f1, val ) ( (f1) > (val) )
      #define file4longGreaterEqLong( f1, f2 ) ( (f1) >= (f2) )
      #define file4longGreaterLong( f1, f2 ) ( (f1) > (f2) )
      #define file4longGreaterZero( f1 ) ( (f1) > 0 )
      #define file4longGreaterEqZero( f1 ) ( (f1) >= 0 )
      #define file4longLess( f1, val ) ( (f1) < (val) )
      #define file4longLessEq( f1, val ) ( (f1) <= (val) )
      #define file4longMultiply( f1, f2 ) ( (f1) *= (f2) )
      #define file4longSetLo( f1, val ) ( (f1) = (val) )
      #define file4longSubtract( f1, val ) ( *(f1) -= (val) )
      #define file4longSubtractLong( f1, f2 ) ( *(f1) -= *(f2) )
      #define file4longDivide( f1, f2 ) ( f1 /= (f2))
      /* LY 4/29/99 */
      #define file4longLessEqLong( f1, f2 ) ( (f1) <= (f2) )
      /* LY 2001/07/18 : added longLessLong and longEqualLong */
      #define file4longLessLong( f1, f2 ) ( (f1) < (f2) )
      #define file4longEqualLong( f1, f2 ) ( (f1) == (f2) )
      #define file4longEqualZero( f1, f2 ) ( (f1) == 0 )
   #endif
#else
   #define FILE4LONG unsigned long

   #define file4longCoerce( val ) ( (unsigned long)val )
   #define file4longAdd( f1, val ) ( *(f1) += (val) )
   #define file4longAddLong( f1, f2 ) ( (*(f1)) += (*(f2)) )
   #define file4longAssign( f2, longLoIn, longHiIn ) ((f2) = (longLoIn))
   #define file4longAssignError( f2 ) ( (f2) = ULONG_MAX )
   #define file4longAssignLong( f1, f2 ) ( (f1) = (f2) )
   #define file4longCheckError( f1 )
   #define file4longCmp( f1, f2 ) ( (f1) == (f2)  ? 0 : 1 )
   #define file4longError( f2 ) ( (long)(f2) )
   // AS Sep 30/04 - for non-fox compile
   #define file4longEqualZero( f1 ) ( (f1) == 0 )
   #define file4longGetHi( f1 ) ( 0 )
   #define file4longGetHiAddress( f1 ) (0)
   #define file4longGetLo( f1 ) ( f1 )
   #define file4longGreater( f1, val ) ( (f1) > (val) )
   #define file4longGreaterLong( f1, f2 ) ( (f1) > (f2) )
   #define file4longGreaterEqLong( f1, f2 ) ( (f1) >= (f2) )
   #define file4longGreaterZero( f1 ) ( (f1) > 0 )
   #define file4longGreaterEqZero( f1 ) ( (f1) >= 0 )
   #define file4longLess( f1, val ) ( (f1) < (val) )
   #define file4longLessEq( f1, val ) ( (f1) <= (val) )
   #define file4longMod( f1, f2 ) ( (f1) % (f2))
   #define file4longMultiply( f1, f2 ) ( (f1) *= (f2) )
   #define file4longSetLo( f1, val ) ( (f1) = (val) )
   #define file4longSubtract( f1, val ) ( *(f1) -= (val) )
   #define file4longSubtractLong( f1, f2 ) ( *(f1) -= *(f2) )
   #define file4longDivide( f1, f2 ) ( (f1) /= (f2))
   #define file4longLessLong( f1, f2 )( (f1) < (f2) )
   #define file4longEqualLong( f1, f2 ) ( (f1) == (f2) )
   /* LY 4/29/99 */
   #define file4longLessEqLong( f1, f2 ) ( (f1) <= (f2) )
   #define file4longEqualZero( f1 ) ( (f1) == 0 )  // LY Jul 23/04
#endif /* S4FILE_EXTENDED */



/* structure whose size, when added to a FILE4LONG, will be 10 */
typedef struct FILE4LONG_EXTENDSt
{
   char space[10 - sizeof( FILE4LONG )] ;
} FILE4LONG_EXTEND ;


// AS Sep 8/04 - changed for debugging support
#ifdef S4WIN32
   // AS Apr 11/05 - enabled general access to thse functions...
   // #ifdef E4ANALYZE
      typedef struct
      {
         CRITICAL_SECTION section ;
         short count ;
         DWORD threadOwner;   // thread that owns the critical section (retrieved via GetCurrentThreadId(void)
      } CRITICAL4SECTION ;
   // #else
   //    #define CRITICAL4SECTION CRITICAL_SECTION
   // #endif
#endif



#if !defined(S4OFF_OPTIMIZE)
   typedef struct
   {
      LINK4 link ;
      LINK4 lruLink ;
      char changed ;
      unsigned len ;
      void S4PTR *data ;
      unsigned long readTime ;
      unsigned long accessTime ;
      double hitCount ;
      OPT4LIST *optList ;       /* the optimization list the file resides on */

      /* these next 2 elements must match the OPT4CMP structure in o4opt.h */
      #ifdef S4CBPP
         FILE4 S4PTR *file ;
      #else
         struct FILE4St *file ;
      #endif
      // AS Apr 13/04 - support for optimizing loarge files
      // unsigned long pos ;
      FILE4LONG pos ;
   } OPT4BLOCK ;
#endif



#if !defined(S4OFF_OPTIMIZE)
   #ifdef S4CBPP
      typedef struct S4CLASS
   #else
      typedef struct
   #endif
   {
      char blockPower ;
      char forceCurrent ;    /* switch forces a read of current contents */
      char numShift ;

      unsigned char oldMode ;
      unsigned char doUpdate ;
      unsigned char checkCount ;
      unsigned char dummyChar ;

      unsigned int minLink ;
      int numBuffers ;

      unsigned maxBlocks ;
      unsigned writeBlockCount ; /* is the buffer full? */

      unsigned long blockSize ;
      unsigned long bufferSize ;
      unsigned long hashTrail ;       /* where last optimized file ended */
      unsigned long mask ;
      unsigned long numBlocks ;
      unsigned long numLists ;
      // AS Apr 13/04 - support for optimizing loarge files
      FILE4LONG readStartPos ;
      FILE4LONG writeStartPos ;
      FILE4LONG writeCurPos ;

      unsigned long readTimeCount ;
      unsigned long accessTimeCount ;
      unsigned int minAccessTimeVariation ;  /* access times of less than this value will not be updated == 1% of blocks */

      char S4PTR *readBuffer ;
      char S4PTR *writeBuffer ;
      char S4PTR *writeBufferActual ;  /* writeBuffer just points to the actual buffer */

      void S4PTR* S4PTR* buffers ;

      OPT4BLOCK S4PTR *blocks ;

      LIST4 S4PTR *lists ;
      OPT4LIST S4PTR *prio[OPT4NUM_LISTS] ;

      LIST4 avail ;
      OPT4LIST dbfLo ;
      OPT4LIST dbfHi ;
      OPT4LIST indexLo ;
      OPT4LIST indexHi ;
      OPT4LIST other ;

      LIST4 optFiles ;

      #ifdef S4WIN32
         #ifdef S4WRITE_DELAY
            LIST4 delayAvail ;   /* extra blocks to allow efficient delay-writing */
            char S4PTR *delayWriteBuffer ;
            char S4PTR *delayLargeBuffer ;
            CRITICAL4SECTION critical4optWrite ;  // AS Sep 8/04 - changed for debugging support
            int delayLargeBufferAvail ;
            int writeBufferActualAvail ;
         #else
            LIST4 space1 ;   /* extra blocks to allow efficient delay-writing */
            char S4PTR *space2 ;
            char S4PTR *space3 ;
            CRITICAL4SECTION space4 ;  // AS Sep 8/04 - changed for debugging support
            int space5 ;
            int space6 ;
         #endif
      #endif

      #ifdef S4WIN32
         #ifdef S4ADVANCE_READ
            char S4PTR *advanceLargeBuffer ;
            CRITICAL4SECTION critical4optRead ;  // AS Sep 8/04 - changed for debugging support
            #ifdef S4CBPP
               FILE4 S4PTR *advanceReadFile ;
            #else
               struct FILE4St S4PTR *advanceReadFile ;
            #endif
            int advanceLargeBufferAvail ;
            // AS Apr 13/04 - support for optimizing loarge files
            FILE4LONG advanceLargePos ;
            unsigned int advanceLargeLen ;
         #else
            char S4PTR *space7 ;
            CRITICAL4SECTION space8 ;  // AS Sep 8/04 - changed for debugging support
            #ifdef S4CBPP
               FILE4 S4PTR *space9 ;
            #else
               struct FILE4St S4PTR *space9 ;
            #endif
            int space10 ;
            FILE4LONG space11 ;
            unsigned int space12 ;
         #endif
      #endif

      #ifdef S4CBPP
         FILE4 S4PTR    *writeFile ;
         FILE4 S4PTR    *readFile ;
      #else
         struct FILE4St S4PTR *writeFile ;        /* which file has the write buffer? */
         struct FILE4St S4PTR *readFile ;        /* which file has the write buffer? */
      #endif
   } OPT4 ;
#endif /* S4OFF_OPTIMIZE */



#ifndef S4INTERNAL_COMPILE_CHECK
   #define file4len( f4 ) ( file4longGetLo( file4lenLow( f4 ) ) )
#endif


// AS Jan 15/03 - Only needed if building server or building OLE-DB
#if defined( S4SERVER ) || defined( OLEDB5BUILD )
   #if defined(__cplusplus) && ( defined(S4WIN32) || ( defined(S4UNIX) && !defined(S4STAND_ALONE ) ) ) && !defined(OLEERRORSTRING)
      class S4EXPORT Find4file
      {
      private:
         #ifdef S4WIN32
            WIN32_FIND_DATA findData ;
            HANDLE rc ;
         #endif
         #ifdef S4UNIX
            DIR *findData ;
         #endif
         char ext[8];
         char fn[LEN4PATH] ;
         char foundName[LEN4PATH] ;

         void close() ;
      public:
         Find4file( const char *path = 0, const char *fileName = 0, const char *extension = 0 ) ;
         const char *getNext(char *dest = 0, size_t destLen = 0 ) ;
         ~Find4file() { close() ; }
      } ;
   #endif
#endif



// AS Nov 26/02 - for data file compression
// AS May 17/04 - server support for data file compression
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   typedef struct
   {
      // the header
      short version ;    // what version of the compression is this? -  set to 1 for this structure
      short blockSize ;  // in 1k multiples
      long numOffsets ;
      long fileLen ;     // the actual physical file length if it wasn't compressed
      long offsets[1] ;  // the offsets start here
   } COMPRESS4DATA_SHORT ;



   // AS Jul 31/03 - Support for large compressed files
   typedef struct
   {
      // the header
      short version ;         // what version of the compression is this? - set to 2 for this structure
      short blockSize ;       // in 1k multiples
      long numOffsets ;
      FILE4LONG fileLen ;     // the actual physical file length if it wasn't compressed
      FILE4LONG offsets[1] ;   // the offsets start here
   } COMPRESS4DATA_LONG ;



   // AS Jun 23/04 - support for writing to compressed tables
   typedef struct
   {
      FILE4LONG position ;
      unsigned short compressedLength ;   // maximum block size is 64k
      unsigned short actualBlockLength ;  // we also store the actual size of the block (used because blocks can be re-used)
   } COMPRESS4WRITE_ARRAY_ENTRY ;


   typedef struct
   {
      FILE4LONG nextPos ;
      unsigned short blockLength ;  // we also store the actual size of the block (used because blocks can be re-used)
   } COMPRESS4FREE_BLOCK ;


   typedef struct COMPRESS4WRITE_BLOCKSt
   {
      // one block of array information
      unsigned long numEntries ;          // the number of array entries in this block - this is a variable because we want to save quite a few at
                                          // a time to avoid having a long chain
//      unsigned long numEntriesUsed ;      // not all the entries may be used (room for expansion)
      FILE4LONG nextBlock ;               // pointer to the next block of arrays, if it exists

      struct COMPRESS4WRITE_BLOCKSt *nextBlockPtr ;
      COMPRESS4WRITE_ARRAY_ENTRY *writeArray ;     // the offsets start here
   } COMPRESS4WRITE_BLOCK ;



   typedef struct
   {
      // the header  *** BE VERY CAREFUL - if this structure is changed, the d4open code must be modified to account (d4compressInit) for initialization
      short version ;         // what version of the compression is this? - set to 3 for this structure
      short blockSize ;       // in 1k multiples - maximum 64k for writeable files
      unsigned short arrayAllocCount ; // the number of blocks to allocate for new arrays whenever we need some additional blocks
                              // this is more than 1 block because we want to keep from requiring too many disk reads.
                              // this value is also the additional length added to the first block to allow for expansion
      unsigned long totalBlocks ;       // the total number of array entries in the file
      unsigned long totalBlocksUsed ;   // the total number of used array entries in the file
      FILE4LONG fileLen ;     // the actual physical file length if it wasn't compressed
      FILE4LONG freeChain ;   // index to next free block - pointing to the smallest available free block (if any).  The blocks are ordered by size - smallest first.
      COMPRESS4WRITE_BLOCK firstArrayBlock ;  // *** THIS MEMBER MUST BE AT THE END OF THIS STRUCTURE - it is allocatable space ***
   } COMPRESS4WRITE_HEADER ;



   // AS Jul 31/03 - support for generic handling of compression
   // there are 3 mutually exclusive write-compression modes we support:  This structure sets the pointer to which is being supported
   // for the given file.  The 3 modes are:
   // (0) read-only compressed file, not a large file
   // (1) read-only compressed file, large file (> 4 Gigs)
   // (2) read/write compressed file
   typedef struct
   {
      short isLongOrCompress ;   // 0 - short, 1 - long, 2 - write compress
      COMPRESS4DATA_SHORT *shortCompress ;
      COMPRESS4DATA_LONG  *longCompress ;
      COMPRESS4WRITE_HEADER *writeCompress ;
      Bool5 isLocked ;    // for write compression must perform locking - do we have it locked?
   } COMPRESS4HANDLER ;

#endif


// AS Nov 8/02 - moved up for compiling, and for WINCE sema4 locking
#if !defined( S4OFF_THREAD ) || defined( S4SEMA4LOCK )
   typedef struct SEMAPHORE4St
   {
      LINK4 link ;   /* Allows the SEMAPHORE4 to be on a list of SEMAPHORE3s */
      #ifdef S4WIN32
         HANDLE handle ; /* Handle to the semaphore itself */
      #endif
   } SEMAPHORE4 ;
#endif


// AS Nov 11/02 - used for wince locking
// AS Feb 15/06 - fixes for ce...always define so file structure size can be correct
typedef struct
{
   HANDLE mutex ;
   char isValid ;
   #ifdef E4ANALYZE
      int lockCount ;
   #else
      int dummyCount ;
   #endif
} MUTEX4 ;



#ifdef S4CBPP
class S4CLASS FILE4
{
public:
#else
typedef struct FILE4St
{
#endif
   #ifndef S4OFF_OPTIMIZE
      LINK4 link ;           /* set to 0 if file not optimized */
   #else
      LINK4 space2b ;
   #endif
   /* accessMode and isReadOnly both on indicate 'r' attribute on file */
   /* isReadOnly only on indicates user access is limited to read only */
   /* if accessMode, will do full bufferring, if isReadOnly, will avoid */
   /* performing any disk writes */

   char doAllocFree ;
   // AS Sep. 17/01 - Must never change this value directly, must use a function in order to keep the validation
   // tables intact.  Changed name from isTemp in order to force compile error to ensure it gets set correctly
   // via function everywhere.
   // function is is file4setTemporary( FILE4 *file, Bool5 flag ) ;
   Bool5 isTemporary ;             /* True if it is a temporary file */

   #if defined(S4WIN32)
      HANDLE hand ;
   #elif defined(S4WIN16)
      HFILE hand ;
   #elif defined(S4PALM)
      FileHand hand ;
   #else
      int hand ;
   #endif

   int isReadOnly ;          /* True if file is read only */

   char S4PTR *nameBuf ;

   S4CONST char S4PTR *name ;

   #ifdef S4MACINTOSH
      struct FSSpec macSpec ;
   #endif

   #ifdef S4CBPP
      CODE4 S4PTR *codeBase ;
   #else
      S4CONV( struct CODE4St *codeBase, struct CODE4St *code_base ) ;
   #endif

   #ifndef S4OFF_MULTI
      short lowAccessMode ;       /* open access level */
   #else
      short space25 ;
   #endif

   #ifdef S4FILE_EXTENDED
      char isLong ;
   #else
      char space26 ;
   #endif
   // AS May 24/02 - exposed file4open only to users now to allow file type considerations to play a larger role
   // (in particular to decide whether or not to encrypt files - only encrypt data/index/memos with block encryption)
   char type ;           /* dbf, index, other */
   #ifndef S4OFF_OPTIMIZE
      char fileCreated ;     /* false if the file has not been created yet - i.e. if a memory-only file */
      char writeBuffer ;    /* buffer writes where possible */
      long hashInit ;
      FILE4LONG len ;            /* internal if optimized */
      FILE4LONG_EXTEND space0 ;
      char bufferWrites ;   /* are writes being bufferred? */
      int  doBuffer ;       /* is the file bufferring on? */
      long expectedReadSize ;  /* record length */
      double hitCountAdd ;
      const void *ownerPtr ;        /* points to DATA4FILE/INDEX4FILE which holds file handle */
   #else
      char  space1 ;
      char  space2 ;
      long  space3 ;
      FILE4LONG  space4 ;
      FILE4LONG_EXTEND space4a;
      char  space5 ;
      int   space7 ;
      long  space11 ;
      double space12 ;
      const void *space13 ;
   #endif

   #ifdef S4WRITE_DELAY
      LIST4 delayWriteFileList ;
   #else
      LIST4 space28 ;
   #endif

   #ifdef S4READ_ADVANCE
      int hasHadPendingAdvanceRead ;   /* if false then can avoid doing checks*/
      /* basically the hasPending member is used for files which never
         get advance-read.  We can cut improve efficiency several percent
         if we know that a file never needs to worry about advance reading.
         Of course, we can save even more if we can advance-read the file.
      */

      LIST4 advanceReadFileList ;
      char *advanceReadBuf ;        /* a buffer for advanced-reading */
      int advanceReadBufStatus ;    /* the status of the advance-read buffer */
                                    /* AR4EMPTY, AR4IN_USE, AR4FULL */
      // AS Apr 13/04 - move FILE4LONG higher in the file, it is referred to earlier now...
      FILE4LONG advanceReadBufPos ;
      unsigned advanceReadBufLen ;
   #else
      int space30a ;
      LIST4 space30b ;
      char *space30c ;
      int space30d ;
      FILE4LONG space30e ;
      unsigned space30f ;
   #endif

   #ifdef E4ANALYZE_ALL
      char dupName[L_tmpnam] ;
      int hasDup ;
      int inUse ;
   #else
      char space31 ;
      int space32 ;
      int space33 ;
   #endif

   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      short preprocessed ;     // true if the file is using preprocessing
   #else
      short space34 ;
   #endif
   // AS May 17/04 - server support for data file compression
   #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      // AS Jul 31/03 - support for generic handling of compression
      COMPRESS4HANDLER *compressInfo ;
      Bool5 freeCompressInfo ;  // AS Jan 16/03 - for generic file compression
      unsigned long compressHeaderOffset ;   // where does the compressed header info start
      long compressDataOffset ;     // where does the compressed data start
      char *compressBuffer ;        // need a buffer to store the compressed data
      char *uncompressBuffer ;      // need a buffer to uncompress the blocks into
      unsigned long actualBlockSize ;  // not modified by being a multiple of 1k
      // Aug 1/03 - support for large files
      FILE4LONG uBufFilePos ;  // what file position is associated with the compressed buffer
      long uBufFileLen ;  // and what length is that buffer?
      FILE4LONG physicalFileLen ;  // the actual physical file length of the compressed file (not the expanced size)
   #else
      void *space35 ;
      Bool5 space36 ;
      unsigned long space37 ;
      long space38 ;
      char *space39 ;
      char *space40 ;
      unsigned long space41 ;
      FILE4LONG space42 ;  // LY May 21/04 : changed from long to match above definition (sizeof(FILE4LONG) can be > sizeof(long))
      long space43 ;
      FILE4LONG space44 ;  // LY May 21/04 : changed from long to match above definition (sizeof(FILE4LONG) can be > sizeof(long))
   #endif

   #ifdef S4WIN32
      // AS Dec 11/02 - Renamed for clarification
      // all low-level reads and writes are protected through this critical section to ensure one thread is not reading the file
      // whilst another is writing to it.
      // for file write compression this is also used to ensure that only one thread is accessing the entire read/write sequence at
      // one time so that one user is not reading the compression information buffers while another user is writing to it.
      #ifdef S4DELAY_WRITE_MT
         CRITICAL4SECTION critical4file ;  // AS Sep 8/04 - changed for debugging support
      #else
         CRITICAL4SECTION space29 ;  // AS Sep 8/04 - changed for debugging support
      #endif
   #endif
   // AS Sep 3/03 - Was not properly closing temporary in-memory files.  Change here so we can track
   // when those files are being closed (in which case we do not want to create them or flush them to disk
   // at all)
   Bool5 closingTempFile ;

   // AS Nov 8/02 - sema4 locking for Windows CE
   #ifdef S4MUTEX4LOCK
      MUTEX4 lockMutex ;
      char lockMutexName[LEN4PATH] ;   // share name - based on file name but use .MUT extension
   #else
      MUTEX4 dummyMutex ;
      char dummyMutexName[LEN4PATH] ;
   #endif
#ifdef S4CBPP
} ;
#else
} FILE4 ;
#endif



#ifdef S4WRITE_DELAY
   typedef void S4PTR * S4DELAY_PARM ;
   typedef void S4CALL S4DELAY_FUNCTION( S4DELAY_PARM ) ;

   typedef struct
   {
      LINK4 link ;
      LINK4 fileLink ;    /* for the given file, all links on delay-chain */
      FILE4 *file ;
      const char *data ;
      FILE4LONG pos ;
      unsigned len ;
      int usageFlag ;
      int status ;

      S4DELAY_FUNCTION *completionRoutine ;
      void *completionData ;   /* routine-specific data for use in completion routine */
   } FILE4WRITE_DELAY ;
#endif



#ifdef S4READ_ADVANCE
   typedef void S4PTR * S4ADVANCE_PARM ;
   typedef void S4CALL S4ADVANCE_FUNCTION( S4ADVANCE_PARM ) ;

   typedef struct
   {
      LINK4 link ;
      LINK4 fileLink ;    /* for the given file, all links on delay-chain */
      FILE4 *file ;
      char *data ;
      // AS Apr 13/04 - support for optimizing loarge files
      FILE4LONG pos ;
      unsigned len ;
      int usageFlag ;
      unsigned status ;

      S4ADVANCE_FUNCTION *completionRoutine ;
      void *completionData ;   /* routine-specific data for use in completion routine */
   } FILE4ADVANCE_READ ;
#endif

/* (CS) In Win CE, structure members are char[4] instead of long.
   This is because of structure alignment on CE.
   Since CodeBase reads and writes these structures directly to/from disk,
   the structure members must be aligned on one-byte boundaries.
   To guarantee that this happens, all members are char. */
typedef struct
{
   #ifdef S4WINCE
      char year[4] ;
   #else
      S4LONG year ;
   #endif
   char month ;
   char day ;
   #ifdef S4WINCE
      char seconds[4]; //Seconds past midnight.
   #else
      unsigned S4LONG seconds ; //Seconds past midnight.
   #endif
}LOG4TIME ;

typedef struct
{
   /* structure must remain fixed positions */
   #ifdef S4WINCE    /* LY 00/03/10 */
      char transId[4] ;
      char clientId[4] ;
      char clientDataId[4] ;
      char serverDataId[4] ;
      short int type ;
      char dataLen[4] ;
   #else
      S4LONG transId ;
      S4LONG clientId ;
      S4LONG clientDataId ;
      S4LONG serverDataId ;
      short int type ;
      unsigned S4LONG dataLen ;
   #endif
   LOG4TIME time ;
} LOG4HEADER ;



typedef struct S4CLASS TRAN4St
{
   /* transaction capabilities for a client */
   struct CODE4TRANSSt *c4trans ;
   LIST4 *dataList ;        /* A list of open data files. */
   LIST4 localDataList ;
   char dateFormat[LEN4DATE_FORMAT];          /* Longest is September 15, 1990 */

   #ifndef S4OFF_WRITE
      int currentTranStatus ;  /* is there a current transaction? */
   #else
      int space1 ;
   #endif
   #ifndef S4OFF_MULTI
      short unlockAuto ;        /* automatic unlocking on or off? */
      short savedUnlockAuto ;
      SINGLE4DISTANT toLock ;
      SINGLE4 locks ;           /* list of Lock4's for to-lock */
   #else
      short space2 ;
      short space3 ;
      SINGLE4 space4 ;
   #endif

   #ifndef S4OFF_WRITE
      /* given transaction details */
      long       transId ;
      unsigned long pos ;       /* used during examination processing */
      LOG4HEADER header ;
      unsigned   dataPos ;
/*      char      *charId ;*/
   #else
      long       space6 ;
      long       space7 ;    /* used during examination processing */
      LOG4HEADER space8 ;
      char      *space9 ;    /* used during transaction writing and examination processing */
      unsigned   space10 ;
      unsigned   space11 ;
      char      *space12 ;
   #endif

   LIST4 closedDataFiles ;  /* during a transaction */

   #ifdef S4CLIENT
      int dataIdCount ;    /* client keeps an id on each data4 */
   #else
      char userId[LEN4TRANS_USERID+1] ;
      char netId[LEN4TRANS_NETID+1] ;
   #endif
} TRAN4 ;



// AS Mar 25/03 - Moved higher up to be available for CODE4TRANS
#if defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) )
   typedef struct
   {
      HANDLE sharedMemoryHandle ;
      int maxLen ;
      int wasU4alloc ;  // true if used u4alloc to allocate, false if used malloc
      FILE4 locker ;    // optionally used to lock/unlock
      Bool5 supportsLocking ;
      #ifdef E4ANALYZE
         Bool5 holdsLock ;  // should never be re-locking TRAN4LOCK_START_COMMIT if we hold the lock
      #endif
      // AS Apr 24/03 - Store the system's memory allocation granularity (previously just used '8'
      long allocationGranularity ;
   } SHARE4 ;
#endif



typedef struct S4CLASS CODE4TRANSSt
{
   #ifdef S4CBPP
      CODE4 S4PTR *c4 ;
   #else
      struct CODE4St *c4 ;
   #endif

   #ifndef S4OFF_TRAN
      int enabled ;
      #if !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
         struct TRAN4FILESt *transFile ;
      #else
         struct TRAN4FILESt *space1 ;
      #endif
   #else
      int space2 ;
      struct TRAN4FILESt *space1 ;
   #endif

   #ifndef S4SERVER
      TRAN4 trans ;
   #else
      TRAN4 space3 ;
   #endif
   #if defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) ) && !defined( S4OFF_TRAN ) && !defined( S4OFF_ODBC_TRANS )
      // AS Mar 25/03 - For ODBC c/s transactions
      SHARE4 *odbcTransShare ;
   #endif
} CODE4TRANS ;



enum Lock4type
{
   /* AS this should be ok with Borland because it is not included in any shared
      or exported structures */
   lock4read = 0,
   lock4write = 1,
   lock4any = 2
} ;



/*
AS 6/10.98 cannot be enumerator because Borland Builder mismatches the DLL
enum TRAN4FILE_STATUS
{
   tran4notRollbackOrCommit,
   tran4rollbackOrCommit
} ;
*/



#ifdef S4SERVER
   typedef struct TRAN4FILE_LOWSt
#else
   typedef struct TRAN4FILESt
#endif
{
   #ifndef S4SERVER
      long   transId ;
      int status ;
      unsigned long fileLocks ;   /* bit encoded, bit i == byte TRAN4LOCK_BASE+i locked */
      #if !defined( S4OFF_OPTIMIZE ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
         int needsFlushing ;
      #endif
   #endif
   long   validState ;    /* true/false switch */
   // AS July 12/01 Allow the primary log file to be disabled (backup logs still left intact if in use)
   // this just does a very low-level disable.  It prevents the log file from being created and written to
   // or read from, but everything else works as per normal.  In fact, transactions are still supported and
   // only fail if you try to roll them back.
   Bool5 isDisabled ;

   #ifdef S4WIN64 /* LY 00/10/09 : address misalignment of file */
//      DWORD spaceIA1 ;
//      WORD spaceIA2 ;
   #endif
   FILE4  file ;

   FILE4LONG fileLen ;   /* used to track file len to avoid costly file4len calls when possible */
   FILE4LONG_EXTEND pad;  /* pad size of FILE4LONG to constant size */

   // AS Apr 28/03 - made trans-shared a run-time switch
   // AS 11/22/99 moved from TRAN4, since this value is used by the transaction file,
   // not the TRAN4.  Note that this userIdNo is the user # of the shared transaction
   // file.  In the shared server implementation (SIMBA) multiple TRAN4 or clients may
   // use the transaction file, but they all use this base userIdNo to generate
   // their transaction id's.  See function tran4fileGetNextTransId for more details.
   int userIdNo ;    /* used to determine the id of user with a shared transaction file */

   CODE4TRANS *c4trans ;
   struct TRAN4FILESt *transFile ;
} TRAN4FILE_LOW ;



// AS 09/15/00 - a new structure wrapper for the trans4file stuff for auto-recovery
#ifdef S4SERVER
   typedef struct TRAN4FILESt
   {
      long transId ;
      int status ;
      unsigned long fileLocks ;   /* bit encoded, bit i == byte TRAN4LOCK_BASE+i locked */
      TRAN4FILE_LOW primary ;
      TRAN4FILE_LOW backup ;

      #if !defined( S4OFF_OPTIMIZE ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
         int needsFlushing ;
      #endif
      #ifdef S4SERVER
         // AS July 12/01 - In some instances we want to use the backup log file (namely when the primary is not available)
         Bool5 useBackup ;
      #endif
   } TRAN4FILE ;
#else
   typedef TRAN4FILE_LOW TRAN4FILE ;
#endif


#ifndef S4OFF_CATALOG
   typedef struct CATALOG4St
   {
      struct DATA4St *data ;
      int valid ;
      int catalogAdd ;
      int catalogStatus ;
      struct FIELD4St *ownerFld, *aliasFld, *pathNameFld, *typeFld, *readOnlyFld,
                       *indexOpenFld, *createFld, *openModeFld, *logFld ;
      struct TAG4St *aliasTag, *pathTag ;
   } CATALOG4 ;
#endif


// AS Apr 14/04 - the list4mutex is actually used in stand/alone as well, if E4ANALYZE is defined
#if !defined( S4STAND_ALONE ) || defined( E4ANALYZE )
   typedef struct LIST4MUTEXSt
   {
      LIST4 list ;
      HANDLE mutex ;
      char isValid ;
      #ifdef E4DEBUG
         Bool5 isHeld ;   // AS July 23/02 - allow to track this in debug mode
      #endif
   } LIST4MUTEX ;
#endif



#ifndef S4STAND_ALONE
   #ifdef S4MAC_TCP
      #define C4ADDRESS class LInternetAddress
      //CJ - There is really only one way to store IP addresses on Macintosh PowerPlant.
      #define C4NETID class LInternetAddress *
   #else
      #ifdef S4MACOT_TCP
         #define C4ADDRESS TCall
         #define C4NETID InetAddress
      #else
         #define C4ADDRESS struct sockaddr_in
         #define C4NETID struct in_addr
      #endif
   #endif
   #ifndef S4OFF_THREAD
      struct INTER4St ;
      struct SIGNAL4ROUTINESt ;

      typedef void S4CALL COMPLETION4ROUTINE( struct INTER4St *, struct SIGNAL4ROUTINESt * ) ;



      typedef struct SIGNAL4ROUTINESt
      {
         LINK4 link ;  /* Allows the SIGNAL4ROUTINE to be on a list */
         HANDLE handle ;
         /* routine to call when the handle is signalled
            the routine is called with the SIGNAL4ROUTINE parameter as the 2nd
            parameter. */
         COMPLETION4ROUTINE *routine ;
         void *data ;   /* a pointer to some additional data the routine may need */

         /* pointer to the signal structure associated with the routine.
            may be either an EVENT4 or a SEMAPHORE4 */
         void *signalStructure ;

         int inArray ; /* indicates whether or not this SIGNAL4ROUTINE is in an SIGNAL4ARRAY */
         int arrayPosition ; /* sometimes set to current position in array */
      } SIGNAL4ROUTINE ;



      typedef struct
      {
         HANDLE *handleArray;     /* the handle array to wait on */
         SIGNAL4ROUTINE **signalRoutineArray ; /* The signal array corresponding to the handle array */
         int numActiveElements ;
         int permenants ;
         int maxElements ;
      } SIGNAL4ROUTINE_ARRAY ;



      typedef struct
      {
         LINK4 link ;       /* allows the EVENT4 to be on a list of EVENT4s*/

         #ifdef S4WIN32
            HANDLE handle ; /* a handle to the event itself */
         #endif
      } EVENT4 ;



      typedef struct INTER4St
      {
         #ifdef S4CBPP
            CODE4 S4PTR *cb ;
         #else
            struct CODE4St *cb ;
         #endif
         /* A list of NET4MESSAGEs which worker threads have requested to be */
         /* asynchronously read, and for which the inter thread needs to */
         /* perform (but has nor yet performed) the asynchronous read */
         LIST4MUTEX readsRequested ;

         /* Semaphore count is equal to the number of reads requested ( i.e the */
         /* number of links in readRequested above ) */
         SEMAPHORE4 readsRequestedSemaphore ;

         /* readsRequestedRoutine is the routine structure which results in the */
         /* appropriate routine being calles every time the readsRequestedSemaphore*/
         /* above gets signalled */
         SIGNAL4ROUTINE readsRequestedRoutine ;

         /* a list of NET4MESSAGEs which worker threads have requested to be */
         /* asynchronously written, and for which the inter thread needs */
         /* to perform (but has not yet performed) the asynchronous write */
         LIST4MUTEX writesRequested ;

         /*Semaphore count is equal to the number of writes requested (i.e. the*/
         /*number of links in writesRequested above )  */
         SEMAPHORE4 writesRequestedSemaphore ;

         /* writesRequestedRoutine is the routine which results in the */
         /* appropriate routine being called every time the writesRequestedSemaphore */
         /* above gets signalled */
         SIGNAL4ROUTINE writesRequestedRoutine ;

         /* a list of CONNECT4THREADs which worker threads have finished servicing */
         /* and for which the communications thread needs to put back into the */
         /* pool of connections which are not being serviced by a worker thread */
         LIST4MUTEX completesRequested ;

         /* Semaphore count is equal to the number of completes requested (i.e the
            number of links in completesRequested); */
         SEMAPHORE4 completesRequestedSemaphore ;

         /* completesRequestedRoutine is the routine structure which results in the
            appropriate routine being called every time the
            completesRequestedSemaphore above gets signalled */
         SIGNAL4ROUTINE completesRequestedRoutine ;

         /* This event gets set when the inter thread is to be shut down
            i.e. called form code4initUndo() */
         EVENT4 shutdownThreadEvent ;

         /* routine to call when the thread should be shut down */
         SIGNAL4ROUTINE shutdownThreadRoutine ;

         /* the communications thread waits on all of the signals in the array.
            When signalled, the signalArray is used to directly call a function
            which was previously associated with the event. This is more
            optimal than using a switch statement to determine which function to
            call based on the signalled event or semaphore */
         SIGNAL4ROUTINE_ARRAY signalArray ;

         EVENT4 *finishedShutdownThreadPtr ;
         MEM4 S4PTR *signalRoutineMemory ; /* For SIGNAL4ROUTINES */
      } INTER4 ;
   #endif /*!S4OFF_THREAD */



   #ifdef S4WINSOCK
      typedef struct CONNECT4LOWSt
      {
         #ifndef CODE4UNAVAILABLE
            SOCKET sockw ;
            SOCKET sockr ;
            // struct sockaddr addressw ;  // set when needed to the address of the sockw socket
            C4ADDRESS addressw ;
            // AS 02/06/01 - method to disable dual sockets...
            // short useDualSocket ;
            #ifdef S4COMPRESS
               short compressLevel ;   // the compression level to use
            #endif
            short minLength ;       // minimum length before compression is considered
            #if defined( S4PREPROCESS_COM )
               #if defined( S4ENCRYPT_DLL ) || defined( S4DLL_BUILD_ENCRYPT )
                  void *preprocess ;
                  void *preprocessInit ;
                  short blockSize ;
               #else
                  PREPROCESS4 *preprocess ;
               #endif
            #endif
            #ifdef S4SERVER
               // AS Sept 30/02 - do internal tracking of the connection status as it gets connected
               // status array entreis are set to '1' as follows [0]:allocated [1]:accepted [2]:received 4 identifying bytes
               // [3]:2nd connection in server4clientAccept(), [4]:(1)found 1st connection in server4clientAccept()(2)did't find
               // [5]:set up sockw in accept, [6]:  // created new javaptr in accept
               // [7]:(1)found 1st connection in server4clientConnectNew()(2)did't find
               char serverConnectStatus[10] ;
               char serverConnectStatusSaved[10] ;   // for when we combine sockets, this is the one that was deleted
               // AS Dec 8/03 - Need to track if the connection is a java connection at the low level (to determine if length and compress/encrypt bytes are being sent)
               Bool5 javaClient ;
            #endif
            #ifdef S4CBPP  /* LY 2003/05/22 : last param of connect4lowAccept() either class or struct, but CONNECT4LOW.c4 declared only as struct */
               CODE4 *c4 ;      // required for dll encryption - AS Mar 12/03 - make generally available
            #else
               struct CODE4St *c4 ;      // required for dll encryption - AS Mar 12/03 - make generally available
            #endif
         #endif
      } CONNECT4LOW ;
   #endif



   #ifdef S4BERKSOCK
      typedef struct
      {
         int sockw ;
         int sockr ;
         C4ADDRESS addressw ;
      } CONNECT4LOW ;
   #endif

   #ifdef S4MAC_TCP
      typedef struct
      {
         Connection4macClient *sockw ;
         Connection4macClient *sockr ;
         C4ADDRESS *addressw ;
      } CONNECT4LOW ;
   #endif

   #ifdef S4MACOT_TCP
      typedef struct
      {
         EndpointRef sockw ;
         EndpointRef sockr ;
         C4ADDRESS *addressw ;
         unsigned short clientPortNum ;
      } CONNECT4LOW ;
   #endif

   typedef struct
   {
      unsigned char tcpAddress[4] ;
   } TCP4ADDRESS ;



   struct CONNECT4St ;
   struct CONNECT4BUFFERSt ;



   #ifndef S4OFF_THREAD
      typedef struct CONNECT4THREADSt
      {
         LINK4 link ;

         #ifdef S4CBPP
            CODE4 S4PTR *cb ;
         #else
            struct CODE4St *cb ;
         #endif
         INTER4 *inter ;/* pointer to interchange thread info */

         struct CONNECT4BUFFERSt *connectBuffer ;

         /*List of NET4MESSAGEs which ReadFile()s completed */
         LIST4MUTEX readMessageCompletedList ;

         /* Equal to # elements on readMessageCompletedList, released by the
            communications thread when a read request has been completed */
         SEMAPHORE4 readMessageCompletedListSemaphore ;

         /* writes in asynchronous WriteFile(), used usually by communications
            thread. Kept here in case a cancel is requested (eg. blastDone)
            list of outstanding SIGNAL4ROUTINEs */
         LIST4 writingSignalsList ;

         /*These next 2 lists are required in case we need to shut down the
           connection, so we can remove the events */

         LIST4 readSignalsList ;   /*list of SIGNAL4ROUTINES waiting for data arrival */
         /* List of NET4MESSAGES which could not be performed due to too many
            already outstanding messages on the socket */
         LIST4 writeMessageOutstandingList ;

         LIST4MUTEX writeMessageCompletedList ; /* writes completed */

         /* Set when interchange thread is finished servicing a
            com4threadWriteCompleted() (i.e. when a write is complete)
            semaphore count equal to number links in writeMessageCompletedList
            above */
         SEMAPHORE4 writeMessageCompletedListSemaphore ;

         /* Set when communications thread is finished
            inter4threadCompleteRequest() */
         SEMAPHORE4 completedSemaphore ;

      } CONNECT4THREAD ;

      typedef struct
      {
         LINK4 link ;
         #ifdef S4WIN32
            OVERLAPPED overlap ; /* also used as a LINK4 when on the avail list */
         #endif
         #ifdef E4ANALYZE
            Bool5 inUse ;       // flag used to track whether message is held by comm. thread
         #endif
         Bool5 independentAllocation ;   // true if this buffer was allocated independently of a chain (to handle encrpytion/decompression)
         CONNECT4THREAD *connectThread ;  /* Conenction associated with this message */
            /* message array length will be equal to CODE4.readMessageBufferLen,*/
            /* which is configurable in the server configuration file and will */
            /* have a #define default for the client (S4READ_MESSAGE_DEFAULT_LEN*/
         short messageBufferLen ;    /* Length of the buffer  */
         char *currentPtr ;                  /* points to the current spot in the message buffer */
         short messageLen ;

         // AS May 14/02 - Need to keep flag, messageLenInternal and message grouped together because we send
         // them both together as one message when sending data if compressed
         char flag ;  // the encrpytion/compression flag
         unsigned short messageLenInternal ;  // in network byte order
         char message[1] ;
      } NET4MESSAGE ;
   #endif /*!S4OFF_THREAD */



   #ifdef S4SERVER
      struct SERVER4CLIENTSt ;
   #endif



   typedef struct CONNECT4BUFFERSt
   {
      #ifndef CODE4UNAVAILABLE
         LINK4 link ;
         #ifdef S4CBPP
            CODE4 S4PTR *cb ;
         #else
            struct CODE4St *cb ;
         #endif

         #ifdef S4WIN32
            DWORD windowsErr ;   /* AS Sept 18/02 - track the actual windows error if overlap fails */
         #endif

         int connected ;              /* indicates # of connections (for client): 1 = 1 connection, 2 = 2 connections (both read and write sockets) */
         CONNECT4LOW *connectLowPtr ;   // AS Jan 21/02 - Rename to isolate out - this member must be reserved

         // AS Jan 21/02 - need to have exclusive access to the CONNECT4LOW structure in communications to avoid overwriting
         // ourselves and causing gpf's.
         #ifdef S4WINSOCK
            HANDLE lowAccessMutex ;
            int hasLowExclusive ;
         #endif

         short numFails ;  // AS Sept. 19/02 - track number of failures to ensure connection does not get stuck

         /* If CONNECT4 is null, it means that this CONNECT4THREAD is not */
         /* bound to a connection which requires a worker thread (i.e. it is an */
         /* auxillary connection) */
         struct CONNECT4St *connect ;

         int communicationError ;
         #ifdef S4OFF_THREAD
            char *workingWriteBuffer ;
            unsigned long workingWriteLen ;
            unsigned long workingWritePos ;
         #else
            CONNECT4THREAD connectThread ;

            int canShareWriteBuffers ;
            int maxWritesOutstanding ;
            int advanceReads ;
            int readSize ;
            short type ;
            short id ;

            NET4MESSAGE *workingReadMessage ;
            long workingReadMessagePos ;
            NET4MESSAGE *messageReadArray ;
            NET4MESSAGE *workingWriteMessage ;
         #endif
         #ifdef E4PARM_LOW
            #ifdef E4ANALYZE
               Bool5 isInitialized ;
            #endif
         #endif
         // AS Jan 23/04 - Support a suspension of performing the relate so other clients can perform actions
         Bool5 connectionFailed ;   // set to true if the connection is terminated during a long relation operation
      #endif
   } CONNECT4BUFFER ;



   typedef struct CONNECT4St
   {
      #ifndef CODE4UNAVAILABLE
         #ifdef S4SERVER
            LINK4 link ;     // for the c4->connectsToService list only
         #endif
         #ifdef S4CBPP
            CODE4 S4PTR *cb ;
         #else
            struct CODE4St *cb ;
         #endif

         /* sturcture used to communicate with the communications thread */
         CONNECT4BUFFER connectBuffer ;
         #ifndef S4OFF_BLAST
            #ifdef S4CLIENT
               short numBlasts ; /* Only client needs to know how many */
            #endif
            LIST4 blastList ; /* currently unused blast connections (CONNECT4BUFFER)*/
         #endif
         #ifdef S4SERVER
            struct SERVER4CLIENTSt *client ;
            int workState ; /* is this connection being serviced? How ?
               because  the connectthread is used to communicate with the
               intercommiunication thread, we need to track the workState at this
               level, since the communications thread lets us know when data
               is received */
         #else
         /* some identification method used by the client to identify the server
            side of the connection is required...
            just use the pointer from the server. Then on the server side,
            skip through a list of connections (to ensure memory integrity */
         /* void *id ; */
            //CJ July 17/01-C4ADDRESS in Mac Client is a base class need to have a pointer instead of an instance.
           #ifdef S4MAC_TCP
                   C4ADDRESS *address ;
           #else
                   C4ADDRESS address ;
            #endif
            short addrLen ;
            long clientId ;
         #endif
      #endif
      /* int connected ; */ /* Is this NOT in the process of disconnecting?*/
   } CONNECT4 ;



   typedef struct PACKET4St
   {
      short type ;
      short status ;   /* function rc */

      S4LONG int dataLen ;
      S4LONG errCode2 ;
      S4LONG clientDataId ;   /* which client-data4 id is being referred to? */
      S4LONG serverDataId ;   /* which server-data4 id is being referred to? */

      short readLock ;
      short unlockAuto ;
      // AS Aug 29/03 - Support for memSizeMemoExpr which can change at any time
      #ifndef S4STAMP_BUILD
         // AS Dec 1/06 - not available in old connections
         unsigned short memSizeMemoExpr ;
         char blanks[2] ;
      #endif
   /*   short requestLockedInfo ; */
   /*   short numRepeat ; */

      S4LONG delayHundredthsIn ;
   } PACKET4 ;



   typedef struct CONNECTION4St
   {
      LINK4 link ;

      CONNECT4 *connect ;

      int connected ;
      unsigned short dataLen ;

      short connectionFailure ;    /* true if a communications failure occurs */

      #ifdef S4CBPP
         CODE4 S4PTR *cb ;
      #else
         struct CODE4St *cb ;
      #endif

      PACKET4 packet ;

      char *buffer ;
      long bufferLen ;
      long curBufferLen ;

      #ifdef E4ANALYZE
         short initUndone ;
      #endif

      char serverName[128] ;
      char port[16];
      char userName[LEN4ACCOUNT_ID + 1];
      char password[LEN4PASSWORD + 1];
   } CONNECTION4 ;
#endif /* S4STAND_ALONE */



#ifdef S4SERVER
   class SERVER4 ;
#endif



#ifdef TIMER5OUT
   typedef struct
   {
      MEM4 *memTimer ;
   } TimerMemory5 ;

   typedef struct Timer5St
   {
      LINK4 link ;

      // *** startTime and stopTime must be on 8 byte boundaries to work under Windows NT
      LARGE_INTEGER startTime ;
      LARGE_INTEGER stopTime ;

      TimerMemory5 *mem ; // For memory allocation
      char *label ;
      LIST4 subTimers ;
      struct Timer5St *master ;
      int level ;
   } Timer5 ;
#endif /* TIMER5OUT */



enum index4format
{
   r4cdx =     200,
   r4mdx =     201,
   r4ntx =     202,
   r4unknown = 204

   /* r4ndx = 203  - not used anymore */
} ;



/* LY 00/08/21 : moved FIELD4INFO and TAG4INFO before CODE4 for WinCE CODE4 */
typedef struct  /* Creating Data File */
{
   char S4PTR *name ;
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


// AS Jan 23/03 - added support for runtime loading of dlls
#ifndef S4FUNCTION2
   #define S4FUNCTION2 __stdcall
#endif
// typedef int S4FUNCTION2 INT4PTR_PTR_PTR_ULONG_INT( void S4PTR *, unsigned long S4PTR *, const void S4PTR *, unsigned long, int ) ;
// typedef int S4FUNCTION2 INT4PTR_PTR_PTR_ULONG( void S4PTR *, unsigned long S4PTR *, const void S4PTR *, unsigned long ) ;
#ifdef S4ENCRYPT_DLL
   typedef int   S4FUNCTION2 INT4PTR( void S4PTR * ) ;
   typedef int   S4FUNCTION2 INT4PTR_CPTR_CPTR_PTR_PTRPTR_PTRPTR( void S4PTR *, const void S4PTR *, const void S4PTR *, void S4PTR *, void S4PTR * S4PTR *, void S4PTR * S4PTR * ) ;
   typedef int   S4FUNCTION2 INT4PTR_CPTR_SHORT_PTR_PTRPTR_PTRPTR( void S4PTR *, const void S4PTR *, short, void S4PTR *, void S4PTR * S4PTR *, void S4PTR * S4PTR * ) ;
   typedef int   S4FUNCTION2 INT4PTR_CPTR_CPTR( void S4PTR *, const void S4PTR *, const void S4PTR * ) ;
   typedef int   S4FUNCTION2 INT4PTR_INT_INT_PTR_PTRPTR_PTRPTR( void S4PTR *, int, int, void S4PTR *, void S4PTR * S4PTR *, void S4PTR * S4PTR *  ) ;
   typedef int   S4FUNCTION2 INT4PTR_PTR_INT( void S4PTR *, void S4PTR *, int ) ;
   typedef int   S4FUNCTION2 INT4PTR_PTR_INT_PTR( void S4PTR *, void S4PTR *, int, void S4PTR * ) ;
   typedef int   S4FUNCTION2 INT4PTR_PTR_PTR_PTRPTR_PTRPTR( void S4PTR *, void S4PTR *, void S4PTR *, void S4PTR * S4PTR *, void S4PTR * S4PTR * ) ;
   typedef int   S4FUNCTION2 INT4PTR_SHORT( void S4PTR *, short ) ;
   typedef short S4FUNCTION2 SHORT4PTR( void S4PTR * ) ;
   // typedef short S4FUNCTION2 SHORT4PTR_PTR( void S4PTR *, void S4PTR * ) ;
   typedef short S4FUNCTION2 SHORT4PTR_PTR_PTR( void S4PTR *, void S4PTR *, void S4PTR * ) ;
   typedef void *S4FUNCTION2 PTR4PTR_FILE4LONG_CPTR_ULONG( void S4PTR *, FILE4LONG, const void S4PTR *, unsigned long ) ;
   typedef void  S4FUNCTION2 VOID4PTR( void S4PTR * ) ;
   typedef void  S4FUNCTION2 VOID4PTR_SHORT( void S4PTR *, short ) ;
   typedef void  S4FUNCTION2 VOID4PTR_PTR_PTR_LONG_CPTR_LONG_PTR( void S4PTR *, void S4PTR *, void S4PTR *, long, const void S4PTR *, long, void S4PTR * ) ;

   typedef struct
   {
      void *preprocess ;
      void *preprocessInit ;
      HINSTANCE encryptDll ;
      // AS Sep 29/06 - at least for the ODBC server driver, we need the stand/alone version of the encryption...for odbc we only support file encryption
      // note that for odbc server driver we load up the standard encrypt4.dll, not encrypt4s.dll (I think)
      #if defined( S4SERVER ) && !defined( S4ODBC_BUILD )
         INT4PTR_PTR_PTR_PTRPTR_PTRPTR *encryptInit ;
      #else
         INT4PTR_CPTR_SHORT_PTR_PTRPTR_PTRPTR *encryptInit ;
      #endif
      // VOID4PTR_PTR_PTR_LONG_CPTR_LONG_PTR *encryptHook ;
      VOID4PTR_PTR_PTR_LONG_CPTR_LONG_PTR *decryptHook ;
      short blockSize ;
      #ifdef S4PREPROCESS_COM
         INT4PTR *encryptCodeInitUndo ;  // uninitialize at CODE4 level - general overhead
         VOID4PTR *encryptComInitUndo ;  // uninitialize the CON4LOW level (per connection, client and server)
         INT4PTR_CPTR_SHORT_PTR_PTRPTR_PTRPTR *encryptComInit ;      // initialize the CON4LOW level (per connection, client and server)
         INT4PTR_PTR_INT_PTR *conLowDecrypt ;
         INT4PTR_PTR_INT *conLowEncrypt ;
         SHORT4PTR *conLowGetEncryptRead ;
         VOID4PTR_SHORT *conLowSetEncryptRead ;
         SHORT4PTR *conLowGetEncryptWrite ;
         VOID4PTR_SHORT *conLowSetEncryptWrite ;
         #ifdef S4CLIENT
            INT4PTR_INT_INT_PTR_PTRPTR_PTRPTR *encryptConnection ;
         #endif
      #endif
      #ifdef S4CLIENT
         INT4PTR_CPTR_CPTR_PTR_PTRPTR_PTRPTR *serverEncryptInit ;
      #endif
      #ifdef S4PREPROCESS_FILE
         // short encryptFileFlag ;        // should the next open/create use encryption?
         VOID4PTR *encryptFileInitUndo ;
         SHORT4PTR *encryptFileGet ;
         INT4PTR_SHORT *encryptFileSet ;
         INT4PTR_SHORT *setEncryptFile ;
         SHORT4PTR_PTR_PTR *encryptFileHook ;
         PTR4PTR_FILE4LONG_CPTR_ULONG *filePreprocess ;
         VOID4PTR *filePreprocessDone ;
      #endif
   } ENCRYPT4DLL ;
#endif


#ifdef S4DLL_BUILD_ENCRYPT
   #define CODE4UNAVAILABLE
#endif



#ifdef S4LOCK_CHECK
   typedef struct
   {
      LINK4  link ;
      int     next ;
      int     prev ;
      HANDLE     hand ;
      long    startPos ;
      long    len ;
      long    endPos ;
   } L4LOCK_INFO ;
#endif
// #ifdef S4ODBC_BUILD
#ifdef S4WIN32 // LY Jul 16/04
   // AS Apr 4/01 - Made available in stand/alone for odbc stored procedures
   #ifndef S4FUNCTION2
      #define S4FUNCTION2 __stdcall
   #endif
   typedef int S4FUNCTION2 S4ADD_FUNCTION( long, const void S4PTR *, long, void S4PTR * S4PTR *, long S4PTR * ) ;
#endif

#ifdef S4CBPP
class S4CLASS CODE4
{
public:
#else
typedef struct CODE4St
{
#endif
   // CS 2007/02/21 This must always be at the top of the CODE4 structure. It is used to calculate the size of the struct.
   int code4top ;

   // AS Jan 24/03 - Allow code4 to be unavailable - we do this when building vb dll's, codecontrol dll's, encrypt dll's, etc.
   #ifndef CODE4UNAVAILABLE
      /* documented members... (except ifdef'ed ones) */

      S4CONV( int autoOpen, int auto_open ) ;               /* Automatic production index file opening */
      int createTemp ;                                      /* create files as temporary ? */
      S4CONV( short errDefaultUnique, short default_unique_error ) ; /* e4unique, r4unique, r4uniqueContinue */
      S4CONV( int errExpr, int expr_error ) ;
      S4CONV( int errFieldName, int field_name_error ) ;
      S4CONV( int errOff, int off_error ) ;                 /* Show error messages? */
      S4CONV( int errOpen, int open_error ) ;               /* Do 'file4open' error ? */
      // AS Dec 3/03 - we want the ability to set off the encryption error, in particular when we attempt to load it internally
      int errEncrypt ;
      short log ;
      S4CONV( int memExpandData, int mem_expand_data ) ;          /* Expanding data allocation */
      S4CONV( unsigned memSizeBuffer, unsigned mem_size_buffer ) ;/* Pack, Zap */
      S4CONV( unsigned memSizeSortBuffer, unsigned mem_size_sort_buffer ) ; /* The default file buffer size when sorting */
      S4CONV( unsigned memSizeSortPool, unsigned mem_size_sort_pool ) ;   /* The default pool size for sorting */
      S4CONV( unsigned memStartData, unsigned mem_start_data ) ;      /* Initial data allocation */
      char safety ;                                          /* File create with safety ? */
      long timeout ;
      short compatibility ;                                   /* FoxPro compatibility with... 25, 26, 30 */
      #if defined( S4WINDOWS ) && !defined( S4CODE_SCREENS )
         HWND hWnd;
      #else
         long dummyHwnd ;  // support for structure size matching...
      #endif

      /* undocumented members... */

      int s4cr2 ;                                            /* used by CodeReporter */

      int initialized ;                                      /* only allow a single initUndo */
      int numericStrLen ;                                    /* the default length for clipper index files */
      int decimals ;                                         /* the default # decimals for clipper index files */

      int memExpandDataFile ;
      unsigned memStartDataFile ;

      unsigned storedKeyLen ;      /* for storedKeu */
      unsigned bufLen ;            /* for fieldBuffer */
      // AS Nov 8/04 - fix general collation bug...
      unsigned bufLen2 ;            /* for fieldBuffer2 */
      // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - use the internal CODE4 buffers instead...
      unsigned tagAddBufLen ;
      char *tagAddBuf ;
      unsigned tagAddBufLen2 ;
      char *tagAddBuf2 ;

      /* AS 02/23/99 cannot use a shared work buf because (at least in server) may evaluation
         an expression within an evaluation if an expression includes a calc.  Therefore, sharing
         this memory does not work.  Must keep the exprWorkBuf on an EXPR4 level

         unsigned exprBufLen ;        - for exprWorkBuf
         char S4PTR *exprWorkBuf ;    - used by expression parsing
      */
      char S4PTR *storedKey ;      /* used by the expr4key() function */
      char S4PTR *fieldBuffer ;
      // AS Nov 8/04 - fix general collation bug...
      char S4PTR *fieldBuffer2 ;

      char indexExtension[4] ;
      char memoExtension[4] ;

      MEM4 S4PTR *bitmapMemory ;
      MEM4 S4PTR *dataMemory ;
      S4CONV( MEM4 S4PTR *calcMemory, MEM4 S4PTR *calc_memory ) ;
      MEM4 S4PTR *data4fileMemory ;
      MEM4 S4PTR *relateDataListMemory ;  /* for R4DATA_LIST */
      MEM4 S4PTR *relateListMemory ;  /* for RELATE4LIST */
      MEM4 S4PTR *relateMemory ;
      MEM4 S4PTR *relationMemory ;
      MEM4 S4PTR *dataListMemory ;  /* for DATA4LIST */

      LIST4 dataFileList ;         /* A list of open data files. */

      FILE4 *errorLog ;             /* log file for error messages */

      short collatingSequence ;   /* the collating sequence to use when creating a VFP tag */
      short codePage ;            /* the codepage to use when creating a database file */

      int didAlloc ;  /* true if code4alloc() called, in which case we auto-free it */

      // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE
      char limitKeySize ;
      char oledbSchemaCreate ;
      short useGeneralTagsInRelate ;  /* used to affect whether can use optimization on general tags (is faster but inconsistent results) */ /* CS 2000/10/09 Should be short */

      #ifdef S4SERVER
         /* for the server, the values may not be available as they reside in
            the SERVER4CLIENT structure.  Use defaults.  Also use defaults to
            set SERVER4CLIENT values. */
         int readOnlyDefault ;
         int errorCodeDefault ;   /* used for when no client struct only */
         long errorCode2Default ; /* used for when no client struct only */
         /* AS June 05/01 - need more defaults */
         int errCreateDefault ;  // default is 0, so this is not initialized in code4init
         int readLockDefault ;  // defalut is 0, so this is not initialized in code4init
      #else
         S4CONV( int errCreate, int create_error ) ;           /* Do 'file4create' error ? */
         S4CONV( int errorCode, int error_code ) ;
         S4CONV( int readOnly, int read_only ) ;
         long errorCode2 ;
      #endif

      #ifdef S4COMPILE_TEST
         #ifndef S4OFF_MULTI
            S4CONV( char accessMode, char exclusive ) ;             /* how should files be opened? */
            int memExpandLock ;       /* Expanding lock memory allocation */
            unsigned memStartLock ;   /* Initial lock memory allocation record locks */
            int lockAttemptsSingle ;                               /* How many times to attempt each lock in a group lock */
            unsigned int lockDelay ;
            int fileFlush ;  /* force hard file flush during write */
            #ifndef S4SERVER
               S4CONV( char readLock, char read_lock ) ;               /* Do lock when reading database ? */
            #endif
            S4CONV( int lockAttempts, int lock_attempts ) ;        /* How many times to attempt locks. */
            MEM4 S4PTR *lockMemory ;
            MEM4 S4PTR *lockGroupMemory ;
            MEM4 S4PTR *lockLinkMemory ;
         #endif
         #ifndef S4OFF_OPTIMIZE
            S4CONV( int optimizeWrite, int optimize_write ) ;
            S4CONV( unsigned memSizeBlock, unsigned mem_size_block ) ;  /* Block size (bytes) for memo and index files */
            int optimize ;            /* should files be automatically bufferred? */
            int memMaxPercent ;
            S4CONV( long memStartMax, long mem_start_max ) ;
         #endif
         #ifndef S4OFF_INDEX
            #ifdef S4OFF_OPTIMIZE
               S4CONV( unsigned memSizeBlock, unsigned mem_size_block ) ;  /* Block size (bytes) for memo and index files */
            #endif
            S4CONV( int errTagName, int tag_name_error ) ;
            S4CONV( int memExpandBlock, int mem_expand_block ) ;        /* Expanding block memory allocation */
            S4CONV( int memExpandIndex, int mem_expand_index ) ;        /* Expanding index allocation */
            S4CONV( int memExpandTag, int mem_expand_tag ) ;            /* Expanding tag allocation */
            S4CONV( unsigned memStartBlock, unsigned mem_start_block ) ;     /* Initial block memory allocation for index files */
            S4CONV( unsigned memStartIndex, unsigned mem_start_index ) ;     /* Initial index file allocation */
            S4CONV( unsigned memStartTag, unsigned mem_start_tag ) ;       /* Initial tag allocation */
            int memExpandTagFile ;
            unsigned memStartTagFile ;
            MEM4 S4PTR *indexMemory ;
            MEM4 S4PTR *index4fileMemory ;
            MEM4 S4PTR *tagMemory ;
            MEM4 S4PTR *tagFileMemory ;
         #endif
         #ifndef S4OFF_MEMO
            S4CONV( unsigned memSizeMemo, unsigned mem_size_memo ) ;
            #ifndef S4SERVER
               // AS Aug 29/03 - This value is stored on a client by client basis
               S4CONV( unsigned memSizeMemoExpr, unsigned mem_size_memo_expr ) ;
            #endif
         #endif
         #ifndef S4OFF_TRAN
            int doTrans ;                                         /* Maintain a transaction file? */
         #endif
      #else
         S4CONV( char accessMode, char exclusive ) ;             /* how should files be opened? */
         int memExpandLock ;       /* Expanding lock memory allocation */
         unsigned memStartLock ;   /* Initial lock memory allocation record locks */
         int lockAttemptsSingle ;                               /* How many times to attempt each lock in a group lock */
         unsigned int lockDelay ;
         int fileFlush ;  /* force hard file flush during write */
         #ifndef S4SERVER
            S4CONV( char readLock, char read_lock ) ;               /* Do lock when reading database ? */
         #endif
         S4CONV( int lockAttempts, int lock_attempts ) ;        /* How many times to attempt locks. */
         MEM4 S4PTR *lockMemory ;
         MEM4 S4PTR *lockGroupMemory ;
         MEM4 S4PTR *lockLinkMemory ;
         S4CONV( int optimizeWrite, int optimize_write ) ;
         int optimize ;            /* should files be automatically bufferred? */
         int memMaxPercent ;
         S4CONV( long memStartMax, long mem_start_max ) ;
         S4CONV( unsigned memSizeBlock, unsigned mem_size_block ) ;  /* Block size (bytes) for memo and index files */
         S4CONV( int errTagName, int tag_name_error ) ;
         S4CONV( int memExpandBlock, int mem_expand_block ) ;        /* Expanding block memory allocation */
         S4CONV( int memExpandIndex, int mem_expand_index ) ;        /* Expanding index allocation */
         S4CONV( int memExpandTag, int mem_expand_tag ) ;            /* Expanding tag allocation */
         S4CONV( unsigned memStartBlock, unsigned mem_start_block ) ;     /* Initial block memory allocation for index files */
         S4CONV( unsigned memStartIndex, unsigned mem_start_index ) ;     /* Initial index file allocation */
         S4CONV( unsigned memStartTag, unsigned mem_start_tag ) ;       /* Initial tag allocation */
         int memExpandTagFile ;
         unsigned memStartTagFile ;
         #ifdef S4WIN64 /* LY 00/10/18 */
            //         DWORD spaceIA6 ;
            //         WORD spaceIA7 ;
         #endif
         MEM4 S4PTR *indexMemory ;
         MEM4 S4PTR *index4fileMemory ;
         MEM4 S4PTR *tagMemory ;
         MEM4 S4PTR *tagFileMemory ;
         S4CONV( unsigned memSizeMemo, unsigned mem_size_memo ) ;
         #ifndef S4SERVER
            // AS Aug 29/03 - This value is stored on a client by client basis
            S4CONV( unsigned memSizeMemoExpr, unsigned mem_size_memo_expr ) ;
         #endif
         int doTrans ;                                         /* Maintain a transaction file? */
      #endif

      #ifdef S4MACINTOSH
         long macDir ;
         int macVol ;
      #endif

      #ifdef S4CLIENT
         char serverName[S4SERVER_NAME_SIZE] ;  /* name of server client is connected to */
      #endif

      #if defined(E4ANALYZE) || defined(E4VBASIC)  // CS 2000/10/10
         int debugInt ;               /* used to check structure integrity (set to 0x5281) */
      #else
         int space_debugInt ;  // CS 2000/08/07 always define to preserve space
      #endif


      /*-----------------------------------------------------------------------------------------------------------------------
         All above members should be index independent...

        -----------------------------------------------------------------------------------------------------------------------*/

      #ifndef S4CLIPPER
         unsigned memStartIndexFile ; /* Initial index file allocation */
         int memExpandIndexFile ;  /* Expanding index file allocation */
      #endif

      #ifndef S4DOS
         #ifdef S4WIN32
            HINSTANCE hInst ;
            HINSTANCE hInstLocal ;
         #else
            unsigned hInst ;
            unsigned hInstLocal ;
         #endif
   /*      char protocol[LEN4PROTOCOL+1] ; */
      #endif

      #if defined( S4CLIENT_OR_FOX ) && !defined( S4OFF_INDEX )
         unsigned int foxCreateIndexBlockSize ;    // block size to use for fox indexes, must be multiple of 512
         unsigned int foxCreateIndexMultiplier ;        // multiplier to use for fox indexes, must be divisible into foxCreateIndexBlockSize
      #else
         unsigned int space_foxCreateIndexBlockSize ;
         unsigned int space_foxCreateIndexMultiplier ;
      #endif

      // AS Aug 14/01, in server case, the trans name is stored in the server->logName.
      #if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) && !defined( S4SERVER )
         char *transFileName ;
      #else
         char *space_transFileName ;
      #endif

      #ifndef S4OFF_TRAN
         // AS Feb 5/03 - For ODBC transactions, allow the file to be opened exclusive.  This is a requirement
         // to allow multiple transactions at one time (we will then cycle through a list of log file names).
         #ifdef S4STAND_ALONE
            char logAccessMode ;
         #endif
         #ifdef S4WIN64 /* LY 00/10/09 : address misalignment of tranData */
            //         DWORD spaceIA ;
            //         WORD spaceIA0 ;
         #endif
         char *tranData ;   /* buffer used to hold transaction entries */
         unsigned int tranDataLen ;  /* length of transaction entry buffer */
         #ifdef S4WIN64 /* LY 00/10/18 : address misalignment of transactionTime */
            //         DWORD spaceIA1 ;
         #endif
         LOG4TIME transactionTime ;
      #endif

      #ifdef S4MNDX
         char *memoUseBuffer ;
      #endif

      #ifndef S4OFF_OPTIMIZE
         OPT4 opt ;
         int hasOpt, doOpt, hadOpt ;
      #endif

      #if defined( S4STAND_ALONE ) && !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
         TRAN4FILE transFile ;
      #endif

      #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
         // AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
         int logOpen ;
      #endif

      #ifdef S4WIN32
         #ifdef S4WRITE_DELAY
            int delayWritesEnabled ;
            Bool5 delayWritesDisabled ;  // AS Nov 21/05 - due to an apparent vb/xp sequencing problem, use this to indicate we were unable
                                         // to get the writes up and going (avoids a later incorrect initialization)
            #ifdef S4WIN64 /* LY 00/10/06 : address misalignment in code4initLow() */
               // WORD spaceIA3 ;
            #endif
            CRITICAL4SECTION critical4delayWriteList ;  // AS Sep 8/04 - changed for debugging support
            LIST4 delayWriteList ;
            #ifdef S4WIN64 /* LY 00/10/06 : address misalignment in code4initLow() */
               // DWORD spaceIA4 ;
            #endif
            MEM4 S4PTR *delayWriteMemory ;
            long uninitializeDelayWrite ;   /* request to uninitialize delay-writes */
            HANDLE initUndoDelayWrite ;    /* event for delay-write thread to notify main
                                             thread that uninitialization is complete */
            HANDLE pendingWriteEvent ;    /* event to notify delay-write thread that
                                             there is an action to perform */
         #else
            int space1a ;
            #ifdef S4WIN64
               // WORD spaceIA3 ;
            #endif
            CRITICAL4SECTION space1b ;  // AS Sep 8/04 - changed for debugging support
            LIST4 space1c ;
            #ifdef S4WIN64
               // DWORD spaceIA4 ;
            #endif
            MEM4 S4PTR *space1d ;
            long space1e ;
            HANDLE space1f ;
            HANDLE space1g ;
         #endif
      #endif
      long largeFileOffset ;

      #ifdef S4WIN32
         #ifdef S4READ_ADVANCE
            int advanceReadsEnabled ;
            Bool5 advanceReadsDisabled ;  // AS Nov 21/05 - due to an apparent vb/xp sequencing problem, use this to indicate we were unable
                                         // to get the writes up and going (avoids a later incorrect initialization)
            #ifdef S4WIN64 /* LY 00/10/06 : address misalignment in code4initLow() */
               // DWORD spaceIA5 ;
            #endif
            CRITICAL4SECTION critical4advanceReadList ;  // AS Sep 8/04 - changed for debugging support
            LIST4 advanceReadList ;
            MEM4 S4PTR *advanceReadMemory ;
            HANDLE initUndoAdvanceRead ;
            long uninitializeAdvanceRead ;
            HANDLE pendingReadEvent ;
         #else
            int space2a ;
            #ifdef S4WIN64
               // DWORD spaceIA5 ;
            #endif
            CRITICAL4SECTION space2b ;  // AS Sep 8/04 - changed for debugging support
            LIST4 space2c ;
            MEM4 S4PTR *space2d ;
            HANDLE space2e ;
            long space2f ;
            HANDLE space2g ;
         #endif
      #endif
      // AS Apr 8/03 - odbc trans support
      // AS Apr 28/03 - made trans-shared a run-time switch
      #if ( defined( S4STAND_ALONE ) && ! defined( S4OFF_TRAN ) ) || ( defined( S4SERVER ) && (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD )))
         /* allow sequential handling of transaction file by not doing locking i.e. for same program to have multiple transactions */
         int doTransLocking ;
      #endif

      #ifdef S4SERVER
         int singleClient ;
         int displayErrors ;
         int readOnlyRequest ;
         unsigned memStartClient ;
         unsigned memExpandClient ;
         struct SERVER4CLIENTSt S4PTR *catalogClient ;
         MEM4 S4PTR *clientMemory ;
         SERVER4 *server ;
         struct SERVER4CLIENTSt S4PTR *currentClient ;
         long maxSockets ;

         #ifdef S4DEBUG_LOG
            char S4PTR *logFileName ;
            FILE4 logFile ;
            int log ;
         #endif
         #ifndef S4OFF_SECURITY
            int idhandling ;
         #endif
         Bool5 runAsService ;   // set to true if the server is run as a service.  Disables message boxes
      #endif

      int errUnlock ;   // AS Mar 26/03 added for odbc trans.

      #ifndef S4SERVER
         // AS Apr 30/02 - Allow for auto-freeing of relates on code4initUndo
         LIST4 relations ;
         // in server, doRemove is on a client by client basis, so don't use this here
         int doRemove ;
         /* singleOpen can take 0,1, or 2, but only 0 and 1 are documented -OPEN4 modes */
         short singleOpen ;      /* only one instance of a DATA4 allowed (as opposed to exclusive which is access for client) */
         S4CONV( int errGo, int go_error ) ;                   /* Do 'd4go' error ? */
         S4CONV( int errRelate, int relate_error ) ;         /* do relate4terminate error when no match and relate4terminate selected */
         S4CONV( int errSkip, int skip_error ) ;           /* Do 'DataIndex::skip' error ? */
         int lockEnforce ;         /* are pre-locks required for record modifications? */

         #ifndef S4OFF_MULTI
            char *lockedNetName ;
            char *lockedNetNameBuf ;
            char *lockedUserName ;
            char *lockedUserNameBuf ;
            #ifdef S4CLIENT
               char *lockedFileName ;
            #else
               const char *lockedFileName ;
            #endif
            char *lockedFileNameBuf ;
            unsigned int lockedFileNameLen ;
            unsigned int lockedUserNameLen ;
            unsigned int lockedNetNameLen ;
            long lockedLockItem ;
         #else
            char *space_lockedNetName ;
            char *space_lockedNetNameBuf ;
            char *space_lockedUserName ;
            char *space_lockedUserNameBuf ;
            #ifdef S4CLIENT
               char *space_lockedFileName ;
            #else
               const char *space_lockedFileName ;
            #endif
            char *space_lockedFileNameBuf ;
            unsigned int space_lockedFileNameLen ;
            unsigned int space_lockedUserNameLen ;
            unsigned int space_lockedNetNameLen ;
            long space_lockedLockItem ;
         #endif

         // AS Sept. 25/02 - need a CODE4TRANS even if S4OFF_TRAN is defined, for unlock handling
         CODE4TRANS c4trans ;
         S4CONV( MEM4 S4PTR *totalMemory, MEM4 S4PTR *total_memory ) ;
         S4CONV( LIST4 calcList, LIST4 calc_list ) ;
         S4CONV( LIST4 totalList, LIST4 total_list ) ;
         S4CONV( int numReports, int num_reports ) ;
         short pageno ;
         long clientDataCount ;
      #endif

      // AS Nov 27/02 - need to track open-for-create for data file compression (do we initialize)
      short openForCreate ;
      #ifdef S4CLIENT
         enum index4format indexFormat ;
         long serverOS ;   // bit mask: OS4UNKNOWN, OS4WIN32, OS4LINUX...
      #else
         int doIndexVerify ;       /* for internal purposes only at this point */
         struct RELATION4St S4PTR *currentRelation ;

         // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - make dynamic
         char *savedKey ;  // [I4MAX_KEY_SIZE + 2 * sizeof(S4LONG)] ;       /* used by i4remove.c, i4tag.c and i4addtag.c, i4versionCheck, t4versionCheck */
         unsigned savedKeyLen ;
      #endif

      #ifdef S4OPTIMIZE_STATS
         struct DATA4St *statusDbf ;
         struct FIELD4St *typeFld, *fileNameFld, *offsetFld, *lengthFld ;
      #endif

      #ifndef S4STAND_ALONE
         MEM4 S4PTR *connectBufferMemory ;
         MEM4 S4PTR *connectLowMemory ;
         MEM4 S4PTR *writeMemory ; /*Memory for NET4MESSAGE for writes Memory */
         #ifdef S4DISTRIBUTED
            LIST4 servers ;       /* list of connected servers */
         #endif

         #ifdef S4CLIENT
            CONNECTION4 defaultServer ;
            CONNECT4 clientConnect ;
            char *savedData ;
            unsigned savedDataLen ;
            // AS May 13/02 - communications compression coding
            Bool5 doCompression;   // true if communications data is being compressed (server drives this)
         #endif
         int ver4 ;

         #ifndef S4OFF_THREAD
            LIST4 eventsAvail ;        /* List of available EVENT4 objects */
            MEM4 S4PTR *eventMemory ;
         #endif

         #ifdef S4SERVER
            int shutdownWorkerThreads ;
            int numWorkerThreads ;     /* configurable */
            /*pointer to an array of threadHandles whose size is equal to */
            /* sizeof(unsigned long)* CODE4.numWorkerThreads */
            unsigned long *workerThreadHandles ;

            /* must be long for interlocked functions */
            long numWaitingWorkerThreads ;  /* of the worker threads, the # waiting to service a client */

            #ifndef S4OFF_THREAD
               LIST4MUTEX connectsToService ;   /* A list of CONNECT4's to service */
               /* Semaphore that worker threads wait on */
               /* Count equal to number of outstanding messages needing servicing */

               SEMAPHORE4 workerSemaphore ;
               // LIST4MUTEX connectListMutex ;   /*List of CONNECT4s Unused */
            #endif

            HANDLE accessMutex ;
            #ifndef S4ODBC_STAND
               int accessMutexCount ;
            #endif
            MEM4 S4PTR *connectHalfMemory ;
            #ifdef S4OFF_THREAD
               LIST4 connectHalfList ;
            #else
               LIST4MUTEX connectHalfListMutex ;
            #endif
         #endif

         int readMessageBufferLen ;  /*Buffer size for communications reading */
         int readMessageNumBuffers ; /*# of buffers per connection for reading*/
         int writeMessageNumOutstanding ; /*# of outstanding writes per connection*/
         int writeMessageBufferLen ; /*buffer size for communications writing */

         #ifndef S4OFF_THREAD
            LIST4MUTEX writeBuffersAvail ; /* list of unused NET4MESSAG for writes */
            LIST4MUTEX connectBufferListMutex ;  /* list of CONNECT4BUFFERs */
         #endif

         #ifdef S4COMM_THREAD
            INTER4 *inter ;
            unsigned long interThreadHandle ;
         #endif
      #endif
      char skipCom ;  /* used for debugging purposes and the ats system */
      /*CJ- made change to facilate security issues for distribution of CodeBase DLLs */
      char *applicationVerify;

      #ifdef S4CLIENT_OR_FOX
         enum Collate4name collateName ;  // which collate to use on next create... collate4none as default or to use old method
         enum Collate4name collateNameUnicode ;
         int codeCollateSetValue ;   // may be collate4general, etc.
         int codeCollateSetValueUnicode ;   // may be collate4general, etc.
      #endif

      #ifdef S4STAND_ALONE
         // AS 05/08/00 -- needed another member to control reccounts for ODBC purposes.
         Bool5 minCountOff ;  // true if don't get/set min count with DATA4, but use the DATA4FILE value instead (ODBC)
         // if set to true then CodeBase will record when file times have been updated to disk
         // it gets set to true if d4versionNumber is ever called for the CODE4 (in the server case if anybody
         // ever requests it)
         // don't do this in server case since we track all the changes to the file, and if others are accessing
         // we can only rely on the system to update their times
         Bool5 updateFileTime ;
      #else
         #if !defined(S4OFF_THREAD) && defined(S4COMPRESS) // CS 2011/05/05
            // AS Jun 18/02 - to handle compression...
            NET4MESSAGE *extraReadMessage ;
            Bool5 extraReadMessageAvail ;
         #endif
      #endif

      // AS Jan 9/02 - code written for use via OLE-DB to timeout on accepting a connection (was previously a define
      // set to 300 seconds).
      #if defined( S4WIN32 ) && !defined( S4OFF_THREAD ) && defined( S4CLIENT )
         long acceptTimeOut ;   // amount of time to wait for server to accept our connection before timing out.
      #endif

      #ifdef TIMER5OUT
         /* case where we want to perform timing of server operations */
         FILE4 timerFile ;
         TimerMemory5 *timerMemory ;
         Timer5 *currentTimer ;
         Timer5 *masterTimer ;
      #endif

      #if defined( S4WIN32 )
         #if defined(__cplusplus) && defined(OLEDB5BUILD)
            Mem5zeroAllocator *memZeroAllocator ;
            Mem5allocator *memNonZeroAllocator ;
         #else
            void *mem1;
            void *mem2;
         #endif
      #endif

      #if defined( S4ODBC_ENABLED ) && defined( S4SERVER )
         Bool5 odbcEngineStarted ;   // are the ODBC services enabled?
         int odbcEngineProcessId ;   // process id for the ODBC services - used to verify that the service is still up and running...
      #endif

      #ifdef EXCEPTION4REINDEX_TEST
         Bool5 doGpf ;   // sometimes force a gpf for test purposes
      #endif

      #ifdef TIME4STATUS
         // to determine status:  percentDone + ( incrementWeight * incrementVal / incrementMax )
         unsigned short actionCode ;  // what action is being performed?  0 = none, 1 = reindex
         double percentDone ;         // what is the 0.0 - 1.0 percentage completion of the task
         double incrementWeight ;
         long incrementVal ;
         long incrementMax ;
         // AS Feb 9/06 - added clipper support for packwithstatus
         #if defined( TIME4STATUS ) && defined( S4WIN32 ) && !defined( S4OFF_WRITE )
            // AS Jun 30/03 - test d4reindexWithProgress
            // AS Mar 25/04 - requires change for C++ build
            struct REINDEX4STATUSSt *packStatus ;  // if set we are doing a pack status, not a reindex status
         #endif
      #endif

      #ifdef S4CLIENT
         // AS 08/22/00 - code for client to client messaging
         void *recvMessage ;
         unsigned long recvMessageLen ;
         // short msgWaitForReceipt ;
         // short msgTimeout ;
         // short msgIncludeSelf ;
      #endif

      short ignoreCase ;
      short attemptUpgrade ;
      double autoIncrementStart ;

      #if defined(S4WINCE) && defined(E4VBASIC)
         FIELD4INFO *fldInfo ;
         int fldInfoSize ;
         unsigned fldInfoLength ;

         TAG4INFO *tagInfo ;
         int tagInfoSize ;
         unsigned tagInfoLength ;
         char *tagExpr ;
         char *tagFilter ;
      #endif

      // AS Jan 17/03 - Required in client-com for general settings before connecting
      // #if defined( S4PREPROCESS_FILE ) || ( defined( S4PREPROCESS_COM ) && defined ( S4CLIENT ) ) && !defined( S4ENCRYPT_DLL )
      //    PREPROCESS4 preprocess ;
      // #endif

      #if defined( S4PREPROCESS_COM )
         void *preprocessContext ;
      #endif

      #if defined( S4PREPROCESS_FILE  ) // || defined( S4PREPROCESS_COM )
         // AS Nov 20/02 - track for preprocessing
         #ifdef S4STAND_ALONE
            Bool5 fileAccessed ;
         #endif
         // AS Nov 20/02 - to track critical section
         #ifdef E4ANALYZE
            long preCScount ;
         #else
            long dummycount ;  // AS Aug 11/05 - ensure here so that CODE4 structure will continue to match up (required for testing odbc)
         #endif
         void *filePreprocessBuffer ;
         unsigned long filePreprocessBufferLen ;
      #endif

      // AS July 12/01 Allow the primary log file to be disabled (backup logs still left intact if in use)
      // this just does a very low-level disable.  It prevents the log file from being created and written to
      // or read from, but everything else works as per normal.  In fact, transactions are still supported and
      // only fail if you try to roll them back.
      Bool5 logDisable ;   // can == log4enabled, log4disabled, log4tempEnabled, log4compressed, log4tempUncompressed

      ERROR_CALLBACK errorCallback;  // CS 2001/03/26 error callback function pointer
      char *lastErrorDescription;  // CS 2002/04/24
      #ifndef S4STAND_ALONE
         // AS Sept 26/02 - new ability to log connection info into log file
         short logConn ;  // Set to level to log connection connects and disconnects: 0 = none, 9 = all, 1 = errors only, >5 = disconnect info
         #ifdef S4SERVER
            // AS May 30/03 - enable/disable log flushing - default is enabled. - improves performance (esp. ODBC), maybe cannot recover from crash.
            short logFlush ;
         #endif
         #ifdef S4CLIENT
            // special log file for this info
            FILE4 connLogFile ;
            Bool5 connLogFileOpen ;
         #endif
      #endif

      #if defined( S4CLIENT_OR_FOX ) && defined( S4COMPRESS )
         // AS June 19/02 - added support for memo field compression
         Bool5 compressedMemos ; // if set to true, foxpro-compatible tables will be created with compressed memo fields
         void *compressedMemosBuffer ;
         unsigned long compressedMemosBufferLen ;

         // AS Nov 26/02 - added support for data file compression
         Bool5 compressedData ;  // if set to true, foxpro-compatible tables will be created with compressed data files
         // HINSTANCE zlibDll ;
         // INT4PTR_PTR_PTR_ULONG_INT *compress ;
         // INT4PTR_PTR_PTR_ULONG *uncompress ;
      #endif

      // AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
      #if !defined( S4CLIENT )
         /* AS Sept 14/01 new functionality for validating the tables when CODE4 is initialized */
         struct DATA4St *validationTable ;
      #endif
      Bool5 odbc ;  // AS Feb 7/03 - New flag to indicate if is odbc at runtime.
      #if ( defined( S4TESTING ) || defined( E4ANALYZE ) ) && !defined( S4SERVER )
         // AS Jun 24/02 - to perform data movement counting for testing
         #ifdef S4CLIENT
            unsigned long expectedCount ;
         #else
            unsigned long testCount ;
         #endif
      #else
         unsigned long countDummy ;  // AS Aug 12/05 - keep same size...
      #endif

      // AS Jan 23/03 - Support for run-time loading of encryption
      #ifdef S4ENCRYPT_DLL
         ENCRYPT4DLL *encrypt ;
      #else // LY Jul 16/04
         #ifdef S4ENCRYPT_HOOK
            int encryptInit ;
            short encryptBlockSize ;
            void *encryptPreprocess ;
            void *encryptPreprocessInit ;
         #endif
      #endif
      // #ifdef S4ODBC_BUILD
      #ifdef S4WIN32 // LY Jul 16/04
         // AS Apr 4/01 - Made available in stand/alone for odbc stored procedures
         HINSTANCE addDll ;
         S4ADD_FUNCTION *addFunction ;
      #endif
      // AS Apr 28/03 - made trans-shared a run-time switch
      #ifndef S4OFF_TRAN
         Bool5 transShared ;
      #endif
      // AS Apr 11/03 - New feature allows unlock appending as well as locking of clones
      #ifndef S4SERVER
         short shareCloneLocks ;
      #endif
   #endif /* CODE4UNAVAILABLE */
   // AS May 13/04 - configureable compression levels
   #if defined( S4COMPRESS ) && !defined( S4SERVER )
      // the server uses the SERVER4CLIENT.fileCompressLevel setting
      short fileCompressLevel ;  //
   #endif
   // AS Jun 24/04 - support for compress write
   short compressWrite ;                     // set to true to support writing to compressed tables
   #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      unsigned short compressArrayAllocCount ;   // the number of array entries to add when expanding out the write block array - default to 64k worth
   #endif
   // AS Oct 3/06 - need to have a default encryption key for odbc...
   #if defined( S4ODBC_BUILD ) && defined( S4SERVER )
      int encryptKeyLen ;
      char encryptKey[32] ;
   #endif
   // AS Dec 13/05 - under Windows getenv is becoming deprecated...
   // AS May 2/06 - always define this to avoid structure size issues
   // #ifdef S4WINDOWS_VS5_PLUS
      char env[LEN4PATH+1] ;
   // #endif
   // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
   #ifdef RELATE4LOG
      FILE4 relateLogFile ;
      struct timeb statusTime ;
   #endif

   #ifdef S4WIN32
      // AS Apr 5/07 - adjust...if the calling thread is not high priority we don't need to sleep
      int highPriority ;
   #endif

   // AS Jan 27/10 - support for lock checking in multi-thread scenarios...
   #ifdef S4LOCK_CHECK
      LIST4 lockList ;
      MEM4 *lockAlloc ;
   #endif

   #if !defined(S4OFF_COMPRESS) && defined(S4COMPRESS_QUICKLZ)
      void *qlz_state_compress;
      void *qlz_state_decompress;
   #endif

   // CS 2007/02/21 This must always be at the bottom of the CODE4 structure. It is used to calculate the size of the struct.
   int code4bottom ;

#ifdef S4CBPP
   } ;
#else
   } CODE4 ;
#endif



// AS Feb 9/06 - added clipper support for packwithstatus
#if defined( TIME4STATUS ) && defined( S4WIN32 ) && !defined( S4OFF_WRITE )
   // AS Jun 30/03 - moved to d4data.h, for support for d4packWithProgress
   // AS Mar 25/04 - moved lower down for C++ build support
   typedef struct REINDEX4STATUSSt
   {
      unsigned short numTags ;
      unsigned short tagOn ;
      CODE4 S4PTR *c4 ;  // AS Mar 25/04 - requires change for C++ build
   } REINDEX4STATUS ;
#endif



#ifndef S4OFF_MEMO
   /* the memoId is stored as a field in the data record.  For S4FOX, you multiply this
      memoId value by MEMO4FILE->blockSize to get the physical byte offset of the memo in
      the memo file. */

   typedef struct
   {
      FILE4     file ;
      short     blockSize ;              /* # Bytes in a given memo block */
      struct DATA4FILESt  S4PTR *data ;
      #ifndef S4OFF_MULTI
         int fileLock ;                  /* True if file is locked */
      #endif
   } MEMO4FILE ;
#endif /* S4OFF_MEMO */



#ifdef S4CBPP
   class S4CLASS FILE4SEQ_READ
   {
   public:
#else
   typedef struct
   {
#endif
      FILE4 S4PTR *file ;

      FILE4LONG pos ;          /* The next position to read from */
      FILE4LONG_EXTEND pad ;   /* pad for variations in FILE4LONG size */
      char S4PTR *buffer ;
      unsigned nextReadLen ;
      unsigned total ;     /* Total buffer length */
      unsigned working ;   /* Temporary working buffer length (to help align write) */
      unsigned avail ;     /* # of bytes currently available */
      #ifdef S4ADVANCE_READ
         int doAdvance ;
         char S4PTR *buf1 ;
         char S4PTR *buf2 ;
         unsigned int buf1status, buf2status ;
         int buf1avail ;
         int buf2avail ;
      #endif
#ifdef S4CBPP
   } ;
#else
   } FILE4SEQ_READ ;
#endif



#ifdef S4CBPP
   class S4CLASS FILE4SEQ_WRITE
   {
   public:
#else
   typedef struct
   {
#endif
      FILE4 S4PTR *file ;

      FILE4LONG pos ;          /* The next position to read from */
      FILE4LONG_EXTEND pad ;   /* pad for variations in FILE4LONG size */
      char S4PTR *buffer ;
      unsigned total ;     /* Total buffer length */
      unsigned working ;   /* Temporary working buffer length (to help align write) */
      unsigned avail ;     /* # of bytes left in working buffer */
      #ifdef S4WRITE_DELAY
         char S4PTR *buf1 ;
         char S4PTR *buf2 ;
         unsigned bufLen ;
         unsigned buf2len ;
         int buf1avail ;
         int buf2avail ;
      #endif
#ifdef S4CBPP
   } ;
#else
   } FILE4SEQ_WRITE ;
#endif



typedef struct  /* Data File Format */
{
   char name[11] ;
   char type ;
   S4LONG offset ;    /* field's offset into record buffer (for vfp 3.0) */
   unsigned char len ;
   unsigned char dec ;
   #ifdef S4CLIENT_OR_FOX
      char nullBinary ;    /* for FOX 3.0 0x02 == allows null fields, 0x04 == binary field, 0x08 = autoIncrement field, 0x10 = autoTimestamp field */
      char filler2[12] ;
   #else
      char filler2[13] ;
   #endif
   char hasTag ;
} FIELD4IMAGE ;



#ifdef S4MDX
   // AS 09/21/00 - MDX dBase 7 version 4 format
   typedef struct  /* Data File Format */
   {
      char name[32] ;
      char type ;
      S4LONG offset ;    /* field's offset into record buffer (for vfp 3.0) */
      unsigned char len ;
      unsigned char dec ;
      #ifdef S4CLIENT_OR_FOX
         char nullBinary ;    /* for FOX 3.0 0x02 == allows null fields, 0x04 == binary field */
         char filler2[12] ;
      #else
         char filler2[13] ;
      #endif
      char hasTag ;
   } FIELD4IMAGE_MDX4 ;
#endif



typedef struct
{
   unsigned short int lo[4] ;
/*   signed short int hi ;     significant bit in this field */
} CURRENCY4 ;



typedef struct FIELD4St /* Internal Structure and Field Routines. */
{
   // AS 05/24/00 NEVER change around this structure - it is used in #define functions to access some members...
   // AS Oct 27/03 - long field names support
   char shortName[11] ;
   unsigned short int len ;
   unsigned short int dec ;
   short int type ;
   unsigned long offset ;   /* codebase allows long */
   struct DATA4St S4PTR *data ;

   /* #ifdef S4CLIENT_OR_FOX  */  /*These members are used in OLEDB*/
      char null ;
      unsigned short int nullBit ;   /* bit which indicates whether field contents currently are null */
      char binary ;   // 0x01 indicates binary (r4memoBin/rcharBin), 0x02 indicates 4-byte double r4memo/r4gen
      // AS 05/24/00 for efficiency, track null info better...
      unsigned short int nullBitByteNum ;
      char nullBitMask ;
   /*#endif  */
   // AS 07/17/00 - was having some problems with OLE-DB where the compiled switches may differ, so expand out the structure
   // to always include all variables...
   #ifndef S4OFF_MEMO
      struct F4MEMOSt S4PTR *memo ;
   #else
      void *space ;
   #endif
   short int debugInt ;         /* used to check structure integrity (set to 0x5281) */
   #ifdef S4CLIENT_OR_FOX
      Bool5 autoIncrement ;        // true if this field is an auto-increment field...
      // AS Mar 11/03 - support for new feature r4autoTimestamp
      Bool5 autoTimestamp ;        // true if this field is an auto-timestamp field...
   #endif
   char *longName ;
} FIELD4 ;



#ifndef S4OFF_MEMO
   typedef struct F4MEMOSt
   {
      S4CONV( int isChanged, int is_changed ) ;
      int status ;           /* 0 - Current contents, 1 - Unknown */
      char  S4PTR *contents ;
      unsigned int len ;
      S4CONV( unsigned int lenMax, unsigned int len_max ) ;
      FIELD4  S4PTR *field ;
      #ifndef S4CLIENT
         // AS Jun 23/03 - For ODBC purposes we want to track the id associated with the read field.  In particular
         // this allows us to check, for a read memo, if the FIELD4 is pointing to our memo.
         long memoOffset ;
         // AS Jun 2/03 - Was failing to properly update indexes on memo fields.  Need to keep a copy of the old key around.
         // There is a known problem here now that we only store the first c4->memSizeMemoExpr bytes of the memo field.  If the
         // user creates a more complex expression that value may have to be updated to a larger value.
         void *contentsOld ;
         unsigned long lenContentsOld ;
         unsigned int lenContentsOldAlloc ;
         // AS Mar 28/05 - issue here, if the status is set to 0 (memo read), we were not saving the memo.  What we really want to
         // track is whether or not we have already saved the memo...
         Bool5 savedMemo ;
      #endif
   } F4MEMO ;
#endif



#ifdef S4SERVER
   typedef struct
   {
      char append ;
      char delet ;
      char update ;
      char index ;
      char alter ;
      char refer ;
      char compress ;
      char read ;
      char open ;    /* virtual field */
   } AUTHORIZE4 ;
#endif



typedef struct
{
   /* Database Header Information */
   char     version ;        /* 0x31 in file is transformed to 0x30 in memory for easy handling like 0x30 files */
   char     yy ;             /* Last Update */
   char     mm ;
   char     dd ;
   S4LONG   numRecs ;
   unsigned short headerLen; /* Header Length, Indicates start of data */
   unsigned short recordLen;

   // with CodeBase, indicates support available -- for new additions to header...
   // flags[0] if set to '1' means the table contains an autoIncrement field
   // flags[1] if set to '1' means that the attached memo file contains potentially compressed entries
   //          - entries are only actually compressed if they are to exceed the size of the memo block
   //          - can tell if compressed if memoBlock->type == 3.
   // flags[2] if set to '1' means that the data file contains compressed data
   // flags[3] if set to '1' means that the data file contains an autoTimestamp field
   // flags[4] if set to '1' means that the data file contains long field names
   char     flags[8] ;
   #ifdef S4DATA_ALIGN  /* LY 2001/07/19 : using double changes size of struct */
      char   autoIncrementVal[8] ;   // current autoIncrement value
   #else
      double   autoIncrementVal ;   // current autoIncrement value
   #endif
   char     hasMdxMemo ;    /* 0x01 for has mdx, in fox 0x02 for has memo */
   char     codePage ;
   char     zero2[2] ;
} DATA4HEADER_FULL ;



#ifdef __cplusplus
   class Single4lock : public Single4
   {
      // use as a place holder for a the lock link
   } ;

   #ifdef S4LOCK_HASH
      // use as a place holder for a the hash link
      class Single4hash : public Single4
      {
      } ;
   #endif



   #ifdef S4LOCK_HASH
   class Lock4 : public Single4lock, public Single4hash
   #else
   class Lock4 : public Single4lock
   #endif
   {
   public:
      #ifdef S4SERVER
         long clientId ;
      #endif
      struct DATA4St *data ;
      long recNum ;
      #ifndef S4CLIENT
         Lock4type lockType ;   // either LOCK4READ or LOCK4WRITE
      #endif
   } ;



   #ifdef S4LOCK_HASH
      /*
      Algorithm Notes

      The # of lists in the hash starts at 1.

      If, when searching for an item, if the # of write locks on the chain searched
        exceeds HASH4LOCK_EXPAND, then the hash is expanded.  Note that we don't
        use read-locks because there could be duplicates (i.e. write locks are
        unique).

      This means that a user who quickly adds and removes a lock won't cause
      the hash to expand, since the lock is added at the start of the hash, and
      is the first to be removed.  It also means that removing locks in the reverse
      order of adding them also results in no increase in the hash table.

      Thus, in general, the hash table will only increase when unlocks are performed
      in an order other than a stack-like ordering or when other users are trying
      to find locks.
      */

      // expands if > this number of write locks on the list
      #define HASH4LOCK_EXPAND 1

      struct DATA4St ;

      class Hash4lock
      {
      public:
         Hash4lock() ;
         ~Hash4lock() ;

         void * operator new( size_t s ) ;
         void operator delete( void *p ) ;

         #ifdef E4HASH
            void check() ;
         #endif

         void expand() ;  // double the entries in the hash
         inline int hash( int val ) { return val & mask ; }
         inline void add( Lock4 *lock )
         {
            #ifdef E4HASH
               check() ;
            #endif
            (ptrs[hash( lock->recNum )]).add( (Single4hash *)lock ) ;
            #ifdef E4HASH
               check() ;
            #endif
         }
         void remove( Lock4 *lock ) ;
         void upgradeLock( struct DATA4St *data, long recNo, Lock4type lockType ) ;

         // find returns 1 if we have locked, r4locked if someone else, 0 if nobody
         int find( struct DATA4St *data, long recNo, Lock4type lockType ) ;

         // for server case where looking for any lock...
         int findAnyLock( struct DATA4St *data, Lock4type lockType ) ;
      private:
         int numEntries ;
         unsigned long mask ;
         Single4hash *ptrs ;  // an array of Single4 lists of Lock4's
      } ;
   #endif /* S4LOCK_HASH */
#endif /* cplusplus */


typedef struct DATA4FILESt
{
   LINK4 link ;

   S4CONV( unsigned int recWidth, unsigned int record_width ) ;

   unsigned short int infoLen ;

   short nFields ;         /* The number of data fields in the database, range from 1 - 32768, */
   short nFieldsMemo ;     /* The number of memo fields in the database, range from 1 - 32768, */

   #ifndef S4ODBC_STAND
      // want to share DATA4's results in stand-alone ODBC
      long minCount ;
   #endif
   // AS May 16/03 - comment - (server)handling modified, == 0 if noone has open (keepopen set true), ==-1 if
   // create has been called but open not yet called.
   long userCount ;      /* number of DATA4's that are using this DATA4FILE */

   CODE4 S4PTR *c4 ;
   char S4PTR *info ;

   /**** the next set of lines must remain in order as they are a file view ****/
      /* Database Header Information */
      char version ;               /* 0x31 in file is transformed to 0x30 in memory for easy handling like 0x30 files */
      #ifndef S4CLIENT
         char     yy ;             /* Last Update */
         char     mm ;
         char     dd ;
      #endif
      S4LONG   numRecs ;
      unsigned short headerLen ; /* Header Length, Indicates start of data */
      unsigned short recordLen;
      char     flags[8] ;           // with CodeBase, indicates support available -- for new additions to header...
      #ifdef S4DATA_ALIGN  /* LY 2001/07/21 : need to match alignment of header when updating with dfile4updateHeader */
         char   autoIncrementVal[8] ;   // current autoIncrement value
      #else
         double   autoIncrementVal ;   // current autoIncrement value
      #endif
      #ifdef S4PREPROCESS_FILE
         // AS May 24/02 - Need these values to pad to 32 bytes (for block preprocessing)
         char     preprocess_hasMdxMemo ;    /* 0x01 for has mdx, in fox 0x02 for has memo */
         char     preprocess_codePage ;
         char     preprocess_zero2[2] ;
      #endif
   /**** the previous set of lines must remain in order as they are a file view ****/

   #ifndef S4OFF_WRITE
      int doDate ;    /* does the date need to be updated on unlock/close? */
   #endif

   #ifdef S4CLIENT
      // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
      // struct DATA4St S4PTR *fileLock ;
      // struct DATA4St S4PTR *appendLock ;
      // struct DATA4St S4PTR *lockTest ;
      long fileLockServerId ;
      long fileLockLockId ;
      long appendLockServerId ;
      long appendLockLockId ;
      long lockTestServerId ;
      long lockTestLockId ;
      char accessName[LEN4PATH+1] ;
      struct CONNECTION4St S4PTR *connection ;
      long serverId ;
      int accessMode ;
      Bool5 codePageRead ;   // true if the codepage has been read from the server
   #else
      char S4PTR *record ;
      FILE4    file ;
      char hasMdxMemo ;        /* Has an MDX and/or a memo file attached to it */

      int fileChanged ;   /* True if the file has been changed since */
                                /* the header has been updated. */
      short    blockSize ;
      #ifndef S4OFF_MEMO
         #ifdef S4WIN64
//            BYTE spaceIA1 ;
         #endif
         MEMO4FILE   memoFile ;   /* Memo file handle */
      #endif

      #if defined( S4FILE_EXTENDED ) && !defined( S4CLIENT )
         // AS 09/13/99 --> track # records before file becomes large...
         unsigned long numRecsBeforeFileLong ;
      #endif

      int tranStatus ;   /* is this data4file using transactions? */
      char valid ;    /* if valid, a close on a datafile will not low-close it */
      #ifndef S4OFF_MULTI
         long fileServerWriteLock ;    /* which data holds file lock */
         long fileClientWriteLock ;    /* which data holds file lock */
         long appendClientLock ;  /* True if the file is locked for appending */
         long appendServerLock ;  /* T the file is locked for appending */

         long tempServerLock ;    /* for lock registering */
         long tempClientLock ;    /* for lock registering */
         unsigned long recordLockWriteCount ;
         unsigned long recordLockReadCount ;
         LIST4 data4list ;
      #endif
      #ifdef S4FOX
         short compatibility ;   /* 2.5/2.6 compatible option */ // CS 2001/04/20 change to short
      #endif
   #endif  /* S4CLIENT */

   int    codePage ;

   #ifdef S4SERVER
      struct SERVER4CLIENTSt S4PTR *singleClient ;   /* only one client has access to the file */
      time_t    nullTime ;   /* The last time the datafile had 0 handles, if any */
      #ifndef S4OFF_MULTI
         struct DATA4St *exclusiveOpen ;
//         F4FLAG locks ;   /* used for quick look-ups of locks using bitmaps - exclusively opened files only */
      #endif
   #endif

   #ifndef S4OFF_OPTIMIZE
      int hiPrio ;   /* used to determine which priority lru list to put block data on and advance group-reads */
                     /* '1' if d4seek, -1 if d4skip */
   #endif

   char openMdx ;       /* does the datafile have an open production file attatched to it */

   #ifdef S4CLIPPER
      LIST4 tagfiles ;
      unsigned long indexLocked ;
   #else
      LIST4    indexes ;        /* list of INDEX4FILE;s */
   #endif
   #ifdef S4LOCK_HASH
      Hash4lock *lockHash ;
   #endif
   #if defined( S4FOX ) && !defined( S4CLIENT )
      Bool5 autoIncrementSupported ;
      // AS Mar 11/03 - support for new feature r4autoTimestamp
      Bool5 autoTimestampSupported ;
      // AS June 19/02 - added support for memo field compression
      Bool5 compressedMemosSupported ;
      Bool5 compressedDataSupported ;
      // AS Oct 27/03 - long field names support
   #endif
   #ifdef S4CLIENT_OR_FOX
      Bool5 longFieldNamesSupported ;
   #endif
   #ifdef __cplusplus
      #ifdef S4CLIENT
         Single4 lockedRecords ;
      #endif
      #ifndef S4OFF_MULTI
         Single4 fileReadLocks ;   // uses Lock4's for convenience
      #endif
   #endif
   #ifdef S4CLIENT
      // AS Oct 12/01 - client needs to track the readonly flag on a DATA4FILE level in addition to the DATA4
      // level.  (If the file initialy opened with full privileges, new DATA4's can re-open on client with
      // more restrictive - i.e. readOnly, but if first open is read-only all rest are maxed out at read-only
      // this is the same as stand-alone).
      int readOnly ;
   #else
      /* AS Aug 15/01 - ability to retrieve a version number associated with the data file, which increases when
         the data file changes */
      long versionNumberOld ;    // the last version number that was returned to the user
      long versionNumber ;       // internal version number counter (not necessarily same number returned to user)
      #ifdef S4WIN32
         FILETIME timeStamp ;         // the latest time stamp checked for the file
      #endif
   #endif
   #ifdef S4PREPROCESS_FILE
      // AS 05/24/02 for block preprocessing need to be able to recover this value to write on block boundaries on header
      char savedVersionNumber ;
   #endif
   #ifdef S4UTILS
      // AS July 16/02 - the existing utility reindexed data files once for every open - so if there were
      // 100 clients with a large table open, there would be 100 reindexes of this table.  Instead I modified
      // this to only re-index if it wasn't already done...
      Bool5 didReindex ;
   #endif
   // AS Jun 12/02 - added support for re-using of deleted rows.
   // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
   #if !defined( S4OFF_INDEX ) && !defined( S4CLIPPER )
      struct TAG4FILESt *appendTag ;
   #endif
   #ifndef S4OFF_TRAN
      // AS Feb 18/03 - Track update status to improve performance in ODBC
      Bool5 odbcUpdateRequired ;
   #endif
   // AS Nov 26/02 - for data file compression
   // AS May 17/04 - server support for data file compression
   #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      // AS Jul 31/03 - support for generic handling of compression
      COMPRESS4HANDLER *compressInfo ;
   #endif
} DATA4FILE ;


#ifdef S4CLIENT
   /* AS Apr 10/02 - New function for batch writing client/server */
   typedef struct
   {
      char *contents ;        // the contents of the memo field
      long contentsLen ;      // the length of the contents
      unsigned long contentsAllocLen ; // amount of space currently allocated for 'contents'
   } MEMO4BATCH_ENTRY ;



   /* AS Apr 10/02 - New function for batch writing client/server */
   typedef struct
   {
      // there is 1 memo batch for each record in the write buffer
      // int memoFieldCount ;
      MEMO4BATCH_ENTRY *memos ;    // an array of entries (1 for each memo field in the record)
   } MEMO4BATCH ;



   /* AS Apr 10/02 - New function for batch writing client/server */
   typedef struct
   {
      long writeRecsToBuf ;       // maximum number to buffer
      long curWriteBufCount ;     // current count of records in the buffer
      char *writeDelayBuf ;       // the delayed-write buffer - holds the rec # and record buffer
      MEMO4BATCH *memos ;         // pointer to an array of memo entries (1 for each record)
      MEMO4BATCH_ENTRY *memoBatchEntry ;   // single allocation for all memos
   } DATA4BATCH_WRITE ;



   typedef struct
   {
      long readRecsToBuf ;        // maximum number to buffer
      long readRecsToBufNext ;    // # of read recs to buffer on the next fetch (can happen with d4skipCache() where the # to buffer can change)
      char *readAdvanceBuf ;      // stores recno / record pairs (the rc is not stored, is always 0 except for last record which is stored in readBufRcSave)
      long readBufNum ;           // number records currently in buffer (may differ from max size)
      int readBufPos ;            // current position in buf
      long readBufRecNoOn ;       // current rec # fetch (to see if bufPos accurately reflects current position)
      short readBufDirection ;    // -1 if backwards, 1 if forwards
      // short readBufRcSave ;    // rc to return after going past bufEndRecno
      // int advanceReadBufRecs ; // size of buf in records
      char *recordExtra ;         // an extra record buffer used for copying
      MEMO4BATCH *memos ;         // pointer to an array of memo entries (1 for each record)
      MEMO4BATCH_ENTRY *memoBatchEntry ;   // single allocation for all memos
      Bool5 doMemos ;             // true if advance read bufferring memo fields as well
      long currentDbl ;           // AS Jul 17/09 for doubling the number to cache
      long modusStart ;
      long nCacheIn ;
   } DATA4BATCH_READ ;
#endif



typedef struct DATA4St
{
   LINK4 link ;
   LINK4 dataFileLink ;

   int bofFlag ;
   int eofFlag ;
   int readOnly ;
   S4CONV( int recordChanged, int record_changed ) ; /* T/F */
   S4LONG count ;                /* a fairly current record count on the database */
   S4CONV( S4LONG recNum, S4LONG rec_num ) ;               /* Record number; -1 unknown; 0 for append */
   S4LONG recNumOld ;            /* Record number, -1 none present; 0 for append */

   char alias[LEN4DATA_ALIAS+1] ;

   S4CONV( CODE4 S4PTR *codeBase, CODE4 S4PTR *code_base ) ;

   TRAN4 S4PTR *trans ;

   FIELD4 S4PTR *fields ;       /* An array of field pointers */

   LIST4 indexes ;               /* list of INDEX4's */

   struct TAG4St *tagSelected ;

   // int    codePage ;

   DATA4FILE *dataFile ;
   /* CJ- The DATA4FILE member of this structure must be at this point for
      CodeReporter to work.  Do not place any new members before this point.
      The next four allocations also must stay together so place new members
      after the declaration of "recordBlank" */

   char S4PTR *groupRecordAlloc ;   /* if the current and old record allocations
                                       allocated at the same time? */
   char S4PTR *record ;          /* Data allocated with 'u4alloc' */
   char S4PTR *recordOld ;       /* Data allocated with 'u4alloc' */
                                 /* Extra byte added for temporary CTRL_Z */

   char S4PTR *recordBlank ;  /* To allow fast-blanking of binary fields */

   #ifndef S4OFF_MEMO
      S4CONV( F4MEMO S4PTR *fieldsMemo, F4MEMO S4PTR *fields_memo ) ;    /* A list of fields to be flushed */
      #ifndef S4OFF_MULTI
         char memoValidated ; /* Can we be sure memo id #'s are up to date. */
      #endif
   #endif

   #ifdef S4CLIENT
      int aliasSet ;
   #endif

   #ifndef S4OFF_TRAN
      #ifndef S4CLIENT
         int logVal ;
      #endif
      char transChanged ;
   #endif

   #ifdef E4VBASIC
      int debugInt ;      /* used to check structure integrity (set to 0x5281) */
   #endif

   #ifdef S4SERVER
      int accessMode ;              /* in what mode did the client open the data file */
      long clientId ;
      long serverId ;    // a server counter that gets incremented on every d4open() level

      // AS Aug 9/02 - Need to have the capability to determine the clientId and serverId of the
      // source DATA4 for the server's relation DATA4 module.  In particular, we want to allow
      // for record counts to be accessible for clients who have a transaction in progress and
      // then use a relation to access those fields.
      long relationsSourceClientId ;
      long relationsSourceServerId ;
      #ifdef S4JAVA
         short int *fieldsDefined ;
         short int numFieldsDefined ;
      #endif
      #ifndef S4OFF_SECURITY
         Bool5 allowRead ;
         Bool5 allowAppend ;
         Bool5 allowDelete ;
         Bool5 allowUpdate ;
         Bool5 allowIndex ;
         Bool5 allowCompress ;
      #endif
   #else
      long clientId ;
      // AS Apr 15/03 - Support for sharing locks on clones, due this via lockId
      // normally lockId == clientId, but for a cloned table the lockId == clientId of the master table
      long lockId ;
   #endif
   #if !defined(S4OFF_MULTI) && !defined(S4CLIENT) && defined(__cplusplus)
      Single4 lockedRecords ; /* list of LOCK4LINK's in client version, Lock4's in stand-alone/server */
   #endif
   #if defined(S4FOX) || defined( S4CLIENT )
      /* AS 05/24/00 for efficiency, track null info better... */
      FIELD4 *nullFlags ;
   #endif
   #if defined( S4FOX ) || defined( S4CLIENT )
      FIELD4 *autoIncrementField ;
      // AS Mar 11/03 - support for new feature r4autoTimestamp
      FIELD4 *autoTimestampField ;
   #endif
   // AS 02/01/01 - Mark in data4 whether was logged so know if need to log the close message
   #ifndef S4OFF_TRAN
      Bool5 openWasLogged ;
   #endif
   #ifdef S4CLIENT
      /* AS Apr 10/02 - New function for advance-reading client/server */
      DATA4BATCH_READ batchRead ;
      DATA4BATCH_WRITE batchWrite ;
      Bool5 includeMemos ;   // if true, memos are transferred on non-batch reading (d4go/d4seek/d4positionSet/etc.)
      // AS Aug 30/02 - for batch reading, we track the seek key to see if it is the same (in which case can skip)
      char savedSeekNextBatchKey[I4MAX_KEY_SIZE] ;
      short savedSeekNextBatchKeyLen ;
      long readBatchMode ;  // used in conjunction with d4readBufferConfigure to control batching
   #endif
   #ifdef S4PREPROCESS_FILE
      // AS May 29/02 - dfile4recWidth returns in incorrect value in the instance where block preprocessing
      // is being used because we have padded the record buffer out.
      long exposedRecordLen ;
   #endif
   // AS Jun 2/03 - If a tag is based on a memo entry and only the memo entry has
   // changed, the record will match.  In that case we want to continue.
   #ifndef S4CLIENT
      short hasMemoExpr ;  // one of r4uninitializedMemoTagExpression(0), r4noMemoTagExpression, or r4memoTagExpression
      Bool5 useOldMemo ;   // set to true to use old memo when generating keys
   #endif
} DATA4 ;



typedef void S4OPERATOR(void) ;



/* NOTE: IT IS CRITICAL THAT THE FOLLOWING STRUCTURE'S MEMBERS ARE NOT
   RE-ORDERED. */
typedef struct E4INFOSt
{
   short int fieldNo ;   /* field no in data structure -- since FIELD4 may be transient */
   FIELD4 S4PTR *fieldPtr ;
   // AS Jan 6/05 - Added support for a new KEYEQUAL function
   struct TAG4FILESt S4PTR *tagPtr ;

   int localData ;       /* true if the fieldPtr points to a local data4 */
   char S4PTR *p1 ;
   int len ;         /* Length */
   int numEntries ; /* Number of entries in sub-expression */
   int numParms ;
   int resultPos ;  /* The position in the result array for result. */
   int i1 ;          /* Could be constant position. 'i1' and 'resultPos'
                       and 'functionI' and 'function'
                       must be at last position due to memcmp() in e4isTag() */
   int i2 ;  // AS 11/16/99 --> used for IIF as length of second string paramater...// AS Dec 16/04 - also used for variable trimming to store the resulting length
   Bool5 varLength ;  // true if we need to use the i2 length instead of the expr4infoPtr->len (for variable length types)
   int functionI ;
   #ifdef S4CLIENT_OR_FOX
      Bool5 isNull ;   // AS Feb 20/04 - we need to know if the value is null for comparison purposes (null only equals null)
   #endif
   S4OPERATOR S4PTR *function ;
} E4INFO ;



typedef struct e4exprSt
{
   E4INFO S4PTR *info ;
   int infoN ;
   S4CONST char S4PTR *source ;
   char S4PTR *constants ;
   int len, type ;
   struct TAG4FILESt S4PTR *tagPtr ;
   DATA4 S4PTR *data ;
   DATA4FILE S4PTR *dataFile ;
   S4CONV( CODE4 S4PTR *codeBase, CODE4 S4PTR *code_base ) ;

   unsigned int lenEval ;     /* This is the length of the buffer needed for evaluation. */
   int numParms ;    /* This is the # of parameter positions used in evaluation. */
   char hasTrim ;    /* special case for key evaluation */
   // as 08/19/99 expose always, reqd. for OLE-DB
   int keyDec, keyLen ;   /* used for CLIPPER version, left in for OLE-DB... */
   /* AS 02/23/99 -- moved from CODE4, because may evaluation expression within an expression */
   unsigned exprBufLen ;        /* for exprWorkBuf */
   #ifdef S4WIN64 /* LY 00/10/18 */
//      BYTE spaceIA ;
   #endif
   char S4PTR *exprWorkBuf ;    /* used by expression parsing */
   #ifndef S4MEMO_OFF
      // AS Feb 11/04 - need to know if an expression includes a memo field
      Bool5 hasMemoField ;       // true if the expression includes a memo field
   #endif
} EXPR4 ;



#ifndef S4CLIENT
   #ifdef S4CLIPPER
      typedef struct
      {
         S4UNSIGNED_LONG  pointer ;    /* =0L if record, not pointer */
         S4UNSIGNED_LONG  num ;
         unsigned char  value[1] ;  /* The key size is variable */
      } B4KEY_DATA ;
   #else



      typedef struct
      {
         S4UNSIGNED_LONG num ;
         unsigned char  value[1] ;  /* The key size is variable */
      } B4KEY_DATA ;
   #endif


   #define b4invalidNode() ( INVALID4BLOCK_ID )
   #define b4nodeValid( nd ) ( b4node( nd ) != b4invalidNode() )
   #define b4nodesEqual( bn1, bn2 ) ( b4node( bn1 ) == b4node( bn2 ) )
   #define b4nodesNotEqual( bn1, bn2 ) ( b4node( bn1 ) != b4node( bn2 ) )
   #define b4nodeInvalid( nd ) ( b4node( nd ) == b4invalidNode() )


   #ifdef S4FOX
      /* the following structure is used only on the leaf nodes of the tree structure */
      typedef struct
      {
         short            freeSpace ;        /* # bytes available in node */
         unsigned char    recNumMask[4] ;      /* record number mask */
         unsigned char    dupByteCnt ;      /* duplicate byte mask count */
         unsigned char    trailByteCnt ;    /* Trailing byte mask count */
         unsigned char    recNumLen ;       /* # bits used for record number */
         unsigned char    dupCntLen ;       /* # bits used for duplicate count */
         unsigned char    trailCntLen ;     /* # bits used for trail count */
         unsigned char    infoLen ;          /* # bytes for holding record number, */
      } B4NODE_HEADER ;



      typedef struct
      {
         S4UNSIGNED_LONG node ;
      } B4NODE ;



      #define i4blockSize( i4 ) ( i4->blockSize )
      #define b4node( nd ) ( (nd).node )
      #define b4hasNoLeftNeighbor( b4 ) ( b4node( (b4)->header.leftNode ) == 0 )
      #define b4hasNoRightNeighbor( b4 ) ( b4node( (b4)->header.rightNode ) == 0 )
      // currently just multiply by 1 for blocks...
      #define i4multiplier( i4 ) ( (i4)->multiplier )
      #define b4nodeAssignNode( nd1, nd2 ) ( (nd1)->node = (nd2).node )
      #define b4nodeAssignLong( nd1, lng ) ( (nd1)->node = (lng) )
      #define b4nodeSetInvalid( nd ) ( (nd)->node = b4invalidNode() )

      // unknown node used for checking routines...
      #define b4unknownNode() ( INVALID4BLOCK_ID - 1 )
      #define b4nodeSetUnknown( nd ) ( (nd)->node = b4unknownNode() )
      #define b4nodeUnknown( nd ) ( b4node( nd ) == b4unknownNode() )


      typedef struct
      {
         short      nodeAttribute ;    /* 0=index, 1=root, 2=leaf, 4 =??? */
         short      nKeys ;            /* Block Image starts here */
         B4NODE leftNode ;    /* left neighbour block - for actual physical byte offset multiply by multiplier, ULONG_MAX if not present */
         B4NODE rightNode ;   /* right neighbour block - for actual physical byte offset multiply by multiplier, ULONG_MAX if not present */
      } B4STD_HEADER ;



      typedef struct
      {
         LINK4 link ;

         struct TAG4FILESt  S4PTR *tag ;

         int changed ;
         B4NODE  fileBlock ; /* Identifies block within index file ULONG_MAX means invalid or unassigned */
                             /* for actual physical byte offset multiply by multiplier */
         int keyOn ;         /* The current key within the block */
         // AS Jun 8/04 - for large key support, using a different trail byte counter
         long curTrailCnt ;   /* current value used for seeking */
         long curDupCnt ;     /* current value used for seeking */
         int dupPos ;        /* bit offset into the info for the duplicate data */
         int trailPos ;      /* bit offset into the info fonfo for the trail data */
         int recPos ;        /* bit offset into the info for the record # data */
         // AS Jun 8/04 - for large key support, using a different trail byte counter
         long trailByteCnt ;
         long dupByteCnt ;
         char  *curPos ;     /* current position into the data (starts at end) */

         int builtOn ;       /* the 'current' key value (i.e. key really 'on') */
         char  S4PTR *builtPos ;     /* position where built on */
         B4KEY_DATA S4PTR *builtKey ;

         B4STD_HEADER header ;
         B4NODE_HEADER nodeHdr ;    /* only if the block is a leaf */
         char  data[1] ;     /* the remaining data */
      } B4BLOCK ;
   #endif  /* ifdef S4FOX  */



   #ifndef S4FOX
      typedef S4UNSIGNED_LONG B4NODE ;  /* LY 2004/02/02 : changed from unsigned long, for 64-bit */
      #define b4node( nd ) ( nd )
      #define b4nodeAssignLong( nd1, lng ) ( *(nd1) = (lng) )
      #define b4nodeSetInvalid( nd ) ( *(nd) = b4invalidNode() )
      #define b4nodeAssignNode( nd1, nd2 ) ( *(nd1) = (nd2) )
      #ifdef S4MDX
         #define i4blockSize( i4 ) ( (i4)->header.blockRw )
      #else
         #define i4blockSize( i4 ) ( B4BLOCK_SIZE_INTERNAL )
      #endif
      #define i4multiplier( i4 ) ( I4MULTIPLY )
   #endif



   #ifdef S4CLIPPER
      enum index4headerWrite
      {
         writeEntireHeader = 0,
         updateOnly = 1
      } ;



      typedef struct
      {
         short          sign ;
         short          version ;
         S4UNSIGNED_LONG  root ;       /* Root Block */
         S4UNSIGNED_LONG  eof ;        /* First Free Block Pointer */
         short          groupLen ;     /* Key Length + 2*sizeof(long) */
         short          keyLen ;       /* Key Length */
         short          keyDec ;       /* Number of Decimals in Key */
         short          keysMax ;      /* Maximum # of keys per block;  <= 100 */
         short          keysHalf ;     /* Maximum # of keys per half block */

    /*   char           expression[256];   The index expression corresponding to the database. */
    /*   short          unique   ;         TRUE if Unique */
    /*   short          descending  ;      TRUE if Descending */
      } I4IND_HEAD_WRITE;



      /* the version position is 2 bytes into the file */
      #define INDEX5VERSION_POS 2
      /* the sign position is 0 bytes into the file */
      #define INDEX5SIGN_POS 0



      typedef struct
      {
         short      oldVersion ;
         unsigned long  headerOffset ;  /* physical file offset into the index file of header for tag ... */
         S4UNSIGNED_LONG virtualEof ;   /* The next available file block */
         short      sign ;
         short      version ;
         S4UNSIGNED_LONG     root ; /* Root Block */
         S4UNSIGNED_LONG     eof ;  /* First Free Block Pointer */
         short      groupLen ;      /* Key Length + 2*sizeof(long) */
         short      keyLen ;        /* Key Length */
         short      keyDec ;        /* Number of Decimals in Key */
         short      keysMax ;       /* Maximum # of keys per block;  <= 100 */
         short      keysHalf ;      /* Maximum # of keys per half block */
      /* char       expression[256] ;   The index expression corresponding to the database. */
         short      unique   ;      /* TRUE if Unique */
         short      descending ;    /* The descending flag corresponding to the index file */
      /* char       filter[256] ;   The filter(for) expression corresponding to the database. */
      }  T4HEADER ;
   #endif /* S4CLIPPER */



   #ifdef S4MDX
      typedef struct
      {
         char   two ;               /* Version number (currently 2) */
         char   yymmdd[3] ;         /* Date of last reindex */
         char   dataName[12] ;      /* Name of associated data file */
         char   dummy1[4] ;         /* extra 4 bytes for data-names-not used in DOS */
         short  blockChunks ;       /* Block Size 1 to 32 (512 byte chunks) */
         short  blockRw ;           /* Block Read/Write Size in bytes */
         char   isProduction ;      /* 1 if production index, else 0 */
         char   numSlots ;          /* number possible tags (48) */
         short  slotSize ;          /* number bytes/tag slot (32) */
         short  numTags ;
         short  dummy2 ;
         S4UNSIGNED_LONG eof ;
         S4UNSIGNED_LONG freeList ;
         char   zero[4] ;
         char   createDate[3];      /* not used by CodeBase */
         char   blank ;
      }  I4HEADER ;
   #endif /* S4MDX */



   #ifndef S4FOX
      typedef struct
      {
         LINK4 link ;
         struct TAG4FILESt  S4PTR *tag ;

         S4UNSIGNED_LONG  fileBlock ;  /* Identifies block within index file */
         int changed ;
         int keyOn ;      /* The current key within the block */

         short      nKeys ; /* Block Image starts here */
         #ifdef S4CLIPPER
            short pointers[( B4BLOCK_SIZE_INTERNAL / 2 - 1 )] ;
            B4KEY_DATA S4PTR *data ;
         #else
            char       dummy[6] ;
            B4KEY_DATA info ;
         #endif
      } B4BLOCK ;
   #endif /* !S4FOX */



   #ifndef S4FOX
      typedef struct
      {
         B4NODE headerPos ;          /* Header position (in I4MULTIPLY(512) byte chunks) */
         char  tag[10] ;
         short x1000 ;               /* used for dBASE/SQL expression type - dBASE only allowed for CBPP 1.0x */
         char  leftChld ;
         char  rightChld ;
         char  parent ;
         char  x2 ;
         char  indexType ;
         char  zeros[11] ;
      } T4DESC ;
   #endif /* !S4FOX */



   #ifdef S4FOX
      typedef struct
      {
         B4NODE          root ;          /* ULONG_MAX means unknown - actually a B4NODE, for actual physical byte offset multiply by multiplier */

         B4NODE          freeList ;      /* start of the free list, for actual physical byte offset multiply by multiplier, (ULONG_MAX if none)  */
         S4UNSIGNED_LONG version ;       /* used multi-user only */
         short           keyLen ;        /* Key Length */
         // AS 10/01/99 - typeCode of 0x02 used to indicate 'nullable'
         unsigned char   typeCode;       /* 0x01 Uniq; 0x02 Null; 0x04 r4candidate; 0x08 For Clause; 0x32 Compact; 0x80 Compound */
         unsigned char   signature ;     /* unused */

         /* AS 06/30/99 --> for CodeBase purposes, we have added block size and multiplier blockSize must be divisiable by multiplier
            blockSize and multiplier are only used for the tagIndex index - assumes the same for entire file...
         */

         S4UNSIGNED_LONG codeBaseNote ;   /* == 0xabcd - if set to this value, then we know to use
                                            the blockSize and multiplier below.  Assume will never otherwise
                                            get set to this apparently random value... */
         S4UNSIGNED_LONG blockSize ;      /* CodeBase only, size of blocks, multiple of 512 */
         S4UNSIGNED_LONG multiplier ;     /* CodeBase only, amount to multiply references (i.e. B4NODES) by to get physical byte offsets */


      /* char           dummy2[470] ;       unused - previously was 482, but we use another 12 bytes now for CodeBase */

         char           sortSeq[8] ;     /* collating sequence (ie GENERAL) */
         char           dummy3[4] ;
         short          descending ;     /* 1 = descending, 0 = ascending */
         short          filterPos ;      /* not used, == to exprLen */
         short          filterLen ;      /* length of filter clause */
         short          exprPos ;        /* not used, == to 0  */
         short          exprLen ;        /* length of expression */
      /* char           exprPool[512] ;  expression and filter pool */
      }  T4HEADER ;
   #endif  /* S4FOX */



   #ifdef S4MDX
      typedef struct
      {
         S4UNSIGNED_LONG root ;          /* -1 means unknown */
         char           dummy1[4] ;
         char           typeCode;        /* 0x10 Normal; 0x58 Uniq,Desc; 0x50 Uniq; 0x18 Desc */
         char           type ;           /* N,D, or C (F is type N)  */
         char           dummy2[2] ;
         short          keyLen ;
         short          keysMax ;        /* Maximum # of keys per block;  <= 100  */
         short          isDate ;         /* 1 if Date Key */
         short          groupLen ;       /* keyLen plus 4 (MDX); plus 8 (NDX) */
         unsigned char  version ;
         char           dummy4 ;
         short          unique   ;       /* 0x4000 (TRUE)if Unique */

         /* Note, 'exprKey[220]' comes after 'unique' and */
         /*       'exprFilter[220]' comes at position 0x2FA */
      }  T4HEADER ;
   #endif /* S4MDX */
#endif /* S4CLIENT */



// AS 07/27/99 -- this has been superseded by more simple collations for foxPro
// typedef struct T4VFPSt
// {
//    #ifdef S4FOX   /* info on codepage and collating sequence */
//       #ifndef S4CLIENT
//          int              codePage ;    /* codepage of database file */
//          int              sortType ;    /* collating seq of tag in index file */
//          translatedChars  *tablePtr ;   /* points to translation table */
//          compressedChars  *compPtr;     /* points to compressed char table */
//          unsigned char    *cpPtr ;      /* points to codepage table */
//       #endif
//    #endif
// } T4VFP ;


#ifdef S4CLIENT
   // AS Feb 12/09 - support for cacheing a larger number of keys
   typedef struct KEY4CACHESt
   {
      int allocSize ;     // number keys allocated for the cache
      int cacheSize ;     // number of keys to cache (may differ from the allocSize if the # of rows to cache changes)
      int currentSize ;   // number keys currently in the cache
      int pos ;           // current position within the cache  // if -1 then the cache is cleared.  Alternately cache is cleared if cachedKeys is null, even if this value is not -1
      long *recNos ;
      char *keys ;
      // AS Jul 14/09 new feature for enhanced functionality
      long modusOriginal ;  // the original requested modus
      long modusCurrent ;   // the current modus value (in the case of doubling the cache size)
      int numRowsIn ;  // original specified numRowsIn value
   } KEY4CACHE ;
#endif


typedef struct TAG4FILESt
{
   LINK4   link ;
   #ifndef S4CLIPPER
      struct INDEX4FILESt S4PTR *indexFile ;
   #else
      void S4PTR *space1 ;
   #endif
   char    alias[LEN4TAG_ALIAS+1] ;
   CODE4 S4PTR *codeBase ;
   int debugInt ;  /* used to check structure integrity (set to 0x5281) */

   #ifdef S4CLIPPER
      int userCount ;
   #else
      int space2 ;
   #endif

   #ifdef S4CLIENT
      char S4PTR *exprPtr ;
      char S4PTR *filterPtr ;
      short int errUniqueHold ;
      short int keyLen ;
      short int keyType ;
      // AS Oct 25/05 - keep track of additional information for the tag...low level tag functions
      Bool5 tagDataValid ;   // set to true if we perform an operation that makes the low-level tag info correct.
                              // reset to false when the tag data is made incorrect (via seek/skip/position with a selected tag)
      DATA4 *refData ;  // gets set when d4tag is called
      // AS Feb 9/09 - Keep the key stored locally
      char S4PTR *currentKey ;
      unsigned long currentKeyLen ;
      unsigned int currentKeyAllocLen ;
      long recNo ;

      // AS Feb 12/09 - support for cacheing a larger number of keys
      KEY4CACHE *cache ;  // if null the cache is not valid (is cleared) / not being used
      Bool5 t4readBuffer ;  // true/false is t4readbuffer activated?

   #else
      EXPR4   S4PTR  *expr ;
      EXPR4   S4PTR  *filter ;
      S4CMP_FUNCTION *cmp ;
      C4STOK S4PTR   *stok ;        /* Conversion for 'seek' */
      C4DTOK S4PTR   *dtok ;        /* Conversion for 'seek' */
      #ifdef S4MDX
         char         hadKeys ;
      #else
         char         space3 ;
      #endif
      char            hasKeys ;

      LIST4           blocks ;
      LIST4           saved ;

      // AS Nov 27/03 - actually, this is only used for debugging purposes
      // AS Nov 27/03 - turns out to be invalid in multi-user since another user may be shrinking the file
      #if defined( S4CLIPPER ) && defined( E4MISC ) && defined( S4SINGLE )
         S4UNSIGNED_LONG       checkEof ;     /* used for debug purposes to verify eof length */
      #else
         S4UNSIGNED_LONG       space6 ;
      #endif

      #ifdef S4CLIPPER
         FILE4 file ;
         S4LONG fileLocked ;
         MEM4 S4PTR *blockMemory ;
         #ifndef S4OFF_OPTIMIZE
            struct TAG4FILESt *readBlockTag ;    /* used for optimization -- is a block-read request occurring? */
         #endif
      #else
         FILE4        space7 ;
         S4LONG       space8 ;
         void S4PTR  *space9 ;
      #endif

      T4HEADER  header ;
      B4NODE    headerOffset ;     /* node Offset in file to the tag's header info. */
      int rootWrite ;        /* True if 'header.root' needs to be written */
      int keyDec ;

      #ifdef S4FOX
         // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
         // T4VFP        vfpInfo ;   // required to always exist for ole-db where index independent...
         enum Collate4name collateName ;
         Bool5    isUnicode ;  // TRUE if unicode, false if 1 byte character
         char         pChar ;
         MEM4 *builtKeyMemory ;
      #endif

      #ifdef S4MDX
         S4LONG       changed ;
      #endif

      #ifdef S4UNIX
         int keyType ;
      #endif
      #ifdef S4COMIX
         // track an available block to avoid index bloat...
         B4NODE availBlock ;
      #endif
   #endif
   // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
   #ifdef SHARE4TAG_REMOVE
      #ifndef S4CLIENT
         #ifndef S4OFF_TRAN
            LIST4 removedKeys ;     /* list of temporarily removed keys for r4unique and e4unique cases */
         #endif
         int added, removed ;             /* was an entry added, removed */
      #endif
   #endif
   // AS Oct 12/04 - We need to track the index access name associated with the TAG4FILE in case the indexfile gets opened again.
   // in particular in client/server to keep the index associated with the correct structure in case it is opened again by another client.
   #ifdef S4CLIPPER
      char indexAccessName[LEN4PATH] ;
   #endif

   // AS May 11/06 - support for general collation using ascend/descend...
   #ifdef S4FOX
      Bool5 hasAscendOrDescend ;
   #endif
} TAG4FILE ;



typedef struct TAG4St
{
   LINK4   link ;
   struct  INDEX4St S4PTR *index ;
   TAG4FILE *tagFile ;
   /* this uniqueError MUST be 0 if the tag is not unique */
   short int errUnique ; /* Is rewriting a unique key an error ? */
   #ifndef S4OFF_TRAN
      int isValid ;
   #endif
   // AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
   #ifndef SHARE4TAG_REMOVE
      #ifndef S4CLIENT
         #ifndef S4OFF_TRAN
            LIST4 removedKeys ;     /* list of temporarily removed keys for r4unique and e4unique cases */
         #endif
         int added, removed ;             /* was an entry added, removed */
      #endif
   #endif
} TAG4 ;



#ifndef S4CLIPPER
   typedef struct INDEX4FILESt
   {
      LINK4  link ;

      LIST4  tags ;
      int userCount ;
      CODE4 S4PTR *codeBase ;
      DATA4FILE S4PTR *dataFile ;

      #ifdef S4CLIENT
         char accessName[LEN4PATH+1] ;
         /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
         char fullIndexName[LEN4PATH+1] ;   // to ensure client can't have 2 instances of same index
         int autoOpened ;
         long clientId, serverId ;
         #ifdef E4VBASIC
            short int debugInt ;         /* used to check structure integrity (set to 0x5281) */
         #else
            short int space1 ;
         #endif
      #else
         FILE4  file ;
         MEM4 S4PTR *blockMemory ;
         long fileLocked ;

         #ifdef S4FOX
            TAG4FILE S4PTR *tagIndex ;    /* the tags are a tag in the index file! */
            B4NODE  eof ;         /* eof location in file, for actual physical byte offset multiply by multiplier */
            unsigned long versionOld ;
            unsigned long versionReadUnlocked ;   /* used to know latest unlocked version read (not guaranteed for writing) */
            unsigned long  blockSize ;      /* CodeBase only, size of blocks, multiple of 512 */
            unsigned long  multiplier ;     /* CodeBase only, amount to multiply references (i.e. B4NODES) by to get physical byte offsets */
         #endif

         #ifdef S4MDX
            TAG4FILE S4PTR *space2 ;
            long space3 ;
            unsigned long space4 ;
            I4HEADER header ;
            long  changed ;
         #endif

         #ifndef S4OFF_OPTIMIZE
            TAG4FILE *readBlockTag ;    /* used for optimization -- is a block-read request occurring? */
         #endif
         #ifndef S4CLIPPER
            // AS Jan 9/03 - not supported for clipper
            Bool5 nonUpdateable ;  // AS Dec 31/02 - Added support to create non-production non-updating indexes
         #endif
      #endif
      int isValid ;
   } INDEX4FILE ;
#endif



typedef struct INDEX4St
{
   LINK4  link ;
   LIST4  tags ;

   DATA4 S4PTR *data ;
   CODE4 S4PTR *codeBase ;

   // AS Feb 7/03 - Need for locking control as well as transactions
   // #ifndef S4OFF_TRAN
      int isValid ;
   // #endif
   #ifndef S4CLIPPER
      INDEX4FILE *indexFile ;
   #else
      long space4;
   #endif
   #ifdef S4CLIENT
      char alias[LEN4PATH+1] ;
   #else
      char accessName[LEN4PATH+1] ;
      #ifdef S4CLIPPER
         FILE4 file ;
         // AS May 26/06 - was not properly using this path value...it needs to be a stored string
         // char S4PTR *path ;
         char nameWithPath[LEN4PATH+1] ;
         long  versionOld ;
      #else
         FILE4 space1 ;
         char S4PTR *space2 ;
         long space3 ;
      #endif
   #endif
} INDEX4 ;



#if !defined(S4OFF_TRAN) && !defined(S4CLIENT)
   typedef struct
   {
      LINK4 link ;
      unsigned long recno ;
      unsigned char key[1] ;   /* of variable length, but marked as '1' for a place holder */
   } TAG4KEY_REMOVED ;
#endif



// AS Feb 9/06 - added clipper support for packwithstatus
#if defined( TIME4STATUS ) && defined( S4WIN32 ) && !defined( S4OFF_WRITE )
   // AS Jun 30/03 - moved to d4data.h, for support for d4packWithProgress
   typedef struct
   {
      DATA4 *data;
      short( __stdcall *callback )( double );
      long sleepInterval ;
      short callbackStarted ;
      short reindexDone ;
      const char *indexFileName ;  // CS 2008/02/22 for i4createWithProgress
      const TAG4INFO *tags ;  // CS 2008/02/22 for i4createWithProgress
   } REINDEX4CALLBACK ;
#endif



#ifndef S4CLIENT
   /* Memo File Structures */
   typedef struct
   {
      S4UNSIGNED_LONG  nextBlock ;  /* Next available memo block.  S4FOX multiply by blockSize to get the physical byte offset into the file */

      #ifdef S4MFOX
         char  unused[2] ;
         short blockSize ;  /* Bytes */
      #else
         #ifndef S4MNDX
            S4LONG  zero ;
            char  fileName[8] ;
            short zero2 ;
            short x102 ;
            short blockSize ;  /* Bytes */
            short zero3 ;
         #endif
      #endif
   } MEMO4HEADER ;



   #ifndef S4MNDX
      #ifndef S4MFOX
         typedef struct
         {
            S4UNSIGNED_LONG  next ;     /* The next free block area (multiply by blockSize to get physical byte offset */
            S4UNSIGNED_LONG  num ;      /* The number of memo blocks in the free block area */

            int toDisk ;                /* TRUE if this information needs to be written to disk */
            S4UNSIGNED_LONG  blockNo ;  /* The current block number (multiply by blockSize to get physical byte offset */
         } MEMO4CHAIN_ENTRY ;
      #endif /*  ifndef S4MFOX  */



      typedef struct
      {
         #ifdef S4MFOX
            // AS Jun 19/02 - New type of '3' indicates compressed - for data file supporting compressed memos only
            S4LONG type ;         /* 0 for picture, 1 for text, 2 for OLE -- picture not supported */
            S4UNSIGNED_LONG numChars ;    /* # bytes in the memo, Including the 'MemoBlock' */
         #else
            short minusOne ;    /* '-1' for dBASE IV */
            short startPos ;    /* the offset of the data into the memoblock == sizoef( S4LONG ) + 2 * sizeof( short ) - i.e. after header info... */
            S4UNSIGNED_LONG  numChars ;    /* Including the 'MemoBlock' */
         #endif
      } MEMO4BLOCK ;
   #endif  /*  ifndef S4MNDX  */
#endif  /*  ifndef S4CLIENT  */



typedef struct
{
   unsigned  char  sigDig ;  /* The number of significant digits; 52 is zero */
   unsigned  char  digitInfo ; /* contains one, len and sign */
   unsigned  char  bcd[10] ;
}  C4BCD ;



#ifndef S4OFF_MULTI
   /* for a single lock */
   typedef struct
   {
      long recNum ;    /* if appropriate */
      // AS Apr 15/03 - support for new lockId for shared clone locking
      long lockId ;
      long serverId ;
      short int type ;
      char blank[2] ;   /* communications (esp. unix client) requires */
   } LOCK4ID ;



   typedef struct
   {
      SINGLE4 link ;
      DATA4 *data ;
      LOCK4ID id ;
   } LOCK4GROUP ;



   #ifdef S4CLIENT
      /* S4CLIENT, not S4OFF_MULTI */
      typedef struct
      {
      /* public: */
         SINGLE4 link ;
         // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
         long lockId ;
         long serverId ;
         //         DATA4 *data ;
         long   recNo ;
         #ifdef S4TESTING
            enum Lock4type lockType ;
         #endif
      } LOCK4LINK ;
   #endif  /* S4CLIENT */
#endif  /* S4OFF_MULTI */



typedef struct ENTRYINFO5st
{
   char identifier[12];
   int  length;
}ENTRYINFO5 ;



// AS Oct 24/03 - Support for file_extended with MDX
#if defined(S4FILE_EXTENDED)
   /* moved the definition of the C4STAMP structure to reduce multiple copies */
   #include "d4stamp.h"
#endif



#ifdef S4USE_LOW_MEMORY
   typedef union
   {
      unsigned long block ;
      struct
      {
         unsigned short protect ;
         unsigned short real ;
      } ptr ;
   } FIXED4MEM ;
#else
   typedef void * FIXED4MEM ;
#endif /* S4USE_LOW_MEMORY */



#if defined(S4TESTING) && !defined(S4SERVER)
   typedef struct
   {
      unsigned char T4SUITE[255] ;
      unsigned char T4RECNO[100] ;
   } ATS4RECINFO ;
#endif


#if defined(S4WINCE) || !defined(__oledb_h__)  /* CS 2001/06/20 Win CE defines __oledb_h__ but not DBDATE and DBTIME
   /* CJ - 03/22/99 - Using OLEDB structures inside the CodeBase DLL normally
   included by oledb.h instead defined here for convenience */
   /* CS - 1999/04/05 Changed structure members from USHORT etc. to
      unsigned short etc. to compile in 16-bit. */

   typedef struct  tagDBDATE
   {
      short year;
      unsigned short month;
      unsigned short day;
   } DBDATE;



   typedef struct  tagDBTIME
   {
      unsigned short hour;
      unsigned short minute;
      unsigned short second;
   } DBTIME;



   typedef struct  tagDBTIMESTAMP
   {
      short year;
      unsigned short month;
      unsigned short day;
      unsigned short hour;
      unsigned short minute;
      unsigned short second;
      unsigned long fraction;
   } DBTIMESTAMP;
#endif /* !defined(OLEDB5BUILD) || !defined(OLEDB5ERROR) */



typedef struct
{
   unsigned long lo[4] ;
} QUAD5LONG ;





// AS May 6/03 - Just make available always
// #if defined(S4CLIENT) || defined(S4DLL_BUILD) || defined(S4LIB_BUILD)   /* LY 2002/11/18 : for pipe4*** in e4not_s.c */
   typedef struct
   {
      CODE4 *c4 ;
      unsigned long id ;
      short timeout ;   // how long to timeout when waiting for receipt to send a message - default -1 wait forever (used if waitForReceipt is true)
                        // if msgWaitForReceipt is true and we get a timeout, the message send is cancelled
      short waitForReceipt ;    // default 0, should we wait to ensure client gets message or not?
   } PIPE4SEND ;



   typedef struct
   {
      CODE4 *c4 ;
      unsigned long id ;
      short timeout ;   // how long to timeout when waiting for a message - default -1 wait forever
   } PIPE4RECV ;
// #endif



#ifdef S4SERVER
   struct MESSAGE4RECEIVESt ;



   typedef struct
   {
      // set up by a client wanting to receive messages ;
      LINK4 link ;
      struct SERVER4CLIENTSt *owner ;
      unsigned long id ;
      long messageIdCount ;
      char *name ;
      LIST4 messageList ;
   } PIPE4 ;



   typedef struct
   {
      // used to track messages where client waits for message to be sent.
      LINK4 link ;
      struct MESSAGE4RECEIVESt *message ;
      struct SERVER4CLIENTSt *sender ;
   } MESSAGE4SEND ;



   typedef struct MESSAGE4RECEIVESt
   {
      LINK4 link ;
      unsigned long id ;
      unsigned long len ;
      void *data ;
      MESSAGE4SEND *messageSend ;    // only set if a client is waiting for a reply for the message
      PIPE4 *pipe ;
   } MESSAGE4RECEIVE ;
#endif

//CJ July 17/01-Macintosh client connection needs a thread easiest way is to declare your own class
//that inherits the powerplant thread class. Below is the control enum and thread class.
#ifdef S4MAC_TCP
   enum Thread4status
   {
      /* these are the various thread statuses that will control the acction of the client thread */
      thread4connect,
      thread4send,
      thread4receive,
      thread4idle,
      thread4disconnect,
      thread4disconnected
   } ;

   class Connection4macClient: public LThread
   {
   public:
      Connection4macClient( LTCPEndpoint* NetworkEndpoint ) ;
       ~Connection4macClient();

      int init( S4LONG sizeOfBuffer ) ;
      void StartDisconnect() ;
      void Connect( C4ADDRESS *ServerAddress ) ;
      void SendData( void *bufferIn, S4LONG len ) ;
      void StartReceiveData( S4LONG len ) ;
      void ReceiveData( void *bufferOut ) ;
      int GetReturnStatus( Thread4status *value ) ;
      inline LInternetAddress *GetLocalAddress() { return ( mEndpoint->GetLocalAddress() ) ; }
      inline EEndpointState GetEndpointState() { return( mEndpoint->GetState() ) ; }

   protected:
      virtual void* Run();
      unsigned char *dataTransferBuffer ;
      unsigned char *positionInBuffer ;

      LTCPEndpoint *mEndpoint;         // network endpoint
      C4ADDRESS *mConnectionAddress ;    // address of the server.
      Bool5 mStartDisconnect;               // true if StartDisconnect() was called
      Bool5 mConnected ;
      Thread4status mStatus ;
      int mReturnStatus ;
      UInt32 retrievedLength ;
      UInt32 RetrievedLengthLeft ;
      UInt32 totalReceived ;
      S4LONG sentLength ;
   } ;
#endif
