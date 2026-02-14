/* d4data.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

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
   collate4test = 4            // test collation.  Users may use this one for customized ones...
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



   #define NUM4AVAIL_COLLATION_ENTRIES 4

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

#if !defined(S4OFF_COMMUNICATIONS) && defined(S4CLIENT)
   struct CONNECTION4St ;
#endif

typedef struct S4EXPORT l4linkSt
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
      unsigned long pos ;
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
      unsigned long readStartPos ;
      unsigned long writeStartPos ;
      unsigned long writeCurPos ;

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
            CRITICAL_SECTION critical4optWrite ;
            int delayLargeBufferAvail ;
            int writeBufferActualAvail ;
         #else
            LIST4 space1 ;   /* extra blocks to allow efficient delay-writing */
            char S4PTR *space2 ;
            char S4PTR *space3 ;
            CRITICAL_SECTION space4 ;
            int space5 ;
            int space6 ;
         #endif
      #endif

      #ifdef S4WIN32
         #ifdef S4ADVANCE_READ
            char S4PTR *advanceLargeBuffer ;
            CRITICAL_SECTION critical4optRead ;
            #ifdef S4CBPP
               FILE4 S4PTR *advanceReadFile ;
            #else
               struct FILE4St S4PTR *advanceReadFile ;
            #endif
            int advanceLargeBufferAvail ;
            unsigned long advanceLargePos ;
            unsigned int advanceLargeLen ;
         #else
            char S4PTR *space7 ;
            CRITICAL_SECTION space8 ;
            #ifdef S4CBPP
               FILE4 S4PTR *space9 ;
            #else
               struct FILE4St S4PTR *space9 ;
            #endif
            int space10 ;
            unsigned long space11 ;
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
         #define file4longGreater( f1, val ) ( (f1).piece.longLo > (val) || (f1).piece.longHi > 0 )
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
         #define file4longLessLong( f1, f2 ) ( (f1) < (f2) )
      #endif /* #ifndef S4UNIX */
   #else
      #define FILE4LONG S4LONG
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
   #define file4longMultiply( f1, f2 ) ( (f1) *= (f2) )
   #define file4longSetLo( f1, val ) ( (f1) = (val) )
   #define file4longSubtract( f1, val ) ( *(f1) -= (val) )
   #define file4longSubtractLong( f1, f2 ) ( *(f1) -= *(f2) )
   #define file4longDivide( f1, f2 ) ( (f1) /= (f2))
   #define file4longLessLong( f1, f2 )( (f1) < (f2) )
   #define file4longEqualLong( f1, f2 ) ( (f1) == (f2) )
   /* LY 4/29/99 */
   #define file4longLessEqLong( f1, f2 ) ( (f1) <= (f2) )
#endif /* S4FILE_EXTENDED */



/* structure whose size, when added to a FILE4LONG, will be 10 */
typedef struct FILE4LONG_EXTENDSt
{
   char space[10 - sizeof( FILE4LONG )] ;
} FILE4LONG_EXTEND ;


#ifndef S4INTERNAL_COMPILE_CHECK
   #define file4len( f4 ) ( file4longGetLo( file4lenLow( f4 ) ) )
#endif


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
   char isTemp ;             /* True if it is a temporary file */

   #if   defined(S4WIN32)
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
      int lowAccessMode ;       /* open access level */
   #else
      int space25 ;
   #endif

   #ifdef S4FILE_EXTENDED
      char isLong ;
   #else
      char space26 ;
   #endif
   #ifndef S4OFF_OPTIMIZE
      char fileCreated ;     /* false if the file has not been created yet - i.e. if a memory-only file */
      char writeBuffer ;    /* buffer writes where possible */
      long hashInit ;
      FILE4LONG len ;            /* internal if optimized */
      FILE4LONG_EXTEND space0 ;
      char type ;           /* dbf, index, other */
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
      char  space6 ;
      int   space7 ;
      long  space11 ;
      double space12 ;
      const void *space13 ;
   #endif

   #ifdef S4WIN32
      #ifdef S4MULTI_THREAD
         #ifdef S4WIN64 /* LY 00/10/09 : address misalignment */
//            BYTE spaceIA1 ;
//            WORD spaceIA2 ;
         #endif
         CRITICAL_SECTION critical4file ;
      #else
         #ifdef S4WIN64
//            BYTE spaceIA1 ;
//            WORD spaceIA2 ;
         #endif
         CRITICAL_SECTION space29 ;
      #endif
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
      unsigned advanceReadBufPos ;   /* we don't support > 4 gigs for advance reading */
      unsigned advanceReadBufLen ;
   #else
      int space30a ;
      LIST4 space30b ;
      char *space30c ;
      int space30d ;
      unsigned space30e ;
      unsigned space30f ;
   #endif

   #ifdef E4ANALYZE_ALL
      char dupName[L_tmpnam] ;
      int hasDup ;
      int inUse ;
   #endif
   #ifdef S4ENCRYPT_HOOK
      short encrypted ;     // true if the file is using encryption
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
      FILE4LONG pos ;   /* we don't support files > 4 gigs for delay-writing */
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
      unsigned pos ;   /* we don't support files > 4 gigs for delay-writing */
      unsigned len ;
      int usageFlag ;
      unsigned status ;

      S4ADVANCE_FUNCTION *completionRoutine ;
      void *completionData ;   /* routine-specific data for use in completion routine */
   } FILE4ADVANCE_READ ;
#endif

typedef struct
{
   #ifdef S4WINCE
      char year[4] ;
      char month ;
      char day ;
      char second[4]; //Seconds past midnight.
   #else
      S4LONG year ;
      char  month ;
      char  day ;
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
      char userId[LEN4USERID+1] ;
      char netId[LEN4NETID+1] ;
   #endif
} TRAN4 ;



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
   #ifdef S4WIN64 /* LY 00/10/09 : address misalignment of file */
//      DWORD spaceIA1 ;
//      WORD spaceIA2 ;
   #endif
   FILE4  file ;

   FILE4LONG fileLen ;   /* used to track file len to avoid costly file4len calls when possible */
   FILE4LONG_EXTEND pad;  /* pad size of FILE4LONG to constant size */

   #ifdef S4TRANS_FILE_SHARED
      // AS 11/22/99 moved from TRAN4, since this value is used by the transaction file,
      // not the TRAN4.  Note that this userIdNo is the user # of the shared transaction
      // file.  In the shared server implementation (SIMBA) multiple TRAN4 or clients may
      // use the transaction file, but they all use this base userIdNo to generate
      // their transaction id's.  See function tran4fileGetNextTransId for more details.
      int userIdNo ;    /* used to determine the id of user with a shared transaction file */
   #endif

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



#ifndef S4OFF_THREAD
   typedef struct SEMAPHORE4St
   {
      LINK4 link ;   /* Allows the SEMAPHORE4 to be on a list of SEMAPHORE3s */
      #ifdef S4WIN32
         HANDLE handle ; /* Handle to the semaphore itself */
      #endif
   } SEMAPHORE4 ;
#endif


#if defined( S4SERVER ) && ( defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED ) )
   typedef struct
   {
      HANDLE mutex ;
      char isValid ;
   } MUTEX4 ;
#endif

#ifndef S4STAND_ALONE
   #define C4ADDRESS struct sockaddr_in
   #define C4NETID struct in_addr
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



      typedef struct LIST4MUTEXSt
      {
         LIST4 list ;
         HANDLE mutex ;
         char isValid ;
      } LIST4MUTEX ;



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
      typedef struct
      {
         SOCKET sockw ;
         SOCKET sockr ;
         // struct sockaddr addressw ;  // set when needed to the address of the sockw socket
         C4ADDRESS addressw ;
         // AS 02/06/01 - method to disable dual sockets...
         // short useDualSocket ;
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
         CONNECT4THREAD *connectThread ;  /* Conenction associated with this message */
         long messageLen ;
            /* message array length will be equal to CODE4.readMessageBufferLen,*/
            /* which is configurable in the server configuration file and will */
            /* have a #define default for the client (S4READ_MESSAGE_DEFAULT_LEN*/
         unsigned long messageBufferLen ;    /* Length of the buffer */
         char message[1] ;
      } NET4MESSAGE ;
   #endif /*!S4OFF_THREAD */



   #ifdef S4SERVER
      struct SERVER4CLIENTSt ;
   #endif



   typedef struct CONNECT4BUFFERSt
   {
      LINK4 link ;
      #ifdef S4CBPP
         CODE4 S4PTR *cb ;
      #else
         struct CODE4St *cb ;
      #endif

      int connected ;
      CONNECT4LOW *connectLow ;

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
   } CONNECT4BUFFER ;



   typedef struct CONNECT4St
   {
      LINK4 link ;
      #ifdef S4CBPP
         CODE4 S4PTR *cb ;
      #else
         struct CODE4St *cb ;
      #endif

      /* sturcture used to communicate with the communications thread */
      CONNECT4BUFFER connectBuffer ;
      #ifndef S4OFF_BLASTS
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
         C4ADDRESS address ;
         short addrLen ;
         long clientId ;
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
typedef void ( *ERROR_CALLBACK)
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
) ;  // CS 2000/03/26  add for error callback


#ifdef S4CBPP
class S4CLASS CODE4
{
public:
#else
typedef struct CODE4St
{
#endif
   /* documented members... (except ifdef'ed ones) */

   S4CONV( int autoOpen, int auto_open ) ;               /* Automatic production index file opening */
   int createTemp ;                                      /* create files as temporary ? */
   S4CONV( short errDefaultUnique, short default_unique_error ) ; /* e4unique, r4unique, r4uniqueContinue */
   S4CONV( int errExpr, int expr_error ) ;
   S4CONV( int errFieldName, int field_name_error ) ;
   S4CONV( int errOff, int off_error ) ;                 /* Show error messages? */
   S4CONV( int errOpen, int open_error ) ;               /* Do 'file4open' error ? */
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

   /* AS 02/23/99 cannot use a shared work buf because (at least in server) may evaluation
      an expression within an evaluation if an expression includes a calc.  Therefore, sharing
      this memory does not work.  Must keep the exprWorkBuf on an EXPR4 level

      unsigned exprBufLen ;        - for exprWorkBuf
      char S4PTR *exprWorkBuf ;    - used by expression parsing
   */
   char S4PTR *storedKey ;      /* used by the expr4key() function */
   char S4PTR *fieldBuffer ;
/*   char S4PTR *version ;   I don't think this is used anywhere anymore... */

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

   char oledbSchemaCreate ;
   short useGeneralTagsInRelate ;  /* used to affect whether can use optimization on general tags (is faster but inconsistent results) */ /* CS 2000/10/09 Should be short */

   #ifdef S4SERVER
      /* for the server, the values may not be available as they reside in
         the SERVER4CLIENT structure.  Use defaults.  Also use defaults to
         set SERVER4CLIENT values. */
      int readOnlyDefault ;
      int errorCodeDefault ;   /* used for when no client struct only */
      long errorCode2Default ; /* used for when no client struct only */
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
         S4CONV( unsigned memSizeMemoExpr, unsigned mem_size_memo_expr ) ;
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
      S4CONV( unsigned memSizeMemoExpr, unsigned mem_size_memo_expr ) ;
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

   #if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
      char *transFileName ;
   #else
      char *space_transFileName ;
   #endif

   #ifndef S4OFF_CATALOG
      #ifndef S4CLIENT
         CATALOG4 S4PTR *catalog ;
      #endif
   #endif

   #ifndef S4OFF_TRAN
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

   #ifdef S4STAND_ALONE
      #ifndef S4OFF_TRAN
         #ifndef S4OFF_WRITE
            TRAN4FILE transFile ;
            int logOpen ;
         #endif
      #endif
   #endif

   #ifdef S4WIN32
      #ifdef S4WRITE_DELAY
         int delayWritesEnabled ;
         #ifdef S4WIN64 /* LY 00/10/06 : address misalignment in code4initLow() */
//            WORD spaceIA3 ;
         #endif
         CRITICAL_SECTION critical4delayWriteList ;
         LIST4 delayWriteList ;
         #ifdef S4WIN64 /* LY 00/10/06 : address misalignment in code4initLow() */
//            DWORD spaceIA4 ;
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
//            WORD spaceIA3 ;
         #endif
         CRITICAL_SECTION space1b ;
         LIST4 space1c ;
         #ifdef S4WIN64
//            DWORD spaceIA4 ;
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
         #ifdef S4WIN64 /* LY 00/10/06 : address misalignment in code4initLow() */
//            DWORD spaceIA5 ;
         #endif
         CRITICAL_SECTION critical4advanceReadList ;
         LIST4 advanceReadList ;
         MEM4 S4PTR *advanceReadMemory ;
         HANDLE initUndoAdvanceRead ;
         long uninitializeAdvanceRead ;
         HANDLE pendingReadEvent ;
      #else
         int space2a ;
         #ifdef S4WIN64
//            DWORD spaceIA5 ;
         #endif
         CRITICAL_SECTION space2b ;
         LIST4 space2c ;
         MEM4 S4PTR *space2d ;
         HANDLE space2e ;
         long space2f ;
         HANDLE space2g ;
      #endif
   #endif
   #if defined( S4TRANS_FILE_SHARED ) && defined( S4TESTING )
      /* allow sequential handling of transaction file by not doing locking
         i.e. for same program to have multiple transactions */
      int doTransLocking ;
   #endif

   #ifndef S4SERVER
      // in server, doRemove is on a client by client basis, so don't use this here
      int doRemove ;
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
   #else
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

      #ifndef S4OFF_TRANS
         CODE4TRANS c4trans ;
      #else
         CODE4TRANS space_c4trans ;
      #endif
      S4CONV( MEM4 S4PTR *totalMemory, MEM4 S4PTR *total_memory ) ;
      S4CONV( LIST4 calcList, LIST4 calc_list ) ;
      S4CONV( LIST4 totalList, LIST4 total_list ) ;
      S4CONV( int numReports, int num_reports ) ;
      short pageno ;
      long clientDataCount ;
   #endif

   #ifdef S4CLIENT
      enum index4format indexFormat ;
      short openForCreate ;
      long serverOS ;   // bit mask: OS4UNKNOWN, OS4WIN32, OS4LINUX...
   #else
      int doIndexVerify ;       /* for internal purposes only at this point */
      struct RELATION4St S4PTR *currentRelation ;
      char savedKey[I4MAX_KEY_SIZE + 2 * sizeof(S4LONG)] ;       /* used by i4remove.c, i4tag.c and i4addtag.c, i4versionCheck, t4versionCheck */
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
         /* LIST4 availConnects; list of CONNECT4s currently not used */
         char *savedData ;
         unsigned savedDataLen ;
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
         #ifdef S4HALFJAVA
            MEM4 S4PTR *halfJavaMemory ;
            #ifdef S4OFF_THREAD
               LIST4 halfJavaList ;
            #else
               LIST4MUTEX halfJavaListMutex ;
            #endif
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
   #endif

   #ifdef TIMER5OUT
      /* case where we want to perform timing of server operations */
      FILE4 timerFile ;
      TimerMemory5 *timerMemory ;
      Timer5 *currentTimer ;
      Timer5 *masterTimer ;
   #endif
   #if defined(S4WIN32)
      #if defined(__cplusplus)
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
   #endif

   #ifdef S4SERVER
      Bool5 runAsService ;   // set to true if the server is run as a service.  Disables message boxes
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
   #ifdef S4ENCRYPT_HOOK
      short encrypt ;        // should the next open/create use encryption?
      void *encryptInit ;    // pointer to hook-provided encryption structure information
      void *fileEncryptedBuffer ;   // used to buffer data after encrypting
      unsigned long fileEncryptedBufferLen ;  // allocated length of above buffer
   #endif
   // #ifndef S4STAND_ALONE
      // AS 02/06/01 - method to disable dual sockets...
      // short useDualSocket ;
   // #endif

   ERROR_CALLBACK errorCallback ;  // CS 2001/03/26 error callback function pointer
#ifdef S4CBPP
   } ;
#else
   } CODE4 ;
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
      char nullBinary ;    /* for FOX 3.0 0x02 == allows null fields, 0x04 == binary field, 0x08 autoIncrement field */
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
   char name[11] ;
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
   #endif
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
   char     version ;
   char     yy ;             /* Last Update */
   char     mm ;
   char     dd ;
   S4LONG   numRecs ;
   unsigned short headerLen; /* Header Length, Indicates start of data */
   unsigned short recordLen;
   char     flags[8] ;           // with CodeBase, indicates support available -- for new additions to header...
   double   autoIncrementVal ;   // current autoIncrement value
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
      /*
         virtual void compare( T *found, void *key ) = 0 ;
         virtual Single4 *nextLink( T *current ) = 0 ;
         T *retrieve( unsigned long inputVal, void *key )
         {
            int counter = 0 ;  // if counter exceeds numEntries, we grow

            int hVal = hash( inputVal ) ;
            Single4 *next = ptrs[hVal];
            for
            {
               if ( next->isLastLink() == 1 )  // assume done
               {
                  // for debug purposes, ensure the item matches...
                  #ifdef E4HASH
                     assert5( compare( (T *)found, key ) == 0 ) ;
                  #endif
                  return found ;
               }
               // otherwise must do a comparison
               if ( compare( (T *)found, key ) == 0 )
                  return found ;
               next = next->next() ;
            }
         }
         #ifdef S4SERVER
            int lockTest( struct DATA4St *data, long clientId, long recNo ) ;
         #else
            int lockTest( struct DATA4St *data, long recNo ) ;
         #endif
      */
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
   long userCount ;      /* number of DATA4's that are using this DATA4FILE */

   CODE4 S4PTR *c4 ;
   char S4PTR *info ;

   /**** the next set of lines must remain in order as they are a file view ****/
      /* Database Header Information */
      char version ;
      #ifndef S4CLIENT
         char     yy ;             /* Last Update */
         char     mm ;
         char     dd ;
      #endif
      S4LONG   numRecs ;
      unsigned short headerLen ; /* Header Length, Indicates start of data */
      unsigned short recordLen;
      char     flags[8] ;           // with CodeBase, indicates support available -- for new additions to header...
      double   autoIncrementVal ;   // current autoIncrement value
   /**** the previous set of lines must remain in order as they are a file view ****/

   #ifndef S4OFF_WRITE
      int doDate ;    /* does the date need to be updated on unlock/close? */
   #endif

   #ifdef S4CLIENT
      struct DATA4St S4PTR *fileLock ;
      struct DATA4St S4PTR *appendLock ;
      struct DATA4St S4PTR *lockTest ;
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
         int compatibility ;   /* 2.5/2.6 compatible option */
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
   #endif
   #ifdef __cplusplus
      #ifdef S4CLIENT
         Single4 lockedRecords ;
      #endif
      #ifndef S4OFF_MULTI
         Single4 fileReadLocks ;   // uses Lock4's for convenience
      #endif
   #endif
} DATA4FILE ;



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
      long serverId ;
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
   #endif
   // AS 02/01/01 - Mark in data4 whether was logged so know if need to log the close message
   #ifndef S4OFF_TRAN
      Bool5 openWasLogged ;
   #endif
} DATA4 ;



typedef void S4OPERATOR(void) ;



/* NOTE: IT IS CRITICAL THAT THE FOLLOWING STRUCTURE'S MEMBERS ARE NOT
   RE-ORDERED. */
typedef struct E4INFOSt
{
   short int fieldNo ;   /* field no in data structure -- since FIELD4 may be transient */
   FIELD4 S4PTR *fieldPtr ;
   int localData ;       /* true if the fieldPtr points to a local data4 */
   char S4PTR *p1 ;
   int len ;         /* Length */
   int numEntries ; /* Number of entries in sub-expression */
   int numParms ;
   int resultPos ;  /* The position in the result array for result. */
   int i1 ;          /* Could be constant position. 'i1' and 'resultPos'
                       and 'functionI' and 'function'
                       must be at last position due to memcmp() in e4isTag() */
   int i2 ;  // AS 11/16/99 --> used for IIF as length of second string paramater...
   int functionI ;
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
// AS 07/27/99 -- this has been superseded by more simple collations for foxPro
//   #ifdef S4FOX
//      struct T4VFPSt S4PTR *vfpInfo ;
//   #else
//      void S4PTR *space1 ;
//   #endif
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
         short      nodeAttribute ;    /* 0=index, 1=root, 2=leaf */
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
         int curTrailCnt ;   /* current value used for seeking */
         int curDupCnt ;     /* current value used for seeking */
         int dupPos ;        /* bit offset into the info for the duplicate data */
         int trailPos ;      /* bit offset into the info fonfo for the trail data */
         int recPos ;        /* bit offset into the info for the record # data */
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
      typedef unsigned long B4NODE ;
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

      #ifdef S4CLIPPER
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
   #ifndef S4CLIENT
      #ifndef S4OFF_TRAN
         LIST4 removedKeys ;     /* list of temporarily removed keys for r4unique and e4unique cases */
      #endif
      int added, removed ;             /* was an entry added, removed */
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

   #ifndef S4OFF_TRAN
      int isValid ;
   #endif
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
         char S4PTR *path ;
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



#ifndef S4CLIENT
   /* Memo File Structures */
   typedef struct
   {
      S4UNSIGNED_LONG  nextBlock ;  /* Next available memo block.  S4FOX multiply by blockSize to get the physical byte offset into the file */

      #ifdef S4MFOX
         char  usused[2] ;
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
      long clientId ;
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
         DATA4 *data ;
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



#if defined(S4FILE_EXTENDED) && defined(S4FOX)
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



#if !defined(__oledb_h__)  /* CS 2000/12/01 */
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

#if defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) )
   typedef struct
   {
      HANDLE sharedMemoryHandle ;
      int maxLen ;
      int wasU4alloc ;  // true if used u4alloc to allocate, false if used malloc
   } SHARE4 ;
#endif



typedef struct
{
   unsigned long lo[4] ;
} QUAD5LONG ;





#ifdef S4CLIENT
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
#endif



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
