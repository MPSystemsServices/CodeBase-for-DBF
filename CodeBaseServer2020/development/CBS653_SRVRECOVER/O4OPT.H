/* o4opt.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* these defines must be here even if no optimization for documentation purposes */

#define OPT4AVAIL -1
#define OPT4NONE   0
#define OPT4DBF    1
#define OPT4INDEX  2
#define OPT4OTHER  3

#ifndef S4OFF_OPTIMIZE

#define OPT4DBF_LO_MIN_LINK     .05
#define OPT4DBF_LO_MAX_TIME     .25
#define OPT4DBF_LO_MIN_TIME     .25
#define OPT4DBF_HI_MIN_LINK     .1
#define OPT4DBF_HI_MAX_TIME    1.0
#define OPT4DBF_HI_MIN_TIME    1.0
#define OPT4INDEX_LO_MIN_LINK   .05
#define OPT4INDEX_LO_MAX_TIME   .25
#define OPT4INDEX_LO_MIN_TIME   .25
#define OPT4INDEX_HI_MIN_LINK   .15
#define OPT4INDEX_HI_MAX_TIME  2.0
#define OPT4INDEX_HI_MIN_TIME  2.0
#define OPT4OTHER_MIN_LINK      .05
#define OPT4OTHER_MAX_TIME      .1
#define OPT4OTHER_MIN_TIME      .1

/* OPT4CHECK_RATE must be a minimum of 5 in order to optimize correctly */
#define OPT4CHECK_RATE        15

/* factor of 2 that estimates the hash distribution, #slots = between #blocks * OPT4BLOCK_DENSITY and #blocks * OPT4BLOCK_DENSITY * 2 */
#define OPT4BLOCK_DENSITY 2L

/* this structure must match OPT4BLOCK with opt4=file followed by pos=pos */
typedef struct
{
   FILE4 S4PTR *file ;
   long pos ;
} OPT4CMP ;

#ifdef __cplusplus
   extern "C" {
#endif

S4EXPORT int S4FUNCTION c4calcType( unsigned long ) ;

int d4updatePrio( CODE4 * ) ;
void file4setWriteOpt( FILE4 *, int ) ;
#ifdef S4ADVANCE_READ
   void opt4fileAdvanceRead( FILE4 *, long, unsigned ) ;
#endif

int opt4blockClear( OPT4BLOCK * ) ;
int opt4blockRemove( OPT4 *, OPT4BLOCK *, int ) ;

int opt4fileDelete( FILE4 *, const unsigned long, const unsigned long ) ;
int opt4fileFlushList( OPT4 *, FILE4 *, LIST4 *, int ) ;
/*long opt4fileHash( OPT4 *, FILE4 *, unsigned long ) ; */
#define opt4fileHash( opt, file, pos )  ((long)( (( (file)->hashInit + (pos) ) >> (opt)->blockPower ) & (opt)->mask ))
void opt4blockLruTop( OPT4BLOCK * ) ;
unsigned opt4fileRead( FILE4 *, unsigned long, void *, unsigned ) ;
OPT4BLOCK *opt4fileReturnBlock( FILE4 *, unsigned long, long ) ;
int opt4fileWrite( FILE4 *, unsigned long, unsigned, const void *, char ) ;

int opt4fileFlush( FILE4 *, const int ) ;
int opt4flushAll( OPT4 *, char ) ;
int opt4flushWriteBuffer( OPT4 * ) ;

#ifdef __cplusplus
   }
#endif

#endif /* S4OFF_OPTIMIZE */
