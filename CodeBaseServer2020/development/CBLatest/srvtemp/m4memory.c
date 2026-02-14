/* m4memory.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifdef S4OS2
   #ifdef __DLL__
      #define  INCL_DOSMEMMGR
   #endif
#endif
#include "d4all.h"

#ifdef S4VB_DOS
   #include "malloc.h"
#endif

#ifdef __TURBOC__
   #pragma hdrstop
#endif

#if defined( S4SERVER ) && defined( S4MEM_PRINT ) && defined( S4TESTING )
   // include files for outputting memory stuff
   #include "t4test.c"
#endif

#ifdef S4MAX
   #define S4MAX_OR_SERVER
#else
   #ifdef S4SERVER
      #define S4MAX_OR_SERVER
   #else
      #ifdef S4TRACK_MEMORY
         #define S4MAX_OR_SERVER
      #endif
   #endif
#endif

#ifdef S4OS2
   #ifdef __DLL__
      #include <bsememf.h>
   #endif
#endif

/* for 32 bit, allow more than 20000 memory allocations... */

#ifdef S4WIN32
   // AS Apr 15/09 - allow this to be larger since it is not all that high for some cases...
   #define MEM4MAX_POINTERS 1000000
#else
   #define MEM4MAX_POINTERS 20000
#endif

#define mem4numTypes 10

#ifdef S4MEM_PRINT
   #ifdef S4WIN_ALLOC
      #ifdef S4TESTING
         #include "t4test.h"
      #endif
   #endif
   #include <stdio.h>
   // stdprn no longer available in Microsoft...
   int v4print = 1 ; /* if v4print == -1 no output, if 0 only u4alloc/u4free output, if 1 all output  */
   /* use globals to hold the current file name and line number */
   const char *m4fileName = 0 ;
   int m4lineNo = 0 ;
   #ifdef S4WINDOWS
      int  S4FUNCTION code4memFileName( void )
      {
         return m4lineNo ;
      }

      const char *S4FUNCTION code4memFileNo( void )
      {
         return m4fileName ;
      }

      void S4FUNCTION code4memLineNoSet( int val )
      {
         m4lineNo = val ;
      }

      void S4FUNCTION code4memFileNameSet( const char *ptr )
      {
         m4fileName = ptr ;
      }
   #endif
#endif

typedef struct
{
   LINK4  link ;
   MEM4  types[mem4numTypes] ;
} MEMORY4GROUP ;

#ifdef S4SEMAPHORE   /* LY 2002/02/20 : switched with S4WIN32 */
   // LY Sep 10/04 : changed from S4LINUX to S4UNIX
   #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : added S4LINUX */
      #ifdef S4LINUX  /* LY July 8/03 : added static */
         static int memoryInitialized = 0 ;  // keeps a count of # of inits, must match # resets
      #else
         int memoryInitialized = 0 ;  // keeps a count of # of inits, must match # resets
      #endif
   #endif
   #ifdef S4WIN32
      /* multi-thread support */
      CRITICAL_SECTION critical4memory, critical4expression ;
      #ifndef S4STAND_ALONE
         CRITICAL_SECTION critical4compression ;
      #endif
      #ifdef E4ANALYZE
         // track who is holding memory and their count on it (should never exeed 1)
         static long g_holdCount = 0 ;
         CODE4 *g_holder ;

         int mem4start( CODE4 *c4 )
         {
            assert5( memoryInitialized >= 1 ) ;
            EnterCriticalSection( &critical4memory ) ;
            assert5( g_holdCount == 0 || g_holder == c4 || c4 == 0 ) ;
            g_holdCount++ ;
            if ( c4 != 0 )
               g_holder = c4 ;
            return 0 ;
         }



         int mem4stop( CODE4 *c4 )
         {
            assert5( memoryInitialized >= 1 ) ;
            assert5( g_holdCount > 0 ) ;
            assert5( g_holder == c4 || c4 == 0 ) ;
            g_holdCount-- ;
            if ( g_holdCount == 0 )
               g_holder = 0 ;
            LeaveCriticalSection( &critical4memory ) ;
            return 0 ;
         }
      #endif
   #endif
   // LY Sep 10/04 : changed from S4LINUX to S4UNIX
   #ifdef S4UNIX /* LY 2002/02/20 */
      #include <errno.h>
      pthread_mutex_t critical4memory ;
      // LY Sep 10/04 : removed static initialize from critical4expression for non-Linux implementations
      pthread_mutex_t critical4expression ;
      /* LY 2003/07/08 : added static */
      static pid_t pidLocking ;
      static unsigned long numLocks ;
   #endif
#endif

#ifndef S4CBPP
   static LIST4  avail = { 0, 0, 0 } ;   /* A list of available MEM4 entries */
   static LIST4  used = { 0, 0, 0 } ;   /* A list of used MEM4 entries */
   static LIST4  groups = { 0, 0, 0 } ; /* A list of Allocated MEM4 groups */
#else
   /* initialization with class and union structures */
   static LIST4  avail  = { {0}, 0, {0} } ;   /* A list of available MEM4 entries */
   static LIST4  used   = { {0}, {0}, {0} } ;   /* A list of used MEM4 entries */
   static LIST4  groups = { {0}, 0, {0} } ;   /* A list of Allocated MEM4 groups */
#endif

int resetInProgress = 0 ;

#ifdef S4SEMAPHORE
// LY Sep 10/04 : changed from !S4LINUX to !S4UNIX
#if !defined( S4WIN32 ) && !defined(S4UNIX) /* added !S4LINUX */
static int mem4start( CODE4 *c4 )
{
   APIRET rc ;

   #ifdef E4ANALYZE
      if ( c4 == 0 )
         return error4( c4, e4info, "OS/2 Semaphore Failure" ) ;
   #endif

   rc = DosRequestMutexSem( c4->hmtxMem, -1 ) ;
   if ( rc != 0 )
      return error4( c4, e4info, "OS/2 Semaphore Failure" ) ;
   return 0 ;
}

static void mem4stop( CODE4 *c4 )
{
   #ifdef E4ANALYZE
      if ( c4 == 0 )
      {
         error4( c4, e4info, "OS/2 Semaphore Failure" ) ;
         return ;
      }
   #endif

   DosReleaseMutexSem( c4->hmtxMem ) ;
}
#endif /* S4WIN32 && S4UNIX */
// LY Sep 10/04 : changed from S4LINUX to S4UNIX
#ifdef S4UNIX /* LY 2002/02/20 */
   /* LY July 9/03 : removed static to avoid odd Linux compiler error */
   int S4FUNCTION mem4start( CODE4 *c4 )
   {
      int rc ;

      if ( memoryInitialized == 0 )
         return -1 ;

      #ifdef S4MEM_PRINT
         if ( v4print > 0 )
            printf( "\nmem4start:  CODE4: %lx  ", c4 ) ;
      #endif

      rc = pthread_mutex_trylock( &critical4memory ) ;

      if ( rc == EBUSY )  /* LY 2002/02/25 : can lock itself out */
      {
         if ( pidLocking == getpid() )
         {
            numLocks++ ;
            return 0 ;
         }
         else
            rc = pthread_mutex_lock( &critical4memory ) ;
      }

      #ifdef S4MEM_PRINT
         if ( v4print > 0 )
            printf( "rc: %d\n", rc ) ;
      #endif

      if ( rc != 0 )
         return error4( c4, e4info, E99999 ) ;

      pidLocking = getpid() ;
      numLocks++ ;
      return 0 ;
   }

   /* LY July 9/03 : removed static to avoid odd Linux compiler error */
   int S4FUNCTION mem4stop( CODE4 *c4 )
   {
      int rc ;
      if ( memoryInitialized == 0 )
         return 0 ;
      if ( ( pidLocking == 0 ) || ( numLocks == 0 ) )
         return error4( c4, e4info, E99999 ) ;

      #ifdef S4MEM_PRINT
         if ( v4print > 0 )
            printf( "\nmem4stop:  code4: %lx  rc: ", c4 ) ;
      #endif

      numLocks-- ;
      if ( numLocks == 0 ) /* for nested locks, release only outermost lock */
      {
         pidLocking = 0 ;
         rc = pthread_mutex_unlock( &critical4memory ) ;

         #ifdef S4MEM_PRINT
            if ( v4print > 0 )
               printf( "%d\n", rc ) ;
         #endif

         if ( rc != 0 )
            return error4( c4, e4info, E99999 ) ;

         #ifdef S4MEM_PRINT
            if ( v4print > 0 )
               printf( "0\n" ) ;
         #endif
      }

      return 0 ;
   }
#endif
#endif /* S4SEMAPHORE */

#ifdef E4MISC

static char **mem4testPointers ;
static int mem4numPointer = -1 ;
static int mem4numUsed = 0 ;

#ifdef S4DATA_ALIGN
   #define mem4extraChars 12
   #define mem4extraTot (2*(mem4extraChars + sizeof(S4LONG)))
#else
   #define mem4extraChars 10
   #define mem4extraTot (mem4extraChars*2 + sizeof(unsigned))
#endif
#define mem4checkChar 0x55

#define MEM4INC 500

/* Returns the pointer to be returned; is passed the pointer allocated by malloc ... */
static char *mem4fixPointer( char *startPtr, unsigned largeLen )
{
   char *returnPtr ;
   unsigned pos ;

   c4memset( startPtr, mem4checkChar, mem4extraChars ) ;
   returnPtr = startPtr + mem4extraChars ;

   c4memcpy( returnPtr, (void *)&largeLen, sizeof(largeLen) ) ;
   pos = largeLen - mem4extraChars ;
   c4memset( startPtr+ pos, mem4checkChar, mem4extraChars ) ;

   return returnPtr + sizeof(unsigned) ;
}



static char *mem4checkPointer( char *returnPtr, int clear )
{
   /* Returns the pointer allocated by malloc;  passed by pointer returned by 'mem4fixPointer' */
   unsigned *largeLenPtr ;
   char *mallocPtr, *testPtr ;
   int i, j ;

   largeLenPtr = (unsigned *)(returnPtr - sizeof(unsigned)) ;
   mallocPtr = returnPtr - sizeof(unsigned) - mem4extraChars ;

   for ( j = 0; j < 2; j++ )
   {
      if (j == 0)
         testPtr = mallocPtr ;
      else
         testPtr = mallocPtr + *largeLenPtr - mem4extraChars ;

      for ( i = 0 ; i < mem4extraChars ; i++ )
         if ( testPtr[i] != mem4checkChar )
         {
            #ifdef S4MEM_PRINT
               if ( v4print > 0 )
               {
                  #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
                     d4displayStr( mem4displayPtr, "corrupt memory: ", 1 ) ;
                     d4displayPtr( mem4displayPtr, mallocPtr- sizeof(unsigned) - mem4extraChars, 0 ) ;
                  #else
                     printf( "\ncorrupt memory: %p", mallocPtr- sizeof(unsigned) - mem4extraChars ) ;
                  #endif
               }
            #endif
            error4( 0, e4result, E85901 ) ;
            return 0 ;
         }
   }
   // AS July 24/02 - if E4LINK is defined always clear the memory to help debugging
   #ifndef E4LINK
      if ( clear == 1 ) /* null the memory to potentially detect re-use, including clearing check chars */
   #endif
   {
      #ifdef E4DEBUG
         if ( (*largeLenPtr) == 0 )   // this would mean we have an empty pointer, assume a memory error
         {
            error4( 0, e4memory, E85901 ) ;
            return 0 ;
         }
      #endif
      c4memset( mallocPtr, 0, *largeLenPtr ) ;
   }
   return mallocPtr ;
}


#ifdef E4ANALYZE
   // AS Dec 4/02 - To aid in debugging due to MSDEV bug causing MSDEV crash
   /* LY July 9/03 : added static */
   static char *g4cmp_ptr = 0 ;
#endif


static int mem4pushPointer( char *ptr )
{
   #ifdef S4WIN_ALLOC
      HANDLE handle, *hPtr, *oldHPtr ;
      hPtr = (HANDLE *)0 ;
   #endif

   if ( mem4numPointer < 0 )
   {
      #ifdef S4WIN_ALLOC
         #ifdef S4WINCE
            handle = LocalAlloc( LMEM_FIXED|LMEM_ZEROINIT, (UINT) sizeof(char *) * MEM4INC + sizeof(HANDLE) ) ;  /* L.Y. 99/10/21 */
         #else
            #ifdef __DLL__
               handle = GlobalAlloc( GMEM_MOVEABLE | GMEM_DDESHARE | GMEM_ZEROINIT, (DWORD) sizeof(char *) * MEM4INC + sizeof(HANDLE) ) ;
            #else
               handle = GlobalAlloc( GMEM_MOVEABLE | GMEM_ZEROINIT, (DWORD) sizeof(char *) * MEM4INC + sizeof(HANDLE) ) ;
            #endif
         #endif

         if ( handle == (HANDLE) 0 )
            return error4( 0, e4memory, E95901 ) ;

         #ifndef S4WINCE
            hPtr = (HANDLE *)GlobalLock( handle ) ;
         #else
            hPtr = (HANDLE *)LocalLock( handle) ;
         #endif

         *hPtr++ = handle ;
         mem4testPointers = (char **)hPtr ;
      #else
         mem4testPointers = (char **)malloc( sizeof(char *) * MEM4INC ) ;
      #endif
      mem4numPointer = MEM4INC ;
   }
   if ( mem4numPointer == mem4numUsed )
   {
      mem4numPointer += MEM4INC ;
      static int didMemoryWarning = 0 ;
      if ( mem4numPointer > MEM4MAX_POINTERS && didMemoryWarning == 0 )
      {
         // not really an error, lets do this as a warning instead...
         error4( 0, e4result, E95901 ) ;
         // just keep going, ignore the problem...
         didMemoryWarning = 1 ;
      }

      #ifdef S4WIN_ALLOC
         oldHPtr = (HANDLE *)(mem4testPointers) ;
         oldHPtr-- ;  /* get the actual handle */

         #ifndef S4WINCE
            GlobalUnlock( *oldHPtr ) ;
         #else
            LocalUnlock( *oldHPtr ) ;
         #endif

         #ifdef S4WINCE
            handle = LocalReAlloc( *oldHPtr, (UINT) sizeof(char *) * mem4numPointer + sizeof( HANDLE ), LMEM_MOVEABLE ) ;
         #else
            #ifdef __DLL__
               handle = GlobalReAlloc( *oldHPtr, (DWORD)sizeof(char *) * mem4numPointer + sizeof( HANDLE ), GMEM_MOVEABLE ) ;
            #else
               handle = GlobalReAlloc( *oldHPtr, (DWORD)sizeof(char *) * mem4numPointer + sizeof( HANDLE ), GMEM_MOVEABLE ) ;
            #endif
         #endif

         if ( handle == (HANDLE) 0 )
            return error4( 0, e4memory, E95901 ) ;

         #ifdef S4WINCE
            hPtr = (HANDLE *) LocalLock( handle ) ;
         #else
            hPtr = (HANDLE *)GlobalLock( handle ) ;
         #endif
         *hPtr++ = handle ;
         mem4testPointers = (char **)hPtr ;
      #else
         mem4testPointers = (char **)realloc( (void *)mem4testPointers, sizeof(char *)*mem4numPointer ) ;
      #endif
   }

   if ( mem4testPointers == 0 )
      return error4( 0, e4memory, E95901 ) ;

   mem4testPointers[mem4numUsed++] = ptr ;

   #ifdef E4ANALYZE
      // AS Dec 4/02 - To aid in debugging due to MSDEV bug causing MSDEV crash
      if ( ptr == g4cmp_ptr )
      {
         if ( ptr == 0 )  // never happens, just let debugger through
            return -1 ;
         return 0 ;
      }
   #endif


   return 0 ;
}



static int mem4popPointer( char *ptr )
{
   int i ;

   if ( mem4testPointers == 0 )   /* failure in allocating tracking memory, so allow shutdown */
   {
      mem4numUsed-- ;
      return 0 ;
   }

   for ( i = mem4numUsed - 1 ; i >= 0 ; i-- )
   {
      if ( mem4testPointers[i] == ptr )
      {
         /* This 'memmove' may create compile warning */
         c4memmove( mem4testPointers+i, mem4testPointers+i+1, (size_t) (sizeof(char *) * (mem4numUsed-i-1))) ;
         mem4numUsed-- ;
         // AS 06/30/99 --> also clear out the old pointer so don't have random bad pointers running around...
         mem4testPointers[mem4numUsed] = 0 ;
         return 0 ;
      }
   }

   return error4( 0, e4result, E95902 ) ;  // the allocated memory was not found...
}



int S4FUNCTION mem4checkMemory()
{
   int i ;
   char *ptr ;

   if ( code4numCodeBase() == 0 && resetInProgress == 0 )
      return 0 ;

   #ifdef S4SEMAPHORE
      mem4start( 0 ) ;
   #endif

   for ( i = 0; i < mem4numUsed; i++ )
   {
      ptr = mem4checkPointer( mem4testPointers[i], 0 ) ;
      if ( ptr == 0 )
         return error4( 0, e4result, E95903 ) ;
   }

   #ifdef S4SEMAPHORE
      mem4stop( 0 ) ;
   #endif

   return 0 ;
}



int S4FUNCTION mem4freeCheck( const int maxLeft )
{
   #ifdef S4MEM_PRINT
      if ( v4print > 0 )
      {
         int i ;
         for ( i = 0; i < mem4numUsed; i++ )
         {
            #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
               d4displayStr( mem4displayPtr, "mem4freeCheck: ", 1 ) ;
               d4displayPtr( mem4displayPtr, mem4testPointers[i], 0 ) ;
            #else
               printf( "\nmem4freeCheck: %p", mem4testPointers[i] ) ;
            #endif
         }
      }
   #endif

   if ( mem4numUsed > maxLeft )
      return error4( 0, e4result, 95904 ) ;

   return ( mem4numUsed ) ;
}
#endif



void *S4FUNCTION mem4allocDefault( MEM4 *memoryType, int doZero )
{
   #ifdef E4PARM_HIGH
      if ( memoryType == 0 )
      {
         error4( 0, e4parm_null, E95905 ) ;
         return 0 ;
      }

      return mem4allocErrDefault( memoryType, 0, doZero ) ;
   #else
      return mem4allocErrDefault( memoryType, 0, 1 ) ;  // CS 2001/02/14 always define this function
   #endif
}



static Y4CHUNK *mem4allocChunkDefault( MEM4 *typePtr )
{
   Y4CHUNK *chunkPtr ;
   int  nAllocate, i ;
   char *ptr ;
   long allocSize ;

   nAllocate = typePtr->unitExpand ;
   if ( l4last( &typePtr->chunks ) == 0 )
      nAllocate = typePtr->unitStart ;

   for ( ;; )
   {
      allocSize = (long)sizeof( LINK4 ) + (long)nAllocate * typePtr->unitSize ;
      if ( (unsigned long)allocSize < (unsigned long)UINT_MAX )
         break ;
      if ( nAllocate <= 1 )
         return 0 ;
      nAllocate = nAllocate / 2 ;
   }

   #ifdef S4DOS    /* no memory sharing under DOS, so can use a CODE4 for limited memory */
      chunkPtr = (Y4CHUNK *)u4allocFreeDefault( typePtr->codeBase, allocSize ) ;
   #else
      chunkPtr = (Y4CHUNK *)u4alloc( allocSize ) ;
   #endif
   if ( chunkPtr == 0 )
      return 0 ;
   ptr = (char *)&chunkPtr->data ;
   for ( i = 0 ; i < nAllocate ; i++ )
      l4add( &typePtr->pieces, (LINK4 *)( ptr + i * typePtr->unitSize ) ) ;

   return  chunkPtr ;
}



#ifdef S4MAX_OR_SERVER
   long mem4allocated = 0L ;
   long g_mem4numAllocated = 0L ;  // AS Oct 2/02 - also track total outstanding allocations

   long u4allocated( void )
   {
      return mem4allocated ;
   }
   #ifdef S4SERVER
      // AS Mar 15/10 - added ability to return total # of allocations for error reporting
      long u4numAllocated( void )
      {
         return g_mem4numAllocated ;
      }
   #endif
#endif

static void *mem4allocLow( MEM4 *memoryType )
{
   LINK4 *nextPiece ;
   Y4CHUNK *newChunk ;
   #ifdef E4MISC
      char *ptr ;
   #endif
   #ifdef S4OS2_SEMAPHORE
      #ifdef __DLL__
         ULONG flags ;
      #endif
   #endif

   if ( memoryType == 0 )
      return 0 ;
   nextPiece = (LINK4 *)l4pop( &memoryType->pieces ) ;

   if ( nextPiece != 0 )
   {
      #ifdef S4SEMAPHORE
         #ifdef S4OS2
            #ifdef __DLL__
               /* get access to the memory */
               flags = PAG_WRITE | PAG_READ  ;
               if ( DosGetSharedMem( nextPiece, flags ) != 0 )
                  return 0 ;
            #endif
         #endif
      #endif
      memoryType->nUsed++ ;
      #ifdef E4MISC
         ptr = mem4fixPointer( (char *)nextPiece, memoryType->unitSize ) ;

         if ( mem4pushPointer( ptr ) == e4memory )
         {
            memoryType->nUsed-- ;
            l4add( &memoryType->pieces, nextPiece ) ;
            return 0 ;
         }

         #ifdef S4MEM_PRINT
            if ( v4print > 0 )
            {
               #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
                  {
                     d4displayStr( mem4displayPtr, "  Y4ALLOC:  ", 1 ) ;
                     d4displayPtr( mem4displayPtr, ptr, 0 ) ;
                     d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
                     d4displayStr( mem4displayPtr, m4fileName, 0 ) ;
                     d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
                     d4displayNum( mem4displayPtr, m4lineNo, 0 ) ;
                  }
               #else
                  printf( "\nY4ALLOC:  0x%08lX  file:  %-30s  line:  %6d", ptr, m4fileName, m4lineNo  ) ;
               #endif
            }
         #endif

         return (void *)ptr ;
      #else
         return nextPiece ;
      #endif
   }

   if ( (newChunk = mem4allocChunkDefault( memoryType )) == 0 )
      return 0 ;
   l4add( &memoryType->chunks, &newChunk->link ) ;

   memoryType->nUsed++ ;
   #ifdef E4MISC
      ptr = mem4fixPointer( (char *)l4pop(&memoryType->pieces), memoryType->unitSize ) ;
      #ifdef S4MEM_PRINT
         if ( v4print > 0 )
         {
            #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
               {
                  d4displayStr( mem4displayPtr, "  Y4ALLOC:  ", 1 ) ;
                  d4displayPtr( mem4displayPtr, ptr, 0 ) ;
                  d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
                  d4displayStr( mem4displayPtr, m4fileName, 0 ) ;
                  d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
                  d4displayNum( mem4displayPtr, m4lineNo, 0 ) ;
               }
            #else
               printf( "\nY4ALLOC:  0x%08lX  file:  %-30s  line:  %5d", ptr, m4fileName, m4lineNo  ) ;
            #endif
         }
      #endif

      mem4pushPointer( ptr ) ;
      return (void *)ptr ;
   #else
      return l4pop( &memoryType->pieces ) ;
   #endif
}



void *S4FUNCTION mem4allocErrDefault( MEM4 *memoryType, CODE4 *c4, int doZero )
{
   void *ptr ;

   if ( code4numCodeBase() == 0 && resetInProgress == 0 )
      return 0 ;

   if ( c4 )
      if ( error4code( c4 ) < 0 )
         return 0 ;

   #ifdef S4SEMAPHORE
      mem4start( c4 ) ;
   #endif

   ptr = mem4allocLow( memoryType ) ;

   #ifdef S4SEMAPHORE
      // LY Sep 10/04 : changed from S4LINUX to S4UNIX
      #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : added S4LINUX */
         mem4stop( c4 ) ;
      #else
         mem4stop( memoryType->codeBase ) ;
      #endif
   #endif

   if ( ptr == 0 )
   {
      if ( c4 )  // should be creating an error in this case as well...
      {
         // error4set( c4, e4memory ) ;
         #ifdef S4SERVER
            // in this case provide more information since we have it
            char errOutBuf[60] ;
            sprintf( errOutBuf, "Memory Status: # allocations: %ld, total allocated %ld", g_mem4numAllocated, mem4allocated ) ;
            error4describe( c4, e4memory, E95905, errOutBuf, 0, 0 ) ;
         #else
            error4( c4, e4memory, E95905 ) ;
         #endif
      }
      return 0 ;
   }
   if ( doZero )
   {
      #ifdef E4MISC
         c4memset( ptr, 0, memoryType->unitSize - mem4extraTot ) ;
      #else
         c4memset( ptr, 0, memoryType->unitSize ) ;
      #endif
   }
   return ptr ;
}



MEM4 *S4FUNCTION mem4createDefault( CODE4 *c4, int start, const unsigned int uSize, int expand, const int isTemp )
{
   MEM4 *onType ;
   unsigned int unitSize ;
   #ifdef S4SEMAPHORE
      #ifdef S4OS2
         #ifdef __DLL__
            ULONG flags ;
         #endif
      #endif
   #endif

   #ifdef E4PARM_HIGH
      /* c4 == 0 is allowable */
      if (  start < 1 || expand < 1 )
      {
         error4( c4, e4parm, E95906 ) ;
         return 0 ;
      }
   #endif

   if ( code4numCodeBase() == 0 && resetInProgress == 0 )
      return 0 ;

   if ( uSize < sizeof( LINK4 ) )   /* ensure enough so that we can keep track of the memory */
      unitSize = sizeof( LINK4 ) ;
   else
      unitSize = uSize ;

   #ifdef E4MISC
      unitSize += mem4extraTot ;
   #endif

   if ( c4 )
      if ( error4code( c4 ) < 0 )
         return 0 ;

   #ifdef S4SEMAPHORE
      mem4start( c4 ) ;
   #endif

   if ( !isTemp )
      for( onType = 0 ;; )
      {
         onType = (MEM4 *)l4next( &used, onType ) ;
         if ( onType == 0 )
            break ;

         #ifdef S4SEMAPHORE
            #ifdef S4OS2
               #ifdef __DLL__
                  /* get access to the memory */
                  flags = PAG_WRITE | PAG_READ  ;
                  if ( DosGetSharedMem( onType, flags ) != 0 )
                     return 0 ;
               #endif
            #endif
         #endif

         #ifdef S4DOS    /* no memory sharing under DOS, so can use a CODE4 for limited memory */
            if ( onType->codeBase == c4 || onType->codeBase == 0 )
         #endif
         if ( onType->unitSize == unitSize && onType->nRepeat > 0 )
         {
            /* Match */
            if ( start > onType->unitStart )
               onType->unitStart = start ;
            if ( expand > onType->unitExpand)
               onType->unitExpand = expand ;
            onType->nRepeat++ ;
            #ifdef S4SEMAPHORE
               mem4stop( c4 ) ;
            #endif
            #ifdef S4DOS
               if ( onType->codeBase == 0 )  /* set to avoid code4initUndo() errors */
                  onType->codeBase = c4 ;
            #endif
            return onType ;
         }
      }

   /* Allocate memory for another MEM4 */

   onType = (MEM4 *)l4last( &avail ) ;
   if ( onType == 0 )
   {
      MEMORY4GROUP *group ;
      int i ;

      #ifdef S4MEM_PRINT
         char *nullPtr = 0 ;

         if ( v4print > 0 )
         {
            #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
               {
                  d4displayStr( mem4displayPtr, "  MEM4:  ", 1 ) ;
                  d4displayPtr( mem4displayPtr, nullPtr, 0 ) ;
                  d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
                  d4displayStr( mem4displayPtr, "ignoreNextLine", 0 ) ;
                  d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
                  d4displayNum( mem4displayPtr, 0, 0 ) ;
                  d4displayStr( mem4displayPtr, "  num bytes:  ", 0 ) ;
                  d4displayNum( mem4displayPtr, 0, 0 ) ;
               }
            #else
               printf("\nMEM4:     0x%08lX  file:  %-30s  line:  %5d  num bytes:  %12d", nullPtr, "ignoreNextLine", 0, 0 ) ;
            #endif
         }
      #endif

      group = (MEMORY4GROUP *)u4allocFreeDefault( c4, (long)sizeof( MEMORY4GROUP ) ) ;
      if ( group == 0 )
      {
         if ( c4 )
            error4set( c4, e4memory ) ;
         #ifdef S4SEMAPHORE
            mem4stop( c4 ) ;
         #endif
         return 0 ;
      }

      for ( i = 0 ; i < mem4numTypes ; i++ )
         l4add( &avail, group->types + i ) ;
      onType = (MEM4 *)l4last( &avail ) ;
      l4add( &groups, group ) ;
   }

   l4remove( &avail, onType ) ;
   c4memset( (void *)onType, 0, sizeof( MEM4 ) ) ;
   l4add( &used, onType ) ;

   onType->unitStart = start ;
   onType->unitSize = unitSize ;
   onType->unitExpand= expand ;
   onType->nRepeat = 1 ;
   onType->nUsed = 0 ;
   if ( isTemp )
      onType->nRepeat = -1 ;

   #ifdef S4SEMAPHORE
      mem4stop( c4 ) ;
   #endif

   #ifdef S4MEM_PRINT
      if ( v4print > 0 )
      {
         #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
            {
               d4displayStr( mem4displayPtr, "  MEM4:     ", 1 ) ;
               d4displayPtr( mem4displayPtr, onType, 0 ) ;
               d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
               d4displayStr( mem4displayPtr, m4fileName, 0 ) ;
               d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
               d4displayNum( mem4displayPtr, m4lineNo, 0 ) ;
            }
         #else
            printf( "\nMEM4:     0x%08lX  file:  %-30s  line:  %5d", onType, m4fileName, m4lineNo ) ;
         #endif
      }
   #endif

   #ifdef S4DOS    /* no memory sharing under DOS, so can use a CODE4 for limited memory */
      onType->codeBase = c4 ;
   #endif
   return onType ;
}



void *S4FUNCTION mem4createAllocDefault( CODE4 *c4, MEM4 **typePtrPtr, int start, const unsigned int unitSize, int expand, const int isTemp, int doZero )
{
   if ( *typePtrPtr == 0 )
   {
      *typePtrPtr = mem4createDefault( c4, start, unitSize, expand, isTemp ) ;
      if ( *typePtrPtr == 0 )
         return 0 ;
   }

   return mem4allocErrDefault( *typePtrPtr, c4, doZero ) ;
}



int S4FUNCTION mem4freeDefault( MEM4 *memoryType, void *freePtr )
{
   #ifdef E4MISC
      int rc ;
      LINK4 *holdPtr ;
   #endif

   if ( freePtr == 0 )  /* as documented */
      return 0 ;

   if ( memoryType == 0 )
      return error4( 0, e4parm_null, E95907 ) ;

   if ( code4numCodeBase() == 0 && resetInProgress == 0 )
      return 0 ;

   #ifdef S4SEMAPHORE
      mem4start( 0 ) ;
   #endif

   memoryType->nUsed-- ;
   #ifdef E4MISC
      if ( memoryType->nUsed < 0 )
      {
         #ifdef S4SEMAPHORE
            mem4stop( 0 ) ;
         #endif
         return error4( 0, e4result, E95907 ) ;
      }

      rc = mem4popPointer( (char *)freePtr ) ;
      holdPtr = (LINK4 *)mem4checkPointer( (char *)freePtr, 0 ) ;
      // AS July 24/02 - move the 0 reset earlier to help with debugging link problems.
      c4memset( ((char *)freePtr), 0, memoryType->unitSize - 2 * mem4extraChars - sizeof( unsigned ) ) ;
      if ( holdPtr != 0 )
      {
         l4add( &memoryType->pieces, holdPtr ) ;
         #ifdef S4MEM_PRINT
            if ( v4print > 0 )
            {
               #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
                  {
                     d4displayStr( mem4displayPtr, "  Y4FREE:   ", 1 ) ;
                     d4displayPtr( mem4displayPtr, freePtr, 0 ) ;
                     d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
                     d4displayStr( mem4displayPtr, m4fileName, 0 ) ;
                     d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
                     d4displayNum( mem4displayPtr, m4lineNo, 0 ) ;
                  }
               #else
                  printf( "\nY4FREE:   0x%08lX  file:  %-30s  line:  %5d", freePtr, m4fileName, m4lineNo ) ;
               #endif
            }
         #endif
      }

      #ifdef S4SEMAPHORE
         mem4stop( 0 ) ;
      #endif
      return rc ;
   #else
      l4add( &memoryType->pieces, (LINK4 *)freePtr ) ;
      #ifdef S4SEMAPHORE
         mem4stop( 0 ) ;
      #endif
      return 0 ;
   #endif
}



void S4FUNCTION mem4release( MEM4 *memoryType )
{
   void *ptr ;

   if ( memoryType == 0 )
      return ;

   if ( code4numCodeBase() == 0 && resetInProgress == 0 )
      return ;

   #ifdef S4SEMAPHORE
      mem4start( 0 ) ;
   #endif

   memoryType->nRepeat-- ;
   if ( memoryType->nRepeat <= 0 )
   {
      // AS 01/04/00 - if here, then nUsed should be 0, otherwise the caller has failed
      // to mem4free all the data...
      // AS Apr 3/09 - for server this should be with E4MISC defined, since it is tracked that way, not E4ANALYZE
      #ifdef E4ANALYZE
         if ( memoryType->nUsed > 0 )  // were memory leaks
            error4( 0, e4memory, E95911 ) ;
      #else
         #ifdef E4MISC
            #ifdef S4SERVER
               if ( memoryType->nUsed > 0 )  // were memory leaks
                  error4( 0, e4memory, E95911 ) ;
            #endif
         #endif
      #endif
      for(;;)
      {
         ptr = l4pop( &memoryType->chunks) ;
         if ( ptr == 0 )
            break ;
         u4freeDefault( ptr ) ;
      }

      l4remove( &used, memoryType ) ;
      l4add( &avail, memoryType ) ;

      #ifdef S4MEM_PRINT
         if ( v4print > 0 )
         {
            #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
               {
                  d4displayStr( mem4displayPtr, "  Y4RELEASE:  ", 1 ) ;
                  d4displayPtr( mem4displayPtr, memoryType, 0 ) ;
               }
            #else
               printf( "\nY4RELEASE: 0x%08lX", memoryType ) ;
            #endif
         }
      #endif
   }

   #ifdef S4SEMAPHORE
      mem4stop( 0 ) ;
   #endif
}



#ifdef S4TESTING
   static int s4allocOff = 0 ;   // LY July 9/03 : added static
#endif



#ifdef S4MAX
   static long mem4maxMemory = 16384 ;

   long u4max( void )
   {
      return mem4maxMemory ;
   }
#endif



void *S4FUNCTION u4allocDefault( long n )
{
   size_t s ;
   char *ptr ;
   #if defined( S4SEMAPHORE ) && defined( S4OS2 ) && defined( __DLL__)
      ULONG    flags;
      APIRET   rc;
   #endif
   #ifdef S4WIN_ALLOC
      HANDLE  handle, *hPtr ;
   #endif

   #ifdef S4SEMAPHORE   /* LY 99/6/8 : S4UNIX */
      // LY Sep 10/04 : changed from S4LINUX to S4UNIX
      #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : added S4LINUX */
         assert5( memoryInitialized >= 1 ) ;  // cannot use if not initialized...
      #endif
   #endif

   #ifdef S4TESTING
      if ( s4allocOff == 1 )   /* for testing, emulate out of memory conditions */
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( n <= 0L )
      {
         error4( 0, e4parm, E85903 ) ;
         return 0 ;
      }
   #endif

   // AS May 22/02 - Ensure the actual allocated amount is reasonable
   #ifdef E4DEBUG
      if ( n > 100000000 )  // allocation over 100 Megs is very unusual
      {
         error4( 0, e4parm, E85903 ) ;
         return 0 ;
      }
   #endif

   #ifdef E4MISC
      n += mem4extraChars * 2 + sizeof( unsigned ) ;
   #endif

   #ifdef S4MAX_OR_SERVER
      n += sizeof( S4LONG ) ;   /* room for length */
   #endif
   #ifdef S4MAX
      if ( mem4allocated + n > mem4maxMemory )
         return 0 ;
   #endif

   s = (size_t) n ;
   if ( n > (long) s )
   {
      // AS Sept 13/02 - add some additional error handling to catch this call earlier
      char outBuf[120] ;
      sprintf( outBuf, "exceeded the size allocation capacity.  Attempted to allocate %ld.  attempt to convert to size_t for allocation produced: %ld", n, (long)s ) ;
      error4describe( 0, e4memory, E85901, outBuf, 0, 0 ) ;
      return 0 ;
   }

   #if defined( S4WIN_ALLOC )
      #ifdef S4WINCE
         handle = LocalAlloc( LMEM_FIXED|LMEM_ZEROINIT, (UINT)s+sizeof(HANDLE) ) ;  /* modified 4/22/98 L.Y. */
      #else
         #ifdef __DLL__
            handle = GlobalAlloc( GMEM_MOVEABLE | GMEM_DDESHARE | GMEM_ZEROINIT, (DWORD) s+ sizeof(HANDLE) ) ;
         #else
            handle = GlobalAlloc( GMEM_MOVEABLE | GMEM_ZEROINIT, (DWORD) s+ sizeof(HANDLE) ) ;
         #endif
      #endif

      if ( handle == (HANDLE)0 )
      {
         #if defined( S4WIN32 ) && defined( E4ANALYZE )
            /* for debugger purposes */
            DWORD err = GetLastError() ;
         #endif
         #ifdef E4MISC
            mem4checkMemory() ;   // AS Oct 2/02 maybe memory has been corrupted, thus causing this problem.
         #endif
         // AS Mar 23/04 - The whole point of this function is to NOT given an error if cannot allocate!
         // AS Sept 13/02 - add some additional error handling to catch this call earlier
         // error4( 0, e4memory, E85901 ) ;
         return 0 ;
      }

      #ifndef S4WINCE
         hPtr = (HANDLE *)GlobalLock( handle ) ;
      #else
         hPtr = (HANDLE *)LocalLock( handle) ;
      #endif
      *hPtr++ = handle ;
      ptr = (char *)hPtr ;
   #elif defined( __DLL__ )
      #ifdef S4OS2_SEMAPHORE
         flags = PAG_WRITE | PAG_READ | OBJ_GETTABLE ;
         rc = DosAllocSharedMem( (void *)&ptr, 0, s, flags ) ;

         if (rc != 0)
            return 0 ;
         flags = PAG_WRITE | PAG_READ ;
         rc = DosGetSharedMem( ptr, flags ) ;
         if ( rc != 0 )
            return 0 ;
      #else
         ptr = (char *)malloc( s ) ;

         if ( ptr == 0 )
         {
            // AS Sept 13/02 - add some additional error handling to catch this call earlier
            // AS May 6/08 this is not really an error since the caller may perform another action if the memory allocation fails...see similar notes below
            // error4( 0, e4memory, E85901 ) ;
            return 0 ;
         }

         #ifndef S4PASCAL_WIN
            c4memset( ptr, 0, s ) ;
         #endif
      #endif
   #else
      #ifdef S4PALM
         ptr = (char *)MemPtrNew( s ) ;
      #else
         ptr = (char *)malloc( s ) ;
      #endif

      if ( ptr == 0 )
      {
         // AS Sept 13/02 - add some additional error handling to catch this call earlier
         // AS Mar 23/04 - The whole point of this function is to NOT given an error if cannot allocate!
         // error4( 0, e4memory, E85901 ) ;
         return 0 ;
      }

                           /* Borland malloc of 64K (or close to) will */
      #ifdef __TURBOC__    /* result in corruption of segment memory due */
         if ( (ptr+s-1 <= ptr) && (s > 1 ))    /* to wrap-around problems  */
         {
            free( ptr ) ;
            return 0 ;
         }
      #endif

      #ifndef S4PASCAL_WIN
         c4memset( ptr, 0, s ) ;
      #endif
   #endif

   #ifdef S4MEM_PRINT
      if ( v4print != -1 )
      {
         #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
            {
               // AS 08/16/99 the info may get lost via d4displayStr, so save now...
               char fileNameBuf[LEN4PATH] ;
               long lineNo = m4lineNo ;
               strcpy( fileNameBuf, m4fileName ) ;
               d4displayStr( mem4displayPtr, "  U4ALLOC:  ", 1 ) ;
               d4displayPtr( mem4displayPtr, ptr, 0 ) ;
               d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
               d4displayStr( mem4displayPtr, fileNameBuf, 0 ) ;
               d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
               d4displayNum( mem4displayPtr, lineNo, 0 ) ;
               d4displayStr( mem4displayPtr, "  num bytes:  ", 0 ) ;
               d4displayNum( mem4displayPtr, n, 0 ) ;
            }
         #else
            printf("\nU4ALLOC:  0x%08lX  file:  %-30s  line:  %5d  num bytes:  %6ld", ptr, m4fileName, m4lineNo, n ) ;
         #endif
      }
   #endif

   #ifdef E4MISC
      #ifdef S4SEMAPHORE
         mem4start( 0 ) ;
      #endif
      ptr = mem4fixPointer( ptr, s ) ;
      mem4pushPointer( ptr ) ;
      c4memset( ptr, 0, s-mem4extraChars*2 - sizeof(unsigned) ) ;
      #ifdef S4SEMAPHORE
         mem4stop( 0 ) ;
      #endif
   #endif

   #ifdef S4MAX_OR_SERVER
      #ifdef S4SEMAPHORE
         mem4start( 0 ) ;
      #endif
      mem4allocated += n ;
      g_mem4numAllocated++ ;
      #ifdef S4SEMAPHORE
         mem4stop( 0 ) ;
      #endif
      *((long *)ptr) = n ;
      ptr += sizeof(S4LONG ) ;
   #endif

   return (void *)ptr ;
}



void *S4FUNCTION u4allocErDefault( CODE4 *c4, long n )
{
   void *ptr = (void *)u4allocFreeDefault( c4, n ) ;
   if ( ptr == 0 && c4 )
   {
      #ifdef S4SERVER
         // in this case provide more information since we have it
         char errOutBuf[60] ;
         sprintf( errOutBuf, "Memory Status: # allocations: %ld, total allocated %ld", g_mem4numAllocated, mem4allocated ) ;
         error4describe( c4, e4memory, E95909, errOutBuf, 0, 0 ) ;
      #else
         error4( c4, e4memory, E95909 ) ;
      #endif
   }

   return ptr ;
}



int S4FUNCTION u4freeDefault( void *ptr )
{
   int rc ;
   #ifdef S4WIN_ALLOC
      HANDLE  hand ;
   #endif

   #ifdef S4MAX_OR_SERVER
      long amount ;
   #endif

   #ifdef S4SEMAPHORE   /* LY 99/6/8 : S4UNIX */
      // LY Sep 10/04 : changed from S4LINUX to S4UNIX
      #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : added S4LINUX */
         assert5( memoryInitialized >= 1 ) ;  // cannot use if not initialized...
      #endif
   #endif

   rc = 0 ;

   /* if we need to access the semaphores, better ensure that the memory
      area is initialized; otherwise allow general freeing.  Change for
      CodeControls, means occasional memory leaks, but not common.
      AS 02/10/96.
   */
   #ifdef S4MAX
      if ( code4numCodeBase() == 0 && resetInProgress == 0 )
         return 0 ;
   #else
      #ifdef E4MISC
         if ( code4numCodeBase() == 0 && resetInProgress == 0 )
            return 0 ;
      #endif
   #endif

   if ( ptr == 0 )
      return 0 ;

   #ifdef S4MAX_OR_SERVER
      ptr = ((char *)ptr) - sizeof(S4LONG) ;
      amount = *((long *)ptr) ;
      #ifdef S4SEMAPHORE
         mem4start( 0 ) ;
      #endif
      mem4allocated -= amount ;
      g_mem4numAllocated-- ;
      #ifdef S4SEMAPHORE
         mem4stop( 0 ) ;
      #endif
   #endif

   #ifdef S4WIN_ALLOC
      #ifdef E4MISC
         #ifdef S4SEMAPHORE
            mem4start( 0 ) ;
         #endif
         rc = mem4popPointer( (char *)ptr ) ;
         ptr = mem4checkPointer( (char *)ptr, 1 ) ;
         #ifdef S4SEMAPHORE
            mem4stop( 0 ) ;
         #endif
         if ( (char *)ptr == 0 )  /* error during mem check */
            return -1 ;
         hand = ((HANDLE *)(char *)ptr)[-1] ;
      #else
         hand = ((HANDLE *)ptr)[-1] ;
      #endif

      #ifdef S4MEM_PRINT
         if ( v4print != -1 )
         {
            #if defined( S4WINDOWS ) && defined( S4TESTING )
               #ifdef S4SEMAPHORE
                  mem4start( 0 ) ;
               #endif
               d4displayStr( mem4displayPtr, "  U4FREE:   ", 1 ) ;
               d4displayPtr( mem4displayPtr, ptr, 0 ) ;
               d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
               d4displayStr( mem4displayPtr, m4fileName, 0 ) ;
               d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
               d4displayNum( mem4displayPtr, m4lineNo, 0 ) ;
               #ifdef S4SEMAPHORE
                  mem4stop( 0 ) ;
               #endif
            #else
               printf( "\nU4FREE:   0x%08lX  file:  %-30s  line:  %5d", ptr, m4fileName, m4lineNo  );
            #endif
         }
      #endif
      #ifndef S4WINCE
         GlobalUnlock( hand ) ;
         hand = GlobalFree( hand ) ;
      #else
         LocalUnlock( hand ) ;
         hand = LocalFree( hand ) ;
      #endif
      if ( hand != (HANDLE) 0 )
      {
         // AS Aug 24/04 - more error info in debug case...for looking in debugger...
         #ifdef E4ANALYZE
            DWORD err = GetLastError() ;
         #endif
         return error4( 0, e4memory, E95910 ) ;
      }
   #else
      #ifdef E4MISC
         #ifdef S4SEMAPHORE
            mem4start( 0 ) ;
         #endif
         rc = mem4popPointer( (char *)ptr ) ;
         ptr = mem4checkPointer( (char *)ptr, 1 ) ;
         #ifdef S4SEMAPHORE
            mem4stop( 0 ) ;
         #endif

         #ifdef S4MEM_PRINT
            if ( v4print != -1 )
            {
               #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
                  {
                     d4displayStr( mem4displayPtr, "  U4FREE:  ", 1 ) ;
                     d4displayPtr( mem4displayPtr, ptr, 0 ) ;
                     d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
                     d4displayStr( mem4displayPtr, m4fileName, 0 ) ;
                     d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
                     d4displayNum( mem4displayPtr, m4lineNo, 0 ) ;
                  }
               #else
                     printf( "\nU4FREE:   0x%08lX  file:  %-30s  line:  %5d", ptr, m4fileName, m4lineNo  );
               #endif
            }
         #endif

         free( ptr ) ;
      #else
         #ifdef S4MEM_PRINT
            if ( v4print != -1 )
            {
               #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
                  {
                     d4displayStr( mem4displayPtr, "  U4FREE:  ", 1 ) ;
                     d4displayPtr( mem4displayPtr, ptr, 0 ) ;
                     d4displayStr( mem4displayPtr, "  file:  ", 0 ) ;
                     d4displayStr( mem4displayPtr, m4fileName, 0 ) ;
                     d4displayStr( mem4displayPtr, "  line:  ", 0 ) ;
                     d4displayNum( mem4displayPtr, m4lineNo, 0 ) ;
                  }
               #else
                  printf( "\nU4FREE:   0x%08lX  file:  %-30s  line:  %5d", ptr, m4fileName, m4lineNo  );
               #endif
            }
         #endif

         #ifdef S4PALM
            MemPtrFree( ptr ) ;
         #else
            free( (char *)ptr ) ;
         #endif
      #endif
   #endif

   return rc ;
}


int S4FUNCTION mem4reset( void )
{
   MEM4 *onType ;
   LINK4 *onChunk, *onGroup ;
   #ifdef S4WIN_ALLOC
      #ifdef E4MISC
         HANDLE hand ;
      #endif
   #endif
   // Jan 27/11 - Added support for lock checking on a CODE4 level to help with multi-thread, updated code to release the memory after
   // #ifdef S4LOCK_CHECK
   //    return error4( 0, e4result, E85904 ) ;
   // #endif

   #ifdef S4SEMAPHORE   /* LY 99/6/8 : S4UNIX */
      // LY Sep 10/04 : changed from S4LINUX to S4UNIX
      #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : added S4LINUX */
         if ( memoryInitialized > 1 )
         {
            memoryInitialized-- ;
            return 0 ;
         }

         if ( memoryInitialized == 0 )
            return 0 ;
      #endif
   #endif

   resetInProgress = 1 ;   /* suspend the disallowment of u4free when no code4's */

   for( onType = 0 ;; )
   {
      onType = (MEM4 *)l4next(&used,onType) ;
      if ( onType == 0 )
         break ;
      do
      {
         onChunk = (LINK4 *)l4pop( &onType->chunks) ;
         u4freeDefault( onChunk ) ;  /* free of 0 still succeeds */
      } while ( onChunk ) ;
   }

   for( ;; )
   {
      onGroup = (LINK4 *)l4pop( &groups ) ;
      if ( onGroup == 0 )
         break ;
      u4freeDefault( onGroup ) ;
   }

   #ifdef E4MISC
      Bool5 memLeft = 0 ;
      if ( mem4numPointer > 0 )
      {
         #ifdef S4MEM_PRINT
            if ( v4print > 0 )
            {
               #if defined( S4WIN_ALLOC ) && defined( S4WINDOWS ) && defined( S4TESTING )
                  {
                     d4displayStr( mem4displayPtr, "  MEM4RESET:  ", 1 ) ;
                     d4displayPtr( mem4displayPtr, mem4testPointers, 0 ) ;
               #else
                  printf( "\nMEM4RESET: %lx", mem4testPointers );
               #endif
            }
            // AS Jul 8/04 - Also print out any unfreed pointers
            if ( mem4numUsed != 0 )
            {
               long numUsed = mem4numUsed ;
               for ( ;; )
               {
                  printf( "\nUNFREED POINTER: 0x%08lX", mem4testPointers[numUsed-1]-12 );  // subtract 12 becuase of storing extra info
                  numUsed-- ;
                  if ( numUsed == 0 )
                     break ;
               }
            }
         #endif

         #ifdef S4WIN_ALLOC
            hand = ((HANDLE *)mem4testPointers)[-1] ;

            #ifndef S4WINCE
               GlobalUnlock( hand ) ;
               hand = GlobalFree( hand ) ;
            #else
               LocalUnlock( hand ) ;
               hand = LocalFree( hand ) ;
            #endif

            if ( hand != (HANDLE)0 )
               return error4( 0, e4memory, E95911 ) ;
         #else
            free( (void *)mem4testPointers ) ;
         #endif

         mem4testPointers = 0 ;
         mem4numPointer = -1 ;

         // AS Jan 23/03 - moved to after to indicate was uninitialized (problems with testing otherwise)
         #ifdef E4ANALYZE
            if ( mem4numUsed > 0 )  // were memory leaks
               memLeft = mem4numUsed ;
         #endif

         mem4numUsed = 0 ;
      }
   #endif

   #ifdef S4SEMAPHORE   /* LY 99/6/8 : S4UNIX, removed extra #ifdef's */
      // LY Sep 10/04 : changed from S4LINUX to S4UNIX
      #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : added S4LINUX */
         assert5( memoryInitialized == 1 ) ;
         memoryInitialized = 0 ;  // force mem4init to reset everything... - should decrement down here anyway...
         mem4init() ;

         #ifdef S4WIN32 /* LY 2002/02/20 */
            DeleteCriticalSection( &critical4memory ) ;
            DeleteCriticalSection( &critical4expression ) ;
            #ifndef S4STAND_ALONE
               DeleteCriticalSection( &critical4compression ) ;
            #endif
         #else
            pthread_mutex_destroy( &critical4memory ) ;
            pthread_mutex_destroy( &critical4expression ) ;
         #endif

         memoryInitialized = 0 ;
      #endif
   #endif

   resetInProgress = 0 ;   /* suspend the disallowment of u4free when no code4's */

   #ifdef E4MISC
      #ifdef E4ANALYZE
         if ( memLeft > 0 )  // were memory leaks
            error4( 0, e4memory, E95911 ) ;
      #endif
   #endif

   return 0 ;
}



void S4FUNCTION mem4init( void )
{
   // AS 06/07/99 allowed multiple handlers onto init/reset for outside of CodeBase
   // capabilities...

   #ifdef S4SEMAPHORE   /* LY 99/6/8 : S4UNIX, remove extra #ifdef's */
      // LY Sep 10/04 : changed from S4LINUX to S4UNIX
      #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : added S4LINUX */
         if ( memoryInitialized )
         {
            memoryInitialized++ ;
            return ;
         }

         memoryInitialized = 1 ;
      #endif
   #endif

   #ifdef S4SEMAPHORE
      // LY Sep 10/04 : changed from S4LINUX to S4UNIX
      #if defined( S4WIN32 ) || defined( S4UNIX ) /* LY 2002/02/20 : addes S4LINUX */
         if ( resetInProgress == 0 )
         {
            #ifdef S4WIN32 /* LY 2002/02/20 */
               InitializeCriticalSection( &critical4memory ) ;
               InitializeCriticalSection( &critical4expression ) ;
               #ifndef S4STAND_ALONE
                  InitializeCriticalSection( &critical4compression ) ;
               #endif
            #else
               // LY Sep 10/04 : added pthread_mutexattr*** code for non-Linux implementations
               pthread_mutexattr_t attr ;
               pthread_mutexattr_init( &attr ) ;
               pthread_mutexattr_settype( &attr, PTHREAD_MUTEX_RECURSIVE ) ;
               // LY Sep 10/04 : replace 0 with &attr
               pthread_mutex_init( &critical4memory, &attr ) ;
               /* LY July 11/03 : t4calc.c locking itself out under E4DEBUG; initialize in declaration above
               pthread_mutex_init( &critical4expression, 0 ) ; */
               pthread_mutex_init( &critical4expression, &attr ) ;
               pthread_mutexattr_destroy( &attr ) ;
               pidLocking = 0 ;
               numLocks = 0 ;
            #endif
         }
      #endif
      mem4start( 0 ) ;
   #endif
   c4memset( (void *)&avail, 0, sizeof( avail ) ) ;
   c4memset( (void *)&used, 0, sizeof( used ) ) ;
   c4memset( (void *)&groups, 0, sizeof( groups ) ) ;
   #ifdef S4SEMAPHORE
      mem4stop( 0 ) ;
   #endif
}

#ifdef S4MAX_OR_SERVER
   #undef S4MAX_OR_SERVER
#endif

#ifdef S4WIN_ALLOC
   #undef S4WIN_ALLOC
#endif




#ifdef S4MEM_PRINT
   // AS 11/10/00 - need to have critical sections around code so that the mem4 file/line globals don't get
   // trounced by other threads calling these functions

   void *S4FUNCTION mem4printAllocZero( MEM4 *memoryType, const char *file, int line )
   {
      mem4start( 0 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val = mem4allocDefault( memoryType, 1 ) ;
      mem4stop( 0 ) ;
      return val ;
   }



   void *mem4printAllocErrZero( MEM4 *memoryType, CODE4 *c4, const char *file, int line )
   {
      mem4start( c4 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val = mem4allocErrDefault( memoryType, c4, 1 ) ;
      mem4stop( c4 ) ;
      return val ;
   }



   void *mem4printAllocNoZero( MEM4 *memoryType, const char *file, int line )
   {
      mem4start( 0 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val = mem4allocDefault( memoryType, 0 ) ;
      mem4stop( 0 ) ;
      return val ;
   }


   void * mem4printAllocErrNoZero( MEM4 *memoryType, CODE4 *c4, const char *file, int line )
   {
      mem4start( c4 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val = mem4allocErrDefault( memoryType, c4, 0 ) ;
      mem4stop( c4 ) ;
      return val ;
   }



   Y4CHUNK *mem4printAllocChunk( MEM4 *typePtr, const char *file, int line )
   {
      mem4start( 0 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      Y4CHUNK *val = mem4allocChunkDefault( typePtr ) ;
      mem4stop( 0 ) ;
      return val ;
   }



   int S4FUNCTION mem4printFree( MEM4 *memoryType, void *freePtr, const char *file, int line )
   {
      mem4start( 0 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      int val = mem4freeDefault( memoryType, freePtr ) ;
      mem4stop( 0 ) ;
      return val ;
   }



   void *S4FUNCTION u4printAlloc( long len, const char *file, int line )
   {
      mem4start( 0 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val = u4allocDefault( len ) ;
      mem4stop( 0 ) ;
      return val ;
   }



   void *S4FUNCTION u4printAllocEr( CODE4 *a, long b, const char *file, int line )
   {
      mem4start( a ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val =  u4allocErDefault( (a), (b) ) ;
      mem4stop( a ) ;
      return val ;
   }



   void *S4FUNCTION u4printAllocFree( CODE4 *a, long b, const char *file, int line )
   {
      mem4start( a ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val = u4allocFreeDefault( (a), (b) ) ;
      mem4stop( a ) ;
      return val ;
   }



   int S4FUNCTION u4printAllocAgain( CODE4 *a, char **b, unsigned int *c, const unsigned int d, const char *file, int line )
   {
      mem4start( a ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      int val = u4allocAgainDefault( (a), (b), (c), (d) ) ;
      mem4stop( a ) ;
      return val ;
   }



   int S4FUNCTION u4printFree( void *a, const char *file, int line )
   {
      mem4start( 0 ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      int val = u4freeDefault( a ) ;
      mem4stop( 0 ) ;
      return val ;
   }



   MEM4 *S4FUNCTION mem4printCreate( CODE4 *a, int b, const unsigned int c, int d, const int e, const char *file, int line )
   {
      mem4start( a ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      MEM4 *val = mem4createDefault( a, b, c, d, e ) ;
      mem4stop( a ) ;
      return val ;
   }



   void *S4FUNCTION mem4printCreateAlloc( CODE4 *a, MEM4 **b, int c, const unsigned int d, int e, const int f, int doZero, const char *file, int line )
   {
      mem4start( a ) ;
      code4memFileNameSet( file ) ;
      code4memLineNoSet( line ) ;
      void *val = mem4createAllocDefault( a, b, c, d, e, f, doZero ) ;
      mem4stop( a ) ;
      return val ;
   }
#endif
