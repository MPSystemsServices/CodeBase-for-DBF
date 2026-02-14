/* f4flag.c  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

/* note:  This module is excluded from the CodeBase error handling
   This is because it may be convenient to call these functions regardless
   of the error state of CodeBase
*/

/* CUSHION is the amount of extra to allocate on initialization */
/* EXPAND is the amount of extra to allocate on expansion */

//#define S4FLAG_RECORD_CUSHION 8000
//#define S4FLAG_RECORD_EXPAND  8000
//
//static void f4flagExpand( F4FLAG *f4 )
//{
//   unsigned char *oldFlags = f4->flags ;
//   unsigned long nFlags ;
//
//   nFlags = f4->numFlags + S4FLAG_RECORD_EXPAND ;
//
//   if ( f4->nFlags > nFlags )  /* means overflow, too many records, so don't bitmap */
//   {
//      u4free( oldFlags ) ;
//      f4->numFlags = 0 ;
//      return ;
//   }
//
//   f4->flags = (unsigned char *)u4allocFree( f4->codeBase, nFlags >> 3 ) ;
//
//   if ( f4->flags == 0 )
//   {
//      error4set( f4->codeBase, 0 ) ;
//      u4free( oldFlags ) ;
//      f4->numFlags = 0 ;
//      return ;
//   }
//
//   memcpy( f4->flags, oldFlags, f4->numFlags >> 3 ) ;
//   memset( f4->flags + f4->numFlags >> 3, (f4->isFlip ? ((unsigned char)0xFF) : (0) ), S4FLAG_RECORD_EXPAND ) ;
//   u4free( oldFlags ) ;
//   f4->numFlags = nFlags ;
//}

int S4FUNCTION f4flagInit( F4FLAG *f4, CODE4 *c4, const unsigned long nFlags, Bool5 allocateCushion )
{
   #ifdef E4PARM_LOW
      if ( f4 == 0 || c4 == 0 )
         return error4( c4, e4parm, E90812 ) ;
   #endif

   c4memset( (void *)f4, 0, sizeof(F4FLAG) ) ;

   if ( nFlags == 0 )
      return 0 ;

   f4->codeBase = c4 ;

//   if ( allocateCushion )
//   {
//      /* include cusion, and round off to nearerst 8 (i.e. put on byte boundary) */
//      nFlags = nFlagsIn + S4FLAG_RECORD_CUSHION + 8 ;
//      nFlags >= 3 ;
//      nFlags <= 3 ;
//      if ( nFlagsIn > nFlags )  /* means overflow, too many records, so don't bitmap */
//         return 0 ;
//      f4->flags = (unsigned char *)u4allocFree( c4, nFlags >> 3 ) ;
//   }
//   else
//   {
      f4->flags = (unsigned char *)u4allocFree( c4, ( nFlags >> 3 ) + 2 ) ;
//   }

   f4->numFlags = nFlags ;

   if ( f4->flags == 0 )
      return e4memory ;
   return 0 ;
}

int S4FUNCTION f4flagReset( F4FLAG *f4, const unsigned long flagNum )
{
   unsigned char lowVal, setVal ;
   unsigned long highVal ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( f4->codeBase, e4parm, E90812 ) ;
   #endif

   #ifdef E4MISC
      /* do in E4MISC becuase user may have an out-of-date index in r4relate.c, catch this
         instead of a gpf later */
      if ( flagNum > f4->numFlags )
         return error4( f4->codeBase, e4info, E90812 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( f4->flags == 0 )
         return error4( f4->codeBase, e4info, E90812 ) ;
   #endif

   lowVal = (unsigned char) (flagNum & 0x7) ;
   highVal = flagNum >> 3 ;
   setVal = (unsigned char) (1 << lowVal) ;
   setVal = (unsigned char) ~setVal ;

   f4->flags[highVal] = (char) (setVal & f4->flags[highVal]) ;

   return 0 ;
}

int S4FUNCTION f4flagSet( F4FLAG *f4, const unsigned long flagNum )
{
   unsigned char lowVal, setVal ;
   unsigned long highVal ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( f4->codeBase, e4parm, E90812 ) ;
   #endif

   #ifdef E4MISC
      /* do in E4MISC becuase user may have an out-of-date index in r4relate.c, catch this
         instead of a gpf later */

      if ( flagNum > f4->numFlags )
      {
         return error4( f4->codeBase, e4info, E90812 ) ;
      }
   #endif

   #ifdef E4ANALYZE
      if ( f4->flags == 0 )
         return error4( f4->codeBase, e4info, E90812 ) ;
   #endif

   lowVal = (unsigned char) (flagNum & 0x7) ;
   highVal = flagNum >> 3 ;
   setVal = (unsigned char) (1 << lowVal) ;

   f4->flags[highVal] = (char)(setVal | f4->flags[highVal]) ;

   return 0 ;
}

int S4FUNCTION f4flagSetRange( F4FLAG *f4, const unsigned long flagNum, const unsigned long numFlags )
{
   unsigned long iFlag ;
   int rc ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   for ( iFlag = 0; iFlag < numFlags; iFlag++ )
   {
      rc = f4flagSet( f4, flagNum + iFlag ) ;
      if ( rc < 0 )
         return rc ;
   }
   return 0 ;
}

int S4FUNCTION f4flagIsSet( F4FLAG *f4, const unsigned long flagNum )
{
   unsigned char lowVal, retVal ;
   unsigned long highVal ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   #ifdef E4ANALYZE
      if ( flagNum > f4->numFlags || f4->flags == 0 )
         return error4( f4->codeBase, e4info, E90812 ) ;
   #endif

   lowVal = (unsigned char) (flagNum & 0x7) ;
   highVal = flagNum >> 3 ;
   retVal = (unsigned char) ((1 << lowVal) & f4->flags[highVal])  ;

   return (int) retVal ;
}

int S4FUNCTION f4flagIsAllSet( F4FLAG *f4, const unsigned long flagNum, const unsigned long nFlags )
{
   int rc ;
   unsigned long iFlag ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   for ( iFlag = flagNum; iFlag <= nFlags; iFlag++ )
   {
      rc = f4flagIsSet( f4, iFlag ) ;
      if ( rc < 0 )
         return rc ;
      if ( rc == 0 )
         return 0 ;
   }
   return 1 ;
}

int S4FUNCTION f4flagIsAnySet( F4FLAG *f4, const unsigned long flagNum, const unsigned long nFlags )
{
   int rc ;
   unsigned long iFlag ;

   #ifdef E4PARM_LOW
      if ( f4 == 0 )
         return error4( 0, e4parm, E90812 ) ;
   #endif

   for ( iFlag = flagNum; iFlag <= nFlags; iFlag++ )
      if ( (rc = f4flagIsSet( f4, iFlag )) < 0 )
         return rc ;
   return 0 ;
}

int S4FUNCTION f4flagAnd( F4FLAG *flagPtr, const F4FLAG *andPtr )
{
   unsigned numBytes ;

   #ifdef E4PARM_LOW
      if ( flagPtr == 0 || andPtr == 0 )
         return error4( 0, e4parm_null, E90812 ) ;
   #endif

   if ( andPtr->numFlags == 0 )
   {
      if ( flagPtr->numFlags == 0 )
         return 0 ;
      if ( flagPtr->isFlip != andPtr->isFlip )
         c4memset( (void *)flagPtr->flags, 1, (unsigned)((flagPtr->numFlags) / 8L + 2L ) ) ;
      else
         c4memset( (void *)flagPtr->flags, 0, (unsigned)((flagPtr->numFlags) / 8L + 2L ) ) ;
      return 0 ;
   }

   #ifdef E4ANALYZE
      if ( flagPtr->numFlags != andPtr->numFlags )
         return error4( flagPtr->codeBase, e4result, E90812 ) ;
   #endif

   numBytes = (unsigned)((flagPtr->numFlags+7)/8) ;

   #ifdef E4ANALYZE
      if ( flagPtr->numFlags != andPtr->numFlags )
         return error4( flagPtr->codeBase, e4result, E90812 ) ;
      if ( (unsigned long)numBytes != ( flagPtr->numFlags + 7L) / 8L )
         return error4( flagPtr->codeBase, e4info, E90812 ) ;
   #endif

   if ( flagPtr->isFlip != andPtr->isFlip )
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
            flagPtr->flags[numBytes] = (unsigned char) (~flagPtr->flags[numBytes] & andPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] &= ~andPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }
   else
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
            flagPtr->flags[numBytes] = (unsigned char) (~flagPtr->flags[numBytes] & ~andPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] &= andPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }

   return 0 ;
}

int S4FUNCTION f4flagOr( F4FLAG *flagPtr, const F4FLAG *orPtr )
{
   unsigned numBytes ;

   #ifdef E4PARM_LOW
      if ( flagPtr == 0 || orPtr == 0 )
         return error4( 0, e4parm_null, E90812 ) ;
   #endif

   if ( orPtr->numFlags == 0 )
      return 0 ;

   numBytes = (unsigned)(flagPtr->numFlags / 8L + 1L ) ;

   #ifdef E4ANALYZE
      if ( flagPtr->numFlags != orPtr->numFlags )
         return error4( flagPtr->codeBase, e4result, E90812 ) ;
      if ( (unsigned long)numBytes != ( flagPtr->numFlags / 8L + 1L ) )
         return error4( flagPtr->codeBase, e4info, E90812 ) ;
   #endif
   if ( flagPtr->isFlip != orPtr->isFlip )
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
           flagPtr->flags[numBytes] = (unsigned char) (~flagPtr->flags[numBytes] | orPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] |= ~orPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }
   else
   {
      if ( flagPtr->isFlip == 1 )
      {
         flagPtr->isFlip = 0 ;
         do
         {
            flagPtr->flags[numBytes] = (unsigned char)(~flagPtr->flags[numBytes] | ~orPtr->flags[numBytes]) ;
         } while (numBytes-- != 0 ) ;
      }
      else
         do
         {
            flagPtr->flags[numBytes] |= orPtr->flags[numBytes] ;
         } while (numBytes-- != 0 ) ;
   }
   return 0 ;
}

void S4FUNCTION f4flagFlipReturns( F4FLAG *flagPtr )
{
   #ifdef E4PARM_LOW
      if ( flagPtr == 0 )
      {
         error4( 0, e4parm_null, E90812 ) ;
         return ;
      }
   #endif

   flagPtr->isFlip = !flagPtr->isFlip ;
}

void S4FUNCTION f4flagSetAll( F4FLAG *flagPtr )
{
   #ifdef E4PARM_LOW
      if ( flagPtr == 0 )
      {
         error4( 0, e4parm_null, E90812 ) ;
         return ;
      }
   #endif

   c4memset( (void *)flagPtr->flags, 0xFF, (unsigned)((flagPtr->numFlags+7)/8) ) ;
}
