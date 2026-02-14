/* i4key.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

char *S4FUNCTION t4getKey( TAG4 *t4 )
{
   #if !defined(S4CLIENT) && !defined(S4OFF_INDEX)
      return tfile4key( t4->tagFile ) ;
   #else
      return 0 ;
   #endif
}

#if !defined(S4CLIENT) && !defined(S4INDEX_OFF)
   char *S4FUNCTION tfile4key( TAG4FILE *t4 )
   {
      B4BLOCK *b4 ;

      #ifdef E4VBASIC
         if ( c4parm_check( t4->codeBase, 1, E91630 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
         {
            error4( 0, e4parm_null, E91630 ) ;
            return 0 ;
         }
      #endif

      if ( error4code( t4->codeBase ) < 0 )
         return 0 ;

      b4 = (B4BLOCK *)(t4->blocks.lastNode) ;

      if ( b4 == 0 )
         return 0 ;

      if ( b4->keyOn >= b4numKeys( b4 ) )  /* eof */
         return 0 ;

      return (char *)b4keyKey( b4, b4->keyOn ) ;
   }
#endif  /* !S4CLIENT && !S4OFF_INDEX */
