/* i4positi.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4CLIENT
#ifndef S4INDEX_OFF

/* 'pos' is an percentage, positioning is approximate. */
int tfile4position2( TAG4FILE *t4, double *result )
{
   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   *result = tfile4position( t4 ) ;
   return 0 ;
}

#ifndef N4OTHER

double tfile4positionDbl( TAG4FILE *t4 )
{
   double pos ;
   B4BLOCK *blockOn ;
   int n, min, max ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return (double)e4codeBase ;

   pos = .5 ;
   min = max = 1 ;    /* is the position a minimal or maximal position? */
   for ( blockOn = (B4BLOCK *)t4->blocks.lastNode ; blockOn != 0 ; )
   {
      #ifdef S4FOX
         n = blockOn->header.nKeys ;
      #else
         n = blockOn->nKeys + 1 ;
         if ( b4leaf( blockOn ) )
            n-- ;
      #endif

      if( n == 0 )
         max = 0 ;
      else
      {
         if ( min == 1 )
            if ( blockOn->keyOn != 0 )
               min = 0 ;
         if ( max == 1 )
            if ( blockOn->keyOn != ( n - 1 ) )
               max = 0 ;
         pos = ( blockOn->keyOn + pos ) / n ;
      }

      blockOn = (B4BLOCK *)blockOn->link.p ;
      if ( blockOn == (B4BLOCK *)t4->blocks.lastNode )
         break ;
   }

   if ( max == 1 )
      pos = 1.0 ;
   if ( min == 1 )
      pos = 0.0 ;

   #ifdef S4FOX
      if ( t4->header.descending )   /* backwards in file... */
         return 1.0 - pos ;
      else
   #endif
   return pos ;
}

double tfile4position( TAG4FILE *t4 )
{
   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return (double)error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return (double)e4codeBase ;

   return tfile4positionDbl( t4 ) ;
}

int tfile4positionSet( TAG4FILE *t4, const double ipos )
{
   int rc, n, finalPos ;
   B4BLOCK *blockOn ;
   double pos ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   #ifndef S4SINGLE
      index4versionCheck( t4->indexFile, 0 ) ;
   #endif

   #ifdef S4FOX
      if ( t4->header.descending )   /* backwards in file... */
         pos = 1.0 - ipos ;
      else
   #endif
      pos = ipos ;

   if ( tfile4upToRoot( t4 ) < 0 )
      return -1 ;

   for(;;)
   {
      #ifdef E4PARM_LOW
         if ( pos < 0.0 || pos > 1.0 )  /* Could be caused by rounding error ? */
            return error4( t4->codeBase, e4parm, E81604 ) ;
      #endif

      blockOn = tfile4block( t4 ) ;

      #ifdef S4FOX
         n = blockOn->header.nKeys ;
      #else
         n = blockOn->nKeys+1 ;
         if ( b4leaf( blockOn ) )
            n-- ;
      #endif

      finalPos = (int)( n * pos ) ;
      if ( finalPos == n )
         finalPos-- ;

      #ifdef S4FOX
         b4go( blockOn, finalPos ) ;
      #else
         blockOn->keyOn = finalPos ;
      #endif

      pos = pos*n - finalPos ;

      rc = tfile4down( t4 ) ;
      if ( rc < 0 )
         return -1 ;
      if ( rc == 1 )
         return 0 ;
   }
}

#endif   /*  ifndef N4OTHER  */

#ifdef N4OTHER

double tfile4positionDbl( TAG4FILE *t4 )
{
   return tfile4position( t4 ) ;
}

double tfile4position( TAG4FILE *t4 )
{
   double pos ;
   B4BLOCK *blockOn ;
   int n, min, max ;
   #ifdef S4CLIPPER
      int first = 1 ;
   #endif

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return (double)error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return (double)e4codeBase ;

   blockOn = (B4BLOCK *) t4->blocks.lastNode ;

   if ( !b4leaf(blockOn) )
      pos = 1.0 ;
   else
      pos = .5 ;

   min = max = 1 ;    /* is the position a minimal or maximal position? */

   for ( ; blockOn != 0 ; )
   {
      n = blockOn->nKeys + 1 ;
      #ifdef S4CLIPPER
         if ( first )
         {
            if ( b4leaf( blockOn ) )
               n-- ;
            else /* if on a branch block, cannot be at min or max position */
            {
               min = 0 ;
               max = 0 ;
            }
            first = 0 ;
         }
      #else
         if ( b4leaf( blockOn ) )
            n-- ;
      #endif

      if( n == 0 )
         max = 0 ;
      else
      {
         if ( min == 1 )
            if ( blockOn->keyOn != 0 )
               min = 0 ;
         if ( max == 1 )
            if ( blockOn->keyOn != ( n - 1 ) )
               max = 0 ;
         pos = ( blockOn->keyOn + pos ) / n ;
      }

      blockOn = (B4BLOCK *) blockOn->link.p ;
      if ( blockOn == (B4BLOCK *)t4->blocks.lastNode )
         break ;
   }
   if ( max == 1 )
      pos = 1.0 ;
   if ( min == 1 )
      pos = 0.0 ;

   #ifdef S4CLIPPER
      if ( t4->header.descending )   /* backwards in file... */
         return 1.0 - pos ;
      else
   #endif
   return pos ;
}

int tfile4positionSet( TAG4FILE *t4, const double posIn )
{
   int rc, n, finalPos ;
   B4BLOCK *blockOn ;
   double pos ;

   #ifdef E4PARM_LOW
      if ( t4 == 0 )
         return error4( 0, e4parm_null, E91642 ) ;
   #endif

   if ( error4code( t4->codeBase ) < 0 )
      return e4codeBase ;

   #ifdef S4CLIPPER
      if ( t4->header.descending )   /* backwards in file... */
         pos = 1.0 - posIn ;
      else
   #endif
   pos = posIn ;

   rc = tfile4upToRoot( t4 ) ;
   if ( rc < 0 )
      return error4stack( t4->codeBase, rc, E91642 ) ;

   for(;;)
   {
      #ifdef E4PARM_LOW
         if ( pos < 0.0 || pos > 1.0 )  /* Could be caused by rounding error ? */
            return error4( 0, e4parm_null, E81604 ) ;
      #endif

      blockOn = tfile4block( t4 ) ;

      n = blockOn->nKeys + 1 ;
      if ( b4leaf( blockOn ) )
         n-- ;

      finalPos = (int)( n * pos ) ;
      if ( finalPos == n )
         finalPos-- ;

      blockOn->keyOn = finalPos ;
      pos = pos*n - finalPos ;

      rc = tfile4down( t4 ) ;
      if ( rc < 0 )
         return -1 ;
      if ( rc == 1 )
         return 0 ;
   }
}

#endif  /* N4OTHER */
#endif  /* S4INDEX_OFF */
#endif  /* S4CLIENT */
