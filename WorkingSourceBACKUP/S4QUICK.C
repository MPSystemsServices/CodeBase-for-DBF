/* s4quick.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.

   Iterative Quick Sort Algorithm

   This algorithm is superior to the more traditional recursive
   Quick Sort as the worst case stack size is proportional to 'log(n)'.
   In this algorithm the stack is explicitly maintained.

   In the recursive algorithm, the worst case depth of recursion is
   proportional to 'n'.  For example, if there were 1000 items to
   sort, the Quick Sort could, in the worst case, call itself
   1000 times.

   This routine assumes that there is a record number after the sort
   data for final comparison resolutions in case that two keys are the same.
*/

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

static int sort( void **pointers, int nPointers, int sortLen, S4CMP_FUNCTION *cmp )
{
   /* A stack size of 32 is enough to sort four billion items. */
   int stackStart[32], stackEnd[32], stackOn ;
   int f, l, i, j, k, middle, rc ;
   void *flip_data ;
   S4LONG l1, l2 ;

   stackOn = 0 ;
   stackStart[0] = 0 ;
   stackEnd[0] = nPointers - 1 ;

   while( stackOn >= 0 )
   {
      #ifdef E4ANALYZE
         if ( ( stackOn > sizeof( stackStart ) / sizeof( int ) ) || stackOn < 0  )
            return error4( 0, e4info, E81801 ) ;
      #endif

      f = stackStart[stackOn] ;
      l = stackEnd[stackOn] ;
      #ifdef E4ANALYZE
         if ( f >= nPointers || l >= nPointers || f < 0 || l < 0 )
            error4( 0, e4info, E81801 ) ;
      #endif
      stackOn-- ;

      while( l-f > 7 )
      {
         /* use fast-pivot method to find best partition */
         middle = ( l + f ) / 2 ;
         #ifdef E4ANALYZE
            if ( middle >= nPointers || middle < 0 )
               error4( 0, e4info, E81801 ) ;
         #endif

         /* rc = greater( middle, l ) ; */
         rc = (*cmp)( pointers[middle], pointers[l], (unsigned int)sortLen ) ;
         if ( rc == 0 )
         {
            c4memcpy( (void *)&l1, ((char *)pointers[middle])+sortLen, sizeof(S4LONG) ) ;
            c4memcpy( (void *)&l2, ((char *)pointers[l])+sortLen, sizeof(S4LONG) ) ;
            rc = (l1 > l2) ? 1 : 0 ;
         }
         if ( rc > 0 )
         {
            /* rc = flip( middle, l ) ; */
            flip_data  = pointers[middle] ;
            pointers[middle] = pointers[l] ;
            pointers[l] = flip_data ;
         }

         /* rc = greater( middle, f ) ; */
         rc = (*cmp)( pointers[middle], pointers[f], (unsigned int)sortLen ) ;
         if ( rc == 0 )
         {
            c4memcpy( (void *)&l1, ((char *)pointers[middle])+sortLen, sizeof(S4LONG) ) ;
            c4memcpy( (void *)&l2, ((char *)pointers[f])+sortLen, sizeof(S4LONG) ) ;
            rc = (l1 > l2) ? 1 : 0 ;
         }
         if ( rc > 0 )
         {
            /* rc = flip( f, middle ) ; */
            flip_data  = pointers[f] ;
            pointers[f] = pointers[middle] ;
            pointers[middle] = flip_data ;
         }
         else
         {
            /* rc = greater( f, l ) ; */
            rc = (*cmp)( pointers[f], pointers[l], (unsigned int)sortLen ) ;
            if ( rc == 0 )
            {
               c4memcpy( (void *)&l1, ((char *)pointers[f])+sortLen, sizeof(S4LONG) ) ;
               c4memcpy( (void *)&l2, ((char *)pointers[l])+sortLen, sizeof(S4LONG) ) ;
               rc = (l1 > l2) ? 1 : 0 ;
            }
            if ( rc > 0 )
            {
               /* rc = flip( f , l ) ; */
               flip_data  = pointers[f] ;
               pointers[f] = pointers[l] ;
               pointers[l] = flip_data ;
            }
         }

         /* arrange elements around the partition */
         i = f ;
         j = l ;
         for( ;; )
         {
            do
            {
               i++ ;
               #ifdef E4ANALYZE
                  if ( i >= nPointers )
                     return error4( 0, e4result, E81801 ) ;
               #endif
               /* rc = less( i, f ) ; */
               rc = (*cmp)( pointers[i], pointers[f], (unsigned int)sortLen ) ;
               if ( rc == 0 )
               {
                  c4memcpy( (void *)&l1, ((char *)pointers[i])+sortLen, sizeof(S4LONG) ) ;
                  c4memcpy( (void *)&l2, ((char *)pointers[f])+sortLen, sizeof(S4LONG) ) ;
                  rc = (l1 < l2) ? -1 : 0 ;
               }
            }
            while( rc < 0 ) ;

            do
            {
               j-- ;
               /* rc = greater( j, f ) ; */
               rc = (*cmp)( pointers[j], pointers[f], (unsigned int)sortLen ) ;
               if ( rc == 0 )
               {
                  c4memcpy( (void *)&l1, ((char *)pointers[j])+sortLen, sizeof(S4LONG) ) ;
                  c4memcpy( (void *)&l2, ((char *)pointers[f])+sortLen, sizeof(S4LONG) ) ;
                  rc = (l1 > l2) ? 1 : 0 ;
               }
            }
            while ( rc > 0 ) ;

            if ( i > j )
               break ;

            /* rc = flip( i, j ) ; */
            flip_data  = pointers[i] ;
            pointers[i] = pointers[j] ;
            pointers[j] = flip_data ;
         }

         /* replace partition element where it belongs */
         /* rc = flip( f, j ) ; */
         flip_data  = pointers[f] ;
         pointers[f] = pointers[j] ;
         pointers[j] = flip_data ;

         /* sort both sides of the partition (smaller side first) */
         if ( j - f > l - j )
         {
            /* Left sort is larger, put it on the stack */
            stackStart[++stackOn] = f ;
            stackEnd[stackOn] = j - 1 ;
            f = j + 1 ;
         }
         else
         {
            /* Right sort is larger, put it on the stack */
            stackStart[++stackOn] = j + 1 ;
            stackEnd[stackOn] = l ;
            l = j - 1 ;
         }
         #ifdef E4ANALYZE
            if ( ( stackOn > sizeof( stackStart ) / sizeof( int ) ) || stackOn < 0  )
               return error4( 0, e4info, E81801 ) ;
         #endif
      }

      /* for a small number of items sort using an insertion algorithm */
      for ( i = f+1 ; i <= l ; i++ )
      {
         for ( j = i ; j > f ; j-- )
         {
            k = j-1 ;
            /* greater( j-1, j ) */
            rc = (*cmp)( pointers[k], pointers[j], (unsigned int)sortLen ) ;
            if ( rc == 0 )
            {
               c4memcpy( (void *)&l1, ((char *)pointers[k])+sortLen, sizeof(S4LONG) ) ;
               c4memcpy( (void *)&l2, ((char *)pointers[j])+sortLen, sizeof(S4LONG) ) ;
               rc = (l1 > l2) ? 1 : 0 ;
            }
            if ( rc <= 0 )
               break ;

            /* flip( j-1, j ) ; */
            flip_data  = pointers[k] ;
            pointers[k] = pointers[j] ;
            pointers[j] = flip_data ;
         }
      }
   }

   return 0 ;
}

int s4quick( void **p, const int pN, S4CMP_FUNCTION *cmpRoutine, const int width )
{
   int rc ;

   if ( pN <= 0 )
      return 0 ;

   rc = sort( p, pN, width, cmpRoutine ) ;

   return rc ;
}
