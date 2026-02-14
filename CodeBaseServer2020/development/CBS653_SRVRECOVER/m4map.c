/* m4map.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4CLIENT
#ifndef S4INDEX_OFF

static void bitmaps4free( BITMAP4 * ) ;

/* create a single bitmap structure */
BITMAP4 *bitmap4create( L4LOGICAL *log, RELATE4 *relate, const char andOr, const char branch )
{
   BITMAP4 *map ;

   map = (BITMAP4 *)mem4allocZero( log->codeBase->bitmapMemory ) ;
   if ( map == 0 )  /* must handle by freeing... */
      return 0 ;

   map->log = log ;
   map->relate = relate ;
   map->andOr = andOr ;
   map->branch = branch ;
   return map ;
}

/* free up a single bitmap structure */
int bitmap4destroy( BITMAP4 *map )
{
   CONST4 *c_on, *c_next ;

   #ifdef E4PARM_HIGH
      if ( map == 0 )
         return error4( 0, e4parm_null, E93701 ) ;
   #endif

   c_on = (CONST4 *)l4first( &map->ne ) ;
   while( c_on != 0 )
   {
      c_next = (CONST4 *)l4next( &map->ne, c_on ) ;
      const4deleteNe( &map->ne, c_on ) ;
      c_on = c_next ;
   }
   // AS 06/28/00 - was not destroying the child maps...
   for ( ;; )
   {
      BITMAP4 *child = (BITMAP4 *)l4pop( &(map->children) ) ;
      if ( child == 0 )
         break ;
      bitmap4destroy( child ) ;
   }
   mem4free( map->log->codeBase->bitmapMemory, map ) ;
   return 0 ;
}

/* can a query or subquery be bitmap optimized?  Also builds the bitmap representation */
static BITMAP4 *bitmap4can( L4LOGICAL *log, int *savePos, RELATE4 *relate )
{
   E4INFO *infoPtr, *infoTwo ;
   int i, keyLen, tagPos, tagPos2, constPos, pos2, *pos ;
   BITMAP4 *map, *childMap ;
   CONST4 *temp, hold ;
   char cTemp ;

   infoPtr = log->expr->info + *savePos ;

   pos = &pos2 ;
   pos2 = *savePos ;
   *savePos = *savePos - infoPtr->numEntries ;

   if ( infoPtr->functionI == E4AND || infoPtr->functionI == E4OR )
   {
      (*pos)-- ;
      if ( infoPtr->functionI == E4AND && relate == 0 )
         relate = (RELATE4 *)log->infoReport[*pos].relateDataList->pointers[0] ;

      if ( infoPtr->functionI == E4AND )
         cTemp = 1 ;
      else
         cTemp = 2 ;

      map = bitmap4create( log, relate, cTemp, 1 ) ;
      if ( map == 0 )
         return 0 ;

      for( i = 0 ; i < infoPtr->numParms ; i++ )
      {
         childMap = bitmap4can( log, pos, relate ) ;
         if ( childMap == 0 && error4code( log->codeBase ) == e4memory )
         {
            error4set( log->codeBase, 0 ) ;
            bitmaps4free( map ) ;
            return 0 ;
         }

         if ( childMap != 0 )
         {
            l4add( &map->children, childMap ) ;
            if ( childMap->andOr == 0 )
               childMap->andOr = map->andOr ;
         }
         else
            if ( cTemp == 2 )   /* if an or case, then cannot create if any sub-expression is not bitmap optimizeable */
               for ( ;; )
               {
                  childMap = (BITMAP4 *)l4first( &map->children ) ;
                  if ( childMap == 0 )
                  {
                     bitmap4destroy( map ) ;
                     return 0 ;
                  }
                  l4remove( &map->children, childMap ) ;
                  bitmap4destroy( childMap ) ;
               }
      }
      if ( map->children.nLink == 0 )   /* not bitmap optimizeable */
      {
         bitmap4destroy( map ) ;
         return 0 ;
      }
   }
   else
   {
      // AS 08/11/00 had problems when query was '1000 = val', etc
      int functionI = infoPtr->functionI ;
      if ( functionI >= E4COMPARE_START && functionI <= E4COMPARE_END )
      {
         /* One must be a constant and the other a tag. */
         infoPtr-- ;
         tagPos = *pos - 1 ;
         tagPos2 = tagPos - infoPtr->numEntries ;
         infoTwo = infoPtr - infoPtr->numEntries ;
         (*pos) -= 1 + infoPtr->numEntries + infoTwo->numEntries ;

         if ( e4isConstant( infoPtr ) )
         {
            if ( !e4isTag( log->infoReport + tagPos2, log->expr, infoTwo, relate->data ) )
               return 0 ;
            constPos = tagPos ;
            tagPos = tagPos2 ;
         }
         else
         {
            if ( e4isConstant( infoTwo ) == 0 || !e4isTag( log->infoReport + tagPos, log->expr, infoPtr, relate->data ) )
               return 0 ;
            constPos = tagPos2 ;
            /* infoPtr = infoTwo ; */
         }

         map = bitmap4create( log, relate, 0, 0 ) ;
         if ( map == 0 )
            return 0 ;
         map->tag = ( log->infoReport + tagPos )->tag ;

         // infoPtr++ ;

         c4memset( (void *)&hold, 0, sizeof( CONST4 ) ) ;
         if ( const4get( &hold, map, log, constPos ) < 0 )
         {
            bitmap4destroy( map ) ;
            return 0 ;
         }

         if ( functionI >= E4COMPARE_START && functionI <= E4COMPARE_END )
         {
            if ( functionI >= E4NOT_EQUAL && functionI < E4GREATER_EQ ) /* != */
            {
               temp = (CONST4 *)u4alloc( (long)sizeof( CONST4 ) ) ;
               if ( temp == 0 )
               {
                  error4set( log->codeBase, 0 ) ;
                  bitmaps4free( map ) ;
                  return 0 ;
               }
               c4memcpy( (void *)temp, (void *)&hold, sizeof( CONST4 ) ) ;
               l4add( &map->ne, temp ) ;
            }
            if (functionI >= E4EQUAL && functionI < E4GREATER) /* == */
               c4memcpy( (void *)&map->eq, (void *)&hold, sizeof( CONST4 ) ) ;
            if ( functionI >= E4GREATER && functionI < E4LESS ) /* > */
            {
               keyLen = map->tag->header.keyLen ;
               #ifdef S4VFP_KEY
                  if ( map->type == r4str && tfile4vfpKey( map->tag ) )
                  {
                     #ifdef E4ANALYZE
                        if ( keyLen % 2 == 1 )
                        {
                           error4( log->codeBase, e4index, E82107 ) ;
                           return 0 ;
                        }
                     #endif
                     keyLen = keyLen / 2 ;
                  }
               #endif
               if ( map->type == r4str && hold.len < keyLen )   /* same as >= since a partial > */
                  c4memcpy( (void *)&map->ge, (void *)&hold, sizeof( CONST4 ) ) ;
               else
                  c4memcpy( (void *)&map->gt, (void *)&hold, sizeof( CONST4 ) ) ;
            }
            if ( functionI >= E4GREATER_EQ && functionI < E4LESS_EQ ) /* >= */
               c4memcpy( (void *)&map->ge, (void *)&hold, sizeof( CONST4 ) ) ;
            if ( functionI >= E4LESS && functionI < E4CHR )  /* < */
               c4memcpy( (void *)&map->lt, (void *)&hold, sizeof( CONST4 ) ) ;
            if ( functionI >= E4LESS_EQ && functionI < E4EQUAL ) /* <= */
               c4memcpy( (void *)&map->le, (void *)&hold, sizeof( CONST4 ) ) ;
         }
      }
      else
         return 0 ;
   }

   return map ;
}

/* free the bitmap tree */
static void bitmaps4free( BITMAP4 *map )
{
   BITMAP4 *mapOn, *mapNext ;

   if ( map->branch == 1 )
   {
      mapOn = (BITMAP4 *)l4first( &map->children ) ;
      while( mapOn != 0 )
      {
         mapNext = (BITMAP4 *)l4next( &map->children, mapOn ) ;
         l4remove( &map->children, mapOn ) ;
         bitmaps4free( mapOn ) ;
         mapOn = mapNext ;
      }
   }

   bitmap4destroy( map ) ;
}

/* free all the bitmap info */
static void bitmap4free( L4LOGICAL *log, BITMAP4 *map )
{
   bitmaps4free( map ) ;
   u4free( log->buf ) ;
   log->buf = 0 ;
   log->bufLen = 0 ;
   log->bufPos = 0 ;
}

/* initialize the bitmap structures, determine if bitmapping is possible */
static BITMAP4 *bitmap4init( L4LOGICAL *log, int pos )
{
   E4INFO *infoPtr ;
   int passPos ;
   BITMAP4 *map ;

   infoPtr = log->expr->info + pos ;

   /* for testing purposes only, allow bitmap disabling */
   if ( log->relation->bitmapDisable == 1 || ( (unsigned long)d4recCount( log->relation->relate.data ) / 16UL >= ( (unsigned long)((unsigned long)UINT_MAX - 2UL)) / 2UL ) )
   {
      log->relation->bitmapsFreed = 1 ;
      return 0 ;
   }

   if ( log->codeBase->bitmapMemory == 0 )
   {
      log->codeBase->bitmapMemory = mem4create( log->codeBase, 10, sizeof( BITMAP4 ), 5, 0 ) ;
      if ( log->codeBase->bitmapMemory == 0 )  /* no memory available for bitmap optimization */
         return 0 ;
   }

   passPos = pos ;
   if ( infoPtr->functionI == E4AND )
      map = bitmap4can( log, &passPos, &log->relation->relate ) ;
   else
      map = bitmap4can( log, &passPos, &log->relation->relate ) ;

   if ( map == 0 && error4code( log->codeBase ) == e4memory )
      error4set( log->codeBase, 0 ) ;

   return map ;
}

/* this function removes all bitmaps from the parent and marks the parent as zero */
static BITMAP4 *bitmap4collapse( BITMAP4 *parent )
{
   BITMAP4 *childOn, *childNext ;

   childOn = (BITMAP4 *)l4first( &parent->children ) ;
   if ( parent->tag == 0 && childOn->tag != 0 )
      parent->tag = childOn->tag ;
   while( childOn != 0 )
   {
      #ifdef E4ANALYZE
         if ( childOn->tag == 0 && childOn->children.nLink == 0 )
         {
            error4( parent->log->codeBase, e4info, E93701 ) ;
            return 0 ;
         }
      #endif
      childNext = (BITMAP4 *)l4next( &parent->children, childOn ) ;
      l4remove( &parent->children, childOn ) ;
      bitmap4destroy( childOn ) ;
      childOn = childNext ;
   }
   parent->noMatch = 1 ;
   return 0 ;
}


/* 0 = successful merger, 1 means collaps because now no records belong to the set */
static int bitmap4combineAndLe( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn, *cNext ;
   char eqFound ;

   if ( map1->eq.len )
   {
      if ( const4less( &map2->le, &map1->eq, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->gt.len )
      if ( const4lessEq( &map2->le, &map1->gt, map1 ) )
         return 1 ;

   if ( map1->ge.len )
   {
      if ( const4less( &map2->le, &map1->ge, map1 ) )
         return 1 ;
      if ( const4eq( &map2->le, &map1->ge, map1 ) )
      {
         if ( map2->eq.len )
            if ( !const4eq( &map2->eq, &map2->le, map1 ) )
               return 1 ;
         c4memcpy( (void *)&map2->eq, (void *)&map2->le, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      eqFound = 0 ;
      while ( cOn != 0 )
      {
         cNext = (CONST4 *)l4next( &map1->ne, cOn ) ;
         if ( const4less( &map2->le, cOn, map1 ) )
            const4deleteNe( &map1->ne, cOn ) ;
         else
            if ( eqFound == 0 )
               if ( const4eq( &map2->le, cOn, map1 ) )
                  eqFound = 1 ;
         cOn = cNext ;
      }
      if ( eqFound )
      {
         c4memcpy( (void *)&map2->lt, (void *)&map2->le, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->lt.len )
   {
      if ( const4less( &map2->le, &map1->lt, map1 ) )
      {
         c4memcpy( (void *)&map1->le, (void *)&map2->le, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map1->lt, 0, sizeof( CONST4 ) ) ;
      }
   }
   else
   {
      if ( map1->le.len )
      {
         if ( const4less( &map2->le, &map1->le, map1 ) )
            c4memcpy( (void *)&map1->le, (void *)&map2->le, sizeof( CONST4 ) ) ;
      }
      else
         c4memcpy( (void *)&map1->le, (void *)&map2->le, sizeof( CONST4 ) ) ;
   }

   c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

/* 0 = successful merger, 1 means collaps because now no records belong to the set */
static int bitmap4combineAndGe( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn, *cNext ;
   char eqFound ;

   if ( map1->eq.len )
   {
      if ( const4less( &map1->eq, &map2->ge, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->lt.len )
   {
      if ( const4lessEq( &map1->lt, &map2->ge, map1 ) )
         return 1 ;
   }
   else
      if ( map1->le.len )
      {
         if ( const4less( &map1->le, &map2->ge, map1 ) )
            return 1 ;
         if ( const4eq( &map1->le, &map2->ge, map1 ) )
         {
            if ( map2->eq.len )
               if ( !const4eq( &map2->eq, &map2->ge, map1 ) )
                  return 1 ;
            c4memcpy( (void *)&map2->eq, (void *)&map2->ge, sizeof( CONST4 ) ) ;
            c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
            return 0 ;
         }
      }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      eqFound = 0 ;
      while ( cOn != 0 )
      {
         cNext = (CONST4 *)l4next( &map1->ne, cOn ) ;
         if ( const4less( cOn, &map2->ge, map1 ) )
            const4deleteNe( &map1->ne, cOn ) ;
         else
            if ( eqFound == 0 )
               if ( const4eq( cOn, &map2->ge, map1 ) )
                  eqFound = 1 ;
         cOn = cNext ;
      }
      if ( eqFound )
      {
         c4memcpy( (void *)&map2->gt, (void *)&map2->ge, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->gt.len )
   {
      if ( const4less( &map1->gt, &map2->ge, map1 ) )
      {
         c4memcpy( (void *)&map1->ge, (void *)&map2->ge, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map1->gt, 0, sizeof( CONST4 ) ) ;
      }
   }
   else
   {
      if ( map1->ge.len )
      {
         if ( const4less( &map1->ge, &map2->ge, map1 ) )
            c4memcpy( (void *)&map1->ge, (void *)&map2->ge, sizeof( CONST4 ) ) ;
      }
      else
         c4memcpy( (void *)&map1->ge, (void *)&map2->ge, sizeof( CONST4 ) ) ;
   }
   c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

/* 0 = successful merger, 1 means collaps because now no records belong to the set */
static int bitmap4combineAndLt( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn, *cNext ;

   if ( map1->eq.len )
   {
      if ( const4lessEq( &map2->lt, &map1->eq, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      while ( cOn != 0 )
      {
         cNext = (CONST4 *)l4next( &map1->ne, cOn ) ;
         if ( const4lessEq( &map2->lt, cOn, map1 ) )
            const4deleteNe( &map1->ne, cOn ) ;
         cOn = cNext ;
      }
   }

   if ( map1->gt.len )
   {
      if ( const4lessEq( &map2->lt, &map1->gt, map1 ) )
         return 1 ;
   }
   else
      if ( map1->ge.len )
         if ( const4lessEq( &map2->lt, &map1->ge, map1 ) )
            return 1 ;

   if ( map1->lt.len )
   {
      if ( const4lessEq( &map2->lt, &map1->lt, map1 ) )
         c4memcpy( (void *)&map1->lt, (void *)&map2->lt, sizeof( CONST4 ) ) ;
   }
   else
   {
      if ( map1->le.len )
      {
         if ( const4lessEq( &map2->lt, &map1->le, map1 ) )
         {
            c4memcpy( (void *)&map1->lt, (void *)&map2->lt, sizeof( CONST4 ) ) ;
            c4memset( (void *)&map1->le, 0, sizeof( CONST4 ) ) ;
         }
      }
      else
         c4memcpy( (void *)&map1->lt, (void *)&map2->lt, sizeof( CONST4 ) ) ;
   }
   c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

/* 0 = successful merger, 1 means collaps because now no records belong to the set */
static int bitmap4combineAndGt( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn, *cNext ;

   if ( map1->eq.len )
   {
      if ( const4lessEq( &map1->eq, &map2->gt, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      while ( cOn != 0 )
      {
         cNext = (CONST4 *)l4next( &map1->ne, cOn ) ;
         if ( const4lessEq( cOn, &map2->gt, map1 ) )
            const4deleteNe( &map1->ne, cOn ) ;
         cOn = cNext ;
      }
   }

   if ( map1->lt.len )
   {
      if ( const4lessEq( &map1->lt, &map2->gt, map1 ) )
         return 1 ;
   }
   else
      if ( map1->le.len )
         if ( const4lessEq( &map1->le, &map2->gt, map1 ) )
            return 1 ;

   if ( map1->gt.len )
   {
      if ( const4less( &map1->gt, &map2->gt, map1 ) )
         c4memcpy( (void *)&map1->gt, (void *)&map2->gt, sizeof( CONST4 ) ) ;
   }
   else
   {
      if ( map1->ge.len )
      {
         if ( const4lessEq( &map1->ge, &map2->gt, map1 ) )
         {
            c4memcpy( (void *)&map1->gt, (void *)&map2->gt, sizeof( CONST4 ) ) ;
            c4memset( (void *)&map1->ge, 0, sizeof( CONST4 ) ) ;
         }
      }
      else
         c4memcpy( (void *)&map1->gt, (void *)&map2->gt, sizeof( CONST4 ) ) ;
   }
   c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

/* 0 = successful merger, 1 means collaps because now no records belong to the set */
static int bitmap4combineAndNe( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn, *cNext, *cOn2, *cNext2 ;
   int eqFound ;

   cOn = (CONST4 *)l4first( &map2->ne ) ;
   while ( cOn != 0 )
   {
      cNext = (CONST4 *)l4next( &map2->ne, cOn ) ;

      if ( map1->eq.len )
      {
         if ( const4eq( cOn, &map1->eq, map1 ) )
            return 1 ;
         else
         {
            const4deleteNe( &map2->ne, cOn ) ;
            cOn = cNext ;
            continue ;
         }
      }

      if ( map1->gt.len )
      {
         if ( const4lessEq( cOn, &map1->gt, map1 ) )
         {
            const4deleteNe( &map2->ne, cOn ) ;
            cOn = cNext ;
            continue ;
         }
      }
      else
         if ( map1->ge.len )
         {
            if ( const4eq( cOn, &map1->ge, map1 ) )
            {
               c4memcpy( (void *)&map1->gt, (void *)&map1->ge, sizeof( CONST4 ) ) ;
               c4memset( (void *)&map1->ge, 0, sizeof( CONST4 ) ) ;
               const4deleteNe( &map2->ne, cOn ) ;
               cOn = cNext ;
               continue ;
            }
            if ( const4lessEq( cOn, &map1->ge, map1 ) )
            {
               const4deleteNe( &map2->ne, cOn ) ;
               cOn = cNext ;
               continue ;
            }
         }

      if ( map1->lt.len )
      {
         if ( const4lessEq( &map1->lt, cOn, map1 ) )
         {
            const4deleteNe( &map2->ne, cOn ) ;
            cOn = cNext ;
            continue ;
         }
      }
      else
         if ( map1->le.len )
         {
            if ( const4eq( cOn, &map1->le, map1 ) )
            {
               c4memcpy( (void *)&map1->lt, (void *)&map1->le, sizeof( CONST4 ) ) ;
               c4memset( (void *)&map1->le, 0, sizeof( CONST4 ) ) ;
               const4deleteNe( &map2->ne, cOn ) ;
               cOn = cNext ;
               continue ;
            }
            if ( const4lessEq( &map1->le, cOn, map1 ) )
            {
               const4deleteNe( &map2->ne, cOn ) ;
               cOn = cNext ;
               continue ;
            }
         }

      if ( map1->ne.nLink != 0 )  /* special case */
      {
         cOn2 = (CONST4 *)l4first( &map1->ne ) ;
         eqFound = 0 ;
         while ( cOn2 != 0 )
         {
            cNext2 = (CONST4 *)l4next( &map1->ne, cOn2 ) ;
            if ( const4eq( cOn, cOn2, map1 ) )
            {
               const4deleteNe( &map2->ne, cOn ) ;
               cOn = cNext ;
               eqFound = 1 ;
               break ;
            }
            cOn2 = cNext2 ;
         }
         if ( eqFound != 1 )
         {
            l4remove( &map2->ne, cOn ) ;
            l4add( &map1->ne, cOn ) ;
         }
         cOn = cNext ;
         continue ;
      }

      /* must add the ne */
      l4remove( &map2->ne, cOn ) ;
      l4add( &map1->ne, cOn ) ;
      cOn = cNext ;
   }

   return 0 ;
}

/* 0 = successful merger, 1 means collaps because now no records belong to the set */
static int bitmap4combineAndEq( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn, *cNext ;

   if ( map1->eq.len )
   {
      if ( const4eq( &map2->eq, &map1->eq, map1 ) )
      {
         c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
      else
         return 1 ;
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      while ( cOn != 0 )
      {
         cNext = (CONST4 *)l4next( &map1->ne, cOn ) ;
         if ( const4eq( &map2->eq, cOn, map1 ) )
            return 1 ;
         else
            const4deleteNe( &map1->ne, cOn ) ;
         cOn = cNext ;
      }
   }

   if ( map1->le.len )
   {
      if ( const4less( &map1->le, &map2->eq, map1 ) )
         return 1 ;
      c4memset( (void *)&map1->le, 0, sizeof( CONST4 ) ) ;
   }
   else
      if ( map1->lt.len )
      {
         if ( const4lessEq( &map1->lt, &map2->eq, map1 ) )
            return 1 ;
         c4memset( (void *)&map1->lt, 0, sizeof( CONST4 ) ) ;
      }

   if ( map1->ge.len )
   {
      if ( const4less( &map2->eq, &map1->ge, map1 ) )
         return 1 ;
      c4memset( (void *)&map1->ge, 0, sizeof( CONST4 ) ) ;
   }
   else
      if ( map1->gt.len )
      {
         if ( const4lessEq( &map2->eq, &map1->gt, map1 ) )
            return 1 ;
         c4memset( (void *)&map1->gt, 0, sizeof( CONST4 ) ) ;
      }

   c4memcpy( (void *)&map1->eq, (void *)&map2->eq, sizeof( CONST4 ) ) ;
   c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

static int bitmap4combineOrLe( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn ;

   if ( map1->eq.len )
      if ( const4lessEq( &map1->eq, &map2->le, map1 ) )
         c4memset( (void *)&map1->eq, 0, sizeof( CONST4 ) ) ;

   if ( map1->gt.len )
   {
      if ( const4lessEq( &map1->gt, &map2->le, map1 ) )
         return 1 ;
   }
   else
      if ( map1->ge.len )
         if ( const4lessEq( &map1->ge, &map2->le, map1 ) )
            return 1 ;

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      #ifdef E4ANALYZE
         if ( map1->ne.nLink != 1 )  /* if 2 links, all must belong... */
            return error4( map1->log->codeBase, e4info, E93701 ) ;
      #endif
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      if ( const4lessEq( cOn, &map2->le, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->lt.len )
   {
      if ( const4lessEq( &map1->lt, &map2->le, map1 ) )
         c4memset( (void *)&map1->lt, 0, sizeof( CONST4 ) ) ;
      else
      {
         c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->le.len )
         if ( !const4lessEq( &map1->le, &map2->le, map1 ) )
         {
            c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
            return 0 ;
         }

   c4memcpy( (void *)&map1->le, (void *)&map2->le, sizeof( CONST4 ) ) ;
   c4memset( (void *)&map2->le, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

static int bitmap4combineOrGe( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn ;

   if ( map1->eq.len )
      if ( const4lessEq( &map2->ge, &map1->eq, map1 ) )
         c4memset( (void *)&map1->eq, 0, sizeof( CONST4 ) ) ;

   if ( map1->lt.len )
   {
      if ( const4lessEq( &map2->ge, &map1->lt, map1 ) )
         return 1 ;
   }
   else
      if ( map1->le.len )
         if ( const4lessEq( &map2->ge, &map1->le, map1 ) )
            return 1 ;

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      #ifdef E4ANALYZE
         if ( map1->ne.nLink != 1 )  /* if 2 links, all must belong... */
            return error4( map1->log->codeBase, e4info, E93701 ) ;
      #endif
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      if ( const4lessEq( &map2->ge, cOn, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->gt.len )
   {
      if ( const4lessEq( &map2->ge, &map1->gt, map1 ) )
         c4memset( (void *)&map1->gt, 0, sizeof( CONST4 ) ) ;
      else
      {
         c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->ge.len )
         if ( !const4lessEq( &map2->ge, &map1->ge, map1 ) )
         {
            c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
            return 0 ;
         }

   c4memcpy( (void *)&map1->ge, (void *)&map2->ge, sizeof( CONST4 ) ) ;
   c4memset( (void *)&map2->ge, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

static int bitmap4combineOrLt( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn ;

   if ( map1->eq.len )
   {
      if ( const4eq( &map1->eq, &map2->lt, map1 ) )
      {
         #ifdef E4ANALYZE
            if ( map2->le.len != 0 )
               return error4( map1->log->codeBase, e4info, E93701 ) ;
         #endif
         c4memcpy( (void *)&map2->le, (void *)&map2->lt, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map1->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
      if ( const4less( &map1->eq, &map2->lt, map1 ) )
         c4memset( (void *)&map1->eq, 0 , sizeof( CONST4 ) ) ;
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      #ifdef E4ANALYZE
         if ( map1->ne.nLink != 1 )  /* if 2 links, all must belong... */
            return error4( map1->log->codeBase, e4info, E93701 ) ;
      #endif
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      if ( const4less( cOn, &map2->lt, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->gt.len )
   {
      if ( const4less( &map1->gt, &map2->lt, map1 ) )
         return 1 ;
      if ( const4eq( &map1->gt, &map2->lt, map1 ) )
      {
         const4addNe( map1, &map2->lt ) ;
         c4memset( (void *)&map1->gt, 0, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->ge.len )
         if ( const4lessEq( &map1->ge, &map2->lt, map1 ) )
            return 1 ;

   if ( map1->lt.len )
   {
      if ( !const4less( &map1->lt, &map2->lt, map1 ) )
      {
         c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->le.len )
      {
         if ( const4lessEq( &map2->lt, &map1->le, map1 ) )
         {
            c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
            return 0 ;
         }
         else
            c4memset( (void *)&map1->le, 0, sizeof( CONST4 ) ) ;
      }

   c4memcpy( (void *)&map1->lt, (void *)&map2->lt, sizeof( CONST4 ) ) ;
   c4memset( (void *)&map2->lt, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

static int bitmap4combineOrGt( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn ;

   if ( map1->eq.len )
   {
      if ( const4eq( &map1->eq, &map2->gt, map1 ) )
      {
         #ifdef E4ANALYZE
            if ( map2->ge.len != 0 )
               return error4( map1->log->codeBase, e4info, E93701 ) ;
         #endif
         c4memcpy( (void *)&map2->ge, (void *)&map2->gt, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map1->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
      if ( const4less( &map2->gt, &map1->eq, map1 ) )
         c4memset( (void *)&map1->eq, 0 , sizeof( CONST4 ) ) ;
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      #ifdef E4ANALYZE
         if ( map1->ne.nLink != 1 )  /* if 2 links, all must belong... */
            return error4( map1->log->codeBase, e4info, E93701 ) ;
      #endif
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      if ( const4less( &map2->gt, cOn, map1 ) )
         return 1 ;
      else
      {
         c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->lt.len )
   {
      if ( const4less( &map2->gt, &map1->lt, map1 ) )
         return 1 ;
      if ( const4eq( &map2->gt, &map1->lt, map1 ) )
      {
         const4addNe( map1, &map2->gt ) ;
         c4memset( (void *)&map1->lt, 0, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->le.len )
         if ( const4lessEq( &map2->gt, &map1->le, map1 ) )
            return 1 ;

   if ( map1->gt.len )
   {
      if ( const4less( &map1->gt, &map2->gt, map1 ) )
      {
         c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->ge.len )
      {
         if ( const4lessEq( &map1->ge, &map2->gt, map1 ) )
         {
            c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
            return 0 ;
         }
         else
            c4memset( (void *)&map1->ge, 0, sizeof( CONST4 ) ) ;
      }

   c4memcpy( (void *)&map1->gt, (void *)&map2->gt, sizeof( CONST4 ) ) ;
   c4memset( (void *)&map2->gt, 0, sizeof( CONST4 ) ) ;
   return 0 ;
}

static int bitmap4combineOrEq( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn ;

   if ( map1->eq.len )
   {
      if ( const4eq( &map1->eq, &map2->eq, map1 ) )
      {
         c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      #ifdef E4ANALYZE
         if ( map1->ne.nLink != 1 )  /* if 2 links, all must belong... */
            return error4( map1->log->codeBase, e4info, E93701 ) ;
      #endif
      cOn = (CONST4 *)l4first( &map1->ne ) ;
      if ( const4eq( &map2->eq, cOn, map1 ) )
         return 1 ;
      c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
      return 0 ;
   }

   if ( map1->lt.len )
   {
      if ( const4eq( &map1->lt, &map2->eq, map1 ) )
      {
         c4memcpy( (void *)&map1->le, (void *)&map1->lt, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map1->lt, 0, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
      if ( const4less( &map2->eq, &map1->lt, map1 ) )
      {
         c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->le.len )
         if ( const4lessEq( &map2->eq, &map1->le, map1 ) )
         {
            c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
            return 0 ;
         }

   if ( map1->gt.len )
   {
      if ( const4eq( &map1->gt, &map2->eq, map1 ) )
      {
         c4memcpy( (void *)&map1->ge, (void *)&map1->gt, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map1->gt, 0, sizeof( CONST4 ) ) ;
         c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
      if ( const4less( &map1->gt, &map2->eq, map1 ) )
      {
         c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
         return 0 ;
      }
   }
   else
      if ( map1->ge.len )
         if ( const4lessEq( &map1->ge, &map2->eq, map1 ) )
         {
            c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
            return 0 ;
         }

   if ( map1->eq.len == 0 )
   {
      c4memcpy( (void *)&map1->eq, (void *)&map2->eq, sizeof( CONST4 ) ) ;
      c4memset( (void *)&map2->eq, 0, sizeof( CONST4 ) ) ;
   }
   return 0 ;
}

static int bitmap4combineOrNe( BITMAP4 *map1, BITMAP4 *map2 )
{
   CONST4 *cOn, *cOn2 ;

   #ifdef E4ANALYZE
      if ( map2->ne.nLink != 1 )  /* if 2 links, all must belong... */
         return error4( map1->log->codeBase, e4info, E93701 ) ;
   #endif

   cOn = (CONST4 *)l4first( &map2->ne ) ;

   if ( map1->eq.len )
   {
      if ( const4eq( &map1->eq, cOn, map1 ) )
         return 1 ;
      c4memset( (void *)&map1->eq, 0, sizeof( CONST4 ) ) ;
   }

   if ( map1->ne.nLink != 0 )  /* special case */
   {
      #ifdef E4ANALYZE
         if ( map1->ne.nLink != 1 )  /* if 2 links, all must belong... */
            return error4( map1->log->codeBase, e4info, E93701 ) ;
      #endif
      cOn2 = (CONST4 *)l4first( &map1->ne ) ;
      if ( const4eq( cOn2, cOn, map1 ) )
      {
         const4deleteNe( &map2->ne, cOn ) ;
         return 0 ;
      }
      else
         return 1 ;
   }

   if ( map1->lt.len )
   {
      if ( const4less( cOn, &map1->lt, map1 ) )
         return 1 ;
      else
         c4memset( (void *)&map1->lt, 0, sizeof( CONST4 ) ) ;
   }
   else
      if ( map1->le.len )
      {
         if ( const4lessEq( cOn, &map1->le, map1 ) )
            return 1 ;
         else
            c4memset( (void *)&map1->le, 0, sizeof( CONST4 ) ) ;
      }

   if ( map1->gt.len )
   {
      if ( const4less( &map1->gt, cOn, map1 ) )
         return 1 ;
      else
         c4memset( (void *)&map1->gt, 0, sizeof( CONST4 ) ) ;
   }
   else
      if ( map1->ge.len )
      {
         if ( const4lessEq( &map1->ge, cOn, map1 ) )
            return 1 ;
         else
            c4memset( (void *)&map1->ge, 0, sizeof( CONST4 ) ) ;
      }

   l4remove( &map2->ne, cOn ) ;
   l4add( &map1->ne, cOn ) ;
   return 0 ;
}

/* merge two leaf maps together */
BITMAP4 * bitmap4combineLeafs( BITMAP4 *parent, BITMAP4 *map1, BITMAP4 *map2 )
{
   BITMAP4 *childOn ;
   #ifdef E4PARM_LOW
      if ( parent == 0 || map1 == 0 || map2 == 0 )
      {
         error4( 0, e4parm_null, E93701 ) ;
         return 0 ;
      }
      if ( map1->type != map2->type )
      {
         error4( map1->log->codeBase, e4info, E83704 ) ;
         return 0 ;
      }
   #endif

   if ( parent->andOr == 1 )  /* and */
   {
      if ( map2->le.len )
         if ( bitmap4combineAndLe( map1, map2 ) == 1 )
            return bitmap4collapse( parent ) ;

      if ( map2->ge.len )
         if ( bitmap4combineAndGe( map1, map2 ) == 1 )
            return bitmap4collapse( parent ) ;

      if ( map2->lt.len )
         if ( bitmap4combineAndLt( map1, map2 ) == 1 )
            return bitmap4collapse( parent ) ;

      if ( map2->gt.len )
         if ( bitmap4combineAndGt( map1, map2 ) == 1 )
            return bitmap4collapse( parent ) ;

      if ( map2->ne.nLink != 0 )  /* special case */
         if ( bitmap4combineAndNe( map1, map2 ) == 1 )
            return bitmap4collapse( parent ) ;

      if ( map2->eq.len )
         if ( bitmap4combineAndEq( map1, map2 ) == 1 )
            return bitmap4collapse( parent ) ;

      #ifdef E4ANALYZE
         if ( map2->le.len || map2->ge.len || map2->lt.len || map2->gt.len || map2->eq.len || map2->ne.nLink )
         {
            error4( parent->log->codeBase, e4info, E93701 ) ;
            return 0 ;
         }
         if ( map1->eq.len )
            if ( map1->le.len || map1->ge.len || map1->lt.len || map1->gt.len || map1->ne.nLink )
            {
               error4( parent->log->codeBase, e4info, E93701 ) ;
               return 0 ;
            }
         if ( map1->ne.nLink )
            if ( map1->eq.len )
            {
               error4( parent->log->codeBase, e4info, E93701 ) ;
               return 0 ;
            }
         if ( map1->ge.len )
            if ( map1->gt.len || map1->eq.len )
            {
               error4( parent->log->codeBase, e4info, E93701 ) ;
               return 0 ;
            }
         if ( map1->gt.len )
            if ( map1->ge.len || map1->eq.len )
            {
               error4( parent->log->codeBase, e4info, E93701 ) ;
               return 0 ;
            }
         if ( map1->le.len )
            if ( map1->lt.len || map1->eq.len )
            {
               error4( parent->log->codeBase, e4info, E93701 ) ;
               return 0 ;
            }
         if ( map1->lt.len )
            if ( map1->le.len || map1->eq.len )
            {
               error4( parent->log->codeBase, e4info, E93701 ) ;
               return 0 ;
            }
      #endif

      l4remove( &parent->children, map2 ) ;
      bitmap4destroy( map2 ) ;
      return map1 ;
   }
   else if ( parent->andOr == 2 )  /* or */
   {
      childOn = map2 ;
      while( childOn != 0 )
      {
         if ( map2->lt.len )
            if ( bitmap4combineOrLt( map1, map2 ) == 1 )
               return bitmap4collapse( parent ) ;

         if ( map2->gt.len )
            if ( bitmap4combineOrGt( map1, map2 ) == 1 )
               return bitmap4collapse( parent ) ;

         if ( map2->le.len )
            if ( bitmap4combineOrLe( map1, map2 ) == 1 )
               return bitmap4collapse( parent ) ;

         if ( map2->ge.len )
            if ( bitmap4combineOrGe( map1, map2 ) == 1 )
               return bitmap4collapse( parent ) ;

         if ( map2->ne.nLink != 0 )  /* special case */
            if ( bitmap4combineOrNe( map1, map2 ) == 1 )
               return bitmap4collapse( parent ) ;

         if ( map2->eq.len )
            if ( bitmap4combineOrEq( map1, map2 ) == 1 )
               return bitmap4collapse( parent ) ;

         childOn = (BITMAP4 *)l4next( &parent->children, childOn ) ;
      }

      if ( map2->le.len || map2->ge.len || map2->lt.len || map2->gt.len || map2->eq.len || map2->ne.nLink )
         return map2 ;
      else
      {
         l4remove( &parent->children, map2 ) ;
         bitmap4destroy( map2 ) ;
         return map1 ;
      }
   }
   return 0 ;
}

int bitmap4copy( BITMAP4 *to, const BITMAP4 *from )
{
   CONST4 *cOn, *temp ;

   #ifdef E4ANALYZE
      if ( from->branch )
         return error4( to->log->codeBase, e4info, E93701 ) ;
   #endif
   c4memset( (void *)to, 0, sizeof( BITMAP4 ) ) ;
   to->andOr = from->andOr ;
   to->log = from->log ;
   to->relate = from->relate ;
   to->type = from->type ;
   to->tag = from->tag ;
   const4duplicate( &to->lt, &from->lt, from->log ) ;
   const4duplicate( &to->le, &from->le, from->log ) ;
   const4duplicate( &to->gt, &from->gt, from->log ) ;
   const4duplicate( &to->ge, &from->ge, from->log ) ;
   const4duplicate( &to->eq, &from->eq, from->log ) ;
   cOn = (CONST4 *)l4first( &from->ne ) ;
   while ( cOn != 0 )
   {
      temp = (CONST4 *)u4alloc( (long)sizeof( CONST4 ) ) ;
      if ( temp == 0 )
         break ;
      const4duplicate( temp, cOn, from->log ) ;
      l4add( &to->ne, temp ) ;
   }

   return 0 ;
}

/* flags a range of bits for the given F4FLAG and map */
static long bitmap4flagRange( F4FLAG *flags, BITMAP4 *map, CONST4 *startCon, CONST4 *endCon, long doFlip, char startVal, char endVal, long check )
{
   long rc ;
   double pos1 = 0.0 ;
   double pos2 = 0.0 ;
   TAG4FILE *tag ;

   #ifdef S4MDX
      int isDesc ;
      long doSkip ;
   #endif

   tag = map->tag ;
   #ifdef S4MDX
      isDesc = ( tag->header.typeCode & 8 ) ? 1 : 0 ;
   #endif
   unsigned long start = 0L ;
   unsigned long end = 0L ;

   /*
       Basically what we want to do here is flag all the records between
       the input range of 'startCon' and 'endCon'.

       Note that the startCon is actually the upper value in the range
       and endCon is the lower value in the range.  For example, if
       the seek expression was 2 <= x <= 4, the startCon would be '4'
       and the endCon would be '2'.

       The basic algorithm is:
         We seek for the startCon (end value) and record the record number
         Then we seek for the endCon (start value)
         Then we loop on:
           - mark the current record
           - skip tag by 1
           - if we have reached the recorded record number, stop loop

       The only compications are the cases where the range is exclusive
       ( eg. 4 <= x <= 2  - x can never match).  To handle this case
       we record the index positions when the 2 seeks are performed.
       if the startCon position (end value) is less than the endCon
       position (start value) then clearly we have no match.

       First do a basic seek for the start of the range:
          startCon is the value to seek for
          startVal is either '0' or '1' and indicates the following:
             '0' means we should skip backwards 1 record after finding
             the value because we are looking for a 'less than'
             condition (i.e. the matching record should not be included)
             '1' means

   */

   if ( startCon != 0 )
   {
      // bitmap4seek returns ULONG_MAX (or -1) if at end of tag - i.e. not found
      start = bitmap4seek( map, startCon, startVal, 0L, 0 ) ;
      if ( start == ULONG_MAX || tfile4eof( map->tag ) )
         return -2L ;
      if ( start != ULONG_MAX && start != 0 )
         pos1 = tfile4position( tag ) ;
   }

   if ( endCon != 0 )
   {
      // bitmap4seek returns ULONG_MAX (or -1) if at end of tag - i.e. not found
      end = bitmap4seek( map, endCon, endVal, start, (int)check ) ;
      if ( end == ULONG_MAX )
         return -2L ;
      if ( end != ULONG_MAX && end != 0 )
         pos2 = tfile4position( tag ) ;
   }

   #ifdef S4MDX
      if ( isDesc )
      {
         if ( start != ULONG_MAX && start != 0  )
         {
            if ( end != ULONG_MAX && end != 0  )
            {
               if ( pos1 < pos2 )   /* wrap around case, no matches */
                  return doFlip ;
               if ( doFlip == 0 && ( pos1 - pos2 ) > 0.5 )   /* go around */
               {
                  rc = doFlip = -1L ;
                  for(;;)
                  {
                     if ( tfile4skip( tag, -1L ) != -1L )
                        break ;
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                  }
                  start = bitmap4seek( map, startCon, startVal, 0, 0 ) ;
                  for(;;)
                  {
                     if ( tfile4skip( tag, 1L ) != 1L )
                        break ;
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                  }
               }
               else
               {
                  if ( doFlip == 0 )
                     doFlip = 1 ;
                  rc = doFlip ;
                  for( ; rc == doFlip ; )
                  {
                     if ( doFlip == -1 )
                        f4flagReset( flags, tfile4recNo( tag ) ) ;
                     else
                        f4flagSet( flags, tfile4recNo( tag ) ) ;
                     if ( tfile4recNo( tag ) == start )
                        break ;
                     rc = tfile4skip( tag, 1L ) == 1L ? doFlip : -doFlip ;
                  }
               }
            }
            else
            {
               if ( doFlip == 0 )
               {
                  if ( pos1 > 0.5 )
                  {
                     doSkip = 1L ;
                     doFlip = -1L ;
                     rc = tfile4skip( tag, 1L ) ;
                  }
                  else
                  {
                     rc = doSkip = -1L ;
                     doFlip = 1L ;
                  }
               }
               else
                  rc = doSkip = -doFlip ;

              for( ; rc == doSkip ; )
               {
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
            }
         }
         else
         {
            if ( end != ULONG_MAX && end != 0  )
            {
               if ( doFlip == 0 )
               {
                  if ( pos2 > .5 )
                  {
                     rc = doSkip = 1L ;
                     doFlip = 1L ;
                  }
                  else
                  {
                     doFlip = -1L ;
                     doSkip = -1L ;
                     rc = tfile4skip( tag, doSkip ) ;
                  }
               }
               else
                  rc = doSkip = -doFlip ;

               for( ; rc == doSkip ; )
               {
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
           }
            else
               return -1L ;
         }
      }
      else
      {
   #endif
         if ( start != ULONG_MAX && start != 0  )
         {
            if ( end != ULONG_MAX && end != 0  )
            {
               if ( pos1 > pos2 )   /* wrap around case, no matches */
                  return doFlip ;
               if ( doFlip == 0 && ( pos2 - pos1 ) > 0.5 )   /* go around */
               {
                  rc = doFlip = -1L ;
                  for(;;)
                  {
                     if ( tfile4skip( tag, 1L ) != 1L )
                        break ;
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                  }
                  bitmap4seek( map, startCon, startVal, 0L, 0 ) ;
                  for(;;)
                  {
                     if ( tfile4skip( tag, -1L ) != -1L )
                        break ;
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                  }
               }
               else
               {
                  if ( doFlip == 0 )
                     doFlip = 1 ;
                  rc = doFlip ;
                  for( ; rc == doFlip ; )
                  {
                     if ( doFlip == -1 )
                        f4flagReset( flags, tfile4recNo( tag ) ) ;
                     else
                        f4flagSet( flags, tfile4recNo( tag ) ) ;
                     if ( tfile4recNo( tag ) == start )
                        break ;
                     rc = tfile4skip( tag, -1L ) == -1L ? doFlip : -doFlip ;
                  }
               }
            }
            else
            {
               if ( doFlip == 0 )
               {
                  if ( pos1 < 0.5 )
                  {
                     doFlip = -1L ;
                     rc = tfile4skip( tag, doFlip ) ;
                  }
                  else
                     rc = doFlip = 1L ;
               }
               else
                  rc = doFlip ;
               for( ; rc == doFlip ; )
               {
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doFlip ) ;
               }
            }
         }
         else
         {
            if ( end != ULONG_MAX && end != 0  )
            {
               if ( doFlip == 0 )
               {
                  if ( pos2 < .5 )
                     rc = doFlip = -1L ;
                  else
                  {
                     doFlip = 1L ;
                     rc = tfile4skip( tag, doFlip ) ;
                  }
               }
               else
                  rc = doFlip ;
               for( ; rc == doFlip ; )
               {
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doFlip ) ;
               }
               doFlip = ( doFlip == -1L ) ? 1L : -1L ;
            }
            else
               return -1L ;
         }
   #ifdef S4MDX
      }
   #endif

   return doFlip ;
}



static long bmf4AndEq( BITMAP4 *map, F4FLAG *flags, long doFlip )
{
   /*

      This function takes the input flags (i.e. bitmap) and "&'s" it with
      all the record numbers whose records match the 'equal' constant
      given by the map paramater.

      We do this by setting:
         start range = equal constant
         end range = equal constant
         call flagRange() function for all data values which lie between
           or are equal to these 2 values:
           startVal set to 1 to indicate seek from top and include the
             equality value found
           endVal set to 2 to indicate seek from bottom and include the
             equality value found

   */
   CONST4 *startCon, *endCon ;
   char startVal, endVal ;

   #ifdef E4ANALYZE
      if ( map->gt.len || map->ge.len || map->le.len || map->lt.len || l4first( &map->ne ) != 0 )
         return error4( map->log->codeBase, e4info, E83705 ) ;
   #endif
   startCon = endCon = &map->eq ;
   startVal = 1 ;
   endVal = 2 ;
   return bitmap4flagRange( flags, map, startCon, endCon, doFlip, startVal, endVal, 0L ) ;
}



static long bmf4AndOt( BITMAP4 *map, F4FLAG *flags, long doFlip )
{
   CONST4 *startCon, *endCon, *cOn ;
   char startVal, endVal ;
   long prevFlip ;
   int doCheck ;

   startVal = endVal = 0 ;
   startCon = endCon = 0 ;

   if ( map->ge.len || map->le.len )  /* AS 08/27/98 t4les1.c shouldn't in either case */
      doCheck = 0 ;
   else
      doCheck = 1 ;

   if ( map->gt.len )
   {
      startCon = &map->gt ;
      startVal = 3 ;
   }
   else
      if ( map->ge.len )
      {
         startCon = &map->ge ;
         startVal = 1 ;
      }

   if ( map->lt.len )
   {
      endCon = &map->lt ;
      endVal = 0 ;
   }
   else
      if ( map->le.len )
      {
         endCon = &map->le ;
         endVal = 2 ;
      }

   /* AS 10/06/97 - check last paramater to 0L, having problems geting 1 record between ranges otherwise working.  test.c - added as t3les1.c */
   /* AS 08/20/98 - updated, only occurs if both ge and le, so made conditional on that */
   doFlip = bitmap4flagRange( flags, map, startCon, endCon, doFlip, startVal, endVal, doCheck ) ;
   if ( doFlip == -2L )  /* no matches */
   {
      f4flagSetAll( flags ) ;
      flags->isFlip = 1 ;
      return -999L ;
   }

   cOn = (CONST4 *)l4first( &map->ne ) ;
   if( cOn != 0 )
   {
      if ( doFlip != 0L )
         prevFlip = doFlip = ( doFlip == 1L ) ? -1L : 1L ;
      else
         prevFlip = 0L ;
      while ( cOn != 0 )
      {
         startCon = endCon = cOn ;
         startVal = 1 ;
         endVal = 2 ;
         doFlip = bitmap4flagRange( flags, map, startCon, endCon, doFlip, startVal, endVal, 0L ) ;
         if ( doFlip == -2L )
         {
            if ( prevFlip == 0L )  /* all records not equal, so mark as such */
            {
               f4flagSetAll( flags ) ;
               doFlip = -1L ;
            }
            else   /* otherwise no change since no un-equal records */
               doFlip = prevFlip ;
         }
         cOn = (CONST4 *)l4next( &map->ne, cOn ) ;
         prevFlip = doFlip ;
      }
      doFlip = ( doFlip == 1L ) ? -1L : 1L ;
   }

   return doFlip ;
}



static int bmf4OrNe( BITMAP4 *map, F4FLAG *flags, long doFlip )
{
   CONST4 *cOn = (CONST4 *)l4first( &map->ne ) ;
   CONST4 *startCon = cOn ;
   CONST4 *endCon = cOn ;
   char startVal = 1 ;
   char endVal = 2 ;

   doFlip = bitmap4flagRange( flags, map, startCon, endCon, doFlip, startVal, endVal, 0L ) ;

   if ( doFlip != -1 )
      f4flagFlipReturns( flags ) ;

   #ifdef E4ANALYZE
      cOn = (CONST4 *)l4next( &map->ne, cOn ) ;
      if ( cOn != 0 )
      {
         error4( map->log->codeBase, e4info, E83705 ) ;
         return -1000 ;
      }
   #endif
   return -999 ;
}



static long bmf4OrOt( BITMAP4 *map, F4FLAG *flags, CODE4 *codeBase )
{
   CONST4 *startCon, *endCon ;
   char startVal, endVal ;
   long doFlip, rc, prevFlip, doSkip ;
   unsigned long start, end ;
   double pos1 = 0 ;
   double pos2 ;
   #ifdef S4MDX
      int isDesc ;
   #endif

   TAG4FILE *tag = map->tag ;
   #ifdef S4MDX
      isDesc = ( tag->header.typeCode & 8 ) ? 1 : 0 ;
   #endif

   #ifdef E4PARM_LOW
      if ( tag == 0 )
      {
         error4( codeBase, e4parm, E93719 ) ;
         return -1000 ;
      }
   #endif

   start = end = doFlip = doSkip = 0L ;
   if ( map->gt.len )
      start = bitmap4seek( map, &map->gt, 3, 0L, 0 ) ;
   else
      if ( map->ge.len )
         start = bitmap4seek( map, &map->ge, 1, 0L, 0 ) ;

   #ifdef S4MDX
      if ( start != ULONG_MAX && start != 0  )
         pos1 = (double)tfile4position( tag ) ;
   #endif

   if ( map->lt.len )
      end = bitmap4seek( map, &map->lt, 0, start, 1 ) ;
   else
      if ( map->le.len )
        end = bitmap4seek( map, &map->le, 2, start, 1 ) ;

   if ( end == ULONG_MAX )  /* no matches */
      return -999L ;

   #ifdef S4MDX
      if ( isDesc )
      {
         if ( start != ULONG_MAX && start != 0  )
         {
            if ( end != ULONG_MAX && end != 0  )
            {
               pos2 = (double)tfile4position( tag ) ;
               if ( ( pos1 - pos2 ) < 0.5 )   /* do between */
               {
                  if ( ( pos1 - pos2 ) < 0 )
                  {
                     doFlip = -1 ;
                     doSkip = -1 ;
                  }
                  else
                     doSkip = doFlip = 1L ;
                  if ( tfile4recNo( tag ) != start )
                  {
                     rc = tfile4skip( tag, doSkip ) ;
                     for( ; rc == doSkip ; )
                     {
                        if ( error4code( codeBase ) < 0 )
                           return -1000L ;
                        if ( tfile4recNo( tag ) == start )
                           break ;
                        f4flagSet( flags, tfile4recNo( tag ) ) ;
                        rc = tfile4skip( tag, doSkip ) ;
                     }
                  }
               }
               else  /* flip and do both ways */
               {
                  for(;;)
                  {
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                     if ( error4code( codeBase ) < 0 )
                        return -1000L ;
                     if ( tfile4skip( tag, -1L ) != -1L )
                        break ;
                  }
                  if ( map->gt.len )
                     start = bitmap4seek( map, &map->gt, 1, 0, 0 ) ;
                  else
                     if ( map->ge.len )
                        start = bitmap4seek( map, &map->ge, 2, 0, 0 ) ;
                  for(;;)
                  {
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                     if ( error4code( codeBase ) < 0 )
                        return -1000L ;
                     if ( tfile4skip( tag, 1L ) != 1L )
                        break ;
                  }
                  doSkip = doFlip = -1L ;
               }
            }
            else
            {
               if ( pos1 > 0.5 )
               {
                  doFlip = -1 ;
                  doSkip = 1 ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
               else
               {
                  doFlip = 1 ;
                  doSkip = rc = -1 ;
               }

               for( ; rc == doSkip ; )
               {
                  if ( error4code( codeBase ) < 0 )
                     return -1000 ;
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
            }
         }
         else
         {
            if ( end != ULONG_MAX && end != 0  )
            {
               if ( (double)tfile4position( tag ) > 0.5 )
               {
                  doFlip = 1 ;
                  doSkip = 1 ;
                  rc = 1 ;
               }
               else
               {
                  doFlip = -1 ;
                  doSkip = -1 ;
                  rc = tfile4skip( tag, doSkip ) ;
               }

               for( ; rc == doSkip ; )
               {
                  if ( error4code( codeBase ) < 0 )
                     return -1000 ;
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
            }
         }

      }
      else
      {
   #endif
         if ( start != ULONG_MAX && start != 0  )
         {
            pos1 = (double)tfile4position( tag ) ;
            if ( end != ULONG_MAX && end != 0  )
            {
               pos2 = (double)tfile4position( tag ) ;
               if ( ( pos2 - pos1 ) < 0.5 )   /* flip and do between */
               {
                  doSkip = doFlip = 1L ;
                  if ( tfile4recNo( tag ) != start )
                  {
                     rc = tfile4skip( tag, doSkip ) ;
                     for( ; rc == doSkip ; )
                     {
                        if ( error4code( codeBase ) < 0 )
                           return -1000L ;
                        if ( tfile4recNo( tag ) == start )
                           break ;
                        f4flagSet( flags, tfile4recNo( tag ) ) ;
                        rc = tfile4skip( tag, doSkip ) ;
                     }
                  }
                  doFlip = doSkip = -1L ;
               }
               else  /* do both ways */
               {
                  for(;;)
                  {
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                     if ( error4code( codeBase ) < 0 )
                        return -1000L ;
                     if ( tfile4skip( tag, -1L ) != -1L )
                        break ;
                  }
                  if ( map->gt.len )
                     bitmap4seek( map, &map->gt, 1, 0L, 0 ) ;
                  else
                     if ( map->ge.len )
                        bitmap4seek( map, &map->ge, 2, 0L, 0 ) ;
                  for(;;)
                  {
                     f4flagSet( flags, tfile4recNo( tag ) ) ;
                     if ( error4code( codeBase ) < 0 )
                        return -1000L ;
                     if ( tfile4skip( tag, 1L ) != 1L )
                        break ;
                  }
               }
            }
            else
            {
               if ( pos1 < 0.5 )
               {
                  doFlip = doSkip = -1 ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
               else
                  doFlip = doSkip = rc = 1 ;

               for( ; rc == doSkip ; )
               {
                  if ( error4code( codeBase ) < 0 )
                     return -1000 ;
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
            }
         }
         else
         {
            if ( end != ULONG_MAX && end != 0  )
            {
               if ( (double)tfile4position( tag ) > 0.5 )
               {
                  doSkip = 1 ;
                  doFlip = 1 ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
               else
               {
                 doFlip = -1 ;
                 rc = doSkip = -1 ;
               }

               for( ; rc == doSkip ; )
               {
                  if ( error4code( codeBase ) < 0 )
                     return -1000 ;
                  f4flagSet( flags, tfile4recNo( tag ) ) ;
                  rc = tfile4skip( tag, doSkip ) ;
               }
               doFlip = doSkip = ( doFlip == -1L ) ? 1L : -1L ;
            }
         }
   #ifdef S4MDX
      }
   #endif

   if ( map->eq.len )
   {
      startCon = endCon = &map->eq ;
      startVal = 1 ;
      endVal = 2 ;
      prevFlip = doFlip ;
      doFlip = bitmap4flagRange( flags, map, startCon, endCon, doFlip, startVal, endVal, 0L ) ;
      if ( doFlip == -2L )
         doFlip = prevFlip ;
   }

   return doFlip ;
}



static int bitmap4flagGenerate( BITMAP4 *map, int mode, F4FLAG *flags )
{
   /* generate the flags for the bitmap tree */
   CODE4 *codeBase ;
   CONST4 *cOn ;
   long doFlip ;
   unsigned long allocSize ;
   int isFlipped ;
   #ifdef S4HAS_DESCENDING
      TAG4FILE *tag ;
      int oldDesc ;
   #endif

   codeBase = map->log->codeBase ;

   #ifdef S4HAS_DESCENDING
      tag = map->tag ;

      #ifdef E4PARM_LOW
         if ( tag == 0 )
            return error4( codeBase, e4parm, E93701 ) ;
      #endif

      oldDesc = tag->header.descending ;
   #endif

   if ( flags->flags == 0 )
   {
      allocSize = (unsigned long)d4recCount( map->relate->data ) + 1L ;
      if ( f4flagInit( flags, codeBase, allocSize, 0 ) < 0 )
      {
         #ifndef S4OPTIMIZE_OFF
            #ifdef S4LOW_MEMORY
               /* note that memory failure could also occur if the value was very large */
               if ( codeBase->hasOpt && codeBase->opt.numBuffers != 0 && codeBase->opt.bufferSize > allocSize )
               {
                  error4set( codeBase, 0 ) ;
                  code4optSuspend( codeBase ) ;
                  codeBase->hadOpt = 0 ;   /* mark to automatically restart optimization later */
                  if ( codeBase->opt.numBuffers > 8 )
                     codeBase->opt.numBuffers-- ;
                  f4flagInit( flags, codeBase, allocSize, 0 ) ;
               }
            #endif
         #endif
      }

      if ( error4code( codeBase ) != 0 )
      {
         #ifdef S4HAS_DESCENDING
            tfile4descending( tag, (const unsigned short)oldDesc ) ;
         #endif
         return -1 ;
      }

      isFlipped = 0 ;
      doFlip = 0L ;
   }
   else
   {
      doFlip = ( flags->isFlip == 1L ) ? -1L : 1L ;
      isFlipped = 1 ;
   }

   if ( map->noMatch == 1 )
   {
      if ( mode == 2 )
      {
         if ( isFlipped ) /* previous setting, so make all succeed */
         {
            f4flagSetAll( flags ) ;
            flags->isFlip = 0 ;
         }
         else   /* none set now, so make all succeed by simple flip */
            flags->isFlip = 1 ;
      }
      #ifdef S4HAS_DESCENDING
         tfile4descending( tag, (const unsigned short)oldDesc ) ;
      #endif
      return 0 ;
   }

   if ( mode == 1 )  /* and */
   {
      if ( map->eq.len )
         doFlip = bmf4AndEq( map, flags, doFlip ) ;
      else
         doFlip = bmf4AndOt( map, flags, doFlip ) ;
   }
   else  /* or */
   {
      cOn = (CONST4 *)l4first( &map->ne ) ;
      if ( cOn != 0 )
         doFlip = bmf4OrNe( map, flags, doFlip ) ;
      else
         doFlip = bmf4OrOt( map, flags, codeBase ) ;
   }

   #ifdef S4HAS_DESCENDING
      tfile4descending( tag, (const unsigned short)oldDesc ) ;
   #endif

   if ( doFlip == -999L )
      return 0 ;

   if ( doFlip == -1000L )
      return -1 ;

   if ( doFlip == -1L && !isFlipped )
      f4flagFlipReturns( flags ) ;
   return 0 ;
}

/* generate the bitmaps */
static F4FLAG *bitmap4generate( BITMAP4 *map )
{
   BITMAP4 *mapOn ;
   F4FLAG *flag1, *flag2 ;

   if ( map->branch == 0 )
   {
      flag1 = (F4FLAG *)u4alloc( (long)sizeof( F4FLAG ) ) ;
      if ( flag1 == 0 )  /* insufficient memory */
         return 0 ;
      c4memset( (void *)flag1, 0, sizeof( F4FLAG ) ) ;
      if ( bitmap4flagGenerate( map, map->andOr, flag1 ) < 0 )
      {
         u4free( flag1->flags ) ;
         u4free( flag1 ) ;
         return 0 ;
      }
      return flag1 ;
   }

   flag1 = flag2 = 0 ;

   mapOn = (BITMAP4 *)l4first( &map->children ) ;
   while( mapOn != 0 )
   {
      flag2 = bitmap4generate( mapOn ) ;
      if ( flag1 == 0 )
         flag1 = flag2 ;
      else
      {
         #ifdef E4ANALYZE
            if ( map->andOr != 1 && map->andOr != 2 )
            {
               error4( map->log->codeBase, e4info, E83705 ) ;
               return 0 ;
            }
         #endif
         if ( map->andOr == 1 )
            f4flagAnd( flag1, flag2 ) ;
         else
            f4flagOr( flag1, flag2 ) ;
         u4free( flag2->flags ) ;
         u4free( flag2 ) ;
      }
      mapOn = (BITMAP4 *)l4next( &map->children, mapOn ) ;
   }

   return flag1 ;
}

/* evaluate bitmaps out of the log */
int bitmap4evaluate( L4LOGICAL *log, const int pos )
{
   BITMAP4 *map ;
   F4FLAG *flags ;

   if ( error4code( log->codeBase ) < 0 )
      return error4code( log->codeBase ) ;

   map = bitmap4init( log, pos ) ;
   if ( map == 0 )
      return 0 ;

   map = bitmap4reduce( 0, map ) ;  // first reduce the map by eliminating unneeded branches.
   map = bitmap4redistribute( 0, map, 1 ) ;
   if ( error4code( log->codeBase ) < 0 )
      return error4code( log->codeBase ) ;
   if ( map == 0 )
      return 0 ;

   map = bitmap4redistributeBranch( 0, map ) ;
   if ( map == 0 )
   {
      if ( error4code( log->codeBase ) == e4memory )
         error4set( log->codeBase, 0 ) ;
      return 0 ;
   }

   flags = bitmap4generate( map ) ;
   if ( flags != 0 )
   {
      c4memcpy( (void *)&map->relate->set, (void *)flags, sizeof( F4FLAG ) ) ;
      u4free( flags ) ;
   }
   bitmap4free( log, map ) ;
   return 0 ;
}

#endif   /* S4INDEX_OFF */
/* returns true/false as to whether or not relate could be bitmap optimized */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION relate4optimizeable( RELATE4 *relate )
{
   #ifdef S4INDEX_OFF
      return 0 ;
   #else
      L4LOGICAL *log ;
      BITMAP4 *map ;
      RELATION4 *relation ;

      relation = relate->relation ;

      if ( relation->exprSource )
      {
         relate4changed( relate ) ;

         relation->log.expr = expr4parseLow( relate->data, relation->exprSource, 0 ) ;
         if ( relation->log.expr == 0 )
            return -1 ;

         log = &relation->log ;

         if ( error4code( log->codeBase ) < 0 )
            return -1 ;

         log4buildDatabaseLists( log ) ;
         map = bitmap4init( log, log->expr->infoN - 1 ) ;
         if ( map == 0 )
         {
            relate4changed( relate ) ;
            return 0 ;
         }
         bitmap4free( log, map ) ;

         relate4changed( relate ) ;
         return 1 ;
      }

      return 0 ;
   #endif
}
#else
int S4FUNCTION relate4optimizeable( RELATE4 *relate )
{
   CONNECTION4 *connection ;
   CONNECTION4RELATE_OPT_INFO_IN *info ;
   int rc ;
   CODE4 *c4 ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94426 ) ;
   #endif

   c4 = relate->codeBase ;

   #ifdef E4ANALYZE
      if ( relate->data == 0 )
         return error4( c4, e4struct, E94426 ) ;
      if ( relate->data->dataFile == 0 )
         return error4( c4, e4struct, E94426 ) ;
   #endif

   if ( relate->relation->isInitialized == 0 || relate->dataTag != relate->data->tagSelected )
   {
      relate->dataTag = relate->data->tagSelected ;
      rc = relate4clientInit( relate ) ;
      if ( rc != 0 )
         return rc ;
   }

   connection = relate->data->dataFile->connection ;
   #ifdef E4ANALYZE
      if ( connection == 0 )
         return error4( c4, e4struct, E94426 ) ;
   #endif
   connection4assign( connection, CON4RELATE_OPT, 0, 0 ) ;
   connection4addData( connection, NULL, sizeof( CONNECTION4RELATE_OPT_INFO_IN ), (void **)&info ) ;
   info->relationId = htonl5(relate->relation->relationId) ;
   info->useGeneralTagsInRelate = htons5( c4->useGeneralTagsInRelate ) ;
   connection4sendMessage( connection ) ;
   rc = connection4receiveMessage( connection ) ;
   if ( rc < 0 )
      return error4stack( c4, rc, E94426 ) ;
   rc = connection4status( connection ) ;
   if ( rc < 0 )
      return connection4error( connection, c4, rc, E94426 ) ;

   return rc ;
}
#endif   /* S4CLIENT */
