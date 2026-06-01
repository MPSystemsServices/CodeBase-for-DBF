/* c4const.c  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4CLIENT
#ifndef S4INDEX_OFF

/* This function creates a branch out of the input constant, and combines it with the input map */
static int bitmap4constantCombine( BITMAP4 *parent, BITMAP4 *oldAndMap, CONST4 *con, int conType )
{
   BITMAP4 *tempLeaf, *andMap, *newBranch ;
   CONST4 *temp ;

   if ( con->len == 0 || error4code( parent->log->codeBase ) == e4memory )
      return 0 ;

   newBranch = bitmap4create( parent->log, parent->relate, 1, 1 ) ;
   if ( newBranch == 0 )
      return 0 ;

   andMap = bitmap4create( parent->log, parent->relate, 0, 0 ) ;
   if ( andMap == 0 )
      return 0 ;
   bitmap4copy( andMap, oldAndMap ) ;
   l4add( &newBranch->children, andMap ) ;

   tempLeaf = bitmap4create( parent->log, parent->relate, 1, 0 ) ;
   if ( tempLeaf == 0 )
      return 0 ;
   tempLeaf->type = andMap->type ;
   tempLeaf->tag = andMap->tag ;
   l4add( &newBranch->children, tempLeaf ) ;

   switch( conType )
   {
      case 1:
         c4memcpy( (void *)&tempLeaf->lt, (void *)con, sizeof( CONST4 ) ) ;
         break ;
      case 2:
         c4memcpy( (void *)&tempLeaf->le, (void *)con, sizeof( CONST4 ) ) ;
         break ;
      case 3:
         c4memcpy( (void *)&tempLeaf->gt, (void *)con, sizeof( CONST4 ) ) ;
         break ;
      case 4:
         c4memcpy( (void *)&tempLeaf->ge, (void *)con, sizeof( CONST4 ) ) ;
         break ;
      case 5:
         c4memcpy( (void *)&tempLeaf->eq, (void *)con, sizeof( CONST4 ) ) ;
         break ;
      case 6:
         temp = (CONST4 *)u4alloc( (long)sizeof( CONST4 ) ) ;
         if ( temp == 0 )
            return 0 ;
         c4memcpy( (void *)temp, (void *)con, sizeof( CONST4 ) ) ;
         l4add( &tempLeaf->ne, temp ) ;
         break ;
      default:
         return error4( parent->log->codeBase, e4info, E93701 ) ;
   }
   c4memset( (void *)con, 0 ,sizeof( CONST4 ) ) ;
   newBranch = bitmap4redistribute( 0, newBranch, 0 ) ;

   if ( error4code( parent->log->codeBase ) < 0 )
      return error4code( parent->log->codeBase ) ;

   if ( newBranch->children.nLink == 0 )
      bitmap4destroy( newBranch ) ;
   else
      l4add( &parent->children, newBranch ) ;

   return 0 ;
}



static void bitmap4redistributeChildren( BITMAP4 *map )
{
   for( BITMAP4 *childOn = (BITMAP4 *)l4first( &map->children ) ;; )
   {
      if ( childOn == 0 )
         break ;
      childOn = bitmap4redistribute( map, childOn, 0 ) ;
      childOn = (BITMAP4 *)l4next( &map->children, childOn ) ;
   }
}



static BITMAP4 *bitmap4redistributeShrink( BITMAP4 *parent, BITMAP4 *map )
{
   if ( map->children.nLink == 1 )   /* just a child, so remove myself */
   {
      BITMAP4 *childMap = (BITMAP4 *)l4first( &map->children ) ;
      l4remove( &map->children, childMap ) ;
      if ( parent != 0 )
      {
         #ifdef E4ANALYZE
            if ( childMap->tag == 0 && childMap->children.nLink == 0 )
            {
               error4( childMap->log->codeBase, e4info, E93701 ) ;
               return 0 ;
            }
         #endif
         if ( parent->tag == 0 && childMap->tag != 0 )
            parent->tag = childMap->tag ;
         l4addAfter( &parent->children, map, childMap ) ;
         l4remove( &parent->children, map ) ;
      }
      bitmap4destroy( map ) ;
      map = childMap ;
   }

   return map ;
}



static int bitmap4redistributeSplit( BITMAP4 *map, BITMAP4 **parent, BITMAP4 **parent2map, BITMAP4 *rightChild )
{
   // returns 1 if we are to continue, or 0 if we cannot continue (i.e. error) (return 0 to caller)

   /*
      go from:    (parent)         to            (parent)
                ...(map)...                ...(map)    (parent2map)...
           ...(child1) (child2)...    ...(child1)...     (child2)
   */
   if ( (*parent2map) == 0 )
   {
      (*parent2map) = bitmap4create( map->log, map->relate, map->andOr, 1 ) ;
      if ( (*parent2map) == 0 )  /* must handle by freeing... */
         return 0 ;
      if ( (*parent) == 0 )
      {
         (*parent) = bitmap4create( map->log, map->relate, map->andOr, 1 ) ;
         if ( (*parent) == 0 )  /* must handle by freeing... */
            return 0 ;
         l4add( &(*parent)->children, map ) ;
      }
      l4add( &(*parent)->children, (*parent2map) ) ;
   }
   l4remove( &map->children, rightChild ) ;
   l4add( &(*parent2map)->children, rightChild ) ;

   return 1 ;
}



static int bitmap4redistributeCombine( BITMAP4 *map, BITMAP4 **parent, BITMAP4 **parent2map )
{
   // returns 1 if we are to continue, or 0 if we cannot continue (i.e. error) (return 0 to caller)
   BITMAP4 *childMap ;
   /* going in we have:

          (parent)
       ... (map) ...

   */
   for( childMap = (BITMAP4 *)l4first( &map->children ) ; childMap != 0 ; )
   {
      BITMAP4 *rightChild = (BITMAP4 *)l4next( &map->children, childMap ) ;
      if ( rightChild == 0 )
         break ;

      char split = 0 ;
      if ( rightChild->tag != childMap->tag || rightChild->andOr != childMap->andOr )
      {
         split = 1 ;
      }
      else
      {
         assert5( map != 0 ) ;  // should never be case, don't need if...
         assert5( rightChild != 0 ) ;  // should never be case, don't need if...

         if ( map->andOr != rightChild->andOr )
            split = 1 ;
      }

      // AS Aug 9/04 - special case where we have one map with eq and the other with ne values and the lengths do not match (partial seeks)
      // in that case do not combine the leafs because that instance is very complicated.
      if ( split == 0 )
      {
         BITMAP4 *eqMap = 0, *neMap = 0 ;
         if ( childMap->eq.len && rightChild->ne.nLink != 0 )
         {
            eqMap = childMap ;
            neMap = rightChild ;
         }
         else if ( rightChild->eq.len && childMap->ne.nLink != 0 )
         {
            eqMap = rightChild ;
            neMap = childMap ;
         }
         if ( eqMap && neMap )
         {
            CONST4 *cNext, *cOn = (CONST4 *)l4first( &neMap->ne ) ;
            while ( cOn != 0 )
            {
               cNext = (CONST4 *)l4next( &neMap->ne, cOn ) ;
               if ( cOn->len != eqMap->eq.len )
               {
                  short smallestLen = min( cOn->len, eqMap->eq.len ) ;
                  if ( c4memcmp( const4return( neMap->log, &eqMap->eq ), const4return( neMap->log, cOn ), smallestLen ) == 0 )
                  {
                     split = 1 ;
                     break ;
                  }
               }
               cOn = cNext ;
            }
         }
      }

      if ( split == 1 )
      {
         if ( bitmap4redistributeSplit( map, parent, parent2map, rightChild ) == 0 )
            return 0 ;
      }
      else
      {
         /*
            go from:    (parent)           to           (parent)
                      ...(map)...                     ...(map)...
                ...(childMap) (rightChild)...        ...(childMap-rightChild-combined)...
         */
         // AS Dec 30/03 ensure they are both leafs...
         if ( childMap->branch == 1 || rightChild->branch == 1 )  // just skip to next child
            childMap = rightChild ;
         else
         {
            childMap = bitmap4combineLeafs( map, childMap, rightChild ) ;
            if ( error4code( map->log->codeBase ) < 0 )
               return 0 ;
         }
      }
   }

   if ( (*parent2map) != 0 )   // means we inserted a new parent.  Redistribute this map...
   {
      #ifdef E4ANALYZE
         if ( (*parent) == 0 )
         {
            error4( map->log->codeBase, e4info, E93701 ) ;
            return 0 ;
         }
      #endif
      /* AS 02/22/99 --> parent2map may change, so get it here... (i.e. if shrunk out)*/
      *parent2map = bitmap4redistribute( (*parent), (*parent2map), 1 ) ;
   }

   return 1 ;
}



BITMAP4 *bitmap4reduce( BITMAP4 *parent, BITMAP4 *map )
{
  // reduce the map by eliminating unneeded branches.
  // basically just go through the tree getting rid of branches with only 1 child.
  // this is done before the distribution because the original expression may have had more
  // levels (eg. more braces) than were actually needed.

  // we reduce all our children, then see if we can be reduced...

   for( BITMAP4 *childOn = (BITMAP4 *)l4first( &map->children ) ;; )
   {
      if ( childOn == 0 )
         break ;
      childOn = bitmap4reduce( map, childOn ) ;
      childOn = (BITMAP4 *)l4next( &map->children, childOn ) ;
   }

   // now reduce ourselves if possible...  -- return result because we may be reduced out
   return bitmap4redistributeShrink( parent, map ) ;
}



BITMAP4 *bitmap4redistribute( BITMAP4 *parent, BITMAP4 *map, const char doShrink )
{
   /* this function redistributes (splits and combines) and/and, or/or block sequences
      Basically we want to try to create easier bitmaps for calculations.  For eg, if a bitmap
      is (val < 10 && val > 5 ), we want to create a simple bitmap between 5 and 10, instead
      of having 2 different bitmaps.  This is important because it reduces computation time
      and memory requirements of having multiple maps.
   */

   // this function returns a new parent as its return if there is no input parent...
   int parentWasZero = (parent == 0) ? 1 : 0 ;

   if ( map->branch == 0 )  // can't redistribute leaf maps
      return map ;

   /* first redistribute all the children of this map */
   #ifdef RELATE4PRINT
      static inPos = 0 ;
      #ifdef S4CONSOLE
         short lineLoop ;
         for ( lineLoop = 0 ; lineLoop < inPos ; lineLoop++ )
            printf( "   " ) ;
         printf( "bitmap4redistribute 1:\n" ) ;
      #endif
      bitmap4print( map, inPos ) ;
      inPos++ ;
   #endif
   bitmap4redistributeChildren( map ) ;
   #ifdef RELATE4PRINT
      #ifdef S4CONSOLE
         for ( lineLoop = 0 ; lineLoop < inPos ; lineLoop++ )
            printf( "   " ) ;
         printf( "bitmap4redistribute 2:\n" ) ;
      #endif
      bitmap4print( map, inPos ) ;
   #endif

   if ( parent != 0 )
      if ( parent->andOr != map->andOr )  /* case where no combos possible */
         return map ;

   /* now combine all leaf children where possible */
   BITMAP4 *parent2map = 0 ;
   if ( bitmap4redistributeCombine( map, &parent, &parent2map ) == 0 )
      return 0 ;

   #ifdef RELATE4PRINT
      #ifdef S4CONSOLE
         for ( lineLoop = 0 ; lineLoop < inPos ; lineLoop++ )
            printf( "   " ) ;
         printf( "bitmap4redistribute 3:\n" ) ;
      #endif
      bitmap4print( map, inPos ) ;
   #endif

   if ( doShrink )
   {
      map = bitmap4redistributeShrink( parent, map ) ;
      #ifdef RELATE4PRINT
         #ifdef S4CONSOLE
            for ( lineLoop = 0 ; lineLoop < inPos ; lineLoop++ )
               printf( "   " ) ;
            printf( "bitmap4redistribute 4:\n" ) ;
         #endif
         bitmap4print( map, 0 ) ;
      #endif
   }

   #ifdef RELATE4PRINT
      inPos-- ;
   #endif

   if ( parent2map != 0 && parent != 0 )
   {
      if ( parentWasZero == 1 )
         return parent ;
      else
         return parent2map ;
   }
   return map ;
}




/* this function redistributes the input maps by breaking the one up into constants and creating maps for each */
BITMAP4 *bitmap4redistributeLeaf( BITMAP4 *parent, BITMAP4 *map1, BITMAP4 *map2 )
{
   BITMAP4 *newBranch, *orMap, *place, *andMap, *temp ;
   CONST4 *cOn ;

   newBranch = bitmap4create( parent->log, parent->relate, 1, 1 ) ;
   if ( newBranch == 0 )
      return 0 ;

   place = bitmap4create( parent->log, parent->relate, 0, 0 ) ;
   if ( place == 0 )
      return 0 ;
   l4addAfter( &parent->children, map1, place ) ;

   l4remove( &parent->children, map1 ) ;
   l4remove( &parent->children, map2 ) ;

   if ( map1->andOr == 1 )
   {
      andMap = map1 ;
      orMap = map2 ;
   }
   else
   {
      andMap = map2 ;
      orMap = map1 ;
   }

   bitmap4constantCombine( newBranch, andMap, &orMap->lt, 1 ) ;
   bitmap4constantCombine( newBranch, andMap, &orMap->le, 2 ) ;
   bitmap4constantCombine( newBranch, andMap, &orMap->gt, 3 ) ;
   bitmap4constantCombine( newBranch, andMap, &orMap->ge, 4 ) ;
   bitmap4constantCombine( newBranch, andMap, &orMap->eq, 5 ) ;
   for( ;; )
   {
      cOn = (CONST4 *)l4first( &orMap->ne ) ;
      if ( cOn == 0 )
         break ;
      bitmap4constantCombine( newBranch, andMap, cOn, 6 ) ;
   }

   if ( error4code( parent->log->codeBase ) == e4memory )
      return 0 ;

   if ( newBranch->children.nLink == 0 )   /* collapsed */
   {
      if ( parent->tag == 0 && andMap->tag != 0 )
         parent->tag = andMap->tag ;
      bitmap4destroy( newBranch ) ;
      newBranch = 0 ;
   }
   else
   {
      while( newBranch->branch == 1 && newBranch->children.nLink == 1 )
      {
         temp = (BITMAP4 *)l4first( &newBranch->children ) ;
         bitmap4destroy( newBranch ) ;
         newBranch = temp ;
      }
      l4addAfter( &parent->children, place, newBranch ) ;
   }

   l4remove( &parent->children, place ) ;
   bitmap4destroy( place ) ;
   bitmap4destroy( orMap ) ;
   bitmap4destroy( andMap ) ;

   return newBranch ;
}

/* this function splits and combines and/or, or/and block sequences */
/* all bitmaps must be in standard bitmap4redistribute format prior to call */
BITMAP4 * bitmap4redistributeBranch( BITMAP4 *parent, BITMAP4 *map )
{
   BITMAP4 *childOn2, *childOn, *childNext2 ;

   if ( map->branch == 0 )
      return map ;

   childOn = (BITMAP4 *)l4first( &map->children ) ;

   for( ;; )
   {
      if ( childOn == 0 )
         break ;
      if ( childOn->branch )
      {
         childOn = bitmap4redistributeBranch( map, childOn ) ;
         if ( childOn == 0 && error4code( parent->log->codeBase ) == e4memory )
            return 0 ;
      }
      if ( childOn->branch == 0 )
      {
         childOn2 = (BITMAP4 *)l4next( &map->children, childOn ) ;
         while( childOn2 != 0 )
         {
            if ( childOn2->branch )
            {
               childOn2 = bitmap4redistributeBranch( map, childOn2 ) ;
               if ( childOn2 == 0 && error4code( parent->log->codeBase ) == e4memory )
                  return 0 ;
            }
            childNext2 = (BITMAP4 *)l4next( &map->children, childOn2 ) ;
            if ( childOn->branch == 0 && map->andOr == 1 && childOn->tag == childOn2->tag &&  childOn->andOr != childOn2->andOr )
            {
               childOn = bitmap4redistributeLeaf( map, childOn, childOn2 ) ;
               if ( childOn == 0 && error4code( parent->log->codeBase ) == e4memory )
                  return 0 ;
            }
            childOn2 = childNext2 ;
         }
      }
      childOn = (BITMAP4 *)l4next( &map->children, childOn ) ;
   }

   if ( map->branch == 1 )
   {
      if ( map->children.nLink == 0 )   /* mark ourselves as a leaf with no match */
      {
         map->branch = 0 ;
         map->noMatch = 1 ;
      }
      else
         if ( map->children.nLink == 1 )   /* just a child, so remove myself */
         {
            childOn = (BITMAP4 *)l4first( &map->children ) ;
            l4remove( &map->children, childOn ) ;
            if ( parent != 0 )
            {
               l4addAfter( &parent->children, map, childOn ) ;
               l4remove( &parent->children, map ) ;
            }
            bitmap4destroy( map ) ;
            map = childOn ;
         }
   }

   return map ;
}

/* location = 0 if seek_before, 1 if seek 1st, 2 if seek last, 3 if seek_after,  */
/* add 10 if it is to be an approximate seek ?? */
/* returns record number - ULONG_MAX returned if error */
#ifdef S4HAS_DESCENDING
unsigned long bitmap4seek( BITMAP4 *map, const CONST4 *con, const char location, const unsigned long check, const int doCheck )
{
   int len, rc ;
   TAG4FILE *tag ;
   char *result ;
   #ifdef S4CLIPPER
      char holdResult[20] ;  /* enough space for a numerical key */
   #endif

   // #ifdef S4VFP_KEY
   //    char buf[I4MAX_KEY_SIZE] ;
   // #endif

   tag = map->tag ;
   result = (char *)const4return( map->log, con ) ;

   /* must convert to a proper key */
   if ( map->type != r4str )
   {
      #ifdef S4CLIPPER
         c4memcpy( holdResult, result, con->len ) ;
         result = holdResult ;
      #endif
      #ifdef E4ANALYZE
         if ( expr4len( tag->expr ) == -1 )
         {
            error4( map->log->codeBase, e4info, E83701 ) ;
            return ULONG_MAX ;
         }
      #endif
      len = expr4keyConvert( tag->expr, (char **)&result, con->len, map->type, tag ) ;
   }
   else
   {
      #ifdef S4FOX
         len = expr4keyConvert( tag->expr, (char **)&result, con->len, map->type, tag ) ;
         /* AS 07/27/99 -> support for generic collating for Unicode and character fields...
           #ifdef S4VFP_KEY
            if ( tfile4vfpKey( tag ) )
            {
               if ( len * 2 > sizeof( buf ) )
                  return error4( map->log->codeBase, e4info, E82102 ) ;
               len = t4strToVFPKey( buf, result, len, len * 2, &tag->vfpInfo ) ;
               if ( len < 0 )
                  return error4( map->log->codeBase, e4info, E85404 ) ;
               result = buf ;
            }
          #endif
         */
      #else
         len = con->len ;
      #endif
   }

   if ( location > 1 )
      tfile4descending( tag, 1 ) ;
   else
      tfile4descending( tag, 0 ) ;
   // AS 04/25/01 - seekRc never used, so removed
   tfile4seek( tag, result, len ) ;
   tfile4descending( tag, 0 ) ;

   /* if doCheck == 1, it means we want to ensure that the record found is not == to
      another found value (i.e. check).  For example if our range is: >3 && <4, if 4
      has been found the previous seek (>3), then check is set to 4 and doCheck to 1,
      that way if our seek finds '4', then we want to stop right away (no find) before
      we do the adjustment of moving back a record.  Note that if the 2nd operator
      is <= or >=, we should not be doing this check (i.e. the caller should have
      inputed doCheck = 0)

      This works because we always do forward seeking (we change the descend
      flag as required)
   */

   if ( doCheck == 1 )
      if ( !tfile4eof( tag ) )
        if ( check == tfile4recNo( tag ) )
           return ULONG_MAX ;

   switch ( location )
   {
      case 0:
         /* in this instance, location is indicating that we want a 'less
            than' position.  Therefore we need to go backwards by one.
            Note that if we are already at the top (i.e. the 1st record
            in the set matches), then clearly a less than condition is
            impossible, so '-1' is returned to indicate this.
         */
         if ( tfile4skip( tag, -1L ) != -1L )   /* at top already */
            return ULONG_MAX ;
         break ;
      case 1:
         /* in this instance, location is indicating that we want a 'greater
            than or equal' position.  Note that if we reached end of tag, then
            clearly all values in the tag are less than our input value so
            '>=' is impossible, so '-1' is returned to indicate this.
         */
         if ( tfile4eof( tag ) )
            return ULONG_MAX ;
         break ;
      case 2:
         /* in this instance location is indicating that want a less than
            or equal seek.  Note that we convert the tag to a descending
            tag before the seek.  This means that we seek starting at the
            larger values and end when we hit the first value which is
            '<=' our desired value we stop.

            Note that we potentilly do a doCheck comparison as well.
         */
         if ( doCheck == 1 )
            if ( !tfile4eof( tag ) )
               if ( check == tfile4recNo( tag ) )   /* case where none belong, so break now */
               {
                  #ifdef S4FOX
                     // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                     // if ( u4keycmp( tfile4keyData(tag)->value, result, (unsigned int)len, (unsigned int)tag->header.keyLen, 0, &tag->vfpInfo ) != 0 )
                     if ( u4keycmp( tfile4keyData(tag)->value, result, (unsigned int)len, (unsigned int)tag->header.keyLen, 0, collation4get( tag->collateName ) ) != 0 )
                  #else
                     if( (*tag->cmp)( tfile4keyData(tag)->value, result, (unsigned int)len ) != 0 )  /* last one is too far, go back one for a closure */
                  #endif
                        return ULONG_MAX ;
               }
         break ;
      case 3:
         /* in this instance location is indicating that we want a 'greater
            than' value.  Notice that we switch the tag to descending before
            the seek, which means we will effectively seek from the largest
            values to the smallest values, and stop at the first value
            which is <= our seek value.

            If we reach eof this then means that all the records are in
            actual fact greater than the value we want.  Therefore just
            go to the top of the file.  Note that a top failure means
            an empty file which means no records match, so return '-1'
            to indicate no matches.

            We need to skip forwards once after the seek since we want
            'greater than' not 'greater than or equal to'.  Note that the
            property of seeking descending is that we will be on the
            first value less than or equal to search key, so we will always
            want to skip one forwards to move onto the first greater than
            key.  If we cannot skip forward then it means that no records
            were found greater than what we seeked for, so return '-1' to
            indicate this.

         */
         if ( tfile4eof( tag ) )
         {
            rc = tfile4top( tag ) ;
            if ( rc != 0 )  /* no records */
               return ULONG_MAX ;
         }
         else
         {
            rc = (int)tfile4skip( tag, 1L ) ;
            if ( rc == 0L )
               return ULONG_MAX ;
         }
         break ;
      default:
         error4( map->log->codeBase, e4info, E93701 ) ;
         return ULONG_MAX ;
   }

   return tfile4recNo( tag ) ;
}
#endif



#ifdef S4MDX
unsigned long bitmap4seek( BITMAP4 *map, const CONST4 *con, const char location, const unsigned long check, const int doCheck )
{
   /*
      con is the constant value we are seeking for
      location has the following meanings:
        '0' --> we want a '<' position from the constant
        '1' --> we want a '>=' position
        '2' --> we want a '<=' position
        '3' --> we want a '>' position
   */
   int len, rc ;
   char didSkip ;

   TAG4FILE *tag = map->tag ;
   char *result = (char *)const4return( map->log, con ) ;
   int isDesc = ( tag->header.typeCode & 8 ) ? 1 : 0 ;

   if ( map->type != r4str )   /* must convert to a proper key */
   {
      #ifdef E4ANALYZE
         if ( expr4len( tag->expr ) == -1 )
            return error4( map->log->codeBase, e4info, E83701 ) ;
      #endif
      /* AS 07/27/99 -> support for generic collating for Unicode and character fields...  */
      // #ifdef S4VFP_KEY
      //#ifdef S4FOX
      //   len = expr4keyConvert( tag->expr, (char **)&result, con->len, map->type, tag->expr->vfpInfo->sortType ) ;
      //#else
         len = expr4keyConvert( tag->expr, (char **)&result, con->len, map->type, tag ) ;
      //#endif
   }
   else
      len = con->len ;

   int seekRc = tfile4seek( tag, result, len ) ;

   /* if doCheck == 1, it means we want to ensure that the record found is not == to
      another found value (i.e. check).  For example if our range is: >3 && <4, if 4
      has been found the previous seek (>3), then check is set to 4 and doCheck to 1,
      that way if our seek finds '4', then we want to stop right away (no find) before
      we do the adjustment of moving back a record.  Note that if the 2nd operator
      is <= or >=, we should not be doing this check (i.e. the caller should have
      inputed doCheck = 0)

      This does not work as simply if the index is actually a descending index.
      In that case we have to do the check after compensating for the descend.
   */

   if ( !isDesc )
   {
      if ( !tfile4eof( tag ) )
         if ( doCheck && location < 2 )
            if ( check == tfile4recNo( tag ) )
               return -1 ;
   }

   switch ( location )
   {
      case 0:
         if ( isDesc )
         {
            if ( tfile4eof( tag ) )
               return -1 ;
            for( ;; )
            {
               // AS 05/17/99 --> to avoid potential gpf if tfile4keyData fails...
               B4KEY_DATA *currentKey = tfile4keyData(tag) ;
               if ( currentKey == 0 ) // means an error
                  return error4( map->log->codeBase, e4info, E93701 ) ;
               if ( (*tag->cmp)( tfile4keyData(tag)->value, result, len ) != 0 )
                  break ;
               rc = (int)tfile4skip( tag, 1L ) ;
               if ( rc < 0 )
                 return -1 ;
               if ( rc != 1 )
               {
                 if ( rc == 0 )
                    return -1 ;
                 break ;
               }
            }
         }
         else
            if ( tfile4skip( tag, -1L ) != -1L )   /* at top already */
               return -1 ;
         break ;
       case 1:
         if ( isDesc )
         {
            if ( seekRc == 2 )
            {
               if ( !tfile4eof( tag ) )
                 if ( check == tfile4recNo( tag ) )   /* case where none belong, so break now */
                    return -1 ;
               if ( tfile4skip( tag, -1L ) != -1L )
                  return -1 ;
            }
            else
            {
               rc = -1 ;
               // keep skipping as long as the current key == result
               for( ;; )
               {
                  // AS 05/17/99 --> to avoid potential gpf if tfile4keyData fails...
                  B4KEY_DATA *currentKey = tfile4keyData(tag) ;
                  if ( currentKey == 0 ) // means an error
                     return error4( map->log->codeBase, e4info, E93701 ) ;
                  if ( (*tag->cmp)( currentKey->value, result, len ) != 0 )
                     break ;
                  rc = (int)tfile4skip( tag, 1L ) ;
                  if ( rc < 0 )
                     return -1 ;
                  if ( rc != 1 )
                     break ;
               }
               if ( rc == 0 )
                  tfile4bottom( tag ) ;
               else
                  tfile4skip( tag, -1L ) ;
            }
         }
         else
         {
            if ( tfile4eof( tag ) )
               return -1 ;
         }
         break ;
      case 2:
         if ( isDesc )
         {
            if ( tfile4eof( tag ) )
               return -1 ;
            if ( seekRc == 2 )
               if ( doCheck == 1 )
                  if ( check == tfile4recNo( tag ) )   /* case where none belong, so break now */
                     return -1 ;
            break ;
         }
         else
         {
            // AS 05/17/99 --> to avoid potential gpf if tfile4keyData fails...
            // AS 07/06/99 --> if at eof was failing because tfile4keyData fails...
            if ( tfile4eof( tag ) )
            {
               /* we want <=, and are at eof, so just skip backwards 1 and we are there (assuming there are records in the datafile) */
               if ( tfile4skip( tag, -1L ) != -1L )   /* no records... */
                  return -1 ;
               break ;
            }
            B4KEY_DATA *currentKey = tfile4keyData(tag) ;
            if ( currentKey == 0 ) // means an error
               return error4( map->log->codeBase, e4info, E93701 ) ;
            if ( (*tag->cmp)( currentKey->value, result, len ) != 0 )  /* last one is too far, go back one for a closure */
            {
               if ( !tfile4eof( tag ) )
                  if ( check == tfile4recNo( tag ) )   /* case where none belong, so break now */
                     return -1 ;
               if ( tfile4skip( tag, -1L ) != -1L )
                  return -1 ;
            }
            // just skip down to case 3...
         }
         // just skip down to case 3...
      case 3:  // sometimes we skip through into here from 'case 2'
         if ( isDesc )
         {
            if ( tfile4skip( tag, -1L ) != -1L )   /* at top already */
               return -1 ;
         }
         else
         {
            // AS 05/17/99 --> if tag file is empty, or at eof already, was failing...
            if ( tfile4eof( tag ) )
            {
               /* on last record not far enough, so none match */
               return -1 ;
            }
            didSkip = 0 ;

            // keep skipping as long as the current key == result
            for( ;; )
            {
               // AS 05/17/99 --> to avoid potential gpf if tfile4keyData fails...
               B4KEY_DATA *currentKey = tfile4keyData(tag) ;
               if ( currentKey == 0 ) // means an error
                  return error4( map->log->codeBase, e4info, E93701 ) ;
               if ( (*tag->cmp)( currentKey->value, result, len ) != 0 )
                  break ;
               rc = (int)tfile4skip( tag, 1L ) ;
               if ( rc < 0 )
                  return -1 ;
               if ( rc != 1 )
               {
                  if ( location == 2 )   /* on last record, but it still belongs, so don't skip back */
                     didSkip = 0 ;
                  if ( location == 3 )   /* on last record not far enough, so none match */
                     return -1 ;
                  break ;
               }
               didSkip = 1 ;
            }

            if ( location == 3 )
            {
               if ( didSkip == 0 && seekRc != 2 )
                  if ( tfile4skip( tag, 1L ) != 1L )
                     return -1 ;
            }
            else
               if ( didSkip == 1 )
                  if ( tfile4skip( tag, -1L ) != -1L )
                     return -1 ;
         }
         break ;
      default:
         return error4( map->log->codeBase, e4info, E93701 ) ;
   }

   return tfile4recNo( tag ) ;
}
#endif



void *const4return( L4LOGICAL *log, const CONST4 *c1 )
{
   /* returns a pointer to the constant value */
   return (void *)( log->buf + c1->offset ) ;
}



int const4memAlloc( L4LOGICAL *log, const unsigned len )
{
   /* updates the log's constant memory buffer, re-allocating memory if required */
   if ( ( log->bufPos + len ) > log->bufLen )
   {
      #ifdef E4ANALYZE
         if ( (long)len + (long)log->bufLen != (long)(len + log->bufLen) )
            return error4( log->codeBase, e4memory, E83702 ) ;
      #endif
      if ( u4allocAgain( log->codeBase, &log->buf, &log->bufLen, log->bufPos + len ) != 0 )
         return error4( log->codeBase, e4memory, E93704 ) ;
   }
   log->bufPos += len ;
   return 0 ;
}

/* duplicate an existing constant */
int const4duplicate( CONST4 *to, const CONST4 *from, L4LOGICAL *log )
{
   unsigned int len ;

   len = (unsigned int)from->len ;

   if ( len == 0 )
      c4memset( (void *)to, 0, (unsigned int)sizeof( CONST4 ) ) ;
   else
   {
      if ( const4memAlloc( log, len ) < 0 )
         return -1 ;
      c4memcpy( log->buf + log->bufPos - len, const4return( log, from ), len ) ;
      to->offset = log->bufLen - len ;
      to->len = len ;
   }

   return 0 ;
}

/* get a constant from an expr. info structure */
int const4get( CONST4 *con, BITMAP4 *map, L4LOGICAL *log, const int pos )
{
   unsigned int len ;
   char *result ;
   int rc ;

   if ( expr4execute( log->expr, pos, (void **)&result ) < 0 )
      return -1 ;
   len = (unsigned int)log->expr->info[pos].len ;

   #ifdef E4ANALYZE
      if ( map->type != 0 && map->type != v4functions[log->expr->info[pos].functionI].returnType )
         return error4( map->log->codeBase, e4info, E83703 ) ;
   #endif

   rc = const4memAlloc( log, len ) ;
   if ( rc < 0 )
      return error4stack( map->log->codeBase, rc, E93704 ) ;

   c4memcpy( log->buf + log->bufPos - len, result, len ) ;
   map->type = v4functions[log->expr->info[pos].functionI].returnType ;
   con->offset = log->bufLen - len ;
   con->len = len ;

   return 0 ;
}

int const4less( CONST4 *p1, CONST4 *p2, BITMAP4 *map )
{
   switch( map->type )
   {
      case r4numDoub:
      case r4dateDoub:
         #ifdef E4ANALYZE
            if ( p1->len != p2->len )
               return error4( map->log->codeBase, e4struct, E93704 ) ;
         #endif
         if ( *(double *)const4return( map->log, p1 ) < *(double *)const4return( map->log, p2 ) )
            return 1 ;
         break ;
      case r4num:
      case r4str:
         if ( p1->len < p2->len )
         {
            if ( c4memcmp( const4return( map->log, p1 ), const4return( map->log, p2 ), (unsigned int)p1->len ) <= 0 )
               return 1 ;
         }
         else
            if ( c4memcmp( const4return( map->log, p1 ), const4return( map->log, p2 ), (unsigned int)p2->len ) < 0 )
               return 1 ;
         break ;
      default:
         return error4( map->log->codeBase, e4info, E93704 ) ;
   }

   return 0 ;
}

int const4eq( CONST4 *p1, CONST4 *p2, BITMAP4 *map )
{
   if ( p1->len < p2->len )
   {
      #ifdef E4ANALYZE
         if ( map->type == r4numDoub || map->type == r4dateDoub )
            return error4( map->log->codeBase, e4struct, E93704 ) ;
      #endif
      return 0 ;
   }

   #ifdef E4ANALYZE
      switch( map->type )
      {
         case r4numDoub:
         case r4dateDoub:
         case r4num:
         case r4str:
         case r4log:
            break ;
         default:
            return error4( map->log->codeBase, e4info, E93704 ) ;
      }
   #endif

   if ( c4memcmp( const4return( map->log, p1 ), const4return( map->log, p2 ), (unsigned int)p1->len ) == 0 )
      return 1 ;

   return 0 ;
}


// returns true if constant value p1 <= constant value p2.
int const4lessEq( CONST4 *p1, CONST4 *p2, BITMAP4 *map )
{
   switch( map->type )
   {
      case r4numDoub:
      case r4dateDoub:
         #ifdef E4ANALYZE
            if ( p1->len != p2->len )
               return error4( map->log->codeBase, e4struct, E93704 ) ;
         #endif
         if ( *(double *)const4return( map->log, p1 ) <= *(double *)const4return( map->log, p2 ) )
            return 1 ;
         break ;
      case r4num:
      case r4str:
         if ( p1->len <= p2->len )
         {
            if ( c4memcmp( const4return( map->log, p1 ), const4return( map->log, p2 ), (unsigned int)p1->len ) <= 0 )
               return 1 ;
         }
         else
            if ( c4memcmp( const4return( map->log, p1 ), const4return( map->log, p2 ), (unsigned int)p2->len ) < 0 )
               return 1 ;
         break ;
      default:
         return error4( map->log->codeBase, e4info, E93704 ) ;
   }

   return 0 ;
}

void const4addNe( BITMAP4 *map, CONST4 *con )
{
   CONST4 *cOn ;

   cOn = (CONST4 *)l4first( &map->ne ) ;
   while ( cOn != 0 )
   {
      if ( const4eq( con, cOn, map ) )  /* ne already exists, so ignore */
         return ;
      cOn = (CONST4 *)l4next( &map->ne, cOn ) ;
   }
   cOn = (CONST4 *) u4alloc( (long)sizeof( CONST4 ) ) ;
   if ( cOn == 0 )
      return ;
   c4memcpy( (void *)cOn, (void *)con, (unsigned int)sizeof( CONST4 ) ) ;
   l4add( &map->ne, cOn ) ;
   c4memset( (void *)con, 0, (unsigned int)sizeof( CONST4 ) ) ;
}

void const4deleteNe( LIST4 *listIn, CONST4 *con )
{
   l4remove( listIn, con ) ;
   u4free( con ) ;
}

#endif   /* S4INDEX_OFF */
#endif   /* S4CLIENT */

