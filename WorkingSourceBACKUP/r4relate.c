/* r4relate.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

// AS Apr 18/05 - new callback functionality - currently only for windows 32 bit...
// AS - moved to R4RELATE.H
// AS Jan 23/04 - Support a suspension of performing the relate so other clients can perform actions
// #ifdef S4SERVER
// #endif

#ifdef S4CLIENT
   static int relation4readBufferAlloc( RELATION4 *relation, short doMemos ) ;
   static int relation4skipFetchMultiple( RELATION4 *relation, long numskip, short startPos, long startOffset ) ;
   static int relate4freeServer( RELATE4 *relate ) ;
#else
   static int relate4currentIsChild( RELATE4 * ) ;
   static int relate4parent( RELATE4 *, RELATE4 * ) ;
   static int relate4nextRelationList( RELATION4 *, int ) ;
   static int relate4prevRecordInScan( RELATE4 * ) ;
   static int relate4prevRelationList( RELATION4 *, int ) ;
   static int relate4topInit( RELATE4 * ) ;
#endif

#ifdef S4SERVER
static int relate4initRelate( RELATE4 *, RELATION4 *, DATA4 *, CODE4 *, int, char * ) ;
#else
static int relate4initRelate( RELATE4 *, RELATION4 *, DATA4 *, CODE4 *, int ) ;
#endif
static int relate4lookup( RELATE4 *, const signed char ) ;
static int relate4readRest( RELATE4 *, signed char ) ;
static void relate4setNotRead( RELATE4 * ) ;
static void relate4sortFree( RELATION4 *, const int ) ;
static int relate4sortGetRecord( RELATION4 *, const long, char *, Bool5, long * ) ;
static int relate4sortNextRecord( RELATION4 * ) ;
static int relate4sortPrevRecord( RELATION4 * ) ;
static int relate4skipInternal( RELATE4 *relate, const long numSkip ) ;

// AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
static int relate4sortSeek( RELATION4 *relation ) ;

#ifdef S4CLIENT
   static int relate4flush( RELATE4 *root )
   {
      RELATE4 *relateOn ;
      int rc ;

      for( relateOn = root ;; )
      {
         rc = d4updateRecord( relateOn->data, 0, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc )
            return rc ;

         if ( relate4next( &relateOn ) == 2 )
            break ;
      }

      return 0 ;
   }
#endif /* S4CLIENT */



#ifndef S4CLIENT
   /* !S4CLIENT */
   static int f4flagIsSetFlip( F4FLAG *flagPtr, const unsigned long r )
   {
      if ( flagPtr->flags == 0 )
         return 1 ;

      if ( flagPtr->isFlip )
         return ! f4flagIsSet( flagPtr, r ) ;
      else
         return f4flagIsSet( flagPtr, r ) ;
   }



   /* !S4CLIENT */
   static unsigned long f4flagGetNextFlip( F4FLAG *f4, const unsigned long startPos, const signed char direction )
   {
      /* returns the number of positions to the next flipped bit in the bitmap - start at startPos (inclusive) */
      unsigned char currentPosInByte, normalizedCurrentByte, shiftAmount, relaventInformationByte, indexAmount ;
      unsigned long onPos, currentByteNumber ;
      int i ;

      #ifdef E4PARM_LOW
         if ( direction != -1 && direction != 1 )
            return error4( 0, e4parm, E90812 ) ;
      #endif

      onPos = startPos ;
      if ( f4->flags == 0 || startPos > f4->numFlags )
         return 0 ;

      /*
         every byte in the flag's data corresponds to 8 entries since it is
         a bitmap, and there are 8 bits in a byte.  Therefore, in order to
         get a positions address into the bitmap, the lower 3 bits or the
         position are used to determine the position within the byte
         (8 posibilities).  The position is shifted 3 bits to the right to
         determine the byte number within the bitmap of the current position.
      */

      currentPosInByte = (unsigned char)( startPos & 0x7 ) ;
      currentByteNumber = (unsigned long)( startPos >> 3 ) ;

      if ( direction != -1 )
      {
         if ( f4->isFlip )
            normalizedCurrentByte = (unsigned char)~((unsigned char)f4->flags[currentByteNumber]) ;
         else
            normalizedCurrentByte = (unsigned char)f4->flags[currentByteNumber] ;

         /*
            - the value we are looking for is the next position which is 'set'.
            to do this:

            - note the structure of the byte:
            [76543210]  --> if our current bit position is '3', we want to know
            which 'next' bit is set.  Position '3' actually means the bit
            entry '3' (i.e. the 3rd bit according to the diagram above).

            - if we shift right by the current bit position, then the entries
              which are previous to our current entry will be effectively
              removed, with the conditional that our value has been shifted
              over and therefore doesn't represent the real mapping anymore.

              in the example above, shift by 3: we get [...76543].  Clearly
              the unwanted bits are no longer there.  Note that the '3' entry
              is still there, because we include the start position as a
              candidate for success.
         */

         relaventInformationByte = (unsigned char)(normalizedCurrentByte >> currentPosInByte) ;

         /*
             if the relavent information byte is 0, then clearly none of the
             'next' bits are set for this byte, so we need to move over to the
             next byte in the set.
         */
         if ( relaventInformationByte == 0 )
         {
            /*
               now clearly our current position will change by:
                  (8 - currentPosInByte) for the first byte we move
                  and by 8 for every byte thereafter.  Subtract
                  the current position first, and then add by 8
                  every time through the loop.
            */

            onPos -= currentPosInByte ;

            for( ; relaventInformationByte == 0 ; onPos += 8 )
            {
               if ( onPos >= f4->numFlags )
               {
                  /*
                      if the current position becomes greater than the number
                      of flags it the set, then clearly we have reached the end.
                      The amount we positioned by is == # of flags - the
                      current position + 1 since the current position is
                      inclusive (i.e. a '0' would indicate the current position
                      was valid)
                  */
                  return ( f4->numFlags - startPos + 1 ) ;
               }
               ++currentByteNumber ;
               if ( f4->isFlip )
                  relaventInformationByte = (unsigned char)~((unsigned char)f4->flags[currentByteNumber]) ;
               else
                  relaventInformationByte = (unsigned char)f4->flags[currentByteNumber] ;
            }
         }

         /*
            if we are here, then we have left a 'relaventInformationByte'
            which represents the next position in the set.
            increment the position one past the current

            just look at the lowest order bit.  If that bit is set then
            the next position is 'set', so done.

            if it is not set, increment the current position by one and
            check the next bit.

            do this until either we have found a bit set or we are done
            (i.e. was actually case of no bits set).
         */

         for( i = 0 ; i <= 7 ; i++, onPos++, relaventInformationByte >>= 1 )
            if ( relaventInformationByte & 0x01 )
               break ;

         /* now clearly we have moved by the difference between the current
            position and the start position */
         return (onPos - startPos) ;
      }
      else
      {
         /* we want to move backwards in the bitmap */
         if ( f4->isFlip )   /* the bitmap is really a mirror of it's true contents */
            normalizedCurrentByte = (unsigned char)~((unsigned char)( f4->flags[currentByteNumber] )) ;
         else
            normalizedCurrentByte = (unsigned char)( f4->flags[currentByteNumber] ) ;

         /*
            as in the forward direction, we desire a normalized byte.  Since
            we are going in the reverse direction, we need to trip off the
            bits on the left of the byte (i.e. the bits > current pos).

            eg.  [76543210], where current position is 5 (inclusive):
               shift left by 7 - currentPosition(5): [543210..]

         */

         shiftAmount = 7 - currentPosInByte ;

         relaventInformationByte = normalizedCurrentByte << shiftAmount ;

         /* if the current byte is empty and is not the first byte in the
            set, then we need to find the previous non-empty byte... */
         if ( relaventInformationByte == 0 && currentByteNumber != 0 )
         {
            indexAmount = 7 ;
            /*
               to track the current position, it is easier to add the amount we
               shifted first, so that we are on an even byte boundary (i.e. at
               the far left of the byte).  Then check each bit starting at the
               left and when we reach

            */

            onPos += shiftAmount ;

            for( ; relaventInformationByte == 0 ; onPos -= 8 )
            {
               --currentByteNumber ;
               if ( currentByteNumber == 0 )  /* on last byte */
               {
                  /* in this case, we have reached the beginning of the flags.
                     therefore we are on the last byte, no need to check
                     further */
                  if ( f4->flags[0] == 0 )  /* this byte is empty, clearly no records match */
                     return startPos ;  /* clearly we have moved by the startpos */
                  if ( f4->isFlip )
                     relaventInformationByte = ~(f4->flags[0]) ;
                  else
                     relaventInformationByte = f4->flags[0] ;
                  onPos -= 8 ;  /* decrement the position by 8 */
                  break ;  /* non-zero byte, so check it */
               }
               if ( f4->isFlip )
                  relaventInformationByte = ~(f4->flags[currentByteNumber]) ;
               else
                  relaventInformationByte = f4->flags[currentByteNumber] ;
            }
         }
         else
            indexAmount = currentPosInByte - 1 ;

         for( i = (int)indexAmount ; i >= 0 ; i--, onPos--, relaventInformationByte <<= 1 )
         {
            if ( relaventInformationByte & 128 )
               break ;
         }

         return (startPos - onPos) ;
      }
   }



   /* !S4CLIENT */
   int r4dataListAdd( LIST4 *l4, DATA4 *data, RELATE4 *relate )
   {
      R4DATA_LIST *r4 ;
      CODE4 *c4 ;

      c4 = relate->codeBase ;

      if ( error4code( c4 ) < 0 )
         return -1 ;

      if ( c4->relateDataListMemory == 0 )
      {
         c4->relateDataListMemory = mem4create( c4, 10, sizeof( R4DATA_LIST ), 10, 0 ) ;
         if ( c4->relateDataListMemory == 0 )
            return 0 ;
      }

      r4 = (R4DATA_LIST *)mem4allocZero( c4->relateDataListMemory ) ;
      if ( r4 == 0 )
         return -1 ;
      r4->data = data ;
      r4->relate = relate ;
      l4add( l4, r4 ) ;
      return 0 ;
   }



   /* !S4CLIENT */
   int r4dataListFind( LIST4 *l4, RELATE4 *r4 )
   {
      R4DATA_LIST *link ;

      for ( link = 0 ;; )
      {
         link = (R4DATA_LIST *)l4next( l4, link ) ;
         if ( link == 0 )
            return 0 ;
         if ( link->relate == r4 )
            return 1 ;
      }
   }



   /* !S4CLIENT */
   void r4dataListFree( LIST4 *l4 )
   {
      R4DATA_LIST *r4data, *r4data2 ;

      for ( r4data = (R4DATA_LIST *)l4first( l4 ) ; r4data ; )
      {
         r4data->relate->sortType = 0 ;
         r4data2 = (R4DATA_LIST *)l4next( l4, r4data ) ;
         l4remove( l4, r4data ) ;
         mem4free( r4data->relate->codeBase->relateDataListMemory, r4data ) ;
         r4data = r4data2 ;
      }
   }



   /* !S4CLIENT */
   static int r4dataListMassage( LIST4 *l4 )
   {
      /* this function takes a completed sort list, and adds data members in the
         following case:

         If (r)elate must be added, r's siblings must also be added

         Or, interpreted differently, if r is a relation, and any of it's children
         must be added, then all of its children must be added.
      */
      RELATE4 *relateChild ;
      R4DATA_LIST *r4data ;
      int addChildren, relateAdded ;

      if ( l4->nLink == 0 )  /* no work required */
         return 0 ;

      r4data = 0 ;

      for( ;; )
      {
         r4data = (R4DATA_LIST *)l4next( l4, r4data ) ;
         if ( r4data == 0 )
            break ;

         relateChild = 0 ;
         addChildren = 0 ;
         for( ;; )
         {
            relateChild = (RELATE4 *)l4next( &r4data->relate->slaves, relateChild ) ;
            if ( relateChild == 0 )
               break ;
            if ( r4dataListFind( l4, relateChild ) )
            {
               addChildren = 1 ;
               break ;
            }
         }

         if ( addChildren == 1 )
         {
            relateAdded = 0 ;
            relateChild = 0 ;
            for( ;; )
            {
               relateChild = (RELATE4 *)l4next( &r4data->relate->slaves, relateChild ) ;
               if ( relateChild == 0 )
                  break ;
               if ( r4dataListFind( l4, relateChild ) == 0 )
               {
                  r4dataListAdd( l4, relateChild->data, relateChild ) ;
                  relateChild->sortType = relate4exact ;
                  relateAdded = 1 ;
               }
            }
            if ( relateAdded == 1 )
               r4data = 0 ;   /* start at list top again to be sure none missed */
         }
      }

      return 0 ;
   }



   /* !S4CLIENT */
   int r4dataListBuild( LIST4 *l4, RELATE4 *relate, EXPR4 *expr, int checkType )
   {
      /* 1 - database added, 0 - database not added, -1 - error */
      /* checkType gives the caller's status in terms of whether we should be included */
      int i ;
      char mustAdd ;
      E4INFO *info ;
      RELATE4 *slaveOn ;

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      mustAdd = 0 ;

      /* 1st check if we must belong */
      for( i = 0 ; i < expr->infoN ; i++ )
      {
         info = expr->info + i ;
         if ( info->fieldPtr )
         {
            if ( info->fieldPtr->data == relate->data )
            {
               mustAdd = 1 ;
               break ;
            }
         }
      }

      relate->sortType = relate4exact ;

      if ( mustAdd )
         checkType = relate4exact ;
      else
      {
         if ( relate->relationType == relate4scan )
            checkType = relate4scan ;
         else
            if ( checkType != relate4scan )   /* non-scan parent must be added, so we add ourselves too, in order to save work later */
               mustAdd = 1 ;
      }

      /* if a child must be added, we must be too: */
      for ( slaveOn = 0 ;; )
      {
         slaveOn = (RELATE4 *)l4next( &relate->slaves, slaveOn ) ;
         if ( slaveOn == 0 )
            break ;
         if ( r4dataListBuild( l4, slaveOn, expr, checkType ) == 1 )
            mustAdd = 1 ;
      }

      if ( mustAdd )
         r4dataListAdd( l4, relate->data, relate ) ;
      else
         if ( relate->relationType == relate4scan )
            relate->sortType = relate4sortSkip ;

      return mustAdd ;
   }



   /* !S4CLIENT */
   static int relate4blankSet( RELATE4 *relate, const signed char direction )
   {
      /* direction : -1 = look backwards, 0 = lookup only, 1 = look forwards */
      RELATE4 *slave ;
      CODE4 *c4 ;
      int rc ;

      #ifdef E4PARM_LOW
         if ( direction < -1 || direction > 1 )
            return error4( 0, e4parm, E94417 ) ;
      #endif

      c4 = relate->codeBase ;

      if ( error4code( c4 ) < 0 )
         return -1 ;

      relate->isRead = 1 ;
      if ( direction != -1 ) /* LY 99/07/05 : AIX -1 == 377?! */
      {
         if ( d4goEof( relate->data ) < 0 )
            return -1 ;
      }
      else
      {
         rc = d4top( relate->data ) ;
         if ( rc )
            return rc ;
         rc = d4skip( relate->data, -1L ) ;
         relate->data->recNum = -1 ;
         d4blank( relate->data ) ;
         relate->data->recordChanged = 0 ;
         if ( error4code( c4 ) < 0 )
            return -1 ;
         #ifdef S4SINGLE
            if ( rc < 0 )
         #else
            if ( rc == r4locked || rc < 0 )
         #endif
               return rc ;
      }

      for( slave = 0 ;; )
      {
         slave = (RELATE4 *)l4next( &relate->slaves, slave ) ;
         if ( slave == 0 )
            return 0 ;
         rc = relate4blankSet( slave, direction ) ;
         if ( rc < 0 )
            return rc ;
      }
   }
#endif /* !S4CLIENT */



#ifdef S4CLIENT
   /* S4CLIENT */
   int relation4unpack( RELATION4 *relation, CONNECTION4 *connection )
   {
      CONNECTION4RELATION_DATA_OUT *info ;
      RELATE4 *relate ;
      unsigned int pos ;
      long recCount ;
      #ifndef S4OFF_MEMO
         int i ;
      #endif

      long len = connection4len( connection ) ;
      const char *data = connection4data( connection ) ;
      if ( len < sizeof( CONNECTION4RELATION_DATA_OUT ) )
         return error4( relation->relate.codeBase, e4packetLen, E94425 ) ;
      info = (CONNECTION4RELATION_DATA_OUT *)data ;
      if ( ntohl5(info->relationId) != relation->relationId )
         return error4( relation->relate.codeBase, e4connection, E84305 ) ;

      pos = sizeof( CONNECTION4RELATION_DATA_OUT ) ;
      for( relate = &relation->relate ;; )
      {
         DATA4 *d4 = relate->data ;
         #ifndef S4OFF_MEMO
            if ( d4->dataFile->nFieldsMemo > 0 )
            {
               for ( i = 0; i < d4->dataFile->nFieldsMemo; i++ )
                  f4memoReset( d4->fieldsMemo[i].field ) ;
            }
         #endif
         if ( len < (long)pos + (long)dfile4recWidth( d4->dataFile ) + (long)sizeof( d4->recNum ) + (long)sizeof(S4LONG) )
            return error4( relation->relate.codeBase, e4packetLen, E94425 ) ;
         memcpy( &d4->recNum, data + pos, sizeof( d4->recNum ) ) ;
         d4->recNum = ntohl5(d4->recNum) ;
         pos += sizeof( d4->recNum ) ;
         recCount = ntohl5(*((long *)( data + pos ))) ;
         pos += sizeof(S4LONG ) ;
         if ( recCount < 0 )
            return error4( relation->relate.codeBase, e4result, E84305 ) ;
         // AS Aug 22/02 - if recCount is 0, we are always at eof as well - was failing on empty tables
         if ( d4->recNum > recCount || recCount == 0 )
            d4->eofFlag = 1 ;
         else
            d4->eofFlag = 0 ;
         // AS Nov 27/01 - Was failing in some cases where skipping backwards...
         if ( recCount == 0 || d4->recNum == 0 || d4->recNum == -1)
            d4->bofFlag = 1 ;
         else
            d4->bofFlag = 0 ;
         memcpy( d4->record, data + pos, dfile4recWidth( d4->dataFile ) ) ;
         pos += dfile4recWidth( d4->dataFile ) ;
         // AS May 21/02 - code to support auto-transfer of memo fields
         assert5port( "added optional transfer of memo fields with client/server communications" ) ;
         if ( relation->includeMemos )
         {
            int numMemos = d4->dataFile->nFieldsMemo ;
            for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
            {
               FIELD4 *field = d4->fieldsMemo[memoLoop].field ;
               long memoLen = ntohl5( *((long *)(data + pos)) ) ;
               pos += sizeof( long ) ;
               int rc2 = f4memoReadSet( field, memoLen, data + pos ) ;
               if ( rc2 != 0 )
                  return rc2 ;
               pos += memoLen ;
            }

         }
         if ( relate4next( &relate ) == 2 )
            break ;
      }

      if ( len != (long)pos )
         return error4( relation->relate.codeBase, e4packetLen, E94425 ) ;

      return 0 ;
   }



   /* S4CLIENT */
   int relation4unpackMultiple( RELATION4 *relation, char *buf )
   {
      assert5port( "client/server support for batch reading of related records" ) ;
      // AS Apr 12/02 block reading of records for relate skip...
      // retrieves 1 relate record from the socket
      // buffer is set out as follows:
      //   <relate rc> plus for each DATA4:
      //       <bof marker><eof marker><rec num><record>
      //

      RELATE4 *relate = &relation->relate ;
      CODE4 *c4 = relate->codeBase ;

      CONNECT4 *connect = c4getClientConnect( c4 ) ;
      int memoOn = 0 ;

      for( ;; )
      {
         DATA4 *data = relate->data ;
         long recNum = connect4receiveLong( connect ) ;
         long recCount = connect4receiveLong( connect ) ;
         short eofFlag = 0 ;
         short bofFlag = 0 ;

         if ( recNum > recCount )
            eofFlag = 1 ;

         if ( recCount == 0 || recNum == 0 || recNum == -1)
            bofFlag = 1 ;

         memcpy( buf, &bofFlag, sizeof( short ) ) ;
         buf += sizeof( short ) ;
         memcpy( buf, &eofFlag, sizeof( short ) ) ;
         buf += sizeof( short ) ;
         memcpy( buf, &recNum, sizeof( long ) ) ;
         buf += sizeof( long ) ;

         long recWidth = dfile4recWidth( data->dataFile ) ;
         connect4receive( connect, buf, recWidth, code4timeoutVal( c4 ) ) ;
         buf += recWidth ;

         if ( relation->doMemos )
         {
            int numMemos = data->dataFile->nFieldsMemo ;
            for ( int memoLoop = 0 ; memoLoop < numMemos ; memoLoop++ )
            {
               assert5( memoOn < relation->numMemos ) ;   // should be enough space for each memo
               MEMO4BATCH_ENTRY *entry = &relation->memos[relation->readBufNum].memos[memoOn] ;
               long memoLen = connect4receiveLong( connect ) ;
               if ( memoLen > 0 )
               {
                  if ( (long)entry->contentsAllocLen < memoLen )
                  {
                     if ( entry->contents != 0 )
                     {
                        u4free( entry->contents ) ;
                        entry->contents = 0 ;
                        entry->contentsAllocLen = 0 ;
                     }
                     entry->contents = (char *)u4allocFree( data->codeBase, memoLen ) ;
                     if ( entry->contents == 0 )
                        return e4memory ;
                     entry->contentsAllocLen = memoLen ;
                  }
                  connect4receive( connect, entry->contents, memoLen, code4timeoutVal( c4 ) ) ;
               }
               entry->contentsLen = memoLen ;
               memoOn++ ;
            }

         }
         if ( relate4next( &relate ) == 2 )
            break ;
      }

      return 0 ;
   }
#endif



#ifdef E4ANALYZE
   // AS Jun 26/02 code to allow for auto-count for testing from relate4top/relate4bottom
   static void relate4doSpecialCount( RELATE4 *relate )
   {
      // code to allow for auto-count for testing from relate4top - but don't call in relate4top if we are
      // being called from relate4count.
      static int inCount = 0 ;

      if ( inCount == 1 )
         return ;
      inCount = 1 ;

      relate->relation->countAccurate = 0 ;  // for testing, ensure we do the count from scratch every time...

      #if defined( S4TESTING ) && defined( S4CLIENT )
         // AS Sep 3/03 - client/server, with S4TESTING defined code was failing due to c4->expectedCount not being set to -1
         relate->codeBase->expectedCount = -1 ;
      #endif

      #ifdef S4WIN32
         // AS Jul 25/05 - con't perform callbacks during this special count process (debugging only)
         QUERY_CALLBACK callback = relate->callback ;
         relate->callback = 0 ;
      #endif

      // count and ensure is valid by skipping through
      unsigned long count = relate4count( relate ) ;
      if ( count == ULONG_MAX - 1 )  // means in the count function already, just return
      {
         #ifdef S4WIN32
            relate->callback = callback ;
         #endif
         return ;
      }
      unsigned long manualCount = 0 ;

      int rc = relate4top( relate ) ;
      while ( rc == 0 )
      {
         manualCount++ ;
         rc = relate4skipInternal( relate, 1L ) ;
      }

      if ( manualCount != count )
      {
         // if ( !( manualCount == 0 && count == ULONG_MAX ) )  // is ok if manual is0, and count return is ULONG_MAX (error)
            error4( relate->codeBase, e4info, E94401 ) ;
      }

      // and now call changed to ensure everything gets reset, which might be required for testing.
      relate4changed( relate ) ;
      inCount = 0 ;

      #ifdef S4WIN32
         relate->callback = callback ;
      #endif
   }
#endif /* E4ANALYZE */



int S4FUNCTION relate4bottom( RELATE4 *relate )
{
   RELATION4 *relation ;
   int rc, rc2 ;
   #ifdef S4CLIENT
      CONNECTION4 *connection ;
      CONNECTION4RELATE_BOTTOM_INFO_IN *info ;
   #else
      #ifndef S4OFF_MULTI
         char oldReadLock ;
      #endif
      long rec ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94401 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_LOW
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94401 ) ;
   #endif

   CODE4 *c4 = relate->codeBase ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   relation = relate->relation ;
   relate = &relation->relate ;

   relate->suspend.isInit = 0 ;

   #ifdef E4ANALYZE
      // AS Jun 26/02 code to allow for auto-count for testing from relate4top/relate4bottom
      if ( relate->relation->isInitialized == 0 )
         relate4doSpecialCount( relate ) ;
      relate->suspend.isInit = 0 ;
   #endif

   // AS Aug 20/02 - set this for client as well
   if ( relation->skipBackwards == 0 )
   {
      relate4sortFree( relation, 0 ) ;
      relate4skipEnable( relate, 1 ) ;
   }

   #ifdef S4CLIENT
      if ( relate->dataTag != relate->data->tagSelected )
         relate4changed( relate ) ;

      if ( relation->isInitialized == 0 )
      {
         // AS Sept 3/02 - Failing to select tag if relate4bottom() is called instead of relate4top()
         relate->dataTag = relate->data->tagSelected ;
         rc = relate4clientInit( relate ) ;
         if ( rc != 0 )
            return rc ;
      }
      #ifdef S4CB51
         #ifndef S4OFF_MULTI
            if ( c4getReadLock( c4 ) )
            {
               rc = relate4lock( relate ) ;
               if ( rc != 0 )
                  return rc ;
            }
         #endif
      #endif
      #ifdef E4ANALYZE
         if ( relate->data == 0 )
            return error4( c4, e4parm, E94401 ) ;
         if ( relate->data->dataFile == 0 )
            return error4( c4, e4parm, E94401 ) ;
      #endif
      connection = relate->data->dataFile->connection ;
      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( c4, e4parm, E94401 ) ;
      #endif

      rc = relate4flush( relate ) ;
      if ( rc )
         return rc ;

      // AS Apr 12/02 block reading of records for relate skip...
      assert5port( "client/server support for batch reading of related records" ) ;
      if ( relation->readRecsToBuf > 0 )
      {
         relation4readBufferReset( relation, 0 ) ;
         rc =  relation4skipFetchMultiple( relation, 0, 2, 0 ) ;  // '2' means bottom
         if ( rc < 0 || rc == r4terminate )
         {
            // AS Aug 6/02 - Ensure that server frees up info as well...
            relate4freeServer( relate ) ;
            relation->isInitialized = 0 ;
            if ( rc < 0 )
               return connection4error( connection, c4, rc, E94401 ) ;
         }
      }
      else
      {
         connection4assign( connection, CON4RELATE_BOTTOM, 0, 0 ) ;
         connection4addData( connection, 0, sizeof( CONNECTION4RELATE_BOTTOM_INFO_IN ), (void **)&info ) ;
         info->relationId = htonl5( relation->relationId ) ;
         // AS May 21/02 - code to support auto-transfer of memo fields
         assert5port( "added optional transfer of memo fields with client/server communications" ) ;
         info->includeMemos = relation->includeMemos ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E94401 ) ;
         rc = connection4status( connection ) ;
         if ( rc < 0 )
         {
            // AS Aug 6/02 - Ensure that server frees up info as well...
            relate4freeServer( relate ) ;
            relation->isInitialized = 0 ;
            if ( rc < 0 )
               return connection4error( connection, c4, rc, E94401 ) ;
         }
         rc2 = relation4unpack( relation, connection ) ;
         if ( rc2 < 0 )
            return error4stack( c4, rc, E94401 ) ;
         if ( rc == r4terminate )
         {
            // AS Aug 6/02 - Ensure that server frees up info as well... - after the unpack
            relate4freeServer( relate ) ;
            relation->isInitialized = 0 ;
         }
      }
      return rc ;
   #else
      #ifndef S4OFF_MULTI
         oldReadLock = c4getReadLock( c4 ) ;
         c4setReadLock( c4, 0 ) ;
      #endif

      for ( ;; )  /* used to minimize return code areas, just break out... */
      {
         rc = relate4topInit( relate ) ;
         if ( rc != 0 )
            break ;

         relate4setNotRead( relate ) ;

         relation->currentRelateLevel = 0 ;
         relate4prevRelationList( relation, 1 ) ;

         if ( relation->inSort == relate4sortDone )
         {
            if ( relate4sortGetRecord( relation, relation->sortRecCount, 0, 1, 0 ) == r4eof )
            {
               rc = r4eof ;
               break ;
            }
            else
               relation->sortRecOn = relation->sortRecCount ;
         }
         else
         {
            rc = d4bottom( relate->data ) ;
            if ( rc != 0 )
               break ;
            if ( relation->exprSource )
            {
               rec = d4recNo( relate->data ) ;
               #ifndef S4OFF_INDEX
                  if ( relate->dataTag )
                  {
                     while ( f4flagIsSetFlip( &relate->set, (unsigned long)rec ) == 0 )
                     {
                        #ifdef S4HAS_DESCENDING
                           rc = (int)tfile4dskip( relate->dataTag->tagFile, -1L ) ;
                        #else
                           rc = (int)tfile4skip( relate->dataTag->tagFile, -1L ) ;
                        #endif
                        if ( rc != -1 )
                        {
                           if ( rc == 0 )
                              rc = r4eof ;
                           break ;
                        }
                        rec = tfile4recNo( relate->dataTag->tagFile ) ;
                     }
                     if ( rc == r4eof )
                        break ;
                  }
                  else
                  {
               #endif

               if ( f4flagIsSetFlip( &relate->set, (unsigned long)rec ) == 0 )
               {
                  rec -= f4flagGetNextFlip( &relate->set, (unsigned long)d4recNo( relate->data), (char)-1 ) ;
                  if ( rec <= 0 )
                  {
                     rc = r4eof ;
                     break ;
                  }
               }
               #ifndef S4OFF_INDEX
                  }
               #endif
               rc = d4go( relate->data, rec ) ;
               if ( rc < 0 )
                  break ;
            }
            relate4setNotRead( relate ) ;
         }
         rc = relate4readRest( relate, -1 ) ;
         if ( rc == relate4filterRecord )
            rc = relate4skipInternal( relate, -1L ) ;

         if ( rc < 0 || rc == r4terminate )
            break ;

         if ( relation->exprSource )
         {
            rc2 = log4true( &relation->log ) ;
            if ( rc2 == r4terminate )
            {
               rc = r4terminate ;
               break ;
            }
            if ( rc2 == 0 )
            {
               if ( relation->inSort == relate4sortSkip )  /* must temporarily disable in order to get a matching scan if available */
               {
                  relation->inSort = 0 ;
                  rc = relate4skipInternal( relate, -1L ) ;
                  relation->inSort = relate4sortSkip ;
               }
               else
                  rc = relate4skipInternal( relate, -1L ) ;
            }
         }

         // AS 01/28/00 - not sufficient to just change 'rc', since then calling relate4eof() returns false.
         // must also move data file to eof position if not in a sort (sort uses sort flag)
         if ( rc == r4bof )
         {
            rc = r4eof ;
            if ( relate->relation->inSort != relate4sortDone )
               d4goEof( relate->relation->relate.data ) ;
         }

         break ;
      }

      #ifndef S4OFF_MULTI
         c4setReadLock( c4, oldReadLock ) ;
      #endif
      return rc ;
   #endif
}



#ifndef S4CLIENT
   static int relate4buildScanList( RELATE4 *master, RELATE4 *relate, RELATION4 *relation )
   {
      RELATE4 *relateOn ;
      RELATE4LIST *ptr ;

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      for( relateOn = 0 ;; )
      {
         relateOn = (RELATE4 *)l4next( &relate->slaves, relateOn ) ;
         if ( relateOn == 0 )
            break ;
         #ifndef S4OFF_INDEX   // CS 2007/01/30
            // AS Nov 19/03 - Although not strictly required according to the API, it would
            // be useful if at this point we version checked the indexes for the slave.  This
            // would mean that the indexes would at least be up to date to the point of the
            // relate4top() call (in case another user had added or changed records since).
            if ( relateOn->dataTag != 0 )
               t4versionCheck( relateOn->dataTag, 0, 0 ) ;
         #endif
         int rc = relate4buildScanList( relate, relateOn, relation ) ;
         if ( rc < 0 )
            return rc ;
      }

      if ( master != 0 )  /* cannot have a scan without a base master */
      {
         if ( relate->relationType == relate4scan || relate == &relation->relate )
         {
            ptr = (RELATE4LIST *)mem4createAllocZero( relate->codeBase, &relate->codeBase->relateListMemory, 5, sizeof(RELATE4LIST), 5, 0 ) ;
            if ( ptr == 0 )
               return e4memory ;
            ptr->ptr = relate ;
            l4add( &master->relateList, ptr ) ;
         }
      }

      return 0 ;
   }
#endif



static void relate4freeRelateList( RELATE4 *relate )
{
   RELATE4 *slaveOn ;
   void *ptr ;

   for( ;; )
   {
      ptr = l4pop( &relate->relateList ) ;
      if ( ptr == 0 )
         break ;
      mem4free( relate->codeBase->relateListMemory, ptr ) ;
   }

   for ( slaveOn = 0 ;; )
   {
      slaveOn = (RELATE4 *)l4next( &relate->slaves, slaveOn ) ;
      if ( slaveOn == 0 )
         return ;
      relate4freeRelateList( slaveOn ) ;
   }

}



int S4FUNCTION relate4changed( RELATE4 *relate )
{
   RELATION4 *relation ;
   CODE4 *c4 ;
   #ifndef S4CLIENT
      int j ;
   #endif

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94402 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94401 ) )
         return -1 ;
   #endif

   c4 = relate->codeBase ;
   if ( error4code( c4 ) < 0 )
      return -1 ;

   #ifndef S4CLIENT
      u4free( relate->scanValue ) ;
      relate->scanValue = 0 ;
   #endif
   relation = relate->relation ;

   #ifdef S4CLIENT
      if ( relation->isInitialized != 0 )
         relation->needsFreeing = 1 ;
      relation4readBufferReset( relation, 1 ) ;
      // AS Aug 6/02 - Ensure that server frees up info as well...
      relate4freeServer( relate ) ;
   #endif

   relation->isInitialized = 0 ;
   // AS Jul 01/02 - relate4count() support, must recalculate count if relate4changed called...
   relate->relation->countAccurate = 0 ;
   relate4sortFree( relation, 0 ) ;

   relate4freeRelateList( &(relation->relate) ) ;

   #ifndef S4CLIENT
      u4free( relation->relate.set.flags ) ;
      c4memset( (void *)&relation->relate.set, 0, sizeof( F4FLAG ) ) ;

      if ( relation->log.expr != 0 )
      {
         for( j = relation->log.expr->infoN; --j >= 0; )
         {
            if ( relation->log.infoReport != 0 )  // AS Nov 4/09 - if the expression was not logical type it is possible that this didn't get set up
            {
               E4INFO_REPORT *info_ptr = relation->log.infoReport + j ;
               // AS Sept 10/01 - info_ptr is occasionally null, so consider this instance
               if ( info_ptr != 0 && info_ptr->relateDataList != 0 )
               {
                  u4free( (info_ptr->relateDataList->pointers) ) ;
                  mem4free( c4->dataListMemory, info_ptr->relateDataList ) ;
               }
            }
         }

         expr4free( relation->log.expr ) ;
         relation->log.expr = 0 ;
         u4free( relation->log.infoReport ) ;
         relation->log.infoReport = 0 ;
      }
      if ( relation->log.bufLen != 0 )
      {
         u4free( relation->log.buf ) ;
         relation->log.bufLen = 0 ;
         relation->log.bufPos = 0 ;
      }
      relation->inSort = 0 ;

      // AS Jun 24/02 - New functionality to skip forward in the master table only
      // relation->masterSkipStatus = 0 ;  // indicate we need to reanalyze this if to use it
   #endif

   return 0 ;
}



#ifdef S4WIN32
   // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
   #ifdef RELATE4LOG
      static void relate4log( RELATE4 *relate, const char *out )
      {
         CODE4 *c4 = relate->codeBase ;
         file4writeInternal( &c4->relateLogFile, file4lenLow( &c4->relateLogFile ), out, strlen( out ) + 1 ) ;
      }
   #endif



   // AS Nov 2/05 - new callback functionality - to adjust the query prior to it actually being
   int S4FUNCTION relate4querySetCallbackInit( RELATE4 *relate, QUERY_SET_CALLBACK callback )
   {
      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm, E94402 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94403 ) )
            return -1 ;
      #endif

      relate->querySetCallback = callback ;
      return 0 ;
   }



   int S4FUNCTION relate4querySetCallbackInitUndo( RELATE4 *relate )
   {
      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94402 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94403 ) )
            return -1 ;
      #endif

      relate->querySetCallback = 0 ;
      return 0 ;
   }



   // AS Apr 18/05 - new callback functionality - currently only for windows 32 bit...
   int S4FUNCTION relate4callbackInit( RELATE4 *relate, QUERY_CALLBACK callback, long numMicroseconds )
   {
      #ifdef E4PARM_HIGH
         if ( relate == 0 || numMicroseconds < 0 )
            return error4( 0, e4parm, E94402 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94403 ) )
            return -1 ;
      #endif

      relate->callback = callback ;
      relate->callbackMicroseconds = numMicroseconds ;
      // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
      #ifdef RELATE4LOG
         char buf[160] ;
         sprintf( buf, "relate4callbackInit: numMicrosecionds: %ld query: %100s\n", numMicroseconds, relate->relation->exprSource ) ;
         relate4log( relate, buf ) ;
      #endif
      return 0 ;
   }



   int S4FUNCTION relate4callbackInitUndo( RELATE4 *relate )
   {
      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94402 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94403 ) )
            return -1 ;
      #endif

      relate->callback = 0 ;
      // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
      #ifdef RELATE4LOG
         char buf[160] ;
         sprintf( buf, "relate4callbackInitUndo: query: %100s\n", relate->relation->exprSource ) ;
         relate4log( relate, buf ) ;
      #endif
      return 0 ;
   }
#endif


RELATE4 *S4FUNCTION relate4createSlave( RELATE4 *master, DATA4 *slaveData, const char *masterExpr, TAG4 *slaveTag )
{
   RELATION4 *relation ;
   RELATE4 *slave ;
   CODE4 *c4 ;
   int rc ;
   #ifdef S4SERVER
      LIST4 *oldList ;
   #endif

   if ( master == 0 )
      return 0 ;

   #ifdef E4VBASIC
      if ( c4parm_check( master, 5, E94403 ) )
         return (RELATE4 *) 0 ;
      // AS Jul 30/02 - Let's also make sure the slaveData is valid...
      if ( c4parm_check( slaveData, 2, E94403 ) )
         return (RELATE4 *) 0 ;
   #endif

   c4 = master->codeBase ;

   if ( error4code( c4 ) < 0 )
      return 0 ;

   #ifdef E4PARM_LOW
      if ( slaveData == 0 || masterExpr == 0 )
      {
         error4( c4, e4parm_null, E94403 ) ;
         return 0 ;
      }
   #endif

   relation = master->relation ;

   /* check that the d4 doesn't belong to any existing relation */
   if ( relate4lookupRelate( &relation->relate, slaveData ) != 0 )
   {
      error4( c4, e4parm, E84403 ) ;
      return 0 ;
   }

   relate4changed( master ) ;

   slave = (RELATE4 *)mem4createAllocZero( c4, &c4->relateMemory, 5, sizeof(RELATE4), 5, 0 ) ;
   if ( slave == 0 )
      return 0 ;

   #ifdef S4SERVER
      rc = relate4initRelate( slave, relation, slaveData, c4, 0, 0 ) ;
   #else
      rc = relate4initRelate( slave, relation, slaveData, c4, 1 ) ;
   #endif
   if ( rc < 0 )
   {
      mem4free( c4->relateMemory, slave ) ;
      return 0 ;
   }
   #ifdef S4SERVER
      oldList = tran4dataList( code4trans( c4 ) ) ;
      tran4dataListSet( code4trans( c4 ), &relation->localDataList ) ;
   #endif
   slave->masterExpr = expr4parseLow( master->data, masterExpr, 0 ) ;
   #ifdef S4SERVER
      tran4dataListSet( code4trans( c4 ), oldList ) ;
   #endif
   if ( slave->masterExpr == 0 )
   {
      mem4free( c4->relateMemory, slave ) ;
      return 0 ;
   }

    // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
    /*
   #ifdef S4VFP_KEY
      if ( slaveTag != 0 )
      {
         slave->masterExpr->vfpInfo = &slaveTag->tagFile->vfpInfo ;
      }
   #endif
   */

   #ifndef S4CLIENT
      #ifndef S4OFF_INDEX
         if ( slaveTag != 0 )
            if ( tfile4type( slaveTag->tagFile ) != expr4type( slave->masterExpr ) )
            #ifdef S4CLIPPER
               if ( !( tfile4type( slaveTag->tagFile ) == r4num && expr4type( slave->masterExpr ) == r4numDoub ) )
                  if ( !( tfile4type( slaveTag->tagFile ) == r4numDoub && expr4type( slave->masterExpr ) == r4num ) )
            #endif
               {
                  #ifndef S4SERVER
                     error4( c4, e4relate, E84404 ) ;
                  #endif
                  mem4free( c4->relateMemory, slave ) ;
                  return 0 ;
               }
      #endif
   #endif

   slave->dataTag = slaveTag ;
   slave->master = master ;

   l4add( &master->slaves, slave ) ;
   relate4matchLen( slave, -1 ) ; /* Set to maximum */

   return slave ;
}



static int relate4dbfInRelation( RELATE4 *relate, const DATA4 *dbf )
{
   RELATE4 *relateOn ;

   relateOn = &relate->relation->relate ;
   while( relateOn->master )
      relateOn = relateOn->master ;

   do
   {
      if ( relateOn->data == dbf )
         return 1 ;
   } while( relate4next( &relateOn ) != 2 ) ;

   return 0 ;
}



#ifdef S4CLIENT
   /* S4CLIENT */
   int S4FUNCTION relate4doAll( RELATE4 *relate )
   {
      CONNECTION4RELATE_DO_INFO_IN *info ;
      CONNECTION4 *connection ;
      int rc, rc2 ;
      CODE4 *c4 ;
      RELATION4 *relation ;

      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94404 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94404 ) )
            return -1 ;
      #endif

      c4 = relate->codeBase ;
      relation = relate->relation ;

      #ifdef E4PARM_HIGH
         if ( relate->master != 0 )
            return error4( c4, e4parm, E84402 ) ;
      #endif

      if ( relation->isInitialized == 0 )  /* need to initialize on server first */
      {
         rc = relate4clientInit( relate ) ;
         if ( rc != 0 )
            return rc ;
      }

      connection = relate->data->dataFile->connection ;
      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( c4, e4struct, E94404 ) ;
      #endif

      rc = relate4flush( relate ) ;
      if ( rc )
         return rc ;

      connection4assign( connection, CON4RELATE_DO, 0, 0 ) ;
      connection4addData( connection, 0, sizeof( CONNECTION4RELATE_DO_INFO_IN ), (void **)&info ) ;
      info->relationId = htonl5(relation->relationId) ;
      info->relateId = htons5(relate->id) ;
      info->masterStartPos = htonl5(d4recNo( relate->data )) ;
      // AS May 21/02 - code to support auto-transfer of memo fields
      assert5port( "added optional transfer of memo fields with client/server communications" ) ;
      info->includeMemos = relation->includeMemos ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E94404 ) ;
      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         // AS Aug 6/02 - Ensure that server frees up info as well...
         relate4freeServer( relate ) ;
         relation->isInitialized = 0 ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E94404 ) ;
      }
      rc2 = relation4unpack( relation, relate->data->dataFile->connection ) ;
      if ( rc2 < 0 )
         return error4stack( c4, rc, E94404 ) ;
      if (  rc == r4terminate )
      {
         // AS Aug 6/02 - Ensure that server frees up info as well...
         relate4freeServer( relate ) ;
         relation->isInitialized = 0 ;
      }
      return rc ;
   }



   /* S4CLIENT */
   int S4FUNCTION relate4doOne( RELATE4 *relate )
   {
      CONNECTION4RELATE_DO_ONE_INFO_IN *info ;
      CONNECTION4RELATE_DO_ONE_INFO_OUT *out ;
      CONNECTION4 *connection ;
      int rc, saveRc ;
      CODE4 *c4 ;

      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94405 ) ;
         if ( relate->master == 0 )
            return error4( relate->codeBase, e4parm, E84405 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94405 ) )
            return -1 ;
      #endif

      c4 = relate->codeBase ;

      if ( relate->relation->isInitialized == 0 )  /* need to initialize on server first */
      {
         rc = relate4clientInit( &relate->relation->relate ) ;
         if ( rc != 0 )
            return rc ;
      }

      connection = relate->data->dataFile->connection ;
      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( 0, e4struct, E94405 ) ;
      #endif

      rc = relate4flush( relate ) ;
      if ( rc )
         return rc ;

      connection4assign( connection, CON4RELATE_DO_ONE, 0, 0 ) ;
      connection4addData( connection, 0, sizeof( CONNECTION4RELATE_DO_ONE_INFO_IN ), (void **)&info ) ;
      info->relationId = htonl5(relate->relation->relationId) ;
      info->relateId = htons5(relate->id) ;
      info->masterStartPos = htonl5(d4recNo( relate->master->data )) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return rc ;
      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E94405 ) ;

      saveRc = rc ;
      if ( saveRc != r4terminate )
      {
         if ( saveRc == r4eof )   /* end of file */
         {
            saveRc = 0 ;
            rc = d4goEof( relate->data ) ;
            if ( rc < 0 )
               return error4stack( c4, rc, E94405 ) ;
         }
         else
         {
            if ( connection4len( connection ) != sizeof( CONNECTION4RELATE_DO_ONE_INFO_OUT ) )
               return error4stack( c4, e4packetLen, E94405 ) ;

            out = (CONNECTION4RELATE_DO_ONE_INFO_OUT *)connection4data( connection ) ;

            rc = d4go( relate->data, ntohl5(out->recNo) ) ;
            if ( rc < 0 )
               return error4stack( c4, rc, E94405 ) ;
         }
      }
      return saveRc ;
   }



#else
   /* !S4CLIENT */
   // AS Jun 28/02 - for internal purposes with relate4skipMaster, allow relate4filterRecord return
   static int relate4doAllLow( RELATE4 *relate, Bool5 allowFilter )
   {
      int rc ;
      CODE4 *c4 ;
      #ifndef S4OFF_MULTI
         char oldReadLock ;
      #endif

      c4 = relate->codeBase ;

      #ifdef E4ANALYZE
         if ( relate->master != 0 )
            return error4( c4, e4struct, E84402 ) ;
      #endif

      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifndef S4OFF_MULTI
         oldReadLock = c4getReadLock( c4 ) ;
         c4setReadLock( c4, 0 ) ;
      #endif
      relate4setNotRead( relate ) ;
      rc = relate4readRest( relate, 0 ) ;
      #ifndef S4OFF_MULTI
         c4setReadLock( c4, oldReadLock ) ;
      #endif

      #ifndef S4LUPACH  /* LY July 7/03 */
         #ifndef S4MACINTOSH  // LY Jul 20/04
            assert5port( "client/server support for retaining a relation" ) ;
         #endif
         // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
         RELATION4 *relation = relate->relation ;
         #ifndef S4MACINTOSH  // LY Jul 20/04
            if ( relation->retainRelation == 0 )
         #endif
         {
            /* AS Feb 5/98 --> just changing flag does not free up file handles and memory, causing failure to free up */
            /*   relate->relation->isInitialized = 0 ; */
            relate4changed( relate ) ;
         }
         #ifndef S4MACINTOSH  // LY Jul 20/04
            else
            {
               // we have a retained relation here.  If there is a sort involved, then we
               // need to re-find the location in the sort...
               if ( rc == 0 && relation->sortSource != 0 )
               {
                  if ( relation->isInitialized == 0 )  // means relate4top or relate4bottom were never called, this is an error
                     return error4( c4, e4relate, E84406 ) ;
                  rc = relate4sortSeek( relation ) ;
               }
            }
         #endif
      #endif

      if ( rc == relate4filterRecord && allowFilter == 0 )  /* no match is an error */
      {
         #ifndef S4SERVER
            if ( c4->errRelate )
               return error4describe( c4, e4lookupErr, E94404, relate->data->alias, 0, 0 ) ;
         #endif
         return r4terminate ;
      }

      return rc ;
   }



   /* !S4CLIENT */
   int S4FUNCTION relate4doAll( RELATE4 *relate )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94405 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94405 ) ;
      #endif

      return relate4doAllLow( relate, 0 ) ;
   }



   /* !S4CLIENT */
   int S4FUNCTION relate4doOne( RELATE4 *relate )
   {
      int rc ;
      CODE4 *c4 ;
      #ifndef S4OFF_MULTI
         char oldReadLock ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94405 ) )
            return -1 ;
      #endif
      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94405 ) ;
      #endif

      c4 = relate->codeBase ;

      #ifdef E4PARM_HIGH
         if ( relate->master == 0 )
            return error4( c4, e4parm, E84405 ) ;
      #endif

      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( relate->master == 0 )   /* no master, so we must be read */
         return 0 ;

      #ifndef S4OFF_MULTI
         oldReadLock = c4getReadLock( c4 ) ;
         c4setReadLock( c4, 0 ) ;
      #endif
      relate4setNotRead( relate ) ;
      rc = relate4lookup( relate, 0 ) ;
      #ifndef S4OFF_MULTI
         c4setReadLock( c4, oldReadLock ) ;
      #endif

      /* AS Feb 5/98 --> just changing flag does not free up file handles and memory, causing failure to free up */
      /*   relate->relation->isInitialized = 0 ; */
      relate4changed( relate ) ;

      relate->isRead = relate->master->isRead ;  /* we are read if master is read */
      if ( rc == relate4filterRecord )  /* no match is an error */
      {
         #ifndef S4SERVER
            if ( c4->errRelate )
               return error4describe( c4, e4relate, E94405, relate->data->alias, 0, 0 ) ;
         #endif
         return r4terminate ;
      }
      return rc ;
   }
#endif



int S4FUNCTION relate4eof( RELATE4 *relate )
{
   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94406 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94406 ) )
         return -1 ;
   #endif

   if ( relate->relation->isInitialized == 0 )
   {
      error4( relate->codeBase, e4relate, E84406 ) ;  // AS Aug 06/02 change error to e4reate not e4info
      return -1 ;
   }

   #ifndef S4CLIENT
      if ( relate->relation->inSort == relate4sortDone )
         return relate->relation->sortEofFlag ;
      else
   #endif
      return d4eof( relate->relation->relate.data ) ;
}



int S4FUNCTION relate4errorAction( RELATE4 *relate, const int code )
{
   int rc ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94407 ) ;
      if ( code != relate4blank && code != relate4skipRec && code != relate4terminate )
         return error4( relate->codeBase, e4parm, E84407 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94401 ) )
         return -1 ;
   #endif

   rc = relate->errorAction ;
   // AS July 3/02 - if the error action changes, mark the relation as changed, otherwise sorts etc. may be incorrect
   if ( rc != code )
   {
      relate->errorAction = code ;
      relate4changed( relate ) ;
   }
   return rc ;
}



int S4FUNCTION relate4freeRelate( RELATE4 *relate, const int closeFiles )
{
   int rc ;
   RELATE4 *relateOn ;
   CODE4 *c4 ;
   #ifdef S4SERVER
      LIST4 *oldList ;
   #endif
   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94408 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94408 ) )
         return -1 ;
   #endif

   rc = 0 ;
   if ( relate->master == 0 )
      return relate4free( relate, closeFiles ) ;

   c4 = relate->codeBase ;

   relate4changed( relate ) ;

   if( closeFiles )
   {
      #ifdef S4SERVER
         oldList = tran4dataList( code4trans( c4 ) ) ;
         tran4dataListSet( code4trans( c4 ), &relate->relation->localDataList ) ;
      #endif
      if( d4close( relate->data ) < 0 )
         rc = -1 ;
      #ifdef S4SERVER
         tran4dataListSet( code4trans( c4 ), oldList ) ;
      #endif
      relate->data = 0 ;
   }

   for( ;; )
   {
      relateOn = (RELATE4 *)l4last( &relate->slaves) ;
      if ( relateOn == 0 )
         break ;
      if( relate4freeRelate( relateOn, closeFiles ) < 0 )
         rc = -1 ;
   }

   if ( relate->masterExpr )
      expr4free( relate->masterExpr ) ;
   u4free( relate->scanValue ) ;
   relate->scanValue = 0 ;
   #ifndef S4CLIENT
      u4free( relate->set.flags ) ;
      relate->set.flags = 0 ;
   #endif

   l4remove( &relate->master->slaves, relate ) ;
   mem4free( c4->relateMemory, relate ) ;
   relate = 0 ;

   return rc ;
}



#ifdef S4CLIENT
   // AS Aug 6/02 - Need functionality to uninitialize relate on server without actually freeing everything
   // on the client side, to avoid memory leakage on server...
   static int relate4freeServer( RELATE4 *relate )
   {
      assert5port( "client/server function added to free data on server in a more organized fashion" ) ;
      CONNECTION4RELATE_FREE_INFO_IN *info ;
      CONNECTION4 *connection ;
      RELATION4 *relation = relate->relation ;
      relate = &relation->relate ;
      // AS Mar 4/04 - if client closes table before calling relate4free (or before code4initUndo calls it), may get a gpf here.
      if ( relation->relationId != 0 && relate->data != 0 && relate->data->dataFile != 0 )   /* if not freed on the server */
      {
         connection = relate->data->dataFile->connection ;
         #ifdef E4ANALYZE
            if ( connection == 0 )
               return error4( relate->codeBase, e4struct, E94408 ) ;
         #endif
         connection4assign( connection, CON4RELATE_FREE, 0, 0 ) ;
         connection4addData( connection, 0, sizeof( CONNECTION4RELATE_FREE_INFO_IN ), (void **)&info ) ;
         info->relationId = htonl5( relation->relationId ) ;
         connection4sendMessage( connection ) ;
         relation->relationId = 0 ;
         int rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return rc ;
         rc = connection4status( connection ) ;
         if ( rc != 0 )
            return connection4error( connection, relate->codeBase, rc, E94408 ) ;
      }

      return 0 ;
   }
#endif



int S4FUNCTION relate4free( RELATE4 *relate, const int closeFiles )
{
   int rc = 0 ;
   RELATE4 *relateOn ;
   CODE4 *c4 ;
   #ifdef S4SERVER
      LIST4 *oldList ;
   #endif
   int oldErrorCode ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94408 ) ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94408 ) )
         return -1 ;
   #endif

   c4 = relate->codeBase ;
   oldErrorCode = error4set(c4, 0 ) ;

   #if defined( S4CB51 ) && !defined( S4SINGLE )
      relate4unlock( relate ) ;
   #endif

   relate4changed( relate ) ;
   RELATION4 *relation = relate->relation ;
   relate = &relation->relate ;

   #ifdef S4CLIENT
      assert5( relation->readAdvanceBuf == 0 ) ;    // relate4changed should have freed it...
   #endif

   #ifndef S4SERVER
      // AS Apr 30/02 - Allow for auto-freeing of relations on code4initUndo
      // AS Jan 3/03 - Moved this up in function to ensure no endless loop in code4initUndo.
      l4remove( &c4->relations, relation ) ;
   #endif

   #ifdef S4CLIENT
      #ifdef E4ANALYZE
         if ( relate->data == 0 )
            return error4( c4, e4struct, E94408 ) ;
         if ( relate->data->dataFile == 0 )
            return error4( c4, e4struct, E94408 ) ;
      #endif

      rc = relate4freeServer( relate ) ;
      if ( rc < 0 )
         return rc ;
   #endif

   int closeMaster = closeFiles ;
   #ifdef S4SERVER
      if ( relate->freeData == 1 || closeFiles == 1 )  // also close the master data file if freeData is set to true in server
         closeMaster = 1 ;
   #endif

   if( closeMaster )
   {
      #ifdef S4SERVER
         oldList = tran4dataList( code4trans( c4 ) ) ;
         tran4dataListSet( code4trans( c4 ), &relation->localDataList ) ;
      #endif

      if( d4close( relate->data ) < 0 )
         rc = -1 ;

      #ifdef S4SERVER
         tran4dataListSet( code4trans( c4 ), oldList ) ;
      #endif
      relate->data = 0 ;
   }

   for( relateOn = 0 ;; )
   {
      relateOn = (RELATE4 *)l4last( &relate->slaves ) ;
      if ( relateOn == 0 )
         break ;
      if( relate4freeRelate( relateOn, closeFiles ) < 0 )
         rc = -1 ;
   }

   relate4sortFree( relation, 1 ) ;
   u4free( relation->exprSource ) ;
   #ifndef S4CLIENT
      if ( relation->sortedKeyBufferLen != 0 )
      {
         u4free( relation->sortedKeyBuffer2 ) ;
         relation->sortedKeyBuffer2 = 0 ;
         relation->sortedKeyBufferLen = 0 ;
      }
   #endif

   mem4free( c4->relationMemory, relation ) ;

   error4set(c4, oldErrorCode) ;
   return rc ;
}



#ifdef S4SERVER
   static DATA4 *relate4dataOpen( RELATE4 *relate, DATA4 *oldData )
   {
      DATA4 *d4 ;
      int oldAccessMode, oldReadOnly ;
      CODE4 *c4 ;

      #ifdef E4PARM_LOW
         if ( relate == 0 || oldData == 0 )
         {
            error4( 0, e4parm_null, E94427 ) ;
            return 0 ;
         }
      #endif

      c4 = relate->codeBase ;

      relate->dataOld = oldData ;

      oldAccessMode = c4->accessMode ;
      c4->accessMode = OPEN4DENY_NONE ;
      oldReadOnly = c4getReadOnly( c4 ) ;
      c4setReadOnly( c4, 1 ) ;
      // we should not be logging these opens since data file contents cannot be changed
      // and we don't want to have to track these back for closing... (or with compressed
      // log file)
      #ifndef S4OFF_TRAN
         int oldStatus = code4tranStatus( c4 ) ;
         code4tranStatusSet( c4, r4off ) ;
      #endif
      d4 = d4openClone( oldData ) ;
      #ifndef S4OFF_TRAN
         code4tranStatusSet( c4, oldStatus ) ;
      #endif
      if ( d4 == 0 )
      {
         error4( c4, e4data, E94409 ) ;
         return 0 ;
      }
      // AS Nov 3/08 - moved to after check in case of failure to open
      // AS Aug 9/02 - Need to have the capability to determine the clientId and serverId of the
      // source DATA4 for the server's relation DATA4 module.  In particular, we want to allow
      // for record counts to be accessible for clients who have a transaction in progress and
      // then use a relation to access those fields.
      d4->relationsSourceClientId = oldData->clientId ;
      d4->relationsSourceServerId = oldData->serverId ;
      #ifdef S4SERVER
         d4->trans = &c4->currentClient->trans ;
      #endif
      c4->accessMode = oldAccessMode ;
      c4setReadOnly( c4, oldReadOnly ) ;

      return d4 ;
   }
#endif /* S4SERVER */



#ifdef S4SERVER
   RELATE4 *S4FUNCTION relate4init( DATA4 *master, char *masterAliasName )
#else
   RELATE4 *S4FUNCTION relate4init( DATA4 *master )
#endif
{
   RELATION4 *relation ;
   CODE4 *c4 ;
   int rc ;

   #ifdef E4VBASIC
      if ( c4parm_check( master, 2, E94410 ) )
         return (RELATE4 *) 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( master == 0 )
      {
         error4( 0, e4parm_null, E94410 ) ;
         return 0 ;
      }
   #endif

   c4 = master->codeBase ;
   if ( error4code( c4 ) < 0 )
      return 0 ;

   if ( c4->relationMemory == 0 )
   {
      c4->relationMemory = mem4create( c4, 5, sizeof( RELATION4 ), 5, 0 ) ;
      if ( c4->relationMemory == 0 )
         return 0 ;
   }
   relation = (RELATION4 *)mem4allocZero( c4->relationMemory ) ;
   if ( relation == 0 )
      return 0 ;

   #ifndef S4CLIENT
      relation->log.relation = relation ;
      relation->log.codeBase = c4 ;
      relation->sort.file.hand = INVALID4HANDLE ;
      relation->sortedFile.hand = INVALID4HANDLE ;
   #endif

   #ifdef S4SERVER
      rc = relate4initRelate( &relation->relate, relation, master, c4, 1, masterAliasName ) ;
   #else
      rc = relate4initRelate( &relation->relate, relation, master, c4, 1 ) ;
   #endif
   if ( rc < 0 )
   {
      mem4free( c4->relationMemory, relation ) ;
      return 0 ;
   }

   #ifdef S4SERVER
      relation->relate.freeData = 1 ;
   #endif

   #ifndef S4SERVER
      // AS Apr 30/02 - Allow for auto-freeing of relations on code4initUndo
      l4add( &c4->relations, relation ) ;
   #endif

   return &relation->relate ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
#ifdef S4SERVER
   static int relate4initRelate( RELATE4 *relate, RELATION4 *relation, DATA4 *data, CODE4 *c4, int doOpen, char *dataAliasName )
#else
   static int relate4initRelate( RELATE4 *relate, RELATION4 *relation, DATA4 *data, CODE4 *c4, int doOpen )
#endif
{
   #ifdef S4SERVER
      LIST4 *oldList ;
   #endif

   relate->codeBase = c4 ;
   relate->relationType = relate4exact ;
   #ifdef E4ANALYZE
      if ( relate->data != 0 )
         error4( relate->codeBase, e4relate, E84406 ) ;
   #endif

   #ifdef S4SERVER
      if ( doOpen == 0 )
         relate->data = data ;
      else
      {
         oldList = tran4dataList( code4trans( c4 ) ) ;
         tran4dataListSet( code4trans( c4 ), &relation->localDataList ) ;
         relate->data = relate4dataOpen( relate, data ) ;
         // AS July 17/02 - relate->data may return null...
         if ( relate->data != 0 &&  dataAliasName != 0 )
            d4aliasSet( relate->data, dataAliasName ) ;
         tran4dataListSet( code4trans( c4 ), oldList ) ;
      }
   #else
      relate->data = data ;
   #endif
   relate->errorAction = relate4blank ;
   relate->relation = relation ;
   if ( relate->data == 0 )
      return error4( c4, e4data, E94410 ) ;

   relate->data->count = d4recCount( relate->data ) ;

   return 0 ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION relate4lockAdd( RELATE4 *relate )
{
   #ifdef S4OFF_MULTI
      return 0 ;
   #else
      int rc ;
      RELATE4 *relateOn ;

      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94411 ) ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94411 ) )
            return -1 ;
      #endif

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      rc = 0 ;
      for( relateOn = &relate->relation->relate ;; )
      {
         if ( relateOn == 0 )
            break ;
         rc = d4lockAddFile( relateOn->data ) ;
         if ( rc != 0 )
            break ;
         if ( relate4next( &relateOn ) == 2 )
            break ;
      }

      return rc ;
   #endif
}



#ifdef S4CB51
/* S4CB51 */
int S4FUNCTION relate4lock( RELATE4 *relate )
{
   /* new form does not allow combining lockAdds spaced around relate4locks()
      if required to do that, S4OLD_RELATE_LOCK code will allow */
   int rc ;

   rc = relate4lockAdd( relate ) ;
   if ( rc < 0 )
      return rc ;
   rc = code4lock( relate->codeBase ) ;
   if ( rc == 0 )
      relate->relation->locked = 1 ;
   return rc ;
}

#endif /* S4CB51 */



#ifdef S4CLIENT
#ifdef S4CB51
#ifdef S4OLD_RELATE_LOCK

/* S4OLD_RELATE_LOCK, S4CB51, S4CLIENT */
int S4FUNCTION relate4lock( RELATE4 *relate )
{
   CONNECTION4 *connection ;
   CONNECTION4RELATE_LOCK_INFO_IN *info ;
   CONNECTION4RELATE_LOCK_INFO_OUT *out ;
   CONNECTION4RELATE_LOCK_SUB_DATA *subData ;
   int rc, count ;
   CODE4 *c4 ;
   DATA4 *data ;
   short countN ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94411 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94408 ) )
         return -1 ;
   #endif

   c4 = relate->codeBase ;

   /* must ensure that client relation is registered before requesting a lock */
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
      {
         error4( c4, e4struct, E94411 ) ;
         return 0 ;
      }
   #endif

   connection4assign( connection, CON4RELATE_LOCK, 0, 0 ) ;
   connection4addData( connection, 0, sizeof( CONNECTION4RELATE_LOCK_INFO_IN ), &info ) ;
   info->relationId = htonl5(relate->relation->relationId) ;
   rc = connection4repeat( connection, -2, -1, -1, 0 ) ;
   if ( rc == r4locked )
      return r4locked ;
   if ( rc < 0 )
      return connection4error( connection, c4, rc, E94411 ) ;

   if ( rc == 0 )  /* add locks */
   {
      if ( connection4len( connection ) < sizeof(CONNECTION4RELATE_LOCK_INFO_OUT) )
         return error4( c4, e4packetLen, E94411 ) ;
      out = (CONNECTION4RELATE_LOCK_INFO_OUT *)connection4data( connection ) ;
      countN = ntohs5(out->count) ;
      if ( connection4len( connection ) != (long)sizeof(CONNECTION4RELATE_LOCK_INFO_OUT) + countN * sizeof( CONNECTION4RELATE_LOCK_SUB_DATA ) )
         return error4( c4, e4packetLen, E94411 ) ;
      subData = (CONNECTION4RELATE_LOCK_SUB_DATA *)( (char *)connection4data( connection ) + sizeof( CONNECTION4RELATE_LOCK_INFO_OUT ) ) ;
      for( count = countN ; count > 0 ; count-- )
      {
         data = tran4data( code4trans( c4 ), ntohl5(subData->serverId), ntohl5(subData->clientId) ) ;
         if ( data == 0 )
            return error4( c4, e4relate, E94411 ) ;
         if ( data->dataFile->fileLock != 0 )
         {
            if ( data->dataFile->fileLock != data )
               return error4( c4, e4info, E94411 ) ;
         }
         else
            data->dataFile->fileLock = data ;
         subData++ ;
      }
   }

   return rc ;
}

#endif /* S4OLD_RELATE_LOCK */
#endif /* S4CB51 */

#else /* S4CLIENT else */



#ifdef S4OLD_RELATE_LOCK

/* S4OLD_RELATE_LOCK, not S4CLIENT */
int S4FUNCTION relate4lock( RELATE4 *relate )
{
   #ifdef S4SINGLE
      return 0 ;
   #else
      CODE4 *c4 ;
      int rc, oldAttempts, count ;
      RELATE4 *relateOn ;
      #ifdef S4SERVER
         RELATE4 *relateSkip ;
         DATA4 *dataTemp ;
      #endif

      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94411 ) ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94411 ) )
            return -1 ;
      #endif

      c4 = relate->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      relate->relation->locked = 1 ;

      count = oldAttempts = c4->lockAttempts ;  /* take care of wait here */
      c4->lockAttempts = 1 ;

      for( ;; )
      {
         rc = 0 ;
         for( relateOn = &relate->relation->relate ;; )
         {
            if ( relateOn == 0 )
               break ;
            #ifdef S4SERVER
               if ( relateOn->dataOld->dataFile->fileServerLock == data4serverId( relateOn->dataOld ) )  /* check for duplicate lock */
               {
                  dataTemp = tran4data( code4trans( c4 ), relateOn->dataOld->dataFile->fileServerLock, relateOn->dataOld->dataFile->fileClientLock ) ;
                  if ( dataTemp == 0 )
                     rc = r4locked ;
                  else
                  {
                     /* check for data in relation via relate search */
                     for ( relateSkip = &relate->relation->relate ;; )
                     {
                        if ( relateSkip == 0 )
                        {
                           rc = r4locked ;
                           break ;
                        }
                        if ( relateSkip->dataOld == dataTemp )
                           break ;
                        if ( relate4next( &relateSkip ) == 2 )
                           break ;
                     }
                  }
               }
               else
                  // AS Apr 15/03 - support for new lockId for shared clone locking
                  rc = dlockAllInternal( relateOn->dataOld->dataFile, data4lockId( relateOn->dataOld ), data4serverId( relateOn->dataOld ), relateOn->dataOld, 0 ) ;
            #else
               rc = d4lockAllInternal( relateOn->data, 1 ) ;
            #endif
            if ( rc != 0 )
               break ;
            if ( relate4next( &relateOn ) == 2 )
               break ;
         }

         if ( rc != r4locked )
            break ;

         relate4unlock( relate ) ;
         if ( count == 0 )
            break ;

         if ( count > 0 )
            count-- ;

         #ifdef S4TEMP
            if ( d4displayQuit( &display ) )
            {
               rc = error4( c4, e4result, E84409 ) ;
               break ;
            }
         #endif

         u4delayHundredth( c4->lockDelay ) ;   /* wait a second & try lock again */
      }

      c4->lockAttempts = oldAttempts ;
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;

      return rc ;
   #endif
}

#endif  /* S4OLD_RELATE_LOCK */



/* not S4CLIENT */
/* LY 2001/07/21 : changed recno to S4LONG for 64-bit */
static int relate4lookupRecno( RELATE4 *relate, const signed char direction, S4LONG *recno )
{
   /* This function performs a lookup when the input relate does not have a data tag.  In this
      case the relate->masterExpr evaluates to a double which can be converted to a record
      number.

      The results of the analysis can be:

      < 0 - error occurred
      relate4filterRecord - the record was filtered from the set due to the query
      0 - successful lookup.
      r4entry - means the record was not found -- i.e. bof/eof condition - for an approx
                tag, this cannot occur - i.e. we can lookup approximately, so no eof/bof
                is possible.
   */
   #ifndef S4SERVER
      int oldGoError ;
   #endif

   int rc ;
   CODE4 *c4 = relate->codeBase ;
   *recno = (S4LONG)expr4double( relate->masterExpr ) ;  /* LY 2001/07/21 : changed to S4LONG for 64-bit */
   if ( error4code( c4 ) < 0 )
      return -1 ;

   if ( direction != 0 )
      if ( f4flagIsSetFlip( &relate->set, (unsigned long)(*recno) ) == 0 )
         return relate4filterRecord ;

   #ifndef S4SERVER
      oldGoError = c4->errGo ;
      c4->errGo = 0 ;
   #endif
   rc = d4go( relate->data, *recno ) ;
   #ifndef S4SERVER
      c4->errGo = oldGoError ;
      if ( rc < 0 )
         return error4stack( c4, rc, E94412 ) ;
   #endif
   if ( rc != r4entry )  /* if not error, then return */
      return 0 ;
   if ( relate->relationType == relate4approx )
   {
      d4goEof( relate->data ) ;
      return 0 ;
   }

   return r4entry ;
}



#ifndef S4OFF_INDEX

/* not S4OFF_INDEX, not S4CLIENT */
static int relate4updateScanValue( RELATE4 *slaveRelate, void *ptr, int len )
{
   /* updates the value being used for scanning.  This is done either when a lookup is
      done (we generate a new scan value with which to perform the lookup), or when
      we have multiple slaves in a scan relationship -- i.e. they may be scanning
      on different key values.

      The input 'slaveRelate' is the relate being scanned (i.e. the slave relate), not
      the master relater.
   */
   RELATE4 *master = slaveRelate->master ;

   if ( master->scanValue != 0 )
      if ( master->scanValueAllocLen < len )  // happens if a new expression of a different size
      {
         u4free( master->scanValue ) ;
         master->scanValue = 0 ;
         master->scanValueAllocLen = 0 ;
      }

   if ( master->scanValue == 0 )
   {
      master->scanValue = (char *)u4allocEr( slaveRelate->codeBase, (long)len ) ;
      if ( master->scanValue == 0 )
         return -1 ;
      master->scanValueAllocLen = len ;
   }

   c4memcpy( master->scanValue, ptr, (unsigned int)len ) ;
   master->scanValueLen = len ;

   return 0 ;
}



/* not S4OFF_INDEX, not S4CLIENT */
static int relate4evaluateMasterExpression( RELATE4 *relate, char **ptr )
{
   /* evaluates the master expression and returns the result in ptr and returns the
      len, which may be modified due to relate->matchLen

      returns <0 in case of error, else len.
    */

   // AS Oct 12/06 - ensure the context is set...
   #ifndef S4CLIENT
      expr4context( relate->masterExpr, relate->master->data ) ;
   #endif

   int len = expr4key( relate->masterExpr, ptr, relate->dataTag->tagFile ) ;
   if ( len < 0 )
      return -1 ;

   #ifdef S4FOX
      // AS Oct 1/02 - there is a potential null conflict here.  In particular, the expression we
      // used may or may not be nullable, and the tag we are seeking on may or may not be nullable.
      // we need these to be consistent, so modify the key as appropriate...
      // AS Nov 15/07 - also need to set the expression context for the Tag's expression...
      #ifndef S4CLIENT
         expr4context( relate->dataTag->tagFile->expr, relate->data ) ;
      #endif

      if ( expr4nullLow( relate->masterExpr, 0 ) )
      {
         // the master expression included a null...
         if ( expr4nullLow( relate->dataTag->tagFile->expr, 0 ) == 0 )
         {
            // if the key is actually null, set the key to search to 0x00000000 so we find bof
            if ( (*ptr)[0] == 0 )  // means is null...
            {
               memset( *ptr, len, 0 ) ;
            }
            else
            {
               // means the tag's expression is non-nullable
               // this means reduce the seek length by 1, and increment 1 past the null marker in the key
               (*ptr)++ ;
            }
            len-- ;  // don't seek on the null marker
         }
      }
      else
      {
         // the masterExpr is not nullable, what about the search tag?
         if ( expr4nullLow( relate->dataTag->tagFile->expr, 0 ) == 1 )
         {
            CODE4 *cb = relate->codeBase ;
            // here we need to insert a 0x80 non-null marker into the seek value
            // if our storedKeyLen is not large enough, expand it...
            if ( cb->storedKeyLen <= (unsigned)(len+1) )
            {
               u4allocAgain( cb, &cb->storedKey, &cb->storedKeyLen, (unsigned)len+1 ) ;
               if ( error4code( cb ) < 0 )
                  return -1 ;
               cb->storedKeyLen = len + 1 ;
            }
            memmove( cb->storedKey+1, (*ptr), len ) ;
            cb->storedKey[0] = (char)(0x80) ;  // since source cannot be null, we must always seek for non-null
            (*ptr) = cb->storedKey ;
            len++ ;
         }
      }
   #endif

   len = (len < relate->matchLen) ? len : relate->matchLen ;   /* min of len and match len */

   #ifdef E4ANALYZE
      if ( len < 0 )
         return error4( relate->codeBase, e4struct, E94413 ) ;
   #endif

   return len ;
}


// AS July 17/02 The following problem needed resolving.  If transaction processing is enabled,
// the record found by the tag may not be valid for the current DATA4 pointer (i.e. a pending
// append)
static void relate4tagFindLast( RELATE4 *relate, TAG4FILE *tagFile, const char *seekKey, int seekLen )
{
   // purpose is to skip in the tag until we find the last position matching the seek condition.
   // the assumption is that we are currently on the first matching position in the tag (so we
   // are just skipping forward)
   // the current position of the tag must be valid in the data set (i.e. not a pending record by
   // another user)
   // relate is the slave relate / current relate that we are doing the seek into (into its data4)
   // tagFile is the tag for skipping
   // seekKey is the key we were seeking
   // seekLen is the length of the key we were seeking

   #ifdef E4ANALYZE
      {
         S4LONG recno ;
         assert5( !tfile4eof( tagFile ) ) ;

         #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4rel41.c on HP*/
            memcpy( &recno, &tfile4keyData( tagFile )->num, sizeof(S4LONG) ) ;
         #else
            recno = tfile4keyData( tagFile )->num ;
         #endif
         // initially the record had to have been valid
         assert5( d4recCountLessEq( relate->data, recno ) ) ;
      }
      long count = 0 ;
   #endif

   for( ;; )
   {
      #ifdef S4HAS_DESCENDING
         if ( !tfile4dskip( tagFile, 1L ) )
      #else
         if ( !tfile4skip( tagFile, 1L ) )
      #endif
         {
            // if we couldn't skip, this is ok, means our position is already valid
            break ;
         }

      #ifdef E4ANALYZE
         count++ ;
      #endif

      // if the key no longer matches, we have gone forward 1 too far...
      #ifdef S4FOX
         if ( u4keycmp( tfile4keyData( tagFile )->value, seekKey, (unsigned int)seekLen, (unsigned int)relate->dataTag->tagFile->header.keyLen, 0, collation4get( tagFile->collateName ) ) )
      #else
         if ( u4memcmp( tfile4keyData( tagFile )->value, seekKey, (unsigned int)seekLen ) )
      #endif
      {
         // on the skip back, we need to take into account that the tag may contain an invalid record,
         // namely that due to pending transactions the tags recno may be invalid.  So, we need to
         // skip backwards until we reposition to a valid position.  We are guaranteed that the
         // initial input position was valid, so we will for sure find a valid spot.
         for( ;; )
         {
            #ifdef S4HAS_DESCENDING
               if ( tfile4dskip( tagFile, -1L ) == 0 )
            #else
               if ( tfile4skip( tagFile, -1L ) == 0 )
            #endif
               {
                  // this cannot actually happen, since it would mean we skipped back past our start position
                  assert5( 0 ) ;
                  return ;
               }

            S4LONG recno ;
            #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4rel41.c on HP*/
               memcpy( &recno, &tfile4keyData( tagFile )->num, sizeof(S4LONG) ) ;
            #else
               recno = tfile4keyData( tagFile )->num ;
            #endif
            // initially the record had to have been valid
            if ( d4recCountLessEq( relate->data, recno ) )  // the record is valid, so done
               return ;

            // if here, we have an invalid matching record, just keep going backwards.
            #ifdef E4ANALYZE
               count-- ;
               assert5( count >= 0 ) ;  // ensure we haven't gone back past our start position - this is impossible.
            #endif
         }
      }
   }

}



// AS July 17/02 The following problem needed resolving.  If transaction processing is enabled,
// the record found by the tag may not be valid for the current DATA4 pointer (i.e. a pending
// append)
static int relate4tagSeek( RELATE4 *relate, TAG4FILE *tagFile, const char *seekKey, int seekLen )
{
   // relate is the slave relate / current relate that we are doing the seek into (into its data4)
   // tagFile is the tag for seeking
   // seekKey is the key we are seeking
   // seekLen is the length of the key we are seeking
   int rc = tfile4seek( tagFile, seekKey, seekLen ) ;
   if ( rc != 0 && rc != r4after )  // if after or success, we may need to skip further to get an accurate record
      return rc ;

   if ( tfile4eof( tagFile ) )
      return r4eof ;

   S4LONG recno ;
   #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4rel41.c on HP*/
      memcpy( &recno, &tfile4keyData( tagFile )->num, sizeof(S4LONG) ) ;
   #else
      recno = tfile4keyData( tagFile )->num ;
   #endif

   if ( d4recCountLessEq( relate->data, recno ) )  // record is valid
      return rc ;

   int initialRc = rc ;

   // in this case, the record is not valid, so need to skip forward.  If our initial return was r4after we
   // don't need to do further equality checks, but if it was success, we need to ensure we are still valid
   // at some point.

   for( ;; )
   {
      #ifdef S4HAS_DESCENDING
         // if descending is supported, using tfile4skip, which takes the descending of the tag into account
         // (means if a descending tag, will actually skip descending) - allows for run-time changes of this flag.
         if ( !tfile4dskip( tagFile, 1L ) )
      #else
         if ( !tfile4skip( tagFile, 1L ) )
      #endif
         {
            // was not able to skip forward any more - means r4eof...
            return r4eof ;
         }

      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4rel41.c on HP*/
         memcpy( &recno, &tfile4keyData( tagFile )->num, sizeof(S4LONG) ) ;
      #else
         recno = tfile4keyData( tagFile )->num ;
      #endif

      if ( d4recCountLessEq( relate->data, recno ) )  // record is valid
      {
         if ( initialRc != 0 ) // if not success (i.e. r4after), just return r4after
            return rc ;
         // now we must see whether we still are at an equality position

         #ifdef S4FOX
            rc = u4keycmp( tfile4keyData( tagFile )->value, seekKey, (unsigned int)seekLen, (unsigned int)relate->dataTag->tagFile->header.keyLen, 0, collation4get( tagFile->collateName ) ) ;
         #else
            rc = u4memcmp( tfile4keyData( tagFile )->value, seekKey, (unsigned int)seekLen ) ;
         #endif
         if ( rc == 0 )
            return rc ;
         return r4after ;
      }
   }

   return -1 ;  // should never actually get here...
}



/* not S4OFF_INDEX, not S4CLIENT */
/* LY 2001/07/21 : changed recno to S4LONG for 64-bit */
static int relate4lookupTag( RELATE4 *relate, const signed char direction, S4LONG *recno )
{
   /*
      performs a lookup using the relates data tag.
      The results of the analysis can be:

      < 0 - error occurred
      relate4filterRecord - the record was filtered from the set due to the query
      0 - successful lookup.
      r4entry - means the record was not found -- i.e. bof/eof condition - for an approx
                tag, this cannot occur - i.e. we can lookup approximately, so no eof/bof
                is possible.
   */

   int rc ;
   TAG4FILE *tagFile = relate->dataTag->tagFile ;
   #ifndef S4OFF_INDEX
      char *ptr ;
   #endif

   // AS Jun 27/02 - There is somewhat undocumented behaviour here.  In particular the following
   // case:  <master> exact relation blank error <slave> scan relation skiprec error <slave2>.  Now, if the master/slave
   // evaulation fails (resulting in a blank composite record for the record for slave2), we
   // continue to evaulate the scan relation.  Because the slave record is at eof or blank, this
   // means we perform a match with a blank record onto slave2.  If there happens to be a match
   // with slave2 (slave2 has a blank or '0' for numerics), we would get a valid record.
   // I am leaving the code as it stands, just noting that the actual effects are not actually
   // documented in the documentation, although it does indicate that a 'blank composite' record
   // is created, so it could be implied that the behaviour is correct.

   int len = relate4evaluateMasterExpression( relate, &ptr ) ;
   if ( len < 0 )
      return -1 ;

   if ( relate->relationType == relate4scan )
   {
      #ifdef E4ANALYZE
         if ( relate->master == 0 )
            return error4( relate->codeBase, e4struct, E84410 ) ;  // CS 2001/04/20 remove "c4" valiable
      #endif
      if ( relate4updateScanValue( relate, ptr, len ) < 0 )
         return -1 ;
   }

   // AS July 17/02 The following problem needed resolving.  If transaction processing is enabled,
   // the record found by the tag may not be valid for the current DATA4 pointer (i.e. a pending
   // append)
   rc = relate4tagSeek( relate, tagFile, ptr, len ) ;
   if ( rc < 0 )
      return -1 ;

   if ( relate->relationType == relate4approx || rc == 0 )
   {
      if ( rc == r4eof )
      {
         d4goEof( relate->data ) ;
         return 0 ;
      }

      if ( direction < 0 && rc == 0 && relate->relationType == relate4scan )  /* look for last one */
         relate4tagFindLast( relate, tagFile, ptr, len ) ;

      #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4rel41.c on HP*/
         memcpy( recno, &tfile4keyData( tagFile )->num, sizeof(S4LONG) ) ;
      #else
         *recno = tfile4keyData( tagFile )->num ;
      #endif

      if ( direction != 0 )
      {
         if ( f4flagIsSetFlip( &relate->set, (unsigned long)(*recno) ) == 0 )
            return relate4filterRecord ;
      }

      // AS Jun 25/02 - don't actually perform the d4go if we can avoid it when counting
      if ( relate->relation->countOnly == 1 && l4numNodes( &relate->slaves ) == 0 )
      {
         // in thise case, we can just do nothing, since we are counting only and
         // there are no slaves in our set.
        // reset the bof/eof flags, if any
        // relate->data->bofFlag = 0 ;
        // relate->data->eofFlag = 0 ;
      }
      else
      {
         if ( d4go( relate->data, *recno ) < 0 )
            return -1 ;
      }
      return 0 ;
   }
   else
   {
      if ( rc == r4eof )
         *recno = 0 ;
      else
      {
         #ifdef S4DATA_ALIGN  /* LY 00/02/18 : HP-UX */
            memcpy( recno, &tfile4keyData( tagFile )->num, sizeof(S4LONG) ) ;
         #else
            *recno = tfile4keyData( tagFile )->num ;
         #endif
      }
   }

   return r4entry ;
}

#endif /* S4OFF_INDEX */



/* not S4CLIENT */
static int relate4lookupError( RELATE4 *relate, const signed char direction, long recno )
{
   /* This function handles the not-found case resulting from a failed lookup */
   RELATION4 *relation = relate->relation ;

   switch( relate->errorAction )  /* if got here, must be error condition */
   {
      case relate4blank:
         if ( d4goEof( relate->data ) < 0 )
            return -1 ;
         if ( direction != 0 )
         {
            if ( recno == 0 )
            {
               if ( relate->set.flags != 0 )
                  return relate4filterRecord ;
            }
            else
               if ( f4flagIsSetFlip( &relate->set, (unsigned long)recno ) == 0 )
                  return relate4filterRecord ;
         }
         return 0 ;
      case relate4skipRec:
         /* if a scan, and a failure, then, if the current relation set is
            below the current position, move it to above the current position */
         if ( relate->relationType == relate4scan )
         {
            if ( relate != &relation->relate )
               while ( relate4currentIsChild( relate ) )
                  relate4nextRelationList( relation, 0 ) ;
         }
         return relate4filterRecord ;
      case relate4terminate:
         #ifndef S4SERVER
            if ( relate->codeBase->errRelate )
               return error4describe( relate->codeBase, e4lookupErr, E94412, relate->data->alias, 0, 0 ) ;
         #endif
         return r4terminate ;
      default:  /* should never get this far */
         return error4( relate->codeBase, e4info, E84411 ) ;
   }
}



/* not S4CLIENT */
static int relate4lookup( RELATE4 *relate, const signed char direction )
{
   /* direction : -1 = look backwards, 0 = lookup only, 1 = look forwards */

   int rc ;
   S4LONG recno ;  /* LY 2001/07/21 : changed from long for 64-bit */
   CODE4 *c4 ;

   #ifdef E4PARM_LOW
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94412 ) ;
      if ( direction < -1 || direction > 1 )
         return error4( 0, e4parm, E94417 ) ;
   #endif

   c4 = relate->codeBase ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( direction != 0 && relate->relation->isInitialized == 0 )  // CS 2001/04/20 remove "relation" variable
      return error4( c4, e4relate, E84406 ) ;  // AS Aug 06/02 change error to e4reate not e4stuct

   relate->isRead = 1 ;
   if ( relate->master == 0 )
      return 0 ;

   #ifdef S4OFF_INDEX
      rc = relate4lookupRecno( relate, direction, &recno ) ;
      if ( rc != r4entry )
         return rc ;
   #else
      d4tagSelect( relate->data, relate->dataTag ) ;

      if ( relate->dataTag == 0 )
      {
         rc = relate4lookupRecno( relate, direction, &recno ) ;
         if ( rc != r4entry )
            return rc ;
      }
      else
      {
         rc = relate4lookupTag( relate, direction, &recno ) ;
         if ( rc != r4entry )
            return rc ;
      }
   #endif

   /* basically we have an r4entry - error condition (not found), so handle as appropriate */
   return relate4lookupError( relate, direction, recno ) ;
}

#endif /* S4CLIENT else */



RELATE4 *relate4lookupRelate( RELATE4 *relate, const DATA4 *d4 )
{
   RELATE4 *relateReturn, *relateOn ;

   if ( relate->data == d4 )
      return relate ;
   for( relateOn = 0 ;; )
   {
      relateOn = (RELATE4 *)l4next( &relate->slaves, relateOn) ;
      if ( relateOn == 0 )
         return 0 ;
      relateReturn = relate4lookupRelate( relateOn, d4 ) ;
      if ( relateReturn )
         return relateReturn ;
   }
}

int S4FUNCTION relate4matchLen( RELATE4 *relate, const int matchLenIn )
{
   int len, matchLen ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94413 ) ;
      if ( relate->master == 0 )
         return error4( relate->codeBase, e4parm, E84405 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94413 ) )
         return -1 ;
   #endif

   if ( error4code( relate->codeBase ) < 0 )
      return e4codeBase ;

   matchLen = matchLenIn ;
   #ifndef S4CLIENT
      expr4context( relate->masterExpr, relate->master->data ) ;
   #endif
   /* AS 03/01/99 --> if tag len is less than master expr len, scan relations do not work
      correctly becuase the key len is less than the compared length after the first match.
      Also see docs, where match len is the least of the 2 lengths...

      len = expr4keyLen( relate->masterExpr ) ;
   */

   #ifdef S4CLIENT
      len = expr4keyLen( relate->masterExpr ) ;
   #else
      #ifndef S4OFF_INDEX
         if ( relate->dataTag != 0 )
         {
            // AS Oct 1/02 - There is an issue here related to the nullability of the tag in FoxPro compatibility
            // in particular, if the tag is nullable but the master expression is not, we need to
            // increment the matchlen by 1 to allow for the inclusion of the null marker
            int keyLen = tfile4keyLen( relate->dataTag->tagFile ) ;
            int exprLen = expr4keyLen( relate->masterExpr ) ;
            #ifdef S4FOX
               // AS Nov 30/06 - need to set the contect for the tag expression as well
               expr4context( relate->dataTag->tagFile->expr, relate->data ) ;
               if ( expr4nullLow( relate->masterExpr, 0 ) == 0 && expr4nullLow( relate->dataTag->tagFile->expr, 0 ) == 1 )
                  exprLen++ ;  // increment by 1 because we compensate later when generating the value to include the non-null marker
            #endif

            /* LY 99/03/09 */
            len = ( exprLen < keyLen ? exprLen : keyLen ) ;
         }
         else
      #endif
            len = expr4keyLen( relate->masterExpr ) ;
   #endif

   #ifdef S4CLIPPER
      if ( matchLen <= 0 )
         matchLen = len ;
   #else
      if ( matchLen <= 0 )
      {
         relate->matchLen = len ;
         #ifdef E4ANALYZE
            if ( relate->matchLen < 0 )
               return error4( 0, e4struct, E94413 ) ;
         #endif
         return len ;
      }
   #endif

   #ifndef S4CLIENT
      #ifndef S4OFF_INDEX
         #ifndef S4CLIPPER
            if ( relate->dataTag )
               if( expr4type( relate->dataTag->tagFile->expr ) != r4str )  /* make sure r4str only */
                  return error4( relate->codeBase, e4relate, E84412 ) ;
         #endif
      #endif
   #endif

   if ( matchLen > len )
      matchLen = len ;

   #ifndef S4CLIENT
      #ifndef S4OFF_INDEX
         if ( relate->dataTag )
         {
            expr4context( relate->dataTag->tagFile->expr, relate->data ) ;
            len = expr4keyLen( relate->dataTag->tagFile->expr ) ;
            if ( matchLen > len )
               matchLen = len ;
         }
      #endif
   #endif

   relate->matchLen = matchLen ;
   relate4changed( relate ) ;

   #ifdef E4ANALYZE
      if ( relate->matchLen < 0 )
         return error4( 0, e4struct, E94413 ) ;
   #endif

   return matchLen ;
}



int S4FUNCTION relate4next( RELATE4 **ptrPtr )
{
   /* r4same = 0, r4down = 1, r4complete = 2 */
   RELATE4 *cur ;
   void *nextLink ;
   int rc ;

   #ifdef E4PARM_HIGH
      if ( ptrPtr == 0 )
      {
         error4( 0, e4parm_null, E94414 ) ;
         return r4complete ;
      }
      if ( *ptrPtr == 0 )
      {
         error4( 0, e4parm_null, E94414 ) ;
         return r4complete ;
      }
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( (*ptrPtr), 5, E94414 ) )
         return -1 ;
   #endif

   cur = *ptrPtr ;
   rc = r4down ;

   if ( cur->slaves.nLink > 0 )
   {
      *ptrPtr = (RELATE4 *)l4first( &cur->slaves ) ;
      return r4down ;
   }

   for(;;)
   {
      rc -- ;
      if ( cur->master == 0 )
      {
         *ptrPtr = 0 ;
         return r4complete ;
      }

      nextLink = l4next( &cur->master->slaves, cur ) ;
      if ( nextLink )
      {
         *ptrPtr = (RELATE4 *)nextLink ;
         return rc ;
      }

      cur = cur->master ;
   }
}



#ifndef S4CLIENT
   static int relate4nextRecordInScan( RELATE4 *relate )
   {
      S4LONG nextRec ;  /* LY 2001/07/21 : changed from long for 64-bit */
      int rc ;
      // AS Mar 15/02 - case where index corruption or run-time modification, can be problems
      // of getting into endless loop...
      Bool5 tagInSynch = 1 ;    // true if we were able to reposition to existing tag spot, false otherwise
      DATA4 *d4 ;
      #ifndef S4SERVER
         int  saveCode ;
      #endif
      #ifndef S4OFF_INDEX
         B4KEY_DATA *key ;
         char *ptr ;
         int len ;
         unsigned char *keyValue ;
         TAG4FILE *tagFile ;
      #endif

      #ifdef E4PARM_LOW
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94415 ) ;
      #endif

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      if ( relate->relation->isInitialized == 0 )
         return error4( relate->codeBase, e4relate, E84406 ) ;  // AS Aug 06/02 change error to e4reate not e4info

      if ( relate->relation->inSort == relate4sortSkip && relate->sortType == relate4sortSkip )
         return r4eof ;

      d4 = relate->data ;
      /* AS 03/04/97 fix #71, if data at eof (blank record), may produce a key
         which when tfile4go() gets called goes to the 1st key (r4after), which may
         flukily match the master key, producing an unexpected match and an
         extra record in the set */
      if ( d4eof( d4 ) )
         return r4eof ;
      #ifndef S4OFF_INDEX
         if ( relate->dataTag == 0 )
         {
      #endif
            if ( d4bof( d4 ) )
               nextRec = 1 ;
            else
               nextRec = d4recNo( d4 ) + 1 ;
            nextRec += f4flagGetNextFlip( &relate->set, (unsigned long)nextRec, (char)1 ) ;
            if ( d4recCountLessEq( d4, nextRec ) == 0 )
               return r4eof ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            tagFile = relate->dataTag->tagFile ;

            /* first, better make sure that tags and positions match record
               positions (in case of cloned DATA's ) */

            expr4context( tagFile->expr, relate->data ) ;
            tfile4exprKey( tagFile, &keyValue ) ;

            if ( relate->data->recNum == -1 )  /* special case because no valid record ... */
            {
               if ( !tfile4eof( tagFile ) )
                  tfile4goEof( tagFile ) ;
            }
            else
            {
               // AS Jun 25/02 - if only counting, the tag will be on the correect place
               // already, and in fact we don't want to go to the data4 recno since it is not
               // accurate (we are not going to the data4 on a count)
               if ( relate->relation->countOnly == 0 )
                  rc = tfile4go( tagFile, keyValue, relate->data->recNum, 0 ) ;
               else
                  rc = 0 ;
               // AS Mar 15/02 - case where index corruption or run-time modification, can be problems
               // of getting into endless loop...
               if ( rc != 0 )
               {
                  if ( rc < 0 )
                     return rc ;
                  // in theory cannot happen unless someone has modified the index or record we were on.
                  #ifdef E4DEBUG
                     // if index is locked, must be corrupt index
                     #ifdef S4CLIPPER
                        if ( tfile4lockTest( tagFile ) == 1 )
                     #else
                        if ( index4lockTest( tagFile->indexFile ) == 1 )  /* corrupt */
                     #endif
                           return error4describe( relate->codeBase, e4index, E81607, tagFile->alias, 0, 0 ) ;
                  #endif
                  tagInSynch = 0 ;
               }
            }

            for(;;)
            {
               if ( d4bof( d4 ) )
               {
                  if ( d4recCountLessEq( d4, 1L ) == 0L )  /* count == 0 */
                     return r4eof ;
                  if ( relate->masterExpr == 0 )   /* top relate, bof */
                     return r4bof ;

                  /* AS 11/17/98 - need to re-evaluate master expression, because we may
                     potentially be using a new slave relate, and therefore a different
                     master expression
                  */

                  len = relate4evaluateMasterExpression( relate, &ptr ) ;
                  if ( len < 0 )
                     return -1 ;
                  if ( relate4updateScanValue( relate, ptr, len ) < 0 )
                     return -1 ;

                  rc = (int)tfile4seek( tagFile, relate->master->scanValue, len ) ;
                  if ( rc < 0 )
                     return -1 ;
                  if ( rc == 0 )
                     rc = 1 ;
                  else
                     rc = 0 ;
               }
               else
                  #ifdef S4HAS_DESCENDING
                     rc = (int)tfile4dskip( tagFile, 1L ) ;
                  #else
                     rc = (int)tfile4skip( tagFile, 1L ) ;
                  #endif
               if ( rc < 0 )
                  return -1 ;
               if ( rc != 1 )
                  return r4eof ;

               key = tfile4keyData( tagFile) ;
               #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4spin.c on HP */
                  memcpy( &nextRec, &key->num, sizeof(S4LONG) ) ;
               #else
                  nextRec = key->num ;
               #endif

               // AS Mar 15/02 - case where index corruption or run-time modification, can be problems
               // of getting into endless loop...
               // if the tag is not in synch, ensure that nextRec is not == record we were on (means we positioned before
               // ourselves and go back to ourselves i.e. endless loop
               if ( tagInSynch == 0 )
               {
                  if ( nextRec == relate->data->recNum )  // only occurs if corrupt index
                     return error4describe( relate->codeBase, e4index, E81607, tagFile->alias, 0, 0 ) ;
                  tagInSynch = 1 ;    // we are ok, not simple endless loop.
               }

               if ( relate->master )
               {
                  #ifdef S4FOX
                     // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                     if ( u4keycmp( key->value, relate->master->scanValue, (unsigned int)relate->master->scanValueLen,
                     //     (unsigned int)tagFile->header.keyLen, 0, &tagFile->vfpInfo ) )
                        (unsigned int)tagFile->header.keyLen, 0, collation4get( tagFile->collateName ) ) )
                  #else
                     /* AS 06/14/99
                        In non-fox, must use tag comparison function.  For example, with bcd's,
                        2 different representations can be used for the same number...
                        if ( u4memcmp( key->value, relate->master->scanValue, (unsigned int)relate->master->scanValueLen ) )
                     */
                     if ( tagFile->cmp( key->value, relate->master->scanValue, (unsigned int)relate->master->scanValueLen ) )
                  #endif
                     return r4eof ;
               }

               if ( f4flagIsSetFlip( &relate->set, (unsigned long)nextRec ) )
               {
                  // AS July 16/02 - There is one other possibility here...  The record may be out of
                  // range due to a transaction in progress by another client (the tag's record number
                  // is > our record number).  If this is the case, continue skipping instead of breaking.
                  if ( d4recCountLessEq( d4, nextRec ) )  // record is ok, so stop.
                     break ;
               }
            }
         }
      #endif

      rc = 0 ;
      // AS Jun 25/02 - don't d4go if already on the record in question...
      if ( d4recNo( d4 ) != nextRec )
      {
         // AS Jun 25/02 - tfile4eof, no dfile4eof if counting and on a slave...
         if ( relate->relation->countOnly == 1  && relate != &relate->relation->relate && l4numNodes( &relate->slaves ) == 0 )
         {
            // if bof/eof flags are set, reset them... to avoid problems later...
            relate->data->bofFlag = 0 ;
            relate->data->eofFlag = 0 ;
            if ( relate->data->recNum == -1 )  // reset to 0 so not at invalid position
               relate->data->recNum = 0 ;
         }
         else
         {
            #ifndef S4SERVER
               saveCode = relate->codeBase->errGo ;
               relate->codeBase->errGo = 0 ;
            #endif
            rc = d4go( d4, nextRec ) ;
            #ifndef S4SERVER
               relate->codeBase->errGo = saveCode ;
            #endif
         }
      }

      if ( rc < 0 )
         return -1 ;
      if ( rc == r4entry )
         return r4eof ;
      relate->isRead = 1 ;   /* we have updated this one */
      return relate4skipped ;
   }



   /* !S4CLIENT */
   static int relate4currentIsChild( RELATE4 *parent )
   {
      /* returns 1 if the current relation set is a child (or is itself) of the input relation */
      RELATE4 *relateOn ;

      relateOn = parent->relation->currentRelateLevel ;

      if ( relateOn == parent )
         return 1 ;

      for ( relateOn = 0 ;; )  /* now recursively check all the descendents */
      {
         relateOn = (RELATE4 *)l4next( &parent->slaves, relateOn ) ;
         if ( relateOn == 0 )
            return 0 ;
         if ( relate4currentIsChild( relateOn ) == 1 )
            return 1 ;
      }
   }



   /* !S4CLIENT */
   static int relate4parent( RELATE4 *parent, RELATE4 *child )
   {
      /* returns 1 if the parent is above the child at any level (grandparent, etc) */
      RELATE4 *slaveOn, *masterOn ;

      for ( slaveOn = child ;; )
      {
         masterOn = slaveOn->master ;
         if ( masterOn == 0 )
            return 0 ;
         if ( masterOn == parent )
            return 1 ;
         slaveOn = slaveOn->master ;  /* go up one level */
      }
   }



   /* !S4CLIENT */
   static int relate4nextRelationList( RELATION4 *relation, int setup )
   {
      /* returns 1 if done, 0 if positioned to a new relate */
      /* if setup is true, it just does positional work (for relate4top), no reads */
      RELATE4 *relateOn, *master ;
      int rc, rc2 ;

      relateOn = relation->currentRelateLevel ;

      if ( relateOn == 0 )  /* means get the first one */
         relateOn = &relation->relate ;
      else
         if ( setup != 1 )
         {
            /* first see if we are part of a scan ourselves, and if so that we are scanned */
            if ( relateOn->relationType == relate4scan || relateOn == &relation->relate )  /* the master is an implicit scan */
            {
               relate4setNotRead( relateOn ) ;  /* This data file & its slaves */
               if ( relation->inSort == relate4sortDone )
                  if ( r4dataListFind( &relation->sortDataList, relateOn ) )
                     return relate4sortNextRecord( relation ) ;
               rc = relate4nextRecordInScan( relateOn ) ;
               if ( rc == relate4skipped )
                  return 0 ;
               rc2 = relate4blankSet( relateOn, (char)1 ) ;
               if ( rc2 == r4locked || rc2 < 0 )  /* error or locked */
                  return rc2 ;
               if ( relateOn->master == 0 )
                  if ( d4eof( relateOn->data ) )
                     return r4eof ;
               /* are our siblings also scanned? */

               /* we are scanned, so fall through and check out our master */
            }

            if ( relateOn->master == 0 )
            {
               relation->currentRelateLevel = 0 ;
               return r4eof ;
            }

            master = relateOn->master ;

            /* try our masters next slave */
            relateOn = (RELATE4 *)l4next( &master->slaves, relateOn ) ;
            if ( relateOn == 0 )  /* no more slaves, try the master itself */
            {
               relation->currentRelateLevel = master ;
               return relate4continue ;  /* either do ourselves or go up further */
            }
         }

      /* we need to go down our own slave list to the bottom level and start seek */
      while ( relateOn->slaves.nLink != 0 )
         relateOn = (RELATE4 *)l4first( &relateOn->slaves ) ;

      /* at the bottom, so try ourselves */
      relation->currentRelateLevel = relateOn->master ;

      if ( setup == 1 )
         return relate4continue ;

      if ( relateOn->master == 0 )  /* done/ eof */
         return r4eof ;

      if ( relateOn->relationType == relate4scan )
         return relate4continue ;

      /* otherwise try our current masters other slaves--i.e. just go get next */
      return relate4nextRelationList( relation, setup ) ;
   }



   /* !S4CLIENT */
   static int relate4nextScanRecord( RELATION4 *relation )
   {
      RELATE4 *relate ;
      int rc, rc2, tryMatches ;
      LIST4 *relateList ;

      if ( error4code( relation->relate.codeBase ) < 0 )
         return -1 ;

      rc = 0 ;

      for ( ;; )
      {
         if ( relation->currentRelateLevel == 0 )
            relation->currentRelateLevel = &relation->relate ;

         for ( ;; )
         {
            relateList = &relation->currentRelateLevel->relateList ;
            tryMatches = 1 ;
            if ( d4eof( relation->currentRelateLevel->data ) ) /* we are at eof, so all children cannot match */
               tryMatches = 0 ;
            else
            {
               if ( relation->currentRelateLevel->master != 0 )
                  if ( d4eof( relation->currentRelateLevel->master->data ) )  /* means no matches possible */
                     tryMatches = 0 ;
            }

            if ( tryMatches == 1 && l4numNodes( relateList ) )
            {
               if ( relateList->selected == 0 )
                  relateList->selected = (LINK4 *)l4first( relateList ) ;
               for( ;; )
               {
                  if ( relateList->selected == 0 )  /* finished with matches for this list */
                     break ;
                  relate = ((RELATE4LIST *)relateList->selected)->ptr ;
                  relate4setNotRead( relate ) ;  /* This data file & its slaves */
                  if ( relation->inSort == relate4sortDone )
                     if ( r4dataListFind( &relation->sortDataList, relate ) )
                        return relate4sortNextRecord( relation ) ;

                  rc = relate4nextRecordInScan( relate ) ;
                  if ( rc == relate4skipped )
                     return 0 ;
                  if ( rc < 0 )
                     return rc ;
                  rc2 = relate4blankSet( relate, (char)1 ) ;
                  if ( rc2 == r4locked || rc2 < 0 )  /* error or locked */
                     return rc2 ;
                  if ( relate->master == 0 )
                     if ( d4eof( relate->data ) )
                        return r4eof ;
                  relateList->selected =(LINK4 *)l4next( relateList, relateList->selected ) ;
               }
            }
            rc = relate4nextRelationList( relation, 0 ) ;
            if ( rc != relate4continue )
               return rc ;
         }

      }
   }



   /* !S4CLIENT */
   static int relate4prevRelationList( RELATION4 *relation, int setup )
   {
      /* returns 1 if done, 0 if positioned to a new relate */
      /* if setup is true, it just does positional work (for relate4top), no reads */
      RELATE4 *relateOn, *master ;
      int rc, rc2 ;

      relateOn = relation->currentRelateLevel ;

      if ( relateOn == 0 )  /* means get the first one */
         relateOn = &relation->relate ;
      else
         if ( setup != 1 )
         {
            /* first see if we are part of a scan ourselves, and if so that we are scanned */
            if ( relateOn->relationType == relate4scan || relateOn == &relation->relate )  /* the master is an implicit scan */
            {
               relate4setNotRead( relateOn ) ;  /* This data file & its slaves */
               if ( relation->inSort == relate4sortDone )
                  if ( r4dataListFind( &relation->sortDataList, relateOn ) )
                     return relate4sortPrevRecord( relation ) ;

               rc = relate4prevRecordInScan( relateOn ) ;
               if ( rc == relate4skipped )
               {
                  if ( relate4eof( relateOn ) )
                  {
                     if ( relation->inSort == relate4sortDone && relation->sortEofFlag == r4eof )
                     {
                        relation->sortRecOn-- ;  /* move off eof on sort part */
                        relation->sortEofFlag = 0 ;
                     }
                     else
                     {
                        rc = d4go( relation->relate.data, d4recCount( relation->relate.data ) ) ;
                        if ( rc < 0 )
                           return rc ;
                     }
                  }
                  return 0 ;
               }
               if ( rc < 0 )
                  return rc ;
               rc2 = relate4blankSet( relateOn, (char)-1 ) ;
               if ( rc2 == r4locked || rc2 < 0 )  /* error or locked */
                  return rc2 ;
               if ( relateOn->master == 0 )
               {
                  if ( d4bof( relateOn->data ) )
                     return r4bof ;
                  if ( d4eof( relateOn->data ) )
                     return r4eof ;
               }
               /* are our siblings also scanned? */

               /* we are scanned, so fall through and check out our master */
            }

            if ( relateOn->master == 0 )
            {
               relation->currentRelateLevel = 0 ;
               return r4bof ;
            }

            master = relateOn->master ;

            /* try our masters prev slave */
            relateOn = (RELATE4 *)l4prev( &master->slaves, relateOn ) ;
            if ( relateOn == 0 )  /* no more slaves, try the master itself */
            {
               relation->currentRelateLevel = master ;
               return relate4continue ;  /* either do ourselves or go up further */
            }
         }

      /* we need to go down our own slave list to the bottom level and start seek */
      while ( relateOn->slaves.nLink != 0 )
         relateOn = (RELATE4 *)l4last( &relateOn->slaves ) ;

      /* at the bottom, so try ourselves */
      relation->currentRelateLevel = relateOn->master ;

      if ( setup == 1 )
         return relate4continue ;

      if ( relateOn->master == 0 )  /* done / bof */
         return r4bof ;

      if ( relateOn->relationType == relate4scan )
         return relate4continue ;

      /* otherwise try our current masters other slaves--i.e. just go get next */
      return relate4prevRelationList( relation, setup ) ;
   }



   /* !S4CLIENT */
   static int relate4prevRecordInScan( RELATE4 *relate )
   {
      S4LONG nextRec ;  /* LY 2001/07/21 : changed from long for 64-bit */
      int  rc ;
      DATA4 *d4 ;
      #ifndef S4SERVER
         int  saveCode ;
      #endif
      #ifndef S4OFF_INDEX
         TAG4FILE *tagFile ;
         B4KEY_DATA *key ;
         unsigned char *keyValue ;
         int len ;
         char *ptr ;
         #ifdef S4HAS_DESCENDING
            unsigned short int oldDesc ;
         #endif
      #endif

      #ifdef E4PARM_LOW
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94416 ) ;
      #endif

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      if ( relate->relation->isInitialized == 0 )
         return error4( relate->codeBase, e4relate, E84406 ) ;

      d4 = relate->data ;

      #ifndef S4OFF_INDEX
         if ( relate->dataTag == 0 )
         {
      #endif
            nextRec = d4recNo( d4 ) - 1 ;
            nextRec -= f4flagGetNextFlip( &relate->set, (unsigned long)nextRec, (char)-1 ) ;
            if ( nextRec <= 0 )
               return r4bof ;
            if ( d4recCountLessEq( d4, nextRec ) == 0 )
               return r4eof ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            tagFile = relate->dataTag->tagFile ;

            /* first, better make sure that tags and positions match record
               positions (in case of cloned DATA's ) */

            expr4context( tagFile->expr, relate->data ) ;
            tfile4exprKey( tagFile, &keyValue ) ;

            // AS Aug 20/02 - Also same case if eof flag is true
            // if ( relate->data->recNum == -1 )  /* special case because no valid record ... */
            if ( relate->data->recNum == -1 || relate->data->eofFlag == 1 )  /* special case because no valid record ... */
            {
               if ( !tfile4eof( tagFile ) )
                  tfile4goEof( tagFile ) ;
            }
            else
            {
               rc = tfile4go( tagFile, keyValue, relate->data->recNum, 0 ) ;
               if ( rc < 0 )
                  return rc ;
            }

            for(;;)
            {
               if ( relate4eof( relate ) )   /* if eof in relate, just leave on last tag entry */
               {
                  rc = tfile4eof( tagFile ) ? 0 : -1 ;
                  /* AS 07/10/98 fix #136 */
                  if ( rc == -1 )  /* move data file to non-eof condition in case record does not match query */
                     d4->eofFlag = 0 ;
               }
               else
               {
                  if ( d4eof( d4 ) == 1 )
                  {
                     if ( d4recCountLessEq( d4, 1L ) == 0L )  /* count == 0 */
                        return r4bof ;
                     if ( relate->masterExpr == 0 )   /* top relate, bof */
                        return r4eof ;

                     /* AS 11/17/98 - need to re-evaluate master expression, because we may
                        potentially be using a new slave relate, and therefore a different
                        master expression
                     */

                     len = relate4evaluateMasterExpression( relate, &ptr ) ;
                     if ( len < 0 )
                        return -1 ;
                     if ( relate4updateScanValue( relate, ptr, len ) < 0 )
                        return -1 ;

                     #ifdef S4HAS_DESCENDING
                        oldDesc = tagFile->header.descending ;
                        tfile4descending( tagFile, ((unsigned short int)(1 - oldDesc)) ) ;  /* invert the descending */
                        rc = (int)tfile4seek( tagFile, relate->master->scanValue, len ) ;
                        tfile4descending( tagFile, oldDesc ) ;
                     #else
                        /* need to find the last matching entry, without seek */
                        rc = (int)tfile4bottom( tagFile ) ;
                        if ( rc == 0 )
                        {
                           rc = -1 ;
                           while ( rc == -1 )
                           {
                              key = tfile4keyData( tagFile) ;
                              if ( u4memcmp( key->value, relate->master->scanValue, (unsigned int)relate->master->scanValueLen ) == 0 )
                              {
                                 rc = 0 ;
                                 break ;
                              }
                              if ( u4memcmp( key->value, relate->master->scanValue, (unsigned int)relate->master->scanValueLen ) < 0 )
                                 return r4bof ;
                              rc = (int)tfile4skip( tagFile, -1L ) ;
                           }
                        }
                     #endif
                     if ( rc < 0 )
                        return -1 ;
                     if ( rc == 0 )
                        rc = -1 ;
                     else
                        rc = 0 ;
                  }
                  else
                  {
                     /* AS 07/31/98 if tag is at eof then do not
                        skip since eof is equivalent to bof which we cannot
                        simulate.  It means we are done with the tag, so don't continue
                     */
                     if ( tfile4eof( tagFile ) )
                        rc = 0 ;   /* rc 0 means did not skip */
                     else
                     {
                        #ifdef S4HAS_DESCENDING
                           rc = (int)tfile4dskip( tagFile, -1L ) ;
                        #else
                           rc = (int)tfile4skip( tagFile, -1L ) ;
                        #endif
                     }
                  }
               }
               if ( rc > 0 )
                  return -1 ;
               if ( rc != -1L )
                  return r4bof ;

               key = tfile4keyData( tagFile) ;
               #ifdef S4DATA_ALIGN  /* LY 00/02/18 : t4spin.c on HP */
                  memcpy( &nextRec, &key->num, sizeof(S4LONG) ) ;
               #else
                  nextRec = key->num ;
               #endif

               if ( relate->master )
                  #ifdef S4FOX
                     // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                     if ( u4keycmp( key->value, relate->master->scanValue, (unsigned int)relate->master->scanValueLen,
                     //     (unsigned int)tagFile->header.keyLen, 0, &tagFile->vfpInfo ) )
                          (unsigned int)tagFile->header.keyLen, 0, collation4get( tagFile->collateName ) ) )
                  #else
                     if ( u4memcmp( key->value, relate->master->scanValue, (unsigned int)relate->master->scanValueLen ) )
                  #endif
                     return r4bof ;

               if ( f4flagIsSetFlip( &relate->set, (unsigned long)nextRec ) )
               {
                  // AS July 16/02 - There is one other possibility here...  The record may be out of
                  // range due to a transaction in progress by another client (the tag's record number
                  // is > our record number).  If this is the case, continue skipping instead of breaking.
                  if ( d4recCountLessEq( d4, nextRec ) )  // record is ok, so stop.
                    break ;
               }
            }
         }
      #endif /* !S4OFF_INDEX */

      #ifndef S4SERVER
         saveCode = relate->codeBase->errGo ;
         relate->codeBase->errGo = 0 ;
      #endif
      rc = d4go( d4, nextRec ) ;
      #ifndef S4SERVER
         relate->codeBase->errGo = saveCode ;
      #endif
      if ( rc < 0 )
         return -1 ;
      if ( rc == r4entry )
         return r4eof ;
      relate->isRead = 1 ;   /* we have updated this one */
      return relate4skipped ;
   }



   /* !S4CLIENT */
   static int relate4prevScanRecord( RELATION4 *relation )
   {
      RELATE4 *relate ;
      int rc, rc2, tryMatches ;
      LIST4 *relateList ;

      if ( error4code( relation->relate.codeBase ) < 0 )
         return -1 ;

      rc = 0 ;

      /* this function gets the next total scan record.  Done so by moving
         from current position to either the next record in ourselves (low
         level) or moving our child to the correct level */

      for ( ;; )
      {
         /* relation->currentRelateLevel is the relation we left off on
            for the last scan (i.e. which level we are on in scanning process)
         */
         if ( relation->currentRelateLevel == 0 )
            relation->currentRelateLevel = &relation->relate ;

         for ( ;; )
         {
            /* relateList is the list of relations for the current relation
               we are working on
            */
            relateList = &relation->currentRelateLevel->relateList ;

            /* tryMatches is whether we should try skipping at a lower level
               (i.e. we are working our way down the scan list) or whether
               we are finished with our scan (i.e. working our way up
               the scan list)
            */
            tryMatches = 1 ;


            /* if our current level is eof then we are finished with our level
               so tryMatches set to 0 (i.e. go up a level in scan */
            if ( d4eof( relation->currentRelateLevel->data ) ) /* we are at eof, so all children cannot match */
               tryMatches = 0 ;
            else
            {
               /* if our master is at eof, then clearly we cannot continue
                  ourselves, so move up to our masters level by setting
                  tryMatches to false */
               if ( relation->currentRelateLevel->master != 0 )
                  if ( d4eof( relation->currentRelateLevel->master->data ) )  /* means no matches possible */
                     tryMatches = 0 ;
            }

            if ( tryMatches == 1 && l4numNodes( relateList ) )
            {
               if ( relateList->selected == 0 )
               {
                  /* this means that we need to restart our relation list
                     since this is the 1st time through; the for loop
                     below takes us through the chain as required, esp. if
                     our list is on a previous selection */
                  relateList->selected = (LINK4 *)l4last( relateList ) ;
               }

               for (;; )
               {
                  if ( relateList->selected == 0 )  /* finished with matches for this list */
                     break ;

                  /* set for sort evaluation in following lines */
                  relate = ((RELATE4LIST *)l4last( relateList ))->ptr ;

                  /* if eof, since we are going backwards (previous), we need
                     to go to bottom of file and scan our way up
                     note that relate4eof() just looks at the sort flag for eof if using
                     a sort, or the eof flag of the relation's highest master's eof flag.
                     it does not change on a relate by relate basis, but rather on a
                     relation by relation basis */
                  if ( relate4eof( relate ) )
                  {
                     /* sort done is set when doing sorts as to whether we
                        are generating the sort or have generated it (sortDone) */
                     if ( relation->inSort != relate4sortDone )
                     {
                        rc = relate4bottom( relate ) ;
                        if ( rc == r4eof )   /* no records, so can't skip back */
                           return r4bof ;
                        else
                           return rc ;
                     }
                  }

                  /* start at the current child relation */
                  relate = ((RELATE4LIST *)relateList->selected)->ptr ;

                  /* relate4setNotRead just marks a flag indicating that the data has not
                     yet been read (it does not affect current positioning, just indicates
                     that since we are changing our position, we will need to re-read our
                     children after we have repositioned since their position is affected
                     by the contents of our record which they are related on */
                  relate4setNotRead( relate ) ;  /* This data file & its slaves */

                  if ( relation->inSort == relate4sortDone )
                     if ( r4dataListFind( &relation->sortDataList, relate ) )
                        return relate4sortPrevRecord( relation ) ;

                  /* now request to get the previous record in the current relation.  There
                     may or may not be more records available depending on whether or not
                     we have finished scanning.  If we do find another valid record,
                     relate4skipped is returned */
                  rc = relate4prevRecordInScan(relate) ;

                  if ( rc == relate4skipped )  /* there was another record in current level */
                  {
                     if ( relate4eof( relate ) )  /* generally only if in sort */
                     {
                        if ( relation->inSort == relate4sortDone && relation->sortEofFlag == r4eof )
                        {
                           relation->sortRecOn-- ;  /* move off eof on sort part */
                           relation->sortEofFlag = 0 ;
                        }
                        else
                        {
                           rc = d4go( relation->relate.data, d4recCount( relation->relate.data ) ) ;
                           if ( rc < 0 )
                              return rc ;
                        }
                     }

                     /* we are at the desired position, we have skipped so return success back up */
                     return r4success ;
                  }
                  if ( rc < 0 )  /* general error handling */
                     return rc ;

                  /* if here it means that there were no scans left for the current relation.
                      that means we need to set ourselves to blank and go on to the next
                      relation */
                  rc2 = relate4blankSet( relate, (char)-1 ) ;
                  if ( rc2 == r4locked || rc2 < 0 )  /* error or locked */
                     return rc2 ;

                  /* if relate->master is 0 it means we are at the topmost level */
                  if ( relate->master == 0 )
                  {
                     /* check if we have moved to eof/bof position */
                     if ( d4bof(relate->data) )
                        return r4bof ;
                     if ( d4eof(relate->data) )
                        return r4eof ;
                  }

                  /* get the previous relation to continue the scan - note that it gets
                     the next relation in the list of our master's relations only, it
                     does not move up the chain of relations */
                  relateList->selected =(LINK4 *)l4prev( relateList, relateList->selected ) ;
               }
            }

            /* if got here it means that we exhausted all of our child relations and they
               have all been scanned through.  Therefore we must move up one level in the
               relation and try to scan further relations up the tree */
            rc = relate4prevRelationList( relation, 0 ) ;
            if ( rc != relate4continue )
               return rc ;
         }
      }
   }
#endif /* !S4CLIENT */



#ifndef S4SERVER
   static int relate4verifyExpression( RELATE4 *relate, const char *expr )
   {
      /* can't check in server because aliases not set up yet */
      /* 03/25/99 - Try to parse the expression here as well, to ensure valid */
      EXPR4 *trialEvaluation = expr4parseLow( relate->data, expr, 0 ) ;
      if ( trialEvaluation == 0 )
      {
         if ( error4code( relate->codeBase ) < 0 )
            return error4code( relate->codeBase ) ;
         return error4( relate->codeBase, e4relate, E94428 ) ;
      }
      // AS Aug 15/02 - Also verify that any DATA4's referred to be fields are part of
      // the relation (either in this relate or a slave)
      for ( int infoIndex = 0 ; infoIndex < trialEvaluation->infoN ; infoIndex++ )
      {
         FIELD4 *fld = trialEvaluation->info[infoIndex].fieldPtr ;
         if ( fld != 0 )
         {
            if ( relate4dbfInRelation( relate, fld->data ) == 0 )
            {
               expr4free( trialEvaluation ) ;
               return error4( relate->codeBase, e4relate, E81510 ) ;
            }
         }
      }

      expr4free( trialEvaluation ) ;
      return 0 ;
   }
#endif




int S4FUNCTION relate4querySet( RELATE4 *relate, const char *expr )
{
   int len ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94428 ) ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94428 ) )
         return -1 ;
   #endif

   // AS Mar 21/06 - use the master RELATE4 for all operations
   relate = &relate->relation->relate ;

   if ( error4code( relate->codeBase ) < 0 )
      return -1 ;

   relate4changed( relate ) ;
   u4free( relate->relation->exprSource ) ;
   relate->relation->exprSource = 0 ;

   // AS Nov 2/05 - new callback functionality - to adjust the query prior to it actually being set
   #ifdef S4WIN32
      if ( relate->querySetCallback != 0 )
         expr = relate->querySetCallback( relate, expr ) ;
   #endif

   if ( expr == 0 )
      return 0 ;
   if ( expr[0] == 0 )
      return 0 ;

   #ifndef S4SERVER
      int rc = relate4verifyExpression( relate, expr ) ;
      if ( rc < 0 )
         return rc ;
   #endif

   len = c4strlen( expr ) + 1 ;
   relate->relation->exprSource = (char *)u4allocEr( relate->codeBase, (long)len ) ;
   if ( relate->relation->exprSource == 0 )
      return -1 ;
   c4memcpy( relate->relation->exprSource, expr, (unsigned int)len ) ;
   return 0 ;
}



#ifndef S4CLIENT
   int relate4readIn( RELATE4 *relate )
   {
      int rc ;

      if ( error4code( relate->codeBase ) < 0 )
         return -1 ;
      if ( relate->isRead )
         return 0 ;
      if ( relate->master )
         if ( relate->master->isRead == 0 )
         {
            rc = relate4readIn( relate->master ) ;
            if ( rc == relate4filterRecord || rc == r4terminate )
               return rc ;
         }

      return relate4lookup( relate, 1 ) ;
   }



   /* !S4CLIENT */
   static int relate4readRest( RELATE4 *relate, signed char direction )
   {
      /* direction : -1 = look backwards, 0 = lookup only, 1 = look forwards */
      RELATE4 *slave ;
      int rc, scanDone ;

      #ifdef E4PARM_LOW
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94417 ) ;
         if ( direction < -1 || direction > 1 )
            return error4( 0, e4parm, E94417 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( error4code( relate->codeBase ) < 0 )
            return e4codeBase ;
      #endif

      rc = 0 ;

      if ( relate->isRead == 0 )
      {
         // AS July 16/02 - Due to transaction processing, do a lookup to ensure our record # is valid (in range)
         // AS Oct 21/02 - there is another instance here - namely that we may be performing a count
         // and we are at the 'record 1 after' in which case we don't want to return r4eof (T4REL2.C)
         if ( relate->relation->countOnly == 0 && d4recCountGreater( relate->data, relate->data->recNum  )  && !d4eof( relate->data ) )
         {
            return relate4filterRecord ;
         }
         rc = relate4lookup( relate, direction );
         if ( rc < 0 ||rc == r4terminate )
         {
            relate4changed( relate ) ;  /* mark it as non-initialized, and free-up resources */
            return rc ;
         }
         if ( rc == relate4filterRecord )
            return rc ;
      }

      scanDone = 0 ;
      for( slave = 0 ;; )
      {
         if ( direction == 1 )
            slave = (RELATE4 *)l4next( &relate->slaves, slave ) ;
         else
            slave = (RELATE4 *)l4prev( &relate->slaves, slave ) ;
         if ( slave == 0 )
            break ;
         if ( slave->isRead == 0 )
            if ( slave->relationType == relate4scan )
            {
               /* if the currentRelateLevel is an upward master of ourselves,
                  then make our master the currentRelateLevel so that the next
                  skip will scan through us */
               if ( relate4parent( slave->relation->currentRelateLevel, slave->master ) )
                  slave->relation->currentRelateLevel = slave->master ;
               if ( direction == 1 )
               {
                  // AS Jun 25/02 - if only counting, position tag only (for slave)
                  if ( relate->relation->countOnly == 1 )
                  {
                     // reset the bof/eof flags, if any
                     slave->data->bofFlag = 0 ;
                     slave->data->eofFlag = 0 ;
                     if ( slave->data->recNum == -1 )  // means blank record, don't want this
                        slave->data->recNum = 0 ;

                  }
                  else
                     d4top( slave->data ) ;

                  #ifndef S4OFF_INDEX
                     tfile4top( slave->dataTag->tagFile ) ;
                  #endif
               }
               else
               {
                  // AS Jun 25/02 - if only counting, position tag only (for slave)
                  if ( relate->relation->countOnly == 1 )
                  {
                     // reset the bof/eof flags, if any
                     slave->data->bofFlag = 0 ;
                     slave->data->eofFlag = 0 ;
                     if ( slave->data->recNum == -1 )  // means blank record, don't want this
                        slave->data->recNum = d4recCount( slave->data ) + 1 ;
                  }
                  else
                     d4bottom( slave->data ) ;

                  #ifndef S4OFF_INDEX
                     tfile4bottom( slave->dataTag->tagFile ) ;
                  #endif
               }
            }
      }
      for( slave = 0 ;; )
      {
         if ( direction == 1 )
            slave = (RELATE4 *)l4next( &relate->slaves, slave ) ;
         else
            slave = (RELATE4 *)l4prev( &relate->slaves, slave ) ;
         if ( slave == 0 )
            return 0 ;
         if ( slave->relationType == relate4scan && scanDone == 1 )
         {
            if ( slave->isRead == 0 )
            {
               relate4blankSet( slave, (-direction) ) ;  /* do reverse of direction */
               slave->isRead = 1 ;
               rc = relate4readRest( slave, direction ) ;
            }
         }
         else
         {
            rc = relate4readRest( slave, direction ) ;
            if ( slave->relationType == relate4scan && rc == 0 )
            {
               switch ( direction )
               {
                  case -1:
                     if ( d4bof( slave->data ) == 0 )
                        scanDone = 1 ;
                     break ;
                  case 1:
                     if ( d4eof( slave->data ) == 0 )
                        scanDone = 1 ;
                     break ;
                  default:
                     break ;
               }
            }
         }
         if ( rc < 0 || rc == relate4filterRecord || rc == r4terminate )
            return rc ;
      }
   }



   /* !S4CLIENT */
   static void relate4setNotRead( RELATE4 *relate )
   {
      RELATE4 *slaveOn ;

      if ( relate->isRead )
      {
         relate->isRead = 0 ;
         for( slaveOn = 0 ;; )
         {
            slaveOn = (RELATE4 *)l4next(&relate->slaves,slaveOn) ;
            if ( slaveOn == 0 )
               return ;
            relate4setNotRead( slaveOn ) ;
         }
      }
   }
#endif



#ifdef S4CLIENT
   static int relation4skipFetchSingle( RELATION4 *relation, long numskip )
   {
      assert5port( "new client/server function to fetch relation records in batches" ) ;
      // normal skip/fetch where block reading is not occurring.
      CONNECTION4RELATE_SKIP_INFO_IN *info ;
      int saveRc ;

      CODE4 *c4 = relation->relate.codeBase ;

      CONNECTION4 *connection = relation->relate.data->dataFile->connection ;
      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( c4, e4struct, E94418 ) ;
      #endif
      connection4assign( connection, CON4RELATE_SKIP, 0, 0 ) ;
      connection4addData( connection, 0, sizeof( CONNECTION4RELATE_SKIP_INFO_IN ), (void **)&info ) ;
      info->relationId = htonl5( relation->relationId ) ;
      info->numSkips = htonl5( numskip ) ;
      // AS May 21/02 - code to support auto-transfer of memo fields
      assert5port( "added optional transfer of memo fields with client/server communications" ) ;
      info->includeMemos = relation->includeMemos ;
      connection4sendMessage( connection ) ;
      saveRc = connection4receiveMessage( connection ) ;
      if ( saveRc < 0 )
         return error4( c4, saveRc, E94418 ) ;
      saveRc = connection4status( connection ) ;
      if ( saveRc < 0 )
      {
         // AS Aug 6/02 - Ensure that server frees up info as well...
         relate4freeServer( &relation->relate ) ;
         relation->isInitialized = 0 ;
         if ( saveRc < 0 )
            return connection4error( connection, c4, saveRc, E94418 ) ;
      }

      int rc = relation4unpack( relation, connection ) ;
      if ( rc < 0 )
         return error4( c4, rc, E94418 ) ;
      if ( saveRc == r4terminate )
      {
         // AS Aug 6/02 - Ensure that server frees up info as well... - ensure relate4unpack is called first
         relate4freeServer( &relation->relate ) ;
         relation->isInitialized = 0 ;
      }
      return saveRc ;
   }



   static int relation4skipFetchFromBuffer( RELATION4 *relation, long pos )
   {
      assert5port( "new client/server function to fetch relation records in batches" ) ;
      // fetches the current relation record from the buffer
      // pos is the position into the buffer to retrive from (range 0 to numRecs-1)
      // also updates the current position in the buffer to the input pos
      relation->readBufPos = pos ;

      char *fromPos = relation->readAdvanceBuf + (relation->readRecLen * pos ) ;

      int rc = (int)(*((long *)fromPos) ) ;
      fromPos += sizeof( long ) ;

      #ifndef S4OFF_MEMO
         int memoOn = 0 ;  // counter for all the memos in the extended relation record (may cross multiple records if there are slaves)
      #endif

      for( RELATE4 *relateOn = &relation->relate ;; )
      {
         DATA4 *data = relateOn->data ;
         #ifndef S4OFF_MEMO
            if ( data->dataFile->nFieldsMemo > 0 )
            {
               for ( int i = 0; i < data->dataFile->nFieldsMemo; i++ )
               {
                  f4memoReset( data->fieldsMemo[i].field ) ;
                  if ( relation->doMemos )
                  {
                     MEMO4BATCH_ENTRY *entry = &relation->memos[pos].memos[memoOn] ;
                     FIELD4 *field = data->fieldsMemo[i].field ;
                     f4memoReadSet( field, entry->contentsLen, entry->contents ) ;
                     field->memo->status = 0 ;
                  }
                  memoOn++ ;
               }
            }
         #endif

         data->bofFlag = *((short *)fromPos) ;
         fromPos += sizeof( short ) ;
         data->eofFlag = *((short *)fromPos) ;
         fromPos += sizeof( short ) ;
         data->recNum = *((long *)fromPos) ;
         fromPos += sizeof( long ) ;
         long recWidth = dfile4recWidth( data->dataFile ) ;
         memcpy( data->record, fromPos, recWidth ) ;
         fromPos += recWidth ;

         if ( relate4next( &relateOn ) == 2 )
            break ;
      }

      if ( rc != 0 )  // reset the buffer
         relation4readBufferReset( relation, 0 ) ;

      return rc ;
   }



   static int relation4skipFetchMultiple( RELATION4 *relation, long numskip, short startPos, long startOffset )
   {
      assert5port( "new client/server function to fetch relation records in batches" ) ;
      // AS Apr 12/02 block reading of records for relate skip...
      // startPos == 0 if skipping, 1 if relate4top, 2 if relate4bottom
      // startOffset indicates if the server side must do some extra skipping first.  This happens, for example,
      // if we started skipping forwards (say 10 recs) (server is on record 10) but then start skipping backwards
      // now we need the server side to skip in the given direction by this new startOffset before continuing
      // to fetch recods.
      CODE4 *c4 = relation->relate.codeBase ;
      CONNECT4 *connect = c4getClientConnect( c4 ) ;

      #ifdef E4ANALYZE
         if ( connect == 0 )
            return error4( c4, e4struct, E94418 ) ;
      #endif

      // AS Aug 1/02 - It is possible if the relation has changed that we need to
      // allocate memory at this point for the bufferring...
      if ( relation->readAdvanceBuf == 0 )
      {
         int rc = relation4readBufferAlloc( relation, relation->doMemos ) ;
         if ( rc != 0 )
            return rc ;
      }

      if ( startPos == 1 ) // means relate4top  - indicate we are skipping forward
         numskip = 1 ;
      if ( startPos == 2 ) // means relate4top - indicate we skipping backward
         numskip = -1 ;

      connect4sendShort( connect, MSG5RELATE_SKIP_MULTI ) ;
      connect4sendLong( connect, relation->relationId ) ;
      connect4sendShort( connect, c4->readLock ) ;
      connect4sendShort( connect, code4unlockAuto( c4 ) ) ;
      connect4sendShort( connect, startPos ) ;
      connect4sendLong( connect, numskip ) ;
      connect4sendLong( connect, relation->readRecsToBuf ) ;
      connect4sendShort( connect, relation->doMemos ) ;
      connect4sendLong( connect, startOffset ) ;
      connect4sendFlush( connect ) ;

      int saveRc = connect4receiveShort( connect ) ;

      if ( numskip > 0 )
         relation->readBufDirection = 1 ;
      else
         relation->readBufDirection = -1 ;

      if ( saveRc >= 0 )
      {
         int rc = saveRc ;

         assert5( relation->readBufNum == 0 ) ;
         char *buf = relation->readAdvanceBuf ;

         for( ;; )
         {
            *((long *)buf) = (long)rc ;
            relation4unpackMultiple( relation, buf + sizeof( long ) ) ;
            relation->readBufNum++ ;
            buf += relation->readRecLen ;

            if ( rc != 0 || relation->readBufNum == relation->readRecsToBuf )  // means we are done
               break ;

            rc = connect4receiveShort( connect ) ;

            if ( rc < 0 )
               break ;
         }
      }

      if ( saveRc >= 0 )   // fill in with the bufferred data
         relation4skipFetchFromBuffer( relation, 0 ) ;

      if ( saveRc < 0 || saveRc == r4terminate )  // in this case no records are sent back
      {
         // AS Aug 6/02 - Ensure that server frees up info as well...
         relate4freeServer( &relation->relate ) ;
         relation->isInitialized = 0 ;
         if ( saveRc < 0 )
            return error4( c4, saveRc, E94418 ) ;
      }

      return saveRc ;
   }
#endif


// AS Apr 18/05 - new callback functionality - currently only for windows 32 bit...
// #ifdef S4SERVER
   #define SUSPEND4START_CNT 1000
   #ifdef S4SERVER
      static void relate4suspendInit( RELATE4SUSPEND *suspend, SERVER4CLIENT *client )
   #else
      static void relate4suspendInit( RELATE4SUSPEND *suspend, RELATE4 *relate )
   #endif
   {
      if ( suspend->isInit == 1 )
         return ;
      // AS Jan 23/04 - Support a suspension of performing the relate so other clients can perform actions
      // #ifdef S4SERVER
         suspend->relCnt = SUSPEND4START_CNT ;  // start fairly high to get a good first estimate
         suspend->relCntStart = SUSPEND4START_CNT ;  // start fairly high to get a good first estimate
      // #else
      //    suspend->relCnt = 2500 ;  // a little lower in stand-alone
      // #endif
      suspend->relCntVal = 0 ;   // set to # of iterations before .1 seconds did elapse (for setting value without needing to use time function)
      suspend->relCntSet = 0 ;  // have we calculated the # of cntVal iterations yet?
      suspend->numInitCountAttempts = 0 ;
      #ifdef S4SERVER
         suspend->client = client ;
      #else
         suspend->relate = relate ;
      #endif
      suspend->isInit = 1 ;
      #ifdef S4WIN32
                        // CS 2011/05/18 ftime not supported on WinCE
                        suspend->relStartTime = GetTickCount();
      #else
                        ftime( &suspend->relStartTime ) ;
                #endif
      // AS Feb 28/06 - only used in server version
      #ifndef S4SERVER
                        #ifdef S4WIN32
                                // CS 2011/05/18 ftime not supported on WinCE
                                suspend->relLastCallTime = GetTickCount();
                        #else
                                ftime( &suspend->relLastCallTime ) ;  // AS Feb 6/06 - wasn't initializing this value...
                        #endif
      #endif
   }



   static int relate4suspend( RELATE4SUSPEND *suspend )
   {
      // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
      #ifdef RELATE4LOG
         char buf[160] ;
         sprintf( buf, "relate4suspend: relCntVal: %ld\n", suspend->relCntVal ) ;
         relate4log( suspend->relate, buf ) ;
      #endif
      suspend->relCnt = suspend->relCntVal ;  // reset the iteration counter for next suspension
      #ifdef S4SERVER
         // we will leave the status of the connection as working, and just suspend ourselves temporarily
         // because the relation module uses its own DATA4's we should be ok, except for tags???   We do this currently
         // with the server and it appears to work ok
         code4exitExclusiveVerifyLast( suspend->client->server->c4, suspend->client ) ;
         u4delayHundredth( 1 ) ;
         code4enterExclusiveVerifyFirst( suspend->client->server->c4, suspend->client, 1 ) ;
         // AS Apr 19/04 - for odbc, the connection is not set up, and thus doesn't need to be set
         assert5( suspend->relCnt > 0 ) ;
         #ifndef S4OFF_COMMUNICATIONS
            // Now ensure our connection is still valid (if not return -1)
            if ( suspend->client->connect.connectBuffer.connectionFailed == 1 )
               return -1 ;
         #endif
         return 0 ;
      #else
         assert5( suspend->relate->callback != 0 ) ;
         return suspend->relate->callback( 0 ) ;
                        #ifdef S4WIN32
                                // CS 2011/05/18 ftime not supported on WinCE
                                suspend->relLastCallTime = GetTickCount();
                        #else
                 ftime( &suspend->relLastCallTime ) ;
              #endif
         assert5( suspend->relCnt > 0 ) ;
         return 0 ;
      #endif
   }


   static int relate4suspendCheck( RELATE4SUSPEND *suspend )
   {
      #ifdef S4STAND_ALONE
         // in stand-alone we try to be more accurate with our timing...
                        #ifdef S4WIN32
                                // CS 2011/05/18 timeb not supported in WinCE.
                                DWORD relCurrentTime = GetTickCount();
                                // elapsed time in millseconds
                 double elapsedTime = (double)relCurrentTime - (double)suspend->relLastCallTime ;
              #else
                                struct timeb relCurrentTime ;
                                ftime( &relCurrentTime ) ;
                                // elapsed time in millseconds
                                double elapsedTime = (double)relCurrentTime.time * 1000 + relCurrentTime.millitm - (double)suspend->relLastCallTime.time * 1000 - suspend->relLastCallTime.millitm ;
                        #endif

         if ( elapsedTime > suspend->relate->callbackMicroseconds )
         {
            // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
            #ifdef RELATE4LOG
               char buf[160] ;
               sprintf( buf, "relate4suspendCheck: elapsedTime: %f\n", elapsedTime ) ;
               relate4log( suspend->relate, buf ) ;
            #endif
                                #ifdef S4WIN32
                                        // CS 2011/05/18 ftime not supported on WinCE
                                        suspend->relLastCallTime = GetTickCount();
                                #else
                                        ftime( &suspend->relLastCallTime ) ;
                                #endif
            return relate4suspend( suspend ) ;
         }

         suspend->relCnt = suspend->relCntVal ;  // reset the iteration counter for next suspension
         assert5( suspend->relCnt > 0 ) ;
         return 0 ;
      #else
         // in server case just suspend
         return relate4suspend( suspend ) ;
      #endif
   }


   static int relate4suspendInitCount( RELATE4SUSPEND *suspend )
   {
      // at this point we are still trying to determine the # of iterations to execute in .25 of a second
      #ifdef S4SERVER
         long numMicroseconds = 250 ;
      #else
         long numMicroseconds = suspend->relate->callbackMicroseconds ;
      #endif

      #ifdef S4WIN32
         // CS 2011/05/18 timeb not supported in WinCE.
         DWORD relCurrentTime = GetTickCount();
         // elapsed time in millseconds
         double elapsedTime = (double)relCurrentTime - (double)suspend->relStartTime ;
      #else
         struct timeb relCurrentTime ;
         ftime( &relCurrentTime ) ;
         // elapsed time in millseconds
         double elapsedTime = (double)relCurrentTime.time * 1000 + relCurrentTime.millitm - (double)suspend->relStartTime.time * 1000 - suspend->relStartTime.millitm ;
      #endif

      // AS Feb 3/06 - add some logging to help diagnose relate suspend issues - Feb 28 - server fix
      #ifdef RELATE4LOG
         RELATE4 *relate = suspend->relate ;
         char buf[160] ;
         sprintf( buf, "relate4suspendInitCount: numMicroseconds = %ld, elapsedTime: %f\n", numMicroseconds, elapsedTime ) ;
         relate4log( relate, buf ) ;
      #endif

      if ( elapsedTime == 0 )  // double the incrementer and retry...generally this shouldn't happen
      {
         suspend->relCntStart = suspend->relCntStart * 2 ;
         suspend->relCnt = suspend->relCntStart ;
         assert5( suspend->relCnt > 0 ) ;
         // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
         #ifdef RELATE4LOG
            char buf[160] ;
            sprintf( buf, "doubling the incrementor since elapsed time was 0\n" ) ;
            relate4log( relate, buf ) ;
         #endif

         return 0 ;
      }

      assert5( elapsedTime != 0 ) ;
      // calculate how many we genereated in the time frame to adjust for 10ths of a second
      // AS Jan 31/06 - avoid a potential overflow on the multiplication...
      double recordsPerTenthSecond = ( (double)suspend->relCntStart * ((double)numMicroseconds / (double)elapsedTime)) ;  // (SUSPEND4START_CNT / (time in milliseconds)) * numMicroseconds to get into hundredths of seconds
      assert5( recordsPerTenthSecond >= 0 ) ;
      suspend->relCntVal = (long)recordsPerTenthSecond ;

      // AS Feb 3/06 - add some logging to help diagnose relate suspend issues
      #ifdef RELATE4LOG
         sprintf( buf, "   relate4suspendInitCount part 2: relCntStart: %ld, relcntVal: %ld relcnt: %ld, numInitCountAttempts: %ld, recordsPerTenthSecond: %f\n",
                  suspend->relCntStart, suspend->relCntVal, suspend->relCnt, (long)suspend->numInitCountAttempts, recordsPerTenthSecond ) ;
         relate4log( relate, buf ) ;
      #endif

      // we are done, what was the count?

      // don't suspend in this case if less time has elapsed than expected since the results are then poor
      if ( elapsedTime < numMicroseconds )
      {
         suspend->numInitCountAttempts++ ;
         // if we are within 20%, then this is ok to call the suspend...
         if ( suspend->numInitCountAttempts <= 3 && elapsedTime / numMicroseconds < .9 )  // in this case recalculate again...
         {
            // in this case we actually want to recalculate again after we finish the expected number of iterations...
            suspend->relCnt = suspend->relCntVal ;  // reset the iteration counter for next suspension
            suspend->relCntStart = suspend->relCntVal ;
                                #ifdef S4WIN32
                                        // CS 2011/05/18 ftime not supported on WinCE
                                        suspend->relStartTime = GetTickCount();
                                #else
                                        ftime( &suspend->relStartTime ) ;
                                #endif
            assert5( suspend->relCnt > 0 ) ;
            return 0 ;
         }
      }

      // also suspend in this case
      suspend->relCntSet = 1 ;

      #ifdef S4STAND_ALONE
         // what we actually do is divide the count by 5 and check at those intervals.  When we go over the clock cycle we suspend
         suspend->relCntStart /= 5 ;
      #endif
      return relate4suspend( suspend ) ;
   }
// #endif


static int relate4skipInternal( RELATE4 *relate, const long numSkip )
{
   int rc ;
   long numskip ;
   #ifndef S4CLIENT
      int sortStatus, rc2 ;
      signed char sign ;
      #ifdef S4REPORT
         #ifdef S4WINDOWS
            char countstring[22];
            static long int scanrecCount = 0, selectrecCount = 0, sortrecCount = 0;
            HWND statwin;
         #endif
      #endif
      #ifndef S4SINGLE
         char oldReadLock ;
      #endif
   #endif

   CODE4 *c4 = relate->codeBase ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;
   RELATION4 *relation = relate->relation ;

   numskip = numSkip ;
   // AS Aug 20/02 - check in client as well...
   if ( numskip < 0 )
   {
      if ( relation->skipBackwards == 0 )
         return error4( c4, e4relate, E84417 ) ;
   }

   #ifdef S4CLIENT
      #ifdef E4ANALYZE
         if ( relate->data == 0 )
            return error4( c4, e4struct, E94418 ) ;
         if ( relate->data->dataFile == 0 )
            return error4( c4, e4struct, E94418 ) ;
      #endif

      rc = relate4flush( relate ) ;
      if ( rc )
         return rc ;

      assert5port( "new client/server functionality to fetch relation records in batches" ) ;
      // AS Apr 12/02 block reading of records for relate skip...
      if ( relation->readRecsToBuf > 0 )
      {
         // check to see if the buffers have been allocated
         if ( relation->readAdvanceBuf == 0 )
            relation4readBufferAlloc( relation, relation->doMemos ) ;

         long skipOffset = 0 ;
         // check if we have the record bufferred
         long curPos = relation->readBufPos ;
         // AS Apr 23/03 - Was not considering case where relate4readBuffer() was called after relate4top()
         // check to see if we are valid at this point
         if ( curPos != -1 )    // -1 means buffer has been reset, and is empty.  Don't need to recalculate
         {
            if ( relation->readBufDirection == 1 )  // reading forwards...
               curPos += numSkip ;
            else
               curPos -= numSkip ;

            if ( curPos < relation->readBufNum && curPos >= 0 )  // in buffer
               return relation4skipFetchFromBuffer( relation, curPos ) ;

            int direction = 1 ;
            if ( numSkip < 0 )
               direction = -1 ;
            if ( relate->relation->readBufDirection != direction )
            {
               // case where we read the buffer in one direction but we are now skipping in the other
               // direction.  This is a special case because we don't want to continue reading the next
               // set in the same direction in the buffer.  Instead what we need to do is to skip
               // in the other direction in the buffer and then change direction...
               // AS Aug 26/02 - Not correct - if we change directions, we also need to
               // adjust the skipOffset by 1 less to ensure that we fetch the row we need
               // otherwise, since we normally start fetching AFTER the row, we will miss a record
               skipOffset = (relate->relation->readBufNum - 1) * direction ;
               numskip += relation->readBufPos ;   // adjust by subtracting the size of the input buffer from the amount to skip
            }
            else  // AS Apr 24/07 - wasn't taking into account that if we are skipping > 1, we don't need to fetch as many forward into the relation
            {
               numskip -= (relation->readBufNum - relation->readBufPos - 1 ) ;   // adjust by subtracting the size of the input buffer from the amount to skip
            }

            relation4readBufferReset( relation, 0 ) ;
         }
         return relation4skipFetchMultiple( relation, numskip, 0, skipOffset ) ;
      }
      else
         return relation4skipFetchSingle( relation, numskip ) ;
   #else
      if ( numskip < 0 )
         sign = -1 ;
      else
         sign = 1 ;

      sortStatus = 0 ;
      rc = 0 ;
      #ifndef S4SINGLE
         /* suspend auto read locking from within the relate */
         oldReadLock = c4getReadLock( c4 ) ;
         c4setReadLock( c4, 0 ) ;
      #endif

      // AS Apr 18/05 - new callback functionality - currently only for windows 32 bit...
      // #ifdef S4SERVER
         // AS Jan 23/04 - Support a suspension of performing the relate so other clients can perform actions
         // do this by checking every 250 iterations of .1 of a second has elapsed, if it has suspend.
         // we will adjust this value to get it close to possible
         // this needs to be on a client basis
         // RELATE4SUSPEND suspend ;
         #ifdef S4SERVER
            if ( c4->server->suspendQuery == 1 )
              relate4suspendInit( &relate->suspend, c4->currentClient ) ;
         #else
            if ( relate->callback != 0 )
              relate4suspendInit( &relate->suspend, relate ) ;
         #endif
      // #endif

      for( ; numskip ; )
      {
         #ifdef S4REPORT
            #ifdef S4WINDOWS
               if( GetWindowWord( c4->hWnd, 8 ) == 666 )
                  statwin = c4->hWnd ;

               if ( statwin )
               {
                  if ( GetWindowWord( statwin, 6 ) == 0 )
                  {
                     SetWindowWord( statwin, 6, 1 ) ;
                     scanrecCount = sortrecCount = selectrecCount = 0 ;
                  }

                  scanrecCount++ ;
                  if ( scanrecCount < 20 || ( scanrecCount % 20 ) == 0 )
                  {
                     c4ltoa45( scanrecCount, countstring, sizeof( countstring ) -1 ) ;
                     countstring[21] = 0 ;
                     SendMessage( (HWND)GetWindowWord( statwin, 0 ), WM_SETTEXT, 0, (LPARAM)((LPSTR)countstring ) ) ;
                  }
               }
            #endif
         #endif

         // AS Apr 18/05 - new callback functionality - currently only for windows 32 bit...
         // #ifdef S4SERVER
            // AS Jan 23/04 - Support suspension
            RELATE4 *master = &relate->relation->relate ;
            assert5( master->suspend.relCnt >= 0 ) ;
            #ifdef S4SERVER
               if ( c4->server->suspendQuery == 1 && ( --(master->suspend.relCnt) == 0 ) )
            #else
               if ( master->callback != 0 && ( --(master->suspend.relCnt) == 0 ) )
            #endif
            {
               if ( master->suspend.relCntSet == 0 )  // still determinnig iteration count
                  relate4suspendInitCount( &master->suspend ) ;
               else  // suspend now
               {
                  int rcRes = relate4suspendCheck( &master->suspend ) ;
                  if ( rcRes < 0 )
                  {
                     #ifdef S4SERVER
                        return e4connect ;
                     #else
                        return rcRes ;
                     #endif
                  }
               }
               assert5( master->suspend.relCnt > 0 ) ;
            }
         // #endif

         if ( sign > 0 )
         {
            rc = relate4nextScanRecord( relation ) ;
            if ( rc == r4eof )
               break ;
         }
         else
         {
            rc = relate4prevScanRecord( relation ) ;
            if ( rc == r4bof )
               break ;
         }

         #ifdef S4SINGLE
            if ( rc < 0 || rc == r4terminate )
         #else
            if ( rc < 0 || rc == r4locked || rc == r4terminate )
         #endif
               break ;

         rc = relate4readRest( relate, sign ) ;
         if ( rc == relate4filterRecord )
            continue ;

         if ( rc < 0 || rc == r4terminate )
            break ;

         if ( relation->exprSource )
         {
            rc2 = log4true(&relation->log ) ;
            if ( rc2 == r4terminate )
            {
               rc = r4terminate ;
               break ;
            }
            if ( rc2 == 0 )
            {
               if ( relation->inSort == relate4sortSkip )  /* must temporarily disable in order to get a matching scan if available */
               {
                  sortStatus = 1 ;
                  relation->inSort = 0 ;
               }
               continue ;
            }
         }

         numskip -= sign ;
      }
      #ifndef S4SINGLE
         /* suspend auto read locking from within the relate */
         c4setReadLock( c4, oldReadLock ) ;
      #endif

      #ifdef S4WINDOWS
         #ifdef S4REPORT
            if(GetWindowWord( c4->hWnd, 8 ) == 666 )
               statwin = c4->hWnd;

            if ( statwin )
            {
               selectrecCount++;
               if ( selectrecCount < 20 || (selectrecCount % 20) == 0 )
               {
                  c4ltoa45(selectrecCount,countstring,sizeof(countstring)-1);
                  countstring[21] = 0;
                  SendMessage((HWND)GetWindowWord(statwin,2),WM_SETTEXT,0,(LPARAM)((LPSTR)countstring));
               }
               if ( relation->inSort )
               {
                  sortrecCount++;
                  if( sortrecCount < 20 || (sortrecCount % 20) == 0)
                  {
                     c4ltoa45(sortrecCount,countstring,sizeof(countstring)-1);
                     countstring[21] = 0;
                     SendMessage((HWND)GetWindowWord(statwin,4),WM_SETTEXT,0,(LPARAM)((LPSTR)countstring));
                  }
               }
            }
         #endif
      #endif

      if ( sortStatus == 1 )
         relation->inSort = relate4sortSkip ;
      // AS Apr 30/02 - was not setting in stand-alone case... - and not doing bof correct
      if ( relate->relation->inSort == relate4sortDone )
      {
         if ( rc == r4eof )
         {
            #ifndef S4CLIENT
               // AS 10/23/00 - in client/server, the client registers EOF if the
               // data recNum is > than the # of recs in the table, so put the recnum
               // forward to this value in the server case so it is returned out correctly
               d4goEof( relate->data ) ;
            #endif
            relate->relation->sortEofFlag = r4eof ;
         }
         #ifndef S4CLIENT
            if ( rc == r4bof )
               d4goBof( relate->data ) ;
         #endif
      }

      return rc ;
   #endif
}


// AS Apr 18/05 - new callback functionality - currently only for windows 32 bit...
int S4FUNCTION relate4skip( RELATE4 *relate, const long numSkip )
{
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94418 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94418 ) ;
   #endif

   RELATION4 *relation = relate->relation ;

   if ( relation->isInitialized == 0 )
      return error4( relate->codeBase, e4relate, E84406 ) ;

   relate = &relation->relate ;

   relate->suspend.isInit = 0 ;
   return relate4skipInternal( relate, numSkip ) ;
}


int S4FUNCTION relate4skipEnable( RELATE4 *relate, const int doEnable )
{
   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94412 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94412 ) )
         return -1 ;
   #endif

   if ( error4code( relate->codeBase ) < 0 )
      return -1 ;

   if ( relate->relation->skipBackwards != (char)doEnable )
   {
      relate->relation->skipBackwards = (char)doEnable ;
      relate4changed( relate ) ;
   }
   return 0 ;
}



static void relate4sortFree( RELATION4 *relation, const int deleteSort )
{
   if ( relation == 0 )
      return ;

   #ifndef S4CLIENT
      sort4free( &relation->sort ) ;
      u4free( relation->otherData ) ;
      relation->otherData = 0 ;
      // AS Sep 3/03 - If the file handle is invalid but the file exists (is temporary in-memory) close as well.
      if ( relation->sortedFile.hand != INVALID4HANDLE || ( relation->sortedFile.isTemporary == 1
         #ifndef S4OFF_OPTIMIZE  // LY Dec 9/03
            && relation->sortedFile.fileCreated == 0
         #endif
         ) )
         file4close( &relation->sortedFile ) ;
      r4dataListFree( &relation->sortDataList ) ;
      relation->inSort = 0 ;
   #endif
   if ( deleteSort )
   {
      u4free( relation->sortSource ) ;
      relation->sortSource = 0 ;
      #ifndef S4CLIENT
         if ( relation->sortExpr != 0 )
         {
            expr4free( relation->sortExpr ) ;
            relation->sortExpr = 0 ;
         }
      #endif
   }
}



#ifndef S4CLIENT
   /* !S4CLIENT */
   static int relate4sort( RELATE4 *relate )
   {
      int rc, i, sortLen ;
      /* LY 2001/07/20 long j, zero ; */
      S4LONG j, zero ;
      char nDbf, *sortKey ;
      R4DATA_LIST *r4data ;
      RELATION4 *relation ;
      CODE4 *c4 ;
      #ifdef S4SERVER
         LIST4 *oldList ;
      #endif

      // cannot perform the actual sort while in the 'inCount' process because we need
      // to read and record the record number for all slaves in this case.
      assert5( relate->relation->countOnly == 0 ) ;

      zero = 0L ;

      #ifdef E4PARM_LOW
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94419 ) ;
      #endif

      c4 = relate->codeBase ;

      #ifdef E4ANALYZE
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;
      #endif

      relation = relate->relation ;
      relate = &relation->relate ;
      rc = 0 ;
      #ifdef S4SERVER
         oldList = tran4dataList( code4trans( c4 ) ) ;
         tran4dataListSet( code4trans( c4 ), &relation->localDataList ) ;
      #endif
      if ( relation->sortExpr )
      {
         expr4free( relation->sortExpr ) ;
         relation->sortExpr = 0 ;
      }
      assert5( relation->sortExpr == 0 ) ;
      // AS May 12/06 - need to pass the tag in for general collation to work with ascend...
      if ( relate->sortTag != 0 )
         relation->sortExpr = expr4parseLow( relate->data, relation->sortSource, relate->sortTag->tagFile ) ;
      else
         relation->sortExpr = expr4parseLow( relate->data, relation->sortSource, 0 ) ;
      #ifdef S4SERVER
         tran4dataListSet( code4trans( c4 ), oldList ) ;
      #endif

      relation->inSort = relate4sortSkip ;
      relation->sortDoneFlag = 0 ;

      rc = relate4top( relate ) ;
      if ( rc )   /* no records satisfy the relate, or error */
         return rc ;

      expr4context( relation->sortExpr, relate->relation->relate.data ) ;

      /* AS sort has nothing to do with tag and ordering because tag is not coming into this.
      if ( relate->dataTag != 0 )
         sortLen = expr4key( relation->sortExpr, &sortKey, relate->dataTag->tagFile ) ;
      else
      */
      // AS Added this in by calling a function to imply a tag ordering...basically it add's support for things like
      // general sort collations
      if ( relate->sortTag != 0 )
         sortLen = expr4key( relation->sortExpr, &sortKey, relate->sortTag->tagFile ) ;
      else
         sortLen = expr4key( relation->sortExpr, &sortKey, 0 ) ;
      if ( sortLen <= 0 )
         return error4( c4, e4relate, E94419 ) ;

      // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
      relate->relation->sortKeyLen = sortLen ;

      #ifdef E4ANALYZE
         if ( relation->sortDataList.nLink != 0 )
            return error4( c4, e4struct, E84413 ) ;
      #endif

      rc = r4dataListBuild( &relation->sortDataList, relate, relation->sortExpr, relate4exact ) ;
      if ( rc < 0 )
         return rc ;

      rc = r4dataListMassage( &relation->sortDataList ) ;
      if ( rc < 0 )
         return rc ;

      nDbf = (char)relation->sortDataList.nLink ;

      relation->sortOtherLen = (int)(nDbf * sizeof(S4LONG )) ;

      relation->otherData = (char *)u4alloc( (long)relation->sortOtherLen ) ;
      if ( relation->otherData == 0 )
         return e4memory ;

      rc = sort4initFree( &relation->sort, c4, sortLen, relation->sortOtherLen, relate ) ;
      if ( rc )
         return rc ;

      #ifdef S4MDX
         switch( expr4type( relation->sortExpr ) )
         {
            // AS Apr 27/04 - also applies if r4numDoub (for example an expression involving a character field)
            case r4numDoub:
            case r4num:
               relation->sort.cmp = (S4CMP_FUNCTION *)c4bcdCmp ;
               break ;
            case r4date:
               relation->sort.cmp = (S4CMP_FUNCTION *)t4cmpDoub ;
               break ;
            default:
               break ;
         }
      #endif

      /* call relate4top() again in case free-ups occurred */
      rc = relate4top( relate ) ;
      if ( rc )   /* no records satisfy the relate, or error */
         return rc ;

      // AS Apr 18/05 - new callback functionality - currently only for windows 32 bit...
      // #ifdef S4SERVER
         // AS Jan 23/04 - Support a suspension of performing the relate so other clients can perform actions
         // RELATE4SUSPEND suspend ;
         #ifdef S4SERVER
            if ( c4->server->suspendQuery == 1 )
              relate4suspendInit( &relate->suspend, c4->currentClient ) ;
         #else
            if ( relate->callback != 0 )
              relate4suspendInit( &relate->suspend, relate ) ;
         #endif
      // #endif

      for ( j = 0L, rc = 0 ; !rc ; j++, rc = relate4skipInternal( relate, 1L ) )
      {
         // #ifdef S4SERVER
            // AS Jan 23/04 - Support suspension
            RELATE4 *master = &relate->relation->relate ;
            assert5( master->suspend.relCnt >= 0 ) ;
            #ifdef S4SERVER
               if ( c4->server->suspendQuery == 1 && ( --(master->suspend.relCnt) == 0 ) )
            #else
               if ( master->callback != 0 && ( --(master->suspend.relCnt) == 0 ) )
            #endif
            {
               if ( master->suspend.relCntSet == 0 )  // still determinnig iteration count
                  relate4suspendInitCount( &master->suspend ) ;
               else  // suspend now
               {
                  int rcRes = relate4suspendCheck( &master->suspend ) ;
                  if ( rcRes < 0 )
                  {
                     #ifdef S4SERVER
                        return e4connect ;
                     #else
                        return rcRes ;
                     #endif
                  }
               }
            }
         // #endif

         for ( i = 0, r4data = 0 ;; i++ )
         {
            r4data = (R4DATA_LIST *)l4next( &relation->sortDataList, r4data ) ;
            if ( r4data == 0 )
               break ;
            if ( d4eof( r4data->data ) || d4bof( r4data->data ) )   /* relate4blank case */
               c4memcpy( relation->otherData + i * sizeof(S4LONG), (void *)&zero, sizeof(S4LONG ) ) ;
            else
               c4memcpy( relation->otherData + i * sizeof(S4LONG), (void *)&r4data->data->recNum, sizeof(S4LONG) ) ;
         }

         /* AS sort has nothing to do with tag and ordering because tag is not coming into this.
            if ( expr4key( relation->sortExpr, &sortKey, relate->dataTag == 0 ? 0 : relate->dataTag->tagFile ) < 0 ) */
         // AS Added this in by calling a function to imply a tag ordering...basically it add's support for things like
         // general sort collations
         if ( relate->sortTag != 0 )
            rc = expr4key( relation->sortExpr, &sortKey, relate->sortTag->tagFile ) ;
         else
            rc = expr4key( relation->sortExpr, &sortKey, 0 ) ;
         if ( rc < 0 )
         {
            u4free( relation->otherData ) ;
            relation->otherData = 0 ;
            error4( c4, e4relate, E94419 ) ;
            return rc ;
         }

         rc = sort4put( &relation->sort, j, sortKey, relation->otherData ) ;
         if ( rc < 0 )
         {
            u4free( relation->otherData ) ;
            relation->otherData = 0 ;
            return rc ;
         }
      }

      if ( rc < 0 || rc == r4terminate )
      {
         u4free( relation->otherData ) ;
         relation->otherData = 0 ;
         return rc ;
      }

      relation->sortRecCount = j ;
      relation->inSort = relate4sortDone ;

      if ( relation->skipBackwards )
      {
         // AS May 24/02 - created file4createInternal for internal use to indicate file types
         rc = file4tempLow( &relation->sortedFile, c4, 1, 1, 0, OPT4NONE ) ;
         if ( rc < 0 )
         {
            u4free( relation->otherData ) ;
            relation->otherData = 0 ;
            return rc ;
         }
      }

      rc = sort4getInitFree( &relation->sort, relate ) ;
      if ( rc < 0 )
         return rc ;

      relation->sortRecOn = relation->sortFilePos = relation->sortRecTo = 0L ;

      return 0 ;
   }



   /* !S4CLIENT */
   static int relate4sortGetRecord( RELATION4 *relation, const long num, char *sortedKey, Bool5 repositionTables, long *masterRecno )
   {
      // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relate4doAll
      // AS Apr 8/02 - the sortedKey paramter, if not null, will store the sorted key in that location...
      // if repositionTables is false, the tables records are not read in, only the sortedKey is filled
      int len, i, rc ;
      char *key ;
      char *other = 0 ;
      R4DATA_LIST *linkOn ;
      S4LONG j, numLeft ;  /* L.Y. 1999/2/5 : sort4get() takes S4LONG* as 2nd param, which may be int* under 64-bit */
      FILE4LONG pos ;
      #ifdef S4DATA_ALIGN
         S4LONG longPtr ;
      #endif

      if ( error4code( relation->relate.codeBase ) < 0 )
         return -1 ;

      if ( num <= 0 )
         return r4bof ;

      relation->sortEofFlag = 0 ;
      numLeft = num - relation->sortRecTo ;

      if ( numLeft <= 0 )  /* already read, so just return from file */
      {
         if ( relation->skipBackwards == 0 )
            return error4( relation->relate.codeBase, e4relate, E84417 ) ;

         // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
         // in this case we store the sort key as well, so must adjust for that
         if ( relation->retainRelation == 1 )
         {
            if ( sortedKey != 0 )
            {
               /* LY July 7/03 : changed from 0 to 0L */
               file4longAssign( pos, ( num - 1 ) * (relation->sortOtherLen + relation->sortKeyLen), 0L ) ;
               len = file4readInternal( &relation->sortedFile, pos, sortedKey, (unsigned int)relation->sortKeyLen ) ;
               if ( len != relation->sortKeyLen )  /* free up and exit */
                  return -1 ;
            }
            /* LY July 7/03 : changed from 0 to 0L */
            file4longAssign( pos, ( num - 1 ) * (relation->sortOtherLen + relation->sortKeyLen) + relation->sortKeyLen, 0L ) ;
         }
         else
         {
            /* LY July 7/03 : changed from 0 to 0L */
            file4longAssign( pos, ( num - 1 ) * relation->sortOtherLen, 0L ) ;
         }

         len = file4readInternal( &relation->sortedFile, pos, relation->otherData, (unsigned int)relation->sortOtherLen ) ;
         if ( len != relation->sortOtherLen )  /* free up and exit */
            return -1 ;
         other = relation->otherData ;
      }
      else
      {
         while ( numLeft-- )
         {
            if ( relation->sortDoneFlag == 1 )  /* sort is finished, therefore must be eof */
               return r4eof ;

            rc = sort4get( &relation->sort, &j, (void **)&key, (void **)&other ) ;
            if ( rc )  /* no more items, or error */
            {
               sort4free( &relation->sort ) ;

               if ( rc == r4done )
               {
                  relation->sortEofFlag = r4eof ;
                  relation->sortDoneFlag = 1 ;
                  return r4eof ;
               }
               else
                  return rc ;
            }

            relation->sortRecTo++ ;

            if ( relation->skipBackwards )
            {
               /* LY July 7/03 : changed from 0 to 0L */
               file4longAssign( pos, relation->sortFilePos, 0L ) ;

               // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
               // we need to store the sort key as well so that we can perform seeks...
               if ( relation->retainRelation == 1 )
               {
                  file4writeInternal( &relation->sortedFile, pos, key, (unsigned int)relation->sortKeyLen ) ;
                  relation->sortFilePos += relation->sortKeyLen ;
                  /* LY July 7/03 : changed from 0 to 0L */
                  file4longAssign( pos, relation->sortFilePos, 0L ) ;
                  if ( sortedKey != 0 )
                     memcpy( sortedKey, key, relation->sortKeyLen ) ;
               }

               file4writeInternal( &relation->sortedFile, pos, other, (unsigned int)relation->sortOtherLen ) ;
               relation->sortFilePos += relation->sortOtherLen ;
               // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
               // we need to store the sort key as well so that we can perform seeks...
            }
         }
      }

      if ( repositionTables == 0 )
      {
         if ( masterRecno != 0 )  // means we want this data returned
         {
            // normally the master table is at the end of the 'other' list, but we will need to ensure this...
            int position = relation->sortDataList.nLink - 1 ;
            long *recnoPtr = (long *)other ;
            for ( linkOn = (R4DATA_LIST *)l4last( &relation->sortDataList ) ;; )
            {
               if ( linkOn->data == relation->relate.data )
               {
                  *masterRecno = recnoPtr[position] ;
                  break ;
               }
               linkOn = (R4DATA_LIST *)l4prev( &relation->sortDataList, linkOn ) ;
               position-- ;

               if ( linkOn == 0 || position < 0 ) // impossible - not found
                  return error4( relation->relate.codeBase, e4info, E94417 ) ;
            }
         }
         return 0 ;
      }

      /* now read the database records in */
      for ( i = 0, linkOn = 0 ;; i++ )
      {
         linkOn = (R4DATA_LIST *)l4next( &relation->sortDataList, linkOn ) ;
         if ( linkOn == 0 )
            return 0 ;
         /* note that the sort positions all blanks to eof whereas in non-sort it is not
            consistent whether it is positioned bof or eof.  For sort it doesn't matter
            because further skips all go through the sort, therefore we already know the
            record order thus it doesn't need to be calculated.  This is a small efficiency. */
         #ifdef S4DATA_ALIGN
            memcpy( &longPtr, (void *)(other + i*sizeof(longPtr)), sizeof(longPtr) ) ;
            if ( longPtr == 0 )  /* relate4blank case */
               rc = d4goEof( linkOn->data ) ;
            else
               rc = d4go( linkOn->data, longPtr ) ;
         #else
            /* LY 2001/04/18 - replace long* with S4LONG* for 64-bit */
            if ( *((S4LONG*)(other) + i ) == 0 )  /* relate4blank case */
               rc = d4goEof( linkOn->data ) ;
            else
               rc = d4go( linkOn->data, *((S4LONG*)(other) + i ) ) ;
         #endif
         if ( rc < 0 )
            return rc ;

         linkOn->relate->isRead = 1 ;
      }

      return 0 ;
   }



   // AS Jun 24/02 - New function to skip forward in the master table only
   static int relate4sortSkipMaster( RELATION4 *relation, long numSkip )
   {
      assert5( relation->retainRelation == 1 ) ;   // should always be true if here

      // we just need to go through the sort data until the master record has skipped enough times...
      long currentMasterRecno = d4recNo( relation->relate.data ) ;
      long newRecno ;
      long sortRecOn = relation->sortRecOn ;
      int toAdd ;
      if ( numSkip > 0 )
         toAdd = 1 ;
      else
         toAdd = -1 ;

      while ( numSkip != 0 )
      {
         sortRecOn += toAdd ;
         int rc = relate4sortGetRecord( relation, sortRecOn, 0, 0, &newRecno ) ;
         if ( rc != 0 )
         {
            if ( rc == r4eof )
               relation->sortRecOn = relation->sortRecCount + 1 ;
            return rc ;
         }
         relation->sortRecOn = sortRecOn ;
         if ( newRecno != currentMasterRecno )
         {
            // means we did a master record shift
            numSkip -= toAdd ;
         }
      }

      // and now do the sort with actually position the records
      return relate4sortGetRecord( relation, relation->sortRecOn, 0, 1, 0 ) ;
   }



   /* !S4CLIENT */
   static int relate4sortSeek( RELATION4 *relation )
   {
      // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
      // it just does a look up into the seek for the current record number(s)
      assert5( relation->retainRelation == 1 ) ;   // should always be true if here

      if ( relation->sortRecCount == 0 )  //  means no records matched sort, so just eof
      {
         d4goEof( relation->relate.data ) ;
         return r4eof ;
      }

      // generate the sort key first
      if ( relation->sortedKeyBufferLen < relation->sortKeyLen )
      {
         if ( relation->sortedKeyBufferLen != 0 )
         {
            // u4free( relation->sortedKeyBuffer ) ;
            u4free( relation->sortedKeyBuffer2 ) ;
            // relation->sortedKeyBuffer = 0 ;
            relation->sortedKeyBuffer2 = 0 ;
            relation->sortedKeyBufferLen = 0 ;
         }

         assert5( relation->sortedKeyBuffer2 == 0 ) ;
         // relation->sortedKeyBuffer = (char *)u4allocFree( relation->relate.codeBase, relation->sortKeyLen ) ;
         relation->sortedKeyBuffer2 = (char *)u4allocFree( relation->relate.codeBase, relation->sortKeyLen ) ;
         // if ( relation->sortedKeyBuffer == 0 || relation->sortedKeyBuffer2 == 0 )
         if ( relation->sortedKeyBuffer2 == 0 )
            return e4memory ;
         relation->sortedKeyBufferLen = relation->sortKeyLen ;
      }

      char *keyPtr ;
      if ( expr4key( relation->sortExpr, &keyPtr, 0 ) < 0 )
         return -1 ;

      // we do a binary seek through the sort set...
      long loKeyOn = -1 ;    // the bottom record in the current binary seek set
      long hiKeyOn = relation->sortRecCount ;   // top record in the current binary seek set
      long keyOn = -2 ;
      long masterRecno ;   // need recno for duplicate matching
      for ( ;; )
      {
         #ifdef E4DEBUG
            long oldKeyOn = keyOn ;
         #endif
         keyOn = (loKeyOn + hiKeyOn) / 2 ;
         #ifdef E4DEBUG
            assert5( oldKeyOn != keyOn ) ;   // would mean key not changed...
         #endif
         int rc = relate4sortGetRecord( relation, keyOn+1, relation->sortedKeyBuffer2, 0, &masterRecno ) ;
         if ( rc != 0 )
            return rc ;
         // int compareRc = c4memcmp( relation->sortedKeyBuffer, relation->sortedKeyBuffer2, relation->sortKeyLen ) ;
         int compareRc = c4memcmp( keyPtr, relation->sortedKeyBuffer2, relation->sortKeyLen ) ;
         if ( compareRc == 0 )  // found
         {
            relation->sortRecOn = keyOn + 1 ;   // reset our position...
            // and fetch (position) all data files
            // need to consider the possibility of duplicates...
            if ( masterRecno == d4recNo( relation->relate.data ) )  // match found
               return relate4sortGetRecord( relation, relation->sortRecOn, relation->sortedKeyBuffer2, 1, 0 ) ;

            // if here, we matched the key but not the recno.  The actual record may be previous to our position or
            // after our position.  look backwards first, then forwards
            long saveKeyOn = keyOn ;
            long lowestPos = keyOn ;   // save the lowest position in case we don't find a match
            for ( ;; )
            {
               // skip backwards until the recno is found or the key has changed
               keyOn-- ;
               if ( keyOn < 0 )  // past bof
                  break ;
               rc = relate4sortGetRecord( relation, keyOn+1, relation->sortedKeyBuffer2, 0, &masterRecno ) ;
               if ( rc != 0 )
                  return rc ;
               if ( masterRecno == d4recNo( relation->relate.data ) )  // match found
               {
                  relation->sortRecOn = keyOn + 1 ;   // reset our position...
                  return relate4sortGetRecord( relation, relation->sortRecOn, relation->sortedKeyBuffer2, 1, 0 ) ;
               }
               compareRc = c4memcmp( keyPtr, relation->sortedKeyBuffer2, relation->sortKeyLen ) ;
               if ( compareRc != 0 )  // means gone to far back, no longer matches
                  break ;
               lowestPos = keyOn ;
            }
            // now try going forwards instead
            keyOn = saveKeyOn ;
            for ( ;; )
            {
               // skip backwards until the recno is found or the key has changed
               keyOn++ ;
               if ( keyOn > hiKeyOn )  // past previous high point
                  break ;
               rc = relate4sortGetRecord( relation, keyOn+1, relation->sortedKeyBuffer2, 0, &masterRecno ) ;
               if ( rc != 0 )
                  return rc ;
               if ( masterRecno == d4recNo( relation->relate.data ) )  // match found
               {
                  relation->sortRecOn = keyOn + 1 ;   // reset our position...
                  return relate4sortGetRecord( relation, relation->sortRecOn, relation->sortedKeyBuffer2, 1, 0 ) ;
               }
               compareRc = c4memcmp( keyPtr, relation->sortedKeyBuffer2, relation->sortKeyLen ) ;
               if ( compareRc != 0 )  // means gone to far back, no longer matches
                  break ;
            }
            // not found, just go to the lowest position in the table - means key matched but recno not (maybe unique continue)
            keyOn = lowestPos ;
            relation->sortRecOn = keyOn + 1 ;   // reset our position...
            return relate4sortGetRecord( relation, relation->sortRecOn, relation->sortedKeyBuffer2, 1, 0 ) ;
         }
         if ( compareRc < 0 )  // the stored value < current posiition
         {
            // assert5( hiKeyOn != keyOn ) ;
            hiKeyOn = keyOn ;
         }
         else // the stored value > current position
         {
            // assert5( loKeyOn != keyOn ) ;
            loKeyOn = keyOn ;
         }
         if ( loKeyOn >= (hiKeyOn - 1) )  // not found, r4after
         {
            relation->sortRecOn = hiKeyOn + 1 ;   // reset our position to hiKeyOn (for r4after)
            int rc = relate4sortGetRecord( relation, relation->sortRecOn, relation->sortedKeyBuffer2, 1, 0 ) ;
            if ( rc == r4eof )   // happens if after last record
            {
               d4goEof( relation->relate.data ) ;
               return r4eof ;
            }
            return r4after ;
         }
      }

      return r4eof ;  // not found and reached end of sort...
   }



   /* !S4CLIENT */
   static int relate4sortNextRecord( RELATION4 *relation )
   {
      int rc ;

      if ( error4code( relation->relate.codeBase ) < 0 )
         return e4codeBase ;

      rc = relate4sortGetRecord( relation, relation->sortRecOn + 1, 0, 1, 0 ) ;
      if ( rc == 0 )
         relation->sortRecOn++ ;
      if ( rc == r4eof )
         relation->sortRecOn = relation->sortRecCount + 1 ;
      return rc ;
   }



   /* !S4CLIENT */
   static int relate4sortPrevRecord( RELATION4 *relation )
   {
      int rc ;

      if ( error4code( relation->relate.codeBase ) < 0 )
         return e4codeBase ;

      rc = relate4sortGetRecord( relation, relation->sortRecOn - 1, 0, 1, 0 ) ;
      if ( rc == 0 )
         relation->sortRecOn-- ;
      return rc ;
   }
#endif



int S4FUNCTION relate4sortSet( RELATE4 *relate, const char *expr )
{
   RELATION4 *relation ;
   int len ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94429 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94429 ) )
         return -1 ;
   #endif

   if ( error4code( relate->codeBase ) < 0 )
      return -1 ;

   relation = relate->relation ;
   relate = &relation->relate ;
   relate4changed( relate ) ;
   u4free( relation->sortSource ) ;
   relation->sortSource = 0 ;

   #ifndef S4CLIENT  /* LY 2002/06/27 */
      // AS Jun 21/02 - After timmerman change's, free sorts...
      if ( relation->sortExpr != 0 )
      {
         expr4free( relation->sortExpr ) ;
         relation->sortExpr = 0 ;
      }
   #endif

   if ( expr )
   {
      if ( expr[0] )
      {

         #ifndef S4SERVER
            int rc = relate4verifyExpression( relate, expr ) ;
            if ( rc < 0 )
               return rc ;
         #endif
         len = c4strlen( expr ) ;
         assert5( relation->sortSource == 0 ) ;
         relation->sortSource = (char *)u4allocEr( relate->codeBase, (long)len + 1L ) ;
         if ( relation->sortSource == 0 )
            return -1 ;
         c4memcpy( relation->sortSource, expr, (unsigned int)len ) ;
      }
   }

   return 0 ;
}



// AS Oct 28/05 - support for foreign languages...
int S4FUNCTION relate4sortSetTag( RELATE4 *relate, TAG4 *tag )
{
   #ifdef E4PARM_HIGH
      if ( relate == 0 || tag == 0 )
         return error4( 0, e4parm_null, E94429 ) ;
   #endif
   relate->sortTag = tag ;
   return 0 ;
}



#ifdef S4CLIENT
   static int relate4add( CONNECTION4 *connection, RELATE4 *relate, unsigned int *relatePos, unsigned short *flexPos, char *relateData )
   {
      int len, savePos ;
      RELATE4 *slaveOn ;
      CONNECTION4RELATE *info ;
      TAG4 *tag ;

      /* add this relate's info */
      savePos = *relatePos ;
      info = (CONNECTION4RELATE *)( relateData + *relatePos ) ;
      info->matchLen = htons5(relate->matchLen) ;
      info->relationType = htons5(relate->relationType) ;
      info->errorAction = htons5(relate->errorAction) ;
      assert5( relate->slaves.nLink < USHRT_MAX ) ;  // ensure no overflow on assigning to unsigned short below
      info->numSlaves = htons5((unsigned short)relate->slaves.nLink) ;
      info->clientId = htonl5(data4clientId( relate->data )) ;

      tag = relate->dataTag ;
      if ( tag == 0 )
         info->dataTagName.offset = 0 ;
      else
      {
         len = strlen( tag->tagFile->alias ) + 1 ;
         info->dataTagName.offset = htons5(*flexPos) ;
         connection4addData( connection, tag->tagFile->alias, len, 0 ) ;
         *flexPos += len ;
      }

      if ( relate->masterExpr == 0 )
         info->masterExpr.offset = 0 ;
      else
      {
         len = strlen( relate->masterExpr->source ) + 1 ;
         info->masterExpr.offset = htons5(*flexPos) ;
         connection4addData( connection, relate->masterExpr->source, len, 0 ) ;
         *flexPos += len ;
      }

      if ( relate->data == 0 )
         info->dataAccessName.offset = 0 ;
      else
      {
         len = strlen( dfile4name( relate->data->dataFile ) ) + 1 ;
         info->dataAccessName.offset = htons5(*flexPos) ;
         connection4addData( connection, dfile4name( relate->data->dataFile ), len, 0 ) ;
         *flexPos += len ;
      }

      if ( relate->data == 0 )
         info->dataAliasName.offset = 0 ;
      else
      {
         if ( relate->data->aliasSet == 0 )
            info->dataAliasName.offset = 0 ;
         else
         {
            len = strlen( d4alias( relate->data ) ) + 1 ;
            info->dataAliasName.offset = htons5(*flexPos) ;
            connection4addData( connection, d4alias( relate->data ), len, 0 ) ;
            *flexPos += len ;
         }
      }

      *relatePos += sizeof( CONNECTION4RELATE ) ;

      /* and do it's slaves:  */
      for ( slaveOn = 0 ;; )
      {
         slaveOn = (RELATE4 *)l4next( &relate->slaves, slaveOn ) ;
         if ( slaveOn == 0 )
            break ;
         relate4add( connection, slaveOn, relatePos, flexPos, relateData ) ;
      }

      return 0 ;
   }



   int relate4clientInit( RELATE4 *master )
   {
      CONNECTION4RELATE_INIT_INFO_IN *info ;
      CONNECTION4RELATE_INIT_INFO_OUT *out ;
      int rc, i ;
      unsigned short relateCount ;
      RELATE4 *relateOn ;
      CONNECTION4 *connection ;
      unsigned int relatePos, sortLen ;
      unsigned short flexPos, flexOffset, exprLen, relateOffset ;
      char *relateData ;
      CODE4 *c4 ;

      #ifdef E4PARM_LOW
         if ( master == 0 )
            return error4( 0, e4parm_null, E94421 ) ;
      #endif

      RELATION4 *relation = master->relation ;

      c4 = master->codeBase ;
      connection = master->data->dataFile->connection ;
      relateCount = 1 ;

      for( relateOn = master ;; relateCount++ )
      {
         if ( relate4next( &relateOn ) == 2 )
            break ;
         if ( relateOn->data != 0 )
         {
            if ( connection == 0 )
               connection = relateOn->data->dataFile->connection ;
            else
               if ( connection != relateOn->data->dataFile->connection )   /* multi-servers not supported on relations */
                  return error4( c4, e4notSupported, E84414 ) ;
         }
      }

      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( c4, e4struct, E94421 ) ;
      #endif
      if ( relateCount == 0 || connection == 0 )
         return error4( c4, e4relate, E84415 ) ;

      connection4assign( connection, CON4RELATE_INIT, 0, 0 ) ;

      connection4addData( connection, 0, sizeof( CONNECTION4RELATE_INIT_INFO_IN ), (void **)&info ) ;
      if ( relation->needsFreeing == 1 )
         info->relationId = htonl5(relation->relationId) ;
      else
         info->relationId = 0 ;
      relateOffset = sizeof( CONNECTION4RELATE_INIT_INFO_IN ) ;
      info->relateOffset = htons5(relateOffset) ;
      flexOffset = relateOffset + relateCount * sizeof( CONNECTION4RELATE ) ;
      info->flexOffset = htons5(flexOffset ) ;
      // AS May 1/02 - added support for leaving the relation status unchanged, to allow positioning within relat4edoAll
      info->retainRelation = htons5( relation->retainRelation ) ;
      info->relation.skipBackwards = htons5(relation->skipBackwards) ;
      info->bitmapDisable = htons5(relation->bitmapDisable) ;
      info->masterClientId = htonl5(master->data->clientId) ;
      if ( relation->exprSource == 0 )
      {
         info->relation.exprSource.offset = 0 ;
         exprLen = 0 ;
      }
      else
      {
         exprLen = strlen( relation->exprSource ) + 1 ;
         info->relation.exprSource.offset = info->flexOffset ;
      }
      if ( relation->sortSource == 0 )
      {
         info->relation.sortSource.offset = 0 ;
         sortLen = 0 ;
      }
      else
      {
         sortLen = (unsigned int)strlen( relation->sortSource ) + 1 ;
         info->relation.sortSource.offset = htons5((unsigned short)(flexOffset + exprLen)) ;
      }

      connection4addData( connection, 0, relateCount * sizeof( CONNECTION4RELATE ), (void **)&relateData ) ;
      if ( exprLen != 0 )
         connection4addData( connection, relation->exprSource, exprLen, 0 ) ;
      if ( sortLen != 0 )
         connection4addData( connection, relation->sortSource, sortLen, 0 ) ;

      relatePos = 0 ;
      flexPos = flexOffset + exprLen + sortLen ;
      rc = relate4add( connection, master, &relatePos, &flexPos, relateData ) ;
      if ( rc < 0 )
      {
         /* u4free( relateData ) ; */
         return error4stack( c4, rc, E94421 ) ;
      }

      connection4sendMessage( connection ) ;
      /* u4free( relateData ) ;*/
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E94421 ) ;
      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E84401 ) ;
      if ( rc != 0 )
         return rc ;
      if ( connection4len( connection ) != (long)sizeof( CONNECTION4RELATE_INIT_INFO_OUT ) + relateCount * (long)sizeof( relateOn->id ) )
         return error4stack( c4, e4packetLen, E94421 ) ;
      out = (CONNECTION4RELATE_INIT_INFO_OUT *)connection4data( connection ) ;
      relation->relationId = ntohl5(out->relationId) ;

      #ifdef E4ANALYZE
         if ( sizeof( relateOn->id ) != sizeof( unsigned short int ) )
            return error4( c4, e4struct, E94421 ) ;
      #endif

      out++ ;   /* go to the end of out for the variable length data */

      for( relateOn = master, i = 0 ;; i++ )
      {
         if ( relateOn == 0 )
            break ;
         relateOn->id = ntohs5(*((unsigned short int *)(((char *)out) + i * sizeof( relateOn->id ) ) )) ;
         rc = relate4next( &relateOn ) ;
         if ( rc == 2 )
            break ;
      }

      relation->isInitialized = 1 ;

      return 0 ;
   }
#endif



#ifndef S4CLIENT
   static int relate4topInit( RELATE4 *relate )
   {
      RELATION4 *relation ;
      int rc ;
      CODE4 *c4 ;
      #ifndef S4OPTIMIZE_OFF
         int has_opt ;
      #endif
      #ifdef S4SERVER
         LIST4 *oldList ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94422 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if( relate == 0 )
            return error4( 0, e4parm_null, E94422 ) ;
      #endif

      c4 = relate->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      relation = relate->relation ;
      relate = &relation->relate ;

      rc = 0 ;

      if ( relation->inSort == relate4sortDone )
         if ( relation->skipBackwards == 0 )
            relate4changed( relate ) ;

      relate->dataTag = relate->data->tagSelected ;
      if ( relation->isInitialized == 0 )
      {
         #ifndef S4OPTIMIZE_OFF
            has_opt = (char)c4->hasOpt ;
         #endif
         // AS July 29/02 - not possible for rc to != 0 at this point...
         // if ( rc < 0 )
         //    return rc ;
         relation->bitmapsFreed = 0 ;
         if ( relation->exprSource )
         {
            #ifdef S4SERVER
               oldList = tran4dataList( code4trans( c4 ) ) ;
               tran4dataListSet( code4trans( c4 ), &relation->localDataList ) ;
            #endif
            relation->log.expr = expr4parseLow( relate->data, relation->exprSource, 0 ) ;
            #ifdef S4SERVER
               tran4dataListSet( code4trans( c4 ), oldList ) ;
            #endif
            if ( relation->log.expr == 0 )
               return error4( c4, e4relate, E83703 ) ;
            if ( expr4type( relation->log.expr ) != r4log )
               return error4 ( c4, e4relate, E80905 ) ;

            if ( log4bitmapDo( &relation->log ) < 0 )
               relation->bitmapsFreed = 1 ;
            rc = log4determineEvaluationOrder( &relation->log ) ;
            if ( rc < 0 )
               return rc ;
         }

         rc = relate4buildScanList( 0, relate, relation ) ;
         if ( rc < 0 )
            return rc ;

         relation->isInitialized = 1 ;
         if ( relation->sortSource )
         {
            // AS July 2/02 - must not be marked as 'incount' for this process because we need to
            // record the slave record numbers.
            int oldInCount = relation->countOnly ;
            relation->countOnly = 0 ;
            rc = relate4sort( relate ) ;
            relation->countOnly = oldInCount ;
            if ( rc < 0 || rc == r4terminate )
               return rc ;
         }

         #ifndef S4OPTIMIZE_OFF
            if ( has_opt )
               code4optRestart( c4 ) ;
         #endif
      }

      return 0 ;
   }
#endif



int S4FUNCTION relate4top( RELATE4 *relate )
{
   RELATION4 *relation ;
   int rc ;
   CODE4 *c4 ;
   DATA4 *d4 ;
   #ifdef S4CLIENT
      CONNECTION4RELATE_TOP_INFO_IN *info ;
      CONNECTION4 *connection ;
      int saveRc ;
   #else
      long rec ;
      int rc2 ;
      #ifndef S4OFF_MULTI
         char oldReadLock ;
      #endif
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94422 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if( relate == 0 )
         return error4( 0, e4parm_null, E94422 ) ;
   #endif

   c4 = relate->codeBase ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   // AS Feb 6/06 - add some logging to help diagnose relate suspend issues
   #ifdef RELATE4LOG
      char buf[160] ;
      sprintf( buf, "relate4top\n" ) ;
      ftime( &c4->statusTime ) ;
      relate4log( relate, buf ) ;
   #endif

   #ifdef E4ANALYZE
      // AS Jun 26/02 code to allow for auto-count for testing from relate4top/relate4bottom
      relate->suspend.isInit = 0 ;
      if ( relate->relation->isInitialized == 0 )
         relate4doSpecialCount( relate ) ;
   #endif

   relation = relate->relation ;
   relate = &relation->relate ;
   relate->suspend.isInit = 0 ;
   d4 = relate->data ;

   rc = 0 ;

   #ifdef S4CLIENT
      #ifdef E4ANALYZE
         if ( d4 == 0 )
            return error4( c4, e4struct, E94422 ) ;
         if ( d4->dataFile == 0 )
            return error4( c4, e4struct, E94422 ) ;
      #endif
      // AS Jun 12/02 - bug here - if tag has changed, call relate4changed() instead of init
      // directly since otherwise certain uninitialization was not being performed
      if ( relate->dataTag != d4->tagSelected )
         relate4changed( relate ) ;

      if ( relation->isInitialized == 0 )
      {
         relate->dataTag = d4->tagSelected ;
         rc = relate4clientInit( relate ) ;
         if ( rc != 0 )
            return rc ;
      }
      #ifdef S4CB51
         #ifndef S4OFF_MULTI
            if ( c4getReadLock( c4 ) )
            {
               rc = relate4lock( relate ) ;
               if ( rc != 0 )
                  return rc ;
            }
         #endif
      #endif
      connection = d4->dataFile->connection ;
      #ifdef E4ANALYZE
         if ( connection == 0 )
            return error4( c4, e4struct, E94422 ) ;
      #endif

      rc = relate4flush( relate ) ;
      if ( rc )
         return rc ;

      // AS Apr 12/02 block reading of records for relate skip...
      assert5port( "new client/server functionality to fetch relation records in batches" ) ;
      if ( relation->readRecsToBuf > 0 )
      {
         relation4readBufferReset( relation, 0 ) ;
         return relation4skipFetchMultiple( relation, 0, 1, 0 ) ;  // '1' means top
      }
      else
      {
         connection4assign( connection, CON4RELATE_TOP, 0, 0 ) ;
         connection4addData( connection, 0, sizeof( CONNECTION4RELATE_TOP_INFO_IN ), (void **)&info ) ;
         info->relationId = htonl5(relation->relationId) ;
         // AS 06/15/00 - added general collating relation tag support in...
         info->useGeneralTagsInRelate = htons5( c4->useGeneralTagsInRelate ) ;
         // AS May 21/02 - code to support auto-transfer of memo fields
         assert5port( "added optional transfer of memo fields with client/server communications" ) ;
         info->includeMemos = relation->includeMemos ;
         connection4sendMessage( connection ) ;
         saveRc = connection4receiveMessage( connection ) ;
         if ( saveRc < 0 )
            return error4stack( c4, saveRc, E94422 ) ;
         saveRc = connection4status( connection ) ;

         if ( saveRc < 0 )
         {
            // AS Aug 6/02 - Ensure that server frees up info as well...
            relate4freeServer( relate ) ;
            relation->isInitialized = 0 ;
            if ( saveRc < 0 )
               return connection4error( connection, c4, saveRc, E94422 ) ;
         }

         rc = relation4unpack( relation, connection ) ;
         if ( rc != 0 )
            return rc ;
         if ( saveRc == r4terminate )
         {
            // don't call freeServer until after the unpack since freeServer sends a new message to the server
            // AS Aug 6/02 - Ensure that server frees up info as well...
            relate4freeServer( relate ) ;
            relation->isInitialized = 0 ;
         }

         return saveRc ;
      }
   #else
      #ifndef S4OFF_MULTI
         oldReadLock = c4getReadLock( c4 ) ;
         c4setReadLock( c4, 0 ) ;
      #endif

      for ( ;; )  /* used to minimize return code areas, just break out... */
      {
         rc = relate4topInit( relate ) ;
         // AS Feb 6/06 - add some logging to help diagnose relate suspend issues
         #ifdef RELATE4LOG
            char buf[160] ;
            struct timeb currentTime ;
            ftime( &currentTime ) ;
            double elapsedTime = (double)currentTime.time * 1000 + currentTime.millitm - (double)c4->statusTime.time * 1000 - c4->statusTime.millitm ;
            sprintf( buf, "relate4topInit completed: elapsed time: %f\n", elapsedTime ) ;
            ftime( &c4->statusTime ) ;
            relate4log( relate, buf ) ;
         #endif

         if (  rc != 0 )
            break ;

         relate4setNotRead( relate ) ;

         relation->currentRelateLevel = 0 ;
         relate4nextRelationList( relation, 1 ) ;

         if ( relation->inSort == relate4sortDone )
         {
            relation->sortRecOn = 0 ;
            rc = relate4sortNextRecord( relation ) ;
         }
         else
            rc = d4top( d4 ) ;

         if ( rc )    /* eof or error */
            break ;

         if ( relation->exprSource )
         {
            rec = d4recNo( d4 ) ;
            if ( f4flagIsSetFlip( &relate->set, (unsigned long)rec ) == 0 )
            {
               #ifndef S4OFF_INDEX
                  if ( relate->dataTag )
                  {
                     while ( f4flagIsSetFlip( &relate->set, (unsigned long)rec ) == 0 )
                     {
                        #ifdef S4HAS_DESCENDING
                           rc = (int)tfile4dskip( relate->dataTag->tagFile, 1L ) ;
                        #else
                           rc = (int)tfile4skip( relate->dataTag->tagFile, 1L ) ;
                        #endif
                        if ( rc != 1 )
                        {
                           if ( rc == 0 )
                           {
                              d4goEof( d4 ) ;
                              rc = r4eof ;
                           }
                           break ;
                        }
                        rec = tfile4recNo( relate->dataTag->tagFile ) ;
                     }
                     if ( rc == r4eof )
                        break ;
                  }
                  else
                  {
               #endif
                  rec = f4flagGetNextFlip( &relate->set, 1L, 1 ) + 1L ;
                  if ( d4recCountLessEq( d4, rec ) == 0 )
                  {
                     d4goEof( d4 ) ;
                     rc = r4eof ;
                     break ;
                  }
               #ifndef S4OFF_INDEX
                  }
               #endif
               // AS Jun 25/02 - modified to remove an unneccessary call to d4go from above
               rc = d4go( d4, rec ) ;
               if ( rc != 0 )
                  break ;
            }
         }

         // AS Feb 6/06 - add some logging to help diagnose relate suspend issues
         #ifdef RELATE4LOG
            ftime( &currentTime ) ;
            elapsedTime = (double)currentTime.time * 1000 + currentTime.millitm - (double)c4->statusTime.time * 1000 - c4->statusTime.millitm ;
            sprintf( buf, "relate4top before relate4readRest: elapsed time: %f\n", elapsedTime ) ;
            ftime( &c4->statusTime ) ;
            relate4log( relate, buf ) ;
         #endif

         rc = relate4readRest( relate, 1 ) ;
         if ( rc == relate4filterRecord )
         {
            rc = relate4skipInternal( relate, 1L ) ;
            break ;
         }

         if ( rc < 0 || rc == r4terminate )
            break ;

         if ( relation->exprSource )
         {
            rc2 = log4true( &relation->log ) ;
            if ( rc2 == r4terminate )
            {
               rc = r4terminate ;
               break ;
            }
            if ( rc2 == 0 )
            {
               if ( relation->inSort == relate4sortSkip )  /* must temporarily disable in order to get a matching scan if available */
               {
                  relation->inSort = 0 ;
                  rc = relate4skipInternal( relate, 1L ) ;
                  relation->inSort = relate4sortSkip ;
               }
               else
                  rc = relate4skipInternal( relate, 1L ) ;
            }
         }
         break ;
      }

      #ifndef S4OFF_MULTI
         c4setReadLock( c4, oldReadLock ) ;
      #endif
      // AS Feb 6/06 - add some logging to help diagnose relate suspend issues
      #ifdef RELATE4LOG
         struct timeb currentTime ;
         ftime( &currentTime ) ;
         double elapsedTime = (double)currentTime.time * 1000 + currentTime.millitm - (double)c4->statusTime.time * 1000 - c4->statusTime.millitm ;
         sprintf( buf, "relate4top exitting: %f\n", elapsedTime ) ;
         ftime( &c4->statusTime ) ;
         relate4log( relate, buf ) ;
      #endif
      return rc ;
   #endif
}



int S4FUNCTION relate4type( RELATE4 *relate, int relateType )
{
   int rc ;

   #ifdef E4PARM_HIGH
      if ( relate == 0 )
         return error4( 0, e4parm_null, E94423 ) ;
      if ( relateType != relate4exact && relateType != relate4scan && relateType != relate4approx )
         return error4( relate->codeBase, e4parm, E84416 ) ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( relate, 5, E94423 ) )
         return -1 ;
   #endif
   rc = relate->relationType ;
   if ( rc != relateType )
   {
      relate->relationType = relateType ;
      relate4changed( relate ) ;
   }
   return rc ;
}



int S4FUNCTION relate4retain( RELATE4 *relate, int flag )
{
   #ifndef S4MACINTOSH  // LY Jul 20/04
      #ifndef S4LUPACH  /* LY July 7/03 */
         assert5port( "new client/server and stand/alone function for retaining a relation" ) ;
         // AS Apr 5/02 - added support for leaving the relation status unchanged, to allow positioning within relate4doAll
         // set flag to '1' to enable retaining, set to 0 to disable
         RELATION4 *relation ;

         if ( relate == 0 )
            return -1 ;

         #ifdef E4VBASIC
            if ( c4parm_check( relate, 5, E94433 ) )
               return -1 ;
         #endif

         if ( error4code( relate->codeBase ) < 0 )
            return -1 ;

         relation = relate->relation ;
         relate = &relation->relate ;
         if ( relation->retainRelation == flag )  // already set to that value...
            return 0 ;
         relation->retainRelation = flag ;
         relate4changed( relate ) ;
         relate4skipEnable( relate, 1 ) ;  // need to be able to move around in relation, so store records
      #endif
      return 0 ;
   #else
      return error4( 0, e4notSupported, E94433 ) ;
   #endif
}



#ifdef S4CB51
   int S4FUNCTION relate4unlock( RELATE4 *relate )
   {
      #ifndef S4SINGLE
         DATA4 *dataOn ;

         #ifdef E4PARM_HIGH
            if ( relate == 0 )
               return error4( 0, e4parm_null, E94424 ) ;
         #endif

         if ( !relate->relation->locked )
            return 0 ;

         for ( dataOn = (DATA4 *)l4first( tran4dataList( code4trans( relate->codeBase ) ) ) ;
               dataOn ; dataOn = (DATA4 *)l4next( tran4dataList ( code4trans( relate->codeBase ) ), dataOn ) )
            if ( relate4dbfInRelation( relate, dataOn ) )
               d4unlockLow( dataOn, 0, 0 ) ;

         relate->relation->locked = 0 ;
      #endif
      return 0 ;
   }
#endif



#ifdef S4CLIENT
   static void relation4batchReadFree( RELATION4 *relation )
   {
      if ( relation->readAdvanceBuf != 0 )
      {
         u4free( relation->readAdvanceBuf ) ;
         relation->readAdvanceBuf = 0 ;
      }
      if ( relation->memos != 0 )
      {
         MEMO4BATCH *relateOn = relation->memos ;
         for ( int onEntry = relation->readRecsToBuf - 1 ; onEntry >= 0 ; onEntry-- )
         {
            if ( relation->memos != 0 )
            {
               for ( int onMemo = relation->numMemos - 1 ; onMemo >= 0 ; onMemo -- )
               {
                  if ( relateOn->memos[onMemo].contents != 0 )
                  {
                     u4free( relateOn->memos[onMemo].contents ) ;
                     relateOn->memos[onMemo].contents = 0 ;
                     relateOn->memos[onMemo].contentsLen = 0 ;
                     relateOn->memos[onMemo].contentsAllocLen = 0 ;
                  }
               }
            }
            relateOn++ ;
         }

         u4free( relation->memos ) ;
         relation->memos = 0 ;
         if ( relation->memoBatchEntry != 0 )
         {
            u4free( relation->memoBatchEntry ) ;
            relation->memoBatchEntry = 0 ;
         }
         relation->numMemos = 0 ;
      }
   }



   static int relation4readBufferAlloc( RELATION4 *relation, short doMemos )
   {
      // used to set up the buffers for read-bufferrring
      long len = sizeof( long ) ;

      int numMemos = 0 ;

      CODE4 *c4 = relation->relate.codeBase ;

      // calculate the length of the relate record (combines all slaves...)
      for( RELATE4 *relateOn = &relation->relate ;; )
      {
         len += dfile4recWidth( relateOn->data->dataFile ) + sizeof( long ) + 2 * sizeof( short ) ;  // also store the record number and bof/eof markers ...
         if ( doMemos == 1 )
            numMemos += relateOn->data->dataFile->nFieldsMemo ;
         if ( relate4next( &relateOn ) == 2 )
            break ;
      }

      relation->readRecLen = len ;
      relation->readAdvanceBuf = (char *)u4allocFree( c4, len * relation->readRecsToBuf ) ;
      if ( relation->readAdvanceBuf == 0 )
         return e4memory ;

      if ( doMemos != 0 && numMemos != 0 )
      {
         relation->numMemos = numMemos ;
         // allocate memory for memo fields
         relation->memos = (MEMO4BATCH *)u4allocFree( c4, relation->readRecsToBuf * sizeof( MEMO4BATCH ) ) ;
         if ( relation->memos == 0 )
         {
            relation4batchReadFree( relation ) ;
            return e4memory ;
         }

         relation->memoBatchEntry = (MEMO4BATCH_ENTRY *)u4allocFree( c4, relation->readRecsToBuf * sizeof( MEMO4BATCH_ENTRY ) * numMemos ) ;
         if ( relation->memoBatchEntry == 0 )
         {
            relation4batchReadFree( relation ) ;
            return e4memory ;
         }
         // and set up the memo entries
         for ( int memoLoop = 0 ; memoLoop < relation->readRecsToBuf ; memoLoop++ )
         {
            relation->memos[memoLoop].memos = relation->memoBatchEntry + (numMemos * memoLoop) ;
         }
      }

      return 0 ;
   }



   void relation4readBufferReset( RELATION4 *relation, Bool5 doFree )
   {
      // used to reset the read buffer to indicate not-read (but leave buffers in place)
      relation->readBufNum = 0 ;
      relation->readBufPos = -1 ;
//      relation->readBufRecNoOn = -1 ;
      relation->readBufDirection = 0 ;
      if ( doFree == 1 )
      {
         if ( relation->readAdvanceBuf != 0 )
         {
            relation4batchReadFree( relation ) ;
            relation->readAdvanceBuf = 0 ;
            relation->readRecLen = 0 ;
         }
      }
      // data->readBufRcSave = 0 ;
   }
#endif


/* AS Apr 11/02 - New function for advance-reading client/server */
S4EXPORT long S4FUNCTION relate4readBuffer( RELATE4 *relate, long numRecsToBuf, short doMemos )
{
   // LY Jul 20/04 : added !S4MACINTOSH
   #if !defined( S4LUPACH ) && !defined( S4MACINTOSH )   /* LY July 7/03 */
      assert5port( "new client/server function for batch reading of relate records" ) ;
   #endif

   // LY Jul 20/04 : added !S4MACINTOSH
   /* LY July 7/03 : added !S4LUPACH */
   #if defined( S4CLIENT ) && !defined( S4LUPACH ) && !defined( S4MACINTOSH )
      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94410 ) ;
         if ( numRecsToBuf < -1 )
            return error4( 0, e4parm, E94410 ) ;
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check( relate, 5, E94410 ) )
            return -1 ;
      #endif

      RELATION4 *relation = relate->relation ;

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      // AS May 2/02  - Relax This Constraint
      // if ( code4indexFormat( relate->codeBase ) != r4cdx )
      //    return error4( relate->codeBase, e4notSupported, E90913 ) ;

      if ( numRecsToBuf == -1 )
         return relation->readRecsToBuf ;

      // AS May 22/02 - New functionality, if numRecsToBuf == 1, doMemos indicates what should happen
      // for memo field transferrance on single record read transfers (relate4skip/relate4top/relate4bottom)
      // in this case, only this functionality is affected
      if ( numRecsToBuf == 1 )
      {
         relation->includeMemos = 1 ;
         return 0 ;
      }

      if ( numRecsToBuf == 0 )
      {
         if ( relation->readRecsToBuf != 0 )
         {
            assert5( relation->readRecsToBuf != 0 ) ;
            relation4batchReadFree( relation ) ;
            relation->readAdvanceBuf = 0 ;
            // relation->readAdvanceBufRecs = 0 ;
            relation->readRecsToBuf = 0 ;
            relation4readBufferReset( relation, 0 ) ;
         }
         assert5( relation->readRecsToBuf == 0 ) ;
         return relation->readRecsToBuf ;
      }

      if ( relation->readRecsToBuf != 0 )  // posibly need to reset buffer
      {
         if( relation->readRecsToBuf < numRecsToBuf )  // if >= just re-use existing buffer
         {
            // in this instance we need to set up the bufferring...
            relation4batchReadFree( relation ) ;
            relation->readAdvanceBuf = 0 ;
         }
      }

      relation->readRecsToBuf = numRecsToBuf ;
      if ( relation->readAdvanceBuf == 0 )
      {
         int rc = relation4readBufferAlloc( relation, doMemos ) ;
         if ( rc != 0 )
         {
            relation->readRecsToBuf = 0 ;
            return rc ;
         }
      }

      relation->doMemos = (unsigned char)doMemos ;
      relation4readBufferReset( relation, 0 ) ;
      return relation->readRecsToBuf ;
   #else
      return 0 ;  // CS 2002/04/24 Always export this function
   #endif
}



#ifndef S4CLIENT
   Bool5 relate4countCanApprox( RELATE4 *relate )
   {
      // returns whether or not can approximate count without doing real count.
      // this has to do with relation types.
      // if the types are scans, we cannot approximate because we don't know the number of matches
      // similarily if there are relate types of relate4exact with error conditions of relate4terminate or relate4skipRec
      // we need to actually perform the operations.

      RELATE4 *relateChild = 0 ;
      for( ;; )
      {
         relateChild = (RELATE4 *)l4next( &relate->slaves, relateChild ) ;
         if ( relateChild == 0 )  // if here, we are ok...
            break ;
         switch( relateChild->relationType )
         {
            case relate4scan:
               return 0 ;
            case relate4exact:
               if ( relateChild->errorAction != relate4blank ) // must go through with relation to see if it is contained, so stop here
                  return 0 ;
               break ;
            case relate4approx:
               break ;
         }

         // and verify our slaves are ok as well...
         if ( relate4countCanApprox( relateChild ) == 0 )
            return 0 ;
      }

      return 1 ;
   }
#endif /* !S4CLIENT */



#ifndef S4CLIENT
   // AS Jun 24/02 - New optimized function to count the number of records in the relation
   static unsigned long S4FUNCTION relate4countDo( RELATE4 *relate )
   {
      // returns ULONG_MAX (and sets Code4.errorCode) on error, else returns the # of records in the
      // relation
      // Counts the number of records in the relation.
      // Fully optimized for the following 2 cases - fully bitmapped with no slaves
      //                                           - don't position to slaves if slaves don't have slaves and the slave not in query (just use slave tags)

      long count = 0 ;

      // consider the possibilities of optimizatin first...
      RELATION4 *relation = relate->relation ;
      if ( relation->exprSource == 0 )  // means no query and no slaves
      {
         // this is an easily optimizeable case, no expression to worry about
         if ( l4numNodes( &(relation->relate.slaves) ) == 0 )     // no slaves and no query, the result set will be == the DATA4 record count
            return d4recCount( relate->data ) ;

         // even if we have slaves, we can make some educated guesses...
         RELATE4 *relateChild = 0 ;
         long tempCount = 0 ;
         // first see if scans are involved
         // Bool5 hasScans = 0 ;
         // Bool5 hasExacts = 0 ;
         Bool5 canTryApprox = relate4countCanApprox( relate ) ;

         if ( canTryApprox == 1 )
         {
            for( ;; )
            {
               relateChild = (RELATE4 *)l4next( &relate->slaves, relateChild ) ;
               if ( relateChild == 0 )          // if here, all was ok, and we have a valid count, so return it out
                  return tempCount ;
               assert5( relateChild->relationType != relate4scan ) ;  // should have been caught in earlier loop
               // if the child is relate4scan, we cannot use the record count, so stop now
               // if ( relateChild->relationType == relate4scan )
               // {
               //    tempCount += d4recCount( relate->data ) ;  //
               //    continue ;
               // }
               if ( relateChild->relationType == relate4approx )
               {
                  // this is another easy case, as long as there aren't any scans going on
                  // if there are scans or exacts, those will figure out the actual status
                  // if ( hasScans == 0 && hasExacts == 0 )
                  // {
                  return d4recCount( relate->data ) ;
                  // }
               }
               else
               {
                  // this is a more complex case - depends on error action
                  if ( relate->errorAction == relate4blank ) // all match, so 1 to 1
                     return d4recCount( relate->data ) ;
                  else // the other cases are too complex, so do normal count
                  {
                     // should never get here, canTryApprox should have been false
                     assert5( 0 ) ;
                     break ;
                  }
               }


            }
         }
      }
      else
      {
         // now consider the case where there is a query expression and no slaves, but it is entirely bitmapped.
         // first need to determine that it is entirely bitmapped...
         if ( l4numNodes( &(relation->relate.slaves) ) == 0 )
         {
            if ( relation->isInitialized == 0 )
            {
               int rc = relate4topInit( relate ) ;
               if (  rc != 0 )
                  return ULONG_MAX ;
            }

            // use the fact that we track whether or not the map is fully mapped...
            if ( relation->fullyMapped == 1 )  // is fully mapped, means a simple counting procedure...
               return f4flagCount( &relate->set, 1 ) ;  // start the count at '1' since we flag based on record #, and there is no record #0
         }
      }

      // here we have the condition where we don't want to skip in the bottom-most slave tags
      // this is done by a flag - we indicate we are performing relation for count only, and since
      // we know with each slave tag if it has slaves, we don't position with bottommost slaves
      // note that there is a caveat - if the query includes slaves, we cannot do this optimization

      if ( relation->exprSource != 0 )  // means there is a query
      {
         if ( relation->isInitialized == 0 )
         {
            int rc = relate4topInit( relate ) ;
            if (  rc != 0 )
               return ULONG_MAX ;
         }
         EXPR4 *expr = relation->log.expr ;
         if ( expr != 0 )
         {
            for( int i = 0 ; i < expr->infoN ; i++ )
            {
               E4INFO *info = expr->info + i ;
               if ( info->fieldPtr )
               {
                  if ( info->fieldPtr->data != relation->relate.data )  // means the data4 is not the master data4...
                  {
                     // cannot skip slaves because they are involved in the expression...
                     relation->countOnly = 0 ;
                     break ;
                  }
               }
               if ( info->functionI == E4CALC_FUNCTION )
               {
                  // also do not perform optimization if a calc is involved, since the calc
                  // may involve a slave relation, and the calc can be changed at any time.
                  relation->countOnly = 0 ;
                  break ;
               }
            }
         }
      }

      int rc = relate4top( relate ) ;
      if ( rc < 0 )
         return ULONG_MAX ;

      while( rc == 0 ) // still records to be returned
      {
         count++ ;
         rc = relate4skipInternal( relate, 1L ) ;
         if ( rc < 0 )
            return ULONG_MAX ;
      }

      return count ;
   }
#endif /* !S4CLIENT */



// AS Jun 24/02 - New optimized function to count the number of records in the relation
unsigned long S4FUNCTION relate4count( RELATE4 *relate )
{
   #ifdef S4CLIENT
      CODE4 *c4 = relate->codeBase ;
      if ( relate->dataTag != relate->data->tagSelected )
         relate4changed( relate ) ;

      if ( relate->relation->isInitialized == 0 )
      {
         // AS Sept 3/02 - Failing to select tag if relate4bottom() is called instead of relate4top()
         relate->dataTag = relate->data->tagSelected ;
         int rc = relate4clientInit( relate ) ;
         if ( rc != 0 )
            return rc ;
      }

      CONNECT4 *connect = c4getClientConnect( c4 ) ;

      connect4sendShort( connect, MSG5RELATE_COUNT ) ;
      #ifdef S4TESTING
         // AS Nov 18/02 - allow to not include for tests not using...
         if ( c4->expectedCount != -1 )
         {
            // request a count
            connect4sendLong( connect, -2L ) ;
            connect4sendLong( connect, c4->expectedCount ) ;
         }
      #endif
      connect4sendLong( connect, relate->relation->relationId ) ;
      connect4sendFlush( connect ) ;
      long count = connect4receiveLong( connect ) ;
      if ( count < 0 )  // if S4TESTING is defined, sends e4result in case of count mismatch
         error4( relate->codeBase, count, E94431 ) ;
      return count ;
   #else
      RELATION4 *relation = relate->relation ;

      if ( relation->countAccurate == 1 )  // count optimization
         return relation->count ;

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      #ifdef E4ANALYZE
         // code to allow for auto-count for testing from relate4top - but don't call in relate4top if we are
         // being called from relate4count.
         static int inCount = 0 ;

         if ( inCount == 1 )
            return ULONG_MAX - 1 ;
         inCount = 1 ;
      #endif

      long sortRecOn = -1 ;
      Bool5 isInitialized ;

      if ( relation->isInitialized == 1 )
      {
         isInitialized = 1 ;
         // we have to consider that we need to remember our current position
         if ( relation->sortExpr != 0 )
            sortRecOn = relation->sortRecOn ;

         // even with the sort rec on saved, need to remember recno's for those not involved in the sort part
         // i.e. some slaves in some situations
         for( RELATE4 *relateOn = &relate->relation->relate ;; )
         {
            relateOn->preCountRecno = relateOn->data->recNum ;
            relateOn->preCountBofFlag = relateOn->data->bofFlag ;
            relateOn->preCountEofFlag = relateOn->data->eofFlag ;

            if ( relate4next( &relateOn ) == 2 )
               break ;
         }
      }

      relation->countOnly = 1 ;
      unsigned long count = relate4countDo( relate ) ;
      relation->countOnly = 0 ;
      if ( count != ULONG_MAX )
      {
         relation->countAccurate = 1 ;
         relation->count = count ;
      }
      #ifdef E4ANALYZE
         inCount = 0 ;
      #endif

      if ( isInitialized == 1 )
      {
         // repositino back to before-count positions
         if ( sortRecOn != -1 )
         {
            // reset to sort position...
            relation->sortRecOn = sortRecOn ;
            relate4sortGetRecord( relation, relation->sortRecOn, 0, 1, 0 ) ;
         }

         for( RELATE4 *relateOn = &relate->relation->relate ;; )
         {
            relateOn->data->bofFlag = relateOn->preCountBofFlag ;
            relateOn->data->eofFlag = relateOn->preCountEofFlag ;
            if ( d4recNo( relateOn->data ) != relateOn->preCountRecno )
            {
               if ( relateOn->data->bofFlag == 0 && relateOn->data->eofFlag == 0 && relateOn->preCountRecno > 0 )  // eof/bof - just change value, else must go
                  d4go( relateOn->data, relateOn->preCountRecno ) ;
               else
                  relateOn->data->recNum = relateOn->preCountRecno ;
            }
            #ifndef S4OFF_INDEX
               if ( relateOn->master != 0 )  // and updated the scan values for scan relations...
               {
                  if ( relateOn->relationType == relate4scan )
                  {
                     char *ptr ;
                     int len = relate4evaluateMasterExpression( relateOn, &ptr ) ;
                     if ( len < 0 )
                        return ULONG_MAX ;

                     if ( relate4updateScanValue( relateOn, ptr, len ) < 0 )
                        return ULONG_MAX ;
                  }
               }
            #endif

            if ( relate4next( &relateOn ) == 2 )
               break ;
         }
      }

      return count ;
   #endif
}



int S4FUNCTION relate4skipMaster( RELATE4 *relate, long numSkip )
{
   // AS Jun 24/02 - New function to skip forward in the master table only
   // returns succeess, < 0 if error, or r4eof/r4bof if at bottom/top
   // it is adviseable to call relate4retain() prior to calling first relate4top() in order
   // to optimize this function call.

   // there are several possibilities here:
   // 1. has a sort - in which case we use special sort skipping functions
   // 2. no sort, but has a query involving a slave or a slave such that master records may be excluded
   // 3. no sort, and no exclusion.
   #ifdef S4CLIENT
      CODE4 *c4 = relate->codeBase ;
      return error4( c4, e4notSupported, E94431 ) ;
   #else
      #ifdef E4PARM_HIGH
         if ( relate == 0 )
            return error4( 0, e4parm_null, E94431 ) ;
      #endif

      if ( error4code( relate->codeBase ) < 0 )
         return e4codeBase ;

      RELATION4 *relation = relate->relation ;
      relate = &relation->relate ;
      int rc = 0 ;

      if ( relation->isInitialized == 0 )
         return error4( relate->codeBase, e4relate, E84406 ) ;

      if ( numSkip == 0 )
         return 0 ;

      // if we haven't already analyzed the relation for master skipping, do so now.
      // we need to retain the relation if we are doing sorting...
      if ( relation->retainRelation == 0 )
      {
         // we really just need to retain the relation... save the position and remember it after
         long recNum = d4recNo( relate->data ) ;
         rc = relate4retain( relate, 1 ) ;
         if ( rc < 0 )
            return rc ;
         rc = relate4top( relate ) ;
         if ( rc < 0 )
            return rc ;
         d4go( relate->data, recNum ) ;
         rc = relate4doAllLow( relate, 0 ) ;
         if ( rc != 0 )
            return rc ;
      }

      if ( relation->sortExpr != 0 )
      {
         // if it has a sort we do special sort call, otherwise just call d4skip and relate4doAll()
         return relate4sortSkipMaster( relation, numSkip ) ;
      }
      else
      {
         while ( numSkip != 0 )
         {
            // AS Aug 27/03 - Changes - was not considering the expression for evaluation
            if ( numSkip > 0 ) // means forward skip
            {
               rc = d4skip( relate->data, 1L ) ;
               if ( rc != 0 )
                  return rc ;
               rc = relate4doAllLow( relate, 1 ) ;
            }
            else  // backwards skip
            {
               rc = d4skip( relate->data, -1L ) ;
               if ( rc != 0 )
                  return rc ;
               rc = relate4doAllLow( relate, 1 ) ;
            }
            if ( rc != relate4filterRecord ) // don't count if filtering this record
            {
               if ( rc != 0 )
                  return rc ;

               if ( relation->exprSource )
               {
                  int rc2 = log4true(&relation->log ) ;
                  if ( rc2 == r4terminate )
                     return r4terminate ;
                  if ( rc2 == 0 )
                     continue ;
               }

               if ( numSkip > 0 )
                  numSkip-- ;
               else
                  numSkip++ ;
            }
         }
      }

      return 0 ;
   #endif
}



#ifdef S4VB_DOS
   RELATE4 *S4FUNCTION relate4createSlave_v( RELATE4 *master, DATA4 *slave, char *masterExpr, TAG4 *slaveTag )
   {
      return relate4createSlave( master, slave, c4str( masterExpr ), slaveTag ) ;
   }



   char * relate4masterExpr_v( RELATE4 *r4 )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( r4, 5, "relate4masterExpr():" ) ) return 0 ;
      #endif

      return v4str(r4->masterExpr->source) ;
   }



   int S4FUNCTION relate4querySet_v ( RELATE4 *relate, char *expr )
   {
      return relate4querySet( relate, c4str(expr) ) ;
   }



   int S4FUNCTION relate4sortSet_v ( RELATE4 *relate, char *expr )
   {
      return relate4sortSet( relate, c4str( expr ) ) ;
   }
#endif
