/* d4seek.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#if !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) && defined( S4FOX )
   int collate4simpleMapping( COLLATE4 *collate )
   {
      #ifdef E4PARM_LOW
         if ( !collate )
         {
            error4describe( 0, e4parmNull, E84907, "collate4simpleMapping", 0, 0 ) ;
            return 0 ;
         }
      #endif

      return ( collate->keySizeCharPerCharAdd == 0 ) ;
   }


   int tfile4stok( TAG4FILE *t4, char *buf, const char *str, int len )
   {
      /* because of possibilities of null, key may need to have a character
         indicater added
      */
      char *ptr = buf ;
      int hasNull = 0 ;

      if ( t4->indexFile->dataFile->compatibility == 30 )
      {
         if ( expr4nullLow( t4->expr, 0 ) )
         {
            if ( len == 0 )   /* seek for null */
            {
               *ptr = 0 ;
               return 1 ;
            }
            *ptr = (char)0x80 ;
            ptr++ ;
            hasNull = 1 ;
         }
      }

      int lenOut ;


      // AS Oct 30/03 - There existed a general collating sequence partial key search
      // problem.  The basic problem was that the tail characters were interfering with the
      // partial match (with partial seeks the tail characters are always ignored)  Therefore,
      // a change was made so that the key conversion would not create tail characters in this case.
      COLLATE4 *collate = collation4get( t4->collateName ) ;

      collate->considerPartialSeek = 1 ;  // instructs the stok to consider we may be doing a partial seek, not that we are for sure doing this
      collate->hasNull = hasNull ;
      collate->maxKeyLen = tfile4keyLen( t4 ) ;
      (*t4->stok)( collate, ptr, str, len, &lenOut ) ;
      collate->considerPartialSeek = 0 ;

      // AS 02/24/00 there is a problem here in particular with general collations whereby after the key has been
      // converted extra bytes are inserted at the end even though these are not strictly required if the seek
      // is a partial seek.  In fact, they may cause the partial seek to fail.  The problem is that we don't want
      // to trim off any actually requested and inserted bytes.  For now we will do a simple work around:
      // if the output length is < key size we will allow trimming of the entry but up to reducing the length
      // to 'len'

      // AS 04/04/00 - change, use the FoxPro method of partial seeking - they just ignore the trail bytes completely
      // on partial seeks.  If it is a full seek then they use the full key.

      // looking for any 'a' related value will find all 'a' related values
      // int keyLenLessNullModifier = tfile4keyLen( t4 ) ;
      // if ( t4->indexFile->dataFile->compatibility == 30 )
      //    if ( expr4nullLow( t4->expr, 0 ) )
      //       keyLenLessNullModifier-- ;

      // if ( lenOut < keyLenLessNullModifier * 2 )
      // {
      //    for ( ; lenOut > len ; lenOut-- )
      //    {
      //       if ( ptr[lenOut-1] != t4->pChar )  // not a trailing blank, done
      //          break ;
      //    }
      //}
      if ( tfile4keyLen( t4 ) > lenOut + hasNull )  // is a partial seek
      {
         // AS 07/06/00 - turns out there is a special case when searching for keys with trailing blanks
         // with general collation:  seeking for "a " is actually the same as searching for "a" since
         // the blanks go at the start of the key.  However, if we actually convert the string "a "
         // the conversion function assumes that a full conversion is occurring (not a partial seek)
         // so the space gets included in the conversion, resulting in looking for nulls as part of
         // the seek even though they cannot be there!  This is due to the fact that even though
         // the ' ' has a normal sort sequence after some other characters, if it is a trailing blank
         // it is considered as being before all other characters in the sort sequence (a bit of a
         // glitch really)
         // AS Nov 19/04 - Turns out there is another special case here...if we are expanding out a double character (e.g. ess-tset)
         // we need to still increase the output length on a partial seek...
         // it turns out that instead of looking for blanks in the head index, we can look for trailing bytes in the converted string (i.e. values <16)
         // LY Jan 5/05 : replaced check for General collation with collate4simpleMapping()
         // if ( t4->collateName == collate4generalCp1252 || t4->collateName == collate4generalCp437 )
         if ( !collate4simpleMapping( collation4get( t4->collateName ) )  )
         {
            // for( int blankIndex = len - 1 ; blankIndex >= 0 ; blankIndex-- )
            // {
            //    if ( str[blankIndex] != ' ' )
            //       break ;
            //    len-- ;
            // }
            len = 0 ;
            for( int blankIndex = 0 ; blankIndex < lenOut ; blankIndex++ )
            {
               // LY Dec 23/04 : cast to unsigned char, since change to
               // cp1252generalCollationArray for CBC653_AZELL resulted in head
               // values > 127
               if ( (unsigned char)ptr[blankIndex] < 16 )
                  break ;
               len++ ;
            }
         }
         lenOut = len ;
      }

      return lenOut + hasNull ;
   }



   // AS Aug 9/05 - Support for new field type binary float
   void tfile4ftok( TAG4FILE *t4, char *buf, const float floatKey )
   {
      #ifdef S4BYTE_SWAP  /* LY 2001/06/26*/
         double tempDbl ;
      #endif
      if ( t4->indexFile->dataFile->compatibility == 30 )
         if ( expr4nullLow( t4->expr, 0 ) )
         {
            buf[0] = (char)0x80 ;
            //#ifdef S4BYTE_SWAP  /* LY 2001/06/26 */
            //   tempDbl = x4reverseDouble( (double*) &floatKey ) ;
            //   (*t4->dtok)( buf+1, tempDbl ) ;
            // #else  LY 2001/07/27 : shouldn't be necessary since t4dblToFox handles byte ordering anyway */
               (*t4->dtok)( buf+1, floatKey ) ;
            // #endif
            return ;
         }

      t4floatToFox( buf, &floatKey ) ;
      return ;
   }


   void tfile4dtok( TAG4FILE *t4, char *buf, const double dkey )
   {
      #ifdef S4BYTE_SWAP  /* LY 2001/06/26*/
         double tempDbl ;
      #endif
      if ( t4->indexFile->dataFile->compatibility == 30 )
         if ( expr4nullLow( t4->expr, 0 ) )
         {
            buf[0] = (char)0x80 ;
            //#ifdef S4BYTE_SWAP  /* LY 2001/06/26 */
            //   tempDbl = x4reverseDouble( (double*) &dkey ) ;
            //   (*t4->dtok)( buf+1, tempDbl ) ;
            // #else  LY 2001/07/27 : shouldn't be necessary since t4dblToFox handles byte ordering anyway */
               (*t4->dtok)( buf+1, dkey ) ;
            // #endif
            return ;
         }

      (*t4->dtok)( buf, dkey ) ;
      return ;
   }
#endif /* #if !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) && !defined( S4FOX )
   #define tfile4stok( t4, buf, str, len ) ( (*((t4)->stok))( 0, (buf), (str), (len), 0 ), len )
   // AS 06/08/00 allow for S4CLIPPER version to format numerics correctly for seeking with strings on numeric field types
   #ifdef S4CLIPPER
      #define tfile4stokNum( t4, buf, str, len, dec ) ( (*((t4)->stok))( 0, (buf), (str), (len), dec ), len )
   #endif
   #define tfile4dtok( t4, buf, str ) (*((t4)->dtok))( (buf), (str) )
#endif


#if !defined( S4OFF_INDEX ) && !defined( S4CLIENT )
   // for OLE-DB/ODBC
   int S4FUNCTION tfile4stokExport( TAG4FILE *t4, char *out, const char *in, int len )
   {
      // AS 11/06/00 - clippern keys if numeric format, should call tfile4stokNum
      #ifdef S4CLIPPER
         if ( tfile4type( t4 ) == r4num || tfile4type( t4 ) == r4numDoub )
         {
            int numDec = t4->header.keyDec ;
            return tfile4stokNum( t4, out, in, len, &numDec ) ;
         }
      #endif
      return tfile4stok( t4, out, in, len ) ;
   }


   // for ODBC
   void S4FUNCTION tfile4dtokExport( TAG4FILE *t4, char *out, const double key )
   {
      assert5( t4->dtok != 0 ) ;
      tfile4dtok( t4, out, key ) ;
   }


   // AS Aug 9/05 - Support for new field type binary float
   void S4FUNCTION tfile4ftokExport( TAG4FILE *t4, char *out, const double key )
   {
      #ifdef S4CLIENT_OR_FOX
         assert5( t4->dtok != 0 ) ;
         // AS Oct 23/05 - fix compile error
         tfile4ftok( t4, out, (const float)key ) ;
      #else
         error4( t4->codeBase, e4notSupported, E91642 ) ;
      #endif
   }

   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   static int d4seekCheck( DATA4 *data, TAG4FILE *tag, const int rc, const char *buf, const int l )
   {
      /* function to ensure the integrity of the seeks return value */
      /* in single user mode, exclusive, or if file is locked, the data is assumed up to date, and not checked */

      int rc2, len ;
      #ifndef S4SINGLE
         int skipped ;
         unsigned char *dbfKey ;

         if ( rc == r4locked )
            return r4locked ;
         skipped = 0 ;
      #endif

      len = l ;

      if ( tfile4eof( tag ) )
         return d4goEof( data ) ;

      if ( len > tag->header.keyLen )
         len = tag->header.keyLen ;

      if ( d4recCountLessEq( data, tfile4recNo( tag ) ) == 0 )  /* past eof */
         while ( d4recCountLessEq( data, tfile4recNo( tag ) ) == 0 )  /* past eof */
         {
            if ( tfile4skip( tag, 1L ) == 0 )  /* eof */
               return d4goEof( data ) ;
            #ifndef S4SINGLE
               skipped = 1 ;
            #endif
         }

      #ifndef S4SINGLE
         #ifdef S4SERVER
            // AS Apr 15/03 - support for new lockId for shared clone locking
            if ( dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4write ) == 1
               || dfile4lockTestFile( data->dataFile, data4lockId( data ), data4serverId( data ), lock4read ) == 1 )
         #else
            if ( d4lockTestFile( data ) == 1 )
         #endif
      #endif
         {
            // AS Sep 25/01 - Coding for new r4lockOnSuccess setting - turn off read-lock if r4after
            // The record is already locked in this case, so it must be valid (i.e. the tag entry must match)
            // This also means we don't need to consider r4lockOnSuccess values since it is already locked.
            #ifndef S4OFF_OPTIMIZE
               data->dataFile->hiPrio = 1 ;
            #endif
            rc2 = d4go( data, tfile4recNo( tag ) ) ;
            #ifndef S4OFF_OPTIMIZE
               data->dataFile->hiPrio = 0 ;
            #endif
            if ( rc2 )
               return rc2 ;
            return rc ;
         }
      #ifndef S4SINGLE
         for( ;; )
         {
            if ( d4recCountLessEq( data, tfile4recNo( tag ) ) != 0 )  /* valid spot */
            {
               #ifndef S4OFF_OPTIMIZE
                  data->dataFile->hiPrio = 1 ;
               #endif
               // AS Sep 25/01 - Coding for new r4lockOnSuccess setting - turn off read-lock if r4after
               CODE4 *c4 = data->codeBase ;

               int oldReadLock = c4getReadLock( c4 ) ;
               // AS Oct 30/01 - Also consider case where rc == r4entry (after 1st found)
               if ( ( rc == r4entry || rc == r4after ) && oldReadLock == r4lockOnSuccess )
                  c4setReadLock( c4, 0 ) ;
               rc2 = d4go( data, tfile4recNo( tag ) ) ;
               c4setReadLock( c4, oldReadLock ) ;
               #ifndef S4OFF_OPTIMIZE
                  data->dataFile->hiPrio = 0 ;
               #endif
               if ( rc2 )
                  return rc2 ;

               if ( expr4context( tag->expr, data ) < 0 )
                  return -1 ;
               if ( tfile4exprKey( tag, &dbfKey ) < 0 )
                  return -1 ;
               /* AS 12/13/99 be a little more caution here, return an error if the key in tag
                  is null.  Note that this should be impossible since rc2 from d4go would then
                  return eof/bof if not at a valid spot.  Maybe occur if an error or invalid
                  index.
               */
               char *tagKey = tfile4key( tag ) ;
               if ( tagKey == 0 )  // maybe a corrupt index...
                  return error4( data->codeBase, e4index, E92907 ) ;
               // AS May 25/01 - We need to take the input 'l' length into account.  For example, if
               // the previous value was 'ABCD' and we look for 'ABC' and the new value is
               // 'ABCE', we get a failed find because of a mismatch when in fact we have
               // actually found the same value!  This is especially true once we skip off...
               #ifdef S4FOX
                  // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                  // if ( !u4keycmp( tfile4key( tag ), dbfKey, (unsigned)expr4keyLen( tag->expr ), (unsigned)tag->header.keyLen, 0, &tag->vfpInfo ) )   /* matched */
                  // AS May 25/01 - We need to take the input 'l' length into account.  For example, if
                  // if ( !u4keycmp( tagKey, dbfKey, (unsigned)expr4keyLen( tag->expr ), (unsigned)tag->header.keyLen, 0, collation4get( tag->collateName ) ) )   /* matched */
                  // AS July 23/01 - We need to do a 'partial' key cmp here, same as d4seek (t4seek was failing)
                  // if ( !u4keycmp( tagKey, dbfKey, (unsigned)expr4keyLen( tag->expr ), (unsigned)l, 0, collation4get( tag->collateName ) ) )   /* matched */
                  // AS Feb 8/10 - need to do a full key length compare to verify it actually matches the database record in this case!
                  if ( !u4keycmp( tagKey, dbfKey, (unsigned)expr4keyLen( tag->expr ), (unsigned)expr4keyLen( tag->expr ), 0, collation4get( tag->collateName ) ) )   /* matched */
                  // if ( !u4keycmpPartial( dbfKey, tagKey, (unsigned)l, (unsigned)expr4keyLen( tag->expr ), 0, collation4get( tag->collateName ), buf, l ) )   /* matched */
               #else
                  // AS May 25/01 - We need to take the input 'l' length into account.  For example, if
                  // if ( !(*tag->cmp)( tagKey, dbfKey, (unsigned)expr4keyLen( tag->expr ) ) )  /* matched */
                  if ( !(*tag->cmp)( tagKey, dbfKey, (unsigned)l ) )  /* matched */
               #endif
               {
                  // AS May 25/01 - We have taken into account that the key matches in the memory
                  // comparison above, but we also need to take into account the possibility that
                  // the filter has changed... eg. record is deleted and the index is on !deleted()
                  Bool5 filterOk = 1 ;
                  if ( tag->filter != 0 )
                  {
                     // AS Sep 28/04 - ensure the context is set for the filter
                     if ( expr4context( tag->filter, data ) < 0 )
                        return -1 ;
                     if ( !expr4true( tag->filter ) )
                        filterOk = 0 ;
                  }
                  if ( filterOk )
                  {
                     if ( skipped )
                     {
                        // AS May 25/01 - We need to take the input 'l' length into account.  For example, if
                        #ifdef S4FOX
                           // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                           // rc2 = u4keycmp( dbfKey, buf, (unsigned)len, (unsigned)expr4keyLen( tag->expr ), 0, &tag->vfpInfo ) ;
                           // rc2 = u4keycmp( dbfKey, buf, (unsigned)len, (unsigned)expr4keyLen( tag->expr ), 0, collation4get( tag->collateName ) ) ;
                           // AS July 23/01 - We need to do a 'partial' key cmp here, same as d4seek (t4seek was failing)
                           // rc2 = u4keycmp( dbfKey, buf, (unsigned)len, (unsigned)l, 0, collation4get( tag->collateName ) ) ;
                           rc2 = u4keycmpPartial( dbfKey, buf, (unsigned)len, (unsigned)l, 0, collation4get( tag->collateName ), buf, l ) ;
                        #else
                           // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                           // rc2 = (*tag->cmp)( dbfKey, buf, (unsigned)expr4keyLen( tag->expr ) ) ;
                           rc2 = (*tag->cmp)( dbfKey, buf, l ) ;
                        #endif
                        if ( rc2 == 0 )   /* worked */
                           return rc2 ;
                        if ( rc2 > 0 )
                           return r4after ;
                        /* other wise, if < 0, can't return r4after, so go down and skip next */
                     }
                     else
                        return rc ;
                  }
               }
            }

            /* try next record */
            if ( tfile4skip( tag, 1L ) == 0 )  /* eof */
               return d4goEof( data ) ;
            if ( error4code( data->codeBase ) < 0 )
               return -1 ;
            skipped = 1 ;
         }
      #endif
   }
#endif  /* #if !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) */



#if !defined( S4OFF_INDEX )
   #ifdef S4CLIENT
      // AS Oct 21/02 - added to support seek without data movement and record transfer
      int d4seekServer( DATA4 *data, const void *searchData, const short len, const int fromCurrentPos, const short seekType, Bool5 doDataPosition )
      {
         CONNECTION4 *connection ;
         CONNECTION4GO_INFO_OUT *out ;
         CONNECTION4SEEK_INFO_IN *infoIn ;
         int rc, saveRc ;
         TAG4 *tag ;
         CODE4 *c4 ;
         #ifdef E4ANALYZE
            int e4anLen ;
         #endif

         #ifdef E4PARM_HIGH
            if ( data == 0 || searchData == 0 || len < 0 || fromCurrentPos < 0 || fromCurrentPos > 1 ||
               ( seekType != CON4SEEK && seekType != CON4SEEK_DBL && seekType != CON4SEEK_LONGLONG ) )
               return error4( 0, e4parm, E92906 ) ;
         #endif

         c4 = data->codeBase ;

         tag = d4tagDefault( data ) ;
         if ( tag == 0 )
            return r4noTag ;
         else  // AS Oct 25/05 - support for low level tag operations
            tag->tagFile->tagDataValid = 0 ;  // reset to invalid

         #ifndef S4OFF_WRITE
            /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc )
               return rc ;
         #endif

         connection = data->dataFile->connection ;
         if ( connection == 0 )
            return error4( c4, e4parm, E92906 ) ;

         connection4assign( connection, seekType, data4clientId( data ), data4serverId( data ) ) ;
         connection4addData( connection, NULL, sizeof(CONNECTION4SEEK_INFO_IN), (void **)&infoIn ) ;
         #ifdef E4ANALYZE
            e4anLen = sizeof( infoIn->tagName ) ;
            if (  e4anLen != even4up( LEN4TAG_ALIAS + 1 ) || e4anLen != even4up( sizeof( tag->tagFile->alias ) ) )
               return error4( c4, e4struct, E92906 ) ;
         #endif
         memcpy( infoIn->tagName, tag->tagFile->alias, LEN4TAG_ALIAS ) ;
         infoIn->tagName[LEN4TAG_ALIAS] = 0 ;
         // AS Oct 24/01 - Need to resolve potential tag aliasing problems (t4indx1) */
         u4ncpy( infoIn->indexFileName, tag->tagFile->indexFile->accessName, strlen( tag->tagFile->indexFile->accessName ) + 1 ) ;
         infoIn->keyLen = htons5(len) ;
         infoIn->fromCurrentPos = fromCurrentPos ;
         if ( fromCurrentPos )   /* verify at valid position, first */
         {
            if ( d4eof( data ) || d4bof( data ) )
               infoIn->startPos = 0 ;
            else
               infoIn->startPos = htonl5(d4recNo( data )) ;   /* for seek next */
         }
         connection4addData( connection, searchData, len, NULL ) ;
         connection4addData( connection, "", 1, NULL ) ;  /* null end for character double seeks */
         infoIn->includeMemos = data->includeMemos ;
         // AS Oct 21/02 - added to support seek without data movement and record transfer
         infoIn->doDataPosition = doDataPosition ;
         saveRc = connection4repeat( connection ) ;
         if ( saveRc < 0 )
            return connection4errorDescribe( connection, c4, saveRc, E92906, tag->tagFile->alias, 0, 0 ) ;
         if ( saveRc == r4locked )
            return r4locked ;
         if ( saveRc == r4eof )
         {
            rc = d4goEof( data ) ;
            if ( rc == 0 )
               rc = r4eof ;
            return rc ;
         }
         if ( doDataPosition == 0 )
         {
            if ( connection4len( connection ) != (long)sizeof( CONNECTION4GO_INFO_OUT ) )
               return error4( c4, e4packetLen, E92906 ) ;
         }
         else
         {
            if ( connection4len( connection ) < (long)sizeof( CONNECTION4GO_INFO_OUT ) + (long)d4recWidth( data ) )
               return error4( c4, e4packetLen, E92906 ) ;
            out = (CONNECTION4GO_INFO_OUT *)connection4data( connection ) ;

            if ( saveRc == r4entry )
               rc = r4entry ;
            else
               rc = 0 ;
            rc = d4goVirtual( data, ntohl5(out->recNo), rc, out, connection, fromCurrentPos ) ;
            if ( rc != 0 )
               return rc ;
         }
         return saveRc ;
      }
   #endif /* S4CLIENT */


   #ifdef S4FOX
      static int d4seekMatch( DATA4 *data, TAG4FILE *tfile, const char *inputKey, const short inputKeyLen )
      {
         // return 0 ;    // for now disable...
         CODE4 *c4 = data->codeBase ;
         // we need to compare the resulting key again with our search expression...just compare the first matching bytes...
         short matchLen = min( inputKeyLen, tfile->header.keyLen ) ;
         // LY Dec 22/04 : if you perform a partial seek and then a exact seek
         // for the same record, tfile4key() will not recalculate the key for
         // the second seek and just return the pointer to its key buffer.
         // However, if the partial seek contained trailing spaces, then
         // the code below for blanking out the key will overwrite
         // tfile4key()'s key buffer, resulting in the wrong key being returned
         // for the exact seek
         // char *dbfKey = tfile4key( tfile ) ;
         char *tempPtr = tfile4key( tfile ) ;
         char *dbfKey = (char *)u4alloc( tfile4keyLen( tfile ) ) ;
         if ( !dbfKey )
            return e4memory ;
         memcpy( dbfKey, tempPtr, tfile4keyLen( tfile ) ) ;

         // We need to do a special modification for partial seeking...when the key is found (general collating sequence)
         // if there are trailing blanks and this is a partial seek, we need to trim the trailing bytes off becuase we only
         // match the head bytes on a partial seek...for example, if the returned key is "a(accent)  " we need to have the
         // resulting key only be "a  " and remove the trail byte that immediately follows the a...
         B4BLOCK *b4 = tfile4block( tfile ) ;
         COLLATE4 *collate = collation4get( tfile->collateName ) ;
         // LY Jan 5/05 : replace direct check of COLLATE4.keySizeCharPerCharAdd with collate4simpleMapping()
         // if ( b4->curTrailCnt != 0 && collate->keySizeCharPerCharAdd != 0 )
         if ( b4->curTrailCnt != 0 && !collate4simpleMapping( collate ) )
         {
            // use the fact that the 'head' bytes are >= 16 to tell when the trail bytes have arrived...
            short blankPos = 0 ;
            short keyLen = tfile4keyLen( tfile ) ;
            for ( ; blankPos < keyLen ; blankPos++ )
            {
               // LY Dec 16/04 : cast to unsigned char, since change to
               // cp1252generalCollationArray for CBC653_AZELL resulted in head
               // values > 127
               if ( (unsigned char )dbfKey[blankPos] < 16 )  // we are done the header bytes..
                  break ;
            }
            short lenBlank = tfile4keyLen( tfile ) - blankPos ;
            memset( dbfKey+blankPos, 0x11, lenBlank ) ;  // AS Dec 21/04 - blanks actually are 0x11 in general collating sequence
         }

         if ( c4->bufLen2 < (unsigned int)tfile->header.keyLen )
         {
            if ( u4allocAgain( c4, &c4->fieldBuffer2, &c4->bufLen2, tfile->header.keyLen ) < 0 )
            {
               u4free( dbfKey ) ;
               return e4memory ;
            }
         }

         char *outputKey2 = c4->fieldBuffer2 ;
         // AS Dec 17/04 - although we are only performing a partial match, we must convert with the entire key length to get a true key...
         // tfile4stok( tfile, outputKey2, inputKey, matchLen ) ;
         // AS Mar 9/11 - the change from Dec 17/04 causes possible memory access errors since the input key may not actually be large enough and is being accessed...try undoing and see what happens
         // the solution is we need another buffer large enough for the full key
         const char *tempKey ;
         char *allocatedTempKey ;
         tempKey = 0 ;
         allocatedTempKey = 0 ;
         if ( matchLen != (tfile->header.keyLen / (collate->keySizeCharPerCharAdd + 1)) )
         {
            allocatedTempKey = (char *)u4alloc( tfile->header.keyLen / (collate->keySizeCharPerCharAdd + 1) ) ;
            if ( !allocatedTempKey )
            {
               u4free( dbfKey ) ;
               return e4memory ;
            }
            // set the key to nulls by default; we need to ensure that the key creation doesn't trim off the blanks (which it does by default), which will mess up the results since we need to have the blank converted as well.
            memset( allocatedTempKey, 0, tfile->header.keyLen / (collate->keySizeCharPerCharAdd + 1) ) ;
            memcpy( allocatedTempKey, inputKey, matchLen ) ;
            tempKey = allocatedTempKey ;
         }
         else
            tempKey = inputKey ;


         tfile4stok( tfile, outputKey2, tempKey, tfile->header.keyLen / (collate->keySizeCharPerCharAdd + 1) ) ;

         // need to to a true memcmp here, not a u4 one...
         int rc = r4success ;
         if ( memcmp( dbfKey, outputKey2, matchLen ) != 0 ) // not found
            rc = r4after ;

         u4free( dbfKey ) ;
         if ( allocatedTempKey != 0 )
            u4free( allocatedTempKey ) ;
         return rc ;
      }
   #endif


   #ifdef S4CLIENT
      static int d4seekFetchMultiple( DATA4 *data, const char *key, short keyLen, double dkey, Bool5 seekNext ) ;
   #endif

   int S4FUNCTION d4seek( DATA4 *data, const char *str )
   {
      #ifdef E4PARM_HIGH
         if ( data == 0 || str == 0 )
            return error4( 0, e4parm_null, E92907 ) ;
      #endif

      #ifdef S4CLIENT
         // AS Dec 17/02 - support for d4seek and multi-fetch skip
         if ( data->batchRead.readRecsToBuf != 0 && ( data->readBatchMode & (r4batchSeekMatch | r4batchSeek )) )
            return d4seekFetchMultiple( data, str, (short)strlen( str ), 0, 0 ) ;
         else
            return d4seekServer( data, str, (short)strlen( str ), 0, CON4SEEK, 1 ) ;
      #else
         return d4seekN( data, str, (short)c4strlen( str ) ) ;
      #endif
   }



   #ifdef S4UNIX
      #ifdef S4MDX
         static int data4seekDoubleConvertKey( TAG4FILE *tfile, const double inputKey, char *outputKey )
         {
            switch ( tfile->keyType )
            {
               case r4num:
                  c4bcdFromD( outputKey, inputKey ) ;
                  break ;
               case r4date:
                  t4noChangeDouble( outputKey, inputKey ) ;
                  break ;
               case r4str:
                  break ;
               default:
                  return error4( tfile->codeBase, e4index, E82901 ) ;  /* LY 99/05/11 : data-> to tfile-> */
            }
            return 0 ;
         }



         static int data4seekStrConvertKey( TAG4FILE *tfile, const char *inputKey, char *outputKey, int inputKeyLen, int *outputKeyLen )
         {
            switch ( tfile->keyType )
            {
               case r4num:
                  c4bcdFromA( 0, (char *)outputKey, (char *)inputKey, inputKeyLen, 0 ) ;  /* LY 00/04/19 : change from 3 to 5 parm c4bcdFromA */
                  break ;
               case r4date:
                  t4strToDateMdx( 0, (char *)outputKey, (char *)inputKey, inputKeyLen, 0 ) ;  /* LY 00/04/19 : change from 3 to 5 parm t4strToDateMdx */
                  break ;
               case r4str:
                  if ( outputKeyLen != 0 )
                     if ( *outputKeyLen > tfile->header.keyLen )
                        *outputKeyLen = tfile->header.keyLen ;
                  t4noChangeStr( 0, (char *)outputKey, (char *)inputKey, inputKeyLen, 0 ) ;  /* LY 00/04/19 : change from 3 to 5 parm t4noChangeStr */
                  break ;
               default:
                  return error4( tfile->codeBase, e4index, E82901 ) ;   /* LY 99/05/11 : data-> to tfile-> */
            }

            return 0 ;
         }
      #endif /* S4MDX */



      #ifdef S4FOX
         static int data4seekDoubleConvertKey( TAG4FILE *tfile, const double inputKey, char *outputKey )
         {
            switch ( tfile->keyType )
            {
               case r4int: // CS 2000/05/25
               case r4num:
               case r4numDoub:
               case r4date:
               case r4dateDoub:
                  t4dblToFox( outputKey, inputKey ) ;
                  break ;
               case r4str:
               case r4log:
                  break ;
               default:
                  return error4( tfile->codeBase, e4index, E82901 ) ;   /* LY 99/05/11 : data-> to tfile-> */
            }
            return 0 ;
         }



         static int data4seekStrConvertKey( TAG4FILE *tfile, const char *inputKey, char *outputKey, int inputKeyLen, int *outputKeyLen )
         {
            int dummyLen ;
            switch ( tfile->keyType )
            {
               case r4num:
               case r4numDoub:
                  t4strToFox( 0, outputKey, inputKey, inputKeyLen, &dummyLen ) ;
                  break ;
               case r4date:
               case r4dateDoub:
                  t4dtstrToFox( 0, outputKey, inputKey, inputKeyLen, &dummyLen ) ;
                  break ;
               case r4str:
                  if ( outputKeyLen != 0 )
                     if ( *outputKeyLen > tfile->header.keyLen )
                        *outputKeyLen = tfile->header.keyLen ;
                  t4noChangeStr( 0, outputKey, inputKey, inputKeyLen, &dummyLen ) ;
                  break ;
               case r4log:
                  t4strToLog( 0, outputKey, inputKey, inputKeyLen, &dummyLen ) ;
                  break ;
               default:
                  return error4( tfile->codeBase, e4index, E82901 ) ;   /* LY 99/05/11 : data-> to tfile-> */
            }

            return 0 ;
         }
      #endif /* S4FOX */



      #ifdef S4CLIPPER
         static int data4seekDoubleConvertKey( TAG4FILE *tfile, const double inputKey, char *outputKey )
         {
            switch ( tfile->keyType )
            {
               case r4num:
               case r4numDoub:
               case r4str:
                  break ;
               case r4date:
               case r4dateDoub:
                  t4dateDoubToStr( outputKey, inputKey ) ;
                  break ;
               default:
                  return error4( tfile->codeBase, e4index, E82901 ) ;   /* LY 99/05/11 : data-> to tfile-> */
            }
            return 0 ;
         }



         static int data4seekStrConvertKey( TAG4FILE *tfile, const char *inputKey, char *outputKey, int inputKeyLen, int *outputKeyLen )
         {
            switch ( tfile->keyType )
            {
               case r4num:
               case r4numDoub:
                  /* LY 00/04/20 : change from 3 to 5 parms for t4strToClip */
                  t4strToClip( 0, (char*)outputKey, (char*)inputKey, tfile->header.keyLen, 0 ) ;  /* LY 99/6/10 : fix t4unique:test36 error */
                  break ;
               case r4date:
               case r4dateDoub:
               case r4str:
                  if ( outputKeyLen != 0 )
                     if ( *outputKeyLen > tfile->header.keyLen )
                        *outputKeyLen = tfile->header.keyLen ;
                  /* LY 00/04/20 : change from 3 to 5 parms for t4noChangeStr */
                  t4noChangeStr( 0, (char*)outputKey, (char*)inputKey, inputKeyLen, 0 ) ;
                  break ;
               default:
                  return error4( tfile->codeBase, e4index, E82901 ) ;   /* LY 99/05/11 : data-> to tfile-> */
            }
            return 0 ;
         }
      #endif /* S4CLIPPER */
   #endif /* S4UNIX */



   #if !defined(S4UNIX) && defined(S4CLIPPER)
      static int data4seekStrConvertKey( TAG4FILE *tfile, const char *inputKey, char *outputKey, int inputKeyLen, int *outputKeyLen )
      {
         switch ( tfile4type( tfile ) )
         {
            case r4num:
            case r4numDoub:
               {
                  int numDec = tfile->header.keyDec ;
                  if ( tfile4stokNum( tfile, outputKey, inputKey, tfile->header.keyLen, &numDec ) < 0 )
                     return -1 ;
               }
               break ;
            default:
               if ( outputKeyLen )
                  if ( *outputKeyLen > tfile->header.keyLen )
                     *outputKeyLen = tfile->header.keyLen ;
               if ( tfile4stok( tfile, outputKey, inputKey, inputKeyLen ) < 0 )
                  return -1 ;
               break ;
         }

         return 0 ;
      }
   #endif /* S4CLIPPER && !S4UNIX */



   #ifndef S4CB51
      #ifdef S4CLIENT
         // AS Aug 30/02 - support for d4seekNext and multi-fetch skip
         static int d4seekFetchMultipleDo( DATA4 *data, const char *key, short keyLen, double dkey, Bool5 seekNext )
         {
            assert5port( "New function for advance-reading client/server" ) ;
            d4readBufferReset( data, 1 ) ;
            int actionCode ;
            if ( seekNext == 0 )  // means we are performing BATCH4SEEK or BATCH4SEEKMATCH
            {
               // we may be using option of matching seeks (find all records that match the seek value)
               // or we may be using the option of non-matching (just retrieve the next group of records whether or not they match the seek key)
               if ( data->readBatchMode & r4batchSeekMatch )  // we are performing with matches
                  actionCode = BATCH4SEEKMATCH ;
               else
                  actionCode = BATCH4SEEK ;
            }
            else  // we are peforming BATCH4SEEKNEXT
               actionCode = BATCH4SEEKNEXT ;
            int rc = d4skipFetchLow( data, actionCode, 0, key, keyLen, dkey ) ;
            if ( keyLen < I4MAX_KEY_SIZE )
            {
               data->savedSeekNextBatchKeyLen = keyLen ;
               if ( key == 0 )  // use dkey
               {
                  assert5( keyLen == sizeof( double ) ) ;
                  memcpy( data->savedSeekNextBatchKey, &dkey, keyLen ) ;
               }
               else
                  memcpy( data->savedSeekNextBatchKey, key, keyLen ) ;
            }
            else
               data->savedSeekNextBatchKeyLen = -1 ;
            return rc ;
         }



         // AS Aug 30/02 - support for d4seekNext and multi-fetch skip
         // AS Dec 17/02 - support for d4seek and multi-fetch skip
         static int d4seekFetchMultiple( DATA4 *data, const char *key, short keyLen, double dkey, Bool5 seekNext )
         {
            assert5port( "New function for advance-reading client/server" ) ;

            // AS Mar 4/03 - Need to perform update here if required
            #ifndef S4OFF_WRITE
               int rc = d4updateRecord( data, 0, 1 ) ;
               if ( rc )
                  return rc ;
            #endif

            // if it is already bufferred, this is equivalent to a 'skip forward 1'
            if ( seekNext == 0 || data->batchRead.readBufRecNoOn != data->recNum )   // see if we are positioned in buffer to current pos
            {
               // we were not positioned at correct spot to start, so reset the buffer
               return d4seekFetchMultipleDo( data, key, keyLen, dkey, seekNext ) ;
            }

            int newBufPos = data->batchRead.readBufPos + 1 ;
            if ( newBufPos > data->batchRead.readBufNum )  // means we have gone outside the buffer
               return d4seekFetchMultipleDo( data, key, keyLen, dkey, seekNext ) ;

            data->batchRead.readBufPos = newBufPos ;
            if ( data->batchRead.readBufPos == data->batchRead.readBufNum )  // means we have read last record in buffer
               return d4seekFetchMultipleDo( data, key, keyLen, dkey, seekNext ) ;

            // now, as long as the seek key has not changed, we can use the buffer...
            if ( key != 0 )
            {
               if ( keyLen != data->savedSeekNextBatchKeyLen || memcmp( key, data->savedSeekNextBatchKey, keyLen ) != 0 )
               {
                  // doesn't match, do a new seek from current position
                  return d4seekFetchMultipleDo( data, key, keyLen, dkey, seekNext ) ;
               }
            }
            else // must be a double seek
            {
               if ( keyLen != data->savedSeekNextBatchKeyLen || memcmp( &dkey, data->savedSeekNextBatchKey, keyLen ) != 0 )
               {
                  // doesn't match, do a new seek from current position
                  return d4seekFetchMultipleDo( data, key, keyLen, dkey, seekNext ) ;
               }
            }

            return d4fetchFromBuffer( data, newBufPos ) ;
         }
      #endif /* S4CLIENT */



      int S4FUNCTION d4seekNext( DATA4 *data, const char *str )
      {
         #ifdef S4CLIENT
            int oldErrGo, rc ;
            CODE4 *c4 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( data == 0 || str == 0 )
               return error4( 0, e4parm_null, E92908 ) ;
         #endif

         #ifdef S4CLIENT
            // AS Aug 30/02 - support for d4seekNext and multi-fetch skip, AS Dec 17/02 updated for config flags
            if ( data->batchRead.readRecsToBuf != 0  && ( data->readBatchMode & r4batchSeekNext ) )
               rc = d4seekFetchMultiple( data, str, (short)strlen( str ), 0, 1 ) ;
            else
            {
               c4 = data->codeBase ;
               oldErrGo = c4->errGo ;   /* avoid for r4entry */
               c4->errGo = 0 ;
               rc = d4seekServer( data, str, (short)strlen( str ), 1, CON4SEEK, 1 ) ;
               c4->errGo = oldErrGo ;
               if ( rc < 0 && error4code( c4 ) == 0 )
                  return error4( c4, rc, E92908 ) ;
            }
            return rc ;
         #else
            return d4seekNextN( data, str, (const short)c4strlen( str ) ) ;
         #endif
      }


      #ifndef S4CLIENT
         static int data4seekConvertKeyToTagFormat( DATA4 *data, const int inputKeyLen, const char *inputKey, char *outputKey )
         {
            /*
               converts the input key to a format appropriate to searching within the tag.

               returns the inputKeyLength of the index key
            */

            TAG4FILE *tfile ;
            int outputKeyLen = inputKeyLen ;  /* by default the output len is the same as the input len */

            assert5( d4tagDefault( data ) != 0 ) ;

            tfile = d4tagDefault( data )->tagFile ;

            #ifdef S4FOX
               if ( d4compatibility( data ) == 30 && inputKeyLen == 0 )  /* means seek for .NULL. */
               {
                  outputKeyLen = tfile->header.keyLen ;
                  c4memset( outputKey, 0, outputKeyLen ) ;
               }
               else
            #endif /* S4FOX */
            {  /* for else in S4FOX */
               #ifdef S4FOX
                  /* fox version requires to see if null */
                  if ( d4compatibility( data ) == 30 )
                     if ( expr4context( tfile->expr, data ) < 0 )
                        return -1 ;
               #endif
               /* BCR 09/14/00 -- stok conversion is needed on LINUX.
               #ifdef S4UNIX
                  if ( data4seekStrConvertKey( tfile, inputKey, outputKey, inputKeyLen, 0 ) < 0 )
                     return -1 ;
               #else /* S4UNIX else */
                  outputKeyLen = tfile4stok( tfile, outputKey, inputKey, inputKeyLen ) ;
               //#endif /* S4UNIX else */

               /* do some final verification on the output key length */

               switch( tfile4type( tfile ) )
               {
                  case r4str:
                  case r4charBin:
                  case r5wstr:
                  case r5wstrLen:
                     if ( outputKeyLen <= 0 )
                        outputKeyLen = c4strlen( inputKey ) ;
                     break ;
                  default:
                     outputKeyLen = tfile->header.keyLen ;
                     break ;
               }
            }  /* for S4FOX */

            return outputKeyLen ;
         }



         /* LY 99/08/16 : from static to static int */
         static int d4seekSynchToCurrentPos( DATA4 *data, unsigned char **dbfKeyPtrPtr, int *saveGo )
         {
            TAG4FILE *tfile ;
            int rc = 0 ;
            #ifndef S4OFF_MULTI
               #ifndef S4SERVER
                  int oldErrGo ;
               #endif
            #endif

            assert5( d4tagDefault( data ) != 0 ) ;

            tfile = d4tagDefault( data )->tagFile;

            #ifndef S4OFF_MULTI
               if ( d4lockTest( data, data->recNum, lock4write ) != 1 )  /* ensure latest from disk if we don't have it locked (i.e. we have it changed) */
               {
                  #ifndef S4SERVER
                     oldErrGo = data->codeBase->errGo ;
                     data->codeBase->errGo = 0 ;
                  #endif
                  #ifndef S4OFF_OPTIMIZE
                     data->dataFile->hiPrio = 1 ;
                  #endif
                  *saveGo = d4go( data, data->recNum ) ;
                  #ifndef S4OFF_OPTIMIZE
                     data->dataFile->hiPrio = 0 ;
                  #endif
                  #ifndef S4SERVER
                     data->codeBase->errGo = oldErrGo ;
                  #endif
                  if ( *saveGo < 0 )
                     return *saveGo ;
               }
               else
            #endif
               *saveGo = 0 ;

            if ( *saveGo != r4entry )
            {
               if ( expr4context( tfile->expr, data ) < 0 )
                  return -1 ;

               rc = tfile4exprKey( tfile, dbfKeyPtrPtr ) ;
               if ( rc < 0 )
                  return rc ;

               rc = tfile4go( tfile, *dbfKeyPtrPtr, data->recNum, 0 ) ;
            }

            return rc ;
         }
      #endif /* S4CLIENT */



      short S4FUNCTION d4seekNextN( DATA4 *data, const char *str, const short lenIn )
      {
         // CODE4 *c4 ;
         int rc ;
         #ifdef S4CLIENT
            //int oldErrGo ;
         #else
            int len, rc2, saveGo ;
            TAG4 *tag ;
            TAG4FILE *tfile ;
            unsigned char *dbfKey ;
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( data, 2, E92905 ) )
               return 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( data == 0 || str == 0 )
               return error4( 0, e4parm_null, E92905 ) ;
         #endif

         #ifdef S4CLIENT
            // AS Aug 30/02 - support for d4seekNext and multi-fetch skip, AS Dec 17/02 updated for config flags
            if ( data->batchRead.readRecsToBuf != 0 && ( data->readBatchMode & r4batchSeekNext ) )
               rc = d4seekFetchMultiple( data, str, lenIn, 0, 1 ) ;
            else
            {
               CODE4 *c4 = data->codeBase ;
               int oldErrGo = c4->errGo ;   /* avoid for r4entry */
               c4->errGo = 0 ;
               rc = d4seekServer( data, str, lenIn, 1, CON4SEEK, 1 ) ;
               c4->errGo = oldErrGo ;
               if ( rc < 0 && error4code( c4 ) == 0 )
                  return error4( c4, rc, E92908 ) ;
            }
            return rc ;
         #else
            CODE4 *c4 = data->codeBase ;
            if ( c4 == 0 )
               return e4info ;
            if ( error4code( c4 ) < 0 )
               return -1 ;

            tag = d4tagDefault( data ) ;
            if ( tag == 0 )
               return r4noTag ;

            #ifndef S4OFF_WRITE
               /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
               rc = d4updateRecord( data, 0, 1 ) ;
               if ( rc )
                  return rc ;
            #endif

            tfile = tag->tagFile ;
            // AS 01/30/01 - Problem here - if the tag type is non-character (say int)
            // the input may be 5 numerical characters, but it gets trimmed here to '4'
            // and thus an incorrect seek is done...  This trim must only apply to char seeks
            switch( tfile4type( tag->tagFile ) )
            {
               case r4str:
               case r5wstr:
               case r5wstrLen:
                  if ( lenIn > tfile->header.keyLen )  /* ignore any characters past key len */
                     len = tfile->header.keyLen ;
                  else
                     len = lenIn ;
                  break ;
               default:
                  len = lenIn ;
                  break ;
            }

            /* convert the key to tag format for seeking */
            // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - use the internal CODE4 field buffer instead...
            // char keyConvertedForSeek[I4MAX_KEY_SIZE] ;
            if ( c4->bufLen < (unsigned int)tfile->header.keyLen )
            {
               if ( u4allocAgain( c4, &c4->fieldBuffer, &c4->bufLen, tfile->header.keyLen ) < 0 )
                  return e4memory ;
            }
            char *keyConvertedForSeek = c4->fieldBuffer ;
            len = data4seekConvertKeyToTagFormat( data, len, str, keyConvertedForSeek ) ;

            t4versionCheck( tag, 0, 0 ) ;

            if ( (long)tfile4recNo( tfile ) != data->recNum )
               rc = d4seekSynchToCurrentPos( data, &dbfKey, &saveGo ) ;
            else
            {
               saveGo = 0 ;
               if ( expr4context( tfile->expr, data ) < 0 )
                  return -1 ;
               // AS Nov 24/03 - Fox only
               // AS Nov 8/04 - now in client code...
               #ifdef S4FOX
                  // AS Oct 30/03 - There existed a general collating sequence partial key search
                  // problem.  The basic problem was that the tail characters were interfering with the
                  // partial match (with partial seeks the tail characters are always ignored)  Therefore,
                  // a change was made so that the key conversion would not create tail characters in this case.
                  COLLATE4 *collate = collation4get( tfile->collateName ) ;
                  collate->considerPartialSeek = 1 ;  // instructs the stok to consider we may be doing a partial seek, not that we are for sure doing this

                  collate->hasNull = (unsigned char)expr4nullLow( tfile->expr, 0 ) ; // AS Jan 6/04 - compile fix
                  collate->maxKeyLen = tfile4keyLen( tfile ) ;
                  collate->lenIn = lenIn ;
               #endif
               rc = tfile4exprKey( tfile, &dbfKey ) ;
               #ifdef S4FOX
                  collate->considerPartialSeek = 0 ;
                  collate->lenIn = 0 ;
               #endif
            }

            if ( rc < 0 )
               return rc ;

            if ( len > tfile->header.keyLen )
               len = tfile->header.keyLen ;

            if ( saveGo == r4entry )  /* at eof or bof, so present record not match, so regular seek */
            {
               rc = tfile4seek( tfile, keyConvertedForSeek, len ) ;
               #ifdef S4FOX
                  // AS Nov 8/04 - special case handling for partial seek (fox) with general collation...
                  // in particular if we look for a key with trailing blanks (e.g. '1   ') we trimmed this down to
                  // '1' due to interfering character matching
                  if ( rc == 0 && tfile4type( tfile ) == r4str && len < lenIn && len < tfile->header.keyLen )  // we did special trimming...
                     rc = d4seekMatch( data, tfile, str, lenIn ) ;
               #endif
               return d4seekCheck( data, tfile, rc, keyConvertedForSeek, len ) ;  /* return a valid value */
            }

            /* first check where the datafile currently is in relation to the seeked-for item */
            #ifdef S4FOX
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // rc = u4keycmp( keyConvertedForSeek, dbfKey, len, (unsigned)expr4keyLen( tfile->expr ), 0, &tfile->vfpInfo ) ;
               // AS 05/01/00 -- was passing in paramaters in wrong order - len with wrong value.
               // rc = u4keycmp( keyConvertedForSeek, dbfKey, len, (unsigned)expr4keyLen( tfile->expr ), 0, collation4get( tfile->collateName ) ) ;
               rc = u4keycmpPartial( dbfKey, keyConvertedForSeek, len, (unsigned)expr4keyLen( tfile->expr ), 0, collation4get( tfile->collateName ), str, lenIn ) ;
            #else
               rc = (*tfile->cmp)( keyConvertedForSeek, dbfKey, (unsigned)len ) ;
            #endif

            if ( rc != 0 )  /* present record not match, so regular seek */
            {
               rc = tfile4seek( tfile, keyConvertedForSeek, len ) ;
               #ifdef S4FOX
                  // AS Nov 8/04 - special case handling for partial seek (fox) with general collation...
                  // in particular if we look for a key with trailing blanks (e.g. '1   ') we trimmed this down to
                  // '1' due to interfering character matching
                  if ( rc == 0 && tfile4type( tfile ) == r4str && len < lenIn && len < tfile->header.keyLen )  // we did special trimming...
                     rc = d4seekMatch( data, tfile, str, lenIn ) ;
               #endif
               return d4seekCheck( data, tfile, rc, keyConvertedForSeek, len ) ;  /* return a valid value */
            }

            rc = (int)tfile4dskip( tfile, 1L ) ;
            /* 01/13/99 AS changes.60 fix #159 */
            if ( rc == 0 )   /* on a valid entry, but it is last entry, so r4entry returned */
               return d4goEof( data ) ;
            if ( rc < 0 )
               return rc ;
            else
               rc = 0 ;

            /* need to check the key against the seek key to see whether or not
               we have gone too far */
            #ifdef S4FOX
               /* fox version returns # matching bytes, so subtract to see if valid value */
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // rc = u4keycmp( tfile4key( tfile ), keyConvertedForSeek, (unsigned)len, (unsigned)tfile->header.keyLen, 0, &tfile->vfpInfo ) ;
               rc = u4keycmpPartial( tfile4key( tfile ), keyConvertedForSeek, (unsigned)len, (unsigned)tfile->header.keyLen, 0, collation4get( tfile->collateName ), str, lenIn  ) ;
            #else
               rc = (*tfile->cmp)( keyConvertedForSeek, tfile4key( tfile ), (unsigned)len ) ;
            #endif
            // AS Oct 30/01 - We don't want to actually use the 'rc' here as input, because it is a simple
            // -1,0,1 comparison result.
            // an 'rc' of non-zero corresponds to 'r4entry', so use that instead...
            if ( rc != 0 )
               rc = r4entry ;
            rc2 = d4seekCheck( data, tfile, rc, keyConvertedForSeek, len ) ;  /* return a valid value */
            if ( rc != 0 )
               return r4entry ;
            return rc2 ;
         #endif
      }
   #endif /* !S4CB51 */




   #ifndef S4CLIENT
      // AS July 1/02 - New functionality to allow seeking without data file positioning.
      short d4seekNLow( DATA4 *data, const char *inputKey, const short inputKeyLen, short doDataPosition )
      {
         TAG4 *tag ;
         int rc, outputKeyLen;
         CODE4 *c4 = data->codeBase ;
         if ( c4 == 0 )
            return e4info ;
         if ( error4code( c4 ) < 0 )
            return -1 ;

         tag = d4tagDefault( data ) ;
         if ( tag == 0 )
            return r4noTag ;

         // AS Oct 18/06 - there was a problem here...the outputKey was set up, but the d4updateRecord() has the possibility of
         // freeing that memory...do the update first
         #ifndef S4OFF_WRITE
            /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc )
               return rc ;
         #endif

         TAG4FILE *tfile = tag->tagFile ;
         // AS Jun 4/04 - Support for index key sizes > I4MAX_KEY_SIZE - use the internal CODE4 field buffer instead...
         // char outputKey[I4MAX_KEY_SIZE] ;
         if ( c4->bufLen < (unsigned int)tfile->header.keyLen )
         {
            if ( u4allocAgain( c4, &c4->fieldBuffer, &c4->bufLen, tfile->header.keyLen ) < 0 )
               return e4memory ;
         }
         char *outputKey = c4->fieldBuffer ;

         outputKeyLen = inputKeyLen ;

         #ifdef S4FOX
            if ( d4compatibility( data ) == 30 && outputKeyLen == 0 )  /* means seek for .NULL. */
            {
               outputKeyLen = tfile->header.keyLen ;
               c4memset( outputKey, 0, outputKeyLen ) ;
            }
            else
         #endif
            {  /* for S4FOX */
               #ifdef S4CLIPPER
                  // AS Oct 19/04 - ensure that the inputKeyLen does not exceed the outputKeyLen or we may overwrite memory
                  short inputLen = inputKeyLen ;
                  if ( inputLen > tfile->header.keyLen )
                  {
                     inputLen = tfile->header.keyLen ;
                     outputKeyLen = tfile->header.keyLen ;
                  }
                  if ( data4seekStrConvertKey( tfile, inputKey, outputKey, inputLen, &outputKeyLen ) < 0 )
                     return -1 ;
               #else
                  #ifdef S4FOX
                     /* fox version requires to see if null */
                     if ( d4compatibility( data ) == 30 )
                        if ( expr4context( tag->tagFile->expr, data ) < 0 )
                           return -1 ;
                  #endif
                  if ( tfile4type( tfile ) == r4str )
                  {
                     int kLen = tfile->header.keyLen ;
                     #ifdef S4FOX
                        // AS 07/05/00 - for general collate sequence, determine # of characters to count
                        // by dividing output by key expansion... (t4vfp1.c)
                        COLLATE4 *collate = collation4get( tfile->collateName ) ;
                        kLen /= ( 1 + collate->keySizeCharPerCharAdd ) ;
                     #endif
                     if ( outputKeyLen > kLen )
                        outputKeyLen = kLen ;
                  }
                  outputKeyLen = tfile4stok( tfile, outputKey, inputKey, outputKeyLen ) ;
               #endif

               switch( tfile4type( tfile ) )
               {
                  case r4str:
                  case r4charBin:
                     if ( outputKeyLen <= 0 )
                        outputKeyLen = c4strlen( inputKey ) ;
                     break ;
                  #if !defined( S4DOS ) && !defined( S4WIN16)
                     case r5wstr:
                     case r5wstrLen:
                        if ( outputKeyLen <= 0 )
                           /* LY 2001/07/13 : changed to c4wcslen for 4 byte wchar on Linux */
                           outputKeyLen = c4wcslen( (const WSTR5 *)inputKey ) * 2 ;
                        break ;
                  #endif
                  default:
                     outputKeyLen = tfile->header.keyLen ;
                     break ;
               }
            }  /* for S4FOX */

         t4versionCheck( tag, 0, 0 ) ;
         rc = tfile4seek( tfile, outputKey, outputKeyLen ) ;
         #ifdef S4FOX
            // AS Nov 8/04 - special case handling for partial seek (fox) with general collation...
            // in particular if we look for a key with trailing blanks (e.g. '1   ') we trimmed this down to
            // '1' due to interfering character matching
            if ( rc == 0 && tfile4type( tfile ) == r4str && outputKeyLen < inputKeyLen && outputKeyLen < tfile->header.keyLen )  // we did special trimming...
               rc = d4seekMatch( data, tfile, inputKey, inputKeyLen ) ;
         #endif

         if ( doDataPosition == 0 )
            return rc ;

         return d4seekCheck( data, tfile, rc, outputKey, outputKeyLen ) ;  /* return a valid value */
      }
   #endif /* !S4CLIENT */



   // AS July 1/02 - New functionality to9 allow seeking without data file positioning.
   short S4FUNCTION d4seekN( DATA4 *data, const char *inputKey, const short inputKeyLen )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92903 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 || inputKey == 0 )
            return error4( 0, e4parm_null, E92903 ) ;
      #endif

      #ifdef S4CLIENT
         // AS Dec 17/02 - support for d4seek and multi-fetch skip
         if ( data->batchRead.readRecsToBuf != 0 && ( data->readBatchMode & (r4batchSeekMatch | r4batchSeek )) )
            return d4seekFetchMultiple( data, inputKey, inputKeyLen, 0, 0 ) ;
         else
            return d4seekServer( data, inputKey, inputKeyLen, 0, CON4SEEK, 1 ) ;
      #else
         return d4seekNLow( data, inputKey, inputKeyLen, 1 ) ;
      #endif
   }



   int S4FUNCTION d4seekDouble( DATA4 *data, const double dkey )
   {
      #ifndef S4CLIENT
         TAG4 *tag ;
         TAG4FILE *tfile ;
         CODE4 *c4 ;
         int rc ;
         char buf[I4MAX_KEY_SIZE] ;
         #ifdef S4CLIPPER
            int len ;
         #endif
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E92903 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E92904 ) ;
      #endif

      #ifdef S4CLIENT
         // AS Dec 17/02 - support for d4seek and multi-fetch skip
         if ( data->batchRead.readRecsToBuf != 0 && ( data->readBatchMode & (r4batchSeekMatch | r4batchSeek )) )
            return d4seekFetchMultiple( data, 0, sizeof( double ), dkey, 0 ) ;
         else
         {
            char temp[8] ;
            c4htond( (void *)&dkey, temp ) ; /* LY 2003/04/17 : see LY 2003/04/17 in c4conlow.c */
            return d4seekServer( data, temp, sizeof( double ), 0, CON4SEEK_DBL, 1 ) ;
         }
      #else
         c4 = data->codeBase ;
         if ( c4 == 0 )
            return e4info ;
         if ( error4code( c4 ) < 0 )
            return -1 ;

         tag = d4tagDefault( data ) ;
         if ( tag == 0 )
            return r4noTag ;

         #ifndef S4OFF_WRITE
            /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc )
               return rc ;
         #endif

         tfile = tag->tagFile ;

         #ifdef S4CLIPPER
            if ( tfile->dtok == 0 )
            {
               len = tfile->header.keyLen ;
               c4dtoa45( dkey, buf, len, tfile->header.keyDec ) ;
               if ( buf[0] == '*' )  /* unknown overflow result */
                  return -1 ;
               c4clip( buf, len, tfile->header.keyDec ) ;
            }
            else
         #else
            if ( tfile->dtok == 0 )
               return error4( data->codeBase, e4seek, E82902 ) ;
         #endif

         #ifdef S4FOX  // CS 2000/05/25 moved up
            /* fox version requires to see if null */
            if ( d4compatibility( data ) == 30 )
               if ( expr4context( tag->tagFile->expr, data ) < 0 )
                  return -1 ;
         #endif

         // CS 2000/05/25 data4seekDoubleConvertKey doesn't work
         //#ifdef S4UNIX
         //   if ( data4seekDoubleConvertKey( tfile, dkey, buf ) < 0 )
         //      return -1 ;
         //#else
            tfile4dtok( tfile, buf, dkey ) ;
         //#endif

         t4versionCheck( tag, 0, 0 ) ;
         rc = tfile4seek( tfile, buf, tfile->header.keyLen ) ;

         return d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
      #endif
   }



   #if !defined( S4NO_LONGLONG )
      int S4FUNCTION d4seekLongLong( DATA4 *data, const LONGLONG dkey )
      {
         #ifdef E4PARM_HIGH
            if ( data == 0 )
               return error4( 0, e4parm_null, E92904 ) ;
         #endif

         #ifdef S4CLIENT
            return d4seekServer( data, &dkey, sizeof( LONGLONG ), 0, CON4SEEK_LONGLONG, 1 ) ;
         #else
            TAG4 *tag ;
            TAG4FILE *tfile ;
            CODE4 *c4 ;
            int rc ;
            char buf[I4MAX_KEY_SIZE] ;
            #ifdef S4CLIPPER
               int len ;
            #endif

            #ifdef E4VBASIC
               if ( c4parm_check( data, 2, E92903 ) )
                  return 0 ;
            #endif

            c4 = data->codeBase ;
            if ( c4 == 0 )
               return e4info ;
            if ( error4code( c4 ) < 0 )
               return -1 ;

            tag = d4tagDefault( data ) ;
            if ( tag == 0 )
               return r4noTag ;

            #ifndef S4OFF_WRITE
               rc = d4updateRecord( data, 0, 1 ) ;
               if ( rc )
                  return rc ;
            #endif

            #ifdef S4FOX  // CS 2000/05/25 moved up
               if ( d4compatibility( data ) == 30 )
                  if ( expr4context( tag->tagFile->expr, data ) < 0 )
                     return -1 ;
            #endif

            tfile = tag->tagFile ;
            // only support for the r5i8 field type
            if ( tfile4type( tfile ) != r5i8 )
            {
               return error4describe( data->codeBase, e4notSupported, E92907, "d4seekLongLong not supported for tags not of r5i8 type", 0 , 0 ) ;
            }

            t4i8ToFox( buf, &dkey ) ;

            t4versionCheck( tag, 0, 0 ) ;
            rc = tfile4seek( tfile, buf, tfile->header.keyLen ) ;

            return d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
         #endif /* S4CLIENT else */
      }

      int S4FUNCTION d4seekNextLongLong( DATA4 *data, const LONGLONG dkey )
      {
         CODE4 *c4 ;
         int rc ;
         #ifdef S4CLIENT
            int oldErrGo ;
         #else
            int rc2, saveGo ;
            TAG4 *tag ;
            TAG4FILE *tfile ;
            char buf[I4MAX_KEY_SIZE] ;
            unsigned char *dbfKey ;
            #ifdef S4CLIPPER
               int len ;
            #endif
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( data, 2, E92909 ) )
               return 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( data == 0 )
               return error4( 0, e4parm_null, E92909 ) ;
         #endif

         #ifdef S4CLIENT
            c4 = data->codeBase ;
            oldErrGo = c4->errGo ;   /* avoid for r4entry */
            c4->errGo = 0 ;
            rc = d4seekServer( data, &dkey, sizeof( LONGLONG ), 1, CON4SEEK_LONGLONG, 1 ) ;
            c4->errGo = oldErrGo ;
            if ( rc < 0 && error4code( c4 ) == 0 )
               return error4( c4, rc, E92908 ) ;
            return rc ;
         #else
            c4 = data->codeBase ;
            if ( c4 == 0 )
               return e4info ;
            if ( error4code( c4 ) < 0 )
               return -1 ;

            tag = d4tagDefault( data ) ;
            if ( tag == 0 )
               return r4noTag ;

            #ifndef S4OFF_WRITE
               /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
               rc = d4updateRecord( data, 0, 1 ) ;
               if ( rc )
                  return rc ;
            #endif

            tfile = tag->tagFile ;

            #ifdef S4FOX  // CS 2000/05/25 moved up
               /* fox version requires to see if null */
               if ( d4compatibility( data ) == 30 )
                  if ( expr4context( tag->tagFile->expr, data ) < 0 )
                     return -1 ;
            #endif

            // only support for the r5i8 field type
            if ( tfile4type( tfile ) != r5i8 )
            {
               return error4describe( data->codeBase, e4notSupported, E92907, "d4seekLongLong not supported for tags not of r5i8 type", 0 , 0 ) ;
            }

            t4i8ToFox( buf, &dkey ) ;

            t4versionCheck( tag, 0, 0 ) ;

            if ( (long)tfile4recNo( tfile ) != data->recNum )
               rc = d4seekSynchToCurrentPos( data, &dbfKey, &saveGo ) ;
            else
            {
               saveGo = 0 ;
               if ( expr4context( tfile->expr, data ) < 0 )
                  return -1 ;
               rc = tfile4exprKey( tfile, &dbfKey ) ;
            }

            if ( rc < 0 )
               return rc ;

            if ( saveGo == r4entry )  /* at eof or bof, so present record not match, so regular seek */
            {
               rc = tfile4seek( tfile, buf, tfile->header.keyLen ) ;
               return d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
            }

            /* first check where the datafile currently is in relation to the
               seeked-for item */
            #ifdef S4FOX
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // rc = u4keycmp( dbfKey, buf, (unsigned)tfile->header.keyLen, (unsigned)expr4keyLen( tfile->expr ), 0, &tfile->vfpInfo ) ;
               rc = u4keycmp( dbfKey, buf, (unsigned)tfile->header.keyLen, (unsigned)expr4keyLen( tfile->expr ), 0, collation4get( tfile->collateName ) ) ;
            #else
               rc = (*tfile->cmp)( buf, dbfKey, (unsigned)tfile->header.keyLen ) ;
            #endif

            if ( rc != 0 )  /* present record not match, so regular seek */
            {
               rc = tfile4seek( tfile, buf, tfile->header.keyLen ) ;
               return d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
            }

            rc = (int)tfile4dskip( tfile, 1L ) ;
            /* 01/13/99 AS changes.60 fix #159 */
            if ( rc == 0 )
               return d4goEof( data ) ;
            if ( rc < 0 )
               return rc ;
            else
               rc = 0 ;

            /* need to check the key against the seek key to see whether or not
               we have gone too far */
            #ifdef S4FOX
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // rc = u4keycmp( tfile4key( tfile ), buf, tfile->header.keyLen, (unsigned)tfile->header.keyLen, 0, &tfile->vfpInfo ) ;
               rc = u4keycmp( tfile4key( tfile ), buf, tfile->header.keyLen, (unsigned)tfile->header.keyLen, 0, collation4get( tfile->collateName ) ) ;
            #else
               rc = (*tfile->cmp)( buf, tfile4key( tfile ), tfile->header.keyLen ) ;
            #endif
            rc2 = d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
            if ( rc != 0 )
               return r4entry ;
            return rc2 ;
         #endif
      }
   #endif /* !defined( S4NO_LONGLONG ) */



   #ifndef S4CB51
      int S4FUNCTION d4seekNextDouble( DATA4 *data, const double dkey )
      {
         CODE4 *c4 ;
         int rc ;
         #ifdef S4CLIENT
            double temp ;
            int oldErrGo ;
         #else
            int rc2, saveGo ;
            TAG4 *tag ;
            TAG4FILE *tfile ;
            char buf[I4MAX_KEY_SIZE] ;
            unsigned char *dbfKey ;
            #ifdef S4CLIPPER
               int len ;
            #endif
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( data, 2, E92909 ) )
               return 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( data == 0 )
               return error4( 0, e4parm_null, E92909 ) ;
         #endif

         #ifdef S4CLIENT
            // AS Aug 30/02 - support for d4seekNext and multi-fetch skip, AS Dec 17/02 updated for config flags
            if ( data->batchRead.readRecsToBuf != 0  && ( data->readBatchMode & r4batchSeekNext ) )
            {
               rc = d4seekFetchMultiple( data, 0, sizeof( double ), dkey, 1 ) ;
            }
            else
            {
               c4 = data->codeBase ;
               oldErrGo = c4->errGo ;   /* avoid for r4entry */
               temp = htond(dkey) ;
               c4->errGo = 0 ;
               rc = d4seekServer( data, &temp, sizeof( double ), 1, CON4SEEK_DBL, 1 ) ;
               c4->errGo = oldErrGo ;
               if ( rc < 0 && error4code( c4 ) == 0 )
                  return error4( c4, rc, E92908 ) ;
            }
            return rc ;
         #else
            c4 = data->codeBase ;
            if ( c4 == 0 )
               return e4info ;
            if ( error4code( c4 ) < 0 )
               return -1 ;

            tag = d4tagDefault( data ) ;
            if ( tag == 0 )
               return r4noTag ;

            #ifndef S4OFF_WRITE
               /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
               rc = d4updateRecord( data, 0, 1 ) ;
               if ( rc )
                  return rc ;
            #endif

            tfile = tag->tagFile ;
            #ifdef S4CLIPPER
               if ( tfile->dtok == 0 )
               {
                  len = tfile->header.keyLen ;
                  c4dtoa45( dkey, buf, len, tfile->header.keyDec ) ;
                  if ( buf[0] == '*' )  /* unknown overflow result */
                     return -1 ;
                  c4clip( buf, len, tfile->header.keyDec ) ;
               }
               else
            #else
               if ( tfile->dtok == 0 )
                  return error4( data->codeBase, e4seek, E82902 ) ;
            #endif

            #ifdef S4FOX  // CS 2000/05/25 moved up
               /* fox version requires to see if null */
               if ( d4compatibility( data ) == 30 )
                  if ( expr4context( tag->tagFile->expr, data ) < 0 )
                     return -1 ;
            #endif

            // CS 2000/05/25 data4seekDoubleConvertKey doesn't work
            //#ifdef S4UNIX
            //   if ( data4seekDoubleConvertKey( tfile, dkey, buf ) < 0 )
            //      return -1 ;
            //#else
               tfile4dtok( tfile, buf, dkey ) ;
            //#endif

            t4versionCheck( tag, 0, 0 ) ;

            if ( (long)tfile4recNo( tfile ) != data->recNum )
               rc = d4seekSynchToCurrentPos( data, &dbfKey, &saveGo ) ;
            else
            {
               saveGo = 0 ;
               if ( expr4context( tfile->expr, data ) < 0 )
                  return -1 ;
               rc = tfile4exprKey( tfile, &dbfKey ) ;
            }

            if ( rc < 0 )
               return rc ;

            if ( saveGo == r4entry )  /* at eof or bof, so present record not match, so regular seek */
            {
               rc = tfile4seek( tfile, buf, tfile->header.keyLen ) ;
               return d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
            }

            /* first check where the datafile currently is in relation to the
               seeked-for item */
            #ifdef S4FOX
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // rc = u4keycmp( dbfKey, buf, (unsigned)tfile->header.keyLen, (unsigned)expr4keyLen( tfile->expr ), 0, &tfile->vfpInfo ) ;
               rc = u4keycmp( dbfKey, buf, (unsigned)tfile->header.keyLen, (unsigned)expr4keyLen( tfile->expr ), 0, collation4get( tfile->collateName ) ) ;
            #else
               rc = (*tfile->cmp)( buf, dbfKey, (unsigned)tfile->header.keyLen ) ;
            #endif

            if ( rc != 0 )  /* present record not match, so regular seek */
            {
               rc = tfile4seek( tfile, buf, tfile->header.keyLen ) ;
               return d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
            }

            rc = (int)tfile4dskip( tfile, 1L ) ;
            /* 01/13/99 AS changes.60 fix #159 */
            if ( rc == 0 )
               return d4goEof( data ) ;
            if ( rc < 0 )
               return rc ;
            else
               rc = 0 ;

            /* need to check the key against the seek key to see whether or not
               we have gone too far */
            #ifdef S4FOX
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // rc = u4keycmp( tfile4key( tfile ), buf, tfile->header.keyLen, (unsigned)tfile->header.keyLen, 0, &tfile->vfpInfo ) ;
               rc = u4keycmp( tfile4key( tfile ), buf, tfile->header.keyLen, (unsigned)tfile->header.keyLen, 0, collation4get( tfile->collateName ) ) ;
            #else
               rc = (*tfile->cmp)( buf, tfile4key( tfile ), tfile->header.keyLen ) ;
            #endif
            rc2 = d4seekCheck( data, tfile, rc, buf, tfile->header.keyLen ) ;  /* return a valid value */
            if ( rc != 0 )
               return r4entry ;
            return rc2 ;
         #endif
      }
   #endif  /* S4CB51 */
#endif  /* S4OFF_INDEX */
