/* d4skip.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4SERVER
   long d4skipRecno( DATA4 *data, long n )
   {
      /* -2 means eof, -3 means bof */
      #ifndef S4OFF_INDEX
         TAG4 *tag ;
         TAG4FILE *tagFile ;
      #endif
      long startRec, newRec ;
      #ifndef S4OFF_INDEX
         unsigned char *keyValue ;
         int rc ;
         long recno, nSkipped ;
      #endif
      CODE4 *c4 ;

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94801 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E94801 ) ;
      #endif

      c4 = data->codeBase ;
      if ( c4 == 0 )
         return e4info ;
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;

      if ( data->recNum < 1L )
      {
         #ifndef S4SERVER
            if ( c4->errSkip )
               return error4( c4, e4info, E84801 ) ;
         #endif
         return e4info ;
      }

      #ifndef S4OFF_INDEX
         tag = data->tagSelected ;  /* avoid function call */
         if ( tag == 0 )
         {
      #endif
         if ( n != 0L )
            if ( d4recCountLessEq( data, 1L ) == 0 )  /* count == 0 */
               return -4L ;

         data->bofFlag = 0 ;
         startRec = data->recNum ;
         newRec = startRec + n ;

         if ( newRec > 0L )
         {
            if ( d4recCountLessEq( data, newRec ) == 0 )
               return -2L ;
            return newRec ;
         }
         else
            return -3L ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            tagFile = tag->tagFile ;
            if ( data->eofFlag )
            {
               if ( n >= 0L )
                  return -2L ;

               rc = d4bottom( data ) ;
               if ( rc && rc != r4eof )
                  return rc ;
               if ( rc == r4eof )
                  return -2L ;
               n++ ;
               data->recNum = (long)tfile4recNo( tagFile ) ;
            }

            data->bofFlag = 0 ;

            #ifndef S4OFF_WRITE
               if ( data->recordChanged )
               {
                  /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
                  rc = d4updateRecord( data, 0, 1 ) ;
                  if ( rc < 0 )
                     return rc ;
               }
            #endif

            t4versionCheck( tag, 1, 0 ) ;

            if ( n == 0 )
               return data->recNum ;

            if ( (long)tfile4recNo( tagFile ) != data->recNum )
            {
               if ( d4lockTest( data, data->recNum, lock4any ) != 1 )  /* ensure latest from disk */
               {
                  rc = d4go( data, data->recNum ) ;
                  if ( rc < 0 )
                     return rc ;
               }

               expr4context( tagFile->expr, data ) ;
               tfile4exprKey( tagFile, &keyValue ) ;

               rc = tfile4go( tagFile, keyValue, data->recNum, 0 ) ;
               if ( rc < 0 )
                  return rc ;

               if ( tfile4empty( tagFile ) )
                  return -4L ;

               if ( tfile4eof( tagFile ) && n >= 0L )
                  return -2L ;

               #ifdef S4HAS_DESCENDING
                  if ( tagFile->header.descending )
                  {
                     if ( (rc > 0) && (n < 0) )
                        n-- ;
                  }
                  else
                     if ( (rc > 0) && (n > 0) )
                        n-- ;
               #else
                  if ( (rc > 0) && (n > 0) )
                     n-- ;
               #endif
            }
            else
            {
               if ( tfile4eof( tagFile ) )
                  return -2L ;
            }

            nSkipped = tfile4dskip( tagFile, n ) ;

            if ( n > 0 && nSkipped != n )
               return -2L ;
            if ( n < 0 && nSkipped != n )
               return -3L ;

            if ( tfile4eof( tagFile ) )
               return -2L ;

            recno = (long)tfile4recNo( tagFile ) ;
            if ( recno < 0 )
               return recno ;
            return recno ;
         }
      #endif
   }
#endif /* S4SERVER */

/* flush the current record to disk, and update the data pointer to point
   to a valid tag location (in case the current record has been filtered out */
/* direction is -1 if line up backwards, or 1 if line up forwards */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4tagSync( DATA4 *data, TAG4 * const tag )
{
   #ifdef S4OFF_INDEX
      return 0 ;
   #else
      CODE4 *c4 ;
      #ifdef S4CLIENT
         int rc, saveRc ;
         CONNECTION4 *connection ;
         CONNECTION4TAG_SYNCH_INFO_IN *info ;
         CONNECTION4TAG_SYNCH_INFO_OUT *out ;
         #ifndef S4OFF_MEMO
            int i ;
         #endif
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E94803 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E94803 ) ;
      #endif

      if ( tag == 0 )
         return 0 ;

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return error4code( c4 ) ;

      #ifdef S4CLIENT
         if ( data->recNum <= 0 || d4eof( data ) )
         {
            #ifndef S4OFF_MEMO
               for ( i = 0; i < data->dataFile->nFieldsMemo; i++ )
                  f4memoReset( data->fieldsMemo[i].field ) ;
            #endif
            data->recordChanged = 0 ;
            return 0 ;
         }

         if ( data->recordChanged == 0 )
         {
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc )
               return rc ;
         }

         connection = data->dataFile->connection ;
         if ( connection == 0 )
            return error4stack( data->codeBase, e4connection, E94803 ) ;

         connection4assign( connection, CON4TAG_SYNCH, data4clientId( data ), data4serverId( data ) ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4TAG_SYNCH_INFO_IN ), (void **)&info ) ;
         memcpy( info->tagName, tag->tagFile->alias, LEN4TAG_ALIAS  ) ;
         info->tagName[LEN4TAG_ALIAS] = 0 ;
         info->recno = htonl5(d4recNo( data )) ;
         saveRc = connection4repeat( connection ) ;

         if ( saveRc == r4eof || saveRc == r4noRecords )
         {
            data->bofFlag = 0 ;
            rc = d4goEof( data ) ;
            if ( rc == 0 )
               rc = r4eof ;
            if ( saveRc == r4noRecords )
               data->bofFlag = 1 ;
            return rc ;
         }

         if ( saveRc == r4bof )
         {
            data->eofFlag = 0 ;
            rc = d4goBof( data ) ;
            data->bofFlag = 1 ;
            if ( rc == 0 )
               rc = r4bof ;
            return rc ;
         }

         if ( saveRc != 0 && saveRc != r4after )
         {
            if ( saveRc < 0 )
               return connection4errorDescribe( connection, c4, saveRc, E94803, tag->tagFile->alias, 0, 0 ) ;
            return saveRc ;
         }

         if ( connection4len( connection ) != sizeof( CONNECTION4TAG_SYNCH_INFO_OUT ) )
            return error4( c4, e4packetLen, E94803 ) ;
         out = (CONNECTION4TAG_SYNCH_INFO_OUT *)connection4data( connection ) ;
         out->recNo = ntohl5(out->recNo) ;
         if ( out->recNo != 0 && out->recNo != d4recNo( data ) )
         {
            rc = d4go( data, out->recNo ) ;
            if ( rc < 0 )
               return rc ;
         }

         return saveRc ;
      #else
         return d4tagSyncDo( data, tag, 1 ) ;
      #endif
   #endif /* S4OFF_INDEX */
}

#if !defined(S4OFF_INDEX) && !defined(S4CLIENT)
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int d4tagSyncDo( DATA4 *data, TAG4 * const tag, int direction )
   {
      int rc, saveRc = 0 ;
      unsigned char *keyValue ;
      #ifndef S4OFF_MULTI
         int done, verifyReccount ;
         #ifndef S4OFF_TRAN
            int nSkipped ;
            CODE4 *c4 ;
         #endif
      #endif
      TAG4 *oldSelected ;
      TAG4FILE *tagFile ;
      long recno ;

      #if !defined(S4OFF_MULTI) && !defined(S4OFF_TRAN)
         c4 = data->codeBase ;
      #endif

      tagFile = tag->tagFile ;
      expr4context( tagFile->expr, data ) ;
      tfile4exprKey( tagFile, &keyValue ) ;
      recno = (long)tfile4recNo( tagFile ) ;
      if ( recno != data->recNum )
      {
         rc = tfile4go( tagFile, keyValue, data->recNum, 0 ) ;
         if ( rc < 0 )
            return rc ;
         if ( rc == r4after )
            saveRc = r4after ;
      }

      oldSelected = data->tagSelected ;
      d4tagSelect( data, tag ) ;

      #ifndef S4OFF_WRITE
         if ( data->recordChanged )
         {
            rc = d4updateRecord( data, 0, 1 ) ;
            if ( rc )
            {
               d4tagSelect( data, oldSelected ) ;
               return rc ;
            }
         }
      #endif

      if ( data->eofFlag )
      {
         d4tagSelect( data, oldSelected ) ;

         /* if forward skip not true, not eof, here is */
         return d4goEof( data ) ;
      }
      else
      {
         t4versionCheck( tag, 1, 0 ) ;

         if ( tfile4eof( tagFile ) )
            return d4goEof( data ) ;

         recno = (long)tfile4recNo( tagFile ) ;
         if ( recno != data->recNum )
         {
            rc = d4go( data, recno ) ;
            saveRc = r4after ;
         }
         else
            rc = 0 ;
      }

      #ifndef S4OFF_MULTI
         #ifdef S4SERVER
            if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) != 1 )
            if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4read ) != 1 )
         #else
            if ( d4lockTestFile( data ) != 1 )
         #endif
            for( verifyReccount = 1, done = 0 ; done == 0 ; )
            {
               #ifndef S4OFF_TRAN
                  /* if transactions are enabled, we do not remove keys from an index file when
                     records are changed to avoid unique problems caused by another user.
                     The result of this is we may need to re-sync ourselves in that case - namely,
                     we do not want to position to a record indicated by a 'removed' entry in
                     a tag.
                  */
                  if ( code4transEnabled( c4 ) )
                     if ( t4unique( tag ) != 0 || tagFile->filter != 0 )
                        if ( code4tranStatus( c4 ) == r4active )
                           if ( d4lockTest( data, recno, lock4write ) == 1 )
                           {
                              /* if within a transaction, and record is locked, it may mean
                                 that the record contents have changed.  If the current tag key
                                 does not match, it must be an old entry (if a unique tag),
                                 and thus skip over
                              */
                              for ( ;; )
                              {
                                 if ( recno > dfile4getMinCount( data->dataFile ) && verifyReccount == 1 )   /* ensure correct sequencing for multi-user */
                                    break ;  /* just let the code below take care of this case */

                                 Bool5 passedFilterTest = 1 ;
                                 Bool5 passedUniqueTest = 1 ;
                                 /* AS 03/25/99 --> also must consider instance where a record now becomes
                                    filtered from the record set...*/
                                 if ( tagFile->filter != 0 )
                                 {
                                    expr4context( tagFile->filter, data ) ;
                                    if ( recno != d4recNo( data ) )  /* need to reposition data4 first */
                                        d4go( data, recno ) ;
                                    if ( expr4true( tagFile->filter ) == 0 ) // should be filtered
                                    {
                                       nSkipped = (int)tfile4skip( tagFile, direction ) ;
                                       passedFilterTest = 0 ;
                                    }
                                 }

                                 if ( t4unique( tag ) != 0 && passedFilterTest == 1 )
                                 {
                                    expr4context( tagFile->expr, data ) ;
                                    if ( recno != d4recNo( data ) )  /* need to reposition data4 first */
                                        d4go( data, recno ) ;
                                    tfile4exprKey( tagFile, &keyValue ) ;
                                    if ( memcmp( tfile4key( tagFile ), keyValue, tagFile->header.keyLen ) != 0 )  /* different, so skip */
                                    {
                                       nSkipped = (int)tfile4skip( tagFile, direction ) ;
                                       passedUniqueTest = 0 ;
                                    }
                                 }

                                 if ( passedUniqueTest == 1 && passedFilterTest == 1 )  // done
                                    break ;

                                 verifyReccount = 1 ;  /* don't re-check reccount if all is ok... */
                                 if ( direction > 0  &&  nSkipped != 1 )
                                 {
                                    rc = d4goEof( data ) ;
                                    done = 1 ;
                                    break ;
                                 }

                                 if ( tfile4eof( tagFile ) )
                                 {
                                    data->bofFlag = 1 ;
                                    rc = d4goEof( data ) ;
                                    done = 1 ;
                                    break ;
                                 }
                                 if ( (long)tfile4recNo( tagFile ) != recno ) /* ok, may continue */
                                 {
                                    recno = (long)tfile4recNo( tagFile ) ;
                                    d4go( data, recno ) ;
                                    break ;
                                 }
                              }
                           }
               #endif /* S4OFF_TRAN */

               if ( done == 1 )
                  break ;

               if ( verifyReccount == 1 )
               {
                  verifyReccount = 0 ;
                  if ( recno > dfile4getMinCount( data->dataFile ) )   /* ensure correct sequencing for multi-user */
                  {
                     while ( d4recCountLessEq( data, recno ) == 0 && done == 0 && rc == 0 )
                     {
                        rc = (int)tfile4skip( tagFile, direction ) ;
                        if ( rc < 0 )
                           done = 1 ;
                        else
                        {
                           if ( rc == 0 )
                           {
                              data->bofFlag = 1 ;
                              done = 1 ;
                              rc = d4goEof( data ) ;
                           }
                           else
                           {
                              recno = (long)tfile4recNo( tagFile ) ;
                              if ( recno <= 0 )
                              {
                                 done = 1 ;
                                 rc = -1 ;
                              }
                              if ( recno > dfile4getMinCount( data->dataFile ) )  // skip to next one
                                 continue ;
                              if ( (long)tfile4recNo( tagFile ) != recno ) /* ok, may continue */
                              {
                                 recno = (long)tfile4recNo( tagFile ) ;
                                 d4go( data, recno ) ;
                                 break ;
                              }
                           }
                        }
                     }
                  }
                  else
                     done = 1 ;
               }
               else
                  done = 1 ;
            }
      #endif /* S4OFF_MULTI */

      d4tagSelect( data, oldSelected ) ;

      if ( saveRc == 0 )
         return rc ;
      else
         return saveRc ;
   }
#endif  // !S4CLIENT && !S4OFF_INDEX

int S4FUNCTION d4skip( DATA4 *data, const long nSkip )
{
   int rc ;
   CODE4 *c4 ;
   #ifdef S4CLIENT
      int saveRc ;
      CONNECTION4 *connection ;
      CONNECTION4GO_INFO_OUT *out ;
      CONNECTION4SKIP_INFO_IN *info ;
   #else
      #ifndef S4OFF_INDEX
         unsigned char *keyValue ;
         long nSkipped, recno ;
         TAG4FILE *tagFile ;
      #endif
      long n ;
      #ifndef S4SERVER
         int saveFlag ;
      #endif
      long startRec, newRec ;
      int oldEofFlag, c1 ;
   #endif
   #ifndef S4OFF_INDEX
      TAG4 *tag ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E94802 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E94802 ) ;
   #endif

   c4 = data->codeBase ;
   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;

   if ( data->recNum < 1L )
   {
      #ifndef S4SERVER
         if ( c4->errSkip )
            error4( c4, e4info, E84801 ) ;
      #endif
      return e4info ;
   }

   #ifdef S4CLIENT
      rc = d4updateRecord( data, 0, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc )
         return rc ;

      connection = data->dataFile->connection ;
      if ( connection == 0 )
         return error4stack( data->codeBase, e4connection, E94802 ) ;

      connection4assign( connection, CON4SKIP, data4clientId( data ), data4serverId( data ) ) ;
      connection4addData( connection, NULL, sizeof( CONNECTION4SKIP_INFO_IN ), (void **)&info ) ;
      info->startPos = htonl5(data->recNum) ;
      info->numSkip = htonl5(nSkip) ;
      #ifdef S4OFF_INDEX
         info->usesTag = 0 ;
      #else
         tag = data->tagSelected ;
         if ( tag == 0 )
            info->usesTag = 0 ;
         else
         {
            info->usesTag = 1 ;
            memcpy( info->tagName, tag->tagFile->alias, LEN4TAG_ALIAS  ) ;
            info->tagName[LEN4TAG_ALIAS] = 0 ;
         }
      #endif
      rc = connection4repeat( connection ) ;
      if ( rc == r4locked )
         return r4locked ;

      if ( (long)connection4len( connection ) < (long)sizeof( CONNECTION4GO_INFO_OUT ) )
         return error4( c4, e4packetLen, E94802 ) ;
      out = (CONNECTION4GO_INFO_OUT *)connection4data( connection ) ;

      saveRc = ntohs5(out->skipRc) ;
      if ( saveRc < 0 )
      {
         char *tagName = 0 ;
         if ( tag != 0 )
            tagName = tag->tagFile->alias ;
         return connection4errorDescribe( connection, c4, saveRc, E94802, tagName, 0, 0 ) ;
      }

      if ( saveRc == r4eof || saveRc == r4noRecords )
      {
         data->bofFlag = 0 ;
         rc = d4goEof( data ) ;
         if ( rc == 0 )
            rc = r4eof ;
         if ( saveRc == r4noRecords )
            data->bofFlag = 1 ;
         return rc ;
      }

      if ( saveRc == r4bof )
      {
         data->eofFlag = 0 ;
         rc = d4goBof( data ) ;
         data->bofFlag = 1 ;
         if ( rc == 0 )
            rc = r4bof ;
         return rc ;
      }

      if ( saveRc != 0 )
      {
         if ( saveRc < 0 )
            return error4( c4, saveRc, E94802 ) ;
         return saveRc ;
      }

      rc = connection4status( connection ) ;
      if ( out->recNo )
      {
         if ( (long)connection4len( connection ) != (long)sizeof( CONNECTION4GO_INFO_OUT ) + (long)dfile4recWidth( data->dataFile ) )
         {
            char *tagName = 0 ;
            if ( tag != 0 )
               tagName = tag->tagFile->alias ;
            return connection4errorDescribe( connection, c4, rc, E94802, tagName, 0, 0 ) ;
         }
         return d4goVirtual( data, ntohl5(out->recNo), rc, out, connection, 0 ) ;  /* maybe r4locked, or whatever */
      }

      return rc ;
   #else
      n = nSkip ;

      #ifndef S4OFF_INDEX
         tag = data->tagSelected ;
         if ( tag == 0 )
         {
      #endif
         data->bofFlag = 0 ;
         startRec = data->recNum ;
         newRec = startRec + n ;
         if ( newRec > 0L )
         {
            #ifndef S4SERVER
               saveFlag = c4->errGo ;
               c4->errGo = 0 ;
            #endif
            #ifndef S4OFF_OPTIMIZE
               data->dataFile->hiPrio = -1 ;  /* indicate d4skip - data level */
            #endif
            rc = d4go( data, newRec ) ;
            #ifndef S4OFF_OPTIMIZE
               data->dataFile->hiPrio = 0 ;
            #endif
            #ifndef S4SERVER
               c4->errGo = saveFlag ;
            #endif
            if ( rc >= 0 && rc != r4entry )
               return rc ;
         }

         c1 = d4recCountLessEq( data, 1L ) ;
         if ( c1 < 0 )
            return c1 ;
         if ( ( c1 == 0 ) || ( d4recCountLessEq( data, newRec ) == 0 ) )
         {
            if ( c1 == 0L )
            {
               rc = d4goEof( data ) ;
               if ( rc != r4eof )
                  return rc ;
               data->bofFlag = 1 ;
            }
            if ( n < 0 )
            {
               data->bofFlag = 1 ;
               return r4bof ;
            }
            else
               return d4goEof( data ) ;
         }

         if ( newRec < 1L )
         {
            oldEofFlag = data->eofFlag ;
            rc = d4go( data, 1L ) ;
            if ( rc )
               return rc ;
            data->bofFlag = 1 ;
            data->eofFlag = oldEofFlag ;
            return r4bof ;
         }

         return d4go( data, newRec ) ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            tagFile = tag->tagFile ;
            if ( data->eofFlag )
            {
               if ( n >= 0 )
                  return d4goEof( data ) ;

               rc = d4bottom( data ) ;
               if ( rc && rc != r4eof )
                  return rc ;
               if ( rc == r4eof )
               {
                  rc = d4goEof( data ) ;
                  if ( rc != r4eof )
                     return rc ;
                  return r4bof ;
               }
               n++ ;
               data->recNum = (long)tfile4recNo( tagFile ) ;
            }

            data->bofFlag = 0 ;

            #ifndef S4OFF_WRITE
               if ( data->recordChanged )
               {
                  /* AS 04/22/97 causes problems with t4seek, unlock reset to 0 */
                  rc = d4updateRecord( data, 0, 1 ) ;
                  if ( rc < 0 )
                     return rc ;
               }
            #endif

            t4versionCheck( tag, 1, 0 ) ;

            if ( n == 0 )
               return 0 ;

            if ( (long)tfile4recNo( tagFile ) != data->recNum )
            {
               rc = d4go( data, data->recNum ) ;
               if ( rc )
                  return rc ;

               expr4context( tagFile->expr, data ) ;
               tfile4exprKey( tagFile, &keyValue ) ;

               rc = tfile4go( tagFile, keyValue, data->recNum, 0 ) ;
               if ( rc < 0 )
                  return rc ;

               #ifdef S4HAS_DESCENDING
                  if ( tagFile->header.descending )
                  {
                     if ( (rc > 0) && (n < 0) )
                        n-- ;
                  }
                  else
                     if ( (rc > 0) && (n > 0) )
                        n-- ;
               #else
                  if ( (rc > 0) && (n > 0) )
                     n-- ;
               #endif
            }

            nSkipped = tfile4dskip( tagFile, n ) ;
            if ( n > 0  &&  nSkipped != n )
               return d4goEof( data ) ;

            if ( tfile4eof( tagFile ) )
            {
               data->bofFlag = 1 ;
               return d4goEof( data ) ;
            }

            /* AS 03/15/99 -- it is possible that the key found may not be in the data file because of update conflicts.  For example
               added to tag but not added to datafile yet.  Or added by another user in transaction so shouldn't go to recor yet.
               modified to skip forward in that case.  */
            int skipDirection ;
            if ( n < 0 )
               skipDirection = -1 ;
            else
               skipDirection = 1 ;

            for( ;; )
            {
               recno = (long)tfile4recNo( tagFile ) ;
               if ( recno < 0 )
                  return (int)recno ;
               if ( d4recCountLessEq( data, (long)tfile4recNo( tagFile ) ) != 0 )  /* valid spot */
                  break ;
               if ( tfile4dskip( tagFile, skipDirection ) == 0 ) // didn't skip
               {
                  data->bofFlag = 1 ;
                  return d4goEof( data ) ;
               }
            }
            rc = d4go( data, recno ) ;
            if ( rc )
               return rc ;
            #ifndef S4OFF_MULTI
               #ifndef S4OFF_TRAN
                  /* if transactions are enabled, we do not remove keys from an index file when
                     records are changed to avoid unique problems caused by another user.
                     The result of this is we may need to re-sync ourselves in that case - namely,
                     we do not want to position to a record indicated by a 'removed' entry in
                     a tag.
                  */
                  if ( code4transEnabled( c4 ) )
                     if ( t4unique( tag ) != 0 ) // || tagFile->filter != 0 )  /* AS 03/25/99 added also to test if filter condition exists... */
                        #ifdef S4SERVER
                           if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) != 1 )
                           if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4read ) != 1 )
                        #else
                           if ( d4lockTestFile( data ) != 1 )
                        #endif
                        {
                           rc = d4tagSyncDo( data, tag, n > 0 ? 1 : -1 ) ;
                           if ( rc != 0 )
                              return rc ;
                        }
               #endif
            #endif
            if ( n == nSkipped )
               return 0 ;

            #ifdef S4HAS_DESCENDING
               if ( ( n < 0 && !tagFile->header.descending ) ||  ( n < 0 && tagFile->header.descending ) )
            #else
               if ( n < 0 )
            #endif
            {
               data->bofFlag = 1 ;
               return r4bof ;
            }
         }
         return 0 ;
      #endif
   #endif
}
