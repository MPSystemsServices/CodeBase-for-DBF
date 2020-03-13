/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* f4memo.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

extern unsigned short f4memoNullChar ;

// AS Dec 5/05 - fix for OFF_MEMO - it is exported, so make it generally available...
/* used internally only */
void S4FUNCTION f4memoAssignField( FIELD4 *fieldTo, FIELD4 *fieldFrom )
{
   #if !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE )
      /* AS 08/03/98 - was not assigning if r4general, r4binary field types */
      if ( fieldFrom->type == fieldTo->type && ( fieldFrom->type == r4memo
         #ifdef S4MDX
           || fieldFrom->type == r4bin
         #endif
         #ifdef S4FOX
           || fieldFrom->type == r4gen
         #endif
          ) )
         f4memoAssignN( fieldTo, f4memoPtr( fieldFrom ), f4memoLen( fieldFrom ) ) ;
      else
   #endif /* #if !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE ) */
         f4assignField( fieldTo, fieldFrom ) ;
 }



#ifndef S4OFF_WRITE
   int S4FUNCTION f4memoAssign( FIELD4 *field, const char *ptr )
   {
      #ifdef E4PARM_HIGH
         if ( field == 0 )
            return error4( 0, e4parm_null, E90518 ) ;
      #endif

      if ( error4code( field->data->codeBase ) < 0 )
         return e4codeBase ;

      #ifdef S4OFF_MEMO
         if ( ptr == 0 )
            f4assignN( field, (char *)0, 0 ) ;
         else
            f4assignN( field, ptr, (unsigned int)c4strlen( ptr ) ) ;
         return 0 ;
      #else
         if ( ptr == 0 )
            return f4memoAssignN( field, (char *)0, 0 ) ;
         else
            return f4memoAssignN( field, ptr, (unsigned int)c4strlen( ptr ) ) ;
      #endif
   }



   // AS Jan 14/04 - this doesn't apply for the client since it isn't removing keys from index tags
   #if !defined( S4OFF_MEMO ) && !defined( S4CLIENT )
      // AS Jan 7/04 - centralize the coding to save the old memo field information
      // AS Dec 12/05 - used externally for deleted row recycling...
      int f4memoSaveOld( FIELD4 *field, Bool5 checkIfInTag )
      {
         CODE4 *c4 = field->data->codeBase ;
         F4MEMO *mfield = field->memo ;
         int lenToAlloc ;
         // AS Mar 28/05 - issue here, if the status is set to 0 (memo read), we were not saving the memo.  What we really want to
         // track is whether or not we have already saved the memo...
         // if ( d4hasMemoExpr( field->data ) == r4memoTagExpression && field->memo->status == 1 )
         // AS Dec 12/05 - update for saving memo for reclycing deleted rows...
         if ( field->memo->savedMemo == 0 )
            if ( checkIfInTag == 0 || d4hasMemoExpr( field->data ) == r4memoTagExpression )
            {
               unsigned long len = f4memoLen( field ) ;
               /* LY May 3/04 : replaced min() (non-standard function) */

               if ( checkIfInTag == 1 )
               {
                  lenToAlloc = code4memSizeMemoExprGet( c4 ) ;
                  mfield->lenContentsOld = ( len < (unsigned long)lenToAlloc ? len : lenToAlloc ) ;  // never make larger than code4 key allocation for memo size
               }
               else
               {
                  mfield->lenContentsOld = len ;
                  lenToAlloc = len ;
               }
               if ( mfield->lenContentsOld != 0 )
               {
                  if ( mfield->lenContentsOldAlloc < mfield->lenContentsOld )
                  {
                     if ( mfield->contentsOld )
                     {
                        u4free( mfield->contentsOld ) ;
                        mfield->lenContentsOldAlloc = 0 ;
                     }
                     mfield->contentsOld = u4allocFree( c4, lenToAlloc ) ;
                     if ( mfield->contentsOld == 0 )
                     {
                        mfield->lenContentsOld = 0 ;
                        return e4memory ;
                     }
                     mfield->lenContentsOldAlloc = lenToAlloc ;
                  }
                  memcpy( mfield->contentsOld, mfield->contents, mfield->lenContentsOld ) ;
               }
               field->memo->savedMemo = 1 ;
            }
         return 0 ;
      }
   #endif //   #if !defined( S4OFF_MEMO ) && !defined( S4CLIENT )



   int S4FUNCTION f4memoAssignN( FIELD4 *field, const char *ptr, const unsigned int ptrLen )
   {
      #ifndef S4OFF_MEMO
         CODE4 *c4 ;
         F4MEMO *mfield ;
         int rc ;
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( (void *)field, 3, E90519 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 )
            return error4( 0, e4parm_null, E90519 ) ;
      #endif

      #ifdef S4OFF_MEMO
         f4assignN( field, ptr, ptrLen ) ;
      #else
         c4 = field->data->codeBase ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         #ifndef S4SERVER
            #ifndef S4OFF_ENFORCE_LOCK
               if ( c4->lockEnforce && field->data->recNum > 0L )
                  if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
                     return error4( c4, e4lock, E90519 ) ;
            #endif
         #endif

         if ( ptrLen > UINT_MAX - 128 )
            return error4( c4, e4memory, E85202 ) ;

         mfield = field->memo ;
         if ( !mfield )
            f4assignN( field, ptr, ptrLen ) ;
         else
         {
            // #ifndef S4CLIENT
               // AS Jun 2/03 - Was failing to properly update indexes on memo fields.  Need to keep a copy of the old key around.
               // need to consider that we need to load it from disk if it hasn't been read yet (based on status)
               // AS Jan 7/04 - f4memoSetLen saves the old memo
               // rc = f4memoSaveOld( field, 1 ) ;
               // if ( rc != 0 )
               //    return rc ;
            // #endif
            // AS Jan 7/04 - f4assignNotNull also moved to f4memoSetLen where it is also required
            rc = f4memoSetLen( field, ptrLen ) ;
            if ( rc )
               return error4stack( c4, rc, E90519 ) ;
            memcpy( mfield->contents, ptr, (size_t)ptrLen ) ;
         }
      #endif

      return 0 ;
   }
#endif /* !S4OFF_WRITE */



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION f4memoFree( FIELD4 *field )
{
   #ifndef S4OFF_MEMO
      F4MEMO *mfield ;

      #ifdef E4PARM_HIGH
         if ( field == 0 )
            return error4( 0, e4parm_null, E90521 ) ;
      #endif

      mfield = field->memo ;

      #ifdef E4ANALYZE
         if ( !mfield )
            return error4( 0, e4struct, E90521 ) ;
      #endif

      // AS 06/28/00 - was not freeing in non-fox where we need to know if ptr != f4memoNullPtr...
      // if ( mfield->lenMax > 0 )
      if ( mfield->lenMax > 0 || mfield->contents !=(char *)(&f4memoNullChar) )
         u4free( mfield->contents ) ;

      mfield->contents = (char *)(&f4memoNullChar) ;

      mfield->status = 1 ;
      mfield->lenMax = 0 ;
      // AS Nov 1/02 - Was not resetting the length to 0 on free, causing potential
      // gpf's and incorrect memos to be written
      mfield->len = 0 ;
   #endif
   return 0 ;
}



unsigned long S4FUNCTION f4memoLen( FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90522 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90522 ) ;
         return 0 ;
      }
   #endif


   #ifdef S4OFF_MEMO
      return (unsigned)field->len ;
   #else
      if ( !field->memo )
         return (unsigned)field->len ;
      if ( field->memo->status == 1 )
      {
         if ( f4memoRead( field ) != 0 )
            return 0 ;
         field->memo->status = 0 ;
      }
      // AS Jul 16/03 - does not apply in client mode
      #if defined( S4CLIENT )
         return field->memo->len ;
      #else
         // AS Jun 2/03 - Was failing to properly update indexes on memo fields.  Support reading old data
         if ( field->data->useOldMemo == 0 )
            return field->memo->len ;
         // else use old memo
         // AS Apr 4/05 - Issue here...even if we should be using the old memo, it wasn't always stored.  For example,
         // if a key is changing that has a memo field, but the memo part doesn't actually change, we won't have saved a change
         // to the memo.  Therefore we should continue to use the new memo since it hasn't chaqnged...
         // AS Feb 8/06 - compile fix
         #ifndef S4OFF_WRITE
            if ( field->memo->savedMemo == 0 )
            {
               field->data->useOldMemo = 0 ;
               f4memoSaveOld( field, 1 ) ;
               field->data->useOldMemo = 1 ;
            }
         #endif
         return field->memo->lenContentsOld ;
      #endif
   #endif
}



unsigned long S4FUNCTION f4memoNcpy( FIELD4 *field, char *memPtr, const unsigned len )
{
   #ifndef S4OFF_MEMO
      CODE4 *c4 ;
      unsigned numCpy ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90523 ) ;
         return 0 ;
      }
   #endif

   #ifdef S4OFF_MEMO
      return f4ncpy( field, memPtr, len ) ;
   #else
      c4 = field->data->codeBase ;
      if ( !field->memo )
         return f4ncpy( field, memPtr, len ) ;

      if ( len == 0 )
         return 0 ;

      if ( error4code( c4 ) < 0 )
         return 0 ;
      error4set( c4, 0 ) ;

      numCpy = f4memoLen( field ) ;
      if ( len <= numCpy )
         numCpy = len - 1 ;

      memcpy( memPtr, f4memoPtr( field ), (size_t)numCpy ) ;

      memPtr[numCpy] = '\000' ;

      return( numCpy ) ;
   #endif
}



char *S4FUNCTION f4memoPtr( FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90524 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90524 ) ;
         return 0 ;
      }
   #endif

   #ifdef S4OFF_MEMO
      return f4ptr(field) ;
   #else
      if ( !field->memo )
         return f4ptr(field) ;
      if ( field->memo->status == 1 )
      {
         if ( f4memoRead( field ) )
            return 0 ;
         field->memo->status = 0 ;
      }

      // AS Jul 16/03 - does not apply in client mode
      #if defined( S4CLIENT )
         return field->memo->contents ;
      #else
         // AS Jun 2/03 - Was failing to properly update indexes on memo fields.  Support reading old data
         if ( field->data->useOldMemo == 0 )
            return field->memo->contents ;
         // else use old memo
         return (char *)(field->memo->contentsOld) ;
      #endif
   #endif
}



// for ODBC
int S4FUNCTION f4memoResetExport( FIELD4 *field )
{
   #ifdef S4OFF_MEMO
      return e4notSupported ;
   #else
      return f4memoReset( field ) ;
   #endif
}



#ifndef S4OFF_MEMO
   #ifdef S4CLIENT
      int f4memoReadSet( FIELD4 *field, long memoLen, const char *contents )
      {
         // AS May 21/02 - if contents paramater is left null everything is done except the actual memo
         // copy.  This allows us to configure the memo prior to copying the data from the socket in client/server.
         F4MEMO *mfield = field->memo ;

         if ( memoLen == 0 )
         {
            if ( mfield->lenMax == 0 )
               mfield->contents = (char *)(&f4memoNullChar) ;
            else
            {
               mfield->contents[0] = 0 ;  // CS 1999/09/14 make last 2 bytes NULL (Unicode NULL)
               mfield->contents[1] = 0 ;
            }
            field->memo->status = 0 ;
            mfield->len = 0 ;
            return 0 ;
         }
         else
         {
            if ( (long)mfield->lenMax < memoLen )
            {
               if ( mfield->lenMax != 0 )
                  u4free( mfield->contents ) ;
               mfield->lenMax = mfield->len = 0 ;
               mfield->contents = (char *)u4allocFree( field->data->codeBase, memoLen + 2 ) ;  // CS 1999/09/14 allow room for Unicode NULL character
               if ( mfield->contents == 0 )
               {
                  mfield->contents = (char *)(&f4memoNullChar) ;
                  return e4memory ;
               }
               mfield->lenMax = (unsigned int)memoLen ;
            }
         }
         mfield->len = (unsigned int)memoLen ;

         // AS May 21/02 - new support, if contents is null don't copy, just set the length and null out...
         if ( contents != 0 )
            memcpy( mfield->contents, contents, memoLen ) ;
         mfield->contents[(mfield->len) + 0] = 0 ;  // CS 1999/09/14 make last 2 bytes NULL (Unicode NULL)
         mfield->contents[(mfield->len) + 1] = 0 ;
         field->memo->status = 0 ;
         return 0 ;
      }
   #endif


   int f4memoRead( FIELD4 *field )
   {
      int rc ;
      F4MEMO *mfield ;
      CODE4 *c4 ;
      #ifdef S4CLIENT
         CONNECTION4 *connection ;
         CONNECTION4MEMO_INFO_IN *info ;
         CONNECTION4MEMO_INFO_OUT *out ;
         DATA4 *data ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 )
            return error4( 0, e4parm_null, E90525 ) ;
      #endif

      c4 = field->data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      mfield = field->memo ;
      mfield->isChanged = 0 ;

      if ( d4recNo( field->data ) < 0 )
      {
         mfield->len = 0 ;
         return mfield->len ;
      }

      #ifdef S4CLIENT
         data = field->data ;
         connection = data->dataFile->connection ;
         if ( connection == 0 )
            return error4( c4, e4connection, E90525 ) ;

         connection4assign( connection, CON4MEMO, data4clientId( data ), data4serverId( data ) ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4MEMO_INFO_IN ), (void **)&info ) ;
         info->recNo = htonl5(d4recNo( data )) ;
         // AS Oct 27/03 - long field names support
         info->fieldNo = htons5((short)d4fieldNumber( data, field->longName )) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E90525 ) ;
         rc = connection4status( connection ) ;
         if ( rc < 0 )
            return connection4error( connection, c4, rc, E90525 ) ;

         if ( rc == r4entry )  /* no record, therefore no memo entry */
         {
            if ( mfield->lenMax == 0 )
               mfield->contents = (char *)(&f4memoNullChar) ;
            else
            {
               mfield->contents[0] = 0 ;  // CS 1999/09/14 make last 2 bytes NULL (Unicode NULL)
               mfield->contents[1] = 0 ;
            }
            field->memo->status = 0 ;
            mfield->len = 0 ;
            return 0 ;
         }

         if ( connection4len( connection ) < sizeof( CONNECTION4MEMO_INFO_OUT ) )
            return e4packetLen ;

         out = (CONNECTION4MEMO_INFO_OUT *)connection4data( connection ) ;
         out->memoLen = ntohl5( out->memoLen ) ;

         if ( connection4len( connection ) != (long)sizeof( CONNECTION4MEMO_INFO_OUT ) + (long)out->memoLen )
            return e4packetLen ;

         return f4memoReadSet( field, out->memoLen, ((char *)out) + sizeof( CONNECTION4MEMO_INFO_OUT ) ) ;
      #else
         #ifndef S4OFF_MULTI
            if ( c4getReadLock( c4 ) )
            {
               rc = d4validateMemoIds( field->data ) ;
               if ( rc < 0 )
                  return error4stack( c4, rc, E90525 ) ;
            }
         #endif

         rc = f4memoReadLow( field ) ;
         if ( rc < 0 )
            return error4stack( c4, rc, E90525 ) ;

         return 0 ;
      #endif
   }



   #ifndef S4CLIENT
      int f4memoReadLow( FIELD4 *field )
      {
         F4MEMO *mfield ;
         int rc ;
         #ifdef S4MFOX
            long mType ;
         #endif

         #ifdef E4PARM_LOW
            if ( field == 0 )
               return error4( 0, e4parm_null, E90526 ) ;
         #endif

         if ( error4code( field->data->codeBase ) < 0 )
            return e4codeBase ;

         mfield = field->memo ;
         #ifdef E4ANALYZE
            if ( !mfield )
               return error4( field->data->codeBase, e4info, E80502 ) ;
         #endif

         mfield->len = mfield->lenMax  ;

         // AS Jun 23/03 - For ODBC purposes we want to track the id associated with the read field
         long fldOffset = f4long( field ) ;
         #ifdef S4MFOX
            rc = memo4fileRead( &field->data->dataFile->memoFile, fldOffset, &mfield->contents, &mfield->len, &mType ) ;
         #else
            rc = memo4fileRead( &field->data->dataFile->memoFile, fldOffset, &mfield->contents, &mfield->len ) ;
         #endif

         if ( rc == 0 )
         {
            #ifdef S4MNDX
               if ( mfield->lenMax > 0 )
                  if ( mfield->len == 0 )
                  {
                     u4free( mfield->contents ) ;
                     mfield->contents = 0 ;
                  }
               if ( mfield->len > mfield->lenMax )
                  mfield->lenMax = mfield->len  ;
               if ( mfield->len == 0 )
               {
                  mfield->lenMax = 0 ;
                  mfield->contents = (char *)(&f4memoNullChar) ;
               }
            #else
               if ( mfield->len > mfield->lenMax )
                  mfield->lenMax = mfield->len  ;
               if ( mfield->lenMax > 0 )
               {
                  mfield->contents[(mfield->len) + 0] = 0 ;  // CS 1999/09/14 make last 2 bytes NULL (Unicode NULL)
                  mfield->contents[(mfield->len) + 1] = 0 ;
               }
               else
                  mfield->contents = (char *)(&f4memoNullChar) ;
            #endif
            // AS Jun 23/03 - For ODBC purposes we want to track the id associated with the read field
            mfield->memoOffset = fldOffset ;
         }

         return rc ;
      }
   #endif  // !S4CLIENT



   int f4memoReset( FIELD4 *field )
   {
      #ifdef E4PARM_LOW
         if ( field == 0 )
            return error4( 0, e4parm_null, E90527 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( field->memo == 0 )
            return error4( 0, e4struct, E90527 ) ;
      #endif

      field->memo->len = 0 ;
      field->memo->status = 1 ;
      field->memo->isChanged = 0 ;
      #ifndef S4CLIENT
         // AS Mar 28/05 - issue here, if the status is set to 0 (memo read), we were not saving the memo.  What we really want to
         // track is whether or not we have already saved the memo...
         field->memo->savedMemo = 0 ;

         // AS Jun 23/03 - For ODBC purposes we want to track the id associated with the read field
         field->memo->memoOffset = -1 ;
      #endif

      return 0 ;
   }



   // for ODBC
   void S4FUNCTION d4memoResetExport( DATA4 *data )
   {
      #ifndef S4OFF_MEMO
         if ( data->fieldsMemo != 0 )
         {
            for ( int memoFieldIndex = 0 ; memoFieldIndex < data->dataFile->nFieldsMemo ; memoFieldIndex++ )
            {
               f4memoReset( data->fieldsMemo[memoFieldIndex].field ) ;
            }
         }
      #endif
   }



   #ifndef S4OFF_WRITE
      int S4FUNCTION f4memoSetLen( FIELD4 *field, const unsigned len )
      {
         F4MEMO *mfield ;
         CODE4 *c4 ;

         #ifdef E4PARM_LOW
            if ( field == 0 )
               return error4( 0, e4parm_null, E90528 ) ;
         #endif

         c4 = field->data->codeBase ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         #ifndef S4SERVER
            #ifndef S4OFF_ENFORCE_LOCK
               if ( c4->lockEnforce  && field->data->recNum > 0L )
                  if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
                     return error4( c4, e4lock, E90528 ) ;
            #endif
         #endif

         mfield = field->memo ;
         if ( mfield == 0 )
            return -1 ;

        // AS Jan 7/04 - some functions were dependent on f4memoSetLen removing the null marker which wasn't being done.
        // centralized this to this location
        #ifdef S4FOX
           if ( d4compatibility( field->data ) == 30 )
              f4assignNotNull( field ) ;
        #endif

         // AS Jan 7/04 - we need to consider updating indexes here as well (previously applied to f4memoAssignN)
         // AS Jan 14/04 - this doesn't apply for the client since it isn't removing keys from index tags
         // AS Feb 8/06 - compile fix
         #if !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
            int rc = f4memoSaveOld( field, 1 ) ;
            if ( rc != 0 )
               return rc ;
         #endif

         if ( mfield->lenMax < len )
         {
            if ( mfield->lenMax > 0 )
               u4free( mfield->contents ) ;
            mfield->lenMax = len ;

            mfield->contents = (char *)u4allocEr( c4, (long)mfield->lenMax + 2L ) ;  // CS 1999/09/14 allow room for Unicode NULL character
            if ( mfield->contents == 0 )
            {
               mfield->contents = (char *)(&f4memoNullChar) ;
               mfield->lenMax = 0 ;
               mfield->status = 1 ;
               return e4memory ;
            }
         }

         mfield->len = len ;

         if ( mfield->lenMax == 0 )
            mfield->contents = (char *)(&f4memoNullChar) ;
         else
         {
            mfield->contents[(mfield->len) + 0] = 0 ;  // CS 1999/09/14 make last 2 bytes NULL (Unicode NULL)
            mfield->contents[(mfield->len) + 1] = 0 ;
         }

         mfield->status = 0 ;
         mfield->isChanged = 1 ;
         field->data->recordChanged = 1 ;
         return 0 ;
      }
   #endif  /* S4OFF_WRITE */
#endif  /* S4OFF_MEMO */



S4CONST char *S4FUNCTION f4memoStr( FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90529 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90529 ) ;
         return 0 ;
      }
   #endif

   #ifdef S4OFF_MEMO
      return f4str( field ) ;
   #else
      if ( field->memo == 0 )
         return f4str( field ) ;
      return f4memoPtr( field ) ;
   #endif
}



#if !defined(S4CLIENT) && !defined(S4OFF_WRITE) && !defined(S4OFF_MEMO)
   int f4memoUpdate( FIELD4 *field )
   {
      #ifdef E4PARM_LOW
         if ( field == 0 )
            return error4( 0, e4parm_null, E90530 ) ;
      #endif

      // AS Sep 2/03 - help with debugging a problem.
      CODE4 *c4 = field->data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      if ( field->memo )
      {
         if ( field->memo->isChanged )
            return f4memoWrite( field ) ;
      }

      return 0 ;
   }



   int f4memoWrite( FIELD4 *field )
   {
      int rc ;
      long memoId, newId ;

      #ifdef E4PARM_LOW
         if ( field == 0 )
            return error4( 0, e4parm_null, E90531 ) ;
      #endif

      if ( error4code( field->data->codeBase ) < 0 )
         return e4codeBase ;

      #ifndef S4OFF_MULTI
         rc = d4validateMemoIds( field->data ) ;
         if ( rc < 0 )
            return error4stack( field->data->codeBase, rc, E90531 ) ;
      #endif

      // AS Jun 23/03 - For ODBC purposes we want to track the id associated with the read field
      F4MEMO *mfield = field->memo ;
      mfield->memoOffset = -1 ;

      memoId = f4long( field ) ;
      newId = memoId ;

      rc = memo4fileWrite( &field->data->dataFile->memoFile, &newId, mfield->contents, mfield->len ) ;

      if ( rc < 0 )
         return error4stack( field->data->codeBase, rc, E90531 ) ;

      if ( newId != memoId )
      {
         if ( newId )
            f4assignLong( field, newId ) ;
         else
            f4blank( field ) ;   /* for FOX 3.0, must set to 0, else spaces */
      }

      // AS Jun 23/03 - For ODBC purposes we want to track the id associated with the read field
      mfield->memoOffset = -1 ;

      mfield->isChanged = newId ;
      return 0 ;
   }
#endif  /* !S4OFF_MEMO && !S4OFF_WRITE && !S4CLIENT */



unsigned int S4FUNCTION f4memoReadPart( FIELD4 *field, char *outBuffer, unsigned int readLen, unsigned int startPos )
{
   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parmNull, E90524 ) ;
         return 0 ;
      }
   #endif

   CODE4 *c4 = field->data->codeBase ;

   #if !defined(S4CLIENT) && !defined(S4OFF_MEMO) && defined( S4FOX )
      // if error, returns 0 and C4->errorCode is set
      // AS 03/29/00 Added partial read/write support independent of normal mechanism

      // LOCKING - same as f4memoRead(), this function will lock the record as
      // part of the read, and does not unlock afterwards.  Can unlock
      // afterwards manually
      #ifdef E4VBASIC
         if ( c4parm_check( (void *)field, 3, E90524 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( outBuffer == 0 || !field->memo )
         {
            error4( c4, e4parm, E90524 ) ;
            return 0 ;
         }
      #endif

      // LY Jul 20/04 : added !S4MACINTOSH
      #if !defined( S4LUPACH ) && !defined( S4MACINTOSH )   /* LY July 7/03 */
         assert5port( "added compressed memo entries support" ) ;
         // AS Jun 19/02 - This function is not supported for compressed memo fields - it is too tricky at this
         // point, could add in support in the future on a consulting basis
         if ( field->data->dataFile->compressedMemosSupported == 1 )
            return error4describe( c4, e4notSupported, E90524, "f4memoReadPart() is not supported on compressed memo fields", 0, 0) ;
      #endif

      if ( error4code( c4 ) < 0 )
         return 0 ;

      F4MEMO *mfield = field->memo ;
      mfield->isChanged = 0 ;

      if ( d4recNo( field->data ) < 0 )
      {
         mfield->len = 0 ;
         return 0 ;
      }

      #ifndef S4OFF_MULTI
         if ( c4getReadLock( c4 ) )
         {
            int rc = d4validateMemoIds( field->data ) ;
            if ( rc < 0 )
               return 0 ;
         }
      #endif

      #ifdef E4ANALYZE
         if ( !mfield )
         {
            error4( c4, e4info, E80502 ) ;
            return 0 ;
         }
      #endif

      mfield->len = mfield->lenMax  ;

      // we pass in &outBuffer, but in actual fact it never changes... (we hope)
      long mType ;
      char *oldOutBuf = outBuffer ;
      unsigned int lenRead = readLen ;
      memo4fileReadPart( &field->data->dataFile->memoFile, f4long( field ), &outBuffer, &lenRead, startPos, readLen, &mType, 1 ) ;
      assert5( oldOutBuf == outBuffer ) ;

      return lenRead ;
   #else
      error4( c4, e4notSupported, E80502 ) ;
      return 0 ;
   #endif  /* !S4OFF_MEMO && !S4CLIENT && defined( S4FOX ) */
}


int S4FUNCTION f4memoWriteFinish( DATA4 *data )
{
   // AS 06/09/00 was not compiling in S4OFF_MULTI
   #if !defined( S4CLIENT ) && !defined( S4OFF_MEMO ) && defined( S4FOX ) && !defined( S4OFF_MULTI )
      return dfile4memoUnlock( data->dataFile ) ;
   #else
      return error4( data->codeBase, e4notSupported, E80502 ) ;
   #endif
}



int S4FUNCTION f4memoWritePart( FIELD4 *field, char *dataToWrite, unsigned int dataLen, long memoLen, long offset )
{
   #ifdef E4PARM_HIGH
      if ( field == 0 || field->data == 0 )
         return error4( 0, e4parmNull, E90524 ) ;
   #endif

   DATA4 *data = field->data ;
   CODE4 *c4 = data->codeBase ;

   #if !defined(S4CLIENT) && !defined(S4OFF_MEMO) && defined( S4FOX ) && !defined( S4OFF_WRITE )
      #ifdef E4VBASIC
         if ( c4parm_check( (void *)field, 3, E90524 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( dataToWrite == 0 || !field->memo )
            return error4( c4, e4parm, E90524 ) ;

         if ( data->recNum < 1L )
         {
            return error4( c4, e4notSupported, E84801 ) ;
         }
      #endif

      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifdef E4PARM_LOW
         if ( field == 0 )
            return error4( 0, e4parm_null, E90528 ) ;
      #endif

      // AS 06/09/00 was not compiling in S4OFF_MULTI
      int rc ;
      #ifndef S4OFF_MULTI
         rc = d4lockInternal( data, d4recNo( data ), 1 ) ;
         if ( rc )
            return rc ;

         if ( offset == 0 )
         {
            rc = dfile4lockMemo( data->dataFile ) ;
            if ( rc )
               return rc ;
         }

         rc = d4validateMemoIds( data ) ;
         if ( rc < 0 )
            return rc ;
      #endif

      // AS 11/22/00 - ensure the old record is up to date before we start overwriting...
      if ( d4readOld( data, d4recNo( data ) ) < 0 )
         return -1 ;

      long memoId = f4long( field ) ;
      long newId = memoId ;

      rc = memo4fileWritePart( &data->dataFile->memoFile, &newId, dataToWrite, memoLen, offset, dataLen, 1L ) ;

      if ( rc < 0 )
         return rc ;

      if ( newId != memoId )
      {
         if ( newId )
            f4assignLong( field, newId ) ;
         else
            f4blank( field ) ;   /* for FOX 3.0, must set to 0, else spaces */
      }
      if ( rc < 0 )
         return rc ;

      // AS 11/22/00 - we only want to update the memo entry piece, leave the rest of the record unchanged for now...
      // update the memo entry in the data's record
      char *tempRecord = data->record ;
      data->record = data->recordOld ;
      if ( newId != memoId )
      {
         if ( newId )
            f4assignLong( field, newId ) ;
         else
            f4blank( field ) ;   /* for FOX 3.0, must set to 0, else spaces */
      }
      rc = d4writeData( data, d4recNo( data ), 0 ) ;
      // AS 11/22/00 d4writeData() resets the recordChanged flag, causing problems with indexes not getting updated later
      data->recordChanged = 1 ;
      data->record = tempRecord ;

      return rc ;
   #else
      return error4( c4, e4notSupported, E80502 ) ;
   #endif  /* !S4OFF_MEMO && !S4CLIENT && defined( S4FOX ) && !defined( S4OFF_WRITE ) */
}


/* AS Sep 27/01 - New coding for assigning and retrieving memos as files */
int S4FUNCTION f4memoAssignFile( FIELD4 *field, S4CONST char *fileNameFrom )
{
   // assigns the contents of a file to a memo field
   #ifdef E4PARM_HIGH
      if ( field == 0 || fileNameFrom == 0 )
         return error4( 0, e4parm_null, E90518 ) ;
   #endif

   CODE4 *c4 = field->data->codeBase ;

   // LY Jul 20/04 : added !S4MACINTOSH
   #if !defined( S4LUPACH ) && !defined( S4MACINTOSH )   /* LY July 7/03 */
      #if !defined(S4CLIENT) && defined(S4FOX)  /* LY 2002/12/27 : added S4FOX */
         assert5port( "added compressed memo entries support" ) ;
         // AS Jun 25/02 - This function is not supported for compressed memo fields - it is too tricky at this
         // point, could add in support in the future on a consulting basis
         if ( field->data->dataFile->compressedMemosSupported == 1 )
            return error4describe( c4, e4notSupported, E90524, "f4memoReadPart() is not supported on compressed memo fields", 0, 0) ;
      #endif
   #endif

   /* LY July 7/03 : added !defined(S4LUPACH) */
   #if !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4FOX ) && !defined(S4LUPACH)
      // allocate 48k.  32k will be given to the file4seqRead module, which with advance
      // reading will read in 16k chunks.  The other 16k will be used for retrieving the
      // memory in 16k pieces (the ideal size with advance reading in 16k chunks as well).
      const int readSize = 16 * 1024 ;
      const int bufLen = 3 * readSize ;
      const int offset = 2 * readSize ;   // offset into buffer which we use for storing data
      char *buffer = (char *)u4allocEr( c4, bufLen ) ;
      if ( buffer == 0 )
         return e4memory ;

      FILE4 fileFrom ;
      char *readTo = buffer + offset;

      // open file in shared read-only mode...
      int oldReadOnly = c4getReadOnly( c4 ) ;
      c4setReadOnly( c4, 1 ) ;
      int oldAccessMode = c4getAccessMode( c4 ) ;
      c4setAccessMode( c4, OPEN4DENY_NONE ) ;
      // AS May 24/02 - created file4openLow for internal use to indicate file types
      int rc = file4openInternal( &fileFrom, c4, fileNameFrom, 0, OPT4NONE ) ;  // this is the file to copy from
      c4setAccessMode( c4, oldAccessMode ) ;
      c4setReadOnly( c4, oldReadOnly ) ;
      if ( rc != 0 )
      {
         u4free( buffer ) ;
         return rc ;
      }

      FILE4LONG finalMemoLen = file4lenLow( &fileFrom ) ;

      FILE4SEQ_READ seqRead ;
      FILE4LONG zero ;
      file4longAssign( zero, 0, 0 ) ;
      rc = file4seqReadInitDo( &seqRead, &fileFrom, zero, buffer, readSize*2, 1 ) ;
      if ( rc == 0 )
      {
         FILE4LONG memoPos ;
         file4longAssign( memoPos, 0, 0 ) ;
         for( ;; )
         {
            unsigned amtRead = file4seqRead( &seqRead, readTo, readSize ) ;
            if ( amtRead == 0 )
            {
               rc = error4code( c4 ) ;
               break ;
            }
            FILE4LONG cmp ;
            file4longAssignLong( cmp, memoPos ) ;
            file4longAdd( &cmp, amtRead ) ;
            if ( file4longLessLong( finalMemoLen, cmp ) )
            {
               // possible the file has been written to while we are reading from it
               // in this case just stop after we reach the end of the 'original' length of the file.
               FILE4LONG result ;
               file4longAssignLong( result, finalMemoLen ) ;
               file4longSubtractLong( &result, &memoPos ) ;
               assert5( file4longGetHi( result ) == 0 ) ;   // we are reading last bit, must be less than a large value
               amtRead = file4longGetLo( result ) ;
            }

            // memo's don't support entries LONGLONG yet, so ensure it is not so.
            assert5( file4longGetHi( finalMemoLen ) == 0 ) ;
            assert5( file4longGetHi( memoPos ) == 0 ) ;
            rc = f4memoWritePart( field, readTo, amtRead, file4longGetLo( finalMemoLen ), file4longGetLo( memoPos ) ) ;
            if ( rc != 0 )
               break ;
            file4longAdd( &memoPos, amtRead ) ;
            if ( amtRead < readSize )  // means we didn't read full size, so we must be complete
            {
               if ( !file4longEqualLong( memoPos, finalMemoLen ) )  // means the file has become shortened... this is an error
                  rc = error4( 0, e4memoCorrupt, E90518 ) ;
               break ;
            }
         }
         #ifdef S4ADVANCE_READ
            // AS Sept 24/02 - not needed nor defined if S4ADVANCE_READ not defined
            file4seqReadInitUndo( &seqRead ) ;
         #endif
      }

      u4free( buffer ) ;
      file4close( &fileFrom ) ;
      return rc ;
   #else
      return error4( c4, e4notSupported, E80502 ) ;
   #endif /*if !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4FOX ) && !defined(S4LUPACH) */
}



int S4FUNCTION f4memoFile( FIELD4 *field, S4CONST char *fileNameTo )
{
   // copies the contents of a memo field to a file
   // to allow overwriting of an existing file, set safety to 0
   // if the memo is empty an empty file is created, so it is adviseable to
   // check via f4memoLen() first...

   #ifdef E4PARM_HIGH
      if ( field == 0 || fileNameTo == 0 )
         return error4( 0, e4parm_null, E90518 ) ;
   #endif

   CODE4 *c4 = field->data->codeBase ;

   // LY Jul 20/04 : added !S4MACINTOSH
   /* LY July 7/03 : added !defined(S4LUPACH) */
   #if !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4FOX ) && !defined(S4LUPACH) && !defined( S4MACINTOSH )
      assert5port( "added compressed memo entries support" ) ;
      // AS Jun 19/02 - This function is not supported for compressed memo fields - it is too tricky at this
      // point, could add in support in the future on a consulting basis
      if ( field->data->dataFile->compressedMemosSupported == 1 )
         return error4describe( c4, e4notSupported, E90518, "f4memoReadPart() is not supported on compressed memo fields", 0, 0) ;


      // allocate 48k.  32k will be given to the file4seqWrite module, which with delay
      // writes will write in 16k chunks.  The other 16k will be used for retrieving the
      // memo in 16k pieces (the ideal size with delay writing in 16k chunks as well).
      const int writeSize = 16 * 1024 ;
      const int bufLen = 3 * writeSize ;
      const int offset = 2 * writeSize ;   // offset into buffer which we use for storing data
      char *buffer = (char *)u4allocEr( c4, bufLen ) ;
      if ( buffer == 0 )
         return e4memory ;

      FILE4 fileTo ;
      char *writeTo = buffer + offset;

      int oldAccessMode = c4getAccessMode( c4 ) ;
      c4setAccessMode( c4, OPEN4DENY_NONE ) ;
      // AS May 24/02 - created file4createInternal for internal use to indicate file types
      // this file is a file to output the memo to, so it doesn't get preprocessed.
      int rc = file4createInternal( &fileTo, c4, fileNameTo, 0, OPT4NONE ) ;
      c4setAccessMode( c4, oldAccessMode ) ;
      if ( rc != 0 )
      {
         u4free( buffer ) ;
         return rc;
      }

      FILE4SEQ_WRITE seqWrite ;
      FILE4LONG zero ;
      file4longAssign( zero, 0, 0 ) ;
      rc = file4seqWriteInitLow( &seqWrite, &fileTo, zero, buffer, writeSize * 2 ) ;
      if ( rc == 0 )
      {
         for( long curPos = 0 ;; )
         {
            unsigned amtRead = f4memoReadPart( field, writeTo, writeSize, curPos ) ;
            if ( amtRead == 0 )
            {
               rc = error4code( c4 ) ;
               break ;
            }
            rc = file4seqWrite( &seqWrite, writeTo, amtRead ) ;
            if ( rc < 0 )
               break ;
            curPos += amtRead ;
            if ( amtRead < writeSize )  // did not read all bytes, so must be done.
               break ;
         }

         int rcSv = file4seqWriteFlush( &seqWrite ) ;
         if ( rc == 0 )
            rc = rcSv ;
      }

      u4free( buffer ) ;
      file4close( &fileTo ) ;
      return rc ;
   #else
      return error4( c4, e4notSupported, E80502 ) ;
   #endif /*if !defined( S4OFF_MEMO ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) && defined( S4FOX ) && !defined(S4LUPACH) */
}


// AS Jun 23/03 - For ODBC purposes we want to track the id associated with the read field
long S4FUNCTION f4memoGetOffset( FIELD4 S4PTR *field )
{
   #ifdef S4CLIENT
      return e4notSupported ;
   #else
      // AS Dec 5/05 - fix for OFF_MEMO
      #ifdef S4OFF_MEMO
         return 0 ;
      #else
         F4MEMO *mfield ;
         mfield = field->memo ;
         if ( mfield == 0 )
            return 0 ;
         return mfield->memoOffset ;
      #endif
   #endif
}



int S4FUNCTION f4isMemo( FIELD4 S4PTR *f4 )
{
   // AS Dec 5/05 - fix for OFF_MEMO
   #ifdef S4OFF_MEMO
      return 0 ;
   #else
      #ifdef E4PARM_HIGH
         if ( f4 == 0 )
            return error4( 0, e4parm_null, E90518 ) ;
      #endif

      return ( f4->memo == 0 ? 0 : 1 ) ;
   #endif
}


unsigned int S4FUNCTION f4memoLenMax( FIELD4 S4PTR *f4 )
{
   // AS Dec 5/05 - fix for OFF_MEMO
   #ifdef S4OFF_MEMO
      return e4notSupported ;
   #else
      #ifdef E4PARM_HIGH
         if ( f4 == 0 )
            return error4( 0, e4parm_null, E90518 ) ;

         if ( f4->memo == 0 )
            return error4( 0, e4info, E90518 ) ;
      #endif

      return f4->memo->lenMax ;
   #endif
}
