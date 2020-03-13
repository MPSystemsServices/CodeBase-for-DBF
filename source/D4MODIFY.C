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

/* d4modify.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */


int S4FUNCTION d4copyTable( DATA4 *data, const char *destFolder, short includeIndex )
{
        // CS Jun 7/2007
        CODE4 *cb;

        #ifdef E4PARM_HIGH
                if ( data == 0 || destFolder == 0 )
                        return error4( 0, e4parmNull, E94111 ) ;
                if ( strlen( destFolder ) > LEN4PATH )
                        return error4( data->codeBase, e4parm, E94111 ) ;
        #endif

        cb = data->codeBase;

        #if defined(S4WIN32) && !defined(S4WINCE)  // CS 2011/05/09 Some work will need to be done to support CE and UNIX.
                if ( error4code( cb ) < 0 )
                        return e4codeBase ;

                int rc;

                #ifdef S4CLIENT
                        CONNECTION4 *connection = &cb->defaultServer ;
                        CONNECTION4COPY_INFO_IN *dataIn ;

                        connection4assign( connection, CON4COPY, data4clientId( data ), data4serverId( data ) ) ;
                        connection4addData( connection, NULL, sizeof(CONNECTION4COPY_INFO_IN), (void **)&dataIn ) ;
                        dataIn->includeIndex = htons5( includeIndex ) ;
                        c4strcpy(dataIn->path, LEN4PATH, destFolder ) ;
                        int len = strlen( dataIn->path ) ;
                        if ( dataIn->path[len-1] != S4DIR )
                        {
                                if ( len >= LEN4PATH )  // no room for the backslash
                                        return error4( data->codeBase, e4parm, E94111 ) ;
                                dataIn->path[len] = S4DIR ;
                                dataIn->path[len+1] = 0 ;
                        }

                        connection4sendMessage( connection ) ;
                        rc = connection4receiveMessage( connection ) ;
                        if ( rc < 0 )
                                return 0 ;
                        if ( connection4type( connection ) != CON4COPY )
                                return 0 ;

                        rc = connection4status( connection ) ;
                        if ( rc != 0 )
                                return connection4error( connection, cb, rc, E94111 ) ;
                        return 0 ;
                #else
                        #ifdef S4CLIPPER
                                return error4( data->codeBase, e4notSupported, E94111 ) ;
                        #else
                                INDEX4 *indexOn;
                                char destFolderBuff[LEN4PATH+1], fileNameBuff[LEN4PATH+1], fullNameBuff[LEN4PATH+1];
                                rc = d4lockAll(data);
                                if (rc != r4success)
                                {
                                        return rc;
                                }

                                rc = d4flush(data);
                                if (rc != r4success)
                                {
                                        return rc;
                                }

                                // Copy the destination path to a separate buffer. This is
                                // so we can ensure that it has a trailing path delimiter.
                                c4strcpy( destFolderBuff, LEN4PATH, destFolder ) ;
                                int len = strlen( destFolderBuff ) ;
                                if ( destFolderBuff[strlen(destFolderBuff)-1] != S4DIR )
                                {
                                        if ( len >= LEN4PATH )  // no room for the backslash
                                                return error4( data->codeBase, e4parm, E94111 ) ;
                                        destFolderBuff[strlen(destFolderBuff)+0] = S4DIR ;
                                        destFolderBuff[strlen(destFolderBuff)+1] = 0 ;
                                }

                                if (cb->safety != 0)
                                {
                                        // Should be error if any files exist. So check before copying any.

                                        // data file
                                        u4namePiece(fileNameBuff, LEN4PATH, data->dataFile->file.name, 0, 1);
                                        u4nameCurrentExtended(fullNameBuff, LEN4PATH, fileNameBuff, destFolderBuff);
                                        if (file4exists(fullNameBuff) >= 0)
                                        {
                                                return error4describe(cb, e4create, E94111, fullNameBuff, 0, 0);
                                        }

                                        #ifndef S4OFF_MEMO
                                                // memo file
                                                if (data->dataFile->nFieldsMemo > 0)
                                                {
                                                        u4namePiece(fileNameBuff, LEN4PATH, data->dataFile->memoFile.file.name, 0, 1);
                                                        u4nameCurrentExtended(fullNameBuff, LEN4PATH, fileNameBuff, destFolderBuff);
                                                        if (file4exists(fullNameBuff) >= 0)
                                                        {
                                                                return error4describe(cb, e4create, E94111, fullNameBuff, 0, 0);
                                                        }
                                                }
                                        #endif

                                        #ifndef S4OFF_INDEX
                                                if (includeIndex)
                                                {
                                                        for(indexOn = 0; ; )
                                                        {
                                                                indexOn = (INDEX4*)l4next(&data->indexes, indexOn);
                                                                if (indexOn == 0)
                                                                {
                                                                        break ;
                                                                }

                                                                u4namePiece(fileNameBuff, LEN4PATH, indexOn->indexFile->file.name, 0, 1);
                                                                u4nameCurrentExtended(fullNameBuff, LEN4PATH, fileNameBuff, destFolderBuff);
                                                                if (file4exists(fullNameBuff) >= 0)
                                                                {
                                                                        return error4describe(cb, e4create, E94111, fullNameBuff, 0, 0);
                                                                }
                                                        }
                                                }
                                        #endif
                                }  // if safety

                                // copy data file
                                u4namePiece(fileNameBuff, LEN4PATH, data->dataFile->file.name, 0, 1);
                                u4nameCurrentExtended(fullNameBuff, LEN4PATH, fileNameBuff, destFolderBuff);
                                if (CopyFile(data->dataFile->file.name, fullNameBuff, FALSE) == 0)
                                {
                                        return error4describe(cb, e4create, E94111, fullNameBuff, 0, 0);
                                }

                                #ifndef S4OFF_MEMO  // CS 2008/02/04
                                        // memo file
                                        if (data->dataFile->nFieldsMemo > 0)
                                        {
                                                u4namePiece(fileNameBuff, LEN4PATH, data->dataFile->memoFile.file.name, 0, 1);
                                                u4nameCurrentExtended(fullNameBuff, LEN4PATH, fileNameBuff, destFolderBuff);
                                                if (CopyFileA(data->dataFile->memoFile.file.name, fullNameBuff, FALSE) == 0)
                                                {
                                                        return error4describe(cb, e4create, E94111, fullNameBuff, 0, 0);
                                                }
                                        }
                                #endif

                                if (includeIndex)
                                {
                                        for(indexOn = 0; ; )
                                        {
                                                indexOn = (INDEX4*)l4next(&data->indexes, indexOn);
                                                if (indexOn == 0)
                                                {
                                                        break ;
                                                }

                                                u4namePiece(fileNameBuff, LEN4PATH, indexOn->indexFile->file.name, 0, 1);
                                                u4nameCurrentExtended(fullNameBuff, LEN4PATH, fileNameBuff, destFolderBuff);
                                                if (CopyFileA(indexOn->indexFile->file.name, fullNameBuff, FALSE) == 0)
                                                {
                                                        return error4describe(cb, e4create, E94111, fullNameBuff, 0, 0);
                                                }
                                        }
                                }

                                return r4success;
                        #endif  // !S4CLIPPER
                #endif  // !S4CLIENT
        #else  // S4WIN32
                return error4(cb, e4notSupported, E94111);
        #endif
}


#if !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
   enum verify4results
   {
      inputsOk,
      inputsBad,
      inputsSkip  // CS 2000/03/29 no trailing comma
   } ;

   static verify4results d4fieldsAddVerifyInputs( DATA4 *d4, short nFields, FIELD4INFO *fieldsToAdd )
   {
      CODE4 *c4 ;
      int oldErrFieldName ;
      short i ;

      #ifdef E4PARM_LOW
         if ( d4 == 0 || (fieldsToAdd == 0 && nFields != 0 ) || nFields < 0 )
         {
            error4( 0, e4parm, E91102 ) ;
            return inputsBad ;
         }
      #endif

      if ( nFields == 0 )  /* no change */
         return inputsSkip ;

      c4 = d4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return inputsBad ;

      #ifndef S4CLIENT
         if ( d4->dataFile->file.isReadOnly == 1 )
         {
            error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;
            return inputsBad ;
         }

         #ifndef S4OFF_MULTI
            if ( d4->dataFile->file.lowAccessMode != OPEN4DENY_RW )
            {
               error4describe( c4, e4write, E81306, d4alias( d4 ), 0, 0 ) ;
               return inputsBad ;
            }
         #endif
      #endif

      oldErrFieldName = c4->errFieldName ;
      c4->errFieldName = 0 ;
      for ( i = 0 ; i < nFields ; i++ )
         if ( d4field( d4, fieldsToAdd[i].name ) != 0 ) /* duplicate field */
         {
            c4->errFieldName = oldErrFieldName ;
            error4( c4, e4fieldName, E91102 ) ;
            return inputsBad ;
         }
         else
            error4set( c4, 0 ) ;

      c4->errFieldName = oldErrFieldName ;

      #ifdef S4CLIENT
         if ( c4->defaultServer.connected == 0 )
         {
            error4( c4, e4connection, E81303 ) ;
            return inputsBad ;
         }
      #endif

      return inputsOk ;
   }



   // AS May 24/02 - created file4createLow for internal use to indicate file types
   static int createReplacementFile( CODE4 *c4, FILE4 *newFile, const char *oldPathName, char fileType )
   {
      char buf[258] ;
      #ifdef S4PALM
         UInt32 t;
      #else
         time_t t ;
      #endif
      int i, rc, saveFlag, safety, pos ;
      #ifdef S4MACINTOSH
         long macDirTemp ;
         int macVolTemp ;
      #endif

      saveFlag = c4getErrCreate( c4 ) ;
      safety = c4->safety ;
      c4setErrCreate( c4, 0 ) ;
      c4->safety = 1 ;

      /* use the d4 path to ensure the file gets created in the correct location */
      #ifdef S4MACINTOSH
         pos = 0 ;
         /* LY 2002/08/11 : newFile->macSpec.parID/vRefNum not initialized to valid settings
         macDirTemp = c4->macDir ;
         macVolTemp = c4->macVol ;
         c4->macDir = newFile->macSpec.parID ;
         c4->macVol = newFile->macSpec.vRefNum ;*/
      #else
         pos = u4namePath( buf, sizeof( buf ) - 14, oldPathName ) ;
      #endif
      c4strcpy( buf + pos, sizeof( buf ) - pos, "TEMP" ) ;  // AS Dec 13/05 vs 5.0 fixes
      for ( i = 0 ;; )
      {
         if ( i >= 100 )
         {
            rc = error4( c4, e4create, E80605 ) ;
            break ;
         }
         i++ ;

         #if defined(S4WINCE) || defined(S4WIN32)
            SYSTEMTIME st ;
            GetLocalTime(&st) ;
            t = st.wMilliseconds ;
         #elif defined(S4PALM)
            t = TimGetSeconds();
         #else
            time( &t ) ;
         #endif
         t %= 10000L ;

         c4ltoa45( (long)t + i, buf + pos + 4, -4 ) ;
         c4strcpy( buf + pos + 8, sizeof( buf ) - pos - 8, ".tmp" ) ;  // AS Dec 13/05 vs 5.0 fixes
         // AS May 24/02 - created file4createInternal for internal use to indicate file types
         rc = file4createInternal( newFile, c4, buf, 1, fileType ) ;
         if ( rc <= 0 )
            break ;
   //      u4delayHundredth( 50 ) ;
      }
      #ifdef S4MACINTOSH
      /* LY 2002/08/11
         c4->macDir = macDirTemp ;
         c4->macVol = macVolTemp ;*/
      #endif

      c4setErrCreate( c4, saveFlag ) ;
      c4->safety = safety ;

      return rc ;
   }

   static int d4copyFields(DATA4 *dataTo, DATA4 *dataFrom)
   {
      // CS 2007/06/05 Copy each field that exists in one table to the other.

      TAG4 *save_selTag;
      FIELD4 *fieldListTo[2047], *fieldListFrom[2047];
      FIELD4 *fieldTo, *fieldFrom;
      int save_errFieldName;
      int i, j, rc, rcAppend = r4success;

      memset(fieldListTo, 0, sizeof(fieldListTo));
      memset(fieldListFrom, 0, sizeof(fieldListFrom));

      save_errFieldName = dataFrom->codeBase->errFieldName;
      dataFrom->codeBase->errFieldName = 0;
      // Iterate through every field in the "to" table. If a field by the same
      // name exists in the "from" table, capture both FIELD4 pointers.
      for (i = 0, j = 1; j <= d4numFields(dataTo); j++)
      {
         fieldTo = d4fieldJ(dataTo, j);
         fieldFrom = d4field(dataFrom, f4name(fieldTo));
         if (fieldFrom != 0)
         {
            // The field exists in both tables.
            fieldListTo[i] = fieldTo;
            fieldListFrom[i] = fieldFrom;
            i++;
         }
      }
      dataFrom->codeBase->errFieldName = save_errFieldName;

      save_selTag = d4tagSelected(dataFrom);
      d4tagSelect(dataFrom, 0);  // select natural order

      // Iterate through all records in the "from" table and transfer to "to".
      for (rc = d4top(dataFrom); rc == r4success; rc = d4skip(dataFrom, 1L))
      {
         d4appendStart(dataTo, 0);
         d4blank(dataTo);

         // Iterate through all common fields.
         for (i = 0; fieldListTo[i] != 0 && fieldListFrom[i] != 0; i++)
         {
            switch (fieldListTo[i]->type)
            {
               case r4memo:
               case r4gen:
               case r4memoBin:
                  f4memoAssignN(fieldListTo[i], f4memoPtr(fieldListFrom[i]), f4memoLen(fieldListFrom[i]));
                  break;
               default:
                  f4assignField(fieldListTo[i], fieldListFrom[i]);
            }
         }

         if (d4deletedInternal(dataFrom))
         {
            d4delete(dataTo);
         }
         else
         {
            d4recall(dataFrom);
         }

         rcAppend = d4append(dataTo);
         if (rcAppend != r4success)
         {
            break;
         }
      }

      d4tagSelect(dataFrom, save_selTag);  // restore tag

      return rcAppend;
   }


   /* copies the first nFieldsMatch fields from d2 to d1, assumes fields match
      if nFieldsMatch == -1, then it uses the DATA4 info to find mathching fields */
   static int d4copy( DATA4 *d1, DATA4 *d2, short nFieldsMatch )
   {
      CODE4 *c4 ;
      int areMemosOrNulls = 0 ;  // AS 05/10/01 - was not initialized properly
      short numFields, i ;
      FIELD4 *f1, *f2 ;

      c4 = d1->codeBase ;

      if ( nFieldsMatch == -1 )
         numFields = d4numFields( d1 ) ;
      else
      {
         numFields = d4numFields( d2 ) ;
         #ifndef S4OFF_MEMO
            // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
            //    if ( d2->dataFile->memoFile.file.hand == NULL ) /* Used to be 0 ? */
            // #else
               if ( d2->dataFile->memoFile.file.hand == INVALID4HANDLE ) /* Used to be 0 ? */
            // #endif
         {
         #endif
            #ifdef S4FOX
               if ( d4compatibility( d2 ) == 30 )  /* maybe null fields */
               {
                  for ( i = 0 ; i < numFields ; i++ )
                     if ( d4fieldJ( d2, i+1 )->null == 1 )  /* has a null field */
                     {
                        #ifndef S4OFF_MEMO
                           switch( f4typeInternal( d4fieldJ( d2, i+1 ) ) )  /* if memo-type then don't count */
                           {
                              case r4memo:
                              case r4memoBin:
                              case r4gen:
                                 continue ;
                           }
                        #endif
                        areMemosOrNulls = 1 ;
                        break ;
                     }
               }
            #endif
         #ifndef S4OFF_MEMO
         }
         else
            areMemosOrNulls = 1 ;
         #endif
      }

      d4top( d2 ) ;

      // AS Apr 9/02 - Avoid overwriting contents of autoIncrement and autoTimestamp fields
      // do by temporarily removing the field marker
      #ifdef S4FOX
         FIELD4 *autoInc = d1->autoIncrementField ;
         FIELD4 *autoTimestamp = d1->autoTimestampField ;
         d1->autoIncrementField = 0 ;
         d1->autoTimestampField = 0 ;
      #endif

      while ( !d4eof( d2 ) && ( error4code( c4 ) == 0 ) )
      {
         d4appendStart( d1, 0L ) ;
         /* not just good enough to memcpy fields if there are memos or null fields */
         if ( nFieldsMatch == -1 || (areMemosOrNulls == 1) )
         {
            // AS Mar 9/06 - Need to also copy the deleted flag over
            if ( d4deletedInternal( d2 ) )
               d4delete( d1 ) ;
            else
               d4recall( d1 ) ;
            for ( i = 1 ; i <= numFields ; i++ )
            {
               f1 = d4fieldJ( d1, i ) ;
               f2 = d4field( d2, f4name( f1 ) ) ;
               #ifndef S4OFF_MEMO
                  f4memoAssignField( f1, f2 ) ;
               #else
                  f4assignField( f1, f2 ) ;
               #endif
            }
         }
         else
         {
            // AS May 29/02 - changed d4recWidth to be an exposed only function due to record length changes with aes encryption
            c4memcpy( d1->record, d2->record, dfile4recWidth( d2->dataFile ) ) ;
         }
         d4append( d1 ) ;
         if ( d4skip( d2, 1L ) != 0 )
            break ;
      }

      #ifdef S4FOX
         d1->autoIncrementField = autoInc ;
         d1->autoTimestampField = autoTimestamp ;
      #endif

      if ( error4code( c4 ) != 0 )
         return -1 ;

      return 0 ;
   }

   /* if doIndexes is false, it means was add field, which did not require
      indexes to be changed, so their file handles are still good. */
   void S4FUNCTION d4replace( DATA4 *keep, DATA4 *newFiles, int doIndexes )
   {
      FILE4LONG pos ;
      #ifndef S4OFF_INDEX
         INDEX4 *idx ;
         #ifdef S4CLIPPER
            TAG4FILE *t4file ;
         #else
            INDEX4FILE *idxFile ;
         #endif
      #endif
      #ifndef S4OFF_MEMO
         char buf[258] ;
         char doRename = 0, nmBuf1[258], nmBuf2[258] ;
      #endif

      assert5( newFiles->dataFile->userCount == 1 ) ;  // cannot replace if multiple opens because we remove the file but then can't close it, causing problems

      #ifndef S4SINGLE
         /* ensure unlock done so partialy closed file does not attempt unlocks */
         // AS Apr 15/03 - support for new lockId for shared clone locking
         d4unlockLow( keep, data4lockId( keep ), 0 ) ;
         d4unlockLow( newFiles, data4lockId( newFiles ), 0 ) ;
      #endif  /* S4SINGLE */

      #ifdef S4CLIPPER
         if ( l4numNodes( &keep->dataFile->tagfiles ) != 0 ) /* has indexes */
      #else
         if ( l4numNodes( &keep->dataFile->indexes ) != 0 ) /* has indexes */
      #endif
      {
         newFiles->dataFile->hasMdxMemo |= 0x01 ;
         /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
         file4longAssign( pos, 28, 0L ) ;
         file4writeInternal( &newFiles->dataFile->file, pos, &newFiles->dataFile->hasMdxMemo, 1 ) ;
      }

      /* first we need to flush the new file to disk, and update its header to ensure all is ok */
      #ifndef S4OFF_WRITE
         // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
         if ( newFiles->dataFile->fileChanged && file4getTemporary( &newFiles->dataFile->file ) != 1 && newFiles->dataFile->file.isReadOnly == 0 )
         {
            u4yymmdd( &newFiles->dataFile->yy ) ;
            #ifdef S4OFF_MULTI
               dfile4updateHeader( newFiles->dataFile, 1, 1, 1 ) ;
            #else
               if ( newFiles->dataFile->file.lowAccessMode == OPEN4DENY_RW )
                  dfile4updateHeader( newFiles->dataFile, 1, 1, 1 ) ;
               else /* only date stamp required */
                  dfile4updateHeader( newFiles->dataFile, 1, 0, 1 ) ;
            #endif
         }
         d4flush( newFiles ) ;
      #endif

      file4replace( &keep->dataFile->file, &newFiles->dataFile->file ) ;
      #ifndef S4OFF_INDEX
         if ( doIndexes == 1 )
         {
            /* all indexes in keep are migrated over to newFiles.  It is assumed that newFiles indexes
               if existant deleted/closed those matching held in keep. */
            while ( (idx = (INDEX4 *)newFiles->indexes.lastNode) != 0 )
            {
               l4remove( &newFiles->indexes, idx ) ;
               l4add( &keep->indexes, idx ) ;
               idx->data = keep ;
               #ifndef S4CLIPPER
                  idxFile = idx->indexFile ;
                  l4remove( &newFiles->dataFile->indexes, idxFile ) ;
                  l4add( &keep->dataFile->indexes, idxFile ) ;
                  idxFile->dataFile = keep->dataFile ;
               #endif
            }
            #ifdef S4CLIPPER
               while ( (t4file = (TAG4FILE *)l4pop( &newFiles->dataFile->tagfiles)) != 0 )
                  l4add( &keep->dataFile->tagfiles, t4file ) ;
            #endif
         }
      #endif /*S4OFF_INDEX */
      #ifndef S4OFF_MEMO
         // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
         //    if ( newFiles->dataFile->memoFile.file.hand != NULL )
         //    {
         //       if ( keep->dataFile->memoFile.file.hand != NULL )
         // #else
            if ( newFiles->dataFile->memoFile.file.hand != INVALID4HANDLE )
            {
               if ( keep->dataFile->memoFile.file.hand != INVALID4HANDLE )
         // #endif
               file4replace( &keep->dataFile->memoFile.file, &newFiles->dataFile->memoFile.file ) ;
            else  /* new memo file, must have added a memo field, just rename */
            {
               doRename = 1 ;
               u4nameCurrent( nmBuf1, sizeof( nmBuf1 ), newFiles->dataFile->memoFile.file.name ) ;
               c4strcpy( nmBuf2, sizeof( nmBuf2 ), keep->dataFile->file.name ) ;  // AS Dec 13/05 vs 5.0 fixes
               u4nameExt( nmBuf2, sizeof(nmBuf2), MEMO4EXT, 1 ) ;
            }
         }
         else /* delete the old one if there was one */
            // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
            //    if ( keep->dataFile->memoFile.file.hand != NULL )
            // #else
               if ( keep->dataFile->memoFile.file.hand != INVALID4HANDLE )
            // #endif
            {
               u4nameCurrent( buf, sizeof( buf ), keep->dataFile->memoFile.file.name ) ;
               file4close( &keep->dataFile->memoFile.file ) ;
               u4remove( buf ) ;
            }
      #endif
      /* at this point, the newFiles structure contains non-valid file handles,
         but all else must be freed up */

      // AS 05/05/99 - server, esp.  must mark file as not valid or it does not get closed at the low
      // level.  This causes memory corruption becuase the actual file is physically invalid...
      newFiles->dataFile->valid = 0 ;
      d4close( newFiles ) ;
      #ifndef S4OFF_MEMO
         if ( doRename == 1 )
            u4rename( nmBuf1, nmBuf2 ) ;
      #endif
   }



   /* creates an empty file and closes it, placing the file name in the output. */
   // AS May 24/02 - created file4createInternal for internal use to indicate file types
   // AS Dec 13/05 vs 5.0 fixes
   #define BUF4NAME_LEN 258
   static int createAndCloseEmptyFile( CODE4 *c4, const char *nameOfFileInSamePath, char *outFileName, char fileType )
   {
      FILE4 newFile ;

      int rc = createReplacementFile( c4, &newFile, nameOfFileInSamePath, fileType ) ;
      if ( rc != 0 )
         return rc ;

      c4strcpy( outFileName, BUF4NAME_LEN, newFile.name ) ;  // AS Dec 13/05 vs 5.0 fixes
      file4close( &newFile ) ;
      return 0 ;
   }



   static FIELD4INFO *getField4infoWithRoom( DATA4 *d4, int numExtraFields, FIELD4INFO *fieldsToAdd )
   {
      /* allocates a FIELD4INFO structure large enough to contain all fields in the DATA4 plus
         the input 'numExtraFields' as well as a null-terminating entry.

         copies the contents of the current DATA4 into the first part of the FIELD4INFO.

         copies the input extra fields into the latter part of the FIELD4INFO.
      */

      FIELD4INFO *fields, *cFields ;
      CODE4 *c4 ;

      c4 = d4->codeBase ;

      fields = (FIELD4INFO *)u4allocFree( c4, sizeof( FIELD4INFO ) * ( d4numFields( d4 ) + numExtraFields + 1 ) ) ;
      if ( fields == 0 )
      {
         error4( c4, e4memory, E91102 ) ;
         return 0 ;
      }

      cFields = d4fieldInfo( d4 ) ;
      c4memcpy( fields, cFields, sizeof( FIELD4INFO ) * d4numFields( d4 ) ) ;
      u4free( cFields ) ;
      c4memcpy( fields + d4numFields( d4 ), fieldsToAdd, sizeof( FIELD4INFO ) * numExtraFields ) ;
      c4memset( fields + d4numFields( d4 ) + numExtraFields, 0, sizeof( FIELD4INFO ) ) ;

      return fields ;
   }



   static DATA4 *d4fieldsAddCreate( DATA4 *d4, char *fileName, const FIELD4INFO *fieldInfo )
   {
      /* creates the 'new' data file */

      CODE4 *c4 = d4->codeBase ;
      #ifdef S4FOX
         assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
         int oldCompatibility = c4->compatibility ;
         c4->compatibility = d4->dataFile->compatibility ;
         assert5( (d4version( d4 ) == 0x30 && c4->compatibility == 30 ) || d4version( d4 ) != 30 ) ;
         assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
      #endif
      int oldSafety = c4->safety ;   /* we need to overwrite our just-created file... */
      c4->safety = 0 ;
      #ifndef S4OFF_MULTI
         int oldAccessMode = c4->accessMode ;
         /* re-create using same access mode as file */
         c4->accessMode = (char)d4->dataFile->file.lowAccessMode ;
      #endif
      DATA4 *newData = d4create( c4, fileName, fieldInfo, 0 ) ;
      #ifndef S4OFF_MULTI
         c4->accessMode = oldAccessMode ;
      #endif
      c4->safety = oldSafety ;
      #ifdef S4FOX
         c4->compatibility = oldCompatibility ;
      #endif

      return newData ;
   }



   /* assumes file open exclusively, otherwise failures with outside apps may occur */
   DATA4 *S4FUNCTION d4fieldsAdd( DATA4 *d4, short nFields, FIELD4INFO *fieldsToAdd )
   {
      switch( d4fieldsAddVerifyInputs( d4, nFields, fieldsToAdd ) )
      {
         case inputsOk:
            break ;
         case inputsBad:
            return 0 ;
         case inputsSkip:
            return d4 ;
      }

      CODE4 *c4 = d4->codeBase ;

      // AS 11/29/00 don't allow adding a field within a transaction...
      #ifndef S4OFF_TRAN
         #ifdef S4SERVER
            // AS 02/20/01 - Also don't allow if a backup log file is being maintained because we cannot currently
            // handle that situation...
            if ( c4->server->doAutoRecovery == 1 )
            {
               error4( c4, e4transViolation, E86404 ) ;
               return 0 ;
            }
         #endif

         if ( d4transEnabled( d4, 0 ) )   // always log if data logging is on, not just if 'active'
            if ( tran4active( d4->codeBase, d4 ) != 0 )
            {
               error4( c4, e4transViolation, E86401 ) ;
               return 0 ;
            }
      #endif
      #ifndef S4OFF_MULTI
         int oldAccessMode = c4->accessMode ;
         int useAccessMode = d4->dataFile->file.lowAccessMode ;  // use the accessmode specified in current DATA4
      #endif

      #ifdef S4CLIPPER
         /* if there are not tags, don't auto-open, else we must auto-open in order that the index
            tags get opened as expected.  Note that this routine results in non-production indexes
            being closed (i.e. not-re-opened after the re-create) */
         int needsAutoOpen ;
         if ( d4tagDefault( d4 ) == 0 )
            needsAutoOpen = 0 ;
         else
            needsAutoOpen = 1 ;
      #endif

      /* we really just want to guarantee a file name in the current directory (create then close it) */
      char buf[BUF4NAME_LEN] ;
      // AS May 24/02 - created file4createInternal for internal use to indicate file types
      if ( createAndCloseEmptyFile( c4, d4->dataFile->file.name, buf, d4->dataFile->file.type ) != 0 )
         return 0 ;

      assert5( strlen( buf ) < sizeof( buf ) ) ;  /* just a memory-overwrite verification after the fact */

      /* get a FIELD4INFO structure filled with info from current DATA4 and the fields we want to add */
      FIELD4INFO *fields = getField4infoWithRoom( d4, nFields, fieldsToAdd ) ;
      if ( fields == 0 )
         return 0 ;

      // AS 02/02/01 - We want to disable transaction logging of create at this point (we do it later)
      #ifndef S4OFF_TRAN
         // if the data was previously logged, then log it after modification, else don't
         Bool5 doLogOpen = d4->openWasLogged ;
         int oldStatus = code4tranStatus( c4 ) ;
         code4tranStatusSet( c4, r4off ) ;
      #endif
      // AS Aug 9/04 - temporarily disable the validation table because we are just modifying the file...
      #ifndef S4OFF_INDEX
         DATA4 *validSave = c4->validationTable ;
         c4->validationTable = 0 ;
      #endif
      DATA4 *newData = d4fieldsAddCreate( d4, buf, fields ) ;
      #ifndef S4OFF_INDEX
         c4->validationTable = validSave ;
      #endif
      #ifndef S4OFF_TRAN
         code4tranStatusSet( c4, oldStatus ) ;
      #endif
      if ( newData == 0 )
      {
         u4free( fields ) ;
         return 0 ;
      }

      if ( d4copy( newData, d4, d4numFields( d4 ) ) != 0 )
      {
         u4free( fields ) ;
         d4close( newData ) ;
         return 0 ;
      }

      /* delete now because esp. in .NTX index files must be replaced
         cannot delete because tag info goes bad; for clip, maybe just close everyone else */

      /* now rename the data, memo, and index files */
      // AS Apr 9/03 - There was an issue here that the headerLen was from the old data
      // file was overwriting the new value, causing data file corruption
      // AS May 20/03 - compile warning fix, made unsigned short from long
      unsigned short headerLen = newData->dataFile->headerLen ;
      d4replace( d4, newData, 0 ) ;
      d4->dataFile->headerLen = headerLen ;

      u4nameCurrent( buf, sizeof( buf ), d4->dataFile->file.name ) ;

      // AS 02/02/01 - Now log the create message (before the open)
      // AS log before closes becuase field info names are corrupt after
      #ifndef S4OFF_TRAN
         oldStatus = code4tranStatus( c4 ) ;
         if ( doLogOpen == 1 )
         {
            TAG4INFO *tags = 0 ;
            if ( d4->indexes.lastNode != 0 )
            {
               INDEX4 *index = (INDEX4 *)( d4->indexes.lastNode ) ;
               tags = i4tagInfo( index ) ;
            }
            d4logClose( d4 ) ;
            d4logCreate( c4, buf, fields, tags ) ;
            u4free( tags ) ;
         }
         else // turn logging off for open
            code4tranStatusSet( c4, r4off ) ;
      #endif

      // AS 02/05/01 - fields are invalid after close (names lost), so free them now
      u4free( fields ) ;
      fields = 0 ;
      // AS 01/24/00 -- in server case, must ensure that file gets physically closed.  Otherwise
      // if keepOpen is true, the old data structures (without the new fields) are left in place,
      // so they are not available...
      #ifdef S4SERVER
         int oldKeepOpen = c4->server->keepOpen ;
         c4->server->keepOpen = 0 ;
      #endif
      d4close( d4 ) ;
      #ifdef S4SERVER
         c4->server->keepOpen = oldKeepOpen ;
      #endif

      #ifdef S4CLIPPER
         int oldAutoOpen = c4->autoOpen ;
         c4->autoOpen = needsAutoOpen ;
      #endif

      #ifdef S4SERVER
         int oldSingleClient = c4->singleClient ;
         c4->singleClient = OPEN4DENY_RW ;
      #endif
      #ifndef S4OFF_MULTI
         c4->accessMode = useAccessMode ;
      #endif

      newData = d4open( c4, buf ) ;

      #ifndef S4OFF_TRAN
         code4tranStatusSet( c4, oldStatus ) ;
      #endif

      #ifndef S4OFF_MULTI
         c4->accessMode = oldAccessMode ;
      #endif
      #ifdef S4SERVER
         c4->singleClient = oldSingleClient ;
      #endif

      #ifdef S4CLIPPER
         c4->autoOpen = oldAutoOpen ;
      #endif

      return newData ;
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */


#ifndef S4OFF_WRITE
   /* assumes file open exclusively, otherwise failures with outside apps may occur */
   DATA4 *S4FUNCTION d4modifyStructure( DATA4 *d4, const FIELD4INFO *fields, const TAG4INFO *tags )
   {
      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
         {
            error4( 0, e4parmNull, E94111 ) ;
            return 0 ;
         }
      #endif
      // CS 2007/06/06
      CODE4 *c4 = d4->codeBase ;
      if ( error4code( c4 ) < 0 )
         return 0 ;

      #ifdef S4CLIENT
         CONNECTION4 *connection = &c4->defaultServer ;
         CONNECTION4MODIFY_INFO_IN *dataIn ;
         char nameBuf[258] ;
         c4strcpy( nameBuf, sizeof( nameBuf), dfile4name( d4->dataFile ) ) ;

         if ( connection == 0 )
         {
            error4( c4, e4connection, E81303 ) ;
            return 0 ;
         }

         if ( tags != 0 )
         {
            for( int i = 0 ; tags[i].name != 0; i++ )
            {
               if ( tags[i].descending )
                  if ( tags[i].descending != r4descending )
                  {
                     error4describe( c4, e4tagInfo, E85302, tags[i].name, 0, 0 ) ;
                     return 0 ;
                  }
            }
         }

         connection4assign( connection, CON4MODIFY, data4clientId( d4 ), data4serverId( d4 ) ) ;
         connection4addData( connection, NULL, sizeof(CONNECTION4MODIFY_INFO_IN), (void **)&dataIn ) ;

         unsigned short numTags ;
         if ( tags == 0 )
            numTags = 0 ;
         else
         {
            for( numTags = 0 ; tags[numTags].name != 0; numTags++ )
            {
               ;
            }
         }

         dataIn->numTags = htons5(numTags) ;

         int i;  // CS 2007/10/12 Declare outside of for loop.
         for( i = 0 ; fields[i].name != 0; i++ )
         {
            ;
         }

         short numFields = i ;
         dataIn->numFields = htons5( numFields ) ;

         short len, offset ;
         offset = sizeof( CONNECTION4MODIFY_INFO_IN ) ;
         len = 0 ;

         for ( short fieldIndex = 0 ; fieldIndex != numFields ; fieldIndex++ )
         {
            len += sizeof( CONNECTION4FIELD_INFO ) ;
            len += strlen( fields[fieldIndex].name ) + 1 ;
         }

         dataIn->fieldInfoLen = htons5(len) ;

         for ( short fieldAssignIndex = 0 ; fieldAssignIndex != numFields ; fieldAssignIndex++ )
         {
            CONNECTION4FIELD_INFO *finfo ;
            len = strlen( fields[fieldAssignIndex].name ) + 1 ;
            connection4addData( connection, NULL, sizeof(CONNECTION4FIELD_INFO), (void **)&finfo ) ;
            finfo->name.offset = htons5((short)(offset + (short)sizeof(CONNECTION4FIELD_INFO))) ;
            finfo->type = htons5(fields[fieldAssignIndex].type) ;
            finfo->len = htons5(fields[fieldAssignIndex].len) ;
            finfo->dec = htons5(fields[fieldAssignIndex].dec) ;
            finfo->nulls = htons5(fields[fieldAssignIndex].nulls) ;
            connection4addData( connection, fields[fieldAssignIndex].name, len, NULL ) ;
            offset += ( len + sizeof( CONNECTION4FIELD_INFO ) ) ;
         }

         for ( unsigned int tagIndex = 0 ; tagIndex != numTags ; tagIndex++ )
         {
            len = strlen( tags[tagIndex].name ) + 1 ;
            offset += sizeof( CONNECTION4TAG_INFO ) ;
            CONNECTION4TAG_INFO *tinfo ;
            connection4addData( connection, NULL, sizeof(CONNECTION4TAG_INFO), (void **)&tinfo ) ;
            tinfo->name.offset = htons5(offset) ;
            unsigned int len2 = 0 ;
            offset += len ;
            // AS Jan 7/05 - if there is no expression (e.g. DEL4REUSE), we were not properly setting the expression offset to 0,
            // causing some possible corruption in the expression name.
            if ( tags[tagIndex].expression )
            {
               len2 = strlen( tags[tagIndex].expression ) + 1 ;
               tinfo->expression.offset = htons5(offset) ;
            }
            else
            {
               len2 = 0 ;
               tinfo->expression.offset = 0 ;
            }
            offset += len2 ;
            unsigned int len3 ;
            if ( tags[tagIndex].filter == 0 )
            {
               len3 = 0 ;
               tinfo->filter.offset = 0 ;
            }
            else
            {
               len3 = strlen( tags[tagIndex].filter ) + 1 ;
               tinfo->filter.offset = htons5(offset) ;
            }
            offset += len3 ;
            tinfo->unique = htons5(tags[tagIndex].unique) ;
            tinfo->descending = htons5(tags[tagIndex].descending) ;
            connection4addData( connection, tags[tagIndex].name, len, NULL ) ;
            if ( len2 != 0 )
               connection4addData( connection, tags[tagIndex].expression, len2, NULL ) ;
            if ( len3 != 0 )
               connection4addData( connection, tags[tagIndex].filter, len3, NULL ) ;
         }

         connection4sendMessage( connection ) ;
         int rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return 0 ;
         if ( connection4type( connection ) != CON4MODIFY )
            return 0 ;

         rc = connection4status( connection ) ;
         if ( rc != 0 )
         {
            connection4error( connection, c4, rc, E94111 ) ;
            return 0 ;
         }

         // now we need to open the table...
         // the table will have been opened exclusively originally...
         d4close ( d4 ) ;  // close the old info out...
         int oldAccessMode = c4->accessMode ;
         c4->accessMode = OPEN4DENY_RW ;
         DATA4 *dataOut = d4open( c4, nameBuf ) ;
         c4->accessMode = oldAccessMode ;
         return dataOut ;
      #else
         FIELD4 *field;
         int save_errFieldName;
         int i;
         // don't allow modifying within a transaction
         #ifndef S4OFF_TRAN
            #ifdef S4SERVER
               // AS 02/20/01 - Also don't allow if a backup log file is being maintained because we cannot currently
               // handle that situation...
               if ( c4->server->doAutoRecovery == 1 )
               {
                  error4( c4, e4transViolation, E86404 ) ;
                  return 0 ;
               }
            #endif

            if ( d4transEnabled( d4, 0 ) )   // always log if data logging is on, not just if 'active'
               if ( tran4active( d4->codeBase, d4 ) != 0 )
               {
                  error4( c4, e4transViolation, E86401 ) ;
                  return 0 ;
               }
         #endif

         #ifndef S4OFF_MULTI
            int oldAccessMode = c4->accessMode ;
            int useAccessMode = d4->dataFile->file.lowAccessMode ;  // use the accessmode specified in current DATA4
         #endif

         save_errFieldName = c4->errFieldName;
         c4->errFieldName = 0;
         // Skip through every field and make sure if there are any with the
         // same name in both the old and new tables, they have the same type.
         for (i = 0; fields[i].name != 0; i++)
         {
            field = d4field(d4, fields[i].name);
            if (field != 0)
            {
               // field exists in source; make sure they are same.
               if (field->type != fields[i].type)
               {
                  c4->errFieldName = save_errFieldName;
                  error4describe(c4, e4fieldType, 00000, "Fields of same name must be the same type.", fields[i].name, 0);
                  return 0;
               }
            }
         }
         c4->errFieldName = save_errFieldName;

         #ifdef S4CLIPPER
            /* if there are not tags, don't auto-open, else we must auto-open in order that the index
               tags get opened as expected.  Note that this routine results in non-production indexes
               being closed (i.e. not-re-opened after the re-create) */
            int needsAutoOpen ;
            if ( d4tagDefault( d4 ) == 0 )
               needsAutoOpen = 0 ;
            else
               needsAutoOpen = 1 ;
         #endif

         /* we really just want to guarantee a file name in the current directory (create then close it) */
         char buf[BUF4NAME_LEN] ;
         // AS May 24/02 - created file4createInternal for internal use to indicate file types
         if ( createAndCloseEmptyFile( c4, d4->dataFile->file.name, buf, d4->dataFile->file.type ) != 0 )
            return 0 ;

         assert5( strlen( buf ) < sizeof( buf ) ) ;  /* just a memory-overwrite verification after the fact */

         // AS 02/02/01 - We want to disable transaction logging of create at this point (we do it later)
         #ifndef S4OFF_TRAN
            // if the data was previously logged, then log it after modification, else don't
            Bool5 doLogOpen = d4->openWasLogged ;
            int oldStatus = code4tranStatus( c4 ) ;
            code4tranStatusSet( c4, r4off ) ;
         #endif
         // AS Aug 9/04 - temporarily disable the validation table because we are just modifying the file...
         #ifndef S4OFF_INDEX
            DATA4 *validSave = c4->validationTable ;
            c4->validationTable = 0 ;
         #endif
         DATA4 *newData = d4fieldsAddCreate( d4, buf, fields ) ;
         #ifndef S4OFF_INDEX
            c4->validationTable = validSave ;
         #endif
         #ifndef S4OFF_TRAN
            code4tranStatusSet( c4, oldStatus ) ;
         #endif
         if ( newData == 0 )
         {
            return 0 ;
         }

         if (d4copyFields(newData, d4) != 0)
         {
            d4close( newData ) ;
            return 0 ;
         }

         /* delete now because esp. in .NTX index files must be replaced
            cannot delete because tag info goes bad; for clip, maybe just close everyone else */

         /* now rename the data, memo, and index files */
         // AS Apr 9/03 - There was an issue here that the headerLen was from the old data
         // file was overwriting the new value, causing data file corruption
         // AS May 20/03 - compile warning fix, made unsigned short from long
         unsigned short headerLen = newData->dataFile->headerLen ;
         d4replace( d4, newData, 0 ) ;
         d4->dataFile->headerLen = headerLen ;

         u4nameCurrent( buf, sizeof( buf ), d4->dataFile->file.name ) ;

         // AS 02/02/01 - Now log the create message (before the open)
         // AS log before closes becuase field info names are corrupt after
         #ifndef S4OFF_TRAN
            oldStatus = code4tranStatus( c4 ) ;
            if ( doLogOpen == 1 )
            {
               TAG4INFO *tags = 0 ;
               if ( d4->indexes.lastNode != 0 )
               {
                  INDEX4 *index = (INDEX4 *)( d4->indexes.lastNode ) ;
                  tags = i4tagInfo( index ) ;
               }
               d4logClose( d4 ) ;
               d4logCreate( c4, buf, fields, tags ) ;
               u4free( tags ) ;
            }
            else // turn logging off for open
               code4tranStatusSet( c4, r4off ) ;
         #endif

         // AS 01/24/00 -- in server case, must ensure that file gets physically closed.  Otherwise
         // if keepOpen is true, the old data structures (without the new fields) are left in place,
         // so they are not available...
         #ifdef S4SERVER
            int oldKeepOpen = c4->server->keepOpen ;
            c4->server->keepOpen = 0 ;
         #endif

         d4close( d4 ) ;

         #ifndef S4OFF_INDEX
            // Temporily open the table in exclusive mode to either recreate or delete the index.

            int save_safety ;
            int save_autoOpen ;

            save_safety = c4->safety ;
            c4->safety = 0 ;
            save_autoOpen = c4->autoOpen ;
            c4->autoOpen = 0 ;
            c4->accessMode = OPEN4DENY_RW ;
            #ifdef S4SERVER
               int saveSingleClient = c4->singleClient ;
               c4->singleClient = OPEN4DENY_RW ;
            #endif

            d4 = d4open( c4, buf ) ;

            #ifdef S4SERVER
               c4->singleClient = saveSingleClient ;
            #endif
            c4->autoOpen = save_autoOpen;
            c4->safety = c4->safety;

            if (d4 != 0)
            {
               if (tags != 0)
               {
                  // TAG4INFO array is available; create index.
                  i4create(d4, 0, tags);
               }
               else
               {
                  // A tag array was not passed in. Therefore
                  // the modified table should not have an index.

                  // Delete the production index file if it exists.
                  char ibuf[LEN4PATH];
                  c4strcpy(ibuf, LEN4PATH, buf);
                  u4nameExt(ibuf, LEN4PATH, code4indexExtension(c4), 1);
                  u4remove(ibuf);

                  // Remove the production flag from the DBF header.
                  #ifdef S4FOX
                     d4->dataFile->hasMdxMemo = d4->dataFile->hasMdxMemo & (~0x01) ;  // bit 1 is for index
                  #else
                     d4->dataFile->hasMdxMemo = 0 ;
                  #endif

                  FILE4LONG pos ;
                  file4longAssign( pos, ( 4 + sizeof( S4LONG ) + 2 * sizeof( short ) + sizeof( char[16] ) ), 0L ) ;
                  #ifdef S4FOX
                     file4writeInternal( &d4->dataFile->file, pos, &(d4->dataFile->hasMdxMemo), sizeof( char ) ) ;
                  #else
                     file4writeInternal( &d4->dataFile->file, pos, &(d4->dataFile->hasMdxMemo), sizeof( d4->dataFile->hasMdxMemo ) ) ;
                  #endif
                  #ifndef S4SERVER
                     d4->dataFile->openMdx = 0 ;
                  #endif
               }
               d4close( d4 ) ;
            }
         #endif

         #ifdef S4SERVER
            c4->server->keepOpen = oldKeepOpen ;
         #endif

         #ifdef S4CLIPPER
            int oldAutoOpen = c4->autoOpen ;
            c4->autoOpen = needsAutoOpen ;
         #endif

         #ifdef S4SERVER
            int oldSingleClient = c4->singleClient ;
            c4->singleClient = OPEN4DENY_RW ;
         #endif
         #ifndef S4OFF_MULTI
            c4->accessMode = useAccessMode ;
         #endif

         newData = d4open( c4, buf ) ;

         #ifndef S4OFF_TRAN
            code4tranStatusSet( c4, oldStatus ) ;
         #endif

         #ifndef S4OFF_MULTI
            c4->accessMode = oldAccessMode ;
         #endif
         #ifdef S4SERVER
            c4->singleClient = oldSingleClient ;
         #endif

         #ifdef S4CLIPPER
            c4->autoOpen = oldAutoOpen ;
         #endif

         return newData ;
      #endif
   }
#endif


#if !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
   typedef struct
   {
      int create ;     /* does it need to be created or just opened? */
      char pathName[258] ;
      const char *namePtr ;
      TAG4INFO *tags ;
   } I4INFO_EXT ;



   static void i4infoExtFree( I4INFO_EXT *info )
   {
      int i ;
      char *ptr ;

      if ( info->tags != 0 )
      {
         for ( i = 0 ;; i++ )
         {
            if ( info->tags[i].name == 0 )
               break ;
            if ( info->tags[i].name != 0 )
            {
               u4free( info->tags[i].name ) ;
               info->tags[i].name = 0 ;
            }
            if ( info->tags[i].expression != 0 )
            {
               ptr = (char *)(info->tags[i].expression) ;
               u4free( ptr ) ;
            }
            if ( info->tags[i].filter != 0 )
            {
               ptr = (char *)(info->tags[i].filter) ;
               u4free( ptr ) ;
            }
         }
         u4free( info->tags ) ;
         info->tags = 0 ;
      }
   }
   #ifndef S4OFF_INDEX
   static void i4infoExtInfo( DATA4 *data, I4INFO_EXT *info, INDEX4 *i4, TAG4INFO *tags )
   {
      int i, numTags, len ;

      #ifdef E4PARM_LOW
         if ( info == 0 || i4 == 0 )
         {
            error4( 0, e4parmNull, E91102 ) ;
            return ;
         }
      #endif

      for ( numTags = 0 ;; numTags++ )
         if ( tags[numTags].name == 0 )
            break ;

      info->tags = (TAG4INFO *)u4alloc( ( numTags + 1 ) * sizeof( TAG4INFO ) ) ;
      #ifdef S4CLIPPER
         /* clipper is always group file... (i.e. use autoOpen off to avoid) */
         info->namePtr = info->pathName ;
         u4nameCurrent( info->pathName, sizeof( info->pathName ), data->dataFile->file.name ) ;
         u4nameExt( info->pathName, sizeof( info->pathName ), GROUP4EXT, 1 ) ;
      #else
         /* always assign the pathName, even if production index file */
         u4nameCurrent( info->pathName, sizeof( info->pathName ), i4->indexFile->file.name ) ;
         if ( index4isProduction( i4->indexFile ) ) /* mark as not */
            info->namePtr = 0 ;
         else
            info->namePtr = info->pathName ;
      #endif

      for ( i = 0 ; i < numTags ; i++ )
      {
         if ( tags[i].name == 0 )
            break ;
         len = c4strlen( tags[i].name ) ;
         info->tags[i].name = (char *)u4alloc( len + 1 ) ;
         c4memcpy( info->tags[i].name, tags[i].name, len ) ;
         len = c4strlen( tags[i].expression ) ;
         info->tags[i].expression = (char *)u4alloc( len + 1 ) ;
         c4memcpy( (char *)info->tags[i].expression, tags[i].expression, len ) ;
         if ( tags[i].filter != 0 )
         {
            len = c4strlen( tags[i].filter ) ;
            if ( len != 0 )
            {
               info->tags[i].filter = (char *)u4alloc( len + 1 ) ;
               c4memcpy( (char *)info->tags[i].filter, tags[i].filter, len ) ;
            }
         }
         info->tags[i].unique = tags[i].unique ;
         info->tags[i].descending = tags[i].descending ;
      }

      return ;
   }
   #endif /*S4OFF_INDEX */
   /* removes the fields given by names
      also cleans up the tags
      also returns 0 if all fields are removed */
   DATA4 *S4FUNCTION d4fieldsRemove( DATA4 **d4, short nFields, char *names[] )
   {
      short i ;
      #ifndef S4OFF_INDEX
         int nTags, numIndexes, needRecreate, idxOn ;
         TAG4INFO *tags = 0 ;
         INDEX4 *idx = 0, *iNext = 0 ;
         EXPR4 *expr = 0 ;
      #endif
      #ifndef S4OFF_MEMO
         char hadMemo = 0, memoName[258] ;
      #endif

      #ifdef E4PARM_LOW
         if ( d4 == 0 || *d4 == 0 || (names == 0 && nFields != 0 ) || nFields < 0 )
         {
            error4( 0, e4parm, E91102 ) ;
            return 0 ;
         }
      #endif

      if ( nFields == 0 )  /* no change */
         return *d4 ;

      CODE4 *c4 = (*d4)->codeBase ;

      if ( error4code( c4 ) < 0 )
         return 0 ;

      // AS 11/29/00 don't allow adding a field within a transaction...
      #ifndef S4OFF_TRAN
         #ifdef S4SERVER
            // AS 02/20/01 - Also don't allow if a backup log file is being maintained because we cannot currently
            // handle that situation...
            if ( c4->server->doAutoRecovery == 1 )
            {
               error4( c4, e4transViolation, E86404 ) ;
               return 0 ;
            }
         #endif

         if ( d4transEnabled( (*d4), 0 ) )   // always log if data logging is on, not just if 'active'
            if ( tran4active( (*d4)->codeBase, (*d4) ) != 0 )
            {
               error4( c4, e4transViolation, E86401 ) ;
               return 0 ;
            }
      #endif

      DATA4FILE *d4file = (*d4)->dataFile ;

      #ifndef S4OFF_MEMO
         // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
         //    hadMemo = d4file->memoFile.file.hand != NULL ;
         // #else
            hadMemo = d4file->memoFile.file.hand != INVALID4HANDLE ;
         // #endif
         if ( hadMemo )
            u4nameCurrent( memoName, sizeof( memoName ), d4file->memoFile.file.name ) ;
      #endif

      if ( d4file->file.isReadOnly == 1 )
      {
         error4describe( c4, e4write, E80606, d4alias( (*d4) ), 0, 0 ) ;
         return 0 ;
      }
      #ifndef S4OFF_MULTI
         if ( d4file->file.lowAccessMode != OPEN4DENY_RW )
         {
            error4describe( c4, e4write, E81306, d4alias( (*d4) ), 0, 0 ) ;
            return 0 ;
         }
      #endif
      int oldErrFieldName = c4->errFieldName ;
      int oldErrExpr = c4->errExpr ;

      c4->errFieldName = 0 ;
      c4->errExpr = 0 ;

      int fieldCount = d4numFields( (*d4) ) ;
      FIELD4INFO *fields = d4fieldInfo( (*d4) ) ;

      #ifdef S4FOX
         assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
         int oldCompatibility = c4->compatibility ;
         c4->compatibility = (*d4)->dataFile->compatibility ;
         assert5( (d4version( (*d4) ) == 0x30 && c4->compatibility == 30 ) || d4version( (*d4) ) != 0x30 ) ;
         assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
      #endif

      DATA4 *newData = 0 ;

      for( ;; )
      {
         for ( i = 0 ; i < nFields ; i++ )
         {
            FIELD4 *field = d4field( (*d4), names[i] ) ;
            if ( field == 0 )
            {
               error4( c4, e4fieldName, E91102 ) ;
               break ;
            }

            /* now find the field within the field info and delete */
            int done = 0 ;
            for ( int j = 0 ; j < fieldCount ; j++ )
            {
               if ( c4strcmp( fields[j].name, f4name( field ) ) == 0 )
               {
                  c4memmove( &(fields[j]), &(fields[j+1]), sizeof( FIELD4INFO ) * (fieldCount - j) ) ;  /* also copy the null entry over */
                  fieldCount-- ;
                  done = 1 ;
                  break ;
               }
            }

            if ( done == 1 )
               continue ;

            /* if here, then error because field not found */
            error4( c4, e4parm, E91102 ) ;
            break ;
         }

         if ( error4code( c4 ) < 0 )
            break ;

         if ( fieldCount == 0 ) /* means we removed all fields */
         {
            d4remove( (*d4) ) ;
            *d4 = 0 ;
            break ;
         }

         /* we really just want to guarantee a file name in the current directory */
         FILE4 newFile ;
         // AS May 24/02 - created file4createInternal for internal use to indicate file types
         if ( createReplacementFile( c4, &newFile, d4file->file.name, d4file->file.type ) != 0 )
            break ;
         char buf[258] ;
         c4strcpy( buf, sizeof( buf ), newFile.name ) ;  // AS Dec 13/05 vs 5.0 fixes
         file4close( &newFile ) ;

         int oldSafety = c4->safety ;  /* we must replace our created replacement file */
         c4->safety = 0 ;
         int oldAccessMode = c4->accessMode ;
         c4->accessMode = OPEN4DENY_RW ;
         // AS Aug 9/04 - temporarily disable the validation table because we are just modifying the file...
         #ifndef S4OFF_INDEX
            DATA4 *validSave = c4->validationTable ;
            c4->validationTable = 0 ;
         #endif
         newData = d4create( c4, buf, fields, 0 ) ;
         #ifndef S4OFF_INDEX
            c4->validationTable = validSave ;
         #endif
         c4->accessMode = oldAccessMode ;
         c4->safety = oldSafety ;
         if ( newData == 0 )
            break ;

         #ifndef S4OFF_INDEX
            /* now remove any tags associated with the removed fields - just evaluate them, and if
               an error (expression error), then remove
               note that if no changes, then leave alone, else rebuild index from scratch
               with new tag info (we don't have a remove function) */
            tags = 0 ;

            #ifdef S4CLIPPER
               char txbuf[258] ;
            #endif

            numIndexes = l4numNodes( &(*d4)->indexes ) ;
            I4INFO_EXT *indexInfo = 0 ;
            if ( numIndexes != 0 )
            {
               indexInfo = (I4INFO_EXT *)u4alloc( sizeof( I4INFO_EXT ) * numIndexes ) ;

               for( idxOn = 0, iNext = (INDEX4 *)l4first( &(*d4)->indexes ) ;; idxOn++ )
               {
                  idx = iNext ;
                  if ( idx == 0 )
                     break ;
                  iNext = (INDEX4 *)l4next( &(*d4)->indexes, idx ) ;
                  tags = i4tagInfo( idx ) ;
                  if ( tags == 0 )
                     break ;
                  for ( nTags = 0 ;; nTags++ )
                     if ( tags[nTags].name == 0 )  /* done */
                        break ;
                  for ( i = 0, needRecreate = 0 ;; i++ )
                  {
                     if ( tags[i].name == 0 )  /* done */
                        break ;
                     expr = expr4parse( newData, tags[i].expression ) ;
                     if ( expr == 0 )
                     {
                        #ifdef S4CLIPPER
                           TAG4 *tag ;
                           char tbuf[258] ;
                        #endif
                        error4set( c4, 0 ) ;
                        #ifdef S4CLIPPER
                           tag = d4tag( (*d4), tags[i].name ) ;
                           if ( tag == 0 )  /* should be impossible */
                           {
                              error4( c4, e4info, E91102 ) ;
                              return 0 ;
                           }
                           u4nameCurrent( txbuf, sizeof( txbuf ), idx->accessName ) ;
                           u4nameExt( txbuf, sizeof( txbuf ), "CGP", 0 ) ;
                           u4nameCurrent( tbuf, sizeof( tbuf ), tag->tagFile->file.name ) ;
                           t4close( tag ) ;
                           u4remove( tbuf ) ;
                        #endif
                        c4memcpy( tags + i, tags + i + 1, sizeof( TAG4INFO ) * (nTags - i) ) ;  /* also copy last (null) entry */
                        i-- ;
                        nTags-- ;
                        needRecreate = 1 ;
                     }
                     else
                        expr4free( expr ) ;
                  }

                  if ( needRecreate == 1 )
                  {
                     if ( nTags == 0 )  /* means remove all tags */
                        indexInfo[idxOn].create = 2 ;
                     else
                        indexInfo[idxOn].create = 1 ;

                     i4infoExtInfo( (*d4), &indexInfo[idxOn], idx, tags ) ;

                     #ifndef S4CLIPPER
                        idx->indexFile->dataFile->hasMdxMemo &= (~1) ;
                        FILE4LONG fPos ;
                        /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
                        file4longAssign( fPos, 28, 0L ) ;
                        file4writeInternal( &idx->indexFile->dataFile->file, fPos, &idx->indexFile->dataFile->hasMdxMemo, 1 ) ;
                        idx->indexFile->dataFile->openMdx = 0 ;
                        #ifdef S4MDX
                           idx->indexFile->header.isProduction = 0 ;
                        #endif
                        i4closeLow( idx ) ;
                     #endif
                  }
                  else
                  {
                     indexInfo[idxOn].create = 0 ;
                     #ifdef S4CLIPPER
                        i4infoExtInfo( (*d4), &indexInfo[idxOn], idx, tags ) ;
                     #endif
                  }

                  if ( tags != 0 )
                  {
                     u4free( tags ) ;
                     tags = 0 ;
                  }
               }
            }
         #endif  /* S4OFF_INDEX */
         /* now copy contents over. */

         d4copy( newData, (*d4), -1 ) ;

         // AS 02/05/01 - this replace has the effect of 'closing' newData since
         // all of the relevant files, etc. get copied to (*d4)
         // therefore, send the close message to the transaction file now
         #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT )
            d4logClose( newData ) ;
         #endif /* !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE ) && !defined( S4CLIENT ) */

         d4replace( (*d4), newData, 1 ) ;

         u4nameCurrent( buf, sizeof( buf ), d4file->file.name ) ;

         // AS May 10/01 - if isChanged flag is set to true, then when we close here
         // the header for the old file overwrites the new header information, thus
         // corrupting the data file.  Try d4modify() from dot.c twice in a row on a table to
         // emulate
         (*d4)->dataFile->fileChanged = 0 ;
         d4close( (*d4) ) ;
         *d4 = 0 ;

         int oldAutoOpen = c4->autoOpen ;
         c4->autoOpen = 0 ;
         oldAccessMode = c4->accessMode ;
         c4->accessMode = OPEN4DENY_RW ;
         #ifdef S4SERVER
            int oldSingleClient = c4->singleClient ;
            c4->singleClient = OPEN4DENY_RW ;
         #endif
         newData = d4open( c4, buf ) ;
         #ifdef S4SERVER
            c4->singleClient = oldSingleClient ;
         #endif
         c4->accessMode = oldAccessMode ;
         c4->autoOpen = oldAutoOpen ;

         /* now do the indexes */
         #ifndef S4OFF_INDEX
            if ( newData != 0 )
            {
               oldSafety = c4->safety ;
               c4->safety = 0 ;
               for ( i = 0 ; i < numIndexes ; i++ )
               {
                  switch( indexInfo[i].create )
                  {
                     case 0: /* open */
                        {
                           /* open all the tags if clipper */
                           #ifdef S4CLIPPER
                              for ( short iTag = 0 ; indexInfo[i].tags[iTag].name != 0 ; iTag++)
                              {
                                 if ( t4open( newData, 0, indexInfo[i].tags[iTag].name ) == 0 )
                                 {
                                    d4close( newData ) ;
                                    return 0 ;
                                 }
                              }
                           #else
                              if ( i4open( newData, indexInfo[i].namePtr ) == 0 )
                              {
                                 d4close( newData ) ;
                                 return 0 ;
                              }
                           #endif
                        }
                        break ;
                     case 1:
                        oldAccessMode = c4->accessMode ;
                        c4->accessMode = OPEN4DENY_RW ;
                        if ( i4create( newData, indexInfo[i].namePtr, indexInfo[i].tags ) == 0 )
                        {
                           d4close( newData ) ;
                           c4->accessMode = oldAccessMode ;
                           return 0 ;
                        }
                        c4->accessMode = oldAccessMode ;
                        break ;
                     case 2:
                        #ifdef S4CLIPPER
                           u4remove( txbuf ) ;
                        #else
                           u4remove( indexInfo[i].pathName ) ;
                        #endif
                        break ;
                     default:
                        error4( c4, e4info, E91102 ) ;
                        break ;
                  }
                  i4infoExtFree( &indexInfo[i] ) ;
               }
               u4free( indexInfo ) ;
               c4->safety = oldSafety ;
            }
         #endif
         break ;
      }

      /* recover code here */

      #ifdef S4FOX
         c4->compatibility = oldCompatibility ;
      #endif
      c4->errFieldName = oldErrFieldName ;
      c4->errExpr = oldErrExpr ;
      if ( fields != 0 )
         u4free( fields ) ;
      #ifndef S4OFF_INDEX
         if ( tags != 0 )
            u4free( tags ) ;
      #endif
      #ifndef S4OFF_MEMO
         if ( hadMemo && newData != 0 ) /* maybe memo file deleted */
            // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
            //    if ( newData->dataFile->memoFile.file.hand == NULL )
            // #else
               if ( newData->dataFile->memoFile.file.hand == INVALID4HANDLE )
            // #endif
              u4remove( memoName ) ;
      #endif

      // AS 01/24/01 - make sure we don't reset d4 if an error occurred, also do some basic assertion testing

      // only one of d4 and newData should be valid - either the old one got closed and a new one created
      // or the old one was left as is and no new one was created.  - or they are both null - on removal
      assert5( ( (*d4) == 0 && ( newData != 0 ) ) || ( (*d4) != 0 && ( newData == 0 ) ) || ( ( (*d4) == 0 ) && (newData == 0 ))) ;

      if ( (*d4) == 0 )   // the old DATA4 was closed
      {
         (*d4) = newData ;  // CS 2000/01/22
      }
      return newData ;
   }
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */




//
//
// /* S4CLIENT */
// DATA4 *S4FUNCTION d4fieldsAdd( DATA4 *d4, short nFields, FIELD4INFO *fieldsToAdd )
// {
//    /* S4CLIENT version */
//
//    char nameBuf[LEN4PATH] ;
//    c4strcpy( nameBuf, dfile4name( d4->dataFile ) ) ;
//    CODE4 *c4 = d4->codeBase ;
//
//    // this process will require us to close and re-open the datafile
//    switch( d4fieldsAddVerifyInputs( d4, nFields, fieldsToAdd ) )
//    {
//       case inputsOk:
//          break ;
//       case inputsBad:
//          return 0 ;
//       case inputsSkip:
//          return d4 ;
//    }
//
//    CONNECTION4 *connection = &c4->defaultServer ;
//    CONNECTION4FIELDS_ADD_INFO_IN *dataIn ;
//    short offset = sizeof( CONNECTION4FIELDS_ADD_INFO_IN ) ;
//
//    connection4assign( connection, CON4FIELDS_ADD, data4clientId( d4 ), data4serverId( d4 ) ) ;
//    connection4addData( connection, NULL, sizeof(CONNECTION4FIELDS_ADD_INFO_IN), (void **)&dataIn ) ;
//    dataIn->numFields = htons5( nFields ) ;
//
//    for ( short j = 0 ; j != nFields ; j++ )
//    {
//       CONNECTION4FIELD_INFO *finfo ;
//       short len = strlen( fieldsToAdd[j].name ) + 1 ;
//       connection4addData( connection, NULL, sizeof(CONNECTION4FIELD_INFO), (void **)&finfo ) ;
//       finfo->name.offset = htons5((short)(offset + (short)sizeof(CONNECTION4FIELD_INFO))) ;
//       finfo->type = htons5(fieldsToAdd[j].type) ;
//       finfo->len = htons5(fieldsToAdd[j].len) ;
//       finfo->dec = htons5(fieldsToAdd[j].dec) ;
//       finfo->nulls = htons5(fieldsToAdd[j].nulls) ;
//       connection4addData( connection, fieldsToAdd[j].name, len, NULL ) ;
//       offset += ( len + sizeof( CONNECTION4FIELD_INFO ) ) ;
//    }
//
//    connection4sendMessage( connection ) ;
//    int rc = connection4receiveMessage( connection ) ;
//    if ( rc < 0 )
//    {
//       #ifdef E4STACK
//          error4stack( c4, rc, E91401 ) ;
//       #endif
//       return 0 ;
//    }
//    if ( connection4type( connection ) != CON4FIELDS_ADD )
//    {
//       #ifdef E4STACK
//          error4stack( c4, e4connection, E91401 ) ;
//       #endif
//       return 0 ;
//    }
//
//    rc = connection4status( connection ) ;
//    if ( rc != 0 )
//    {
//       if ( c4getErrCreate( c4 ) == 0 )
//          error4set( c4, r4noCreate ) ;
//       else
//          connection4errorDescribe( connection, c4, rc, E91401, nameBuf, 0, 0 ) ;
//       return 0 ;
//    }
//
//    // note that various open settings...
//    int oldAccessMode = c4->accessMode ;
//    int oldAutoOpen = c4->autoOpen ;
//
//    // must be open in exclusive mode in order to modify
//    c4->accessMode = OPEN4DENY_RW ;
//
//    // if there is an index, assume autoOpen true
//    if ( l4numNodes( &d4->indexes ) != 0 )
//       c4->autoOpen = 1 ;
//    else
//       c4->autoOpen = 0 ;
//
//    rc = d4close( d4 ) ;
//    DATA4 *dataOut = 0 ;
//    if ( rc == 0 )
//       dataOut = d4open( c4, nameBuf ) ;
//
//    c4->accessMode = oldAccessMode ;
//    c4->autoOpen = oldAutoOpen ;
//
//    return dataOut ;
// }



