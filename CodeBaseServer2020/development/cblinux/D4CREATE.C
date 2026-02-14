/* d4create.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#ifndef S4OFF_WRITE
   #ifdef S4CLIPPER
      extern unsigned short f4memoNullChar ;
   #endif

   /* !S4OFF_WRITE */
   DATA4 *S4FUNCTION d4createTemp( CODE4 *c4, const FIELD4INFO *fieldData, const TAG4INFO *tagInfo )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( c4, 1, E91401 ) )
            return 0 ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( c4 == 0 || fieldData == 0 )
         {
            error4( c4, e4parm_null, E91401 ) ;
            return 0 ;
         }
      #endif

      #ifdef S4CLIPPER
         int numTags = 0 ;
         char **ptrs = 0 ;
         if ( tagInfo != 0 )
         {
            // AS 05=6/03/98 -- problem if tag files already exist...
            // make the tags names '0' so they created as temporary, and then assign the names
            // back after

            for ( numTags = 0 ;; numTags++ )
            {
               if ( tagInfo[numTags].name == 0 )
                  break ;
            }

            /* AS 08/03/98 loop used but not initialized */
            ptrs = (char **)u4allocEr( c4, numTags * sizeof( char * ) ) ;
            if ( ptrs == 0 )
               return 0 ;

            int loop ;
            for ( loop = 0 ; tagInfo[loop].name ; loop++ )
            {
               ptrs[loop] = tagInfo[loop].name ;
               char **temp = (char **) &tagInfo[loop].name ;  /* LY 99/5/25 : convoluted compiler error fix */
               *temp = (char *)(&f4memoNullChar) ;
            }
            numTags = loop ;
         }
      #endif

      int oldTemp = c4->createTemp ;
      c4->createTemp = 1 ;
      DATA4 *data = d4create( c4, (char *)0, fieldData, tagInfo ) ;
      c4->createTemp = oldTemp ;

      #ifdef S4CLIPPER
         if ( tagInfo != 0 )
         {
            TAG4 *tag = 0 ;
            for ( int loop = 0 ; loop < numTags ; loop++ )
            {
               char **temp = (char **) &tagInfo[loop].name ;  /* LY 99/5/25 : convoluted compiler error fix */
               *temp = ptrs[loop] ;
               if ( data != 0 )
               {
                  tag = d4tagNext( data, tag ) ;
                  if ( tag == 0 )  // error
                  {
                     d4close( data ) ;
                     // AS 06/26/00 clipper memory leak fixed
                     u4free( ptrs ) ;
                     error4( c4, e4data, E91401 ) ;
                     return 0 ;
                  }
                  u4namePiece( tag->tagFile->alias, sizeof( tag->tagFile->alias ), tagInfo[loop].name, 0, 0 ) ;
               }
            }
            // AS 06/26/00 clipper memory leak fixed
            u4free( ptrs ) ;

         }
      #endif

      if ( data == 0 )
      {
         #ifdef E4STACK
            error4stack( c4, e4data, E91401 ) ;
         #endif
         return 0 ;
      }

      return data ;
   }



   #ifndef S4CLIENT
      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      /* !S4OFF_WRITE, !S4CLIENT */
      static void d4createClose( CODE4 *c4, DATA4 *d4, int doDelete )
      {
         if ( d4 == 0 )
            return ;

         if ( doDelete == 1 )
         {
            file4setTemporary( &d4->dataFile->file, 1, 0 ) ;  // it won't be registed in validation table yet

            // and delete any index files
            #ifdef S4CLIPPER
               for ( TAG4FILE *tagFile = 0 ;; )
               {
                  tagFile = (TAG4FILE *)l4next( &d4->dataFile->tagfiles, tagFile ) ;
                  if ( tagFile == 0 )
                     break ;
                  file4setTemporary( &tagFile->file, 1, 0 ) ;
               }
            #endif
         }

         #ifdef S4OFF_TRAN
            d4close( d4 ) ;
         #else
            int oldStatus = code4tranStatus( c4 ) ;
            code4tranStatusSet( c4, r4off ) ;
            d4close( d4 ) ;
            code4tranStatusSet( c4, oldStatus ) ;
         #endif
      }
   #endif // !S4CLIENT



   /* !S4OFF_WRITE */
   DATA4 *S4FUNCTION d4createLow( CODE4 *c4, const char *name, const FIELD4INFO *fieldData, const TAG4INFO *tagInfo )
   {
       #ifdef S4CLIENT
         short len, offset ;
      #else
         int oldReadOnly ;
         // AS 09/29/00 - allow the logging of create now (for backup/recovery)
         // #ifndef S4OFF_TRAN
         //    int saveStatus ;
         // #endif
      #endif
      char nameBuf[258] ;
      int rc, i, oldAutoOpen ;
      DATA4FILE *d4 = NULL ;
      DATA4 *data = NULL ;
      TAG4 *tag = NULL ;

      if ( error4code( c4 ) < 0 )
         return 0 ;

      #if defined( S4STAND_ALONE ) && !defined( S4OFF_TRAN )
         if ( c4->logOpen )   /* open now so that can turn off during create */
         {
            rc = code4logOpen( c4, 0, 0 ) ;
            if ( rc < 0 )
               return 0 ;
            else
               error4set( c4, 0 ) ;   /* remove r4open if it already existed */
         }
      #endif

      /* AS 06/04/97 ole-db allows creation of permanent files without fixed file names */
      if ( c4->createTemp == 1 || name == 0 )
      {
         #if !defined( S4OFF_CATALOG ) && !defined( CAT4CREATE )
            if ( cat4avail( c4->catalog ) == 1 )
               c4->catalog->valid = 0 ;
         #endif
      }
      else
      {
         #ifdef S4CLIENT
            d4 = dfile4data( c4, name ) ;
         #else
            u4nameCurrent( nameBuf, sizeof( nameBuf ), name ) ;
            u4nameExt( nameBuf, sizeof( nameBuf ), DBF4EXT, 0 ) ;
            d4 = dfile4data( c4, nameBuf ) ;
         #endif
         if ( d4 != 0 )
         {
            if ( c4getErrCreate( c4 ) == 0 )
               error4set( c4, r4noCreate ) ;
            else
              error4describe( c4, e4create, E81304, name, (char *)0, (char *)0 ) ;
            return 0 ;
         }
      }

      #ifdef S4CLIENT
         if ( !c4->defaultServer.connected )
         {
            rc = code4connect( c4, 0, DEF4PROCESS_ID, 0, 0, 0 ) ;
            if ( rc == 0 )
            {
               if ( !c4->defaultServer.connected )
               {
                  error4( c4, e4connection, E84302 ) ;
                  return 0 ;
               }
            }
            if ( rc != 0 )
            {
               if ( c4->defaultServer.connected )
               {
                  connection4initUndo( &c4->defaultServer ) ;
                  /* connection4free( &c4->defaultServer ) ;*/
                  c4->defaultServer.connected = 0 ;
               }
               return 0 ;
            }
         }
         CONNECTION4 *connection = &c4->defaultServer ;

         if ( connection == 0 )
         {
            error4( c4, e4connection, E81303 ) ;
            return 0 ;
         }

         connection4assign( connection, CON4CREATE, 0, 0 ) ;
         CONNECTION4CREATE_INFO_IN *dataIn ;
         connection4addData( connection, NULL, sizeof(CONNECTION4CREATE_INFO_IN), (void **)&dataIn ) ;

         if ( c4->createTemp == 1 )
         {
            dataIn->createTemp = 1 ;
            dataIn->log = htons5( LOG4TRANS ) ;
         }
         else
            dataIn->log = htons5( c4->log ) ;

         dataIn->collatingSequence = htons5( c4->collatingSequence ) ;
         dataIn->codePage = htons5( c4->codePage ) ;
         dataIn->collateName = htons5( (short)c4->collateName ) ;
         dataIn->collateNameUnicode = htons5( (short)c4->collateNameUnicode ) ;
         dataIn->autoIncrementStart = c4->autoIncrementStart ;
         assert5port( "added compressed memo entries support" ) ;
         dataIn->compressedMemosSupported = htons5( c4->compressedMemos ) ;

         dataIn->oledbSchemaCreate = c4->oledbSchemaCreate ;
         dataIn->safety = c4->safety ;
         dataIn->readOnly = c4getReadOnly( c4 ) ;  /* catalog purposes */
         /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
         dataIn->fileFlush = c4->fileFlush ;
         #ifdef S4PREPROCESS_FILE
            #ifdef S4ENCRYPT_DLL
               dataIn->preprocessFile = code4getPreprocessFile( c4 ) ;
            #else
               dataIn->preprocessFile = preprocess4fileHook( &c4->preprocess, 0, preprocess4getInit( &c4->preprocess ) ) ;
            #endif
            dataIn->preprocessFile = ntohs5( dataIn->preprocessFile ) ;
         #endif

         dataIn->compatibility = htons5(c4->compatibility) ;
         if ( name != 0 )
         {
            #ifdef E4MISC
               if ( strlen( name ) > LEN4PATH )
                  error4describe( c4, e4create, E84301, name, (char *)0, (char *)0 ) ;
            #endif
            strncpy( dataIn->name, name, LEN4PATH ) ;
         }

         unsigned short numTags ;
         if ( tagInfo == 0 )
            numTags = 0 ;
         else
         {
            for( numTags = 0 ; tagInfo[numTags].name != 0; numTags++ )
            {
               ;
            }
         }

         dataIn->numTags = htons5(numTags) ;

         for( i = 0 ; fieldData[i].name != 0; i++ )
         {
            ;
         }

         short numFields = i ;
         dataIn->numFields = htons5( numFields ) ;
         len = 0 ;

         for ( short fieldIndex = 0 ; fieldIndex != numFields ; fieldIndex++ )
         {
            len += sizeof( CONNECTION4FIELD_INFO ) ;
            len += strlen( fieldData[fieldIndex].name ) + 1 ;
         }

         dataIn->fieldInfoLen = htons5(len) ;
         offset = sizeof( CONNECTION4CREATE_INFO_IN ) ;

         for ( short fieldAssignIndex = 0 ; fieldAssignIndex != numFields ; fieldAssignIndex++ )
         {
            CONNECTION4FIELD_INFO *finfo ;
            len = strlen( fieldData[fieldAssignIndex].name ) + 1 ;
            connection4addData( connection, NULL, sizeof(CONNECTION4FIELD_INFO), (void **)&finfo ) ;
            finfo->name.offset = htons5((short)(offset + (short)sizeof(CONNECTION4FIELD_INFO))) ;
            finfo->type = htons5(fieldData[fieldAssignIndex].type) ;
            finfo->len = htons5(fieldData[fieldAssignIndex].len) ;
            finfo->dec = htons5(fieldData[fieldAssignIndex].dec) ;
            finfo->nulls = htons5(fieldData[fieldAssignIndex].nulls) ;
            connection4addData( connection, fieldData[fieldAssignIndex].name, len, NULL ) ;
            offset += ( len + sizeof( CONNECTION4FIELD_INFO ) ) ;
         }

         for ( unsigned int tagIndex = 0 ; tagIndex != numTags ; tagIndex++ )
         {
            len = strlen( tagInfo[tagIndex].name ) + 1 ;
            offset += sizeof( CONNECTION4TAG_INFO ) ;
            CONNECTION4TAG_INFO *tinfo ;
            connection4addData( connection, NULL, sizeof(CONNECTION4TAG_INFO), (void **)&tinfo ) ;
            tinfo->name.offset = htons5(offset) ;
            unsigned int len2 = 0 ;
            if ( tagInfo[tagIndex].expression )
               len2 = strlen( tagInfo[tagIndex].expression ) + 1 ;
            offset += len ;
            tinfo->expression.offset = htons5(offset) ;
            offset += len2 ;
            unsigned int len3 ;
            if ( tagInfo[tagIndex].filter == 0 )
            {
               len3 = 0 ;
               tinfo->filter.offset = 0 ;
            }
            else
            {
               len3 = strlen( tagInfo[tagIndex].filter ) + 1 ;
               tinfo->filter.offset = htons5(offset) ;
            }
            offset += len3 ;
            tinfo->unique = htons5(tagInfo[tagIndex].unique) ;
            tinfo->descending = htons5(tagInfo[tagIndex].descending) ;
            connection4addData( connection, tagInfo[tagIndex].name, len, NULL ) ;
            if ( len2 != 0 )
               connection4addData( connection, tagInfo[tagIndex].expression, len2, NULL ) ;
            if ( len3 != 0 )
               connection4addData( connection, tagInfo[tagIndex].filter, len3, NULL ) ;
         }

         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
         {
            #ifdef E4STACK
               error4stack( c4, rc, E91401 ) ;
            #endif
            return 0 ;
         }
         if ( connection4type( connection ) != CON4CREATE )
         {
            #ifdef E4STACK
               error4stack( c4, e4connection, E91401 ) ;
            #endif
            return 0 ;
         }

         rc = connection4status( connection ) ;
         if ( rc != 0 )
         {
            if ( c4getErrCreate( c4 ) == 0 )
               error4set( c4, r4noCreate ) ;
            else
               connection4errorDescribe( connection, c4, rc, E91401, name, 0, 0 ) ;
            return 0 ;
         }

         oldAutoOpen = c4->autoOpen ;
         if ( tagInfo == 0 )
            c4->autoOpen = 0 ;
         else
         {
            if ( tagInfo[0].name == 0 )
               c4->autoOpen = 0 ;
            else
               c4->autoOpen = 1 ;
         }

         if ( c4->createTemp == 0 )
         {
            if ( connection4len( connection ) != 0 )
            {
               c4->autoOpen = oldAutoOpen ;
               #ifdef E4STACK
                  error4stack( c4, e4packetLen, E91401 ) ;
               #endif
               return 0 ;
            }
            if ( numTags == 0 )
               c4->openForCreate = 2 ;
            else
               c4->openForCreate = 1 ;
            data = d4open( c4, name ) ;
            c4->openForCreate = 0 ;
         }
         else
         {
            int oldAccessMode = c4->accessMode ;
            c4->accessMode = OPEN4DENY_RW ;
            if ( name == 0 )
            {
               if ( connection4len( connection ) > sizeof( nameBuf ) + 1 )
               {
                  c4->accessMode = oldAccessMode ;
                  c4->autoOpen = oldAutoOpen ;
                  error4( c4, e4packetLen, E91401 ) ;
                  return 0 ;
               }
               memcpy( nameBuf, connection4data( connection ), (unsigned int)connection4len( connection ) ) ;
               nameBuf[connection4len( connection )] = 0 ;
               if ( numTags == 0 )
                  c4->openForCreate = 2 ;
               else
                  c4->openForCreate = 1 ;
               data = d4open( c4, nameBuf ) ;
               c4->openForCreate = 0 ;
            }
            else
            {
               if ( numTags == 0 )
                  c4->openForCreate = 2 ;
               else
                  c4->openForCreate = 1 ;
               data = d4open( c4, name ) ;
               c4->openForCreate = 0 ;
            }
            c4->accessMode = oldAccessMode ;
         }
         c4->autoOpen = oldAutoOpen ;
         if ( data == 0 )
            return 0 ;

         /* set the unique settings... */
         code4indexFormat( c4 ) ;  /* need to call this to avoid corruption in d4tag() call below which asks
                                      this question calling the server destroying com struct */
         connection4assign( connection, CON4UNIQUE_SET, data4clientId( data ), data4serverId( data ) ) ;
         CONNECTION4UNIQUE_INFO_IN *uniqueInfoIn ;
         connection4addData( connection, NULL, sizeof(CONNECTION4UNIQUE_INFO_IN), (void **)&uniqueInfoIn ) ;
         /* uniqueInfoIn->numTags = htons5(numTags) ; */
         short numUniqueTags = numTags ;
         for ( unsigned int tagIndexSet = 0 ; tagIndexSet != numTags ; tagIndexSet++ )
         {
            if ( tagInfo[tagIndexSet].unique == c4->errDefaultUnique )
               numUniqueTags-- ;
            else
            {
               /* AS 10/28/99 - clipper version, tag info may include path info - strip this off... */
               char tagName[LEN4TAG_ALIAS+1] ;  // AS 08/10/00 - s4copy emp.dbf - tag name of 10 characters failed to create correctly
               u4namePiece( tagName, sizeof( tagName ), tagInfo[tagIndexSet].name, 0, 0 ) ;
               tag = d4tag( data, tagName ) ;
               if ( tag == 0 )
               {
                  rc = error4describe( data->codeBase, e4name, E81406, d4alias( data ), tagInfo[tagIndexSet].name, 0 ) ;
                  break ;
               }
               CONNECTION4UNIQUE_TAG_INFO *uniqueTagInfo ;
               connection4addData( connection, NULL, sizeof(CONNECTION4UNIQUE_TAG_INFO), (void **)&uniqueTagInfo ) ;
               uniqueTagInfo->unique = htons5(tagInfo[tagIndexSet].unique) ;
               tag->errUnique = tagInfo[tagIndexSet].unique ;
               memcpy( uniqueTagInfo->alias, tag->tagFile->alias, LEN4TAG_ALIAS ) ;
            }
         }
         uniqueInfoIn->numTags = htons5(numUniqueTags) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc >= 0 )
         {
            rc = connection4status( connection ) ;
            if ( rc < 0 )
               connection4error( connection, c4, rc, E91401 ) ;
         }
         if ( rc < 0 )
         {
            d4close( data ) ;
            return 0 ;
         }

         return data ;
      #else
         if ( c4->createTemp == 1 )
         {
            data = 0 ;
            rc = dfile4create( c4, name, fieldData, tagInfo, &data ) ;
            if ( rc < 0 || data == 0 )   // data == 0 if rc == r4noOpen, some cases
            {
               d4createClose( c4, data, 1 ) ;
               return 0 ;
            }
         }
         else
         {
            // AS 09/29/00 - allow the logging of create now (for backup/recovery)
            // #ifndef S4OFF_TRAN
            //    saveStatus = code4tranStatus( c4 ) ;
            //    code4tranStatusSet( c4, r4off ) ;
            // #endif
            oldReadOnly = c4getReadOnly( c4 ) ;
            c4setReadOnly( c4, 0 ) ;
            if ( name == 0 )
            {
               rc = dfile4create( c4, name, fieldData, tagInfo, &data ) ;
               if ( rc < 0 )
               {
                  d4createClose( c4, data, 1 ) ;
                  return 0 ;
               }
               u4nameCurrent( nameBuf, sizeof( nameBuf ), data->dataFile->file.name ) ;
               d4close( data ) ;
            }
            else
               rc = dfile4create( c4, name, fieldData, tagInfo, 0 ) ;
            c4setReadOnly( c4, oldReadOnly ) ;
            // AS 09/29/00 - allow the logging of create now (for backup/recovery)
            // #ifndef S4OFF_TRAN
            //    if ( saveStatus != 0 )   /* which may have resulted in transaction enabling when file created, so leave */
            //       code4tranStatusSet( c4, saveStatus ) ;
            // #endif
            if ( rc != 0 )
               return 0 ;
            oldAutoOpen = c4->autoOpen ;
            if ( tagInfo == 0 )
               c4->autoOpen = 0 ;
            else
            {
               if ( tagInfo[0].name == 0 )
                  c4->autoOpen = 0 ;
               else /* make sure on */
                  c4->autoOpen = 1 ;
            }
            #ifndef S4CLIENT
               // AS Nov 27/02 - need to track open-for-create for data file compression (do we initialize)
               c4->openForCreate = 1 ;
            #endif
            if ( name == NULL )
               data = d4open( c4, nameBuf ) ;
            else
               data = d4open( c4, name ) ;
            #ifndef S4CLIENT
               c4->openForCreate = 0 ;
            #endif
            c4->autoOpen = oldAutoOpen ;
            if ( data == 0 )
               return 0 ;
         }

         /* set the unique settings... */
         if ( tagInfo != 0 )
         {
            for ( i = 0, tag = 0 ; tagInfo[i].name != 0 ; i++ )
            {
               if ( tagInfo[i].name[0] == 0 )  // temporary, just d4tagNext...
                  tag = d4tagNext( data, tag ) ;
               else
               {
                  // AS 11/22/00 - Not working correct for clipper... tag name should not include path...
                  #ifdef S4CLIPPER
                     char tagName[LEN4TAG_ALIAS+1] ;
                     u4namePiece( tagName, sizeof( tagName ), tagInfo[i].name, 0, 0 ) ;
                  #else
                     char *tagName ;
                     tagName = tagInfo[i].name ;
                  #endif
                  tag = d4tag( data, tagName ) ;
               }
               if ( tag == 0 )
               {
                  error4describe( data->codeBase, e4name, E81406, d4alias( data ), tagInfo[i].name, 0 ) ;
                  return data ;
               }
               tag->errUnique = tagInfo[i].unique ;
            }
         }

         return data ;
      #endif
   }



   /* !S4OFF_WRITE */
   DATA4 *S4FUNCTION d4create( CODE4 *c4, const char *name, const FIELD4INFO *fieldData, const TAG4INFO *tagInfo )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( c4, 1, E91401 ) )
            return 0 ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( c4 == 0 || fieldData == 0 )
         {
            error4( c4, e4parm_null, E91401 ) ;
            return 0 ;
         }
         //#ifndef OLEDB5BUILD /* commented out for compatablity with OLEDB and library users */
            /* AS 06/04/97 ole-db allows creation of permanent files without fixed file names */
            if ( name == 0 && c4->createTemp != 1 )
            {
               error4( c4, e4parm_null, E91401 ) ;
               return 0 ;
            }
         //#endif */
         #ifdef S4CLIENT
            /* 04/22/99 - must test input tag info which may not be caught by server... */
            if ( tagInfo != 0 )
            {
               for( int i = 0 ; tagInfo[i].name != 0; i++ )
               {
                  if ( tagInfo[i].descending )
                     if ( tagInfo[i].descending != r4descending )
                     {
                        error4describe( c4, e4tagInfo, E85302, tagInfo[i].name, 0, 0 ) ;
                        return 0 ;
                     }
               }
            }
         #endif
      #endif

      // AS Nov 20/02 - track for preprocessing
      // AS Jan 28/03 - we can change this still after calling d4create, just not before d4open.
      // #if defined( S4PREPROCESS_FILE ) && defined( S4STAND_ALONE )
      //    c4->fileAccessed = 1 ;
      // #endif

      #ifdef E4MISC
         /* AS 04/09/99 - added check in e4misc case to verify field names are unique. */
         for ( const FIELD4INFO *fieldOuter = fieldData ;; fieldOuter++ )
         {
            if ( fieldOuter->name == 0 || fieldOuter->name[0] == 0 )
               break ;
            for ( const FIELD4INFO *fieldInner = fieldOuter + 1 ;; fieldInner++ )
            {
               if ( fieldInner->name == 0 || fieldInner->name[0] == 0 )
                  break ;
               if ( strcmp( fieldOuter->name, fieldInner->name ) == 0 )
               {
                  error4( c4, e4parm, E80911 ) ;
                  return 0 ;
               }
            }
         }
      #endif

      #ifdef E4ANALYZE
         if ( c4->debugInt != 0x5281 )
         {
            error4( c4, e4info, E81301 ) ;
            return 0 ;
         }
      #endif

      // AS Jan 16/03 - There are now other cases where the compatibility gets bumped up, so save it always
      #ifdef S4FOX
         int oldCompatibility = c4->compatibility ;

         #if defined( S4PREPROCESS_FILE )
            // preprocessing only supported with 0x30 mode, set if not already done...
            c4->compatibility = 30 ;
         #endif
      #endif

      DATA4 *data = d4createLow( c4,name,fieldData,tagInfo ) ;

      #ifdef S4FOX
         if ( data == 0 && c4->compatibility < 30 && error4code( c4 ) == 0 )  // Jan 16/03 maybe a compatibility problem, returns 0 but does not fail in that case
         {
            c4->compatibility = 30 ;
            data = d4createLow( c4,name,fieldData,tagInfo ) ;
         }

         // AS Jan 16/03 - There are now other cases where the compatibility gets bumped up, so save it always
         c4->compatibility = oldCompatibility ;
      #endif
      return data ;
   }


   #ifndef S4CLIENT
      /* !S4OFF_WRITE, !S4CLIENT */
      static void format4createName( char *outName, int outNameLen, const char *inName )
      {
         u4nameCurrent( outName, outNameLen, inName ) ;
         u4nameExt( outName, outNameLen, DBF4EXT, 0 ) ;
      }



      /* !S4OFF_WRITE, !S4CLIENT */
      static int dfile4verifyNotAlreadyOpen( CODE4 *c4, char *dataVerifyName )
      {
         /* check if the given data is already open in our CODE4.  If it is, attempt to close it.
            i.e. if nobody actually has a handle to it, then close it.
         */

         int rc ;
         DATA4FILE *dfile = dfile4data( c4, dataVerifyName ) ;

         if ( dfile != 0 )
         {
            /* an unused dfile may be removed to allow re-create - so attempt a close which will
               only actually close if nobody in the current CODE4 has a handle to the DATA4
            */
            rc = dfile4closeLow( dfile ) ;
            if ( rc < 0 )
               return rc ;
            dfile = dfile4data( c4, dataVerifyName ) ;
            if ( dfile != 0 )   /* datafile already open -- can't create */
               return error4describe( c4, e4create, E81304, dataVerifyName, (char *)0, (char *)0 ) ;
         }

         return 0 ;
      }



      /* !S4OFF_WRITE, !S4CLIENT */
      static S4LONG calculateRecordLength( CODE4 *c4, const FIELD4INFO *fieldData, unsigned short *numFlds,
                                           int *numNulls, int *hasMemo, Bool5 *hasAutoIncrement )
      {
         /* goes through the fieldData and calculates the record length and the # of fields.
            returns the record length, sets numFlds to the # of fields on output, and sets
            the # of null fields into numNulls.

            return is < 0 if error.
         */

         long calcRecordLen = 1L ;   /* start at 1 byte for the deleted flag */
         *numNulls = 0 ;
         *numFlds = 0 ;
         *hasMemo = 0 ;

         for ( ; fieldData[(*numFlds)].name ; (*numFlds)++ )
         {
            #ifdef S4FOX
               if ( c4->compatibility == 30 )
                  if ( fieldData[(*numFlds)].nulls == r4null )
                     (*numNulls)++ ;

               if ( fieldData[(*numFlds)].nulls == r4autoIncrement )
               {
                  if ( (*hasAutoIncrement) )
                     return error4( c4, e4data, E81410 ) ;
                  (*hasAutoIncrement) = 1 ;
                  if ( fieldData[(*numFlds)].type != r4double )
                     return error4( c4, e4data, E81411 ) ;
               }
            #endif

            #ifndef S4OFF_MEMO
               if ( fieldData[(*numFlds)].type == r4memo || fieldData[(*numFlds)].type == r4gen
               #ifdef S4FOX
                   || fieldData[(*numFlds)].type == r4memoBin
               #else
                   || (fieldData[(*numFlds)].type == r4bin && c4->oledbSchemaCreate != 1 )
               #endif
                  )
                  *hasMemo = 1 ;
            #endif
            switch( fieldData[(*numFlds)].type )
            {
               case r4num:
               case r4float:
               case r4str:
                  calcRecordLen += fieldData[(*numFlds)].len ;
                  break ;
               case r4memo:
               case r4gen:
               #ifndef S4FOX
                  case r4bin:  // CS 2000/09/26 move to memo section
               #endif
                  #ifdef S4OFF_MEMO
                     #ifdef E4MISC
                        return error4( c4, e4notMemo, E91102 ) ;
                     #endif
                  #else
                     #ifdef S4FOX
                        if ( c4->compatibility == 30 )
                           calcRecordLen += 4 ;
                        else
                     #endif
                        calcRecordLen += 10 ;
                     break ;
                  #endif
               case r4date:
                  calcRecordLen += 8 ;
                  break ;
               case r4log:
                  calcRecordLen += 1 ;
                  break ;
               #ifdef S4FOX
                  case r4memoBin:
                     if ( c4->compatibility != 30 )
                        return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                        // return error4( c4, e4data, E81404 ) ;
                     calcRecordLen += 4 ;
                     break ;
                  case r4charBin:
                     if ( c4->compatibility != 30 )
                        return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                        // return error4( c4, e4data, E81404 ) ;
                     calcRecordLen += fieldData[(*numFlds)].len ;
                     break ;
                  case r4currency:
                     if ( c4->compatibility != 30 )
                        return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                        // return error4( c4, e4data, E81404 ) ;
                     calcRecordLen += 8 ;
                     break ;
                  case r4dateTime:
                     if ( c4->compatibility != 30 )
                        return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                        // return error4( c4, e4data, E81404 ) ;
                     calcRecordLen += 8 ;
                     break ;
                  case r4double:
                     if ( c4->compatibility != 30 )
                        return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                        // return error4( c4, e4data, E81404 ) ;
                     calcRecordLen += 8 ;
                     break ;
                  case r4int:
                     if ( c4->compatibility != 30 )
                        return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                        // return error4( c4, e4data, E81404 ) ;
                     calcRecordLen += 4 ;
                     break ;
               #endif
               default:
                  switch( fieldData[(*numFlds)].type )
                  {
                     case r5wstr:
                        #ifdef S4FOX  // CS 2001/04/27
                           // AS 07/25/01 - Allow for compatibility of 31 as well...
                           // if ( c4->compatibility != 30 )
                           if ( c4->compatibility < 30 )
                              return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                              // return error4( c4, e4data, E81404 ) ;
                        #endif
                        calcRecordLen += even4up( fieldData[(*numFlds)].len ) ;
                        break ;
                     case r5guid:
                        calcRecordLen += LEN5GUID ;
                        break ;
                     case r5wstrLen:
                        /* wstrLen includes extra bytes to store the length */
                        calcRecordLen += even4up( fieldData[(*numFlds)].len ) + sizeof( short ) ;
                        break ;
                     case r5date:
                        calcRecordLen += sizeof( double ) ;
                        break ;
                     case r5i2:
                     case r5ui2:
                        calcRecordLen += 2 ;
                        break ;
                     case r4int:
                     case r5ui4:
                        calcRecordLen += 4 ;
                        break ;
                     case r5i8:
                     case r5ui8:
                        calcRecordLen += 8 ;
                        break ;
                     case r5dbDate:
                     case r5dbTime:
                        calcRecordLen += 6 ;
                        break ;
                     case r5dbTimeStamp:
                        calcRecordLen += 16 ;
                        break ;
                     case r4memoBin:
                        calcRecordLen += 4 ;
                        break ;
                     case r4charBin:
                        calcRecordLen += fieldData[(*numFlds)].len ;
                        break ;
                     case r4currency:
                        calcRecordLen += 8 ;
                        break ;
                     case r4dateTime:
                        calcRecordLen += 8 ;
                        break ;
                     case r4double:
                        calcRecordLen += 8 ;
                        break ;
                     default:
                        char strType[16];   // CS 2001/05/17 include bad field type in error message
                        /* LY 2002/10/10 : sprintf() not available in HPC SDK */
                        #ifndef S4WINCE
                           if (isprint(fieldData[(*numFlds)].type))
                              sprintf(strType, "%d ('%c')", fieldData[(*numFlds)].type, fieldData[(*numFlds)].type);
                           else
                              sprintf(strType, "%d", fieldData[(*numFlds)].type);
                        #else
                           _itoa( fieldData[(*numFlds)].type, strType, 10 ) ;
                        #endif
                        return error4describe( c4, e4fieldType, E81404, strType, 0, 0 ) ;
                  }
                  break ;
            }
         }

         if ( (unsigned long)calcRecordLen >= (unsigned long)USHRT_MAX ) /* Increment for deletion flag. */
         {
            if ( c4->largeFileOffset == 0 )  /* allow if largeFileOffset != 0, otherwise error */
               return error4( c4, e4recordLen, E81407 ) ;
         }

         #ifdef S4FOX
            if ( c4->compatibility == 30 )
               if ( (*numNulls) > 0 )  /* extra field for null settings */
                  calcRecordLen += ( (*numNulls) + 7 ) / 8 ;
         #endif

         #ifdef S4PREPROCESS_FILE
            if ( code4getPreprocessFile( c4 ) == 1 )
            {
               // AS May 24/02 - Modify the record length for preprocessing - the record must lie on the block
               // byte boundary
               short blockSize = code4getPreprocessBlockSize( c4 ) ;
               if ( ( calcRecordLen % blockSize ) != 0 )
                  calcRecordLen += (blockSize - ( calcRecordLen % blockSize ) ) ;
            }
         #endif

         return calcRecordLen ;
      }



      /* !S4OFF_WRITE, !S4CLIENT */
      static S4LONG calculateHeaderLength( CODE4 *c4, unsigned short numFields, int numNulls )
      {
         S4LONG headerLen = (long)numFields * 32 + 34 ;

         #ifdef S4FOX
            if ( c4->compatibility == 30 )
            {
               if ( numNulls > 0 )  /* extra field for null settings */
                  headerLen += 32 ;
               headerLen += 263 ;    /* visual fox reserves an extra 263 bytes */
            }
         #endif

         if ( (unsigned long)headerLen >= (unsigned long)USHRT_MAX )
            return error4( c4, e4recordLen, E81408 ) ;

         #ifdef S4PREPROCESS_FILE
            if ( code4getPreprocessFile( c4 ) == 1 )
            {
               // AS May 24/02 - Modify the record length for preprocessing - the record must lie on the block
               // byte boundary
               short blockSize = code4getPreprocessBlockSize( c4 ) ;
               if ( ( headerLen % blockSize ) != 0 )
                  headerLen += (blockSize - ( headerLen % blockSize ) ) ;
               assert5( ( headerLen % blockSize ) == 0 ) ;
            }
         #endif

         return headerLen ;
      }



      /* LY 99/08/16 : from static to static int */
      /* !S4OFF_WRITE, !S4CLIENT */
      static int dfile4createFile( CODE4 *c4, FILE4 *file, char *name, int createTemp, char **tempName, int *tempFreeSet )
      {
         /* actually physically create the file */

         int rc, oldCreateTemp, len ;

         *tempName = 0 ;
         *tempFreeSet = 0 ;

         if ( createTemp == 0  )
         {
            #ifdef S4SERVER
               #ifndef S4OFF_CATALOG
                  if ( c4->createTemp != 1 && cat4avail( c4->catalog ) )
                  {
                     u4ncpy( name, cat4pathName( c4->catalog ), (unsigned int)cat4pathNameLen( c4->catalog ) ) ;
                     for ( i = 0 ; i < cat4pathNameLen( c4->catalog ) ; i++ )
                        if ( name[i] == ' ' )
                        {
                           name[i] = 0 ;
                           break ;
                        }
                  }
               #endif  /* S4OFF_CATALOG */
            #endif /* S4SERVER */

            if ( c4->createTemp == 1 )  /* take care of setting as temp later */
            {
               c4->createTemp = 0 ;
               // AS May 24/02 - created file4createLow for internal use to indicate file types
               rc = file4createInternal( file, c4, name, 1, OPT4DBF ) ;
               c4->createTemp = 1 ;
            }
            else
            {
               // AS May 24/02 - created file4createLow for internal use to indicate file types
               rc = file4createInternal( file, c4, name, 1, OPT4DBF ) ;
            }
         }
         else
         {
            oldCreateTemp = c4->createTemp ;
            c4->createTemp = 0 ;
            // AS 04/21/98 conformance tests, schema tables must have .dbf extension...
      //      rc = file4tempLow( &file, c4, 0, oldCreateTemp, (char *)(oldCreateTemp ? NULL : "DBF") ) ;  /* need to set as non-remove to get name input */
            // AS May 24/02 - created file4createLow for internal use to indicate file types
            rc = file4tempLow( file, c4, 0, oldCreateTemp, DBF4EXT, OPT4DBF ) ;  /* need to set as non-remove to get name input */
            c4->createTemp = oldCreateTemp ;
            if ( rc == 0 )
            {
               if ( oldCreateTemp == 1 )
                  file4setTemporary( file, 1, 0 ) ;  // AS Sep. 18/01 - last param 0, don't register as temp, only mark it
               if ( file->name != 0 )
               {
                  len = c4strlen( file->name ) ;
                  if ( len > sizeof( name ) + 1 )
                     len = sizeof( name ) - 1 ;
                  c4memcpy( name, file->name, len ) ;
                  name[len] = 0 ;
                  (*tempName) = file->nameBuf ;
               }
               #ifdef E4DEBUG
                  else
                  {
                     file4close( file ) ;
                     rc = error4( c4, e4info, E91102 ) ;
                  }
               #endif
               (*tempFreeSet) = 1 ;
            }
         }

         return rc ;
      }



      /* !S4OFF_WRITE, !S4CLIENT */
      static void dfile4createHeaderImageSet( FIELD4IMAGE *image, unsigned char len, unsigned char dec, char nullBinaryMask = 0 )
      {
         image->len = len ;
         image->dec = dec ;
         #ifdef S4FOX
            image->nullBinary |= nullBinaryMask ;
         #endif
      }



      /* !S4OFF_WRITE, !S4CLIENT */
      static unsigned short dfile4createCalcHeaderLen( CODE4 *c4, int numFields, int numNulls, const FIELD4INFO *fieldData )
      {
         unsigned short headerLen = (unsigned short)( 32 * ( numFields + 1 ) + 1) ;
         #ifdef S4FOX
            if ( c4->compatibility == 30 )  /* 3.0 file */
            {
               if ( numNulls > 0 )  /* extra field for null settings */
                  headerLen += 32 ;
               headerLen += 263 ;    /* visual fox reserves an extra 263 bytes */
            }
         #endif

         /* AS 09/03/99 have new ability to create additional info in header for
            CodeBase specific info. */
         // if any of the field types are enhanced, then add space for them in the file...
         /*
            - not enabled yet...


         */

         // just try to see what Fox does...
         // headerLen += 122 ;

         #ifdef S4PREPROCESS_FILE
            if ( code4getPreprocessFile( c4 ) == 1 )
            {
               // AS May 24/02 - Modify the record length for preprocessing - the record must lie on the block
               // byte boundary
               short blockSize = code4getPreprocessBlockSize( c4 ) ;
               if ( ( headerLen % blockSize ) != 0 )
                  headerLen += (blockSize - ( headerLen % blockSize ) ) ;
               assert5( ( headerLen % blockSize ) == 0 ) ;
            }
         #endif

         return headerLen ;
      }



      /* !S4OFF_WRITE, !S4CLIENT */
      static int dfile4createHeader( CODE4 *c4, int needsMemo, FILE4 *file, S4LONG calcRecordLen,
                                     unsigned short numFields, const FIELD4INFO *fieldData, int numNulls,
                                     int *hasMemo, Bool5 hasAutoIncrement, Bool5 hasCompressedMemos,
                                     Bool5 hasCompressedData ) //, short compressedBlockSize )
      {
         #ifndef S4LUPACH  /* LY 2003/07/07 */
            assert5port( "added compressed memo entries support" ) ;
         #endif
         /*

            RETURNS

            0 - success
            < 0 - failure - the caller should delete the file
         */
         #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
            double tempDbl ;
         #endif
         *hasMemo = 0 ;   // initialize to not having memo, if a memo field is found, change this at that time

         FILE4LONG pos ;
         file4longAssign( pos, 0, 0L ) ;

         FILE4SEQ_WRITE seqWrite ;
         char buffer[0x800] ;
         int rc = file4seqWriteInitLow( &seqWrite, file, pos, buffer, sizeof(buffer) ) ;
         if ( rc < 0 )
            return rc ;

         DATA4HEADER_FULL createHeader ;
         c4memset( (void *)&createHeader, 0, sizeof(createHeader) ) ;
         #ifdef S4FOX
            if ( c4->compatibility == 30 )  /* 3.0 file */
            {
               #ifndef S4LUPACH  /* LY 2003/07/07 */
                  assert5port( "added compressed memo entries support" ) ;
                  assert5port( "added compressed data support" ) ;
                  if ( hasAutoIncrement == 1 || hasCompressedMemos == 1 || hasCompressedData == 1 )
               #else
                  if ( hasAutoIncrement == 1 )
               #endif
                  createHeader.version = 0x31 ;
               else
                  createHeader.version = 0x30 ;
               #ifndef S4OFF_MEMO
                  if ( needsMemo )
                     createHeader.hasMdxMemo |= 0x02 ;
               #endif
            }
            else
            {
               #ifndef S4OFF_MEMO
                  if ( needsMemo )
                     createHeader.version = (char)0xF5 ;
                  else
               #endif
                  createHeader.version = (char)0x03 ;
            }
         #else
            #ifndef S4OFF_MEMO
               if ( needsMemo )
                  #ifdef S4MNDX
                     createHeader.version = (char)0x83 ;
                  #endif  /* S4MNDX */
                  #ifdef S4MMDX
                     createHeader.version = (char)0x8B ;
                  #endif  /* S4MMDX */
               else
            #endif /* S4OFF_MEMO */
               createHeader.version = (char)0x03 ;
         #endif /* S4FOX */

         u4yymmdd( &createHeader.yy ) ;
         createHeader.headerLen = dfile4createCalcHeaderLen( c4, numFields, numNulls, fieldData ) ;
         createHeader.recordLen = (unsigned short)calcRecordLen ;

         #ifdef S4FOX
            createHeader.codePage = (char)c4->codePage ;   /* write codepage stamp */
            // set the auto-increment feature if avail...
            if ( hasAutoIncrement )
            {
               if ( c4->compatibility != 30 )
               {
                  file4seqWriteFlush( &seqWrite ) ;
                  return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                  // return error4describe( c4, e4data, E81404, "Auto-increment field type requested, but only supported if compatibility has been set to 30", 0, 0 ) ;
               }
               #ifdef S4DATA_ALIGN  /* LY 2001/07/19 : autoIncrementVal changed to char[8] from double */
                  memcpy( createHeader.autoIncrementVal, (char*)&c4->autoIncrementStart, sizeof(double) ) ;
               #else
                  createHeader.autoIncrementVal = c4->autoIncrementStart ;
               #endif
               createHeader.flags[0] = 1 ;   // means contains autoIncrement
            }
            #ifndef S4LUPACH  /* LY 2003/07/07 */
               assert5port( "added compressed memo entries support" ) ;
               if ( hasCompressedMemos )
               {
                  if ( c4->compatibility != 30 )
                  {
                     file4seqWriteFlush( &seqWrite ) ;
                     return e4compatibility ;  // AS don't hard error out, just return e4compatibility so we can retry
                     // return error4describe( c4, e4data, E81404, "Compressed memos requested, but only supported if compatibility has been set to 30", 0, 0 ) ;
                  }
                  createHeader.flags[1] = 1 ;   // means memo fields may be compressed
               }
               assert5port( "added compressed data support" ) ;
               if ( hasCompressedData )
               {
                  assert5( c4->compatibility == 30 ) ;  // should have been set by caller
                  createHeader.flags[2] = 1 ;   // means data file is compressed
               }
            #endif
         #else
            assert5( hasCompressedData == 0 ) ;  // only supported in fox
         #endif

         #ifdef S4BYTE_SWAP
            createHeader.numRecs = x4reverseLong( (void *)&createHeader.numRecs ) ;
            createHeader.headerLen = x4reverseShort( (void *)&createHeader.headerLen ) ;
            createHeader.recordLen = x4reverseShort( (void *)&createHeader.recordLen ) ;
            /* LY 2001/07/21 : need to swap auto-increment */
            #ifdef S4DATA_ALIGN
               tempDbl = x4reverseDouble( &createHeader.autoIncrementVal ) ;
               memcpy( createHeader.autoIncrementVal, (char*)&tempDbl, sizeof(double) ) ;
            #else
               createHeader.autoIncrementVal = x4reverseDouble( &createHeader.autoIncrementVal ) ;
            #endif
         #endif  /* S4BYTE_SWAP */

         rc = file4seqWrite( &seqWrite, (char *) &createHeader, sizeof(createHeader) ) ;
         if ( rc < 0 )
         {
            file4seqWriteFlush( &seqWrite ) ;
            return rc ;
         }

         #ifdef S4FOX
            calcRecordLen = 1L ;
         #endif
         for ( int i = 0; i < (int)numFields; i++ )
         {
            FIELD4IMAGE createFieldImage ;
            c4memset( (void *)&createFieldImage, 0, sizeof(createFieldImage) ) ;
            u4ncpy( createFieldImage.name, fieldData[i].name, sizeof(createFieldImage.name));
            c4trimN( createFieldImage.name, sizeof(createFieldImage.name) ) ;
            c4upper( createFieldImage.name ) ;

            createFieldImage.type = (char)fieldData[i].type ;
            c4upper( &createFieldImage.type ) ;
            #ifdef S4FOX
               createFieldImage.offset = (short)calcRecordLen ;
               #ifdef S4BYTE_SWAP
                  createFieldImage.offset = x4reverseLong( (void *)&createFieldImage.offset ) ;
               #endif
               calcRecordLen += fieldData[i].len ;
               if ( fieldData[i].nulls == r4null )
                  createFieldImage.nullBinary |= 0x02 ;
               if ( fieldData[i].nulls == r4autoIncrement )
                  createFieldImage.nullBinary |= 0x08 ;
            #endif

            switch( createFieldImage.type )
            {
               case r4str:
                  dfile4createHeaderImageSet( &createFieldImage, (unsigned char)(fieldData[i].len & 0xFF), (unsigned char)(fieldData[i].len >> 8) ) ;
                  break ;
               case r4date:
                  dfile4createHeaderImageSet( &createFieldImage, 8, 0 ) ;
                  break ;
               case r4log:
                  dfile4createHeaderImageSet( &createFieldImage, 1, 0 ) ;
                  break ;
               case r4num:
               case r4float:
                  dfile4createHeaderImageSet( &createFieldImage, (unsigned char)fieldData[i].len, (unsigned char)fieldData[i].dec ) ;
                  #ifdef E4MISC
                     {
                        int len = fieldData[i].len ;
                        int dec = fieldData[i].dec ;
                        // AS 07/31/00 - in server/clipper, we need a numeric field width 20 for odbc
                        // support.  To allow this, relax the constraint for temporary files in clipper
                        // server case if this is the problem...
                        Bool5 badData = 0 ;
                        if ( len > F4MAX_NUMERIC )
                        {
                           #ifdef S4SERVER
                              #ifdef S4CLIPPER
                                 if ( c4->createTemp == 0 || len > 20 )
                              #endif
                           #endif
                                    badData = 1 ;
                        }

                        if ( ( len < 1 || len <= 2 && dec != 0 || dec < 0 ) || ( dec > len - F4DECIMAL_OFFSET  && dec > 0 ) || ( dec > F4MAX_DECIMAL ) )
                           badData = 1 ;

                        if ( badData == 1 )
                        {
                           rc = error4( c4, e4data, E81404 ) ;
                        }
                     }
                  #endif
                  break ;
               case r5guid:
                  dfile4createHeaderImageSet( &createFieldImage, LEN5GUID, 0, 0x04 ) ;
                  break ;
               case r5wstr:
                  dfile4createHeaderImageSet( &createFieldImage, (unsigned char)even4up(fieldData[i].len & 0xFF), (unsigned char)(fieldData[i].len >> 8), 0x04 ) ;
                  break ;
               case r5wstrLen:
                  /* for wstrLen, we add 2 bytes to the input length in order to save the field length*/
                  dfile4createHeaderImageSet( &createFieldImage, (unsigned char)even4up( (fieldData[i].len + sizeof( unsigned short ))& 0xFF), (unsigned char)( (fieldData[i].len + sizeof( unsigned short ) ) >> 8), 0x04 ) ;
                  break ;
               case r4charBin:
                  createFieldImage.type = r4str ;
                  dfile4createHeaderImageSet( &createFieldImage, (unsigned char)(fieldData[i].len & 0xFF), (unsigned char)(fieldData[i].len >> 8), 0x04 ) ;
                  break ;
               case r4currency:
                  dfile4createHeaderImageSet( &createFieldImage, 8, 4, 0x04 ) ;
                  break ;
               case r4dateTime:
                  dfile4createHeaderImageSet( &createFieldImage, 8, 0, 0x04 ) ;
                  break ;
               case r5i2:
               case r5ui2:
                  dfile4createHeaderImageSet( &createFieldImage, 2, 0, 0x04 ) ;
                  break ;
               case r5date:
                  dfile4createHeaderImageSet( &createFieldImage, sizeof( double ), 0, 0x04 ) ;
                  break ;
               case r5ui4:
               case r4int:
                  dfile4createHeaderImageSet( &createFieldImage, 4, 0, 0x04 ) ;
                  break ;
               case r5i8:
               case r5ui8:
                  dfile4createHeaderImageSet( &createFieldImage, 8, 0, 0x04 ) ;
                  break ;
               case r5dbDate:
               case r5dbTime:
                  dfile4createHeaderImageSet( &createFieldImage, 6, 0, 0x04 ) ;
                  break ;
               case r5dbTimeStamp:
                  dfile4createHeaderImageSet( &createFieldImage, 16, 0, 0x04 ) ;
                  break ;
               #ifndef S4OFF_MEMO
                  case r4memo:
                  case r4gen:
                  #ifndef S4FOX
                     case r4bin:
                  #endif
                     #ifdef S4FOX
                        if ( c4->compatibility == 30 )  /* 3.0 file */
                           createFieldImage.len = 4 ;
                        else
                           createFieldImage.len = 10 ;
                     #else
                        createFieldImage.len = 10 ;
                     #endif
                     createFieldImage.dec = 0 ;
                     break ;
               #endif
               #ifdef S4FOX
                  case r4double:  /* used as r4bin in MDX (binary memo) */
                     dfile4createHeaderImageSet( &createFieldImage, 8, (unsigned char)fieldData[i].dec, 0x04 ) ;
                     break ;
                  case r4memoBin:
                     createFieldImage.type = r4memo ;
                     dfile4createHeaderImageSet( &createFieldImage, 4, 0, 0x04 ) ;
                     break ;
               #endif
               default:
                  rc = error4( c4, e4fieldType, E81404 ) ;
            }

            if ( rc == 0 )
            {
               rc = file4seqWrite( &seqWrite, &createFieldImage, sizeof(createFieldImage) ) ;
               if ( rc < 0 )
                  break ;
            }
         }

         #ifdef S4FOX
            if ( rc == 0 )
               if ( numNulls > 0 ) /* need to add the special field */
               {
                  FIELD4IMAGE createFieldImage ;
                  c4memset( (void *)&createFieldImage, 0, sizeof(createFieldImage) ) ;
                  u4ncpy( createFieldImage.name, "_NullFlags", sizeof(createFieldImage.name));
                  c4trimN( createFieldImage.name, sizeof(createFieldImage.name) ) ;

                  createFieldImage.offset = (short)calcRecordLen ;
                  createFieldImage.type = r4system ;
                  createFieldImage.nullBinary = 0x05 ;
                  createFieldImage.len = (numNulls+7)/8 ;  /* 1 byte for every 8 nulls */
                  createFieldImage.dec = 0 ;
                  rc = file4seqWrite( &seqWrite, &createFieldImage, sizeof(createFieldImage) ) ;
               }
         #endif

         if ( rc == 0 )
         {
            #ifdef S4FOX
               if ( c4->compatibility == 30 )  /* 3.0 file */
               {
                  rc = file4seqWriteRepeat( &seqWrite, 1, '\015' ) ;
                  if ( rc == 0 )
                     rc = file4seqWriteRepeat( &seqWrite, 263, '\0' ) ;
               }
               else
                  rc = file4seqWrite( &seqWrite, "\015\032", 2 ) ;

               // AS Dec 11/02 - don't pad unless preprocess is on...
               #ifdef S4PREPROCESS_FILE
                  if ( code4getPreprocessFile( c4 ) == 1 )
                  {
                     // AS May 24/02 - Modify the record length for preprocessing - the record must lie on the block
                     // byte boundary
                     short blockSize = code4getPreprocessBlockSize( c4 ) ;
                     if ( blockSize > 1 )
                     {
                        rc = file4seqWriteRepeat( &seqWrite, 8, '\0' ) ;
                        assert5( createHeader.headerLen % blockSize == 0 ) ;
                     }
                  }
               #endif

               // ensure the header len does match the expected value...
               // AS Jan 3/03 - The header may be > size of the seqWrite total, use mod instead
               // AS Jan 7/03 - Applies only for foxpro 30 compatibility
               assert5( c4->compatibility != 30 || (createHeader.headerLen + seqWrite.avail) % seqWrite.total == 0 ) ;

               // test for what fox does...
               // file4seqWrite( &seqWrite, "CODEBASE", 8 ) ;
               // rc = file4seqWriteRepeat( &seqWrite, 114, '\0' ) ;
            #else
               rc = file4seqWrite( &seqWrite, "\015\032", 2 ) ;
            #endif
            #ifndef S4OFF_MEMO
               if ( rc == 0 )
               {
                  #ifdef S4FOX
                     if ( c4->compatibility == 30 )
                     {
                        if ( createHeader.hasMdxMemo & 0x02 )
                           *hasMemo = 1 ;
                     }
                     else
                        if ( createHeader.version & 0x80 )
                           *hasMemo = 1 ;
                  #else
                     if ( createHeader.version & 0x80 )
                        *hasMemo = 1 ;
                  #endif
               }
            #endif
         }

         /*
         #ifdef S4FOX
            if ( rc == = && hasCompressedData )
            {
               assert5port( "added compressed data support" ) ;
               short version = 1 ;  // for possible future changes
               rc = file4seqWrite( &seqWrite, &version, sizeof( verison ) ) ;
               // the # of 1k blocks to compress... (compress size)
               if ( rc == 0 )
                  rc = file4seqWrite( &seqWrite, &compressedBlockSize, sizeof( compressedBlockSize ) ) ;
               long numBlocks = 0 ;  // unknown at this point

            }
         #endif
         */
         int rc2 = file4seqWriteFlush( &seqWrite ) ;
         if ( rc < 0 )
            return rc ;
         return rc2 ;
      }



      /* !S4OFF_WRITE, !S4CLIENT */
      static void dfile4createIndex( DATA4 *data, const TAG4INFO *tagInfo, const char *name )
      {
         /*
            This functino may change the accessMode (in particular in server).  It should be saved and
            reset by the user function if required.

            ERRORS

            sets CODE4.errorCode to appropriate error and returns.
         */
         CODE4 *c4 = data->codeBase ;
         #ifndef S4OFF_INDEX
            #ifdef S4CLIPPER
               char nameBuf[258] ;
            #endif
         #endif

         #ifdef S4SERVER
            data->accessMode = OPEN4DENY_RW ;
         #endif

         #ifdef S4OFF_INDEX
            #ifdef E4MISC
               if ( tagInfo )
                  error4( c4, e4notIndex, E91102 ) ;
            #endif
         #else
            if ( tagInfo )
               #ifdef S4CLIPPER
                  if ( name == 0 )
                  {
                     if ( i4create( data, 0, tagInfo ) == 0 )
                        if ( error4code( c4 ) != r4noCreate && error4code( c4 ) > 0 )  // if == r4noCreate, just pass through
                           error4( c4, e4create, E91102 ) ;
                   }
                  else
                  {
                     /* 03/06/96 AS --> fix #25 changes.60 */
                     u4namePiece( nameBuf, sizeof( nameBuf ), name, 1, 0 ) ;
                     if ( i4create( data, nameBuf, tagInfo ) == 0 )
                        if ( error4code( c4 ) != r4noCreate && error4code( c4 ) > 0 )  // if == r4noCreate, just pass through
                           error4( c4, e4create, E91102 ) ;
                  }
               #else
                  if ( i4create( data, 0, tagInfo ) == 0 )
                     if ( error4code( c4 ) != r4noCreate && error4code( c4 ) > 0 )  // if == r4noCreate, just pass through
                        error4( c4, e4create, E91102 ) ;
               #endif
         #endif
      }



      #ifndef S4OFF_TRAN
         /* !S4OFF_WRITE, !S4CLIENT, !S4OFF_TRAN */
         int d4logCreate( CODE4 *c4, const char *name, const FIELD4INFO *fieldData, const TAG4INFO *tagInfo )
         {
            // this function logs the 'create' message into the transaction log file

            TRAN4 *trans = code4trans( c4 ) ;
            int tranCode = TRAN4CREATE ;

            short nameLen = strlen(name ) ;
            int dataLen = nameLen + 8 ;  // extra 8 bytes for 3 shorts representing compatibility, # fields and # tags and sizeof of name length

            // calculate the total length of data (field4info plust tag4info
            short fieldCounter = 0 ;
            for ( ;; fieldCounter++ )
            {
               if ( fieldData[fieldCounter].name == 0 )
                  break ;
               dataLen += 1 ;  // size of length of name (max 10)
               dataLen += strlen( fieldData[fieldCounter].name ) ;   // room for name
               assert5( sizeof( fieldData[fieldCounter].type ) == 2 ) ;
               assert5( sizeof( fieldData[fieldCounter].len ) == 2 ) ;
               assert5( sizeof( fieldData[fieldCounter].dec ) == 2 ) ;
               assert5( sizeof( fieldData[fieldCounter].nulls ) == 2 ) ;
               dataLen += 4 * sizeof( short ) ;  // room for type, len, dec, nulls
            }

            short tagCounter = 0 ;

            if ( tagInfo != 0 )
            {
               for ( ;; tagCounter++ )
               {
                  if ( tagInfo[tagCounter].name == 0 )
                     break ;
                  dataLen += 1 ;  // size of name
                  dataLen += strlen( tagInfo[tagCounter].name ) ;
                  dataLen += 4  ;   // size for expression and filter
                  if ( tagInfo[tagCounter].expression )  // may be null with DEL4REUSE
                     dataLen += strlen( tagInfo[tagCounter].expression ) ;
                  if ( tagInfo[tagCounter].filter )
                     dataLen += strlen( tagInfo[tagCounter].filter ) ;
                  dataLen += 2 * sizeof( short ) ;   // unique and descending
                  assert5( sizeof(tagInfo[tagCounter].unique) == 2 ) ;
                  assert5( sizeof(tagInfo[tagCounter].descending) == 2 ) ;
               }
            }

            S4LONG connectionId ;
            #ifdef S4STAND_ALONE
               connectionId = 0L ;
            #else
               connectionId = c4->currentClient->id ;
            #endif
            // AS 02/05/01 - There is no client/server id on create since open also gets logged.
            if ( tran4set( trans, trans->currentTranStatus, -1L, connectionId, tranCode, (unsigned)dataLen, -1, -1 ) < 0 )
               return error4code( c4 ) ;
            assert5( sizeof( c4->compatibility ) == sizeof( short ) ) ;
            tran4putData( trans, &c4->compatibility, sizeof( short ) ) ;
            assert5( sizeof( nameLen ) == sizeof( short ) ) ;   // in tran file, 2 bytes for length
            tran4putData( trans, &nameLen, sizeof( short ) ) ;
            tran4putData( trans, name, (unsigned)nameLen ) ;

            //
            tran4putData( trans, &fieldCounter, sizeof( fieldCounter ) ) ;
            tran4putData( trans, &tagCounter, sizeof( tagCounter ) ) ;

            for ( fieldCounter = 0 ;; fieldCounter++ )
            {
               if ( fieldData[fieldCounter].name == 0 )
                  break ;
               char nLen = strlen( fieldData[fieldCounter].name ) ;
               assert5( nLen >= 0 ) ;
               tran4putData( trans, &nLen, sizeof( nLen ) ) ;
               tran4putData( trans, fieldData[fieldCounter].name, nLen ) ;
               tran4putData( trans, &(fieldData[fieldCounter].type), sizeof(fieldData[fieldCounter].type) ) ;
               tran4putData( trans, &(fieldData[fieldCounter].len), sizeof(fieldData[fieldCounter].len) ) ;
               tran4putData( trans, &(fieldData[fieldCounter].dec), sizeof(fieldData[fieldCounter].dec) ) ;
               tran4putData( trans, &(fieldData[fieldCounter].nulls), sizeof(fieldData[fieldCounter].nulls) ) ;
            }

            if ( tagCounter > 0 )
            {
               for ( tagCounter = 0 ;; tagCounter++ )
               {
                  if ( tagInfo[tagCounter].name == 0 )
                     break ;
                  char nLen = strlen( tagInfo[tagCounter].name ) ;
                  tran4putData( trans, &nLen, sizeof( nLen ) ) ;
                  tran4putData( trans, tagInfo[tagCounter].name, nLen ) ;
                  short eLen ;
                  if ( tagInfo[tagCounter].expression == 0 )
                     eLen = 0 ;
                  else
                     eLen = strlen( tagInfo[tagCounter].expression ) ;
                  tran4putData( trans, &eLen, sizeof( eLen ) ) ;
                  if ( eLen != 0 )
                     tran4putData( trans, tagInfo[tagCounter].expression, eLen ) ;
                  if ( tagInfo[tagCounter].filter == 0 )
                     eLen = 0 ;
                  else
                     eLen = strlen( tagInfo[tagCounter].filter ) ;
                  tran4putData( trans, &eLen, sizeof( eLen ) ) ;
                  if ( eLen != 0 )
                      tran4putData( trans, tagInfo[tagCounter].filter, eLen ) ;
                  tran4putData( trans, &tagInfo[tagCounter].unique, sizeof(tagInfo[tagCounter].unique) ) ;
                  tran4putData( trans, &tagInfo[tagCounter].descending, sizeof(tagInfo[tagCounter].descending) ) ;
               }
            }

            if ( tran4lowAppend( trans, 0, 0 ) != 0 )
               return e4transAppend ;
            return 0 ;
         }
      #endif



      /* !S4OFF_WRITE, !S4CLIENT */
      int dfile4create( CODE4 *c4, const char *name, const FIELD4INFO *fieldData, const TAG4INFO *tagInfo, DATA4 **temp )
      {
         #ifdef E4VBASIC
            if ( c4parm_check( c4, 1, E91102 ) )
               return -1 ;
         #endif  /* E4VBASIC */

         #ifdef E4PARM_LOW
            if ( c4 == 0 || fieldData == 0 )
               return error4( c4, e4parm_null, E91102 ) ;
         #endif

         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         error4set( c4, 0 ) ;  // get rid of any positive code

         Bool5 createTemp ;
         char buf[258] ;

         if ( name != 0 )
         {
            format4createName( buf, sizeof( buf ), name ) ;
            int rc = dfile4verifyNotAlreadyOpen( c4, buf ) ;
            if ( rc < 0 )
               return rc ;
            createTemp = 0 ;
         }
         else
            createTemp = 1 ;

         int needsMemo, numNulls ;
         unsigned short nFlds ;
         Bool5 hasAutoIncrement = 0 ;
         S4LONG calcRecordLen = calculateRecordLength( c4, fieldData, &nFlds, &numNulls, &needsMemo, &hasAutoIncrement ) ;
         if ( calcRecordLen < 0 )
            return (int)calcRecordLen ;

         S4LONG lheaderLen = calculateHeaderLength( c4, nFlds, numNulls ) ;
         if ( lheaderLen < 0 )
            return (int)lheaderLen ;

         FILE4 file ;
         char *tempName ;
         int tempFreeSet ;
         int rc = dfile4createFile( c4, &file, buf, createTemp, &tempName, &tempFreeSet ) ;
         if ( rc != 0 )  /* either error or r4noCreate, etc. */
            return rc ;

         /* Write the header */
         int hasMemo ;
         #if defined( S4CLIENT_OR_FOX ) && defined( S4COMPRESS )  /* LY 2002/12/27 */
            assert5port( "added compressed memo entries support" ) ;
            assert5port( "added compressed data file support" ) ;
            rc = dfile4createHeader( c4, needsMemo, &file, calcRecordLen, nFlds, fieldData, numNulls, &hasMemo, hasAutoIncrement, c4->compressedMemos, c4->compressedData ) ;
         #else
            rc = dfile4createHeader( c4, needsMemo, &file, calcRecordLen, nFlds, fieldData, numNulls, &hasMemo, hasAutoIncrement, 0, 0 ) ;
         #endif
         if ( rc < 0 )
         {
            /* force the file to not be created in case of error */
            file4setTemporary( &file, 1, 0 ) ;  // it won't be registed in validation table yet
         }
         else
         {
            if ( tempFreeSet == 1 )
            {
               file.doAllocFree = 0 ;
               file4setTemporary( &file, 0, 0 ) ;  // it won't be registed in validation table yet
            }
         }

         file4close( &file ) ;

         if ( rc < 0 )
            return rc ;

         #ifndef S4OFF_MEMO
            if ( hasMemo )
            {
               int oldCreateTemp = c4->createTemp ;
               c4->createTemp = 0 ;
               MEMO4FILE m4file ;

               if ( name == 0 )
                  rc = memo4fileCreate( &m4file, c4, 0, tempName ) ;
               else
               {
                  u4nameExt( buf, sizeof(buf), MEMO4EXT, 1 ) ;
                  rc = memo4fileCreate( &m4file, c4, 0, buf ) ;
               }
               c4->createTemp = oldCreateTemp ;
               if ( rc == 0 )
                  file4close( &m4file.file ) ;
            }
         #endif

         if ( rc < 0 )
         {
            if ( tempFreeSet == 1 && tempName != 0 )
               u4free( tempName ) ;
            return error4stack( c4, (short)rc, E91102 ) ;
         }

         int oldAutoOpen = c4->autoOpen ;

         #ifndef S4OFF_MULTI
            int oldAccessMode = c4->accessMode ;
            #ifdef E4DEBUG_SHARE
               c4->accessMode = OPEN4DENY_NONE ;
            #else
               c4->accessMode = OPEN4DENY_RW ;
            #endif
         #endif

         if ( tagInfo == 0 )
            c4->autoOpen = 0 ;
         else
         {
            if ( tagInfo[0].name == 0 )
               c4->autoOpen = 0 ;
         }
         #ifdef S4CLIPPER
            c4->autoOpen = 0 ;
         #endif

         #ifndef S4OFF_TRAN
            // AS 01/31/01 Want to disable the open message on a table create...
            int oldStatus ;
//            if ( name == 0 || c4->createTemp == 1 )
//            {
               oldStatus = code4tranStatus( c4 ) ;
               code4tranStatusSet( c4, r4off ) ;
//            }
//            else
//               oldStatus = 0 ;
         #endif

         DATA4 *data ;

         #ifndef S4CLIENT
            // AS Nov 27/02 - need to track open-for-create for data file compression (do we initialize)
            c4->openForCreate = 1 ;
         #endif

         if ( name == 0 )
            data = d4open( c4, tempName ) ;
         else
            data = d4open( c4, name ) ;

         #ifndef S4CLIENT
            c4->openForCreate = 0 ;
         #endif

         if ( tempFreeSet == 1 && tempName != 0 )
            u4free( tempName ) ;

         // AS 01/31/01 Want to disable the open message on a table create...
         #ifndef S4OFF_TRAN
            code4tranStatusSet( c4, oldStatus ) ;
         #endif

         if ( createTemp == 1 || c4->createTemp == 1 )
         {
            if ( data != 0 )
            {
               if ( tempFreeSet == 1 )
                  if ( data->dataFile->file.name != 0 )
                     data->dataFile->file.doAllocFree = 1 ;
               // AS Oct 23/02 - it is registered already after all
               if ( c4->createTemp == 1 )
                  file4setTemporary( &data->dataFile->file, 1, 1 ) ;  // it won't be registed in validation table yet
               #ifndef S4OFF_TRAN
                  data->logVal = LOG4TRANS ;
               #endif
               #ifndef S4OFF_MEMO
                  // AS 04/10/01 if createTemp is true but c4->createTemp is false, we leave
                  // file intact (see a few lines above where we do the same with
                  // datafile).  This is due to a later re-opening and setting on
                  // the file
                  // if ( hasMemo && ( c4->createTemp == 1 || createTemp == 1 ) )
                  if ( hasMemo && c4->createTemp == 1 )
                  {
                     #ifdef E4ANALYZE
                        if ( data->dataFile->memoFile.file.hand == INVALID4HANDLE )
                           error4( 0, e4struct, E91102 ) ;
                     #endif
                     file4setTemporary( &data->dataFile->memoFile.file, 1, 0 ) ;
                  }
               #endif
            }
         }
         else
         {
            // AS Dec 6/02 - avoid gpf if data is null
            if ( data != 0 )
            {
               // AS Oct 29/02 - was not setting the log flag correctly in client/server
               #ifndef S4OFF_TRAN
                  #ifndef S4CLIENT
                     if ( code4transEnabled( data->codeBase ) == 1 )
                        data->logVal = c4->log ;
                  #endif
               #endif
            }
         }

         c4->autoOpen = oldAutoOpen ;
         if ( data == 0 )
            error4( c4, e4open, E91102 ) ;
         else
            dfile4createIndex( data, tagInfo, name ) ;

         #ifndef S4OFF_MULTI
            c4->accessMode = oldAccessMode ;
         #endif
         rc = error4code( c4 ) ;
         if ( rc != 0 || data == 0 )
         {
            d4createClose( c4, data, 1 ) ;
            data = 0 ;
         }
         else
         {
            // AS 09/22/00 - for backup-recovery, allow logging of table creation for non-temporary files...
            #ifndef S4OFF_TRAN
               // AS 02/02/01 - We want to log creates when 'name' == 0 in the case of OLE-DB
               // where the file is being created as non-temp (but lets the system define a name)
               Bool5 logTrans = 0 ;
               if ( data != 0 && code4transEnabled( c4 ) )
               {
                  if (  createTemp == 1 && file4getTemporary( &data->dataFile->file ) == 0 )
                  {
                     // non temporary, set the name so that it gets inserted properly
                     name = data->dataFile->file.name ;
                  }

                  if ( c4->createTemp == 0 && createTemp != 1 )
                     logTrans = 1 ;
               }

               if ( logTrans )
               {
                  rc = d4logCreate( c4, data->dataFile->file.name, fieldData, tagInfo ) ;
               }
            #endif /* !S4OFF_TRAN */

            if ( ( c4->createTemp != 1 && createTemp == 0 ) || temp == 0 )  // if temp is NULL must close even if temporary
            {
               DATA4FILE *dfile = data->dataFile ;
               #ifdef S4SERVER
                  int oldKeepOpen = c4->server->keepOpen ;
                  c4->server->keepOpen = 1 ;
               #endif
               d4createClose( c4, data, 0 ) ;
               // don't log since closed anyway
               // #ifndef S4OFF_TRAN
               //    if ( logTrans && rc == 0 )
               //    {
               //       // AS 02/02/01 - Also mark the data as open logged so that the close gets marked
               //       assert5( data->openWasLogged == 0 ) ;  // should not be logging before create
               //
               //       d4openConcludeSetupTransactions( data ) ;
               //    }
               // #endif
               #ifdef S4SERVER
                  c4->server->keepOpen = oldKeepOpen ;
                  /* the server case requires an explict low close as well */
                  dfile4closeLow( dfile ) ;
               #endif
               data = 0 ;  // AS not valid anymore...
            }
            else
            {
               *temp = data ;
               #ifndef S4OFF_TRAN
                  if ( logTrans && rc == 0 )
                  {
                     // AS 02/02/01 - Also mark the data as open logged so that the close gets marked
                     assert5( data->openWasLogged == 0 ) ;  // should not be logging before create

                     d4openConcludeSetupTransactions( data ) ;
                  }
               #endif
            }
         }

         return error4code( c4 ) ;
      }
   #endif  /* not S4CLIENT */
#endif  /* S4OFF_WRITE */


// AS Nov 26/02 - New function for data file compression
#if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   /* !S4OFF_WRITE, S4STAND_ALONE, S4FOX */
   static DATA4 *d4compressTempCopy( DATA4 *from )
   {
      // makes a temporary copy of the from table, performing a field by field copy, and compressing
      // the memo field if required.  This is required in many instances such as updating the file
      // from a 2.5 file to a 3.0 file for example where the field widths change.
      CODE4 *c4 = from->codeBase ;

      FIELD4INFO *fieldsFrom = d4fieldInfo( from ) ;
      if ( fieldsFrom == 0 )
         return 0 ;

      int oldCompatibility = c4getCompatibility( c4 ) ;
      c4setCompatibility( c4, 30 ) ;  // require for compressed files
      // AS Jan 2/03 - Also need to take CodePage into account
      int oldCodePage = c4->codePage ;
      c4->codePage = from->dataFile->codePage ;
      DATA4 *tempData = d4createTemp( c4, fieldsFrom, 0 ) ;
      c4->codePage = oldCodePage ;
      c4setCompatibility( c4, oldCompatibility ) ;
      u4free( fieldsFrom ) ;

      if ( tempData == 0 )
         return 0 ;

      int rc = d4top( from ) ;

      while ( rc == 0 )
      {
         rc = d4appendStart( tempData, 0 ) ;
         if ( rc != 0 )
            break ;

         int nFields = d4numFields( tempData ) ;
         for ( int fldLoop = 0 ; fldLoop < nFields ; fldLoop++ )
         {
            FIELD4 *fieldFrom = d4fieldJ( from, fldLoop + 1 ) ;
            FIELD4 *fieldTo = d4fieldJ( tempData, fldLoop + 1 ) ;
            f4memoAssignField( fieldTo, fieldFrom ) ;
         }

         rc = d4append( tempData ) ;
         if ( rc != 0 )
            break ;

         rc = d4skip( from, 1 ) ;
      }

      if ( rc == r4eof )  // means successfully copied all records
         rc = 0 ;

      if ( rc != 0 )
      {
         d4close( tempData ) ;
         return 0 ;
      }

      if ( from->dataFile->openMdx == 1 )
      {
         INDEX4 *index = d4index( from, 0 ) ;
         if ( index == 0 )
         {
            d4close( tempData ) ;
            u4free( fieldsFrom ) ;
            return 0 ;
         }
         TAG4INFO *tags = i4tagInfo( index ) ;
         if ( tags == 0 )
         {
            d4close( tempData ) ;
            u4free( fieldsFrom ) ;
            u4free( tags ) ;
            return 0 ;
         }
         int oldCreateTemp = c4->createTemp ;
         c4->createTemp = 1 ;
         if ( i4create( tempData, 0, tags ) == 0 )
         {
            d4close( tempData ) ;
            u4free( fieldsFrom ) ;
            u4free( tags ) ;
            return 0 ;
         }
         c4->createTemp = oldCreateTemp ;
         u4free( tags ) ;
      }

      return tempData ;
   }



   /* !S4OFF_WRITE, S4STAND_ALONE, S4FOX */
   static DATA4 *d4compressLow( DATA4 **fromInOut, const char *compressName, short blockSize, Bool5 recreateMemos )
   {
      // if a temporary table is created and exported, it is put into the fromIn input/output paramater,
      // and the caller must close the temp. table.
      assert5port( "function for d4compress" ) ;
      DATA4 *from = *fromInOut ;
      CODE4 *c4 = from->codeBase ;
      FIELD4INFO *fieldsFrom = d4fieldInfo( from ) ;
      if ( fieldsFrom == 0 )
         return 0 ;

      int oldCompatibility = c4getCompatibility( c4 ) ;
      c4setCompatibility( c4, 30 ) ;  // require for compressed files
      assert5( c4->compressedData == 0 ) ;  // only to be changed in this call...
      c4->compressedData = 1 ;
      // AS Jan 2/03 - Also need to take CodePage into account
      int oldCodePage = c4->codePage ;
      c4->codePage = from->dataFile->codePage ;
      DATA4 *newData = d4create( c4, compressName, fieldsFrom, 0 ) ;
      c4->codePage = oldCodePage ;
      c4->compressedData = 0 ;
      c4setCompatibility( c4, oldCompatibility ) ;
      u4free( fieldsFrom ) ;

      if ( newData == 0 )
         return 0 ;

      d4lockFile( newData ) ;

      Bool5 assignByField = 0 ;  // do we need to assign field by field?
      unsigned long recWidthFrom = d4recWidth( from ) ;

      if ( recreateMemos == 1 )   // to create new compressed memos, must assign memo fields individually
         assignByField = 1 ;
      else if ( recWidthFrom != d4recWidth( newData ) )
      {
         // means field lengths don't match.  This is ok, probably converting file formats.
         // in this case, we need to assign field by field, so set the field assign flag to true.
         assignByField = 1 ;
      }

      DATA4 *tempData = 0 ;

      if ( assignByField == 1 )
      {
         // in this case, create a temporary table first to assign the data, and then compress the
         // results.
         // This is not the most efficient approach but this is more of a special case situation.
         tempData = d4compressTempCopy( from ) ;
         if ( tempData == 0 )
         {
            d4remove( newData ) ;
            return 0 ;
         }
         from = tempData ;
         recWidthFrom = d4recWidth( tempData ) ;
         assert5( recWidthFrom == d4recWidth( newData ) ) ;  // better be the same after the copy

      }

      DATA4FILE *newDataFile = newData->dataFile ;

      // now calculate the compressed header info
      // AS Jan 3/03 If the data file is empty, there are no compressed offsets.
      long count = d4recCount( from ) ;
      long numOffsets = (count * recWidthFrom / (blockSize*1024)) + 1 ;
      long sizeInfo = sizeof( COMPRESS4DATA ) + (numOffsets - 1) * sizeof( long ) ;  // -1 because COMPRESS4DATA structure contains the first entry
      #ifdef S4PREPROCESS_FILE
         // ensure sizeInfo is on a block boundary for reading/writing - makes much more efficient
         if ( newDataFile->file.preprocessed == 1 )
         {
            short blockSize = code4getPreprocessBlockSize( c4 ) ;
            if ( ( sizeInfo % blockSize ) != 0 )
            {
               sizeInfo += (blockSize - ( sizeInfo % blockSize ) ) ;
            }
         }
      #endif
      newDataFile->compressInfo = (COMPRESS4DATA *)u4allocFree( c4, sizeInfo ) ;
      if ( newDataFile->compressInfo == 0 )
      {
         d4remove( newData ) ;
         if ( tempData != 0 )
            d4close( tempData ) ;
         return 0 ;
      }

      newDataFile->compressInfo->version = 1 ;
      newDataFile->compressInfo->blockSize = blockSize ;
      newDataFile->compressInfo->numOffsets = numOffsets ;
      newDataFile->compressInfo->fileLen = file4len( &(from->dataFile->file) ) ;

      long headerFilePos = file4len( &newDataFile->file ) ;
      long fileOffset = headerFilePos + sizeInfo ;  // where to start writing to data file

      // buffer and offset into compression buffer - when it is full we compress it.
      long uncompressedOffset = 0 ;
      long uncompressedBufSize = blockSize * 1024 ;
      unsigned char *uncompressedBuffer = (unsigned char *)u4allocFree( c4, uncompressedBufSize ) ;
      long compressedBufSize = (long)(uncompressedBufSize * 1.01) + 12 ;
      unsigned char *compressedBuffer = (unsigned char *)u4allocFree( c4, compressedBufSize ) ;
      if ( uncompressedBuffer == 0 || compressedBuffer == 0 )
      {
         if ( uncompressedBuffer != 0 )
            u4free( uncompressedBuffer ) ;
         if ( compressedBuffer != 0 )
            u4free( compressedBuffer ) ;
         d4remove( newData ) ;
         if ( tempData != 0 )
            d4close( tempData ) ;
         return 0 ;
      }
      long bufferCount = 0 ;  // which buffer entry are we on?

      int rc = d4top( from ) ;

      while ( rc == 0 )
      {
         rc = d4appendStart( newData, 0 ) ;
         if ( rc != 0 )
            break ;

         long lenLeft = recWidthFrom ;
         int recOffset = 0 ;
         char *fromRecPtr = d4record( from ) ;
         while ( lenLeft > 0 )
         {
            long lenToCopy = min( lenLeft, uncompressedBufSize - uncompressedOffset ) ;
            memcpy( uncompressedBuffer + uncompressedOffset, fromRecPtr + recOffset, lenToCopy ) ;
            uncompressedOffset += lenToCopy ;
            if ( uncompressedOffset == uncompressedBufSize )  // buffer is full, compress, write, and reset
            {
               newDataFile->compressInfo->offsets[bufferCount] = fileOffset ;
               unsigned long outLen = (unsigned long)compressedBufSize ;
               rc = c4compress( c4, compressedBuffer, &outLen, uncompressedBuffer, uncompressedBufSize, 9, 0 ) ;
               if ( rc != 0 )
                  break ;
               bufferCount++ ;
               assert5( bufferCount <= newDataFile->compressInfo->numOffsets ) ;
               uncompressedOffset = 0 ;
               #ifdef S4PREPROCESS_FILE
                  if ( newDataFile->file.preprocessed == 1 )
                  {
                     // AS May 24/02 - Modify the record length for preprocessing - the record must lie on the block
                     // byte boundary
                     short blockSize = code4getPreprocessBlockSize( c4 ) ;
                     if ( ( outLen % blockSize ) != 0 )
                     {
                        outLen += (blockSize - ( outLen % blockSize ) ) ;
                     }
                  }
               #endif
               rc = file4write( &newDataFile->file, fileOffset, compressedBuffer, outLen ) ;
               if ( rc != 0 )
                  break ;
               fileOffset += outLen ;
            }
            lenLeft -= lenToCopy ;
            recOffset += lenToCopy ;
         }

         rc = d4skip( from, 1 ) ;
      }


      if ( rc == r4eof )  // reached eof successfully
         rc = 0 ;

      if ( rc == 0 && uncompressedOffset != 0 )
      {
         // compress the last piece - use nulls to fill up the buffer
         memset( uncompressedBuffer + uncompressedOffset, 0, uncompressedBufSize - uncompressedOffset ) ;
         unsigned long outLen = (unsigned long)compressedBufSize ;
         rc = c4compress( c4, compressedBuffer, &outLen, uncompressedBuffer, uncompressedBufSize, 9, 0 ) ;
         if ( rc == 0 )
         {
            #ifdef S4PREPROCESS_FILE
               if ( newDataFile->file.preprocessed == 1 )
               {
                  // AS May 24/02 - Modify the record length for preprocessing - the record must lie on the block
                  // byte boundary
                  short blockSize = code4getPreprocessBlockSize( c4 ) ;
                  if ( ( outLen % blockSize ) != 0 )
                     outLen += (blockSize - ( outLen % blockSize ) ) ;
               }
            #endif
            rc = file4write( &newDataFile->file, fileOffset, compressedBuffer, outLen ) ;
         }
         newDataFile->compressInfo->offsets[bufferCount] = fileOffset ;
         bufferCount++ ;
         assert5( bufferCount == newDataFile->compressInfo->numOffsets ) ;
      }

      if ( rc == 0 )
      {
         // now write out the header information...
         rc = file4write( &newDataFile->file, headerFilePos, newDataFile->compressInfo, sizeInfo ) ;
         if ( rc == 0 )  // update the # of records
         {
            newDataFile->numRecs = d4recCount( from ) ;
            dfile4updateHeader( newDataFile, 0, 1, 0 ) ;
         }
      }

      u4free( uncompressedBuffer ) ;
      u4free( compressedBuffer ) ;

      if ( rc != 0 )
      {
         d4remove( newData ) ;
         if ( tempData != 0 )
            d4close( tempData ) ;
         return 0 ;
      }

      if ( tempData != 0 )
         *fromInOut = tempData ;

      return newData ;
   }
#endif /* defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) */



#ifndef S4LUPACH  /* LY 2003/07/07 */
/* !S4OFF_WRITE, S4STAND_ALONE, S4FOX */
DATA4 * S4FUNCTION d4compress( DATA4 *fromIn, const char *compressName, short blockSize )
{
   assert5port( "function for d4compress" ) ;
   #ifdef E4VBASIC
      if ( c4parm_check( fromIn, 2, E96701 ) )
         return 0 ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( fromIn == 0 || compressName == 0 || blockSize < 0 )
      {
         error4( 0, e4parm_null, E96701 ) ;
         return 0 ;
      }
   #endif

   // AS Jan 3/03 - If the table is empty, return a failure code
   if ( d4recCount( fromIn ) == 0 )
   {
      error4describe( fromIn->codeBase, e4notSupported, E96701, "Compressed of a table with no records not supported for table: ", compressName, 0 ) ;
      return 0 ;
   }

   if ( blockSize == 0 ) // use a default setting, currently set at 32k
      blockSize = 32 ;

   CODE4 *c4 = fromIn->codeBase ;

   #if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      // lock data file to ensure no modifications occurring
      int rc = d4lockFile( fromIn ) ;
      if ( rc != 0 )
         return 0 ;

      char nameBuf[LEN4PATH+1] ;
      u4nameCurrent( nameBuf, sizeof( nameBuf ), compressName ) ;

      // and compress the data file

      // check if we need to copy memo file over or create a new compressed version
      Bool5 recreateMemo = 0 ;
      #ifndef S4OFF_MEMO
         if ( fromIn->dataFile->nFieldsMemo > 0 && c4->compressedMemos == 1 && fromIn->dataFile->compressedMemosSupported == 0 )  // requesting to compress
            recreateMemo = 1 ;
      #endif

      // compress the data file
      // from on output will be the temptable if changed
      DATA4 *from = fromIn ;
      DATA4 *newData = d4compressLow( &from, compressName, blockSize, recreateMemo ) ;
      if ( newData == 0 )
         return 0 ;

      // copy the index files over (they should be unchanged)
      #ifndef S4OFF_MEMO
         if ( from->dataFile->nFieldsMemo > 0 )  // copy over
         {
            u4nameExt( nameBuf, sizeof( nameBuf ), MEMO4EXT, 1 ) ;

            // rc = file4createInternal( &newData->dataFile->memoFile.file, c4, nameBuf, 1, OPT4OTHER ) ;
            assert5( newData->dataFile->memoFile.file.hand != 0 ) ;
            if ( rc == 0 )
               rc = file4copy( &newData->dataFile->memoFile.file, &from->dataFile->memoFile.file ) ;
            if ( rc != 0 )
            {
               d4remove( newData ) ;
               if ( from != fromIn )  // means from is a temp table, so close it
                  d4close( from ) ;
               return 0 ;
            }
         }
      #endif

      #ifndef S4OFF_INDEX
         if ( from->dataFile->openMdx == 1 )
         {
            INDEX4 *index = d4index( from, 0 ) ;
            if ( index == 0 )
            {
               d4remove( newData ) ;
               if ( from != fromIn )  // means from is a temp table, so close it
                  d4close( from ) ;
               return 0 ;
            }
            u4nameExt( nameBuf, sizeof( nameBuf ), INDEX4EXT, 1 ) ;

            FILE4 indexFile ;

            rc = file4createInternal( &indexFile, c4, nameBuf, 1, OPT4INDEX ) ;
            if ( rc == 0 )
               rc = file4copy( &indexFile, &index->indexFile->file ) ;
            if ( rc != 0 )
            {
               d4remove( newData ) ;
               if ( from != fromIn )  // means from is a temp table, so close it
                  d4close( from ) ;
               return 0 ;
            }
            newData->dataFile->hasMdxMemo |= 1 ;
            newData->dataFile->openMdx = 1 ;
            FILE4LONG pos ;
            file4longAssign( pos, ( 4 + sizeof( S4LONG ) + 2 * sizeof( short ) + sizeof( char[16] ) ), 0 ) ;
            #ifdef S4FOX
               file4writeInternal( &newData->dataFile->file, pos, &(newData->dataFile->hasMdxMemo), sizeof( char ) ) ;
            #else
               file4writeInternal( &newData->dataFile->file, pos, &(newData->dataFile->hasMdxMemo), sizeof( newData->dataFile->hasMdxMemo ) ) ;
            #endif
            file4close( &indexFile ) ;
         }
      #endif

      d4close( newData ) ;  // close so that indexes, etc. become available properly.
      if ( from != fromIn )  // means from is a temp table, so close it
         d4close( from ) ;

      int oldReadOnly = c4->readOnly ;
      c4->readOnly = 1 ;  // ensure file is open readonly mode
      newData = d4open( c4, compressName ) ;
      c4->readOnly = oldReadOnly ;
      return newData ;
   #else
      error4( c4, e4notSupported, E96701 ) ;
      return 0 ;
   #endif /* defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) */
}
#endif
