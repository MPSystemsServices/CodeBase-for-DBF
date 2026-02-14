/* d4servol.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* server ole-db related functions */

#include "d4all.h"

#ifdef S4SERVER
#ifdef OLEDB5BUILD
   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIntegratedTagSelect( SERVER4CLIENT *client )
   {
      // checks for tag and puts into RowsetValues if available...
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = client->server->c4 ;
      short iData = connect4receiveShort( connect ) ;
      short iRowset = connect4receiveShort( connect ) ;
      char *tagName = connect4receiveString( connect ) ;

      Table5serverBase *table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      Table5rowsetValues *rowsetValues ;
      if ( iRowset == 0 )  // need a Table5rowsetValues to track index tags...
      {
         rowsetValues = new Table5rowsetValues ;
         iRowset = client->rowsets.add( rowsetValues ) ;
         rowsetValues->table = table ;
         rowsetValues->fieldSet.init( table->table ) ;
      }
      else
         rowsetValues = client->rowsets.get( iRowset ) ;

      TAG4 *selectedTag ;
      short rc = 0 ;

      if ( tagName == 0 )
         selectedTag = 0 ;
      else
      {
         client->server->c4->errTagName = 0 ;
         selectedTag = d4tag( rowsetValues->table->data4(), tagName ) ;
         if ( selectedTag == 0 )
            rc = -1 ;
      }

      if ( rc == 0 )
         rowsetValues->selectedTag = selectedTag ;

      connect4sendShort( connect, rc ) ;
      connect4sendShort( connect, iRowset ) ;
      connect4sendFlush( connect ) ;
      u4free( tagName ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgDataFetchNextRecno( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iData = connect4receiveShort( connect ) ;
      long startRecNo = connect4receiveLong( connect ) ;
      short increment = connect4receiveShort( connect ) ;
      Table5rowsetValues *rowsetValues = client->rowsets.get( iData ) ;

      if ( rowsetValues->table == 0 || (increment != 1 && increment != -1) )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendLong( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      // AS 05/25/99 --> assume don't use with tag...
      assert5( rowsetValues->table->data4()->tagSelected == 0 ) ;
      long nextRecNo = rowsetValues->table->skip( startRecNo, increment, 0 ) ;

      connect4sendLong( connect, nextRecNo ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgDataSchemaRequestDirect( SERVER4CLIENT *client )
   {
      // called when client must retrieve schema raw buffer from server.  Occurs on a refetch of a row
      // because of a changed fieldset for a schema table...

      CONNECT4 *connect = &client->connect ;
      short iRowset = connect4receiveShort( connect ) ;
      Table5rowsetValues *rowsetValues = client->rowsets.get( iRowset ) ;
      long recNo = connect4receiveLong( connect ) ;

      if ( rowsetValues->table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendLong( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      short rc = rowsetValues->table->goRead( recNo, rowsetValues ) ;
      connect4sendShort( connect, rc ) ;
      if ( rc == 0 )
      {
         char *recBuf = rowsetValues->table->data4()->record ;
         rowsetValues->fieldSet.sendNonDeferredFields( connect, recBuf ) ;
      }
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgDataAddColumn( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iData ;
      FIELD4INFO fields[2] ;
      memset( fields, 0, sizeof( fields ) ) ;
      long rc = 0 ;

      iData = connect4receiveShort( connect ) ;

      Table5serverBase *table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendLong( connect, E_FAIL ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      fields[0].name = connect4receiveString( connect ) ;
      fields[0].type = connect4receiveShort( connect ) ;
      fields[0].len = connect4receiveShort( connect ) ;
      fields[0].dec = connect4receiveShort( connect ) ;
      fields[0].nulls = connect4receiveShort( connect ) ;

      try
      {
         #ifndef S4OFF_SECURITY
            DATA4 *data = table->data4() ;
            if ( data == 0 )  // invalid...
            {
               // bad data, must delete client
               error4( connect->cb, e4connect, E91530 ) ;
               connect4sendLong( connect, E_FAIL ) ;
               connect4sendFlush( connect ) ;
               return ;
            }
            if ( file4getTemporary( &data->dataFile->file ) == 0 )  // don't disallow adding columns to temp files
               if ( account4userAllowCreate( &client->account, 0, TRUE ) == FALSE )
                  throw Err5oledb( DB_SEC_E_PERMISSIONDENIED, connect->cb ) ;
         #endif

         table->table->addColumns( 1, fields ) ;
      }
      catch( Err5oledb &err )
      {
         rc = err.oledbCode() ;
      }

      // AS 01/22/01 - Was not freeing up this info, so was not getting regenerated, so was incorrect
      if ( rc == 0 )
         table->table->freeColumnInfo() ;

      connect4sendLong( connect, rc ) ;
      connect4sendFlush( connect ) ;

      u4free( fields[0].name ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgDataRemoveColumn( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iData ;
      long rc = 0 ;

      iData = connect4receiveShort( connect ) ;

      Table5serverBase *table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      char *colName = connect4receiveString( connect ) ;

      try
      {
         table->table->removeColumn( colName ) ;
      }
      catch( Err5oledb &err )
      {
         rc = err.oledbCode() ;
      }

      if ( rc == 0 && error4code( connect->cb ) < 0 ) /* internal failure */
      {
         rc = E_FAIL ;
         error4set( connect->cb, 0 ) ;
      }

      // AS 01/22/01 - Was not freeing up this info, so was not getting regenerated, so was incorrect
      if ( rc == 0 )
         table->table->freeColumnInfo() ;

      connect4sendLong( connect, rc ) ;
      connect4sendFlush( connect ) ;

      u4free( colName ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgInTransaction( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;

      client->session->setInTransaction( connect4receiveChar( connect ) ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIsoLevel( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;

      ISOLEVEL lev ;
      connect4receive( connect, &lev, sizeof( ISOLEVEL ), code4timeoutVal( connect->cb ) ) ;
      if ( client->session == 0 )   // this is the 1st OLE-DB call from the client
      {
         CODE4 *c4 = client->server->c4 ;
         client->session = new Session5( c4 ) ;
         client->tables.session = client->session ;
         client->session->init( &client->server->source ) ;
         assert5( client->session != 0 ) ;
      }
      assert5( client->session->source != 0 ) ;
      client->session->isolationLevel( lev ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgDataClose( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *table = client->tables.remove( iData ) ;

      if ( table != 0 )  // schema tables are not removed
      {
         // AS 09/28/99 --> was not removing from the sessions list of open tables...
         client->session->openTables.remove( table->table ) ;

         delete table->table ;
         delete table ;
      }
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgTableWriteResultInc( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *table = client->tables.get( iData ) ;

      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      table->table->writeResultPositionIncrement() ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgRequestDeferredField( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
   //   Table5serverBase *table ;
      Table5rowsetValues *rowset ;
      short iRowset ;

      iRowset = connect4receiveShort( connect ) ;

      rowset = client->rowsets.get( iRowset ) ;
      if ( rowset == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      long recNo = connect4receiveLong( connect ) ;

      // AS 01/22/01 - it is ok if the 'rc' is 'r4deleted', which occurs on rows with deleted exposed
      int rc = rowset->table->go( recNo ) ;
      if ( rc != 0 && rc != r4deleted )  // means the record no longer valid -- should not happen
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      connect4sendShort( connect, rc ) ;

      rowset->fieldSet.sendDeferredFields( connect ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgTableRemoveIndexes( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *table = client->tables.get( iData ) ;

      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      short rc = 0 ;
      try
      {
         table->table->removeIndexes() ;
      }
      catch( Err5oledb& )
      {
         rc = -1 ;
      }

      connect4sendShort( connect, rc ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgTableRemove( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *table  = client->tables.get( iData ) ;

      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      short rc = 0 ;
      try
      {
         Source5all *source = &client->server->source ;
         if ( source->tablesSchema != 0 )  // remove the schema entry if available
            source->tablesSchema->removeTable( table->table ) ;
         table->table->remove() ;
      }
      catch( Err5oledb& )
      {
         rc = -1 ;
      }

      connect4sendShort( connect, rc ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIndexRequestKey( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iIndex = connect4receiveShort( connect ) ;
      Index5server *index = client->indexes.get( iIndex ) ;

      if ( index == 0 )
      {
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         return ;
      }

      long recNo = connect4receiveLong( connect ) ;

      index->refetchWithKey( recNo, index->currentKey ) ;
      connect4send( connect, index->currentKey, index->keyLen() ) ;

      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgRowRequestArr( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;

      short iData = connect4receiveShort( connect ) ;

      Table5rowsetValues *rowsetValues = client->rowsets.get( iData ) ;
      if ( rowsetValues->table == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      rowsetValues->exposeDeleted = connect4receiveChar( connect ) ;

      long numRecs = connect4receiveLong( connect ) ;

      char *recBuf = rowsetValues->table->data4()->record ;

      int rc = 0 ;

      for( long i = 0 ; i < numRecs ; i++ )
      {
         long recNo = connect4receiveLong( connect ) ;
         rc = rowsetValues->table->goRead( recNo, rowsetValues ) ;
         connect4sendShort( connect, rc ) ;
         switch( rc )
         {
            case r4success:
               break ;
            case r4locked:
               rc = 1 ;
               break ;
            default:
            case r4eof:
            case r4bof:
            case r4deleted:  // any of these are invalid rows
               rc = -1 ;
               break ;
         }

         if ( rc == -1 )
            continue ;

         connect4sendLong( connect, recNo ) ;

         if ( rc == 1 )  // means continue (don't send record info) (r4locked, r4deleted)
            continue ;

   //      if (!( table->fieldSet.nNonDeferredFields == 0 && table->fieldSet.nDeferredSend == 0 ))  // no fields to send, also don't send null bytes
         rowsetValues->fieldSet.sendNonDeferredFields( connect, recBuf ) ;
      }
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgRowsetDestroy( SERVER4CLIENT *client )
   {
      // destroy the rowset object...
      CONNECT4 *connect = &client->connect ;
      short iRowset = connect4receiveShort( connect ) ;
      Table5rowsetValues *rowset = client->rowsets.remove( iRowset ) ;

      if ( rowset == 0 )
      {
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      delete rowset ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSchemaPopulateIndexes( SERVER4CLIENT *client )
   {
      // destroy the rowset object...
      CONNECT4 *connect = &client->connect ;
      Source5all *source = &client->server->source ;
      WSTR5 *catalogKey = connect4receiveWideString( connect ) ;
      WSTR5 *tableKey = connect4receiveWideString( connect ) ;

      assert5( source->tablesSchema != 0 ) ;

      source->tablesSchema->populateIndexesRowset( catalogKey, tableKey ) ;
      u4free( tableKey ) ;
      u4free( catalogKey ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSchemaPopulateColumns( SERVER4CLIENT *client )
   {
      // destroy the rowset object...
      CONNECT4 *connect = &client->connect ;
      Source5all *source = &client->server->source ;
      WSTR5 *catalogKey = connect4receiveWideString( connect ) ;
      WSTR5 *tableKey = connect4receiveWideString( connect ) ;

      assert5( source->tablesSchema != 0 ) ;

      // AS 01/24/00 - was not catching potential errors here...
      try
      {
         source->tablesSchema->populateColumnsRowset( catalogKey, tableKey ) ;
      }
      catch( Err5oledb & )
      {
         // error4set( client->server->c4, 0 ) ;
         // it is a critical error if this happens, it means that the server somehow did
         // not handle the schema tables corectly.  disconnect the client...
         error4( client->server->c4, e4connect, E91530 ) ;
      }
      u4free( tableKey ) ;
      u4free( catalogKey ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSchemaRequestSeq( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;

      short iRowset = connect4receiveShort( connect ) ;
      Table5rowsetValues *rowset = client->rowsets.get( iRowset ) ;
      if ( rowset == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      Table5schemaServer *table = (Table5schemaServer *)(rowset->table->table) ;

      long startRecNo = connect4receiveLong( connect ) ;
      long offset = connect4receiveLong( connect ) ;
      long cRecNo = connect4receiveLong( connect ) ;
      rowset->exposeDeleted = connect4receiveChar( connect ) ;
      rowset->setStartRecno = connect4receiveLong( connect ) ;
      rowset->setEndRecno = connect4receiveLong( connect ) ;

      short rc = 0 ;
      long rc1 = 0L ;

      try
      {
         table->rowRequestSchemaSequential( startRecNo, offset, cRecNo, rowset, -1 ) ;
      }
      catch( Err5oledb &err )
      {
         rc1 = err.oledbCode() ;
      }

      connect4sendLong( connect, rc1 ) ;
      if ( rc1 < 0 || rc1 == DB_S_ENDOFROWSET )   // error or DB_S_ENDOFROWSET
      {
         connect4sendFlush( connect ) ;
         return ;
      }

      long uRecNo ;
      signed char increment ;

      // now send the records down...
      if( cRecNo < 0 )
      {
         increment = -1 ;
         uRecNo = -cRecNo ;
      }
      else
      {
         uRecNo = cRecNo ;
         increment = 1 ;
      }

      long recNo = startRecNo - increment;  // in case of bof/eof return ( no recno )
      long count ;
      int nextIsDone = 0 ;
      char *recBuf = table->data->record ;

      for( count = 0 ;; )
      {
         if ( count == uRecNo ) // then we are done..., don't send rc because client doesn't expect it when all records received
         {
            rc = r4done ;
            break ;
         }
         if ( nextIsDone == 1 )  // done, but send client r4done code then break
            rc = r4done ;
         else
         {
            rc = rowset->table->goRead( rowset->curRecNo, rowset ) ;
            if ( rc != r4eof && rc != r4done )
            {
               if ( rowset->setEndRecno == (unsigned long)rowset->curRecNo )
                  nextIsDone = 1 ;
            }
         }
         connect4sendShort( connect, rc ) ;
         switch( rc )
         {
            case r4success:
               break ;
            case r4locked:
               rc = 1 ;
               break ;
            case r4deleted:
               rc = 1 ;
               break ;
            case r4eof:
            case r4bof:
            default:
               rc = -1 ;
               break ;
         }
         if ( rc < 0 )  // severe type error or bof/eof
            break ;
         connect4sendLong( connect, rowset->curRecNo ) ;
         count++ ;

         if ( rc == 1 )  // means continue (don't send record info) (r4locked, r4deleted)
            continue ;

         rowset->fieldSet.sendNonDeferredFields( connect, recBuf ) ;

         if ( nextIsDone == 0 )
         {
            try
            {
               table->skip() ;
            }
            catch( Err5eof )
            {
               rc = r4eof ;
               if ( count != uRecNo ) // send eof and end, else user won't request anyway...
               {
                  connect4sendShort( connect, r4eof ) ;
                  break ;
               }
            }

            rowset->curRecNo = table->recNo() ;
         }
      }

      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgAddTag( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;
      short iData = connect4receiveShort( connect ) ;

      Table5serverBase *tblBase = client->tables.get( iData ) ;
      if ( tblBase == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         connect4sendLong(connect, E_FAIL) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      Table5server *table = (Table5server *)tblBase->table ;

      TAG4INFO tag[2] ;
      memset( &tag[1], 0, sizeof( TAG4INFO ) ) ;
      tag[0].name = connect4receiveString( connect ) ;

      short doSendName ;
      if ( tag[0].name == 0 )  // generate name
      {
         tag[0].name = table->generateTagName() ;
         doSendName = 1 ;
      }
      else
         doSendName = 0 ;

      char *expr = connect4receiveString( connect ) ;
      tag[0].expression = expr ;
      char *filt = connect4receiveString( connect ) ;
      tag[0].filter = filt ;
      tag[0].unique = connect4receiveShort( connect ) ;
      tag[0].descending = connect4receiveShort( connect ) ;

      HRESULT rc = 0 ;
      try
      {
         table->addTag( tag ) ;
      }
      catch( Err5oledb& err)
      {
         error4set( c4, 0 ) ;
         rc = err.oledbCode() ;
      }

      connect4sendLong( connect, rc ) ;

      if ( doSendName == 1 && rc == 0 )
         connect4sendString( connect, tag[0].name ) ;
      u4free( tag[0].name ) ;
      u4free( expr ) ;
      u4free( filt ) ;
      connect4sendFlush( connect ) ;

      // AS 05/05/99 - let client continue while the schemas are updated in the background...
      // previously was not updating this, do so now...
      if ( rc == 0 )
         if ( client->session->source->indexesSchema != 0 )
            client->session->source->indexesSchema->check( table ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgTagSelect( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *table = client->tables.get( iData ) ;

      if ( table == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      short tagAliasLen = connect4receiveShort( connect ) ;
      char *tagAlias = (char *)alloc5null( tagAliasLen + 2 ) ;  // wide string, 2 bytes at end for NULL
      connect4receive( connect, tagAlias, tagAliasLen, code4timeoutVal( c4 ) ) ;
      try
      {
         table->tagSelect( tagAlias, 0 ) ;
      }
      catch( Err5internal& )
      {
         // something seriously wrong, delete client
          // bad data, must delete client
          error4( c4, e4connect, E91530 ) ;
      }
      free5( tagAlias ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgFindOrAddEntry( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *table = client->tables.get( iData ) ;

      if ( table == 0 || iData != SCHEMA5ID_TABLES )  // only the tables table supports this function call...
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      WSTR5 catName[LEN5TABLE_CATALOG/2], tableName[LEN5TABLE_NAME/2] ;
      memset( tableName, 0, LEN5TABLE_NAME ) ;
      int rc = connect4receive( connect, catName, LEN5TABLE_CATALOG, code4timeoutVal( c4 ) ) ;
      if ( rc == 0 )
         rc = connect4receive( connect, tableName, LEN5TABLE_NAME, code4timeoutVal( c4 ) ) ;

      if ( rc != 0 || error4code( c4 ) < 0 )  // AS it has been known to happen, catch it here...
      {
          // bad data, must delete client
          error4( c4, e4connect, E91530 ) ;
          return ;
      }

      Table5tables *tbls = (Table5tables *)(table->table) ;
      try
      {
         // AS 06/10/99 -- catName must not be an empty string.  The client will send it's information,
         // but we need to combine that information with the current directory settings...
         char catNameChar[LEN4PATH] ;
         getCharPtrFromUnicode5( catName, catNameChar ) ;
         char curCatName[LEN4PATH] ;
         u4nameCurrent( curCatName, sizeof( curCatName ), catNameChar ) ;
         getUnicodeFromCharPtr5( curCatName, catName ) ;
         assert5( catName[0] != 0 ) ;  // it should at least include a drive letter, etc.
         tbls->findOrAddEntry( catName, tableName ) ;
      }
      catch( Err5oledb )
      {
          // bad data, must delete client
          error4( c4, e4connect, E91530 ) ;
          return ;
      }
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgAddDirectory( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *table = client->tables.get( iData ) ;
      short len = connect4receiveShort( connect ) ;

      if ( table == 0 || iData != SCHEMA5ID_TABLES || len > LEN4PATH * sizeof( short ) )  // only the tables table supports this function call...
      {
          // bad data, must delete client
          error4( c4, e4connect, E91530 ) ;
          return ;
      }

      WSTR5 pathName[LEN4PATH] ;
      connect4receive( connect, pathName, len, code4timeoutVal( c4 ) ) ;
      pathName[len/2] = 0 ;

      Table5tables *tbls = (Table5tables *)(table->table) ;
      try
      {
         tbls->addDirectory( pathName ) ;
      }
      catch( Err5oledb )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSetRestrictions( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;
      short iData = connect4receiveShort( connect ) ;
      Table5serverBase *tbl = client->tables.get( iData ) ;

      if ( tbl == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      Table5schemaServer *table = (Table5schemaServer *)(tbl->table) ;

      short keyLen = connect4receiveShort( connect ) ;
      char *key = 0 ;
      if ( keyLen != 0 )
      {
         key = (char *)alloc5null( keyLen ) ;
         connect4receive( connect, key, keyLen, code4timeoutVal( c4 ) ) ;
      }

      long endRecNo, startRecNo ;
      Schema5state state ;

      short rc = table->setRestrictions( &endRecNo, &startRecNo, key, keyLen, &state ) ;

      free5( key ) ;

      connect4sendShort( connect, rc ) ;

      if ( rc == 0 )
      {
         connect4send( connect, &state, sizeof( state ) ) ;
         if ( state != schema5eof )
         {
            connect4sendLong( connect, startRecNo ) ;
            connect4sendLong( connect, endRecNo ) ;
         }

         connect4sendFlush( connect ) ;
      }
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgCurrentDirectory( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      char curDir[LEN4PATH] ;

      u4nameCurrent( curDir, sizeof( curDir ), "" ) ;
      // AS 01/12/01 - Need to upper case this in Windows as it is sometimes
      // used (at least in tests) to form keys where all should be upper case
      #ifndef S4UNIX
         c4upper( curDir ) ;
      #endif
      short len = strlen( curDir ) ;
      connect4sendShort( connect, len ) ;
      connect4send( connect, curDir, len ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgReccount( SERVER4CLIENT *client )
   {
      Table5serverBase *table ;
      CONNECT4 *connect = &client->connect ;
      short iData ;

      iData = connect4receiveShort( connect ) ;
      table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         return ;
      }
      connect4sendLong( connect, d4recCount( table->data4() ) ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIndexDestroyRange( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;

      short rangeIndex = connect4receiveShort( connect ) ;
      Index5rangeInfo *range = client->indexRanges.remove( rangeIndex ) ;
      delete range ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIndexSetRange( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short oldRangeIndex = connect4receiveShort( connect ) ;
      short iIndex = connect4receiveShort( connect ) ;
      Index5server *index = client->indexes.get( iIndex ) ;

      if ( index == 0 )
      {
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         return ;
      }

      Index5rangeInfo *range = new Index5rangeInfo() ;

      short rangeIndex = 0 ;

      try
      {
         range->setRange( connect ) ;
      }
      catch ( Err5oledb& )
      {
         rangeIndex = -1 ;
         delete range ;
      }

      if ( rangeIndex == 0 )
      {
         if ( oldRangeIndex != -1 )  // remove the old one as well...
         {
            Index5rangeInfo *old = client->indexRanges.remove( oldRangeIndex ) ;
            if ( old == 0 )   // range didn't exist, critical failure for client
            {
               // bad data, must delete client
               error4( client->connect.cb, e4connect, E91530 ) ;
               connect4sendShort( connect, -1 ) ;
               return ;
            }
            delete old ;
         }
         rangeIndex = client->indexRanges.add( range ) ;
      }

      connect4sendShort( connect, rangeIndex ) ;

      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIndexRemove( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iIndex = connect4receiveShort( connect ) ;
      Index5server *index = client->indexes.remove( iIndex ) ;

      if ( index == 0 )
      {
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         return ;
      }

      index->remove() ;
      delete index ;
      connect4sendShort( connect, 0 ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIntegratedRowRequest( SERVER4CLIENT *client )
   {
      // similar to indexRowRequest, but we send back the bound table rowset fields for the integrated
      // index instead of the key.
      Table5rowsetValues rowsetValues ;
      memset( &rowsetValues, 0, sizeof( rowsetValues ) ) ;

      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;

      short iRowset = connect4receiveShort( connect ) ;
      Table5rowsetValues *baseTableRowsetValues = client->rowsets.get( iRowset ) ;

      short iIndex = connect4receiveShort( connect ) ;
      Index5server *index = client->indexes.get( iIndex ) ;
      if ( index == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      short keyLeng = connect4receiveShort( connect ) ;
      if ( keyLeng > I4MAX_KEY_SIZE )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }
      if ( keyLeng != 0 )
         connect4receive( connect, index->currentKey, keyLeng, code4timeoutVal( c4 ) ) ;
      ULONG startRecNo = connect4receiveLong( connect ) ;
      LONG offset = connect4receiveLong( connect ) ;
      LONG cRecNo = connect4receiveLong( connect ) ;
      rowsetValues.oldDirection = connect4receiveChar( connect ) ;
      rowsetValues.atEndpoint = connect4receiveChar( connect ) ;
      rowsetValues.didSeek = connect4receiveChar( connect ) ;

      short iRangeData = connect4receiveShort( connect ) ;
      Index5rangeInfo *rangeData ;

      if ( iRangeData == -1 )
         rangeData = 0 ;
      else
         rangeData = client->indexRanges.get( iRangeData ) ;

      int direction ;
      if ( cRecNo < 0 )
      {
         direction = -1 ;
         cRecNo = -cRecNo ;
      }
      else
         direction = 1 ;

      long rc = 0  ;
      try
      {
         if ( keyLeng == 0 )
            index->rowPosition( 0, startRecNo, offset, direction, &rowsetValues, rangeData ) ;
         else
            index->rowPosition( index->currentKey, startRecNo, offset, direction, &rowsetValues, rangeData ) ;
      }
      catch( Err5bof& )
      {
         if ( direction == -1 )  // at bof - bof of rowset with no records
         {
            if ( offset != 0 )
               rc = DB_E_BADSTARTPOSITION ;
            else   /* was not due to offset, so only end of rowset */
               rc = DB_S_ENDOFROWSET ;
         }
      }
      catch( Err5eof& )
      {
         if ( direction == 1 )  // at end - end of rowset with no records
         {
            if ( offset != 0 )
               rc = DB_E_BADSTARTPOSITION ;
            else   /* was not due to offset, so only end of rowset */
               rc = DB_S_ENDOFROWSET ;
         }
      }
      catch( Err5oledb &err )
      {
         switch ( err.oledbCode() )
         {
            case DB_E_BADSTARTPOSITION:
               rc = DB_E_BADSTARTPOSITION ;
               break ;
            case DB_S_ENDOFROWSET:
               rc = DB_S_ENDOFROWSET ;
               break ;
            default:
               throw Err5internal( c4 ) ;  // no other errors should be possible
         }
      }

      connect4sendLong( connect, rc ) ;  // send the general return code status

      ULONG recNo ;

      // now send the records accross
      if ( rc == 0 && cRecNo != 0 )
      {
         for ( int loop = 0 ; loop < cRecNo ; loop++ )
         {
            try
            {
               index->rowGet( direction, &recNo, index->currentKey, &rowsetValues, rangeData ) ;
            }
            catch( Err5bof& )
            {
               connect4sendShort( connect, r4bof ) ;
               break ;
            }
            catch( Err5eof& )
            {
               connect4sendShort( connect, r4eof ) ;
               break ;
            }

            // now that we have the recno, must go the the record (maybe locked,etc.)
            try
            {
               rc = baseTableRowsetValues->table->goRead( recNo, baseTableRowsetValues ) ;
            }
            catch( Err5& )
            {
               rc = error4code( c4 ) ;
               if ( rc == 0 )
                  rc = -1 ;
            }
            if ( rc == r4deleted )  // means don't expose deleted, so skip this record...
            {
               // AS 06/30/99 --> in fact, what we want to do is change the loop counter (decrement it by 1)
               // this is superior to changing cRecNo.  Plus, cRecNo is always positive, so should always
               // just be derementing, not affecting by direction...
               // cRecNo += direction ;  // doesn't count as a record
               loop-- ;
               connect4sendShort( connect, r4skipped ) ;
               continue ;
            }
            connect4sendShort( connect, (short)rc ) ;
            switch( rc )
            {
               case r4success:
                  break ;
               case r4locked:
                  rc = 1 ;
                  break ;
               case r4eof:
               case r4bof:
               default:
                  rc = -1 ;
                  break ;
            }

            if ( rc < 0 )  // severe type error or bof/eof
               break ;

            connect4sendLong( connect, recNo ) ;

            if ( rc == 1 )  // means continue (don't send record info) (r4locked, r4deleted)
               continue ;

            char *recBuf = baseTableRowsetValues->table->data4()->record ;
            baseTableRowsetValues->fieldSet.sendNonDeferredFields( connect, recBuf ) ;
         }

         connect4sendChar( connect, rowsetValues.oldDirection ) ;
         connect4sendChar( connect, rowsetValues.atEndpoint ) ;
         connect4sendLong( connect, recNo ) ;
         connect4send( connect, index->currentKey, index->keyLen() ) ;
      }

      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIndexRowRequest( SERVER4CLIENT *client, int doKey )
   {
      CODE4 *c4 ;
      CONNECT4 *connect = &client->connect ;
      short iIndex, keyLeng ;
      Index5server *index ;
      int loop ;
      long rc ;
      ULONG startRecNo, recNo ;
      LONG cRecNo, offset ;
      Table5rowsetValues rowsetValues ;
      memset( &rowsetValues, 0, sizeof( rowsetValues ) ) ;

      c4 = client->connect.cb ;

      iIndex = connect4receiveShort( connect ) ;
      index = client->indexes.get( iIndex ) ;
      if ( index == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      keyLeng = connect4receiveShort( connect ) ;
      if ( keyLeng > I4MAX_KEY_SIZE )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }
      if ( keyLeng != 0 )
         connect4receive( connect, index->currentKey, keyLeng, code4timeoutVal( c4 ) ) ;
      startRecNo = connect4receiveLong( connect ) ;
      offset = connect4receiveLong( connect ) ;
      cRecNo = connect4receiveLong( connect ) ;
      rowsetValues.oldDirection = connect4receiveChar( connect ) ;
      rowsetValues.atEndpoint = connect4receiveChar( connect ) ;
      rowsetValues.didSeek = connect4receiveChar( connect ) ;

      short iRangeData = connect4receiveShort( connect ) ;
      Index5rangeInfo *rangeData ;

      if ( iRangeData == -1 )
         rangeData = 0 ;
      else
         rangeData = client->indexRanges.get( iRangeData ) ;

      int direction ;
      if ( cRecNo < 0 )
      {
         direction = -1 ;
         cRecNo = -cRecNo ;
      }
      else
         direction = 1 ;

      rc = 0  ;
      try
      {
         if ( keyLeng == 0 )
            index->rowPosition( 0, startRecNo, offset, direction, &rowsetValues, rangeData ) ;
         else
            index->rowPosition( index->currentKey, startRecNo, offset, direction, &rowsetValues, rangeData ) ;
      }
      catch( Err5bof& )
      {
         if ( direction == -1 )  // at bof - bof of rowset with no records
         {
            if ( offset != 0 )
               rc = DB_E_BADSTARTPOSITION ;
            else   /* was not due to offset, so only end of rowset */
               rc = DB_S_ENDOFROWSET ;
         }
      }
      catch( Err5eof& )
      {
         if ( direction == 1 )  // at end - end of rowset with no records
         {
            if ( offset != 0 )
               rc = DB_E_BADSTARTPOSITION ;
            else   /* was not due to offset, so only end of rowset */
               rc = DB_S_ENDOFROWSET ;
         }
      }
      catch( Err5oledb &err )
      {
         switch ( err.oledbCode() )
         {
            case DB_E_BADSTARTPOSITION:
               rc = DB_E_BADSTARTPOSITION ;
               break ;
            case DB_S_ENDOFROWSET:
               rc = DB_S_ENDOFROWSET ;
               break ;
            default:
               throw Err5internal( c4 ) ;  // no other errors should be possible
         }
      }

      connect4sendLong( connect, rc ) ;  // send the general return code status

      // now send the records accross
      if ( rc == 0 && cRecNo != 0 )
      {
         for ( loop = 0 ; loop < cRecNo ; loop++ )
         {
            try
            {
               index->rowGet( direction, &recNo, index->currentKey, &rowsetValues, rangeData ) ;
            }
            catch( Err5bof& )
            {
               connect4sendLong( connect, r4bof ) ;
               break ;
            }
            catch( Err5eof& )
            {
               connect4sendLong( connect, r4eof ) ;
               break ;
            }
            connect4sendLong( connect, 0 ) ;  // success
            connect4sendLong( connect, recNo ) ;
            if ( doKey )
               connect4send( connect, index->currentKey, index->keyLen() ) ;
         }

         connect4sendChar( connect, rowsetValues.oldDirection ) ;
         connect4sendChar( connect, rowsetValues.atEndpoint ) ;
         connect4sendLong( connect, recNo ) ;
         connect4send( connect, index->currentKey, index->keyLen() ) ;
      }

      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgRowRequestSeq( SERVER4CLIENT *client )
   {
      CODE4 *c4 ;
      CONNECT4 *connect = &client->connect ;
      short iRowset ;
      Table5rowsetValues *rowset ;
      ULONG startRecNo ;
      LONG cRecNo ;

      c4 = client->connect.cb ;

      #ifdef TIMER5OUT
         server4timerStart( c4->server, "receive short " )  ;
      #endif
      iRowset = connect4receiveShort( connect ) ;
      #ifdef TIMER5OUT
         server4timerStop( c4->server ) ;
      #endif

      #ifdef TIMER5OUT
         server4timerStart( c4->server, "get rowset " )  ;
      #endif
      rowset = client->rowsets.get( iRowset ) ;
      #ifdef TIMER5OUT
         server4timerStop( c4->server ) ;
      #endif
      if ( rowset == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      #ifdef TIMER5OUT
         server4timerStart( c4->server, "receive various info " )  ;
      #endif
      rowset->exposeDeleted = connect4receiveChar( connect ) ;
      char countDeletedAsSkip = connect4receiveChar( connect ) ;
      long oldStartRecNo = connect4receiveLong( connect ) ;
      startRecNo = connect4receiveLong( connect ) ;
      cRecNo = connect4receiveLong( connect ) ;
      char isSpecialBookmark = connect4receiveChar( connect ) ;
      #ifdef TIMER5OUT
         server4timerStop( c4->server ) ;
      #endif

      short direction ;
      if ( cRecNo < 0 )
         direction = -1 ;
      else
         direction = 1 ;

      char *recBuf = rowset->table->data4()->record ;

      int rc = 0 ;
      rowset->didSkip = 0 ;

      if ( rowset->exposeDeleted == 0 )  // special skipping case - bypass deleted rows
      {
         int sign = 1 ;
         int offset = startRecNo - oldStartRecNo ;
         int offsetStart = offset ;
         if ( offset < 0 )
         {
            sign = -1 ;
            offset = -offset ;
         }
         else if ( offset == 0 && cRecNo < 0 )
            sign = -1 ;
         if ( countDeletedAsSkip && isSpecialBookmark == 0 )
         {
            #ifdef TIMER5OUT
               server4timerStart( c4->server, "goRead " )  ;
            #endif
            rc = rowset->table->goRead( oldStartRecNo, rowset ) ;
            #ifdef TIMER5OUT
               server4timerStop( c4->server ) ;
            #endif
            if ( rc == r4deleted )  // special case, count in offset
            {
               connect4sendShort( connect, r4skipped ) ;
               if ( offset != 0 )
                  offset-- ;
               oldStartRecNo += sign ;
            }
         }
         #ifdef TIMER5OUT
            server4timerStart( c4->server, "for loop 1 of goRead " )  ;
         #endif
         for ( ;; offset-- )
         {
            try
            {
               rc = rowset->table->goRead( oldStartRecNo, rowset ) ;
            }
            catch( Err5& )
            {
               rc = error4code( c4 ) ;
               if ( rc == 0 )
                  rc = -1 ;
            }
            if ( rc == r4deleted )  // bypass
            {
               if ( offsetStart == 0 )
               {
                  switch( isSpecialBookmark )  // special case read eof/bof is elsewhere
                  {
                     case r4bof:
                        break ;
                     case r4eof:
                        break ;
                     case 0:
                        offset++ ;
                        break ;
                     default:  // should not occur
                        throw Err5internal( c4 ) ;
                        break ;
                  }
               }
               else
                  offset++ ;
            }
            if ( offset == 0 )
               break ;
            oldStartRecNo += sign ;
         }
         #ifdef TIMER5OUT
            server4timerStop( c4->server ) ;
         #endif
         startRecNo = oldStartRecNo ;
      }

      #ifdef TIMER5OUT
         server4timerStart( c4->server, "for loop 2 of goRead " )  ;
      #endif
      for( ;; cRecNo -= direction, startRecNo += direction )
      {
         if ( cRecNo == 0 )  // done  -- don't send eof in this case
            break ;
         try
         {
            #ifdef TIMER5OUT
               server4timerStart( c4->server, "goRead call " )  ;
            #endif
            rc = rowset->table->goRead( startRecNo, rowset ) ;
            #ifdef TIMER5OUT
               server4timerStop( c4->server ) ;
            #endif
         }
         catch( Err5& )
         {
            rc = error4code( c4 ) ;
            if ( rc == 0 )
               rc = -1 ;
         }
         // AS 01/18/01 - Due to conformance/specs change, we need to set the 'deleted' marker
         // on deleted records, even if they are exposed.  Hence, the goRead() function above
         // was changed to return r4deleted even if deleted rows are exposed.  So, in this case,
         // if rc == r4deleted we still need to check expose flag.
         // if ( rc == r4deleted )  // means don't expose deleted, so skip this record...
         if ( ( rc == r4deleted ) && ( rowset->exposeDeleted == 0 ) )  // means don't expose deleted, so skip this record...
         {
            cRecNo += direction ;  // doesn't count as a record
            #ifdef TIMER5OUT
               server4timerStart( c4->server, "deleted case, send deleted " )  ;
            #endif
            connect4sendShort( connect, r4skipped ) ;
            #ifdef TIMER5OUT
               server4timerStop( c4->server ) ;
            #endif
            continue ;
         }
         #ifdef TIMER5OUT
            server4timerStart( c4->server, "send rc " )  ;
         #endif
         connect4sendShort( connect, rc ) ;
         #ifdef TIMER5OUT
            server4timerStop( c4->server ) ;
         #endif
         switch( rc )
         {
            case r4success:
               break ;
            case r4locked:
               rc = 1 ;
               break ;
            // AS 01/18/01 - New cae - return r4deleted on exposed rows
            case r4deleted:
               if ( rowset->exposeDeleted == 0 )
                  rc = -1 ;
               else
                  rc = 0 ;  // do send record info in this case
               assert5( rc != 1 ) ;
               break ;
            case r4eof:
            case r4bof:
            default:
               rc = -1 ;
               break ;
         }

         if ( rc < 0 )  // severe type error or bof/eof
            break ;

         #ifdef TIMER5OUT
            server4timerStart( c4->server, "send recno " )  ;
         #endif
         connect4sendLong( connect, startRecNo ) ;
         #ifdef TIMER5OUT
            server4timerStop( c4->server ) ;
         #endif

         if ( rc == 1 )  // means continue (don't send record info) (r4locked, r4deleted)
            continue ;

         #ifdef TIMER5OUT
            server4timerStart( c4->server, "send non-deferred fields " )  ;
         #endif
         rowset->fieldSet.sendNonDeferredFields( connect, recBuf ) ;
         #ifdef TIMER5OUT
            server4timerStop( c4->server ) ;
         #endif
      }
      #ifdef TIMER5OUT
         server4timerStop( c4->server ) ;
      #endif
      #ifdef TIMER5OUT
         server4timerStart( c4->server, "send flush " )  ;
      #endif
      connect4sendFlush( connect ) ;
      #ifdef TIMER5OUT
         server4timerStop( c4->server ) ;
      #endif
   }



   /* OLEDB5BUILD, S4SERVER */
   /*
   void server4clientMsgIndexUpdateFieldset( SERVER4CLIENT *client )
   {
      CODE4 *c4 ;
      CONNECT4 *connect = &client->connect ;
      short iIndex, nColumns ;
      ULONG nFields, iOrdinal ;
      Index5server *index ;

      c4 = client->connect.cb ;

      iIndex = connect4receiveShort( connect ) ;
      index = client->indexes.get( iIndex ) ;
      if ( index == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      COLUMN5INFO *columns = index->columns( &nColumns ) ;

      nFields = connect4receiveLong( connect ) ;

      // first remove the existing ones since order may have changed
      for ( short j = 0 ; j < nColumns ; j++ )
      {
         if ( columns[j].link.n != 0 )
            index->fieldsToSend.remove( &columns[j] ) ;
      }

      for ( ULONG i = 0 ; i < nFields ; i++ )
      {
         iOrdinal = connect4receiveLong( connect ) ;
         index->fieldsToSend.add( &columns[iOrdinal] ) ;
      }
   }
   */



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgUpdateFieldset( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iRowset, iData ;
      short nFields, iOrdinal ;
      char isDeferred ;
      Table5serverBase *table ;
      Table5rowsetValues *rowset ;

      CODE4 *c4 = client->connect.cb ;

      iData = connect4receiveShort( connect ) ;
      table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }
      iRowset = connect4receiveShort( connect ) ;
      if ( iRowset == 0 )  // a new set of fields for the table
      {
         rowset = new Table5rowsetValues ;
         iRowset = client->rowsets.add( rowset ) ;
         rowset->table = table ;
         rowset->fieldSet.init( table->table ) ;
      }
      else
      {
         rowset = client->rowsets.get( iRowset ) ;
         if ( rowset == 0 )
         {
            // bad data, must delete client
            error4( c4, e4connect, E91530 ) ;
            return ;
         }
         assert5( rowset->table == table ) ;
      }

      connect4sendShort( connect, iRowset ) ;
      connect4sendFlush( connect ) ;

      // first remove the existing ones since order may have changed
      rowset->fieldSet.resetFieldsToSend() ;

      for ( nFields = 0 ;; nFields++ )
      {
         iOrdinal = connect4receiveShort( connect ) ;
         if ( iOrdinal == -1 ) // means done
            break ;
         if ( iOrdinal > rowset->fieldSet.numFields() || iOrdinal <= 0 )  // we subtract 1 below, so better not be 0 (or negative)
         {
            // bad data, must delete client
            error4( c4, e4connect, E91530 ) ;
            return ;
         }
         isDeferred = connect4receiveChar( connect ) ;

         rowset->fieldSet.markFieldToSend( iOrdinal-1, isDeferred ) ;
      }
   }



   /* OLEDB5BUILD, S4SERVER */
   static void sendColumns( Table5server *table, CONNECT4 *connect, COLUMN5INFO *columns, int nColumns, int isIndex )
   {
      // isIndex --> index sends the offsetRaw as well since it is static since all fields sent
      connect4sendShort( connect, nColumns ) ;
      for ( int i = 0 ; i < nColumns ; i++ )
      {
         #ifdef S4MDX
            if ( columns[i].type == r4memo || ( columns[i].type == r4bin ) )
         #else
            if ( columns[i].type == r4memo || columns[i].type == r4memoBin || columns[i].type == r4gen )
         #endif
            columns[i].isDeferred = 1 ;
         if ( columns[i].name == 0 )  // bookmark field
         {
            // AS 04/13/99 -- should be null, so send a length of 0 instead...
            // connect4sendShort( connect, 2 ) ;
            // connect4send( connect, L"", 2 ) ;
            connect4sendShort( connect, 0 ) ;
         }
         else
         {
            WSTR5 *name ;
            if ( isIndex )  // table not available, ok to use column info for name
               name = columns[i].name ;
            else
               name = table->getName( i ) ;
            short nameLen = ( wcslen( name ) + 1 ) * 2 ; // also send NULLs
            assert5( nameLen <= FIELD5NAME_MAX_BYTES ) ;
            connect4sendShort( connect, nameLen ) ;
            connect4send( connect, name, nameLen ) ;
         }
         connect4send( connect, &columns[i].isNullable, sizeof(columns[i].isNullable) ) ;
         connect4send( connect, &columns[i].len, sizeof(columns[i].len) ) ;
         assert5( (isIndex && columns[i].rawLen < I4MAX_KEY_SIZE) || (!isIndex) ) ;
         connect4send( connect, &columns[i].rawLen, sizeof(columns[i].rawLen) ) ;
         connect4send( connect, &columns[i].type, sizeof(columns[i].type) ) ;
         connect4send( connect, &columns[i].precision, sizeof(columns[i].precision) ) ;
         connect4send( connect, &columns[i].descending, sizeof(columns[i].descending) ) ;
         connect4send( connect, &columns[i].typeRaw, sizeof(columns[i].typeRaw) ) ;
         if ( isIndex )
            connect4send( connect, &columns[i].offsetRaw, sizeof(columns[i].offsetRaw) ) ;
         connect4send( connect, &columns[i].isComplex, sizeof(columns[i].isComplex) ) ;
         connect4send( connect, &columns[i].decimalsRaw, sizeof(columns[i].decimalsRaw) ) ;
         connect4send( connect, &columns[i].scale, sizeof(columns[i].scale) ) ;
         connect4send( connect, &columns[i].isDeferred, sizeof(columns[i].isDeferred) ) ;
         connect4send( connect, &columns[i].nullBitRawPosition, sizeof(columns[i].nullBitRawPosition) ) ;
         connect4send( connect, &columns[i].ordinal, sizeof(columns[i].ordinal) ) ;
         connect4send( connect, &columns[i].order, sizeof(columns[i].order) ) ;
         connect4send( connect, &columns[i].lossOfInfo, sizeof(columns[i].lossOfInfo) ) ;
      }

      connect4sendFlush( connect ) ;
   }





   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgIndexColumnInfo( SERVER4CLIENT *client )
   {
      short iIndex, nColumns ;
      CODE4 *c4 ;
      CONNECT4 *connect = &client->connect ;
      Index5server *index ;

      c4 = client->connect.cb ;

      iIndex = connect4receiveShort( connect ) ;
      index = client->indexes.get( iIndex ) ;
      if ( index == 0 )
      {
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      COLUMN5INFO *columns = index->columns( &nColumns ) ;

      sendColumns( 0, connect, columns, nColumns, 1 ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgColumnInfo( SERVER4CLIENT *client )
   {
      short iData, nColumns ;
      CODE4 *c4 ;
      CONNECT4 *connect = &client->connect ;
      Table5serverBase *table ;

      c4 = client->connect.cb ;

      iData = connect4receiveShort( connect ) ;
      table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      COLUMN5INFO *columns = table->table->columns( &nColumns ) ;

      sendColumns( table->table, connect, columns, nColumns, 0 ) ;
   }



   static void connect4receiveRowsetValues( CONNECT4 *connect, Table5rowsetValues *rowsetValues )
   {
       rowsetValues->exposeDeleted = connect4receiveChar( connect ) ;
       rowsetValues->startCount = connect4receiveLong( connect ) ;
       rowsetValues->didSeek = connect4receiveChar( connect ) ;
       rowsetValues->setStartRecno = connect4receiveLong( connect ) ;
       rowsetValues->setEndRecno = connect4receiveLong( connect ) ;
       rowsetValues->requestMethod = connect4receiveChar( connect ) ;
       rowsetValues->descendingIsDesired = connect4receiveChar( connect ) ;
       rowsetValues->mdxBof = connect4receiveChar( connect ) ;
       rowsetValues->oldDirection = connect4receiveChar( connect ) ;
       rowsetValues->oldOldDirection = connect4receiveChar( connect ) ;
       rowsetValues->atEndpoint = connect4receiveChar( connect ) ;
       rowsetValues->oldAtEndpoint = connect4receiveChar( connect ) ;
       rowsetValues->didSkip = connect4receiveChar( connect ) ;
       rowsetValues->curRecNo = connect4receiveLong( connect ) ;
   }


   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionSeek( SERVER4CLIENT *client )
   {
      // not supported yet...
      CODE4 *c4 ;
      Index5server *index ;
      CONNECT4 *connect = &client->connect ;

      c4 = client->connect.cb ;

      short iIndex = connect4receiveShort( connect ) ;
      index = client->indexes.get( iIndex ) ;
      if ( index == 0 )
      {
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         return ;
      }

      int keyLen = connect4receiveShort( connect ) ;
      char *key = (char *)u4alloc( keyLen ) ;
      connect4receive( connect, key, keyLen, code4timeoutVal( c4 ) ) ;
      DBSEEK seekOptions ;
      connect4receive( connect, &seekOptions, sizeof( seekOptions ), code4timeoutVal( c4 ) ) ;
      Table5rowsetValues rowsetValues ;

      connect4receiveRowsetValues( connect, &rowsetValues ) ;

      short iRangeData = connect4receiveShort( connect ) ;
      Index5rangeInfo *rangeData ;

      if ( iRangeData == -1 )
         rangeData = 0 ;
      else
         rangeData = client->indexRanges.get( iRangeData ) ;

      short rc = 0 ;
      try
      {
         index->rowSeek( key, keyLen, seekOptions, &rowsetValues, rangeData ) ;
      }
      catch( Err5oledb &err )
      {
         // AS 04/19/99 --> changed from oledb-code. c/s t5samp6.cpp - if Err5found, need err5oleFound...
         rc = (short)err.errCode() ;
      }
      catch( Err5& )
      {
         // would occur only say if connection has failed... - terminate connection
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         u4free( key ) ;
         return ;
      }

      connect4sendShort( connect, rc ) ;
      if ( rc == 0 )
      {
         connect4sendChar( connect, rowsetValues.oldDirection ) ;
         connect4sendChar( connect, rowsetValues.atEndpoint ) ;
         connect4sendLong( connect, index->recNo() ) ;
         connect4send( connect, index->key(), index->keyLen() ) ;
      }
      connect4sendFlush( connect ) ;
      u4free( key ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionDefaults( SERVER4CLIENT *client )
   {
      CODE4 *c4 ;
      short iRowset ;
      Table5rowsetValues *rowset ;
      CONNECT4 *connect = &client->connect ;
      #ifdef E4DEBUG
         long len = 0 ;
      #endif

      c4 = connect->cb ;
      iRowset = connect4receiveShort( connect ) ;

      rowset = client->rowsets.get( iRowset ) ;
      if ( rowset == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      #ifdef E4DEBUG
         if ( client->lastCommandWasLen == 1 )  // verify length for debug
         {
            len += 2 * sizeof( short ) ;  // the action code and iData
            len += rowset->fieldSet.countNonDeferredFieldsLen() ;
            if ( len != client->commandLen )   // length mismatch
               throw Err5internal( c4 ) ;
         }
      #endif

      rowset->fieldSet.sendDefaultValues( connect ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionDelete( SERVER4CLIENT *client )
   {
      CODE4 *c4 ;
      short iData ;
      Table5serverBase *table ;
      CONNECT4 *connect = &client->connect ;

      c4 = connect->cb ;
      iData = connect4receiveShort( connect ) ;

      table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         return ;
      }

      ULONG recNo = connect4receiveLong( connect ) ;

      table->table->deleteRec( recNo ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionWrite( SERVER4CLIENT *client )
   {
      // CODE4 *c4 ;
      // short iRowset ;
      // Table5rowsetValues *rowset ;
      ByRef5 *byRef = 0 ;
      Ptr5lenAlloc *ptrsAlloc = 0 ;

      CONNECT4 *connect = &client->connect ;
      #ifdef E4DEBUG
         long len = 0 ;
      #endif

      CODE4 *c4 = connect->cb ;
      short iRowset = connect4receiveShort( connect ) ;

      Table5rowsetValues *rowset = client->rowsets.get( iRowset ) ;
      if ( rowset == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      #ifdef E4DEBUG
         if ( client->lastCommandWasLen == 1 )  // verify length for debug
         {
            len += 2 * sizeof( short ) ;  // the action code and iData
            len += sizeof( long ) ;       // the recno
         }
      #endif

      ULONG recNo = connect4receiveLong( connect ) ;
      if ( rowset->fieldSet.nDeferredSend != 0 )
      {
         byRef = (ByRef5 *)u4alloc( rowset->fieldSet.nDeferredSend * sizeof( ByRef5 ) ) ;
         ptrsAlloc = rowset->fieldSet.receiveDeferredFields( connect, byRef ) ;
      }

      #ifdef E4DEBUG
         if ( client->lastCommandWasLen == 1 )  // verify length for debug
         {
            len += rowset->fieldSet.countNonDeferredFieldsLen() ;
            for ( int i = 0 ; i < rowset->fieldSet.nDeferredSend ; i++ )
               len += byRef[i].len() + sizeof( long ) ;

            if ( len != client->commandLen )   // length mismatch
            {
               throw Err5internal( c4 ) ;
               // bad data, must delete client
               error4( c4, e4connect, E91530 ) ;
               if ( ptrsAlloc != 0 )
               {
                  ptrsAlloc->free() ;
                  delete [] ptrsAlloc ;
               }
               free5( byRef ) ;
               return ;
            }
         }
      #endif

      char *rawBuffer = rowset->table->table->writeGetRawBuffer( recNo, 0 ) ;
      // rawBuffer == 0 occurs if record is locked for sure, maybe other times...
      // if rawBuffer is 0, that is fine, receiveNoDeferredFields just dumps out the extra sent information...
      rowset->fieldSet.receiveNonDeferredFields( connect, rawBuffer ) ;

      if ( rawBuffer != 0 ) // buffer is available... write the row out
         rowset->table->table->rowWrite( recNo, byRef, &rowset->fieldSet ) ;

      if ( byRef != 0 )
         u4free( byRef ) ;
      if ( ptrsAlloc != 0 )
      {
         ptrsAlloc->free() ;
         delete [] ptrsAlloc ;
      }
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionWriteDone( SERVER4CLIENT *client )
   {
      short iData ;
      Table5serverBase *table ;
      CONNECT4 *connect = &client->connect ;

      iData = connect4receiveShort( connect ) ;

      table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( connect->cb, e4connect, E91530 ) ;
         return ;
      }

      long nErrors ;
      Result5update *errorList ;

      table->table->flush( &errorList, &nErrors, -1 ) ;

      connect4sendLong( connect, nErrors ) ;
      connect4send( connect, errorList, nErrors * sizeof( Result5update ) ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   static void sendSchemaVersions( CONNECT4 *connect, SERVER4CLIENT *client )
   {
      // sends the schema versions accross.  0 is sent if not available.
      long version  ;
      Source5all *source ;
      source = &client->server->source ;

      if ( source->tablesSchema != 0 )
         version = source->tablesSchema->version ;
      else
         version = 0 ;
      connect4sendLong( connect, version ) ;

      if ( source->indexesSchema != 0 )
         version = source->indexesSchema->version ;
      else
         version = 0 ;
      connect4sendLong( connect, version ) ;

      if ( source->columnsSchema != 0 )
         version = source->columnsSchema->version ;
      else
         version = 0 ;
      connect4sendLong( connect, version ) ;

      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionUnlockAll( SERVER4CLIENT *client )
   {
      // just requests a code4unlockAll() to release all locks...
      code4unlockDo( tran4dataList( &client->trans ) ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionSchemaOpen( SERVER4CLIENT *client )
   {
      short iData ;
      Schema5type schemaType ;
      CONNECT4 *connect = &client->connect ;
      Table5schemaBase *schema = 0 ;

      CODE4 *c4 = client->connect.cb ;
      Source5all *source = &client->server->source ;

      connect4receive( connect, &schemaType, sizeof( Schema5type ), code4timeoutVal( c4 ) ) ;
      source->isUnicode = connect4receiveChar( connect ) ;

      SERVER4CLIENT *curClient = c4->currentClient ;
      c4->currentClient = c4->catalogClient ;  // for transactions, etc., all done through catalog client for schema tables

      if ( client->session == 0 )   // this is the 1st OLE-DB call from the client
      {
         client->session = new Session5( c4 ) ;
         client->tables.session = client->session ;
         client->session->init( source ) ;
         assert5( client->session != 0 ) ;
      }
      assert5( client->session->source != 0 ) ;

      // in this case, we must create a new one...
      Context4client clientContext ;
      clientContext.setClient( curClient ) ;
      try
      {
         switch( schemaType )
         {
            case schema5typeTables:
               if ( source->tablesSchema == 0 )  // need to open the tables schema
               {
                  source->tablesSchema = new Table5tables( source ) ;
                  assert5( source->tablesSchema->source != 0 ) ;
                  try
                  {
                     source->tablesSchema->init( &clientContext ) ;
                  }
                  catch( Err5oledb &err )
                  {
                     delete source->tablesSchema ;
                     source->tablesSchema = 0 ;
                     throw Err5oledb( err.oledbCode(), c4 ) ;
                  }
               }
               iData = SCHEMA5ID_TABLES ; // special code indicating the schema tables table
               schema = source->tablesSchema ;
               if ( client->session->tablesSchema == 0 )
               {
                  client->session->tablesSchema = new Table5schemaServerLocal( source->tablesSchema ) ;
                  client->session->tablesSchema->init( source->tablesSchema ) ;
               }
               break ;
            case schema5typeIndexes:
               if ( source->indexesSchema == 0 )  // need to open the tables schema
               {
                  source->indexesSchema = new Table5indexes( source ) ;
                  assert5( source->indexesSchema->source() != 0 ) ;
                  try
                  {
                     source->indexesSchema->init( &clientContext ) ;
                  }
                  catch( Err5oledb &err )
                  {
                     delete source->indexesSchema ;
                     source->indexesSchema = 0 ;
                     throw Err5oledb( err.oledbCode(), c4 ) ;
                  }
               }
               iData = SCHEMA5ID_INDEXES ; // special code indicating the schema indexes table
               schema = source->indexesSchema ;
               if ( client->session->indexesSchema == 0 )
               {
                  client->session->indexesSchema = new Table5schemaServerLocal( source->indexesSchema ) ;
                  client->session->indexesSchema->init( source->indexesSchema ) ;
               }
               break ;
            case schema5typeColumns:
               if ( source->columnsSchema == 0 )  // need to open the tables schema
               {
                  source->columnsSchema = new Table5columns( source ) ;
                  assert5( source->columnsSchema->source() != 0 ) ;
                  try
                  {
                     source->columnsSchema->init( &clientContext ) ;
                  }
                  catch( Err5oledb &err )
                  {
                     delete source->columnsSchema ;
                     source->columnsSchema = 0 ;
                     throw Err5oledb( err.oledbCode(), c4 ) ;
                  }
               }
               iData = SCHEMA5ID_COLUMNS ; // special code indicating the schema columns table
               schema = source->columnsSchema ;
               if ( client->session->columnsSchema == 0 )
               {
                  client->session->columnsSchema = new Table5schemaServerLocal( source->columnsSchema ) ;
                  client->session->columnsSchema->init( source->columnsSchema ) ;
               }
               break ;
            default:  // invalid id
               throw( Err5open( 0 ) ) ;
               break ;
         }
      }
      catch( Err5& )
      {
         connect4sendShort( connect, -1 ) ;
         connect4sendFlush( connect ) ;
         c4->currentClient = curClient ;
         clientContext.resetClient() ;
         return ;
      }

      clientContext.resetClient() ;
      c4->currentClient = curClient ;

      connect4sendShort( connect, 0 ) ;
      connect4sendShort( connect, iData ) ;
      DATA4 *data = schema->data ;
      connect4sendLong( connect, d4numFields( data ) ) ;
      connect4sendShort( connect, schema->numNullBytes() ) ;

      // now send the versions for the schema tables, if available (client
      // uses for seeing if it needs to update itself at a later date)
      sendSchemaVersions( connect, client ) ;

      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionDataCreate( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;
      char *fNames ;
      short loop, iData;
      HRESULT rc = S_OK ;

      c4->singleClient = connect4receiveShort( connect ) ;  // all possible accessMode settings supported here...
      Source5all *source = &client->server->source ;
      c4->compatibility = connect4receiveShort( connect ) ;

      char *name = connect4receiveString( connect ) ;

      short numFields = connect4receiveShort( connect ) ;
      FIELD4INFO *fields = (FIELD4INFO *)alloc5( ( numFields + 1 ) * sizeof( FIELD4INFO ) ) ;
      memset( fields + numFields, 0, sizeof( FIELD4INFO ) ) ;
      fNames = (char *)alloc5null( numFields * 13 ) ;
      for ( loop = 0 ; loop < numFields ; loop++ )
      {
         connect4receive( connect, fNames + 13 * loop, 12, code4timeoutVal( c4 ) ) ;
         fields[loop].name = fNames + 13 * loop ;
         fields[loop].type = connect4receiveShort( connect ) ;
         fields[loop].len = connect4receiveShort( connect ) ;
         fields[loop].dec = connect4receiveShort( connect ) ;
         fields[loop].nulls = connect4receiveShort( connect ) ;
      }

      if ( client->session == 0 )   // this is the 1st OLE-DB call from the client
      {
         client->session = new Session5( c4 ) ;
         client->tables.session = client->session ;
         client->session->init( source ) ;
         assert5( client->session != 0 ) ;
      }
      assert5( client->session->source != 0 ) ;

      Table5server *table = new Table5server( &client->server->source, schema5false ) ;
      rc = 0 ;
      try
      {
         table->create( name, fields ) ;
      }
      catch( Err5oledb& err )
      {
         rc = err.oledbCode() ;
         error4set( c4, 0 ) ;
      }

      u4free( name ) ;
      free5( fields ) ;
      free5( fNames ) ;
      connect4sendLong( connect, rc ) ;
      if ( rc != S_OK )
      {
         connect4sendFlush( connect ) ;
         return ;
      }

      Table5serverBase *base = new Table5serverBase ;
      base->init( table ) ;
      iData = client->tables.add( base ) ;
      connect4sendShort( connect, iData ) ;

      short len = strlen( table->name() ) ;
      connect4sendShort( connect, len ) ;
      connect4send( connect, table->name(), len ) ;

      // now send the versions for the schema tables, if available (client
      // uses for seeing if it needs to update itself at a later date)
      sendSchemaVersions( connect, client ) ;

      // AS 05/05/99 - let client continue while the schemas are updated in the background...
      connect4sendFlush( connect ) ;

      // AS 04/15/99 --> was not adding entry to schema table on create...
      if ( client->session->source->tablesSchema != 0 )
         client->session->source->tablesSchema->addTable( table ) ;
      if ( client->session->source->indexesSchema != 0 )
         client->session->source->indexesSchema->check( table ) ;
      if ( client->session->source->columnsSchema != 0 )
         client->session->source->columnsSchema->check( table ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionDataOpen( SERVER4CLIENT *client )
   {
      char dataName[LEN4PATH], nameBuf[LEN4PATH] ;

      CONNECT4 *connect = &client->connect ;
      CODE4 *c4 = connect->cb ;

      short dataNameLen = connect4receiveShort( connect ) ;
      assert5( dataNameLen <= sizeof( dataName ) ) ;
      connect4receive( connect, dataName, dataNameLen, code4timeoutVal( c4 ) ) ;
      short accessMode = connect4receiveShort( connect ) ;
      short readOnly = connect4receiveShort( connect ) ;

      u4nameCurrent( nameBuf, LEN4PATH, dataName ) ;
      u4nameExt( nameBuf, LEN4PATH, DBF4EXT, 0 ) ;

   /*
      - I am going to just create another table structure in case of repetition.
      - it will likely be easier in the long run.  The alternative would be to
        tell the client he/she already has a handle, and give the id of it.

      // look through our list of tables to see if there is a match (i.e. if client provides a slightly different name, but is same file)
      if ( client->session != NULL )
      {
         table = client->session->findExisitingTable( nameBuf ) ;
         if ( table != NULL )
         {
            table->refCount++ ;
            connect4sendShort( connect, table->iData ) ;
            connect4sendFlush( connect ) ;
            return ;
         }
      }
   */

      int oldErrOpen = c4->errOpen ;
      c4->errOpen = 0 ;
      /* AS 07/21/99 - for win 95/98 to avoid endless laze writes, always setting 0 here.  If need to concern in OLE-DB, modify here. */
      DATA4 *data = D4open( c4, nameBuf, accessMode, r4unique, c4->log, 0, 0, readOnly, 0, c4->compatibility ) ;
      c4->errOpen = oldErrOpen ;

      if ( data == NULL )
      {

         HRESULT rc ;
         if (error4code(c4) == r4noExist )  // AS 06/03/99 --> need to know reason for failure, e4open not good enough...
            rc = DB_E_NOTABLE ;
         else
         {
            assert5( error4code(c4) <= -69 || error4code(c4) >= -60 ) ;  // errOpen off, so should get these errors...
            rc = E_FAIL ;
         }
         error4set( c4, 0 ) ;
         connect4sendLong( connect, rc ) ;
         connect4sendFlush( connect ) ;
         return ;
      }
      else
      {
         connect4sendLong( connect, 0 ) ;
         if ( client->session == NULL )   // this is the 1st OLE-DB call from the client
         {
            client->session = new Session5( c4 ) ;
            client->tables.session = client->session ;
            client->session->init( &client->server->source ) ;
            assert5( client->session != NULL ) ;
         }
         assert5( client->session->source != NULL ) ;
         Table5server *table = new Table5server( client->session->source, schema5false, data ) ;
         Table5serverBase *base = new Table5serverBase ;
         base->init( table ) ;
         short iData = client->tables.add( base ) ;
         connect4sendShort( connect, iData ) ;
         connect4sendLong( connect, d4numFields( data ) ) ;
         connect4sendShort( connect, table->numNullBytes() ) ;
      }
      // now send the versions for the schema tables, if available (client
      // uses for seeing if it needs to update itself at a later date)
      sendSchemaVersions( connect, client ) ;
      connect4sendFlush( connect ) ;
   }



   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionIndexClose( SERVER4CLIENT *client )
   {
      CONNECT4 *connect = &client->connect ;
      short iIndex = connect4receiveShort( connect ) ;
      Index5server *index = client->indexes.remove( iIndex ) ;
      if ( index == 0 )
      {
         // bad data, must delete client
         error4( client->connect.cb, e4connect, E91530 ) ;
         connect4sendShort( connect, -1 ) ;
         return ;
      }
      delete index ;
   }


   /* OLEDB5BUILD, S4SERVER */
   void server4clientMsgSessionIndexOpen( SERVER4CLIENT *client )
   {
      short iIndex, rc, iData ;
      Table5serverBase *table ;
      CODE4 *c4 ;
      CONNECT4 *connect = &client->connect ;
      Index5server *index ;

      c4 = client->connect.cb ;

      iData = connect4receiveShort( connect ) ;
      table = client->tables.get( iData ) ;
      if ( table == 0 )
      {
         // bad data, must delete client
         error4( c4, e4connect, E91530 ) ;
         return ;
      }

      short len = connect4receiveShort( connect ) ;
      char *name = (char *)alloc5null( len + 1 ) ;
      connect4receive( connect, name, len, code4timeoutVal( c4 ) ) ;

      rc = 0 ;
      try
      {
         index = new Index5server( client->session, table->table, name ) ;
      }
      catch( Err5oledb& )
      {
         rc = e4open ;
         connect4sendShort( connect, rc ) ;
         connect4sendFlush( connect ) ;
         free5( name ) ;
         return ;
      }

      free5( name ) ;

      iIndex = client->indexes.add( index ) ;

      connect4sendShort( connect, rc ) ;
      connect4sendShort( connect, iIndex ) ;
      connect4sendShort( connect, index->keyLen() ) ;
      connect4sendShort( connect, index->unique() ) ;
      // now send the versions for the schema tables, if available (client
      // uses for seeing if it needs to update itself at a later date)
      sendSchemaVersions( connect, client ) ;
      connect4sendFlush( connect ) ;
   }
#endif /* OLEDB5BUILD */



#ifndef S4ODBC_BUILD
/* S4SERVER */
int server4clientTables( SERVER4CLIENT *client )
{
   short len ;
   char *name ;
   CONNECT4 *connect = &client->connect ;

   len = connect4receiveShort( connect ) ;
   name = (char *)u4alloc( len + 1 ) ;

   connect4receive( connect, name, len, code4timeoutVal(client->connect.cb) ) ;
   name[len] = 0 ;

   #ifdef OLEDB5BUILD
      Source5all *source = &client->server->source ;

      if ( client->session == NULL )   // this is the 1st OLE-DB call from the client
      {
         client->session = new Session5( client->server->c4 ) ;
         client->tables.session = client->session ;
         client->session->init( source ) ;
         assert5( client->session != NULL ) ;
      }
      assert5( client->session->source != NULL ) ;

      if ( source->tablesSchema == NULL )  // need to open the tables schema
      {
         source->tablesSchema = new Table5tables( client->session->source ) ;
         Context4client clientContext ;
         clientContext.setClient( client ) ;
         try
         {
            source->tablesSchema->init( &clientContext ) ;
         }
         catch( Err5& )
         {
            clientContext.resetClient() ;
            connect4sendShort( connect, -1 ) ;
            u4free( name ) ;
            return connect4sendFlush( connect ) ;
         }
         clientContext.resetClient() ;
      }
      if ( client->session->tablesSchema == NULL )
      {
         client->session->tablesSchema = new Table5schemaServerLocal( source->tablesSchema ) ;
         client->session->tablesSchema->init( source->tablesSchema ) ;
      }

      WSTR5 *addDir = getUnicodeFromCharPtr5( name, NULL ) ;
      source->tablesSchema->addDirectory( addDir ) ;
      free5( addDir ) ;

      name = 0 ;
      const char *nm2 = source->tablesSchema->name() ;
      len = strlen( nm2 ) ;
      connect4sendShort( connect, len ) ;
      connect4send( connect, nm2, len ) ;
   #else
      connect4sendShort( connect, 0 ) ;  // sending a zero says that none is available
   #endif

   u4free( name ) ;
   return connect4sendFlush( connect ) ;
}
#endif /* !S4ODBC_BUILD */
#endif /* S4SERVER */
