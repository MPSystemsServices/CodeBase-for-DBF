/* d4secure.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4SERVER
   #include "tcp4char.c"
   int f4tcpAddress( FIELD4 *field, TCP4ADDRESS *address )
   {
      char *ptr = f4ptr( field ) ;
      unsigned char max = (unsigned char)f4len( field ) ;

      return tcp4charToAddress( ptr, max, address ) ;
   }



   void tcp4addressToChar( char *result, TCP4ADDRESS tcpAddress )
   {
      memset( result, ' ', 15 ) ;
      result[15] = 0 ;

      c4ltoa45( tcpAddress.tcpAddress[0], result, 3 ) ;
      result[3] = '.' ;
      c4ltoa45( tcpAddress.tcpAddress[1], result+4, 3 ) ;
      result[7] = '.' ;
      c4ltoa45( tcpAddress.tcpAddress[2], result+8, 3 ) ;
      result[11] = '.' ;
      c4ltoa45( tcpAddress.tcpAddress[3], result+12, 3 ) ;
   }



   int S4FUNCTION account4userInit( ACCOUNT4USER *userAccount, ACCOUNT4 *accounts, PRIVILEGE4 *privilege,
                                    TCP4ADDRESS tcpAddress, const char *id, const char *password )
   {
      #ifdef E4PARM_LOW
         if ( userAccount == NULL || accounts == NULL || privilege == NULL || id == NULL || password == NULL )
            return error4( NULL, e4parmNull, E96801 ) ;
      #endif

      assert5( accounts->data != 0 ) ;
      CODE4 *c4 = accounts->data->codeBase ;

      assert5( c4 != 0 ) ;

      if ( error4code( c4 ) < 0 )
         return -1 ;

      memset( userAccount, 0, sizeof( ACCOUNT4USER ) ) ;
      char seekKey[LEN4ACCOUNT_ID+LEN4PASSWORD] ;
      memset( seekKey, ' ', sizeof( seekKey ) ) ;
      int len = strlen( id ) ;

      if ( len > LEN4ACCOUNT_ID )
         return error4( c4, e4invalidUserId, E96801 ) ;

      memcpy( userAccount->accountId, id, len ) ;
      userAccount->accountId[len] = 0 ;
      c4upper( userAccount->accountId ) ;
   //   memset( userAccount->accountId+len, ' ', LEN4ACCOUNT_ID - len ) ;
      memcpy( seekKey, userAccount->accountId, len ) ;
      len = strlen( password ) ;

      if ( len > LEN4PASSWORD )
         return error4( c4, e4invalidPassword, E96801 ) ;

      memcpy( seekKey + LEN4ACCOUNT_ID, password, len ) ;
      tcp4addressToChar( userAccount->tcpAddress, tcpAddress ) ;

      userAccount->accounts = accounts ;
      userAccount->privileges = privilege ;

      // #ifdef S4OFF_COMMUNICATIONS
      //    /* just assume full admin privileges */
      //    userAccount->allowRead = TRUE ;
      //    userAccount->allowAppend = TRUE ;
      //    userAccount->allowDelete = TRUE ;
      //    userAccount->allowUpdate = TRUE ;
      //    userAccount->allowIndex = TRUE ;
      //    userAccount->allowCompress = TRUE ;
      //    userAccount->allowCreateDel = TRUE ;
      //    userAccount->allowCreate = TRUE ;
      //    userAccount->allowCreateTmp = TRUE ;
      //    userAccount->allowDisconnect = TRUE ;
      //    return 0 ;
      // #else

      // if not communications, assume the tcp address is equal...
      #ifndef S4OFF_COMMUNICATIONS
         TCP4ADDRESS localAddress = connect4localTcpAddress( &c4->currentClient->connect ) ;
         if ( memcmp( &tcpAddress, &localAddress, sizeof( TCP4ADDRESS ) ) == 0 )
      #endif
      {
         // first check if admin account...
         // matches, so we are admin, else just fall through to normal coding
         if ( strcmp( userAccount->accountId, "ADMIN" ) == 0 )
         {
            userAccount->allowRead = TRUE ;
            userAccount->allowAppend = TRUE ;
            userAccount->allowDelete = TRUE ;
            userAccount->allowUpdate = TRUE ;
            userAccount->allowIndex = TRUE ;
            userAccount->allowCompress = TRUE ;
            userAccount->allowCreateDel = TRUE ;
            userAccount->allowCreate = TRUE ;
            userAccount->allowCreateTmp = TRUE ;
            userAccount->allowDisconnect = TRUE ;
            return 0 ;
         }
      }

      // AS Sept. 18/02 - need to ensure we are using catalogClient when manipulating the acounts table which
      // was opened at that level (for propery lock handling)
      SERVER4CLIENT *oldClient = c4->currentClient ;
      c4->currentClient = c4->catalogClient ;
      d4tagSelect( accounts->data, d4tag( accounts->data, "NAMEPASS" ) ) ;
      int rc = d4seekN( accounts->data, seekKey, sizeof( seekKey ) ) ;

      if ( rc != 0 )  // check if password only was bad
      {
         int rc = d4seekN( accounts->data, seekKey, LEN4ACCOUNT_ID ) ;
         c4->currentClient = oldClient ;
         if ( rc == 0 )
            return error4( c4, e4invalidPassword, E96801 ) ;
         return error4( c4, e4invalidUserId, E96801 ) ;
      }

      c4->currentClient = oldClient ;

      #ifndef S4OFF_COMMUNICATIONS
         // if next succeeds, it means we are not logged on local to server, so do tcpip check
         if ( memcmp( &tcpAddress, &localAddress, sizeof( TCP4ADDRESS ) ) != 0 )
         {
            TCP4ADDRESS tcpBegin ;
            int rc1 = f4tcpAddress( accounts->tcpBeginField, &tcpBegin ) ;

            if ( rc1 < 0 )
               return error4( c4, e4invalidTcpAddress, E96801 ) ;

            if ( rc1 == r4blankTcpAddress )
               tcpBegin = c4->server->tcpBegin ;

            TCP4ADDRESS tcpEnd ;
            int rc2 = f4tcpAddress( accounts->tcpEndField, &tcpEnd ) ;

            if ( rc2 < 0 )
               return error4( c4, e4invalidTcpAddress, E96801 ) ;

            if ( rc2 == r4blankTcpAddress )
               tcpEnd = c4->server->tcpEnd ;

            if ( memcmp( &tcpAddress, &tcpBegin, sizeof( TCP4ADDRESS ) ) < 0 || memcmp( &tcpAddress, &tcpEnd, sizeof( TCP4ADDRESS ) ) > 0 )
               return error4( c4, e4invalidTcpAddress, E96801 ) ;
         }
      #endif

      userAccount->allowRead = f4true( accounts->readField ) ;
      userAccount->allowAppend = f4true( accounts->appendField ) ;
      userAccount->allowDelete = f4true( accounts->deleteField ) ;
      userAccount->allowUpdate = f4true( accounts->updateField ) ;
      userAccount->allowIndex = f4true( accounts->indexField ) ;
      userAccount->allowCompress = f4true( accounts->compressField ) ;
      userAccount->allowCreateDel = f4true( accounts->createDelField ) ;
      userAccount->allowCreate = f4true( accounts->createField ) ;
      userAccount->allowDisconnect = f4true( accounts->disconnectField ) ;

      if ( userAccount->allowCreate != FALSE || userAccount->allowCreateDel != FALSE )
         userAccount->allowCreateTmp = TRUE ;
      else
         userAccount->allowCreateTmp = f4true( accounts->createTmpField ) ;

      return 0 ;
      // #endif /* not S4OFF_COMMUNICATIONS */
   }



   Bool5 S4FUNCTION account4userAllowReadLow( ACCOUNT4USER *userAccount, DATA4 *data )
   {
      assert5( userAccount != 0 && data != 0 ) ;
      assert5( userAccount->privileges != 0 ) ;

      if ( userAccount->allowRead != FALSE || data->allowRead == TRUE )
         return TRUE ;

      // all DATA4 permission values default to FALSE, so verify manually to see if allowed

      data->allowRead = privilege4allowRead( userAccount->privileges, userAccount->accountId, data->dataFile->file.name ) ;
      return data->allowRead ;
   }



   Bool5 S4FUNCTION account4userAllowAppend( ACCOUNT4USER *userAccount, DATA4 *data )
   {
      assert5( userAccount != 0 && data != 0 ) ;
      assert5( userAccount->privileges != 0 ) ;
      if ( userAccount->allowAppend != FALSE )
         return TRUE ;

      if ( data->allowAppend == TRUE )
         return TRUE ;

      data->allowAppend = privilege4allowAppend( userAccount->privileges, userAccount->accountId, data->dataFile->file.name ) ;
      return data->allowAppend ;
   }



   Bool5 S4FUNCTION account4userAllowDelete( ACCOUNT4USER *userAccount, DATA4 *data )
   {
      assert5( userAccount != 0 && data != 0 ) ;
      assert5( userAccount->privileges != 0 ) ;
      if ( userAccount->allowDelete != FALSE )
         return TRUE ;

      if ( data->allowDelete == TRUE )
         return TRUE ;

      data->allowDelete = privilege4allowDelete( userAccount->privileges, userAccount->accountId, data->dataFile->file.name ) ;
      return data->allowDelete ;
   }



   Bool5 S4FUNCTION account4userAllowUpdate( ACCOUNT4USER *userAccount, DATA4 *data )
   {
      assert5( userAccount != 0 && data != 0 ) ;
      assert5( userAccount->privileges != 0 ) ;
      if ( userAccount->allowUpdate != FALSE )
         return TRUE ;

      if ( data->allowUpdate == TRUE )
         return TRUE ;

      data->allowUpdate = privilege4allowUpdate( userAccount->privileges, userAccount->accountId, data->dataFile->file.name ) ;
      return data->allowUpdate ;
   }



   Bool5 S4FUNCTION account4userAllowIndex( ACCOUNT4USER *userAccount, DATA4 *data )
   {
      assert5( userAccount != 0 && data != 0 ) ;
      assert5( userAccount->privileges != 0 ) ;
      if ( userAccount->allowIndex != FALSE )
         return TRUE ;

      if ( data->allowIndex == TRUE )
         return TRUE ;

      data->allowIndex = privilege4allowIndex( userAccount->privileges, userAccount->accountId, data->dataFile->file.name ) ;
      return data->allowIndex ;
   }



   Bool5 S4FUNCTION account4userAllowCompress( ACCOUNT4USER *userAccount, DATA4 *data )
   {
      assert5( userAccount != 0 && data != 0 ) ;
      assert5( userAccount->privileges != 0 ) ;
      if ( userAccount->allowCompress != FALSE )
         return TRUE ;

      if ( data->allowCompress == TRUE )
         return TRUE ;

      data->allowCompress = privilege4allowCompress( userAccount->privileges, userAccount->accountId, data->dataFile->file.name ) ;
      return data->allowCompress ;
   }



   Bool5 S4FUNCTION privilege4allowDo( PRIVILEGE4 *privilege, const char *accountId, const char *fullPathName, FIELD4 *field )
   {
      char seekKey[241] ;
      int rc, len ;
      DATA4 *d4 = privilege->privilegeData ;

      /* first check access rights to the directory */
      memset( seekKey+LEN4ACCOUNT_ID, 0, sizeof( seekKey ) - LEN4ACCOUNT_ID) ;
      memcpy( seekKey, accountId, LEN4ACCOUNT_ID ) ;
      memset( seekKey+strlen( seekKey ), ' ', LEN4ACCOUNT_ID-strlen( seekKey ) ) ;  // blank out extras for id
      // AS 07/05/00 - moved upper to after the path is appended...
      u4namePath( seekKey + LEN4ACCOUNT_ID, 240 - LEN4ACCOUNT_ID, fullPathName ) ;
      #ifndef S4UNIX
         c4upper( seekKey ) ;
      #endif

      TAG4 *tag = d4tag( d4, "PRIVILEG" ) ;
      d4tagSelect( d4, tag ) ;
      int keyLen = tfile4keyLen( tag->tagFile ) ;

      // seek whole key (i.e. blanks in the alias field)
      len = strlen( seekKey ) ;
      // AS 07/05/00 - was doing only a partial seek, so was succeeding where it shouldn't
      seekKey[len] = '|' ;
      memset( seekKey+len+1, ' ', keyLen - len ) ;
      assert5( keyLen < sizeof( seekKey ) ) ;
      seekKey[keyLen] = 0 ;
      rc = d4seek( d4, seekKey ) ;
      if ( rc == 0 ) // found
         if ( f4true( field ) )
            return TRUE ;

      // else check for with alias (if not found or not sufficient rights yet)
      // get name and extension
      u4namePiece( seekKey + len + 1, 240 - len - 1, fullPathName, 0, 1 ) ;
      // AS Jan 28/02 - was not upper casing the added table name...
      #ifndef S4UNIX
         c4upper( seekKey ) ;
      #endif
      rc = d4seek( d4, seekKey ) ;
      if ( rc == 0 ) // found
         return f4true( field ) ;

      error4set( d4->codeBase, e4authorize ) ;

      return FALSE ;
   }



   int S4FUNCTION privilege4init( CODE4 *c4, PRIVILEGE4 *privilege, const char *privilegeFileName, int firstTimeThrough )
   {
      int oldTagErr, oldOpenErr, oldAutoOpen, oldAccessMode, oldSingleClient, oldSafety ;
      TAG4 *tag ;
      char *name = NULL ;
      char buf[258], buf2[258] ;

      #ifdef E4PARM_LOW
         if ( c4 == NULL || privilege == NULL || privilegeFileName == NULL )
            return error4( c4, e4parmNull, E96801 ) ;
      #endif

      if ( error4code( c4 ) < 0 )
         return -1 ;

      oldSingleClient = c4->singleClient ;
      c4->singleClient = OPEN4DENY_NONE ;
      oldAccessMode = c4->accessMode ;
      #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
         // for simba, we have a multi-user shared server, so the privileges file is shared...
         c4->accessMode = OPEN4DENY_NONE ;
      #else
         c4->accessMode = OPEN4DENY_RW ;
      #endif
      oldOpenErr = c4->errOpen ;
      c4->errOpen = 0 ;
      privilege->privilegeData = d4open( c4, privilegeFileName ) ;
      c4->errOpen = oldOpenErr ;
      if ( privilege->privilegeData == NULL )
      {
         if ( firstTimeThrough == 0 )  // already re-created, still a failure
            return error4describe( c4, e4open, E70228, privilegeFileName, 0, 0 ) ;
         oldAutoOpen = c4->autoOpen ;
         c4->autoOpen = 0 ;
         privilege->privilegeData = d4open( c4, privilegeFileName ) ;
         c4->autoOpen = oldAutoOpen ;
         if ( privilege->privilegeData == NULL )
         {
            c4->accessMode = oldAccessMode ;
            c4->singleClient = oldSingleClient ;
            return error4describe( c4, e4open, E70229, privilegeFileName, 0, 0 ) ;
         }
      }
      c4->accessMode = oldAccessMode ;
      c4->singleClient = oldSingleClient ;

      oldTagErr = c4->errTagName ;
      c4->errTagName = 0 ;
      tag = d4tag( privilege->privilegeData, "PRIVILEG" ) ;  // just ensure it exists
      c4->errTagName = oldTagErr ;

      // AS Mar 21/03 - Don't pack if we are the ODBC_BUILD (still do if the ODBC_ENABLED)
      #ifndef S4ODBC_BUILD
         d4pack( privilege->privilegeData ) ;
      #endif
      if ( tag == NULL )  // create index
      {
         if ( firstTimeThrough == 0 )  // already re-created, still a failure
            return error4describe( c4, e4open, E70228, privilegeFileName, 0, 0 ) ;
         /* must re-open in exclusive mode */
         d4close( privilege->privilegeData ) ;

         oldSingleClient = c4->singleClient ;
         c4->singleClient = OPEN4DENY_RW ;
         oldAccessMode = c4->accessMode ;
         #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
            // for simba, we have a multi-user shared server, so the privileges file is shared...
            c4->accessMode = OPEN4DENY_NONE ;
         #else
            c4->accessMode = OPEN4DENY_RW ;
         #endif
         oldOpenErr = c4->errOpen ;
         c4->errOpen = 0 ;
         oldAutoOpen = c4->autoOpen ;
         c4->autoOpen = 0 ;
         privilege->privilegeData = d4open( c4, privilegeFileName ) ;
         c4->autoOpen = oldAutoOpen ;
         c4->errOpen = oldOpenErr ;
         c4->accessMode = oldAccessMode ;
         c4->singleClient = oldSingleClient ;
         if ( privilege->privilegeData == NULL )
            return error4describe( c4, e4open, E70229, privilegeFileName, 0, 0 ) ;

         error4set( c4, 0 ) ;
         if ( code4indexFormat( c4 ) == r4ntx )  // Clipper must have a file name to create production index
         {
            /* AS 11/10/98 --> must include full path or put in wrong place and get endless loop */
            u4namePath( buf, sizeof( buf ), privilege->privilegeData->dataFile->file.name ) ;
            char drive[3], *path ;  /* LY 2003/12/18 : changed from *drive */
            if ( buf[1] == ':' )  // remove drive marker
            {
               /* LY 2003/12/18 : u4nameMake() expects drive to include ":"
               after drive letter (leaves drive letter off if ":" not present);
               if DEFPATH and SYSTEMPATH are on different drives, index and
               group files do not get created */
               strncpy( drive, buf, 2 ) ;
               drive[2] = 0 ;
               buf[1] = 0 ;
               path = buf+2 ;
            }
            else
            {
               drive[0] = 0 ; /* LY 2003/12/18 */
               path = buf ;
            }

            u4nameMake( buf2, sizeof( buf2 ), drive, path, "PRIV4" ) ;
            name = buf2 ;
         }
         if ( code4indexFormat( c4 ) == r4mdx )  // allow longer tags, but MDX incompatible
            c4->oledbSchemaCreate = 1 ;
         oldSafety = c4->safety ;
         c4->safety = 0 ;
         if ( i4create( privilege->privilegeData, name, privilege4tags ) == NULL)
            error4( c4, e4authorize, E70104 ) ;
         else
         {
            tag = d4tag( privilege->privilegeData, "PRIVILEG" ) ;  // just ensure it exists
            if ( tag == NULL )  // failed somehow
               error4( c4, e4authorize, E70104 ) ;
         }
         c4->safety = oldSafety ;
         if ( code4indexFormat( c4 ) == r4mdx )  // allow longer tags, but MDX incompatible
            c4->oledbSchemaCreate = 0;  // allow longer tags, but MDX incompatible

         /* close and re-open in non-exclusive mode */
         d4close( privilege->privilegeData ) ;
         privilege->privilegeData = 0 ;  // in case open fails...
         return privilege4init( c4, privilege, privilegeFileName, 0 ) ; /* call ourselves, but should never get back here */
      }

      // AS Apr 15/03 - support for new lockId for shared clone locking
      d4unlockLow( privilege->privilegeData, data4lockId( privilege->privilegeData ), 1 ) ;

      privilege->accountIdField = d4field( privilege->privilegeData, "ACCOUNTID" ) ;
      privilege->pathField = d4field( privilege->privilegeData, "PATH" ) ;
      privilege->tableField = d4field( privilege->privilegeData, "TABLE" ) ;
      privilege->readField = d4field( privilege->privilegeData, "READ" ) ;
      privilege->appendField = d4field( privilege->privilegeData, "APPEND" ) ;
      privilege->deleteField = d4field( privilege->privilegeData, "DELETE" ) ;
      privilege->updateField = d4field( privilege->privilegeData, "UPDATE" ) ;
      privilege->indexField = d4field( privilege->privilegeData, "INDEX" ) ;
      privilege->compressField = d4field( privilege->privilegeData, "COMPRESS" ) ;

      if ( error4code( c4 ) < 0 )
      {
         privilege4initUndo( privilege ) ;
         return -1 ;
      }

      #ifdef E4DEBUG
         d4check( privilege->privilegeData ) ;
         // AS Apr 15/03 - support for new lockId for shared clone locking
         d4unlockLow( privilege->privilegeData, data4lockId( privilege->privilegeData ), 1 ) ;
      #endif

      return 0 ;
   }



   void S4FUNCTION privilege4initUndo( PRIVILEGE4 *privilege )
   {
      if ( privilege->privilegeData != NULL )
      {
         #ifdef E4DEBUG
            CODE4 *c4 = privilege->privilegeData->codeBase ;
            if ( c4->currentClient != c4->catalogClient || ! c4accessMutexCountOne( c4 ) )
               error4( c4, e4struct, E96801 ) ;
         #endif
         d4close( privilege->privilegeData ) ;
         privilege->privilegeData = NULL ;
         #ifdef E4DEBUG
            if ( c4->currentClient != c4->catalogClient || ! c4accessMutexCountOne( c4 ) )
               error4( c4, e4struct, E96801 ) ;
         #endif
      }
   }



   int S4FUNCTION account4init( CODE4 *c4, ACCOUNT4 *account, const char *accountFileName, int firstTimeThrough )
   {
      // if not first time through, then an error on open...
      int oldTagErr, oldOpenErr, oldAutoOpen, oldAccessMode, oldSingleClient, oldSafety ;
      TAG4 *tag ;
      char *name = NULL ;
      char buf[258], buf2[258] ;

      #ifdef E4PARM_LOW
         if ( c4 == NULL || account == NULL || accountFileName == NULL )
            return error4( c4, e4parmNull, E96801 ) ;
      #endif

      if ( error4code( c4 ) < 0 )
         return -1 ;

      oldSingleClient = c4->singleClient ;
      c4->singleClient = OPEN4DENY_NONE ;
      oldAccessMode = c4->accessMode ;
      #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
         // for simba, we have a multi-user shared server, so the accounts file is shared...
         c4->accessMode = OPEN4DENY_NONE ;
      #else
         c4->accessMode = OPEN4DENY_RW ;
      #endif
      oldOpenErr = c4->errOpen ;
      c4->errOpen = 0 ;
      account->data = d4open( c4, accountFileName ) ;
      c4->errOpen = oldOpenErr ;
      if ( account->data == NULL )  // try without index
      {
         if ( firstTimeThrough == 0 )  // already re-created, still a failure
            return error4describe( c4, e4open, E70228, accountFileName, 0, 0 ) ;
         oldAutoOpen = c4->autoOpen ;
         c4->autoOpen = 0 ;
         account->data = d4open( c4, accountFileName ) ;
         c4->autoOpen = oldAutoOpen ;
         if ( account->data == NULL )  // open failed
         {
            c4->accessMode = oldAccessMode ;
            c4->singleClient = oldSingleClient ;
            return error4describe( c4, e4open, E70228, accountFileName, 0, 0 ) ;
         }
      }
      c4->accessMode = oldAccessMode ;
      c4->singleClient = oldSingleClient ;

      oldTagErr = c4->errTagName ;
      c4->errTagName = 0 ;
      tag = d4tag( account->data, "NAMEPASS" ) ;  // just ensure it exists
      c4->errTagName = oldTagErr ;

      // AS Mar 21/03 - Don't pack if we are the ODBC_BUILD (still do if the ODBC_ENABLED)
      #ifndef S4ODBC_BUILD
         d4pack( account->data ) ;
      #endif

      if ( tag == NULL )  // create index
      {
         if ( firstTimeThrough == 0 )  // already re-created, still a failure
            return error4describe( c4, e4open, E70228, accountFileName, 0, 0 ) ;
         /* must re-open in exclusive mode */
         d4close( account->data ) ;

         oldSingleClient = c4->singleClient ;
         c4->singleClient = OPEN4DENY_RW ;
         oldAccessMode = c4->accessMode ;
         #if defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED )
            // for simba, we have a multi-user shared server, so the accounts file is shared...
            c4->accessMode = OPEN4DENY_NONE ;
         #else
            c4->accessMode = OPEN4DENY_RW ;
         #endif
         oldOpenErr = c4->errOpen ;
         c4->errOpen = 0 ;
         oldAutoOpen = c4->autoOpen ;
         c4->autoOpen = 0 ;
         account->data = d4open( c4, accountFileName ) ;
         c4->autoOpen = oldAutoOpen ;
         c4->errOpen = oldOpenErr ;
         c4->accessMode = oldAccessMode ;
         c4->singleClient = oldSingleClient ;
         if ( account->data == NULL )
            return error4describe( c4, e4open, E70228, accountFileName, 0, 0 ) ;

         error4set( c4, 0 ) ;
         if ( code4indexFormat( c4 ) == r4ntx )  // Clipper must have a file name to create production index
         {
            /* AS 11/10/98 --> must include full path or put in wrong place and get endless loop */
            u4namePath( buf, sizeof( buf ), account->data->dataFile->file.name ) ;
            char drive[3], *path ;  /* LY 2003/12/18 : changed from *drive */
            if ( buf[1] == ':' )  // remove drive marker
            {
               /* LY 2003/12/18 : u4nameMake() expects drive to include ":"
               after drive letter (leaves drive letter off if ":" not present);
               if DEFPATH and SYSTEMPATH are on different drives, index and
               group files do not get created */
               strncpy( drive, buf, 2 ) ;
               drive[2] = 0 ;
               buf[1] = 0 ;
               path = buf+2 ;
            }
            else
            {
               drive[0] = 0 ; /* LY 2003/12/18 */
               path = buf ;
            }

            u4nameMake( buf2, sizeof( buf2 ), drive, path, "ACCOUNT4" ) ;
            name = buf2 ;
         }
         oldSafety = c4->safety ;
         c4->safety = 0 ;
         if ( i4create( account->data, name, account4tags ) == 0 )
            error4( c4, e4authorize, E70104 ) ;
         else
         {
            tag = d4tag( account->data, "NAMEPASS" ) ;  // just ensure it exists
            if ( tag == NULL )  // failed somehow
               error4( c4, e4authorize, E70104 ) ;
         }
         c4->safety = oldSafety ;
         /* close and re-open in non-exclusive mode */
         d4close( account->data ) ;
         account->data = 0 ;  // in case open fails...
         return account4init( c4, account, accountFileName, 0 ) ;
      }

      // AS Apr 15/03 - support for new lockId for shared clone locking
      d4unlockLow( account->data, data4lockId( account->data ), 1 ) ;

      account->accountIdField = d4field( account->data, "ACCOUNTID" ) ;
      account->passwordField = d4field( account->data, "PASSWORD" ) ;
      account->tcpBeginField = d4field( account->data, "TCP_BEGIN" ) ;
      account->tcpEndField = d4field( account->data, "TCP_END" ) ;
      account->createField = d4field( account->data, "CREATE" ) ;
      account->createDelField = d4field( account->data, "CREATE_DEL" ) ;
      account->createTmpField = d4field( account->data, "CREATE_TMP" ) ;
      account->readField = d4field( account->data, "READ" ) ;
      account->appendField = d4field( account->data, "APPEND" ) ;
      account->deleteField = d4field( account->data, "DELETE" ) ;
      account->updateField = d4field( account->data, "UPDATE" ) ;
      account->indexField = d4field( account->data, "INDEX" ) ;
      account->compressField = d4field( account->data, "COMPRESS" ) ;
      account->disconnectField = d4field( account->data, "DISCONNECT" ) ;

      if ( error4code( c4 ) < 0 )
      {
         account4initUndo( account ) ;
         return -1 ;
      }

      #ifdef E4DEBUG
         d4check( account->data ) ;
         // AS Apr 15/03 - support for new lockId for shared clone locking
         d4unlockLow( account->data, data4lockId( account->data ), 1 ) ;
      #endif

      return 0 ;
   }



   void S4FUNCTION account4initUndo( ACCOUNT4 *account )
   {
      if ( account->data != NULL )
      {
         #ifdef E4DEBUG
            CODE4 *c4 = account->data->codeBase ;
            if ( c4->currentClient != c4->catalogClient || ! c4accessMutexCountOne( c4 ) )
               error4( c4, e4struct, E96801 ) ;
         #endif

         d4close( account->data ) ;
         account->data = NULL ;
         #ifdef E4DEBUG
            if ( c4->currentClient != c4->catalogClient || ! c4accessMutexCountOne( c4 ) )
               error4( c4, e4struct, E96801 ) ;
         #endif
      }
   }



   Bool5 S4FUNCTION account4userAllowCreate( ACCOUNT4USER *userAccount, int isTemp, int doReplace )
   {
      if ( isTemp )
         return userAccount->allowCreateTmp ;

      if ( doReplace )
         return userAccount->allowCreateDel ;

      return userAccount->allowCreate ;
   }
#endif /* S4SERVER */
