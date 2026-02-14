#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

#ifndef S4OFF_COMMUNICATIONS
#ifdef S4JAVA

#include "j4java.h"

static int java4messageSetAnyCharValue( CODE4 *, CONNECT4 *, SERVER4CLIENT *, char * ) ;
static int java4messageStatusField( CODE4 *, CONNECT4 *, SERVER4CLIENT *, FIELD4 *, char ) ;
static int java4messageStatusLow( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client, int doFields ) ;

#define java4messageStatusFields( cb, connect, client ) ( java4messageStatusLow( (cb), (connect), (client), (1) ) )
#define java4messageStatus( cb, connect, client )  java4messageStatusLow( (cb), (connect), (client), (0) )

#define java4socketRead( cb, connect, buf, len ) ( java4socketReadLow( (cb), (connect), (buf), (len), 0, 0 ) )
#define java4socketReadChar( cb, connect, c ) ( java4socketRead( (cb), (connect), (c), 1 ) )

#define java4socketReceive( cb, connect, buf, len, ftt ) ( java4socketReadLow( (cb), (connect), (buf), (len), 1, ftt ) )

#define java4socketWriteChar( cb, connect, c ) ( java4socketWrite( (cb), (connect), &(c), 1 ) )
#define java4socketWrite( cb, connect, buf, len ) ( java4socketWriteExtended( (cb), (connect), (buf), (len) ) )

static int java4socketReadDouble( CODE4 *, CONNECT4 *, double *, char ) ;
static int java4socketReadExtended( CODE4 *, CONNECT4 *, void *, unsigned long ) ;
static int java4socketReadLong( CODE4 *, CONNECT4 *, long * ) ;
static int java4socketReadLow( CODE4 *, CONNECT4 *connect, void *, unsigned short, short, char ) ;
static int java4socketReadShort( CODE4 *, CONNECT4 *, short * ) ;
static int java4socketReadString( CODE4 *, CONNECT4 *, char **, short, long * ) ;
static int java4socketSend( CODE4 *, CONNECT4 * ) ;

/* static int java4socketWrite( CODE4 *, CONNECT4 *, void *, unsigned short ) ; */
static int java4socketWriteDouble( CODE4 *, CONNECT4 *, double ) ;
static int java4socketWriteExtended( CODE4 *, CONNECT4 *, const void *, unsigned long ) ;
static int java4socketWriteLong( CODE4 *, CONNECT4 *, long ) ;
static int java4socketWriteShort( CODE4 *, CONNECT4 *, short ) ;



#ifdef S4LOCAL
#ifndef E4OFF_STRING
   long S4FUNCTION s4WSAError(void)
   {
      long err ;

      err = WSAGetLastError()-WSABASEERR ;
      if (err > 1000)   /* shrink the numbering gaps*/
         err -= 907 ; /* 1001-94*/
      if (err > 90)
         err -= 19 ;  /* 91-72*/
      if (err > 34)
         err -= 10 ;  /* 35-25*/
      if (err > 23)
         err-- ;      /* 24-23*/
      if (err > 21)
         err -= 7 ;   /* 22-15*/
      if (err > 12)
         err -= 3 ;   /* 13-10*/
      if (err > 8)
         err -= 4 ;   /* 9-5*/
      err -= 4 ;
      return (S4SOCK_BASE_ERROR_DEF+err) ;
   }
#endif
#endif



static void java4freeFieldsTags( FIELD4INFO *fields, CONNECTION4TAG_INFO *tags )
{
   int i ;

   if ( fields != 0 )
   {
      for ( i = 0 ;; i++ )
      {
         if ( fields[i].name == 0 )
            break ;
         u4free( fields[i].name ) ;
      }
      u4free( fields ) ;
   }

   if ( tags != 0 )
   {
      for ( i = 0 ;; i++ )
      {
         if ( tags[i].name.ptr == 0 )
            break ;
         u4free( tags[i].name.ptr ) ;
         u4free( tags[i].expression.ptr ) ;
         u4free( tags[i].filter.ptr ) ;
      }
      u4free( tags ) ;
   }
}



static int java4getFields( CODE4 *cb, DATA4 *d4, CONNECT4 *connect )
{
   FIELD4 *field ;
   int i, rc ;
   char cLen, *memoPtr ;
   long len, len2 ;
   short sLen ;
   double dbl ;

   for ( i = 0 ; i < d4->numFieldsDefined ; i++ )
   {
      if ( d4->fieldsDefined[i] == -1 )  /* deleted flag */
      {
         java4socketRead( cb, connect, &cLen, sizeof( cLen ) ) ;
         if ( cLen == java4defaultFieldLen )
            cLen = 1 ;

         rc = java4socketRead( cb, connect, d4->record, cLen ) ;
         if ( rc != 0 )
            return rc ;
      }
      else
      {
         field = d4fieldJ( d4, d4->fieldsDefined[i] ) ;
         rc = java4socketRead( cb, connect, &cLen, sizeof( cLen ) ) ;
         if ( rc != 0 )
            return rc ;
         if ( cLen <= 0 )
         {
            switch( cLen )
            {
               case java4blankField:
                  f4blank( field ) ;
                  len = -1 ;
                  break ;
               case java4nullField:
                  #ifdef S4FOX
                     if ( d4compatibility( d4 ) == 30 )
                        f4assignNull( field ) ;
                     else
                  #endif
                     f4blank( field ) ;
                  len = -1 ;
                  break ;
               case java4defaultFieldLen:
                  if ( f4type( field ) == r4memo
                     #ifdef S4FOX
                        || f4type( field ) == r4gen || f4type( field ) == r4memoBin
                     #endif
                     )
                     len = 0 ;   /* for memo fields, default len means empty string means len=0 */
                  else
                     len = f4len( field ) ;
                  break ;
               case java4shortLen:
                  rc = java4socketReadShort( cb, connect, &sLen ) ;
                  len = sLen ;
                  break ;
               case java4longLen:
                  rc = java4socketReadLong( cb, connect, &len ) ;
                  break ;
               case java4noChange:  /* means do no assign */
                  len = -1 ;
                  break ;
               case java4doubleField:  /* means assign double */
                  java4socketReadDouble( cb, connect, &dbl, 0 ) ;
                  f4assignDouble( field, dbl ) ;
                  len = -1 ;
                  break ;
               default:
                  return -1 ;
            }
            if ( rc != 0 )
               return rc ;
         }
         else
            len = cLen ;

         if ( len == -1 )   /* blank or null or no change */
            continue ;

         switch( f4type( field ) )  /* memo's must use assign */
         {
            case r4memo:     /* variable length fields */
            #ifdef S4FOX
               case r4gen:
               case r4memoBin:
            #endif
               if ( len != 0 )
               {
                  memoPtr = (char *)u4allocFree( cb, len ) ;
                  if ( memoPtr == 0 )
                     return e4memory ;
                  rc = java4socketReadExtended( cb, connect, memoPtr, len ) ;
                  if ( rc == 0 )
                  {
                     f4memoAssignN( field, memoPtr, len ) ;
                     u4free( memoPtr ) ;
                  }
               }
               break ;
            default:
               len2 = (long)f4len( field ) ;
               if ( len > len2 )
                  return e4len ;

               rc = java4socketReadExtended( cb, connect, d4->record + field->offset, len ) ;
               /* blank out the remainder of the field, if any */
               len2 -= len ;
               if ( len2 > 0 )
               {
                  // AS Jun 28/02 - not doing a proper blank - sometimes null data is present in blank fields
                  // memset( d4->record + field->offset + len, ' ', len2 ) ;
                  memcpy( d4->record + field->offset + len, d4->recordBlank + field->offset + len, len2 ) ;
               }
               break ;
         }
         if ( rc != 0 )
            return rc ;
      }
   }

   return 0 ;
}



static int java4getFields2( CODE4 *cb, DATA4 *d4, CONNECT4 *connect )
{
   // CS 2000/03/20
   FIELD4 *field ;
   short numFields, jField ;

   int i, rc ;
   char cLen, *memoPtr ;
   long len, len2 ;
   short sLen ;
   double dbl ;

   // client says how many fields are being sent
   rc = java4socketReadShort( cb, connect, &numFields ) ;
   if ( rc != 0 )
      return rc ;

   for ( i = numFields ; i ; i-- )
   {
      // client gives the field number
      rc = java4socketReadShort( cb, connect, &jField ) ;
      if ( rc != 0 )
         return rc ;

      if ( jField == -1 )  // deleted flag
      {
         java4socketRead( cb, connect, &cLen, sizeof( cLen ) ) ;
         if ( cLen == java4defaultFieldLen )
            cLen = 1 ;

         rc = java4socketRead( cb, connect, d4->record, cLen ) ;
         if ( rc != 0 )
            return rc ;
      }
      else
      {
         field = d4fieldJ( d4, jField ) ;
         rc = java4socketRead( cb, connect, &cLen, sizeof( cLen ) ) ;
         if ( rc != 0 )
            return rc ;
         if ( cLen <= 0 )
         {
            switch( cLen )
            {
               case java4blankField:
                  f4blank( field ) ;
                  len = -1 ;
                  break ;
               case java4nullField:
                  #ifdef S4FOX
                     if ( d4compatibility( d4 ) == 30 )
                        f4assignNull( field ) ;
                     else
                  #endif
                     f4blank( field ) ;
                  len = -1 ;
                  break ;
               case java4defaultFieldLen:
                  if ( f4type( field ) == r4memo
                     #ifdef S4FOX
                        || f4type( field ) == r4gen || f4type( field ) == r4memoBin
                     #endif
                     )
                     len = 0 ;   /* for memo fields, default len means empty string means len=0 */
                  else
                     len = f4len( field ) ;
                  break ;
               case java4shortLen:
                  rc = java4socketReadShort( cb, connect, &sLen ) ;
                  len = sLen ;
                  break ;
               case java4longLen:
                  rc = java4socketReadLong( cb, connect, &len ) ;
                  break ;
               case java4noChange:  /* means do no assign */
                  len = -1 ;
                  break ;
               case java4doubleField:  /* means assign double */
                  java4socketReadDouble( cb, connect, &dbl, 0 ) ;
                  f4assignDouble( field, dbl ) ;
                  len = -1 ;
                  break ;
               default:
                  return -1 ;
            }
            if ( rc != 0 )
               return rc ;
         }
         else
            len = cLen ;

         if ( len == -1 )   /* blank or null or no change */
            continue ;

         switch( f4type( field ) )  /* memo's must use assign */
         {
            case r4memo:     /* variable length fields */
            #ifdef S4FOX
               case r4gen:
               case r4memoBin:
            #endif
               if ( len != 0 )
               {
                  memoPtr = (char *)u4allocFree( cb, len ) ;
                  if ( memoPtr == 0 )
                     return e4memory ;
                  rc = java4socketReadExtended( cb, connect, memoPtr, len ) ;
                  if ( rc == 0 )
                  {
                     f4memoAssignN( field, memoPtr, len ) ;
                     u4free( memoPtr ) ;
                  }
               }
               break ;
            default:
               len2 = (long)f4len( field ) ;
               if ( len > len2 )
                  return e4len ;

               rc = java4socketReadExtended( cb, connect, d4->record + field->offset, len ) ;
               /* blank out the remainder of the field, if any */
               len2 -= len ;
               if ( len2 > 0 )
               {
                  // AS Jun 28/02 - not doing a proper blank - sometimes null data is present in blank fields
                  // memset( d4->record + field->offset + len, ' ', len2 ) ;
                  memcpy( d4->record + field->offset + len, d4->recordBlank + field->offset + len, len2 ) ;
               }
               break ;
         }
         if ( rc != 0 )
            return rc ;
      }
   }

   return 0 ;
}



static int java4messageAppend( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   rc = d4appendStart( data, 0 ) ;
   if ( rc != 0 )
      return rc ;

   d4blank( data ) ;

   rc = java4getFields( cb, data, connect ) ;
   if ( rc != 0 )
      return rc ;

   return D4append( data, data4clientId( data ), data4serverId( data ) ) ;
}



static int java4messageAppend2( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   // CS 2000/03/20
   DATA4 *data ;
   int rc ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   rc = d4appendStart( data, 0 ) ;
   if ( rc != 0 )
      return rc ;

   d4blank( data ) ;

   rc = java4getFields2( cb, data, connect ) ;
   if ( rc != 0 )
      return rc ;

   return D4append( data, data4clientId( data ), data4serverId( data ) ) ;
}



static int java4messageBlank( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   d4blank( client->currentData ) ;
   return 0 ;
}



static int java4messageBottom( SERVER4CLIENT *client )
{
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   return D4bottom( client->currentData ) ;
}



static int java4messageClose( SERVER4CLIENT *client )
{
   int rc ;
   DATA4 *data ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;
   if ( data->fieldsDefined != 0 )
   {
      data->numFieldsDefined = 0 ;
      u4free( data->fieldsDefined ) ;
      data->fieldsDefined = 0 ;
   }
   rc = D4close( data ) ;
   client->currentData = 0 ;
   return rc ;
}



int java4messageConnect( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   short portNo ;
   long version ;
   char *userName, *pass = 0 ;
   TCP4ADDRESS address;
   #ifdef S4HALFJAVA
      CONNECT4HALF *javaPtr ;
   #else
      C4ADDRESS actualAddress ;
      C4NETID netId ;
      short addrLen ;
   #endif
   CODE4 *c4 = client->server->c4 ;

   // AS 08/21/00 - was not checking valid return code here before proceding.
   int rc = connect4receive(&client->connect, (char *)&portNo, sizeof( short ), code4timeoutVal(client->connect.cb) ) ;
   if ( rc != 0 )
      return rc ;

   /* We'll get the address from the socket */
   CONNECT4BUFFER *connectBuffer = &client->connect.connectBuffer ;
   connect4lowEnterExclusive( connectBuffer ) ;
   address = connect4lowPeerTcpAddress( connect4bufferLowGet( connectBuffer ) ) ;
   // AS Dec 8/03 - Need to track if the connection is a java connection at the low level (to determine if length and compress/encrypt bytes are being sent)
   CONNECT4LOW *conLow = connect4bufferLowGet( connectBuffer ) ;
   conLow->javaClient = 1 ;

   #ifdef S4HALFJAVA
      /* March 3, 1998. Java now connects twice to the server */
      #ifndef S4OFF_THREAD
         list4mutexWait(&c4->connectHalfListMutex) ;
      #endif
      javaPtr = code4findFirstConnection(c4, address, portNo, 0 ) ;
      if (javaPtr)
      {
         short returnValue = r4success ;
         connect4bufferLowGet( connectBuffer )->sockw = javaPtr->sock ;
         // AS May 13/02 - communications compression coding - don't compress the connection message
         assert5port( "Support for compression" ) ;
         connect4lowWrite( connect4bufferLowGet( connectBuffer ), (char *)&returnValue, 2, 0 ) ;
         mem4free(c4->connectHalfMemory, javaPtr) ;
      }
      else
      {
         // AS 03/09/01 - changed coding to be more robust.  previously memory corruption was possible on server
         // client->connectHalf.address = address ;
         // client->connectHalf.port = portNo ;
         // client->connectHalf.fromClient = 1 ;
         // client->connectHalf.client = client ;

         javaPtr = (CONNECT4HALF *)mem4allocZero( c4->connectHalfMemory ) ;
         if ( javaPtr == 0 )
         {
            #ifndef S4OFF_THREAD
               list4mutexRelease( &c4->connectHalfListMutex ) ;
            #endif
            connect4lowExitExclusive( connectBuffer ) ;
            return error4( c4, e4memory, E96978 ) ;
         }
         // address and port are sufficient to uniquely identify the connection
         javaPtr->address = address ;
         javaPtr->port = portNo ;
         javaPtr->fromClient = 1 ;
         // javaPtr->sock = newSocket->sockr ;
         javaPtr->client = client ;
         client->connectHalf = javaPtr ;  // for cleanup and reference purposes

         // AS Aug 30/01 - Was corrupting memory here.
         #ifdef S4OFF_THREAD
            l4add( &c4->connectHalfList, javaPtr) ;
         #else
            l4add( &c4->connectHalfListMutex.list, javaPtr) ;
         #endif
      }
      #ifndef S4OFF_THREAD
         list4mutexRelease(&cb->connectHalfListMutex) ;
      #endif
      connect4lowExitExclusive( connectBuffer ) ;
   #else
       //     java4socketReadShort( cb, connect, &portNo ) ;
      memcpy(&netId, &address, 4 ) ;
      address4makeNetID(&actualAddress, &addrLen,  netId, portNo ) ;
      u4delayHundredth( 50 ) ; /* give the java client time to set up the connection.  No big deal for a worker thread to wait for a small amount of time */

      rc = connect4lowConnect( connect4bufferLowGet( connectBuffer ), &actualAddress, addrLen ) ;
      connect4lowExitExclusive( connectBuffer ) ;
      if ( rc != 0 )
         return rc ;
   #endif

   /* 08/14/97 AS --> work-around because java clients only have 1 socket, not 2
   conect->connectBuffer.connectLow->sockw = connect->connectBuffer.connectLow->sockr ;

   - work around removed because implementing dual sockets on java
   */

   client->javaClient = 1 ;   /* this client is a java client */

   client->unlockAuto = LOCK4ALL ;

   java4socketReadLong( cb, connect, &version ) ;
   rc = java4socketReadString( cb, connect, &userName, sizeof( char ), 0 ) ;
   if ( rc != 0 )
      return rc ;
   rc = java4socketReadString( cb, connect, &pass, sizeof( char ), 0 ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;

   /* note that for java clients there is no way to change the date-format, leave at default... */
   memcpy( client->trans.dateFormat, "MM/DD/YY", 9 ) ;

   /* CS 2000/07/07 Keep the server backward compatible with the java clients.
   The client will be allowed to connect if the version of the server is the
   same or greater than that of the client. If the client has a lower version,
   fool Server4clientTranAddUser into thinking that the version is the same. */
   if (version < S4VERSION)
      version = S4VERSION ;
   rc = Server4clientTranAddUser( client, version, client->id, userName, ( unsigned short )strlen( userName ), pass, connect4peerTcpAddress( connect ) ) ;
   u4free( userName ) ;
   u4free( pass ) ;
   return rc ;
}



static int java4messageCreate( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   /* if succeeds, returns r4done to indicate that rc and data sent back to client */
   char *dataName ;
   DATA4 *data ;
   short numFields, i, numTags ;
   FIELD4INFO *fields ;
   CONNECTION4TAG_INFO *tags ;   /* use because not const */
   int rc ;
   TAG4 *tag ;

   fields = 0 ;
   tags = 0 ;
   data = 0 ;
   rc = 0 ;

   if ( java4socketReadString( cb, connect, &dataName, sizeof( short ), 0 ) != 0 )
      return -1 ;

   if ( java4socketReadShort( cb, connect, &numFields ) != 0 )
   {
      u4free( dataName ) ;
      return -1 ;
   }

   fields = (FIELD4INFO *)u4allocFree( cb, (numFields + 1) * sizeof( FIELD4INFO ) ) ;
   for ( i = 0 ; rc == 0 && i < numFields ; i++ )
   {
      if ( rc == 0 )
         rc = java4socketReadString( cb, connect, &fields[i].name, sizeof( char ), 0 ) ;
      if ( rc == 0 )
         rc = java4socketRead( cb, connect, &fields[i].type, 1 ) ;
      if ( rc == 0 )
         rc = java4socketReadShort( cb, connect, (short *)(&fields[i].len) ) ;
      if ( rc == 0 )
         rc = java4socketRead( cb, connect, &fields[i].dec, 1 ) ;
      if ( rc == 0 )
         rc = java4socketRead( cb, connect, &fields[i].nulls, 1 ) ;
   }

   if ( rc == 0 )
   {
      rc = java4socketReadShort( cb, connect, &numTags ) ;

      if ( numTags > 0 && rc == 0 )
      {
         tags = (CONNECTION4TAG_INFO *)u4allocFree( cb, (numTags + 1) * sizeof( TAG4INFO ) ) ;

         for ( i = 0 ; rc == 0 && i < numTags; i++ )
         {
            if ( rc == 0 )
               rc = java4socketReadString( cb, connect, &tags[i].name.ptr, sizeof( char ), 0 ) ;
            if ( rc == 0 )
               rc = java4socketReadString( cb, connect, &tags[i].expression.ptr, sizeof( short ), 0 ) ;
            if ( rc == 0 )
               rc = java4socketReadString( cb, connect, &tags[i].filter.ptr, sizeof( short ), 0 ) ;
            if ( rc == 0 )
               rc = java4socketReadShort( cb, connect, &tags[i].unique ) ;
            if ( rc == 0 )
               rc = java4socketReadShort( cb, connect, (short *)(&tags[i].descending) ) ;
         }
      }
   }

   if ( rc != 0 || error4code( cb ) < 0 || client->status != 0 )
   {
      u4free( dataName ) ;
      java4freeFieldsTags( fields, tags ) ;
      return -1 ;
   }

   /* AS 07/21/99 - added parm for win 95/98 to avoid endless laze writes - always 0 for java */
   /* AS 08/04/99 - added parm for code page / collating sequences - set to none for java. */
   rc = D4create( cb, dataName, fields, (TAG4INFO *)tags, &data, client->accessMode, client->safety, 0, 0, 0, collate4none, collate4none ) ;

   if ( rc != 0 )
   {
      java4freeFieldsTags( fields, tags ) ;
      u4free( dataName ) ;
      return rc ;
   }

   if ( data == 0 )   /* need to open */
   {
      /* AS 07/21/99 - for win 95/98 to avoid endless laze writes, always setting 0 here.*/
      data = D4open( cb, dataName, client->accessMode, client->errDefaultUnique, cb->log, 1, 0, client->readOnly, JAVA4SINGLE_OPEN_SETTING, cb->compatibility) ;
   }

   u4free( dataName ) ;

   if ( data == 0 )
   {
      java4freeFieldsTags( fields, tags ) ;
      return error4code( cb ) ;
   }

   /* now set the tag unique settings */
   if ( tags != 0 )
      for ( i = 0 ; tags[i].name.ptr != 0 ; i++ )
      {
         tag = d4tag( data, tags[i].name.ptr ) ;
         tag->errUnique = tags[i].unique ;
      }

   java4freeFieldsTags( fields, tags ) ;

   if ( client->status != 0 )
   {
      d4close( data ) ;
      return client->status ;
   }
   if ( java4socketWriteShort( cb, connect, ( short )client->status ) != 0 )
   {
      d4close( data ) ;
      return -1 ;
   }
   data->fieldsDefined = (short *)(u4allocFree( cb, d4numFields(data) * sizeof( short ) )) ;
   if ( data->fieldsDefined == 0 )
   {
      d4close( data ) ;
      return e4memory ;
   }
   client->currentData = data ;
   java4socketWriteLong( cb, connect, data->serverId ) ;

   return r4done ;
}



static int java4messageCreateIndex( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   char *indexName ;
   DATA4 *data ;
   short i, numTags ;
   CONNECTION4TAG_INFO *tags ;   /* use because not const */
   INDEX4 *index ;
   int rc ;
   long len ;
   TAG4 *tag ;

   tags = 0 ;
   rc = 0 ;

   if ( java4socketReadString( cb, connect, &indexName, sizeof( short ), &len ) != 0 )
      return -1 ;
   if ( len == 0 )
   {
      u4free( indexName ) ;
      indexName = 0 ;
      #ifdef S4CLIPPER  /* LY 2002/04/22 : create <alias>.CGP file if Data4cs::createIndex(null, ...) */
         indexName = (char *) u4allocErr( cb, strlen( client->currentData->alias )+1 ) ;
         if ( indexName )
            strcpy( indexName, client->currentData->alias ) ;
      #endif
   }

   if ( java4socketReadShort( cb, connect, &numTags ) != 0 )
   {
      u4free( indexName ) ;
      return -1 ;
   }

   if ( numTags == 0 )
      return 0 ;

   tags = (CONNECTION4TAG_INFO *)u4allocFree( cb, (numTags + 1) * sizeof( TAG4INFO ) ) ;

   for ( i = 0 ; rc == 0 && i < numTags; i++ )
   {
      if ( rc == 0 )
         rc = java4socketReadString( cb, connect, &tags[i].name.ptr, sizeof( char ), 0 ) ;
      if ( rc == 0 )
         rc = java4socketReadString( cb, connect, &tags[i].expression.ptr, sizeof( short ), 0 ) ;
      if ( rc == 0 )
         rc = java4socketReadString( cb, connect, &tags[i].filter.ptr, sizeof( short ), 0 ) ;
      if ( rc == 0 )
         rc = java4socketReadShort( cb, connect, &tags[i].unique ) ;
      if ( rc == 0 )
         rc = java4socketReadShort( cb, connect, (short *)(&tags[i].descending) ) ;
   }

   data = client->currentData ;

   if ( rc != 0 || client->status != 0 || data == 0 || error4code( cb ) < 0 )
   {
      u4free( indexName ) ;
      java4freeFieldsTags( 0, tags ) ;
      return -1 ;
   }

   /* AS 07/21/99 - for win 95/98 to avoid endless lazy writes, always setting 0 here.*/
   /* AS 08/04/99 - added parm for code page / collating sequences - set to none for java. */
   rc = I4create( data, indexName, (TAG4INFO *)tags, OPEN4DENY_RW, client->safety, 0, client->readOnly, 0, 0, collate4none, collate4none ) ;

   if ( rc != 0 )
      return rc ;

   /* fix the accessMode setting */
   /* AS 07/21/99 - for win 95/98 to avoid endless lazy writes, always setting 0 here.*/
   /* AS 04/05/01 - added another parmater for 'create temp' -- am assuming this is ineligible for Java */
   index = I4open( data, indexName, client->accessMode, client->errDefaultUnique, client->readOnly, 0, 1, 0, client->safety, 0 ) ;

   u4free( indexName ) ;

   if ( index == 0 )
   {
      java4freeFieldsTags( 0, tags ) ;
      return -1 ;
   }

   /* now set the tag unique settings */
   if ( tags != 0 )
      for ( i = 0 ; tags[i].name.ptr != 0 ; i++ )
      {
         tag = d4tag( data, tags[i].name.ptr ) ;
         tag->errUnique = tags[i].unique ;
      }

   java4freeFieldsTags( 0, tags ) ;
   return 0 ;
}



static int java4messageDisconnect( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client, SERVER4 *server )
{
   int rc ;

   #ifndef S4OFF_TRAN
      #ifndef S4OFF_WRITE
         if ( client->trans.currentTranStatus == r4active )
            server4clientTransactionRollback( client ) ;
      #endif
   #endif
   #ifndef S4OFF_LOG
      if ( server->c4->logConn > 0 )
      {
         char buf[250] ;
         sprintf( buf, "IS0400:Server is disconnecting client due to java4messageDisconnect for userId: [%s] and ip adress: [%s]",
                  client->account.accountId, client->account.tcpAddress ) ;
         assert5( strlen( buf ) < sizeof( buf ) ) ;
         code4logInfo( server->c4, buf ) ;
      }
   #endif
   rc = server4disconnect( server, client ) ;
   if ( rc < 0 && rc > -2000 )
   {
      error4set( cb, 0 ) ;
      rc = 0 ;
   }
   return rc ;
}



static int java4messageGetFieldInfo( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   char *fieldName, fieldType, numDec ;
   DATA4 *data ;
   short fieldLen ;
   FIELD4 *field ;
   long len ;
   int rc = 0 ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   if ( java4socketReadString( cb, connect, &fieldName, sizeof( char ), &len ) != 0 )
      return -1 ;

   if ( len == 0 )
      return error4( cb, e4fieldName, E80910 ) ;

   field = d4field( data, fieldName ) ;
   u4free( fieldName ) ;

   if ( field == 0 )
      rc = error4( cb, e4fieldName, E80910 ) ;
   else
   {
      #ifdef S4FOX
         /* currently no support for binary fields, so ensure it isn't.
            Note that when added, the assign area of code must be changed
            because it currently just inserts spaces in left over field parts */
         if ( field )
         {
            /* AS 03/05/99 - binary value of 2 is ok, it means foxpro 3.0 memo/general field types (i.e. memo id is binary but field is not)*/
            /* if ( field->binary != 0 ) */
            if ( field->binary == 1 )
               rc = error4( cb, e4fieldType, E80912 ) ;
         }
      #endif
   }

   client->status = rc ;
   // AS Aug 30/05 - in the case of non-zero rc, the caller of this function sends the status back, so we shouldn't do this.
   if ( rc )
      return rc ;

   java4messageStatus( cb, connect, client ) ;

   // inform the client of the number of bytes of field information being sent
   if ( java4socketWriteShort( cb, connect, 6 ) != 0 )
      return -1 ;

   if ( f4type( field ) == r4memo
      #ifdef S4FOX
        || f4type( field ) == r4gen
        || f4type( field ) == r4memoBin
      #else
        || f4type( field ) == r4bin
      #endif
      )
   {
      fieldLen = java4lenVariable ;
      numDec = 0 ;
   }
   else
   {
      fieldLen = (short)f4len( field ) ;
      numDec = f4decimals( field ) ;
   }

   fieldType = f4type(field);

   /* now return the data */
   /* **IMPORTANT** If anything is added to the following field
      information being sent to the client, the number of bytes sent
      to the client (a few lines up) must be appropriately modified. */
   rc = java4socketWriteShort( cb, connect, fieldLen ) ;
   if ( rc != 0 )
      return rc ;
   rc = java4socketWriteChar( cb, connect, numDec ) ;
   if ( rc != 0 )
      return rc ;
   rc = java4socketWriteChar( cb, connect, fieldType ) ;
   if ( rc != 0 )
      return rc ;
   rc = java4socketWriteShort( cb, connect, f4number(field) ) ;
   if ( rc != 0 )
      return rc ;

   rc = java4socketSend( cb, connect ) ;
   if (rc)
      return rc;
   return r4done;
}



static int java4messageGo( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   long recNo ;
   int rc ;

   rc = java4socketReadLong( cb, connect, &recNo ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;

   return D4go( client->currentData, recNo, 1, 0 ) ;
}



static int java4messageLockGroup( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   short numLocks ;
   DATA4 *data ;
   long serverId, recNum, lockType ;
   int rc ;

   rc = java4socketReadShort( cb, connect, &numLocks ) ;
   /* ensure numLocks read was valid */
   if ( rc != 0 )
      return rc ;

   for ( ; numLocks > 0 ; numLocks-- )
   {
      rc = java4socketReadLong( cb, connect, &serverId ) ;
      if ( rc != 0 )
         return rc ;

      data = tran4data( &client->trans, serverId, -1 ) ;
      if ( data == 0 )
         return e4data ;

      rc = java4socketReadLong( cb, connect, &lockType ) ;
      if ( rc != 0 )
         return rc ;
      switch ( lockType )
      {
         case java4lockAll:
            d4lockAddAll( data ) ;
            break ;
         case java4lockAppend:
            d4lockAddAppend( data ) ;
            break ;
         case java4lockFile:
            d4lockAddFile( data ) ;
            break ;
         default:  /* case LOCK4RECORD: */
            recNum = lockType ;
            if ( recNum < 1 )
               return error4( cb, e4lock, E81505 ) ;
            d4lockAdd( data, recNum ) ;
            break ;
      }
   }

   if ( client->status != 0 )
   {
      code4lockClear( cb ) ;
      return client->status ;
   }

   rc = tran4lock( &client->trans ) ;
   if ( rc != 0 )
      code4lockClear( cb ) ;
   return rc ;
}



static int java4messageOpen( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   /* if succeeds, returns r4done to indicate that rc and data sent back to client */
   char *dataName ;
   DATA4 *data ;
   int rc ;

   rc = java4socketReadString( cb, connect, &dataName, sizeof( short ), 0 ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;

   /* AS 07/21/99 - for win 95/98 to avoid endless laze writes, always setting 0 here.*/
   data = D4open( cb, dataName, client->accessMode, client->errDefaultUnique, cb->log, 0, 0, client->readOnly, JAVA4SINGLE_OPEN_SETTING, cb->compatibility ) ;
   u4free( dataName ) ;

   if ( data == 0 )
      return error4code( cb ) ;

   client->status = 0 ;
   data->fieldsDefined = (short *)(u4allocFree( cb, d4numFields(data) * sizeof( short ) )) ;
   if ( data->fieldsDefined == 0 )
   {
      d4close( data ) ;
      return e4memory ;
   }

   if ( client->status != 0 )
   {
      d4close( data ) ;
      return client->status ;
   }
   if ( java4socketWriteShort( cb, connect, ( short )client->status ) != 0 )
   {
      d4close( data ) ;
      return -1 ;
   }

   client->currentData = data ;
   java4socketWriteLong( cb, connect, data->serverId ) ;
   return r4done ;
}



static int java4messageOpenIndex( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   char *indexName ;
   DATA4 *data ;
   int rc ;
   INDEX4 *index ;
   char oldAccessMode ;
   short oldErrDefaultUnique ;

   rc = java4socketReadString( cb, connect, &indexName, sizeof( short ), 0 ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   oldAccessMode = cb->accessMode ;
   oldErrDefaultUnique = cb->errDefaultUnique ;
   cb->accessMode = client->accessMode ;
   cb->errDefaultUnique = client->errDefaultUnique ;
   // AS Oct 12/04 - use I4open for security and handling, and r4noOpen is ok, means it is another client reoping the index...
   index = I4open( data, indexName, client->accessMode, client->errDefaultUnique, client->readOnly, 0, 1, 0, client->safety, 0 ) ;
   if ( error4code( cb ) == r4noOpen )
      error4set( cb, 0 ) ;
   cb->accessMode = oldAccessMode ;
   cb->errDefaultUnique = oldErrDefaultUnique ;
   u4free( indexName ) ;

   if ( index == 0 )
      return error4code( cb ) ;

   return 0 ;
}



static int java4messagePack( SERVER4CLIENT *client )
{
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   return D4pack( client->currentData ) ;
}



static int java4messagePosition( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   /* if succeeds, returns r4done to indicate that rc and data sent back to client */
   double pos ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   pos = D4position( client->currentData ) ;
   if ( pos < 0 )
      return -1 ;

   if ( client->status != 0 )
      return client->status ;
   if ( java4socketWriteShort( cb, connect, ( short )client->status ) != 0 )
      return -1 ;
   java4socketWriteDouble( cb, connect, pos ) ;

   return r4done ;
}



static int java4messagePositionSet( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   int rc ;
   double pos ;

   rc = java4socketReadDouble( cb, connect, &pos, 0 ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;

   return D4positionSet( client->currentData, pos ) ;
}



static int java4messageRecCount( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   /* if succeeds, returns r4done to indicate that rc and data sent back to client */
   int rc ;
   long count ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;

   count = D4recCount( client->currentData ) ;
   if ( count < 0 )
      return (int)count ;

   if ( client->status != 0 )
      return client->status ;
   if ( java4socketWriteShort( cb, connect, ( short )client->status ) != 0 )
      return -1 ;
   rc = java4socketWriteLong( cb, connect, count ) ;

   return r4done ;
}



static int java4messageRecNo( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   long recNo ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   recNo = d4recNo( client->currentData ) ;
   if ( recNo < -1 )   /* -1 is a valid return code */
      return -1 ;

   if ( client->status != 0 )
      return client->status ;
   if ( java4socketWriteShort( cb, connect, ( short )client->status ) != 0 )
      return -1 ;
   java4socketWriteLong( cb, connect, recNo ) ;

   return r4done ;
}



static int java4messageRegisterField( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   char *fieldName, *allowableTypes, fieldType, numDec ;
   DATA4 *data ;
   short fieldNum, i, fieldLen ;
   FIELD4 *field ;
   long len ;
   int rc ;
   char neg ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   if ( java4socketReadString( cb, connect, &fieldName, sizeof( char ), &len ) != 0 )
      return -1 ;

   if ( len != 0 )  /* length of zero means deleted field */
      fieldNum = d4fieldNumber( data, fieldName ) ;

   if ( fieldName != 0 )
      u4free( fieldName ) ;

   if ( len == 0 )   /* deleted flag */
      fieldNum = -1 ;
   else
   {
      /* read the type info to clear buffer for sure */
      rc = java4socketReadString( cb, connect, &allowableTypes, sizeof( char ), 0 ) ;
      /* AS 03/29/99 return fieldNum error on higher priority thatn read string failure */
      if ( fieldNum < 0 )
         return error4( cb, e4fieldName, E80910 ) ;
      if ( rc != 0 )
         return -1 ;
      /* ensure the field type is valid */
      field = d4fieldJ( data, fieldNum ) ;
      fieldType = f4type( field ) ;
      if ( strrchr( allowableTypes, fieldType ) == 0 )
         return error4( cb, e4fieldType, E80501 ) ;
   }

   if ( client->status != 0 )
      return client->status ;

   /* verify that the field hasn't already been defined */
   for ( i = 0 ; i < data->numFieldsDefined ; i++ )
      if ( data->fieldsDefined[i] == fieldNum )  /* deleted field */
         return error4( cb, e4fieldType, E80911 ) ;

   #ifdef S4FOX
      /* currently no support for binary fields, so ensure it isn't.
         Note that when added, the assign area of code must be changed
         because it currently just inserts spaces in left over field parts */
     if ( fieldNum != -1 )
     {
        /* AS 03/05/99 - binary value of 2 is ok, it means foxpro 3.0 memo/general field types (i.e. memo id is binary but field is not)*/
        /* if ( field->binary != 0 ) */
        if ( field->binary == 1 )
           return error4( cb, e4fieldType, E80912 ) ;
     }
   #endif

   /* all ok, so write valid status */
   if ( java4socketWriteShort( cb, connect, ( short )client->status ) != 0 )
      return -1 ;

   if ( fieldNum != -1 )  /* not deleted flag, so return properties */
   {
      if ( f4type( field ) == r4memo
         #ifdef S4FOX
           || f4type( field ) == r4gen
           || f4type( field ) == r4memoBin
         #else
           || f4type( field ) == r4bin
         #endif
         )
      {
         fieldLen = java4lenVariable ;
         numDec = 0 ;
      }
      else
      {
         fieldLen = (short)f4len( field ) ;
         numDec = f4decimals( field ) ;
      }

      /* now return the data */
      rc = java4socketWriteShort( cb, connect, fieldLen ) ;
      if ( rc != 0 )
         return rc ;
      rc = java4socketWriteChar( cb, connect, numDec ) ;
      if ( rc != 0 )
         return rc ;
      rc = java4socketWriteChar( cb, connect, fieldType ) ;
      if ( rc != 0 )
         return rc ;
   }

   data->fieldsDefined[data->numFieldsDefined] = fieldNum ;
   data->numFieldsDefined++ ;

   /* if on a valid record, pass the field contents back */
   if ( d4bof( data ) || d4eof( data ) || d4recNo( data ) < 1L || d4recNo( data ) > d4recCount( data ) )
   {
      /* just send a zero length down */
      neg = java4blankField ;
      rc = java4socketWriteChar( cb, connect, neg ) ;
   }
   else  /* send back the field contents */
   {
      if ( fieldNum == -1 )  /* deleted field */
      {
         neg = java4defaultFieldLen ;
         rc = java4socketWriteChar( cb, connect, neg ) ;
         if ( rc == 0 )
            rc = java4socketWriteChar( cb, connect, data->record[0] ) ;
      }
      else
         rc = java4messageStatusField( cb, connect, client, field, 1 ) ;

      if ( rc < 0 )
         return -1 ;
   }

   return r4done ;
}



static int java4messageReindex( SERVER4CLIENT *client )
{
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   return D4reindex( client->currentData ) ;
}



static int java4messageSeekDouble( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   double seekKey ;
   int rc ;

   rc = java4socketReadDouble( cb, connect, &seekKey, 1 ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;

   return D4seekDouble( client->currentData, client->currentData->tagSelected, seekKey, 0 ) ;
}



static int java4messageSeekN( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   char *seekKey ;
   int rc ;
   long keyLen ;

   rc = java4socketReadString( cb, connect, &seekKey, sizeof( short ), &keyLen ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;

   rc = D4seekN( client->currentData, client->currentData->tagSelected, seekKey, (short)keyLen, 0 ) ;
   u4free( seekKey ) ;
   return rc ;
}



static int java4messageSelectData( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   int rc ;
   long serverId ;
   DATA4 *data ;

   rc = java4socketReadLong( cb, connect, &serverId ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;

   data = tran4data( &client->trans, serverId, -1 ) ;
   if ( data == 0 )
      return e4data ;

   client->currentData = data ;
   return 0 ;
}



static int java4messageSelectTag( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   char *tagName ;
   int rc ;
   TAG4 *tag = 0;
   long len ;

   rc = java4socketReadString( cb, connect, &tagName, sizeof( short ), &len ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 || client->currentData == 0 )
   {
      u4free( tagName ) ;
      return -1 ;
   }

   if ( len != 0 )
   {
      tag = d4tag( client->currentData, tagName ) ;
      u4free( tagName ) ;
      if ( tag == 0 )
         return e4tagName ;  // CS 2000/07/19 change e4name to e4tagName
   }

   d4tagSelect( client->currentData, tag ) ;

   return 0 ;
}



static int java4messageSetAccessMode( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   return java4messageSetAnyCharValue( cb, connect, client, &client->accessMode ) ;
}



static int java4messageSetAnyCharValue( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client, char *setValue )
{
   char readChar ;

   if ( java4socketReadChar( cb, connect, &readChar ) != 0 )
      return -1 ;

   if ( client->status != 0 )
      return client->status ;

   *setValue = readChar ;

   return 0 ;
}

static int java4messageSetDefaultUnique( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   return java4messageSetAnyCharValue( cb, connect, client, &client->errDefaultUnique ) ;
}



static int java4messageSetReadLock( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   return java4messageSetAnyCharValue( cb, connect, client, &client->readLock ) ;
}



static int java4messageSetReadOnly( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   return java4messageSetAnyCharValue( cb, connect, client, &client->readOnly ) ;
}



static int java4messageSetSafety( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   return java4messageSetAnyCharValue( cb, connect, client, &client->safety ) ;
}



static int java4messageSetUnlockAuto( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   return java4messageSetAnyCharValue( cb, connect, client, &client->unlockAuto ) ;
}



static int java4messageSkip( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   long numSkip ;
   short skipRc ;
   int rc ;

   rc = java4socketReadLong( cb, connect, &numSkip ) ;
   if ( rc != 0 )
      return rc ;

   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;

   rc = D4skip( client->currentData, client->currentData->tagSelected, numSkip, &skipRc, client->currentData->recNum ) ;
   if ( skipRc < 0 )
      return skipRc ;
   else
      return rc ;
}



static int java4messageStatusLow( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client, int doFields )
{
   int rc, i ;
   DATA4 *data ;
   FIELD4 *field ;
   char neg ;
   const char *eString ;
   short len ;

   rc = java4socketWriteShort( cb, connect, (short)client->status ) ;
   if ( rc != 0 )
      return rc ;
   if ( client->status < 0 )  /* send an error string down as well */
   {
      eString = error4text( cb, client->errorCode2 ) ;
      if ( eString == 0 )
         java4socketWriteShort( cb, connect, (short)0 ) ;
      else
      {
         len = strlen( eString ) ;
         java4socketWriteShort( cb, connect, len ) ;
         java4socketWrite( cb, connect, eString, len ) ;
      }
      client->errorCode2 = 0 ;
   }

   /* if doFields is true, then do them, otherwise just exit out */

   if ( doFields == 0 )
      client->status = 0 ;
   else
   {
      if ( client->status != 0 && client->status != r4after )  /* r4after means return field values */
         client->status = 0 ;
      else
      {
         data = client->currentData ;
         if ( data == 0 )
            return e4data ;

         client->status = 0 ;

         /* now send the fields accross */
         for ( i = 0 ; i < data->numFieldsDefined ; i++ )
         {
            if ( rc != 0 )
               return rc ;

            if ( data->fieldsDefined[i] == -1 )
            {
               neg = java4defaultFieldLen ;
               rc = java4socketWriteChar( cb, connect, neg ) ;
               if ( rc == 0 )
                  rc = java4socketWriteChar( cb, connect, data->record[0] ) ;
               continue ;
            }

            field = &data->fields[data->fieldsDefined[i]-1] ;

            java4messageStatusField( cb, connect, client, field, 0 ) ;
         }

         if ( rc != 0 )
            return rc ;
      }
   }
   return java4socketSend( cb, connect ) ;
}



static int java4messageRefreshFields( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   /* The client will send the numbers of the fields it needs.
      The server will send the contents of the requested fields back to the client. */
   int rc ;
   long numFields ;
   short jField ;
   DATA4 *data ;
   FIELD4 *field ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   client->status = 0 ;

   // the client tells the server how many fields it needs
   rc = java4socketReadLong( cb, connect, &numFields ) ;

   // now send the fields accross
   for ( ; (numFields > 0) && (rc == r4success); numFields-- )
   {
      // get the requested field number
      rc = java4socketReadShort( cb, connect, &jField ) ;
      if ( rc != r4success )
         return rc ;

      if ( jField == -1 )
      {
         // The field number is -1; the client wants the deleted flag.
         char neg = java4defaultFieldLen ;
         rc = java4socketWriteChar( cb, connect, neg ) ;
         if ( rc == 0 )
            rc = java4socketWriteChar( cb, connect, data->record[0] ) ;
      }
      else
      {
         field = d4fieldJ( data, jField ) ;
         if ( !field )
            rc = e4parm ;
         else
            rc = java4messageStatusField( cb, connect, client, field, 0 ) ;
      }
   }

   if ( rc != 0 )
      return rc ;

   return java4socketSend( cb, connect ) ;
}



static int java4messageStatusField( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client, FIELD4 *field, char sendFullLengthFlag )
{
   /* sendFullLengthFlag is required because when registering fields on the java
      side, the field length is not always known right away (due to the way
      the java side is set up).  This is a quick work-around for the problem.
      java side unknown because the super class, which registers the field, is
      called before the local field type length is set, which is used in the
      field contents transfer */
   char neg ;
   unsigned char fieldLenChar ;
   int rc, fType ;
   long fieldLen ;

   #ifdef S4FOX
      if ( f4null( field ) )
      {
         neg = java4nullField ;
         rc = java4socketWriteChar( cb, connect, neg ) ;
         return 0 ;
      }
   #endif

   fType = f4type( field ) ;

   if ( sendFullLengthFlag == 1 )  /* emulate by pretending a memo, which sends length down */
      fType = r4memo ;

   switch ( fType )
   {
      case r4memo:     /* variable length fields */
      #ifdef S4FOX
         case r4gen:
         case r4memoBin:
      #endif
         fieldLen = f4memoLen( field ) ;
         if ( fieldLen < 128 )
         {
            fieldLenChar = (char)fieldLen ;
            rc = java4socketWriteChar( cb, connect, fieldLenChar ) ;
         }
         else
         {
            if ( fieldLen < 32768 )
               neg = java4shortLen ;
            else
               neg = java4longLen ;

            rc = java4socketWriteChar( cb, connect, neg ) ;
            if ( rc != 0 )
               return rc ;
            switch ( neg )
            {
               case java4shortLen:
                  rc = java4socketWriteShort( cb, connect, (short)fieldLen ) ;
                  break ;
               case java4longLen:
                  rc = java4socketWriteLong( cb, connect, fieldLen ) ;
                  break ;
            }
         }
         if (rc == 0)
            rc = java4socketWriteExtended( cb, connect, f4memoPtr(field), fieldLen ) ;
         return rc ;
      case r4date:   /* special handling for blank date fields */
         if ( memcmp( f4ptr( field ), "        ", 8 ) == 0 )
         {
            neg = java4blankField ;
            rc = java4socketWriteChar( cb, connect, neg ) ;
            return rc ;
         }
         /* otherwise fall through to default */
      default: /* full field width */
         neg = java4defaultFieldLen ;
         rc = java4socketWriteChar( cb, connect, neg ) ;
         fieldLen = f4len( field ) ;
         break ;
   }

   if ( rc == 0 )
      rc = java4socketWriteExtended( cb, connect, field->data->record + field->offset, fieldLen ) ;

   return rc ;
}



static int java4messageTop( SERVER4CLIENT *client )
{
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   return D4top( client->currentData ) ;
}



static int java4messageTranCommit(SERVER4CLIENT *client)
{
   assert5port( "transaction support for java" ) ;
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   return Server4clientTranCommit(client);
}



static int java4messageTranRollback(SERVER4CLIENT *client)
{
   assert5port( "transaction support for java" ) ;
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   return Server4clientTranRollback(client);
}



static int java4messageTranStart(SERVER4CLIENT *client)
{
   assert5port( "transaction support for java" ) ;
   if ( client->status != 0 )
      return client->status ;
   if ( client->currentData == 0 )
      return e4data ;
   return Server4clientTranStart(client);
}



static int java4messageUnlock( CODE4 *cb, SERVER4CLIENT *client )
{
   if ( client->status != 0 )
      return client->status ;

   return Client4unlock( client ) ;
}



static int java4messageWrite( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   rc = java4getFields( cb, data, connect ) ;
   if ( rc != 0 )
      return rc ;

   return D4write( data, d4recNo( data ), 0, 1 ) ;
}



static int java4messageWrite2( CODE4 *cb, CONNECT4 *connect, SERVER4CLIENT *client )
{
   DATA4 *data ;
   int rc ;

   data = client->currentData ;
   if ( data == 0 )
      return e4data ;

   rc = java4getFields2( cb, data, connect ) ;
   if ( rc != 0 )
      return rc ;

   return D4write( data, d4recNo( data ), 0, 1 ) ;
}



// AS July 26/02 - Need to have output messageType == final message for receiveMessage to work correctly
int S4FUNCTION java4processMessage( CODE4 *cb, SERVER4CLIENT *client, short *messageType )
{
   SERVER4 *server ;
   short doDisconnect=0 ;
   int rc=0, saveError, serverRC=0, outRc ;
   char firstTimeThrough ;
   CONNECT4 *connect ;
   #ifdef S4COM_PRINT
      #ifdef S4SERVER
         char buffer[128] ;
      #endif
   #endif

   server = cb->server ;
   #ifdef E4PARM_LOW
      if ( client == 0 )
         return error4( 0, e4parm_null, E70179 ) ;
   #endif
   #ifdef E4ANALYZE
      rc = server4verify( server, 1 ) ;
      if ( rc < 0 )
         return rc ;
      rc = server4clientVerify( client, 1 ) ;
      if ( rc < 0 )
         return rc ;
   #endif
   // AS 08/29/00 - done at a higher level now
   // code4enterExclusive(cb, client ) ;

   connect = &client->connect ;
   server->info.numRequests++ ;
   client->info.numRequests++ ;
   code4unlockAutoSet( cb, client->unlockAuto ) ;

   /* loop continues until outRc is non-zero.  r4done indicates just exit
      the function (after calling socket4send()).  Any other value means
      write the outRC to the socket and then call socket4send()) */
   /* also, if a server error occurs, the loop returns out */
   for ( firstTimeThrough  = 1, outRc = 0 ; outRc == 0 ; )
   {
      if ( firstTimeThrough )
         firstTimeThrough = 0 ;
      else
      {
         // AS July 17/02 - Normally we want to continue with message until done.  However, if
         // the message was a connect message, we have to give up exclusivity control to allow
         // the accept thread to continue on setting up connections in case there are 2 simultaneous
         // connectinos, in which case we get stuck here waiting for the accept thread which is stuck
         // waiting for us.
         Bool5 wasConnect = 0 ;
         if ( (*messageType) == JAVA4CONNECT )
         {
            wasConnect = 1 ;
            code4exitExclusiveVerifyLast( cb, client ) ;
         }
         rc = java4socketReceive( cb, connect, messageType, sizeof( short ), firstTimeThrough ) ;
         if ( wasConnect == 1 )
         {
            code4enterExclusiveVerifyFirst( cb, client, 1 ) ;
         }
         if ( rc != 0 )
         {
            if ( rc == -1 )  /* no messages */
               break ;
            else
            {
               /* if a socket error, just clear the error and continue
                  (until the client actually quits and sends a disconnect
                   implicitly) */
               if ( error4code( cb ) == -1320 )
                  error4set( cb, 0 ) ;
               // AS 08/29/00 - done at a higher level now
               // code4exitExclusive(cb, client ) ;
               return rc ;
            }
         }
         (*messageType) = ntohs5( *messageType ) ;
         #ifdef S4COM_PRINT
            #ifdef S4SERVER
               sprintf(buffer, "Receiving Message: %s, on socket %d", s4connectionPrint( (*messageType) ), connect->connectBuffer.connectLowPtr->sockr ) ;
               debug4display(buffer) ;
            #endif
         #endif
      }
      #ifdef EXCLUDE
         if ( !connectMessage )
         {
            rc = java4socketReceive( cb, connect, messageType, sizeof( short ), firstTimeThrough ) ;
            firstTimeThrough = 0 ;
            if ( rc != 0 )
            {
               if ( rc == -1 )  /* no messages */
                  break ;
               else
               {
                  /* if a socket error, just clear the error and continue
                     (until the client actually quits and sends a disconnect
                      implicitly) */
                  if ( error4code( cb ) == -1320 )
                     error4set( cb, 0 ) ;
                  // AS 08/29/00 - done at a higher level now
                  // code4exitExclusive(cb, client ) ;
                  return rc ;
               }
            }
            (*messageType) = ntohs5( messageType ) ;
         }
         else
         {
            (*messageType) = JAVA4CONNECT ;
            connectMessage = 0 ;
            firstTimeThrough = 0 ;
         }
      #endif
      switch ( (*messageType) )
      {
         case JAVA4APPEND:
            rc = java4messageAppend( cb, connect, client ) ;
            break ;
         case JAVA4APPEND2:
            rc = java4messageAppend2( cb, connect, client ) ;
            break ;
         case JAVA4ACCESS_MODE_SET:
            rc = java4messageSetAccessMode( cb, connect, client ) ;
            break ;
         case JAVA4BLANK:
            rc = java4messageBlank( cb, connect, client ) ;
            break ;
         case JAVA4BOTTOM:
            rc = java4messageBottom( client ) ;
            break ;
         case JAVA4CLOSE:
            rc = java4messageClose( client ) ;
            break ;
         case JAVA4CONNECT:
            #ifdef S4PROFILE
               /* if profiling, ensure that no extra dll's exist.
                  all code must be in a single executeable for multi-threads
                  to be profileable by the MicroSoft */
               if ( code4numCodeBase() == 0 )  /* error */
               {
                  doDisconnect = 1 ;
                  error4( cb, e4info, E81505 ) ;
                  break ;
               }
            #endif
            rc = java4messageConnect( cb, connect, client ) ;
            if ( rc == r4done )
                outRc = r4done ;
            break ;
         case JAVA4CREATE:
            /* if outRc != 0 then rc out gets done after end of switch.
               otherwise the function returns r4done */
            outRc = java4messageCreate( cb, connect, client ) ;
            break ;
         case JAVA4DEFAULT_UNIQUE_SET:
            rc = java4messageSetDefaultUnique( cb, connect, client ) ;
            break ;
         case JAVA4DISCONNECT:   /* no return message since client has disconnected */
            rc = java4messageDisconnect( cb, connect, client, server ) ;
            outRc = r4done ;
            break ;
         case JAVA4GET_FIELD_INFO:
            outRc = java4messageGetFieldInfo( cb, connect, client ) ;
            break ;
         case JAVA4GO:
            rc = java4messageGo( cb, connect, client ) ;
            break ;
         case JAVA4INDEX_CREATE:
            rc = java4messageCreateIndex( cb, connect, client ) ;
            break ;
         case JAVA4INDEX_OPEN:
            rc = java4messageOpenIndex( cb, connect, client ) ;
            break ;
         case JAVA4LOCK_GROUP:
            rc = java4messageLockGroup( cb, connect, client ) ;
            break ;
         case JAVA4OPEN:
            /* if outRc != 0 then rc out gets done after end of switch.
               otherwise the function returns r4done */
            outRc = java4messageOpen( cb, connect, client ) ;
            break ;
         case JAVA4PACK:
            rc = java4messagePack( client ) ;
            break ;
         case JAVA4POSITION:
            /* if outRc != 0 then rc out gets done after end of switch.
               otherwise the function returns r4done */
            outRc = java4messagePosition( cb, connect, client ) ;
            break ;
         case JAVA4POSITION_SET:
            rc = java4messagePositionSet( cb, connect, client ) ;
            break ;
         case JAVA4READ_LOCK_SET:
            rc = java4messageSetReadLock( cb, connect, client ) ;
            break ;
         case JAVA4READ_ONLY_SET:
            rc = java4messageSetReadOnly( cb, connect, client ) ;
            break ;
         case JAVA4RECCOUNT:
            /* if outRc != 0 then rc out gets done after end of switch.
               otherwise the function returns r4done */
            outRc = java4messageRecCount( cb, connect, client ) ;
            break ;
         case JAVA4RECNO:
            /* if outRc != 0 then rc out gets done after end of switch.
               otherwise the function returns r4done */
            outRc = java4messageRecNo( cb, connect, client ) ;
            break ;
         case JAVA4REFRESH:
            outRc = java4messageRefreshFields( cb, connect, client ) ;
            break ;
         case JAVA4REGISTER_FIELD:
            outRc = java4messageRegisterField( cb, connect, client ) ;
            break ;
         case JAVA4REINDEX:
            rc = java4messageReindex( client ) ;
            break ;
         case JAVA4SAFETY_SET:
            rc = java4messageSetSafety( cb, connect, client ) ;
            break ;
         case JAVA4SEEK_DBL:
            rc = java4messageSeekDouble( cb, connect, client ) ;
            break ;
         case JAVA4SEEK_N:
            rc = java4messageSeekN( cb, connect, client ) ;
            break ;
         case JAVA4SELECT_DATA:
            rc = java4messageSelectData( cb, connect, client ) ;
            break ;
         case JAVA4SELECT_TAG:
            rc = java4messageSelectTag( cb, connect, client ) ;
            break ;
         case JAVA4SKIP:
            rc = java4messageSkip( cb, connect, client ) ;
            break ;
         case JAVA4STATUS_CODE:
            rc = java4messageStatus( cb, connect, client ) ;
            outRc = r4done ;
            break ;
         case JAVA4STATUS_FIELDS:
            rc = java4messageStatusFields( cb, connect, client ) ;
            outRc = r4done ;
            break ;
         case JAVA4TOP:
            rc = java4messageTop( client ) ;
            break ;
         case JAVA4TRANCOMMIT:   // CS 2002/04/30
            rc = java4messageTranCommit( client ) ;
            break ;
         case JAVA4TRANROLLBACK:   // CS 2002/04/30
            rc = java4messageTranRollback( client ) ;
            break ;
         case JAVA4TRANSTART:   // CS 2002/04/30
            rc = java4messageTranStart( client ) ;
            break ;
         case JAVA4UNLOCK:
            rc = java4messageUnlock( cb, client ) ;
            break ;
         case JAVA4UNLOCK_AUTO_SET:
            rc = java4messageSetUnlockAuto( cb, connect, client ) ;
            break ;
         case JAVA4WRITE:
            rc = java4messageWrite( cb, connect, client ) ;
            break ;
         case JAVA4WRITE2:
            rc = java4messageWrite2( cb, connect, client ) ;
            break ;
         default:
            rc = e4message ;
            doDisconnect = 1 ;  // this is serious enough to cancel the client
            break ;
      }

      if ( error4code( cb ) != 0 && rc == 0 )
         rc = -1 ;
      if ( rc < 0 )
      {
         /* clear error return, and send result back to client, then continue */
         saveError = error4set( cb, 0 ) ;
         if ( saveError == 0 )   /* case where a return code error */
            saveError = rc ;
         if ( saveError <= -2100 || saveError == e4memory )   /* tell client it is a server error */
         {
            client->status = e4server ;
            client->errorCode2 = error4code2( cb ) ;
            error4set( cb, ( short )saveError ) ;
            // AS 08/29/00 - done at a higher level now
            // code4exitExclusive(cb, client ) ;
            return saveError ;
         }
         else
         {
            client->status = saveError ;
            client->errorCode2 = error4code2( cb ) ;
            serverRC = 0 ;
         }
      }
      else
         if ( rc > 0 )   /* r4 code also stops processing until status requested */
            client->status = rc ;
      if ( doDisconnect == 1 )
      {
         #ifndef S4OFF_LOG
            if ( server->c4->logConn > 0 )
            {
               char buf[250] ;
               sprintf( buf, "IS0410:Server is disconnecting client due to java for userId: [%s] and ip adress: [%s]",
                        client->account.accountId, client->account.tcpAddress ) ;
               assert5( strlen( buf ) < sizeof( buf ) ) ;
               code4logInfo( server->c4, buf ) ;
            }
         #endif
         server4disconnect( server, client ) ;
         break ;
      }
   }

   if ( outRc != 0 && outRc != r4done )  /* means send an rc message out */
   {
      client->status = outRc ;
      java4messageStatus( cb, connect, client ) ;
   }

   if ( (*messageType) != JAVA4DISCONNECT && doDisconnect != 1 )    // disconnect removes connection, so cannot send
      java4socketSend( cb, connect ) ;  /* disregard a failure since it should eventually appear as a disconnected client */

   if ( rc < 0 )
      error4set2( cb, 0 ) ;

   if ( error4code( cb ) == -1320 )
      error4set( cb, 0 ) ;
   // AS 08/29/00 - done at a higher level now
   // code4exitExclusive(cb, client ) ;
   return 0 ;
}



static int java4socketReadDouble( CODE4 *cb, CONNECT4 *connect, double *outDouble, char trueDouble )
{
   /* if a true double is required (==1), must convert to character and then
      back to a double */
   double dbl1, dbl2 ;
   char *dblSrc, *dblDest, dblChar[25] ;
   int rc, i ;

   /* incoming double must be byte swapped */
   /* also, java doubles are more precise than C doubles, and also may have
      different final bit values.  Therefore, in order for true compatibility
      (which index files absolutely require), a conversion must be done
      to character and then back to double */

   rc = java4socketRead( cb, connect, &dbl1, sizeof( double ) ) ;
   if ( rc != 0 )
      return rc ;

   dblSrc = (char *)&dbl1 ;
   dblDest = (char *)&dbl2 ;

   for ( i = 0 ; i < 8 ; i++ )
      dblDest[i] = dblSrc[7-i] ;

   if ( trueDouble == 1 )
   {
      c4dtoa45( dbl2, dblChar, sizeof( dblChar ), 15 ) ;
      *outDouble = c4atod( dblChar, strlen( dblChar ) ) ;
   }
   else
      *outDouble = dbl2 ;

   return 0 ;
}



static int java4socketReadExtended( CODE4 *cb, CONNECT4 *connect, void *buffer, unsigned long bufSize )
{
   unsigned short readMax ;
   unsigned long lenLeft, offset ;
   int rc ;

   offset = 0 ;
   lenLeft = bufSize ;
   readMax = USHRT_MAX ;
   while( lenLeft > 0 )
   {
      if ( readMax > lenLeft )
         readMax = (unsigned short)lenLeft ;
      rc = java4socketReadLow( cb, connect, (char *)buffer + offset, readMax, 0, 0 ) ;
      if ( rc != 0 )
         return rc ;
      lenLeft -= readMax ;
      offset += readMax ;
   }

   return 0 ;
}



static int java4socketReadLong( CODE4 *cb, CONNECT4 *connect, long *outLong )
{
   int rc ;

   rc = java4socketRead( cb, connect, outLong, sizeof( long ) ) ;
   if ( rc != 0 )
      return rc ;
   *outLong = ntohl5( *outLong ) ;
   return 0 ;
}



static int java4socketReadLow( CODE4 *cb, CONNECT4 *connect, void *buffer, unsigned short bufSize, short receiveMessage, char firstTimeThrough )
{
   if ( error4code( cb ) < 0 )
      return error4code( cb ) ;

   return connect4receive( connect, buffer, bufSize, code4timeoutVal( cb ) ) ;
}



static int java4socketReadShort( CODE4 *cb, CONNECT4 *connect, short *outShort )
{
   int rc ;

   rc = java4socketRead( cb, connect, outShort, sizeof( short ) ) ;
   if ( rc != 0 )
      return rc ;
   *outShort = ntohs5( *outShort ) ;
   return 0 ;
}



static int java4socketReadString( CODE4 *cb, CONNECT4 *connect, char **outBuffer, short stringLengthSize, long *lenOut )
{
   int rc ;
   long lenLong, len ;
   short lenShort ;
   char lenChar ;

   if ( lenOut != 0 )
      *lenOut = 0 ;

   switch( stringLengthSize )
   {
      case sizeof( lenLong ):
         rc = java4socketReadLong( cb, connect, &lenLong ) ;
         len = lenLong;
         break ;
      case sizeof( lenShort ):
         rc = java4socketReadShort( cb, connect, &lenShort ) ;
         len = lenShort;
         break ;
      case sizeof( lenChar ):
         rc = java4socketRead( cb, connect, &lenChar, sizeof( lenChar ) ) ;
         len = lenChar ;
         break ;
      default:
         return e4parm ;
   }

   /* ensure valid length was read */
   if ( rc != 0 )
      return rc ;

   *outBuffer = ( char * )u4allocErr( cb, len+1 ) ;
   if ( *outBuffer == 0 )
      return e4memory ;
   rc = java4socketReadExtended( cb, connect, *outBuffer, len ) ;
   if ( rc != 0 )
   {
      u4free( *outBuffer ) ;
      return rc ;
   }
   ( *outBuffer )[len] = 0 ;
   if ( lenOut != 0 )
      *lenOut = len ;
   return 0 ;
}



static int java4socketSend( CODE4 *cb, CONNECT4 *connect )
{
   SERVER4 *server ;
   int rc ;

   server = cb->currentClient->server ;
   rc = connect4bufferSend( &connect->connectBuffer, server->javaSendBuffer, server->javaSendLen ) ;
   server->javaSendLen = 0 ;
   if (rc < 0 )
      return rc ;
   return connect4sendFlush( connect ) ;
}



static int java4socketWriteDouble( CODE4 *cb, CONNECT4 *connect, double writeDouble )
{
   double dbl1 ;
   char *dblSrc, *dblDest ;
   int i ;

   dblSrc = (char *)&writeDouble ;
   dblDest = (char *)&dbl1 ;

   for ( i = 0 ; i < 8 ; i++ )
      dblDest[i] = dblSrc[7-i] ;

   return java4socketWrite( cb, connect, &dbl1, sizeof( double ) ) ;
}



static int java4socketWriteExtended( CODE4 *cb, CONNECT4 *connect, const void *buffer, unsigned long bufSize )
{
   unsigned short sendMax ;
   unsigned long lenLeft, offset ;
   SERVER4 *server ;

   server = cb->currentClient->server ;
   sendMax = server->javaSendBufferLen ;

   if ( bufSize + (unsigned long)server->javaSendLen <= (unsigned long)sendMax )
   {
      memcpy( server->javaSendBuffer + server->javaSendLen, buffer, bufSize ) ;
      server->javaSendLen += (unsigned short)bufSize ;
      return 0 ;
   }

   /* otherwise must split and write */
   offset = sendMax - server->javaSendLen ;
   memcpy( server->javaSendBuffer + server->javaSendLen, buffer, offset ) ;
   server->javaSendLen = sendMax ;
   lenLeft = bufSize - offset ;

   while( lenLeft > 0 )
   {
      if ( java4socketSend( cb, connect ) != 0 )
         return -1 ;

      // AS Sep 9/05 - this code is incorrect.  if lenLeft is less than sendMax, we need to copy/send lenLeft to avoid a memory error
      // if ( bufSize <= sendMax )
      if ( bufSize <= sendMax || lenLeft < sendMax )
      {
         memcpy( server->javaSendBuffer, (char *)buffer + offset, lenLeft ) ;
         server->javaSendLen = (unsigned short)lenLeft ;
         return 0 ;
      }
      else
      {
         memcpy( server->javaSendBuffer, (char *)buffer + offset, sendMax ) ;
         server->javaSendLen = sendMax ;
         lenLeft -= sendMax ;
      }
   }

   return 0 ;
}



static int java4socketWriteLong( CODE4 *cb, CONNECT4 *connect, long writeLong )
{
   writeLong = htonl5( writeLong ) ;
   return java4socketWrite( cb, connect, &writeLong, sizeof( long ) ) ;
}



static int java4socketWriteShort( CODE4 *cb, CONNECT4 *connect, short writeShort )
{
   writeShort = htons5( writeShort ) ;
   return java4socketWrite( cb, connect, &writeShort, sizeof( short ) ) ;
}



#endif /* S4JAVA */
#endif /* S4OFF_COMMUNICATIONS */
