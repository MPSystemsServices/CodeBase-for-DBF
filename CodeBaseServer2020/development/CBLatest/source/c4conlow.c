/* c4conlow.c   (c)Copyright Sequiter Software Inc., 1988-2002.  All rights reserved. */

/*
  If E4PARM_LOW is defined, the functions in this module will ensure that
  their paramaters are not invalid.

The connect4low module is a system-specific implementation of the low-level
functionality required to be performed by a connection.

Nothing in this entire file will ever set CODE4.errorCode

None of these functions are relevant for the STAND-ALONE version of CodeBase.
*/

#include "d4all.h"

// include file for compression/decompression
#ifdef S4COMPRESS
   #include "zlib.h"
#endif
#if defined(S4OFF_THREAD) && !defined(S4WINCE)  /* LY 2002/10/04: added S4WINCE */
   #include <errno.h>
#endif

#if defined( S4OFF_COMMUNICATIONS ) || defined( S4STAND_ALONE ) || ( !defined( S4WINSOCK ) && !defined( S4BERKSOCK ) )
   #define DoNotCompileC4CONLOW
#endif


#ifndef DoNotCompileC4CONLOW
static int connect4lowInit(CONNECT4LOW *con, int readsock, CODE4 *c4 ) ;

#ifdef S4COMPRESS
   // AS Jul 16/03 - declaration was missing for client/server (resides in c4code.c)
   extern unsigned char g_compressed[] ;
   extern CRITICAL_SECTION critical4compression ;

   // compression is by default enabled.  A sample message is sent to the server to determine the connection
   // speed.  If the speed is below a compression time level then compression is performed.  Note that there
   // are cases where it is still desireable to perform client-side only compression.  In particular it can
   // speed up the server if the server is handling a lot of incoming communications to reduce overall network
   // traffic.

   void connect4lowCompressConfigure( CONNECT4LOW *connect, short compressLevel, short minLength )
   {
      // used to configure the compression.  set compressLevel to 0 to disable...
      if ( compressLevel != -1 && compressLevel < 10 )
         connect->compressLevel = compressLevel ;
      if ( minLength >= 0 )
         connect->minLength = minLength ;
   }

   // AS May 13/04 - moved to c4code.c to be generally available...

   static int connect4lowCompress( CONNECT4LOW *connect, unsigned char *buffer, int len, Bool5 *didCompress )
   {
      // compressLevel indicates compress time vs size options, make it run-time configureable.
      *didCompress = 0 ;

      if ( connect->compressLevel == 0 )  // means don't compress
         return len ;

      EnterCriticalSection( &critical4compression ) ;

      assert5( ( len * 1.001 + 12 ) < COMPRESS4MAX_LENGTH ) ;  // for compress, buffer must be 1% larger plus 12 bytes
      unsigned long outLen = COMPRESS4MAX_LENGTH ;  // AS Jul 16/03 - use define instead

      #ifdef S4COMPRESS_ZLIB
         int rc = compress2( g_compressed, &outLen, buffer, len, connect->compressLevel ) ;
         if ( rc != Z_OK )
            return rc ;
      #endif

      #ifdef S4COMPRESS_QUICKLZ
         if (connect->c4->qlz_state_compress == 0)
         {
            // Allocate memory for the qlz_state_compress struct.
            connect->c4->qlz_state_compress = u4allocErr(connect->c4, qlz_get_setting(1));
            if (connect->c4->qlz_state_compress == 0)
            {
               return error4code(connect->c4);
            }
         }

         outLen = qlz_compress(buffer, (char*)g_compressed, len, connect->c4->qlz_state_compress);
         if (outLen <= 0)
         {
            // Compression failed. No reason available.
            return -1;
         }
      #endif

      if ( (int)outLen > len )   // don't use if compression resulted in more data
         return len ;

      *didCompress = 1 ;
      memcpy( buffer, g_compressed, outLen ) ;

      LeaveCriticalSection( &critical4compression ) ;
      return (int)outLen ;
   }



   int connect4lowUncompress( CODE4 *c4, unsigned char *buffer, int len, int maxLen )
   {
      assert5( len < COMPRESS4MAX_LENGTH ) ;// AS Jul 16/03 - use define instead
      memcpy( g_compressed, buffer, len ) ;
      unsigned long rLen = maxLen ;

      #ifdef S4COMPRESS_ZLIB
         int rc = uncompress( buffer, &rLen, g_compressed, len ) ;
         if ( rc != Z_OK )
         {
            return -1 ;
         }
      #endif

      #ifdef S4COMPRESS_QUICKLZ
         if (c4->qlz_state_decompress == 0)
         {
            // Allocate memory for the qlz_state_decompress struct.
            c4->qlz_state_decompress = u4allocErr(c4, qlz_get_setting(2));
            if (c4->qlz_state_decompress == 0)
            {
               return error4code(c4);
            }
         }

         rLen = qlz_decompress((const char*)g_compressed, buffer, c4->qlz_state_decompress);
         if (rLen <= 0)
         {
            return -1;
         }
      #endif

      return (int)rLen ;
   }
#endif /* S4COMPRESS */



int address4getLocalNetID(C4NETID *netID, short *idLen, CONNECT4LOW *conLow )
{
   C4ADDRESS address ;
   int rc, temp ;

   temp= sizeof(struct sockaddr_in) ;
   rc = getsockname(conLow->sockw, (struct sockaddr *)&address, &temp ) ;
   if (rc || (temp != sizeof(C4ADDRESS)) )
   {
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }
   *netID = address.sin_addr ;
   *idLen = sizeof ( C4NETID ) ;
   return r4success ;
}



// AS Aug 02/01 - Added more error info for sockets...
static int getSockErr(int err)
{
   switch ( err )
   {
      case WSANOTINITIALISED:
         return e4socket ;
      case WSAENETDOWN:
         return e4socket ;
      case WSAEFAULT:
         return e4socket ;
      case WSAENOTCONN:
         return e4socket ;
      case WSAEINTR:
         return e4socket ;
      case WSAEINPROGRESS:
         return e4socket ;
      case WSAENETRESET:
         return e4socket ;
      case WSAENOTSOCK:
         return e4socket ;
      case WSAEOPNOTSUPP:
         return e4socket ;
      case WSAESHUTDOWN:
         return e4socket ;
      case WSAEWOULDBLOCK:
         return e4socket ;
      case WSAEMSGSIZE:
         return e4socket ;
      case WSAEINVAL:
         return e4socket ;
      case WSAECONNABORTED:
         return e4socket ;
      case WSAETIMEDOUT:
         return e4socket ;
      case WSAECONNRESET:
         return e4socket ;
      default:
         return e4socket ;
   }

   return e4socket;

}



int address4makeName(C4ADDRESS *address, short *addrLen, unsigned short portNo, const char *machineName)
{
   struct hostent *phe ;
   struct sockaddr_in *destSin ;
   long rc ;   /*May be forced to hold an address */

   #ifdef E4PARM_LOW
      if (machineName == 0 )
         return error4( 0, e4parmNull, E96901 ) ;
   #endif

   destSin = (struct sockaddr_in *)address ;
   destSin->sin_family = AF_INET ;
   destSin->sin_port = htons((short)portNo) ;
   *addrLen = sizeof(*destSin ) ;
   #ifndef S4NO_INET_ATON
      rc = inet_aton(machineName, &(destSin->sin_addr) ) ;
      if (rc)  /*success*/
         return r4success ;
   #else
      rc = inet_addr(machineName) ;
      #ifdef S4WINSOCK
      if (rc !=  INADDR_NONE )
      #else
      if (rc != -1 )
      #endif
      {
         destSin->sin_addr.s_addr = rc ;
         return r4success ;
      }
   #endif
   phe = gethostbyname(machineName) ;
   if (phe == 0 )
      #ifndef S4WINSOCK
         switch (h_errno)
         {
            case HOST_NOT_FOUND :
            case TRY_AGAIN :
               return r4hostNotFound;
            case NO_ADDRESS :
               return r4hostUnavailable;
            case NO_RECOVERY :
               return r4errNetwork;
         }
      #else
         switch (WSAGetLastError())
         {
            case WSAENETDOWN :
            case WSANO_RECOVERY :
               return r4errNetwork ;
            case WSAHOST_NOT_FOUND :
            case WSATRY_AGAIN :
               return r4hostNotFound ;
            case WSANO_DATA :
               return r4hostUnavailable ;
            default :
               #ifndef S4OFF_LOG
                  // AS Sept 26/02 - log this error as it should never actually occur...
                  char buf[10] ;
                  c4ltoa45( WSAGetLastError(), buf, sizeof( buf ) ) ;
                  error4describe( 0, e4connect, E96901, "unknown network error: Number: ", buf, 0 ) ;
               #endif
               return r4noConnect ;
         }
      #endif
   memcpy( (char *)&(destSin->sin_addr), phe->h_addr, phe->h_length ) ;
   return r4success;
}



int address4makeNetID(C4ADDRESS *address, short *addrLen, C4NETID netID, unsigned short portNo )
{
   #ifdef E4PARM_LOW
      if (address == 0 )
         return error4( 0, e4parmNull, E96902 ) ;
   #endif

   address->sin_family = AF_INET ;
   address->sin_port = portNo ;
   address->sin_addr = netID ;
   *addrLen = sizeof(*address ) ;
   return r4success;
}



// AS Jan 21/02 - Need to use a semaphore with the connect4low structure in windows because otherwise
// we are trampling ourselves
#ifdef E4DEBUG
   CONNECT4LOW *connect4bufferLowGet( CONNECT4BUFFER *con4buffer )
   {
      #ifdef S4WINSOCK
         if ( con4buffer->hasLowExclusive == 0 )
         {
            error4( 0, e4parm, E96901 ) ;
            return 0 ;
         }
      #endif
      return con4buffer->connectLowPtr ;
   }



   void connect4bufferLowSet( CONNECT4BUFFER *con4buffer, CONNECT4LOW *val )
   {
      #ifdef S4WINSOCK
         if ( con4buffer->hasLowExclusive == 0 )
         {
            error4( 0, e4parm, E96901 ) ;
            return ;
         }
      #endif
      con4buffer->connectLowPtr = val ;
   }
#endif



#ifdef S4WINSOCK
   // AS Jan 21/02 - Need to use a semaphore with the connect4low structure in windows because otherwise
   // we are trampling ourselves
   void connect4lowEnterExclusive( CONNECT4BUFFER *con4buffer )
   {
      WaitForSingleObject( con4buffer->lowAccessMutex, INFINITE ) ;
      #ifdef E4DEBUG
         if ( con4buffer->hasLowExclusive == 1 )   // someone else already has!
         {
            error4( 0, e4parm, E96901 ) ;
            return ;
         }
         con4buffer->hasLowExclusive = 1 ;
      #endif
   }



   void connect4lowExitExclusive( CONNECT4BUFFER *con4buffer )
   {
      #ifdef E4DEBUG
         if ( con4buffer->hasLowExclusive != 1 )   // mean's we didnt' reserve it
         {
            error4( 0, e4parm, E96901 ) ;
            return ;
         }
         con4buffer->hasLowExclusive = 0 ;
      #endif
         ReleaseMutex( con4buffer->lowAccessMutex ) ;
   }
#endif



// AS Jan 30/03 - Need the CODE4 for encryption dll handling
int connect4lowAccept( CONNECT4LOW *listen, CONNECT4LOW *acc, CODE4 *c4 )
{
/*
   DESCRIPTION

   This function accepts a connection on a CONNECT4LOW which is performing
     a listen.

   PARAMATERS

   listen is the CONNECT4LOW which is listening on a socket in order to
     accept a connection
   acc is an unused CONNECT4LOW structure which will be initialized and
     receives the accept

   RETURNS

   r4success
   < 0  error, does NOT set CODE4.error

   NOTES

   This function does the following:
     Accept the connection into the new CONNECT4LOW accept structure
     Initialize the accept structure
     return
*/

   #ifdef E4PARM_LOW
      if ( ( listen == 0 ) || ( acc == 0 ) )
         return error4( 0, e4parmNull, E96903 ) ;
   #endif

   acc->sockr = accept(listen->sockr,  0, 0 ) ;
   #ifdef S4WINSOCK
      assert5( acc->sockr != INVALID_SOCKET ) ;  // AS Jun 9/06 - try to break into problem code...
      if ( acc->sockr == INVALID_SOCKET )
         return e4socket ;
   #endif
   #ifdef S4BERKSOCK
      if ( acc->sockr < 0 )
         return e4socket ;
   #endif

   if ( connect4lowInit( acc, 1, c4 ) )
   {
      connect4lowError( acc ) ;
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }
   #ifdef S4SERVER
      acc->serverConnectStatus[1] = '1' ;  // we are accepted
   #endif
   return r4success ;
}



int connect4lowAcceptWait( CONNECT4LOW *listen, CONNECT4LOW *acc, int waitSecs )
{
/*
   DESCRIPTION

   This function accepts a connection on a CONNECT4LOW which is performing
     a listen. It will wait at most waitSecs for the connection.

   PARAMATERS

   listen is the CONNECT4LOW which is listening on a socket in order to
     accept a connection
   acc is an unused CONNECT4LOW structure which will be initialized and
     receives the accept
   waitSecs is # seconds to wait to attempt the connect before failing out.

   RETURNS

   r4success
   < 0  error, does NOT set CODE4.error

   NOTES

   This function does the following:
     Accept the connection into the new CONNECT4LOW accept structure
     Initialize the accept structure
     return
*/

   int status, rc ;
   fd_set  readfd, errorfd, readSet, errorSet ;
   struct timeval timest, timeNull = {0,0} ;
   char buf ;

   #ifdef E4PARM_LOW
      if ( (listen==0) || (acc==0 ) )
         return error4( 0, e4parmNull, E96904 ) ;
   #endif

   /* Find out waittime */
   memset(&timest, 0, sizeof(timest) ) ;
   if (waitSecs > 0)
      timest.tv_sec = waitSecs ;
   else
      timest.tv_sec = CON4LOW_ACCEPT_TIME ;

   while (!acc->sockr)
   {
      FD_ZERO(&readfd ) ;
      FD_SET(listen->sockr, &readfd) ;
      FD_ZERO(&errorfd ) ;
      FD_SET(listen->sockr, &errorfd) ;

      /*Check if an accept would get connected immeditatly */
      status = select((int)listen->sockr+1, &readfd, (fd_set *)0, &errorfd, &timest ) ;
      if (status == 0)  /*Time out */
      {
         FD_ZERO(  &errorSet ) ;
         FD_SET( acc->sockr, &errorSet ) ;

         status = select( 0, 0, 0, &errorSet, &timeNull ) ;
         if ( (status == 1) || (status < 0) )
           return e4connection ;

         /* It wasn't an error that stopped us */
         FD_ZERO ( &readSet ) ;
         FD_SET  ( acc->sockr, &readSet ) ;

         status = select( 0, &readSet, 0, 0, &timeNull ) ;
         if ( status == 1 )     /* There is data to be read.  Peek at message.*/
         {
            status = recv( acc->sockr, &buf, sizeof(char), MSG_PEEK ) ;
            #ifdef S4WINSOCK
               if ( status == INVALID_SOCKET )
            #endif
            #ifdef S4BERKSOCK
               if ( status < 0 )
            #endif
               return e4connection ;
            else
               return e4result ;
         }
         else /*if (status < 0 )*/
            return e4connection ;
      }
      else if ( (status != 1) || (!FD_ISSET((int)listen->sockr, &readfd )))
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }
      else /* there was an connection that is able to be accepted */
      {
         // AS Jan 30/03 - Need the CODE4 for encryption dll handling
         rc = connect4lowAccept( listen, acc, listen->c4 ) ;
         if (rc < 0 )
            return rc ;
      }
   }
   return r4success ;
}



void connect4lowBlast(CONNECT4LOW *low )
{
   if (low->sockw)
      low->sockr = low->sockw ;
   else
      low->sockw = low->sockr ;
}



int connect4lowCheck( CONNECT4LOW *connect )
{
   int value, rc, len ;

   len = sizeof(int) ;
   if (connect->sockw)
   {
      rc = getsockopt(connect->sockw, SOL_SOCKET, SO_ERROR, (char *)&value,  &len ) ;
      #ifdef S4WINSOCK
         if ( rc == SOCKET_ERROR )
      #else
         if ( rc < 0 )
      #endif
         {
            assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
            return e4socket ;
         }
      #ifdef S4WINSOCK
         if ( (value == WSAENETRESET ) || (value == WSAENOTCONN ) )
      #else
         if ( value == ETIMEDOUT )
      #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }
      len = sizeof(int) ;
   }

   if (connect->sockr)
   {
      rc = getsockopt(connect->sockr, SOL_SOCKET, SO_ERROR, (char *)&value,  &len ) ;
      #ifdef S4WINSOCK
         if ( rc == SOCKET_ERROR )
      #else
         if ( rc < 0 )
      #endif
         {
            assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
            return e4socket ;
         }
      #ifdef S4WINSOCK
         if ( (value == WSAENETRESET ) || (value == WSAENOTCONN ) )
      #else
         if ( value == ETIMEDOUT )
      #endif
         {
            assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
            return e4socket ;
         }
   }
   return r4success ;
}



int connect4lowClose(CONNECT4LOW *connect )
{
/*
   PARAMATERS
   connect is the connection to close

   RETURNS
   r4success
   < 0  error, does NOT set CODE4.error

   NOTES
   Use closesocket()
*/

   // AS Dec 6/06 variables used but not initialized
   int rc = 0, rc2 = 0 ;

   // AS 08/17/99 connect may be null, in which case just return, nothing to close...
   if ( connect == 0 )
      return 0 ;

   if (connect->sockw != 0 )
   {
      rc = closesocket(connect->sockw) ;
      #ifdef S4WINSOCK
         if ( rc != SOCKET_ERROR )
            connect->sockw = INVALID_SOCKET ;
      #else
         if ( rc < 0 )
            connect->sockw = 0 ;
      #endif
   }
   if (connect->sockr != 0 )
   {
      rc2 = closesocket(connect->sockr) ;
      #ifdef S4WINSOCK
         if ( rc2 != SOCKET_ERROR )
            connect->sockr = INVALID_SOCKET ;
      #else
         if ( rc2 < 0 )
            connect->sockr = 0 ;
      #endif
   }
   #ifdef S4WINSOCK
      if ( (rc == SOCKET_ERROR) || (rc2 == SOCKET_ERROR) )
   #else
      if ( (rc < 0) || (rc2 < 0 ) )
   #endif
      return e4connect ;
   return r4success ;
}



// AS Jan 30/03 - Need the CODE4 for encryption dll handling
int connect4lowConnect( CONNECT4LOW *conLow, C4ADDRESS *address, int addrLen, CODE4 *c4 )
{
   /* This function performs a standard connect */
   int rc ;
   S4LONG value = 0 ;

   rc = connect4lowConnectConnect( conLow, address, addrLen, c4 ) ;
   #ifdef S4CLIENT
      if (rc)
         return rc ;
      /* Always send 4 bytes. For compatibility with Java clients. */
      // AS May 13/02 - communications compression coding - don't compress the connection message
      rc = connect4lowWrite( conLow, (char *)&value, 4, 0 ) ;
      // AS Jun 9/06 - sometimes e4socket is returned here in Windows (Server 2003 O/S) if many connections are tried at the same time.
      // in that case just retry...
      if ( rc == e4socket )
         rc = r4noServerOnHost ;
   #endif
   return rc ;
}



int connect4lowReconnect(CONNECT4LOW *conLow, C4ADDRESS *address, int addrLen )
{
   /*
      AS 03/08/01 - this function is provided as part of the new connection process whereby the
      client side sets up both connections to the server, which avoids firewall problems.

      The input 'connection' is a connection which already has the first part of its connection
      initialized (via connect4lowConnect)

   */
   int rc ;

   rc = connect4lowConnectReconnect( conLow, address, addrLen ) ;
   #ifdef S4CLIENT
      if (rc)
         return rc ;
      // AS Dec 8/03 - need to network swap the byte
      S4LONG value = htonl5( -1 ) ;
      // a value of -1 means this is the 2nd connection part...
      // we need to ensure that we put it onto the 'sockr' socket since that is the
      // new socket being created...

      // AS 08/02/01 - Winsock - a Socket is a long, not a short... use Socket
      // short saveSocket = conLow->sockw ;
      SOCKET saveSocket = conLow->sockw ;
      conLow->sockw = conLow->sockr ;
      // AS May 13/02 - communications compression coding - don't compress the connection message
      rc = connect4lowWrite( conLow, (char *)&value, 4, 0 ) ;
      if ( rc == 0 )
      {
         unsigned short portNo = htons5( conLow->addressw.sin_port ) ;
         // AS May 13/02 - communications compression coding - don't compress the connection message
         rc = connect4lowWrite( conLow, (char *)&portNo, sizeof( unsigned short ), 0 ) ;
      }
      conLow->sockw = saveSocket ;
      if (rc)
         return rc ;

      // we also need to get a status 'rc' code back on this same socket (the read one we set up)
      short status = 0 ;   // AS Aug 2/01 - default to 0 so we don't return status if rc on next line is bad
      // AS Jan 9/02 - We wait a maximum time of CON4LOW_ACCEPT_INTERVAL, which is the interval of waiting for the server
      // to respond to our connection.  If this times out we return a timeout to the caller of the function who is then
      // ultimately responsible for handling the timeout by monitoring CODE4.acceptTimeOut to see if the final timeout has occurred
      // AS May 13/02 - communications compression coding - don't compress the connection message
      rc = connect4lowRead( conLow, (char *)&status, sizeof( short ), CON4LOW_ACCEPT_INTERVAL, 0 ) ;
      if ( status != 0 )
         return status ;
   #endif
   return rc ;
}



int connect4lowConnectReconnect( CONNECT4LOW *connection, C4ADDRESS *address, int addrLen )
{
   /*
      AS 03/08/01 - this function is provided as part of the new connection process whereby the
      client side sets up both connections to the server, which avoids firewall problems.

      The input 'connection' is a connection which already has the first part of its connection
      initialized (via connect4lowConnectConnect)

   */
   int rc ;

   #ifdef E4PARM_LOW
      if (connection == 0 )
         return error4( 0, e4parmNull, E96905 ) ;
   #endif

   connection->sockr = socket(AF_INET, SOCK_STREAM, 0 ) ;
   #ifdef S4WINSOCK
      if ( connection->sockr == INVALID_SOCKET )
   #else
      if ( connection->sockr <= 0 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   rc = connect(connection->sockr, (struct sockaddr *)address,  addrLen ) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
   {
      #ifndef S4WINSOCK
         switch (errno)
         {
            case ECONNREFUSED :
               rc = r4noServerOnHost;
               break;
            case ETIMEDOUT :
               rc = r4connectTimeOut;
               break;
            case ENETUNREACH :
            case EINPROGRESS :
               rc = r4errNetwork;
               rc = r4errNetwork;
               break;
            case EADDRINUSE :
               rc = r4hostUnavailable;
               break;
            default :
               rc = r4noConnect;
         }
      #else
         switch (WSAGetLastError())
         {
            case WSAENETDOWN :
            case WSAENOBUFS :
               rc = r4errNetwork ;
               break ;
            case WSAENETUNREACH :
            case WSAEADDRNOTAVAIL :
            case WSAEAFNOSUPPORT :
               rc = r4hostUnavailable ;
               break ;
            case WSAECONNREFUSED :
               rc = r4noServerOnHost ;
               break ;
            case WSAEMFILE :
               rc = r4numSockets ;
               break ;
            case WSAETIMEDOUT :
               rc = r4connectTimeOut ;
               break ;
            default :
               #ifndef S4OFF_LOG
                  // AS Sept 26/02 - log this error as it should never actually occur...
                  char buf[10] ;
                  c4ltoa45( WSAGetLastError(), buf, sizeof( buf ) ) ;
                  error4describe( 0, e4connect, E96901, "unknown network error: Number: ", buf, 0 ) ;
               #endif
               rc = r4noConnect ;
         }
      #endif
      connect4lowError(connection) ;
      return rc ;
   }
   return r4success ;
}


// AS Jan 30/03 - Need the CODE4 for encryption dll handling
int connect4lowConnectConnect( CONNECT4LOW *connection, C4ADDRESS *address, int addrLen, CODE4 *c4 )
{
/* PARAMATERS

   connect is the connection to connect on
   address is the internet address and portnumber to connect to

   RETURNS

   r4success
   < 0  error

   NOTES

   This function performs a blocked connect.

   This function does the following:
     Initializes the connect structure
     Performs the connect
     returns

   see socket4netConnectServer() in c4comws.c
*/

   int rc ;

   #ifdef E4PARM_LOW
      if (connection == 0 )
         return error4( 0, e4parmNull, E96905 ) ;
   #endif

   connection->sockw = socket( AF_INET, SOCK_STREAM, 0 ) ;
   #ifdef S4WINSOCK
      if ( connection->sockw == INVALID_SOCKET )
   #else
      if ( connection->sockw <= 0 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   rc = connect( connection->sockw, (struct sockaddr *)address,  addrLen ) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
   {
      #ifndef S4WINSOCK
         switch (errno)
         {
            case ECONNREFUSED :
               rc = r4noServerOnHost;
               break;
            case ETIMEDOUT :
               rc = r4connectTimeOut;
               break;
            case ENETUNREACH :
            case EINPROGRESS :
               rc = r4errNetwork;
               rc = r4errNetwork;
               break;
            case EADDRINUSE :
               rc = r4hostUnavailable;
               break;
            default :
               rc = r4noConnect;
         }
      #else
         switch (WSAGetLastError())
         {
            case WSAENETDOWN :
            case WSAENOBUFS :
               rc = r4errNetwork ;
               break ;
            case WSAENETUNREACH :
            case WSAEADDRNOTAVAIL :
            case WSAEAFNOSUPPORT :
               rc = r4hostUnavailable ;
               break ;
            case WSAECONNREFUSED :
               rc = r4noServerOnHost ;
               break ;
            case WSAEMFILE :
               rc = r4numSockets ;
               break ;
            case WSAETIMEDOUT :
               rc = r4connectTimeOut ;
               break ;
            default :
               #ifndef S4OFF_LOG
                  // AS Sept 26/02 - log this error as it should never actually occur...
                  char buf[10] ;
                  c4ltoa45( WSAGetLastError(), buf, sizeof( buf ) ) ;
                  error4describe( 0, e4connect, E96901, "unknown network error: Number: ", buf, 0 ) ;
               #endif
               rc = r4noConnect ;
         }
      #endif
      connect4lowError(connection) ;
      return rc ;
   }
   if (connect4lowInit( connection, 0, c4 ) )
   {
      connect4lowError( connection ) ;
      return e4connect ;
   }
   return r4success ;
}


// AS Jan 30/03 - Need the CODE4 for encryption dll handling
static int connect4lowInit( CONNECT4LOW *con, int readsock, CODE4 *c4 )
{
   /* NOTES

      This function is used to initialize the socket options of a valid socket
        handle.

      It should set the following options:
        int yesFlag = 1, OFlag = 0 ;
        long lyesFlag = 1 ;
        setsockopt(con->sock, SOL_SOCKET, SO_KEEPALIVE, (char *)&yesFlag,  sizeof(int) ) ;
        setsockopt(con->sock, IPPROTO_TCP, TCP_NODELAY, (char *)&yesFlag,  sizeof(int) ) ;
        setsockopt(con->sock, SOL_SOCKET, SO_SNDBUF, (char *)&OFlag,  sizeof(int) ) ;
        ioctl(con->sock, FIONBIO, &lyesFlag ) ;
   */

   int rc, yesFlag = 1, OFlag = 0 ;
   long lyesFlag = 1 ;
   #ifdef S4WINSOCK
      SOCKET sock ;
   #else
      int sock ;
   #endif

   #ifdef E4PARM_LOW
      if (con == 0 )
         return error4( 0, e4parmNull, E96907 ) ;
   #endif

   if (readsock)
      sock = con->sockr ;
   else
      sock = con->sockw ;

   rc = setsockopt( sock, SOL_SOCKET, SO_KEEPALIVE, (char *)&yesFlag,  sizeof(int) ) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   rc = setsockopt( sock, IPPROTO_TCP, TCP_NODELAY, (char *)&yesFlag,  sizeof(int) ) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   rc = ioctl( sock, FIONBIO, (unsigned long *)&lyesFlag ) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   if ( !readsock )
   {
      int len = sizeof( con->addressw ) ;
      rc = getsockname( sock, (struct sockaddr *)(&con->addressw), &len ) ;
      #ifdef S4WINSOCK
         if ( rc == SOCKET_ERROR )
      #else
         if ( rc < 0 )
      #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }
   }

   #ifdef S4COMPRESS
      // disable compression to start - set level to 0 and min to-compress length to max send length...
      // later on, as part of completing the connection, the client does a test and then configures the connection
      // compression.
      con->compressLevel = 0 ;
      con->minLength = COMPRESS4MAX_LENGTH ;
   #endif

   // AS Mar 12/03 - make generally available
   // AS Jan 30/03 - Need the CODE4 for encryption dll handling
   con->c4 = c4 ;
   #ifdef S4PREPROCESS_COM
      memset( &con->preprocess, 0, sizeof( con->preprocess ) ) ;
   #endif

   return r4success ;
}



int connect4lowListen(CONNECT4LOW *hear, CODE4 *c4, short *portNo, int maxPendingConnects )
{
/* PARAMATERS

   listen is an unused CONNECT4LOW structure which will be initialized and
     used to perform the listen
   c4 is the CODE4 structure
   portNo is the port # on which to listen.  If the portNo is -1, then a
     port number is dynamically assigned.
   maxPendingConnects is the maximum # of pending connects allowed on the
     new listen.

   RETURNS

   >= 0 The port number on which the listen is being done is returned.
   < 0  error, does NOT set CODE4.error

   NOTES

   This function is used to create a listen on a port number for any new
     connections.

   I have included extractions from c4comws.c where useful;  the are marked
     with .>. characters.  Code is extracted from socket4netInitServer()

   This function does the following:
     Initialize the listen structure

     Initializes a new socket
        > sock  = socket( AF_INET, SOCK_STREAM, 0 ) ;
        > if (sock == INVALID_SOCKET)
        >    return error4( socket4net->cb, e4socket, S4WSAERROR ) ;

     Bind it to the port, if given
        > localSin.sin_addr.s_addr = INADDR_ANY ;
        > localSin.sin_family = AF_INET ;
        > localSin.sin_port = htons( portNo ) ;

     To bind to an arbitrary port #, assign '0' as the port number.
        eg.   localSin.sin_addr.s_addr = INADDR_ANY ;
        eg.   localSin.sin_family = AF_INET ;
        eg.   localSin.sin_port = 0             ;

     Reinitialize the listening socket:
         To reinit listening socket without rebooting
        > if ( setsockopt( sock, SOL_SOCKET, SO_REUSEADDR,
               (const char FAR *)&yesFlag, sizeof(int) ) < 0 )
        >    return error4( socket4net->cb, e4socket, S4WSAERROR ) ;
        > if ( bind( sock, (struct sockaddr far *) &localSin,
               sizeof( localSin ) ) < 0 )
        >    return error4( socket4net->cb, e4socket, S4WSAERROR ) ;
        > if ( listen( sock, WS4MAX_PENDING_CONNECTS ) < 0 )
        >    return error4( socket4net->cb, e4socket, S4WSAERROR ) ;

     If no port number was specified, get it now (after the bind).  This
       can be done using getHostName().

     Perform the listen
        > if ( listen( socket4net->listensock, WS4MAX_PENDING_CONNECTS ) < 0 )
        >    return error4( socket4net->cb, e4socket, S4WSAERROR ) ;

     Return the port number
*/

   struct sockaddr_in localSin ;
   int rc, yesFlag = 1, size;

   #ifdef E4PARM_LOW
      if (hear == 0)
         return error4( 0, e4parmNull, E96908 ) ;
   #endif

   hear->sockr = socket(AF_INET, SOCK_STREAM, 0 ) ;
   #ifdef S4WINSOCK
      if ( hear->sockr == INVALID_SOCKET )
   #else
      if ( hear->sockr == -1 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   rc = setsockopt(hear->sockr, SOL_SOCKET, SO_REUSEADDR, (char *)&yesFlag,  sizeof(int) ) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
   {
      connect4lowError( hear ) ;
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }

   #ifdef S4UNIX
      memset(&localSin, 0, sizeof(sockaddr_in));
   #endif
   localSin.sin_family = AF_INET ;
   if (*portNo == -1)
      localSin.sin_port = 0 ;
   else
      localSin.sin_port = htons(*portNo) ;
   localSin.sin_addr.s_addr = htonl(INADDR_ANY) ;

   rc = bind( hear->sockr, (struct sockaddr *)&localSin, sizeof(localSin));
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
   {
      connect4lowError( hear ) ;
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }

   size = sizeof(localSin) ;
   rc = getsockname(hear->sockr, (struct sockaddr *)&localSin, &size ) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
   {
      connect4lowError( hear ) ;
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }

   rc = listen(hear->sockr, maxPendingConnects) ;
   #ifdef S4WINSOCK
      if ( rc == SOCKET_ERROR )
   #else
      if ( rc < 0 )
   #endif
   {
      connect4lowError( hear ) ;
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }

   *portNo = localSin.sin_port ;
   return r4success ;
}



TCP4ADDRESS connect4lowLocalTcpAddress( CONNECT4LOW *conlow )
{
   TCP4ADDRESS address ;
   struct sockaddr name ;
   struct sockaddr_in *in ;
   int nameLen ;

   nameLen = sizeof( name ) ;

   getsockname( conlow->sockr, &name, &nameLen ) ;

   in = (struct sockaddr_in *)&(name) ;

   #ifdef S4WINTEL
      address.tcpAddress[0] = in->sin_addr.S_un.S_un_b.s_b1 ;
      address.tcpAddress[1] = in->sin_addr.S_un.S_un_b.s_b2 ;
      address.tcpAddress[2] = in->sin_addr.S_un.S_un_b.s_b3 ;
      address.tcpAddress[3] = in->sin_addr.S_un.S_un_b.s_b4 ;
   #else
      memcpy(&address.tcpAddress, &in->sin_addr.s_addr, 4) ;
   #endif
   return address ;
}



TCP4ADDRESS connect4lowPeerTcpAddress( CONNECT4LOW *conlow )
{
   TCP4ADDRESS address ;
   struct sockaddr name ;
   struct sockaddr_in *in ;
   int nameLen ;

   nameLen = sizeof( name ) ;

   #ifdef S4SERVER
      getpeername( conlow->sockr, &name, &nameLen ) ;
   #else
      getpeername( conlow->sockw, &name, &nameLen ) ;
   #endif

   in = (struct sockaddr_in *)&(name) ;

   #ifdef S4WINTEL
      address.tcpAddress[0] = in->sin_addr.S_un.S_un_b.s_b1 ;
      address.tcpAddress[1] = in->sin_addr.S_un.S_un_b.s_b2 ;
      address.tcpAddress[2] = in->sin_addr.S_un.S_un_b.s_b3 ;
      address.tcpAddress[3] = in->sin_addr.S_un.S_un_b.s_b4 ;
   #else
      memcpy(&address.tcpAddress, &in->sin_addr.s_addr, 4) ;
   #endif
   return address ;
}


static int connect4lowReadDo( CONNECT4LOW *conlow, char *buffer, int len, long timeoutInSeconds )
{
   int left = len, rc, dummy ;
   char *ptr = buffer ;
   fd_set  readfd, errorfd ;
   #ifdef S4LEN_PRINT
      char bufErr[128] ;
   #endif

   do
   {
      FD_ZERO(&readfd ) ;
      FD_SET(conlow->sockr, &readfd) ;
      FD_ZERO(&errorfd ) ;
      FD_SET(conlow->sockr, &errorfd) ;

      // AS 08/17/99 -- was occasionally waiting forever here, even when client had disconnected...
      struct timeval timeout ;
      timeout.tv_usec = 0 ;
      if ( timeoutInSeconds <= 0 )   // default for when not set...
         timeout.tv_sec = CON4LOW_ACCEPT_TIME ;
      else
         timeout.tv_sec = timeoutInSeconds ;

      // #if defined( S4WIN32 ) && !defined( S4OFF_THREAD ) && defined( S4CLIENT )
      //    // we wait for this interval time before timeout out.  the caller of this function will use c4->acceptTimeOut to
      //    // determine the number of times to call the lowRead function prior to timing out
      //    timeout.tv_sec = CON4LOW_ACCEPT_INTERVAL ;
      // #else
      //    timeout.tv_sec = CON4LOW_ACCEPT_TIME ;
      // #endif
      dummy = select((int)conlow->sockr+1, &readfd, (fd_set *)0, &errorfd, &timeout ) ;

      if ( (dummy != 1 ) || (!FD_ISSET((int)conlow->sockr, &readfd )) || FD_ISSET((int)conlow->sockr, &errorfd ))
      {
         int rcSave = -1 ;

         if ( FD_ISSET( (int)conlow->sockr, &errorfd ) )
         {
            rcSave = getSockErr(WSAGetLastError()) ;
         }
         else if ( dummy == 0 )  // means timeout
         {
            rcSave = r4connectTimeOut ;
         }

         connect4lowError(conlow) ;
         return rcSave;
      }

      rc = recv(conlow->sockr, ptr, left, 0 ) ;
      if (rc <= 0)
      {
         int rcSave = getSockErr(WSAGetLastError()) ;
         connect4lowError(conlow) ;
         if ( rcSave == 0 )
            return -1 ;
         return rcSave;
      }

      left -= rc ;
      ptr += rc ;
   } while (left > 0 ) ;
   #ifdef S4LEN_PRINT
      sprintf(bufErr, "Read: %d, on socket %d", len, conlow->sockr ) ;
      debug4display(bufErr) ;
   #endif

   return r4success ;
}



// AS Jan 9/02 - Added timeoutInSeconds paramater to better affect timeout spans.  In particular when the client
// is reconnecting to the server we want this value small because sometimes the server and client miscommunicate and
// we timeout.  Set to 0 or -1 to use a default timeout instead (CON4LOW_ACCEPT_TIME)
int connect4lowRead( CONNECT4LOW *conlow, char *buffer, int len, long timeoutInSeconds, Bool5 doCompress )
{
   #ifdef E4PARM_LOW
      if ((connect == 0) || (buffer == 0 ) || ( len <= 0 ) )
         return error4( 0, e4parmNull, E96909 ) ;
   #endif

   char info[3] ;  // store the compress/preprocess flags and the length.
   int rc = 0 ;

   // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
   // get the flags and compressed length of the message first
   unsigned short lenReceive ;
   #ifdef S4SERVER
      #ifdef S4STAMP_BUILD
         // AS Dec 1/06 - allow old cb clients to connect

         if ( 0 )
      #else
         if ( conlow->javaClient == 0 )
      #endif
   #endif
   {
      rc = connect4lowReadDo( conlow, info, 3, timeoutInSeconds ) ;
      if ( rc != 0 )
         return rc ;
      lenReceive = htons5(*(short *)(info+1)) ;  // length used for uncompression

      #ifdef S4PREPROCESS_COM
         #if defined( S4ENCRYPT_DLL )
            if ( !( ( con4lowGetEncryptRead( conlow->c4, conlow ) == 1 ) && ( info[0] & r4preprocessed ) ) )
            {
               if ( con4lowGetEncryptRead( conlow->c4, conlow ) == 1 )   // this is a severe error if not sending preprocessed data on an preprocessed connection
                  return error4describe( 0, e4info, E96909, "other party has sent unprocessed data on an processed connection", 0, 0 ) ;
            }
         #else
            if ( !(( preprocess4getRead( &(conlow->preprocess) ) == 1 ) && ( info[0] & r4preprocessed ) ))
            {
               if ( preprocess4getRead( &(conlow->preprocess) ) == 1 )   // this is a severe error if not sending preprocessed data on an preprocessed connection
                  return error4describe( 0, e4info, E96909, "other party has sent unprocessed data on an processed connection", 0, 0 ) ;
            }
         #endif
      #endif

      if ( lenReceive > len )  // bad connection data
         return error4( 0, e4connect, E96909 ) ;

      assert5( lenReceive <= len ) ;  // ensure we have room for the message (ensure it really was compressed or at least not lengthened)
   }
   #ifdef S4SERVER
      else
      {
         lenReceive = len ;
      }
   #endif
   rc = connect4lowReadDo( conlow, buffer, lenReceive, timeoutInSeconds ) ;
   if ( rc != 0 )
      return rc ;

   #ifdef S4SERVER
      #ifdef S4STAMP_BUILD
         // AS Dec 1/06 - allow old cb clients to connect

         if ( 0 )
      #else
         if ( conlow->javaClient == 0 )
      #endif
   #endif
   {
      #ifdef S4PREPROCESS_COM
         #if defined( S4ENCRYPT_DLL )
            assert5( ( con4lowGetEncryptRead( conlow->c4, conlow ) == 1 && ( info[0] & r4preprocessed ) ) || ( con4lowGetEncryptRead( conlow->c4, conlow ) == 0 && !( info[0] & r4preprocessed )) ) ;
            if ( info[0] & r4preprocessed )
               con4lowPostProcess( conlow->c4, conlow->preprocess, (unsigned char *)buffer, lenReceive, 0 ) ;   // compressed in place, returning the length of the compressed data
            else
            {
               if ( con4lowGetEncryptRead( conlow->c4, conlow ) == 1 )   // this is a severe error if not sending preprocessed data on an preprocessed connection
               return error4describe( 0, e4info, E96909, "other party has sent unprocessed data on a processed connection", 0, 0 ) ;
            }
         #else
            assert5( ( preprocess4getRead( &(conlow->preprocess) ) == 1 && ( info[0] & r4preprocessed ) ) || ( preprocess4getRead( &(conlow->preprocess) ) == 0 && !( info[0] & r4preprocessed )) ) ;
            if ( info[0] & r4preprocessed )
               connect4lowPostProcess( &conlow->preprocess, (unsigned char *)buffer, lenReceive ) ;   // compressed in place, returning the length of the compressed data
            else
            {
               if ( preprocess4getRead( &(conlow->preprocess) ) == 1 )   // this is a severe error if not sending preprocessed data on an preprocessed connection
               return error4describe( 0, e4info, E96909, "other party has sent unprocessed data on a processed connection", 0, 0 ) ;
            }
         #endif
      #endif

      #ifdef S4COMPRESS
         if ( info[0] & r4compressed )
            len = connect4lowUncompress( conlow->c4, (unsigned char *)buffer, lenReceive, len ) ;   // compressed in place, returning the length of the compressed data
      #endif

      assert5( len == lenReceive ) ;   // low read must match quantities exactly
   }

   return r4success ;
}



static int connect4lowWriteDo( CONNECT4LOW *conlow, char *buffer, int len )
{
   int nWritten, dummy, left ;
   char *ptr =  buffer ;
   fd_set  writefd, errorfd ;
   #ifdef S4LEN_PRINT
      char bufErr[128] ;
   #endif

   left = len ;
   while(left > 0 )
   {
      nWritten = send(conlow->sockw, ptr, left, 0) ;
      if (nWritten == -1 )
      {
         #ifdef S4WINTEL
            // AS Jun 9/06 - try to improve the error handling a little...
            int rc = WSAGetLastError() ;
            if ( rc == WSAEWOULDBLOCK )
         #else
            if ( errno == EAGAIN )
         #endif
         {
            FD_ZERO(&writefd ) ;
            FD_SET(conlow->sockw, &writefd) ;
            FD_ZERO(&errorfd ) ;
            FD_SET(conlow->sockw, &errorfd) ;
            dummy = select((int)conlow->sockw+1, (fd_set *)0, &writefd, &errorfd, 0 ) ;
            if ( (dummy != 1 ) || (!FD_ISSET((int)conlow->sockw, &writefd )) ||
                FD_ISSET((int)conlow->sockw, &errorfd ))
            {
               connect4lowError(conlow) ;
               assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
               return e4socket;
            }
            nWritten = send(conlow->sockw, ptr, left, 0 ) ;
            if (nWritten < 0 )
            {
               connect4lowError(conlow) ;
               assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
               return e4socket ;
            }
         }
         else
         {
            // int i = 0 ;
            // int j = 4 / i ;
            connect4lowError(conlow) ;
            return e4socket ;
         }
      }
      left -= nWritten ;
      ptr += nWritten ;
   }
   #ifdef S4LEN_PRINT
      sprintf(bufErr, "Sent: %d, on socket %d", len, conlow->sockw ) ;
      debug4display(bufErr) ;
   #endif
   return r4success ;
}



int connect4lowWrite( CONNECT4LOW *conlow, char *buffer, unsigned short len, Bool5 doCompress )
{
   #ifdef E4PARM_LOW
      if ((connect == 0) || (buffer == 0 ) || ( len <= 0 ) )
         return error4( 0, e4parmNull, E96910 ) ;
   #endif
   // AS May 13/02 - communications compression coding

   char info[3] ;  // store the compress/preprocess flags and the length.
   info[0] = 0 ;
   unsigned short compLen ;

   // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
   // get the flags and compressed length of the message first
   #ifdef S4SERVER
      #ifdef S4STAMP_BUILD
         // AS Dec 1/06 - allow old cb clients to connect

         if ( 1 )
      #else
         if ( conlow->javaClient == 1 )
      #endif
      {
         compLen = len ;
      }
      else
   #endif
   {
      #ifdef S4COMPRESS
         if ( len >= conlow->minLength )
         {
            Bool5 didCompress ;
            compLen = connect4lowCompress( conlow, (unsigned char *)buffer, len, &didCompress ) ;   // compressed in place, returning the length of the compressed data
            assert5( compLen <= len ) ;  // ensure the message was actually compressed (or at least not made larger)
            if ( didCompress )
               info[0] |= r4compressed ;
         }
         else
      #endif
            compLen = len ;

      #ifdef S4SERVER
         // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
         #ifdef S4STAMP_BUILD
            // AS Dec 1/06 - allow old cb clients to connect

            if ( 0 )
         #else
            if ( conlow->javaClient == 0 )
         #endif
      #endif
      {
         *((short *)(info +1)) = htons5( compLen ) ;

         #ifdef S4PREPROCESS_COM
            #ifdef S4ENCRYPT_DLL
               if ( con4lowGetEncryptWrite( conlow->c4, conlow ) == 1 )
               {
                  short blockSize = conlow->blockSize ;
                  assert5( ( len % blockSize ) == 0 ) ;
                  if ( ( len % blockSize ) == 0 )
                  {
                     con4lowPreProcess( conlow->c4, conlow->preprocess, (unsigned char *)buffer, len ) ;
                     info[0] |= r4preprocessed ;
                  }
               }
            #else
               if ( preprocess4getWrite( &(conlow->preprocess) ) == 1 )
               {
                  short blockSize = preprocess4getBlockSize( &(conlow->preprocess) ) ;
                  assert5( ( len % blockSize ) == 0 ) ;
                  if ( ( len % blockSize ) == 0 )
                  {
                     connect4lowPreprocess( &conlow->preprocess, (unsigned char *)buffer, len ) ;
                     info[0] |= r4preprocessed ;
                  }
               }
            #endif
         #endif

         // send the compressed length first
         int rc = connect4lowWriteDo( conlow, info, 3 ) ;
         if ( rc < 0 )
            return rc;
      }
   }

   return connect4lowWriteDo( conlow, buffer, compLen ) ;
}



#ifndef S4OFF_THREAD
int connect4lowReadAsynchronous(CONNECT4LOW *connect, NET4MESSAGE *message, EVENT4 *event, Bool5 doCompress )
{
/* PARAMATERS

   connect is the connection on which read data is being requested;
     connect->socket contains the socket on which to read
       the information
   message is the NET4MESSAGE which encapsulates the following information:
     NET4MESSAGE->overlap is the overlap structure rerquired by the WIN32
       API to perform asynchronous i/o.
     NET4MESSAGE->message is the buffer to read into
     NET4MESSAGE->messageLen is the length of the buffer to read into
   event contains the event handle which will be signalled when the
     asynchronous operation has completed.

   RETURNS

   r4success means the write operation has completed successfully.  (i.e.
     this return means that the operation completed without pending,
     and thus the event was not used)
   r4pending means the write operation is pending.
   e4outstanding means the read oepration failed due to too many
     outstanding read packets.  This means that the function could not
     be completed at this time, and should be re-attempted only after a
     currently pending asynchronous read call has been completed,
     does NOT set CODE4.error
   other < 0 critical connection error, does NOT set CODE4.error

   NOTES

   Low level function to perform an asynchronous read

   This function does the following:
     Initialize the NET4MESSAGE.overlap structure as required:
       memset NET4MESSAGE.overlap to zero
       set NET4MESSAGE.overlap.event to the input EVENT4.handle
     Perform the asynchronous ReadFile() request
     If the ReadFile() returns true:
       return r4success
     If the ReadFile() returns false:
       Call GetLastError(), if result is 'ERROR_INVALID_USER_BUFFER' or
         'ERROR_NOT_ENOUGH_MEMORY', there are too many outstanding buffers,
         so return e4outstanding
       If GetLastError() returned ERROR_IO_PENDING, this means that the
         read is being performed asynchronously, so return r4pending
       Otherwise, the connection failed:
         call connect4lowError
         return e4socket
*/

   int readStart ;
   int dummy ;
   #ifdef S4REQUEST_PRINT
      char buffer[128] ;
      #ifdef S4LEN_PRINT
         LARGE_INTEGER ts ;
      #endif
   #else
      #ifdef S4LEN_PRINT
         char buffer[128] ;
         LARGE_INTEGER ts, tf ;
         double ls, lf ;
      #endif
   #endif

   #ifdef E4PARM_LOW
      if ( (message == 0 ) || ( event == 0 ) )
         return error4( 0, e4parmNull, E96911 ) ;
   #endif

   if ( connect == 0 )   /* AS 08/20/99 -- possible if connecting failed, to get here with null connect - just clear out */
   {
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }

   message->overlap.Offset = 0 ;
   message->overlap.OffsetHigh = 0 ;
   message->overlap.hEvent = event->handle ;
   dummy = event4reset(event) ;
   if ( dummy < 0)
   {
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }

   #ifdef S4REQUEST_PRINT
      /* note that message->messageLen is always total message size because it only gets set later when checking overlapped status */
      sprintf(buffer, "Pending read requested on socket %d", (int)connect->sockr ) ;
      debug4display(buffer) ;
   #endif

   // AS 05/07/99 --> was not checking this and occasionally crashing out...
   #ifdef S4WINSOCK
      if ( connect->sockr == INVALID_SOCKET )
   #endif
   #ifdef S4BERKSOCK
      if ( connect->sockr < 0 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   readStart = ReadFile( (HANDLE)connect->sockr, message->message, message->messageLen, (unsigned long *)&dummy, &message->overlap  ) ;
   if (readStart)
   {
      #ifdef S4LEN_PRINT
         QueryPerformanceCounter(&ts) ;
         QueryPerformanceFrequency(&tf) ;
         ls = ts.QuadPart ;
         lf = tf.QuadPart ;
         /* note that message->messageLen is always total message size because it only gets set later when checking overlapped status */
         sprintf(buffer, "Immediate read completed on socket %3d at time %f", (int)connect->sockr, (double)ls/lf ) ;
         debug4display(buffer) ;
      #endif
      return r4success ;
   }

   switch( GetLastError() )
   {
      case ERROR_IO_PENDING:
         return r4pending ;
      case ERROR_INVALID_USER_BUFFER:
      case ERROR_NOT_ENOUGH_MEMORY:
        return e4outstanding  ;
      default:
      {
         connect4lowError(connect) ;
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket;
      }
   }
}



int connect4lowWriteAsynchronous(CONNECT4LOW *conLow, NET4MESSAGE *message, EVENT4 *event, Bool5 doCompress )
{
/* PARAMATERS

   conLow is the connection on which to perform the write
     connect->socket contains the socket on which to transmit
       the information
   message is the NET4MESSAGE which encapsulates the following information:
     NET4MESSAGE->overlap is the overlap structure rerquired by the WIN32
       API to perform asynchronous i/o.
     NET4MESSAGE->messageLen is the length of the message to send
     NET4MESSAGE->message is the message to send
   event contains the event handle which will be signalled when the
     asynchronous operation has completed.

   RETURNS

   r4success means the write operation has completed successfully.  (i.e.
     this return means that the operation completed without pending,
     and thus the event was not used)
   r4pending means the write operation is pending.
   e4outstanding means the write oepration failed due to too many
     outstanding write packets.  This means that the function could not
     be completed at this time, and should be re-attempted only after a
     currently pending asynchronous write call has been completed,
     does NOT set CODE4.error
   other < 0 critical connection error, does NOT set CODE4.error

   NOTES

   Low level function to perform an asynchronous write.

   This function does the following:
     Initialize the NET4MESSAGE.overlap structure as required.
     Perform the asynchronous WriteFile() request
     If the WriteFile() returns true:
       return r4success
     If the WriteFile() returns false:
       Call GetLastError(), if result is 'ERROR_INVALID_USER_BUFFER' or
         'ERROR_NOT_ENOUGH_MEMORY', there are too many outstanding buffers,
         so return e4outstanding
       If GetLastError() returned ERROR_IO_PENDING, this means that the
         write is being performed asynchronously, so return r4pending
       Otherwise, the connection failed:
         call connect4lowError
         return e4socket
*/

   // AS May 13/02 - communications compression coding
   message->flag = 0 ;
   #ifdef S4SERVER
      // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
         // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
         #ifdef S4STAMP_BUILD
            // AS Dec 1/06 - allow old cb clients to connect

            if ( 0 )
         #else
            if ( conLow->javaClient == 0 )
         #endif
   #endif
   {
      #ifdef S4COMPRESS
         if ( message->messageLen >= conLow->minLength )
         {
            Bool5 didCompress ;
            message->messageLen = connect4lowCompress( conLow, (unsigned char *)message->message, message->messageLen, &didCompress ) ;   // compressed in place, returning the length of the compressed data
            assert5( message->messageLen >= 0 ) ;
            assert5( message->messageLen <= message->messageBufferLen ) ;  // ensure the message was actually compressed (or at least not made larger)
            if ( didCompress )
               message->flag |= r4compressed ;
         }
      #endif

      message->messageLenInternal = ntohs5( message->messageLen ) ;

      #ifdef S4PREPROCESS_COM
         #if defined( S4ENCRYPT_DLL )
            if ( con4lowGetEncryptWrite( conLow->c4, conLow ) == 1 )
            {
               short blockSize = conLow->blockSize ;
               if ( blockSize == 0 )  // should never occur... - means error
                  return -1 ;

               if ( ( message->messageLen % blockSize ) != 0 )
               {
                  assert5( ( message->messageBufferLen % blockSize ) == 0 ) ;  // should be on preprocession block boundary
                  message->messageLen += (blockSize - (message->messageLen % blockSize ) ) ;
                  assert5( message->messageLen <= message->messageBufferLen ) ;  // we shouldn't have exceeded due to previous assertion
               }

               if ( con4lowPreProcess( conLow->c4, conLow->preprocess, (unsigned char *)message->message, message->messageLen ) < 0 )
                  return -1 ;
               message->flag |= r4preprocessed ;
            }
         #else
            if ( preprocess4getWrite( &conLow->preprocess ) == 1 )
            {
               short blockSize = preprocess4getBlockSize( &(conLow->preprocess) ) ;
               if ( blockSize == 0 )  // should never occur... - means error
                  return -1 ;

               if ( ( message->messageLen % blockSize ) != 0 )
               {
                  assert5( ( message->messageBufferLen % blockSize ) == 0 ) ;  // should be on preprocession block boundary
                  message->messageLen += (blockSize - (message->messageLen % blockSize ) ) ;
                  assert5( message->messageLen <= message->messageBufferLen ) ;  // we shouldn't have exceeded due to previous assertion
               }

               if ( connect4lowPreprocess( &conLow->preprocess, (unsigned char *)message->message, message->messageLen ) < 0 )
                  return -1 ;
               message->flag |= r4preprocessed ;
            }
         #endif
      #endif
   }

   int writeStart ;
   int dummy ;
   #ifdef S4LEN_PRINT
      char buffer[128] ;
      LARGE_INTEGER ts, tf ;
      double ls, lf ;
   #endif

   #ifdef E4PARM_LOW
      if ((conLow == 0) || (message == 0 ) || ( event == 0 ) )
         return error4( 0, e4parmNull, E96912 ) ;
   #endif

   memset(&message->overlap, 0, sizeof(OVERLAPPED) ) ;
   message->overlap.hEvent = event->handle ;
   dummy = event4reset(event) ;
   if ( dummy < 0 )
   {
      assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
      return e4socket ;
   }

   // AS 05/07/99 --> was not checking this and occasionally crashing out...
   #ifdef S4WINSOCK
      if ( conLow->sockw == INVALID_SOCKET )
   #endif
   #ifdef S4BERKSOCK
      if ( conLow->sockw < 0 )
   #endif
      {
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
      }

   // AS May 14/02
   // with the compression, we also need to send the flag and (uncompressed) message length (just point to message->flag which
   // resides just before message->message in the structure
   // writeStart = WriteFile( (HANDLE)conLow->sockw,  message->message, message->messageLen, (unsigned long *)&dummy, &message->overlap  ) ;

   unsigned short writeLen ;
   char *writePoint ;
   #ifdef S4SERVER
      // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
      #ifdef S4STAMP_BUILD
         // AS Dec 1/06 - allow old cb clients to connect

         if ( 0 )
      #else
         if ( conLow->javaClient == 0 )
      #endif
   #endif
   {
      writeLen = message->messageLen + 3 ;
      writePoint = &message->flag ;
   }
   #ifdef S4SERVER
      else
      {
         writeLen = message->messageLen ;
         writePoint = message->message ;
      }
   #endif

   writeStart = WriteFile( (HANDLE)conLow->sockw,  writePoint, writeLen, (unsigned long *)&dummy, &message->overlap  ) ;
   #ifdef S4LEN_PRINT
      QueryPerformanceCounter(&ts) ;
      QueryPerformanceFrequency(&tf) ;
      ls = ts.QuadPart ;
      lf = tf.QuadPart ;
      sprintf(buffer, "Sent: %3d, on socket %3d at time %f", writeLen, conLow->sockw, (double)ls/lf ) ;
      debug4display(buffer) ;
   #endif
   if (writeStart)
      return r4success ;

   switch ( GetLastError() )
   {
      case ERROR_IO_PENDING:
         return (r4pending) ;
      case ERROR_INVALID_USER_BUFFER:
      case ERROR_NOT_ENOUGH_MEMORY:
         return e4outstanding ;
      default:
      {
         connect4lowError(conLow) ;
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket;
      }
   }
}



int connect4lowWriteSynchronous( CONNECT4LOW *conLow, NET4MESSAGE *message, Bool5 doCompress )
{
/* PARAMATERS

   conLow is the connection on which to perform the write
     conLow->socket contains the socket on which to transmit
       the information
   message is the NET4MESSAGE which encapsulates the following information:
     NET4MESSAGE->messageLen is the length of the message to send
     NET4MESSAGE->message is the message to send

   RETURNS

   r4success means the write operation has completed successfully.
   other < 0 critical connection error, does NOT set CODE4.error

   NOTES

   Low level function to perform a synchronous write.

   This function does the following:
     Perform the synchronous send() request, and return the result.
*/

   int sent = 0, rc, dummy ;
   fd_set  writefd, errorfd ;
   #ifdef S4LEN_PRINT
      char buffer[128] ;
      LARGE_INTEGER ts ;
   #endif

   #ifdef E4PARM_LOW
      if ((conLow == 0) || (message == 0 ) )
         return error4( 0, e4parmNull, E96913 ) ;
   #endif

   // AS May 13/02 - communications compression coding
   message->flag = 0 ;
   #ifdef S4SERVER
      // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
      #ifdef S4STAMP_BUILD
         // AS Dec 1/06 - allow old cb clients to connect

         if ( 0 )
      #else
         if ( conLow->javaClient == 0 )
      #endif
   #endif
   {
      #ifdef S4COMPRESS
         if ( message->messageLen >= conLow->minLength )
         {
            Bool5 didCompress ;
            message->messageLen = connect4lowCompress( conLow, (unsigned char *)message->message, message->messageLen, &didCompress ) ;   // compressed in place, returning the length of the compressed data
            assert5( message->messageLen <= message->messageBufferLen ) ;  // ensure the message was actually compressed (or at least not made larger)
            if ( didCompress )
               message->flag |= r4compressed ;
         }
      #endif

      message->messageLenInternal = ntohs5( message->messageLen ) ;

      #ifdef S4PREPROCESS_COM
         #if defined( S4ENCRYPT_DLL )
            if ( con4lowGetEncryptWrite( conLow->c4, conLow ) == 1 )
            {
               short blockSize = conLow->blockSize ;
               if ( ( message->messageLen % blockSize ) != 0 )
               {
                  assert5( ( message->messageBufferLen % blockSize ) == 0 ) ;  // should be on preprocession block boundary
                  message->messageLen += (blockSize - (message->messageLen % blockSize ) ) ;
                  assert5( message->messageLen <= message->messageBufferLen ) ;  // we shouldn't have exceeded due to previous assertion
               }

               con4lowPreProcess( conLow->c4, conLow->preprocess,(unsigned char *)message->message, message->messageLen ) ;
               message->flag |= r4preprocessed ;
            }
         #else
            if ( preprocess4getWrite( &(conLow->preprocess) ) == 1 )
            {
               short blockSize = preprocess4getBlockSize( &(conLow->preprocess) ) ;
               if ( ( message->messageLen % blockSize ) != 0 )
               {
                  assert5( ( message->messageBufferLen % blockSize ) == 0 ) ;  // should be on preprocession block boundary
                  message->messageLen += (blockSize - (message->messageLen % blockSize ) ) ;
                  assert5( message->messageLen <= message->messageBufferLen ) ;  // we shouldn't have exceeded due to previous assertion
               }

               connect4lowPreprocess( &conLow->preprocess, (unsigned char *)message->message, message->messageLen ) ;
               message->flag |= r4preprocessed ;
            }
         #endif
      #endif
   }

   char *sendPtr ;
   long lenToSend ;
   #ifdef S4SERVER
      // AS Dec 5/03 - Java clients do not support accepting compression info, so skip this part for them
      #ifdef S4STAMP_BUILD
         // AS Dec 1/06 - allow old cb clients to connect

         if ( 0 )
      #else
         if ( conLow->javaClient == 0 )
      #endif
   #endif
   {
      sendPtr = &message->flag ;
      lenToSend = message->messageLen + 3 ;
   }
   #ifdef S4SERVER
      else
      {
         sendPtr = message->message ;
         lenToSend = message->messageLen ;
      }
   #endif

   do
   {
      FD_ZERO(&writefd ) ;
      FD_SET(conLow->sockw, &writefd) ;
      FD_ZERO(&errorfd ) ;
      FD_SET(conLow->sockw, &errorfd) ;

      dummy = select((int)conLow->sockw+1, (fd_set *)0, &writefd, &errorfd, 0 ) ;
      if ( (dummy != 1 ) || (!FD_ISSET((int)conLow->sockw, &writefd )))
      {
         connect4lowError(conLow) ;
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket;
      }

      rc = send(conLow->sockw, sendPtr + sent, lenToSend - sent, 0 ) ;
      #ifdef S4WINSOCK
         if (rc == SOCKET_ERROR )
      #else
         if (rc < 0 )
      #endif
      {
         if ((dummy = WSAGetLastError())==WSAEWOULDBLOCK )
         {
            Sleep(0) ;
            continue ;
         }
         connect4lowError(conLow) ;
         assert5( 0 ) ;  // AS Jun 9/06 - try to break into problem code...
         return e4socket ;
       }
      sent += rc ;
   }
   while (sent < message->messageLen) ;
   #ifdef S4LEN_PRINT
      QueryPerformanceCounter(&ts) ;
      sprintf(buffer, "Sent: %d, on socket %d at time %d", message->messageLen, conLow->sockw, ts ) ;
      debug4display(buffer) ;
   #endif
   return r4success ;
}
#endif /*!S4OFF_THREAD */



#ifndef S4BYTEORDER_3210
   double htond(double value)
   {
      return( x4reverseDouble( &value ) ) ;
   /*
      char returnValue[8] ;
      char *from, *to ;

      to = returnValue ;
      from = ((char *)(&value))+8 ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      return (*(double *)returnValue) ; */ //This is the same as the x4reverseDouble function use it instead.
   }

   /* LY 2003/04/17 : some customers have experienced exception errors related
      to the regular htond() and x4reverseDouble() functions, due to the
      combination of three things:

      1) Windows 98 (Windows 2000 seems unaffected; unknown effect on other OSs)
      2) certain HP printer drivers that directly manipulate the floating-point
         status registers (e.g. Neil Devlin and HP 1100)
      3) htond() and x4reverseDouble() casting the byte-swapped value back to
         a double

      If the byte-swapped value being cast does not represent a valid double,
      the default behavior seems to be to either ignore or handle it, but the
      manipulation of the FP status registers by the printer driver changes this
      behavior, and a floating point exception occurs. */
   void c4htond( void *oldValue, void *newValue )
   {
      char *from, *to ;

      to = (char *)newValue ;
      from = (char *)oldValue + 8 ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
      *to++ = *--from ;
   }
#endif /*!S4BYTEORDER_3210 */
#endif /*!Donotcompilec4conlow*/
