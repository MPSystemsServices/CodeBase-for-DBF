#include "d4all.h"

#ifndef S4STAND_ALONE
#ifndef S4OFF_COMMUNICATIONS

#ifdef S4CLIENT

int connect4reconnect( CONNECT4 *connect, CODE4 *c4 )
{
   /*
      AS 03/08/01 - this function is provided as part of the new connection process whereby the
      client side sets up both connections to the server, which avoids firewall problems.

      The input 'connect' is a connection which already has the first part of its connection
      initialized (via connect4connect)

   */
   #ifdef E4PARM_LOW
      if ( c4 == 0 || connect == 0 )
         return error4( c4, e4parmNull, E96973 ) ;
   #endif
   int rc ;
   CONNECT4LOW *connectLow = connect->connectBuffer.connectLow ;
   if ( connectLow == 0 )
      return error4( c4, e4parm, E96973 ) ;

   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;

   rc = connect4lowReconnect( connectLow, &connect->address, connect->addrLen ) ;
   if ( rc != r4success )
   {
      mem4free( c4->connectLowMemory, connectLow ) ;
      switch (rc)
      {
         case r4errNetwork :
            return error4( c4, e4socket, E88050 ) ;
         case r4hostUnavailable :
            return error4( c4, e4connection, E88065 ) ;
         case r4noServerOnHost :
            return error4( c4, e4connection, E88061 ) ;
         case r4numSockets :
            return error4( c4, e4socket, E88059 ) ;
         case r4connectTimeOut :
            return error4 (c4, e4connection, E88060 ) ;
         case r4noConnect :
            return error4( c4, e4connection, E86820 ) ;
         default:
            return error4(c4, rc, E86820 ) ;
      }
   }
   // rc = connect4init(connect, connectLow, c4) ;
   // if ( rc < 0 )
   // {
   //    connect4lowClose( connectLow ) ;
   //    mem4free( c4->connectLowMemory, connectLow ) ;
   //    return (error4(c4, rc, E96973 )) ;
   // }
   return r4success ;
}



int connect4connect( CONNECT4 *connect, CODE4 *c4, unsigned short portNo, const char *machineName )
{
/* PARAMATERS

   connect is a CONNECT4 to connect and initialize
   machineName is the name of the machine to connect to.  This can be either
     a tcp/ip physical address or a host name which maps to a physical
     tcp/ip address.
   portNo is the port number to connect to

   ERRORS

   If error4code(CODE4) is < 0 this function should just return NULL
   Any failure should cause an error4() to be generated and the function to
     return NULL.  Any initialization which was performed must be undone
     before returning NULL.

   RETURNS

   NULL - a failure occurred.  Check error4code(CODE4) to discover which
     error
   NON-NULL - a valid connected CONNECT4 is returned to the caller

   NOTES

   This function is used to create a new connection between itself and
     another process which is performing a connect4accept().

   This function does the following:

     Create a connect4low
     Mkake a C4ADDRESS that the connect4low... functions can understand
     connect on connectLow to make the write connection
     Call connect4init() to make the read connection and other initialization
*/
   int rc ;
   CONNECT4LOW *connectLow ;

   #ifdef E4PARM_LOW
      if (c4 == NULL)
         return (error4(NULL, e4parmNull, E96973 )) ;
   #endif

   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;

   /* if (c4->connectLowMemory == NULL ) */
   /*   c4->connectLowMemory = mem4create(c4, MEMORY4START_CONNECT_LOW, sizeof(CONNECT4LOW), MEMORY4EXPAND_CONNECT_LOW, 0 ) ; */

   connectLow = (CONNECT4LOW *)mem4allocZero( c4->connectLowMemory ) ;
   if ( connectLow == 0 )
      return error4( c4, e4memory, E91004 ) ;

   rc = address4makeName( &connect->address, &connect->addrLen, portNo, machineName ) ;
   if ( rc != r4success )
   {
      mem4free( c4->connectLowMemory, connectLow ) ;
      switch (rc)
      {
         case r4errNetwork :
            return error4( c4, e4socket, E88050 ) ;
         case r4hostNotFound :
            return error4describe( c4, e4connection, E89004, machineName, 0, 0 ) ;
         case r4hostUnavailable :
            return error4describe( c4, e4connection, E88065, machineName, 0, 0 ) ;
         case r4noConnect :
            return error4describe( c4, e4connection, E86820, machineName, 0, 0 ) ;
         default:
            return error4describe(c4, rc, E86904, machineName, 0, 0 ) ;
      }
   }

   rc = connect4lowConnect(connectLow, &connect->address, connect->addrLen ) ;
   if ( rc != r4success )
   {
      mem4free( c4->connectLowMemory, connectLow ) ;
      switch (rc)
      {
         case r4errNetwork :
            return error4( c4, e4socket, E88050 ) ;
         case r4hostUnavailable :
            return error4describe( c4, e4connection, E88065, machineName, 0, 0 ) ;
         case r4noServerOnHost :
            #ifdef S4UNIX
               const char *portStr ;
               int dec, sign ;
               portStr = fcvt( portNo, 5, &dec, &sign ) ;
            #else
               char portStr[8];
               itoa( portNo, portStr, 10 ) ;
            #endif
            return error4describe( c4, e4connection, E88061, machineName, "with process id", portStr ) ;
         case r4numSockets :
            return error4( c4, e4socket, E88059 ) ;
         case r4connectTimeOut :
            return error4 (c4, e4connection, E88060 ) ;
         case r4noConnect :
            return error4describe( c4, e4connection, E86820, machineName, 0, 0 ) ;
         default:
            return error4(c4, rc, E86820 ) ;
      }
   }
   rc = connect4init(connect, connectLow, c4) ;
   if ( rc < 0 )
   {
      connect4lowClose( connectLow ) ;
      mem4free( c4->connectLowMemory, connectLow ) ;
      return (error4(c4, rc, E96973 )) ;
   }
   return r4success ;
}

#endif /*S4CLIENT */



void connect4disconnect(CONNECT4 *connect )
{
/* ERRORS

   Ignore any errors and continue disconnecting.  Ignore error4code(CODE4).

   NOTES

   This function is used to disconnect a CONNECT4.
   Call connect4bufferDisconnect on all the blast connections.
   Call connect4bufferDisconnect to do the actual processing.
*/

  CONNECT4BUFFER *connectBuffer = NULL;

   #ifdef E4PARM_LOW
      if (connect == NULL)
      {
         error4(NULL, e4parmNull, E96974 ) ;
         return;
      }
   #endif

   while (connectBuffer = (CONNECT4BUFFER *)l4next(&connect->blastList, (LINK4 *)connectBuffer ) )
      connect4bufferDisconnect(connectBuffer) ;

   connect4bufferDisconnect(&connect->connectBuffer) ;
}



int connect4init(CONNECT4 *connect, CONNECT4LOW *connectLow, CODE4 *c4)
{
/* PARAMATERS

   connect is the CONNECT4 to initialize
   connect4low is a write connect connect4low

   ERRORS

   If error4code(CODE4) is < 0 this function should just return error
   Any failure should cause an error4() to be generated and the function to
     return.  Any initialization which was performed must be undone
     before returning NULL.

   NOTES

   Initializes the CONNECT4BUFFER structure:
     Set the variables to their needed valuse
     Call connect4bufferInit() to complete the read socket connection.
*/
   int rc ;

   #ifdef E4PARM_LOW
      if ( (connect == NULL) || (connectLow == NULL) || (c4 == NULL ) )
         return (error4(NULL, e4parmNull, E96975 ) ) ;
   #endif

   if ( error4code(c4) < 0)
      return error4code(c4) ;

   connect->cb = c4 ;
   #ifdef S4SERVER
      connect->workState = CONNECT4IDLE ;
   #endif

   rc = connect4bufferInit(&connect->connectBuffer, connect, connectLow, c4->readMessageNumBuffers, c4->readMessageBufferLen, c4->writeMessageNumOutstanding) ;
   if (rc < 0 )
   {
      connect4initUndo(connect) ;
      return error4(c4, rc, E96975) ;
   }
   /* connect->connectBuffer.connected = 2 ; */
   return r4success ;
}



void connect4initUndo(CONNECT4 *connect)
{
/* ERRORS

   Ignore any errors and continue uninitializing.  Do not set
     error4code(CODE4).  Errors may occur if the other side forcefully
     disconnected.  Ignore error4code(CODE4)

   NOTES

   Disconnects the connection.
     Uninitialize the blast connections.

   Cleans up the CONNECT4 structure.
*/
   CONNECT4BUFFER *connectBuffer = NULL ;
   #ifdef E4PARM_LOW
      if (connect == NULL )
      {
         error4(NULL, e4parmNull, E96976 ) ;
         return ;
      }
   #endif

   while (connectBuffer = (CONNECT4BUFFER *)l4next(&connect->blastList, (LINK4 *)connectBuffer ) )
      connect4bufferInitUndo(connectBuffer) ;

   connect4bufferInitUndo(&connect->connectBuffer ) ;
}



TCP4ADDRESS connect4localTcpAddress( CONNECT4 *connect )
{
   // AS 10/13/99 --> for Simba - if no connection, return an address of '0:0:0:0'
   if ( connect->connectBuffer.connectLow == 0 )
   {
      TCP4ADDRESS addressOut ;
      memset( &addressOut, 0, sizeof( TCP4ADDRESS ) ) ;
      return addressOut ;
   }
   else
      return connect4lowLocalTcpAddress( connect->connectBuffer.connectLow ) ;
}



TCP4ADDRESS connect4peerTcpAddress( CONNECT4 *connect )
{
   return connect4lowPeerTcpAddress( connect->connectBuffer.connectLow ) ;
}



#ifdef S4SERVER
#ifndef S4OFF_THREAD
int connect4workContinue(CONNECT4 *connect )
{
/* RETURNS

   r4finished - means the connection is finished and that there are no
     more messages to service for it (for now).  This means the worker
     thread can go on to service another connection
   r4success - means that there is another message on the connection to
     service, and so the worker thread should service that message.

   ERRORS

   If an error occurs, shut down the CONNECT4, free it up, and return
     r4finished.

   NOTES

   This function is called by a worker thread when it is finished
     processing a message for a connection.

   This function does the following:
     Check CODE4.errorCode.  If it is e4connect, then the connection is
       dead.  In that case, return e4connect.
     Examine current working message.  If workingReadMessage is not NULL,
       then there must be more data to work on, so return r4success.

     Call conect4threadWorkContinue( CONNECT4THREAD ):
       if result is not r4finished, return the result.
       if result is r4finished, return r4finished.
*/
   int rc, saveRc = r4success ;
   CONNECT4THREAD *connectThread ;

   #ifdef E4PARM_LOW
      if (connect == NULL )
         return (error4(NULL, e4parmNull, E96977 )) ;
   #endif

   if ( connect->cb != NULL )  // can happen when a connection is disallowed (i.e. not connected yet)
      if (error4code(connect->cb) < 0 )
         return error4code(connect->cb) ;

   if (connect->connectBuffer.workingReadMessage != NULL )
      return r4success;

  /* return connect4threadWorkContinue(&connect->connectBuffer.connectThread) ; */
   connectThread = &connect->connectBuffer.connectThread ;

   list4mutexWait(&connectThread->readMessageCompletedList) ;
   #ifdef S4SERVER
      if ( connect->workState == CONNECT4SHUTDOWN )  /* stop servicing the client */
      {
         list4mutexRelease(&connectThread->readMessageCompletedList) ;
         return r4finished ;
      }
   #endif
   rc = l4numNodes( &connectThread->readMessageCompletedList.list ) ;
   if (rc < 0)
      saveRc = rc ;
   else
      if (rc==0)
      {
         saveRc = r4finished ;
         #ifdef S4SERVER
            connect->workState = CONNECT4IDLE ;
         #endif
      }
   list4mutexRelease(&connectThread->readMessageCompletedList) ;
   return saveRc ;

}
#endif /* !S4OFF_THREAD */
#endif /*S4SERVER */
#endif /* not S4OFF_COMMUNICATIONS */
#endif /*!S4STAND_ALONE */
