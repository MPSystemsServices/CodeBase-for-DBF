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
      CONNECT4BUFFER *connectBuffer = &(connect->connectBuffer) ;
      connect4lowEnterExclusive( connectBuffer ) ;
      CONNECT4LOW *connectLow = connect4bufferLowGet( connectBuffer ) ;
      if ( connectLow == 0 )
      {
         connect4lowExitExclusive( connectBuffer ) ;
         return error4( c4, e4parm, E96973 ) ;
      }

      if ( error4code( c4 ) < 0 )
      {
         connect4lowExitExclusive( connectBuffer ) ;
         return error4code( c4 ) ;
      }

      #ifdef S4MAC_TCP
         rc = connect4lowReconnect( connectLow, connect->address, connect->addrLen ) ;
      #else
         rc = connect4lowReconnect( connectLow, &connect->address, connect->addrLen ) ;
      #endif
      if ( rc != r4success )
      {
         mem4free( c4->connectLowMemory, connectLow ) ;
         connect4bufferLowSet( connectBuffer, 0 ) ;
         connect4lowExitExclusive( connectBuffer ) ;
         switch ( rc )
         {
            case r4errNetwork:
               return error4( c4, e4socket, E88050 ) ;
            case r4hostUnavailable:
               return error4( c4, e4connection, E88065 ) ;
            case r4noServerOnHost:
               // AS Sept. 25/02 - sometimes this error is returned even when a server is available,
               // so retry instead.
               return r4connectTimeOut ;
               // return error4( c4, e4connection, E88061 ) ;
            case r4numSockets:
               return error4( c4, e4socket, E88059 ) ;
            case r4connectTimeOut:  // AS Aug 29/01 - Occasionally server times out, return that in that case
               return r4connectTimeOut ;
               // return error4 (c4, e4connection, E88060 ) ;
            case r4noConnect:
               return error4( c4, e4connection, E86820 ) ;
            // AS Jun 9/06 - e4socket somestimes returned when server is timing out...Windows Server 2003 problem
            case e4socket:
               return r4connectTimeOut ;
            default:
               return error4(c4, rc, E86820 ) ;
         }
      }

      connect4lowExitExclusive( connectBuffer ) ;
      return r4success ;
   }



   /* S4CLIENT */
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
      */
      int rc ;
      CONNECT4LOW *connectLow ;

      #ifdef E4PARM_LOW
         if (c4 == NULL)
            return (error4(NULL, e4parmNull, E96973 )) ;
      #endif

      // AS Aug 29/01 -- we want to retry the connection again from scratch up to a fixed
      // number of times in case the server is just temporarily overburdened, and also
      // occasionally fails on reconnect due to a sequencing problem.
      // AS Jan 9/02 - implemented code to perform the actual timeout based on a CODE4.acceptTimeOut setting
      int maxRetry = CONNECT4MAX_TRY ;
      #if defined( S4WIN32 ) && !defined( S4OFF_THREAD ) && defined( S4CLIENT )
         if ( c4->acceptTimeOut <= 0 )  // don;t actually wait longer than 2 hours no matter what the setting...
            maxRetry = 3600 ;
         else
         {
            maxRetry = c4->acceptTimeOut / CON4LOW_ACCEPT_INTERVAL ;
            // always do at least one retry on the connection no matter what the settings are...
            if ( maxRetry < 2 )
               maxRetry = 2 ;
         }
      #endif


      for ( int tryConnectIndex = 0 ; tryConnectIndex < maxRetry ; tryConnectIndex++ )
      {
         if ( error4code( c4 ) < 0 )
            return error4code( c4 ) ;

         connectLow = (CONNECT4LOW *)mem4allocZero( c4->connectLowMemory ) ;
         if ( connectLow == 0 )
            return error4( c4, e4memory, E91004 ) ;

         #ifdef S4MAC_TCP
            rc = address4makeNameMac( &(connect->address), portNo, machineName ) ;
         #else
            rc = address4makeName( &connect->address, &connect->addrLen, portNo, machineName ) ;
         #endif
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

         CONNECT4BUFFER *connectBuffer = &(connect->connectBuffer) ;
         // connect4lowEnterExclusive( connectBuffer ) ; - connectBuffer not initialized yet
         // AS Jan 30/03 - Need the CODE4 for encryption dll handling
         #ifdef S4MAC_TCP
            rc = connect4lowConnect( connectLow, connect->address, connect->addrLen, c4 ) ;
         #else
            rc = connect4lowConnect( connectLow, &connect->address, connect->addrLen, c4 ) ;
         #endif
         if ( rc != r4success )
         {
            // AS Jan 9/02 - In some cases this memory was already freed, resulting in double free
            // only free it if connectBuffer->connectLow != 0 already (since it gets set right
            // away this should never otherwise occur)
            // AS Jul 29/02 - cannot call low get because don't have exclusive...
            // AS Sept. 18/02 server only, otherwise can always free
            #ifdef S4SERVER
               if ( connectBuffer->connectLowPtr != 0 )
               {
            #endif
                  mem4free( c4->connectLowMemory, connectLow ) ;
            #ifdef S4SERVER
                  connect4bufferLowSet( connectBuffer, 0 ) ;
               }
            #endif
            // connect4lowExitExclusive( connectBuffer ) ; - connectBuffer not initialized yet
            switch (rc)
            {
               case r4errNetwork :
                  return error4( c4, e4socket, E88050 ) ;
               case r4hostUnavailable :
                  return error4describe( c4, e4connection, E88065, machineName, 0, 0 ) ;
               case r4noServerOnHost :
                  rc = r4connectTimeOut ;
                  break ;
                  // AS Sept. 25/02 - sometimes this error is returned even when a server is available,
                  // so retry instead.
                  /*
                  #if defined(S4UNIX ) || defined(S4MACINTOSH)
                     const char *portStr ;
                     int dec, sign ;
                     #ifdef S4NO_FCVT
                        portStr = f4fcvt( portNo, 5, &dec, &sign ) ;
                     #else
                        portStr = fcvt( portNo, 5, &dec, &sign ) ;
                     #endif
                  #else
                     char portStr[8];
                     itoa( portNo, portStr, 10 ) ;
                  #endif
                  return error4describe( c4, e4connection, E88061, machineName, "with process id", portStr ) ;
                  */
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

         if ( rc == r4connectTimeOut )  // try connecting again in this case
         {
            // log5( "CONNECTION NOTE: RETRYING FAILED RECONNECTION\r\n" );
            u4delayHundredth( 10 ) ;  // Jun 9/06 wait a short period of time...in case server overburdened
            continue ;
         }

         rc = connect4init( connect, &connectLow, c4 ) ;
         if ( rc == 0 )
         {
            #ifndef S4OFF_LOG
               if ( c4->logConn > 1 && rc == 0 )
               {
                  char buf[250] ;
                  sprintf( buf, "IC0070:connect4connect() succeeded in creating a connection to the server" ) ;
                  assert5( strlen( buf ) < sizeof( buf ) ) ;
                  code4logInfo( c4, buf ) ;
               }
            #endif
            // connect4lowExitExclusive( connectBuffer ) ; - connectBuffer not initialized yet
            return rc ;
         }
         if ( rc != 0 )
         {
            connect4lowEnterExclusive( connectBuffer ) ;
            connect4lowClose( connectLow ) ;
            // AS Jan 9/02 - In some cases this memory was already freed, resulting in double free
            // only free it if connectBuffer->connectLow != 0 already (since it gets set right
            // away this should never otherwise occur)
            if ( connect4bufferLowGet( connectBuffer ) != 0 )
            {
               mem4free( c4->connectLowMemory, connectLow ) ;
               connect4bufferLowSet( connectBuffer, 0 ) ;
            }
            connectLow = 0 ;
            connect4lowExitExclusive( connectBuffer ) ;
         }
         if ( rc == r4connectTimeOut )  // try connecting again in this case
         {
            // log5( "CONNECTION NOTE: RETRYING FAILED RECONNECTION\r\n" );
            u4delayHundredth( 10 ) ;  // Jun 9/06 wait a short period of time...in case server overburdened
            #ifndef S4OFF_LOG
               if ( c4->logConn > 1 )
               {
                  char buf[250] ;
                  sprintf( buf, "IC0071:connect4connect() connection attempt time-out, retrying..." ) ;
                  assert5( strlen( buf ) < sizeof( buf ) ) ;
                  code4logInfo( c4, buf ) ;
               }
            #endif
            continue ;
         }

         #ifdef E4ANALYZE
            int i = 0 ;
            int j = 4 / i ;
         #endif
         return error4( c4, rc, E96973 ) ;
      }
      #ifndef S4OFF_LOG
         if ( c4->logConn > 1 && rc == 0 )
         {
            char buf[250] ;
            sprintf( buf, "IC0072:connect4connect() succeeded in creating a connection to the server" ) ;
            assert5( strlen( buf ) < sizeof( buf ) ) ;
            code4logInfo( c4, buf ) ;
         }
      #endif
         /*
      if ( rc != 0 )
      {
         #ifdef E4ANALYZE
            int i = 0 ;
            int j = 4 / i ;
         #endif
      }
      */
      return rc ;
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

   // AS July 26/02 - was not working...
   #ifndef S4OFF_BLAST
      while ((connectBuffer = (CONNECT4BUFFER *)l4next(&connect->blastList, (LINK4 *)connectBuffer )) != NULL )
         connect4bufferDisconnect(connectBuffer) ;
   #endif

   connect4bufferDisconnect(&connect->connectBuffer) ;
}



// AS Sept 26/02 - had a dual free being called.
int connect4init(CONNECT4 *connect, CONNECT4LOW **connectLow, CODE4 *c4)
{
   /* PARAMATERS

      connect is the CONNECT4 to initialize
      connect4low is a pointer to a write connect connect4low pointer.  It will be set to null if we
         free it up as part of a failed connection

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
      if ( (connect == NULL) || (connectLow == NULL) || ((*connectLow) == NULL) || (c4 == NULL ) )
         return (error4(NULL, e4parmNull, E96975 ) ) ;
   #endif

   #ifdef S4SERVER
      assert5( connect->link.n == 0 && connect->link.p == 0 ) ;
      assert5( connect->workState == 0 ) ;
   #endif

   if ( error4code( c4 ) < 0 )
      return error4code( c4 ) ;

   connect->cb = c4 ;

   rc = connect4bufferInit( &connect->connectBuffer, connect, connectLow, c4->readMessageNumBuffers, c4->readMessageBufferLen, c4->writeMessageNumOutstanding) ;
   if ( rc != 0 )
   {
      #ifndef S4OFF_LOG
         if ( c4->logConn > 0 )
         {
            char buf[250] ;
            sprintf( buf, "EC0080:connect4bufferInit() failed for client" ) ;
            assert5( strlen( buf ) < sizeof( buf ) ) ;
            code4logInfo( c4, buf ) ;
         }
      #endif
      // ensure work state is shutdown since not initialized and required for initUndo (debug)
      #ifdef S4SERVER
         connect->workState = CONNECT4SHUTDOWN ;
      #endif
      connect4initUndo( connect ) ;
      return rc ;
   }

   // AS July 24/02 - don't put into idle state until after initialization is complete
   #ifdef S4SERVER
      assert5( connect->workState == 0 ) ;
      assert5( connect->link.n == 0 && connect->link.p == 0 ) ;
      // #ifdef S4TESTING
      //    u4writeErr( "setting state to IDLE", 1 ) ;
      // #endif
      connect->workState = CONNECT4IDLE ;
   #endif
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
   #ifdef E4PARM_LOW
      if (connect == NULL )
      {
         error4(NULL, e4parmNull, E96976 ) ;
         return ;
      }
   #endif

   CONNECT4BUFFER *connectBuffer = NULL ;
   // AS July 24/02 - was not considering case where the connect is still on the list of to-be-serviced connects
   #ifdef S4SERVER
      assert5( connect->workState == CONNECT4SHUTDOWN ) ;
      if ( connect->link.n != 0 )
      {
         list4mutexWait( &connect->cb->connectsToService ) ;
         if ( connect->link.n != 0 )  // if still on the list, remove it
            l4remove( &(connect->cb->connectsToService.list), &(connect->link) ) ;
         list4mutexRelease( &connect->cb->connectsToService ) ;
      }
   #endif
   #ifdef E4LINK
      assert5( connect->link.n == 0 && connect->link.p == 0 ) ;
   #endif

   // AS July 26/02 - was not working...
   #ifndef S4OFF_BLAST
      while ((connectBuffer = (CONNECT4BUFFER *)l4next(&connect->blastList, (LINK4 *)connectBuffer )) != NULL )
         connect4bufferInitUndo(connectBuffer) ;
   #endif

   connect4bufferInitUndo(&connect->connectBuffer ) ;
}



TCP4ADDRESS connect4localTcpAddress( CONNECT4 *connect )
{
   // AS 10/13/99 --> for Simba - if no connection, return an address of '0:0:0:0'
   CONNECT4BUFFER *connectBuffer = &(connect->connectBuffer) ;
   connect4lowEnterExclusive( connectBuffer ) ;
   TCP4ADDRESS addressOut ;
   if ( connect4bufferLowGet( connectBuffer ) == 0 )
      memset( &addressOut, 0, sizeof( TCP4ADDRESS ) ) ;
   else
      addressOut = connect4lowLocalTcpAddress( connect4bufferLowGet( connectBuffer ) ) ;

   connect4lowExitExclusive( connectBuffer ) ;
   return addressOut ;
}



TCP4ADDRESS connect4peerTcpAddress( CONNECT4 *connect )
{
   CONNECT4BUFFER *connectBuffer = &(connect->connectBuffer) ;
   connect4lowEnterExclusive( connectBuffer ) ;
   TCP4ADDRESS addressOut = connect4lowPeerTcpAddress( connect4bufferLowGet( connectBuffer ) ) ;
   connect4lowExitExclusive( connectBuffer ) ;
   return addressOut ;
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

   assert5( connect->workState == CONNECT4WORKING || connect->workState == CONNECT4SHUTDOWN ) ;

   if ( connect->cb != NULL )  // can happen when a connection is disallowed (i.e. not connected yet)
      if (error4code(connect->cb) < 0 )
         return error4code(connect->cb) ;

   if (connect->connectBuffer.workingReadMessage != NULL )
      return r4success;

   /* return connect4threadWorkContinue(&connect->connectBuffer.connectThread) ; */
   connectThread = &connect->connectBuffer.connectThread ;

   // AS July 24/02 - The list may not be initialized (either shutdown or full
   // connect not completed).  In that case, say we are r4finished.
   if ( connectThread->readMessageCompletedList.isValid == 0 )
      return r4finished ;

   list4mutexWait( &connectThread->readMessageCompletedList ) ;
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
      if ( rc == 0 )
      {
         saveRc = r4finished ;
         #ifdef S4SERVER
            // the connect must not be on the to-service list at this point because it is currently being
            // serviced
            #ifdef E4LINK
               assert5( connect->link.n == 0 && connect->link.p == 0 ) ;
            #endif
            // if the state is not working, don't change to idle.  It will get changed to idle if required
            // in other stages.
            if ( connect->workState == CONNECT4WORKING )
            {
               connect->workState = CONNECT4IDLE ;
               // #ifdef S4TESTING
               //    u4writeErr( "setting state to IDLE", 1 ) ;
               // #endif
            }
         #endif
      }
   list4mutexRelease(&connectThread->readMessageCompletedList) ;
   return saveRc ;

}
#endif /* !S4OFF_THREAD */
#endif /*S4SERVER */
#endif /* not S4OFF_COMMUNICATIONS */
#endif /*!S4STAND_ALONE */
