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
#ifndef S4OFF_THREAD
/* CONNECT4THREAD structure is required per connection if a communications
   thread is being used
*/

#ifdef S4SERVER
   void connect4threadRegisterShutdown( CONNECT4THREAD *connectThread )
   {
      /*
         DESCRIPTION

         Change the workState to CONNECT4SHUTDOWN.  This has the effect of
           new incoming commands being ignored by the server.
      */

      if ( connectThread->readMessageCompletedList.isValid )  /* if it is not valid, maybe - not initialized */
         list4mutexWait( &connectThread->readMessageCompletedList ) ;
      if ( connectThread->connectBuffer != 0 )
      {
         // AS Sep 3/03 - Improve sequencing, don't allow state change shile retrieving the message.
         list4mutexWait( &connectThread->cb->connectsToService ) ;

         long lpCount = 10 ;
         while ( connectThread->connectBuffer->connect->workState == CONNECT4RETRIEVING )
         {
            list4mutexRelease( &connectThread->cb->connectsToService ) ;
            Sleep( 1 ) ;
            list4mutexWait( &connectThread->cb->connectsToService ) ;
            // AS Sep 4/03 - count on a loop before just ignoring...
            if ( lpCount-- == 0 )
               break ;
         }

         connectThread->connectBuffer->connect->workState = CONNECT4SHUTDOWN ;
         list4mutexRelease( &connectThread->cb->connectsToService ) ;
      }
      if ( connectThread->readMessageCompletedList.isValid )
         list4mutexRelease( &connectThread->readMessageCompletedList ) ;
   }
#endif



void connect4threadCompleteRequest( CONNECT4THREAD *connectThread )
{
/* ERRORS

   In case of error, this function ignores the error and continues on.  It
     does not set error4code( CODE4 ).  This is because the connection may already
     be invalid at this point, and thus expected connection errors may occur
     which we just want to ignore.

   NOTES

   This function is called by the application/server in order to signal to the
     communications thread that a connection no longer needs servicing.

   After making the request, it waits for the request to be completed by
     waiting on the CONNECT4THREAD.completedSemaphore semaphore

   This routine does the following:
     inter4completeRequested( connectThread->inter, connectThread )
     WaitForSingleObject( connectThread->completedSemaphore )
*/

   #ifdef E4PARM_LOW
      if ( connectThread == 0 )
      {
         error4( 0, e4parmNull, E96945 ) ;
         return ;
      }
   #endif

   #ifdef S4SERVER
      connect4threadRegisterShutdown( connectThread ) ;
      if ( connectThread->link.n )
         list4mutexRemoveLink( &connectThread->cb->connectsToService, &(connectThread->connectBuffer->connect->link) ) ;
   #endif
   inter4completeRequested( connectThread->inter, connectThread ) ;
   semaphore4wait( &connectThread->completedSemaphore, WAIT4EVER ) ;
}



int connect4threadInit( CONNECT4THREAD *connectThread, CODE4 *c4, CONNECT4BUFFER *connectBuffer )
{
/* PARAMATERS

   connectThread is the CONNECT4THREAD to initialize
   connect is a possibly 0 input CONNECT4.  If null, it means that this
     connection is a special connection and does not need to have the
     communications thread assign a worker thread to it when data comes in.
   connectLow is a required pointer to a valid low-level connection
   startWorkState is used to specify a work state for the connect.  If the
     connect is special-service ( eg. blasting ), this should
     be set to CONNECT4SPECIAL.  Otherwise it should be set to 0.

   ERRORS

   If failure, call error4() to set CodeBase error code, and return error
      back to caller.  De-initialize anything which was initialized in case
     of error before returning to caller.

   RETURNS

   r4success
   < 0

   NOTES

   Initializes the CONNECT4THREAD structure
*/

   int rc ;

   #ifdef E4PARM_LOW
      if ( ( connectThread == 0 ) || ( connectBuffer == 0 ) || ( c4==0 ) )
         return error4( 0, e4parmNull, E96925 ) ;
   #endif

   if ( error4code( c4 ) < 0 )
     return e4codeBase ; ;

   connectThread->cb = c4 ;
   connectThread->inter = c4->inter ;
   connectThread->connectBuffer = connectBuffer ;

   rc = semaphore4init( &connectThread->readMessageCompletedListSemaphore ) ;
   if ( rc )
   {
      #ifndef S4OFF_LOG
         if ( c4->logConn > 0 )
            code4logInfo( c4, "EA0001:Failure to initialize semaphore: connectThread->readMessageCompletedListSemaphore" ) ;
      #endif
      return error4( c4, rc, E96925 ) ;
   }
   rc = semaphore4init( &connectThread->writeMessageCompletedListSemaphore ) ;
   if ( rc )
   {
      semaphore4initUndo( &connectThread->readMessageCompletedListSemaphore ) ;
      #ifndef S4OFF_LOG
         if ( c4->logConn > 0 )
            code4logInfo( c4, "EA0002:Failure to initialize semaphore: connectThread->readMessageCompletedListSemaphore" ) ;
      #endif
      return error4( c4, rc, E96925 ) ;
   }
   rc = semaphore4init( &connectThread->completedSemaphore ) ;
   if ( rc )
   {
      semaphore4initUndo( &connectThread->readMessageCompletedListSemaphore ) ;
      semaphore4initUndo( &connectThread->writeMessageCompletedListSemaphore ) ;
      #ifndef S4OFF_LOG
         if ( c4->logConn > 0 )
            code4logInfo( c4, "EA0003:Failure to initialize semaphore: connectThread->completedSemaphore" ) ;
      #endif
      return error4( c4, rc, E96925 ) ;
   }
   list4mutexInit( &connectThread->readMessageCompletedList ) ;
   list4mutexInit( &connectThread->writeMessageCompletedList ) ;
   return r4success ;
}



void connect4threadInitUndo( CONNECT4THREAD *connectThread )
{
/* RETURNS

   No return code.  Ignore any failure when de-initializing structures.
     Does not set error4code( CODE4 ).  errors are irrelevant since we are
     shutting down a connection, and it may produce errors simply because
     it has already been forcefully disconnected.

   NOTES

   Cleans up the CONNECT4THREAD structure.

   This function does the following
     list4mutexInitUndo( connectThread->readMessageCompletedList ) ;
     semaphore4initUndo( connectThread->readMessageCompletedListSemaphore ) ;
     list4mutexInitUndo( connectThread->writingSignalsList ) ;
     list4mutexInitUndo( connectThread->writeMessageCompletedList ) ;
     semaphore4initUndo( connectThread->writeCompletedMemoryListSemaphore ) ;
     semaphore4initUndo( connectThread->completedSemaphore ) ;
*/

   /* ignore any failure when de-initializing */

   /* AS 08/02/99 - was releasing this sometimes with messages still on list... */

   for( ;; )
   {
      // just need to remove from list, actual freeing is from a group array, and is done elsewhere...
      NET4MESSAGE *message = (NET4MESSAGE *)l4first( &connectThread->readMessageCompletedList.list ) ;
      if ( message == 0 )
         break ;
      // AS 11/09/00 ensure nobody else messing with list...
      list4mutexRemoveLink( &connectThread->readMessageCompletedList, (LINK4 *)message ) ;
      // AS May 16/02 - the message must be freed if it was independently allocated, do that now
      if ( message->independentAllocation == 1 )
      {
         u4free( message ) ;
         return ;
      }
   }

   list4mutexInitUndo( &connectThread->readMessageCompletedList ) ;

   /* AS 08/02/99 - was releasing this sometimes with messages still on list... */
   // AS Aug 27/02 - was erroring out if not initialized yet
   if ( connectThread->writeMessageCompletedList.isValid == 1 )
   {
      if ( list4mutexWait( &(connectThread->writeMessageCompletedList) ) == 0 )
      {
         for( ;; )
         {
            NET4MESSAGE *message = (NET4MESSAGE *)l4first( &connectThread->writeMessageCompletedList.list ) ;
            if ( message == 0 )
               break ;

            // AS 11/09/00 ensure nobody else messing with list...
            list4mutexRemoveLink( &connectThread->writeMessageCompletedList, (LINK4 *)message ) ;
            mem4free( connectThread->cb->writeMemory, message ) ;
         }
         list4mutexRelease( &(connectThread->writeMessageCompletedList) ) ;
      }
   }

   list4mutexInitUndo( &connectThread->writeMessageCompletedList ) ;
   semaphore4initUndo( &connectThread->readMessageCompletedListSemaphore ) ;
   semaphore4initUndo( &connectThread->writeMessageCompletedListSemaphore ) ;
   semaphore4initUndo( &connectThread->completedSemaphore ) ;
}



// AS Jan 28/04 - improve the connection error handling - check the status of a connection when 0 length message received
static void connect4BufferVerifyConnection( CONNECT4BUFFER *buffer, NET4MESSAGE *message )
{
   // AS Nov 27/02 - It is also possible, at least with XP clients, to get more empty
   // packets even after connection is complete.  In that case, do a shutdown after 500
   // failures
   // AS Jan 23/04 - Let's do a select analysis on the low level socket in this case to
   // determine its status
   struct timeval timest ;
   memset(&timest, 0, sizeof(timest) ) ;
   timest.tv_sec = 1 ;
   fd_set errorfd ;
   FD_ZERO(&errorfd ) ;
   CONNECT4LOW *conLow = connect4bufferLowGet( buffer ) ;
   if ( conLow == 0 )
   {
      message->messageLen = -1 ;
      buffer->connectionFailed = 1 ;
   }
   else
   {
      int status = select( conLow->sockr, (fd_set *)0, (fd_set *)0, &errorfd, &timest ) ;
      if ( status < 0 )
      {
         message->messageLen = -1 ;
         // also make an internal note that the connection is invalid, for suspended relate operations
         buffer->connectionFailed = 1 ;
      }
      else
      {
         if ( buffer->numFails > 500 )  // happens if client application terminates abnormally
         {
            message->messageLen = -1 ;
            buffer->connectionFailed = 1 ;
         }
      }
   }
}



void connect4threadReadCompleted( CONNECT4THREAD *connectThread, SIGNAL4ROUTINE *routine,  NET4MESSAGE *message )
{
/* ERRORS

   In case of error, set message.messageLen to -1.  This will indicate to
     the higher-level routines that a failure occurred.

   NOTES

   Called by the communications thread to indicate to the CONNECT4THREAD that
     a read has been completed.

   This function does the following:
     routine is removed from connectThread->readSignalsList ( call
       connect4threadReadSignalRetrieve() )
     message.messageLen is set by querying NET4MESSAGE.overlap; Use GetOverlappedResult

     MOVED THE NEXT PART TO CONNECT4THREADREADSTORE()

   Return


   PSEUDOCODE

   l4remove( connectThread->readsSignalsList, routine ) ;
   message.messageLen = message.overlap.internalHigh ;
   list4mutexAdd( connectThread->readMessageCompletedList, message ) ;
   semaphore4release( connectThread->readMessageCompletedListSemaphore ) ;

   etc.
*/
   #ifdef E4PARM_LOW
      if ( ( connectThread == 0 ) || ( message == 0 ) || ( routine == 0 ) )
      {
         error4( 0, e4parmNull, E96946 ) ;
         return ;
      }
   #endif

   SIGNAL4ROUTINE *sig = connect4threadReadSignalRetrieve( connectThread, routine, 1 ) ;
   if ( sig == 0 )
      message->messageLen = -1 ;

   #ifdef E4ANALYZE
      assert5( message->inUse == 1 ) ;    // should not be in use now
   #endif

   if ( message->messageLen != -1 )
   {
      CONNECT4BUFFER *connectBuffer = connectThread->connectBuffer ;
      connect4lowEnterExclusive( connectBuffer ) ;

      /* AS 08/27/99 --> possible to get gpf here on uninitialization... */
      if ( connect4bufferLowGet( connectBuffer ) == 0 || connect4bufferLowGet( connectBuffer )->sockr == 0 )
      {
         message->messageLen = -1 ;
         #ifndef S4OFF_LOG
            if ( connectThread->cb->logConn > 1 )  // means we go here during a normal disconnect operation
               code4logInfo( connectThread->cb, "IA0001:Connection on ReadCompleted is no longer valid due to uninitialization" ) ;
         #endif
      }
      else
      {
         int rc = GetOverlappedResult( (HANDLE)connect4bufferLowGet( connectBuffer )->sockr, &message->overlap,
                                       ( unsigned long * )&message->messageLen, TRUE ) ;
         if ( rc == FALSE || message->messageLen < 0 )
         {
            // return of 0 or FALSE means connection failure
            #ifdef S4SERVER
               if ( connectBuffer->connect->workState != CONNECT4SHUTDOWN ) // means we are shutting down, so this isn't an error
            #endif
               {
                  // AS Sept 18/02 - track the actual error reason as well
                  if ( rc == FALSE )
                  {
                     DWORD winErr = GetLastError() ;
                     if ( winErr != 0 )
                        connectBuffer->windowsErr = winErr ;

                     if ( connectBuffer->connectLowPtr->sockw == 0 )
                     {
                        #ifndef S4OFF_LOG
                           if ( connectThread->cb->logConn > 1 )
                           {
                              char buf[250] ;
                              sprintf( buf, "WA0020:GetOverlappedResult received an error when the sockw was not connected, disconnecting the connection" ) ;
                              assert5( strlen( buf ) < sizeof( buf ) ) ;
                              code4logInfo( connectThread->cb, buf ) ;
                           }
                        #endif
                        message->messageLen = -1 ;
                     }
                     else
                     {
                        // if the windows error is 64, try relaxing the constraint to halt...
                        if ( connectBuffer->numFails > 100 || winErr != 64 )
                        {
                           #ifndef S4OFF_LOG
                              if ( connectThread->cb->logConn > 0 )
                              {
                                 char buf[250] ;
                                 sprintf( buf, "EA0021:GetOverlappedResult result causing connection to be terminated where numFails is %ld, and winErr is %ld",
                                         (long)connectBuffer->numFails, (long)winErr ) ;
                                 assert5( strlen( buf ) < sizeof( buf ) ) ;
                                 code4logInfo( connectThread->cb, buf ) ;
                              }
                           #endif
                           message->messageLen = -1 ;
                        }
                        else
                        {
                           // AS Jan 28/04 - improve the connection error handling
                           connect4BufferVerifyConnection( connectBuffer, message ) ;
                           #ifndef S4OFF_LOG
                              if ( connectThread->cb->logConn > 7 )
                              {
                                 char buf[250] ;
                                 sprintf( buf, "WA0022:GetOverlappedResult unexpected result where numFails is %ld, and winErr is %ld",
                                         (long)connectBuffer->numFails, (long)winErr ) ;
                                 assert5( strlen( buf ) < sizeof( buf ) ) ;
                                 code4logInfo( connectThread->cb, buf ) ;
                              }
                           #endif
                           connectBuffer->numFails++ ;
                           connectBuffer->windowsErr = 0 ;
                        }
                     }
                  }
                  else  // rc was not FALSE - means messageLen already < 0
                  {
                     #ifndef S4OFF_LOG
                        if ( connectThread->cb->logConn > 0 )
                        {
                           char buf[250] ;
                           sprintf( buf, "EA0030:GetOverlappedResult received a bad message, messageLen was set to %ld",
                                   (long)message->messageLen ) ;
                           assert5( strlen( buf ) < sizeof( buf ) ) ;
                           code4logInfo( connectThread->cb, buf ) ;
                        }
                     #endif
                     message->messageLen = -1 ;
                  }
               }
         }
         else if ( message->messageLen == 0 )
         {
            // There is a problem here.  If GetOverlappedResult returns TRUE but messageLen == 0 (should never
            // happen according to Microsoft API, but does anyway), the connection may be valid (should ignore
            // the message) or it may be invalid (means terminate connection).  How do we determine the actual
            // status???
            #ifdef S4SERVER
               if ( connectBuffer->connect->workState != CONNECT4SHUTDOWN ) // means we are shutting down, so this isn't an error
            #endif
               {
                  // AS Sept. 19/02 - There are 2 possibilities here:  1.  The connection is valid and Windows has just
                  // returned an empty message.  This occurs periodically on some systems (possibly related to network errors
                  // or warnings).  2.  The connection is not completely set up and we have a time-out
                  // problem.  In the 2nd case we need to terminate the connection otherwise the connection procedure gets
                  // stuck for some unknown reason (and we cannot ever complete the full connection).  The way to determine
                  // our actual status is to check to see if sockw is 0.  If it is we are in the 2nd possibility and we
                  // will mark the connection for removal, otherwise we allow it to continue on.
                  // also fail if numRequests is low and number of failures in a row has exceeded 10
                  if ( connectBuffer->connectLowPtr->sockw == 0 )
                  {
                     #ifndef S4OFF_LOG
                        if ( connectThread->cb->logConn > 1 )  // happens when the client side time-outs waiting for the server on connect - if server is otherwise busy
                        {
                           char buf[250] ;
                           sprintf( buf, "WA0040:GetOverlappedResult received an empty packet when the sockw was not connected, disconnecting the connection" ) ;
                           assert5( strlen( buf ) < sizeof( buf ) ) ;
                           code4logInfo( connectThread->cb, buf ) ;
                        }
                     #endif
                     message->messageLen = -1 ;
                  }
                  else
                  {
                     #ifdef S4SERVER
                        // in client, let it retry forever...
                        if ( connectBuffer->connect->client->info.numRequests < 10 )
                        {
                           if ( connectBuffer->numFails > 100 )  // happens if client application terminates abnormally
                           {
                              #ifndef S4OFF_LOG
                                 if ( connectThread->cb->logConn > 1 )
                                 {
                                    char buf[250] ;
                                    strcpy( buf, "WA0041:[possibly client abnormal termination]Server GetOverlappedResult received an empty packet for an early connection ( <10 requests) and" ) ;
                                    sprintf( buf+strlen( buf), " erroring due to exceeding maximum fails of: %ld", (long)(connectBuffer->numFails-1) ) ;
                                    assert5( strlen( buf ) < sizeof( buf ) ) ;
                                    code4logInfo( connectThread->cb, buf ) ;
                                 }
                              #endif
                              message->messageLen = -1 ;
                           }
                           else
                           {
                              connectBuffer->numFails++ ;
                              // AS Jan 28/04 - improve the connection error handling
                              connect4BufferVerifyConnection( connectBuffer, message ) ;
                              #ifndef S4OFF_LOG
                                 if ( connectThread->cb->logConn > 7 )
                                 {
                                    char buf[250] ;
                                    sprintf( buf, "WA0042:Server GetOverlappedResult received an empty packet for an early connection ( <10 requests), and the number of consecutive fails is: %ld", (long)(connectBuffer->numFails) ) ;
                                    assert5( strlen( buf ) < sizeof( buf ) ) ;
                                    code4logInfo( connectThread->cb, buf ) ;
                                 }
                              #endif
                           }
                        }
                        else
                        {
                           connectBuffer->numFails++ ;
                           // AS Jan 28/04 - improve the connection error handling
                           connect4BufferVerifyConnection( connectBuffer, message ) ;
                           // AS Nov 27/02 - It is also possible, at least with XP clients, to get more empty
                           // packets even after connection is complete.  In that case, do a shutdown after 500
                           // failures
                           if ( connectBuffer->numFails > 500 )  // happens if client application terminates abnormally
                           {
                              #ifndef S4OFF_LOG
                                 if ( connectThread->cb->logConn > 1 )
                                 {
                                    char buf[250] ;
                                    strcpy( buf, "WA0041:[possibly client abnormal termination]Server GetOverlappedResult received an empty packet for an existing connection ( >10 requests) and" ) ;
                                    sprintf( buf+strlen( buf), " erroring due to exceeding maximum fails of: %ld", (long)(connectBuffer->numFails-1) ) ;
                                    assert5( strlen( buf ) < sizeof( buf ) ) ;
                                    code4logInfo( connectThread->cb, buf ) ;
                                 }
                              #endif
                              message->messageLen = -1 ;
                           }
                           else
                           {
                              #ifndef S4OFF_LOG
                                 if ( connectThread->cb->logConn > 7 )
                                 {
                                    char buf[250] ;
                                    sprintf( buf, "WA0043:Server GetOverlappedResult received an empty packet for an existing connection, so disregarding it, number of consecutive fails is %ld", (long)(connectBuffer->numFails) ) ;
                                    assert5( strlen( buf ) < sizeof( buf ) ) ;
                                    code4logInfo( connectThread->cb, buf ) ;
                                 }
                              #endif
                           }
                        }
                     #else
                        #ifndef S4OFF_LOG
                           if ( connectThread->cb->logConn > 7 )
                           {
                              char buf[250] ;
                              sprintf( buf, "WA0044:Client GetOverlappedResult received an empty packet, ignoring the packet" ) ;
                              assert5( strlen( buf ) < sizeof( buf ) ) ;
                              code4logInfo( connectThread->cb, buf ) ;
                           }
                        #endif
                        u4delayHundredth( 5 ) ;  // wait a bit in case communications is cleaning up
                     #endif
                  }
               }
         }
         else  // reset the failure count for the buffer
         {
            #ifndef S4OFF_LOG
               if ( connectThread->cb->logConn > 8 && connectBuffer->numFails != 0 )
               {
                  char buf[250] ;
                  sprintf( buf, "IA0045:Resetting numFails for connection, where number of consecutive fails was: %ld", connectBuffer->numFails ) ;
                  assert5( strlen( buf ) < sizeof( buf ) ) ;
                  code4logInfo( connectThread->cb, buf ) ;
               }
            #endif
            connectBuffer->numFails = 0 ;
         }
      }

      connect4lowExitExclusive( connectBuffer ) ;
   }

   #ifdef S4LEN_PRINT
      LARGE_INTEGER ts, tf ;

      QueryPerformanceCounter( &ts ) ;
      QueryPerformanceFrequency( &tf ) ;
      double ls = ts.QuadPart ;
      double lf = tf.QuadPart ;
      char buffer[128] ;
      sprintf( buffer, "Read: %3d, on socket %3d at time %f", ( int )message->messageLen, ( int )connectThread->connectBuffer->connectLow->sockr, ( double )ls/lf ) ;
      debug4display( buffer ) ;
      #ifdef S4PART_OUT
         u4writeOut( message->message, 1, min( message->messageLen, 30 ) ) ;
      #endif
      #ifdef S4ALL_OUT
         u4writeOut( message->message, 1, message->messageLen ) ;
      #endif
   #endif

   connect4threadReadStore(connectThread, message, 0 ) ;
   #ifdef E4ANALYZE
      message->inUse = 0 ;    // mark as not in use on the signals list
   #endif
   return ;
}



void connect4threadReadStore( CONNECT4THREAD *connectThread, NET4MESSAGE *message, Bool5 atTop )
{
   /* This function takes the code from connect4threadReadCompleted.
      It simply stores it on the readMessageCompletedList and sets off the
      appropriate signals.

      AS 05/15/02 - added atTop paramater, normally false, this allows the message to be added at the
      top of the list instead of the bottom.  This occurs when we remove an item from the list and
      then need to put it back (for compression/encrpytion, after we extract part of the message out).

      It does more work for the server


     The message is placed on the connectThread.readMessageCompletedList
       list4mutex:
     The CONNECT4THREAD.readMessageCompletedListSemaphore is released to
       indicate to a worker thread a read has been completed
     Check to see whether or not the CONNECT4THREAD is being serviced by a
       worker thread:
       Call list4mutexWait( CONNECT4THREAD.readMessageCompletedList ).  This
         disallows the worker threads from changing the state of a connection
         while we are examining it.
       examine the state of CONNECT4THREAD.workState
       if it is CONNECT4IDLE
          Add CONNECT4THREAD to CODE4.connectsToService
          Set CONNECT4THREAD.workState to CONNECT4WORKING
          Release the semaphore CODE4.workerSemaphore
       Call list4mutexRelease( CONNECT4THREAD.readMessageCompletedList )
   */

   #ifdef E4PARM_LOW
      if ( ( connectThread == 0 ) || ( message == 0 ) )
      {
         error4( 0, e4parmNull, E96984 ) ;
         return ;
      }
   #endif

   // AS 08/25/00 Problem here that the semaphore gets released before the server does its
   // check to release the worker thread.  Esp. in multi-processer systems, may get instance
   // whereby the message gets processed by an active thread before we request the worker thread
   // to be active, in which case the message gets removed and the worker thread has nothing
   // to do but get stuck in endless loop.  Move line to after #ifdef S4SERVER
   // AS July 24/02 - modify so that we add to list in server case after reserving list, to avoid
   // potential multi-thread problems and improve performance.
   #ifdef S4SERVER
      // AS July 26/02 -Need to do some sequencing here on connect... if workState is
      // neither working nor idle (e.g. still connecting), let's wait here for a little
      // while until the status hopefully will change...
      CONNECT4 *connect = connectThread->connectBuffer->connect ;
      int waitCount = 0 ;

      // AS Aug 29/02 - problem here, the workState gets changed before the connected flag, so
      // in theory we can have a non-zero workState but have the connected flag not set yet...
      // so add this wait condition as well
      while ( (connect->workState == 0 || connect->client->connection.connected == 0 ) && message->messageLen > 0 )
      {
         if ( waitCount++ > 300 )  // if waiting this long (3 seconds), terminate the connection
         {
            message->messageLen = -1 ;  // force an error
            break ;
         }
         Sleep(10) ;  // wait 1/100th of a second, wait count to wait up to 3 seconds
      }

      assert5( connect->client->connection.connected == 1 || message->messageLen <= 0 ) ;

      list4mutexWait( &connectThread->readMessageCompletedList ) ;
      if ( atTop == 1 )
         l4addBefore( &connectThread->readMessageCompletedList.list, l4first( &connectThread->readMessageCompletedList.list ), message ) ;
      else
         l4add( &(connectThread->readMessageCompletedList.list), message ) ;
         // list4mutexAdd( &connectThread->readMessageCompletedList, message ) ;
      // l4add( &(connectThread->readMessageCompletedList.list), message ) ;
      if ( connect->workState == CONNECT4IDLE )
      {
         // AS July 24/02 - We need to reserve the connectsToService until we have updated the workstate
         // otherwise we have the possibility that a worker thread will grab this connection because
         // the worker semaphore may already be in a released state from another client...
         assert5( connect->client->connection.connected == 1 ) ;
         list4mutexWait( &connectThread->cb->connectsToService ) ;
         l4add( &connectThread->cb->connectsToService.list, connectThread->connectBuffer->connect ) ;
         // #ifdef S4TESTING
         //    u4writeErr( "setting state to WORKING", 1 ) ;
         // #endif
         connectThread->connectBuffer->connect->workState = CONNECT4WORKING ;
         list4mutexRelease( &connectThread->cb->connectsToService ) ;
         semaphore4release( &connectThread->cb->workerSemaphore ) ;
      }
      list4mutexRelease( &connectThread->readMessageCompletedList ) ;
   #else
      // AS Mar 13/03 - Was not adding to top as required (client)
      if ( atTop == 1 )
         list4mutexAddTop( &connectThread->readMessageCompletedList, message ) ;
      else
         list4mutexAdd( &connectThread->readMessageCompletedList, message ) ;
   #endif
   semaphore4release( &connectThread->readMessageCompletedListSemaphore ) ;
}



int connect4threadReadRequest( CONNECT4THREAD *connectThread, NET4MESSAGE *message )
{
/* PARAMATERS

   connectThread is the connect thread structure
   message is a message initialized for performing a ReadFile

   ERRORS/RETURNS

   r4success
   < 0 error ( set error4code( CODE4 ) by calling error4() )

   NOTES

   This function is called by the application/server in order to signal to the
     communications thread that a ReadFile request is required.

   This function is used to request the communications thread to perform
     a ReadFile on the input NET4MESSAGE

   This function does the following:
     set message->connectThread = connectThread
     Call inter4readRequested( connectThread->inter, message )
*/

   #ifdef S4REQUEST_PRINT
      char buffer[128] ;
   #endif

   #ifdef E4PARM_LOW
      if ( ( connectThread==0 ) || ( message == 0 ) )
         return error4( 0, e4parmNull, E96947 ) ;
   #endif

   #ifdef S4REQUEST_PRINT
      sprintf( buffer, "Read requested by intercommunications thread on socket %d", ( int )connectThread->connectBuffer->connectLow->sockr ) ;
      debug4display( buffer ) ;
   #endif

   // AS May 16/02 - Added code to manipulate parts of the NET4MESSAGE for compression and encryption
   // we now have a current pointer into the message - set this prior to every read to point to the start of the buffer

   // if the message was internally allocated, it means is was created temporarily to handle a special
   // encrpytion/compression case.  In that case, we actually don't want to use it for requesting because it
   // will have the side-effect of increasing the number of outstanding reads on the connection.  Instead, just
   // free it now.
   if ( message->independentAllocation != 0 )
   {
      if ( message->independentAllocation == 1 )  // means we want to free it...
         u4free( message ) ;

      // AS May 10/11 - not available in compression
      #ifndef S4OFF_COMPRESS
         else  // it was used by c4, make avail again
         {
               assert5( message == connectThread->cb->extraReadMessage ) ;
               connectThread->cb->extraReadMessageAvail = 1 ;

         }
      #endif
      return r4success ;
   }

   message->currentPtr = message->message ;
   message->messageLen = message->messageBufferLen ;
   message->connectThread = connectThread ;
   inter4readRequested( connectThread->inter, message ) ;
   return r4success ;
}



SIGNAL4ROUTINE *connect4threadReadSignalRetrieve( CONNECT4THREAD *connectThread, SIGNAL4ROUTINE *routine, int doRemove )
{
   /* PARAMATERS

      routine is the routine to remove from the list.  If routine is 0, an
        element is popped off the list using l4pop ( this is only done when reads
        are being cancelled, in which case the order does not matter )

      NOTES

      This function is used to remove and return a member of the outstanding
        reads signal list.

      if ( routine == 0 )
         return l4pop( CONNECT4THREAD.readSignalsList ) ;
      else
      {
         l4remove( CONNECT4THREAD.readSignalsList, routine )
         return routine ;
      }
   */

   #ifdef E4PARM_LOW
      if ( connectThread == 0 )
      {
         error4( 0, e4parmNull, E96948 ) ;
         return 0 ;
      }
   #endif

   /* NOTE: cannot check error code because this function is called by the
      intercommunications thread, and the errorCode can be changed by any
      other thread at any time, unrelated to a communications failure
     if ( error4code( connectThread->cb )<0 )
        return 0 ;
   */

   if ( routine == 0 )
   {
      if ( doRemove )
         return ( SIGNAL4ROUTINE * )l4pop( &connectThread->readSignalsList ) ;
      else
         return ( SIGNAL4ROUTINE * )l4last( &connectThread->readSignalsList ) ;
   }
   if ( doRemove )
      l4remove( &connectThread->readSignalsList, routine ) ;
   return routine ;
}



void connect4threadWriteCompleted( CONNECT4THREAD *connectThread, NET4MESSAGE *message, SIGNAL4ROUTINE *routine )
{
   /* ERRORS

      In case of error, set message.messageLen to -1.  This will indicate to
        the higher-level routines that a failure occurred.

      NOTES

      This function does the following:

      Check NET4MESSAGE.messageLen:
        If set to -1 then the write failed.
        If set to 0 then the write succeeded.
        If > 0, do the following:
          get the result of the send by checking the NET4MESSAGE.overlapped
            if write failed:
              set NET4MESSAGE.messageLen to -1
            else
              set NET4MESSAGE.messageLen to 0

      Remove the SIGNAL4ROUTINE from CONNECT4THREAD.writingSignalsList
      Add NET4MESSAGE to CONNECT4THREAD.writeMessageCompletedList.
      call semaphore4trigger( CONNECT4THREAD.writeCompletedSemaphore )
   */

   int rc, write ;

   #ifdef E4PARM_LOW
      if ( ( connectThread==0 ) || ( message == 0 )||( routine==0 ) )
      {
         error4( 0, e4parmNull, E96949 ) ;
         return ;
      }
   #endif

   if ( message->messageLen > 0 )
   {
      CONNECT4BUFFER *connectBuffer = connectThread->connectBuffer ;
      connect4lowEnterExclusive( connectBuffer ) ;
      if ( connect4bufferLowGet( connectBuffer ) == 0 )
         message->messageLen = 0 ;
      else
      {
         rc = GetOverlappedResult( ( HANDLE )connect4bufferLowGet( connectBuffer )->sockw, &message->overlap, ( unsigned long * )&write, TRUE ) ;
         if ( rc )
            message->messageLen = 0 ;
         else
         {
            // AS Sept 18/02 - track the actual error reason as well
            DWORD winErr = GetLastError() ;
            if ( winErr != 0 )
               connectBuffer->windowsErr = winErr ;
            message->messageLen = -1 ;
         }
      }
      connect4lowExitExclusive( connectBuffer ) ;
   }
   connect4threadWriteSignalRetrieve( connectThread, routine, 1 ) ;
   /* l4remove( &connectThread->writingSignalsList, routine ) ; */
   list4mutexAdd( &connectThread->writeMessageCompletedList, message ) ;
   semaphore4release( &connectThread->writeMessageCompletedListSemaphore ) ;
}



void connect4threadWriteRequest( CONNECT4THREAD *connectThread, NET4MESSAGE *message )
{
   /* PARAMATERS:

      connect is the CONNECTION4THREAD for which to perform the write.
      message is the message to send.  It can be added directly to the list
        of messages to send.  After this function call, the caller should no
        longer use the message pointer.

      ERRORS/RETURNS

      r4success
      < 0 error ( set error4code( CODE4 ) by calling error4() )

      NOTES

      This function is called by the application/server in order to signal to the
        communications thread that a WriteFile request is required.

      This function does the following:
        set message->connectThread = connectThread
        Call inter4writeRequested( connectThread->inter, message )
   */

   #ifdef E4PARM_LOW
      if ( ( connectThread==0 ) || ( message == 0 ) )
      {
         error4( 0, e4parmNull, E96950 ) ;
         return ;
      }
   #endif

   message->connectThread = connectThread ;
   // AS May 16/02 - Added code to manipulate parts of the NET4MESSAGE for compression and encryption
   // we now have a current pointer into the message - set this prior to every write to point to the start of the buffer
   message->currentPtr = message->message ;
   inter4writeRequested( connectThread->inter, message ) ;
}



SIGNAL4ROUTINE *connect4threadWriteSignalRetrieve( CONNECT4THREAD *connectThread, SIGNAL4ROUTINE *routine, int doRemove )
{
   /* PARAMATERS

      routine is the routine to remove from the list.  If routine is 0, an
        element is popped off the list using l4pop ( this is only done when reads
        are being cancelled, in which case the order does not matter. When a
        write is being cancelled, order remains important. )

      NOTES

      This function is used to remove and return a member of the outstanding
        reads signal list.

      if ( routine == 0 )
         routine = l4first( CONNECT4THREAD.writingSignalsList ) ;

      l4remove( CONNECT4THREAD.writingSignalsList, routine )
      return routine ;
   */

   #ifdef E4PARM_LOW
      if ( connectThread==0 )
      {
         error4( 0, e4parmNull, E96951 ) ;
         return 0 ;
      }
   #endif

   if ( routine == 0 )
   {
      routine = ( SIGNAL4ROUTINE * )l4first( &connectThread->writingSignalsList ) ;
      if ( routine == 0 )
         return 0 ;
   }
   if ( doRemove )
      l4remove( &connectThread->writingSignalsList, routine ) ;
   return routine ;
}
#endif /*!S4OFF_THREAD */
#endif /* !S4OFF_COMMUNICATIONS */
#endif /*!S4STAND_ALONE */
