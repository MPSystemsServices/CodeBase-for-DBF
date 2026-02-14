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
      connectThread->connectBuffer->connect->workState = CONNECT4SHUTDOWN ;
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
         list4mutexRemoveLink( &connectThread->cb->connectsToService, ( LINK4 * )connectThread ) ;
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

   This function does the following:
     connectThread->cb = c4 ;
     connectThread->inter = c4->inter ;
     connectThread->connectLow = connectLow ;
     if ( connect == 0 )
        connectThread->workState = CONNECT4SPECIAL ;
     else
     {
        connectThread->connect = connect ;
        connectThread->workState = CONNECT4IDLE ;
     }
     list4mutexInit( connectThread->readMessageCompletedList ) ;
     semaphore4init( connectThread->readMessageCompletedListSemaphore ) ;
     list4mutexInit( connectThread->writingSignalsList ) ;
     list4mutexInit( connectThread->writeMessageCompletedList ) ;
     semaphore4init( connectThread->writeCompletedMemoryListSemaphore ) ;
     semaphore4init( connectThread->completedSemaphore ) ;
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

   /* connectThread->connectLow = connectLow ; */
   #ifdef S4SERVER
/*
      if ( connect == 0 )
         connectThread->workState = CONNECT4SPECIAL ;
      else
      {
         connectThread->connect = connect ;
         connectThread->workState = CONNECT4IDLE ;
      }
*/
   #endif
   rc = semaphore4init( &connectThread->readMessageCompletedListSemaphore ) ;
   if ( rc )
      return error4( c4, rc, E96925 ) ;
   rc = semaphore4init( &connectThread->writeMessageCompletedListSemaphore ) ;
   if ( rc )
   {
      semaphore4initUndo( &connectThread->readMessageCompletedListSemaphore ) ;
      return error4( c4, rc, E96925 ) ;
   }
   rc = semaphore4init( &connectThread->completedSemaphore ) ;
   if ( rc )
   {
      semaphore4initUndo( &connectThread->readMessageCompletedListSemaphore ) ;
      semaphore4initUndo( &connectThread->writeMessageCompletedListSemaphore ) ;
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
   }

   list4mutexInitUndo( &connectThread->readMessageCompletedList ) ;

   /* AS 08/02/99 - was releasing this sometimes with messages still on list... */
   for( ;; )
   {
      NET4MESSAGE *message = (NET4MESSAGE *)l4first( &connectThread->writeMessageCompletedList.list ) ;
      if ( message == 0 )
         break ;

      // AS 11/09/00 ensure nobody else messing with list...
      list4mutexRemoveLink( &connectThread->writeMessageCompletedList, (LINK4 *)message ) ;
      mem4free( connectThread->cb->writeMemory, message ) ;
   }

   list4mutexInitUndo( &connectThread->writeMessageCompletedList ) ;
   semaphore4initUndo( &connectThread->readMessageCompletedListSemaphore ) ;
   semaphore4initUndo( &connectThread->writeMessageCompletedListSemaphore ) ;
   semaphore4initUndo( &connectThread->completedSemaphore ) ;
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
      /* AS 08/27/99 --> possible to get gpf here on uninitialization... */
      if ( connectThread->connectBuffer->connectLow == 0 || connectThread->connectBuffer->connectLow->sockr == 0 )
         message->messageLen = -1 ;
      else
      {
         int rc = GetOverlappedResult( ( HANDLE )connectThread->connectBuffer->connectLow->sockr, &message->overlap, ( unsigned long * )&message->messageLen, TRUE ) ;
         if ( rc == FALSE || message->messageLen <= 0 )
            message->messageLen = -1 ;
      }
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

   connect4threadReadStore(connectThread, message ) ;
   #ifdef E4ANALYZE
      message->inUse = 0 ;    // mark as not in use on the signals list
   #endif
   return ;
}



void connect4threadReadStore( CONNECT4THREAD *connectThread, NET4MESSAGE *message )
{
   /* This function takes the code from connect4threadReadCompleted.
      It simply stores it on the readMessageCompletedList and sets off the
      appropriate signals.

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
   list4mutexAdd( &connectThread->readMessageCompletedList, message ) ;
   // AS 08/25/00 Problem here that the semaphore gets released before the server does its
   // check to release the worker thread.  Esp. in multi-processer systems, may get instance
   // whereby the message gets processed by an active thread before we request the worker thread
   // to be active, in which case the message gets removed and the worker thread has nothing
   // to do but get stuck in endless loop.  Move line to after #ifdef S4SERVER
   #ifdef S4SERVER
      list4mutexWait( &connectThread->readMessageCompletedList ) ;
      if ( connectThread->connectBuffer->connect->workState == CONNECT4IDLE )
      {
         list4mutexAdd( &connectThread->cb->connectsToService, connectThread->connectBuffer->connect ) ;
         connectThread->connectBuffer->connect->workState = CONNECT4WORKING ;
         semaphore4release( &connectThread->cb->workerSemaphore ) ;
      }
      list4mutexRelease( &connectThread->readMessageCompletedList ) ;
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

   if ( message->messageLen >0 )
   {
      rc = GetOverlappedResult( ( HANDLE )connectThread->connectBuffer->connectLow->sockw, &message->overlap, ( unsigned long * )&write, TRUE ) ;
      if ( rc )
         message->messageLen = 0 ;
      else
         message->messageLen = -1 ;
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
#endif /* not S4OFF_COMMUNICATIONS */
#endif /*!S4STAND_ALONE */
