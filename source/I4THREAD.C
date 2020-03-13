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

/* This module is used to describe the communications thread.

If E4PARM_LOW is defined, the functions in this module will ensure that
  their paramaters are not invalid.  This error will be generated even
  for those functions which otherwise say that no error is generated.

The communications thread is used to perform asynchronous communications
as requested by worker threads.
*/

#include "d4all.h"
#ifndef S4STAND_ALONE
#ifndef S4OFF_COMMUNICATIONS
#ifndef S4OFF_THREAD
#include <process.h>

static void inter4releaseSignal( INTER4 *, SIGNAL4ROUTINE * ) ;

void inter4( void *input )
{
/* PARAMATERS

   inter is an initialized INTER4 structure.  ( the inter4()
     routine will get called from code4init() after initializing the
     INTER4 structure ).  We want an initialized structure so we
     do not have to worry about special handling of errors if the
     initialization fails.

   ERRORS

   Just ignore any errors.  In theory, I don't believe that errors should
     happen.  WIN32 specs do not specify any particular error which would
     occur when calling WaitForMultipleObjects() which is the only error
     point in this routine ( i.e. from a signal4arrayWait failure ).  Do NOT
     set error4code( CODE4 ) in case of failure.

   NOTES

   Initialization phase:
     Call inter4Init( INTER4 )

   The function calls signal4arrayWait on the array of signals, which
     blocks the thread execution until one of the signals has been
     signaled.

   The following is a list of the types of signals which are kept in the
     array of signals, and, when signaled, will cause the communications
     thread to awaken:

     A CONNECT4THREAD has signaled that a ReadFile() is requested ( by releasing
       INTER4.readsRequestedSemaphore )
       call inter4ReadRequired()

     Windows Sockets has signaled that a ReadFile() has completed ( data has
       arrived )
       call inter4ReadCompleted()

     A CONNECT4THREAD has signaled that a writeFile() is requested ( by releasing
       INTER4.writesRequestedSemaphore )
       call inter4WriteRequired()

     Windows Sockets has signaled that a WriteFile() has completed
       call inter4WriteCompleted()

     A CONNECT4THREAD has signaled that a connection is no longer being used ( by
       releasing INTER4.completesRequestedSemaphore )
       ( i.e. cancel any outstanding ReadFiles() )
       call inter4completeRequired()

   PSEUDOCODE

     SIGNAL4ROUTINE *routineToExecute ;

     for( ;; )
     {
        routineToExecute = signal4arrayWait( c4->inter->signalArray ) ;
        if ( routineToExecute != 0 )
           routineToExecute->routine( c4->inter, routineToExecute ) ;
     }
*/
   INTER4 *inter = ( INTER4 * )input ;
   SIGNAL4ROUTINE *routineToExecute ;
   long waitTime;
   int rc ;
   #ifdef S4CLIENT
      #ifdef S4DEAD_CHECK
         long checkTime, currentTime ;
         int i = 0 ;
      #endif
   #endif

   #ifdef E4PARM_LOW
      if ( inter == 0 )
      {
         error4( 0, e4parmNull, E96952 ) ;
         return ;
      }
   #endif

   #ifdef S4CLIENT
      #ifdef S4DEAD_CHECK
         waitTime = INTER4WAITTIME*1000 ;
         checkTime = time( 0 ) + DEAD4CHECK_TIMEOUT ;
      #else
         waitTime = INFINITE ;
      #endif
   #else
      waitTime = INFINITE ;
   #endif


   while ( 1 )
   {
      rc = signal4arrayWait( &inter->signalArray, &routineToExecute, waitTime ) ;
      //routineToExecute = signal4arrayWait( &inter->signalArray, waitTime ) ;
      if ( rc == e4result )  /* means a wait failed -- just try again */
         continue ;
      if ( routineToExecute != 0 )
         routineToExecute->routine( inter, routineToExecute ) ;
      #ifdef S4CLIENT
         #ifdef S4DEAD_CHECK
            if ( i==DEAD4CHECK_LOOP )
            {
               currentTime = time( 0 ) ;
               i = 0 ;
               if ( currentTime > checkTime )
               {
                  inter4connectionCheck( inter ) ;
                  checkTime = currentTime + DEAD4CHECK_TIMEOUT ;
               }
            }
            i++ ;
         #endif
      #endif
   }
}



void inter4completeRequested( INTER4 *inter, CONNECT4THREAD *connectThread )
{
/* ERRORS

   No errors are possible in this module

   NOTES

   This function is called by a worker thread to request that the
     communications thread be signalled to request a complete for the
     input CONNECT4THREAD

   This function does the following:
     list4mutexAdd( inter->completesRequested, connectThread ) ;
     semaphore4release( inter->completesRequestedSemaphore ) ;
     return
*/

   #ifdef E4PARM_LOW
      if ( ( inter == 0 )|| ( connectThread == 0 ) )
      {
         error4( 0, e4parmNull, E96953 ) ;
         return ;
      }
   #endif

   /* NOTE: cannot check error code because this function is called by the
      intercommunications thread, and the errorCode can be changed by any
      other thread at any time, unrelated to a communications failure
      if ( error4code( inter->cb ) < 0 )
         return ;
   */

   list4mutexAdd( &inter->completesRequested, connectThread ) ;
   semaphore4release( &inter->completesRequestedSemaphore ) ;
   return ;
}



void inter4completeRequired( INTER4 *inter, SIGNAL4ROUTINE *signal )
{
/* ERRORS

   Ignore any errors and continue uninitialization.  Do not set the
     error4code( CODE4 ) in case of failure.

   NOTES

   This function is called when the communications thread has been signaled
     to stop servicing a connection.

   The connection which should be completed can be obtained from the
     INTER4.completesRequested list4mutex.

   Any outstanding reads should be cancelled, and any unserviced completed
     reads should be removed.

   When any outstanding writes are completed by the communications thread,
     there will be no more outstanding signals on this connection, and
     thus it will in effect no longer be serviced by the communications
     thread.

   This routine releases the CONNECT4THREAD.completedSemaphore
*/

   CONNECT4THREAD *conThread ;
   SIGNAL4ROUTINE *sigRoutine ;
   int rc ;

   #ifdef E4PARM_LOW
      if ( ( inter == 0 ) && ( signal == 0 ) )
      {
         error4( 0, e4parmNull, E96954 ) ;
         return ;
      }
   #endif

   conThread = ( CONNECT4THREAD * )list4mutexRemove( &inter->completesRequested ) ;

   if ( conThread == 0 )
      return ;
   /* Wait for writing writes to finish */

   while( 1 )
   {
      /* sigRoutine = connect4threadWriteSignalRetrieve( conThread, 0, 1 ) ; */
      sigRoutine = (SIGNAL4ROUTINE *)l4first( &conThread->writingSignalsList );

      if ( sigRoutine == 0 )
         break;
      rc = event4wait( (EVENT4 *)sigRoutine->signalStructure, 1 ) ;
      if ( rc <= 0 )
      {
         l4remove( &conThread->writingSignalsList, (void *)sigRoutine ) ;
         inter4releaseSignal( inter, sigRoutine ) ;  /* AS 07/09/98 was losing some resources, think here */
         continue ;
      }
      inter4writeCompleted( inter, sigRoutine ) ;
   }


   CONNECT4BUFFER *connectBuffer = conThread->connectBuffer ;
   connect4lowEnterExclusive( connectBuffer ) ;
   if ( connect4bufferLowGet( connectBuffer ) != 0 )
      connect4lowClose( connect4bufferLowGet( connectBuffer ) ) ;   /* Ignoring return code */
   connect4lowExitExclusive( connectBuffer ) ;

   while( 1 )
   {
      sigRoutine = (SIGNAL4ROUTINE *)l4first( &conThread->readSignalsList ) ;
      if ( sigRoutine == 0 )
         break ;
      rc = event4wait( (EVENT4 *)sigRoutine->signalStructure, 1 ) ;
      if ( rc <= 0 )
      {
         l4remove( &conThread->readSignalsList, (void *)sigRoutine ) ;
         inter4releaseSignal( inter, sigRoutine ) ;  /* AS 07/09/98 was losing some resources, think here */
         continue ;
      }
      inter4readCompleted( inter, sigRoutine ) ;
   }
   semaphore4release( &conThread->completedSemaphore ) ;
}



#ifdef S4DEAD_CHECK
#ifdef S4CLIENT
void inter4connectionCheck( INTER4 *inter )
{
   CONNECT4LOW conlow ;
   CONNECT4 *connect ;

   connect = &inter->cb->clientConnect ;
   connect4lowConnectConnect( &conlow, &connect->address, connect->addrLen ) ;
   connect4lowWrite( &conlow, ( char * )&connect->clientId, 4 ) ;
   connect4lowClose( &conlow ) ;
}
#endif /* S4CLIENT */
#endif /* S4DEAD_CHECK */



void inter4error( INTER4 *inter, CONNECT4THREAD *connectThread )
{
/* ERRORS

   Ignore any errors and continue uninitialization.  Do not set the
     error4code( CODE4 ) in case of failure.

   NOTES

   This function is called when a CONNECT4LOW has produced a connection
     failure from with a inter4 function.

   This function calls connect4lowError( CONNECT4LOW )

   This causes the application/server thread to determine that there
     is a problem and disconnect the connection.

   This function should also cancel any outstanding reads/writes on the
     socket, if any, causing errors to be propogated back up the the client.
     This is done by cancelling the reads and writes, setting the
     NET4MESSAGE.messageLen to -1, and posting the read/write to the
     appropriate list.
*/

   #ifdef E4PARM_LOW
      if ( ( inter == 0 ) && ( connectThread == 0 ) )
      {
         error4( 0, e4parmNull, E96955 ) ;
         return ;
      }
   #endif

   CONNECT4BUFFER *connectBuffer = connectThread->connectBuffer ;
   connect4lowEnterExclusive( connectBuffer ) ;
   connect4lowError( connect4bufferLowGet( connectBuffer ) ) ;
   connect4lowExitExclusive( connectBuffer ) ;

   while ( 1 )
   {
      SIGNAL4ROUTINE *sigRoutine = connect4threadWriteSignalRetrieve( connectThread, 0, 0 ) ;
      if ( sigRoutine == 0 )
         break ;
      if ( event4wait( (EVENT4 *)sigRoutine->signalStructure, 1 ) <= 0 )
         continue ;
      inter4writeCompleted( inter, sigRoutine ) ;
   }

   while( 1 )
   {
      // AS 08/29/00 - make more efficient, pop off
      SIGNAL4ROUTINE *sigRoutine = (SIGNAL4ROUTINE *)l4pop( &connectThread->readSignalsList ) ;
      if ( sigRoutine == 0 )
         break ;
      // l4remove( &connectThread->readSignalsList, (void *)sigRoutine ) ;
      if ( event4wait( (EVENT4 *)sigRoutine->signalStructure, 1 ) <= 0 )
         continue ;
      NET4MESSAGE *message = (NET4MESSAGE *)sigRoutine->data ;
      message->messageLen = -1 ;
      #ifdef E4ANALYZE  // CS 2000/08/30
         assert5( message->inUse == 1 ) ;
         message->inUse = 0 ;
      #endif
      connect4threadReadStore(message->connectThread, message, 0 ) ;
      inter4releaseSignal(inter, sigRoutine ) ;
   }
}



int inter4init( INTER4 *inter, CODE4 *cb )
{
/* PARAMATERS

   inter is the communication thread structure to be initialized
   c4 is a valid CODE4 pointer

   ERRORS

   In case of failure, set the error4code( CODE4 ) and return the appropriate
     error code.

   NOTES

   This function is used to initialize a INTER4 object.


   The following initializations are done:
     The entire structure is memset to zero.
     semaphore4init( &INTER4.readsRequestedSemaphore )
     signal4routineInit( &INTER4.readsRequestedRoutine,
                          INTER4.readsRequestedSemaphore.handle,
                          inter4readRequired,
                          &INTER4.readsRequestedSemaphore, 0 )
     semaphore4init( &INTER4.writesRequestedSemaphore )
     signal4routineInit( &INTER4.writesRequestedRoutine,
                          INTER4.writesRequestedSemaphore.handle,
                          inter4writeRequired,
                          &INTER4.writesRequestedSemaphore, 0 )
     semaphore4init( &INTER4.completesRequestedSemaphore )
     signal4routineInit( &INTER4.completesRequestedRoutine,
                          INTER4.completesRequestedSemaphore.handle,
                          inter4completeRequired,
                          &INTER4.completesRequestedSemaphore, 0 )
     event4init( &INTER4.shutdownThreadEvent )
     signal4routineInit( &INTER4.shutdownThreadRoutine,
                          INTER4.shutdownThreadEvent.handle,
                          inter4shutdown, &INTER4.shutdownThreadEvent,
                          0 )
     INTER4.signalArray is initialized using signal4arrayInit()
     INTER4.readsRequestedRoutine is added to the signalArray
     INTER4.writesRequestedRoutine is added to the signalArray
     INTER4.completesRequestedRoutine is added to the signalArray
     INTER4.ShutdownThreadRoutine is added to the signalArray
     list4mutexInit( inter->readsRequested )
     list4mutexInit( inter->writesRequested )
     list4mutexInit( inter->completesRequested )
*/

   #ifdef E4PARM_LOW
      if ( ( inter == 0 ) || ( cb == 0 ) )
         return ( error4( cb, e4parm_null, E96956 ) ) ;
   #endif

   memset( inter, 0, sizeof( INTER4 ) ) ;
   inter->cb = cb ;

   int rc = semaphore4init( &inter->readsRequestedSemaphore ) ;
   if ( rc )
      return error4( cb, rc, E96956 ) ;

   rc = semaphore4init( &inter->writesRequestedSemaphore ) ;
   if ( rc )
   {
      semaphore4initUndo( &inter->readsRequestedSemaphore ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = semaphore4init( &inter->completesRequestedSemaphore ) ;
   if ( rc )
   {
      semaphore4initUndo( &inter->readsRequestedSemaphore ) ;
      semaphore4initUndo( &inter->writesRequestedSemaphore ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = event4init( &inter->shutdownThreadEvent ) ;
   if ( rc )
   {
      semaphore4initUndo( &inter->readsRequestedSemaphore ) ;
      semaphore4initUndo( &inter->writesRequestedSemaphore ) ;
      semaphore4initUndo( &inter->completesRequestedSemaphore ) ;
      return error4( cb, rc, E96956 ) ;
   }
   inter->signalRoutineMemory = mem4create( cb, MEMORY4START_SIGNAL_ROUTINE , sizeof( SIGNAL4ROUTINE ), MEMORY4EXPAND_SIGNAL_ROUTINE, 0 ) ;
   if ( inter->signalRoutineMemory == 0 )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }

   rc = signal4routineInit( &inter->readsRequestedRoutine, inter->readsRequestedSemaphore.handle, inter4readRequired, &inter->readsRequestedSemaphore, 0 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = signal4routineInit( &inter->writesRequestedRoutine, inter->writesRequestedSemaphore.handle, inter4writeRequired, &inter->writesRequestedSemaphore, 0 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = signal4routineInit( &inter->completesRequestedRoutine, inter->completesRequestedSemaphore.handle, inter4completeRequired, &inter->completesRequestedSemaphore, 0 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = signal4routineInit( &inter->shutdownThreadRoutine, inter->shutdownThreadEvent.handle, inter4shutdown, &inter->shutdownThreadEvent, 0 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = signal4arrayInit( &inter->signalArray, cb, 100 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }

   rc = signal4arrayAdd( &inter->signalArray, &inter->readsRequestedRoutine, 1 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = signal4arrayAdd( &inter->signalArray, &inter->writesRequestedRoutine, 1 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = signal4arrayAdd( &inter->signalArray, &inter->completesRequestedRoutine, 1 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   rc = signal4arrayAdd( &inter->signalArray, &inter->shutdownThreadRoutine, 1 ) ;
   if ( rc )
   {
      inter4initUndo( inter ) ;
      return error4( cb, rc, E96956 ) ;
   }
   list4mutexInit( &inter->readsRequested ) ;
   list4mutexInit( &inter->writesRequested ) ;
   list4mutexInit( &inter->completesRequested ) ;
   return r4success ;
}

void inter4initUndo( INTER4 *inter )
{
/* ERRORS

   Ignore any errors and continue uninitialization.  Do not set the
     error4code( CODE4 ) in case of failure.
*/
   semaphore4initUndo( &inter->readsRequestedSemaphore ) ;
   semaphore4initUndo( &inter->writesRequestedSemaphore ) ;
   semaphore4initUndo( &inter->completesRequestedSemaphore ) ;
   event4initUndo( &inter->shutdownThreadEvent ) ;

   signal4arrayInitUndo( &inter->signalArray ) ;

   list4mutexInitUndo( &inter->readsRequested ) ;
   list4mutexInitUndo( &inter->writesRequested ) ;
   list4mutexInitUndo( &inter->completesRequested ) ;
   mem4release( inter->signalRoutineMemory ) ;
   code4clearEvents( inter->cb ) ;
}



void inter4readCompleted( INTER4 *inter, SIGNAL4ROUTINE *signalRoutine )
{
/* PARAMATERS

   inter is the communication thread structure.
   signalRoutine is the SIGNAL4ROUTINE corresponding to the read

   ERRORS

   No errors are possible in this module. It doesn't check CODE4, because
   if we are calling inter4completeRequired() due to a shutdown error,
   we need this function to clean up any pending reads.

   NOTES

   Called when a read is completed.

   SIGNAL4ROUTINE.data corresponds to the NET4MESSAGE structure which
     is associated with the completed ReadFile()

   The following is done:
     CONNECT4THREAD is obtained from NET4MESSAGE.connectThread
     Call connect4threadReadCompleted( CONNECT4THREAD, SIGNAL4ROUTINE,
       NET4MESSAGE )
     Call code4freeEvent( CODE4, SIGNAL4ROUTINE.signalStructure )
     Call mem4free( c4, SIGNAL4ROUTINE )
     return
*/

   NET4MESSAGE *mess ;
   CONNECT4THREAD *conThr ;
   SIGNAL4ROUTINE *sig ;

   #ifdef E4PARM_LOW
      if ( ( inter==0 ) || ( signalRoutine == 0 ) )
      {
         error4( 0, e4parmNull, E96957 ) ;
         return ;
      }
   #endif

   mess = (NET4MESSAGE *)signalRoutine->data ;
   conThr = mess->connectThread ;
   #ifdef E4DEBUG /* We should only be completing on the first message */
      sig = (SIGNAL4ROUTINE *)l4first(&conThr->readSignalsList) ;
      if (signalRoutine != sig )
      {
         error4( 0, e4message, E96957 ) ;
         return ;
      }
   #endif

   connect4threadReadCompleted( conThr, signalRoutine, mess ) ;

   /* Clean up the signal */
   inter4releaseSignal( inter, signalRoutine ) ;

   /* If there are any on the list, we need to add them to the signal4array*/
   sig = (SIGNAL4ROUTINE *)l4first( &mess->connectThread->readSignalsList ) ;
   if ( sig )
      signal4arrayAdd( &inter->signalArray, sig, 0 ) ;

   return ;
}



void inter4readRequested( INTER4 *inter, NET4MESSAGE *message )
{
/* ERRORS

   No errors are possible in this module

   NOTES

   This function is called by a worker to thread to request that the
     communications thread be signalled to request a read for the input
     message

   This function does the following:
     list4mutexAdd( inter->readsRequested, message ) ;
     semaphore4release( inter->readsRequestedSemaphore ) ;
     return
*/

   #ifdef S4REQUEST_PRINT
      char buffer[128] ;
   #endif

   #ifdef E4PARM_LOW
      if ( ( inter==0 ) || ( message == 0 ) )
      {
         error4( 0, e4parmNull, E96958 ) ;
         return ;
      }
   #endif

   #ifdef S4REQUEST_PRINT
      sprintf( buffer, "Read requested by worker thread on socket %d", ( int )message->connectThread->connectBuffer->connectLow->sockr ) ;
      debug4display( buffer ) ;
   #endif

   list4mutexAdd( &inter->readsRequested, message ) ;
   semaphore4release( &inter->readsRequestedSemaphore ) ;
   return ;
}



void inter4readRequired( INTER4 *inter, SIGNAL4ROUTINE *dummy )
{
/* PARAMATERS

   inter is the communication thread structure.
   dummy is not used, but is required to have consistent paramaters to the
      function pointers for the SIGNAL4ROUTINE.function

   ERRORS

   In case of error, call error4() with a NULL CODE4 ( since one is not
     available ), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.
     Return NULL.

   NOTES

   This function is called when the communications thread has been signaled
     to perform an asynchronous ReadFile command.

   This function does the following:
     Remove the first element of INTER4.readsRequested, which is a
       NET4MESSAGE structure
     obtain a SIGNAL4ROUTINE object from INTER4.signalRoutineMemory
     obtain an EVENT4 object by calling code4getEvent() ;
     initialize the SIGNAL4ROUTINE object by calling:
       signal4routineInit( &SIGNAL4ROUTINE, EVENT4.handle,
         inter4readCompleted, &EVENT4, NET4MESSAGE )
     Call connect4lowReadAsynchronous( CONNECT4, NET4MESSAGE, EVENT4 )
       If result is r4success:
         call inter4readCompleted( INTER4, SIGNAL4ROUTINE )
         return
       If result is r4pending:
         Add the SIGNAL4ROUTINE to CONNECT4THREAD.reads list.
         Place the SIGNAL4ROUTINE by calling
           signal4arrayAdd( INTER4.signalArray, SIGNAL4ROUTINE, 0 ) ;
         return
       If result is e4outsanding:
         For now, just treat as any other error.  If it becomes as issue,
           we can address it specifically ( it will only become an issue
           depending on how the blasts are implemented )
       If result is any other error:
         Set NET4MESSAGE.messageLen to -1 to indicate an error.
         Call inter4readCompleted( INTER4, SIGNAL4ROUTINE )
         Call inter4error( INTER4 ) to indicate a connection failed
         return
*/

   NET4MESSAGE *mess ;
   SIGNAL4ROUTINE *sig ;
   EVENT4 *eve ;
   int rc ;

   #ifdef E4PARM_LOW
      if ( inter == 0 )
      {
         error4( 0, e4parmNull, E96959 ) ;
         return ;
      }
   #endif

   /* NOTE: cannot check error code because this function is called by the
      intercommunications thread, and the errorCode can be changed by any
      other thread at any time, unrelated to a communications failure
      if ( error4code( inter->cb ) )
      {
         error4( inter->cb, error4code( inter->cb ), E96959 ) ;
         return ;
      }
   */

   mess = (NET4MESSAGE *)list4mutexRemove( &inter->readsRequested ) ;
   if ( mess == 0 )
   {
      error4( 0, e4result, E96959 ) ;
      return ;
   }
   sig = (SIGNAL4ROUTINE *)mem4allocZero( inter->signalRoutineMemory ) ;
   if ( sig == 0 )
   {
      error4( 0, e4memory, E96959 ) ;
      return ;
   }
   eve = code4getEvent( inter->cb );
   // eve was returning a null in at least some cases - due to error - meaning gpf later
   if ( eve == 0 )
      return ;
   rc = signal4routineInit( sig, eve->handle, inter4readCompleted, eve, mess ) ;
   if ( rc )
   {
      error4( 0, rc, E96959 ) ;
      return ;
   }

   CONNECT4BUFFER *connectBuffer = mess->connectThread->connectBuffer ;
   // AS May 23/07 - connectBuffer was null in some cases here...possibly if the connection had since been closed off.  In that case just cancel this...
   if ( connectBuffer == 0 )
      return ;
   connect4lowEnterExclusive( connectBuffer ) ;
   rc = connect4lowReadAsynchronous( connect4bufferLowGet( connectBuffer ), mess, eve, c4getCompress( inter->cb ) ) ;
   connect4lowExitExclusive( connectBuffer ) ;
   if ( rc == r4success )
      SetEvent( eve->handle ) ;

   #ifdef E4ANALYZE
      assert5( mess->inUse == 0 ) ;    // should not be in use now
      mess->inUse = 1 ;    // mark as in use on the signals list
   #endif

   if ( rc >=0 )
   {
      l4add( &mess->connectThread->readSignalsList, sig ) ; /* add to the end of the list */
      if ( sig == l4first( &mess->connectThread->readSignalsList ) ) /* if it is ths first we need to put it on the SIGNAL4ARRAY */
      {
         rc = signal4arrayAdd( &inter->signalArray, sig, 0 ) ;
         if ( rc < 0 )
            error4( 0, rc, E96959 ) ;
      }
      return ;
   }
   l4add( &mess->connectThread->readSignalsList, sig ) ;
   SetEvent( eve->handle ) ;
/*   mess->messageLen = -1 ;
   if ( sig == l4first( &mess->connectThread->readSignalsList ) )
   {
      rc = signal4arrayAdd( &inter->signalArray, sig, 0 ) ;
      if ( rc < 0 )
         error4( 0, rc, E96959 ) ;
   }
   inter4readCompleted( inter, sig );*/
   inter4error( inter, mess->connectThread ) ;
   return ;
}



static void inter4releaseSignal( INTER4 *inter, SIGNAL4ROUTINE *signalRoutine )
{
   #ifdef E4PARM_LOW
      if ( ( inter==0 ) || ( signalRoutine == 0 ) )
      {
         error4( 0, e4parmNull, E96983 ) ;
         return ;
      }
   #endif

   code4freeEvent( inter->cb, (EVENT4 *)signalRoutine->signalStructure ) ;
   if (signalRoutine->inArray)
      signal4arrayRemoveSignal( &inter->signalArray, signalRoutine ) ;
   mem4free( inter->signalRoutineMemory, signalRoutine ) ;
}



void inter4shutdown( INTER4 *inter, SIGNAL4ROUTINE *dummy )
{
/* ERRORS

   Ignore any errors and continue uninitialization.  Do not set the
     error4code( CODE4 ) in case of failure.

   NOTES

   This function gets called when the thread is to be shutdown.

   Usually there would be no active connections left when this function gets
     called ( i.e. code4initUndo() ).

   Call connect4threadInitUndo() for each CONNECT4THREAD being serviced.

   Remove communication thread signals from the signal array:
     signal4arrayRemoveSignal( inter->signalArray,
                               inter->readsRequestedRoutine )
     signal4arrayRemoveSignal( inter->signalArray,
                               inter->writesRequestedRoutine )
     signal4arrayRemoveSignal( inter->signalArray,
                               inter->completesRequestedRoutine )
     signal4arrayRemoveSignal( inter->signalArray,
                               inter->shutdownThreadRoutine )
   Free up the events and semaphores:
     event4initUndo( inter->shutdownThreadEvent )
     semaphore4initUndo( inter->readsRequestedSemaphore )
     semaphore4initUndo( inter->writesRequestedSemaphore )
     semaphore4initUndo( inter->completesRequestedSemaphore )
   Uninitialize the list4mutex's
     list4mutexInitUndo( inter->readsRequested )
     list4mutexInitUndo( inter->writesRequested )
     list4mutexInitUndo( inter->completesRequested )

   If INTER4.finishedShutdownThreadPtr is not NULL, then call event4set() on
     that EVENT4.

   End our thread ( call _endthread() )
*/

   #ifdef E4PARM_LOW
      if ( inter == 0 )
      {
         error4( 0, e4parmNull, E96960 ) ;
         return ;
      }
   #endif

   inter4initUndo( inter ) ;

   if ( inter->finishedShutdownThreadPtr != 0 )
      event4set( inter->finishedShutdownThreadPtr ) ;

   _endthread() ;
}



void inter4shutdownRequest( INTER4 *inter )
{
/* DESCRIPTION

   This function is used to request that the communications thread shut down.

   RETURNS

   This function does not return until the intercommunications thread has
     shut down.

   ERRORS

   Ignore any errors and continue.  If the creation of the EVENT4 fails,
     then continue with the program, and just return without waiting on
     the EVENT4.

   NOTES

   Create an EVENT4 ( local ), and assign it to INTER4.finishedShutdownThreadPtr
   Set the event INTER4.shutdownThreadEvent
   Wait on the EVENT4 ( local )
   Uninitialize the EVENT4
   Return
*/

   int rc ;
   EVENT4 eve ;

   #ifdef E4PARM_LOW
      if ( inter == 0 )
      {
         error4( 0, e4parmNull, E96961 ) ;
         return ;
      }
   #endif

   // AS Dec 30/09 - if the initialization of the inter thread hasn't been completed we don't need to do this (e.g. an error during intialization of the inter thread)
   if ( inter->cb->interThreadHandle == 0 )
      return ;

   rc = event4init( &eve );
   if ( rc )
   {
      error4( 0, e4net, E96961 ) ;
      return ;
   }
   inter->finishedShutdownThreadPtr = &eve ;
   event4set( &inter->shutdownThreadEvent ) ;
  // inter4shutdown( inter, 0 ) ;
   event4wait( &eve, WAIT4EVER ) ;
   event4initUndo( &eve ) ;
   return ;
}



void inter4write( INTER4 *inter, NET4MESSAGE *message, SIGNAL4ROUTINE *signalRoutine )
{
/* PARAMATERS

   inter is the communication thread structure.
   message contains the buffer which is to be written.

   ERRORS

   In case of error, call error4() with a NULL CODE4 ( since one is not
     available ), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.
     Return NULL.

   RETURNS

   r4success means the write completed successfully
   r4pending means the write is pending
   < 0 means an error occurred.

   NOTES

   This function performs an asynchronous writeFile()

   It does the following:
     Obtain CONNECT4LOW which is CONNECT4THREAD.connectLow
     Call connect4lowWriteAsynchronous( CONNECT4LOW, NET4MESSAGE,
       SIGNAL4ROUTINE.signalStructure )
     If result is e4outstanding
       Treat like any other error.  This error should never occur because
         it has already been accounted for by not pending a write when the
         maximum allowable outstanding are already requested.
     If result is any other error:
       Set NET4MESSAGE.messageLen to -1
       Call code4freeEvent( INTER4, EVENT4 )
       Call mem4free( c4, SIGNAL4ROUTINE )
       Call list4mutexRelease( INTER4.writesRequested )
       Call inter4error( INTER4 ) to indicate a connection failed
       return from function.
     Otherwise ( r4success, r4outstanding )
       Add the NET4MESSAGE to the CONNECT4THREAD.writingFileMemoryList.
     If r4success was returned by connect4writeLow():
       Set NET4MESSAGE.messageLen to r4succcess ( to indicate successful write )
       Call inter4writeCompleted( inter, SIGNAL4ROUTINE ) ;
       Return from function.
     If r4outstanding was returned by connect4writeLow():
       Add the SIGNAL4ROUTINE to INTER4.signalArray ( signal4arrayAdd )
       Add the SIGNAL4ROUTINE to CONNECT4THREAD.writingSignalsList ( l4add )
       Return from function.
*/

   #ifdef E4PARM_LOW
      if ( ( inter==0 ) || ( message == 0 ) || ( signalRoutine == 0 ) )
      {
         error4( 0, e4parmNull, E96962 ) ;
         return ;
      }
   #endif

   /* NOTE: cannot check error code because this function is called by the
      intercommunications thread, and the errorCode can be changed by any
      other thread at any time, unrelated to a communications failure
      if ( error4code( inter->cb ) )
      {
         error4( inter->cb, error4code( inter->cb ), E96962 ) ;
         return ;
      }
   */

   CONNECT4THREAD *conThrd = message->connectThread ;
   CONNECT4BUFFER *connectBuffer = conThrd->connectBuffer ;
   connect4lowEnterExclusive( connectBuffer ) ;
   // AS 08/17/99 conLow may be NULL if a connection timeout on connect occurs, in which case, just mark as failed...
   int rc ;
   if ( connect4bufferLowGet( connectBuffer ) == 0 )
      rc = -1 ;
   else
   {
      // AS May 13/02 - communications compression coding
      rc = connect4lowWriteAsynchronous( connect4bufferLowGet( connectBuffer ), message, (EVENT4 *)signalRoutine->signalStructure, c4getCompress( connectBuffer->cb )) ;
   }

   connect4lowExitExclusive( connectBuffer ) ;

   if ( rc < 0 )  // write failed
   {
      message->messageLen = -1 ;
      code4freeEvent( inter->cb, (EVENT4 *)signalRoutine->signalStructure ) ;
      list4mutexAdd( &conThrd->writeMessageCompletedList, message ) ;
      semaphore4release( &conThrd->writeMessageCompletedListSemaphore ) ;
      mem4free( inter->signalRoutineMemory, signalRoutine ) ;
      inter4error( inter, conThrd ) ;
      return ;
   }

   l4add( &conThrd->writingSignalsList, signalRoutine ) ;

   if ( rc == r4success )  // write succeeded and completed (as opposed to being left in a pending state)
   {
      message->messageLen = r4success ;
      inter4writeCompleted( inter, signalRoutine ) ;
      return ;
   }

   // write is left as pending...put an entry into the signal array so that we will be signalled
   // when the write is finished.  Since only the intercommunications thread uses this array
   // and only the intercommunications thread calls current function, we do not need to semaphore
   // the signal array.
   signal4arrayAdd( &inter->signalArray, signalRoutine, 0 ) ;
   return ;
}



void inter4writeCompleted( INTER4 *inter, SIGNAL4ROUTINE *signalRoutine )
{
/* PARAMATERS

   inter is the communication thread structure
   signalRoutine is the signal which was triggerred as having completed a
     write

   ERRORS

   In case of error, call error4() with a NULL CODE4 ( since one is not
     available ), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.

   NOTES

   signalRoutine.data contains the pointer to the NET4MESSAGE structure which
     completed the write.
   The CONNECT4THREAD can be obtained from the NET4MESSAGE structure.

   Call connect4threadWriteCompleted( CONNECT4THREAD, NET4MESSAGE,
      SIGNAL4ROUTINE )

   Check if any elements are on CON4THREAD.writeMessageOutstandingList
     If there are:
       Remove the first element ( NET4MESSAGE )
       initialize the SIGNAL4ROUTINE object by calling:
         signal4routineInit( SIGNAL4ROUTINE, SIGNAL4ROUTINE.signalStructure,
           inter4writeCompleted, &EVENT4, NET4MESSAGE )
       Call inter4write( INTER4, NET4MESSAGE, SIGNAL4ROUTINE )
       return
   Otherwise
     Call code4freeEvent( CODE4, SIGNAL4ROUTINE.signalStructure )
     Call mem4free( c4, SIGNAL4ROUTINE )
     return
*/

   #ifdef E4PARM_LOW
      if ( ( inter == 0 ) || ( signalRoutine == 0 ) )
      {
         error4( 0, e4parmNull, E96963 ) ;
         return ;
      }
   #endif

   /* NOTE: cannot check error code because this function is called by the
      intercommunications thread, and the errorCode can be changed by any
      other thread at any time, unrelated to a communications failure
      if ( error4code( inter->cb ) )
      {
         error4( inter->cb, error4code( inter->cb ), E96963 ) ;
         return ;
      }
   */

   NET4MESSAGE *mess = (NET4MESSAGE *)signalRoutine->data ;
   CONNECT4THREAD *conThrd = mess->connectThread ;
   connect4threadWriteCompleted( conThrd, mess, signalRoutine ) ;

   // AS Mar 10/03 - Might not be a severe error, but it is a communications error.
   // was failing if client terminated in the middle of a large memo retrieval.
   if ( mess->messageLen < 0 && conThrd->connectBuffer->communicationError == 0 )
   {
      conThrd->connectBuffer->communicationError = e4connect ;
   }

   // AS 09/09/99 - not an error if mess->messageLen < 0, this may occur during shutdown procedure
   // if ( mess->messageLen < 0 )
   // {
   //    error4( 0, e4result, E96963 ) ;
   //    return ;
   // }

   // request that the next outstanding message be written...
   NET4MESSAGE *outstandingMessage = (NET4MESSAGE *)l4first( &conThrd->writeMessageOutstandingList ) ;
   if ( outstandingMessage != 0 )
   {
      l4remove( &conThrd->writeMessageOutstandingList, outstandingMessage ) ;
      signal4arrayRemoveSignal( &inter->signalArray, signalRoutine ) ;
      int rc = signal4routineInit( signalRoutine, ( (EVENT4 *)( signalRoutine->signalStructure ) )->handle, inter4writeCompleted, signalRoutine->signalStructure, outstandingMessage ) ;

      if ( rc )
      {
         error4( 0, rc, E96963 ) ;
         return ;
      }

      inter4write( inter, outstandingMessage, signalRoutine );
      return ;
   }

   inter4releaseSignal( inter, signalRoutine ) ;

   return ;
}



void inter4writeRequested( INTER4 *inter, NET4MESSAGE *message )
{
/* ERRORS

   No errors are possible in this module

   NOTES

   This function is called by a worker to thread to request that the
     communications thread be signalled to request a write for the input
     message

   This function does the following:
     Adds the NET4MESSAGE to the INTER4.writeRequested list4mutex.
     Releases the semaphore INTER4.writeRequestedSemaphore
*/

   #ifdef E4PARM_LOW
      if ( ( inter==0 ) || ( message == 0 ) )
      {
         error4( 0, e4parmNull, E96964 ) ;
         return ;
      }
   #endif

   list4mutexAdd( &inter->writesRequested, message ) ;
   semaphore4release( &inter->writesRequestedSemaphore ) ;
}



void inter4writeRequired( INTER4 *inter, SIGNAL4ROUTINE *dummy )
{
/* PARAMATERS

   inter is the communication thread structure.
   dummy is not used, but is required to have consistent paramaters to the
      function pointers for the SIGNAL4ROUTINE.function

   ERRORS

   In case of error, call error4() with a NULL CODE4 ( since one is not
     available ), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.
     Return NULL.

   NOTES

   This function is called when the communications thread has been signaled
     to perform a WriteFile command.

   This function does the following:

   Obtain CONNECT4THREAD which is NET4MESSAGE.connectThread
   Check the number of pending writes.
     Check CONNECT4THREAD.writeMessagesOutstandingList, if there are links on this
       list or if CONNECT4THREAD.writingSignalsList is equal to S4MAX_WRITES:
       ( then we have already the maximum # of writes pending, so ):
         In this case, it would be a waste of time to try to request a write
         since if the maximum allowed are already pending, the request will
         simply fail.  When a pending write is finished, it automatically
         attempts to schedule another write from this list.
       Therefore, add the NET4MESSAGE to:
         CONNECT4THREAD.outstandingWritesList

     obtain a SIGNAL4ROUTINE object from INTER4.signalRoutineMemory
     obtain an EVENT4 object by calling code4getEvent() ;
     initialize the SIGNAL4ROUTINE object by calling:
       signal4routineInit( SIGNAL4ROUTINE, EVENT4.handle,
         inter4writeCompleted, &EVENT4, NET4MESSAGE )
     Remove the first NET4MESSAGE from INTER4.writesRequested
     Call inter4write( INTER4, NET4MESSAGE, SIGNAL4ROUTINE )
*/

   #ifdef E4PARM_LOW
      if ( inter == 0 )
      {
         error4( 0, e4parmNull, E96965 ) ;
         return ;
      }
   #endif

   NET4MESSAGE *message = (NET4MESSAGE *)list4mutexRemove( &inter->writesRequested ) ;
   if ( message == 0 )
   {
      error4( 0, e4result, E96965 ) ;
      return ;
   }

   CONNECT4THREAD *conThrd = message->connectThread ;
   // If there is already a message on the 'outstanding' list (i.e. there is a pending
   // write already), then just add this message to the list of outstanding messages
   // to write.
   NET4MESSAGE *messOutstanding = (NET4MESSAGE *)l4first( &conThrd->writeMessageOutstandingList ) ;

   if ( messOutstanding != 0 )
   {
      l4add( &conThrd->writeMessageOutstandingList, message ) ;
      return ;
   }

   // if the # of to-write messages sent to the system exceeds or == maximum, just put our
   // message on the outstanding list for later writing
   if ( l4numNodes( &conThrd->writingSignalsList ) >=S4MAX_WRITES )
   {
      l4add( &conThrd->writeMessageOutstandingList, message ) ;
      return ;
   }

   // we are allowed to write the message out, so set it up and do so.
   SIGNAL4ROUTINE *sig = (SIGNAL4ROUTINE *)mem4allocZero( inter->signalRoutineMemory ) ;
   EVENT4 *eve = code4getEvent( inter->cb ) ;
   signal4routineInit( sig, eve->handle, inter4writeCompleted, eve, message ) ;
   inter4write( inter, message, sig ) ;
}
#endif /*!S4OFF_THREAD */
#endif /* not S4OFF_COMMUNICATIONS */
#endif /*!S4STAND_ALONE */
