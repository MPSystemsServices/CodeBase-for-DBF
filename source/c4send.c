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

/* c4send.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* Functions used for inter-client communications */

#include "d4all.h"

#if defined(S4CLIENT) || defined(S4SERVER)
   #include <sys\timeb.h>
#endif

#ifdef S4CLIENT

PIPE4RECV * S4FUNCTION pipe4recvOpen( CODE4 *c4, const char *pipeName )
{
   // opens a pipe for receiving.  Only 1 user is allowed to open a pipe for receiving
   // return 0, CODE4.errorCode e4open means another client already has the pipe open for receiving
   #ifdef E4PARM_HIGH
      if ( c4 == 0 || pipeName == 0 )
      {
         error4( c4, e4parm_null, E91004 ) ;
         return 0 ;
      }
   #endif

   if ( !c4->defaultServer.connected )
   {
      if ( code4connect( c4, 0, DEF4PROCESS_ID, 0, 0, 0 ) == r4success )
      {
         if ( !c4->defaultServer.connected )
         {
            error4describe( c4, e4connection, E84302, DEF4SERVER_ID, DEF4PROCESS_ID, 0 ) ;
            return 0 ;
         }
      }
      else
      {
         if ( c4->defaultServer.connected )
         {
            connection4initUndo( &c4->defaultServer ) ;
            /* connection4free( c4->defaultServer ) ; */
            c4->defaultServer.connected = 0 ;
         }
         if ( error4code( c4 ) >= 0 )   /* probably r4connected, which should now become an error */
            error4describe( c4, e4connection, E81001, DEF4SERVER_ID, DEF4PROCESS_ID, 0 ) ;
         return 0 ;
      }
   }

   CONNECT4 *connect = &c4->clientConnect ;
   if ( connect->connectBuffer.connected == 0 )  /* not connected */
   {
      error4( c4, e4connect, 88057L ) ;
      return 0 ;
   }

   connect4sendShort( connect, STREAM4CLIENT_PIPE_OPEN_RECV ) ;
   connect4sendString( connect, pipeName ) ;
   connect4sendFlush( connect ) ;
   short status = connect4receiveShort( connect ) ;
   if ( status < 0 )
   {
      if ( c4->errOpen == 0 )
         error4set( c4, r4noOpen ) ;
      else
         error4( c4, status, E91004 ) ;
      return 0 ;
   }

   PIPE4RECV *pipe = (PIPE4RECV *)u4allocFree( c4, sizeof( PIPE4RECV ) ) ;
   if ( pipe == 0 )
      return 0 ;

   pipe->id = connect4receiveLong( connect ) ;
   pipe->c4 = c4 ;
   pipe->timeout = -1 ;

   return pipe ;
} ;



PIPE4SEND * S4FUNCTION pipe4sendOpen( CODE4 *c4, const char *pipeName )
{
   // opens a pipe for sending.  There is no limit on the number of users opening a pipe for sending
   // return 0, CODE4.errorCode e4open means another client already has the pipe open for receiving
   #ifdef E4PARM_HIGH
      if ( c4 == 0 || pipeName == 0 )
      {
         error4( c4, e4parm_null, E91004 ) ;
         return 0 ;
      }
   #endif

   if ( !c4->defaultServer.connected )
   {
      if ( code4connect( c4, 0, DEF4PROCESS_ID, 0, 0, 0 ) == r4success )
      {
         if ( !c4->defaultServer.connected )
         {
            error4describe( c4, e4connection, E84302, DEF4SERVER_ID, DEF4PROCESS_ID, 0 ) ;
            return 0 ;
         }
      }
      else
      {
         if ( c4->defaultServer.connected )
         {
            connection4initUndo( &c4->defaultServer ) ;
            /* connection4free( c4->defaultServer ) ; */
            c4->defaultServer.connected = 0 ;
         }
         if ( error4code( c4 ) >= 0 )   /* probably r4connected, which should now become an error */
            error4describe( c4, e4connection, E81001, DEF4SERVER_ID, DEF4PROCESS_ID, 0 ) ;
         return 0 ;
      }
   }

   CONNECT4 *connect = &c4->clientConnect ;
   if ( connect->connectBuffer.connected == 0 )  /* not connected */
   {
      error4( c4, e4connect, 88057L ) ;
      return 0 ;
   }

   connect4sendShort( connect, STREAM4CLIENT_PIPE_OPEN_SEND ) ;
   connect4sendString( connect, pipeName ) ;
   connect4sendFlush( connect ) ;
   short status = connect4receiveShort( connect ) ;
   if ( status < 0 )
   {
      if ( c4->errOpen == 0 )
         error4set( c4, r4noOpen ) ;
      else
         error4( c4, status, E91004 ) ;
      return 0 ;
   }

   PIPE4SEND *pipe = (PIPE4SEND *)u4allocFree( c4, sizeof( PIPE4SEND ) ) ;
   if ( pipe == 0 )
      return 0 ;

   pipe->id = connect4receiveLong( connect ) ;
   pipe->c4 = c4 ;
   pipe->waitForReceipt = 0 ;
   pipe->timeout = 0 ;

   return pipe ;
} ;



void S4FUNCTION pipe4recvClose( PIPE4RECV *pipe )
{
   #ifdef E4PARM_HIGH
      if ( pipe == 0 )
      {
         error4( 0, e4parm_null, E91004 ) ;
         return ;
      }
   #endif

   // just free internally

   CONNECT4 *connect = &pipe->c4->clientConnect ;
   if ( connect == 0 )  /* not connected */
   {
      error4( pipe->c4, e4connect, 88057L ) ;
      return ;
   }

   connect4sendShort( connect, STREAM4CLIENT_PIPE_CLOSE_RECV ) ;
   connect4sendLong( connect, pipe->id ) ;
   connect4sendFlush( connect ) ;

   u4free( pipe ) ;
}



void S4FUNCTION pipe4sendClose( PIPE4SEND *pipe )
{
   #ifdef E4PARM_HIGH
      if ( pipe == 0 )
      {
         error4( 0, e4parm_null, E91004 ) ;
      }
   #endif

   // CONNECT4 *connect = &pipe->c4->clientConnect ;
   // if ( connect == 0 )  /* not connected */
   // {
   //    error4( pipe->c4, e4connect, 88057L ) ;
   //    return ;
   // }

   // connect4sendShort( connect, STREAM4CLIENT_PIPE_CLOSE_SEND ) ;
   // connect4sendLong( connect, pipe->id ) ;
   // connect4sendFlush( connect ) ;

   u4free( pipe ) ;
}



short S4FUNCTION pipe4sendMessageN( PIPE4SEND *pipe, void *message, unsigned long messageLen )
{
   /*
      DESCRIPTION

      Sends a message from one CodeBase client to another via a pipe
      If the client gets forcefully disconnected while waiting for a receipt on the message, the message send is cancelled.

      PARAMATERS

      pipe:       the PIPE4SEND to the pipe that is doing the send
      message:    the message to send. The message can take any format e.g. text, Unicode, sound, graphics, other binary data.
      msgLen:     the length of message, in bytes.

      RETURNS
         r4success      The message was successfully sent.
                        - the client being sent to is connected and either PIPE4SEND.waitForReceipt is 0, or PIPE4SEND.waitForReceipt
                          is true and the receipient received the message.
         r4timeout      The message was not sent because the timeout period expired before the recipient received the message.
                        This only applies when PIPE4SEND.waitForReceipt is true.
         r4notConnected The message was not sent the connection on the other end is no longer valid.
         <0 - error
   */
   #ifdef E4PARM_HIGH
      if ( pipe == 0 || message == 0 || pipe->waitForReceipt < -1 )
         return error4( 0, e4parm_null, E91004 ) ;
   #endif

   CODE4 *c4 = pipe->c4 ;
   CONNECT4 *connect = &c4->clientConnect ;
   if ( connect == 0 )  /* not connected */
      return error4( c4, e4connect, 88057L ) ;

   connect4sendShort( connect, STREAM4CLIENT_PIPE_SEND ) ;

   connect4sendLong( connect, pipe->id ) ;
   connect4sendShort( connect, pipe->waitForReceipt ) ;
   connect4sendLong( connect, messageLen ) ;
   connect4send( connect, message, messageLen ) ;

   connect4sendFlush( connect ) ;
   short status = connect4receiveShort( connect ) ;

   if ( status < 0 )
      return error4( c4, status, 91004 ) ;
   if ( status == r4notConnected )  // invalid client
      return status ;

   if ( pipe->waitForReceipt != 0 )
   {
      long messageId = connect4receiveLong( connect ) ;
      // must wait for receipt of message.
      // check every 10th of a second for the first 10 seconds, then every second

      long timeout = pipe->timeout ;

      int numChecks = 0 ;

      struct timeb mStart ;
      ftime( &mStart ) ;

      for( ;; )
      {
         connect4sendShort( connect, STREAM4CLIENT_PIPE_SEND_CHECK ) ;
         connect4sendLong( connect, messageId ) ;
         connect4sendFlush( connect ) ;

         // status is one of '<0' error, '0' success, or 'r4timeout' not sent yet
         short status = connect4receiveShort( connect ) ;
         if ( status < 0 )
            return error4( c4, status, 91004 ) ;

         if ( status == 0 )
            return 0 ;

         assert5( status == r4continue ) ;   // the only other valid code from server...

         struct timeb mEnd ;
         ftime( &mEnd ) ;
         double timeAmount = (double)mEnd.time + ((double)mEnd.millitm)/(double)1000 - (double)mStart.time - ((double)mStart.millitm)/(double)1000 ; ;

         if ( timeout != -1 && timeAmount > timeout )
         {
            connect4sendShort( connect, STREAM4CLIENT_PIPE_SEND_CANCEL ) ;
            connect4sendLong( connect, messageId ) ;
            connect4sendFlush( connect ) ;
            return r4timeout ;
         }

         if ( numChecks < 100 )  // for first 10 seconds check every 10th of a second
         {
            u4delayHundredth( 10 ) ;
            numChecks++ ;
         }
         else   // after 10 seconds, check every second
         {
            u4delaySec() ;
         }
      }

   }

   return status ;
}



short S4FUNCTION pipe4sendMessage( PIPE4SEND *pipe, char *message )
{
   #ifdef E4PARM_HIGH
      if ( pipe == 0 || message == 0 )
         return error4( 0, e4parm_null, E91004 ) ;
   #endif

   return pipe4sendMessageN( pipe, message, strlen( message ) + 1 ) ;
}



short S4FUNCTION pipe4recvMessage( PIPE4RECV *pipe, unsigned long *msgLen, void **msg )
{
   /*
      Read the next unread message addressed to this pipe.
      pipe is the PIPE4RECV pointer that corresponds to the pipe connection to the CodeBase Server.
      msgLen points to an unsigned long where the length of the message being RECVd is placed. This parameter can be null.
      msg points to a pointer that will contain the address of the message.  This msg is valid until the next call to code4recvMessage

      returns:
         r4success - a message has been retrieved
         r4timeout - no messages available within timeout period
         e4open - invalid pipe (pipe not registered on server)
         < 0 - error

      NOTES:     S4PTR

      This program checks every 10th of a second to see if a message
      is available for the first 10 seconds, then checks every 1 second.
   */

   #ifdef E4PARM_HIGH
      if ( pipe == 0 || msgLen == 0 || msg == 0 || pipe->timeout < -1 )
         return error4( 0, e4parm_null, E91004 ) ;
   #endif

   *msgLen = 0 ;
   *msg = 0 ;

   CODE4 *c4 = pipe->c4 ;
   CONNECT4 *connect = &c4->clientConnect ;
   if ( connect == 0 )  /* not connected */
      return error4( c4, e4connect, 88057L ) ;

   long timeout = pipe->timeout ;

   int numChecks = 0 ;

   struct timeb mStart ;
   ftime( &mStart ) ;

   for ( ;; )
   {
      connect4sendShort( connect, STREAM4CLIENT_PIPE_RECV ) ;
      connect4sendLong( connect, pipe->id ) ;
      connect4sendFlush( connect ) ;

      short status = connect4receiveShort( connect ) ;
      if ( status < 0 )
         return error4( c4, status, 88057L ) ;
      if ( status != r4timeout )  // r4timeout means no message found
      {
         unsigned long len = connect4receiveLong( connect ) ;

         if ( len != 0 )  // message found
         {
            if ( c4->recvMessageLen < len )  // must reallocate
            {
               if ( c4->recvMessage != 0 )
               {
                  u4free( c4->recvMessage ) ;
                  c4->recvMessage = 0 ;
                  c4->recvMessageLen = 0 ;
               }

               c4->recvMessage = u4allocFree( c4, len ) ;
               if ( c4->recvMessage < 0 )
                  return error4( c4, e4memory, E91004 ) ;
               c4->recvMessageLen = len ;
            }

            int rc = connect4receive( connect, c4->recvMessage, len, code4timeoutVal(c4) ) ;
            if ( rc < 0 )
               return rc ;

            *msg = c4->recvMessage ;
            *msgLen = len ;
            return r4success ;
         }
      }

      // check the timeout status
      if ( timeout == -1 )
      {
         if ( numChecks < 100 )  // for first 10 seconds check every 10th of a second
         {
            u4delayHundredth( 10 ) ;
            numChecks++ ;
         }
         else   // after 10 seconds, check every second
         {
            u4delaySec() ;
         }
      }
      else
      {
         struct timeb mEnd ;
         ftime( &mEnd ) ;
         double timeAmount = (double)mEnd.time + ((double)mEnd.millitm)/(double)1000 - (double)mStart.time - ((double)mStart.millitm)/(double)1000 ; ;
         if ( timeAmount > timeout )
            return r4timeout ;

         if ( numChecks < 100 )  // for first 10 seconds check every 10th of a second
         {
            u4delayHundredth( 10 ) ;
            numChecks++ ;
         }
         else   // after 10 seconds, check every second
         {
            u4delaySec() ;
         }
      }
   }
}

#endif /* S4CLIENT */


#ifdef S4SERVER
   /*
       Message handling on server side:

       When a client sends a message, it is put onto a list of messages to send to the client
       When a client requests a message, the next one is sent down the line

       Need to be able to add message, remove message, cancel message (with id)

   */

#endif
