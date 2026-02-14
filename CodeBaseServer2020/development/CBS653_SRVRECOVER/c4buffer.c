#include "d4all.h"

#if !defined(S4STAND_ALONE) && !defined(S4OFF_COMMUNICATIONS)
   #ifdef S4TIMEOUT_HOOK
      #include <sys\timeb.h>
   #endif

   #ifdef S4COM_PRINT
      /*********************  Debug Display Functions ****************/
      #ifdef S4WINDOWS
         void debug4displayInit( D4DISPLAY *disp, HWND h )
         {
            HDC hdc ;
            TEXTMETRIC tmLocal ;

            memset( disp, 0, sizeof(D4DISPLAY) ) ;
            disp->hWnd = h ;
            disp->lpmsg = &(disp->msg) ;
            hdc = GetDC(disp->hWnd) ;
            GetTextMetrics( hdc, &tmLocal ) ;
            disp->tm = tmLocal ;
            ReleaseDC(disp->hWnd,hdc) ;
            disp->x = 0 ;
            disp->y = 0 ;
            disp->didClose = 0 ;
            disp->didQuit  = 0 ;
         }
      #endif

      void S4FUNCTION debug4display( const char *str )
      {
         int len ;
         char blankLine[180] ;
         #ifdef S4WINDOWS
            D4DISPLAY *disp = &debugDisplay ;
            RECT rect ;
            HDC hdc ;
            int height, width ;
            #ifdef S4WIN32
               SIZE dword;
            #else
               DWORD dword ;
            #endif
         #endif

         memset(blankLine, 32, 179) ;
         blankLine[179] = '\0' ;

         if ( str == 0 )
            return ;
         len = strlen(str) ;

         #ifdef S4WINDOWS
            hdc = GetDC(disp->hWnd) ;
            #ifndef S4WIN32
               dword = GetTextExtent( hdc, str, len ) ;
               height = HIWORD( dword ) ;
               width  = LOWORD( dword ) ;
            #else
               GetTextExtentPoint32( hdc, str, len, &dword ) ;
               height = dword.cy ;
               width  = dword.cx ;
            #endif

            if( height > 0 )
            {
               disp->x = 0 ;
               disp->y += height + disp->tm.tmExternalLeading ;
            }
            GetClientRect( disp->hWnd, &rect ) ;

            if ( (disp->y+height) > rect.bottom )
            {
               disp->y = 0 ;
               InvalidateRect( disp->hWnd, &rect, 1 ) ;
            }

            TextOut( hdc, disp->x,disp->y, str, len ) ;

            if ( (disp->y+(2*height)) > rect.bottom )
               TextOut( hdc, 0, 0, blankLine, strlen(blankLine) ) ;
            else
               TextOut( hdc, 0, (disp->y+height+disp->tm.tmExternalLeading), blankLine, strlen(blankLine) ) ;

            disp->x += width ;

            ReleaseDC(disp->hWnd,hdc) ;
         #endif

         #ifdef S4TESTING
            u4writeErr( str, 1 ) ;
         #endif
      }
   #endif /* S4COM_PRINT */

   /* AS 03/19/98
      Change to design and implementation.
      CODE4.errorCode does not cause communications failures any more.
      the CONNECT4BUFFER contains a member communicationError which causes
      the communications to fail if < 0.

      The reason is that it is legal for the server to send communications to
      the client when the CODE4 is in an error state.  Usually this means a
      client error, and not a real problem.

      When we get a failure from the low level communications, both the code4
      and the communicationError are set to error states.

      The exception to this rule are the various set-up connections functions,
      which can not set up connections when in an error-state.
   */



   #ifndef S4OFF_BLAST
      #ifdef S4CLIENT
         CONNECT4BUFFER *connect4bufferAuxConnection( CONNECT4 *connect, int numAdvanceReadBuffers, int bufSize, int maxOutstandingWrites )
         {
         /* DESCRIPTION

            THis function creates a new connection between an already existing client/
              server pair. This connection can then be used for blast functions.

            PARAMATERS

            connect is a CONNECT4 that already has a write connection
            The remaining parameters are a the qualifications neccesary for
              this connection. NOTE: This can be either a read-only or write-only
              connection, but not both.

            ERRORS

            If error4code( CODE4 ) is < 0 this function should just return 0
            Any failure should cause an error4( ) to be generated and the function to
              return 0.  Any initialization which was performed must be undone
              before returning 0.

            RETURNS

            0 - a failure occurred.  Check error4code( CODE4 ) to discover which
              error
            NON-0 - a valid connected CONNECT4 is returned to the caller
         */

            CODE4 *c4 ;
            CONNECT4BUFFER *connectBuffer ;
            int rc ;

            #ifdef E4PARM_LOW
               if ( !connect )
               {
                  error4( 0, e4parmNull, E96914 ) ;
                  return 0 ;
               }
               if ( ( ( numAdvanceReadBuffers > 0 ) && ( maxOutstandingWrites > 0 ) ) || ( ( numAdvanceReadBuffers == 0 ) && ( maxOutstandingWrites == 0 ) ) )
               {
                  error4( connect->cb, e4parmNull, E96914 ) ;
                  return 0 ;
               }
            #endif

            c4 = connect->cb ;

            if ( error4code( c4 ) )
               return 0 ;

            if ( c4->connectBufferMemory == 0 )
               c4->connectBufferMemory = mem4create( c4, MEMORY4START_CONNECT_BUFFER, sizeof( CONNECT4BUFFER ), MEMORY4EXPAND_CONNECT_BUFFER, 0 ) ;

            connectBuffer = ( CONNECT4BUFFER * )mem4allocZero( c4->connectBufferMemory ) ;
            if ( connectBuffer == 0 )
            {
               error4( c4, e4memory, E96914 ) ;
               return 0 ;
            }
            rc = connect4bufferInit( connectBuffer, connect, 0, numAdvanceReadBuffers, bufSize, maxOutstandingWrites ) ;
            if ( rc < 0 )
            {
               connect4bufferInitUndo( connectBuffer ) ;
               mem4free( c4->connectBufferMemory, connectBuffer );
               error4( c4, rc, E86902 ) ;
               return 0 ;
            }
            connect4lowBlast( connectBuffer->connectLow ) ;
            return connectBuffer ;
         }



         CONNECT4BUFFER *connect4bufferAuxConnectionGet( CONNECT4 *connect, int numAdvanceReadBuffers, int bufSize, int maxOutstandingWrites )
         {
         /* DESCRIPTION

            This function  returns a connection between the Client and Server. It
              does this by first checking if there is an unused connection it can
              use, and if not, creates a new one.

            PARAMATERS

            connect is a CONNECT4 that already has a write connection
            The remaining parameters are a the qualifications neccesary for
              this connection. NOTE: This can be either a read-only or write-only
              connection, but not both.

            ERRORS

            If error4code( CODE4 ) is < 0 this function should just return 0
            Any failure should cause an error4( ) to be generated and the function to
              return 0.  Any initialization which was performed must be undone
              before returning 0.

            RETURNS

            0 - a failure occurred.  Check error4code( CODE4 ) to discover which
              error
            NON-0 - a valid connected CONNECT4 is returned to the caller
         */
            CONNECT4BUFFER *connectBuffer = 0;

            #ifdef E4PARM_LOW
               if ( !connect )
               {
                  error4( 0, e4parmNull, E96915 ) ;
                  return 0 ;
               }
               if ( ( ( numAdvanceReadBuffers > 0 ) && ( maxOutstandingWrites > 0 ) ) || ( ( numAdvanceReadBuffers == 0 ) && ( maxOutstandingWrites == 0 ) ) )
               {
                  error4( connect->cb, e4parmNull, E96915 ) ;
                  return 0 ;
               }
            #endif

            if ( error4code( connect->cb ) )
               return 0 ;

            connectBuffer = ( CONNECT4BUFFER * )l4first( &connect->blastList ) ;
            while ( connectBuffer )
            {
               if ( connectBuffer->type > 0 )
               {
                  if ( maxOutstandingWrites > 0 )
                  {
                     l4remove( &connect->blastList, connectBuffer ) ;
                     connectBuffer->maxWritesOutstanding = maxOutstandingWrites ;
                     return connectBuffer ;
                  }
               }
               else        /* connectBuffer->type < 0 */
               {
                  if ( ( connectBuffer->advanceReads == numAdvanceReadBuffers ) &&
                    ( connectBuffer->readSize == bufSize ) )
                  {
                     l4remove( &connect->blastList, connectBuffer ) ;
                     return connectBuffer ;
                  }
               }
               connectBuffer = ( CONNECT4BUFFER * )l4next( &connect->blastList, ( LINK4 * )connectBuffer );
            }
            return connect4bufferAuxConnection( connect, numAdvanceReadBuffers, bufSize, maxOutstandingWrites ) ;
         }
      #endif /* S4CLIENT */



      void connect4bufferAuxConnectionPut( CONNECT4BUFFER *connectBuffer, CONNECT4 *connect )
      {
      /* DESCRIPTION

         This function is intended to be called when a CONNECT4BUFFER is no
           longer needed for blast functions. It is then returned to the list
           and can be picked out of when a new connection is needed */

         l4add( &connect->blastList, connectBuffer ) ;
      }



      #ifdef S4SERVER
         CONNECT4BUFFER *connect4bufferAuxConnectionSpecific( CONNECT4 *connect, short id )
         {
         /*
            DESCRIPTION

            This function is used by the server. When a client requests that a
              connection is to be reused, it passes the id number of that connection.
              This function takes that id and uses it to find the correct connection.
              It then takes it off the list, and returns it
         */

            CONNECT4BUFFER *connectBuffer = 0 ;

            #ifdef E4PARM_LOW
               if ( !connect )
               {
                  error4( 0, e4parmNull, E96916 ) ;
                  return 0 ;
               }
               if ( !id )
               {
                  error4( connect->cb, e4parmNull, E96916 ) ;
                  return 0 ;
               }
            #endif

            if ( connectBuffer->communicationError < 0 )
               return 0 ;

            do
            {
               connectBuffer = ( CONNECT4BUFFER * )l4next( &connect->blastList, connectBuffer ) ;
               if ( connectBuffer == 0 )
                  return 0 ;
            } while ( connectBuffer->id != id ) ;

            l4remove( &connect->blastList, connectBuffer ) ;
            return connectBuffer ;
         }
      #endif /* S4SERVER */
   #endif /* !S4OFF_BLAST */



   #ifdef S4CLIENT
      int connect4bufferConnect( CONNECT4BUFFER *connectBuffer, CONNECT4 *connect )
      {
      /* DESCRIPTION

         THis function is used to create the complementary socket on a typical
           connection. It can also be used to create the single socket
           connection that is used by the blast functions.

         PARAMATERS

         connectBuffer is the structure we want the connection to be in. If its
           type indicates its going to be a one-way connection, a connect4low is
           created for it. Otherwise, it uses the existing connect4low.
         connect is a CONNECT4 that already has a write connection so that
           messages can be sent along it to get the server to try to connect
           to the client.

         ERRORS

            If an error occurs, just return the error code.

         RETURNS

         <0 - a failure occurred.  Check error4code( CODE4 ) to discover which
           error
         r4success - The connection was successfu;
      */

         CONNECT4LOW *connectLow ;
         // C4NETID netID ;
         CODE4 *c4 ;
         // int rc ;
         // short port ;
         // short addrLen, temp ;
         short messageType ;
         short reconnection = 0 ;

         #ifdef E4PARM_LOW
            if ( ( !connect ) || ( !connectBuffer ) )
               return e4parmNull ;
         #endif

         c4 = connect->cb ;
         if ( error4code( c4 ) < 0 )
            return e4codeBase ;

         #ifndef S4OFF_BLAST
            short conn = connectBuffer->type ;
            if ( conn == 0 )
               connectLow = connectBuffer->connectLow ;
            else
            {
               connectBuffer->id = ++connect->numBlasts ;
               reconnection = htons5( ( short )( ( -conn ) * ( connectBuffer->id ) ) ) ;
               connectLow = ( CONNECT4LOW * )mem4allocZero( c4->connectLowMemory ) ;
               if ( connectLow == 0 )
               {
                  connectBuffer->communicationError = e4connection ;
                  return error4( c4, e4memory, E96917 ) ;
               }
               connectBuffer->connectLow = connectLow ;
            }
         #else
            connectLow = connectBuffer->connectLow ;
         #endif
         connectBuffer->connectLow = connectLow ;

         // AS 03/08/01 - implemented new client/server code so that client performs both connections
         // to avoid firewall problem.

         #ifdef S4OLD_CODE
            #if defined(S4COM_PRINT) && defined(S4CLIENT) && defined(S4UNIX)
               printf( "Sending Message:  STREAM4RECONNECT\n" ) ;
            #endif

            messageType = htons5( STREAM4RECONNECT ) ;
            connect4send( connect, &messageType, sizeof( short ) ) ;
            connect4send( connect, &reconnection, sizeof( short ) ) ;

            CONNECT4LOW listenSocket ;
            memset( &listenSocket, 0, sizeof( listenSocket ) ) ;  /* AS 07/09/98 was not getting initialized, causing random problems later */
            // listenSocket->useDualSocket = c4->useDualSocket ;
            // temp = htons5( c4->useDualSocket ) ;
            // connect4send( connect, &temp, sizeof( short ) ) ;

            // AS 02/06/01 - set up an option to try using the same socket to connect back...
            port = -1 ;
            rc = connect4lowListen( &listenSocket, c4, &port, 1 ) ;
            if ( rc < 0 )
            {
               connectBuffer->communicationError = e4connection ;
               return error4( c4, e4result, E96908 ) ;
            }

            rc = address4getLocalNetID( &netID, &addrLen, connect->connectBuffer.connectLow ) ;
            if ( rc < 0 )
            {
               connectBuffer->communicationError = e4connection ;
               return error4( c4, rc, E96918 ) ;
            }

            temp = htons5( sizeof( short ) ) ;
            connect4send( connect, &temp, sizeof( short ) ) ;
            connect4send( connect, &port, sizeof( short ) ) ;
            temp = htons5( addrLen ) ;
            connect4send( connect, &temp, sizeof( short ) ) ;
            connect4send( connect, &netID, addrLen );
            connect4sendFlush( connect ) ;
            if ( connect4lowAcceptWait( &listenSocket, connectLow, code4timeoutVal( c4 ) ) < 0 )
            {
               connect4lowClose( &listenSocket ) ;
               connect4lowClose( connectLow ) ;
               if ( connectLow == connectBuffer->connectLow )
                  connectBuffer->connectLow = 0 ;
               mem4free( c4->connectLowMemory, connectLow ) ;
               return e4result ;
            }
            connect4lowClose( &listenSocket ) ;
         #else
            #if defined(S4COM_PRINT) && defined(S4CLIENT) && defined(S4UNIX)
               printf( "Sending Message:  STREAM4RECONNECT_NEW\n" ) ;
            #endif

            messageType = htons5( STREAM4RECONNECT_NEW ) ;
            connect4send( connect, &messageType, sizeof( short ) ) ;
            unsigned short portNo = htons5( connect->connectBuffer.connectLow->addressw.sin_port ) ;
            connect4send( connect, &portNo, sizeof( short ) ) ;

            connect4sendFlush( connect ) ;
            connect4reconnect( connect, c4 ) ;

            // writeShort((short)write.getLocalPort());
            // short minusOne = htons5( -1 ) ;

            // connect4sendShort( connect, minusOne );

            // read = new Socket(host, port);
            // DataOutputStream tempOut = new DataOutputStream(read.getOutputStream());
            // tempOut.writeInt(-1);
            // tempOut.writeShort((short)write.getLocalPort());
            // dataInStream = new DataInputStream(read.getInputStream());
            // short rc;
            // rc = readShort();
            // if (rc != Status4.SUCCESS)
            //    throw new Error4unexpected( rc ) ;

         #endif

         connectBuffer->connected++ ;
         return connectBuffer->communicationError ;
      }
   #endif  /*S4CLIENT*/



   void connect4bufferDisconnect( CONNECT4BUFFER *connectBuffer )
   {
      /* ERRORS

         Ignore any errors and continue disconnecting.  Ignore error4code( CODE4 ).

         NOTES

         This function is used to disconnect a CONNECT4BUFFER.

         It calls connect4completeRequest( ) to request the communications thread
           to stop servicing the connect.
      */

      #ifdef E4PARM_LOW
         if ( connectBuffer == 0 )
         {
            error4( 0, e4parmNull, E96919 ) ;
            return;
         }
      #endif

      if ( !( connectBuffer->connected ) )
         return ;

      /* 08/18/99 --> in OFF_THREAD case the connection wasn't getting properly closed... */
      #ifdef S4OFF_THREAD
         connect4lowClose( connectBuffer->connectLow ) ;
      #else
         connect4threadCompleteRequest( &connectBuffer->connectThread ) ;

         NET4MESSAGE *message = connect4bufferWriteRetrieve( connectBuffer, 0, 0 ) ;
         while ( message != 0 )
         {
            list4mutexAdd( &connectBuffer->cb->writeBuffersAvail, message ) ;
            message = connect4bufferWriteRetrieve( connectBuffer, 0, 0 ) ;
         }
      #endif
      connectBuffer->connected = 0 ;
   }



   #ifndef S4OFF_THREAD
      NET4MESSAGE *connect4bufferGetWriteBuffer( CONNECT4BUFFER *connectBuffer )
      {
      /* DESCRIPTION

         This function does some searching to try and produce a write buffer for
           use by the caller. It also checks to make sure we don't try pending
           too many reads at once.

         ERRORS

         If error4code( CODE4 ) is < 0 this function should just return 0
         Any failure should cause an error4( ) to be generated and the function to
           return 0.  Any initialization which was performed must be undone
           before returning 0.

         NOTES

         This function is used to obtain a NET4MESSAGE buffer for writing.

         The following is done:

         Call connect4threadWriteRetrieve( ) with doWait false.  If a write buffer is
           available that we previously sent, this will return it to us, and we
           just return it back to the caller. If there are already too many writes being
           attempted, we just stop until a previous one completes.

         If null is returned, check CODE4.writeBuffersAVail, which is a list of
           available write NET4MESSAGE buffers.  If a buffer is available there,
           just return it back to the caller.

         If asynchronous writing is being done and no buffer is available,
           skip through the list of CONNECT4s and call
           connect4threadWriteRetrieve( ) with each of them, to try to obtain a write
           buffer.

         If no buffer is still available, a new one is allocated with
           mem4allocZero( CODE4.writeMemory ), and returned.
      */

         CONNECT4BUFFER *conBuf ;
         LIST4 *list ;
         int total ;

         #ifdef E4PARM_LOW
            if ( connect == 0 )
            {
               error4( 0, e4parmNull, E96920 ) ;
               return 0 ;
            }
         #endif

         if ( connectBuffer->communicationError < 0 )
            return 0 ;

         CODE4 *c4 = connectBuffer->cb ;

         NET4MESSAGE *mess = connect4bufferWriteRetrieve( connectBuffer, 0, 0 ) ;
         if ( mess != 0 )
            return mess ;

         total = l4numNodes( &connectBuffer->connectThread.writingSignalsList ) +
                 l4numNodes( &connectBuffer->connectThread.writeMessageOutstandingList ) ;
         if ( total >= connectBuffer->maxWritesOutstanding )
         {
            mess = connect4bufferWriteRetrieve( connectBuffer, 0, -1 ) ;
            return mess ;
         }

         // if a message is available then return in to caller
         mess = (NET4MESSAGE *)list4mutexRemove( &c4->writeBuffersAvail ) ;
         if ( mess != 0 )
         {
            mess->connectThread = &connectBuffer->connectThread ;
            return mess ;
         }

         // no message available, we need to wait until one is available...
         if ( c4->ver4 == ver4NT )
         {
            list4mutexWait( &c4->connectBufferListMutex ) ;
            list = &c4->connectBufferListMutex.list ;
            conBuf = ( CONNECT4BUFFER * )l4first( list ) ;

            // there is a message here, let's return it
            while( conBuf != 0 )
            {
               mess = connect4bufferWriteRetrieve( conBuf, 1, 0 ) ;
               if ( mess != 0 )
               {
                  mess->connectThread = &connectBuffer->connectThread ;
                  list4mutexRelease( &c4->connectBufferListMutex );
                  return mess ;
               }
               conBuf = ( CONNECT4BUFFER * )l4next( list, conBuf ) ;
            }
            list4mutexRelease( &c4->connectBufferListMutex );
         }

         // could not get an existing message, so create a new one and return that

         if ( c4->writeMemory == 0 )
            c4->writeMemory = mem4create( c4, MEMORY4START_WRITE_MEMORY, ( sizeof(NET4MESSAGE) + c4->writeMessageBufferLen ), MEMORY4EXPAND_WRITE_MEMORY, 0 ) ;

         mess = (NET4MESSAGE *)mem4allocNoZero( c4->writeMemory ) ;  /* don't need to zero, caller sets data */
         if ( mess != 0 )
         {
            mess->connectThread = &connectBuffer->connectThread ;
            mess->messageBufferLen = c4->writeMessageBufferLen ;
            return mess ;
         }
         connectBuffer->communicationError = e4connection ;
         error4( c4, e4memory, E96920 ) ;
         return 0 ;
      }
   #endif /*!S4OFF_THREAD */



   int connect4bufferInit( CONNECT4BUFFER *connectBuffer, CONNECT4 *connect, CONNECT4LOW *connectLow, int numAdvanceReadBuffers, int bufSize, int maxOutstandingWrites )
   {
   /* PARAMATERS

      connectBuffer is the CONNECT4BUFFER to initialize

      ERRORS

      If error4code( CODE4 ) is < 0 this function should just return error
      Any failure should cause an error4( ) to be generated and the function to
        return.  Any initialization which was performed must be undone
        before returning 0.

      NOTES

      Initializes the CONNECT4BUFFER structure:
        Initialized CONNECT4BUFFER.connectThread
        Create the connection ( could be secondary or blast )
        Perform advance reads if needed
   */

      int rc = 0 ;
      CODE4 *c4 ;

      #ifdef E4PARM_LOW
         if ( ( connectBuffer == 0 ) || ( connect == 0 ) || ( connect->cb == 0 ) )
            return ( error4( 0, e4parmNull, E96921 ) ) ;
      #endif

      c4 = connect->cb ;

      if ( error4code( c4 ) < 0 )
      {
         connectBuffer->communicationError = e4connection ;
         return e4codeBase ;
      }

      #ifdef S4CLIENT
      //   for ( int tryConnectIndex = 0 ; tryConnectIndex < CONNECT4MAX_TRY ; tryConnectIndex++ )
      //   {
      //      error4set( c4, 0 ) ;  // set up for re-try...
      #endif
            connectBuffer->connect = connect ;
            connectBuffer->cb = c4 ;
            #ifndef S4OFF_THREAD
               connectBuffer->maxWritesOutstanding = maxOutstandingWrites ;
               connectBuffer->advanceReads = numAdvanceReadBuffers ;
               connectBuffer->readSize = bufSize;
            #endif
            connectBuffer->connectLow = connectLow ;

            #ifndef S4OFF_THREAD
               rc = connect4threadInit( &connectBuffer->connectThread, c4, connectBuffer );
               if ( rc < 0 )
               {
                  connect4bufferInitUndo( connectBuffer ) ;
                  connectBuffer->communicationError = e4connection ;
                  return error4( c4, rc, E96925 ) ;
               }
               if ( maxOutstandingWrites > 0 )
                  if ( numAdvanceReadBuffers > 0 )
                     connectBuffer->type = 0 ;  /* Main connection */
                  else
                     connectBuffer->type = 1 ;  /* Write only connection */
               else
                  connectBuffer->type = -1 ;     /* Read only connection */
            #else
               connectBuffer->workingWriteLen = c4->writeMessageBufferLen ;
               connectBuffer->workingWriteBuffer = (char *)u4alloc( connectBuffer->workingWriteLen ) ;
               if ( connectBuffer->workingWriteBuffer == 0 )
               {
                  connect4bufferInitUndo( connectBuffer ) ;
                  connectBuffer->communicationError = e4connection ;
                  return error4( c4, e4memory, E96921 ) ;
               }
            #endif

            #ifndef S4OFF_THREAD
               if ( connectBuffer->type )/* != 0*/
                  connectBuffer->connected = 0 ;
               else
            #endif
                  connectBuffer->connected = 1 ;

            #ifdef S4CLIENT
               // AS 08/17/99 -- we want to retry the connection again from scratch up to a fixed
               // number of times in case the server is just temporarily overburdened...
               if ( error4code( c4 ) < 0 )
               {
                  connect4bufferInitUndo( connectBuffer ) ;
                  connectBuffer->communicationError = e4connection ;
                  return error4code( c4 ) ;
               }

               rc = connect4bufferConnect( connectBuffer, connect ) ;
               if ( rc < 0 )  // non-critical error, just retry connecting to server...
               {
                  connect4bufferInitUndo( connectBuffer ) ;
                  connectBuffer->communicationError = e4connection ;
                  return error4code( c4 ) ;
                  // connect4bufferInitUndo( connectBuffer ) ;
                  // continue ;
               }

      //         break ;   // if here, then made through ok...
            #endif
      #ifdef S4CLIENT
      //    }
      #endif

      if ( rc < 0 )
         return error4( c4, rc, E96921 ) ;

      #ifndef S4OFF_BLAST
         if ( !connectBuffer->type )
            list4mutexAdd( &c4->connectBufferListMutex, connectBuffer ) ;
      #endif
      #ifndef S4OFF_THREAD
      if ( numAdvanceReadBuffers > 0 )
      {
         rc = connect4bufferInitRead( connectBuffer, numAdvanceReadBuffers, bufSize ) ;
         if ( rc < 0 )
         {
            connect4bufferInitUndo( &connect->connectBuffer ) ;
            connectBuffer->communicationError = e4connection ;
            return error4( c4, rc, E96921 ) ;
         }
      }
      #endif
      return r4success ;
   }



   void connect4bufferInitUndo( CONNECT4BUFFER *connectBuffer )
   {
   /* ERRORS

      Ignore any errors and continue uninitializing.  Do not set
        error4code( CODE4 ).  Errors may occur if the other side forcefully
        disconnected.  Ignore error4code( CODE4 )

      NOTES


      Disconnects the connection.
      Cleans up the CONNECT4BUFFER structure.
   */

      #ifdef E4PARM_LOW
         if ( connectBuffer == 0 )
         {
            error4( 0, e4parmNull, E96922 ) ;
            return ;
         }
      #endif

      connect4bufferDisconnect( connectBuffer ) ;

      #ifndef S4OFF_THREAD
         connect4threadInitUndo( &connectBuffer->connectThread ) ;

         if ( !connectBuffer->type )
         {
            if ( ( (LINK4 *)connectBuffer )->n )
               list4mutexRemoveLink( &connectBuffer->cb->connectBufferListMutex, (LINK4 *)connectBuffer ) ;
         }

         #ifdef E4ANALYZE
            if ( connectBuffer->cb != 0 )
               for ( int i = 0 ; i < connectBuffer->cb->readMessageNumBuffers ; i++ )
               {
                  NET4MESSAGE *mess = (NET4MESSAGE *)( ((char *)connectBuffer->messageReadArray) + ( i*( ( sizeof( NET4MESSAGE ) ) + connectBuffer->cb->readMessageBufferLen) ) ) ;
                  // AS it is possible that mess is empty - this happens as part of the disconnect
                  if ( mess == 0 )
                     break ;
                  assert5( mess->inUse == 0 ) ;
               }
         #endif

         u4free( connectBuffer->messageReadArray ) ;
      #else
         u4free( connectBuffer->workingWriteBuffer ) ;
      #endif

      if ( connectBuffer->connectLow != 0 )
      {
         mem4free( connectBuffer->cb->connectLowMemory, connectBuffer->connectLow ) ;
         connectBuffer->connectLow = 0 ;
      }

      #ifndef S4OFF_THREAD
         if ( connectBuffer->workingWriteMessage != 0 )  // free this up if not 0
         {
            list4mutexAdd( &connectBuffer->cb->writeBuffersAvail, connectBuffer->workingWriteMessage ) ;
            connectBuffer->workingWriteMessage = 0 ;
         }

         memset( connectBuffer, 0, sizeof( CONNECT4BUFFER ) ) ;
      #endif
   }



   #ifdef S4SERVER
      // ensure the client is exclusive before registering a server-side error
      int error4isolated( CODE4 *c4, SERVER4CLIENT *client, int errCode, long errCode2 )
      {
         // must have exclusive c4 to ensure error code recorded properly
         code4enterExclusive( c4, client, 1 ) ;
         error4( c4, errCode, errCode2 ) ;
         code4exitExclusive( c4, client ) ;
         return errCode ;
      }
   #else
      #define error4isolated( c4, client, errCode, errCode2 ) error4( (c4), (errCode), (errCode2) )
   #endif



   #ifndef S4OFF_THREAD
      int connect4bufferInitRead( CONNECT4BUFFER *connectBuffer, int numAdvance, int bufSize )
      {
      /* PARAMATERS

         connectBuffer is the CONNECT4BUFFER to initialize

         ERRORS

         If error4code( CODE4 ) is < 0 this function should just return error
         Any failure should cause an error4( ) to be generated and the function to
           return.  Any initialization which was performed must be undone
           before returning 0.

         NOTES

          Allocate CODE4.readMessageNumBuffers NET4MESSAGE buffers with message
             sizes of CODE4.readMessageBufferLen ( in a single allocation )
           Add the NET4MESSAGE structures to CONNECT4BUFFER.buffersAvail
              -actually, this area is never used, so it was removed.
           Place advance reads

         Uses readMessageBufferLen and readMessageNumBuffers to allocate a
           pool of memory to use for NET4MESSAGE structures that are used when
           performing read operations on the connection.  In the future we may
           do some memory optimization by one or all of the following:
           - do not allocate the memory pool until it is actually needed ( i.e.
             the connection may only be used to perform writes )
           - keep a list of unused pools located centrally to reduce the number
             required
           - have some additional pools to increase the number of ReadFiles which
             can be bufferred.
      */

         int size,i ;
         NET4MESSAGE *mess ;
         char *ptr ;

         if ( connectBuffer->communicationError < 0 )
            return -1 ;

         size = numAdvance *( sizeof( NET4MESSAGE ) + bufSize ) ;
         connectBuffer->messageReadArray = (NET4MESSAGE *)u4alloc( size );
         ptr = (char *)connectBuffer->messageReadArray ;
         if ( ptr == 0 )
         {
            connectBuffer->communicationError = e4connection ;
            return ( error4( connectBuffer->cb, e4memory, E96923 ) ) ;
         }
         for ( i = 0; i < numAdvance; i++ )
         {
            mess = (NET4MESSAGE *)( ptr + ( i*( ( sizeof( NET4MESSAGE ) ) +bufSize ) ) ) ;
            mess->messageBufferLen = bufSize ;

            if ( connect4threadReadRequest( &connectBuffer->connectThread, mess ) < 0 )
            {
               connectBuffer->communicationError = e4connection ;
               return -1 ;
            }
         }
         return r4success ;
      }


      NET4MESSAGE *connect4bufferReadRetrieve( CONNECT4BUFFER *connectBuffer, int waitSecs )
      {
      /* DESCRIPTION

         This function is used to retrieve a read message, if one is available.

         PARAMATERS

         waitSecs is the number of seconds to wait if no message is available.
           if set to WAIT4EVER then do not return until a message is available.
           if set to WAIT4NONE then do not wait at all, even ignoring the timeout hook
            (this is used internally to clear out any extra communications data where if
             there is none we just want to return back to the caller)

         ERRORS

         If an error occurs return 0.  set error4code( CODE4 ) to error value.
         Check NET4MESSAGE.messageLen after a successful receive.  If it is '-1',
           then need to shut down the connection.  Set error4code( CODE4 ) to e4connect,
           free up the NET4MESSAGE structure, and return 0.

         RETURNS

         The message retrieved, or 0 if the timeout elapsed

         NOTES

         This function does the following:

         call sema4wait( ) on CONNECT4THREAD.readMessageCompletedListSemaphore,
           inputing the waitSecs.

         if the result of the wait is true, return the first element of the
           list4mutex readMessageCompletedList( ).

         if the result of the wait was false, return 0
      */

         // this function must not require CODE4 exclusivity

         #ifdef E4PARM_LOW
            if ( connectBuffer == 0 )
            {
               error4( 0, e4parmNull, E96924 ) ;
               return 0 ;
            }
         #endif

         if ( connectBuffer->communicationError < 0 )
            return 0 ;

         CODE4 *c4 = connectBuffer->cb ;
         #ifdef S4TIMEOUT_HOOK
            int count, elapsedHundredths ;
            struct timeb origTime, newTime ;
         #endif

         int rc ;

         #if defined( S4TIMEOUT_HOOK ) && !defined( S4SERVER )
            ftime( &origTime) ;
            /* AS 04/01/99 Added coding for S4TIMEOUT_HOOK which was not re-coded with new communications */
            for ( count = 0 ;; count++ )
         #endif
         {  // required for for loop with timout hook
            rc = semaphore4wait( &connectBuffer->connectThread.readMessageCompletedListSemaphore, ( waitSecs == WAIT4NONE ? 0 : waitSecs ) ) ;
            if ( rc < 0 )
            {
               connectBuffer->communicationError = e4connection ;
               error4isolated( c4, connectBuffer->connect->client, rc, E96966 );
               return 0;
            }

            #if defined( S4TIMEOUT_HOOK ) && !defined( S4SERVER )
               if ( rc == 0 && waitSecs != WAIT4NONE )  // means timed out
               {
                  ftime( &newTime ) ;
                  elapsedHundredths = ((newTime.time - origTime.time)*100 ) +  (( (int)newTime.millitm - (int)origTime.millitm ) / 10 ) ;
                  if ( code4timeoutHook( c4, count, elapsedHundredths ) != 0 )  // 0 means retry, anything else means don't wait
                     return 0 ;
               }
               else
                  break ;
            #endif

         }  // required for for loop with timout hook

         if ( rc == 0 )
            return 0 ;

         NET4MESSAGE *mess = (NET4MESSAGE *)list4mutexRemove( &connectBuffer->connectThread.readMessageCompletedList ) ;

         if ( mess == 0 )
         {
            connectBuffer->communicationError = e4connection ;
            error4isolated( c4, connectBuffer->connect->client, e4result, E96966 );
            return 0 ;
         }
         if ( mess->messageLen == -1 )
         {
            #ifdef S4SERVER
               // must have exclusive c4
               // AS 07/19/00 was a case where server shut down while waiting for client,
               // then could not recover because connectBuffer->connect was null below...
               SERVER4CLIENT *client = connectBuffer->connect->client ;
               code4enterExclusive( c4, client, 1 ) ;
            #endif
            connect4lowClose( connectBuffer->connectLow ) ;
            mem4free( c4->connectLowMemory, connectBuffer->connectLow ) ;
            connectBuffer->connectLow = 0 ;
            // AS 08/29/00 - cannot set connected to 0, or connection is not properly uninitialized causing gpf in server - try disconnect instead
            connect4bufferDisconnect( connectBuffer ) ;
            // connectBuffer->connected = 0 ;  // CS 2000/08/22
            connectBuffer->communicationError = e4connection ;
            error4( c4, e4connect, E96966 ) ;
            #ifdef S4SERVER
               code4exitExclusive( c4, client ) ;
            #endif
            return 0 ;
         }
         return mess ;
      }
   #endif /*S4OFF_THREAD */



   #if defined(S4COM_PRINT) && defined(S4COM_PRINT_DATA)
      static void dumpData( void *data, long dataLen )
      {
         char buffer[128] ;
         sprintf(buffer, "Connect4bufferReceive len: %d", dataLen ) ;
         debug4display(buffer) ;
         for ( int i = 0 ; i < dataLen ; i++ )
         {
            sprintf(buffer, "Data Byte: %X", (int)(((char *)data)[i]) ) ;
            debug4display( buffer ) ;
         }
      }
   #else
      #define dumpData( a, b )
   #endif


   int S4FUNCTION connect4bufferReceive( CONNECT4BUFFER *connectBuffer, void *data, long dataLen, int waitSecs )
   {
      /* PARAMATERS

         connectBuffer is the connection on which read data is being requested.
         data is a pointer to the address of where the retrieved data should be
           placed.
         dataLen is the quantity of data to retrieve.
         waitSecs is the number of seconds to wait before returing.  If set to
           WAIT4EVER, then do not return until completed.

         ERRORS

         If error4code( CODE4 ) is < 0 this function should just return error
         Any other failure should cause an error4( ) to be generated and the function
           to return.

         RETURNS

         r4success - data has been retrieved.
         r4timeout - timed out
         < 0 - error

         NOTES:

         This function usually does not return until all the required data has been
           obtained from the socket.  It would make multiple calls to
           connect4threadReadRetrieve( ) and copy the data until all of it was
           retrieved.

         If a call to connect4threadReadRetrieve( ) times out, return r4timeout;
           after call to connect4threadReadRetrieve( ), if return was 0, check
           code4errorCode( ); if zero, then return r4timeout.  If e4connect, the
           the connection has failed.  In that case, call connect4disconnect( )
           and return e4connect.

         This function must verify that the connection is valid ( i.e. that
           CONNECT4.connectThread is non-null ).  If it is null, then return
           e4connect ( we have no connection )

         The only problem arises when too much data is retrieved from
           connect4threadReadRetrieve( ).
           To handle this contingincy a pointer to extra data must be kept and used
           as required.  Two members are added to CONNECT4 for this use:
            NET4MESSAGE *workingReadMessage ; // pointer to partially used message
            long workingReadMessagePos ;      // current position in partial message

         When all of a given NET4MESSAGE structure is finished with,  call
           connect4threadReadRequest( CONNECT4THREAD, NET4MESSAGE )
      */
      #ifdef E4PARM_LOW
         if ( ( connectBuffer == 0 ) || ( data == 0 && dataLen != 0 ) || dataLen < 0 )
            return error4( 0, e4parmNull, E96967 ) ;
      #endif

      if ( dataLen == 0 )
         return r4success ;

      if ( connectBuffer->communicationError < 0 )
         return connectBuffer->communicationError ;

      CODE4 *c4 = connectBuffer->cb ;

      if ( connectBuffer->connectLow == 0 )
      {
         connectBuffer->communicationError = e4connection ;
         return error4isolated( c4, connectBuffer->connect->client, e4connection, E96967 ) ;
      }

      #ifdef S4OFF_THREAD
         int rc = connect4lowRead( connectBuffer->connectLow, (char *)data, dataLen ) ;
         if ( rc < 0 )
         {
            // this function does not require CODE4 exclusivity
            connect4bufferDisconnect( connectBuffer ) ;
            connectBuffer->communicationError = e4connection ;
            return error4isolated( c4, connectBuffer->connect->client, e4net, E96967 ) ;
         }
      #else
         long left = dataLen ;               /* amount that still needs to be read */
         char *ptr = (char *)data ;          /* Where we should write to */
         if ( connectBuffer->workingReadMessage != 0 )
         {
            if ( left >= connectBuffer->workingReadMessage->messageLen - connectBuffer->workingReadMessagePos )
            {
               long read = connectBuffer->workingReadMessage->messageLen - connectBuffer->workingReadMessagePos ;
               memcpy( ptr, connectBuffer->workingReadMessage->message + connectBuffer->workingReadMessagePos, read ) ;

               // this function does not require CODE4 exclusivity
               connect4threadReadRequest( &connectBuffer->connectThread, connectBuffer->workingReadMessage ) ;
               connectBuffer->workingReadMessage = 0 ;
               ptr += read ;
               left -= read ;
            }
            else
            {
               memcpy( ptr, connectBuffer->workingReadMessage->message+connectBuffer->workingReadMessagePos, left ) ;
               connectBuffer->workingReadMessagePos += left ;
               dumpData( data, dataLen ) ;
               return r4success ;
            }
         }

         while ( left > 0 )
         {
            // this function must not require CODE4 exclusivity
            NET4MESSAGE *mess = connect4bufferReadRetrieve( connectBuffer, waitSecs ) ;
            if ( mess == 0 )
            {
               #ifdef S4SERVER
                  /* EA/AS 08/29/97 fix for failures during partial connection */
                  /*  for server, we don't care whether or not a timeout, just disconnect anyway ( i.e. timeout equivalent to connection error for server ) */
                  connectBuffer->communicationError = e4connection ;
                  return e4net ;
               #else
                  if ( error4code( c4 ) == 0 )
                     return r4timeout ;
                  else
                  {
                     // this function does not require CODE4 exclusivity
                     connect4bufferDisconnect( connectBuffer ) ;
                     connectBuffer->communicationError = e4connection ;
                     return error4( c4, e4net, E96967 ) ;
                  }
               #endif
            }
            long read = mess->messageLen ;
            if ( read > left )
            {
               memcpy( ptr, mess->message, left ) ;
               connectBuffer->workingReadMessage = mess ;
               connectBuffer->workingReadMessagePos = left ;
               dumpData( data, dataLen ) ;
               return r4success ;
            }
            memcpy( ptr, mess->message, mess->messageLen ) ;

            // this function does not require CODE4 exclusivity
            connect4threadReadRequest( &connectBuffer->connectThread, mess ) ;
            left -= read ;
            ptr += read ;
         }
      #endif /*!/S4OFF_THREAD*/

      dumpData( data, dataLen ) ;
      return r4success ;
   }



   int S4FUNCTION connect4bufferSend( CONNECT4BUFFER *connectBuffer, const void *data, long dataLen )
   {
   /* ERRORS

      If error4code( CODE4 ) is < 0 this function should just return error
      Any other failure should cause an error4( ) to be generated and the function
        to return.  Any initialization must be undone before returning to caller.

      NOTES

      This function executes asynchronously.

      This function is used to add some data which will be sent on the
        connection.  The input data is copied into an internal buffer.  When
        the internal buffer is full, it is sent accross the connection.  To
        deliver any outstanding data, connect4sendFlush( ) must be called.

      This function uses the follwoing two CONNECT4 messages to keep track of the
        partially filled working send buffer:
         NET4MESSAGE *workingWriteMessage ; // pointer to partially used message
         long workingWriteMessagePos ;    // current position in partial message

      This function calls connect4sendFlush( ) when CONNECT4.workingWriteMessage
        is full and ready to send.

      To obtain a new NET4MESSAGE buffer ( i.e. if CONNECT4.workingWriteMessage is
        0 or if we fill it up and still have more data to send ):
        call connectgetWriteBuffer( CONNECT4 )
   */

      #ifdef E4PARM_LOW
         if ( ( connectBuffer == 0 ) || ( data == 0 && dataLen != 0 ) || dataLen < 0 )
            return error4( 0, e4parmNull, E96968 ) ;
      #endif

      #if defined(S4COM_PRINT) && defined(S4COM_PRINT_DATA)
         char buffer[128] ;
         sprintf(buffer, "Connect4bufferSend len: %d", dataLen ) ;
         debug4display(buffer) ;
         for ( int i = 0 ; i < dataLen ; i++ )
         {
            sprintf(buffer, "Data Byte: %X", (int)(((char *)data)[i]) ) ;
            debug4display( buffer ) ;
         }
      #endif

      /* 03/19/98 AS - changed error handling - in case of CodeBase error, still ok to perform
         communications.  If the socket is in error, then don't perform.  */
      if ( connectBuffer->communicationError < 0 )
         return connectBuffer->communicationError ;

      if ( dataLen == 0 )
         return r4success ;

      long left = dataLen ;
      const char *ptr = ( const char * )data ;

      #ifdef S4OFF_THREAD
         if ( connectBuffer->workingWritePos > 0 )
         {
            long space = connectBuffer->workingWriteLen - connectBuffer->workingWritePos;
            if ( space > dataLen )
            {
               memcpy( connectBuffer->workingWriteBuffer + connectBuffer->workingWritePos, data, left ) ;
               connectBuffer->workingWritePos += dataLen ;
               return r4success ;
            }
            else
            {
               memcpy( connectBuffer->workingWriteBuffer + connectBuffer->workingWritePos, data, space ) ;
               connectBuffer->workingWritePos = connectBuffer->workingWriteLen ;
               int rc = connect4bufferSendFlush( connectBuffer ) ;
               if ( rc < 0 )
                  return rc ;
               connectBuffer->workingWritePos = 0 ;
               left -= space ;
               ptr += space ;
            }
         }
         while ( left > 0 )
         {
            if ( connectBuffer->workingWriteLen > (unsigned long)left )
            {
               memcpy( connectBuffer->workingWriteBuffer, ptr, left ) ;
               connectBuffer->workingWritePos = left ;
               return r4success ;
            }
            memcpy( connectBuffer->workingWriteBuffer, ptr, connectBuffer->workingWriteLen );
            ptr += connectBuffer->workingWriteLen ;
            left -= connectBuffer->workingWriteLen ;
            connectBuffer->workingWritePos = connectBuffer->workingWriteLen;  /* This line may not be needed */
            int rc = connect4bufferSendFlush( connectBuffer ) ;
            if ( rc )
               return rc ;
         }
      #else
         if ( connectBuffer->workingWriteMessage != 0 )
         {
            long space = connectBuffer->workingWriteMessage->messageBufferLen - connectBuffer->workingWriteMessage->messageLen ;
            if ( space > dataLen )
            {
               assert5( dataLen >= 0 ) ;
               memcpy( connectBuffer->workingWriteMessage->message + connectBuffer->workingWriteMessage->messageLen, data, left ) ;
               connectBuffer->workingWriteMessage->messageLen += dataLen ;
               return r4success ;
            }
            else
            {
               memcpy( connectBuffer->workingWriteMessage->message + connectBuffer->workingWriteMessage->messageLen, data, space ) ;
               connectBuffer->workingWriteMessage->messageLen = connectBuffer->workingWriteMessage->messageBufferLen ;
               int rc = connect4bufferSendFlush( connectBuffer ) ;
               if ( rc < 0 )
                  return rc ;
               connectBuffer->workingWriteMessage = 0 ;
               left -= space ;
               ptr += space ;
            }
         }
         while ( left > 0 )
         {
            NET4MESSAGE *mess = connect4bufferGetWriteBuffer( connectBuffer ) ;

            // set the current working message to the message we received...
            assert5( connectBuffer->workingWriteMessage == 0 ) ;
            connectBuffer->workingWriteMessage = mess ;
            if ( mess == 0 )
            {
               connectBuffer->communicationError = e4connection ;
               return error4( connectBuffer->cb, e4memory, E96968 ) ;
            }

            // if there is more than enough room in this message for our info, just copy
            // our info in, and return to the user who will flush it later.
            if ( mess->messageBufferLen > (unsigned long)left )
            {
               memcpy( mess->message, ptr, left ) ;
               mess->messageLen = left ;
               return r4success ;
            }

            // our message is larger than the write message buffer size
            // fill up and send this message buffer, then go back to start of while loop to
            // send the remainder of the message.
            memcpy( mess->message, ptr, mess->messageBufferLen );
            ptr += mess->messageBufferLen ;
            left -= mess->messageBufferLen ;
            mess->messageLen = mess->messageBufferLen ; /* This line may not be needed */
            int rc = connect4bufferSendFlush( connectBuffer ) ;
            if ( rc )
               return rc ;
         }
      #endif

      #ifdef E4ANALYZE
         #ifndef E4DONT_FLUSH
            /* easier to debug if pieces flushed on every send */
            connect4bufferSendFlush( connectBuffer ) ;
         #endif
      #endif

      return r4success ;
   }



   int S4FUNCTION connect4bufferSendLong( CONNECT4BUFFER *connectBuffer, const long info )
   {
   /*
      int rc ;
      #ifndef S4OFF_THREAD
         NET4MESSAGE *mess ;
      #endif

      #ifdef E4PARM_LOW
         if ( ( connectBuffer == 0 ) )
            return error4( 0, e4parmNull, E96969 ) ;
      #endif

      if ( error4code( connectBuffer->cb ) < 0 )
         return error4code( connectBuffer->cb ) ;
   */

      const long data = htonl5( info );

      /* AS 01/05/98 - seems more maintainable to only send in one place, so changed */
      return connect4bufferSend( connectBuffer, &data, sizeof( long ) ) ;
   }



   char S4FUNCTION connect4bufferReceiveChar( CONNECT4BUFFER *connectBuffer, int waitSecs )
   {
      assert5( waitSecs == -1 ) ;  // we don't handle timeouts, so better not be setting to a real value

      char info = 0 ;  // in case of error, return 0 as value...
      connect4bufferReceive( connectBuffer, &info, sizeof( char ), waitSecs ) ;
      return info ;
   }



   short S4FUNCTION connect4bufferReceiveShort( CONNECT4BUFFER *connectBuffer, int waitSecs )
   {
      // AS 04/02/01 - We do handle time outs - it just sticks an error code into CODE4.
      // need this for code4swapLogFile...
      // assert5( waitSecs == -1 ) ;  // we don't handle timeouts, so better not be setting to a real value

      short info = 0 ; // in case of error, return 0 as value...
      int rc = connect4bufferReceive( connectBuffer, &info, sizeof( short ), waitSecs ) ;
      if ( rc != 0 )  // user should check error code in any case, but lets return -1 to help them out
      {
         error4set( connectBuffer->cb, rc ) ;
         return -1 ;
      }
      info = htons5( info ) ;
      return info ;
   }



   long S4FUNCTION connect4bufferReceiveLong( CONNECT4BUFFER *connectBuffer, int waitSecs )
   {
      assert5( waitSecs == -1 ) ;  // we don't handle timeouts, so better not be setting to a real value

      long info = 0 ;  // in case of error, return 0 as value...
      connect4bufferReceive( connectBuffer, &info, sizeof( long ), waitSecs ) ;
      info = htonl5( info ) ;
      return info ;
   }



   char *S4FUNCTION connect4bufferReceiveString( CONNECT4BUFFER *connectBuffer, int waitSecs )
   {
      short len ;
      char *out ;

      assert5( waitSecs == -1 ) ;  // we don't handle timeouts, so better not be setting to a real value
      len = connect4bufferReceiveShort( connectBuffer, waitSecs ) ;

      if ( len == 0 )
         return 0 ;
      else
      {
         out = (char *)u4alloc( len + 1 ) ;
         if ( out != 0 )
            connect4bufferReceive( connectBuffer, out, len, waitSecs ) ;
         out[len] = 0 ;
         return out ;
      }
   }



   WSTR5 *S4FUNCTION connect4bufferReceiveWideString( CONNECT4BUFFER *connectBuffer, int waitSecs )
   {
      /* AS 10/01/98 --> need to be able to send "" strings as valid data, different from a null
         entry */

      assert5( waitSecs == -1 ) ;  // we don't handle timeouts, so better not be setting to a real value
      short len = connect4bufferReceiveShort( connectBuffer, waitSecs ) ;

      if ( len == 0 )
         return 0 ;
      else
      {
         WSTR5 *out = (WSTR5 *)u4alloc( len ) ;
         if ( out != 0 )
            connect4bufferReceive( connectBuffer, (char *)out, len, waitSecs ) ;

         assert5( out[(len/2)-1] == 0 ) ;  // AS 07/18/00 - was not looking in the correct location...

         return out ;
      }
   }



   int S4FUNCTION connect4bufferSendString( CONNECT4BUFFER *connectBuffer, const char *in )
   {
      short len ;
      if ( in == 0 )
         len = 0 ;
      else
         len = strlen( in ) ;

      connect4bufferSendShort( connectBuffer, len ) ;
      return connect4bufferSend( connectBuffer, in, len ) ;
   }



   int S4FUNCTION connect4bufferSendWideString( CONNECT4BUFFER *connectBuffer, const WSTR5 *in )
   {
      /* AS 10/01/98 --> need to be able to send "" strings as valid data, different from a null
         entry */
      short len ;
      if ( in == 0 )
         len = 0 ;
      else
         len = (c4wcslen( in ) + 1 )* 2 ;  /* LY 2001/09/13 : changed from wcslen */

      connect4bufferSendShort( connectBuffer, len ) ;
      return connect4bufferSend( connectBuffer, in, len ) ;
   }



   int S4FUNCTION connect4bufferSendShort( CONNECT4BUFFER *connectBuffer, const short info )
   {
      short data = htons5( info ) ;

      /* AS 01/05/98 - seems more maintainable to only send in one place, so changed */
      return connect4bufferSend( connectBuffer, &data, sizeof( short ) ) ;
   }



   int S4FUNCTION connect4bufferSendChar( CONNECT4BUFFER *connectBuffer, const char info )
   {
      /* AS 01/05/98 - seems more maintainable to only send in one place, so changed */
      return connect4bufferSend( connectBuffer, &info, sizeof( char ) ) ;
   }



   int S4FUNCTION connect4bufferSendFlush( CONNECT4BUFFER *connectBuffer )
   {
   /* ERRORS

      If error4code( CODE4 ) is < 0 this function should just return error
      Any other failure should cause an error4( ) to be generated and the function
        to return.

      NOTES

      This function executes asynchronously.

      This function is used to send the CONNECT4.workingWriteMessage via
        com4threadWriteRequired( ).
   */
      int rc ;
      CODE4 *c4 ;

      #ifdef E4PARM_LOW
         if ( connectBuffer == 0 )
            return error4( 0, e4parmNull, E96971 ) ;
      #endif

      c4 = connectBuffer->cb ;

      #ifdef S4OFF_THREAD
         /* 03/19/98 AS - changed error handling - in case of CodeBase error, still ok to perform
            communications.  If the socket is in error, then don't perform.  */
         if ( connectBuffer->communicationError < 0 )
            return connectBuffer->communicationError ;

         rc = connect4lowWrite( connectBuffer->connectLow, connectBuffer->workingWriteBuffer, connectBuffer->workingWritePos ) ;
         if ( rc )
         {
            connect4bufferDisconnect( connectBuffer ) ;
            connectBuffer->communicationError = e4connection ;
            return error4( c4, e4connection, E96971 ) ;
         }
         connectBuffer->workingWritePos = 0 ;
      #else
         if ( connectBuffer->workingWriteMessage == 0 )
            return r4success ;

         /* 03/19/98 AS - changed error handling - in case of CodeBase error, still ok to perform
            communications.  If the socket is in error, then don't perform.  */
         if ( connectBuffer->communicationError < 0 )
            return connectBuffer->communicationError ;

         if ( c4->ver4 != ver4NT )
         {
            rc = connect4lowWriteSynchronous( connectBuffer->connectLow, connectBuffer->workingWriteMessage ) ;
            if ( rc )
            {
               connect4bufferDisconnect( connectBuffer ) ;
               connectBuffer->communicationError = e4connection ;
               return error4( c4, e4connection, E96971 ) ;
            }
            list4mutexAdd( &c4->writeBuffersAvail, connectBuffer->workingWriteMessage ) ;
         }
         else
            inter4writeRequested( connectBuffer->connectThread.inter, connectBuffer->workingWriteMessage ) ;

         connectBuffer->workingWriteMessage = 0 ;
      #endif
      return r4success ;
   }



   #ifndef S4OFF_THREAD
      NET4MESSAGE *connect4bufferWriteRetrieve( CONNECT4BUFFER *connectBuffer, int obtainForOther, int waitSecs )
      {
      /* PARAMATERS

         connectThread is the CONNECT4THREAD requesting a completed write buffer
         waitSecs is the number of seconds to wait if no message is available.
           if set to WAIT4EVER then do not return until a message is available.

         ERRORS/RETURNS

         NON-0 a valid NET4MESSAGE is returned to the caller
         0 may be returned either if an error occurred or if no message was
           available and doWait was FALSE.  To check for error, examine
           CODE4.error, which gets set by error4( ) being called in the case of
           an error occurring in this function.
         Additional: remove error4( ) from the CODE4 check. It's causing
           problems with connect4disconnect( )

         NOTES

         This function is used to retrieve a NET4MESSAGE structure which was
           performing an asynchronous write.

         If waitSecs is <0, this function will not return until a NET4MESSAGE
           structure is available or the timeout elapses.

         This function does the following:
           Call semaphore4wait( CONNECT4THREAD.writeCompletedSemaphore, waitSecs )
           Remove the first element of CONNECT4THREAD.writeMessageCompletedList
           ( list4mutex ), and returns it to the caller.
      */

         #ifdef E4PARM_LOW
            if ( connectBuffer == 0 )
            {
               error4( 0, e4parmNull, E96972 ) ;
               return 0 ;
            }
         #endif

         if ( ( obtainForOther ) && ( !connectBuffer->canShareWriteBuffers ) )
            return 0 ;

         if ( connectBuffer->communicationError < 0 )
            return 0 ;

         int rc = semaphore4wait( &connectBuffer->connectThread.writeMessageCompletedListSemaphore, waitSecs ) ;

         // AS 08/20/99 --> actually, rc of '1' means success, 0 means not signalled.  Therefore, code
         // was incorrect here...
         // if ( rc != 0 )
         if ( rc != 1 )
         {
            if ( rc < 0 )
            {
               error4( connectBuffer->cb, rc, E96972 ) ;
               connectBuffer->communicationError = e4connection ;
            }
            return 0 ;
         }

         NET4MESSAGE *mess = (NET4MESSAGE *)list4mutexRemove( &connectBuffer->connectThread.writeMessageCompletedList ) ;
         return mess ;
      }
   #endif /*!S4OFF_THREAD */
#endif /* !S4OFF_COMMUNICATIONS && !S4STAND_ALONE */

