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
#ifdef S4CLIENT
#ifndef S4OFF_BLAST
int blast4testWrite( CODE4 *c4, long numBytes )
{
   CONNECT4BUFFER *connectBuffer ;
   void *data ;
   int rc ;
   long len = numBytes ;
   short id, type = htons5(STREAM4BLAST_TEST_WRITE) ;

   data = u4alloc(numBytes) ;
   if (!data)
      return error4(c4, e4memory, E96980 ) ;
   connectBuffer = connect4bufferAuxConnectionGet(&c4->clientConnect, 0, 0, 1+((int)(numBytes / c4->writeMessageBufferLen)) ) ;
   connect4send(&c4->clientConnect, &type, sizeof(short) ) ;
   len = htonl5(numBytes) ;
   connect4send(&c4->clientConnect, &len, sizeof(S4LONG) ) ;
   id = htons5(connectBuffer->id) ;
   connect4send(&c4->clientConnect, &id, sizeof(short) ) ;
   connect4sendFlush(&c4->clientConnect ) ;
   rc = connect4bufferSend(connectBuffer, data, numBytes ) ;
   if (rc < 0)
   {
      u4free(data) ;
      return rc ;
   }
   rc = connect4bufferSendFlush(connectBuffer) ;
   u4free(data) ;
   connect4bufferAuxConnectionPut(connectBuffer, &c4->clientConnect ) ;
   return rc ;
}


int blast4testRead( CODE4 *c4, long numBytes )
{
   CONNECT4BUFFER *connectBuffer ;
   void *data ;
   int rc ;
   S4LONG left, bufLen, len ;
   short id, type = htons5(STREAM4BLAST_TEST_READ) ;

   data = u4alloc(bufLen = c4->readMessageBufferLen) ;
   if (!data)
      return error4(c4, e4memory, E96981 ) ;
   connectBuffer = connect4bufferAuxConnectionGet(&c4->clientConnect, c4->readMessageNumBuffers, c4->readMessageBufferLen, 0 ) ;
   connect4send(&c4->clientConnect, &type, sizeof(short) ) ;
   len = htonl5(numBytes) ;
   connect4send(&c4->clientConnect, &len, sizeof(S4LONG) ) ;
   id = htons5(connectBuffer->id) ;
   connect4send(&c4->clientConnect, &id, sizeof(short) ) ;
   connect4sendFlush(&c4->clientConnect ) ;
   left = numBytes ;
   while (left > 0 )
   {
      if (left > bufLen )
      {
         rc = connect4bufferReceive( connectBuffer, data, bufLen, code4timeoutVal(c4)) ;
         if (rc < 0 )
            return error4(c4, rc, E96981 ) ;
         left -= bufLen ;
      }
      else
      {
         rc = connect4bufferReceive( connectBuffer, data, left, code4timeoutVal(c4)) ;
         if (rc < 0 )
            return error4(c4, rc, E96981 ) ;
         left = 0 ;
      }
   }
   u4free(data) ;
   connect4bufferAuxConnectionPut(connectBuffer, &c4->clientConnect ) ;
   return r4success ;
}
#endif /* !S4OFF_BLAST */
#endif
