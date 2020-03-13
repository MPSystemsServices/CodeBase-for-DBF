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

/* c4comws.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef C4COMWS_INC
#define C4COMWS_INC

#ifndef S4OFF_COMMUNICATIONS
#ifdef S4WINSOCK

#define WINSOCK_VERSION           257     /* Version 1.1 */
#define WS4TCP_HEADER_SIZE         42     /* ??? */
#define WS4BUF_SIZE              (8192-WS4TCP_HEADER_SIZE)
#define WS4MAX_PENDING_CONNECTS     4
#define WS4NO_FLAGS_SET             0
#define DUMMY_MESSAGE             567
#define WS4RECV_TIMEOUT         10000     /* msecs */
#define WS4ACCEPT_TIMEOUT        1000     /* msecs */

/* For now always multi-home
#define S4MULTIHOME             "Multi-Homed"
*/

#ifdef S4JAVA
   #define S4CLIENT_UNKNOWN   0
   #define S4CLIENT_JAVA      1
   #define S4CLIENT_OTHER     2
#endif

#ifdef E4OFF_STRING
   #define S4SOCK_BASE_ERROR_NUM   88000L
#else
   #define S4SOCK_BASE_ERROR_DEF   E88004
#endif

#ifdef S4USE_LOW_MEMORY
   #define s4real( fixedMem ) ( (void *)MAKELP(fixedMem.ptr.real, 0) )
   #define s4protected( fixedMem ) ( (void *)MAKELP(fixedMem.ptr.protect, 0) )
#else
   #define s4real( fixedMem ) ( fixedMem )
   #define s4protected( fixedMem ) ( fixedMem )
#endif

#ifndef S4LOCAL
typedef struct  SOCKET4NETSt
{
   unsigned int             listensock;
   struct CONNECTION4NETSt  *connect;
   SOCKET4                  *socket4;
   CODE4                    *cb;
   #ifdef S4SERVER
      LIST4 netList ;
      int   previousClient;
   #endif
} SOCKET4NET;


typedef struct CONNECTION4NETSt
{
   LINK4         link;
   CODE4        *cb;
   long          connid;
   unsigned int  sock;
   SOCKET4NET   *socket4net;
   char          connected;
   char          hostaddr[20];
#ifdef S4JAVA
   char          clientType;
#endif
} CONNECTION4NET;

#endif /* S4LOCAL */

#ifdef __cplusplus
   extern "C" {
#endif

#ifdef E4OFF_STRING
   #define S4WSAERROR (S4SOCK_BASE_ERROR_NUM-WSABASEERR+WSAGetLastError())
#else
   #define S4WSAERROR s4WSAError()

   long S4FUNCTION s4WSAError(void) ;
#endif

#ifdef __cplusplus
   }
#endif

#endif /* S4WINSOCK */
#endif /* S4OFF_COMMUNICATIONS */

#endif /* C4COMWS_INC */
