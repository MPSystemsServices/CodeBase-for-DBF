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

/* d4info.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4CLIENT
   // LY Jul 28/04 : changed method of returning name, due to memory leak
   int S4FUNCTION code4serverConfigName( CODE4 *c4, char *name )
   {
      CONNECTION4 *connection ;
      int rc, len ;
      // char *name, *ptr ;
      char *ptr ;

      #ifdef E4PARM_LOW
         if ( c4 == 0 )
         {
            return error4( 0, e4parm_null, E96101 ) ;
         }
      #endif

      if ( !c4->defaultServer.connected )
      {
         return error4( c4, e4connection, E84302 ) ;
      }

      #ifdef E4ANALYZE
         if ( c4->defaultServer.connect == 0 )
         {
            return error4( c4, e4parm, E96101 ) ;
         }
      #endif

      connection = &c4->defaultServer ;
      connection4assign( connection, CON4CONFIG_NAME, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return rc ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         connection4error( connection, c4, rc, E96101 ) ;
         return rc ;
      }

      ptr = (char *)connection4data( connection ) ;
      len = strlen( ptr ) ;

      #ifdef E4ANALYZE
         if ( len > LEN4PATH + 1 || len < 0 )
         {
            error4( c4, e4struct, E96101 ) ;
            return 0 ;
         }
      #endif

      memcpy( name, ptr, len+1 ) ;

      return 0 ;
   }



   int S4FUNCTION code4infoRetrieve( CODE4 *c4, S4LONG *memAlloc, unsigned short *nClients, S4LONG *elapsedSeconds, int *nOpenFiles )
   {
      int rc ;
      CONNECTION4SERVER_INFO_OUT *out ;

      #ifdef E4PARM_LOW
         if ( c4 == 0 || memAlloc == 0 || nClients == 0 || elapsedSeconds == 0 || nOpenFiles == 0 )
            return error4( 0, e4parm_null, E96101 ) ;
      #endif

      if ( !c4->defaultServer.connected )
         return error4( c4, e4connection, E84302 ) ;

      #ifdef E4ANALYZE
         if ( c4->defaultServer.connect == 0 )
            return error4( c4, e4parm, E96101 ) ;
      #endif

      CONNECTION4 *connection = &c4->defaultServer ;
      connection4assign( connection, CON4INFO, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96101 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96101 ) ;

      if ( connection4len( connection ) < sizeof( CONNECTION4SERVER_INFO_OUT ) )
         return error4( c4, e4packetLen, E96101 ) ;

      out = (CONNECTION4SERVER_INFO_OUT *)connection4data( connection ) ;

      // CS 2002/07/18 change from hton?5 to ntoh?5
      *memAlloc = ntohl5( out->memAlloc ) ;
      *nClients = ntohs5( out->numClients ) + ntohs5( out->numClientsOdbc ) ;
      *elapsedSeconds = ntohl5( out->elapsedSeconds ) ;
      *nOpenFiles = ntohs5( out->nOpenFiles ) ;

      return 0 ;
   }



   int S4FUNCTION code4info( CODE4 *c4 )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E96101 ) ;
      #endif

      if ( !c4->defaultServer.connected )
         return error4( c4, e4connection, E84302 ) ;

      #ifdef E4ANALYZE
         if ( c4->defaultServer.connect == 0 )
            return error4( c4, e4parm, E96101 ) ;
      #endif

      CONNECTION4 *connection = &c4->defaultServer ;
      connection4assign( connection, CON4INFO, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96101 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96101 ) ;

      if ( connection4len( connection ) < sizeof( CONNECTION4SERVER_INFO_OUT ) )
         return error4( c4, e4packetLen, E96101 ) ;

      #ifdef S4CONSOLE
         CONNECTION4SERVER_INFO_OUT *out = (CONNECTION4SERVER_INFO_OUT *)connection4data( connection ) ;

         printf( "\nSERVER STATS\n------------\n" ) ;

         if ( ntohl5(out->memMax) == -1 )
            printf( "memMax: unknown\n" ) ;
         else
            printf( "memMax: %ld\n", ntohl5(out->memMax) ) ;

         if ( ntohl5(out->memAlloc) == -1 )
            printf( "memAlloc: unknown\n" ) ;
         else
            printf( "memAlloc: %ld\n", ntohl5(out->memAlloc) ) ;
         printf( "numRequests: %ld\n", ntohl5(out->numRequests) ) ;
         unsigned short int numClients = ntohs5(out->numClients) ;
         printf( "numClients: %d\n", numClients + out->numClientsOdbc ) ;
         printf( "elapsedSeconds: %ld\n", (long)ntohl5(out->elapsedSeconds) ) ;

         if ( connection4len( connection ) != (long)(sizeof( CONNECTION4SERVER_INFO_OUT ) + (long)(numClients * sizeof( CONNECTION4CLIENT_INFO )) ) )
            return error4( c4, e4packetLen, E96101 ) ;

         printf( "\nCLIENT STATS\n------------\n" ) ;

         for ( unsigned short clientIndex = 0 ; clientIndex < numClients ; clientIndex++ )
         {
            CONNECTION4CLIENT_INFO *client = (CONNECTION4CLIENT_INFO *)(((char *)out) + sizeof( CONNECTION4SERVER_INFO_OUT ) + clientIndex * sizeof( CONNECTION4CLIENT_INFO ) ) ;

            /* printf( Client Name... */
            printf( "numData: %d\n", ntohs5(client->numData) ) ;
            printf( "numRelate: %d\n", ntohs5(client->numRelate) ) ;
            printf( "numRequests: %ld\n", ntohl5(client->numRequests) ) ;
            printf( "numTransactions: %d\n", ntohl5(client->numTransactions) ) ;
            printf( "numCompletedTransactions: %d\n", ntohl5(client->numCompletedTransactions) ) ;
            printf( "numRollbacks: %d\n", ntohl5(client->numRollbacks) ) ;
            printf( "elapsedSeconds: %ld\n", (long)ntohl5(client->elapsedSeconds) ) ;
            printf( "activeTransaction: %d\n", ntohs5(client->activeTransaction) ) ;
         }

         printf( "-----------------------------------\n\n" ) ;
      #endif

      return 0 ;
   }
#endif /* S4CLIENT */
