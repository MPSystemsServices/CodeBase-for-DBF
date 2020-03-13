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

/* c4com2.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */


#ifndef S4OFF_COMMUNICATIONS
   #ifdef E4PARM_LOW
      long packet4len( const PACKET4 *packet )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         return ntohl5(packet->dataLen) ;
      }

      int packet4setLen( PACKET4 *packet, const long int dataLen )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         packet->dataLen = htonl5(dataLen) ;

         return 0 ;
      }

      void packet4setStatus( PACKET4 *packet, int status )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return ;
         }

         packet->status = htons5((short)status) ;
      }

      void packet4setType( PACKET4 *packet, const short type )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return ;
         }

         packet->type = htons5((short)type) ;
      }

      short packet4status( const PACKET4 *packet )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         return (short)(ntohs5(packet->status)) ;
      }

      long packet4errCode2( const PACKET4 *packet )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         return ntohl5(packet->errCode2) ;
      }

      int packet4setErrCode2( PACKET4 *packet, const long code2 )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         packet->errCode2 = htonl5(code2) ;
         return 0 ;
      }

      int packet4type( const PACKET4 *packet )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         return ntohs5(packet->type) ;
      }

      long packet4serverId( const PACKET4 *packet )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return 0 ;
         }
         return ntohl5(packet->serverDataId) ;
      }

      long packet4clientId( const PACKET4 *packet )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return 0 ;
         }

         return ntohl5(packet->clientDataId) ;
      }

      int packet4readLock( const PACKET4 *packet )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         return ntohs5(packet->readLock) ;
      }

      int packet4unlockAuto( const PACKET4 *packet )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         return ntohs5(packet->unlockAuto) ;
      }

      void packet4setClientId( PACKET4 *packet, const long id )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return ;
         }

         packet->clientDataId = htonl5(id) ;
      }

      void packet4setServerId( PACKET4 *packet, const long id )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return ;
         }

         packet->serverDataId = htonl5(id) ;
      }

      void packet4setReadLock( PACKET4 *packet, const int readLock )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return ;
         }

         packet->readLock = htons5((short)readLock) ;
      }

      /*
      void packet4setRequestLockedInfo( PACKET4 *packet, const int val )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return ;
         }

         packet->requestLockedInfo = htons5((short)val) ;
      }

      int packet4requestLockedInfo( const PACKET4 *packet )
      {
         if ( packet == 0 )
            return error4( 0, e4parm_null, E90159 ) ;

         return ntohs5(packet->requestLockedInfo) ;
      }
      */

      void packet4setUnlockAuto( PACKET4 *packet, const int unlockAuto )
      {
         if ( packet == 0 )
         {
            error4( 0, e4parm_null, E90159 ) ;
            return ;
         }

         packet->unlockAuto = htons5((short)unlockAuto) ;
      }

      long connection4clientId( const CONNECTION4 *connection )
      {
         #ifdef E4PARM_LOW
            if ( connection == 0 )
            {
               error4( 0, e4parm_null, E90160 ) ;
               return 0 ;
            }
         #endif

         return packet4clientId( &connection->packet ) ;
      }

      int connection4readLock( const CONNECTION4 *connection )
      {
         if ( connection == 0 )
            return error4( 0, e4parm_null, E90160 ) ;

         return packet4readLock( &connection->packet ) ;
      }

      int connection4unlockAuto( const CONNECTION4 *connection )
      {
         if ( connection == 0 )
            return error4( 0, e4parm_null, E90160 ) ;

         return packet4unlockAuto( &connection->packet ) ;
      }

      long connection4serverId( const CONNECTION4 *connection )
      {
         if ( connection == 0 )
         {
            error4( 0, e4parm_null, E90160 ) ;
            return 0 ;
         }

         return packet4serverId( &connection->packet ) ;
      }

      long int connection4len( const CONNECTION4 *connection )
      {
         if ( connection == 0 )
         {
            error4( 0, e4parm_null, E90160 ) ;
            return 0 ;
         }

         return ( packet4len( &connection->packet ) ) ;
      }

      int connection4setLen( CONNECTION4 *connection, const long int dataLen )
      {
         if ( connection == 0 )
            return error4( 0, e4parm_null, E90160 ) ;

         return packet4setLen( &connection->packet, dataLen ) ;
      }

      /*
      void connection4setRequestLockedInfo( CONNECTION4 *connection, int val )
      {
         if ( connection == 0 )
         {
            error4( 0, e4parm_null, E90160 ) ;
            return ;
         }

         packet4setRequestLockedInfo( &connection->packet, val ) ;
      }
      */

      short connection4status( const CONNECTION4 *connection )
      {
         if ( connection == 0 )
            return error4( 0, e4parm_null, E90160 ) ;

         return packet4status( &connection->packet ) ;
      }

      long connection4errCode2( const CONNECTION4 *connection )
      {
         if ( connection == 0 )
            return error4( 0, e4parm_null, E90160 ) ;

         return packet4errCode2( &connection->packet ) ;
      }

      int connection4type( const CONNECTION4 *connection )
      {
         if ( connection == 0 )
            return error4( 0, e4parm_null, E90160 ) ;

         return packet4type( &connection->packet ) ;
      }
   #endif /* E4PARM_LOW -- i.e. inline */

   #ifdef S4COM_PRINT
      const char *s4connectionPrint( const int type )
      {
         const char *out ;

         switch( type )
         {
            case CON4ADD_TAG:
               out = "CON4ADD_TAG" ;
               break ;
            case CON4APPEND:
               out = "CON4APPEND" ;
               break ;
            case CON4BOTTOM:
               out = "CON4BOTTOM" ;
               break ;
            case CON4CALC_CREATE:
               out = "CON4CALC_CREATE" ;
               break ;
            case CON4CALC_RESET:
               out = "CON4CALC_RESET" ;
               break ;
            case CON4CANCEL:
               out = "CON4CANCEL" ;
               break ;
            case CON4CATALOG:
               out = "CON4CATALOG" ;
               break ;
            case CON4CHECK:
               out = "CON4CHECK" ;
               break ;
            case CON4CLOSE:
               out = "CON4CLOSE" ;
               break ;
            case CON4COMMIT_BOTH_PHASES:
               out = "CON4COMMIT_BOTH_PHASES" ;
               break ;
            case CON4COMMIT_PHASE_ONE:
               out = "CON4COMMIT_PHASE_ONE" ;
               break ;
            case CON4COMMIT_PHASE_TWO:
               out = "CON4COMMIT_PHASE_TWO" ;
               break ;
            case CON4CONFIG_NAME:
               out = "CON4CONFIG_NAME" ;
               break ;
            case CON4CONNECT:
               out = "CON4CONNECT" ;
               break ;
            case CON4CREATE:
               out = "CON4CREATE" ;
               break ;
            case CON4DATA_FNAME:
               out = "CON4DATA_FNAME" ;
               break ;
            case CON4DATE_FORMAT:
               out = "CON4DATE_FORMAT" ;
               break ;
            case STREAM4DISCONNECT:
               out = "STREAM4DISCONNECT" ;
               break ;
            case CON4DISCONNECT:
               out = "CON4DISCONNECT" ;
               break ;
            case CON4GO:
               out = "CON4GO" ;
               break ;
            case CON4INDEX_CLOSE:
               out = "CON4INDEX_CLOSE" ;
               break ;
            case CON4INDEX_CREATE:
               out = "CON4INDEX_CREATE" ;
               break ;
            case CON4INDEX_FNAME:
               out = "CON4INDEX_FNAME" ;
               break ;
            case CON4INDEX_FORMAT:
               out = "CON4INDEX_FORMAT" ;
               break ;
            case CON4INDEX_INFO:
               out = "CON4INDEX_INFO" ;
               break ;
            case CON4INDEX_OPEN:
               out = "CON4INDEX_OPEN" ;
               break ;
            case CON4INDEX_REINDEX:
               out = "CON4INDEX_REINDEX" ;
               break ;
            case CON4INFO:
               out = "CON4INFO" ;
               break ;
            #ifndef S4SINGLE
               case CON4LOCK:
                  out = "CON4LOCK" ;
                  break ;
               case CON4LOCK_GROUP:
                  out = "CON4LOCK_GROUP" ;
                  break ;
            #endif
            case CON4MEMO:
               out = "CON4MEMO" ;
               break ;
            case CON4MEMO_COMPRESS:
               out = "CON4MEMO_COMPRESS" ;
               break ;
            case CON4OPEN:
               out = "CON4OPEN" ;
               break ;
            case CON4PACK:
               out = "CON4PACK" ;
               break ;
            case CON4PASSWORD_SET:
               out = "CON4PASSWORD_SET" ;
               break ;
            case CON4POSITION:
               out = "CON4POSITION" ;
               break ;
            case CON4POSITION_SET:
               out = "CON4POSITION_SET" ;
               break ;
            case CON4RECCOUNT:
               out = "CON4RECCOUNT" ;
               break ;
            case CON4REINDEX:
               out = "CON4REINDEX" ;
               break ;
            case CON4RELATE_BOTTOM:
               out = "CON4RELATE_BOTTOM" ;
               break ;
            case CON4RELATE_DO:
               out = "CON4RELATE_DO" ;
               break ;
            case CON4RELATE_DO_ONE:
               out = "CON4RELATE_DO_ONE" ;
               break ;
            case CON4RELATE_FREE:
               out = "CON4RELATE_FREE" ;
               break ;
            case CON4RELATE_INIT:
               out = "CON4RELATE_INIT" ;
               break ;
            #ifndef S4SINGLE
               case CON4RELATE_LOCK:
                  out = "CON4RELATE_LOCK" ;
                  break ;
            #endif
            case CON4RELATE_OPT:
               out = "CON4RELATE_OPT" ;
               break ;
            case CON4RELATE_SKIP:
               out = "CON4RELATE_SKIP" ;
               break ;
            case CON4RELATE_TOP:
               out = "CON4RELATE_TOP" ;
               break ;
            case CON4REMOVE:
               out = "CON4REMOVE" ;
               break ;
            case CON4RESTART:
               out = "CON4RESTART" ;
               break ;
            case CON4CONNECT_ACCEPT_NEW:
               out = "CON4CONNECT_ACCEPT_NEW" ;
               break ;
            case CON4CONNECT_CUT_ALL:
               out = "CON4CONNECT_CUT_ALL" ;
               break ;
            case CON4CONNECT_CUT:
               out = "CON4CONNECT_CUT" ;
               break ;
            case CON4CLOSE_FILES:
               out = "CON4CLOSE_FILES" ;
               break ;
            case CON4SHUTDOWN:
               out = "CON4SHUTDOWN" ;
               break ;
            case CON4CRASH:
               out = "CON4CRASH" ;
               break ;
            case CON4ROLLBACK:
               out = "CON4ROLLBACK" ;
               break ;
            case CON4SEEK:
               out = "CON4SEEK" ;
               break ;
            case CON4SEEK_DBL:
               out = "CON4SEEK_DBL" ;
               break ;
            case CON4SEEK_LONGLONG:
               out = "CON4SEEK_LONGLONG" ;
               break ;
            case CON4SKIP:
               out = "CON4SKIP" ;
               break ;
            case CON4START:
               out = "CON4START" ;
               break ;
            case CON4TAG_OPEN:
               out = "CON4TAG_OPEN" ;
               break ;
            case CON4TAG_SYNCH:
               out = "CON4TAG_SYNCH" ;
               break ;
            case CON4TOP:
               out = "CON4TOP" ;
               break ;
            case CON4TRAN_EOF:
               out = "CON4TRAN_EOF" ;
               break ;
            case CON4TRAN_EOF_HALT:
               out = "CON4TRAN_EOF_HALT" ;
               break ;
            case CON4UNIQUE_SET:
               out = "CON4UNIQUE_SET" ;
               break ;
            #ifndef S4SINGLE
               case CON4UNLOCK:
                  out = "CON4UNLOCK" ;
                  break ;
            #endif
            case CON4WRITE:
               out = "CON4WRITE" ;
               break ;
            case CON4ZAP:
               out = "CON4ZAP" ;
               break ;
            case STREAM4RECONNECT:
               out = "STREAM4RECONNECT" ;
               break ;
            case STREAM4STATUS:
               out = "STREAM4STATUS" ;
               break ;
            case STREAM4PING:
               out = "STREAM4PING" ;
               break ;
            case STREAM4BLAST_TEST_READ:
               out = "STREAM4BLAST_TEST_READ" ;
               break ;
            case STREAM4BLAST_TEST_WRITE:
               out = "STREAM4BLAST_TEST_WRITE" ;
               break ;
            case STREAM4LOCKED_INFO:
               out = "STREAM4LOCKED_INFO" ;
               break ;
            case STREAM4UNLOCK_ALL:
               out = "STREAM4UNLOCK_ALL" ;
               break ;
            case STREAM4UNLOCK_DATA:
               out = "STREAM4UNLOCK_DATA" ;
               break ;
            case MSG5DB_SCHEMA_REQUEST_SEQ:
               out = "MSG5DB_SCHEMA_REQUEST_SEQ" ;
               break ;
            case MSG5DB_TAG_SELECT:
               out = "MSG5DB_TAG_SELECT" ;
               break ;
            case MSG5DB_ADD_DIRECTORY:
               out = "MSG5DB_ADD_DIRECTORY" ;
               break ;
            case MSG5DB_SET_RESTRICTIONS:
               out = "MSG5DB_SET_RESTRICTIONS" ;
               break ;
            case MSG5DB_CURRENT_DIRECTORY:
               out = "MSG5DB_CURRENT_DIRECTORY" ;
               break ;
            case MSG5DB_COLUMN_INFO:
               out = "MSG5DB_COLUMN_INFO" ;
               break ;
            case  MSG5DB_INDEX_COLUMN_INFO:
               out = "MSG5DB_INDEX_COLUMN_INFO" ;
               break ;
            case MSG5DB_SESSION_DATA_OPEN:
               out = "MSG5DB_SESSION_DATA_OPEN" ;
               break ;
            case MSG5DB_UPDATE_FIELDSET:
               out = "MSG5DB_UPDATE_FIELDSET" ;
               break ;
            case MSG5DB_RECCOUNT:
               out = "MSG5DB_RECCOUNT" ;
               break ;
            case MSG5DB_ROW_REQUEST_SEQUENTIAL:
               out = "MSG5DB_ROW_REQUEST_SEQUENTIAL" ;
               break ;
            case MSG5DB_SESSION_INDEX_OPEN:
               out = "MSG5DB_SESSION_INDEX_OPEN" ;
               break ;
            case MSG5DB_INDEX_UPDATE_FIELDSET:
               out = "MSG5DB_INDEX_UPDATE_FIELDSET" ;
               break ;
            case MSG5DB_INDEX_ROW_REQUEST_KEYS:
               out = "MSG5DB_INDEX_ROW_REQUEST_KEYS" ;
               break ;
            case MSG5DB_INDEX_ROW_REQUEST:
               out = "MSG5DB_INDEX_ROW_REQUEST" ;
               break ;
            case MSG5DB_SESSION_SCHEMA_OPEN:
               out = "MSG5DB_SESSION_SCHEMA_OPEN" ;
               break ;
            #ifdef S4JAVA
               case JAVA4APPEND:
                  out = "JAVA4APPEND" ;
                  break ;
               case JAVA4ACCESS_MODE_SET:
                  out = "JAVA4ACCESS_MODE_SET" ;
                  break ;
               case JAVA4BLANK:
                  out = "JAVA4BLANK" ;
                  break ;
               case JAVA4BOTTOM:
                  out = "JAVA4BOTTOM" ;
                  break ;
               case JAVA4CLOSE:
                  out = "JAVA4CLOSE" ;
                  break ;
               case JAVA4CONNECT:
                  out = "JAVA4CONNECT" ;
                  break ;
               case JAVA4CREATE:
                  out = "JAVA4CREATE" ;
                  break ;
               case JAVA4DEFAULT_UNIQUE_SET:
                  out = "JAVA4DEFAULT_UNIQUE_SET" ;
                  break ;
               case JAVA4DISCONNECT:   /* no return message since client has disconnected */
                  out = "JAVA4DISCONNECT" ;
                  break ;
               case JAVA4GO:
                  out = "JAVA4GO" ;
                  break ;
               case JAVA4INDEX_CREATE:
                  out = "JAVA4INDEX_CREATE" ;
                  break ;
               case JAVA4INDEX_OPEN:
                  out = "JAVA4INDEX_OPEN" ;
                  break ;
               case JAVA4LOCK_GROUP:
                  out = "JAVA4LOCK_GROUP" ;
                  break ;
               case JAVA4OPEN:
                  out = "JAVA4OPEN" ;
                  break ;
               case JAVA4PACK:
                  out = "JAVA4PACK" ;
                  break ;
               case JAVA4POSITION:
                  out = "JAVA4POSITION" ;
                  break ;
               case JAVA4POSITION_SET:
                  out = "JAVA4POSITION_SET" ;
                  break ;
               case JAVA4READ_LOCK_SET:
                  out = "JAVA4READ_LOCK_SET" ;
                  break ;
               case JAVA4READ_ONLY_SET:
                  out = "JAVA4READ_ONLY_SET" ;
                  break ;
               case JAVA4RECCOUNT:
                  out = "JAVA4RECCOUNT" ;
                  break ;
               case JAVA4RECNO:
                  out = "JAVA4RECNO" ;
                  break ;
               case JAVA4REGISTER_FIELD:
                  out = "JAVA4REGISTER_FIELD" ;
                  break ;
               case JAVA4REINDEX:
                  out = "JAVA4REINDEX" ;
                  break ;
               case JAVA4SAFETY_SET:
                  out = "JAVA4SAFETY_SET" ;
                  break ;
               case JAVA4SEEK_DBL:
                  out = "JAVA4SEEK_DBL" ;
                  break ;
               case JAVA4SEEK_N:
                  out = "JAVA4SEEK_N" ;
                  break ;
               case JAVA4SELECT_DATA:
                  out = "JAVA4SELECT_DATA" ;
                  break ;
               case JAVA4SELECT_TAG:
                  out = "JAVA4SELECT_TAG" ;
                  break ;
               case JAVA4SKIP:
                  out = "JAVA4SKIP" ;
                  break ;
               case JAVA4STATUS_CODE:
                  out = "JAVA4STATUS_CODE" ;
                  break ;
               case JAVA4STATUS_FIELDS:
                  out = "JAVA4STATUS_FIELDS" ;
                  break ;
               case JAVA4TOP:
                  out = "JAVA4TOP" ;
                  break ;
               case JAVA4UNLOCK:
                  out = "JAVA4UNLOCK" ;
                  break ;
               case JAVA4UNLOCK_AUTO_SET:
                  out = "JAVA4UNLOCK_AUTO_SET" ;
                  break ;
               case JAVA4WRITE:
                  out = "JAVA4WRITE" ;
                  break ;
            #endif   /* S4JAVA */
            default:
               out = "INVALID MESSAGE" ;
               break ;
         }
         return out ;
         /* printf( "%s\n", out ) ; */
      }
   #endif
#endif /* S4OFF_COMMUNICATIONS */
