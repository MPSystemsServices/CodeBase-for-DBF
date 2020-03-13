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

/* LOG5.CPP used for logging results to a file for testing */

#include "d4all.h"
#ifdef OLEDB5ERROR
   #include "oledb5err.hpp"
#endif

#include "util5.hpp"

#ifdef S4OLEDEBUG_PRINT
   // AS Oct 4/01 - Note neccesarily linking to c4dll - ensure s4dll defined
   #if defined( S4DLL ) && ( defined( OLEDB5BUILD ) || defined( ERROR4LOG ) )
       // linking to c4dll, so use external file4write, not internal file4writeInternal
      #ifndef USE4EXTERNAL
         #define USE4EXTERNAL
      #endif
   #endif

   // to enable, change this value to 0 (in debugger or outside of)
   static int debug4invokedLog = 1 ;

   void log4invokeDebugger()
   {
      // allows for run-time invoking of debugger...
      if ( debug4invokedLog == 1 )  // already in debugger...
         return ;
      static char szBuf[200] ;
      int iMsg ;

      wsprintf( szBuf, "drm4cb debugger invocation, Cancel if debugger already running, No to ignore." ) ;

      iMsg = MessageBox( GetActiveWindow(), szBuf, "Assertion Error", MB_YESNOCANCEL | MB_DEFBUTTON2 | MB_ICONSTOP | MB_TASKMODAL ) ;
      if (IDYES == iMsg)
      {
         /* force an exception to invoke the debugger */
         int i = 0 ;
         i = 1 / i ;   // manually through debugger change i to 1 to continue...
      }
      else if ( IDCANCEL == iMsg )
      {
         DebugBreak() ;
      }

      // always set to 1 - only ask user once if they want to enter into the debugger...
      debug4invokedLog = 1 ;
   }



   int log5enabled = 0 ;

   // to enable logging of queries, set this value to 1 (in debugger or outside of), also, log5enabled must be set to 1
   int log5queryEnabled = 0 ;



   void log5enable()
   {
      log5enabled = 1 ;
   }



   void log5queryEnable()
   {
      log5queryEnabled = 1 ;
      log5enable() ;
   }



   void log5disable()
   {
      log5enabled = 0 ;
   }



   void log5query( const char *ptrToWrite )
   {
      if ( log5queryEnabled == 1 )
         log5( ptrToWrite ) ;
   }



   CODE4 *g_c4 = 0 ;
   FILE4 g_file ;

   void log5closeFile()
   {

      if ( g_c4 == 0 )
         return ;

      file4close( &g_file ) ;

      code4initUndo( g_c4 ) ;
      g_c4 = 0 ;
   }



   static void log5openFile()
   {
      g_c4 = code4alloc( 1 ) ;
      // AS Feb 26/02 - Not taking into account case where code4alloc fails (e.g. version mismatch)
      if ( g_c4 == 0 )
         return ;
      c4setErrOpen( g_c4, 0 ) ;


      // AS Jan 3/01 - If the file already exists just try to create a new one .. never
      // re-use an existing file (Mic. SQL uses multiple processes so it gets confusing if all output
      // goes to a single file)

      char name[255];
      char fileNameStart[] = "c:\\outfiles\\out" ;
      char nameAppend[] = "00001" ;

      for ( ;; )
      {
         strcpy( name, fileNameStart ) ;
         strcat( name, nameAppend ) ;

         // AS May 16/03 - make internal open
         if ( file4openInternal( &g_file, g_c4, name, 0, OPT4OTHER ) == 0 )
         {
            file4close( &g_file ) ;
            if ( nameAppend[4] == '9' )
            {
               if ( nameAppend[3] == '9' )
               {
                  if ( nameAppend[2] == '9' )
                  {
                     if ( nameAppend[1] == '9' )
                     {
                        if ( nameAppend[0] == '9' )
                        {
                           // file create failed - just return without creating
                           error4( g_c4, e4open, E90603 ) ;
                           code4initUndo( g_c4 ) ;
                           return ;
                        }
                        else
                        {
                           (nameAppend[0])++;
                        }

                        nameAppend[1] = '0';
                     }
                     else
                     {
                        (nameAppend[1])++;
                     }

                     nameAppend[2] = '0';
                  }
                  else
                  {
                     (nameAppend[2])++;
                  }

                  nameAppend[3] = '0';
               }
               else
               {
                  (nameAppend[3])++;
               }

               nameAppend[4] = '0';
            }
            else
               (nameAppend[4])++;
            continue ;

         }

         // AS May 16/03 - make internal create
         if ( file4createInternal( &g_file, g_c4, name, 0, OPT4OTHER ) != 0 )
         {
            error4( g_c4, e4open, E90603 ) ;
            code4initUndo( g_c4 ) ;
            return ;
         }

         return ;
      }
   }



   void log5( const char *ptrToWrite )
   {
      if ( log5enabled == 0 )
         log4invokeDebugger() ;
      if ( log5enabled == 0 )
         return ;


      if ( g_c4 == 0 )
         log5openFile();

      if ( g_c4 == 0 || g_file.hand == INVALID4HANDLE )
         return ;

      #ifdef USE4EXTERNAL
         if ( file4write( &g_file, file4len( &g_file ), ptrToWrite, strlen( ptrToWrite ) ) != 0 )
      #else
         if ( file4writeInternal( &g_file, file4lenLow( &g_file ), ptrToWrite, strlen( ptrToWrite ) ) != 0 )
      #endif
      {
         error4( g_c4, e4write, E90603 ) ;
         log5closeFile();
         return ;
      }
   }


   #ifdef OLEDB5BUILD
      void out5riidInfo( REFIID riid )
      {
         if ( log5queryEnabled == 0 )
            return ;

         if ( riid == IID_IUnknown )
         {
            log5( " RIID == IID_IUnknown" ) ;
            return ;
         }

         if ( riid == IID_IDBInitialize )
         {
            log5( " RIID == IID_IDBInitialize" ) ;
            return ;
         }

         if ( riid == IID_IPersist )
         {
            log5( " RIID == IID_IPersist" ) ;
            return ;
         }

         if ( riid == IID_IDBProperties )
         {
            log5( " RIID == IID_IDBProperties" ) ;
            return ;
         }

         if ( riid == IID_IDBInfo )
         {
            log5( " RIID == IID_IDBInfo" ) ;
            return ;
         }

         if ( riid == IID_IDBCreateSession )
         {
            log5( " RIID == IID_IDBCreateSession" ) ;
            return ;
         }

         if ( riid == IID_IMarshal )
         {
            log5( " RIID == IID_IMarshal" ) ;
            return ;
         }

         if ( riid == IID_IStdMarshalInfo )
         {
            log5( " RIID == IID_IStdMarshalInfo" ) ;
            return ;
         }

         if ( riid == IID_IExternalConnection )
         {
            log5( " RIID == IID_IExternalConnection" ) ;
            return ;
         }

         if ( riid == IID_IClassFactory )
         {
            log5( " RIID == IID_IClassFactory" ) ;
            return ;
         }

         if ( riid == IID_IDBSchemaRowset )
         {
            log5( " RIID == IID_IDBSchemaRowset" ) ;
            return ;
         }

         if ( riid == IID_IDBSchemaRowset )
         {
            log5( " RIID == IID_IDBSchemaRowset" ) ;
            return ;
         }

         if ( riid == IID_IRowset )
         {
            log5( " RIID == IID_IIRowset" ) ;
            return ;
         }

         if ( riid == IID_IRowsetInfo )
         {
            log5( " RIID == IID_IRowsetInfo" ) ;
            return ;
         }

         if ( riid == IID_IRowsetChange )
         {
            log5( " RIID == IID_IRowsetChange" ) ;
            return ;
         }

         if ( riid == IID_IRowsetUpdate )
         {
            log5( " RIID == IID_IRowsetUpdate" ) ;
            return ;
         }

         if ( riid == IID_IRowsetLocate )
         {
            log5( " RIID == IID_IRowsetLocate" ) ;
            return ;
         }

         if ( riid == IID_IDBAsynchStatus )
         {
            log5( " RIID == IID_IDBAsynchStatus" ) ;
            return ;
         }

         if ( riid == IID_IAccessor )
         {
            log5( " RIID == IID_IAccessor" ) ;
            return ;
         }

         if ( riid == IID_IConvertType )
         {
            log5( " RIID == IID_IConvertType" ) ;
            return ;
         }

         if ( riid == IID_IColumnsInfo )
         {
            log5( " RIID == IID_IColumnsInfo" ) ;
            return ;
         }

         if ( riid == IID_ITransactionJoin )
         {
            log5( " RIID == IID_ITransactionJoin" ) ;
            return ;
         }

         if ( riid == IID_ISessionProperties )
         {
            log5( " RIID == IID_ISessionProperties" ) ;
            return ;
         }

         if ( riid == IID_IOpenRowset )
         {
            log5( " RIID == IID_IOpenRowset" ) ;
            return ;
         }

         if ( riid == IID_IGetDataSource )
         {
            log5( " RIID == IID_IGetDataSource" ) ;
            return ;
         }

         if ( riid == IID_IDBCreateCommand )
         {
            log5( " RIID == IID_IDBCreateCommand" ) ;
            return ;
         }

         if ( riid == IID_IRowsetIndex )
         {
            log5( " RIID == IID_IRowsetIndex" ) ;
            return ;
         }

         if ( riid == IID_ISupportErrorInfo )
         {
            log5( " RIID == IID_ISupportErrorInfo" ) ;
            return ;
         }

         log5( " RIID == <unknown value>" ) ;

      /*   if ( riid == IID_I )
         {
            log5( " RIID == IID_I" ) ;
            return ;
         }

         if ( riid == IID_I )
         {
            log5( " RIID == IID_I" ) ;
            return ;
         }

         if ( riid == IID_I )
         {
            log5( " RIID == IID_I" ) ;
            return ;
         }
      */

      }
   #endif /* #ifdef OLEDB5BUILD */
#endif /* S4OLEDEBUG_PRINT */
