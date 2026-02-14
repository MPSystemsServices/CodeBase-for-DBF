/* d4simba.c - contains copies of some simba server admin functionality... */

#define DRM4BUILD
#include <windows.h>
// #include "drm4cb.h"
// #include "util5.hpp"
#include <direct.h>
#include <process.h>
#include <math.h>
#include "pa_os.h"
#include "simbadrm.h"
#include "simbalam.h"
#include "regrc.h"
#include "svcrc.h"
//#include "mgrstr.h"
#include "common.h"
#include <sqlext.h>
#include <stdio.h>
#include <stdlib.h>
#include "srvrstr.h"

// AS Sep 15/03 - include for cgiadmin
#include "CgiAdminMessageLoader.h"

// include longlong conversion...
#include "c4long.c"

// #define MAXLEN_RESOURCE_STR 256
#define _REG_MEM_TOK 2048
#define _DEFAULT_MGR_PORT 1583L

#define LENGTH int
#define PSTRZ char *
#define STRZ char
#define LONGN unsigned long
#define INTN unsigned int

static const STRZ _szMgrPort[] = "SvrPort" ;
static const STRZ _szEmpty[] = "" ;



static HINSTANCE _hCgcGetInstance(VOID)
{
   HINSTANCE               hInstance;

   #ifdef CLWIN32
      hInstance = GetModuleHandle(NULL);
   #else
      hInstance = (HINSTANCE)NULL;
   #endif  /* CLWIN32 */

   return (hInstance);
}


static VOID _CgcGetServerName( char * pszServerName )
{
   // produces name as <computer name><port#> ... eg "andrew3:5183"
   char szHostName[65] ;
   char * pszExpress ;
   char szTmp[MAXLEN_RESOURCE_STR + 1] ;
   long lPort ;
   char * pszTmp ;

   // get computer name
   gethostname( szHostName, sizeof( szHostName ) ) ;

   // get port # from registry...
   // pszExpress = pszStrLoadResString( _hCgcGetInstance(), REG_EXP_NAME ) ;
   pszExpress = "CodeBaseOdbc" ;  // from registry, use 'Express' entry (may change later)
   if ( pszExpress != NULL )
   {
      char *iniFile = "HKEY_LOCAL_MACHINE\\Software\\CodeBaseOdbc\\SERVER.INI" ;  // may change...
      lenStrGetProfileString( iniFile, pszExpress, (char *)_szMgrPort, (char *)_szEmpty, szTmp, sizeof( szTmp ) ) ;
      lPort = strtol( szTmp, &pszTmp, 10 ) ;
      if ( '\0' == szTmp[0] || *pszTmp != '\0' || lPort < 0L || lPort > 65535L )
         lPort = _DEFAULT_MGR_PORT ;

      sprintf( pszServerName, "%s.%ld", szHostName, lPort ) ;
   }
}


#ifdef __cplusplus
   extern "C" short _rpcLnaShutdownXP( char * pszServerName, unsigned short ui16ShutdownMode ) ;
#else
   extern short _rpcLnaShutdownXP( char * pszServerName, unsigned short ui16ShutdownMode ) ;
#endif

#ifdef __cplusplus
   extern "C" short _rpcLnaEnumUsersXP( PSTRZ pszServerName, LAM_USER_LIST **pastLamUsers ) ;
#else
   extern short _rpcLnaEnumUsersXP( PSTRZ pszServerName, LAM_USER_LIST **pastLamUsers ) ;
#endif

#define LNA_SHUTDOWN_GRACEFUL 1
#define LNA_SHUTDOWN_NOW      2

static int loadResources()
{
   static int resourcesLoaded = 0 ;
   // static STRZ szDefaultExpKey[MAXLEN_RESOURCE_STR + 1] = "CodeServerOdbc" ;
   // STRZ szExpressName[MAXLEN_RESOURCE_STR + 1] ;
   // static STRZ _szSvcServiceName[MAXLEN_RESOURCE_STR + 1] ;

   if ( resourcesLoaded == 0 )
   {
      InitializeCgiAdminMessages();
      // if (!bMessageManagerStartup(COMPONENT_INSTANCE, pMessageSource))
      // {
      //    DisplayMessageLoadError(LOAD_MESSAGE_ERROR);
      //
      //    return FALSE;
      // }
      //
      // PSTRZ   pszTmp;
      // pszTmp = pszStrLoadResString(_hCgcGetInstance(), REG_SVR_NAME);
      /* get the resource DLLs */
      // AS 10/31/00 - the names of these dll's has changed...
      // if ( !fStrLoadResFiles( "oemexpr.dll", "exprres.dll" ) ||
      // if ( !fStrLoadResFiles( "oemsrvr.dll", "srvrres.dll" ) ||
//      if ( !fStrLoadResFiles( "oemsrvr.dll" ) ||
//         !fStrLoadRemoteResString( REG_SVR_NAME, szDefaultExpKey, sizeof( szDefaultExpKey ) ) ||
//         !fStrLoadRemoteResString( SVR_SVR_NAME, szExpressName, sizeof( szExpressName ) ) ||
//         !fStrLoadRemoteResString( SVC_SIMBA_SVC, _szSvcServiceName, sizeof( _szSvcServiceName ) ) )
//            return -1 ;
      resourcesLoaded = 1 ;
   }

   return 0 ;
}



int odbc4shutdown( int mode )
{
   // mode can be one of
   //   LNA_SHUTDOWN_GRACEFUL 1
   //   LNA_SHUTDOWN_NOW      2

   int rc = 0 ;
   char szServerName[128] ;

   if ( loadResources() == -1 )
      return -1 ;

   // produces name as <computer name><socket#> ... eg "andrew3:5183"
   _CgcGetServerName( szServerName ) ;
   _rpcLnaShutdownXP( szServerName, (short)mode ) ;
   // _rpcLnaShutdownXP( szServerName, LNA_SHUTDOWN_NOW ) ;

//   rc = _rcCgcInitialize() ;

   return rc ;
}


short odbc4getClientsList( void **odbcClientsHandle )
{
   // note, the odbcClientsHandle returned must be freed by calling odbc4freeClientsList
   char szServerName[128] ;
   LAM_USER_LIST *pLamUserList = 0 ;
   short rc ;

   if ( loadResources() == -1 )
      return -1 ;

   _CgcGetServerName( szServerName ) ;
   rc = _rpcLnaEnumUsersXP( szServerName, &pLamUserList ) ;
   // for ( ;; )
   // {
   //    LAM_USER *pLamUser ;
   //    pLamUser = &pLamUserList->pstLamUser[ui16] ;
   // }

   *odbcClientsHandle = pLamUserList ;

   return rc ;
}



void odbc4freeClientsList( void *odbcClientsHandle )
{
   LAM_USER_LIST *pLamUserList = (LAM_USER_LIST *)odbcClientsHandle ;

   if ( pLamUserList != NULL )
   {
      free( pLamUserList->pstLamUser ) ;
      free( pLamUserList ) ;
   }
}



short odbc4getClientsFirst( void *odbcClientsHandle )
{
   // returns -1 if no clients
   LAM_USER_LIST *pLamUserList = (LAM_USER_LIST *)odbcClientsHandle ;

   if ( pLamUserList->ui16NumUsers == 0 )
      return -1 ;

   return 0 ;
}



short odbc4getClientsNext( void *odbcClientsHandle, short lastClient )
{
   // returns -1 if no more clients
   LAM_USER_LIST *pLamUserList = (LAM_USER_LIST *)odbcClientsHandle ;

   if ( lastClient + 1 >= pLamUserList->ui16NumUsers )
      return -1 ;

   return lastClient + 1 ;
}



typedef struct
{
   unsigned char tcpAddress[4] ;
} TCP4ADDRESS ;


// all non-normal addresses are an error...
#define r4blankTcpAddress -1
#define e4invalidTcpAddress -1
// include the tcp4charToAddress function...
#include "tcp4char.c"

int odbc4getTcpAddress( void *odbcClientsHandle, short clientHandle, TCP4ADDRESS *address )
{
   LAM_USER_LIST *pLamUserList = (LAM_USER_LIST *)odbcClientsHandle ;
   LAM_USER *pLamUser = &pLamUserList->pstLamUser[clientHandle] ;

   return tcp4charToAddress( pLamUser->szIP, (unsigned char)strlen( pLamUser->szIP ), address ) ;
}



const char *odbc4getAccountId( void *odbcClientsHandle, short clientHandle )
{
   LAM_USER_LIST *pLamUserList = (LAM_USER_LIST *)odbcClientsHandle ;
   LAM_USER *pLamUser = &pLamUserList->pstLamUser[clientHandle] ;

   return pLamUser->szUserName ;
}


LONGLONG odbc4getLamUserId( void *odbcClientsHandle, short clientHandle )
{
   LAM_USER_LIST *pLamUserList = (LAM_USER_LIST *)odbcClientsHandle ;
   LAM_USER *pLamUser = &pLamUserList->pstLamUser[clientHandle] ;

   // the combo of 'processId' + 'threadId' gives a unique identifier for the client,
   // and that value is used by Simba for disconnect purposes.  Therefore use it here as
   // well
   // I believe that this pointer value should be unique, so use it for odbc clients...
   LONGLONG out = ((LONGLONG)pLamUser->ui32ProcessId << 32) + pLamUser->ui32ThreadId ;
   return out ;
}


extern short _rpcLnaKillConnectionsXP( PSTRZ pszServerName, CONNECTION_ID_LIST *pstConnectionIdList ) ;

void odbc4cutOneClient( LONGLONG connIdLongLong )
{
   // disconnects one ODBC client

   // input is a connection id, LONGLONG, where 1st long is the processid, and 2nd long
   // is the threadid.

   STRZ szServerName[128] ;
   CONNECTION_ID_LIST ConnectionList ;
   CONNECTION_ID connectionId ;
   unsigned long *longArray ;

   if ( loadResources() == -1 )
      return ;

   // the input LONGLONG is to be interpreted as 2 longs, the first = processId, second = threadId
   longArray = (unsigned long *)&connIdLongLong ;

   connectionId.ui32ProcessId = longArray[1] ;
   connectionId.ui32ThreadId = longArray[0] ;

   ConnectionList.ui16NumConnectionId = 1 ;
   ConnectionList.pstConnectionId = &connectionId ;

   _CgcGetServerName( szServerName ) ;
   _rpcLnaKillConnectionsXP( szServerName, &ConnectionList ) ;
}



long odbc4getClientCount()
{
   LAM_USER_LIST *pLamUserList ;
   long userCount ;

   // AS Mar 26/03 - For now, this is not working, return 0
   return 0 ;

   if ( loadResources() == -1 )
      return -1 ;

   if ( odbc4getClientsList( (void **)&pLamUserList ) < 0 )
      return -1 ;

   userCount = pLamUserList->ui16NumUsers ;

   odbc4freeClientsList( pLamUserList ) ;

   return userCount ;
}



void odbc4cutAllClients()
{
   // disconnects all ODBC clients
   // create a list of clients to kill containing all clients, then kill them...
   STRZ szServerName[128] ;
   CONNECTION_ID_LIST ConnectionList ;
   CONNECTION_ID *pConnectionIds = NULL ;
   LAM_USER_LIST *pLamUserList ;
   unsigned int clientIndex ;

   if ( loadResources() == -1 )
      return ;

   if ( odbc4getClientsList( (void **)&pLamUserList ) < 0 )
      return ;
   pConnectionIds = (CONNECTION_ID *)malloc( sizeof( CONNECTION_ID ) * pLamUserList->ui16NumUsers ) ;
   if ( pConnectionIds == 0 )
   {
      odbc4freeClientsList( pLamUserList ) ;
      return ;
   }

   for ( clientIndex = 0 ; clientIndex < pLamUserList->ui16NumUsers ; ++clientIndex )
   {
      LAM_USER *pLamUser = &pLamUserList->pstLamUser[clientIndex] ;

      pConnectionIds[clientIndex].ui32ProcessId = pLamUser->ui32ProcessId ;
      pConnectionIds[clientIndex].ui32ThreadId = pLamUser->ui32ThreadId ;
   }

   ConnectionList.ui16NumConnectionId = pLamUserList->ui16NumUsers ;
   ConnectionList.pstConnectionId = pConnectionIds ;

   _CgcGetServerName( szServerName ) ;
   _rpcLnaKillConnectionsXP( szServerName, &ConnectionList ) ;

   free( pConnectionIds ) ;
   odbc4freeClientsList( pLamUserList ) ;
}

#ifdef __cplusplus
   extern "C"
#endif
BOOL APIENTRY DllMain( HINSTANCE hModule, DWORD reasonForCall, LPVOID reserved )
{
   return TRUE;
}
