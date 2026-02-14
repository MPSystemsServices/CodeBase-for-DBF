/* d4config.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* Used to install the CodeBase engine as a service */
/* CodeBase server must be placed into %SystemRoot%\\system(32)\\ to set up */
/* LY 2001/06/14 : allowing user to specify location of s4service.exe */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef S4ODBC_BUILD
   #define S4SERVICE_NAME "CodeBase SQL Server"
#else
   #define S4SERVICE_NAME "CodeBase Database Server"
#endif

void showError(const char *functionName,DWORD error_num)
{
   fprintf(stderr,"%s failed. GetLastError returned ",functionName);
   switch(error_num)
   {
      case ERROR_ACCESS_DENIED :
         fprintf(stderr,"ERROR_ACCESS_DENIED (%ld)\n",error_num);
         break;
      case ERROR_CIRCULAR_DEPENDENCY :
         fprintf(stderr,"ERROR_CIRCULAR_DEPENDENCY (%ld)\n",error_num);
         break;
      case ERROR_DATABASE_DOES_NOT_EXIST :
         fprintf(stderr,"ERROR_DATABASE_DOES_NOT_EXIST (%ld)\n",error_num);
         break;
      case ERROR_DUP_NAME :
         fprintf(stderr,"ERROR_DUP_NAME (%ld)\n",error_num);
         break;
      case ERROR_INVALID_HANDLE :
         fprintf(stderr,"ERROR_INVALID_HANDLE (%ld)\n",error_num);
         break;
      case ERROR_INVALID_PARAMETER :
         fprintf(stderr,"ERROR_INVALID_PARAMETER (%ld)\n",error_num);
         break;
      case ERROR_INVALID_NAME :
         fprintf(stderr,"ERROR_INVALID_NAME (%ld)\n",error_num);
         break;
      case ERROR_INVALID_SERVICE_ACCOUNT :
         fprintf(stderr,"ERROR_INVALID_SERVICE_ACCOUNT (%ld)\n",error_num);
         break;
      case ERROR_SERVICE_DOES_NOT_EXIST :
         fprintf(stderr,"ERROR_SERVICE_DOES_NOT_EXIST (%ld)\n",error_num);
         break;
      case ERROR_SERVICE_EXISTS :
         fprintf(stderr,"ERROR_SERVICE_EXISTS (%ld)\n",error_num);
         break;
      case ERROR_SERVICE_MARKED_FOR_DELETE :
         fprintf(stderr,"ERROR_SERVICE_MARKED_FOR_DELETE (%ld)\n",error_num);
         break;
      default :
         fprintf(stderr,"%ld\n",error_num);
   }
}

void main( DWORD argc, LPTSTR *argv )
{
   char printUsage = 0 ;
   int doAdd = 0 ;
   char pszBinaryPathName[_MAX_PATH] ;
   WIN32_FIND_DATA dummy ;

   if ( argc < 2 )  /* LY 2001/06/14 : changed != to < to allow path arg */
      printUsage = 1 ;
   else
   {
      if ( strcmp( argv[1], "-add" ) == 0 )
         doAdd = 1 ;
      else
         if ( strcmp( argv[1], "-del" ) != 0 )
            printUsage = 1 ;
      if ( argc == 3 )
         sprintf( pszBinaryPathName, "%s\\s4service.exe", argv[2] ) ;
      else
         strcpy( pszBinaryPathName, "%%SystemRoot%%\\system32\\s4service.exe" ) ;
   }

   if ( doAdd && FindFirstFile( pszBinaryPathName, &dummy ) == INVALID_HANDLE_VALUE )
   {
      printf( "%s not found.  Exiting.\n", pszBinaryPathName ) ;
      return ;
   }

   if ( printUsage == 1 )
   {
      printf( "Usage:  d4config [-add <servicepath> | -del]\n\n" ) ;
      printf( "-add <servicepath>  to add s4service.exe as a service\n" ) ;
      printf( "                    <servicepath> is full path to s4service.exe;\n" ) ;
      printf( "                    if <servicepath> is blank, d4config looks for\n" ) ;
      printf( "                    s4service.exe in %%SystemRoot%%\\system32\n\n" ) ;
      printf( "-del                to delete s4service.exe as a service\n" ) ;
      return ;
   }

   SC_HANDLE schSCManager = OpenSCManager( 0, 0, SC_MANAGER_ALL_ACCESS ) ;
   if ( schSCManager == 0 )
   {
      showError("OpenSCManager",GetLastError());
      return;
   }

   if ( doAdd == 1 )
   {
      SC_HANDLE schService = CreateService(
         schSCManager,              // SCManager database
         S4SERVICE_NAME,              // name of service
         S4SERVICE_NAME,           // service name to display
         SERVICE_ALL_ACCESS,        // desired access
         SERVICE_WIN32_OWN_PROCESS, // service type
         SERVICE_AUTO_START,      // start type
         SERVICE_ERROR_NORMAL,      // error control type
         (LPCTSTR) pszBinaryPathName,        // service's binary
         NULL,                      // no load ordering group
         NULL,                      // no tag identifier
         NULL,                      // no dependencies
         NULL,                      // LocalSystem account
         NULL);                     // no password

      if (schService == 0 )
         showError("CreateService",GetLastError());
      else
      {
         printf( "CreateService SUCCESS.\n" ) ;
         CloseServiceHandle( schService ) ;
      }
   }
   else
   {
      SC_HANDLE schService = OpenService(
           schSCManager,       // SCManager database
           S4SERVICE_NAME,       // name of service
           DELETE);            // only need DELETE access

      if (schService == NULL)
         showError("OpenService",GetLastError());
      else
      {
         if (! DeleteService(schService) )
            showError("DeleteService",GetLastError());
         else
            printf("DeleteService SUCCESS\n");

         CloseServiceHandle(schService);
      }
   }

   CloseServiceHandle( schSCManager ) ;

   return ;
}
