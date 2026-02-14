#include "d4all.h"

#ifdef S4SERVER
   #ifndef S4ODBC_BUILD
      #ifndef S4UNIX
         int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow )
         {
            return local4main( hInstance, hPrevInstance, lpCmdLine, nCmdShow ) ;
         }
      #else  // else is S4UNIX
         #include <signal.h>

         void server4shutdownSignal(int signo)
         {
            /* This function is called by the operating system when a
               shutdown signal is thrown. See the code near the top
               of int main() for the signals that would trigger this
               function. */
            server4quit(serverPtr, r4shutdown);
         }

         int main( int argc, char **argv )
         {

            if ( argc > 2 )
            {
               fprintf(stderr, "Use is: server [config-name]\n");
               exit( 1 );
            }
            if ( signal(SIGPIPE, SIG_IGN) == SIG_ERR )
            {
               fprintf(stderr, "Cannot catch SIGPIPE\n");
               exit( 1 );
            }
            if ( signal(SIGINT, server4shutdownSignal) == SIG_ERR )
            {
               fprintf(stderr, "Cannot catch SIGINT\n");
               exit( 1 );
            }
            if ( signal(SIGTERM, server4shutdownSignal) == SIG_ERR )
            {
               fprintf(stderr, "Cannot catch SIGINT\n");
               exit( 1 );
            }

            d4server( argc == 1 ? 0 : (void *) argv[1] ) ;

            if ( serverPtr != 0 )
            {
               server4initUndo( serverPtr ) ;
               server4free( serverPtr ) ;
               serverPtr = 0 ;
            }
         }
      #endif  // S4UNIX
   #endif  // ! S4ODBC_BUILD
#endif  // S4SERVER
