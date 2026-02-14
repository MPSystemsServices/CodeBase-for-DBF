/* p4port.h (c)Copyright Sequiter Software Inc., 1988-1998.  All rights reserved.

   Declarations for non-standard C runtime library functions.

   CB 6.4 RELEASE DATE: JULY 10/1998
*/

#ifdef S4UNIX

/* CodeBase Multi-Platform defines for AIX cc*/
   #define S464BIT              /* use for 64 bit systems */
/* #define S4BYTEORDER_2301  */ /* use for 2301 systems */
/* #define S4BYTEORDER_3210  */ /* use for 3210 systems */
/* #define S4BYTE_SWAP       */ /* use for swapping bytes on 3210 or 2301 systems */
   #define S4NO_NEGATIVE_LOCK   /* use if negative locking not supported */
   #define S4DATA_ALIGN         /* Use if data needs to be on byte boundaries */
/* #define S4MEMCMP          */ /* use if memcmp() uses signed byte comparison */

/* #define S4NO_ATOF         */ /* use if atof() not found */
   #define S4NO_CHSIZE          /* use if chsize() not found */
   #define S4NO_ECVT            /* use if ecvt() not found */
   #define S4NO_FCVT            /* use if fcvt() not found */
/* #define S4NO_FCHMOD       */ /* use if fchmod() not found */
   #define S4NO_FILELENGTH      /* use if filelength() not found */
/* #define S4NO_FTIME        */ /* Use if ftime() is not supported */
/* #define S4NO_LOCKF        */ /* Use if lockf() is not supported */
/* #define S4NO_MEMMOVE      */ /* use if memmove() not found */
/* #define S4NO_POW          */ /* use if pow() not found */
/* #define S4NO_RENAME       */ /* use if rename() not found */
/* #define S4NO_REMOVE       */ /* use if remove() not found */
/* #define S4NO_SIZE_T       */ /* use if variable size_t not found */
   #define S4NO_STRLWR          /* use if strlwr() not found */
   #define S4NO_STRUPR          /* use if strupr() not found */
   #define S4NO_STRNICMP        /* use if strnicmp() not found */
/* #define S4NO_USLEEP       */ /* use if usleep() not found */
   #define S4LINUX

/**********************************************************************/
/**********************************************************************/

   #include <unistd.h>
   #ifndef S4STAND_ALONE
      #ifdef S4BERKSOCK
         #include <sys/socket.h>
         #include <sys/socketvar.h>
         #include <netinet/in.h>
         #include <netinet/tcp.h>
         #include <netdb.h>
         #include <sys/types.h>
         #include <sys/time.h>
         #include <sys/ioctl.h>
         #include <arpa/inet.h>
         #ifndef S4LINUX
            #include <sys/select.h>
         #endif
      #endif
   #endif
#endif

/* */
/* */
/* */
/* */

#ifdef S4MULTIC4
   #include <sys/wait.h>
#endif

#define  S4CMP_PARM  const void *
