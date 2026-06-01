/* d4stamp.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef D4STAMP_INC
#define D4STAMP_INC

typedef struct
{
   ENTRYINFO5 SearchString;
   ENTRYINFO5 doLockingString ;
   int doLargeLocking;
   int previousCode4init;
}C4STAMP;

#endif
