/* d4verify.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

void verifySwitches( void ) ;

CODE4 cb ;

FIELD4INFO fieldInfo[] =
{
   { "TEST_FIELD",'C',20,0 },
   { 0,0,0,0 },
};

void main()
{
   verifySwitches() ;
   printf( "\nSwitches Verified Successfully!\n" ) ;

   code4init( &cb ) ;

   #ifdef N4OTHER
      cb.autoOpen = 0 ;
   #endif

   cb.errOpen = 0 ;

   #ifdef S4CLIENT
      printf("\nAttempting implicit connection..." ) ;
   #endif

   if( d4open( &cb, "TEST.DBF" ) != 0 )
      printf( "\nTEST.DBF Opened\n" ) ;
   else
   {
      if( d4create(&cb,"TEST.DBF",fieldInfo,0) != 0 )
         printf("\nTEST.DBF Created\n") ;
   }

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}

void verifySwitches( void )
{
   long d4switch ;

   d4switch = u4switch() ;

   printf( "\n\nLibrary Conditional Compilation Switches:\n" ) ;
   if( d4switch & 1L )
      printf( "S4FOX - FoxPro Index File Compatibility\n" ) ;
   if( d4switch & 2L )
      printf( "S4CLIPPER - Clipper Index File Compatibility\n" ) ;
   if( d4switch & 4L )
      printf( "S4MDX - dBASE IV Index File Compatibility\n" ) ;
   if( d4switch & 0x10L )
      printf( "S4DOS - DOS\n" ) ;
   if( d4switch & 0x20L )
      printf( "S4WIN16 - Microsoft Windows 16-Bit Application\n" ) ;
   if( d4switch & 0x40L )
      printf( "S4WIN32 - Microsoft Windows 32-Bit Application\n" ) ;
   if( d4switch & 0x100L )
      printf( "S4OS2 - OS/2\n" ) ;
   if( d4switch & 0x200L )
      printf( "S4UNIX - UNIX\n" ) ;
   if( d4switch & 0x400L )
      printf( "S4MACINTOSH - MACINTOSH\n" ) ;
   if( d4switch & 0x800L )
      printf( "S4PACAL_WIN - PASCAL Windows\n" ) ;
   if( d4switch & 0x1000L )
      printf( "S4CB51 - CodeBase 5.1 compatibility\n" ) ;
   if( d4switch & 0x2000L )
      printf( "\nS4SAFE  - Immediate File Length Updates\n" ) ;
   if( d4switch & 0x4000L )
      printf( "\nS4LOCK_HOOK - Custom Lock Failure Function\n" ) ;
   if( d4switch & 0x8000L )
      printf( "\nS4MAX - Maximum Memory Allocation Testing\n" ) ;
   if( d4switch & 0x10000L )
      printf( "\nS4TIMEOUT_HOOK - Custom Timeout Hook Function\n" ) ;
   if( d4switch & 0x20000L )
      printf( "E4ANALYZE - Structure Analysis CodeBase Error Checking\n" ) ;
   if( d4switch & 0x40000L )
      printf( "E4DEBUG - Extended CodeBase Error Checking\n" ) ;
   if( d4switch & 0x80000L )
      printf( "E4HOOK - Custom Error Function\n" ) ;
   if( d4switch & 0x100000L )
      printf( "E4LINK - Link List CodeBase Error Checking\n" ) ;
   if( d4switch & 0x200000L )
      printf( "E4MISC - Miscellaneous CodeBase Error Checking\n" ) ;
   if( d4switch & 0x400000L )
      printf( "E4OFF - No CodeBase Error Display\n" ) ;
   if( d4switch & 0x800000L )
      printf( "E4OFF_STRING - No Extended String CodeBase Error Display\n" ) ;
   if( d4switch & 0x1000000L )
      printf( "E4PARM_HIGH - High-level Functional Parameter CodeBase Error Display\n" ) ;
   if( d4switch & 0x2000000L )
      printf( "E4PAUSE - CodeBase Pause-on-error Enabled\n" ) ;
   if( d4switch & 0x4000000L )
      printf( "E4STOP - CodeBase Stop-on-error Enabled\n" ) ;
   if( d4switch & 0x8000000L )
      printf( "E4STOP_CRITICAL - CodeBase Stop-on-critical-error Enabled\n" ) ;
   if( d4switch & 0x10000000L )
      printf( "S4OFF_INDEX - CodeBase Index Support Source Code Removed\n" ) ;
   if( d4switch & 0x20000000L )
      printf( "S4OFF_MEMO - CodeBase Memo Support Source Code Removed\n" ) ;
   if( d4switch & 0x40000000L )
      printf( "S4OFF_MULTI - CodeBase Multi-user Support Source Code Removed\n" ) ;
   if( d4switch & 0x80000000L )
      printf( "S4OFF_OPTIMIZE - CodeBase Optimization Source Code Removed\n" ) ;
   if( d4switch & 0x8L )
      printf( "S4CLIENT - CodeBase In Client/Server Mode\n" ) ;
   if( d4switch & 0x80L )
      printf( "S4STAND_ALONE - CodeBase In Stand Alone Mode\n" ) ;

/* no room for these switches
   if( d4switch & 0x100000000L )
      printf( "S4OFF_REPORT - CodeBase Reporter Source Code Removed\n" ) ;
   if( d4switch & 0x200000000L )
      printf( "S4OFF_TRAN - CodeBase Transactions Support Source Code Removed\n" ) ;
   if( d4switch & 0x400000000L )
      printf( "S4OFF_WRITE - CodeBase File Write Source Code Removed\n" ) ;
*/

   #ifdef S4STAND_ALONE
      if( ( d4switch & 0x80L ) == 0 )
         error4describe( &cb, e4result, E87001, "S4STAND_ALONE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x80L )
         error4describe( &cb, e4result, E87001, "S4STAND_ALONE must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4CLIENT
      if( ( d4switch & 0x8L ) == 0 )
         error4describe( &cb, e4result, E87001, "S4CLIENT must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x8L )
         error4describe( &cb, e4result, E87001, "S4CLIENT must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_OPTIMIZE
      if( (d4switch & 0x80000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_OPTIMIZE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x80000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_OPTIMIZE must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_INDEX
      if( (d4switch & 0x10000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_INDEX must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x10000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_INDEX must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_MEMO
      if( (d4switch & 0x20000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_MEMO must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x20000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_MEMO must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_MULTI
      if( (d4switch & 0x40000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_MULTI must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x40000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_MULTI must be off in Library Build too.", 0, 0 ) ;
   #endif

/*
   #ifdef S4OFF_TRAN
      if( (d4switch & 0x200000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_TRAN must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x200000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_TRAN must be off in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4OFF_WRITE
      if( (d4switch & 0x400000000L )  ==  0 )
         error4describe( &cb, e4result, E87001, "S4OFF_WRITE must be used in Library Build too.", 0, 0 ) ;
   #else
      if( d4switch & 0x400000000L )
         error4describe( &cb, e4result, E87001, "S4OFF_WRITE must be off in Library Build too.", 0, 0 ) ;
   #endif
*/

   #ifdef S4FOX
      if( (d4switch & 1)  == 0 )
         error4describe( &cb, e4result, E87001, "S4FOX must be used in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4CLIPPER
      if( (d4switch & 2)  == 0 )
         error4describe( &cb, e4result, E87001, "S4CLIPPER must be used in Library Build too.", 0, 0 ) ;
   #endif

   #ifdef S4MDX
      if( (d4switch & 4)  == 0 )
         error4describe( &cb, e4result, E87001, "S4MDX must be used in Library Build too.", 0, 0 ) ;
   #endif
}
