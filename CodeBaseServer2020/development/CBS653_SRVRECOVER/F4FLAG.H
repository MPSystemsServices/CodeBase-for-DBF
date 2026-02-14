/* f4flag.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

typedef struct
{
   CODE4 S4PTR *codeBase ;
   unsigned char S4PTR *flags ;
   unsigned long  numFlags ;
   Bool5    isFlip ;
//   Bool5    special ;    /* special flag, used for flag lock module */
} F4FLAG ;

#ifdef __cplusplus
   extern "C" {
#endif

S4EXPORT int  S4FUNCTION f4flagInit( F4FLAG S4PTR *, CODE4 S4PTR *, const unsigned long, Bool5 ) ;
S4EXPORT int  S4FUNCTION f4flagSet( F4FLAG S4PTR *, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagReset( F4FLAG S4PTR *, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagIsSet( F4FLAG S4PTR *, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagIsAllSet( F4FLAG S4PTR *, const unsigned long, const unsigned long ) ;
S4EXPORT int  S4FUNCTION f4flagIsAnySet( F4FLAG S4PTR *, const unsigned long, const unsigned long ) ;
S4EXPORT void S4FUNCTION f4flagSetAll( F4FLAG S4PTR * ) ;
S4EXPORT int  S4FUNCTION f4flagSetRange( F4FLAG S4PTR *, const unsigned long, const unsigned long ) ;

/* For Report Module */
S4EXPORT int  S4FUNCTION f4flagOr( F4FLAG S4PTR *, const F4FLAG S4PTR * ) ;
S4EXPORT int  S4FUNCTION f4flagAnd( F4FLAG S4PTR *, const F4FLAG S4PTR * ) ;
S4EXPORT void S4FUNCTION f4flagFlipReturns( F4FLAG S4PTR * ) ;

/* For Lock Module */
//#define flag4lockInit( f4, c4, nFlags ) f4flagInit( (f4), (c4), (nFlags), 1 )
//#define flag4lockLock( f4, n ) ( ( f4flagSet( (f4), (n) ) ) )
//#define flag4lockUnlock( f4, n ) ( f4flagReset( (f4), (n) ) )
//#define flag4lockEnabled( f4 ) ( (f4)->flags )
//#define flag4lockIsLocked( f4, n ) ( f4flagIsSet( (f4), (n) )
// #define flag4lockIsLockedFile( f4, n ) ( (f4)->isFlip )
// #define flag4lockIsLockedAppend( f4, n ) ( (f4)->special )
// #define flag4lockLockFile( f4, n ) ( (f4)->isFlip = 1, (f4)->special = 1 )
// #define flag4lockUnlockFile( f4, n ) ( (f4)->isFlip = 0, (f4)->special = 0 )
// #define flag4lockLockAppend( f4, n ) ( (f4)->special = 1 )
// #define flag4lockUnlockAppend( f4, n ) ( (f4)->special = 0 )

#ifdef __cplusplus
   }
#endif

