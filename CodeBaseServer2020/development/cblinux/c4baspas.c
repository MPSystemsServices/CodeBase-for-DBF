/* c4baspas.c  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.  */

#include "d4all.h"

#define r4check -5

#if defined(S4DLL_BUILD) || defined(S4LINUX)
   #include <math.h>

   #ifdef S4WINDOWS
      extern HINSTANCE cb5inst ;
   #endif

   #ifndef S4UNIX
      #ifdef __TURBOC__
         #pragma hdrstop
      #endif
   #endif

   short c4setLog( short *logValue, short newValue )
   {                             /* if value is true, return VB True value (-1) */
      short temp;
      if (newValue == r4check) return (!!(*logValue))*(-1);

      temp = *logValue;
      *logValue = !!newValue;  /* any non-zero value becomes one */
      return (!!temp)*(-1);
   }

   int c4setLog( int *logValue, short newValue )
   {                             /* if value is true, return VB True value (-1) */
      int temp;
      if (newValue == r4check) return (!!(*logValue))*(-1);

      temp = *logValue;
      *logValue = !!newValue;  /* any non-zero value becomes one */
      return (!!temp)*(-1);
   }

   char c4setLog( char *logValue, short newValue )
   {                             /* if value is true, return VB True value (-1) */
      char temp;
      if (newValue == r4check) return (!!(*logValue))*(-1);

      temp = *logValue;
      *logValue = !!newValue;  /* any non-zero value becomes one */
      return (!!temp)*(-1);
   }


   #ifdef __cplusplus
      extern "C" {
   #endif

   /*  CODE4 CLASS TRANSLATIONS  */
   short S4FUNCTION code4autoOpen( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40101 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->autoOpen), value ) ;
   }

   #ifndef S4CB51
   short S4FUNCTION code4codePage( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, 0 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->codePage ) ;
      temp = cb->codePage ;
      cb->codePage = value ;

      return( temp ) ;
   }

   short S4FUNCTION code4collatingSequence( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, 0 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->collatingSequence ) ;
      temp = cb->collatingSequence ;
      cb->collatingSequence = value ;

      return( temp ) ;
   }
   #endif

   short S4FUNCTION code4compatibility( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, 0 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->compatibility ) ;
      temp = cb->compatibility ;
      cb->compatibility = value ;

      return( temp ) ;
   }

   short S4FUNCTION code4createTemp( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, 0 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->createTemp), value ) ;
   }

   short S4FUNCTION code4errCreate( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40102 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errCreate), value ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4defaultUniqueError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errDefaultUnique( CODE4 *cb, short value )
   #endif
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40104 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check && value != e4unique ) return ( r4check ) ;
      if ( value == r4check ) return ( cb->errDefaultUnique ) ;
      temp = cb->errDefaultUnique ;
      cb->errDefaultUnique = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4errorCode( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40105 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( error4code( cb ) ) ;
      temp = error4code( cb ) ;
      error4set( cb, value ) ;
      return( temp ) ;
   }

   /* Needed for CSA application */
   short S4FUNCTION code4errorCode2( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40105 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( (short)error4code2( cb ) ) ;
      temp = (short)error4code2( cb ) ;
      error4set2( cb, value ) ;
      return( temp ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4exclusive( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4accessMode( CODE4 *cb, short value )
   #endif
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40106 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->accessMode ) ;
      temp = cb->accessMode ;
      cb->accessMode =(char)(value) ;
      return( temp ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4exprError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errExpr( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40107 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errExpr), value ) ;
   }


   #ifdef S4CB51
   short S4FUNCTION code4fieldNameError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errFieldName( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40108 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errFieldName), value ) ;
   }

   short S4FUNCTION code4fileFlush( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40152 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->fileFlush), value ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4goError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errGo( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40109 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errGo), value ) ;
   }

   #ifndef S4WIN64
      long S4FUNCTION code4hInst( CODE4 *cb, long value )
      {
         #ifdef S4WIN32
            void *temp ;
         #else
            short temp ;
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( cb, 1, E40110 ) ) return -1L ;
         #endif

         if ( value < 0 && value != r4check ) return (long) r4check ;
         if ( value == r4check ) return (long) cb->hInst ;
         temp = cb->hInst ;
         #ifdef S4WIN32
            cb->hInst = (HINSTANCE) value ;
         #else
            cb->hInst = (int) value ;
         #endif
         return (long)temp ;
      }

      long S4FUNCTION code4hWnd( CODE4 *cb, long value )
      {
         #ifndef S4WINDOWS
            return 0 ;
         #else
            #ifdef S4WIN32
               void *temp ;
            #else
               short temp ;
            #endif

            #ifdef E4VBASIC
               if ( c4parm_check( cb, 1, E40111 ) ) return -1L ;
            #endif

            if ( value < 0 && value != r4check ) return (long) r4check ;
            if ( value == r4check ) return (long) cb->hWnd ;
            temp = cb->hWnd ;
            #ifdef S4WIN32
               cb->hWnd = (HWND) value ;
            #else
               cb->hWnd = (unsigned int) value ;
            #endif
            return (long) temp ;
         #endif
      }
   #endif

   short S4FUNCTION code4lockAttempts( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40112 ) ) return -1 ;
      #endif

      if ( value < -1 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->lockAttempts ) ;
      temp = cb->lockAttempts ;
      cb->lockAttempts = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4lockAttemptsSingle( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40112 ) ) return -1 ;
      #endif

      if ( value < -1 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->lockAttemptsSingle ) ;
      temp = cb->lockAttemptsSingle ;
      cb->lockAttemptsSingle = value ;
      return( temp ) ;
   }

   long S4FUNCTION code4lockDelay( CODE4 *cb, long value )
   {
      long temp ;

   /*   'change error code */
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40112 ) ) return -1 ;
      #endif

      if (( value < 0 && value != r4check ) || ( (unsigned long)value > UINT_MAX ) )
         return( r4check ) ;
      if ( value == r4check ) return( (long)cb->lockDelay ) ;

      temp = (long)cb->lockDelay ;
      cb->lockDelay = (unsigned int)value ;

      return( temp ) ;
   }

   short S4FUNCTION code4lockEnforce( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, 0 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->lockEnforce), value ) ;
   }

   #ifndef S4CB51
   short S4FUNCTION code4log( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40154 ) ) return -1 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->log ) ;
      temp = cb->log ;
      cb->log = value ;
      return( temp ) ;
   }
   #endif

   short S4FUNCTION code4memExpandBlock( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40113 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memExpandBlock ) ;
      temp = cb->memExpandBlock ;
      cb->memExpandBlock = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memExpandData( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40114 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memExpandData ) ;
      temp = cb->memExpandData ;
      cb->memExpandData = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memExpandIndex( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40115 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memExpandIndex ) ;
      temp = cb->memExpandIndex ;
      cb->memExpandIndex = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memExpandLock( CODE4 *cb, short value )
   {
      short temp ;

   /*   'change error code */
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40115 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memExpandLock ) ;
      temp = cb->memExpandLock ;
      cb->memExpandLock = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memExpandTag( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40116 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memExpandTag ) ;
      temp = cb->memExpandTag ;
      cb->memExpandTag = value ;
      return( temp ) ;
   }

   long S4FUNCTION code4memSizeBlock( CODE4 *cb, long value )
   {
      long temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40117 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return (long)  ( cb->memSizeBlock ) ;
      temp = (long) cb->memSizeBlock ;
      cb->memSizeBlock = (unsigned) value ;
      return( temp ) ;
   }

   long S4FUNCTION code4memSizeBuffer( CODE4 *cb, long value )
   {
      long temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40118 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return (long) cb->memSizeBuffer ;
      temp = (long) cb->memSizeBuffer ;
      cb->memSizeBuffer = (unsigned) value ;
      return  ( temp ) ;
   }

   short S4FUNCTION code4memSizeMemo( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40119 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return (int) ( cb->memSizeMemo ) ;
      temp = (int) cb->memSizeMemo ;
      cb->memSizeMemo = (unsigned) value ;
      return( temp ) ;
   }

   long S4FUNCTION code4memSizeMemoExpr( CODE4 *cb, long value )
   {
      long temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40120 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return (long) cb->memSizeMemoExpr ;
      temp = (long) cb->memSizeMemoExpr ;
      cb->memSizeMemoExpr = (unsigned) value ;
      return  ( temp ) ;
   }

   long S4FUNCTION code4memSizeSortBuffer( CODE4 *cb, long value )
   {
      long temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40121 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return (long) cb->memSizeSortBuffer ;
      temp = (long) cb->memSizeSortBuffer ;
      cb->memSizeSortBuffer = (unsigned) value ;
      return  ( temp ) ;
   }

   long S4FUNCTION code4memSizeSortPool( CODE4 *cb, long value )
   {
      long temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40122 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return (long) cb->memSizeSortPool ;
      temp = (long) cb->memSizeSortPool ;
      cb->memSizeSortPool = (unsigned) value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memStartBlock( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40123 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memStartBlock ) ;
      temp = cb->memStartBlock ;
      cb->memStartBlock = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memStartData( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40125 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memStartData ) ;
      temp = cb->memStartData ;
      cb->memStartData = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memStartIndex( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40126 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memStartIndex ) ;
      temp = cb->memStartIndex ;
      cb->memStartIndex = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memStartLock( CODE4 *cb, short value )
   {
      short temp ;

   /*   'change error code */
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40115 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memStartLock ) ;
      temp = cb->memStartLock ;
      cb->memStartLock = value ;
      return( temp ) ;
   }

   short S4FUNCTION code4memStartTag( CODE4 *cb, short value )
   {
      short temp ;

      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40127 ) ) return 0 ;
      #endif

      if ( value < 0 && value != r4check ) return( r4check ) ;
      if ( value == r4check ) return( cb->memStartTag ) ;
      temp = cb->memStartTag ;
      cb->memStartTag = value ;
      return( temp ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4offError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errOff( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40128 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errOff), value ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4openError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errOpen( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40129 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errOpen), value ) ;
   }

   short S4FUNCTION code4optimize( CODE4 *cb, short value )
   {
      #ifndef S4OPTIMIZE_OFF
         short temp ;

         #ifdef E4VBASIC
            if ( c4parm_check( cb, 1, E40130 ) ) return -1 ;
         #endif

         if ( value < -1 && value != r4check ) return( r4check ) ;
         if ( value == r4check ) return( cb->optimize ) ;
         temp = cb->optimize ;
         cb->optimize = value ;
         return( temp ) ;
      #else
         return 0 ;
      #endif
   }

   short S4FUNCTION code4optimizeWrite( CODE4 *cb, short value )
   {
      #ifdef S4OFF_OPTIMIZE
         return 0 ;
      #else
         short temp ;

         #ifdef E4VBASIC
            if ( c4parm_check( cb, 1, E40131 ) ) return -1 ;
         #endif

         if ( value < -1 && value != r4check ) return( r4check ) ;
         if ( value == r4check ) return( cb->S4CONV( optimizeWrite, optimize_write ) ) ;
         temp = cb->S4CONV( optimizeWrite, optimize_write ) ;
         cb->S4CONV( optimizeWrite, optimize_write )= value ;
         return( temp ) ;
      #endif
   }

   short S4FUNCTION code4readLock( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40132 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->readLock), value ) ;
   }

   short S4FUNCTION code4readOnly( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40133 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->readOnly), value ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4relateError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errRelate( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40101 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errRelate), value ) ;
   }

   short S4FUNCTION code4safety( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40135 ) ) return -1 ;
      #endif

      if (value == r4check) return (!!(cb->safety))*(-1);

      return c4setLog( &(cb->safety), value ) ;
   }

   short S4FUNCTION code4singleOpen( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, 0 ) ) return -1 ;
      #endif

      if (value == r4check) return (!!(cb->singleOpen))*(-1);

      return c4setLog( &(cb->singleOpen), value ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4skipError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errSkip( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40136 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errSkip), value ) ;
   }

   #ifdef S4CB51
   short S4FUNCTION code4tagNameError( CODE4 *cb, short value )
   #else
   short S4FUNCTION code4errTagName( CODE4 *cb, short value )
   #endif
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40137 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->errTagName), value ) ;
   }

   short S4FUNCTION code4tranStatusCB( CODE4 *cb )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40133 ) ) return -1 ;
      #endif

      return code4tranStatus( cb ) ;
   }

   short S4FUNCTION code4unlockAutoCB( CODE4 *cb )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40133 ) ) return -1 ;
      #endif

      return code4unlockAuto( cb ) ;
   }

   void S4FUNCTION code4unlockAutoSetCB( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, E40133 ) ) return ;
      #endif

      code4unlockAutoSet( cb, value ) ;
   }

   short S4FUNCTION code4useGeneralTagsInRelate( CODE4 *cb, short value )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( cb, 1, 0 ) ) return -1 ;
      #endif

      return c4setLog( &(cb->useGeneralTagsInRelate), value ) ;
   }


   /*
      DATA4 CLASS TRANSLATIONS
      A lot of these functions required because VB-32 still uses 2 byte ints
      Others are required because of source code macros. A few are actually
      needed to make specific enhancements for CodeBase from VB (ie d4init)
   */

   #if !defined(S4WINCE) && !defined(S4WIN64)   /* LY 00/09/20 */
      CODE4 S4PTR* S4FUNCTION code4initP( void )
      {
         CODE4 *c4 ;

         #ifdef S4WIN16
            SetHandleCount( 100 );
         #endif

         c4 = code4alloc( 1 ) ;
         if (c4==NULL)
            error4( 0, e4memory, 0 );

         #ifdef S4WINDOWS
         c4->hWnd = 0 ;
            #ifdef S4WIN32
               c4->hInst = (HINSTANCE)GetWindowLong( 0, GWL_HINSTANCE ) ;
            #else
               c4->hInst = GetWindowWord( 0, GWW_HINSTANCE ) ;
            #endif
         #endif

         return c4 ;
      }
   #endif

   CODE4 S4PTR* S4FUNCTION code4initVB( void )
   {
      CODE4 *c4 ;

      c4 = code4alloc( 1 ) ;
      if (c4==NULL)
         error4( 0, e4memory, 0 );
      #ifdef S4WINDOWS
         c4->hWnd = 0 ;
         c4->hInst = cb5inst ;
      #endif
      return( c4 ) ;
   }

   short S4FUNCTION d4logVB( DATA4 *d4, short logFlag )
   {
      return (short) d4log( d4, (int)logFlag ) ;
   }

   short S4FUNCTION d4optimizeVB( DATA4 *d4, short flag )
   {
      return (short) d4optimize( d4, (int)flag ) ;
   }

   short S4FUNCTION d4optimizeWriteVB( DATA4 *d4, short flag )
   {
      return (short) d4optimizeWrite( d4, (int)flag ) ;
   }

   long S4FUNCTION d4recWidth_v( DATA4 *data )
   {
      // AS May 28/02 - don't use the exposed function with block preprocessing because we modify the record
      #ifndef S4INTERNAL_COMPILE_CHECK
         return (long)d4recWidth( data ) ;
      #endif
   }

   int S4FUNCTION d4tagSelectP( DATA4 S4PTR * d4, TAG4 S4PTR * tag )
   {
      #ifdef E4PARM_NULL
         if ( d4 == 0 || tag == 0 )
            return error4( 0, e4parmNull, E92409 ) ;
      #endif

      d4tagSelect( d4, tag ) ;
      return d4->codeBase->errorCode ;
   }

   short S4FUNCTION d4writeVB(DATA4 *data, long recno)
   {
      return (short)d4write(data, recno) ;
   }


   /* DATE4 CLASS TRANSLATIONS */

   /* These functions needed because some date4 functions macroed to c4atol */

   short S4FUNCTION date4day_v( char *dateString )
   {
      return date4day( dateString ) ;
   }

   short S4FUNCTION date4month_v( char *dateString )
   {
      return date4month( dateString ) ;
   }

   short S4FUNCTION date4year_v( char *dateString )
   {
      return date4year( dateString ) ;
   }


   /* ERROR4 Class translations */

   short S4FUNCTION error4VB( CODE4 *c4, short errCode, long extraInfo )
   {
      return (short) error4( c4, (int)errCode, extraInfo ) ;
   }

   short S4FUNCTION error4describeVB( CODE4 *c4, short errCode, long extraInfo, char* desc1, char* desc2, char* desc3 )
   {
      return (short) error4describe( c4, (int)errCode, extraInfo, desc1, desc2, desc3 ) ;
   }

   short S4FUNCTION error4fileVB( CODE4 *c4, char* fileName, short overWrite )
   {
      return (short) error4file( c4, fileName, (int)overWrite ) ;
   }

   short S4FUNCTION error4setVB( CODE4 *c4, short errCode )
   {
      return (short) error4set( c4, (int)errCode ) ;
   }

   /* EXPR4 CLASS TRANSLATIONS */

   DATA4 S4PTR* S4FUNCTION expr4dataCB( EXPR4* expr )
   {
      if (!expr)
      {
         error4( 0, e4parmNull, E90904 ) ;
         return 0 ;
      }

      return expr4data( expr ) ;
   }

   void S4FUNCTION expr4freeCB( EXPR4 *expr )
   {
      if (!expr)
         error4( 0, e4parmNull, E95910 ) ;
      else
         expr4free( expr ) ;
   }

   long S4FUNCTION expr4lenCB( EXPR4 *expr )
   {
      if (!expr)
         return error4( 0, e4parmNull, E90922 ) ;
      return expr4len( expr ) ;
   }

   short S4FUNCTION expr4typeCB( EXPR4 *expr )
   {
      if (!expr)
         return error4( 0, e4parmNull, E90922 ) ;
      return expr4type( expr ) ;
   }

   EXPR4 S4PTR* S4FUNCTION expr4parseCB( DATA4 *data, char S4PTR *string )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E90904 ) ) return 0 ;
      #endif

      if (!data)
      {
         error4( 0, e4parmNull, E90904 ) ;
         return 0 ;
      }

      return expr4parse( data, string ) ;
   }


   /* FIELD4 Class Translations */

   void S4FUNCTION f4assignCharVB( FIELD4 *f4, short character )
   {
      f4assignChar( f4, (int)character ) ;
   }

   void S4FUNCTION f4assignIntVB( FIELD4 *field, const short iValue )
   {
      f4assignInt( field, (const int)iValue ) ;
   }

   void S4FUNCTION f4assignNVB( FIELD4 *field, const char* data, short dataLen )
   {
      f4assignN( field, data, (int)dataLen ) ;
   }

   short S4FUNCTION f4memoAssignNVB( FIELD4 *field, char* data, short dataLen )
   {
      return (short)f4memoAssignN( field, data, (int)dataLen ) ;
   }


   /* RELATE4 CLASS TRANSLATIONS */

   DATA4 S4PTR* S4FUNCTION relate4dataCB( RELATE4 *r4 )
   {
      #ifdef S4CB51
         error4( r4->codeBase, e4notSupported, E40140 ) ;
         return 0 ;
      #else
         #ifdef E4VBASIC
            if ( c4parm_check( r4, 5, E40140 ) )
               return 0 ;
         #endif

         return relate4data( r4 ) ;
      #endif
   }

   TAG4 S4PTR* S4FUNCTION relate4dataTagCB( RELATE4 *r4 )
   {
      #ifdef S4CB51
         error4( r4->codeBase, e4notSupported, E40141 ) ;
         return 0 ;
      #else
         #ifdef E4VBASIC
            if ( c4parm_check( r4, 5, E40141 ) )
               return 0 ;
         #endif

         return relate4dataTag( r4 ) ;
      #endif
   }

   #ifdef S4CB51
      short S4FUNCTION relate4do_v( RELATE4 *r4 )
      {
         return relate4do( r4 ) ;
      }
   #endif

   short S4FUNCTION relate4errorActionVB( RELATE4* r4, short action )
   {
      return (short)relate4errorAction( r4, (int)action ) ;
   }

   short S4FUNCTION relate4freeVB( RELATE4* r4, short closeFiles )
   {
      return (short)relate4free( r4, (int)closeFiles ) ;
   }

   RELATE4 S4PTR* S4FUNCTION relate4masterCB( RELATE4 *r4 )
   {
      #ifdef S4CB51
         error4( r4->codeBase, e4notSupported, E40142 ) ;
         return 0 ;
      #else
         #ifdef E4VBASIC
            if ( c4parm_check( r4, 5, E40142 ) ) return 0 ;
         #endif

         return relate4master( r4 ) ;
      #endif
   }

   const char S4PTR* S4FUNCTION relate4masterExprCB( RELATE4 *r4 )
   {
      #ifdef S4CB51
         error4( r4->codeBase, e4notSupported, E40144 ) ;
         return 0 ;
      #else
         #ifdef E4VBASIC
            if ( c4parm_check( r4, 5, E40144 ) ) return 0 ;
         #endif

         return relate4masterExpr( r4 ) ;
      #endif
   }

   short S4FUNCTION relate4matchLenVB( RELATE4* r4, short len )
   {
      return (short)relate4matchLen( r4, (int)len ) ;
   }

   short S4FUNCTION relate4skipEnableVB( RELATE4* r4, short doEnable )
   {
      return (short)relate4skipEnable( r4, (int)doEnable ) ;
   }

   RELATE4 S4PTR* S4FUNCTION relate4topMaster( RELATE4 *r4 )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( r4, 5, E40143 ) ) return 0 ;
      #endif

      #ifdef S4CB51
         return (RELATE4 *)r4->relation ;
      #else
         return relate4master( r4 ) ;
      #endif
   }

   short S4FUNCTION relate4typeVB( RELATE4* r4, short type )
   {
      return (short)relate4type( r4, (int)type ) ;
   }

   #if defined(S4WIN32) || defined(S4WIN16)
      int S4FUNCTION report4decimal_v( PREPORT4 report, char *decimal )
      {
         #ifndef S4OFF_REPORT
            #ifdef E4VBASIC
               if ( c4parm_check( report, 6, 0 ) ) return -1 ;
            #endif

            #ifndef S4VB_DOS
               return report4decimal( report, (char) decimal[0] ) ;
            #else
               return report4decimal( report, (char) c4str(decimal)[0] ) ;
            #endif
         #else
            return e4notSupported;
         #endif
      }

      RELATE4 S4PTR * S4FUNCTION report4relate( REPORT4 *r )
      {
         #ifdef E4VBASIC
            if ( r == 0 )
            {
               error4( 0, e4parmNull, E40145 ) ;
               return 0 ;
            }
         #endif

         #ifdef S4OFF_REPORT
            error4( r->codeBase, e4notSupported, 0 ) ;
            return 0;
         #else
            return r->relate ;
         #endif
      }

      short S4FUNCTION report4screenBreaks( REPORT4* r4, short value )
      {
         #ifdef E4VBASIC
            if ( r4 == 0 )
               return error4( 0, e4parmNull, E40155 ) ;
         #endif

         #ifdef S4OFF_REPORT
            return error4( r4->codeBase, e4notSupported, 0 ) ;
         #else
            short temp ;

            if ( value == r4check )
               return r4->screen_breaks ;
            if ( value < 0 )
               return( r4check ) ;

            temp = r4->screen_breaks ;

            if( value != r4check )
               r4->screen_breaks = ( value > 0 ) ;

            return temp ;
         #endif
      }

      long __stdcall RepWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
      {
         #if !defined(S4OFF_REPORT) && defined(S4WINDOWS)
            static HWND userhWnd ;

            switch ( message )
            {
            /*
               case WM_CREATE :
                  break ;
            */
               case WM_USER :
                  userhWnd = (HWND) wParam ;
                  EnableWindow( userhWnd, FALSE ) ;
                  return (long) report4do( (REPORT4 *) lParam ) ;

               case CRM_REPORTCLOSED :
                  EnableWindow( userhWnd, TRUE ) ;
                  SetFocus( userhWnd ) ;
                  /* SendMessage( hWnd, WM_DESTROY, (WPARAM) NULL, (LPARAM) NULL ) ; */
                  SendMessage( hWnd, WM_CLOSE, (WPARAM) NULL, (LPARAM) NULL ) ;
                  return 0 ;

               case WM_DESTROY :
                  PostQuitMessage(0) ;
                  return 0 ;
            }

            return DefWindowProc( hWnd, message, wParam, lParam ) ;
         #else
            return e4notSupported;
         #endif
      }

      void report4registerClass( REPORT4 *r4 )
      {
         #if !defined(S4OFF_REPORT) && defined(S4WINDOWS)
            WNDCLASS wndclass, temp ;

            if( GetClassInfo( (HINSTANCE)r4->hWnd, TEXT("RepClass"), &temp ) )
               UnregisterClass( TEXT("RepClass"), (HINSTANCE)r4->hWnd ) ;

            wndclass.style = CS_GLOBALCLASS ;
            wndclass.lpfnWndProc = RepWndProc ;
            wndclass.cbClsExtra = 0 ;
            wndclass.cbWndExtra = PWIN_OFF_OLDOBJ + sizeof(long) ;
            wndclass.hInstance = cb5inst ;
            /* wndclass.hInstance = GetWindowWord( r4->hWnd, GWW_HINSTANCE ) ; */
            #ifndef S4WINCE
               wndclass.hIcon = LoadIcon( (HINSTANCE) NULL, IDI_APPLICATION) ;
            #endif
            wndclass.hCursor = LoadCursor( (HINSTANCE) NULL,IDC_ARROW) ;
            wndclass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH) ;
            wndclass.lpszMenuName = NULL ;
            wndclass.lpszClassName = TEXT("RepClass") ;  // CS 2001/06/22

            RegisterClass(&wndclass) ;
         #endif
      }

      void report4unregisterClass( REPORT4 *r4 )
      {
         #if !defined(S4OFF_REPORT) && defined(S4WINDOWS)
            WNDCLASS temp ;

            if( GetClassInfo( (HINSTANCE)r4->hWnd, TEXT("RepClass"), &temp ) )
               UnregisterClass( TEXT("RepClass"), (HINSTANCE)r4->hWnd ) ;
         #endif
      }

      short S4FUNCTION report4doCB( REPORT4 *r4 )
      {
         #ifdef E4PARM_HIGH
            if( r4 == 0 )
               return error4( 0, e4parm_null, E95702 ) ;
         #endif

         #if defined(S4OFF_REPORT) || !defined(S4WINDOWS)
            return error4( r4->codeBase, e4notSupported, 0 ) ;
         #else
            HWND hWnd, userhWnd ;
            MSG msg ;
            short rc ;

            if ( ! r4->hWnd ) return - 1 ;

            userhWnd = r4->hWnd ;

            /* if report is going to printer don't need a window */
            if ( r4->output_code == 0 ) return report4do(r4) ;

            report4registerClass( r4 )  ;

            hWnd = CreateWindow( TEXT("RepClass"), TEXT("RepWindow"), WS_OVERLAPPED,
                     0, 0, 0, 0, (HWND) NULL, (HMENU) NULL, cb5inst, (void FAR*) NULL);

         /*      r4->codeBase->hInst, (void FAR*) NULL); */

            if ( ! hWnd ) return -1 ;

            r4->hWnd = hWnd ;

            rc = (int) SendMessage( hWnd, WM_USER, (WPARAM) userhWnd, (LPARAM) r4  ) ;

            if ( r4->output_code )
            {
               while ( GetMessage ( &msg, (HWND) NULL, (UINT) 0 , (UINT) 0 ) )
               {
                  TranslateMessage( &msg ) ;
                  DispatchMessage( &msg ) ;
               }
            }

            r4->hWnd = userhWnd ;
            /*
               JH 01/12/00 - fix for Visual Basic
               RepClass is still registered - may cause Unhandled Exception
               if report4do called again - call UnregisterClass
            */
            UnregisterClass( TEXT("RepClass"), (HINSTANCE)hWnd ) ;

            return rc ;
         #endif
      }
   #endif  // S4WIN16/32

   /* TAG4 CLASS TRANSLATIONS */

   const char S4PTR *S4FUNCTION t4exprCB( TAG4 *tag )
   {
      return t4expr( tag ) ;
   }

   const char S4PTR *S4FUNCTION t4filterCB( TAG4 *tag )
   {
      return t4filter( tag ) ;
   }

   short S4FUNCTION t4uniqueSetVB( TAG4* t4, const short uniqueCode )
   {
      return (short)t4uniqueSet( t4, uniqueCode ) ;
   }

   #ifdef S4CB51
      short S4FUNCTION tag4uniqueError( TAG4 *t4, short value )
      {
         short rc, temp ;

         #ifdef E4VBASIC
            if ( c4parm_check( t4, 4, E40151 ) ) return -1 ;
         #endif

         if ( value < 0 && value != r4check ) return( r4check ) ;
         if ( value == r4check ) return t4unique( t4 ) ;

         temp = t4unique( t4 ) ;
         rc = t4uniqueSet( t4, value ) ;
         if( rc != 0 )
            return error4( t4->index->codeBase, rc, E91601 ) ;

         return( temp ) ;

      }
   #endif /* ifdef S4CB51 */


   #ifndef S4LUPACH  /* LY 2003/07/09 */
   TAG4 *S4FUNCTION t4openCB(DATA4 *d4, char *name)
   {
      return t4open( d4, (INDEX4 *)0, name ) ;
   }
   #endif

   void S4FUNCTION u4memCpy( char *dest, char *source, long len)
   {
      memcpy(dest, source, (unsigned int)len) ;
   }

   short S4FUNCTION u4strLen(char *s)
   {
      return (short)strlen(s) ;
   }

   /* Needed for CSA application */

   long S4FUNCTION u4ptrPtr2Long( void **ptr, int index)
   {
      return *(long *) ptr[index];
   }

   int S4FUNCTION d4lockVB( DATA4 *data, const long rec )
   {
      return d4lock(data,rec);
   }

   int S4FUNCTION d4lockAllVB( DATA4 *data )
   {
      return d4lockAll(data);
   }

   int S4FUNCTION d4lockAppendVB( DATA4 *data )
   {
      return d4lockAppend(data);
   }

   int S4FUNCTION d4lockFileVB( DATA4 *data )
   {
      return d4lockFile(data);
   }

   struct PIPE4VBINFO
   {
      unsigned long msgLen ;
      long msgPtr ;
   } ;

   S4EXPORT struct PIPE4VBINFO S4FUNCTION pipe4recvMessageVB( PIPE4RECV *pipe )
   {
      struct PIPE4VBINFO info ;

      #ifdef S4CLIENT
         if ( pipe4recvMessage( pipe, &info.msgLen, (void **)&info.msgPtr ) != r4success )
      #else
         error4( 0, e4notSupported, E96992 ) ;
         info.msgPtr = 0 ;
      #endif
            info.msgLen = 0 ;

      return info ;
   }
   /*
      Dec 6/95 - 'nulls' member added to FIELD4INFO struct, creating a
      naturally four byte aligned structured. Therefore this structure
      and functions are no longer required. However, it could be used
      again if the FIELD4INFO struct ever changes.


   #ifdef S4WIN32

      This function is required for VB 32-bit because it uses 4 byte alignment,
      which adds an extra two bytes to the FIELD4INFO structure, which is only
      10 bytes long.


   typedef struct
   {
      char S4PTR *name ;
      short int type ;
      unsigned short int len ;
      unsigned short int dec ;
      short int dummy ;
   } FIELD4INFOVB32 ;

   S4EXPORT DATA4 * S4FUNCTION d4createVB( CODE4 *cb, char *name, FIELD4INFOVB32* fldInfo, TAG4INFO *tagInfo)
   {
      int i = 0, numFlds = 0, numTags = 0 ;
      FIELD4INFO *fldInfoC ;
      DATA4 *data ;

      while ( fldInfo[i].name )
      {
         numFlds++ ;
         i++ ;
      }

      if ( ! (fldInfoC = (FIELD4INFO *)u4alloc( sizeof(FIELD4INFO) * (numFlds + 1) ) ) )
         error4describe( cb, e4create, E91401, name, "Out of Memory", 0 ) ;

      for( i = 0 ; i < numFlds ; i++ )
      {
         fldInfoC[i].name = fldInfo[i].name ;
         fldInfoC[i].type = fldInfo[i].type ;
         fldInfoC[i].len  = fldInfo[i].len ;
         fldInfoC[i].dec  = fldInfo[i].dec ;
         fldInfoC[i].nulls = fldInfo[i].nulls ;
      }

      data = d4create( cb, name, fldInfoC, tagInfo ) ;

      u4free( (void *)fldInfoC ) ;

      return data ;
   }

   #else

   S4EXPORT DATA4 * S4FUNCTION d4createVB( CODE4 *cb, char *name, FIELD4INFO* fldInfo, TAG4INFO *tagInfo)
   {
      return d4create( cb, name, fldInfo, tagInfo ) ;
   }

   #endif

   */


   /* for C# functionality */

   static char g_null[] =
   {
      "\0"
   } ;


   S4EXPORT void S4FUNCTION code4dateFormatW( CODE4 *c4, char *buff )
   {
      const char *ptr = code4dateFormat( c4 ) ;
      if ( ptr )
         strncpy( buff, ptr, LEN4DATE_FORMAT ) ;
   }


   S4EXPORT void S4FUNCTION code4indexExtensionW( CODE4 *c4, char *buff )
   {
      const char *ptr = code4indexExtension( c4 ) ;
      if ( ptr )
         strncpy( buff, ptr, 3 ) ;
   }


   S4EXPORT void S4FUNCTION code4lockFileNameW( CODE4 *c4, char *buff )
   {
      const char *ptr = code4lockFileName( c4 ) ;
      if ( ptr )
         strcpy( buff, ptr ) ;
   }


   S4EXPORT void S4FUNCTION code4lockUserIdW( CODE4 *c4, char *buff )
   {
      const char *ptr = code4lockUserId( c4 ) ;
      if ( ptr )
         strcpy( buff, ptr ) ;
   }


   S4EXPORT void S4FUNCTION code4lockNetworkIdW( CODE4 *c4, char *buff )
   {
      const char *ptr = code4lockNetworkId( c4 ) ;
      if ( ptr )
         strcpy( buff, ptr ) ;
   }


   S4EXPORT void S4FUNCTION code4logFileNameW( CODE4 *c4, char *buff )
   {
      const char *ptr = code4logFileName( c4 ) ;
      if ( ptr )
         strcpy( buff, ptr ) ;
   }


   S4EXPORT void S4FUNCTION d4aliasW( DATA4 *db, char *buff )
   {
      const char *ptr = d4alias( db ) ;
      if ( ptr )
         strncpy( buff, ptr, LEN4DATA_ALIAS ) ;
   }


   S4EXPORT CODE4* S4FUNCTION d4code( DATA4 *db )
   {
      if ( db )
         return db->codeBase ;
      else
         return 0 ;
   }


   S4EXPORT void S4FUNCTION d4fileNameW( DATA4 *db, char *buff )
   {
      const char *ptr = d4fileName( db ) ;
      if ( ptr )
         strcpy( buff, ptr ) ;
   }


   S4EXPORT INDEX4* S4FUNCTION d4nextIndex( CODE4 *cb, DATA4 *db, INDEX4 *index )
   {
      INDEX4 *idx = 0 ;

      if ( db )
         idx = (INDEX4*) l4next( &(db->indexes), index ) ;
      else
         error4describe( cb, e4parmNull, 00000L, "d4nextIndex", 0, 0 ) ;

      return idx ;
   }


   S4EXPORT char * S4FUNCTION date4alloc( CODE4 *cb, char *pict )
   {
      char *ptr ;
      ptr = (char *) u4allocErr( cb, strlen(pict)+1 ) ;
      return ptr ;
   }


   S4EXPORT void S4FUNCTION date4assignW( char *ptr, char *d)
   {
      u4ncpy( ptr, d, 8 ) ;
   }


   S4EXPORT char * S4FUNCTION date4formatW( char *ptr, char *result, char *pict )
   {
      date4format( ptr, result, pict ) ;
      return result ;
   }


   S4EXPORT char * S4FUNCTION date4timeNowW( char *result )
   {
      date4timeNow( result ) ;
      return result ;
   }


   S4EXPORT void S4FUNCTION error4textW( CODE4 *c4, const long errCode2, char *buff )
   {
      strcpy( buff, error4text( c4, errCode2 ) ) ;
   }


   S4EXPORT void S4FUNCTION expr4sourceW( const EXPR4 *e4expr, char *buff )
   {
      strcpy( buff, expr4source( e4expr ) ) ;
   }


   S4EXPORT void S4FUNCTION expr4strW( EXPR4 *e4expr, char *buff )
   {
      const char *ptr = expr4str( e4expr ) ;
      if ( ptr )
         strncpy( buff, ptr, expr4len( e4expr ) ) ;
   }


   S4EXPORT void S4FUNCTION f4assignBytesW( FIELD4 *field, char *buff,
      int buffLen )
   {
      char *ptr = f4ptr( field ) ;
      if ( ptr )
      {
         if ( buff )
         {
            f4blank( field ) ;
            memcpy( ptr, buff,
               ( (int)f4len( field ) > buffLen ? buffLen : f4len( field ) ) ) ;
         }
      }
   }


   S4EXPORT void S4FUNCTION f4bytesW( FIELD4 *field, char *buff, int buffLen )
   {
      char *ptr = f4ptr( field ) ;
      if ( ptr )
      {
         if ( buff )
            memcpy( buff, ptr,
               ( (int)f4len( field ) > buffLen ? buffLen : f4len( field ) ) ) ;
      }
   }


   S4EXPORT void S4FUNCTION f4currencyW( FIELD4 *field, short dec, char *buff )
   {
      const char *ptr = f4currency( field, dec ) ;
      if ( ptr )
         strncpy( buff, ptr, 20 ) ;
   }


   S4EXPORT void S4FUNCTION f4dateTimeW( FIELD4 *field, char *buff )
   {
      const char *ptr = f4dateTime( field ) ;
      if ( ptr )
         strncpy( buff, ptr, 16 ) ;
   }


   S4EXPORT FIELD4INFO * S4FUNCTION f4infoAdd( CODE4 *cb, FIELD4INFO *fldInfo,
      int fldInfoSize, char *name, unsigned short type, int len,
      int dec, int nulls )
   {
      char *buff = 0 ;
      // unsigned short wideType[2] ;
      unsigned int oldFldInfoLen = fldInfoSize * sizeof(FIELD4INFO) ;

      if ( name != 0 )
      {
         buff = (char *) u4allocEr( cb, 11*sizeof(char) ) ;
         if ( buff )
         {
            memcpy( buff, name, 10*sizeof(char) ) ;
            c4upper( buff ) ;
         }
         else
         {
            error4describe( cb, e4memory, 00000L, "could not add field name",
               0, 0 ) ;
            return 0 ;
         }
      }

   /*   memset( wideType, 0, 2*sizeof(unsigned short) ) ;
      wideType[0] = type ;
      c4utoa( wideType ) ;*/

      if( u4allocAgain( cb, (char**)&fldInfo, &oldFldInfoLen,
         (fldInfoSize+2)*sizeof(FIELD4INFO)) != 0 )
      {
         error4describe( cb, e4memory, 00000L, "could not add field info",
            0, 0 ) ;
         if ( buff )
            u4free( buff ) ;
         return 0 ;
      }

      fldInfo[fldInfoSize].name = buff ;
      fldInfo[fldInfoSize].type = (short int) type ;
      fldInfo[fldInfoSize].len = (unsigned short int) len ;
      fldInfo[fldInfoSize].dec = (unsigned short int) dec ;
      fldInfo[fldInfoSize].nulls = (unsigned short int) nulls ;

      fldInfoSize++ ;
      fldInfo[fldInfoSize].name = 0 ;
      fldInfo[fldInfoSize].type = 0 ;
      fldInfo[fldInfoSize].len = 0 ;
      fldInfo[fldInfoSize].dec = 0 ;
      fldInfo[fldInfoSize].nulls = 0 ;

      return fldInfo ;
   }


   S4EXPORT FIELD4INFO * S4FUNCTION f4infoInit( CODE4 *cb, char *name,
      unsigned short type, int len, int dec, int nulls )
   {
      FIELD4INFO *fldInfo = 0 ;

      return f4infoAdd( cb, fldInfo, 0, name, type, len, dec, nulls ) ;
   }


   S4EXPORT void S4FUNCTION f4infoFree( FIELD4INFO *fldInfo, int fldInfoSize )
   {
      for( int i = fldInfoSize-1 ; i >= 0 ; i-- )
         if ( fldInfo[i].name )
            u4free( fldInfo[i].name ) ;
      u4free( fldInfo ) ;
   }


   S4EXPORT char S4PTR * S4FUNCTION f4infoName( CODE4 *code, FIELD4INFO *fldInfo, int fldNum )
   {
      return fldInfo[fldNum-1].name ;
   }


   S4EXPORT int S4FUNCTION f4memoAssignBytesW( FIELD4 *fld, char *buff,
      unsigned int len )
   {
      int rc ;

      if ( buff )
      {
         rc = f4memoSetLen( fld, len ) ;
         if ( rc == r4success )
            memcpy( f4memoPtr( fld ), buff, len ) ;

         return rc ;
      }
      else
         return e4parmNull ;
   }


   S4EXPORT unsigned int S4FUNCTION f4memoBytesW( FIELD4 *fld, char *buff,
      unsigned int len )
   {
      unsigned int retLen = 0 ;
      char *ptr ;

      if ( buff )
      {
         ptr = f4memoPtr( fld ) ;
         if ( ptr )
         {
            retLen = f4memoLen( fld ) > len ? len : f4memoLen( fld ) ;
            memcpy( buff, ptr, retLen ) ;
         }
      }

      return retLen ;
   }


   S4EXPORT void S4FUNCTION f4memoChanged( FIELD4 *fld )
   {
      fld->data->recordChanged = 1 ;
      if ( fld->memo != 0 )
         fld->memo->isChanged =  1 ;
   }


   S4EXPORT int S4FUNCTION f4memoSetLenW( FIELD4 *field, unsigned int newLen )
   {
      int rc ;
      char *buf = 0 ;

      if( field->memo == 0 )
         rc = -1 ;
      else
      {
         if( newLen > field->memo->lenMax )
         {
            buf = ( char * ) u4allocFree( ( CODE4 * ) field->data->codeBase, newLen ) ;
            memcpy( buf, field->memo->contents, field->memo->lenMax ) ;
         }
         rc = f4memoSetLen( field,newLen ) ;
         if( buf != 0 )
         {
            memcpy( field->memo->contents, buf, field->memo->lenMax ) ;
            u4free( buf ) ;
         }
      }
      return rc ;
   }


   S4EXPORT void f4memoReadW( FIELD4 *field, unsigned int readLen, unsigned int startPos, char *buff )
   {
      f4memoReadPart( field, buff, readLen, startPos ) ;
   }


   S4EXPORT void S4FUNCTION f4memoStrW( FIELD4 *field, char *buff )
   {
      const char *ptr = f4memoStr( field ) ;
      if ( ptr )
         strncpy( buff, ptr, f4memoLen( field ) ) ;
   }


   S4EXPORT void S4FUNCTION f4nameW( FIELD4 *field, char *buff )
   {
      const char *ptr = f4name( field ) ;
      if ( ptr )
         strncpy( buff, ptr, 10 ) ;
   }


   S4EXPORT void S4FUNCTION f4strW( FIELD4 *field, char *buff )
   {
      const char *ptr = f4str( field ) ;
      if ( ptr )
         strncpy( buff, ptr, f4len( field ) ) ;
   }


   S4EXPORT void S4FUNCTION f4strUnicode( FIELD4 *field, char *buff )
   {
      unsigned short *workBuff ;
      int len = f4len( field ) ;
      const char *ptr = f4ptr( field ) ;
      if ( ptr )
      {
         workBuff = (unsigned short *) u4allocErr( field->data->dataFile->c4, sizeof(unsigned short) * len + 1 ) ;
         if ( workBuff )
         {
            memset( workBuff, 0, len + 1 ) ;
            memcpy( workBuff, ptr, len ) ;
            c4utoa( workBuff ) ;
            strcpy( buff, (char *)workBuff ) ;
            u4free( workBuff ) ;
         }
      }
   }


   S4EXPORT int S4FUNCTION file4isValid( FILE4 *file )
   {
      if ( file )
         return ( file->hand == INVALID4HANDLE ? 0 : 1 ) ;
      else
         return -1 ;
   }


   S4EXPORT char * S4FUNCTION file4nameW( FILE4 *file )
   {
      if ( file )
         return (char *)file->name ;
      else
         return g_null ;
   }


   S4EXPORT CODE4* S4FUNCTION i4code( INDEX4 *idx )
   {
      if ( idx )
         return idx->codeBase ;
      else
         return 0 ;
   }


   S4EXPORT void S4FUNCTION i4fileNameW( INDEX4 *idx, char *buff )
   {
      const char *ptr = i4fileName( idx ) ;
      if ( ptr )
         strcpy( buff, ptr ) ;
   }


   S4EXPORT INDEX4 * S4FUNCTION i4openW( DATA4 *data, const char *name )
   {
      if ( strlen( name ) )
         return i4open( data, name ) ;
      else
         return i4open( data, 0 ) ;
   }


   S4EXPORT void S4FUNCTION relate4masterExprW( RELATE4 *relate, char *buff )
   {
      const char *ptr = relate4masterExpr( relate ) ;
      if ( ptr )
         strcpy( buff, ptr ) ;
   }


   S4EXPORT void S4FUNCTION t4aliasW( TAG4 *tag, char *buff )
   {
      const char *ptr = t4alias( tag ) ;
      if ( ptr )
         strncpy( buff, ptr, LEN4TAG_ALIAS ) ;
   }


   S4EXPORT void S4FUNCTION t4exprW( TAG4 *tag, char *buff )
   {
      const char *ptr ;
      if ( tag )
      {
         ptr = t4expr( tag ) ;
         if ( ptr )
            strcpy( buff, ptr ) ;
      }
   }


   S4EXPORT void S4FUNCTION t4filterW( TAG4 *tag, char *buff )
   {
      const char *ptr ;
      if ( tag )
      {
         ptr = t4filter( tag ) ;
         if ( ptr )
            strcpy( buff, ptr ) ;
      }
   }


   S4EXPORT TAG4INFO * S4FUNCTION t4infoAdd( CODE4 *cb, TAG4INFO *tagInfo, int tagInfoSize,
      char *name, char *expr, char *filter, short unique, unsigned short descending )
   {
      char *buff = 0 ;
      char *exprBuff = 0 ;
      char *filterBuff = 0 ;
      unsigned int oldTagInfoLen = tagInfoSize * sizeof(TAG4INFO) ;
      size_t len ;

      if ( name != 0 )
      {
         len = strlen( name ) ;
         buff = (char *) u4allocEr( cb, (len+1)*sizeof(char) ) ;
         if ( buff )
         {
            memcpy( buff, name, len*sizeof(char) ) ;
         }
         else
         {
            error4describe( cb, e4memory, 00000L, "could not add tag name", 0, 0 ) ;
            return 0 ;
         }
      }

      if ( expr != 0 )
      {
         len = strlen( expr ) ;
         exprBuff = (char *) u4allocEr( cb, (len+1)*sizeof(char) ) ;
         if ( exprBuff )
         {
            memcpy( exprBuff, expr, len*sizeof(char) ) ;
         }
         else
         {
            error4describe( cb, e4memory, 00000L, "could not add tag expr", 0, 0 ) ;
            if ( buff )
               u4free( buff ) ;
            return 0 ;
         }
      }

      if ( filter != 0 )
      {
         len = strlen( filter ) ;
         filterBuff = (char *) u4allocEr( cb, (len+1)*sizeof(char) ) ;
         if ( filterBuff )
         {
            memcpy( filterBuff, filter, len*sizeof(char) ) ;
         }
         else
         {
            error4describe( cb, e4memory, 00000L, "could not add tag filter", 0, 0 ) ;
            if ( buff )
               u4free( buff ) ;
            if ( exprBuff )
               u4free( exprBuff ) ;
            return 0 ;
         }
      }

      if ( u4allocAgain( cb, (char **)&tagInfo, &oldTagInfoLen,
            (tagInfoSize+2) * sizeof( TAG4INFO )) != 0 )
      {
         error4describe( cb, e4memory, 00000L, "could not add tag info", 0, 0 ) ;
         if ( buff )
            u4free( buff ) ;
         if ( exprBuff )
            u4free( exprBuff ) ;
         if ( filterBuff )
            u4free( filterBuff ) ;
         return 0 ;
      }

      tagInfo[tagInfoSize].name = buff ;
      tagInfo[tagInfoSize].expression = exprBuff ;
      tagInfo[tagInfoSize].filter = filterBuff ;
      tagInfo[tagInfoSize].unique = unique ;
      tagInfo[tagInfoSize].descending = descending ;

      tagInfo[tagInfoSize+1].name = 0 ;
      tagInfo[tagInfoSize+1].expression = 0 ;
      tagInfo[tagInfoSize+1].filter = 0 ;
      tagInfo[tagInfoSize+1].unique = 0 ;
      tagInfo[tagInfoSize+1].descending = 0 ;

      return tagInfo ;
   }


   S4EXPORT TAG4INFO * S4FUNCTION t4infoInit( CODE4 *cb, char *name, char *expr,
      char *filter, short unique, unsigned short descending )
   {
      TAG4INFO *tagInfo = 0 ;

      return t4infoAdd( cb, tagInfo, 0, name, expr, filter, unique, descending ) ;
   }


   S4EXPORT int S4FUNCTION t4infoDel( CODE4 *cb, TAG4INFO *tagInfo, int tagInfoSize, int index )
   {
      if ( tagInfo )
      {
         if ( tagInfo[index].name )
            u4free( tagInfo[index].name ) ;
         if ( tagInfo[index].expression )
            u4freeDefault( (void *)tagInfo[index].expression ) ;
         tagInfo[index].expression = 0 ;
         if ( tagInfo[index].filter )
            u4freeDefault( (void *)tagInfo[index].filter ) ;
         tagInfo[index].filter=0;
         memcpy( tagInfo+index, tagInfo+index+1, sizeof(TAG4INFO) * (tagInfoSize-index) ) ;
         return 0 ;
      }
      else
         return error4describe( cb, e4parmNull, 00000L, "t4infoDel", 0, 0 ) ;
   }


   S4EXPORT unsigned short S4FUNCTION t4infoDescend( CODE4 *cb, TAG4INFO *tagInfo,
      int tagPos )
   {
      if ( tagInfo )
      {
         if( tagInfo[tagPos].name != 0 )
            return tagInfo[tagPos].descending ;
      }
      else
         error4describe( cb, e4parmNull, 00000L, "t4infoDescend", 0, 0 ) ;

      return 0 ;
   }


   S4EXPORT char * S4FUNCTION t4infoExpr( CODE4 *cb, TAG4INFO *tagInfo, int tagPos )
   {
      if ( tagInfo )
      {
         if( tagInfo[tagPos].name != 0 )
         {
            return (char *)tagInfo[tagPos].expression ;
         }
      }
      else
         error4describe( cb, e4parmNull, 00000L, "t4infoExpr", 0, 0 ) ;

      return g_null ;
   }


   S4EXPORT char * S4FUNCTION t4infoFilterW( CODE4 *cb, TAG4INFO *tagInfo, int tagPos )
   {
      if ( tagInfo )
      {
         if( tagInfo[tagPos].name != 0 )
         {
            if ( tagInfo[tagPos].filter )
            {
               return (char*) tagInfo[tagPos].filter ;
            }
         }
      }
      else
         error4describe( cb, e4parmNull, 00000L, "t4infoExprW", 0, 0 ) ;

      return g_null ;
   }


   /* for TAG4INFO returned by i4tagInfo() */
   S4EXPORT void S4FUNCTION t4infoFreeInternal( TAG4INFO *tagInfo )
   {
      u4free( tagInfo ) ;
   }


   /* for TAG4INFO returned by t4infoAdd() */
   S4EXPORT void S4FUNCTION t4infoFree( TAG4INFO *tagInfo, int tagInfoSize )
   {
      if ( tagInfo )
      {
         for( ; tagInfoSize > 0 ; )
         {
            tagInfoSize -- ;
            u4freeDefault( (void *)tagInfo[tagInfoSize].name ) ;
            u4freeDefault( (void *)tagInfo[tagInfoSize].expression ) ;
            u4freeDefault( (void *)tagInfo[tagInfoSize].filter ) ;
         }
         u4free( tagInfo ) ;
      }
   }


   S4EXPORT char * S4FUNCTION t4infoName( CODE4 *cb, TAG4INFO *tagInfo, int tagPos )
   {
      if ( tagInfo )
      {
         if( tagInfo[tagPos].name != 0 )
         {
            return tagInfo[tagPos].name ;
         }
      }
      else
         error4describe( cb, e4parmNull, 00000L, "t4infoNameW", 0, 0 ) ;

      return g_null ;
   }


   S4EXPORT int S4FUNCTION t4infoSize( CODE4 *cb, TAG4INFO *tagInfo )
   {
      int i = 0 ;

      if ( tagInfo )
      {
         while ( tagInfo[i].name != 0 )
            i++ ;
         return i ;
      }
      else
         return error4describe( cb, e4parmNull, 00000L, "t4infoSize", 0, 0 ) ;
   }


   S4EXPORT short S4FUNCTION t4infoUnique( CODE4 *cb, TAG4INFO *tagInfo, int tagPos )
   {
      if ( tagInfo )
      {
         if( tagInfo[tagPos].name != 0 )
            return tagInfo[tagPos].unique ;
      }
      else
         error4describe( cb, e4parmNull, 00000L, "t4infoUnique", 0, 0 ) ;

      return 0 ;
   }


   S4EXPORT TAG4INFO * S4FUNCTION t4tagInfo( TAG4 S4PTR *tag )
   {
      TAG4INFO *tagInfo = 0 ;

      if ( tag )
         tagInfo = i4tagInfo( tag->index ) ;
      else
         error4describe( 0, e4parmNull, 00000L, "t4tagInfo", 0, 0 ) ;

      return tagInfo ;
   }
   #ifdef __cplusplus
      }
   #endif
#endif  /* S4DLL_BUILD */

// AS Oct 24/01 - OLE-DB always requires this function
long S4FUNCTION code4memStartMax( CODE4 *cb, long value )
{
   long temp ;

   #ifdef E4VBASIC
      if ( c4parm_check( cb, 1, E40124 ) ) return 0 ;
   #endif

   if( value < 0 && value != r4check )
      return( r4check ) ;
   if ( value == r4check )
      return( cb->memStartMax ) ;

   temp = cb->memStartMax ;
   cb->memStartMax = value ;

   return( temp ) ;
}



