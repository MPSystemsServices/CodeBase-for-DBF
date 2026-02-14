/* e4expr.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

char **expr4 ;
char  *expr4constants ;
E4INFO *expr4infoPtr ;
EXPR4 *expr4ptr ;

#ifndef S4CLIENT
   /* AS May 7, 2014 clears the expr4 context if necessary to avoid bad pointers and so forth when a clone is closed */
   int S4FUNCTION expr4contextClear( EXPR4 *expr, DATA4 *data )
   {
      short int i ;
      #ifdef E4PARM_LOW
         if ( expr == 0 || data == 0 )
            return error4( 0, e4parm_null, E90918 ) ;
      #endif

      if ( expr->data == data )  // only clear the context if the context is already set to the current context
      {
         expr->data = 0 ;
         expr->dataFile = 0 ;
         for ( i = 0 ; i < expr->infoN ; i++ )   /* update FIELD4's if reqd */
         {
            if ( expr->info[i].fieldNo != 0 )
               if ( expr->info[i].fieldPtr != 0 )
                  if ( expr->info[i].localData == 1 )
                     expr->info[i].fieldPtr = 0 ;
         }
      }
      return 0 ;
   }

   /* sets the data4 context for the expression */
   int S4FUNCTION expr4context( EXPR4 *expr, DATA4 *data )
   {
      short int i ;
      #ifdef E4PARM_LOW
         if ( expr == 0 || data == 0 )
            return error4( 0, e4parm_null, E90918 ) ;
      #endif

      // AS Jun 15/05 - try relaxing this constraint...what if the DATA4 was closed and re-opened???
      // if ( expr->data != data )
      {
         expr->data = data ;
         expr->dataFile = data->dataFile ;
         for ( i = 0 ; i < expr->infoN ; i++ )   /* update FIELD4's if reqd */
         {
            if ( expr->info[i].fieldNo != 0 )
               if ( expr->info[i].fieldPtr != 0 )
                  if ( expr->info[i].localData == 1 )
                     expr->info[i].fieldPtr = d4fieldJ( data, expr->info[i].fieldNo ) ;
         }
      }
      data->dataFile->record = d4record( data ) ;
      return 0 ;
   }
#endif /* S4CLIENT */

#ifdef S4SEMAPHORE   /* LY 2002/02/20 : switched with S4WIN32 */
   #ifdef S4WIN32
      /* multi-thread support */
      extern CRITICAL_SECTION critical4expression ;
   #endif
   #ifdef S4LINUX
      extern pthread_mutex_t critical4expression ; /* LY 2002/02/20 : protect expr module */
   #endif
#endif


int expr4reset( EXPR4 *expr )
{
   /* resets the expression module without entering critical section.  Useful by itself
      when performing a calc where we need to reset the information for the calc but
      are already in the expression module */
   // AS Jun 14/05 - if expr == 0, fail out
   #ifdef E4PARM_LOW
      if ( expr == 0 )
         return error4( 0, e4parm_null, E90912 ) ;
   #endif
   expr4buf = expr->exprWorkBuf ;  /* initialize the working buffer pointer */
   #ifndef S4CLIENT
      if ( expr->tagPtr )  /* is a tag, so verift context validity */
         if ( expr->dataFile->record == 0 )
            return expr4context( expr, expr->data ) ;
   #endif
   return 0 ;
}



int expr4start( EXPR4 *expr )
{
   #ifdef S4SEMAPHORE
      #ifdef S4OS2
         APIRET rc ;

         rc = DosRequestMutexSem( expr->codeBase->hmtxExpr, -1 ) ;
         if ( rc != 0 )
            return error4( expr->codeBase, e4info, "OS/2 Semaphore Failure" ) ;
      #endif
      #ifdef S4WIN32
         EnterCriticalSection( &critical4expression ) ;
      #endif
      #ifdef S4LINUX /* LY 2002/02/20 */
         int rc ;

         rc = pthread_mutex_lock( &critical4expression ) ;
         if ( rc != 0 )
            return error4( expr->codeBase, e4info, E90922 ) ;
      #endif
   #endif
   return expr4reset( expr ) ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
static void expr4stop( CODE4 *c4 )
{
   /* clear the globals to ensure they are not used without initialization */
   expr4buf = 0 ;
   expr4 = 0 ;
   expr4ptr = 0 ;
   expr4infoPtr = 0 ;
   expr4constants = 0 ;
   #ifdef S4SEMAPHORE
      #ifdef S4OS2
         DosReleaseMutexSem( c4->hmtxExpr ) ;
      #endif
      #ifdef S4WIN32
         LeaveCriticalSection( &critical4expression ) ;
      #endif
      #ifdef S4LINUX
         pthread_mutex_unlock( &critical4expression ) ;  /* LY 2002/02/20 */
      #endif
   #endif
}


int S4FUNCTION expr4double2( EXPR4 *e4expr, double *result )
{
   *result = expr4double( e4expr ) ;
   if ( *result < 0.0 )
      return -1;
   return 0;
}


double S4FUNCTION expr4double( EXPR4 *e4expr )
{
   char *ptr ;
   int len ;
   #ifdef S4DATA_ALIGN
      double doub ;
   #endif

   len = expr4vary( e4expr, &ptr ) ;
   if ( len >= 0 )
      switch( expr4type( e4expr ) )
      {
         case r4numDoub:
         case r4dateDoub:
            #ifdef S4DATA_ALIGN
               memcpy( (void *)&doub, ptr, sizeof(double) ) ;
               return  doub ;
            #else
               return  *( (double *)ptr ) ;
            #endif
         case r4num:
         case r4str:
            return c4atod( ptr, len ) ;
         case r4date:
            return (double)date4long( ptr ) ;
         default:
           #ifdef E4ANALYZE
              error4( e4expr->codeBase, e4info, E90914 ) ;
           #endif
           break ;
      }

   return 0.0 ;
}



int S4FUNCTION expr4execute( EXPR4 *expr, const int pos, void **resultPtrPtr )
{
   E4INFO *lastInfo ;
   char *pointers[E4MAX_STACK_ENTRIES] ;
   int infoPos ;
   short int rc ;

   #ifdef E4PARM_LOW
      if ( expr == 0 )
         return error4( 0, e4parm_null, E90912 ) ;
      if ( pos < 0 || resultPtrPtr == 0 )
         return error4( expr->codeBase, e4parm, E90912 ) ;
   #endif

   if ( error4code( expr->codeBase ) < 0 )
      return e4codeBase ;

   rc = expr4start( expr ) ;
   if ( rc != 0 )
      return error4( expr->codeBase, rc, E90912 ) ;

   expr4 = pointers ;
   expr4constants = expr->constants ;
   expr4ptr = expr ;
   lastInfo = expr->info + pos ;
   infoPos = pos - lastInfo->numEntries + 1 ;

   for( ; infoPos <= pos ; infoPos++ )
   {
      expr4infoPtr = expr->info+ infoPos ;
      (*expr4infoPtr->function)() ;
   }

   *resultPtrPtr = pointers[0] ;
   #ifdef E4ANALYZE
      if ( pointers[0] != expr4[-1] )
         return error4( expr->codeBase, e4result, E90912 ) ;
   #endif

   expr4stop( expr->codeBase ) ;

   return error4code( expr->codeBase ) ;
}



#ifndef S4CLIENT
   #ifdef S4MDX
      static int expr4keyConvertIndexDependent( EXPR4 *e4expr, char **ptrToPtrToConvertedResult, int resultLen, int exprType )
      {
         double *dPtr ;
         C4BCD bcd ;
         #ifdef S4DATA_ALIGN
            double dtmp;
         #endif

         CODE4 *cb = e4expr->codeBase ;

         switch ( exprType )
         {
            case r4num:
               c4bcdFromA( 0, (char *) &bcd, (char *) *ptrToPtrToConvertedResult, resultLen, 0 ) ;
               #ifdef E4ANALYZE
                  if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(C4BCD) )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               c4memcpy( cb->storedKey, (void *)&bcd, sizeof(C4BCD) ) ;
               resultLen = (int)sizeof( C4BCD ) ;
               break ;
            case r4numDoub:
               {
                  #ifdef S4DATA_ALIGN
                     // LY Feb 17/05 : casting &dtmp no longer avoid optimization problem; must copy byte at a time
                     // memcpy( (void *)&dtmp, *ptrToPtrToConvertedResult, sizeof(double));  // LY Oct 10/03 : cast dtmp to void *, to avoid use of numeric assembly instruction
                     for ( int i = 0 ; i < sizeof( double ) ; i++ )
                        ((char *)&dtmp)[i] = (*ptrToPtrToConvertedResult)[i] ;
                     c4bcdFromD( (char *)&bcd, dtmp);
                  #else
                     dPtr = (double *) (*ptrToPtrToConvertedResult) ;
                     c4bcdFromD( (char *) &bcd, *dPtr ) ;
                  #endif
                  #ifdef E4ANALYZE
                     if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(C4BCD)  )
                        return error4( cb, e4info, E80903 ) ;
                  #endif
                  c4memcpy( cb->storedKey, (void *)&bcd, sizeof(C4BCD) ) ;
                  resultLen = (int)sizeof( C4BCD ) ;
               }
               break ;
            case r4date:
               {
                  double d = 0 ;
                  date4formatMdx2( *ptrToPtrToConvertedResult, &d ) ;
                  if ( d == 0 )
                     d = 1.0E300 ;
                  #ifdef E4ANALYZE
                     if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(double)  )
                        return error4( cb, e4info, E80903 ) ;
                  #endif
                  c4memcpy( cb->storedKey, (void *)&d, sizeof(double) ) ;
                  resultLen = (int)sizeof( double ) ;
               }
               break ;
            case r4dateDoub:
               #ifdef E4ANALYZE
                  if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(double) )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               c4memcpy( cb->storedKey, *ptrToPtrToConvertedResult, sizeof(double) ) ;
               dPtr = (double *)( cb->storedKey ) ;
               if ( *dPtr == 0 )
                  *dPtr = 1.0E300 ;
               #ifdef S4BYTE_SWAP
                  *(double *)cb->storedKey = x4reverseDouble((double *)cb->storedKey) ;
               #endif
               resultLen = (int)sizeof( double ) ;
               break ;
            default:
               #ifdef E4ANALYZE
                  // AS Nov 19/03 - warning fix
                  if ( cb->storedKey == 0 || (int)cb->storedKeyLen < resultLen )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               c4memcpy( cb->storedKey, *ptrToPtrToConvertedResult, (unsigned int)resultLen ) ;
               break ;
         }

         return resultLen ;
      }
   #endif /* S4MDX */



   #ifdef S4FOX
      static int expr4keyConvertIndexDependent( EXPR4 *e4expr, char **ptrToPtrToConvertedFrom, int resultLen, int exprType )
      {
         #ifndef S4DATA_ALIGN
            double *dPtr ;
         #endif
         CURRENCY4 *currency ;
         S4LONG *lPtr ;  /* LY 2001/07/28 : changed from long for 64-bit */
         float *flt ;
         double d ;
         #if defined(S4DATA_ALIGN) || defined(S4BYTE_SWAP)  /* LY 2001/02/20 */
            double tempdoub ;
            int tempint ;
         #endif

         CODE4 *cb = e4expr->codeBase ;

         switch ( exprType )
         {
            case r5i2:
               {
                  // LY Jun 29/04 : added S4DATA_ALIGN
                  /* LY 2001/07/28 : changed from long for 64-bit */
                  S4LONG i2asLong ;
                  #ifdef S4DATA_ALIGN
                     short tempShort ;
                     // LY Jul 9/04 : couldn't do memcpy since release mode
                     // still resulted in numeric assembly operator and
                     // alignment error despite casting pointers to char*
                     ((char *)&tempShort)[0] = (*ptrToPtrToConvertedFrom)[0] ;
                     ((char *)&tempShort)[1] = (*ptrToPtrToConvertedFrom)[1] ;
                     i2asLong = (S4LONG)tempShort ;
                  #elif defined( S4BYTE_SWAP )  // LY Aug 24/04 : byte-swap field value before casting to S4LONG
                     S4LONG tempLong = x4reverseShort( (short *)( *ptrToPtrToConvertedFrom ) ) ;
                     i2asLong = x4reverseLong( &tempLong ) ;
                  #else
                     i2asLong = *((short *)( *ptrToPtrToConvertedFrom )) ;
                  #endif
                  #ifdef E4ANALYZE
                     if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(S4LONG) )
                        return error4( cb, e4info, E80903 ) ;
                  #endif
                  t4intToFox( cb->storedKey, &i2asLong ) ;
                  resultLen = sizeof( long ) ; // stored as 4 byte long in index
               }
               break ;
            case r5ui2:
               {
                  // LY Jun 30/04 : changed declaration of ui2asUnsignedLong, added S4DATA_ALIGN
                  unsigned long ui2asUnsignedLong ;
                  #ifdef S4DATA_ALIGN
                     memset( &ui2asUnsignedLong, 0, sizeof( unsigned long ) ) ;
                     // LY Jul 9/04 : couldn't do memcpy since release mode
                     // still resulted in numeric assembly operator and
                     // alignment error despite casting pointers to char*
                     ((char *)&ui2asUnsignedLong)[0] = (*ptrToPtrToConvertedFrom)[0];
                     ((char *)&ui2asUnsignedLong)[1] = (*ptrToPtrToConvertedFrom)[1];
                  #elif defined( S4BYTE_SWAP )  // LY Aug 24/04 : byte-swap value field value before casting to unsigned long
                     short tempShort = x4reverseShort( (short *)( *ptrToPtrToConvertedFrom ) ) ;
                     unsigned long tempULong = 0 ;
                     memcpy( (char *)&tempULong + sizeof(short), &tempShort, sizeof(short) ) ;
                     ui2asUnsignedLong = x4reverseLong( &tempULong ) ;
                  #else
                     ui2asUnsignedLong = *((unsigned short *)( *ptrToPtrToConvertedFrom )) ;
                  #endif
                  #ifdef E4ANALYZE
                     if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(unsigned long) )
                        return error4( cb, e4info, E80903 ) ;
                  #endif
                  t4unsignedIntToFox( cb->storedKey, &ui2asUnsignedLong ) ;
                  resultLen = sizeof( long ) ; // stored as 4 byte unsigned long in index
               }
               break ;
            case r5ui4:
               {
                  // LY Jun 30/04 : changed declaration of ui4, added S4DATA_ALIGN
                  unsigned long ui4 ;
                  #ifdef S4DATA_ALIGN
                     // LY Jul 9/04 : couldn't do memcpy since release mode
                     // still resulted in numeric assembly operator and
                     // alignment error despite casting pointers to char*
                     ((char *)&ui4)[0] = (*ptrToPtrToConvertedFrom)[0];
                     ((char *)&ui4)[1] = (*ptrToPtrToConvertedFrom)[1];
                     ((char *)&ui4)[2] = (*ptrToPtrToConvertedFrom)[2];
                     ((char *)&ui4)[3] = (*ptrToPtrToConvertedFrom)[3];
                  #else
                     ui4 = *((unsigned long *)( *ptrToPtrToConvertedFrom )) ;
                  #endif
                  #ifdef E4ANALYZE
                     if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(unsigned long) )
                        return error4( cb, e4info, E80903 ) ;
                  #endif
                  t4unsignedIntToFox( cb->storedKey, &ui4 ) ;
               }
               break ;
            case r4int:
               lPtr = (S4LONG *)( *ptrToPtrToConvertedFrom ) ;  /* LY 2001/07/28 : changed from long* for 64-bit */
               #ifdef E4ANALYZE
                  if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(long) )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               t4intToFox( cb->storedKey, lPtr ) ;
               break ;
            case r5dbDate:
               t4dbDateToFox( cb->storedKey, (DBDATE *)(*ptrToPtrToConvertedFrom) ) ;
               break ;
            case r5dbTime:
               t4dbTimeToFox( cb->storedKey, (DBTIME *)(*ptrToPtrToConvertedFrom) ) ;
               break ;
            case r5dbTimeStamp:
               t4dbTimeStampToFox( cb->storedKey, (DBTIMESTAMP *)(*ptrToPtrToConvertedFrom) ) ;
               break ;
            // AS 05/06/99 -- only support r5i8 on 32-bit windows...
            #if !defined( S4NO_LONGLONG )
               case r5ui8:   // only used for 'cardinality' field, so always assume less than MAX(I8)
               case r5i8:
                  {
                     /* LY 04/22/99 : LONGLONG on WIN32 = long long on Mac */
                     #ifdef S4MACINTOSH
                        long long *longLongPtr = (long long *)( *ptrToPtrToConvertedFrom ) ;
                     #else
                        LONGLONG *longLongPtr = (LONGLONG *)( *ptrToPtrToConvertedFrom ) ;
                     #endif
                     t4i8ToFox( cb->storedKey, longLongPtr ) ;
                     resultLen = 8 ; // stored as 4 byte long in index
                  }
                  break ;
            #endif /* #if defined( S4WIN32 ) || defined( S4MACINTOSH ) */
            case r4currency:
               currency = (CURRENCY4 *)( *ptrToPtrToConvertedFrom ) ;
               t4curToFox( cb->storedKey, currency ) ;
               resultLen = 8 ;
               break ;
            case r4dateTime:
               /* LY 2001/07/28 : changed from long* for 64-bit */
               t4dateTimeToFox( cb->storedKey, (S4LONG *)(*ptrToPtrToConvertedFrom) ) ;
               resultLen = 8 ;
               break ;
            case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
               t4dateTimeToFoxMilli( cb->storedKey, (S4LONG *)(*ptrToPtrToConvertedFrom) ) ;
               resultLen = 8 ;
               break ;
            case r4num:
               d = c4atod( *ptrToPtrToConvertedFrom, resultLen ) ;
               //#ifdef S4BYTE_SWAP  /* LY 2001/06/26 */
               //   tempdoub = x4reverseDouble( &d ) ;
               //   d = tempdoub ;
               //#endif  LY 2001/07/21 : t4dblToFox handles byteswapping already
               t4dblToFox( cb->storedKey, d ) ;
               resultLen = (int)sizeof( double ) ;
               break ;
            case r4date:
               d = (double)date4long( *ptrToPtrToConvertedFrom ) ;
               t4dblToFox( cb->storedKey, d ) ;
               resultLen = (int)sizeof( double ) ;
               break ;
            case r4numDoub:
            case r4dateDoub:
               #ifdef S4DATA_ALIGN
                  /* LY 00/11/09 : cast to void* for IA-64 */
                  // LY Jul 12/04 : copy value to tempdoub byte at a time, since memcpy could not
                  // avoid numeric assembly operator and alignment exception in release mode,
                  // despite casting tempdoub to non-double*
                  // memcpy((char*)&tempdoub, *ptrToPtrToConvertedFrom, sizeof(double) );
                  ((char *)&tempdoub)[0] = (*ptrToPtrToConvertedFrom)[0] ;
                  ((char *)&tempdoub)[1] = (*ptrToPtrToConvertedFrom)[1] ;
                  ((char *)&tempdoub)[2] = (*ptrToPtrToConvertedFrom)[2] ;
                  ((char *)&tempdoub)[3] = (*ptrToPtrToConvertedFrom)[3] ;
                  ((char *)&tempdoub)[4] = (*ptrToPtrToConvertedFrom)[4] ;
                  ((char *)&tempdoub)[5] = (*ptrToPtrToConvertedFrom)[5] ;
                  ((char *)&tempdoub)[6] = (*ptrToPtrToConvertedFrom)[6] ;
                  ((char *)&tempdoub)[7] = (*ptrToPtrToConvertedFrom)[7] ;
               #else
                  // LY 2002/08/11 : t4dblToFox handles byteswapping already
                  // #ifdef S4BYTE_SWAP  /* LY 2001/02/20 */
                  //    tempdoub = x4reverseDouble( (double *)(*ptrToPtrToConvertedFrom) ) ;
                  //    dPtr = &tempdoub ;
                  // #else
                     dPtr = (double *) (*ptrToPtrToConvertedFrom) ;
                  // #endif
               #endif
               if ( expr4currency( e4expr ) )  /* then should be converted to a currency */
                  #ifdef S4DATA_ALIGN
                     t4dblToCurFox( cb->storedKey, tempdoub ) ;
                  #else
                     t4dblToCurFox( cb->storedKey, *dPtr ) ;
                  #endif
               else
                  #ifdef S4DATA_ALIGN
                     t4dblToFox( cb->storedKey, tempdoub ) ;
                  #else
                     t4dblToFox( cb->storedKey, *dPtr ) ;
                  #endif
               resultLen = (int)sizeof( double ) ;
               break ;
            case r4log:
               #ifdef S4DATA_ALIGN
                  // LY Jul 12/04 : couldn't do straight memcpy, since release mode
                  // still results in numeric assembly operator despite casting tempint to char*
                  tempint = 0 ;
                  *((char *)&tempint) = **ptrToPtrToConvertedFrom ;
                  switch(tempint)
               #else
                  switch( *(int *)*ptrToPtrToConvertedFrom )
               #endif
               {
                  case 1:
                     cb->storedKey[0] = 'T' ;
                     break ;
                  case 0:
                     cb->storedKey[0] = 'F' ;
                     break ;
                  default:
                     #ifdef E4ANALYZE
                        return error4( e4expr->codeBase, e4info, E81002 ) ;
                     #else
                        cb->storedKey[0] = 'F' ;
                     #endif
               }
               resultLen = 1 ;
               break ;
            case r4floatBin:
               flt = (float *)(*ptrToPtrToConvertedFrom ) ;
               #ifdef E4ANALYZE
                  if ( cb->storedKey == 0 || cb->storedKeyLen < sizeof(float) )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               t4floatToFox( cb->storedKey, flt ) ;
               break ;
            default:
               #ifdef E4ANALYZE
                  if ( cb->storedKey == 0 || cb->storedKeyLen < (unsigned int)resultLen )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)resultLen ) ;
               break ;
         }

         return resultLen ;
      }
   #endif /* S4FOX */



   #ifdef S4CLIPPER
      static int expr4keyConvertIndexDependent( EXPR4 *e4expr, char **ptrToPtrToConvertedFrom, int resultLen, int exprType )
      {
         long l ;
         int oldDec, tLen ;
         double d ;

         CODE4 *cb = e4expr->codeBase ;
         TAG4FILE *t4file = e4expr->tagPtr ;

         switch ( exprType )
         {
            case r4num:
               resultLen = e4expr->keyLen ;
               #ifdef E4ANALYZE
                  if ( cb->storedKey == 0 )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               /* AS 04/22/97 fixes for main.cpp (user test), using VAL, get failures */
               if ( t4file != 0 )
               {
                  /* tLen is used for the difference between the expression len and the tag len */
                  tLen = resultLen - t4file->header.keyLen ;
                  c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom + tLen, resultLen - tLen) ;
                  // oldDec = cb->decimals ;
                  // cb->decimals = t4file->expr->keyDec ;
                  c4clip( cb->storedKey, t4file->header.keyLen, t4file->expr->keyDec ) ;
                  // cb->decimals = oldDec ;
               }
               else
               {
                  #ifdef E4ANALYZE
                     // AS Aug 26/02 - compiler warning fix
                     if ( cb->storedKey == 0 || cb->storedKeyLen < (unsigned)resultLen )
                        return error4( cb, e4info, E80903 ) ;
                  #endif
                  c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, resultLen ) ;
                  c4clip( cb->storedKey, resultLen, cb->decimals ) ;
               }
               break ;
            case r4numDoub:
               #ifdef E4ANALYZE
                  // AS Aug 26/02 - compiler warning fix
                  if ( cb->storedKey == 0 || cb->storedKeyLen < (unsigned)resultLen )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               if ( t4file != 0 )
               {
                  oldDec = cb->decimals ;
                  resultLen = t4file->header.keyLen ;
                  cb->decimals = t4file->expr->keyDec ;
                  c4dtoaClipper( *((double *)*ptrToPtrToConvertedFrom), cb->storedKey, resultLen, cb->decimals ) ;
                  cb->decimals = oldDec ;
               }
               else
               {
                  resultLen = e4expr->keyLen ;
                  c4dtoaClipper( *((double *)*ptrToPtrToConvertedFrom), cb->storedKey, resultLen, cb->decimals ) ;
               }
               break ;
            case r4dateDoub:

               d = *( ( double  *)*ptrToPtrToConvertedFrom ) ;
               l = (long)d ;
               date4assign( cb->storedKey, l ) ;
               break ;
            default:
               #ifdef E4ANALYZE
                  // AS Aug 26/02 - compiler warning fix
                  if ( cb->storedKey == 0 || cb->storedKeyLen < (unsigned)resultLen )
                     return error4( cb, e4info, E80903 ) ;
               #endif
               c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)resultLen ) ;
               break ;
         }

         return resultLen ;
      }
   #endif /* S4CLIPPER */



   #if !defined( S4OFF_INDEX ) && defined( S4FOX )
      static int collate4convertUnicodeToMachineKey( COLLATE4 *collate, char *keyResult, const char *unicodeFrom, unsigned int fromLen )
      {
         // conversion from unicode to machine ordered key
         int dummy ;
         t4unicodeToMachine( collate, keyResult, unicodeFrom, fromLen, &dummy ) ;
         assert5( (unsigned int)dummy == fromLen ) ;  // output key len (dummy) should == calculated key len - except maybe nulls taken into account?
         return 0 ;
      }
   #endif



   static int expr4keyConvertIndexStr( EXPR4 *e4expr, char **ptrToPtrToConvertedFrom, int inputPtrLen, TAG4FILE *t4file )
   {
      CODE4 *cb = e4expr->codeBase ;

      #ifdef E4ANALYZE
         if ( cb->storedKey == 0 )
            return error4( cb, e4info, E80903 ) ;
      #endif

      // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
      #if !defined( S4OFF_INDEX )
         #if defined( S4FOX )
            if ( t4file != 0 )
            {
               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // if ( t4file->expr->vfpInfo->sortType == sort4general )
               COLLATE4 *collate = collation4get( t4file->collateName ) ;
               // AS May 12/06 - support for ascend with general fields...don't collate in that case because the ascend operator does this already
               if ( ( collate->collateType == collate4machineByteOrder ) || ( t4file->hasAscendOrDescend == 1 ) )
               {
                  if ( t4file->isUnicode && ( t4file->hasAscendOrDescend != 1 ))  // need to reverseShort every character
                  {
                     if ( collate4convertUnicodeToMachineKey( collate, cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)inputPtrLen ) < 0 )
                        return -1 ;
                  }
                  else  // normal characters machine order - leave in byte order...
                     c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)inputPtrLen ) ;
               }
               else
               {
                  int keyLen = expr4keyLen( e4expr ) ;
                  if ( cb->storedKeyLen <= (unsigned)keyLen )
                  {
                     u4allocAgain( cb, &cb->storedKey, &cb->storedKeyLen, (unsigned)keyLen+1 ) ;
                     if ( error4code( cb ) < 0 )
                        return -1 ;
                     cb->storedKeyLen = keyLen + 1 ;
                  }
                  if ( expr4nullLow( e4expr, 0 ) )
                     keyLen-- ;

                  /* if trim, then replace nulls with blanks before translation */
                  if ( e4expr->hasTrim )
                  {
                     for( int i = inputPtrLen - 1 ; (i >= 0) && ((*ptrToPtrToConvertedFrom)[i] == 0) ; i-- )
                        (*ptrToPtrToConvertedFrom)[i] = ' ' ;
                  }

                  int lenOut ;
                  if ( collate4convertCharToKey( collate, cb->storedKey, *ptrToPtrToConvertedFrom, inputPtrLen, t4file->isUnicode, &lenOut ) < 0 )
                     return -1 ;

                  return lenOut ;
               }
            }
            else
               c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)inputPtrLen ) ;
         #else /* #if defined( S4FOX ) */
            // AS 08/17/99 --> wstr keys, want to collate in machine byte order so that they
            // get sorted correctly...
            if ( t4file != 0 )
            {
               switch ( tfile4type( t4file ) )
               {
                  case r5wstr:
                  case r5wstrLen:
                     t4unicodeToMachine( 0, cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)inputPtrLen, 0 ) ;
                     break ;
                  default:
                     c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)inputPtrLen ) ;
                     break ;
               }
            }
            else
               c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)inputPtrLen ) ;
         #endif /* #if defined( S4FOX ) */
      #else
         c4memcpy( cb->storedKey, *ptrToPtrToConvertedFrom, (unsigned int)inputPtrLen ) ;
      #endif /* S4OFF_INDEX */

      /* if trim, then replace nulls with blanks */
      if ( e4expr->hasTrim )
      {
         // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
         #ifdef S4FOX
            // if ( e4expr->vfpInfo )
            if ( t4file == 0 || collation4get( t4file->collateName )->collateType == collate4machineByteOrder )
         #endif
               {
                  for( int i = inputPtrLen - 1 ; (i >= 0) && (cb->storedKey[i] == 0) ; i-- )
                     cb->storedKey[i] = ' ' ;
               }
      }

      return inputPtrLen ;
   }



   int S4FUNCTION expr4key( EXPR4 *e4expr, char **ptrToPtrToConvertedFrom, TAG4FILE *t4file )
   {
      #ifdef E4PARM_HIGH
         if ( e4expr == 0 || ptrToPtrToConvertedFrom == 0 )
            return error4( 0, e4parm_null, E90913 ) ;
      #endif

      if ( error4code( e4expr->codeBase ) < 0 )
         return e4codeBase ;

      int resultLen = expr4vary( e4expr, ptrToPtrToConvertedFrom ) ;
      if ( resultLen < 0 )
          return resultLen ;

      return expr4keyConvert( e4expr, ptrToPtrToConvertedFrom, resultLen, e4expr->type, t4file ) ;
   }



   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int expr4keyConvert( EXPR4 *e4expr, char **ptrToPtrToConvertedFrom, const int rLen, const int exprType, TAG4FILE *t4file )
   {
      CODE4 *cb = e4expr->codeBase ;

      int resultLen ;
      if ( exprType == r4str || exprType == r5wstr || exprType == r5wstrLen )  // consider collations...
         resultLen = expr4keyConvertIndexStr( e4expr, ptrToPtrToConvertedFrom, rLen, t4file ) ;
      else
         resultLen = expr4keyConvertIndexDependent( e4expr, ptrToPtrToConvertedFrom, rLen, exprType ) ;

      if ( resultLen < 0 )
         return resultLen ;

      #ifdef E4ANALYZE
         if ( (unsigned)resultLen >= cb->storedKeyLen )
            return error4( cb, e4info, E80903 ) ;
      #endif

      #ifdef S4FOX
         if ( expr4nullLow( e4expr, 0 ) )  /* maybe a null value, so check */
         {
            if ( expr4nullLow( e4expr, 1 ) )  /* value is null */
               c4memset( cb->storedKey, 0, resultLen + 1 ) ;
            else
            {
               c4memmove( cb->storedKey + 1, cb->storedKey, resultLen ) ;
               cb->storedKey[0] = (char)0x80 ;
            }
            resultLen++ ;
         }
      #endif

      cb->storedKey[resultLen] = 0 ;    /* null end it */
      *ptrToPtrToConvertedFrom = cb->storedKey ;
      return resultLen ;
   }
#endif



// AS Feb 2/01 - Always export this function (for ODBC)
int S4FUNCTION collate4convertCharToKey( COLLATE4 *collate, char *keyResult, const char *charFrom, unsigned int fromLen, int isUnicode, int *lenOut )
{
   #if !defined( S4OFF_INDEX ) && defined( S4FOX ) && !defined( S4CLIENT )
      // generic non-machine collation, used by ODBC as well as codebase internals
      // int dummy ;

      switch( collate->collateType )
      {
         case collate4subSortCompress:
            if ( isUnicode )
               t4convertSubSortCompressUnicode( collate, keyResult, charFrom, fromLen, lenOut ) ;
            else
               t4convertSubSortCompressChar( collate, keyResult, charFrom, fromLen, lenOut ) ;
            //   return error4( cb, e4info, E85404 ) ;
            // assert5( (unsigned int)dummy == fromLen ) ;  // output key len (dummy) should == calculated key len - except maybe nulls taken into account?
            break ;
         case collate4simple:
            if ( isUnicode )  // not supported
               return -1 ;
            else
               t4convertSimpleChar( collate, keyResult, charFrom, fromLen, lenOut ) ;
            break ;
         // case collate4subSort: - not implemented yet
         // case collate4compress: - not implemented yet
         default:
            return -1 ;
      }
   #endif

   return 0 ;
}



int S4FUNCTION expr4currency( const EXPR4 *e4expr )
{
   /* returns true if a currency field resides in the expression
      tag file type is currency if there is any currency field value within
      the expression and the result type is otherwise r4numDoub
   */
   #ifndef S4CLIENT_OR_FOX
      if ( code4indexFormat( e4expr->codeBase ) != r4cdx )
         return 0 ;

      for ( int parms = 0 ; parms < e4expr->infoN ; parms++ )
      {
         FIELD4 *field = e4expr->info[parms].fieldPtr ;
         if ( field != 0 )
            if ( f4typeInternal( field ) == r4currency )
               return 1 ;
      }
   #endif

   return 0 ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
short S4FUNCTION expr4nullLow( const EXPR4 *e4expr, const short forAdd )
{
   #ifdef S4CLIENT_OR_FOX
      /* if forAdd is 0, then returns whether or not the expression has the
         possibility of a null return.  If one, it evaluates the current
         expression to see if the current result is null
      */

      #ifdef S4CLIENT
         if ( code4indexFormat( e4expr->codeBase ) != r4cdx )
            return 0 ;
      #else
         #ifndef S4OFF_INDEX
            /* checking the tag setting doesn't apply for client since would only
               be looking at it as an expression, not a tag-related value */
            if ( forAdd == 0 )
            {
               TAG4FILE *tag = e4expr->tagPtr ;
               if ( tag != 0 )   /* candidate keys don't make room for the null since disallowed */
                  if ( tag->header.typeCode & 0x04 )  /* r4candidate */
                     return 0 ;
            }
         #endif
      #endif

      for ( int parms = 0 ; parms < e4expr->infoN ; parms++ )
      {
         FIELD4 *field = e4expr->info[parms].fieldPtr ;

         if ( field != 0 )  /* if has null now, then add 1 byte for index storage */
         {
            if ( field->null == 1 )
            {
               if ( forAdd == 1 )  /* only want to know whether contents are null */
               {
                  if ( f4null( field ) )
                     return 1 ;
               }
               else
                  return 1 ;
            }
         }
      }
   #endif /* S4CLIENT_OF_FOX */

   return 0 ;
}


int S4FUNCTION expr4keyLenFromType( int exprType, int valueLength, CODE4 *c4 )
{
   // given the input type, determines the length of the index key.
   // this is used by expr4keyLen to determine key length, and by OLE-DB to determine
   // partial key lengths based on type.
   //
   // value length is a standard length of the value, often for example, a field length
   //
   // return '-1' if cannot determine from type (use expr4len() to determine length)
   // return '-2' if potentially collation dependent (see if tag associated...)

   #ifdef S4CLIENT
      switch( exprType )
      {
         case r4num:
            switch( code4indexFormat( c4 ) )
            {
               case r4cdx:
                  // AS 11/08/99 was returing wrong value in fox (t4spin.c)
                  return (int)sizeof( double ) ;
               case r4ntx:
                  return valueLength ;
               case r4mdx:
                  return (int)sizeof( C4BCD ) ;
            }
            break ;
         case r4date:
            switch( code4indexFormat( c4 ) )
            {
               case r4cdx:
               case r4mdx:
                  return (int)sizeof( double ) ;
            }
            break ;
         case r4dateDoub:
            switch( code4indexFormat( c4 ) )
            {
               case r4mdx:
                  return (int)sizeof( double ) ;
            }
            break ;
         case r4numDoub:
            switch( code4indexFormat( c4 ) )
            {
               case r4cdx:
                  return (int)sizeof( double ) ;
               case r4ntx:
                  return c4->numericStrLen ;
               case r4mdx:
                  return (int)sizeof( C4BCD ) ;
            }
            break ;
         case r4log:
            switch( code4indexFormat( c4 ) )
            {
               case r4cdx:
                  return (int)sizeof( char ) ;
            }
            break ;
         case r4int:
         case r5ui4:
         case r5ui2:  // stored in index as 4 byte unsigned long
         case r5i2:   // stored in index as 4 byte long
            if ( code4indexFormat( c4 ) == r4cdx )
               return (int)sizeof( S4LONG ) ;  /* LY 2001/07/28 : changed from long for 64-bit */
         case r4dateTime:
         case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
         case r4currency:
            if ( code4indexFormat( c4 ) == r4cdx )
               return (int)sizeof( double ) ;
      }
      return -1 ;   // means use expression length info to determine length...
   #else
      switch( exprType )
      {
         #ifdef S4FOX
            case r4num:
            case r4date:
            case r4numDoub:
            case r4currency:
            case r4dateTime:
            case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
               return (int)sizeof( double ) ;
            case r4int:
            case r5ui4:
            case r5ui2:  // stored in index as 4 byte unsigned long
            case r5i2:   // stored in index as 4 byte long
               return (int)sizeof( S4LONG ) ;  /* LY 2001/07/28 : changed from long for 64-bit */
            case r4log:
               return (int)sizeof( char ) ;
         #endif  /*  ifdef S4FOX      */
         #ifdef S4CLIPPER
            case r4num:  /* numeric field return, this fixex length problem */
               return valueLength ;
            case r4numDoub:
               return c4->numericStrLen ;
         #endif  /*  ifdef S4CLIPPER  */
         #ifdef S4MDX
            case r4num:
            case r4numDoub:
               return (int)sizeof( C4BCD ) ;
            case r4date:
            case r4dateDoub:
               return (int)sizeof( double ) ;
         #endif  /* S4MDX */
         default:
            // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
            #ifdef S4FOX
               return -2 ;
            #else
               return -1 ;
            #endif
       }
   #endif

   assert5( 0 ) ;   // should never get here, switch statments return...
   return -1 ;
}



int S4FUNCTION expr4keyLen( EXPR4 *e4expr )
{
   #ifdef E4PARM_LOW
      if ( e4expr == 0 )
         return error4( 0, e4parm_null, E90915 ) ;
   #endif

   int nullIndicatorLen = expr4nullLow( e4expr, 0 ) ;   /* extra byte for nulls if required */

   int valueLength = 0 ;
   if ( e4expr->info[0].fieldPtr != 0 )
      valueLength = f4len( e4expr->info[0].fieldPtr ) ;
   int basicTypeLength = expr4keyLenFromType( e4expr->type, valueLength, e4expr->codeBase ) ;

   switch( basicTypeLength )
   {
      case -1:   // means couldn't determine from type, use expr4len...
         return nullIndicatorLen + expr4len( e4expr ) ;
      #ifdef S4FOX
         case -2:   // means collation-dependent for length...
            if ( e4expr->tagPtr != 0 )
            {
               // the actual length is affected as follows:
               // 'nullIndicatorLen' + the expression length modified for collations whereby
               // there may be extra bytes on a per character basis (namely extra bytes per
               // character for sub-sort data)
               // therefore, need to know # of characters for this calculation...
               int exprResultLen = expr4len( e4expr ) ;

               // int numChars ;
               // if ( e4expr->tagPtr->isUnicode )  // each character 2 bytes long...
               //    numChars = exprResultLen / 2 ;
               // else
               //    numChars = exprResultLen ;
               // return exprResultLen + numChars * ( collation4get( e4expr->tagPtr->collateName )->keySizeBytesPerCharAdd ) ;

               assert5( collation4get( e4expr->tagPtr->collateName )->collateType != collate4unknown ) ;  // ensure loaded before using for key size...
               // AS May 11/06 - support for ascend/descend with general collating sequence.
               //  In this case we used the expression length; we store the collation in the tag so we know how to collate the ascend operation
               //  for strings, but the data is already collated and therefore does not require adding the keySizePerCharAdd
               int lenOut = nullIndicatorLen + exprResultLen ;
               if ( e4expr->tagPtr->hasAscendOrDescend != 1 )
                  lenOut += exprResultLen * ( collation4get( e4expr->tagPtr->collateName )->keySizeCharPerCharAdd ) ;
               return lenOut ;
            }

            return nullIndicatorLen + expr4len( e4expr ) ;
      #endif
      default:   // use the basicTypeLength as the length indicator
         return nullIndicatorLen + basicTypeLength ;
   }
}



static char e4nullChar = '\0' ;



S4CONST char *S4FUNCTION expr4source( const EXPR4 *e4expr )
{
   if ( e4expr == 0 )
      return &e4nullChar ;
   return e4expr->source ;
}



const char *S4FUNCTION expr4str( EXPR4 *expr )
{
   char *str ;

   #ifdef E4PARM_HIGH
      if ( expr == 0 )
      {
         error4( 0, e4parm_null, E90919 ) ;
         return 0 ;
      }
   #endif

   switch( expr4type( expr ) )
   {
      case r4str:
      case r4date:
         expr4vary( expr, &str ) ;
         break ;
      default:
         error4( expr->codeBase, e4parm ,E90919 ) ;
         return 0 ;
   }

   return str ;
}



int S4FUNCTION expr4true( EXPR4 *e4expr )
{
   int resultLen ;
   int *iPtr ;

   resultLen = expr4vary( e4expr, (char **)&iPtr ) ;
   if ( resultLen < 0 )
      return -1 ;

   if ( expr4type( e4expr ) != r4log )
      return error4describe( e4expr->codeBase, e4result, E80905, expr4source( e4expr ), 0, 0 ) ;

   /* for sure avoid returning negative values which mean true but are interpreted as errors */
   return ( ( *iPtr != 0 ) ? 1 : 0 ) ;
}



int S4FUNCTION expr4vary( EXPR4 *expr, char **resultPtrPtr )
{
   char *pointers[E4MAX_STACK_ENTRIES] ;
   int infoPos, rc ;

   #ifdef E4PARM_HIGH
      if ( expr == 0 )
         return error4( 0, e4parm_null, E90916 ) ;
      if ( resultPtrPtr == 0 )
         return error4( expr->codeBase, e4parm_null, E90916 ) ;
   #endif

   if ( error4code( expr->codeBase ) < 0 )
      return e4codeBase ;

   rc = expr4start( expr ) ;
   if ( rc < 0 )
      return rc ;

   expr4 = pointers ;
   expr4constants = expr->constants ;
   expr4ptr = expr ;

   for( infoPos = 0; infoPos < expr->infoN; infoPos++ )
   {
      expr4infoPtr = expr->info+ infoPos ;
      (*expr4infoPtr->function)() ;
   }

   // pointers[0] points to somewhere in expr4buf which is set to expr->exprWorkBuf which is where the output
   // from the expression is stored (i.e. it is stored as a member of the EXPR4 structure).
   *resultPtrPtr = pointers[0] ;
   #ifdef E4ANALYZE
      if ( pointers[0] != expr4[-1] )
         return error4( expr->codeBase, e4result, E90916 ) ;
   #endif

   expr4stop( expr->codeBase ) ;

   return expr->len ;
}
