/* f4field.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef __cplusplus
   extern "C" {
#endif
short S4FUNCTION f4len_v( FIELD4 * ) ;
#ifdef __cplusplus
   }
#endif

char *S4FUNCTION f4assignPtr( FIELD4 *field )
{
   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90507 ) ;
         return 0 ;
      }
   #endif

   #ifndef S4OFF_WRITE
      #ifdef S4CLIENT_OR_FOX
         if ( d4version( field->data ) == 0x30 )
            f4assignNotNull( field ) ;
      #endif

      field->data->recordChanged = 1 ;
   #endif /* S4OFF_WRITE */

   return ( field->data->record + field->offset ) ;
}



void S4FUNCTION f4blank( FIELD4 *field )
{
   /* This function is always required since it is part of d4open sequence.
      Note, that as specified in manual, this function does not change
      the recordChanged flag.
   */

   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90508 ) )
         return ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90508 ) ;
         return ;
      }
   #endif

   if ( error4code( field->data->codeBase ) < 0 )
      return ;

   #ifndef S4SERVER
      #ifndef S4OFF_ENFORCE_LOCK
         if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
            if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
            {
               error4( field->data->codeBase, e4lock, E90508 ) ;
               return ;
            }
      #endif
   #endif

   if ( s5fox )
   {
      // AS 05/26/99 --> for OLE-DB, blank date is 1899:12:13 else VB fails
      if ( field->type == r5dbDate )
      {
         DBDATE *dbDate = (DBDATE *)f4assignPtr( field ) ;
         dbDate->year = 1899 ;
         dbDate->month = 12 ;
         dbDate->day = 30 ;
      }
      else
      {
         if ( field->binary )
            c4memset( f4assignPtr( field ), 0, field->len ) ;
         else
            c4memset( f4assignPtr( field ), ' ', field->len ) ;
      }
   }
   else
   {
      switch ( field->type )
      {
         case r5wstr:
         case r5wstrLen:
         case r4int:
         case r4charBin:
         case r4memoBin:
         case r4currency:
         case r4dateTime:
         case r4double:
         case r5guid:
         case r5i2:
         case r5ui2:
         case r5ui4:
         case r5i8:
         case r5ui8:
         case r5dbTime:
         case r5dbTimeStamp:
            c4memset( f4assignPtr( field ), 0, field->len ) ;
            break ;
         case r5dbDate:
            // AS 05/26/99 --> for OLE-DB, blank date is 1899:12:13 else VB fails
            {
               DBDATE *dbDate = (DBDATE *)f4assignPtr( field ) ;
               dbDate->year = 1899 ;
               dbDate->month = 12 ;
               dbDate->day = 30 ;
            }
            break ;
         default:
            c4memset( f4assignPtr( field ), ' ', field->len ) ;
            break ;
      }
   }
}

DATA4 *S4FUNCTION f4data( const FIELD4 *field )
{
   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90509 ) ;
         return 0 ;
      }
   #endif

   return field->data ;
}

int S4FUNCTION f4decimals( const FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90510 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
         return error4( 0, e4parm_null, E90510 ) ;
   #endif

   return field->dec ;
}

unsigned long S4FUNCTION f4len( const FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90511 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90511 ) ;
         return 0 ;
      }
   #endif

   return field->len ;
}

S4CONST char *S4FUNCTION f4name( S4CONST FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90512 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90512 ) ;
         return 0 ;
      }
   #endif

   return field->name ;
}

int S4FUNCTION f4number( const FIELD4 *field )
{
   FIELD4 *fieldOn ;
   int fNum ;

   #ifdef E4PARM_HIGH
      if ( field == 0 )
         return error4( 0, e4parm_null, E90538 ) ;
   #endif

   fieldOn = field->data->fields ;

   for ( fNum = 1 ;;  fNum++ )
   {
      if ( fieldOn == field )
         return fNum ;
      fieldOn++ ;
   }
}

/* CJ -07/30/99-This function should only be called the customer.  It is a mistake to
call this function from the Codebase library use f4typeInternal instead */
int S4FUNCTION f4type( const FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90513 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
         return error4( 0, e4parm_null, E90513 ) ;
   #endif

   #ifdef S4CLIENT_OR_FOX
      switch( field->type )
      {
         case 'C':
            if ( field->binary )
               return r4charBin ;
            else
               return r4str ;
         case 'M':
            if ( field->binary == 1 )  // AS 08/18/99 == 1 means a binary memo, == 2 means normal memo...
               return r4memoBin ;
            else
               return r4memo ;
         default:
            return (int)field->type ;
      }
   #else
      return (int)field->type ;
   #endif

}



/* CJ -07/30/99- Because the CodeBase library treats Float as Numeric internally only Numeric is used
for field types comparisons in the CodeBase library.*/
int f4typeInternal( const FIELD4 *field )
{
   #ifdef S4CLIENT_OR_FOX
      switch( field->type )
      {
         case 'F':
            return (int)r4num ;
         case 'C':
            if ( field->binary )
               return r4charBin ;
            else
               return r4str ;
         case 'M':
            if ( field->binary == 1 )  // AS 08/18/99 == 1 means a binary memo, == 2 means normal memo...
               return r4memoBin ;
            else
               return r4memo ;
         default:
            return (int)field->type ;
      }
   #else
      if ( field->type == 'F' )
         return (int)r4num ;
      else
         return (int)field->type ;
   #endif
}

static void makeNegative( CURRENCY4 *, const CURRENCY4 * ) ;

int S4FUNCTION date4timeCompare( const long *dt1, const long *dt2 )
{
   long date1, date2, time1, time2 ;

   #ifdef S4DATA_ALIGN
      memcpy( &date1, &dt1[0], sizeof(long) ) ;
      memcpy( &date2, &dt2[0], sizeof(long) ) ;
   #else
      date1 = dt1[0] ;
      date2 = dt2[0] ;
   #endif

   if ( date1 == date2 )  /* must compare on times */
   {
      #ifdef S4DATA_ALIGN
         memcpy( &time1, &dt1[1], sizeof(long) ) ;
         memcpy( &time2, &dt2[1], sizeof(long) ) ;
      #else
         time1 = dt1[1] ;
         time2 = dt2[1] ;
      #endif

      if ( time1 > time2 )
         return 1 ;
      if ( time1 < time2 )
         return -1 ;
      return 0 ;
   }
   else  /* dates not equal */
   {
      if ( date1 > date2 )
         return 1 ;
      return -1 ;
   }
}

#ifdef S4BTYE_SWAP
   inline static getShort( short a ) { return x4reverseShort( &(a) ) ; }
#else
   #define getShort( a ) ( a )
#endif

/* returns -1 if c1 < c2, 0 if equal, 1 if c1 > c2 */
int S4FUNCTION currency4compare( const CURRENCY4 *c1, const CURRENCY4 *c2 )
{
   short int sign1, sign2 ;
   short int loop ;
   long result ;

   sign1 = (short int)(getShort(c1->lo[3])) >= 0 ? 1 : -1 ;
   sign2 = (short int)(getShort(c2->lo[3])) >= 0 ? 1 : -1 ;

   if ( sign1 != sign2 )   /* one positive, one negative, so comparison easy */
   {
      if ( sign1 == -1 )
         return -1 ;
      else
         return 1 ;
   }

   if ( sign1 == -1 )   /* negative comparison, larger value is smaller */
   {
      for ( loop = 3 ; loop > 0 ; loop-- )
      {
         result = getShort(c1->lo[loop]) ;
         result -= getShort(c2->lo[loop]) ;

         if ( result < 0 )
            return 1 ;
         if ( result > 0 )
            return -1 ;
      }
      return 0 ;
   }

   /* otherwise both values positive, larger is larger */
   for ( loop = 3 ; loop > 0 ; loop-- )
   {
      result = getShort(c1->lo[loop]) ;
      result -= getShort(c2->lo[loop]) ;

      if ( result < 0 )
         return -1 ;
      if ( result > 0 )
         return 1 ;
   }
   return 0 ;
}

int S4FUNCTION currency4add( CURRENCY4 *result, const CURRENCY4 *c1, const CURRENCY4 *c2 )
{
   // AS 06/23/00, this functionality does not work for very very large numbers.
   // in WIN32, LONGLONG is available, so use it instead.
   // input source is longlong 10000 times greater than actual value (i.e. 4 decimal digits available by dividing input by 10000)
   #ifndef S4NO_LONGLONG
      #ifdef S4DATA_ALIGN
         LONGLONG tempC1, tempC2, tempResult ;
         memcpy( &tempC1, c1, sizeof(LONGLONG) ) ;
         memcpy( &tempC2, c2, sizeof(LONGLONG) ) ;
         tempResult = tempC1 + tempC2 ;
         memcpy( result, &tempResult, sizeof(LONGLONG) ) ;
      #else
         *((LONGLONG *)result) = *((LONGLONG *)c1) + *((LONGLONG *)c2) ;
      #endif
   #else
      short int sign1, sign2;
      char carry ;
      short int loop ;
      long val1, val2, resultVal ;
      #ifdef S4BYTE_SWAP   /* LY 00/12/15 */
         unsigned short tempUShort ;
      #endif

      sign1 = (short int)(getShort((c1->lo[3]))) >= 0 ? 1 : -1 ;
      sign2 = (short int)(getShort((c2->lo[3]))) >= 0 ? 1 : -1 ;

      carry = 0 ;
      for ( loop = 0 ; loop < 4 ; loop++ )
      {
         if ( sign1 == -1 && sign2 == -1 )
         {
            #ifndef S4BYTE_SWAP  /* LY 00/12/15 */
               val1 = -(short int)(getShort(c1->lo[loop])) ;
               val2 = -(short int)(getShort(c2->lo[loop])) ;
            #else
               val1 = -((long)(x4reverseShort( &c1->lo[loop] )) & 0x0000ffff);
               val2 = -((long)(x4reverseShort( &c2->lo[loop] )) & 0x0000ffff);
            #endif
            if ( val1 == 0 )
               val1 = 65536 ;
            if ( val2 == 0 )
               val2 = 65536 ;
            if ( loop != 0 )
               val2-- ;
         }
         else
         {
            #ifndef S4BYTE_SWAP  /* LY 00/12/15 */
               val1 = getShort(c1->lo[loop]) ;
               val2 = getShort(c2->lo[loop]) ;
            #else
               val1 = (long)(x4reverseShort( &c1->lo[loop] )) & 0x0000ffff ;
               val2 = (long)(x4reverseShort( &c2->lo[loop] )) & 0x0000ffff ;
            #endif
         }
         resultVal = val1 + val2 + carry ;
         carry = 0 ;
         if ( (unsigned long)resultVal > (unsigned long)USHRT_MAX )
         {
            #ifdef S4TESTING
               if ( ( resultVal - (long)USHRT_MAX ) > USHRT_MAX )
                  return error4( 0, e4info, E99999 ) ;
            #endif
            carry = 1 ;
            #ifndef S4BYTE_SWAP  /* LY 00/12/15 */
               result->lo[loop] = getShort((int)(resultVal - (long)USHRT_MAX - 1)) ;
            #else
               tempUShort = (unsigned short)(resultVal - (long)USHRT_MAX - 1) ;
               result->lo[loop] = x4reverseShort( &tempUShort ) ;
            #endif
         }
         else
            #ifndef S4BYTE_SWAP  /* LY 00/12/15 */
               result->lo[loop] = getShort((int)resultVal) ;
            #else
               {
                  tempUShort = (unsigned short) resultVal ;
                  result->lo[loop] = x4reverseShort( &tempUShort ) ;
               }
            #endif
         if ( sign1 == -1 && sign2 == -1 )
            #ifndef S4BYTE_SWAP  /* LY 00/12/15 */
               result->lo[loop] = getShort(-((short int)result->lo[loop])) ;
            #else
               {
                  tempUShort = -((short int)result->lo[loop]) ;
                  result->lo[loop] = x4reverseShort( &tempUShort ) ;
               }
            #endif
      }
   #endif

   return 0 ;
}



int S4FUNCTION currency4subtract( CURRENCY4 *result, const CURRENCY4 *c1, const CURRENCY4 *c2 )
{
   // AS 06/23/00, this functionality does not work for very very large numbers.
   // in WIN32, LONGLONG is available, so use it instead.
   // input source is longlong 10000 times greater than actual value (i.e. 4 decimal digits available by dividing input by 10000)
   #ifdef S4WIN32
      #ifdef S4DATA_ALIGN
         LONGLONG tempC1, tempC2, tempResult ;
         memcpy( &tempC1, c1, sizeof(LONGLONG) ) ;
         memcpy( &tempC2, c2, sizeof(LONGLONG) ) ;
         tempResult = tempC1 - tempC2 ;
         memcpy( result, &tempResult, sizeof(LONGLONG) ) ;
      #else
         *((LONGLONG *)result) = *((LONGLONG *)c1) - *((LONGLONG *)c2) ;
      #endif
   #else
      CURRENCY4 temp ;

      c4memcpy( &temp, c2, sizeof( CURRENCY4 ) ) ;
      makeNegative( &temp, &temp ) ;

      currency4add( result, c1, &temp ) ;
   #endif

   return 0 ;
}

int S4FUNCTION currency4multiplyShort( CURRENCY4 *result, const CURRENCY4 *c1, const short c2 )
{
   int loop, sign1, sign2, signResult, pos ;
   CURRENCY4 cur1, hold ;
   unsigned long val1, valResult ;
   short int cur2 ;
   #ifdef S4BYTE_SWAP  /* LY 00/12/13 */
      unsigned short tempUShort ;
   #endif

   #ifdef S4DATA_ALIGN
      short int tempSInt ;
      memcpy( &tempSInt, &c1->lo[3], sizeof(short int) ) ;
      sign1 = getShort( tempSInt ) >= 0 ? 1 : -1 ;
   #else
      sign1 = getShort((short int)(c1->lo[3])) >= 0 ? 1 : -1 ;
   #endif
   sign2 = c2 >= 0 ? 1 : -1 ;

   if ( sign1 == -1 )
   {
      makeNegative( &cur1, c1 ) ;
      signResult = -1 ;
   }
   else
   {
      c4memcpy( &cur1, c1, sizeof( CURRENCY4 ) ) ;
      signResult = 1 ;
   }

   if ( sign2 == -1 )
   {
      cur2 = -c2 ;
      signResult *= -1 ;
   }
   else  /* no change to sign */
      cur2 = c2 ;

   c4memset( result, 0, sizeof( CURRENCY4 ) ) ;

   for ( loop = 0 ; loop < 4 ; loop++ )
   {
      #ifndef S4BYTE_SWAP  /* LY 00/12/13 */
         val1 = getShort(cur1.lo[loop]) ;
      #else
         tempUShort = getShort(cur1.lo[loop]) ;
         val1 = x4reverseShort( &tempUShort ) & 0x0000ffff ;
      #endif
      pos = loop ;
      if ( pos >= 4 )  /* too large to store results */
         break ;
      if ( val1 == 0 )
         continue ;
      valResult = val1 * cur2 ;
      #ifdef S4BYTE_SWAP
         valResult = x4reverseLong( &valResult ) ;
      #endif
      c4memset( &hold, 0, sizeof( CURRENCY4 ) ) ;
      if ( pos == 3 )  /* only least significant bytes important */
         c4memcpy( &(hold.lo[pos]), &valResult, 2 ) ;
      else
         c4memcpy( &(hold.lo[pos]), &valResult, 4 ) ;
      currency4add( result, result, &hold ) ;
   }

   if ( signResult == -1 )
      makeNegative( result, result ) ;

   return 0 ;
}

#ifdef NOT_IMPLEMENTED_YET
   int currency4multiply( CURRENCY4 *result, const CURRENCY4 *c1, const CURRENCY4 *c2 )
   {
      int loop, sign1, sign2, signResult, jLoop, pos ;
      CURRENCY4 cur1, cur2, hold, tenThousand ;
      unsigned long val1, val2, valResult ;

      sign1 = (short int)(c1->lo[3]) >= 0 ? 1 : -1 ;
      sign2 = (short int)(c2->lo[3]) >= 0 ? 1 : -1 ;

      if ( sign1 == -1 )
      {
         makeNegative( &cur1, c1 ) ;
         signResult = -1 ;
      }
      else
      {
         memcpy( &cur1, c1, sizeof( CURRENCY4 ) ) ;
         signResult = 1 ;
      }

      if ( sign2 == -1 )
      {
         makeNegative( &cur2, c2 ) ;
         signResult *= -1 ;
      }
      else  /* no change to sign */
         memcpy( &cur2, c2, sizeof( CURRENCY4 ) ) ;

      memset( result, 0, sizeof( CURRENCY4 ) ) ;
      memset( &tenThousand, 0, sizeof( CURRENCY4 ) ) ;
      tenThousand.lo[0] = 0x2710 ;

      for ( jLoop = 0 ; jLoop < 4 ; jLoop++ )
      {
         val2 = cur2.lo[jLoop] ;
         if ( val2 == 0 )
            continue ;
         for ( loop = 0 ; loop < 4 ; loop++ )
         {
            val1 = cur1.lo[loop] ;
            pos = loop + jLoop ;
            if ( pos >= 4 )  /* too large to store results */
               break ;
            if ( val1 == 0 )
               continue ;
            valResult = val1 * val2 ;
            memset( &hold, 0, sizeof( CURRENCY4 ) ) ;
            if ( pos == 3 )  /* only least significant bytes important */
               memcpy( &(hold.lo[pos]), &valResult, 2 ) ;
            else
               memcpy( &(hold.lo[pos]), &valResult, 4 ) ;
            currency4add( result, result, &hold ) ;
         }
      }

      if ( signResult == -1 )
         makeNegative( result, result ) ;

      return 0 ;
   }
#endif

static void makeNegative( CURRENCY4 *result, const CURRENCY4 *source )
{
   int loop ;
   CURRENCY4 one ;

   if ( ( (short int)(getShort(source->lo[3])) >= 0 ? 1 : -1 ) == -1 )  /* negative */
   {
      c4memset( &one, 0, sizeof( CURRENCY4 ) ) ;
      #ifdef S4BYTE_SWAP  /* LY 2001/01/05 */
         one.lo[0] = 0x0100 ;
      #else
         one.lo[0] = 0x0001 ;
      #endif

      for ( loop = 0; loop < 4 ; loop++ )
         result->lo[loop] = ~source->lo[loop] ;
      currency4add( result, result, &one ) ;
   }
   else /* positive */
   {
      c4memset( &one, (unsigned char)0xFF, sizeof( CURRENCY4 ) ) ;

      c4memcpy( result, source, sizeof( CURRENCY4 ) ) ;
      currency4add( result, result, &one ) ;
      for ( loop = 0; loop < 4 ; loop++ )
         result->lo[loop] = ~result->lo[loop] ;
   }
}

#ifndef S4NO_LONGLONG    // CS 2000/04/19
   static void shiftLeft( long *left, long *right, int numShift )
   {
      int loop ;

      for ( loop = 0 ; loop < numShift ; loop++ )
      {
         *left = *left << 1 ;
         *left = *left | ( 1 * ( *right & 0x80000000 ) ) ;
         *right = *right << 1 ;
      }
   }

   static void shiftRight( long *left, long *right, int numShift )
   {
      int loop ;

      for ( loop = 0 ; loop < numShift ; loop++ )
      {
         *right = *right >> 1 ;
         *right = *right | ( 0x80000000 * ( *left & 0x00000001 ) ) ;
         *left = *left >> 1 ;
      }
   }
#endif

int S4FUNCTION currency4divideShort( CURRENCY4 *result, const CURRENCY4 *c1, const short c2 )
{
   unsigned short int shortResult ;
   short int cur2 ;
   unsigned long shortMod ;
   int sign1, sign2, signResult, loop ;
   CURRENCY4 add, cur1 ;
   #ifdef S4BYTE_SWAP  /* LY 00/12/15 */
      unsigned short tempUShort ;
   #endif

   sign1 = (short int)(getShort(c1->lo[3])) >= 0 ? 1 : -1 ;
   sign2 = c2 >= 0 ? 1 : -1 ;

   if ( sign1 == -1 )
   {
      makeNegative( &cur1, c1 ) ;
      signResult = -1 ;
   }
   else
   {
      c4memcpy( &cur1, c1, sizeof( CURRENCY4 ) ) ;
      signResult = 1 ;
   }

   if ( sign2 == -1 )
   {
      cur2 = -c2 ;
      signResult *= -1 ;
   }
   else  /* no change to sign */
      cur2 = c2 ;

   if ( cur2 == 0 )
      return -1 ;

   c4memset( result, 0, sizeof( CURRENCY4 ) ) ;

   if ( *((long *)(&cur1.lo[2])) == 0 )  /* value smaller than max long */
   {
      *((long *)(&result->lo[0])) = *((long *)(&cur1.lo[0])) / cur2 ;
      if ( signResult == -1 )
         makeNegative( result, result ) ;
      return 0 ;
   }

   shortMod = 0 ;
   for ( loop = 3 ; loop >= 0 ; loop-- )
   {
      c4memset( &add, 0, sizeof( CURRENCY4 ) ) ;
      #ifndef S4BYTE_SWAP  /* LY 00/12/15 */
         shortResult = (unsigned short)(((unsigned long)cur1.lo[loop] + shortMod )/ cur2) ;
         c4memcpy( ((unsigned short int *)(&add)) + loop, &shortResult, sizeof( short int ) ) ;
      #else
         tempUShort = x4reverseShort( &cur1.lo[loop] ) ;
         shortResult = (unsigned short)((((unsigned long)(tempUShort) & 0x0000ffff) + shortMod )/ cur2) ;
         add.lo[loop] = x4reverseShort( &shortResult ) ;
      #endif
      if ( loop != 0 )
         #ifndef S4BYTE_SWAP  /* LY 00/12/15 */
            shortMod = ((unsigned long)(USHRT_MAX)+1) * ((cur1.lo[loop] + shortMod) % cur2 ) ;
         #else
            shortMod = ((unsigned long)(USHRT_MAX)+1) * ((tempUShort + shortMod) % cur2 ) ;
         #endif
      currency4add( result, result, &add) ;
   }

   if ( signResult == -1 )
      makeNegative( result, result ) ;

   return 0 ;
}

#ifdef NOT_IMPLEMENTED_YET
   int currency4divide( CURRENCY4 *result, const CURRENCY4 *c1, const CURRENCY4 *c2 )
   {
      long topLeft, topRight, bottomLeft, bottomRight, resultRight ;
      long stopShiftVal, holdLeft, holdRight ;
      int sign1, sign2, signResult, numShift, signHold ;
      CURRENCY4 add, subtract, hold, hold2, cur1, cur2 ;

      sign1 = (short int)(c1->lo[3]) >= 0 ? 1 : -1 ;
      sign2 = (short int)(c2->lo[3]) >= 0 ? 1 : -1 ;

      if ( sign1 == -1 )
      {
         makeNegative( &cur1, c1 ) ;
         signResult = -1 ;
      }
      else
      {
         memcpy( &cur1, c1, sizeof( CURRENCY4 ) ) ;
         signResult = 1 ;
      }

      if ( sign2 == -1 )
      {
         makeNegative( &cur2, c2 ) ;
         signResult *= -1 ;
      }
      else  /* no change to sign */
         memcpy( &cur2, c2, sizeof( CURRENCY4 ) ) ;

      topLeft = *((long *)(&cur1.lo[2])) ;
      topRight = *((long *)(&cur1.lo[0])) ;
      bottomLeft = *((long *)(&cur2.lo[2])) ;
      bottomRight = *((long *)(&cur2.lo[0])) ;

      if ( bottomLeft == 0 && bottomRight == 0 )  /* divide by zero */
         return -1 ;

      memset( result, 0, sizeof( CURRENCY4 ) ) ;

      if ( topLeft == 0 )  /* value smaller than max long */
      {
         if ( bottomLeft != 0 )  /* bottom larger than top */
            return 0 ;
         if ( bottomRight == 0 )
            return -1 ;
         resultRight = topRight / bottomRight ;
         memcpy( &(result->lo[0]), &resultRight, sizeof(S4LONG ) ) ;
         if ( signResult == -1 )
            makeNegative( result, result ) ;
         return 0 ;
      }

      if ( topLeft < bottomLeft )   /* zero return */
      {
         if ( signResult == -1 )
            makeNegative( result, result ) ;
         return 0 ;
      }

      if ( topLeft == bottomLeft )  /* either one or zero return */
      {
         if ( topRight >= bottomRight )
         {
            result->lo[0] = 1 ;
            if ( signResult == -1 )
               makeNegative( result, result ) ;
         }
         return 0 ;
      }

      /* case where topLeft > 0 && topLeft > bottomLeft */
      memcpy( &(hold.lo[0]), &topRight, sizeof(S4LONG ) ) ;
      memcpy( &(hold.lo[2]), &topLeft, sizeof(S4LONG ) ) ;
      stopShiftVal = topLeft / 2 ;
      for ( ;; )
      {
         numShift = -1 ;
         while( bottomLeft < stopShiftVal )
         {
            holdLeft = bottomLeft ;
            holdRight = bottomRight ;
            shiftLeft( &bottomLeft, &bottomRight, 1 ) ;
            numShift++ ;
         }

         bottomLeft = holdLeft ;
         bottomRight = holdRight ;

         memset( &add, 0, sizeof( CURRENCY4 ) ) ;

         currency4add( result, result, &add ) ;

         #ifdef E4DEBUG
            if ( ( (short int)(hold->lo[3]) >= 0 ? 1 : -1 ) == -1 )
               return error4( 0, e4info, E90542 ) ;
         #endif

         for ( signHold = 1 ;; )
         {
            memcpy( &(subtract.lo[0]), &holdRight, sizeof(S4LONG ) ) ;
            memcpy( &(subtract.lo[2]), &holdLeft, sizeof(S4LONG ) ) ;
            memcpy( &hold2, &hold, sizeof( CURRENCY4 ) ) ;
            currency4subtract( &hold, &hold, &subtract ) ;
            signHold = (short int)(hold.lo[3]) >= 0 ? 1 : -1 ;
            if ( signHold == -1 )
               break ;
         }

         memcpy( &hold, &hold2, sizeof( CURRENCY4 ) ) ;
         topLeft = *((long *)(&hold.lo[2])) ;
         topRight = *((long *)(&hold.lo[0])) ;

         stopShiftVal = topLeft / 2 ;
         while ( bottomLeft > topLeft )
         {
            shiftRight( &bottomLeft, &bottomRight, 1 ) ;
            numShift-- ;
         }
      }

      if ( signResult == -1 )
         makeNegative( result, result ) ;

      return 0 ;
   }
#endif

/*
int currency4mod( CURRENCY4 *result, const CURRENCY4 *c1, const CURRENCY4 *c1 )
{

}
*/

int S4FUNCTION c4currencyToA( char *out, int outLen, const CURRENCY4 *sourceIn, short numDec, int *finalLen )
{
   // AS 06/23/00, this functionality does not work for very very large numbers.
   // in WIN32, LONGLONG is available, so use it instead.
   // input source is longlong 10000 times greater than actual value (i.e. 4 decimal digits available by dividing input by 10000)
   #ifndef S4NO_LONGLONG  // CS 2001/04/19
      #ifdef S4DATA_ALIGN  // LY 00/07/24 : for S4WINCE
         LONGLONG inputVal ;
         memcpy( &inputVal, (void *)sourceIn, sizeof(LONGLONG) ) ;   // LY 00/11/20 : cast to void*
      #else
         LONGLONG inputVal = *((LONGLONG *)sourceIn) ;
      #endif
      unsigned char buf[21] ;
      memset( buf, 0, sizeof( buf ) ) ;
      memset( out, 0, outLen ) ;

      LONGLONG absoluteVal ;
      int sign ;
      if ( inputVal < 0 )
      {
         sign = -1 ;
         absoluteVal = -inputVal ;
      }
      else
      {
         sign = 1 ;
         absoluteVal = inputVal ;
      }

      // get the decimal portion first.
      long decimals = (long)(absoluteVal % 10000) ;
      char decBuf[5] ;
      c4ltoa45( decimals, decBuf, -4 ) ;

      // trim off any extra decimals as required (input specifies the # of decimals to include)
      int pos = 20 - numDec ;
      if ( numDec > 0 )  // need to insert decimals
      {
         memcpy( buf + pos, decBuf, numDec ) ;
         pos-- ;
         buf[pos] = '.' ;
      }
      pos-- ;

      absoluteVal /= 10000 ;

      if ( absoluteVal == 0 )  // special case, do format "0.xxxx" not ".xxxx"
      {
         buf[pos] = '0' ;
         pos-- ;
      }
      else
      {
         for( ; pos >= 0 && absoluteVal != 0 ; pos-- )
         {
            char val = (char) (absoluteVal % 10) ;
            absoluteVal /= 10 ;
            buf[pos] = '0' + val ;
         }
      }

      pos++ ;

      int outPos ;
      if ( sign < 0 )
      {
         out[0] = '-' ;
         outPos = 1 ;   // offset into output buffer
      }
      else
         outPos = 0 ;

      /* LY 2001/09/13 : no min() in ANSI C */
      int lenToCopy = ( outLen - outPos < 20 - pos ? outLen - outPos : 20 - pos ) ;
      memcpy( out+outPos, buf+pos, lenToCopy ) ;
      if ( finalLen != 0 )
         *finalLen = lenToCopy ;
   #else
      int sign, pos, loop, reqdLen ;
      CURRENCY4 hold, old, mod, sv, source ;
      unsigned char buf[21] ;
      unsigned long l1 ;

      if ( finalLen != 0 )
         *finalLen = 0 ;

      c4memcpy( &source, sourceIn, sizeof( CURRENCY4 ) ) ;
      sign = (short int)getShort((source.lo[3])) >= 0 ? 1 : -1 ;
      if ( sign == -1 )
         makeNegative( &source, &source ) ;

      c4memcpy( &hold, &source, sizeof( CURRENCY4 ) ) ;
      c4memset( buf, 0, sizeof( buf ) ) ;

      for ( pos = 0 ; pos < 20 ; pos++ )
      {
         if ( *((long *)(&hold.lo[2])) == 0L )  /* only right portion */
            break ;
         c4memcpy( &old, &hold, sizeof( CURRENCY4 ) ) ;
         currency4divideShort( &hold, &hold, 10 ) ;
         currency4multiplyShort( &sv, &hold, 10 ) ;
         currency4subtract( &mod, &old, &sv ) ;
         buf[pos] = (char)mod.lo[0] ;
      }

      /* if any portion is left, it is only the right portion */
      if ( *((long *)(&hold.lo[2])) == 0L )  /* only right portion */
      {
         #ifdef S4BYTE_SWAP
            l1 = x4reverseLong(((long *)(&hold.lo[0]))) ;
         #else
            l1 = *((long *)(&hold.lo[0])) ;
         #endif

         if ( l1 == 0 )
         {
            if ( pos == 0 )   /* zero */
               pos = -1 ;
         }
         else
         {
            for ( ; pos < 20 ; )
            {
               buf[pos] = (unsigned char) (l1 % 10) ;
               l1 = l1 / 10 ;
               if ( l1 == 0 )
                  break ;
               pos++ ;
            }
         }
      }

      c4memset( out, 0, outLen ) ;

      if ( pos == -1 )
      {
         reqdLen = 6 ;  /* x.xxxx */
         c4memset( out, '0', 5 ) ;
      }
      else
      {
         reqdLen = pos + 2 ; /* for decimal point and because pos is 1 less than amount */
         if ( reqdLen < 6 )
         {
            c4memset( out, '0', 5 ) ;
            reqdLen = 6 ;  /* x.xxxx */
         }
      }


      if ( (reqdLen+1) > outLen + (sign == -1) )   /* insufficient space */
         return -1 ;

      loop = 5 - (pos+1) ;
      if ( loop < 0 )
         loop = 0 ;
      for ( ; pos >= 0 ; pos--, loop++ )  /* reverse the string */
         out[loop] = buf[pos] + '0' ;

      if ( finalLen != 0 )
         *finalLen = reqdLen ;

      /* now reposition for the decimal points */
      c4memmove( &(out[reqdLen-4]), &(out[reqdLen-5]), 4 ) ;
      out[reqdLen-5] = '.' ;

      /* now remove any decimal points according to input */
      if ( numDec == 0 )
      {
         reqdLen -= 5 ;
         out[reqdLen] = 0 ;
      }
      else
      {
         if ( numDec > 4 || numDec < 0 )
            numDec = 4 ;
         out[reqdLen-4+numDec] = 0 ;
         reqdLen -= (4 - numDec) ;
      }

      if ( sign == -1 )
      {
         c4memmove( out+1, out, reqdLen + 1 ) ;
         out[0] = '-' ;
      }
   #endif

   return 0 ;
}



int S4FUNCTION c4atoCurrency( CURRENCY4 *result, const char *str, int strLen )
{
   /*
      converts string from [+/-] [#]+ [.] [#]+
      i.e. all pieces are optional, #'s may be multiple.  eg. "-23.47" is valid, as is
      ".003", "3", etc.  Maximum of 4 digits after the decimal.
      returns -1 if an invalid currency input string
  */
   char buf[21] ;
   int loop, len, numDecimals, numWhole, sign, numDigits ;
   short int multiplier, mPower ;
   long val ;
   const char *decPt ;
   CURRENCY4 temp, mult ;
   char *ptr ;
   #ifdef S4DATA_ALIGN
      short int tempSInt ;
   #endif

   c4memset( (void*)result, 0, sizeof( CURRENCY4 ) ) ;  /* LY 00/11/09 : cast to void* */

   decPt = 0 ;
   len = strLen ;
   if ( len == 0 )
      return 0 ;
   for ( ;; )
   {
      if ( str[0] != ' ' )
         break ;
      len-- ;
      str++ ;
      if ( len == 0 )
         return 0 ;
   }
   if ( str[0] == '-' )
   {
      sign = -1 ;
      str++ ;
      len-- ;
   }
   else
   {
      sign = 1 ;
      if ( str[0] == '+' )
      {
         str++ ;
         len-- ;
      }
   }
   while ( str[0] == '0' && str[1] == '0' )  /* get rid of leading zeros */
   {
      str++ ;
      len-- ;
   }
   numDecimals = 0 ;
   numWhole = len ;
   for ( loop = len - 1 ; loop >= 0 ; loop-- )
   {
      if ( str[loop] == '.' )
      {
         numDecimals = len - loop - 1 ;
         numWhole = len - numDecimals - 1 ;
         decPt = &str[loop]+1 ;
         break ;
      }
   }

   if ( numDecimals > 4 )
      return -1 ;

   numDigits = numWhole + 4 ;

   if ( numDigits > 20 )
      return -1 ;

   c4memcpy( buf, str, numWhole ) ;

   if ( decPt != 0 )
      c4memmove( buf+numWhole, decPt, numDecimals ) ;

   for ( loop = numDecimals ; loop < 4 ; loop++ )
   {
      buf[numWhole+loop] = '0' ;
   }

   buf[numDigits] = 0 ;

   if ( numDigits < 10 || ( numDigits == 10 && ( str[0] <= '3' && str[0] >= '0' ) ) )
   {
      /* fits in a single long */

      val = c4atol( buf, c4strlen( buf ) ) ;
      if ( sign == -1 )
         val = -val ;

      if ( val == 0 )   /* negative 0 is still zero */
         sign = 1 ;
      if ( sign == 1 )   /* positive value */
      {
         #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
            /* LY 00/11/09 : cast to void* for IA-64 */
            memset( (void*)(result+sizeof(short int)*2), 0, sizeof(short int)*2 ) ;
         #else
            result->lo[3] = 0 ;
            result->lo[2] = 0 ;
         #endif
      }
      else   /* negative value */
      {
         #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
            /* LY 00/11/09 : cast to void* for IA-64 */
            memset( (void*)(result+sizeof(short int)*2), 0xff, sizeof(short int)*2 ) ;
         #else
            result->lo[3] = 0xFFFF ;
            result->lo[2] = 0xFFFF ;
         #endif
      }
      #ifdef S4BYTE_SWAP
         val = x4reverseLong( &val ) ;
      #endif

      c4memcpy( (void*)result, &val, sizeof(S4LONG ) ) ;   /* LY 00/11/09 : cast to void* */

      return 0 ;
   }

   /* more complex scenario because number is larger than a long */
   /* first get the first 9 bytes and store to long */
   val = c4atol( buf, 9 ) ;
   #ifdef S4BYTE_SWAP  /* LY 00/12/13 : byte swap before passing to currency4***() */
      val = (double)x4reverseLong( &val ) ;
   #endif
   c4memcpy( &result->lo[0], &val, sizeof(S4LONG) ) ;

   ptr = buf + 9 ;
   numDigits -= 9 ;
   while ( numDigits > 4 )
   {
      multiplier = c4atoi( ptr, 4 ) ;
      c4memset( &mult, 0, sizeof( CURRENCY4 ) ) ;
      mult.lo[0] = 10000 ;
      currency4multiplyShort( &temp, result, 10000 ) ;
      #ifdef S4WIN64
         memcpy( result, &temp, sizeof(CURRENCY4) ) ;
      #else
         *result = temp ;
      #endif
      if ( multiplier != 0 )  /* add */
      {
         c4memset( &temp, 0, sizeof( temp ) ) ;
         c4memcpy( &(temp.lo[0]), &multiplier, sizeof( short ) ) ;
         currency4add( result, result, &temp ) ;
      }
      numDigits -= 4 ;
      ptr += 4 ;
   }

   if ( numDigits != 0 )
   {
      multiplier = c4atoi( ptr, numDigits ) ;
      mPower = 1 ;
      for ( ; numDigits > 0 ; numDigits-- )
         mPower *= 10 ;
      currency4multiplyShort( &temp, result, mPower ) ;
      #ifdef S4WIN64
         memcpy( result, &temp, sizeof(CURRENCY4) ) ;
      #else
         *result = temp ;
      #endif
      if ( multiplier != 0 )  /* add */
      {
         c4memset( &temp, 0, sizeof( temp ) ) ;
         c4memcpy( &(temp.lo[0]), &multiplier, sizeof( short ) ) ;
         currency4add( result, result, &temp ) ;
      }
   }

   if ( sign == -1 )
   {
      c4memset( &mult, 0, sizeof( CURRENCY4 ) ) ;
      mult.lo[0] = 0x0001 ;
      currency4subtract( result, result, &mult ) ;
      for ( loop = 0; loop < 4 ; loop++ )
      #ifdef S4DATA_ALIGN
      {
         memcpy( &tempSInt, &result->lo[loop], sizeof(short int) ) ;
         tempSInt = ~tempSInt ;
         memcpy( &result->lo[loop], &tempSInt, sizeof(short int) ) ;
      }
      #else
         result->lo[loop] = ~result->lo[loop] ;
      #endif
   }

   return 0 ;
}



#ifndef S4OFF_WRITE
   void S4FUNCTION f4assignCurrency( FIELD4 *field, const char *str )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( field, 3, E90541 ) )
            return ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 || str == 0 )
         {
            error4( 0, e4parm_null, E90541 ) ;
            return ;
         }
      #endif

      if ( field->type != r4currency )
      {
         #ifdef E4PARM_HIGH
            error4( field->data->codeBase, e4parm, E81409 ) ;
         #endif
         return ;
      }

      #ifdef E4ANALYZE
         if ( field->data == 0 )
         {
            error4( 0, e4struct, E90541 ) ;
            return ;
         }
         if ( field->data->codeBase == 0 )
         {
            error4( 0, e4struct, E90541 ) ;
            return ;
         }
      #endif

      #ifdef E4PARM_HIGH
         if ( field->type != r4currency )
         {
            error4( field->data->codeBase, e4parm, E81404 ) ;
            return ;
         }
      #endif

      if ( error4code( field->data->codeBase ) < 0 )
         return ;

      #ifndef S4SERVER
         #ifndef S4OFF_ENFORCE_LOCK
            if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
               if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
               {
                  error4( field->data->codeBase, e4lock, E90541 ) ;
                  return ;
               }
         #endif
      #endif

      c4atoCurrency( (CURRENCY4 *)f4assignPtr( field ), str, c4strlen( str ) ) ;
   }
#endif /* S4OFF_WRITE */

/* this function uses the same memory as f4str() */
char *S4FUNCTION f4currency( const FIELD4 *field, short numDec )
{
   CODE4 *codeBase ;

   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90542 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 || numDec < 0 || numDec > 4 )
      {
         error4( 0, e4parm, E90542 ) ;
         return 0 ;
      }
   #endif

   codeBase = field->data->codeBase ;

   #ifdef E4PARM_HIGH
      if ( field->type != r4currency )
      {
         error4( codeBase, e4parm, E81409 ) ;
         return 0 ;
      }
   #endif

   if ( error4code( codeBase ) < 0 )
      return 0 ;

   if ( codeBase->bufLen <= 20 )   /* not room for field length + null */
   {
      if ( u4allocAgain( codeBase, &codeBase->fieldBuffer, &codeBase->bufLen, 21 ) < 0 )
      {
         #ifdef E4STACK
            error4stack( codeBase, e4memory, E90542 ) ;
         #endif
         return 0 ;
      }
   }
   else
      codeBase->fieldBuffer[20] = 0 ;

   c4currencyToA( codeBase->fieldBuffer, 20, (CURRENCY4 *)f4ptr( field ), numDec, 0 ) ;
   return codeBase->fieldBuffer ;
}

#ifdef S4CLIENT_OR_FOX
   #ifndef S4OFF_WRITE
      void S4FUNCTION f4assignNotNull( FIELD4 *field )
      {
         FIELD4 *nullFlags ;
         unsigned short int byteNum, offset ;
         char mask ;
         char *fPtr ;

         if ( d4version( field->data ) != 0x30 )
            return ;

         if ( field->null == 1 )
         {
            if ( f4null( field ) )
            {
               nullFlags = &(field->data->fields[d4numFields(field->data)]) ;
               #ifdef E4MISC
                  if ( nullFlags->type != r4system )
                  {
                     error4( field->data->codeBase, e4data, E83805 ) ;
                     return ;
                  }
               #endif
               byteNum = field->nullBit / 8 ;
               #ifdef E4MISC
                  if ( nullFlags->len < ( byteNum + 1 ) )
                  {
                     error4( field->data->codeBase, e4data, E83805 ) ;
                     return ;
                  }
               #endif
               offset = ( field->nullBit - (byteNum * 8) ) ;
               mask = (unsigned int)0x01 << offset ;
               mask = ~mask ;
               fPtr = f4ptr( nullFlags ) ;
               fPtr[byteNum] = fPtr[byteNum] & mask ;
            }
         }
      }

      void S4FUNCTION f4assignNull( FIELD4 *field )
      {
         FIELD4 *nullFlags ;
         unsigned short int byteNum, offset ;
         char mask ;
         char *fPtr ;

         #ifdef E4VBASIC
            if ( c4parm_check( field, 3, E90539 ) )
               return ;
         #endif

         #ifdef E4PARM_HIGH
            if ( field == 0 )
            {
               error4( 0, e4parm_null, E90539 ) ;
               return ;
            }
         #endif

         if ( error4code( field->data->codeBase ) < 0 )
            return ;

         if ( d4version( field->data ) != 0x30 )
         {
            error4( field->data->codeBase, e4parm, E81409 ) ;
            return ;
         }

         #ifndef S4SERVER
            #ifndef S4OFF_ENFORCE_LOCK
               if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
                  if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
                  {
                     error4( field->data->codeBase, e4lock, E90539 ) ;
                     return ;
                  }
            #endif
         #endif

         if ( field->null == 1 )
         {
            nullFlags = &(field->data->fields[d4numFields(field->data)]) ;
            #ifdef E4MISC
               if ( nullFlags->type != r4system )
               {
                  error4( field->data->codeBase, e4data, E83805 ) ;
                  return ;
               }
            #endif
            byteNum = field->nullBit / 8 ;
            #ifdef E4MISC
               if ( nullFlags->len < ( byteNum + 1 ) )
               {
                  error4( field->data->codeBase, e4data, E83805 ) ;
                  return ;
               }
            #endif
            offset = ( field->nullBit - (byteNum * 8 ) ) ;
            mask = 0x01 << offset ;
            fPtr = f4assignPtr( nullFlags ) ;
            fPtr[byteNum] = fPtr[byteNum] | mask ;
         }
         #ifdef E4PARM_HIGH
            else
               error4( field->data->codeBase, e4parm, E81409 ) ;
         #endif
      }
   #endif  /* S4OFF_WRITE */

   int S4FUNCTION f4null( const FIELD4 *field )
   {

      #ifdef E4VBASIC
         if ( c4parm_check( field, 3, E90540 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 )
            return error4( 0, e4parm_null, E90540 ) ;
      #endif

      // AS 05/24/00 not strictly required, since would be 0 in other case...
      #ifdef E4ANALYZE
         if ( d4version( field->data ) != 0x30 && field->null == 1 )
            return error4( field->data->codeBase, e4data, E83805 ) ;
      #endif

      if ( field->null == 1 )
      {
         // AS 05/24/00 for efficiency, track null info better...
         // nullFlags = &(field->data->fields[d4numFields(field->data)]) ;

         #ifdef E4MISC
            if ( field->data->nullFlags->type != r4system )
               return error4( field->data->codeBase, e4data, E83805 ) ;
         #endif
         // unsigned short int byteNum = field->nullBit / 8 ;
         #ifdef E4MISC
            if ( field->data->nullFlags->len < ( field->nullBitByteNum + 1 ) )
               return error4( field->data->codeBase, e4data, E83805 ) ;
         #endif
         // unsigned short int offset = ( field->nullBit - (byteNum * 8 ) ) ;
         // char mask = 0x01 << offset ;
         if ( ( f4ptr( field->data->nullFlags )[field->nullBitByteNum] & field->nullBitMask ) != 0 )
            return 1 ;
      }
      return 0 ;
   }
#else
   int S4FUNCTION f4null( const FIELD4 *field )
   {
      // for mdx/clipper never null, so just return that...
      return 0 ;
   }
#endif /* S4CLIENT_OR_FOX */

static void time4assign( char *ptr, unsigned long inVal )
{
   // AS 09/14/00 - removed millisecond portion.  FoxPro ignores this part, and in fact always rounds up and often
   // inserts '1.999' if '2' is requested.
   // inVal is # of seconds (i.e. 1000 = 1 second)
   // output is the time in HH:MM:SS format, where hour is 24hour clock,
   long seconds, minutes, hours, milliSeconds, val ;

   val = inVal / 1000 ;  /* remove the thousandths's of seconds */

   // see if we need to round up...
   milliSeconds = inVal - ( val * 1000 ) ;
   if ( milliSeconds >= 500 ) // round up
      val++ ;

   seconds = val % 60 ;
   val /= 60 ;
   minutes = val % 60 ;
   val /= 60 ;
   hours = val ;

   c4ltoa45( hours, ptr, -2 ) ;  /* - means fill with 0s */
   ptr[2] = ':' ;
   c4ltoa45( minutes, ptr+3, -2 ) ;  /* - means fill with 0s */
   ptr[5] = ':' ;
   c4ltoa45( seconds, ptr+6, -2 ) ;  /* - means fill with 0s */
   // AS 01/26/00 - changed code to always return a decimal portion, and in ".xxx" format
   // if ( milliSeconds != 0 )
   // {
   //    ptr[8] = '.' ;
   //    c4ltoa45( milliSeconds, ptr+9, -3 ) ;  /* - means fill with 0s */
   // }
}



char S4PTR *S4FUNCTION f4dateTime( const FIELD4 *field )
{
   /* this function uses the same memory as f4str() */
   // AS 09/14/00 - removed millisecond portion.  FoxPro ignores this part, and in fact always rounds up and often
   // inserts '1.999' if '2' is requested.
   CODE4 *codeBase ;
   long val ;
   char *fPtr ;

   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90543 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm, E90543 ) ;
         return 0 ;
      }
   #endif

   codeBase = field->data->codeBase ;

   #ifdef E4PARM_HIGH
      if ( field->type != r4dateTime )
      {
         error4( codeBase, e4parm, E81409 ) ;
         return 0 ;
      }
   #endif

   if ( error4code( codeBase ) < 0 )
      return 0 ;

   const int date4timeLen = 16 ;

   if ( codeBase->bufLen <= date4timeLen )   /* not room for field length + null*/
   {
      if ( u4allocAgain( codeBase, &codeBase->fieldBuffer, &codeBase->bufLen, date4timeLen + 1 ) < 0 )
      {
         #ifdef E4STACK
            error4stack( codeBase, e4memory, E90542 ) ;
         #endif
         return 0 ;
      }
   }
   c4memset( codeBase->fieldBuffer, ' ', date4timeLen ) ;
   codeBase->fieldBuffer[date4timeLen] = 0 ;

   fPtr = f4ptr( field ) ;
   #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
      memcpy( &val, fPtr, sizeof(S4LONG) ) ; /* date */
   #else
      val = *((long *)fPtr) ; /* date */
   #endif
   date4assign( codeBase->fieldBuffer, val ) ;
   #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
      /* LY 00/11/09 : remove S4LONG* from fPtr for IA-64 */
      memcpy( &val, fPtr + sizeof(S4LONG), sizeof(S4LONG) ) ; /* time */
   #else
      val = *(((long *)fPtr) + 1) ; /* time */
   #endif
   time4assign( codeBase->fieldBuffer + 8, val ) ;
   c4trimN( codeBase->fieldBuffer, date4timeLen + 1 ) ;
   return codeBase->fieldBuffer ;
}



void S4FUNCTION f4assignDateTime( FIELD4 *field, const char *dateTime )
{
   // AS 09/14/00 - removed millisecond portion.  FoxPro ignores this part, and in fact always rounds up and often
   // inserts '1.999' if '2' is requested.
   /* input date time of format: "YYYYMMDDHH:MM:SS" */
   #ifdef S4OFF_WRITE
      error4( field->data->codeBase, e4notSupported, E90543 ) ;
   #else
      long date, time ;
      #ifdef S4DATA_ALIGN  /* LY 00/11/06 : changed from S4WINCE */
         char *buffPtr ;
      #endif

      if ( field->type != r4dateTime )
      {
         #ifdef E4PARM_HIGH
            error4( field->data->codeBase, e4parm, E81409 ) ;
         #endif
         return ;
      }

      date = date4long( dateTime ) ;
      time = time4long( dateTime + 8, c4strlen( dateTime ) - 8 ) ;

      #ifdef S4DATA_ALIGN  /* LY 00/11/16 : changed from S4WINCE */
         buffPtr = (char *) f4assignPtr( field ) ;
         memcpy( buffPtr, &date, sizeof(long) ) ;
         memcpy( buffPtr+sizeof(long), &time, sizeof(long) ) ;
      #else
         *((long *)f4assignPtr( field )) = date ;
         *(((long *)f4assignPtr( field ))+1) = time ;
      #endif
   #endif /* S4OFF_WRITE */
}

short S4FUNCTION f4len_v( FIELD4 *f4 )
{
   return (short)f4len( f4 );
}

#ifdef S4VB_DOS
   char * f4name_v( FIELD4 *fld )
   {
      return v4str( f4name(fld) ) ;
   }
#endif /* S4VB_DOS */
