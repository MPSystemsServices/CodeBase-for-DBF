/* i4conv.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef P4ARGS_USED
   #pragma argsused
#endif

#ifdef S4WIN32    /* LY 00/01/24: for 32-bit only */
   // include longlong conversion...
   #include "c4long.c"
#endif

BOOL s5fox=FALSE, s5mdx=FALSE, s5clipper=FALSE,s5hasDescending=TRUE ;

#ifndef S4NO_LONGLONG
   int quad5longDivideShort( void *resultIn, const void *c1In, const short c2 )
   {
      QUAD5LONG *result = (QUAD5LONG *)resultIn ;
      const QUAD5LONG *c1 = (QUAD5LONG *)c1In ;

      // function to divide a ulong by a short that results in value > ulonglong
      // i.e. > 64 bit longlong value
      // addapted from CURRENCY4 functions in f4field.c
      // returns the remainder if any
      unsigned long shortResult ;
      short int cur2 ;
      ULONGLONG shortMod ;
      int loop, remainder ;
      QUAD5LONG add, cur1 ;

      assert5( c2 != 0 && result != 0 && c1 != 0 ) ;

      memcpy( &cur1, c1, sizeof( QUAD5LONG ) ) ;
      cur2 = c2 ;

      memset( result, 0, sizeof( QUAD5LONG ) ) ;

      if ( *((ULONGLONG *)(&cur1.lo[2])) == 0 )  /* value smaller than max long */
      {
         remainder = (int) *((ULONGLONG *)(&cur1.lo[0])) % cur2 ;
         *((ULONGLONG *)(&result->lo[0])) = *((ULONGLONG *)(&cur1.lo[0])) / cur2 ;
         return remainder ;
      }

      shortMod = 0 ;
      for ( loop = 3 ; loop >= 0 ; loop-- )
      {
         memset( &add, 0, sizeof( QUAD5LONG ) ) ;
         shortResult = (unsigned long)(((ULONGLONG)cur1.lo[loop] + shortMod ) / cur2) ;
         memcpy( ((unsigned long *)(&add)) + loop, &shortResult, sizeof( unsigned long ) ) ;
         shortMod = ((ULONGLONG)(ULONG_MAX)+1) * ((cur1.lo[loop] + shortMod) % cur2 ) ;
         quad5longAdd( result, result, &add) ;
      }

      return (int)shortMod ;
   }



   void quad5longAdd( QUAD5LONG *result, const QUAD5LONG *c1, const QUAD5LONG *c2 )
   {
      // function to add 2 ulonglong values which may result in > ulonglong value
      // i.e. result > 64 bit longlong
      // addapted from CURRENCY4 functions in f4field.c
      char carry ;
      short int loop ;
      ULONGLONG val1, val2, resultVal ;

      carry = 0 ;
      for ( loop = 0 ; loop < 4 ; loop++ )
      {
         val1 = c1->lo[loop] ;
         val2 = c2->lo[loop] ;
         resultVal = val1 + val2 + carry ;
         carry = 0 ;
         if ( resultVal > ULONG_MAX )
         {
            assert5( ( resultVal - (long)ULONG_MAX ) > ULONG_MAX ) ;
            carry = 1 ;
            result->lo[loop] = (int)(resultVal - (long)ULONG_MAX - 1) ;
         }
         else
            result->lo[loop] = (int)resultVal ;
      }
   }
#endif


void e4applyAscend( int keyType, char *buffer, unsigned long len )
{
   // AS 08/11/99 --> function added to independently allow performing ascends on strings
   // (i.e. without using an EXPR4) *)

   switch( keyType )
   {
      case r4num:
         c4clip( buffer, len, 0 ) ;
         break ;
      case r4dateDoub:
         date4assign( buffer, (long)len ) ;
         break ;
      case r4log:
         if( *((int *)buffer) )
            buffer[0] = '1' ;
         else
            buffer[0] = '0' ;
         break ;
      #ifdef S4FOX
         case r4numDoub:
            t4dblToFox( buffer, *((double *)buffer) ) ;   /* converts a double to an ordered byte */
            break ;
         case r5i2:
            {
               long i2asInt = len ;
               t4intToFox( buffer, &i2asInt ) ;
            }
            break ;
         case r5ui2:
            {
               unsigned long ui2asUnsignedLong = len ;
               t4unsignedIntToFox( buffer, &ui2asUnsignedLong ) ;
            }
            break ;
         case r5ui4:
            {
               unsigned long temp = len ;
               t4unsignedIntToFox( buffer, &temp ) ;
               break ;
            }
         case r4int:
            {
               long temp = len ;
               t4intToFox( buffer, &temp ) ;
               break ;
            }
         #ifndef S4NO_LONGLONG
            case r5i8:
               {
                  LONGLONG temp = len ;
                  t4i8ToFox( buffer, &temp ) ;
                  break ;
               }
         #endif
         case r5dbDate:
            t4dbDateToFox( buffer, ((DBDATE *)buffer) ) ;
            break ;
         case r5dbTime:
            t4dbTimeToFox( buffer, ((DBTIME *)buffer) ) ;
            break ;
         // already implemented by type r4dateTime
         case r5dbTimeStamp:
            t4dbTimeStampToFox( buffer, ((DBTIMESTAMP *)buffer) ) ;
            break ;
         case r4currency:
            t4curToFox( buffer, ((CURRENCY4 *)buffer) ) ;
            break ;
         case r4dateTime:
            t4dateTimeToFox( buffer, ((long *)buffer) ) ;
            break ;
      #endif /* S4FOX */
      /* AS 08/18/99 all index types should put wstr into machine order...*/
      case r5wstr:
      case r5wstrLen:
         // AS 08/09/99 Ascend will always collate the unicode as machine sequence... (i.e. reverse short on chars)
         t4unicodeToMachine( 0, buffer, buffer, len, 0 ) ;
         break ;
      default:  /* r4str, r4date */
         /* in these instances, already in ascending order, so don't need to do anything */
         break ;
   }

}


// AS 09/14/00 - FoxPro does not support millisecond portion, and if assigning '1.0' second sometimes inputs '0.999', so
// don't support it here either.
long S4FUNCTION time4long( const char *time, int strLen )
{
   // convert from format 'HHxMMxSS where 'x' can be any character, so input may
   // be 12:14:32  or may be '12.32.43'
   long val, hold ;

   hold = c4atol( time, 2 ) ;
   val = hold*3600000 ;
   hold = c4atol( time+3, 2 ) ;
   val += hold*60000 ;
   hold = c4atol( time+6, 2 ) ;
   val += hold*1000 ;

   // if ( time[8] != 0 && strLen > 8 )
   // {
   //    hold = c4atol( time+9, 3 ) ;
   //    // AS 09/14/00 - It turns out that FoxPro always rounds the time to the nearest second before converting, so do this instead
   //    // val += hold ;
   //    if ( hold >= 500 )
   //       val += 1000 ;
   // }

   return val ;
}



#if defined( S4CLIENT ) || ( defined( S4FOX ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) ) )
   void t4dblToCur( char *result, const double d )
   {
      /* result must be big enough to hold a CURRENCY4 structure - 8 bytes */
      char currencyBuffer[21] ;
      // AS 06/23/00 - was not zeroing out, causing problems in some cases
      c4memset( currencyBuffer, 0, sizeof( currencyBuffer ) ) ;

      c4dtoa45( d, currencyBuffer, 20, 4 ) ;
      c4atoCurrency( (CURRENCY4 *)result, currencyBuffer, 20 ) ;
   }
#endif



char *c4descendBinary( char *to, const char *from, int len )
{
   for(; len-- > 0; )
      to[len] = ~from[len] ;
   return to ;
}



char *c4descend( char *to, const char *from, int len )
{
   /* note that the CLIPPER version is compatible with Clipper but does not
      properly descend names if nulls are included */
   if ( s5clipper )
   {
      for(; len-- > 0; )
         to[len] = -from[len] ;
   }
   else
   {
      for(; len-- > 0; )
         to[len] = ~from[len] ;
   }

   return to ;
}



#if !defined( S4CLIENT ) && defined( S4FOX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4strToCur( COLLATE4 *collateIn, char *result, const char *input, const int len, int *lenOut )
   {
      // AS 06/23/00 -- ok if collateIn is not zero, shouldn't matter... (t4cur was failing)
      assert5( lenOut != 0 ) ;  // should not be used.
      CURRENCY4 hold ;

      c4atoCurrency( &hold, input, len ) ;
      t4curToFox( result, &hold ) ;
      *lenOut = sizeof( CURRENCY4 ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



#if defined( S4CLIENT ) || defined( S4FOX )
   /*
       COLLATION NOTES:

       There are 2 collations:
          character collation - to apply to indexes for character fields
          unicode collation   - to apply to indexes for unicode fields

       Note that if the fields are used as part of any expressions, they do not qualify
       for collation.  For eg. STR( numericField ) - this cannot be collated.
       eg2. ASCEND( chcaracterFiedl ) - this cannot be collated.

       The collations in these cases is simple machine collation.
   */

   /* used as a filler pointer in collation array for machine sequences where we want to have a
      valid pointer to indicate the collation is loaded, but we don't want to actually have a
      table
   */
   static char unused4filler ;


   #include "coll4arr.c"



   /* the actual array of collations available... */
   COLLATE4 collationArray[NUM4AVAIL_COLLATION_ENTRIES] =
   {
      /* the basic machine byte order sequence...
         Collate4machine
      */
      { collate4machineByteOrder, &unused4filler, &unused4filler, &unused4filler, &unused4filler, UNUSED4, UNUSED4, UNUSED4, 0, 0, UNUSED4, UNUSED4, 0 },

      /* general collation for CodePage 1252
         Collate4generalCp1252
      */
      { collate4subSortCompress, (void *)cp1252generalCollationArray, MUST4GENERATE_ARRAY,
        (void *)cp1252generalCompressArray, MUST4GENERATE_ARRAY, NEED4ONE_EXTRA_BYTE, EXPAND4CHAR_TO_TWO_BYTES, NO4TAIL_BYTES, 0, 0, UNUSED4, UNUSED4, 1 },


      /* general collation for CodePage 437
         Collate4generalCp437
      */
      { collate4subSortCompress, (void *)cp437generalCollationArray, MUST4GENERATE_ARRAY,
        (void *)cp437generalCompressArray, MUST4GENERATE_ARRAY, NEED4ONE_EXTRA_BYTE, EXPAND4CHAR_TO_TWO_BYTES, NO4TAIL_BYTES, 0, 0, UNUSED4, UNUSED4, 1 },

      /* This entry is for testing the loading capability.  Also, users may use this entry
         to create their own customized collating sequences.
         Collate4test
      */
      { collate4unknown, MUST4LOAD_ARRAY, MUST4LOAD_ARRAY, MUST4LOAD_ARRAY, MUST4LOAD_ARRAY,
        MUST4LOAD, MUST4LOAD, MUST4LOAD, 0, 0, MUST4LOAD, MUST4LOAD, MUST4LOAD },

      /* to add an entry, increment NUM4AVAIL_COLLATION_ENTRIES by 1, add a member
         to Collate4name (oldLastValue+1), and append the entry to this array
         Collate4test
      */

   } ;



   void t4convertSubSortCompressUnicode( COLLATE4 *collate, char *resultIn, const char *inputIn, const int lenIn, int *lenOut )
   {
      // same as t4convertSubSortCompressChar but for unicode characters.
      // note that lenIn gives the length in bytes, not characters (divide by 2 to get # characters)
      assert5( collate != 0 && resultIn != 0 && inputIn != 0 && lenIn >= 0 ) ;

      WSTR5 *input = (WSTR5 *)inputIn ;
      WSTR5 *result = (WSTR5 *)resultIn ;

      // AS 07/29/99 -- we need to allocate 4 bytes for every 2 byte character becuase otherwise
      // we have a severely limited character expansion possibility.
      unsigned int numChars = lenIn / 2 ;
      *lenOut = lenIn + lenIn * collate->keySizeCharPerCharAdd ;  // extra 2-byte characters required

      Translate4arrayUnicode *translateArray = (Translate4arrayUnicode *)collate->unicodeToKeyTranslationArray ;
      assert5( translateArray != 0 ) ;

      unsigned short tailCharacters[I4MAX_KEY_SIZE] ;  // array used to temporarily store the tail characters

      unsigned short resultHeadIndex = 0 ;  // index into the (head part) of the result array
      unsigned short resultTailIndex = 0 ;  // index into the tail array

      /* AS 07/28/99 --> FoxPro compatibility requires us to remove trailing blanks before
         conversion, otherwise sort sequence comes out incorrect.  (even though ' ' sorts
         after some other characters, it sorts before all others when it is by itself).

         Do this by reducing the input length for any trailing spaces.
      */

      // AS 08/07/99 --> for unicode, pChar is 0, not 32... ???
      // for ( unsigned int inputIndex = numChars - 1 ; ( inputIndex >= 0 && input[inputIndex] == 0 ) ; inputIndex-- )
      /* LY 00/11/07 : if inputIndex = 0, then inputIndex-- = UINT_MAX >= 0 */
      // for ( unsigned int inputIndex = numChars - 1 ; ( inputIndex >= 0 && inputIndex != UINT_MAX && input[inputIndex] == L' ' ) ; inputIndex-- )
      for ( long inputIndex = numChars - 1 ; ( inputIndex >= 0 && input[inputIndex] == L' ' ) ; inputIndex-- )  // CS 2000/12/01 condition was always true
      {
         numChars-- ; ;
      }

      assert5( numChars >= 0 ) ;

      for ( unsigned int charIndex = 0 ; charIndex < numChars ; charIndex++ )
      {
         unsigned short unicodeToCollate = input[charIndex] ;
         if ( translateArray[unicodeToCollate].headChar == collate->expandOrCompressUnicode )
         {
            // means we have an expansion or compression, the sub-sort character indicates
            // the element number of the compression array which applies for this character...

            Expansion4compressionArray *compressArray = (Expansion4compressionArray *)collate->charToKeyCompressionArray ;
            unsigned int compressIndex = translateArray[unicodeToCollate].tailChar ;

            if ( compressArray[compressIndex].type == expand4 )
            {
               result[resultHeadIndex++] = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar1ForExpansionIndex].headChar ;
               unsigned short tailChar = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar1ForExpansionIndex].tailChar ;
               if ( tailChar != collate->noTailUnicode )  // means we have a tail character...
                  tailCharacters[resultTailIndex++] = x4reverseShort( &tailChar ) ;
               result[resultHeadIndex++] = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar2ForExpansionIndex].headChar ;
               tailChar = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar2ForExpansionIndex].tailChar ;
               if ( tailChar != collate->noTailUnicode )  // means we have a tail character...
                  tailCharacters[resultTailIndex++] = x4reverseShort( &tailChar ) ;
            }
            else
            {
               assert5( compressArray[compressIndex].type == compress4 ) ;
               error4( 0, e4notSupported, E84907 ) ;  // have not coded compression yet, not needed yet for any supported collations
               return ;
            }
         }
         else
         {
            result[resultHeadIndex++] = translateArray[unicodeToCollate].headChar ;
            unsigned short tailChar = translateArray[unicodeToCollate].tailChar ;
            if ( tailChar != collate->noTailUnicode )  // means we have a tail character...
               tailCharacters[resultTailIndex++] = x4reverseShort( &tailChar ) ;
         }
      }

      // now copy the tail characters in...
      assert5( *lenOut >= resultHeadIndex ) ;
      unsigned short maxCopy = *lenOut - resultHeadIndex * 2 ;
      unsigned short amountToCopy = ( maxCopy < resultTailIndex * 2 ? maxCopy : resultTailIndex * 2 ) ;
      c4memcpy( result + resultHeadIndex, tailCharacters, amountToCopy ) ;

      // and set the rest of the bytes to NULL
      assert5( amountToCopy <= maxCopy ) ;
      c4memset( ((char *)(result + resultHeadIndex)) + amountToCopy, 0, maxCopy - amountToCopy ) ;
   }
#endif /* defined( S4FOX ) */



void t4unicodeToMachine( COLLATE4 *collate, char *output, const char *input, const int numBytes, int *lenOut )
{
   // every character must be reversed to get into memcmp order...
   int numChars = numBytes / 2 ;
   short *inputUnicode = (short *)input ;
   short *outputKey = (short *)output ;
   // AS 08/18/99 --> < not <=, else memory overwrite...
   // for ( int charIndex = 0 ; charIndex <= numChars ; charIndex++ )
   for ( int charIndex = 0 ; charIndex < numChars ; charIndex++ )
   {
      outputKey[charIndex] = x4reverseShort( &inputUnicode[charIndex] ) ;
   }

   if ( lenOut != 0 )
      *lenOut = numBytes ;
}



#if !defined( S4CLIENT ) && ( defined( S4FOX ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) ) )
   void t4strToDateTime( COLLATE4 *collate, char *result, const char *input, const int len, int *lenOut )
   {
      // AS 06/26/00 - not requite that collate be 0
      // assert5( collate == 0 && lenOut != 0 ) ;  // should not be used.
      assert5( lenOut != 0 ) ;  // should not be used.
      /* seek string must be:  CCYYMMDDHH:MM:SS */
      long dt[2] ;

      if ( len < 16 )
      {
         c4memset( result, 0, 8 ) ;   /* 4 byte double */
         return ;
      }

      dt[0] = date4long( input ) ;
      dt[1] = time4long( input + 8, len - 8 ) ;
      t4dateTimeToFox( result, dt ) ;

      *lenOut = 2 * sizeof( long ) ;
   }
#endif



#if defined( S4CLIENT ) || ( defined( S4FOX ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) ) )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4strToInt( COLLATE4 *collate, char *result, const char *input, const int len, int *lenOut )
   {
      assert5( lenOut != 0 ) ;  // should not be used.
      long val ;

      val = c4atol( input, len ) ;
      t4intToFox( result, &val ) ;

      *lenOut = sizeof( long ) ;
   }
#endif



#if !defined( S4CLIENT ) && defined( S4FOX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4strToUnsignedInt( COLLATE4 *collate, char *result, const char *input, const int len, int *lenOut )
   {
      assert5( collate == 0 && lenOut != 0 ) ;  // should not be used.
      unsigned long val ;

      val = c4atoul( input, len ) ;
      t4unsignedIntToFox( result, &val ) ;
      *lenOut = sizeof( unsigned long ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4CLIENT ) && defined( S4FOX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4dblToInt( char *result, const double d )
   {
      long val ;

      val = (int)d ;
      t4intToFox( result, &val ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4CLIENT ) && defined( S4FOX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4dblToUnsignedInt( char *result, const double d )
   {
      unsigned long val ;

      if ( d < 0 )  // should be unsigned, just make smallest unsigned long if < 0
         val = 0 ;
      else
         val = (unsigned int)d ;
      t4unsignedIntToFox( result, &val ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */


#if !defined( S4CLIENT ) && ( defined( S4MDX ) || defined( S4CLIPPER ) )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4strToDateMdx( COLLATE4 *dummyCollate, char *result, const char *input, const int dummy, int *dummyIntOut )
   {
      assert5( dummyCollate == 0 && dummyIntOut == 0 ) ;  // not used here, should be 0

      double d = (double) date4long(input) ;
      #ifdef S4BYTE_SWAP
         d = x4reverseDouble( &d) ;
      #endif
      c4memcpy( result, (void *)&d, sizeof(double) ) ;
   }
#endif /* !defined( S4CLIENT ) && ( defined( S4MDX ) || defined( S4CLIPPER ) ) */


#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX )
   void t4strToTime( COLLATE4 *collate, char *result, const char *input, const int len, int *lenOut )
   {
      assert5( collate == 0 && lenOut != 0 ) ;  // should not be used.
      // input format is HH:MM:SS - just make each short entry into memcmp (fox) format
      DBTIME *time = (DBTIME *)result ;

      short val = (short)c4atol( input, 2 ) ;

      t4shortToFox( (char *)&time->hour, &val  ) ;
      val = (short)c4atol( input + 3, 2 ) ;
      t4shortToFox( (char *)&time->minute, &val  ) ;
      val = (short)c4atol( input + 6, 2 ) ;
      t4shortToFox( (char *)&time->second, &val  ) ;

      *lenOut = sizeof( DBTIME ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) && defined( S4WIN32 )
   void t4strToLongLong( COLLATE4 *collate, char *result, const char *input, const int len, int *lenOut )
   {
      assert5( collate == 0 && lenOut != 0 ) ;  // should not be used.

      LONGLONG *lngLng = (LONGLONG *)result ;
      *lngLng = c4atoLongLong( input, len ) ;
      t4i8ToFox( (char *)lngLng, lngLng ) ;

      *lenOut = sizeof( LONGLONG ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) && defined( S4WIN32 )
   void t4dblToLongLong( char *result, const double input )
   {
      LONGLONG *lngLng = (LONGLONG *)result ;
      *lngLng = (LONGLONG)input ;
      t4i8ToFox( (char *)lngLng, lngLng ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX )
   void t4strToLog( COLLATE4 *collate, char *dest, const char *src, const int l, int *lenOut )
   {
      assert5( collate == 0 && lenOut != 0 ) ;  // should not be used.
      int pos = 0 ;

      for ( ; l != pos ; pos++ )
         switch( src[pos] )
         {
            case 't':
            case 'T':
            case 'y':
            case 'Y':
               dest[0] = 'T' ;
               return ;
            case 'f':
            case 'F':
            case 'n':
            case 'N':
               dest[0] = 'F' ;
               return ;
            default:
               break ;
         }

      dest[0] = 'F' ;

      *lenOut = 1 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_INDEX ) */



#if !defined( S4CLIENT ) && defined( S4CLIPPER )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void  t4strToDoub( COLLATE4 *dummyCollate, char *result, const char *input, const int dummy, int *dummyIntOut )
   {
      assert5( dummyCollate == 0 && dummyIntOut == 0 ) ;  // not used here, should be 0

      double d = c4atod( input, c4strlen( input )) ;
      c4memcpy( result, &d, sizeof(double) ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4CLIENT ) && defined( S4CLIPPER )
   void t4strToClip( COLLATE4 *dummyCollate, char *result, const char *input, const int len, int *dummyIntOut )
   {
      assert5( dummyCollate == 0 ) ;  // not used here, should be 0

      /* if input len != 0 then it is used to position value to right-justify */

      int iLen = c4strlen( input ) ;
      int dLen = len - iLen ;

      if ( dLen > 0 )
      {
         // AS 06/08/00 - allow formatting of key based on decimal input...put decimals after...
         if ( dummyIntOut != 0 )
         {
            if ( (*dummyIntOut) != 0 )  // there are decimals
            {
               int inputNumDec = 0 ;    // # of decimals in input key...
               Bool5 foundDecimal = 0 ;
               for ( int inputIndex = 0 ; inputIndex < iLen ; inputIndex++ )
               {
                  if ( input[inputIndex] == '.' )  // found decimal point
                  {
                     foundDecimal = 1 ;
                     int numDecimalsInInput = iLen - foundDecimal - 1 ;  // subtract -1 for '.' point
                     if ( numDecimalsInInput < (*dummyIntOut) )  // insufficient # of decimals in input, append 0's...
                     {
                        int numZerosToAdd = (*dummyIntOut) - numDecimalsInInput ;
                        if ( dLen >= numZerosToAdd )  // only bother if we have room to insert all the decimals required
                        {
                           dLen -= numZerosToAdd ;  // reduce the number of blanks to pre-append
                           c4memset( result + dLen + iLen, '0', numZerosToAdd ) ;
                        }
                     }
                     break ;
                  }
               }
               if ( foundDecimal == 0 )  // didn't find, so append if possible
               {
                  int decimalsToAppend = (*dummyIntOut) ;
                  if ( dLen >= decimalsToAppend + 1 )  // only bother if there is room to fully insert the decimals
                  {
                     dLen -= ( decimalsToAppend + 1 ) ;  // reduce the number of blanks to pre-append
                     result[dLen + iLen] = '.' ;
                     c4memset( result + dLen + iLen + 1, '0', decimalsToAppend ) ;
                  }
               }
            }
         }

         c4memset( result, ' ', dLen ) ;
         c4memcpy( result + dLen, input, iLen ) ;
      }
      else
         c4memcpy( result, input, iLen ) ;

      c4clip( result, c4strlen( result ), dLen ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if !defined( S4CLIENT ) && defined( S4CLIPPER )
   void  t4dateDoubToStr( char *result, const double d )
   {
      long  l ;

      l = (long) d ;
      date4assign( result, l ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4CLIPPER ) */



#if defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT )
   void t4intToFox( char *result, const long *val )
   {
      int isPositive ;

      #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
         long tempLong ;
         memcpy( &tempLong, (void*)val, sizeof(long) ) ; /* LY 00/11/09 : cast to void* for IA-64 */
         isPositive = tempLong > 0 ;
      #else
         isPositive = *val > 0 ;
      #endif
      *((long *)result) = x4reverseLong( val ) ;
      if ( isPositive )
         result[0] += (unsigned)0x80 ;
      else /* negative */
         result[0] -= (unsigned)0x80 ;
   }
#endif /* defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT ) */



#if defined( S4CLIENT ) || ( defined( S4FOX ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) ) )
   void t4unsignedIntToFox( char *result, const unsigned long *val )
   {
      *((long *)result) = x4reverseLong( val ) ;

      /* for unsigned, just don't do the negative value shift and the results are correct...
         if ( isPositive )
            result[0] += (unsigned)0x80 ;
         else
            result[0] -= (unsigned)0x80 ;
      */
   }
#endif



#if defined( S4CLIENT ) || ( defined( S4FOX ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) ) )
   void t4foxToUnsignedInt( char *result, const unsigned long *val )
   {
      *((long *)result) = x4reverseLong( val ) ;
   }
#endif



#if !defined( S4CLIENT ) && defined( S4FOX )
   void t4shortToFox( char *result, const short *val )
   {
      int isPositive ;

      isPositive = *val > 0 ;
      *((short *)result) = x4reverseShort( val ) ;
      if ( isPositive )
         result[0] += (unsigned)0x80 ;
      else /* negative */
         result[0] -= (unsigned)0x80 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */


#if ( ( defined( S4FOX ) || defined( OLEDB5BUILD ) ) || defined( S4CLIENT ) ) && !defined( S4NO_LONGLONG )
   void t4foxToI8( char *result, const char *fromPtr )
   {
      Bool5 isPositive =  ( fromPtr[0] & ( unsigned )0x80 ) != 0 ;

      *((LONGLONG *)result) = x4reverseLongLong( (const LONGLONG *)fromPtr ) ;
      if ( isPositive )
         result[7] -= (unsigned)0x80 ;
      else /* negative */
         result[7] += (unsigned)0x80 ;
   }
#endif /* ( defined( S4FOX ) || defined( OLEDB5BUILD ) ) || defined( S4CLIENT ) ) && defined( S4WIN32 ) */



#if !defined( S4NO_LONGLONG) && ( defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT ) )  // CS 2000/06/01, BCR 2000/08/18
   void t4i8ToFox( char *result, const LONGLONG *val )
   {
      int isPositive ;

      isPositive = *val > 0 ;
      #ifdef S4BYTE_SWAP
         strncpy( result, (const char *) val, 8 ) ;
      #else
         *((LONGLONG *)result) = x4reverseLongLong( val ) ;
      #endif
      if ( isPositive )
         result[0] += (unsigned)0x80 ;
      else /* negative */
         result[0] -= (unsigned)0x80 ;
   }
#endif //S4NO_LONGLONG && S4WIN32 && ( S4FOX || OLEDB5BUILD || S4CLIENT )



#if !defined( S4CLIENT ) && defined( S4FOX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4dtstrToFox( COLLATE4 *collate, char *result, const char *inputPtr, const int inputPtrLen, int *lenOut )
   {
      assert5( ( collate == 0 || collate->collateType == collate4machineByteOrder ) && lenOut != 0 ) ;  // should not be used.
      t4dblToFox( result, (double) date4long( inputPtr ) ) ;
      *lenOut = inputPtrLen ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4CLIENT ) && defined( S4FOX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4dtstrToDbDate( COLLATE4 *collate, char *result, const char *inputPtr, const int inputPtrLen, int *lenOut )
   {
      assert5( collate == 0 && lenOut != 0 ) ;  // should not be used.
      /* must convert from 'YYYYMMDD' to DBDATE format fox */
      DBDATE dbDate ;
      dbDate.year = c4atoi( inputPtr, 4 ) ;
      dbDate.month = c4atoi( inputPtr + 4, 2 ) ;
      dbDate.day = c4atoi( inputPtr + 6, 2 ) ;

      t4dbDateToFox( result, &dbDate ) ;
      *lenOut = sizeof( DBDATE ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4CLIENT ) && defined( S4FOX )
   static void dbDate4assignLow( DBDATE *datePtr, const long ldate )
   {
      /* Converts from a Julian day to the DBDATE format. */
      long totDays ;
      int  iTemp, year, nDays, maxDaysInYear, month, day ;

      c4memset( datePtr, 0, sizeof( DBDATE ) ) ;

      if ( ldate <= 0 )  // means just use a blank date
         return ;

      totDays = ldate - JULIAN4ADJUSTMENT ;
      iTemp = (int)( (double)totDays / 365.2425 ) ;
      year = iTemp + 1 ;
      nDays = (int)( totDays - c4ytoj( year ) ) ;
      if ( nDays <= 0 )
      {
         year-- ;
         nDays = (int)( totDays - c4ytoj( year ) ) ;
      }

      if ( (( year % 4 == 0 ) && ( year % 100 )) || ( year % 400 == 0 ) )
         maxDaysInYear = 366 ;
      else
         maxDaysInYear = 365 ;

      if ( nDays > maxDaysInYear )
      {
         year++ ;
         nDays -= maxDaysInYear ;
      }

      c4monDy( year, nDays, &month, &day ) ;

      datePtr->year = year ;
      datePtr->month = month ;
      datePtr->day = day ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4CLIENT ) && defined( S4FOX )
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   void t4dblToDbDate( char *result, const double d )
   {
      // must convert from a julian date value as a double to a DBDATE value
      dbDate4assignLow( (DBDATE *)result, (long)d ) ;
      t4dbDateToFox( result, (DBDATE *)result ) ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



#if !defined( S4CLIENT ) && defined( S4FOX )
   void t4strToFox( COLLATE4 *collate, char *result, const char *inputPtr, const int inputPtrLen, int *lenOut )
   {
      // assert5( collate == 0 && lenOut != 0 ) ;  // should not be used.
      t4dblToFox( result, c4atod( inputPtr, inputPtrLen ) ) ;
      // AS 06/26/00 - output is always in double format, so len must always be 8, not input len which may be anything since a string (t4field.c)
      // *lenOut = inputPtrLen ;
      *lenOut = 8 ;
   }
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */



void t4dbDateToFox( char *result, const DBDATE *val )
{
   DBDATE *dbDateResult = (DBDATE *)result ;

   dbDateResult->year = x4reverseShort( &val->year ) ;
   dbDateResult->month = x4reverseShort( &val->month ) ;
   dbDateResult->day = x4reverseShort( &val->day ) ;
}



void t4foxToDbDate( char *result, const char *valIn )
{
   DBDATE *dbDateResult = (DBDATE *)result ;
   DBDATE *val = (DBDATE *)valIn ;

   dbDateResult->year = x4reverseShort( &val->year ) ;
   dbDateResult->month = x4reverseShort( &val->month ) ;
   dbDateResult->day = x4reverseShort( &val->day ) ;
}



void t4foxToDbTime( char *result, const char *valIn )
{
   DBTIME *dbTimeResult = (DBTIME *)result ;
   DBTIME *val = (DBTIME *)valIn ;

   dbTimeResult->hour = x4reverseShort( &val->hour ) ;
   dbTimeResult->minute = x4reverseShort( &val->minute ) ;
   dbTimeResult->second = x4reverseShort( &val->second ) ;
}



void t4foxToDbTimeStamp( char *result, const char *valIn )
{
   DBTIMESTAMP *dbTimeStampResult = (DBTIMESTAMP *)result ;
   DBTIMESTAMP *val = (DBTIMESTAMP *)valIn ;

   dbTimeStampResult->year = x4reverseShort( &val->year ) ;
   dbTimeStampResult->month = x4reverseShort( &val->month ) ;
   dbTimeStampResult->day = x4reverseShort( &val->day ) ;
   dbTimeStampResult->hour = x4reverseShort( &val->hour ) ;
   dbTimeStampResult->minute = x4reverseShort( &val->minute ) ;
   dbTimeStampResult->second = x4reverseShort( &val->second ) ;
   dbTimeStampResult->fraction = x4reverseShort( &val->fraction ) ;
}



void t4dbTimeToFox( char *result, const DBTIME *val )
{
   DBTIME *dbTimeResult = (DBTIME *)result ;

   dbTimeResult->hour = x4reverseShort( &val->hour ) ;
   dbTimeResult->minute = x4reverseShort( &val->minute ) ;
   dbTimeResult->second = x4reverseShort( &val->second ) ;
}



void t4dbTimeStampToFox( char *result, const DBTIMESTAMP *val )
{
   DBTIMESTAMP *dbTimeStampResult = (DBTIMESTAMP *)result ;

   dbTimeStampResult->year = x4reverseShort( &val->year ) ;
   dbTimeStampResult->month = x4reverseShort( &val->month ) ;
   dbTimeStampResult->day = x4reverseShort( &val->day ) ;
   dbTimeStampResult->hour = x4reverseShort( &val->hour ) ;
   dbTimeStampResult->minute = x4reverseShort( &val->minute ) ;
   dbTimeStampResult->second = x4reverseShort( &val->second ) ;
   dbTimeStampResult->fraction = x4reverseShort( &val->fraction ) ;
}



void t4strToDbTimeStamp( COLLATE4 *collate, char *toPtr, const char *fromPtr, const int numLen, int *lenOut )
{
   /* converts from CCYYMMDDHH:MM:SS.FFFFFFFFF to ascending fox index format... */
   /*               01234567890123456789012345 */
   assert5( collate != 0 && toPtr != 0 && fromPtr != 0 && numLen >= 16 ) ;

   DBTIMESTAMP dbTimeStampResult ;

   dbTimeStampResult.year = (short)c4atol( fromPtr, 4 ) ;
   dbTimeStampResult.month = ( fromPtr[4] - '0' ) * 10 + ( fromPtr[5] - '0' ) ;
   dbTimeStampResult.day = ( fromPtr[6] - '0' ) * 10 + ( fromPtr[7] - '0' ) ;

   /* HOURS */
   dbTimeStampResult.hour = ( fromPtr[8] - '0' ) * 10 + ( fromPtr[9] - '0' ) ;

   /*MINUTES */
   dbTimeStampResult.minute = ( fromPtr[11] - '0' ) * 10 + ( fromPtr[12] - '0' ) ;

   /*SECONDS */
   dbTimeStampResult.second = ( fromPtr[14] - '0' ) * 10 + ( fromPtr[15] - '0' ) ;

   /*MILLESECONDS */
   dbTimeStampResult.fraction = 0 ;
   if ( numLen > 17 )  // there is a fractional component...
      dbTimeStampResult.fraction = c4atol( fromPtr + 17, numLen - 17 ) ;

   t4dbTimeStampToFox( toPtr, &dbTimeStampResult ) ;

   *lenOut = sizeof( DBTIMESTAMP ) ;
}



#if defined( S4CLIENT ) || ( defined( S4FOX ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) ) )
   void t4curToFox( char *result, const CURRENCY4 *source )
   {
      char i ;
      int isPositive ;
      char buffer[8] ;

      #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
         short int tempShrt ;
         /* LY 00/11/09 : removed source[3] for IA-64 */
         memcpy( &tempShrt, (void*)(source+sizeof(short int)*3), sizeof(short int) ) ;
         isPositive = ( tempShrt >= 0 ) ;
      #else
         isPositive = ( (short int)(source->lo[3]) >= 0 ) ;
      #endif

      c4memcpy(buffer, source, 8 ) ;
      for ( i = 0 ; i < 8 ; i++ )
         result[i] = ( buffer[7-i] ) ;

      if ( isPositive )
         result[0] += (unsigned)0x80 ;
      else /* negative */
         result[0] -= (unsigned)0x80 ;
   }
#endif



// AS 05/26/99 only used for ole-db, avoid due to non-unix
#ifdef OLEDB5BUILD
   /*
   static int shift4one( unsigned char *byte, int carryAdd )
   {
      // returns 1 if a carry bit, else 0...
      int carry = 0 ;
      if ( byte[0] >= 0x80 )
         carry = 1 ;
      byte[0] = ((byte[0]) << 1) + carryAdd ;
      return carry ;
   }


   static void power4shift( unsigned char *val, int numShift )
   {
      if ( numShift == 0 )
         return ;

      int carry = shift4one( &val[0], 0 ) ;
      carry = shift4one( &val[1], carry ) ;
      carry = shift4one( &val[2], carry ) ;
      carry = shift4one( &val[3], carry ) ;
      carry = shift4one( &val[4], carry ) ;
      carry = shift4one( &val[5], carry ) ;

      // the last byte is a special case, only the last 4 bits are shifted
      unsigned char partialShift = val[6] & 0x0F ;
      partialShift = (partialShift << 1) + carry ;

      val[6] = (val[6] & 0xF0) + partialShift ;

      // recursive shift...
      power4shift( val, numShift-1 ) ;
   }



   static void power4decrease( unsigned char *val, int numDecrease )
   {
      // decrease the power by numDecreae (eg. 2 would make the value 1/4 of what previous 1 = 1/2, 3 = 1/8, etc)
      // does this by manipulating the power part of the double...

      // the power part is in byte 7 first 4 bits and byte 8 last 7 bits...
      // just subtract 'numDecrease' from the power number...

      // try the first 4 bits first...

      unsigned char lowerSubtractor = val[6] & 0xF0 ;

      throw Err5internal( 0 ) ;  // function does not work properly...

      if ( lowerSubtractor > 16 * numDecrease )  // just subtract
      {
         lowerSubtractor -= 16 * numDecrease ;
         val[6] = val[6] & 0x0F + lowerSubtractor ;
      }
      else // else must carry over from the next byte...
      {
         unsigned totalPower = (val[7] & 0x7F) ;
         totalPower = (totalPower << 8) + lowerSubtractor ;
         totalPower -= 16 * numDecrease ;
         val[7] = ( val[7] & 0x80 ) + (totalPower >> 8) ;
         val[6] = val[6] & 0x0F + totalPower & 0x00F0 ;

      }
   }
   */


   void t4foxToDateTime( long *result, const char *input )
   {
      double val ;

      /* AS 04/01/99

         The date-time value stored in the double uses every bit of precision available
         to it under the knowledge that the decimal number will never be too large.
         This actually results in an exceeding of the normal precision of the double.
         Usually the last few bits have no influence on a double number unless the number
         is quite small, etc.

         The format of the double is as in IBM PC Architecture:

           byte 1    byte 2    byte 3    byte 4    byte 5    byte 6    byte 7    byte 8
         (01234567)(01234567)(01234567)(01234567)(01234567)(01234567)(01234567)(01234567)

         byte 8 bit 0 controls the sign of the number
         byte 8 bits 1-7 and byte 7 bits 0-3 control the exponenent

      */

      t4foxToDbl( (char *)&val, input ) ;

      long date = (long)val ;

      // to bring the precision back in, shift the number over twice and decrease the power value by 2...
      // for now, this function is not 100% accurate, but we represent the index as complex
      // and make notes in our documentation, so ok...
      // power4shift( (unsigned char *)&val, 3 ) ;
      // power4decrease( (unsigned char *)&val, 4 ) ;

      val -= (double)date ;

      long time = (long)(val * 86400000.0) ;

      result[0] = date ;
      result[1] = time ;
   }
#endif /* OLEDB5BUILD */



#if defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT )
   // AS 09/14/00 - flags for fox compatibility - FoxPro appears to have an unusual conversion
   // algorithm which could not be deciphered for key creation.  This bitmap below indicates
   // all records which need to have their keys decremented by 1 to be fox compatible.

   static unsigned char flags4dateTimeFlags[] =
   {
       96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
        8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
        4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
       64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
        4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
        8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
       64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
        8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
       64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
        4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
       64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
        4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
        8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
        8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3,
      129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,
       96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,
       24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129,
      193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,
       48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,
       12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2,
      131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,
       96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,
       24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
        6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,
        6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131,
      129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,
       24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  12,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,
        2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131, 193,
      192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,  48,
       48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,  24,  12,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
       64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,   6,   3,
      131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 193, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
        4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193, 192,
       96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,  48,
       24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,   4,
        6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,   3, 131,
      193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,  64,  96,
       48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
       12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,  24,
       12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,   6,
        3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
       64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,  96,  48,
       48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  24,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,
        4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,
       96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,
       24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,
        6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6, 131,
      129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193, 192,
       96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,  48,  48,
       24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,
        6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131,
      129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,  96,
       96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,  48,  24,
       24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  48,  48,  24,  24,  12,  12,
        6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131,
      129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,
       96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,
       24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,  12,   6,
        6,   3, 131, 193, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6, 131, 129,
      193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,
       48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,
        8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,
        6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3, 131, 129,
      193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 193,  96,  96,
       48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,
       12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,
        3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129,
      192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,  96,  96,
       48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,  48,  24,  24,
       12,  12,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,  24,  12,   6,   6,
        3, 131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193,
      192,  96,  96,  48,  24,  24,  12,  12,   6,   2, 131, 129, 193, 192,  96,  48,
       48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,  48,  24,   8,
       12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,  12,   6,   6,
      131, 129, 193, 192,  96,  96,  48,  24,  24,  12,  12,   6,   3, 131, 129, 193,
      192,  96,  48,  48,  24,  24,  12,   4,   6,   3, 131, 129, 193,  96,  96,  48,
       48,  24,   8,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,  16,  24,  12,
       12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,  12,   6,   3,
      131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3, 131, 129, 192,
       96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 131, 193, 192,  96,  96,  48,
       24,  24,  12,  12,   6,   6, 131, 129, 193, 192,  96,  32,  48,  24,  24,  12,
       12,   6,   3, 131, 129, 193,  64,  96,  48,  48,  24,  24,  12,   6,   6,   3,
      131, 129, 192,  96,  96,  48,  48,  24,  12,  12,   6,   6,   3, 129, 193, 192,
        0,   0
   } ;

   static Bool5 flags4initialized = 0 ;

   static F4FLAG flags4dateTime ;

   void flags4dateTimeInit()
   {
      if ( flags4initialized == 1 )
         return ;
      flags4dateTime.codeBase = 0 ;   // only used for error purposes, we want the flags to be CODE4 independent
      flags4dateTime.flags = flags4dateTimeFlags ;
      flags4dateTime.numFlags = 24*60*60 ;
      flags4dateTime.isFlip = 0 ;
      flags4initialized = 1 ;
   }

   void t4dateTimeToFox( char *result, const long *input )
   {
      double date, time ;
      double val ;
      #ifdef S4DATA_ALIGN
         long tempLong ;
      #endif

      if( flags4initialized == 0 )
         flags4dateTimeInit() ;

      assert5( flags4initialized == 1 ) ;

      #ifdef S4DATA_ALIGN  /* LY 00/11/07 : for S4WIN64 */
         memcpy( &tempLong, (void*)input, sizeof(long) ) ;  /* LY 2001/02/21 */
         date = (double) tempLong ;
         memcpy( &tempLong, (void*)(input+sizeof(long)), sizeof(long) ) ;
         long extraTime = tempLong % 1000 ;
         long tmLong = tempLong - extraTime ;
      #else
         date = (double)input[0] ;
         // AS 09/14/00 - FoxPro rounds up to the nearest second...
         long extraTime = input[1] % 1000 ;
         long tmLong = input[1] - extraTime ;
      #endif
      if ( extraTime >= 500 ) // round up
      tmLong += 1000 ;

      time = (double)tmLong ;

      val = date + ( time / 86400000.0 ) ;   /* 86400000 is the # of milliseconds in a day */

      // AS 09/14/00 - need to do the flag check to see if we must decrement the value to compensate for Fox compatibility conversion
      int isSet = f4flagIsSet( &flags4dateTime, tmLong / 1000 + 1 ) ;
      if ( isSet )  // must decrement the first byte...
      {
         unsigned char *valPtr = (unsigned char *)&val ;
         if ( valPtr[0] == 0 )  // must borrow from previous byte
         {
            valPtr[1]-- ;
            valPtr[0] = 0xFF ;
         }
         else
            valPtr[0]-- ;
      }

      t4dblToFox( result, val ) ;
   }
#endif /* defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT ) */



#if ( defined( S4FOX ) || defined( S4CLIENT ) )
   void t4dblToCurFox( char *result, const double doub )
   {
      CURRENCY4 hold ;

      t4dblToCur( (char *)&hold, doub ) ;
      t4curToFox( result, &hold ) ;
   }
#endif /* ( defined( S4FOX ) || defined( S4CLIENT ) ) */



#if defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT )
   void S4FUNCTION t4dblToFoxExport( char *result, const double doub )
   {
      /* CS 2000/08/09 This function is different than t4dblToFox in that
         it is declared as extern "C" in d4declar.h so that is can be called
         by 'C' programs (e.g. d4low.c).
      */
      t4dblToFox( result, doub ) ;
   }



   void t4dblToFox( char *result, const double doub )
   {
      char i ;
      int isPositive ;

      isPositive = ( doub >= 0 ) ;

      #ifdef S4BYTEORDER_3210
         if ( isPositive )
         {
            memcpy( (char *)result, (char *)&doub, 8 ) ;
            result[0] += (unsigned)0x80 ;
         }
         else /* negative */
         {
            for ( i = 0 ; i < 8 ; i++ )
               result[i] = (char) (~(*( (unsigned char *)&doub + i ))) ;
         }
      #else
         #ifdef S4BYTEORDER_2301
            memcpy( (char *)result, ((char *)&doub) + 4, 4 ) ;
            memcpy( ((char *)result) + 4, ((char *)&doub), 4 ) ;
            memcpy( (void *)&doub, result, sizeof(double) ) ;
         #endif
         if ( isPositive )
         {
            for ( i = 0 ; i < 8 ; i++ )
               result[i] = *( (char *)&doub + 7 - i ) ;
            result[0] += (unsigned)0x80 ;
         }
         else /* negative */
            for ( i = 0 ; i < 8 ; i++ )
               result[i] = (char) (~(*( (unsigned char *)&doub + 7 - i ))) ;
      #endif
   }
#endif /* defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT ) */



// AS 05/25/99 --> only used with OLE-DB, else not... avoid problems with UNIX
#ifdef OLEDB5BUILD
   void t4foxToDbl( char *result, const char *input )
   {
      #ifdef S4BYTEORDER_3210
         throw Err5internal( 0 ) ;  // not coded
      #endif
      #ifdef S4BYTEORDER_2301
         throw Err5internal( 0 ) ;  // not coded
      #endif

      char i ;
      int isPositive = ( ( input[0] & 0x80 ) != 0 )  ;

      if ( isPositive )
      {
         result[7] = input[0] & 0x7F ;
         for ( i = 0 ; i < 7 ; i++ )
            result[i] = *( input + 7 - i ) ;
      }
      else // negative
         for ( i = 0 ; i < 8 ; i++ )
            result[i] = ( char ) ( ~( *( ( unsigned char * )input + 7 - i ) ) ) ;
   }
#endif /* OLEDB5BUILD */



#if defined( S4MDX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT )
void c4bcdFromA( COLLATE4 *dummyCollate, char *result, const char *inputPtr, const int inputPtrLen, int *dummyIntOut )
{
   assert5( dummyCollate == 0 && dummyIntOut == 0 ) ;  // not used here, should be 0
   char *ptr ;
   unsigned len ;

   c4memset( result, 0, sizeof(C4BCD) ) ;

   int lastPos = inputPtrLen - 1 ;
   int pos = 0 ;
   for ( ; pos <= lastPos; pos++ )
   {
      if ( inputPtr[pos] != ' ' )
         break ;
   }

   if ( pos <= lastPos )
   {
      if ( inputPtr[pos] == '-' )
      {
         ((C4BCD *)result)->digitInfo=(unsigned char)((int)((C4BCD *)result)->digitInfo | 0x80) ;
         pos++ ;
      }
      else
      {
         if ( inputPtr[pos] == '+' )  pos++ ;
      }
   }

   for ( ; pos <= lastPos; pos++ )
   {
      if ( inputPtr[pos] != ' ' && inputPtr[pos] != '0' )
         break ;
   }

   int isBeforeDec = 1 ;

   ((C4BCD *)result)->sigDig = 0x34 ;
   if ( pos <= lastPos )
   {
      if ( inputPtr[pos] == '.' )
      {
         isBeforeDec = 0 ;
         pos++ ;
         for ( ; pos <= lastPos; pos++ )
         {
            if ( inputPtr[pos] != '0' )  break ;
            ((C4BCD *)result)->sigDig-- ;
         }
      }
   }

   ptr = (char *) ((C4BCD *)result)->bcd ; ;
   int zeroCount = 0 ;
   int n = 0 ;

   for ( ; pos <= lastPos ; pos++ )
   {
      if ( inputPtr[pos] >= '0' && inputPtr[pos] <= '9' )
      {
         if ( inputPtr[pos] == '0' )
            zeroCount++ ;
         else
            zeroCount = 0 ;
         if ( n >= 20 )  break ;
         if ( n & 1 )
            *ptr++ |= inputPtr[pos] - '0' ;
         else
            *ptr += (unsigned char)( (unsigned char)((unsigned char)inputPtr[pos] - (unsigned char)'0') << 4 ) ;
      }
      else
      {
         if ( inputPtr[pos] != '.'  ||  ! isBeforeDec )
            break ;

         isBeforeDec = 0 ;
         continue ;
      }
      if ( isBeforeDec )
         ((C4BCD *)result)->sigDig++ ;

      n++ ;
   }

   /* 'always one' bit filled  */
   ((C4BCD *)result)->digitInfo = (unsigned char)( ((C4BCD *)result)->digitInfo | 0x1 ) ;

   #ifdef E4MISC
      if ( n - zeroCount < 0 )
      {
         error4( 0, e4info, E95105 ) ;
         return ;
      }
   #endif

   len = n - zeroCount ;

   if (len > 31)
      len = 31 ;

   ((C4BCD *)result)->digitInfo = (unsigned char)( ((C4BCD *)result)->digitInfo | (len << 2) ) ;

   if ( len == 0 )
      ((C4BCD *)result)->digitInfo = (unsigned char)( ((C4BCD *)result)->digitInfo & 0x7F ) ;
}
#else
void c4bcdFromA( char *junk1, const char *junk2, const int junk3 )
{
   error4(0,e4notSupported, 0L);
}



#endif /* defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT ) */
