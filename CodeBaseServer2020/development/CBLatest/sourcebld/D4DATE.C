/* d4date.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

// AS 08/14/00 - moved defs to d4date.c for mdx odbc

static int monthTot[] =
    { 0, 0,  31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 } ;
         /* Jan Feb Mar  Apr  May  Jun        Jul  Aug  Sep  Oct  Nov  Dec
             31  28  31   30   31   30         31   31   30        31   30   31
         */

typedef struct
{
   char cdow[12] ;
}  DOW ;

typedef struct
{
   char cmonth[10] ;
} MONTH ;

static DOW dayOfWeek[] =
#ifndef S4LANGUAGE
   {
      { "\0          " },
      { "Sunday\0    " },
      { "Monday\0    " },
      { "Tuesday\0   " },
      { "Wednesday\0 " },
      { "Thursday\0  " },
      { "Friday\0    " },
      { "Saturday\0  " },
   } ;
#else
   #ifdef S4GERMAN
      {
         { "\0          " },
         { "Sonntag\0   " },
         { "Montag\0    " },
         { "Dienstag\0  " },
         { "Mittwoch\0  " },
         { "Donnerstag\0" },
         { "Freitag\0   " },
         { "Samstag\0   " },
      } ;
   #endif
   #ifdef S4FRENCH
      {
         { "\0          " },
         { "Dimanche\0  " },
         { "Lundi\0     " },
         { "Mardi\0     " },
         { "Mercredi\0  " },
         { "Jeudi\0     " },
         { "Vendredi\0  " },
         { "Samedi\0    " },
      } ;
   #endif
   #ifdef S4SWEDISH
      {
         { "\0          " },
         { "M†ndag\0    " },
         { "Tisdag\0    " },
         { "Onsdag\0    " },
         { "Torsdag\0   " },
         { "Fredag\0    " },
         { "L”rdag\0    " },
         { "S”ndag\0    " },
      } ;
   #endif
   #ifdef S4FINNISH
      {
         { "\0          " },
         { "Maanantai\0 " },
         { "Tiistai\0   " },
         { "Keskiviikko " },
         { "Torstai\0   " },
         { "Perjantai\0 " },
         { "Lauantai\0  " },
         { "Suununtai\0 " },
      } ;
   #endif
   #ifdef S4NORWEGIAN
      {
         { "\0          " },
         { "Mandag\0    " },
         { "Tirsdag\0   " },
         { "Onsdag\0    " },
         { "Torsdag\0   " },
         { "Fredag\0    " },
         { "L”rdag\0    " },
         { "S”ndag\0    " },
      } ;
   #endif
#endif

static MONTH monthOfYear[] =
#ifndef S4LANGUAGE
   {
      { "\0        " },
      { "January\0 " },
      { "February\0" },
      { "March\0   " },
      { "April\0   " },
      { "May\0     " },
      { "June\0    " },
      { "July\0    " },
      { "August\0  " },
      { "September" },
      { "October\0 " },
      { "November\0" },
      { "December\0" },
   } ;
#else
   #ifdef S4GERMAN
      {
         { "\0        " },
         { "Januar\0  " },
         { "Februar\0 " },
         { "M„rz\0    " },
         { "April\0   " },
         { "Mai\0     " },
         { "Juni\0    " },
         { "Juli\0    " },
         { "August\0  " },
         { "September" },
         { "Oktober\0 " },
         { "November\0" },
         { "Dezember\0" },
      } ;
   #endif
   #ifdef S4FRENCH
      {
         { "\0        " },
         { "Janvier\0 " },
         { "F‚vrier\0 " },
         { "Mars\0    " },
         { "Avril\0   " },
         { "Mai\0     " },
         { "Juin\0    " },
         { "Juillet\0 " },
         { "Ao–t\0    " },
         { "Septembre" },
         { "Octobre\0 " },
         { "Novembre\0" },
         { "D‚cembre\0" },
      } ;
   #endif
   #ifdef S4SWEDISH
      {
         { "\0        " },
         { "Januari\0 " },
         { "Februari\0" },
         { "Mars\0    " },
         { "April\0   " },
         { "Maj\0     " },
         { "Juni\0    " },
         { "Juli\0    " },
         { "Augusti\0 " },
         { "September" },
         { "Oktober\0 " },
         { "November\0" },
         { "December\0" },
      } ;
   #endif
   #ifdef S4FINNISH
      {
         { "\0        " },
         { "Tammikuu\0" },
         { "Helmikuu\0" },
         { "Maaliskuu" },
         { "Huhtikuu\0" },
         { "Toukokuu\0" },
         { "Kes„kuu\0 " },
         { "Hein„kuu\0" },
         { "Elokuu\0  " },
         { "Syyskuu\0 " },
         { "Lokakuu\0 " },
         { "Marraskuu" },
         { "Joulukuu\0" },
      } ;
   #endif
   #ifdef S4NORWEGIAN
      {
         { "\0        " },
         { "Januar\0  " },
         { "Februar\0 " },
         { "Mars\0    " },
         { "April\0   " },
         { "Mai\0     " },
         { "Juni\0    " },
         { "Juli\0    " },
         { "August \0 " },
         { "September" },
         { "Oktober\0 " },
         { "November\0" },
         { "Desember\0" },
      } ;
   #endif
#endif

#ifndef S4SERVER
   const char *S4FUNCTION code4dateFormat( CODE4 *c4 )
   {
      #ifdef E4PARM_HIGH
         if ( c4 == 0 )
         {
            error4( c4, e4parm_null, E96303 ) ;
            return 0 ;
         }
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( c4, 1, E96303 ) )
            return 0 ;
      #endif

      return c4->c4trans.trans.dateFormat ;
   }

   int S4FUNCTION code4dateFormatSet( CODE4 *c4, const char *str )
   {
      #ifdef S4CLIENT
         CONNECTION4 *connection ;
         CONNECTION4DATE_FORMAT_SET_INFO_IN *infoIn ;
         int rc ;
      #endif

      #ifdef E4PARM_HIGH
         if ( c4 == 0 || str == 0 )
            return error4( c4, e4parm_null, E96302 ) ;
         if ( c4strlen( str ) >= sizeof( c4->c4trans.trans.dateFormat ) )
            return error4( c4, e4parm, E96302 ) ;
      #endif

      #ifdef S4SERVER
         c4strcpy( c4->currentClient->trans.dateFormat, sizeof( c4->c4trans.trans.dateFormat ), str ) ;
      #else
         c4strcpy( c4->c4trans.trans.dateFormat, sizeof( c4->c4trans.trans.dateFormat ), str ) ;  // AS Dec 13/05 vs 5.0 fixes
         #ifdef S4CLIENT
            if ( c4->defaultServer.connected )
            {
               connection = &c4->defaultServer ;
               #ifdef E4ANALYZE
                  if ( connection == 0 )
                     return error4( c4, e4struct, E96302 ) ;
               #endif
               connection4assign( connection, CON4DATE_FORMAT, 0L, 0L ) ;
               connection4addData( connection, NULL, sizeof(CONNECTION4DATE_FORMAT_SET_INFO_IN), (void **)&infoIn ) ;
               memcpy( infoIn->dateFormat, &c4->c4trans.trans.dateFormat, sizeof( c4->c4trans.trans.dateFormat ) ) ;
               connection4sendMessage( connection ) ;
               rc = connection4receiveMessage( connection ) ;
               if ( rc < 0 )
                  return error4stack( c4, rc, E96302 ) ;
               rc = connection4status( connection ) ;
               if ( rc < 0 )
                  connection4error( connection, c4, rc, E96302 ) ;
               return rc ;
            }
         #endif
      #endif

      return 0 ;
   }
#endif

int S4FUNCTION date4isLeap( const char *date )
{
   int year ;

   year = date4year( date ) ;
   return ( ( ((year%4 == 0) && (year%100 != 0)) || (year%400 == 0 )) ?  1 : 0 ) ;
}

static int c4dayOfYear( const int year, const int month, const int day )
{
   /*  Returns */
   /*     >0   The day of the year starting from 1 */
   /*          Ex.    Jan 1, returns  1 */
   /*     -1   Illegal Date */
   int isLeap, monthDays ;

   // AS 06/21/00 it turns out that year 0 is really 1BC, and is not a leap year!
   isLeap =  ( ((year%4 == 0) && (year%100 != 0)) || (year%400 == 0) ) ?  1 : 0 ;

   monthDays = monthTot[ month+1 ] -  monthTot[ month] ;
   if ( month == 2 )  monthDays += isLeap ;

   if ( year  < 0  ||
        month < 1  ||  month > 12  ||
        day   < 1  ||  day   > monthDays )
        return( -1 ) ;        /* Illegal Date */

   if ( month <= 2 )  isLeap = 0 ;

   return(  monthTot[month] + day + isLeap ) ;
}

int S4FUNCTION c4monDy( const int year, const int days,  int *monthPtr,  int *dayPtr )
{
   /*  Given the year and the day of the year, returns the month and day of month. */
   int isLeap, i ;

   isLeap = ( ((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0) ) ?  1 : 0 ;
   if ( days <= 59 )
      isLeap = 0 ;

   for( i = 2; i <= 13; i++)
   {
      if ( days <= monthTot[i] + isLeap )
      {
         *monthPtr = --i ;
         if ( i <= 2)
            isLeap = 0 ;

         *dayPtr = days - monthTot[ i] - isLeap ;
         return 0 ;
      }
   }
   *dayPtr = 0 ;
   *monthPtr = 0 ;

   return -1 ;
}

long S4FUNCTION c4ytoj( const int y )
{
   int yr ;
   /*  Calculates the number of days to the year */
   /*  This calculation takes into account the fact that */
   /*     1)  Years divisible by 400 are always leap years. */
   /*     2)  Years divisible by 100 but not 400 are not leap years. */
   /*     3)  Otherwise, years divisible by four are leap years. */
   /*  Since we do not want to consider the current year, we will */
   /*  subtract the year by 1 before doing the calculation. */

   // AS 06/21/00 there is a problem for negative years.  Namely, the /4 adjustment,
   // to see this, year 4, yr = 3, yr/4 = 0,  year -2, yr = -3, yr/4 = 0  a range of 7 years both
   // have the same adjustment.  To recover from this, if yr < 0, we need to subtract a further '1' value
   yr = y - 1 ;
   return( yr*365L +  yr/4L - yr/100L + yr/400L - ( (yr < 0) ? 1 : 0 )  ) ;
}

int S4FUNCTION date4assignLow( char *datePtr, const long ldate, int isOle )
{
   /* Converts from a Julian day to the dbf file date format. */
   long totDays ;
   int  iTemp, year, nDays, nDaysInYear, month, day ;

   if ( ldate <= 0 )
   {
      c4memset( datePtr, isOle ? '0' : ' ',  8 ) ;
      return 0L ;
   }

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
      nDaysInYear = 366 ;
   else
      nDaysInYear = 365 ;

   if ( nDays > nDaysInYear )
   {
      year++ ;
      nDays -= nDaysInYear ;
   }

   #ifdef E4MISC
      if ( c4monDy( year, nDays, &month, &day ) < 0 )
         return error4( 0, e4result, E96301 ) ;
   #else
      c4monDy( year, nDays, &month, &day ) ;
   #endif

   c4ltoa45( (long)year, datePtr, -4 ) ;
   c4ltoa45( (long)month, datePtr + 4, -2 ) ;
   c4ltoa45( (long)day, datePtr + 6, -2 ) ;

   return 0 ;
}

S4CONST char *S4FUNCTION date4cdow( const char *datePtr )
{
   return dayOfWeek[date4dow(datePtr)].cdow ;
}

S4CONST char *S4FUNCTION date4cmonth( const char *datePtr )
{
   return monthOfYear[date4month( datePtr )].cmonth ;
}

/* inlined*/
/*int S4FUNCTION date4day( const char *datePtr )*/
/*{*/
/*   return (int)c4atol( datePtr + 6, 2 ) ;*/
/*}*/

int S4FUNCTION date4dow( const char *datePtr )
{
   long date ;
   date = date4long(datePtr) ;
   if ( date < 0 )
      return 0 ;
   return (int)( ( date + 1 ) % 7 ) + 1 ;
}

void S4FUNCTION date4format( const char *datePtr, char *result, char *picture )
{
   int rest, mNum ;
   unsigned int resultLen, length ;
   char *ptrEnd, *monthPtr, tChar ;

   resultLen = c4strlen( picture ) ;
   c4memset( result, ' ', resultLen ) ;

   c4upper( picture ) ;
   c4encode( result, datePtr, picture, "CCYYMMDD" ) ;

   ptrEnd = c4strchr( picture, 'M' ) ;
   if ( ptrEnd )
   {
      monthPtr = result+ (int)( ptrEnd - picture ) ;
      length = 0 ;
      while ( *(ptrEnd++) == 'M' )
         length++ ;

      if ( length > 2)
      {
         /* Convert from a numeric form to character format for month */
         if (!c4memcmp( datePtr+4, "  ", 2 ))   /* if blank month */
         {
            c4memset( monthPtr, ' ', length ) ;
            return ;
         }

         mNum = c4atoi( datePtr+4, 2) ;

         if ( mNum < 1)
            mNum = 1 ;
         if ( mNum > 12)
            mNum = 12 ;

         rest = length - 9 ;
         if (length > 9)
            length = 9 ;

         c4memcpy( monthPtr, monthOfYear[mNum].cmonth, length ) ;
         if (rest > 0)
            c4memset( monthPtr+length, (int) ' ', (size_t)rest ) ;

         tChar = monthOfYear[mNum].cmonth[length] ;
         if( tChar == '\0' || tChar == ' ' )
         {
            mNum = c4strlen(monthOfYear[mNum].cmonth) ;
            if ( (unsigned)mNum != length )
               monthPtr[mNum] = ' ' ;
         }
         #ifdef S4WINDOWS
            #ifndef S4WINCE
               OemToAnsi( result, result ) ;
            #endif
         #endif
      }
   }
}

double S4FUNCTION date4formatMdx( const char *datePtr )
{
   long ldate ;
   ldate = date4long(datePtr) ;
   if ( ldate == 0 )
      return (double) S4NULL_DATE ;  /* Blank or Null date */
   return (double)ldate ;
}

int S4FUNCTION date4formatMdx2( const char *datePtr, double *doubPtr )
{
   long ldate ;

   ldate = date4long(datePtr) ;
   if ( ldate == 0 )
      *doubPtr = (double)S4NULL_DATE ;  /* Blank or Null date */
   else
      *doubPtr = (double)ldate ;
   #ifdef S4BYTE_SWAP
      *doubPtr = x4reverseDouble(doubPtr) ;
   #endif

   return 0 ;
}

void S4FUNCTION date4init( char *datePtr, const char *dateData, char *picture )
{
   char *monthStart, monthData[10], buf[2] ;
   int yearCount, monthCount, dayCount, centuryCount, i, length ;
   long currentCentury ;
   /* dayCount = 5 ;
   monthCount = 3 ;
   yearCount = 1 ;
   centuryCount= -1 ; */
   dayCount = 8 ;
   monthCount = 6 ;
   yearCount = 4 ;
   centuryCount = 2 ;

   c4memset( datePtr, ' ', 8 ) ;

   c4upper( picture ) ;
   /* LY 2003/07/23 : if the picture contains single digits old algorithm fails
      ( "M/DD/YY" and "9/11/01" produces "2001/90/11" ) */
   //   for ( i=0; picture[i] != '\0'; i++ )
   for ( i = ( strlen( picture ) < strlen( dateData ) ? strlen( picture ) : strlen( dateData ) ) - 1 ; i >= 0 ; i-- )
   {
      if ( dateData[i] == 0 )  // reached end of input string - just stop, ignore rest of picture
         break ;

      switch( picture[i] )
      {
         case 'D':
            // if ( ++dayCount >= 8 )
            if ( --dayCount < 6  )
               break ;
            datePtr[dayCount] = dateData[i] ;
            break ;
         case 'M':
            // if ( ++monthCount >=6 )
            if ( --monthCount < 4 )
               break ;
            datePtr[monthCount] = dateData[i] ;
            break ;
         case 'Y':
            // if ( ++yearCount >= 4 )
            if ( --yearCount < 2 )
               break ;
            datePtr[yearCount] = dateData[i] ;
            break ;
         case 'C':
            // if ( ++centuryCount >= 2 )
            if ( --centuryCount < 0 )
               break ;
            datePtr[centuryCount] = dateData[i] ;
            break ;
         default:
            break ;
      }
   }

   if ( c4strcmp( datePtr, "        " ) == 0 )
      return ;

   // if ( centuryCount == -1 )
   if ( centuryCount == 2 )
   {
      /* check the century on the system clock if possible */
      #if defined(S4WINCE) || defined(S4WIN32)
         SYSTEMTIME st ;
         GetLocalTime(&st) ;
         currentCentury = st.wYear / 100 ;
      #elif defined(S4PALM)
         DateTimeType timeNow;
         TimSecondsToDateTime(TimGetSeconds(),&timeNow);
         currentCentury = timeNow.year / 100 ;
      #else
         time_t currentTime ;
         struct tm *brokenTime ;
         time( &currentTime ) ;
         brokenTime = localtime( &currentTime ) ;
         currentCentury = (1900 + brokenTime->tm_year) / 100 ;
      #endif
      c4ltoa45( currentCentury, datePtr, 2 ) ;
      /* memcpy( datePtr, "19", (size_t)2 ) ; */
   }
   // if ( yearCount ==  1 )
   if ( yearCount ==  4 )
      c4memcpy( datePtr + 2, "01", (size_t)2 ) ;
   // if ( monthCount == 3 )
   if ( monthCount == 6 )
      c4memcpy( datePtr + 4, "01", (size_t)2 ) ;
   // if ( dayCount == 5 )
   if ( dayCount == 8 )
      c4memcpy( datePtr + 6, "01", (size_t)2 ) ;

   // if ( monthCount >= 6 )
   if ( monthCount < 4 )
   {
      /* Convert the Month from Character Form to Date Format */
      monthStart = c4strchr( picture, 'M' ) ;

      // length = monthCount - 3 ;        /* Number of 'M' characters in picture */
      length = 6 - monthCount ;        /* Number of 'M' characters in picture */

      datePtr[4] = ' ';
      datePtr[5] = ' ';

      if ( length > 3 )
         length = 3 ;
      c4strncpy( monthData, sizeof( monthData ), dateData + (int)( monthStart - picture ), (size_t)length) ;  // AS Dec 13/05 vs 5.0 fixes
      while ( length > 0 )
         if ( monthData[length-1] == ' ' )
            length-- ;
         else
            break ;

      monthData[length] = '\0' ;

      c4lower( monthData ) ;
      buf[0] = monthData[0] ;
      buf[1] = 0 ;
      c4upper(buf) ;
      monthData[0] = buf[0] ;

      if ( length > 0 )
         for( i = 1 ; i <= 12; i++ )
         {
            if ( u4memcmp( monthOfYear[i].cmonth, monthData, (size_t)length ) == 0 )
            {
               c4ltoa45( (long) i, datePtr+4, 2 ) ;  /* Found Month Match */
               break ;
            }
         }
   }

   for ( i = 0 ; i < 8 ; i++ )
      if ( datePtr[i] == ' ' )
         datePtr[i] = '0' ;
}



long S4FUNCTION date4yymmddToJulianLong( const int year, const int month, const int day )
{
   int dayOfTheYear = c4dayOfYear( year, month, day ) ;
   if ( dayOfTheYear < 1 )    /* Illegal Date */
      return -1L ;

   return ( c4ytoj( year ) + dayOfTheYear + JULIAN4ADJUSTMENT ) ;
}



long S4FUNCTION date4long( const char *datePtr )
{
   /*  Returns: */
   /*    >0  -  Julian day */
   /*           That is the number of days since the date  Jan 1, 4713 BC */
   /*           Ex.  Jan 1, 1981 is  2444606 */
   /*     0  -  NULL Date (dbfDate is all blank) */
   /*    -1  -  Illegal Date */
   int year, month, day, i ;

   /* first verify that the input date is valid --> which returns a -1 */
   for ( i = 0 ; i < 8 ; i++ )
   {
      if ( ( datePtr[i] < '0' || datePtr[i] > '9' ) && datePtr[i] != ' ' )
         return -1 ;
   }

   year = c4atoi( datePtr, 4 ) ;
   if ( year == 0)
   {
      // AS 01/24/00 -- foxPro sometimes inserts a blank date as '00000000', so consider this as blank as well
      if ( c4memcmp( datePtr, "        ", 8 ) == 0 || c4memcmp( datePtr, "00000000", 8 ) == 0 )
         return  0 ;
   }

   month = c4atoi( datePtr + 4, 2 ) ;
   day = c4atoi( datePtr + 6, 2 ) ;

   return date4yymmddToJulianLong( year, month, day ) ;
}



void S4FUNCTION date4timeNow( char *timeData )
{
   #if defined(S4WINCE) || defined(S4WIN32)
      SYSTEMTIME st;
      GetLocalTime( &st );
      c4ltoa45( (long)st.wHour, timeData, -2) ;
      c4ltoa45( (long)st.wMinute, timeData + 3, -2) ;
      c4ltoa45( (long)st.wSecond, timeData + 6, -2) ;
   #elif defined(S4PALM)
      DateTimeType timeNow;
      TimSecondsToDateTime(TimGetSeconds(),&timeNow);
      c4ltoa45( (long)timeNow.hour, timeData, -2) ;
      c4ltoa45( (long)timeNow.minute, timeData + 3, -2) ;
      c4ltoa45( (long)timeNow.second, timeData + 6, -2) ;
   #else
      #ifdef S4UNIX_THREADS
         long timeVal ;
         struct tm result ;

         time( (time_t *)&timeVal) ;
         localtime_r( (time_t *)&timeVal, &result) ;

         c4ltoa45( (long)result.tm_hour, timeData, -2) ;
         c4ltoa45( (long)result.tm_min, timeData + 3, -2) ;
         c4ltoa45( (long)result.tm_sec, timeData + 6, -2) ;
      #else  /* !S4WINCE, !S4WIN32, !S4UNIX_THREADS */
         time_t timeVal ;
         struct tm *tmPtr ;

         time( &timeVal) ;
         tmPtr = localtime( &timeVal) ;

         c4ltoa45( (long)tmPtr->tm_hour, timeData, -2) ;
         c4ltoa45( (long)tmPtr->tm_min, timeData + 3, -2) ;
         c4ltoa45( (long)tmPtr->tm_sec, timeData + 6, -2) ;
      #endif
   #endif
   timeData[2] = ':' ;
   timeData[5] = ':' ;
}



void S4FUNCTION date4today( char *datePtr )
{
   #if defined(S4WINCE) || defined(S4WIN32)
      SYSTEMTIME st;
      GetLocalTime( &st );

      c4ltoa45( st.wYear, datePtr, -4 ) ;
      c4ltoa45( st.wMonth, datePtr + 4, -2 ) ;
      c4ltoa45( st.wDay, datePtr + 6, -2 ) ;
   #elif defined(S4PALM)
      DateTimeType timeNow;
      TimSecondsToDateTime(TimGetSeconds(),&timeNow);
      c4ltoa45( timeNow.year, datePtr, -4 ) ;
      c4ltoa45( timeNow.month, datePtr + 4, -2 ) ;
      c4ltoa45( timeNow.day, datePtr + 6, -2 ) ;
   #else
      #ifdef S4UNIX_THREADS
         time_t timeVal ;
         struct tm result ;

         time( &timeVal ) ;
         localtime_r( &timeVal, &result ) ;

         c4ltoa45( 1900L + result.tm_year, datePtr, -4 ) ;
         c4ltoa45( (long)result.tm_mon + 1, datePtr + 4, -2 ) ;
         c4ltoa45( (long)result.tm_mday, datePtr + 6, -2 ) ;
      #else  /* !S4WINCE, !S4WIN32, !S4UNIX_THREADS */
         time_t timeVal ;
         struct tm *tmPtr ;

         time( &timeVal ) ;
         tmPtr = localtime( &timeVal ) ;

         c4ltoa45( 1900L + tmPtr->tm_year, datePtr, -4 ) ;
         c4ltoa45( (long)tmPtr->tm_mon + 1, datePtr + 4, -2 ) ;
         c4ltoa45( (long)tmPtr->tm_mday, datePtr + 6, -2 ) ;
      #endif
   #endif
}



void date4timeNowFull( void *timeData )
{
   // returns full date and time, including time to the millisecond level.
   // output is in r4dateTime / r4dateTimeMilli format ((2 longs), first long is date, 2nd is time )
   #ifndef S4MACINTOSH  // LY Jul 16/04
      assert5port( "added autoTimestampField support" ) ;
   #endif
   // AS Mar 11/03 - support for new feature r4autoTimestamp
   #if defined(S4WINCE) || defined(S4WIN32)
      SYSTEMTIME st;
      GetSystemTime( &st );  // use Co-ordinated Universal Time in case machines are run accross time-zone boundaries.

      long *dtTime = (long *)timeData ;
       dtTime[0] = date4yymmddToJulianLong( st.wYear, st.wMonth, st.wDay ) ;
       dtTime[1] = st.wHour * 3600000 + st.wMinute * 60000 + st.wSecond * 1000 + st.wMilliseconds ;
   #else
      error4( 0, e4notSupported, E96303 ) ;
   #endif

}
