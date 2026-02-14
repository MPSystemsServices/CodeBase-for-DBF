/*ex75.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 cb ;
   DATA4 *data;
   FIELD4 *birthField ;
   TAG4 *birthTag ;
   char myBirthDate[8] = "19690225" ;
   char today[8], result[18], tomorrow[8], yesterday[8] ;

   date4today( today ) ; /* set today equal to the system clock date.*/
   date4format( today, result, "MMMMMMMM DD, CCYY") ;
   printf( "Today is %s, %s\n", date4cdow( today ), result ) ;

   date4assign( tomorrow, date4long( today )+ 1L) ;
   date4format( tomorrow, result, "MMMMMMMM DD, CCYY") ;
   printf( "Tomorrow is %s, %s\n", date4cdow( tomorrow ), result ) ;

   date4assign( yesterday, date4long( tomorrow ) -2L );
   date4format( yesterday, result, "MMMMMMMM DD, CCYY") ;
   printf( "Yesterday was %s, %s\n", date4cdow( yesterday ), result ) ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   birthField = d4field( data, "BIRTH_DATE" ) ;
   birthTag = d4tag( data, "INF_BRTH" ) ;
   d4tagSelect( data, birthTag ) ;

   if( d4seek( data, myBirthDate ) == 0 )
      printf( "I'm in record %d\n", d4recNo( data )) ;

   /* change all birthdate fields to my birth date.*/
   for( d4top( data ) ; !d4eof( data ) ; d4skip( data, 1 ) )
      f4assign( birthField, myBirthDate ) ;
   code4initUndo( &cb ) ;
}
