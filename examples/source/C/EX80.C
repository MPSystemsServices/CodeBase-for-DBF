/*ex80.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char dt[8] = "19901002" ;
   char  result[20] ;

   date4format( dt, result, "YY.MM.DD" ) ;   /* 'result' will contain "90.10.02"*/
   printf("%s\n", result ) ;
   date4format( dt, result, "CCYY.MM.DD" ) ; /* 'result' will contain "1990.10.02"*/
   printf("%s\n", result ) ;
   date4format( dt, result, "MM/DD/YY" ) ;   /* 'result' will contain "10/02/90"*/
   printf("%s\n", result ) ;
   date4format( dt, result, "MMM DD/CCYY" ) ;
   printf("%s\n", result) ;  /* outputs "Oct 02/1990"*/
}
