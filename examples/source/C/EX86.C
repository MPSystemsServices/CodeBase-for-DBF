/*ex86.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *bdate ;
   char date1[8], date2[8] ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   bdate = d4field( data, "BIRTH_DATE" ) ;
   d4top( data ) ;

   strcpy( date1, f4str( bdate ) ) ; /* make a copy of the field's contents*/
   d4skip( data, 1 ) ;
   strcpy( date2, f4str( bdate ) ) ; /* make a copy of the field's contents*/

   if( date4year( date1 ) != date4year( date2 ) )
      printf("The people in the 1st and 2nd records were born in different years: %d, %d.\n"
                           , date4year( date1 ), date4year( date2 )) ;
   else
      printf("The people in the 1st and 2nd record were born in the same year %d.\n"
                           , date4year( date1 ) ) ;
    d4close( data ) ;
    code4initUndo( &cb ) ;
}
