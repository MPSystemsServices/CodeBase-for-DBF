//ex98.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000;

void main( )
{
   Code4  cb ;
   Data4  info( cb, "INFO" ) ;
   Field4 birthDate(info, "BIRTH_DATE"), amount(info, "AGE" ) ;
   Date4  today, bDate ;
   long   ageInDays, currentValue ;

   cb.exitTest( ) ;
   info.top( ) ;
   info.go( 1L ) ;

   today.today( ) ;
   bDate.assign(birthDate.str());
   ageInDays =  long( today ) - long( bDate ) ;

   cout << "Age in days:  " << ageInDays << endl;

   /* Assume field "AGE" is of type Numeric or
                                    Floating Point */
   currentValue =  long( amount ) ;

   cout << "Current value is:  " << currentValue << endl;

   cb.closeAll( );
   cb.initUndo( );
}
