//ex37.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

static FIELD4INFO fieldArray[ ] =
{
   { "NAME_FIELD", 'C', 20, 0 },
   { "AGE_FIELD",  'N',  3, 0 },
   { "BIRTH_DATE", 'D',  8, 0 },
   { 0,0,0,0 }
} ;

void main( )
{
   Code4 cb ;
   Data4 data, secondFile ;

   data.create( cb, "FIRSTDBF", fieldArray ) ;
   cb.exitTest( ) ;

   // initialize fields object with the fields of FIRSTDBF.DBF
   Field4info fields( data ) ;
   // add a new field
   fields.add( "NEW_FLD", 'C', 20 ) ;

   cb.safety = 0 ;       // overwrite the file if it exists
   secondFile.create( cb,"NEWDBF", fields.fields( ) ) ;

   if( cb.errorCode )
      cout << "An error occurred, NEWDBF not created" << endl ;
   else
      cout << "Created successfully!" << endl ;

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
