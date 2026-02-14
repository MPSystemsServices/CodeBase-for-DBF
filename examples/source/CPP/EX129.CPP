//ex129.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

typedef struct myStructSt
{
   char id[6] ;
   int checkSum ;
   char password[15] ;
} MY_STRUCT ;

void main( )
{
   Code4 cb ;
   File4seqWrite writePassFile ;
   File4 passFile ;
   MY_STRUCT person ;
   Str4max id( person.id, sizeof(person.id) ) ;
   Str4max pass( person.password, sizeof( person.password) ) ;
   char buffer[ 0x1400 ] ; // 5K... space for 200 structures
   int i ;

   cb.safety = 0 ;

   passFile.create( cb, "PASS.FIL" ) ;
   writePassFile.init( passFile, 0, buffer, sizeof( buffer ) ) ;

   for( i = 200 ; i ; i -- )
   {
      id.assignLong( i ) ;
      person.checkSum = i ;
      pass.assign( "PASSWORD" ) ;
      pass.add( id ) ;
      writePassFile.write( &person, sizeof( MY_STRUCT ) ) ;
   } // physically write only once.
   writePassFile.flush( ) ;

   File4seqRead readPassFile( passFile, 0, buffer, sizeof(buffer) );

   for( i = 200 ; i ; i -- )
   {
      // only one physical read occurs... the rest are in memory
      readPassFile.read( &person, sizeof( MY_STRUCT ) ) ;
      if( (int) id == person.checkSum )
         cout << "Valid Password: " << person.password << endl ;
   }
   passFile.close( ) ; // writePassFile and readPassFile are invalid now
   cb.initUndo( ) ;
}
