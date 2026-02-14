using System;
using CodeBase;

namespace c4ap
{
   class ex63
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 people = new Data4( cb, "people.dbf" ) ;
 
         /* Assume 'PEOPLE.DBF' has a production index file with tags
            PPL_NAME, PPL_AGE, PPL_BRTH */
         people.select( new Tag4( people, "PPL_NAME" ) ) ;
 
         if( people.seek( "fred" ) == Code4.r4success )
            Console.WriteLine( "fred is in record # {0}\n", people.recNo( ) ) ;
 
         if( people.seek( "HANK STEVENS" ) == Code4.r4success )
            Console.WriteLine( "HANK STEVENS is in record # {1}\n", people.recNo( ) ) ;
 
         people.select( new Tag4( people, "PPL_AGE" ) ) ;
         Field4 age = new Field4( people, "AGE" ) ;
 
         int rc = people.seek( 0.0 ) ;
 
         if( rc == Code4.r4success || rc == Code4.r4after )
            Console.WriteLine( "The youngest age is: {0}\n", (double) age.getDouble( ) ) ;
 
         // Seek using the char * version
         rc = people.seek( "0" ) ;
         if( rc == Code4.r4success || rc == Code4.r4after )
            Console.WriteLine( "The youngest age is: {0}\n", (double) age.getDouble( ) ) ;
 
         // Assume PPL_BRTH is a Date key expression
         people.select( new Tag4( people, "PPL_BRTH" )) ;
         string birth = "19600415" ;
 
         if( people.seek( birth ) == Code4.r4success ) // String in CCYYMMDD format
            Console.WriteLine( "Found: {0}\n", birth ) ;
 
         people.close( ) ;
         cb.initUndo( ) ;
      }
   }
}