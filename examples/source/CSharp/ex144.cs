using System;
using CodeBase;
 
namespace c4ap
{
   class ex144
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
 
         Data4  employee = new Data4( cb, "EMPLOYEE" ) ;
         Data4  office = new Data4( cb, "OFFICE" ) ;
         Data4  building = new Data4( cb, "BUILDING" ) ;
 
         // Set up the tags.
         Tag4 officeNo = new Tag4( office, "OFF_NUM" ) ;
         Tag4 buildNo = new Tag4( building, "BUILD_NO" ) ;
 
         // Create the relations
         Relate4set master = new Relate4set( employee ) ;
 
         Relate4 toOffice = new Relate4( master, office, "EMPLOYEE->OFFICE_NO", officeNo ) ;
         Relate4 toBuilding = new Relate4( toOffice, building, "OFFICE->BUILD_NO", buildNo ) ;
 
         // Go to employee, at record 2
         employee.go( 2 ) ;
 
         // Lock the data files and their index files.
         master.lockAdd( ) ;
         cb.lockGroup( ) ;
 
         // This call causes the corresponding records in data files "OFFICE" and
         // "BUILDING" to be looked up.
         master.doAll( ) ;
 
         // Go to office, at record 3
         office.go( 3 ) ;
 
         // This call causes the building record to be looked up from the office
         toBuilding.doOne( ) ;
 
         //  ..  and so on
 
         cb.initUndo( ) ;
      }
   }
}