using System;
using CodeBase;

namespace c4ap
{
   class ex67
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "DBF" ) ;
         Tag4 tag = new Tag4( data, "DBF_NAME" ) ; // A tag with '.NOT.DELETED()' filter
 
         data.lockAll( ) ;
         data.select( tag ) ;
         data.top( ) ; // Position to the first record that is not deleted.
         Console.WriteLine( "first record that is not deleted is {0}\n", data.recNo() ) ;
 
         data.deleteRec( ) ; // The current record no longer is located in the tag
 
         data.tagSync( ) ; // The change to the record is flushed, and the datafile is
         // positioned on a valid record.
 
         data.top( ) ;
         Console.WriteLine( "the new first record that is not deleted is {0}\n ", data.recNo() ) ;
         data.skip( ) ;
 
         //... some other code ...
 
         cb.initUndo( ) ;
      }
   }
}