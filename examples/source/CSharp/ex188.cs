using System;
using CodeBase;
 
namespace c4ap
{
   class ex188
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
 
         // open the datafile but not the index file
         cb.autoOpen = 0 ;
         Data4 data = new Data4( cb, "INFO" ) ;
 
         Tag4info tags = new Tag4info( cb ) ;
         tags.add( "retired", "age", "age>55", 0, Code4.r4descending) ;
         tags.add( "inf_name", "name", "", Code4.r4uniqueContinue, 0 ) ;
 
         Index4 index = new Index4( ) ;
         cb.safety = 0 ; // overwrite an existing file
         index.create( ref data, "INFO2", ref tags ) ;
         if( cb.errorCode != 0 )
            Console.WriteLine( "An error occurred" ) ;
 
         cb.initUndo( ) ; // Tag4info destructor called
      }
   }
}