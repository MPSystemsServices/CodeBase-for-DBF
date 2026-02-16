using System;
using CodeBase;

namespace c4ap
{
   class ex65
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "INFO" ) ; // automatically open data & index file.
         Tag4 nameTag = new Tag4( data, "INF_NAME" ) ;
         Tag4 firstTag = new Tag4( );
         firstTag.initFirst( data ) ;
 
         data.top();
         data.recall();
 
         data.select( nameTag ) ; // Select the 'INF_NAME'
         data.seek( "JONES" ) ;    // Seek using 'INF_NAME'
 
         data.select( firstTag ) ; // Select the 'AGE' tag which is the
         // first tag of the first open index
         data.seek( 32 ) ;       // Seek using selected tag 'AGE'
 
         data.select( ) ;        // Select record ordering
         data.seek( "ginger" ) ; // The seek uses the first tag of the first index
         // when no tag is selected, so the seek fails even
         // if "ginger" is in the data file
 
         data.top( ) ;          //Physical top of the data file
 
         cb.initUndo( ) ;   // close all files and free up Code4 memory
      }
   }
}