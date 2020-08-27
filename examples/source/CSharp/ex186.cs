using System;
using CodeBase;
 
namespace c4ap
{
   class ex186
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4( ) ;
         Data4 info = new Data4( settings, "INFO" ) ;

         // List the names of the tags in any production index file corresponding
         // to "INFO" 
 
         Console.WriteLine( "Production index file tags for data file: {0}\n", info.alias ) ;
 
         Tag4 tag = new Tag4( ) ; 
         for( tag.initFirst( info ); tag.isValid( ) != 0; tag.initNext( ))
            Console.WriteLine( "Tag name: {0}\n", tag.alias( ) ) ;
 
         settings.initUndo( ) ;
      }
   }
}