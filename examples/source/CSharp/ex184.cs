using System;
using CodeBase;
 
namespace c4ap
{
   class ex184
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         cb.readOnly = 1 ;
         Data4 data = new Data4( cb, "INFO" ) ;
         cb.exitTest( ) ;
 
         // get the reference to the "INF_NAME" tag
         Tag4 tag = new Tag4( data, "INF_NAME" ) ;
 
         data.select( tag ) ; // select the "INF_NAME" ordering
         for( data.top( ) ; data.eof( ) == 0 ; data.skip( ) )
         {
            Field4 field = new Field4( data, "NAME" ) ;
            Console.WriteLine( "{0}\n", field.str( ) ) ; // List out the NAMES
         }
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}