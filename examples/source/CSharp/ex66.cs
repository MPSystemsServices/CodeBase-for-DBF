using System;
using CodeBase;

namespace c4ap
{
   class ex66
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "NAMES" ) ;
 
         // Skip to the last record in the file whose NAME field contains "John"
 
         cb.optStart( ) ;
 
         Field4 name = new Field4( data, "F_NAME" ) ;
 
         for( data.bottom( ) ;data.bof( ) == 0 ; data.skip( -1 ) )
         {
            if( name.str().Trim() == "John" ) // anything but -1 indicates a find
               break ;
         }
         if( data.bof( ) != 0 )
            Console.WriteLine( "John not located\n" ) ;
         else
            Console.WriteLine( "The last John is in record: {0}\n", data.recNo( ) ) ;
 
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}