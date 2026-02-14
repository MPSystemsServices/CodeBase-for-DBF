using System;
using CodeBase;
 
namespace c4ap
{
   class ex102
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         cb.readOnly = 1 ;
         Data4 data = new Data4( cb, "INFO" ) ;
 
         // Display all of the field names
         for( short i = 1 ; i <= data.numFields( ) ; i++ )
         {
            Field4 field = new Field4( data, i ) ;
            Console.WriteLine( field.name( ) ) ;
         }
 
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}