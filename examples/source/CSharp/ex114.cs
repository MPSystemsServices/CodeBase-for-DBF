using System;
using CodeBase;
 
namespace c4ap
{
   class ex114
   {
      static void displayTheRecord( Data4 d )
      {
         int numFields = d.numFields( ) ;
         short curField = 1 ;
 
         for( ; curField <= numFields; curField++ )
         {
            Field4memo genericField = new Field4memo( d, curField ) ;
 
            Console.WriteLine( "{0}\t", genericField.str( ) ) ;
         }
         Console.WriteLine( ) ;
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb  = new Code4( ) ;
         Data4 data = new Data4( cb, "DATA2" ) ;
         data.top( ) ;
         displayTheRecord( data ) ;
         cb.initUndo( ) ;
      }
   }
}