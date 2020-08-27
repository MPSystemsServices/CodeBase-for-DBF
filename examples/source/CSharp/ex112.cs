using System;
using CodeBase;
 
namespace c4ap
{
   class ex112
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "DATA3" ) ;
         Field4memo comments = new Field4memo( data, "COMMENTS" ) ;
         Field4memo name = new Field4memo( data, "NAME" ) ;
 
         data.top( ) ;
         // display the null terminated contents of the memo field
         Console.WriteLine( "Memo field contents: {0}", comments.str( ) ) ;
 
         // display the non-null terminated contents of the NAME field.
         // this displays NAME plus any additional fields in the record buffer
         Console.WriteLine( "NAME field contents: {0}", name.str( ) ) ;
 
         cb.initUndo( ) ; // close all files and free memory
      }
   }
}