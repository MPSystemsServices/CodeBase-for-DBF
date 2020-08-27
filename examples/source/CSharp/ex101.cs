using System;
using CodeBase;
 
namespace c4ap
{
   class ex101
   {
      static void displayFieldStats( Field4 f )
      {
         Data4 db = new Data4( f.data( ) ) ;
 
         Console.WriteLine( "-----------------------------------------------------" ) ;
         Console.WriteLine( "Database: {0} Field: {1}\nLength: {2}     Type: {3}\nDecimals: {4}"
            , db.alias, f.name( ), f.len( ), f.type( ), f.decimals( ) ) ;
         Console.WriteLine( "-----------------------------------------------------\n" ) ;
          
         return ;
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         cb.readOnly = 1 ;
         Data4 data = new Data4( cb, "INFO" ) ;
         Field4 field = new Field4( data, "NAME" ) ;
         displayFieldStats( field ) ;
         cb.initUndo( ) ;
      }
   }
}