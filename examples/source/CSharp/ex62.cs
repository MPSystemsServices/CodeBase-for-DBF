using System;
using CodeBase;

namespace c4ap
{
   class ex62
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         cb.accessMode = Code4.OPEN4DENY_RW ;
         Data4 data = new Data4( cb, "INFO" ) ;
 
         Console.WriteLine( "Reindexing {0}\nPlease Wait", data.alias ) ;
         int rc = data.reindex( ) ;
 
         if ( rc != 0 )
            Console.WriteLine( "\nReindex NOT successful.\n" ) ;
         else
            Console.WriteLine( "\nSuccessfully reindexed.\n" ) ;
 
         cb.initUndo( ) ;
      }
   }
}