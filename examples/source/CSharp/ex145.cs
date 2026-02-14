using System;
using CodeBase;
 
namespace c4ap
{
   class ex145
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 info = new Data4( cb, "INFO" ) ;
 
         Relate4set TopMaster = new Relate4set( info ) ;
         // ... other code ...
 
         // This relation tree is no longer needed. Create a new one
         TopMaster.free( ) ;
         TopMaster.init( info ) ;
 
         // ... other code ...
         TopMaster.free( 1 ) ; // Automatically close all files in the relation
         cb.closeAll( ) ;     // close any remaining files
         cb.initUndo( ) ;
      }
   }
}