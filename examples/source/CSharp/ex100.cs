using System;
using CodeBase;
 
namespace c4ap
{
   class ex100
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4( ) ;
         Data4 info = new Data4( settings, "INFO" ) ;
         Data4 data = new Data4( settings, "DATA" ) ;
         settings.exitTest( ) ;
 
         Field4 infoName = new Field4( info, "NAME" ) ;
         Field4 dataLname = new Field4( data, "LNAME" ) ;
 
         info.lockAddAll( ) ;
         data.lockAddAll( ) ;
         settings.lockGroup( ) ;
         for( info.top( ) , data.top( ) ; info.eof( ) == 0 && data.eof( ) == 0 ;
               info.skip( ) ,data.skip( ) )
            dataLname.assignField( ref infoName ) ; // copy 'LNAME' into 'NAME'
 
         settings.closeAll( ) ;
         settings.initUndo( ) ;
      }
   }
}