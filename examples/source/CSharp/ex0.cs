using System ;
using CodeBase ;

namespace c4ap
{
   class ex0
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4() ;
         Data4 dataFile = new Data4() ;

         settings.autoOpen = 0 ;
         settings.memSizeBlock = 0x800 ; // 2048
         settings.memSizeBuffer = 0x2000 ; // 8192

         settings.errDefaultUnique = Code4.r4unique ;
         dataFile.open( ref settings, "INFO" ) ;

         // this is equivalent to calling Code4::exitTest( )
         if( settings.errorCode != 0 )  settings.exit( ) ;

         // ...

         settings.initUndo( ) ;
      }
   }
}