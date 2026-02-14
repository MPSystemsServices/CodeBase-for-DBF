using System ;
using CodeBase ;

namespace c4ap
{
   class ex52
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         cb.accessMode = Code4.OPEN4DENY_RW ;

         // Open the file exclusively, default optimization is the same as if
         // Data4::optimize( Code4.OPT4EXCLUSIVE ) were called.
         Data4 info = new Data4( cb, "INFO" ) ;

         // open a shared file.
         cb.accessMode = Code4.OPEN4DENY_NONE ;
         Data4 extra = new Data4( cb, "DATA" ) ;

         extra.optimize( Code4.OPT4ALL ) ;  // read optimize the "DATA" file

         int rc = cb.optStart( ) ;  // Begin the memory optimizations.
         if ( rc == Code4.r4success )
            Console.WriteLine( "Memory optimiztion is implemented" ) ;
         else
            Console.WriteLine( "Either the S4OFF_OPTIMIZE switch is defined or "
               + "there is insufficient memory for optimization" ) ;

         // .... Some other code ....

         cb.closeAll( ) ;
         cb.initUndo( ) ;
      }
   }
}