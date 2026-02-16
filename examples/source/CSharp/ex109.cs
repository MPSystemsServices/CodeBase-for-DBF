using System;
using CodeBase;
 
namespace c4ap
{
	class ex109
	{
		[STAThread]
		static void Main(string[] args)
		{
			Code4 cb = new Code4( ) ;
			Data4 data = new Data4( cb, "INFO" ) ;
			Field4info fields = new Field4info( data ) ;

			if( data.numFields( ) == fields.numFields( ) )
				Console.WriteLine( "A copy has been made of the fields" ) ;
			else
				Console.WriteLine( "An error must have occurred." ) ;

			cb.initUndo( ) ;
		}
	}
}