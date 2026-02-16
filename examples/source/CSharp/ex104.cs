using System;
using CodeBase;
 
namespace c4ap
{
	class ex104
	{
		static void displayField( Field4 field )
		{
			Console.WriteLine( "{0}   {1}\n", field.name( ), field.str( ) ) ;
		}
 
		[STAThread]
		static void Main(string[] args)
		{
			Code4 cb = new Code4( ) ;
			cb.readOnly = 1 ;
			Data4 data = new Data4( cb, "INFO" ) ;
			Field4 field = new Field4( data, "NAME" ) ;
			data.top( ) ;
			displayField( field ) ;
			cb.initUndo( ) ;
		}
	}
}