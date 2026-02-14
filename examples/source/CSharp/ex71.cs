using System;
using CodeBase;

namespace c4ap
{
	class ex71
	{
		static int zapLast( Data4 info, int toDelete )
		{
			Console.WriteLine( "{0} has {1} records.\n", info.alias, info.recCount( ) ) ;

			// Remove the last 'toDelete' records in the data file.
			// endRec parameter defaults to 1 Billion

			info.zap( info.recCount( ) - toDelete + 1, 1000000) ;

			Console.WriteLine( "\n{0} now has {1} records.\n", info.alias, info.recCount( ) ) ;

			return info.recCount( ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			Code4 cb = new Code4( ) ;
			Data4 data = new Data4( cb, "INFO" ) ;
			zapLast( data, 10 ) ;
			cb.initUndo( ) ;
		}
	}
}