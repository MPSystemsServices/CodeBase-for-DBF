using System;
using CodeBase;

namespace c4ap
{
	class APPEND
	{
		[STAThread]
		static void Main(string[] args)
		{
			Code4 cb = new Code4();
			cb.optimize = Code4.OPT4ALL ;
			cb.optimizeWrite = Code4.OPT4ALL ;

			Data4 dataFrom = new Data4( cb, "from_dbf") ;
			Data4 dataTo = new Data4( cb, "to_dbf" ) ;
			
			cb.exitTest( ) ;

			Field4 infoFrom = new Field4( dataFrom, "NAME" ) ;
			Field4 infoTo = new Field4( dataTo, "FNAME" ) ;

			cb.optStart( ) ;

			cb.lockAttempts = 1 ;
			int rc1 = dataFrom.lockAll( ) ;
			int rc2 = dataTo.lockAll( ) ;

			if( rc1 != 0 || rc2 != 0 )
			{
				Console.WriteLine( "Locking Failed" ) ;
				cb.exit( ) ;
			}

			for( int rc = dataFrom.top( ); rc == 0; rc = dataFrom.skip( ) )
			{
				dataTo.appendStart( ) ;
				infoTo.assignField( ref infoFrom ) ;
				dataTo.append( ) ;
			}

			dataFrom.unlock( ) ;
			dataTo.unlock( ) ;

			cb.closeAll( ) ;
			cb.initUndo( ) ;
		}
	}
}
