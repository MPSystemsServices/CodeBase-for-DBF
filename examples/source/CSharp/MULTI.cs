using System;
using CodeBase;

namespace c4ap
{
	class MULTI
	{
		static Code4 cb = new Code4() ;
		static Data4 dataNames = new Data4() ;
		static Field4 fieldName ;
		static Tag4 tagName = new Tag4() ;

		static void addRecord()
		{
			Console.WriteLine( "Enter New Record\n" ) ;
			string buf ;
			for(;;)
			{
				buf = Console.ReadLine() ;
				if (buf != "") break ;
			}

			dataNames.appendStart( ) ;
			fieldName.assign( buf ) ;
			dataNames.append( ) ;
			dataNames.unlock( ) ;
		}

		static void findRecord()
		{
			Console.WriteLine( "Enter Name to Find" ) ;
			string buf ;
			for(;;)
			{
				buf = Console.ReadLine() ;
				if (buf != "") break ;
			}

			dataNames.select( tagName ) ;
			dataNames.seek( buf ) ;
		}

		static void modifyRecord()
		{
			short oldLockAttempts = cb.lockAttempts ;
			cb.lockAttempts = 1 ; // Only make one lock attempt

			int rc = dataNames.lockRecord(dataNames.recNo( ) ) ;
			if(rc == Code4.r4locked)
				Console.WriteLine( "Record locked. Unable to Edit" ) ;
			else
			{
				Console.WriteLine( "Enter Replacement Record" ) ;
				string buf ;
				for(;;)
				{
					buf = Console.ReadLine() ;
					if (buf != "") break ;
				}

				fieldName.assign( buf ) ;
				dataNames.flush( ) ;
				dataNames.unlock( ) ;
			}
			cb.lockAttempts = oldLockAttempts ;
		}

		static void listData()
		{
			cb.optStart( ) ;
			dataNames.optimize( Code4.OPT4ALL ) ;
			dataNames.select( tagName ) ;

			for( dataNames.top( ); dataNames.eof( ) == 0; dataNames.skip( ) )
				Console.WriteLine("{0} {1}\n", dataNames.recNo( ), fieldName.str( ) );

			dataNames.optimize( Code4.OPT4OFF ) ;
			cb.optSuspend( ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			cb.accessMode = Code4.OPEN4DENY_NONE ;
			cb.readOnly = 0 ;
			cb.readLock = 0 ;
			cb.lockAttempts = Code4.WAIT4EVER ;
			cb.lockEnforce = 1 ;

			dataNames.open( ref cb, "names" ) ;
			cb.exitTest(  ) ;

			fieldName = new Field4( dataNames, "NAME" ) ;
			tagName.init( dataNames, "NAMENAME" ) ;

			dataNames.top( ) ;

			for(;;)
			{
				cb.errorCode = 0 ;

				Console.WriteLine( "Record #: {0}   Name: {1}", 
					dataNames.recNo( ), fieldName.str( ) ) ;

				Console.WriteLine( "Enter Command ('a','f','l','m' or 'x') " ) ;

				int command ;
				do
				{
					command = Console.Read() ;
				} while (command == 10) ;
				switch( command )
				{
					case 'a':
						addRecord( ) ;
						break ;
					case 'f':
						findRecord( ) ;
						break ;
					case 'l':
						listData( ) ;
						break ;
					case 'm':
						modifyRecord( ) ;
						break ;
					case 'x':
						cb.closeAll( ) ;
						cb.initUndo( ) ;
						cb.exit( ) ;
						break;
				}
			}
		}
	}
}