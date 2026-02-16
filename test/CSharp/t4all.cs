using System;
using CodeBase ;
namespace ConsoleApplication1
{
	/// <summary>
	/// Summary description for t4all1.
	/// </summary>
	class t4all
	{
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{
			//
			// TODO: Add code to start application here
			//
         Code4 cb ;
         Data4 data = new Data4(), data2 = new Data4();
         Tag4 tag = new Tag4() ;
         Index4 index ;
         Expr4 expr = new Expr4() ;
         Relate4set rel = new Relate4set() ;
         Relate4 relS = new Relate4() ;
         string strTemp ;
         Field4 fStr, fNum, fLog, fCur, fDT, fDouble ;
         Field4memo fMem ;
         int rc ;
         short sRc ;

         cb = new Code4() ;

         // CODE4 structure members

         sRc = cb.accessMode ;
         sRc = cb.autoOpen ;
         rc = cb.codePage ;
         rc = cb.collatingSequence ;
         cb.compatibility = 30 ;
         rc = cb.createTemp ;
         rc = cb.errCreate ;
         rc = cb.errDefaultUnique ;
         rc = cb.errExpr ;
         rc = cb.errFieldName ;
         rc = cb.errGo ;
         rc = cb.errOff ;
         rc = cb.errorCode ;
         rc = cb.errOpen ;
         rc = cb.errRelate ;
         rc = cb.errSkip ;
         rc = cb.errTagName ;
         rc = cb.fileFlush ;
         cb.hWnd = 0 ;
         rc = cb.lockAttempts ;
         rc = cb.lockAttemptsSingle ;
         cb.lockDelay = 10 ;
         rc = cb.lockEnforce ;
         rc = cb.log ;
         rc = cb.memExpandBlock ;
         rc = cb.memExpandData ;
         rc = cb.memExpandIndex ;
         rc = cb.memExpandLock ;
         rc = cb.memExpandTag ;
         cb.memSizeBlock = 1024 ;
         cb.memSizeBuffer = 32768 ;
         cb.memSizeMemo = 512 ;
         cb.memSizeMemoExpr = 1024 ;
         cb.memSizeSortBuffer = 4096 ;
         cb.memSizeSortPool = 0xF000 ;
         cb.memStartBlock = 10 ;
         cb.memStartData = 10 ;
         cb.memStartIndex = 10 ;
         sRc = cb.memStartLock ;
         rc = cb.memStartMax ;
         cb.memStartTag = 10 ;
         rc = cb.optimize ;
         rc = cb.optimizeWrite ;
         rc = cb.readLock ;
         rc = cb.readOnly ;
         rc = cb.safety ;
         rc = cb.singleOpen ;
         rc = cb.timeout ;
         cb.timeout = 100 ;

         cb.errOff = 1 ;
         cb.connect( "localhost", "23165", "", "", "" ) ;
         cb.errOff = 0 ;
         cb.errorCode = 0 ;

         cb.readOnly = 1 ;
         cb.compatibility = 30 ;
         if ( cb.indexExtension().ToUpper() == "NTX" )
            data.open( ref cb, "c:\\dbfs\\ntx\\student" );        
         else
            data.open( ref cb, "h:\\savetest\\student" );
         cb.readOnly = 0 ;
         expr.parse( data, new Field4(data, 1).name() );
         cb.calcCreate( expr, "FFF" ) ;
         cb.calcReset() ;

         cb.closeAll() ;

         cb.readOnly = 1 ;
         cb.compatibility = 30 ;
         if ( cb.indexExtension().ToUpper() == "NTX" )
            data.open( ref cb, "c:\\dbfs\\ntx\\student" ) ;
         else
            data.open( ref cb, "h:\\savetest\\student" ) ;
         cb.readOnly = 0 ;
         cb.data( "STUDENT" ) ;
         data.close() ;

         string datestr = cb.dateFormat ;
         cb.dateFormat = "MMM DD CCYY" ;

      // code4exit(); -- see end 

         cb.flushFiles() ;;

         string extstr = cb.indexExtension() ;

      // code4init(); -- see beginning
      // code4initUndo(); -- see end

         cb.lockGroup() ;

         cb.lockClear() ;

         cb.lockFileName() ;

         cb.lockItem() ;

         cb.lockNetworkId() ;

         cb.lockUserId() ;

         cb.logOpenOff() ;

         cb.safety = 0 ;
         cb.logCreate( "t4all.log", "t4all" ) ;

         cb.logFileName() ;

         cb.initUndo() ;
         cb.init() ;

         cb.errOff = 1 ;
         cb.connect( "localhost", "23165", "", "", "" ) ;
         cb.errOff = 0 ;
         cb.errorCode = 0 ;

         cb.logOpen( "t4all.log", "t4all" ) ;

         cb.optAll() ;

         cb.optStart() ;

         cb.optSuspend() ;

         cb.compatibility = 30 ;
         cb.safety = 0 ;

         Field4info fields = new Field4info( ref cb ) ;
         fields.add( "STR", Code4.r4str, 10, 0, Code4.r4null ) ;
         fields.add( "NUM", Code4.r4num, 4, 0, 0 ) ;
         fields.add( "LOG", Code4.r4log, 10, 0, 0 ) ;
         if ( cb.indexExtension().ToUpper() == "CDX" )
         {
            fields.add( "CUR", Code4.r4currency, 10, 0, 0 ) ;
            fields.add( "DOUBLE", Code4.r4double, 10, 0, 0 ) ;
            fields.add( "DT", Code4.r4dateTime, 10, 0, 0 ) ;
         }
         fields.add( "MEM", Code4.r4memo, 10, 0, 0 ) ;

         Tag4info tags = new Tag4info( cb ) ;
         tags.add( "STR", "STR", "", 0, 0 ) ;
         tags.add( "NUM", "NUM", "", Code4.r4uniqueContinue, 0 ) ;

         data.create( ref cb, "t4all", ref fields, ref tags);

         cb.tranStart() ;
         cb.tranCommit() ;

         cb.tranStart() ;
         cb.tranRollback() ;

         cb.tranStatus() ;

         data.close() ;

         cb.unlock() ;

         rc = cb.unlockAuto ;

         cb.unlockAuto = Code4.LOCK4ALL ;

         ///////////////////////////////////////////

         cb.compatibility = 30 ;
         cb.safety = 0 ;
         data.create( ref cb, "t4all", ref fields, ref tags );

         string newStr = data.alias ;

         data.alias = "newalias" ;

         data.appendStart( 0 );

         data.append() ;

         data.appendBlank() ;

         data.blank() ;

         data.eof() ;

         data.bottom() ;

         data.changed = 0 ;

         data.checkIndex() ;

         data.close() ;

      // d4create: see above

      // d4createCB: should be performed by d4create()

         cb.compatibility = 30 ;
         cb.safety = 0 ;
         data.create( ref cb, "t4all", ref fields ) ;

         data.appendBlank() ;
         data.deleteRec() ;

         data.deleted() ;

         data.eof() ;

         // d4field(data, 'str');

         // u4free(PVoid(d4fieldInfo(data)));

         data.fieldNumber( new Field4( data, 1).name() ) ;

         data.fileName() ;

         data.flush() ;

         data.freeBlocks() ;

         rc = data.appendBlank() ;
         data.go( 1 );

         data.goBof() ;

         data.goEof() ;

         data.index( "" ) ;

         data.lockRecord( 1 );

         data.lockAdd( 1 ) ;

         data.lockAddAll() ;

         data.lockAddAppend() ;

         data.lockAddFile() ;

         data.lockAll() ;

         data.lockAppend() ;

         data.lockFile() ;

         data.log = 0 ;

         rc = data.log ;

         data.memoCompress() ;

         data.numFields() ;

         data.close() ;
         cb.compatibility = 30 ;
         data.open( ref cb, "t4all" );

         data2.openClone( data ) ;
         data2.close() ;

         data.optimize( Code4.OPT4OFF ) ;

         data.optimizeWrite( Code4.OPT4OFF ) ;

         data.pack() ;

         double newDouble = data.position ;

         data.position = 0.5 ;

         data.recall() ;

         // d4record(data);

         data.recWidth() ;

         data.refresh() ;

         data.refreshRecord() ;

         data.reindex() ;

         data.remove() ;
         cb.compatibility = 30 ;
         cb.safety = 0 ;
         data.create( ref cb, "t4all", ref fields, ref tags ) ;

         data.select( new Tag4( data, "STR") );
         data.seek( "s" );

         data.seekNext( "s" );

         data.seek( "sssss", 2 ) ;

         data.seekNext( "sssss", 2 ) ;

         data.select( new Tag4(data, "NUM") );
         data.seek( 2.0 );

         data.seekNext( 2.0 );

         data.top() ;
         data.skip( 1 ) ;

      // d4tag: see d4seek();

         tag.initFirst( data ) ;

         tag.initNext() ;

         tag.initPrev() ;

      // d4tagSelect: see d4seek();

         tag.init( data, "" ) ;

         data.tagSync() ;

         data.top() ;

         data.unlock() ;

         data.appendBlank() ;
         data.appendBlank() ;
         data.write( 1 ) ;

         data.zap( 2, 99 ) ;

         //////////////////////////////////////////////

         strTemp = "" ;
         // date4assign(strTemp, date4long('19980909'));

         //date4cdow('19980909');

         //date4cmonth('19980909');

         //date4day('19980909');

         //date4format('19980909', strTemp, 'CCYYMMDD');

         //date4init(strTemp, '19980909', 'CCYYMMDD');

         //date4isLeap('19980909');

      //{date4long: see date4assign();}

         //date4month('19980909');

         //date4today(strTemp);

         //date4year('19980909');

         //

         cb.errOff = 1 ; 
      // don't display msgbox

         cb.throwError( -60, 90210 ) ;
         cb.errorCode = 0 ;

         cb.error( -60, 90210, "This is a test", "", "" ) ;
         cb.errorCode = 0 ;

         cb.exitTest() ;

         cb.errorFile( "t4all.err", 1 ) ;

         cb.errorSet( 0 ) ;

         cb.errorText( -60 ) ;

         cb.initUndo() ;
         cb.init() ;

         //
         cb.errOff = 1 ;
         cb.connect( "localhost", "23165", "", "", "" ) ;
         cb.errOff = 0 ;
         cb.errorCode = 0 ;

         cb.compatibility = 30;
         cb.safety = 0 ;
         data.create( ref cb, "t4all", ref fields, ref tags ) ;

         expr = new Expr4() ;
         expr.parse( data, "STR" ) ;

         expr.data() ;

         expr.len() ;

         expr.isNull() ;

         expr.source() ;

         expr.str() ;

         expr.free() ;

         expr.parse( data, "NUM" ) ;

         newDouble = (double)expr ;

         expr.free() ;

         expr.parse( data, "LOG" ) ;

         expr.isTrue() ;

         expr.free() ;

         //

         cb.closeAll() ;
         cb.compatibility = 30 ;
         cb.safety = 0 ;
         data.create( ref cb, "t4all", ref fields, ref tags ) ;

         fStr = new Field4( data, "STR" ) ;
         fNum = new Field4( data, "NUM" ) ;
         fLog = new Field4( data, "LOG" ) ;
         if ( cb.indexExtension().ToUpper() == "CDX" )
         {
            fCur = new Field4( data, "CUR" ) ;
            fDT = new Field4( data, "DT" ) ;
            fDouble = new Field4( data, "DOUBLE" ) ;
         }
         else
         {
            fCur = new Field4( data, 1 ) ;
            fDT = new Field4( data, 1 ) ;
            fDouble = new Field4( data, 1 ) ;
         }
         fMem = new Field4memo( data, "MEM" ) ;

         fStr.assign( "A" ) ;

         //f4assignChar(fLog, 65);

         if ( cb.indexExtension().ToUpper() == "CDX" )
         {
            fCur.assignCurrency( "8.75" ) ;

            fDT.assignDateTime( "1998090912:12:12" ) ;

            fDouble.assignDouble( 8.75 ) ;
         }
         
         fStr.assignField( ref fNum ) ;

         fNum.assignInt( 5 ) ;

         // f4assignLong(fNum, 12345678);

         if ( cb.indexExtension().ToUpper() == "CDX" )
            fStr.assignNull() ;

         fStr.assign( "aaaaa", 2 ) ;

         // f4blank(fStr);

         fLog.getChar() ;

         if ( cb.indexExtension().ToUpper() == "CDX" )
         {
            fCur.currency( 2 ) ;

            fDT.dateTime() ;
         }

         fDT.data() ;

         fNum.decimals() ;

         fNum.getInt() ;

         if ( cb.indexExtension().ToUpper() == "CDX" )
            fDouble.getDouble() ;

         fStr.len() ;

         // f4long(fNum);

         fMem.assign( "AAA" ) ;

         fMem.assign( "AAA", 2 ) ;

         fMem.free() ;

         fMem.len() ;

         fMem.str() ;

         fLog.name() ;

         fStr.isNull() ;

         if ( cb.indexExtension().ToUpper() == "CDX" )
            fCur.number() ;

         // f4ptr(fStr);

         fStr.str() ;

         fLog.isTrue() ;

         if ( cb.indexExtension().ToUpper() == "CDX" )
            fDouble.type() ;

         //

         cb.closeAll() ;

         cb.compatibility = 30 ;
         cb.safety = 0 ;
         data.create( ref cb, "t4all", ref fields, ref tags );

         // i4create(data, 't4all', @tags);

         data.close() ;
         cb.compatibility = 30 ;
         cb.autoOpen = 0 ;
         data.open( ref cb, "t4all" ) ;
         cb.autoOpen = 1 ;
         index = new Index4( data, "t4all" ) ;

         // i4createCB: should be called by i4create();

         index.fileName() ;

         index.reindex() ;

         index.tag( "STR" ) ;

         // i4tagAdd(index, @junkTags);

         // i4tagInfo(index);

         index.close() ;
         //

         cb.closeAll() ;
         cb.readOnly = 1 ;
         cb.compatibility = 30 ;
         if ( cb.indexExtension().ToUpper() == "NTX" )
         {
            data.open( ref cb, "c:\\dbfs\\ntx\\student" ) ;
            data2.open( ref cb, "c:\\dbfs\\ntx\\enroll" ) ;
         }
         else
         {
            data.open( ref cb, "h:\\savetest\\student" ) ;
            data2.open( ref cb, "h:\\savetest\\enroll" ) ;
         }
         cb.readOnly =  0 ;
         rel = new Relate4set() ;
         rel.init( data ) ;

         relS = new Relate4( rel, data2, "ID", new Tag4( data2, "STU_ID_TAG") );

         rel.querySet( "AGE < 30" ) ;

         rel.sortSet( "ID" ) ;

         rel.bottom() ;

         rel.changed() ;

         rel.data() ;

         rel.dataTag() ;

         rel.doAll() ;

         relS.doOne() ;

         rel.eof() ;

         rel.errorAction( Code4.relate4blank ) ;

         rel.lockAdd() ;

         rel.master() ;

         rel.masterExpr() ;

         relS.matchLen( 10 ) ;

         rel.optimizeable() ;

         rel.top() ;

         rel.skip( 1 ) ;

         rel.skipEnable( 1 ) ;

         relS.type( Code4.relate4scan ) ;

         Relate4iterator relIt = new Relate4iterator( rel ) ;
         relIt.next() ;

         //

         cb.closeAll() ;
         cb.readOnly = 1 ;
         cb.errOff = 0 ;
         cb.errOpen = 1 ;
         cb.compatibility = 30 ;

//                    report := report4retrieve(cb, 'q:\cb64\cb64\coderep\examples\tut1', 1, 'q:\cb64\cb64\coderep\examples');
//                                report4caption(report, 'T4ALL');
//                                report4currency(report, '$');
//                                report4dateFormat(report, 'CCYYMMDD');
//                                report4decimal(report, '.');
//                                report4margins(report, 10, 10, 10, 10, 1);
//                                report4pageSize(report, 10, 10, 1);
//                                report4parent(report, 0);
//                                report4printerSelect(report);
//                                report4querySet(report, 'T4ALL');
//                                report4relate(report);
//                                report4separator(report, ',');
//                                report4sortSet(report, 'T4ALL');
//                                report4toScreen(report, 1);
//                                report4do(report);
//                                report4save(report, 'c:\temp\tut1', 0);
//                                report4free(report, 1, 1);

         //

         cb.closeAll() ;

         cb.compatibility = 30 ;
         cb.safety = 0 ;
         data.create( ref cb, "t4all", ref fields, ref tags ) ;

         tag.init( data, "STR" ) ;

         tag.alias() ;

         if ( cb.indexExtension().ToUpper() == "NTX" )
         {
            tag.close() ;
            tag.open( data, "STR" ) ;
         }

         tag.expr() ;

         tag.filter() ;
                       
         tag.init( data, "NUM" ) ;
         short newShrt = tag.unique ;

         tag.unique = Code4.r4unique ;

         cb.initUndo() ;
         cb.init() ;
         Console.WriteLine( "Application is about to exit. That is good." ) ;
         //cb.exit() ;
         cb.initUndo() ;
		}
	}
}
