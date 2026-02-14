/* d4inline.h   (c)Copyright Sequiter Software Inc., 1988-1998.  All rights reserved. */

#ifdef S4INLINE

   /* B4BLOCK.C */
   #ifndef S4INDEX_OFF
      #ifndef S4CLIENT
         #ifdef S4MDX
            #define b4goEof( b4 )         ( (b4)->keyOn = (b4)->nKeys )
            #define b4key( b4, iKey )     ( (B4KEY_DATA *)((char *)&((b4)->info.num) + (b4)->tag->header.groupLen * (iKey)) )
            #define b4keyKey( b4, iKey ) ( (unsigned char *)(((B4KEY_DATA *)( (char *)&((b4)->info.num) + (b4)->tag->header.groupLen * (iKey) ))->value ) )
            #define b4lastpos( b4 )        ( ( b4leaf( (b4) ) ) ? ( (b4)->nKeys - 1 ) : ( (b4)->nKeys ) )
            #define b4leaf( b4 )           ( b4key( (b4), (b4)->nKeys )->num == 0L )
            #define b4recNo( b4, i )       ( b4key( (b4), (i) )->num )
         #endif /* S4MDX */

         #ifdef S4FOX
            #define b4insert( b4, k, r, r2, nf ) ( b4leaf( (b4) ) ? b4insertLeaf( (b4), (k), (r) ) : b4insertBranch( (b4), (k), (r), (r2), (nf) ) )
            #define b4go( b4, iKey )      ( b4skip( (b4), (iKey) - (b4)->keyOn ) )
            /* LY 2001/03/20 - changed b4keyKey() for case where b4key() returns 0 */
            #define b4keyKey( b4, iKey )  ( b4key( (b4), (iKey) ) ? (unsigned char *)b4key( (b4), (iKey) )->value : 0 )
            #define b4lastpos( b4 )        ( (b4)->header.nKeys - 1 )
            #define b4leaf( b4 )           (  (b4)->header.nodeAttribute >= 2 )
         #endif /* S4FOX */

         #ifdef S4CLIPPER
            #define b4goEof( b4 )         ( (b4)->keyOn = (b4)->nKeys )
            #define b4keyKey( b4, iKey ) ( (unsigned char *) b4key( (b4), (iKey) )->value )
            #define b4lastpos( b4 )        ( ( b4leaf( (b4) ) ) ? ( (b4)->nKeys - 1 ) : ( (b4)->nKeys ) )
            #define b4leaf( b4 )           ( ( b4key( (b4), 0 )->pointer == 0L ) )
            #define b4recNo( b4, i )       ( b4key( (b4), i )->num )
            #ifdef S4CLIPPER
               #define b4key( b4, iKey )     ( (B4KEY_DATA *)((char *)&((b4)->nKeys) + ((b4)->pointers)[(iKey)] ) )
            #endif /* S4CLIPPER */
         #endif /* S4CLIPPER */
      #endif  /* S4CLIENT */
   #endif  /* S4INDEX_OFF */

   #ifdef S4CLIENT
      #define code4tranInit2( a, b, c )  ( 0 )
      #define code4tranInitUndo( a )
   #endif

   /* D4DATA.C */
   #ifdef S4STAND_ALONE
      #define data4serverId( d4 ) ( (d4)->clientId )
      #define data4clientId( d4 ) ( (d4)->clientId )
   #endif

   #ifdef S4SERVER
      #define data4serverId( d4 ) ( (d4)->serverId )
      #define data4clientId( d4 ) ( (d4)->clientId )
      #ifndef S4OFF_WRITE
         #define code4transEnabled( c4 ) ( (c4)->currentClient == 0 ? 0 : ( (c4)->currentClient->trans.c4trans->enabled && ( code4tranStatus( (c4) ) != r4rollback ) && ( code4tranStatus( (c4) ) != r4off ) ) )
      #endif
      #define code4trans( c4 ) ( (c4)->currentClient == 0 ? 0 : (&(c4)->currentClient->trans ) )
   #else
      #ifndef S4SINGLE
      #endif
      #define code4trans( c4 ) ( &(c4)->c4trans.trans )
      #ifndef S4OFF_TRAN
         #ifndef S4OFF_WRITE
         #endif
         #ifndef S4OFF_WRITE
            #ifdef S4CLIENT
               #define code4transEnabled( c4 ) ( (c4)->c4trans.enabled )
            #else
               #define code4transEnabled( c4 ) ( (c4)->c4trans.enabled && ( code4tranStatus( (c4) ) != r4rollback ) && ( code4tranStatus( (c4) ) != r4off ) )
            #endif
         #endif
      #endif
   #endif  /* S4SERVER */

   #ifdef S4CLIENT
      #define data4serverId( d4 ) ( (d4)->dataFile->serverId )
      #define data4clientId( d4 ) ( (d4)->clientId )
   #endif

   #ifdef S4SERVER
      #define code4dateFormat( c4 ) ( (c4)->currentClient == 0 ? 0 : ( (c4)->currentClient->trans.dateFormat ) )
   #endif

   /* C4TRANS.C */
   #ifdef S4STAND_ALONE /* temp patch */
      #define code4tranRollbackSingle( c4 )     ( tran4lowRollback( &((c4)->c4trans.trans), 0, 0 ) )
   #endif
   #ifndef S4OFF_TRAN
      #define tran4bottom( t4 )        ( tran4fileBottom( (t4)->c4trans->transFile, (t4) ) )
      #define tran4entryLen( t4 )      ( sizeof( LOG4HEADER ) + (t4)->dataLen + sizeof( TRAN4ENTRY_LEN ) )
      #define tran4clientDataId( t4 )  ( (t4)->header.clientDataId )
      #define tran4clientId( t4 )      ( (t4)->header.clientId )
      #define tran4id( t4 )            ( (t4)->header.transId )
      #define tran4len( t4 )           ( (t4)->header.dataLen )
      #define tran4serverDataId( t4 )  ( (t4)->header.serverDataId )
      #define tran4skip( t4, d )       ( tran4fileSkip( (t4)->c4trans->transFile, (t4), (d) ) )
      #define tran4top( t4 )           ( tran4fileTop( (t4)->c4trans->transFile, (t4) ) )
      #define tran4type( t4 )          ( (t4)->header.type )
      #ifdef S4SERVER
         #define code4connectionId( c4 ) ( c4->currentClient->id )
      #endif
   #endif

   #define u4ptrEqual( a, b ) ( a == b )
   #define u4delaySec() ( u4delayHundredth( 100 ) )

   #define tran4dataList( t4 ) ( (t4)->dataList )
   #define tran4dataListSet( t4, l4 ) ( (t4)->dataList = l4 )

   #define expr4parse( a, b ) ( expr4parseLow( (a), (b), 0 ) )

#else   /* NOT S4INLINE STARTS NOW... */

   #ifdef __cplusplus
      extern "C" {
   #endif
   S4EXPORT void S4PTR * S4FUNCTION l4first( const LIST4 S4PTR * ) ;  /* Returns 0 if none */
   S4EXPORT void S4PTR * S4FUNCTION l4last( const LIST4 S4PTR * ) ;   /* Returns 0 if none */
   S4EXPORT void S4PTR * S4FUNCTION l4next( const LIST4 S4PTR *, const void S4PTR * ) ;  /* Returns 0 if none */
   S4EXPORT void S4FUNCTION l4add( LIST4 S4PTR *, void S4PTR * ) ;

   S4EXPORT int S4FUNCTION error4code( CODE4 S4PTR * ) ;

   LIST4 * S4FUNCTION tran4dataList( TRAN4 * ) ;
   int tran4dataListSet( TRAN4 *, LIST4 * ) ;

   void *s4real( FIXED4MEM ) ;
   void *s4protected( FIXED4MEM ) ;
   long data4clientId( DATA4 * ) ;
   long S4FUNCTION data4serverId( DATA4 * ) ;
   #ifdef __cplusplus
      }
   #endif
   #ifdef S4SERVER
      #define code4dateFormat( c4 ) ( (c4)->c4trans.trans.dateFormat )
   #endif
   #ifdef __cplusplus
      extern "C" {
   #endif
   #ifndef S4SINGLE
      int code4unlockAutoSave( CODE4 *c) ;
   #endif
   #ifndef S4OFF_WRITE
      #ifndef S4OFF_TRAN
         int code4transEnabled( CODE4 * ) ;
      #endif
   #endif
   TRAN4 *code4trans( CODE4 * ) ;

   /* C4TRANS.C */
   #if !defined(S4CLIENT) && !defined(S4OFF_TRAN)
      unsigned short int tran4entryLen( LOG4HEADER * ) ;
   #endif

   S4EXPORT int    S4FUNCTION u4ptrEqual( const void S4PTR *, const void S4PTR * ) ;

   #ifdef __cplusplus
      }
   #endif
#endif   /* S4INLINE */

#ifndef S4STAND_ALONE
   #ifdef S4WINSOCK
      #define ioctl(a,b,c)         (ioctlsocket((a),(b),(c)))
   #endif
   #ifdef S4BERKSOCK
      #define closesocket(alpha)         (close(alpha))
   #endif
#endif


#ifndef S4NO_FILELENGTH
   #define u4filelength( a )          ( filelength( a ) )
#endif

#ifdef S4CB51
   #define c4trim_n( a, b )          ( c4trimN( a, b ) )

   #ifndef S4SERVER
/*   #ifndef S4CBPP*/
      #define code4date_format( a )  ( code4dateFormat( a ) )
      #define code4date_format_set( a, b ) ( code4dateFormatSet( a, b ) )
/*   #endif*/
   #endif
   #ifdef S4CLIENT
      #define code4freeBlocks( a )   ( 0 )
   #endif
   #define d4alias_set( a, b )       ( d4aliasSet( a, b ) )
   #define d4append_blank( a )       ( d4appendBlank( a ) )
   #define d4append_start( a, b )    ( d4appendStart( a, b ) )
   #define d4close_all( a )          ( code4close( a ) )
   #define d4data( a,b )             ( code4data( a, b ) )
   #define d4field_info( a )         ( d4fieldInfo( a ) )
   #define d4field_j( a, b )         ( d4fieldJ( a, b ) )
   #define d4field_number( a, b )    ( d4fieldNumber( a, b ) )
   #define d4flush_data( a )         ( d4flushData( a ) )
   #define d4flush_files( a )        ( code4flush( a ) )
   #define d4free_blocks( a )        ( d4freeBlocks( a ) )
   #define d4go_data( a, b )         ( d4goData( a, b ) )
   #define d4go_bof( a )             ( d4goBof( a ) )
   #define d4go_eof( a )             ( d4goEof( a ) )
   #define d4init( a )               ( code4init( a ) )
   #define d4init_undo( a )          ( code4initUndo( a ) )
   #define d4lock_all( a )        ( d4lockAll( a ) )
   #define d4lock_append( a )        ( d4lockAppend( a ) )
   #define d4lock_file( a )          ( d4lockFile( a ) )
   #ifndef S4CLIENT
      #define d4lock_index( a )      ( d4lockIndex( a ) )
   #endif
   #define d4lock_test( a, b )       ( d4lockTest( a, b ) )
   #define d4lock_test_append( a )   ( d4lockTestAppend( a ) )
   #define d4lock_test_file( a )     ( d4lockTestFile( a ) )
   #define d4memo_compress( a )      ( d4memoCompress( a ) )
   #define d4num_fields( a )         ( d4numFields( a ) )
/*   #ifdef S4CBPP*/
      #define d4optimize_write( a, b ) ( d4optimizeWrite( a, b ) )
/*   #endif*/
   #define d4opt_start( a )          ( code4optStart( a ) )
   #define d4opt_suspend( a )        ( code4optSuspend( a ) )
   #define d4pack_data( a )          ( d4packData( a ) )
   #define d4position_set( a, b )    ( d4positionSet( a, b ) )
   #define d4reccount( a )           ( d4recCount( a ) )
   #define d4recno( a )              ( d4recNo( a ) )
   #define d4record_position( a, b ) ( d4recPosition( a, b ) )
/*   #ifdef S4CBPP*/
      #define d4record_width( a )       ( d4recWidth( a ) )
/*   #endif*/
   #define d4refresh_record( a )     ( d4refreshRecord( a ) )
   #define d4seek_double( a, b )     ( d4seekDouble( a, b ) )
   #define d4seek_n( a, b, c )       ( d4seekN( a, b, c ) )
   #define d4tag_default( a )        ( d4tagDefault( a ) )
   #define d4tag_next( a, b )        ( d4tagNext( a, b ) )
   #define d4tag_prev( a, b )        ( d4tagPrev( a, b ) )
   #define d4tag_select( a, b )      ( d4tagSelect( a, b ) )
   #define d4tag_selected( a )       ( d4tagSelected( a ) )
   #define d4unlock_files( a )       ( code4unlock( a ) )
   #define d4update_header( a, b, c )( dfile4updateHeader( (a)->dataFile, b, c ) )
   #define d4write_data( a, b )      ( d4writeData( a, b ) )
   #define d4write_keys( a, b )      ( d4writeKeys( a, b ) )
   #define d4zap_data( a, b, c )     ( d4zapData( a, b, c ) )

   #define date4format_mdx( a )      ( date4formatMdx( a ) )
   #define date4format_mdx2( a, b )  ( date4formatMdx2( a, b ) )
   #define date4time_now( a )        ( date4timeNow( a ) )

/*   #ifdef S4CBPP*/
      #define expr4calc_create( a, b, c )    ( code4calcCreate( a, b, c ) )
/*   #endif*/
   #define expr4calc_delete( a )             ( expr4calcDelete( a ) )
   #define expr4calc_lookup( a, b, c )       ( expr4calcLookup( a, b, c ) )
   #define expr4calc_massage( a )            ( expr4calcMassage( a ) )
   #define expr4calc_modify( a, b )          ( expr4calcModify( a, b ) )
   #define expr4calc_name_change( a, b, c )  ( expr4calcNameChange( a, b, c ) )
   #define expr4calc_reset( a )              ( code4calcReset( a ) )
   #ifndef S4CLIENT
      #define expr4key_convert( a, b, c, d ) (( expr4keyConvert( a, b, c, d ) )
   #endif
   #define expr4key_len( a )                 (  expr4keyLen( a ) )

   #define f4assign_char( a, b )      ( f4assignChar( a, b ) )
   #define f4assign_double( a, b )    ( f4assignDouble( a, b ) )
   #define f4assign_field( a, b )     ( f4assignField( a, b ) )
   #define f4assign_int( a, b )       ( f4assignInt( a, b ) )
   #define f4assign_long( a, b )      ( f4assignLong( a, b ) )
   #define f4assign_n( a, b, c )      ( f4assignN( a, b, c ) )
   #define f4assign_ptr( a )          ( f4assignPtr( a ) )
   #define f4memo_assign( a, b )      ( f4memoAssign( a, b ) )
   #define f4memo_assign_n( a, b, c ) ( f4memoAssignN( a, b, c ) )
   #define f4memo_free( a )           ( f4memoFree( a ) )
   #define f4memo_len( a )            ( f4memoLen( a ) )
   #define f4memo_ncpy( a, b, c )     ( f4memoNcpy( a, b, c ) )
   #define f4memo_ptr( a )            ( f4memoPtr( a ) )
   #define f4memo_set_len( a, b )     ( f4memoSetLen( a, b ) )
   #define f4memo_str( a )            ( f4memoStr( a ) )

   #define file4len_set( a, b )            ( file4lenSet( a, b ) )
   #define file4lock_hook( a, b, c, d, e ) ( file4lockHook( a, b, c, d, e ) )
/*   #ifdef S4CBPP*/
      #define file4optimize_write( a, b )  ( file4optimizeWrite( a, b ) )
/*   #endif*/
   #define file4read_all( a, b, c, d )     ( file4readAll( a, b, c, d ) )
   #define file4read_error( a )            ( file4readError( a ) )

   #define file4seq_read( a, b, c )             ( file4seqRead( a, b, c ) )
   #define file4seq_read_all( a, b, c )         ( file4seqReadAll( a, b, c ) )
   #define file4seq_read_init( a, b, c, d, e )  ( file4seqReadInit( a, b, c, d, e ) )
   #define file4seq_write( a, b, c )            ( file4seqWrite( a, b, c ) )
   #define file4seq_write_flush( a )            ( file4seqWriteFlush( a ) )
   #define file4seq_write_init( a, b, c, d, e ) ( file4seqWriteInit( a, b, c, d, e ) )
   #define file4seq_write_repeat( a, b, c )     ( file4seqWriteRepeat( a, b, c ) )

   #ifndef S4CLIENT
      #define i4add_tag( a, b )    ( i4tagAdd( a, b ) )
   #endif
   #define i4tag_info( a )         ( i4tagInfo( a ) )
   #define l4add_after( a, b, c )  ( l4addAfter( a, b, c ) )
   #define l4add_before( a, b, c ) ( l4addBefore( a, b, c ) )

   #define relate4create_slave( a, b, c, d ) ( relate4createSlave( a, b, c, d ) )
   #define relate4do( a )                    ( relate4doAll( a ) )
   #define relate4do_one( a )                ( relate4doOne( a ) )
/*   #ifdef S4CBPP*/
      #define relate4error_action( a, b )    ( relate4errorAction( a, b ) )
      #define relate4match_len( a, b )       ( relate4matchLen( a, b ) )
/*   #endif*/
   #define relate4free_relate( a, b )        ( relate4freeRelate( a, b ) )
   #define relate4query_set( a, b )          ( relate4querySet( a, b ) )
   #define relate4skip_enable( a, b )        ( relate4skipEnable( a, b ) )
   #define relate4sort_set( a, b )           ( relate4sortSet( a, b ) )

   #define sort4assign_cmp( a, b ) ( sort4assignCmp( a, b ) )
   #define sort4get_init( a )      ( sort4getInit( a ) )

   #define t4unique_set( a, b )   ( t4uniqueSet( a, b ) )
   #ifndef S4CLIENT  /* what to do if S4CLIENT defined */
      #define t4add( a, b, c )       ( tfile4add( (a)->tagFile, b, c ) )
      #define t4add_calc( a, b )     ( t4addCalc( a, b ) )
      #define t4block( a )           ( tfile4block( (a)->tagFile ) )
      #define t4bottom( a )          ( tfile4bottom( (a)->tagFile ) )
      #define t4down( a )            ( tfile4down( (a)->tagFile ) )
      #define t4dump( a, b, c )      ( tfile4dump( (a)->tagFile, b, c ) )
      #define t4eof( a )             ( tfile4eof( (a)->tagFile ) )
      #ifdef S4CLIPPER
         #define t4flush( a )           ( tfile4flush( (a)->tagFile ) )
      #endif
      #define t4free_all( a )        ( tfile4freeAll( (a)->tagFile ) )
      #define t4go( a, b, c )        ( tfile4go( (a)->tagFile, b, c, 0 ) )
      #define t4is_descending( a )   ( tfile4isDescending( (a)->tagFile ) )
      #define t4key( a )             ( tfile4key( (a)->tagFile ) )
      #define t4position( a )        ( tfile4position( (a)->tagFile ) )
      #define t4position_set( a, b ) ( tfile4positionSet( (a)->tagFile, b ) )
      #define t4recno( a )           ( tfile4recNo( (a)->tagFile ) )
      #define t4remove( a, b, c )    ( tfile4remove( (a)->tagFile, b, c ) )
      #define t4remove_calc( a, b )  ( tfile4removeCalc( (a)->tagFile, b ) )
      #define t4seek( a, b, c )      ( tfile4seek( (a)->tagFile, b, c ) )
      #ifdef S4HAS_DESCENDING
         #define t4dskip( a, b )     ( tfile4dskip( (a)->tagFile, b ) )
      #endif
      #define t4skip( a, b )         ( tfile4skip( (a)->tagFile, b ) )
      #define t4top( a )             ( tfile4top( (a)->tagFile ) )
      #define t4up( a )              ( tfile4up( (a)->tagFile ) )
      #define t4up_to_root( a )      ( tfile4upToRoot( (a)->tagFile ) )
   #endif /* not S4CLIENT*/

   #define u4alloc_again( a, b, c, d )   ( u4allocAgain( a, b, c, d ) )
   #define u4alloc_er( a, b )            ( u4allocEr( a, b ) )
   #define u4alloc_free( a, b )          ( u4allocFree( a, b ) )
   #define u4name_char( a )              ( u4nameChar( a ) )
   #define u4name_ext( a, b, c, d )      ( u4nameExt( a, b, c, d ) )
   #define u4name_piece( a, b, c, d, e ) ( u4namePiece( a, b, c, d, e ) )
   #define u4ptr_equal( a, b )           ( u4ptrEqual( a, b ) )

   #define e4code( a )      ( error4code( a ) )
   #define e4exit( a )      ( code4exit( a ) )
   #define e4exit_test( a ) ( e4exitTest( a ) )
   #define e4set( a, b )    ( error4set( a, b ) )

   #ifdef S4FOX
      #define t4str_to_fox( a, b, c )    ( t4strToFox( a, b, c ) )
      #define t4dtstr_to_fox( a, b, c )  ( t4dtstrToFox( a, b, c ) )
      #define t4no_change_str( a, b, c ) ( t4noChangeStr( a, b, c ) )
      #define t4str_to_log( a, b, c )    ( t4strToLog( a, b, c ) )
      #define t4dbl_to_fox( a, b )       ( t4dblToFox( a, b ) )
   #endif
#endif  /* S4CB51 */
