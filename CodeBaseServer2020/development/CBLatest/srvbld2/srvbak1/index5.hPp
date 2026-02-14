// index5.hpp
#ifdef OLEDB5BUILD

class S5CLASS Index5 ;
class S5CLASS Rowset5index ;
class S5CLASS Table5cache ;
#ifdef S4STAND_ALONE
   class S5CLASS Table5stand ;
#endif
#ifdef S4SERVER
   class S5CLASS Table5server ;
#endif

S5EXPORT COLUMN5INFO * S4FUNCTION t5columns( TAG4 S4PTR *tag, short S4PTR *nColumns, int isUnicode, Source5config *src ) ;

#ifndef S4SERVER
// S4STAND_ALONE, S4CLIENT
class S5CLASS Index5 : public Table5all
{
   // This class is used to define the virtual functions which a Index5client and
   // Index5stand must implement in order to provide support for a Rowset5index.
   // By creating this class, the Rowset5index class can be implemented without using
   // #ifdefs to different structures depending on whether it is implemented as client
   // or stand-alone
public:
   Index5() : Table5all( 0 ) { ; }
   virtual ~Index5() { ; }

   HRESULT mapColumnIds( unsigned long nIds, const struct tagDBID columnNames[], unsigned long columndIds[] ) ;
   virtual COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) = 0 ;
   virtual void close() = 0 ;
   // virtual short keyLen() = 0 ;  -- AS 03/24/99 --> moved to Table5all class...
   virtual int isDeleted() = 0 ;
   virtual int nullable() = 0 ;
   inline short numNullBytes() { return ( nullable() ? 1 : 0 ) ; }   // either 1 or 0 bytes...
   inline virtual void lockIndex() { return ; }  // client just ignores
   virtual void positionRetrieve( Rowset5indexPlusIntegratedIndex *indexRowset ) = 0 ;
   virtual void refetchWithKey( unsigned long recNo, char *keyBuf ) = 0 ;
   virtual int  rowGet( int direction, unsigned long *recNo, void *key, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) = 0 ;
   virtual void rowPosition( const void *startPos, unsigned long startRecno, long lRowsOffset, int direction, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) = 0 ;
   virtual void  rowRequest( Rowset5indexPlusIntegratedIndex *indexRowSet, long lRowsOffset, int cRecNo, int sendKey, Table5rowsetValues *rowetValues ) = 0 ;
   virtual void rowSeek( char *key, int keyLen, DBSEEK seekOptions, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) = 0 ;
//   virtual void setRange( const void *startKey, int startKeyLen, const void *endKey, int endKeyLen, DBRANGE rangeOptions ) = 0 ;
   inline virtual void unlockIndex() { return ; }  // client just ignores
   inline int isFieldDeferred( int iOrdinal ) { return 0 ; }   // no deferred fields for index
   virtual void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) = 0 ; //, int *offsetRaw = 0 ) = 0 ;
   virtual void remove() = 0 ;
   void setStartPosition( Rowset5indexPlusIntegratedIndex *indexRowset, long startRecNo ) ;
   inline void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection, int iRowset, char countDeletedAsSkip = 0 )
      { throw Err5internal(0) ; }  // not implemented; is it required?
   inline int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction,
      Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition )
      { throw Err5internal(0) ; return S_OK ;}  // not implemented
   // does nothing in index case - no deferred fields
   virtual COLUMN5INFO *getIndexRowsetColumns( short *numColumns, int *freeRequired ) = 0 ;
   inline void rowRequestGetDeferredFields( Fieldset5all *fields, ULONG recNo, char **rawBuffer, ByRef5 *byRefBuffer, Table5rowsetValues *rowsetValues, int iRowset ) { ; }
   inline int isReadOnly( int iOrdinal ) { return 0 ; }  // returns false because the context is key setting for searching.
   virtual short unique() = 0 ;  // returns unique setting (e4unique, r4unique, r4candidate, r4uniqueContinue)
   #ifdef S4FOX
      virtual inline Collate4name collateName() { return collate4none ; }  // used for collated index rows only, by default not known
   #endif
} ;
#endif /* not S4SERVER */

#ifndef S4CLIENT
// S4STAND_ALONE, S4SERVER
class S5CLASS Index5low
{
   // This class is used to perform the actual low-level physical index operations that
   // are required to support Index5rowset operations.
   // only used as a base class, no need for special memory allocations
public:
   Session5 *session ;

   COLUMN5INFO *getColumns( short *nColums, int *freeRequired ) { *freeRequired = 1 ; return t5columns( tag, nColums, session->isUnicode, session->source ) ; }
   inline char *key() { return tfile4key( tagFile ) ; }
   int rangeSatisfy( Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData, char *keyIn = 0, short keyLength = 0, int descend = 0 ) ;
   int rowGet( int direction, unsigned long *recNo, void *key, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;
   void rowPosition( const void *startPos, unsigned long startRecno, long lRowsOffset, int direction, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;
   // if useEndpoints is true, then top()/bottom() is considered a single skip
   void skip( int direction, char useEndpoints, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;
   inline short keyLen() { return t4keyLenExported( tag ) ; }
   void remove() ;
   void rowSeek( char *key, short keyLength, DBSEEK seekOptions, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;

   inline unsigned long recNo() { return tfile4recNo( tagFile ) ; }
   void refetchWithKey( unsigned long recNo, char *keyBuf ) ;
   inline short unique() { return t4unique( tag ) ; }
protected:
   Index5low( Session5 *session, Table5low *tbl, char *tagName ) { init( session, tbl, tagName ) ; }
   Index5low( Session5 *session, Table5low *tbl, LPCWSTR tagName ) ;

   void init( Session5 *sessn, Table5low *tbl, char *tagName ) ;
   inline void bof( Table5rowsetValues *rowsetValues )
   {
      rowsetValues->atEndpoint = 1 ;
      throw Err5bof() ;
   }
   inline void bottom( Table5rowsetValues *rowsetValues ) { tagBottom( rowsetValues ) ; }

   inline int descending() { return tfile4isDescending( tagFile ) ; }
   inline void descending( int newValue, Table5rowsetValues *rowsetValues )
   {
      if ( s5mdx )
         rowsetValues->descendingIsDesired = newValue ;
      else
         tfile4descending( tagFile, (newValue > 0) ? r4descending : 0 ) ;
   }
   inline int descendingDesired( Table5rowsetValues *rowsetValues )
   {
       return ( s5mdx ? ( rowsetValues->descendingIsDesired) : descending() )  ;
   }

   inline void eof( Table5rowsetValues *rowsetValues )
   {
      rowsetValues->atEndpoint = 1 ;
      throw Err5eof() ;
   }
   void go( const void *key, unsigned long recNo, Table5rowsetValues *rowsetValues ) ;
   void go( unsigned long recNo ) ;
   int isDeleted() ;
   // used to lock the index file itself.  in general we want to do this in order to
   // avoid worrying about other people updating an index in the middle of our 0reads
   int lockIndex() ;
   int lockIsolationRead( unsigned long recNo ) ;
   inline void notFound( Table5rowsetValues *rowsetValues )
   {
      rowsetValues->atEndpoint = 1 ;
      throw Err5found(session->c4) ;
   }
   void positionRetrieve( ULONG *recNum, void *keyOut ) ;
   int rowSeekRestrict( char *key, short *keyLength, DBSEEK *seekOptions, int incrementor, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;
   int seekLow( const void *key, int seekLen, int direction, Table5rowsetValues *rowsetValues ) ;   // performs a low-level seek
   inline void top( Table5rowsetValues *rowsetValues ) { tagTop( rowsetValues ) ; }
   inline void unlockIndex()
   {
      if (s5clipper)
      {
         if ( tfile4unlock( tag->tagFile, data4serverId( tag->index->data ) ) != 0 )
            throw Err5internal(session->c4);
      }
      else
         if ( dfile4unlockIndex( tag->index->data->dataFile, data4serverId( tag->index->data ) ) != 0 )
            throw Err5internal(session->c4) ;
   }
   #ifdef S4FOX
      inline Collate4name collateNameLow() { assert5( tagFile != 0 ) ; return tagFile->collateName ; }  // used for collated index rows only, by default not known
   #endif
protected:
   TAG4 *tag ;                // Index5 corresponds to a TAG4
private:
   void rowPositionSetupStartPosition( Table5rowsetValues *rowsetValues, const unsigned long offsetDo,
     int *atEndpointPos, const int direction, Index5rangeInfo *rangeData, Bool5 *prePositioned,
     const char directionOffset, const int directionDo, const int directionToSeek ) ;

   void Index5low::rowPositionPostionAtStartPosition( const void *startPos, const unsigned long startRecno,
     Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData, const unsigned long offsetDo, const int directionDo,
     const int direction, const long lRowsOffset, const char directionOffset ) ;

   void Index5low::rowPositionPrecursoryPosition( Table5rowsetValues *rowsetValues, const int directionDo,
     const int directionToSeek, const char directionOffset, unsigned long *offsetDo, const int direction,
     Index5rangeInfo *rangeData ) ;

   TAG4FILE *tagFile ;        // to actually perform low-level operations

   int isNullable ;
   Table5low *table ;
   int seekEqualVerify ;  // sometimes need to do a final comparision at end with original specs
   void tagTop( Table5rowsetValues *rowsetValues ) ;
   void tagBottom( Table5rowsetValues *rowsetValues ) ;
   int tagSkip( int nSkip, Table5rowsetValues *rowsetValues ) ;
} ;
#endif /* not S4CLIENT */



#ifdef S4CLIENT
class S5CLASS Index5client : public Index5
{
   // this class is used to perform communications requests for the client and to
   // interact with the server in order to perform the required Index5 operations
   // required to support an Index5rowset
public:
   Index5client( Session5 *sess, Table5client *tbl, LPCWSTR tagName ) ;
   ~Index5client() { close() ; }

   COLUMN5INFO *getColumns( short *numColumns, int *freeRequired ) ;
   void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) ; //, int *offsetRaw = 0 ) ;
   short iDataGet() { return iData ; }
   inline short keyLen() { return keyLength ; }  // need this info for various purposes, maybe get on receive on instigation
   unsigned short numFields() ;
   void remove() ;
   inline void rowPosition( const void *startPos, unsigned long startRecno, long lRowsOffset, int direction, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) { ; }
   void updateFieldset( Fieldset5all *fieldset, int *iRowset ) { ; }  // only an issue of whether or not receiving keys, not relevant here...
   void updateVersions() ;
   inline ULONG maxCount() { return ROWSET5MAX_GET_HANDLES ; }  // for data files, max count == rec count
   inline COLUMN5INFO *getIndexRowsetColumns( short *numColumns, int *freeRequired ) { return Index5client::getColumns( numColumns, freeRequired ) ; }

   short iData ;     // index into server's Table5server array
   short iIndex ;   // index into server's array of Index5server
   void rowRequestIntegrated( Rowset5indexPlusIntegratedIndex *indexRowSet, long lRowsOffset, int cRecNo, int sendKey, Table5rowsetValues *rowetValues ) ;
   int getOffsetValue( const Fieldset5all *fieldsetIn, const Field5all *fieldOn, int *offset ) ;
   void setNullBytesOffset( Fieldset5all *fieldsetIn, int offset ) { ; }
   Bool5 lossOfInfo( int iOrdinal ) ;
   int getArrayIndexFromOrdinal( int iOrdinal ) ;
   inline short unique() { assert5( uniqueVal != -1 ) ; return uniqueVal ; }
private:
   Table5cache *table ;  // Index requires a Table5cache for CodeBase consistency, locking and to avoid locking conflicts

   int isDeletedFlag ;    // zero if not deleted, 1 if deleted, obtained when open() is called
   short keyLength ;
   int doFlush ;
   short uniqueVal ;
   Session5 *session ;

   void close() ;
   inline int isDeleted() { return isDeletedFlag ; } // True if filter expression is .NOT. DELETED(), passed on open
   inline int nullable() { return (columns())[1].isNullable ; }
   void positionRetrieve( Rowset5indexPlusIntegratedIndex *indexRowset ) ;
   void refetchWithKey( unsigned long recNo, char *keyBuf ) ;
   int rowGet( int direction, unsigned long *recNo, void *key, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;
   void rowRequest( Rowset5indexPlusIntegratedIndex *indexRowSet, long lRowsOffset, int cRecNo, int sendKey, Table5rowsetValues *rowetValues ) ;
   void rowSeek( char *key, int keyLen, DBSEEK seekOptions, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;
   inline int numDeferredFields() { return 0 ; }  // always send accross the whole key or no key
} ;
#endif /* S4CLIENT */



#ifndef S4SERVER
class5 Rowset5integratedIndexIndexColumnInfo : public Column5info
{
   // same as regular index info except that the column ordinals match those of the base rowset
   // not those of the index rowset
public:
   Rowset5integratedIndexIndexColumnInfo() { integratedIndexRowset = 0 ; }
   void init( Rowset5integratedIndex *integratedIndexRowsetParm )
      {
         integratedIndexRowset = integratedIndexRowsetParm ;
      }
   COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) ;
private:
   Rowset5integratedIndex *integratedIndexRowset ;
} ;
#endif /* !S4SERVER */


#ifdef S4STAND_ALONE
class S5CLASS Index5stand : public Index5, public Index5low
{
   // This class is used in the stand-alone instance to provide Index5 support for an
   // Rowset5index.  This class implements Index5, and uses base class Index5low to perform the
   // actual low-level physical operations.
public:
   // required for locking, we need DATA4 access.  Set up based on dataName when open() called
   Table5stand *table ;

   ~Index5stand() { ; }
   Index5stand( Session5 *sess, Table5low *tbl, LPCWSTR tagName ) : Index5low( sess, tbl, tagName ) { table = 0 ; }
   inline void close() { ; }   // all is done on destructor, nothing to do here

   inline ULONG maxCount() { return ROWSET5MAX_GET_HANDLES ; }  // for data files, max count == rec count
//   inline ULONG maxCount() { return table->recCount() ; }  // for data files, max count == rec count

//   HRESULT mapColumnIds( unsigned long nIds, const struct tagDBID columnNames[], unsigned long columndIds[] ) ;
   inline int isDeleted() { return Index5low::isDeleted() ; }
   inline short keyLen() { return Index5low::keyLen() ; }
   void positionRetrieve( Rowset5indexPlusIntegratedIndex *indexRowset ) ;
   inline void refetchWithKey( unsigned long recNo, char *keyBuf ) { Index5low::refetchWithKey( recNo, keyBuf ) ; }
   inline int rowGet( int direction, unsigned long *recNo, void *key, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData )
      { return Index5low::rowGet( direction, recNo, key, rowsetValues, rangeData ) ; }
   inline void rowRequest( Rowset5indexPlusIntegratedIndex *indexRowSet, long lRowsOffset, int cRecNo, int sendKey, Table5rowsetValues *rowetValues ) { ; }
   inline void rowPosition( const void *startPos, unsigned long startRecno, long lRowsOffset, int direction, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData )
      { Index5low::rowPosition( startPos, startRecno, lRowsOffset, direction, rowsetValues, rangeData ) ; }
   inline void rowSeek( char *key, int keyLen, DBSEEK seekOptions, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData )
      { Index5low::rowSeek( key, keyLen, seekOptions, rowsetValues, rangeData ) ; }
//   void setRange( const void *startKey, int startKeyLen, const void *endKey, int endKeyLen, DBRANGE rangeOptions ) ;
   inline void updateFieldset( Fieldset5all *fieldset, int *iRowset ) { ; }
   inline COLUMN5INFO *getIndexRowsetColumns( short *numColumns, int *freeRequired ) { return Index5low::getColumns( numColumns, freeRequired ) ; }
   int nullable() ;
   inline void remove() { Index5low::remove() ; }
   inline int numDeferredFields() { return 0 ; }
   virtual COLUMN5INFO *getColumns( short *numColumns, int *freeRequired ) { return Index5low::getColumns( numColumns, freeRequired ) ; }
   void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) ; //, int *offsetRaw = 0 ) ;
   unsigned short numFields() ;
   long getFieldInfoOffset( int ordinal ) ;
   int getOffsetValue( const Fieldset5all *fieldsetIn, const Field5all *fieldOn, int *offset ) ;
   void setNullBytesOffset( Fieldset5all *fieldsetIn, int offset ) { ; }
   Bool5 lossOfInfo( int iOrdinal ) ;
   int getArrayIndexFromOrdinal( int iOrdinal ) ;
   inline short unique() { return Index5low::unique() ; }
   #ifdef S4FOX
      inline Collate4name collateName() { return Index5low::collateNameLow() ; }
   #endif
} ;
#endif /* S4STAND_ALONE */



#ifndef S4SERVER
#ifdef S4CLIENT
class S5CLASS Index5integratedIndex : public Index5client
#else
class S5CLASS Index5integratedIndex : public Index5stand
#endif
{
   // basically the same as Index5stand but the column info is different because the ordinals
   // match those of the base table rowset, not those of the index file rowset
public:
   #ifdef S4CLIENT
      Index5integratedIndex( Rowset5integratedIndex *integratedIndexRowset, Session5 *sess, Table5client *tbl, LPCWSTR tagName ) : Index5client( sess, tbl, tagName ) { indexColumnInfo.init( integratedIndexRowset ) ; }
   #else
      Index5integratedIndex( Rowset5integratedIndex *integratedIndexRowset, Session5 *sess, Table5low *tbl, LPCWSTR tagName ) : Index5stand( sess, tbl, tagName ) { indexColumnInfo.init( integratedIndexRowset ) ; }
   #endif

   COLUMN5INFO *getColumns( short *numColumns, int *freeRequired ) { return indexColumnInfo.getColumns( numColumns, freeRequired ) ; }
//   COLUMN5INFO *getIndexRowsetColumns( short *numColumns ) { return getColumns( numColumns ) ; }
private:
   Rowset5integratedIndexIndexColumnInfo indexColumnInfo ;
} ;
#endif /* !S4SERVER */




#ifdef S4SERVER
// S4SERVER
class S5CLASS Index5server : public Index5low, public Column5info
{
   // This class is used by the server to both interact with the Index5client (client's
   // Index) and use the base class Index5low to perform the low-level index operations
   // required by the server to support a Client's Rowset5index
public:
   // required for locking, we need DATA4 access.  Set up based on dataId when open() called
   Table5server *table ;
   Index5server( Session5 *session, Table5low *tbl, char *tagName ) : Index5low( session, tbl, tagName ) {}
   void * operator new( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
   void operator delete( void *p ) { free5(p) ; }

   int tagRowsRetrieve( char *currentKey, long currentRecNo, long nOffset, long nRecs, int sendKeys ) ;

   char currentKey[I4MAX_KEY_SIZE] ;
private:
   COLUMN5INFO *getColumns( short *nColums, int *freeRequired ) { return Index5low::getColumns( nColums, freeRequired ) ; }

} ;
#endif /* S4SERVER */

#endif /* OLEDB5BUILD */
