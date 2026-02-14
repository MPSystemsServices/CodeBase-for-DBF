// rowset5.hpp

#ifdef OLEDB5BUILD

/*
    (*) - means look at the previous defined class for details

    Table5rowsetValues

    Index5rangeInfo

    Rowset5interface::Unknown5

    RowList5::List5

    Rowset5index::IRowsetIndex
                ::Rowset5all::Row5bufControl
                            ::IAccessor
                            ::IColumnsInfo
                            ::IConvertType
                            ::IRowset
                            ::IRowsetInfo
                            ::Unknown5

    Rowset5::IRowsetUpdate
           ::IRowsetScroll
           ::RowsetPlusSchema5::IRowsetIdentity
                              ::Rowset5all::Row5bufControl
                                          ::IAccessor
                                          ::IColumnsInfo
                                          ::IConvertType
                                          ::IRowset
                                          ::IRowsetInfo
                                          ::Unknown5

      Rowset5integratedIndex::Rowset5indexPlusIntegratedIndex
                            ::Rowset5
                            ::IRowsetCurrentIndex
*/

inline int valid5ptr( const void *ptr ) { return (ptr != 0 ) ; }

#define assignIfValidPtrPtr( pp, p ) ( (pp) ? (*(pp) = p) : 0 )
#define zero5pointer( p ) ( (p) ? (*(p) = 0 ) : 0 )

LPOLESTR SysAllocStringWithFree5( LPWSTR ptr ) ;

// Table5rowsetValues are the various rowset-depending settings which affect
// the table5 functions.  In general these are stored by the rowsets and
// passed to the table functions as required
class Table5rowsetValues
{
public:
   Table5rowsetValues() ;
   void * operator new( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
   void operator delete( void *p ) { free5(p) ; }

   char exposeDeleted ;
   long startCount ;
   char didSeek ; /* need to know if positioning was done via seeking or not for dynamic positioning purposes */
   ULONG setStartRecno, setEndRecno ;
   char requestMethod ;

   char descendingIsDesired ;   // now do we want to treat seeks and skips
   char mdxBof ;                 /*These last two values are only used in DbaseIV files and indexes*/

   char oldDirection, oldOldDirection ;   // used in case direction changes
   char atEndpoint, oldAtEndpoint ;   // position of bof/eof.  for use when using ranges and at eof/bof while physically at a valid location
   char didSkip ;
   char didSkipForBookmarkRC ;  // AS 12/14/00 - used for return code handling for getRowsAt
   long curRecNo ;  // *** in client, this is used ONLY for the provider types rowset, not any other rowsets...

   ULONG **recArray ;       // an array of pointers to bookmarks
   #ifdef S4CLIENT
      Bool5 hasSelectedTag ;
      ULONG clientReRequestRows ;  // AS 01/22/01 - case with IRowsetIdentity with deleted rows...
   #else
      TAG4 *selectedTag ;
   #endif
   #ifdef S4STAND_ALONE
      long count ;    // this is used to contain the # of records available on a getHandles request, and is the max of # requested and # available
   #endif
   #ifdef S4SERVER
      Fieldset5server fieldSet ;   // the server has a single fieldSet used always
      Table5serverBase *table ;
   #endif
} ;

#ifdef S4CLIENT
class S5CLASS Index5rangeInfo
{
public:
   Index5rangeInfo() { index = -1 ; connect = 0 ; }
   ~Index5rangeInfo() { initUndo() ; }
   void * operator new( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
   void operator delete( void *p ) { free5(p) ; }
   void setRange( CONNECT4 *connectIn, short clientIndex, const void *sKey, short startKeyLen, const void *eKey, short endKeyLen, DBRANGE rOptions, short realKeyLen, char isNull ) ;
   short index ;
   void initUndo() ;  // does nothing in client/serverÿ
private:
   CONNECT4 *connect ;
} ;
#else
class S5CLASS Index5rangeInfo
{
   // This class is used to store the range information for an Rowset5index
   // do not need to worry about memory allocation since this class is always a static member
   // of other classes
   // This class is not used in S4CLIENT (server tracks the range)
   // if startKeyLen != 0 then there is valid startKey data, else startKey may be just a leftover
   // allocation.  The same applies to endKeyLen/endKey.
public:
   void *startKey ;
   void *endKey ;
   short startKeyLen ;
   short endKeyLen ;
   short realKeyLen ;
   char excludeNulls ;    // TRUE if nulls should be excluded

   char rangeType ;       // RANGE5MATCH, RANGE5PREFIX, RANGE5NORMAL
   // for RANGE5NORMAL:
   char inclusiveStart ;  // TRUE if inclusive start, otherwise exclusive
   char inclusiveEnd ;    // TRUE if inclusive end, otherwise exclusive

   int isInit ;

   ~Index5rangeInfo() { initUndo() ; }
   Index5rangeInfo() { initNoRange( 1 ) ; }
   void * operator new( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
   void operator delete( void *p ) { free5(p) ; }

   void initUndo() ;

   inline int hasStartKey() { return startKeyLen ; }
   inline int hasEndKey() { return endKeyLen ; }
   void initNoRange( int nullKeyPtrs = 0 ) ;
   #ifdef S4SERVER
      void setRange( CONNECT4 *connect ) ;
   #endif
   #ifdef S4STAND_ALONE
      void setRange( const void *sKey, short startKeyLen, const void *eKey, short endKeyLen, DBRANGE rOptions, short realKeyLen, char isNull ) ;
   #endif
} ;
#endif

void range5copy( Index5rangeInfo *toRange, Index5rangeInfo *fromRange ) ;

#ifndef S4SERVER
class Accessor5 ;
class Table5 ;
#ifdef S4STAND_ALONE
   class Table5stand ;
#endif
#ifdef S4CLIENT
   class Table5client ;
#endif
class Table5all ;
class Index5 ;
class Row5index ;
class Rowset5index ;
class Rowset5;
class Rowset5schema ;
class Rowset5integratedIndex ;
class Result5update ;

inline Row5index *pRow5index( HROW h ) { return (Row5index *) h ; }
inline Row5 *pRow5( HROW h ) { return (Row5 *) h ; }

class5 Rowset5Interface : public Unknown5
{
public:
   Rowset5Interface()  {pRowset5=NULL; pRowset5index=NULL; pRowset5schema=NULL; notSetInterface=0; }
   DECLARE5_IUnknown

   Rowset5 *pRowset5 ;
   Rowset5index *pRowset5index ;
   Schema5rowset *pRowset5schema ;
   Rowset5integratedIndex *pRowset5integrated ;

   short notSetInterface ;
};



class Null5buffer
{
   // class includes querying and setting null information...
public:
   Null5buffer() { nullArray = 0 ; }
   ~Null5buffer()
   {
      if ( nullArray != 0 )
      {
         free5( nullArray ) ;
         nullArray = 0 ;
      }
   }

   int isNullableFieldNull( char *buf, ULONG rowNullBitPosition ) ;
   inline int nullTest( char *buf, ULONG rowNullBitPosition, long iOrdinal = -1 )
   {
      /* AS 09/01/98 critical that this function be inline and fast */
      // AS 08/25/99 -- cannot do this test here, for non-fox, use nullArray sometimes...
      // assert5( rowNullBitPosition != ULONG(-1) ) ; // AS 08/23/99 should be caught earlier for speedier code...

      if ( nullArray == 0 || iOrdinal == -1 )
         return ( (rowNullBitPosition == -1) ? 0 : ( s5fox ? ( isNullableFieldNull( buf, rowNullBitPosition ) ) : ( rowNullBitPosition == 1 ) ) ) ;
      else
      {
         #ifdef E4DEBUG
            assert5( iOrdinal >= 0 && iOrdinal < arraySize ) ;
         #endif
         return nullArray[iOrdinal] ;
      }
   }
   void nullSet( char *buf, ULONG rowNullBitPosition ) ;
   void nullReset( char *buf, ULONG rowNullBitPosition ) ;

   // function for clipper/mdx to support 'always null' fields for schema tables...
   virtual void nullSchemaSetup( FIELD4INFO *fieldInfo ) ;
private:
   // for mdx/clipper, use an array of 'always null' fields to support null values within
   // schema tables.  Failure to return 'null' for some schema table values results in engine
   // conformance failures (i.e. ole-db requires nullable fields or we must support the fields
   // fully, blank is not acceptable).

   // nullArray not as compressed as possible, but speed is more important than slight space
   // savings here.
   Bool5 *nullArray ;
   #ifdef E4DEBUG
      long arraySize ;
   #endif

   inline void nullSchemaSetNull( long alwaysNullFieldOrdinal )
   {
      #ifdef E4DEBUG
         assert5( alwaysNullFieldOrdinal < arraySize ) ;
      #endif
      nullArray[alwaysNullFieldOrdinal] = 1 ;
   }
} ;



class Accessor5handler : public Row5bufControl, private Null5buffer
{
   // this is seperated out of the rowset object because for integrated indexes we need 2 of
   // these - 1 handler for the table-level accessors, and 1 for the index-level accessors
public:
   Accessor5handler( Session5 *sessionParm, int isIndexParm, int isIntegratedIndexParm ) ;
   ~Accessor5handler() ;
   List5             accessorsNew ;   // The list of accessors created since 'fieldsetCurrent' was last updated
   List5             accessors ;      // Current list of Accessor5all
                                      // or that are only for seeking/setrange in the case of 'index' rowsets.
   void accessorsNewCheckAndUpdateFieldset( Accessor5all *accessorNow, int isNullable, int *iRowset ) ; // Transfers 'accessorsNew' -> 'accessors', Updates 'fieldsetCurrent'
   Fieldset5all     *fieldsetCurrent; // pointer to the Fieldset5 defining the fields
                                      // used by currently defined accessors; determines what to fetch.
                                      // Always add fields, never remove.
                                      // Initialized to all fields under single user.
   Table5all        *tableAll ;   // may point to a Table5 or an Index5 depending on rowset type
   List5             fieldsetOld ;    // List of potential field sets used in Row5's; needed to free the Row5 memory only.
   inline Row5bufField *field( int iOrdinal ) { return fieldsetCurrent->field(iOrdinal) ; }
   inline Row5bufField *field( int iOrdinal, DBTYPE wType ) { return fieldsetCurrent->field(iOrdinal, wType) ; }
   inline Row5bufField *fieldEntry( int iEntry ) { return fieldsetCurrent->fields + iEntry ; }
   inline ULONG nEntries( int iOrdinal ) { return fieldsetCurrent->iField[iOrdinal].nEntries() ; }
   inline ULONG nEntries() { return fieldsetCurrent->numFields() ; }
   virtual inline int isFieldDeferred( int iOrdinal ) ;

   int               doCheckNewList ; // An accessor was added or initialized & it needs to be checked.
   Session5 *session ;

   inline unsigned getNumFields() { return nFields ; }  // returns # fields in rowset - for index rowset or index accessor of integrated index, this is # columns in index
   inline void setNumFields( unsigned nFieldsSet ) { nFields = nFieldsSet ; }

   inline int nullTest( char *buf, ULONG rowNullBitPosition, long iOrdinal )
   {
      return
      (
         // AS 08/25/99 -- cannot do this test here, for non-fox, use nullArray sometimes...
         // ( rowNullBitPosition == ULONG(-1) ) ?
         //    0
         // :
         //    (
               isIndex ?
                  nullTestIndex( buf, rowNullBitPosition )
               :
                  Null5buffer::nullTest( buf, rowNullBitPosition, iOrdinal )
         //    )
      ) ;
   }
   inline void nullSet( char *buf, ULONG rowNullBitPosition )
   {
      (isIndex ?
         nullSetIndex( buf, rowNullBitPosition )
      :
         Null5buffer::nullSet( buf, rowNullBitPosition )
      ) ;
   }
   inline void nullReset( char *buf, ULONG rowNullBitPosition )
   {
      (isIndex ?
         nullResetIndex( buf, rowNullBitPosition )
      :
         Null5buffer::nullReset( buf, rowNullBitPosition )
      ) ;
   }

   inline int isNullableFieldNull( char *buf, ULONG rowNullBitPosition ) { return Null5buffer::isNullableFieldNull( buf, rowNullBitPosition ) ; }
   int isIntegratedIndexIndex ;    // true if the handler is handling index accessor for integrated index accessors, else false

   int indexRowLen; // Initialized to 0 which means that the session memory allocator can be used.
                   // Otherwise it is the dynamic amount of memory that currently needs to be
                   // allocated to hold the row5index structure, the key, provider memory conversions according to the
                   // current fieldset and the record number.
   inline void nullSchemaSetup( FIELD4INFO *fieldInfo ) { Null5buffer::nullSchemaSetup( fieldInfo ) ; }
        Collate4name collateName() ;
private:
   inline void nullResetIndex( char *buf, ULONG nullPosition ) { assert5( nullPosition >= 0 ) ; buf[nullPosition] = (char)0x80 ; }
   inline void nullSetIndex( char *buf, ULONG nullPosition ) { assert5( nullPosition >= 0 ) ; buf[nullPosition] = 0x00 ; }
   inline int nullTestIndex( char *buf, ULONG nullPosition )
   {
      // nullPosition is the byte position (must take into account 4 bytes for recno, for example)
      // AS 08/25/99 -- cannot do this test here, for non-fox, use nullArray sometimes...
      // assert5( nullPosition != ULONG(-1) ) ; // AS 08/23/99 should be caught earlier for speedier code...
      return ( ( (nullPosition != ULONG(-1)) && (nullPosition != index4nullValue) ) ? ( ( buf[nullPosition] == 0x00 ) ? 1 : 0 ) : 0 ) ;
   }
   unsigned nFields ;    // The total number of fields in the data file including one for the delete field and one for the recno field.  for index rowsets this is a different value representing index

   int isIndex ;    // true if the handler is handling index accessors, else false
} ;



// This is for all of the rowset functionality for Index, Rowset & Schema.
// The base class Row5bufControl is for the row buffer.
class5 Rowset5all : private IAccessor, private IColumnsInfo, private IConvertType, private ISupportErrorInfo,
                    public IRowset, private IRowsetInfo, public Unknown5
{
public:
   Rowset5all( Session5 *sess, int isIndex ) ;
   ~Rowset5all() ;

   DECLARE5_IAccessor
   DECLARE5_IColumnsInfo
   DECLARE5_IConvertType
   DECLARE5_IRowset
   DECLARE5_IRowsetInfo
   DECLARE5_ISupportErrorInfo
   DECLARE5_IUnknown

   virtual int isNullable() { return 0 ; }  // only index rowsets which override this are nullable - i.e. is the whole row nullable or not?
   virtual ULONG startOrdinal() { return 0 ; }   // most rowset types have ordinal 0 for bookmark (but not index or schema)

   int doAllFields() ; // Returns True/False;  True for standalone configuration & schemas rowsets.
   virtual HRESULT getHandles( LONG offset, LONG cRecno, ULONG *nRowsObtained, Row5all **hRows, Bool5 updateNextRecNo, char countDeletedAsSkip ) = 0 ;
   virtual void freeRow( Row5all *row ) = 0 ;  // delete function must be specific to Row5/Row5index
   virtual Rowset5properties *properties() = 0 ;
   virtual Accessor5all *createAccessor() = 0 ; // { return new Accessor5all ; }

   #ifdef S4CONFORMANCE
      HRESULT verifyAccessor( HACCESSOR hAccessor ) ;
      Bool5 rowsFetched ;   // required for a non-relevant condition to be checked
   #endif
   virtual HRESULT verifyRowHandle( Row5all *row ) = 0 ;

   #ifdef S4OLEDEBUG_PRINT
      void log5getData( HROW hRow, HACCESSOR hAccessor, const char *pData ) ;
      virtual const char *rowsetName() = 0 ;
   #endif
   HRESULT verifySetIUnKnown(REFIID riid, IUnknown *punkOuter);
   HRESULT releaseRows( ULONG cRows, const HROW __RPC_FAR rghRows[], DBROWOPTIONS __RPC_FAR rgRowOptions[], ULONG __RPC_FAR rgRefCounts[], DBROWSTATUS __RPC_FAR rgRowStatus[]) ;

   virtual Bool5 hasIndexAvailable() { return 0 ; }  // is an index tag available from the rowset? - usually no except for integrated index rowsets with indexes
   virtual Bool5 isIntegratedIndexRowset() { return 0 ; }  // be default not index integrated, only overrident for index integrated rowsets

   Accessor5handler accessorHandler ;
   Session5         *session ;
   Table5rowsetValues rowsetValues ;
   IUnknown         *pUnkOuter ;
   Rowset5Interface *outerControl ;
   int iRowset ;
} ;



class5 Key5
{
public:
   Key5() { key = 0 ; keyLen = 0 ; recNo = 0 ; }
   ~Key5() { if ( key ) free5( key ) ; }

   void *key ;
   short keyLen ;
   unsigned long recNo ;
} ;



class5 Rowset5indexPlusIntegratedIndex
{
   // contains index functionality common between Rowset5index and Rowset5integratedIndex
public:
   inline short keyLen() ;
   inline void refetchWithKey( unsigned long recNo, char *keyBuf ) ;
   inline int rowGet( int direction, unsigned long *recNo, void *key, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData ) ;
   inline int isDeleted() ;   // True if filter expression is .NOT. DELETED()
   inline int isNullable() ;

   // currently must be public because set by Index5 class
   Key5 currentKey ;
   Index5rangeInfo rangeData ;

   HRESULT STDMETHODCALLTYPE Seek( HACCESSOR hAccessor, ULONG cKeyValues, void *pData, DBSEEK dwSeekOptions ) ;
   HRESULT STDMETHODCALLTYPE SetRange( HACCESSOR hAccessor, ULONG cStartKeyColumns, void *pStartData, ULONG cEndKeyColumns, void *pEndData, DBRANGE dwRangeOptions ) ;
   HRESULT STDMETHODCALLTYPE GetIndexInfo( ULONG *pcKeyColumns, DBINDEXCOLUMNDESC **prgIndexColumnDesc,  ULONG *pcIndexProperties, DBPROPSET **prgIndexProperties ) ;

   virtual HRESULT STDMETHODCALLTYPE RestartPosition( HCHAPTER hReserved ) = 0 ;
   HRESULT getHandlesPosition( LONG offset, LONG cRecno, ULONG *nRowsObtained, Row5all **hRows, Table5rowsetValues *rowsetValues, char countDeletedAsSkip = 0 ) ;
   void rowPosition( long lRowsOffset, int direction, Table5rowsetValues *rowsetValues ) ;

   virtual inline Session5 *getSession() = 0 ;
   virtual Table5rowsetValues *tableRowsetValues() = 0 ;
   virtual Rowset5indexProperties *getIndexProperties() = 0 ;
   virtual short recnoOffset() = 0 ;   // amount of offset if reqd. for holding recno at start of row (for non-integrated)
   virtual int sendKeys() = 0 ;
   virtual Bool5 isIntegratedIndexRowset() = 0 ;
   void    remove() ;

   Index5 *index ;
   int *iRowset ;  // points to iRowset from Rowset5all
protected:
   Rowset5indexPlusIntegratedIndex() ;
   ~Rowset5indexPlusIntegratedIndex() ;

//   virtual COLUMN5INFO *getIndexColumns( short *nColumns ) = 0 ;
   virtual COLUMN5INFO *columnsRowset( short *nColumns ) = 0 ;

//   int nullTest( char *buf, ULONG nullPosition ) ;
//   void nullSet( char *buf, ULONG nullPosition ) ;  // required for seeking
//   void nullReset( char *buf, ULONG nullPosition ) ;  // required for seeking

   void getKeyFromAccessor( HACCESSOR hAccessor, ULONG cStartKeyColumns, char **ppKey, ULONG *pKeyLen, void *pData, ULONG *pLenConverted ) ;
   HRESULT seek( void *keyValue, int keyValueLen, DBSEEK seekOptions ) ;

   inline void rangeRestore()  { range5copy( &rangeData, &savedRange ) ; }
   inline void rangeSave() { range5copy( &savedRange, &rangeData ) ; }
   void restoreOldPosition( Table5rowsetValues *rowsetValues ) ;
   void saveOldPosition( Table5rowsetValues *rowsetValues ) ;
   HRESULT verifyRangeOptions( DBRANGE dwRangeOptions, HACCESSOR hAccessor, int startKeyCols ) ;
   HRESULT verifySeekOptions( DBSEEK dwSeekOptions ) ;

   char *keyWorkingMemory ;
   char *keyWorkingMemory2 ;
   ULONG keyWorkingMemoryLen ;
   ULONG keyWorkingMemory2Len ;
   WSTR5 *storedIndexId ;
   HRESULT verifyAccessorBindings( HACCESSOR hAccessor, ULONG cKeyValues, void *pData ) ;
   Index5rangeInfo savedRange ;
private:
//   virtual void getKeyFromAccessor( HACCESSOR hAccessor, ULONG cStartKeyColumns, char **ppKey, void *pData, ULONG *pLenConverted ) = 0 ;
//   virtual HRESULT verifyAccessorBindings( HACCESSOR hAccessor, ULONG cKeyValues, void *pData ) = 0 ;
   Key5 oldKey ;
   #ifndef MSQL7COMPATABILITY
      // ADO frees up this memory despite documentation not mentioning it, so leave it for them...
      DBID *indexInfoIdColumns ;   // used to keep memory for index info
      int indexInfoIdNumColumns ;
   #endif
} ;



class5 Rowset5index : public Rowset5all, public Rowset5indexPlusIntegratedIndex, public IRowsetIndex
{
   // Corresponds to an OLE DB index rowset.
   // For transactions, the corresponding data file is locked.
public:
   Rowset5index( Session5 *sess ) ;
   ~Rowset5index() ;

   void * operator new( size_t s ) { void *ptr = alloc5( s ) ; return ptr ; }
   void operator delete( void *p ) { free5(p) ; }

   DECLARE5_IUnknown

   inline HRESULT STDMETHODCALLTYPE Seek( HACCESSOR hAccessor, ULONG cKeyValues, void *pData, DBSEEK dwSeekOptions )
      {
         return Rowset5indexPlusIntegratedIndex::Seek( hAccessor, cKeyValues, pData, dwSeekOptions ) ;
      }
   inline HRESULT STDMETHODCALLTYPE SetRange( HACCESSOR hAccessor, ULONG cStartKeyColumns, void *pStartData, ULONG cEndKeyColumns, void *pEndData, DBRANGE dwRangeOptions )
      {
         return Rowset5indexPlusIntegratedIndex::SetRange( hAccessor, cStartKeyColumns, pStartData, cEndKeyColumns, pEndData, dwRangeOptions ) ;
      }
   inline HRESULT STDMETHODCALLTYPE GetIndexInfo( ULONG *pcKeyColumns, DBINDEXCOLUMNDESC **prgIndexColumnDesc,  ULONG *pcIndexProperties, DBPROPSET **prgIndexProperties )
      {
         return Rowset5indexPlusIntegratedIndex::GetIndexInfo( pcKeyColumns, prgIndexColumnDesc, pcIndexProperties, prgIndexProperties ) ;
      }
   // DECLARE5_IRowsetIndex  -- Some here, some in integrated...

   // get index info must be specific to Rowset5index because info is different for integrated indexes

   Table5 *tableDbf ;    // pointer to the dbf table corresponding to this index

   int sendKeys() { return doSendKeys ; }
   int doSendKeys ;

   // AS 01/09/00 - added tracking of rows for freeing purposes (t5index1.cpp)
   List5 rowList ;

   inline int isNullable() { return Rowset5indexPlusIntegratedIndex::isNullable() ; }
   /* also reflects some row property settings into rowset set by setRowProperties function*/
   // properties are specific to RowsetIndex, integrated indexes have different properties
   Rowset5indexProperties indexProperties ;
   HRESULT setRowProperties( ULONG cPropertySets, DBPROPSET __RPC_FAR rgPropertySets[] ) ;
   HRESULT checkSetRowProperties(REFIID riid, LPVOID *ppv);
   inline Rowset5properties *properties() { return &indexProperties ; }
   inline Rowset5indexProperties *getIndexProperties() { return &indexProperties ; }
//   inline COLUMN5INFO *getIndexColumns( short *nColumns ) ;
   inline COLUMN5INFO *columnsRowset( short *nColumns ) ; //{ return index->columns( nColumns ) ; }  // the rowset is the same as the index for Rowset5index


   short recnoOffset() { return sizeof( long ) ; }  // offset 4 bytes into key for holding the record number
   void    close() ;
   //   void    dataSet( Row5all *, char *pConsumerBuffer, char *rowWorkingBuffer, char *pRowBuffer ) ;

   // row handling functions are specific to RowsetIndex, integrated indexes use base table rows instead
   inline void freeRow( Row5all *row ) ;
   HRESULT getHandles( LONG offset, LONG cRecno, ULONG *nRowsObtained, Row5all **hRows, Bool5 updateNextRecNo, char countDeletedAsSkip ) ;
   ULONG   startOrdinal() { return 1 ; }   // index rowsets don't have bookmark fields... or at least not implemented yet...
   Accessor5all *createAccessor() { return new Accessor5index( 0 ) ; }
   HRESULT verifyRowHandle( Row5all *row ) ;

   #ifdef S4OLEDEBUG_PRINT
      const char *rowsetName() { return rowsetNameForLog ; }
      char rowsetNameForLog[LEN4PATH*2] ;
   #endif
   int     open( Session5 *session, const LPCWSTR dataName, const LPCWSTR indexName ) ; // See Also: Session5::OpenRowset
   HRESULT STDMETHODCALLTYPE RestartPosition( HCHAPTER hReserved ) ;

   inline Session5 *getSession() { return session ; }
   Table5rowsetValues *tableRowsetValues() { return &rowsetValues ; }
   Bool5 isIntegratedIndexRowset() { return 0 ; }
private:
   void getKeyFromAccessor( HACCESSOR hAccessor, ULONG cStartKeyColumns, char **ppKey, ULONG *pKeyLen, void *pData, ULONG *pLenConverted ) ;
//   void rowPosition( long lRowsOffset, int direction, Table5rowsetValues *rowsetValues ) ;
   HRESULT verifyAccessorBindings( HACCESSOR hAccessor, ULONG cKeyValues, void *pData ) ;
} ;



class5 RowList5 : public List5  // list of Row5
{
   // class RowList5 is used with isIdentity, in which case the row list is ordered
   // and has no duplicates
public:

   RowList5() { isIdentity = 0 ; rowListIsSorted = 0 ; otherRowList = 0 ; }

   // the following functions are used for Rowsets supporting IRowsetIdentity
   void add( Row5 *row ) ;
   void addDo( Row5 *row ) ;     // special add operation
   inline Row5all *find( unsigned long recno )  // speed optimization
      { return ( isIdentity ? findIdentityTrue( recno ) : 0 ) ; }
   Row5all *findIdentityTrue( unsigned long recno ) ;    // find
   void sort() ;    // needed when have an unsorted list
   int isIdentity ;
   inline Link5 *addAfter( Link5 *anchor, Link5 *item )  { throw Err5internal(0) ; return NULL ; }  // must go through our add()
   inline Link5 *addBefore( Link5 *anchor, Link5 *item ) { throw Err5internal(0) ; return NULL ; }  // must go through our add()
   inline void setOtherList( RowList5 *oth ) { otherRowList = oth ; }
   HRESULT find( Row5all *row ) ;

private:
   int rowListIsSorted ;
   RowList5 *otherRowList ;  // occasionaly act in conjunction
} ;



class5 RowsetPlusSchema5 : public Rowset5all, private IRowsetIdentity
{
public:
   inline void * operator new( size_t s ) { return alloc5null( s ) ; }
   void operator delete( void *p ) { free5(p) ; }
   Table5 *tableDbf ;
   DECLARE5_IUnknown
   DECLARE5_IRowsetIdentity

   // also need to call rowProperties.fInit() and then manually reset memo fields properties to deferred - how in c/s?
   RowsetPlusSchema5( Session5 *sess ) : Rowset5all( sess, 0 )
      { nextRecNo = 0 ; rowFreeOKflag = 0 ; byRefBuffer = 0 ; }

   ~RowsetPlusSchema5() ;

   virtual HRESULT flush() { return S_OK ; } // required for deconstructor before freeing rowlist handles, if rowset5, override (i.e. if can flush rows)
   void initOnCreate() ;

   virtual inline ULONG baseTableCount() ;  // overridden for static tables....

   // following are the functions implemented commonly for both Schema5rowset and Rowset5 support
   virtual void freeRow( Row5all *row ) ;
   virtual HRESULT getHandles( LONG offset, LONG cRecno, ULONG *nRowsObtained, Row5all **hRows, Bool5 updateNextRecNo, char countDeletedAsSkip ) ;  // virtual because schema overrides
   void getHandles( ULONG **recNoArray, LONG cRecNo, Row5all **hRows, Bool5 neexNextRecno, const ULONG __RPC_FAR rgcbBookmarks[] ) ;
   virtual inline int isDeletedExposed() { return !properties()->getREMDELM() ; } // ?? Attach to properties later
   refIncrement(  Row5 *hRow ) ;
   refDecrement(  Row5 *hRow ) ;
   void rowFreeOK( int trueFalseFlag ) ; // In some cases there may be accessors which give pointers
                   // to the provider's row data.  (See DBBINDINGS.dwMemOwner.)  The default value is
                   // false.  When a row is created it should never free its data values if the
                   // value of this flag is currently true.
   virtual HRESULT STDMETHODCALLTYPE RestartPosition( HCHAPTER hReserved) ;

   long  nextRecNo, saveNextRecNo ; //Needs to be in stand-alone and server code // The next Position, initialized to '0' to indicate just created state.
                     // Used in GetNextRows only.
   char saveLastDirection ;   // which direction were we going on last getNextRows request?  (affects next record position)
   int rowFreeOKflag ;
   virtual inline Accessor5all *createAccessor()  { return new Accessor5rowset ; }

   RowList5 rowList ;
   HRESULT verifyRowHandle( Row5all *row ) ;

   #ifdef S4OLEDEBUG_PRINT
      virtual const char *rowsetName() = 0 ;
   #endif

   int getDeferred( Accessor5 *hAccessor, Row5 *hRow ) ; // Andrew
   virtual Row5 *getExistingRow( long recNo ) { return NULL ; }   // be default no existing rows (for Rowset5 class only )
   Rowset5properties *properties() { return &rowProperties ; }
   Rowset5properties rowProperties ;

   void saveOldPosition() ;
   void restoreOldPosition() ;

   // for schema, rows might not be in range, can check buffer contents or row #, depending on implementation
   inline virtual char verifyRowInRange( char *raw, long recNo, Table5rowsetValues *rowsetValues ) { return 1 ; }
   virtual void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char countDeletedAsSkip = 0 ) = 0 ;
   virtual int rowRefetch( ULONG recNo, char **raw, ULONG *recNoOut ) = 0 ;

   ByRef5 *byRefBuffer ;
protected:
   int readNextRow( ULONG *count, ULONG *recNo, const ULONG uRecNo, const int increment, Row5all **hRows ) ;
} ;



class5 Rowset5 : public RowsetPlusSchema5, private IRowsetUpdate, private IRowsetScroll
{
// Corresponds to the OLE DB Rowset object.
// not directly allocated since static member of RowSet5client or RowSet5stand
// S4CLIENT, S4STAND_ALONE
public:
//   void * operator new( size_t s, Session5 *sess ) { void *ptr = alloc5(s) ; memset( ptr, 0, s); return ptr ; }
   inline void * operator new( size_t s ) { return alloc5null( s ) ; }
   void operator delete( void *p ) { free5(p) ; }
   Rowset5( Session5 *sess ) ;
  ~Rowset5() ;

   DECLARE5_IUnknown
   DECLARE5_IRowsetChange
   DECLARE5_IRowsetLocate
   DECLARE5_IRowsetScroll
   DECLARE5_IRowsetUpdate
//   DECLARE5_IRowsetFind

   HRESULT update( HCHAPTER hReserved, ULONG cRows, const HROW rghRows[], ULONG *pcRows,
                   HROW **prgRows, DBROWSTATUS **prgRowStatus ) ;

   void close() ; // Andrew (virtual/cached close, free up everything, etc.
   void open( Session5 *sessionIn, const LPCWSTR name ) ; // 0 - OK; r4noOpen - Could not open.
   void create( Session5 *sessionIn, const LPCWSTR name, FIELD4INFO *fields ) ;

   // for these functions, only one accessor pointer is used, and it is just copied over
   void rowRestoreFromOriginal( Row5 *hRow ) ;  // copy from original
   void rowSaveToOriginal( Row5 *hRow ) ; // Makes copy because the current is going to be changed
                     // Also moves to list of potentially changed rows

   // Allocates memory for a brand new row.
   // All that will need to be done is the setting of data and the flush.
   // Returns null pointer on out of memory.
   Row5 *appendAlloc() ;

   // AS 09/22/99 added insert boolean - more efficient
   HRESULT setDataLow( HROW hRow, Accessor5all *accessor, void *pData, Bool5 setFromInsert ) ;

   // Returns HRESULT in accordance with IRowsetChange::DeleteRows, ..
   HRESULT flush() ; // Flushes list of changed rows.  Frees "original" memory.
             // For use when RowsetUpdate stuff is in use.
             // For use when deleting or inserting.

   inline HRESULT flush( HROW *row, DBROWSTATUS **prgRowStatus = 0 )  // used to flush a single row (used when RowsetUpdate stuff not in use
      { return update( 0, 1, row, 0, 0, prgRowStatus ) ; }

   // the following functions are virtual, and are dependent on the implementation:
   // either RowSet5client or RowSet5stand

   // - Returns non-error return codes in accordance with IRowset::GetNextRows,
   // IRowsetLocate::GetRowsAt and IRowsetLocate::GetRowsByBookmark respectively.
   // - Throws errors.
   // If 'startRecNo' is zero, use an internal next position.
   // If 'startRecNo' is ULONG_MAX, use the last record.
   // getHandle( long recNo, Row5 **hRow )

   int rowGetNonDeferred() ; // Andrew
   void setChanged( Row5 *row, DBPENDINGSTATUS stat ) ;
   // AS 09/13/99 changed to just contain a flag for performance reasons...
   // inline int isImmediateUpdateMode() { return !properties()->getIRU() ; }  // ?? Needs updating with properties.  Refer to IRowSetChange::DeleteRows & property for turning off IRowsetUpdate interface.
   inline int isImmediateUpdateMode() { return isImmediateUpdateMd ; }  // ?? Needs updating with properties.  Refer to IRowSetChange::DeleteRows & property for turning off IRowsetUpdate interface.
   Bool5 selectTag( char *tagName ) ;

   // overridden by integrated index...
   virtual void setStartPosition( long startPos ) { nextRecNo = startPos ; }
   // statusArray contains the row status after a call to rowGet()
   // the results are only valid until the next call to rowGet()
//KS Is this still needed   StatusType *statusArray ; // One entry per field on an ordinal array index basis

   char exposeDeleted ; // On the call to getHandles( long startRecNo, long cRecNo )
                        // it determines whether to filter out deleted records.
                        // Also on the call to getHandles( long *recNoArray, ..)
                        // it determines whether to send back an error code or the
                        // record on deleted records.
                        // This variable is set and read directly.

   char areAllFieldsBound ;  // can make optimizations if known that all fields are bound

   void removeIndexes() ;
   RowList5 rowListChanged ;
   HRESULT verifyRowHandle( Row5all *row ) ;

   #ifdef S4OLEDEBUG_PRINT
      const char *rowsetName() { return rowsetNameForLog ; }
      char rowsetNameForLog[LEN4PATH*2] ;
   #endif

   // ?? Code in rowset5.cpp is commented out.  short columnPropSet(short iProp, short columnID, BOOL value) ;
   //checkLength( int, char *, Accessor5 * ) ;
   //checkStatus( int, char *, Accessor5 * ) ;
   //checkValue( int, char *, Accessor5 * ) ;

   int rowRefetch( ULONG recNo, char **raw, ULONG *recNoOut ) ;
   inline void freeRow( Row5all *row ) { RowsetPlusSchema5::freeRow( row ) ; }

   // following required for iRowsetLocate
   inline HRESULT STDMETHODCALLTYPE AddRefRows( ULONG cRows, const HROW __RPC_FAR rghRows[], ULONG __RPC_FAR rgRefCounts[], DBROWSTATUS __RPC_FAR rgRowStatus[] )
      { return Rowset5all::AddRefRows( cRows, rghRows, rgRefCounts, rgRowStatus ) ; }
   inline HRESULT STDMETHODCALLTYPE GetData( HROW hRow, HACCESSOR hAccessor, void __RPC_FAR *pData )
      { return Rowset5all::GetData(hRow, hAccessor, pData) ; }
   HRESULT STDMETHODCALLTYPE Rowset5::GetNextRows( HCHAPTER hReserved, LONG lRowsOffset, LONG cRows, ULONG __RPC_FAR *pcRowsObtained, HROW __RPC_FAR *__RPC_FAR *rows) ;
   inline HRESULT STDMETHODCALLTYPE Rowset5::ReleaseRows( ULONG cRows, const HROW __RPC_FAR rghRows[], DBROWOPTIONS __RPC_FAR rgRowOptions[], ULONG __RPC_FAR rgRefCounts[], DBROWSTATUS __RPC_FAR rgRowStatus[])
      { return Rowset5all::ReleaseRows( cRows, rghRows, rgRowOptions, rgRefCounts, rgRowStatus ) ; }
   inline HRESULT STDMETHODCALLTYPE Rowset5::RestartPosition( HCHAPTER hReserved)
      { return RowsetPlusSchema5::RestartPosition(hReserved) ; }
   void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char countDeletedAsSkip = 0 ) ;

   HRESULT setRowProperties( ULONG cPropertySets, DBPROPSET __RPC_FAR rgPropertySets[] ) ;  // also reflects some row property settings into rowset
   HRESULT checkSetRowProperties(REFIID riid, LPVOID *ppv);
   // set by setRowProperties function
   Row5 *getExistingRow( long recNo ) ;
   #ifdef S4STAND_ALONE
      // AS Feb 20/02 - customized work to select a default index if not provided...
      const char *getDefaultIndex() ;
   #endif
protected:
   Bool5 isImmediateUpdateMd ;
private:
   #ifdef S4STAND_ALONE
      Table5stand *tableStand ;  // is 0 if not available...
   #endif
   #ifdef S4CLIENT
      Table5client *tableClient ;  // is 0 if not available...
   #endif
   inline int rowHandleInvalid( Row5 *row ) ;

   long getChangedRows( ULONG cRows, HROW **rows, HROW **prgRows, const HROW rghRows[], int *didAllocRowHandles ) ;
   void initInputRowsForUpdate( HROW **prgRows, const HROW rghRows[], long count ) ;
   void initAllRowsForUpdate( HROW *rows ) ;
   HROW *allocRowsForUpdate( long count, int *didAllocRowHandles ) ;
   long countChangedRows() ;
   DBROWSTATUS *initStatus( DBROWSTATUS **prgRowStatus, long count ) ;
   void checkRowsDeletedStatus( long count, HROW *rows ) ;
   int rowIsChanged( Row5 *row ) ;
   void writeRows( long count, HROW *rows, Fieldset5all *fieldsetOn ) ;
   void initializeErrorRows( long nError, Result5update *errorList, HROW *rows, Fieldset5all *fieldsetOn ) ;
   void incrementRefCountForOutputRows( const HROW rghRows[], ULONG *pcRows, HROW *rows, long count ) ;
   long setOutputStatus( long count, HROW *rows, DBROWSTATUS *status ) ;
   long lastRowFound ;
} ;
#endif /* S4SERVER */



class Column5info
{
public:
   Column5info() { cols = 0 ; nCols = 0 ; freeIsRequired = 0 ; }
   ~Column5info() { freeColumnInfo() ; }
   inline COLUMN5INFO *columns( short *nColumns = 0 )
   {
      if ( cols == NULL )
      {
         cols = getColumns( &nCols, &freeIsRequired ) ;
         assert5( cols != NULL ) ;
      }
      if ( nColumns != 0 )
         *nColumns = nCols ;
      return cols ;
   }
   inline short nColumns()
   {
      if ( cols == NULL )
      {
         cols = getColumns( &nCols, &freeIsRequired ) ;
         assert5( cols != NULL ) ;
      }
      return nCols ;
   }
   // AS 06/07/99 --> when adding/removing columns, need to free up this info to regenerate later...
   void freeColumnInfo()
   {
      if ( cols != 0 && freeIsRequired )
      {
         i5columnsFree( cols, nCols ) ;
         cols = 0 ;
         nCols = 0 ;
         freeIsRequired = 0 ;
      }
   }
private:
   virtual COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) = 0 ;
   COLUMN5INFO *cols ;
   short nCols ;
   int freeIsRequired ;
} ;


#ifndef S4SERVER
// class5 Rowset5integratedIndex ;


class5 Rowset5integratedIndex : public Rowset5indexPlusIntegratedIndex, public Rowset5, public IRowsetCurrentIndex
{
   // adds integrated index operations on top of a Rowset5 object...
public:
   void * operator new( size_t s ) { void *ptr = alloc5(s) ; memset( ptr, 0, s); return ptr ; }
   void operator delete( void *p ) { free5(p) ; }

   Rowset5integratedIndex( Session5 *sess ) : Rowset5( sess )
      {
         //outerControl= new Rowset5Interface() ;
         assert5(outerControl != 0) ;
         outerControl->pRowset5 = this ;
      }
  ~Rowset5integratedIndex() { ; }

   DECLARE5_IRowsetCurrentIndex
   DECLARE5_IUnknown
//   DECLARE5_IRowsetIndex  - some of these functions are in Rowset5indexPlusIntegratedIndex, so don't expose all...

   /* some interfaces not supported yet for integrated indexes...
      DECLARE5_IRowsetChange
      DECLARE5_IRowsetScroll
      DECLARE5_IRowsetUpdate
   */

    // integrated have recno at end of key, so no offset required
    // I belive changed so it is at begining, thus offset of 4,
   short recnoOffset() { return sizeof( long ) ; }
   inline HRESULT STDMETHODCALLTYPE Seek( HACCESSOR hAccessor, ULONG cKeyValues, void *pData, DBSEEK dwSeekOptions )
      {
         return Rowset5indexPlusIntegratedIndex::Seek( hAccessor, cKeyValues, pData, dwSeekOptions ) ;
      }
   inline HRESULT STDMETHODCALLTYPE SetRange( HACCESSOR hAccessor, ULONG cStartKeyColumns, void *pStartData, ULONG cEndKeyColumns, void *pEndData, DBRANGE dwRangeOptions )
      {
         return Rowset5indexPlusIntegratedIndex::SetRange( hAccessor, cStartKeyColumns, pStartData, cEndKeyColumns, pEndData, dwRangeOptions ) ;
      }
   inline HRESULT STDMETHODCALLTYPE GetIndexInfo( ULONG *pcKeyColumns, DBINDEXCOLUMNDESC **prgIndexColumnDesc,  ULONG *pcIndexProperties, DBPROPSET **prgIndexProperties )
      {
         return Rowset5indexPlusIntegratedIndex::GetIndexInfo( pcKeyColumns, prgIndexColumnDesc, pcIndexProperties, prgIndexProperties ) ;
      }

   // index info is specific to integrated indexes

   inline int isNullable() { return Rowset5indexPlusIntegratedIndex::isNullable() ; }
   HRESULT STDMETHODCALLTYPE RestartPosition( HCHAPTER hReserved ) ;
   Rowset5indexProperties indexProperties ;
   HRESULT setRowProperties( ULONG cPropertySets, DBPROPSET __RPC_FAR rgPropertySets[] ) ;
   HRESULT checkSetRowProperties(REFIID riid, LPVOID *ppv);
   Rowset5properties *properties() { return &indexProperties ; }
   inline Rowset5indexProperties *getIndexProperties() { return &indexProperties ; }
   virtual Bool5 hasIndexAvailable() ;  // look at base table to see if a tag exists
   Bool5 hasTagSelected() ;  // have we selected a tag on the integrated rowset?
   inline COLUMN5INFO *columnsRowset( short *nColumns ) ;
   int open( Session5 *session, const LPCWSTR dataName, const LPCWSTR indexName ) ; // See Also: Session5::OpenRowset
   Accessor5all *createAccessor() { return new Accessor5integrated ; }
   HRESULT getHandles( LONG offset, LONG cRecno, ULONG *nRowsObtained, Row5all **hRows, Bool5 updateNextRecNo, char countDeletedAsSkip ) ;
   int sendKeys() { return 0 ; }   // we want the rows, not the keys, irrelevant here
   void setStartPosition( long startPos ) ;

   Bool5 isIntegratedIndexRowset() { return 1 ; }

   inline Session5 *getSession() { return session ; }
   Table5rowsetValues *tableRowsetValues() { return &rowsetValues ; }
private:
//   HRESULT verifyAccessorBindings( HACCESSOR hAccessor, ULONG cKeyValues, void *pData ) { throw Err5internal() ; } // not coded yet, need to figure what to do here...
//   void getKeyFromAccessor( HACCESSOR hAccessor, ULONG cStartKeyColumns, char **ppKey, void *pData, ULONG *pLenConverted ) { throw Err5internal() ; } // not coded yet, need to figure what to do here...
} ;

#endif /* S4SERVER */



int extracts5name( LPCWSTR tblName, LPCWSTR idxName, LPWSTR *tableName, LPWSTR *indexName, Bool5 removeNonIntegratedMarker ) ;



#endif /* OLEDB5BUILD */
