// blast5.hpp

#ifdef OLEDB5BUILD

#ifndef S4OFF_BLAST

// not coded for server yet...
#ifndef S4SERVER

class Blast5fieldLow
{
public:
   // all field blasts require these operations
   virtual int read( void *ptr, long numBytes, long *bytesRead ) ;
   virtual int write( void *ptr, long numBytes, long *bytesWrote ) ;
   virtual void initRead( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
   virtual void initUndo() ;
   virtual void initWrite( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
} ;

class Blast5field
{
public:
   // for constructor allocation, use Session5.blastMemory

   // this class is used to perform field-level blasts
   // memo only currently supported; later maybe file-name field

   CODE4 *c4 ;

   Blast5fieldLow *blast ;  // actual blast implementation; blast5fileMemo; blast5socketMemo; ...

   Blast5field( CODE4 *code4 ) { c4 = code4 ; }
   ~Blast5field() { initUndo() ; }

   int createBlast() ;
   int initRead( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
   void initUndo() ;
   int initWrite( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
   int read( void *ptr, long numBytes, long *bytesRead )  { return blast->read( ptr, numBytes, bytesRead ) ; }
   int write( void *ptr, long numBytes, long *bytesWrote ) { return blast->write( ptr, numBytes, bytesWrote ) ; }
} ;

class Blast5memo : Blast5fieldLow
{
   // a blast for a memo field
   // no code, just a place-holder
} ;

class Blast5memoFile : Blast5memo
{
public:
   // for constructor allocation, use Session5.blastMemeFileMemory

   // a file-level implementation of a memo field blast
   long currentOffset ;
   FIELD4 *field ;
   long recNo ;
   int status ;   // STATUS5NOTHING, STATUS5WRITING  -- reading is irrelevant

   char *buffer ;
   long bufferLen ;

   Blast5memoFile() { status = STATUS5NOTHING ; }
   ~Blast5memoFile() { initUndo() ; }

   void initRead( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
   void initUndo() ;
   void initWrite( Table5 *table, int fieldOrdinal, long recNoIn, long offset ) ;
   int read( void *ptr, long numBytes, long *bytesRead ) ;
   int write( void *ptr, long numBytes, long *bytesWrote ) ;
} ;

//class Blast5readSocket ;
//class Blast5writeSocket ;

#ifndef S4STAND_ALONE
class Blast5readSocket : Link5
{
public:
   // for constructor allocation, use Code4.blastReadSocketMemory; also note:
   // unused objects of this type are stored in List4 Code4.blastReadSockets

   // cannot share read and write sockets due to outstanding reads/writes disallowed under Windows 95
   CODE4 *c4 ;
   CONNECT4THREAD *socket ;

   long totalBytesRead ;

   char *buffer[BLAST5NUM_BUFFERS] ;
   long bufferLen[BLAST5NUM_BUFFERS] ;

   Blast5readSocket( CODE4 *code4 ) { c4 = code4 ; }
   ~Blast5readSocket() { close() ; freeBuffers() ; }

   void close() ; // close the socket
   void freeBuffers() ;

   int init() ;
   int read( void *ptr, long numBytes, int *bytesRead ) ;
   initUndo() ;
} ;

class Blast5writeSocket : LINK4
{
public:
   // for constructor allocation, use Code4.blastWriteSocketMemory; also note:
   // unused objects of this type are stored in List4 Code4.blastWriteSockets

   // cannot share read and write sockets due to outstanding reads/writes disallowed under Windows 95
   CODE4 *c4 ;
   CONNECT4THREAD *socket ;

   long totalBytesWritten ;

   char *buffer[BLAST5NUM_BUFFERS] ;
   long bufferLen[BLAST5NUM_BUFFERS] ;

   Blast5writeSocket( CODE4 *code4 ) { c4 = code4 ; }
   ~Blast5writeSocket() { close() ; freeBuffers() ; }

   void close() ; // close the socket
   void freeBuffers() ;

   int write( void *ptr, long numBytes, long *bytesWrote ) ;

   int init() ;
   initUndo() ;
} ;

/*

- currently do not need to implement; add if we ever support file-name fields
class Blast5file : Blast5low
{
public:
   File4 *file ;
   File4seqRead seqRead ;
   File4seqWrite seqWrite ;
   void *buffer ;
   int bufferLen ;

   Blast5file( Code4 *c4, File4 *file, long startPosition ) ;   // set up a file blast
   ~Blast5file() { initUndo() ; u4free( buffer ) ; buffer = 0 ; bufferLen = 0 ;  }

   int bufferInit( int readOrWriteMode ) ;

   void initUndo() ;
   int initRead( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
   int initWrite( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
   int read( void *ptr, long numBytes, long *bytesRead ) ;
   int write( void *ptr, long numBytes, long *bytesWrote ) ;
} ;
*/

class Blast5memoSocket : Blast5memo
{
public:
   // for constructor allocation, use Session5.blastMemeSocketMemory

   // a socket-level implementation of a memo field blast
   int status ;   // STATUS5NOTHING, STATUS5WRITING, STATUS5READING
   CODE4 *c4 ;

   Blast5readSocket *readSocket ;
   Blast5writeSocket *writeSocket ;

   Blast5memoSocket( CODE4 *code4 ) { c4 = code4 ; status = STATUS5NOTHING ; }
   ~Blast5memoSocket() { initUndo() ; }

   void initRead( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;
   void initUndo() ;
   void initWrite( Table5 *table, int fieldOrdinal, long recNo, long offset ) ;

   int read( void *ptr, long numBytes, int *bytesRead ) { return readSocket->read( ptr, numBytes, bytesRead ) ; }
   int write( void *ptr, long numBytes, long *bytesWrote ) { return writeSocket->write( ptr, numBytes, bytesWrote ) ; }
} ;

#endif /* S4STAND_ALONE */
#endif /* S4SERVER */

#endif /* S4OFF_BLAST */
#endif /* OLEDB5BUILD */
