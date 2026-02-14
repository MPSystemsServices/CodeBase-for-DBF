

class S5CLASS Link5 : public LINK4
{
public:
   inline LINK4 *link() { return (LINK4 *)this ; }
} ;



class S5CLASS List5 : public LIST4
{
public:
   List5()                     { init() ; }
   // leave out, else get linker errors when creating dll
//   List5( List5 &) ;             // illegal operation, force link error
//   operator =( List5 &);         // illegal operation, force link error

   inline Link5 *add( Link5 *item )     { l4add( this, item->link() ) ; return item ; }
   inline Link5 *addAfter( Link5 *anchor, Link5 *item )
                               { l4addAfter( this, anchor, item->link() ) ; return item ; }
   inline Link5 *addBefore( Link5 *anchor, Link5 *item )
                               { l4addBefore( this, anchor->link(), item->link() ) ; return item;}
   inline Link5 *first()          { return (Link5 *)l4first( this ) ; }
   inline Link5 *last()           { return (Link5 *)l4last(this) ; }
   inline Link5 *next( Link5 *item = NULL ) { return (Link5 *)l4next( this, item->link() ) ; }
   inline int   numNodes()            { return nLink ; }
   inline Link5 *prev( Link5 *item){ return (Link5 *)l4prev( this, item ) ; }
   inline Link5 *pop()            { return (Link5 *)l4pop( this ) ; }
   inline void remove( Link5 *const item )   { l4remove( this, item ) ; }
   inline int removeVerify( Link5 *const item ) // Returns non-zero if the item was not on the list.
   {
      for( Link5 *on = 0; on = this->next( on ); )
         if( on == item )
         {
            this->remove( item ) ;
            return 0 ;
         }
      return 1 ;
   }
   inline seek( Link5 * item ) { return l4seek( this, item->link() ) ; }
   inline void init()                { lastNode = 0 ; nLink = 0 ; selected = 0 ; }
   inline int empty()  { return ( numNodes() == 0 ) ; }
} ;

