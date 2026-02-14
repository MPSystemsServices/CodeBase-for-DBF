/* l4link.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef E4LINK
void S4FUNCTION l4addLow( LIST4 *listIn, void *item )
{
   if ( listIn == 0 || item == 0 )
      error4( 0, e4parm_null, E96201 ) ;
   else
   l4addAfter( listIn, listIn->lastNode, item ) ;
}
#endif /* E4LINK */



#ifndef L4ADD_AFTER_INLINE
void S4FUNCTION l4addAfter( LIST4 *listIn, void *anchor, void *item )
{
   #ifdef S4DATA_ALIG2
      LINK4  *temp ;
   #endif

   #ifdef E4LINK
      if ( listIn == 0 || item == 0 )
      {
         error4( 0, e4parm_null, E96202 ) ;
         return ;
      }
      if ( l4seek( listIn, item ) == 1 )
      {
         error4( 0, e4info, E96202 ) ;
         return ;
      }
      if ( listIn->lastNode != 0 )
      {
         if ( anchor == 0 )
         {
            error4( 0, e4parm_null, E96202 ) ;
            return ;
         }
         if ( l4seek( listIn, anchor ) == 0 )
         {
            error4( 0, e4info, E96202 ) ;
            return ;
         }
      }
   #endif

   if ( listIn->lastNode == 0 )
   {
      listIn->lastNode = (LINK4 *)item ;
      LINK4PTR(item)->p = (LINK4 *)item ;
      LINK4PTR(item)->n = (LINK4 *)item ;
   }
   else
   {
      #ifdef S4DATA_ALIG2
         memcpy( &LINK4PTR(item)->p, &anchor, sizeof(LINK4 *)  );
         memcpy( &LINK4PTR(item)->n, &LINK4PTR(anchor)->n, sizeof(LINK4 *) ) ;
         memcpy( &temp, &LINK4PTR(anchor)->n, sizeof(LINK4 *) ) ;
         memcpy( &LINK4PTR(temp)->p, &item, sizeof(LINK4 *) ) ;
         memcpy( &LINK4PTR(anchor)->n, &item, sizeof(LINK4 *) ) ;
      #else
         LINK4PTR(item)->p = (LINK4 *)anchor ;
         LINK4PTR(item)->n = LINK4PTR(anchor)->n ;
         LINK4PTR(anchor)->n->p= (LINK4 *)item ;
         LINK4PTR(anchor)->n = (LINK4 *)item ;
      #endif /* !S4DATA_ALIG2 */
      if ( anchor == (void *)listIn->lastNode )
         listIn->lastNode = (LINK4 *)item ;
   }

   listIn->nLink++ ;
   #ifdef E4LINK
      l4check( listIn ) ;
   #endif
}
#endif /* L4ADD_AFTER_INLINE */



void S4FUNCTION l4addBefore( LIST4 *listIn, void *anchor, void *item )
{
   #ifdef E4LINK
      if ( listIn == 0 || item == 0 )
      {
         error4( 0, e4parm_null, E96203 ) ;
         return ;
      }
      if ( l4seek( listIn, item ) == 1 )
      {
         error4( 0, e4info, E96203 ) ;
         return ;
      }
      if ( listIn->lastNode != 0 )
      {
         if ( anchor == 0 )
         {
            error4( 0, e4parm_null, E96202 ) ;
            return ;
         }
         if ( l4seek( listIn, anchor ) == 0 )
         {
            error4( 0, e4info, E96203 ) ;
            return ;
         }
      }
   #endif

   if ( listIn->lastNode == 0 )
   {
      listIn->lastNode = (LINK4 *)item ;
      LINK4PTR(item)->p = (LINK4 *)item ;
      LINK4PTR(item)->n = (LINK4 *)item ;
   }
   else
   {
      LINK4PTR(item)->n = (LINK4 *)anchor ;
      LINK4PTR(item)->p = LINK4PTR(anchor)->p ;
      LINK4PTR(anchor)->p->n= (LINK4 *)item ;
      LINK4PTR(anchor)->p = (LINK4 *)item ;
   }

   listIn->nLink++ ;
   #ifdef E4LINK
      l4check( listIn ) ;
   #endif
}



#ifdef E4LINK
static void l4verifySelected( LIST4 *listIn )
{
   if ( listIn != NULL )
      if ( listIn->selected != NULL )
         if ( l4seek( listIn, listIn->selected ) == 0 )
            error4( 0, e4info, E96201 ) ;
}
#endif /* E4LINK */



#ifdef E4LINK
int S4FUNCTION l4check( LIST4 *listIn )
{
   /* Check the Linked List */
   LINK4 *onLink ;
   unsigned int i ;

   if ( listIn == 0 )
      return error4( 0, e4parm_null, E96204 ) ;

   onLink = listIn->lastNode ;
   if ( onLink == 0 )
   {
      if ( listIn->nLink != 0 )
         return error4( 0, e4info, E86201 ) ;
   }
   else
      if ( listIn->nLink == 0 )
         return error4( 0, e4info, E86201 ) ;

   for ( i = 1; i <= listIn->nLink; i++ )
   {
      if ( onLink->n->p != onLink  ||  onLink->p->n != onLink )
         return error4( 0, e4info, E86201 ) ;

      onLink = onLink->n ;

      if ( i == listIn->nLink || onLink == listIn->lastNode )
         if ( i != listIn->nLink || onLink != listIn->lastNode )
            return error4( 0, e4info, E86201 ) ;
   }

   l4verifySelected( listIn ) ;
   return 0 ;
}
#endif /* E4LINK */



#ifdef E4LINK
void *S4FUNCTION l4firstLow( const LIST4 *listIn )
{
   if ( listIn == 0 )
   {
      error4( 0, e4parm_null, E96205 ) ;
      return 0 ;
   }

   if ( listIn->lastNode == 0 )
      return 0 ;
   return (void *)listIn->lastNode->n ;
}
#endif /* E4LINK */



#ifdef E4LINK
void *S4FUNCTION l4lastLow( const LIST4 *listIn )
{
   if ( listIn == 0 )
   {
      error4( 0, e4parm_null, E96206 ) ;
      return 0 ;
   }

   return (void *)listIn->lastNode ;
}
#endif /* E4LINK */



#ifdef E4LINK
void *S4FUNCTION l4nextLow( const LIST4 *listIn, const void *link )
{
   if ( listIn == 0 )
   {
      error4( 0, e4parm_null, E96207 ) ;
      return 0 ;
   }
   /* Make sure the link is on the linked listIn ! */
   if ( link != 0 )
      if ( l4seek( listIn, link ) == 0 )
      {
         error4( 0, e4parm_null, E86202 ) ;
         return 0 ;
      }

   if ( link == (void *)listIn->lastNode )
      return 0 ;
   if ( link == 0 )
      return l4first( listIn ) ;

   return (void *)(LINK4PTR(link)->n) ;
}
#endif /* E4LINK */



#ifdef E4LINK
static void *S4FUNCTION l4nextNoSeek( const LIST4 *listIn, const void *link )
{
   if ( link == (void *)listIn->lastNode )
      return 0 ;
   if ( link == 0 )
      return l4first( listIn ) ;

   return (void *)(LINK4PTR(link)->n) ;
}
#else  /* E4LINK else */
#define l4nextNoSeek( lst, lnk ) l4next( (lst), (lnk) )
#endif /* E4LINK else */


#ifdef E4LINK
void *S4FUNCTION l4popLow( LIST4 *listIn )
{
   if ( listIn == 0 )
   {
      error4( 0, e4parm_null, E96208 ) ;
      return 0 ;
   }

   return l4remove( listIn, listIn->lastNode ) ;
}
#endif /* E4LINK */



void *S4FUNCTION l4prev( const LIST4 *listIn, const void *link )
{
   #ifdef E4LINK
      if ( listIn == 0 )
      {
         error4( 0, e4parm_null, E96209 ) ;
         return 0 ;
      }
      /* Make sure the link is on the linked listIn ! */
      if ( link != 0 )
         if ( l4seek( listIn, link ) == 0 )
         {
            error4( 0, e4parm_null, E86202 ) ;
            return 0 ;
         }
   #endif

   if ( link == 0 )
       return (void *)listIn->lastNode ;
   if ( (void *)listIn->lastNode->n == link )
      return 0 ;

   return (void *)(LINK4PTR(link)->p) ;
}



void *S4FUNCTION l4remove( LIST4 *listIn, void *item )
{
   /* AS 09/06/98 changed to return item to optimize l4pop, which should
      produce faster code */
   #ifdef S4DATA_ALIG2
      LINK4   *temp, *temp2;
   #endif

   if ( item == 0 )
      return 0 ;

   #ifdef E4LINK
      if ( listIn == 0 )
      {
         error4( 0, e4parm_null, E86202 ) ;
         return 0 ;
      }
      /* Make sure the link being removed is on the linked listIn ! */
      if ( l4seek( listIn, item ) == 0 )
      {
         error4( 0, e4parm_null, E86202 ) ;
         return 0 ;
      }
   #endif

   if ( listIn->selected == item )
   {
      listIn->selected = (LINK4 *)(LINK4PTR(item)->p) ;
      if ( listIn->selected == item )
         listIn->selected = 0 ;
   }
   #ifdef S4DATA_ALIG2
      memcpy( &temp, &LINK4PTR(item)->p, sizeof(LINK4 *) );
      memcpy( &LINK4PTR(temp)->n, &LINK4PTR(item)->n, sizeof(LINK4 *) );
      memcpy( &temp, &LINK4PTR(item)->n, sizeof(LINK4 *) );
      memcpy( &LINK4PTR(temp)->p, &LINK4PTR(item)->p, sizeof(LINK4 *) );
      if ( item == (void *)listIn->lastNode )
      {
         memcpy(&temp, &((LIST4 *)listIn)->lastNode, sizeof(LIST4 *) ) ;
         memcpy(&temp2, &LINK4PTR(temp)->p, sizeof(LINK4 *) ) ;
         if (temp == temp2)
            listIn->lastNode = 0 ;
         else
            listIn->lastNode = temp2 ;
      }
   #else
      LINK4PTR(item)->p->n = LINK4PTR(item)->n ;
      LINK4PTR(item)->n->p = LINK4PTR(item)->p ;
      if ( item == (void *)listIn->lastNode )
      {
         if ( listIn->lastNode->p == listIn->lastNode )
            listIn->lastNode = 0 ;
         else
            listIn->lastNode = listIn->lastNode->p ;
      }
   #endif  /* !S4DATA_ALIG2 */

   listIn->nLink-- ;

   #ifdef E4LINK
      LINK4PTR(item)->p = 0 ;
      LINK4PTR(item)->n = 0 ;
      l4check( listIn ) ;
   #endif

   return item ;
}



/* returns true if the selected link is contained in the listIn */
int S4FUNCTION l4seek( const LIST4 *listIn, const void *item )
{
   LINK4 *linkOn ;
   #ifdef E4LINK
      unsigned long nLink ;
   #endif

   #ifdef E4PARM_LOW
      if ( listIn == 0 || item == 0 )
         error4( 0, e4parm_null, E96211 ) ;
   #endif

   #ifdef E4LINK
      for ( linkOn = 0, nLink = 0 ;; nLink++ )
   #else
      for ( linkOn = 0 ;; )
   #endif
   {
      linkOn = (LINK4 *)l4nextNoSeek( listIn, linkOn ) ;
      if ( linkOn == 0 )
         break ;
      if ( (void *)linkOn == item )
         return 1 ;
      #ifdef E4LINK
         if ( nLink > l4numNodes( listIn ) )
            error4( 0, e4struct, E96211 ) ;
      #endif
   }

   return 0 ;
}

/*int l4reset( const LIST4 *listIn, const void *item ) */
