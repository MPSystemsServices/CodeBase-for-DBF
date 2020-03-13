/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

// timer5.h

#ifndef TIMER5_INC

#define TIMER5_INC

#ifdef TIMER5OUT
/*

   usage:

   Call timerMemory5create() to get a pointer for timer memory
     This must be called to provide as paramater to timer5createTop()

   Call timer5create() to create a top-level timer (the 1st timer)
     This must be called to get the first(master) timer which may
     be passed to timer5create() to create sub timers if desired.

   Call timer5createSub() to create a sub-level timer.  The input to this
     function is the timer to which this one is sub (either a return
     from timer5createTop() or return from timer5create() )



*/


S4EXPORT TimerMemory5 * S4FUNCTION timerMemory5create( CODE4 S4PTR * ) ;
S4EXPORT void S4FUNCTION timerMemory5destroy( TimerMemory5 S4PTR * ) ;

S4EXPORT Timer5 * S4FUNCTION timer5createSub( Timer5 S4PTR *, const char S4PTR * ) ;
S4EXPORT Timer5 * S4FUNCTION timer5create( TimerMemory5 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION timer5destroy( Timer5 S4PTR * ) ;
S4EXPORT void S4FUNCTION timer5stop( Timer5 S4PTR * ) ;
S4EXPORT void S4FUNCTION timer5start( Timer5 S4PTR * ) ;
S4EXPORT void S4FUNCTION timer5displayResults( Timer5 S4PTR *, FILE4 S4PTR * ) ;

#endif /* TIMER5OUT */

#endif /* not TIMER5_INC */
