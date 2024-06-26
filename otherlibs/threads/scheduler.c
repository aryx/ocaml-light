/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id: scheduler.c,v 1.32 1997/12/09 09:10:31 xleroy Exp $ */

/* The thread scheduler */

#include "callback.h"
#include "config.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "fail.h"
#include "io.h"
#include "roots.h"
#include "alloc.h"
#include "memory.h"
#include "signals.h"

//defined(HAS_SETITIMER) && \ for plan9 for now, TODO!!

#if ! (defined(HAS_SELECT) && \
       defined(HAS_GETTIMEOFDAY) && \
       (defined(HAS_WAITPID) || defined(HAS_WAIT4)))
#include "Cannot compile libthreads, system calls missing"
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/select.h>

/* Configuration */

/* Initial size of stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* Max computation time before rescheduling, in microseconds (50ms) */
#define Thread_timeout 50000

/* The thread descriptors */

struct thread_struct {
  value ident;                  /* Unique id (for equality comparisons) */
  struct thread_struct * next;  /* Double linking of threads */
  struct thread_struct * prev;
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;
  value * trapsp;
  value status;                 /* RUNNABLE, KILLED. etc (see below) */
  value readfds, writefds, exceptfds;
                /* Lists of file descriptors on which we're doing select() */
  value delay;                  /* Time until which this thread is blocked */
  value joining;                /* Thread we're trying to join */
  value waitpid;                /* PID of process we're waiting for */
  value retval;                 /* Value to return when thread resumes */
};
//pad: saved backtrace in recent ocaml

typedef struct thread_struct * thread_t;

#define RUNNABLE Val_int(0)
#define KILLED Val_int(1)
#define SUSPENDED Val_int(2)
#define BLOCKED_IO Val_int(4)
#define BLOCKED_DELAY Val_int(8)
#define BLOCKED_JOIN Val_int(16)
#define BLOCKED_WAIT Val_int(32)

#define RESUMED_WAKEUP Val_int(0)
#define RESUMED_DELAY Val_int(1)
#define RESUMED_JOIN Val_int(2)

#define TAG_RESUMED_IO 0
#define TAG_RESUMED_WAIT 1

#define NO_FDS Val_unit
#define NO_DELAY Val_unit
#define NO_JOINING Val_unit
#define NO_WAITPID Val_int(0)

#define DELAY_INFTY 1E30        /* +infty, for this purpose */

/* The thread currently active */
static thread_t curr_thread = NULL;
/* Identifier for next thread creation */
static value next_ident = Val_int(0);

#define Assign(dst,src) modify((value *)&(dst), (value)(src))

/* Scan the stacks of the other threads */

static void (*prev_scan_roots_hook) (scanning_action);

static void thread_scan_roots(scanning_action action)
{
  thread_t th;

  /* Scan all active descriptors */
  (*action)((value) curr_thread, (value *) &curr_thread);
  /* Don't scan curr_thread->sp, this has already been done.
     Don't scan local roots either, for the same reason. */
  for (th = curr_thread->next; th != curr_thread; th = th->next) {
    (*action)((value) th, (value *) &th);
    do_local_roots(action, th->sp, th->stack_high, NULL);
  }
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

/* Initialize the thread machinery */

value thread_initialize(value unit)       /* ML */
{
  //struct itimerval timer;
  /* Create a descriptor for the current thread */
  curr_thread =
    (thread_t) alloc_shr(sizeof(struct thread_struct) / sizeof(value), 0);
  curr_thread->ident = next_ident;
  next_ident = Val_int(Int_val(next_ident) + 1);
  curr_thread->next = curr_thread;
  curr_thread->prev = curr_thread;
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;
  curr_thread->status = RUNNABLE;
  curr_thread->readfds = NO_FDS;
  curr_thread->writefds = NO_FDS;
  curr_thread->exceptfds = NO_FDS;
  curr_thread->delay = NO_DELAY;
  curr_thread->joining = NO_JOINING;
  curr_thread->waitpid = NO_WAITPID;
  curr_thread->retval = Val_unit;
  /* Initialize GC */
  prev_scan_roots_hook = scan_roots_hook;
  scan_roots_hook = thread_scan_roots;
  /* Initialize interval timer */
  //timer.it_interval.tv_sec = 0;
  //timer.it_interval.tv_usec = Thread_timeout;
  //timer.it_value = timer.it_interval;
  //setitimer(ITIMER_VIRTUAL, &timer, NULL);
  return Val_unit;
}
//pad: timer initialization done in separate function
// so first set vtalarm callback from ocaml and then set timer
// for this callback


/* Create a thread */

value thread_new(value clos)          /* ML */
{
  thread_t th;
  /* Allocate the thread and its stack */
  Begin_root(clos);
    th = (thread_t) alloc_shr(sizeof(struct thread_struct) / sizeof(value), 0);
  End_roots();
  th->ident = next_ident;
  next_ident = Val_int(Int_val(next_ident) + 1);
  th->stack_low = (value *) stat_alloc(Thread_stack_size);
  th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
  th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
  th->sp = th->stack_high;
  th->trapsp = th->stack_high;
  /* Set up a return frame that pretends we're applying clos to ().
     This way, when this thread is activated, the RETURN will take us
     to the entry point of the closure. */
  th->sp -= 4;
  th->sp[0] = Val_unit;
  th->sp[1] = (value) Code_val(clos);
  th->sp[2] = clos;
  th->sp[3] = Val_long(0);      /* no extra args */
  /* Fake a C call frame */
  th->sp--;
  th->sp[0] = Val_unit;         /* a dummy environment */
  /* The thread is initially runnable */
  th->status = RUNNABLE;
  th->readfds = NO_FDS;
  th->writefds = NO_FDS;
  th->exceptfds = NO_FDS;
  th->delay = NO_DELAY;
  th->joining = NO_JOINING;
  th->waitpid = NO_WAITPID;
  th->retval = Val_unit;
  /* Insert thread in doubly linked list of threads */
  th->prev = curr_thread->prev;
  th->next = curr_thread;
  Assign(curr_thread->prev->next, th);
  Assign(curr_thread->prev, th);
  /* Return thread */
  return (value) th;
}

/* Return the thread identifier */

value thread_id(value th)             /* ML */
{
  return ((struct thread_struct *)th)->ident;
}

/* Return the current time as a floating-point number */

static double timeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (double) tv.tv_sec + (double) tv.tv_usec * 1e-6;
}

/* Find a runnable thread and activate it */

#define FOREACH_THREAD(x) x = curr_thread; do { x = x->next;
#define END_FOREACH(x) } while (x != curr_thread)

static value alloc_process_status(int pid, int status);
static void add_fdlist_to_set(value fdl, fd_set *set);
static value inter_fdlist_set(value fdl, fd_set *set);

#ifndef FD_SETSIZE // for plan9
#define FD_SETSIZE (sizeof(int) * 8) // from otherlibs/unix/select.c
#endif

static value schedule_thread(void)
{
  thread_t run_thread, th;
  fd_set readfds, writefds, exceptfds;
  double delay, now;
  int need_select, need_wait;

  /* Don't allow preemption during a callback */
  if (callback_depth > 0) return curr_thread->retval;

  /* Save the status of the current thread */
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;

try_again:
  /* Build fdsets and delay for select.
     See if some join or wait operations succeeded. */
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  delay = DELAY_INFTY;
  now = -1.0;
  need_select = 0;
  need_wait = 0;

  FOREACH_THREAD(th)
    if (th->status & (BLOCKED_IO - 1)) {
      add_fdlist_to_set(th->readfds, &readfds);
      add_fdlist_to_set(th->writefds, &writefds);
      add_fdlist_to_set(th->exceptfds, &exceptfds);
      need_select = 1;
    }
    if (th->status & (BLOCKED_DELAY - 1)) {
      double th_delay;
      if (now < 0.0) now = timeofday();
      th_delay = Double_val(th->delay) - now;
      if (th_delay <= 0) {
        th->status = RUNNABLE;
        th->retval = RESUMED_DELAY;
      } else {
        if (th_delay < delay) delay = th_delay;
      }
    }
    if (th->status & (BLOCKED_JOIN - 1)) {
      if (((thread_t)(th->joining))->status == KILLED) {
        th->status = RUNNABLE;
        th->retval = RESUMED_JOIN;
      }
    }
    if (th->status & (BLOCKED_WAIT - 1)) {
      int status, pid;
      pid = waitpid(Int_val(th->waitpid), &status, WNOHANG);
      if (pid > 0) {
        th->status = RUNNABLE;
        Assign(th->retval, alloc_process_status(pid, status));
      } else {
        need_wait = 1;
      }
    }
  END_FOREACH(th);

  /* Find if a thread is runnable. */
  run_thread = NULL;
  FOREACH_THREAD(th)
    if (th->status == RUNNABLE) { run_thread = th; break; }
  END_FOREACH(th);

  /* Do the select if needed */
  if (need_select || run_thread == NULL) {
    struct timeval delay_tv, * delay_ptr;
    int retcode;
    /* If a thread is blocked on wait, don't block forever */
    if (need_wait && delay > Thread_timeout * 1e-6) {
      delay = Thread_timeout * 1e-6;
    }
    /* Convert delay to a timeval */
    /* If a thread is runnable, just poll */
    if (run_thread != NULL) {
      delay_tv.tv_sec = 0;
      delay_tv.tv_usec = 0;
      delay_ptr = &delay_tv;
    }
    else if (delay != DELAY_INFTY) {
      delay_tv.tv_sec = (unsigned int) delay;
      delay_tv.tv_usec = (delay - (double) delay_tv.tv_sec) * 1E6;
      delay_ptr = &delay_tv;
    }
    else {
      delay_ptr = NULL;
    }
    enter_blocking_section();
    retcode = select(FD_SETSIZE, &readfds, &writefds, &exceptfds, delay_ptr);
    leave_blocking_section();
    if (retcode > 0) {
      /* Some descriptors are ready. 
         Mark the corresponding threads runnable. */
      FOREACH_THREAD(th)
        if (th->status & (BLOCKED_IO - 1)) {
	  value r = Val_unit, w = Val_unit, e = Val_unit;
          Begin_roots3(r,w,e);
            r = inter_fdlist_set(th->readfds, &readfds);
	    w = inter_fdlist_set(th->writefds, &writefds);
	    e = inter_fdlist_set(th->exceptfds, &exceptfds);
	    if (r != NO_FDS || w != NO_FDS || e != NO_FDS) {
	      value retval = alloc_small(3, TAG_RESUMED_IO);
	      Field(retval, 0) = r;
	      Field(retval, 1) = w;
	      Field(retval, 2) = e;
	      Assign(th->retval, retval);
	      th->status = RUNNABLE;
	      if (run_thread == NULL) run_thread = th; /* Found one. */
	    }
          End_roots();
        }
      END_FOREACH(th);
    }
    /* If we get here with run_thread still NULL, one of the following
       may have happened:
       - a delay has expired
       - a wait() needs to be polled again
       - the select() failed (e.g. was interrupted)
       In these cases, we go through the loop once more to make the
       corresponding threads runnable. */
    if (run_thread == NULL &&
        (delay != DELAY_INFTY || need_wait || retcode == -1))
      goto try_again;
  }

  /* If we haven't something to run at that point, we're in big trouble. */
  if (run_thread == NULL) invalid_argument("Thread: deadlock");

  /* Free everything the thread was waiting on */
  Assign(run_thread->readfds, NO_FDS);
  Assign(run_thread->writefds, NO_FDS);
  Assign(run_thread->exceptfds, NO_FDS);
  Assign(run_thread->delay, NO_DELAY);
  Assign(run_thread->joining, NO_JOINING);
  run_thread->waitpid = NO_WAITPID;

  /* Activate the thread */
  curr_thread = run_thread;
  stack_low = curr_thread->stack_low;
  stack_high = curr_thread->stack_high;
  stack_threshold = curr_thread->stack_threshold;
  extern_sp = curr_thread->sp;
  trapsp = curr_thread->trapsp;
  return curr_thread->retval;
}

/* Since context switching is not allowed in callbacks, a thread that
   blocks during a callback is a deadlock. */

static void check_callback(void)
{
  if (callback_depth > 0)
    fatal_error("Thread: deadlock during callback");
}

/* Reschedule without suspending the current thread */

value thread_yield(value unit)        /* ML */
{
  Assert(curr_thread != NULL);
  curr_thread->retval = Val_unit;
  return schedule_thread();
}

/* Suspend the current thread */

value thread_sleep(value unit)        /* ML */
{
  Assert(curr_thread != NULL);
  check_callback();
  curr_thread->status = SUSPENDED;
  return schedule_thread();
}

/* Suspend the current thread on a select() request */

value thread_select(value arg)        /* ML */
{
  double date;
  /* Don't do an error if we're not initialized yet
     (we can be called from thread-safe Pervasives before initialization),
     just return immediately. */
  if (curr_thread == NULL) return RESUMED_WAKEUP;
  check_callback();
  Assign(curr_thread->readfds, Field(arg, 0));
  Assign(curr_thread->writefds, Field(arg, 1));
  Assign(curr_thread->exceptfds, Field(arg, 2));
  date = Double_val(Field(arg, 3));
  if (date >= 0.0) {
    date += timeofday();
    Assign(curr_thread->delay, copy_double(date));
    curr_thread->status = BLOCKED_IO | BLOCKED_DELAY;
  } else {
    curr_thread->status = BLOCKED_IO;
  }
  return schedule_thread();
}

/* Primitives to implement suspension on buffered channels */

value thread_inchan_ready(value vchan) /* ML */
{
  struct channel * chan = Channel(vchan);
  return Val_bool(chan->curr < chan->max);
}

value thread_outchan_ready(value vchan, value vsize) /* ML */
{
  struct channel * chan = Channel(vchan);
  long size = Long_val(vsize);
  /* Negative size means we want to flush the buffer entirely */
  if (size < 0) {
    return Val_bool(chan->curr == chan->buff);
  } else {
    int free = chan->end - chan->curr;
    if (chan->curr == chan->buff)
      return Val_bool(size < free);
    else
      return Val_bool(size <= free);
  }
}

/* Suspend the current thread for some time */

value thread_delay(value time)          /* ML */
{
  double date = timeofday() + Double_val(time);
  Assert(curr_thread != NULL);
  check_callback();
  curr_thread->status = BLOCKED_DELAY;
  Assign(curr_thread->delay, copy_double(date));
  return schedule_thread();
}

/* Suspend the current thread until another thread terminates */

value thread_join(value th)          /* ML */
{
  check_callback();
  Assert(curr_thread != NULL);
  if (((thread_t)th)->status == KILLED) return Val_unit;
  curr_thread->status = BLOCKED_JOIN;
  Assign(curr_thread->joining, th);
  return schedule_thread();
}

/* Suspend the current thread until a Unix process exits */

value thread_wait_pid(value pid)          /* ML */
{
  Assert(curr_thread != NULL);
  check_callback();
  curr_thread->status = BLOCKED_WAIT;
  curr_thread->waitpid = pid;
  return schedule_thread();
}

/* Reactivate another thread */

value thread_wakeup(value thread)     /* ML */
{
  thread_t th = (thread_t) thread;
  switch (th->status) {
  case SUSPENDED:
    th->status = RUNNABLE;
    th->retval = RESUMED_WAKEUP;
    break;
  case KILLED:
    failwith("Thread.wakeup: killed thread");
  default:
    failwith("Thread.wakeup: thread not suspended");
  }
  return Val_unit;
}

/* Return the current thread */

value thread_self(value unit)         /* ML */
{
  Assert(curr_thread != NULL);
  return (value) curr_thread;
}

/* Kill a thread */

value thread_kill(value thread)       /* ML */
{
  value retval = Val_unit;
  thread_t th = (thread_t) thread;
  if (th->status == KILLED) failwith("Thread.kill: killed thread");
  /* Don't paint ourselves in a corner */
  if (th == th->next) failwith("Thread.kill: cannot kill the last thread");
  /* This thread is no longer waiting on anything */
  th->status = KILLED;
  /* If this is the current thread, activate another one */
  if (th == curr_thread) retval = schedule_thread();
  /* Remove thread from the doubly-linked list */
  Assign(th->prev->next, th->next);
  Assign(th->next->prev, th->prev);
  /* Free its resources */
  stat_free((char *) th->stack_low);
  th->stack_low = NULL;
  th->stack_high = NULL;
  th->stack_threshold = NULL;
  th->sp = NULL;
  th->trapsp = NULL;
  return retval;
}

/* Set a list of file descriptors in a fdset */

static void add_fdlist_to_set(value fdl, fd_set *set)
{
  for (/*nothing*/; fdl != NO_FDS; fdl = Field(fdl, 1)) {
    FD_SET(Int_val(Field(fdl, 0)), set);
  }
}

/* Build the intersection of a list and a fdset (the list of file descriptors
   which are both in the list and in the fdset). */

static value inter_fdlist_set(value fdl, fd_set *set)
{
  value res = Val_unit;
  value cons;

  Begin_roots2(fdl, res);
    for (res = NO_FDS; fdl != NO_FDS; fdl = Field(fdl, 1)) {
      int fd = Int_val(Field(fdl, 0));
      if (FD_ISSET(fd, set)) {
        cons = alloc_small(2, 0);
	Field(cons, 0) = Val_int(fd);
	Field(cons, 1) = res;
	res = cons;
	FD_CLR(fd, set); /* wake up only one thread per fd ready */
      }
    }
  End_roots();
  return res;
}

/* Auxiliary function for allocating the result of a waitpid() call */

#if !(defined(WIFEXITED) && defined(WEXITSTATUS) && defined(WIFSTOPPED) && \
      defined(WSTOPSIG) && defined(WTERMSIG))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) (((status) & 0xFF) == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status) & 0x3F)
#endif

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int pid, int status)
{
  value st, res;

  if (WIFEXITED(status)) {
    st = alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(WSTOPSIG(status));
  }
  else {
    st = alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(WTERMSIG(status));
  }
  Begin_root(st);
    res = alloc_small(2, TAG_RESUMED_WAIT);
    Field(res, 0) = Val_int(pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}
