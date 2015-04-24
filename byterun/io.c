/*s: byterun/io.c */
/*s: copyright header C xavier */
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
/*e: copyright header C xavier */

/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <string.h>

#include "config.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef __STDC__
#include <limits.h>
#endif

#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "sys.h"

#ifndef INT_MAX
/*s: constant INT_MAX */
#define INT_MAX 0x7FFFFFFF
/*e: constant INT_MAX */
#endif

#ifndef SEEK_SET
/*s: constant SEEK_SET */
#define SEEK_SET 0
/*e: constant SEEK_SET */
/*s: constant SEEK_CUR */
#define SEEK_CUR 1
/*e: constant SEEK_CUR */
/*s: constant SEEK_END */
#define SEEK_END 2
/*e: constant SEEK_END */
#endif

/*s: global channel_mutex_free */
/* Hooks for locking channels */

void (*channel_mutex_free) (struct channel *) = NULL;
/*e: global channel_mutex_free */
/*s: global channel_mutex_lock */
void (*channel_mutex_lock) (struct channel *) = NULL;
/*e: global channel_mutex_lock */
/*s: global channel_mutex_unlock */
void (*channel_mutex_unlock) (struct channel *) = NULL;
/*e: global channel_mutex_unlock */
/*s: global channel_mutex_unlock_exn */
void (*channel_mutex_unlock_exn) (void) = NULL;
/*e: global channel_mutex_unlock_exn */

/*s: function open_descriptor */
/* Basic functions over type struct channel *.
   These functions can be called directly from C.
   No locking is performed. */

struct channel * open_descriptor(int fd)
{
  struct channel * channel;

  channel = (struct channel *) stat_alloc(sizeof(struct channel));
  channel->fd = fd;
  channel->offset = 0;
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  channel->mutex = NULL;
  return channel;
}
/*e: function open_descriptor */

/*s: function close_channel */
void close_channel(struct channel *channel)
{
  close(channel->fd);
  if (channel_mutex_free != NULL) (*channel_mutex_free)(channel);
  stat_free(channel);
}  
/*e: function close_channel */

/*s: function channel_size */
long channel_size(struct channel *channel)
{
  long end;

  end = lseek(channel->fd, 0, SEEK_END);
  if (end == -1 ||
      lseek(channel->fd, channel->offset, SEEK_SET) != channel->offset) {
    sys_error(NO_ARG);
  }
  return end;
}
/*e: function channel_size */

/* Output */

#ifndef EINTR
/*s: constant EINTR */
#define EINTR (-1)
/*e: constant EINTR */
#endif
#ifndef EAGAIN
/*s: constant EAGAIN */
#define EAGAIN (-1)
/*e: constant EAGAIN */
#endif
#ifndef EWOULDBLOCK
/*s: constant EWOULDBLOCK */
#define EWOULDBLOCK (-1)
/*e: constant EWOULDBLOCK */
#endif

/*s: function do_write */
static int do_write(int fd, char *p, int n)
{
  int retcode;

  Assert(!Is_young(p));
again:
  enter_blocking_section();
  retcode = write(fd, p, n);
  leave_blocking_section();
  if (retcode == -1) {
    if (errno == EINTR) goto again;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      /* We couldn't do a partial write here, probably because
         n <= PIPE_BUF and POSIX says that writes of less than
         PIPE_BUF characters must be atomic.
         So, we force a partial write of 1 character.
         This should always succeed if we've done a select
         on writing just before. */
      if (n > 1) { n = 1; goto again; }
    }
  }
  if (retcode == -1) sys_error(NO_ARG);
  return retcode;
}
/*e: function do_write */

/*s: function flush_partial */
/* Attempt to flush the buffer. This will make room in the buffer for
   at least one character. Returns true if the buffer is empty at the
   end of the flush, or false if some data remains in the buffer. */

int flush_partial(struct channel *channel)
{
  int towrite, written;

  Lock(channel);
  towrite = channel->curr - channel->buff;
  if (towrite > 0) {
    written = do_write(channel->fd, channel->buff, towrite);
    channel->offset += written;
    if (written < towrite)
      bcopy(channel->buff + written, channel->buff, towrite - written);
    channel->curr -= written;
  }
  Unlock(channel);
  return (channel->curr == channel->buff);
}
/*e: function flush_partial */

/*s: function flush */
/* Flush completely the buffer. */

void flush(struct channel *channel)
{
  while (! flush_partial(channel)) /*nothing*/;
}
/*e: function flush */

/*s: function putword */
/* Output data */

void putword(struct channel *channel, uint32 w)
{
  putch(channel, w >> 24);
  putch(channel, w >> 16);
  putch(channel, w >> 8);
  putch(channel, w);
}
/*e: function putword */

/*s: function putblock */
int putblock(struct channel *channel, char *p, long int len)
{
  int n, free, towrite, written;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  free = channel->end - channel->curr;
  if (n <= free) {
    /* Write request small enough to fit in buffer: transfer to buffer. */
    bcopy(p, channel->curr, n);
    channel->curr += n;
    return n;
  } else {
    /* Write request overflows buffer: transfer whatever fits to buffer
       and write the buffer */
    bcopy(p, channel->curr, free);
    towrite = channel->end - channel->buff;
    written = do_write(channel->fd, channel->buff, towrite);
    if (written < towrite)
      bcopy(channel->buff + written, channel->buff, towrite - written);
    channel->offset += written;
    channel->curr = channel->end - written;
    channel->max = channel->end - written;
    return free;
  }
}
/*e: function putblock */

/*s: function really_putblock */
void really_putblock(struct channel *channel, char *p, long int len)
{
  int written;
  while (len > 0) {
    written = putblock(channel, p, len);
    p += written;
    len -= written;
  }
}
/*e: function really_putblock */

/*s: function seek_out */
void seek_out(struct channel *channel, long int dest)
{
  flush(channel);
  if (lseek(channel->fd, dest, 0) != dest) sys_error(NO_ARG);
  channel->offset = dest;
}
/*e: function seek_out */

/*s: function pos_out */
long pos_out(struct channel *channel)
{
  return channel->offset + channel->curr - channel->buff;
}
/*e: function pos_out */

/*s: function do_read */
/* Input */

static int do_read(int fd, char *p, unsigned int n)
{
  int retcode;

  Assert(!Is_young(p));
  enter_blocking_section();
#ifdef EINTR
  do { retcode = read(fd, p, n); } while (retcode == -1 && errno == EINTR);
#else
  retcode = read(fd, p, n);
#endif
  leave_blocking_section();
  if (retcode == -1) sys_error(NO_ARG);
  return retcode;
}
/*e: function do_read */

/*s: function refill */
unsigned char refill(struct channel *channel)
{
  int n;

  n = do_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
  if (n == 0) raise_end_of_file();
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}
/*e: function refill */

/*s: function getword */
uint32 getword(struct channel *channel)
{
  int i;
  uint32 res;

  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + getch(channel);
  }
  return res;
}
/*e: function getword */

/*s: function getblock */
int getblock(struct channel *channel, char *p, long int len)
{
  int n, avail, nread;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    bcopy(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else if (avail > 0) {
    bcopy(channel->curr, p, avail);
    channel->curr += avail;
    return avail;
  } else if (n < IO_BUFFER_SIZE) {
    nread = do_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    bcopy(channel->buff, p, n);
    channel->curr = channel->buff + n;
    return n;
  } else {
    nread = do_read(channel->fd, p, n);
    channel->offset += nread;
    return nread;
  }
}
/*e: function getblock */

/*s: function really_getblock */
int really_getblock(struct channel *chan, char *p, long int n)
{
  int r;
  while (n > 0) {
    r = getblock(chan, p, n);
    if (r == 0) break;
    p += r;
    n -= r;
  }
  return (n == 0);
}
/*e: function really_getblock */

/*s: function seek_in */
void seek_in(struct channel *channel, long int dest)
{
  if (dest >= channel->offset - (channel->max - channel->buff) &&
      dest <= channel->offset) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    if (lseek(channel->fd, dest, SEEK_SET) != dest) sys_error(NO_ARG);
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
}
/*e: function seek_in */

/*s: function pos_in */
long pos_in(struct channel *channel)
{
  return channel->offset - (channel->max - channel->curr);
}
/*e: function pos_in */

/*s: function input_scan_line */
long input_scan_line(struct channel *channel)
{
  char * p;
  int n;

  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        bcopy(channel->curr, channel->buff, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no newline was encountered. */
        return -(channel->max - channel->curr);
      }
      /* Fill the buffer as much as possible */
      n = do_read(channel->fd, channel->max, channel->end - channel->max);
      if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered 
           a newline. */
        return -(channel->max - channel->curr);
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  return (p - channel->curr);
}
/*e: function input_scan_line */

/*s: function finalize_channel */
/* Caml entry points for the I/O functions.  Wrap struct channel *
   objects into a heap-allocated, finalized object.  Perform locking
   and unlocking around the I/O operations. */

static void finalize_channel(value vchan)
{
  struct channel * chan = Channel(vchan);
  if (channel_mutex_free != NULL) (*channel_mutex_free)(chan);
  stat_free(chan);
}
/*e: function finalize_channel */

/*s: function alloc_channel */
static value alloc_channel(struct channel *chan)
{
  value res = alloc_final(2, finalize_channel, 1, 32);
  Field(res, 1) = (value) chan;
  return res;
}
/*e: function alloc_channel */

/*s: function caml_open_descriptor */
value caml_open_descriptor(value fd)       /* ML */
{
  return alloc_channel(open_descriptor(Int_val(fd)));
}
/*e: function caml_open_descriptor */

/*s: function channel_descriptor */
value channel_descriptor(value vchannel)   /* ML */
{
  return Val_long(Channel(vchannel)->fd);
}
/*e: function channel_descriptor */

/*s: function caml_close_channel */
value caml_close_channel(value vchannel)      /* ML */
{
  /* For output channels, must have flushed before */
  struct channel * channel = Channel(vchannel);
  close(channel->fd);
  channel->fd = -1;
  return Val_unit;
}
/*e: function caml_close_channel */

/*s: function caml_channel_size */
value caml_channel_size(value vchannel)      /* ML */
{
  return Val_long(channel_size(Channel(vchannel)));
}
/*e: function caml_channel_size */

/*s: function caml_flush_partial */
value caml_flush_partial(value vchannel)            /* ML */
{
  return Val_bool(flush_partial(Channel(vchannel)));
}
/*e: function caml_flush_partial */

/*s: function caml_flush */
value caml_flush(value vchannel)            /* ML */
{
  flush(Channel(vchannel));
  return Val_unit;
}
/*e: function caml_flush */

/*s: function caml_output_char */
value caml_output_char(value vchannel, value ch)  /* ML */
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  putch(channel, Long_val(ch));
  Unlock(channel);
  return Val_unit;
}
/*e: function caml_output_char */

/*s: function caml_output_int */
value caml_output_int(value vchannel, value w)    /* ML */
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  putword(channel, Long_val(w));
  Unlock(channel);
  return Val_unit;
}
/*e: function caml_output_int */

value caml_output_partial(value vchannel, value buff, value start, value length) /* ML */
{
  struct channel * channel = Channel(vchannel);
  int res;
  Begin_root(buff)
    Lock(channel);
    res = putblock(channel, &Byte(buff, Long_val(start)), Long_val(length));
    Unlock(channel);
  End_roots();
  return Val_int(res);
}

/*s: function caml_output */
value caml_output(value vchannel, value buff, value start, value length) /* ML */
{
  struct channel * channel = Channel(vchannel);
  long pos = Long_val(start);
  long len = Long_val(length);

  Begin_root(buff)
    Lock(channel);
      while (len > 0) {
        int written = putblock(channel, &Byte(buff, pos), len);
        pos += written;
        len -= written;
      }
    Unlock(channel);
  End_roots();
  return Val_unit;
}
/*e: function caml_output */

/*s: function caml_seek_out */
value caml_seek_out(value vchannel, value pos)    /* ML */
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  seek_out(channel, Long_val(pos));
  Unlock(channel);
  return Val_unit;
}
/*e: function caml_seek_out */

/*s: function caml_pos_out */
value caml_pos_out(value vchannel)          /* ML */
{
  return Val_long(pos_out(Channel(vchannel)));
}
/*e: function caml_pos_out */

/*s: function caml_input_char */
value caml_input_char(value vchannel)       /* ML */
{
  struct channel * channel = Channel(vchannel);
  unsigned char c;

  Lock(channel);
  c = getch(channel);
  Unlock(channel);
  return Val_long(c);
}
/*e: function caml_input_char */

/*s: function caml_input_int */
value caml_input_int(value vchannel)        /* ML */
{
  struct channel * channel = Channel(vchannel);
  long i;

  Lock(channel);
  i = getword(channel);
  Unlock(channel);
#ifdef ARCH_SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  return Val_long(i);
}
/*e: function caml_input_int */

/*s: function caml_input */
value caml_input(value vchannel, value buff, value start, value length) /* ML */
{
  struct channel * channel = Channel(vchannel);
  long res;

  Begin_root(buff)
    Lock(channel);
    res = getblock(channel, &Byte(buff, Long_val(start)), Long_val(length));
    Unlock(channel);
  End_roots();
  return Val_long(res);
}
/*e: function caml_input */

/*s: function caml_seek_in */
value caml_seek_in(value vchannel, value pos)     /* ML */
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  seek_in(channel, Long_val(pos));
  Unlock(channel);
  return Val_unit;
}
/*e: function caml_seek_in */

/*s: function caml_pos_in */
value caml_pos_in(value vchannel)           /* ML */
{
  return Val_long(pos_in(Channel(vchannel)));
}
/*e: function caml_pos_in */

/*s: function caml_input_scan_line */
value caml_input_scan_line(value vchannel)       /* ML */
{
  struct channel * channel = Channel(vchannel);
  long res;

  Lock(channel);
  res = input_scan_line(channel);
  Unlock(channel);
  return Val_long(res);
}
/*e: function caml_input_scan_line */

/*e: byterun/io.c */
