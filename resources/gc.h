#ifndef _GC_H_
#define _GC_H_

#include <inttypes.h>

/**
 * This function performs a mark-and-compact garbage collection pass on a heap.
 * Upon completion, the heap cursor is updated to point to the new start of free
 * heap memory.
 *
 * @param desired_free The minimum number of bytes which should be free after
 *                     garbage collection.  If the number of free bytes after
 *                     garbage collection is less than this number then the
 *                     program will halt.  If the function returns, at least
 *                     this many bytes are guaranteed to be free.
 */
void gc(int64_t desired_free) asm("gc");

#endif
