#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern uint64_t* start_of_stack;
extern uint64_t* end_of_stack;
extern uint64_t* start_of_heap;
extern uint64_t* end_of_heap;
extern uint64_t* heap_cursor;

/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write

      debugf("Pointer %p changed to pointer %p.\n", old, new);

  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
#define debugf(fmt, ...) ;

// This macro enables all debugf statements.  (They become printf statements.)
//  #define debugf(fmt, ...) printf(fmt, ##__VA_ARGS__); fflush(stdout)


/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap() {
  debugf("HEAP:\n");
  int c = 0;
  for (uint64_t* p = (uint64_t*)((uint64_t)start_of_heap & 0xFFFFFFFFFFFFFFFC);
       p < end_of_heap; p += 1) {
    if (c==0) {
      debugf("%016"PRIx64":", p);
    }
    if (p >= start_of_heap) {
      debugf("    %016"PRIx64, *p);
    } else {
      debugf("            ");
    }
    c++;
    if (c==4) {
      debugf("\n");
      c=0;
    }
  }
  if (c!=0) {
    debugf("\n");
  }
}

bool is_heap_ptr (uint64_t ptr) {
  /* checks if pointer ends in 01 and points to valid heap memory */
  uint64_t* machine_ptr = ptr - 1;
  if (((ptr & 0x3) == 0x1) &&
     (machine_ptr >= start_of_heap) && 
     (machine_ptr < end_of_heap)) {
      return true;
    }
  else {
    return false;
  }
}

int heap_object_size (uint64_t* ptr) {
  /* ptr is a machine pointer
    returns size of heap object in words (multiply by 8 to get bytes) */
  if ((*ptr & 0x8000000000000000) == 0x8000000000000000) { //object is a closure
    return (((*ptr) & 0x0FFFFFFFFFFFFFFF) + 4);
  }
  else { //object is a tuple
    return (*ptr + 2);
  }
}
void mark_ptr(uint64_t* heap_element, int i){
  /* takes a machine ptr */
  uint64_t* gc_word = heap_element + 1;
  *gc_word = i;
  return;
}

void dfs_mark(int64_t start){
  uint64_t* start_ptr = start - 1;
  // debugf("start value: %x\n", start);
  if (*(start_ptr + 1) == 0x0){ // Want to go to GC word which is always 8bytes from start of heap ptr
    mark_ptr(start_ptr, 1); // Set GC word to 1
    for (int i = 0; i < heap_object_size(start_ptr); i++ ){ // Walk down heap element to look for other heap ptrs
      // debugf("for loop: %d\n", i);
      uint64_t* temp_ptr = start_ptr + i;
      if (is_heap_ptr(*temp_ptr)){ // found another heap ptr
        // debugf("found another heap prt: %x\n", temp_ptr);
        dfs_mark(*temp_ptr);
    }
  }
  return; // if we exit from the for loop then we didn't find a heap ptr in our current node
  }else{
    return; // We have already marked it (to avoid getting stuck in a cycle we should break)
  }
}

void dfs_unmark(int64_t start){
  uint64_t* start_ptr = start - 1;
  if (*(start_ptr + 1) != 0x0){ // Want to go to GC word which is always 8bytes from start of heap ptr
    mark_ptr(start_ptr, 0); // Set GC word to 1
    for (int i = 0; i < heap_object_size(start_ptr); i++ ){ // Walk down heap element to look for other heap ptrs
      uint64_t* temp_ptr = start_ptr + i;
      if (is_heap_ptr(*temp_ptr)){ // found another heap ptr
        dfs_unmark(*temp_ptr);
    }
  }
  return; // if we exit from the for loop then we didn't find a heap ptr in our current node
  }else{
    return; // We have already marked it (to avoid getting stuck in a cycle we should break)
  }
}

void mark_heap(){
  uint64_t* stack_ptr = end_of_stack;
  while (stack_ptr < start_of_stack) {
    if (is_heap_ptr(*stack_ptr)){
      dfs_mark(*stack_ptr);
    }
    stack_ptr ++; // will automatically move by 8 bytes
  }
  return;
}

void forward() {
  uint64_t* next_heap_object = start_of_heap;
  uint64_t* next_live_destination = start_of_heap;
  while (next_heap_object < heap_cursor) {
    int size = heap_object_size(next_heap_object);
    // debugf("size of object at %p is %d\n", next_heap_object, size);
    if (*(next_heap_object + 1) == 1) { //GC word is set, object reachable
      *(next_heap_object + 1) = next_live_destination;
      // debugf("set new destination to %p\n", next_live_destination);
      next_live_destination += size;
    }
    next_heap_object += size;
  }
}

void update(){
  uint64_t* current = end_of_stack;
  while (current < start_of_stack){
    uint64_t bird_ptr = *current;
    if (is_heap_ptr(bird_ptr)) {
      uint64_t* new_location_ptr = bird_ptr + 7; //subtract 1 to make machine ptr, add 8 to access GC word
      uint64_t new_location = *new_location_ptr;
      *current = new_location + 1;
    }
    current ++;
  }
  uint64_t* next_heap_word = start_of_heap; 
  while (next_heap_word < heap_cursor) {
    uint64_t bird_ptr = *next_heap_word;
    if (is_heap_ptr(bird_ptr)) { //element on heap is a heap pointer
      uint64_t* new_location_ptr = bird_ptr + 7;
      uint64_t new_location = *new_location_ptr;
      *next_heap_word = new_location + 1;
    }
    next_heap_word ++;
  }  
}

void compact () {
  uint64_t* next_heap_object = start_of_heap;
  uint64_t* old_heap_cursor = heap_cursor;
  while (next_heap_object < old_heap_cursor) {
    int size = heap_object_size(next_heap_object);
    uint64_t* current_heap_object = next_heap_object;
    next_heap_object += size;
    if (*(current_heap_object + 1) != 0) { //object reachable, GC word not 0
      //copy heap object to memory location *(next_heap_object + 1)
      uint64_t* dst_ptr = *(current_heap_object + 1); //destination is ptr stored in GC word
      uint64_t* src_ptr = current_heap_object; //source is machine ptr to current object
      //size is number of words to be copied
      memmove(dst_ptr, src_ptr, sizeof(uint64_t)*size);
      heap_cursor = dst_ptr+size;
      // debugf("new heap_cursor: %p\n", heap_cursor);
    }
  }
  // debugf("end of compact -- next_heap_object: %p\n", next_heap_object);
  // debugf("end of compact -- heap_cursor: %p\n", heap_cursor);
}

void unmark_heap () {
  uint64_t* iteration_ptr = end_of_stack;
  while (iteration_ptr < start_of_stack) {
    if (is_heap_ptr(*iteration_ptr)){
      debugf("found heap ptr %p\n", *iteration_ptr);
      dfs_unmark(*iteration_ptr);
    }
    iteration_ptr ++; // will automatically move by 8 bytes
  }
  iteration_ptr = heap_cursor;
  int64_t bad_value = 0xbadbadffffbadbad;
  while (iteration_ptr < end_of_heap){
    *iteration_ptr = bad_value;
    iteration_ptr ++;
  }
  return;
}

void check_desired_free (int64_t desired_free) {
  int64_t newly_free = end_of_heap - heap_cursor;
  // debugf("words free after mark-compact: %d\ndesired_free: %x\n", newly_free, desired_free);
  if ((newly_free *8) < desired_free){
    stopWithError(7);
  }
}

void gc(int64_t desired_free) {
  mark_heap();
  debugf("after mark\n");
  dump_heap();
  forward();
  debugf("after forward\n");
  dump_heap();
  update();
  debugf("after update\n");
  dump_heap();
  compact();
  debugf("after compact\n");
  dump_heap();
  unmark_heap();
  debugf("after unmark\n");
  dump_heap();
  check_desired_free (desired_free);
}

