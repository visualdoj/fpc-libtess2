unit libtess2_priorityq;

{*
** SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
** Copyright (C) [dates of first publication] Silicon Graphics, Inc.
** All Rights Reserved.
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
** of the Software, and to permit persons to whom the Software is furnished to do so,
** subject to the following conditions:
**
** The above copyright notice including the dates of first publication and either this
** permission notice or a reference to http://oss.sgi.com/projects/FreeB/ shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
** INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
** PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL SILICON GRAPHICS, INC.
** BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
** OR OTHER DEALINGS IN THE SOFTWARE.
**
** Except as contained in this notice, the name of Silicon Graphics, Inc. shall not
** be used in advertising or otherwise to promote the sale, use or other dealings in
** this Software without prior written authorization from Silicon Graphics, Inc.
*}
{*
** Author: Eric Veach, July 1994.
** Pascal port: Doj, 2019
*}

{$MODE FPC}
{$MODESWITCH RESULT}
{$MODESWITCH OUT}

interface

uses
  libtess2_structs,
  libtess2_geom;

//  The basic operations are insertion of a new key (pqInsert),
// and examination/extraction of a key whose value is minimum
// (pqMinimum/pqExtractMin).  Deletion is also allowed (pqDelete);
// for this purpose pqInsert returns a "handle" which is supplied
// as the argument.
//
// An initial heap may be created efficiently by calling pqInsert
// repeatedly, then calling pqInit.  In any case pqInit must be called
// before any operations other than pqInsert are used.
//
// If the heap is empty, pqMinimum/pqExtractMin will return a NULL key.
// This may also be tested with pqIsEmpty.
//
//
//  Since we support deletion the data structure is a little more
// complicated than an ordinary heap.  "nodes" is the heap itself;
// active nodes are stored in the range 1..pq->size.  When the
// heap exceeds its allocated size (pq->max), its size doubles.
// The children of node i are nodes 2i and 2i+1.
//
// Each node stores an index into an array "handles".  Each handle
// stores a key, plus a pointer back to the node which currently
// represents that key (ie. nodes[handles[i].node].handle == i).
//

function pqNewPriorityQ(alloc: PTESSalloc; size: SizeUInt; leq: TPriorityQLeq): PPriorityQ;
procedure pqDeletePriorityQ(alloc: PTESSalloc; pq: PPriorityQ);

function pqInit(alloc: PTESSalloc; pq: PPriorityQ): Boolean;
function pqInsert(alloc: PTESSalloc; pq: PPriorityQ; keyNew: PQkey): PQhandle;
function pqExtractMin(pq: PPriorityQ): PQkey;
procedure pqDelete(pq: PPriorityQ; curr: PQhandle);

function pqMinimum(pq: PPriorityQ): PQkey;
function pqIsEmpty(pq: PPriorityQ): Boolean;

implementation

const INIT_SIZE =  32;

{$IF Defined(FOR_TRITE_TEST_PROGRAM)}
{$error LEQ cannot be substituted easily for the FOR_TRITE_TEST_PROGRAM feature.
 Do the substitution in the unit by yourslef (or remove this error):
  LEQ -> pq^.leq
}
//{$MACRO ON}
//{$DEFINE LEQ:=pq^.leq}
function LEQ(x, y: PQkey): Boolean;
begin
  Exit(libtess2_geom.VertLeq(PTESSvertex(x), PTESSvertex(y)));
end;
{$ELSE}
// Violates modularity, but a little faster
function LEQ(x, y: PQkey): Boolean;
begin
  Exit(libtess2_geom.VertLeq(PTESSvertex(x), PTESSvertex(y)));
end;
{$ENDIF}


// Include all the code for the regular heap-based queue here.

//  The basic operations are insertion of a new key (pqInsert),
// and examination/extraction of a key whose value is minimum
// (pqMinimum/pqExtractMin).  Deletion is also allowed (pqDelete);
// for this purpose pqInsert returns a "handle" which is supplied
// as the argument.
//
// An initial heap may be created efficiently by calling pqInsert
// repeatedly, then calling pqInit.  In any case pqInit must be called
// before any operations other than pqInsert are used.
//
// If the heap is empty, pqMinimum/pqExtractMin will return a NULL key.
// This may also be tested with pqIsEmpty.
//


//  Since we support deletion the data structure is a little more
// complicated than an ordinary heap.  "nodes" is the heap itself;
// active nodes are stored in the range 1..pq->size.  When the
// heap exceeds its allocated size (pq->max), its size doubles.
// The children of node i are nodes 2i and 2i+1.
//
// Each node stores an index into an array "handles".  Each handle
// stores a key, plus a pointer back to the node which currently
// represents that key (ie. nodes[handles[i].node].handle == i).
//


function pqHeapMinimum(pq: PPriorityQHeap): PQkey; inline;
begin
  Exit((pq)^.handles[(pq)^.nodes[1].handle].key);
end;

function pqHeapIsEmpty(pq: PPriorityQHeap): Boolean;
begin
  Exit((pq)^.size = 0);
end;


// really pqHeapNewPriorityQHeap
function pqHeapNewPriorityQ(alloc: PTESSalloc; size: PtrInt; leq: TPriorityQLeq): PPriorityQHeap;
var
  pq: PPriorityQHeap;
begin
  pq := PPriorityQHeap(alloc^.memalloc(alloc^.userData, SizeOf(TPriorityQHeap)));
  if pq = nil then
    Exit(nil);

  pq^.size := 0;
  pq^.max := size;
  pq^.nodes := PPQnode(alloc^.memalloc(alloc^.userData, (size + 1) * SizeOf(pq^.nodes[0])));
  if pq^.nodes = nil then begin
    alloc^.memfree(alloc^.userData, pq);
    Exit(nil);
  end;

  pq^.handles := PPQhandleElem(alloc^.memalloc(alloc^.userData, (size + 1) * SizeOf(pq^.handles[0])));
  if pq^.handles = nil then begin
    alloc^.memfree(alloc^.userData, pq^.nodes);
    alloc^.memfree(alloc^.userData, pq);
    Exit(nil);
  end;

  pq^.initialized := False;
  pq^.freeList := 0;
  pq^.leq := leq;

  pq^.nodes[1].handle := 1;  // so that Minimum() returns NULL
  pq^.handles[1].key := nil;
  Exit(pq);
end;

// really pqHeapDeletePriorityQHeap
procedure pqHeapDeletePriorityQ(alloc: PTESSalloc; pq: PPriorityQHeap);
begin
  alloc^.memfree(alloc^.userData, pq^.handles);
  alloc^.memfree(alloc^.userData, pq^.nodes);
  alloc^.memfree(alloc^.userData, pq);
end;

procedure FloatDown(pq: PPriorityQHeap; curr: PQhandle);
var
  n: PPQnode;
  h: PPQhandleElem;
  hCurr, hChild: PQhandle;
  child: PQhandle;
begin
  n := pq^.nodes;
  h := pq^.handles;

  hCurr := n[curr].handle;
  while True do begin
    child := curr shl 1;
    if (child < pq^.size) and LEQ(h[n[child + 1].handle].key, h[n[child].handle].key) then begin
      Inc(child);
    end;

    Assert(child <= pq^.max);

    hChild := n[child].handle;
    if (child > pq^.size) or LEQ(h[hCurr].key, h[hChild].key) then begin
      n[curr].handle := hCurr;
      h[hCurr].node := curr;
      break;
    end;
    n[curr].handle := hChild;
    h[hChild].node := curr;
    curr := child;
  end;
end;


procedure FloatUp(pq: PPriorityQHeap; curr: PQhandle);
var
  n: PPQnode;
  h: PPQhandleElem;
  hCurr, hParent: PQhandle;
  parent: PQhandle;
begin
  n := pq^.nodes;
  h := pq^.handles;

  hCurr := n[curr].handle;
  while True do begin
    parent := curr shr 1;
    hParent := n[parent].handle;
    if (parent = 0) or LEQ(h[hParent].key, h[hCurr].key) then begin
      n[curr].handle := hCurr;
      h[hCurr].node := curr;
      break;
    end;
    n[curr].handle := hParent;
    h[hParent].node := curr;
    curr := parent;
  end;
end;

// really pqHeapInit
procedure pqHeapInit(pq: PPriorityQHeap);
var
  i: PQhandle;
begin
  // This method of building a heap is O(n), rather than O(n lg n).

  for i := pq^.size downto 1 do begin
    FloatDown(pq, i);
  end;
  pq^.initialized := True;
end;

// really pqHeapInsert
// returns INV_HANDLE iff out of memory
function pqHeapInsert(alloc: PTESSalloc; pq: PPriorityQHeap; keyNew: PQkey): PQhandle;
var
  curr: PQhandle;
  free: PQhandle;
  saveNodes: PPQnode;
  saveHandles: PPQhandleElem;
begin
  Inc(pq^.size);
  curr := pq^.size;
  if (curr * 2) > pq^.max then begin
    if alloc^.memrealloc = nil then begin
      Exit(INV_HANDLE);
    end else begin
      saveNodes := pq^.nodes;
      saveHandles := pq^.handles;

      // If the heap overflows, double its size.
      pq^.max := pq^.max shl 1;
      pq^.nodes := PPQnode(alloc^.memrealloc(alloc^.userData, pq^.nodes,
        SizeUInt((pq^.max + 1) * SizeOf(pq^.nodes[0]))));
      if pq^.nodes = nil then begin
        pq^.nodes := saveNodes;  // restore ptr to free upon return
        Exit(INV_HANDLE);
      end;
      pq^.handles := PPQhandleElem(alloc^.memrealloc(alloc^.userData, pq^.handles,
        SizeUInt((pq^.max + 1) * SizeOf(pq^.handles[0]))));
      if pq^.handles = nil then begin
        pq^.handles := saveHandles; // restore ptr to free upon return
        Exit(INV_HANDLE);
      end;
    end;
  end;

  if pq^.freeList = 0 then begin
    free := curr;
  end else begin
    free := pq^.freeList;
    pq^.freeList := pq^.handles[free].node;
  end;

  pq^.nodes[curr].handle := free;
  pq^.handles[free].node := curr;
  pq^.handles[free].key := keyNew;

  if pq^.initialized then begin
    FloatUp(pq, curr);
  end;
  Assert(free <> INV_HANDLE);
  Exit(free);
end;

// really pqHeapExtractMin
function pqHeapExtractMin(pq: PPriorityQHeap): PQkey;
var
  n: PPQnode;
  h: PPQhandleElem;
  hMin: PQhandle;
  min: PQkey;
begin
  n := pq^.nodes;
  h := pq^.handles;
  hMin := n[1].handle;
  min := h[hMin].key;

  if pq^.size > 0 then begin
    n[1].handle := n[pq^.size].handle;
    h[n[1].handle].node := 1;

    h[hMin].key := nil;
    h[hMin].node := pq^.freeList;
    pq^.freeList := hMin;

    Dec(pq^.size);
    if pq^.size > 0 then begin
      FloatDown(pq, 1);
    end;
  end;
  Exit(min);
end;

// really pqHeapDelete
procedure pqHeapDelete(pq: PPriorityQHeap; hCurr: PQhandle);
var
  n: PPQnode;
  h: PPQhandleElem;
  curr: PQhandle;
begin
  n := pq^.nodes;
  h := pq^.handles;

  Assert((hCurr >= 1) and (hCurr <= pq^.max) and (h[hCurr].key <> nil));

  curr := h[hCurr].node;
  n[curr].handle := n[pq^.size].handle;
  h[n[curr].handle].node := curr;

  Dec(pq^.size);
  if curr <= pq^.size then begin
    if (curr <= 1) or LEQ(h[n[curr shr 1].handle].key, h[n[curr].handle].key) then begin
      FloatDown(pq, curr);
    end else begin
      FloatUp(pq, curr);
    end;
  end;
  h[hCurr].key := nil;
  h[hCurr].node := pq^.freeList;
  pq^.freeList := hCurr;
end;



// Now redefine all the function names to map to their "Sort" versions.

// really tessPqSortNewPriorityQ
function pqNewPriorityQ(alloc: PTESSalloc; size: SizeUInt; leq: TPriorityQLeq): PPriorityQ;
var
  pq: PPriorityQ;
begin
  pq := PPriorityQ(alloc^.memalloc(alloc^.userData, SizeOf(TPriorityQ)));
  if pq = nil then
    Exit(nil);

  pq^.heap := pqHeapNewPriorityQ(alloc, size, leq);
  if pq^.heap = nil then begin
    alloc^.memfree(alloc^.userData, pq);
    Exit(nil);
  end;

//  pq^.keys = (PQkey *)memAlloc( INIT_SIZE * sizeof(pq^.keys[0]) );
  pq^.keys := PPQkey(alloc^.memalloc(alloc^.userData, size * SizeOf(pq^.keys[0])));
  if pq^.keys = nil then begin
    pqHeapDeletePriorityQ(alloc, pq^.heap);
    alloc^.memfree(alloc^.userData, pq);
    Exit(nil);
  end;

  pq^.size := 0;
  pq^.max := size; //INIT_SIZE;
  pq^.initialized := False;
  pq^.leq := leq;

  Exit(pq);
end;

// really tessPqSortDeletePriorityQ
procedure pqDeletePriorityQ(alloc: PTESSalloc; pq: PPriorityQ);
begin
  assert(pq <> nil);
  if pq^.heap <> nil then
    pqHeapDeletePriorityQ(alloc, pq^.heap);
  if pq^.order <> nil then
    alloc^.memfree(alloc^.userData, pq^.order);
  if pq^.keys <> nil then
    alloc^.memfree(alloc^.userData, pq^.keys);
  alloc^.memfree(alloc^.userData, pq);
end;


function LT(x, y: PQkey): Boolean; inline;
begin
  Exit(not LEQ(y, x));
end;

function GT(x, Y: PQkey): Boolean; inline;
begin
  Exit(not LEQ(x, y));
end;

procedure Swap(a, b: PPPQkey);
var
  tmp: PPQkey;
begin
  tmp := a^;
  a^ := b^;
  b^ := tmp;
end;

// really tessPqSortInit
function pqInit(alloc: PTESSalloc; pq: PPriorityQ): Boolean;
type
  PStackRecord = ^TStackRecord;
  TStackRecord = record
    p: PPPQkey;
    r: PPPQkey;
  end;
var
  Stack: array[0 .. 50 - 1] of TStackRecord;
  top: PStackRecord;
  p, r, i, j: PPPQkey;
  piv: PPQkey;
  seed: PtrUInt;
begin
  top := @Stack[0];
  seed := 2016473283;

  //  Create an array of indirect pointers to the keys, so that we
  // the handles we have returned are still valid.
  //
  //
  // pq^.order = (PQkey **)memAlloc( (size_t)
  // (pq^.size * sizeof(pq^.order[0])) );
  //
  pq^.order := PPPQkey(alloc^.memalloc(alloc^.userData,
                      SizeUInt((pq^.size+1) * SizeOf(pq^.order[0]))));
  // the previous line is a patch to compensate for the fact that IBM
  // machines return a null on a malloc of zero bytes (unlike SGI),
  // so we have to put in this defense to guard against a memory
  // fault four lines down. from fossum@austin.ibm.com.
  if pq^.order = nil then
    Exit(False);

  p := pq^.order;
  r := p + pq^.size - 1;
  piv := pq^.keys;
  i := p;
  while i <= r do begin
    i^ := piv;
    Inc(piv);
    Inc(i);
  end;

  //  Sort the indirect pointers in descending order,
  // using randomized Quicksort
  //
  top^.p := p;
  top^.r := r;
  while top >= @Stack[0] do begin
    p := top^.p;
    r := top^.r;
    while r > p + 10 do begin
      seed := seed * 1539415821 + 1;
      i := p + seed mod (r - p + 1);
      piv := i^;
      i^ := p^;
      p^ := piv;
      i := p - 1;
      j := r + 1;
      repeat
        repeat Inc(i) until not GT(i^^, piv^);
        repeat Dec(j) until not LT(j^^, piv^);
        Swap(i, j);
      until i >= j;
      Swap(i, j); // Undo last swap
      if i - p < r - j then begin
        top^.p := j + 1;
        top^.r := r;
        Inc(top);
        r := i - 1;
      end else begin
        top^.p := p;
        top^.r := i - 1;
        Inc(top);
        p := j + 1;
      end;
    end;
    // Insertion sort small lists
    i := p + 1;
    while i <= r do begin
      piv := i^;
      j := i;
      while (j > p) and LT((j - 1)^^, piv^) do begin
        j^ := (j - 1)^;
        Dec(j);
      end;
      j^ := piv;
      Inc(i);
    end;
    Dec(top);
  end;
  pq^.max := pq^.size;
  pq^.initialized := True;
  pqHeapInit(pq^.heap);  // always succeeds

{$IF not Defined(NDEBUG)}
  p := pq^.order;
  r := p + pq^.size - 1;
  i := p;
  while i < r do begin
    assert(LEQ((i+1)^^, i^^));
    Inc(i);
  end;
{$ENDIF}

  Exit(True);
end;

// really tessPqSortInsert
// returns INV_HANDLE iff out of memory
function pqInsert(alloc: PTESSalloc; pq: PPriorityQ; keyNew: PQkey): PQhandle;
var
  curr: PQhandle;
  saveKey: PPQkey;
begin
  if pq^.initialized then begin
    Exit(pqHeapInsert(alloc, pq^.heap, keyNew));
  end;
  curr := pq^.size;
  Inc(pq^.size);
  if pq^.size >= pq^.max then begin
    if alloc^.memrealloc = nil then begin
      Exit(INV_HANDLE);
    end else begin
      saveKey := pq^.keys;
      // If the heap overflows, double its size.
      pq^.max := pq^.max shl 1;
      pq^.keys := PPQkey(alloc^.memrealloc(alloc^.userData, pq^.keys,
        SizeUInt(pq^.max * SizeOf(pq^.keys[0]))));
      if pq^.keys = nil then begin
        pq^.keys := saveKey;  // restore ptr to free upon return
        Exit(INV_HANDLE);
      end;
    end;
  end;
  assert(curr <> INV_HANDLE);
  pq^.keys[curr] := keyNew;

  // Negative handles index the sorted array.
  Exit(-(curr+1));
end;

// really tessPqSortExtractMin
function pqExtractMin(pq: PPriorityQ): PQkey;
var
  sortMin, heapMin: PQkey;
begin
  if pq^.size = 0 then begin
    Exit(pqHeapExtractMin(pq^.heap));
  end;
  sortMin := (pq^.order[pq^.size - 1])^;
  if not pqHeapIsEmpty(pq^.heap) then begin
    heapMin := pqHeapMinimum(pq^.heap);
    if LEQ(heapMin, sortMin) then begin
      Exit(pqHeapExtractMin(pq^.heap));
    end;
  end;
  repeat
    Dec(pq^.size);
  until (pq^.size <= 0) or ((pq^.order[pq^.size - 1])^ <> nil);
  Exit(sortMin);
end;

// really tessPqSortMinimum
function pqMinimum(pq: PPriorityQ): PQkey;
var
  sortMin, heapMin: PQkey;
begin
  if pq^.size = 0 then begin
    Exit(pqHeapMinimum(pq^.heap));
  end;
  sortMin := (pq^.order[pq^.size - 1])^;
  if not pqHeapIsEmpty(pq^.heap) then begin
    heapMin := pqHeapMinimum(pq^.heap);
    if LEQ(heapMin, sortMin) then begin
      Exit(heapMin);
    end;
  end;
  Exit(sortMin);
end;

// really tessPqSortIsEmpty
function pqIsEmpty(pq: PPriorityQ): Boolean;
begin
  Exit((pq^.size = 0) and pqHeapIsEmpty(pq^.heap));
end;

// really tessPqSortDelete
procedure pqDelete(pq: PPriorityQ; curr: PQhandle);
begin
  if curr >= 0 then begin
    pqHeapDelete(pq^.heap, curr);
    Exit;
  end;
  curr := - (curr + 1);
  Assert((curr < pq^.max) and (pq^.keys[curr] <> nil));

  pq^.keys[curr] := nil;
  while (pq^.size > 0) and ((pq^.order[pq^.size - 1])^ = nil) do begin
    Dec(pq^.size);
  end;
end;

end.
