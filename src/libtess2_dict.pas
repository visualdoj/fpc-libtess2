unit libtess2_dict;

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
  libtess2_bucketalloc;

function dictNewDict(alloc: PTESSalloc; frame: Pointer; leq: TDictLeq): PDict;

procedure dictDeleteDict(alloc: PTESSalloc; dict: PDict);

//  Search returns the node with the smallest key greater than or equal
// to the given key.  If there is no such key, returns a node whose
// key is NULL.  Similarly, Succ(Max(d)) has a NULL key, etc.
//
function dictSearch(dict: PDict; key: TDictKey): PDictNode;
function dictInsertBefore(dict: PDict; node: PDictNode; key: TDictKey): PDictNode;
procedure dictDelete(dict: PDict; node: PDictNode);

function dictKey(n: PDictNode): TDictKey; inline;
function dictSucc(n: PDictNode): PDictNode; inline;
function dictPred(n: PDictNode): PDictNode; inline;
function dictMin(d: PDict): PDictNode; inline;
function dictMax(d: PDict): PDictNode; inline;
function dictInsert(d: PDict; k: TDictKey): PDictNode; inline;

implementation

function dictKey(n: PDictNode): TDictKey;
begin
  Exit(n^.key);
end;

function dictSucc(n: PDictNode): PDictNode;
begin
  Exit(n^.next);
end;

function dictPred(n: PDictNode): PDictNode;
begin
  Exit(n^.prev);
end;

function dictMin(d: PDict): PDictNode;
begin
  Exit(d^.head.next);
end;

function dictMax(d: PDict): PDictNode;
begin
  Exit(d^.head.prev);
end;

function dictInsert(d: PDict; k: TDictKey): PDictNode;
begin
  Exit(dictInsertBefore(d, @d^.head, k));
end;

// really tessDictListNewDict
function dictNewDict(alloc: PTESSalloc; frame: Pointer; leq: TDictLeq): PDict;
var
  dict: PDict;
  head: PDictNode;
begin
  dict := PDict(alloc^.memalloc(alloc^.userData, SizeOf(TDict)));

  if dict = nil then
    Exit(nil);

  head := @dict^.head;

  head^.key := nil;
  head^.next := head;
  head^.prev := head;

  dict^.frame := frame;
  dict^.leq := leq;

  if alloc^.dictNodeBucketSize < 16 then
    alloc^.dictNodeBucketSize := 16;
  if alloc^.dictNodeBucketSize > 4096 then
    alloc^.dictNodeBucketSize := 4096;
  dict^.nodePool := createBucketAlloc(alloc, 'Dict', SizeOf(TDictNode), alloc^.dictNodeBucketSize);

  Exit(dict);
end;

// really tessDictListDeleteDict
procedure dictDeleteDict(alloc: PTESSalloc; dict: PDict);
begin
  deleteBucketAlloc(dict^.nodePool);
  alloc^.memfree(alloc^.userData, dict);
end;

// really tessDictListInsertBefore
function dictInsertBefore(dict: PDict; node: PDictNode; key: TDictKey): PDictNode;
var
  newNode: PDictNode;
begin
  repeat
    node := node^.prev;
  until (node^.key = nil) or dict^.leq(dict^.frame, node^.key, key);

  newNode := PDictNode(bucketAlloc(dict^.nodePool));
  if newNode = nil then
    Exit(nil);

  newNode^.key := key;
  newNode^.next := node^.next;
  node^.next^.prev := newNode;
  newNode^.prev := node;
  node^.next := newNode;

  Exit(newNode);
end;

// really tessDictListDelete
procedure dictDelete(dict: PDict; node: PDictNode); //ARGSUSED
begin
  node^.next^.prev := node^.prev;
  node^.prev^.next := node^.next;
  bucketFree(dict^.nodePool, node);
end;

// really tessDictListSearch
function dictSearch(dict: PDict; key: TDictKey): PDictNode;
var
  node: PDictNode;
begin
  node := @dict^.head;

  repeat
    node := node^.next;
  until (node^.key = nil) or dict^.leq(dict^.frame, key, node^.key);

  Exit(node);
end;

end.
