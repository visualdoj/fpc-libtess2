unit libtess2_bucketalloc;

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
** Author: Mikko Mononen, July 2009.
** Pascal port: Doj, 2019
*}

{$MODE FPC}
{$MODESWITCH RESULT}
{$MODESWITCH OUT}

interface

uses
  libtess2_structs;

function createBucketAlloc(alloc: PTESSalloc; name: PAnsiChar;
                           itemSize: PtrUInt; bucketSize: PtrUInt): PBucketAlloc;
function bucketAlloc(ba: PBucketAlloc): Pointer;
procedure bucketFree(ba: PBucketAlloc; ptr: Pointer);
procedure deleteBucketAlloc(ba: PBucketAlloc);

implementation

//{$DEFINE CHECK_BOUNDS}

function CreateBucket(ba: PBucketAlloc): Boolean;
var
  size: SizeUInt;
  bucket: PBucket;
  freelist: Pointer;
  head: PByte;
  it: PByte;
begin
  // Allocate memory for the bucket
  size := SizeOf(TBucket) + ba^.itemSize * ba^.bucketSize;
  bucket := PBucket(ba^.alloc^.memalloc(ba^.alloc^.userData, size));
  if bucket = nil then
    Exit(False);
  bucket^.next := nil;

  // Add the bucket into the list of buckets.
  bucket^.next := ba^.buckets;
  ba^.buckets := bucket;

  // Add new items to the free list.
  freelist := ba^.freelist;
  head := PByte(bucket) + SizeOf(TBucket);
  it := head + ba^.itemSize * ba^.bucketSize;
  repeat
    Dec(it, ba^.itemSize);
    // Store pointer to next free item.
    PPointer(it)^ := freelist;
    // Pointer to next location containing a free item.
    freelist := Pointer(it);
  until it = head;
  // Update pointer to next location containing a free item.
  ba^.freelist := Pointer(it);

  Exit(True);
end;

function NextFreeItem(ba: PBucketAlloc): Pointer;
begin
  Exit(PPointer(ba^.freelist)^);
end;

function createBucketAlloc(alloc: PTESSalloc; name: PAnsiChar;
                           itemSize: PtrUInt; bucketSize: PtrUInt): PBucketAlloc;
var
  ba: PBucketAlloc;
begin
  ba := PBucketAlloc(alloc^.memalloc(alloc^.userData, SizeOf(TBucketAlloc)));

  ba^.alloc := alloc;
  ba^.name := name;
  ba^.itemSize := itemSize;
  if ba^.itemSize < SizeOf(Pointer) then
    ba^.itemSize := SizeOf(Pointer);
  ba^.bucketSize := bucketSize;
  ba^.freelist := nil;
  ba^.buckets := nil;

  if not CreateBucket(ba) then begin
    alloc^.memfree( alloc^.userData, ba );
    Exit(nil);
  end;

  Exit(ba);
end;

function bucketAlloc(ba: PBucketAlloc): Pointer;
var
  it: Pointer;
begin
  // If running out of memory, allocate new bucket and update the freelist.
  if (ba^.freelist = nil) or (NextFreeItem(ba) = nil) then begin
    if not CreateBucket(ba) then
      Exit(nil);
  end;

  // Pop item from in front of the free list.
  it := ba^.freelist;
  ba^.freelist := NextFreeItem(ba);

  Exit(it);
end;

procedure bucketFree(ba: PBucketAlloc; ptr: Pointer);
var
  inBounds: Boolean;
  bucket: PBucket;
  bucketMin, bucketMax: Pointer;
begin
{$IF Defined(CHECK_BOUNDS)}
  inBounds := False;

  // Check that the pointer is allocated with this allocator.
  bucket := ba^.buckets;
  while bucket <> nil do begin
    bucketMin := Pointer(PByte(bucket) + SizeOf(TBucket));
    bucketMax := Pointer(PByte(bucket) + SizeOf(TBucket) + ba^.itemSize * ba^.bucketSize);
    if (ptr >= bucketMin) and (ptr < bucketMax ) then begin
      inBounds := True;
      break;
    end;
    bucket := bucket^.next;
  end;

  if inBounds then begin
    // Add the node in front of the free list.
    PPointer(ptr)^ := ba^.freelist;
    ba^.freelist := ptr;
  end else begin
    if IsConsole then begin
      Writeln('ERROR! pointer 0x', HexStr(PtrUInt(ptr), SizeOf(ptr) * 2), ' does not belong to allocator ''', ba^.name, '''');
    end;
  end;
{$ELSE}
  // Add the node in front of the free list.
  PPointer(ptr)^ := ba^.freelist;
  ba^.freelist := ptr;
{$ENDIF}
end;

procedure deleteBucketAlloc(ba: PBucketAlloc);
var
  alloc: PTESSalloc;
  bucket: PBucket;
  next: PBucket;
begin
  alloc := ba^.alloc;
  bucket := ba^.buckets;
  while bucket <> nil do begin
    next := bucket^.next;
    alloc^.memfree(alloc^.userData, bucket);
    bucket := next;
  end;
  ba^.freelist := nil;
  ba^.buckets := nil;
  alloc^.memfree(alloc^.userData, ba);
end;

end.
