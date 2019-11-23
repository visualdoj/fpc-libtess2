# libtess2 for pascal

This is a port of [libtess2](https://github.com/memononen/libtess2) library to
Free Pascal. The purpose of the port is to reduce complexity of using libtess2
in free pascal applications. No need to link with external library, no need to
use C compiler.

I've tried to save interface and behaviour the same as much as possible, so
using the library in pascal code is pretty similar to using it in C code:

```pascal
uses
  libtess2_tesselator; // tesselator.h


const
  CONTOUR1: array[0 .. 5] of TESSreal = (
    185.28, 455.91,
    143.93, 270.04,
    608.54, 489.26
  );
  CONTOUR2: array[0 .. 5] of TESSreal = (
    167.59, 256.50,
    217.29, 556.70,
    363.79, 668.17
  );



var
  tess: PTESStesselator;

  I, Count: LongInt;
  Indices: PTESSindex;
  Vertices: PTESSreal;



begin
  tess := tessNewTess(nil);
  if tess = nil then begin
    Writeln(stderr, 'Out of memory');
    Halt(1);
  end;

  tessAddContour(tess, 2, @CONTOUR1[0], 2 * SizeOf(TESSreal), 3);
  tessAddContour(tess, 2, @CONTOUR2[0], 2 * SizeOf(TESSreal), 3);

  if not tessTesselate(tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil) then begin
    Writeln(stderr, 'Tesselation failed');
    Halt(1);
  end;

  Count    := tessGetElementCount(tess);
  Indices  := tessGetElements(tess);
  Vertices := tessGetVertices(tess);
  Writeln('Triangulation (', Count, ' triangles):');
  for I := 0 to Count - 1 do begin
    Writeln( I:2, Vertices[2 * Indices[3 * I + 0]], ' ', Vertices[2 * Indices[3 * I + 0] + 1]);
    Writeln('  ', Vertices[2 * Indices[3 * I + 1]], ' ', Vertices[2 * Indices[3 * I + 2] + 1]);
    Writeln('  ', Vertices[2 * Indices[3 * I + 2]], ' ', Vertices[2 * Indices[3 * I + 2] + 1]);
  end;

  tessDeleteTess(tess);
end.
```

I've tried to save implementation the same as much as possible as well to
simplify side-by-side comparison and debugging, although it is necessary to
change names of some types to avoid ambiuities, and their placements to get rid
of circular units dependecies.
