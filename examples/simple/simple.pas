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
