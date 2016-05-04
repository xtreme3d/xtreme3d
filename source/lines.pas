function LinesCreate(parent: real): real; stdcall;
var
  li: TGLLines;
begin
  if not (parent=0) then
    li := TGLLines.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    li := TGLLines.CreateAsChild(scene.Objects);
  result := Integer(li);
end;

function LinesAddNode(lines, x, y, z: real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  li.AddNode(x, y, z);
  result := 1.0;
end;

function LinesDeleteNode(lines, ind: real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  li.Nodes.Delete(trunc64(ind));
  result := 1.0;
end;

function LinesSetColors(lines,
  linecolor, linealpha,
  nodecolor, nodealpha : real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  li.LineColor.AsWinColor := TColor(trunc64(linecolor));
  li.LineColor.Alpha := linealpha;
  li.NodeColor.AsWinColor := TColor(trunc64(nodecolor));
  li.NodeColor.Alpha := nodealpha;
  result := 1.0;
end;

function LinesSetSize(lines, linewidth, nodesize: real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  li.LineWidth := linewidth;
  li.NodeSize := nodesize;
  result := 1.0;
end;

function LinesSetSplineMode(lines, lsm: real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  if lsm = 0 then li.SplineMode := lsmLines;
  if lsm = 1 then li.SplineMode := lsmCubicSpline;
  if lsm = 2 then li.SplineMode := lsmBezierSpline;
  if lsm = 3 then li.SplineMode := lsmNURBSCurve;
  if lsm = 4 then li.SplineMode := lsmSegments;
  result := 1.0;
end;

function LinesSetNodesAspect(lines, lna: real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  if lna = 0 then li.NodesAspect := lnaInvisible;
  if lna = 1 then li.NodesAspect := lnaAxes;
  if lna = 2 then li.NodesAspect := lnaCube;
  if lna = 3 then li.NodesAspect := lnaDodecahedron;
  result := 1.0;
end;

function LinesSetDivision(lines, division: real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  li.Division := trunc64(division);
  result := 1.0;
end;

