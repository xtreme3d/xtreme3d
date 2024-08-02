// Pipe functions by FireRun

function PipeCreate(divs,slic, parent: real): real; cdecl;
var
  pipe: TGLPipe;
begin
    if not (parent=0) then
    pipe:=TGLPipe.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    pipe:=TGLPipe.CreateAsChild(scene.Objects);
	
	pipe.Division:=trunc(divs);
	pipe.Slices:=trunc(slic);
  result:=ObjToReal(pipe);
end;

function PipeAddNode(pipe, x,y,z: real): real; cdecl;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(RealToPtr(pipe));
  pipeObj.AddNode(x,y,z);
  result := 1;
end;

function PipeSetDivision(pipe, divs: real): real; cdecl;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(RealToPtr(pipe));
  pipeObj.Division:=trunc(divs);
  result := 1;
end;

function PipeSetSplineMode(pipe, mode: real): real; cdecl;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(RealToPtr(pipe));
  if mode = 0 then pipeObj.SplineMode := lsmLines;
  if mode = 1 then pipeObj.SplineMode := lsmCubicSpline;
  if mode = 2 then pipeObj.SplineMode := lsmBezierSpline;
  if mode = 3 then pipeObj.SplineMode := lsmNURBSCurve;
  if mode = 4 then pipeObj.SplineMode := lsmSegments;
  result := 1;
end;

function PipeDeleteNode(pipe, ind: real): real; cdecl;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(RealToPtr(pipe));
  pipeObj.Nodes.Delete(trunc(ind));
  result := 1;
end;

function PipeSetRadius(pipe, rad: real): real; cdecl;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(RealToPtr(pipe));
  pipeObj.Radius:=rad;
  result := 1;
end;

function PipeSetNode(pipe,ind, x,y,z: real): real; cdecl;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(RealToPtr(pipe));
  pipeObj.Nodes[trunc(ind)].X:=x;
  pipeObj.Nodes[trunc(ind)].Y:=y;
  pipeObj.Nodes[trunc(ind)].Z:=z;
  result := 1;
end;

function PipeSetSlices(pipe,slic: real): real; cdecl;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(RealToPtr(pipe));
  pipeObj.Slices:=trunc(slic);
  result := 1;
end;
