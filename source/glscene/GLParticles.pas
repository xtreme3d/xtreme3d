// GLParticles
{: Particle systems for GLScene, based on replication of full-featured scene objects.<p>

	<b>History : </b><font size=-1><ul>
      <li>27/07/04 - EG - Added KillParticles
      <li>18/04/04 - EG - Added Before/After events
      <li>12/07/01 - EG - Fixed FEdgeColor memory leak
      <li>18/07/01 - EG - VisibilityCulling compatibility changes
		<li>17/04/00 - EG - Added Assign, Removed ActivateParticle
	   <li>16/04/00 - EG - Creation
	</ul></font>
}
unit GLParticles;

interface

uses Classes, GLScene, VectorGeometry, OpenGL1x, GLTexture, GLMisc;

type

   TGLParticleEvent = procedure (Sender : TObject; particle : TGLBaseSceneObject) of object;

	// TGLParticles
	//
	{: Manager object of a particle system.<p>
		Particles in a TGLParticles system are described as normal scene objects,
		however their children are to be :<ul>
		<li>"particle template" : the first object (index=0), this one will be
			duplicated to create new particles, it does not receive progression
			events and is visible at design-time only.
		<li>"live particle" : the other objects (index>0), this ones are rendered
			and receive progression events.
		</ul><br>TGLParticles may also maintain an internal, non-persistent
		("freezed") set of objects : the allocated objects pool. Why ? Creating
		and freeing objects takes cpu-cycles, especially for the TComponent class,
		and GLScene objects are TComponent. To reduce this load (and at the expense
		of memory space), the particle systems can move "dead" particles to a pool
		instead of freeing them, and will pick in the pool instead of creating
		new objects when new particles are requested. To take advantage of this
		behaviour, you should set the ParticlePoolSize property to a non-null
		value and use the KillParticle function instead of "Free" to kill a
      particle.<br>
      All direct access to a TGLParticles children should be avoided.<p>
      For high-performance particle systems of basic particles, you should
      look into GLParticleFX instead, TGLParticles being rather focused on
      complex particles.
	}
	TGLParticles = class (TGLImmaterialSceneObject)
		private
			{ Private Declarations }
			FCubeSize : TGLFloat;
			FEdgeColor : TGLColor;
			FVisibleAtRunTime : Boolean;
			particlePool : TList;
         FParticlePoolSize : Integer;
         FOnCreateParticle : TGLParticleEvent;
         FOnActivateParticle : TGLParticleEvent;
         FOnKillParticle : TGLParticleEvent;
         FOnDestroyParticle : TGLParticleEvent;
         FOnBeforeRenderParticles, FOnAfterRenderParticles : TDirectRenderEvent;

		protected
			{ Protected Declarations }
			procedure SetCubeSize(const val : TGLFloat);
			procedure SetEdgeColor(const val : TGLColor);
			procedure SetVisibleAtRunTime(const val : Boolean);
			procedure SetParticlePoolSize(val : Integer);

			procedure ClearParticlePool;

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
			procedure BuildList(var rci : TRenderContextInfo); override;
			procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
			procedure DoProgress(const progressTime : TProgressTimes); override;

			{: Request creation of a new particle.<p>
				Particle will be either created or retrieved from the particlePool. }
			function CreateParticle : TGLBaseSceneObject;
			{: Kill given particle.<p>
				If particlePool is not full, particle will be sent to the pool,
				if not, it will be freed. }
			procedure KillParticle(aParticle : TGLBaseSceneObject);
         {: Kill all particles. }
         procedure KillParticles;

		published
			{ Published Declarations }
			property CubeSize : TGLFloat read FCubeSize write SetCubeSize;
			property EdgeColor : TGLColor read FEdgeColor write SetEdgeColor;
			property VisibleAtRunTime : Boolean read FVisibleAtRunTime write SetVisibleAtRunTime default False;

			{: Size of the particle pool (for storing killed particles).<p>
            Default size is zero, meaning the particlePool is disabled. }
			property ParticlePoolSize : Integer read FParticlePoolSize write SetParticlePoolSize default 0; 

         {: Fired a particle has been created as a template duplicate.<p>
            When the event is triggered, the particle has yet been added  to
            the scene. }
         property OnCreateParticle : TGLParticleEvent read FOnCreateParticle write FOnCreateParticle;
         {: Fired when a particle will get in the "live" list.<p>
            The particle has just been "Assigned" with the template, may happen
            after a creation or a pick from the particle pool. }
         property OnActivateParticle : TGLParticleEvent read FOnActivateParticle write FOnActivateParticle;
			{: Triggered when a particle is killed.<p>
            When the event is fired, the particle is still parented, after this
            event, the particle will either go to the pool or be destroyed if
            the pool is full. }
         property OnKillParticle : TGLParticleEvent read FOnKillParticle write FOnKillParticle;
         {: Triggered just before destroying a particle.<p>
            The particle can be in the pool (ie. not parented). }
         property OnDestroyParticle : TGLParticleEvent read FOnDestroyParticle write FOnDestroyParticle;
         {: Fired before rendering the first of the particles. }
         property OnBeforeRenderParticles : TDirectRenderEvent read FOnBeforeRenderParticles write FOnBeforeRenderParticles;
         {: Fired after rendering the last of the particles. }
         property OnAfterRenderParticles : TDirectRenderEvent read FOnAfterRenderParticles write FOnAfterRenderParticles;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

//----------------- TGLParticles -----------------------------------------------------

// Create
//
constructor TGLParticles.Create(AOwner : TComponent);
begin
	inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
	FCubeSize:=1;
	FEdgeColor:=TGLColor.Create(Self);
	FEdgeColor.Initialize(clrWhite);
	particlePool:=TList.Create;
end;

// Destroy
//
destructor TGLParticles.Destroy;
begin
   FEdgeColor.Free;
	ClearParticlePool;
	particlePool.Free;
	inherited;
end;

// Assign
//
procedure TGLParticles.Assign(Source: TPersistent);
begin
	if Source is TGLParticles then begin
		FCubeSize:=TGLParticles(Source).FCubeSize;
		FEdgeColor.Color:=TGLParticles(Source).FEdgeColor.Color;
		FVisibleAtRunTime:=TGLParticles(Source).FVisibleAtRunTime;
		ClearParticlePool;
		FParticlePoolSize:=TGLParticles(Source).FParticlePoolSize;
		FOnCreateParticle:=TGLParticles(Source).FOnCreateParticle;
		FOnActivateParticle:=TGLParticles(Source).FOnActivateParticle;
		FOnKillParticle:=TGLParticles(Source).FOnKillParticle;
		FOnDestroyParticle:=TGLParticles(Source).FOnDestroyParticle;
	end;
	inherited Assign(Source);
end;

// ClearParticlePool
//
procedure TGLParticles.ClearParticlePool;
var
	particle : TGLBaseSceneObject;
	i : Integer;
begin
	if Assigned(FOnDestroyParticle) then begin
		for i:=0 to particlePool.Count-1 do begin
			particle:=TGLBaseSceneObject(particlePool[i]);
			FOnDestroyParticle(Self, particle);
			particle.Free;
		end;
	end else for i:=0 to particlePool.Count-1 do
		TGLBaseSceneObject(particlePool[i]).Free;
	particlePool.Clear;
end;

// BuildList
//
procedure TGLParticles.BuildList(var rci : TRenderContextInfo);
var
	mi, ma : Single;
begin
   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
   glDisable(GL_LIGHTING);
   glEnable(GL_LINE_STIPPLE);
   glEnable(GL_LINE_SMOOTH);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glLineWidth(1);
   glLineStipple(1, $AAAA);
   ma:=FCubeSize*0.5;
   mi:=-ma;
   with EdgeColor do glColor3f(Color[0], Color[1], Color[2]);
   glBegin(GL_LINE_STRIP);
      // front face
      glVertex3f(ma, mi, mi); glVertex3f(ma, ma, mi);
      glVertex3f(ma, ma, ma); glVertex3f(ma, mi, ma);
      glVertex3f(ma, mi, mi);
      // partial up back fac
      glVertex3f(mi, mi, mi); glVertex3f(mi, mi, ma);
      glVertex3f(mi, ma, ma); glVertex3f(mi, ma, mi);
      // right side low
      glVertex3f(ma, ma, mi);
   glEnd;
   glBegin(GL_LINES);
      // right high
      glVertex3f(ma, ma, ma);	glVertex3f(mi, ma, ma);
      // back low
      glVertex3f(mi, mi, mi); glVertex3f(mi, ma, mi);
      // left high
      glVertex3f(ma, mi, ma); glVertex3f(mi, mi, ma);
   glEnd;
   glPopAttrib;
end;

// DoRender
//
procedure TGLParticles.DoRender(var rci : TRenderContextInfo;
                                renderSelf, renderChildren : Boolean);
begin
   if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
      BuildList(rci);
   if Assigned(FOnBeforeRenderParticles) then
      FOnBeforeRenderParticles(Self, rci);
   if csDesigning in ComponentState then begin
      // design-time, everything is visible for user convenience
      if Count>0 then
         Self.RenderChildren(0, Count-1, rci);
   end else begin
      // run-time, template is NOT visible
      if Count>1 then
         Self.RenderChildren(1, Count-1, rci);
   end;
   if Assigned(FOnAfterRenderParticles) then
      FOnAfterRenderParticles(Self, rci);
end;

// DoProgress
//
procedure TGLParticles.DoProgress(const progressTime : TProgressTimes);
var
   i : Integer;
begin
	for i:=Count-1 downto 1 do
		Children[i].DoProgress(progressTime);
	Behaviours.DoProgress(progressTime);
   if Assigned(OnProgress) then with progressTime do
		OnProgress(Self, deltaTime, newTime);
end;

// SetCubeSize
//
procedure TGLParticles.SetCubeSize(const val : TGLFloat);
begin
	if val<>FCubeSize then begin
		FCubeSize:=val;
		StructureChanged;
	end;
end;

// SetEdgeColor
//
procedure TGLParticles.SetEdgeColor(const val : TGLColor);
begin
	if val<>FEdgeColor then begin
		FEdgeColor.Assign(val);
		StructureChanged;
	end;
end;

// SetVisibleAtRunTime
//
procedure TGLParticles.SetVisibleAtRunTime(const val : Boolean);
begin
	if val<>FVisibleAtRunTime then begin
		FVisibleAtRunTime:=val;
		StructureChanged;
	end;
end;

// SetParticlePoolSize
//
procedure TGLParticles.SetParticlePoolSize(val : Integer);
var
   particle : TGLBaseSceneObject;
begin
	if val<0 then val:=0;
	if FParticlePoolSize<>val then begin
		FParticlePoolSize:=val;
      with particlePool do while Count>val do begin
         particle:=TGLBaseSceneObject(Items[Count-1]);
         if Assigned(FOnDestroyParticle) then
            FOnDestroyParticle(Self, particle);
         particle.Free;
         Delete(Count-1);
      end;
   end;
end;

// CreateParticle
//
function TGLParticles.CreateParticle : TGLBaseSceneObject;
begin
   if Count>0 then begin
		if particlePool.Count>0 then begin
			Result:=TGLBaseSceneObject(particlePool[particlePool.Count-1]);
			particlePool.Delete(particlePool.Count-1);
			Result.Assign(Children[0]);
		end else begin
         Result:=TGLSceneObjectClass(Children[0].ClassType).Create(Self);
			Result.Assign(Children[0]);
			if Assigned(FOnCreateParticle) then
				FOnCreateParticle(Self, Result);
		end;
		AddChild(Result);
		if Assigned(FOnActivateParticle) then
         FOnActivateParticle(Self, Result);
   end else Result:=nil;
end;

// KillParticle
//
procedure TGLParticles.KillParticle(aParticle : TGLBaseSceneObject);
begin
	Assert(aParticle.Parent=Self, 'KillParticle : particle is not mine !');
   if Assigned(FOnKillParticle) then
      FOnKillParticle(Self, aParticle);
	if particlePool.Count<FParticlePoolSize then begin
		Remove(aParticle, False);
		particlePool.Add(aParticle)
	end else begin
      if Assigned(FOnDestroyParticle) then
         FOnDestroyParticle(Self, aParticle);
      aParticle.Free;
   end;
end;

// KillParticles
//
procedure TGLParticles.KillParticles;
begin
   while Count>1 do
      KillParticle(Children[Count-1]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClass(TGLParticles);

end.

