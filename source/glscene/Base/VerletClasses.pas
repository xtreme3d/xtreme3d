//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VerletClasses<p>

   Base Verlet modelling/simulation classes.<p>
   This unit is generic, GLScene-specific sub-classes are in GLVerletClasses.<p>

   Note that currently, the SatisfyConstraintForEdge methods push the nodes in
   the edge uniformly - it should push the closer node more for correct physics.
   It's a matter of leverage. <p>

	<b>History : </b><font size=-1><ul>
      <li>14/04/04 - MF - Fixed force for springs, was referring to deltaP...
      <li>13/04/04 - MF - Minor drag changes
      <li>13/04/04 - EG - Added TVCHeightField and TVCSlider, fixed TVCFloor
                          and TVFSpring, altered the way world Drag operates 
      <li>06/03/04 - MF - Small updates to accomodate hair
      <li>11/07/03 - EG - Optimized TVCCube collider
      <li>11/07/03 - MF - A bit of a documentation effort
      <li>10/07/03 - MF - Verlets now use spatial partitioning objects to speed
                          up space queries
      <li>10/07/03 - MF - Renaming TVerletAssembly to TVerletWorld
      <li>24/06/03 - MF - Added force kickbacks for integration with external
                          physics. Needs to be split into force+torque and add
                          friction to the kickback
      <li>19/06/03 - MF - Added TVerletGlobalConstraint.SatisfyConstraintForEdge
                          and implemented for TVCSphere and TVCCapsule 
      <li>19/06/03 - MF - Added friction to TVCCylinder
      <li>19/06/03 - MF - Added surface normals to all colliders - surface
                          normal is identical to Normalize(Movement)!
      <li>18/06/03 - MF - Moved FrictionRatio to TVerletGlobalFrictionConstraint
      <li>18/06/03 - EG - Updated TVCCapsule
      <li>18/06/03 - MF - Updated TVCFloor to use a normal and a point
      <li>18/06/03 - MF - Added TVCCapsule
      <li>17/06/03 - MF - Added TVFAirResistance
      <li>17/06/03 - MF - Added TVCCube collider
      <li>16/06/03 - MF - Fixed TVFSpring.SetRestlengthToCurrent
      <li>24/07/02 - EG - Added TVCCylinder
      <li>18/07/02 - EG - Improved forces & constraints
      <li>23/06/02 - EG - Stricter encapsulation, fixed some leaks,
                          Various optimizations (+25%)
      <li>21/06/02 - EG - Creation (original code by Mattias Fagerlund)
   </ul>
}
unit VerletClasses;

interface

uses Classes, VectorGeometry, SysUtils, VectorLists, SpatialPartitioning,
  GeometryBB;

const
   G_DRAG = 0.0001;
   cDEFAULT_CONSTRAINT_FRICTION = 0.6;

type
   TVerletEdgeList = class;
   TVerletWorld = class;

   TVerletProgressTimes = packed record
      deltaTime, newTime : Double;
      sqrDeltaTime, invSqrDeltaTime : Single;
   end;

   // TVerletNode
   //
   {: Basic verlet node }
   TVerletNode = class(TSpacePartitionLeaf)
      private
			{ Private Declarations }
         FForce : TAffineVector;
         FOwner : TVerletWorld;
         FWeight, FInvWeight : Single;
         FRadius : Single;
         FNailedDown : Boolean;
         FFriction : Single;
         FChangedOnStep : Integer;
         function GetSpeed: TAffineVector;

		protected
			{ Protected Declarations }
         FLocation, FOldLocation : TAffineVector;

         procedure SetLocation(const Value: TAffineVector);virtual;

         procedure SetWeight(const value : Single);

         procedure AfterProgress; virtual;

      public
			{ Public Declarations }
         constructor CreateOwned(const aOwner : TVerletWorld); virtual;
         destructor Destroy; override;

         {: Applies friction }
         procedure ApplyFriction(const friction, penetrationDepth : Single;
                                 const surfaceNormal : TAffineVector);
         {: Simple and less accurate method for friction }
         procedure OldApplyFriction(const friction, penetrationDepth : Single);

         {: Perform Verlet integration }
         procedure Verlet(const vpt : TVerletProgressTimes); virtual;

         {: Initlializes the node. For the base class, it just makes sure that
         FOldPosition = FPosition, so that speed is zero }
         procedure Initialize; dynamic;

         {: Calculates the distance to another node }
         function DistanceToNode(const node : TVerletNode) : Single;

         {: Calculates the movement of the node }
         function GetMovement : TAffineVector;

         {: The TVerletNode inherits from TSpacePartitionLeaf, and it needs to
         know how to publish itself. The owner ( a TVerletWorld ) has a spatial
         partitioning object}
         procedure UpdateCachedAABBAndBSphere; override;

         {: The VerletWorld that owns this verlet }
         property Owner : TVerletWorld read FOwner;

         {: The location of the verlet }
         property Location : TAffineVector read FLocation write SetLocation;

         {: The old location of the verlet. This is used for verlet integration }
         property OldLocation : TAffineVector read FOldLocation write FOldLocation;

         {: The radius of the verlet node - this has been more or less deprecated }
         property Radius : Single read FRadius write FRadius;

         {: A sum of all forces that has been applied to this verlet node during a step }
         property Force : TAffineVector read FForce write FForce;

         {: If the node is nailed down, it can't be moved by either force,
         constraint or verlet integration - but you can still move it by hand }
         property NailedDown : Boolean read FNailedDown write FNailedDown;

         {: The weight of a node determines how much it's affected by a force }
         property Weight : Single read FWeight write SetWeight;

         {: InvWeight is 1/Weight, and is kept up to date automatically }
         property InvWeight : Single read FInvWeight;

         {: Returns the speed of the verlet node. Speed = Movement / deltatime }
         property Speed : TAffineVector read GetSpeed;

         {: Each node has a friction that effects how it reacts during contacts.}
         property Friction : Single read FFriction write FFriction;

         {: What phyisics step was this node last changed? Used to keep track
         of when the spatial partitioning needs to be updated }
         property ChangedOnStep : Integer read FChangedOnStep;
   end;

   TVerletNodeClass = class of TVerletNode;

   // TVerletNodeList
   //
   TVerletNodeList = class(TList)
      private
			{ Private Declarations }
         function GetItems(i : Integer): TVerletNode;
         procedure SetItems(i : Integer; const value : TVerletNode);

      public
			{ Public Declarations }
         property Items[i : Integer] : TVerletNode read GetItems write SetItems; default;
   end;

   // TVerletConstraint
   //
   TVerletConstraint = class (TObject)
      private
			{ Private Declarations }
         FOwner : TVerletWorld;
         FEnabled : Boolean;
         FTag : Integer;

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); virtual;
         destructor Destroy; override;

         {: Updates the position of one or several nodes to make sure that they
            don't violate the constraint }
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); virtual; abstract;
         {: Notifies removal of a node }
         procedure RemoveNode(const aNode : TVerletNode); virtual; abstract;
         {: Method that's fired before the physics iterations are performed}
         procedure BeforeIterations; virtual;

         {: Onwer of the constraint }
         property Owner : TVerletWorld read FOwner;
         {: Determines if the constraint should be enforced or not }
         property Enabled : Boolean read FEnabled write FEnabled;
         {: Tag field reserved for the user. }
         property Tag : Integer read FTag write FTag;
   end;

   // TVerletDualConstraint
   //
   TVerletDualConstraint = class (TVerletConstraint)
      private
			{ Private Declarations }
         FNodeA, FNodeB : TVerletNode;

      public
			{ Public Declarations }
         procedure RemoveNode(const aNode : TVerletNode); override;

         {: Reference to NodeA. }
         property NodeA : TVerletNode read FNodeA write FNodeA;
         {: Reference to NodeB. }
         property NodeB : TVerletNode read FNodeB write FNodeB;
   end;

   // TVerletGroupConstraint
   //
   TVerletGroupConstraint = class (TVerletConstraint)
      private
			{ Private Declarations }
         FNodes : TVerletNodeList;

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); override;
         destructor Destroy; override;

         procedure RemoveNode(const aNode : TVerletNode); override;

         {: The list of nodes that this constraint will effect}
         property Nodes : TVerletNodeList read FNodes;
   end;

   // TVerletEdge
   // Verlet edges simulate rigid collission edges
   TVerletEdge = class(TSpacePartitionLeaf)
      private
			{ Private Declarations }
         FNodeA: TVerletNode;
         FNodeB: TVerletNode;

      public
			{ Public Declarations }
         {: The TVerletEdge inherits from TSpacePartitionLeaf, and it needs to
         know how to publish itself. The owner ( a TVerletWorld ) has a spatial
         partitioning object}
         procedure UpdateCachedAABBAndBSphere; override;

         constructor CreateEdgeOwned(const aNodeA, aNodeB : TVerletNode);

         {: One of the nodes in the edge }
         property NodeA : TVerletNode read FNodeA write FNodeA;

         {: One of the nodes in the edge }
         property NodeB : TVerletNode read FNodeB write FNodeB;
   end;

   TVerletEdgeList = class(TList)
      private
			{ Private Declarations }
         function GetItems(i: Integer): TVerletEdge;
         procedure SetItems(i: Integer; const Value: TVerletEdge);

      public
			{ Public Declarations }
         property Items[i : Integer] : TVerletEdge read GetItems write SetItems; default;
   end;

   // TVerletGlobalConstraint
   //
   TVerletGlobalConstraint = class (TVerletConstraint)
      private
			{ Private Declarations }
         FKickbackForce: TAffineVector;
         FKickbackTorque : TAffineVector;
         FLocation: TAffineVector;
         procedure SetLocation(const Value: TAffineVector); virtual;

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); override;
         destructor Destroy; override;

         procedure RemoveNode(const aNode : TVerletNode); override;
         procedure BeforeIterations; override;

         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;
         procedure SatisfyConstraintForNode(const aNode : TVerletNode;
                        const iteration, maxIterations : Integer); virtual; abstract;
         procedure SatisfyConstraintForEdge(const aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); virtual;

         property Location : TAffineVector read FLocation write SetLocation;

         {: The force that this collider has experienced while correcting the
         verlet possitions. This force can be applied to ODE bodies, for
         instance }
         property KickbackForce : TAffineVector read FKickbackForce write FKickbackForce;
         {: The torque that this collider has experienced while correcting the
         verlet possitions, in reference to the center of the collider. The
         torque  force can be applied to ODE bodies, but it must first be
         translated. A torque can be trasnalted by <p>
         <p>
         EM(b) = EM(a) + EF x VectorSubtract(b, a). <p>
         <P>
         Simply adding the torque to the body will NOT work correctly. See
         TranslateKickbackTorque}
         property KickbackTorque : TAffineVector read FKickbackTorque write FKickbackTorque;

         procedure AddKickbackForceAt(const Pos : TAffineVector; const Force : TAffineVector);

         function TranslateKickbackTorque(const TorqueCenter : TAffineVector) : TAffineVector;
   end;

   // TVerletGlobalFrictionConstraint
   //
   TVerletGlobalFrictionConstraint = class (TVerletGlobalConstraint)
      private
			{ Private Declarations }
         FFrictionRatio: Single;

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); override;
         
         property FrictionRatio : Single read FFrictionRatio write FFrictionRatio;
   end;

   TVerletGlobalFrictionConstraintSP = class(TVerletGlobalFrictionConstraint)
      public
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;
         procedure PerformSpaceQuery; virtual; abstract;
   end;

   TVerletGlobalFrictionConstraintSphere = class(TVerletGlobalFrictionConstraintSP)
      private
         FCachedBSphere: TBSphere;

         procedure SetLocation(const Value: TAffineVector); override;
      public
         procedure UpdateCachedBSphere;
         procedure PerformSpaceQuery; override;
         function GetBSphere : TBSphere; virtual; abstract;

         property CachedBSphere : TBSphere read FCachedBSphere;
   end;

   TVerletGlobalFrictionConstraintBox = class(TVerletGlobalFrictionConstraintSP)
      private
         FCachedAABB: TAABB;

         procedure SetLocation(const Value: TAffineVector); override;
      public
         procedure UpdateCachedAABB;

         procedure PerformSpaceQuery; override;
         function GetAABB : TAABB; virtual; abstract;

         property CachedAABB : TAABB read FCachedAABB;
   end;

   // TVerletConstraintList
   //
   TVerletConstraintList = class(TList)
      private
			{ Private Declarations }
         function GetItems(i : Integer): TVerletConstraint;
         procedure SetItems(i : Integer; const Value: TVerletConstraint);

      public
			{ Public Declarations }
         property Items[i : Integer] : TVerletConstraint read GetItems write SetItems; default;
   end;

   // TVerletForce
   //
   {: Generic verlet force. }
   TVerletForce = class (TObject)
      private
			{ Private Declarations }
         FOwner : TVerletWorld;

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); virtual;
         destructor Destroy; override;

         //: Implementation should add force to force resultant for all relevant nodes
         procedure AddForce(const vpt : TVerletProgressTimes); virtual; abstract;

         //: Notifies removal of a node
         procedure RemoveNode(const aNode : TVerletNode); virtual; abstract;

         property Owner : TVerletWorld read FOwner;
   end;

   // TVerletDualForce
   //
   {: A verlet force that applies to two specified nodes. }
   TVerletDualForce = class (TVerletForce)
      private
			{ Private Declarations }
         FNodeA, FNodeB : TVerletNode;

      public
			{ Public Declarations }
         procedure RemoveNode(const aNode : TVerletNode); override;

         {: Reference to NodeA. }
         property NodeA : TVerletNode read FNodeA write FNodeA;
         {: Reference to NodeB. }
         property NodeB : TVerletNode read FNodeB write FNodeB;
   end;

   // TVerletGroupForce
   //
   {: A verlet force that applies to a specified group of nodes. }
   TVerletGroupForce = class (TVerletForce)
      private
			{ Private Declarations }
         FNodes : TVerletNodeList;

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); override;
         destructor Destroy; override;

         procedure RemoveNode(const aNode : TVerletNode); override;

         {: Nodes of the force group, referred, NOT owned. }
         property Nodes : TVerletNodeList read FNodes;
   end;

   // TVerletGlobalForce
   //
   {: A global force (applied to all verlet nodes). }
   TVerletGlobalForce = class (TVerletForce)
      private
			{ Private Declarations }

      public
			{ Public Declarations }
         procedure RemoveNode(const aNode : TVerletNode); override;

         procedure AddForce(const vpt : TVerletProgressTimes); override;
         procedure AddForceToNode(const aNode : TVerletNode); virtual; abstract;
   end;

   // TVerletForceList
   //
   TVerletForceList = class (TList)
      private
			{ Private Declarations }
         function GetItems(i : Integer): TVerletForce;
         procedure SetItems(i : Integer; const Value: TVerletForce);

      public
			{ Public Declarations }
         property Items[i : Integer] : TVerletForce read GetItems write SetItems; default;
   end;

   TVCStick = class;
   TVFSpring = class;
   TVCSlider = class;

   TUpdateSpacePartion = (uspEveryIteration, uspEveryFrame, uspNever);
   TCollisionConstraintTypes = (cctEdge, cctNode);
   TCollisionConstraintTypesSet = set of TCollisionConstraintTypes;
   
   // TVerletWorld
   //
   TVerletWorld = class (TObject)
      private
			{ Private Declarations }
         FIterations : Integer;
         FNodes : TVerletNodeList;
         FConstraints : TVerletConstraintList;
         FForces : TVerletForceList;
         FMaxDeltaTime, FSimTime : Single;
         FDrag : Single;
         FCurrentDeltaTime: Single;
         FInvCurrentDeltaTime : Single;
         FSolidEdges: TVerletEdgeList;
         FSpacePartition: TBaseSpacePartition;
         FCurrentStepCount: Integer;
         FUpdateSpacePartion: TUpdateSpacePartion;
         FCollisionConstraintTypes: TCollisionConstraintTypesSet;
         FConstraintsWithBeforeIterations: TVerletConstraintList;
         FVerletNodeClass: TVerletNodeClass;
         FInertia: Boolean;
         FInertaPauseSteps : Integer;

		protected
			{ Protected Declarations }
         procedure AccumulateForces(const vpt : TVerletProgressTimes); virtual;
         procedure Verlet(const vpt : TVerletProgressTimes); virtual;
         procedure SatisfyConstraints(const vpt : TVerletProgressTimes); virtual;

         procedure DoUpdateSpacePartition;

      public
			{ Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         function  AddNode(const aNode : TVerletNode) : Integer;
         procedure RemoveNode(const aNode : TVerletNode);
         
         function  AddConstraint(const aConstraint : TVerletConstraint) : Integer;
         procedure RemoveConstraint(const aConstraint : TVerletConstraint);

         function  AddForce(const aForce : TVerletForce) : Integer;
         procedure RemoveForce(const aForce : TVerletForce);

         procedure AddSolidEdge(const aNodeA, aNodeB : TVerletNode);

         procedure PauseInertia(const IterationSteps : Integer);

         function CreateOwnedNode(const location : TAffineVector;
                                  const aRadius : Single = 0;
                                  const aWeight : Single = 1) : TVerletNode;
         function CreateStick(const aNodeA, aNodeB : TVerletNode;
                              const Slack : Single = 0) : TVCStick;
         function CreateSpring(const aNodeA, aNodeB : TVerletNode;
                               const aStrength, aDamping : Single; const aSlack : Single = 0) : TVFSpring;
         function CreateSlider(const aNodeA, aNodeB : TVerletNode;
                               const aSlideDirection : TAffineVector) : TVCSlider;

         procedure Initialize; dynamic;
         procedure CreateOctree(const OctreeMin, OctreeMax : TAffineVector;
          const LeafThreshold, MaxTreeDepth : Integer);

         function Progress(const deltaTime, newTime : Double) : Integer; virtual;

         function FirstNode : TVerletNode;
         function LastNode : TVerletNode;

         property Drag : Single read FDrag write FDrag;
         property Iterations : Integer read FIterations write FIterations;
         property Nodes : TVerletNodeList read FNodes;
         property Constraints : TVerletConstraintList read FConstraints;
         property ConstraintsWithBeforeIterations : TVerletConstraintList read FConstraintsWithBeforeIterations;

         property SimTime : Single read FSimTime write FSimTime;
         property MaxDeltaTime : Single read FMaxDeltaTime write FMaxDeltaTime;

         property CurrentDeltaTime : Single read FCurrentDeltaTime;
         property SolidEdges : TVerletEdgeList read FSolidEdges write FSolidEdges;
         property CurrentStepCount : Integer read FCurrentStepCount;
         property SpacePartition: TBaseSpacePartition read FSpacePartition;
         property UpdateSpacePartion : TUpdateSpacePartion read FUpdateSpacePartion write FUpdateSpacePartion;
         property CollisionConstraintTypes : TCollisionConstraintTypesSet read FCollisionConstraintTypes write FCollisionConstraintTypes;
         property VerletNodeClass : TVerletNodeClass read FVerletNodeClass write FVerletNodeClass;
         property Inertia : boolean read FInertia write FInertia;
   end;

   // TVFGravity
   //
   TVFGravity = class(TVerletGlobalForce)
      private
			{ Private Declarations }
         FGravity : TAffineVector;

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); override;

         procedure AddForceToNode(const aNode : TVerletNode); override;

         property Gravity : TAffineVector read FGravity write FGravity;
   end;

   // TVFAirResistance
   //
   TVFAirResistance = class(TVerletGlobalForce)
      private
			{ Private Declarations }
         FDragCoeff: Single;
         FWindDirection: TAffineVector;
         FWindMagnitude: Single;
         FWindChaos: Single;
         procedure SetWindDirection(const Value: TAffineVector);

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); override;
         procedure AddForceToNode(const aNode : TVerletNode); override;

         property DragCoeff : Single read FDragCoeff write FDragCoeff;
         property WindDirection : TAffineVector read FWindDirection write SetWindDirection;
         property WindMagnitude : Single read FWindMagnitude write FWindMagnitude;
         {: Measures how chaotic the wind is, as a fraction of the wind magnitude }
         property WindChaos : Single read FWindChaos write FWindChaos;
   end;

   // TVFSpring
   //
   TVFSpring = class (TVerletDualForce)
      private
			{ Private Declarations }
         FRestLength : Single;
         FStrength : Single;
         FDamping : Single;
         FSlack : Single;
         FForceFactor : Single;

      protected
         { Protected Declarations }
         procedure SetSlack(const value : Single);

      public
			{ Public Declarations }
         procedure AddForce(const vpt : TVerletProgressTimes); override;

         //: Must be invoked after adjust node locations or strength 
         procedure SetRestLengthToCurrent;

         property Strength : Single read FStrength write FStrength;
         property Damping : Single read FDamping write FDamping;
         property Slack : Single read FSlack write SetSlack;
   end;

   // TVCFloor
   //
   {: Floor collision constraint }
   TVCFloor = class (TVerletGlobalFrictionConstraintSP)
      private
			{ Private Declarations }
         FBounceRatio, FFloorLevel : Single;
         FNormal : TAffineVector;

      protected
         { Protected Declarations }
         procedure SetNormal(const value : TAffineVector);

      public
			{ Public Declarations }
         constructor Create(const aOwner : TVerletWorld); override;

         procedure PerformSpaceQuery; override;
         procedure SatisfyConstraintForNode(const aNode : TVerletNode;
                        const iteration, maxIterations : Integer); override;

         property BounceRatio : Single read FBounceRatio write FBounceRatio;
         property FloorLevel : Single read FFloorLevel write FFloorLevel;
         property Normal : TAffineVector read FNormal write SetNormal;
   end;

   TVCHeightField = class;
   TVCHeightFieldOnNeedHeight = function (hfConstraint : TVCHeightField; node : TVerletNode) : Single of object;

   // TVCHeightField
   //
   {: HeightField collision constraint (punctual!) }
   TVCHeightField = class (TVCFloor)
      private
			{ Private Declarations }
         FOnNeedHeight : TVCHeightFieldOnNeedHeight;

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(const aNode : TVerletNode;
                        const iteration, maxIterations : Integer); override;

         property OnNeedHeight : TVCHeightFieldOnNeedHeight read FOnNeedHeight write FOnNeedHeight;
   end;

   // TVCStick
   //
   {: Stick constraint.<p>
      Imposes a fixed distance between two nodes. }
   TVCStick = class (TVerletDualConstraint)
      private
			{ Private Declarations }
         FSlack : Single;
         FRestLength : Single;

      public
			{ Public Declarations }
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;
         procedure SetRestLengthToCurrent;

         property Slack : Single read FSlack write FSlack;
         property RestLength : Single read FRestLength write FRestLength;
   end;

   // TVCRigidBody
   //
   {: Rigid body constraint.<p>
      Regroups several nodes in a rigid body conformation, somewhat similar
      to a stick but for multiple nodes. <p>
      EXPERIMENTAL, DOES NOT WORK!
      }
   TVCRigidBody = class (TVerletGroupConstraint)
      private
			{ Private Declarations }
         FNodeParams : array of TAffineVector;
         FNodeCoords : array of TAffineVector;
         FNatMatrix, FInvNatMatrix : TAffineMatrix;

      protected
			{ Protected Declarations }
         procedure ComputeBarycenter(var barycenter : TAffineVector);
         procedure ComputeNaturals(const barycenter : TAffineVector;
                                   var natX, natY, natZ : TAffineVector);

      public
			{ Public Declarations }
         procedure ComputeRigidityParameters;
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;
   end;

   // TVCSlider
   //
   {: Slider constraint.<p>
      Imposes that two nodes be aligned on a defined direction, on which they
      can slide freely. Note that the direction is fixed and won't rotate
      with the verlet assembly!. }
   TVCSlider = class (TVerletDualConstraint)
      private
			{ Private Declarations }
         FSlideDirection : TAffineVector;
         FConstrained : Boolean;

      protected
         { Protected Declarations }
         procedure SetSlideDirection(const value : TAffineVector);

      public
			{ Public Declarations }
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;

         property SlideDirection : TAffineVector read FSlideDirection write SetSlideDirection;
         {: Constrain NodeB to the halfplane defined by NodeA and SlideDirection. } 
         property Constrained : Boolean read FConstrained write FConstrained;
   end;

   // TVCSphere
   //
   {: Sphere collision constraint. }
   TVCSphere = class (TVerletGlobalFrictionConstraintSphere)
      private
			{ Private Declarations }
         FRadius  : Single;

      public
			{ Public Declarations }
         function GetBSphere : TBSphere; override;
         procedure SatisfyConstraintForNode(const aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         procedure SatisfyConstraintForEdge(const aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); override;

         property Radius : Single read FRadius write FRadius;
   end;

   // TVCCylinder
   //
   {: Cylinder collision constraint.<p>
      The cylinder is considered infinite by this constraint. }
   TVCCylinder = class (TVerletGlobalFrictionConstraint)
      private
			{ Private Declarations }
         FAxis : TAffineVector;
         FRadius, FRadius2  : Single;

      protected
			{ Protected Declarations }
         procedure SetRadius(const val : Single);

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(const aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         {: A base point on the cylinder axis.<p>
            Can theoretically be anywhere, however, to reduce floating point
            precision issues, choose it in the area where collision detection
            will occur. }
         //property Base : TAffineVector read FBase write FBase;
         {: Cylinder axis vector.<p>
            Must be normalized. }
         property Axis : TAffineVector read FAxis write FAxis;
         {: Cylinder radius. }
         property Radius : Single read FRadius write SetRadius;
   end;

   // TVCCube
   //
   {: Cube collision constraint. }
   TVCCube = class (TVerletGlobalFrictionConstraintBox)
      private
			{ Private Declarations }
         FHalfSides : TAffineVector;
         FSides: TAffineVector;
         FDirection: TAffineVector;
         procedure SetSides(const Value: TAffineVector);

      public
			{ Public Declarations }
         function GetAABB : TAABB; override;

         procedure SatisfyConstraintForNode(const aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         // Broken and very slow!
         procedure SatisfyConstraintForEdge(const aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); override;//}

         property Direction : TAffineVector read FDirection write FDirection;
         property Sides : TAffineVector read FSides write SetSides;
   end;

   // TVCCapsule
   //
   {: Capsule collision constraint. }
   TVCCapsule = class (TVerletGlobalFrictionConstraintSphere)
      private
			{ Private Declarations }
         FAxis : TAffineVector;
         FRadius, FRadius2, FLength, FLengthDiv2 : Single;

      protected
			{ Protected Declarations }
         procedure SetAxis(const val : TAffineVector);
         procedure SetRadius(const val : Single);
         procedure SetLength(const val : Single);

      public
			{ Public Declarations }
         function GetBSphere: TBSphere; override;

         procedure SatisfyConstraintForNode(const aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         procedure SatisfyConstraintForEdge(const aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); override;

         // property Base : TAffineVector read FBase write FBase;
         property Axis : TAffineVector read FAxis write SetAxis;
         property Radius : Single read FRadius write SetRadius;
         property Length : Single read FLength write SetLength;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TVerletNode ------------------
// ------------------

// Create
//
constructor TVerletNode.CreateOwned(const aOwner : TVerletWorld);
begin

   inherited CreateOwned(aOwner.SpacePartition);
   if Assigned(aOwner) then
      aOwner.AddNode(Self);

   FWeight:=1;
   FInvWeight:=1;
   FRadius:=0;
   FFriction:=1;
end;

// Destroy
//
destructor TVerletNode.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveNode(Self);

   inherited;
end;

// ApplyFriction
//
{ TODO: Improve the friction calculations

  Friction = - NormalForce * FrictionConstant

  To compute the NormalForce, which is the force acting on the normal of the
  collider, we can use the fact that F = m*a.

  m is the weight of the node, a is the acceleration (retardation) caused by the
  collission.

  Acceleration := - PenetrationDepth / Owner.FCurrentDeltaTime;

  The force with which the node has been "stopped" from penetration
  NormalForce := Weight * Acceleration;

  This force should be applied to stopping the movement.
}
procedure TVerletNode.ApplyFriction(const friction, penetrationDepth : Single;
                                    const surfaceNormal : TAffineVector);
var
   frictionMove, move, moveNormal : TAffineVector;
   realFriction : Single;
begin
   if (penetrationDepth>0) then begin
       realFriction := friction*FFriction;
       if realFriction>0 then begin
           VectorSubtract(Location, OldLocation, move);
           moveNormal:=VectorScale(surfaceNormal, VectorDotProduct(move, surfaceNormal));
           frictionMove:=VectorSubtract(move, moveNormal);
           if penetrationDepth>Radius then
              ScaleVector(frictionMove, realFriction)
           else ScaleVector(frictionMove, realFriction*Sqrt(penetrationDepth/Radius));
           VectorAdd(OldLocation, frictionMove, FOldLocation);
       end;
   end;
end;

// OldApplyFriction
//
procedure TVerletNode.OldApplyFriction(const friction, penetrationDepth : Single);
var
   frictionMove, move : TAffineVector;
//   pd : Single;
begin
   VectorSubtract(Location, OldLocation, move);
   VectorScale(move, friction*FFriction, frictionMove);
   //pd:=Abs(penetrationDepth);
   //ScaleVector(frictionMove, friction*pd);
   VectorAdd(OldLocation, frictionMove, FOldLocation);
end;

// DistanceToNode
//
function TVerletNode.DistanceToNode(const node : TVerletNode) : Single;
begin
   Result:=VectorDistance(Location, node.Location);
end;

// GetMovement
//
function TVerletNode.GetMovement : TAffineVector;
begin
   Result:=VectorSubtract(Location, OldLocation);
end;

// Initialize
//
procedure TVerletNode.Initialize;
begin
   FOldLocation:=Location;
end;

// SetWeight
//
procedure TVerletNode.SetWeight(const value : Single);
begin
   FWeight:=value;
   if value<>0 then
      FInvWeight:=1/value
   else FInvWeight:=1;
end;

// Verlet
//
procedure TVerletNode.Verlet(const vpt : TVerletProgressTimes);
var
  newLocation, temp, move, accel : TAffineVector;
begin
   if NailedDown then begin
      FOldLocation:=Location;
   end else begin
      if Owner.Inertia then begin
         temp:=Location;
         VectorSubtract(Location, OldLocation, move);

         ScaleVector(move, 1-Owner.Drag);//*Sqr(deltaTime));

         VectorAdd(Location, move, newLocation);

         VectorScale(Force, vpt.sqrDeltaTime*FInvWeight, accel);
         AddVector(newLocation, accel);

         Location:=newLocation;
         FOldLocation:=temp;
      end else begin
         newLocation := Location;
         VectorScale(Force, vpt.sqrDeltaTime*FInvWeight, accel);
         AddVector(newLocation, accel);

         Location := newLocation;
         FOldLocation:=Location;
      end;
   end;
end;

// Updated
//
procedure TVerletNode.AfterProgress;
begin
   // nothing here, reserved for subclass use
end;

// ------------------
// ------------------ TVerletNodeList ------------------
// ------------------

// GetItems
//
function TVerletNodeList.GetItems(i : Integer) : TVerletNode;
begin
   Result:=Get(i);
end;

// SetItems
//
procedure TVerletNodeList.SetItems(i : Integer; const value : TVerletNode);
begin
   Put(i, value);
end;

function TVerletNode.GetSpeed: TAffineVector;
begin
  result := VectorScale(VectorSubtract(FLocation, FOldLocation), 1/Owner.CurrentDeltaTime);
end;

// ------------------
// ------------------ TVerletConstraint ------------------
// ------------------

// Create
//
constructor TVerletConstraint.Create(const aOwner : TVerletWorld);
begin
   inherited Create;
   if Assigned(aOwner) then
      aOwner.AddConstraint(Self);
   FEnabled:=True;
end;

// Destroy
//
destructor TVerletConstraint.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveConstraint(Self);
   inherited;
end;

// Create
//
procedure TVerletConstraint.BeforeIterations;
begin
  // NADA!
end;

// ------------------
// ------------------ TVerletDualConstraint ------------------
// ------------------

// RemoveNode
//
procedure TVerletDualConstraint.RemoveNode(const aNode : TVerletNode);
begin
   if FNodeA=aNode then
      FNodeA:=nil;
   if FNodeB=aNode then
      FNodeB:=nil;
   if (FNodeA=nil) and (FNodeA=nil) then
      Free;
end;

// ------------------
// ------------------ TVerletGroupConstraint ------------------
// ------------------

// Create
//
constructor TVerletGroupConstraint.Create(const aOwner : TVerletWorld);
begin
   inherited Create(aOwner);
   FNodes:=TVerletNodeList.Create;
end;

// Destroy
//
destructor TVerletGroupConstraint.Destroy;
begin
   FNodes.Free;
   inherited;
end;

// RemoveNode
//
procedure TVerletGroupConstraint.RemoveNode(const aNode : TVerletNode);
begin
   FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TVerletGlobalConstraint ------------------
// ------------------

// RemoveNode
//

procedure TVerletGlobalConstraint.AddKickbackForceAt(const Pos : TAffineVector; const Force: TAffineVector);
var
  dPos : TAffineVector;
begin
  // Sum forces
  AddVector(FKickbackForce, Force);

  // Sum torques
  dPos := VectorSubtract(Pos, FLocation);
  AddVector(FKickbackTorque, VectorCrossProduct(dPos, Force));
end;

function TVerletGlobalConstraint.TranslateKickbackTorque(
  const TorqueCenter: TAffineVector): TAffineVector;
var
  Torque : TAffineVector;
begin
  // EM(b) = EM(a) + EF x VectorSubtract(b, a). <p>
  Torque := VectorAdd(FKickbackTorque, VectorCrossProduct(VectorSubtract(TorqueCenter, FLocation), FKickbackForce));
end;

procedure TVerletGlobalConstraint.BeforeIterations;
begin
  inherited;
  FKickbackForce := NullVector;
  FKickbackTorque := NullVector;
end;

procedure TVerletGlobalConstraint.RemoveNode(const aNode : TVerletNode);
begin
   // nothing to do here
end;

// SetLocation
//
procedure TVerletGlobalConstraint.SetLocation(const Value: TAffineVector);
begin
  FLocation := Value;
end;

// SatisfyConstraint
//
procedure TVerletGlobalConstraint.SatisfyConstraint(const iteration, maxIterations : Integer);
var
   i : Integer;
   node : TVerletNode;
   list : PPointerList;
begin
   list:=Owner.Nodes.List;
   if cctNode in Owner.CollisionConstraintTypes then
     for i:=0 to Owner.Nodes.Count-1 do begin
        node:=TVerletNode(list[i]);
        if not node.NailedDown then
           SatisfyConstraintForNode(node, iteration, maxIterations);
     end;//}

   if cctEdge in Owner.CollisionConstraintTypes then
     for i:=0 to Owner.SolidEdges.Count-1 do begin
         SatisfyConstraintForEdge(Owner.SolidEdges[i], iteration, maxIterations);
   end;//}
end;

// SatisfyConstraintForEdge
//
procedure TVerletGlobalConstraint.SatisfyConstraintForEdge(
  const aEdge: TVerletEdge; const iteration, maxIterations: Integer);
begin
  // Purely virtual, but can't be abstract...
end;

// ------------------
// ------------------ TVerletGlobalFrictionConstraint ------------------
// ------------------

// Create
//
constructor TVerletGlobalFrictionConstraint.Create(const aOwner: TVerletWorld);
begin
   inherited;
   FFrictionRatio:=cDEFAULT_CONSTRAINT_FRICTION;
end;

// ------------------
// ------------------ TVerletGlobalFrictionConstraintSP ------------------
// ------------------

// SatisfyConstraint
//
procedure TVerletGlobalFrictionConstraintSP.SatisfyConstraint(
                              const iteration, maxIterations: Integer);
var
   i : Integer;
   node : TVerletNode;
   edge : TVerletEdge;
   SP : TBaseSpacePartition;
   Leaf : TSpacePartitionLeaf;
begin
   if Owner.SpacePartition=nil then begin
      inherited;
      Exit;
   end;

   PerformSpaceQuery;
   SP := Owner.SpacePartition;

   for i:=0 to SP.QueryResult.Count-1 do begin
      Leaf:=SP.QueryResult[i];
      if Leaf is TVerletNode then begin
         if cctNode in Owner.CollisionConstraintTypes then begin
            node := Leaf as TVerletNode;
            if not node.NailedDown then
               SatisfyConstraintForNode(node, iteration, maxIterations);
         end;
      end else if Leaf is TVerletEdge then begin
         if cctEdge in Owner.CollisionConstraintTypes then begin
            edge := Leaf as TVerletEdge;
            SatisfyConstraintForEdge(edge, iteration, maxIterations);
         end;
      end else Assert(False, 'Bad objects in list!');
   end;
end;

// ------------------
// ------------------ TVerletConstraintList ------------------
// ------------------

// GetItems
//
function TVerletConstraintList.GetItems(i : Integer) : TVerletConstraint;
begin
   Result:=Get(i);
end;

// SetItems
//
procedure TVerletConstraintList.SetItems(i : Integer;
                                         const value : TVerletConstraint);
begin
   Put(i, value);
end;

// ------------------
// ------------------ TVerletForce ------------------
// ------------------

// Create
//
constructor TVerletForce.Create(const aOwner : TVerletWorld);
begin
   inherited Create;
   if Assigned(aOwner) then
      aOwner.AddForce(Self);
end;

// Destroy
//
destructor TVerletForce.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveForce(Self);
   inherited;
end;

// ------------------
// ------------------ TVerletGroupForce ------------------
// ------------------

// Create
//
constructor TVerletGroupForce.Create(const aOwner : TVerletWorld);
begin
   inherited Create(aOwner);
   FNodes:=TVerletNodeList.Create;
end;

// Destroy
//
destructor TVerletGroupForce.Destroy;
begin
   FNodes.Free;
   inherited;
end;

// RemoveNode
//
procedure TVerletGroupForce.RemoveNode(const aNode : TVerletNode);
begin
   FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TVerletGlobalForce ------------------
// ------------------

// RemoveNode
//
procedure TVerletGlobalForce.RemoveNode(const aNode : TVerletNode);
begin
   // nothing to do here
end;

// AddForce
//
procedure TVerletGlobalForce.AddForce;
var
   i : Integer;
   node : TVerletNode;
   list : PPointerList;
begin
   list:=Owner.Nodes.List;
   for i:=0 to Owner.Nodes.Count-1 do begin
      node:=TVerletNode(list[i]);
      if not node.NailedDown then
         AddForceToNode(node);
   end;
end;

// ------------------
// ------------------ TVerletDualForce ------------------
// ------------------

// RemoveNode
//
procedure TVerletDualForce.RemoveNode(const aNode : TVerletNode);
begin
   if FNodeA=aNode then
      FNodeA:=nil;
   if FNodeB=aNode then
      FNodeB:=nil;
end;

// ------------------
// ------------------ TVerletForceList ------------------
// ------------------

// GetItems
//
function TVerletForceList.GetItems(i : Integer) : TVerletForce;
begin
   Result:=Get(i);
end;

// SetItems
//
procedure TVerletForceList.SetItems(i : Integer; const value : TVerletForce);
begin
   Put(i, value);
end;

// ------------------
// ------------------ TVerletWorld ------------------
// ------------------

// Create
//
constructor TVerletWorld.Create;
begin
   inherited;
   FDrag:=G_DRAG;
   FNodes:=TVerletNodeList.Create;
   FConstraints:=TVerletConstraintList.Create;
   FConstraintsWithBeforeIterations:=TVerletConstraintList.Create;
   FForces:=TVerletForceList.Create;
   FMaxDeltaTime:=0.02;
   FIterations:=3;
   FSolidEdges := TVerletEdgeList.Create;
   FCurrentStepCount := 0;
   FUpdateSpacePartion := uspNever;
   FCollisionConstraintTypes := [cctNode, cctEdge];
   FSpacePartition := nil;
   FVerletNodeClass := TVerletNode;
   FInertia := True;
end;

// Destroy
//
destructor TVerletWorld.Destroy;
var
   i : Integer;
begin
   // Delete all nodes
   for i:=0 to FNodes.Count-1 do with FNodes[i] do begin
      FOwner:=nil;
      Free;
   end;
   FreeAndNil(FNodes);
   // Delete all constraints
   for i:=0 to FConstraints.Count-1 do with FConstraints[i] do begin
      FOwner:=nil;
      Free;
   end;
   FreeAndNil(FConstraints);
   // Delete all forces
   for i:=0 to FForces.Count-1 do with FForces[i] do begin
      FOwner:=nil;
      Free;
   end;
   FreeAndNil(FForces);
   FreeAndNil(FConstraintsWithBeforeIterations);

   for i := 0 to FSolidEdges.Count-1 do
    FSolidEdges[i].Free;
   FreeAndNil(FSolidEdges);

   FreeAndNil(FSpacePartition);

   inherited;
end;

// AccumulateForces
//
procedure TVerletWorld.AccumulateForces(const vpt : TVerletProgressTimes);
var
   i : Integer;
begin
   // First of all, reset all forces
   for i:=0 to FNodes.Count-1 do
      FNodes[i].FForce:=NullVector;
   // Now, update all forces in the assembly!
   for i:=0 to FForces.Count-1 do
      FForces[i].AddForce(vpt);
end;

// AddNode
//
function TVerletWorld.AddNode(const aNode : TVerletNode) : Integer;
begin
   if Assigned(aNode.FOwner) then
      aNode.Owner.FNodes.Remove(aNode);
   Result:=FNodes.Add(aNode);
   aNode.FOwner:=Self;
end;

// RemoveNode
//
procedure TVerletWorld.RemoveNode(const aNode : TVerletNode);
var
   i : Integer;
begin
   if aNode.Owner=Self then begin
      FNodes.Remove(aNode);
      aNode.FOwner:=nil;
      // drop refs in constraints
      for i:=FConstraints.Count-1 downto 0 do
         FConstraints[i].RemoveNode(aNode);
      // drop refs in forces
      for i:=FForces.Count-1 downto 0 do
         FForces[i].RemoveNode(aNode);
   end;
end;

// AddConstraint
//
function TVerletWorld.AddConstraint(const aConstraint : TVerletConstraint) : Integer;
begin
   if Assigned(aConstraint.FOwner) then
      aConstraint.Owner.FConstraints.Remove(aConstraint);
   Result:=FConstraints.Add(aConstraint);
   aConstraint.FOwner:=Self;
end;

// RemoveConstraint
//
procedure TVerletWorld.RemoveConstraint(const aConstraint : TVerletConstraint);
begin
   if aConstraint.Owner=Self then begin
      FConstraints.Remove(aConstraint);
      aConstraint.FOwner:=nil;
   end;
end;

// AddForce
//
function TVerletWorld.AddForce(const aForce : TVerletForce) : Integer;
begin
   if Assigned(aForce.FOwner) then
      aForce.Owner.FForces.Remove(aForce);
   Result:=FForces.Add(aForce);
   aForce.FOwner:=Self;
end;

// RemoveForce
//
procedure TVerletWorld.RemoveForce(const aForce : TVerletForce);
begin
   if aForce.Owner=Self then begin
      FForces.Remove(aForce);
      aForce.FOwner:=nil;
   end;
end;

// AddSolidEdge
//
procedure TVerletWorld.AddSolidEdge(const aNodeA, aNodeB: TVerletNode);
var
  VerletEdge : TVerletEdge;
begin
  VerletEdge := TVerletEdge.CreateEdgeOwned(aNodeA, aNodeB);
  SolidEdges.Add(VerletEdge);
end;

// FirstNode
//
function TVerletWorld.FirstNode : TVerletNode;
begin
   Assert(FNodes.Count>0, 'There are no nodes in the assembly!');
   Result:=FNodes[0];
end;

// lastNode
//
function TVerletWorld.LastNode : TVerletNode;
begin
   Assert(FNodes.Count>0, 'There are no nodes in the assembly!');
   Result:=FNodes[FNodes.Count-1];
end;

// CreateOwnedNode
//
function TVerletWorld.CreateOwnedNode(const location : TAffineVector;
            const aRadius : Single = 0; const aWeight : Single=1) : TVerletNode;
begin
   Result:=VerletNodeClass.CreateOwned(self);
   Result.Location:=Location;
   Result.OldLocation:=Location;
   Result.Weight:=aWeight;
   Result.Radius:=aRadius;
end;

// CreateStick
//
function TVerletWorld.CreateStick(const aNodeA, aNodeB : TVerletNode; const Slack : Single = 0) : TVCStick;
begin
   Assert(aNodeA <> aNodeB, 'Can''t create stick between same node!');
   Result:=TVCStick.Create(Self);
   Result.NodeA:=aNodeA;
   Result.NodeB:=aNodeB;
   Result.SetRestLengthToCurrent;
   Result.Slack := Slack;
end;

// CreateSpring
//
function TVerletWorld.CreateSpring(const aNodeA, aNodeB : TVerletNode;
              const aStrength, aDamping : Single; const aSlack : Single = 0) : TVFSpring;
begin
   Result:=TVFSpring.Create(Self);
   Result.NodeA:=aNodeA;
   Result.NodeB:=aNodeB;
   Result.Strength:=aStrength;
   Result.Damping:=aDamping;
   Result.Slack:=aSlack;
   Result.SetRestLengthToCurrent;
end;

// CreateSlider
//
function TVerletWorld.CreateSlider(const aNodeA, aNodeB : TVerletNode;
                                   const aSlideDirection : TAffineVector) : TVCSlider;
begin
   Result:=TVCSlider.Create(Self);
   Result.NodeA:=aNodeA;
   Result.NodeB:=aNodeB;
   Result.SlideDirection:=aSlideDirection;
end;

// Initialize
//
procedure TVerletWorld.Initialize;
var
   i : Integer;
begin
   for i:=0 to FNodes.Count-1 do
      FNodes[i].Initialize;
end;

// Progress
//
function TVerletWorld.Progress(const deltaTime, newTime : Double) : Integer;
var
   i : Integer;
   ticks : Integer;
   myDeltaTime : Single;
   vpt : TVerletProgressTimes;
begin
   ticks:=0;
   myDeltaTime:=FMaxDeltaTime;
   FCurrentDeltaTime:=FMaxDeltaTime;
   FInvCurrentDeltaTime:=1/FCurrentDeltaTime;

   vpt.deltaTime:=myDeltaTime;
   vpt.sqrDeltaTime:=Sqr(myDeltaTime);
   vpt.invSqrDeltaTime:=1/vpt.sqrDeltaTime;

   while FSimTime<newTime do begin
      Inc(ticks);
      FSimTime:=FSimTime+myDeltaTime;
      vpt.newTime:=FSimTime;
      Verlet(vpt);
      AccumulateForces(vpt);
      SatisfyConstraints(vpt);

      if FInertaPauseSteps>0 then
      begin
        dec(FInertaPauseSteps);
        if FInertaPauseSteps=0 then
          Inertia := true;
      end;

      Break;
   end;

   Result:=ticks;

   for i:=0 to FNodes.Count-1 do
      FNodes[i].AfterProgress;
end;

// DoUpdateSpacePartition
//
procedure TVerletWorld.DoUpdateSpacePartition;
var
  i : Integer;
begin
  if Assigned(SpacePartition) then
  begin
    for i:=0 to FSolidEdges.Count-1 do
      if (FSolidEdges[i].FNodeA.FChangedOnStep=FCurrentStepCount) or
         (FSolidEdges[i].FNodeB.FChangedOnStep=FCurrentStepCount) then
        FSolidEdges[i].Changed;

    for i:=0 to FNodes.Count-1 do
      if (FNodes[i].FChangedOnStep=FCurrentStepCount) then
        FNodes[i].Changed;
  end;
end;

// SatisfyConstraints
//
procedure TVerletWorld.SatisfyConstraints(const vpt : TVerletProgressTimes);
var
   i, j : Integer;
   Constraint : TVerletConstraint;
begin
   for i:=0 to FConstraintsWithBeforeIterations.Count-1 do
   begin
     Constraint := FConstraintsWithBeforeIterations[i];
     Constraint.BeforeIterations;
   end;

   if UpdateSpacePartion=uspEveryFrame then
     inc(FCurrentStepCount);

   for j:=0 to Iterations-1 do
   begin
      for i:=0 to FConstraints.Count-1 do with FConstraints[i] do
         if Enabled then
            SatisfyConstraint(j, Iterations);//}

      if UpdateSpacePartion=uspEveryIteration then
        DoUpdateSpacePartition;
   end;

   if UpdateSpacePartion=uspEveryFrame then
    DoUpdateSpacePartition;//}
end;

// Verlet
//
procedure TVerletWorld.Verlet(const vpt : TVerletProgressTimes);
var
   i : Integer;
begin
   if UpdateSpacePartion<>uspNever then
     inc(FCurrentStepCount);

   for i:=0 to FNodes.Count-1 do
      FNodes[i].Verlet(vpt);

   if UpdateSpacePartion<>uspNever then
    DoUpdateSpacePartition;
end;

// ------------------
// ------------------ TVFGravity ------------------
// ------------------

// Create
//
constructor TVFGravity.Create(const aOwner : TVerletWorld);
begin
   inherited;
   FGravity[0]:=0;
   FGravity[1]:=-9.81;
   FGravity[2]:=0;
end;

// AddForceToNode
//
procedure TVFGravity.AddForceToNode(const aNode : TVerletNode);
begin
   CombineVector(aNode.FForce, Gravity, @aNode.Weight);
end;

// ------------------
// ------------------ TVFSpring ------------------
// ------------------

// SetSlack
//
procedure TVFSpring.SetSlack(const value : Single);
begin
   if value<=0 then
      FSlack:=0
   else FSlack:=value;
end;

// AddForce
//
procedure TVFSpring.AddForce;
var
   hTerm, dTerm : Single;
   deltaV, force : TAffineVector;
   deltaLength : Single;
begin
   VectorSubtract(NodeA.Location, NodeB.Location, force);
   deltaLength:=VectorLength(force);

   if deltaLength>FSlack then begin
     hTerm:=(FRestLength-deltaLength)*FForceFactor;
     force:=VectorScale(force, hTerm/deltaLength);
   end else force:=NullVector;
   if FDamping<>0 then begin
      VectorSubtract(NodeA.GetMovement, NodeB.GetMovement, deltaV);
      dTerm:=-0.25*FDamping*vpt.invSqrDeltaTime;
      CombineVector(force, deltaV, dTerm);
   end;

   AddVector(NodeA.FForce, force);
   SubtractVector(NodeB.FForce, force);
end;

// SetRestLengthToCurrent
//
procedure TVFSpring.SetRestLengthToCurrent;
begin
   FRestLength:=VectorDistance(NodeA.Location, NodeB.Location);
   FForceFactor:=FStrength/FRestLength;
end;

// ------------------
// ------------------ TVFAirResistance ------------------
// ------------------

procedure TVFAirResistance.AddForceToNode(const aNode: TVerletNode);
var
  s, F, FCurrentWindBurst : TAffineVector;
  sMag : Single;
  r : Single;
  Chaos : Single;
begin
  s := aNode.Speed;

  if FWindMagnitude<>0 then
  begin
    Chaos := FWindMagnitude * FWindChaos;
    FCurrentWindBurst[0] := FWindDirection[0] * FWindMagnitude + Chaos * (random-0.5) * 2;
    FCurrentWindBurst[1] := FWindDirection[1] * FWindMagnitude + Chaos * (random-0.5) * 2;
    FCurrentWindBurst[2] := FWindDirection[2] * FWindMagnitude + Chaos * (random-0.5) * 2;

    s := VectorSubtract(s, FCurrentWindBurst);
  end;

  sMag := VectorLength(s);

  r := aNode.Radius + 1;

  if sMag<> 0 then
  begin
    F := VectorScale(s, - sqr(sMag) * sqr(r) * pi * FDragCoeff);

    aNode.FForce := VectorAdd(aNode.FForce, F);
  end;
end;

constructor TVFAirResistance.Create(const aOwner: TVerletWorld);
begin
  inherited;

  FDragCoeff := 0.001;
  FWindDirection[0] := 0;
  FWindDirection[1] := 0;
  FWindDirection[2] := 0;
  FWindMagnitude := 0;
  FWindChaos := 0;
end;

procedure TVFAirResistance.SetWindDirection(const Value: TAffineVector);
begin
  FWindDirection := VectorNormalize(Value);
end;

// ------------------
// ------------------ TVCFloor ------------------
// ------------------

// Create
//
constructor TVCFloor.Create(const aOwner: TVerletWorld);
begin
  inherited;
  MakeVector(FNormal, 0, 1, 0);
  MakeVector(FLocation, 0, 0, 0);
end;

// PerformSpaceQuery
//
procedure TVCFloor.PerformSpaceQuery;
begin
   Owner.SpacePartition.QueryPlane(FLocation, FNormal);
end;

// SatisfyConstraintForNode
//
procedure TVCFloor.SatisfyConstraintForNode(const aNode : TVerletNode;
                                            const iteration, maxIterations : Integer);
var
   penetrationDepth : Single;
   currentPenetrationDepth : Single;
   d : TAffineVector;
   correction : TAffineVector;
begin
   currentPenetrationDepth:=-PointPlaneDistance(aNode.Location, FLocation, FNormal)
                            +aNode.Radius+FFloorLevel;

   // Record how far down the node goes
   penetrationDepth:=currentPenetrationDepth;
   // Correct the node location
   if currentPenetrationDepth>0 then begin
      correction:=VectorScale(FNormal, currentPenetrationDepth);
      if BounceRatio>0 then begin
         d:=VectorSubtract(aNode.FLocation, aNode.FOldLocation);
         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
         AddVector(aNode.FLocation, correction);
         aNode.FOldLocation:=VectorAdd(aNode.FLocation, VectorScale(d, BounceRatio));
      end else begin
         AddVector(aNode.FLocation, correction);
         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
         aNode.FChangedOnStep:=Owner.CurrentStepCount;
      end;
   end;
end;

// SetNormal
//
procedure TVCFloor.SetNormal(const Value: TAffineVector);
begin
  FNormal := Value;
  NormalizeVector(FNormal);
end;

// ------------------
// ------------------ TVCHeightField ------------------
// ------------------

// SatisfyConstraintForNode
//
procedure TVCHeightField.SatisfyConstraintForNode(const aNode : TVerletNode;
                                            const iteration, maxIterations : Integer);
var
   penetrationDepth : Single;
   currentPenetrationDepth : Single;
   d : TAffineVector;
   correction : TAffineVector;
begin
   currentPenetrationDepth:=-PointPlaneDistance(aNode.Location, FLocation, FNormal)+aNode.Radius;
   if Assigned(FOnNeedHeight) then
      currentPenetrationDepth:=currentPenetrationDepth+FOnNeedHeight(Self, aNode);

   // Record how far down the node goes
   penetrationDepth:=currentPenetrationDepth;
   // Correct the node location
   if currentPenetrationDepth>0 then begin
      correction:=VectorScale(FNormal, currentPenetrationDepth);
      if BounceRatio>0 then begin
         d:=VectorSubtract(aNode.FLocation, aNode.FOldLocation);
         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
         AddVector(aNode.FLocation, correction);
         aNode.FOldLocation:=VectorAdd(aNode.FLocation, VectorScale(d, BounceRatio));
      end else begin
         AddVector(aNode.FLocation, correction);
         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
         aNode.FChangedOnStep:=Owner.CurrentStepCount;
      end;
   end;
end;

// ------------------
// ------------------ TVCStick ------------------
// ------------------

// SatisfyConstraint
//
procedure TVCStick.SatisfyConstraint(const iteration, maxIterations : Integer);
var
   delta : TAffineVector;
   f, r : Single;
   deltaLength, diff : Single;
const
   cDefaultDelta : TAffineVector = (0.01, 0, 0);
begin
   Assert((NodeA<>NodeB), 'The nodes are identical - that causes division by zero!');

   VectorSubtract(NodeB.Location, NodeA.Location, delta);
   deltaLength:=VectorLength(delta);
   // Avoid div by zero!
   if deltaLength<1e-3 then begin
      delta:=cDefaultDelta;
      deltaLength:=0.01;
   end;

   diff:=(deltaLength-RestLength)/deltaLength;

   if Abs(diff)>Slack then begin
      r:=1/(NodeA.InvWeight+NodeB.InvWeight);
      if diff<0 then
         diff:=(diff+Slack)*r
      else diff:=(diff-Slack)*r;

      // Take into acount the different weights of the nodes!

      if not NodeA.NailedDown then begin
         f:=diff*NodeA.InvWeight;
         CombineVector(NodeA.FLocation, delta, f);
         NodeA.FChangedOnStep := Owner.CurrentStepCount;
      end;
      if not NodeB.NailedDown then begin
         f:=-diff*NodeB.InvWeight;
         CombineVector(NodeB.FLocation, delta, f);
         NodeB.FChangedOnStep := Owner.CurrentStepCount;
      end;
   end;
end;

// SetRestLengthToCurrent
//
procedure TVCStick.SetRestLengthToCurrent;
begin
   FRestLength:=VectorDistance(NodeA.Location, NodeB.Location);
end;

// ------------------
// ------------------ TVCRigidBody ------------------
// ------------------

// ComputeBarycenter
//
procedure TVCRigidBody.ComputeBarycenter(var barycenter : TAffineVector);
var
   i : Integer;
   totWeight : Single;
begin
   // first we compute the barycenter
   totWeight:=0;
   barycenter:=NullVector;
   for i:=0 to Nodes.Count-1 do with Nodes[i] do begin
      CombineVector(barycenter, Location, @Weight);
      totWeight:=totWeight+Weight;
   end;
   if totWeight>0 then
      ScaleVector(barycenter, 1/totWeight);
end;

// ComputeNaturals
//
procedure TVCRigidBody.ComputeNaturals(const barycenter : TAffineVector;
                                       var natX, natY, natZ : TAffineVector);
var
   i : Integer;
   delta : TAffineVector;
begin
   natX:=NullVector;
   natY:=NullVector;
   natZ:=NullVector;
   for i:=0 to Nodes.Count-1 do begin
      delta:=VectorSubtract(Nodes[i].Location, barycenter);
      CombineVector(natX, delta, FNodeParams[i][0]);
      CombineVector(natY, delta, FNodeParams[i][1]);
      CombineVector(natZ, delta, FNodeParams[i][2]);
   end;
end;

// ComputeRigidityParameters
//
procedure TVCRigidBody.ComputeRigidityParameters;
var
   i : Integer;
   barycenter : TAffineVector;
   d : Single;
begin
   // first we compute the barycenter
   ComputeBarycenter(barycenter);
   // next the parameters
   SetLength(FNodeParams, Nodes.Count);
   SetLength(FNodeCoords, Nodes.Count);
   for i:=0 to Nodes.Count-1 do begin
      FNodeCoords[i]:=VectorSubtract(Nodes[i].Location, barycenter);
      d:=Nodes[i].Weight/VectorLength(FNodeCoords[i]);
      FNodeParams[i][0]:=FNodeCoords[i][0]*d;
      FNodeParams[i][1]:=FNodeCoords[i][1]*d;
      FNodeParams[i][2]:=FNodeCoords[i][2]*d;
   end;

   ComputeNaturals(barycenter, FNatMatrix[0], FNatMatrix[1], FNatMatrix[2]);

   FNatMatrix[2]:=VectorCrossProduct(FNatMatrix[0], FNatMatrix[1]);
   FNatMatrix[1]:=VectorCrossProduct(FNatMatrix[2], FNatMatrix[0]);
   NormalizeVector(FNatMatrix[0]);
   NormalizeVector(FNatMatrix[1]);
   NormalizeVector(FNatMatrix[2]);   

   FInvNatMatrix:=FNatMatrix;
//   TransposeMatrix(FInvNatMatrix);
   InvertMatrix(FInvNatMatrix);
end;

// SatisfyConstraint
//
procedure TVCRigidBody.SatisfyConstraint(const iteration, maxIterations : Integer);
var
   i : Integer;
   barycenter, delta : TAffineVector;
   nrjBase, nrjAdjust : TaffineVector;
   natural : array [0..2] of TAffineVector;
   deltas : array of TAffineVector;
begin
   Assert(Nodes.Count=Length(FNodeParams), 'You forgot to call ComputeRigidityParameters!');
   // compute the barycenter
   ComputeBarycenter(barycenter);
   // compute the natural axises
   ComputeNaturals(barycenter, natural[0], natural[1], natural[2]);

   natural[2]:=VectorCrossProduct(natural[0], natural[1]);
   natural[1]:=VectorCrossProduct(natural[2], natural[0]);
   for i:=0 to 2 do
      NormalizeVector(natural[i]);

   natural[0]:=VectorTransform(natural[0], FInvNatMatrix);
   natural[1]:=VectorTransform(natural[1], FInvNatMatrix);
   natural[2]:=VectorTransform(natural[2], FInvNatMatrix);
   // make the natural axises orthonormal, by picking the longest two
{   for i:=0 to 2 do
      vectNorm[i]:=VectorNorm(natural[i]);
   if (vectNorm[0]<vectNorm[1]) and (vectNorm[0]<vectNorm[2]) then begin
      natural[0]:=VectorCrossProduct(natural[1], natural[2]);
      natural[1]:=VectorCrossProduct(natural[2], natural[0]);
   end else if (vectNorm[1]<vectNorm[0]) and (vectNorm[1]<vectNorm[2]) then begin
      natural[1]:=VectorCrossProduct(natural[2], natural[0]);
      natural[2]:=VectorCrossProduct(natural[0], natural[1]);
   end else begin
      natural[2]:=VectorCrossProduct(natural[0], natural[1]);
      natural[0]:=VectorCrossProduct(natural[1], natural[2]);
   end; }

   // now the axises are back, recompute the position of all points
   SetLength(deltas, Nodes.Count);
   nrjBase:=NullVector;
   for i:=0 to Nodes.Count-1 do begin
      nrjBase:=VectorAdd(nrjBase, VectorCrossProduct(VectorSubtract(Nodes[i].Location, barycenter),
                                                     Nodes[i].GetMovement));
   end;

   nrjAdjust:=NullVector;
   for i:=0 to Nodes.Count-1 do begin
      delta:=VectorCombine3(natural[0], natural[1], natural[2],
                            FNodeCoords[i][0], FNodeCoords[i][1], FNodeCoords[i][2]);
      deltas[i]:=VectorSubtract(VectorAdd(barycenter, delta), Nodes[i].Location);
      nrjAdjust:=VectorAdd(nrjBase, VectorCrossProduct(VectorSubtract(Nodes[i].Location, barycenter),
                                                       deltas[i]));
      Nodes[i].Location:=VectorAdd(Nodes[i].Location, deltas[i]);
      Nodes[i].FOldLocation:=VectorAdd(Nodes[i].FOldLocation, deltas[i]);
//      Nodes[i].FOldLocation:=Nodes[i].Location;
   end;

   deltas[0]:=nrjBase;
   deltas[1]:=nrjAdjust;
end;

// ------------------
// ------------------ TVCSlider ------------------
// ------------------

// SetSlideDirection
//
procedure TVCSlider.SetSlideDirection(const value : TAffineVector);
begin
   FSlideDirection:=VectorNormalize(value);
end;

// SatisfyConstraint
//
procedure TVCSlider.SatisfyConstraint(const iteration, maxIterations : Integer);
var
   delta : TAffineVector;
   f, r : Single;
   projB : TAffineVector;
const
   cDefaultDelta : TAffineVector = (0.01, 0, 0);
begin
   Assert((NodeA<>NodeB), 'The nodes are identical - that causes division by zero!');

   // project B in the plane defined by A and SlideDirection
   projB:=VectorSubtract(NodeB.Location, NodeA.Location);
   f:=VectorDotProduct(projB, SlideDirection);
   projB:=VectorCombine(NodeB.Location, SlideDirection, 1, -f);
   if Constrained and (f<0) then
      NodeB.Location:=projB;

   VectorSubtract(projB, NodeA.Location, delta);

   // Take into acount the different weights of the nodes!
   r:=1/(NodeA.InvWeight+NodeB.InvWeight);

   if not NodeA.NailedDown then begin
      f:=r*NodeA.InvWeight;
      CombineVector(NodeA.FLocation, delta, f);
      NodeA.FChangedOnStep:=Owner.CurrentStepCount;
   end;
   if not NodeB.NailedDown then begin
      f:=-r*NodeB.InvWeight;
      CombineVector(NodeB.FLocation, delta, f);
      NodeB.FChangedOnStep:=Owner.CurrentStepCount;
   end;
end;

// ------------------
// ------------------ TVCSphere ------------------
// ------------------


// SatisfyConstraintForEdge
//
function TVCSphere.GetBSphere: TBSphere;
begin
  result.Center := FLocation;
  result.Radius := FRadius;
end;

procedure TVCSphere.SatisfyConstraintForEdge(const aEdge: TVerletEdge;
  const iteration, maxIterations: Integer);
var
  closestPoint, move, delta, contactNormal : TAffineVector;
  deltaLength, diff : Single;
begin
  // If the edge penetrates the sphere, try pushing the nodes until it no
  // longer does
  closestPoint := PointSegmentClosestPoint(FLocation, aEdge.NodeA.FLocation, aEdge.NodeB.FLocation);

  // Find the distance between the two
  VectorSubtract(closestPoint, Location, delta);

  deltaLength := VectorLength(delta);

  if deltaLength<Radius then  begin
      if deltaLength>0 then begin
         contactNormal := VectorScale(delta, 1/deltaLength);
         aEdge.NodeA.ApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength), contactNormal);
         aEdge.NodeB.ApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength), contactNormal);
      end;

      // Move it outside the sphere!
      diff:=(Radius-deltaLength)/deltaLength;
      VectorScale(delta, diff, move);

      AddVector(aEdge.NodeA.FLocation, move);
      AddVector(aEdge.NodeB.FLocation, move);

      // Add the force to the kickback
      // F = a * m
      // a = move / deltatime
      AddKickbackForceAt(
        FLocation,
        VectorScale(move, -(aEdge.NodeA.FWeight + aEdge.NodeB.FWeight)  * Owner.FInvCurrentDeltaTime));

      aEdge.NodeA.FChangedOnStep := Owner.CurrentStepCount;
      aEdge.NodeB.FChangedOnStep := Owner.CurrentStepCount;
  end;
end;

// SatisfyConstraintForNode
//
procedure TVCSphere.SatisfyConstraintForNode(const aNode : TVerletNode;
  const iteration, maxIterations : Integer);
var
   delta, move, contactNormal : TAffineVector;
   deltaLength, diff : Single;
begin
   // Find the distance between the two
   VectorSubtract(aNode.Location, Location, delta);

   // Is it inside the sphere?
   deltaLength:=VectorLength(delta)-aNode.Radius;
   if Abs(deltaLength)<Radius then begin
      if deltaLength>0 then begin
         contactNormal := VectorScale(delta, 1/deltaLength);
         aNode.ApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength), contactNormal);
      end
      else
        // Slow it down - this part should not be fired
        aNode.OldApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength));

      // Move it outside the sphere!
      diff:=(Radius-deltaLength)/deltaLength;
      VectorScale(delta, diff, move);

      AddVector(aNode.FLocation, move);
      aNode.FChangedOnStep := Owner.CurrentStepCount;

      // Add the force to the kickback
      // F = a * m
      // a = move / deltatime
      AddKickbackForceAt(
        FLocation,
        VectorScale(move, -aNode.FWeight * Owner.FInvCurrentDeltaTime));
   end;
end;

// ------------------
// ------------------ TVCCylinder ------------------
// ------------------

// SetRadius
//
procedure TVCCylinder.SetRadius(const val : Single);
begin
   FRadius:=val;
   FRadius2:=Sqr(val);
end;

// SatisfyConstraintForNode
//
procedure TVCCylinder.SatisfyConstraintForNode(const aNode : TVerletNode;
                                    const iteration, maxIterations : Integer);
var
   proj, newLocation, move : TAffineVector;
   f, dist2, penetrationDepth : Single;
begin
   // Compute projection of node position on the axis
   f:=PointProject(aNode.Location, FLocation, FAxis);
   proj:=VectorCombine(FLocation, FAxis, 1, f);

   // Sqr distance
   dist2:=VectorDistance2(proj, aNode.Location);
   if dist2<FRadius2 then begin
      // move out of the cylinder
      VectorLerp(proj, aNode.Location, FRadius*RSqrt(dist2), newLocation);

      move := VectorSubtract(aNode.FLocation, newLocation);

      penetrationDepth := VectorLength(Move);

      aNode.ApplyFriction(FFrictionRatio, penetrationDepth, VectorScale(move, 1/penetrationDepth));

      aNode.FLocation := newLocation;
      aNode.FChangedOnStep := Owner.CurrentStepCount;
   end;
end;

// ------------------
// ------------------ TVCCube ------------------
// ------------------

function TVCCube.GetAABB:TAABB;
begin
  VectorAdd(FLocation, FHalfSides, result.max);
  VectorSubtract(FLocation, FHalfSides, result.min);
end;

// BROKEN AND VERY SLOW!
procedure TVCCube.SatisfyConstraintForEdge(const aEdge: TVerletEdge;
  const iteration, maxIterations: Integer);
var
  Corners : array[0..7] of TAffineVector;
  EdgeRelative : array[0..1] of TAffineVector;

  shortestMove, contactNormal : TAffineVector;
  shortestDeltaLength : Single;

  procedure AddCorner(CornerID : Integer; x,y,z : Single);
  begin
    x := (x-0.5)*2;
    y := (y-0.5)*2;
    z := (z-0.5)*2;
    MakeVector(Corners[CornerID], FHalfSides[0]*x, FHalfSides[1]*y, FHalfSides[2]*z);
    AddVector(Corners[CornerID], FLocation);
  end;

  procedure TryEdge(Corner0, Corner1 : Integer);
  var
    CubeEdgeClosest, aEdgeClosest : TAffineVector;
    CenteraEdge, move : TAffineVector;
    deltaLength : Single;
  begin
    SegmentSegmentClosestPoint(
      Corners[Corner0],
      Corners[Corner1],
      aEdge.NodeA.FLocation,
      aEdge.NodeB.FLocation,
      CubeEdgeClosest,
      aEdgeClosest);

    CenteraEdge := VectorSubtract(aEdgeClosest, FLocation);

    if (abs(CenteraEdge[0])<FHalfSides[0]) and
       (abs(CenteraEdge[1])<FHalfSides[1]) and
       (abs(CenteraEdge[2])<FHalfSides[2]) then
    begin
      // The distance to move the edge is the difference between CenterCubeEdge and
      // CenteraEdge
      move := VectorSubtract(CubeEdgeClosest, aEdgeClosest);

      deltaLength := VectorLength(move);

      if (deltaLength>0) and (deltaLength<shortestDeltaLength) then
      begin
        shortestDeltaLength := deltaLength;
        shortestMove := move;
      end;
    end;
  end;
begin
  // DISABLED!
  exit;

  // Early out test
  EdgeRelative[0] := VectorSubtract(aEdge.FNodeA.FLocation, FLocation);
  EdgeRelative[1] := VectorSubtract(aEdge.FNodeB.FLocation, FLocation);

  // If both edges are on the same side of _any_ box side, the edge can't
  // cut the box
  if ((EdgeRelative[0][0]> FHalfSides[0]) and (EdgeRelative[1][0] >FHalfSides[0])) or
     ((EdgeRelative[0][0]<-FHalfSides[0]) and (EdgeRelative[1][0]<-FHalfSides[0])) or

     ((EdgeRelative[0][1]> FHalfSides[1]) and (EdgeRelative[1][1]> FHalfSides[1])) or
     ((EdgeRelative[0][1]<-FHalfSides[1]) and (EdgeRelative[1][1]<-FHalfSides[1])) or

     ((EdgeRelative[0][2]> FHalfSides[2]) and (EdgeRelative[1][2]> FHalfSides[2])) or
     ((EdgeRelative[0][2]<-FHalfSides[2]) and (EdgeRelative[1][2]<-FHalfSides[2])) then
  begin
    exit;
  end;

  // For each cube edge:
  //   find closest positions between CubeEdge and aEdge
  //   if aEdgeClosestPosition within cube then
  //     move nodes until closest position is outside cube
  //     exit
  AddCorner(0, 0, 0, 0);
  AddCorner(1, 1, 0, 0);
  AddCorner(2, 1, 1, 0);
  AddCorner(3, 0, 1, 0);

  AddCorner(4, 0, 0, 1);
  AddCorner(5, 1, 0, 1);
  AddCorner(6, 1, 1, 1);
  AddCorner(7, 0, 1, 1);

  shortestDeltaLength := 10e30;

  TryEdge(0,1);
  TryEdge(1,2);
  TryEdge(2,3);
  TryEdge(3,0);

  TryEdge(4,5);
  TryEdge(5,6);
  TryEdge(6,7);
  TryEdge(7,4);

  TryEdge(0,3);
  TryEdge(1,5);
  TryEdge(2,6);
  TryEdge(3,7);

  if shortestDeltaLength<10e8 then
  begin
     contactNormal := VectorScale(shortestMove, 1/shortestDeltaLength);

     {aEdge.NodeA.ApplyFriction(FFrictionRatio, shortestDeltaLength, contactNormal);
     aEdge.NodeB.ApplyFriction(FFrictionRatio, shortestDeltaLength, contactNormal);//}

     AddVector(aEdge.NodeA.FLocation, shortestMove);
     AddVector(aEdge.NodeB.FLocation, shortestMove);//}

     aEdge.NodeA.Changed;
     aEdge.NodeB.Changed;

     aEdge.NodeA.FChangedOnStep := Owner.CurrentStepCount;
     aEdge.NodeB.FChangedOnStep := Owner.CurrentStepCount;
  end;
end;//*)

procedure TVCCube.SatisfyConstraintForNode(const aNode: TVerletNode;
  const iteration, maxIterations: Integer);
var
   p, absP, contactNormal : TAffineVector;
   dp : Single;
   smallestSide : Integer;
begin
   // TODO: Direction of Cube should be used to rotate the nodes location, as it
   // stands, the cube can only face in one direction.

   p:=VectorSubtract(aNode.FLocation, FLocation);

   absP[0]:=FHalfSides[0]-Abs(p[0]);
   absP[1]:=FHalfSides[1]-Abs(p[1]);
   absP[2]:=FHalfSides[2]-Abs(p[2]);

   if (PInteger(@absP[0])^<=0) or (PInteger(@absP[1])^<=0) or(PInteger(@absP[2])^<=0) then
      Exit;

   if absP[0]<absP[1] then
      if absP[0]<absP[2] then
         smallestSide:=0
      else smallestSide:=2
   else if absP[1]<absP[2] then
      smallestSide:=1
   else smallestSide:=2;

   contactNormal:=NullVector;

   // Only move along the "shortest" axis
   if PInteger(@p[smallestSide])^>=0 then begin
      dp:=absP[smallestSide];
      contactNormal[smallestSide]:=1;
      aNode.ApplyFriction(FFrictionRatio, dp, contactNormal);
      aNode.FLocation[smallestSide]:=aNode.FLocation[smallestSide]+dp;
   end else begin
      dp:=absP[smallestSide];
      contactNormal[smallestSide]:=-1;
      aNode.ApplyFriction(FFrictionRatio, dp, contactNormal);
      aNode.FLocation[smallestSide]:=aNode.FLocation[smallestSide]-dp;
   end;

   aNode.FChangedOnStep:=Owner.CurrentStepCount;
end;

procedure TVCCube.SetSides(const Value: TAffineVector);
begin
  FSides := Value;
  FHalfSides := VectorScale(Sides, 0.5);
  UpdateCachedAABB;
end;

// ------------------
// ------------------ TVCCapsule ------------------
// ------------------

{ TVCCapsule }
// SetAxis
//
procedure TVCCapsule.SetAxis(const val : TAffineVector);
begin
   FAxis:=VectorNormalize(val);
   UpdateCachedBSphere;
end;

// SetLength
//
procedure TVCCapsule.SetLength(const val : Single);
begin
   FLength:=val;
   FLengthDiv2:=val*0.5;
   UpdateCachedBSphere;
end;

// SetRadius
//
procedure TVCCapsule.SetRadius(const val : Single);
begin
   FRadius:=val;
   FRadius2:=Sqr(val);
   UpdateCachedBSphere;
end;

// GetBSphere
//
function TVCCapsule.GetBSphere: TBSphere;
begin
  result.Center := FLocation;
  result.Radius := Length + Radius;
end;

// SatisfyConstraintForNode
//
procedure TVCCapsule.SatisfyConstraintForNode(const aNode : TVerletNode;
                                              const iteration, maxIterations : Integer);
var
   p, n2, penetrationDepth  : Single;
   closest, v : TAffineVector;
   newLocation, move : TAffineVector;

begin
   // Find the closest point to location on the capsule axis
   p:=ClampValue(PointProject(aNode.Location, FLocation, FAxis),
                 -FLengthDiv2, FLengthDiv2);
   closest:=VectorCombine(FLocation, FAxis, 1, p);

   // vector from closest to location
   VectorSubtract(aNode.Location, closest, v);

   // should it be altered?
   n2:=VectorNorm(v);

   if n2<FRadius2 then
   begin
      newLocation := VectorCombine(closest, v, 1, Sqrt(FRadius2/n2));

      // Do friction calculations
      move := VectorSubtract(newLocation,aNode.FLocation);
      penetrationDepth := VectorLength(move);

      //aNode.OldApplyFriction(FFrictionRatio, penetrationDepth);
      aNode.ApplyFriction(FFrictionRatio, penetrationDepth, VectorScale(move, 1/penetrationDepth));

      aNode.FLocation:=newLocation;
      aNode.FChangedOnStep := Owner.CurrentStepCount;

      AddKickbackForceAt(
        FLocation,
        VectorScale(move, -aNode.FWeight * Owner.FInvCurrentDeltaTime));
   end;
end;


procedure TVCCapsule.SatisfyConstraintForEdge(const aEdge: TVerletEdge;
  const iteration, maxIterations: Integer);
var
   sphereLocation, closestPoint, dummy, delta, move, contactNormal : TAffineVector;
   Ax0, Ax1 : TAffineVector;
   deltaLength, diff, penetrationDepth : Single;
begin
  VectorScale(FAxis, FLengthDiv2, Ax0);
  AddVector(Ax0, FLocation);
  VectorScale(FAxis, -FLengthDiv2, Ax1);
  AddVector(Ax1, FLocation);

   SegmentSegmentClosestPoint(
    aEdge.NodeA.FLocation,
    aEdge.NodeB.FLocation,
    Ax0,
    Ax1,
    dummy,
    sphereLocation);

  // If the edge penetrates the sphere, try pushing the nodes until it no
  // longer does
  closestPoint := PointSegmentClosestPoint(sphereLocation, aEdge.NodeA.FLocation, aEdge.NodeB.FLocation);

  // Find the distance between the two
  VectorSubtract(closestPoint, sphereLocation, delta);

  deltaLength := VectorLength(delta);

  if deltaLength<Radius then  begin
      // Move it outside the sphere!
      diff:=(Radius-deltaLength)/deltaLength;
      VectorScale(delta, diff, move);

      penetrationDepth := VectorLength(move);
      contactNormal := VectorScale(move, 1/penetrationDepth);
      aEdge.NodeA.ApplyFriction(FFrictionRatio, penetrationDepth, contactNormal);
      aEdge.NodeB.ApplyFriction(FFrictionRatio, penetrationDepth, contactNormal);

      AddVector(aEdge.NodeA.FLocation, move);
      AddVector(aEdge.NodeB.FLocation, move);

      aEdge.NodeA.FChangedOnStep := Owner.CurrentStepCount;
      aEdge.NodeB.FChangedOnStep := Owner.CurrentStepCount;

      AddKickbackForceAt(
        FLocation,
        VectorScale(move, -(aEdge.NodeA.FWeight + aEdge.NodeB.FWeight)  * Owner.FInvCurrentDeltaTime));
  end;

end;
// ------------------
// ------------------ TVerletEdge ------------------
// ------------------

{ TVerletEdge }

constructor TVerletEdge.CreateEdgeOwned(const aNodeA, aNodeB: TVerletNode);
begin
  FNodeA := aNodeA;
  FNodeB := aNodeB;

  inherited CreateOwned(aNodeA.Owner.SpacePartition);
end;

// ------------------
// ------------------ TVerletEdgeList ------------------
// ------------------

procedure TVerletEdge.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB.min := FNodeA.FLocation;
  FCachedAABB.max := FNodeA.FLocation;

  AABBInclude(FCachedAABB, FNodeB.FLocation);

  AABBToBSphere(FCachedAABB, FCachedBSphere);
end;

{ TVerletEdgeList }

function TVerletEdgeList.GetItems(i: Integer): TVerletEdge;
begin
  result := Get(i);
end;

procedure TVerletEdgeList.SetItems(i: Integer; const Value: TVerletEdge);
begin
  put(i, Value);
end;

procedure TVerletNode.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB.min := FLocation;
  FCachedAABB.max := FLocation;
  FCachedBSphere.Center := FLocation;
  FCachedBSphere.Radius := 0;
end;

procedure TVerletNode.SetLocation(const Value: TAffineVector);
begin
  FLocation := Value;
  FChangedOnStep := Owner.CurrentStepCount;
end;

procedure TVerletWorld.CreateOctree(const OctreeMin,
  OctreeMax: TAffineVector; const LeafThreshold, MaxTreeDepth: Integer);
var
  Octree : TOctreeSpacePartition;
begin
  Assert(FNodes.Count=0,'You can only create an octree while the world is empty!');

  FreeAndNil(FSpacePartition);

  Octree := TOctreeSpacePartition.Create;

  Octree.SetSize(OctreeMin, OctreeMax);
  Octree.MaxTreeDepth := MaxTreeDepth;
  Octree.LeafThreshold := LeafThreshold;
  Octree.CullingMode := cmGrossCulling;

  FSpacePartition := Octree;

  if FUpdateSpacePartion = uspNever then
    FUpdateSpacePartion := uspEveryFrame;
end;

procedure TVerletWorld.PauseInertia(const IterationSteps: Integer);
begin
  FInertaPauseSteps := IterationSteps+1;
  Inertia := false;
end;

{ TVerletGlobalFrictionConstraintBox }

procedure TVerletGlobalFrictionConstraintBox.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryAABB(FCachedAABB);
end;

procedure TVerletGlobalFrictionConstraintBox.SetLocation(
  const Value: TAffineVector);
begin
  inherited;

  UpdateCachedAABB;
end;

procedure TVerletGlobalFrictionConstraintBox.UpdateCachedAABB;
begin
  FCachedAABB := GetAABB;
end;

{ TVerletGlobalFrictionConstraintSphere }

procedure TVerletGlobalFrictionConstraintSphere.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryBSphere(FCachedBSphere);
end;

procedure TVerletGlobalFrictionConstraintSphere.SetLocation(
  const Value: TAffineVector);
begin
  inherited;
  UpdateCachedBSphere;
end;

procedure TVerletGlobalFrictionConstraintSphere.UpdateCachedBSphere;
begin
  FCachedBSphere := GetBSphere;
end;

constructor TVerletGlobalConstraint.Create(const aOwner: TVerletWorld);
begin
  inherited;
  if Assigned(aOwner) then
    aOwner.ConstraintsWithBeforeIterations.Add(self);
end;

destructor TVerletGlobalConstraint.Destroy;
begin
  if Assigned(Owner) then
    Owner.ConstraintsWithBeforeIterations.Remove(self);

  inherited;
end;

end.
