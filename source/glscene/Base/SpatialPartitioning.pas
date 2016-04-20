{: SpatialPartitioning<p>

	Spatial Partitioning speeds up geometrical queries, like what objects does A
  overlap.<p>

  Nota that the class TOctreeSpacePartition is optimized for dynamic scenes with
  objects that are small in relation to the size of the Octree space. This from
  Eric;

  <i>The non-duplicating octree shouldn't really be used if  you have big objects,
  and this especially if you have lots of big objects (the more objects you have
  the less efficient the partitionning, due to the "magnifying glass" effect of
  the non-discriminating volume).</i><p>


	<b>History : </b><font size=-1><ul>
      <li>09/12/04 - MF - Renamed TQuadSpacePartition to TQuadtreeSpacePartition
      <li>08/12/04 - MF - Fixed AV error reported by DanB
      <li>03/12/04 - MF - Added quadtree for typical 2d (landscape) scenes
      <li>02/12/04 - MF - Removed rcci, cleaned up so that only frustum is used
                          streamlined frustum culling.
      <li>01/12/04 - HRLI - Added rcci/frustum culling
      <li>23/06/03 - MF - Separated functionality for Octrees and general
                          sectored space partitions so Quadtrees will be easy
                          to add.
      <li>20/06/03 - MF - Created
  </ul></font>
}

unit SpatialPartitioning;

interface

uses
  Classes, VectorGeometry, SysUtils, GeometryBB, PersistentClasses, Math;

const
  cOctree_LEAF_TRHESHOLD = 30;
  cOctree_MAX_TREE_DEPTH = 8;
  cOctree_GROW_GRAVY = 0.1;

type
  TBaseSpacePartition = class;

  {: Describes a cone, and is used for cone collision}
  TSPCone = record
    {: The base of the cone }
    Base : TAffineVector;

    {: The axis of the cone }
    Axis : TAffineVector;

    {: Angle of the cone }
    Angle : single;

    {: Length of the cone }
    Length : single;
  end;

  {: Extended frustum, used for fast intersection testing }
  TExtendedFrustum = record
    Frustum : TFrustum;
    BSphere : TBSphere;
    // SPCone : TSPCone;
  end;

  {: Used to store the actual objects in the SpacePartition }
  TSpacePartitionLeaf = class(TPersistentObject)
  private
    FSpacePartition : TBaseSpacePartition;
    procedure SetSpacePartition(const Value: TBaseSpacePartition);
  public
    {: This can be used by the space partitioner as it sees fit}
    FPartitionTag : pointer;
    {: Leaves cache their AABBs so they can easily be accessed when needed by
    the space partitioner }
    FCachedAABB : TAABB;
    {: Leaves cache their BoundingSpheres so they can easily be accessed when
    needed by the space partitioner }
    FCachedBSphere : TBSphere;

    {: Whenever the size or location of the leaf changes, the space partitioner
    should be notified through a call to Changed. In the basic version, all it
    does is update the cached AABB and BSphere. You do not need to override this
    method.}
    procedure Changed; virtual;

    // *******************
    // *** Override this!
    {: AABBs and BSpheres are cached for leafs, and this function should be
    overriden to update the cache from the structure that the leaf stores. This
    is the only function you MUST override to use space partitions.}
    procedure UpdateCachedAABBAndBSphere; virtual;

    {: The TBaseSpacePartition that owns this leaf}
    property SpacePartition : TBaseSpacePartition read FSpacePartition write SetSpacePartition;

    {: This tag can be used by the space partition to store vital information
    in the leaf}
    property PartitionTag : pointer read FPartitionTag;

    constructor CreateOwned(SpacePartition : TBaseSpacePartition);
    destructor Destroy; override;
  published
  end;

  {: List for storing space partition leaves}
  TSpacePartitionLeafList = class(TPersistentObjectList)
  private
    function GetItems(i: integer): TSpacePartitionLeaf;
    procedure SetItems(i: integer; const Value: TSpacePartitionLeaf);
  public
    property Items[i : integer] : TSpacePartitionLeaf read GetItems write SetItems; default;
    constructor Create; override;
  end;

  TCullingMode = (cmFineCulling, cmGrossCulling);

  {: Basic space partition, does not implement any actual space partitioning }
  TBaseSpacePartition = class(TPersistentObject)
  private
    FCullingMode: TCullingMode;
    {: Query space for Leaves that intersect a cone, result is returned through
    QueryResult}
    function QueryCone(const aCone : TSPCone) : integer; virtual;
  protected
    FQueryResult: TSpacePartitionLeafList;
    FQueryInterObjectTests : integer;

    {: Empties the search result and resetting all search statistics }
    procedure FlushQueryResult; virtual;
  public
    {: The results from the last query }
    property QueryResult : TSpacePartitionLeafList read FQueryResult;

    {: Clear all internal storage Leaves }
    procedure Clear; virtual;

    // ** Update space partition
    {: Add a leaf}
    procedure AddLeaf(aLeaf : TSpacePartitionLeaf); virtual;
    {: Remove a leaf}
    procedure RemoveLeaf(aLeaf : TSpacePartitionLeaf); virtual;
    {: Called by leaf when it has changed}
    procedure LeafChanged(aLeaf : TSpacePartitionLeaf); virtual;

    // ** Query space partition
    {: Query space for Leaves that intersect the axis aligned bounding box,
    result is returned through QueryResult}
    function QueryAABB(const aAABB : TAABB) : integer; virtual;
    {: Query space for Leaves that intersect the bounding sphere, result is
    returned through QueryResult}
    function QueryBSphere(const aBSphere : TBSphere) : integer; virtual;
    {: Query space for Leaves that intersect the bounding sphere or box
    of a leaf. Result is returned through QueryResult}
    function QueryLeaf(const aLeaf : TSpacePartitionLeaf) : integer; virtual;
    {: Query space for Leaves that intersect a plane. Result is returned through
    QueryResult}
    function QueryPlane(const Location, Normal: TAffineVector) : integer; virtual;
    {: Query space for Leaves that intersect a Frustum. Result is returned through
    QueryResult}
    function QueryFrustum(const Frustum : TFrustum) : integer; virtual;
    {: Query space for Leaves that intersect an extended frustum. Result is
    returned through QueryResult. Extended frustum is slightly faster than the
    regular frustum because it uses a bounding sphere for the frustum}
    function QueryFrustumEx(const ExtendedFrustum : TExtendedFrustum) : integer; virtual;

    {: Once a query has been run, this number tells of how many inter object
    tests that were run. This value must be set by all that override the
    queries. }
    property QueryInterObjectTests : integer read FQueryInterObjectTests;

    {: Some space partitioners delay processing changes until all changes have
    been made. ProcessUpdated should be called when all changes have been
    performed. }
    procedure ProcessUpdated; virtual;

    {: Determines if the spatial structure should do very simple preliminary
    culling (gross culling) or a more detailed form of culling (fine culling)}
    property CullingMode : TCullingMode read FCullingMode write FCullingMode;

    constructor Create; override;
    destructor Destroy; override;
  end;

  {: Implements a list of all leaves added to the space partition, _not_ a
  good solution, but it can be used as a benchmark against more complex methods}
  TLeavedSpacePartition = class(TBaseSpacePartition)
  private
    FLeaves : TSpacePartitionLeafList;

    {: Query space for Leaves that intersect a cone, result is returned through
    QueryResult}
    function QueryCone(const aCone : TSPCone) : integer; override;
  public
    {: Clear all internal storage Leaves }
    procedure Clear; override;

    // ** Update space partition
    {: Add a leaf}
    procedure AddLeaf(aLeaf : TSpacePartitionLeaf); override;
    {: Remove a leaf}
    procedure RemoveLeaf(aLeaf : TSpacePartitionLeaf); override;

    // ** Query space partition
    {: Query space for Leaves that intersect the axis aligned bounding box,
    result is returned through QueryResult. This override scans _all_ leaves
    in the list, so it's far from optimal.}
    function QueryAABB(const aAABB : TAABB) : integer; override;
    {: Query space for Leaves that intersect the bounding sphere, result is
    returned through QueryResult. This override scans _all_ leaves
    in the list, so it's far from optimal.}
    function QueryBSphere(const aBSphere : TBSphere) : integer; override;
    {: Query space for Leaves that intersect a plane. Result is returned through
    QueryResult}
    function QueryPlane(const FLocation, FNormal: TAffineVector) : integer; override;

    constructor Create; override;
    destructor Destroy; override;
  published
    property Leaves : TSpacePartitionLeafList read FLeaves;
  end;

  TSectoredSpacePartition = class;
  TSectorNode = class;
  TSectorNodeArray = array[0..7] of TSectorNode;

  {: Implements a SectorNode node. Each node can have 0 or 8 children, each child
  being a portion of the size of the parent. For quadtrees, that's 1/4, for
  octrees, it's 1/8 }
  TSectorNode = class
  private
    FLeaves : TSpacePartitionLeafList;
    FAABB : TAABB;
    FSectoredSpacePartition : TSectoredSpacePartition;
    FRecursiveLeafCount: integer;
    FParent: TSectorNode;
    FNodeDepth : integer;
    FChildCount : integer;
    FChildren: TSectorNodeArray;
    FBSphere: TBSphere;
    function GetNoChildren: boolean;
    procedure SetAABB(const Value: TAABB);
    function GetCenter: TAffineVector;
  protected
    {: Recursively counts the RecursiveLeafCount, this should only be used in
    debugging purposes, because the proprtyu RecursiveLeafCount is always up to
    date.}
    function CalcRecursiveLeafCount : integer;

    {: Places a leaf in one of the children of this node, or in the node itself
    if it doesn't fit in any of the children }
    function PlaceLeafInChild(aLeaf : TSpacePartitionLeaf ) : TSectorNode;

    {: Debug method that checks that FRecursiveLeafCount and
    CalcRecursiveLeafCount actually agree }
    function VerifyRecursiveLeafCount : string;

    {: Executed whenever the children of the node has changed}
    procedure ChildrenChanged; virtual;
  public
    {: Clear deletes all children and empties the leaves. It doesn't destroy
    the leaves, as they belong to the SpacePartition}
    procedure Clear;

    {: The Axis Aligned Bounding Box for this node. All leaves MUST fit inside
    this box. }
    property AABB : TAABB read FAABB write SetAABB;
    {: BSphere for this node }
    property BSphere : TBSphere read FBSphere;
    {: Center of the AABB for this node.}
    property Center : TAffineVector read GetCenter;
    {: NoChildren is true if the node has no children.}
    property NoChildren : boolean read GetNoChildren;
    {: A list of the children for this node, only ChildCount children are none
    nil }
    property Children : TSectorNodeArray read FChildren;
    {: The number of child sectors that have been created }
    property ChildCount : integer read FChildCount;

    {: Computes which child the AABB should go in. Returns nil if no such child
    exists }
    function GetChildForAABB(AABB : TAABB) : TSectorNode; virtual;

    {: The leaves that are stored in this node }
    property Leaves : TSpacePartitionLeafList read FLeaves;


    {: The Structure that owns this node }
    property SectoredSpacePartition : TSectoredSpacePartition read FSectoredSpacePartition;

    {: The parent node of this node. If parent is nil, that means that this
    node is the root node }
    property Parent : TSectorNode read FParent;

    {: The number of leaves stored in this node and all it's children.}
    property RecursiveLeafCount : integer read FRecursiveLeafCount;

    {: The tree depth at which this node is located. For the root, this value
    is 0, for the roots children, it is 1 and so on }
    property NodeDepth : integer read FNodeDepth;

    {: Checks if an AABB fits completely inside this node }
    function AABBFitsInNode(const aAABB : TAABB) : boolean; virtual;

    {: Checks if an AABB intersects this node }
    function AABBIntersectsNode(const aAABB : TAABB) : boolean; virtual;

    {: Checks if a BSphere fits completely inside this node }
    function BSphereFitsInNode(const BSphere : TBSphere) : boolean; virtual;

    {: Checks if a BSphere intersects this node }
    function BSphereIntersectsNode(const BSphere : TBSphere) : boolean; virtual;

    {: Checks if a AABB partially or completely contains this sector}
    function AABBContainsSector(const AABB : TAABB) : TSpaceContains; virtual;

    {: Checks if a BSphere partially or completely contains this sector}
    function BSphereContainsSector(const BSphere : TBSphere) : TSpaceContains; virtual;

    {: Checks if this node partially or completely contains a BSphere}
    function ContainsBSphere(const aBSphere : TBSphere) : TSpaceContains; virtual;

    {: Checks if this node partially or completely contains an AABB}
    function ContainsAABB(const aAABB : TAABB) : TSpaceContains; virtual;

    {: Adds leaf to this node - or one of it's children. If the node has enough
    leaves and has no children, children will be created and all leaves will be
    spread among the children. }
    function AddLeaf(aLeaf : TSpacePartitionLeaf) : TSectorNode;

    {: Remove leaf will remove a leaf from this node. If it is determined that
    this node has too few leaves after the delete, it may be collapsed. Returns
    true if the node was actually collapsed}
    function RemoveLeaf(aLeaf : TSpacePartitionLeaf; OwnerByThis : boolean) : boolean;

    {: Query the node and its children for leaves that match the AABB }
    procedure QueryAABB(const aAABB : TAABB; const QueryResult : TSpacePartitionLeafList);

    {: Query the node and its children for leaves that match the BSphere }
    procedure QueryBSphere(const aBSphere : TBSphere; const QueryResult : TSpacePartitionLeafList);

    {: Query the node and its children for leaves that match the plane }
    procedure QueryPlane(const Location, Normal: TAffineVector; const QueryResult : TSpacePartitionLeafList);

    {: Query the node and its children for leaves that match the Frustum. }
    procedure QueryFrustum(const Frustum : TFrustum; const QueryResult : TSpacePartitionLeafList);

    {: Query the node and its children for leaves that match the extended
    frustum. }
    procedure QueryFrustumEx(const ExtendedFrustum : TExtendedFrustum; const QueryResult : TSpacePartitionLeafList);

    {: Adds all leaves to query result without testing if they intersect, and
    then do the same for all children. This is used when QueryAABB or
    QueryBSphere determines that a node fits completely in the searched space}
    procedure AddAllLeavesRecursive(const QueryResult : TSpacePartitionLeafList);

    {: Add children to this node and spread the leaves among it's children }
    procedure ExpandNode;

    {: Create the number of children this node type needs }
    procedure CreateChildren; virtual;

    {: Delete all children for this node, adding their leaves to this node }
    procedure CollapseNode;

    {: Returns the number of nodes in the Octree }
    function GetNodeCount : integer;

    constructor Create(aSectoredSpacePartition : TSectoredSpacePartition; aParent : TSectorNode);

    destructor Destroy; override;
  end;

  {: Implements sectored space partitioning, sectored space partitions include
  Octrees, Quadtrees and  BSP-trees }
  TGrowMethod = (gmNever, gmBestFit, gmIncreaseToFitAll);
  TSectoredSpacePartition = class(TLeavedSpacePartition)
  private
    FRootNode : TSectorNode;
    FLeafThreshold: integer;
    FMaxTreeDepth: integer;
    FGrowGravy: single;
    FGrowMethod: TGrowMethod;
    procedure SetLeafThreshold(const Value: integer);
    procedure SetMaxTreeDepth(const Value: integer);
  protected
    FQueryNodeTests : integer;

    {: Empties the search result and resetting all search statistics }
    procedure FlushQueryResult; override;
  public
    // ** Update space partition
    {: Add a leaf to the structure. If the leaf doesn't fit in the structure, the
    structure is either grown or an exception is raised. If GrowMethod is set to
    gmBestFit or gmIncreaseToFitAll, the octree will be grown.}
    procedure AddLeaf(aLeaf : TSpacePartitionLeaf); override;

    {: Remove a leaf from the structure.}
    procedure RemoveLeaf(aLeaf : TSpacePartitionLeaf); override;

    {: Called by leaf when it has changed, the leaf will be moved to an
    apropriate node}
    procedure LeafChanged(aLeaf : TSpacePartitionLeaf); override;

    // ** Query space partition
    {: Query space for Leaves that intersect the axis aligned bounding box,
    result is returned through QueryResult. This method simply defers to the
    QueryAABB method of the root node.}
    function QueryAABB(const aAABB : TAABB) : integer; override;

    {: Query space for Leaves that intersect the bounding sphere, result is
    returned through QueryResult. This method simply defers to the
    QueryBSphere method of the root node.}
    function QueryBSphere(const aBSphere : TBSphere) : integer; override;

    {: Query space for Leaves that intersect the bounding sphere or box
    of a leaf. Result is returned through QueryResult}
    function QueryLeaf(const aLeaf : TSpacePartitionLeaf) : integer; override;

    {: Query space for Leaves that intersect a plane. Result is returned through
    QueryResult}
    function QueryPlane(const Location, Normal: TAffineVector) : integer; override;

    {: Query space for Leaves that intersect a Frustum. Result is returned through
    QueryResult}
    function QueryFrustum(const Frustum : TFrustum) : integer; override;

    {: Query space for Leaves that intersect an extended frustum. Result is
    returned through QueryResult}
    function QueryFrustumEx(const ExtendedFrustum : TExtendedFrustum) : integer; override;

    {: After a query has been run, this value will contain the number of nodes
    that were checked during the query }
    property QueryNodeTests : integer read FQueryNodeTests;

    {: Returns the number of nodes in the structure }
    function GetNodeCount : integer;

    {: UpdateOctreeSize will grow and / or shrink the structure to fit the
    current leaves +-gravy}
    procedure UpdateStructureSize(Gravy : single);

    {: Rebuild tree will change the tree to the newAABB size, and completely
    rebuild it }
    procedure RebuildTree(const NewAABB : TAABB);

    {: Returns the _total_ AABB in structure }
    function GetAABB : TAABB;

    {: CreateNewNode creates a new node of the TSectorNode subclass that this
    structure requires }
    function CreateNewNode(aParent : TSectorNode) : TSectorNode; virtual;

    constructor Create; override;
    destructor Destroy; override;
  published
    {: Root TSectorNode that all others stem from }
    property RootNode : TSectorNode read FRootNode;

    {: Determines how deep a tree should be allowed to grow. }
    property MaxTreeDepth : integer read FMaxTreeDepth write SetMaxTreeDepth;

    {: Determines when a node should be split up to form children. }
    property LeafThreshold : integer read FLeafThreshold write SetLeafThreshold;

    {: Determines if the structure should grow with new leaves, or if an exception
    should be raised }
    property GrowMethod : TGrowMethod read FGrowMethod write FGrowMethod;

    {: When the structure is recreated because it's no longer large enough to fit
    all leafs, it will become large enough to safely fit all leafs, plus
    GrowGravy. This is to prevent too many grows }
    property GrowGravy : single read FGrowGravy write FGrowGravy;
  end;

  // ** OCTTREE
  {: Implements sector node that handles octrees}
  TSPOctreeNode = class(TSectorNode)
  public
    {: Create 8 TSPOctreeNode children }
    procedure CreateChildren; override;

    {: Checks if an AABB fits completely inside this node }
    function AABBFitsInNode(const aAABB : TAABB) : boolean; override;

    {: Checks if an AABB intersects this node }
    function AABBIntersectsNode(const aAABB : TAABB) : boolean; override;

    {: Checks if a BSphere fits completely inside this node }
    function BSphereFitsInNode(const BSphere : TBSphere) : boolean; override;

    {: Checks if a BSphere intersects this node }
    function BSphereIntersectsNode(const BSphere : TBSphere) : boolean; override;
  end;

  {: Implements octrees}
  TOctreeSpacePartition = class(TSectoredSpacePartition)
  public
    {: Set size updates the size of the Octree }
    procedure SetSize(const Min, Max : TAffineVector);

    {: CreateNewNode creates a new TSPOctreeNode }
    function CreateNewNode(aParent : TSectorNode) : TSectorNode; override;
  end;

  // ** QUADTREE
  {: Implements sector node that handles quadtrees.}
  TSPQuadtreeNode = class(TSPOctreeNode)
  protected
    {: Executed whenever the children of the node has changed. In the quadtree,
     we want to make sure the Y value of the AABB is correct up and down and that
     the bounding sphere is correct}
    procedure ChildrenChanged; override;
  public
    {: Create 4 TSPQuadtreeNode children }
    procedure CreateChildren; override;

    {: Checks if an AABB fits completely inside this node }
    function AABBFitsInNode(const aAABB : TAABB) : boolean; override;

    {: Checks if an AABB intersects this node }
    function AABBIntersectsNode(const aAABB : TAABB) : boolean; override;

    {: Checks if a BSphere fits completely inside this node }
    function BSphereFitsInNode(const BSphere : TBSphere) : boolean; override;

    {: Checks if a BSphere intersects this node }
    function BSphereIntersectsNode(const BSphere : TBSphere) : boolean; override;

    {: Computes which child the AABB should go in. Returns nil if no such child
    exists }
    function GetChildForAABB(AABB : TAABB) : TSectorNode; override;
  end;

  {: Implements quadtrees.<p>
    Quadtrees are hardcoded to completely ignore the Y axis, only using X and Z
    to determine positioning.<p>
    This means that they're well suited for 2d-ish situations (landscapes with
    trees for instance) but not for fully 3d situations (space fighting).}
  TQuadtreeSpacePartition = class(TSectoredSpacePartition)
  public
    {: Set size updates the size of the Octree }
    procedure SetSize(const Min, Max : TAffineVector);

    {: CreateNewNode creates a new TSPOctreeNode }
    function CreateNewNode(aParent : TSectorNode) : TSectorNode; override;
  end;

  {: Determines to which extent one Cone contains an BSphere}
  function ConeContainsBSphere(const Cone : TSPCone; BSphere : TBSphere) : TSpaceContains;

  {: Determines if a extended frustum intersects an BSphere}
  function ExtendedFrustumIntersectsBSphere(const AExtendedFrustum : TExtendedFrustum; ABSphere : TBSphere) : boolean;

  {: Create an extended frustum from a number of values }
  function ExtendedFrustumMake(const AFrustum : TFrustum; const ANearDist,
    AFarDist, AFieldOfViewRadians : single;
    const ACameraPosition, ALookVector : TAffineVector{;
    const AScreenWidth, AScreenHeight : integer {}) : TExtendedFrustum;

implementation

// This was copied from Octree.pas!
//
// Theory on FlagMax and FlagMin:
// When a node is subdivided, each of the 8 children assumes 1/8th ownership of its
// parent's bounding box (defined by parent extents).  Calculating a child's min/max
// extent only requires 3 values: the parent's min extent, the parent's max extent
// and the midpoint of the parent's extents (since the cube is divided in half twice).
// The following arrays assume that the children are numbered from 0 to 7, named Upper
// and Lower (Upper = top 4 cubes on Y axis, Bottom = lower 4 cubes), Left and Right, and
// Fore and Back (Fore facing furthest away from you the viewer).
// Each node can use its corresponding element in the array to flag the operation needed
// to find its new min/max extent.  Note that min, mid and max refer to an array of
// 3 coordinates (x,y,z); each of which are flagged separately. Also note that these
// flags are based on the Y vector being the up vector.
const
  cMIN = 0;
  cMID = 1;
  cMAX = 2;

   cOctFlagMIN: array[0..7] of array [0..2] of byte = (
      (cMIN,cMID,cMID), //Upper Fore Left
      (cMID,cMID,cMID), //Upper Fore Right
      (cMIN,cMID,cMIN), //Upper Back Left
      (cMID,cMID,cMIN), //Upper Back Right

      (cMIN,cMIN,cMID), //Lower Fore Left  (similar to above except height/2)
      (cMID,cMIN,cMID), //Lower Fore Right
      (cMIN,cMIN,cMIN), //Lower Back Left
      (cMID,cMIN,cMIN)  //Lower Back Right
    );

   cOctFlagMax: array[0..7] of array [0..2] of byte = (
      (cMID,cMAX,cMAX), //Upper Fore Left
      (cMAX,cMAX,cMAX), //Upper Fore Right
      (cMID,cMAX,cMID), //Upper Back Left
      (cMAX,cMAX,cMID), //Upper Back Right

      (cMID,cMID,cMAX), //Lower Fore Left   (similar to above except height/2)
      (cMAX,cMID,cMAX), //Lower Fore Right
      (cMID,cMID,cMID), //Lower Back Left
      (cMAX,cMID,cMID)  //Lower Back Right
    );

function ConeContainsBSphere(const Cone : TSPCone; BSphere : TBSphere) : TSpaceContains;
var
  U, D : TAffineVector;
  e, dsqr : single;
begin
  // NOTE: This code hasn't been verified

  // U = K.vertex - (Sphere.radius/K.sin)*K.axis;
  U := VectorSubtract(Cone.Base, VectorScale(Cone.Axis, BSphere.Radius / sin(Cone.Angle)));

  // D = S.center - U;
  D := VectorSubtract(BSphere.Center, U);

  // dsqr = Dot(D,D)
  dsqr := VectorDotProduct(D, D);

  // e = Dot(K.axis,D);
  e := VectorDotProduct(Cone.Axis, D);

  if (e > 0) and (e*e >= dsqr*sqr(cos(Cone.Angle))) then
  begin
    // D = S.center - K.vertex;
    D := VectorSubtract(BSphere.Center, Cone.Base);

    // dsqr = Dot(D,D);
    dsqr := VectorDotProduct(D, D);

    // e = -Dot(K.axis,D);
    e := - VectorDotProduct(Cone.Axis, D);

    if (e > 0) and (e*e >= dsqr*(sqr(sin(Cone.Angle)))) then
    begin
      if dsqr <= BSphere.radius*BSphere.radius then
        result := scContainsPartially
      else
        result := scNoOverlap;
    end
    else
      result := scContainsPartially;
  end else
    result := scNoOverlap;
end;//}

function ExtendedFrustumIntersectsBSphere(const AExtendedFrustum : TExtendedFrustum; ABSphere : TBSphere) : boolean;
begin
   // Test if the bounding sphere of the node intersect the bounding sphere of the
  // frustum? This test is exremely fast
  if not BSphereIntersectsBSphere(ABSphere, AExtendedFrustum.BSphere) then
    result := false

  // Test if the bsphere of the node intersects the frustum
  else if IsVolumeClipped(ABSphere.Center, ABSphere.Radius, AExtendedFrustum.Frustum) then
    result := false

  else
    result := true;
end;

function ExtendedFrustumMake(const AFrustum : TFrustum; const ANearDist,
  AFarDist, AFieldOfViewRadians : single;
  const ACameraPosition, ALookVector : TAffineVector{;
  const AScreenWidth, AScreenHeight : integer{}) : TExtendedFrustum;
var
  ViewLen : single;
  Height, Width : single;
  // Depth, Corner, NewFov : single;
  P, Q, vDiff : TAffineVector;//}
begin
  // See http://www.flipcode.com/articles/article_frustumculling.shtml for
  // details calculate the radius of the frustum sphere

  result.Frustum := AFrustum;

  // ************
  // Create a bounding sphere for the entire frustum - only bspheres that
  // intersect this bounding sphere can in turn intersect the frustum
  ViewLen := AFarDist - ANearDist;

  // use some trig to find the height of the frustum at the far plane
  Height := ViewLen * sin(AFieldOfViewRadians / 2); // was tan( !?

  // with an aspect ratio of 1, the width will be the same
  Width := Height;

  // halfway point between near/far planes starting at the origin and extending along the z axis
  P := AffineVectorMake(0,0, ANearDist + ViewLen / 2);

  // the calculate far corner of the frustum
  Q := AffineVectorMake(Width, Height, ViewLen);

  // the vector between P and Q
  vDiff := VectorSubtract(P, Q);

  // the radius becomes the length of this vector
  result.BSphere.Radius := VectorLength(vDiff);

  // calculate the center of the sphere
  result.BSphere.Center := VectorAdd(ACameraPosition, VectorScale(ALookVector, ViewLen/2 + ANearDist));

  // ************
  // Create a cone
  // calculate the length of the fov triangle
  {Depth  := AScreenHeight / tan(AFieldOfViewRadians / 2);

  // calculate the corner of the screen
  Corner := sqrt(AScreenHeight * AScreenHeight + AScreenWidth * AScreenWidth);

  // now calculate the new fov
  NewFov := ArcTan2(Corner, Depth);

  // apply to the cone
  result.SPCone.Axis := ALookVector;
  result.SPCone.Base := ACameraPosition;
  result.SPCone.Angle := NewFov; //}
end;

{ TSpacePartitionLeaf }

procedure TSpacePartitionLeaf.UpdateCachedAABBAndBSphere;
begin
  // You MUST override TSpacePartitionLeaf.UpdateCachedAABBAndBSphere, if you
  // only have easy access to a bounding sphere, or only an axis aligned
  // bounding box, you can easily convert from one to the other by using
  // AABBToBSphere and BSphereToAABB.
  //
  // You MUST set both FCachedAABB AND FCachedBSphere
  Assert(false, 'You MUST override TSpacePartitionLeaf.UpdateCachedAABBAndBSphere!');
end;

procedure TSpacePartitionLeaf.Changed;
begin
  UpdateCachedAABBAndBSphere;
  SpacePartition.LeafChanged(self);
end;

constructor TSpacePartitionLeaf.CreateOwned(SpacePartition: TBaseSpacePartition);
begin
  inherited Create;
  
  FSpacePartition := SpacePartition;

  if SpacePartition <> nil then
    SpacePartition.AddLeaf(self);
end;

destructor TSpacePartitionLeaf.Destroy;
begin
  if Assigned(FSpacePartition) then
    FSpacePartition.RemoveLeaf(self);

  inherited;
end;

procedure TSpacePartitionLeaf.SetSpacePartition(
  const Value: TBaseSpacePartition);
begin
  if Assigned(FSpacePartition) then
    FSpacePartition.RemoveLeaf(self);

  FSpacePartition := Value;

  if Assigned(FSpacePartition) then
    FSpacePartition.AddLeaf(self);
end;

{ TSpacePartitionLeafList }

constructor TSpacePartitionLeafList.Create;
begin
  inherited;
  GrowthDelta := 128;
end;

function TSpacePartitionLeafList.GetItems(i: integer): TSpacePartitionLeaf;
begin
  result := TSpacePartitionLeaf(Get(i));
end;

procedure TSpacePartitionLeafList.SetItems(i: integer;
  const Value: TSpacePartitionLeaf);
begin
  Put(i, Value);
end;

{ TBaseSpacePartition }

procedure TBaseSpacePartition.AddLeaf(aLeaf: TSpacePartitionLeaf);
begin
  // Virtual
  aLeaf.UpdateCachedAABBAndBSphere;
end;

procedure TBaseSpacePartition.Clear;
begin
  // Virtual
end;

constructor TBaseSpacePartition.Create;
begin
  inherited;

  FQueryResult := TSpacePartitionLeafList.Create
end;

destructor TBaseSpacePartition.Destroy;
begin
  FreeAndNil(FQueryResult);
  inherited;
end;

procedure TBaseSpacePartition.FlushQueryResult;
begin
  FQueryResult.Count := 0;
  FQueryInterObjectTests := 0;
end;

procedure TBaseSpacePartition.LeafChanged(aLeaf: TSpacePartitionLeaf);
begin
  // Virtual
end;

procedure TBaseSpacePartition.ProcessUpdated;
begin
  // Virtual
end;

function TBaseSpacePartition.QueryAABB(const aAABB : TAABB): integer;
begin
  // Virtual
  result := 0;
end;

function TBaseSpacePartition.QueryBSphere(const aBSphere : TBSphere) : integer;
begin
  // Virtual
  result := 0;
end;

function TBaseSpacePartition.QueryCone(const aCone: TSPCone): integer;
begin
  // Virtual
  result := 0;
end;

function TBaseSpacePartition.QueryPlane(
  const Location, Normal: TAffineVector): integer;
begin
  // Virtual
  result := 0;
end;

function TBaseSpacePartition.QueryLeaf(
  const aLeaf: TSpacePartitionLeaf): integer;
begin
  QueryBSphere(aLeaf.FCachedBSphere);
  // Remove self if it was included (it should have been)
  FQueryResult.Remove(aLeaf);
  result := FQueryResult.Count;
end;

procedure TBaseSpacePartition.RemoveLeaf(aLeaf: TSpacePartitionLeaf);
begin
  // Virtual
end;

function TBaseSpacePartition.QueryFrustum(
  const Frustum: TFrustum): integer;
begin
  // Virtual
  result := 0;
end;

function TBaseSpacePartition.QueryFrustumEx(
  const ExtendedFrustum: TExtendedFrustum): integer;
begin
  // Virtual
  result := 0;
end;

{ TLeavedSpacePartition }

procedure TLeavedSpacePartition.AddLeaf(aLeaf: TSpacePartitionLeaf);
begin
  FLeaves.Add(aLeaf);
  aLeaf.UpdateCachedAABBAndBSphere;
end;

procedure TLeavedSpacePartition.Clear;
var
  i : integer;
begin
  inherited;

  for i := 0 to FLeaves.Count-1 do
  begin
    FLeaves[i].FSpacePartition := nil;
    FLeaves[i].Free;
  end;

  FLeaves.Clear;
end;

constructor TLeavedSpacePartition.Create;
begin
  inherited;

  FLeaves := TSpacePartitionLeafList.Create;
end;

destructor TLeavedSpacePartition.Destroy;
begin
  Clear;
  FreeAndNil(FLeaves);

  inherited;
end;

procedure TLeavedSpacePartition.RemoveLeaf(aLeaf: TSpacePartitionLeaf);
begin
  FLeaves.Remove(aLeaf);
end;

function TLeavedSpacePartition.QueryAABB(const aAABB: TAABB): integer;
var
  i : integer;
begin
  // Very brute force!
  FlushQueryResult;

  for i := 0 to Leaves.Count-1 do
  begin
    inc(FQueryInterObjectTests);

    if IntersectAABBsAbsolute(aAABB, Leaves[i].FCachedAABB) then
      FQueryResult.Add(Leaves[i]);
  end;

  result := FQueryResult.Count;
end;

function TLeavedSpacePartition.QueryBSphere(const aBSphere : TBSphere) : integer;
var
  i : integer;
  Distance2 : single;

  Leaf : TSpacePartitionLeaf;
begin
  // Very brute force!
  FlushQueryResult;

  for i := 0 to Leaves.Count-1 do
  begin
    Leaf := Leaves[i];
    Distance2 := VectorDistance2(Leaf.FCachedBSphere.Center, aBSphere.Center);

    inc(FQueryInterObjectTests);

    if Distance2<sqr(Leaf.FCachedBSphere.Radius + aBSphere.Radius) then
      FQueryResult.Add(Leaf);
  end;

  result := FQueryResult.Count;
end;

function TLeavedSpacePartition.QueryCone(const aCone: TSPCone): integer;
var
  i : integer;
begin
  // Very brute force!
  FlushQueryResult;

  for i := 0 to Leaves.Count-1 do
  begin
    inc(FQueryInterObjectTests);

    if ConeContainsBSphere(aCone, Leaves[i].FCachedBSphere)<>scNoOverlap then
      FQueryResult.Add(Leaves[i]);
  end;

  result := FQueryResult.Count;
end;

function TLeavedSpacePartition.QueryPlane(
  const FLocation, FNormal: TAffineVector): integer;
var
  i : integer;
  currentPenetrationDepth : single;
  Leaf : TSpacePartitionLeaf;
begin
  // Very brute force!
  FlushQueryResult;

  for i := 0 to Leaves.Count-1 do
  begin
    inc(FQueryInterObjectTests);

    Leaf := Leaves[i];

    currentPenetrationDepth := -(PointPlaneDistance(Leaf.FCachedBSphere.Center, FLocation, FNormal)-Leaf.FCachedBSphere.Radius);

    // Correct the node location
    if currentPenetrationDepth>0 then
      FQueryResult.Add(Leaves[i]);
  end;//}

  result := FQueryResult.Count;
end;

{ TSectorNode }

function TSectorNode.AABBFitsInNode(const aAABB: TAABB): boolean;
begin
  result := ContainsAABB(aAABB) in [scContainsFully];
end;

function TSectorNode.AABBIntersectsNode(const aAABB: TAABB): boolean;
begin
  result := ContainsAABB(aAABB) in [scContainsPartially, scContainsFully];
end;

procedure TSectorNode.AddAllLeavesRecursive(const QueryResult : TSpacePartitionLeafList);
var
  i : integer;
begin
  for i := 0 to FLeaves.Count-1 do
    QueryResult.Add(FLEaves[i]);

  for i := 0 to FChildCount-1 do
    FChildren[i].AddAllLeavesRecursive(QueryResult);
end;

function TSectorNode.AddLeaf(aLeaf: TSpacePartitionLeaf): TSectorNode;
begin
  // Time to grow the node?
  if NoChildren and
    (FLeaves.Count>=FSectoredSpacePartition.FLeafThreshold) and
    (FNodeDepth<FSectoredSpacePartition.FMaxTreeDepth) then
  begin
    ExpandNode;
  end;

  inc(FRecursiveLeafCount);

  if NoChildren then
  begin
    FLeaves.Add(aLeaf);
    ChildrenChanged;
    aLeaf.FPartitionTag := self;
    result := self;
  end else
  begin
    // Does it fit completely in any of the children?
    result := PlaceLeafInChild(aLeaf);
  end;
end;

function TSectorNode.BSphereFitsInNode(const BSphere: TBSphere): boolean;
begin
  result := ContainsBSphere(BSphere) in [scContainsFully];
end;

function TSectorNode.BSphereIntersectsNode(const BSphere: TBSphere): boolean;
begin
  result := ContainsBSphere(BSphere) in [scContainsPartially, scContainsFully];
end;

function TSectorNode.CalcRecursiveLeafCount: integer;
var
  i : integer;
begin
  result := FLeaves.Count;

  for i := 0 to FChildCount-1 do
    result := result + FChildren[i].CalcRecursiveLeafCount;
end;

procedure TSectorNode.Clear;
var
  i : integer;
begin
  for i := 0 to FChildCount-1 do
    FreeAndNil(FChildren[i]);

  FChildCount := 0;

  FLeaves.Clear;
end;

constructor TSectorNode.Create(aSectoredSpacePartition : TSectoredSpacePartition; aParent : TSectorNode);
begin
  FLeaves := TSpacePartitionLeafList.Create;
  FChildCount := 0;
  FParent := aParent;
  FSectoredSpacePartition := aSectoredSpacePartition;

  if aParent=nil then
    FNodeDepth := 0
  else
    FNodeDepth := aParent.FNodeDepth + 1;
end;

procedure TSectorNode.ExpandNode;
var
  i : integer;
  OldLeaves : TSpacePartitionLeafList;

begin
  CreateChildren;

  // The children have been added, now move all leaves to the children - if
  // we can
  OldLeaves := FLeaves;
  try
    FLeaves := TSpacePartitionLeafList.Create;
    for i := 0 to OldLeaves.Count-1 do
      PlaceLeafInChild(OldLeaves[i]);
  finally
    OldLeaves.Free;
  end;
end;

procedure TSectorNode.CollapseNode;
var
  i,j : integer;
begin
  for i := 0 to FChildCount-1 do begin
    FChildren[i].CollapseNode;

    for j := 0 to FChildren[i].FLeaves.Count-1 do begin
      FChildren[i].FLeaves[j].FPartitionTag := self;
      FLeaves.Add(FChildren[i].FLeaves[j]);
    end;

    FChildren[i].FLeaves.Clear;

    FreeAndNil(FChildren[i]);
  end;

  FChildCount := 0;
end;

destructor TSectorNode.Destroy;
begin
  Clear;
  FreeAndNil(FLeaves);
  inherited;
end;

function TSectorNode.GetNoChildren: boolean;
begin
  result := FChildCount=0;
end;

function TSectorNode.GetNodeCount: integer;
var
  i : integer;
begin
  result := 1;

  for i := 0 to FChildCount-1 do
    result := result + FChildren[i].GetNodeCount;
end;

function TSectorNode.PlaceLeafInChild(aLeaf: TSpacePartitionLeaf)  : TSectorNode;
var
  ChildNode : TSectorNode;
begin
  // Which child does it fit in?
  ChildNode := GetChildForAABB(aLeaf.FCachedAABB);

  if ChildNode <> nil then
  begin
    result := ChildNode.AddLeaf(aLeaf);
    exit;
  end;//}
  
  // Doesn't fit the any child
  aLeaf.FPartitionTag := self;
  FLeaves.Add(aLeaf);
  ChildrenChanged;
  result := self;
end;

procedure TSectorNode.QueryAABB(const aAABB: TAABB;
  const QueryResult: TSpacePartitionLeafList);
var
  i : integer;
  SpaceContains : TSpaceContains;
begin
  inc(FSectoredSpacePartition.FQueryNodeTests);

  SpaceContains := AABBContainsSector(aAABB);

  if SpaceContains = scContainsFully then
  begin
    AddAllLeavesRecursive(QueryResult);
  end else
  if SpaceContains = scContainsPartially then
  begin
    // Add all leaves that overlap
    if FSectoredSpacePartition.CullingMode = cmFineCulling then
    begin
      for i := 0 to FLeaves.Count-1 do
      begin
        inc(FSectoredSpacePartition.FQueryInterObjectTests);

        if IntersectAABBsAbsolute(FLeaves[i].FCachedAABB, aAABB) then
          QueryResult.Add(FLeaves[i]);
      end;
    end else
    begin
      for i := 0 to FLeaves.Count-1 do
        QueryResult.Add(FLeaves[i]);
    end;

    // Recursively let the children add their leaves
    for i := 0 to FChildCount-1 do
      FChildren[i].QueryAABB(aAABB, QueryResult);
  end;
end;

procedure TSectorNode.QueryBSphere(const aBSphere: TBSphere;
  const QueryResult: TSpacePartitionLeafList);
var
  i : integer;
  SpaceContains : TSpaceContains;
begin
  inc(FSectoredSpacePartition.FQueryNodeTests);

  SpaceContains := BSphereContainsSector(aBSphere);

  if SpaceContains = scContainsFully then
  begin
    AddAllLeavesRecursive(QueryResult);
  end else
  if SpaceContains = scContainsPartially then
  begin
    // Add all leaves that overlap
    if FSectoredSpacePartition.CullingMode = cmFineCulling then
    begin
      for i := 0 to FLeaves.Count-1 do
      begin
        inc(FSectoredSpacePartition.FQueryInterObjectTests);
        if BSphereContainsAABB(aBSphere, FLeaves[i].FCachedAABB) <> scNoOverlap then
          QueryResult.Add(FLeaves[i]);
      end;
    end else
      for i := 0 to FLeaves.Count-1 do
        QueryResult.Add(FLeaves[i]);

    // Recursively let the children add their leaves
    for i := 0 to FChildCount-1 do
      FChildren[i].QueryBSphere(aBSphere, QueryResult);
  end;
end;

procedure TSectorNode.QueryPlane(const Location, Normal: TAffineVector;
  const QueryResult: TSpacePartitionLeafList);
var
  i : integer;
  SpaceContains : TSpaceContains;
begin
  inc(FSectoredSpacePartition.FQueryNodeTests);

  SpaceContains := PlaneContainsBSphere(Location, Normal, FBSphere);

  if SpaceContains = scContainsFully then
  begin
    AddAllLeavesRecursive(QueryResult);
  end else
  if SpaceContains = scContainsPartially then
  begin
    // Add all leaves that overlap
    if FSectoredSpacePartition.CullingMode = cmFineCulling then
    begin
      for i := 0 to FLeaves.Count-1 do
        if PlaneContainsBSphere(Location, Normal, FLeaves[i].FCachedBSphere) <> scNoOverlap then
          QueryResult.Add(FLeaves[i]);
    end else
      for i := 0 to FLeaves.Count-1 do
        QueryResult.Add(FLeaves[i]);

    // Recursively let the children add their leaves
    for i := 0 to FChildCount-1 do
    begin
      inc(FSectoredSpacePartition.FQueryInterObjectTests);

      FChildren[i].QueryPlane(Location, Normal, QueryResult);
    end;
  end;//}
end;

function TSectorNode.RemoveLeaf(aLeaf: TSpacePartitionLeaf; OwnerByThis : boolean) : boolean;
begin
  result := false;
  dec(FRecursiveLeafCount);

  if OwnerByThis then
  begin
    aLeaf.FPartitionTag := nil;
    FLeaves.Remove(aLeaf);
    ChildrenChanged;
  end;

  // If there aren't enough leaves anymore, it's time to remove the node!
  if not NoChildren and (FRecursiveLeafCount+1<FSectoredSpacePartition.FLeafThreshold) then begin
    CollapseNode;
    result := true;
  end;

  if Parent<>nil then
    Parent.RemoveLeaf(aLeaf, false);
end;

function TSectorNode.VerifyRecursiveLeafCount : string;
var
  i : integer;
begin
  if FRecursiveLeafCount<>CalcRecursiveLeafCount then
  begin
    result := Format('Node at depth %d mismatches, %d<>%d!',[FNodeDepth, FRecursiveLeafCount, CalcRecursiveLeafCount]);
    exit;
  end;

  for i := 0 to FChildCount-1 do
  begin
    result := FChildren[i].VerifyRecursiveLeafCount;
    if result<>'' then
      exit;
  end;
end;

procedure TSectorNode.CreateChildren;
begin
  Assert(false, 'You must override CreateChildren!');
end;

function TSectorNode.AABBContainsSector(const AABB: TAABB): TSpaceContains;
begin
  result := AABBContainsAABB(AABB, FAABB);
end;

function TSectorNode.BSphereContainsSector(
  const BSphere: TBSphere): TSpaceContains;
begin
  result := BSphereContainsAABB(BSphere, FAABB);
end;

function TSectorNode.ContainsAABB(const aAABB: TAABB): TSpaceContains;
begin
  result := AABBContainsAABB(FAABB, aAABB);
end;

function TSectorNode.ContainsBSphere(
  const aBSphere: TBSphere): TSpaceContains;
begin
  result := AABBContainsBSphere(FAABB, aBSphere);
end;

procedure TSectorNode.SetAABB(const Value: TAABB);
begin
  FAABB := Value;

  AABBToBSphere(FAABB, FBSphere);
end;

function TSectorNode.GetChildForAABB(
  AABB: TAABB): TSectorNode;
var
  Location : TAffineVector;
  ChildNode : TSectorNode;
  ChildNodeIndex : integer;
begin
  Assert(FChildCount>0, 'There are no children in this node!');
  // Instead of looping through all children, we simply determine on which
  // side of the center node the child is located
  ChildNodeIndex := 0;

  Location := AABB.min;

  // Upper / Lower
  if Location[1]<FBSphere.Center[1] then  ChildNodeIndex := 4;

  // Left / Right
  if Location[2]<FBSphere.Center[2] then ChildNodeIndex := ChildNodeIndex or 2;

  // Fore / Back
  if Location[0]>FBSphere.Center[0] then ChildNodeIndex := ChildNodeIndex or 1;

  Assert((ChildNodeIndex>=0) and (ChildNodeIndex<=8),
    Format('ChildNodeIndex is out of range (%d)!',[ChildNodeIndex]));

  ChildNode := FChildren[ChildNodeIndex];

  Assert(Assigned(ChildNode),'ChildNode not assigned');

  if ChildNode.AABBFitsInNode(AABB) then
    result := ChildNode
  else
    result := nil;
end;

function TSectorNode.GetCenter: TAffineVector;
begin
  result := FBSphere.Center;
end;

procedure TSectorNode.QueryFrustum(const Frustum: TFrustum;
  const QueryResult: TSpacePartitionLeafList);
var
  SpaceContains : TSpaceContains;
  i : integer;
begin
  inc(FSectoredSpacePartition.FQueryNodeTests);

  // Check if the frustum contains the bsphere of the node
  if not IsVolumeClipped(BSphere.Center, BSphere.Radius, Frustum) then
    SpaceContains := FrustumContainsAABB(frustum, AABB)
  else
    SpaceContains := scNoOverlap;

  // If the frustum fully contains the leaf, then we need not check every piece,
  // just add them all
  if SpaceContains=scContainsFully then begin
    AddAllLeavesRecursive(QueryResult);
  end else

  // If the frustum partiall contains the leaf, then we should add the leaves
  // that intersect the frustum and recurse for all children
  if SpaceContains=scContainsPartially then begin
    for i := 0 to FLeaves.Count-1 do begin
      inc(FSectoredSpacePartition.FQueryInterObjectTests);

      if not IsVolumeClipped(FLeaves[i].FCachedBSphere.Center,FLeaves[i].FCachedBSphere.Radius,Frustum) then
        QueryResult.Add(FLeaves[i]);
    end;

    // Recursively let the children add their leaves
    for i := 0 to FChildCount-1 do
      FChildren[i].QueryFrustum(Frustum,QueryResult);
  end;
end;

procedure TSectorNode.ChildrenChanged;
begin
  // Do nothing in the basic case
end;

procedure TSectorNode.QueryFrustumEx(
  const ExtendedFrustum: TExtendedFrustum;
  const QueryResult: TSpacePartitionLeafList);
var
  SpaceContains : TSpaceContains;
  i : integer;
begin
  inc(FSectoredSpacePartition.FQueryNodeTests);

  // Does the extended frustum intersect the bsphere at all?
  if not ExtendedFrustumIntersectsBSphere(ExtendedFrustum, BSphere) then
    SpaceContains := scNoOverlap

  else
  // Test if the bounding frustum intersects the AABB of the node
    SpaceContains := FrustumContainsAABB(ExtendedFrustum.Frustum, AABB);//}

  // If the frustum fully contains the leaf, then we need not check every piece,
  // just add them all
  if SpaceContains=scContainsFully then begin
    AddAllLeavesRecursive(QueryResult);
  end else

  // If the frustum partially contains the leaf, then we should add the leaves
  // that intersect the frustum and recurse for all children
  if SpaceContains=scContainsPartially then begin
    for i := 0 to FLeaves.Count-1 do begin
      // Early out 1
      if not BSphereIntersectsBSphere(FLeaves[i].FCachedBSphere, ExtendedFrustum.BSphere) then continue;

      inc(FSectoredSpacePartition.FQueryInterObjectTests);

      if not IsVolumeClipped(FLeaves[i].FCachedBSphere.Center, FLeaves[i].FCachedBSphere.Radius, ExtendedFrustum.Frustum) then
        QueryResult.Add(FLeaves[i]);
    end;

    // Recursively let the children add their leaves
    for i := 0 to FChildCount-1 do
      FChildren[i].QueryFrustumEx(ExtendedFrustum, QueryResult);
  end;
end;

{ TSectoredSpacePartition }

procedure TSectoredSpacePartition.AddLeaf(aLeaf: TSpacePartitionLeaf);
begin
  inherited;
  FRootNode.AddLeaf(aLeaf);

  if not FRootNode.AABBFitsInNode(aLeaf.FCachedAABB) then
  begin
    if FGrowMethod in [gmBestFit, gmIncreaseToFitAll] then
      UpdateStructureSize(GrowGravy)
    else
      Assert(False, 'Node is outside Octree!');
  end;
end;

constructor TSectoredSpacePartition.Create;
begin
  FLeafThreshold := cOctree_LEAF_TRHESHOLD;
  FMaxTreeDepth := cOctree_MAX_TREE_DEPTH;

  FRootNode := CreateNewNode(nil);
  FGrowMethod := gmIncreaseToFitAll;

  FGrowGravy := cOctree_GROW_GRAVY;

  inherited Create;
end;

function TSectoredSpacePartition.CreateNewNode(aParent : TSectorNode) : TSectorNode;
begin
  result := TSectorNode.Create(self, aParent);
end;

destructor TSectoredSpacePartition.Destroy;
begin
  FRootNode.Free;
  inherited;
end;

function TSectoredSpacePartition.GetAABB: TAABB;
var
  i : integer;
begin
  if FLeaves.Count=0 then
  begin
    MakeVector(result.min, 0,0,0);
    MakeVector(result.max, 0,0,0);
  end else
  begin
    result := FLeaves[0].FCachedAABB;

    for i := 1 to FLeaves.Count-1 do
      AddAABB(result, FLeaves[i].FCachedAABB);
  end;
end;

function TSectoredSpacePartition.GetNodeCount: integer;
begin
  result := FRootNode.GetNodeCount;
end;

procedure TSectoredSpacePartition.LeafChanged(aLeaf: TSpacePartitionLeaf);
var
  Node : TSectorNode;
begin
  // If the leaf still fits in the old node, leave it there - or in one of the
  // children
  Node := TSectorNode(aLeaf.FPartitionTag);

  Assert(Node<>nil,'No leaf node could be found!');

  if Node.AABBFitsInNode(aLeaf.FCachedAABB) then
  begin
    // If the node has children, try to add the leaf to them - otherwise just
    // leave it!
    if Node.FChildCount>0 then
    begin
      Node.FLeaves.Remove(aLeaf);
      Node.PlaceLeafInChild(aLeaf);
      Node.ChildrenChanged;
    end;
  end else
  begin
    Node.RemoveLeaf(aLeaf, true);

    // Does this leaf still fit in the Octree?
    if not FRootNode.AABBFitsInNode(aLeaf.FCachedAABB) then
    begin
      if FGrowMethod in [gmBestFit, gmIncreaseToFitAll] then
        UpdateStructureSize(cOctree_GROW_GRAVY)
      else
        Assert(False, 'Node is outside Octree!');
    end else
      FRootNode.AddLeaf(aLeaf);
  end;
end;

function TSectoredSpacePartition.QueryAABB(const aAABB: TAABB): integer;
begin
  FlushQueryResult;
  FRootNode.QueryAABB(aAABB, FQueryResult);
  result := FQueryResult.Count;
end;

function TSectoredSpacePartition.QueryBSphere(
  const aBSphere: TBSphere): integer;
begin
  FlushQueryResult;
  FRootNode.QueryBSphere(aBSphere, FQueryResult);
  result := FQueryResult.Count;
end;

function TSectoredSpacePartition.QueryPlane(const Location,
  Normal: TAffineVector): integer;
begin
  FlushQueryResult;
  FRootNode.QueryPlane(Location, Normal, FQueryResult);
  result := FQueryResult.Count;
end;

function TSectoredSpacePartition.QueryLeaf(
  const aLeaf: TSpacePartitionLeaf): integer;
var
  i : integer;
  Node : TSectorNode;
  TestLeaf : TSpacePartitionLeaf;
begin
  // Query current node and all nodes upwards until we find the root, no need
  // to check intersections, because we know that the leaf partially intersects
  // all it's parents.

  Node := TSectorNode(aLeaf.FPartitionTag);
  FlushQueryResult;

  // First, query downwards!
  Node.QueryAABB(aLeaf.FCachedAABB, QueryResult);

  // Now, query parents and upwards!
  Node := Node.Parent;

  while Node<>nil do
  begin
    inc(FQueryNodeTests);

    // Add all leaves that overlap
    for i := 0 to Node.FLeaves.Count-1 do
    begin
      TestLeaf := Node.FLeaves[i];
      inc(FQueryInterObjectTests);
      if IntersectAABBsAbsolute(TestLeaf.FCachedAABB, aLeaf.FCachedAABB) then
        QueryResult.Add(TestLeaf);
    end;

    // Try the parent
    Node := Node.Parent;
  end;

  QueryResult.Remove(aLeaf);

  result := QueryResult.Count;
end;

procedure TSectoredSpacePartition.RemoveLeaf(aLeaf: TSpacePartitionLeaf);
begin
  inherited;
  TSectorNode(aLeaf.FPartitionTag).RemoveLeaf(aLeaf, true);
end;

procedure TSectoredSpacePartition.SetLeafThreshold(const Value: integer);
begin
  FLeafThreshold := Value;
end;

procedure TSectoredSpacePartition.SetMaxTreeDepth(const Value: integer);
begin
  FMaxTreeDepth := Value;
end;

procedure TSectoredSpacePartition.RebuildTree(const NewAABB : TAABB);
var
  i : integer;
  OldLeaves : TSpacePartitionLeafList;
  tempGrowMethod : TGrowMethod;
begin
  // Delete ALL nodes in the tree
  FRootNode.Free;

  FRootNode := CreateNewNode(nil);
  FRootNode.AABB := NewAABB;

  // Insert all nodes again
  OldLeaves := FLeaves;
  FLeaves := TSpacePartitionLeafList.Create;

  // This will cause an except if the build goes badly, which is better than
  // an infinite loop
  tempGrowMethod := FGrowMethod;

  for i := 0 to OldLeaves.Count-1 do
    AddLeaf(OldLeaves[i]);

  OldLeaves.Free;  
  FGrowMethod := tempGrowMethod;
end;

procedure TSectoredSpacePartition.UpdateStructureSize(Gravy: single);
var
  MaxAABB, NewAABB : TAABB;
  AABBSize : TAffineVector;
begin
  // Create the new extents for the Octree
  MaxAABB := GetAABB;
  AABBSize := VectorSubtract(MaxAABB.max, MaxAABB.min);

  if FGrowMethod=gmBestFit then
  begin
    NewAABB.min := VectorSubtract(MaxAABB.min, VectorScale(AABBSize, Gravy));
    NewAABB.max := VectorAdd(MaxAABB.max, VectorScale(AABBSize, Gravy));//}
  end else
  if FGrowMethod=gmIncreaseToFitAll then
  begin
    NewAABB.min := VectorSubtract(MaxAABB.min, VectorScale(AABBSize, Gravy));
    NewAABB.max := VectorAdd(MaxAABB.max, VectorScale(AABBSize, Gravy));//}

    AddAABB(NewAABB, FRootNode.AABB);
  end;

  RebuildTree(NewAABB);
end;

procedure TSectoredSpacePartition.FlushQueryResult;
begin
  inherited;

  FQueryNodeTests := 0;
end;

function TSectoredSpacePartition.QueryFrustum(
  const Frustum: TFrustum): integer;
begin
  FlushQueryResult;
  FRootNode.QueryFrustum(Frustum, FQueryResult);
  result := FQueryResult.Count;
end;

function TSectoredSpacePartition.QueryFrustumEx(
  const ExtendedFrustum: TExtendedFrustum): integer;
begin
  FlushQueryResult;
  FRootNode.QueryFrustumEx(ExtendedFrustum, FQueryResult);
  result := FQueryResult.Count;
end;

{ TSPOctreeNode }

function TSPOctreeNode.AABBFitsInNode(const aAABB: TAABB): boolean;
begin
  // Faster than inherited method
  result := AABBFitsInAABBAbsolute(aAABB, FAABB);
end;

function TSPOctreeNode.AABBIntersectsNode(const aAABB: TAABB): boolean;
begin
  // Faster than inherited method
  result := IntersectAABBsAbsolute(FAABB, aAABB);
end;

function TSPOctreeNode.BSphereFitsInNode(const BSphere: TBSphere): boolean;
var
  AABB : TAABB;
begin
  // Faster than inherited method
  BSphereToAABB(BSphere, AABB);
  result := AABBFitsInAABBAbsolute(AABB, FAABB);//}
end;

function TSPOctreeNode.BSphereIntersectsNode(
  const BSphere: TBSphere): boolean;
var
  AABB : TAABB;
begin
  // Faster than inherited method
  BSphereToAABB(BSphere, AABB);
  result := IntersectAABBsAbsolute(AABB, FAABB);//}
end;

procedure TSPOctreeNode.CreateChildren;
var
  i : integer;
  AABB : TAABB;
  function GetExtent(const flags: array of byte): TAffineVector;
  var
    n: integer;
  begin
     for n:=0 to 2 do begin
       case flags[n] of
         cMIN: result[n]:=FAABB.min[n];
         cMID: result[n]:=(FAABB.max[n]+FAABB.min[n])/2;
         cMAX: result[n]:=FAABB.max[n];
       end;
     end;
  end;
begin
  Assert(FChildCount=0, 'Children allready exist!');

  for i := 0 to 7 do
  begin
    FChildren[i] := FSectoredSpacePartition.CreateNewNode(self);

    //Generate new extents based on parent's extents
    AABB.min :=GetExtent(cOctFlagMIN[i]);
    AABB.max :=GetExtent(cOctFlagMax[i]);
    FChildren[i].AABB := AABB;
  end;

  FChildCount := 8;
end;

{ TOctreeSpacePartition }

function TOctreeSpacePartition.CreateNewNode(
  aParent: TSectorNode): TSectorNode;
begin
  result := TSPOctreeNode.Create(self, aParent);
end;

procedure TOctreeSpacePartition.SetSize(const Min, Max : TAffineVector);
var
  AABB : TAABB;
begin
  AABB.Min := Min;
  AABB.Max := Max;

  RebuildTree(AABB);
end;

{ TSPQuadtreeNode }

function TSPQuadtreeNode.AABBFitsInNode(const aAABB: TAABB): boolean;
begin
  result :=
    (aAABB.min[0]>=FAABB.min[0]) and
    (aAABB.min[2]>=FAABB.min[2]) and

    (aAABB.max[0]<=FAABB.max[0]) and
    (aAABB.max[2]<=FAABB.max[2]);
end;

function TSPQuadtreeNode.AABBIntersectsNode(const aAABB: TAABB): boolean;
begin
  Assert(false,Format('AABBIntersectsNode not implemented on %s',[ClassName]));
  result := false;
end;

function TSPQuadtreeNode.BSphereFitsInNode(const BSphere: TBSphere): boolean;
begin
  Assert(false,Format('BSphereFitsInNode not implemented on %s',[ClassName]));
  result := false;
end;

function TSPQuadtreeNode.BSphereIntersectsNode(
  const BSphere: TBSphere): boolean;
begin
  Assert(false,Format('BSphereIntersectsNode not implemented on %s',[ClassName]));
  result := false;
end;

procedure TSPQuadtreeNode.ChildrenChanged;
var
  i : integer;
  newMin, newMax : single;
begin
  inherited;

  // Establish a baseline
  if Leaves.Count>0 then begin
    newMin := Leaves[0].FCachedAABB.Min[1];
    newMax := Leaves[0].FCachedAABB.Max[1];
  end else

  if FChildCount>0 then begin
    newMin := FChildren[0].AABB.Min[1];
    newMax := FChildren[0].AABB.Max[1];
  end else begin
    // This should never happen!
    newMin := 1e9;
    newMax := -1e9;
  end;

  for i := 0 to Leaves.Count-1 do begin
    newMin := min(newMin, Leaves[i].FCachedAABB.Min[1]);
    newMax := max(newMax, Leaves[i].FCachedAABB.Max[1]);
  end;

  for i := 0 to FChildCount-1 do begin
    newMin := min(newMin, FChildren[i].AABB.Min[1]);
    newMax := max(newMax, FChildren[i].AABB.Max[1]);
  end;

  if (AABB.max[1] <> newMax) and (AABB.min[1] <> newMin) then begin
    FAABB.max[1] := newMax;
    FAABB.min[1] := newMin;

    // Make sure the parent updates it's bounds as well
    if Assigned(Parent) then
      Parent.ChildrenChanged;//}
  end;
end;

procedure TSPQuadtreeNode.CreateChildren;
var
  ChildNodeIndex : integer;
  AABB : TAABB;
  X, Z : integer;
begin
  for ChildNodeIndex := 0 to 3 do
  begin
    FChildren[ChildNodeIndex] := FSectoredSpacePartition.CreateNewNode(self);

    // Y is ignored so it's set to a very large number
    AABB.min[1] := FAABB.min[1];
    AABB.max[1] := FAABB.max[1];

    //Generate new extents based on parent's extents
    if ((ChildNodeIndex and 1)>0) then X := 1 else X := 0;
    if ((ChildNodeIndex and 2)>0) then Z := 1 else Z := 0;

    if X = 0 then
    begin
      AABB.min[0] := FAABB.min[0] + (FAABB.max[0]+FAABB.min[0])/2 * X;
      AABB.max[0] := (FAABB.max[0]+FAABB.min[0])/2 * (1+X);
    end else
    begin
      AABB.min[0] := (FAABB.max[0]+FAABB.min[0])/2;
      AABB.max[0] := FAABB.max[0];
    end;

    if Z = 0 then
    begin
      AABB.min[2] := FAABB.min[2];
      AABB.max[2] := (FAABB.max[2]+FAABB.min[2])/2;
    end else
    begin
      AABB.min[2] := (FAABB.max[2]+FAABB.min[2])/2;
      AABB.max[2] := FAABB.max[2];
    end;

    FChildren[ChildNodeIndex].AABB := AABB;
  end;

  FChildCount := 4;
end;

function TSPQuadtreeNode.GetChildForAABB(AABB: TAABB): TSectorNode;
var
  Location : TAffineVector;
  ChildNode : TSectorNode;
  ChildNodeIndex : integer;
begin
  // Instead of looping through all children, we simply determine on which
  // side of the center node the child is located
  ChildNodeIndex := 0;

  Location := AABB.min;

  // Fore / Back
  if Location[0]>FBSphere.Center[0] then ChildNodeIndex := ChildNodeIndex or 1;

  // Left / Right
  if Location[2]>FBSphere.Center[2] then ChildNodeIndex := ChildNodeIndex or 2;

  Assert(ChildNodeIndex<ChildCount,'Bad ChildNodeIndex!');
  ChildNode := FChildren[ChildNodeIndex];

  if ChildNode.AABBFitsInNode(AABB) then
  begin
    result := ChildNode;
    exit;
  end;

  result := nil;
end;

{ TQuadtreeSpacePartition }

function TQuadtreeSpacePartition.CreateNewNode(
  aParent: TSectorNode): TSectorNode;
begin
  result := TSPQuadtreeNode.Create(self, aParent);
end;

procedure TQuadtreeSpacePartition.SetSize(const Min, Max: TAffineVector);
var
  AABB : TAABB;
begin
  AABB.Min := Min;
  AABB.Max := Max;

  RebuildTree(AABB);
end;
end.
