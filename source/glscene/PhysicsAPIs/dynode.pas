unit dynode; // Autocreated dynamic version of odeimport.pas.
{*************************************************************************
 *                                                                       *
 * Open Dynamics Engine, Copyright (C) 2001,2002 Russell L. Smith.       *
 * All rights reserved.  Email: russ@q12.org   Web: www.q12.org          *
 *                                                                       *
 * This library is free software; you can redistribute it and/or         *
 * modify it under the terms of EITHER:                                  *
 *   (1) The GNU Lesser General Public License as published by the Free  *
 *       Software Foundation; either version 2.1 of the License, or (at  *
 *       your option) any later version. The text of the GNU Lesser      *
 *       General Public License is included with this library in the     *
 *       file LICENSE.TXT.                                               *
 *   (2) The BSD-style license that is included with this library in     *
 *       the file LICENSE-BSD.TXT.                                       *
 *                                                                       *
 * This library is distributed in the hope that it will be useful,       *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files    *
 * LICENSE.TXT and LICENSE-BSD.TXT for more details.                     *
 *                                                                       *
 *************************************************************************}

{*************************************************************************
 *                                                                       *
 * ODE Delphi Import unit : 0.8.14                                       *
 *                                                                       *
 *   Created by Mattias Fagerlund ( mattias@cambrianlabs.com )  and      *
 *              Christophe ( chroma@skynet.be ) Hosten                   *
 *                                                                       *
 *  Special thanks to Eric Grange for his help                           *
 *                                                                       *
 *  All real work was of course performed by Russell L. Smith,           *
 *    who created ODE.                                                   *
 *                                                                       *
 *  Convertion started 2002-09-16.                                       *
 *                                                                       *
 *  There is no Delphi documentation for ODE, see the original ODE files *
 *  for information http://opende.sourceforge.net/ode-docs.html          *
 *                                                                       *
 *************************************************************************}

// This is a dynamically bound version of the odeimport.pas file - it
// was initiated by Martin Waldegger <martin.waldegger@chello.at> and
// auto-created by mattias fagerlund.
//
// This version was auto-created on 2004-07-11.


 {
  Some notes;

  Sometimes it's easier and faster to refer to the members of the objects
  directly, like Body.Pos, Geom.Data or Body.Mass, instead of calling the built
  in routines. Be very careful if you do this, because some of the built in
  routines alter states when called.

  Examples

  Geom.Body := Body; // DON'T DO THIS
  bGeomSetBody(Geom, Body); // This method must be used

  Setting the mass of a body is another example. Typically, reading members is
  fine, but before writing directly to member fields yourself, you should check
  the c source and see what the function/procedure does. The source can be found
  at http://www.q12.org/ode/

  ***********************************************************************

  Change history

  2004.05.19 - CH - New single and double dll. Added support for the new QuickStep solver
  2004.04.25 - CH - New single and double dll. Trimesh now works in both mode.
  2004.04.22 - MF - Fixes to make DelphiODE behave better when used as dynamic
  2004.04.21 - CH - New single and double dll. Now handles Capped Cylinder vs Trimesh collision
                    Added dJointGetUniversalAngle and dJointGetUniversalAngleRate, ...
  2004.04.08 - DL - Changed calling convention of dJointCreateContact and
                    dBodySetMass to require pointers. Again for compatability reasons.
  2004.04.08 - DL - Minor compatibilit fixes (performed by MF) 
  2004.04.05 - CH - New single and double dll. Now handles trimesh/trimesh collision
  2004.03.30 - DL - better crossplatform Module loading support using moduleloader.pas
  2004.03.11 - CH - New single and double dll. Now handles auto-disable and new terrain geom
  2004.03.08 - DL - Support for Delphi, Kylix, FreePascal, TMT Pascal and GnuPascal compilers
  2004.02.25 - CH - New single and double dll. Now handles breakable joints
  2004.02.18 - CH - New single and double dll
  2003.09.08 - CH - New single and double dll. Now handles cones and terrain
  2003.09.01 - EG - TriCollider bindings now dynamic to allow use of unit in Delphi5
                    (compatibility with other platforms unknow)
  2003.07.23 - CH - New single dll, now handles Plane2D
  2003.07.18 - CH - Added set and get UniversalParam, new dll deployed
  2003.07.07 - MF - DelphiODE now defaults to Single precision, because TriMesh
                    only works with Single precision. We're hoping this will be corrected in the
                    future
  2003.07.05 - CH - updated file to support new tri-collider code
  2003.06.13 - CH - Created new DLL, adding dWorldStepFast and dJointTypePlane2D
  2003.06.12 - MF - fixed Single support, which was slightly broken
  2003.06.10 - MF - Removed GeomTransformGroup as they're not in the DLL.
  2003.02.11 - JV - added syntax enforcement on some enumerated types
  2003.02.01 - CH - removed dGeomGroup and all it's procedures / functions due to deprecation
  2003.02.01 - MF - added a few new functions
  2003.01.20 - CH - compiled a new DLL and added the new functions.
  2002.10.31 - CH - compiled a new dll version, with some minor updates to friction among other things
  2002.10.10 - MF - added the functions needed to support cylinder and GeomTransformGroup.
  2002.10.10 - CH - compiled a new DLL with the new cylinder geom and the new GeomTransformGroup.
  2002.09.25 - MF - I'm having issues with the single precision DLL, it seems less stable.
                    DelphiODE will still default to double precision. The goal is single
                    precision though, so the Tri-Collider can be used.
  2002.09.24 - CH - New Single and Double precision DLLs created
  2002.09.22 - DL - Preliminary Linux support
  2002.09.16 - MF & CH - Conversion started

  MF = Mattias Fagerlund
  CH = Christophe Hosten (Chroma)
  JV = John Villar
  DL = Dominique Louis
  EG = Eric Grange
 }

{$I delphiode.inc}

interface

uses
{$IFDEF __GPC__}
  system,
  gpc,
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF FPC}
    {$IFDEF Ver1_0}
    linux,
    {$ELSE}
    pthreads,
    baseunix,
    unix,
    {$ENDIF}
    x,
    xlib,
  {$ELSE}
  Types,
  Libc,
  Xlib,
  {$ENDIF}
{$ENDIF}

{$IFDEF __MACH__}
  GPCMacOSAll,
  {$ENDIF}
  Classes;

const
  {$IFDEF WIN32}
  ODEDLL = 'ode.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  ODEDLL = 'libode.so';
  {$ENDIF}
  {$IFDEF MACOS}
  ODEDLL = 'libode.dylib';
  {$ENDIF}
  {$IFDEF DARWIN} // MacOS X
  ODEDLL = 'libode.dylib';
  {$ENDIF}

type
  // ********************************************************************
  // ********************************************************************
  //
  //   ODE precision:
  //
  //   ODE can be run in Single or Double precision, Single is less precise,
  //   but requires less memory.
  //
  //   If you choose to run in Single mode, you must deploy the single precision
  //   dll (this is default)
  //
  //   If you choose to run in Double mode, you must deploy the double precision
  //   dll (named ode-Double.dll and located in the dll directory)

  {$define cSINGLE}  // Remove "$" from "$define" to make DelphiODE double based

  {$ifdef cSINGLE}
    TdReal = single;
  {$else}
    TdReal = double;
  {$endif}

  PdReal = ^TdReal;

  {define cODEDebugEnabled} // Debug mode


  // Pointers to internal ODE structures. These I haven't been able to reproduce
  // in delphi, they're C++ classes.
  TODEID = cardinal;
  TdJointID = TODEID;
  TdJointGroupID = TODEID;

  TdRealArray = array[0..15] of TdReal;
  PdRealArray = ^TdRealArray;

  // typedef dReal dVector3[4];
  // Q: Why isn't TdVector3 = array[0..2] of TdReal?
  // A: Because of SIMD alignment.
  TdVector3 = array[0..3] of TdReal;
  PdVector3 = ^TdVector3;

  // typedef dReal dVector4[4];
  TdVector4 = array[0..3] of TdReal;
  PdVector4 = ^TdVector4;

  // typedef dReal dMatrix3[4*3];
  TdMatrix3 = array[0..4*3-1] of TdReal;
  PdMatrix3 = ^TdMatrix3;

  TdMatrix3_As3x4 = array[0..2, 0..3] of TdReal;
  PdMatrix3_As3x4 = ^TdMatrix3_As3x4;

  // typedef dReal dMatrix4[4*4];
  TdMatrix4 = array[0..4*4-1] of TdReal;
  PdMatrix4 = ^TdMatrix4;

  // typedef dReal dMatrix6[8*6];
  TdMatrix6 = array[0..8*6-1] of TdReal;
  PdMatrix6 = ^TdMatrix6;

  // typedef dReal dQuaternion[4];
  TdQuaternion = TdVector4;//array[0..3] of TdReal;
  PdQuaternion = ^TdQuaternion;

  // No typedef for AABB
  TdAABB = array[0..5] of TdReal;


(*enum {
  dxBodyFlagFiniteRotation = 1,		// use finite rotations
  dxBodyFlagFiniteRotationAxis = 2,	// use finite rotations only along axis
  dxBodyDisabled = 4			// body is disabled
};*)

// Delphi 5 can't handle enums like this :(
(*  TBodyFlags =
    (dxBodyFlagFiniteRotation = 1,		// use finite rotations
    dxBodyFlagFiniteRotationAxis = 2,	// use finite rotations only along axis
    dxBodyDisabled = 4,			          // body is disabled
    dxBodyNoGravity = 8);             // body is not influenced by gravity*)

(* Change: New Type added, syntax enforcement *)
  TBodyFlags = Integer;

(* These consts now have defined types *)
const
  dxBodyFlagFiniteRotation: TBodyFlags = 1;		  // use finite rotations
  dxBodyFlagFiniteRotationAxis: TBodyFlags = 2;	// use finite rotations only along axis
  dxBodyDisabled: TBodyFlags = 4;			          // body is disabled
  dxBodyNoGravity: TBodyFlags = 8;              // body is not influenced by gravity

(*typedef struct dMass {
  dReal mass;   // total mass of the rigid body
  dVector4 c;   // center of gravity position in body frame (x,y,z)
  dMatrix3 I;   // 3x3 inertia tensor in body frame, about POR
} dMass;*)

type
  TdMass = record
    mass : TdReal; // total mass of the rigid body
    c : TdVector4; // center of gravity position in body frame (x,y,z)
    I : TdMatrix3; // 3x3 inertia tensor in body frame, about POR
  end;
  PdMass = ^TdMass;

(*struct dBase {
  void *operator new (size_t size) { return dAlloc (size); }
  void operator delete (void *ptr, size_t size) { dFree (ptr,size); }
  void *operator new[] (size_t size) { return dAlloc (size); }
  void operator delete[] (void *ptr, size_t size) { dFree (ptr,size); }
};

struct dObject : public dBase {
  dxWorld *world;		// world this object is in
  dObject *next;		// next object of this type in list
  dObject **tome;		// pointer to previous object's next ptr
  void *userdata;		// user settable data
  int tag;			// used by dynamics algorithms
};*)


  PdxWorld = ^TdxWorld;

  PdObject = ^TdObject;
  PPdObject = ^PdObject;
  TdObject = record
    World     : PdxWorld;  // world this object is in
    next      : PdObject;	// next object of this type in list
    tome      : PPdObject;	// pointer to previous object's next ptr
    userdata  : pointer;		// user settable data
    tag       : integer;		// used by dynamics algorithms
  end;

(*struct dxBody : public dObject {
  dxJointNode *firstjoint;	// list of attached joints
  int flags;			  // some dxBodyFlagXXX flags
  dMass mass;			  // mass parameters about POR
  dMatrix3 invI;		// inverse of mass.I
  dReal invMass;		// 1 / mass.mass
  dVector3 pos;			// position of POR (point of reference)
  dQuaternion q;		// orientation quaternion
  dMatrix3 R;			  // rotation matrix, always corresponds to q
  dVector3 lvel,avel;		// linear and angular velocity of POR
  dVector3 facc,tacc;		// force and torque accululators
  dVector3 finite_rot_axis;	// finite rotation axis, unit length or 0=none
}; *)

  PdxBody = ^TdxBody;
  TdxBody = record
    BaseObject : TdObject;

    {$ifdef cSINGLE}
    Padding : byte;
    {$endif}

    firstjoint : TdJointID;	// list of attached joints
    flags : integer;			  // some dxBodyFlagXXX flags
    mass : TdMass;			    // mass parameters about POR
    invI : TdMatrix3 ;		  // inverse of mass.I
    invMass : TdReal;		    // 1 / mass.mass
    pos : TdVector3;			  // position of POR (point of reference)
    q : TdQuaternion;		    // orientation quaternion
    R : TdMatrix3;			    // rotation matrix, always corresponds to q
    lvel,avel : TdVector3;	// linear and angular velocity of POR
    facc,tacc : TdVector3 ;	// force and torque accululators
    finite_rot_axis : TdVector3 ;	// finite rotation axis, unit length or 0=none
  end;

  TBodyList = class(TList)
  private
    function GetItems(i: integer): PdxBody;
    procedure SetItems(i: integer; const Value: PdxBody);
  public
    property Items[i : integer] : PdxBody read GetItems write SetItems; default;
    procedure DeleteAllBodies;
  end;


(*struct dxWorld : public dBase {
  dxBody *firstbody;		// body linked list
  dxJoint *firstjoint;	// joint linked list
  int nb,nj;			      // number of bodies and joints in lists
  dVector3 gravity;		  // gravity vector (m/s/s)
  dReal global_erp;		  // global error reduction parameter
  dReal global_cfm;		  // global costraint force mixing parameter
};*)

  TdxWorld = record //(TdBase)
    firstbody : PdxBody;		// body linked list
    firstjoint : TdJointID;	// joint linked list
    nb,nj : integer;			  // number of bodies and joints in lists
    gravity : TdVector3;		// gravity vector (m/s/s)
    global_erp : TdReal;		// global error reduction parameter
    global_cfm : TdReal;		// global costraint force mixing parameter
  end;


(*  typedef struct dJointFeedback {
  dVector3 f1;       // force that joint applies to body 1
  dVector3 t1;       // torque that joint applies to body 1
  dVector3 f2;       // force that joint applies to body 2
  dVector3 t2;       // torque that joint applies to body 2
} dJointFeedback;)*)

  TdJointFeedback = record
    f1 : TdVector3;       // force that joint applies to body 1
    t1 : TdVector3;       // torque that joint applies to body 1
    f2 : TdVector3;       // force that joint applies to body 2
    t2 : TdVector3;       // torque that joint applies to body 2
  end;

  PTdJointFeedback = ^TdJointFeedback;

(*enum {
  d_ERR_UNKNOWN = 0,		/* unknown error */
  d_ERR_IASSERT,		/* internal assertion failed */
  d_ERR_UASSERT,		/* user assertion failed */
  d_ERR_LCP			/* user assertion failed */
};*)
  TdErrorType =
    (d_ERR_UNKNOWN,
     d_ERR_IASSERT,
     d_ERR_UASSERT,
     d_ERR_LCP { = 3//});


(*/* joint type numbers */

enum {
  dJointTypeNone = 0,		/* or "unknown" */
  dJointTypeBall,
  dJointTypeHinge,
  dJointTypeSlider,
  dJointTypeContact,
  dJointTypeHinge2,
  dJointTypeFixed,
  dJointTypeNull,
  dJointTypeAMotor
};*)

  TdJointTypeNumbers =
    (dJointTypeNone,		// or "unknown"
    dJointTypeBall,
    dJointTypeHinge,
    dJointTypeSlider,
    dJointTypeContact,
    dJointTypeHinge2,
    dJointTypeFixed,
    dJointTypeNull,
    dJointTypeAMotor,
    dJointTypePlane2D);

  TdAngularMotorModeNumbers =
    (dAMotorUser,
     dAMotorEuler);


// joint flags
(*enum {
  // if this flag is set, the joint was allocated in a joint group
  dJOINT_INGROUP = 1,

  // if this flag is set, the joint was attached with arguments (0,body).
  // our convention is to treat all attaches as (body,0), i.e. so node[0].body
  // is always nonzero, so this flag records the fact that the arguments were
  // swapped.
  dJOINT_REVERSE = 2,

  // if this flag is set, the joint can not have just one body attached to it,
  // it must have either zero or two bodies attached.
  dJOINT_TWOBODIES = 4
};*)
  //TJointFlag = (

(* Change: New Type added, syntax enforcement *)
  type
    TJointFlag = Integer;

(* These consts now have defined types *)
  const
    dJOINT_INGROUP: TJointFlag = 1;
    dJOINT_REVERSE: TJointFlag = 2;
    dJOINT_TWOBODIES: TJointFlag = 4;

  // Space constants
  const
    TYPE_SIMPLE = $bad;
    TYPE_HASH = $babe;


(*enum {
  dContactMu2		= 0x001,
  dContactFDir1		= 0x002,
  dContactBounce	= 0x004,
  dContactSoftERP	= 0x008,
  dContactSoftCFM	= 0x010,
  dContactMotion1	= 0x020,
  dContactMotion2	= 0x040,
  dContactSlip1		= 0x080,
  dContactSlip2		= 0x100,

  dContactApprox0	= 0x0000,
  dContactApprox1_1	= 0x1000,
  dContactApprox1_2	= 0x2000,
  dContactApprox1	= 0x3000
};*)
//  TdContactType = (
(* Change: New Type added, syntax enforcement *)
  type
    TdContactType = Integer;

(* These consts now have defined types *)
  const
    dContactMu2: TdContactType		    = $0001;
    dContactFDir1: TdContactType	  	= $0002;
    dContactBounce: TdContactType   	= $0004;
    dContactSoftERP: TdContactType  	= $0008;
    dContactSoftCFM: TdContactType  	= $0010;
    dContactMotion1: TdContactType  	= $0020;
    dContactMotion2: TdContactType	  = $0040;
    dContactSlip1: TdContactType		  = $0080;
    dContactSlip2: TdContactType		  = $0100;

    dContactApprox0: TdContactType	  = $00000;
    dContactApprox1_1: TdContactType	= $1000;
    dContactApprox1_2: TdContactType	= $2000;
    dContactApprox1: TdContactType	  = $3000;


(*  typedef struct dSurfaceParameters {
  /* must always be defined */
  int mode;
  dReal mu;

  /* only defined if the corresponding flag is set in mode */
  dReal mu2;
  dReal bounce;
  dReal bounce_vel;
  dReal soft_erp;
  dReal soft_cfm;
  dReal motion1,motion2;
  dReal slip1,slip2;
} dSurfaceParameters;*)

// enum { TRIMESH_FACE_NORMALS, TRIMESH_LAST_TRANSFORMATION };
const
  TRIMESH_FACE_NORMALS = 0;
  TRIMESH_LAST_TRANSFORMATION = 1;

type
  TdSurfaceParameters = record
    // must always be defined
    mode : cardinal;
    mu : TdReal;

    // only defined if the corresponding flag is set in mode
    mu2,
    bounce,
    bounce_vel,
    soft_erp,
    soft_cfm,
    motion1,motion2,
    slip1,slip2 : TdReal
  end;

(*typedef struct dContactGeom {
  dVector3 pos;
  dVector3 normal;
  dReal depth;
  dGeomID g1,g2;
} dContactGeom;*)

  PdxGeom = ^TdxGeom;

  TdContactGeom = record
    pos : TdVector3;
    normal : TdVector3;
    depth : TdReal;
    g1,g2 : PdxGeom;
  end;

  PdContactGeom = ^TdContactGeom;

  (*struct dContact {
  dSurfaceParameters surface;
  dContactGeom geom;
  dVector3 fdir1;
};*)
  TdContact = record
    surface : TdSurfaceParameters;
    geom : TdContactGeom;
    fdir1 : TdVector3;
  end;
  PdContact = ^TdContact;

  // Collission callback structure
  TdNearCallback = procedure(data : pointer; o1, o2 : PdxGeom); cdecl;

  TdColliderFn = function(o1, o2 : PdxGeom; flags : Integer;
                  contact : PdContactGeom; skip : Integer) : Integer; cdecl;
  TdGetColliderFnFn = function(num : Integer) : TdColliderFn; cdecl;
  TdGetAABBFn = procedure(g : PdxGeom; var aabb : TdAABB); cdecl;
  TdGeomDtorFn = procedure(o : PdxGeom); cdecl;
  TdAABBTestFn = function(o1, o2 : PdxGeom; const aabb2 : TdAABB) : Integer; cdecl;

(*typedef struct dGeomClass {
  int bytes;
  dGetColliderFnFn *collider;
  dGetAABBFn *aabb;
  dAABBTestFn *aabb_test;
  dGeomDtorFn *dtor;
} dGeomClass;*)

  TdGeomClass = record
    bytes : integer;                 // extra storage size
    collider : TdGetColliderFnFn;    // collider function
    aabb : TdGetAABBFn;       // bounding box function
    aabb_test : TdAABBTestFn; // aabb tester, can be 0 for none
    dtor : TdGeomDtorFn;      // destructor, can be 0 for none
  end;

  PdGeomClass = ^TdGeomClass;

(*struct dxGeomClass {
 dGetColliderFnFn *collider;
  dGetAABBFn *aabb;
  dAABBTestFn *aabb_test;
  dGeomDtorFn *dtor;
  int num;             // class number
  int size;            // total size of object, including extra data area
};*)

  TdxGeomClass = record
    collider : TdGetColliderFnFn;    // collider function
    aabb : TdGetAABBFn;              // bounding box function
    aabb_test : TdAABBTestFn;        // aabb tester, can be 0 for none
    dtor  : TdGeomDtorFn;             // destructor, can be 0 for none
    num   : integer;             // class number
    size  : integer;            // total size of object, including extra data area
  end;

  PdxGeomClass = ^TdxGeomClass;

  (*// position vector and rotation matrix for geometry objects that are not
  // connected to bodies.

  struct dxPosR {
    dVector3 pos;
    dMatrix3 R;
  };*)

  TdxPosR = record
    pos : TdVector3;
    R : TdMatrix3;
  end;

  (*struct dxSpace : public dBase {
  int type;			// don't want to use RTTI
  virtual void destroy()=0;
  virtual void add (dGeomID)=0;
  virtual void remove (dGeomID)=0;
  virtual void collide (void *data, dNearCallback *callback)=0;
  virtual int query (dGeomID)=0;
};*)
  TdxSpace = record
    _Garbage : integer;
// * Explaining _Garbage *
// That member first appears in the TdxSpace record.  This is
// because the dxSpace struct declares its functions as virtual.  Since the
// class (a struct in C++ is considered a public class) now contains virtual
// functions (functions in dxBase are not virtual), it needs a vtable to know
// what functions to call in what class.  Visual C++ places the pointer to the
// vtable at offset 0, which is what your _Garbage field caters for.
//
// Steve 'Sly' Williams

    SpaceType : integer;
  end;

  PdxSpace = ^TdxSpace;

  TdRealHugeArray = array[0..65535] of TdReal;
  PdRealHugeArray = ^TdRealHugeArray;

  // Tri-list collider
  TdIntegerArray = array[0..65535] of Integer;
  PdIntegerArray = ^TdIntegerArray;

  TdVector3Array = array[0..65535] of TdVector3;
  PdVector3Array = ^TdVector3Array;

(*struct dxTriMeshData{
	Model BVTree;
	MeshInterface Mesh;

    dxTriMeshData();
    ~dxTriMeshData();
    
    void Build(const void* Vertices, int VertexStide, int VertexCount, 
	       const void* Indices, int IndexCount, int TriStride, 
	       const void* Normals, 
	       bool Single);

        /* aabb in model space */
        dVector3 AABBCenter;
        dVector3 AABBExtents;

    /* data for use in collison resolution */
    const void* Normals;
    Matrix4x4   last_trans;
};*)
  TdxTriMeshData = record
    unknown : byte; //
  end;

  PdxTriMeshData = ^TdxTriMeshData;

(*//simple space - reports all n^2 object intersections
struct dxSimpleSpace : public dxSpace {
  dGeomID first;
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxSimpleSpace = record
    _Garbage : integer; // I don't know where this comes from!
    SpaceType : integer;
    First : PdxGeom;
  end;

  PdxSimpleSpace = ^TdxSimpleSpace;

(*//currently the space 'container' is just a list of the geoms in the space.
struct dxHashSpace : public dxSpace {
  dxGeom *first;
  int global_minlevel;	// smallest hash table level to put AABBs in
  int global_maxlevel;	// objects that need a level larger than this will be
			// put in a "big objects" list instead of a hash table
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxHashSpace = record
    _Garbage : integer; // I don't know where this comes from!
    SpaceType : integer;
    First : PdxGeom;
    global_minlevel : integer;
    global_maxlevel : integer;
  end;

  PdxHashSpace = ^TdxHashSpace;

  (*typedef struct dGeomSpaceData {
  dGeomID next;
} dGeomSpaceData; *)

  TdGeomSpaceData = record
    next : PdxGeom;
  end;

  (*// common data for all geometry objects. the class-specific data area follows
  // this structure. pos and R will either point to a separately allocated
  // buffer (if body is 0 - pos points to the dxPosR object) or to the pos and
  // R of the body (if body nonzero).
  struct dxGeom {		// a dGeomID is a pointer to this
    dxGeomClass *_class;	// class of this object
    void *data;		// user data pointer
    dBodyID body;		// dynamics body associated with this object (if any)
    dReal *pos;		// pointer to object's position vector
    dReal *R;		// pointer to object's rotation matrix
    dSpaceID spaceid;	// the space this object is in
    dGeomSpaceData space;	// reserved for use by space this object is in
    dReal *space_aabb;	// ptr to aabb array held by dSpaceCollide() fn
    // class-specific data follows here, with proper alignment.
  };*)

  TdxGeom = record // a dGeomID is a pointer to this
    _class : PdxGeomClass;	// class of this object

    {$ifdef cSINGLE}
    Padding : array [0..4] of byte;
    {$else}
    Padding : array [0..2] of pointer;
    {$endif}

    data : pointer;		// user data pointer
    Body : PdxBody ;		// dynamics body associated with this object (if any)
    Pos : PdVector3;		// pointer to object's position vector
    R : PdMatrix3;		// pointer to object's rotation matrix
    spaceid : PdxSpace;	// the space this object is in
    space : TdGeomSpaceData ;	// reserved for use by space this object is in
    space_aabb : PdReal;	// ptr to aabb array held by dSpaceCollide() fn
    // class-specific data follows here, with proper alignment.
  end;

  TGeomList = class(TList)
  private
    function GetItems(i: integer): PdxGeom;
    procedure SetItems(i: integer; const Value: PdxGeom);
  public
    property Items[i : integer] : PdxGeom read GetItems write SetItems; default;
    procedure DeleteAllGeoms(DeleteDataAsObject : boolean=false);
  end;

(*
/* standard joint parameter names. why are these here? - because we don't want
 * to include all the joint function definitions in joint.cpp. hmmmm.
 * MSVC complains if we call D_ALL_PARAM_NAMES_X with a blank second argument,
 * which is why we have the D_ALL_PARAM_NAMES macro as well. please copy and
 * paste between these two.
 */

#define D_ALL_PARAM_NAMES(start) \
  /* parameters for limits and motors */ \
  dParamLoStop = start, \
  dParamHiStop, \
  dParamVel, \
  dParamFMax, \
  dParamFudgeFactor, \
  dParamBounce, \
  dParamCFM, \
  dParamStopERP, \
  dParamStopCFM, \
  /* parameters for suspension */ \
  dParamSuspensionERP, \
  dParamSuspensionCFM,

#define D_ALL_PARAM_NAMES_X(start,x) \
  /* parameters for limits and motors */ \
  dParamLoStop ## x = start, \
  dParamHiStop ## x, \
  dParamVel ## x, \
  dParamFMax ## x, \
  dParamFudgeFactor ## x, \
  dParamBounce ## x, \
  dParamCFM ## x, \
  dParamStopERP ## x, \
  dParamStopCFM ## x, \
  /* parameters for suspension */ \
  dParamSuspensionERP ## x, \
  dParamSuspensionCFM ## x,

enum {
  D_ALL_PARAM_NAMES(0)
  D_ALL_PARAM_NAMES_X(0x100,2)
  D_ALL_PARAM_NAMES_X(0x200,3)

  /* add a multiple of this constant to the basic parameter numbers to get
   * the parameters for the second, third etc axes.
   */
  dParamGroup=0x100
};*)

//  TJointParams = (
    // parameters for limits and motors
(* Change: New Type added, sintax enforcement *)
  type
    TJointParams = Integer;

(* These consts now have defined types *)
  const
    _priv_dParamLoStop  = 0;
    _priv_dParamLoStop2 = $100;
    _priv_dParamLoStop3 = $200;
  const
    dParamLoStop: TJointParams = _priv_dParamLoStop;
    dParamHiStop: TJointParams = _priv_dParamLoStop + 1;
    dParamVel: TJointParams = _priv_dParamLoStop + 2;
    dParamFMax: TJointParams = _priv_dParamLoStop + 3;
    dParamFudgeFactor: TJointParams = _priv_dParamLoStop + 4;
    dParamBounce: TJointParams = _priv_dParamLoStop + 5;
    dParamCFM: TJointParams = _priv_dParamLoStop + 6;
    dParamStopERP: TJointParams = _priv_dParamLoStop + 7;
    dParamStopCFM: TJointParams = _priv_dParamLoStop + 8;
    // parameters for suspension
    dParamSuspensionERP: TJointParams = _priv_dParamLoStop + 9;
    dParamSuspensionCFM: TJointParams = _priv_dParamLoStop + 10;

    // SECOND AXEL
    // parameters for limits and motors
    dParamLoStop2: TJointParams = _priv_dParamLoStop2;
    dParamHiStop2: TJointParams = _priv_dParamLoStop2 + 1;
    dParamVel2: TJointParams = _priv_dParamLoStop2 + 2;
    dParamFMax2: TJointParams = _priv_dParamLoStop2 + 3;
    dParamFudgeFactor2: TJointParams = _priv_dParamLoStop2 + 4;
    dParamBounce2: TJointParams = _priv_dParamLoStop2 + 5;
    dParamCFM2: TJointParams = _priv_dParamLoStop2 + 6;
    dParamStopERP2: TJointParams = _priv_dParamLoStop2 + 7;
    dParamStopCFM2: TJointParams = _priv_dParamLoStop2 + 8;
    // parameters for suspension
    dParamSuspensionERP2: TJointParams = _priv_dParamLoStop2 + 9;
    dParamSuspensionCFM2: TJointParams = _priv_dParamLoStop2 + 10;

    // THIRD AXEL
    // parameters for limits and motors
    dParamLoStop3: TJointParams = _priv_dParamLoStop3;
    dParamHiStop3: TJointParams = _priv_dParamLoStop3 + 1;
    dParamVel3: TJointParams = _priv_dParamLoStop3 + 2;
    dParamFMax3: TJointParams = _priv_dParamLoStop3 + 3;
    dParamFudgeFactor3: TJointParams = _priv_dParamLoStop3 + 4;
    dParamBounce3: TJointParams = _priv_dParamLoStop3 + 5;
    dParamCFM3: TJointParams = _priv_dParamLoStop3 + 6;
    dParamStopERP3: TJointParams = _priv_dParamLoStop3 + 7;
    dParamStopCFM3: TJointParams = _priv_dParamLoStop3 + 8;
    // parameters for suspension
    dParamSuspensionERP3: TJointParams = _priv_dParamLoStop3 + 9;
    dParamSuspensionCFM3: TJointParams = _priv_dParamLoStop3 + 10;

    // AGAIN!
    // dParamGroup * 2 + dParamBounce = dParamBounce2
    dParamGroup: TJointParams = $100;

{ TODO :
// How does one import integers?
dBoxClass
dCCylinderClass
dGeomTransformClass
dSphereClass
dPlaneClass
...

// Functions
dBoxBox
dDebug
dError
dMessage
dFactorCholesky
dFactorLDLT
dPlaneSpace
dInvertPDMatrix
dIsPositiveDefinite
dLDLTAddTL
dLDLTRemove
dRemoveRowCol
dSetDebugHandler
dSetErrorHandler
dSetMessageHandler
dSetZero
dSolveCholesky
dSolveLDLT}

  // ***************************************************************************
  // ***************************************************************************
  // ***** WORLD

  //----- dWorld -----

var
  dWorldCreate: function: PdxWorld; cdecl;
  dWorldDestroy: procedure(const World: PdxWorld); cdecl;
  dWorldGetCFM: function(const World: PdxWorld):  TdReal; cdecl;
  dWorldGetERP: function(const World: PdxWorld):  TdReal; cdecl;
  dWorldGetGravity: procedure(const World: PdxWorld; var gravity: TdVector3); cdecl;
  dWorldImpulseToForce: procedure(const World: PdxWorld; const stepsize, ix, iy, iz: TdReal; var force: TdVector3); cdecl;
  dWorldSetCFM: procedure(const World: PdxWorld; cfm: TdReal); cdecl;
  dWorldSetERP: procedure(const World: PdxWorld; erp: TdReal); cdecl;
  dWorldSetGravity: procedure(const World: PdxWorld; const x, y, z: TdReal); cdecl;
  dWorldSetContactMaxCorrectingVel: procedure(const World: PdxWorld; const vel: TdReal); cdecl;
  dWorldGetContactMaxCorrectingVel: function(const World: PdxWorld):  TdReal; cdecl;
  dWorldSetContactSurfaceLayer: procedure(const World: PdxWorld; const depth: TdReal); cdecl;
  dWorldGetContactSurfaceLayer: function(const World: PdxWorld):  TdReal; cdecl;

  // Step
  dWorldStep: procedure(const World: PdxWorld; const stepsize: TdReal); cdecl;
  // QuickStep
  dWorldQuickStep: procedure(const World: PdxWorld; const stepsize: TdReal); cdecl;
  dWorldSetQuickStepNumIterations: procedure(const World: PdxWorld; const num: integer); cdecl;
  dWorldGetQuickStepNumIterations: function(const World: PdxWorld):  integer; cdecl;
  dWorldSetQuickStepW: procedure(const World: PdxWorld; const param: TdReal); cdecl;
  dWorldGetQuickStepW: function(const World: PdxWorld):  TdReal; cdecl;
  // Stepfast
  dWorldStepFast1: procedure(const World: PdxWorld; const stepsize: TdReal; const iterations: Integer); cdecl;
  dWorldSetAutoEnableDepthSF1: procedure(const World: PdxWorld; autodepth: Integer); cdecl;
  dWorldGetAutoEnableDepthSF1: function(const World: PdxWorld):  Integer; cdecl;
  dCloseODE: procedure; cdecl;

  //----- dBody -----
  dBodyAddForce: procedure(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl;
  dBodyAddForceAtPos: procedure(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl;
  dBodyAddForceAtRelPos: procedure(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl;
  dBodyAddRelForce: procedure(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl;
  dBodyAddRelForceAtPos: procedure(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl;
  dBodyAddRelForceAtRelPos: procedure(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl;
  dBodyAddRelTorque: procedure(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl;
  dBodyAddTorque: procedure(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl;

  dBodyCreate: function(const World: PdxWorld):  PdxBody; cdecl;
  dBodyDestroy: procedure(const Body: PdxBody); cdecl;
  dBodyDisable: procedure(const Body: PdxBody); cdecl;
  dBodyEnable: procedure(const Body: PdxBody); cdecl;
  dBodyGetAngularVel: function(const Body: PdxBody):  PdVector3; cdecl;
  dBodyGetFiniteRotationAxis: procedure(const Body: PdxBody; var result: TdVector3); cdecl;
  dBodyGetFiniteRotationMode: function(const Body: PdxBody):  Integer; cdecl;
  dBodyGetForce: function(const Body: PdxBody):  PdVector3; cdecl;
  dBodyGetGravityMode: function(const Body: PdxBody):  Integer; cdecl;
  dBodyGetJoint: function(const Body: PdxBody; const index: Integer):  TdJointID; cdecl;
  dBodyGetLinearVel: function(const Body: PdxBody):  PdVector3; cdecl;
  dBodyGetMass: procedure(const Body: PdxBody; var mass: TdMass); cdecl;
  dBodyGetNumJoints: function(const Body: PdxBody):  Integer; cdecl;
  dBodyGetPointVel: procedure(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl;
  dBodyGetPosRelPoint: procedure(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl;
  dBodyGetPosition: function(const Body: PdxBody):  PdVector3; cdecl;
  dBodyGetQuaternion: function(const Body: PdxBody):  PdQuaternion; cdecl;
  dBodyGetRelPointPos: procedure(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl;
  dBodyGetRelPointVel: procedure(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl;
  dBodyGetRotation: function(const Body: PdxBody):  PdMatrix3; cdecl;
  dBodyGetTorque: function(const Body: PdxBody):  PdVector3; cdecl;
  dBodyIsEnabled: function(const Body: PdxBody):  Integer; cdecl;
  dBodySetAngularVel: procedure(const Body: PdxBody; const x, y, z: TdReal); cdecl;
  dBodySetFiniteRotationAxis: procedure(const Body: PdxBody; const x, y, z: TdReal); cdecl;
  dBodySetFiniteRotationMode: procedure(const Body: PdxBody; const mode: Integer); cdecl;
  dBodySetForce: procedure(const Body: PdxBody; const x, y, z: TdReal); cdecl;
  dBodySetGravityMode: procedure(const Body: PdxBody; const mode: Integer); cdecl;
  dBodySetLinearVel: procedure(const Body: PdxBody; const x, y, z: TdReal); cdecl;
  dBodySetMass: procedure(const Body: PdxBody; const mass: PdMass); cdecl;
  dBodySetPosition: procedure(const Body: PdxBody; const x, y, z: TdReal); cdecl;
  dBodySetQuaternion: procedure(const Body: PdxBody; const q: TdQuaternion); cdecl;
  dBodySetRotation: procedure(const Body: PdxBody; const R: TdMatrix3); cdecl;
  dBodySetTorque: procedure(const Body: PdxBody; const x, y, z: TdReal); cdecl;
  dBodyVectorFromWorld: procedure(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl;
  dBodyVectorToWorld: procedure(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl;
  dBodySetData: procedure(const Body: PdxBody; data : pointer); cdecl;
  dBodyGetData: function(const Body: PdxBody):  pointer; cdecl;

  //----- dJoint -----
  dJointAttach: procedure(const dJointID : TdJointID; const body1, body2: PdxBody); cdecl;
  dJointCreateAMotor: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointCreateBall: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointCreateContact: function(const World : PdxWorld; dJointGroupID : TdJointGroupID; const dContact: PdContact):  TdJointID; cdecl;

  dJointCreateFixed: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointCreateHinge: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointCreateHinge2: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointCreateSlider: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointCreateUniversal: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointCreatePlane2D: function(const World : PdxWorld; dJointGroupID : TdJointGroupID):  TdJointID; cdecl;
  dJointDestroy: procedure(const dJointID : TdJointID); cdecl;
  dJointGetAMotorAngle: function(const dJointID : TdJointID; const anum: Integer):  TdReal; cdecl;
  dJointGetAMotorAngleRate: function(const dJointID : TdJointID; const anum: Integer):  TdReal; cdecl;
  dJointGetAMotorAxis: procedure(const dJointID : TdJointID; const anum: Integer; var result: TdVector3); cdecl;
  dJointGetAMotorAxisRel: function(const dJointID : TdJointID; const anum: Integer):  Integer; cdecl;
  dJointGetAMotorMode: function(const dJointID : TdJointID):  Integer; cdecl;
  dJointGetAMotorNumAxes: function(const dJointID : TdJointID):  Integer; cdecl;
  dJointGetAMotorParam: function(const dJointID : TdJointID; const parameter: TJointParams):  TdReal; cdecl;
  dJointGetBallAnchor: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetBallAnchor2: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetBody: function(const dJointID : TdJointID; const index: Integer):  PdxBody; cdecl;
  dJointGetHinge2Anchor: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetHinge2Anchor2: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetHinge2Angle1: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetHinge2Angle1Rate: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetHinge2Angle2Rate: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetHinge2Axis1: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetHinge2Axis2: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetHinge2Param: function(const dJointID : TdJointID; const parameter: TJointParams):  TdReal; cdecl;
  dJointGetHingeAnchor: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetHingeAnchor2: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetHingeAngle: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetHingeAngleRate: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetHingeAxis: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetHingeParam: function(const dJointID : TdJointID; const parameter: TJointParams):  TdReal; cdecl;
  dJointGetSliderAxis: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetSliderParam: function(const dJointID : TdJointID; const parameter: TJointParams):  TdReal; cdecl;
  dJointGetSliderPosition: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetSliderPositionRate: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetType: function(const dJointID : TdJointID):  Integer; cdecl;
  dJointGetUniversalAnchor: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetUniversalAnchor2: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetUniversalAxis1: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetUniversalAxis2: procedure(const dJointID : TdJointID; var result: TdVector3); cdecl;
  dJointGetUniversalParam: function(const dJointID : TdJointID; const parameter: TJointParams):  TdReal; cdecl;
  dJointGetUniversalAngle1: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetUniversalAngle2: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetUniversalAngle1Rate: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGetUniversalAngle2Rate: function(const dJointID : TdJointID):  TdReal; cdecl;
  dJointGroupCreate: function(const max_size: Integer):  TdJointGroupID; cdecl;
  dJointGroupDestroy: procedure(const dJointGroupID : TdJointGroupID); cdecl;
  dJointGroupEmpty: procedure(const dJointGroupID : TdJointGroupID); cdecl;
  dJointSetAMotorAngle: procedure(const dJointID : TdJointID; const anum: Integer; const angle: TdReal); cdecl;
  dJointSetAMotorAxis: procedure(const dJointID : TdJointID; const anum, rel: Integer; const x, y, z: TdReal); cdecl;
  dJointSetAMotorMode: procedure(const dJointID : TdJointID; const mode: TdAngularMotorModeNumbers); cdecl;
  dJointSetAMotorNumAxes: procedure(const dJointID : TdJointID; const num: Integer); cdecl;
  dJointSetAMotorParam: procedure(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl;
  dJointSetBallAnchor: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetFixed: procedure(const dJointID : TdJointID); cdecl;
  dJointSetHinge2Anchor: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetHinge2Axis1: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetHinge2Axis2: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetHinge2Param: procedure(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl;
  dJointSetHingeAnchor: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetHingeAxis: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetHingeParam: procedure(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl;
  dJointSetSliderAxis: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetSliderParam: procedure(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl;
  dJointSetUniversalAnchor: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetUniversalAxis1: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetUniversalAxis2: procedure(const dJointID : TdJointID; const x, y, z: TdReal); cdecl;
  dJointSetUniversalParam: procedure(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl;
  dJointSetPlane2DXParam: procedure(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl;
  dJointSetPlane2DYParam: procedure(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl;
  dJointSetPlane2DAngleParam: procedure(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl;
  dJointGetData: function(const dJointID : TdJointID):  pointer; cdecl;
  dJointSetData: procedure(const dJointID : TdJointID; data : Pointer); cdecl;
  dJointAddAMotorTorques: procedure(const dJointID : TdJointID; torque1, torque2, torque3: TdReal); cdecl;
  dJointAddHinge2Torques: procedure(const dJointID : TdJointID; torque1, torque2: TdReal); cdecl;
  dJointAddHingeTorque: procedure(const dJointID : TdJointID; torque: TdReal); cdecl;
  dJointAddSliderForce: procedure(const dJointID : TdJointID; force: TdReal); cdecl;
  dJointAddUniversalTorques: procedure(const dJointID : TdJointID; torque1, torque2: TdReal); cdecl;

  // callback routines for feedback of joints
  dJointSetFeedback: procedure(const dJointID : TdJointID; Feedback : PTdJointFeedback); cdecl;
  dJointGetFeedback: function(const dJointID : TdJointID):  PTdJointFeedback; cdecl;
  dJointCorrectHinge2: procedure(const dJointID : TdJointID); cdecl;

  //* Auto-disable functions */
  dWorldGetAutoDisableLinearThreshold: function(const World: PdxWorld):  TdReal; cdecl;
  dWorldSetAutoDisableLinearThreshold: procedure(const World: PdxWorld; linThreshold: TdReal); cdecl;
  dWorldGetAutoDisableAngularThreshold: function(const World: PdxWorld):  TdReal; cdecl;
  dWorldSetAutoDisableAngularThreshold: procedure(const World: PdxWorld; angThreshold: TdReal); cdecl;
  dWorldGetAutoDisableSteps: function(const World: PdxWorld):  Integer; cdecl;
  dWorldSetAutoDisableSteps: procedure(const World: PdxWorld; steps: Integer); cdecl;
  dWorldGetAutoDisableTime: function(const World: PdxWorld):  TdReal; cdecl;
  dWorldSetAutoDisableTime: procedure(const World: PdxWorld; time: TdReal); cdecl;
  dWorldGetAutoDisableFlag: function(const World: PdxWorld):  Integer; cdecl;
  dWorldSetAutoDisableFlag: procedure(const World: PdxWorld; do_auto_disable: Integer); cdecl;

  dBodyGetAutoDisableLinearThreshold: function(const Body: PdxBody):  TdReal; cdecl;
  dBodySetAutoDisableLinearThreshold: procedure(const Body: PdxBody; linThreshold: TdReal); cdecl;
  dBodyGetAutoDisableAngularThreshold: function(const Body: PdxBody):  TdReal; cdecl;
  dBodySetAutoDisableAngularThreshold: procedure(const Body: PdxBody; angThreshold: TdReal); cdecl;
  dBodyGetAutoDisableSteps: function(const Body: PdxBody):  Integer; cdecl;
  dBodySetAutoDisableSteps: procedure(const Body: PdxBody; steps: Integer); cdecl;
  dBodyGetAutoDisableTime: function(const Body: PdxBody):  TdReal; cdecl;
  dBodySetAutoDisableTime: procedure(const Body: PdxBody; time: TdReal); cdecl;
  dBodyGetAutoDisableFlag: function(const Body: PdxBody):  Integer; cdecl;
  dBodySetAutoDisableFlag: procedure(const Body: PdxBody; do_auto_disable: Integer); cdecl;
  dBodySetAutoDisableDefaults: procedure(const Body: PdxBody); cdecl;

  //----- dGeom -----
  dGeomBoxGetLengths: procedure(const Geom : PdxGeom; var result: TdVector3); cdecl;
  dGeomBoxSetLengths: procedure(const Geom : PdxGeom; const lx, ly, lz: TdReal); cdecl;
  dGeomCCylinderGetParams: procedure(const Geom : PdxGeom; var radius, length: TdReal); cdecl;
  dGeomCCylinderSetParams: procedure(const Geom : PdxGeom; const radius, length: TdReal); cdecl;
  dGeomDestroy: procedure(const Geom : PdxGeom); cdecl;
  dGeomGetAABB: procedure(const Geom : PdxGeom; var aabb: TdAABB); cdecl;
  dGeomGetBody: function(const Geom : PdxGeom):  PdxBody; cdecl;
  dGeomGetClass: function(const Geom : PdxGeom):  Integer; cdecl;
  dGeomGetPosition: function(const Geom : PdxGeom):  PdVector3; cdecl;
  dGeomGetRotation: function(const Geom : PdxGeom):  PdMatrix3; cdecl;
  dGeomGetQuaternion: procedure(const Geom : PdxGeom; var result: TdQuaternion); cdecl;
  dGeomGetSpace: function(const Geom : PdxGeom):  PdxSpace; cdecl;

  dGeomPlaneGetParams: procedure(const Geom : PdxGeom; var result: TdVector4); cdecl;
  dGeomPlaneSetParams: procedure(const Geom : PdxGeom; const a, b, c, d: TdReal); cdecl;
  dGeomSetBody: procedure(const Geom : PdxGeom; Body: PdxBody); cdecl;
  dGeomSetPosition: procedure(const Geom : PdxGeom; const x, y, z: TdReal); cdecl;
  dGeomSetRotation: procedure(const Geom : PdxGeom; const R: TdMatrix3); cdecl;
  dGeomSetQuaternion: procedure(const Geom : PdxGeom; const TdQuaternion); cdecl;

  dGeomSphereGetRadius: function(const Geom : PdxGeom):  TdReal; cdecl;
  dGeomSphereSetRadius: procedure(const Geom : PdxGeom; const radius: TdReal); cdecl;
  dGeomTransformGetCleanup: function(const Geom : PdxGeom):  Integer; cdecl;
  dGeomTransformGetGeom: function(const Geom : PdxGeom):  PdxGeom; cdecl;
  dGeomTransformSetCleanup: procedure(const Geom : PdxGeom; const mode: Integer); cdecl;
  dGeomTransformSetGeom: procedure(const Geom, obj: PdxGeom); cdecl;
  dGeomSetData: procedure(const Geom : PdxGeom; data : pointer); cdecl;
  dGeomGetData: function(const Geom : PdxGeom):  pointer; cdecl;
  dGeomTransformSetInfo: procedure(const Geom : PdxGeom; mode : integer); cdecl;
  dGeomTransformGetInfo: function(const Geom : PdxGeom):  integer; cdecl;

  dGeomIsSpace: function(const Geom : PdxGeom):  integer; cdecl;
  dGeomSetCategoryBits: procedure(const Geom : PdxGeom; bits : Cardinal); cdecl;
  dGeomSetCollideBits: procedure(const Geom : PdxGeom; bits : Cardinal); cdecl;
  dGeomGetCategoryBits: function(const Geom : PdxGeom):  cardinal; cdecl;
  dGeomGetCollideBits: function(const Geom : PdxGeom):  cardinal; cdecl;
  dGeomEnable: procedure(const Geom : PdxGeom); cdecl;
  dGeomDisable: procedure(const Geom : PdxGeom); cdecl;
  dGeomIsEnabled: function(const Geom : PdxGeom):  integer; cdecl;

  dGeomSpherePointDepth: function(const Geom : PdxGeom; const x,y,z : TdReal):  TdReal; cdecl;
  dGeomBoxPointDepth: function(const Geom : PdxGeom; const x,y,z : TdReal):  TdReal; cdecl;
  dGeomPlanePointDepth: function(const Geom : PdxGeom; const x,y,z : TdReal):  TdReal; cdecl;
  dGeomCCylinderPointDepth: function(const Geom : PdxGeom; const x,y,z : TdReal):  TdReal; cdecl;

  // A strange fix, so the class ids can be updated
  // ***************
  function dCreateSphere(const Space : PdxSpace; const radius: TdReal): PdxGeom; cdecl;
  function dCreateBox(const Space : PdxSpace; const lx, ly, lz: TdReal): PdxGeom; cdecl;
  function dCreatePlane(const Space : PdxSpace; const a, b, c, d: TdReal): PdxGeom; cdecl;
  function dCreateCCylinder(const Space : PdxSpace; const radius, length: TdReal): PdxGeom; cdecl;
  function dCreateCylinder(const Space : PdxSpace; r, lz : TdReal) : PdxGeom; cdecl;
  function dCreateCone(const Space : PdxSpace; radius, length: TdReal): PdxGeom; cdecl;
  function dCreateTerrainY(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer; bFinite, bPlaceable: Integer): PdxGeom; cdecl;
  function dCreateTerrainZ(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer; bFinite, bPlaceable: Integer): PdxGeom; cdecl;
  function dCreateRay(const Space : PdxSpace; length: TdReal) : PdxGeom; cdecl;
  function dCreateGeomTransform(const Space : PdxSpace): PdxGeom; cdecl;
  function dCreateTriMesh(const Space : PdxSpace; Data: PdxTriMeshData; Callback, ArrayCallback, RayCallback: Pointer): PdxGeom; cdecl;


var
  EXT_dCreateSphere: function(const Space : PdxSpace; const radius: TdReal):  PdxGeom; cdecl;
  EXT_dCreateBox: function(const Space : PdxSpace; const lx, ly, lz: TdReal):  PdxGeom; cdecl;
  EXT_dCreatePlane: function(const Space : PdxSpace; const a, b, c, d: TdReal):  PdxGeom; cdecl;
  EXT_dCreateCCylinder: function(const Space : PdxSpace; const radius, length: TdReal):  PdxGeom; cdecl;
  EXT_dCreateCylinder: function(const Space : PdxSpace; r, lz : TdReal):  PdxGeom; cdecl;
  EXT_dCreateCone: function(const Space : PdxSpace; radius, length: TdReal):  PdxGeom; cdecl;
  EXT_dCreateTerrainY: function(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer; bFinite, bPlaceable: Integer):  PdxGeom; cdecl;
  EXT_dCreateTerrainZ: function(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer; bFinite, bPlaceable: Integer):  PdxGeom; cdecl;
  EXT_dCreateRay: function(const Space : PdxSpace; length : TdReal):  PdxGeom; cdecl;
  EXT_dCreateGeomTransform: function(const Space : PdxSpace):  PdxGeom; cdecl;
  EXT_dCreateTriMesh: function(const Space : PdxSpace; Data: PdxTriMeshData; Callback, ArrayCallback, RayCallback: Pointer):  PdxGeom; cdecl;
  // ***************

  // dCone
  dGeomConeSetParams: procedure(const Geom: PdxGeom; radius, length: TdReal); cdecl;
  dGeomConeGetParams: procedure(const Geom: PdxGeom; var radius, length: TdReal); cdecl;
  dGeomConePointDepth: function(const Geom: PdxGeom; const x, y, z: TdReal):  TdReal; cdecl;

  // dTerrain
  dGeomTerrainYPointDepth: function(const Geom: PdxGeom; const x, y, z: TdReal):  TdReal; cdecl;
  dGeomTerrainZPointDepth: function(const Geom: PdxGeom; const x, y, z: TdReal):  TdReal; cdecl;

  // dCylinder (not a capped cylinder).
  dGeomCylinderSetParams: procedure(const Geom : PdxGeom; radius, length : TdReal); cdecl;
  dGeomCylinderGetParams: procedure(const Geom : PdxGeom; var radius, length : TdReal); cdecl;

  //dRay
  dGeomRaySetLength: procedure(const Geom : PdxGeom; length: TdReal); cdecl;
  dGeomRayGetLength: function(const Geom : PdxGeom):  TdReal; cdecl;
  dGeomRaySetClosestHit: procedure(const Geom : PdxGeom; closestHit: Integer); cdecl;
  dGeomRayGetClosestHit: function(const Geom : PdxGeom):  Integer; cdecl;
  dGeomRaySet: procedure(const Geom : PdxGeom; px, py, pz, dx, dy, dz: TdReal); cdecl;
  dGeomRayGet: procedure(const Geom : PdxGeom; var start, dir: TdVector3); cdecl;

  dCreateGeomClass: function(const classptr : TdGeomClass):  Integer; cdecl;
  dGeomGetClassData: function(o : PdxGeom):  Pointer; cdecl;
  dCreateGeom: function(classnum : Integer):  PdxGeom; cdecl;

  //----- dTrilistCollider -----
  dGeomTriMeshDataBuildSimple: procedure(g: PdxTriMeshData; Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray; IndexCount: Integer); cdecl;
  dGeomTriMeshDataBuildSimple1: procedure(g: PdxTriMeshData; Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray; IndexCount: Integer; Normals: PdVector3Array); cdecl;
  dGeomTriMeshDataBuildDouble: procedure(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl;
  dGeomTriMeshDataBuildDouble1: procedure(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer; Normals: PdVector3Array); cdecl;
  dGeomTriMeshDataBuildSingle: procedure(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl;
  dGeomTriMeshDataBuildSingle1: procedure(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer; Normals: PdVector3Array); cdecl;
  dGeomTriMeshDataCreate: function: PdxTriMeshData; cdecl;
  dGeomTriMeshDataSet: procedure(g: PdxTriMeshData; data_id: Integer; data: Pointer); cdecl;
  dGeomTriMeshDataDestroy: procedure(g: PdxTriMeshData); cdecl;
  dGeomTriMeshGetTriangle: procedure(g: PdxGeom; Index: Integer; v0, v1, v2: PdVector3); cdecl;
  dGeomTriMeshGetPoint: procedure(g: PdxGeom; Index: Integer; u, v: TdReal; result: TdVector3); cdecl;
  dGeomTriMeshClearTCCache: procedure(g: PdxGeom); cdecl;
  dGeomTriMeshEnableTC: procedure(g: PdxGeom; geomClass, enable: Integer); cdecl;
  dGeomTriMeshIsTCEnabled: function(g: PdxGeom; geomClass: Integer):  Integer; cdecl;
  dGeomTriMeshGetArrayCallback: function(g: PdxGeom):  Pointer; cdecl;
  dGeomTriMeshGetCallback: function(g: PdxGeom):  Pointer; cdecl;
  dGeomTriMeshGetRayCallback: function(g: PdxGeom):  Pointer; cdecl;
  dGeomTriMeshSetArrayCallback: procedure(g: PdxGeom; ArrayCallback: Pointer); cdecl;
  dGeomTriMeshSetCallback: procedure(g: PdxGeom; Callback: Pointer); cdecl;
  dGeomTriMeshSetRayCallback: procedure(g: PdxGeom; RayCallback: Pointer); cdecl;
  dGeomTriMeshSetData: procedure(g: PdxGeom; Data: PdxTriMeshData); cdecl;

  {MethodVariables}

  //----- dSpace -----
  dSpaceAdd: procedure(const Space : PdxSpace; const Geom : PdxGeom); cdecl;
  dSpaceDestroy: procedure(const Space: PdxSpace); cdecl;
  dSpaceClean: procedure(const Space : PdxSpace); cdecl;
  dSpaceQuery: function(const Space : PdxSpace; const Geom : PdxGeom):  Integer; cdecl;
  dSpaceRemove: procedure(const Space : PdxSpace; const Geom : PdxGeom); cdecl;
  dSimpleSpaceCreate: function(Space : PdxSpace):  PdxSpace; cdecl;
  dHashSpaceCreate: function(Space : PdxSpace):  PdxSpace; cdecl;
  dQuadTreeSpaceCreate: function(const Space : PdxSpace; const Center, Extents : TdVector3; const Depth : Integer):  PdxSpace; cdecl;
  dHashSpaceSetLevels: procedure(const Space: PdxSpace; const minlevel, maxlevel: Integer); cdecl;
  dHashSpaceGetLevels: procedure(const Space: PdxSpace; var minlevel, maxlevel: Integer); cdecl;
  dInfiniteAABB: procedure(geom : PdxGeom; var aabb : TdAABB); cdecl;
  dSpaceGetNumGeoms: function(const Space: PdxSpace):  integer; cdecl;
  dSpaceGetGeom: function(const Space: PdxSpace; const i: Integer):  PdxGeom; cdecl;

  dSpaceSetCleanup: procedure(space : PdxSpace; const mode : integer); cdecl;
  dSpaceGetCleanup: function(Space : PdxSpace):  integer; cdecl;

  //----- dMass -----
  dMassAdd: procedure(var a,b : TdMass); cdecl;
  dMassAdjust: procedure(var m: TdMass; newmass: TdReal); cdecl;
  dMassRotate: procedure(var m: TdMass; var R: TdMatrix3); cdecl;
  dMassSetBox: procedure(var m: TdMass; density, lx, ly, lz: TdReal); cdecl;
  dMassSetBoxTotal: procedure(var m: TdMass; total_mass, lx, ly, lz: TdReal); cdecl;
  dMassSetCylinder: procedure(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl;
  dMassSetCylinderTotal: procedure(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl;
  dMassSetCappedCylinder: procedure(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl;
  dMassSetCappedCylinderTotal: procedure(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl;
  dMassSetParameters: procedure(var m: TdMass; themass, cgx, cgy, cgz, I11, I22, I33, I12, I13, I23: TdReal); cdecl;
  dMassSetSphere: procedure(var m: TdMass; density, radius: TdReal); cdecl;
  dMassSetSphereTotal: procedure(var m: TdMass; total_mass, radius: TdReal); cdecl;
  dMassSetTriMesh: procedure(var m: TdMass; density: TdReal; Vertices: PdVector3Array; nVertexStride, nVertices: Integer; Indices: PdIntegerArray; IndexCount: Integer); cdecl;
  dMassSetTriMeshTotal: procedure(var m: TdMass; total_mass: TdReal; Vertices: PdVector3Array; nVertexStride, nVertices: Integer; Indices: PdIntegerArray; IndexCount: Integer); cdecl;
  dMassSetZero: procedure(var m: TdMass); cdecl;
  dMassTranslate: procedure(var m: TdMass; x, y, z: TdReal); cdecl;
  procedure dMassSetCone(var m : TdMass; const density, radius, length : TdReal); cdecl;

  //----- Rotation.h -----

var
  dQFromAxisAndAngle: procedure(var q : TdQuaternion; const ax, ay ,az, angle : TdReal); cdecl;
  dRFromAxisAndAngle: procedure(var R : TdMatrix3; const ax, ay ,az, angle : TdReal); cdecl;
  dRSetIdentity: procedure(var R : TdMatrix3); cdecl;
  dQSetIdentity: procedure(var Q : TdQuaternion); cdecl;
  dRFromEulerAngles: procedure(var R : TdMatrix3; const phi, theta, psi : TdReal); cdecl;
  dRFrom2Axes: procedure(var R: TdMatrix3; const ax, ay, az, bx, by, bz : TdReal); cdecl;
  dRFromZAxis: procedure(var R: TdMatrix3; const ax, ay, az : TdReal); cdecl;

  dMultiply0: procedure(const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl;
  dMultiply1: procedure(const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl;
  dMultiply2: procedure(const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl;
  dQMultiply0: procedure(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  dQMultiply1: procedure(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  dQMultiply2: procedure(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  dQMultiply3: procedure(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  dRfromQ: procedure(var R : TdMatrix3; const q : TdQuaternion); cdecl;
  dQfromR: procedure(var q : TdQuaternion; const R : TdMatrix3); cdecl;
  dDQfromW: procedure(var dq : TdVector4; const w : TdVector3; const q: TdQuaternion); cdecl;

  //----- Math -----
  dNormalize3: procedure(var a : TdVector3); cdecl;
  dNormalize4: procedure(var a : TdVector4); cdecl;

  //----- Misc -----
  dClosestLineSegmentPoints: procedure(const a1, a2, b1, b2 : TdVector3; var cp1, cp2 : TdVector3); cdecl;

  dBoxTouchesBox: function(const _p1 : TdVector3; const R1 : TdMatrix3; const side1 : TdVector3; const _p2 : TdVector3; const R2 : TdMatrix3; const side2 : TdVector3):  integer; cdecl;

  dMaxDifference: function(A, B : PdReal; n, m : integer):  TdReal; cdecl;
  dMakeRandomVector: procedure(var n1 : TdVector3; a : integer; f : TdReal); cdecl;
  dAreConnected: function(a, b : PdxBody):  integer; cdecl;
  dAreConnectedExcluding: function(a, b : PdxBody; joint_type : TdJointTypeNumbers):  integer; cdecl;

  dCollide: function(o1, o2 : PdxGeom; flags : integer; var Contact : TdContactGeom; Skip : integer):  integer; cdecl;
  dSpaceCollide: procedure(const Space : PdxSpace; data : pointer; callback : TdNearCallback); cdecl;
  dSpaceCollide2: procedure(o1, o2 : PdxGeom; data : pointer; callback : TdNearCallback); cdecl;
  dMakeRandomMatrix: procedure(A : PdRealArray; n, m : integer; range :  TdReal); cdecl;
  dClearUpperTriangle: procedure(A : PdRealArray; n : integer); cdecl;
  //function dMaxDifferenceLowerTriangle (A : PdRealArray;  var B : TdReal;  n : integer) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dMaxDifferenceLowerTriangle'{$ELSE} ODEDLL{$ENDIF __GPC__};

  dRandGetSeed: function: Cardinal; cdecl;
  dRandSetSeed: procedure(const s: Cardinal); cdecl;
  dRandInt: function(const n: Integer):  Integer; cdecl;
  dRandReal: function: TdReal; cdecl;
  // return 1 if the random number generator is working.
  dTestRand: function: Integer; cdecl;

  dTestMatrixComparison: procedure; cdecl;
  dTestSolveLCP: procedure; cdecl;

  //----- Recreated -----
  function dDot (const a, b : TdVector3) : TdReal; overload;
  function dDot (const a, b : PdVector3) : TdReal; overload;

  function dDOT14(const a,b : TdRealArray) : TdReal; overload;
  function dDOT14(const a,b : PdRealArray) : TdReal; overload;

  procedure dMULTIPLY0_333(var A : TdMatrix3; const B,C : TdMatrix3);
  procedure dMULTIPLY0_331(var A : TdVector3; const B : TdMatrix3; const C : TdVector3);

  function Vector3ScalarMul(const a : TdVector3; const Scalar : TdReal) : TdVector3;
  function Vector3ADD(const a, b : TdVector3) : TdVector3;
  function Vector3SUB(const a, b : TdVector3) : TdVector3;
  function Vector3Length(const a : TdVector3) : TdReal;
  function Vector3Cross(const V1, V2 : TdVector3) : TdVector3;
  function Vector3Make(const x,y,z : TdReal) : TdVector3;

  procedure DisableStillBodies(World : PdxWorld; Threshold : TdReal=0.0001);

  procedure VerifyDelphiODE(Body : PdxBody; Geom : PdxGeom);

  // These are made public in the dynamic version
  function InitODE(ADllName : PChar) : boolean;
  procedure CloseODE;

var
  // These must be set up, so I catch the first time a user creates a new
  // object and ask what the class number became. Very fancy... They should
  // be exported from the dll, but how does one export integers from dlls?
  dSphereClass : integer=-1;
  dBoxClass : integer=-1;
  dCCylinderClass : integer=-1;
  dGeomTransformClass : integer=-1;
  dPlaneClass : integer=-1;
  dCylinderClass : integer=-1;
  dRayClass : integer=-1;
  dGeomTransformGroupClass : integer=-1;
  dTriMeshClass : integer=-1;
  dTerrainYClass : integer=-1;
  dTerrainZClass : integer=-1;
  dConeClass : integer=-1;

  IsODEInitialized : boolean = False;
  DisabledDebugGeom : boolean = False;
  DisabledDebugCollision : boolean = False;

{$IFDEF cODEDebugEnabled}
var
   ODEDebugGeomList: TGeomList;
{$ENDIF}
{$IFDEF cODEDebugCollisionEnabled}
var
   ODEDebugCollisionList: array of TdContact;
{$ENDIF}

implementation

uses
  moduleloader;

var
  WasStillBefore, WasStillBeforeOld : TList;

const
  // to be used as descriptive indices
  IndexX = 0;
  IndexY = 1;
  IndexZ = 2;
  IndexW = 3;

{ TBodyList }

procedure TBodyList.DeleteAllBodies;
var
  i : integer;
begin
  for i := 0 to Count-1 do
    dBodyDestroy(Get(i));

  Clear;
end;

function TBodyList.GetItems(i: integer): PdxBody;
begin
  result := Get(i);
end;

procedure TBodyList.SetItems(i: integer; const Value: PdxBody);
begin
  Put(i, Value);
end;

{ TGeomList }

procedure TGeomList.DeleteAllGeoms(DeleteDataAsObject : boolean=false);
var
  i : integer;
  geom : PdxGeom;
begin
  for i := 0 to Count-1 do
  begin
    geom := Get(i);
    if DeleteDataAsObject and (geom.data<>nil) then
      TObject(geom.data).Free;

    dGeomDestroy(geom);
  end;

  Clear;
end;

function TGeomList.GetItems(i: integer): PdxGeom;
begin
  result := Get(i);
end;

procedure TGeomList.SetItems(i: integer; const Value: PdxGeom);
begin
  Put(i, Value);
end;

//----- Recreated -----

function dDot (const a, b : PdVector3) : TdReal;
begin
  Assert(Assigned(a),'a not assigned!');
  Assert(Assigned(b),'b not assigned!');
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

function dDot (const a, b : TdVector3) : TdReal;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

// #define dDOT(a,b)   ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2])
// #define dDOT14(a,b) ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8])
// #define dDOT41(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[1] + (a)[8]*(b)[2])
// #define dDOT44(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[4] + (a)[8]*(b)[8])

function dDOT14(const a,b : TdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

function dDOT14(const a,b : PdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

procedure dMULTIPLY0_331(var A : TdVector3; const B : TdMatrix3; const C : TdVector3);
{var
  v : PdVector3;}
begin
  // #define dMULTIPLY0_331(A,B,C) dMULTIPLYOP0_331(A,=,B,C)

  //  #define dMULTIPLYOP0_331(A,op,B,C) \
  //    (A)[0] op dDOT((B),(C)); \
  //    (A)[1] op dDOT((B+4),(C)); \
  //    (A)[2] op dDOT((B+8),(C));


  A[0] := dDOT(PdVector3(@(B[0]))^, C);

  A[1] := dDOT(PdVector3(@(B[4]))^, C);
  A[2] := dDOT(PdVector3(@(B[8]))^, C);//}
end;

procedure dMULTIPLY0_333(var A : TdMatrix3; const B,C : TdMatrix3);
begin
  // #define dMULTIPLY0_333(A,B,C) dMULTIPLYOP0_333(A,=,B,C)
  // #define dMULTIPLYOP0_333(A,op,B,C) \
  //   (A)[0] op dDOT14((B),(C)); \
  //   (A)[1] op dDOT14((B),(C+1)); \
  //   (A)[2] op dDOT14((B),(C+2)); \
  //   (A)[4] op dDOT14((B+4),(C)); \
  //   (A)[5] op dDOT14((B+4),(C+1)); \
  //   (A)[6] op dDOT14((B+4),(C+2)); \
  //   (A)[8] op dDOT14((B+8),(C)); \
  //   (A)[9] op dDOT14((B+8),(C+1)); \
  //   (A)[10] op dDOT14((B+8),(C+2));

  A[0] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[0])));
  A[1] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[1])));
  A[2] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[2])));

  A[4] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[0])));
  A[5] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[1])));
  A[6] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[2])));

  A[8] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[0])));
  A[9] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[1])));
  A[10] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[2])));
end;

function Vector3ScalarMul(const a : TdVector3; const Scalar : TdReal) : TdVector3;
begin
  result[0] := a[0]*Scalar;
  result[1] := a[1]*Scalar;
  result[2] := a[2]*Scalar;
end;

function Vector3ADD(const a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]+b[0];
  result[1] := a[1]+b[1];
  result[2] := a[2]+b[2];
end;

function Vector3SUB(const a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]-b[0];
  result[1] := a[1]-b[1];
  result[2] := a[2]-b[2];
end;

function Vector3Length(const a : TdVector3) : TdReal;
begin
  result := sqrt(sqr(a[0])+sqr(a[1])+sqr(a[2]));
end;

function Vector3Cross(const V1, V2 : TdVector3) : TdVector3;
begin
   Result[IndexX]:=V1[IndexY] * V2[IndexZ] - V1[IndexZ] * V2[IndexY];
   Result[IndexY]:=V1[IndexZ] * V2[IndexX] - V1[IndexX] * V2[IndexZ];
   Result[IndexZ]:=V1[IndexX] * V2[IndexY] - V1[IndexY] * V2[IndexX];
end;

function Vector3Make(const x,y,z : TdReal) : TdVector3;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

function dCreateSphere(const Space : PdxSpace; const radius: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreateSphere(Space, radius);

  if dSphereClass=-1 then
    dSphereClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateBox(const Space : PdxSpace; const lx, ly, lz: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreateBox(Space, lx, ly, lz);

  if dBoxClass=-1 then
    dBoxClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreatePlane(const Space : PdxSpace; const a, b, c, d: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreatePlane(Space, a, b, c, d);

  if dPlaneClass=-1 then
    dPlaneClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateCCylinder(const Space : PdxSpace; const radius, length: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreateCCylinder(Space, radius, length);

  if dCCylinderClass=-1 then
    dCCylinderClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateCone(const Space : PdxSpace; radius, length: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreateCone(Space, radius, length);

  if dConeClass=-1 then
    dConeClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

procedure dMassSetCone(var m : TdMass; const density, radius, length : TdReal); cdecl;
var
  ms, Rsqr, Lsqr,
  Ixx, Iyy, Izz : TdReal;
begin
  // Calculate Mass
  Rsqr:=radius*radius;
  Lsqr:=length*length;
  ms:=Pi*Rsqr*length*density/3;

  // Calculate Mass Moments of Inertia about the Centroid
  Ixx:=0.15*ms*Rsqr+0.0375*ms*Lsqr;
  Iyy:=0.15*ms*Rsqr+0.0375*ms*Lsqr;
  Izz:=0.3*ms*Rsqr;

  // Set the ODE Mass parameters
  with m do begin
    mass:=ms;
    c[0]:=0; c[1]:=0; c[2]:=0.25*length;
    I[0]:=Ixx; I[1]:=0;   I[2]:=0;    I[4]:=0;
    I[4]:=0;   I[5]:=Iyy; I[6]:=0;    I[7]:=0;
    I[8]:=0;   I[9]:=0;   I[10]:=Izz; I[11]:=0;
  end;
end;

function dCreateTerrainY(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer; bFinite, bPlaceable: Integer): PdxGeom; cdecl;
begin
  result := EXT_dCreateTerrainY(Space, pHeights, vLength, nNumNodesPerSide, bFinite, bPlaceable);

  if dTerrainYClass=-1 then
    dTerrainYClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateTerrainZ(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer; bFinite, bPlaceable: Integer): PdxGeom; cdecl;
begin
  result := EXT_dCreateTerrainZ(Space, pHeights, vLength, nNumNodesPerSide, bFinite, bPlaceable);

  if dTerrainZClass=-1 then
    dTerrainZClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateGeomTransform(const Space : PdxSpace): PdxGeom; cdecl;
begin
  result := EXT_dCreateGeomTransform(Space);

  if dGeomTransformClass=-1 then
    dGeomTransformClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateCylinder(const Space : PdxSpace; r, lz : TdReal) : PdxGeom; cdecl;
begin
  result := EXT_dCreateCylinder(Space, r, lz);

  if dCylinderClass=-1 then
    dCylinderClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateRay(const Space : PdxSpace; length : TdReal) : PdxGeom; cdecl;
begin
  result := EXT_dCreateRay(Space, length);

  if dRayClass=-1 then
    dRayClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateTriMesh(const Space : PdxSpace; Data: PdxTriMeshData; Callback, ArrayCallback, RayCallback: Pointer): PdxGeom; cdecl;
begin
  result := EXT_dCreateTriMesh(Space, Data, Callback, ArrayCallback, RayCallback);

  if dTriMeshClass=-1 then
    dTriMeshClass := dGeomGetClass(result);
end;

procedure DisableStillBodies(World : PdxWorld; Threshold : TdReal=0.0001);
var
  Body : PdxBody;
  TempList : TList;
begin
  if not Assigned(WasStillBefore) then
  begin
    WasStillBefore := TList.Create;
    WasStillBeforeOld := TList.Create;
  end;

  Body := World.FirstBody;

  WasStillBefore.Clear;

  // We can't disable bodies just as soon as they're still - that could disable
  // bodies that are just slowed down or titering on an edge. If they've been
  // still for two frames, we consider them truly still. 
  while Assigned(Body) do
  begin
    if dBodyIsEnabled(Body)=1 then
    begin
      // Is the body still?
      if (abs(Body.lvel[0])<Threshold) and (abs(Body.lvel[1])<Threshold) and (abs(Body.lvel[2])<Threshold) and
         (abs(Body.avel[0])<Threshold) and (abs(Body.avel[1])<Threshold) and (abs(Body.avel[2])<Threshold) then
      begin
        if WasStillBeforeOld.IndexOf(Body)<>-1 then
          dBodyDisable(Body)
        else
          WasStillBefore.Add(Body);
      end;
    end;

    Body := PdxBody(Body.BaseObject.next);
  end;

  TempList := WasStillBeforeOld;
  WasStillBeforeOld := WasStillBefore;
  WasStillBefore := TempList;
end;

procedure VerifyDelphiODE(Body : PdxBody; Geom : PdxGeom);
var
  m : TdMass;
  VerificationPointer : pointer;
begin
  VerificationPointer := pointer( -1 ); // A known pointer
  // Verify Body
  dBodySetData( Body, VerificationPointer );
  Assert( dBodyGetData( Body ) = VerificationPointer, 'Body test 1 fails' );
  Assert( Body.BaseObject.userdata = VerificationPointer, 'Body test 2 fails' );

  dBodyGetMass(Body, m);

  Assert(Body.mass.mass = m.mass, 'Body test 3 fails');

  // Verify Geom
  dGeomSetData( Geom, VerificationPointer );
  Assert( dGeomGetData( Geom ) = VerificationPointer, 'Geom test 1 fails' );
  Assert(dGeomGetBody(Geom)=Geom.Body, 'Geom test 2 fails');
  Assert( Geom.Data = VerificationPointer, 'Geom test 3 fails' );
end;

var
  vODEHandle : TModuleHandle;

function InitODE(ADllName : PChar) : boolean;
begin
  if ADllName = '' then
    ADllName := ODEDLL;

  IsODEInitialized := LoadModule( vODEHandle, ADllName );
  result := IsODEInitialized;

  if IsODEInitialized then
  begin
  // These lines are auto-created
  dWorldCreate := GetModuleSymbol( vODEHandle, 'dWorldCreate' );
  dWorldDestroy := GetModuleSymbol( vODEHandle, 'dWorldDestroy' );
  dWorldGetCFM := GetModuleSymbol( vODEHandle, 'dWorldGetCFM' );
  dWorldGetERP := GetModuleSymbol( vODEHandle, 'dWorldGetERP' );
  dWorldGetGravity := GetModuleSymbol( vODEHandle, 'dWorldGetGravity' );
  dWorldImpulseToForce := GetModuleSymbol( vODEHandle, 'dWorldImpulseToForce' );
  dWorldSetCFM := GetModuleSymbol( vODEHandle, 'dWorldSetCFM' );
  dWorldSetERP := GetModuleSymbol( vODEHandle, 'dWorldSetERP' );
  dWorldSetGravity := GetModuleSymbol( vODEHandle, 'dWorldSetGravity' );
  dWorldSetContactMaxCorrectingVel := GetModuleSymbol( vODEHandle, 'dWorldSetContactMaxCorrectingVel' );
  dWorldGetContactMaxCorrectingVel := GetModuleSymbol( vODEHandle, 'dWorldGetContactMaxCorrectingVel' );
  dWorldSetContactSurfaceLayer := GetModuleSymbol( vODEHandle, 'dWorldSetContactSurfaceLayer' );
  dWorldGetContactSurfaceLayer := GetModuleSymbol( vODEHandle, 'dWorldGetContactSurfaceLayer' );
  dWorldStep := GetModuleSymbol( vODEHandle, 'dWorldStep' );
  dWorldQuickStep := GetModuleSymbol( vODEHandle, 'dWorldQuickStep' );
  dWorldSetQuickStepNumIterations := GetModuleSymbol( vODEHandle, 'dWorldSetQuickStepNumIterations' );
  dWorldGetQuickStepNumIterations := GetModuleSymbol( vODEHandle, 'dWorldGetQuickStepNumIterations' );
  dWorldSetQuickStepW := GetModuleSymbol( vODEHandle, 'dWorldSetQuickStepW' );
  dWorldGetQuickStepW := GetModuleSymbol( vODEHandle, 'dWorldGetQuickStepW' );
  dWorldStepFast1 := GetModuleSymbol( vODEHandle, 'dWorldStepFast1' );
  dWorldSetAutoEnableDepthSF1 := GetModuleSymbol( vODEHandle, 'dWorldSetAutoEnableDepthSF1' );
  dWorldGetAutoEnableDepthSF1 := GetModuleSymbol( vODEHandle, 'dWorldGetAutoEnableDepthSF1' );
  dCloseODE := GetModuleSymbol( vODEHandle, 'dCloseODE' );
  dBodyAddForce := GetModuleSymbol( vODEHandle, 'dBodyAddForce' );
  dBodyAddForceAtPos := GetModuleSymbol( vODEHandle, 'dBodyAddForceAtPos' );
  dBodyAddForceAtRelPos := GetModuleSymbol( vODEHandle, 'dBodyAddForceAtRelPos' );
  dBodyAddRelForce := GetModuleSymbol( vODEHandle, 'dBodyAddRelForce' );
  dBodyAddRelForceAtPos := GetModuleSymbol( vODEHandle, 'dBodyAddRelForceAtPos' );
  dBodyAddRelForceAtRelPos := GetModuleSymbol( vODEHandle, 'dBodyAddRelForceAtRelPos' );
  dBodyAddRelTorque := GetModuleSymbol( vODEHandle, 'dBodyAddRelTorque' );
  dBodyAddTorque := GetModuleSymbol( vODEHandle, 'dBodyAddTorque' );
  dBodyCreate := GetModuleSymbol( vODEHandle, 'dBodyCreate' );
  dBodyDestroy := GetModuleSymbol( vODEHandle, 'dBodyDestroy' );
  dBodyDisable := GetModuleSymbol( vODEHandle, 'dBodyDisable' );
  dBodyEnable := GetModuleSymbol( vODEHandle, 'dBodyEnable' );
  dBodyGetAngularVel := GetModuleSymbol( vODEHandle, 'dBodyGetAngularVel' );
  dBodyGetFiniteRotationAxis := GetModuleSymbol( vODEHandle, 'dBodyGetFiniteRotationAxis' );
  dBodyGetFiniteRotationMode := GetModuleSymbol( vODEHandle, 'dBodyGetFiniteRotationMode' );
  dBodyGetForce := GetModuleSymbol( vODEHandle, 'dBodyGetForce' );
  dBodyGetGravityMode := GetModuleSymbol( vODEHandle, 'dBodyGetGravityMode' );
  dBodyGetJoint := GetModuleSymbol( vODEHandle, 'dBodyGetJoint' );
  dBodyGetLinearVel := GetModuleSymbol( vODEHandle, 'dBodyGetLinearVel' );
  dBodyGetMass := GetModuleSymbol( vODEHandle, 'dBodyGetMass' );
  dBodyGetNumJoints := GetModuleSymbol( vODEHandle, 'dBodyGetNumJoints' );
  dBodyGetPointVel := GetModuleSymbol( vODEHandle, 'dBodyGetPointVel' );
  dBodyGetPosRelPoint := GetModuleSymbol( vODEHandle, 'dBodyGetPosRelPoint' );
  dBodyGetPosition := GetModuleSymbol( vODEHandle, 'dBodyGetPosition' );
  dBodyGetQuaternion := GetModuleSymbol( vODEHandle, 'dBodyGetQuaternion' );
  dBodyGetRelPointPos := GetModuleSymbol( vODEHandle, 'dBodyGetRelPointPos' );
  dBodyGetRelPointVel := GetModuleSymbol( vODEHandle, 'dBodyGetRelPointVel' );
  dBodyGetRotation := GetModuleSymbol( vODEHandle, 'dBodyGetRotation' );
  dBodyGetTorque := GetModuleSymbol( vODEHandle, 'dBodyGetTorque' );
  dBodyIsEnabled := GetModuleSymbol( vODEHandle, 'dBodyIsEnabled' );
  dBodySetAngularVel := GetModuleSymbol( vODEHandle, 'dBodySetAngularVel' );
  dBodySetFiniteRotationAxis := GetModuleSymbol( vODEHandle, 'dBodySetFiniteRotationAxis' );
  dBodySetFiniteRotationMode := GetModuleSymbol( vODEHandle, 'dBodySetFiniteRotationMode' );
  dBodySetForce := GetModuleSymbol( vODEHandle, 'dBodySetForce' );
  dBodySetGravityMode := GetModuleSymbol( vODEHandle, 'dBodySetGravityMode' );
  dBodySetLinearVel := GetModuleSymbol( vODEHandle, 'dBodySetLinearVel' );
  dBodySetMass := GetModuleSymbol( vODEHandle, 'dBodySetMass' );
  dBodySetPosition := GetModuleSymbol( vODEHandle, 'dBodySetPosition' );
  dBodySetQuaternion := GetModuleSymbol( vODEHandle, 'dBodySetQuaternion' );
  dBodySetRotation := GetModuleSymbol( vODEHandle, 'dBodySetRotation' );
  dBodySetTorque := GetModuleSymbol( vODEHandle, 'dBodySetTorque' );
  dBodyVectorFromWorld := GetModuleSymbol( vODEHandle, 'dBodyVectorFromWorld' );
  dBodyVectorToWorld := GetModuleSymbol( vODEHandle, 'dBodyVectorToWorld' );
  dBodySetData := GetModuleSymbol( vODEHandle, 'dBodySetData' );
  dBodyGetData := GetModuleSymbol( vODEHandle, 'dBodyGetData' );
  dJointAttach := GetModuleSymbol( vODEHandle, 'dJointAttach' );
  dJointCreateAMotor := GetModuleSymbol( vODEHandle, 'dJointCreateAMotor' );
  dJointCreateBall := GetModuleSymbol( vODEHandle, 'dJointCreateBall' );
  dJointCreateContact := GetModuleSymbol( vODEHandle, 'dJointCreateContact' );
  dJointCreateFixed := GetModuleSymbol( vODEHandle, 'dJointCreateFixed' );
  dJointCreateHinge := GetModuleSymbol( vODEHandle, 'dJointCreateHinge' );
  dJointCreateHinge2 := GetModuleSymbol( vODEHandle, 'dJointCreateHinge2' );
  dJointCreateSlider := GetModuleSymbol( vODEHandle, 'dJointCreateSlider' );
  dJointCreateUniversal := GetModuleSymbol( vODEHandle, 'dJointCreateUniversal' );
  dJointCreatePlane2D := GetModuleSymbol( vODEHandle, 'dJointCreatePlane2D' );
  dJointDestroy := GetModuleSymbol( vODEHandle, 'dJointDestroy' );
  dJointGetAMotorAngle := GetModuleSymbol( vODEHandle, 'dJointGetAMotorAngle' );
  dJointGetAMotorAngleRate := GetModuleSymbol( vODEHandle, 'dJointGetAMotorAngleRate' );
  dJointGetAMotorAxis := GetModuleSymbol( vODEHandle, 'dJointGetAMotorAxis' );
  dJointGetAMotorAxisRel := GetModuleSymbol( vODEHandle, 'dJointGetAMotorAxisRel' );
  dJointGetAMotorMode := GetModuleSymbol( vODEHandle, 'dJointGetAMotorMode' );
  dJointGetAMotorNumAxes := GetModuleSymbol( vODEHandle, 'dJointGetAMotorNumAxes' );
  dJointGetAMotorParam := GetModuleSymbol( vODEHandle, 'dJointGetAMotorParam' );
  dJointGetBallAnchor := GetModuleSymbol( vODEHandle, 'dJointGetBallAnchor' );
  dJointGetBallAnchor2 := GetModuleSymbol( vODEHandle, 'dJointGetBallAnchor2' );
  dJointGetBody := GetModuleSymbol( vODEHandle, 'dJointGetBody' );
  dJointGetHinge2Anchor := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Anchor' );
  dJointGetHinge2Anchor2 := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Anchor2' );
  dJointGetHinge2Angle1 := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Angle1' );
  dJointGetHinge2Angle1Rate := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Angle1Rate' );
  dJointGetHinge2Angle2Rate := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Angle2Rate' );
  dJointGetHinge2Axis1 := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Axis1' );
  dJointGetHinge2Axis2 := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Axis2' );
  dJointGetHinge2Param := GetModuleSymbol( vODEHandle, 'dJointGetHinge2Param' );
  dJointGetHingeAnchor := GetModuleSymbol( vODEHandle, 'dJointGetHingeAnchor' );
  dJointGetHingeAnchor2 := GetModuleSymbol( vODEHandle, 'dJointGetHingeAnchor2' );
  dJointGetHingeAngle := GetModuleSymbol( vODEHandle, 'dJointGetHingeAngle' );
  dJointGetHingeAngleRate := GetModuleSymbol( vODEHandle, 'dJointGetHingeAngleRate' );
  dJointGetHingeAxis := GetModuleSymbol( vODEHandle, 'dJointGetHingeAxis' );
  dJointGetHingeParam := GetModuleSymbol( vODEHandle, 'dJointGetHingeParam' );
  dJointGetSliderAxis := GetModuleSymbol( vODEHandle, 'dJointGetSliderAxis' );
  dJointGetSliderParam := GetModuleSymbol( vODEHandle, 'dJointGetSliderParam' );
  dJointGetSliderPosition := GetModuleSymbol( vODEHandle, 'dJointGetSliderPosition' );
  dJointGetSliderPositionRate := GetModuleSymbol( vODEHandle, 'dJointGetSliderPositionRate' );
  dJointGetType := GetModuleSymbol( vODEHandle, 'dJointGetType' );
  dJointGetUniversalAnchor := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAnchor' );
  dJointGetUniversalAnchor2 := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAnchor2' );
  dJointGetUniversalAxis1 := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAxis1' );
  dJointGetUniversalAxis2 := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAxis2' );
  dJointGetUniversalParam := GetModuleSymbol( vODEHandle, 'dJointGetUniversalParam' );
  dJointGetUniversalAngle1 := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAngle1' );
  dJointGetUniversalAngle2 := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAngle2' );
  dJointGetUniversalAngle1Rate := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAngle1Rate' );
  dJointGetUniversalAngle2Rate := GetModuleSymbol( vODEHandle, 'dJointGetUniversalAngle2Rate' );
  dJointGroupCreate := GetModuleSymbol( vODEHandle, 'dJointGroupCreate' );
  dJointGroupDestroy := GetModuleSymbol( vODEHandle, 'dJointGroupDestroy' );
  dJointGroupEmpty := GetModuleSymbol( vODEHandle, 'dJointGroupEmpty' );
  dJointSetAMotorAngle := GetModuleSymbol( vODEHandle, 'dJointSetAMotorAngle' );
  dJointSetAMotorAxis := GetModuleSymbol( vODEHandle, 'dJointSetAMotorAxis' );
  dJointSetAMotorMode := GetModuleSymbol( vODEHandle, 'dJointSetAMotorMode' );
  dJointSetAMotorNumAxes := GetModuleSymbol( vODEHandle, 'dJointSetAMotorNumAxes' );
  dJointSetAMotorParam := GetModuleSymbol( vODEHandle, 'dJointSetAMotorParam' );
  dJointSetBallAnchor := GetModuleSymbol( vODEHandle, 'dJointSetBallAnchor' );
  dJointSetFixed := GetModuleSymbol( vODEHandle, 'dJointSetFixed' );
  dJointSetHinge2Anchor := GetModuleSymbol( vODEHandle, 'dJointSetHinge2Anchor' );
  dJointSetHinge2Axis1 := GetModuleSymbol( vODEHandle, 'dJointSetHinge2Axis1' );
  dJointSetHinge2Axis2 := GetModuleSymbol( vODEHandle, 'dJointSetHinge2Axis2' );
  dJointSetHinge2Param := GetModuleSymbol( vODEHandle, 'dJointSetHinge2Param' );
  dJointSetHingeAnchor := GetModuleSymbol( vODEHandle, 'dJointSetHingeAnchor' );
  dJointSetHingeAxis := GetModuleSymbol( vODEHandle, 'dJointSetHingeAxis' );
  dJointSetHingeParam := GetModuleSymbol( vODEHandle, 'dJointSetHingeParam' );
  dJointSetSliderAxis := GetModuleSymbol( vODEHandle, 'dJointSetSliderAxis' );
  dJointSetSliderParam := GetModuleSymbol( vODEHandle, 'dJointSetSliderParam' );
  dJointSetUniversalAnchor := GetModuleSymbol( vODEHandle, 'dJointSetUniversalAnchor' );
  dJointSetUniversalAxis1 := GetModuleSymbol( vODEHandle, 'dJointSetUniversalAxis1' );
  dJointSetUniversalAxis2 := GetModuleSymbol( vODEHandle, 'dJointSetUniversalAxis2' );
  dJointSetUniversalParam := GetModuleSymbol( vODEHandle, 'dJointSetUniversalParam' );
  dJointSetPlane2DXParam := GetModuleSymbol( vODEHandle, 'dJointSetPlane2DXParam' );
  dJointSetPlane2DYParam := GetModuleSymbol( vODEHandle, 'dJointSetPlane2DYParam' );
  dJointSetPlane2DAngleParam := GetModuleSymbol( vODEHandle, 'dJointSetPlane2DAngleParam' );
  dJointGetData := GetModuleSymbol( vODEHandle, 'dJointGetData' );
  dJointSetData := GetModuleSymbol( vODEHandle, 'dJointSetData' );
  dJointAddAMotorTorques := GetModuleSymbol( vODEHandle, 'dJointAddAMotorTorques' );
  dJointAddHinge2Torques := GetModuleSymbol( vODEHandle, 'dJointAddHinge2Torques' );
  dJointAddHingeTorque := GetModuleSymbol( vODEHandle, 'dJointAddHingeTorque' );
  dJointAddSliderForce := GetModuleSymbol( vODEHandle, 'dJointAddSliderForce' );
  dJointAddUniversalTorques := GetModuleSymbol( vODEHandle, 'dJointAddUniversalTorques' );
  dJointSetFeedback := GetModuleSymbol( vODEHandle, 'dJointSetFeedback' );
  dJointGetFeedback := GetModuleSymbol( vODEHandle, 'dJointGetFeedback' );
  dJointCorrectHinge2 := GetModuleSymbol( vODEHandle, 'dJointCorrectHinge2' );
  dWorldGetAutoDisableLinearThreshold := GetModuleSymbol( vODEHandle, 'dWorldGetAutoDisableLinearThreshold' );
  dWorldSetAutoDisableLinearThreshold := GetModuleSymbol( vODEHandle, 'dWorldSetAutoDisableLinearThreshold' );
  dWorldGetAutoDisableAngularThreshold := GetModuleSymbol( vODEHandle, 'dWorldGetAutoDisableAngularThreshold' );
  dWorldSetAutoDisableAngularThreshold := GetModuleSymbol( vODEHandle, 'dWorldSetAutoDisableAngularThreshold' );
  dWorldGetAutoDisableSteps := GetModuleSymbol( vODEHandle, 'dWorldGetAutoDisableSteps' );
  dWorldSetAutoDisableSteps := GetModuleSymbol( vODEHandle, 'dWorldSetAutoDisableSteps' );
  dWorldGetAutoDisableTime := GetModuleSymbol( vODEHandle, 'dWorldGetAutoDisableTime' );
  dWorldSetAutoDisableTime := GetModuleSymbol( vODEHandle, 'dWorldSetAutoDisableTime' );
  dWorldGetAutoDisableFlag := GetModuleSymbol( vODEHandle, 'dWorldGetAutoDisableFlag' );
  dWorldSetAutoDisableFlag := GetModuleSymbol( vODEHandle, 'dWorldSetAutoDisableFlag' );
  dBodyGetAutoDisableLinearThreshold := GetModuleSymbol( vODEHandle, 'dBodyGetAutoDisableLinearThreshold' );
  dBodySetAutoDisableLinearThreshold := GetModuleSymbol( vODEHandle, 'dBodySetAutoDisableLinearThreshold' );
  dBodyGetAutoDisableAngularThreshold := GetModuleSymbol( vODEHandle, 'dBodyGetAutoDisableAngularThreshold' );
  dBodySetAutoDisableAngularThreshold := GetModuleSymbol( vODEHandle, 'dBodySetAutoDisableAngularThreshold' );
  dBodyGetAutoDisableSteps := GetModuleSymbol( vODEHandle, 'dBodyGetAutoDisableSteps' );
  dBodySetAutoDisableSteps := GetModuleSymbol( vODEHandle, 'dBodySetAutoDisableSteps' );
  dBodyGetAutoDisableTime := GetModuleSymbol( vODEHandle, 'dBodyGetAutoDisableTime' );
  dBodySetAutoDisableTime := GetModuleSymbol( vODEHandle, 'dBodySetAutoDisableTime' );
  dBodyGetAutoDisableFlag := GetModuleSymbol( vODEHandle, 'dBodyGetAutoDisableFlag' );
  dBodySetAutoDisableFlag := GetModuleSymbol( vODEHandle, 'dBodySetAutoDisableFlag' );
  dBodySetAutoDisableDefaults := GetModuleSymbol( vODEHandle, 'dBodySetAutoDisableDefaults' );
  dGeomBoxGetLengths := GetModuleSymbol( vODEHandle, 'dGeomBoxGetLengths' );
  dGeomBoxSetLengths := GetModuleSymbol( vODEHandle, 'dGeomBoxSetLengths' );
  dGeomCCylinderGetParams := GetModuleSymbol( vODEHandle, 'dGeomCCylinderGetParams' );
  dGeomCCylinderSetParams := GetModuleSymbol( vODEHandle, 'dGeomCCylinderSetParams' );
  dGeomDestroy := GetModuleSymbol( vODEHandle, 'dGeomDestroy' );
  dGeomGetAABB := GetModuleSymbol( vODEHandle, 'dGeomGetAABB' );
  dGeomGetBody := GetModuleSymbol( vODEHandle, 'dGeomGetBody' );
  dGeomGetClass := GetModuleSymbol( vODEHandle, 'dGeomGetClass' );
  dGeomGetPosition := GetModuleSymbol( vODEHandle, 'dGeomGetPosition' );
  dGeomGetRotation := GetModuleSymbol( vODEHandle, 'dGeomGetRotation' );
  dGeomGetQuaternion := GetModuleSymbol( vODEHandle, 'dGeomGetQuaternion' );
  dGeomGetSpace := GetModuleSymbol( vODEHandle, 'dGeomGetSpace' );
  dGeomPlaneGetParams := GetModuleSymbol( vODEHandle, 'dGeomPlaneGetParams' );
  dGeomPlaneSetParams := GetModuleSymbol( vODEHandle, 'dGeomPlaneSetParams' );
  dGeomSetBody := GetModuleSymbol( vODEHandle, 'dGeomSetBody' );
  dGeomSetPosition := GetModuleSymbol( vODEHandle, 'dGeomSetPosition' );
  dGeomSetRotation := GetModuleSymbol( vODEHandle, 'dGeomSetRotation' );
  dGeomSetQuaternion := GetModuleSymbol( vODEHandle, 'dGeomSetQuaternion' );
  dGeomSphereGetRadius := GetModuleSymbol( vODEHandle, 'dGeomSphereGetRadius' );
  dGeomSphereSetRadius := GetModuleSymbol( vODEHandle, 'dGeomSphereSetRadius' );
  dGeomTransformGetCleanup := GetModuleSymbol( vODEHandle, 'dGeomTransformGetCleanup' );
  dGeomTransformGetGeom := GetModuleSymbol( vODEHandle, 'dGeomTransformGetGeom' );
  dGeomTransformSetCleanup := GetModuleSymbol( vODEHandle, 'dGeomTransformSetCleanup' );
  dGeomTransformSetGeom := GetModuleSymbol( vODEHandle, 'dGeomTransformSetGeom' );
  dGeomSetData := GetModuleSymbol( vODEHandle, 'dGeomSetData' );
  dGeomGetData := GetModuleSymbol( vODEHandle, 'dGeomGetData' );
  dGeomTransformSetInfo := GetModuleSymbol( vODEHandle, 'dGeomTransformSetInfo' );
  dGeomTransformGetInfo := GetModuleSymbol( vODEHandle, 'dGeomTransformGetInfo' );
  dGeomIsSpace := GetModuleSymbol( vODEHandle, 'dGeomIsSpace' );
  dGeomSetCategoryBits := GetModuleSymbol( vODEHandle, 'dGeomSetCategoryBits' );
  dGeomSetCollideBits := GetModuleSymbol( vODEHandle, 'dGeomSetCollideBits' );
  dGeomGetCategoryBits := GetModuleSymbol( vODEHandle, 'dGeomGetCategoryBits' );
  dGeomGetCollideBits := GetModuleSymbol( vODEHandle, 'dGeomGetCollideBits' );
  dGeomEnable := GetModuleSymbol( vODEHandle, 'dGeomEnable' );
  dGeomDisable := GetModuleSymbol( vODEHandle, 'dGeomDisable' );
  dGeomIsEnabled := GetModuleSymbol( vODEHandle, 'dGeomIsEnabled' );
  dGeomSpherePointDepth := GetModuleSymbol( vODEHandle, 'dGeomSpherePointDepth' );
  dGeomBoxPointDepth := GetModuleSymbol( vODEHandle, 'dGeomBoxPointDepth' );
  dGeomPlanePointDepth := GetModuleSymbol( vODEHandle, 'dGeomPlanePointDepth' );
  dGeomCCylinderPointDepth := GetModuleSymbol( vODEHandle, 'dGeomCCylinderPointDepth' );
  EXT_dCreateSphere := GetModuleSymbol( vODEHandle, 'dCreateSphere' );
  EXT_dCreateBox := GetModuleSymbol( vODEHandle, 'dCreateBox' );
  EXT_dCreatePlane := GetModuleSymbol( vODEHandle, 'dCreatePlane' );
  EXT_dCreateCCylinder := GetModuleSymbol( vODEHandle, 'dCreateCCylinder' );
  EXT_dCreateCylinder := GetModuleSymbol( vODEHandle, 'dCreateCylinder' );
  EXT_dCreateCone := GetModuleSymbol( vODEHandle, 'dCreateCone' );
  EXT_dCreateTerrainY := GetModuleSymbol( vODEHandle, 'dCreateTerrainY' );
  EXT_dCreateTerrainZ := GetModuleSymbol( vODEHandle, 'dCreateTerrainZ' );
  EXT_dCreateRay := GetModuleSymbol( vODEHandle, 'dCreateRay' );
  EXT_dCreateGeomTransform := GetModuleSymbol( vODEHandle, 'dCreateGeomTransform' );
  EXT_dCreateTriMesh := GetModuleSymbol( vODEHandle, 'dCreateTriMesh' );
  dGeomConeSetParams := GetModuleSymbol( vODEHandle, 'dGeomConeSetParams' );
  dGeomConeGetParams := GetModuleSymbol( vODEHandle, 'dGeomConeGetParams' );
  dGeomConePointDepth := GetModuleSymbol( vODEHandle, 'dGeomConePointDepth' );
  dGeomTerrainYPointDepth := GetModuleSymbol( vODEHandle, 'dGeomTerrainYPointDepth' );
  dGeomTerrainZPointDepth := GetModuleSymbol( vODEHandle, 'dGeomTerrainZPointDepth' );
  dGeomCylinderSetParams := GetModuleSymbol( vODEHandle, 'dGeomCylinderSetParams' );
  dGeomCylinderGetParams := GetModuleSymbol( vODEHandle, 'dGeomCylinderGetParams' );
  dGeomRaySetLength := GetModuleSymbol( vODEHandle, 'dGeomRaySetLength' );
  dGeomRayGetLength := GetModuleSymbol( vODEHandle, 'dGeomRayGetLength' );
  dGeomRaySetClosestHit := GetModuleSymbol( vODEHandle, 'dGeomRaySetClosestHit' );
  dGeomRayGetClosestHit := GetModuleSymbol( vODEHandle, 'dGeomRayGetClosestHit' );
  dGeomRaySet := GetModuleSymbol( vODEHandle, 'dGeomRaySet' );
  dGeomRayGet := GetModuleSymbol( vODEHandle, 'dGeomRayGet' );
  dCreateGeomClass := GetModuleSymbol( vODEHandle, 'dCreateGeomClass' );
  dGeomGetClassData := GetModuleSymbol( vODEHandle, 'dGeomGetClassData' );
  dCreateGeom := GetModuleSymbol( vODEHandle, 'dCreateGeom' );
  dGeomTriMeshDataBuildSimple := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataBuildSimple' );
  dGeomTriMeshDataBuildSimple1 := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataBuildSimple1' );
  dGeomTriMeshDataBuildDouble := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataBuildDouble' );
  dGeomTriMeshDataBuildDouble1 := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataBuildDouble1' );
  dGeomTriMeshDataBuildSingle := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataBuildSingle' );
  dGeomTriMeshDataBuildSingle1 := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataBuildSingle1' );
  dGeomTriMeshDataCreate := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataCreate' );
  dGeomTriMeshDataSet := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataSet' );
  dGeomTriMeshDataDestroy := GetModuleSymbol( vODEHandle, 'dGeomTriMeshDataDestroy' );
  dGeomTriMeshGetTriangle := GetModuleSymbol( vODEHandle, 'dGeomTriMeshGetTriangle' );
  dGeomTriMeshGetPoint := GetModuleSymbol( vODEHandle, 'dGeomTriMeshGetPoint' );
  dGeomTriMeshClearTCCache := GetModuleSymbol( vODEHandle, 'dGeomTriMeshClearTCCache' );
  dGeomTriMeshEnableTC := GetModuleSymbol( vODEHandle, 'dGeomTriMeshEnableTC' );
  dGeomTriMeshIsTCEnabled := GetModuleSymbol( vODEHandle, 'dGeomTriMeshIsTCEnabled' );
  dGeomTriMeshGetArrayCallback := GetModuleSymbol( vODEHandle, 'dGeomTriMeshGetArrayCallback' );
  dGeomTriMeshGetCallback := GetModuleSymbol( vODEHandle, 'dGeomTriMeshGetCallback' );
  dGeomTriMeshGetRayCallback := GetModuleSymbol( vODEHandle, 'dGeomTriMeshGetRayCallback' );
  dGeomTriMeshSetArrayCallback := GetModuleSymbol( vODEHandle, 'dGeomTriMeshSetArrayCallback' );
  dGeomTriMeshSetCallback := GetModuleSymbol( vODEHandle, 'dGeomTriMeshSetCallback' );
  dGeomTriMeshSetRayCallback := GetModuleSymbol( vODEHandle, 'dGeomTriMeshSetRayCallback' );
  dGeomTriMeshSetData := GetModuleSymbol( vODEHandle, 'dGeomTriMeshSetData' );
  dSpaceAdd := GetModuleSymbol( vODEHandle, 'dSpaceAdd' );
  dSpaceDestroy := GetModuleSymbol( vODEHandle, 'dSpaceDestroy' );
  dSpaceClean := GetModuleSymbol( vODEHandle, 'dSpaceClean' );
  dSpaceQuery := GetModuleSymbol( vODEHandle, 'dSpaceQuery' );
  dSpaceRemove := GetModuleSymbol( vODEHandle, 'dSpaceRemove' );
  dSimpleSpaceCreate := GetModuleSymbol( vODEHandle, 'dSimpleSpaceCreate' );
  dHashSpaceCreate := GetModuleSymbol( vODEHandle, 'dHashSpaceCreate' );
  dQuadTreeSpaceCreate := GetModuleSymbol( vODEHandle, 'dQuadTreeSpaceCreate' );
  dHashSpaceSetLevels := GetModuleSymbol( vODEHandle, 'dHashSpaceSetLevels' );
  dHashSpaceGetLevels := GetModuleSymbol( vODEHandle, 'dHashSpaceGetLevels' );
  dInfiniteAABB := GetModuleSymbol( vODEHandle, 'dInfiniteAABB' );
  dSpaceGetNumGeoms := GetModuleSymbol( vODEHandle, 'dSpaceGetNumGeoms' );
  dSpaceGetGeom := GetModuleSymbol( vODEHandle, 'dSpaceGetGeom' );
  dSpaceSetCleanup := GetModuleSymbol( vODEHandle, 'dSpaceSetCleanup' );
  dSpaceGetCleanup := GetModuleSymbol( vODEHandle, 'dSpaceGetCleanup' );
  dMassAdd := GetModuleSymbol( vODEHandle, 'dMassAdd' );
  dMassAdjust := GetModuleSymbol( vODEHandle, 'dMassAdjust' );
  dMassRotate := GetModuleSymbol( vODEHandle, 'dMassRotate' );
  dMassSetBox := GetModuleSymbol( vODEHandle, 'dMassSetBox' );
  dMassSetBoxTotal := GetModuleSymbol( vODEHandle, 'dMassSetBoxTotal' );
  dMassSetCylinder := GetModuleSymbol( vODEHandle, 'dMassSetCylinder' );
  dMassSetCylinderTotal := GetModuleSymbol( vODEHandle, 'dMassSetCylinderTotal' );
  dMassSetCappedCylinder := GetModuleSymbol( vODEHandle, 'dMassSetCappedCylinder' );
  dMassSetCappedCylinderTotal := GetModuleSymbol( vODEHandle, 'dMassSetCappedCylinderTotal' );
  dMassSetParameters := GetModuleSymbol( vODEHandle, 'dMassSetParameters' );
  dMassSetSphere := GetModuleSymbol( vODEHandle, 'dMassSetSphere' );
  dMassSetSphereTotal := GetModuleSymbol( vODEHandle, 'dMassSetSphereTotal' );
  dMassSetTriMesh := GetModuleSymbol( vODEHandle, 'dMassSetTriMesh' );
  dMassSetTriMeshTotal := GetModuleSymbol( vODEHandle, 'dMassSetTriMeshTotal' );
  dMassSetZero := GetModuleSymbol( vODEHandle, 'dMassSetZero' );
  dMassTranslate := GetModuleSymbol( vODEHandle, 'dMassTranslate' );
  dQFromAxisAndAngle := GetModuleSymbol( vODEHandle, 'dQFromAxisAndAngle' );
  dRFromAxisAndAngle := GetModuleSymbol( vODEHandle, 'dRFromAxisAndAngle' );
  dRSetIdentity := GetModuleSymbol( vODEHandle, 'dRSetIdentity' );
  dQSetIdentity := GetModuleSymbol( vODEHandle, 'dQSetIdentity' );
  dRFromEulerAngles := GetModuleSymbol( vODEHandle, 'dRFromEulerAngles' );
  dRFrom2Axes := GetModuleSymbol( vODEHandle, 'dRFrom2Axes' );
  dRFromZAxis := GetModuleSymbol( vODEHandle, 'dRFromZAxis' );
  dMultiply0 := GetModuleSymbol( vODEHandle, 'dMultiply0' );
  dMultiply1 := GetModuleSymbol( vODEHandle, 'dMultiply1' );
  dMultiply2 := GetModuleSymbol( vODEHandle, 'dMultiply2' );
  dQMultiply0 := GetModuleSymbol( vODEHandle, 'dQMultiply0' );
  dQMultiply1 := GetModuleSymbol( vODEHandle, 'dQMultiply1' );
  dQMultiply2 := GetModuleSymbol( vODEHandle, 'dQMultiply2' );
  dQMultiply3 := GetModuleSymbol( vODEHandle, 'dQMultiply3' );
  dRfromQ := GetModuleSymbol( vODEHandle, 'dRfromQ' );
  dQfromR := GetModuleSymbol( vODEHandle, 'dQfromR' );
  dDQfromW := GetModuleSymbol( vODEHandle, 'dDQfromW' );
  dNormalize3 := GetModuleSymbol( vODEHandle, 'dNormalize3' );
  dNormalize4 := GetModuleSymbol( vODEHandle, 'dNormalize4' );
  dClosestLineSegmentPoints := GetModuleSymbol( vODEHandle, 'dClosestLineSegmentPoints' );
  dBoxTouchesBox := GetModuleSymbol( vODEHandle, 'dBoxTouchesBox' );
  dMaxDifference := GetModuleSymbol( vODEHandle, 'dMaxDifference' );
  dMakeRandomVector := GetModuleSymbol( vODEHandle, 'dMakeRandomVector' );
  dAreConnected := GetModuleSymbol( vODEHandle, 'dAreConnected' );
  dAreConnectedExcluding := GetModuleSymbol( vODEHandle, 'dAreConnectedExcluding' );
  dCollide := GetModuleSymbol( vODEHandle, 'dCollide' );
  dSpaceCollide := GetModuleSymbol( vODEHandle, 'dSpaceCollide' );
  dSpaceCollide2 := GetModuleSymbol( vODEHandle, 'dSpaceCollide2' );
  dMakeRandomMatrix := GetModuleSymbol( vODEHandle, 'dMakeRandomMatrix' );
  dClearUpperTriangle := GetModuleSymbol( vODEHandle, 'dClearUpperTriangle' );
  dRandGetSeed := GetModuleSymbol( vODEHandle, 'dRandGetSeed' );
  dRandSetSeed := GetModuleSymbol( vODEHandle, 'dRandSetSeed' );
  dRandInt := GetModuleSymbol( vODEHandle, 'dRandInt' );
  dRandReal := GetModuleSymbol( vODEHandle, 'dRandReal' );
  dTestRand := GetModuleSymbol( vODEHandle, 'dTestRand' );
  dTestMatrixComparison := GetModuleSymbol( vODEHandle, 'dTestMatrixComparison' );
  dTestSolveLCP := GetModuleSymbol( vODEHandle, 'dTestSolveLCP' );

  end;
end;

procedure CloseODE;
begin
  IsODEInitialized := false;
  UnLoadModule( vODEHandle );
end;

end.
