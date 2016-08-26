{: GLVectorFileObjects<p>

	Vector File related objects for GLScene<p>

	<b>History :</b><font size=-1><ul>
      26/08/02 - EG - Added missing header, added glsUnknownExtension
	</ul></font>
}
unit GLStrings;

interface

resourcestring

  // SceneViewer
  glsNoRenderingContext = 'Could not create a rendering context';
  glsWrongVersion       = 'Need at least OpenGL version 1.1';
  glsTooManyLights      = 'Too many lights in the scene';
  glsDisplayList        = 'Failed to create a new display list for object ''%s''';
  glsWrongBitmapCanvas  = 'Couldn''t create a rendering context for the given bitmap';
  glsWrongPrinter       = 'Couldn''t render to printer';
  glsAlreadyRendering   = 'Already rendering';

  // GLTree
  glsSceneRoot  = 'Scene root';
  glsObjectRoot = 'Scene objects';
  glsCameraRoot = 'Cameras';
  glsCamera     = 'Camera';

  // GLTexture
  glsImageInvalid = 'Could not load texture, image is invalid';
  glsNoNewTexture = 'Could not get new texture name';

  // GLObjects
  glsSphereTopBottom = 'The top angle must be higher than the bottom angle';
  glsSphereStartStop = 'The start angle must be smaller than then stop angle';
  glsMaterialNotFound = 'Loading failed: could not find material %s';
  glsInterleaveNotSupported = 'Interleaved Array format not supported yet. Sorry.';

  // common messages
  glsOutOfMemory = 'Fatal: Out of memory';
  glsFailedOpenFile = 'Could not open file: %s';
  glsNoDescriptionAvailable = 'No description available';
  glsUnBalancedBeginEndUpdate = 'Unbalanced Begin/EndUpdate';
  glsUnknownExtension = 'Unknown file extension (%s), maybe you forgot to add the support '
                       +'unit to your uses? (%s?)' ;

  // object categories
  glsOCBasicGeometry = 'Basic geometry';
  glsOCAdvancedGeometry = 'Advanced geometry';
  glsOCMeshObjects = 'Mesh objects';
  glsOCParticleSystems = 'Particle systems';
  glsOCEnvironmentObjects = 'Environment objects';
  glsOCSpecialObjects = 'Special objects';
  glsOCGraphPlottingObjects = 'Graph-plotting objects';
  glsOCDoodad = 'Doodad objects';
  glsOCHUDObjects = 'HUD objects';
  glsOCGuiObjects = 'GUI objects';

implementation

end.

