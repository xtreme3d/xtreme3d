phongVertexProgram =
  'varying vec3 position;' + #13#10 +
  'varying vec3 n, e;' + #13#10 + 
  
  'void main()' + #13#10 +
  '{' + #13#10 +
  '  gl_TexCoord[0] = gl_MultiTexCoord0;' + #13#10 +
  '  n = normalize(gl_NormalMatrix * gl_Normal);' + #13#10 +
  '  position = (gl_ModelViewMatrix * gl_Vertex).xyz;' + #13#10 +
  '  e = -normalize(position);' + #13#10 +
  '  gl_Position = ftransform();' + #13#10 +
  '  gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;' + #13#10 +
  '}' + #13#10;

phongFragmentProgram =
  'varying vec3 position;' + #13#10 +
  'varying vec3 n, e;' + #13#10 +

  'uniform sampler2D diffuseMap;' + #13#10 +
  'uniform int maxNumLights;' + #13#10 +
  'uniform bool useTexture;' + #13#10 +

  'void main()' + #13#10 +
  '{' + #13#10 +
    'vec3 N = normalize(n);' + #13#10 +
    'vec3 E = normalize(e);' + #13#10 +
    
    'vec2 texCoords = gl_TexCoord[0].st;' + #13#10 +

    'vec4 diffuseTex = useTexture? texture2D(diffuseMap, texCoords) : vec4(1, 1, 1, 1);' + #13#10 +
        
    'float diffuse;' + #13#10 +
    'float specular;' + #13#10 +
    'vec4 diffuseSum = vec4(0.0, 0.0, 0.0, 0.0);' + #13#10 +
    'vec4 specularSum = vec4(0.0, 0.0, 0.0, 0.0);' + #13#10 +

    'float distanceToLight;' + #13#10 +
    'float attenuation = 1.0;'  + #13#10 +
    'vec3 H;' + #13#10 +
    'vec3 L;' + #13#10 +
    'float NH;' + #13#10 +
    
    'for (int i = 0; i < maxNumLights; i++)' + #13#10 +
    '{' + #13#10 +
    '  if (gl_LightSource[i].position.w > 0.0)' + #13#10 +
    '  {' + #13#10 +
    '    vec3 positionToLightSource = vec3(gl_LightSource[i].position.xyz - position);' + #13#10 +
    '    distanceToLight = length(positionToLightSource);' + #13#10 +
    '    L = normalize(positionToLightSource);' + #13#10 +
    '    attenuation = gl_LightSource[i].constantAttenuation / ((1.0+gl_LightSource[i].linearAttenuation*distanceToLight) * (1.0+gl_LightSource[i].quadraticAttenuation*distanceToLight*distanceToLight));' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  {' + #13#10 +
    '    L = gl_LightSource[0].position.xyz;' + #13#10 +
    '    attenuation = 1.0;' + #13#10 +
    '  }' + #13#10 +
                  
    '  diffuse = clamp(dot(N, L), 0.0, 1.0);' + #13#10 +
    
    '  H = normalize(L + E);' + #13#10 +
    '  NH = dot(N, H);' + #13#10 +
    '  specular = pow(max(NH, 0.0), 3.0 * gl_FrontMaterial.shininess);' + #13#10 +
    
    '  diffuseSum += gl_LightSource[i].diffuse * diffuse * attenuation;' + #13#10 +
    '  specularSum += gl_LightSource[i].specular * specular * attenuation;' + #13#10 +
    '}'  + #13#10 +
    
    'gl_FragColor =' + #13#10 +
    '  diffuseTex * gl_FrontMaterial.ambient +' + #13#10 +
    '  diffuseTex * gl_FrontMaterial.diffuse * diffuseSum +' + #13#10 +
    '  gl_FrontMaterial.specular * specularSum;' + #13#10 +

    'gl_FragColor.a = 1.0;' + #13#10 +
  '}' + #13#10;
