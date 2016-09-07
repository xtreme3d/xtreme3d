bumpVertexProgram =
  'varying vec3 position;' + #13#10 +
  'varying vec3 n, t, b, e;' + #13#10 + 
  'varying vec4 shadowCoord;' + #13#10 + 
  'uniform mat4 shadowMatrix;' + #13#10 + 
  'uniform bool useShadowMap;' + #13#10 + 
  
  'void main()' + #13#10 +
  '{' + #13#10 +
  '  gl_TexCoord[0] = gl_MultiTexCoord0;' + #13#10 +
  
  '  n = normalize(gl_NormalMatrix * gl_Normal);' + #13#10 +
  '  t = normalize(gl_NormalMatrix * gl_MultiTexCoord2.xyz);' + #13#10 +
  '  b = cross(t, n);' + #13#10 +
  
  '  position = (gl_ModelViewMatrix * gl_Vertex).xyz;' + #13#10 +
  
  '  e = position;' + #13#10 +
  '  e.x = dot(position, t);' + #13#10 +
  '  e.y = dot(position, b);' + #13#10 +
  '  e.z = dot(position, n);' + #13#10 +
  '  e = -normalize(e);' + #13#10 +
  
  '  shadowCoord = useShadowMap? shadowMatrix * (gl_ModelViewMatrix * gl_Vertex) : vec4(0.0, 0.0, 0.0, 0.0);' + #13#10 +

  '  gl_Position = ftransform();' + #13#10 +
  '}' + #13#10;

bumpFragmentProgram =
  'varying vec3 position;' + #13#10 +
  'varying vec3 n, t, b, e;' + #13#10 +
  'varying vec4 shadowCoord;' + #13#10 +

  'uniform sampler2D diffuseMap;' + #13#10 +
  'uniform sampler2D normalMap;' + #13#10 +
  'uniform sampler2D heightMap;' + #13#10 +
  'uniform sampler2DShadow shadowMap;' + #13#10 +
  'uniform int maxNumLights;' + #13#10 +
  'uniform bool useParallax;' + #13#10 +
  'uniform float parallaxHeight;' + #13#10 +
  'uniform vec2 shadowMapSize;' + #13#10 +
  'uniform float shadowBlurRadius;' + #13#10 +

  'const float parallaxBias = -0.01;' + #13#10 +
  
  'float lookup(sampler2DShadow depths, vec4 coord, vec2 offset)' + #13#10 +
  '{' + #13#10 +
    'vec2 texelSize = 1.0 / shadowMapSize;' + #13#10 +
    'vec2 v = offset * texelSize * coord.w;' + #13#10 +
    'float z = shadow2DProj(depths, coord + vec4(v.x, v.y, 0.0, 0.0)).z;' + #13#10 +
    'return z;' + #13#10 +
  '}' + #13#10 +

  'void main()' + #13#10 +
  '{' + #13#10 +
    'vec3 nn = normalize(n);' + #13#10 +
    'vec3 tn = normalize(t);' + #13#10 +
    'vec3 bn = normalize(b);' + #13#10 +
    'vec3 E = normalize(e);' + #13#10 +
    
    'vec2 texCoords = gl_TexCoord[0].st;' + #13#10 +

    'if (useParallax)' + #13#10 +
    '{' + #13#10 +
      'float height = texture2D(heightMap, texCoords).r;'  + #13#10 +
      'height = height * parallaxHeight + parallaxBias;' + #13#10 +
      'texCoords = texCoords + (height * E.xy);' + #13#10 +
    '}' + #13#10 +
    
    'float shadow = 1.0;' + #13#10 +
    'if (shadowCoord.w > 0.0)' + #13#10 +
    '{' + #13#10 +
      'shadow = 0.0;' + #13#10 +
      'if (shadowBlurRadius > 0.0)' + #13#10 +
      '{' + #13#10 +
        'float x, y;' + #13#10 +
	      'for (y = -shadowBlurRadius ; y < shadowBlurRadius ; y += 1.0)' + #13#10 +
	      'for (x = -shadowBlurRadius ; x < shadowBlurRadius ; x += 1.0)' + #13#10 +
        '{' + #13#10 +
	        'shadow += lookup(shadowMap, shadowCoord, vec2(x, y));' + #13#10 +
        '}' + #13#10 +
	      'shadow /= shadowBlurRadius * shadowBlurRadius * 4.0;' + #13#10 +
      '}' + #13#10 +
      'else' + #13#10 +
        'shadow = lookup(shadowMap, shadowCoord, vec2(0.0, 0.0));' + #13#10 +
    '}' + #13#10 +

    'vec4 diffuseTex = texture2D(diffuseMap, texCoords);' + #13#10 +
    'vec3 N = normalize(texture2D(normalMap, texCoords).rgb * 2.0 - 1.0);' + #13#10 +
        
    'float diffuse;' + #13#10 +
    'float specular;' + #13#10 +
    'vec4 diffuseSum = vec4(0.0, 0.0, 0.0, 0.0);' + #13#10 +
    'vec4 specularSum = vec4(0.0, 0.0, 0.0, 0.0);' + #13#10 +

    'vec3 directionToLight;' + #13#10 +
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
    '    directionToLight = normalize(positionToLightSource);' + #13#10 +
    '    attenuation = gl_LightSource[i].constantAttenuation / ((1.0+gl_LightSource[i].linearAttenuation*distanceToLight) * (1.0+gl_LightSource[i].quadraticAttenuation*distanceToLight*distanceToLight));' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  {' + #13#10 +
    '    directionToLight = gl_LightSource[0].position.xyz;' + #13#10 +
    '    attenuation = 1.0;' + #13#10 +
    '  }' + #13#10 +
        
    '  L = vec3(dot(directionToLight, tn),' + #13#10 +
    '           dot(directionToLight, bn),' + #13#10 +
    '           dot(directionToLight, nn));' + #13#10 +
                  
    '  diffuse = clamp(dot(N, L), 0.0, 1.0);' + #13#10 +
    
    '  H = normalize(L + E);' + #13#10 +
    '  NH = dot(N, H);' + #13#10 +
    '  specular = pow(max(NH, 0.0), 3.0 * gl_FrontMaterial.shininess);' + #13#10 +
    
    '  diffuseSum += gl_LightSource[i].diffuse * diffuse * attenuation;' + #13#10 +
    '  specularSum += gl_LightSource[i].specular * specular * attenuation;' + #13#10 +
    '}'  + #13#10 +
    
    'gl_FragColor =' + #13#10 +
    '  diffuseTex * gl_FrontMaterial.ambient +' + #13#10 +
    '  diffuseTex * gl_FrontMaterial.diffuse * diffuseSum * shadow +' + #13#10 +
    '  gl_FrontMaterial.specular * specularSum * shadow;' + #13#10 +

    'gl_FragColor.a = 1.0;' + #13#10 +
  '}' + #13#10;
