varying vec3 position;
varying vec3 n, e;

uniform sampler2D diffuseMap;
uniform sampler2D normalMap;
uniform int maxNumLights;

mat3 cotangent_frame(vec3 N, vec3 p, vec2 uv)
{
  // get edge vectors of the pixel triangle
  vec3 dp1 = dFdx(p);
  vec3 dp2 = dFdy(p);
  vec2 duv1 = dFdx(uv);
  vec2 duv2 = dFdy(uv);
  
  // solve the linear system
  vec3 dp2perp = cross(dp2, N);
  vec3 dp1perp = cross(N, dp1);
  vec3 T = dp2perp * duv1.x + dp1perp * duv2.x;
  vec3 B = dp2perp * duv1.y + dp1perp * duv2.y;
  
  // construct a scale-invariant frame
  float invmax = inversesqrt(max(dot(T, T), dot(B, B)));
  return mat3(T * invmax, B * invmax, N);
}

void main()
{
    vec3 nn = normalize(n);
    vec3 E = normalize(e);
    
    vec2 texCoords = gl_TexCoord[0].st;

    vec4 diffuseTex = texture2D(diffuseMap, texCoords);
    vec3 tN = normalize(texture2D(normalMap, texCoords).rgb * 2.0 - 1.0);
    
    mat3 TBN = cotangent_frame(nn, -E, texCoords);
    vec3 N = normalize(TBN * tN);
    
    float diffuse;
    float specular;
    vec4 diffuseSum = vec4(0.0, 0.0, 0.0, 0.0);
    vec4 specularSum = vec4(0.0, 0.0, 0.0, 0.0);

    vec3 directionToLight;
    float distanceToLight;
    float attenuation = 1.0;
    vec3 H;
    vec3 L;
    float NH;
    
    for (int i = 0; i < maxNumLights; i++)
    {
        if (gl_LightSource[i].position.w > 0.0)
        {
            vec3 positionToLightSource = vec3(gl_LightSource[i].position.xyz - position);
            distanceToLight = length(positionToLightSource);
            directionToLight = normalize(positionToLightSource);
            attenuation = gl_LightSource[i].constantAttenuation / 
                    ((1.0+gl_LightSource[i].linearAttenuation*distanceToLight) * 
                     (1.0+gl_LightSource[i].quadraticAttenuation*distanceToLight*distanceToLight));
        }
        else
        {
            directionToLight = gl_LightSource[0].position.xyz;
            attenuation = 1.0;
        }
        
        L = directionToLight;
                  
        diffuse = clamp(dot(N, L), 0.0, 1.0);
    
        H = normalize(L + E);
        NH = dot(N, H);
        specular = pow(max(NH, 0.0), 3.0 * gl_FrontMaterial.shininess);
    
        diffuseSum += gl_LightSource[i].diffuse * diffuse * attenuation;
        specularSum += gl_LightSource[i].specular * specular * attenuation;
    }
    
    gl_FragColor =
        diffuseTex * gl_FrontMaterial.ambient +
        diffuseTex * gl_FrontMaterial.diffuse * diffuseSum +
        gl_FrontMaterial.specular * specularSum;

    gl_FragColor.a = 1.0;
}
