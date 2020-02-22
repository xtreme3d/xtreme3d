varying vec3 position;
varying vec3 n, e;

varying vec4 shadowCoord1;
varying vec4 shadowCoord2;
varying vec4 shadowCoord3;

uniform sampler2D texture;

uniform sampler2DShadow shadowMap1;
uniform sampler2DShadow shadowMap2;
uniform sampler2DShadow shadowMap3;

uniform vec2 shadowMapSize; 

uniform bool usePCF;
uniform bool drawCascades;

float shadowLookup(sampler2DShadow depths, vec4 coord, vec2 offset)
{
    vec2 texelSize = 1.0 / shadowMapSize;
    vec2 v = offset * texelSize * coord.w;
    float z = shadow2DProj(depths, coord + vec4(v.x, v.y, 0.0, 0.0)).z;
    return z;
}

float weight(vec4 tc)
{
    vec2 proj = vec2(tc.x / tc.w, tc.y / tc.w);
    proj = (1.0 - abs(proj * 2.0 - 1.0)) * 8.0;
    proj = clamp(proj, 0.0, 1.0);
    return min(proj.x, proj.y);
}

float pcf(sampler2DShadow depths, vec4 coord, float radius)
{
    float s = 0.0;
    float x, y;
	for (y = -radius ; y < radius ; y += 1.0)
	for (x = -radius ; x < radius ; x += 1.0)
    {
	    s += shadowLookup(depths, coord, vec2(x, y));
    }
	s /= radius * radius * 4.0;
    return s;
}

const vec4 c_white = vec4(1.0, 1.0, 1.0, 1.0);
const vec4 c_red   = vec4(1.0, 0.0, 0.0, 1.0);
const vec4 c_green = vec4(0.0, 1.0, 0.0, 1.0);
const vec4 c_blue  = vec4(0.0, 0.0, 1.0, 1.0);
    
void main()
{
    vec4 tex = texture2D(texture, gl_TexCoord[0].st);
    
    float s1, s2, s3;

    if (usePCF)
    {
        s1 = pcf(shadowMap1, shadowCoord1, 3.0);
        s2 = pcf(shadowMap2, shadowCoord2, 2.0);
        s3 = pcf(shadowMap3, shadowCoord3, 2.0);
    }
    else
    {
        s1 = shadow2DProj(shadowMap1, shadowCoord1).z;
        s2 = shadow2DProj(shadowMap2, shadowCoord2).z;
        s3 = shadow2DProj(shadowMap3, shadowCoord3).z;
    }

    float w1 = weight(shadowCoord1);
    float w2 = weight(shadowCoord2);
    float w3 = weight(shadowCoord3);
    
    s3 = mix(1.0, s3, w3);
    s2 = mix(s3, s2, w2);
    s1 = mix(s2, s1, w1);
    
    // remove this if you don't need cascade visualization
    vec4 c3 = mix(c_white, c_blue, w3);
    vec4 c2 = mix(c3, c_green, w2);
    vec4 c1 = mix(c2, c_red, w1);
    
    vec3 N = normalize(n);
    vec3 E = normalize(e);
    vec3 L = gl_LightSource[0].position.xyz;
    float dirDiffBrightness = clamp(dot(N, L), 0.0, 1.0);
    
    // TODO: light cycle
    
    vec3 positionToLightSource = vec3(gl_LightSource[1].position.xyz - position);
    float distanceToLight = length(positionToLightSource);
    vec3 Lp = normalize(positionToLightSource);
    float attenuation =
      gl_LightSource[1].constantAttenuation /
      ((1.0+gl_LightSource[1].linearAttenuation * distanceToLight) *
      (1.0+gl_LightSource[1].quadraticAttenuation * distanceToLight * distanceToLight));
    vec4 pointDiffSum = (gl_LightSource[1].ambient + gl_LightSource[1].diffuse * clamp(dot(N, Lp), 0.0, 1.0)) * attenuation;
    
    float fogDistance = gl_FragCoord.z / gl_FragCoord.w;
    float fogFactor = clamp((gl_Fog.end - fogDistance) / (gl_Fog.end - gl_Fog.start), 0.0, 1.0);
    
    const vec4 ambientColor = vec4(0.3, 0.25, 0.25, 1.0);
    const vec4 sunColor = vec4(0.8, 0.7, 0.7, 1.0);
    
    vec4 objColor = tex * (ambientColor + pointDiffSum + sunColor * dirDiffBrightness * s1);
    
    vec4 color = drawCascades? c1 * s1 : mix(gl_Fog.color, objColor, fogFactor);
    
    gl_FragColor = color;
    gl_FragColor.a = 1.0;
}
