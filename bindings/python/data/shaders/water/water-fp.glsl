varying vec3 position;
varying vec3 worldPos;
varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;
varying vec3 eye;
varying vec3 eyeTan;

uniform sampler2D reflectionMap;
uniform sampler2D refractionMap;

uniform sampler2D positionMap;

uniform sampler2D normalMap1;
uniform sampler2D normalMap2;

uniform vec2 viewSize;
uniform float scrollTime;

uniform mat4 invViewMatrix;

const vec3 waterDeepColor = vec3(0.15, 0.15, 0.075); //vec3(0.1, 0.2, 0.2);
const vec3 waterScatterColor = waterDeepColor * 4.0;

const float flowSpeed = 0.1;
const float textureScale = 0.25;
const float waveAmplitude = 0.7;

const float uvDistortion = 0.025;

const float dispersion = 2.0;
const vec3 eta = vec3(1.0 + dispersion * 0.9, 1.0 + dispersion * 0.6, 1.0 + dispersion * 0.3);

const float fresnelPower = 3.0;
const float f0 = 0.05;

const float scatterDepth = 2.0;

const float sunScatterAmount = 3.5; //amount of sunlight scattering of waves
const vec3 sunScatterColor = vec3(0.0, 1.0, 0.95); // color of the sunlight scattering

void main()
{
    vec2 screenCoord = gl_FragCoord.xy / viewSize;
    
    vec3 dirToLight = normalize(gl_LightSource[0].position.xyz);         
    vec3 eyeN = normalize(normal);
    vec3 eyeT = normalize(tangent);
    vec3 eyeB = normalize(binormal);
    vec3 L = vec3(dot(dirToLight, eyeT),
                  dot(dirToLight, eyeB),
                  dot(dirToLight, eyeN));
                  
    vec3 E = normalize(eyeTan);
    vec3 H = normalize(L + E);
    
    vec2 uv = gl_TexCoord[0].st * textureScale;
    vec2 scrolluv1 = vec2(uv.s, uv.t + scrollTime * flowSpeed);
    vec2 scrolluv2 = vec2(uv.s + scrollTime * flowSpeed, uv.t);
    vec3 norm1 = normalize(texture2D(normalMap1, scrolluv1).rgb * 2.0 - 1.0);
    vec3 norm2 = normalize(texture2D(normalMap2, scrolluv2).rgb * 2.0 - 1.0);
    
    float fade = min(100.0, length(position)) / 100.0;
    vec3 N = mix(vec3(0.0, 0.0, 1.0), (norm1 + norm2) * 0.5, waveAmplitude);  
    N = mix(N, vec3(0.0, 0.0, 1.0), fade);
    
    vec2 noise = N.xy * uvDistortion;
    
    vec2 projCoord = screenCoord + noise;
    projCoord = clamp(projCoord, 0.0, 1.0);
    
    vec3 sunPos = vec3(gl_ModelViewMatrixInverse * gl_LightSource[0].position);
    vec3 sunext = vec3(0.45, 0.55, 0.68); //sunlight extinction
    float sunFade = clamp((sunPos.z+10.0)/20.0,0.0,1.0);

    vec3 R = reflect(-E, N);
      
    vec3 reflection = texture2D(reflectionMap, vec2(projCoord.x, 1.0 - projCoord.y)).rgb;
    
    vec2 noiseR = noise * eta.r;
    vec2 noiseG = noise * eta.g;
    vec2 noiseB = noise * eta.b;

    vec3 refraction;
    refraction.r = texture2D(refractionMap, clamp(screenCoord + noiseR, 0.0, 1.0)).r;
    refraction.g = texture2D(refractionMap, clamp(screenCoord + noiseG, 0.0, 1.0)).g;
    refraction.b = texture2D(refractionMap, clamp(screenCoord + noiseB, 0.0, 1.0)).b;
    
    float fresnel = f0 + pow(1.0 - dot(N, E), fresnelPower);
    
    vec3 wPosBottom = vec3(invViewMatrix * vec4(texture2D(positionMap, projCoord).xyz, 1.0)); // world position behind the water in this point
    float depthFactor = 1.0 - clamp(distance(worldPos, wPosBottom) / scatterDepth, 0.0, 1.0);
    
    vec3 luminosity = vec3(0.30, 0.59, 0.11);
    float reflectivity = pow(dot(luminosity, reflection.rgb * 2.0), 3.0);
    
    vec3 specColor = vec3(1.0, 1.0, 1.0);
    
    float NH = dot(N, H);
    float specular = float(max(NH, 0.0) > 0.99) * reflectivity;
       
    vec3 color = mix(waterDeepColor, refraction, depthFactor);
    
    color = mix(color, reflection, fresnel);

    gl_FragColor = vec4(vec3(color + (specColor * specular)), 1.0);
}
