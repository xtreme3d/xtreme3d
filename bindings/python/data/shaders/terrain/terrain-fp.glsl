uniform sampler2D texture1;
uniform sampler2D texture2;
uniform sampler2D mask;
uniform sampler2D normalmap;
uniform mat4 viewMatrix;

varying vec3 position;
varying vec3 positionWorld;

float luminance(vec3 color)
{
    return (
        color.x * 0.27 +
        color.y * 0.67 +
        color.z * 0.06
    );
}

vec3 blend(vec3 t1, vec3 t2, float t)
{
    float a1 = (luminance(t1) > t)? 0.0 : 1.0;
    t = (t < 1.0 && t > 0.0)? mix(0.0, a1, t) : t;
    return mix(t1, t2, t);
}

vec3 slopeMapping(sampler2D texture1, sampler2D texture2, vec2 texcoords, vec3 normal)
{
    vec3 c1 = texture2D(texture1, texcoords).rgb;
    vec3 c2 = texture2D(texture2, texcoords).rgb;
    float slope = clamp(dot(normal, vec3(0, 1, 0)), 0.3, 0.7);
    slope = (0.7 - slope) / (0.7 - 0.3);
    return blend(c1, c2, 1.0 - slope);
}

vec3 fresnel(float cosTheta, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(1.0 - cosTheta, 5.0);
}

void main()
{
    vec3 E = normalize(-position);
    vec2 texcoords = gl_TexCoord[0].st;
    vec2 tiledTexcoords = texcoords * 100.0;
    
    vec3 nWorld = normalize(texture2D(normalmap, texcoords).rgb * 2.0 - 1.0).xzy;
    vec3 N = mat3(viewMatrix) * nWorld;
    
    vec3 L = gl_LightSource[0].position.xyz;
    float ambient = 0.5;
    float diffuse = clamp(dot(N, L), 0.0, 1.0);
    
    vec3 f = fresnel(max(dot(N, E), 0.0), 0.0);
    
    vec3 comb = slopeMapping(texture1, texture2, tiledTexcoords, nWorld);
    
    vec3 finalColor = comb * (ambient + diffuse) + vec3(1.0) * f * diffuse * 0.2; 
    
    gl_FragColor = vec4(finalColor, 1.0);
}
