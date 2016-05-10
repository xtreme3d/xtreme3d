#version 120
varying vec3 position;
varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;
varying vec3 eyeTan;
varying vec3 eye;
varying vec3 worldView;

uniform samplerCube texture0;
uniform sampler2D texture1;
uniform sampler2D texture2;
uniform mat4 viewMatrix;
uniform float scrollTime;
const vec4 waterColor = vec4(0.0, 0.05, 0.05, 1.0);
    
void main()
{
    vec3 dirToLight = normalize(gl_LightSource[0].position.xyz - position);                    
    vec3 eyeN = normalize(normal);
    vec3 eyeT = normalize(tangent);
    vec3 eyeB = normalize(binormal);
    vec3 L = vec3(dot(dirToLight, eyeT),
                  dot(dirToLight, eyeB),
                  dot(dirToLight, eyeN));
   
    vec2 scrolluv1 = vec2(gl_TexCoord[0].s, gl_TexCoord[0].t + scrollTime * 0.1);
    vec2 scrolluv2 = vec2(gl_TexCoord[0].s + scrollTime * 0.1, gl_TexCoord[0].t);
   
    vec3 norm1 = normalize(texture2D(texture1, scrolluv1).rgb * 2.0 - 1.0);
    vec3 norm2 = normalize(texture2D(texture1, scrolluv2).rgb * 2.0 - 1.0);
    vec3 norm = (norm1 + norm2) * 0.5;
    vec3 norm3 = vec3(0, 0, 1);
    vec3 N = mix(norm, norm3, 0.9);

    vec3 E = normalize(eyeTan);
    vec3 worldNormal = transpose(mat3(viewMatrix)) * N.xzy;
    vec3 R = reflect(normalize(worldView), worldNormal);
    R = normalize(vec3(-R.x, R.y, -R.z));

    vec3 H = normalize(L + E);

    float fresnel = 1.0 - max(dot(eye, eyeN), 0.0);

    float diffuse = clamp(dot(norm, L), 0.0, 1.0);
    float specular = pow(max(dot(H, norm), 0.0), 3.0 * gl_FrontMaterial.shininess);
    
    vec4 tex = textureCube(texture0, R); 
    vec4 reflectionTerm = tex * fresnel + waterColor * diffuse * (1.0 - fresnel);
   
    vec4 col =
     reflectionTerm +
     specular * gl_FrontMaterial.specular;
     
    col.a = max(0.9, fresnel);

    gl_FragColor = col;
}
