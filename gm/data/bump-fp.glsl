varying vec3 position;
varying vec3 n, t, b, e;

uniform sampler2D diffuseMap;
uniform sampler2D normalMap;

void main()
{
    vec3 nn = normalize(n);
    vec3 tn = normalize(t);
    vec3 bn = normalize(b);
    vec3 E = normalize(e);
    
    vec4 diffuseTex = texture2D(diffuseMap, gl_TexCoord[0].st);
    vec3 N = normalize(texture2D(normalMap, gl_TexCoord[0].st).rgb * 2.0 - 1.0);
    
    vec3 directionToLight = normalize(gl_LightSource[0].position.xyz - position);
    vec3 L = vec3(dot(directionToLight, tn),
                  dot(directionToLight, bn),
                  dot(directionToLight, nn));
                  
    float diffuse = clamp(dot(N, L), 0.0, 1.0);
    
    vec3 R = normalize(-reflect(L, N));
    vec3 H = normalize(L + E);
    float NH = dot(N, H);
    float specular = pow(max(NH, 0.0), 3.0 * gl_FrontMaterial.shininess); 
        
    gl_FragColor = 
        diffuseTex * gl_FrontMaterial.ambient + 
        diffuseTex * gl_FrontMaterial.diffuse * diffuse + 
        gl_FrontMaterial.specular * specular;
    gl_FragColor.a = 1.0;
}
