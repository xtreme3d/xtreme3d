uniform sampler2D fboColor;

void main(void)
{
    gl_FragColor = texture2D(fboColor, gl_TexCoord[0].st);
}
