#version 330 core
in vec2 tex_coord;
in vec3 norm;
out vec4 frag_color;

uniform sampler2D texture1;
uniform float time;

void main() {
    frag_color = vec4(0.0,1.0,0.0,0.2);
}