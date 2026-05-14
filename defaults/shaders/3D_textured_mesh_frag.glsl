#version 330 core
in vec2 tex_coord;
in vec3 norm;
out vec4 frag_color;

uniform sampler2D texture1;
uniform float time;

void main() {
    frag_color = texture(texture1, tex_coord);
}