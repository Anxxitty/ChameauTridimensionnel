#version 330 core
in vec2 tex_coord;
out vec4 frag_color;

uniform sampler2D tex;

void main() {
    frag_color = texture(tex, vec2(tex_coord.x, 1-tex_coord.y));
}