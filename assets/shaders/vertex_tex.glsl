#version 330 core
layout (location = 0) in vec4 pos;
layout (location = 1) in vec2 i_tex_coord;
out vec2 tex_coord;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
void main() {
    gl_Position = projection_matrix *  view_matrix * model_matrix * pos;
    tex_coord = i_tex_coord;
}