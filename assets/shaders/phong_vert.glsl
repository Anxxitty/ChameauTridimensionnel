#version 330 core
layout (location = 0) in vec4 pos;
layout (location = 1) in vec2 i_tex_coord;
layout (location = 2) in vec3 i_norm;
out vec2 tex_coord;
out vec3 norm;
out vec3 frag_pos;
out vec3 o_light_pos;
uniform vec3 light_pos;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform float time;
void main() {
    gl_Position = projection_matrix * view_matrix * model_matrix * pos;
    tex_coord = vec2(i_tex_coord.x, 1 - i_tex_coord.y);
    norm = normalize(mat3(view_matrix) * mat3(model_matrix) * i_norm);
    o_light_pos = mat3(view_matrix) * light_pos;
    frag_pos = vec3(view_matrix * model_matrix * pos);
}