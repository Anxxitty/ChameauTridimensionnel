#version 330 core

layout (location = 0) in vec4 i_position;
layout (location = 1) in vec2 i_texture_coordinates;
layout (location = 2) in vec3 i_normal;

out Vertex {
    vec3 normal;
} vertex;

uniform mat4 view_matrix;
uniform mat4 model_matrix;
uniform mat4 normal_matrix;

void main() {
    vertex.normal = normalize(mat3(view_matrix) * mat3(normal_matrix) * i_normal);
    gl_Position = view_matrix * model_matrix * i_position;
}