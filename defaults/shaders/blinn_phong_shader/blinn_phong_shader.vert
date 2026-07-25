#version 330 core

layout (location = 0) in vec4 i_position;
layout (location = 1) in vec2 i_texture_coordinates;
layout (location = 2) in vec3 i_normal;

out Vertex {
    vec2 texture_coordinates;
    vec3 normal;
    vec3 fragment_position;
} vertex;

uniform mat4 model_matrix;
uniform mat4 normal_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform float time;

void main() {
    gl_Position = projection_matrix * view_matrix * model_matrix * i_position;

    vertex.texture_coordinates = vec2(i_texture_coordinates.x, 1 - i_texture_coordinates.y);
    vertex.normal = normalize(mat3(view_matrix) * mat3(normal_matrix) * i_normal);
    vertex.fragment_position = vec3(view_matrix * model_matrix * i_position);
}