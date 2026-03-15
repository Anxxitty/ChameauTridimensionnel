#version 330 core
layout (location = 0) in vec4 aPos;
out vec4 color;
uniform mat4 modelMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;
void main() {
    gl_Position = projectionMatrix *  viewMatrix * modelMatrix * aPos;
    color = vec4(1.0, 0.0, 0.0, 0.0);
}