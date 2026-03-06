#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aCol;
out vec4 color;
uniform mat4 translationMatrix;
uniform mat4 projectionMatrix;
uniform mat4 rotationMatrix1;
uniform mat4 rotationMatrix2;
uniform mat4 scaleMatrix;
void main() {
    gl_Position = projectionMatrix * translationMatrix * scaleMatrix * rotationMatrix2 * rotationMatrix1 * vec4(aPos, 1.0);
    color = aCol;
}