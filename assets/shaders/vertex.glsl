#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aCol;
out vec4 color;
uniform mat4 localRotation;
uniform mat4 scale;
uniform mat4 sceneCoordinates;
uniform mat4 cameraTranslation;
uniform mat4 projectionMatrix;
void main() {
    gl_Position = projectionMatrix *  cameraTranslation * sceneCoordinates * scale * localRotation * vec4(aPos, 1.0);
    color = aCol;
}