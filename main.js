// Get the canvas element and set resolution to the full viewport width (square)
var canvas = document.getElementById('glcanvas');
canvas.width = window.innerWidth;
canvas.height = window.innerWidth; // Ensures a square resolution

var gl = canvas.getContext('webgl2');
if (!gl) {
    alert("Unable to initialize WebGL. Your browser may not support it.");
}

// Vertex shader source code
var vertexShaderSource = 
    `#version 300 es
    precision highp float;
    in vec4 position;
    void main() {
        gl_Position = position;
    }
`;

// Function to load shader source code from a file
function loadShaderSource(url) {
    return fetch(url).then(response => {
        if (!response.ok) {
            throw new Error('Could not load shader from ' + url);
        }
        return response.text();
    });
}

// Function to compile a shader
function createShader(gl, type, source) {
    var shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    var success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
    if (!success) {
        console.error('Shader compilation failed: ', gl.getShaderInfoLog(shader));
        gl.deleteShader(shader);
        return null;
    }
    return shader;
}

// Function to create a shader program
function createProgram(gl, vertexShader, fragmentShader) {
    var program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    var success = gl.getProgramParameter(program, gl.LINK_STATUS);
    if (!success) {
        console.error('Program failed to link: ', gl.getProgramInfoLog(program));
        gl.deleteProgram(program);
        return null;
    }
    return program;
}

// ----------------------------------------------------
// Global variables for pupil movement
// ----------------------------------------------------
var pupilRadius = 8;
var corneaRadius = 14.5;
var pupilPos = { x: 0, y: 0 };
var pupilVel = { x: 0, y: 0 };
var tiltScale = 0.015;           // scale for tilt-based force
var accelerometerScale = 0.05;   // scale for accelerometer-based force
var friction = 0.7;              // friction factor < 1 => velocity decays

// ----------------------------------------------------
// Global variables for camera movement
// ----------------------------------------------------
var cameraRadius = 100;                // Fixed distance from the object
const initCameraPos = [0, cameraRadius, 0];   // Start above the object
var cameraPos = initCameraPos;   // Start above the object

// ----------------------------------------------------
// getControlValues now returns non-camera controls only.
// ----------------------------------------------------
function getControlValues() {
    return {
        focalLength: parseFloat(document.getElementById('focalLength').value),
        lightPos: [
            parseFloat(document.getElementById('lightX').value),
            parseFloat(document.getElementById('lightY').value),
            parseFloat(document.getElementById('lightZ').value)
        ],
        radiance: parseFloat(document.getElementById('radiance').value),
    };
}

// ----------------------------------------------------
// Device orientation handler: update both pupil and camera positions.
// ----------------------------------------------------

function createRotationX(theta) {
    var cosT = Math.cos(theta);
    var sinT = Math.sin(theta);
    return [
        [1,     0,      0],
        [0,  cosT,  -sinT],
        [0,  sinT,   cosT]
    ];
}

function createRotationZ(theta) {
    var cosT = Math.cos(theta);
    var sinT = Math.sin(theta);
    return [
        [cosT, -sinT,  0],
        [sinT,  cosT,  0],
        [0,  0,        1]
    ];
}

function multiplyMatrixVector(matrix, vector) {
    var result = [0, 0, 0];
    for (var i = 0; i < 3; i++) {
        result[i] = matrix[i][0] * vector[0] +
                    matrix[i][1] * vector[1] +
                    matrix[i][2] * vector[2];
    }
    return result;
}

function handleDeviceOrientation(e) {
    var ay = e.gamma * tiltScale;
    var ax = -e.beta  * tiltScale;
    pupilVel.x += ax;
    pupilVel.y += ay;
    pupilVel.x *= friction;
    pupilVel.y *= friction;
    pupilPos.x += pupilVel.x;
    pupilPos.y += pupilVel.y;
    var maxMove = corneaRadius - pupilRadius;
    var dist = Math.sqrt(pupilPos.x * pupilPos.x + pupilPos.y * pupilPos.y);
    if (dist > maxMove) {
        var ratio = maxMove / dist;
        pupilPos.x *= ratio;
        pupilPos.y *= ratio;
    }
    
    var phi   = e.gamma * Math.PI / 180; // horizontal angle
    var theta = e.beta  * Math.PI / 180; // vertical angle
    var maxPhi = Math.PI;
    phi = Math.max(-maxPhi, Math.min(maxPhi, phi));
    var maxTheta = Math.PI / 2.5;
    theta = Math.max(-maxTheta, Math.min(maxTheta, theta));


    var rotX = createRotationX(-phi);
    var rotZ = createRotationZ(-theta);

    cameraPos = multiplyMatrixVector(rotX, initCameraPos);
    cameraPos = multiplyMatrixVector(rotZ, cameraPos);
    console.log(cameraPos);
}

// For mobile devices with tilt:
var isMobile = /Android|iPhone|iPad|iPod|BlackBerry|Windows Phone/i.test(navigator.userAgent);
if (isMobile && window.DeviceOrientationEvent) {
    if (typeof DeviceOrientationEvent.requestPermission === 'function') {
        var button = document.createElement('button');
        button.textContent = 'Enable tilt control';
        button.style.position = 'absolute';
        button.style.zIndex = '9999';
        button.style.top = '10px';
        button.style.left = '10px';
        document.body.appendChild(button);
        button.addEventListener('click', function() {
            DeviceOrientationEvent.requestPermission().then(function(permissionState) {
                if (permissionState === 'granted') {
                    window.addEventListener('deviceorientation', handleDeviceOrientation);
                    document.body.removeChild(button);
                } else {
                    alert('Permission not granted for DeviceOrientation');
                }
            }).catch(function(err) {
                console.error(err);
            });
        });
    } else {
        window.addEventListener('deviceorientation', handleDeviceOrientation);
    }
} else {
    // For desktop: Use mouse movement to simulate tilt.
    canvas.addEventListener('mousemove', function(e) {
        var rect = canvas.getBoundingClientRect();
        var mouseX = e.clientX - rect.left;
        var mouseY = e.clientY - rect.top;

        // ----- Update pupil position (existing mouse logic) -----
        var ndcX = (mouseX / canvas.width ) * 2.0 - 1.0;
        var ndcY = (mouseY / canvas.height) * 2.0 - 1.0;
        var maxMove = corneaRadius - pupilRadius;
        var rawX = ndcX * maxMove;
        var rawY = ndcY * maxMove;
        var dist = Math.sqrt(rawX * rawX + rawY * rawY);
        if (dist > maxMove) {
            rawX = (rawX / dist) * maxMove;
            rawY = (rawY / dist) * maxMove;
        }
        pupilPos.x = rawX;
        pupilPos.y = rawY;
    });
}

// Load the fragment shader and set up the rendering
loadShaderSource('fragment.glsl').then(fragmentShaderSource => {
    var vertexShader = createShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
    var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);
    var program = createProgram(gl, vertexShader, fragmentShader);
    if (!program) { return; }

    // Set up position buffer for a full-screen quad
    var positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    var positions = new Float32Array([
        -1.0, -1.0,
         1.0, -1.0,
        -1.0,  1.0,
         1.0,  1.0
    ]);
    gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);

    // Get attribute/uniform locations
    var positionAttributeLocation = gl.getAttribLocation(program, 'position');
    var u_resolutionLocation      = gl.getUniformLocation(program, 'u_resolution');
    var u_texResolutionLocation   = gl.getUniformLocation(program, 'u_texResolution');
    var u_cameraPosLocation       = gl.getUniformLocation(program, 'u_cameraPos');
    var u_focalLengthLocation     = gl.getUniformLocation(program, 'u_focalLength');
    var u_lightPosLocation        = gl.getUniformLocation(program, 'u_lightPos');
    var u_radianceLocation        = gl.getUniformLocation(program, 'u_lightRadiance');
    var u_textureLocation         = gl.getUniformLocation(program, 'u_texture');
    var u_pupilPosLocation        = gl.getUniformLocation(program, 'u_pupilPos');
    var u_pupilRadiusLocation     = gl.getUniformLocation(program, 'u_pupilRadius');
    var u_corneaRadiusLocation    = gl.getUniformLocation(program, 'u_corneaRadius');

    gl.enableVertexAttribArray(positionAttributeLocation);
    gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);

    // Update uniforms – pass in both camera and pupil data.
    function updateUniforms() {
        var values = getControlValues();
        gl.uniform2f(u_resolutionLocation, canvas.width, canvas.height);
        gl.uniform3fv(u_cameraPosLocation, cameraPos);
        gl.uniform1f(u_focalLengthLocation, values.focalLength);
        gl.uniform3fv(u_lightPosLocation, values.lightPos);
        gl.uniform1f(u_radianceLocation, values.radiance);

        gl.uniform2f(u_pupilPosLocation, pupilPos.x, pupilPos.y);
        gl.uniform1f(u_pupilRadiusLocation, pupilRadius);
        gl.uniform1f(u_corneaRadiusLocation, corneaRadius);

        var texWidth = video.videoWidth || canvas.width;
        var texHeight = video.videoHeight || canvas.height;
        gl.uniform2f(u_texResolutionLocation, texWidth, texHeight);
    }

    // Create a texture object for the video feed.
    var texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texture);
    // Initialize with a temporary pixel.
    gl.texImage2D(
        gl.TEXTURE_2D, 0, gl.RGBA, 1, 1, 0,
        gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array([255, 255, 255, 255])
    );
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);

    // Create a video element to capture the camera feed.
    var video = document.createElement('video');
    video.autoplay = true;
    video.playsInline = true;
    video.muted = true;

    // Request the camera stream.
    navigator.mediaDevices.getUserMedia({ video: true })
        .then(stream => {
            video.srcObject = stream;
            video.play();
        })
        .catch(err => {
            console.error("Error accessing camera: " + err);
        });

    let animationFrameId = null;

    function render() {
        if (animationFrameId) {
            cancelAnimationFrame(animationFrameId);
        }
        animationFrameId = requestAnimationFrame(render);

        if (video.readyState >= video.HAVE_CURRENT_DATA) {
            gl.bindTexture(gl.TEXTURE_2D, texture);
            gl.texImage2D(
                gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA,
                gl.UNSIGNED_BYTE, video
            );
            gl.generateMipmap(gl.TEXTURE_2D);
        }

        gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
        gl.useProgram(program);
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        gl.enableVertexAttribArray(positionAttributeLocation);
        gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);

        updateUniforms();

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.uniform1i(u_textureLocation, 0);

        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    }

    // UI controls for the remaining values
    var controls = ['focalLength', 'lightX', 'lightY', 'lightZ', 'radiance'];
    controls.forEach(function(controlId) {
        var control = document.getElementById(controlId);
        control.addEventListener('input', function() {
            render();
        });
    });

    // Initial render
    render();
}).catch(error => {
    console.error(error);
});