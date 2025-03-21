// Get the canvas element
var canvas = document.getElementById('glcanvas');

// Set width and height
canvas.width = 3000;
canvas.height = 2000;

// Initialize WebGL context
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

// Function to get the current values from the UI and return them as an object
function getControlValues() {
    return {
        cameraPos: [
            parseFloat(document.getElementById('cameraX').value),
            parseFloat(document.getElementById('cameraY').value),
            parseFloat(document.getElementById('cameraZ').value)
        ],
        focalLength: parseFloat(document.getElementById('focalLength').value),
        cameraPolarizer: parseFloat(document.getElementById('cameraPolarizer').value),
        lightPos: [
            parseFloat(document.getElementById('lightX').value),
            parseFloat(document.getElementById('lightY').value),
            parseFloat(document.getElementById('lightZ').value)
        ],
        radiance: parseFloat(document.getElementById('radiance').value),
        alpha: parseFloat(document.getElementById('alpha').value),
    };
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

    // Get attribute and uniform locations
    var positionAttributeLocation = gl.getAttribLocation(program, 'position');
    var u_resolutionLocation = gl.getUniformLocation(program, 'u_resolution');
    var u_cameraPosLocation = gl.getUniformLocation(program, 'u_cameraPos');
    var u_focalLengthLocation = gl.getUniformLocation(program, 'u_focalLength');
    var u_cameraPolarizerLocation = gl.getUniformLocation(program, 'u_cameraPolarizer');
    var u_lightPosLocation = gl.getUniformLocation(program, 'u_lightPos');
    var u_radianceLocation = gl.getUniformLocation(program, 'u_lightRadiance');
    var u_alphaLocation = gl.getUniformLocation(program, 'u_alpha');
    var u_textureLocation = gl.getUniformLocation(program, 'u_texture');

    // Enable the attribute
    gl.enableVertexAttribArray(positionAttributeLocation);
    gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);

    // Function to update uniforms based on control values
    function updateUniforms() {
        var values = getControlValues();
        gl.uniform2f(u_resolutionLocation, canvas.width, canvas.height);
        gl.uniform3fv(u_cameraPosLocation, values.cameraPos);
        gl.uniform1f(u_focalLengthLocation, values.focalLength);
        gl.uniform1f(u_cameraPolarizerLocation, values.cameraPolarizer);
        gl.uniform3fv(u_lightPosLocation, values.lightPos);
        gl.uniform1f(u_radianceLocation, values.radiance);
        gl.uniform1f(u_alphaLocation, values.alpha);
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
    video.muted = true; // Mute to avoid feedback issues.

    // Request the camera stream.
    navigator.mediaDevices.getUserMedia({ video: true })
        .then(stream => {
            video.srcObject = stream;
            video.play();
        })
        .catch(err => {
            console.error("Error accessing camera: " + err);
        });


    let animationFrameId = null; // Track the current frame

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

    // Set up event listeners for UI controls.
    var controls = ['cameraX', 'cameraY', 'cameraZ', 'focalLength', 'cameraPolarizer', 'lightX', 'lightY', 'lightZ', 'radiance', 'alpha'];
    controls.forEach(function(controlId) {
        var control = document.getElementById(controlId);
        control.addEventListener('input', function() {
            // Render immediately on change.
            render();
        });
    });

    // Initial render
    render();

    // Download button functionality remains the same.
    var downloadButton = document.getElementById('downloadButton');
    downloadButton.addEventListener('click', function() {
        render();
        var dataURL = canvas.toDataURL('image/png');
        var link = document.createElement('a');
        link.href = dataURL;
        link.download = 'rendered_image.png';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    });
}).catch(error => {
    console.error(error);
});