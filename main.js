/************************************
 *  main.js – WebGL + Face Tracking (No IPD Input)
 ************************************/

// =======================
// Global Variables
// =======================
var canvas = document.getElementById('glcanvas');
canvas.width = 3000;
canvas.height = 2000;

var gl = canvas.getContext('webgl2');
if (!gl) {
    alert("Unable to initialize WebGL2. Your browser may not support it.");
}

// Face tracking variables:
let video;       // The single video element for both WebGL texture & FaceMesh
let faceMesh, mpCamera;
let facePoints = [];
// Fixed IPD in meters (no input from user):
const ipdInMeters = 0.065;

// We'll store a computed user face position & normal, used as a "camera"
let userFacePos    = [0, 0, 500];
let userFaceNormal = [0, 0, -1];

// Mediapipe FaceMesh constants for iris
const LEFT_IRIS_IDX = 468;
const RIGHT_IRIS_IDX = 473;

/************************************
 * FaceMesh Initialization & Callbacks
 ************************************/
function initFaceMesh() {
    faceMesh = new FaceMesh({
        locateFile: (file) => `https://cdn.jsdelivr.net/npm/@mediapipe/face_mesh/${file}`
    });
    faceMesh.setOptions({
        maxNumFaces: 1,
        refineLandmarks: true,
        minDetectionConfidence: 0.5,
        minTrackingConfidence: 0.5
    });
    faceMesh.onResults(onFaceResults);

    // Use the same video element for the Mediapipe Camera
    mpCamera = new Camera(video, {
        onFrame: async () => {
            await faceMesh.send({ image: video });
        },
        width: 640,
        height: 480,
    });
    mpCamera.start();
}

// Called by FaceMesh on each detection
function onFaceResults(results) {
    if (results.multiFaceLandmarks && results.multiFaceLandmarks.length > 0) {
        facePoints = results.multiFaceLandmarks[0];
    } else {
        facePoints = [];
    }
    if (facePoints.length > 0) {
        computeFacePositionAndNormal();
    }
}

// Approximate 3D face position & normal from FaceMesh landmarks
function computeFacePositionAndNormal() {
    // 1) Compute the average (cxNorm, cyNorm) in [0..1]
    let sumX = 0, sumY = 0;
    for (let pt of facePoints) {
        sumX += pt.x;
        sumY += pt.y;
    }
    let cxNorm = sumX / facePoints.length;
    let cyNorm = sumY / facePoints.length;

    // 2) Iris distance in normalized coords
    let leftIris  = facePoints[LEFT_IRIS_IDX];
    let rightIris = facePoints[RIGHT_IRIS_IDX];
    let measuredIrisDist = 0.06; // fallback
    if (leftIris && rightIris) {
        measuredIrisDist = dist2D(leftIris, rightIris);
    }

    // 3) Approx. faceZ from IPD
    let faceZ = ipdInMeters / measuredIrisDist;

    // 4) Convert the normalized center to some 3D coordinate system
    let scaleXY = 400;  // tweak to taste
    let x3 = (cxNorm - 0.5) * scaleXY;
    let y3 = (cyNorm) * -scaleXY + 500;
    userFacePos = [ x3, y3, faceZ * 1000 ];
    // console.log(userFacePos);

    // 5) Nose tip (#1) → normal
    let noseTip = facePoints[1];
    if (noseTip) {
        let nx = noseTip.x - cxNorm;
        let ny = noseTip.y - cyNorm;
        let n3 = [ nx, -ny, 0 ];
        normalizeInPlace(n3);
        userFaceNormal = n3;
    }
}

function dist2D(a, b) {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    return Math.sqrt(dx*dx + dy*dy);
}

function normalizeInPlace(v) {
    let len = Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
    if (len > 1e-7) {
        v[0]/=len; v[1]/=len; v[2]/=len;
    }
}

/************************************
 * Shader Setup
 ************************************/
var vertexShaderSource = `#version 300 es
precision highp float;
in vec4 position;
void main() {
    gl_Position = position;
}
`;

// Loads external shader text via fetch
function loadShaderSource(url) {
    return fetch(url).then(response => {
        if (!response.ok) {
            throw new Error('Could not load shader from ' + url);
        }
        return response.text();
    });
}

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

/************************************
 * UI Control Values (No IPD)
 ************************************/
function getControlValues() {
    // We keep the other controls, but no IPD
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

/************************************
 * Main: Load Fragment Shader & Start
 ************************************/
loadShaderSource('fragment.glsl').then(fragmentShaderSource => {
    let vertexShader   = createShader(gl, gl.VERTEX_SHADER,   vertexShaderSource);
    let fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);
    let program        = createProgram(gl, vertexShader, fragmentShader);
    if (!program) return;

    // Full-screen quad
    let positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    let positions = new Float32Array([
        -1.0, -1.0,
         1.0, -1.0,
        -1.0,  1.0,
         1.0,  1.0
    ]);
    gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);

    // Attribute & Uniform locations
    let positionAttrLoc   = gl.getAttribLocation(program, 'position');
    let u_resolutionLoc   = gl.getUniformLocation(program, 'u_resolution');
    let u_cameraPosLoc    = gl.getUniformLocation(program, 'u_cameraPos');
    let u_focalLenLoc     = gl.getUniformLocation(program, 'u_focalLength');
    let u_camPolarLoc     = gl.getUniformLocation(program, 'u_cameraPolarizer');
    let u_lightPosLoc     = gl.getUniformLocation(program, 'u_lightPos');
    let u_radianceLoc     = gl.getUniformLocation(program, 'u_lightRadiance');
    let u_alphaLoc        = gl.getUniformLocation(program, 'u_alpha');
    let u_textureLoc      = gl.getUniformLocation(program, 'u_texture');

    // Add a uniform for camera/face normal
    let u_cameraNormalLoc = gl.getUniformLocation(program, 'u_cameraNormal');

    // Enable vertex attribute
    gl.enableVertexAttribArray(positionAttrLoc);
    gl.vertexAttribPointer(positionAttrLoc, 2, gl.FLOAT, false, 0, 0);

    // This function updates our uniforms each frame
    function updateUniforms() {
        let values = getControlValues();
        gl.uniform2f(u_resolutionLoc, canvas.width, canvas.height);

        // Overwrite cameraPos with face center from FaceMesh
        // gl.uniform3fv(u_cameraPosLoc, userFacePos);
        gl.uniform3fv(u_cameraPosLoc, values.cameraPos);

        // The rest come from UI
        gl.uniform1f(u_focalLenLoc, values.focalLength);
        gl.uniform1f(u_camPolarLoc, values.cameraPolarizer);
        gl.uniform3fv(u_lightPosLoc, values.lightPos);
        gl.uniform1f(u_radianceLoc, values.radiance);
        gl.uniform1f(u_alphaLoc, values.alpha);

        // Send face normal
        gl.uniform3fv(u_cameraNormalLoc, userFaceNormal);
    }

    // =========================================
    // Create the texture for the camera feed
    // =========================================
    let texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texture);
    // Temporary single-pixel
    gl.texImage2D(
        gl.TEXTURE_2D, 0, gl.RGBA, 1, 1, 0,
        gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array([255,255,255,255])
    );
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);

    // Now create the video element (global 'video' used in initFaceMesh)
    video = document.createElement('video');
    video.autoplay    = true;
    video.playsInline = true;
    video.muted       = true;

    // Request camera stream
    navigator.mediaDevices.getUserMedia({ video: true })
    .then(stream => {
        video.srcObject = stream;
        video.play();
        // Once video is playing, set up FaceMesh
        initFaceMesh();
    })
    .catch(err => {
        // The AbortError can appear if permission is denied or the camera is not available
        console.error("Error accessing camera: ", err);
    });

    // ===================
    // Render Loop
    // ===================
    let animationFrameId = null;
    function render() {
        if (animationFrameId) {
            cancelAnimationFrame(animationFrameId);
        }
        animationFrameId = requestAnimationFrame(render);

        // Update the texture from the video feed
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
        gl.enableVertexAttribArray(positionAttrLoc);
        gl.vertexAttribPointer(positionAttrLoc, 2, gl.FLOAT, false, 0, 0);

        // Update uniforms
        updateUniforms();

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.uniform1i(u_textureLoc, 0);

        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    }

    // Listen to UI changes
    let controls = [
      'cameraX','cameraY','cameraZ','focalLength','cameraPolarizer',
      'lightX','lightY','lightZ','radiance','alpha'
    ];
    controls.forEach(controlId => {
        let ctrl = document.getElementById(controlId);
        if (!ctrl) return;
        ctrl.addEventListener('input', render);
    });

    // Kick off the rendering
    render();

    // Download button
    var downloadButton = document.getElementById('downloadButton');
    downloadButton.addEventListener('click', () => {
        render();
        var dataURL = canvas.toDataURL('image/png');
        var link = document.createElement('a');
        link.href = dataURL;
        link.download = 'rendered_image.png';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    });
})
.catch(error => {
    console.error("Error fetching fragment.glsl or initializing shaders: ", error);
});