const Hls = require("hls.js");

const stream = document.getElementById("stream");
const streamSrc = stream?.dataset.stream;

if (stream && Hls.isSupported() && streamSrc.endsWith(".m3u8")) {
  const hls = new Hls();

  hls.loadSource(streamSrc);
  hls.attachMedia(stream);
  hls.on(Hls.Events.MANIFEST_PARSED, function () {
    stream.play();
  });
}

if (stream && streamSrc.endsWith(".mp4")) {
  const source = document.createElement("source");

  source.setAttribute("src", streamSrc);
  source.setAttribute("type", "video/mp4");

  stream.appendChild(source);
}

if (stream?.muted)
  $("#alerts").append(
    "<div class='alert warn' onclick='$(this).hide()'>The video may be muted</div>"
  );
