const randUpTo = (max) => Math.random() * max
class Trongle {
  constructor(element) {
    this.element = element;
    this.x = Math.floor(randUpTo(window.innerWidth-element.clientWidth));
    this.y = Math.floor(randUpTo(window.innerHeight-element.clientHeight));
    this.dx = (Math.random() * 2 - 1) * 0.3;
    this.dy = (Math.random() * 2 - 1) * 0.3;
    this.width = element.clientWidth;
    this.height = element.clientHeight;
    this.lastRender = performance.now();
  }

  update(elapsedTime) {
    this.x += (this.dx*elapsedTime)
    this.y += (this.dy*elapsedTime)

    this.dx *= this.x + this.width >= window.innerWidth || this.x <= 0 ? -1 : 1;
    this.dy *= this.y + this.height >= window.innerHeight || this.y <= 0 ? -1 : 1;

    this.x = Math.max(0, Math.min(this.x, window.innerWidth-this.width));
    this.y = Math.max(0, Math.min(this.y, window.innerHeight-this.height));
  }

  draw() {
    this.element.style.left = this.x + "px";
    this.element.style.top = this.y + "px";
  }

  loop(timestamp) {
    const elapsedTime = timestamp - this.lastRender;
    this.update(elapsedTime);
    this.draw();

    this.lastRender = timestamp;
    requestAnimationFrame(this.loop.bind(this));
  }
}

const makeTrongleElement = (src) => {
  const trongleImage = document.createElement("img");
  trongleImage.style.position = "absolute";
  trongleImage.src = src;
  trongleImage.width = "100";
  trongleImage.height = "100";
  trongleImage.style.left = `-200px`;
  trongleImage.style.top = `-200px`;
  return trongleImage;
}

const makeTrongles = (n, trongleSrcs, wrapperElement) => {
  const trongles = Array(n).fill(0).map(() => {
    const trongleImage = makeTrongleElement(trongleSrcs[Math.floor(Math.random() * trongleSrcs.length)]);
    wrapperElement.appendChild(trongleImage);

    return new Trongle(trongleImage);
  });

  requestAnimationFrame((t) => trongles.map((trongle) => trongle.loop(t)));
}

const TRONGLE_PATHS = [
  "cubeongle.png", "doritongle.png", "pentongle.png", "quadrongle.png", "trapezongle.png",
  "trongAngry.png", "trongFem.png", "trongHandsome.png", "trongPants.png", "trongPants2.png",
  "trongPoly.png", "trongYeah.png", "trongle.png", "tronglePat.gif", "tuxongle.png"
].map((x) => `/images/rongles/${x}`);

export const trongle = (env, fs, ...args) => {
  const min = parseInt(args[0]) ||  6;
  const max = Math.max(min, parseInt(args[1]) || 13);

  const numTrongles = Math.floor(Math.random() * (max-min)) + min;

  makeTrongles(numTrongles, TRONGLE_PATHS, document.body);

  return {
    stdout: `Instantiated ${numTrongles} trongles. Gaming!`,
  };
}
