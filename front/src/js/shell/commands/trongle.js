const randUpTo = (max) => Math.random() * max;

class Trongle {
  constructor(element, MAX_VEL = 0.3) {
    this.element = element;
    this.x = Math.floor(randUpTo(window.innerWidth - element.clientWidth));
    this.y = Math.floor(randUpTo(window.innerHeight - element.clientHeight));
    this.dx = (randUpTo(2) - 1) * MAX_VEL;
    this.dy = (randUpTo(2) - 1) * MAX_VEL;
    this.width = element.clientWidth;
    this.height = element.clientHeight;
    this.lastRender = performance.now();
  }

  update(elapsedTime, maxHeight, maxWidth) {
    this.x += this.dx * elapsedTime;
    this.y += this.dy * elapsedTime;

    this.dx *= this.x + this.width >= maxWidth || this.x <= 0 ? -1 : 1;
    this.dy *= this.y + this.height >= maxHeight || this.y <= 0 ? -1 : 1;

    this.x = Math.max(0, Math.min(this.x, maxWidth - this.width));
    this.y = Math.max(0, Math.min(this.y, maxHeight - this.height));
  }

  draw() {
    this.element.style.left = this.x + "px";
    this.element.style.top = this.y + "px";
  }

  loopStep(timestamp, maxHeight, maxWidth) {
    const elapsedTime = timestamp - this.lastRender;
    if (elapsedTime > 30 / 1000) {
      // 30 FPS for performance reasons
      this.update(elapsedTime, maxHeight, maxWidth);
      this.draw();

      this.lastRender = timestamp;
    }
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
};

const makeTrongles = (n, trongleSrcs, wrapperElement) => {
  const trongles = Array(n)
    .fill(0)
    .map(() => {
      const trongleImage = makeTrongleElement(
        trongleSrcs[Math.floor(Math.random() * trongleSrcs.length)]
      );
      wrapperElement.appendChild(trongleImage);

      return new Trongle(trongleImage);
    });

  const dHeight = $(document).height();
  const dWidth = $(document).width();

  const loop = (t) => {
    const width = $(document).width();
    const height = $(document).height();
    for (const trongle of trongles) {
      trongle.loopStep(t, width, height);
    }
    requestAnimationFrame(loop);
  };
  requestAnimationFrame(loop);
};

const TRONGLE_PATHS = [
  "cubeongle.png",
  "doritongle.png",
  "pentongle.png",
  "quadrongle.png",
  "trapezongle.png",
  "trongAngry.png",
  "trongFem.png",
  "trongHandsome.png",
  "trongPants.png",
  "trongPants2.png",
  "trongPoly.png",
  "trongYeah.png",
  "trongle.png",
  "tronglePat.gif",
  "tuxongle.png",
].map((x) => `/images/rongles/${x}`);

export const trongle = (env, fs, ...args) => {
  const min = parseInt(args[0]) || 6;
  const max = Math.max(min, parseInt(args[1]) || 13);

  const numTrongles = Math.floor(Math.random() * (max - min)) + min;

  makeTrongles(numTrongles, TRONGLE_PATHS, document.body);

  return {
    streams: {
      stdout: `Instantiated ${numTrongles} trongles. Gaming!`,
    },
  };
};
