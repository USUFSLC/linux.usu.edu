#!/usr/bin/env node

const esbuild = require("esbuild");
[
  {
    entryPoints: ["src/js/app.js"],
    outfile: "dist/js/main.js",
  },
  {
    entryPoints: ["src/css/style.css"],
    external: ['*.ttf'],
    outfile: "dist/css/style.css",
  }
].forEach((x) =>
  esbuild.build({
    logLevel: "info",
    bundle: true,
    minify: true,
    ...x
  })
  .catch(() => process.exit(1))
);
