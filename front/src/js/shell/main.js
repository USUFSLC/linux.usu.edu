/**
 * Author: Simponic
 * A simple JS "shell".
 */

import { FileSystem } from "./filesystem";
import { Shell } from "./shell";

import { cd } from "./commands/cd";
import { ls } from "./commands/ls";
import { echo } from "./commands/echo";
import { trongle } from "./commands/trongle";

const FILES = {
  "home" : {
    "logan" : {
      "Downloads" : {
        "sayHello" : (env, fs, ...args) => {
          return {
            stdout: "Hello, world"
          }
        }
      }
    },
  },
  "usr" : {
    "bin" : {
      cd, // Traditionally cd and echo are shell-specific commands, but whatever
      echo,
      ls,
    },
    "local" : {
      "bin" : {
        trongle
      }
    }
  }
};

const fs = new FileSystem(FILES);
export const shell = new Shell(fs, {
  PS1: "<blue>${USER}</blue><orange>@</orange><bold><purple>${HOSTNAME}</purple></bold>:<yellow>${PWD}</yellow> <orange>$</orange>",
  USER: "guest",
  HOSTNAME: "usufslc.com",
  PWD: "/home",
  PATH: "/usr/bin:/usr/local/bin",
});