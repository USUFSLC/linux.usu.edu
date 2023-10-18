/**
 * Author: Simponic
 * A simple JS "shell".
 */

import { FileSystem } from "./filesystem";
import { Shell } from "./shell";

import { cal } from "./commands/cal";
import { cd } from "./commands/cd";
import { ls } from "./commands/ls";
import { echo } from "./commands/echo";
import { trongle } from "./commands/trongle";
import { tronglesay } from "./commands/tronglesay";
import { mkdir } from "./commands/mkdir";
import { touch } from "./commands/touch";
import { cat } from "./commands/cat";
import { ed } from "./commands/ed";
import { tree } from "./commands/tree";
import { clear } from "./commands/clear";
import { rm } from "./commands/rm";
import { help } from "./commands/help";

const FILES = {
  home: {},
  usr: {
    bin: {
      cd, // Traditionally cd and echo are shell-specific commands, but whatever
      cal,
      echo,
      ls,
      mkdir,
      touch,
      cat,
      ed,
      tree,
      rm,
      help,
      tronglesay,
    },
    local: {
      bin: {
        trongle,
        clear,
      },
    },
  },
};

const fs = new FileSystem(FILES);
export const shell = new Shell(fs, {
  PS1: "<blue>${USER}</blue><orange>@</orange><bold><purple>${HOSTNAME}</purple></bold>:<yellow>${PWD}</yellow> <orange>$</orange>",
  HOSTNAME: "usufslc.com",
  PWD: "/home",
  PATH: "/usr/bin:/usr/local/bin",
});
