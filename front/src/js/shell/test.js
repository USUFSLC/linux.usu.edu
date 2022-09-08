// Test with `npm run test`

import { FileSystem } from "./filesystem.js";
import { Shell } from "./shell.js";

// Test helpers
const it = (desc, f) => {
  try {
    f();
    console.log(`${desc} ... OK`);
  } catch (error) {
    console.log(`${desc} ... FAILED`);
    console.error(error);
  }
};

const assert = (condition, message) => {
  if (!condition) {
    throw new Error(message);
  }
};


const testFs = {
  "a" : {
    "b" : {
      "c" : "This is a file",
      "d" : "This is another file",
      "e" : {
        "f" : "This is a file",
        "yes" : (env, fs, ...args) => {
          return {
            streams: {
              stdout: Array(parseInt(args[0])).fill("yes").join(" ")
            }
          };
        }
      },
      "echo" : (env, fs, ...args) => {
        return {
          streams: {
            stdout: args.join(" ")
          }
        };
      }
    }
  },
  "g" : {
    "h" : () => {
      return {
        streams: {
          stdout: "Hello, world"
        }
      };
    }
  }
};

export const runTests = () => {
  // Filesystem tests
  it('has no parent on root node', () => {
    const root = new FileSystem(testFs).root;
    assert(root.parent === null, "Root has no parent");
  });

  it('correctly builds a tree with children', () => {
    const root = new FileSystem(testFs).root;
    assert(Object.keys(root.children).length === 2, "Root has two children");
    assert(Object.keys(root.children.a.children).length === 1, "a has one child");
    assert(Object.keys(root.children.a.children.b.children).length === 4, "b has four children");
    assert(Object.keys(root.children.a.children.b.children.e.children).length === 2, "e has two children");
  });

  it('builds with reference to parent', () => {
    const root = new FileSystem(testFs).root;
    assert(root.children.a.parent === root, "/ -> a <- /");
    assert(root.children.a.children.b.parent === root.children.a, "/ -> a -> b <- a");
    assert(root.children.a.children.b.children.c.parent === root.children.a.children.b, "/ -> a -> b -> c <- b");
    assert(root.children.a.children.b.children.c.parent.parent.parent === root, "/ -> a -> b -> c <- b <- a <- /");
  });

  it('returns root node when absolute path', () => {
    const root = new FileSystem(testFs).root;
    assert(root.name === "/", "Root node has name /");
  });

  it('returns the correctly formatted absolute directory', () => {
    const fs = new FileSystem(testFs);
    assert(fs.absolutePath("/", "a/b/c") === "/a/b/c", "/ + a/b/c ");
    assert(fs.absolutePath("/home/logan", "/a/b/c") === "/a/b/c", "/ + a/b/c ");
    assert(fs.absolutePath("/home/logan/", "/a/b/c") === "/a/b/c", "/ + a/b/c ");
    assert(fs.absolutePath("/home/logan", "a/b/c") === "/home/logan/a/b/c", "/ + a/b/c ");
  });

  it('inserts new nodes correctly', () => {
    const fs = new FileSystem({ });
    const home = fs.insertNewNodeAt("/home");
    assert(!home.error, "creating home node doesn't fail");
    const usr = fs.insertNewNodeAt("/usr");
    assert(!usr.error, "creating usr node doesn't fail");
    const home_bruh = fs.insertNewNodeAt("/home/bruh");
    assert(!home_bruh.error, "creating home_bruh node doesn't fail");
    const home_bruh_failure = fs.insertNewNodeAt("/home/bruh");
    assert(home_bruh_failure.error, "Trying to insert a node at a conflicting path fails");
  });

  it('returns correct nodes with paths', () => {
    const fs = new FileSystem(testFs);
    const root = fs.getNode("/");
    assert(fs.getNode("/a/b/../../.") === root, "Using . and .. returns correct path");
    assert(fs.getNode("/..") === root, "Getting parent on root returns root");
    const a = root.children.a;
    assert(fs.getNode("/a/../a/./") === a, "Using . and .. returns correct path");

    assert(fs.getNode("/a/../a/./").getFullPath() === "/a", "getFullPath reduces path");
    assert(fs.getNode("/a/../a/./../..").getFullPath() === "/", "getFullPath reduces path on recursive parent root node");
  });

  it('returns the correct status of a dir/file', () => {
    const fs = new FileSystem(testFs);
    assert(fs.pathStatus("/a/b/c", "file").node.name === "c", "File status is correct");
    assert(fs.pathStatus("/a/b", "file").error, "Path is a dir, fails on file status");
    assert(fs.pathStatus("/a/b", "file").node, "Path is a dir, fails on file status, still returns node");
    assert(fs.pathStatus("/g/h").type === "file", "When no type specified, returns node and file type");
    assert(fs.pathStatus("/asdfasdfasdfasdf").error, "Path does not exist, fails on status");
  });

  it('has fileContents when it is a file', () => {
    const fs = new FileSystem(testFs);
    assert(fs.getNode("/a/b/c").fileContents === "This is a file", "File contents are correct");
    assert(fs.getNode("/a/b/d").fileContents === "This is another file", "File contents are correct");
    assert(fs.getNode("/a/b/e/f").fileContents === "This is a file", "File contents are correct");
    assert(typeof fs.getNode("/g/h").fileContents === "function", "File contents are correct");
  });

  it('deletes a directory or file', () => {
    const fs = new FileSystem(testFs);
    const root = fs.getNode("/");
    assert(Object.keys(root.children).length === 2, "Root has two children");
    fs.getNode("/a").delete();
    assert(Object.keys(root.children).length === 1, "Root has one child");

    assert(Object.keys(root.children.g.children).length === 1, "g has one child");
    fs.getNode("/g/h").delete();
    assert(Object.keys(root.children.g.children).length === 0, "g has no children");
  });


  // Shell tests
  it('parses commands correctly', () => {
    const arrayEqual = (a, b) => {
      if (a.length !== b.length) {
        return false;
      }
      return a.every((item, index) => item === b[index]);
    };
    assert(arrayEqual(Shell.parseCommand('ls /'), ["ls", "/"]), "Parses ls command correctly");
    assert(
      arrayEqual(
        Shell.parseCommand('  /asdf/fdsa "Is a gaming   moment " and   stuff '), 
        ["/asdf/fdsa", "Is a gaming   moment ", "and", "stuff"]
      ), "Parses whitespace correctly");
  });  

  it('initializes environment', () => {
    const shell = new Shell(new FileSystem(testFs), {
      PWD: "/a/b",
      PATH: "/g"
    });
    assert(shell.getEnv("PWD") === "/a/b", "PWD is correct");
    assert(shell.getEnv("PATH") === "/g", "PATH is correct");
  });

  it('executes commands in path and PWD', () => {
    const shell = new Shell(new FileSystem(testFs), {
      PWD: "/",
      PATH: "/g:/a/b"
    });
    assert(shell.run("h").streams.stdout === "Hello, world", "'h' program executed from PATH");
    assert(shell.run("echo Hello, world").streams.stdout === "Hello, world", "'echo Hello, world' program executed from PATH");
    shell.setEnv("PWD", "/a/b/e");
    assert(shell.run("./yes 5").streams.stdout === "yes yes yes yes yes", "'./yes 5' executed from PWD");
    shell.setEnv("PWD", "/");
    assert(shell.run("/a/b/e/yes 5").streams.stdout === "yes yes yes yes yes", "'/a/b/e/yes 5' execute from absolute path");
  });

  it('replaces environment variables in command', () => {
    const shell = new Shell(new FileSystem(testFs), {
      PWD: "/",
      PATH: "/g:/a/b"
    });
    assert(shell.run("echo $PWD").streams.stdout === "/", "PWD env var is correct");
  });


  it('sets environment variables from command line', () => {
    const shell = new Shell(new FileSystem(testFs), {
      PWD: "/",
      PATH: "/g:/a/b"
    });
    shell.run("PS1=asdf");
    assert(shell.getEnv("PS1") === "asdf", "PS1 env var set correctly");

    shell.run("PS1=\"asdf gaming moment\"");
    assert(shell.getEnv("PS1") === "asdf gaming moment", "PS1 env var  with quotes set correctly");
  });
    
  it('replaces tilde with user home', () => {
    const shell = new Shell(new FileSystem(testFs), {
      USER: "bruh",
      PWD: "/",
      PATH: "/g:/a/b"
    });

    assert(shell.run("echo ~").streams.stdout === "/home/bruh");
  });

  it('keeps shell history', () => {
    const shell = new Shell(new FileSystem(testFs), {
      USER: "bruh",
      PWD: "/",
      PATH: "/g:/a/b"
    });

    shell.run("ls");
    shell.run("echo bruh");
    shell.run("echo moment");
    assert(shell.history[0] === "ls", "First command in history");
    assert(shell.history[1] === "echo bruh", "Second command in history");
  });

  it('prints to stderr on error', () => {
    const shell = new Shell(new FileSystem(testFs), {
      PWD: "/",
      PATH: "/g:/a/b"
    });
    assert(shell.run("bruhMoment").streams.stderr === "bruhMoment: command not found", "Prints command not found to stderr");
    assert(shell.run("/a").streams.stderr === "/a is not executable", "Prints that a is not executable to stderr");
  });

  it('builds classed prompts', () => {
    const shell = new Shell(new FileSystem(testFs), {
      PWD: "/",
      PATH: "/g:/a/b",
      PS1: "> ",
      USER: "test",
      HOSTNAME: "localhost"
    });
    assert(shell.buildPrompt() === "> ", "Prompt is correct");
    shell.setEnv("PS1", "<blue>${USER}</blue><orange>@</orange><purple>${HOSTNAME}</purple>:${PWD} <green>$</green>");
    assert(shell.buildPrompt() === '<span class="blue">test</span><span class="orange">@</span><span class="purple">localhost</span>:/ <span class="green">$</span>', "Prompt is correct");
  });
}

runTests();
