export const touch = (env, fs, ...args) => {
  const abs = fs.absolutePath(env["PWD"], args[0]);

  const newChild = fs.insertNewNodeAt(abs);
  if (newChild.error) {
    return {
      streams: {
        stderr: newChild.error
      }
    };
  }
  newChild.fileContents = ""; // initialize with empty contents

  return {
    streams: {
      stdout: `New file: ${newChild.getFullPath()} created`
    }
  };
};
