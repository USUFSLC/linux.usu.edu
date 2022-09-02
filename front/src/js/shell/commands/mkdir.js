export const mkdir = (env, fs, ...args) => {
  const abs = fs.absolutePath(env["PWD"], args[0]);

  const newChild = fs.insertNewNodeAt(abs);
  if (newChild.error) {
    return {
      streams: {
        stderr: newChild.error
      }
    };
  }

  return {
    streams: {
      stdout: `${newChild.getFullPath()} created`
    }
  };
};
