export const rm = (env, fs, ...args) => {
  const path = args[0];
  let { error, node } = fs.pathStatus(fs.absolutePath(env.PWD, path));
  if (error) {
    return {
      streams: {
        stderr: error
      }
    };
  }

  node.delete();

  return {
    streams: {
      stdout: `Recursively removed ${path}`
    }
  };
};
