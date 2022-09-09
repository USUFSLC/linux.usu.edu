export const rm = (env, fs, ...args) => {
  for (let path of args) {
    let { error, node } = fs.pathStatus(fs.absolutePath(env.PWD, path));
    if (error) {
      return {
        streams: {
          stderr: error
        }
      };
    }

    node.delete();
  }

  return {
    streams: {
      stdout: `Recursively removed ${args.join(", ")}`
    }
  };
};
