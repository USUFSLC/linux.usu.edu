export const cd = (env, fs, ...args) => {
  const newDir = args[0];
  if (!newDir) {
    return {
      streams: {
        stderr: "No directory specified"
      }
    };
  }

  let PWD = fs.pathStatus(fs.absolutePath(env.PWD, newDir), "directory");
  if (PWD.error) {
    return {
      streams: {
        stderr: PWD.error
      }
    };
  }

  return {
    env: {
      PWD: PWD.node.getFullPath()
    }
  };
}
