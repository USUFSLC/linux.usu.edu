export const cd = (env, fs, ...args) => {
  const newDir = args[0] || `/home/${env.USER}`;

  let PWD = fs.pathStatus(fs.absolutePath(env.PWD, newDir), "directory");
  if (PWD.error) {
    return {
      streams: {
        stderr: PWD.error,
      },
    };
  }

  return {
    env: {
      PWD: PWD.node.getFullPath(),
    },
  };
};
