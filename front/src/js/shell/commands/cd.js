export const cd = (env, fs, ...args) => {
  const newDir = args[0];
  if (!newDir) {
    return {
      stderr: "No directory specified"
    };
  }

  let PWD = fs.pathStatus(fs.absolutePath(env.PWD, newDir), "directory");
  if (PWD.error) {
    return {
      stderr: PWD.error
    };
  }

  return {
    PWD: PWD.node.getFullPath()
  }
}