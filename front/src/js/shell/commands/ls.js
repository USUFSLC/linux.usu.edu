export const ls = (env, fs, ...args) => {
  let result = "";
  for (let path of (args.length ? args : [env.PWD])) {
    result += "\n";
    const { error, node } = fs.pathStatus(fs.absolutePath(env.PWD, path), "directory");
    if (error) {
      return {
        streams: {
          stderr: error
        }
      };
    }
    result += `${path}:\n  ${Object.keys(node.children).join("\n  ")}`;
  }

  return {
    streams: {
      stdout: result
    }
  };
};
