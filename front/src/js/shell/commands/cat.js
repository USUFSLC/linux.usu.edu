export const cat = (env, fs, ...args) => {
  let result = "";
  for (let file of args) {
    const path = fs.absolutePath(env["PWD"], file);
    const nodeStatus = fs.pathStatus(path, "file");
    if (nodeStatus.error) {
      return {
        streams: {
          stderr: nodeStatus.error
        }
      };
    }
    result += nodeStatus.node.fileContents + "\n";
  }
  return {
    streams: {
      stdout: result
    }
  };
};
