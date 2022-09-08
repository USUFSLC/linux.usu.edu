export const tree = (env, fs, ...args) => {
  const path = fs.absolutePath(env["PWD"], args[0] || ".");
  const { error, node } = fs.pathStatus(path, "directory");
  if (error) {
    return {
      streams: {
        stderr: error
      }
    };
  }

  let result = [];
  const traverseDir = (fs, depth=0) => {
    if (fs.children) {
      for (let child of Object.keys(fs.children)) {
        result.push(traverseDir(fs.children[child], depth+1));
      } 
    }
    return (depth > 1 ? Array(depth-1).fill("    ").join("") : "") + `└── ${fs.name}`;
  };

  traverseDir(node);

  return {
    streams: {
      stdout: result.reverse().join("\n")
    }
  };
};
