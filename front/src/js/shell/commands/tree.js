export const tree = (env, fs, ...args) => {
  let result = [];
  const traverseDir = (fs, depth = 0) => {
    if (fs.children) {
      for (let child of Object.keys(fs.children)) {
        result.push(traverseDir(fs.children[child], depth + 1));
      }
    }
    return (
      (depth > 1
        ? Array(depth - 1)
            .fill("    ")
            .join("")
        : "") + `└── ${fs.name}`
    );
  };

  for (let dir of args.length ? args : ["."]) {
    const path = fs.absolutePath(env["PWD"], dir);

    const { error, node } = fs.pathStatus(path, "directory");
    if (error) {
      return {
        streams: {
          stderr: error,
        },
      };
    }
    traverseDir(node);
    result.push(dir);
  }

  return {
    streams: {
      stdout: result.reverse().join("\n"),
    },
  };
};
