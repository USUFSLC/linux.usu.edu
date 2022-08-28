export const ls = (env, fs, ...args) => {
  return {
    stdout: Object.keys(fs.getNode(fs.absolutePath(env.PWD, args[0] || env.PWD)).children).join("\n")
  }
}