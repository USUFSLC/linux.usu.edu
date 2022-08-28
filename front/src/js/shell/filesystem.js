export class FileSystemNode {
  constructor(name) {
    this.name = name;
    this.fileContents = null;
    this.parent = null;
    this.children = {};
  }

  getFullPath() {
    if (this.parent) {
      return `${this.parent.getFullPath()}/${this.name}`;
    }
    return ""; // No need for a trailing slash on the root node
  }

  delete() {
    for (const child of Object.values(this.children)) {
      child.delete();
    }
    if (this.parent) {
      delete this.parent.children[this.name];
    }
  }
}

export class FileSystem {
  constructor(filesystemRepresentation) {
    this.root = new FileSystemNode("/");

    let curr = this.root;
    const constructFs = (obj) => {
      for (let key of Object.keys(obj)) {
        const newNode = new FileSystemNode(key);
        newNode.parent = curr;
        curr.children[key] = newNode;

        if (typeof obj[key] === "object") {
          // It's a directory
          curr = newNode;
          constructFs(obj[key]);
          curr = curr.parent;
        } else {
          // It's a file
          newNode.fileContents = obj[key];
        }
      }
    }
    constructFs(filesystemRepresentation);
  }

  reducePath(fsNode) {
    return fsNode.getFullPath();
  }

  absolutePath(currentWorkingDirectory, path) {
    if (path.startsWith("/")) {
      return path;
    } else {
      const endsWithSlash = currentWorkingDirectory.endsWith("/");
      return `${currentWorkingDirectory}${endsWithSlash ? "" : "/"}${path}`;
    }
  }

  getNode(path, fsNode=this.root) {
    if (!fsNode) {
      return null;
    }   

    if (!path) {
      return fsNode;
    } else if (path.startsWith("..")) {
      return this.getNode(path.slice(2), fsNode.parent || fsNode); // Cannot go above root
    } else if (path.startsWith(".")) {
      return this.getNode(path.slice(1), fsNode);
    }

    const slashIndex = path.search("/");
    if (slashIndex > -1) {
      if (path.startsWith("/")) {
        return this.getNode(path.slice(1), fsNode);
      }
      const [curr, rest] = [path.slice(0, slashIndex), path.slice(slashIndex + 1)];
      return this.getNode(rest, fsNode.children[curr]);
    }

    return fsNode.children[path];
  }

  pathStatus(path, type, fsNode=this.root) {
    const node = this.getNode(path, fsNode);
    if (!node) {
      return { error: `No such ${type}: ${path}` };
    } 
    const result = {
      type: node.fileContents !== null ? "file" : "directory",
      path: this.reducePath(node),
      node,
    };
    if (type && result.type !== type) {
      return { error: `${path} is not a ${type}` };
    }
    return result;
  }
}
