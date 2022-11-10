export class FileSystemNode {
  constructor(name) {
    this.name = name;
    this.fileContents = null;
    this.parent = null;
    this.children = {};
  }

  getFullPath() {
    if (this.parent) {
      const parentPath = this.parent.getFullPath();
      return `${parentPath + (!this.parent?.parent ? "" : "/")}${this.name}`;
    }
    return "/";
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
    };
    constructFs(filesystemRepresentation);
  }

  absolutePath(currentWorkingDirectory, path) {
    if (path.startsWith("/")) {
      return path;
    } else {
      const endsWithSlash = currentWorkingDirectory.endsWith("/");
      return `${currentWorkingDirectory}${endsWithSlash ? "" : "/"}${path}`;
    }
  }

  getNode(path, fsNode = this.root) {
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
      const [curr, rest] = [
        path.slice(0, slashIndex),
        path.slice(slashIndex + 1),
      ];
      return this.getNode(rest, fsNode.children[curr]);
    }

    return fsNode.children[path];
  }

  insertNewNodeAt(absPath) {
    if (this.getNode(absPath)) {
      return {
        error: `Something already exists at ${absPath}`,
      };
    }

    const path = absPath.split("/").filter((x) => x);
    const newDir = path.pop();
    const parentNodeStatus = this.pathStatus(`/${path.join("/")}`, "directory");

    if (parentNodeStatus.error) {
      return parentNodeStatus;
    }

    const newChild = new FileSystemNode(newDir);
    newChild.parent = parentNodeStatus.node;
    newChild.parent.children[newDir] = newChild;
    return newChild;
  }

  pathStatus(path, type, fsNode = this.root) {
    const node = this.getNode(path, fsNode);
    if (!node) {
      if (type) {
        return { error: `No such ${type}: ${path}` };
      }
      return { error: `${path} does not exist` };
    }
    const result = {
      type: node.fileContents !== null ? "file" : "directory",
      path: node.getFullPath(),
      node,
    };
    if (type && result.type !== type) {
      return { error: `${path} is not a ${type}`, node };
    }
    return result;
  }
}
