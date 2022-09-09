export class Shell {
  constructor(fs, env) {
    this.fs = fs;
    this.history = [];
    this.env = {
      PS1: "${PWD}>",
      USER: "guest",
      HOSTNAME: "localhost",
      PWD: "/",
      PATH: "",
      ...env
    };
    this.streams = {
      stdout: "", // Hacky stdout/stderr for now, maybe move to a "file" later?
      stderr: "",
    };
  }

  static parseCommand(command) {
    return (command.trim()
            .match(/(["'])(?:\\\1|.)*?\1|[^ "]+/g) || [])
      .map(arg => arg.replace(/^["']|["']$/g, ""))
      .filter(arg => arg !== "");
  }

  getEnv(name) {
    return this.env[name];
  }

  setEnv(name, value) {
    this.env[name] = value;
  }

  buildPrompt() {
    return this.env.PS1.replaceAll(/\$\{DATE\}/g, new Date().toLocaleString())
                       .replaceAll(/<(\w+)>/g, (_, name) => `<span class="${name}">`)
                       .replaceAll(/<\/(\w+)>/g, "</span>")
                       .replaceAll(/\$\{(\w+)\}/g, (match, name) => this.env[name] || match);
  }

  execute(name, ...args) {
    let binaryStatus;
    if (name.startsWith("/")) {
      binaryStatus = this.fs.pathStatus(name);
    } else {
      for (let path of [this.env.PWD, ...this.env.PATH.split(":")]) {
        binaryStatus = this.fs.pathStatus(`${path}/${name}`);
        if (!binaryStatus.error) {
          break;
        }
      }
    }

    if (binaryStatus.error) {
      return {
        streams: {
          stderr: `${name}: command not found`
        }
      };
    }

    if (binaryStatus.type === "file" && typeof binaryStatus.node.fileContents === "function") {
      try {
        return binaryStatus.node.fileContents(this.env, this.fs, ...args);
      } catch (e) {
        return {
          streams: {
            stderr: e.message
          }
        };
      }
    }
    return {
      streams: {
        stderr: `${name} is not executable`
      }
    };
  }

  run(command) {
    this.streams.stdout = "";
    this.streams.stderr = "";

    if (command) {
      this.history.push(command);

      const withEnvVars = command
            .replaceAll(/(\w+)=("([^"]+)"|[^ ]+)/g, (match, name, value, inQuotes) => {
              // Set environment variables from assignment
              this.env[name] = inQuotes || value;
              return "";
            })
            .replaceAll(/\$(\w+)/g, (match, name) => this.env[name] || match); // Replace values
      const withUserHome = withEnvVars.replaceAll(/\~/g, `/home/${this.env.USER}`);
      const withWildCard = withUserHome.replaceAll(/([/\w]+)?\*/g, (match, path) => {
        path = this.fs.absolutePath(this.env.PWD, path || this.env.PWD);
        const { node, error } = this.fs.pathStatus(path);
        if (error) {
          return "";
        }
        const resp = Object.keys(node.children).map((childName) => this.fs.absolutePath(path, childName)).join(" ");
        return resp;
      });

      const [name, ...args] = Shell.parseCommand(withWildCard);

      if (name) {
        const result = this.execute(name, ...args);
        this.env = {
          ...this.env,
          ...result.env
        };
        this.streams = {
          ...this.streams,
          ...result.streams
        };
      }
    }

    return {
      env: this.env,
      streams: this.streams,
    };
  }
}
