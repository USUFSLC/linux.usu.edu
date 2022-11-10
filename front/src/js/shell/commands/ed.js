const edWindow = (fileName, fileContents, saveCallback) => {
  const modal = $("#modal");
  modal.html(`
     <div class="modal-content">
       <form>
         <strong><label id="file-name"></label></strong>
         <hr>
         <textarea id="file-contents" class="ed-text" rows="20" name="file-contents"></textarea>
         <br>
         <button id="save-file">Save</button>
       </form>
     </div>
   </div>`);
  $("#file-name").text(`${fileName}`);
  $("#file-contents").val(fileContents);
  $("#save-file").on("click", (e) => {
    e.preventDefault();
    saveCallback($("#file-contents").val());
    $("#modal").hide();
  });
};

export const ed = (env, fs, ...args) => {
  const path = fs.absolutePath(env["PWD"], args[0]);
  let { error, node } = fs.pathStatus(path, "file");
  if (error) {
    if (node) {
      // File is a directory
      return {
        streams: {
          stderr: error,
        },
      };
    }
    // Node does not yet exist - attempt to create it
    node = fs.insertNewNodeAt(path);
    if (node.error) {
      return {
        streams: {
          stderr: node.error,
        },
      };
    }

    node.fileContents = "";
  }

  if (typeof node.fileContents === "function") {
    return {
      streams: {
        stderr: `${path} is a binary`,
      },
    };
  }

  edWindow(
    node.name,
    node.fileContents,
    (fileContents) => (node.fileContents = fileContents)
  );

  $("#modal").show();
  $("#file-contents").focus();

  return {
    streams: {
      stdout: `Now editing ${path}`,
    },
  };
};
