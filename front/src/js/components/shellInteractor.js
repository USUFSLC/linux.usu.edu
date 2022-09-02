const userName = $("#user-name").val();
window.shell.setEnv("USER", userName);
window.shell.run("mkdir /home/$USER");
window.shell.setEnv("PWD", `/home/${userName}`);

const magic = (input) => $('<span>').text(input).html();

$("#shell-input").on("submit", function (e) {
  e.preventDefault();
  const command = $("#shell-command").val();
  $("#shell-command").val("");

  const oldPrompt = window.shell.buildPrompt();
  const result = window.shell.run(command);
  $("#terminal-history").append(`<div class="terminal-entry">
                                  <p>${oldPrompt} <span class="green">${command}</span></p>
                                  <pre class="red">${magic(result.streams.stderr)}</pre>
                                  <pre>${magic(result.streams.stdout)}</pre>
                                </div>`);

  $("#prompt").html(window.shell.buildPrompt());
});
$("#prompt").html(window.shell.buildPrompt());
