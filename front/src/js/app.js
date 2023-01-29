window.magicTextToHtml = (input) => $("<span>").text(input).html();
window.$ = require("jquery");
window.shell = require("./shell/main").shell;

require("./components/sidebar");
require("./components/themePicker");
require("./components/shellInteractor");
require("./components/alerts");
require("./components/stream");
require("./components/dates");
require("./components/forms");
require("./components/deleteButton");
