window.$ = require("jquery");
window.jQuery = window.$;
window.flatpickr = require("flatpickr");

window.SimpleCalendar = require("../../node_modules/simple-calendar/dist/jquery.simple-calendar.min.js");

window.shell = require("./shell/main").shell;

require("./components/sidebar");
require("./components/themePicker");
require("./components/shellInteractor");
require("./components/alerts.js");
require("./components/date.js");

require("./components/stream.js");

require("./components/dates.js");
