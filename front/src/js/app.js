window.$ = require('jquery');

window.shell = require('./shell/main').shell;

require('./components/sidebar');
require('./components/themePicker');
require('./components/shellInteractor');
require('./components/alerts.js');
