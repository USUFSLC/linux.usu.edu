const flipFlopTheme = theme => theme === 'dark' ? 'light' : 'dark';
const themePickerSources = {
  light: '/images/sun.svg',
  dark: '/images/moon.svg'
};

const setTheme = (theme) => {
  $('#theme-icon').attr('src', themePickerSources[theme]);

  $(document.documentElement).attr('data-theme', theme);
  localStorage.setItem('theme', theme);
};

$('#theme-switcher').on('click', _ => setTheme(flipFlopTheme($(document.documentElement).attr('data-theme'))));

setTheme(localStorage.getItem('theme'));

