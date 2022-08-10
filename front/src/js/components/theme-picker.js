const flipFlopTheme = theme => theme === 'dark' ? 'light' : 'dark';
const themePickerSources = {
  light: '/images/sun.svg',
  dark: '/images/moon.svg'
};

const setTheme = (theme) => {
  $('#theme-icon').attr('src', themePickerSources[theme]);

  $('#wrapper').attr('class', theme);
  localStorage.setItem('theme', theme);
};

$('#theme-switcher').on('click', _ => setTheme(flipFlopTheme($('#wrapper').attr('class'))));

setTheme(localStorage.getItem('theme') || 'dark');

