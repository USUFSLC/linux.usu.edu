// Sets theme before rendering & jquery loaded to prevent uninitialized theme (ugly white background)
document.documentElement.setAttribute('data-theme', localStorage.getItem('theme') || 'dark');
