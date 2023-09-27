// Sets theme before rendering & jquery loaded to prevent flashing of uninitialized theme
// (ugly white background)
document.documentElement.setAttribute(
  "data-theme",
  localStorage.getItem("theme") || "light"
);
