$('#sidebar-toggle').on('click', function () {
  [this, '#sidebar'].forEach((elem) =>
    $(elem).toggleClass('active')
  );
});
// Escape key
$('html').on('keyup', e => 
  e.key === 'Escape' && 
  ['#sidebar', '#sidebar-toggle'].forEach(elem => 
    $(elem).removeClass('active')
  )
);
