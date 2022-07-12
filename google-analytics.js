$(() => {
  
  /* Default installation */
    
    window.dataLayer = window.dataLayer || [];
    function gtag() {
      dataLayer.push(arguments);
    };
    gtag('js', new Date());
    gtag('config', 'G-Z967JJVQQX');
    
});

$(document).on('change', '#bins', function(e) {
    ga('send', 'event', 'widget', 'select bins', $(e.currentTarget).val());
  });