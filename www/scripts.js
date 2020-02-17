
// ~~~~~~~~~~~~ SHINYJS FUNCTIONS ~~~~~~~~~~~~~ //

shinyjs.toTop = function() {
  window.scrollTo(0, 0);
};

shinyjs.showEng = function() {
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=full_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=about_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=full_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=about_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').click();
};

shinyjs.showFr = function() {
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=full_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=about_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=full_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=about_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').click();
};

shinyjs.showMainContent = function() {
  $('body').find('#loadingscrn').hide();
  $('body').find('#mainscrn').show();
};
