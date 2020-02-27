
// ~~~~~~~~~~~~ SHINYJS FUNCTIONS ~~~~~~~~~~~~~ //

/* Created by: Sijia Wang
 * Team: Data Analytics and Business Solutions (DABS)
 * Version: 1.0
 * Last modified: 2020-02-26
 * Description: JavaScript for 2019 PSES/SAFF dashboard application.
 */

// Scrolls to the top of the page
shinyjs.toTop = function() {
  window.scrollTo(0, 0);
};

// Hides French language tabs and shows English language tabs; clicks on 
//   Compare Data tab by default
shinyjs.showEng = function() {
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=full_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=about_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=full_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=about_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').click();
};

// Hides English language tabs and shows French language tabs; clicks on
//   Comparer les données tab by default
shinyjs.showFr = function() {
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=full_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=about_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=full_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=about_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').click();
};

// Hides loading screen and shows main content
shinyjs.showMainContent = function() {
  $('body').find('#loadingscrn').hide();
  $('body').find('#mainscrn').show();
};
