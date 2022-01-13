$(document).ready(function() {
  // js function to replace nav-tabs class by recipe-nav-tabs
  // (for recipe page)
  $(".tabset-recipe>ul").removeClass("nav-tabs")
  $(".tabset-recipe>ul").addClass("recipe-nav-tabs")

  // js function to replace nav-tabs class by custom-nav-tabs
  // (for non-recipe pages)
  $(".level1>ul").removeClass("nav-tabs")
  $(".level1>ul").addClass("custom-nav-tabs")
});