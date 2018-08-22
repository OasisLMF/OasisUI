shinyjs.changeJSGridTextVisibility = function(params) {
  var defaultParams = {
    index: null,
    visible: false
  };
  params = shinyjs.getParams(params, defaultParams);
  jsGridText = document.getElementsByClassName("js-grid-text-" + params.index)[0];
  if (jsGridText) {
    jsGridText.style.visibility = params.visible ? "visible" : "hidden";
  }
};